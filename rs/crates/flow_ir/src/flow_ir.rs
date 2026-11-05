//! Port of lib/flow_ir/flow_ir.ml(i): the public entry of the typed dataflow IR over `flow`
//! evaluations. OCaml submodules are Rust modules with their OCaml names (`Gpu`, `Cost`,
//! `Profile`, `Executor`); optional arguments are option structs; exceptions are `Result`s.
//!
//! Unlike OCaml, the closures a program keeps (`Gpu::Backend`, `Gpu::Kernel`) are `Arc`s so a
//! kernel can be shared; "physical equality" of a backend is `Arc::ptr_eq`.
#![allow(non_camel_case_types, non_snake_case, clippy::too_many_arguments, clippy::type_complexity)]

use crate::core_exec;
use crate::core_fuse;
use crate::core_ir as c;
use crate::core_lower;
use crate::fallback;
use crate::packed;
use flow_check::workspace::{self as w, Path, Paths, Term};
use flow_eval as e;
use flow_syntax::context;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as v, ResidualId, Value};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

pub type Packed = packed::T;

pub mod Gpu {
    use super::*;

    /// Borrowed output metadata; `output` selects a fused producer's buffer. `gpu_seconds` is the
    /// completed device dispatch duration when the backend supports timestamps.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Value {
        pub identity: i64,
        pub count: i64,
        pub width: i64,
        pub output: i64,
        pub stamp: i64,
        pub gpu_seconds: Option<f64>,
    }

    /// An output kept where it was computed, possibly still in flight. `settle` (on the initial
    /// domain, once per dispatch) waits for the dispatch and checks its status: an `Err` means the
    /// values are not to be used. `materialise` (once, from any domain once settled, never failing)
    /// reads it as `width` owned planes of `count` floats. Until then the device owns it, and
    /// `resident` is a placeholder array (of one element) the same backend binds to that buffer
    /// when a later kernel's source is it. `release(held)` says its owner dropped the result.
    /// `bounds` (a vec3 output, settled and unread) reduces it where it is to its per-component
    /// minima then maxima, six floats; `None` when the backend cannot.
    pub struct Planes {
        pub count: i64,
        pub width: i64,
        pub materialise: Box<dyn Fn() -> Vec<Vec<f64>> + Send + Sync>,
        pub resident: Option<super::Value>,
        pub settle: Box<dyn Fn() -> Result<(), Diagnostic> + Send + Sync>,
        pub release: Box<dyn Fn(bool) + Send + Sync>,
        pub bounds: Box<dyn Fn() -> Option<Vec<f64>> + Send + Sync>,
    }

    pub struct Kernel {
        pub run: Box<dyn Fn(&packed::Inputs) -> Result<Value, Diagnostic> + Send + Sync>,
        pub readback: Box<dyn Fn(&Value) -> Result<super::Value, Diagnostic> + Send + Sync>,
        /// Keeps the output for a lazy planar store: nothing is read until it materialises.
        pub planes: Box<dyn Fn(&Value) -> Result<Planes, Diagnostic> + Send + Sync>,
    }

    /// `planes` from a `readback`, deinterleaving its array.
    pub fn planes_of(readback: &dyn Fn(&Value) -> Result<super::Value, Diagnostic>, value: &Value) -> Result<Planes, Diagnostic> {
        match readback(value)? {
            super::Value::Float_array(data) | super::Value::Vec2_array(data) | super::Value::Vec3_array(data) | super::Value::Vec4_array(data) => {
                let width = value.width;
                let count = data.len() as i64 / width.max(1);
                let planes: Vec<Vec<f64>> = (0..width)
                    .map(|c| (0..count).map(|i| data[(i * width + c) as usize]).collect())
                    .collect();
                let planes = Arc::new(planes);
                Ok(Planes {
                    count,
                    width,
                    materialise: Box::new(move || (*planes).clone()),
                    resident: None,
                    settle: Box::new(|| Ok(())),
                    release: Box::new(|_| {}),
                    bounds: Box::new(|| None),
                })
            }
            _ => Err(v::fail("E_GPU", "GPU output is not a float array.")),
        }
    }

    /// Backend-owned integer storage token; no native handle crosses this boundary.
    pub type IntegerStorage = Arc<dyn std::any::Any + Send + Sync>;

    pub struct Integers {
        pub count: i64,
        pub storage: Option<IntegerStorage>,
        pub materialise: Box<dyn Fn() -> Vec<i64> + Send + Sync>,
        pub release: Box<dyn Fn(bool) + Send + Sync>,
    }

    /// A backend-neutral resident value scope. Validate topology before retaining its fields;
    /// retain final streams before closing the executor. `None` declines an unsupported stream.
    pub struct Residency {
        pub executor: Arc<core_exec::Resident>,
        pub retain_uint32: Box<dyn Fn(&super::Value) -> Option<Integers>>,
        pub retain_float: Box<dyn Fn(&super::Value) -> Option<Planes>>,
        /// point_count, vertices, offsets, kinds, max_polygon_size
        pub validate_topology: Box<dyn Fn(i64, &super::Value, &super::Value, &super::Value, i64) -> Option<Result<(), Diagnostic>>>,
    }

    pub struct Backend {
        pub residency: Box<dyn Fn(bool) -> Option<Residency> + Send + Sync>,
        /// With prepared inputs, measure their actual layout, uniforms and frame values. Without
        /// them the backend may offer a shape estimate; callers recheck after preparation.
        pub cost: Box<dyn Fn(Option<&packed::Inputs>, bool, &Packed, i64) -> Option<f64> + Send + Sync>,
        pub prepare: Box<dyn Fn(bool, &Packed) -> Result<Arc<Kernel>, Diagnostic> + Send + Sync>,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Policy { Measured, Qualification }

    #[derive(Clone)]
    pub struct Config {
        pub backend: Option<Arc<Backend>>,
        pub fast: bool,
        pub policy: Policy,
        /// Seconds to move one float between device and host, measured on this device. `None`
        /// keeps the M1-fitted `Cost::table`, which unified memory matches.
        pub readback_per_float: Option<f64>,
    }

    /// Exact CPU execution with no device backend.
    pub fn default() -> Config { Config { backend: None, fast: false, policy: Policy::Measured, readback_per_float: None } }
}

/// A profile owner: authored path and iteration.
pub type Id = (Path, Vec<i64>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tier { Interp, Core, Cpu_kernel, Gpu, Gpu_compile, Gpu_readback }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlacementReason {
    Unsupported_form,
    Missing_consumer,
    Validation_read,
    Cpu_consumer,
    No_cost_evidence,
    Cpu_measured_cheaper,
    Device_measured_cheaper,
    Held_on_device,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Placement {
    pub owner: Id,
    pub count: Option<i64>,
    pub reason: PlacementReason,
    pub cpu_seconds: Option<f64>,
    pub device_seconds: Option<f64>,
    pub cpu_consumers: Option<i64>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Execution {
    pub owner: Id,
    pub sites: Vec<(i64, Path, Vec<i64>)>,
    pub tier: Tier,
    pub seconds: f64,
}

pub mod Profile {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct StaticMap {
        pub site: Path,
        pub count: i64,
        pub tier: Tier,
        pub deferred: bool,
        pub refusal: Vec<Diagnostic>,
        pub head_elements: i64,
        pub tail_elements: i64,
        pub seconds: f64,
        pub head_seconds: f64,
        pub kernel_seconds: f64,
        pub tail_seconds: f64,
    }

    pub struct T {
        pub clock: Arc<dyn Fn() -> f64 + Send + Sync>,
        executions: Mutex<Vec<Execution>>,
        placements: Mutex<Vec<Placement>>,
        /// 512 slots of (serial, entry)
        maps: Mutex<Vec<Option<(usize, StaticMap)>>>,
        map_next: AtomicUsize,
    }

    struct Pending {
        started: f64,
        head_end: Option<f64>,
        kernel_end: f64,
        kernel_seconds: f64,
        tier: Tier,
        deferred: bool,
        refusal: Vec<Diagnostic>,
    }

    thread_local! {
        /// the scoped profile of this thread and the static maps in flight
        static CURRENT: RefCell<Option<(Arc<T>, Vec<Arc<Mutex<Pending>>>)>> = const { RefCell::new(None) };
    }

    pub fn create(clock: Arc<dyn Fn() -> f64 + Send + Sync>) -> Arc<T> {
        Arc::new(T {
            clock,
            executions: Mutex::new(Vec::new()),
            placements: Mutex::new(Vec::new()),
            maps: Mutex::new(vec![None; 512]),
            map_next: AtomicUsize::new(0),
        })
    }

    /// Immutable snapshot of at most 512 recent groups. Each packed group has one timing,
    /// excluding input materialization and nested groups.
    pub fn executions(profile: &T) -> Vec<Execution> { profile.executions.lock().unwrap().clone() }

    /// At most 512 recent placement decisions, newest first, one per owner.
    pub fn placements(profile: &T) -> Vec<Placement> { profile.placements.lock().unwrap().clone() }

    fn record_placement(profile: &T, placement: &Placement) {
        // ponytail: scan at most 512 owners per decision; index if profiling itself becomes costly.
        let mut list = profile.placements.lock().unwrap();
        list.retain(|old| old.owner != placement.owner);
        list.truncate(511);
        list.insert(0, placement.clone());
    }

    /// Record one decision for its owner, replacing the owner's previous one; `None` uses the
    /// scoped profile of this thread.
    pub fn place(profile: Option<&Arc<T>>, placement: &Placement) {
        let off = |reason: &str| fallback::note(&placement.owner.0.join("/"), reason);
        match placement.reason {
            PlacementReason::Unsupported_form => off("its form is refused on the device"),
            PlacementReason::Missing_consumer => off("nothing says who reads it"),
            _ => {}
        }
        let profile = match profile {
            Some(p) => Some(p.clone()),
            None => CURRENT.with(|c| c.borrow().as_ref().map(|(p, _)| p.clone())),
        };
        if let Some(profile) = profile { record_placement(&profile, placement); }
    }

    /// At most 512 recent static maps, oldest first.
    pub fn static_maps(profile: &T) -> Vec<StaticMap> {
        let mut found: Vec<(usize, StaticMap)> = profile.maps.lock().unwrap().iter().flatten().cloned().collect();
        found.sort_by(|a, b| a.0.cmp(&b.0));
        found.into_iter().map(|(_, m)| m).collect()
    }

    /// Scope static-map observations to this thread, restoring the previous scope on failure.
    pub fn sample<X>(profile: &Arc<T>, run: &mut dyn FnMut() -> X) -> X {
        let previous = CURRENT.with(|c| c.borrow_mut().replace((profile.clone(), Vec::new())));
        struct Restore(Option<(Arc<T>, Vec<Arc<Mutex<Pending>>>)>);
        impl Drop for Restore {
            fn drop(&mut self) { let p = self.0.take(); CURRENT.with(|c| *c.borrow_mut() = p); }
        }
        let _restore = Restore(previous);
        run()
    }

    pub fn record(profile: &T, execution: &Execution) {
        // ponytail: 512 recent groups, a linear scan once per group; index if profiling many small
        // groups measurably costs a frame.
        let mut list = profile.executions.lock().unwrap();
        let mut next = vec![execution.clone()];
        for old in list.iter() {
            if next.len() >= 512 { break; }
            if !(old.sites == execution.sites && old.tier == execution.tier) { next.push(old.clone()); }
        }
        *list = next;
    }

    /// The `Executor` entry of a static map: `finish(head, tail, completed)` closes it.
    pub fn map(site: &Path, _term: &Term, count: usize) -> Arc<dyn Fn(usize, usize, bool) + Send + Sync> {
        let current = CURRENT.with(|c| c.borrow().as_ref().map(|(p, _)| p.clone()));
        let Some(profile) = current else { return Arc::new(|_, _, _| {}); };
        let started = (profile.clock)();
        let pending = Arc::new(Mutex::new(Pending {
            started,
            head_end: None,
            kernel_end: started,
            kernel_seconds: 0.0,
            tier: Tier::Interp,
            deferred: false,
            refusal: Vec::new(),
        }));
        CURRENT.with(|c| {
            if let Some((_, stack)) = c.borrow_mut().as_mut() { stack.insert(0, pending.clone()); }
        });
        let site = site.clone();
        Arc::new(move |head, tail, completed| {
            let finished = (profile.clock)();
            CURRENT.with(|c| {
                if let Some((_, stack)) = c.borrow_mut().as_mut() { stack.retain(|f| !Arc::ptr_eq(f, &pending)); }
            });
            let p = pending.lock().unwrap();
            let kernel = completed && head + tail < count;
            let head_end = p.head_end.unwrap_or(finished);
            let entry = StaticMap {
                site: site.clone(),
                count: count as i64,
                tier: if kernel { p.tier } else { Tier::Interp },
                deferred: p.deferred,
                refusal: p.refusal.clone(),
                head_elements: head as i64,
                tail_elements: tail as i64,
                seconds: (finished - p.started).max(0.0),
                head_seconds: (head_end - p.started).max(0.0),
                kernel_seconds: p.kernel_seconds,
                tail_seconds: if p.kernel_end == p.started { 0.0 } else { (finished - head_end - p.kernel_seconds).max(0.0) },
            };
            let serial = profile.map_next.fetch_add(1, Ordering::Relaxed);
            let mut slots = profile.maps.lock().unwrap();
            let slot = serial & 511;
            let newer = matches!(&slots[slot], Some((s, _)) if *s > serial);
            if !newer { slots[slot] = Some((serial, entry)); }
        })
    }

    /// Run a static-map attempt at `tier`: `run` is given a function to record a refusal.
    pub fn attempt(tier: Tier, run: &mut dyn FnMut(&dyn Fn(Vec<Diagnostic>)) -> Option<Value>) -> Option<Value> {
        let current = CURRENT.with(|c| c.borrow().as_ref().and_then(|(p, stack)| stack.first().map(|s| (p.clone(), s.clone()))));
        match current {
            Some((profile, pending)) => {
                let began = (profile.clock)();
                {
                    let mut p = pending.lock().unwrap();
                    if p.head_end.is_none() { p.head_end = Some(began); }
                }
                let refuse = {
                    let pending = pending.clone();
                    move |reasons: Vec<Diagnostic>| pending.lock().unwrap().refusal = reasons
                };
                let result = run(&refuse);
                let mut p = pending.lock().unwrap();
                if result.is_some() { p.tier = tier; }
                p.kernel_end = (profile.clock)();
                let elapsed = (p.kernel_end - began).max(0.0);
                p.kernel_seconds += elapsed;
                result
            }
            None => run(&|_| {}),
        }
    }

    pub fn deferred() {
        CURRENT.with(|c| {
            if let Some((_, stack)) = c.borrow().as_ref() {
                if let Some(p) = stack.first() { p.lock().unwrap().deferred = true; }
            }
        });
    }
}

pub mod Cost {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct TierCost { pub fixed: f64, pub per_element: f64 }

    fn table(tier: Tier) -> TierCost {
        match tier {
            // Apple M1, one domain: bench_core placement's 4,096/16,384 arithmetic slope. Lookup and
            // inner-loop bodies cost more; this remains the cheap-body baseline.
            Tier::Interp => TierCost { fixed: 0.0, per_element: 0.614741 / 1_000_000.0 },
            Tier::Core => TierCost { fixed: 0.000000170, per_element: 0.0 },
            Tier::Cpu_kernel => {
                let per_element = (0.016522884 - 0.000040054) / (1_000_000.0 - 1024.0);
                TierCost { fixed: 0.000040054 - (per_element * 1024.0), per_element }
            }
            Tier::Gpu => {
                let per_element = (0.020215034 - 0.000437975) / (1_000_000.0 - 1024.0);
                TierCost { fixed: 0.000437975 - (per_element * 1024.0), per_element }
            }
            Tier::Gpu_readback => {
                let per_element = (0.007947922 - 0.000010014) / (1_000_000.0 - 1024.0);
                TierCost { fixed: 0.000010014 - (per_element * 1024.0), per_element }
            }
            Tier::Gpu_compile => TierCost { fixed: 0.008223057, per_element: 0.0 },
        }
    }

    pub fn estimate(tier: Tier, count: i64) -> f64 {
        assert!(count >= 0, "Flow_ir.Cost.estimate: negative count");
        let cost = table(tier);
        cost.fixed + (cost.per_element * count as f64)
    }

    pub fn display_sink(tier: Tier, count: i64) -> f64 {
        assert!(count >= 0, "Flow_ir.Cost.display_sink: negative count");
        count as f64
            * match tier {
                Tier::Gpu => ((0.028567791 - 0.001480103) / 990_000.0) - table(Tier::Gpu).per_element,
                _ => ((0.903350830 - 0.010123968) / 990_000.0) - table(Tier::Cpu_kernel).per_element,
            }
    }

    pub fn cheapest(legal: &[Tier], count: i64) -> Tier {
        assert!(!legal.is_empty(), "Flow_ir.Cost.cheapest: no legal tiers");
        let mut best = legal[0];
        for tier in &legal[1..] {
            if estimate(*tier, count) < estimate(best, count) { best = *tier; }
        }
        best
    }

    /// The same measured CPU decision used at placement and force time.
    pub fn packed(count: i64) -> Tier { cheapest(&[Tier::Interp, Tier::Cpu_kernel], count) }

    /// Compare total time to the known consumers, including transfers, validation and sinks.
    /// Missing, negative or nonfinite evidence keeps CPU. Ties keep CPU.
    pub fn choose(cpu_seconds: Option<f64>, device_seconds: Option<f64>, resident: bool, cpu_consumers: i64) -> PlacementReason {
        assert!(cpu_consumers >= 0, "Flow_ir.Cost.choose: negative consumer count");
        let valid = |seconds: f64| seconds.is_finite() && seconds >= 0.0;
        match (cpu_seconds, device_seconds) {
            (Some(cpu), Some(device)) if valid(cpu) && valid(device) => {
                if device < cpu {
                    if resident { PlacementReason::Held_on_device } else { PlacementReason::Device_measured_cheaper }
                } else if cpu_consumers > 0 {
                    PlacementReason::Cpu_consumer
                } else {
                    PlacementReason::Cpu_measured_cheaper
                }
            }
            _ => PlacementReason::No_cost_evidence,
        }
    }

    /// A CPU cook sampled for comparison stops here: past it the CPU has lost, and a floor keeps a
    /// tiny site's one-time compile from reading as slow.
    pub fn probe_budget(device_seconds: f64) -> f64 { f64::max(0.02, 2.0 * device_seconds) }

    pub fn on_device(reason: PlacementReason) -> bool {
        matches!(reason, PlacementReason::Device_measured_cheaper | PlacementReason::Held_on_device)
    }

    pub fn reason_name(reason: PlacementReason) -> &'static str {
        match reason {
            PlacementReason::Unsupported_form => "unsupported_form",
            PlacementReason::Missing_consumer => "missing_consumer",
            PlacementReason::Validation_read => "validation_read",
            PlacementReason::Cpu_consumer => "cpu_consumer",
            PlacementReason::No_cost_evidence => "no_cost_evidence",
            PlacementReason::Cpu_measured_cheaper => "cpu_measured_cheaper",
            PlacementReason::Device_measured_cheaper => "device_measured_cheaper",
            PlacementReason::Held_on_device => "held_on_device",
        }
    }

    /// Seconds to move `count` elements of `width` floats between host and device: the M1 table, or
    /// the gpu config's `readback_per_float` when the device measured its own.
    pub fn readback(gpu: Option<&Gpu::Config>, count: i64, width: i64) -> f64 {
        let cost = table(Tier::Gpu_readback);
        let per_float = gpu.and_then(|g| g.readback_per_float).unwrap_or(cost.per_element);
        cost.fixed + (per_float * count as f64 * width as f64)
    }

    pub fn outputs_readback(gpu: Option<&Gpu::Config>, packed: &Packed, count: i64) -> f64 {
        packed.outputs.iter().fold(0.0, |total, lanes| total + readback(gpu, count, lanes.len() as i64))
    }
}

fn scalar_uniforms(packed: &Packed) -> bool {
    let scalar = |ty: &Ty| matches!(ty, Ty::Int | Ty::Float | Ty::Bool | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text);
    packed.uniforms.iter().all(|u| match u {
        Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..) | Value::Text(_) => true,
        Value::Residual(r) => e::Residual::residual_ty(*r).is_some_and(|ty| scalar(&ty)),
        Value::Source(_, ty, _) | Value::Struct(_, ty, _) => scalar(ty),
        _ => false,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sink { Display(String), Sop_input }

// ---------------------------------------------------------------------------------------------
// Static kernels.

/// Raised (as `Err`) by `static` with `verify` when an exact static kernel differs from the walker.
pub const STATIC_VERIFICATION_FAILED: &str = "E_STATIC_VERIFICATION_FAILED";

fn same_bits(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int_array(a), Value::Int_array(b)) => a == b,
        (Value::Float_array(a), Value::Float_array(b))
        | (Value::Vec2_array(a), Value::Vec2_array(b))
        | (Value::Vec3_array(a), Value::Vec3_array(b))
        | (Value::Vec4_array(a), Value::Vec4_array(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.to_bits() == y.to_bits()),
        _ => false,
    }
}

/// `f`, with a panic turned into the refusal OCaml's `Invalid_argument | Not_found | Failure` arm makes:
/// `E_PACKED_EXECUTION` carrying the panic text, so the map falls back to the walker.
fn guarded_force(f: &mut dyn FnMut() -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            let text = payload.downcast_ref::<String>().cloned().or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_owned())).unwrap_or_else(|| "panic".to_owned());
            Err(v::fail("E_PACKED_EXECUTION", &text))
        }
    }
}

/// Only exact static kernels receive trust. Failure or a live capture lets the evaluator walk from
/// zero; eager recording still checks its ordinary head. Qualification additionally compares every
/// element with the reference (a difference is recorded in `failure`).
fn static_map(verify: bool, failure: &Arc<Mutex<Option<Diagnostic>>>, residual: ResidualId) -> Option<Value> {
    Profile::attempt(Tier::Cpu_kernel, &mut |refuse: &dyn Fn(Vec<Diagnostic>)| {
        let value = Value::Residual(residual);
        if e::frame_dependent(&value) || e::state_dependent(&value) { return None; }
        let view = e::Residual::residual_view(residual).ok()?;
        let options = packed::CompileOptions::default();
        let compiled = packed::compile_result(&options, residual, &view.term);
        let packed = match compiled {
            Err(reasons) => {
                refuse(reasons);
                return None;
            }
            Ok(p) => p,
        };
        // OCaml catches Invalid_argument, Not_found and Failure here only: a kernel that cannot run is
        // a refusal (E_PACKED_EXECUTION) and the walker takes the map, not a dead cook
        match guarded_force(&mut || packed::force(&packed, &input::at_time(0.0), &packed::ForceOptions::default())) {
            Ok(value) => {
                if verify {
                    let reference = match e::Host::force_reference(&Value::Residual(residual), &input::at_time(0.0), None, None, None) {
                        Ok(v) => v,
                        Err(d) => {
                            *failure.lock().unwrap() = Some(v::fail(STATIC_VERIFICATION_FAILED, &d.message));
                            return None;
                        }
                    };
                    if !same_bits(&reference, &value) {
                        *failure.lock().unwrap() = Some(v::fail(
                            STATIC_VERIFICATION_FAILED,
                            &format!("Exact static kernel differs from the walker at {}", view.site.join("/")),
                        ));
                        return None;
                    }
                }
                Some(value)
            }
            Err(d) => {
                refuse(vec![d]);
                None
            }
        }
    })
}

/// Exact static kernels run before the walker for maps above 4,096 elements. Refusal or failure
/// restarts reference evaluation from zero. `record` keeps eager checked heads; `replay` instead
/// uses closure call selectors for body records. `verify` compares every successful kernel element
/// with the walker bit for bit and fails on any difference; it is for qualification.
#[derive(Clone, Default)]
pub struct StaticOptions {
    pub record: bool,
    pub inputs: Option<Vec<(String, Vec<(String, Value)>)>>,
    pub replay: bool,
    pub verify: bool,
}

pub fn static_(options: &StaticOptions, workspace: &w::Workspace) -> Result<e::Eval, Diagnostic> {
    let failure: Arc<Mutex<Option<Diagnostic>>> = Arc::new(Mutex::new(None));
    let verify = options.verify;
    let handlers = e::ResidualHandlers {
        trusted: true,
        replay: options.replay,
        map: Some(Arc::new(|site, term, count| Profile::map(site, term, count))),
        bulk: Some(Arc::new({
            let failure = failure.clone();
            move |r| static_map(verify, &failure, r)
        })),
        ..e::ResidualHandlers::default()
    };
    let result = e::Residual::handle(handlers, &mut || {
        e::Host::static_with_requests(None, options.record, options.inputs.as_deref(), workspace)
    });
    if let Some(d) = failure.lock().unwrap().take() { return Err(d); }
    result
}

// ---------------------------------------------------------------------------------------------
// Qualification.

#[derive(Clone, Default)]
pub struct QualifyOptions {
    pub record: bool,
    pub inputs: Option<Vec<(String, Vec<(String, Value)>)>>,
    /// Called with the associated authored producer (None for an ambiguous or missing association)
    /// and the actual observed handle.
    pub observe: Option<Arc<dyn Fn(Option<Path>, ResidualId) + Send + Sync>>,
    pub replay: bool,
    pub verify: bool,
    /// Keep qualifying large static maps in SOP graphs as residuals for their cook. Host graphs
    /// stay eager; the default is false for CLI checking and independent evaluation.
    pub defer: bool,
    pub gpu: Option<Gpu::Config>,
}

type Form = flow_syntax::arena::SyntaxRow;

/// What authored association compares: a form's identity without the form.
type FormKey = (i64, rays_diagnostic::Span);
fn key_of_form(f: &Form) -> FormKey { (f.id, f.span.clone()) }
fn key_same(k: &FormKey, f: &Form) -> bool { k.0 == f.id && k.1 == f.span }

struct Tables {
    roots: HashMap<i64, Vec<(FormKey, Path)>>,
    index: HashMap<i64, Vec<(FormKey, Path, Paths)>>,
    callable_owners: HashMap<i64, Vec<(FormKey, Path)>>,
}

struct Walker {
    collect: bool,
    t: Tables,
    called: HashMap<i64, Vec<(Term, Path)>>,
    /// Bodies already found equal to a stored one under an owner.
    called_seen: HashMap<(u64, u32), Vec<Path>>,
}

impl Walker {
    fn callable(&mut self, owner: &Path, arena: &flow_check::term_arena::TermArena, id: flow_check::term_arena::TermId) {
        let form = arena.syntax().row(arena.row(id).form);
        let entries = self.t.callable_owners.entry(form.id).or_default();
        if !entries.iter().any(|(key, path)| key_same(key, form) && path == owner) {
            entries.insert(0, (key_of_form(form), owner.clone()));
        }
    }

    fn add(&mut self, owner: &Path, arena: &flow_check::term_arena::TermArena, id: flow_check::term_arena::TermId) {
        let term = arena.row(id);
        let form = arena.syntax().row(term.form);
        let mut producers: Paths = Paths::new();
        for (key, path) in self.t.roots.get(&form.id).map(|v| v.as_slice()).unwrap_or(&[]) {
            if key_same(key, form) { producers.insert(path.clone()); }
        }
        let producer_path = term.path.and_then(|path| producers.iter().find(|candidate| {
            let names = arena.path(path);
            names.len() == candidate.len() && names.iter().zip(candidate.iter()).all(|(&name, part)| arena.name(name) == part)
        })).cloned();
        let producers: Paths = match producer_path {
            Some(path) => std::iter::once(path).collect(),
            _ if producers.contains(owner) => std::iter::once(owner.clone()).collect(),
            _ => producers,
        };
        let entries = self.t.index.entry(form.id).or_default();
        let same = |x: &(FormKey, Path, Paths)| key_same(&x.0, form) && x.1 == *owner;
        let mut merged = entries.iter().position(same).map(|i| entries.remove(i).2).unwrap_or_default();
        merged.extend(producers);
        entries.insert(0, (key_of_form(form), owner.clone(), merged));
    }

    fn walk(&mut self, owner: &Path, arena: &Arc<flow_check::term_arena::TermArena>, id: flow_check::term_arena::TermId) {
        use w::Node;
        let term = arena.row(id);
        let path = term.path.map(|path| arena.path(path).iter().map(|&name| arena.name(name).to_owned()).collect());
        let owner = path.as_ref().unwrap_or(owner);
        if !self.collect {
            match term.node {
                Node::Hof(hof) if matches!(arena.hof(hof).kind, w::HofKind::Map | w::HofKind::Reduce) => self.add(owner, arena, id),
                Node::Loop(_) => self.add(owner, arena, id),
                Node::Op(op) if matches!(arena.name(arena.op(op).op), "array/sum" | "image/map") => self.add(owner, arena, id),
                _ => {}
            }
        }
        match term.node {
            Node::Loop(loop_id) => {
                let zone = arena.path(arena.loop_node(loop_id).zone).iter().map(|&name| arena.name(name).to_owned()).collect();
                for &child in arena.children(id) { self.walk(&zone, arena, child); }
            }
            Node::Fn(fn_id) => {
                let function = arena.function(fn_id);
                let zone = arena.path(function.zone).iter().map(|&name| arena.name(name).to_owned()).collect();
                if self.collect { self.callable(&zone, arena, function.body); }
                if let Some(capture) = function.capture { self.walk(owner, arena, capture); }
                self.walk(&zone, arena, function.body);
            }
            Node::CallFn(fn_id) => {
                let call = arena.call_fn(fn_id);
                for &arg in arena.term_ids(call.args) { self.walk(owner, arena, arg); }
                if let Some(body_id) = call.body {
                    let form = arena.syntax().row(arena.row(body_id).form);
                    let mut owners = self.t.callable_owners.get(&form.id).into_iter().flatten().filter(|(key, _)| key_same(key, form)).map(|(_, owner)| owner);
                    let call_owner = match owners.next() {
                        Some(first) if owners.all(|other| other == first) => first.clone(),
                        _ => owner.clone(),
                    };
                    // Calls through the same body and owner add nothing; another row still pays
                    // the authored-content comparison before sharing the walk.
                    let body = arena.handle(body_id);
                    let identity = body.identity();
                    let walked = self.called.entry(form.id).or_default();
                    let known = self.called_seen.get(&identity).is_some_and(|paths| paths.contains(&call_owner))
                        || walked.iter().any(|(seen, path)| *path == call_owner && (seen.identity() == identity || *seen == body));
                    if known {
                        let paths = self.called_seen.entry(identity).or_default();
                        if !paths.contains(&call_owner) { paths.push(call_owner.clone()); }
                    } else {
                        walked.insert(0, (body, call_owner.clone()));
                        self.walk(&call_owner, arena, body_id);
                    }
                }
            }
            Node::State(state_id) => {
                let state = arena.state(state_id);
                let zone = arena.path(state.zone).iter().map(|&name| arena.name(name).to_owned()).collect();
                self.walk(owner, arena, state.init);
                self.walk(&zone, arena, state.step);
            }
            Node::Bypass(call) => {
                if let Some(input) = w::bypass_input(&arena.handle(call)) { self.walk(owner, arena, input.row_id()); }
            }
            _ => {
                for &child in arena.children(id) { self.walk(owner, arena, child); }
            }
        }
    }
}

fn plain_value(value: &Value) -> bool {
    match value {
        Value::Fn(_) => false,
        Value::List(xs) => xs.iter().all(plain_value),
        Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => fs.iter().all(|(_, x)| plain_value(x)),
        _ => true,
    }
}

/// A map's iteration tuple only names its sites (a loop's tuple selects its skips), and a closure
/// has no value key, so a view holding one is never matched.
fn acceptance_key(view: &e::Residual::View) -> Option<String> {
    if !view.bindings.iter().all(|(_, x)| plain_value(x)) { return None; }
    let iter: Vec<i64> = match view.term.node { w::Node::Hof(..) => Vec::new(), _ => view.iter.clone() };
    let bindings: Vec<(String, Vec<u8>)> = view.bindings.iter().map(|(n, x)| (n.clone(), v::key_of(&|r| r.0, x))).collect();
    Some(format!(
        "{:?}",
        (
            view.term.arena().syntax().row(view.term.form).id,
            &view.site,
            &view.owner,
            iter,
            view.instance,
            &view.prefix,
            (view.previous, view.host, view.geometry, context::name(view.context)),
            bindings,
        )
    ))
}

fn pending(path: &Path) -> Vec<Diagnostic> {
    vec![v::fail("E_PACKED_PENDING", &format!("No unambiguous static specialization was observed for producer {}.", path.join("/")))]
}

struct Qualifier {
    t: Tables,
    ops: Arc<Vec<flow_check::op::Op>>,
    gpu: Gpu::Config,
    accepted: Mutex<HashMap<String, Vec<(Path, Arc<Term>)>>>,
    outcomes: Mutex<HashMap<Path, Vec<Diagnostic>>>,
    observe: Option<Arc<dyn Fn(Option<Path>, ResidualId) + Send + Sync>>,
    packed_sites: Vec<(Path, bool)>,
    defer: bool,
}

impl Qualifier {
    fn observed(&self, residual: ResidualId) {
        let Ok(view) = e::Residual::residual_view(residual) else { return };
        let arena = view.term.arena();
        let form = arena.syntax().row(view.term.form);
        let producers: Paths = self
            .t
            .index
            .get(&form.id)
            .and_then(|v| v.iter().find(|(key, owner, _)| key_same(key, form) && *owner == view.owner))
            .map(|(_, _, p)| p.clone())
            .unwrap_or_default();
        let producer: Option<Path> = if producers.len() == 1 { producers.iter().next().cloned() } else { None };
        if producer.is_none() {
            let mut possible = producers.clone();
            for (key, path) in self.t.roots.get(&form.id).map(|v| v.as_slice()).unwrap_or(&[]) {
                if key_same(key, form) { possible.insert(path.clone()); }
            }
            let mut outcomes = self.outcomes.lock().unwrap();
            for path in possible {
                match outcomes.get(&path) {
                    Some(x) if !x.is_empty() => {}
                    _ => { outcomes.insert(path.clone(), pending(&path)); }
                }
            }
        }
        let adapted: Result<ResidualId, Vec<Diagnostic>> = match (&producer, view.term.node) {
            (Some(path), w::Node::Op(id)) if arena.name(arena.op(id).op) == "image/map" => {
                let canonical = match (
                    flow_check::op::find(&self.ops, "image/map", &context::image()),
                    flow_check::op::find(&[], "image/map", &context::image()),
                ) {
                    (Some(actual), Some(builtin)) => Arc::ptr_eq(&actual.body, &builtin.body) && actual.category == builtin.category && actual.name == builtin.name && actual.signature == builtin.signature,
                    _ => false,
                };
                if !canonical {
                    Err(vec![v::fail("E_GPU_FORM", "Image qualification needs the canonical image/map declaration.")])
                } else {
                    match arena.fields(arena.op(id).args).iter().find(|field| arena.name(field.name) == "function") {
                        None => Err(vec![v::fail("E_KERNEL_FORM", "Image map needs a pixel function.")]),
                        Some(field) => {
                            let mapped = e::Residual::eval_term(residual, &arena.handle(field.term), &input::at_time(0.0), None, None, None).and_then(|x| match x {
                                Value::Fn(function) => e::Host::map_function(
                                    Some((view.instance, view.site.clone(), view.iter.clone())),
                                    Some(path.clone()),
                                    &flow_syntax::ty::FnSignature { params: vec![Ty::Vec2], result: Ty::Vec4 },
                                    function,
                                    &[Value::Vec2_array(Arc::new(vec![0.5, 0.5]))],
                                ),
                                _ => Err(v::fail("E_KERNEL_FORM", "Image map needs a pixel function.")),
                            });
                            match mapped {
                                Ok(Value::Residual(r)) => Ok(r),
                                Ok(_) => Err(vec![v::fail("E_KERNEL_FORM", "Image map did not produce a pixel program.")]),
                                Err(d) => Err(vec![d]),
                            }
                        }
                    }
                }
            }
            _ => Ok(residual),
        };
        // A producer's first refusal stands (below), and an accepted lowering is a function of the
        // residual's view, so a map observed again with the same captures, as an iterate's body is
        // on every pass, is not lowered again.
        let settled = |path: &Path| {
            matches!(self.outcomes.lock().unwrap().get(path), Some(x) if x.first().map(|d| d.code != "E_PACKED_PENDING").unwrap_or(false))
        };
        if let Some(path) = &producer {
            let settled = settled(path);
            let key = if settled { None } else { acceptance_key(&view) };
            let seen = match &key {
                Some(key) => self.accepted.lock().unwrap().get(key).map(|l| l.iter().any(|(p, t)| p == path && (t.identity() == view.term.identity() || **t == *view.term))).unwrap_or(false),
                None => false,
            };
            if !(settled || seen) {
                let reasons = match &adapted {
                    Err(reasons) => reasons.clone(),
                    Ok(r) => match e::Residual::residual_view(*r) {
                        Err(d) => vec![d],
                        Ok(rv) => match packed::compile_result(&packed::CompileOptions::default(), *r, &rv.term) {
                            Err(reasons) => reasons,
                            Ok(p) => packed::gpu_refusals(true, self.gpu.fast, &p),
                        },
                    },
                };
                let reasons: Vec<Diagnostic> = reasons
                    .into_iter()
                    .map(|d| Diagnostic {
                        span: if d.span.is_some() { d.span.clone() } else { Some(form.span.clone()) },
                        message: format!(
                            "{} (producer {}, instance {}, tuple [{}])",
                            d.message,
                            path.join("/"),
                            view.instance,
                            view.iter.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(",")
                        ),
                        ..d
                    })
                    .collect();
                if reasons.is_empty() {
                    if let Some(key) = &key {
                        self.accepted.lock().unwrap().entry(key.clone()).or_default().insert(0, (path.clone(), view.term.clone()));
                    }
                }
                let mut outcomes = self.outcomes.lock().unwrap();
                match outcomes.get(path) {
                    Some(x) if !x.is_empty() && reasons.is_empty() => {}
                    _ => { outcomes.insert(path.clone(), reasons); }
                }
            }
        }
        if let Some(callback) = &self.observe { callback(producer, adapted.unwrap_or(residual)); }
    }

    /// A static map the checker accepts as an approximable producer may run on the device and come
    /// back as a 32-bit array: the SOP-input readback of the owner decision, taken within a
    /// tolerance.
    fn bulk_approx(&self, residual: ResidualId) -> Option<Value> {
        Profile::attempt(Tier::Gpu, &mut |_| {
            let backend = self.gpu.backend.clone()?;
            let view = e::Residual::residual_view(residual).ok()?;
            if self.defer && view.context == context::sop() { return None; }
            let value = Value::Residual(residual);
            if e::frame_dependent(&value) || e::state_dependent(&value) { return None; }
            let packed = packed::compile(&packed::CompileOptions::default(), residual, &view.term)?;
            let (_, site, _) = packed::site(&packed);
            if !self.packed_sites.iter().any(|(path, candidate)| *path == site && *candidate) { return None; }
            // the site is read before preparing inputs, which re-enters the evaluator
            let (_, site_path, site_iter) = packed::site(&packed);
            let fast = self.gpu.fast;
            let count = packed::static_count(&packed).unwrap_or(0);
            let placed = |inputs: Option<&packed::Inputs>, count: i64| -> bool {
                if self.gpu.policy == Gpu::Policy::Qualification { return true; }
                let cpu_seconds = Some(Cost::estimate(Cost::packed(count), count));
                let device_seconds = (backend.cost)(inputs, fast, &packed, count).and_then(|seconds| {
                    if !(seconds.is_finite() && seconds >= 0.0) { None } else { Some(seconds + Cost::outputs_readback(Some(&self.gpu), &packed, count)) }
                });
                let reason = Cost::choose(cpu_seconds, device_seconds, false, 1);
                                Profile::place(
                    None,
                    &Placement { owner: (site_path.clone(), site_iter.clone()), count: Some(count), reason, cpu_seconds, device_seconds, cpu_consumers: Some(1) },
                );
                Cost::on_device(reason) || (inputs.is_none() && reason == PlacementReason::No_cost_evidence)
            };
            if !packed::gpu_refusals(true, fast, &packed).is_empty() || (scalar_uniforms(&packed) && !placed(None, count)) {
                return None;
            }
            let inputs = packed::host::prepare(&packed, &packed::host::PrepareOptions { fast, ..Default::default() }, &input::at_time(0.0)).ok()?;
            if !placed(Some(&inputs), inputs.count) { return None; }
            let kernel = (backend.prepare)(fast, &packed).ok()?;
            let output = (kernel.run)(&inputs).ok()?;
            (kernel.readback)(&output).ok()
        })
    }

    fn defer_map(&self, residual: ResidualId) -> bool {
        let Ok(view) = e::Residual::residual_view(residual) else { return false };
        let value = Value::Residual(residual);
        if view.context != context::sop() || e::frame_dependent(&value) || e::state_dependent(&value) { return false; }
        match packed::compile(&packed::CompileOptions::default(), residual, &view.term) {
            None => false,
            Some(_) => {
                Profile::deferred();
                true
            }
        }
    }
}

/// Evaluate once with actual captures and qualify immutable checker candidates through production
/// packed compilation and the shared GPU form checks. Any observed refusal dominates successes
/// across instances/tuples. Unobserved or ambiguous producers stay explicitly pending. Every call
/// rebuilds conclusions; no residual/program proofs are retained and live forcing never
/// requalifies.
pub fn qualify_workspace(options: &QualifyOptions, workspace: &w::Workspace) -> Result<(w::Workspace, e::Eval), Diagnostic> {
    let _phase = flow_syntax::phase_timer::measure("qualify");
    let index_phase = flow_syntax::phase_timer::measure("qualify.index");
    let gpu = options.gpu.clone().unwrap_or_else(Gpu::default);
    let mut roots: HashMap<i64, Vec<(FormKey, Path)>> = HashMap::new();
    for (form, path) in &workspace.packed_roots {
        roots.entry(form.id).or_default().insert(0, (key_of_form(form), path.clone()));
    }
    let mut walker = Walker {
        collect: true,
        t: Tables { roots, index: HashMap::new(), callable_owners: HashMap::new() },
        called: HashMap::new(),
        called_seen: HashMap::new(),
    };
    for graph in workspace.defs.iter() { walker.callable(&vec![format!("def:{}", graph.name)], graph.body.arena(), graph.body.row_id()); }
    for collect in [true, false] {
        walker.collect = collect;
        walker.called.clear();
        walker.called_seen.clear();
        for (i, graph) in workspace.graphs.iter().chain(workspace.defs.iter()).enumerate() {
            let owner: Path = if i >= workspace.graphs.len() { vec![format!("def:{}", graph.name)] } else { vec![graph.name.clone()] };
            for (_, _, term) in &graph.inputs {
                if let Some(term) = term { walker.walk(&owner, term.arena(), term.row_id()); }
            }
            walker.walk(&owner, graph.body.arena(), graph.body.row_id());
        }
    }
    drop(index_phase);
    let q = Arc::new(Qualifier {
        t: walker.t,
        ops: workspace.ops.clone(),
        gpu: gpu.clone(),
        accepted: Mutex::new(HashMap::new()),
        outcomes: Mutex::new(HashMap::new()),
        observe: options.observe.clone(),
        packed_sites: workspace.packed.iter().map(|(p, f)| (p.clone(), matches!(f, w::PackedForm::Candidate(_)))).collect(),
        defer: options.defer,
    });
    let failure: Arc<Mutex<Option<Diagnostic>>> = Arc::new(Mutex::new(None));
    let verify = options.verify;
    let handlers = e::ResidualHandlers {
        trusted: true,
        replay: options.replay,
        map: Some(Arc::new(|site, term, count| Profile::map(site, term, count))),
        observe: Some(Arc::new({
            let q = q.clone();
            move |r| q.observed(r)
        })),
        defer: if options.defer {
            Some(Arc::new({
                let q = q.clone();
                move |r| q.defer_map(r)
            }))
        } else {
            None
        },
        bulk: Some(Arc::new({
            let failure = failure.clone();
            move |r| static_map(verify, &failure, r)
        })),
        bulk_approx: Some(Arc::new({
            let q = q.clone();
            move |r| q.bulk_approx(r)
        })),
        ..e::ResidualHandlers::default()
    };
    let evaluated = e::Residual::handle(handlers, &mut || {
        e::Host::static_with_requests(None, options.record, options.inputs.as_deref(), workspace)
    });
    if let Some(d) = failure.lock().unwrap().take() { return Err(d); }
    let evaluated = evaluated?;
    let mut packed_by_path: HashMap<&Path, &w::PackedForm> = HashMap::new();
    for (path, form) in &workspace.packed {
        packed_by_path.entry(path).or_insert(form);
    }
    let outcomes = q.outcomes.lock().unwrap();
    let reasons = |path: &Path| -> Vec<Diagnostic> {
        match packed_by_path.get(path) {
            Some(w::PackedForm::Refused(reasons)) => reasons.clone(),
            _ => outcomes.get(path).cloned().unwrap_or_else(|| pending(path)),
        }
    };
    let mut approx = Paths::new();
    let mut approx_reasons: Vec<(Path, Vec<Diagnostic>)> = Vec::new();
    for (path, form) in &workspace.packed {
        let refused: Vec<Diagnostic> = match form {
            w::PackedForm::Refused(reasons) => reasons.clone(),
            w::PackedForm::Candidate(producers) => producers.iter().flat_map(|p| reasons(p)).collect(),
        };
        if refused.is_empty() { approx.insert(path.clone()); } else { approx_reasons.insert(0, (path.clone(), refused)); }
    }
    approx_reasons.reverse();
    let mut qualified = workspace.clone();
    qualified.approx = approx;
    qualified.approx_reasons = approx_reasons;
    Ok((qualified, evaluated))
}

// ---------------------------------------------------------------------------------------------
// Executor.

pub mod Executor {
    use super::*;
    use flow_check::workspace::Node;

    #[derive(Clone)]
    pub enum Candidate {
        Core(Arc<core_exec::Prepared>),
        Packed(Packed),
        Reference,
    }

    type CountSource = c::Region::CountSource;

    pub struct ProgramData {
        pub value: Value,
        candidates: Mutex<HashMap<(i64, bool), Candidate>>,
        root_core: Mutex<Option<(bool, Arc<core_exec::Prepared>)>>,
        packed_region: Option<Packed>,
        packed_root: Option<(Packed, bool)>,
        trace: Option<core_lower::Trace>,
        count_source: CountSource,
        profile: Option<Arc<Profile::T>>,
        approx: Paths,
        sink: Option<Sink>,
        gpu_kernel: Mutex<Option<(Arc<Gpu::Backend>, bool, Packed, Arc<Gpu::Kernel>)>>,
        record_region: std::sync::OnceLock<Option<(Vec<String>, Packed)>>,
    }

    pub type Program = Arc<ProgramData>;

    thread_local! {
        /// A residual is immutable: its strict lowering, and the captures that lowering took in,
        /// are kept for its next strict compile (a write's check, its program and its device
        /// record compile one value three times).
        static STRICTS: RefCell<HashMap<i64, Arc<(Result<c::Program, String>, Vec<i64>)>>> = RefCell::new(HashMap::new());
    }

    /// This thread's strict lowerings still kept: the ledger walks them.
    pub fn kept_programs() -> Vec<c::Program> {
        STRICTS.with(|t| t.borrow().values().filter_map(|entry| entry.0.as_ref().ok().cloned()).collect())
    }

    fn direct_region(
        trace: &Option<core_lower::Trace>,
        through_exact: bool,
        readback: bool,
        count_source: &CountSource,
        value: &Value,
    ) -> Option<(Packed, bool)> {
        fn find(
            trace: &Option<core_lower::Trace>,
            through_exact: bool,
            readback: bool,
            count_source: &CountSource,
            seen: &mut Vec<(ResidualId, Term)>,
            residual: ResidualId,
            term: &Term,
        ) -> Option<(Packed, bool)> {
            if seen.iter().any(|(r, t)| *r == residual && t == term) { return None; }
            seen.push((residual, term.clone()));
            let arena = term.arena();
            match term.node {
                Node::Expanded(id) => find(trace, through_exact, readback, count_source, seen, residual, &term.child(arena.expanded(id).body)),
                Node::Bypass(call) => w::bypass_input(&term.child(call)).and_then(|input| find(trace, through_exact, readback, count_source, seen, residual, &input)),
                Node::Op(id) if readback && term.name(arena.op(id).op) == "exact" && arena.fields(arena.op(id).args).len() == 1 && term.name(arena.fields(arena.op(id).args)[0].name) == "value" => {
                    find(trace, through_exact, readback, count_source, seen, residual, &term.child(arena.fields(arena.op(id).args)[0].term)).map(|(packed, _)| (packed, true))
                }
                Node::Hof(id) if matches!(arena.hof(id).kind, w::HofKind::Map | w::HofKind::Reduce) => {
                    compile_region(trace, through_exact, count_source, residual, term).map(|p| (p, false))
                }
                Node::Loop(_) => compile_region(trace, through_exact, count_source, residual, term).map(|p| (p, false)),
                Node::Op(id) if term.name(arena.op(id).op) == "array/sum" => compile_region(trace, through_exact, count_source, residual, term).map(|p| (p, false)),
                // A bound map is the map it names.
                Node::RefBinding(id) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => {
                    match e::Residual::residual_binding(residual, term.name(arena.ref_binding(id).name)) {
                        Some(Value::Residual(nested)) => {
                            let term = e::Residual::residual_term(nested)?;
                            find(trace, through_exact, readback, count_source, seen, nested, &term)
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        match value {
            Value::Residual(residual) => {
                let term = e::Residual::residual_term(*residual)?;
                find(trace, through_exact, readback, count_source, &mut Vec::new(), *residual, &term)
            }
            _ => None,
        }
    }

    fn compile_region(trace: &Option<core_lower::Trace>, through_exact: bool, count_source: &CountSource, residual: ResidualId, term: &Term) -> Option<Packed> {
        packed::compile(
            &packed::CompileOptions { trace: trace.clone(), through_exact, fusion: true, count_source: Some(count_source.clone()) },
            residual,
            term,
        )
    }

    fn strict_lower(trace: &Option<core_lower::Trace>, residual: ResidualId) -> Arc<(Result<c::Program, String>, Vec<i64>)> {
        let lower = || {
            let _phase = flow_syntax::phase_timer::measure("executor.strict_lower");
            let inline: std::rc::Rc<RefCell<Vec<i64>>> = std::rc::Rc::new(RefCell::new(Vec::new()));
            let hook = {
                let inline = inline.clone();
                std::rc::Rc::new(move |r: ResidualId| inline.borrow_mut().insert(0, r.0)) as core_lower::LoweredHook
            };
            let core = core_lower::lower(None, trace.clone(), true, None, Some(hook), residual);
            let ids = inline.borrow().clone();
            Arc::new((core, ids))
        };
        if trace.is_some() { return lower(); }
        if let Some(kept) = STRICTS.with(|t| t.borrow().get(&residual.0).cloned()) { return kept; }
        let kept = lower();
        // ponytail: a whole clear at 4096 residuals, as Eval's free-name memo
        STRICTS.with(|t| {
            let mut t = t.borrow_mut();
            if t.len() >= 4096 { t.clear(); }
            t.insert(residual.0, kept.clone());
        });
        kept
    }

    #[derive(Clone, Default)]
    pub struct CompileOptions {
        pub trace: Option<core_lower::Trace>,
        pub profile: Option<Arc<Profile::T>>,
        pub strict: bool,
        pub approx: Paths,
        pub sink: Option<Sink>,
        pub count_source: Option<CountSource>,
    }

    fn kernel_form(span: &rays_diagnostic::Span) -> Diagnostic {
        rays_diagnostic::error(
            "E_KERNEL_FORM".into(),
            "Numeric node bodies must compile to packed kernels.".into(),
            Some(span.clone()),
            None,
            None,
            Vec::new(),
            Vec::new(),
        )
    }

    struct Validate {
        trace: Option<core_lower::Trace>,
        count_source: CountSource,
        inline: HashMap<i64, ()>,
        validated: HashMap<i64, ()>,
    }

    impl Validate {
        fn unsupported_source(&self, residual: ResidualId, term: &Term) -> Option<rays_diagnostic::Span> {
            self.unsupported_row(residual, term.arena(), term.row_id())
        }

        fn unsupported_row(&self, residual: ResidualId, arena: &Arc<flow_check::term_arena::TermArena>, id: flow_check::term_arena::TermId) -> Option<rays_diagnostic::Span> {
            let term = arena.row(id);
            let child = |id| self.unsupported_row(residual, arena, id);
            match term.node {
                Node::Hof(hof) if matches!(arena.hof(hof).kind, w::HofKind::Map | w::HofKind::Reduce) => self.region_source(residual, &arena.handle(id)),
                Node::Loop(_) => self.region_source(residual, &arena.handle(id)),
                Node::Op(op) if arena.name(arena.op(op).op) == "array/sum" => self.region_source(residual, &arena.handle(id)),
                Node::RefBinding(binding) => {
                    let binding = arena.ref_binding(binding);
                    let mut captured = e::Residual::residual_binding(residual, arena.name(binding.name));
                    for &field in arena.name_ids(binding.fields) {
                        captured = captured.and_then(|x| match x {
                            Value::Record(fs) | Value::Struct(_, _, fs) => fs.iter().find(|(n, _)| n == arena.name(field)).map(|(_, x)| x.clone()),
                            _ => None,
                        });
                    }
                    match captured {
                        Some(Value::Residual(r)) => {
                            let term = e::Residual::residual_term(r)?;
                            self.unsupported_source(r, &term)
                        }
                        _ => None,
                    }
                }
                Node::Bypass(call) => w::bypass_input(&arena.handle(call)).and_then(|input| child(input.row_id())),
                Node::Let(let_id) => {
                    let value = arena.let_node(let_id);
                    child(value.body).or_else(|| arena.bindings(value.bindings).iter().find_map(|binding| child(binding.term)))
                }
                Node::Cond(cond) => {
                    let value = arena.cond(cond);
                    child(value.default).or_else(|| arena.arms(value.arms).iter().find_map(|arm| child(arm.test).or_else(|| child(arm.body))))
                }
                Node::Case(case) => {
                    let value = arena.case(case);
                    child(value.value).or_else(|| child(value.default)).or_else(|| arena.case_arms(value.arms).iter().find_map(|arm| child(arm.body)))
                }
                Node::CallFn(call) => {
                    let call = arena.call_fn(call);
                    if let Some(span) = arena.term_ids(call.args).iter().find_map(|&id| child(id)) { return Some(span); }
                    let body = call.body?;
                    match e::Residual::residual_definition(residual, arena.name(call.name)) {
                        None => child(body),
                        Some(definition) => {
                            let bindings: Vec<(String, Value)> = definition
                                .inputs
                                .iter()
                                .enumerate()
                                .map(|(index, (name, ty, _))| {
                                    (name.clone(), Value::Source("$core/argument".into(), ty.clone(), vec![("index".into(), Value::Int(index as i64))]))
                                })
                                .collect();
                            let nested = e::Residual::with_bindings(residual, &arena.handle(body), &bindings).ok()?;
                            self.unsupported_row(nested, arena, body)
                        }
                    }
                }
                Node::Fn(function) => {
                    let function = arena.function(function);
                    let params = arena.params(function.params);
                    let signature = match arena.ty(term.ty) { Ty::Fn(Some(signature)) => Some(signature), _ => None };
                    let param_ty = |index: usize| match signature {
                        Some(signature) => signature.params.get(index),
                        None => params[index].ty.map(|ty| arena.ty(ty)),
                    };
                    let eligible = signature.is_none_or(|signature| signature.params.len() == params.len())
                        && params.iter().enumerate().all(|(index, param)| {
                            matches!(arena.pattern(param.pattern), flow_check::term_arena::PatternNode::Name(_))
                                && param_ty(index).map(|ty| match ty {
                                    Ty::Int | Ty::Float | Ty::Bool | Ty::Text | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => true,
                                    Ty::Array(inner) => matches!(**inner, Ty::Int | Ty::Float | Ty::Text | Ty::Vec2 | Ty::Vec3 | Ty::Vec4),
                                    _ => false,
                                }) == Some(true)
                        });
                    if eligible {
                        let bindings: Vec<(String, Value)> = params
                            .iter()
                            .enumerate()
                            .map(|(index, param)| {
                                let name = match arena.pattern(param.pattern) { flow_check::term_arena::PatternNode::Name(name) => arena.name(name).to_owned(), _ => unreachable!() };
                                (name, Value::Source("$core/argument".into(), param_ty(index).expect("typed").clone(), vec![("index".into(), Value::Int(index as i64))]))
                            })
                            .collect();
                        let nested = e::Residual::with_bindings(residual, &arena.handle(function.body), &bindings).ok()?;
                        self.unsupported_row(nested, arena, function.body)
                    } else {
                        child(function.body).or_else(|| function.capture.and_then(child))
                    }
                }
                _ => arena.children(id).iter().find_map(|&id| child(id)),
            }
        }

        fn region_source(&self, residual: ResidualId, term: &Term) -> Option<rays_diagnostic::Span> {
            let options = packed::CompileOptions { trace: self.trace.clone(), through_exact: false, fusion: true, count_source: Some(self.count_source.clone()) };
            match packed::compile_result(&options, residual, term) {
                Err(_) => Some(term.arena().syntax().row(term.form).span.clone()),
                Ok(p) => self.unsupported_packed_source(&p),
            }
        }

        fn unsupported_packed_source(&self, p: &Packed) -> Option<rays_diagnostic::Span> {
            if let Some(span) = p.sources.iter().find_map(|(residual, term)| self.unsupported_source(*residual, term)) { return Some(span); }
            p.uniforms.iter().find_map(|u| self.unsupported_captured(u))
        }

        fn unsupported_captured(&self, value: &Value) -> Option<rays_diagnostic::Span> {
            match value {
                Value::Residual(r) => {
                    let term = e::Residual::residual_term(*r)?;
                    self.unsupported_source(*r, &term)
                }
                Value::List(xs) => xs.iter().find_map(|x| self.unsupported_captured(x)),
                Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => fs.iter().find_map(|(_, x)| self.unsupported_captured(x)),
                _ => None,
            }
        }

        fn validate_residual(&mut self, residual: ResidualId) -> Result<(), Diagnostic> {
            let view = e::Residual::residual_view(residual)?;
            // Per-element Vec3 placeholders are runtime uniforms, not nested kernels.
            let element_capture = matches!(view.term.node, Node::RefBinding(id) if view.term.arena().name_ids(view.term.arena().ref_binding(id).fields).is_empty() && *view.term.ty() == Ty::Vec3 && view.term.name(view.term.arena().ref_binding(id).name).starts_with("$elem:"));
            if view.previous || element_capture || e::state_dependent(&Value::Residual(residual)) { return Ok(()); }
            let lowered = strict_lower(&self.trace, residual);
            for id in &lowered.1 { self.inline.insert(*id, ()); }
            match &lowered.0 {
                Ok(core) if core.regions.iter().all(|r| self.unsupported_packed_source(r).is_none()) => Ok(()),
                _ => Err(kernel_form(&view.term.arena().syntax().row(view.term.form).span)),
            }
        }

        // a residual shared by many (a matrix several bindings read) is validated once, and one a
        // strict lowering took into its program (a capture it reads) was lowered there
        fn validate_value(&mut self, value: &Value) -> Result<(), Diagnostic> {
            match value {
                Value::Residual(r) if self.validated.contains_key(&r.0) => Ok(()),
                Value::Residual(r) => {
                    self.validated.insert(r.0, ());
                    if !self.inline.contains_key(&r.0) { self.validate_residual(*r)?; }
                    let view = e::Residual::residual_view(*r)?;
                    for (_, x) in &view.bindings { self.validate_value(x)?; }
                    Ok(())
                }
                Value::List(xs) => {
                    for x in xs.iter() { self.validate_value(x)?; }
                    Ok(())
                }
                Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => {
                    for (_, x) in fs { self.validate_value(x)?; }
                    Ok(())
                }
                Value::Fn(f) => {
                    for (_, x) in e::Residual::function_bindings(*f) { self.validate_value(&x)?; }
                    Ok(())
                }
                _ => Ok(()),
            }
        }
    }

    pub fn compile(v: &Value, o: &CompileOptions) -> Result<Program, Diagnostic> {
        let _phase = flow_syntax::phase_timer::measure("executor.compile");
        let count_source = o.count_source.clone().unwrap_or_else(CountSource::none);
        if o.strict {
            let mut validate = Validate { trace: o.trace.clone(), count_source: count_source.clone(), inline: HashMap::new(), validated: HashMap::new() };
            validate.validate_value(v)?;
        }
        let packed_region = direct_region(&o.trace, false, false, &count_source, v).map(|(p, _)| p);
        let packed_root = match &packed_region {
            Some(p) => Some((p.clone(), false)),
            None => direct_region(&o.trace, false, true, &count_source, v),
        };
        Ok(Arc::new(ProgramData {
            value: v.clone(),
            candidates: Mutex::new(HashMap::new()),
            root_core: Mutex::new(None),
            packed_region,
            packed_root,
            trace: o.trace.clone(),
            count_source,
            profile: o.profile.clone(),
            approx: o.approx.clone(),
            sink: o.sink.clone(),
            gpu_kernel: Mutex::new(None),
            record_region: std::sync::OnceLock::new(),
        }))
    }

    /// The root's packed region, when the compiled value is one.
    pub fn packed_region(p: &Program) -> Option<Packed> { p.packed_region.clone() }
    /// Packed root, through transparent `exact` readback boundaries.
    pub fn packed_root(p: &Program) -> Option<Packed> { p.packed_root.as_ref().map(|(x, _)| x.clone()) }
    /// Whether the root directly runs a packed region or its exact readback.
    pub fn root_is_packed_or_readback(p: &Program) -> bool { p.packed_root.is_some() }

    /// Record fields whose arrays are freshly allocated by an elementwise packed root.
    pub fn fresh_csr_fields(p: &Program) -> Vec<String> {
        match &p.value {
            Value::Record(fields) => fields
                .iter()
                .filter_map(|(name, value)| {
                    if name == "offsets" || name == "values" {
                        direct_region(&p.trace, false, true, &p.count_source, value).and_then(|(packed, _)| if packed::elementwise(&packed) { Some(name.clone()) } else { None })
                    } else {
                        None
                    }
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// One timing per packed group, recorded when the group runs.
    fn measure(ran: Arc<std::sync::atomic::AtomicBool>, profile: &Option<Arc<Profile::T>>) -> Option<packed::Measure> {
        let profile = profile.clone()?;
        Some(packed::Measure {
            clock: profile.clock.clone(),
            report: {
                let profile = profile.clone();
                Arc::new(move |packed: &Packed, seconds: f64, reference: bool| {
                    ran.store(true, Ordering::Relaxed);
                    let sites = packed::provenance(packed);
                    let (_, path, iter) = packed::site(packed);
                    Profile::record(
                        &profile,
                        &Execution { owner: (path, iter), sites, seconds, tier: if reference { Tier::Interp } else { Tier::Cpu_kernel } },
                    );
                })
            },
        })
    }

    #[derive(Clone, Default)]
    pub struct ForceOptions {
        pub resident: Option<Arc<core_exec::Resident>>,
        pub state: Option<e::State>,
        pub elems: Option<Vec<(String, Value)>>,
        pub resolve: Option<core_exec::ResolveFn>,
        pub reference: bool,
        pub strict: bool,
    }

    /// Narrow test counters for lowering and reference-walker attempts in one force operation.
    #[derive(Clone, Default)]
    pub struct Counters {
        pub lower: Option<Arc<AtomicUsize>>,
        pub walker: Option<Arc<AtomicUsize>>,
    }

    pub struct Into {
        pub packed: Packed,
        pub column_source: packed::ColumnSource,
        pub count: i64,
        pub outputs: Arc<Mutex<Vec<Vec<Option<Vec<f64>>>>>>,
    }

    fn count_walk(counter: &Option<Arc<AtomicUsize>>) -> Arc<dyn Fn()> {
        let counter = counter.clone();
        Arc::new(move || {
            if let Some(c) = &counter { c.fetch_add(1, Ordering::Relaxed); }
        })
    }

    /// One force operation's caches and settings; `dispatch` recurses into it.
    struct Run {
        p: Program,
        o: ForceOptions,
        live: Frame,
        counters: Counters,
        measure: Option<packed::Measure>,
        forced: Mutex<HashMap<i64, Value>>,
        operation: Mutex<HashMap<(i64, bool), Candidate>>,
        fell_back: std::sync::atomic::AtomicBool,
        packed_ran: Arc<std::sync::atomic::AtomicBool>,
    }

    fn reference(run: &Run, value: &Value, state: &e::State) -> Result<Value, Diagnostic> {
        let walk = count_walk(&run.counters.walker);
        e::Host::force_reference_observed(value, &run.live, Some(state), run.o.elems.as_deref(), run.o.resolve.clone(), &*walk)
    }

    impl Run {
        fn candidate(&self, r: ResidualId) -> Candidate {
            let key = (r.0, self.o.strict);
            if let Some(found) = self.operation.lock().unwrap().get(&key) { return found.clone(); }
            let result = {
                let mut candidates = self.p.candidates.lock().unwrap();
                match candidates.get(&key) {
                    Some(found) => found.clone(),
                    None => {
                        if let Some(c) = &self.counters.lower { c.fetch_add(1, Ordering::Relaxed); }
                        let template = |r: ResidualId| -> Candidate {
                            let options = packed::CompileOptions {
                                trace: self.p.trace.clone(),
                                through_exact: false,
                                fusion: true,
                                count_source: Some(self.p.count_source.clone()),
                            };
                            match e::Residual::residual_view(r).ok().and_then(|view| packed::compile_template(&options, r, &view.term)) {
                                Some(p) => Candidate::Packed(p),
                                None => Candidate::Reference,
                            }
                        };
                        let candidate = match core_lower::lower(None, self.p.trace.clone(), self.o.strict, None, None, r) {
                            Ok(core) => {
                                // Placement declines before execution, while state is untouched.
                                let declined = !self.o.strict
                                    && core.regions.iter().any(|region| region.count.map(|count| Cost::packed(count) != Tier::Cpu_kernel).unwrap_or(false));
                                if declined { template(r) } else { Candidate::Core(Arc::new(core_exec::prepare(&core, false, false, &Paths::new(), false))) }
                            }
                            Err(_) => template(r),
                        };
                        if candidates.len() >= 128 { candidates.clear(); }
                        candidates.insert(key, candidate.clone());
                        candidate
                    }
                }
            };
            if let (Candidate::Core(prepared), Value::Residual(root)) = (&result, &self.p.value) {
                if *root == r && prepared.direct() { *self.p.root_core.lock().unwrap() = Some((self.o.strict, prepared.clone())); }
            }
            self.operation.lock().unwrap().insert(key, result.clone());
            result
        }

        fn dispatch_fn(self: &Arc<Self>) -> packed::Dispatch {
            let run = self.clone();
            Arc::new(move |residual, term| {
                let view = e::Residual::residual_view(residual)?;
                let residual = if *view.term == *term { residual } else { e::Residual::with_bindings(residual, term, &view.bindings)? };
                run.walk(&Value::Residual(residual))
            })
        }

        fn force_options(self: &Arc<Self>) -> packed::ForceOptions {
            packed::ForceOptions {
                dispatch: Some(self.dispatch_fn()),
                strict: self.o.strict,
                state: self.o.state.clone(),
                elems: self.o.elems.clone(),
                resolve: self.o.resolve.clone(),
                measure: self.measure.clone(),
                on_walk: Some({
                    let counter = self.counters.walker.clone();
                    Arc::new(move || {
                        if let Some(c) = &counter { c.fetch_add(1, Ordering::Relaxed); }
                    })
                }),
                ..Default::default()
            }
        }

        fn execute_core(self: &Arc<Self>, root: Option<ResidualId>, prepared: &core_exec::Prepared) -> Result<Value, Diagnostic> {
            let run = self.clone();
            let options = self.force_options();
            let live = self.live.clone();
            core_exec::execute(
                prepared,
                &self.live,
                &core_exec::Options {
                    root,
                    strict: self.o.strict,
                    rerun_on_error: false,
                    on_walk: Some(count_walk(&self.counters.walker)),
                    force: Some(Arc::new(move |p: &Packed| packed::force(p, &live, &options))),
                    resolve: self.o.resolve.clone(),
                    resident: self.o.resident.clone(),
                    host_source: None,
                },
            )
            .map_err(|d| {
                let _ = &run;
                d
            })
        }

        fn execute(self: &Arc<Self>, r: ResidualId, live: &Frame) -> Option<Result<Value, Diagnostic>> {
            match self.candidate(r) {
                Candidate::Core(p) => Some(self.execute_core(Some(r), &p)),
                Candidate::Packed(p)
                    if self.o.strict || !packed::static_count(&p).map(|count| Cost::packed(count) != Tier::Cpu_kernel).unwrap_or(false) =>
                {
                    Some(packed::force(&p, live, &self.force_options()))
                }
                _ => {
                    let view = e::Residual::residual_view(r).ok();
                    let needs_kernel = view.is_some_and(|view| match view.term.node {
                        Node::Hof(id) => matches!(view.term.arena().hof(id).kind, w::HofKind::Map | w::HofKind::Reduce),
                        Node::Loop(_) => true,
                        Node::Op(id) => view.term.name(view.term.arena().op(id).op) == "array/sum",
                        _ => false,
                    });
                    if self.o.strict && needs_kernel {
                        Some(Err(v::fail("E_KERNEL_FORM", "Numeric node bodies must compile to packed kernels.")))
                    } else {
                        None
                    }
                }
            }
        }

        fn walk(self: &Arc<Self>, value: &Value) -> Result<Value, Diagnostic> {
            let state = self.o.state.clone().expect("a force operation has a state");
            let strict_or_resident = self.o.strict || self.o.resident.is_some();
            match value {
                Value::Residual(r) => {
                    if let Some(found) = self.forced.lock().unwrap().get(&r.0) { return Ok(found.clone()); }
                    e::Residual::bind_live(*r, &self.live, Some(&state))?;
                    let restore = e::Residual::checkpoint(*r)?;
                    let result = match self.execute(*r, &self.live) {
                        Some(Ok(value)) => Ok(value),
                        Some(Err(d)) => {
                            if strict_or_resident {
                                Err(d)
                            } else {
                                restore();
                                self.fell_back.store(true, Ordering::Relaxed);
                                reference(self, &Value::Residual(*r), &state)
                            }
                        }
                        None => {
                            if strict_or_resident && !e::state_dependent(&Value::Residual(*r)) {
                                restore();
                                Err(v::fail("E_CORE_FORM", "This value needs a compiled executor."))
                            } else {
                                restore();
                                self.fell_back.store(true, Ordering::Relaxed);
                                reference(self, &Value::Residual(*r), &state)
                            }
                        }
                    }?;
                    self.forced.lock().unwrap().insert(r.0, result.clone());
                    self.walk(&result)
                }
                Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => {
                    let forced = reference(self, value, &state)?;
                    self.walk(&forced)
                }
                Value::List(values) => Ok(Value::List(Arc::new(values.iter().map(|x| self.walk(x)).collect::<Result<_, _>>()?))),
                Value::Record(fields) => Ok(Value::Record(self.walk_fields(fields)?)),
                Value::Struct(name, ty, fields) => Ok(Value::Struct(name.clone(), ty.clone(), self.walk_fields(fields)?)),
                other => Ok(other.clone()),
            }
        }

        fn walk_fields(self: &Arc<Self>, fields: &[(String, Value)]) -> Result<Vec<(String, Value)>, Diagnostic> {
            fields.iter().map(|(n, x)| Ok((n.clone(), self.walk(x)?))).collect()
        }
    }

    pub fn force_impl(program: &Program, o: &ForceOptions, into: Option<&Into>, counters: &Counters, live: &Frame) -> Result<Value, Diagnostic> {
        let fast = if into.is_some() || o.state.is_some() || o.elems.is_some() || o.resolve.is_some() || o.reference || program.profile.is_some() {
            None
        } else {
            match &*program.root_core.lock().unwrap() {
                // Region programs use the per-force dispatcher/resolver below, even when cached.
                Some((cached_strict, prepared)) if *cached_strict == o.strict && prepared.direct() => Some(prepared.clone()),
                _ => None,
            }
        };
        if let Some(prepared) = fast {
            if e::is_live(&program.value) {
                let result = core_exec::execute(
                    &prepared,
                    live,
                    &core_exec::Options { resident: o.resident.clone(), strict: o.strict, ..Default::default() },
                );
                return match result {
                    Ok(v) => Ok(v),
                    Err(_) if !o.strict && o.resident.is_none() => {
                        let walk = count_walk(&counters.walker);
                        e::Host::force_reference_observed(&program.value, live, None, None, None, &*walk)
                    }
                    Err(d) => Err(d),
                };
            }
        }
        let started = program.profile.as_ref().map(|p| (p.clock)());
        let packed_ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let mut fell_back = o.reference;
        let result: Result<Value, Diagnostic>;
        if !e::is_live(&program.value) {
            result = Ok(program.value.clone());
        } else {
            let state = o.state.clone().unwrap_or_else(e::create_state);
            let mut opts = o.clone();
            opts.state = Some(state.clone());
            let run = Arc::new(Run {
                p: program.clone(),
                o: opts,
                live: live.clone(),
                counters: counters.clone(),
                measure: measure(packed_ran.clone(), &program.profile),
                forced: Mutex::new(HashMap::new()),
                operation: Mutex::new(HashMap::new()),
                fell_back: std::sync::atomic::AtomicBool::new(false),
                packed_ran: packed_ran.clone(),
            });
            let fallback = |run: &Arc<Run>| -> Result<Value, Diagnostic> {
                run.fell_back.store(true, Ordering::Relaxed);
                reference(run, &program.value, &state)
            };
            let body = |run: &Arc<Run>| -> Result<Value, Diagnostic> {
                match into {
                    None => {
                        let record_core = match &program.value {
                            // a record over state its lane advanced reads it as data (Core_exec
                            // keeps a program that writes state off the resident device)
                            Value::Record(fields) if o.resident.is_some() || !(e::frame_dependent(&program.value) || e::state_dependent(&program.value)) => {
                                let root = fields.iter().find_map(|(_, x)| match x {
                                    Value::Residual(r)
                                        if o.resident.is_some() || e::Residual::residual_context(*r).is_some_and(|c| c == context::sop()) =>
                                    {
                                        Some(*r)
                                    }
                                    _ => None,
                                });
                                root.and_then(|root| {
                                    let cached = match &*program.root_core.lock().unwrap() {
                                        Some((cached_strict, prepared)) if *cached_strict == o.strict => Some(prepared.clone()),
                                        _ => None,
                                    };
                                    let prepared = match cached {
                                        Some(p) => Some(p),
                                        None => match core_lower::lower(Some(program.value.clone()), program.trace.clone(), o.strict, None, None, root) {
                                            Err(name) => {
                                                let site = e::Residual::residual_view(root).map(|v| v.site.join("/")).unwrap_or_default();
                                                fallback::note(&site, &format!("record does not lower: {name}"));
                                                None
                                            }
                                            Ok(core) => {
                                                let prepared = Arc::new(core_exec::prepare(&core, false, false, &Paths::new(), false));
                                                *program.root_core.lock().unwrap() = Some((o.strict, prepared.clone()));
                                                Some(prepared)
                                            }
                                        },
                                    };
                                    prepared.map(|p| (root, p))
                                })
                            }
                            _ => None,
                        };
                        match record_core {
                            None => run.walk(&program.value),
                            Some((root, prepared)) => {
                                // a stateful record's root, bound to this frame and state, carries
                                // what its reads see
                                let ran = if e::state_dependent(&program.value) {
                                    e::Residual::bind_live(root, live, Some(&state)).and_then(|_| run.execute_core(Some(root), &prepared))
                                } else {
                                    run.execute_core(None, &prepared)
                                };
                                match ran {
                                    Ok(result) => run.walk(&result),
                                    Err(d) if o.strict || o.resident.is_some() => Err(d),
                                    Err(_) => fallback(run),
                                }
                            }
                        }
                    }
                    Some(into) => {
                        let mut outputs = into.outputs.lock().unwrap();
                        packed::force_into(&into.packed, live, &run.force_options(), &into.column_source, into.count, &mut outputs).map(|_| Value::No_geo)
                    }
                }
            };
            let mut produced: Option<Result<Value, Diagnostic>> = None;
            let go = |run: &Arc<Run>| -> Result<Value, Diagnostic> { if o.reference { fallback(run) } else { body(run) } };
            let transacted = e::transaction(&state, &mut || {
                let r = go(&run);
                let ok = r.as_ref().map(|_| ()).map_err(|d| d.clone());
                produced = Some(r);
                ok
            });
            result = match (transacted, produced) {
                (_, Some(r)) => r,
                (Err(d), None) => Err(d),
                (Ok(()), None) => Ok(Value::No_geo),
            };
            fell_back = fell_back || run.fell_back.load(Ordering::Relaxed);
        }
        if let Some(profile) = &program.profile {
            if !packed_ran.load(Ordering::Relaxed) {
                let (id, sites): (Id, Vec<(i64, Path, Vec<i64>)>) = match &program.value {
                    Value::Residual(residual) => match e::Residual::residual_view(*residual) {
                        Ok(view) => {
                            let sites = match packed_root(program) {
                                Some(p) => packed::provenance(&p),
                                None => vec![(view.instance, view.site.clone(), view.iter.clone())],
                            };
                            ((view.site, view.iter), sites)
                        }
                        Err(_) => ((vec!["@value".to_owned()], Vec::new()), Vec::new()),
                    },
                    _ => ((vec!["@value".to_owned()], Vec::new()), Vec::new()),
                };
                let tier = if fell_back {
                    Tier::Interp
                } else {
                    match packed_root(program) {
                        Some(p) => Cost::packed(packed::static_count(&p).unwrap_or(1)),
                        None => Tier::Core,
                    }
                };
                let path = match id.0.split_first() {
                    Some((namespace, rest)) if namespace.starts_with("@instance:") => rest.to_vec(),
                    _ => id.0.clone(),
                };
                Profile::record(
                    profile,
                    &Execution { owner: (path, id.1), sites, tier, seconds: ((profile.clock)() - started.unwrap_or(0.0)).max(0.0) },
                );
            }
        }
        result
    }

    /// Selected/probed cones use `reference`. Unsupported terms keep the reference tree walker;
    /// supported scalar dataflow uses its operator records. Failures rerun the reference for its
    /// precise diagnostic. `strict` requires packed numeric bodies at compilation and execution,
    /// without cost-based interpreter placement or error fallback. A failed resident attempt
    /// closes the scope and retries the whole value on CPU under the requested `strict` policy.
    pub fn force(program: &Program, o: &ForceOptions, live: &Frame) -> Result<Value, Diagnostic> {
        match force_impl(program, o, None, &Counters::default(), live) {
            Err(_) if o.resident.is_some() => {
                // A failed field closed the shared scope: earlier fields' placeholders are invalid too.
                (o.resident.as_ref().unwrap().close)();
                let mut o = o.clone();
                o.resident = None;
                force_impl(program, &o, None, &Counters::default(), live)
            }
            result => result,
        }
    }

    pub mod Private {
        use super::*;

        pub fn force_with_lower_attempts(program: &Program, o: &ForceOptions, live: &Frame) -> (Result<Value, Diagnostic>, usize) {
            let count = Arc::new(AtomicUsize::new(0));
            let result = force_impl(program, o, None, &Counters { lower: Some(count.clone()), walker: None }, live);
            (result, count.load(Ordering::Relaxed))
        }

        pub fn force_with_walker_counts(program: &Program, o: &ForceOptions, live: &Frame) -> (Result<Value, Diagnostic>, usize, usize) {
            let lower = Arc::new(AtomicUsize::new(0));
            let walker = Arc::new(AtomicUsize::new(0));
            let result = force_impl(program, o, None, &Counters { lower: Some(lower.clone()), walker: Some(walker.clone()) }, live);
            (result, lower.load(Ordering::Relaxed), walker.load(Ordering::Relaxed))
        }
    }

    fn force_into_cpu(
        o: &ForceOptions,
        into: &Into,
        program: &Program,
        live: &Frame,
    ) -> Result<Option<()>, Diagnostic> {
        match packed_root(program) {
            Some(p) if packed::elementwise(&p) => force_impl(program, o, Some(into), &Counters::default(), live).map(|_| Some(())),
            _ => Ok(None),
        }
    }

    #[derive(Clone)]
    pub enum Displayed {
        Cpu(Value),
        Gpu(Gpu::Value),
    }

    /// The owner's one-kernel cache.
    pub struct KernelCache {
        pub cached: Box<dyn Fn() -> Option<(Arc<Gpu::Backend>, bool, Packed, Arc<Gpu::Kernel>)>>,
        pub store: Box<dyn Fn((Arc<Gpu::Backend>, bool, Packed, Arc<Gpu::Kernel>))>,
    }

    /// A packed region on the GPU when the backend takes it and measures it cheaper. `display` says
    /// a GPU array may leave the executor.
    fn display_packed(
        state: Option<&e::State>,
        resolve: Option<&core_exec::ResolveFn>,
        source: Option<&packed::host::SourceFn>,
        gpu: &Gpu::Config,
        approx: &Paths,
        display: bool,
        profile: &Option<Arc<Profile::T>>,
        cache: &KernelCache,
        readback: bool,
        packed_region: &Packed,
        live: &Frame,
    ) -> Result<Option<Displayed>, Diagnostic> {
        let refused = |reason: PlacementReason| -> Result<Option<Displayed>, Diagnostic> {
            let (_, path, iter) = packed::site(packed_region);
            Profile::place(
                profile.as_ref(),
                &Placement {
                    owner: (path, iter),
                    count: packed::static_count(packed_region),
                    reason,
                    cpu_seconds: None,
                    device_seconds: None,
                    cpu_consumers: Some(if readback { 1 } else { 0 }),
                },
            );
            Ok(None)
        };
        let Some(backend) = gpu.backend.clone() else { return refused(PlacementReason::No_cost_evidence) };
        let fast = gpu.fast;
        let (_, site, _) = packed::site(packed_region);
        // fast precision: a displayed region needs no qualified path, its only use is the screen
        if !((fast && display) || approx.contains(&site)) || !packed::gpu_refusals(true, fast, packed_region).is_empty() {
            return refused(PlacementReason::Unsupported_form);
        }
        let cheaper = |inputs: Option<&packed::Inputs>, count: i64| -> bool {
            if gpu.policy == Gpu::Policy::Qualification { return true; }
            let cpu_seconds = Some(Cost::estimate(Cost::packed(count), count) + if readback { 0.0 } else { Cost::display_sink(Tier::Cpu_kernel, count) });
            let device_seconds = (backend.cost)(inputs, fast, packed_region, count).and_then(|seconds| {
                if !(seconds.is_finite() && seconds >= 0.0) {
                    None
                } else {
                    Some(seconds + if readback { Cost::outputs_readback(Some(gpu), packed_region, count) } else { Cost::display_sink(Tier::Gpu, count) })
                }
            });
            let cpu_consumers = if readback { 1 } else { 0 };
            let reason = Cost::choose(cpu_seconds, device_seconds, false, cpu_consumers);
            let (_, path, iter) = packed::site(packed_region);
            Profile::place(
                profile.as_ref(),
                &Placement { owner: (path, iter), count: Some(count), reason, cpu_seconds, device_seconds, cpu_consumers: Some(cpu_consumers) },
            );
            Cost::on_device(reason) || (inputs.is_none() && reason == PlacementReason::No_cost_evidence)
        };
        // Static cardinality lets unmeasured placement decline before allocating producer inputs,
        // especially the million-element exact SOP value lane.
        if scalar_uniforms(packed_region) && packed::static_count(packed_region).map(|count| !cheaper(None, count)).unwrap_or(false) {
            return Ok(None);
        }
        let inputs = packed::host::prepare(
            packed_region,
            &packed::host::PrepareOptions { fast, state: state.cloned(), resolve: resolve.cloned(), source: source.cloned(), ..Default::default() },
            live,
        )?;
        if !cheaper(Some(&inputs), inputs.count) { return Ok(None); }
        if !(display || readback) {
            let _ = refused(PlacementReason::Missing_consumer);
            return Err(v::fail(
                "E_APPROX_SINK",
                "GPU arrays require a display sink or (exact x) before CPU, SOP, state, export or cache use.",
            ));
        }
        let timed = |tier: Tier, seconds: &dyn Fn(&Gpu::Value) -> Option<f64>, run: &mut dyn FnMut() -> Result<Gpu::Value, Diagnostic>| -> Result<Gpu::Value, Diagnostic> {
            let start = profile.as_ref().map(|p| (p.clock)());
            let result = run();
            let elapsed = match &result { Ok(value) => seconds(value), Err(_) => None };
            if let Some(p) = profile {
                let (_, path, iter) = packed::site(packed_region);
                Profile::record(
                    p,
                    &Execution {
                        owner: (path, iter),
                        sites: packed::provenance(packed_region),
                        tier,
                        seconds: match elapsed {
                            Some(value) if value.is_finite() && value >= 0.0 => value,
                            _ => ((p.clock)() - start.unwrap_or(0.0)).max(0.0),
                        },
                    },
                );
            }
            result
        };
        let kernel = match (cache.cached)() {
            Some((previous, previous_fast, p, kernel)) if Arc::ptr_eq(&previous, &backend) && previous_fast == fast && p.residual == packed_region.residual && Arc::ptr_eq(&p.code, &packed_region.code) => kernel,
            _ => {
                let start = profile.as_ref().map(|p| (p.clock)());
                let result = (backend.prepare)(fast, packed_region);
                if let Some(p) = profile {
                    let (_, path, iter) = packed::site(packed_region);
                    Profile::record(
                        p,
                        &Execution {
                            owner: (path, iter),
                            sites: packed::provenance(packed_region),
                            tier: Tier::Gpu_compile,
                            seconds: ((p.clock)() - start.unwrap_or(0.0)).max(0.0),
                        },
                    );
                }
                let kernel = result?;
                (cache.store)((backend.clone(), fast, packed_region.clone(), kernel.clone()));
                kernel
            }
        };
        let output = timed(Tier::Gpu, &|o: &Gpu::Value| o.gpu_seconds, &mut || (kernel.run)(&inputs))?;
        if readback {
            let value = timed_value(profile, packed_region, Tier::Gpu_readback, &mut || (kernel.readback)(&output))?;
            Ok(Some(Displayed::Cpu(value)))
        } else {
            Ok(Some(Displayed::Gpu(output)))
        }
    }

    fn timed_value(profile: &Option<Arc<Profile::T>>, packed_region: &Packed, tier: Tier, run: &mut dyn FnMut() -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
        let start = profile.as_ref().map(|p| (p.clock)());
        let result = run();
        if let Some(p) = profile {
            let (_, path, iter) = packed::site(packed_region);
            Profile::record(
                p,
                &Execution { owner: (path, iter), sites: packed::provenance(packed_region), tier, seconds: ((p.clock)() - start.unwrap_or(0.0)).max(0.0) },
            );
        }
        result
    }

    fn program_cache(program: &Program) -> KernelCache {
        let a = program.clone();
        let b = program.clone();
        KernelCache {
            cached: Box::new(move || a.gpu_kernel.lock().unwrap().clone()),
            store: Box::new(move |k| *b.gpu_kernel.lock().unwrap() = Some(k)),
        }
    }

    /// Select a GPU producer without executing the CPU fallback. `None` leaves ordinary worker
    /// cooking intact; explicit readbacks return owned CPU data.
    pub fn try_display(
        state: Option<&e::State>,
        resolve: Option<&core_exec::ResolveFn>,
        reference: bool,
        gpu: &Gpu::Config,
        program: &Program,
        live: &Frame,
    ) -> Result<Option<Displayed>, Diagnostic> {
        if reference || gpu.backend.is_none() { return Ok(None); }
        match &program.packed_root {
            None => Ok(None),
            Some((packed_region, readback)) => display_packed(
                state,
                resolve,
                None,
                gpu,
                &program.approx,
                matches!(program.sink, Some(Sink::Display(_))),
                &program.profile,
                &program_cache(program),
                *readback || program.sink == Some(Sink::Sop_input),
                packed_region,
                live,
            ),
        }
    }

    /// `region_display` is the placement a draw program's region gets as a mark attribute: the GPU
    /// output when the current backend takes the region (its site is in `approx`, it has no GPU
    /// refusal, the static or run-time count is at least 1,024 and the cost model or
    /// `Qualification` says so), else `None` for the CPU. It keeps the kernels of the last 64
    /// regions it saw.
    pub struct RegionDisplay {
        gpu: Gpu::Config,
        profile: Option<Arc<Profile::T>>,
        approx: Paths,
        kernels: Arc<Mutex<Vec<(bool, Arc<Gpu::Backend>, Packed, Arc<Gpu::Kernel>)>>>,
        ranges: Arc<Mutex<HashMap<i64, Value>>>,
    }

    pub fn region_display(gpu: Option<Gpu::Config>, profile: Option<Arc<Profile::T>>, approx: Option<Paths>) -> RegionDisplay {
        RegionDisplay {
            gpu: gpu.unwrap_or_else(Gpu::default),
            profile,
            approx: approx.unwrap_or_default(),
            kernels: Arc::new(Mutex::new(Vec::new())),
            // ponytail: ranges bounded by count (<= 64M floats) and 16 entries
            ranges: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    impl RegionDisplay {
        pub fn call(&self, state: Option<&e::State>, resolve: Option<&core_exec::ResolveFn>, packed_region: &Packed, live: &Frame) -> Result<Option<Gpu::Value>, Diagnostic> {
            if self.gpu.backend.is_none() { return Ok(None); }
            let ranges = self.ranges.clone();
            let source: packed::host::SourceFn = Arc::new(move |residual: ResidualId, term: &Term| -> Result<Option<Value>, Diagnostic> {
                fn find(ranges: &Mutex<HashMap<i64, Value>>, residual: ResidualId, term: &Term) -> Result<Option<Value>, Diagnostic> {
                    let arena = term.arena();
                    match term.node {
                        Node::RefBinding(id) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => {
                            let view = e::Residual::residual_view(residual)?;
                            match view.bindings.iter().find(|(n, _)| n == term.name(arena.ref_binding(id).name)) {
                                Some((_, Value::Residual(r))) => {
                                    let rv = e::Residual::residual_view(*r)?;
                                    find(ranges, *r, &rv.term)
                                }
                                _ => Ok(None),
                            }
                        }
                        Node::Op(id) if term.name(arena.op(id).op) == "array/range" && arena.fields(arena.op(id).args).len() == 1 && term.name(arena.fields(arena.op(id).args)[0].name) == "count" => {
                            if let Node::Lit(literal) = arena.row(arena.fields(arena.op(id).args)[0].term).node {
                                if let flow_check::term_arena::Literal::Int(count) = arena.literal(literal) {
                                    if matches!(term.ty(), Ty::Array(inner) if **inner == Ty::Float) && count >= 0 && count <= 64 * 1024 * 1024 / 8 {
                                        if let Some(found) = ranges.lock().unwrap().get(&count) { return Ok(Some(found.clone())); }
                                        let value = e::Residual::eval_term(residual, term, &input::at_time(0.0), None, None, None)?;
                                        let mut table = ranges.lock().unwrap();
                                        if table.len() >= 16 { table.clear(); }
                                        table.insert(count, value.clone());
                                        return Ok(Some(value));
                                    }
                                }
                            }
                            Ok(None)
                        }
                        _ => Ok(None),
                    }
                }
                find(&ranges, residual, term)
            });
            // a draw binds a fresh `packed` each frame over its site's code: kernels are kept by the code
            let kernels = self.kernels.clone();
            let kernels2 = self.kernels.clone();
            let gpu_fast = self.gpu.fast;
            let code = packed_region.code.clone();
            let cache = KernelCache {
                cached: Box::new(move || {
                    kernels.lock().unwrap().iter().find_map(|(fast, b, p, kernel)| {
                        if *fast == gpu_fast && Arc::ptr_eq(&p.code, &code) { Some((b.clone(), *fast, p.clone(), kernel.clone())) } else { None }
                    })
                }),
                store: Box::new(move |(b, _, p, kernel)| {
                    let mut k = kernels2.lock().unwrap();
                    k.retain(|(f, _, q, _)| !(*f == gpu_fast && Arc::ptr_eq(&q.code, &p.code)));
                    k.insert(0, (gpu_fast, b, p, kernel));
                    k.truncate(64);
                }),
            };
            let result = display_packed(state, resolve, Some(&source), &self.gpu, &self.approx, true, &self.profile, &cache, false, packed_region, live)?;
            Ok(result.map(|d| match d {
                Displayed::Gpu(v) => v,
                Displayed::Cpu(_) => unreachable!("a display placement is never a readback"),
            }))
        }
    }

    /// In fast precision a region of 4,096 elements or more that a drawing computes with (not a
    /// mark attribute) runs on the current backend and is read back as a 32-bit array; `None`
    /// leaves it to the CPU. Kernels are kept by the region's form, at most 16.
    /// ponytail: a fixed 4,096-element threshold, not the measured cost.
    pub struct RegionReadback {
        gpu: Gpu::Config,
        kernels: Mutex<Vec<(bool, String, Arc<Gpu::Backend>, Arc<Gpu::Kernel>)>>,
    }

    pub fn region_readback(gpu: Option<Gpu::Config>) -> RegionReadback {
        RegionReadback { gpu: gpu.unwrap_or_else(Gpu::default), kernels: Mutex::new(Vec::new()) }
    }

    impl RegionReadback {
        pub fn call(&self, packed_region: &Packed, live: &Frame) -> Result<Option<Value>, Diagnostic> {
            let gpu = &self.gpu;
            if !gpu.fast { return Ok(None); }
            let Some(backend) = gpu.backend.clone() else { return Ok(None) };
            if !packed::gpu_refusals(true, gpu.fast, packed_region).is_empty() { return Ok(None); }
            let inputs = packed::host::prepare(packed_region, &packed::host::PrepareOptions { fast: gpu.fast, ..Default::default() }, live)?;
            if inputs.count < 4096 { return Ok(None); }
            let key = format!("{:?}", (&*packed_region.code, &*packed_region.widths, &*packed_region.outputs));
            let found = self.kernels.lock().unwrap().iter().find(|(f, k, b, _)| *f == gpu.fast && Arc::ptr_eq(b, &backend) && *k == key).map(|(_, _, _, kernel)| kernel.clone());
            let kernel = match found {
                Some(k) => k,
                None => {
                    let kernel = (backend.prepare)(gpu.fast, packed_region)?;
                    let mut kernels = self.kernels.lock().unwrap();
                    kernels.insert(0, (gpu.fast, key, backend.clone(), kernel.clone()));
                    kernels.truncate(16);
                    kernel
                }
            };
            let output = (kernel.run)(&inputs)?;
            // the device carries integers as floats: an Int map comes back as the Int array it is
            let value = (kernel.readback)(&output)?;
            Ok(Some(match (value, packed_region.term.ty()) {
                (Value::Float_array(xs), Ty::Array(inner)) if **inner == Ty::Int => Value::Int_array(Arc::new(xs.iter().map(|x| *x as i64).collect())),
                (value, _) => value,
            }))
        }
    }

    /// Execute a collecting packed root into disjoint owned component planes. `None` leaves a
    /// scalar or unsupported root on its ordinary execution path. Borrowed inputs remain immutable;
    /// omitted output planes are still checked.
    pub fn force_into(
        o: &ForceOptions,
        gpu: Option<Gpu::Config>,
        column_source: packed::ColumnSource,
        count: i64,
        outputs: Arc<Mutex<Vec<Vec<Option<Vec<f64>>>>>>,
        program: &Program,
        live: &Frame,
    ) -> Result<Option<()>, Diagnostic> {
        let gpu = gpu.unwrap_or_else(Gpu::default);
        let widths = outputs.lock().unwrap().len();
        let into_cpu = || {
            let packed_root = packed_root(program).ok_or_else(|| v::fail("E_CORE", "no packed root"));
            match packed_root {
                Ok(p) => force_into_cpu(o, &Into { packed: p, column_source: column_source.clone(), count, outputs: outputs.clone() }, program, live),
                Err(_) => Ok(None),
            }
        };
        match (&program.sink, widths, &gpu.backend, packed_root(program)) {
            (Some(Sink::Sop_input), 1, Some(_), Some(p)) if packed::elementwise(&p) => {
                let (packed_region, _) = program.packed_root.clone().expect("packed root");
                match display_packed(
                    o.state.as_ref(),
                    o.resolve.as_ref(),
                    None,
                    &gpu,
                    &program.approx,
                    false,
                    &program.profile,
                    &program_cache(program),
                    true,
                    &packed_region,
                    live,
                ) {
                    Ok(Some(Displayed::Cpu(value))) => {
                        let (data, width) = match &value {
                            Value::Float_array(d) => (Some(d.clone()), 1),
                            Value::Vec2_array(d) => (Some(d.clone()), 2),
                            Value::Vec3_array(d) => (Some(d.clone()), 3),
                            Value::Vec4_array(d) => (Some(d.clone()), 4),
                            _ => (None, 0),
                        };
                        let planes = outputs.lock().unwrap()[0].len();
                        match data {
                            Some(data) if width != 0 && width == planes && data.len() as i64 == count * width as i64 => {
                                let mut out = outputs.lock().unwrap();
                                for (component, plane) in out[0].iter_mut().enumerate() {
                                    if let Some(plane) = plane {
                                        for i in 0..count as usize { plane[i] = data[i * width + component]; }
                                    }
                                }
                                Ok(Some(()))
                            }
                            _ => into_cpu(),
                        }
                    }
                    Ok(_) | Err(_) => into_cpu(),
                }
            }
            _ => into_cpu(),
        }
    }

    fn record_region(program: &Program) -> Option<(Vec<String>, Packed)> {
        program
            .record_region
            .get_or_init(|| match &program.value {
                Value::Record(fields) => {
                    let regions: Vec<(String, Packed)> = fields
                        .iter()
                        .filter_map(|(name, value)| {
                            direct_region(&program.trace, true, true, &program.count_source, value)
                                .and_then(|(packed, _)| if packed::elementwise(&packed) { Some((name.clone(), packed)) } else { None })
                        })
                        .collect();
                    if regions.len() != fields.len() { return None; }
                    let packed_list: Vec<Packed> = regions.iter().map(|(_, p)| p.clone()).collect();
                    core_fuse::merge(&packed_list).map(core_fuse::compact_sources).map(|merged| (regions.into_iter().map(|(n, _)| n).collect(), merged))
                }
                _ => None,
            })
            .clone()
    }

    #[derive(Clone, Default)]
    pub struct RecordOptions {
        pub state: Option<e::State>,
        pub resolve: Option<core_exec::ResolveFn>,
        /// a source bound as the value `v` (an attribute read) that the device already holds: its
        /// placeholder and count, passed instead of evaluating it
        pub resident: Option<Arc<dyn Fn(&Value) -> Option<(Value, i64)> + Send + Sync>>,
        pub expected_count: Option<i64>,
        pub cpu_consumers: Option<i64>,
        /// seconds added to the CPU alternative: what its result costs downstream that the device
        /// one does not
        pub cpu_penalty: f64,
        /// prices the CPU route of this site from its measured cooks
        pub cpu_cost: Option<Arc<dyn Fn(&Id, i64) -> Option<f64> + Send + Sync>>,
        pub device_first: Option<Arc<dyn Fn(&Id, i64, Option<f64>) -> bool + Send + Sync>>,
        pub gpu: Option<Gpu::Config>,
    }

    /// The elementwise fields of a record program (a fused SOP write's) as one region on the
    /// device, in fast precision when the shared measured comparison places it: one dispatch, each
    /// output kept on the device as its planes, by field name, read only when they materialise.
    /// `None` keeps the record on the CPU, also when the device fails.
    pub fn record_on_device(program: &Program, o: &RecordOptions, live: &Frame) -> Result<Option<Vec<(String, Gpu::Planes)>>, Diagnostic> {
        let gpu = o.gpu.clone().unwrap_or_else(Gpu::default);
        if let Some(count) = o.cpu_consumers { assert!(count >= 0, "record_on_device: negative consumers"); }
        if let Some(count) = o.expected_count { assert!(count >= 0, "record_on_device: negative expected count"); }
        if gpu.backend.is_none() {
            fn site(value: &Value) -> Option<Id> {
                match value {
                    Value::Residual(r) => e::Residual::residual_view(*r).ok().map(|view| (view.site, view.iter)),
                    Value::Record(fields) => fields.iter().rev().find_map(|(_, x)| site(x)),
                    _ => None,
                }
            }
            Profile::place(
                program.profile.as_ref(),
                &Placement {
                    owner: site(&program.value).unwrap_or_else(|| (vec!["@value".to_owned()], Vec::new())),
                    count: o.expected_count,
                    reason: PlacementReason::No_cost_evidence,
                    cpu_seconds: None,
                    device_seconds: None,
                    cpu_consumers: o.cpu_consumers,
                },
            );
            return Ok(None);
        }
        let report = |merged: &Packed, count: Option<i64>, reason: PlacementReason, cpu_seconds: Option<f64>, device_seconds: Option<f64>| {
            let (_, path, iter) = packed::site(merged);
            Profile::place(
                program.profile.as_ref(),
                &Placement { owner: (path, iter), count, reason, cpu_seconds, device_seconds, cpu_consumers: o.cpu_consumers },
            );
        };
        let region = record_region(program);
        let backend = gpu.backend.clone().expect("backend");
        match region {
            Some((names, merged)) if gpu.fast && packed::gpu_refusals(true, true, &merged).is_empty() => {
                if gpu.policy == Gpu::Policy::Measured && o.cpu_consumers.is_none() {
                    report(&merged, packed::static_count(&merged), PlacementReason::Missing_consumer, None, None);
                    return Ok(None);
                }
                let select = |inputs: Option<&packed::Inputs>, preflight: bool, held: bool, held_readback: f64, count: i64| -> bool {
                    if gpu.policy == Gpu::Policy::Qualification { return true; }
                    let readers = o.cpu_consumers.expect("consumers known");
                    let cpu_seconds = (match &o.cpu_cost {
                        None => Some(Cost::estimate(Cost::packed(count), count)),
                        Some(cost) => {
                            let (_, path, iter) = packed::site(&merged);
                            cost(&(path, iter), count)
                        }
                    })
                    .map(|seconds| seconds + held_readback + o.cpu_penalty);
                    let first = if cpu_seconds.is_none() { o.device_first.clone() } else { None };
                    // no CPU evidence and no device-first rule decides nothing, so it calibrates nothing
                    let device_seconds = if cpu_seconds.is_none() && first.is_none() {
                        None
                    } else {
                        (backend.cost)(inputs, true, &merged, count).and_then(|seconds| {
                            if !(seconds.is_finite() && seconds >= 0.0) {
                                None
                            } else {
                                Some(seconds + if readers == 0 { 0.0 } else { Cost::outputs_readback(Some(&gpu), &merged, count) })
                            }
                        })
                    };
                    let reason = Cost::choose(cpu_seconds, device_seconds, held, readers);
                    report(&merged, Some(count), reason, cpu_seconds, device_seconds);
                    Cost::on_device(reason)
                        || (preflight && reason == PlacementReason::No_cost_evidence && (cpu_seconds.is_some() || first.is_some()))
                        || (!preflight
                            && match &first {
                                Some(first) => {
                                    let (_, path, iter) = packed::site(&merged);
                                    // a CPU sample over input the device holds would read it back to
                                    // learn nothing: that readback is already what the CPU route costs
                                    held || first(&(path, iter), count, device_seconds)
                                }
                                None => false,
                            })
                };
                let preflight = if o.resident.is_none() && scalar_uniforms(&merged) {
                    o.expected_count.map(|count| (count, select(None, true, false, 0.0, count)))
                } else {
                    None
                };
                if matches!(preflight, Some((_, false))) { return Ok(None); }
                let held = Arc::new(Mutex::new((false, Vec::<Value>::new(), 0.0f64)));
                let resident: Option<Arc<dyn Fn(&Value) -> Option<(Value, i64)> + Send + Sync>> = o.resident.clone().map(|resident| {
                    let held = held.clone();
                    let gpu = gpu.clone();
                    Arc::new(move |value: &Value| {
                        let found = resident(value);
                        if let Some((v0, count)) = &found {
                            let mut h = held.lock().unwrap();
                            if !h.1.iter().any(|old| old == v0) {
                                h.0 = true;
                                h.1.insert(0, v0.clone());
                                let width = match v0 { Value::Vec2_array(_) => 2, Value::Vec3_array(_) => 3, Value::Vec4_array(_) => 4, _ => 1 };
                                h.2 += Cost::readback(Some(&gpu), *count, width);
                            }
                        }
                        found
                    }) as Arc<dyn Fn(&Value) -> Option<(Value, i64)> + Send + Sync>
                });
                let device = (|| -> Result<Option<Vec<(String, Gpu::Planes)>>, Diagnostic> {
                    let inputs = packed::host::prepare(
                        &merged,
                        &packed::host::PrepareOptions {
                            fast: true,
                            index: true,
                            state: o.state.clone(),
                            resolve: o.resolve.clone(),
                            resident: resident.clone(),
                            ..Default::default()
                        },
                        live,
                    )?;
                    let (is_held, _, readback) = held.lock().unwrap().clone();
                    if !select(Some(&inputs), false, is_held, readback, inputs.count) { return Ok(None); }
                    let cached = program.gpu_kernel.lock().unwrap().clone();
                    let kernel = match cached {
                        Some((b, true, p, kernel)) if Arc::ptr_eq(&b, &backend) && Arc::ptr_eq(&p.code, &merged.code) && p.residual == merged.residual => kernel,
                        _ => {
                            let kernel = (backend.prepare)(true, &merged)?;
                            *program.gpu_kernel.lock().unwrap() = Some((backend.clone(), true, merged.clone(), kernel.clone()));
                            kernel
                        }
                    };
                    let output = (kernel.run)(&inputs)?;
                    let mut values: Vec<(String, Gpu::Planes)> = Vec::new();
                    for (i, name) in names.iter().enumerate() {
                        let planes = (kernel.planes)(&Gpu::Value { output: i as i64, width: merged.outputs[i].len() as i64, ..output.clone() });
                        match planes {
                            Ok(planes) => values.push((name.clone(), planes)),
                            Err(d) => {
                                for (_, earlier) in &values { (earlier.release)(false); }
                                return Err(d);
                            }
                        }
                    }
                    Ok(Some(values))
                })();
                match device {
                    Ok(placed) => Ok(placed),
                    Err(_) => {
                        report(&merged, packed::static_count(&merged), PlacementReason::Unsupported_form, None, None);
                        Ok(None)
                    }
                }
            }
            Some((_, merged)) => {
                report(
                    &merged,
                    packed::static_count(&merged),
                    if gpu.backend.is_none() { PlacementReason::No_cost_evidence } else { PlacementReason::Unsupported_form },
                    None,
                    None,
                );
                Ok(None)
            }
            None => Ok(None),
        }
    }

    pub fn force_display(state: Option<&e::State>, reference: bool, gpu: Option<Gpu::Config>, program: &Program, live: &Frame) -> Result<Displayed, Diagnostic> {
        let gpu = gpu.unwrap_or_else(Gpu::default);
        match try_display(state, None, reference, &gpu, program, live)? {
            Some(value) => Ok(value),
            None => {
                let o = ForceOptions { state: state.cloned(), reference, ..Default::default() };
                force(program, &o, live).map(Displayed::Cpu)
            }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Tests: ports of lib/flow_ir/test_ir.ml (cost, qualification), test_core_resident.ml,
// test_core_parity.ml and test_frame_kernel.ml.

#[cfg(test)]
mod tests {
    use super::*;
    use flow_check::catalog::{self, Catalog};
    use flow_syntax::syntax;
    use std::sync::atomic::AtomicBool;

    /// OCaml static_map: a kernel run that raises is a refusal with E_PACKED_EXECUTION, not a dead cook.
    #[test]
    fn a_panicking_static_kernel_is_a_refusal() {
        let error = guarded_force(&mut || panic!("index out of bounds: the len is 3")).expect_err("a panic is an error");
        assert_eq!(error.code, "E_PACKED_EXECUTION");
        assert!(error.message.contains("index out of bounds"), "{}", error.message);
        assert_eq!(guarded_force(&mut || Ok(Value::Int(7))).unwrap(), Value::Int(7));
        let kept = guarded_force(&mut || Err(v::fail("E_X", "kept"))).expect_err("an Err passes through");
        assert_eq!(kept.code, "E_X");
    }

    fn empty() -> Catalog { Catalog { version: 1, kinds: vec![] } }

    fn manifest() -> &'static Catalog {
        static C: std::sync::OnceLock<Catalog> = std::sync::OnceLock::new();
        C.get_or_init(|| {
            let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../lib/flow_sop/flow_manifest.sexp");
            catalog::catalog_of_manifest(&std::fs::read_to_string(path).unwrap()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))).0
        })
    }

    fn show(ds: &[Diagnostic]) -> String { ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ") }

    fn check_in(catalog: &Catalog, source: &str) -> w::Workspace {
        let forms = syntax::parse(source).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        match w::check(&[], false, false, None, catalog, &forms) {
            (Some(workspace), ds) if !ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) => workspace,
            (_, ds) => panic!("{}", show(&ds)),
        }
    }

    fn ok<X>(r: Result<X, Diagnostic>) -> X { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }

    // --- Cost.choose -------------------------------------------------------------------------

    #[test]
    fn cost_choose_picks_by_measured_total_time() {
        let choose = |cpu: Option<f64>, device: Option<f64>, resident: bool, consumers: i64| Cost::choose(cpu, device, resident, consumers);
        for bad in [None, Some(f64::NAN), Some(f64::INFINITY), Some(f64::NEG_INFINITY), Some(-1e-12)] {
            assert_eq!(choose(bad, Some(1.0), false, 0), PlacementReason::No_cost_evidence);
            assert_eq!(choose(Some(1.0), bad, false, 0), PlacementReason::No_cost_evidence);
        }
        assert_eq!(choose(Some(1.0), Some(0.5), false, 0), PlacementReason::Device_measured_cheaper);
        assert_eq!(choose(Some(1.0), Some(0.5), true, 0), PlacementReason::Held_on_device);
        assert_eq!(choose(Some(1.0), Some(1.0), false, 0), PlacementReason::Cpu_measured_cheaper);
        assert_eq!(choose(Some(1.0), Some(1.5), true, 1), PlacementReason::Cpu_consumer);
        assert!(!Cost::on_device(choose(None, None, true, 0)));
        assert_eq!(Cost::reason_name(PlacementReason::Cpu_consumer), "cpu_consumer");
    }

    // --- the resident device emulator (test_core_resident.ml) ----------------------------------

    fn compile_value(body: &str) -> (Value, c::Program, core_exec::Prepared) {
        let body = format!("(if (>= t 0.) {body} {body})");
        let source = format!("(workspace w (graph g :context value (let* [tested {body}] 0.)))");
        let workspace = check_in(&empty(), &source);
        let evaluated = ok(e::r#static(&workspace, true, None));
        let value = evaluated.records.iter().find(|(p, _)| *p == vec!["g".to_owned(), "tested".to_owned()]).expect("tested").1[0].1.clone();
        let Value::Residual(residual) = &value else { panic!("expected residual") };
        let program = core_lower::lower(None, None, true, None, None, *residual).unwrap_or_else(|name| panic!("Core refused {name}"));
        let prepared = core_exec::prepare(&program, false, true, &Paths::new(), false);
        (value, program, prepared)
    }

    struct Device {
        resident: Arc<core_exec::Resident>,
        invokes: Arc<AtomicUsize>,
        reads: Arc<AtomicUsize>,
        closes: Arc<AtomicUsize>,
    }

    /// Placeholders have excess capacity and deliberately wrong contents. Only the device
    /// callbacks may read their actual values without a payload materialisation.
    fn device(decline: bool, overflow: bool, error: bool) -> Device {
        let held: Arc<Mutex<Vec<(Value, Value)>>> = Arc::new(Mutex::new(Vec::new()));
        let invokes = Arc::new(AtomicUsize::new(0));
        let reads = Arc::new(AtomicUsize::new(0));
        let closes = Arc::new(AtomicUsize::new(0));
        let overflow = Arc::new(AtomicBool::new(overflow));
        let actual = {
            let held = held.clone();
            move |value: &Value| -> Value {
                held.lock().unwrap().iter().find(|(key, _)| core_exec::phys_eq(key, value)).map(|(_, x)| x.clone()).unwrap_or_else(|| value.clone())
            }
        };
        let keep = {
            let held = held.clone();
            move |value: &Value| -> Value {
                let count = v::array_length(value).unwrap() + 3;
                let placeholder = match value {
                    Value::Int_array(_) => Value::Int_array(Arc::new(vec![-999; count])),
                    Value::Vec3_array(_) => Value::Vec3_array(Arc::new(vec![-999.0; count * 3])),
                    _ => panic!("unexpected device value"),
                };
                held.lock().unwrap().insert(0, (placeholder.clone(), value.clone()));
                placeholder
            }
        };
        let settle = {
            let overflow = overflow.clone();
            move || -> Result<(), Diagnostic> {
                if overflow.swap(false, Ordering::SeqCst) { return Err(core_exec::resident_overflow()); }
                if error { return Err(v::fail("E_TEST_STATUS", "device status")); }
                Ok(())
            }
        };
        let resident = core_exec::Resident {
            invoke: Box::new({
                let (actual, keep, invokes) = (actual.clone(), keep.clone(), invokes.clone());
                move |packed, args, _locals, live| {
                    if !matches!(packed.term.ty(), Ty::Array(_)) { return Ok(None); }
                    let n = invokes.fetch_add(1, Ordering::SeqCst) + 1;
                    if decline && n % 2 == 0 { return Ok(None); }
                    let args: Vec<Value> = args.iter().map(|a| actual(a)).collect();
                    let bound = packed::host::bind_arguments(packed, &args, None)?;
                    let forced = packed::force(&bound, live, &packed::ForceOptions { strict: true, ..Default::default() })?;
                    Ok(Some(keep(&forced)))
                }
            }),
            compact: Box::new({
                let (actual, keep) = (actual.clone(), keep.clone());
                move |mask| match actual(mask) {
                    Value::Int_array(mask) => {
                        let indices: Vec<i64> = (0..mask.len()).filter(|k| mask[*k] != 0).map(|k| k as i64).collect();
                        Ok(Some(keep(&Value::Int_array(Arc::new(indices)))))
                    }
                    _ => Ok(None),
                }
            }),
            coerce: Box::new(|_, _| Ok(None)),
            finish: Box::new({
                let (actual, reads) = (actual.clone(), reads.clone());
                move |value| {
                    let resolved = actual(value);
                    if !core_exec::phys_eq(&resolved, value) { reads.fetch_add(1, Ordering::SeqCst);  }
                    Ok(resolved)
                }
            }),
            count: Box::new({
                let (actual, settle) = (actual.clone(), settle.clone());
                move |value| {
                    let resolved = actual(value);
                    if core_exec::phys_eq(&resolved, value) {
                        Ok(None)
                    } else {
                        settle()?;
                        Ok(Some(v::array_length(&resolved)? as i64))
                    }
                }
            }),
            close: Box::new({
                let (closes, held) = (closes.clone(), held.clone());
                move || {
                    closes.fetch_add(1, Ordering::SeqCst);
                    held.lock().unwrap().clear();
                }
            }),
            settle: Box::new(settle),
            capacity: Box::new(|_| None),
            range: Box::new(|_, _, _, _| Ok(None)),
            fill: Box::new(|_, _, _| Ok(None)),
            concat: Box::new(|_, _| Ok(None)),
            unfold: Box::new(|_, _, _, _, _, _| Ok(None)),
            keep: Box::new(|_| Ok(None)),
        };
        Device { resident: Arc::new(resident), invokes, reads, closes }
    }

    fn materialise(device: &Device, value: &Value) -> Value {
        match value {
            Value::Record(fields) => Value::Record(fields.iter().map(|(k, x)| (k.clone(), materialise(device, x))).collect()),
            other => (device.resident.finish)(other).unwrap(),
        }
    }

    fn body(cpu: bool) -> String {
        let mut text = String::from(
            "(let* [mask (map (fn [(i : int)] (if (= (int/mod i 2) 0) 1 0))
                         (array/range (int t) :type int))
           faces (array/compact mask)
           n (array/count faces)
           points (map (fn [(i : int)] [(float i) 0.0 0.0]) faces)]
      {:points points :indices faces :count n",
        );
        if cpu { text.push_str(" :words (fold [s \"\"] [i faces] (str s i \",\")) :sum (array/sum faces)"); }
        text.push_str("})");
        text
    }

    fn exec(prepared: &core_exec::Prepared, live: &Frame, device: &Device) -> Result<Value, Diagnostic> {
        core_exec::execute(prepared, live, &core_exec::Options { strict: true, resident: Some(device.resident.clone()), ..Default::default() })
    }

    fn ensure(c: bool, m: &str) -> Result<(), String> { if c { Ok(()) } else { Err(m.to_owned()) } }

    #[test]
    fn resident_value_execution_counts_escaping_records_and_cpu_readers() {
        for domains in [1, 4] {
            let outcome = parallel::run_unit(Some(domains), &|| {
                for cpu in [false, true] {
                    let (value, core, prepared) = compile_value(&body(cpu));
                    ensure(core.regions.len() >= 2, "two regions")?;
                    for decline in [false, true] {
                        for time in [17.0, 17.0, 0.0, 257.0, 3.0] {
                            let live = input::at_time(time);
                            let d = device(decline, false, false);
                            let result = exec(&prepared, &live, &d).map_err(|x| rays_diagnostic::to_string(&x))?;
                            ensure(d.invokes.load(Ordering::SeqCst) >= 2 && d.closes.load(Ordering::SeqCst) == 0, "invokes without closes")?;
                            if !(cpu || decline) { ensure(d.reads.load(Ordering::SeqCst) == 0, "no reads")?; }
                            if cpu { ensure(d.reads.load(Ordering::SeqCst) > 0, "cpu readers read")?; }
                            let reference = e::Host::force_reference(&value, &live, None, None, None).map_err(|x| rays_diagnostic::to_string(&x))?;
                            ensure(materialise(&d, &result) == reference, "matches the reference")?;
                            (d.resident.close)();
                        }
                    }
                }
                Ok(())
            });
            outcome.unwrap();
        }
    }

    #[test]
    fn resident_fold_keeps_arrays_and_reports_overflow_and_status_errors() {
        let (value, _, prepared) = compile_value(
            "(let* [p (map (fn [i] [i 0.0 0.0]) (array/range (int t)))
             out (fold [b p] [d (range 2)]
               (map (fn [(v : vec3)] [(+ v.x 1.0) v.y v.z]) b))]
        {:before p :after out})",
        );
        let d = device(false, false, false);
        let live = input::at_time(17.0);
        let result = ok(exec(&prepared, &live, &d));
        assert!(d.closes.load(Ordering::SeqCst) == 0 && d.reads.load(Ordering::SeqCst) == 0);
        assert_eq!(materialise(&d, &result), ok(e::Host::force_reference(&value, &live, None, None, None)));
        (d.resident.close)();

        let (value, _, prepared) = compile_value(&body(false));
        let d = device(false, true, false);
        let result = ok(exec(&prepared, &live, &d));
        assert!(d.closes.load(Ordering::SeqCst) == 1 && d.reads.load(Ordering::SeqCst) == 0);
        assert_eq!(materialise(&d, &result), ok(e::Host::force_reference(&value, &live, None, None, None)));
        (d.resident.close)();

        let d = device(false, false, true);
        match exec(&prepared, &live, &d) {
            Err(x) => assert!(x.code == "E_TEST_STATUS" && d.closes.load(Ordering::SeqCst) == 1),
            Ok(_) => panic!("device status error was ignored"),
        }

        let (_, _, prepared) = compile_value("(state [a 0.] (+ a t))");
        let d = device(false, false, false);
        // a stateful value reads its state through its root residual: without one it fails, on the
        // device as on the CPU
        match exec(&prepared, &live, &d) {
            Err(x) => assert!(x.code == "E_CORE_STATE" && d.invokes.load(Ordering::SeqCst) == 0 && d.closes.load(Ordering::SeqCst) == 1, "{}", rays_diagnostic::to_string(&x)),
            Ok(_) => panic!("a stateful value ran without its state"),
        }
    }

    #[test]
    fn resident_field_failure_discards_earlier_placeholders_and_reruns_the_cpu_value() {
        let (record, _, _) = compile_value(
            "{:first (map (fn [(i : int)] (int/add i 1)) (array/range (int t) :type int)) :second (map (fn [(i : int)] (int/mul i 2)) (array/range (int t) :type int))}",
        );
        let Value::Residual(root) = &record else { panic!("residual") };
        let view = ok(e::Residual::residual_view(*root));
        fn fields(arena: &Arc<flow_check::term_arena::TermArena>, id: flow_check::term_arena::TermId) -> Option<Vec<(String, Term)>> {
            match arena.row(id).node {
                w::Node::Record(fs) => Some(arena.fields(fs).iter().map(|field| (arena.name(field.name).to_owned(), arena.handle(field.term))).collect()),
                _ => arena.children(id).iter().find_map(|&id| fields(arena, id)),
            }
        }
        let fs = fields(view.term.arena(), view.term.row_id()).expect("record fields");
        let value = Value::Record(
            fs.iter().map(|(name, term)| (name.clone(), Value::Residual(ok(e::Residual::with_bindings(*root, term, &view.bindings))))).collect(),
        );
        let program = ok(Executor::compile(&value, &Executor::CompileOptions::default()));
        let live = input::at_time(5001.0);
        let d = device(false, false, false);
        // the second field's settle fails
        let invokes = d.invokes.clone();
        let inner = d.resident.clone();
        let closes = d.closes.clone();
        let reads = d.reads.clone();
        let resident = core_exec::Resident {
            settle: Box::new({
                let (inner, invokes) = (inner.clone(), invokes.clone());
                move || {
                    if invokes.load(Ordering::SeqCst) >= 2 { return Err(v::fail("E_TEST_STATUS", "second field failed")); }
                    (inner.settle)()
                }
            }),
            fill: Box::new({ let i = inner.clone(); move |a, b, c| (i.fill)(a, b, c) }),
            invoke: Box::new({ let i = inner.clone(); move |a, b, c, d| (i.invoke)(a, b, c, d) }),
            unfold: Box::new({ let i = inner.clone(); move |a, b, c, d, e, f| (i.unfold)(a, b, c, d, e, f) }),
            compact: Box::new({ let i = inner.clone(); move |a| (i.compact)(a) }),
            coerce: Box::new({ let i = inner.clone(); move |a, b| (i.coerce)(a, b) }),
            concat: Box::new({ let i = inner.clone(); move |a, b| (i.concat)(a, b) }),
            finish: Box::new({ let i = inner.clone(); move |a| (i.finish)(a) }),
            keep: Box::new({ let i = inner.clone(); move |a| (i.keep)(a) }),
            count: Box::new({ let i = inner.clone(); move |a| (i.count)(a) }),
            capacity: Box::new({ let i = inner.clone(); move |a| (i.capacity)(a) }),
            close: Box::new({ let i = inner.clone(); move || (i.close)() }),
            range: Box::new({ let i = inner.clone(); move |a, b, c, d| (i.range)(a, b, c, d) }),
        };
        let actual = ok(Executor::force(
            &program,
            &Executor::ForceOptions { strict: true, resident: Some(Arc::new(resident)), ..Default::default() },
            &live,
        ));
        assert!(invokes.load(Ordering::SeqCst) == 2 && closes.load(Ordering::SeqCst) > 0 && reads.load(Ordering::SeqCst) == 0);
        assert_eq!(actual, ok(e::Host::force_reference(&value, &live, None, None, None)));
    }

    // --- parity (test_core_parity.ml) ----------------------------------------------------------

    fn same_bits(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
            (Value::Vec2(a, b), Value::Vec2(x, y)) => a.to_bits() == x.to_bits() && b.to_bits() == y.to_bits(),
            (Value::Vec3(a, b, c), Value::Vec3(x, y, z)) => a.to_bits() == x.to_bits() && b.to_bits() == y.to_bits() && c.to_bits() == z.to_bits(),
            (Value::Vec4(a, b, c, d), Value::Vec4(x, y, z, w)) => a.to_bits() == x.to_bits() && b.to_bits() == y.to_bits() && c.to_bits() == z.to_bits() && d.to_bits() == w.to_bits(),
            (Value::List(xs), Value::List(ys)) => xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| same_bits(a, b)),
            (Value::Float_array(xs), Value::Float_array(ys))
            | (Value::Vec2_array(xs), Value::Vec2_array(ys))
            | (Value::Vec3_array(xs), Value::Vec3_array(ys))
            | (Value::Vec4_array(xs), Value::Vec4_array(ys)) => xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| a.to_bits() == b.to_bits()),
            (Value::Record(fs), Value::Record(gs)) | (Value::Struct(_, _, fs), Value::Struct(_, _, gs)) => {
                fs.len() == gs.len() && fs.iter().zip(gs.iter()).all(|((n, a), (m, b))| n == m && same_bits(a, b))
            }
            (Value::Fn(_), Value::Fn(_)) => true,
            (a, b) => a == b,
        }
    }

    fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => same_bits(a, b),
            (Err(a), Err(b)) => a == b,
            _ => false,
        }
    }

    fn value_of(body: &str) -> Value {
        let workspace = check_in(manifest(), &format!("(workspace w (graph g :context value {body}))"));
        let evaluated = ok(e::r#static(&workspace, false, None));
        evaluated.results.iter().find(|(n, _)| n == "g").expect("g").1.clone()
    }

    /// Compare the executor, the reference walker and the direct Core run of one value at several
    /// times; returns how many direct runs matched.
    fn compare(value: &Value) -> usize {
        let program = ok(Executor::compile(value, &Executor::CompileOptions::default()));
        let mut direct = 0;
        for time in [0.0, 0.37, 1.9, 6.28, 100.25, -3.0] {
            let live = input::at_time(time);
            let reference = e::Host::force_reference(value, &live, None, None, None);
            let forced = Executor::force(&program, &Executor::ForceOptions::default(), &live);
            assert!(same(&forced, &reference), "executor vs reference at t={time}");
            if let Value::Residual(r) = value {
                if let Ok(core) = core_lower::lower(None, None, false, None, None, *r) {
                    let prepared = core_exec::prepare(&core, true, false, &Paths::new(), false);
                    let ran = core_exec::execute(&prepared, &live, &core_exec::Options::default());
                    match (&ran, &reference) {
                        (Ok(actual), Ok(expected)) => {
                            direct += 1;
                            assert!(same_bits(actual, expected), "core vs reference at t={time}");
                        }
                        (Err(_), Err(_)) => {}
                        _ => panic!("core and reference disagree about failure at t={time}"),
                    }
                }
            }
        }
        direct
    }

    #[test]
    fn core_parity_scalar_expressions_and_diagnostics() {
        let mut direct = 0;
        for body in [
            "(sum [k (range 6)] (/ (sin (* (+ t k) (+ (* 2 k) 1))) (+ (* 2 k) 1)))",
            "(let* [a 3 b (* a 2)] (+ (* t b) (mod t 0.7)))",
            "(if (> (sin t) 0.2) (pow t 2) (- 0 t))",
            "(let* [v [(* t 2) (+ t 1) (min t 3)]] (+ v.x v.y))",
            "(nth (for [i (range 4)] (* i t)) 2)",
            "(sum [x (for [i (range 4)] (+ (* i t) 1))] (* x 2))",
            "(sum [i (range 5)] (sum [j (range 3)] (* (+ t i) j)))",
            "(let* [a t a1 (+ a a) a2 (+ a1 a1) a3 (+ a2 a2)] (* a3 0.5))",
            "(let* [c (value/lerp [1 2 3] [t 0 1] 0.25)] c.z)",
            "(+ (* (sin t) 0.5) (* t t))",
            "(- (sqrt t) (cos (* t 3)))",
            "(* t 2)",
            "(- (+ (sin t) t) t)",
            "(+ (* 3 4) t)",
            "(* 2 (- t 1.5))",
            "(let* [a (* t 100000000000.) b (* a a) c (* b b) d (* c c) e (* d d) f (* e e)] (- f (sin t)))",
            "(sum [i (range 0)] t)",
            "(/ t 0)",
            "(sqrt (/ (- t 2) 0))",
            "(exp (* t 100000.))",
            "(int (* t 10000000000000000000.))",
        ] {
            direct += compare(&value_of(body));
        }
        assert!(direct > 0);
        // Repeated forces use the stateless shortcut, but ownership, invalid frames and a later
        // arithmetic failure still take their original diagnostics.
        let v = value_of("(exp t)");
        let a = ok(Executor::compile(&v, &Executor::CompileOptions::default()));
        let b = ok(Executor::compile(&v, &Executor::CompileOptions::default()));
        for time in [0.0, 1.0, 10000.0, 2.0] {
            let live = input::at_time(time);
            let expected = e::Host::force_reference(&v, &live, None, None, None);
            let o = Executor::ForceOptions::default();
            assert!(same(&Executor::force(&a, &o, &live), &expected));
            assert!(same(&Executor::force(&a, &o, &live), &expected));
            assert!(same(&Executor::force(&b, &o, &live), &expected));
        }
        let mut live = input::at_time(1.0);
        live.dt = f64::NAN;
        let expected = e::Host::force_reference(&v, &live, None, None, None);
        assert!(same(&Executor::force(&a, &Executor::ForceOptions::default(), &live), &expected));
    }

    #[test]
    fn core_parity_workspace_cases() {
        for name in ["bloom"] {
            let path = format!("{}/../../../specification/workspace/cases/{name}.lisp", env!("CARGO_MANIFEST_DIR"));
            let workspace = check_in(manifest(), &std::fs::read_to_string(path).unwrap());
            let evaluated = ok(e::r#static(&workspace, false, None));
            for (_, value) in &evaluated.results { compare(value); }
            for node in &evaluated.plan.nodes {
                for (_, value) in &node.args { compare(value); }
            }
        }
    }

    // --- frame folds (test_frame_kernel.ml) ----------------------------------------------------

    fn live_at(frame: i64, dt: f64) -> Frame {
        let mut live = input::at_time(frame as f64 / 60.0);
        live.frame = frame;
        live.dt = dt;
        live.size = (800, 600);
        live
    }

    fn state_value(body: &str) -> Value {
        let workspace = check_in(&empty(), &format!("(workspace w (graph g :context value (let* [tested {body}] 0.0)))"));
        let evaluated = ok(e::r#static(&workspace, true, None));
        evaluated.records.iter().find(|(p, _)| *p == vec!["g".to_owned(), "tested".to_owned()]).expect("tested").1[0].1.clone()
    }

    fn compare_frames(value: &Value, frames: &[(i64, f64)]) {
        let program = ok(Executor::compile(value, &Executor::CompileOptions::default()));
        let states = [e::create_state(), e::create_state()];
        let reference = e::create_state();
        for (frame, dt) in frames {
            let live = live_at(*frame, *dt);
            let expected = e::Host::force_reference(value, &live, Some(&reference), None, None);
            for (domains, state) in [(1, &states[0]), (8, &states[1])] {
                parallel::run_unit(Some(domains), &|| {
                    let o = Executor::ForceOptions { state: Some(state.clone()), ..Default::default() };
                    let actual = Executor::force(&program, &o, &live);
                    ensure(same(&actual, &expected), "forced value matches the reference")?;
                    ensure(format!("{:?}", e::Host::state_values(state)) == format!("{:?}", e::Host::state_values(&reference)), "state matches")
                })
                .unwrap();
            }
        }
    }

    fn compare_prepared(value: &Value, frames: &[(i64, f64)]) {
        let Value::Residual(r) = value else { panic!("Expected a frame residual.") };
        let core = core_lower::lower(None, None, false, None, None, *r).unwrap_or_else(|name| panic!("Prepared kernel refused {name}"));
        let prepared = core_exec::prepare(&core, false, false, &Paths::new(), false);
        let state = e::create_state();
        let reference = e::create_state();
        for (frame, dt) in frames {
            let live = live_at(*frame, *dt);
            let expected = e::Host::force_reference(value, &live, Some(&reference), None, None);
            let actual = e::Residual::bind_live(*r, &live, Some(&state))
                .and_then(|_| core_exec::execute(&prepared, &live, &core_exec::Options { root: Some(*r), ..Default::default() }));
            assert!(same(&actual, &expected), "prepared kernel vs reference at frame {frame}: {:?} / {:?}", actual.as_ref().map(|v| format!("{v:?}").chars().take(300).collect::<String>()), expected.as_ref().map(|v| format!("{v:?}").chars().take(300).collect::<String>()));
        }
    }

    const FOLD_FRAMES: [(i64, f64); 5] = [(0, 0.125), (1, 0.5), (2, 1.0), (1024, 0.125), (1025, 0.125)];

    #[test]
    fn frame_kernel_bound_function_capture() {
        let bound = state_value(
            "(state [p {:xs (array/float 2051) :scale 0.125}] (let* [f (fn [x] (+ x p.scale))] {:xs (map f p.xs) :scale (+ p.scale (frame/dt))}))",
        );
        compare_frames(&bound, &[(0, 0.125), (1, 0.125), (2, 0.125), (3, 0.125), (4, 0.125)]);
    }

    #[test]
    fn frame_kernel_record_state_and_growing_source() {
        for body in [
            "(state [p {:xs (array/float 2051) :scale 0.125}] {:xs (map (fn [x] (+ x p.scale)) p.xs) :scale (+ p.scale (frame/dt))})",
            "(state [p (array/float 0)] (map (fn [x] (+ x (frame/dt))) (array/range (frame/index))))",
        ] {
            compare_frames(&state_value(body), &FOLD_FRAMES);
        }
    }

    #[test]
    fn frame_kernel_multi_block_branch() {
        // ponytail: 4,097 elements (four blocks and one over) instead of the OCaml 16,385: the
        // reference walker is the cost in a debug build, the block seam is the same.
        compare_frames(
            &state_value("(state [p (array/range 4097)] (map (fn [x] (if (not (and (>= x 0) (< x (frame/width)))) (- 0 x) (+ x (frame/dt)))) p))"),
            &FOLD_FRAMES,
        );
    }

    #[test]
    fn frame_kernel_scan_and_error_branch() {
        for body in [
            "(state [p (array/float 2051)] (scan [a 0.0] [x p] (+ a (+ x (frame/dt)))))",
            "(state [p (array/float 2051)] (map (fn [x] (if (< (frame/dt) 1) (+ x (frame/dt)) (pow (+ x 2) 10000000))) p))",
        ] {
            compare_frames(&state_value(body), &[(0, 0.125), (1, 0.5), (2, 1.0), (1025, 0.125)]);
        }
        let v = state_value(
            "(let* [a (state [p (array/float 2051)] (map (fn [x] (+ x (frame/dt))) p))] {:xs a :error (array/nth (array/float 2) (frame/index))})",
        );
        compare_frames(&v, &[(0, 0.125), (1, 0.125), (2, 0.125), (1, 0.125), (3, 0.125)]);
    }

    fn prepared_cases(first: usize, last: usize) {
        for body in [
            "(state [p (exact (map (fn [x] [x (- 0 x)]) (array/range 2051)))] (map (fn [v] (+ (* v (frame/dt)) 0.5)) p))",
            "(state [p (exact (map (fn [x] [x (- 0 x) 1 2]) (array/range 2051)))] (map (fn [v] (+ (* v (frame/dt)) [1 2 3 4])) p))",
            "(state [p {:xs (array/range 2051) :scale 0.125}] {:xs (map (fn [x] (if (> x 1024) (+ x p.scale) (sin x))) p.xs) :scale (+ p.scale (frame/dt))})",
            "(state [p (array/range 2051)] (map (fn [x] (if (< (frame/dt) 1) (+ x 1) (pow (+ x 2) 10000000))) p))",
            "(state [p (array/range 2051)] (filter (fn [x] (< x (frame/width))) p))",
            "(state [p (array/range 2051)] (map (fn [x] (let* [f (fn [y] (+ y (frame/dt)))] (+ (f x) (f x)))) p))",
        ].iter().enumerate().filter(|(i, _)| *i >= first && *i < last).map(|(_, b)| *b) {
            compare_prepared(&state_value(body), &[(0, 0.125), (1, 0.5), (2, 1.0), (3, 20.0), (4, 0.125), (1, 0.25)]);
        }
    }

    #[test]
    fn frame_kernel_prepared_programs_match_the_reference_a() { prepared_cases(0, 3); }

    #[test]
    fn frame_kernel_prepared_programs_match_the_reference_b() { prepared_cases(3, 6); }

    // --- test_ir.ml: qualification, GPU-neutral callbacks, executor parity ---------------------

    fn check_ops(ops: Vec<flow_check::op::Op>, source: &str) -> w::Workspace {
        let forms = syntax::parse(source).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        match w::check(&ops, false, false, None, &empty(), &forms) {
            (Some(workspace), ds) if !ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) => workspace,
            (_, ds) => panic!("{}", show(&ds)),
        }
    }

    fn check(source: &str) -> w::Workspace { check_in(&empty(), source) }

    fn fast_gpu() -> Gpu::Config { Gpu::Config { fast: true, ..Gpu::default() } }

    fn qualify_with(options: QualifyOptions, workspace: &w::Workspace) -> (w::Workspace, e::Eval) {
        ok(qualify_workspace(&options, workspace))
    }

    fn reasons_of<'a>(workspace: &'a w::Workspace, path: &Path) -> &'a Vec<Diagnostic> {
        &workspace.approx_reasons.iter().find(|(p, _)| p == path).unwrap_or_else(|| panic!("no reasons for {path:?}")).1
    }

    fn p(parts: &[&str]) -> Path { parts.iter().map(|s| s.to_string()).collect() }

    #[test]
    fn qualifier_body_cache_uses_arena_row_identity() {
        use flow_check::term_arena::{Draft, TermBuilder};
        let form = syntax::parse("0").unwrap().remove(0);
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&form);
        let body = builder.push(None, &Ty::Float, form, Draft::Time);
        let call = builder.push(None, &Ty::Float, form, Draft::CallFn { fn_name: "f".into(), args: Vec::new(), body: Some(body) });
        let arena = builder.freeze();
        let owner = p(&["g"]);
        let mut walker = Walker {
            collect: false,
            t: Tables { roots: HashMap::new(), index: HashMap::new(), callable_owners: HashMap::new() },
            called: HashMap::new(),
            called_seen: HashMap::new(),
        };
        walker.walk(&owner, &arena, call);
        walker.walk(&owner, &arena, call);
        let first = arena.handle(body).identity();
        assert_eq!(walker.called_seen[&first], vec![owner.clone()]);

        let mut builder = TermBuilder::new();
        let roots = builder.rewrite_roots(&[arena.handle(call)], &mut |_, _, draft| draft);
        let rewritten = builder.freeze();
        let w::Node::CallFn(id) = rewritten.row(roots[0]).node else { panic!("call") };
        let second = rewritten.handle(rewritten.call_fn(id).body.unwrap()).identity();
        assert_ne!(first, second);
        walker.walk(&owner, &rewritten, roots[0]);
        assert_eq!(walker.called_seen[&second], vec![owner]);
        assert_eq!(walker.called.values().map(Vec::len).sum::<usize>(), 1);
    }

    #[test]
    fn image_qualification_authored_sites_and_shared_functions() {
        let observed: Arc<Mutex<Vec<(Option<Path>, Path)>>> = Arc::new(Mutex::new(Vec::new()));
        let qualify = |inputs: Option<Vec<(String, Vec<(String, Value)>)>>, gpu: Gpu::Config, workspace: &w::Workspace| {
            observed.lock().unwrap().clear();
            let seen = observed.clone();
            let options = QualifyOptions {
                record: true,
                inputs,
                gpu: Some(gpu),
                observe: Some(Arc::new(move |producer: Option<Path>, residual: ResidualId| {
                    let view = e::Residual::residual_view(residual).unwrap();
                    if producer.is_some() && matches!(view.term.ty(), Ty::Array(inner) if **inner == Ty::Vec4) {
                        for fusion in [true, false] {
                            let o = packed::CompileOptions { fusion, ..packed::CompileOptions::default() };
                            if let Ok(compiled) = packed::compile_result(&o, residual, &view.term) {
                                if packed::gpu_refusals(false, false, &compiled).is_empty() {
                                    assert_eq!(packed::site(&compiled), (view.instance, producer.clone().unwrap(), view.iter.clone()));
                                }
                            }
                        }
                        seen.lock().unwrap().push((producer, view.site.clone()));
                    }
                })),
                ..Default::default()
            };
            let (qualified, evaluation) = qualify_with(options, workspace);
            (qualified, evaluation, observed.lock().unwrap().clone())
        };
        let path = p(&["img", "picture"]);
        let image = |body: &str| format!("(workspace pixels (graph img :context image (let* [picture\n    (image/map (fn [uv] {body}))] picture)))");
        for (body, accepted, code) in [
            ("[uv.x uv.y t 1]", true, ""),
            ("[(floor 2.5) uv.y t 1]", true, ""),
            ("[(floor uv.x) uv.y t 1]", false, "E_GPU_FORM"),
            ("[(+ uv.x 1e39) uv.y t 1]", false, "E_GPU_FORM"),
        ] {
            let (qualified, _, _) = qualify(None, Gpu::default(), &check(&image(body)));
            assert_eq!(qualified.approx.contains(&path), accepted, "{body}");
            if !accepted { assert!(reasons_of(&qualified, &path).iter().any(|d| d.code == code), "{body}"); }
        }
        for body in [
            "[(fold [a uv.x] [i (range 3)] (+ a 0.25)) uv.y t 1]",
            "[(iterate 4611686018427387903 uv.x (fn [a] (+ a 1.))\n          :until (fn [prev next] (>= next (+ uv.x 3.)))) uv.y t 1]",
        ] {
            let (qualified, _, _) = qualify(None, fast_gpu(), &check(&image(body)));
            if !qualified.approx.contains(&path) {
                panic!("{}", show(reasons_of(&qualified, &path)));
            }
        }
        let shared = check("(workspace pixels (graph img :context image\n    (let* [f (fn [uv] [uv.x uv.y t 1]) held (fn [uv] [uv.x uv.y t 1])\n      a (image/map f) b (image/map f)] b)))");
        let (qualified, _, seen) = qualify(None, Gpu::default(), &shared);
        assert!(qualified.approx.contains(&p(&["img", "a"])) && qualified.approx.contains(&p(&["img", "b"])));
        assert!(!qualified.approx.contains(&p(&["img", "held"])));
        assert!(seen.iter().any(|(pr, _)| *pr == Some(p(&["img", "a"]))) && seen.iter().any(|(pr, _)| *pr == Some(p(&["img", "b"]))));
        let captures = check(
            "(workspace pixels\n    (graph img :context image [(offset : float 2.0)]\n      (let* [picture (image/map (fn [uv] [(+ uv.x offset) uv.y t 1]))] picture))\n    (graph host :context host [(one : float 2.0) (two : float 3.0)]\n      (list (ref img :offset one) (ref img :offset two))))",
        );
        for (one, two, accepted) in [(1e39, 7.0, false), (7.0, 1e39, false), (2.0, 7.0, true)] {
            let inputs = vec![("host".to_owned(), vec![("one".to_owned(), Value::Float(one)), ("two".to_owned(), Value::Float(two))])];
            let (qualified, _, _) = qualify(Some(inputs), Gpu::default(), &captures);
            assert_eq!(qualified.approx.contains(&path), accepted);
            if !accepted { assert!(reasons_of(&qualified, &path).iter().any(|d| d.code == "E_GPU_FORM")); }
        }
        let state = check("(workspace pixels (graph img :context image\n    (let* [bias (state [n 0.0] (+ n 0.1)) picture (image/map (fn [uv] [uv.x uv.y bias 1]))] picture)))");
        let (qualified, _, _) = qualify(None, Gpu::default(), &state);
        // a state value is a uniform the host steps with its own state each frame
        assert!(qualified.approx.contains(&path));
        let mut counterfeit = flow_check::op::find(&[], "image/map", &context::image()).expect("image/map");
        counterfeit.category = "Counterfeit image map".into();
        let mut counterfeited = shared.clone();
        Arc::make_mut(&mut counterfeited.ops).insert(0, counterfeit);
        let (qualified, _, _) = qualify(None, Gpu::default(), &counterfeited);
        assert!(!qualified.approx.contains(&p(&["img", "a"])));
        assert!(reasons_of(&qualified, &p(&["img", "a"])).iter().any(|d| d.code == "E_GPU_FORM"));
    }

    fn static_map_case(width: i64, readback_per_float: Option<f64>, shape: Option<f64>, actual: Option<f64>, expected: PlacementReason, dispatched: usize) {
        let host = |width: i64| {
            if width == 1 {
                check("(workspace static (graph g :context host\n(map (fn [i] (+ (float i) 0.25)) (array/range 4097))))")
            } else {
                check("(workspace static (graph g :context host\n(map (fn [i] [(float i) 0.25 0.5 1.]) (array/range 4097))))")
            }
        };
        // Callback/placement coverage; the readback is computed on CPU.
        {
            let ws = host(width);
            let calls = Arc::new(AtomicUsize::new(0));
            let profile = Profile::create(Arc::new(|| 0.0));
            let backend = Gpu::Backend {
                residency: Box::new(|_| None),
                cost: Box::new(move |inputs, _, _, _| if inputs.is_some() { actual } else { shape }),
                prepare: Box::new({
                    let calls = calls.clone();
                    move |_, packed_region| {
                        let (calls, readback_region) = (calls.clone(), packed_region.clone());
                        Ok(Arc::new(Gpu::Kernel {
                            run: Box::new(move |inputs| {
                                calls.fetch_add(1, Ordering::SeqCst);
                                Ok(Gpu::Value { identity: 1, output: 0, count: inputs.count, width, stamp: 0, gpu_seconds: actual })
                            }),
                            readback: Box::new(move |_| packed::force(&readback_region, &input::at_time(0.0), &packed::ForceOptions::default())),
                            planes: Box::new(|_| Err(v::fail("E_TEST", "static map uses readback, not planes"))),
                        }))
                    }
                }),
            };
            let gpu = Gpu::Config { backend: Some(Arc::new(backend)), fast: true, policy: Gpu::Policy::Measured, readback_per_float };
            let result = Profile::sample(&profile, &mut || ok(qualify_workspace(&QualifyOptions { record: true, gpu: Some(gpu.clone()), ..Default::default() }, &ws)));
            let expected_value = if width == 1 {
                Value::Float_array(Arc::new((0..4097).map(|i| i as f64 + 0.25).collect()))
            } else {
                Value::Vec4_array(Arc::new((0..4097 * 4).map(|i| match i % 4 { 0 => (i / 4) as f64, 1 => 0.25, 2 => 0.5, _ => 1.0 }).collect()))
            };
            let g = result.1.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
            assert_eq!(g, expected_value);
            assert_eq!(calls.load(Ordering::SeqCst), dispatched);
            let placement = Profile::placements(&profile).remove(0);
            assert!(placement.reason == expected && placement.cpu_consumers == Some(1), "{:?}", placement);
        }
    }

    fn static_price() -> Option<f64> {
        let cpu = Cost::estimate(Cost::packed(4097), 4097);
        Some(cpu - Cost::estimate(Tier::Gpu_readback, 8194))
    }

    // One test per case: each qualifies a 4,097-element map (the static-kernel threshold) and
    // walks it for the reference, about a second in a debug build.
    #[test]
    fn static_maps_device_cheaper_from_actual_cost() { static_map_case(1, None, Some(0.0), Some(0.0), PlacementReason::Device_measured_cheaper, 1); }
    #[test]
    fn static_maps_prepared_only_evidence() { static_map_case(1, None, None, Some(0.0), PlacementReason::Device_measured_cheaper, 1); }
    #[test]
    fn static_maps_missing_actual_cost() { static_map_case(1, None, Some(0.0), None, PlacementReason::No_cost_evidence, 0); }
    #[test]
    fn static_maps_negative_cost() { static_map_case(1, None, Some(0.0), Some(-1e-12), PlacementReason::No_cost_evidence, 0); }
    #[test]
    fn static_maps_expensive_device_keeps_cpu_reader() { static_map_case(1, None, Some(1.0), Some(0.0), PlacementReason::Cpu_consumer, 0); }
    #[test]
    fn static_maps_priced_readback_scalar() { let p = static_price(); static_map_case(1, None, p, p, PlacementReason::Device_measured_cheaper, 1); }
    #[test]
    fn static_maps_priced_readback_vec4() { let p = static_price(); static_map_case(4, None, p, p, PlacementReason::Cpu_consumer, 0); }
    #[test]
    fn static_maps_slow_card_readback() {
        let per_float = (Cost::estimate(Tier::Gpu_readback, 2048) - Cost::estimate(Tier::Gpu_readback, 1024)) / 1024.0;
        let p = static_price();
        static_map_case(1, Some(2.0 * per_float), p, p, PlacementReason::Cpu_consumer, 0);
    }
    #[test]
    fn static_maps_free_card_readback() { let p = static_price(); static_map_case(1, Some(0.0), p, p, PlacementReason::Device_measured_cheaper, 1); }


    fn has_region(value: &Value) -> bool {
        fn packed_child_region(residual: ResidualId, term: &Term) -> bool {
            packed::compile(&packed::CompileOptions::default(), residual, term).is_some()
                || term.children().iter().any(|&id| packed_child_region(residual, &term.child(id)))
        }
        fn has_fields(fs: &[(String, Value)]) -> bool { fs.iter().any(|(_, x)| has_region(x)) }
        match value {
            Value::Residual(r) => {
                let view = e::Residual::residual_view(*r).unwrap();
                core_lower::lower(None, None, false, None, None, *r).map(|core| !core.regions.is_empty()).unwrap_or(false)
                    || packed_child_region(*r, &view.term)
                    || has_fields(&view.bindings)
            }
            Value::List(xs) => xs.iter().any(has_region),
            Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => has_fields(fs),
            Value::Fn(f) => has_fields(&e::Residual::function_bindings(*f)),
            _ => false,
        }
    }

    fn value_with(ops: Vec<flow_check::op::Op>, body: &str) -> Value {
        let ws = check_ops(ops, &format!("(workspace w (graph g :context value {body}))"));
        let evaluated = ok(e::r#static(&ws, true, None));
        evaluated.results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
    }
    fn value(body: &str) -> Value { value_with(Vec::new(), body) }

    fn compiled(v: &Value) -> Executor::Program { ok(Executor::compile(v, &Executor::CompileOptions::default())) }
    fn forced(program: &Executor::Program, live: &Frame) -> Result<Value, Diagnostic> { Executor::force(program, &Executor::ForceOptions::default(), live) }

    #[test]
    fn executor_sums_match_the_reference_for_edge_counts() {
        let live = input::at_time(0.125);
        for count in [0, 16, 33, 34, 512, 1024] {
            let v = value(&format!("(array/sum (map (fn [x] (sin (+ x t))) (array/range {count})))"));
            assert!(same(&forced(&compiled(&v), &live), &e::Host::force_reference(&v, &live, None, None, None)));
        }
    }

    #[test]
    fn executor_profile_keeps_512_groups_and_scalar_bodies_match_across_domains() {
        let profile = Profile::create(Arc::new(|| 1.0));
        for i in 0..=512 {
            let ws = check(&format!("(workspace w (graph g{i} :context value (+ t 1)))"));
            let evaluated = ok(e::r#static(&ws, false, None));
            let program = ok(Executor::compile(&evaluated.results[0].1, &Executor::CompileOptions { profile: Some(profile.clone()), ..Default::default() }));
            ok(forced(&program, &input::at_time(0.0)));
        }
        let reports = Profile::executions(&profile);
        assert_eq!(reports.len(), 512);
        assert!(reports.iter().all(|r| r.sites.iter().all(|(instance, _, _)| *instance == 0)));
        assert!(reports[0].seconds == 0.0 && reports[0].tier == Tier::Core);
        for body in [
            "(+ (* t 2.0) (sin t))",
            "(exact (+ (* t 2.0) (sin t)))",
            "(let* [a t b (* a 3)] (+ b a))",
            "(let* [p [(sin t) (* t 2) (+ t 1)]] (+ p.x p.z))",
            "(let* [r {:x t :nested (list [(sin t) 0 0] [t 0 0])}] r.x)",
            "(if (> t 1) (sqrt t) (sin t))",
            "(sum [x (range 3)] (* t x))",
            "(array/sum (map (fn [x] (+ x t)) (array/range 128)))",
            "(let* [f (fn [x] (* x t))] (sum [x (map f (list 1 2 3))] x))",
            "(sqrt (pow t 1000000))",
        ] {
            let v = value(body);
            let program = compiled(&v);
            for time in [0.0, 0.125, 1.25, 7.0] {
                let live = input::at_time(time);
                let reference = e::Host::force_reference(&v, &live, None, None, None);
                for domains in [1, 8] {
                    parallel::run_unit(Some(domains), &|| {
                        ensure(same(&forced(&program, &live), &reference), "executor matches")?;
                        let o = Executor::ForceOptions { reference: true, ..Default::default() };
                        ensure(same(&Executor::force(&program, &o, &live), &reference), "reference option matches")
                    })
                    .unwrap();
                }
            }
        }
        let mut bad_live = input::at_time(0.0);
        bad_live.dt = f64::NAN;
        let v = value("(+ t 1)");
        assert!(same(&forced(&compiled(&v), &bad_live), &e::Host::force_reference(&v, &bad_live, None, None, None)));
        let v = value("(let* [a (state [p 0.0] (+ p (frame/dt))) b (* a 2)] (+ a b))");
        let program = compiled(&v);
        let optimized = e::create_state();
        let reference = e::create_state();
        for frame in [0, 1, 2, 2, 3, 1] {
            let mut live = input::at_time(frame as f64);
            live.frame = frame;
            live.dt = 0.125;
            let o = Executor::ForceOptions { state: Some(optimized.clone()), ..Default::default() };
            assert!(same(&Executor::force(&program, &o, &live), &e::Host::force_reference(&v, &live, Some(&reference), None, None)));
        }
        let (a, b) = (e::Host::state_values(&optimized), e::Host::state_values(&reference));
        assert!(a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| same_bits(x, y)));
    }

    #[test]
    fn executor_packed_core_regions_match_sequential_and_parallel() {
        for body in [
            "(array/sum (map (fn [x] (sin (+ (* x 0.25) t))) (array/range 2051)))",
            "(array/sum (map (fn [x] (/ (+ x t) (- x 1))) (array/range 2051)))",
            "(array/sum (map (fn [x] (mod (- t x) 0.7)) (array/range 2051)))",
            "(let* [p (array/nth (map (fn [p n] (+ p (* n (* t 0.8)))) (array/vec3 2051 [1 2 3]) (array/vec3 2053 [0 1 0])) 2050)] p.y)",
            "(array/sum (map (fn [x] (let* [a (* x t) b (sqrt a)] (+ (cos b) b))) (array/range 2051)))",
            "(array/sum (map (fn [x] (min (+ x t) (max x 100.0))) (array/range 2051)))",
            "(array/sum (map (fn [x] (+ (pow t 1000000) (* x 0))) (array/range 2051)))",
        ] {
            let v = value(body);
            let program = compiled(&v);
            assert!(has_region(&v), "packed Core region missing for: {body}");
            for time in [0.0, 0.125, 7.0] {
                let live = input::at_time(time);
                let reference = e::Host::force_reference(&v, &live, None, None, None);
                for domains in [1, 8] {
                    parallel::run_unit(Some(domains), &|| ensure(same(&forced(&program, &live), &reference), "region matches the reference")).unwrap();
                }
            }
        }
        let v = value("(let* [unused (pow t 1000000) mapped (map (fn [x] (+ x t)) (array/range 2051))] (array/sum mapped))");
        let program = compiled(&v);
        for time in [0.0, 0.125, 7.0] {
            let live = input::at_time(time);
            assert!(same(&forced(&program, &live), &e::Host::force_reference(&v, &live, None, None, None)));
        }
        let v = value("(array/sum (map (fn [x] (+ x t)) (array/float 0)))");
        assert_eq!(forced(&compiled(&v), &input::at_time(1.0)), Ok(Value::Float(0.0)));
        let v = value("(array/sum (map (fn [x] (+ x t)) (array/range (frame/index))))");
        let program = compiled(&v);
        for count in [0, 1023, 1024, 2051] {
            let mut live = input::at_time(1.25);
            live.frame = count;
            assert!(same(&forced(&program, &live), &e::Host::force_reference(&v, &live, None, None, None)));
        }
    }

    fn mapped_value(ops: Vec<flow_check::op::Op>, body: &str) -> Value {
        let ws = check_ops(ops, &format!("(workspace noise (graph g :context value (let* [mapped {body}] (array/sum mapped))))"));
        let evaluated = ok(e::r#static(&ws, true, None));
        evaluated.records.iter().find(|(path, _)| *path == p(&["g", "mapped"])).unwrap().1[0].1.clone()
    }

    #[test]
    fn executor_noise_kernels_are_exact_and_custom_noise_stays_unpacked() {
        let v = mapped_value(Vec::new(), "(map (fn [p] (exact (noise3 (+ p [t 0 0])))) (array/vec3 2051 [0.3 0.7 -0.2]))");
        let program = compiled(&v);
        assert!(Executor::packed_region(&program).is_some());
        for time in [0.0, 0.125, 1.25, 7.0] {
            let live = input::at_time(time);
            let reference = ok(e::Host::force_reference(&v, &live, None, None, None));
            for domains in [1, 8] {
                parallel::run_unit(Some(domains), &|| {
                    let result = forced(&program, &live).map_err(|d| rays_diagnostic::to_string(&d))?;
                    ensure(same_bits(&result, &reference), "noise matches the reference")?;
                    match &result {
                        Value::Float_array(xs) => {
                            let expected = rays_math::noise::sample3(&rays_math::noise::create(0), 0.3 + time, 0.7, -0.2);
                            ensure(xs.len() == 2051 && xs.iter().all(|x| x.to_bits() == expected.to_bits()), "samples equal")
                        }
                        _ => Err("expected a float array".into()),
                    }
                })
                .unwrap();
            }
        }
        let v = mapped_value(Vec::new(), "(map (fn [p] (noise3 (* p (pow t 1000000)))) (array/vec3 2051 [1 2 3]))");
        let live = input::at_time(7.0);
        assert!(same(&forced(&compiled(&v), &live), &e::Host::force_reference(&v, &live, None, None, None)));
        let mut custom = flow_check::op::find(&[], "noise3", &context::value()).expect("noise3");
        custom.name = "toy_noise3".into();
        custom.packed_extension = None;
        custom.body = Arc::new(|_, _, _| Ok(Value::Float(0.125)));
        let v = mapped_value(vec![custom], "(map (fn [p] (toy_noise3 p)) (array/vec3 2051 [1 2 3]))");
        let program = compiled(&v);
        assert!(Executor::packed_region(&program).is_none());
        assert!(same(&forced(&program, &live), &e::Host::force_reference(&v, &live, None, None, None)));
        match ok(forced(&program, &live)) {
            Value::Float_array(xs) => assert!(xs.iter().all(|x| *x == 0.125)),
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn executor_seeded_octave_noise_and_color_forms() {
        for seed in [0, 2026, -451] {
            for octaves in [1, 5, 32] {
                let v = mapped_value(
                    Vec::new(),
                    &format!("(map (fn [p] (noise3 (+ p [t 0 0]) :seed {seed} :octaves {octaves})) (array/vec3 2051 [0.31 0.73 -0.29]))"),
                );
                let program = compiled(&v);
                assert!(Executor::packed_region(&program).is_some());
                for time in [0.0, 1.25] {
                    let live = input::at_time(time);
                    let reference = ok(e::Host::force_reference(&v, &live, None, None, None));
                    for domains in [1, 8] {
                        parallel::run_unit(Some(domains), &|| {
                            let actual = forced(&program, &live).map_err(|d| rays_diagnostic::to_string(&d))?;
                            ensure(same_bits(&actual, &reference), "octave noise matches")
                        })
                        .unwrap();
                    }
                }
            }
        }
        for expression in ["(color/hsl 220 0.7 0.4)", "(color/hsla 451 0.5 0.6 0.25)", "(color/gradient (list \"#101426\" \"#f26444\") 0.3)"] {
            let ws = check(&format!("(workspace c (graph g :context value (let* [result {expression}] 0)))"));
            let evaluated = ok(e::r#static(&ws, false, None));
            ok(forced(&compiled(&evaluated.results.iter().find(|(n, _)| n == "g").unwrap().1), &input::at_time(0.0)));
        }
    }

    #[test]
    fn executor_large_map_matches_the_walker_and_domains() {
        // ponytail: 65,536 elements, not the OCaml 1,000,000: the reference walker is the cost in a
        // debug build; the block, chunk and domain seams are the same.
        let ws = check("(workspace large (graph g :context value (let* [xs (array/range 8192) mapped (map (fn [x] (sin (+ (* x 0.25) t))) xs)] (array/sum mapped))))");
        let evaluated = ok(e::r#static(&ws, true, None));
        let v = evaluated.records.iter().find(|(path, _)| *path == p(&["g", "mapped"])).unwrap().1[0].1.clone();
        let program = compiled(&v);
        let live = input::at_time(1.25);
        let one: Mutex<Option<Value>> = Mutex::new(None);
        let eight: Mutex<Option<Value>> = Mutex::new(None);
        parallel::run_unit(Some(1), &|| {
            *one.lock().unwrap() = Some(forced(&program, &live).map_err(|d| rays_diagnostic::to_string(&d))?);
            Ok(())
        })
        .unwrap();
        parallel::run_unit(Some(8), &|| {
            *eight.lock().unwrap() = Some(forced(&program, &live).map_err(|d| rays_diagnostic::to_string(&d))?);
            Ok(())
        })
        .unwrap();
        let (one, eight) = (one.into_inner().unwrap().unwrap(), eight.into_inner().unwrap().unwrap());
        let reference = ok(e::Host::force_reference(&v, &live, None, None, None));
        assert!(same_bits(&one, &reference));
        assert!(same_bits(&one, &eight));
        match one {
            Value::Float_array(xs) => {
                assert_eq!(xs.len(), 8192);
                for (i, x) in xs.iter().enumerate() {
                    assert_eq!(x.to_bits(), ((i as f64 * 0.25) + 1.25).sin().to_bits());
                }
            }
            _ => panic!("CPU map did not return a packed array"),
        }
    }

    #[test]
    fn executor_workspace_cases_match_the_reference() {
        for name in ["bloom", "variations"] {
            let path = format!("{}/../../../specification/workspace/cases/{name}.lisp", env!("CARGO_MANIFEST_DIR"));
            let ws = check_in(manifest(), &std::fs::read_to_string(path).unwrap());
            let evaluated = ok(e::r#static(&ws, true, None));
            let mut values: Vec<Value> = evaluated.results.iter().map(|(_, x)| x.clone()).collect();
            values.extend(evaluated.states.iter().cloned());
            for node in &evaluated.plan.nodes { values.extend(node.args.iter().map(|(_, x)| x.clone())); }
            for instance in &evaluated.plan.instances {
                values.push(instance.result.clone());
                values.extend(instance.inputs.iter().map(|(_, x)| x.clone()));
            }
            for (_, records) in &evaluated.records { values.extend(records.iter().map(|(_, x)| x.clone())); }
            let prepared: Vec<(Value, Executor::Program)> = values.into_iter().map(|x| { let program = compiled(&x); (x, program) }).collect();
            for time in [0.0, 1.25] {
                let live = input::at_time(time);
                for domains in [1, 8] {
                    parallel::run_unit(Some(domains), &|| {
                        let reference_state = e::create_state();
                        let ir_state = e::create_state();
                        for (x, program) in &prepared {
                            let o = Executor::ForceOptions { state: Some(ir_state.clone()), ..Default::default() };
                            ensure(
                                same(&Executor::force(program, &o, &live), &e::Host::force_reference(x, &live, Some(&reference_state), None, None)),
                                "workspace value matches",
                            )?;
                        }
                        Ok(())
                    })
                    .unwrap();
                }
            }
        }
    }
}
