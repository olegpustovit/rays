//! Port of lib/flow_native/test_workspace_gpu_service.ml: a worker cook reaches the
//! workspace-owned packed-attribute GPU proxy only while the owner thread pumps it.
//!
//! "Initial domain" is the thread that created the service (the test's own thread); the OCaml
//! `Domain.spawn` workers are scoped threads.
use crate::workspace_gpu as wg;
use flow_ir::core_exec::Resident;
use flow_ir::flow_ir::gpu as G;
use flow_sop::{lower, value_lane};
use flow_ir::packed;
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use sop::{context, edit_graph as edit, payload, session};
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread::ThreadId;
use std::time::{Duration, Instant};

const COUNT: i64 = 2048;

fn text() -> String {
    format!(
        "(workspace kernel (graph g :context sop (let* [g (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns {} :rows 2) amp 0.8 freq 0.16 result (sop/with_attr g :P (exact (map (fn [i] [(+ i t) 0 0]) (array/range {COUNT}))))] result)))",
        COUNT / 2
    )
}

fn ok<A>(r: Result<A, rays_diagnostic::Diagnostic>) -> A {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn forms() -> Vec<flow_syntax::syntax::Syntax> {
    ok(flow_syntax::syntax::parse(&text()))
}

fn lower_with(gpu: G::Config) -> lower::T {
    ok(lower::workspace(None, Vec::new(), lower::Options { gpu, ..Default::default() }, &forms()))
}

/// The point positions of a graph, flat, cooked on the calling thread.
fn cook(gpu: Option<&G::Config>, graph: &lower::Graph) -> Vec<f64> {
    let resolved = ok(value_lane::resolve(gpu, None, &value_lane::create(None), 0., &graph.network));
    let node = edit::compile_node(&resolved.geometry, graph.root.expect("a graph result")).unwrap_or_else(|m| panic!("{m}"));
    let mut session = session::create(4, 1 << 28).unwrap_or_else(|m| panic!("{m}"));
    let context = context::create(context::Options { domains: Some(4), time: Some(0.), ..Default::default() }).unwrap_or_else(|m| panic!("{m}"));
    let output = session::cook(&mut session, None, &context, &node);
    session::close(&mut session);
    let geometry = ok(payload::geometry(&ok(output).payload));
    let positions = rdk_core::geometry::positions(&geometry);
    let mut flat = Vec::new();
    for i in 0..COUNT {
        let (x, y, z) = rdk_core::packed::float3::get(positions, i).unwrap();
        flat.extend([x, y, z]);
    }
    flat
}

fn on_owner(main: ThreadId) -> bool {
    std::thread::current().id() == main
}

#[derive(Clone)]
struct Counters {
    costs: Arc<AtomicI64>,
    prepares: Arc<AtomicI64>,
    runs: Arc<AtomicI64>,
    reads: Arc<AtomicI64>,
}

fn counters() -> Counters {
    Counters { costs: Arc::new(AtomicI64::new(0)), prepares: Arc::new(AtomicI64::new(0)), runs: Arc::new(AtomicI64::new(0)), reads: Arc::new(AtomicI64::new(0)) }
}

/// A device with no residency whose kernels answer `rounded`, counting what it is asked.
struct Counting {
    main: ThreadId,
    counters: Counters,
    rounded: Arc<Vec<f64>>,
    cost: Option<f64>,
}

impl G::Backend for Counting {
    fn cost(&self, _: Option<&packed::Inputs>, _: bool, _: &packed::T, _: i64) -> Option<f64> {
        self.counters.costs.fetch_add(1, Ordering::SeqCst);
        self.cost
    }
    fn prepare(&self, _: bool, _: &packed::T) -> Result<Arc<dyn G::Kernel>, Diagnostic> {
        assert!(on_owner(self.main));
        self.counters.prepares.fetch_add(1, Ordering::SeqCst);
        Ok(Arc::new(CountingKernel { main: self.main, counters: self.counters.clone(), rounded: self.rounded.clone() }))
    }
}

struct CountingKernel {
    main: ThreadId,
    counters: Counters,
    rounded: Arc<Vec<f64>>,
}

impl G::Kernel for CountingKernel {
    fn run(&self, inputs: &packed::Inputs) -> Result<G::Value, Diagnostic> {
        assert!(on_owner(self.main));
        assert_eq!(inputs.count, COUNT);
        let n = self.counters.runs.fetch_add(1, Ordering::SeqCst) + 1;
        Ok(G::Value { identity: 1, count: COUNT, width: 3, output: 0, stamp: n, gpu_seconds: None })
    }
    fn readback(&self, _: &G::Value) -> Result<Value, Diagnostic> {
        assert!(on_owner(self.main));
        self.counters.reads.fetch_add(1, Ordering::SeqCst);
        Ok(Value::Floats(3, self.rounded.clone()))
    }
    fn planes(&self, value: &G::Value) -> Result<G::Planes, Diagnostic> {
        assert!(on_owner(self.main));
        G::planes_of(&|v: &G::Value| self.readback(v), value)
    }
}

fn backend(main: ThreadId, counters: &Counters, flat: Arc<Vec<f64>>, cost: Option<f64>) -> Arc<dyn G::Backend> {
    let rounded = Arc::new(flat.iter().map(|x| (*x as f32) as f64).collect());
    Arc::new(Counting { main, counters: counters.clone(), rounded, cost })
}

/// Counts the stub's calls, each of which must arrive on the owner thread.
#[derive(Clone)]
struct Calls {
    main: ThreadId,
    count: Arc<AtomicUsize>,
}

impl Calls {
    fn hit(&self) {
        assert!(on_owner(self.main));
        self.count.fetch_add(1, Ordering::SeqCst);
    }
}

/// A resident scope with canned answers, for the proxy to route to.
struct Stub(Calls);

impl G::Backend for Stub {
    fn residency(&self, fast: bool) -> Option<Box<dyn G::Residency>> {
        self.0.hit();
        assert!(fast);
        Some(Box::new(StubResidency { calls: self.0.clone(), executor: Arc::new(StubResident(self.0.clone())) }))
    }
    fn prepare(&self, _: bool, _: &packed::T) -> Result<Arc<dyn G::Kernel>, Diagnostic> {
        panic!("resident test dispatched ordinary kernel")
    }
}

struct StubResidency {
    calls: Calls,
    executor: Arc<dyn Resident>,
}

impl G::Residency for StubResidency {
    fn executor(&self) -> &Arc<dyn Resident> {
        &self.executor
    }
    fn retain_uint32(&self, _: &Value) -> Option<G::Integers> {
        self.calls.hit();
        Some(G::Integers { count: 1, storage: None, ops: Box::new(StubIntegers) })
    }
    fn retain_float(&self, _: &Value) -> Option<G::Planes> {
        self.calls.hit();
        None
    }
    fn pack_planes(&self, _: &[Value; 3]) -> Result<Option<Value>, Diagnostic> {
        Ok(None)
    }
    fn incidence(&self) -> G::IncidenceBuilder {
        Arc::new(|_, _| Ok(None))
    }
    fn validate_topology(&self, _: i64, _: &Value, _: &Value, _: &Value, _: i64) -> Option<Result<(), Diagnostic>> {
        self.calls.hit();
        Some(Ok(()))
    }
    fn ready(&self) -> bool {
        true
    }
}

struct StubIntegers;

impl G::IntegersOps for StubIntegers {
    fn resident(&self) -> Option<Value> {
        None
    }
    fn materialise(&self) -> Result<Vec<i64>, Diagnostic> {
        Ok(vec![16777217])
    }
    fn release(&self, _: bool) {}
}

struct StubResident(Calls);

impl Resident for StubResident {
    fn fill(&self, _: &Ty, _: i64, _: &Value) -> Result<Option<Value>, Diagnostic> {
        self.0.hit();
        Ok(Some(Value::Int_array(Arc::new(vec![0]))))
    }
    fn compact(&self, value: &Value) -> Result<Option<Value>, Diagnostic> {
        self.0.hit();
        Ok(Some(value.clone()))
    }
    fn coerce(&self, _: &Ty, value: &Value) -> Result<Option<Value>, Diagnostic> {
        self.0.hit();
        Ok(Some(value.clone()))
    }
    fn finish(&self, value: &Value) -> Result<Value, Diagnostic> {
        self.0.hit();
        Ok(value.clone())
    }
    fn count(&self, _: &Value) -> Result<Option<i64>, Diagnostic> {
        self.0.hit();
        Ok(Some(1))
    }
    fn close(&self) {
        self.0.hit();
    }
    fn settle(&self) -> Result<(), Diagnostic> {
        self.0.hit();
        Ok(())
    }
    fn range(&self, _: &Ty, _: &Value, _: i64, _: i64) -> Result<Option<Value>, Diagnostic> {
        self.0.hit();
        Ok(Some(Value::Int_array(Arc::new(vec![0]))))
    }
}

#[test]
fn a_worker_reaches_the_resident_scope_only_through_the_owner_pump() {
    let service = wg::create(None);
    let main = std::thread::current().id();
    let calls = Calls { main, count: Arc::new(AtomicUsize::new(0)) };
    wg::install_test_backend(&service, Arc::new(Stub(calls.clone())));
    let gpu = wg::config(&service, false);
    let done = AtomicBool::new(false);
    std::thread::scope(|s| {
        let worker = s.spawn(|| {
            struct Finish<'a>(&'a AtomicBool);
            impl Drop for Finish<'_> {
                fn drop(&mut self) {
                    self.0.store(true, Ordering::SeqCst);
                }
            }
            let _finish = Finish(&done);
            let scope = gpu.backend.as_ref().unwrap().residency(true).expect("residency");
            let r = scope.executor();
            let value = Value::Int_array(Arc::new(vec![0]));
            let array_int = Ty::Array(Box::new(Ty::Int));
            r.settle().unwrap();
            assert_eq!(r.compact(&value).unwrap(), Some(value.clone()));
            assert_eq!(r.coerce(&array_int, &value).unwrap(), Some(value.clone()));
            assert!(r.finish(&value).unwrap() == value && r.count(&value).unwrap() == Some(1));
            assert_eq!(r.range(&array_int, &Value::Int(1), 1, 1).unwrap(), Some(value.clone()));
            assert_eq!(r.fill(&array_int, 1, &Value::Int(0)).unwrap(), Some(value.clone()));
            assert_eq!(scope.retain_uint32(&value).unwrap().materialise().unwrap(), vec![16777217]);
            assert!(scope.retain_float(&value).is_none());
            ok(scope.validate_topology(1, &value, &value, &value, 3).expect("validated"));
            r.close();
        });
        while !done.load(Ordering::SeqCst) {
            wg::pump(&service, true);
            std::hint::spin_loop();
        }
        worker.join().unwrap();
    });
    assert_eq!(calls.count.load(Ordering::SeqCst), 12);
    wg::close(&service);
}

#[test]
fn a_worker_cook_selects_cpu_when_slower_and_reads_back_when_cheaper() {
    let main = std::thread::current().id();
    let cpu_lowered = ok(lower::workspace(None, Vec::new(), lower::Options::default(), &forms()));
    let cpu_graph = cpu_lowered.graphs[0].clone();
    assert!(!cpu_graph.network.frame_nodes.is_empty());
    let cpu = cook(None, &cpu_graph);
    let flat = Arc::new(cpu.clone());
    let c = counters();
    let owner = wg::create(None);
    let on_worker = |graph: &lower::Graph, scoped: bool| -> Vec<f64> {
        let work = |gpu: &G::Config| -> Vec<f64> {
            let finished = AtomicBool::new(false);
            let service_seen = AtomicBool::new(false);
            let result = std::thread::scope(|s| {
                let worker = s.spawn(|| {
                    struct Finish<'a>(&'a AtomicBool);
                    impl Drop for Finish<'_> {
                        fn drop(&mut self) {
                            self.0.store(true, Ordering::SeqCst);
                        }
                    }
                    let _finish = Finish(&finished);
                    service_seen.store(gpu.backend.is_some(), Ordering::SeqCst);
                    cook(Some(gpu), graph)
                });
                while !finished.load(Ordering::SeqCst) {
                    wg::pump(&owner, true);
                    std::hint::spin_loop();
                }
                worker.join().unwrap()
            });
            assert!(service_seen.load(Ordering::SeqCst));
            result
        };
        if scoped {
            let mut out = Vec::new();
            wg::with_backend(&owner, false, &mut |gpu| out = work(gpu));
            out
        } else {
            work(&wg::config(&owner, false))
        }
    };
    wg::install_test_backend(&owner, backend(main, &c, flat.clone(), Some(1.0)));
    let gpu = wg::config(&owner, false);
    let gpu_lowered = lower_with(gpu);
    let graph = gpu_lowered.graphs[0].clone();
    let slower = on_worker(&graph, true);
    assert_eq!(c.runs.load(Ordering::SeqCst), 0);
    assert_eq!(slower, cpu);
    wg::install_test_backend(&owner, backend(main, &c, flat, Some(0.)));
    wg::with_backend(&owner, false, &mut |_| {});
    // The lowered graph captures its immutable backend config. Re-lower after replacing the test
    // backend instead of expecting an old graph to observe the replacement.
    let cheap_lowered = lower_with(wg::config(&owner, false));
    let cheap_graph = cheap_lowered.graphs[0].clone();
    let placed = on_worker(&cheap_graph, false);
    let (costs, prepares, runs, reads) = (c.costs.load(Ordering::SeqCst), c.prepares.load(Ordering::SeqCst), c.runs.load(Ordering::SeqCst), c.reads.load(Ordering::SeqCst));
    if runs != 1 || reads != 1 {
        panic!("expected one worker dispatch/readback, got cost/prepare/run/readback {costs}/{prepares}/{runs}/{reads}");
    }
    for i in 0..COUNT as usize {
        let near = |a: f64, b: f64| (a - b).abs() <= 1e-4 * (1. + a.abs());
        assert!(near(cpu[3 * i], placed[3 * i]) && near(cpu[3 * i + 1], placed[3 * i + 1]) && near(cpu[3 * i + 2], placed[3 * i + 2]));
    }
    wg::close(&owner);
}

#[test]
fn nested_scopes_keep_their_own_backend() {
    let main = std::thread::current().id();
    let flat = Arc::new(vec![0.; 3]);
    let c = counters();
    let outer = wg::create(None);
    wg::install_test_backend(&outer, backend(main, &c, flat.clone(), Some(0.)));
    let nested = wg::create(None);
    wg::install_test_backend(&nested, backend(main, &c, flat, Some(0.)));
    wg::with_backend(&outer, false, &mut |first| {
        wg::with_backend(&nested, false, &mut |second| {
            assert!(!Arc::ptr_eq(first.backend.as_ref().unwrap(), second.backend.as_ref().unwrap()));
        });
        assert!(Arc::ptr_eq(first.backend.as_ref().unwrap(), wg::config(&outer, false).backend.as_ref().unwrap()));
    });
    wg::close(&nested);
    wg::close(&outer);
}

#[test]
fn closing_cancels_a_waiting_request() {
    let main = std::thread::current().id();
    let c = counters();
    let closing = wg::create(None);
    wg::install_test_backend(&closing, backend(main, &c, Arc::new(vec![0.; 3]), Some(0.)));
    wg::with_backend(&closing, false, &mut |_| {});
    let closing_remote = wg::remote(&closing);
    let entered = AtomicBool::new(false);
    let cancelled = std::thread::scope(|s| {
        let waiting = s.spawn(|| {
            entered.store(true, Ordering::SeqCst);
            wg::on_owner_for_test(&closing_remote, false, Box::new(|| {})).err()
        });
        while !entered.load(Ordering::SeqCst) {
            std::hint::spin_loop();
        }
        wg::close(&closing);
        waiting.join().unwrap()
    });
    assert!(cancelled.is_some_and(|d| d.code == "E_GPU"));
}

/// A pump that served a request waits briefly for the worker's next one, but never takes one that
/// blocks on the device (a settle): that waits for the next pump, so the frame never stalls on the
/// GPU. The settle is sent only after the first request returns, so no pump can find both queued.
#[test]
fn a_pump_never_takes_a_settle_as_a_follow_up() {
    let chained = wg::create(None);
    let chained_remote = wg::remote(&chained);
    let (first, settled) = (Arc::new(AtomicBool::new(false)), Arc::new(AtomicBool::new(false)));
    std::thread::scope(|s| {
        let (f, st) = (first.clone(), settled.clone());
        let worker = s.spawn(|| {
            wg::on_owner_for_test(&chained_remote, false, Box::new(move || f.store(true, Ordering::SeqCst))).unwrap();
            wg::on_owner_for_test(&chained_remote, true, Box::new(move || st.store(true, Ordering::SeqCst))).unwrap();
        });
        while !first.load(Ordering::SeqCst) {
            wg::pump(&chained, true);
        }
        assert!(!settled.load(Ordering::SeqCst));
        while !settled.load(Ordering::SeqCst) {
            wg::pump(&chained, true);
        }
        worker.join().unwrap();
    });
    wg::close(&chained);
}

/// `wait` sleeps until a request is queued: a request sent while the owner waits wakes it long
/// before a 10 s timeout, and a stale wake only makes one wait return early.
#[test]
fn wait_wakes_when_a_request_is_queued() {
    let sleeper = wg::create(None);
    let sleeper_remote = wg::remote(&sleeper);
    let sent = Arc::new(AtomicBool::new(false));
    std::thread::scope(|s| {
        let flag = sent.clone();
        let sender = s.spawn(|| {
            std::thread::sleep(Duration::from_millis(50));
            wg::on_owner_for_test(&sleeper_remote, false, Box::new(move || flag.store(true, Ordering::SeqCst))).unwrap();
        });
        let start = Instant::now();
        wg::wait(&sleeper, 10.);
        assert!(start.elapsed() < Duration::from_secs(5));
        wg::pump(&sleeper, false);
        assert!(sent.load(Ordering::SeqCst));
        sender.join().unwrap();
    });
    wg::close(&sleeper);
}
