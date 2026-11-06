//! Port of lib/flow_native/test_workspace_gpu_service.ml: a worker cook reaches the
//! workspace-owned packed-attribute GPU proxy only while the owner thread pumps it.
//!
//! "Initial domain" is the thread that created the service (the test's own thread); the OCaml
//! `Domain.spawn` workers are scoped threads.
use crate::workspace_gpu as wg;
use flow_ir::core_exec::Resident;
use flow_ir::flow_ir::Gpu as G;
use flow_sop::{lower, value_lane};
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use sop::{context, edit_graph as edit, payload, session};
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicUsize, Ordering};
use std::sync::Arc;
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
    let positions = rdk::geometry::positions(&geometry);
    let mut flat = Vec::new();
    for i in 0..COUNT {
        let (x, y, z) = rdk::packed::Float3::get(positions, i).unwrap();
        flat.extend([x, y, z]);
    }
    flat
}

fn on_owner(main: std::thread::ThreadId) -> bool {
    std::thread::current().id() == main
}

struct Counters {
    costs: Arc<AtomicI64>,
    prepares: Arc<AtomicI64>,
    runs: Arc<AtomicI64>,
    reads: Arc<AtomicI64>,
}

fn backend(main: std::thread::ThreadId, counters: &Counters, flat: Arc<Vec<f64>>, cost: Option<f64>) -> G::Backend {
    let (costs, prepares, runs, reads) = (counters.costs.clone(), counters.prepares.clone(), counters.runs.clone(), counters.reads.clone());
    G::Backend {
        residency: Box::new(|_| None),
        cost: Box::new(move |_, _, _, _| {
            costs.fetch_add(1, Ordering::SeqCst);
            cost
        }),
        prepare: Box::new(move |_, _| {
            assert!(on_owner(main));
            prepares.fetch_add(1, Ordering::SeqCst);
            let (runs, reads, flat2) = (runs.clone(), reads.clone(), flat.clone());
            let rounded: Arc<Vec<f64>> = Arc::new(flat.iter().map(|x| (*x as f32) as f64).collect());
            let readback_rounded = rounded.clone();
            let readback = move |_: &G::Value| {
                assert!(on_owner(main));
                reads.fetch_add(1, Ordering::SeqCst);
                Ok(Value::Vec3_array(readback_rounded.clone()))
            };
            let planes_rounded = rounded;
            let _ = flat2;
            Ok(Arc::new(G::Kernel {
                run: Box::new(move |inputs| {
                    assert!(on_owner(main));
                    assert_eq!(inputs.count, COUNT);
                    let n = runs.fetch_add(1, Ordering::SeqCst) + 1;
                    Ok(G::Value { identity: 1, count: COUNT, width: 3, output: 0, stamp: n, gpu_seconds: None })
                }),
                readback: Box::new(readback.clone()),
                planes: Box::new(move |value| {
                    assert!(on_owner(main));
                    let _ = &planes_rounded;
                    G::planes_of(&readback, value)
                }),
            }))
        }),
    }
}

fn counters() -> Counters {
    Counters { costs: Arc::new(AtomicI64::new(0)), prepares: Arc::new(AtomicI64::new(0)), runs: Arc::new(AtomicI64::new(0)), reads: Arc::new(AtomicI64::new(0)) }
}

#[test]
fn a_worker_reaches_the_resident_scope_only_through_the_owner_pump() {
    let service = wg::create(None);
    let main = std::thread::current().id();
    let calls = Arc::new(AtomicUsize::new(0));
let owner: Arc<dyn Fn() + Send + Sync> = {        let calls = calls.clone();        Arc::new(move || {            assert!(on_owner(main));            calls.fetch_add(1, Ordering::SeqCst);        })    };    let ow = owner.clone();
    let backend = G::Backend {
        cost: Box::new(|_, _, _, _| None),
        prepare: Box::new(|_, _| panic!("resident test dispatched ordinary kernel")),
        residency: Box::new(move |fast| {
            ow();
            assert!(fast);
let mut os: Vec<Arc<dyn Fn() + Send + Sync>> = (0..16).map(|_| owner.clone()).collect();            let (o16, o15, o14, o13, o12, o11, o10, o9, o8, o7, o6, o5, o4, o3, o2, o1) = (os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap(), os.pop().unwrap());
            Some(G::Residency {
                executor: Arc::new(Resident {
                    fill: Box::new(move |_, _, _| {
                        o1();
                        Ok(Some(Value::Int_array(Arc::new(vec![0]))))
                    }),
                    invoke: Box::new(move |_, _, _, _| {
                        o2();
                        Ok(None)
                    }),
                    unfold: Box::new(move |_, _, _, _, _, _| {
                        o3();
                        Ok(None)
                    }),
                    keep: Box::new(move |_| {
                        o4();
                        Ok(None)
                    }),
                    compact: Box::new(move |value| {
                        o5();
                        Ok(Some(value.clone()))
                    }),
                    coerce: Box::new(move |_, value| {
                        o6();
                        Ok(Some(value.clone()))
                    }),
                    concat: Box::new(move |a, _| {
                        o7();
                        Ok(Some(a.clone()))
                    }),
                    finish: Box::new(move |value| {
                        o8();
                        Ok(value.clone())
                    }),
                    count: Box::new(move |_| {
                        o9();
                        Ok(Some(1))
                    }),
                    capacity: Box::new(move |_| {
                        o10();
                        Some(1)
                    }),
                    close: Box::new(move || o11()),
                    settle: Box::new(move || {
                        o12();
                        Ok(())
                    }),
                    range: Box::new(move |_, _, _, _| {
                        o13();
                        Ok(Some(Value::Int_array(Arc::new(vec![0]))))
                    }),
                }),
                retain_uint32: Box::new(move |_| {
                    o14();
                    Some(G::Integers { count: 1, storage: None, materialise: Box::new(|| vec![16777217]), release: Box::new(|_| {}) })
                }),
                retain_float: Box::new(move |_| {
                    o15();
                    None
                }),
                validate_topology: Box::new(move |_, _, _, _, _| {
                    o16();
                    Some(Ok(()))
                }),
                ready: Box::new(|| true),
            })
        }),
    };
    wg::install_test_backend(&service, backend);
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
            let scope = (gpu.backend.as_ref().unwrap().residency)(true).expect("residency");
            let r = &scope.executor;
            let value = Value::Int_array(Arc::new(vec![0]));
            let array_int = Ty::Array(Box::new(Ty::Int));
            (r.settle)().unwrap();
            assert_eq!((r.compact)(&value).unwrap(), Some(value.clone()));
            assert_eq!((r.coerce)(&array_int, &value).unwrap(), Some(value.clone()));
            assert!((r.finish)(&value).unwrap() == value && (r.count)(&value).unwrap() == Some(1));
            assert_eq!((r.range)(&array_int, &Value::Int(1), 1, 1).unwrap(), Some(value.clone()));
            assert_eq!((r.fill)(&array_int, 1, &Value::Int(0)).unwrap(), Some(value.clone()));
            assert_eq!(((scope.retain_uint32)(&value).unwrap().materialise)(), vec![16777217]);
            assert!((scope.retain_float)(&value).is_none());
            ok((scope.validate_topology)(1, &value, &value, &value, 3).expect("validated"));
            (r.close)();
        });
        while !done.load(Ordering::SeqCst) {
            wg::pump(&service, true);
            std::hint::spin_loop();
        }
        worker.join().unwrap();
    });
    assert_eq!(calls.load(Ordering::SeqCst), 12);
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
