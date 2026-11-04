// Re-entrant and concurrent evaluation: host callbacks that evaluate, rewrite and bind again in the evaluation
// that called them (OCaml has no lock: they share it), from 8 threads at once, one arena shared by all of them.
use crate::*;
use crate::test_workspace_eval::{check, ok};
use flow_check::workspace as W;
use std::sync::{Arc, atomic::{AtomicI64, Ordering}, mpsc};

const SOURCE: &str = "(workspace w (graph g :context host (let* [xs (array/range 8) ys (map (fn [i] (+ (* i 2) t)) xs)] (array/sum ys))))";
fn expected(t: f64) -> f64 { 56.0 + 8.0 * t }

// The kernel handler re-enters the evaluation it was offered by: a nested live evaluation at another frame, a rewrite
// whose callback rewrites again, a dependence walk and a rebinding. It declines, so the evaluator walks the map itself.
fn handlers(nested: Arc<AtomicI64>) -> ResidualHandlers {
    let fresh = Arc::new(AtomicI64::new(0));
    ResidualHandlers {
        kernel: Some(Arc::new(move |id| {
            let view = Residual::residual_view(id).unwrap();
            if let W::Node::Hof(W::HofKind::Map, terms) = &view.term.node {
                let xs = Residual::eval_term(id, &terms[1], &input::at_time(9.0), None, None, None).unwrap();
                assert_eq!(flow_syntax::value::array_length(&xs).unwrap(), 8);
            }
            let fresh = fresh.clone();
            let mint = move || -(1i64 << 50) - fresh.fetch_add(1, Ordering::Relaxed) - 1;
            let inner = |value: &Value| match value {
                Value::Fn(_) => Some(Host::rewrite_with(value, &|_| None, &mint, &|_, _| None)),
                _ => None,
            };
            let copy = Host::rewrite_with(&Value::Residual(id), &inner, &mint, &|_, _| None);
            let _ = frame_dependent(&copy);
            let _ = Residual::with_bindings(id, &view.term, &[]).unwrap();
            nested.fetch_add(1, Ordering::Relaxed);
            None
        })),
        ..Default::default()
    }
}

fn force_at(evaluated: &Eval, value: &Value, t: f64, nested: &Arc<AtomicI64>) -> f64 {
    let mut out = None;
    let _ = Residual::handle(handlers(nested.clone()), &mut || { out = Some(force(value, &input::at_time(t), None)); Ok(evaluated.clone()) });
    match out.unwrap().unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) {
        Value::Int(n) => n as f64,
        Value::Float(x) => x,
        v => panic!("not a number: {v:?}"),
    }
}

#[test]
fn reentrant_callbacks_on_eight_threads_share_arenas_without_deadlock() {
    let ws = check(SOURCE);
    let shared = ok(static_eval(&ws));
    let residual = shared.results[0].1.clone();
    assert!(matches!(residual, Value::Residual(_)), "the graph is live");
    // the offers one force makes, alone
    let alone = Arc::new(AtomicI64::new(0));
    assert_eq!(force_at(&shared, &residual, 0.5, &alone), expected(0.5));
    let per_force = alone.load(Ordering::Relaxed);
    assert!(per_force > 0, "the map is offered to the kernel handler");
    let nested = Arc::new(AtomicI64::new(0));
    let (done, finished) = mpsc::channel();
    for thread in 0..8 {
        let (ws, shared, residual, nested, done) = (ws.clone(), shared.clone(), residual.clone(), nested.clone(), done.clone());
        std::thread::spawn(move || {
            for run in 0..50 {
                let t = (thread * 50 + run) as f64 * 0.25;
                // the arena every thread shares, and one of this thread's own
                assert_eq!(force_at(&shared, &residual, t, &nested), expected(t), "shared arena, thread {thread} run {run}");
                let own = ok(static_eval(&ws));
                let value = own.results[0].1.clone();
                assert_eq!(force_at(&own, &value, t, &nested), expected(t), "own arena, thread {thread} run {run}");
            }
            done.send(thread).unwrap();
        });
    }
    drop(done);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    for _ in 0..8 {
        let left = deadline.saturating_duration_since(std::time::Instant::now());
        finished.recv_timeout(left).expect("a thread hung or panicked in re-entrant evaluation");
    }
    assert_eq!(nested.load(Ordering::Relaxed), 8 * 50 * 2 * per_force, "every evaluation made the same offers and re-entered each");
}

// A kernel handler that evaluates the very term it was offered (a kernel compile lowering its residual) is not offered it again: the
// callback runs outside its handler, as an OCaml effect handler's does. Before, this recursed until the stack overflowed (procedural_modeling).
#[test]
fn a_kernel_callback_evaluating_its_own_term_is_not_offered_it_again() {
    let ws = check(SOURCE);
    let evaluated = ok(static_eval(&ws));
    let residual = evaluated.results[0].1.clone();
    let offers = Arc::new(AtomicI64::new(0));
    let counted = offers.clone();
    let handlers = ResidualHandlers {
        kernel: Some(Arc::new(move |id| {
            counted.fetch_add(1, Ordering::Relaxed);
            assert!(counted.load(Ordering::Relaxed) < 100, "the kernel handler was offered its own term again");
            let view = Residual::residual_view(id).unwrap();
            let _ = Residual::eval_term(id, &view.term, &input::at_time(2.0), None, None, None).unwrap();
            None
        })),
        ..Default::default()
    };
    let mut out = None;
    let _ = Residual::handle(handlers, &mut || { out = Some(force(&residual, &input::at_time(2.0), None)); Ok(evaluated.clone()) });
    assert_eq!(out.unwrap().unwrap(), Value::Float(expected(2.0)));
    assert!(offers.load(Ordering::Relaxed) > 0);
}
