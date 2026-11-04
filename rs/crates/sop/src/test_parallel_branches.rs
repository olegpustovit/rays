//! Port of lib/sop/test_parallel_branches.ml: parallel branch transactions of `Session::cook` are
//! byte-, key- and counter-identical to input-order execution, including cancellation and errors.
//! The OCaml test builds some graphs from Lisp SOPs (noise_displace, merge, grid); here the same
//! shapes are native nodes that allocate new planes (`shifted`), since the Lisp reader is not part
//! of this crate. Timings are driven by an injected clock instead of sleeping.
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use rdk::{attribute, cancel, geometry, packed};

use crate::context::{self, dependencies};
use crate::node::{self, build, CookMode};
use crate::session;
use crate::support;
use crate::test_support::*;
use crate::zone;

type Run = Arc<dyn Fn(&context::T) + Send + Sync>;

fn ctx(domains: i64, cancel: Option<Arc<cancel::T>>) -> context::T {
    context::create(context::Options { domains: Some(domains), grain: Some(97), seed: Some(42), cancel, ..Default::default() })
        .unwrap()
}

fn sess(entries: i64) -> session::T {
    session::create(entries, 64 * 1024 * 1024).unwrap()
}

fn cook_at(s: &mut session::T, domains: i64, n: &node::T) -> session::Output {
    match session::cook(s, None, &ctx(domains, None), n) {
        Ok(output) => output,
        Err(error) => panic!("{}", rays_diagnostic::error_to_string(&error)),
    }
}

/// `node ?inputs ?run label geometry`: a generic node returning `geometry` after `run`.
fn node(label: &str, inputs: &[node::T], run: Option<Run>, geometry: &geometry::T) -> node::T {
    let geometry = geometry.clone();
    build::make_geometry(
        build::Args {
            label: Some(label.to_owned()),
            parameters: label.as_bytes().to_vec(),
            cook_mode: CookMode::Generic,
            dependencies: dependencies::STATIC,
            inputs: inputs.to_vec(),
            ..build::args("branch_test")
        },
        None,
        Arc::new(move |_, context, _| {
            if let Some(run) = &run {
                run(context);
            }
            Ok(node::GeometryCooked { geometry: geometry.clone(), instances: None })
        }),
    )
}

fn counters(s: &session::T) -> [i64; 9] {
    let s = session::stats(s);
    [
        s.cooks,
        s.hits,
        s.misses,
        s.evictions,
        s.volatile_hits,
        s.volatile_misses,
        s.retained_entries,
        s.volatile_entries,
        s.retained_payload_bytes,
    ]
}

/// Bit patterns of the positions and every float attribute: byte identity independent of data ids.
fn geometry_bytes(g: &geometry::T) -> Vec<u64> {
    let view = packed::Float3::View::view(geometry::positions(g)).unwrap();
    let mut bits: Vec<u64> = [&view.x, &view.y, &view.z].iter().flat_map(|p| p.iter().map(|v| v.to_bits())).collect();
    for a in geometry::attributes(g) {
        if let Ok(attribute::Storage::Float(values)) = attribute::storage(&a) {
            bits.extend(values.iter().map(|v| v.to_bits()));
        }
    }
    bits
}

fn bytes_of(output: &session::Output) -> Vec<u64> {
    geometry_bytes(&geometry_of(output))
}

fn exact(entries: i64, prepare: &dyn Fn(&mut session::T), graph: &node::T) -> (session::Output, session::Output, i64) {
    let (mut one, mut eight) = (sess(entries), sess(entries));
    prepare(&mut one);
    prepare(&mut eight);
    session::set_parallel_override(&mut one, Some(true));
    session::set_parallel_override(&mut eight, Some(true));
    let a = cook_at(&mut one, 1, graph);
    let b = cook_at(&mut eight, 8, graph);
    assert_eq!(bytes_of(&a), bytes_of(&b), "branch geometry changed");
    assert_eq!(counters(&one), counters(&eight), "input-order cache counters changed");
    assert_eq!(
        session::private::cache_keys(&one),
        session::private::cache_keys(&eight),
        "cache identities changed for shared immutable output planes"
    );
    let fanouts = session::private::fanouts(&eight);
    session::close(&mut one);
    session::close(&mut eight);
    (a, b, fanouts)
}

fn no_prepare(_: &mut session::T) {}

fn geometry_fixture() -> geometry::T {
    rdk::line_geometry::points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]).unwrap()
}

/// A positions-changing transform that allocates a new plane (a stand-in for noise_displace).
fn shifted(by: f64) -> Transform {
    Arc::new(move |g| {
        let view = packed::Float3::View::view(geometry::positions(g)).unwrap();
        let moved = packed::Float3::of_owned(
            view.x.iter().enumerate().map(|(i, x)| x + by + i as f64 * 0.125).collect(),
            view.y.to_vec(),
            view.z.to_vec(),
        )
        .unwrap();
        geometry::with_positions(moved, g).unwrap()
    })
}

/// Joins two inputs: the first input's plane shifted by the second's x (depends on both).
fn join(label: &str, a: &node::T, b: &node::T) -> node::T {
    build::make_geometry(
        build::Args {
            label: Some(label.to_owned()),
            parameters: label.as_bytes().to_vec(),
            cook_mode: CookMode::Generic,
            inputs: vec![a.clone(), b.clone()],
            ..build::args("join_test")
        },
        None,
        Arc::new(|_, _, inputs| {
            let (a, b) = (inputs[0].clone(), inputs[1].clone());
            let (pa, pb) = (
                packed::Float3::View::view(geometry::positions(&a)).unwrap(),
                packed::Float3::View::view(geometry::positions(&b)).unwrap(),
            );
            let x: Vec<f64> = pa.x.iter().zip(pb.x.iter()).map(|(p, q)| p + q).collect();
            let merged = packed::Float3::of_owned(x, pa.y.to_vec(), pa.z.to_vec()).unwrap();
            Ok(node::GeometryCooked { geometry: geometry::with_positions(merged, &a).unwrap(), instances: None })
        }),
    )
}

fn tally(calls: &Arc<AtomicI64>) -> Run {
    let calls = Arc::clone(calls);
    Arc::new(move |_| {
        calls.fetch_add(1, Ordering::SeqCst);
    })
}

#[test]
fn independent_branches_fork_and_shared_prefix_cooks_once() {
    let geometry = geometry_fixture();
    let (source_a, source_b) = (node("source_a", &[], None, &geometry), node("source_b", &[], None, &geometry));
    let a = node("a", &[source_a], None, &geometry);
    let b = node("b", &[source_b], None, &geometry);
    let root = node("join", &[a, b], None, &geometry);
    let (_, _, fanouts) = exact(64, &no_prepare, &root);
    assert_eq!(fanouts, 1, "two independent branches were not forked");

    let shared_calls = calls();
    let shared = node("shared", &[], Some(tally(&shared_calls)), &geometry);
    let left = node("left", &[shared.clone()], None, &geometry);
    let right = node("right", &[shared.clone()], None, &geometry);
    let (_, _, fanouts) = exact(64, &no_prepare, &node("shared_join", &[left.clone(), right.clone()], None, &geometry));
    assert_eq!(count(&shared_calls), 2, "an uncached shared ancestor cooked more than once per session");
    assert_eq!(fanouts, 1, "shared prefix prevented independent suffix work");

    let preceding = node("preceding", &[], None, &geometry);
    let prefix_left = node("prefix_left", &[preceding, shared], None, &geometry);
    exact(2, &no_prepare, &node("prefix_order_join", &[prefix_left, right.clone()], None, &geometry));

    // zero retention and volatile sessions
    exact(0, &no_prepare, &root);
    shared_calls.store(0, Ordering::SeqCst);
    exact(0, &no_prepare, &node("zero_shared_join", &[left, right], None, &geometry));
    assert_eq!(count(&shared_calls), 2, "zero-retention shared prefix cooked more than once");
    let volatile = |s: &mut session::T| session::set_volatile(s, Arc::new(|_| true));
    exact(0, &volatile, &root);
    exact(64, &volatile, &root);
}

#[test]
fn stale_hit_after_eviction_is_recooked_sequentially() {
    // A later worker initially hits old_a; the preceding insertion evicts it. Replaying the stale
    // hit would change counters and CLOCK state.
    let geometry = geometry_fixture();
    let (old_a, old_b) = (node("old_a", &[], None, &geometry), node("old_b", &[], None, &geometry));
    let new_a = node("new_a", &[], None, &geometry);
    let later = node("later", &[old_a.clone()], None, &geometry);
    let prepare = move |s: &mut session::T| {
        cook_at(s, 1, &old_a);
        cook_at(s, 1, &old_b);
    };
    let (_, _, fanouts) = exact(2, &prepare, &node("eviction_join", &[new_a, later], None, &geometry));
    assert_eq!(fanouts, 1, "eviction regression did not exercise a parallel transaction");
}

#[test]
fn warm_cache_hits_are_not_forked_and_reused_ids_are_exact() {
    let geometry = geometry_fixture();
    let a = node("a", &[node("source_a", &[], None, &geometry)], None, &geometry);
    let b = node("b", &[node("source_b", &[], None, &geometry)], None, &geometry);
    let root = node("join", &[a.clone(), b.clone()], None, &geometry);
    let mut cached = sess(64);
    session::set_parallel_override(&mut cached, Some(true));
    cook_at(&mut cached, 8, &root);
    let before = session::private::fanouts(&cached);
    assert_eq!(before, 1);
    cook_at(&mut cached, 8, &root);
    assert_eq!(session::private::fanouts(&cached), before, "warm cache hits were forked");
    session::close(&mut cached);
    let reused = build::restore_id(node::id(&a), &b).unwrap();
    exact(64, &no_prepare, &node("reused_id_join", &[a.clone(), reused.clone()], None, &geometry));
    exact(0, &no_prepare, &node("zero_reused_id_join", &[a, reused], None, &geometry));
}

#[test]
fn learned_costs_gate_the_automatic_fanout() {
    let geometry = geometry_fixture();
    let mut learned = sess(64);
    // each cook measures exactly 10 ms on this clock (its start and end read consecutively)
    let ticks = Arc::new(AtomicI64::new(0));
    session::set_clock(&mut learned, Arc::new(move || ticks.fetch_add(1, Ordering::SeqCst) as f64 * 0.01));
    let slow = |label: &str| node(label, &[], None, &geometry);
    let suffix = |label: &str| node(label, &[slow(&format!("{label}_source"))], None, &geometry);
    let graph = node("cost_join", &[suffix("cost_a"), suffix("cost_b")], None, &geometry);
    cook_at(&mut learned, 8, &graph);
    assert_eq!(session::private::fanouts(&learned), 0, "cold unknown costs bypassed the automatic gate");
    session::private::clear_cache_keep_timings(&mut learned);
    cook_at(&mut learned, 8, &graph);
    assert_eq!(session::private::fanouts(&learned), 1, "cheap final operators hid measured heavy subtrees");
    session::close(&mut learned);
}

#[test]
fn component_cache_refreshes_are_not_forked_as_cold_cooks() {
    let geometry = with_color(1.0, &fixture());
    let source = snapshot(&geometry);
    let tally = calls();
    let a = counted(&source, &tally, &["P"], &["P"], shifted(1.0));
    let b = counted(&source, &tally, &["P"], &["P"], shifted(2.0));
    let merged = join("merge", &a, &b);
    let mut refreshing = sess(64);
    session::set_parallel_override(&mut refreshing, Some(true));
    cook_at(&mut refreshing, 8, &merged);
    assert_eq!(session::private::fanouts(&refreshing), 1, "the cold cook was not forked");
    let source = snapshot(&with_color(2.0, &geometry));
    cook_at(&mut refreshing, 8, &source);
    let a = build::rebuild_with_inputs(&a, &[source.clone()]);
    let b = build::rebuild_with_inputs(&b, &[source]);
    let before = session::private::fanouts(&refreshing);
    cook_at(&mut refreshing, 8, &build::rebuild_with_inputs(&merged, &[a, b]));
    assert_eq!(session::private::fanouts(&refreshing), before, "component cache refreshes were forked as cold cooks");
    session::close(&mut refreshing);
}

#[test]
fn branches_execute_concurrently() {
    // fails if the first branch runs synchronously before the remaining branches are scheduled
    let geometry = geometry_fixture();
    let ready = Arc::new(AtomicI64::new(0));
    let rendezvous: Run = {
        let ready = Arc::clone(&ready);
        Arc::new(move |_| {
            ready.fetch_add(1, Ordering::SeqCst);
            let deadline = Instant::now() + Duration::from_secs(5);
            while ready.load(Ordering::SeqCst) < 2 && Instant::now() < deadline {
                std::thread::yield_now();
            }
            assert_eq!(ready.load(Ordering::SeqCst), 2, "two coarse branches did not execute concurrently");
        })
    };
    let mut parallel = sess(64);
    session::set_parallel_override(&mut parallel, Some(true));
    let graph = node(
        "barrier_join",
        &[
            node("barrier_a", &[], Some(Arc::clone(&rendezvous)), &geometry),
            node("barrier_b", &[], Some(rendezvous), &geometry),
        ],
        None,
        &geometry,
    );
    cook_at(&mut parallel, 8, &graph);
    session::close(&mut parallel);
}

fn broken(label: &str) -> node::T {
    let label_owned = label.to_owned();
    build::make_geometry(
        build::Args {
            label: Some(label.to_owned()),
            parameters: label.as_bytes().to_vec(),
            cook_mode: CookMode::Generator,
            ..build::args("broken")
        },
        None,
        Arc::new(move |_, _, _| Err(support::diagnostic(&label_owned, &label_owned))),
    )
}

#[test]
fn first_input_error_wins_and_counters_match() {
    let geometry = geometry_fixture();
    let errors = node("errors", &[broken("first"), broken("second")], None, &geometry);
    let (mut one, mut eight) = (sess(64), sess(64));
    session::set_parallel_override(&mut eight, Some(true));
    let first = |s: &mut session::T, domains| match session::cook(s, None, &ctx(domains, None), &errors) {
        Err(error) => rays_diagnostic::error_to_string(&error),
        Ok(_) => panic!("broken branches succeeded"),
    };
    assert_eq!(first(&mut one, 1), first(&mut eight, 8), "first input error/trace changed");
    assert_eq!(counters(&one), counters(&eight), "later failed branch changed parent counters");
    session::close(&mut one);
    session::close(&mut eight);
}

#[test]
fn cancellation_keeps_its_typed_diagnostic_and_inserts_nothing() {
    let geometry = geometry_fixture();
    let token = Arc::new(cancel::create());
    let cancel_now: Run = {
        let token = Arc::clone(&token);
        Arc::new(move |_| cancel::cancel(&token))
    };
    let graph = node(
        "cancel_join",
        &[node("cancel_now", &[], Some(cancel_now), &geometry), node("other", &[], None, &geometry)],
        None,
        &geometry,
    );
    let mut cancelled = sess(64);
    session::set_parallel_override(&mut cancelled, Some(true));
    match session::cook(&mut cancelled, None, &ctx(8, Some(token)), &graph) {
        Err(error) => assert_eq!(error.code, "cancelled", "cancellation lost its typed diagnostic"),
        Ok(_) => panic!("cancelled branch succeeded"),
    }
    assert!(session::private::cache_keys(&cancelled).is_empty(), "cancelled fanout inserted cache entries");
    session::close(&mut cancelled);

    // zero retention
    let token = Arc::new(cancel::create());
    let zero_cancel: Run = {
        let token = Arc::clone(&token);
        Arc::new(move |_| cancel::cancel(&token))
    };
    let mut uncached = sess(0);
    match session::cook(&mut uncached, None, &ctx(8, Some(token)), &node("zero_cancel", &[], Some(zero_cancel), &geometry)) {
        Err(error) => assert_eq!(error.code, "cancelled", "zero-retention cook lost cancellation"),
        Ok(_) => panic!("zero-retention cancelled cook succeeded"),
    }
    assert!(session::private::cache_keys(&uncached).is_empty(), "zero-retention cancellation retained a key");
    session::close(&mut uncached);
}

#[test]
fn later_cancellation_does_not_hide_the_earlier_error() {
    let geometry = geometry_fixture();
    let token = Arc::new(cancel::create());
    let entered = Arc::new(AtomicBool::new(false));
    let first = {
        let entered = Arc::clone(&entered);
        build::make_geometry(
            build::Args { cook_mode: CookMode::Generator, ..build::args("first_error") },
            None,
            Arc::new(move |_, _, _| {
                entered.store(true, Ordering::SeqCst);
                Err(support::diagnostic("first_error", "first error"))
            }),
        )
    };
    let cancel_after_first: Run = {
        let (entered, token) = (Arc::clone(&entered), Arc::clone(&token));
        Arc::new(move |_| {
            let deadline = Instant::now() + Duration::from_secs(5);
            while !entered.load(Ordering::SeqCst) && Instant::now() < deadline {
                std::thread::yield_now();
            }
            assert!(entered.load(Ordering::SeqCst), "first error branch was not scheduled");
            cancel::cancel(&token);
        })
    };
    let graph = node("first_error_join", &[first, node("later_cancel", &[], Some(cancel_after_first), &geometry)], None, &geometry);
    let mut cancelled = sess(64);
    session::set_parallel_override(&mut cancelled, Some(true));
    match session::cook(&mut cancelled, None, &ctx(8, Some(token)), &graph) {
        Err(error) => assert_eq!(error.code, "first_error", "later cancellation hid the earlier branch error"),
        Ok(_) => panic!("error/cancellation succeeded"),
    }
    session::close(&mut cancelled);
}

#[test]
fn allocating_chains_are_byte_identical() {
    // Real allocating kernels: byte identity, independently of runtime data ids (fresh allocations
    // intentionally get different cache identities).
    let tally = calls();
    let chain = |seed: f64| {
        let first = counted(&snapshot(&fixture()), &tally, &["P"], &["P"], shifted(seed));
        counted(&first, &tally, &["P"], &["P"], shifted(seed + 1.0))
    };
    let real = join("merge", &chain(1.0), &chain(3.0));
    let (mut one, mut eight) = (sess(64), sess(64));
    session::set_parallel_override(&mut eight, Some(true));
    assert_eq!(
        bytes_of(&cook_at(&mut one, 1, &real)),
        bytes_of(&cook_at(&mut eight, 8, &real)),
        "allocating two-chain geometry differs at eight domains"
    );
    assert_eq!(counters(&one), counters(&eight), "two-chain cache accounting differs");
    assert_eq!(session::private::fanouts(&eight), 1);
    session::close(&mut one);
    session::close(&mut eight);
}

#[test]
fn shared_packed_materialization_replays_exactly() {
    let geometry = geometry_fixture();
    let transforms = Arc::new(vec![
        rays_math::mat4::identity(),
        rays_math::mat4::translation(rays_math::vec3::create(2.0, 0.0, 0.0)),
    ]);
    let packed_source = {
        let geometry = geometry.clone();
        build::make_geometry(
            build::Args { cook_mode: CookMode::Generator, ..build::args("packed_source") },
            None,
            Arc::new(move |_, _, _| {
                Ok(node::GeometryCooked { geometry: geometry.clone(), instances: Some(Arc::clone(&transforms)) })
            }),
        )
    };
    let consume = |label: &str| {
        build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                parameters: label.as_bytes().to_vec(),
                cook_mode: CookMode::Generic,
                inputs: vec![packed_source.clone()],
                ..build::args("consume_packed")
            },
            None,
            Arc::new(|_, _, inputs| Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: None })),
        )
    };
    let graph = join("merge", &consume("packed_a"), &consume("packed_b"));
    let (mut one, mut eight) = (sess(64), sess(64));
    session::set_parallel_override(&mut eight, Some(true));
    assert_eq!(
        bytes_of(&cook_at(&mut one, 1, &graph)),
        bytes_of(&cook_at(&mut eight, 8, &graph)),
        "shared packed materialization changed branch bytes"
    );
    assert_eq!(counters(&one), counters(&eight), "materialization replay changed cache accounting");
    session::close(&mut one);
    session::close(&mut eight);
    exact(0, &no_prepare, &graph);
}

#[test]
fn zone_roots_fork_and_stay_exact() {
    let points = rdk::line_geometry::points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0), (3.0, 0.0, 0.0)]).unwrap();
    let common = snapshot(&fixture());
    let tally = calls();
    let body: zone::Body = Arc::new(move |inputs, _| {
        let common = inputs[1].clone();
        let tally = Arc::clone(&tally);
        Arc::new(move |element: &zone::Element| counted(&common, &tally, &["P"], &["P"], shifted(element.index as f64)))
    });
    let graph = zone::node(zone::args(zone::Kind::Points, "source", 0), body, vec![snapshot(&points), common]);
    let (mut one, mut eight) = (sess(64), sess(64));
    session::set_parallel_override(&mut eight, Some(true));
    assert_eq!(
        bytes_of(&cook_at(&mut one, 1, &graph)),
        bytes_of(&cook_at(&mut eight, 8, &graph)),
        "zone geometry differs at eight domains"
    );
    assert_eq!(counters(&one), counters(&eight), "zone cache accounting differs");
    assert!(session::private::fanouts(&eight) > 0, "expanded zone roots were not forked");
    session::close(&mut one);
    session::close(&mut eight);
}

/// The session-level core of lib/sop/test_parallel_exact.ml: one graph of real native SOPs (clip,
/// soft transform) cooked at 1 and 4 domains is equal, plane for plane.
#[test]
fn native_sops_are_equal_at_one_and_many_domains() {
    use crate::edit_graph;
    use crate::shapes;
    let source = snapshot(&fixture());
    let clip = edit_graph::instantiate(&shapes::clip::factory(), &[source.clone()]).unwrap();
    let soft = {
        let n = edit_graph::instantiate(&shapes::soft_transform::factory(), &[source]).unwrap();
        node::apply_parameters(&n, &[("translate_y".to_owned(), param::Value::Float(0.5))]).unwrap().0
    };
    let graph = join("native_join", &clip, &soft);
    let cook_once = |domains: i64, entries: i64| {
        let mut s = sess(entries);
        let out = bytes_of(&cook_at(&mut s, domains, &graph));
        session::close(&mut s);
        out
    };
    let reference = cook_once(1, 16);
    assert_eq!(cook_once(4, 16), reference);
    assert_eq!(cook_once(4, 0), reference);
    // fresh allocations get different cache identities per run, so compare bytes and counters only
    let (mut one, mut eight) = (sess(16), sess(16));
    session::set_parallel_override(&mut eight, Some(true));
    assert_eq!(bytes_of(&cook_at(&mut one, 1, &graph)), reference);
    assert_eq!(bytes_of(&cook_at(&mut eight, 8, &graph)), reference);
    assert_eq!(counters(&one), counters(&eight));
    assert_eq!(session::private::fanouts(&eight), 1);
    session::close(&mut one);
    session::close(&mut eight);
}
