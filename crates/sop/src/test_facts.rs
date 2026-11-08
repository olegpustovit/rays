//! Port of lib/sop/test_facts.ml (the parts that do not drive Lisp nodes): component cache facts,
//! refusals, consumer facts, payload budget and shared plane charge.
//! Not ported: test_reuse, test_parameter_reads, test_normals_cache_refresh (they cook Lisp
//! `sop/*` nodes) and test_chain_peak (measures the OCaml GC).
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

use rdk_core::{attribute, edge_group, geometry, group, packed, topology_index};

use crate::context;
use crate::node::{self, build, CookMode, InputUse};
use crate::session;
use crate::test_support::*;

fn check(ok: bool, message: &str) {
    assert!(ok, "{message}");
}

#[test]
fn dependencies() {
    let geometry_value = with_color(1.0, &fixture());
    let calls = calls();
    let n = counted(&snapshot(&geometry_value), &calls, &["P", "N"], &[], identity());
    let mut evaluator = session(64);
    cook(&mut evaluator, 1, &n);
    cook(&mut evaluator, 1, &rewire(&n, &with_color(2.0, &geometry_value)));
    check(count(&calls) == 1, "unread color changed a component key");
    let points = geometry::point_count(&geometry_value) as usize;
    let normals = packed::Float3::of_owned(vec![1.0; points], vec![0.0; points], vec![0.0; points]).unwrap();
    let normal = attribute::create_key_owned(
        &attribute::normal(attribute::Owner::Point),
        attribute::Storage::Float3(normals.clone()),
    )
    .unwrap();
    let changed_n = geometry::with_attribute(normal, &geometry_value).unwrap();
    cook(&mut evaluator, 1, &rewire(&n, &changed_n));
    check(count(&calls) == 2, "read N did not invalidate");
    let changed_p = geometry::with_positions(normals, &changed_n).unwrap();
    cook(&mut evaluator, 1, &rewire(&n, &changed_p));
    check(count(&calls) == 3, "read P did not invalidate");
    let fresh_topology = geometry::topology(&fixture()).clone();
    let changed_topology = geometry::create(
        geometry::positions(&changed_p).clone(),
        fresh_topology.clone(),
        Some(geometry::attributes(&changed_p)),
        None,
        None,
    )
    .unwrap();
    cook(&mut evaluator, 1, &rewire(&n, &changed_topology));
    check(count(&calls) == 4, "topology did not invalidate");
    let selection =
        group::init(None, group::Owner::Point, "selection", geometry::point_count(&geometry_value), &|_| true).unwrap();
    let changed_group = geometry::with_group(selection, &changed_topology).unwrap();
    cook(&mut evaluator, 1, &rewire(&n, &changed_group));
    check(count(&calls) == 5, "group did not invalidate");
    let index = topology_index::create(None, &fresh_topology).unwrap();
    let edges = edge_group::init(None, &fresh_topology, &index, "edges", &|_| true).unwrap();
    cook(&mut evaluator, 1, &rewire(&n, &geometry::with_edge_group(edges, &changed_group).unwrap()));
    check(count(&calls) == 6, "edge group did not invalidate");
    let opaque_calls = Arc::clone(&calls);
    let opaque = build::make_geometry(
        build::Args {
            cook_mode: CookMode::DuplicateInput(0),
            inputs: vec![snapshot(&geometry_value)],
            ..build::args("opaque")
        },
        None,
        Arc::new(move |_, _, inputs| {
            opaque_calls.fetch_add(1, Ordering::SeqCst);
            Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: None })
        }),
    );
    let facts_of = node::facts(&opaque);
    check(
        facts_of.elementwise == node::Elementwise::None && facts_of.topology == node::Topology::Changed && facts_of.exact,
        "opaque declaration is not conservative",
    );
    cook(&mut evaluator, 1, &opaque);
    cook(&mut evaluator, 1, &rewire(&opaque, &with_color(3.0, &geometry_value)));
    check(count(&calls) == 8, "opaque node used a component key");
    let identity_calls = self::calls();
    let identity_node = counted(&snapshot(&geometry_value), &identity_calls, &[], &[], identity());
    cook(&mut evaluator, 1, &identity_node);
    let changed = with_color(4.0, &changed_p);
    let output = cook(&mut evaluator, 1, &rewire(&identity_node, &changed));
    let got = geometry_of(&output);
    check(
        count(&identity_calls) == 1
            && packed::Float3::data_id(geometry::positions(&got)) == packed::Float3::data_id(geometry::positions(&changed))
            && same_geometry(&got, &changed),
        "read-free cache hit did not carry fresh P/attributes",
    );
    session::close(&mut evaluator);
}

#[test]
fn write_footprint() {
    let geometry_value = fixture();
    let remove: Transform = Arc::new(|g| geometry::without_attribute(attribute::Owner::Point, "N", g));
    let absent = geometry::without_attribute(attribute::Owner::Point, "N", &geometry_value);
    let calls = calls();
    let n = counted(&snapshot(&absent), &calls, &["P"], &["N"], remove);
    let mut evaluator = session(64);
    cook(&mut evaluator, 1, &n);
    let output = cook(&mut evaluator, 1, &rewire(&n, &geometry_value));
    check(
        count(&calls) == 2 && geometry::find_attribute(attribute::Owner::Point, "N", &geometry_of(&output)).is_none(),
        "adding an unread written attribute reused an incomplete delta",
    );
    let color4: Transform = Arc::new(|g| with_color(4.0, g));
    let cd = counted(&snapshot(&with_color(1.0, &geometry_value)), &calls, &["P"], &["Cd"], color4);
    let first = cook(&mut evaluator, 1, &cd);
    let changed = geometry::with_attribute(
        scalar(attribute::Owner::Primitive, "Cd", geometry::primitive_count(&geometry_value), 3.0),
        &with_color(2.0, &geometry_value),
    )
    .unwrap();
    let second = cook(&mut evaluator, 1, &rewire(&cd, &changed));
    let id_of = |g: &geometry::T, owner| geometry::find_attribute(owner, "Cd", g).map(|a| attribute::data_id(&a));
    check(
        id_of(&geometry_of(&second), attribute::Owner::Primitive) == id_of(&changed, attribute::Owner::Primitive),
        "writer discarded a different owner's attribute",
    );
    let floats = |g: &geometry::T| match geometry::find_attribute(attribute::Owner::Point, "Cd", g).map(|a| attribute::storage(&a).unwrap()) {
        Some(attribute::Storage::Float(values)) => Some(values),
        _ => None,
    };
    check(
        floats(&geometry_of(&first)) == floats(&geometry_of(&second)) && floats(&geometry_of(&first)).is_some(),
        "writer did not preserve its computed attribute",
    );
    session::close(&mut evaluator);
}

fn refusals(entries: i64) {
    let geometry_value = fixture();
    let node_with = |transform: Transform| counted(&snapshot(&geometry_value), &self::calls(), &["P"], &[], transform);
    let mut evaluator = session(entries);
    let mut rejects = |n: &node::T| match session::cook(&mut evaluator, None, &context(1), n) {
        Err(error) => check(
            error.code == "E_NODE_FACTS" && error.trace.iter().any(|t| t.node_id == node::id(n)),
            "incorrect facts did not return a traced E_NODE_FACTS",
        ),
        Ok(_) => panic!("incorrect facts were accepted"),
    };
    rejects(&node_with(Arc::new(|g| {
        geometry::with_attribute(scalar(attribute::Owner::Point, "undeclared", geometry::point_count(g), 1.0), g).unwrap()
    })));
    rejects(&node_with(Arc::new(|g| geometry::without_attribute(attribute::Owner::Point, "N", g))));
    rejects(&node_with(Arc::new(|_| fixture())));
    rejects(&node_with(Arc::new(|g| {
        let added = group::init(None, group::Owner::Point, "added", geometry::point_count(g), &|_| true).unwrap();
        geometry::with_group(added, g).unwrap()
    })));
    let packed_node = build::make_geometry(
        build::Args {
            cook_mode: CookMode::DuplicateInput(0),
            inputs: vec![snapshot(&geometry_value)],
            ..build::args("packed")
        },
        None,
        Arc::new(|_, _, inputs| {
            Ok(node::GeometryCooked {
                geometry: inputs[0].clone(),
                instances: Some(Arc::new(vec![rays_math::mat4::identity()])),
            })
        }),
    );
    rejects(&facts(&packed_node, &["P"], &[]));
    session::close(&mut evaluator);
}

#[test]
fn refusals_cached() {
    refusals(64);
}

#[test]
fn refusals_uncached() {
    refusals(0);
}

#[test]
fn payload_budget() {
    let geometry_value = with_color(1.0, &fixture());
    let translate: Transform = Arc::new(|g| {
        let view = packed::Float3::View::view(geometry::positions(g)).unwrap();
        let moved = packed::Float3::of_owned(view.x.iter().map(|x| x + 1.0).collect(), view.y.to_vec(), view.z.to_vec()).unwrap();
        geometry::with_positions(moved, g).unwrap()
    });
    let n = counted(&snapshot(&geometry_value), &calls(), &["P"], &["P"], translate);
    let mut evaluator = session(2);
    let output = cook(&mut evaluator, 1, &n);
    let fresh = with_color(2.0, &geometry_value);
    let next = build::rebuild_with_inputs(&n, &[snapshot(&fresh)]);
    let updated = cook(&mut evaluator, 1, &next);
    // the P-writer reads P only: its moved positions are reused for the recoloured input
    check(
        packed::Float3::data_id(geometry::positions(&geometry_of(&updated)))
            == packed::Float3::data_id(geometry::positions(&geometry_of(&output))),
        "budget test missed reuse",
    );
    let mut components = std::collections::HashMap::new();
    for g in [fresh, geometry_of(&updated)] {
        for (id, bytes) in geometry::payload_components(&g) {
            components.insert(id, bytes);
        }
    }
    let bytes: i64 = components.values().sum();
    check(
        session::stats(&evaluator).retained_payload_bytes == bytes,
        "refresh retained the previous unread payload or lost reference counts",
    );
    session::close(&mut evaluator);
}

fn seen_list() -> Arc<Mutex<Vec<Option<i64>>>> {
    Arc::new(Mutex::new(Vec::new()))
}

fn consumer_source(seen: &Arc<Mutex<Vec<Option<i64>>>>) -> node::T {
    let geometry_value = rdk_ops::line_geometry::points(&[]).unwrap();
    let seen = Arc::clone(seen);
    build::make_geometry(
        build::Args { cook_mode: CookMode::Generator, ..build::args("consumer_source") },
        None,
        Arc::new(move |node_id, context, _| {
            seen.lock().unwrap().push(context::cpu_consumers(context, node_id));
            Ok(node::GeometryCooked { geometry: geometry_value.clone(), instances: None })
        }),
    )
}

fn use_node(kind: InputUse, policy: Option<node::InputPolicy>, inputs: Vec<node::T>) -> node::T {
    let made = build::make_geometry(
        build::Args { input_policy: policy, inputs, ..build::args("consumer_use") },
        None,
        Arc::new(move |_, _, inputs| {
            if kind == InputUse::Cpu {
                for g in inputs {
                    packed::Float3::View::view(geometry::positions(g)).unwrap();
                }
            }
            Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: None })
        }),
    );
    build::with_input_use(kind, &made)
}

fn run(cpu_consumers: Option<i64>, ctx: Option<context::T>, root: &node::T) {
    let mut e = session(0);
    let ctx = ctx.unwrap_or_else(|| context::create(context::Options::default()).unwrap());
    session::cook(&mut e, cpu_consumers, &ctx, root).unwrap();
    session::close(&mut e);
}

#[test]
fn consumer_facts() {
    let seen = seen_list();
    let s = consumer_source(&seen);
    let a = use_node(InputUse::Forward(0), None, vec![s.clone()]);
    let b = use_node(InputUse::Forward(0), None, vec![s.clone()]);
    run(None, None, &use_node(InputUse::Cpu, None, vec![a, b]));
    check(*seen.lock().unwrap() == vec![Some(1)], "forwarding diamond double-counted one CPU reader");
    let a = use_node(InputUse::Cpu, None, vec![s.clone()]);
    let b = use_node(InputUse::Cpu, None, vec![s.clone()]);
    for domains in [1, 4] {
        seen.lock().unwrap().clear();
        run(None, Some(context(domains)), &use_node(InputUse::Unknown, None, vec![a.clone(), b.clone()]));
        check(*seen.lock().unwrap() == vec![Some(2)], "shared source lost CPU consumers");
    }
    seen.lock().unwrap().clear();
    run(None, None, &use_node(InputUse::Unknown, None, vec![a, s.clone()]));
    check(*seen.lock().unwrap() == vec![None], "opaque use became a known resident consumer");
    let mut e = session(0);
    let ctx = context(1);
    let root = use_node(InputUse::Forward(0), None, vec![s.clone()]);
    seen.lock().unwrap().clear();
    session::cook(&mut e, Some(0), &ctx, &root).unwrap();
    session::cook(&mut e, Some(0), &ctx, &root).unwrap();
    check(*seen.lock().unwrap() == vec![Some(0), Some(0)], "unchanged root rescanned consumers or lost the resident sink");
    let scans = |e: &session::T| session::stats(e).consumer_scans;
    check(scans(&e) == 1, "unchanged root rescanned consumers");
    session::cook(&mut e, Some(1), &ctx, &root).unwrap();
    check(seen.lock().unwrap().last() == Some(&Some(1)) && scans(&e) == 2, "changed external consumer reused stale facts");
    session::cook(&mut e, Some(0), &context::with_cpu_consumers(&ctx, node::id(&s), Some(2)), &root).unwrap();
    check(seen.lock().unwrap().last() == Some(&Some(2)), "nested cook forgot enclosing consumers");
    session::cook(&mut e, Some(1), &context::with_cpu_consumers(&ctx, node::id(&s), None), &root).unwrap();
    check(seen.lock().unwrap().last() == Some(&None), "nested cook erased an enclosing unknown use");
    check(
        matches!(session::cook(&mut e, Some(-1), &ctx, &root), Err(d) if d.code == "consumer_count"),
        "negative root consumer count was accepted",
    );
    session::close(&mut e);
    seen.lock().unwrap().clear();
    let alias = node::relabel("same id, another node", &s);
    run(
        None,
        None,
        &use_node(
            InputUse::Unknown,
            None,
            vec![use_node(InputUse::Cpu, None, vec![s.clone()]), use_node(InputUse::Cpu, None, vec![alias])],
        ),
    );
    check(*seen.lock().unwrap() == vec![None, None], "reused node ids received ambiguous consumer facts");
    seen.lock().unwrap().clear();
    let skipped = seen_list();
    run(
        None,
        None,
        &use_node(InputUse::Cpu, Some(node::InputPolicy::Only(1)), vec![consumer_source(&skipped), s.clone()]),
    );
    check(skipped.lock().unwrap().is_empty() && *seen.lock().unwrap() == vec![Some(1)], "consumer census visited an unselected input");
}

/// a plane two volatile slots share is on the ledger once, and leaves when the last one goes
#[test]
fn shared_plane_charge() {
    let owner = lru::cache("sop.volatile");
    let before = mem::bytes(&owner);
    let shared = rdk_ops::line_geometry::points(&vec![(1.0, 2.0, 3.0); 1000]).unwrap();
    let holder = |name: &str| {
        let g = shared.clone();
        build::make_geometry(
            build::Args { ..build::args(name) },
            None,
            Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: g.clone(), instances: None })),
        )
    };
    let a = holder("volatile_a");
    let b = holder("volatile_b");
    let mut e = session(64);
    let (ida, idb) = (node::id(&a), node::id(&b));
    session::set_volatile(&mut e, Arc::new(move |id| id == ida || id == idb));
    cook(&mut e, 1, &a);
    let once = mem::bytes(&owner) - before;
    cook(&mut e, 1, &b);
    assert!(once > 0 && mem::bytes(&owner) - before == once, "shared plane: {once} once, {} with two slots", mem::bytes(&owner) - before);
    session::close(&mut e);
    assert_eq!(mem::bytes(&owner), before, "closing left a slot's charge on the ledger");
}

/// OCaml `Session.node_result`: an exception in a node's cook is that node's error, not the end of
/// the cook; the session cooks again afterwards.
#[test]
fn a_panicking_node_is_a_diagnostic_on_that_node() {
    let panicking = build::make_geometry(
        build::Args { label: Some("boom".to_owned()), cook_mode: CookMode::Generator, dependencies: crate::context::dependencies::STATIC, ..build::args("panic_test") },
        None,
        Arc::new(|_, _, _| -> Result<node::GeometryCooked, rays_diagnostic::Diagnostic> { panic!("kernel exploded") }),
    );
    let mut evaluator = session(64);
    let error = match session::cook(&mut evaluator, None, &context(1), &panicking) {
        Ok(_) => panic!("a panicking cook produced an output"),
        Err(error) => error,
    };
    assert_eq!(error.code, "uncaught_node_exception");
    assert_eq!(error.message, "a procedural node raised an exception while cooking");
    assert_eq!(error.cause.as_deref(), Some(b"kernel exploded".as_slice()));
    // the session survives: a good node cooks, and the same bad node fails the same way again
    let good = snapshot(&fixture());
    cook(&mut evaluator, 1, &good);
    assert!(session::cook(&mut evaluator, None, &context(1), &panicking).is_err());
}
