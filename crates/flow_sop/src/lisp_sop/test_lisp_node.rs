//! Port of lib/flow_sop/test_lisp_node.ml: the Lisp node's own protocol (its factory node, as the
//! inspector and node menu build it: parameters, rebuilds, cook identity).
use super::support::*;
use crate::{lisp_sop, lower, operators};
use rdk_core::geometry as geo;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node::{self, build, CookMode};
use sop::session;
use std::sync::Arc;

fn lower_ref(reference: bool, text: &str) -> node::T {
    let l = lower::workspace(None, Vec::new(), lower::Options { reference, ..lower::Options::default() }, &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = l.graphs.iter().find(|g| g.default).expect("a default graph").clone();
    lisp_sop::root(&l, &graph).expect("graph root")
}

/// 128 entries: a test cooks many graphs in one session, and their working set must fit the cache
/// for a recook to hit.
fn with_session(f: &mut dyn FnMut(&mut session::T)) {
    let mut s = session::create(128, 10_000_000).unwrap();
    f(&mut s);
    session::close(&mut s);
}

fn cook_with(s: &mut session::T, domains: i64, grain: i64, seed: i64, n: &node::T) -> geo::T {
    let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), seed: Some(seed), ..Default::default() }).unwrap();
    cook(s, &context, n)
}

fn line_points(points: &[(f64, f64, f64)]) -> geo::T {
    rdk_ops::line_geometry::points(points).unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)))
}

fn external(name: &str, geometry: geo::T) -> Vec<(String, node::T)> {
    vec![(name.to_owned(), lisp_sop::snapshot(&geometry))]
}

#[test]
fn lazy_cooked_selection_keeps_a_chained_write() {
    let source = r#"(workspace select
    (defnode sop/select_write [(geo : geometry)]
      (if (= (sop/count geo :points) 1)
        (let* [first (sop/with_attr geo "stage" (array/int 1 7) :type int)]
          (sop/with_attr first "seen" (array/int 1 (sop/count first :points)) :owner detail :type int))
        (error "E_UNTAKEN" "Untaken cooked branch")))
    (graph g :context sop (sop/select_write (sop/ext_src))))"#;
    let geometry = line_points(&[(0.0, 0.0, 0.0)]);
    let compile = |reference: bool| -> node::T {
        let snapshot = lisp_sop::snapshot(&geometry);
        let factory = edit::factory(edit::factory_args("ext_src", "Source", &["Test"]), 0, Arc::new(move |_| Ok(snapshot.clone())));
        let mut factories = vec![factory];
        factories.extend(sop::nodes::factories());
        let l = lower::workspace(Some(factories), Vec::new(), lower::Options { reference, ..lower::Options::default() }, &parse(source)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        lisp_sop::root(&l, &l.graphs[0]).unwrap()
    };
    let mut expected = None;
    with_session(&mut |s| expected = Some(cook_with(s, 1, 97, 0, &compile(false))));
    let expected = geometry_bytes(&expected.unwrap());
    for reference in [false, true] {
        for domains in [1i64, 8] {
            with_session(&mut |s| {
                let actual = cook_with(s, domains, 97, 0, &compile(reference));
                assert!(geometry_bytes(&actual) == expected, "lazy cooked selection disconnected a chained write or changed domain bytes");
                for (owner, name, value) in [(rdk_core::attribute::Owner::Point, "stage", 7i64), (rdk_core::attribute::Owner::Detail, "seen", 1)] {
                    let a = geo::find_attribute(owner, name, &actual).expect("chained cooked write lost an attribute");
                    match rdk_core::attribute::storage(&a) {
                        Ok(rdk_core::attribute::Storage::Int(values)) => assert!(*values == vec![value], "chained cooked write has wrong values"),
                        _ => panic!("chained cooked write has wrong type"),
                    }
                }
            });
        }
    }
}

const SHIFT: &str = r#"(workspace test
  (defnode sop/lisp_shift :label "Lisp shift" :category ["Deform"]
    :facts {:elementwise points :reads ["P"] :writes ["P"] :topology preserved :exact true}
    [(geo : geometry) (distance : float 0.5 :min -10 :max 10 :label "Distance")]
    (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance 0.0 0.0]))
      (sop/attr geo :P)))))
  (graph g :context sop (sop/lisp_shift (sop/ext_src) :distance 2.0)))"#;

#[test]
fn empty_geometry_survives_a_lisp_map() {
    let empty = line_points(&[]);
    let node = lisp_sop::workspace_node(&external("src", empty.clone()), SHIFT);
    with_session(&mut |s| {
        assert!(geometry_bytes(&cook_with(s, 1, 97, 0, &node)) == geometry_bytes(&empty), "empty geometry changed through a Lisp map");
    });
}

#[test]
fn authored_body_errors_cook_lazily() {
    let source = |body: &str| format!("(workspace body_error\n    (defnode sop/body_error [(geo : geometry) (bad : bool true)] {body})\n    (graph g :context sop (sop/body_error (sop/line :points 2))))");
    let expect_error = |s: &mut session::T, domains: i64, n: &node::T| {
        let context = sctx::create(ContextOptions { domains: Some(domains), ..Default::default() }).unwrap();
        match session::cook(s, None, &context, n) {
            Err(d) => assert!(d.code == "E_BODY" && d.message == "body failure" && d.cause.as_deref() == Some(b"authored cause"), "authored body error changed during cooking: {}", rays_diagnostic::error_to_string(&d)),
            Ok(_) => panic!("authored body error succeeded"),
        }
    };
    for reference in [false, true] {
        for body in [
            "(if bad (error \"E_BODY\" \"body failure\" :cause \"authored cause\") geo)",
            "(let* [ignored (error \"E_BODY\" \"body failure\" :cause \"authored cause\")] geo)",
        ] {
            let n = lower_ref(reference, &source(body));
            with_session(&mut |s| {
                for domains in [1i64, 8] {
                    expect_error(s, domains, &n);
                }
            });
            let (healthy, _) = node::apply_parameters(&n, &[("bad".to_owned(), param::Value::Bool(false))]).unwrap();
            if body.starts_with("(if") {
                with_session(&mut |s| {
                    cook_with(s, 1, 97, 0, &healthy);
                });
                let (broken, _) = node::apply_parameters(&healthy, &[("bad".to_owned(), param::Value::Bool(true))]).unwrap();
                with_session(&mut |s| expect_error(s, 1, &broken));
            }
        }
    }
    let healthy = lower_ref(false, &source("(if false (error \"E_BODY\" \"body failure\") geo)"));
    with_session(&mut |s| {
        cook_with(s, 1, 97, 0, &healthy);
    });
    let seed = operators::seed_for(0, 0, "body_error", "body_error");
    let seeded = lower_ref(false, &source(&format!("(if (int/eq (sop/context_seed \"body_error\") {seed}) (error \"E_BODY\" \"body failure\" :cause \"authored cause\") geo)")));
    assert!(sop::context::dependencies::mem(sop::context::dependencies::Fact::Seed, node::dependencies(&seeded)), "deferred body error lost its context seed dependency");
    with_session(&mut |s| {
        expect_error(s, 1, &seeded);
        cook_with(s, 1, 97, 123456789, &seeded);
        expect_error(s, 1, &seeded);
    });
    let salt: i64 = -1582480459898762999;
    let salted_seed = operators::seed_for(salt, 0, "body_error", "body_error");
    assert!(salted_seed != seed && operators::seed_for(0, 0, "body_error", "body_error") == seed, "context seed salt did not change the mix or changed the unsalted one");
    // the same salt as native's 0x6a09e667f3bcc909 xor: its low 63 bits
    assert_eq!(salt & i64::MAX, 0x6a09e667f3bcc909, "salt literal is not native's constant");
    let salted_call = lower_ref(false, &source(&format!("(if (int/eq (sop/context_seed \"body_error\" :salt {salt}) {salted_seed}) (error \"E_BODY\" \"body failure\" :cause \"authored cause\") geo)")));
    with_session(&mut |s| expect_error(s, 1, &salted_call));
}

fn prepared_bodies_reuse_unchanged_graphs_and_recompile_live_drives(expression: &str) {
    {
        for reference in [false, true] {
            let n = lower_ref(
                reference,
                &format!("(workspace live_body\n      (defnode sop/live_body [(geo : geometry)]\n        (sop/transform geo :translate [{expression} 0.0 0.0]))\n      (graph g :context sop (sop/live_body (sop/line :points 2))))"),
            );
            with_session(&mut |s| {
                for (time, dt) in [(0.0f64, 0.25f64), (1.0, 0.5), (2.0, 0.125)] {
                    let live = input::Frame { dt, frame: time as i64, tick: time as i64, ..input::at_time(time) };
                    let context = sctx::create(ContextOptions { time: Some(time), frame: Some(live.frame), input: Some(live), ..Default::default() }).unwrap();
                    let actual = cook(s, &context, &n);
                    let offset = if expression == "t" { time } else { dt };
                    let expected_node = lisp_sop::node(&[], &format!("(sop/transform (sop/line :points 2) :translate [{} 0.0 0.0])", lisp_sop::float(offset)));
                    let expected = cook_with(s, 1, 97, 0, &expected_node);
                    assert!(geometry_bytes(&actual) == geometry_bytes(&expected), "Prepared body reused stale geometry for {expression} at t={time} dt={dt}");
                    let before = session::stats(s);
                    cook(s, &context, &n);
                    assert!(session::stats(s).misses == before.misses, "An unchanged prepared body missed its warm cache");
                }
            });
        }
    }
}

/// The reuse half of the above, cheap: a live body is recompiled for a new time (its geometry moves,
/// it is not the stale one) and an unchanged one hits the warm cache.
#[test]
fn a_live_body_recompiles_for_a_new_time_and_an_unchanged_one_hits_the_cache() {
    let n = lower_ref(false, "(workspace live_body
      (defnode sop/live_body [(geo : geometry)]
        (sop/transform geo :translate [t 0.0 0.0]))
      (graph g :context sop (sop/live_body (sop/line :points 2))))");
    with_session(&mut |s| {
        let at = |time: f64| sctx::create(ContextOptions { time: Some(time), frame: Some(time as i64), input: Some(input::Frame { frame: time as i64, tick: time as i64, ..input::at_time(time) }), ..Default::default() }).unwrap();
        let (first, second) = (at(0.0), at(1.0));
        let a = geometry_bytes(&cook(s, &first, &n));
        let b = geometry_bytes(&cook(s, &second, &n));
        assert!(a != b, "A live body reused the geometry of another time");
        let before = session::stats(s);
        assert!(geometry_bytes(&cook(s, &second, &n)) == b);
        assert!(session::stats(s).misses == before.misses, "An unchanged prepared body missed its warm cache");
    });
}

#[test]

fn prepared_bodies_reuse_unchanged_graphs_and_recompile_a_live_time() {
    prepared_bodies_reuse_unchanged_graphs_and_recompile_live_drives("t");
}

#[test]

fn prepared_bodies_reuse_unchanged_graphs_and_recompile_a_live_state() {
    prepared_bodies_reuse_unchanged_graphs_and_recompile_live_drives("(state [previous 0.0] (+ previous (frame/dt)))");
}

#[allow(dead_code)]
fn _unused() -> (CookMode, fn(&node::T) -> node::T) {
    (CookMode::Generator, |n| build::with_facts(node::facts(n).clone(), n))
}

// ---- sections 5 onward ----

use rays_math::{mat4, vec3};
use std::sync::atomic::{AtomicI64, Ordering};

/// OCaml `==` on cooked geometries: the same payload.
fn same(a: &geo::T, b: &geo::T) -> bool {
    geo::data_id(a) == geo::data_id(b)
}

fn rdk_ok<A>(r: Result<A, rdk_core::error::T>) -> A {
    r.unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)))
}

fn position(g: &geo::T, i: i64) -> (f64, f64, f64) {
    rdk_core::packed::float3::get(geo::positions(g), i).unwrap()
}

fn field_names(n: &node::T) -> Vec<String> {
    node::parameter_fields(n).iter().map(|f| f.name.clone()).collect()
}

fn point_count_of(s: &mut session::T, n: &node::T) -> i64 {
    geo::point_count(&cook_with(s, 1, 97, 0, n))
}

fn bytes_of(s: &mut session::T, n: &node::T) -> Vec<u8> {
    geometry_bytes(&cook_with(s, 1, 97, 0, n))
}


#[test]
fn materialized_writes_own_storage_expand_instances_and_report_errors() {
    let original = line_points(&[(0.0, -0.0, 2.0), (1.0, 3.0, 4.0)]);
    let before = geometry_bytes(&original);
    let transforms = vec![mat4::translation(vec3::create(1.0, 0.0, 0.0)), mat4::translation(vec3::create(3.0, 0.0, 0.0))];
    let shared = Arc::new(transforms.clone());
    let source = original.clone();
    let input = build::make_geometry(
        build::Args { cook_mode: CookMode::Generator, ..build::args("test.materialized_input") },
        None,
        Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: source.clone(), instances: Some(shared.clone()) })),
    );
    let body = r#"(let* [data (exact (map (fn [p] (+ p [0.5 -0.0 0.0])) (sop/attr geo :P)))
                      copied (sop/with_attr geo :copied data)
                      rows {:offsets (array/range (int/add (sop/count copied :points) 1) :type int)
                            :values (array/float (sop/count copied :points) 0.25)}]
                 (sop/with_attr copied :rows rows :type float_array))"#;
    let node_of = |body: &str| {
        lisp_sop::workspace_node(
            &[("src".to_owned(), input.clone())],
            &format!("(workspace direct_write (defnode sop/direct_write [(geo : geometry)] (if (int/gt (sop/count geo :points) 0) {body} geo)) (graph g :context sop (sop/direct_write (sop/ext_src))))"),
        )
    };
    let actual = node_of(body);
    let expected = rdk_ok(rdk_ops::instance_copy::materialize_instances(&transforms, &original));
    with_session(&mut |s| {
        for domains in [1i64, 8] {
            session::clear(s);
            let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(97), ..Default::default() }).unwrap();
            let output = session::cook(s, None, &context, &actual).unwrap();
            let geometry = sop::payload::geometry(&output.payload).unwrap();
            assert!(output.instances.is_none(), "Materialized write retained already-expanded instances");
            assert!(geo::point_count(&geometry) == geo::point_count(&expected), "Materialized write lost source instance cardinality");
            let storage = |name: &str| rdk_core::attribute::storage(&geo::find_attribute(rdk_core::attribute::Owner::Point, name, &geometry).unwrap()).unwrap();
            match storage("copied") {
                rdk_core::attribute::Storage::Float3(data) => {
                    let data_x = rdk_ok(rdk_core::packed::float3::view::view(&data)).x;
                    let position_x = rdk_ok(rdk_core::packed::float3::view::view(geo::positions(&geometry))).x;
                    assert!(!Arc::ptr_eq(&data_x, &position_x), "Computed write borrowed its input storage");
                    for i in 0..geo::point_count(&geometry) {
                        let (x, y, z) = position(&expected, i);
                        assert!(rdk_core::packed::float3::get(&data, i).unwrap() == (x + 0.5, y + -0.0, z + 0.0), "Computed write changed numeric values");
                    }
                }
                _ => panic!("Materialized write lost tuple storage"),
            }
            match storage("rows") {
                rdk_core::attribute::Storage::FloatArray(data) => {
                    let v = rdk_core::packed::float_array::view::view(&data);
                    assert!(*v.offsets == [0i64, 1, 2, 3, 4] && *v.values == [0.25f64; 4], "Materialized CSR write changed row layout");
                }
                _ => panic!("Materialized write lost CSR storage"),
            }
            for (body, code) in [
                ("(sop/with_attr geo :bad (array/float (int/add (sop/count geo :points) 1) 0.5) :type float)", "E_ATTR_COUNT"),
                ("(sop/with_attr geo :bad (array/float (int/add (sop/count geo :points) 1) (exp 1000.0)) :type float)", "E_NONFINITE"),
            ] {
                match session::cook(s, None, &context, &node_of(body)) {
                    Err(error) => assert!(error.code == code, "Materialized write changed producer/count error precedence"),
                    Ok(_) => panic!("Malformed materialized write succeeded"),
                }
            }
            let token = Arc::new(rdk_core::cancel::create());
            rdk_core::cancel::cancel(&token);
            let cancelled = sctx::create(ContextOptions { domains: Some(domains), cancel: Some(token), ..Default::default() }).unwrap();
            match session::cook(s, None, &cancelled, &actual) {
                Err(error) => assert!(error.code == "cancelled" || error.code == "E_CANCELLED", "Materialized write lost cancellation"),
                Ok(_) => panic!("Cancelled materialized write succeeded"),
            }
        }
    });
    assert!(before == geometry_bytes(&original), "Materialized writes mutated their input");
}

#[test]
fn a_selected_cooked_error_keeps_its_diagnostic() {
    let n = lower_ref(
        false,
        r#"(workspace selected_error
    (defnode sop/selected_error [(geo : geometry)]
      (if (= (sop/count geo :points) 1)
        (error "E_SELECTED" "selected failure" :cause (str "point count " (sop/count geo :points)))
        geo))
    (graph g :context sop (sop/selected_error (sop/line :kind "Points" :points 1))))"#,
    );
    with_session(&mut |s| {
        for domains in [1i64, 8] {
            let context = sctx::create(ContextOptions { domains: Some(domains), ..Default::default() }).unwrap();
            match session::cook(s, None, &context, &n) {
                Err(d) => assert!(d.code == "E_SELECTED" && d.message == "selected failure" && d.cause.as_deref() == Some(b"point count 1"), "compiled cooked selection diagnostic differs: {}", rays_diagnostic::error_to_string(&d)),
                Ok(_) => panic!("selected cooked error succeeded"),
            }
        }
    });
}

#[test]
fn a_malformed_cooked_handle_is_refused_by_its_typed_guard() {
    use flow_syntax::{context as fctx, ty::Ty, value::Value};
    let original = flow_check::op::find(&[], "frame/dt", &fctx::value()).expect("frame/dt");
    for (id, message) in [(999i64, "A cooked handle is missing from this execution."), (0, "A cooked handle has the wrong declared type.")] {
        let probe = flow_check::op::Op {
            name: "bad_handle".to_owned(),
            signature: flow_check::op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] },
            out: Arc::new(|_| Ok(Ty::Bool)),
            out_args: None,
            live: false,
            arithmetic: None,
            packed_extension: None,
            body: Arc::new(move |_, _, _| Ok(flow_syntax::value::source_value("$core/cooked", Ty::Bool, vec![("id".to_owned(), Value::Int(id))]))),
            ..original.clone()
        };
        for count in [0usize, 1] {
            let geometry = line_points(&vec![(0.0, 0.0, 0.0); count]);
            let snapshot = lisp_sop::snapshot(&geometry);
            let factory = edit::factory(edit::factory_args("handle_source", "Source", &["Test"]), 0, Arc::new(move |_| Ok(snapshot.clone())));
            let mut factories = vec![factory];
            factories.extend(sop::nodes::factories());
            let cat = crate::catalog::of_factories(crate::manifest::VERSION, &[], None, &factories).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
            let forms = parse("(workspace handles\n        (defnode sop/handle_probe [(geo : geometry)]\n          (if (int/gt (sop/count geo :points) 0) (if (bad_handle) geo geo) geo))\n        (graph g :context sop (sop/handle_probe (sop/handle_source))))");
            let mut ops = vec![probe.clone()];
            ops.extend(operators::all().iter().cloned());
            let checked = match crate::catalog::check_workspace(&ops, false, None, &cat, &forms) {
                (Some(c), _) => c,
                (_, diagnostics) => panic!("{}", diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
            };
            let lowered = lower::of_checked(Some(factories), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
            let n = lisp_sop::root(&lowered, &lowered.graphs[0]).unwrap();
            for domains in [1i64, 8] {
                with_session(&mut |s| {
                    let context = sctx::create(ContextOptions { domains: Some(domains), ..Default::default() }).unwrap();
                    let result = session::cook(s, None, &context, &n);
                    if count == 0 {
                        match result {
                            Ok(output) => assert!(geometry_bytes(&sop::payload::geometry(&output.payload).unwrap()) == geometry_bytes(&geometry), "untaken malformed handle changed geometry"),
                            Err(d) => panic!("{}", rays_diagnostic::error_to_string(&d)),
                        }
                    } else {
                        match result {
                            Err(d) => assert!(d.code == "E_DATA_SOURCE" && d.message == message, "malformed cooked handle bypassed its typed guard"),
                            Ok(_) => panic!("malformed cooked handle succeeded"),
                        }
                    }
                });
            }
        }
    }
}

#[test]
fn parameter_validation_rejects_without_changing_the_node() {
    for predicate in ["(> count 0)", "(if (> count 0) true (error \"E_PARAM\" \"count must be positive\" :cause \"validation reason\"))"] {
        let n = lower_ref(
            false,
            &format!("(workspace parameter_error\n      (defnode sop/parameter_error :validate {predicate} :code \"E_PARAM\" :message \"count must be positive\"\n        [(geo : geometry) (count : int 1)] geo)\n      (graph g :context sop (sop/parameter_error (sop/line))))"),
        );
        let previous = node::parameter_key(&n).to_vec();
        match node::apply_parameters(&n, &[("count".to_owned(), param::Value::Int(0))]) {
            Err(message) => assert!(message == "count must be positive", "parameter validation error formatting differs"),
            Ok(_) => panic!("invalid parameter edit succeeded"),
        }
        assert!(node::parameter_key(&n) == previous, "rejected validation changed the node");
    }
}

#[test]
fn a_lisp_body_keeps_topology_attributes_and_ordered_groups() {
    use rdk_core::attribute::{Owner, Storage};
    let positions = rdk_ok(rdk_core::packed::float3::of_owned(vec![-1.0, 0.0, 1.0], vec![-0.0, 0.0, 0.0], vec![2.0, 3.0, 4.0]));
    let topology = rdk_ok(rdk_core::topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]));
    let attributes: Vec<rdk_core::attribute::T> = vec![
        (Owner::Point, "weight", Storage::Float(Arc::new(vec![0.25, 0.5, 0.75]))),
        (Owner::Point, "id", Storage::Int(Arc::new(vec![2, 1, 0]))),
        (Owner::Point, "tag", Storage::Text(Arc::new(vec![b"one".to_vec(), b"two".to_vec(), b"three".to_vec()].into()))),
        (Owner::Point, "weights", Storage::FloatArray(rdk_ok(rdk_core::packed::float_array::create_owned(vec![0, 0, 2, 3], vec![0.25, 0.5, 0.75])))),
        (Owner::Vertex, "uv", Storage::Float3(positions.clone())),
        (Owner::Primitive, "material", Storage::Int(Arc::new(vec![7]))),
    ]
    .into_iter()
    .map(|(owner, name, storage)| rdk_ok(rdk_core::attribute::create_owned(name, owner, storage)))
    .collect();
    let groups = vec![rdk_ok(rdk_core::group::ordered(rdk_core::group::Owner::Point, "ordered", 3, &[2, 0]))];
    let index = rdk_ok(rdk_core::topology_index::create(None, &topology));
    let edge_groups = vec![rdk_ok(rdk_core::edge_group::init(None, &topology, &index, "selected_edges", &|i| i == 0))];
    let original = rdk_ok(geo::create(positions, topology, Some(attributes), Some(groups), Some(edge_groups)));
    let before = geometry_bytes(&original);
    let p = rdk_ok(rdk_core::packed::float3::view::view(geo::positions(&original)));
    let shifted = rdk_ok(rdk_core::packed::float3::view::of_owned_exn(p.x.iter().map(|x| x + 2.0).collect(), p.y.iter().map(|y| y + 0.0).collect(), p.z.iter().map(|z| z + 0.0).collect()));
    let expected = geometry_bytes(&rdk_ok(geo::with_positions(shifted, &original)));
    let n = lisp_sop::workspace_node(&[("src".to_owned(), lisp_sop::snapshot(&original))], SHIFT);
    for domains in [1i64, 4] {
        for grain in [1i64, 97] {
            with_session(&mut |s| {
                assert!(geometry_bytes(&cook_with(s, domains, grain, 0, &n)) == expected, "Lisp body changed topology, attributes or ordered groups");
                assert!(geometry_bytes(&original) == before, "Lisp body mutated its immutable input");
            });
        }
    }
}

#[test]
fn a_defnode_keeps_its_operation_inputs_schema_and_cache_identity() {
    let original = lisp_sop::node(&[], "(sop/line :points 3)");
    let n = lisp_sop::workspace_node(&[("src".to_owned(), original.clone())], SHIFT);
    assert!(node::operation(&n) == "lisp_shift", "defnode must keep its operation");
    assert!(node::inputs(&n).len() == 1, "body leaked into public inputs");
    assert!(field_names(&n) == vec!["distance".to_owned()], "parameter schema was lost");
    let mut s = session::create(32, 1_000_000).unwrap();
    let context = sctx::create(ContextOptions::default()).unwrap();
    let input = cook(&mut s, &context, &original);
    let first = cook(&mut s, &context, &n);
    let (ix, iy, iz) = position(&input, 0);
    let (x, y, z) = position(&first, 0);
    assert!(x == ix + 2.0 && y == iy && z == iz, "body did not use authored parameters");
    assert!(same(&cook(&mut s, &context, &n), &first), "unchanged Lisp node missed the cache");
    let (changed, _) = node::apply_parameters(&n, &[("distance".to_owned(), param::Value::Float(3.0))]).unwrap();
    assert!(node::id(&changed) == node::id(&n), "parameter edit changed logical identity");
    let result = cook(&mut s, &context, &changed);
    assert!(!same(&result, &first), "changed parameter hit old cache");
    assert!(position(&result, 0).0 == ix + 3.0, "parameter edit did not rebuild the Lisp body");
    // edits share one specialized, assembled body; each still reads its own values
    for distance in [2.0, 5.0, 3.0] {
        let (edited, _) = node::apply_parameters(&changed, &[("distance".to_owned(), param::Value::Float(distance))]).unwrap();
        assert!(position(&cook(&mut s, &context, &edited), 0).0 == ix + distance, "a shared body kept an earlier value");
    }
    session::close(&mut s);
}

const CONTROLS: &str = r#"(workspace controls
  (defnode sop/all_controls
    [(geo : geometry)
     (enabled : bool true)
     (count : int 2 :min 1 :max 10 :hard_min 1)
     (amount : float 0.5 :min -10 :max 10 :hard_min -20 :hard_max 20)
     (tag : text "original")
     (mode : text "A" :kind choice :choices ["A" "B"])
     (offset : vec3 [1 2 3] :min -10 :max 10)]
    (let* [distance (+ (+ (+ (+ amount count)
      (if enabled 1.0 0.0)) (if (equal? tag "original") 0.0 10.0))
      (if (equal? mode "A") 0.0 20.0))]
      (sop/with_attr geo :P (exact
        (map (fn [p] (+ (+ p offset) [distance 0.0 0.0])) (sop/attr geo :P))))))
  (graph g :context sop (sop/all_controls (sop/line :kind "Points" :points 1))))"#;

#[test]
fn every_control_kind_reaches_the_body_with_exactly_one_miss_per_edit() {
    with_session(&mut |s| {
        let n = lower_ref(false, CONTROLS);
        let first = cook_with(s, 1, 97, 0, &n);
        assert!(position(&first, 0) == (4.5, 2.0, 3.0), "scalar and vector defaults did not reach the body");
        let before = session::stats(s);
        assert!(same(&cook_with(s, 1, 97, 0, &n), &first), "all-controls unchanged node missed cache");
        assert!(session::stats(s).misses == before.misses, "unchanged body caused hidden public misses");
        assert!(field_names(&n) == ["enabled", "count", "amount", "tag", "mode", "offset_x", "offset_y", "offset_z"].map(String::from).to_vec(), "parameter kinds lost declaration ordering");
        use param::Value as V;
        for (name, value, expected) in [
            ("enabled", V::Bool(false), (3.5, 2.0, 3.0)),
            ("count", V::Int(3), (5.5, 2.0, 3.0)),
            ("amount", V::Float(1.5), (5.5, 2.0, 3.0)),
            ("tag", V::Text(b"changed".to_vec()), (14.5, 2.0, 3.0)),
            ("mode", V::Choice("B".to_owned()), (24.5, 2.0, 3.0)),
            ("offset_x", V::Float(4.0), (7.5, 2.0, 3.0)),
            ("offset_y", V::Float(4.0), (4.5, 4.0, 3.0)),
            ("offset_z", V::Float(4.0), (4.5, 2.0, 4.0)),
        ] {
            let (changed, effects) = node::apply_parameters(&n, &[(name.to_owned(), value)]).unwrap();
            assert!(effects.cook && node::id(&changed) == node::id(&n), "{name}: parameter edit lost identity");
            let before = session::stats(s);
            let result = cook_with(s, 1, 97, 0, &changed);
            assert!(position(&result, 0) == expected, "{name}: body ignored updated parameter");
            assert!(session::stats(s).misses == before.misses + 1, "{name}: changed Lisp parameter must cause exactly one public miss");
            let before = session::stats(s);
            let again = cook_with(s, 1, 97, 0, &changed);
            assert!(same(&again, &result) && session::stats(s).misses == before.misses, "{name}: repeated changed value missed cache");
        }
        for (name, value) in [("amount", V::Float(f64::NAN)), ("amount", V::Float(f64::INFINITY)), ("mode", V::Choice("missing".to_owned()))] {
            assert!(node::apply_parameters(&n, &[(name.to_owned(), value)]).is_err(), "{name}: parameter normalization accepted an invalid value");
        }
        // Inspector writes use the native Param clamp; authored Lisp is checked.
        let (clamped, _) = node::apply_parameters(&n, &[("count".to_owned(), V::Int(0))]).unwrap();
        let count = node::parameter_fields(&clamped).into_iter().find(|f| f.name == "count").unwrap();
        assert!(count.current == V::Int(1), "integer hard bound differs from native inspector normalization");
    });
}

#[test]
fn node_validation_runs_before_the_body() {
    // The body would fail its node's hard range if evaluated before validation.
    let n = lower_ref(
        false,
        r#"(workspace validate
    (defnode sop/validated
      :validate (> count 0) :code "E_POSITIVE" :message "count must be positive"
      [(count : int 1 :min -10 :max 10)]
      (sop/line :kind "Points" :points count))
    (graph g :context sop (sop/validated)))"#,
    );
    match node::apply_parameters(&n, &[("count".to_owned(), param::Value::Int(0))]) {
        Err(message) => assert!(message.contains("E_POSITIVE"), "body ran before node validation"),
        Ok(_) => panic!("node validation did not reject edited value"),
    }
}

#[test]
fn optional_and_rest_slots_pass_through() {
    with_session(&mut |s| {
        let optional = lower_ref(
            false,
            r#"(workspace optional
    (defnode sop/optional [(geo : geometry :required false)]
      (if (equal? geo nil) (sop/line :kind "Points" :points 1) geo))
    (graph g :context sop (sop/optional)))"#,
        );
        assert!(point_count_of(s, &optional) == 1, "absent optional slot did not become nil");
        for (slot, call, points) in [
            ("", "(sop/repeated (sop/line :points 2) (sop/line :points 3))", 2),
            (":required false", "(sop/repeated)", 1),
            (":required false", "(sop/repeated (sop/line :points 3))", 3),
            (":required false", "(sop/repeated nil)", 1),
        ] {
            let text = format!("(workspace rest\n      (defnode sop/repeated [(geos : geometry {slot} :rest true)]\n        (if (> (count geos) 0) (first geos) (sop/line :kind \"Points\" :points 1)))\n      (graph g :context sop {call}))");
            let n = lower_ref(false, &text);
            assert!(point_count_of(s, &n) == points, "rest-slot topology passthrough failed");
        }
    });
}

#[test]
fn nested_lisp_calls_keep_parameters_and_leak_no_cache_nodes() {
    let source = r#"(workspace nested
    (defnode sop/inner [(geo : geometry) (distance : float 0.5)]
      (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance 0.0 0.0])) (sop/attr geo :P)))))
    (defnode sop/outer [(geo : geometry) (distance : float 1.5)]
      (sop/inner geo :distance distance))
    (graph g :context sop (sop/outer (sop/line :kind "Points" :points 1))))"#;
    with_session(&mut |s| {
        let n = lower_ref(false, source);
        let g = cook_with(s, 1, 97, 0, &n);
        assert!(position(&g, 0) == (1.5, 0.0, 0.0), "nested Lisp call lost its parameters");
        let before = session::stats(s);
        let (n, _) = node::apply_parameters(&n, &[("distance".to_owned(), param::Value::Float(2.5))]).unwrap();
        let g = cook_with(s, 1, 97, 0, &n);
        assert!(position(&g, 0) == (2.5, 0.0, 0.0), "nested Lisp parameter edit did not recook");
        assert!(session::stats(s).misses == before.misses + 1, "nested bodies leaked private cache nodes");
    });
}

#[test]
fn exact_lisp_geometry_bytes_are_equal_across_interpreter_domains_and_grain() {
    let source = r#"(workspace exact
    (defnode sop/shift [(geo : geometry) (distance : float 0.125)]
      (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance -0.0 0.0])) (sop/attr geo :P)))))
    (graph g :context sop (sop/shift (sop/grid :columns 64 :rows 64))))"#;
    let mut baseline = Vec::new();
    with_session(&mut |s| baseline = bytes_of(s, &lower_ref(false, source)));
    for reference in [false, true] {
        let n = lower_ref(reference, source);
        for domains in [1i64, 4] {
            for grain in [1i64, 97, 4096] {
                with_session(&mut |s| {
                    let g = cook_with(s, domains, grain, 0, &n);
                    assert!(geo::point_count(&g) == 4225, "exactness fixture cardinality changed");
                    assert!(geometry_bytes(&g) == baseline, "Lisp geometry bytes differ by interpreter/kernel/domain/grain");
                });
            }
        }
    }
}

#[test]

fn typed_function_slots_keep_their_callable_and_captures() {
    for (keyword, call) in [("", "(sop/function_slot field)"), (":keyword true", "(sop/function_slot :field field)")] {
        with_session(&mut |s| {
            let source = format!("(workspace typed_function\n    (defnode sop/function_slot [(field : (fn [vec3] float) {keyword})]\n      (sop/iso_surface :field field :resolution [4 4 4]))\n    (graph g :context sop\n      (let* [radius 0.7 field (fn [p] (- (length p) radius))]\n        {call})))");
            let geometry = bytes_of(s, &lower_ref(false, &source));
            let native = bytes_of(s, &lisp_sop::node(&[], "(sop/iso_surface :field (fn [p] (- (length p) 0.7)) :resolution [4 4 4])"));
            assert!(geometry == native, "typed function slot lost its callable or captures");
        });
    }
}

#[test]

fn typed_image_slots_keep_their_cooked_pixels() {
    for (keyword, call) in [
        ("", r#"(sop/image_slot (sop/grid :columns 2 :rows 2 :uv_attribute "uv") (image/noise :width 2 :height 2))"#),
        (":keyword true", r#"(sop/image_slot (sop/grid :columns 2 :rows 2 :uv_attribute "uv") :picture (image/noise :width 2 :height 2))"#),
    ] {
        with_session(&mut |s| {
            let source = format!("(workspace typed_image\n    (defnode sop/image_slot [(geo : geometry) (picture : image {keyword})]\n      (sop/attr_from_image geo :image picture))\n    (graph g :context sop {call}))");
            let geometry = bytes_of(s, &lower_ref(false, &source));
            let native = bytes_of(s, &lisp_sop::node(&[], r#"(sop/attr_from_image (sop/grid :columns 2 :rows 2 :uv_attribute "uv") :image (image/noise :width 2 :height 2))"#));
            assert!(geometry == native, "typed image slot lost its cooked pixels");
        });
    }
}

#[test]

fn a_distinct_local_function_is_not_replaced_by_forwarding() {
    with_session(&mut |s| {
        let body = "(let* [local (fn [p] (- (length (+ p [0.25 0.0 0.0])) 0.45))]\n    (sop/merge\n      (sop/iso_surface :field field :resolution [4 4 4])\n      (sop/iso_surface :field local :resolution [4 4 4])))";
        let source = format!("(workspace distinct_functions\n    (defnode sop/two_fields [(field : (fn [vec3] float))] {body})\n    (graph g :context sop (sop/two_fields (fn [p] (- (length p) 0.7)))))");
        let native = format!("(let* [field (fn [p] (- (length p) 0.7))] {body})");
        let a = bytes_of(s, &lower_ref(false, &source));
        let b = bytes_of(s, &lisp_sop::node(&[], &native));
        assert!(a == b, "outer function forwarding replaced a distinct local function");
    });
}

#[test]

fn a_function_slot_keeps_its_outer_cooked_geometry_capture() {
    with_session(&mut |s| {
        let body = |call: &str| format!("(let* [base (sop/line :kind \"Points\" :points 1 :origin [0.7 0.0 0.0])\n         sample {{:radius (array/sum (sop/attr base :P))}}\n         field (fn [p] (- (length p) sample.radius.x))] {call})");
        let source = format!("(workspace cooked_capture\n    (defnode sop/function_capture [(field : (fn [vec3] float))]\n      (sop/iso_surface :field field :resolution [4 4 4]))\n    (graph g :context sop {}))", body("(sop/function_capture field)"));
        let geometry = bytes_of(s, &lower_ref(false, &source));
        let native = bytes_of(s, &lisp_sop::node(&[], &body("(sop/iso_surface :field field :resolution [4 4 4])")));
        assert!(geometry == native, "function slot lost its outer cooked-geometry capture");
    });
}

#[test]
fn a_map_function_slot_keeps_its_separate_outer_geometry_capture() {
    let body = |call: &str| format!("(let* [base (sop/line :kind \"Points\" :points 1 :origin [0.7 0.2 -0.1])\n      sample {{:offset (array/sum (sop/attr base :P))}}\n      field (fn [p] (+ p sample.offset))\n      geo (sop/grid :columns 2 :rows 2)] {call})");
    let mut native = Vec::new();
    with_session(&mut |s| native = bytes_of(s, &lisp_sop::node(&[], &body("(sop/with_attr geo :P (exact (map field (sop/attr geo :P))))"))));
    for call in ["(sop/map_field geo field)", "(sop/map_outer geo field)"] {
        let source = format!("(workspace map_capture\n      (defnode sop/map_field [(geo : geometry) (field : (fn [vec3] vec3))]\n        (sop/with_attr geo :P (exact (map field (sop/attr geo :P)))))\n      (defnode sop/map_outer [(geo : geometry) (field : (fn [vec3] vec3))]\n        (sop/map_field geo field))\n      (graph g :context sop {}))", body(call));
        for reference in [false, true] {
            let n = lower_ref(reference, &source);
            assert!(node::inputs(&n).len() == 2, "forwarded function geometry leaked into public input slots");
            for domains in [1i64, 4] {
                with_session(&mut |s| {
                    let g = cook_with(s, domains, 97, 0, &n);
                    assert!(geometry_bytes(&g) == native, "map function slot confused its separate outer geometry capture with the input geometry");
                    let before = session::stats(s);
                    assert!(same(&cook_with(s, domains, 97, 0, &n), &g) && session::stats(s).misses == before.misses, "unchanged forwarded geometry capture missed the public cache");
                });
            }
        }
    }
}

#[test]
fn a_numeric_body_without_a_packed_kernel_is_refused_not_interpreted() {
    let source = r#"(workspace refused
    (defnode sop/unsupported_map [(geo : geometry)]
      (sop/with_attr geo :P (exact
        (map (fn [p] (if (equal? p [0.0 0.0 0.0]) p (+ p [1.0 0.0 0.0])))
          (sop/attr geo :P)))))
    (graph g :context sop (sop/unsupported_map (sop/grid :columns 2 :rows 2))))"#;
    let refusal = |d: &rays_diagnostic::Diagnostic| d.code == "E_KERNEL_FORM" && d.message == "Numeric node bodies must compile to packed kernels." && d.span.is_some();
    // Projection and graph compilation retain the body; cooking is where strict kernel preparation must refuse it.
    match lower::workspace(None, Vec::new(), lower::Options::default(), &parse(source)) {
        Err(d) => assert!(refusal(&d), "numeric Lisp body had an unrelated refusal: {}", rays_diagnostic::to_string(&d)),
        Ok(l) => {
            let graph = l.graphs.iter().find(|g| g.default).unwrap().clone();
            match lisp_sop::root(&l, &graph) {
                Err(message) => assert!(message.split(' ').any(|w| w == "[E_KERNEL_FORM]"), "numeric Lisp body had an unrelated graph refusal: {message}"),
                Ok(n) => with_session(&mut |s| {
                    let context = sctx::create(ContextOptions { domains: Some(1), ..Default::default() }).unwrap();
                    match session::cook(s, None, &context, &n) {
                        Err(d) => assert!(refusal(&d), "numeric Lisp body had an unrelated cook refusal: {}", rays_diagnostic::error_to_string(&d)),
                        Ok(_) => panic!("numeric Lisp body silently accepted an interpreter fallback"),
                    }
                }),
            }
        }
    }
}

const GRID_PREFIX: &str = "(let* [base (sop/line :kind \"Points\" :points 1 :origin [0.7 0.2 -0.1])\n    sample {:offset (array/sum (sop/attr base :P))}\n    field (fn [p] (+ p sample.offset))\n    other (fn [p] (- p sample.offset))\n    geo (sop/grid :columns 2 :rows 2)]";

#[test]

fn optional_and_rest_callable_slots_keep_bytes_and_cache() {
    with_session(&mut |s| {
        let prefix = |call: &str| format!("{GRID_PREFIX} {call})");
        let unchanged = cook_with(s, 1, 97, 0, &lisp_sop::node(&[], &prefix("geo")));
        let mapped = cook_with(s, 1, 97, 0, &lisp_sop::node(&[], &prefix("(sop/with_attr geo :P (exact (map field (sop/attr geo :P))))")));
        let twice = cook_with(s, 1, 97, 0, &lisp_sop::node(&[], &prefix("(sop/with_attr geo :P (exact (map field (map field (sop/attr geo :P)))))")));
        let different = cook_with(s, 1, 97, 0, &lisp_sop::node(&[], &prefix("(sop/with_attr geo :P (exact (map other (map field (sop/attr geo :P)))))")));
        let cases: Vec<(&str, &str, Vec<(&str, &geo::T)>)> = vec![
            ("[(geo : geometry) (field : (fn [vec3] vec3) :required false)]", "(if (equal? field nil) geo (sop/with_attr geo :P (exact (map field (sop/attr geo :P)))))", vec![("(sop/slots geo)", &unchanged), ("(sop/slots geo field)", &mapped)]),
            ("[(geo : geometry) (fields : (fn [vec3] vec3) :rest true :required false)]", "(if (= (count fields) 0) geo\n      (sop/with_attr geo :P (exact (map (first fields) (sop/attr geo :P)))))", vec![("(sop/slots geo)", &unchanged), ("(sop/slots geo field field)", &mapped), ("(sop/slots geo (list field field))", &mapped)]),
            ("[(geo : geometry) (fields : (fn [vec3] vec3) :rest true)]", "(let* [field (first fields) other (first (rest fields))]\n      (sop/with_attr geo :P (exact (map other (map field (sop/attr geo :P))))))", vec![("(sop/slots geo field field)", &twice), ("(sop/slots geo field other)", &different)]),
            ("[(geo : geometry :required false) (field : (fn [vec3] vec3))]", "(let* [target (if (equal? geo nil) (sop/grid :columns 2 :rows 2) geo)]\n      (sop/with_attr target :P (exact (map field (sop/attr target :P)))))", vec![("(sop/slots nil field)", &mapped)]),
            ("[(field : (fn [vec3] vec3)) (geos : geometry :rest true :required false)]", "(let* [target (if (= (count geos) 0) (sop/grid :columns 2 :rows 2) (first geos))]\n      (sop/with_attr target :P (exact (map field (sop/attr target :P)))))", vec![("(sop/slots field)", &mapped), ("(sop/slots field geo geo)", &mapped)]),
        ];
        for (inputs, body, calls) in cases {
            for (call, expected) in calls {
                let source = format!("(workspace repeated_functions\n      (defnode sop/slots {inputs} {body}) (graph g :context sop {}))", prefix(call));
                let n = lower_ref(false, &source);
                let g = cook_with(s, 1, 97, 0, &n);
                assert!(geometry_bytes(&g) == geometry_bytes(expected), "typed callable slot changed bytes: {call}");
                let before = session::stats(s);
                assert!(same(&cook_with(s, 1, 97, 0, &n), &g) && session::stats(s).misses == before.misses, "unchanged optional/rest callable missed its cache");
            }
        }
    });
}

#[test]

fn optional_and_rest_image_slots_keep_bytes() {
    with_session(&mut |s| {
        let prefix = |call: &str| format!("(let* [geo (sop/grid :columns 2 :rows 2 :uv_attribute \"uv\")\n    picture (image/noise :width 2 :height 2)] {call})");
        let unchanged = bytes_of(s, &lisp_sop::node(&[], &prefix("geo")));
        let textured = bytes_of(s, &lisp_sop::node(&[], &prefix(r#"(sop/attr_from_image geo :image picture :attribute "shade")"#)));
        let cases: Vec<(&str, &str, Vec<(&str, &Vec<u8>)>)> = vec![
            ("(picture : image :required false)", "(if (equal? picture nil) geo\n      (sop/attr_from_image geo :image picture :attribute \"shade\"))", vec![("(sop/images geo)", &unchanged), ("(sop/images geo picture)", &textured)]),
            ("(pictures : image :rest true :required false)", "(if (= (count pictures) 0) geo\n      (sop/attr_from_image geo :image (first pictures) :attribute \"shade\"))", vec![("(sop/images geo)", &unchanged), ("(sop/images geo picture picture)", &textured), ("(sop/images geo (list picture picture))", &textured)]),
        ];
        for (slot, body, calls) in cases {
            for (call, expected) in calls {
                let source = format!("(workspace repeated_images\n      (defnode sop/images [(geo : geometry) {slot}] {body})\n      (graph g :context sop {}))", prefix(call));
                assert!(&bytes_of(s, &lower_ref(false, &source)) == expected, "typed image slot changed bytes: {call}");
            }
        }
    });
}

fn lower_prev(previous: Option<&lower::T>, factories: Option<Vec<edit::Factory>>, text: &str) -> (lower::T, node::T) {
    let options = lower::Options { compiled_ids: previous.map(|l| l.compiled_ids.clone()).unwrap_or_default(), sites: previous.map(|l| l.sites.clone()).unwrap_or_default(), ..lower::Options::default() };
    let l = lower::workspace(factories, Vec::new(), options, &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let n = lisp_sop::root(&l, &l.graphs[0]).unwrap();
    (l, n)
}

#[test]
fn call_site_and_helper_edits_invalidate_only_what_depends_on_them() {
    let source = |amount: &str, helper: &str| {
        format!(
            "(workspace stable_body
    (defn shifted :context value [(p : vec3)] (+ p [{helper} 0.0 0.0]))
    (defnode sop/stable_body [(geo : geometry) (amount : float 0.0)]
      (sop/with_attr geo :P (exact (map (fn [(p : vec3)]
        (+ (shifted p) [amount 0.0 0.0])) (sop/attr geo :P)))))
    (graph g :context sop (let* [upstream (sop/line :kind \"Points\" :points 3)
      edited (sop/stable_body upstream :amount {amount})
      sibling (sop/stable_body upstream :amount 0.0)] (sop/merge edited sibling))))"
        )
    };
    for domains in [1i64, 8] {
        with_session(&mut |s| {
            let (first, n) = lower_prev(None, Some(sop::nodes::factories()), &source("1.0", "2.0"));
            cook_with(s, domains, 97, 0, &n);
            let before = session::stats(s);
            let (edited, n) = lower_prev(Some(&first), Some(sop::nodes::factories()), &source("3.0", "2.0"));
            cook_with(s, domains, 97, 0, &n);
            let after = session::stats(s);
            // the edited card's body, its card and the merge recook; the line and the sibling's body and card hit
            assert!(after.misses - before.misses == 3 && after.hits - before.hits == 4, "A call-site parameter edit recooked an unchanged Lisp sibling: misses {} hits {}", after.misses - before.misses, after.hits - before.hits);
            let (_, n) = lower_prev(Some(&edited), Some(sop::nodes::factories()), &source("3.0", "4.0"));
            let changed = cook_with(s, domains, 97, 0, &n);
            let after_helper = session::stats(s);
            assert!(after_helper.misses - after.misses == 3 && after_helper.hits - after.hits == 4, "A helper body edit failed to invalidate both dependent Lisp bodies: misses {} hits {}", after_helper.misses - after.misses, after_helper.hits - after.hits);
            let (_, oracle) = lower_prev(None, Some(sop::nodes::factories()), &source("3.0", "4.0"));
            with_session(&mut |fresh| {
                assert!(geometry_bytes(&changed) == geometry_bytes(&cook_with(fresh, domains, 97, 0, &oracle)), "Helper invalidation returned stale cached geometry");
            });
        });
    }
}

const CONE_INITIAL: [&str; 9] = ["0.0", "2.0", "0.0", "0.0", "-10.0", "point", "0.0", "0.0", "0.0"];

fn cone_source(extra: &str, v: &[&str; 9]) -> String {
    format!(
        "(workspace declaration_cones
    {0}
    (defn unrelated :context value [] {1})
    (defn leaf :context value [(p : vec3)] (+ p [{2} 0.0 0.0]))
    (defmacro unrelated_adjusted [p] `(+ ~p [{3} 0.0 0.0]))
    (defmacro adjusted_inner [p] `(+ ~p [{4} 0.0 0.0]))
    (defmacro adjusted [p] `(adjusted_inner ~p))
    (defmacro dispatch [callee arg] `(~callee ~arg))
    (defmacro dispatched_inner [p] `(let* [held# ~p] (+ held# [{5} 0.0 0.0])))
    (defn through :context value [(p : vec3)] (leaf (dispatch dispatched_inner (adjusted p))))
    (defn allowed :context value [(amount : float)] (>= amount {6}))
    (defn canonical_owner :context value [(raw : text)] \"{7}\")
    (defn owner :context value [(raw : text)] (canonical_owner raw))
    (defnode sop/cone_child :validate (allowed amount)
      [(geo : geometry) (amount : float {8})]
      (let* [moved (exact (map through (sop/attr geo :P)))]
        (sop/with_attr geo :P (exact (map (fn [(p : vec3)]
          (+ p [(+ amount {9}) 0.0 0.0])) moved)))))
    (defnode sop/cone_parent
      [(geo : geometry) (delta : float 0.0)
       (rules : text \" POINT \" :kind encoded :normalize (fn [(raw : text)] (owner raw)))]
      (let* [child (sop/cone_child geo)]
        (sop/with_attr child :P (exact (map (fn [(p : vec3)]
          (+ p [delta 0.0 0.0])) (sop/attr child :P))))))
    (graph g :context sop (let* [upstream (sop/line :kind \"Points\" :points 3)
      edited (sop/cone_parent upstream :delta 1.0)
      sibling (sop/cone_parent upstream :delta 0.0)] (sop/merge edited sibling))))",
        extra, v[0], v[1], v[7], v[6], v[8], v[4], v[5], v[2], v[3]
    )
}

#[test]

fn declaration_cones_invalidate_exactly_the_edited_dependencies() {
    let initial = CONE_INITIAL;
    for domains in [1i64, 8] {
        for (index, value, label) in [
            (0usize, "9.0", "unrelated helper"),
            (1, "4.0", "transitive helper/Fn_ref"),
            (2, "0.5", "child default"),
            (3, "0.75", "child body"),
            (4, "-9.0", "child validation helper"),
            (5, "detail", "transitive normalizer helper"),
            (6, "1.25", "nested macro"),
            (7, "2.0", "unrelated macro"),
            (8, "0.25", "parameterized macro head"),
        ] {
            with_session(&mut |s| {
                let (first, n) = lower_prev(None, None, &cone_source("", &initial));
                let before_bytes = geometry_bytes(&cook_with(s, domains, 97, 0, &n));
                let before = session::stats(s);
                let mut changed = initial;
                changed[index] = value;
                let (_, n) = lower_prev(Some(&first), None, &cone_source("", &changed));
                let output = cook_with(s, domains, 97, 0, &n);
                let after = session::stats(s);
                if index == 0 || index == 7 {
                    assert!(after.misses == before.misses && after.hits > before.hits, "An unrelated helper edit recooked a declaration outside its dependency cone ({label})");
                    assert!(geometry_bytes(&output) == before_bytes, "An unrelated helper edit changed cached geometry");
                } else {
                    // the cards' fused bodies recook (the oracle below checks their bytes), the line hits
                    assert!(after.misses > before.misses && after.hits > before.hits, "{label}: a transitive declaration edit must invalidate both consumers and keep Line warm");
                }
                let (_, oracle) = lower_prev(None, None, &cone_source("", &changed));
                with_session(&mut |fresh| {
                    assert!(geometry_bytes(&output) == geometry_bytes(&cook_with(fresh, domains, 97, 0, &oracle)), "{label}: declaration edit returned stale geometry");
                });
                let before = session::stats(s);
                cook_with(s, domains, 97, 0, &n);
                assert!(session::stats(s).misses == before.misses, "{label}: unchanged declaration cone missed its warm cache");
            });
        }
    }
}

#[test]
fn unrelated_structural_macro_and_helper_edits_keep_the_cache() {
    let extra = "(defmacro irrelevant_bump [x] `(let* [owned# ~x] (+ owned# 1.0)))\n    (defn extra_unrelated :context value [] (irrelevant_bump 2.0))";
    for domains in [1i64, 8] {
        with_session(&mut |s| {
            let (first, n) = lower_prev(None, None, &cone_source("", &CONE_INITIAL));
            let bytes = geometry_bytes(&cook_with(s, domains, 97, 0, &n));
            let before = session::stats(s);
            let (_, n) = lower_prev(Some(&first), None, &cone_source(extra, &CONE_INITIAL));
            assert!(geometry_bytes(&cook_with(s, domains, 97, 0, &n)) == bytes, "Unrelated structural macro/helper edits changed cached geometry");
            let after = session::stats(s);
            assert!(after.misses == before.misses && after.hits > before.hits, "Unrelated macro expansion changed a reachable hygienic source key");
        });
    }
}

fn disable_boundary(form: &flow_syntax::syntax::Syntax) -> flow_syntax::syntax::Syntax {
    use flow_syntax::{arena::{SyntaxBuilder, SyntaxId}, syntax::{Node, Syntax}};
    fn rewrite(b: &mut SyntaxBuilder, form: &Syntax) -> SyntaxId {
        let node = match form.node {
            Node::List(range) | Node::Vec(range) | Node::Map(range) => {
                let boundary = matches!(form.node, Node::List(_)) && flow_syntax::syntax::head(form) == Some("sop/error_boundary");
                let mut children: Vec<_> = form.range(range).iter().map(|&id| {
                    if boundary { b.import(&form.child(id)) } else { rewrite(b, &form.child(id)) }
                }).collect();
                if boundary {
                    let enabled = b.intern("enabled");
                    children.push(b.push(0, Node::Kw(enabled), rays_diagnostic::Span { start: 0, finish: 0 }, None));
                    let disabled = b.intern("false");
                    children.push(b.push(0, Node::Sym(disabled), rays_diagnostic::Span { start: 0, finish: 0 }, None));
                }
                let children = b.range(&children);
                match form.node { Node::List(_) => Node::List(children), Node::Vec(_) => Node::Vec(children), _ => Node::Map(children) }
            }
            _ => return b.import(form),
        };
        b.push(form.id, node, form.span.clone(), form.arena().trivia(form.row_id()).cloned())
    }
    let mut b = SyntaxBuilder::new();
    let id = rewrite(&mut b, form);
    b.freeze().handle(id)
}

/// Deferred child cooking stays inside its authored error boundary.
#[test]
fn deferred_child_cooking_stays_inside_its_authored_error_boundary() {
    use rays_diagnostic::Trace;
    let calls = Arc::new(AtomicI64::new(0));
    let retained = vec![
        Trace { node_id: 88, label: "retained child".to_owned(), operation: "native.retained".to_owned() },
        Trace { node_id: 89, label: "retained input".to_owned(), operation: "native.input".to_owned() },
    ];
    let mut child_error = rays_diagnostic::error("E_CHILD".to_owned(), "child cook failed".to_owned(), None, None, Some([b"\xff raw ".as_slice(), "日本😀".as_bytes()].concat()), vec!["retained hint".to_owned()], Vec::new());
    child_error.trace = retained.clone();
    let counter = calls.clone();
    let failure = child_error.clone();
    let factory = edit::factory(
        edit::factory_args("failing_child", "Failing child", &["Test"]),
        1,
        Arc::new(move |inputs| {
            let counter = counter.clone();
            let failure = failure.clone();
            Ok(build::make(
                build::Args { cook_mode: CookMode::Generic, inputs: inputs.to_vec(), ..build::args("native.failing_child") },
                None,
                Arc::new(move |_, _, _| {
                    counter.fetch_add(1, Ordering::SeqCst);
                    Err(failure.clone())
                }),
            ))
        }),
    );
    let source = r#"(workspace boundary
    (defnode sop/error_boundary [(geo : geometry) (enabled : bool true)]
      (if enabled
        (with_error (sop/failing_child geo)
          (fn [failure] (assoc failure :code "E_WRAPPED" :message "authored wrapping")))
        (with_error geo (fn [failure] (error "E_MAPPER" "unused mapper")))))
    (graph g :context sop (sop/error_boundary (sop/line :points 2))))"#;
    let unused_source = r#"(workspace boundary
    (defnode sop/error_boundary [(geo : geometry) (enabled : bool true)]
      (let* [unused (if enabled
        (with_error (sop/failing_child geo)
          (fn [failure] (assoc failure :code "E_WRAPPED" :message "authored wrapping")))
        nil)] geo))
    (graph g :context sop (sop/error_boundary (sop/line :points 2))))"#;
    let compile = |reference: bool, text: &str| -> node::T {
        let mut factories = vec![factory.clone()];
        factories.extend(sop::nodes::factories());
        let l = lower::workspace(Some(factories), Vec::new(), lower::Options { reference, ..lower::Options::default() }, &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        lisp_sop::root(&l, &l.graphs[0]).unwrap()
    };
    for text in [source, unused_source] {
        for reference in [false, true] {
            let n = compile(reference, text);
            for domains in [1i64, 4, 8] {
                for grain in [1i64, 97, 4096] {
                    with_session(&mut |s| {
                        let before = calls.load(Ordering::SeqCst);
                        let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), ..Default::default() }).unwrap();
                        match session::cook(s, None, &context, &n) {
                            Ok(_) => panic!("protected child failure was lost"),
                            Err(actual) => {
                                assert!(actual.code == "E_WRAPPED" && actual.message == "authored wrapping" && actual.cause == child_error.cause, "deferred diagnostic mapping changed its authored payload: {}", rays_diagnostic::error_to_string(&actual));
                                assert!(actual.hints == child_error.hints && actual.trace.iter().filter(|f| retained.contains(f)).cloned().collect::<Vec<_>>() == retained, "deferred mapping lost native hints or trace");
                                assert!(calls.load(Ordering::SeqCst) == before + 1, "protected child cooked twice or outside the boundary");
                            }
                        }
                    });
                }
            }
            let forms = parse(text);
            let disabled = flow_syntax::lisp::print(&forms.iter().map(disable_boundary).collect::<Vec<_>>()).unwrap().0;
            let before = calls.load(Ordering::SeqCst);
            let mut original = None;
            with_session(&mut |s| original = Some(bytes_of(s, &lower_ref(false, "(workspace original (graph g :context sop (sop/line :points 2)))"))));
            let mut actual = None;
            with_session(&mut |s| actual = Some(bytes_of(s, &compile(reference, &disabled))));
            assert!(calls.load(Ordering::SeqCst) == before && actual == original, "untaken child/mapper executed or altered successful geometry");
        }
    }
}

/// sop/context_frame: the cook frame as a float, a Frame cache dependency, rebuilt per frame.
#[test]
fn context_frame_is_a_float_frame_dependency_in_bodies_only() {
    use sop::context::dependencies::{mem, Fact};
    let source = |body: &str| format!("(workspace frame_body\n    (defnode sop/frame_probe [(geo : geometry)]\n      (sop/with_attr geo \"f\" (array/float (sop/count geo :points) {body}) :owner \"point\" :type float))\n    (graph g :context sop (sop/frame_probe (sop/line :points 3))))");
    let cook_at = |s: &mut session::T, frame: i64, n: &node::T| {
        let context = sctx::create(ContextOptions { frame: Some(frame), ..Default::default() }).unwrap();
        geometry_bytes(&cook(s, &context, n))
    };
    for reference in [false, true] {
        let n = lower_ref(reference, &source("(sop/context_frame)"));
        assert!(mem(Fact::Frame, node::dependencies(&n)), "context frame lost its Frame cache dependency");
        assert!(!mem(Fact::Seed, node::dependencies(&n)), "context frame added a seed dependency");
        with_session(&mut |s| {
            for frame in [0i64, 7, 3, 7] {
                let expected = cook_at(s, frame, &lower_ref(reference, &source(&format!("{frame}.0"))));
                assert!(cook_at(s, frame, &n) == expected, "context frame changed with the cook frame");
                assert!(cook_at(s, frame, &n) == expected, "context frame cook is not repeatable");
            }
        });
        assert!(!mem(Fact::Frame, node::dependencies(&lower_ref(reference, &source("1.0")))), "a constant body gained a Frame dependency");
    }
    match lower::workspace(None, Vec::new(), lower::Options::default(), &parse("(workspace w (graph g :context sop (sop/line :points (int (sop/context_frame)))))")) {
        Err(d) => assert!(d.code == "E_NODE_CONTEXT", "context frame outside a body has the wrong code"),
        Ok(_) => panic!("context frame outside a defnode body was accepted"),
    }
}

/// sop/context_time: the cook time in seconds, a Time cache dependency (not Frame), body only.
#[test]
fn context_time_is_a_float_time_dependency_in_bodies_only() {
    use sop::context::dependencies::{mem, Fact};
    let source = |body: &str| format!("(workspace time_body\n    (defnode sop/time_probe [(geo : geometry)]\n      (sop/with_attr geo \"t\" (array/float (sop/count geo :points) {body}) :owner \"point\" :type float))\n    (graph g :context sop (sop/time_probe (sop/line :points 3))))");
    let cook_at = |s: &mut session::T, time: f64, n: &node::T| {
        let context = sctx::create(ContextOptions { time: Some(time), ..Default::default() }).unwrap();
        geometry_bytes(&cook(s, &context, n))
    };
    for reference in [false, true] {
        let n = lower_ref(reference, &source("(sop/context_time)"));
        assert!(mem(Fact::Time, node::dependencies(&n)), "context time lost its Time cache dependency");
        assert!(!mem(Fact::Frame, node::dependencies(&n)), "context time added a Frame dependency");
        with_session(&mut |s| {
            for time in [0.0f64, 1.25, 0.5, 1.25] {
                let expected = cook_at(s, time, &lower_ref(reference, &source(&lisp_sop::float(time))));
                assert!(cook_at(s, time, &n) == expected, "context time changed with the cook time");
            }
        });
        assert!(!mem(Fact::Time, node::dependencies(&lower_ref(reference, &source("1.0")))), "a constant body gained a Time dependency");
    }
    match lower::workspace(None, Vec::new(), lower::Options::default(), &parse("(workspace w (graph g :context sop (sop/line :points (int (sop/context_time)))))")) {
        Err(d) => assert!(d.code == "E_NODE_CONTEXT", "context time outside a body has the wrong code"),
        Ok(_) => panic!("context time outside a defnode body was accepted"),
    }
}

/// A Lisp node's body is lowered over fresh stand-ins for its inputs on every build: the factory
/// node (`card_node`, as `lisp_sop::root` takes it) and its rebuild over other inputs (`restore`, an
/// input edit) share one build of the body's nodes, and only the node that cooks prepares its
/// program (the inlined body the factory node replaces never cooks). Every node cooks the inlined
/// body's bytes.
#[test]
fn a_lisp_node_rebuilt_over_new_inputs_builds_its_body_once() {
    fn phases<X>(run: &mut dyn FnMut() -> X) -> (X, flow_syntax::phase_timer::T) {
        let start = std::time::Instant::now();
        let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || start.elapsed().as_secs_f64()));
        let x = run();
        (x, guard.finish())
    }
    let calls = flow_syntax::phase_timer::calls;
    let text = "(workspace w (graph g :context sop (sop/subdivide (sop/box) :iterations 1 :hole_group \"\")))";
    let l = lower::workspace(None, Vec::new(), lower::Options::default(), &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let (n, t) = phases(&mut || lisp_sop::root(&l, l.graphs.iter().find(|g| g.default).expect("a default graph")).expect("graph root"));
    assert_eq!(node::operation(&n), "subdivide", "the root is the factory node");
    assert_eq!(calls(&t, "lower.inlined_build"), 1, "the factory node builds its body once");
    with_session(&mut |s| {
        let input = cook_with(s, 1, 97, 0, &node::inputs(&n)[0]);
        let (rebuilt, t) = phases(&mut || build::rebuild_with_inputs(&n, &[lisp_sop::snapshot(&input)]));
        assert_eq!(calls(&t, "lower.inlined_build"), 0, "a rebuild over new inputs built the body's nodes again");
        let (cooked, t) = phases(&mut || [geometry_bytes(&cook_with(s, 1, 97, 0, &n)), geometry_bytes(&cook_with(s, 1, 97, 0, &rebuilt))]);
        assert_eq!(calls(&t, "lower.structural_prepare"), 1, "the factory node and its rebuild did not share one prepared program");
        let graph = l.graphs.iter().find(|g| g.default).unwrap();
        let expected = geometry_bytes(&cook_with(s, 1, 97, 0, &edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap()));
        assert!(cooked[0] == expected, "the factory node cooks differently from the inlined body");
        assert!(cooked[1] == expected, "the rebuilt node cooks differently");
    });
}
