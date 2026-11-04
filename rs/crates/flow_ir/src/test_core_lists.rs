// Port of lib/flow_ir/test_core_lists.ml.
use crate::core_ir as C;
use crate::core_lower;
use crate::flow_ir::Executor;
use crate::test_support::{self as ts, ok};
use flow_check::op;
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{ResidualId, Value};
use flow_syntax::{context, syntax};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
        (Value::List(a), Value::List(b)) => a.len() == b.len() && a.iter().zip(b).all(|(a, b)| same(a, b)),
        (Value::Record(a), Value::Record(b)) => a.len() == b.len() && a.iter().zip(b).all(|((n, a), (m, b))| n == m && same(a, b)),
        (a, b) => a == b,
    }
}

fn show(r: &Result<Value, Diagnostic>) -> String {
    match r {
        Ok(v) => String::from_utf8_lossy(&E::show(v)).into_owned(),
        Err(d) => rays_diagnostic::to_string(d),
    }
}

fn records_at(source: &str, name: &str, graph: &str) -> Value {
    let ws = ts::check_workspace(source);
    let evaluation = ok(E::r#static(&ws, true, None));
    let path = vec![graph.to_owned(), name.to_owned()];
    evaluation.records.iter().find(|(p, _)| *p == path).expect("record").1[0].1.clone()
}

/// Default: four times, one on each side of zero, the 4096 bound and past it. RAYS_SLOW=1 runs all eleven.
fn times() -> &'static [f64] {
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") {
        &[-3., -2.5, -0., 0., 0.37, 1., 1.9, 3., 8., 4096., 4097.]
    } else {
        &[-3., 0.37, 1.9, 4097.]
    }
}

fn check(name: &str, body: &str, failures: &mut Vec<String>) {
    let tested = if name.starts_with("captured-") { body.to_owned() } else { format!("(if (> t -1000.) {body} {body})") };
    let source = format!("(workspace w (graph g :context value (let* [tested {tested}] 0.)))");
    let Value::Residual(r) = records_at(&source, "tested", "g") else { panic!("{name}: expected residual") };
    let core = core_lower::lower(None, None, false, None, None, r).unwrap_or_else(|n| panic!("{name}: refused {n}"));
    if name == "iterate-huge-until" {
        let mut seen = false;
        C::iter(
            &mut |s: &C::Statement| {
                if let C::StatementNode::Counted { stop, .. } = &s.node {
                    assert!(stop.is_some());
                    seen = true;
                }
            },
            &core.body,
        );
        assert!(seen);
    }
    assert!(C::pp(&core) == C::pp(&core_lower::lower(None, None, false, None, None, r).unwrap()));
    let value = Value::Residual(r);
    let program = ok(Executor::compile(&value, &Default::default()));
    for &time in times() {
        let live = input::at_time(time);
        let expected = E::Host::force_reference(&value, &live, None, None, None);
        let mut verify = |label: &str, actual: Result<Value, Diagnostic>| match (&actual, &expected) {
            (Ok(a), Ok(b)) if same(a, b) => (),
            (Err(a), Err(b)) if a.code == b.code => (),
            _ => failures.push(format!("{name} at {time} ({label}): {} <> {}", show(&actual), show(&expected))),
        };
        verify("walker", E::force(&value, &live, None));
        verify("core", ts::run_simple(&core, &live));
        verify("executor", Executor::force(&program, &Default::default(), &live));
    }
}

fn parity_part(part: usize) {
    let mut failures = Vec::new();
    for (index, (name, body)) in ts::lists().iter().enumerate() {
        if index % 4 == part {
            check(name, body, &mut failures);
        }
    }
    assert!(failures.is_empty(), "{} mismatches:\n{}", failures.len(), failures.join("\n"));
}

#[test]
fn core_lists_parity_part_0() { parity_part(0) }
#[test]
fn core_lists_parity_part_1() { parity_part(1) }
#[test]
fn core_lists_parity_part_2() { parity_part(2) }
#[test]
fn core_lists_parity_part_3() { parity_part(3) }

fn builder_source() -> C::Source { C::Source { site: vec!["builder".into()], owner: vec![], instance: 0, prefix: vec![], iter: vec![], span: None } }

#[test]
fn core_list_stores_need_a_local_builder() {
    let statement = |id: i64, node: C::StatementNode| C::Statement { id, source: builder_source(), node };
    let list_int = Ty::List(Box::new(Ty::Int));
    let program = |body: Vec<C::Statement>| C::Program {
        constants: vec![C::Literal::Int(2), C::Literal::Int(0), C::Literal::Int(7), C::Literal::List(vec![C::Literal::Int(0), C::Literal::Int(0)])],
        locals: vec![Ty::Int, list_int.clone(), Ty::Int, Ty::Int, list_int.clone(), Ty::Record(vec![("xs".into(), list_int.clone())])],
        body,
        results: vec![1],
        state_slots: vec![],
        regions: vec![],
        call_slots: 0,
    };
    let init = || {
        vec![
            statement(0, C::StatementNode::Assign(0, C::Expr::Constant(0))),
            statement(1, C::StatementNode::Assign(1, C::Expr::Prim(C::Primitive::Sequence_build, vec![("count".into(), 0)]))),
            statement(2, C::StatementNode::Assign(2, C::Expr::Constant(1))),
            statement(3, C::StatementNode::Assign(3, C::Expr::Constant(2))),
        ]
    };
    let store = || statement(4, C::StatementNode::Store { sequence: 1, index: 2, value: 3 });
    let live = input::at_time(0.);
    let mut good = init();
    good.push(store());
    assert!(ts::run_simple(&program(good), &live) == Ok(Value::List(vec![Value::Int(7), Value::No_geo])));
    let mut published = init();
    published.push(statement(5, C::StatementNode::Assign(4, C::Expr::Local(1))));
    published.push(statement(6, C::StatementNode::Assign(5, C::Expr::Fields(vec![("xs".into(), 4)]))));
    published.push(store());
    let constant = vec![
        statement(0, C::StatementNode::Assign(1, C::Expr::Constant(3))),
        statement(1, C::StatementNode::Assign(2, C::Expr::Constant(1))),
        statement(2, C::StatementNode::Assign(3, C::Expr::Constant(2))),
        store(),
    ];
    for body in [published, constant] {
        match ts::run_simple(&program(body), &live) {
            Err(d) => assert!(d.code == "E_CORE_OWNERSHIP"),
            Ok(_) => panic!("A published or constant sequence accepted a store"),
        }
    }
}

#[test]
fn cooked_builders_have_their_own_length_guard() {
    let source = C::Source { site: vec!["cooked-builder".into()], ..builder_source() };
    let statement = |id: i64, node: C::StatementNode| C::Statement { id, source: source.clone(), node };
    let program = |count: i64, cooked: bool| C::Program {
        constants: vec![C::Literal::Int(count), C::Literal::Bool(cooked)],
        locals: vec![Ty::Int, Ty::Bool, Ty::List(Box::new(ty::geometry()))],
        body: vec![
            statement(0, C::StatementNode::Assign(0, C::Expr::Constant(0))),
            statement(1, C::StatementNode::Assign(1, C::Expr::Constant(1))),
            statement(2, C::StatementNode::Assign(2, C::Expr::Prim(C::Primitive::Sequence_build, vec![("count".into(), 0), ("cooked".into(), 1)]))),
        ],
        results: vec![2],
        state_slots: vec![],
        regions: vec![],
        call_slots: 0,
    };
    let live = input::at_time(0.);
    match ts::run(None, true, None, None, &program(5001, true), &live) {
        Ok(Value::List(xs)) => assert!(xs.len() == 5001 && xs.iter().all(|x| *x == Value::No_geo)),
        _ => panic!("Typed cooked collection retained an unrelated value-list cap"),
    }
    for (count, cooked) in [(5001, false), (-1, true)] {
        match ts::run(None, true, None, None, &program(count, cooked), &live) {
            Err(d) => assert!(d.code == "E_ITER_BOUND" && d.message == "Core list length exceeds its bound."),
            _ => panic!("Ordinary list or negative cooked builder lost its existing guard"),
        }
    }
    assert!(matches!(ts::run(None, true, None, None, &program(4096, false), &live), Ok(Value::List(xs)) if xs.len() == 4096));
}

fn captured(ty: &str, body: &str, value: Value) -> ResidualId {
    let default = if ty == "geometry" { "(list nil)" } else { "(list 1)" };
    let source = format!("(workspace captures (graph g :context host [(parts : (list {ty}) {default})] (let* [clock t tested {body}] 0.)))");
    let ws = ts::check_workspace(&source);
    let evaluation = ok(E::r#static(&ws, true, None));
    let path = vec!["g".to_owned(), "clock".to_owned()];
    let Value::Residual(r) = evaluation.records.iter().find(|(p, _)| *p == path).expect("clock").1[0].1.clone() else { panic!("Capture context needs a live clock") };
    let W::Node::Let(bindings, _) = &ws.graphs[0].body.node else { panic!("Missing checked capture term") };
    let term = bindings.iter().find_map(|(p, t)| if *p == W::Pattern::Name("tested".into()) { Some(t.clone()) } else { None }).expect("Missing checked capture term");
    ok(E::Residual::with_bindings(r, &term, &[("parts".to_owned(), value)]))
}

#[test]
fn known_5001_geometry_map_filter_captures() {
    // 5001 elements in RAYS_SLOW=1; the default 1025 still overflows a 100-arm unrolled selector.
    // 5001 elements in RAYS_SLOW=1. The default 257 still overflows the 100-statement loop-body check; the
    // reference clones the whole captured list per selector call, so the cost is quadratic in this size.
    let n = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 5001 } else { 257 };
    let xs: Vec<Value> = (0..n).map(|i| if i % 11 == 10 { Value::No_geo } else { Value::Deferred(ty::geometry(), if i % 2 == 0 { 17 } else { 23 }) }).collect();
    let resolve: crate::core_exec::ResolveFn = Arc::new(|value: &Value| match value {
        Value::Source(name, ty, args) if name == "$core/cooked" && *ty == ty::geometry() => match args.as_slice() {
            [(_, Value::Int(id))] => Ok(Value::Deferred(ty.clone(), *id)),
            _ => Err(ts_error(format!("Unexpected capture source: {name}"))),
        },
        Value::Source(name, ty, args) if name == "$core/operator" && *ty == ty::geometry() && matches!(args.first(), Some((n, Value::Text(t))) if n == "$name" && (t == b"nth" || t == b"first")) => {
            let Some((_, Value::Text(text))) = args.first() else { unreachable!("matched above") };
            let op = op::find(&[], &String::from_utf8_lossy(text), &context::host()).expect("collection op");
            (op.body)(&input::at_time(0.), &|_, _| panic!("Collection selector unexpectedly constructed a node"), &args[..])
        }
        Value::Source(name, ty, args) => Err(ts_error(format!("Unexpected capture source: {name} : {} {}", ty::to_string(ty), args.iter().map(|(n, v)| format!("{n}={}", String::from_utf8_lossy(&E::show(v)))).collect::<Vec<_>>().join("; ")))),
        value => Err(ts_error(format!("Unexpected capture value: {}", String::from_utf8_lossy(&E::show(value))))),
    });
    for body in [
        "(map (fn [(geo : geometry)] (nth (list geo) (int/mod (frame/index) 1))) parts)",
        "(filter (fn [(geo : geometry)] (if (< t 0.5) true (equal? geo (first parts)))) parts)",
    ] {
        let r = captured("geometry", body, Value::List(xs.clone()));
        let core = ts::lower_strict(r).unwrap_or_else(|why| panic!("Known geometry HOF refused: {why}"));
        // A homogeneous captured collection is constructed once before Counted.
        // Its loop must not contain a 5001-arm selector or rebuild its input.
        let assignments = |statements: &[C::Statement]| {
            let mut total = 0;
            C::iter(&mut |_: &C::Statement| total += 1, statements);
            total
        };
        let loops: Vec<&Vec<C::Statement>> = core.body.iter().filter_map(|s| if let C::StatementNode::Counted { body, .. } = &s.node { Some(body) } else { None }).collect();
        if loops.is_empty() || loops.iter().any(|body| assignments(body) >= 100) {
            panic!("Known collection loop shape: {body} loops={} statements={}", loops.len(), loops.iter().map(|b| assignments(b).to_string()).collect::<Vec<_>>().join(","));
        }
        let builds = |statements: &[C::Statement]| {
            let mut count = 0;
            C::iter(&mut |s: &C::Statement| { if matches!(&s.node, C::StatementNode::Assign(_, C::Expr::Prim(C::Primitive::Sequence_build, _))) { count += 1; } }, statements);
            count
        };
        assert!(loops.iter().all(|body| builds(body) == 0));
        for time in [0., 0.25, 0.75, 1.] {
            let live: Frame = input::at_time(time);
            let expected = ok(E::Host::force_reference(&Value::Residual(r), &live, None, None, Some(resolve.clone())));
            let actual = ok(ts::run(None, true, None, Some(resolve.clone()), &core, &live));
            assert!(same(&actual, &expected));
        }
    }
    let ordinary = captured("int", "(filter (fn [(x : int)] (< t 0.5)) parts)", Value::List(vec![Value::Int(1); 5001]));
    assert!(ts::lower_strict(ordinary).is_err());
}

fn ts_error(message: String) -> Diagnostic {
    rays_diagnostic::error("E_DATA_SOURCE".into(), message, None, None, None, Vec::new(), Vec::new())
}

// iterate over a packed array: the map is one region lowered once and invoked by one counted
// loop, so 5000 passes do not re-lower anything; the count is a live int, past the 4,096 bound
#[test]
fn iterate_over_a_packed_array_is_one_region_and_one_counted_loop() {
    // 8 lanes and 5000 passes in RAYS_SLOW=1; the default 2 lanes and 4097 passes (least count past the bound).
    let slow = std::env::var("RAYS_SLOW").as_deref() == Ok("1");
    let (passes, width) = if slow { (5000, 8) } else { (4097, 2) };
    let source = format!("(workspace w (graph g :context value (let* [tested (iterate (int (+ {passes}. (* t 0.))) (array/float {width} 1.0)\n    (fn [p] (exact (map (fn [(i : int)] (* 0.999 (+ (array/nth p i) (* t 0.)))) (array/range {width} :type int)))))] 0.)))");
    let Value::Residual(r) = records_at(&source, "tested", "g") else { panic!("iterate: expected residual") };
    let core = ts::lower_strict(r).unwrap_or_else(|n| panic!("iterate refused: {n}"));
    let mut counted = 0;
    C::iter(&mut |s: &C::Statement| { if matches!(s.node, C::StatementNode::Counted { .. }) { counted += 1; } }, &core.body);
    assert_eq!(core.regions.len(), 1);
    assert_eq!(counted, 1);
    let live = input::at_time(0.);
    let want = E::Host::force_reference(&Value::Residual(r), &live, None, None, None);
    match (ts::run_simple(&core, &live), want) {
        (Ok(Value::Float_array(a)), Ok(Value::Float_array(b))) => assert!(a.len() == width && a.iter().zip(b.iter()).all(|(x, y)| x.to_bits() == y.to_bits())),
        (a, b) => panic!("iterate packed: {} <> {}", show(&a), show(&b)),
    }
}
