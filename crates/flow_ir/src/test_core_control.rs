// Port of lib/flow_ir/test_core_control.ml (core_control.expected is read in place).
use crate::core_ir as C;
use crate::core_lower;
use crate::flow_ir::executor;
use crate::test_support::{self as ts, ok};
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::ty::Ty;
use flow_syntax::value::{ResidualId, Value};

fn checked(source: &str) -> W::Workspace {
    let forms = ok(syntax::parse(source));
    match W::check(&[ts::offset_op()], false, false, None, &ts::catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    }
}

fn residual(source: &str) -> ResidualId {
    let evaluation = ok(E::static_with(&checked(source), true, None, None));
    let path: flow_check::workspace::Path = vec!["g".into(), "tested".into()];
    match &evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1 {
        Value::Residual(r) => *r,
        _ => panic!("Expected a residual: {source}"),
    }
}

fn lower(r: ResidualId) -> C::Program { core_lower::lower(None, None, false, None, None, r).unwrap_or_else(|name| panic!("Core refused {name}")) }

fn bits(x: f64, y: f64) -> bool { x.to_bits() == y.to_bits() }

fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => bits(*x, *y),
        (Value::Vec2(a, b), Value::Vec2(x, y)) => bits(*a, *x) && bits(*b, *y),
        (Value::Vec3(a, b, c), Value::Vec3(x, y, z)) => bits(*a, *x) && bits(*b, *y) && bits(*c, *z),
        (Value::Vec4(a, b, c, d), Value::Vec4(x, y, z, w)) => bits(*a, *x) && bits(*b, *y) && bits(*c, *z) && bits(*d, *w),
        (Value::List(a), Value::List(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| same(a, b)),
        (Value::Record(a), Value::Record(b)) => a.len() == b.len() && a.iter().zip(b).all(|((n, a), (m, b))| n == m && same(a, b)),
        (a, b) => a == b,
    }
}

fn dump() -> String {
    let mut out = String::new();
    for (name, source) in ts::control() {
        let r = residual(&source);
        let core = lower(r);
        let printed = C::pp(&core);
        assert!(printed == C::pp(&lower(r)));
        for time in [-3., -2.5, 0., 0.37, 1., 1.9, 100.25] {
            let live = input::at_time(time);
            let expected = E::host::force_reference(&Value::Residual(r), &live, None, None, None);
            match (ts::run_simple(&core, &live), expected) {
                (Ok(actual), Ok(expected)) => {
                    if name == "bypass" || name == "bypass-keyword" {
                        assert!(same(&actual, &Value::Float(time)));
                    }
                    assert!(same(&actual, &expected), "{name}: value mismatch");
                }
                (Err(actual), Err(expected)) => assert!(actual.code == expected.code),
                _ => panic!("{name}: result mismatch"),
            }
        }
        out.push_str(&name);
        out.push('\n');
        out.push_str(&printed);
    }
    out
}

#[test]
fn core_control_print_is_stable() {
    let actual = dump();
    let expected = include_str!("../../../crates/flow_ir/core_control.expected").replace("\r\n", "\n");
    if actual != expected {
        let first = actual.lines().zip(expected.lines()).position(|(a, b)| a != b).unwrap_or(0);
        panic!("Core control print changed at line {}:\nactual:   {:?}\nexpected: {:?}", first + 1, actual.lines().nth(first), expected.lines().nth(first));
    }
}

fn leaf(template: &C::Statement, id: i64) -> C::Statement { C::Statement { id, node: C::StatementNode::Assign(id, C::Expr::Constant(id)), ..template.clone() } }

#[test]
fn dynamic_call_identity_and_statement_traversal() {
    let r = residual(&ts::workspace("", "", "(if (> t -100.) (let* [f (fn [x] (+ x 1.)) n (if (> t 0.) (f t) 0.)] (+ n (f t))) t)"));
    let core = lower(r);
    let mut last_call = None;
    C::iter(
        &mut |s: &C::Statement| {
            if let C::StatementNode::Assign(n, C::Expr::Call_index(_)) = &s.node { last_call = Some(*n); }
        },
        &core.body,
    );
    let index = last_call.expect("a call index");
    let counts = C::Program { results: vec![index], ..core.clone() };
    for (time, count) in [(1., 1), (-1., 0)] {
        let live = input::at_time(time);
        assert!(ok(ts::run_simple(&counts, &live)) == Value::Int(count));
        assert!(ok(ts::run_simple(&counts, &live)) == Value::Int(count));
    }
    let template = core.body[0].clone();
    let stmt = |id: i64, node: C::StatementNode| C::Statement { id, node, ..template.clone() };
    let nested = stmt(
        100,
        C::StatementNode::Protect {
            body: vec![stmt(
                101,
                C::StatementNode::If {
                    condition: 0,
                    yes: vec![stmt(
                        102,
                        C::StatementNode::Counted {
                            first: 1,
                            last: 2,
                            count: 3,
                            index: 4,
                            item: 5,
                            limit: 8,
                            stop: Some(6),
                            init: vec![leaf(&template, 103)],
                            body: vec![leaf(&template, 104)],
                        },
                    )],
                    no: vec![],
                },
            )],
            value: 6,
            failure: 7,
            handler: vec![stmt(
                105,
                C::StatementNode::Diagnose_rows {
                    rows: 8,
                    policy: W::DiagnosticPolicy::First,
                    index: 9,
                    handler: vec![leaf(&template, 106)],
                    diagnostic: 10,
                    result: 11,
                },
            )],
            mapped: 12,
            result: 13,
        },
    );
    let mut visited = Vec::new();
    C::iter(&mut |s: &C::Statement| visited.push(s.id), std::slice::from_ref(&nested));
    assert_eq!(visited, vec![100, 101, 102, 103, 104, 105, 106]);
    let assign = stmt(110, C::StatementNode::Assign(110, C::Expr::Local(111)));
    assert_eq!(C::reads(&assign), vec![111]);
    assert_eq!(C::writes(&assign), vec![110]);
    let image = stmt(111, C::StatementNode::Draw(C::Draw::Image { source: (Ty::Named("image".into()), 7), at: 1, scale: 2, angle: 3 }));
    assert_eq!(C::reads(&image), vec![1, 2, 3]);
    let outer = C::children(&nested);
    let inner = C::children(outer[0]);
    assert_eq!(C::reads(inner[0]), vec![1, 2, 6]);
    let workspace = checked("(workspace traversal (graph g :context value (+ (* t 2.) 1.)))");
    let term = &workspace.graphs[0].body;
    let mut builder = flow_check::term_arena::TermBuilder::new();
    let mapped = builder.rewrite_roots_with(std::slice::from_ref(term), &mut |builder, source, draft| {
        let id = builder.rebuild(source, draft);
        if source.identity().0 == term.identity().0 && term.children().contains(&source.row_id()) {
            builder.with_type(id, &Ty::Bool)
        } else { id }
    })[0];
    let mapped = builder.freeze().handle(mapped);
    assert!(mapped.ty() == term.ty());
    assert_eq!(mapped.children().iter().map(|id| mapped.child(*id).ty().clone()).collect::<Vec<_>>(), vec![Ty::Bool, Ty::Bool]);
    let child = term.child(term.children()[0]);
    let grandchildren = child.children();
    assert!(grandchildren.len() == 2 && *child.child(grandchildren[0]).ty() == Ty::Float, "Workspace.map_children changed a grandchild");
}

#[test]
fn bypass_returns_the_unevaluated_time() {
    for body in ["^:bypass (+ t 5.)", "^:bypass (+ t (exp 10000.))", "^:bypass (test/offset :offset (exp 10000.) t)"] {
        let r = residual(&ts::workspace("", "", body));
        let core = lower(r);
        let program = ok(executor::compile(&Value::Residual(r), &Default::default()));
        for time in [0., 0.37, 2.] {
            let live = input::at_time(time);
            assert!(ok(executor::force(&program, &Default::default(), &live)) == Value::Float(time));
            assert!(ok(ts::run_simple(&core, &live)) == Value::Float(time));
        }
    }
}

#[test]
fn instruction_guard_layers_preserve_order_and_borrow_the_body() {
    use C::region::Instruction as I;
    let instruction = I::Guarded(vec![(5, true)], Box::new(I::Guarded(vec![], Box::new(
        I::Guarded(vec![(2, false), (7, true)], Box::new(I::Const(2.0))),
    ))));
    let (guards, body) = instruction.guarded();
    assert_eq!(guards, [(5, true), (2, false), (7, true)]);
    assert_eq!(*body, I::Const(2.0));
    assert!(std::ptr::eq(body, instruction.unguarded()));
    assert_eq!(body.guarded(), (Vec::new(), body));
}
