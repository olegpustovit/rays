// Port of lib/flow_ir/test_core_errors.ml.
use crate::core_exec::{self, Options};
use crate::core_lower;
use crate::flow_ir::Executor;
use crate::test_support::{self as ts, ok};
use flow_check::op;
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::context;
use flow_syntax::syntax;
use flow_syntax::value::{ResidualId, Value};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

fn workspace(body: &str) -> W::Workspace {
    let source = format!("(workspace w (graph g :context value (let* [tested {body}] 0.)))");
    let forms = ok(syntax::parse(&source));
    match W::check(&[], false, false, None, &ts::catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{body}: {}", ts::show_ds(&ds)),
    }
}

fn same_bits(a: f64, b: f64) -> bool { a.to_bits() == b.to_bits() }

fn same_result(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(Value::Float(a)), Ok(Value::Float(b))) => same_bits(*a, *b),
        (Ok(a), Ok(b)) => a == b,
        (Err(a), Err(b)) => a == b,
        _ => false,
    }
}

fn recorded(body: &str) -> Value {
    let evaluation = ok(E::r#static(&workspace(body), true, None));
    let path = vec!["g".to_owned(), "tested".to_owned()];
    evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1.clone()
}

fn reference(value: &Value, live: &Frame) -> Result<Value, Diagnostic> { E::Host::force_reference(value, live, None, None, None) }

fn live_frame(frame: i64) -> Frame { Frame { frame, ..input::at_time(frame as f64 * 0.25) } }

fn state_values(state: &E::State) -> Value { Value::List(Arc::new(E::Host::state_values(state))) }

fn compile_strict(value: &Value) -> crate::flow_ir::Executor::Program {
    ok(Executor::compile(value, &Executor::CompileOptions { strict: true, ..Default::default() }))
}

// The core itself fails on these, and the executor must answer with the walker's own
// diagnostic (message and span) rather than a core one.
fn failing() -> Vec<(&'static str, &'static str, Vec<f64>)> {
    vec![
        ("division by zero", "(int/div (int (+ 10 t)) (int (* t 0)))", vec![1., 2.]),
        ("out-of-range nth", "(nth (list t 1 2) (+ 5 (int t)))", vec![0., 1.5]),
        ("non-finite result", "(exp (* t 100000.))", vec![1., 3.]),
        ("list slice", "(first (take (int t) (list t 1)))", vec![0., 0.5]),
        ("authored error", r##"(error (str "E_RULES_" (int t)) (str "exact é日本😀 " t))"##, vec![1., 2.]),
        ("authored cause", r##"(error "E_CAUSE" "selected failure" :cause (str "reason é日本😀 " t))"##, vec![1., 2.]),
    ]
}

#[test]
fn core_failures_answer_with_the_walkers_diagnostic() {
    for (name, body, times) in failing() {
        let value = recorded(body);
        let Value::Residual(r) = value else { panic!("{name}: not live") };
        let program = core_lower::lower(None, None, false, None, None, r).unwrap_or_else(|n| panic!("{name}: refused {n}"));
        let prepared = core_exec::prepare(&program, false, false, &Default::default(), false);
        let compiled = ok(Executor::compile(&value, &Default::default()));
        for time in times {
            let live = input::at_time(time);
            let expected = reference(&value, &live);
            assert!(expected.is_err());
            let plain = core_exec::execute(&prepared, &live, &Options::default());
            assert!(plain.is_err(), "{name}: the core should fail");
            let strict = core_exec::execute(&prepared, &live, &Options { root: Some(r), strict: true, ..Default::default() });
            assert!(same_result(&strict, &plain));
            let actual = Executor::force(&compiled, &Default::default(), &live);
            assert!(same_result(&actual, &expected), "{name} at {time}: diagnostic differs from the walker");
        }
    }
}

#[test]
fn byte_error_operands_keep_native_diagnostic_representation() {
    for (code, message, cause) in [
        (b"E_BYTES".as_slice(), b"selected".as_slice(), b"raw\xff\0cause".as_slice()),
        (b"E_\xff".as_slice(), b"selected".as_slice(), b"cause".as_slice()),
        (b"E_BYTES".as_slice(), b"raw\xffmessage".as_slice(), b"cause".as_slice()),
    ] {
        let body = format!("(if (> t 0.) (error {} {} :cause {}) 0.)", ts::text_literal(code), ts::text_literal(message), ts::text_literal(cause));
        let value = recorded(&body);
        let Value::Residual(r) = &value else { panic!("expected live error") };
        let program = core_lower::lower(None, None, true, None, None, *r).expect("byte error lowers");
        let prepared = core_exec::prepare(&program, false, false, &Default::default(), false);
        for time in [-0.25, 0.25] {
            let live = input::at_time(time);
            let expected = reference(&value, &live);
            let actual = core_exec::execute(&prepared, &live, &Options { strict: true, rerun_on_error: false, ..Default::default() });
            match (actual, expected) {
                (Ok(a), Ok(b)) => assert_eq!(a, b),
                (Err(a), Err(b)) => {
                    assert_eq!(a.code, b.code);
                    assert_eq!(a.message, b.message);
                    assert_eq!(a.cause, b.cause);
                }
                (a, b) => panic!("native byte error mismatch: {a:?} / {b:?}"),
            }
        }
    }
}

fn state_value() -> Value {
    let evaluation = ok(E::static_eval(&workspace(
        r##"(state [s 0.] (if (> t 0.6) (error "E_RULES" (str "exact failure " t) :cause "state rollback") (+ s 1.)))"##,
    )));
    evaluation.states[0].clone()
}

#[test]
fn a_failure_after_a_state_write_restores_the_plane() {
    let value = state_value();
    let reference_state = E::create_state();
    let core = E::create_state();
    let mut failures = 0;
    let compiled = ok(Executor::compile(&value, &Default::default()));
    for frame in 0..8 {
        let live = live_frame(frame);
        let core_before = E::state_stamp(&core);
        let reference_before = E::state_stamp(&reference_state);
        let expected = E::Host::force_reference(&value, &live, Some(&reference_state), None, None);
        let (actual, _, walks) = Executor::Private::force_with_walker_counts(&compiled, &Executor::ForceOptions { state: Some(core.clone()), ..Default::default() }, &live);
        if expected.is_err() { failures += 1; }
        assert!(same_result(&actual, &expected), "state frame {frame}");
        if expected.is_err() {
            assert!(E::state_stamp(&core) == core_before && E::state_stamp(&reference_state) == reference_before);
            assert_eq!(walks, 1);
        } else {
            assert_eq!(walks, 0);
        }
        assert!(same_result(&Ok(state_values(&core)), &Ok(state_values(&reference_state))));
    }
    assert!(failures > 0);
}

fn opaque_op() -> op::Op {
    let mut o = op::find(&[], "frame/dt", &context::value()).expect("frame/dt");
    o.name = "test/opaque".into();
    o.live = true;
    o.arithmetic = None;
    o.packed_extension = None;
    o.body = Arc::new(|live: &Frame, _node, _args| Ok(Value::Float(live.t + 1.)));
    o
}

#[test]
fn a_refused_extension_op_is_interpreted_once_and_shared() {
    let forms = ok(syntax::parse("(workspace w (graph g :context value (test/opaque)))"));
    let ws = match W::check(&[opaque_op()], false, false, None, &ts::catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    };
    let evaluation = ok(E::static_eval(&ws));
    let residual: ResidualId = match &evaluation.results.iter().find(|(n, _)| n == "g").unwrap().1 {
        Value::Residual(r) => *r,
        _ => panic!("opaque result was not residual"),
    };
    assert_eq!(core_lower::lower(None, None, false, None, None, residual).err().as_deref(), Some("test/opaque"));
    let shared = Value::List(Arc::new(vec![Value::Residual(residual), Value::List(Arc::new(vec![Value::Residual(residual)]))]));
    let program = ok(Executor::compile(&shared, &Default::default()));
    let live = input::at_time(0.25);
    let (actual, attempts, walks) = Executor::Private::force_with_walker_counts(&program, &Default::default(), &live);
    assert!(actual == reference(&shared, &live));
    assert!(attempts == 1 && walks == 1);
}

#[test]
fn strict_state_failures_keep_their_exact_diagnostic_and_rollback() {
    let value = state_value();
    let reference_state = E::create_state();
    let strict = E::create_state();
    let compiled = compile_strict(&value);
    for frame in 0..8 {
        let live = live_frame(frame);
        let reference_before = E::state_stamp(&reference_state);
        let strict_before = E::state_stamp(&strict);
        let expected = E::Host::force_reference(&value, &live, Some(&reference_state), None, None);
        let actual = Executor::force(&compiled, &Executor::ForceOptions { strict: true, state: Some(strict.clone()), ..Default::default() }, &live);
        match (&actual, &expected) {
            (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
            (a, b) => assert!(same_result(a, b)),
        }
        if expected.is_err() {
            assert!(E::state_stamp(&strict) == strict_before && E::state_stamp(&reference_state) == reference_before);
        }
        assert!(same_result(&Ok(state_values(&strict)), &Ok(state_values(&reference_state))));
    }
}

#[test]
fn authored_errors_keep_exact_dynamic_messages() {
    let value = recorded(r##"(if (> t 0.) (error (str "E_TEXT_" (int t)) (str "é日本😀 " t) :cause (str "reason " t)) "safe")"##);
    let Value::Residual(residual) = value else { panic!("residual") };
    let core = core_lower::lower(None, None, true, None, None, residual).expect("strict lowering");
    let compiled = compile_strict(&value);
    for time in [-1., 1., 2.] {
        let live = input::at_time(time);
        let expected = reference(&value, &live);
        let check = |actual: Result<Value, Diagnostic>| match actual {
            Ok(actual) => assert!(expected == Ok(actual)),
            Err(d) => match &expected {
                Err(e) => assert!(d.code == e.code && d.message == e.message && d.cause == e.cause),
                _ => panic!("unexpected error"),
            },
        };
        check(ts::run_simple(&core, &live));
        check(Executor::force(&compiled, &Executor::ForceOptions { strict: true, ..Default::default() }, &live));
    }
    match E::static_eval(&workspace(r##"(error "E_LITERAL" "literal failure" :cause "literal cause")"##)) {
        Err(d) => assert!(d.code == "E_LITERAL" && d.cause.as_deref() == Some(b"literal cause") && d.span.is_some()),
        Ok(_) => panic!("literal error was accepted"),
    }
    for count in [0, 5001] {
        let value = recorded(&format!(
            r##"(map (fn [x] (if (> t 0.) (error "E_PACKED_RULE" "exact é日本😀" :cause "selected packed cause") x)) (array/range {count}))"##
        ));
        let compiled = compile_strict(&value);
        for time in [-1., 1.] {
            let live = input::at_time(time);
            let expected = reference(&value, &live);
            for domains in [1, 8] {
                ts::with_domains(domains, &|| {
                    match (Executor::force(&compiled, &Executor::ForceOptions { strict: true, ..Default::default() }, &live), &expected) {
                        (Ok(actual), Ok(expected)) => assert!(&actual == expected),
                        (Err(actual), Err(expected)) => assert!(actual.code == expected.code && actual.message == expected.message && actual.cause == expected.cause),
                        _ => panic!("packed error parity"),
                    }
                });
            }
        }
    }
    let dynamic = recorded(r##"(map (fn [x] (if (> t 0.) (error "E_DYNAMIC" (str x)) x)) (array/range 5001))"##);
    match Executor::compile(&dynamic, &Executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) if d.code == "E_KERNEL_FORM" => (),
        Err(d) => panic!("dynamic packed refusal returned {}", rays_diagnostic::to_string(&d)),
        Ok(_) => panic!("dynamic packed refusal was accepted"),
    }
    assert!(reference(&dynamic, &input::at_time(-1.)).is_ok());
}
