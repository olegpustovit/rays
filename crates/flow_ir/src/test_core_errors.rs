// Port of lib/flow_ir/test_core_errors.ml.
use crate::core_exec::{self, Options};
use crate::core_lower;
use crate::flow_ir::executor;
use crate::test_support::{self as ts, ok};
use flow_check::op;
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::context;
use flow_syntax::syntax;
use flow_syntax::value::{ResidualId, Value, source_value};
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

fn reference(value: &Value, live: &Frame) -> Result<Value, Diagnostic> { E::host::force_reference(value, live, None, None, None) }

fn live_frame(frame: i64) -> Frame { Frame { frame, ..input::at_time(frame as f64 * 0.25) } }

#[test]
fn core_loops_observe_cancellation_without_restarting_the_walker() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    for body in [
        "(fold [s \"\"] [i (range 4096)] (str s (if (< t 0.) \"a\" \"b\")))",
        "(fold [s \"\"] [i (array/range 4096 :type int)] (str s (if (< t 0.) \"a\" \"b\")))",
        "(iterate 4096 \"\" (fn [(s : text)] (str s (if (< t 0.) \"a\" \"b\"))) :until (fn [before after] false))",
    ] {
        let Value::Residual(root) = recorded(body) else { panic!("live loop expected") };
        let core = core_lower::lower(None,None,false,None,None,root).expect("core loop");
        assert!(core.regions.is_empty(),"the regression must exercise core loops, not packed kernels");
        let prepared = core_exec::prepare(&core,false,false,&Default::default(),false);
        let walked = Arc::new(AtomicUsize::new(0));
        let walks = walked.clone();
        let options = Options {root:Some(root),on_walk:Some(Arc::new(move || {walks.fetch_add(1,Ordering::Relaxed);})),..Default::default()};
        let probes = Arc::new(AtomicUsize::new(0));
        let calls = probes.clone();
        assert!(!E::with_interrupt(Arc::new(move || calls.fetch_add(1,Ordering::Relaxed)>=2),&mut || {
            assert_eq!(core_exec::execute(&prepared,&input::at_time(1.),&options).unwrap_err().code,"cancelled");
        }));
        assert_eq!(walked.load(Ordering::Relaxed),0);
        assert_eq!(core_exec::execute(&prepared,&input::at_time(1.),&options).unwrap(),Value::Text(vec![b'b';4096].into()));
    }
}

fn state_values(state: &E::State) -> Value { Value::List(Arc::new(E::host::state_values(state))) }

fn compile_strict(value: &Value) -> crate::flow_ir::executor::Program {
    ok(executor::compile(value, &executor::CompileOptions { strict: true, ..Default::default() }))
}

#[test]
fn strict_validation_distinguishes_equal_residual_numbers_in_different_homes() {
    let rewrite = |value: Value| {
        let next = std::cell::Cell::new(-8_440_001i64);
        E::host::rewrite_with(&value, &|_| None, &|| {
            let id = next.get();
            next.set(id - 1);
            id
        }, &|_, _| None)
    };
    let good = rewrite(recorded("(map (fn [x] (+ x t)) (array/range 16))"));
    let bad = recorded(r#"(map (fn [x] (if (> t 0.) (error "E_DYNAMIC" (str x)) x)) (array/range 16))"#);
    let options = executor::CompileOptions { strict: true, ..Default::default() };
    assert!(matches!(executor::compile(&bad, &options), Err(d) if d.code == "E_KERNEL_FORM"), "the uncached fixture must refuse");
    let bad = rewrite(bad);
    let (Value::Residual(a), Value::Residual(b)) = (&good, &bad) else { panic!("live maps must be residuals"); };
    assert_eq!(a.0, b.0);
    assert_ne!(a.1, b.1);
    assert!(executor::compile(&good, &options).is_ok());
    for value in [&bad, &Value::Record(vec![("good".into(), good.clone()), ("bad".into(), bad.clone())])] {
        match executor::compile(value, &options) {
            Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
            Ok(_) => panic!("a different home's cached or visited residual hid the dynamic error"),
        }
    }
    assert!(executor::compile(&good, &options).is_ok(), "a refusal in another home must not poison valid compilation");
    let other = rewrite(recorded("(map (fn [x] (* x t)) (array/range 16))"));
    let root = *a;
    let both = Value::Record(vec![("sum".into(), good), ("product".into(), other)]);
    let program = ok(executor::compile(&both, &options));
    let core = core_lower::lower(Some(both.clone()), None, true, None, None, root).expect("mixed-home core");
    let prepared = core_exec::prepare(&core, false, false, &W::Paths::new(), false);
    for time in [0.5, 2.0] {
        let live = input::at_time(time);
        let actual = executor::force(&program, &executor::ForceOptions { strict: true, ..Default::default() }, &live);
        let expected = reference(&both, &live);
        assert!(same_result(&actual, &expected), "core lowering differs at t={time}: actual {actual:?}, expected {expected:?}");
        let direct = core_exec::execute(&prepared, &live, &Options { strict: true, rerun_on_error: false, ..Default::default() });
        assert!(same_result(&direct, &expected), "direct core differs at t={time}: actual {direct:?}, expected {expected:?}");
    }
    let forms = ok(syntax::parse("(workspace w (graph g :context host [(a : float 0) (b : float 0)] (map (fn [x] (+ x (+ a b))) (array/range 16))))"));
    let (workspace, diagnostics) = W::check(&[], false, false, None, &ts::catalog(), &forms);
    assert!(diagnostics.is_empty(), "{}", ts::show_ds(&diagnostics));
    let inputs = vec![("g".into(), vec![("a".into(), rewrite(recorded("(+ t 1)"))), ("b".into(), rewrite(recorded("(* t 2)")))])];
    let workspace = workspace.unwrap();
    let evaluation = ok(E::static_with(&workspace, false, Some(&inputs), None));
    let value = &evaluation.results[0].1;
    let program = ok(executor::compile(value, &options));
    let packed = executor::packed_region(&program).expect("direct packed map with mixed-home uniforms");
    assert_eq!(packed.uniforms.len(), 2, "both foreign scalar captures must reach host preparation");
    for time in [0.5, 2.0] {
        let live = input::at_time(time);
        let actual = crate::packed::force(&packed, &live, &Default::default());
        let expected = reference(value, &live);
        assert!(same_result(&actual, &expected), "packed uniforms differ at t={time}: actual {actual:?}, expected {expected:?}");
        let host = ok(crate::packed::host::prepare_values(&packed, &Default::default(), &live));
        for (uniform, actual) in packed.uniforms.iter().zip(host.uniforms.iter()) {
            let expected = ok(reference(uniform, &live));
            assert_eq!(*actual, expected, "host uniform differs at t={time}");
        }
    }
    let forms = ok(syntax::parse("(workspace w (graph g :context value [(x : float 0)] (+ x t)))"));
    let (scalar, diagnostics) = W::check(&[], false, false, None, &ts::catalog(), &forms);
    assert!(diagnostics.is_empty(), "{}", ts::show_ds(&diagnostics));
    let scalar = scalar.unwrap();
    let argument = |index| {
        let marker = source_value("$core/argument", flow_syntax::ty::Ty::Float, vec![("index".into(), Value::Int(index))]);
        let inputs = vec![("g".into(), vec![("x".into(), marker)])];
        rewrite(ok(E::static_with(&scalar, false, Some(&inputs), None)).results[0].1.clone())
    };
    let inputs = vec![("g".into(), vec![("a".into(), argument(0)), ("b".into(), argument(1))])];
    let evaluation = ok(E::static_with(&workspace, false, Some(&inputs), None));
    let program = ok(executor::compile(&evaluation.results[0].1, &Default::default()));
    let packed = executor::packed_region(&program).expect("packed map with mixed-home argument captures");
    assert_eq!(ok(crate::packed::host::argument_types(&packed)), vec![flow_syntax::ty::Ty::Float; 2], "both homes' argument declarations must be visited");
    let bound = ok(crate::packed::host::bind_arguments(&packed, &[Value::Float(2.0), Value::Float(5.0)], None));
    for time in [0.5, 2.0] {
        let actual = ok(crate::packed::force(&bound, &input::at_time(time), &Default::default()));
        let expected = Value::Floats(1, Arc::new((0..16).map(|x| x as f64 + 7.0 + 2.0 * time).collect()));
        assert_eq!(actual, expected, "binding both homes' arguments changed their values");
    }
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
        let compiled = ok(executor::compile(&value, &Default::default()));
        for time in times {
            let live = input::at_time(time);
            let expected = reference(&value, &live);
            assert!(expected.is_err());
            let plain = core_exec::execute(&prepared, &live, &Options::default());
            assert!(plain.is_err(), "{name}: the core should fail");
            let strict = core_exec::execute(&prepared, &live, &Options { root: Some(r), strict: true, ..Default::default() });
            assert!(same_result(&strict, &plain));
            let actual = executor::force(&compiled, &Default::default(), &live);
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
    let compiled = ok(executor::compile(&value, &Default::default()));
    for frame in 0..8 {
        let live = live_frame(frame);
        let core_before = E::state_stamp(&core);
        let reference_before = E::state_stamp(&reference_state);
        let expected = E::host::force_reference(&value, &live, Some(&reference_state), None, None);
        let (actual, _, walks) = executor::private::force_with_walker_counts(&compiled, &executor::ForceOptions { state: Some(core.clone()), ..Default::default() }, &live);
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
    let mut o = op::find(&[], "frame/dt", &context::value()).expect("frame/dt").clone();
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
    let program = ok(executor::compile(&shared, &Default::default()));
    let live = input::at_time(0.25);
    let (actual, attempts, walks) = executor::private::force_with_walker_counts(&program, &Default::default(), &live);
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
        let expected = E::host::force_reference(&value, &live, Some(&reference_state), None, None);
        let actual = executor::force(&compiled, &executor::ForceOptions { strict: true, state: Some(strict.clone()), ..Default::default() }, &live);
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
        check(executor::force(&compiled, &executor::ForceOptions { strict: true, ..Default::default() }, &live));
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
                    match (executor::force(&compiled, &executor::ForceOptions { strict: true, ..Default::default() }, &live), &expected) {
                        (Ok(actual), Ok(expected)) => assert!(&actual == expected),
                        (Err(actual), Err(expected)) => assert!(actual.code == expected.code && actual.message == expected.message && actual.cause == expected.cause),
                        _ => panic!("packed error parity"),
                    }
                });
            }
        }
    }
    let dynamic = recorded(r##"(map (fn [x] (if (> t 0.) (error "E_DYNAMIC" (str x)) x)) (array/range 5001))"##);
    match executor::compile(&dynamic, &executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) if d.code == "E_KERNEL_FORM" => (),
        Err(d) => panic!("dynamic packed refusal returned {}", rays_diagnostic::to_string(&d)),
        Ok(_) => panic!("dynamic packed refusal was accepted"),
    }
    assert!(reference(&dynamic, &input::at_time(-1.)).is_ok());
}
