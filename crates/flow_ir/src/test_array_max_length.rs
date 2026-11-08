// Port of lib/flow_ir/test_array_max_length.ml.
use crate::packed;
use crate::test_support::*;
use flow_check::op::{self, MAX_WOSIZE};
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::value::{self as v, Value};
use flow_syntax::{context, syntax, ty};
use flow_syntax::ty::Ty;

fn source(body: &str) -> Vec<syntax::Syntax> {
    ok(syntax::parse(&format!("(workspace capacity (graph g :context host {body}))")))
}
fn checked(body: &str) -> w::Workspace {
    match w::check(&[], false, false, None, &catalog(), &source(body)) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}
fn value(body: &str) -> Value {
    ok(e::static_eval(&checked(body))).results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
}

fn limits() -> Vec<(&'static str, i64)> {
    let m = MAX_WOSIZE as i64;
    vec![("int", m), ("text", m), ("float", m), ("vec2", m / 2), ("vec3", m / 3), ("vec4", m / 4)]
}
fn literal(kind: &str) -> String {
    format!("(array/max_length :type {kind})")
}
fn failure(code: &str, r: Result<Value, rays_diagnostic::Diagnostic>) {
    match r {
        Err(d) => assert_eq!(d.code, code),
        Ok(_) => panic!("expected {code}"),
    }
}
fn is_ds_code(ds: &[rays_diagnostic::Diagnostic], code: &str) -> bool {
    ds.iter().any(|d| d.code == code)
}

#[test]
fn op_direct_and_literal_selectors() {
    let op = op::find(&[], "array/max_length", &context::value()).unwrap();
    let direct = |args: Vec<(String, Value)>| (op.body)(&input::at_time(0.), &|_, _| panic!("node"), &args);
    assert!(op.signature.pos.is_empty() && op.signature.kw == vec![("type".to_string(), Ty::Text)]);
    let m = MAX_WOSIZE as i64;
    assert_eq!(direct(vec![]).unwrap(), Value::Int(m));
    assert_eq!(value("(array/max_length)"), Value::Int(m));
    assert_eq!(op::result_type(&op, &[]).unwrap(), Ty::Int);
    for (kind, limit) in limits() {
        assert_eq!(direct(vec![("type".into(), Value::Text(kind.as_bytes().to_vec()))]).unwrap(), Value::Int(limit));
        assert_eq!(value(&literal(kind)), Value::Int(limit));
        assert_eq!(op::result_type(&op, &[("type".into(), Ty::Text, Some(param::Value::Text(kind.as_bytes().to_vec())))]).unwrap(), Ty::Int);
        let body = format!("(if (> t 0.) {} (int/sub {} 1))", literal(kind), literal(kind));
        let val = value(&body);
        let r = residual(&val);
        let core = lower_strict(r).unwrap_or_else(|f| panic!("{f}"));
        let compiled = executor_compile(&val, true);
        for time in [-1., 1.] {
            let live = live_at(time);
            let expected = ok(force_reference(&val, &live));
            assert_eq!(expected, Value::Int(if time > 0. { limit } else { limit - 1 }));
            assert_eq!(ok(run(Some(r), true, None, None, &core, &live)), expected);
            for domains in [1, 4, 8] {
                with_domains(domains, &|| assert_eq!(ok(executor_force(&compiled, true, &live)), expected));
            }
        }
        // Symbolic cap-minus-extra validation; no constructor or huge allocation.
        for extra in [0, 1] {
            let maximum = limit - extra;
            let guard = |count: i64| {
                format!("(let* [maximum (int/sub {} {extra})] (if (int/le {count} maximum) \"ok\" (error \"E_CAPACITY\" \"count exceeds element capacity\")))", literal(kind))
            };
            assert_eq!(value(&guard(maximum)), Value::Text(b"ok".to_vec()));
            match e::static_eval(&checked(&guard(maximum + 1))) {
                Err(d) => assert!(d.code == "E_CAPACITY" && d.message == "count exceeds element capacity"),
                Ok(_) => panic!("guard should fail"),
            }
        }
    }
    failure("E_ARRAY_TYPE", direct(vec![("type".into(), Value::Text(b"unknown".to_vec()))]));
    match op::result_type(&op, &[("type".into(), Ty::Text, None)]) {
        Err(d) => assert_eq!(d.code, "E_ARRAY_TYPE"),
        Ok(_) => panic!("expected E_ARRAY_TYPE"),
    }
    for (body, code) in [
        ("(array/max_length :type \"unknown\")", "E_ARRAY_TYPE"),
        ("(array/max_length :type 1)", "E_TYPE"),
        ("(array/max_length :type (if (> t 0.) \"int\" \"float\"))", "E_ARRAY_TYPE"),
        ("(array/max_length 1)", "E_ARITY"),
    ] {
        match w::check(&[], false, false, None, &catalog(), &source(body)) {
            (None, ds) => assert!(is_ds_code(&ds, code), "{body}: {}", show_ds(&ds)),
            _ => panic!("{body} should not check"),
        }
    }
}

fn compile_packed(r: v::ResidualId) -> packed::T {
    let term = e::Residual::residual_view(r).unwrap().term;
    packed::compile_result(&Default::default(), r, &term).unwrap_or_else(|ds| panic!("{}", show_ds(&ds)))
}

#[test]
fn exact_int_kernels_keep_gpu_refusal() {
    // Exact Int kernels retain ordinary GPU refusal, even when Int work feeds Float.
    for body in [
        "(map (fn [(i : int)] (int/sub (array/max_length :type int) (int/add i (frame/index)))) (array/range 5001 :type int))",
        "(map (fn [(i : int)] (float (int/sub (array/max_length :type int) (int/add i (frame/index))))) (array/range 5001 :type int))",
    ] {
        let val = value(body);
        let p = compile_packed(residual(&val));
        assert!(!packed::gpu_refusals(false, false, &p).is_empty());
        let compiled = executor_compile(&val, true);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                let live = live_frame(1., 3);
                assert_eq!(ok(executor_force(&compiled, true, &live)), ok(force_reference(&val, &live)));
            });
        }
    }
}

#[test]
fn statically_folded_float_conversion_has_no_int_bank() {
    // An explicit, statically folded Float conversion has no retained Int bank.
    let val = value("(map (fn [x] (+ (+ x t) (float (array/max_length)))) (array/range 5001))");
    let p = compile_packed(residual(&val));
    assert!(packed::gpu_refusals(false, false, &p).is_empty());
    let compiled = executor_compile(&val, true);
    for domains in [1, 4, 8] {
        with_domains(domains, &|| {
            let live = live_at(1.);
            assert_eq!(ok(executor_force(&compiled, true, &live)), ok(force_reference(&val, &live)));
        });
    }
}

// Product defect: v::array_init (flow_syntax value.rs) does not reject count > native bound before
// Vec::with_capacity, so limit+1 aborts the process with "memory allocation of ... bytes failed".
// OCaml raises E_ARRAY_RANGE (Flow.Value.array_init ty (limit + 1)).
#[test]
fn array_init_past_the_native_bound_is_a_range_error() {
    for (kind, limit) in limits() {
        let element = ty::of_string(kind).unwrap();
        failure("E_ARRAY_RANGE", v::array_init(&element, limit + 1, &|_| panic!("element built")));
    }
}

// Product defect: array/max_length's body (flow_check op.rs array_ops) reads :type with text_string,
// so a non-text argument fails E_TYPE; OCaml reports E_ARRAY_TYPE for Int, Bool and Text_array.
#[test]
fn direct_non_text_type_is_array_type() {
    let op = op::find(&[], "array/max_length", &context::value()).unwrap();
    for arg in [Value::Int(1), Value::Bool(true), Value::Text_array(std::sync::Arc::new(vec![b"float".to_vec()].into()))] {
        failure("E_ARRAY_TYPE", (op.body)(&input::at_time(0.), &|_, _| panic!("node"), &vec![("type".into(), arg)]));
    }
}
