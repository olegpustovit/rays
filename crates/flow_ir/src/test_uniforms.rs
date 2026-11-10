// Count sweeps include 0/1/1023/1024/1025/2049 and the full chunk case while keeping individual debug cases within budget.
use crate::core_ir::region::Instruction;
use crate::flow_ir::executor;
use crate::packed::{self, host};
use crate::test_support::*;
use flow_check::op::{self, Op};
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::packed_ops::Binary;
use flow_syntax::syntax;
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::{self as v, Value, struct_value};
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(a), Ok(b)) => bits(a) == bits(b),
        (Err(a), Err(b)) => rays_diagnostic::to_string(a) == rays_diagnostic::to_string(b),
        _ => false,
    }
}
fn show(r: &Result<Value, Diagnostic>) -> String {
    match r {
        Ok(v) => format!("{v:?}"),
        Err(d) => rays_diagnostic::to_string(d),
    }
}
fn components(v: &Value) -> Vec<f64> {
    match v {
        Value::Float(x) => vec![*x],
        Value::Vec2(x, y) => vec![*x, *y],
        Value::Vec3(x, y, z) => vec![*x, *y, *z],
        Value::Vec4(x, y, z, w) => vec![*x, *y, *z, *w],
        _ => panic!("components"),
    }
}
fn width(ty: &Ty) -> usize {
    match ty {
        Ty::Float => 1,
        Ty::Vec2 => 2,
        Ty::Vec3 => 3,
        Ty::Vec4 => 4,
        _ => panic!("width"),
    }
}
fn float_bits(a: &[f64]) -> Vec<u64> {
    a.iter().map(|x| x.to_bits()).collect()
}

/// `probe/data`: a source extension returning a struct-typed array placeholder.
fn source(ty: &Ty) -> Op {
    let mut o = op::all().iter().find(|o| o.name == "noise3").expect("noise3").clone();
    o.name = "probe/data".into();
    o.packed_extension = None;
    o.signature = op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] };
    let array = Ty::Array(Box::new(ty.clone()));
    o.out = {
        let array = array.clone();
        Arc::new(move |_| Ok(array.clone()))
    };
    o.shape = op::Shape::Struct { splice: false };
    o.check = Arc::new(|_| Ok(()));
    o.body = Arc::new(move |_, _, _| Ok(struct_value("probe/data", array.clone(), vec![].into())));
    o
}

fn workspace(bindings: &str, ops: &[Op], body: &str) -> w::Workspace {
    let forms = ok(syntax::parse(&format!("(workspace w (graph g :context value (let* [{bindings} tested {body}] 0.0)))")));
    match w::check(ops, false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}
fn evaluate(ws: &w::Workspace) -> Value {
    let ev = ok(e::r#static(ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).expect("record");
    records[0].1.clone()
}
fn function_of(v: &Value) -> v::FnId {
    match v {
        Value::Fn(f) => *f,
        _ => panic!("expected a function"),
    }
}

struct Fixture {
    value: Value,
    sum: Value,
    packed: packed::T,
    // a function id lives as long as its evaluation's workspace is alive in this thread
    _keep: Vec<e::Eval>,
}

fn fixture(ty: &Ty, reducer: &str, over: Option<Op>, seed: Option<&str>) -> Fixture {
    let w_ = width(ty);
    let seed = match seed {
        Some(s) => s.to_string(),
        None if w_ == 1 => "-0.0".to_string(),
        None => format!("[{}]", vec!["-0.0"; w_].join(" ")),
    };
    let body = match w_ {
        1 => "[s uv.x uv.y t]",
        2 => "[s.x s.y uv.x t]",
        3 => "[s.x s.y s.z t]",
        _ => "[s.x s.y s.z s.w]",
    };
    let mut ws = workspace(&format!("data (probe/data) s (reduce {reducer} {seed} data)"), &[source(ty)], &format!("(fn [(uv : vec2)] {body})"));
    // Exercise the compiler's independent declaration guard even for an unchecked extension.
    if let Some(op) = over {
        std::sync::Arc::make_mut(&mut ws.ops).insert(0, op);
    }
    let ev = ok(e::r#static(&ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    let f = function_of(&records[0].1);
    let sum = e::residual::function_bindings(f).into_iter().find(|(n, _)| n == "s").expect("binding s").1;
    let uv = Value::Floats(2, Arc::new((0..2049 * 2).map(|i| (i % 17) as f64 / 16.).collect()));
    let value = ok(e::host::map_function(None, None, &FnSignature { params: vec![Ty::Vec2], result: Ty::Vec4 }, f, &[uv]));
    let r = residual(&value);
    let packed = compile_region(r, true, None).unwrap_or_else(|ds| panic!("uniform compile {} reducer={reducer} seed={seed}: {}", flow_syntax::ty::to_string(ty), show_ds(&ds)));
    Fixture { value, sum, packed, _keep: vec![ev] }
}

/// A measure that counts non-reference, non-collecting runs and checks `ordered_add`.
fn observed(fast: Option<bool>, counter: Arc<AtomicI64>) -> packed::Measure {
    packed::Measure {
        clock: Arc::new(|| 0.),
        report: Arc::new(move |p: &packed::T, _seconds, reference| {
            if !reference && !host::view(p).collecting {
                counter.fetch_add(1, Ordering::SeqCst);
                if let Some(expected) = fast {
                    assert_eq!(host::ordered_add(p), expected, "uniform ordered-add expected={expected}");
                }
            }
        }),
    }
}

fn cancellation(i: usize) -> f64 {
    match i % 5 {
        0 => 1e16,
        1 => 1.,
        2 => -1e16,
        3 => 1.,
        _ => -0.,
    }
}
fn data(ty: &Ty, count: usize, time: f64) -> Value {
    let w_ = width(ty);
    let values: Vec<f64> = (0..count * w_)
        .map(|i| {
            if w_ == 1 {
                if i == count - 1 { time } else { cancellation(i) }
            } else {
                match i % w_ {
                    0 => cancellation(i / w_),
                    1 => time,
                    2 => -0.,
                    _ => cancellation(i / w_) + time,
                }
            }
        })
        .collect();
    let a = Arc::new(values);
    match w_ {
        1 => Value::Floats(1, a),
        2 => Value::Floats(2, a),
        3 => Value::Floats(3, a),
        _ => Value::Floats(4, a),
    }
}
fn resolve_source(current: Arc<Mutex<Value>>) -> ResolveArc {
    Arc::new(move |v: &Value| match v {
        Value::Struct(g) if g.name == "probe/data" => Ok(current.lock().unwrap().clone()),
        _ => Err(v::fail("E_DATA_SOURCE", "Unexpected source.")),
    })
}

/// Recorded map calls are nested in this record. Tiny maps use the ordinary interpreter placement;
/// strict numeric bodies still require packed execution.
#[test]
fn tiny_maps_in_recorded_call_records() {
    let ws = workspace("", &[], "(fn [(x : float)] [(* (- x 6.0) 50.0) t 0.0])");
    let ev = ok(e::r#static(&ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    let f = function_of(&records[0].1);
    let call = ok(e::host::map_function(None, None, &FnSignature { params: vec![Ty::Float], result: Ty::Vec3 }, f, &[Value::Floats(1, Arc::new((0..13).map(|i| i as f64).collect()))]));
    let value = Value::Record(vec![("offset".into(), Value::Int(0)), ("call".into(), call)].into());
    for strict in [false, true] {
        let program = ok(executor::compile(&value, &executor::CompileOptions { strict, ..Default::default() }));
        for time in [0., 0.25] {
            let live = live_at(time);
            let expected = force_reference(&value, &live);
            for domains in [1, 8] {
                with_domains(domains, &|| {
                    let actual = executor_force(&program, strict, &live);
                    assert!(same(&actual, &expected), "recorded map mismatch: strict={strict} t={time} domains={domains} expected={} actual={}", show(&expected), show(&actual));
                });
            }
        }
    }
}

#[test]
fn one_force_lowers_129_residuals_once() {
    let ws = workspace("", &[], "(fn [(x : float)] [x t 0.0])");
    let ev = ok(e::r#static(&ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    let f = function_of(&records[0].1);
    let residuals: Vec<Value> = (0..129).map(|i| ok(e::host::map_function(None, None, &FnSignature { params: vec![Ty::Float], result: Ty::Vec3 }, f, &[Value::Floats(1, Arc::new(vec![i as f64]))]))).collect();
    let mut items = residuals.clone();
    items.push(residuals[0].clone());
    let value = Value::List(Arc::new(items));
    let program = ok(executor::compile(&value, &Default::default()));
    let live = live_at(0.25);
    let (actual, attempts) = executor::private::force_with_lower_attempts(&program, &Default::default(), &live);
    assert!(same(&actual, &force_reference(&value, &live)));
    assert_eq!(attempts, 129);
}

#[test]
fn strict_uniform_reductions() {
    for reducer in ["+", "min"] {
        for wrapped in [false, true] {
            for integer in [false, true] {
                let reduction = format!("(reduce {reducer} 0.0 data)");
                let uniform = if wrapped { format!("(+ t {reduction})") } else { reduction };
                let body = if integer { "(int (+ x u))" } else { "(+ x u)" };
                let ws = workspace(&format!("data (probe/data) u {uniform}"), &[source(&Ty::Float)], &format!("(fn [(x : float)] {body})"));
                let ev = ok(e::r#static(&ws, true, None));
                let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
                let f = function_of(&records[0].1);
                let value = ok(e::host::map_function(
                    None,
                    None,
                    &FnSignature { params: vec![Ty::Float], result: if integer { Ty::Int } else { Ty::Float } },
                    f,
                    &[Value::Floats(1, Arc::new((0..2049).map(|i| (i % 11) as f64).collect()))],
                ));
                let r = residual(&value);
                let p = packed::compile(&Default::default(), r, &e::residual::residual_view(r).unwrap().term).expect("packed region");
                let current = Arc::new(Mutex::new(Value::Floats(1, Arc::new(vec![1., 2., 3., 4.]))));
                let resolve = resolve_source(current);
                let live = live_at(0.25);
                let reference = reference_with(&value, &live, None, Some(resolve.clone()));
                for domains in [1, 8] {
                    with_domains(domains, &|| {
                        let actual = packed_force(&p, &live, true, Some(resolve.clone()));
                        let compiled = executor::compile(&value, &executor::CompileOptions { strict: true, ..Default::default() });
                        if reducer == "+" {
                            assert!(same(&actual, &reference), "strict uniform packed mismatch reducer={reducer} actual={} expected={}", show(&actual), show(&reference));
                            assert!(same(&exec_force(&ok(compiled), &live, None, Some(resolve.clone())), &reference));
                        } else {
                            match &actual {
                                Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
                                Ok(_) => panic!("Strict captured reduction used the interpreter"),
                            }
                            match &compiled {
                                Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
                                Ok(_) => panic!("Strict captured reduction used the interpreter"),
                            }
                        }
                    });
                }
            }
        }
    }
}

fn custom_op() -> Op {
    let mut o = op::find(&[], "+", &flow_syntax::context::value()).unwrap().clone();
    o.arithmetic = None;
    o.body = Arc::new(|_, _, args| match &args[..] {
        [(_, a), (_, b)] => Ok(Value::Float(v::num(a)? - v::num(b)?)),
        _ => panic!("custom body"),
    });
    o
}

/// (type, reducer, override, seed, packed_expected, fast)
type Case = (Ty, &'static str, bool, Option<&'static str>, bool, Option<bool>);
fn cases() -> Vec<Case> {
    vec![
        (Ty::Float, "+", false, None, true, Some(true)),
        (Ty::Float, "(fn [a x] (+ a x))", false, None, true, Some(true)),
        (Ty::Vec2, "+", false, None, true, Some(true)),
        (Ty::Vec3, "+", false, None, true, Some(true)),
        (Ty::Vec4, "+", false, None, true, Some(true)),
        (Ty::Vec3, "(fn [a x] (+ a x))", false, None, true, Some(true)),
        (Ty::Float, "(fn [a x] (+ (* a 0.99) x))", false, None, true, Some(false)),
        (Ty::Vec3, "(fn [a x] (+ x a))", false, None, true, Some(false)),
        (Ty::Float, "min", false, None, false, Some(false)),
        (Ty::Float, "+", true, None, false, Some(false)),
        (Ty::Float, "+", false, Some("0"), true, Some(true)),
    ]
}

fn uniform_case(index: usize) {
    let (ty, reducer, over, seed, packed_expected, fast) = cases().remove(index);
    let fx = fixture(&ty, reducer, if over { Some(custom_op()) } else { None }, seed);
    let counts: Vec<usize> = if ty == Ty::Vec3 && reducer == "+" { vec![0, 1, 1023, 1024, 1025, 2049] } else { vec![0, 2049] };
    for count in counts {
        for time in [0., 0.25, 0.5] {
            let current = Arc::new(Mutex::new(data(&ty, count, time)));
            let original = bits(&current.lock().unwrap());
            let live = {
                let mut l = live_at(time);
                l.frame = (time * 4.) as i64;
                l
            };
            let resolve = resolve_source(current.clone());
            let expected = reference_with(&fx.value, &live, None, Some(resolve.clone()));
            let scalar = ok(reference_with(&fx.sum, &live, None, Some(resolve.clone())));
            if seed == Some("0") {
                match (count, &scalar) {
                    (0, Value::Int(0)) => {}
                    (n, Value::Float(_)) if n > 0 => {}
                    _ => panic!("seed 0 result {scalar:?} at count {count}"),
                }
            }
            let uniform = match &scalar {
                Value::Int(n) => vec![*n as f64],
                other => components(other),
            };
            for domains in [1, 8] {
                with_domains(domains, &|| {
                    let cpu = Arc::new(AtomicI64::new(0));
                    let gpu = Arc::new(AtomicI64::new(0));
                    let forced = packed::force(&fx.packed, &live, &packed::ForceOptions { resolve: Some(resolve.clone()), measure: Some(observed(fast, cpu.clone())), ..Default::default() });
                    assert!(same(&forced, &expected));
                    let program = ok(executor::compile(&fx.sum, &Default::default()));
                    let actual = executor::force(&program, &executor::ForceOptions { resolve: Some(resolve.clone()), ..Default::default() }, &live);
                    assert!(same(&actual, &Ok(scalar.clone())), "uniform executor {:?} reducer={reducer} count={count} time={time} domains={domains} expected={scalar:?} actual={}", ty, show(&actual));
                    let prepared = ok(host::prepare(&fx.packed, &host::PrepareOptions { resolve: Some(resolve.clone()), measure: Some(observed(fast, gpu.clone())), ..Default::default() }, &live));
                    assert!(prepared.uniforms.iter().any(|a| float_bits(a) == float_bits(&uniform)));
                    let (c, g) = (cpu.load(Ordering::SeqCst), gpu.load(Ordering::SeqCst));
                    assert!((c > 0) == packed_expected && (g > 0) == packed_expected, "uniform {:?} reducer={reducer} count={count} time={time} domains={domains} CPU={c} GPU={g} expected={packed_expected}", ty);
                    assert_eq!(bits(&current.lock().unwrap()), original);
                });
            }
        }
    }
    drop(fx);
}
macro_rules! uniform_cases {
    ($($name:ident: $i:expr),*) => { $( #[test] fn $name() { uniform_case($i); } )* };
}
uniform_cases!(
    uniform_float_plus: 0, uniform_float_fn_add: 1, uniform_vec2_plus: 2, uniform_vec3_plus: 3, uniform_vec4_plus: 4,
    uniform_vec3_fn_add: 5, uniform_float_decay: 6, uniform_vec3_swapped_add: 7, uniform_float_min: 8,
    uniform_float_custom_plus: 9, uniform_float_integer_seed: 10
);

fn compile_value(body: &str) -> (Value, packed::T) {
    let value = evaluate(&workspace("", &[], body));
    let r = residual(&value);
    let p = packed::compile(&Default::default(), r, &e::residual::residual_view(r).unwrap().term).expect("packed region");
    (value, p)
}

/// A scalar capture is a uniform: another parameter value keeps the same register code, so the
/// GPU reuses its compiled shader (isogrid_city's `seed` cost a 150-450 ms MSL compile per value).
#[test]
fn changed_captures_keep_the_register_code() {
    let compile = |seed: i64, scale: f64| {
        let value = evaluate(&workspace(&format!("seed {seed} scale {scale:?}"), &[], "(map (fn [(i : int)] (* scale (+ t (float (int/xor i seed))))) (array/range 64 :type int))"));
        let r = residual(&value);
        let p = packed::compile(&Default::default(), r, &e::residual::residual_view(r).unwrap().term).expect("packed region");
        assert!(same(&packed_force(&p, &live_at(0.), true, None), &force_reference(&value, &live_at(0.))));
        p
    };
    let (a, b) = (compile(17, 0.5), compile(18, 2.0));
    assert_eq!(format!("{:?}", a.code), format!("{:?}", b.code));
    assert_ne!(format!("{:?}", a.uniforms), format!("{:?}", b.uniforms));
}

#[test]
fn product_loops_are_not_ordered_adds() {
    for body in [
        "(scan [a [t 0.0]] [x (map (fn [x] [x -0.0]) (array/range 2049))] (+ a x))",
        "(fold [a [t 0.0]] [x (array/range 2049)] (+ a x))",
    ] {
        let (value, p) = compile_value(body);
        assert!(!host::ordered_add(&p));
        for domains in [1, 8] {
            with_domains(domains, &|| {
                let live = live_at(0.);
                assert!(same(&packed_force(&p, &live, false, None), &force_reference(&value, &live)));
            });
        }
    }
    let (value, p) = compile_value("(fold [a [t 0.0 0.0]] [x (array/vec3 2049 [0.25 -0.0 1.0])] (+ a x))");
    // Same instructions as a supported reduction, but the fold is a Product loop.
    assert_eq!(
        host::view(&p).code,
        vec![
            Instruction::Input(0, 3, 0),
            Instruction::Input(0, 3, 1),
            Instruction::Input(0, 3, 2),
            Instruction::Accumulator(0),
            Instruction::Accumulator(1),
            Instruction::Accumulator(2),
            Instruction::Binary(Binary::Add, 3, 0),
            Instruction::Binary(Binary::Add, 4, 1),
            Instruction::Binary(Binary::Add, 5, 2),
        ]
    );
    assert!(!host::ordered_add(&p));
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let live = live_at(0.25);
            assert!(same(&packed_force(&p, &live, false, None), &force_reference(&value, &live)));
        });
    }
}

/// A resolver may advance a fold before a later reduction error; both callers roll it back.
fn rollback_case(ty: Ty, failed_component: usize, check_state: bool) {
    rollback_case_on(ty, failed_component, check_state, &[1, 8]);
}
fn rollback_case_on(ty: Ty, failed_component: usize, check_state: bool, domain_list: &[i64]) {
    let fold = evaluate(&workspace("", &[], "(state [a 0.0] (+ a 1.0))"));
    let live = live_at(0.);
    let fx = fixture(&ty, "+", None, None);
    let w_ = width(&ty);
    for domains in domain_list.iter().copied() {
        with_domains(domains, &|| {
            for count in [2usize, 1100] {
                for gpu in [false, true] {
                    let state = e::create_state();
                    let before = e::state_stamp(&state);
                    let values: Vec<f64> = (0..count * w_).map(|i| if i % w_ == failed_component && i / w_ >= count - 2 { 1e308 } else { 0. }).collect();
                    let make = |vals: Vec<f64>| if w_ == 1 { Value::Floats(1, Arc::new(vals)) } else { Value::Floats(3, Arc::new(vals)) };
                    let current = Arc::new(Mutex::new(make(values)));
                    let original = bits(&current.lock().unwrap());
                    let resolve: ResolveArc = {
                        let (state, fold, current, live) = (state.clone(), fold.clone(), current.clone(), live.clone());
                        Arc::new(move |value: &Value| {
                            ok(e::host::force_reference(&fold, &live, Some(&state), None, None));
                            resolve_source(current.clone())(value)
                        })
                    };
                    let reference_state = e::create_state();
                    let reference_before = e::state_stamp(&reference_state);
                    let reference_resolve: ResolveArc = {
                        let (state, fold, current, live) = (reference_state.clone(), fold.clone(), current.clone(), live.clone());
                        Arc::new(move |value: &Value| {
                            ok(e::host::force_reference(&fold, &live, Some(&state), None, None));
                            resolve_source(current.clone())(value)
                        })
                    };
                    let expected = reference_with(&fx.value, &live, Some(&reference_state), Some(reference_resolve));
                    assert!(expected.is_err() && e::state_stamp(&reference_state) == reference_before);
                    let result = if gpu {
                        host::prepare(&fx.packed, &host::PrepareOptions { state: Some(state.clone()), resolve: Some(resolve.clone()), ..Default::default() }, &live).map(|_| Value::Int(0))
                    } else {
                        packed::force(&fx.packed, &live, &packed::ForceOptions { state: Some(state.clone()), resolve: Some(resolve.clone()), ..Default::default() })
                    };
                    assert!(same(&result, &expected), "gpu={gpu} count={count} domains={domains} result={} expected={} stamp {} vs {}", show(&result), show(&expected), e::state_stamp(&state), before);
                    if check_state {
                        assert_eq!(e::state_stamp(&state), before, "state not rolled back: gpu={gpu} count={count}");
                    }
                    assert_eq!(bits(&current.lock().unwrap()), original);
                    *current.lock().unwrap() = if w_ == 1 { Value::Floats(1, Arc::new(vec![1., 2., 3.])) } else { Value::Floats(3, Arc::new(vec![1., 2., 3., 4., 5., 6.])) };
                    let expected = reference_with(&fx.value, &live, None, Some(resolve_source(current.clone())));
                    if gpu {
                        let prepared = ok(host::prepare(&fx.packed, &host::PrepareOptions { state: Some(state.clone()), resolve: Some(resolve.clone()), ..Default::default() }, &live));
                        let expected_uniform = components(&ok(reference_with(&fx.sum, &live, None, Some(resolve_source(current.clone())))));
                        assert!(prepared.uniforms.iter().any(|a| float_bits(a) == float_bits(&expected_uniform)));
                    } else {
                        let again = packed::force(&fx.packed, &live, &packed::ForceOptions { state: Some(state.clone()), resolve: Some(resolve.clone()), ..Default::default() });
                        assert!(same(&again, &expected));
                    }
                    if check_state {
                        assert_eq!(ok(reference_with(&fold, &live, Some(&state), None)), Value::Float(1.));
                    }
                }
            }
        });
    }
}
#[test]
fn rollback_float() {
    rollback_case(Ty::Float, 0, false);
}
#[test]
fn rollback_vec3_x() {
    rollback_case(Ty::Vec3, 0, false);
}
#[test]
fn rollback_vec3_y() {
    rollback_case(Ty::Vec3, 1, false);
}
#[test]
fn rollback_vec3_z() {
    rollback_case(Ty::Vec3, 2, false);
}

/// A map of fourteen 1,024-lane blocks splits into five chunks, which the domains share: values,
/// the first error and an ordered sum are those of one domain.
#[test]
fn small_maps_share_chunks_across_domains() {
    let source = "(array/range 13824 :type int)";
    for body in [
        format!("(map (fn [x] [(- (float x) t) (float (int/mod x 6))]) {source})"),
        format!("(map (fn [x] (if (int/ge x 9000) (int/div x 0) (int/add x (frame/index)))) {source})"),
        format!("(array/sum (map (fn [x] (* (float x) (+ t 0.1))) {source}))"),
    ] {
        let (value, p) = compile_value(&body);
        let mut live = live_at(0.25);
        live.frame = 3;
        let expected = force_reference(&value, &live);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                assert!(same(&expected, &packed_force(&p, &live, true, None)), "Small chunk parity: {body}");
            });
        }
    }
}

// Known failure: if state advancement precedes a failed reduction, packed::force/host::prepare may retain the advanced stamp instead of rolling back; dedicated rollback tests pin this assertion.
#[test]
fn rollback_restores_state() {
    rollback_case(Ty::Float, 0, true);
    rollback_case(Ty::Vec3, 0, true);
    rollback_case(Ty::Vec3, 1, true);
    rollback_case(Ty::Vec3, 2, true);
}
