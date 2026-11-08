// Port of lib/flow_ir/test_scatter.ml. The `--bench` block is not ported.
use crate::flow_ir::executor;
use crate::test_support::*;
use flow_check::op::MAX_WOSIZE;
use flow_eval as e;
use flow_syntax::ty::Ty;
use flow_syntax::value::{Value, source_value};
use rays_diagnostic::Diagnostic;
use std::sync::{Arc, Mutex};

const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

fn ints(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}
fn floats(v: &[f64]) -> Value {
    Value::Floats(1, Arc::new(v.to_vec()))
}
fn scatter(indices: Value, values: Value, size: Value) -> Result<Value, Diagnostic> {
    call_op("array/scatter_add", vec![("indices", indices), ("values", values), ("size", size)])
}
fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) {
    match (a, b) {
        (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
        _ => panic!("Scatter reference/compiled disagreement: {:?} / {:?}", a.as_ref().map(bits), b.as_ref().map(bits)),
    }
}
fn value(body: &str) -> Value {
    composition_value(body)
}
fn reference(width: usize, indices: &[i64], values: &[f64], size: usize) -> Vec<f64> {
    let mut result = vec![0.; size * width];
    for (row, index) in indices.iter().enumerate() {
        for c in 0..width {
            result[*index as usize * width + c] += values[row * width + c];
        }
    }
    result
}
fn wrap(width: usize, a: Vec<f64>) -> Value {
    let a = Arc::new(a);
    match width {
        1 => Value::Floats(1, a),
        2 => Value::Floats(2, a),
        3 => Value::Floats(3, a),
        _ => Value::Floats(4, a),
    }
}
fn flat(v: &Value) -> &Arc<Vec<f64>> {
    match v {
        Value::Floats(1..=4, a) => a,
        _ => panic!("expected a float array"),
    }
}

#[test]
fn exact_int_wrap_and_ownership() {
    // Exact signed63 wrapping, no float conversion of indices or values.
    assert_eq!(
        scatter(ints(&[0, 0, 1, 1]), ints(&[MAX_INT, 1, 9007199254740993, MIN_INT]), Value::Int(3)).unwrap(),
        ints(&[MIN_INT, 9007199254740993 + MIN_INT, 0])
    );
    let input = ints(&[1, 2]);
    let output = scatter(ints(&[0, 0]), input.clone(), Value::Int(1)).unwrap();
    match (&input, &output) {
        (Value::Int_array(original), Value::Int_array(result)) => {
            assert!(!Arc::ptr_eq(original, result));
            assert_eq!(original.as_slice(), &[1, 2]);
        }
        _ => panic!("expected int arrays"),
    }
}

#[test]
fn ordered_float_sums_match_the_reference_loop() {
    let size = 97usize;
    for count in [0usize, 1, 13, 32769] {
        let indices: Vec<i64> = (0..count).map(|i| (i % size) as i64).collect();
        let table = [1e16, 1., -1e16, -0., 0., f64::from_bits(1), -f64::from_bits(1)];
        for width in 1..=4usize {
            let data: Vec<f64> = (0..count * width).map(|i| table[i % 7]).collect();
            let inputs = (ints(&indices), wrap(width, data.clone()));
            let before = (bits(&inputs.0), bits(&inputs.1));
            let expected = wrap(width, reference(width, &indices, &data, size));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let got = scatter(inputs.0.clone(), inputs.1.clone(), Value::Int(size as i64)).unwrap();
                    assert_eq!(bits(&got), bits(&expected));
                    assert!(!Arc::ptr_eq(flat(&got), flat(&inputs.1)));
                    assert_eq!((bits(&inputs.0), bits(&inputs.1)), before);
                });
            }
        }
    }
}

#[test]
fn malformed_inputs() {
    assert_eq!(scatter(ints(&[]), ints(&[]), Value::Int(0)).unwrap(), ints(&[]));
    for index in [-1, 1, 9007199254740993, MAX_INT] {
        reject("E_ARRAY_RANGE", scatter(ints(&[index]), ints(&[1]), Value::Int(1)));
    }
    for size in [-1, MAX_WOSIZE as i64 + 1, MAX_INT] {
        reject("E_ARRAY_RANGE", scatter(ints(&[]), ints(&[]), Value::Int(size)));
    }
    reject("E_ARRAY_RANGE", scatter(ints(&[]), Value::Floats(4, Arc::new(vec![])), Value::Int(MAX_WOSIZE as i64 / 4 + 1)));
    reject("E_ARRAY_COUNT", scatter(ints(&[0]), ints(&[]), Value::Int(1)));
    reject("E_ARRAY_TYPE", scatter(floats(&[0.]), floats(&[1.]), Value::Int(1)));
    reject("E_ARRAY_TYPE", scatter(ints(&[]), Value::Text_array(Arc::new(Vec::<Vec<u8>>::new().into())), Value::Int(1)));
    reject("E_ARRAY_TYPE", scatter(ints(&[0]), Value::Floats(3, Arc::new(vec![1., 2.])), Value::Int(1)));
    reject("E_ARRAY_TYPE", scatter(ints(&[]), ints(&[]), Value::Float(1.)));
    // Error order is source-row, then component. A later invalid index cannot replace an earlier
    // overflowing sum; an earlier index wins the reverse.
    reject("E_NONFINITE", scatter(ints(&[0, 0, 1]), floats(&[f64::MAX, f64::MAX, 1.]), Value::Int(1)));
    reject("E_ARRAY_RANGE", scatter(ints(&[1, 0, 0]), floats(&[1., f64::MAX, f64::MAX]), Value::Int(1)));
    for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        reject("E_NONFINITE", scatter(ints(&[0]), floats(&[bad]), Value::Int(1)));
    }
}

#[test]
fn core_and_executor_match_the_reference() {
    // Sizes: OCaml used 32769 / 5001; 2049 keeps two 1024-lane block crossings within the debug budget.
    for body in [
        "(array/scatter_add (map (fn [(i : int)] (int/mod i 97)) (array/range 2049 :type int)) (map (fn [x] (+ x t)) (array/range 2049)) 97)",
        "(array/scatter_add (array/int 13 0) (array/int 13 (int/add (frame/index) 9007199254740993)) 2)",
        "(let* [accumulate (fn [(ids : (array int)) (items : (array float)) (size : int)] (array/scatter_add ids items size))] (accumulate (array/int 13 0) (array/float 13 t) 2))",
        "(array/scatter_add (array/int 2049 0) (array/vec3 2049 [t -0. 1]) 3)",
        "(array/scatter_add (array/int 0) (array/float 0 t) 0)",
        "(if (> t 0.) (array/float 1) (array/scatter_add (array/int 1 -1) (array/float 1) 1))",
        "(array/scatter_add (array/int 3 0) (array/float 3 (* t 1.7976931348623157e308)) 1)",
        "(map (fn [x] (+ x t)) (array/scatter_add (array/range 2049 :type int) (array/float 2049 1.) 2049))",
    ] {
        let v = value(body);
        let r = residual(&v);
        let core = lower_strict(r).unwrap_or_else(|f| panic!("{f}"));
        let executable = executor_compile(&v, true);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                for time in [-1., 0., 0.25, 2.] {
                    let live = live_at(time);
                    let expected = force_reference(&v, &live);
                    same(&run(None, false, None, None, &core, &live), &expected);
                    same(&executor_force(&executable, true, &live), &expected);
                }
            });
        }
    }
    let unsupported = value("(array/scatter_add (array/int 2049 0) (map (fn [x] (value/rand x t)) (array/range 2049)) 1)");
    match executor::compile(&unsupported, &executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
        Ok(_) => panic!("Unsupported scatter producer accepted"),
    }
    // Variable-size scatter is a whole-array primitive, never a per-row GPU scatter instruction.
    let dynamic = value("(map (fn [(n : int)] (array/nth (array/scatter_add (array/int n 0) (array/float n t) 1) 0)) (array/range 2049 :type int))");
    assert!(compile_region(residual(&dynamic), true, None).is_err());
}

fn host_declarations() -> &'static str {
    "[(indices : (array int) (array/int 0)) (data : (array float) (array/float 0))]"
}
fn host_inputs() -> Vec<(String, Vec<(String, Value)>)> {
    vec![(
        "g".to_string(),
        vec![
            ("indices".to_string(), source_value("fixture/indices", Ty::Array(Box::new(Ty::Int)), vec![])),
            ("data".to_string(), source_value("fixture/values", Ty::Array(Box::new(Ty::Float)), vec![])),
        ],
    )]
}

/// Sources resolve in authored argument order and retain type/cause failures. A changed host
/// buffer across forces must not be memoized globally.
#[test]
fn host_sources_order_errors_and_laziness() {
    let v = dynamic_value(&host_inputs(), host_declarations(), "(array/scatter_add indices data 2)");
    let executable = executor_compile(&v, true);
    let data = Arc::new(Mutex::new(floats(&[1., 2., 3.])));
    let calls = Arc::new(Mutex::new(Vec::<&'static str>::new()));
    let resolve: ResolveArc = {
        let (data, calls) = (data.clone(), calls.clone());
        Arc::new(move |s: &Value| match s {
            Value::Source(g) if g.name == "fixture/indices" => {
                calls.lock().unwrap().push("indices");
                Ok(ints(&[0, 1, 0]))
            }
            Value::Source(g) if g.name == "fixture/values" => {
                calls.lock().unwrap().push("values");
                Ok(data.lock().unwrap().clone())
            }
            _ => panic!("unexpected source"),
        })
    };
    for domain in [1, 4, 8] {
        with_domains(domain, &|| {
            for next in [[1., 2., 3.], [7., 8., 9.]] {
                *data.lock().unwrap() = floats(&next);
                calls.lock().unwrap().clear();
                let live = live_at(1.);
                same(&exec_force(&executable, &live, None, Some(resolve.clone())), &reference_with(&v, &live, None, Some(resolve.clone())));
                // Domains share this recorder, so the one-domain order is exact and the others are per pair.
                let calls = calls.lock().unwrap().clone();
                assert_eq!(calls, vec!["indices", "values", "indices", "values"]);
            }
        });
    }
    let live = live_at(1.);
    let wrong: ResolveArc = Arc::new(|s: &Value| match s {
        Value::Source(g) if g.name == "fixture/indices" => Ok(floats(&[0., 1., 0.])),
        _ => panic!("unexpected source"),
    });
    same(&exec_force(&executable, &live, None, Some(wrong.clone())), &reference_with(&v, &live, None, Some(wrong)));

    let skipped = dynamic_value(&host_inputs(), host_declarations(), "(if (> t 0.) (array/float 2) (array/scatter_add indices data 2))");
    let skipped_program = executor_compile(&skipped, true);
    let no_source: ResolveArc = Arc::new(|_| panic!("Untaken scatter resolved its host source"));
    same(&exec_force(&skipped_program, &live, None, Some(no_source.clone())), &reference_with(&skipped, &live, None, Some(no_source)));

    let failed: ResolveArc = Arc::new(|s: &Value| match s {
        Value::Source(g) if g.name == "fixture/indices" => {
            let mut d = flow_syntax::value::fail("E_DATA_SOURCE", "missing indices");
            d.cause = Some("scatter input cause".into());
            Err(d)
        }
        _ => panic!("unexpected source"),
    });
    same(&exec_force(&executable, &live, None, Some(failed.clone())), &reference_with(&v, &live, None, Some(failed)));
}

#[test]
fn state_matches_the_reference_across_frames() {
    let stateful = value("(let* [counter (state [s 0.] (+ s 1.))] (if (> t 0.) (array/scatter_add (array/int 1 -1) (array/float 1 counter) 1) (array/float 1 counter)))");
    let reference_state = e::create_state();
    let compiled_state = e::create_state();
    let executable = executor_compile(&stateful, true);
    for (frame, time) in [-1., 1., -1., 1., -1.].iter().enumerate() {
        let mut live = live_frame(*time, frame as i64);
        live.tick = frame as i64;
        same(&exec_force(&executable, &live, Some(&compiled_state), None), &reference_with(&stateful, &live, Some(&reference_state), None));
        let a: Vec<String> = e::host::state_values(&reference_state).iter().map(bits).collect();
        let b: Vec<String> = e::host::state_values(&compiled_state).iter().map(bits).collect();
        assert_eq!(a, b);
    }
}
