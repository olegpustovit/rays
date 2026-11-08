// Port of lib/flow_ir/test_compact_scan.ml.
use crate::flow_ir::Executor;
use crate::test_support::*;
use flow_eval as e;
use flow_syntax::ty::Ty;
use flow_syntax::value::{Value, source_value};
use rays_diagnostic::Diagnostic;
use std::sync::{Arc, Mutex};

const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

/// OCaml 63-bit wrapping addition.
fn add63(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}
fn ints(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}
fn call(name: &str, array: Value) -> Result<Value, Diagnostic> {
    call_op(name, vec![(if name == "array/compact" { "mask" } else { "array" }, array)])
}
fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) {
    match (a, b) {
        (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause, "{} / {}", a.message, b.message),
        _ => panic!("Compaction/scan reference disagreement: {:?} / {:?}", a.as_ref().map(bits), b.as_ref().map(bits)),
    }
}
fn live_value(body: &str) -> Value {
    composition_value(body)
}
fn compact(mask: &[i64]) -> Vec<i64> {
    mask.iter().enumerate().filter(|(_, n)| **n == 1).map(|(i, _)| i as i64).collect()
}
fn scan(values: &[i64]) -> Vec<i64> {
    let mut sum = 0;
    let mut out = vec![0];
    for n in values {
        sum = add63(sum, *n);
        out.push(sum);
    }
    out
}
fn arc_of(v: &Value) -> &Arc<Vec<i64>> {
    match v {
        Value::Int_array(a) => a,
        _ => panic!("expected an int array"),
    }
}

#[test]
fn compact_kept_indices_and_immutable_source() {
    for count in [0usize, 1, 13, 32769] {
        let patterns: [&dyn Fn(usize) -> i64; 3] = [&|_| 0, &|_| 1, &|i| if i % 3 == 0 { 1 } else { 0 }];
        for pattern in patterns {
            let mask: Vec<i64> = (0..count).map(pattern).collect();
            let input = ints(&mask);
            let expected = ints(&compact(&mask));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let actual = call("array/compact", input.clone()).unwrap();
                    assert_eq!(bits(&actual), bits(&expected));
                    assert!(!Arc::ptr_eq(arc_of(&actual), arc_of(&input)));
                });
            }
            assert_eq!(arc_of(&input).as_slice(), mask.as_slice());
        }
    }
}

#[test]
fn scan_sum_offsets_and_wrap() {
    for count in [0usize, 1, 13, 32769] {
        let pattern = [MAX_INT, 1, MIN_INT, -1, 9007199254740993, 0, 7];
        let values: Vec<i64> = (0..count).map(|i| pattern[i % 7]).collect();
        let input = ints(&values);
        let expected = ints(&scan(&values));
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                let actual = call("array/scan_sum", input.clone()).unwrap();
                assert_eq!(bits(&actual), bits(&expected));
                assert!(!Arc::ptr_eq(arc_of(&actual), arc_of(&input)) && arc_of(&actual).len() == count + 1);
            });
        }
        assert_eq!(arc_of(&input).as_slice(), values.as_slice());
    }
    assert_eq!(call("array/compact", ints(&[])).unwrap(), ints(&[]));
    assert_eq!(call("array/scan_sum", ints(&[])).unwrap(), ints(&[0]));
    assert_eq!(call("array/scan_sum", ints(&[3, 0, 2])).unwrap(), ints(&[0, 3, 3, 5]));
    assert_eq!(call("array/scan_sum", ints(&[MAX_INT, 1, MIN_INT])).unwrap(), ints(&[0, MAX_INT, MIN_INT, 0]));
}

#[test]
fn bad_masks_and_types() {
    let mut big: Vec<i64> = vec![1; 32769];
    big[32768] = MIN_INT;
    for mask in [vec![2], vec![-1], vec![1, 0, MAX_INT], big] {
        reject("E_ARRAY_MASK", call("array/compact", ints(&mask)));
    }
    for name in ["array/compact", "array/scan_sum"] {
        for bad in [
            Value::Float_array(Arc::new(vec![])),
            Value::Float_array(Arc::new(vec![0., 1.])),
            Value::Vec3_array(Arc::new(vec![0., 1.])),
            Value::Text_array(Arc::new(vec![b"0".to_vec()])),
            Value::Int(1),
            Value::Bool(true),
            Value::List(Arc::new(vec![Value::Int(1)])),
        ] {
            reject("E_ARRAY_TYPE", call(name, bad));
        }
    }
}

#[test]
fn core_and_executor_match_the_reference() {
    // Sizes: OCaml used 32769-row bodies. The default loop uses 1025 rows (one 1024-lane block crossing),
    // 1 and 4 domains and two times (both sides of `(> t 0.)`); RAYS_SLOW=1 runs 2049 rows, 1/4/8 domains
    // and four times. No huge array is fabricated to exercise the allocation bound. The guard precedes
    // count+1 and allocation; runtime proof uses real small/large tails.
    let slow = std::env::var("RAYS_SLOW").as_deref() == Ok("1");
    let n = if slow { 2049 } else { 1025 };
    let (domain_counts, times): (&[i64], &[f64]) = if slow { (&[1, 4, 8], &[-1., 0., 0.25, 2.]) } else { (&[1, 4], &[-1., 2.]) };
    let bodies = [
        format!("(array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range {n} :type int)))"),
        format!("(array/scan_sum (map (fn [(i : int)] (int/add i (frame/index))) (array/range {n} :type int)))"),
        "(array/scan_sum (array/int 13 9007199254740993))".to_string(),
        "(array/scan_sum (array/int 0))".to_string(),
        "(array/compact (array/int 0))".to_string(),
        "(if (> t 0.) (array/int 1) (array/compact (array/int 1 2)))".to_string(),
        "(array/compact (array/int 13 (if (> t 0.) 1 2)))".to_string(),
        format!("(let* [collect (fn [(mask : (array int))] (array/compact mask))] (collect (array/int {n} 1)))"),
        format!("(let* [kept (array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range {n} :type int))) table (array/range {n})] (map (fn [(i : int)] (array/nth table i)) kept))"),
        format!("(map (fn [(i : int)] (int/add i (frame/index))) (array/scan_sum (array/int {n} 1)))"),
    ];
    for body in &bodies {
        let value = live_value(body);
        let r = residual(&value);
        let core = lower_strict(r).unwrap_or_else(|f| panic!("{f}"));
        let executor = executor_compile(&value, true);
        for &domains in domain_counts {
            with_domains(domains, &|| {
                for &time in times {
                    let live = live_frame(time, 3);
                    let reference = force_reference(&value, &live);
                    same(&run(None, false, None, None, &core, &live), &reference);
                    same(&executor_force(&executor, true, &live), &reference);
                }
            });
        }
    }
}

#[test]
fn unsupported_producer_and_dynamic_count_refuse() {
    let unsupported = live_value("(array/compact (map (fn [x] (int (value/rand x t))) (array/range 2049)))");
    match Executor::compile(&unsupported, &Executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
        Ok(_) => panic!("Unsupported producer accepted"),
    }
    for operation in ["array/compact", "array/scan_sum"] {
        let v = live_value(&format!("(map (fn [(n : int)] (array/count ({operation} (array/int n 1)))) (array/range 5001 :type int))"));
        assert!(compile_region(residual(&v), true, None).is_err(), "{operation}");
    }
}

fn mask_inputs() -> Vec<(String, Vec<(String, Value)>)> {
    vec![("g".to_string(), vec![("data".to_string(), source_value("fixture/mask", Ty::Array(Box::new(Ty::Int)), vec![]))])]
}
fn dynamic(body: &str) -> Value {
    dynamic_value(&mask_inputs(), "[(data : (array int) (array/int 0))]", body)
}
fn resolver(current: Arc<Mutex<Vec<i64>>>) -> ResolveArc {
    Arc::new(move |v: &Value| match v {
        Value::Source(g) if g.name == "fixture/mask" => Ok(ints(&current.lock().unwrap())),
        _ => panic!("unexpected source"),
    })
}

/// Only the actual host array has its origin. Compact changes length from mask data and scan has
/// n+1 offsets, so derived maps cannot inherit it.
#[test]
fn count_origin_only_for_the_host_array() {
    for operation in ["direct", "array/compact", "array/scan_sum"] {
        let source = if operation == "direct" { "data".to_string() } else { format!("({operation} data)") };
        let value = dynamic(&format!("(map (fn [(i : int)] (int/add i (frame/index))) {source})"));
        let r = residual(&value);
        for fusion in [false, true] {
            let p = ok_ds(compile_region(r, fusion, Some(data_count_source())));
            let expected = if operation == "direct" && fusion { Some((vec!["fixture".to_string(), "data".to_string()], vec![])) } else { None };
            assert_eq!(crate::packed::count_origin(&p), expected, "Origin {operation} fusion={fusion}");
            let current = Arc::new(Mutex::new(vec![1, 0, 1]));
            let resolve = resolver(current.clone());
            for data in [vec![1, 0, 1], vec![0, 1, 0, 1, 1], vec![]] {
                *current.lock().unwrap() = data;
                let live = live_frame(1., 3);
                for domains in [1, 4, 8] {
                    with_domains(domains, &|| {
                        same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve.clone())));
                    });
                }
            }
        }
    }
}

#[test]
fn zipped_derived_arrays_have_no_origin() {
    for operation in ["array/compact", "array/scan_sum"] {
        let value = dynamic(&format!("(map (fn [(a : int) (b : int)] (int/add a b)) data ({operation} data))"));
        let r = residual(&value);
        for fusion in [false, true] {
            let p = ok_ds(compile_region(r, fusion, Some(data_count_source())));
            assert!(crate::packed::count_origin(&p).is_none() && crate::packed::static_count(&p).is_none() && p.sources.len() == 2);
            let resolve: ResolveArc = Arc::new(|v: &Value| match v {
                Value::Source(g) if g.name == "fixture/mask" => Ok(ints(&[1, 0, 1, 0])),
                _ => panic!("unexpected source"),
            });
            let live = live_at(1.);
            same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve)));
        }
    }
}

#[test]
fn source_errors_and_untaken_sources() {
    for operation in ["array/compact", "array/scan_sum"] {
        let value = dynamic(&format!("({operation} data)"));
        let executor = executor_compile(&value, true);
        let live = live_at(1.);
        let missing: ResolveArc = Arc::new(|_| {
            let mut d = flow_syntax::value::fail("E_DATA_SOURCE", "missing mask");
            d.cause = Some("scan source cause".into());
            Err(d)
        });
        same(&exec_force(&executor, &live, None, Some(missing.clone())), &reference_with(&value, &live, None, Some(missing)));
        let wrong: ResolveArc = Arc::new(|_| Ok(Value::Float_array(Arc::new(vec![1.]))));
        same(&exec_force(&executor, &live, None, Some(wrong.clone())), &reference_with(&value, &live, None, Some(wrong)));
        let skipped = dynamic(&format!("(if (> t 0.) (array/int 0) ({operation} data))"));
        let no_source: ResolveArc = Arc::new(|_| panic!("Untaken scan resolved Source"));
        same(
            &exec_force(&executor_compile(&skipped, true), &live, None, Some(no_source.clone())),
            &reference_with(&skipped, &live, None, Some(no_source)),
        );
    }
}

#[test]
fn state_matches_the_reference_across_frames() {
    let stateful = live_value("(let* [counter (state [s 0] (int/add s 1))] (if (> t 0.) (array/compact (array/int 1 2)) (array/scan_sum (array/int 1 counter))))");
    let a = e::create_state();
    let b = e::create_state();
    let executor = executor_compile(&stateful, true);
    for (frame, time) in [-1., 1., -1., 1., -1.].iter().enumerate() {
        let mut live = live_frame(*time, frame as i64);
        live.tick = frame as i64;
        same(&exec_force(&executor, &live, Some(&a), None), &reference_with(&stateful, &live, Some(&b), None));
        assert_eq!(bits_all(&e::Host::state_values(&a)), bits_all(&e::Host::state_values(&b)));
    }
    // A state-reading map is a reference kernel by design: strict compilation accepts it and matches the reference.
    let reader = live_value("(let* [counter (state [s 0.] (+ s 1.))] (map (fn [x] (+ x counter)) (array/range 4)))");
    let a = e::create_state();
    let b = e::create_state();
    let executor = executor_compile(&reader, true);
    for frame in 0..3 {
        let mut live = live_frame(1., frame);
        live.tick = frame;
        same(&exec_force(&executor, &live, Some(&a), None), &reference_with(&reader, &live, Some(&b), None));
        assert_eq!(bits_all(&e::Host::state_values(&a)), bits_all(&e::Host::state_values(&b)));
    }
}

fn bits_all(vs: &[Value]) -> Vec<String> {
    vs.iter().map(bits).collect()
}
