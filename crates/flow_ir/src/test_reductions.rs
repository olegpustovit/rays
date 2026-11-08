// Port of lib/flow_ir/test_reductions.ml. The `--bench` block is not ported (a timing harness,
// not an assertion).
use crate::flow_ir::Executor;
use crate::packed;
use crate::test_support::*;
use flow_check::op::{reduce_array, Reduction};
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::syntax;
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

// Sizes: OCaml used 5001-element arrays (5 blocks of 1024); 2049 keeps three block crossings and fits the 5 s budget in debug.
// OCaml native ints are 63 bits.
const MIN_INT: i64 = -4611686018427387904;
const MAX_INT: i64 = 4611686018427387903;

fn same(a: &Value, b: &Value) -> bool {
    bits(a) == bits(b)
}
fn ints(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}
fn floats(v: &[f64]) -> Value {
    Value::Float_array(Arc::new(v.to_vec()))
}
fn reject(code: &str, r: Result<Value, Diagnostic>) {
    match r {
        Err(d) => assert_eq!(d.code, code),
        Ok(v) => panic!("accepted {code}: {v:?}"),
    }
}

fn value_with(declarations: &str, inputs: Option<&[(String, Vec<(String, Value)>)]>, body: &str) -> Value {
    let source = format!("(workspace w (graph g :context host {declarations} (if (> t -1000.) {body} {body})))");
    let forms = ok(syntax::parse(&source));
    let workspace = match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    };
    ok(e::r#static(&workspace, false, inputs)).results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
}
fn value(body: &str) -> Value {
    value_with("", None, body)
}

#[test]
fn reduce_array_direct() {
    let integers = ints(&[MIN_INT, MAX_INT, 9007199254740993, -9007199254740993]);
    assert_eq!(reduce_array(None, Reduction::Minimum, None, &integers).unwrap(), Value::Int(MIN_INT));
    assert_eq!(reduce_array(None, Reduction::Maximum, None, &integers).unwrap(), Value::Int(MAX_INT));
    assert_eq!(reduce_array(None, Reduction::Sum, Some(&ints(&[0, 2, 4])), &integers).unwrap(), ints(&[-1, 0]));
    let zeros = floats(&[-0., 0.]);
    assert!(same(&reduce_array(None, Reduction::Minimum, None, &zeros).unwrap(), &Value::Float(-0.)));
    let v2 = Value::Vec2_array(Arc::new(vec![3., -0., 1., 0.]));
    assert!(same(&reduce_array(None, Reduction::Minimum, None, &v2).unwrap(), &Value::Vec2(1., -0.)));
    assert_eq!(reduce_array(None, Reduction::Sum, Some(&ints(&[0, 0, 0])), &ints(&[])).unwrap(), ints(&[0, 0]));
    assert!(same(
        &reduce_array(None, Reduction::Sum, Some(&ints(&[0, 0, 0])), &Value::Vec4_array(Arc::new(vec![]))).unwrap(),
        &Value::Vec4_array(Arc::new(vec![0.; 8]))
    ));
    for operation in [Reduction::Minimum, Reduction::Maximum] {
        reject("E_ARRAY_EMPTY", reduce_array(None, operation, None, &floats(&[])));
        reject("E_ARRAY_EMPTY", reduce_array(None, operation, Some(&ints(&[0, 0, 1])), &floats(&[1.])));
        assert_eq!(reduce_array(None, operation, Some(&ints(&[0])), &floats(&[])).unwrap(), floats(&[]));
    }
    for offsets in [vec![], vec![1, 2], vec![0, 1], vec![0, 3, 2], vec![0, -1, 2], vec![0, MAX_INT, 2]] {
        reject("E_ARRAY_OFFSETS", reduce_array(None, Reduction::Sum, Some(&ints(&offsets)), &floats(&[1., 2.])));
    }
    reject("E_ARRAY_TYPE", reduce_array(None, Reduction::Sum, Some(&floats(&[0., 2.])), &ints(&[1, 2])));
    reject("E_ARRAY_TYPE", reduce_array(None, Reduction::Minimum, None, &Value::Text_array(Arc::new(vec![b"a".to_vec()]))));
    reject("E_NONFINITE", reduce_array(None, Reduction::Maximum, None, &floats(&[f64::NAN])));
    reject("E_NONFINITE", reduce_array(None, Reduction::Sum, Some(&ints(&[0, 2])), &floats(&[f64::MAX, f64::MAX])));
}

#[test]
fn reduce_array_rows_parallel_matches_sequential() {
    let count = 32769usize; // rows cross the 1024 block and the grain of 97
    let offsets = ints(&(0..=count as i64).map(|i| i * 3).collect::<Vec<_>>());
    for operation in [Reduction::Sum, Reduction::Minimum, Reduction::Maximum] {
        let pattern = [1e16, 1., -1e16, -0., 0., 1e-100];
        let data = Value::Vec3_array(Arc::new((0..count * 9).map(|i| pattern[i % 6]).collect()));
        let before = (bits(&data), bits(&offsets));
        let reference = reduce_array(None, operation, Some(&offsets), &data).unwrap();
        for domains in [1, 8] {
            let result = std::cell::RefCell::new(None);
            with_domains(domains, &|| {
                let rows = |start: i64, finish: i64, f: &(dyn Fn(i64) + Send + Sync)| {
                    parallel::for_(97, start, finish, &|i| {
                        f(i);
                        Ok(())
                    })
                    .unwrap()
                };
                *result.borrow_mut() = Some(reduce_array(Some(&rows), operation, Some(&offsets), &data).unwrap());
            });
            assert!(same(&result.into_inner().unwrap(), &reference));
        }
        assert_eq!((bits(&data), bits(&offsets)), before);
    }
}

#[test]
fn reductions_core_reference_and_cpu_agree() {
    for body in [
        "(array/min (array/vec3 2049 [t 3 -0.]))",
        "(array/max (array/int 2049 (int/add 9007199254740993 (frame/index))))",
        "(array/segment_sum (map (fn [x] [x t 1.]) (array/range 2049)) (map (fn [x] (int x)) (array/range 2050)))",
        "(array/segment_min (array/float 2049 t) (map (fn [x] (int x)) (array/range 2050)))",
        "(array/segment_max (array/int 2049 (frame/index)) (map (fn [x] (int x)) (array/range 2050)))",
        "(array/segment_argmax (map (fn [(i : int)] (int/mod (int/mul i 5) 7)) (array/range 2049 :type int)) (map (fn [(i : int)] (int/mul i 3)) (array/range 684 :type int)))",
        "(array/segment_argmax (map (fn [(i : int)] (* t (float (int/mod (int/mul i 5) 7)))) (array/range 2049 :type int)) (map (fn [(i : int)] (int/mul i 3)) (array/range 684 :type int)))",
        "(let* [minimum (array/min (array/float 2049 t))] (map (fn [x] (+ x minimum)) (array/range 11)))",
        "(map (fn [x] (+ x t)) (array/segment_sum (array/float 2049 1.) (map (fn [x] (int x)) (array/range 2050))))",
        "(let* [threshold (int/add (frame/index) 9007199254740993)] (map (fn [x] (int/add x threshold)) (array/range 2049 :type int)))",
        "(let* [bias (array/min (array/float 7 t)) shift (fn [x] (+ x bias))] (map shift (array/range 2049)))",
        "(let* [column (map (fn [x] (+ x t)) (array/range 2049)) minimum (array/min column)\n         sample {:nested {:column column :bias minimum}} shift (fn [x] (+ x sample.nested.bias))]\n         (map shift sample.nested.column))",
    ] {
        let val = value(body);
        let residual = residual(&val);
        let core = lower_strict(residual).unwrap_or_else(|f| panic!("{body}: {f}"));
        let executable = executor_compile(&val, true);
        for time in [-0., 0., 0.25, 2.] {
            let live = live_at(time);
            let reference = ok(force_reference(&val, &live));
            for domains in [1, 8] {
                let out = std::cell::RefCell::new(None);
                with_domains(domains, &|| {
                    *out.borrow_mut() = Some((ok(run(Some(residual), false, None, None, &core, &live)), ok(executor_force(&executable, true, &live))));
                });
                let (direct, compiled) = out.into_inner().unwrap();
                assert!(same(&direct, &reference) && same(&compiled, &reference), "{body} at t={time}");
            }
        }
            }
}

#[test]
fn empty_body_reduction_and_argument_binding() {
    let empty = value("(map (fn [x] (if (> t 0.) (array/min (array/float 0)) 0.)) (array/range 0))");
    assert_eq!(ok(force_reference(&empty, &live_at(1.))), floats(&[]));
    assert!(Executor::compile(&empty, &Executor::CompileOptions { strict: true, ..Default::default() }).is_err(), "strict compilation accepted an empty body reduction");

    let captured = value("(let* [minimum (array/min (array/float 7 t))] (map (fn [x] (+ x minimum)) (array/range 11)))");
    let core = lower_strict(residual(&captured)).unwrap();
    let p = core.regions[0].clone();
    assert_eq!(packed::host::argument_types(&p).unwrap(), vec![Ty::Float]);
    for arguments in [vec![], vec![Value::Int(1)], vec![Value::Float(1.), Value::Float(2.)]] {
        match packed::host::bind_arguments(&p, &arguments, None) {
            Err(d) => assert_eq!(d.code, "E_CORE_ARGUMENT"),
            Ok(_) => panic!("invalid argument accepted"),
        }
    }
    match packed::host::bind_arguments(&p, &[Value::Float(f64::NAN)], None) {
        Err(d) => assert_eq!(d.code, "E_NONFINITE"),
        Ok(_) => panic!("nonfinite argument accepted"),
    }
    let bound = ok(packed::host::bind_arguments(&p, &[Value::Float(3.)], None));
    assert!(p.sources.iter().any(|(r, _)| *r == p.residual));
    for (i, (r, _)) in p.sources.iter().enumerate() {
        if *r == p.residual {
            assert!(bound.sources[i].0 == bound.residual);
        }
    }
    for ((original, _), (copy, _)) in p.stages.iter().zip(bound.stages.iter()) {
        if *original == p.residual {
            assert!(*copy == bound.residual);
        }
    }
    let forced = ok(packed::force(&bound, &live_at(1.), &packed::ForceOptions { strict: true, ..Default::default() }));
    assert_eq!(forced, floats(&(0..11).map(|i| i as f64 + 3.).collect::<Vec<_>>()));
}

#[test]
fn bound_arguments_release_arrays_with_the_last_region_clone() {
    let captured = value("(let* [data (state [p (exact (array/range 32))] (map (fn [x] (+ x t)) p))] (map (fn [x] (+ x (array/nth data 0))) (array/range 32)))");
    let core = lower_strict(residual(&captured)).unwrap();
    let p = core.regions.iter().rev().find(|p| packed::host::argument_types(p).unwrap() == vec![Ty::Array(Box::new(Ty::Float))]).expect("array argument");
    for frame in 0..32 {
        let data = Arc::new(vec![frame as f64; 32]);
        let weak = Arc::downgrade(&data);
        let bound = ok(packed::host::bind_arguments(p, &[Value::Float_array(data)], None));
        let clone = bound.clone();
        drop(bound);
        assert!(weak.upgrade().is_some(), "a region clone still owns the argument");
        let forced = ok(packed::force(&clone, &live_at(1.), &packed::ForceOptions { strict: true, ..Default::default() }));
        assert_eq!(forced, floats(&(0..32).map(|i| (i + frame) as f64).collect::<Vec<_>>()));
        drop(clone);
        assert!(weak.upgrade().is_none(), "finished bindings retained frame {frame}'s array");
    }
}

#[test]
fn source_column_and_unsupported_producer() {
    let source = value_with(
        "[(data : (array float) (array/float 0))]",
        Some(&[("g".to_string(), vec![("data".to_string(), Value::Source("fixture/column".into(), Ty::Array(Box::new(Ty::Float)), vec![]))])]),
        "(let* [minimum (array/min data) sample {:nested {:column data :bias minimum}}\n      shift (fn [x] (+ x sample.nested.bias))] (map shift sample.nested.column))",
    );
    let compiled = executor_compile(&source, true);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let data = floats(&(0..2049).map(|i| (i % 17) as f64 - 5.).collect::<Vec<_>>());
            let before = bits(&data);
            let resolve: crate::core_exec::ResolveFn = {
                let data = data.clone();
                Arc::new(move |v: &Value| match v {
                    Value::Source(name, _, args) if name == "fixture/column" && args.is_empty() => Ok(data.clone()),
                    _ => Err(flow_syntax::value::fail("E_DATA_SOURCE", "Unexpected reduction fixture source.")),
                })
            };
            let live = live_at(0.25);
            let forced = Executor::force(&compiled, &Executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() }, &live);
            let reference = e::Host::force_reference(&source, &live, None, None, Some(resolve));
            assert!(same(&ok(forced), &ok(reference)));
            assert_eq!(before, bits(&data));
        });
    }
    let unsupported = value("(array/min (map (fn [x] (value/rand (+ x t))) (array/range 2049)))");
    match Executor::compile(&unsupported, &Executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
        Ok(_) => panic!("unsupported producer accepted"),
    }
}

// Product defect: Maximum over a Float_array [-0., 0.] returns -0. (flow_check op.rs reduce_array, the
// width-1 path uses f64::max, whose zero sign is unspecified); OCaml returns +0.
#[test]
fn maximum_of_signed_zeros_is_positive_zero() {
    let v2 = Value::Vec2_array(Arc::new(vec![3., -0., 1., 0.]));
    assert!(same(&reduce_array(None, Reduction::Maximum, None, &v2).unwrap(), &Value::Vec2(3., 0.)));
    let zeros = floats(&[-0., 0.]);
    assert!(same(&reduce_array(None, Reduction::Maximum, None, &zeros).unwrap(), &Value::Float(0.)));
}
