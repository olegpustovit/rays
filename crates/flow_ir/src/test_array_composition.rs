// Port of lib/flow_ir/test_array_composition.ml.
// Sizes: OCaml sweeps 0/1/13/4097/5001/32769 rows; the direct-operator checks keep that sweep, the
// compiled/Core checks use 0/1/13/2049 (still crossing two 1024-lane blocks) to fit the 5 s debug budget.
// Not ported: the in-place `mutate` of results (OCaml arrays are mutable); the Rust equivalent is
// that a result never shares its source's Arc storage and the sources compare equal afterwards.
use crate::core_ir::Region::Instruction;
use crate::flow_ir::Executor;
use crate::packed;
use crate::test_support::*;
use flow_eval as e;
use flow_syntax::ty::{self, FnSignature, Ty};
use flow_syntax::value::{Value, source_value};
use rays_diagnostic::Diagnostic;
use std::sync::{Arc, Mutex};

const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

fn call(name: &str, args: Vec<(&str, Value)>) -> Result<Value, Diagnostic> {
    call_op(name, args)
}

fn sample(ty: &Ty, count: usize) -> Value {
    match ty {
        Ty::Int => {
            let p = [MIN_INT, MAX_INT, 9007199254740993, -1, 0];
            Value::Int_array(Arc::new((0..count).map(|i| p[i % 5]).collect()))
        }
        Ty::Text => Value::Text_array(Arc::new((0..count).map(|i| if i % 2 == 0 { all_bytes() } else { "é日本😀\0".as_bytes().to_vec() }).collect::<rays_math::text_array::TextArray>())),
        ty => {
            let width = flow_syntax::packed_ops::scalar_width(ty).unwrap();
            let p = [-0., 0., f64::from_bits(1), 1., -1.];
            let data = Arc::new((0..count * width).map(|i| p[i % 5]).collect::<Vec<f64>>());
            match ty {
                Ty::Float => Value::Floats(1, data),
                Ty::Vec2 => Value::Floats(2, data),
                Ty::Vec3 => Value::Floats(3, data),
                Ty::Vec4 => Value::Floats(4, data),
                _ => unreachable!(),
            }
        }
    }
}

fn concat_expected(a: &Value, b: &Value) -> Value {
    fn cat<T: Clone>(a: &Arc<Vec<T>>, b: &Arc<Vec<T>>) -> Arc<Vec<T>> {
        Arc::new(a.iter().chain(b.iter()).cloned().collect())
    }
    match (a, b) {
        (Value::Int_array(a), Value::Int_array(b)) => Value::Int_array(cat(a, b)),
        (Value::Text_array(a), Value::Text_array(b)) => Value::Text_array(Arc::new(a.iter().chain(b.iter()).collect())),
        (Value::Floats(1, a), Value::Floats(1, b)) => Value::Floats(1, cat(a, b)),
        (Value::Floats(2, a), Value::Floats(2, b)) => Value::Floats(2, cat(a, b)),
        (Value::Floats(3, a), Value::Floats(3, b)) => Value::Floats(3, cat(a, b)),
        (Value::Floats(4, a), Value::Floats(4, b)) => Value::Floats(4, cat(a, b)),
        _ => unreachable!(),
    }
}
fn slice_expected(a: &Value, first: usize, count: usize) -> Value {
    fn sub<T: Clone>(a: &Arc<Vec<T>>, w: usize, first: usize, count: usize) -> Arc<Vec<T>> {
        Arc::new(a[w * first..w * (first + count)].to_vec())
    }
    match a {
        Value::Int_array(a) => Value::Int_array(sub(a, 1, first, count)),
        Value::Text_array(a) => Value::Text_array(Arc::new(a.iter().skip(first).take(count).collect())),
        Value::Floats(1, a) => Value::Floats(1, sub(a, 1, first, count)),
        Value::Floats(2, a) => Value::Floats(2, sub(a, 2, first, count)),
        Value::Floats(3, a) => Value::Floats(3, sub(a, 3, first, count)),
        Value::Floats(4, a) => Value::Floats(4, sub(a, 4, first, count)),
        _ => unreachable!(),
    }
}
/// Whether two array values share one allocation.
fn shares_storage(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int_array(a), Value::Int_array(b)) => Arc::ptr_eq(a, b),
        (Value::Text_array(a), Value::Text_array(b)) => Arc::ptr_eq(a, b),
        (Value::Floats(aw @ 1..=4, a), Value::Floats(bw, b)) if aw == bw => Arc::ptr_eq(a, b),
        _ => false,
    }
}

fn instantiated(ty: &Ty, table: Value, body: &str, indices: Vec<i64>) -> Value {
    let declarations = format!("[(data : (array {}) {})]", ty::to_string(ty), constant(ty, "0"));
    let workspace = composition_checked(&declarations, &format!("(let* [tested (fn [(i : int)] {body})] 0.)"));
    let evaluation = ok(e::r#static(&workspace, true, Some(&[("g".to_string(), vec![("data".to_string(), table)])])));
    let (_, records) = evaluation.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    let f = match &records[0].1 {
        Value::Fn(f) => *f,
        _ => panic!("expected a function"),
    };
    ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int], result: ty.clone() }, f, &[Value::Int_array(Arc::new(indices))]))
}

#[test]
fn concat_and_slice_copy_exactly() {
    let shared = Arc::new(vec![0.25; 12]);
    for width in 1..=4 {
        let a = Value::Floats(width, shared.clone());
        for other_width in 1..=4 {
            let b = Value::Floats(other_width, shared.clone());
            assert_eq!(crate::core_exec::phys_eq(&a, &b), width == other_width);
            if width != other_width {
                reject("E_ARRAY_TYPE", call("array/concat", vec![("first", a.clone()), ("second", b)]));
            }
        }
    }
    for ty in composition_types() {
        for count in [0usize, 1, 13, 4097, 5001, 32769] {
            let a = sample(&ty, count);
            let b = sample(&ty, 13);
            let before = (bits(&a), bits(&b));
            let concat = call("array/concat", vec![("first", a.clone()), ("second", b.clone())]).unwrap();
            assert_eq!(bits(&concat), bits(&concat_expected(&a, &b)));
            assert!(!shares_storage(&concat, &a) && !shares_storage(&concat, &b));
            assert_eq!((bits(&a), bits(&b)), before);
            for (first, length) in [(0, count), (count, 0), (count / 2, count - count / 2)] {
                let slice = call("array/slice", vec![("array", a.clone()), ("first", Value::Int(first as i64)), ("count", Value::Int(length as i64))]).unwrap();
                assert_eq!(bits(&slice), bits(&slice_expected(&a, first, length)));
                if length > 0 {
                    assert!(!shares_storage(&slice, &a));
                }
                assert_eq!((bits(&a), bits(&b)), before);
            }
        }
    }
}

#[test]
fn range_and_type_errors() {
    for (first, count) in [(-1, 0), (0, -1), (14, 0), (13, 1), (12, 2)] {
        reject("E_ARRAY_RANGE", call("array/slice", vec![("array", sample(&Ty::Int, 13)), ("first", Value::Int(first)), ("count", Value::Int(count))]));
    }
    for ty in composition_types() {
        if ty != Ty::Int {
            reject("E_ARRAY_TYPE", call("array/concat", vec![("first", sample(&Ty::Int, 1)), ("second", sample(&ty, 1))]));
        }
    }
}

fn gather_checks(ty: Ty) {
    for body in ["(array/nth (array/concat data data) i)", "(array/nth (array/slice data 0 13) i)"] {
        let table = sample(&ty, 13);
        for count in [0usize, 1, 13, 2049] {
            let value = instantiated(&ty, table.clone(), body, (0..count).map(|i| (i % 13) as i64).collect());
            let r = residual(&value);
            for fusion in [false, true] {
                let p = ok_ds(compile_region(r, fusion, None));
                assert!(!packed::gpu_refusals(false, false, &p).is_empty());
                let live = live_at(0.25);
                let before = bits(&table);
                for domains in [1, 4, 8] {
                    with_domains(domains, &|| same(&packed_force(&p, &live, true, None), &reference_with(&value, &live, None, None)));
                }
                assert_eq!(bits(&table), before);
            }
        }
    }
}
macro_rules! per_type {
    ($($name:ident: $ty:expr),*) => { $( #[test] fn $name() { gather_checks($ty); } )* };
}
per_type!(gather_int: Ty::Int, gather_float: Ty::Float, gather_vec2: Ty::Vec2, gather_vec3: Ty::Vec3, gather_vec4: Ty::Vec4, gather_text: Ty::Text);

fn has_array_prepare(program: &crate::core_ir::Program) -> bool {
    program.regions.iter().any(|r| {
        r.code.iter().any(|i| match i {
            Instruction::Array_prepare(..) => true,
            Instruction::Guarded(_, inner) => matches!(**inner, Instruction::Array_prepare(..)),
            _ => false,
        })
    })
}

fn composition_programs(filter: &dyn Fn(&str) -> bool) {
    let mut all = composition_cases();
    all.extend(composition_error_cases());
    for (name, body) in all.into_iter().filter(|(n, _)| filter(n)) {
        let value = composition_value(&body);
        let (rid, core) = composition_core(&value);
        let executor = executor_compile(&value, true);
        for count in [0i64, 13, 2049] {
            for time in [-0.25, 0.25] {
                let mut live = live_at(time);
                live.frame = count;
                live.tick = count;
                let reference = force_reference(&value, &live);
                for domains in [1, 4, 8] {
                    with_domains(domains, &|| {
                        same(&run(Some(rid), true, None, None, &core, &live), &reference);
                        same(&executor_force(&executor, true, &live), &reference);
                    });
                }
            }
        }
        if name.ends_with("inline-concat") || name.ends_with("inline-slice") {
            assert!(has_array_prepare(&core), "{name}");
        }
    }
}
macro_rules! per_prefix {
    ($($name:ident: $prefix:expr),*) => { $( #[test] fn $name() { composition_programs(&|n| n.starts_with($prefix)); } )* };
}
per_prefix!(programs_int: "int/", programs_float: "float/", programs_vec2: "vec2/", programs_vec3: "vec3/", programs_vec4: "vec4/", programs_text: "text/");

#[test]
fn programs_error_cases() {
    let names: Vec<String> = composition_error_cases().into_iter().map(|(n, _)| n).collect();
    composition_programs(&|n| names.iter().any(|x| x == n));
}

fn table_inputs(array_ty: &Ty) -> Vec<(String, Vec<(String, Value)>)> {
    vec![("g".to_string(), vec![("data".to_string(), source_value("fixture/table", array_ty.clone(), vec![]))])]
}

/// Invariant tables may capture live host data; no prepared array survives a force. Source
/// errors/types/layouts and selected callback order stay native.
fn source_checks(ty: Ty) {
    let array_ty = Ty::Array(Box::new(ty.clone()));
    let declarations = format!("[(data : (array {}) {})]", ty::to_string(&ty), constant(&ty, "0"));
    let text_body = "(map (fn [(i : int)] (array/nth (array/slice (array/concat data data) 0 (array/count data)) (int/mod i (array/count data)))) (array/range (frame/index) :type int))";
    let value = dynamic_value(&table_inputs(&array_ty), &declarations, text_body);
    let rid = residual(&value);
    let current = Arc::new(Mutex::new(sample(&ty, 13)));
    let resolve: ResolveArc = {
        let current = current.clone();
        Arc::new(move |v: &Value| match v {
            Value::Source(g) if g.name == "fixture/table" => Ok(current.lock().unwrap().clone()),
            _ => panic!("unexpected source"),
        })
    };
    for fusion in [false, true] {
        let p = ok_ds(compile_region(rid, fusion, None));
        assert!(crate::packed::count_origin(&p).is_none() && !packed::gpu_refusals(false, false, &p).is_empty());
        for size in [13usize, 3, 0, 13] {
            *current.lock().unwrap() = sample(&ty, size);
            let before = bits(&current.lock().unwrap());
            let mut live = live_at(0.25);
            live.frame = if size == 0 { 0 } else { 1030 };
            for domains in [1, 4, 8] {
                with_domains(domains, &|| same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve.clone()))));
            }
            assert_eq!(bits(&current.lock().unwrap()), before);
        }
        let mut live = live_at(0.25);
        live.frame = 13;
        let resolvers: Vec<ResolveArc> = vec![
            Arc::new(|_| {
                let mut d = flow_syntax::value::fail("E_DATA_SOURCE", "missing table");
                d.cause = Some("table cause".into());
                Err(d)
            }),
            Arc::new(|_| {
                let mut d = flow_syntax::value::fail("E_CANCELLED", "cancelled table");
                d.cause = Some("host cancel".into());
                Err(d)
            }),
            Arc::new(|_| Ok(Value::Int(0))),
            Arc::new(|_| Ok(Value::Floats(3, Arc::new(vec![1., 2.])))),
            Arc::new(|_| Ok(Value::Floats(1, Arc::new(vec![f64::NAN])))),
        ];
        for r in resolvers {
            same(&packed_force(&p, &live, true, Some(r.clone())), &reference_with(&value, &live, None, Some(r)));
        }
        let no_source: ResolveArc = Arc::new(|_| panic!("Empty gather forced composed table"));
        let mut empty = live.clone();
        empty.frame = 0;
        same(&packed_force(&p, &empty, true, Some(no_source.clone())), &reference_with(&value, &empty, None, Some(no_source)));
    }
    let dynamic = |body: &str| dynamic_value(&table_inputs(&array_ty), &declarations, body);
    for source in ["data", "(array/concat data data)", "(array/slice data 0 (array/count data))"] {
        let value = dynamic(&format!("(map (fn [x] x) {source})"));
        let r = residual(&value);
        for fusion in [false, true] {
            let p = ok_ds(compile_region(r, fusion, Some(data_count_source())));
            let origin = if source == "data" && fusion { Some((vec!["fixture".to_string(), "data".to_string()], vec![])) } else { None };
            assert_eq!(crate::packed::count_origin(&p), origin, "{source} fusion={fusion}");
            *current.lock().unwrap() = sample(&ty, 13);
            let live = live_at(0.25);
            same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve.clone())));
        }
    }
    let value = dynamic("(map (fn [a b] a) data (array/concat data data))");
    for fusion in [false, true] {
        let p = ok_ds(compile_region(residual(&value), fusion, Some(data_count_source())));
        assert!(crate::packed::count_origin(&p).is_none() && p.sources.len() == 2);
    }
}
macro_rules! per_type_sources {
    ($($name:ident: $ty:expr),*) => { $( #[test] fn $name() { source_checks($ty); } )* };
}
per_type_sources!(sources_int: Ty::Int, sources_float: Ty::Float, sources_vec2: Ty::Vec2, sources_vec3: Ty::Vec3, sources_vec4: Ty::Vec4, sources_text: Ty::Text);

/// Honest row dependence still refuses; no source equality is invented by composition, even when
/// both maps have a host count-origin hook.
#[test]
fn row_dependence_refuses() {
    for body in ["(array/nth (array/slice data i 1) 0)", "(array/nth (array/concat data (array/int i)) 0)"] {
        let v = instantiated(&Ty::Int, sample(&Ty::Int, 13), body, vec![0, 1, 2]);
        assert!(compile_region(residual(&v), true, None).is_err(), "{body}");
    }
}

#[test]
fn argument_errors_precede_the_table() {
    for body in [
        "(array/nth (array/concat data data) (error \"E_INDEX\" \"bad index\" :cause \"index cause\"))",
        "(array/nth (array/slice data (error \"E_FIRST\" \"bad first\") 1) (error \"E_INDEX\" \"bad index\"))",
        "(let* [unused (array/concat data data)] (error \"E_INDEX\" \"after alias\"))",
        "(if (int/lt i 0) (array/nth (array/concat data data) 0) i)",
    ] {
        let value = instantiated(&Ty::Int, source_value("fixture/order", Ty::Array(Box::new(Ty::Int)), vec![]), body, vec![0, 1, 2]);
        let p = ok_ds(compile_region(residual(&value), true, None));
        let calls = Arc::new(Mutex::new(0));
        let resolve: ResolveArc = {
            let calls = calls.clone();
            Arc::new(move |_| {
                *calls.lock().unwrap() += 1;
                let mut d = flow_syntax::value::fail("E_DATA_SOURCE", "bad table");
                d.cause = Some("order cause".into());
                Err(d)
            })
        };
        let live = live_at(0.25);
        same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve)));
        if body.starts_with("(if") {
            assert_eq!(*calls.lock().unwrap(), 0);
        }
    }
}

/// Two separately checked closures capture the same spelling. Supply the helper through the
/// existing callable-binding API: graph-local shadowing, Fn graph inputs and escaping Fn results
/// are correctly refused.
// Product defect: map_function on the template body fails E_FUNCTION_ID ("Function belongs to an expired evaluation") at
// the `(helper ...)` call: the helper FnId lives in another evaluation's arena (get_fn is per arena in flow_eval), where OCaml
// function ids are global.
#[test]
fn captured_helpers() {
    let helpers: Vec<(&str, Box<dyn Fn(f64) -> i64>)> = vec![
        ("(array/nth (array/concat data data) j)", Box::new(|_| 9)),
        ("(array/nth (if (> t 0.) data (array/int 1 11)) 0)", Box::new(|time| if time > 0. { 9 } else { 11 })),
        ("(array/count (array/concat data data))", Box::new(|_| 18)),
    ];
    for (helper_body, expected) in helpers {
        for count in [0i64, 13, 2049] {
            let value = helper_value(helper_body, count).unwrap();
            let r = residual(&value);
            let (_, core) = composition_core(&value);
            let executor = executor_compile(&value, true);
            for fusion in [false, true] {
                let p = ok_ds(compile_region(r, fusion, None));
                for time in [-0.25, 0.25] {
                    let live = live_at(time);
                    let reference = force_reference(&value, &live);
                    let want = Value::Int_array(Arc::new(vec![expected(time); count as usize]));
                    assert_eq!(bits(reference.as_ref().unwrap()), bits(&want));
                    for domains in [1, 8] {
                        with_domains(domains, &|| {
                            same(&packed_force(&p, &live, true, None), &reference);
                            same(&run(Some(r), true, None, None, &core, &live), &reference);
                            same(&executor_force(&executor, true, &live), &reference);
                        });
                    }
                }
            }
        }
    }
}

#[test]
fn stateful_slice_matches_the_reference() {
    let stateful = composition_value("(let* [counter (state [s 0] (int/add s 1))] (array/slice (array/concat (array/int 1 counter) (array/int 1)) (if (> t 0.) 3 0) 1))");
    let executor = executor_compile(&stateful, true);
    let a = e::create_state();
    let b = e::create_state();
    for (frame, time) in [-1., 1., -1., 1., -1.].iter().enumerate() {
        let mut live = live_frame(*time, frame as i64);
        live.tick = frame as i64;
        same(&exec_force(&executor, &live, Some(&a), None), &reference_with(&stateful, &live, Some(&b), None));
        let sa: Vec<String> = e::Host::state_values(&a).iter().map(bits).collect();
        let sb: Vec<String> = e::Host::state_values(&b).iter().map(bits).collect();
        assert_eq!(sa, sb);
    }
    let _ = Executor::kept_programs;
}
