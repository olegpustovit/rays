// Port of lib/flow_ir/test_grade_index.ml.
use crate::flow_ir::Executor;
use crate::test_support::*;
use flow_eval as e;
use flow_syntax::ty::Ty;
use flow_syntax::value::{Value, source_value};
use rays_diagnostic::Diagnostic;
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};

const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

fn ints(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}
fn floats(v: &[f64]) -> Value {
    Value::Float_array(Arc::new(v.to_vec()))
}
fn texts(v: &[Vec<u8>]) -> Value {
    Value::Text_array(Arc::new(v.to_vec().into()))
}
fn grade(keys: Value, descending: bool) -> Result<Value, Diagnostic> {
    call_op("array/grade", vec![("keys", keys), ("descending", Value::Bool(descending))])
}
fn lookup(keys: Value, queries: Value) -> Result<Value, Diagnostic> {
    call_op("array/index_of", vec![("keys", keys), ("queries", queries)])
}
fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) {
    match (a, b) {
        (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
        _ => panic!("Grade/lookup reference disagreement: {:?} / {:?}", a.as_ref().map(bits), b.as_ref().map(bits)),
    }
}
fn live_value(body: &str) -> Value {
    composition_value(body)
}

/// A stable sort of 0..count by `compare`; descending swaps its arguments.
fn oracle_grade(compare: &dyn Fn(usize, usize) -> Ordering, count: usize, descending: bool) -> Vec<i64> {
    let mut order: Vec<usize> = (0..count).collect();
    if descending {
        order.sort_by(|a, b| compare(*b, *a));
    } else {
        order.sort_by(|a, b| compare(*a, *b));
    }
    order.into_iter().map(|i| i as i64).collect()
}
fn oracle_lookup<T: PartialEq>(keys: &[T], queries: &[T]) -> Vec<i64> {
    queries.iter().map(|q| keys.iter().position(|k| k == q).map_or(-1, |i| i as i64)).collect()
}

fn raw() -> Vec<Vec<u8>> {
    vec![
        b"".to_vec(),
        b"a".to_vec(),
        b"a".to_vec(),
        b"x\0y".to_vec(),
        vec![255, 128],
        "é日本😀".as_bytes().to_vec(),
        vec![240, 144, 128, 128],
        vec![238, 128, 128],
        (0..=255u8).collect(),
    ]
}
fn every_byte_descending() -> Vec<Vec<u8>> {
    (0..256).map(|i| vec![(255 - i) as u8]).collect()
}

#[test]
fn grade_direct_orders() {
    assert_eq!(grade(ints(&[2, 1, 2]), false).unwrap(), ints(&[1, 0, 2]));
    assert_eq!(grade(ints(&[2, 1, 2]), true).unwrap(), ints(&[0, 2, 1]));
    assert_eq!(grade(floats(&[-0., 0., -0., 0.]), true).unwrap(), ints(&[0, 1, 2, 3]));
    for count in [0usize, 1, 13, 32769] {
        let int_pattern = [MIN_INT, MAX_INT, 9007199254740993, 9007199254740992, 0, -1, 1];
        let integers: Vec<i64> = (0..count).map(|i| int_pattern[i % 7]).collect();
        let float_pattern = [-0., 0., f64::from_bits(1), -f64::MAX, f64::MAX, 1., -1.];
        let fl: Vec<f64> = (0..count).map(|i| float_pattern[i % 7]).collect();
        let raw = raw();
        let tx: Vec<Vec<u8>> = (0..count).map(|i| raw[i % raw.len()].clone()).collect();
        let cases: Vec<(Value, Box<dyn Fn(usize, usize) -> Ordering>)> = vec![
            (ints(&integers), Box::new({
                let v = integers.clone();
                move |a, b| v[a].cmp(&v[b])
            })),
            (floats(&fl), Box::new({
                let v = fl.clone();
                move |a, b| v[a].partial_cmp(&v[b]).unwrap()
            })),
            (texts(&tx), Box::new({
                let v = tx.clone();
                move |a, b| v[a].cmp(&v[b])
            })),
        ];
        for (keys, compare) in cases {
            let before = bits(&keys);
            for descending in [false, true] {
                let expected = ints(&oracle_grade(&*compare, count, descending));
                for domains in [1, 4, 8] {
                    with_domains(domains, &|| {
                        let got = grade(keys.clone(), descending).unwrap();
                        assert_eq!(bits(&got), bits(&expected));
                        assert_eq!(bits(&keys), before);
                    });
                }
            }
        }
    }
    // Byte order differs from UTF-16 order for these two valid Unicode strings.
    assert_eq!(grade(texts(&[vec![240, 144, 128, 128], vec![238, 128, 128]]), false).unwrap(), ints(&[1, 0]));
    assert_eq!(grade(texts(&every_byte_descending()), false).unwrap(), ints(&(0..256).map(|i| 255 - i).collect::<Vec<_>>()));
}

#[test]
fn index_of_first_match() {
    let all = every_byte_descending();
    for count in [0usize, 1, 13, 32769] {
        let keys: Vec<i64> = (0..count).map(|i| if i % 3 == 0 { MIN_INT } else { 9007199254740993 }).collect();
        let pattern = [MAX_INT, MIN_INT, 9007199254740993, 9007199254740992];
        let queries: Vec<i64> = (0..count + 7).map(|i| pattern[i % 4]).collect();
        // These repeated-key fixtures have all first occurrences in their initial period; keep the
        // independent linear oracle bounded for the large tail.
        let raw = raw();
        let text_keys: Vec<Vec<u8>> = (0..count).map(|i| raw[i % raw.len()].clone()).collect();
        let mut text_queries = raw.clone();
        text_queries.extend(all.iter().cloned());
        let cases = vec![
            (ints(&keys), ints(&queries), oracle_lookup(&keys[..count.min(3)], &queries)),
            (texts(&text_keys), texts(&text_queries), oracle_lookup(&text_keys[..count.min(raw.len())], &text_queries)),
        ];
        for (k, q, expected) in cases {
            let original = (bits(&k), bits(&q));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let got = lookup(k.clone(), q.clone()).unwrap();
                    assert_eq!(bits(&got), bits(&ints(&expected)));
                    assert_eq!((bits(&k), bits(&q)), original);
                });
            }
        }
    }
    assert_eq!(
        lookup(texts(&[b"a".to_vec(), b"a".to_vec(), b"b".to_vec()]), texts(&[b"b".to_vec(), b"a".to_vec(), b"missing".to_vec()])).unwrap(),
        ints(&[2, 0, -1])
    );
}

#[test]
fn bad_inputs() {
    for x in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        reject("E_NONFINITE", grade(floats(&[0., x]), false));
    }
    for bad in [Value::Vec3_array(Arc::new(vec![0., 0., 0.])), Value::Int(1), Value::Text(b"a".to_vec()), Value::List(Arc::new(vec![Value::Int(1)]))] {
        reject("E_ARRAY_TYPE", grade(bad, false));
    }
    for (a, b) in [(ints(&[]), texts(&[])), (floats(&[]), floats(&[])), (Value::Int(1), ints(&[]))] {
        reject("E_ARRAY_TYPE", lookup(a, b));
    }
}

fn check_bodies(bodies: &[&str]) {
    for body in bodies {
        let body = *body;
        let value = live_value(body);
        let r = residual(&value);
        let core = lower_strict(r).unwrap_or_else(|f| panic!("{f}"));
        let executor = executor_compile(&value, true);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                for time in [-1., 0., 0.25, 2.] {
                    let live = live_frame(time, 3);
                    let expected = force_reference(&value, &live);
                    same(&run(None, false, None, None, &core, &live), &expected);
                    same(&executor_force(&executor, true, &live), &expected);
                }
            });
        }
    }
}

#[test]
fn grade_programs_match_the_reference() {
    // Sizes: OCaml used 32769 / 5001 rows; 2049 (1100 for the gather-style bodies) keeps one or two 1024-lane block
    // crossings within the 5 s debug budget.
    check_bodies(&[
        "(array/grade (map (fn [(i : int)] (int/mod (int/add i (frame/index)) 7)) (array/range 2049 :type int)) :descending true)",
        "(array/grade (map (fn [x] (if (> t 0.) x (- 0. x))) (array/range 2049)))",
        "(array/grade (map (fn [(x : text)] (if (> t 0.) x \"safe\")) (array/text 2049 \"é日本😀\")) :descending true)",
    ]);
}

#[test]
fn index_of_programs_match_the_reference() {
    check_bodies(&[
        "(array/index_of (array/int 3 9007199254740993) (map (fn [(i : int)] (if (int/eq i 0) 9007199254740993 i)) (array/range 2049 :type int)))",
        "(array/index_of (array/text 2 \"a\") (map (fn [(x : text)] (if (> t 0.) x \"missing\")) (array/text 1100 \"a\")))",
    ]);
}

#[test]
fn small_and_gather_programs_match_the_reference() {
    check_bodies(&[
        "(array/grade (array/text 0))",
        "(array/index_of (array/int 0) (array/int 13 1))",
        "(let* [rank (fn [(keys : (array int))] (array/grade keys :descending true))] (rank (array/int 13 (frame/index))))",
        "(let* [table (array/range 1100) order (array/grade table :descending true)] (map (fn [(i : int)] (array/nth table i)) order))",
        "(map (fn [(i : int)] (int/add i (frame/index))) (array/index_of (array/int 2 3) (array/range 1100 :type int)))",
        "(if (> t 0.) (array/int 0) (array/grade (error \"E_KEYS\" \"bad keys\" :cause \"key cause\")))",
        "(array/index_of (error \"E_KEYS\" \"keys first\" :cause \"raw cause\") (error \"E_QUERIES\" \"queries second\"))",
    ]);
}

#[test]
fn unsupported_and_dynamic_programs_refuse() {
    let unsupported = live_value("(array/grade (map (fn [x] (value/rand x t)) (array/range 2049)))");
    match Executor::compile(&unsupported, &Executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) => assert_eq!(d.code, "E_KERNEL_FORM"),
        Ok(_) => panic!("Unsupported producer accepted"),
    }
    for body in ["(array/nth (array/grade (array/int i)) 0)", "(array/nth (array/index_of (array/int i) (array/int 1 i)) 0)"] {
        let value = live_value(&format!("(map (fn [(i : int)] {body}) (array/range 2049 :type int))"));
        assert!(compile_region(residual(&value), true, None).is_err(), "{body}");
    }
}

fn declarations() -> &'static str {
    "[(keys : (array int) (array/int 0)) (queries : (array int) (array/int 0))]"
}
fn inputs() -> Vec<(String, Vec<(String, Value)>)> {
    vec![(
        "g".to_string(),
        vec![
            ("keys".to_string(), source_value("fixture/keys", Ty::Array(Box::new(Ty::Int)), vec![])),
            ("queries".to_string(), source_value("fixture/queries", Ty::Array(Box::new(Ty::Int)), vec![])),
        ],
    )]
}
fn dynamic(body: &str) -> Value {
    dynamic_value(&inputs(), declarations(), body)
}
fn count_source() -> crate::core_ir::Region::CountSource {
    crate::core_ir::Region::CountSource::new(Arc::new(|_, term| match &term.node {
        flow_check::workspace::Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() && term.name(term.arena().ref_binding(*id).name) == "keys" => Some((vec!["fixture".to_string(), "keys".to_string()], vec![])),
        flow_check::workspace::Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() && term.name(term.arena().ref_binding(*id).name) == "queries" => Some((vec!["fixture".to_string(), "queries".to_string()], vec![])),
        _ => None,
    }))
}

#[test]
fn derived_arrays_have_no_count_origin() {
    for operation in ["(array/grade keys)", "(array/index_of keys queries)"] {
        let value = dynamic(&format!("(map (fn [(i : int)] (int/add i (frame/index))) {operation})"));
        let r = residual(&value);
        for fusion in [false, true] {
            let p = ok_ds(compile_region(r, fusion, Some(count_source())));
            assert!(crate::packed::count_origin(&p).is_none());
            let current = Arc::new(Mutex::new((vec![2, 1, 2], vec![2, 9, 1, 2])));
            let resolve: ResolveArc = {
                let current = current.clone();
                Arc::new(move |v: &Value| match v {
                    Value::Source(g) if g.name == "fixture/keys" => Ok(ints(&current.lock().unwrap().0)),
                    Value::Source(g) if g.name == "fixture/queries" => Ok(ints(&current.lock().unwrap().1)),
                    _ => panic!("unexpected source"),
                })
            };
            for buffers in [(vec![2, 1, 2], vec![2, 9, 1, 2]), (vec![MIN_INT, MAX_INT], vec![MAX_INT]), (vec![], vec![0, 0])] {
                *current.lock().unwrap() = buffers.clone();
                let before = format!("{:?}", buffers);
                for domains in [1, 4, 8] {
                    with_domains(domains, &|| {
                        let live = live_frame(1., 3);
                        same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve.clone())));
                        assert_eq!(format!("{:?}", buffers), before);
                    });
                }
            }
        }
    }
    for result in ["(array/grade keys)", "(array/index_of keys queries)"] {
        let value = dynamic(&format!("(map (fn [(a : int) (b : int)] (int/add a b)) keys {result})"));
        let r = residual(&value);
        for fusion in [false, true] {
            let p = ok_ds(compile_region(r, fusion, Some(count_source())));
            assert!(crate::packed::count_origin(&p).is_none() && crate::packed::static_count(&p).is_none() && p.sources.len() == 2);
            let resolve: ResolveArc = Arc::new(|v: &Value| match v {
                Value::Source(g) if g.name == "fixture/keys" => Ok(ints(&[2, 1, 2, 0])),
                Value::Source(g) if g.name == "fixture/queries" => Ok(ints(&[1, 9])),
                _ => panic!("unexpected source"),
            });
            let live = live_at(1.);
            same(&packed_force(&p, &live, true, Some(resolve.clone())), &reference_with(&value, &live, None, Some(resolve)));
        }
    }
}

#[test]
fn source_errors_untaken_sources_and_state() {
    let value = dynamic("(array/index_of keys queries)");
    let executor = executor_compile(&value, true);
    let live = live_at(1.);
    let missing: ResolveArc = Arc::new(|v: &Value| match v {
        Value::Source(g) if g.name == "fixture/keys" => {
            let mut d = flow_syntax::value::fail("E_DATA_SOURCE", "missing keys");
            d.cause = Some("lookup source cause".into());
            Err(d)
        }
        Value::Source(g) if g.name == "fixture/queries" => Ok(ints(&[])),
        _ => panic!("unexpected source"),
    });
    same(&exec_force(&executor, &live, None, Some(missing.clone())), &reference_with(&value, &live, None, Some(missing)));
    let wrong: ResolveArc = Arc::new(|_| Ok(texts(&[b"wrong".to_vec()])));
    same(&exec_force(&executor, &live, None, Some(wrong.clone())), &reference_with(&value, &live, None, Some(wrong)));
    let skipped = dynamic("(if (> t 0.) (array/int 0) (array/index_of keys queries))");
    let no_source: ResolveArc = Arc::new(|_| panic!("Untaken lookup forced Source"));
    same(
        &exec_force(&executor_compile(&skipped, true), &live, None, Some(no_source.clone())),
        &reference_with(&skipped, &live, None, Some(no_source)),
    );

    let stateful = live_value("(let* [counter (state [s 0] (int/add s 1))] (if (> t 0.) (array/index_of (error \"E_KEYS\" \"rollback\" :cause \"same cause\") (array/int 0)) (array/grade (array/int 13 counter))))");
    let a = e::create_state();
    let b = e::create_state();
    let executor = executor_compile(&stateful, true);
    for (frame, time) in [-1., 1., -1., 1., -1.].iter().enumerate() {
        let mut live = live_frame(*time, frame as i64);
        live.tick = frame as i64;
        same(&exec_force(&executor, &live, Some(&a), None), &reference_with(&stateful, &live, Some(&b), None));
        let sa: Vec<String> = e::Host::state_values(&a).iter().map(bits).collect();
        let sb: Vec<String> = e::Host::state_values(&b).iter().map(bits).collect();
        assert_eq!(sa, sb);
    }
}
