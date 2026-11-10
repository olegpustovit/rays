// The Value-only blocks (storage,
// validation, allocation, the GC-weak memo) belong to flow_syntax's value tests.
use crate::*;
use crate::test_workspace::{errors, show_ds};
use flow_check::{catalog::Catalog, workspace as W};
use flow_syntax::{syntax, ty};
use std::sync::{Arc, atomic::{AtomicI64, Ordering}};

fn check(ctx: &str, body: &str) -> W::Workspace {
    let src = format!("(workspace w (graph g :context {ctx} {body}))");
    match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &syntax::parse(&src).unwrap()) { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{body}: {}", show_ds(&ds)) }
}
fn g(e: &Eval) -> Value { e.results.iter().find(|(n, _)| n == "g").unwrap().1.clone() }
fn value(ctx: &str, body: &str) -> Value { g(&run(&check(ctx, body), false, None, None, None, 0.).unwrap_or_else(|d| panic!("{body}: {}", rays_diagnostic::to_string(&d)))) }
fn at(time: f64, frame: i64, dt: f64) -> input::Frame { let mut f = input::at_time(time); f.frame = frame; f.dt = dt; f }
fn text_array(v: &[&str]) -> Value { Value::Text_array(Arc::new(v.iter().map(|s| s.as_bytes().to_vec()).collect())) }

#[test]
fn packed_arrays_through_the_evaluator() {
    assert_eq!(value("host", "(array/nth (map (fn [x] [x (+ x 1)]) (array/range 3)) 2)"), Value::Vec2(2., 3.));
    assert_eq!(value("host", "(let* [a (map (fn [x] [x 2 3 4]) (array/range 3))]\n    (array/nth (array/concat (array/slice a 2 1) (array/slice a 0 1)) 1))"), Value::Vec4(0., 2., 3., 4.));
    assert_eq!(value("host", "(array/sum (map (fn [x] [x 1]) (array/range 3)))"), Value::Vec2(3., 3.));
    assert_eq!(value("host", "(array/sum (map (fn [x] [x 1 2 3]) (array/range 3)))"), Value::Vec4(3., 3., 6., 9.));
    assert_eq!(value("host", "(array/sum (map (fn [x] [x 1 2 3]) (array/range 0)))"), Value::Vec4(0., 0., 0., 0.));
    assert_eq!(value("value", "(array/count (array/float 10000))"), Value::Int(10000));
    assert_eq!(value("value", "(array/sum (array/float 10000 2))"), Value::Float(20000.));
    assert_eq!(value("value", "(let* [p (array/nth (array/vec3 10000 [1 2 3]) 9999)] p.x)"), Value::Float(1.));
    assert_eq!(value("value", "(array/sum (map (fn [x] (+ x 1)) (array/range 10000)))"), Value::Float(50005000.));
    assert_eq!(value("value", "(fold [s 0.0] [x (array/float 10000 2)] (+ s x))"), Value::Float(20000.));
    assert_eq!(value("value", "(sum [x (array/float 10000 2)] x)"), Value::Float(20000.));
    assert_eq!(value("value", "(array/count (for [x (array/range 10000)] [x 0 0]))"), Value::Int(10000));
    assert_eq!(value("value", "(array/count (filter (fn [x] (> x 4999)) (array/range 10000)))"), Value::Int(5000));
    assert_eq!(value("value", "(array/nth (sort-by (fn [x] (- 0 x)) (array/range 10)) 0)"), Value::Float(9.));
    let w = check("value", "(let* [p (state [prev (array/float 10000)] (map (fn [x] (+ x (frame/dt))) prev))] (array/sum p))");
    let export = || { let s = create_state(); (0..5).map(|frame| g(&run(&w, false, None, Some(&s), Some(&at(frame as f64, frame, 0.125)), frame as f64).unwrap())).collect::<Vec<_>>() };
    assert_eq!(export(), vec![Value::Float(1250.), Value::Float(2500.), Value::Float(3750.), Value::Float(5000.), Value::Float(6250.)]);
    assert_eq!(export(), export());
    assert_eq!(ty::of_string("array:vec3"), Some(Ty::Array(Box::new(Ty::Vec3))));
    assert_eq!(ty::of_string("array:text"), Some(Ty::Array(Box::new(Ty::Text))));
    assert!(static_eval(&check("value", "(array/count (array/float -1))")).is_err());
    assert!(static_eval(&check("value", "(array/nth (array/float 10) 10)")).is_err());
    assert!(run(&check("value", "(sum [x (array/float 2 1e308)] x)"), false, None, None, None, 0.).is_err());
    let typed = check("value", "(array/count (array/float (frame/index)))");
    assert_eq!(g(&run(&typed, false, None, None, Some(&at(0., 10000, 0.)), 0.).unwrap()), Value::Int(10000));
}

#[test]
fn live_packed_maps_defer() {
    assert_eq!(value("value", "(array/sum (map (fn [x] (+ x t)) (array/float 10 2)))"), Value::Float(20.));
    let live_value = |body: &str| g(&run(&check("value", body), false, None, None, None, 1.25).unwrap());
    assert_eq!(live_value("(array/sum (map (fn [x] (+ x t)) (array/float 10 2)))"), Value::Float(32.5));
    assert_eq!(live_value("(array/sum (for [x (array/float 10 2)] (+ x t)))"), Value::Float(32.5));
    assert_eq!(live_value("(array/sum (scan [s 0.0] [x (array/float 3 2)] (+ s (+ x t))))"), Value::Float(19.5));
    assert_eq!(live_value("(let* [p (array/nth (map (fn [p] (+ p [t 0 0])) (array/vec3 3 [1 2 3])) 2)] p.x)"), Value::Float(2.25));
    let (_, ds) = W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &syntax::parse("(workspace w (graph g :context value (map + (list 1 2) (array/float 2))))").unwrap());
    assert!(ds.iter().any(|d| d.code == "E_ARRAY_TYPE"));
    let _ = errors(&[]);
}

#[test]
fn text_arrays_through_the_evaluator() {
    let text = "\"猫😀é\"";
    let expected = text_array(&vec!["猫😀é"; 5001]);
    assert_eq!(value("host", "(array/text 3)"), text_array(&["", "", ""]));
    for body in [format!("(array/text 5001 {text})"), format!("(map (fn [x] x) (array/text 5001 {text}))"), format!("(for [x (array/text 5001 {text})] x)"), format!("(scan [a \"\"] [x (array/text 5001 {text})] x)")] {
        assert_eq!(value("host", &body), expected, "{body}");
    }
    assert_eq!(value("value", "(array/count (array/text 5001))"), Value::Int(5001));
    assert_eq!(value("host", "(for [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] :skip [5001] y)"), text_array(&vec!["😀"; 5001]));
    assert_eq!(value("host", "(scan [a \"seed\"] [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] y)"), text_array(&vec!["😀"; 5002]));
    assert_eq!(value("host", &format!("(array/concat (array/text 2 {text}) (array/text 1 \"other\"))")), text_array(&["猫😀é", "猫😀é", "other"]));
    assert_eq!(value("host", &format!("(array/slice (array/text 5001 {text}) 4999 2)")), text_array(&["猫😀é", "猫😀é"]));
    assert_eq!(value("value", "(fold [a 0] [x (array/text 5001)] (int/add a 1))"), Value::Int(5001));
    assert_eq!(value("value", "(reduce (fn [a x] (int/add a 1)) 0 (array/text 5001))"), Value::Int(5001));
    for body in ["(map (fn [x] x) (array/text 0))", "(for [x (array/text 0)] x)", "(scan [a \"seed\"] [x (array/text 0)] x)", "(array/slice (array/text 1) 1 0)", "(for [x (array/text 0) y (array/text -1)] y)", "(for [x (array/text 2) y (array/text 0)] y)", "(scan [a \"seed\"] [x (array/text 0) y (array/text -1)] y)", "(array/concat (array/text 0) (array/text 0))"] {
        assert_eq!(value("host", body), text_array(&[]), "{body}");
    }
    assert_eq!(value("host", "(fold [a \"seed\"] [x (array/text 0)] x)"), Value::Text(b"seed".as_slice().into()));
    let bulk_calls = Arc::new(AtomicI64::new(0));
    let c = bulk_calls.clone();
    let handlers = ResidualHandlers { bulk: Some(Arc::new(move |_| { c.fetch_add(1, Ordering::SeqCst); None })), ..Default::default() };
    let w = check("host", "(map (fn [x] x) (array/text 5001 \"猫\"))");
    residual::handle(handlers, &mut || host::static_with_requests(None, false, None, &w)).unwrap();
    assert_eq!(bulk_calls.load(Ordering::SeqCst), 0);
    for body in ["(array/text -1)", "(array/nth (array/text 0) 0)", "(array/slice (array/text 1) 0 2)", "(array/concat (array/text 1) (array/int 1))", "(array/sum (array/text 0))"] {
        assert!(static_eval(&check("host", body)).is_err(), "{body}");
    }
}

// Too slow for the default loop in a debug build (about ten seconds); RAYS_SLOW=1 runs it.
#[test]
fn live_packed_maps_large_and_budgeted() {
    if !std::env::var("RAYS_SLOW").is_ok_and(|v| v == "1") { return; }
    let live_value = |body: &str| g(&run(&check("value", body), false, None, None, None, 1.25).unwrap());
    let bounded = check("value", "(array/sum (map (fn [x] (sum [i (range 4096)] (+ x (+ i t)))) (array/float 1024)))");
    match run(&bounded, false, None, None, None, 1.) { Err(d) => assert_eq!(d.code, "E_EVAL_BUDGET"), Ok(_) => panic!("packed block bypassed the evaluation budget") }
    assert_eq!(live_value("(array/sum (map (fn [x] (+ x t)) (array/float 100000 2)))"), Value::Float(325000.));
    assert_eq!(live_value("(sum [x (array/float 100000 2)] (+ x t))"), Value::Float(325000.));
    assert_eq!(live_value("(fold [s 0.0] [x (array/float 100000 2)] (+ s (+ x t)))"), Value::Float(325000.));
}
