// Port of lib/flow_ir/test_core_operators.ml.
use crate::core_lower;
use crate::flow_ir::Executor;
use crate::test_support::{self as ts, ok};
use flow_check::op::{self, Shape};
use flow_eval as E;
use flow_syntax::context;
use flow_syntax::value::Value;
use input::{Frame, Key, MouseButton};
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

fn bits(x: f64, y: f64) -> bool { x.to_bits() == y.to_bits() }

fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => bits(*x, *y),
        (Value::Vec3(a, b, c), Value::Vec3(x, y, z)) => bits(*a, *x) && bits(*b, *y) && bits(*c, *z),
        (Value::List(a), Value::List(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| same(a, b)),
        (Value::Float_array(a), Value::Float_array(b))
        | (Value::Vec2_array(a), Value::Vec2_array(b))
        | (Value::Vec3_array(a), Value::Vec3_array(b))
        | (Value::Vec4_array(a), Value::Vec4_array(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| bits(*x, *y)),
        (a, b) => a == b,
    }
}

// Every value-context scalar operator the closure compiler accepts, with at least one
// live expression. The coverage check below fails when an operator is added without a case.
const CASES: &[(&str, &str)] = &[
    ("array/max_length", "(int/sub (array/max_length :type int) (frame/index))"),
    ("text/repr", r##"(text/repr (str "é日本😀\t\n\\\"" t))"##),
    ("fma", "(fma t 0.9999999999999998 -1.)"),
    ("float/next_after", "(float/next_after t (- t 1.0))"),
    ("float/mul_checked", "(float/mul_checked t 1.7976931348623157e308)"),
    ("float/fma_checked", "(float/fma_checked t 1.7976931348623157e308 -1.7976931348623157e308)"),
    ("array/vec4", "(array/nth (array/vec4 4 [t 1 2 3]) 2)"),
    ("text/table_decode", r##"(text/table_decode (if (> t 0.) "a\t猫\n" ""))"##),
    ("text/split", r##"(array/nth (text/split (str "a,é," t) ",") 1)"##),
    ("text/tokens", r##"(array/nth (text/tokens (str "a, é\t" t "\n")) 1)"##),
    ("text/to_float", r##"(text/to_float (str " -1.5e1" (int t)))"##),
    ("text/to_int", r##"(text/to_int (str " 4" (int t)))"##),
    ("text/table_encode", r##"(text/table_encode (list (list (if (> t 0.) "猫\n" "a\t") "\\")))"##),
    ("text/blank?", r##"(text/blank? (if (> t 0.) " \t\r\n" " "))"##),
    ("array/grade", "(array/nth (array/grade (array/float 13 t) :descending true) 0)"),
    ("array/index_of", "(array/nth (array/index_of (array/int 13 (frame/index)) (array/int 13 3)) 0)"),
    ("array/compact", "(array/count (array/compact (array/int 13 (if (> t 0.) 1 0))))"),
    ("array/scan_sum", "(array/nth (array/scan_sum (array/int 13 (frame/index))) 13)"),
    ("array/scatter_add", "(array/nth (array/scatter_add (array/int 7 0) (array/vec3 7 [t 1 2]) 3) 0)"),
    ("array/min", "(array/min (array/float 7 t))"),
    ("array/max", "(array/max (array/int 7 (frame/index)))"),
    ("array/segment_sum", "(array/nth (array/segment_sum (array/vec3 5 [t 1 2]) (map (fn [x] (int x)) (array/range 6))) 3)"),
    ("array/segment_min", "(array/nth (array/segment_min (array/float 5 t) (map (fn [x] (int x)) (array/range 6))) 3)"),
    ("array/segment_max", "(array/nth (array/segment_max (array/int 5 (frame/index)) (map (fn [x] (int x)) (array/range 6))) 3)"),
    ("array/segment_argmax", "(array/nth (array/segment_argmax (array/int 5 (frame/index)) (map (fn [x] (int x)) (array/range 6))) 3)"),
    ("tan", "(tan t)"),
    ("atan2", "(atan2 t -0.3)"),
    ("asin", "(asin (* t 0.1))"),
    ("acos", "(acos (* t 0.1))"),
    ("log", "(log (+ t 4.0))"),
    ("sign", "(sign t)"),
    ("rsqrt", "(rsqrt (+ t 4.0))"),
    ("hypot", "(hypot t 3.0)"),
    ("ldexp", "(ldexp t 3.0)"),
    ("dot", "(dot [t 0.3 -0.2] [0.7 -0.2 1.1])"),
    ("cross", "(cross [t 0.3 -0.2] [0.7 -0.2 1.1])"),
    ("normalize", "(normalize [t 0.3 -0.2])"),
    ("frame/input", "(count (get (frame/input) :keys))"),
    ("frame/dt", "(+ (frame/dt) t)"),
    ("frame/index", "(* t (frame/index))"),
    ("frame/width", "(* t (frame/width))"),
    ("frame/height", "(* t (frame/height))"),
    ("pointer/x", "(+ t (pointer/x))"),
    ("pointer/y", "(+ t (pointer/y))"),
    ("pointer/down", "(if (pointer/down) t 0.)"),
    ("pointer/down", r##"(if (pointer/down "right") t 0.)"##),
    ("pointer/down", r##"(if (pointer/down "x1") t 0.)"##),
    ("pointer/down", r##"(if (pointer/down "nope") t 0.)"##),
    ("key/down", r##"(if (key/down "space") t 0.)"##),
    ("key/down", r##"(if (key/down "zzz") t 0.)"##),
    ("array/range", "(array/nth (array/range 8) (+ 1 (int t)))"),
    ("array/float", "(array/nth (array/float 4 t) 1)"),
    ("array/int", "(array/nth (array/int 4 (frame/index)) 1)"),
    ("array/text", r##"(array/nth (array/text 4 (if (> t 0.) "猫" "😀")) 1)"##),
    ("array/text", r##"(array/nth (array/text 4 (if (> t 0.) "a" "b")) 1)"##),
    ("array/text", "(+ t (array/count (array/text 0)))"),
    ("array/vec3", "(array/nth (array/vec3 4 [t 1 2]) 2)"),
    ("array/count", "(array/count (array/float (+ 3 (int t))))"),
    ("array/nth", "(array/nth (array/range 8) (+ 2 (int t)))"),
    ("array/slice", "(array/count (array/slice (array/range 10) (int t) 4))"),
    ("array/concat", "(array/count (array/concat (array/range (+ 2 (int t))) (array/float 2)))"),
    ("array/sum", "(array/sum (array/float 5 t))"),
    ("array/sum", "(array/sum (array/vec3 3 [t 1 2]))"),
    ("array/sum", "(+ t (array/sum (array/float 0)))"),
    ("int/mul", "(int/mul (int t) 3)"),
    ("int/div", "(int/div (int t) 2)"),
    ("int/mod", "(int/mod (int (* t 7)) 3)"),
    ("int/and", "(int/and (int t) 6)"),
    ("int/xor", "(int/xor (int t) 5)"),
    ("int/add", "(int/add (frame/index) 9007199254740993)"),
    ("int/sub", "(int/sub (frame/index) 9007199254740993)"),
    ("int/or", "(int/or (frame/index) 9007199254740993)"),
    ("int/shl", "(int/shl (frame/index) 31)"),
    ("int/shr", "(int/shr (int/sub (frame/index) 9007199254740993) 31)"),
    ("int/lt", "(int/lt (frame/index) 9007199254740993)"),
    ("int/le", "(int/le (frame/index) 9007199254740993)"),
    ("int/gt", "(int/gt (frame/index) 9007199254740993)"),
    ("int/ge", "(int/ge (frame/index) 9007199254740993)"),
    ("int/eq", "(int/eq (frame/index) 9007199254740993)"),
    ("length", "(length [t 1 2])"),
    ("equal?", "(equal? t 1.)"),
    ("exp", "(exp t)"),
    ("exact", "(exact (+ t 1))"),
    ("+", "(+ t 1)"),
    ("-", "(- t 1)"),
    ("*", "(* t 3)"),
    ("/", "(/ t 3)"),
    ("mod", "(mod t 0.7)"),
    ("pow", "(pow t 2)"),
    ("min", "(min t 1)"),
    ("max", "(max t 1)"),
    ("sin", "(sin t)"),
    ("cos", "(cos t)"),
    ("sqrt", "(sqrt (+ t 1))"),
    ("floor", "(floor t)"),
    ("ceil", "(ceil t)"),
    ("round", "(round t)"),
    ("int", "(int (* t 10))"),
    ("float", "(float (int t))"),
    ("abs", "(abs t)"),
    ("<", "(if (< t 1) 1. 0.)"),
    (">", "(if (> t 1) 1. 0.)"),
    ("<=", "(if (<= t 1) 1. 0.)"),
    (">=", "(if (>= t 1) 1. 0.)"),
    ("=", "(if (= t 1) 1. 0.)"),
    ("and", "(if (and (> t 0) (< t 2)) 1. 0.)"),
    ("or", "(if (or (> t 2) (< t 0)) 1. 0.)"),
    ("not", "(if (not (> t 1)) 1. 0.)"),
    ("value/rand", "(value/rand t 1 2)"),
    ("rand/at", "(rand/at (frame/index) (int t))"),
    ("noise3", "(noise3 [t 0.3 -0.2])"),
    ("color/hsl", "(color/hsl (* t 60.) 0.75 0.5)"),
    ("color/gradient", r##"(color/gradient (list "#101426" "#f26444") t)"##),
    ("text/trim", r##"(text/trim (str " \t éÉ日本 " t " \n"))"##),
    ("text/lower", r##"(text/lower (str "ABZéÉΣİ日本😀" t))"##),
    ("error", r##"(if (> t 0.) (error (str "E_EXACT_" (int t)) (str "é日本😀 " t)) 7.)"##),
    ("value/hsv", "(value/hsv (* t 0.37) 0.5 0.9)"),
    ("value/hsv", "(value/hsv (- 0 t) 1. 0.25)"),
    ("value/hsv", "(value/hsv (* t 0.166) 0.7 0.8)"),
    ("value/hsv", "(value/hsv (+ t 5.) 0.3 1.)"),
    ("value/lerp", "(value/lerp t 1 0.25)"),
    ("value/lerp", "(value/lerp (* t 1.3) 0.7 0.3)"),
    ("value/lerp", "(value/lerp [t 1 2] [3 (* t 1.1) 5] (/ t 7))"),
    ("value/polar", "(value/polar t 2)"),
    ("range", "(range (int (+ t 3)))"),
    ("linspace", "(linspace 0 t 5)"),
    ("linspace", "(linspace 0 (+ 1 t) 1)"),
    ("linspace", "(linspace 1 t 0)"),
    ("linspace", "(linspace 0 t (int (* t 3)))"),
    ("count", "(+ t (count (list 1 2 3)))"),
    ("first", "(first (list t 1 2))"),
    ("first", "(first (take (int t) (list t 1)))"),
    ("last", "(+ t (last (list 1 2 3)))"),
    ("last", "(last (take (int t) (list t 1)))"),
    ("rest", "(sum [x (rest (list t 1 2))] (* x t))"),
    ("nth", "(nth (list t 1 2) (int t))"),
    ("reverse", "(sum [x (reverse (list t 1 2))] (* x t))"),
    ("take", "(sum [x (take 2 (list t 1 2))] (* x t))"),
    ("drop", "(sum [x (drop 1 (list t 1 2))] (* x t))"),
    ("live?", "(+ t (if (live? 2.0) 1.0 0.0))"),
];

const TIMES: [f64; 8] = [-3., -0., 0., 0.37, 1., 1.9, 3., 7.5];

fn lives() -> Vec<Frame> {
    let mut out = Vec::new();
    for time in TIMES {
        let base = input::at_time(time);
        out.push(base.clone());
        out.push(Frame {
            buttons: vec![MouseButton::LeftButton, MouseButton::MouseX1],
            keys: vec![Key::Space, Key::KeyChar('a')],
            pointer: (12., 37.),
            size: (640, 480),
            frame: 3,
            dt: 0.125,
            ..base
        });
    }
    out
}

const NOT_COVERED: &[&str] = &[
    "color/hsla", "text/float", "scene/trace", "gpu/fwidth", "gpu/float-bits", "gpu/sample", "gpu/sample-level", "gpu/read", "gpu/texture-size",
    "gpu/instance-id", "gpu/front-facing", "gpu/depth", "gpu/fragment-x", "gpu/fragment-y", "gpu/trace-triangles", "gpu/trace-any", "gpu/trace-curves",
    "gpu/trace-curves-any", "gpu/scene-vertex", "gpu/scene-vertex-end", "gpu/scene-instance", "gpu/scene-sphere", "gpu/scene-strand", "gpu/scene-spheres",
    "gpu/scene-strands", "gpu/group-scan", "gpu/store", "gpu/store-int",
];

#[test]
fn every_scalar_value_operator_has_a_case() {
    for o in op::of_context(&[], &context::value()) {
        if o.ctx == context::value() && o.shape == Shape::Scalar && !NOT_COVERED.contains(&o.name.as_str()) && !CASES.iter().any(|(n, _)| *n == o.name) {
            panic!("No core operator case for {}", o.name);
        }
    }
}

fn operator_cases(cases: &[(&str, &str)], failures: &mut Vec<String>) -> usize {
    let lives = lives();
    let mut n = 0;
    for (name, body) in cases {
        let source = format!("(workspace w (graph g :context value (let* [tested {body}] 0.)))");
        let ws = ts::check_workspace(&source);
        let evaluation = ok(E::r#static(&ws, true, None));
        let path = vec!["g".to_owned(), "tested".to_owned()];
        let value = evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1.clone();
        let Value::Residual(r) = &value else { panic!("Not live: {body}") };
        let core = core_lower::lower(None, None, false, None, None, *r).unwrap_or_else(|n| panic!("{name} {body}: refused {n}"));
        for live in &lives {
            n += 1;
            match (ts::run_simple(&core, live), E::Host::force_reference(&value, live, None, None, None)) {
                (Ok(a), Ok(b)) => {
                    if !same(&a, &b) {
                        failures.push(format!("{body} at {}: {a:?} <> {b:?}", live.t));
                    }
                }
                (Err(a), Err(b)) => {
                    if a.code != b.code { failures.push(format!("{body} at {}: {} <> {}", live.t, a.code, b.code)); }
                }
                (Ok(a), Err(b)) => failures.push(format!("{body} at {}: {a:?} but reference {}", live.t, b.code)),
                (Err(a), Ok(b)) => failures.push(format!("{body} at {}: {} but reference {b:?}", live.t, a.code)),
            }
        }
    }
    n
}

#[test]
fn scalar_value_operators_lower_and_match_the_walker_bit_for_bit_part_1() {
    let mut failures = Vec::new();
    let n = operator_cases(&CASES[..60], &mut failures);
    assert!(failures.is_empty(), "{} mismatches of {n}:\n{}", failures.len(), failures.join("\n"));
}

#[test]
fn scalar_value_operators_lower_and_match_the_walker_bit_for_bit_part_2() {
    let mut failures = Vec::new();
    let n = operator_cases(&CASES[60..], &mut failures);
    assert!(failures.is_empty(), "{} mismatches of {n}:\n{}", failures.len(), failures.join("\n"));
}

fn live_value(body: &str) -> Value {
    let source = format!("(workspace text_arrays (graph g :context host (if (> t -1000.) {body} {body})))");
    let ws = ts::check_workspace(&source);
    ok(E::static_eval(&ws)).results.iter().find(|(n, _)| n == "g").expect("g").1.clone()
}

fn residual_of(value: &Value) -> flow_syntax::value::ResidualId {
    match value {
        Value::Residual(r) => *r,
        _ => panic!("live text result expected"),
    }
}

fn force_strict(program: &Executor::Program, live: &Frame) -> Result<Value, Diagnostic> {
    Executor::force(program, &Executor::ForceOptions { strict: true, ..Default::default() }, live)
}

const TEXT_BODIES: [&str; 13] = [
        r##"(map (fn [x] (if (> t 0.) x "😀")) (array/text 5001 "猫"))"##,
        r##"(map (fn [x] (text/trim (if (> t 0.) x "　A "))) (array/text 5001 " \tABéÉ日本😀\r\n"))"##,
        r##"(map (fn [x] (text/lower (if (> t 0.) x "ÉΣİZ"))) (array/text 5001 "ABéÉ日本😀"))"##,
        r##"(map (fn [x] (text/repr (if (> t 0.) x "ÉΣİZ"))) (array/text 5001 "ABéÉ日本😀"))"##,
        r##"(for [x (array/text 5001 "猫")] (if (> t 0.) x "😀"))"##,
        r##"(scan [a ""] [x (array/text 5001 "猫")] (if (> t 0.) x "😀"))"##,
        r##"(for [x (array/text 2 "猫") y (array/text 2501 "😀")] :skip [5001] (if (> t 0.) x y))"##,
        r##"(scan [a "seed"] [x (array/text 2 "猫") y (array/text 2501 "😀")] (if (> t 0.) x y))"##,
        r##"(for [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))"##,
        r##"(for [x (array/text 2) y (array/text 0)] (if (> t 0.) x y))"##,
        r##"(scan [a "seed"] [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))"##,
        "(fold [a 0] [x (array/text 5001)] (int/add a (if (> t 0.) 1 2)))",
        r##"(map (fn [x] (if (> t 0.) x "😀")) (array/text 0))"##,
];

fn text_array_bodies(bodies: &[&str]) {
    for body in bodies {
        let value = live_value(body);
        let core = ts::lower_strict(residual_of(&value)).unwrap_or_else(|form| panic!("{body}: strict Core refused {form}"));
        // Text leaves may collect in a region; Text transformations and scans
        // retain the structured Core path. Both execute without a walker retry.
        let program = ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
        for time in [-1., 0., 1.] {
            let live = input::at_time(time);
            let expected = ok(E::Host::force_reference(&value, &live, None, None, None));
            assert!(ok(ts::run_simple(&core, &live)) == expected, "{body} at {time}");
            for domains in [1, 8] {
                ts::with_domains(domains, &|| assert!(ok(force_strict(&program, &live)) == expected, "{body} at {time}, {domains} domains"));
            }
        }
    }
}

#[test]
fn text_arrays_strict_core_and_executor_match_reference_beyond_4096_part_1() { text_array_bodies(&TEXT_BODIES[..3]) }
#[test]
fn text_arrays_strict_core_and_executor_match_reference_beyond_4096_part_2() { text_array_bodies(&TEXT_BODIES[3..6]) }
#[test]
fn text_arrays_strict_core_and_executor_match_reference_beyond_4096_part_3() { text_array_bodies(&TEXT_BODIES[6..9]) }
#[test]
fn text_arrays_strict_core_and_executor_match_reference_beyond_4096_part_4() { text_array_bodies(&TEXT_BODIES[9..]) }

#[test]
fn text_arrays_dependent_clauses_and_nested_numeric_bodies_refuse_honestly() {
    let dependent = live_value(r##"(for [x (array/text 2 "猫") y (array/text (if (equal? x "猫") 2501 0) "😀")] (if (> t 0.) x y))"##);
    assert_eq!(ts::lower_strict(residual_of(&dependent)).err().as_deref(), Some("text_collect_dependent_clause"));
    assert!(Executor::compile(&dependent, &Executor::CompileOptions { strict: true, ..Default::default() }).is_err());
    assert!(ok(E::Host::force_reference(&dependent, &input::at_time(1.), None, None, None)) == Value::Text_array(Arc::new(vec!["猫".as_bytes().to_vec(); 5002].into())));
    let nested_numeric = live_value(
        "(map (fn [x] (text/trim (text/lower (str (array/sum (map (fn [y] (value/rand y t)) (array/float 2))))))) (array/text 2))",
    );
    match ts::lower_strict(residual_of(&nested_numeric)) {
        Ok(program) => assert!(!program.regions.is_empty()),
        Err(form) => panic!("Nested numeric Text body refused as {form}"),
    }
    match Executor::compile(&nested_numeric, &Executor::CompileOptions { strict: true, ..Default::default() }) {
        Err(d) => assert!(d.code == "E_KERNEL_FORM"),
        Ok(_) => panic!("Unsupported numeric producer inside Text map qualified"),
    }
    assert!(matches!(ok(E::Host::force_reference(&nested_numeric, &input::at_time(1.), None, None, None)), Value::Text_array(values) if values.len() == 2));
}
