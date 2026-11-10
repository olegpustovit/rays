//! Port of lib/flow_web/test_js.ml: goldens for `js::region` (read in place from lib/flow_web/goldens),
//! and a run of every emitted region function under node against native `packed::force_all`.
//! The node comparisons are split over several tests so each stays under the 5 s budget; they skip
//! (loudly) when node is absent.
use crate::js;
use crate::node_harness::{node_available, run_ok, runtime_url};
use crate::test_program::{die, carried_fixtures, compile, fixtures, two_outputs};
use flow_eval as E;
use flow_ir::core_ir::region::{self, Instruction, Iteration, RegionResult};
use flow_ir::packed::{self, host, ForceOptions};
use flow_syntax::value::{self, Value};

fn numbers(values: &[f64]) -> String { values.iter().map(|v| param::float_text(*v)).collect::<Vec<_>>().join(",") }

fn flatten(v: &Value) -> Vec<f64> {
    match v {
        Value::Floats(1..=4, a) => (**a).clone(),
        Value::Float(x) => vec![*x],
        Value::Vec2(x, y) => vec![*x, *y],
        Value::Vec3(x, y, z) => vec![*x, *y, *z],
        Value::Vec4(x, y, z, w) => vec![*x, *y, *z, *w],
        _ => panic!("unexpected value"),
    }
}

fn close(a: f64, b: f64) -> bool { a == b || (a - b).abs() <= 1e-12 * a.abs().max(b.abs()) }

fn live() -> input::Frame {
    let mut live = input::at_time(1.0);
    live.frame = 3;
    live
}

fn unguarded_int_uniform(i: &Instruction, index: i64) -> bool {
    match i {
        Instruction::Int_uniform(n) => *n == index,
        Instruction::Guarded(_, body) => unguarded_int_uniform(body, index),
        _ => false,
    }
}

/// Runs the region in two ranges (to exercise the carried state) and returns the status and one
/// array per output line.
pub(crate) fn run_node_raw(fast: bool, program: &packed::T) -> (i64, Vec<Vec<String>>) {
    let live = live();
    let view = host::view(program);
    let inputs = ((host::prepare_values(program, &host::PrepareOptions::default(), &live))).unwrap_or_else(|d| die(d));
    let count: i64 = if program.iteration == Iteration::Product {
        inputs.arrays.iter().map(|a| ((value::array_length(a))).unwrap_or_else(|d| die(d)) as i64).product()
    } else {
        inputs.count
    };
    let mut valid: Vec<i64> = program.skip.iter().copied().filter(|i| *i >= 0 && *i < count).collect();
    valid.sort();
    valid.dedup();
    let kept = count - valid.len() as i64;
    let collecting = matches!(program.result, RegionResult::Collect | RegionResult::Accumulate(_, true));
    let width = view.outputs[0].len() as i64;
    let mut outputs: Vec<i64> = Vec::new();
    if collecting {
        outputs.extend(view.outputs.iter().map(|o| kept * o.len() as i64));
    }
    if program.result != RegionResult::Collect {
        outputs.push(width + 1);
    }
    let mut script = String::with_capacity(65536);
    script.push_str(&format!("import {{noise3}} from \"{}\";\n", runtime_url("prims.js")));
    script.push_str(&((js::region(fast, program))).unwrap_or_else(|d| die(d)));
    let array = |values: &[f64]| format!("Float64Array.from([{}])", numbers(values));
    let scalar = |v: &Value| match v {
        Value::Int(n) => format!("{n}n"),
        Value::Float(f) => param::float_text(*f),
        _ => panic!("scalar frame fact expected"),
    };
    let packed_text = |v: &Value| match v {
        Value::Int_array(values) => format!("BigInt64Array.from([{}])", values.iter().map(|n| format!("{n}n")).collect::<Vec<_>>().join(",")),
        v => array(&flatten(v)),
    };
    let uniform = |index: usize, v: &Value| -> String {
        if view.code.iter().any(|i| unguarded_int_uniform(i, index as i64)) {
            format!("[{}]", scalar(v))
        } else {
            match v {
                Value::Int_array(_) => packed_text(v),
                Value::Int(n) => array(&[*n as f64]),
                _ => array(&flatten(v)),
            }
        }
    };
    script.push_str(&format!("const inputs = [{}];\n", inputs.arrays.iter().map(&packed_text).collect::<Vec<_>>().join(",")));
    let mut uniforms: Vec<String> = inputs.uniforms.iter().enumerate().map(|(i, v)| uniform(i, v)).collect();
    uniforms.push(format!("[{}]", inputs.frame.iter().map(&scalar).collect::<Vec<_>>().join(",")));
    script.push_str(&format!("const uniforms = [{}];\n", uniforms.join(",")));
    let declared: Vec<String> = outputs
        .iter()
        .enumerate()
        .map(|(k, length)| {
            let integer = region::integer_register(&view.code[view.outputs[k.min(view.outputs.len() - 1)][0] as usize]);
            if integer && program.result != RegionResult::Collect && k == outputs.len() - 1 {
                format!("Array({length}).fill(0n)")
            } else {
                format!("new {}({length})", if integer { "BigInt64Array" } else { "Float64Array" })
            }
        })
        .collect();
    script.push_str(&format!("const outputs = [{}];\n", declared.join(",")));
    match &program.result {
        RegionResult::Sum(false) => script.push_str(&format!("outputs[outputs.length - 1][{width}] = 0;\n")),
        RegionResult::Sum(true) => script.push_str(&format!("outputs[outputs.length - 1][{width}] = 1;\n")),
        RegionResult::Accumulate(seed, _) => {
            let seed = ((E::residual::eval_term(program.residual, seed, &live, None, None, None))).unwrap_or_else(|d| die(d));
            let values: Vec<String> = match &seed {
                Value::Int(_) => vec![scalar(&seed)],
                _ => flatten(&seed).iter().map(|v| param::float_text(*v)).collect(),
            };
            for (i, seed) in values.iter().enumerate() {
                script.push_str(&format!("outputs[outputs.length - 1][{i}] = {seed};\n"));
            }
            script.push_str(&format!("outputs[outputs.length - 1][{width}] = 1;\n"));
        }
        RegionResult::Collect => {}
    }
    script.push_str(&format!("let status = region(inputs, uniforms, outputs, 0, {});\n", count / 2));
    script.push_str(&format!("if (status === 0) status = region(inputs, uniforms, outputs, {}, {count});\n", count / 2));
    script.push_str("const str = (x) => Object.is(x, -0) ? '-0' : String(x);\nconsole.log(status);\n");
    script.push_str("for (const o of outputs) console.log(Array.from(o).map(str).join(' '));\n");
    let out = run_ok(&script);
    let mut lines = out.split('\n');
    let status: i64 = lines.next().expect("status line").trim().parse().expect("status");
    let produced: Vec<Vec<String>> =
        lines.take(outputs.len()).map(|l| l.trim_end_matches('\r').split(' ').filter(|s| !s.is_empty()).map(str::to_owned).collect()).collect();
    (status, produced)
}

fn run_node(fast: bool, program: &packed::T) -> (i64, Vec<Vec<f64>>) {
    let (status, outputs) = run_node_raw(fast, program);
    (status, outputs.into_iter().map(|o| o.iter().map(|s| s.parse::<f64>().expect("float")).collect()).collect())
}

fn compare_integer(fast: bool, source: &str) {
    let program = compile(source);
    let live = live();
    let (status, produced) = run_node_raw(fast, &program);
    assert_eq!(status, 0, "{source}");
    let expected = ((packed::force_all(&program, &live, &ForceOptions { strict: true, ..Default::default() }))).unwrap_or_else(|d| die(d));
    let output = produced.first().expect("an output");
    for v in &expected {
        let values: Vec<i64> = match v {
            Value::Int_array(values) => (**values).clone(),
            Value::Int(n) => vec![*n],
            _ => panic!("Integer result expected"),
        };
        assert!(output.len() >= values.len());
        for (i, n) in values.iter().enumerate() {
            assert!(output[i] == n.to_string(), "Integer JS mismatch at {i}: {} <> {n}", output[i]);
        }
    }
    let reference = ((host::reference(&program, &live, None, None, None))).unwrap_or_else(|d| die(d));
    assert_eq!(expected, vec![reference]);
}

fn compare_native(fast: bool, name: &str, program: &packed::T) {
    let (status, produced) = run_node(fast, program);
    assert_eq!(status, 0, "{name}");
    let live = live();
    let expected: Vec<Vec<f64>> = ((packed::force_all(program, &live, &ForceOptions::default()))).unwrap_or_else(|d| die(d)).iter().map(flatten).collect();
    let check = |actual: &[f64], wanted: &[f64]| {
        for (i, w) in wanted.iter().enumerate() {
            assert!(close(actual[i], *w), "{name}: element {i} is {:.17e}, native {:.17e}", actual[i], w);
        }
    };
    match &program.result {
        RegionResult::Collect | RegionResult::Accumulate(_, true) => {
            for (k, wanted) in expected.iter().enumerate() {
                assert!(produced[k].len() >= wanted.len(), "{name}");
                check(&produced[k], wanted);
            }
        }
        _ => check(&produced[0], &expected[0]),
    }
}

pub(crate) fn programs() -> Vec<(String, packed::T)> {
    let range = "(array/range 1024)";
    let map = |body: &str| format!("(map (fn [x] {body}) {range})");
    let mut out: Vec<(String, packed::T)> = fixtures(1024).into_iter().map(|(n, p)| (n.to_owned(), p)).collect();
    out.push(("two_outputs".to_owned(), two_outputs()));
    let mut cases: Vec<(String, String)> = Vec::new();
    for name in ["/", "mod", "pow", "min", "max"] {
        cases.push((format!("op {name}"), map(&format!("({name} x t)"))));
    }
    for name in ["sin", "cos", "sqrt", "abs"] {
        cases.push((format!("op {name}"), map(&format!("({name} (+ x t))"))));
    }
    cases.extend([
        ("logic".to_owned(), map("(if (and (> x t) (or (< x 8) (not (>= x 500)))) x (- 0 x))")),
        ("cond".to_owned(), map("(cond (< x t) (+ x 1) (> x 5) (- x 1) :else (* x t))")),
        ("vec3".to_owned(), map("[x t (/ x 3)]")),
        ("noise octaves".to_owned(), map("(noise3 [(* x 0.37) (* t 1.3) (- 0.0 4.2)] :seed 5 :octaves 4)")),
        ("noise neg".to_owned(), map("(noise3 [(* x (- 0.0 0.37)) (- t 3.0) (- 0.0 40.5)] :seed 7)")),
        ("sum".to_owned(), format!("(sum [x {range}] (sin (+ x t)))")),
        ("sum vec".to_owned(), format!("(sum [x {range}] [x t])")),
        ("fold".to_owned(), format!("(fold [a 0.0] [x {range}] (- (* a 0.99) (* (+ x t) 0.0001)))")),
        ("scan".to_owned(), format!("(scan [a 0.0] [x {range}] (- (* a 0.99) (* (+ x t) 0.0001)))")),
        ("scan vec".to_owned(), format!("(scan [a [0.0 0.0]] [x {range}] (+ (* a 0.99) [x t]))")),
        ("skip".to_owned(), format!("(for [x {range}] :skip [0 5 1023] (sin (+ x t)))")),
        ("skip two".to_owned(), format!("(for [x {range}] :skip [0 700] (* x t))")),
        ("product".to_owned(), "(for [x (array/range 32) y (array/range 32)] (+ (* x y) t))".to_owned()),
    ]);
    out.extend(cases.into_iter().map(|(name, source)| (name, compile(&source))));
    out
}

pub(crate) fn carried() -> Vec<(String, packed::T)> {
    let cases: Vec<(&str, &str)> = vec![
        ("carried dynamic", "(fold [a t] [i (range -2 (int/mod (int x) 5))] (+ a (* i 0.5)))"),
        ("carried empty", "(fold [a t] [i (range 0)] (error \"E_UNTAKEN\" \"empty loop\"))"),
        ("carried nested", "(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))"),
        (
            "carried record",
            "(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false :text \"start\"}]
    [i (range (int/mod (int x) 5))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)]
      :flag (not s.flag) :text (if s.flag \"a\" \"b\")})]
    [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])",
        ),
        ("carried guarded", "(if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)"),
        // a live octave count, an Int register
        ("noise live octaves", "(noise3 [(* x 0.37) t 0.5] :seed 5 :octaves (int/add 3 (frame/index)))"),
        ("carried scope", "(let* [a (fold [s t] [i (range 2)] (+ s (+ x 1.)))] (+ a (+ x 1.)))"),
        (
            "iterate-empty-error",
            "(iterate -1 t (fn [a] (error \"E_EMPTY\" \"untaken body\"))
    :until (fn [prev next] (error \"E_EMPTY\" \"untaken predicate\")))",
        ),
    ];
    let mut out: Vec<(String, packed::T)> =
        cases.into_iter().map(|(name, body)| (name.to_owned(), compile(&format!("(map (fn [x] {body}) (array/range 65))")))).collect();
    out.extend(carried_fixtures(65).into_iter().filter(|(name, _)| name.starts_with("iterate-")));
    out
}

fn golden(name: &str) -> &'static str {
    match name {
        "arithmetic" => include_str!("../../../crates/flow_web/goldens/arithmetic.js"),
        "noise" => include_str!("../../../crates/flow_web/goldens/noise.js"),
        "select" => include_str!("../../../crates/flow_web/goldens/select.js"),
        "two_outputs" => include_str!("../../../crates/flow_web/goldens/two_outputs.js"),
        other => panic!("no golden {other}"),
    }
}

#[test]
fn region_goldens() {
    let programs = programs();
    for name in ["arithmetic", "noise", "select", "two_outputs"] {
        let program = &programs.iter().find(|(n, _)| n == name).expect("program").1;
        let source = ((js::region(false, program))).unwrap_or_else(|d| die(d));
        assert!(golden(name) == source, "golden {name}.js differs");
        assert_eq!(source, ((js::region(false, program))).unwrap_or_else(|d| die(d)));
    }
}

#[test]
fn explicit_error_cause_is_raised_only_when_selected() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let caused = compile("(map (fn [x] (if (> x t) (error \"E_CAUSE\" \"selected\" :cause \"reason\") x)) (array/range 2))");
    let mut source = ((js::region(false, &caused))).unwrap_or_else(|d| die(d));
    source.push_str(&format!(
        "const inputs = [Float64Array.from([0,1])], uniforms = [new Float64Array({}).fill(0.5)], outputs = [new Float64Array(2)];\n",
        host::view(&caused).code.len()
    ));
    source.push_str(
        "
if (region(inputs, uniforms, outputs, 0, 0) !== 0 || region(inputs, uniforms, outputs, 0, 1) !== 0) throw new Error('untaken cause raised');
try { region(inputs, uniforms, outputs, 1, 2); throw new Error('selected cause did not raise'); }
catch (e) { if (e.code !== 'E_CAUSE' || e.message !== 'selected' || e.cause !== 'reason') throw e; }
",
    );
    run_ok(&source);
}

fn compare_programs(from: usize, to: usize) {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let programs = programs();
    for (name, program) in programs.iter().skip(from).take(to - from) {
        compare_native(false, name, program);
    }
}

#[test]
fn node_matches_native_programs_a() { compare_programs(0, 10); }

#[test]
fn node_matches_native_programs_b() { compare_programs(10, 20); }

#[test]
fn node_matches_native_programs_c() { compare_programs(20, 40); }

fn compare_carried(from: usize, to: usize) {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let carried = carried();
    for (name, program) in carried.iter().skip(from).take(to - from) {
        compare_native(false, name, program);
        compare_native(true, &format!("{name} fast"), program);
    }
}

#[test]
fn node_matches_native_carried_a() { compare_carried(0, 5); }

#[test]
fn node_matches_native_carried_b() { compare_carried(5, 10); }

#[test]
fn node_matches_native_carried_c() { compare_carried(10, 20); }

#[test]
fn node_matches_native_uint32() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let uint32 = carried_fixtures(65).into_iter().find(|(n, _)| n == "loop-uint32").expect("loop-uint32").1;
    compare_native(false, "uint32", &uint32);
    compare_native(true, "uint32 fast", &uint32);
}

/// The three 32769-row sources are 4097 rows in the default loop (still past the 4096 chunk); RAYS_SLOW=1 runs 32769.
fn wide_integers_sources() -> Vec<String> {
    let big = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 32769 } else { 4097 };
    vec![
        format!("(map (fn [x] (int/add x (frame/index))) (array/int {big} 4611686018427387903))"),
        format!("(map (fn [x] (int/sub x (frame/index))) (array/int {big} -4611686018427387904))"),
        format!("(map (fn [x] (int/xor x (int/add 9007199254740993 (frame/index)))) (array/range {big} :type int))"),
        "(map (fn [x] (int/shr (int/add x (int/sub (frame/index) 9007199254740993)) 31)) (array/range 1025 :type int))".to_string(),
        "(map (fn [x] (floor (+ x t))) (array/range 1025))".to_string(),
        "(array/sum (map (fn [x] (int/add x (frame/index))) (array/int 1025 9007199254740993)))".to_string(),
        "(scan [a (frame/index)] [x (array/range 1025 :type int)] (int/add a x))".to_string(),
        "(fold [a (frame/index)] [x (array/range 1025 :type int)] (int/add a x))".to_string(),
    ]
}

fn wide_integers(from: usize, to: usize) {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    for source in wide_integers_sources().iter().skip(from).take(to - from) {
        compare_integer(false, source);
    }
}

#[test]
fn integer_registers_past_2_pow_53_loop() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    for fast in [false, true] {
        compare_integer(
            fast,
            "(map (fn [x] (let* [lo (int/add 9007199254740993 (frame/index))]
        (fold [a (frame/index)] [i (range lo (int/add lo 3))] (int/add a i)))) (array/range 65))",
        );
    }
}

/// Division wraps at the signed boundary; indexed randomness retains every seed and index bit.
#[test]
fn int_div_of_min_and_rand_at_agree_with_native() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let cases = [
        "(map (fn [(x : int)] (let* [m (int/shl 1 (int/add 63 (int/mod (frame/index) 1))) n (int/sub 0 (int/add 1 (int/mod (frame/index) 1)))]
        (+ (if (int/eq (int/div m n) m) 1. 0.) (if (int/eq (int/mod m n) 0) 2. 0.)))) (array/range 16 :type int))",
        "(map (fn [(x : int)] (rand/at (int/add 4611686018427387904 (frame/index)) (int/sub 0 (int/add x 4611686018427387905)))) (array/range 16 :type int))",
    ];
    for fast in [false, true] {
        for source in cases {
            compare_native(fast, source, &compile(source));
        }
    }
}

#[test]
fn integer_registers_past_2_pow_53_a() { wide_integers(0, 4); }

#[test]
fn integer_registers_past_2_pow_53_b() { wide_integers(4, 8); }

// Fast precision's 64-bit register pairs agree with native, including values past 2^53.
const FAST_INTEGERS: [&str; 10] = [
    "(map (fn [(x : int)] (int/and (int/xor (int/mul x 73856093) (int/mul (int/mul x x) 19349663)) 4611686018427387903)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/mod (int/mul (int/add x 1103515245) 2654435761) 1000003)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/div (int/mul (int/add x (frame/index)) 3037000493) 7)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/mod (int/sub 0 (int/mul x 2654435761)) 1000)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/div (int/sub 0 (int/mul x 2654435761)) 1000)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/shr (int/mul x 2654435761) 7)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (int/shl x 40)) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [(x : int)] (if (int/lt (int/mul x 2654435761) 5000000000000) (int/mul x 3) (int/sub 0 x))) (array/range (int/add 4097 (frame/index)) :type int))",
    "(map (fn [x] (int (* x 1.5))) (array/range (int/add 4097 (frame/index))))",
    "(map (fn [(x : int)] (int/add (array/nth (array/int 4 9007199254740993) (int/mod x 4)) x)) (array/range (int/add 4097 (frame/index)) :type int))",
];

fn fast_integers(from: usize, to: usize) {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    for source in FAST_INTEGERS.iter().skip(from).take(to - from) {
        compare_integer(true, source);
    }
}

#[test]
fn fast_64_bit_register_pairs_agree_with_native_a() { fast_integers(0, 4); }

#[test]
fn fast_64_bit_register_pairs_agree_with_native_b() { fast_integers(4, 7); }

#[test]
fn fast_64_bit_register_pairs_agree_with_native_c() { fast_integers(7, 10); }

#[test]
fn nonfinite_value_is_a_status_and_native_refuses_it_too() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let overflow = compile("(map (fn [x] (pow (+ x t) 1000000)) (array/range 1024))");
    assert_eq!(run_node(false, &overflow).0, 1);
    assert!(packed::force(&overflow, &input::at_time(1.0), &ForceOptions::default()).is_err());
}

/// The OCaml test's full-size hash case (70,000 elements, past one 65,536-element block); too slow
/// for the 5 s budget at -O0, so it runs only with `--ignored`.
#[test]
#[ignore]
fn fast_64_bit_hash_at_70000_elements() {
    if !node_available() {
        return;
    }
    compare_integer(
        true,
        "(map (fn [(x : int)] (int/and (int/xor (int/mul x 73856093) (int/mul (int/mul x x) 19349663)) 4611686018427387903)) (array/range (int/add 70000 (frame/index)) :type int))",
    );
}

/// Frozen independent vectors exercise the shipped runtime, not only generated region code.
#[test]
fn full_width_random_runtime_vectors() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let mut script = format!("import * as p from \"{}\";\n", runtime_url("prims.js"));
    script.push_str("import assert from 'node:assert/strict';const view=new DataView(new ArrayBuffer(8));const bits=x=>{view.setFloat64(0,x,true);return view.getBigUint64(0,true)};\n");
    script.push_str("assert.equal(bits(p.rand_at(0n,0n)),0x3fec4415072f63b9n);\n");
    script.push_str("assert.equal(bits(p.rand_at(1n,0n)),0x3fe22145bd91204bn);\n");
    script.push_str("assert.equal(bits(p.rand_at(-1n,0n)),0x3fec9b2e2ee36ca5n);\n");
    script.push_str("assert.equal(bits(p.rand_at(4611686018427387904n,0n)),0x3f654a1d51c1f500n);\n");
    script.push_str("assert.equal(bits(p.rand_at(-4611686018427387904n,0n)),0x3fa2b21e4fe2a180n);\n");
    script.push_str("assert.equal(bits(p.rand_at(9223372036854775807n,0n)),0x3fc533ebaa9701ccn);\n");
    script.push_str("assert.equal(bits(p.rand_at(-9223372036854775808n,0n)),0x3fd207b02884aa7cn);\n");
    script.push_str("assert.equal(bits(p.rand_at(0n,-1n)),0x0000000000000000n);\n");
    script.push_str("assert.equal(bits(p.rand_at(0n,9223372036854775807n)),0x3fc2e13752bce754n);\n");
    script.push_str("assert.equal(bits(p.rand_at(0n,-9223372036854775808n)),0x3fd207b02884aa7cn);\n");
    script.push_str("assert.equal(bits(p.rand_at(-1n,-1n)),0x3fe69a0abf9e5977n);\n");
    script.push_str("assert.equal(bits(p.rand_at(-9223372036854775808n,9223372036854775807n)),0x0000000000000000n);\n");
    script.push_str("assert.equal(bits(p.rand_at(9223372036854775807n,-9223372036854775808n)),0x3fec9b2e2ee36ca5n);\n");
    assert_eq!(rays_math::hash::float_sequence(&[]).to_bits(), 0x3fc6587b4fa55f50);
    script.push_str("assert.equal(bits(p.value_rand()),0x3fc6587b4fa55f50n);\n");
    assert_eq!(rays_math::hash::float_sequence(&[0.0]).to_bits(), 0x3fea0570ae1251aa);
    script.push_str("assert.equal(bits(p.value_rand(0.0)),0x3fea0570ae1251aan);\n");
    assert_eq!(rays_math::hash::float_sequence(&[-0.0]).to_bits(), 0x3fea0570ae1251aa);
    script.push_str("assert.equal(bits(p.value_rand(-0.0)),0x3fea0570ae1251aan);\n");
    assert_eq!(rays_math::hash::float_sequence(&[1.0]).to_bits(), 0x3fe5e5711b4bd1e0);
    script.push_str("assert.equal(bits(p.value_rand(1.0)),0x3fe5e5711b4bd1e0n);\n");
    assert_eq!(rays_math::hash::float_sequence(&[1.0, 2.0]).to_bits(), 0x3fdc00e7fb3c1606);
    script.push_str("assert.equal(bits(p.value_rand(1.0,2.0)),0x3fdc00e7fb3c1606n);\n");
    assert_eq!(rays_math::hash::float_sequence(&[2.0, 1.0]).to_bits(), 0x3fe39c48d5cedf8d);
    script.push_str("assert.equal(bits(p.value_rand(2.0,1.0)),0x3fe39c48d5cedf8dn);\n");
    assert_eq!(rays_math::hash::float_sequence(&[-1.25, 1e+20]).to_bits(), 0x3fc6292b1cb3f9c0);
    script.push_str("assert.equal(bits(p.value_rand(-1.25,1e+20)),0x3fc6292b1cb3f9c0n);\n");
    script.push_str("assert.equal(p.sop_context_seed(\"n\",new Map([[\"seed\",0n],[\"label\",\"\"]])),-7541087314458812168n);\n");
    script.push_str("assert.equal(p.sop_context_seed(\"\\u017e\\ud83d\\ude42\",new Map([[\"seed\",-1n],[\"label\",\"a\\u0000b\"]])),-4018171267666195374n);\n");
    script.push_str("assert.equal(p.sop_context_seed(\"a:b\",new Map([[\"seed\",-9223372036854775808n],[\"label\",\"c\"]])),-7593990123217220747n);\n");
    script.push_str("assert.equal(p.sop_context_seed(\"a\",new Map([[\"seed\",9223372036854775807n],[\"label\",\"b:c\"]])),-6270758179017106594n);\n");
    for seed in [0, -1, i64::MIN, i64::MAX] {
        let noise = rays_math::noise::create(seed);
        let expected = rays_math::noise::sample3(&noise, 0.125, -1.25, 9.5).to_bits();
        script.push_str(&format!("assert.equal(bits(p.noise3(new Float64Array([0.125,-1.25,9.5]),{seed}n,1n,2,0.5)),{expected}n);\n"));
    }
    run_ok(&script);
}
