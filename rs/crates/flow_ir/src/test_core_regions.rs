// Port of lib/flow_ir/test_core_regions.ml.
use crate::core_fuse;
use crate::core_ir::Region as R;
use crate::core_lower;
use crate::flow_ir::Executor;
use crate::packed::{self, host};
use crate::test_support::{self as ts, ok};
use flow_check::op::{self, Op};
use flow_eval as E;
use flow_syntax::packed_ops::Unary;
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use flow_syntax::{context, syntax};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

type RegionT = R::T;

const OCAML_MAX_INT: i64 = 4611686018427387903;
const OCAML_MIN_INT: i64 = -4611686018427387904;

fn counting_measure(kernels: &Arc<AtomicI64>) -> packed::Measure {
    let kernels = kernels.clone();
    packed::Measure {
        clock: Arc::new(|| 0.),
        report: Arc::new(move |_, _, reference| {
            if !reference { kernels.fetch_add(1, Ordering::SeqCst); }
        }),
    }
}

#[test]
fn index_only_preparation_counts_without_building_the_range() {
    for (count_expression, base) in [
        ("(array/count (map (fn [(i : int)] (int/add i 1)) (array/range (int t) :type int)))", 0),
        ("(int/add (array/count (map (fn [(i : int)] (int/add i 1)) (array/range 5001 :type int))) (int t))", 5001),
    ] {
        let program = ts::compile(&format!("(map (fn [(i : int)] i) (array/range {count_expression} :type int))"));
        for count in [0, 1, 5001] {
            let kernels = Arc::new(AtomicI64::new(0));
            let options = host::PrepareOptions { measure: Some(counting_measure(&kernels)), index: true, ..Default::default() };
            let inputs = ok(host::prepare_values(&program, &options, &input::at_time(count as f64)));
            assert_eq!(inputs.count, base + count);
            assert!(kernels.load(Ordering::SeqCst) > 0);
            for array in &inputs.arrays {
                match array {
                    Value::Int_array(a) => assert!(a.is_empty()),
                    _ => panic!("expected an empty Int_array"),
                }
            }
        }
    }
}

#[test]
fn a_nested_count_is_a_uniform() {
    let program = ts::compile(
        "(map (fn [(i : int)] (int/add i (array/count (map (fn [(j : int)] (int/add j 1)) (array/range (int t) :type int))))) (array/range 1 :type int))",
    );
    for count in [0, 1, 5001] {
        let kernels = Arc::new(AtomicI64::new(0));
        let options = host::PrepareOptions { measure: Some(counting_measure(&kernels)), index: true, ..Default::default() };
        let inputs = ok(host::prepare_values(&program, &options, &input::at_time(count as f64)));
        assert!(inputs.count == 1 && kernels.load(Ordering::SeqCst) > 0);
        assert!(inputs.uniforms.iter().any(|u| *u == Value::Int(count)));
    }
}

fn source_count(declarations: &str, body: &str) -> (RegionT, flow_syntax::value::ResidualId) {
    let p = ts::compile_with(None, true, declarations, &format!("(map (fn [(i : int)] (int/add i 7)) (array/range {body} :type int))"));
    let (residual, term) = p.sources[0].clone();
    let view = E::Residual::residual_view(residual).unwrap();
    let input = ok(E::Residual::with_bindings(residual, &term, &view.bindings));
    let rebuilt = R::rebuild(&p, R::Rebuild { sources: Some(vec![(input, term)]), count: Some(None), origin: Some(None), ..Default::default() }).unwrap_or_else(|ds| panic!("{}", ts::show_ds(&ds)));
    (rebuilt, input)
}

fn same_result(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(a), Ok(b)) => same(a, b),
        (Err(a), Err(b)) => a.code == b.code,
        _ => false,
    }
}

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

fn strict() -> packed::ForceOptions { packed::ForceOptions { strict: true, ..Default::default() } }

fn reference(program: &RegionT, live: &Frame) -> Result<Value, Diagnostic> { host::reference(program, live, None, None, None) }

#[test]
fn merging_sibling_regions_requires_the_same_immutable_sources() {
    let (first, input) = source_count("", "(int t)");
    let cloned = match E::Host::rewrite(&Value::Residual(input), &|_| None) {
        Value::Residual(r) => r,
        _ => panic!("expected a residual"),
    };
    assert!(cloned != input);
    let cloned_term = E::Residual::residual_view(cloned).unwrap().term;
    let second = R::rebuild(&first, R::Rebuild { sources: Some(vec![(cloned, Arc::new(cloned_term))]), ..Default::default() }).unwrap_or_else(|ds| panic!("{}", ts::show_ds(&ds)));
    let merged = core_fuse::compact_sources(core_fuse::merge(&[first.clone(), second]).expect("merge"));
    assert_eq!(merged.sources.len(), 1);
    let (different, _) = source_count("", "(int/add (int t) 1)");
    assert!(core_fuse::merge(&[first.clone(), different]).is_none());
    let declared = |delta: i64| source_count(&format!("(defn bound :context value [(n : int)] (int/add n {delta}))"), "(bound (int t))").0;
    assert!(core_fuse::merge(&[declared(1), declared(2)]).is_none());
    for time in [0., 1., 1025.] {
        let live = input::at_time(time);
        let expected = ok(reference(&first, &live));
        for domains in [1, 4] {
            ts::with_domains(domains, &|| {
                let actual = ok(packed::force_all(&merged, &live, &strict()));
                assert_eq!(actual.len(), 2);
                for value in &actual { assert!(*value == expected); }
            });
        }
    }
}

#[test]
fn index_only_range_preparation_keeps_its_reference_error() {
    let program = ts::compile("(map (fn [(i : int)] (int/add i (int t))) (array/range (int t) :type int))");
    for t in [-1., -17.] {
        let live = input::at_time(t);
        let options = host::PrepareOptions { index: true, ..Default::default() };
        match (host::prepare_values(&program, &options, &live), reference(&program, &live)) {
            (Err(actual), Err(expected)) => assert!(actual == expected),
            _ => panic!("index-only range preparation lost its reference error"),
        }
    }
}

// Dense parameter sweeps: every colour sector and stop boundary, many roundings.
fn sweeps() -> Vec<(&'static str, &'static str, Vec<f64>)> {
    vec![
        ("hsl-sectors", "(color/hsl (* t 60.) 0.75 0.5)", (0..161).map(|i| (i - 40) as f64 * 0.25).collect()),
        ("hsl", "(color/hsl (* t 37.31) (abs (sin (* t 1.7))) (abs (cos (* t 0.9))))", (0..40000).map(|i| (i as f64 * 0.0137) - 100.).collect()),
        ("gradient", r##"(color/gradient (list "#101426" "#244a72" "#3ca6a6" "#f2d16b" "#f26444") t)"##, (0..2501).map(|i| (i - 500) as f64 * 0.001).collect()),
        ("gradient-two", r##"(color/gradient (list "#000000" "#ffffff") t)"##, (0..2501).map(|i| i as f64 * 0.0004).collect()),
        ("gradient-one", r##"(color/gradient (list "#336699") t)"##, vec![0., 0.5]),
    ]
}

const TIMES: [f64; 7] = [-3., -0., 0., 0.37, 1., 1.9, 3.];

fn check(times: &[f64], region: bool, name: &str, body: &str) {
    let tested = if region { body.to_owned() } else { format!("(if (> t -1000.) {body} {body})") };
    let source = format!("(workspace w (graph g :context value (let* [tested {tested}] 0.)))");
    let ws = ts::check_workspace(&source);
    let evaluation = ok(E::r#static(&ws, true, None));
    let path = vec!["g".to_owned(), "tested".to_owned()];
    let value = evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1.clone();
    let Value::Residual(r) = &value else { panic!("{name}: expected residual") };
    let core = core_lower::lower(None, None, false, None, None, *r).unwrap_or_else(|n| panic!("{name}: refused {n}"));
    let text = crate::core_ir::pp(&core);
    let has_region = !core.regions.is_empty();
    if has_region != region {
        panic!("{name}: region expectation\n{text}");
    }
    let program = ok(Executor::compile(&value, &Default::default()));
    for time in times {
        let live = input::at_time(*time);
        let expected = E::Host::force_reference(&value, &live, None, None, None);
        // A failing region declines to the packed path, so only successes compare with the walker.
        match (ts::run_simple(&core, &live), &expected) {
            (Ok(a), Ok(b)) => assert!(same(&a, b), "{name} at {time} (direct)"),
            (Err(_), Err(_)) if !region => (),
            (Err(_), _) if region => (),
            _ => panic!("{name} at {time} (direct outcome)"),
        }
        if !region || expected.is_ok() {
            let actual = Executor::force(&program, &Default::default(), &live);
            assert!(same_result(&actual, &expected), "{name} at {time} (core)");
        }
    }
}

// A workspace operator that reuses a shipped expansion's name keeps its own body: core declines it.
#[test]
fn a_custom_operator_with_a_shipped_name_is_declined_by_core() {
    let mut custom: Op = op::find(&[], "noise3", &context::value()).expect("noise3");
    custom.name = "toy_noise3".into();
    custom.packed_extension = None;
    custom.body = Arc::new(|_, _, _| Ok(Value::Float(0.125)));
    let forms = ok(syntax::parse("(workspace w (graph g :context value (let* [tested (toy_noise3 [t 0 0])] 0.)))"));
    let ws = match flow_check::workspace::check(&[custom], false, false, None, &ts::catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    };
    let evaluation = ok(E::r#static(&ws, true, None));
    let path = vec!["g".to_owned(), "tested".to_owned()];
    match &evaluation.records.iter().find(|(p, _)| *p == path).unwrap().1[0].1 {
        Value::Residual(r) => assert_eq!(core_lower::lower(None, None, false, None, None, *r).err().as_deref(), Some("toy_noise3")),
        _ => panic!("expected residual"),
    }
}

fn primitives_slice(range: std::ops::Range<usize>) {
    let all = ts::primitives();
    for (name, body) in &all[range] {
        check(&TIMES, false, name, body);
    }
}

#[test]
fn core_regions_primitives_scalar_and_array_builders() { primitives_slice(0..8) }
#[test]
fn core_regions_primitives_packed_map_int() { primitives_slice(8..9) }
#[test]
fn core_regions_primitives_packed_map_zip() { primitives_slice(9..10) }
#[test]
fn core_regions_primitives_packed_map_float_and_empty() { primitives_slice(10..12) }
#[test]
fn core_regions_primitives_packed_filter() { primitives_slice(12..13) }
#[test]
fn core_regions_primitives_packed_filter_vec() { primitives_slice(13..14) }

fn slow() -> bool { std::env::var("RAYS_SLOW").is_ok_and(|v| v == "1") }

// The default loop samples every `stride`th parameter of a dense sweep (debug builds run a
// parameter in about 1 ms); RAYS_SLOW=1 runs every parameter, as the OCaml test does.
fn sweep(name: &str, from: usize, to: usize, stride: usize) {
    for (n, body, times) in sweeps() {
        if n == name {
            let end = to.min(times.len());
            let chosen: Vec<f64> = times[from.min(end)..end].iter().copied().step_by(stride).collect();
            check(&chosen, false, n, body);
        }
    }
}

#[test]
fn core_regions_sweep_hsl_sectors() { sweep("hsl-sectors", 0, usize::MAX, 1) }
#[test]
fn core_regions_sweep_hsl_sampled() { sweep("hsl", 0, usize::MAX, 64) }
#[test]
fn core_regions_sweep_hsl_full_part_1() { if slow() { sweep("hsl", 0, 10000, 1) } }
#[test]
fn core_regions_sweep_hsl_full_part_2() { if slow() { sweep("hsl", 10000, 20000, 1) } }
#[test]
fn core_regions_sweep_hsl_full_part_3() { if slow() { sweep("hsl", 20000, 30000, 1) } }
#[test]
fn core_regions_sweep_hsl_full_part_4() { if slow() { sweep("hsl", 30000, 40000, 1) } }
#[test]
fn core_regions_sweep_gradient_sampled() { sweep("gradient", 0, usize::MAX, 16) }
#[test]
fn core_regions_sweep_gradient_full() { if slow() { sweep("gradient", 0, usize::MAX, 1) } }
#[test]
fn core_regions_sweep_gradient_two_sampled() { sweep("gradient-two", 0, usize::MAX, 16) }
#[test]
fn core_regions_sweep_gradient_two_full() { if slow() { sweep("gradient-two", 0, usize::MAX, 1) } }
#[test]
fn core_regions_sweep_gradient_one() { sweep("gradient-one", 0, usize::MAX, 1) }

#[test]
fn core_regions_region_ref_forms_match_the_walker() {
    for (name, body) in ts::regions() {
        check(&TIMES, true, &name, &body);
    }
}

// ---- carried loops

fn bare(instruction: &R::Instruction) -> &R::Instruction {
    match instruction {
        R::Instruction::Guarded(_, body) => bare(body),
        other => other,
    }
}

fn verify_loop(name: &str, program: &RegionT) {
    assert!(program.code.iter().any(|i| matches!(bare(i), R::Instruction::Loop_begin(_))), "{name}: no loop");
    for time in [-1., 0.37, 5.] {
        let live = input::at_time(time);
        let expected = reference(program, &live);
        for domains in [1, 8] {
            ts::with_domains(domains, &|| {
                let actual = packed::force(program, &live, &strict());
                if let (Err(actual), Err(expected)) = (&actual, &expected) {
                    if expected.code == "E_ITER_BOUND" {
                        assert!(actual.span == expected.span && actual.message == expected.message);
                    }
                }
                assert!(same_result(&actual, &expected), "carried {name} at {time}, {domains} domains");
            });
        }
    }
}

fn carried_cases() -> Vec<(&'static str, &'static str)> {
    vec![
        ("bounds", "(fold [a t] [i (range -2 (int x))] (+ a (* i 0.5)))"),
        ("range-limit", "(fold [a x] [i (range (+ 4097 (abs (int t))))] (+ a 1.))"),
        (
            "record-swap",
            "(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false :text \"start\"}]\n      [i (range (int x))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)]\n       :flag (not s.flag) :text (if s.flag \"a\" \"b\")})]\n      [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])",
        ),
        ("nested", "(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))"),
        ("empty", "(fold [a t] [i (range 0)] (/ a 0.))"),
        ("empty-error", "(fold [a t] [i (range 0)] (error \"E_INNER\" \"bad\"))"),
        ("scope", "(let* [a (fold [s t] [i (range 2)] (+ s (+ x 1.)))] (+ a (+ x 1.)))"),
        ("guarded-error", "(if (> x t) (fold [a 0.] [i (range 2)] (/ (+ a x) (- x x))) t)"),
        ("error-order", "(fold [a t] [i (range (if (= x 0.) 1 5000))] (if (> t -100.) (error \"E_FIRST\" \"first row\") a))"),
        ("iterate-count", "(iterate (- (int x) 2) t (fn [a] (+ a x)))"),
        (
            "iterate-empty-error",
            "(iterate 0 t (fn [a] (error \"E_EMPTY\" \"untaken body\"))\n      :until (fn [prev next] (error \"E_EMPTY\" \"untaken predicate\")))",
        ),
        ("iterate-nested", "(iterate 3 t (fn [a] (fold [b a] [j (range 2)] (+ b (+ x j)))))"),
        (
            "iterate-lazy-error",
            "(iterate 4611686018427387903 (+ x t)\n      (fn [a] (if (> a (+ (+ x t) 2.)) (error \"E_LATE\" \"past stop\") (+ a 1.)))\n      :until (fn [prev next] (>= next (+ (+ x t) 3.))))",
        ),
        (
            "iterate-predicate-error",
            "(iterate 2 t (fn [a] (+ a x))\n      :until (fn [prev next] (error \"E_PREDICATE\" \"selected predicate\")))",
        ),
        ("iterate-int-wrap", "(iterate (+ (abs (int t)) (int x)) 4611686018427387903 (fn [a] (int/add a 1)))"),
        (
            "iterate-text",
            "(let* [s (iterate 4611686018427387903 {:flag true :text \"old\" :n t}\n      (fn [a] {:flag false :text \"next\" :n (+ a.n x)}) :until (fn [prev next] prev.flag))] s.n)",
        ),
    ]
}

#[test]
fn carried_loops_empty_dynamic_nested_record_and_error_parity_part_1() {
    for (name, body) in &carried_cases()[..8] {
        verify_loop(name, &ts::compile(&format!("(map (fn [x] {body}) (array/range 7))")));
    }
}

#[test]
fn carried_loops_empty_dynamic_nested_record_and_error_parity_part_2() {
    for (name, body) in &carried_cases()[8..] {
        verify_loop(name, &ts::compile(&format!("(map (fn [x] {body}) (array/range 7))")));
    }
}

#[test]
fn carried_loops_helpers_and_named_functions() {
    verify_loop("helper", &ts::compile("(let* [f (fn [a] (+ a t))] (map (fn [x] (fold [a x] [i (range 3)] (f a))) (array/range 7)))"));
    verify_loop("iterate-helper", &ts::compile("(let* [f (fn [a] (+ a t))] (map (fn [x] (iterate 3 x f)) (array/range 7)))"));
    verify_loop(
        "iterate-named-helper",
        &ts::compile_with(None, true, "(defn step :context value [(a : float)] (+ a t))", "(map (fn [x] (iterate 3 x step)) (array/range 7))"),
    );
    verify_loop("outer-accumulator", &ts::compile("(fold [a 1.] [x (array/range 4)] (fold [b a] [i (range 2)] (+ b (+ t x))))"));
}

#[test]
fn carried_loops_defn_pcg_vec3() {
    let pcg = ts::compile_with(
        None,
        true,
        "(defn pcg :context value [(s : int)]\n        (let* [state (int/and (int/add (int/mul s 747796405) 2891336453) 4294967295)\n               word (int/and (int/mul (int/xor (int/shr state (int/add (int/shr state 28) 4)) state) 277803737) 4294967295)]\n          {:state state :word (int/xor (int/shr word 22) word)}))\n      (defn tint :context value [(c : vec3) (k : float)] [(* c.x k) (* c.y k) (+ c.z k)])",
        "(map (fn [x] (let* [r (fold [r {:state (int/and (int x) 4294967295) :acc [t 1. 0.]}]\n        [i (range 3)] (let* [p (pcg r.state)]\n          {:state p.state :acc (tint r.acc (/ (float (int/and p.word 65535)) 65536.))}))]\n        (+ r.acc.x (+ r.acc.y r.acc.z)))) (array/range 65))",
    );
    verify_loop("defn-pcg-vec3", &pcg);
    assert!(packed::gpu_refusals(true, true, &pcg).is_empty());
}

// 32769 crosses a parallel chunk boundary but takes several seconds in a debug build:
// the default loop uses 4097 elements, RAYS_SLOW=1 the OCaml size.
fn parallel_count() -> usize { if std::env::var("RAYS_SLOW").is_ok_and(|v| v == "1") { 32769 } else { 4097 } }

#[test]
fn carried_loops_parallel() {
    let parallel_count = parallel_count();
    verify_loop("parallel", &ts::compile(&format!("(map (fn [x] (fold [a t] [i (range 3)] (+ a (* x 0.125)))) (array/range {parallel_count}))")));
}

#[test]
fn carried_loops_iterate_parallel() {
    let parallel_count = parallel_count();
    verify_loop(
        "iterate-parallel",
        &ts::compile(&format!("(map (fn [x] (iterate 5 (+ x t) (fn [a] (+ a 1.))\n      :until (fn [prev next] (>= next (+ (+ x t) 2.))))) (array/range {parallel_count}))")),
    );
}

#[test]
fn carried_loops_uint32_and_iterate_fixtures() {
    let fixtures = ts::carried_fixtures(65);
    verify_loop("uint32", &fixtures.iter().find(|(n, _)| n == "loop-uint32").unwrap().1);
    for (name, program) in ts::carried_fixtures(7) {
        if name.starts_with("iterate-") {
            verify_loop(&name, &program);
        }
    }
}

#[test]
fn malformed_loops_are_rejected_when_rebuilt() {
    let program = ts::compile("(map (fn [x] (fold [a t] [i (range 2)] (+ a x))) (array/range 1))");
    let header = program.code.iter().position(|i| matches!(bare(i), R::Instruction::Loop_begin(_))).expect("loop header");
    let R::Instruction::Loop_begin(loop_) = bare(&program.code[header]).clone() else { unreachable!("matched above") };
    let result = program.code.iter().position(|i| matches!(bare(i), R::Instruction::Loop_result(..))).expect("loop result");
    let rejected = |name: &str, code: Vec<R::Instruction>, outputs: Vec<Vec<i64>>| {
        if R::rebuild(&program, R::Rebuild { code: Some(code), outputs: Some(outputs), ..Default::default() }).is_ok() {
            panic!("accepted malformed loop: {name}");
        }
    };
    let changed = |slot: usize, instruction: R::Instruction| {
        let mut code = program.code.to_vec();
        code[slot] = instruction;
        code
    };
    let h = header as i64;
    let outputs = program.outputs.to_vec();
    rejected("end outside program", changed(header, R::Instruction::Loop_begin(R::Loop { finish: program.code.len() as i64, ..loop_.clone() })), outputs.clone());
    rejected("carry type", changed(header + 1, R::Instruction::Loop_carry(h, 0, Ty::Text)), outputs.clone());
    rejected("carry component", changed(header + 1, R::Instruction::Loop_carry(h, 1, Ty::Float)), outputs.clone());
    let finish = loop_.finish as usize;
    rejected("end owner", changed(finish, R::Instruction::Loop_end(h + 1, vec![h + 1], None)), outputs.clone());
    rejected("integer stop", changed(finish, R::Instruction::Loop_end(h, vec![h + 1], Some(h))), outputs.clone());
    rejected("future stop", changed(finish, R::Instruction::Loop_end(h, vec![h + 1], Some(result as i64))), outputs.clone());
    rejected("result component", changed(result, R::Instruction::Loop_result(h, OCAML_MAX_INT, Ty::Float)), outputs.clone());
    rejected("escaped carry", program.code.to_vec(), vec![vec![h + 1]]);
    rejected("escaped body value", changed(result, R::Instruction::Unary(Unary::Abs, loop_.finish - 1)), outputs.clone());
    assert_eq!(op::range_length(None, OCAML_MAX_INT, OCAML_MIN_INT).unwrap(), 0);
    match op::range_length(None, OCAML_MIN_INT, OCAML_MAX_INT) {
        Err(error) => assert!(error.code == "E_ITER_BOUND"),
        Ok(_) => panic!("range overflow accepted"),
    }
}

// ---- loop fusion

#[test]
fn loop_fusion_producer_consumer_scopes_guards_and_sibling_outputs_agree() {
    let fused: Vec<(String, RegionT)> = ts::carried_fixtures(65).into_iter().filter(|(n, _)| n.starts_with("iterate-fused-")).collect();
    for (name, program) in &fused {
        assert_eq!(program.stages.len(), 2, "{name}: expected two fused stages");
        for time in [-1., 0.37, 5.] {
            let live = input::at_time(time);
            let expected = reference(program, &live);
            for domains in [1, 8] {
                ts::with_domains(domains, &|| assert!(same_result(&packed::force(program, &live, &strict()), &expected), "{name}: fused loop parity"));
            }
        }
    }
    let siblings: Vec<RegionT> = fused.iter().take(2).map(|(_, p)| p.clone()).collect();
    let merged = core_fuse::merge(&siblings).expect("sibling merge");
    for time in [-1., 0.37, 5.] {
        let live = input::at_time(time);
        let expected: Vec<Value> = siblings.iter().map(|p| ok(reference(p, &live))).collect();
        for domains in [1, 8] {
            ts::with_domains(domains, &|| {
                let actual = ok(packed::force_all(&merged, &live, &strict()));
                assert!(actual.len() == expected.len() && actual.iter().zip(&expected).all(|(a, b)| same(a, b)));
            });
        }
    }
    let text = ts::compile("(map (fn [s] (if (> t 0.) s \"other\"))\n      (map (fn [x] (iterate 2 \"old\" (fn [s] (if (> x t) \"next\" s)))) (array/range 7)))");
    assert_eq!(text.stages.len(), 2);
    let independent = ts::compile("(map (fn [x] [(fold [a t] [i (range 2)] (+ a x))\n                   (fold [a t] [i (range 2)] (+ a x))]) (array/range 7))");
    assert_eq!(independent.code.iter().filter(|i| matches!(bare(i), R::Instruction::Loop_begin(_))).count(), 2);
    let reduction = ts::compile("(fold [total 1.] [x (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.))))\n      (array/range 7))] (+ (* total 0.5) x))");
    assert_eq!(reduction.stages.len(), 2);
    for program in [&text, &independent, &reduction] {
        for time in [-1., 0.37, 5.] {
            let live = input::at_time(time);
            assert!(same_result(&packed::force(program, &live, &strict()), &reference(program, &live)));
        }
    }
}

#[test]
fn loop_fusion_at_scale_and_shortened_producers() {
    // 32769 rows and 1/8 domains in RAYS_SLOW=1; the default 4097 rows crosses the 4096 chunk and runs 1/4 domains.
    let (rows, domain_counts) = if slow() { (32769, [1, 8]) } else { (4097, [1, 4]) };
    let parallel = ts::carried_fixtures(rows).into_iter().find(|(n, _)| n == "iterate-fused-both").unwrap().1;
    let live = input::at_time(0.37);
    let expected = reference(&parallel, &live);
    for domains in domain_counts {
        ts::with_domains(domains, &|| assert!(same_result(&packed::force(&parallel, &live, &strict()), &expected)));
    }
    let shortened = ts::compile(
        "(map (fn [x y] (+ x y))\n      (map (fn [x] (iterate 1 t (fn [a] (if (> x 1.)\n        (error \"E_TAIL\" \"producer tail\") a)))) (array/range 3)) (array/float 1))",
    );
    assert_eq!(shortened.stages.len(), 1);
    assert!(same_result(&packed::force(&shortened, &live, &strict()), &reference(&shortened, &live)));
}

#[test]
fn carried_int_float_widening_retains_exact_initial_bits_through_structured_core() {
    for (name, body) in [
        ("iterate-carried-widen", "(iterate 1 9007199254740993\n      (fn [a] (+ (float (int/sub a 9007199254740992)) (* t 0.))))"),
        ("fold-carried-widen", "(fold [a 9007199254740993] [i (range 1)]\n      (+ (float (int/sub a 9007199254740992)) (* t 0.)))"),
        ("record-carried-widen", "(let* [s (iterate 1 {:n 9007199254740993}\n      (fn [a] {:n (+ (float (int/sub a.n 9007199254740992)) (* t 0.))}))] s.n)"),
    ] {
        check(&[0.37], false, name, &format!("(map (fn [x] {body}) (array/range 1))"));
    }
}
