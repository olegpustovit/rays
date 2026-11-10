//! Shared Metal operations, seeded arithmetic/noise kernels and typed refusals compare with owned goldens. Web and CPU qualification lives in their owning crates.
use flow_check::op;
use flow_ir::core_ir::region::{Instruction, Loop};
use flow_ir::kernel_layout::{self, PlanOptions, Visit};
use flow_ir::packed;
use flow_shader::emit::{self, Options};
use flow_syntax::context;
use flow_syntax::packed_ops;
use ogpu_core::shader;
use std::fs;
use flow_ir::test_support as tp;

fn fast() -> Options {
    Options { fast: true, ..Options::default() }
}

fn exact(program: &packed::T) -> emit::Kernel {
    emit::kernel(&Options::default(), program).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn fast_kernel(program: &packed::T) -> emit::Kernel {
    emit::kernel(&fast(), program).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn exact_uint32_buffers_and_outputs() {
    let integer = tp::compile("(map (fn [(i : int)] (+ (+ 16777217 i) (int t))) (array/range 16 :type int))");
    let ordinary = fast_kernel(&integer);
    let strict = emit::kernel(&Options { uint32_outputs: vec![0], ..fast() }, &integer).unwrap();
    assert!(ordinary.output_uint32 == vec![false] && strict.output_uint32 == vec![true]);
    let input = emit::kernel(&Options { uint32_inputs: vec![0], ..fast() }, &integer).unwrap();
    assert_eq!(input.input_uint32, vec![true]);
    assert!(emit::kernel(&Options { uint32_inputs: vec![-1], ..fast() }, &integer).is_err());
    assert!(emit::kernel(&Options { uint32_tables: vec![0], ..fast() }, &integer).is_err());
    assert!(ordinary.output_widths == strict.output_widths && ordinary.interface == strict.interface);
    for outputs in [vec![-1], vec![1]] {
        assert!(emit::kernel(&Options { uint32_outputs: outputs, ..fast() }, &integer).is_err());
    }
    for body in ["(map (fn [i] (+ i t)) (array/range 16))", "(map (fn [i] [i t]) (array/range 16))"] {
        let packed = tp::compile(body);
        assert!(emit::kernel(&Options { uint32_outputs: vec![0], ..fast() }, &packed).is_err());
    }
}

pub fn integer_status_bits_are_published() {
    for (source, flag) in [
        (r#"(map (fn [(x : int)] (float (int/div x (int/sub (frame/index) (frame/index))))) (array/range 8 :type int))"#, "2"),
        (r#"(map (fn [(x : int)] (float (int/shr x (int/add 64 (int/mod (frame/index) 1))))) (array/range 8 :type int))"#, "4"),
    ] {
        let msl = fast_kernel(&tp::compile(source));
        assert!(msl.source.contains(&format!("?{flag}u:0u;")));
    }
}

pub fn msl_division_guards_minus_one() {
    // MIN / -1 and MIN % -1 overflow, which C++ leaves undefined; the kernel must not divide by -1
    let msl = fast_kernel(&tp::compile("(map (fn [(x : int)] (let* [m (int/sub 0 (int/add 1 (int/mod (frame/index) 1)))] (float (int/add (int/div x m) (int/mod x m))))) (array/range 8 :type int))"));
    assert!(msl.source.contains("==-1"));
}

struct Loops(i64);
impl Visit for Loops {
    fn instruction(&mut self, _: usize, _: &Instruction) {}
    fn loop_(&mut self, _: usize, _: &Loop, _: &[i64], _: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) {
        self.0 += 1;
        body(self);
    }
}

pub fn carried_fixtures_share_the_layout() {
    for (_, program) in tp::carried_fixtures(65) {
        let layout = kernel_layout::plan(PlanOptions { fast: true, ..PlanOptions::default() }, &program);
        let metal = fast_kernel(&program);
        assert_eq!(metal.uniform_layout, layout.uniform_layout);
        let mut loops = Loops(0);
        kernel_layout::iter(&layout, &mut loops);
        assert!(loops.0 > 0);
    }
}

pub fn integer_inputs_and_frames() {
    for source in [r#"(map (fn [x] (+ (float x) t)) (array/range 16 :type "int"))"#, "(map (fn [x] (+ x (float (frame/index)))) (array/range 16))"] {
        let program = tp::compile(source);
        let layout = kernel_layout::plan(PlanOptions { fast: true, ..PlanOptions::default() }, &program);
        assert_eq!(layout.input_used, vec![true]);
        for (slot, instruction) in layout.view.code.iter().enumerate() {
            if let Instruction::IntFrame(_) = instruction {
                assert!(layout.frames[slot] > 0);
            }
        }
        let metal = fast_kernel(&program);
        assert_eq!(metal.uniform_layout, layout.uniform_layout);
    }
}

pub fn exact_gathers_stay_on_the_cpu() {
    let program = tp::compile("(let* [table (array/float 7 0.25)] (map (fn [i] (+ t (array/nth table (int/mod (int i) 7)))) (array/range 32769)))");
    let refusals = packed::gpu_refusals(false, false, &program);
    let emitted = emit::kernel(&Options::default(), &program).err().expect("a refusal");
    assert!(refusals[0].code == "E_GPU_FORM" && emitted == refusals[0]);
    assert_eq!(refusals[0].message, "Indexed array reads stay on the CPU.");
}

pub fn guarded_checks_and_refusals() {
    let program = tp::compile("(map (fn [x] (if (> x t) (+ x 1.0) (exp (* x 1000.0)))) (array/range 16))");
    let view = packed::host::view(&program);
    let msl = exact(&program);
    let lines: Vec<&str> = msl.source.split('\n').collect();
    let mut guarded = false;
    for (slot, instruction) in view.code.iter().enumerate() {
        if let Instruction::Guarded(guards, body) = instruction {
            if matches!(**body, Instruction::Unary(packed_ops::Unary::Exp, _)) {
                guarded = true;
                assert!(!guards.is_empty());
                let assignment = format!("    r{slot}=");
                let at = lines.iter().position(|l| l.starts_with(&assignment)).expect("the guarded assignment");
                assert_eq!(lines[at + 1], format!("    sb|=!isfinite(r{slot});"));
            }
        }
    }
    assert!(guarded);
    let nested = exact(&tp::compile("(map (fn [x] (if (> x t) (noise3 [(pow (+ x t) 2.0) t 0.0] :seed 31) (+ x t))) (array/range 16))"));
    assert_eq!(nested.table_seeds, vec![31]);
    assert!(nested.uniform_layout.iter().any(|(_, _, width)| *width == 1));
    assert!(shader::validate_bindings(&nested.interface).is_ok());
    for source in [
        r#"(map (fn [x] (+ (float x) t)) (array/range 16 :type "int"))"#,
        "(let* [offset (frame/index)] (map (fn [x] (+ x (float offset))) (array/range 16)))",
    ] {
        match emit::kernel(&Options::default(), &tp::compile(source)) {
            Err(d) => assert_eq!(d.code, "E_GPU_FORM"),
            Ok(_) => panic!("{source} must be refused"),
        }
    }
}

pub fn qualifying_forms_and_widths() {
    let qualifies = |source: &str| {
        let program = tp::compile(source);
        assert!(packed::gpu_refusals(false, false, &program).is_empty());
        exact(&program);
    };
    qualifies("(for [x (array/range 1024)] (+ x t))");
    for source in [
        "(map (fn [x] (+ x t)) (array/range 1024))",
        "(map (fn [x] [x t]) (array/range 1024))",
        "(map (fn [x] [x t 2 3]) (array/range 1024))",
        "(map (fn [x] (noise3 [x t 0] :octaves 32)) (array/range 1024))",
        "(map (fn [x] (let* [unused 1e39] (+ x t))) (array/range 1024))",
    ] {
        qualifies(source);
    }
    let overflowing = tp::compile("(map (fn [x] (+ x (+ t 1e39))) (array/range 1024))");
    let reasons = packed::gpu_refusals(false, false, &overflowing);
    let emitted = emit::kernel(&Options::default(), &overflowing).err().expect("a refusal");
    assert!(reasons[0].code == "E_GPU_FORM" && emitted == reasons[0]);
    for (width, body) in [
        (2, "(map (fn [x] [x t]) (array/range 1024))"),
        (4, "(map (fn [x] [x t (+ x t) 1]) (array/range 1024))"),
        (4, "(map (fn [(uv : vec2)] [uv.x uv.y t 1]) (map (fn [x] [x 1]) (array/range 1024)))"),
        (2, "(let* [offset [1 2 3 4]] (map (fn [(p : vec4)] [(+ p.x offset.w) (+ p.w t)])\n       (map (fn [x] [x 1 2 3]) (array/range 1024))))"),
    ] {
        let program = tp::compile_with(None, false, "", body);
        assert_eq!(exact(&program).output_widths, vec![width]);
    }
}

pub fn every_packed_operation_is_known() {
    for name in emit::names() {
        assert!(op::find(&[], &name, &context::value()).is_some(), "{name}");
    }
    assert_eq!(emit::names(), packed_ops::names());
}

pub fn every_packed_operation_prints_or_is_refused() {
    for name in emit::names() {
        let body = match name.as_str() {
            n if packed_ops::integer(n).is_some() => format!("(if (> t 0.0) ({name} x 1) x)"),
            n if packed_ops::comparison(n).is_some() => format!("(if ({name} x 1) (+ t 1.0) t)"),
            "noise3" => "(noise3 [x t 0])".to_owned(),
            "fma" => "(fma x t 0.5)".to_owned(),
            "rand/at" => "(rand/at (int t) x)".to_owned(),
            "length" => "(length [x t 0])".to_owned(),
            "dot" => "(dot [x t 0] [1 2 3])".to_owned(),
            "cross" => "(cross [x t 0] [1 2 3])".to_owned(),
            "normalize" => "(normalize [x t 0])".to_owned(),
            "asin" | "acos" => format!("({name} (* (+ x t) 0.0001))"),
            "log" => "(log (+ (+ x t) 1.0))".to_owned(),
            "and" | "or" => format!("(if ({name} (> x t) (< x 8)) x t)"),
            "not" => "(if (not (> x t)) x t)".to_owned(),
            "<" | "<=" | ">" | ">=" | "=" => format!("(if ({name} x t) x t)"),
            n if packed_ops::unary(n).is_some() => format!("({name} (+ x t))"),
            _ => format!("({name} x t)"),
        };
        let integer = packed_ops::integer(&name).is_some() || packed_ops::comparison(&name).is_some() || name == "rand/at";
        let array = if integer { r#"(array/range 1024 :type "int")"# } else { "(array/range 1024)" };
        let program = tp::compile(&format!("(map (fn [x] {body}) {array})"));
        let printed = emit::kernel(&Options::default(), &program);
        if integer || name == "hypot" {
            match printed {
                Err(d) => assert_eq!(d.code, "E_GPU_FORM", "{name}"),
                Ok(_) => panic!("Metal must refuse {name}"),
            }
        } else {
            printed.unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
        }
    }
}

pub fn golden_kernels_match_byte_for_byte() {
    let mut cases: Vec<(String, packed::T)> = tp::fixtures(None, 1024).into_iter().map(|(n, p)| (n, p)).collect();
    cases.push(("two_outputs".to_owned(), tp::two_outputs()));
    for (name, program) in cases {
        let msl = exact(&program);
        let expected = fs::read_to_string(format!("{}/../../crates/flow_gpu/goldens/{name}.metal", env!("CARGO_MANIFEST_DIR"))).unwrap();
        assert_eq!(msl.source, expected, "{name}");
        assert_eq!(msl.interface.len() as i64, msl.input_widths.len() as i64 + msl.output_widths.len() as i64 + 2 + if msl.table_seeds.is_empty() { 0 } else { 1 });
        assert!(shader::validate_bindings(&msl.interface).is_ok());
    }
}

pub fn ordered_reductions_are_refused_with_the_shared_reason() {
    for source in [
        "(sum [x (array/range 1024)] (+ x t))",
        "(for [x (array/range 32) y (array/range 32)] (+ (+ x y) t))",
        "(for [x (array/range 1024)] :skip [0] (+ x t))",
    ] {
        let program = tp::compile(source);
        let reasons = packed::gpu_refusals(false, false, &program);
        let emitted = emit::kernel(&Options::default(), &program).err().expect("a refusal");
        assert!(reasons[0].code == emitted.code && reasons[0].message == emitted.message, "{source}");
    }
}

pub fn fused_and_checked_arithmetic_is_emitted() {
    for (source, fragment) in [
        ("(map (fn [x] (get (float/mul_checked x t) :value)) (array/range 1024))", "isfinite(("),
        ("(map (fn [x] (if (get (float/fma_checked x t 1.0) :valid) x 0.0)) (array/range 1024))", "?1.0f:0.0f)"),
        ("(map (fn [x] (get (float/fma_checked x t 1.0) :value)) (array/range 1024))", "fma("),
    ] {
        assert!(exact(&tp::compile(source)).source.contains(fragment), "{source}");
    }
    let uniform = exact(&tp::compile("(map (fn [x] t) (array/range 1024))"));
    assert!(uniform.interface.len() == 3 && !uniform.interface.iter().any(|b| b.binding == 0));
}

pub fn only_inputs_and_outputs_are_finite_checked() {
    let program = tp::compile("(map (fn [x] (if (> x t) 0.0 (min (* x x) 1.0))) (array/range 1024))");
    let hidden = exact(&program);
    let checks = packed::host::finite_checks(&program);
    let view = packed::host::view(&program);
    let lines: Vec<&str> = hidden.source.split('\n').collect();
    for (index, line) in lines.iter().enumerate() {
        if let Some(rest) = line.strip_prefix("  float r") {
            let slot: usize = rest.split('=').next().unwrap().parse().unwrap();
            let guard = format!("  sb|=!isfinite(r{slot});");
            if !matches!(view.code[slot], Instruction::Guarded(..)) {
                assert_eq!(lines[index + 1] == guard, checks[slot], "r{slot}");
            }
        }
    }
}

pub fn kernel_layout_is_shared_with_the_printer() {
    let program = tp::compile("(let* [table (map (fn [i] [i (* i 2.) (- 0. i)]) (array/range 64))] (map (fn [x] (let* [p (array/nth table (int/mod (int x) 64))] (+ (+ p.x (* p.y 0.5)) p.z))) (array/range (int/add 4097 (frame/index)))))");
    let layout = kernel_layout::plan(PlanOptions { fast: true, ..PlanOptions::default() }, &program);
    let msl = fast_kernel(&program);
    assert_eq!(msl.uniform_layout, layout.uniform_layout);
    assert_eq!(msl.table_seeds, layout.table_seeds);
    assert_eq!(msl.gathers, layout.gathers.iter().map(|(_, g)| *g).collect::<Vec<_>>());
    assert_eq!(msl.counts, layout.counts);
    assert_eq!(msl.uniform_bytes, layout.uniform_bytes);
    let program = tp::compile("(map (fn [x] (+ x t)) (array/range 16))");
    let layout = kernel_layout::plan(PlanOptions::default(), &program);
    let msl = exact(&program);
    assert_eq!(msl.uniform_layout, layout.uniform_layout);
    assert_eq!(msl.table_seeds, layout.table_seeds);
    assert_eq!(msl.uniform_bytes, layout.uniform_bytes);
}

pub const CASES: &[(&str, fn())] = &[
    ("exact_uint32_buffers_and_outputs", exact_uint32_buffers_and_outputs),
    ("integer_status_bits_are_published", integer_status_bits_are_published),
    ("msl_division_guards_minus_one", msl_division_guards_minus_one),
    ("carried_fixtures_share_the_layout", carried_fixtures_share_the_layout),
    ("integer_inputs_and_frames", integer_inputs_and_frames),
    ("exact_gathers_stay_on_the_cpu", exact_gathers_stay_on_the_cpu),
    ("guarded_checks_and_refusals", guarded_checks_and_refusals),
    ("qualifying_forms_and_widths", qualifying_forms_and_widths),
    ("every_packed_operation_is_known", every_packed_operation_is_known),
    ("every_packed_operation_prints_or_is_refused", every_packed_operation_prints_or_is_refused),
    ("golden_kernels_match_byte_for_byte", golden_kernels_match_byte_for_byte),
    ("ordered_reductions_are_refused_with_the_shared_reason", ordered_reductions_are_refused_with_the_shared_reason),
    ("fused_and_checked_arithmetic_is_emitted", fused_and_checked_arithmetic_is_emitted),
    ("only_inputs_and_outputs_are_finite_checked", only_inputs_and_outputs_are_finite_checked),
    ("kernel_layout_is_shared_with_the_printer", kernel_layout_is_shared_with_the_printer),
];
