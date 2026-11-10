//! Owned WGSL fixtures cover shared refusals, image and fast kernels; browser qualification performs real target compilation.
use crate::test_program::{die, carried_fixtures, compile, fixtures, two_outputs, vector_fixtures};
use crate::wgsl;
use flow_ir::core_ir::region::Instruction;
use flow_ir::kernel_layout::{self, PlanOptions};
use flow_ir::packed::{self, host};

fn golden(name: &str) -> &'static str {
    match name {
        "arithmetic" => include_str!("../../../crates/flow_web/goldens/arithmetic.wgsl"),
        "select" => include_str!("../../../crates/flow_web/goldens/select.wgsl"),
        "noise" => include_str!("../../../crates/flow_web/goldens/noise.wgsl"),
        "two_outputs" => include_str!("../../../crates/flow_web/goldens/two_outputs.wgsl"),
        other => panic!("no golden {other}"),
    }
}

fn contains(s: &str, sub: &str) -> bool { s.contains(sub) }

#[test]
fn guarded_exp_checks_finiteness_right_after_its_assignment() {
    let program = compile("(map (fn [x] (if (> x t) (+ x 1.0) (exp (* x 1000.0)))) (array/range 16))");
    let view = host::view(&program);
    let wgsl = ((wgsl::kernel(false, &program))).unwrap_or_else(|d| die(d));
    let lines: Vec<&str> = wgsl.source.split('\n').collect();
    let mut guarded = false;
    for (slot, instruction) in view.code.iter().enumerate() {
        if let Instruction::Guarded(guards, body) = instruction {
            if matches!(**body, Instruction::Unary(flow_syntax::packed_ops::Unary::Exp, _)) {
                guarded = true;
                assert!(!guards.is_empty());
                let assignment = format!("    r{slot} =");
                let at = lines.iter().position(|l| l.starts_with(&assignment)).expect("assignment line");
                assert_eq!(lines[at + 1], format!("    if (!wfinite(r{slot})) {{ sb |= 1u; }}"));
            }
        }
    }
    assert!(guarded);
    let nested = compile("(map (fn [x] (if (> x t) (noise3 [(pow (+ x t) 2.0) t 0.0] :seed 31) (+ x t))) (array/range 16))");
    let nested = ((wgsl::kernel(false, &nested))).unwrap_or_else(|d| die(d));
    assert_eq!(nested.table_seeds, vec![31]);
    assert!(nested.source.split('\n').any(|l| l.starts_with("fn wpow")));
    for source in [
        "(map (fn [x] (+ (float x) t)) (array/range 16 :type \"int\"))",
        "(let* [offset (frame/index)] (map (fn [x] (+ x (float offset))) (array/range 16)))",
    ] {
        let program = compile(source);
        match wgsl::kernel(false, &program) {
            Err(d) => assert_eq!(d.code, "E_GPU_FORM"),
            Ok(_) => panic!("{source} must be refused"),
        }
    }
}

#[test]
fn operators_fractal_noise_and_hypot() {
    for body in [
        "(tan (+ (* x 0.0001) t))",
        "(atan2 x t)",
        "(asin (* (+ x t) 0.0001))",
        "(acos (* (+ x t) 0.0001))",
        "(exp (* (+ x t) 0.0001))",
        "(log (+ (+ x t) 1.0))",
        "(sign (- x t))",
    ] {
        let program = compile(&format!("(map (fn [x] {body}) (array/range 1024))"));
        ((wgsl::kernel(false, &program))).unwrap_or_else(|d| die(d));
    }
    // a live lacunarity and gain: the fractal fBm over the region's registers, no refusal
    let fractal = compile("(map (fn [x] (noise3 [x t 0.0] :seed 3 :octaves 4 :lacunarity (+ 1.7 t) :gain (- 0.62 t))) (array/range 1024))");
    assert!(packed::gpu_refusals(false, false, &fractal).is_empty());
    let source = ((wgsl::kernel(false, &fractal))).unwrap_or_else(|d| die(d)).source;
    assert!(contains(&source, "fn wfbm_fractal") && contains(&source, "wfbm_fractal(vec3<f32>("));
    // a live octave count: the fast kernel's wfbm_live over an Int uniform, no refusal
    let live = compile("(map (fn [x] (noise3 [x t 0.0] :seed 3 :octaves (int (+ 2.0 t)))) (array/range 1024))");
    assert!(packed::gpu_refusals(true, true, &live).is_empty());
    let source = ((wgsl::kernel_fast(&live))).unwrap_or_else(|d| die(d)).source;
    assert!(contains(&source, "fn wfbm_live") && contains(&source, "wfbm_live(vec3<f32>("));
    let hypot = compile("(map (fn [x] (hypot x t)) (array/range 1024))");
    match wgsl::kernel(false, &hypot) {
        Err(d) => assert_eq!(d.code, "E_GPU_FORM"),
        Ok(_) => panic!("WGSL must refuse unsupported native-safe hypot"),
    }
}

#[test]
fn fast_kernels_print_for_every_fast_case() {
    let fast_cases = crate::oracle_cases::fast_cases();
    for (name, source) in fast_cases {
        let program = compile(source);
        let w = wgsl::kernel_fast(&program).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
        assert!(w.entry.starts_with("kernel_") && w.counts.is_some(), "{name}");
    }
    for (name, program) in carried_fixtures(4096) {
        let w = wgsl::kernel_fast(&program).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
        assert!(w.source.contains("@compute @workgroup_size(64)"), "{name}");
    }
    for (name, source) in crate::oracle_cases::error_cases() {
        let program = compile(source);
        wgsl::kernel_fast(&program).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
    }
}

#[test]
fn goldens_and_binding_numbering() {
    let mut programs = fixtures(1024);
    programs.push(("two_outputs", two_outputs()));
    for (name, program) in &programs {
        let w = ((wgsl::kernel(false, program))).unwrap_or_else(|d| die(d));
        assert!(golden(name) == w.source, "golden {name}.wgsl differs");
        // The numbering the matching Metal goldens use.
        let bindings: Vec<i64> = w.interface.iter().map(|b| b.binding).collect();
        let expected: Vec<i64> = match *name {
            "noise" | "two_outputs" => vec![0, 1, 2, 3, 4],
            _ => vec![0, 1, 2, 3],
        };
        assert_eq!(bindings, expected, "{name}");
        assert_eq!(w.uniform_bytes % 16, 0);
        assert_eq!(((wgsl::kernel(false, program))).unwrap_or_else(|d| die(d)).source, w.source);
    }
}

#[test]
fn shared_refusals_pass_through() {
    for source in [
        "(sum [x (array/range 1024)] (+ x t))",
        "(map (fn [x] (cross [x t 0] [1 2 3])) (array/range 1024))",
        "(for [x (array/range 1024)] :skip [0] (+ x t))",
    ] {
        let program = compile(source);
        let reasons = packed::gpu_refusals(false, false, &program);
        match (reasons.first(), wgsl::kernel(false, &program)) {
            (Some(reason), Err(emitted)) => assert_eq!(emitted, *reason, "{source}"),
            _ => panic!("{source}"),
        }
    }
}

#[test]
fn image_kernels() {
    let programs = fixtures(1024);
    let pixels = vector_fixtures(1).into_iter().find(|(n, _)| *n == "vec4").unwrap().1;
    let image = ((wgsl::kernel(true, &pixels))).unwrap_or_else(|d| die(d));
    let image_layout = kernel_layout::plan(PlanOptions { image: true, ..Default::default() }, &pixels);
    assert_eq!(image.uniform_layout, image_layout.uniform_layout);
    assert_eq!(image.uniform_bytes, kernel_layout::uniform_words(&image_layout) * 16);
    assert!(image.input_widths.is_empty() && image.output_widths == vec![4]);
    assert_eq!(image.interface.iter().map(|b| b.binding).collect::<Vec<_>>(), vec![0, 1, 2]);
    assert!(image.uniform_layout.iter().any(|(name, _, _)| name == "image:width"));
    assert!(image.uniform_layout.iter().any(|(name, _, _)| name == "image:height"));
    assert!(wgsl::kernel(true, &programs[0].1).is_err());
}

#[test]
fn finite_checks_follow_the_plan() {
    let hidden = compile("(map (fn [x] (if (> x t) 0.0 (min (* x x) 1.0))) (array/range 1024))");
    let source = ((wgsl::kernel(false, &hidden))).unwrap_or_else(|d| die(d)).source;
    let lines: Vec<&str> = source.split('\n').collect();
    let checks = packed::finite_checks(&hidden);
    for (index, line) in lines.iter().enumerate() {
        if let Some(rest) = line.strip_prefix("  let r") {
            let slot: usize = rest.split(':').next().unwrap().parse().unwrap();
            let guard = format!("  if (!wfinite(r{slot})) {{ sb |= 1u; }}");
            assert_eq!(lines[index + 1] == guard, checks[slot], "slot {slot}");
        }
    }
}
