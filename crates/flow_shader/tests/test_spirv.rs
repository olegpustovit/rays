//! Port of lib/flow_gpu/test_spirv.ml (the SPIR-V half of lib/flow_shader's tests): every kernel
//! prints deterministically, shares the Metal kernel's bindings and layout, and matches the digest
//! goldens byte for byte (size and MD5 of the module): exact kernels crates/flow_gpu/goldens/spirv.sexp,
//! fast ones tests/spirv_fast.sexp.
mod support;
use flow_ir::kernel_layout;
use flow_shader::emit::{self, Curves};
use rays_math::hash::md5_hex;
use flow_shader::spirv;
use ogpu_core::shader;
use std::collections::HashMap;
use std::fs;
use support as tp;

fn cases() -> Vec<(String, flow_ir::packed::T)> {
    let mut out: Vec<(String, flow_ir::packed::T)> = [
        ("arithmetic", "(map (fn [x] [(/ x (- x 3.)) (pow (abs (- x 4.)) 0.5) (mod x 7.) (+ t (abs x))]) (array/range 16))"),
        ("guarded", "(map (fn [x] (if (> x t) (+ x 1.) (exp (* x 1000.)))) (array/range 16))"),
        ("intrinsics", "(map (fn [x] [(sin (+ x t)) (cos x) (fma x t 0.25) (log (+ (abs x) 1.))]) (array/range 16))"),
    ]
    .iter()
    .map(|(n, s)| (n.to_string(), tp::compile(s)))
    .collect();
    out.push(("two_outputs".into(), tp::two_outputs()));
    out.extend(
        [
            ("noise", "(map (fn [x] (noise3 [(- x 0.5) t 0.25] :seed 31 :octaves 3)) (array/range 16))"),
            ("noise_seeds", "(map (fn [x] (if (> x t) (noise3 [x t -1.25] :seed 73 :octaves 32) (noise3 [(- 0. x) t 0.5] :seed 31))) (array/range 16))"),
            ("checked", "(map (fn [x] [(get (float/mul_checked x t) :value) (if (get (float/fma_checked x t 1.) :valid) x 0.)]) (array/range 16))"),
            ("operators", "(map (fn [x] (+ (atan2 (sign (- x t)) (sqrt (+ x t))) (if (and (< x t) (not (= x 2.))) (+ (tan x) (asin (* x 0.001))) (+ (acos (* x 0.001)) (max (min x t) (- x t)))))) (array/range 16))"),
        ]
        .iter()
        .map(|(n, s)| (n.to_string(), tp::compile(s))),
    );
    out.extend(tp::vector_fixtures(16).into_iter().map(|(n, p)| (n.to_string(), p)));
    out
}

fn fast_sources() -> Vec<(String, String)> {
    let int63 = format!("(map (fn [x] {}) (array/range 16))", tp::INT64_CHECKS);
    let mut v: Vec<(&str, String)> = vec![
        ("fast_float", "(map (fn [x] (+ (sin x) t)) (array/range 16))".into()),
        ("fast_input", r#"(map (fn [x] (+ (float x) t)) (array/range 16 :type "int"))"#.into()),
        ("fast_int63", int63),
        ("fast_arithmetic", r#"(let* [n (frame/index)] (map (fn [x] [(float (int/div (int/sub x n) (int/sub n 3))) (float (int/mod (int/sub x n) (int/sub n 3))) (float (int/mul (int/add x 4294967297) 4294967295)) (if (int/le x n) t (float (int/xor x 63)))]) (array/range 16 :type "int")))"#.into()),
        ("fast_bits", r#"(map (fn [x] [(float (int/and (int/shr (int/shl (int/add x 1) 40) 13) 4294967295)) (float (int/or (int/xor x 255) 65536)) (float (abs (int/sub (frame/index) x))) (if (int/ge x (frame/index)) t (+ t 1.))]) (array/range 16 :type "int"))"#.into()),
        ("fast_convert", r#"(map (fn [x] [(float (floor (- (+ x t) 0.5))) (float (ceil (- t x))) (float (round (- x t))) (float (int (+ x t)))]) (array/range 16))"#.into()),
        ("fast_trace", r#"(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t (float h.prim) (+ h.u h.normal.z) (if h.hit 1. 0.)])) (array/range 16 :type "int"))"#.into()),
        ("fast_trace_instanced", r#"(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t (float h.instance) (+ h.u h.normal.x) (if h.hit 1. 0.)])) (array/range 16 :type "int"))"#.into()),
        ("fast_trace_curves", r#"(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t (float h.prim) (+ h.u h.normal.y) (if h.hit 1. 0.)])) (array/range 16 :type "int"))"#.into()),
        ("fast_gather", r#"(let* [table (array/float 7 0.25)] (map (fn [i] (+ t (array/nth table (int/mod i 7)))) (array/range 16 :type "int")))"#.into()),
        ("fast_list_gather", "(let* [table (list [1 0 0] [0 1 0] [0 0 1])] (map (fn [(i : int)] (+ (nth table (int/mod i 3)) [t 0. 0.])) (array/range 16 :type int)))".into()),
        ("fast_list_float_index", "(let* [table (list 0.25 0.5 0.75)] (map (fn [(i : int)] (+ (nth table (float (int/mod i 3))) t)) (array/range 16 :type int)))".into()),
        ("fast_inner_map_sum", "(map (fn [(x : int)] (+ (array/sum (map (fn [(j : int)] (+ x j)) (array/range 3 :type int))) (frame/index))) (array/range 16 :type int))".into()),
        ("fast_inner_slice_sum", "(let* [table (array/range 64 :type int)] (map (fn [(x : int)] (+ (array/sum (array/slice table (int/mod x 61) 3)) (frame/index))) (array/range 16 :type int)))".into()),
        ("fast_vec_gather", r#"(let* [table (map (fn [x] [x (+ x 1.) (- x 1.)]) (array/range 7))] (map (fn [i] (+ (array/nth table (int/mod i 7)) [t 0. 0.])) (array/range 16 :type "int")))"#.into()),
        ("fast_int_gather", r#"(let* [table (array/int 7 12345)] (map (fn [i] (+ t (float (array/nth table (int/mod i 7))))) (array/range 16 :type "int")))"#.into()),
        ("fast_whole", r#"(map (fn [x] (let* [n (int/add x 4294967297) floating (float n)] [(float (int (- floating 4294967296.))) (+ floating 1.) (float (int (mod floating 7.))) t])) (array/range 16 :type "int"))"#.into()),
        ("fast_guarded_whole", r#"(map (fn [x] (let* [n (float (int/add x 4294967297)) v (if (int/lt x 7) (+ n 1.) (- n 1.))] [(float (int v)) v t])) (array/range 16 :type "int"))"#.into()),
        ("fast_invalid_shift", "(map (fn [(x : int)] (float (int/shr x (int/add 64 (int/mod (frame/index) 1))))) (array/range 16 :type int))".into()),
        ("fast_negative_shift", "(map (fn [(x : int)] (float (int/shr x (int/sub (frame/index) (int/add (frame/index) 1))))) (array/range 16 :type int))".into()),
        ("fast_divide_by_zero", "(map (fn [(x : int)] (float (int/div x (int/sub (frame/index) (frame/index))))) (array/range 16 :type int))".into()),
        ("fast_noise_octaves", "(map (fn [x] (noise3 [x t 0.25] :seed 31 :octaves (int/add 2 (frame/index)))) (array/range 16))".into()),
        ("fast_modulo_by_zero", "(map (fn [(x : int)] (float (int/mod x (int/sub (frame/index) (frame/index))))) (array/range 16 :type int))".into()),
    ];
    v.drain(..).map(|(n, s)| (n.to_owned(), s)).collect()
}

fn fast_cases() -> Vec<(String, flow_ir::packed::T)> {
    let mut out: Vec<(String, flow_ir::packed::T)> = fast_sources().into_iter().map(|(n, s)| (n, tp::compile(&s))).collect();
    out.extend(tp::carried_fixtures(16));
    out
}

/// Exact and fast kernel digests (fast: 32-bit Int registers), rewritten with `RAYS_UPDATE_FIXTURES=1`.
const EXACT_GOLDEN: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/flow_gpu/goldens/spirv.sexp");
const FAST_GOLDEN: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/spirv_fast.sexp");

fn golden(path: &str) -> HashMap<String, (usize, String)> {
    let text = fs::read_to_string(path).unwrap_or_default();
    text.lines()
        .map(|line| {
            let parts: Vec<&str> = line.trim_matches(|c| c == '(' || c == ')').split(' ').collect();
            (parts[0].to_owned(), (parts[1].parse().unwrap(), parts[2].to_owned()))
        })
        .collect()
}

fn has_int64(bytes: &[u8]) -> bool {
    let word = |at: usize| u32::from_le_bytes([bytes[at], bytes[at + 1], bytes[at + 2], bytes[at + 3]]);
    let mut at = 20;
    while at < bytes.len() {
        let instruction = word(at);
        if instruction & 0xffff == 17 && word(at + 4) == 11 {
            return true;
        }
        at += (instruction >> 16) as usize * 4;
    }
    false
}

/// One case: the shared assertions, then the digest against the golden.
fn check(name: &str, fast: bool, program: &flow_ir::packed::T, golden: &HashMap<String, (usize, String)>) -> (usize, String) {
    let instanced = name.ends_with("instanced");
    let curves = name.ends_with("curves");
    let options = spirv::Options { fast, instanced, curves, ..spirv::Options::default() };
    let emitted = spirv::kernel(&options, program).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
    let again = spirv::kernel(&options, program).unwrap();
    assert_eq!(emitted.bytes, again.bytes, "{name}: deterministic");
    let word = |at: usize| u32::from_le_bytes([emitted.bytes[at], emitted.bytes[at + 1], emitted.bytes[at + 2], emitted.bytes[at + 3]]);
    assert_eq!(word(0), 0x07230203);
    assert_eq!(word(4), if emitted.layout.scene_binding.is_none() { 0x00010300 } else { 0x00010400 });
    assert_eq!(emitted.bytes.len() % 4, 0);
    if fast {
        assert!(!has_int64(&emitted.bytes), "{name}: a fast kernel needs no int64 capability");
    }
    assert!(shader::validate_bindings(&emitted.interface).is_ok());
    assert_eq!(emitted.uniform_bytes, kernel_layout::uniform_words(&emitted.layout) * 16);
    let metal = emit::kernel(
        &emit::Options { fast, instanced, curves: if curves { Some(Curves::Loop) } else { None }, ..emit::Options::default() },
        program,
    )
    .unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
    assert_eq!(emitted.interface, metal.interface, "SPIR-V/Metal binding interface differs for {name}");
    assert_eq!(emitted.layout.uniform_layout, metal.uniform_layout);
    assert_eq!(emitted.layout.table_seeds, metal.table_seeds);
    assert_eq!(emitted.layout.gathers.iter().map(|(_, g)| *g).collect::<Vec<_>>(), metal.gathers);
    assert_eq!(emitted.layout.counts, metal.counts);
    let printed = (emitted.bytes.len(), md5_hex(&emitted.bytes));
    if golden.is_empty() {
        return printed;
    }
    let (size, digest) = golden.get(name).unwrap_or_else(|| panic!("no golden for {name}"));
    assert_eq!(printed, (*size, digest.clone()), "{name}: digest");
    printed
}

fn check_all(cases: Vec<(String, flow_ir::packed::T)>, fast: bool) {
    let update = std::env::var_os("RAYS_UPDATE_FIXTURES").is_some();
    let golden = if update { HashMap::new() } else { golden(if fast { FAST_GOLDEN } else { EXACT_GOLDEN }) };
    let lines: Vec<String> = cases.iter().map(|(name, program)| {
        let (size, digest) = check(name, fast, program, &golden);
        format!("({name} {size} {digest})\n")
    }).collect();
    if update {
        fs::write(if fast { FAST_GOLDEN } else { EXACT_GOLDEN }, lines.concat()).unwrap();
    }
}

#[test]
fn exact_kernels_match_the_digest_goldens() {
    check_all(cases(), false);
}

#[test]
fn fast_kernels_match_the_digest_goldens() {
    let (sources, carried): (Vec<_>, Vec<_>) = fast_cases().into_iter().partition(|(n, _)| n.starts_with("fast_"));
    let mut all = sources;
    all.extend(carried);
    check_all(all, true);
}

#[test]
fn moving_scenes_and_unfolded_kernels_print() {
    let find = |name: &str| fast_sources().into_iter().find(|(n, _)| n == name).map(|(_, s)| tp::compile(&s)).unwrap();
    // moving scenes compile to candidate-AABB ray queries instead of a static fallback
    let moving = spirv::kernel(&spirv::Options { fast: true, motion: true, ..spirv::Options::default() }, &find("fast_trace")).unwrap();
    assert_eq!(&moving.bytes[0..4], &0x07230203u32.to_le_bytes());
    assert!(moving.layout.scene_motion);
    assert!(shader::validate_bindings(&moving.interface).is_ok());
    let instances =
        spirv::kernel(&spirv::Options { fast: true, instanced: true, motion: true, ..spirv::Options::default() }, &find("fast_trace_instanced")).unwrap();
    assert!(instances.layout.scene_motion);
    assert!(shader::validate_bindings(&instances.interface).is_ok());
    // an unfolded kernel: its passes in a structured loop over function variables
    let unfolded = spirv::kernel(&spirv::Options { fast: true, unfold: Some(4), ..spirv::Options::default() }, &find("fast_float")).unwrap();
    assert_eq!(&unfolded.bytes[0..4], &0x07230203u32.to_le_bytes());
    assert!(shader::validate_bindings(&unfolded.interface).is_ok());
}
