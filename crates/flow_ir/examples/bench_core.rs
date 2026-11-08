//! Packed CPU kernels at 1 and 8 domains: milliseconds per force, min and median of eleven samples,
//! plus an FNV digest of the forced bits (equal before and after any engine change, equal across
//! domains) and, for small cases, equality with the reference interpreter.
//! `cargo run -p flow_ir --example bench_core [-- filter]`; add `--release` for release numbers.
//! Compare two builds by copying the binary and alternating runs: the machine is rarely idle.
use flow_check::catalog::Catalog;
use flow_check::workspace as W;
use flow_eval as E;
use flow_ir::packed::{self, CompileOptions, ForceOptions};
use flow_syntax::syntax;
use flow_syntax::value::Value;
use std::cell::Cell;
use std::time::Instant;

const SAMPLES: usize = 11;

fn compile(body: &str, fusion: bool) -> packed::T {
    let text = format!("(workspace w (graph g :context value (let* [tested {body}] 0.0)))");
    let forms = syntax::parse(&text).expect("parse");
    let workspace = match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let evaluation = E::static_with(&workspace, true, None, None).expect("static");
    let path: flow_check::workspace::Path = vec!["g".to_owned(), "tested".to_owned()];
    let Value::Residual(residual) = evaluation.records.iter().find(|(p, _)| *p == path).expect("tested").1[0].1.clone() else {
        panic!("kernel did not defer: {body}")
    };
    let term = E::residual::residual_view(residual).unwrap().term;
    packed::compile(&CompileOptions { fusion, ..Default::default() }, residual, &term).expect("kernel does not compile")
}

fn digest(v: &Result<Value, rays_diagnostic::Diagnostic>) -> u64 {
    let mut h = 0xcbf2_9ce4_8422_2325u64;
    let mut eat = |x: u64| h = (h ^ x).wrapping_mul(0x100_0000_01b3);
    match v {
        Ok(Value::Floats(1..=4, a)) => a.iter().for_each(|x| eat(x.to_bits())),
        Ok(Value::Int_array(a)) => a.iter().for_each(|x| eat(*x as u64)),
        Ok(Value::Float(x)) => eat(x.to_bits()),
        Ok(other) => format!("{other:?}").bytes().for_each(|b| eat(b as u64)),
        Err(d) => rays_diagnostic::to_string(d).bytes().for_each(|b| eat(b as u64)),
    }
    h
}

/// (min, median) seconds per call over `SAMPLES` samples of `reps` calls.
fn measure(reps: usize, f: &mut dyn FnMut()) -> (f64, f64) {
    f();
    let mut samples: Vec<f64> = (0..SAMPLES)
        .map(|_| {
            let start = Instant::now();
            for _ in 0..reps {
                f();
            }
            start.elapsed().as_secs_f64() / reps as f64
        })
        .collect();
    samples.sort_by(f64::total_cmp);
    (samples[0], samples[SAMPLES / 2])
}

fn map(n: usize) -> String {
    format!("(map (fn [x] (+ (* x 2) t)) (array/range {n}))")
}

fn chain(op: &str, initial: &str, passes: usize) -> String {
    (0..passes).fold(initial.to_owned(), |value, _| format!("({op} {value} x)"))
}

fn main() {
    let filter = std::env::args().nth(1).unwrap_or_default();
    // (label, body, fusion, reps per sample, compare with the reference interpreter)
    let mut cases: Vec<(String, String, bool, usize, bool)> = vec![
        ("map 1,024".into(), map(1024), true, 200, true),
        ("map 1,000,000".into(), map(1_000_000), true, 2, false),
        ("fused maps 1,000,000".into(), format!("(map (fn [y] (* y 0.5)) {})", map(1_000_000)), true, 2, false),
        ("sum 1,000,000".into(), "(sum [x (array/range 1000000)] (* x t))".into(), true, 2, false),
        ("fold-add 1,000,000".into(), "(fold [a t] [x (array/range 1000000)] (+ a x))".into(), true, 2, false),
        ("reduce-add 1,000,000".into(), "(reduce + t (array/range 1000000))".into(), true, 2, false),
        ("reduce vec3 add 65,536".into(), "(reduce + [t 0.0 0.0] (array/vec3 65536 [0.25 -0.0 1.0]))".into(), true, 5, false),
        ("fold vec3 add 65,536".into(), "(fold [a [t 0.0 0.0]] [x (array/vec3 65536 [0.25 -0.0 1.0])] (+ a x))".into(), true, 5, false),
        ("unary 1,000,000".into(), "(map (fn [x] (sin (+ (* x 0.001) t))) (array/range 1000000))".into(), true, 2, false),
        ("select 1,000,000".into(), "(map (fn [x] (if (< (mod x 7.0) 3.0) (* x t) (+ x 1.0))) (array/range 1000000))".into(), true, 2, false),
        ("vec3 1,000,000".into(), "(map (fn [x] [x (* x t) (+ x 1.0)]) (array/range 1000000))".into(), true, 2, false),
        ("noise3 65,536".into(), "(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31)) (array/range 65536))".into(), true, 3, false),
        ("noise3 3 octaves 65,536".into(), "(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) (array/range 65536))".into(), true, 3, false),
        (
            "displace zip 65,536".into(),
            "(map (fn [p n] (+ p (* n (* (noise3 (* p 0.5) :seed 7) t)))) (array/vec3 65536 [1 2 3]) (array/vec3 65536 [0 1 0]))".into(),
            true,
            3,
            false,
        ),
        (
            "height zip 65,536".into(),
            "(map (fn [h p] (+ h (* (- (* (noise3 [(* p.x 0.1) (* p.y 0.1) 0.0] :seed 7) 2.0) 0.5) t))) (array/float 65536 1.0) (array/vec3 65536 [1 2 3]))".into(),
            true,
            3,
            false,
        ),
        ("text map 5,001".into(), r#"(map (fn [x] (if (> t 0.) x "😀")) (array/text 5001 "猫"))"#.into(), true, 20, false),
        ("int map 1,000,000".into(), "(map (fn [x] (int/add (int/mul x 3) (int t))) (array/range 1000000 :type int))".into(), true, 2, false),
        ("int to float 1,000,000".into(), "(map (fn [x] (* (float x) t)) (array/range 1000000 :type int))".into(), true, 2, false),
    ];
    // the 47-instruction small-map scratch bodies, float and integer
    for count in [1usize, 11, 65, 1024] {
        let body = chain("+", "(+ x t)", 44);
        cases.push((format!("scratch float {count}"), format!("(map (fn [x] {body}) (array/range {count}))"), true, 100, true));
        let body = chain("int/add", "(int/add x (int t))", 43);
        cases.push((format!("scratch int {count}"), format!("(map (fn [x] {body}) (array/range {count} :type int))"), true, 100, true));
    }
    println!("{:<26} {:>7} {:>11} {:>11}  digest", "case", "domains", "min ms", "median ms");
    for (label, body, fusion, reps, check) in cases.iter().filter(|c| c.0.contains(&filter)) {
        let program = compile(body, *fusion);
        let live = input::at_time(0.37);
        let options = ForceOptions { strict: true, ..Default::default() };
        let mut first = None;
        for domains in [1, 8] {
            let row = Cell::new((0.0, 0.0, 0u64));
            parallel::run_unit(Some(domains), &|| {
                let forced = packed::force(&program, &live, &options);
                let sum = digest(&forced);
                if *check {
                    let reference = digest(&packed::reference(&program, &live, None, None, None));
                    assert_eq!(sum, reference, "{label}: packed differs from the reference interpreter");
                }
                let (min, median) = measure(*reps, &mut || {
                    std::hint::black_box(packed::force(&program, &live, &options).ok());
                });
                row.set((min, median, sum));
                Ok(())
            })
            .unwrap();
            let (min, median, sum) = row.get();
            assert_eq!(*first.get_or_insert(sum), sum, "{label}: one and eight domains differ");
            println!("{label:<26} {domains:>7} {:>11.4} {:>11.4}  {sum:016x}", min * 1e3, median * 1e3);
        }
    }
}
