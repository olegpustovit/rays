//! compile a body to a packed region through the
//! real checker, evaluator and `packed::compile_result`.
use flow_check::catalog::Catalog;
use flow_check::workspace::{self as W, Path};
use flow_eval as E;
use flow_ir::core_ir::region;
use flow_ir::packed::{self, CompileOptions};
use flow_syntax::syntax;
use flow_syntax::value::Value;

fn checked(declarations: &str, body: &str) -> W::Workspace {
    let text = format!("(workspace w {declarations} (graph g :context value (let* [tested {body}] 0.0)))");
    let forms = syntax::parse(&text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
    }
}

fn recorded(declarations: &str, body: &str) -> Value {
    let evaluation = E::static_with(&checked(declarations, body), true, None, None).expect("static");
    let path: Path = vec!["g".into(), "tested".into()];
    evaluation.records.iter().find(|(p, _)| *p == path).expect("tested").1[0].1.clone()
}

pub fn compile_with(declarations: &str, fusion: bool, body: &str) -> packed::T {
    let Value::Residual(residual) = recorded(declarations, body) else { panic!("expected live packed program: {body}") };
    let options = CompileOptions { fusion, ..Default::default() };
    let view = E::residual::residual_view(residual).unwrap();
    match packed::compile_result(&options, residual, &view.term) {
        Ok(program) => program,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
    }
}

pub fn compile(body: &str) -> packed::T { compile_with("", true, body) }

pub fn compile_unfused(body: &str) -> packed::T { compile_with("", false, body) }

/// `Test_program.ok` is `result.unwrap_or_else(|d| die(d))` here (no generic helpers in this tree).
pub fn die(d: rays_diagnostic::Diagnostic) -> ! { panic!("{}", rays_diagnostic::to_string(&d)) }

pub fn fixtures(count: i64) -> Vec<(&'static str, packed::T)> {
    let array = format!("(array/range {count})");
    vec![
        ("arithmetic", compile(&format!("(map (fn [x] (+ (* x 2) t)) {array})"))),
        ("select", compile(&format!("(map (fn [x] (if (< x t) (+ x 1) (- x 1))) {array})"))),
        ("noise", compile(&format!("(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) {array})"))),
    ]
}

pub fn vector_fixtures(count: i64) -> Vec<(&'static str, packed::T)> {
    let array = format!("(array/range {count})");
    vec![
        ("vec2", compile(&format!("(map (fn [x] [x (+ x t)]) {array})"))),
        ("vec4", compile_unfused(&format!("(map (fn [(p : vec2)] [p.y p.x t 1])\n     (map (fn [x] [x (+ x 1)]) {array}))"))),
        (
            "vec4-input",
            compile_unfused(&format!("(let* [offset [t 1 2 3]]\n     (map (fn [(p : vec4)] (+ p offset)) (map (fn [x] [x 1 2 3]) {array})))")),
        ),
    ]
}

const INT64_CHECKS: &str = "(let* [n (int/add (int x) (frame/index))
  d (int/add (int/mod n 4) 1) even (int/mul d 2)
  low (int/shl 1 (int/add 63 (int/mod n 1)))]
  [(if (int/lt (int/add 9223372036854775807 d) 0) 1. 0.)
   (if (int/eq (int/mul 9223372036854775807 even) (int/sub 0 even)) 1. 0.)
   (if (int/eq (int/shr (int/sub 0 d) 63) 1) 1. 0.)
   (if (and (int/lt (abs low) 0) (int/eq (int/div low -1) low)) 1. 0.)])";

pub fn carried_fixtures(count: i64) -> Vec<(String, packed::T)> {
    let mut cases: Vec<(String, String)> = vec![
        ("loop-bounds".into(), "(fold [a t] [i (range -2 (int/mod (int x) 5))] (+ a (* i 0.5)))".into()),
        ("loop-empty".into(), "(fold [a t] [i (range 0)] (+ a x))".into()),
        ("loop-nested".into(), "(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))".into()),
        (
            "loop-record".into(),
            "(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false}]
      [i (range (int/mod (int x) 5))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)] :flag (not s.flag)})]
      [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])"
                .into(),
        ),
        ("loop-guard".into(), "(if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)".into()),
        ("iterate-count".into(), "(iterate (- (int/mod (int x) 7) 2) t (fn [a] (+ a x)))".into()),
        (
            "iterate-until".into(),
            "(iterate 4611686018427387903 (+ x t) (fn [a] (+ a 1.))
      :until (fn [prev next] (>= next (+ (+ x t) 3.))))"
                .into(),
        ),
        (
            "iterate-old-state".into(),
            "(let* [s (iterate 4611686018427387903 {:flag true :n t}
      (fn [a] {:flag false :n (+ a.n x)}) :until (fn [prev next] prev.flag))] s.n)"
                .into(),
        ),
        ("iterate-int63".into(), format!("(iterate 1 [t 0. 0. 0.] (fn [unused] {INT64_CHECKS}))")),
        (
            "loop-uint32".into(),
            "(let* [s (fold [s {:state (int/and (int x) 4294967295) :value 0}]
      [i (range 3)] (let* [state (int/and (int/add (int/mul s.state 747796405) 2891336453) 4294967295)
        word (int/and (int/mul (int/xor (int/shr state (int/add (int/shr state 28) 4)) state) 277803737) 4294967295)
        value (int/and (int/xor (int/shr word 22) word) 65535)] {:state state :value value}))]
      (+ (float s.value) t))"
                .into(),
        ),
    ];
    let mut out: Vec<(String, packed::T)> =
        cases.drain(..).map(|(name, body)| (name, compile(&format!("(map (fn [x] {body}) (array/range {count}))")))).collect();
    out.push((
        "iterate-captured-function".into(),
        compile(&format!("(let* [offset t f (fn [a] (+ a offset))]\n    (map (fn [x] (iterate 3 x f)) (array/range {count})))")),
    ));
    for (name, body) in [
        (
            "iterate-fused-producer",
            "(map (fn [y] (if (> y t) (* y 2.) t))
      (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.)))) (array/range %d)))",
        ),
        (
            "iterate-fused-consumer",
            "(map (fn [y] (iterate 3 y (fn [a] (+ a y))))
      (map (fn [x] (+ x t)) (array/range %d)))",
        ),
        (
            "iterate-fused-both",
            "(map (fn [y] (if (> y t)
        (iterate 4611686018427387903 y (fn [a] (+ a 1.))
          :until (fn [prev next] (>= next (+ y 2.)))) t))
      (map (fn [x] (if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)) (array/range %d)))",
        ),
        (
            "iterate-fused-integer",
            "(map (fn [y] (float (iterate 3 y
        (fn [a] (int/xor a (frame/index))))))
      (map (fn [x] (iterate 2 (int/add (int x) (frame/index))
        (fn [a] (int/add a 4611686018427387903)))) (array/range %d)))",
        ),
    ] {
        out.push((name.into(), compile(&body.replace("%d", &count.to_string()))));
    }
    out
}

/// Attribute fusion will produce several outputs; no surface does yet, so the region is built by
/// hand: a scalar and a vec2 from one register program.
pub fn two_outputs() -> packed::T {
    let region = compile("(map (fn [x] [x (+ x t)]) (array/range 1024))");
    let o = region.outputs[0].clone();
    match region::rebuild(&region, region::Rebuild { outputs: Some(vec![vec![o[1]], vec![o[0], o[1]]]), ..Default::default() }) {
        Ok(program) => program,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}
