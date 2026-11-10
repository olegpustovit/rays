#![allow(dead_code)]
use flow_syntax::symbol::Symbol;
use crate::core_exec;
use crate::core_ir as c;
use crate::core_lower;
use crate::flow_ir::executor;
use crate::packed;
use flow_check::catalog::Catalog;
use flow_check::op::{self, Op, Signature};
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::value::{ResidualId, Value};
use flow_syntax::{context, syntax, ty::Ty};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

pub type Trace = Arc<dyn Fn(&Result<packed::T, Vec<Diagnostic>>) + Send + Sync>;

pub fn ok<T>(r: Result<T, Diagnostic>) -> T {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}
pub fn show_ds(ds: &[Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")
}
pub fn catalog() -> Catalog {
    Catalog { version: 1, kinds: vec![] }
}
pub fn residual(v: &Value) -> ResidualId {
    match v {
        Value::Residual(r) => *r,
        _ => panic!("expected a residual"),
    }
}
pub fn live_at(t: f64) -> Frame {
    input::at_time(t)
}
pub fn live_frame(t: f64, frame: i64) -> Frame {
    let mut f = input::at_time(t);
    f.frame = frame;
    f
}
pub fn with_domains(domains: i64, f: &dyn Fn()) {
    parallel::run_unit(Some(domains), &|| {
        f();
        Ok(())
    })
    .unwrap();
}
pub fn bits(v: &Value) -> String {
    format!("{:?}", v)
}

pub fn check_workspace(source: &str) -> w::Workspace {
    let forms = ok(syntax::parse(source));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}

// ----

pub fn compile_with(trace: Option<Trace>, fusion: bool, declarations: &str, body: &str) -> packed::T {
    let source = format!("(workspace w {declarations} (graph g :context value (let* [tested {body}] 0.0)))");
    let workspace = check_workspace(&source);
    let evaluated = ok(e::r#static(&workspace, true, None));
    let (_, records) = evaluated
        .records
        .iter()
        .find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()])
        .expect("record g/tested");
    match &records[0].1 {
        Value::Residual(r) => {
            let term = e::residual::residual_view(*r).unwrap().term;
            let options = packed::CompileOptions { trace, fusion, ..Default::default() };
            match packed::compile_result(&options, *r, &term) {
                Ok(p) => p,
                Err(ds) => panic!("{}", show_ds(&ds)),
            }
        }
        _ => panic!("expected live packed program"),
    }
}
pub fn compile(body: &str) -> packed::T {
    compile_with(None, true, "", body)
}
pub fn compile_unfused(body: &str) -> packed::T {
    compile_with(None, false, "", body)
}

pub fn fixtures(trace: Option<Trace>, count: i64) -> Vec<(String, packed::T)> {
    let array = format!("(array/range {count})");
    let c = |b: String| compile_with(trace.clone(), true, "", &b);
    vec![
        ("arithmetic".into(), c(format!("(map (fn [x] (+ (* x 2) t)) {array})"))),
        ("select".into(), c(format!("(map (fn [x] (if (< x t) (+ x 1) (- x 1))) {array})"))),
        ("noise".into(), c(format!("(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) {array})"))),
    ]
}

pub fn vector_fixtures(trace: Option<Trace>, count: i64) -> Vec<(String, packed::T)> {
    let array = format!("(array/range {count})");
    vec![
        ("vec2".into(), compile_with(trace.clone(), true, "", &format!("(map (fn [x] [x (+ x t)]) {array})"))),
        (
            "vec4".into(),
            compile_with(trace.clone(), false, "", &format!("(map (fn [(p : vec2)] [p.y p.x t 1])\n     (map (fn [x] [x (+ x 1)]) {array}))")),
        ),
        (
            "vec4-input".into(),
            compile_with(
                trace.clone(),
                false,
                "",
                &format!("(let* [offset [t 1 2 3]]\n     (map (fn [(p : vec4)] (+ p offset)) (map (fn [x] [x 1 2 3]) {array})))"),
            ),
        ),
    ]
}

pub const INT64_CHECKS: &str = "(let* [n (int/add (int x) (frame/index))\n  d (int/add (int/mod n 4) 1) even (int/mul d 2)\n  low (int/shl 1 (int/add 63 (int/mod n 1)))]\n  [(if (int/lt (int/add 9223372036854775807 d) 0) 1. 0.)\n   (if (int/eq (int/mul 9223372036854775807 even) (int/sub 0 even)) 1. 0.)\n   (if (int/eq (int/shr (int/sub 0 d) 63) 1) 1. 0.)\n   (if (and (int/lt (abs low) 0) (int/eq (int/div low -1) low)) 1. 0.)])";

pub fn carried_fixtures(count: i64) -> Vec<(String, packed::T)> {
    let mapped: Vec<(&str, String)> = vec![
        ("loop-bounds", "(fold [a t] [i (range -2 (int/mod (int x) 5))] (+ a (* i 0.5)))".into()),
        ("loop-empty", "(fold [a t] [i (range 0)] (+ a x))".into()),
        ("loop-nested", "(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))".into()),
        (
            "loop-record",
            "(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false}]\n      [i (range (int/mod (int x) 5))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)] :flag (not s.flag)})]\n      [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])".into(),
        ),
        ("loop-guard", "(if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)".into()),
        ("iterate-count", "(iterate (- (int/mod (int x) 7) 2) t (fn [a] (+ a x)))".into()),
        (
            "iterate-until",
            "(iterate 4611686018427387903 (+ x t) (fn [a] (+ a 1.))\n      :until (fn [prev next] (>= next (+ (+ x t) 3.))))".into(),
        ),
        (
            "iterate-old-state",
            "(let* [s (iterate 4611686018427387903 {:flag true :n t}\n      (fn [a] {:flag false :n (+ a.n x)}) :until (fn [prev next] prev.flag))] s.n)".into(),
        ),
        ("iterate-int63", format!("(iterate 1 [t 0. 0. 0.] (fn [unused] {INT64_CHECKS}))")),
        (
            "loop-uint32",
            "(let* [s (fold [s {:state (int/and (int x) 4294967295) :value 0}]\n      [i (range 3)] (let* [state (int/and (int/add (int/mul s.state 747796405) 2891336453) 4294967295)\n        word (int/and (int/mul (int/xor (int/shr state (int/add (int/shr state 28) 4)) state) 277803737) 4294967295)\n        value (int/and (int/xor (int/shr word 22) word) 65535)] {:state state :value value}))]\n      (+ (float s.value) t))".into(),
        ),
    ];
    let mut out: Vec<(String, packed::T)> = mapped
        .into_iter()
        .map(|(name, body)| (name.to_string(), compile(&format!("(map (fn [x] {body}) (array/range {count}))"))))
        .collect();
    out.push((
        "iterate-captured-function".into(),
        compile(&format!("(let* [offset t f (fn [a] (+ a offset))]\n    (map (fn [x] (iterate 3 x f)) (array/range {count})))")),
    ));
    let fused: Vec<(&str, String)> = vec![
        ("iterate-fused-producer", format!("(map (fn [y] (if (> y t) (* y 2.) t))\n      (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.)))) (array/range {count})))")),
        ("iterate-fused-consumer", format!("(map (fn [y] (iterate 3 y (fn [a] (+ a y))))\n      (map (fn [x] (+ x t)) (array/range {count})))")),
        (
            "iterate-fused-both",
            format!("(map (fn [y] (if (> y t)\n        (iterate 4611686018427387903 y (fn [a] (+ a 1.))\n          :until (fn [prev next] (>= next (+ y 2.)))) t))\n      (map (fn [x] (if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)) (array/range {count})))"),
        ),
        (
            "iterate-fused-integer",
            format!("(map (fn [y] (float (iterate 3 y\n        (fn [a] (int/xor a (frame/index))))))\n      (map (fn [x] (iterate 2 (int/add (int x) (frame/index))\n        (fn [a] (int/add a 9223372036854775807)))) (array/range {count})))"),
        ),
    ];
    out.extend(fused.into_iter().map(|(n, b)| (n.to_string(), compile(&b))));
    out
}

/// Attribute fusion will produce several outputs; no surface does yet, so the region is built by
/// hand: a scalar and a vec2 from one register program.
pub fn two_outputs() -> packed::T {
    let region = compile("(map (fn [x] [x (+ x t)]) (array/range 1024))");
    let o = region.outputs[0].clone();
    let rebuild = c::region::Rebuild { outputs: Some(vec![vec![o[1]], vec![o[0], o[1]]]), ..Default::default() };
    match c::region::rebuild(&region, rebuild) {
        Ok(p) => p,
        Err(ds) => panic!("{}", show_ds(&ds)),
    }
}

pub fn prepare(program: &c::Program) -> core_exec::Prepared {
    core_exec::prepare(program, false, false, &Default::default(), false)
}
pub fn prepare_clobber(program: &c::Program) -> core_exec::Prepared {
    core_exec::prepare(program, true, false, &Default::default(), false)
}

/// `Test_program.run`: a clobbering prepare, then execute.
pub fn run(
    root: Option<ResidualId>,
    strict: bool,
    force: Option<core_exec::ForceFn>,
    resolve: Option<core_exec::ResolveFn>,
    program: &c::Program,
    live: &Frame,
) -> Result<Value, Diagnostic> {
    let options = core_exec::Options { root, strict, force, resolve, ..Default::default() };
    core_exec::execute(&prepare_clobber(program), live, &options)
}
pub fn run_simple(program: &c::Program, live: &Frame) -> Result<Value, Diagnostic> {
    run(None, false, None, None, program, live)
}

/// A Core program outside its regions is plain data. ponytail: clone checks compare Debug output; use raw-bit structural equality if payload distinctions require it.
pub fn plain_roundtrip(p: &c::Program) {
    let copy = p.clone();
    assert_eq!(
        format!("{:?}", (&p.constants, &p.locals, &p.body, &p.results, &p.state_slots, p.call_slots)),
        format!("{:?}", (&copy.constants, &copy.locals, &copy.body, &copy.results, &copy.state_slots, copy.call_slots)),
        "Core program does not round-trip as plain data"
    );
}

/// The strict lowering of a residual value (`Core_lower.lower ~strict:true r`).
pub fn lower_strict(r: ResidualId) -> Result<c::Program, String> {
    core_lower::lower(None, None, true, None, None, r)
}

// ----

pub fn offset_op() -> Op {
    let mut o = op::find(&[], "+", &context::value()).expect("+ op").clone();
    o.name = "test/offset".into();
    o.signature = Signature { pos: vec![("value".into(), Ty::Float)], opt: vec![], rest: None, kw: vec![("offset".into(), Ty::Float)] };
    o
}

pub fn workspace(defs: &str, inputs: &str, body: &str) -> String {
    format!("(workspace w {defs} (graph g :context value {inputs} (let* [tested {body}] 0.)))")
}
fn ws(body: &str) -> String {
    workspace("", "", body)
}

pub fn control() -> Vec<(String, String)> {
    let p = |n: &str, s: String| (n.to_string(), s);
    vec![
        p("vec2", ws("[t (+ t 1.)]")),
        p("vec4", ws("[t (+ t 1.) (- 0. t) (* t 2.)]")),
        p("cond", ws("(cond (< t 0.) (- 0. t) (> t 1.) (* t 2.) :else (+ t 0.5))")),
        p("case-number", ws("(case (int t) 0 10. 1 20. :else 30.)")),
        p("case-bool", ws("(case t false 10. true 20. :else 30.)")),
        p("case-text", ws("(case (if (> t 0.) \"a\" \"b\") \"b\" (- 0. t) :else (+ t 1.))")),
        p("case-rounding", ws("(case (+ 9007199254740992 t) 9007199254740993 1. :else 2.)")),
        p("let-vector", ws("(if (> t 0.) (let* [[x y] [t (* t 2.)]] (+ x y)) (- 0. t))")),
        p("let-vector4", ws("(if (> t 0.) (let* [[x y z w] [t (+ t 1.) (- 0. t) (* t 2.)]] (+ x (+ y (+ z w)))) t)")),
        p("let-nested", workspace("", "[(r : (list vec2) (list [1. 2.] [3. 4.]))]", "(if (> t 0.) (let* [[[a b] [c d]] r] (+ t (+ a (+ b (+ c d))))) t)")),
        p("let-list", ws("(if (> t 0.) (let* [[x y] (for [i (range 3)] (+ t i))] (+ x y)) t)")),
        p("let-keys", workspace("", "[(r : {:a float :b float} {:a 2. :b 3.})]", "(if (> t 0.) (let* [{:keys [a b]} r] (+ t (+ a b))) t)")),
        p("local-fn", ws("(if (> t 0.) (let* [bias (* t 2.) f (fn [x] (+ x bias))] (+ (f t) (f 1.))) t)")),
        p("typed-fn", ws("(if (> t -10.) (let* [f (fn [(x : int)] (+ x 1))] (f t)) 0)")),
        p("typed-vector-fn", ws("(if (> t -10.) (let* [f (fn [(x : vec4)] x.w)] (f t)) 0.)")),
        p("pattern-fn", ws("(if (> t 0.) (let* [f (fn [[x y]] (+ x y))] (f [t (* t 2.)])) t)")),
        p("named-fn", workspace("(defn f :context value [(x : float) (bias : float 2.)] (+ x bias))", "", "(if (> t 0.) (f t) t)")),
        p("operator-fn", workspace("(defn use2 :context value [(f : fn) (x : float) (y : float)] (f x y))", "", "(if (> t 0.) (use2 + t 2.) t)")),
        p("callable-argument", workspace("(defn use :context value [(f : fn) (x : float)] (f x))", "", "(if (> t 0.) (let* [bias t f (fn [x] (+ x bias))] (use f t)) t)")),
        p("macro", workspace("(defmacro twice [x] `(+ ~x ~x))", "", "(if (> t 0.) (twice t) t)")),
        p("bypass", ws("(if (> t 0.) ^:bypass (+ t (exp 10000.)) t)")),
        p("bypass-keyword", ws("(if (> t 0.) ^:bypass (test/offset :offset (exp 10000.) t) t)")),
        p("bypass-coercion", workspace("(defn asint :context value [(x : float)] (int x))", "", "(if (> t -10.) ^:bypass (asint t) 0)")),
        p("pattern-error", ws("(if (> t 0.) (let* [[x y z] (for [i (range 2)] (+ t i))] (+ x y)) t)")),
        p("untaken-error", ws("(if (> t 0.) (+ t 1.) (exp 10000.))")),
    ]
}

fn pairs(xs: &[(&str, &str)]) -> Vec<(String, String)> {
    xs.iter().map(|(a, b)| (a.to_string(), b.to_string())).collect()
}

pub fn lists() -> Vec<(String, String)> {
    pairs(&[
        ("literal", "(if (> t -1000.) (list t 1 2.) (list))"),
        ("range", "(range t)"),
        ("range-two", "(range (- t 2.) (+ t 3.))"),
        ("range-large-integers", "(range (+ 9007199254740992. (* t 2.)) (+ (+ 9007199254740992. (* t 2.)) 6.))"),
        ("take", "(take t (list 0 1 2))"),
        ("drop", "(drop t (list 0 1 2))"),
        ("rest", "(rest (list t))"),
        ("reverse", "(reverse (list t 1 2))"),
        ("concat", "(concat (list t 1) (list 2 3.))"),
        ("nth", "(nth (list 1 2 3) (int t))"),
        ("count", "(count (range t))"),
        ("map", "(map (fn [x] (+ x t)) (range 1000))"),
        ("map-zip", "(map (fn [x y z] (+ (+ x y) (+ z t))) (list 1 2 3) (list 4) (list 5 6))"),
        ("map-tags", "(map (fn [x] (if (> t 0.) x (float x))) (range 3))"),
        ("map-empty", "(map (fn [x] (/ t 0.)) (range 0))"),
        ("filter", "(filter (fn [x] (> x t)) (range 8))"),
        ("reduce", "(reduce (fn [a x] (+ a (* x t))) 0 (range 1024))"),
        ("reduce-tags", "(reduce (fn [a x] (+ a x)) 0 (list t 0.25))"),
        ("reduce-widened-tag", "(reduce (fn [a x] (if (> x 0.) 1.5 2)) 0 (list t 0))"),
        ("reduce-record", "(reduce (fn [a x] (assoc a :n (+ a.n t))) {:n 0.} (range 3))"),
        ("reduce-record-int", "(reduce (fn [a x] (assoc a :n (+ a.n t))) {:n 0} (range 3))"),
        ("reduce-empty", "(reduce (fn [a x] (/ t 0.)) 0 (range 0))"),
        ("sort", "(sort_by (fn [x] (- t x)) (range 12))"),
        ("sort-stable", "(sort_by (fn [x] (+ t x.k)) (list {:k 2 :id 0} {:k 1 :id 1} {:k 2 :id 2} {:k 1 :id 3}))"),
        ("sort-empty", "(sort_by (fn [x] (/ t 0.)) (range 0))"),
        ("for-large", "(for [i (range 1000)] (+ i t))"),
        ("for-live-items", "(for [i (list t (+ t 1.))] (* i t))"),
        ("for-product", "(for [i (range 2) j (range 3)] :skip [4] (+ (+ (* 10 i) j) t))"),
        ("for-dependent", "(for [i (range 3) j (range i)] (+ (+ (* 10 i) j) t))"),
        ("for-lazy-source", "(for [i (range 0) j (list (/ t 0.))] t)"),
        ("for-skip", "(for [i (range 4)] :skip [0 1 2 3] (/ t 0.))"),
        ("sum-dynamic", "(sum [i (range t)] (* i t))"),
        ("sum-product", "(sum [i (range 4) j (range 3)] (+ (+ i j) t))"),
        ("sum-bound", "(sum [i (range t) j (range 2)] t)"),
        ("fold", "(fold [a 0] [i (range 1000)] (+ a t))"),
        ("fold-record", "(fold [a {:x 0. :y 1.}] [i (range 4)] (assoc a :x (+ a.x t) :y a.x))"),
        ("fold-record-packed", "(fold [a {:n 0.}] [i (array/range 4096)] (assoc a :n (+ a.n t)))"),
        ("iterate", "(iterate (int (+ 3. t)) 0. (fn [a] (+ a t)))"),
        ("iterate-large", "(iterate (+ 4500 (int t)) 0. (fn [a] (+ a 1.)))"),
        ("iterate-until", "(iterate 8 0. (fn [a] (+ a t)) :until (fn [p n] (> n 2.)))"),
        (
            "iterate-huge-until",
            "(iterate 4611686018427387903 t\n    (fn [a] (if (>= a (+ t 2.)) (error \"E_PAST_STOP\" \"body ran after stop\") (+ a 1.)))\n    :until (fn [prev next] (>= next (+ t 2.))))",
        ),
        ("iterate-record", "(iterate 3 {:n 0.} (fn [a] (assoc a :n (+ a.n t))))"),
        ("iterate-list", "(iterate 3 (list t) (fn [a] (list (+ (nth a 0) 1.))))"),
        ("iterate-widen", "(iterate 3 0 (fn [a] (+ a t)))"),
        ("scan", "(scan [a 0] [i (range 4)] (+ a t))"),
        ("scan-record", "(scan [a {:n 0}] [i (range 3)] (assoc a :n (+ a.n t)))"),
        ("nested-skip", "(for [i (range 3)] (for [j (range 2)] :skip [[1 0] [2 1]] (+ (+ (* 10 i) j) t)))"),
        ("captured-list-count", "(let* [xs (list t (/ t 0.))] (+ (count xs) t))"),
        ("captured-list-nth", "(let* [xs (list t 1)] (nth xs 0))"),
        ("captured-list-lazy", "(let* [xs (list t (/ t 0.))] (nth xs 0))"),
        ("captured-list-map", "(let* [xs (list t 1) ys (map (fn [x] (+ x 1.)) xs)] (if (> t -1000.) ys ys))"),
        ("captured-list-reduce", "(let* [xs (list t 1)] (reduce + 0 xs))"),
        ("captured-list-ignored", "(let* [xs (list t (/ t 0.)) ys (map (fn [x] 1) xs)] (+ (first ys) t))"),
        ("list-accumulator", "(fold [a (list 0)] [i (range 2)] (list (+ (nth a 0) t)))"),
        ("empty-list-accumulator", "(fold [a (list)] [i (range 1)] (list t 0.5))"),
        ("empty-list-accumulator-tags", "(fold [a (list)] [i (range 2)] (if (> i 0) (list 1) (list t)))"),
    ])
}

pub fn primitives() -> Vec<(String, String)> {
    pairs(&[
        ("range", "(array/nth (array/range 8) (int (* t 0.)))"),
        ("float", "(array/float (+ 3 (int t)) 1.5)"),
        ("vec3", "(array/nth (array/vec3 4) 2)"),
        ("count", "(array/count (array/float (+ 3 (int t))))"),
        ("nth", "(array/nth (array/range 8) (+ 2 (int t)))"),
        ("slice", "(array/slice (array/range 10) (int t) 4)"),
        ("concat", "(array/concat (array/range (+ 2 (int t))) (array/float 2))"),
        ("noise3", "(noise3 [t (* t 0.5) 1.25] :seed 7 :octaves 3)"),
        ("packed-map-int", "(map (fn [x] [(int x) (* x t) 0]) (array/range (+ 5000 (int t))))"),
        ("packed-map-zip", "(map (fn [p v] (+ p (* v t))) (array/vec3 3 [1 2 3]) (array/vec3 3 [0.5 0.25 0.125]))"),
        ("packed-map-float", "(map (fn [x] (floor (* x t))) (array/range 40))"),
        ("packed-map-empty", "(map (fn [x] [x x x]) (array/range (int (* t 0.))))"),
        ("packed-filter", "(filter (fn [x] (> (int x) (int (* t 10.)))) (array/range (+ 5000 (int t))))"),
        ("packed-filter-vec", "(filter (fn [p] (> p.x t)) (map (fn [x] [x (* x 2.) 0]) (array/range 20)))"),
    ])
}

pub fn regions() -> Vec<(String, String)> {
    pairs(&[
        ("map", "(map (fn [x] (+ (* x t) 1.)) (array/range 2048))"),
        ("for", "(for [x (array/range 2048)] (* x t))"),
        ("sum", "(sum [x (array/range 2048)] (* x t))"),
        ("array-sum", "(array/sum (map (fn [x] (* x t)) (array/range 2048)))"),
        ("fold", "(fold [a 0.] [x (array/range 2048)] (+ a (* x t)))"),
        ("reduce", "(reduce (fn [a x] (+ a (* x t))) 0. (array/range 2048))"),
        ("nonfinite", "(map (fn [x] (/ x (- t t))) (array/range 4))"),
    ])
}

pub fn lower_expressions() -> Vec<String> {
    [
        "(sum [k (range 6)] (/ (sin (* (+ t k) (+ (* 2 k) 1))) (+ (* 2 k) 1)))",
        "(let* [a 3 b (* a 2)] (+ (* t b) (mod t 0.7)))",
        "(if (> (sin t) 0.2) (pow t 2) (- 0 t))",
        "(let* [v [(* t 2) (+ t 1) (min t 3)]] (+ v.x v.y))",
        "(nth (for [i (range 4)] (* i t)) 2)",
        "(sum [x (for [i (range 4)] (+ (* i t) 1))] (* x 2))",
        "(sum [i (range 5)] (sum [j (range 3)] (* (+ t i) j)))",
        "(let* [a t a1 (+ a a) a2 (+ a1 a1) a3 (+ a2 a2)] (* a3 0.5))",
        "(let* [c (value/lerp [1 2 3] [t 0 1] 0.25)] c.z)",
        "(sum [i (range 0)] t)",
        "(/ t 0)",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

pub fn pictures() -> Vec<String> {
    [
        "(draw/background \"#14161c\")",
        "(draw/merge (draw/rect [1 2 0] [30 40 0]) (draw/rect [1 2 0] [3 4 0] :stroke \"#fff\") (draw/rect [0 0 0] [5 5 0] :fill \"#f00\" :stroke \"#00f\"))",
        "(draw/circle [10 20 0] 4)",
        "(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill \"#5aaaf0\")",
        "(draw/line [0 0 0] [5 5 0])",
        "(draw/line [0 0 0] [5 5 0] :color \"#0f0\" :width 3)",
        "(draw/point [3 4 0])",
        "(draw/point [3 4 0] :color \"#abc\")",
        "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate (* t 2) (draw/scale 2 (draw/point [1 1 0])))))",
        "(draw/scale [2 3 0] (draw/background \"#000\"))",
        "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4))] (draw/circles p))",
        "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4)) r (map (fn [i] (* i 0.5)) (array/range 4))] (draw/circles p :radius r :stroke \"#fff\"))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill \"#f00\"))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill c))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/lines p p :width 2))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/points p :color \"#1e1e1e\"))",
        "(draw/text [12 12 0] (str \"frame \" (frame/index)))",
        "(draw/text [12 12 0] \"hi\" :color \"#abc\" :size 20)",
        "(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill \"#123\")",
        "(draw/rounded_rect [4 5 0] [60 40 0])",
        "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke \"#fff\")",
        "(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color \"#f0f\")",
        "(draw/image (image/noise) :at [4 5 0] :scale 2 :angle (* t 0.5))",
        "(draw/image (image/noise))",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

pub fn state_cases() -> Vec<(String, String)> {
    pairs(&[
        ("counter", "(state [s 0.] (+ s t))"),
        ("integer", "(state [s 0] (+ s 1))"),
        ("record", "(state [s {:n 0 :m 1.}] (assoc s :n (+ s.n 1) :m (* s.m 1.5)))"),
        ("record-keys", "(state [s {:n 0 :m 1.}] (let* [{:keys [n m]} s] {:n (+ n 1) :m (* m 2.)}))"),
        ("two-reads", "(let* [a (state [s 0] (+ s 1)) b (state [s 10] (+ s 2))] (+ a b))"),
        ("same-read-twice", "(let* [a (state [s 0] (+ s 1))] (+ a a))"),
        ("loop", "(for [i (range 3)] (state [s 0.] (+ s (* i t))))"),
        ("function", "(let* [f (fn [x] (state [s 0.] (+ s x)))] (+ (f 1.) (f t)))"),
        ("event-records", "(state [n 0] (+ n (reduce (fn [a e] (if (equal? e.key \"a\") (+ a 1) a)) 0 (get (frame/input) :events))))"),
        ("events", "(state [n 0] (+ n (count (get (frame/input) :events))))"),
        ("list", "(state [s (list)] (if (< (count s) 3) (concat s (list t)) s))"),
        ("error", "(state [s 0.] (if (> t 0.6) (/ t 0) (+ s 1.)))"),
        ("coerced", "(state [s 0.] (if (> t 0.6) 2 (+ s t)))"),
    ])
}

pub fn state_refs() -> Vec<(String, String, String)> {
    vec![
        ("ref".into(), "(graph counter :context value (state [s 0.] (+ s t)))".into(), "(let* [a (ref counter)] (+ a (* a 2.)))".into()),
        (
            "ref-record".into(),
            "(graph model :context host (state [m {:n 0 :flag 0}] (assoc m :n (+ m.n 1) :flag (if (> t 0.6) 1 0))))".into(),
            "(+ (get (ref model) :n) (get (ref model) :flag))".into(),
        ),
    ]
}

// ----

pub fn composition_checked(declarations: &str, body: &str) -> w::Workspace {
    let text = format!("(workspace composition (graph g :context host {declarations} {body}))");
    let forms = ok(syntax::parse(&text));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{body}: {}", show_ds(&ds)),
    }
}
pub fn live_body(body: &str) -> String {
    format!("(if (> t -1000.) {body} {body})")
}
pub fn composition_value(body: &str) -> Value {
    let ev = ok(e::static_eval(&composition_checked("", &live_body(body))));
    ev.results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
}
/// `core value`: the residual and its strict Core program.
pub fn composition_core(value: &Value) -> (ResidualId, c::Program) {
    match value {
        Value::Residual(r) => match lower_strict(*r) {
            Ok(p) => (*r, p),
            Err(form) => panic!("Composition Core refusal: {form}"),
        },
        _ => panic!("Composition needs a live residual"),
    }
}
pub fn text_literal(bytes: &[u8]) -> String {
    flow_syntax::lisp::quote_string(bytes)
}
pub fn all_bytes() -> Vec<u8> {
    (0..=255u8).collect()
}
pub fn composition_types() -> Vec<Ty> {
    vec![Ty::Int, Ty::Float, Ty::Vec2, Ty::Vec3, Ty::Vec4, Ty::Text]
}
pub fn element(ty: &Ty) -> String {
    match ty {
        Ty::Int => "(int/add i 9007199254740993)".into(),
        Ty::Float => "(if (int/eq i 0) -0. (float i))".into(),
        Ty::Vec2 => "[(float i) -0.]".into(),
        Ty::Vec3 => "[(float i) -0. t]".into(),
        Ty::Vec4 => "[(float i) -0. t 0.125]".into(),
        Ty::Text => format!("(if (int/eq (int/mod i 2) 0) {} \"é日本😀\")", text_literal(&all_bytes())),
        _ => unreachable!("element type"),
    }
}
pub fn producer(ty: &Ty, count: &str) -> String {
    format!("(map (fn [(i : int)] {}) (array/range {count} :type int))", element(ty))
}
pub fn constant(ty: &Ty, count: &str) -> String {
    match ty {
        Ty::Int => format!("(array/int {count} 9007199254740993)"),
        Ty::Float => format!("(array/float {count} -0.)"),
        Ty::Vec2 => format!("(map (fn [x] [x -0.]) (array/float {count} 1.))"),
        Ty::Vec3 => format!("(array/vec3 {count} [1. -0. 0.125])"),
        Ty::Vec4 => format!("(array/vec4 {count} [1. -0. 0.125 2.])"),
        Ty::Text => format!("(array/text {count} {})", text_literal(&all_bytes())),
        _ => unreachable!("constant type"),
    }
}
pub fn gather(table: &str) -> String {
    format!("(map (fn [(i : int)] (array/nth {table} (int/mod i 13))) (array/range (frame/index) :type int))")
}
pub fn composition_cases() -> Vec<(String, String)> {
    let mut out = vec![];
    for ty in composition_types() {
        let name = flow_syntax::ty::to_string(&ty);
        let bind = |count: &str, body: &str| format!("(let* [table {}] {body})", producer(&ty, count));
        out.push((format!("{name}/concat"), bind("(frame/index)", "(array/concat table table)")));
        out.push((
            format!("{name}/slice"),
            bind("(frame/index)", "(array/slice table (int/div (frame/index) 2) (int/sub (frame/index) (int/div (frame/index) 2)))"),
        ));
        out.push((format!("{name}/hoisted"), bind("13", &format!("(let* [joined (array/concat table table)] {})", gather("joined")))));
        out.push((format!("{name}/inline-concat"), bind("13", &gather("(array/concat table table)"))));
        out.push((format!("{name}/inline-slice"), bind("13", &gather("(array/slice table 0 13)"))));
    }
    out
}
pub fn composition_error_cases() -> Vec<(String, String)> {
    pairs(&[
        ("late-index", "(map (fn [(i : int)] (array/nth (array/concat (array/int 1) (array/int 1)) i)) (array/range (frame/index) :type int))"),
        ("selected-slice", "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) 2 0) i)) (array/range (frame/index) :type int))"),
        ("untaken-slice", "(map (fn [(i : int)] (if (int/lt i 0) (array/nth (array/slice (array/int 1) 2 0) i) i)) (array/range (frame/index) :type int))"),
        (
            "table-cause",
            "(map (fn [(i : int)] (array/nth (array/concat (if (> t 0.) (error \"E_COMPOSE\" \"selected table\" :cause \"raw\\255\\000cause\") (array/int 1)) (array/int 1)) i)) (array/range (frame/index) :type int))",
        ),
        (
            "argument-before-copy",
            "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) (error \"E_FIRST\" \"slice first\" :cause \"first cause\") (error \"E_COUNT\" \"slice count\")) i)) (array/range (frame/index) :type int))",
        ),
        ("empty-product", "(for [i (array/range (frame/index) :type int) j (array/range -1 :type int)] (array/nth (array/slice (array/int 0) 1 0) i))"),
        ("empty-zip", "(map (fn [(i : int) (j : int)] (array/nth (array/slice (array/int 0) 1 0) i)) (array/range (frame/index) :type int) (array/range -1 :type int))"),
        (
            "earliest-row",
            "(map (fn [(i : int)] (if (int/eq i 0) (error \"E_ROW_ZERO\" \"first row\" :cause \"zero cause\") (array/nth (array/slice (array/int 1) 2 0) i))) (array/range (frame/index) :type int))",
        ),
    ])
}

// `captured_helper`: a recorded `tested` function with the helper bound in.
thread_local! {
    static KEEP: std::cell::RefCell<Vec<e::Eval>> = const { std::cell::RefCell::new(Vec::new()) };
}
pub fn captured_helper(body: &str) -> flow_syntax::value::FnId {
    let recorded = |source: &str| ok(e::r#static(&composition_checked("", source), true, None));
    // Retain every evaluation used by fixture function IDs for this thread.
    let callable = |ev: e::Eval| {
        let (_, recs) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
        let f = match &recs[0].1 {
            Value::Fn(f) => *f,
            _ => panic!("expected a function"),
        };
        KEEP.with(|k| k.borrow_mut().push(ev));
        f
    };
    let helper = callable(recorded(&format!("(let* [data (array/int 9 9) tested (fn [(j : int)] {body})] 0.)")));
    let template = callable(recorded(
        "(let* [data (array/int 1 7) tested (fn [(helper : (fn [int] int)) (i : int)] (if (int/lt i 0) (array/nth data 0) (helper (int/mod i 2))))] 0.)",
    ));
    let (params, tbody) = e::residual::function_body(template).unwrap();
    let root_eval = ok(e::static_eval(&composition_checked("", &live_body("t"))));
    let root = residual(&root_eval.results.iter().find(|(n, _)| n == "g").unwrap().1);
    KEEP.with(|k| k.borrow_mut().push(root_eval));
    let view = e::residual::residual_view(root).unwrap();
    let mut bindings = vec![("helper".into(), Value::Fn(helper))];
    bindings.extend(e::residual::function_bindings(template));
    ok(e::residual::compiled_function(root, view.instance, vec![], vec!["g".into(), "tested".into()], params[1..].to_vec(), tbody, bindings))
}
pub fn helper_value(body: &str, count: i64) -> Result<Value, Diagnostic> {
    let signature = flow_syntax::ty::FnSignature { params: vec![Ty::Int], result: Ty::Int };
    e::host::map_function(None, None, &signature, captured_helper(body), &[Value::IntArray(Arc::new((0..count).collect()))])
}
/// `same`: equal results are equal bytes; equal errors have equal code, message and cause.
pub fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) {
    match (a, b) {
        (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
        _ => panic!("Composition/reference result mismatch"),
    }
}

pub fn executor_compile(v: &Value, strict: bool) -> executor::Program {
    ok(executor::compile(v, &executor::CompileOptions { strict, ..Default::default() }))
}
pub fn executor_force(p: &executor::Program, strict: bool, live: &Frame) -> Result<Value, Diagnostic> {
    executor::force(p, &executor::ForceOptions { strict, ..Default::default() }, live)
}
pub fn force_reference(v: &Value, live: &Frame) -> Result<Value, Diagnostic> {
    e::host::force_reference(v, live, None, None, None)
}

// ---- helpers shared by the array tests (irtest1)

pub type ResolveArc = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;

/// `op.body ~live ~node [(key, value)...]`.
pub fn call_op(name: &str, args: Vec<(&str, Value)>) -> Result<Value, Diagnostic> {
    let op = op::find(&[], name, &context::value()).unwrap_or_else(|| panic!("no op {name}"));
    let args: Vec<(Symbol, Value)> = args.into_iter().map(|(k, v)| (k.into(), v)).collect();
    (op.body)(&input::at_time(0.), &|_, _| panic!("node"), &args)
}
pub fn reject(code: &str, r: Result<Value, Diagnostic>) {
    match r {
        Err(d) => assert_eq!(d.code, code, "{}", d.message),
        Ok(v) => panic!("accepted {code}: {v:?}"),
    }
}
/// Compile a residual value's region with options.
pub fn compile_region(r: ResidualId, fusion: bool, count_source: Option<c::region::CountSource>) -> Result<packed::T, Vec<Diagnostic>> {
    let term = e::residual::residual_view(r).unwrap().term;
    packed::compile_result(&packed::CompileOptions { fusion, count_source, ..Default::default() }, r, &term)
}
pub fn ok_ds<T>(r: Result<T, Vec<Diagnostic>>) -> T {
    r.unwrap_or_else(|ds| panic!("{}", show_ds(&ds)))
}
pub fn packed_force(p: &packed::T, live: &Frame, strict: bool, resolve: Option<ResolveArc>) -> Result<Value, Diagnostic> {
    packed::force(p, live, &packed::ForceOptions { strict, resolve, ..Default::default() })
}
pub fn reference_with(v: &Value, live: &Frame, state: Option<&e::State>, resolve: Option<ResolveArc>) -> Result<Value, Diagnostic> {
    e::host::force_reference(v, live, state, None, resolve)
}
pub fn exec_force(p: &executor::Program, live: &Frame, state: Option<&e::State>, resolve: Option<ResolveArc>) -> Result<Value, Diagnostic> {
    executor::force(p, &executor::ForceOptions { strict: true, state: state.cloned(), resolve, ..Default::default() }, live)
}
/// A host-context workspace with declared inputs, evaluated with source inputs.
pub fn dynamic_value(inputs: &[(String, Vec<(Symbol, Value)>)], declarations: &str, body: &str) -> Value {
    let ws = composition_checked(declarations, body);
    ok(e::r#static(&ws, false, Some(inputs))).results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
}
/// A count source naming a `data` binding's origin `["fixture","data"]`.
pub fn data_count_source() -> c::region::CountSource {
    c::region::CountSource::new(Arc::new(|_, term| match &term.node {
        w::Node::RefBinding(id) if term.name(term.arena().ref_binding(*id).name) == "data" && term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() => Some((vec!["fixture".into(), "data".into()], vec![])),
        _ => None,
    }))
}
