use crate::core_exec::{self, Options};
use crate::core_ir as C;
use crate::core_lower;
use crate::flow_ir::{executor, profile, Tier};
use flow_check::catalog::Catalog;
use flow_check::workspace::{self as W, Paths};
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::value::{ResidualId, Value};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

fn ok<X>(r: Result<X, Diagnostic>) -> X { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }

fn residual(inputs: &str, body: &str) -> ResidualId {
    let source = format!("(workspace w (graph g :context value {inputs} (let* [tested {body}] 0.)))");
    let forms = ok(syntax::parse(&source));
    let ws = match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let evaluation = ok(E::static_with(&ws, true, None, None));
    let path: flow_check::workspace::Path = vec!["g".into(), "tested".into()];
    match &evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1 {
        Value::Residual(r) => *r,
        _ => panic!("Expected a residual: {body}"),
    }
}

fn run(program: &C::Program, live: &Frame) -> Result<Value, Diagnostic> {
    core_exec::execute(&core_exec::prepare(program, true, false, &Paths::new(), false), live, &Options::default())
}

fn lower(r: ResidualId) -> Result<C::Program, String> { core_lower::lower(None, None, false, None, None, r) }

fn bits(x: f64, y: f64) -> bool { x.to_bits() == y.to_bits() }

fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) => bits(*a, *b),
        (Value::Vec2(a, b), Value::Vec2(x, y)) => bits(*a, *x) && bits(*b, *y),
        (Value::Vec3(a, b, c), Value::Vec3(x, y, z)) => bits(*a, *x) && bits(*b, *y) && bits(*c, *z),
        (Value::Vec4(a, b, c, d), Value::Vec4(x, y, z, w)) => bits(*a, *x) && bits(*b, *y) && bits(*c, *z) && bits(*d, *w),
        (Value::Record(a), Value::Record(b)) => a.len() == b.len() && a.iter().zip(b).all(|((n, a), (m, b))| n == m && same(a, b)),
        (Value::List(a), Value::List(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| same(a, b)),
        (a, b) => a == b,
    }
}

fn reference(value: &Value, live: &Frame) -> Result<Value, Diagnostic> { E::host::force_reference(value, live, None, None, None) }


fn equal_result(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(a), Ok(b)) => same(a, b),
        (Err(a), Err(b)) => a.code == b.code,
        _ => false,
    }
}

#[test]
fn concrete_values_skip_resolver_and_state() {
    let value = Value::Record(vec![
        ("values".into(), Value::Int_array(Arc::new(vec![1, 0, 1]))),
        ("nested".into(), Value::List(Arc::new(vec![Value::Float(-0.0), Value::Text("raw\0é".as_bytes().into())]))),
    ].into());
    for reference in [false, true] {
        let ticks = Arc::new(AtomicI64::new(0));
        let clock = ticks.clone();
        let profile = profile::create(Arc::new(move || (clock.fetch_add(1, Ordering::SeqCst) + 1) as f64));
        let program = ok(executor::compile(&value, &crate::flow_ir::executor::CompileOptions { strict: true, profile: Some(profile.clone()), ..Default::default() }));
        let state = E::create_state();
        let stamp = E::state_stamp(&state);
        let resolutions = Arc::new(AtomicI64::new(0));
        let counter = resolutions.clone();
        let resolve: core_exec::ResolveFn = Arc::new(move |_| {
            counter.fetch_add(1, Ordering::SeqCst);
            Err(rays_diagnostic::error("E_UNUSED".into(), "unused resolver".into(), None, None, None, Vec::new(), Vec::new()))
        });
        let options = executor::ForceOptions { strict: true, reference, state: Some(state.clone()), resolve: Some(resolve), ..Default::default() };
        let result = ok(executor::force(&program, &options, &input::at_time(f64::NAN)));
        // Cloned record values compare structurally.
        assert!(result == value && E::state_stamp(&state) == stamp && resolutions.load(Ordering::SeqCst) == 0 && ticks.load(Ordering::SeqCst) == 2);
        let executions = profile::executions(&profile);
        assert_eq!(executions.len(), 1, "Concrete execution lost its profile entry");
        assert!(executions[0].seconds == 1. && executions[0].tier == if reference { Tier::Interp } else { Tier::Core });
    }
}

fn cases() -> Vec<(&'static str, ResidualId)> {
    let r = |body: &str| residual("", body);
    vec![
        ("record", r(r##"(if (> t 0.) {:a t :b (+ t 1.)} {:a (- 0. t) :b 2.})"##)),
        ("nested", r(r##"(if (> t 0.) {:v {:x t :y [t 2.]} :label "yes"} {:v {:x (- 0. t) :y [0. t]} :label "no"})"##)),
        ("get", r(r##"(if (> t 0.) (get {:a t :b (* t 2.)} :b) (get {:a (- 0. t) :b 3.} :a))"##)),
        ("dotted", r(r##"(if (> t 0.) (let* [r {:inner {:v [t 2. 3. 4.]}}] r.inner.v.w) t)"##)),
        ("keys", r(r##"(if (> t 0.) (let* [{:keys [a b]} {:a t :b (* t 2.)}] (+ a b)) t)"##)),
        ("assoc-order", r(r##"(if (> t 0.) (assoc {:a t :b 2} :a 1.9 :c t :b (+ t 0.7)) {:a t :b 2 :c t})"##)),
        (
            "assoc-nested",
            r(r##"(if (> t 0.) (assoc {:r {:i 0 :v [1. 2.]} :text "old"} :r {:i t :v t :extra true} :text (str t)) {:r {:i 0 :v [0. 0.] :extra false} :text "none"})"##),
        ),
        ("assoc-tag", r(r##"(let* [r (if (> t 0.) {:n 1} {:n 1.})] (assoc r :n 1.7))"##)),
        ("assoc-captured", r(r##"(let* [r {:n (int t)}] (assoc r :n 1.7))"##)),
        ("assoc-captured-branch", r(r##"(let* [r {:n (int t)} chosen (if (> t 0.) r r)] (assoc chosen :n 1.7))"##)),
        ("assoc-captured-nested", r(r##"(let* [r {:v {:n (int t)}}] (assoc r :v {:n 1.7}))"##)),
        (
            "assoc-captured-lazy",
            r(r##"(let* [r {:v {:n (int t)} :unused (exp (* t 100000.))} updated (assoc r :v {:n 1.7})] updated.v.n)"##),
        ),
        ("branch-order", r(r##"(if (> t 0.) {:a t :b 2.} {:b 2. :a t})"##)),
        ("branch-order-show", r(r##"(str (if (> t 0.) {:a t :b 2.} {:b 2. :a t}))"##)),
        ("branch-order-assoc", r(r##"(let* [r (if (> t 0.) {:a 1 :b 2.} {:b 2. :a 1})] (assoc r :a t :c 3))"##)),
        (
            "branch-order-nested",
            r(r##"(if (> t -10.) (let* [r {:v {:a 0 :b 2.}} nv (if (> t 0.) {:a t :b 3} {:b 3 :a t})] (assoc r :v nv)) {:v {:a 0 :b 2.}})"##),
        ),
        ("branch-order-get", r(r##"(let* [r (if (> t 0.) {:a t :b 2.} {:b 2. :a t})] (get r :a))"##)),
        ("text", r(r##"(if (> t 0.) "a\n\"\\é🙂" "b")"##)),
        ("str", r(r##"(if (> t 0.) (str "t=" t "/" (int t) "/" (> t 1.) "/" [t -0.]) (str "negative=" t))"##)),
        (
            "str-record",
            r(r##"(if (> t 0.) (str {:a t :nested {:b [t 2.] :c "text"}}) (str {:a (- 0. t) :nested {:b [0. t] :c "text"}}))"##),
        ),
        (
            "str-list",
            residual("[(empty : (list float) (list))]", r##"(if (> t 0.) (str (for [i (range 3)] (+ t i))) (str empty))"##),
        ),
        ("typed-branch", r(r##"(if (> t -10.) (let* [f (fn [(x : float)] (equal? x 1.))] (f (if (> t 0.) 1 1.))) false)"##)),
        (
            "equal-record",
            r(r##"(if (> t 0.) (equal? {:a t :b {:text "yes"}} {:a t :b {:text "yes"}}) (equal? {:a t :b {:text "yes"}} {:b {:text "yes"} :a t}))"##),
        ),
        ("equal-captures", r(r##"(let* [a {:v t} b {:v t}] (equal? a b))"##)),
        ("equal-capture-fresh", r(r##"(let* [a {:v {:x t}}] (if (> t 0.) (equal? a {:v {:x t}}) false))"##)),
        ("assoc-error", r(r##"(if (> t 0.) (assoc {:n 0} :n 10000000000000000000.) {:n 0})"##)),
        ("equal-tags", r(r##"(equal? (if (> t 0.) 1 1.) 1)"##)),
        ("equal-text", r(r##"(equal? (if (> t 0.) "same" "other") "same")"##)),
        ("captured", r(r##"(let* [r {:safe t :bad (exp (* t 100000.))}] (if (> t 0.) (get r :safe) t))"##)),
        ("captured-str", r(r##"(let* [r {:n t :v [t 2.]}] (if (> t 0.) (str r) "none"))"##)),
    ]
}

fn dump() -> (String, Vec<String>) {
    let mut out = String::new();
    let mut failures = Vec::new();
    for (name, r) in cases() {
        let core = lower(r).unwrap_or_else(|n| panic!("{name}: Core refused {n}"));
        let program = ok(executor::compile(&Value::Residual(r), &Default::default()));
        let printed = C::pp(&core);
        for time in [-3., -2.5, -0., 0., 0.00005, 0.00015, 0.37, 1., 1.9, 100.25] {
            let live = input::at_time(time);
            let value = Value::Residual(r);
            let expected = reference(&value, &live);
            // Every comparison is collected so one run lists all differing cases.
            if !equal_result(&E::force(&value, &live, None), &expected) {
                failures.push(format!("{name} at {time}: E::force differs from the reference"));
            }
            let actual = executor::force(&program, &Default::default(), &live);
            if !equal_result(&actual, &expected) {
                failures.push(format!("{name} at {time}: executor {actual:?} <> reference {expected:?}"));
            }
            match (run(&core, &live), &expected) {
                (Ok(actual), Ok(expected)) => {
                    if !same(&actual, expected) {
                        failures.push(format!("{name} at {time:.17}: core {actual:?} <> reference {expected:?}"));
                    }
                }
                (Err(a), Err(b)) => {
                    if a.code != b.code { failures.push(format!("{name} at {time}: core error {} <> {}", a.code, b.code)); }
                }
                (core, reference) => failures.push(format!("{name} at {time}: result mismatch core {core:?} reference {reference:?}")),
            }
        }
        out.push_str(name);
        out.push('\n');
        out.push_str(&printed);
    }
    (out, failures)
}

#[test]
fn core_record_and_text_print_is_stable() {
    let (actual, _) = dump();
    let expected = include_str!("../../../crates/flow_ir/core_records.expected").replace("\r\n", "\n");
    if actual != expected {
        let first = actual.lines().zip(expected.lines()).position(|(a, b)| a != b).unwrap_or(0);
        panic!("Core record/text print changed at line {}:\nactual:   {:?}\nexpected: {:?}", first + 1, actual.lines().nth(first), expected.lines().nth(first));
    }
}

// Known failure: the reference walker leaves captured record fields residual while Core and executor force them; captured-str consequently prints a question mark.
#[test]
fn core_and_executor_match_the_reference_walker_on_every_case() {
    let (_, failures) = dump();
    assert!(failures.is_empty(), "{} mismatches:\n{}", failures.len(), failures.join("\n"));
}

#[test]
fn lazy_record_accumulator_errors_keep_their_fields() {
    for body in [
        r##"(fold [state {:n 0}] [i (range 3)]
          (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause")
            (assoc state :n (int/add state.n 1))))"##,
        r##"(fold [state {:n 0}] [i (range 3)]
          (assoc (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause") state)
            :n (int/add state.n 1)))"##,
    ] {
        let r = residual("", body);
        let core = lower(r).unwrap_or_else(|n| panic!("{n}"));
        let program = ok(executor::compile(&Value::Residual(r), &Default::default()));
        for time in [0., 1.] {
            let live = input::at_time(time);
            let expected = reference(&Value::Residual(r), &live);
            for actual in [run(&core, &live), executor::force(&program, &Default::default(), &live)] {
                match (&actual, &expected) {
                    (Ok(a), Ok(b)) => assert!(same(a, b)),
                    (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
                    _ => panic!("Lazy record accumulator error changed"),
                }
            }
        }
    }
}
