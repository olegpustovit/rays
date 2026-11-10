// Static packed maps cross the 4096-element walked head and compare result bits, records and failures against reference evaluation. Profile tiers identify actual kernel execution.
use flow_syntax::symbol::Symbol;
use crate::core_exec;
use crate::core_ir::{Expr, StatementNode};
use crate::core_lower;
use crate::flow_ir::{self, executor, profile, Tier};
use crate::packed;
use crate::test_support::*;
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::syntax;
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

fn checked(body: &str) -> w::Workspace {
    let forms = ok(syntax::parse(&format!("(workspace w (graph g :context host {body}))")));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{body}: {}", show_ds(&ds)),
    }
}
fn check_source(source: &str) -> w::Workspace {
    let forms = ok(syntax::parse(source));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}

fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Floats(1..=4, _), _) => {
            flow_syntax::value::ty_of(a) == flow_syntax::value::ty_of(b) && bits(a) == bits(b)
        }
        (Value::Record(x), Value::Record(y)) => x.len() == y.len() && x.iter().zip(y).all(|((n, v), (m, w))| n == m && same(v, w)),
        (Value::List(x), Value::List(y)) => x.len() == y.len() && x.iter().zip(y.iter()).all(|(p, q)| same(p, q)),
        (Value::Residual(_), Value::Residual(_)) => true,
        _ => bits(a) == bits(b),
    }
}
fn result_of(ev: &e::Eval, name: &str) -> Value {
    ev.results.iter().find(|(n, _)| n == name).unwrap().1.clone()
}
fn eq_err(a: &Diagnostic, b: &Diagnostic) -> bool {
    format!("{a:?}") == format!("{b:?}")
}
fn record_options() -> flow_ir::StaticOptions {
    flow_ir::StaticOptions { record: true, ..Default::default() }
}
fn fast_static(ws: &w::Workspace) -> Result<e::Eval, Diagnostic> {
    flow_ir::static_(&Default::default(), ws)
}
fn clock_zero() -> Arc<dyn Fn() -> f64 + Send + Sync> {
    Arc::new(|| 0.)
}

fn maps() -> Vec<&'static str> {
    vec![
        // the hash of sketches/million_marks, without its drift
        "(let* [indices (array/range 4200) extent 300.0]\n     (map (fn [i] (let* [h (fn [s] (abs (mod (* (sin (+ (* i 12.9898) (* s 78.233))) 43758.5453) 1.0)))\n                         x (- (+ (h 1.0) (h 2.0)) 1.0) y (- (h 3.0) 0.5)]\n                    [(* x extent) (* y extent) 0])) indices))",
        "(map (fn [i] (* (sqrt i) 0.5)) (array/range 4097))",
        "(let* [a (array/range 4500) b (map (fn [i] (+ i 0.5)) a)] (map (fn [x y] [(- y x) (* x y)]) a b))",
        "(let* [a (array/range 5000)] {:first (map (fn [i] (if (> (mod i 3) 1) (- 0 i) (/ i 7))) a)\n                                 :second (map (fn [i] [i (cos i) (pow i 0.5) 1]) a)})",
        // 4,096 elements and fewer are the walker's alone
        "(map (fn [i] (* i 3)) (array/range 4096))",
        // forms the kernel refuses are walked to the end
        "(map (fn [i] (value/rand i 7)) (array/range 4200))",
        // a map that reads the frame stays a residual
        "(map (fn [i] (+ i t)) (array/range 5000))",
    ]
}

fn check_map(body: &str) {
    let ws = checked(body);
    let walked = ok(e::r#static(&ws, true, None));
    let fast = ok(flow_ir::static_(&record_options(), &ws));
    assert!(same(&result_of(&walked, "g"), &result_of(&fast, "g")), "Flow_ir.static differs from the walker for {body}");
    // The same paths are recorded. A path every element reaches has the same 4,096 records; one
    // inside a branch has those of the first 4,096 elements, where the walker alone would go on to
    // later elements until that path had 4,096.
    let counts = |r: &e::Eval| r.records.iter().map(|(p, v)| (p.clone(), v.len())).collect::<Vec<_>>();
    let (cw, cf) = (counts(&walked), counts(&fast));
    assert!(cw.iter().map(|x| &x.0).eq(cf.iter().map(|x| &x.0)) && cw.iter().zip(&cf).all(|(w, f)| f.1 <= w.1), "records differ for {body}");
    let branches = body.contains("(if ");
    if !branches {
        assert_eq!(cw, cf, "records of a map without a branch differ for {body}");
    }
    let qualified = ok(flow_ir::qualify_workspace(&flow_ir::QualifyOptions { record: true, ..Default::default() }, &ws)).1;
    assert!(same(&result_of(&walked, "g"), &result_of(&qualified, "g")), "qualify_workspace differs from the walker for {body}");
}
macro_rules! map_tests {
    ($($name:ident: $i:expr),*) => { $( #[test] fn $name() { check_map(maps()[$i]); } )* };
}
map_tests!(map_sqrt_4097: 1, map_zipped_4500: 2, map_record_fields: 3, map_walker_alone_4096: 4, map_kernel_refused_walks: 5, map_frame_reader_stays_residual: 6);

/// A failure, before or after the walked elements, is the walker's diagnostic.
#[test]
fn failures_are_the_walkers() {
    for body in ["(map (fn [i] (exp (- i 4300))) (array/range 5200))", "(map (fn [i] (exp (+ i 700))) (array/range 5200))"] {
        let ws = checked(body);
        match (e::static_eval(&ws), fast_static(&ws)) {
            (Err(a), Err(b)) => assert!(eq_err(&a, &b), "another diagnostic for {body}"),
            _ => panic!("expected a failure for {body}"),
        }
    }
}

/// The profile must report Cpu_kernel with no walked tail beyond the head.
#[test]
fn kernel_makes_the_elements_past_the_head() {
    let ws = checked("(map (fn [i] [(sin i) (cos i) 0]) (array/range 20000))");
    let profile = profile::create(clock_zero());
    let fast = ok(profile::sample(&profile, &mut || fast_static(&ws)));
    match result_of(&fast, "g") {
        Value::Floats(3, d) => assert!(d.len() == 60000 && d[3 * 19999] == rays_math::libm::sin(19999.0)),
        _ => panic!("a vec3 array"),
    }
    let maps = profile::static_maps(&profile);
    assert!(maps.iter().any(|m| m.count == 20000 && m.tier == Tier::CpuKernel && m.tail_elements == 0), "walked: {:?}", maps.last().map(|m| (m.tier, m.head_elements, m.tail_elements)));
}

#[test]
fn static_map_profile_head_tail_and_refusal() {
    let ticks = Arc::new(AtomicU64::new(0));
    let profile = {
        let ticks = ticks.clone();
        profile::create(Arc::new(move || (ticks.fetch_add(1, Ordering::SeqCst) + 1) as f64))
    };
    let workspace = checked("(map (fn [i] (* i 0.5)) (array/range 4100))");
    let observed = ok(profile::sample(&profile, &mut || fast_static(&workspace)));
    assert!(same(&result_of(&observed, "g"), &result_of(&ok(e::static_eval(&workspace)), "g")));
    let maps = profile::static_maps(&profile);
    assert_eq!(maps.len(), 1, "static map profile missing");
    let map = &maps[0];
    assert!(map.site == vec!["g".to_string(), "@result".to_string()] && map.count == 4100);
    assert!(map.tier == Tier::CpuKernel && map.refusal.is_empty());
    assert!(map.tail_elements == 0); // head_elements == 0 is `kernel_first_head_is_zero`
    assert_eq!(map.seconds, map.head_seconds + map.kernel_seconds + map.tail_seconds);

    let refused = checked("(map (fn [i] (value/rand i 7)) (array/range 4100))");
    ok(profile::sample(&profile, &mut || fast_static(&refused)));
    let map = profile::static_maps(&profile).into_iter().last().unwrap();
    assert!(map.tier == Tier::Interp && !map.refusal.is_empty());
    assert!(map.head_elements == 4096 && map.tail_elements == 4);

    let failing = checked("(map (fn [i] (exp (- i 4300))) (array/range 5200))");
    match (profile::sample(&profile, &mut || fast_static(&failing)), e::static_eval(&failing)) {
        (Err(a), Err(b)) => assert!(eq_err(&a, &b)),
        _ => panic!("expected the walker's failure"),
    }
    let map = profile::static_maps(&profile).into_iter().last().unwrap();
    assert!(map.tier == Tier::Interp && map.tail_elements > 0);
    assert!(map.refusal.iter().any(|d| d.code == "E_NONFINITE"));

    // A failed scope must restore its predecessor; no platform clock or waiting in this check.
    let scope = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| profile::sample(&profile, &mut || -> () { panic!("scope") })));
    assert!(scope.is_err());
    let small = checked("(map (fn [i] (* i 0.5)) (array/range 2))");
    for _ in 0..513 {
        ok(profile::sample(&profile, &mut || fast_static(&small)));
    }
    assert_eq!(profile::static_maps(&profile).len(), 512);
    ok(fast_static(&workspace));
    assert_eq!(profile::static_maps(&profile).len(), 512);
}

#[test]
fn profiles_are_scoped_per_domain() {
    let small = checked("(map (fn [i] (* i 0.5)) (array/range 2))");
    let concurrent = profile::create(clock_zero());
    let run = |domains: i64| -> Vec<Value> {
        let out = Mutex::new(Vec::<Value>::new());
        with_domains(domains, &|| {
            let outcome = Mutex::new(Vec::new());
            parallel::for_(1, 0, 7, &|_| {
                let ev = ok(profile::sample(&concurrent, &mut || fast_static(&small)));
                outcome.lock().unwrap().push(result_of(&ev, "g"));
                Ok(())
            })
            .unwrap();
            out.lock().unwrap().extend(outcome.into_inner().unwrap());
        });
        out.into_inner().unwrap()
    };
    let sequential = run(1);
    let parallel = run(8);
    assert!(sequential.iter().zip(parallel.iter()).all(|(a, b)| same(a, b)));
    assert_eq!(profile::static_maps(&concurrent).len(), 16);
}

fn gather_workspace(table: &str, bindings: &str, body: &str) -> w::Workspace {
    check_source(&format!(
        "(workspace gathers (graph source :context host {{:values {table}}}) (graph g :context host (let* [data (ref source) {bindings}] (map (fn [(i : int)] {body}) (array/range 4101 :type int)))))"
    ))
}
fn gather_parity(workspace: &w::Workspace) {
    let profile = profile::create(clock_zero());
    match (e::static_eval(workspace), profile::sample(&profile, &mut || fast_static(workspace))) {
        (Ok(reference), Ok(compiled)) => {
            assert!(same(&result_of(&reference, "g"), &result_of(&compiled, "g")));
            let map = profile::static_maps(&profile).into_iter().find(|m| m.count == 4101).expect("4101 map");
            assert!(map.tier == Tier::CpuKernel && map.tail_elements == 0, "gather walked: {}", show_ds(&map.refusal));
        }
        (Err(reference), Err(compiled)) => {
            assert!(eq_err(&reference, &compiled) && reference.code == "E_ARRAY_RANGE");
            let map = profile::static_maps(&profile).into_iter().last().unwrap();
            assert!(map.head_elements == 4096 && map.tail_elements > 0);
        }
        _ => panic!("gather result differs from walker"),
    }
}

#[test]
fn static_gathers_match_the_walker() {
    for (bindings, body) in [
        ("", "(if (= (array/nth data.values (int/mod i 3)) 1) i 0)"),
        ("lookup (fn [table index] (array/nth table index))", "(if (= (lookup data.values (int/mod i 3)) 1) i 0)"),
        ("lookup (fn [(table : (array int)) (index : int)] (array/nth table index))", "(if (= (lookup data.values (int/mod i 3)) 1) i 0)"),
        ("", "(if (< i 0) (array/nth data.values 100) (array/nth data.values (int/mod i 3)))"),
        ("", "(array/nth data.values (if (= i 4099) 3 (int/mod i 3)))"),
    ] {
        gather_parity(&gather_workspace("(array/int 3 1)", bindings, body));
    }
}

#[test]
fn text_gather_residual() {
    let text_workspace = gather_workspace("(array/text 3 \"red\")", "lookup (fn [(i : int)] (array/nth data.values (int/mod i 3)))", "i");
    let text_evaluated = ok(e::r#static(&text_workspace, true, None));
    let (_, records) = text_evaluated.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "lookup".to_string()]).expect("lookup record");
    assert_eq!(records.len(), 1, "missing Text lookup function");
    let f = match &records[0].1 {
        Value::Fn(f) => *f,
        _ => panic!("missing Text lookup function"),
    };
    let value = ok(e::host::map_function(None, None, &FnSignature { params: vec![Ty::Int], result: Ty::Text }, f, &[Value::IntArray(Arc::new((0..4101).collect()))]));
    let r = residual(&value);
    let p = compile_region(r, true, None).unwrap_or_else(|ds| panic!("{}", show_ds(&ds[..1])));
    assert_eq!(bits(&ok(packed_force(&p, &live_at(0.), true, None))), bits(&ok(force_reference(&value, &live_at(0.)))));
}

/// An inconsistent concrete annotation is a compiler refusal at the gather, not a checker change.
/// Corrupt one checked type to exercise a boundary ordinary source cannot manufacture.
#[test]
fn inconsistent_gather_type_is_refused() {
    let checked_ws = gather_workspace("(array/int 3 1)", "", "(if (= (array/nth data.values (int/mod i 3)) 1) i 0)");
    let seen: Arc<Mutex<Option<flow_syntax::value::ResidualId>>> = Arc::new(Mutex::new(None));
    let handlers = e::ResidualHandlers {
        bulk: Some({
            let seen = seen.clone();
            Arc::new(move |r| {
                *seen.lock().unwrap() = Some(r);
                None
            })
        }),
        ..Default::default()
    };
    ok(e::residual::handle(handlers, &mut || e::host::static_with_requests(None, false, None, &checked_ws)));
    let residual_id = seen.lock().unwrap().expect("a bulk request");
    let view = e::residual::residual_view(residual_id).unwrap();
    let mut builder = flow_check::term_arena::TermBuilder::new();
    let term = builder.rewrite_roots_with(std::slice::from_ref(view.term.as_ref()), &mut |builder, source, draft| {
        let id = builder.rebuild(source, draft);
        match source.node {
            w::Node::Op(op) if source.name(source.arena().op(op).op) == "array/nth" => builder.with_type(id, &Ty::Float),
            _ => id,
        }
    })[0];
    let term = builder.freeze().handle(term);
    let rid = ok(e::residual::with_bindings(residual_id, &term, &view.owned_bindings()));
    match compile_region_term(rid, &term) {
        Err(ds) if ds.len() == 1 => {
            let diagnostic = &ds[0];
            assert!(diagnostic.code == "E_PACKED_TYPE" && diagnostic.span.is_some(), "{}", show_ds(&ds));
            let words: Vec<&str> = diagnostic.message.split(' ').collect();
            for word in ["(array/nth", "float,", "int."] {
                assert!(words.contains(&word), "{}", diagnostic.message);
            }
        }
        _ => panic!("inconsistent gather type was not refused"),
    }
}
fn compile_region_term(r: flow_syntax::value::ResidualId, term: &w::Term) -> Result<packed::T, Vec<Diagnostic>> {
    packed::compile_result(&Default::default(), r, term)
}

#[test]
fn static_list_guards() {
    for index in [0, 4100] {
        let source = format!("(workspace w (graph g :context host (let* [table (list 1 2 3)] (map (fn [(i : int)] (nth table (if (= i {index}) 0.5 0))) (array/range 4101 :type int)))))");
        let forms = ok(syntax::parse(&source));
        let (workspace, diagnostics) = w::check(&[], false, false, None, &catalog(), &forms);
        assert!(workspace.is_none() && diagnostics.iter().any(|d| d.code == "E_TYPE" && d.message.contains("nth index")), "{}", show_ds(&diagnostics));
        for bad in ["-1", "3"] {
            let source = format!("(let* [table (list 1 2 3)] (map (fn [(i : int)] (nth table (if (= i {index}) {bad} 0))) (array/range 4101 :type int)))");
            let workspace = checked(&source);
            match (e::static_eval(&workspace), fast_static(&workspace)) {
                (Err(a), Err(b)) => assert!(eq_err(&a, &b) && a.code == "E_LIST_RANGE"),
                _ => panic!("list range failure differs from walker"),
            }
        }
    }
    let workspace = checked("(let* [table (list)] (map (fn [(i : int)] (nth table i)) (array/range 4101 :type int)))");
    match (e::static_eval(&workspace), fast_static(&workspace)) {
        (Err(a), Err(b)) => assert!(eq_err(&a, &b) && a.code == "E_LIST_RANGE"),
        _ => panic!("empty list error differs"),
    }
}

/// Trusted static maps: full bit verification and replayed bodies.
#[test]
fn trusted_replay_verifies_bits() {
    let workspace = checked("(let* [f (fn [(i : int)] (let* [y (+ i 7)] (* y 2))) a (map f (array/range 4101 :type int)) b (map f (array/range 4101 :type int))] {:a a :b b :last (f 7)})");
    let eager = ok(flow_ir::static_(&flow_ir::StaticOptions { record: true, verify: true, ..Default::default() }, &workspace));
    let profile = profile::create(clock_zero());
    let replay = ok(profile::sample(&profile, &mut || flow_ir::static_(&flow_ir::StaticOptions { record: true, replay: true, verify: true, ..Default::default() }, &workspace)));
    assert!(same(&result_of(&eager, "g"), &result_of(&replay, "g")));
    for map in profile::static_maps(&profile) {
        assert!(map.tier == Tier::CpuKernel && map.tail_elements == 0);
    }
    let mut eager_probe = flow_graph::probe::make(&eager, Default::default());
    let mut replay_probe = flow_graph::probe::make(&replay, Default::default());
    let path = eager.records.iter().find(|(path, _)| path.last().is_some_and(|s| s == "y")).expect("y records").0.clone();
    for k in [0, 4095, 4096, 4100, 4101, 8196, 8201] {
        let a = eager_probe.at(&path, &[k]);
        let b = replay_probe.at(&path, &[k]);
        assert!(a == b && b.is_some(), "replay probe differs or is absent at call {k} ({})", flow_syntax::symbol::join_path(&path, "/"));
    }
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let compiled = ok(flow_ir::static_(&flow_ir::StaticOptions { replay: true, verify: true, ..Default::default() }, &workspace));
            assert!(same(&result_of(&eager, "g"), &result_of(&compiled, "g")));
        });
    }
}

#[test]
fn replay_failures_and_named_function_recording() {
    for body in [
        "(map (fn [(i : int)] (exp (if (= i 4100) 1000. 0.))) (array/range 4101 :type int))",
        "(let* [table (list 1 2 3)] (map (fn [(i : int)] (nth table (if (= i 4100) 3 0))) (array/range 4101 :type int)))",
    ] {
        let workspace = checked(body);
        let expected = e::r#static(&workspace, true, None);
        assert!(expected.is_err(), "expected replay failure for {body}");
        let actual = flow_ir::static_(&flow_ir::StaticOptions { record: true, replay: true, ..Default::default() }, &workspace);
        match (actual, expected) {
            (Err(a), Err(b)) => assert!(eq_err(&a, &b)),
            _ => panic!("replay should fail like the walker"),
        }
    }
    let workspace = check_source("(workspace w (defn f :context host [(i : int)] (+ i 1)) (graph g :context host (map f (array/range 4101 :type int))))");
    let profile = profile::create(clock_zero());
    let replay = ok(profile::sample(&profile, &mut || flow_ir::static_(&flow_ir::StaticOptions { record: true, replay: true, verify: true, ..Default::default() }, &workspace)));
    let eager = ok(flow_ir::static_(&record_options(), &workspace));
    assert!(same(&result_of(&eager, "g"), &result_of(&replay, "g")));
    assert_eq!(eager.records.len(), replay.records.len());
    for ((path, values), (other, replayed)) in eager.records.iter().zip(replay.records.iter()) {
        assert!(path == other && values.len() == replayed.len(), "named recording differs: {path:?} ({}), {other:?} ({})", values.len(), replayed.len());
        for ((i, a), (j, b)) in values.iter().zip(replayed) {
            assert!(i == j && same(a, b), "named record differs at {path:?}");
        }
    }
    for map in profile::static_maps(&profile) {
        assert_eq!(map.head_elements, 4096);
    }
}

#[test]
fn strict_captured_iterate() {
    let workspace = checked("(let* [indices (array/range 7 :type int) seed (map (fn [(i : int)] (int/mod (int/add i (int t)) 7)) indices) jumped (exact (iterate 4 seed (fn [(p : (array int))] (exact (map (fn [(x : int)] (array/nth p x)) p))) :until (fn [(a : (array int)) (b : (array int))] (int/eq (array/sum a) (array/sum b)))))] (map (fn [(i : int)] (array/nth jumped i)) indices))");
    let value = result_of(&ok(e::static_eval(&workspace)), "g");
    let compiled = executor_compile(&value, true);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            for time in [0., 1., 2.] {
                let live = live_at(time);
                assert!(same(&ok(e::force(&value, &live, None)), &ok(executor_force(&compiled, true, &live))));
            }
        });
    }
}

// Known failure: static_map_profile, trusted_replay and clock checks observe a walked 4096-element head before the kernel instead of a complete eager kernel run.
#[test]
fn kernel_first_profile_clock() {
    let ticks = Arc::new(AtomicU64::new(0));
    let profile = {
        let ticks = ticks.clone();
        profile::create(Arc::new(move || if ticks.fetch_add(1, Ordering::SeqCst) + 1 < 3 { 0. } else { 1. }))
    };
    let workspace = checked("(map (fn [i] (* i 0.5)) (array/range 4100))");
    ok(profile::sample(&profile, &mut || fast_static(&workspace)));
    let maps = profile::static_maps(&profile);
    assert_eq!(maps.len(), 1, "kernel-first profile missing");
    let map = &maps[0];
    assert!(map.head_elements == 0 && map.tail_elements == 0);
    assert!(map.head_seconds == 0. && map.kernel_seconds == 1. && map.tail_seconds == 0.);
    assert_eq!(map.seconds, 1.);
}

fn staged() -> w::Workspace {
    check_source(
        r#"(workspace staged
        (defn build :context value [(n : int)]
          (map (fn [(i : int)] (+ i 1)) (array/range n :type int)))
        (graph volume :context host (build 4101))
        (graph sculpture :context sop
          (let* [mask (map (fn [(i : int)] (if (< i 2000) 1 0)) (array/range 4101 :type int))
                 faces (array/compact mask)
                 count (array/count faces)
                 vertices (array/range (* count 4) :type int)
                 points (map (fn [(i : int)]
                            [(float (array/nth faces (int/div i 4)))
                             (float (int/mod i 4)) 0.]) vertices)
                 tiny (array/slice faces 0 2)
                 pairs (for [a tiny b tiny] [a b 0])
                 copied (map (fn [(i : int)] [i 0 0]) tiny)
                 dropped (array/compact (map (fn [(bit : int)] (- 1 bit)) mask))
                 face_copy (map (fn [(i : int)] [i 0 0]) faces)
                 drop_copy (map (fn [(i : int)] [i 0 0]) dropped)
                 small (build 3)] nil)))"#,
    )
}
fn recorded(ev: &e::Eval, name: &str) -> Value {
    let (_, vs) = ev.records.iter().find(|(p, _)| p == &vec!["sculpture".to_string(), name.to_string()]).unwrap_or_else(|| panic!("no record {name}"));
    vs[0].1.clone()
}
fn qualify(ws: &w::Workspace, defer: bool) -> e::Eval {
    ok(flow_ir::qualify_workspace(&flow_ir::QualifyOptions { record: true, replay: true, defer, ..Default::default() }, ws)).1
}

/// SOP deferral: host/small maps eager, compiled arrays/counts exact, late diagnostics preserved.
#[test]
fn sop_deferral() {
    let checked_ws = staged();
    let eager = qualify(&checked_ws, false);
    let profile = profile::create(clock_zero());
    let deferred = ok(profile::sample(&profile, &mut || flow_ir::qualify_workspace(&flow_ir::QualifyOptions { defer: true, record: true, replay: true, ..Default::default() }, &checked_ws))).1;
    assert!(same(&result_of(&eager, "volume"), &result_of(&deferred, "volume")));
    for name in ["mask", "faces", "count", "vertices", "points"] {
        let value = recorded(&deferred, name);
        assert!(matches!(value, Value::Residual(_)), "not deferred: {name}");
        let program = executor_compile(&value, true);
        for domains in [1, 8] {
            with_domains(domains, &|| {
                assert!(same(&recorded(&eager, name), &ok(executor_force(&program, true, &live_at(0.)))), "{name}");
            });
        }
    }
    let points = recorded(&deferred, "points");
    let program = executor_compile(&points, true);
    assert!(executor::packed_root(&program).is_some());
    let root = residual(&points);
    let core = core_lower::lower(None, None, false, None, None, root).unwrap_or_else(|f| panic!("{f}"));
    assert!(core.regions.len() >= 2);
    crate::core_ir::iter(
        &mut |s| {
            if let StatementNode::Assign(_, Expr::Source(value)) = &s.node {
                assert!(!e::is_live(value));
            }
        },
        &core.body,
    );
    let production = ok(executor::compile(&points, &Default::default()));
    let prepared = prepare(&core);
    let names = ["mask", "faces", "count", "vertices", "points"];
    let record = |ev: &e::Eval| Value::Record(names.iter().map(|n| ((*n).into(), recorded(ev, n))).collect::<Vec<_>>().into());
    let shared = record(&deferred);
    let shared_core = core_lower::lower(Some(shared.clone()), None, true, None, None, root).unwrap_or_else(|f| panic!("{f}"));
    assert_eq!(shared_core.regions.len(), 2);
    let fields: Vec<(Symbol, Value)> = ["copied", "pairs", "face_copy", "drop_copy"].iter().map(|n| ((*n).into(), recorded(&deferred, n))).collect();
    let origin_core = core_lower::lower(Some(Value::Record(fields.into())), None, true, None, None, root).unwrap_or_else(|f| panic!("{f}"));
    let origins = |name: &str| {
        origin_core
            .regions
            .iter()
            .find(|r| r.term.path().is_some_and(|ids| ids.iter().map(|&id| r.term.name(id)).eq(["sculpture", name])))
            .unwrap_or_else(|| panic!("no region {name}"))
            .origin
            .clone()
    };
    assert!(origins("copied").is_some() && origins("pairs").is_none());
    assert!(origins("face_copy").is_some() && origins("drop_copy").is_some());
    assert!(origins("face_copy") != origins("drop_copy"));
    let shared_profile = profile::create(clock_zero());
    let shared_program = ok(executor::compile(&shared, &executor::CompileOptions { profile: Some(shared_profile.clone()), strict: true, ..Default::default() }));
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let live = live_at(0.);
            let strict = core_exec::Options { strict: true, ..Default::default() };
            assert!(same(&recorded(&eager, "points"), &ok(core_exec::execute(&prepared, &live, &strict))));
            assert!(same(&recorded(&eager, "points"), &ok(executor::force(&production, &Default::default(), &live))));
            let runs = Arc::new(Mutex::new(0));
            let force: core_exec::ForceFn = {
                let runs = runs.clone();
                let live = live.clone();
                Arc::new(move |p: &core_exec::Packed| {
                    *runs.lock().unwrap() += 1;
                    packed::force(p, &live, &packed::ForceOptions { strict: true, ..Default::default() })
                })
            };
            let actual = ok(core_exec::execute(&prepare(&shared_core), &live, &core_exec::Options { force: Some(force), ..Default::default() }));
            assert!(*runs.lock().unwrap() == 2 && same(&record(&eager), &actual));
            assert!(same(&record(&eager), &ok(executor_force(&shared_program, true, &live))));
            let cpu = profile::executions(&shared_profile).iter().filter(|x| x.tier == Tier::CpuKernel).count();
            assert_eq!(cpu, 2);
            let outputs = Arc::new(Mutex::new(vec![(0..3).map(|_| Some(vec![f64::NAN; 8000])).collect::<Vec<_>>()]));
            let column_source: packed::ColumnSource = Arc::new(|_, _| Ok(None));
            let done = ok(executor::force_into(&executor::ForceOptions { strict: true, ..Default::default() }, None, column_source, 8000, outputs.clone(), &production, &live));
            assert_eq!(done, Some(()));
            let planes = outputs.lock().unwrap();
            let interleaved = Value::Floats(3, Arc::new((0..24000).map(|i| planes[0][i % 3].as_ref().unwrap()[i / 3]).collect()));
            assert!(same(&recorded(&eager, "points"), &interleaved));
        });
    }
    assert!(same(&recorded(&eager, "small"), &recorded(&deferred, "small")));
    let maps = profile::static_maps(&profile);
    // Large SOP arrays defer at construction, so they are never walked eagerly; only the small map is eager.
    assert!(maps.iter().filter(|m| m.site.first().is_some_and(|s| s == "sculpture")).all(|m| m.count <= 4096 && !m.deferred && m.tail_elements == 0));
    assert!(maps.iter().any(|m| m.site.first().is_some_and(|s| s == "sculpture") && m.count == 3 && m.head_elements == 3));
    assert!(maps.iter().any(|m| m.count == 4101 && m.site.first().map(|s| s == "volume").unwrap_or(false) && !m.deferred));
}

#[test]
fn sop_deferral_preserves_late_diagnostics() {
    let bad = check_source(
        r#"(workspace bad
        (graph sculpture :context sop
          (let* [bad_data (map (fn [(i : int)]
                         (array/nth (array/int 1 17) (if (< i 4096) 0 1)))
                       (array/range 4101 :type int))] nil)))"#,
    );
    let expected = match flow_ir::qualify_workspace(&Default::default(), &bad) {
        Err(d) => d,
        Ok(_) => panic!("eager error missing"),
    };
    let deferred = qualify(&bad, true);
    let value = recorded(&deferred, "bad_data");
    let program = executor_compile(&value, true);
    // Production force restores the authored error span by rerunning a failed compiled leaf.
    // Qualification above still requires a fully compiled form.
    match executor::force(&program, &Default::default(), &live_at(0.)) {
        Err(actual) => assert!(eq_err(&actual, &expected), "deferred diagnostic expected {} but got {}", rays_diagnostic::to_string(&expected), rays_diagnostic::to_string(&actual)),
        Ok(_) => panic!("deferred late error missing"),
    }
    let record = Value::Record(vec![("before".into(), Value::Int(17)), ("failed".into(), value)].into());
    let program = executor_compile(&record, true);
    match executor::force(&program, &Default::default(), &live_at(0.)) {
        Err(actual) => assert!(eq_err(&actual, &expected)),
        Ok(_) => panic!("deferred late error missing in the record"),
    }
}

#[test]
fn kernel_first_head_is_zero() {
    let workspace = checked("(map (fn [i] (* i 0.5)) (array/range 4100))");
    let profile = profile::create(clock_zero());
    ok(profile::sample(&profile, &mut || fast_static(&workspace)));
    let map = profile::static_maps(&profile).remove(0);
    assert!(map.head_elements == 0 && map.tail_elements == 0);
    let workspace = checked("(let* [f (fn [(i : int)] (let* [y (+ i 7)] (* y 2))) a (map f (array/range 4101 :type int))] {:a a})");
    let profile = profile::create(clock_zero());
    ok(profile::sample(&profile, &mut || flow_ir::static_(&flow_ir::StaticOptions { record: true, replay: true, verify: true, ..Default::default() }, &workspace)));
    for map in profile::static_maps(&profile) {
        assert!(map.head_elements == 0 && map.tail_elements == 0);
    }
}


// Keep all three nested captured-helper hash calls and cross the 4096-element kernel boundary.
#[test]
fn map_million_marks_hash() {
    check_map(maps()[0]);
}

#[test]
fn map_hash_one_call() {
    check_map("(let* [indices (array/range 4097) extent 300.0]\n     (map (fn [i] (let* [x (- (abs (mod (* (sin (+ (* i 12.9898) 78.233)) 43758.5453) 1.0)) 0.5)]\n                    [(* x extent) (* x 2.0) 0])) indices))");
}

/// A fold over a table, and its count, in a map body (spiral's ramp segment, a helper whose array is
/// a parameter) is the kernel's, with the walker's values; so is a fold over a captured or sliced table.
#[test]
fn fold_over_a_table_is_the_kernels() {
    let ws = check_source(
        "(workspace w
           (defn seg :context value [(ps : (array float)) (u : float)]
             (let* [below (fold [n 0] [p ps] (if (<= p u) (int/add n 1) n))] (int/sub below (array/count ps))))
           (graph g :context host
             (let* [knots (map (fn [i] (* i 0.25)) (array/range 5))]
               {:helper (map (fn [i] (seg knots (* i 0.0002))) (array/range 5000))
                :captured (map (fn [i] (fold [s 0.0] [k knots] (+ s (* k i)))) (array/range 4200))
                :sliced (map (fn [i] (fold [s 0.0] [k (array/slice knots 1 3)] (+ s (* k i)))) (array/range 4300))})))",
    );
    let walked = ok(e::r#static(&ws, true, None));
    let profile = profile::create(clock_zero());
    let fast = ok(profile::sample(&profile, &mut || fast_static(&ws)));
    assert!(same(&result_of(&walked, "g"), &result_of(&fast, "g")), "fold over a table differs from the walker");
    let maps = profile::static_maps(&profile);
    let kernels = maps.iter().filter(|m| m.count > 4096 && m.tier == Tier::CpuKernel && m.refusal.is_empty()).count();
    assert_eq!(kernels, 3, "{:?}", maps.iter().map(|m| (m.count, m.tier, m.refusal.iter().map(|d| d.message.clone()).collect::<Vec<_>>())).collect::<Vec<_>>());
}
