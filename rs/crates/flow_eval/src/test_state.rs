// Port of lib/flow/test_state.ml.
use crate::*;
use crate::test_workspace::{base_catalog as _unused, show_ds};
use flow_check::{catalog::Catalog, op::{self, Op}, workspace as W};
use flow_syntax::{context, syntax, ty, value};
use rays_diagnostic::Diagnostic;
use std::sync::{atomic::{AtomicI64, Ordering as O}, Arc};

fn empty() -> Catalog { Catalog { version: 1, kinds: vec![] } }
fn checked_src(source: &str) -> W::Workspace {
    match W::check(&[], false, false, None, &empty(), &syntax::parse(source).unwrap()) { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{}", show_ds(&ds)) }
}
fn checked(body: &str) -> W::Workspace { checked_src(&format!("(workspace w (graph g :context value {body}))")) }
fn value_of(r: &Result<Eval, Diagnostic>) -> Value { r.as_ref().unwrap().results.iter().find(|(n, _)| n == "g").unwrap().1.clone() }
fn at(time: f64, frame: i64, tick: i64) -> input::Frame { let mut f = input::at_time(time); f.frame = frame; f.tick = tick; f }
fn get(e: &Eval, n: &str) -> Value { e.results.iter().find(|(k, _)| k == n).unwrap().1.clone() }

#[test]
fn forked_states_diverge() {
    let workspace = checked("(state [n 0.0] (+ n t))");
    let run0 = |state: &State, frame: i64, time: f64| run(&workspace, false, None, Some(state), Some(&at(time, frame, 0)), time);
    let initial = create_state();
    assert_eq!(value_of(&run0(&initial, 0, 0.25)), Value::Float(0.25));
    let (left, right) = (fork_state(&initial), fork_state(&initial));
    assert_eq!(state_stamp(&left), state_stamp(&right));
    assert_eq!(value_of(&run0(&left, 1, 0.75)), Value::Float(1.0));
    assert_eq!(value_of(&run0(&right, 1, 1.25)), Value::Float(1.5));
    assert_ne!(state_stamp(&left), state_stamp(&right));
}

#[test]
fn graph_state_clocks_and_snapshots_agree() {
    let workspace = checked_src("(workspace references\n    (graph level :context value [(step : float 2)] (state [n 0.0] (+ n step)))\n    (graph host :context host (state [n 0] (+ n 3)))\n    (graph inline :context value (if (< (ref level) 4) 2 4))\n    (graph bound :context value (let* [n (ref level)] (if (< n 4) 2 4)))\n    (graph override_inline :context value (if (< (ref level :step 4) 8) 4 8))\n    (graph override_bound :context value\n      (let* [n (ref level :step 4.0)] (if (< n 8) 4 8)))\n    (graph other :context value (if (< (ref level :step 6) 12) 6 12))\n    (graph shared :context value (+ (ref level :step 4) (ref level :step 4.0)))\n    (graph host_inline :context value (if (< (ref host) 6) 3 6))\n    (graph host_bound :context value (let* [n (ref host)] (if (< n 6) 3 6))))");
    let evaluation = r#static(&workspace, false, None).unwrap();
    let identities = || evaluation.plan.instances.iter().map(|i| (i.graph.clone(), i.default, i.inputs.iter().map(|(n, v)| (n.clone(), show(v))).collect::<Vec<_>>())).collect::<Vec<_>>();
    let original = identities();
    assert_eq!(original.len(), 12);
    type Force = fn(&Value, &input::Frame, &State) -> Result<Value, Diagnostic>;
    let forces: [Force; 2] = [|v, l, s| force(v, l, Some(s)), |v, l, s| Host::force_reference(v, l, Some(s), None, None)];
    for force_fn in forces {
        let caller = create_state();
        for (tick, frame, level, host) in [(0i64, 0i64, 1i64, 1i64), (1, 0, 1, 2), (1, 1, 2, 2)] {
            let live = at(0., frame, tick);
            for v in &evaluation.states { force_fn(v, &live, &caller).unwrap(); }
            let stamp = state_stamp(&caller);
            let expected: Vec<(&str, Value)> = vec![
                ("level", Value::Float((2 * level) as f64)), ("host", Value::Int(3 * host)), ("inline", Value::Int(2 * level)), ("bound", Value::Int(2 * level)),
                ("override_inline", Value::Int(4 * level)), ("override_bound", Value::Int(4 * level)), ("other", Value::Int(6 * level)), ("shared", Value::Float((8 * level) as f64)),
                ("host_inline", Value::Int(3 * host)), ("host_bound", Value::Int(3 * host)),
            ];
            let forward = evaluation.results.clone();
            let mut backward = evaluation.results.clone(); backward.reverse();
            for results in [forward, backward] {
                let private_state = fork_state(&caller);
                for _ in 0..2 {
                    for (name, residual) in &results {
                        let actual = force_fn(residual, &live, &private_state).unwrap();
                        let wanted = &expected.iter().find(|(n, _)| n == name).unwrap().1;
                        assert!(actual == *wanted, "Graph state {name} at tick{tick}/frame{frame}: expected {}, got {}", String::from_utf8_lossy(&show(wanted)), String::from_utf8_lossy(&show(&actual)));
                    }
                }
                assert!(state_stamp(&private_state) == stamp && state_stamp(&caller) == stamp);
            }
            assert_eq!(identities(), original);
        }
    }
}

#[test]
fn state_folds_seek_reset_and_roll_back() {
    let w = checked("(let* [s (state [previous 0.0] (+ previous (frame/dt)))] (+ s s))");
    let state = create_state();
    let run_at = |frame: i64| { let mut l = at(frame as f64, frame, 0); l.dt = 0.25; run(&w, false, None, Some(&state), Some(&l), frame as f64) };
    assert_eq!(value_of(&run_at(0)), Value::Float(0.5));
    assert_eq!(value_of(&run_at(0)), Value::Float(0.5));
    assert_eq!(value_of(&run_at(1)), Value::Float(1.));
    assert_eq!(value_of(&run_at(2)), Value::Float(1.5));
    // Seeking backwards and explicit reload reset both begin a fresh fold.
    assert_eq!(value_of(&run_at(0)), Value::Float(0.5));
    reset_state(&state, true);
    assert_eq!(value_of(&run_at(1)), Value::Float(0.5));
    // Independent playback sessions have independent snapshots.
    let export = || {
        let state = create_state();
        (0..8).map(|frame| { let mut l = at(frame as f64 * 0.125, frame, 0); l.dt = 0.125; value_of(&run(&w, false, None, Some(&state), Some(&l), frame as f64 * 0.125)) }).collect::<Vec<_>>()
    };
    assert_eq!(export(), export());
    // Every state reads the previous snapshot, independent of forcing order.
    let w = checked("(let* [a (state [x 0] (+ x 1)) b (state [y 0] (+ y a))] (+ a b))");
    let s = create_state();
    let run_frame = |frame: i64| value_of(&run(&w, false, None, Some(&s), Some(&at(0., frame, 0)), 0.));
    assert!(run_frame(0) == Value::Int(2) && run_frame(1) == Value::Int(5) && run_frame(1) == Value::Int(5));
    // An error after the fold step must not commit a partial frame.
    let w = checked("(let* [a (state [x 0] (+ x 1))] (+ a (nth (list 0) (frame/index))))");
    let s = create_state();
    let run_frame = |frame: i64| run(&w, false, None, Some(&s), Some(&at(0., frame, 0)), 0.);
    assert_eq!(value_of(&run_frame(0)), Value::Int(1));
    assert!(run_frame(1).is_err());
    assert_eq!(value_of(&run_frame(0)), Value::Int(1));
    let (_, ds) = W::check(&[], false, false, None, &empty(), &syntax::parse("(workspace w (graph g :context value (state [s t] s)))").unwrap());
    assert!(ds.iter().any(|d| d.code == "E_STATE_INIT"));
}

#[test]
fn host_and_animated_state_clocks() {
    let workspace = checked_src("(workspace w (graph host :context host (state [n 0] (+ n 1))) (graph animated :context value (state [n 0] (+ n 1))))");
    let state = create_state();
    let run_at = |state: &State, tick: i64, frame: i64| run(&workspace, false, None, Some(state), Some(&at(0., frame, tick)), 0.).unwrap();
    let result = run_at(&state, 0, 0);
    assert!(get(&result, "host") == Value::Int(1) && get(&result, "animated") == Value::Int(1));
    let result = run_at(&state, 1, 0);
    assert!(get(&result, "host") == Value::Int(2) && get(&result, "animated") == Value::Int(1));
    assert_eq!(get(&run_at(&state, 1, 0), "host"), Value::Int(2));
    let snapshot = fork_state(&state);
    assert_eq!(get(&run_at(&state, 2, 1), "host"), Value::Int(3));
    let result = run_at(&snapshot, 2, 1);
    assert!(get(&result, "host") == Value::Int(3) && get(&result, "animated") == Value::Int(2));
    reset_state(&state, false);
    let result = run_at(&state, 3, 0);
    assert!(get(&result, "host") == Value::Int(4) && get(&result, "animated") == Value::Int(1));
    reset_state(&state, true);
    assert_eq!(get(&run_at(&state, 4, 0), "host"), Value::Int(1));
}

#[test]
fn exact_images_cannot_be_state() {
    let catalog = empty();
    for expression in [
        "(state [s (exact (image/noise))] s)", "(let* [img (exact (image/noise)) alias img] (state [s alias] s))", "(state [s (list (exact (image/noise)))] s)",
        "(state [s {:image (exact (image/noise))}] s)", "(let* [snapshot (fn [] (exact (image/noise)))] (state [s (snapshot)] s))",
    ] {
        let src = format!("(workspace images (graph g :context value {expression}))");
        let (workspace, ds) = W::check(&[], false, false, None, &catalog, &syntax::parse(&src).unwrap());
        assert!(workspace.is_none() && ds.iter().any(|d| d.code == "E_STATE_TYPE"), "{expression}: {}", show_ds(&ds));
    }
    let (workspace, ds) = W::check(&[], false, false, None, &catalog, &syntax::parse("(workspace images\n    (graph img :context image (exact (image/noise)))\n    (graph g :context value (state [s (ref img)] s)))").unwrap());
    assert!(workspace.is_none() && ds.iter().any(|d| d.code == "E_STATE_TYPE"));
    assert_eq!(value_of(&run(&checked("(state [s (exact 1.0)] (+ s 1))"), false, None, None, None, 0.)), Value::Float(2.));
    let array_state = create_state();
    assert_eq!(value_of(&run(&checked("(array/sum (state [s (exact (array/range 3))] s))"), false, None, Some(&array_state), None, 0.)), Value::Float(3.));
    assert!(Host::state_values(&array_state).contains(&Value::Float_array(Arc::new(vec![0., 1., 2.]))));
    // An opaque extension can conceal resources from static types. Check the seed before the step
    // reads it, and the step before it reaches next.
    for wrap in 0..4 {
        let calls = Arc::new(AtomicI64::new(0));
        let mut operator: Op = op::find(&[], "sin", &context::value()).unwrap();
        operator.name = "test/opaque".into();
        operator.signature = op::Signature { pos: vec![("resource".into(), ty::Ty::Bool)], opt: vec![], rest: None, kw: vec![] };
        operator.out = Arc::new(|_| Ok(ty::Ty::Any));
        operator.any_num = false; operator.arithmetic = None; operator.packed_extension = None;
        let c = calls.clone();
        operator.body = Arc::new(move |_, _, args| {
            c.fetch_add(1, O::SeqCst);
            let resource = value::truthy(&args.iter().find(|(n, _)| n == "resource").unwrap().1)?;
            let v = if resource { Value::Deferred(ty::image(), 0) } else { Value::Int(0) };
            Ok(match wrap { 0 => v, 1 => Value::List(vec![v]), 2 => Value::Record(vec![("image".into(), v)]), _ => Value::Struct("opaque".into(), ty::Ty::Any, vec![("image".into(), v)]) })
        });
        for (seed, step, expected_calls) in [("(test/opaque true)", "(test/opaque false)", 1), ("(test/opaque false)", "(test/opaque true)", 2)] {
            let src = format!("(workspace opaque (graph g :context value (state [s {seed}] {step})))");
            let workspace = match W::check(std::slice::from_ref(&operator), false, false, None, &catalog, &syntax::parse(&src).unwrap()) { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{}", show_ds(&ds)) };
            let evaluation = r#static(&workspace, false, None).unwrap();
            let residual = get(&evaluation, "g");
            type Force = fn(&Value, &input::Frame, &State) -> Result<Value, Diagnostic>;
            let forces: [Force; 2] = [|v, l, s| force(v, l, Some(s)), |v, l, s| Host::force_reference(v, l, Some(s), None, None)];
            for force_fn in forces {
                let state = create_state();
                let stamp = state_stamp(&state);
                calls.store(0, O::SeqCst);
                match force_fn(&residual, &input::at_time(0.), &state) { Err(d) => assert_eq!(d.code, "E_STATE_TYPE"), Ok(_) => panic!("accepted") }
                assert!(calls.load(O::SeqCst) == expected_calls && state_stamp(&state) == stamp && Host::state_values(&state).is_empty(), "{wrap} {seed}: calls {}", calls.load(O::SeqCst));
            }
        }
    }
}
