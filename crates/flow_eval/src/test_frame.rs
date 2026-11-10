use std::sync::Arc;
use crate::*;
use crate::test_workspace::{errors, show_ds};
use flow_check::{catalog::Catalog, workspace as W};
use flow_syntax::{syntax, value};
use input::{Event, Key, MouseButton, ScrollPhase};
use rays_diagnostic::Diagnostic;

fn empty() -> Catalog { Catalog { version: 1, kinds: vec![] } }
fn check_in(text: &str, g: &str) -> (Option<W::Workspace>, Vec<Diagnostic>) {
    let src = format!("(workspace w (graph g :context value {text}))");
    let _ = g;
    W::check(&[], false, false, None, &empty(), &syntax::parse(&src).unwrap())
}
fn check(text: &str) -> W::Workspace {
    match check_in(text, "g") { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{}", show_ds(&ds)) }
}
fn recorded(e: &Eval, path: &[&str]) -> Value {
    let p: flow_check::workspace::Path = path.iter().map(|s| (*s).into()).collect();
    e.records.iter().find(|(q, _)| *q == p).unwrap().1[0].1.clone()
}
fn result_g(e: &Eval) -> Value { e.results.iter().find(|(n, _)| n == "g").unwrap().1.clone() }
fn has_code(ds: &[Diagnostic], code: &str) -> bool { ds.iter().any(|d| d.code == code) }

#[test]
fn frame_reads_force_against_the_live_frame() {
    let w = check("(let* [x (pointer/x) r {:x x :y (pointer/y) :dt (frame/dt) :i (frame/index) :w (frame/width) :h (frame/height) :mouse (pointer/down) :right (pointer/down \"right\") :key (key/down \"space\") :time t}] r.x)");
    let s = r#static(&w, true, None).unwrap();
    let value = recorded(&s, &["g", "r"]);
    assert!(is_live(&value) && frame_dependent(&value));
    let mut live = input::at_time(3.);
    live.dt = 0.125;
    live.frame = 24;
    live.size = (800, 600);
    live.pointer = (12., 37.);
    live.buttons = vec![MouseButton::LeftButton];
    live.keys = vec![Key::Space];
    live.events = vec![Event::KeyPressed(Key::Space), Event::PointerPressed(MouseButton::LeftButton, (12., 37.))];
    let f = |n: &str, v: Value| (n.into(), v);
    let expected = Value::Record(vec![f("x", Value::Float(12.)), f("y", Value::Float(37.)), f("dt", Value::Float(0.125)), f("i", Value::Int(24)), f("w", Value::Int(800)), f("h", Value::Int(600)), f("mouse", Value::Bool(true)), f("right", Value::Bool(false)), f("key", Value::Bool(true)), f("time", Value::Float(3.))].into());
    assert_eq!(force(&value, &live, None), Ok(expected.clone()));
    let mut changed = live.clone();
    changed.pointer = (99., 37.);
    changed.keys = vec![];
    changed.size = (900, 600);
    for l in [&live, &changed, &live] {
        assert_eq!(force(&value, l, None), host::force_reference(&value, l, None, None, None));
    }
    assert_ne!(force(&value, &changed, None), Ok(expected));
    assert_eq!(result_g(&run(&w, false, None, None, Some(&live), 3.).unwrap()), Value::Float(12.));
    let mut bad = live.clone(); bad.dt = f64::NAN;
    assert!(force(&value, &bad, None).is_err());
    let mut bad = live.clone(); bad.size = (-1, 600);
    assert!(force(&value, &bad, None).is_err());
    let mut bad = live.clone(); bad.events = vec![Event::Pinched(f64::NAN)];
    assert!(force(&value, &bad, None).is_err());
    let (mut a, mut b) = (live.clone(), live.clone());
    a.dt = 0.; b.dt = -0.;
    assert!(!input::equal(&a, &b));
    let input_v = result_g(&r#static(&check("(let* [f (frame/input)] (count f.events))"), true, None).unwrap());
    assert_eq!(force(&input_v, &live, None), Ok(Value::Int(2)));
    let input_v = result_g(&r#static(&check("(let* [event (nth (get (frame/input) :events) 0)] (if (equal? event.kind \"dialog-opened\") event.id 0))"), false, None).unwrap());
    let mut dialog = live.clone(); dialog.events = vec![Event::DialogOpened(27)];
    assert_eq!(force(&input_v, &dialog, None), Ok(Value::Int(27)));
    let snapshot = r#static(&check("(let* [f (frame/input)\n      r {:keys f.keys :buttons f.buttons\n         :key (get (nth f.events 0) :key)\n         :button (get (nth f.events 1) :button)\n         :phase (get (nth f.events 2) :phase)}] 0.0)"), true, None).unwrap();
    let snapshot = recorded(&snapshot, &["g", "r"]);
    let mut live = live;
    live.keys = vec![Key::Space, Key::Unknown(4242)];
    live.buttons = vec![MouseButton::LeftButton, MouseButton::MouseX2];
    live.events = vec![Event::KeyPressed(Key::Unknown(4242)), Event::PointerCancelled(MouseButton::MouseX2), Event::TrackpadScrolled { delta: (0., -12.), phase: ScrollPhase::Momentum, time: 3.5 }];
    let t = |s: &str| Value::Text(s.as_bytes().into());
    assert_eq!(force(&snapshot, &live, None), Ok(Value::Record(vec![
        f("keys", Value::List(Arc::new(vec![t("space"), t("unknown:4242")]))),
        f("buttons", Value::List(Arc::new(vec![t("left"), t("x2")]))),
        f("key", t("unknown:4242")),
        f("button", t("x2")),
        f("phase", t("momentum")),
    ].into())));
    let (_, ds) = check_in("(for [i (range (frame/index))] i)", "g");
    assert!(has_code(&ds, "E_TIME_COUNT"));
}

#[test]
fn frame_dependent_counts_are_rejected() {
    let (_, ds) = check_in("(let* [r (if (> (frame/index) 0) {:items (list 1 2)} {:items (list 1)})] (for [x r.items] x))", "g");
    assert!(has_code(&ds, "E_TIME_COUNT"), "{}", show_ds(&ds));
    let (_, ds) = check_in("(let* [f (frame/input)] (for [e f.events] e.kind))", "g");
    assert!(has_code(&ds, "E_TIME_COUNT"), "{}", show_ds(&ds));
}

// A map is live when its function reads the frame, whichever elements do: the first million never
// read it here, and walking their heavy branch to find out would exhaust the static budget.
#[test]
fn a_map_is_live_when_its_function_reads_the_frame() {
    let w = check("(let* [r (map (fn [(i : int)] (if (< i 1000000) (array/sum (map (fn [(j : int)] (* j 2.0)) (array/range 1000 :type int))) t)) (array/range 1000001 :type int))] (array/nth r 0))");
    let s = r#static(&w, true, None).unwrap();
    assert!(is_live(&recorded(&s, &["g", "r"])));
    let _ = (errors(&[]), value::hash(&[]));
}
