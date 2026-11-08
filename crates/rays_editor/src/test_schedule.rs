//! Port of the schedule half of test_packed_pieces.ml `test_timeline_and_schedule`.
use crate::schedule as s;
use crate::test_timeline::frame_with;
use input::MouseButton;
use param::NO_EFFECTS;
use rays::frame;
use sop::context::dependencies as deps;
use sop::{custom, node};

fn graph(fact: Option<deps::Fact>) -> node::T {
    let mut args = custom::plain_args("schedule_test");
    args.dependencies = fact.map_or(deps::STATIC, deps::one);
    custom::plain(args, vec![], std::sync::Arc::new(|_, _| rdk_rays::geometry_view::points_of_nothing().map_err(|e| format!("{e:?}"))))
}

fn quiet() -> frame::Frame {
    frame_with(0.25, vec![], vec![])
}

fn held() -> frame::Frame {
    frame_with(0.25, vec![], vec![MouseButton::LeftButton])
}

#[allow(clippy::too_many_arguments)]
fn step(live: bool, input: Option<&input::Frame>, schedule: &s::Schedule, graphs: &[node::T], context_changed: bool, busy: bool, frame: &frame::Frame) -> (s::Schedule, bool) {
    s::step(live, input, schedule, graphs, NO_EFFECTS, context_changed, false, busy, frame)
}

#[test]
fn cook_scheduler_fires_on_change_clock_and_held_drags() {
    let fixed = graph(None);
    let dynamic = graph(Some(deps::Fact::Time));
    let (schedule, fire) = step(false, None, &s::initial(), std::slice::from_ref(&fixed), false, false, &quiet());
    assert!(fire, "skipped the initial graph");
    let (schedule, fire) = step(false, None, &schedule, std::slice::from_ref(&fixed), true, false, &quiet());
    assert!(!fire, "static graph recooked for an unrelated clock change");
    let (schedule, fire) = step(false, None, &schedule, std::slice::from_ref(&dynamic), true, false, &held());
    assert!(!fire, "dynamic graph cooked while a parameter drag was held");
    let (_, fire) = step(true, None, &schedule, std::slice::from_ref(&dynamic), true, false, &held());
    assert!(fire, "live cooking did not cook during a held drag");
    let (_, fire) = step(true, None, &schedule, std::slice::from_ref(&dynamic), true, true, &held());
    assert!(!fire, "live cooking cancelled a running cook during a drag");
    let (_, fire) = step(false, None, &schedule, std::slice::from_ref(&dynamic), false, false, &quiet());
    assert!(fire, "lost the latest held dynamic request");
}

fn frame_at(t: f64, pointer: (f64, f64), keys: Vec<input::Key>, size: (i64, i64)) -> input::Frame {
    let mut f = input::at_time(t);
    f.pointer = pointer;
    f.keys = keys;
    f.size = size;
    f
}

/// how many of `frames` after the first cook again
fn count(graph: &node::T, frames: &[input::Frame]) -> i64 {
    let (mut schedule, _) = step(false, Some(&frames[0]), &s::initial(), std::slice::from_ref(graph), false, false, &quiet());
    let mut cooks = 0;
    for input in &frames[1..] {
        let changed = input.t != schedule.input.as_ref().unwrap().t;
        let (next, fire) = step(false, Some(input), &schedule, std::slice::from_ref(graph), changed, false, &quiet());
        schedule = next;
        cooks += i64::from(fire);
    }
    cooks
}

#[test]
fn an_input_slice_nobody_reads_never_recooks() {
    let time = graph(Some(deps::Fact::Time));
    let pointer = graph(Some(deps::Fact::Pointer));
    let at = |t: f64| frame_at(t, (0.0, 0.0), vec![], (100, 100));
    let moving: Vec<input::Frame> = (0..30).map(|i| frame_at(0.0, (i as f64, 1.0), vec![input::Key::Space], (100, 100))).collect();
    let clock: Vec<input::Frame> = (0..30).map(|i| frame_at(i as f64, (i as f64, 1.0), vec![], (100, 100))).collect();
    let resized: Vec<input::Frame> = (0..30).map(|i| frame_at(0.0, (0.0, 0.0), vec![], (i, 1))).collect();
    let with_first = |rest: &[input::Frame]| -> Vec<input::Frame> { std::iter::once(at(0.0)).chain(rest.iter().cloned()).collect() };
    assert_eq!(count(&time, &with_first(&moving)), 0, "a t-only graph recooked on pointer or keys");
    assert_eq!(count(&time, &with_first(&resized)), 0, "a t-only graph recooked on a resize");
    assert_eq!(count(&time, &with_first(&clock)), 29, "a t-only graph missed a clock change");
    assert_eq!(count(&pointer, &with_first(&moving)), 30, "a pointer graph missed motion");
    let still: Vec<input::Frame> = (0..30).map(|_| at(0.0)).collect();
    assert_eq!(count(&pointer, &with_first(&still)), 0);
}
