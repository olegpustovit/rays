//! Timeline controls and scheduling intents.
use crate::timeline::{self as t, Mode};
use rays::frame;

pub fn frame_with(dt: f64, events: Vec<input::Event>, buttons: Vec<input::MouseButton>) -> frame::Frame {
    let mut input = input::at_time(0.0);
    input.size = (100, 100);
    input.dt = dt;
    input.events = events;
    input.buttons = buttons;
    frame::Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

fn tick() -> frame::Frame {
    frame_with(0.25, vec![], vec![])
}

#[test]
fn timeline_advances_pauses_stops_resets_and_seeks() {
    let (timeline, changes) = t::update(&t::create(), &tick());
    assert!(t::time(&timeline) == 0.25 && t::frame(&timeline) == 1 && t::changed_context(&changes), "did not advance deterministically");
    let (timeline, _) = t::toggle_pause(&timeline);
    let (timeline, _) = t::update(&timeline, &tick());
    assert!(t::mode(&timeline) == Mode::Paused && t::time(&timeline) == 0.25, "pause did not freeze time");
    let (timeline, _) = t::stop(&timeline);
    assert!(t::mode(&timeline) == Mode::Stopped && t::time(&timeline) == 0.0 && t::frame(&timeline) == 0, "stop did not rewind");
    let (timeline, reset_changes) = t::reset(&timeline);
    let (timeline, _) = t::update(&timeline, &tick());
    assert!(t::mode(&timeline) == Mode::Playing && t::time(&timeline) == 0.25 && t::changed_context(&reset_changes), "reset did not restart playback");
    // Seek jumps at the mean observed step (0.25 s), pauses, and changes the cook context; keys never
    // drive the clock directly.
    let (sought, seek_changes) = t::seek(&timeline, 8);
    assert!(t::frame(&sought) == 8 && t::time(&sought) == 2.0 && t::mode(&sought) == Mode::Paused && t::changed_context(&seek_changes));
    assert_eq!(t::frame(&t::seek(&timeline, -3).0), 0, "seek did not clamp at frame 0");
    let key = frame_with(0.25, vec![input::Event::KeyPressed(input::Key::KeyChar('p'))], vec![]);
    assert_eq!(t::mode(&t::update(&t::create(), &key).0), Mode::Playing, "timeline reacted to a key press");
}

/// `editor_loop::may_rest` treats Playing + dt==0 as clock_idle: the timeline must not advance then,
/// or resting would freeze a moving picture. Positive dt while Playing must advance (basic's
/// "frame N" text); may_rest must stay awake on that path.
#[test]
fn playing_dt_advances_frame_index_dt_zero_does_not() {
    let playing = t::create();
    assert_eq!(t::mode(&playing), Mode::Playing);
    let (moved, changes) = t::update(&playing, &frame_with(1.0 / 60.0, vec![], vec![]));
    assert_eq!(t::frame(&moved), 1, "Playing + dt>0 must advance frame/index");
    assert!(t::time(&moved) > 0.0);
    assert!(t::changed_context(&changes), "advance is a cook-context change");

    let (frozen, idle) = t::update(&moved, &frame_with(0.0, vec![], vec![]));
    assert_eq!(t::frame(&frozen), 1, "Playing + dt==0 must not advance frame/index");
    assert_eq!(t::time(&frozen), t::time(&moved), "clock must not move at dt==0");
    assert!(!t::changed_context(&idle), "dt==0 while Playing is semantically at rest");
}
