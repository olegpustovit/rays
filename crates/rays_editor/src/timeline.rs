//! Deterministic sketch-owned playback clock.
//!
//! The runtime frame clock always advances, while this clock can pause, stop, and reset without
//! touching global state. It reads no keys: the host drives `toggle_pause`, `stop`, and `reset`
//! from its keymap.
#![allow(non_camel_case_types)]
use rays::frame;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Playing,
    Paused,
    Stopped,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct t {
    pub mode: Mode,
    pub time: f64,
    pub frame: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Change {
    Advanced,
    Paused_now,
    Resumed,
    Stopped_now,
    Reset_now,
    Seeked,
}

pub fn create() -> t {
    t { mode: Mode::Playing, time: 0.0, frame: 0 }
}
pub fn mode(value: &t) -> Mode {
    value.mode
}
pub fn time(value: &t) -> f64 {
    value.time
}
pub fn frame(value: &t) -> i64 {
    value.frame
}
pub fn reset(_: &t) -> (t, Vec<Change>) {
    (create(), vec![Change::Reset_now])
}
pub fn stop(_: &t) -> (t, Vec<Change>) {
    (t { mode: Mode::Stopped, time: 0.0, frame: 0 }, vec![Change::Stopped_now])
}

pub fn toggle_pause(value: &t) -> (t, Vec<Change>) {
    match value.mode {
        Mode::Playing => (t { mode: Mode::Paused, ..*value }, vec![Change::Paused_now]),
        Mode::Paused | Mode::Stopped => (t { mode: Mode::Playing, ..*value }, vec![Change::Resumed]),
    }
}

/// Jump to `frame` (clamped at 0) and pause; time is `frame` times the mean step observed so far,
/// or 1/60 s before any. `Seeked` changes the context.
pub fn seek(value: &t, frame: i64) -> (t, Vec<Change>) {
    let frame = frame.max(0);
    let step = if value.frame > 0 { value.time / value.frame as f64 } else { 1.0 / 60.0 };
    (
        t { mode: if value.mode == Mode::Stopped { Mode::Stopped } else { Mode::Paused }, frame, time: frame as f64 * step },
        vec![Change::Seeked],
    )
}

pub fn update(value: &t, runtime_frame: &frame::t) -> (t, Vec<Change>) {
    let dt = runtime_frame.input.dt;
    if value.mode == Mode::Playing && dt.is_finite() && dt > 0.0 {
        (t { time: value.time + dt, frame: value.frame + 1, ..*value }, vec![Change::Advanced])
    } else {
        (*value, Vec::new())
    }
}

pub fn changed_context(changes: &[Change]) -> bool {
    changes.iter().any(|c| matches!(c, Change::Advanced | Change::Stopped_now | Change::Reset_now | Change::Seeked))
}
