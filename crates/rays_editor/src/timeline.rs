//! Deterministic sketch-owned playback clock.
//!
//! The runtime frame clock always advances, while this clock can pause, stop, and reset without
//! touching global state. It reads no keys: the host drives `toggle_pause`, `stop`, and `reset`
//! from its keymap.

use rays::frame;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Playing,
    Paused,
    Stopped,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Timeline {
    pub mode: Mode,
    pub time: f64,
    pub frame: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Change {
    Advanced,
    PausedNow,
    Resumed,
    StoppedNow,
    ResetNow,
    Seeked,
}

pub fn create() -> Timeline {
    Timeline { mode: Mode::Playing, time: 0.0, frame: 0 }
}
pub fn mode(value: &Timeline) -> Mode {
    value.mode
}
pub fn time(value: &Timeline) -> f64 {
    value.time
}
pub fn frame(value: &Timeline) -> i64 {
    value.frame
}
pub fn reset(_: &Timeline) -> (Timeline, Vec<Change>) {
    (create(), vec![Change::ResetNow])
}
pub fn stop(_: &Timeline) -> (Timeline, Vec<Change>) {
    (Timeline { mode: Mode::Stopped, time: 0.0, frame: 0 }, vec![Change::StoppedNow])
}

pub fn toggle_pause(value: &Timeline) -> (Timeline, Vec<Change>) {
    match value.mode {
        Mode::Playing => (Timeline { mode: Mode::Paused, ..*value }, vec![Change::PausedNow]),
        Mode::Paused | Mode::Stopped => (Timeline { mode: Mode::Playing, ..*value }, vec![Change::Resumed]),
    }
}

/// Jump to `frame` (clamped at 0) and pause; time is `frame` times the mean step observed so far,
/// or 1/60 s before any. `Seeked` changes the context.
pub fn seek(value: &Timeline, frame: i64) -> (Timeline, Vec<Change>) {
    let frame = frame.max(0);
    let step = if value.frame > 0 { value.time / value.frame as f64 } else { 1.0 / 60.0 };
    (
        Timeline { mode: if value.mode == Mode::Stopped { Mode::Stopped } else { Mode::Paused }, frame, time: frame as f64 * step },
        vec![Change::Seeked],
    )
}

pub fn update(value: &Timeline, runtime_frame: &frame::Frame) -> (Timeline, Vec<Change>) {
    let dt = runtime_frame.input.dt;
    if value.mode == Mode::Playing && dt.is_finite() && dt > 0.0 {
        (Timeline { time: value.time + dt, frame: value.frame + 1, ..*value }, vec![Change::Advanced])
    } else {
        (*value, Vec::new())
    }
}

pub fn changed_context(changes: &[Change]) -> bool {
    changes.iter().any(|c| matches!(c, Change::Advanced | Change::StoppedNow | Change::ResetNow | Change::Seeked))
}
