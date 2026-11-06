//! Private per-sketch realtime clock and frame pacing.
use std::time::Instant;

pub struct t {
    origin: Instant,
    last_frame_time: f64,
    delta_time: f64,
    target_fps: Option<i64>,
    vsync_enabled: bool,
}

pub fn create(target_fps: i64, vsync_enabled: bool) -> t {
    t {
        origin: Instant::now(),
        last_frame_time: 0.,
        delta_time: 0.,
        target_fps: if target_fps > 0 { Some(target_fps) } else { None },
        vsync_enabled,
    }
}

pub fn now(state: &t) -> f64 {
    state.origin.elapsed().as_secs_f64()
}

pub fn delta(state: &t) -> f64 {
    state.delta_time
}

pub fn update(state: &mut t) {
    let current_time = now(state);
    state.delta_time = (current_time - state.last_frame_time).min(0.1);
    state.last_frame_time = current_time;
}

pub fn limit_frame_rate(state: &t) {
    match state.target_fps {
        None => {}
        Some(_) if state.vsync_enabled => {}
        Some(fps) => {
            let delay = (1. / fps as f64) - state.delta_time;
            if delay > 0. {
                std::thread::sleep(std::time::Duration::from_secs_f64(delay));
            }
        }
    }
}
