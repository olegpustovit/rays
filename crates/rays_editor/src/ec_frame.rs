//! Convert between sketch frames and the slimmer `editor_core::frame` used by the command router.
use editor_core::frame as ec;
use rays::frame;

pub fn to(frame: &frame::t) -> ec::Frame {
    ec::Frame {
        input: frame.input.clone(),
        pixel_scale: frame.pixel_scale,
        fps: frame.fps,
        mouse_delta: frame.mouse_delta,
    }
}

pub fn apply(base: &frame::t, routed: ec::Frame) -> frame::t {
    frame::t {
        input: routed.input,
        pixel_scale: base.pixel_scale,
        fps: base.fps,
        mouse_delta: base.mouse_delta,
    }
}
