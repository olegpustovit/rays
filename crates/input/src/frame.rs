//! Immutable facts about one sketch frame.
use crate::{Event, MouseButton};

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    /// Canonical logical input and frame facts.
    pub input: crate::Frame,
    /// Backing pixels per logical point on each axis.
    pub pixel_scale: (f64, f64),
    pub fps: f64,
    /// Accumulated logical pointer movement for this frame.
    pub mouse_delta: (f64, f64),
}

pub fn mouse_down(button: MouseButton, frame: &Frame) -> bool {
    frame.input.buttons.contains(&button)
}

pub fn has_event(predicate: &dyn Fn(&Event) -> bool, frame: &Frame) -> bool {
    frame.input.events.iter().any(predicate)
}
