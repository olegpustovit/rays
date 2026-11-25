//! Capture ordered input and logical dimensions.
use input::{Event, Frame};

/// The window's focus is the editor's: a sketch reads only its loss.
pub fn of_frame(f: &rays::frame::Frame) -> Frame {
    let mut live = f.input.clone();
    live.events.retain(|event| *event != Event::FocusGained);
    live
}
