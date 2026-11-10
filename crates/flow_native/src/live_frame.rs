//! Capture ordered input and logical dimensions.
use input::{Event, Frame};

/// The window's focus is the editor's: a sketch reads only its loss.
pub fn of_frame(f: &rays::frame::Frame) -> Frame {
    Frame { events: f.input.events.iter().filter(|e| **e != Event::Focus_gained).cloned().collect(), ..f.input.clone() }
}
