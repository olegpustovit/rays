//! Capture ordered input and logical dimensions.
use input::{Event, Frame};

/// Copy `f.input` into `dst`, dropping `FocusGained`. Reuses `dst`'s vec capacities (warm = 0 alloc
/// when events/keys/buttons are plain moves with no text).
pub fn refresh(dst: &mut Frame, f: &rays::frame::Frame) {
    copy_input(dst, &f.input);
    dst.events.retain(|event| *event != Event::FocusGained);
}

/// Copy an input snapshot while retaining the storage of its event and held-input lists.
pub fn copy_input(dst: &mut Frame, src: &Frame) {
    dst.t = src.t;
    dst.dt = src.dt;
    dst.frame = src.frame;
    dst.tick = src.tick;
    dst.size = src.size;
    dst.pointer = src.pointer;
    dst.buttons.clone_from(&src.buttons);
    dst.keys.clone_from(&src.keys);
    dst.events.clone_from(&src.events);
}

/// The window's focus is the editor's: a sketch reads only its loss.
pub fn of_frame(f: &rays::frame::Frame) -> Frame {
    let mut live = input::at_time(0.0);
    refresh(&mut live, f);
    live
}
