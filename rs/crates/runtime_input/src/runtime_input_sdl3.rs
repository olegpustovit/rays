use input::{Event, Key, MouseButton, ScrollPhase};
use sdl3::{
    DialogOutcome, DropChange, Event as SdlEvent, Key as SdlKey, MouseButton as SdlMouseButton,
    PinchPhase, ScrollPhase as SdlScrollPhase, WindowChange,
};

use crate::runtime_input;

pub fn button(raw: SdlMouseButton) -> MouseButton {
    match raw {
        SdlMouseButton::Left => MouseButton::LeftButton,
        SdlMouseButton::Middle => MouseButton::MiddleButton,
        SdlMouseButton::Right => MouseButton::RightButton,
        SdlMouseButton::X1 => MouseButton::MouseX1,
        SdlMouseButton::X2 => MouseButton::MouseX2,
    }
}

pub fn key(raw: SdlKey) -> Key {
    match raw {
        SdlKey::Char(character) => Key::KeyChar(character),
        SdlKey::ArrowUp => Key::ArrowUp,
        SdlKey::ArrowDown => Key::ArrowDown,
        SdlKey::ArrowLeft => Key::ArrowLeft,
        SdlKey::ArrowRight => Key::ArrowRight,
        SdlKey::Space => Key::Space,
        SdlKey::Enter => Key::Enter,
        SdlKey::Escape => Key::Escape,
        SdlKey::Backspace => Key::Backspace,
        SdlKey::Tab => Key::Tab,
        SdlKey::Shift => Key::Shift,
        SdlKey::Control => Key::Ctrl,
        SdlKey::Alt => Key::Alt,
        SdlKey::Meta => Key::Meta,
        SdlKey::F1 => Key::F1,
        SdlKey::F2 => Key::F2,
        SdlKey::F3 => Key::F3,
        SdlKey::F4 => Key::F4,
        SdlKey::F5 => Key::F5,
        SdlKey::F6 => Key::F6,
        SdlKey::F7 => Key::F7,
        SdlKey::F8 => Key::F8,
        SdlKey::F9 => Key::F9,
        SdlKey::F10 => Key::F10,
        SdlKey::F11 => Key::F11,
        SdlKey::F12 => Key::F12,
        SdlKey::Home => Key::Home,
        SdlKey::End => Key::End,
        SdlKey::PageUp => Key::PageUp,
        SdlKey::PageDown => Key::PageDown,
        SdlKey::Insert => Key::Insert,
        SdlKey::Delete => Key::Delete,
        SdlKey::Unknown(code) => Key::Unknown(i64::from(code)),
    }
}

pub fn scroll_phase(phase: SdlScrollPhase) -> ScrollPhase {
    match phase {
        SdlScrollPhase::Began => ScrollPhase::Touched,
        SdlScrollPhase::Changed => ScrollPhase::Moved,
        SdlScrollPhase::Ended => ScrollPhase::Lifted,
        SdlScrollPhase::Momentum => ScrollPhase::Momentum,
    }
}

// A zero or negative size is what a minimizing window can report; the next
// real size follows, so it is dropped rather than treated as an error.
pub fn positive(width: i32, height: i32) -> bool {
    width > 0 && height > 0
}

pub fn translate(event: SdlEvent) -> Option<runtime_input::event> {
    match event {
        SdlEvent::MouseMotion { x, y, .. } => Some(Event::Pointer_moved((x, y))),
        SdlEvent::MouseButton { button: raw, down, x, y } => {
            let value = button(raw);
            Some(if down {
                Event::Pointer_pressed(value, (x, y))
            } else {
                Event::Pointer_released(value, (x, y))
            })
        }
        SdlEvent::MouseWheel { x, y, .. } => Some(Event::Scrolled((x, y))),
        SdlEvent::Scroll { x, y, phase, seconds } => Some(Event::Trackpad_scrolled {
            delta: (x, y),
            phase: scroll_phase(phase),
            time: seconds,
        }),
        SdlEvent::Key { key: raw, down, .. } => {
            let value = key(raw);
            Some(if down { Event::Key_pressed(value) } else { Event::Key_released(value) })
        }
        SdlEvent::TextInput(text) => Some(Event::Text_input(text)),
        SdlEvent::TextEditing { text, start, length } => Some(Event::Text_editing {
            text,
            start: i64::from(start),
            length: i64::from(length),
        }),
        SdlEvent::Window(WindowChange::Resized(width, height)) if positive(width, height) => {
            Some(Event::Resized((i64::from(width), i64::from(height))))
        }
        SdlEvent::Window(WindowChange::PixelSizeChanged(width, height))
            if positive(width, height) =>
        {
            Some(Event::Pixel_size_changed((i64::from(width), i64::from(height))))
        }
        SdlEvent::Window(WindowChange::Resized(..) | WindowChange::PixelSizeChanged(..)) => None,
        SdlEvent::Window(WindowChange::FocusLost) => Some(Event::Focus_lost),
        SdlEvent::Window(WindowChange::FocusGained) => Some(Event::Focus_gained),
        SdlEvent::Window(WindowChange::Shown | WindowChange::Restored) => {
            Some(Event::Visibility_changed(true))
        }
        // SDL sends an event when a window becomes occluded and none when it stops:
        // the runtime reads the window's state to see it end (Runtime.visible).
        SdlEvent::Window(
            WindowChange::Hidden | WindowChange::Minimized | WindowChange::Occluded,
        ) => Some(Event::Visibility_changed(false)),
        SdlEvent::Window(WindowChange::CloseRequested) => Some(Event::Closed),
        SdlEvent::Quit => Some(Event::Closed),
        SdlEvent::Pinch { phase: PinchPhase::Updated, scale } => Some(Event::Pinched(scale)),
        SdlEvent::Pinch { phase: PinchPhase::Began | PinchPhase::Ended, .. } => None,
        SdlEvent::Drop { change: DropChange::File(path), .. } => Some(Event::File_dropped(path)),
        SdlEvent::Drop { change: DropChange::Position, x, y } => {
            Some(Event::File_drag_moved((x, y)))
        }
        SdlEvent::Drop { change: DropChange::Complete, .. } => Some(Event::File_drag_ended),
        SdlEvent::Drop { change: DropChange::Begin, .. } => None,
        SdlEvent::Dialog { id, outcome: DialogOutcome::Chosen(paths) } => {
            Some(Event::File_dialog { id: i64::from(id), result: Ok(paths) })
        }
        SdlEvent::Dialog { id, outcome: DialogOutcome::Cancelled } => {
            Some(Event::File_dialog { id: i64::from(id), result: Ok(Vec::new()) })
        }
        SdlEvent::Dialog { id, outcome: DialogOutcome::Failed(message) } => {
            Some(Event::File_dialog { id: i64::from(id), result: Err(message) })
        }
    }
}

// SDL's positions, motion and window sizes are window coordinates; Rays reads logical points.
// Wheel notches and trackpad deltas are not positions and pass unchanged.
pub fn in_points(unit: f64, event: SdlEvent) -> SdlEvent {
    if unit == 1. {
        return event;
    }
    let p = |v: f64| v / unit;
    let size = |v: i32| std::cmp::max(1, (f64::from(v) / unit).round() as i32);
    match event {
        SdlEvent::MouseMotion { x, y, dx, dy } => {
            SdlEvent::MouseMotion { x: p(x), y: p(y), dx: p(dx), dy: p(dy) }
        }
        SdlEvent::MouseButton { button, down, x, y } => {
            SdlEvent::MouseButton { button, down, x: p(x), y: p(y) }
        }
        SdlEvent::Drop { change, x, y } => SdlEvent::Drop { change, x: p(x), y: p(y) },
        SdlEvent::Window(WindowChange::Resized(width, height)) if positive(width, height) => {
            SdlEvent::Window(WindowChange::Resized(size(width), size(height)))
        }
        event => event,
    }
}

pub fn push(value: &mut runtime_input::t, event: SdlEvent) -> Result<(), String> {
    let event = in_points(runtime_input::unit(value), event);
    match event {
        SdlEvent::MouseMotion { x, y, dx, dy } => {
            runtime_input::add_motion(value, dx, dy);
            runtime_input::push(value, Event::Pointer_moved((x, y)))
        }
        event => match translate(event) {
            None => Ok(()),
            Some(event) => runtime_input::push(value, event),
        },
    }
}

// One bad event never drops the rest of the batch; the first error is returned.
pub fn push_all(value: &mut runtime_input::t, events: Vec<SdlEvent>) -> Result<(), String> {
    events.into_iter().fold(Ok(()), |first, event| match (push(value, event), first) {
        (Err(error), Ok(())) => Err(error),
        (_, first) => first,
    })
}

pub fn pump(value: &mut runtime_input::t) -> Result<(), String> {
    match sdl3::event::poll_coalesced() {
        Err(error) => Err(error.to_string()),
        Ok(events) => push_all(value, events),
    }
}

pub fn wait(timeout_ms: i32) -> Result<bool, String> {
    sdl3::event::wait(timeout_ms).map_err(|error| error.to_string())
}
