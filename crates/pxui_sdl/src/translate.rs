use input::{Event, Key, MouseButton, ScrollPhase};
use sdl3::{
    DialogOutcome, DropChange, Event as SdlEvent, Key as SdlKey, MouseButton as SdlMouseButton,
    PinchPhase, ScrollPhase as SdlScrollPhase, WindowChange,
};

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

pub fn translate(event: SdlEvent) -> Option<Event> {
    match event {
        SdlEvent::MouseMotion { x, y, .. } => Some(Event::PointerMoved((x, y))),
        SdlEvent::MouseButton { button: raw, down, x, y } => {
            let value = button(raw);
            Some(if down {
                Event::PointerPressed(value, (x, y))
            } else {
                Event::PointerReleased(value, (x, y))
            })
        }
        SdlEvent::MouseWheel { x, y, .. } => Some(Event::Scrolled((x, y))),
        SdlEvent::Scroll { x, y, phase, seconds } => Some(Event::TrackpadScrolled {
            delta: (x, y),
            phase: scroll_phase(phase),
            time: seconds,
        }),
        SdlEvent::Key { key: raw, down, .. } => {
            let value = key(raw);
            Some(if down { Event::KeyPressed(value) } else { Event::KeyReleased(value) })
        }
        SdlEvent::TextInput(text) => Some(Event::TextInput(text)),
        SdlEvent::TextEditing { text, start, length } => Some(Event::TextEditing {
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
            Some(Event::PixelSizeChanged((i64::from(width), i64::from(height))))
        }
        SdlEvent::Window(WindowChange::Resized(..) | WindowChange::PixelSizeChanged(..)) => None,
        SdlEvent::Window(WindowChange::FocusLost) => Some(Event::FocusLost),
        SdlEvent::Window(WindowChange::FocusGained) => Some(Event::FocusGained),
        SdlEvent::Window(WindowChange::Shown | WindowChange::Restored) => {
            Some(Event::VisibilityChanged(true))
        }
        // SDL sends an event when a window becomes occluded and none when it stops:
        // the host reads the window's state to see it end (Window::visible).
        SdlEvent::Window(
            WindowChange::Hidden | WindowChange::Minimized | WindowChange::Occluded,
        ) => Some(Event::VisibilityChanged(false)),
        SdlEvent::Window(WindowChange::CloseRequested) => Some(Event::Closed),
        SdlEvent::Quit => Some(Event::Closed),
        SdlEvent::Pinch { phase: PinchPhase::Updated, scale } => Some(Event::Pinched(scale)),
        SdlEvent::Pinch { phase: PinchPhase::Began | PinchPhase::Ended, .. } => None,
        SdlEvent::Drop { change: DropChange::File(path), .. } => Some(Event::FileDropped(path)),
        SdlEvent::Drop { change: DropChange::Position, x, y } => {
            Some(Event::FileDragMoved((x, y)))
        }
        SdlEvent::Drop { change: DropChange::Complete, .. } => Some(Event::FileDragEnded),
        SdlEvent::Drop { change: DropChange::Begin, .. } => None,
        SdlEvent::Dialog { id, outcome: DialogOutcome::Chosen(paths) } => {
            Some(Event::FileDialog { id: i64::from(id), result: Ok(paths) })
        }
        SdlEvent::Dialog { id, outcome: DialogOutcome::Cancelled } => {
            Some(Event::FileDialog { id: i64::from(id), result: Ok(Vec::new()) })
        }
        SdlEvent::Dialog { id, outcome: DialogOutcome::Failed(message) } => {
            Some(Event::FileDialog { id: i64::from(id), result: Err(message) })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translation_table() {
        let press = SdlEvent::MouseButton { button: SdlMouseButton::Right, down: true, x: 3., y: 4. };
        assert_eq!(translate(press), Some(Event::PointerPressed(MouseButton::RightButton, (3., 4.))));
        let key = SdlEvent::Key { key: SdlKey::Char('a'), modifiers: Vec::new(), down: false, repeat: false };
        assert_eq!(translate(key), Some(Event::KeyReleased(Key::KeyChar('a'))));
        assert_eq!(translate(SdlEvent::Quit), Some(Event::Closed));
        assert_eq!(translate(SdlEvent::Window(WindowChange::Hidden)), Some(Event::VisibilityChanged(false)));
        // a minimizing window's zero size is dropped, a real one arrives in logical points
        assert_eq!(translate(SdlEvent::Window(WindowChange::Resized(0, 5))), None);
        let resized = in_points(2., SdlEvent::Window(WindowChange::Resized(40, 30)));
        assert_eq!(translate(resized), Some(Event::Resized((20, 15))));
        let moved = in_points(2., SdlEvent::MouseMotion { x: 8., y: 6., dx: 2., dy: 2. });
        assert_eq!(translate(moved), Some(Event::PointerMoved((4., 3.))));
    }
}
