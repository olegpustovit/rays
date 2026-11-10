use input::{Event, Key, MouseButton, Platform};
use pxui::ui::*;
use input::frame;

/// The golden face: the pointer positions in the text tests assume its advance.
pub const DEPARTURE_MONO: &[u8] = include_bytes!("../../../../assets/fonts/DepartureMono-Regular.otf");

/// A text engine over the golden face alone.
pub fn golden_text() -> pxui::text::Text {
    pxui::text::Text::new(&[DEPARTURE_MONO])
}

/// A UI over the golden face.
pub fn new_ui(font_size: Option<i64>) -> Ui {
    Ui::create(Platform::Apple, font_size, golden_text())
}

/// The host's clipboard, in the test process.
pub mod clipboard {
    use std::cell::RefCell;
    thread_local! { static TEXT: RefCell<String> = const { RefCell::new(String::new()) }; }
    pub fn get_text() -> Result<String, String> {
        Ok(TEXT.with(|t| t.borrow().clone()))
    }
    pub fn set_text(text: &str) -> Result<(), String> {
        TEXT.with(|t| *t.borrow_mut() = text.to_owned());
        Ok(())
    }
}

/// Run a frame as a host does: the clipboard text the last frame asked for arrives first as
/// `Event::Paste`, the clipboard write the frame asked for applies after.
pub fn host_frame(ui: &mut Ui, frame: &frame::Frame, f: &mut dyn FnMut(&mut Ui)) {
    if ui.output().platform.paste {
        let mut with = frame.clone();
        with.input.events.insert(0, Event::Paste(clipboard::get_text().unwrap()));
        ui.frame(&with, f);
    } else {
        ui.frame(frame, f);
    }
    if let Some(text) = &ui.output().platform.clipboard {
        clipboard::set_text(text).unwrap();
    }
}

pub fn fail(message: &str) -> ! {
    panic!("{message}");
}

pub fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

pub fn pointer(p: (i64, i64)) -> (f64, f64) {
    (p.0 as f64, p.1 as f64)
}

pub fn press(p: (i64, i64)) -> Event {
    Event::PointerPressed(MouseButton::LeftButton, pointer(p))
}

pub fn release(p: (i64, i64)) -> Event {
    Event::PointerReleased(MouseButton::LeftButton, pointer(p))
}

pub fn move_to(p: (i64, i64)) -> Event {
    Event::PointerMoved(pointer(p))
}

pub fn key_down(k: Key) -> Event {
    Event::KeyPressed(k)
}

pub fn kc(c: char) -> Key {
    Key::KeyChar(c)
}

pub fn frame_at(keys: Vec<Key>, scale: f64, time: f64, events: Vec<Event>) -> frame::Frame {
    let mut input = input::at_time(time);
    input.dt = 1.0 / 60.0;
    input.size = (320, 240);
    input.pointer = (0.0, 0.0);
    input.keys = keys;
    input.events = events;
    frame::Frame { input, pixel_scale: (scale, scale), fps: 60.0, mouse_delta: (0.0, 0.0) }
}
