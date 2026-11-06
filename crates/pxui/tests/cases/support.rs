//! Helpers shared by the PXUI interaction tests (the OCaml test_ui.ml frame builders).
use input::{Event, Key, MouseButton, Platform};
use pxui::ui::*;
use rays::frame;

pub fn font_path() -> String {
    format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"))
}

/// A UI over the golden face: the pointer positions in the text tests assume its advance.
pub fn new_ui(font_size: Option<i64>) -> Ui {
    Ui::create(Platform::Apple, font_size, Some(font_path()))
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
    Event::Pointer_pressed(MouseButton::LeftButton, pointer(p))
}

pub fn release(p: (i64, i64)) -> Event {
    Event::Pointer_released(MouseButton::LeftButton, pointer(p))
}

pub fn move_to(p: (i64, i64)) -> Event {
    Event::Pointer_moved(pointer(p))
}

pub fn key_down(k: Key) -> Event {
    Event::Key_pressed(k)
}

pub fn kc(c: char) -> Key {
    Key::KeyChar(c)
}

/// The OCaml test_ui `frame`: a 320x240 frame at `time` with the given scale, keys and events.
pub fn frame_at(keys: Vec<Key>, scale: f64, time: f64, events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(time);
    input.dt = 1.0 / 60.0;
    input.size = (320, 240);
    input.pointer = (0.0, 0.0);
    input.keys = keys;
    input.events = events;
    frame::t { input, pixel_scale: (scale, scale), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

pub fn simple_frame(events: Vec<Event>) -> frame::t {
    frame_at(Vec::new(), 1.0, 0.0, events)
}
