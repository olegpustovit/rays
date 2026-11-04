//! Per-sketch native input source and the held state folded into each frame.
use input::{Event, Key, MouseButton};
use runtime_input as ri;
use runtime_input::runtime_input_sdl3 as sdl;

/// OCaml's polymorphic `compare` on `Input.key`: constant constructors (ArrowUp .. Delete) in
/// declaration order first, then `KeyChar c`, then `Unknown n`.
fn key_rank(key: &Key) -> (i64, i64) {
    let index = match key {
        Key::ArrowUp => 0,
        Key::ArrowDown => 1,
        Key::ArrowLeft => 2,
        Key::ArrowRight => 3,
        Key::Space => 4,
        Key::Enter => 5,
        Key::Escape => 6,
        Key::Backspace => 7,
        Key::Tab => 8,
        Key::Shift => 9,
        Key::Ctrl => 10,
        Key::Alt => 11,
        Key::Meta => 12,
        Key::F1 => 13,
        Key::F2 => 14,
        Key::F3 => 15,
        Key::F4 => 16,
        Key::F5 => 17,
        Key::F6 => 18,
        Key::F7 => 19,
        Key::F8 => 20,
        Key::F9 => 21,
        Key::F10 => 22,
        Key::F11 => 23,
        Key::F12 => 24,
        Key::Home => 25,
        Key::End => 26,
        Key::PageUp => 27,
        Key::PageDown => 28,
        Key::Insert => 29,
        Key::Delete => 30,
        Key::KeyChar(c) => return (1, *c as i64),
        Key::Unknown(n) => return (2, *n),
    };
    (0, index)
}

fn button_rank(button: &MouseButton) -> i64 {
    match button {
        MouseButton::LeftButton => 0,
        MouseButton::RightButton => 1,
        MouseButton::MiddleButton => 2,
        MouseButton::MouseX1 => 3,
        MouseButton::MouseX2 => 4,
    }
}

pub struct t {
    source: ri::t,
    /// Sorted, distinct (OCaml's `Set`).
    pressed_keys: Vec<Key>,
    pressed_buttons: Vec<MouseButton>,
    mouse: (f64, f64),
    delta: (f64, f64),
}

pub fn create() -> Result<t, String> {
    let source = ri::create(4096, 1, 1)?;
    Ok(t { source, pressed_keys: Vec::new(), pressed_buttons: Vec::new(), mouse: (0., 0.), delta: (0., 0.) })
}

fn move_to(state: &mut t, x: f64, y: f64) {
    let (cx, cy) = state.mouse;
    let (dx, dy) = state.delta;
    state.delta = (dx + x - cx, dy + y - cy);
    state.mouse = (x, y);
}

fn add_key(keys: &mut Vec<Key>, key: Key) {
    let rank = key_rank(&key);
    match keys.binary_search_by(|k| key_rank(k).cmp(&rank)) {
        Ok(_) => {}
        Err(at) => keys.insert(at, key),
    }
}

fn remove_key(keys: &mut Vec<Key>, key: &Key) {
    let rank = key_rank(key);
    if let Ok(at) = keys.binary_search_by(|k| key_rank(k).cmp(&rank)) {
        keys.remove(at);
    }
}

fn add_button(buttons: &mut Vec<MouseButton>, button: MouseButton) {
    let rank = button_rank(&button);
    match buttons.binary_search_by(|b| button_rank(b).cmp(&rank)) {
        Ok(_) => {}
        Err(at) => buttons.insert(at, button),
    }
}

fn remove_button(buttons: &mut Vec<MouseButton>, button: &MouseButton) {
    let rank = button_rank(button);
    if let Ok(at) = buttons.binary_search_by(|b| button_rank(b).cmp(&rank)) {
        buttons.remove(at);
    }
}

pub fn apply(state: &mut t, event: &Event) {
    match event {
        Event::Key_pressed(key) => add_key(&mut state.pressed_keys, *key),
        Event::Key_released(key) => remove_key(&mut state.pressed_keys, key),
        Event::Pointer_moved((x, y)) => move_to(state, *x, *y),
        Event::Pointer_pressed(button, (x, y)) => {
            move_to(state, *x, *y);
            add_button(&mut state.pressed_buttons, *button);
        }
        Event::Pointer_released(button, (x, y)) => {
            move_to(state, *x, *y);
            remove_button(&mut state.pressed_buttons, button);
        }
        Event::Pointer_cancelled(button) => remove_button(&mut state.pressed_buttons, button),
        Event::Focus_lost => {
            state.pressed_keys.clear();
            state.pressed_buttons.clear();
        }
        _ => {}
    }
}

pub fn configure(state: &t, logical_width: i64, logical_height: i64) -> Result<(), String> {
    ri::set_extent(&state.source, logical_width, logical_height)
}

pub fn set_relative(state: &mut t, enabled: bool) {
    ri::set_relative(&mut state.source, enabled)
}

pub fn set_unit(state: &mut t, unit: f64) {
    ri::set_unit(&mut state.source, unit)
}

pub fn set_visible(state: &mut t, visible: bool) {
    let _ = ri::push(&mut state.source, Event::Visibility_changed(visible));
}

pub struct polled {
    pub events: Vec<Event>,
    pub window_changed: bool,
    pub visible: bool,
}

pub fn wait(_state: &t, timeout_ms: i32) -> bool {
    sdl::wait(timeout_ms).unwrap_or(false)
}

pub fn poll(state: &mut t) -> Result<polled, String> {
    state.delta = (0., 0.);
    ri::begin_frame(&mut state.source);
    let pumped = sdl::pump(&mut state.source);
    let native = ri::drain(&mut state.source);
    let window_changed = native.iter().any(|e| matches!(e, Event::Resized(_) | Event::Pixel_size_changed(_)));
    let mut events = Vec::new();
    for event in native {
        if matches!(event, Event::Pixel_size_changed(_) | Event::Visibility_changed(_)) {
            continue;
        }
        apply(state, &event);
        events.push(event);
    }
    if ri::relative(&state.source) {
        state.delta = ri::snapshot(&state.source).mouse_delta;
    }
    pumped.map(|()| polled { events, window_changed, visible: ri::snapshot(&state.source).visible })
}

pub fn mouse(state: &t) -> (f64, f64) {
    state.mouse
}

pub fn mouse_delta(state: &t) -> (f64, f64) {
    state.delta
}

pub fn keys(state: &t) -> Vec<Key> {
    state.pressed_keys.clone()
}

pub fn buttons(state: &t) -> Vec<MouseButton> {
    state.pressed_buttons.clone()
}
