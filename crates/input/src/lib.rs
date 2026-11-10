pub mod frame;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Key {
    KeyChar(char),
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    Space,
    Enter,
    Escape,
    Backspace,
    Tab,
    Shift,
    Ctrl,
    Alt,
    Meta,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    Home,
    End,
    PageUp,
    PageDown,
    Insert,
    Delete,
    Unknown(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MouseButton {
    LeftButton,
    RightButton,
    MiddleButton,
    MouseX1,
    MouseX2,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ScrollPhase {
    Touched,
    Moved,
    Lifted,
    Momentum,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
pub enum Event {
    Key_pressed(Key),
    Key_released(Key),
    Pointer_moved((f64, f64)),
    Pointer_pressed(MouseButton, (f64, f64)),
    Pointer_released(MouseButton, (f64, f64)),
    Pointer_cancelled(MouseButton),
    Scrolled((f64, f64)),
    Text_input(String),
    Text_editing { text: String, start: i64, length: i64 },
    File_dropped(String),
    File_drag_moved((f64, f64)),
    File_drag_ended,
    Pinched(f64),
    File_dialog { id: i64, result: Result<Vec<String>, String> },
    Dialog_opened(i64),
    Resized((i64, i64)),
    Focus_lost,
    Focus_gained,
    Closed,
    Pixel_size_changed((i64, i64)),
    Visibility_changed(bool),
    Trackpad_scrolled { delta: (f64, f64), phase: ScrollPhase, time: f64 },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub t: f64,
    pub dt: f64,
    pub frame: i64,
    pub tick: i64,
    pub size: (i64, i64),
    pub pointer: (f64, f64),
    pub buttons: Vec<MouseButton>,
    pub keys: Vec<Key>,
    pub events: Vec<Event>,
}

#[allow(non_camel_case_types)]
pub type key = Key;
#[allow(non_camel_case_types)]
pub type mouse_button = MouseButton;
#[allow(non_camel_case_types)]
pub type scroll_phase = ScrollPhase;
#[allow(non_camel_case_types)]
pub type event = Event;

pub fn at_time(time: f64) -> Frame {
    Frame {
        t: time,
        dt: 0.0,
        frame: 0,
        tick: 0,
        size: (0, 0),
        pointer: (0.0, 0.0),
        buttons: Vec::new(),
        keys: Vec::new(),
        events: Vec::new(),
    }
}

pub fn key_name(key: Key) -> String {
    match key {
        Key::KeyChar(c) => c.to_ascii_lowercase().to_string(),
        Key::ArrowUp => "up".into(),
        Key::ArrowDown => "down".into(),
        Key::ArrowLeft => "left".into(),
        Key::ArrowRight => "right".into(),
        Key::Space => "space".into(),
        Key::Enter => "enter".into(),
        Key::Escape => "escape".into(),
        Key::Backspace => "backspace".into(),
        Key::Tab => "tab".into(),
        Key::Shift => "shift".into(),
        Key::Ctrl => "ctrl".into(),
        Key::Alt => "alt".into(),
        Key::Meta => "meta".into(),
        Key::F1 => "f1".into(),
        Key::F2 => "f2".into(),
        Key::F3 => "f3".into(),
        Key::F4 => "f4".into(),
        Key::F5 => "f5".into(),
        Key::F6 => "f6".into(),
        Key::F7 => "f7".into(),
        Key::F8 => "f8".into(),
        Key::F9 => "f9".into(),
        Key::F10 => "f10".into(),
        Key::F11 => "f11".into(),
        Key::F12 => "f12".into(),
        Key::Home => "home".into(),
        Key::End => "end".into(),
        Key::PageUp => "pageup".into(),
        Key::PageDown => "pagedown".into(),
        Key::Insert => "insert".into(),
        Key::Delete => "delete".into(),
        Key::Unknown(n) => format!("unknown:{n}"),
    }
}

pub fn button_name(button: MouseButton) -> &'static str {
    match button {
        MouseButton::LeftButton => "left",
        MouseButton::RightButton => "right",
        MouseButton::MiddleButton => "middle",
        MouseButton::MouseX1 => "x1",
        MouseButton::MouseX2 => "x2",
    }
}

pub fn scroll_phase_name(phase: ScrollPhase) -> &'static str {
    match phase {
        ScrollPhase::Touched => "touched",
        ScrollPhase::Moved => "moved",
        ScrollPhase::Lifted => "lifted",
        ScrollPhase::Momentum => "momentum",
    }
}

fn add(key: Key, keys: &mut Vec<Key>) {
    if !keys.contains(&key) {
        keys.insert(0, key);
    }
}

pub fn keys_after(keys: &[Key], event: &Event) -> Vec<Key> {
    let mut keys = keys.to_vec();
    match event {
        Event::Key_pressed(key) => add(*key, &mut keys),
        Event::Key_released(key) => keys.retain(|held| held != key),
        Event::Focus_lost => keys.clear(),
        _ => {}
    }
    keys
}

pub fn keys_before(previous: &[Key], held: &[Key], events: &[Event]) -> Vec<Key> {
    let mut keys = held.to_vec();
    for event in events {
        match event {
            Event::Key_pressed(key) | Event::Key_released(key) => {
                if previous.contains(key) {
                    add(*key, &mut keys);
                } else {
                    keys.retain(|held| held != key);
                }
            }
            Event::Focus_lost => keys = previous.to_vec(),
            _ => {}
        }
    }
    keys
}

pub fn validate(v: &Frame) -> Result<(), String> {
    let event_valid = |event: &Event| match event {
        Event::Pointer_moved(p) | Event::Scrolled(p) | Event::File_drag_moved(p) => finite_pair(*p),
        Event::Pointer_pressed(_, p) | Event::Pointer_released(_, p) => finite_pair(*p),
        Event::Pinched(f) => f.is_finite(),
        Event::Trackpad_scrolled { delta, time, .. } => finite_pair(*delta) && time.is_finite(),
        Event::Text_editing { start, length, .. } => *start >= 0 && *length >= 0,
        Event::Resized((w, h)) => *w >= 0 && *h >= 0,
        _ => true,
    };
    if !(v.t.is_finite() && v.dt.is_finite() && finite_pair(v.pointer))
        || !v.events.iter().all(event_valid)
    {
        Err("Frame facts must be finite; event dimensions and text ranges must be non-negative.".into())
    } else if v.dt < 0.0 || v.frame < 0 || v.tick < 0 || v.size.0 < 0 || v.size.1 < 0 {
        Err("Frame step, index and dimensions must be non-negative.".into())
    } else {
        Ok(())
    }
}

fn finite_pair((x, y): (f64, f64)) -> bool {
    x.is_finite() && y.is_finite()
}

pub fn identity(v: &Frame) -> String {
    let mut out = String::new();
    put_f64(&mut out, v.t);
    put_f64(&mut out, v.dt);
    put_int(&mut out, v.frame);
    put_int(&mut out, v.tick);
    put_int(&mut out, v.size.0);
    put_int(&mut out, v.size.1);
    put_f64(&mut out, v.pointer.0);
    put_f64(&mut out, v.pointer.1);
    put_len(&mut out, v.buttons.len());
    for button in &v.buttons {
        put_button(&mut out, *button);
    }
    put_len(&mut out, v.keys.len());
    for key in &v.keys {
        put_key(&mut out, *key);
    }
    put_len(&mut out, v.events.len());
    for event in &v.events {
        put_event(&mut out, event);
    }
    out
}

const HEX: &[u8; 16] = b"0123456789abcdef";
fn put_byte(out: &mut String, byte: u8) {
    out.push(HEX[(byte >> 4) as usize] as char);
    out.push(HEX[(byte & 15) as usize] as char);
}
fn put_u64(out: &mut String, value: u64) {
    for byte in value.to_le_bytes() {
        put_byte(out, byte);
    }
}
fn put_int(out: &mut String, value: i64) {
    put_u64(out, value as u64);
}
fn put_f64(out: &mut String, value: f64) {
    put_u64(out, value.to_bits());
}
fn put_len(out: &mut String, len: usize) {
    put_u64(out, len as u64);
}
fn put_str(out: &mut String, value: &str) {
    put_len(out, value.len());
    for byte in value.bytes() {
        put_byte(out, byte);
    }
}
fn put_key(out: &mut String, key: Key) {
    let (tag, value) = match key {
        Key::KeyChar(c) => (0, c as u32 as u64),
        Key::ArrowUp => (1, 0),
        Key::ArrowDown => (2, 0),
        Key::ArrowLeft => (3, 0),
        Key::ArrowRight => (4, 0),
        Key::Space => (5, 0),
        Key::Enter => (6, 0),
        Key::Escape => (7, 0),
        Key::Backspace => (8, 0),
        Key::Tab => (9, 0),
        Key::Shift => (10, 0),
        Key::Ctrl => (11, 0),
        Key::Alt => (12, 0),
        Key::Meta => (13, 0),
        Key::F1 => (14, 0),
        Key::F2 => (15, 0),
        Key::F3 => (16, 0),
        Key::F4 => (17, 0),
        Key::F5 => (18, 0),
        Key::F6 => (19, 0),
        Key::F7 => (20, 0),
        Key::F8 => (21, 0),
        Key::F9 => (22, 0),
        Key::F10 => (23, 0),
        Key::F11 => (24, 0),
        Key::F12 => (25, 0),
        Key::Home => (26, 0),
        Key::End => (27, 0),
        Key::PageUp => (28, 0),
        Key::PageDown => (29, 0),
        Key::Insert => (30, 0),
        Key::Delete => (31, 0),
        Key::Unknown(n) => (32, n as i64 as u64),
    };
    put_byte(out, tag);
    put_u64(out, value);
}
fn put_button(out: &mut String, button: MouseButton) {
    put_byte(out, match button {
        MouseButton::LeftButton => 0,
        MouseButton::RightButton => 1,
        MouseButton::MiddleButton => 2,
        MouseButton::MouseX1 => 3,
        MouseButton::MouseX2 => 4,
    });
}
fn put_pair(out: &mut String, pair: (f64, f64)) {
    put_f64(out, pair.0);
    put_f64(out, pair.1);
}
fn put_event(out: &mut String, event: &Event) {
    match event {
        Event::Key_pressed(key) => { put_byte(out, 0); put_key(out, *key); }
        Event::Key_released(key) => { put_byte(out, 1); put_key(out, *key); }
        Event::Pointer_moved(p) => { put_byte(out, 2); put_pair(out, *p); }
        Event::Pointer_pressed(button, p) => { put_byte(out, 3); put_button(out, *button); put_pair(out, *p); }
        Event::Pointer_released(button, p) => { put_byte(out, 4); put_button(out, *button); put_pair(out, *p); }
        Event::Pointer_cancelled(button) => { put_byte(out, 5); put_button(out, *button); }
        Event::Scrolled(p) => { put_byte(out, 6); put_pair(out, *p); }
        Event::Text_input(text) => { put_byte(out, 7); put_str(out, text); }
        Event::Text_editing { text, start, length } => { put_byte(out, 8); put_str(out, text); put_int(out, *start); put_int(out, *length); }
        Event::File_dropped(path) => { put_byte(out, 9); put_str(out, path); }
        Event::File_drag_moved(p) => { put_byte(out, 10); put_pair(out, *p); }
        Event::File_drag_ended => put_byte(out, 11),
        Event::Pinched(f) => { put_byte(out, 12); put_f64(out, *f); }
        Event::File_dialog { id, result } => {
            put_byte(out, 13); put_int(out, *id);
            match result {
                Ok(paths) => { put_byte(out, 0); put_len(out, paths.len()); for path in paths { put_str(out, path); } }
                Err(error) => { put_byte(out, 1); put_str(out, error); }
            }
        }
        Event::Dialog_opened(id) => { put_byte(out, 14); put_int(out, *id); }
        Event::Resized((w, h)) => { put_byte(out, 15); put_int(out, *w); put_int(out, *h); }
        Event::Focus_lost => put_byte(out, 16),
        Event::Focus_gained => put_byte(out, 17),
        Event::Closed => put_byte(out, 18),
        Event::Pixel_size_changed((w, h)) => { put_byte(out, 19); put_int(out, *w); put_int(out, *h); }
        Event::Visibility_changed(visible) => { put_byte(out, 20); put_byte(out, u8::from(*visible)); }
        Event::Trackpad_scrolled { delta, phase, time } => {
            put_byte(out, 21); put_pair(out, *delta);
            put_byte(out, match phase { ScrollPhase::Touched => 0, ScrollPhase::Moved => 1, ScrollPhase::Lifted => 2, ScrollPhase::Momentum => 3 });
            put_f64(out, *time);
        }
    }
}

fn float_eq(a: f64, b: f64) -> bool {
    a.to_bits() == b.to_bits()
}
fn pair_eq(a: (f64, f64), b: (f64, f64)) -> bool {
    float_eq(a.0, b.0) && float_eq(a.1, b.1)
}
fn event_eq(a: &Event, b: &Event) -> bool {
    use Event::*;
    match (a, b) {
        (Key_pressed(a), Key_pressed(b)) | (Key_released(a), Key_released(b)) => a == b,
        (Pointer_moved(a), Pointer_moved(b)) | (Scrolled(a), Scrolled(b)) | (File_drag_moved(a), File_drag_moved(b)) => pair_eq(*a, *b),
        (Pointer_pressed(ab, ap), Pointer_pressed(bb, bp)) | (Pointer_released(ab, ap), Pointer_released(bb, bp)) => ab == bb && pair_eq(*ap, *bp),
        (Pointer_cancelled(a), Pointer_cancelled(b)) => a == b,
        (Text_input(a), Text_input(b)) | (File_dropped(a), File_dropped(b)) => a == b,
        (Text_editing { text: at, start: as_, length: al }, Text_editing { text: bt, start: bs, length: bl }) => at == bt && as_ == bs && al == bl,
        (File_drag_ended, File_drag_ended) | (Focus_lost, Focus_lost) | (Focus_gained, Focus_gained) | (Closed, Closed) => true,
        (Pinched(a), Pinched(b)) => float_eq(*a, *b),
        (File_dialog { id: ai, result: ar }, File_dialog { id: bi, result: br }) => ai == bi && ar == br,
        (Dialog_opened(a), Dialog_opened(b)) => a == b,
        (Resized(a), Resized(b)) | (Pixel_size_changed(a), Pixel_size_changed(b)) => a == b,
        (Visibility_changed(a), Visibility_changed(b)) => a == b,
        (Trackpad_scrolled { delta: ad, phase: ap, time: at }, Trackpad_scrolled { delta: bd, phase: bp, time: bt }) => pair_eq(*ad, *bd) && ap == bp && float_eq(*at, *bt),
        _ => false,
    }
}

pub fn equal(a: &Frame, b: &Frame) -> bool {
    float_eq(a.t, b.t)
        && float_eq(a.dt, b.dt)
        && a.frame == b.frame
        && a.tick == b.tick
        && a.size == b.size
        && pair_eq(a.pointer, b.pointer)
        && a.buttons == b.buttons
        && a.keys == b.keys
        && a.events.len() == b.events.len()
        && a.events.iter().zip(&b.events).all(|(a, b)| event_eq(a, b))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Platform {
    Apple,
    Other,
}
#[allow(non_camel_case_types)]
pub type platform = Platform;

pub fn platform_of_name(name: &str) -> Platform {
    match name {
        "macOS" | "Mac OS X" => Platform::Apple,
        _ => Platform::Other,
    }
}
pub fn primary(platform: Platform) -> Key {
    match platform {
        Platform::Apple => Key::Meta,
        Platform::Other => Key::Ctrl,
    }
}
pub fn command_held(platform: Platform, keys: &[Key]) -> bool {
    match platform {
        Platform::Apple => keys.contains(&Key::Meta) || keys.contains(&Key::Ctrl),
        Platform::Other => keys.contains(&Key::Ctrl),
    }
}
pub fn word_held(platform: Platform, keys: &[Key]) -> bool {
    keys.contains(&match platform { Platform::Apple => Key::Alt, Platform::Other => Key::Ctrl })
}

#[allow(non_camel_case_types)]
pub mod gate {
    use super::{Event, Frame, Key};

    pub type frame = Frame;

    #[derive(Clone, Debug)]
    pub struct Gate {
        pub prompt: bool,
        pub menu: bool,
        pub leader: bool,
        pub text_focus: bool,
        pub focus_on_picture: bool,
        pub pointer_on_picture: bool,
        pub window_key: bool,
        pub pressed: bool,
        pub previous: Frame,
        pub blocked: Vec<Key>,
    }

    fn pointer_event(event: &Event) -> bool {
        matches!(event,
            Event::Pointer_moved(_) | Event::Pointer_pressed(_, _) | Event::Pointer_released(_, _)
            | Event::Pointer_cancelled(_) | Event::Scrolled(_) | Event::Pinched(_)
            | Event::Trackpad_scrolled { .. })
    }
    fn key_event(event: &Event) -> bool {
        matches!(event, Event::Key_pressed(_) | Event::Key_released(_) | Event::Text_input(_) | Event::Text_editing { .. })
    }

    pub fn gate(g: &Gate, live: &Frame) -> (Frame, Vec<Key>) {
        let on_picture = !g.prompt && !g.menu && g.pointer_on_picture && g.window_key;
        let mut live = live.clone();
        if !( !g.previous.buttons.is_empty() || ((live.buttons.is_empty() || g.pressed) && on_picture) ) {
            live.pointer = g.previous.pointer;
            live.buttons.clear();
            live.events.retain(|event| !pointer_event(event));
        }
        if g.prompt || g.menu || g.leader || g.text_focus || !g.focus_on_picture {
            let blocked = std::mem::take(&mut live.keys);
            live.events.retain(|event| !key_event(event));
            (live, blocked)
        } else {
            let blocked: Vec<Key> = live.keys.iter().copied().filter(|key| g.blocked.contains(key)).collect();
            live.keys.retain(|key| !blocked.contains(key));
            live.events.retain(|event| match event {
                Event::Key_pressed(key) | Event::Key_released(key) => !blocked.contains(key),
                _ => true,
            });
            (live, blocked)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn input(keys: Vec<Key>, buttons: Vec<MouseButton>, pointer: (f64, f64), events: Vec<Event>) -> Frame {
        Frame { t: 0.0, dt: 1.0 / 60.0, frame: 0, tick: 0, size: (100, 100), pointer, buttons, keys, events }
    }
    fn open() -> gate::Gate {
        gate::Gate {
            prompt: false, menu: false, leader: false, text_focus: false,
            focus_on_picture: true, pointer_on_picture: true, window_key: true,
            pressed: false, previous: input(vec![], vec![], (10.0, 10.0), vec![]), blocked: vec![],
        }
    }
    fn presses(key: Key) -> Frame {
        input(vec![key], vec![], (10.0, 10.0), vec![Event::Key_pressed(key), Event::Text_input(key_name(key))])
    }
    fn sees(frame: &Frame, key: Key) -> bool {
        frame.keys.contains(&key) || frame.events.contains(&Event::Key_pressed(key))
    }

    #[test]
    fn gate_behavior() {
        let open = open();
        for (what, gate, reaches) in [
            ("a viewer or canvas in focus", open.clone(), true),
            ("the graph pane in focus", gate::Gate { focus_on_picture: false, ..open.clone() }, false),
            ("the text pane in focus", gate::Gate { focus_on_picture: false, text_focus: true, ..open.clone() }, false),
            ("a leader half typed", gate::Gate { leader: true, ..open.clone() }, false),
            ("a prompt", gate::Gate { prompt: true, ..open.clone() }, false),
            ("a menu", gate::Gate { menu: true, ..open.clone() }, false),
            ("a text field in a canvas in focus", gate::Gate { text_focus: true, ..open.clone() }, false),
        ] {
            for key in [Key::Escape, Key::KeyChar('q')] {
                let (seen, _) = gate::gate(&gate, &presses(key));
                assert_eq!(sees(&seen, key), reaches, "{}: {} {} the sketch", what, key_name(key), if reaches { "did not reach" } else { "reached" });
                assert_eq!(seen.events.iter().any(|event| matches!(event, Event::Text_input(_))), reaches, "{}: typed text and the key disagree", what);
            }
        }

        let closing = gate::Gate { leader: true, ..open.clone() };
        let (seen, blocked) = gate::gate(&closing, &presses(Key::Escape));
        assert!(!sees(&seen, Key::Escape) && blocked == vec![Key::Escape], "the leader's Esc reached the sketch or was not remembered");
        let held = input(vec![Key::Escape], vec![], (10.0, 10.0), vec![]);
        let (seen, blocked) = gate::gate(&gate::Gate { blocked: blocked, ..open.clone() }, &held);
        assert!(!sees(&seen, Key::Escape) && blocked == vec![Key::Escape], "a held Esc leaked when the keyboard came back");
        let (seen, blocked) = gate::gate(
            &gate::Gate { blocked: blocked, ..open.clone() },
            &input(vec![Key::Escape, Key::KeyChar('q')], vec![], (10.0, 10.0), vec![Event::Key_pressed(Key::KeyChar('q'))]),
        );
        assert!(!sees(&seen, Key::Escape) && sees(&seen, Key::KeyChar('q')) && blocked == vec![Key::Escape], "another key was hidden with the held one");
        let (seen, blocked) = gate::gate(&gate::Gate { blocked: blocked, ..open.clone() }, &input(vec![], vec![], (10.0, 10.0), vec![Event::Key_released(Key::Escape)]));
        assert!(blocked.is_empty() && seen.keys.is_empty(), "a released key stayed hidden");
        let (seen, _) = gate::gate(&gate::Gate { blocked, ..open.clone() }, &presses(Key::Escape));
        assert!(sees(&seen, Key::Escape), "a fresh press after the release did not reach the sketch");

        let moved = input(vec![], vec![], (50.0, 50.0), vec![Event::Pointer_moved((50.0, 50.0))]);
        let away = gate::Gate { pointer_on_picture: false, previous: input(vec![], vec![], (5.0, 5.0), vec![]), ..open };
        let (seen, _) = gate::gate(&away, &moved);
        assert!(seen.pointer == (5.0, 5.0) && seen.events.is_empty(), "the pointer left the picture and the sketch followed");
        let (seen, _) = gate::gate(&gate::Gate { pointer_on_picture: true, ..away.clone() }, &moved);
        assert!(seen.pointer == (50.0, 50.0) && !seen.events.is_empty(), "the pointer over the picture was hidden");
        let (seen, _) = gate::gate(&gate::Gate { window_key: false, pointer_on_picture: true, ..away.clone() }, &moved);
        assert!(seen.pointer == (5.0, 5.0) && seen.events.is_empty(), "a pointer over an unfocused window reached the sketch");
        let (seen, _) = gate::gate(&gate::Gate { prompt: true, pointer_on_picture: true, ..away.clone() }, &moved);
        assert!(seen.pointer == (5.0, 5.0), "a prompt did not hold the pointer");
        let drag = input(vec![], vec![MouseButton::LeftButton], (50.0, 50.0), vec![Event::Pointer_moved((50.0, 50.0))]);
        let (seen, _) = gate::gate(&gate::Gate { previous: input(vec![], vec![MouseButton::LeftButton], (5.0, 5.0), vec![]), ..away.clone() }, &drag);
        assert!(seen.pointer == (50.0, 50.0), "a drag that began on the picture was cut off");
        let (seen, _) = gate::gate(&away, &drag);
        assert!(seen.buttons.is_empty() && seen.pointer == (5.0, 5.0), "a drag that began elsewhere became the sketch's");
        let (seen, _) = gate::gate(&gate::Gate { pointer_on_picture: true, pressed: true, ..away }, &drag);
        assert!(!seen.buttons.is_empty(), "a press on the picture was hidden");
    }
    
    #[test]
    fn time_only_frame_has_empty_input_facts() {
        let frame = at_time(12.5);
        assert_eq!(frame.t, 12.5);
        assert_eq!(frame.dt, 0.0);
        assert_eq!((frame.frame, frame.tick, frame.size.0, frame.size.1), (0, 0, 0, 0));
        assert_eq!(frame.pointer, (0.0, 0.0));
        assert!(frame.buttons.is_empty() && frame.keys.is_empty() && frame.events.is_empty());
    }

    #[test]
    fn key_snapshots_reconstruct_ordered_frame_transitions() {
        let a = Key::KeyChar('a');
        let b = Key::KeyChar('b');
        let previous = [a];
        let held = [b];
        let events = [Event::Key_released(a), Event::Key_pressed(b)];
        let before = keys_before(&previous, &held, &events);
        assert_eq!(before, vec![a]);
        let after = events.iter().fold(before, |keys, event| keys_after(&keys, event));
        assert_eq!(after, held.to_vec());
    }

    #[test]
    fn key_state_reconstruction() {
        let a = Key::KeyChar('a');
        let b = Key::KeyChar('b');
        let c = Key::KeyChar('c');
        assert_eq!(keys_after(&[a, b], &Event::Key_pressed(b)), vec![a, b]);
        assert_eq!(keys_after(&[a, b], &Event::Key_pressed(c)), vec![c, a, b]);
        assert_eq!(keys_after(&[a, b], &Event::Key_released(a)), vec![b]);
        assert!(keys_after(&[a, b], &Event::Focus_lost).is_empty());

        assert_eq!(keys_before(&[a], &[b], &[Event::Key_pressed(a)]), vec![a, b]);
        assert_eq!(keys_before(&[], &[a, b], &[Event::Key_released(a)]), vec![b]);
        assert_eq!(keys_before(&[a], &[b], &[Event::Focus_lost]), vec![a]);
    }

    #[test]
    fn canonical_names_and_platform_modifiers() {
        let names = [
            (Key::KeyChar('Q'), "q"),
            (Key::ArrowUp, "up"), (Key::ArrowDown, "down"),
            (Key::ArrowLeft, "left"), (Key::ArrowRight, "right"),
            (Key::Space, "space"), (Key::Enter, "enter"), (Key::Escape, "escape"),
            (Key::Backspace, "backspace"), (Key::Tab, "tab"), (Key::Shift, "shift"),
            (Key::Ctrl, "ctrl"), (Key::Alt, "alt"), (Key::Meta, "meta"),
            (Key::F1, "f1"), (Key::F2, "f2"), (Key::F3, "f3"), (Key::F4, "f4"),
            (Key::F5, "f5"), (Key::F6, "f6"), (Key::F7, "f7"), (Key::F8, "f8"),
            (Key::F9, "f9"), (Key::F10, "f10"), (Key::F11, "f11"), (Key::F12, "f12"),
            (Key::Home, "home"), (Key::End, "end"), (Key::PageUp, "pageup"),
            (Key::PageDown, "pagedown"), (Key::Insert, "insert"), (Key::Delete, "delete"),
            (Key::Unknown(-7), "unknown:-7"),
        ];
        for (key, name) in names {
            assert_eq!(key_name(key), name);
        }
        assert_eq!(
            [MouseButton::LeftButton, MouseButton::RightButton, MouseButton::MiddleButton, MouseButton::MouseX1, MouseButton::MouseX2]
                .map(button_name),
            ["left", "right", "middle", "x1", "x2"]
        );
        assert_eq!(
            [ScrollPhase::Touched, ScrollPhase::Moved, ScrollPhase::Lifted, ScrollPhase::Momentum]
                .map(scroll_phase_name),
            ["touched", "moved", "lifted", "momentum"]
        );

        let apple = platform_of_name("macOS");
        assert_eq!(apple, Platform::Apple);
        assert_eq!(platform_of_name("Mac OS X"), Platform::Apple);
        assert_eq!(platform_of_name("macos"), Platform::Other);
        assert_eq!(primary(apple), Key::Meta);
        assert_eq!(primary(Platform::Other), Key::Ctrl);
        assert!(command_held(apple, &[Key::Meta]));
        assert!(command_held(apple, &[Key::Ctrl]));
        assert!(!command_held(apple, &[Key::Alt]));
        assert!(command_held(Platform::Other, &[Key::Ctrl]));
        assert!(!command_held(Platform::Other, &[Key::Meta]));
        assert!(word_held(apple, &[Key::Alt]));
        assert!(!word_held(apple, &[Key::Ctrl]));
        assert!(word_held(Platform::Other, &[Key::Ctrl]));
    }

    #[test]
    fn frame_validation_boundaries() {
        let mut frame = at_time(0.0);
        assert_eq!(validate(&frame), Ok(()));
        frame.size = (0, 0);
        frame.events = vec![
            Event::Resized((0, 0)),
            Event::Text_editing { text: String::new(), start: 0, length: 0 },
            Event::Pixel_size_changed((-1, -1)),
        ];
        assert_eq!(validate(&frame), Ok(()));

        let facts_error = "Frame facts must be finite; event dimensions and text ranges must be non-negative.";
        let bounds_error = "Frame step, index and dimensions must be non-negative.";
        let mut invalid = at_time(0.0);
        invalid.t = f64::NAN;
        assert_eq!(validate(&invalid), Err(facts_error.into()));
        invalid = at_time(0.0);
        invalid.dt = f64::INFINITY;
        assert_eq!(validate(&invalid), Err(facts_error.into()));
        invalid = at_time(0.0);
        invalid.pointer = (0.0, f64::NEG_INFINITY);
        assert_eq!(validate(&invalid), Err(facts_error.into()));
        invalid = at_time(0.0);
        invalid.events = vec![Event::Pointer_moved((f64::NAN, 0.0))];
        assert_eq!(validate(&invalid), Err(facts_error.into()));
        invalid.events = vec![Event::Text_editing { text: String::new(), start: -1, length: 0 }];
        assert_eq!(validate(&invalid), Err(facts_error.into()));
        invalid.events = vec![Event::Resized((0, -1))];
        assert_eq!(validate(&invalid), Err(facts_error.into()));

        invalid = at_time(0.0);
        invalid.dt = -0.1;
        assert_eq!(validate(&invalid), Err(bounds_error.into()));
        invalid = at_time(0.0);
        invalid.frame = -1;
        assert_eq!(validate(&invalid), Err(bounds_error.into()));
        invalid = at_time(0.0);
        invalid.tick = -1;
        assert_eq!(validate(&invalid), Err(bounds_error.into()));
        invalid = at_time(0.0);
        invalid.size = (-1, 0);
        assert_eq!(validate(&invalid), Err(bounds_error.into()));
    }

    #[test]
    fn exact_frame_identity_and_event_payloads() {
        let mut positive_zero = at_time(0.0);
        let mut negative_zero = positive_zero.clone();
        negative_zero.pointer.0 = -0.0;
        assert!(!equal(&positive_zero, &negative_zero));
        assert_ne!(identity(&positive_zero), identity(&negative_zero));

        let nan_a = f64::from_bits(0x7ff8_0000_0000_0001);
        let nan_b = f64::from_bits(0x7ff8_0000_0000_0002);
        positive_zero.t = nan_a;
        negative_zero = positive_zero.clone();
        assert!(equal(&positive_zero, &negative_zero));
        assert_eq!(identity(&positive_zero), identity(&negative_zero));
        negative_zero.t = nan_b;
        assert!(!equal(&positive_zero, &negative_zero));
        assert_ne!(identity(&positive_zero), identity(&negative_zero));

        let mut event_a = at_time(0.0);
        event_a.events = vec![Event::Pinched(-0.0)];
        let mut event_b = event_a.clone();
        event_b.events[0] = Event::Pinched(0.0);
        assert!(!equal(&event_a, &event_b));
        assert_ne!(identity(&event_a), identity(&event_b));
        event_a.events[0] = Event::Pinched(nan_a);
        event_b.events[0] = Event::Pinched(nan_a);
        assert!(equal(&event_a, &event_b));
        assert_eq!(identity(&event_a), identity(&event_b));
        event_b.events[0] = Event::Pinched(nan_b);
        assert!(!equal(&event_a, &event_b));
        assert_ne!(identity(&event_a), identity(&event_b));

        let payload_changes = [
            (Event::Key_pressed(Key::KeyChar('a')), Event::Key_pressed(Key::KeyChar('b'))),
            (Event::Pointer_moved((1.0, 2.0)), Event::Pointer_moved((1.0, 3.0))),
            (Event::Pointer_pressed(MouseButton::LeftButton, (1.0, 2.0)), Event::Pointer_pressed(MouseButton::RightButton, (1.0, 2.0))),
            (Event::Text_input("first".into()), Event::Text_input("other".into())),
            (Event::Text_editing { text: "x".into(), start: 1, length: 2 }, Event::Text_editing { text: "x".into(), start: 2, length: 2 }),
            (Event::File_dropped("a".into()), Event::File_dropped("b".into())),
            (Event::Pinched(1.0), Event::Pinched(2.0)),
            (Event::File_dialog { id: 4, result: Ok(vec!["a".into()]) }, Event::File_dialog { id: 4, result: Ok(vec!["b".into()]) }),
            (Event::Dialog_opened(4), Event::Dialog_opened(5)),
            (Event::Resized((10, 20)), Event::Resized((10, 21))),
            (Event::Visibility_changed(false), Event::Visibility_changed(true)),
            (Event::Trackpad_scrolled { delta: (1.0, 2.0), phase: ScrollPhase::Moved, time: 3.0 }, Event::Trackpad_scrolled { delta: (1.0, 2.0), phase: ScrollPhase::Lifted, time: 3.0 }),
        ];
        for (before, after) in payload_changes {
            let mut a = at_time(0.0);
            a.events = vec![before];
            let mut b = at_time(0.0);
            b.events = vec![after];
            assert!(!equal(&a, &b));
            assert_ne!(identity(&a), identity(&b));
        }

        let mut ordered = at_time(0.0);
        ordered.events = vec![Event::Key_pressed(Key::Escape), Event::Text_input("x".into())];
        let mut reversed = ordered.clone();
        reversed.events.reverse();
        assert!(!equal(&ordered, &reversed));
        assert_ne!(identity(&ordered), identity(&reversed));
    }
}
