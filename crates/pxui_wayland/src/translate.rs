//! The pure half of the host: Wayland's raw input facts become `input` events with pxui_sdl's conventions
//! (logical points, SDL's key naming, wheel y positive away from the user, trackpad deltas in points).
use input::{Event, Key, MouseButton, ScrollPhase};

/// The key an xkb event names. `sym` is the key's unshifted keysym and `utf32` its character (0 if none), as SDL
/// names a key by its unshifted keycode; `evdev` is the scancode, the fallback for letters and digits on non-Latin
/// layouts.
pub fn key(evdev: u32, sym: u32, utf32: u32) -> Key {
    match sym {
        0xff52 => return Key::ArrowUp,
        0xff54 => return Key::ArrowDown,
        0xff51 => return Key::ArrowLeft,
        0xff53 => return Key::ArrowRight,
        0x0020 => return Key::Space,
        0xff0d | 0xff8d => return Key::Enter,
        0xff1b => return Key::Escape,
        0xff08 => return Key::Backspace,
        0xff09 | 0xfe20 => return Key::Tab,
        0xffe1 | 0xffe2 => return Key::Shift,
        0xffe3 | 0xffe4 => return Key::Ctrl,
        // AltGr is ISO_Level3_Shift on most layouts: SDL names the right Alt key Alt too.
        0xffe9 | 0xffea | 0xfe03 => return Key::Alt,
        0xffeb | 0xffec | 0xffe7 | 0xffe8 => return Key::Meta,
        0xffbe => return Key::F1,
        0xffbf => return Key::F2,
        0xffc0 => return Key::F3,
        0xffc1 => return Key::F4,
        0xffc2 => return Key::F5,
        0xffc3 => return Key::F6,
        0xffc4 => return Key::F7,
        0xffc5 => return Key::F8,
        0xffc6 => return Key::F9,
        0xffc7 => return Key::F10,
        0xffc8 => return Key::F11,
        0xffc9 => return Key::F12,
        0xff50 => return Key::Home,
        0xff57 => return Key::End,
        0xff55 => return Key::PageUp,
        0xff56 => return Key::PageDown,
        0xff63 => return Key::Insert,
        0xffff => return Key::Delete,
        _ => {}
    }
    if (33..=126).contains(&utf32) {
        return Key::KeyChar((utf32 as u8 as char).to_ascii_lowercase());
    }
    const ROWS: [(u32, &str); 4] = [(2, "1234567890"), (16, "qwertyuiop"), (30, "asdfghjkl"), (44, "zxcvbnm")];
    for (first, row) in ROWS {
        if let Some(c) = evdev.checked_sub(first).and_then(|i| row.as_bytes().get(i as usize)) {
            return Key::KeyChar(*c as char);
        }
    }
    Key::Unknown(i64::from(sym))
}

/// Linux input button codes (BTN_LEFT ...); others are dropped.
pub fn button(code: i32) -> Option<MouseButton> {
    Some(match code {
        0x110 => MouseButton::LeftButton,
        0x111 => MouseButton::RightButton,
        0x112 => MouseButton::MiddleButton,
        0x113 => MouseButton::MouseX1,
        0x114 => MouseButton::MouseX2,
        _ => return None,
    })
}

/// Modifier bits the shim reports with a key.
pub const CTRL: i32 = 2;
pub const ALT: i32 = 4;
pub const LOGO: i32 = 8;

/// The text a key press types: none with a command modifier held or for control characters (Ctrl+A is "\x01").
pub fn typed(text: &str, mods: i32) -> Option<&str> {
    let command = mods & (CTRL | ALT | LOGO) != 0;
    (!command && !text.is_empty() && !text.chars().any(char::is_control)).then_some(text)
}

/// text-input-v3 gives the preedit cursor as UTF-8 byte offsets (-1: hidden); TextEditing counts characters
/// from `start` for `length`, as SDL does. A hidden cursor sits after the text.
pub fn preedit(text: &str, begin: i32, end: i32) -> (i64, i64) {
    let chars = |range: Option<&str>| range.map_or(0, |s| s.chars().count() as i64);
    if begin < 0 || end < begin {
        return (chars(Some(text)), 0);
    }
    let (begin, end) = (begin as usize, end as usize);
    (chars(text.get(..begin)), chars(text.get(begin..end)))
}

/// Drawable pixels for a logical size at `scale120` (120ths, wp_fractional_scale's unit): rounded halfway away
/// from zero, as the protocol asks, and never below one pixel.
pub fn drawable(logical: (i64, i64), scale120: i32) -> (i64, i64) {
    let px = |v: i64| ((v as f64 * f64::from(scale120) / 120.0).round() as i64).max(1);
    (px(logical.0), px(logical.1))
}

pub const AXIS_VERTICAL: i32 = 0;
pub const SOURCE_FINGER: i32 = 1;
/// Continuous axis units a wheel notch moves when the compositor sends no discrete steps.
// ponytail: libinput's 15 degrees reported as 10 units is the common default; read axis_value120 where it matters.
pub const UNITS_PER_NOTCH: f64 = 10.0;

/// Collects one wl_pointer frame's axis events; `frame` turns them into at most a scroll and a lift.
#[derive(Default)]
pub struct Scroll {
    delta: [f64; 2],
    steps120: [i32; 2],
    has_steps: bool,
    source: Option<i32>,
    stopped: bool,
    time_ms: u32,
    touching: bool,
}

impl Scroll {
    pub fn axis(&mut self, axis: i32, value: f64, time_ms: u32) {
        self.delta[usize::from(axis == AXIS_VERTICAL)] += value;
        self.time_ms = time_ms;
    }
    pub fn steps(&mut self, axis: i32, value120: i32) {
        self.steps120[usize::from(axis == AXIS_VERTICAL)] += value120;
        self.has_steps = true;
    }
    pub fn source(&mut self, source: i32) {
        self.source = Some(source);
    }
    pub fn stop(&mut self, time_ms: u32) {
        self.stopped = true;
        self.time_ms = time_ms;
    }
    /// Fingers become TrackpadScrolled (Touched, Moved..., Lifted on axis_stop); wheels become Scrolled in
    /// notches. Wayland's axes grow rightwards and downwards; Rays' y grows away from the user.
    pub fn frame(&mut self, out: &mut Vec<Event>) {
        let touching = self.touching;
        let frame = std::mem::replace(self, Scroll { touching, ..Scroll::default() });
        let [dx, dy] = frame.delta;
        let time = f64::from(frame.time_ms) / 1000.0;
        let finger = frame.source == Some(SOURCE_FINGER) || (frame.source.is_none() && touching);
        if finger {
            if dx != 0.0 || dy != 0.0 {
                let phase = if touching { ScrollPhase::Moved } else { ScrollPhase::Touched };
                out.push(Event::TrackpadScrolled { delta: (dx, -dy), phase, time });
                self.touching = true;
            }
            if frame.stopped && self.touching {
                out.push(Event::TrackpadScrolled { delta: (0.0, 0.0), phase: ScrollPhase::Lifted, time });
                self.touching = false;
            }
        } else if frame.has_steps {
            let [sx, sy] = frame.steps120;
            if sx != 0 || sy != 0 {
                out.push(Event::Scrolled((f64::from(sx) / 120.0, -f64::from(sy) / 120.0)));
            }
        } else if dx != 0.0 || dy != 0.0 {
            out.push(Event::Scrolled((dx / UNITS_PER_NOTCH, -dy / UNITS_PER_NOTCH)));
        }
    }
}

/// Client-side key repeat (Wayland sends one press): `rate` repeats a second after `delay` milliseconds.
pub struct Repeat {
    pub rate: i32,
    pub delay: i32,
    held: Option<(i32, Key, Option<String>, f64)>,
}

impl Repeat {
    pub fn new() -> Repeat {
        Repeat { rate: 25, delay: 600, held: None }
    }
    pub fn press(&mut self, code: i32, key: Key, text: Option<String>, repeats: bool, now: f64) {
        self.held = (repeats && self.rate > 0).then(|| (code, key, text, now + f64::from(self.delay) / 1000.0));
    }
    pub fn release(&mut self, code: i32) {
        if self.held.as_ref().is_some_and(|held| held.0 == code) {
            self.held = None;
        }
    }
    pub fn clear(&mut self) {
        self.held = None;
    }
    /// When the next repeat is due, for a host that sleeps until then.
    pub fn deadline(&self) -> Option<f64> {
        self.held.as_ref().map(|held| held.3)
    }
    /// The key (and its text) to press again if a repeat is due. A host that fell behind gets one repeat,
    /// not a burst.
    pub fn due(&mut self, now: f64) -> Option<(Key, Option<String>)> {
        let interval = 1.0 / f64::from(self.rate.max(1));
        let held = self.held.as_mut().filter(|held| now >= held.3)?;
        held.3 = if held.3 + interval > now { held.3 + interval } else { now + interval };
        Some((held.1, held.2.clone()))
    }
}

impl Default for Repeat {
    fn default() -> Self {
        Repeat::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keysyms_name_keys_as_sdl_does() {
        assert_eq!(key(30, 0x61, 'a' as u32), Key::KeyChar('a'));
        assert_eq!(key(30, 0x41, 'A' as u32), Key::KeyChar('a'));
        assert_eq!(key(2, 0x31, '1' as u32), Key::KeyChar('1'));
        // a Cyrillic layout's ф on the A key, and a dead key on a digit, fall back to the scancode
        assert_eq!(key(30, 0x6c6, 'ф' as u32), Key::KeyChar('a'));
        assert_eq!(key(11, 0xfe51, 0), Key::KeyChar('0'));
        assert_eq!(key(57, 0x20, ' ' as u32), Key::Space);
        assert_eq!(key(96, 0xff8d, '\r' as u32), Key::Enter);
        assert_eq!(key(42, 0xffe1, 0), Key::Shift);
        assert_eq!(key(29, 0xffe3, 0), Key::Ctrl);
        assert_eq!(key(100, 0xfe03, 0), Key::Alt);
        assert_eq!(key(125, 0xffeb, 0), Key::Meta);
        assert_eq!(key(68, 0xffc7, 0), Key::F10);
        assert_eq!(key(111, 0xffff, 0x7f), Key::Delete);
        assert_eq!(key(103, 0xff52, 0), Key::ArrowUp);
        assert_eq!(key(142, 0x1008ff2f, 0), Key::Unknown(0x1008ff2f));
        assert_eq!(button(0x111), Some(MouseButton::RightButton));
        assert_eq!(button(0x115), None);
    }

    #[test]
    fn text_and_preedit() {
        assert_eq!(typed("A", 0), Some("A"));
        assert_eq!(typed("a", CTRL), None);
        assert_eq!(typed("\u{1}", 0), None);
        assert_eq!(typed("\r", 0), None);
        assert_eq!(typed("", 0), None);
        // "日本語" is 9 bytes: the cursor after the first character, selecting the second
        assert_eq!(preedit("日本語", 3, 6), (1, 1));
        assert_eq!(preedit("日本語", -1, -1), (3, 0));
        assert_eq!(preedit("ab", 1, 1), (1, 0));
    }

    #[test]
    fn fractional_scale_rounds_halfway_away_from_zero() {
        assert_eq!(drawable((800, 600), 120), (800, 600));
        assert_eq!(drawable((800, 600), 240), (1600, 1200));
        // 1.25: 1366 * 1.25 = 1707.5
        assert_eq!(drawable((1366, 767), 150), (1708, 959));
        assert_eq!(drawable((3, 3), 180), (5, 5));
        assert_eq!(drawable((1, 1), 60), (1, 1));
    }

    #[test]
    fn scroll_frames() {
        let mut scroll = Scroll::default();
        let mut out = Vec::new();
        // a wheel notch down with value120 and its continuous value: one notch, y towards the user
        scroll.source(0);
        scroll.axis(0, 15.0, 10);
        scroll.steps(0, 120);
        scroll.frame(&mut out);
        assert_eq!(out, [Event::Scrolled((0.0, -1.0))]);
        // a wheel without discrete steps: continuous units over the notch size
        out.clear();
        scroll.axis(1, 5.0, 20);
        scroll.frame(&mut out);
        assert_eq!(out, [Event::Scrolled((0.5, 0.0))]);
        // two fingers: touch, move, then axis_stop lifts; the stop frame needs no source
        out.clear();
        scroll.source(SOURCE_FINGER);
        scroll.axis(0, -4.0, 1000);
        scroll.frame(&mut out);
        scroll.source(SOURCE_FINGER);
        scroll.axis(1, 2.0, 1016);
        scroll.frame(&mut out);
        scroll.stop(1032);
        scroll.frame(&mut out);
        assert_eq!(out, [
            Event::TrackpadScrolled { delta: (0.0, 4.0), phase: ScrollPhase::Touched, time: 1.0 },
            Event::TrackpadScrolled { delta: (2.0, -0.0), phase: ScrollPhase::Moved, time: 1.016 },
            Event::TrackpadScrolled { delta: (0.0, 0.0), phase: ScrollPhase::Lifted, time: 1.032 },
        ]);
        // an empty frame (a button's) emits nothing
        out.clear();
        scroll.frame(&mut out);
        assert!(out.is_empty());
    }

    #[test]
    fn key_repeat_waits_delay_then_rate() {
        let mut repeat = Repeat { rate: 10, delay: 500, ..Repeat::new() };
        repeat.press(30, Key::KeyChar('a'), Some("a".into()), true, 0.0);
        assert_eq!(repeat.deadline(), Some(0.5));
        assert_eq!(repeat.due(0.4), None);
        assert_eq!(repeat.due(0.5), Some((Key::KeyChar('a'), Some("a".into()))));
        assert_eq!(repeat.due(0.55), None);
        assert!(repeat.due(0.6).is_some());
        // a stalled host gets one repeat, then the rhythm restarts from now
        assert!(repeat.due(5.0).is_some());
        assert_eq!(repeat.deadline(), Some(5.1));
        repeat.release(31);
        assert!(repeat.deadline().is_some(), "another key's release keeps the repeat");
        repeat.release(30);
        assert_eq!(repeat.deadline(), None);
        repeat.press(42, Key::Shift, None, false, 0.0);
        assert_eq!(repeat.deadline(), None, "modifiers do not repeat");
        repeat.rate = 0;
        repeat.press(30, Key::KeyChar('a'), None, true, 0.0);
        assert_eq!(repeat.deadline(), None, "rate 0 disables repeat");
    }
}
