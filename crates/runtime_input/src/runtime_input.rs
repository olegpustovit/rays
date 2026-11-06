use std::collections::VecDeque;
use std::sync::OnceLock;

use input::{Event, Platform, platform_of_name};

#[allow(non_camel_case_types)]
pub type event = Event;

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
pub struct snapshot {
    pub pointer: (f64, f64),
    pub mouse_delta: (f64, f64),
    pub visible: bool,
}

// All-float records store their fields unboxed, so per-event updates assign in place.
#[allow(non_camel_case_types)]
#[derive(Debug)]
struct pair {
    px: f64,
    py: f64,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub struct t {
    max_events: i64,
    events: VecDeque<Event>,
    pointer: pair,
    mouse_delta: pair,
    visible: bool,
    // Relative (captured) pointer mode: motion deltas come from the device's
    // relative motion, since absolute positions stop at the window edge.
    relative: bool,
    // Window coordinates per logical point (Runtime.window_unit_of).
    unit: f64,
}

pub fn create(max_events: i64, logical_width: i64, logical_height: i64) -> Result<t, String> {
    if max_events <= 0 {
        Err("max_events must be positive".to_string())
    } else if logical_width <= 0 || logical_height <= 0 {
        Err("logical dimensions must be positive".to_string())
    } else {
        Ok(t {
            max_events,
            events: VecDeque::new(),
            pointer: pair { px: 0., py: 0. },
            mouse_delta: pair { px: 0., py: 0. },
            visible: true,
            relative: false,
            unit: 1.,
        })
    }
}

fn apply(value: &mut t, event: &Event) {
    match *event {
        Event::Pointer_moved((x, y)) => {
            if !value.relative {
                value.mouse_delta.px = value.mouse_delta.px + x - value.pointer.px;
                value.mouse_delta.py = value.mouse_delta.py + y - value.pointer.py;
            }
            value.pointer.px = x;
            value.pointer.py = y;
        }
        Event::Pointer_pressed(_, (x, y)) | Event::Pointer_released(_, (x, y)) => {
            value.pointer.px = x;
            value.pointer.py = y;
        }
        Event::Visibility_changed(visible) => value.visible = visible,
        Event::Text_input(_)
        | Event::Text_editing { .. }
        | Event::File_dropped(_)
        | Event::File_drag_moved(_)
        | Event::File_drag_ended
        | Event::Pixel_size_changed(_)
        | Event::Pinched(_)
        | Event::Trackpad_scrolled { .. }
        | Event::Focus_gained
        | Event::Closed
        | Event::File_dialog { .. }
        | Event::Dialog_opened(_)
        | Event::Pointer_cancelled(_)
        | Event::Scrolled(_)
        | Event::Key_pressed(_)
        | Event::Key_released(_)
        | Event::Focus_lost
        | Event::Resized(_) => {}
    }
}

pub fn push(value: &mut t, event: event) -> Result<(), String> {
    let error = match &event {
        Event::File_dropped(path) if path.is_empty() || path.contains('\0') => {
            Some("file-drop path is malformed")
        }
        Event::Resized((width, height)) | Event::Pixel_size_changed((width, height))
            if *width <= 0 || *height <= 0 =>
        {
            Some("resize dimensions must be positive")
        }
        Event::Pinched(scale) if !scale.is_finite() || *scale <= 0. => {
            Some("pinch scale must be finite and positive")
        }
        Event::Trackpad_scrolled { delta: (x, y), time: seconds, .. }
            if !(x.is_finite() && y.is_finite() && seconds.is_finite()) =>
        {
            Some("scroll gesture must be finite")
        }
        _ => None,
    };
    if let Some(message) = error {
        return Err(message.to_string());
    }
    apply(value, &event);
    if value.events.len() as i64 == value.max_events {
        value.events.pop_front();
    }
    value.events.push_back(event);
    Ok(())
}

pub fn drain(value: &mut t) -> Vec<event> {
    value.events.drain(..).collect()
}

pub fn set_relative(value: &mut t, relative: bool) {
    value.relative = relative;
}

pub fn relative(value: &t) -> bool {
    value.relative
}

pub fn set_unit(value: &mut t, unit: f64) {
    if unit.is_finite() && unit > 0. {
        value.unit = unit;
    }
}

pub fn unit(value: &t) -> f64 {
    value.unit
}

pub fn add_motion(value: &mut t, dx: f64, dy: f64) {
    if value.relative {
        value.mouse_delta.px += dx;
        value.mouse_delta.py += dy;
    }
}

pub fn begin_frame(value: &mut t) {
    value.mouse_delta.px = 0.;
    value.mouse_delta.py = 0.;
}

pub fn set_extent(_value: &t, logical_width: i64, logical_height: i64) -> Result<(), String> {
    if logical_width <= 0 || logical_height <= 0 {
        Err("logical dimensions must be positive".to_string())
    } else {
        Ok(())
    }
}

pub fn snapshot(value: &t) -> snapshot {
    snapshot {
        pointer: (value.pointer.px, value.pointer.py),
        mouse_delta: (value.mouse_delta.px, value.mouse_delta.py),
        visible: value.visible,
    }
}

static PLATFORM: OnceLock<Platform> = OnceLock::new();

// Read once from SDL, on first use (OCaml reads it at module initialisation).
pub fn platform() -> Platform {
    *PLATFORM.get_or_init(|| platform_of_name(&sdl3::platform_name()))
}
