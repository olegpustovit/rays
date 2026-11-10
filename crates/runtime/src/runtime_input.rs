use std::collections::VecDeque;
use std::sync::OnceLock;

pub use input::Event;
use input::{Platform, platform_of_name};





#[derive(Clone, Debug, PartialEq)]
pub struct Snapshot {
    pub pointer: (f64, f64),
    pub mouse_delta: (f64, f64),
    pub visible: bool,
}

// All-float records store their fields unboxed, so per-event updates assign in place.

#[derive(Debug)]
struct Pair {
    px: f64,
    py: f64,
}

#[derive(Debug)]
pub struct RuntimeInput {
    max_events: i64,
    events: VecDeque<Event>,
    pointer: Pair,
    mouse_delta: Pair,
    visible: bool,
    // Relative (captured) pointer mode: motion deltas come from the device's
    // relative motion, since absolute positions stop at the window edge.
    relative: bool,
    // Window coordinates per logical point (Runtime.window_unit_of).
    unit: f64,
}

pub fn create(max_events: i64, logical_width: i64, logical_height: i64) -> Result<RuntimeInput, String> {
    if max_events <= 0 {
        Err("max_events must be positive".to_string())
    } else if logical_width <= 0 || logical_height <= 0 {
        Err("logical dimensions must be positive".to_string())
    } else {
        Ok(RuntimeInput {
            max_events,
            events: VecDeque::new(),
            pointer: Pair { px: 0., py: 0. },
            mouse_delta: Pair { px: 0., py: 0. },
            visible: true,
            relative: false,
            unit: 1.,
        })
    }
}

fn apply(value: &mut RuntimeInput, event: &Event) {
    match *event {
        Event::PointerMoved((x, y)) => {
            if !value.relative {
                value.mouse_delta.px = value.mouse_delta.px + x - value.pointer.px;
                value.mouse_delta.py = value.mouse_delta.py + y - value.pointer.py;
            }
            value.pointer.px = x;
            value.pointer.py = y;
        }
        Event::PointerPressed(_, (x, y)) | Event::PointerReleased(_, (x, y)) => {
            value.pointer.px = x;
            value.pointer.py = y;
        }
        Event::VisibilityChanged(visible) => value.visible = visible,
        Event::TextInput(_)
        | Event::TextEditing { .. }
        | Event::FileDropped(_)
        | Event::FileDragMoved(_)
        | Event::FileDragEnded
        | Event::PixelSizeChanged(_)
        | Event::Pinched(_)
        | Event::TrackpadScrolled { .. }
        | Event::FocusGained
        | Event::Closed
        | Event::FileDialog { .. }
        | Event::DialogOpened(_)
        | Event::PointerCancelled(_)
        | Event::Scrolled(_)
        | Event::KeyPressed(_)
        | Event::KeyReleased(_)
        | Event::FocusLost
        | Event::Resized(_) => {}
    }
}

pub fn push(value: &mut RuntimeInput, event: Event) -> Result<(), String> {
    let error = match &event {
        Event::FileDropped(path) if path.is_empty() || path.contains('\0') => {
            Some("file-drop path is malformed")
        }
        Event::Resized((width, height)) | Event::PixelSizeChanged((width, height))
            if *width <= 0 || *height <= 0 =>
        {
            Some("resize dimensions must be positive")
        }
        Event::Pinched(scale) if !scale.is_finite() || *scale <= 0. => {
            Some("pinch scale must be finite and positive")
        }
        Event::TrackpadScrolled { delta: (x, y), time: seconds, .. }
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

pub fn drain(value: &mut RuntimeInput) -> Vec<Event> {
    value.events.drain(..).collect()
}

pub fn set_relative(value: &mut RuntimeInput, relative: bool) {
    value.relative = relative;
}

pub fn relative(value: &RuntimeInput) -> bool {
    value.relative
}

pub fn set_unit(value: &mut RuntimeInput, unit: f64) {
    if unit.is_finite() && unit > 0. {
        value.unit = unit;
    }
}

pub fn unit(value: &RuntimeInput) -> f64 {
    value.unit
}

pub fn add_motion(value: &mut RuntimeInput, dx: f64, dy: f64) {
    if value.relative {
        value.mouse_delta.px += dx;
        value.mouse_delta.py += dy;
    }
}

pub fn begin_frame(value: &mut RuntimeInput) {
    value.mouse_delta.px = 0.;
    value.mouse_delta.py = 0.;
}

pub fn set_extent(_value: &RuntimeInput, logical_width: i64, logical_height: i64) -> Result<(), String> {
    if logical_width <= 0 || logical_height <= 0 {
        Err("logical dimensions must be positive".to_string())
    } else {
        Ok(())
    }
}

pub fn snapshot(value: &RuntimeInput) -> Snapshot {
    Snapshot {
        pointer: (value.pointer.px, value.pointer.py),
        mouse_delta: (value.mouse_delta.px, value.mouse_delta.py),
        visible: value.visible,
    }
}

static PLATFORM: OnceLock<Platform> = OnceLock::new();

// Read SDL input state once on first use.
pub fn platform() -> Platform {
    *PLATFORM.get_or_init(|| platform_of_name(&sdl3::platform_name()))
}
