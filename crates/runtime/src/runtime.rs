use ogpu::error::{self, Error, ErrorKind};
use ogpu::surface;
use sdl3::cursor::{Cursor, Shape};
use sdl3::window::{Flag, Window};
use sdl3::{DialogFilter, DialogKind, NativeView, Rect};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct frame_facts {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub pixel_scale_x: f64,
    pub pixel_scale_y: f64,
}

/// An offscreen target never vsyncs and reports its drawable/logical ratio as its density.
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct presentation_facts {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub pixel_density: f64,
}

/// SDL announces a change of size or density with an event instead of making every frame ask.
/// The announcement is one bit: set by the event, taken by the next frame, which re-reads the
/// window only if it was set.
#[allow(non_snake_case)]
pub mod Change_flag {
    #[allow(non_camel_case_types)]
    #[derive(Debug)]
    pub struct t {
        announced: bool,
    }

    pub fn create() -> t {
        t { announced: false }
    }

    pub fn announce(flag: &mut t) {
        flag.announced = true;
    }

    /// Clears the announcement and says whether there was one.
    pub fn take(flag: &mut t) -> bool {
        std::mem::replace(&mut flag.announced, false)
    }

    /// `query` runs once per announcement, however many arrived; true when it ran.
    pub fn refresh(flag: &mut t, query: &mut dyn FnMut()) -> bool {
        let announced = take(flag);
        if announced {
            query();
        }
        announced
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum cursor_shape {
    Default,
    Horizontal_resize,
    Vertical_resize,
    Text,
}

#[allow(non_camel_case_types)]
struct window {
    handle: Window,
    view: NativeView,
    vsync: bool,
    cursors: Vec<(cursor_shape, Cursor)>,
    cursor_shape: Option<cursor_shape>,
    text_input: bool,
    changed: Change_flag::t,
    /// window coordinates per logical point: SDL's display scale over its pixel density
    unit: f64,
}

/// One render target: a presenting window, or (`window = None`) an owned offscreen texture.
#[allow(non_camel_case_types)]
pub struct t {
    window: Option<window>,
    facts: frame_facts,
    presentation: Option<presentation_facts>,
    dead: bool,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum dialog_kind {
    Open_file,
    Open_files,
    Save_file,
    Open_folder,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct dialog_filter {
    pub name: String,
    pub pattern: String,
}


fn stale_error(operation: &str) -> Error {
    error::make(operation, ErrorKind::StaleHandle, "runtime is destroyed")
}

fn fail_error(op: &str, text: &str) -> Error {
    error::make(op, ErrorKind::InvalidState, text)
}

fn sdl_error(op: &str, failure: &sdl3::Error) -> Error {
    fail_error(op, &failure.to_string())
}

pub fn clipboard_set_text(text: &str) -> Result<(), Error> {
    sdl3::clipboard::set_text(text).map_err(|failure| sdl_error("Runtime.clipboard_set_text", &failure))
}

pub fn clipboard_get_text() -> Result<String, Error> {
    sdl3::clipboard::get_text().map_err(|failure| sdl_error("Runtime.clipboard_get_text", &failure))
}

// macOS reports window coordinates in points (display scale 2 over density 2: one coordinate a
// point); Windows reports pixels with density 1 and the monitor's scaling as the display scale
// (1.5 at 150%). Rays lays out in points, so window coordinates are divided by that ratio here
// and multiplied back where a size or a rect goes to SDL.
fn window_unit(window: &Window) -> Result<f64, Error> {
    let op = "Runtime.window_unit";
    let scale = window.display_scale().map_err(|failure| sdl_error(op, &failure))? as f64;
    let density = window.pixel_density().map_err(|failure| sdl_error(op, &failure))? as f64;
    let unit = scale / density;
    Ok(if (unit - 1.).abs() < 1e-3 { 1. } else { unit })
}

// OCaml `Float.round` rounds half away from zero, like f64::round.
fn to_points(unit: f64, size: i32) -> i64 {
    std::cmp::max(1, ((size as f64) / unit).round() as i64)
}

fn to_window(unit: f64, size: i64) -> i32 {
    std::cmp::max(1, ((size as f64) * unit).round() as i64) as i32
}

fn facts_in(unit: f64, window: &Window) -> Result<frame_facts, Error> {
    let (lw, lh) = window.size().map_err(|failure| sdl_error("Runtime.facts", &failure))?;
    let (dw, dh) = window.size_in_pixels().map_err(|failure| sdl_error("Runtime.facts", &failure))?;
    if lw > 0 && lh > 0 && dw > 0 && dh > 0 {
        let lw = to_points(unit, lw);
        let lh = to_points(unit, lh);
        let (dw, dh) = (dw as i64, dh as i64);
        Ok(frame_facts {
            logical_width: lw,
            logical_height: lh,
            drawable_width: dw,
            drawable_height: dh,
            pixel_scale_x: dw as f64 / lw as f64,
            pixel_scale_y: dh as f64 / lh as f64,
        })
    } else {
        Err(fail_error("Runtime.facts", "window dimensions are invalid"))
    }
}

fn facts(window: &Window) -> Result<frame_facts, Error> {
    facts_in(window_unit(window)?, window)
}

// Every step is checked: the first failure is the result.
fn reveal(window: &Window) -> std::result::Result<(), sdl3::Error> {
    window.show()?;
    window.restore()?;
    window.raise_window()?;
    window.center()?;
    window.sync()
}

/// `high_density` (default true) asks for a drawable at the display's native density; false keeps
/// the drawable at the logical size, so a capture is the same bytes on every display.
pub fn create(
    vsync: Option<bool>,
    hidden: Option<bool>,
    high_density: Option<bool>,
    title: Option<&str>,
    width: i64,
    height: i64,
) -> Result<t, Error> {
    let op = "Runtime.create";
    let vsync = vsync.unwrap_or(true);
    let hidden = hidden.unwrap_or(true);
    let high_density = high_density.unwrap_or(true);
    let title = title.unwrap_or("Rays");
    if width <= 0 || height <= 0 {
        return Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"));
    }
    sdl3::init::init(&[sdl3::init::Subsystem::Video]).map_err(|failure| sdl_error(op, &failure))?;
    // The window and view release themselves when `build` returns; a failure then quits video.
    let build = || -> Result<t, Error> {
        // SDL itself turns a Control-click into the secondary click.
        sdl3::hint::control_click_is_right_click(true).map_err(|failure| sdl_error(op, &failure))?;
        if cfg!(windows) {
            sdl3::hint::mouse_relative_system_scale(true).map_err(|failure| sdl_error(op, &failure))?;
        }
        let mut flags = vec![Flag::NativeView];
        if high_density {
            flags.push(Flag::HighPixelDensity);
        }
        if hidden {
            flags.push(Flag::Hidden);
        }
        let handle = Window::create(title, width as i32, height as i32, &flags).map_err(|failure| sdl_error(op, &failure))?;
        let unit = window_unit(&handle)?;
        if unit != 1. {
            handle.set_size(to_window(unit, width), to_window(unit, height)).map_err(|failure| sdl_error(op, &failure))?;
        }
        if !hidden {
            reveal(&handle).map_err(|failure| sdl_error(op, &failure))?;
        }
        let mut view = NativeView::create(&handle).map_err(|failure| sdl_error(op, &failure))?;
        let _token = view.layer().map_err(|failure| sdl_error(op, &failure))?;
        // Size the renderer for the actual drawable once, rather than allocating a
        // logical-size target and immediately resizing it.
        let facts = facts(&handle)?;
        Ok(t {
            window: Some(window {
                handle,
                view,
                vsync,
                cursors: Vec::new(),
                cursor_shape: None,
                text_input: false,
                changed: Change_flag::create(),
                unit,
            }),
            facts,
            presentation: None,
            dead: false,
        })
    };
    let created = build();
    if created.is_err() {
        let _ = sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video]);
    }
    created
}

fn offscreen_facts(logical_width: i64, logical_height: i64, width: i64, height: i64) -> frame_facts {
    frame_facts {
        logical_width,
        logical_height,
        drawable_width: width,
        drawable_height: height,
        pixel_scale_x: width as f64 / logical_width as f64,
        pixel_scale_y: height as f64 / logical_height as f64,
    }
}

/// An offscreen frame-facts source; GPU resources are owned by `Renderer`.
pub fn create_offscreen(logical_width: i64, logical_height: i64, width: i64, height: i64) -> Result<t, Error> {
    let op = "Runtime.create_offscreen";
    if width <= 0 || height <= 0 || logical_width <= 0 || logical_height <= 0 {
        Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"))
    } else {
        Ok(t {
            window: None,
            facts: offscreen_facts(logical_width, logical_height, width, height),
            presentation: None,
            dead: false,
        })
    }
}

fn scale_rect(facts: &frame_facts, rect: (i64, i64, i64, i64)) -> (i64, i64, i64, i64) {
    let (x, y, w, h) = rect;
    let edge = |value: i64, logical: i64, drawable: i64| value * drawable / logical;
    let l = edge(x, facts.logical_width, facts.drawable_width);
    let t = edge(y, facts.logical_height, facts.drawable_height);
    let r = edge(x + w, facts.logical_width, facts.drawable_width);
    let b = edge(y + h, facts.logical_height, facts.drawable_height);
    (l, t, r - l, b - t)
}

pub fn map_logical_rect(facts: &frame_facts, rect: (i64, i64, i64, i64)) -> (i64, i64, i64, i64) {
    scale_rect(facts, rect)
}

// A window's drawable follows SDL, which announces each change as a size event
// (`window_changed`); an offscreen target changes only through `resize`. A frame costs no SDL
// call while nothing was announced.
pub fn sync_facts(value: &mut t) -> Result<(), Error> {
    let Some(window) = value.window.as_mut() else { return Ok(()) };
    if !Change_flag::take(&mut window.changed) {
        return Ok(());
    }
    window.unit = window_unit(&window.handle)?;
    let live = facts_in(window.unit, &window.handle)?;
    let f = &value.facts;
    if live.logical_width == f.logical_width
        && live.logical_height == f.logical_height
        && live.drawable_width == f.drawable_width
        && live.drawable_height == f.drawable_height
    {
        return Ok(());
    }
    value.facts = live;
    Ok(())
}

/// Resizes to `width`x`height` logical points. A window's drawable follows its display; an
/// offscreen target uses a 1x drawable.
pub fn resize(value: &mut t, width: i64, height: i64) -> Result<(), Error> {
    let op = "Runtime.resize";
    if value.dead {
        return Err(stale_error(op));
    }
    if width <= 0 || height <= 0 {
        return Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"));
    }
    match value.window.as_ref() {
        Some(window) => {
            window.handle.set_size(to_window(window.unit, width), to_window(window.unit, height)).map_err(|failure| sdl_error(op, &failure))?;
            value.presentation = None;
            value.facts = facts(&window.handle)?;
            Ok(())
        }
        None => {
            value.presentation = None;
            value.facts = offscreen_facts(width, height, width, height);
            Ok(())
        }
    }
}

pub fn frame_facts(value: &t) -> frame_facts {
    value.facts
}

/// Window coordinates per logical point: 1 on macOS and offscreen, the monitor's scaling (1.5 at
/// 150%) on Windows, whose SDL events and sizes are in pixels. Input divides by it.
pub fn window_unit_of(value: &t) -> f64 {
    value.window.as_ref().map_or(1., |w| w.unit)
}

fn live_window(operation: &str, value: &mut t, callback: &mut dyn FnMut(&mut window) -> Result<(), Error>) -> Result<(), Error> {
    if value.dead {
        return Err(stale_error(operation));
    }
    match value.window.as_mut() {
        None => Err(error::make(operation, ErrorKind::Unsupported, "operation requires a window")),
        Some(window) => callback(window),
    }
}

fn query_presentation(value: &t) -> Result<presentation_facts, Error> {
    let f = &value.facts;
    let pixel_density = match value.window.as_ref() {
        None => f.pixel_scale_x,
        Some(w) => w.handle.pixel_density().map_err(|failure| sdl_error("Runtime.presentation_facts", &failure))? as f64 * w.unit,
    };
    Ok(presentation_facts {
        logical_width: f.logical_width,
        logical_height: f.logical_height,
        drawable_width: f.drawable_width,
        drawable_height: f.drawable_height,
        pixel_density,
    })
}

/// Cached presentation facts, requeried when the drawable changes and after resize or show.
pub fn presentation_facts(value: &mut t) -> Result<presentation_facts, Error> {
    if value.dead {
        return Err(stale_error("Runtime.presentation_facts"));
    }
    let live = value.facts;
    if let Some(cached) = value.presentation {
        if cached.logical_width == live.logical_width
            && cached.logical_height == live.logical_height
            && cached.drawable_width == live.drawable_width
            && cached.drawable_height == live.drawable_height
        {
            return Ok(cached);
        }
    }
    let facts = query_presentation(value)?;
    value.presentation = Some(facts);
    Ok(facts)
}

/// Tell a window target that SDL announced a size or density change, so the next frame re-reads
/// its facts. The window is not polled between announcements.
pub fn window_changed(value: &mut t) {
    if !value.dead {
        if let Some(window) = value.window.as_mut() {
            Change_flag::announce(&mut window.changed);
        }
    }
}

pub fn set_resizable(value: &mut t, enabled: bool) -> Result<(), Error> {
    live_window("Runtime.set_resizable", value, &mut |w| {
        w.handle.set_resizable(enabled).map_err(|failure| sdl_error("Runtime.set_resizable", &failure))
    })
}

/// The native window's own background (sRGB, 0..1): what the title bar shows, since a Rays window
/// hides its title and keeps only the traffic lights.
pub fn set_window_background(value: &mut t, color: (f64, f64, f64)) -> Result<(), Error> {
    live_window("Runtime.set_window_background", value, &mut |w| {
        w.handle.set_background(color.0 as f32, color.1 as f32, color.2 as f32).map_err(|failure| sdl_error("Runtime.set_window_background", &failure))
    })
}

/// Hide and capture the pointer, reporting relative motion (fly cameras).
pub fn set_relative_mouse(value: &mut t, enabled: bool) -> Result<(), Error> {
    live_window("Runtime.set_relative_mouse", value, &mut |w| {
        w.handle.set_relative_mouse(enabled).map_err(|failure| sdl_error("Runtime.set_relative_mouse", &failure))
    })
}

pub fn set_cursor(value: &mut t, shape: cursor_shape) -> Result<(), Error> {
    live_window("Runtime.set_cursor", value, &mut |window| {
        if window.cursor_shape == Some(shape) {
            return Ok(());
        }
        let index = match window.cursors.iter().position(|(s, _)| *s == shape) {
            Some(i) => i,
            None => {
                let native = match shape {
                    cursor_shape::Default => Shape::Default,
                    cursor_shape::Horizontal_resize => Shape::EwResize,
                    cursor_shape::Vertical_resize => Shape::NsResize,
                    cursor_shape::Text => Shape::Text,
                };
                let cursor = Cursor::create(native).map_err(|failure| sdl_error("Runtime.set_cursor", &failure))?;
                // OCaml prepends; lookup is by key so order only matters for destroy order.
                window.cursors.insert(0, (shape, cursor));
                0
            }
        };
        window.cursors[index].1.set().map_err(|failure| sdl_error("Runtime.set_cursor", &failure))?;
        window.cursor_shape = Some(shape);
        Ok(())
    })
}

/// Text input runs only while a text field has focus: `Some (region, cursor)` (logical points)
/// starts it and places the input method there, `None` stops it. Repeating the same answer costs
/// no SDL call.
pub fn set_text_input(value: &mut t, area: Option<((i64, i64, i64, i64), i64)>) -> Result<(), Error> {
    let operation = "Runtime.set_text_input";
    live_window(operation, value, &mut |window| match area {
        None => {
            if window.text_input {
                sdl3::text_input::stop(&window.handle).map_err(|failure| sdl_error(operation, &failure))?;
                window.text_input = false;
            }
            Ok(())
        }
        Some(((x, y, width, height), cursor)) => {
            if !window.text_input {
                sdl3::text_input::start(&window.handle).map_err(|failure| sdl_error(operation, &failure))?;
                window.text_input = true;
            }
            let unit = window.unit;
            let scale = |v: i64| (v as f64 * unit).round() as i32;
            sdl3::text_input::set_area(
                    &window.handle,
                    Rect { x: scale(x), y: scale(y), width: scale(width), height: scale(height) },
                    scale(cursor),
                ).map_err(|failure| sdl_error(operation, &failure))
        }
    })
}

pub fn show(value: &mut t) -> Result<(), Error> {
    live_window("Runtime.show", value, &mut |window| {
        reveal(&window.handle).map_err(|failure| sdl_error("Runtime.show", &failure))?;
        Change_flag::announce(&mut window.changed);
        Ok(())
    })?;
    let synced = sync_facts(value);
    if synced.is_ok() {
        value.presentation = None;
    }
    synced
}

/// Opens a native file dialog and returns at once with its id; the outcome is a
/// `Input.File_dialog` carrying the same id. `pattern` lists extensions separated by semicolons
/// ("png;jpg"), or "*". At most 8 dialogs are open at once; a ninth is an error.
pub fn show_dialog(
    value: &mut t,
    filters: Option<&[dialog_filter]>,
    default_location: Option<&str>,
    kind: dialog_kind,
) -> Result<i64, Error> {
    let op = "Runtime.show_dialog";
    let mut id = 0;
    let filters: Vec<DialogFilter> = filters
        .unwrap_or(&[])
        .iter()
        .map(|f| DialogFilter { name: f.name.clone(), pattern: f.pattern.clone() })
        .collect();
    let native = match kind {
        dialog_kind::Open_file => DialogKind::OpenFile,
        dialog_kind::Open_files => DialogKind::OpenFiles,
        dialog_kind::Save_file => DialogKind::SaveFile,
        dialog_kind::Open_folder => DialogKind::OpenFolder,
    };
    live_window(op, value, &mut |window| {
        id = sdl3::dialog::show(&window.handle, native, &filters, default_location).map_err(|failure| sdl_error(op, &failure))? as i64;
        Ok(())
    })?;
    Ok(id)
}

/// Shown and neither minimized nor covered. SDL announces the start of occlusion and not its end,
/// so a caller that saw the window hidden asks this once per frame to learn when it is back.
pub fn visible(value: &mut t) -> Result<bool, Error> {
    let mut seen = false;
    live_window("Runtime.visible", value, &mut |window| {
        let state = window.handle.state().map_err(|failure| sdl_error("Runtime.visible", &failure))?;
        seen = !(state.hidden || state.minimized || state.occluded);
        Ok(())
    })?;
    Ok(seen)
}

pub fn destroy(value: &mut t) -> Result<(), Error> {
    if value.dead {
        return Ok(());
    }
    value.dead = true;
    let mut failure: Option<Error> = None;
    let mut record = |result: Result<(), Error>| {
        if let Err(e) = result {
            if failure.is_none() {
                failure = Some(e);
            }
        }
    };
    if let Some(window) = value.window.as_mut() {
        if window.text_input {
            record(sdl3::text_input::stop(&window.handle).map_err(|failure| sdl_error("Runtime.destroy", &failure)));
        }
        for (_, cursor) in window.cursors.iter_mut() {
            record(cursor.destroy().map_err(|failure| sdl_error("Runtime.destroy", &failure)));
        }
        window.cursors.clear();
        record(window.view.destroy().map_err(|failure| sdl_error("Runtime.destroy", &failure)));
        record(window.handle.destroy().map_err(|failure| sdl_error("Runtime.destroy", &failure)));
        record(sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video]).map_err(|failure| sdl_error("Runtime.destroy", &failure)));
    }
    match failure {
        None => Ok(()),
        Some(e) => Err(e),
    }
}

pub fn is_window(value: &t) -> bool {
    value.window.is_some()
}

pub fn surface_configuration(value: &mut t) -> Result<surface::Configuration, Error> {
    let layer = match value.window.as_mut() {
        None => None,
        Some(w) => Some(w.view.layer().map_err(|failure| sdl_error("Runtime.surface_configuration", &failure))?),
    };
    let f = &value.facts;
    let vsync = value.window.as_ref().is_some_and(|w| w.vsync);
    Ok(surface::Configuration {
        logical_width: f.logical_width,
        logical_height: f.logical_height,
        physical_width: f.drawable_width,
        physical_height: f.drawable_height,
        format: surface::Format::Bgra8Unorm,
        present_mode: if vsync { surface::PresentMode::Fifo } else { surface::PresentMode::Immediate },
        max_acquired: 2,
        layer,
    })
}
