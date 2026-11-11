use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::surface;
use crate::host::{self, Extent, Window};
use std::{cell::RefCell, rc::Rc};

pub use crate::host::{clipboard_get_text, clipboard_set_text, CursorShape, DialogFilter, DialogKind};
pub use pxui_sdl::change_flag;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FrameFacts {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub pixel_scale_x: f64,
    pub pixel_scale_y: f64,
}

/// An offscreen target never vsyncs and reports its drawable/logical ratio as its density.

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PresentationFacts {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub pixel_density: f64,
}

/// One render target: a presenting `pxui_sdl` window, or (`window = None`) an owned offscreen
/// texture's extent. Whoever opened it destroys it; the renderer only reads it.
pub struct Runtime {
    window: Option<Rc<RefCell<Window>>>,
    facts: FrameFacts,
    presentation: Option<PresentationFacts>,
    dead: bool,
}

/// A target shared between its owner (who opens and destroys it) and the renderer that draws to it.
pub type Target = Rc<RefCell<Runtime>>;

/// A visible, resizable window target for the caller to own.
pub fn open_window(title: &str, width: i64, height: i64, vsync: bool, high_density: bool) -> Result<Target, Error> {
    let mut runtime = create(Some(vsync), Some(false), Some(high_density), Some(title), width, height)?;
    if let Err(e) = set_resizable(&mut runtime, true) {
        let _ = destroy(&mut runtime);
        return Err(e);
    }
    Ok(Rc::new(RefCell::new(runtime)))
}

fn stale_error(operation: &str) -> Error {
    error::make(operation, ErrorKind::StaleHandle, "runtime is destroyed")
}

fn fail_error(op: &str, text: &str) -> Error {
    error::make(op, ErrorKind::InvalidState, text)
}

fn facts_of(extent: Extent) -> FrameFacts {
    offscreen_facts(extent.logical.0, extent.logical.1, extent.drawable.0, extent.drawable.1)
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
) -> Result<Runtime, Error> {
    let op = "Runtime.create";
    if width <= 0 || height <= 0 {
        return Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"));
    }
    let window = Window::open_with(title.unwrap_or("Rays"), width, height, vsync.unwrap_or(true), hidden.unwrap_or(true), high_density.unwrap_or(true))
        .map_err(|text| fail_error(op, &text))?;
    let facts = facts_of(window.extent());
    let window = Rc::new(RefCell::new(window));
    host::opened(&window);
    Ok(Runtime { facts, window: Some(window), presentation: None, dead: false })
}

fn offscreen_facts(logical_width: i64, logical_height: i64, width: i64, height: i64) -> FrameFacts {
    FrameFacts {
        logical_width,
        logical_height,
        drawable_width: width,
        drawable_height: height,
        pixel_scale_x: width as f64 / logical_width as f64,
        pixel_scale_y: height as f64 / logical_height as f64,
    }
}

/// An offscreen frame-facts source; GPU resources are owned by `Renderer`.
pub fn create_offscreen(logical_width: i64, logical_height: i64, width: i64, height: i64) -> Result<Runtime, Error> {
    let op = "Runtime.create_offscreen";
    if width <= 0 || height <= 0 || logical_width <= 0 || logical_height <= 0 {
        Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"))
    } else {
        Ok(Runtime {
            window: None,
            facts: offscreen_facts(logical_width, logical_height, width, height),
            presentation: None,
            dead: false,
        })
    }
}

fn scale_rect(facts: &FrameFacts, rect: (i64, i64, i64, i64)) -> (i64, i64, i64, i64) {
    let (x, y, w, h) = rect;
    let edge = |value: i64, logical: i64, drawable: i64| value * drawable / logical;
    let l = edge(x, facts.logical_width, facts.drawable_width);
    let t = edge(y, facts.logical_height, facts.drawable_height);
    let r = edge(x + w, facts.logical_width, facts.drawable_width);
    let b = edge(y + h, facts.logical_height, facts.drawable_height);
    (l, t, r - l, b - t)
}

pub fn map_logical_rect(facts: &FrameFacts, rect: (i64, i64, i64, i64)) -> (i64, i64, i64, i64) {
    scale_rect(facts, rect)
}

// A window's drawable follows SDL, which announces each change as a size event
// (`window_changed`); an offscreen target changes only through `resize`. A frame costs no SDL
// call while nothing was announced.
pub fn sync_facts(value: &mut Runtime) -> Result<(), Error> {
    let Some(window) = value.window.as_ref() else { return Ok(()) };
    let mut window = window.borrow_mut();
    window.sync().map_err(|text| fail_error("Runtime.facts", &text))?;
    value.facts = facts_of(window.extent());
    Ok(())
}

/// Resizes to `width`x`height` logical points. A window's drawable follows its display; an
/// offscreen target uses a 1x drawable.
pub fn resize(value: &mut Runtime, width: i64, height: i64) -> Result<(), Error> {
    let op = "Runtime.resize";
    if value.dead {
        return Err(stale_error(op));
    }
    if width <= 0 || height <= 0 {
        return Err(error::make(op, ErrorKind::InvalidArgument, "dimensions must be positive"));
    }
    value.presentation = None;
    match value.window.as_mut() {
        Some(window) => {
            let mut window = window.borrow_mut();
            window.resize(width, height).map_err(|text| fail_error(op, &text))?;
            value.facts = facts_of(window.extent());
        }
        None => value.facts = offscreen_facts(width, height, width, height),
    }
    Ok(())
}

pub fn frame_facts(value: &Runtime) -> FrameFacts {
    value.facts
}

/// Window coordinates per logical point: 1 on macOS and offscreen, the monitor's scaling (1.5 at
/// 150%) on Windows, whose SDL events and sizes are in pixels. Input divides by it.
pub fn window_unit_of(value: &Runtime) -> f64 {
    value.window.as_ref().map_or(1., |w| w.borrow().unit())
}

fn live_window(operation: &str, value: &mut Runtime, callback: &mut dyn FnMut(&mut Window) -> Result<(), String>) -> Result<(), Error> {
    if value.dead {
        return Err(stale_error(operation));
    }
    match value.window.as_ref() {
        None => Err(error::make(operation, ErrorKind::Unsupported, "operation requires a window")),
        Some(window) => callback(&mut window.borrow_mut()).map_err(|text| fail_error(operation, &text)),
    }
}

fn query_presentation(value: &Runtime) -> Result<PresentationFacts, Error> {
    let f = &value.facts;
    let pixel_density = match value.window.as_ref() {
        None => f.pixel_scale_x,
        Some(w) => w.borrow().pixel_density().map_err(|text| fail_error("Runtime.presentation_facts", &text))?,
    };
    Ok(PresentationFacts {
        logical_width: f.logical_width,
        logical_height: f.logical_height,
        drawable_width: f.drawable_width,
        drawable_height: f.drawable_height,
        pixel_density,
    })
}

/// Cached presentation facts, requeried when the drawable changes and after resize or show.
pub fn presentation_facts(value: &mut Runtime) -> Result<PresentationFacts, Error> {
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
pub fn window_changed(value: &mut Runtime) {
    if !value.dead {
        if let Some(window) = value.window.as_ref() {
            window.borrow_mut().announce_change();
        }
    }
}

pub fn set_resizable(value: &mut Runtime, enabled: bool) -> Result<(), Error> {
    live_window("Runtime.set_resizable", value, &mut |w| w.set_resizable(enabled))
}

/// The native window's own background (sRGB, 0..1).
pub fn set_window_background(value: &mut Runtime, color: (f64, f64, f64)) -> Result<(), Error> {
    live_window("Runtime.set_window_background", value, &mut |w| w.set_background(color))
}

/// Hide and capture the pointer, reporting relative motion (fly cameras).
pub fn set_relative_mouse(value: &mut Runtime, enabled: bool) -> Result<(), Error> {
    live_window("Runtime.set_relative_mouse", value, &mut |w| w.set_relative_mouse(enabled))
}

pub fn set_cursor(value: &mut Runtime, shape: CursorShape) -> Result<(), Error> {
    live_window("Runtime.set_cursor", value, &mut |w| w.set_cursor(shape))
}

/// Text input runs only while a text field has focus: `Some (region, cursor)` (logical points)
/// starts it and places the input method there, `None` stops it.
pub fn set_text_input(value: &mut Runtime, area: Option<((i64, i64, i64, i64), i64)>) -> Result<(), Error> {
    live_window("Runtime.set_text_input", value, &mut |w| w.set_text_input(area))
}

pub fn show(value: &mut Runtime) -> Result<(), Error> {
    live_window("Runtime.show", value, &mut |w| w.show())?;
    let synced = sync_facts(value);
    if synced.is_ok() {
        value.presentation = None;
    }
    synced
}

/// Opens a native file dialog and returns at once with its id; the outcome is a
/// `Input.File_dialog` carrying the same id.
pub fn show_dialog(
    value: &mut Runtime,
    filters: Option<&[DialogFilter]>,
    default_location: Option<&str>,
    kind: DialogKind,
) -> Result<i64, Error> {
    let mut id = 0;
    live_window("Runtime.show_dialog", value, &mut |w| {
        id = w.show_dialog(filters.unwrap_or(&[]), default_location, kind)?;
        Ok(())
    })
    .map_err(|mut e| {
        if host::DIALOGS_UNSUPPORTED && e.kind == ErrorKind::InvalidState {
            e.kind = ErrorKind::Unsupported;
        }
        e
    })?;
    Ok(id)
}

/// Shown and neither minimized nor covered; see `Window::visible`.
pub fn visible(value: &mut Runtime) -> Result<bool, Error> {
    let mut seen = false;
    live_window("Runtime.visible", value, &mut |w| {
        seen = w.visible()?;
        Ok(())
    })?;
    Ok(seen)
}

pub fn destroy(value: &mut Runtime) -> Result<(), Error> {
    if value.dead {
        return Ok(());
    }
    value.dead = true;
    match value.window.as_ref() {
        Some(window) => window.borrow_mut().destroy().map_err(|text| fail_error("Runtime.destroy", &text)),
        None => Ok(()),
    }
}

pub fn is_window(value: &Runtime) -> bool {
    value.window.is_some()
}

pub fn surface_configuration(value: &mut Runtime) -> Result<surface::Configuration, Error> {
    let layer = match value.window.as_ref() {
        None => None,
        Some(w) => Some(w.borrow_mut().token().map_err(|text| fail_error("Runtime.surface_configuration", &text))?),
    };
    let f = &value.facts;
    let vsync = value.window.as_ref().is_some_and(|w| w.borrow().vsync());
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
