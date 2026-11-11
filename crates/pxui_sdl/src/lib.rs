//! The SDL3 window host: window lifecycle, the event pump translated to `input` events, cursor,
//! text input, clipboard and the native layer token a GPU backend presents to. A leaf: it knows
//! nothing of the UI engine or the renderer; callers pass plain values.
use sdl3::cursor::{Cursor, Shape};
use sdl3::window::{Flag, Window as SdlWindow};
use sdl3::{DialogFilter as SdlDialogFilter, DialogKind as SdlDialogKind, NativeView, Rect};
use std::time::Instant;

pub mod translate;

pub use native_layer_token_sys::portable::Token;
/// Native binary startup policy; does not acquire SDL subsystems or change their ownership.
pub use sdl3::init::configure_native_process;

pub type Error = String;

/// Window size in logical points and drawable size in pixels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Extent {
    pub logical: (i64, i64),
    pub drawable: (i64, i64),
}

/// SDL announces a change of size or density with an event instead of making every frame ask.
/// The announcement is one bit: set by the event, taken by the next frame, which re-reads the
/// window only if it was set.
pub mod change_flag {
    #[derive(Debug)]
    pub struct ChangeFlag {
        announced: bool,
    }

    pub fn create() -> ChangeFlag {
        ChangeFlag { announced: false }
    }

    pub fn announce(flag: &mut ChangeFlag) {
        flag.announced = true;
    }

    /// Clears the announcement and says whether there was one.
    pub fn take(flag: &mut ChangeFlag) -> bool {
        std::mem::replace(&mut flag.announced, false)
    }

    /// `query` runs once per announcement, however many arrived; true when it ran.
    pub fn refresh(flag: &mut ChangeFlag, query: &mut dyn FnMut()) -> bool {
        let announced = take(flag);
        if announced {
            query();
        }
        announced
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorShape {
    Default,
    HorizontalResize,
    VerticalResize,
    Text,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DialogKind {
    OpenFile,
    OpenFiles,
    SaveFile,
    OpenFolder,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DialogFilter {
    pub name: String,
    pub pattern: String,
}

pub struct Window {
    handle: SdlWindow,
    view: NativeView,
    vsync: bool,
    cursors: Vec<(CursorShape, Cursor)>,
    cursor_shape: Option<CursorShape>,
    text_input: bool,
    changed: change_flag::ChangeFlag,
    /// window coordinates per logical point: SDL's display scale over its pixel density
    unit: f64,
    extent: Extent,
    started: Instant,
    dead: bool,
}

fn fail(failure: sdl3::Error) -> Error {
    failure.to_string()
}

pub fn clipboard_set_text(text: &str) -> Result<(), Error> {
    sdl3::clipboard::set_text(text).map_err(fail)
}

pub fn clipboard_get_text() -> Result<String, Error> {
    sdl3::clipboard::get_text().map_err(fail)
}

// macOS reports window coordinates in points (display scale 2 over density 2: one coordinate a
// point); Windows reports pixels with density 1 and the monitor's scaling as the display scale
// (1.5 at 150%). Rays lays out in points, so window coordinates are divided by that ratio here
// and multiplied back where a size or a rect goes to SDL.
fn window_unit(window: &SdlWindow) -> Result<f64, Error> {
    let scale = window.display_scale().map_err(fail)? as f64;
    let density = window.pixel_density().map_err(fail)? as f64;
    let unit = scale / density;
    Ok(if (unit - 1.).abs() < 1e-3 { 1. } else { unit })
}

// Round halfway values away from zero.
fn to_points(unit: f64, size: i32) -> i64 {
    std::cmp::max(1, ((size as f64) / unit).round() as i64)
}

fn to_window(unit: f64, size: i64) -> i32 {
    std::cmp::max(1, ((size as f64) * unit).round() as i64) as i32
}

fn extent_in(unit: f64, window: &SdlWindow) -> Result<Extent, Error> {
    let (lw, lh) = window.size().map_err(fail)?;
    let (dw, dh) = window.size_in_pixels().map_err(fail)?;
    if lw > 0 && lh > 0 && dw > 0 && dh > 0 {
        Ok(Extent { logical: (to_points(unit, lw), to_points(unit, lh)), drawable: (dw as i64, dh as i64) })
    } else {
        Err("window dimensions are invalid".to_owned())
    }
}

// Every step is checked: the first failure is the result.
fn reveal(window: &SdlWindow) -> Result<(), sdl3::Error> {
    window.show()?;
    window.restore()?;
    window.raise_window()?;
    window.center()?;
    window.sync()
}

impl Window {
    /// A visible, vsynced, high-density window.
    pub fn open(title: &str, width: i64, height: i64) -> Result<Window, Error> {
        Window::open_with(title, width, height, true, false, true)
    }

    /// `high_density` asks for a drawable at the display's native density; false keeps the
    /// drawable at the logical size, so a capture is the same bytes on every display.
    pub fn open_with(title: &str, width: i64, height: i64, vsync: bool, hidden: bool, high_density: bool) -> Result<Window, Error> {
        if width <= 0 || height <= 0 {
            return Err("dimensions must be positive".to_owned());
        }
        sdl3::init::init(&[sdl3::init::Subsystem::Video]).map_err(fail)?;
        // The window and view release themselves when `build` returns; a failure then quits video.
        let build = || -> Result<Window, Error> {
            // SDL itself turns a Control-click into the secondary click.
            sdl3::hint::control_click_is_right_click(true).map_err(fail)?;
            if cfg!(windows) {
                sdl3::hint::mouse_relative_system_scale(true).map_err(fail)?;
            }
            let mut flags = vec![Flag::NativeView];
            if high_density {
                flags.push(Flag::HighPixelDensity);
            }
            if hidden {
                flags.push(Flag::Hidden);
            }
            let handle = SdlWindow::create(title, width as i32, height as i32, &flags).map_err(fail)?;
            let unit = window_unit(&handle)?;
            if unit != 1. {
                handle.set_size(to_window(unit, width), to_window(unit, height)).map_err(fail)?;
            }
            if !hidden {
                reveal(&handle).map_err(fail)?;
            }
            let mut view = NativeView::create(&handle).map_err(fail)?;
            let _token = view.layer().map_err(fail)?;
            // Size the target for the actual drawable once, rather than resizing it after.
            let extent = extent_in(unit, &handle)?;
            Ok(Window {
                handle,
                view,
                vsync,
                cursors: Vec::new(),
                cursor_shape: None,
                text_input: false,
                changed: change_flag::create(),
                unit,
                extent,
                started: Instant::now(),
                dead: false,
            })
        };
        let created = build();
        if created.is_err() {
            let _ = sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video]);
        }
        created
    }

    fn live(&self) -> Result<(), Error> {
        if self.dead { Err("window is destroyed".to_owned()) } else { Ok(()) }
    }

    /// Appends the frame's events (positions in logical points) and says whether the window is
    /// still open: false once a close request or quit arrived.
    pub fn pump(&mut self, events: &mut Vec<input::Event>) -> bool {
        let mut open = true;
        if let Ok(polled) = sdl3::event::poll_coalesced() {
            for raw in polled {
                if matches!(raw, sdl3::Event::Window(sdl3::WindowChange::Resized(..) | sdl3::WindowChange::PixelSizeChanged(..))) {
                    change_flag::announce(&mut self.changed);
                }
                if let Some(event) = translate::translate(translate::in_points(self.unit, raw)) {
                    open &= !matches!(event, input::Event::Closed);
                    events.push(event);
                }
            }
        }
        // ponytail: a failed re-read keeps the last extent; surfacing it needs a pump error channel
        let _ = self.sync();
        open
    }

    /// Sleeps until an event arrives or `deadline` (seconds on the `time` clock) passes; `None` waits
    /// for an event. The events stay queued for the next `pump` (`pxui_wayland::Window::wait`).
    pub fn wait(&mut self, deadline: Option<f64>) {
        let ms = deadline.map_or(i32::MAX, |at| ((at - self.time()).max(0.0) * 1000.0).ceil().min(f64::from(i32::MAX)) as i32);
        let _ = sdl3::event::wait(ms);
    }

    /// Logical size in points.
    pub fn size(&self) -> (i64, i64) {
        self.extent.logical
    }

    pub fn extent(&self) -> Extent {
        self.extent
    }

    /// Drawable pixels per logical point.
    pub fn scale(&self) -> f64 {
        self.extent.drawable.0 as f64 / self.extent.logical.0 as f64
    }

    /// Seconds since the window opened.
    pub fn time(&self) -> f64 {
        self.started.elapsed().as_secs_f64()
    }

    pub fn vsync(&self) -> bool {
        self.vsync
    }

    /// Window coordinates per logical point: 1 on macOS, the monitor's scaling (1.5 at 150%) on
    /// Windows, whose SDL events and sizes are in pixels.
    pub fn unit(&self) -> f64 {
        self.unit
    }

    /// The native layer a GPU backend presents to.
    pub fn token(&mut self) -> Result<Token, Error> {
        self.live()?;
        self.view.layer().map_err(fail)
    }

    /// Drawable pixels per logical point as the display reports it.
    pub fn pixel_density(&self) -> Result<f64, Error> {
        Ok(self.handle.pixel_density().map_err(fail)? as f64 * self.unit)
    }

    /// Tell the window that SDL announced a size or density change, so `sync` re-reads it.
    /// The window is not polled between announcements.
    pub fn announce_change(&mut self) {
        change_flag::announce(&mut self.changed);
    }

    /// Re-reads unit and extent if a change was announced; no SDL call otherwise.
    pub fn sync(&mut self) -> Result<(), Error> {
        if self.dead || !change_flag::take(&mut self.changed) {
            return Ok(());
        }
        self.unit = window_unit(&self.handle)?;
        self.extent = extent_in(self.unit, &self.handle)?;
        Ok(())
    }

    /// Resizes to `width`x`height` logical points.
    pub fn resize(&mut self, width: i64, height: i64) -> Result<(), Error> {
        self.live()?;
        if width <= 0 || height <= 0 {
            return Err("dimensions must be positive".to_owned());
        }
        self.handle.set_size(to_window(self.unit, width), to_window(self.unit, height)).map_err(fail)?;
        self.extent = extent_in(self.unit, &self.handle)?;
        Ok(())
    }

    pub fn set_resizable(&mut self, enabled: bool) -> Result<(), Error> {
        self.live()?;
        self.handle.set_resizable(enabled).map_err(fail)
    }

    /// The native window's own background (sRGB, 0..1): what the title bar shows, since a Rays
    /// window hides its title and keeps only the traffic lights.
    pub fn set_background(&mut self, color: (f64, f64, f64)) -> Result<(), Error> {
        self.live()?;
        self.handle.set_background(color.0 as f32, color.1 as f32, color.2 as f32).map_err(fail)
    }

    /// Hide and capture the pointer, reporting relative motion (fly cameras).
    pub fn set_relative_mouse(&mut self, enabled: bool) -> Result<(), Error> {
        self.live()?;
        self.handle.set_relative_mouse(enabled).map_err(fail)
    }

    pub fn set_cursor(&mut self, shape: CursorShape) -> Result<(), Error> {
        self.live()?;
        if self.cursor_shape == Some(shape) {
            return Ok(());
        }
        let index = match self.cursors.iter().position(|(s, _)| *s == shape) {
            Some(i) => i,
            None => {
                let native = match shape {
                    CursorShape::Default => Shape::Default,
                    CursorShape::HorizontalResize => Shape::EwResize,
                    CursorShape::VerticalResize => Shape::NsResize,
                    CursorShape::Text => Shape::Text,
                };
                // Lookup is by key; retained list order determines destruction order.
                self.cursors.insert(0, (shape, Cursor::create(native).map_err(fail)?));
                0
            }
        };
        self.cursors[index].1.set().map_err(fail)?;
        self.cursor_shape = Some(shape);
        Ok(())
    }

    /// Text input runs only while a text field has focus: `Some (region, cursor)` (logical points)
    /// starts it and places the input method there, `None` stops it. Repeating the same answer
    /// costs no SDL call.
    pub fn set_text_input(&mut self, area: Option<((i64, i64, i64, i64), i64)>) -> Result<(), Error> {
        self.live()?;
        match area {
            None => {
                if self.text_input {
                    sdl3::text_input::stop(&self.handle).map_err(fail)?;
                    self.text_input = false;
                }
                Ok(())
            }
            Some(((x, y, width, height), cursor)) => {
                if !self.text_input {
                    sdl3::text_input::start(&self.handle).map_err(fail)?;
                    self.text_input = true;
                }
                let unit = self.unit;
                let scale = |v: i64| (v as f64 * unit).round() as i32;
                sdl3::text_input::set_area(
                    &self.handle,
                    Rect { x: scale(x), y: scale(y), width: scale(width), height: scale(height) },
                    scale(cursor),
                )
                .map_err(fail)
            }
        }
    }

    pub fn show(&mut self) -> Result<(), Error> {
        self.live()?;
        reveal(&self.handle).map_err(fail)?;
        self.announce_change();
        self.sync()
    }

    /// Opens a native file dialog and returns at once with its id; the outcome is an
    /// `Event::FileDialog` carrying the same id. `pattern` lists extensions separated by
    /// semicolons ("png;jpg"), or "*". At most 8 dialogs are open at once; a ninth is an error.
    pub fn show_dialog(&mut self, filters: &[DialogFilter], default_location: Option<&str>, kind: DialogKind) -> Result<i64, Error> {
        self.live()?;
        let filters: Vec<SdlDialogFilter> =
            filters.iter().map(|f| SdlDialogFilter { name: f.name.clone(), pattern: f.pattern.clone() }).collect();
        let native = match kind {
            DialogKind::OpenFile => SdlDialogKind::OpenFile,
            DialogKind::OpenFiles => SdlDialogKind::OpenFiles,
            DialogKind::SaveFile => SdlDialogKind::SaveFile,
            DialogKind::OpenFolder => SdlDialogKind::OpenFolder,
        };
        Ok(sdl3::dialog::show(&self.handle, native, &filters, default_location).map_err(fail)? as i64)
    }

    /// Shown and neither minimized nor covered. SDL announces the start of occlusion and not its
    /// end, so a caller that saw the window hidden asks this once per frame to learn when it is back.
    pub fn visible(&mut self) -> Result<bool, Error> {
        self.live()?;
        let state = self.handle.state().map_err(fail)?;
        Ok(!(state.hidden || state.minimized || state.occluded))
    }

    pub fn destroy(&mut self) -> Result<(), Error> {
        if self.dead {
            return Ok(());
        }
        self.dead = true;
        let mut failure: Option<Error> = None;
        let mut record = |result: Result<(), sdl3::Error>| {
            if let Err(e) = result {
                failure.get_or_insert(e.to_string());
            }
        };
        if self.text_input {
            record(sdl3::text_input::stop(&self.handle));
        }
        for (_, cursor) in self.cursors.iter_mut() {
            record(cursor.destroy());
        }
        self.cursors.clear();
        record(self.view.destroy());
        record(self.handle.destroy());
        record(sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video]));
        failure.map_or(Ok(()), Err)
    }
}
