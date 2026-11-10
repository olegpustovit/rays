//! The Wayland client window host, pxui_sdl's Linux sibling: the same function names, so an app links one or the
//! other. An xdg-shell toplevel, seat input translated to `input` events, text-input-v3, the clipboard, fractional
//! or integer scale and the native layer token (`WaylandLayer`) a Vulkan backend presents to. Optional protocols
//! (fractional scale, viewporter, cursor shape, decorations, text input, data device) degrade to no-ops.
//! Empty on other targets.
#![cfg(target_os = "linux")]
use native_layer_token_sys::WaylandLayer;
use std::ffi::{CStr, CString, c_char, c_void};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicI64, Ordering};
use std::time::Instant;

pub mod translate;

pub use native_layer_token_sys::portable::Token;

pub type Error = String;

/// Window size in logical points and drawable size in pixels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Extent {
    pub logical: (i64, i64),
    pub drawable: (i64, i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorShape {
    Default,
    HorizontalResize,
    VerticalResize,
    Text,
}

/// Which optional protocols the compositor offered, for logs and tests.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Protocols(pub i32);
impl Protocols {
    pub const VIEWPORTER: i32 = 1;
    pub const FRACTIONAL_SCALE: i32 = 2;
    pub const CURSOR_SHAPE: i32 = 4;
    pub const DECORATION: i32 = 8;
    pub const TEXT_INPUT: i32 = 16;
    pub const DATA_DEVICE: i32 = 32;
    pub const SEAT: i32 = 64;
    pub const POINTER: i32 = 128;
    pub const KEYBOARD: i32 = 256;
    pub const SERVER_DECORATED: i32 = 512;
    pub fn has(self, bit: i32) -> bool {
        self.0 & bit != 0
    }
}

/// Where a requested paste reads from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PasteSource {
    /// The compositor's selection offer, read through a pipe.
    Selection,
    /// This window's own clipboard text: the compositor sent it no offer (it lacks keyboard focus).
    Own,
    /// Nothing to paste: an empty Paste event follows.
    Empty,
}

#[repr(C)]
struct RawEvent {
    kind: i32,
    a: i32,
    b: i32,
    c: i32,
    d: i32,
    e: i32,
    time: u32,
    x: f64,
    y: f64,
    text: *mut c_char,
}

// Mirrors the enum at the top of shim.c.
const KEY: i32 = 1;
const MOTION: i32 = 2;
const BUTTON: i32 = 3;
const AXIS: i32 = 4;
const AXIS_DISCRETE: i32 = 5;
const AXIS_SOURCE: i32 = 6;
const AXIS_STOP: i32 = 7;
const FRAME: i32 = 8;
const FOCUS: i32 = 9;
const CLOSE: i32 = 10;
const PREEDIT: i32 = 11;
const COMMIT: i32 = 12;
const PASTE: i32 = 13;
const VISIBLE: i32 = 14;

unsafe extern "C" {
    fn rw_open(title: *const c_char, width: i32, height: i32, error: *mut i32) -> *mut c_void;
    fn rw_close(window: *mut c_void);
    fn rw_display(window: *mut c_void) -> *mut c_void;
    fn rw_surface(window: *mut c_void) -> *mut c_void;
    fn rw_protocols(window: *mut c_void) -> i32;
    fn rw_state(window: *mut c_void, width: *mut i32, height: *mut i32, scale120: *mut i32, rate: *mut i32, delay: *mut i32);
    fn rw_dispatch(window: *mut c_void, timeout_ms: i32) -> i32;
    fn rw_next(window: *mut c_void, out: *mut RawEvent) -> i32;
    fn rw_event_free(text: *mut c_char);
    fn rw_resize(window: *mut c_void, width: i32, height: i32);
    fn rw_set_resizable(window: *mut c_void, enabled: i32);
    fn rw_set_cursor(window: *mut c_void, shape: i32) -> i32;
    fn rw_set_text_input(window: *mut c_void, enabled: i32, x: i32, y: i32, width: i32, height: i32) -> i32;
    fn rw_clipboard_set(window: *mut c_void, text: *const c_char) -> i32;
    fn rw_request_paste(window: *mut c_void) -> i32;
}

unsafe extern "C" fn keep(_: *mut c_void) {}
unsafe extern "C" fn free_layer(layer: *mut c_void) {
    // SAFETY: the token owns the box made in `open` and releases it exactly once.
    drop(unsafe { Box::from_raw(layer.cast::<WaylandLayer>()) })
}

static NEXT_OWNER: AtomicI64 = AtomicI64::new(1);

fn dimension(value: i64) -> Result<i32, Error> {
    i32::try_from(value).ok().filter(|v| *v > 0).ok_or_else(|| "dimensions must be positive".to_owned())
}

pub struct Window {
    raw: Option<NonNull<c_void>>,
    token: Token,
    vsync: bool,
    high_density: bool,
    extent: Extent,
    /// The extent the last Resized/PixelSizeChanged events announced.
    reported: Extent,
    scale120: i32,
    pointer: (f64, f64),
    scroll: translate::Scroll,
    repeat: translate::Repeat,
    text_input: bool,
    cursor_shape: Option<CursorShape>,
    visible: bool,
    started: Instant,
}

impl Window {
    /// A vsynced, high-density window.
    pub fn open(title: &str, width: i64, height: i64) -> Result<Window, Error> {
        Window::open_with(title, width, height, true, false, true)
    }

    /// `high_density` false keeps the drawable at the logical size. `hidden` is accepted for pxui_sdl parity:
    /// a Wayland window is not shown until its first present anyway.
    pub fn open_with(title: &str, width: i64, height: i64, vsync: bool, hidden: bool, high_density: bool) -> Result<Window, Error> {
        let _ = hidden;
        let (w, h) = (dimension(width)?, dimension(height)?);
        let title = CString::new(title).map_err(|_| "title contains a NUL byte".to_owned())?;
        let mut error = 0;
        // SAFETY: plain values in; the shim returns an owned window or NULL with an error code.
        let raw = NonNull::new(unsafe { rw_open(title.as_ptr(), w, h, &mut error) }).ok_or_else(|| {
            match error {
                1 => "no Wayland compositor (WAYLAND_DISPLAY / XDG_RUNTIME_DIR)",
                2 => "the compositor lacks wl_compositor or xdg_wm_base",
                3 => "the compositor sent no first configure",
                _ => "out of memory",
            }
            .to_owned()
        })?;
        // SAFETY: display and surface live until rw_close, which `destroy` runs only after invalidating the token.
        let layer = Box::into_raw(Box::new(unsafe { WaylandLayer { display: rw_display(raw.as_ptr()), surface: rw_surface(raw.as_ptr()) } }));
        let owner = NEXT_OWNER.fetch_add(1, Ordering::Relaxed);
        // SAFETY: the layer box stays valid until the paired release callback frees it.
        let token = match unsafe { native_layer_token_sys::Token::from_raw(layer.cast(), owner, 1, Some(keep), Some(free_layer)) } {
            Ok(token) => Token::from_sys(token),
            Err(e) => {
                // SAFETY: the box was not handed to a token; the window has no other owner yet.
                unsafe { drop(Box::from_raw(layer)); rw_close(raw.as_ptr()) };
                return Err(e.to_string());
            }
        };
        let extent = Extent { logical: (width, height), drawable: (width, height) };
        let mut window = Window {
            raw: Some(raw),
            token,
            vsync,
            high_density,
            extent,
            reported: extent,
            scale120: 120,
            pointer: (0.0, 0.0),
            scroll: translate::Scroll::default(),
            repeat: translate::Repeat::new(),
            text_input: false,
            cursor_shape: None,
            visible: true,
            started: Instant::now(),
        };
        window.read_state();
        window.reported = window.extent;
        Ok(window)
    }

    fn live(&self) -> Result<*mut c_void, Error> {
        self.raw.map(NonNull::as_ptr).ok_or_else(|| "window is destroyed".to_owned())
    }

    fn read_state(&mut self) {
        let Some(raw) = self.raw else { return };
        let (mut w, mut h, mut scale120, mut rate, mut delay) = (0, 0, 0, 0, 0);
        // SAFETY: a live window; plain out-parameters.
        unsafe { rw_state(raw.as_ptr(), &mut w, &mut h, &mut scale120, &mut rate, &mut delay) };
        self.scale120 = if self.high_density { scale120 } else { 120 };
        let logical = (i64::from(w.max(1)), i64::from(h.max(1)));
        self.extent = Extent { logical, drawable: translate::drawable(logical, self.scale120) };
        self.repeat.rate = rate;
        self.repeat.delay = delay;
    }

    /// Appends the events that arrived (positions in logical points) without blocking, and says whether the
    /// window is still open: false once the compositor asked to close it or the connection was lost.
    pub fn pump(&mut self, events: &mut Vec<input::Event>) -> bool {
        let Ok(raw) = self.live() else { return false };
        // SAFETY: a live window.
        let mut open = unsafe { rw_dispatch(raw, 0) } == 0;
        if !open {
            events.push(input::Event::Closed);
        }
        let now = self.time();
        let mut event = RawEvent { kind: 0, a: 0, b: 0, c: 0, d: 0, e: 0, time: 0, x: 0.0, y: 0.0, text: std::ptr::null_mut() };
        // SAFETY: a live window; each popped event's text is copied, then released to the shim.
        while unsafe { rw_next(raw, &mut event) } != 0 {
            let text = if event.text.is_null() {
                String::new()
            } else {
                let copied = unsafe { CStr::from_ptr(event.text) }.to_string_lossy().into_owned();
                unsafe { rw_event_free(event.text) };
                copied
            };
            open &= self.translate(&event, text, now, events);
        }
        if let Some((key, text)) = self.repeat.due(now) {
            events.push(input::Event::KeyPressed(key));
            if let Some(text) = text.filter(|_| self.text_input) {
                events.push(input::Event::TextInput(text));
            }
        }
        self.read_state();
        if self.extent.logical != self.reported.logical {
            events.push(input::Event::Resized(self.extent.logical));
        }
        if self.extent.drawable != self.reported.drawable {
            events.push(input::Event::PixelSizeChanged(self.extent.drawable));
        }
        self.reported = self.extent;
        open
    }

    fn translate(&mut self, raw: &RawEvent, text: String, now: f64, events: &mut Vec<input::Event>) -> bool {
        use input::Event;
        match raw.kind {
            KEY => {
                let key = translate::key(raw.b as u32, raw.c as u32, raw.x as u32);
                if raw.a != 0 {
                    events.push(Event::KeyPressed(key));
                    let typed = translate::typed(&text, raw.e).filter(|_| self.text_input).map(str::to_owned);
                    if let Some(typed) = &typed {
                        events.push(Event::TextInput(typed.clone()));
                    }
                    self.repeat.press(raw.b, key, typed, raw.d != 0, now);
                } else {
                    events.push(Event::KeyReleased(key));
                    self.repeat.release(raw.b);
                }
            }
            MOTION => {
                self.pointer = (raw.x, raw.y);
                events.push(Event::PointerMoved(self.pointer));
            }
            BUTTON => {
                if let Some(button) = translate::button(raw.a) {
                    events.push(if raw.b != 0 { Event::PointerPressed(button, self.pointer) } else { Event::PointerReleased(button, self.pointer) });
                }
            }
            AXIS => self.scroll.axis(raw.a, raw.x, raw.time),
            AXIS_DISCRETE => self.scroll.steps(raw.a, raw.b),
            AXIS_SOURCE => self.scroll.source(raw.a),
            AXIS_STOP => self.scroll.stop(raw.time),
            FRAME => self.scroll.frame(events),
            FOCUS if raw.a != 0 => events.push(Event::FocusGained),
            FOCUS => {
                self.repeat.clear();
                events.push(Event::FocusLost);
            }
            CLOSE => {
                events.push(Event::Closed);
                return false;
            }
            PREEDIT => {
                let (start, length) = translate::preedit(&text, raw.a, raw.b);
                events.push(Event::TextEditing { text, start, length });
            }
            COMMIT => events.push(Event::TextInput(text)),
            PASTE => events.push(Event::Paste(text)),
            VISIBLE => {
                self.visible = raw.a != 0;
                events.push(Event::VisibilityChanged(self.visible));
            }
            _ => {}
        }
        true
    }

    /// Sleeps until an event arrives, a key repeat is due or `deadline` (seconds on the `time` clock) passes;
    /// `None` waits for an event. The events are then ready for the next `pump`.
    pub fn wait(&mut self, deadline: Option<f64>) {
        let Ok(raw) = self.live() else { return };
        let deadline = match (deadline, self.repeat.deadline()) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (a, b) => a.or(b),
        };
        let timeout = deadline.map_or(-1, |at| ((at - self.time()).max(0.0) * 1000.0).ceil().min(f64::from(i32::MAX)) as i32);
        // SAFETY: a live window. A lost connection is reported by the next pump.
        unsafe { rw_dispatch(raw, timeout) };
    }

    /// Logical size in points.
    pub fn size(&self) -> (i64, i64) {
        self.extent.logical
    }

    pub fn extent(&self) -> Extent {
        self.extent
    }

    /// Drawable pixels per logical point: the compositor's fractional scale (e.g. 1.25), else the integer
    /// scale of the outputs the window is on.
    pub fn scale(&self) -> f64 {
        f64::from(self.scale120) / 120.0
    }

    /// Seconds since the window opened.
    pub fn time(&self) -> f64 {
        self.started.elapsed().as_secs_f64()
    }

    pub fn vsync(&self) -> bool {
        self.vsync
    }

    /// Window coordinates per logical point: Wayland surface coordinates are logical points.
    pub fn unit(&self) -> f64 {
        1.0
    }

    /// Drawable pixels per logical point as the compositor reports it.
    pub fn pixel_density(&self) -> Result<f64, Error> {
        self.live()?;
        Ok(self.scale())
    }

    /// The `WaylandLayer` a Vulkan backend presents to. Destroy GPU surfaces made from it before the window.
    pub fn token(&mut self) -> Result<Token, Error> {
        self.live()?;
        Ok(self.token.clone())
    }

    /// The optional protocols the compositor offered.
    pub fn protocols(&self) -> Result<Protocols, Error> {
        // SAFETY: a live window.
        Ok(Protocols(unsafe { rw_protocols(self.live()?) }))
    }

    /// Resizes to `width`x`height` logical points: a Wayland client picks its own size, so this takes effect
    /// with the next present, and the next `pump` announces it.
    pub fn resize(&mut self, width: i64, height: i64) -> Result<(), Error> {
        let raw = self.live()?;
        let (w, h) = (dimension(width)?, dimension(height)?);
        // SAFETY: a live window.
        unsafe { rw_resize(raw, w, h) };
        self.read_state();
        Ok(())
    }

    pub fn set_resizable(&mut self, enabled: bool) -> Result<(), Error> {
        // SAFETY: a live window.
        unsafe { rw_set_resizable(self.live()?, i32::from(enabled)) };
        Ok(())
    }

    /// Without cursor-shape-v1 the compositor keeps its own cursor.
    // ponytail: no wl_cursor theme fallback (libwayland-cursor + a shm buffer); add it for compositors without cursor-shape-v1.
    pub fn set_cursor(&mut self, shape: CursorShape) -> Result<(), Error> {
        let raw = self.live()?;
        if self.cursor_shape == Some(shape) {
            return Ok(());
        }
        // wp_cursor_shape_device_v1.shape: default, text, ew-resize, ns-resize
        let native = match shape {
            CursorShape::Default => 1,
            CursorShape::Text => 9,
            CursorShape::HorizontalResize => 26,
            CursorShape::VerticalResize => 27,
        };
        // SAFETY: a live window.
        unsafe { rw_set_cursor(raw, native) };
        self.cursor_shape = Some(shape);
        Ok(())
    }

    /// Text input runs only while a text field has focus: `Some((region, cursor))` (logical points, `cursor` an
    /// x offset into the region) starts it and places the input method's caret there, `None` stops it. Typed
    /// text arrives as TextInput, the input method's composition as TextEditing and its result as TextInput.
    pub fn set_text_input(&mut self, area: Option<((i64, i64, i64, i64), i64)>) -> Result<(), Error> {
        let raw = self.live()?;
        let clamp = |v: i64| v.clamp(i64::from(i32::MIN), i64::from(i32::MAX)) as i32;
        self.text_input = area.is_some();
        let ((x, y, _, height), cursor) = area.unwrap_or(((0, 0, 0, 0), 0));
        // SAFETY: a live window.
        unsafe { rw_set_text_input(raw, i32::from(area.is_some()), clamp(x + cursor), clamp(y), 1, clamp(height)) };
        Ok(())
    }

    /// Offers `text` as the clipboard selection. Fails without a data device.
    pub fn clipboard_set_text(&mut self, text: &str) -> Result<(), Error> {
        let raw = self.live()?;
        let text = CString::new(text).map_err(|_| "clipboard text contains a NUL byte".to_owned())?;
        // SAFETY: a live window; the shim copies the text.
        match unsafe { rw_clipboard_set(raw, text.as_ptr()) } {
            0 => Ok(()),
            1 => Err("the compositor offers no wl_data_device_manager".to_owned()),
            _ => Err("out of memory".to_owned()),
        }
    }

    /// Asks for the clipboard's text: exactly one `Event::Paste` arrives with a later `pump` (empty when the
    /// clipboard holds no text). Wayland reads the selection through a pipe, so the paste cannot be synchronous.
    pub fn request_paste(&mut self) -> Result<PasteSource, Error> {
        // SAFETY: a live window.
        Ok(match unsafe { rw_request_paste(self.live()?) } {
            0 => PasteSource::Selection,
            1 => PasteSource::Own,
            _ => PasteSource::Empty,
        })
    }

    /// False while the compositor has suspended the window (xdg_toplevel `suspended`, e.g. fully hidden).
    pub fn visible(&mut self) -> Result<bool, Error> {
        self.live()?;
        Ok(self.visible)
    }

    /// Invalidates the token and closes the window. GPU surfaces made from the token must be destroyed first.
    pub fn destroy(&mut self) -> Result<(), Error> {
        if let Some(raw) = self.raw.take() {
            self.token.invalidate();
            // SAFETY: the window is live and no longer reachable from self.
            unsafe { rw_close(raw.as_ptr()) };
        }
        Ok(())
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        let _ = self.destroy();
    }
}
