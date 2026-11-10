use std::cell::Cell;
use std::ffi::{CStr, CString, c_char, c_void};
use std::rc::Rc;
use std::marker::PhantomData;
use std::ptr::NonNull;

use native_layer_token_sys::{Release, Retain, Token as LayerToken};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    Sdl,
    Unsupported,
    InvalidArgument,
    Destroyed,
    ParentHasDependents,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String,
}


fn invalid(message: &str) -> Error {
    Error { kind: ErrorKind::InvalidArgument, message: message.to_string() }
}

fn input(value: &str, what: &str) -> std::result::Result<CString, Error> {
    CString::new(value).map_err(|_| invalid(&format!("{what} contains a NUL byte")))
}

unsafe fn string(pointer: *const c_char) -> String {
    if pointer.is_null() {
        String::new()
    } else {
        // SAFETY: SDL-owned strings are NUL-terminated and remain valid until the next SDL call.
        unsafe { CStr::from_ptr(pointer) }.to_string_lossy().into_owned()
    }
}

unsafe fn owned_string(pointer: *mut c_char) -> Option<String> {
    if pointer.is_null() {
        None
    } else {
        // SAFETY: this pointer is returned by an SDL allocation function and is freed below.
        let value = unsafe { string(pointer) };
        // SAFETY: SDL allocated the text; the C bridge frees it with SDL_free.
        unsafe { ffi::rays_sdl3_free(pointer.cast()) };
        Some(value)
    }
}

fn sdl_error(kind: ErrorKind, fallback: &str) -> Error {
    // SAFETY: SDL_GetError returns a borrowed, NUL-terminated thread-local string.
    let message = unsafe { string(ffi::rays_sdl3_get_error()) };
    Error { kind, message: if message.is_empty() { fallback.to_owned() } else { message } }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Version {
    pub major: i32,
    pub minor: i32,
    pub patch: i32,
}

fn version(number: i32) -> Version {
    Version { major: number / 1_000_000, minor: number / 1_000 % 1_000, patch: number % 1_000 }
}

pub fn linked_version() -> Version {
    // SAFETY: this is a pure SDL version query.
    version(unsafe { ffi::rays_sdl3_linked_version() })
}

pub fn compiled_version() -> Version {
    // SAFETY: this returns the version macro from the checked SDL headers.
    version(unsafe { ffi::rays_sdl3_compiled_version() })
}

pub fn platform_name() -> String {
    // SAFETY: SDL_GetPlatform returns a borrowed, NUL-terminated string.
    unsafe { string(ffi::rays_sdl3_platform_name()) }
}

pub fn pref_path(org: &str, app: &str) -> std::result::Result<Option<String>, Error> {
    let org = input(org, "organization")?;
    let app = input(app, "application")?;
    // SAFETY: both CString arguments stay alive for the call; SDL returns an SDL_free allocation.
    Ok(unsafe { owned_string(ffi::rays_sdl3_pref_path(org.as_ptr(), app.as_ptr())) })
}

pub fn is_main_thread() -> bool {
    // SAFETY: the platform bridge compares the current thread to the process main thread.
    unsafe { ffi::rays_sdl3_is_main_thread() }
}

/// Sets the native binary's fatal-error policy without initializing or owning SDL.
pub fn configure_fatal_errors() {
    // SAFETY: the bridge only sets process error-reporting flags; the safe SDL layer
    // requires the initial thread before exposing this process-wide startup operation.
    unsafe { ffi::rays_sdl3_configure_fatal_errors() }
}

pub fn clear_error() {
    // SAFETY: clears SDL's thread-local error text.
    unsafe { ffi::rays_sdl3_clear_error() }
}

pub fn init_subsystems(bits: u32) -> std::result::Result<(), Error> {
    // SAFETY: subsystem bits are translated to SDL flags in the C bridge.
    if unsafe { ffi::rays_sdl3_init_subsystem(bits) } {
        Ok(())
    } else {
        Err(sdl_error(ErrorKind::Sdl, "SDL initialization failed without an error"))
    }
}

pub fn quit_subsystems(bits: u32) {
    // SAFETY: subsystem bits are translated to SDL flags in the C bridge.
    unsafe { ffi::rays_sdl3_quit_subsystem(bits) }
}

pub fn set_control_click_right_click(enabled: bool) -> std::result::Result<(), Error> {
    // SAFETY: passes a boolean to SDL's named hint.
    if unsafe { ffi::rays_sdl3_set_control_click_right_click(enabled) } {
        Ok(())
    } else {
        Err(sdl_error(ErrorKind::Sdl, "SDL rejected the Control-click hint"))
    }
}

pub fn set_mouse_relative_system_scale(enabled: bool) -> std::result::Result<(), Error> {
    // SAFETY: passes a boolean to SDL's named hint.
    if unsafe { ffi::rays_sdl3_set_mouse_relative_system_scale(enabled) } {
        Ok(())
    } else {
        Err(sdl_error(ErrorKind::Sdl, "SDL rejected the relative-mouse scaling hint"))
    }
}

pub fn set_hint(name: &str, value: &str) -> std::result::Result<(), Error> {
    let name = input(name, "hint name")?;
    let value = input(value, "hint value")?;
    // SAFETY: both CStrings live through SDL's copying call.
    if unsafe { ffi::rays_sdl3_set_hint(name.as_ptr(), value.as_ptr()) } {
        Ok(())
    } else {
        Err(sdl_error(ErrorKind::Sdl, "SDL rejected the hint"))
    }
}

pub fn clipboard_set_text(text: &str) -> std::result::Result<(), Error> {
    let text = input(text, "clipboard text")?;
    // SAFETY: the CString lives through SDL's copying call.
    if unsafe { ffi::rays_sdl3_clipboard_set_text(text.as_ptr()) } {
        Ok(())
    } else {
        Err(sdl_error(ErrorKind::Sdl, "SDL clipboard write failed"))
    }
}

pub fn clipboard_get_text() -> std::result::Result<String, Error> {
    // SAFETY: SDL returns an SDL_free allocation, copied and freed immediately.
    unsafe { owned_string(ffi::rays_sdl3_clipboard_get_text()) }
        .ok_or_else(|| sdl_error(ErrorKind::Sdl, "SDL clipboard read failed"))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorShape {
    Default,
    Text,
    EwResize,
    NsResize,
}

impl CursorShape {
    fn code(self) -> i32 {
        match self { Self::Default => 0, Self::Text => 1, Self::EwResize => 2, Self::NsResize => 3 }
    }
}

pub struct Cursor {
    raw: Option<NonNull<ffi::SDLCursor>>,
    _main_thread_only: PhantomData<Rc<()>>,
}

impl Cursor {
    pub fn create(shape: CursorShape) -> std::result::Result<Self, Error> {
        clear_error();
        // SAFETY: shape is mapped to a checked native cursor enum in the bridge.
        let raw = unsafe { ffi::rays_sdl3_create_cursor(shape.code()) };
        let raw = NonNull::new(raw)
            .ok_or_else(|| sdl_error(ErrorKind::Unsupported, "system cursors are unavailable"))?;
        Ok(Self { raw: Some(raw), _main_thread_only: PhantomData })
    }

    pub fn set(&self) -> std::result::Result<(), Error> {
        let raw = self.raw.ok_or_else(|| destroyed("cursor is destroyed"))?;
        clear_error();
        // SAFETY: this owned cursor remains live through the call.
        if unsafe { ffi::rays_sdl3_set_cursor(raw.as_ptr()) } {
            Ok(())
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not set the cursor"))
        }
    }

    pub fn destroy(&mut self) {
        if let Some(raw) = self.raw.take() {
            // SAFETY: this cursor is uniquely owned and destroyed exactly once.
            unsafe { ffi::rays_sdl3_destroy_cursor(raw.as_ptr()) }
        }
    }
}

impl Drop for Cursor {
    fn drop(&mut self) {
        if let Some(raw) = self.raw.take() {
            // SAFETY: this cursor is uniquely owned and destroyed exactly once.
            unsafe { ffi::rays_sdl3_destroy_cursor(raw.as_ptr()) }
        }
    }
}

fn destroyed(message: &str) -> Error {
    Error { kind: ErrorKind::Destroyed, message: message.to_owned() }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WindowFlag {
    Hidden,
    HighPixelDensity,
    Metal,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WindowState {
    pub hidden: bool,
    pub minimized: bool,
    pub occluded: bool,
}

struct WindowHandle {
    raw: Cell<Option<NonNull<ffi::SDLWindow>>>,
}

impl WindowHandle {
    fn raw(&self) -> std::result::Result<NonNull<ffi::SDLWindow>, Error> {
        self.raw.get().ok_or_else(|| destroyed("window is destroyed"))
    }

    fn destroy(&self) {
        if let Some(raw) = self.raw.take() {
            // SAFETY: this shared handle stores one SDL window and clears it before destruction.
            unsafe { ffi::rays_sdl3_destroy_window(raw.as_ptr()) }
        }
    }
}

impl Drop for WindowHandle {
    fn drop(&mut self) { self.destroy(); }
}

pub struct Window {
    handle: Rc<WindowHandle>,
    _main_thread_only: PhantomData<Rc<()>>,
}

impl Window {
    pub fn create(title: &str, width: i32, height: i32, flags: &[WindowFlag]) -> std::result::Result<Self, Error> {
        if width <= 0 || height <= 0 {
            return Err(invalid("window dimensions must be positive"));
        }
        let title = input(title, "window title")?;
        let bits = flags.iter().fold(0, |bits, flag| bits | match flag {
            WindowFlag::Hidden => 1,
            WindowFlag::HighPixelDensity => 2,
            WindowFlag::Metal => 4,
        });
        clear_error();
        // SAFETY: the title is a live CString and dimensions/flags are valid for SDL.
        let raw = unsafe { ffi::rays_sdl3_create_window(title.as_ptr(), width, height, bits) };
        let raw = NonNull::new(raw)
            .ok_or_else(|| sdl_error(ErrorKind::Sdl, "SDL window creation failed without an error"))?;
        Ok(Self {
            handle: Rc::new(WindowHandle { raw: Cell::new(Some(raw)) }),
            _main_thread_only: PhantomData,
        })
    }

    fn raw(&self) -> std::result::Result<NonNull<ffi::SDLWindow>, Error> { self.handle.raw() }

    fn clone_handle(&self) -> Self {
        Self { handle: Rc::clone(&self.handle), _main_thread_only: PhantomData }
    }

    pub fn destroy(&mut self) -> std::result::Result<(), Error> {
        if Rc::strong_count(&self.handle) != 1 {
            return Err(Error {
                kind: ErrorKind::ParentHasDependents,
                message: "window still owns native presentation views".to_owned(),
            });
        }
        self.handle.destroy();
        Ok(())
    }

    pub fn size(&self) -> std::result::Result<(i32, i32), Error> {
        let raw = self.raw()?;
        let (mut width, mut height) = (0, 0);
        clear_error();
        // SAFETY: valid output pointers are supplied for the live window.
        if unsafe { ffi::rays_sdl3_window_size(raw.as_ptr(), &mut width, &mut height) } {
            Ok((width, height))
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not read the window size"))
        }
    }

    pub fn size_in_pixels(&self) -> std::result::Result<(i32, i32), Error> {
        let raw = self.raw()?;
        let (mut width, mut height) = (0, 0);
        clear_error();
        // SAFETY: valid output pointers are supplied for the live window.
        if unsafe { ffi::rays_sdl3_window_size_in_pixels(raw.as_ptr(), &mut width, &mut height) } {
            Ok((width, height))
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not read drawable size"))
        }
    }

    pub fn state(&self) -> std::result::Result<WindowState, Error> {
        let raw = self.raw()?;
        // SAFETY: the window handle is live.
        let bits = unsafe { ffi::rays_sdl3_window_state(raw.as_ptr()) };
        Ok(WindowState { hidden: bits & 1 != 0, minimized: bits & 2 != 0, occluded: bits & 4 != 0 })
    }


    pub fn pixel_density(&self) -> std::result::Result<f32, Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        Ok(unsafe { ffi::rays_sdl3_window_pixel_density(raw.as_ptr()) })
    }

    pub fn display_scale(&self) -> std::result::Result<f32, Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        Ok(unsafe { ffi::rays_sdl3_window_display_scale(raw.as_ptr()) })
    }

    pub fn position(&self) -> std::result::Result<(i32, i32), Error> {
        let raw = self.raw()?;
        let (mut x, mut y) = (0, 0);
        clear_error();
        // SAFETY: valid output pointers are supplied for the live window.
        if unsafe { ffi::rays_sdl3_window_position(raw.as_ptr(), &mut x, &mut y) } {
            Ok((x, y))
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not read the window position"))
        }
    }

    pub fn title(&self) -> std::result::Result<String, Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: SDL returns a borrowed, NUL-terminated title for a live window.
        Ok(unsafe { string(ffi::rays_sdl3_window_title(raw.as_ptr())) })
    }

    fn boolean(&self, operation: unsafe extern "C" fn(*mut ffi::SDLWindow) -> bool, fallback: &str) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle remains live for this call.
        if unsafe { operation(raw.as_ptr()) } { Ok(()) } else { Err(sdl_error(ErrorKind::Sdl, fallback)) }
    }

    pub fn center(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_center_window, "SDL could not center the window")
    }

    pub fn show(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_show_window, "SDL could not show the window")
    }

    pub fn raise(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_raise_window, "SDL could not raise the window")
    }

    pub fn hide(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_hide_window, "SDL could not hide the window")
    }

    pub fn restore(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_restore_window, "SDL could not restore the window")
    }

    pub fn sync(&self) -> std::result::Result<(), Error> {
        self.boolean(ffi::rays_sdl3_sync_window, "SDL could not synchronize the window")
    }

    pub fn set_size(&self, width: i32, height: i32) -> std::result::Result<(), Error> {
        if width <= 0 || height <= 0 { return Err(invalid("window dimensions must be positive")); }
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window is live and dimensions are positive.
        if unsafe { ffi::rays_sdl3_set_window_size(raw.as_ptr(), width, height) } {
            Ok(())
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not resize the window"))
        }
    }

    pub fn set_resizable(&self, enabled: bool) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        if unsafe { ffi::rays_sdl3_set_window_resizable(raw.as_ptr(), enabled) } {
            Ok(())
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not change window resizability"))
        }
    }

    pub fn set_relative_mouse(&self, enabled: bool) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        if unsafe { ffi::rays_sdl3_set_window_relative_mouse(raw.as_ptr(), enabled) } {
            Ok(())
        } else {
            Err(sdl_error(ErrorKind::Unsupported, "relative mouse mode is unavailable"))
        }
    }

    pub fn set_background(&self, red: f32, green: f32, blue: f32) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        // SAFETY: the window handle is live; this platform operation is a no-op off Cocoa.
        unsafe { ffi::rays_sdl3_set_window_background(raw.as_ptr(), red.into(), green.into(), blue.into()) };
        Ok(())
    }

    pub fn start_text_input(&self) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        if unsafe { ffi::rays_sdl3_start_text_input(raw.as_ptr()) } { Ok(()) }
        else { Err(sdl_error(ErrorKind::Sdl, "SDL could not start text input")) }
    }

    pub fn stop_text_input(&self) -> std::result::Result<(), Error> {
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window handle is live.
        if unsafe { ffi::rays_sdl3_stop_text_input(raw.as_ptr()) } { Ok(()) }
        else { Err(sdl_error(ErrorKind::Sdl, "SDL could not stop text input")) }
    }

    pub fn set_text_input_area(&self, area: Rect, cursor: i32) -> std::result::Result<(), Error> {
        if cursor < 0 { return Err(invalid("text-input cursor offset must be non-negative")); }
        if area.width <= 0 || area.height <= 0 { return Err(invalid("text-input area dimensions must be positive")); }
        let raw = self.raw()?;
        clear_error();
        // SAFETY: the window is live and the rectangle/cursor have been validated.
        if unsafe { ffi::rays_sdl3_set_text_input_area(raw.as_ptr(), area.x, area.y, area.width, area.height, cursor) } {
            Ok(())
        } else {
            Err(sdl_error(ErrorKind::Sdl, "SDL could not set the text-input area"))
        }
    }

    pub fn show_dialog(&self, kind: DialogKind, filters: &[DialogFilter], default_location: Option<&str>) -> std::result::Result<i32, Error> {
        if filters.len() > 32 { return Err(invalid("too many dialog filters")); }
        if filters.iter().any(|filter| filter.name.is_empty() || filter.pattern.is_empty()) {
            return Err(invalid("a dialog filter needs a name and a pattern"));
        }
        let mut names = Vec::with_capacity(filters.len());
        let mut patterns = Vec::with_capacity(filters.len());
        for filter in filters {
            names.push(input(&filter.name, "dialog filter name")?);
            patterns.push(input(&filter.pattern, "dialog filter pattern")?);
        }
        let names: Vec<_> = names.iter().map(|text| text.as_ptr()).collect();
        let patterns: Vec<_> = patterns.iter().map(|text| text.as_ptr()).collect();
        let location = default_location.map(|text| input(text, "default dialog location")).transpose()?;
        let raw = self.raw()?;
        clear_error();
        // SAFETY: arrays and C strings stay live through the native call; SDL copies them into its slots.
        let id = unsafe {
            ffi::rays_sdl3_show_dialog(raw.as_ptr(), kind.code(), names.as_ptr(), patterns.as_ptr(),
                filters.len() as i32, location.as_ref().map_or(std::ptr::null(), |text| text.as_ptr()))
        };
        if id == 0 { Err(sdl_error(ErrorKind::Sdl, "SDL could not open the file dialog")) }
        else { Ok(id) }
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DialogKind { OpenFile, OpenFiles, SaveFile, OpenFolder }

impl DialogKind {
    fn code(self) -> i32 {
        match self { Self::OpenFile => 0, Self::OpenFiles => 1, Self::SaveFile => 2, Self::OpenFolder => 3 }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DialogFilter {
    pub name: String,
    pub pattern: String,
}

pub struct MetalView {
    raw: Option<NonNull<c_void>>,
    parent: Option<Window>,
    tokens: Vec<LayerToken>,
    _main_thread_only: PhantomData<Rc<()>>,
}

impl MetalView {
    pub fn create(window: &Window) -> std::result::Result<Self, Error> {
        let parent = window.clone_handle();
        let window = parent.raw()?;
        clear_error();
        // SAFETY: the retained parent window remains live through this view's destruction.
        let raw = unsafe { ffi::rays_sdl3_create_metal_view(window.as_ptr()) };
        let raw = NonNull::new(raw)
            .ok_or_else(|| sdl_error(ErrorKind::Sdl, "SDL could not create a native presentation view"))?;
        Ok(Self { raw: Some(raw), parent: Some(parent), tokens: Vec::new(), _main_thread_only: PhantomData })
    }

    pub fn layer_token(&mut self, generation: i64) -> std::result::Result<LayerToken, Error> {
        let view = self.raw.ok_or_else(|| destroyed("Metal view is destroyed"))?;
        let parent = self.parent.as_ref().ok_or_else(|| destroyed("Metal view is destroyed"))?;
        let window = parent.raw()?;
        // SAFETY: both native handles are live; the ID is used only for token identity.
        let owner_id = unsafe { ffi::rays_sdl3_window_id(window.as_ptr()) as i64 };
        // SAFETY: this view is live and the returned native layer remains retained by the view.
        let layer = unsafe { ffi::rays_sdl3_metal_layer(view.as_ptr()) };
        if layer.is_null() { return Err(sdl_error(ErrorKind::Sdl, "SDL could not read the native layer")); }
        let (retain, release): (Option<Retain>, Option<Release>) = if cfg!(target_os = "macos") {
            (Some(ffi::rays_sdl3_retain_layer), Some(ffi::rays_sdl3_release_layer))
        } else {
            (None, None)
        };
        // SAFETY: layer is valid while the view lives; platform retain/release callbacks match the
        // borrowed layer's ownership model and are callable on token invalidation/drop threads.
        let token = unsafe { LayerToken::from_raw(layer, owner_id, generation, retain, release) }
            .map_err(|error| Error { kind: ErrorKind::Sdl, message: error.to_string() })?;
        self.tokens.push(token.clone());
        Ok(token)
    }

    pub fn destroy(&mut self) {
        for token in &self.tokens { token.invalidate(); }
        self.tokens.clear();
        if let Some(raw) = self.raw.take() {
            // SAFETY: this view is uniquely owned and destroyed exactly once.
            unsafe { ffi::rays_sdl3_destroy_metal_view(raw.as_ptr()) }
        }
        drop(self.parent.take());
    }
}

impl Drop for MetalView {
    fn drop(&mut self) { self.destroy(); }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Key {
    Char(char), ArrowUp, ArrowDown, ArrowLeft, ArrowRight, Space, Enter, Escape, Backspace,
    Tab, Shift, Control, Alt, Meta, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
    Home, End, PageUp, PageDown, Insert, Delete, Unknown(i32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyModifier { Shift, Control, Alt, Meta, NumLock, CapsLock, ScrollLock }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MouseButton { Left, Middle, Right, X1, X2 }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WheelDirection { Normal, Flipped }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PinchPhase { Began, Updated, Ended }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollPhase { Began, Changed, Ended, Momentum }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WindowChange {
    Shown, Hidden, Minimized, Restored, Occluded, FocusGained, FocusLost, CloseRequested,
    Resized(i32, i32), PixelSizeChanged(i32, i32),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DropChange { Begin, Position, Complete, File(String) }
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DialogOutcome { Chosen(Vec<String>), Cancelled, Failed(String) }

#[derive(Clone, Debug, PartialEq)]
pub enum Event {
    Quit,
    Window(WindowChange),
    Key { key: Key, modifiers: Vec<KeyModifier>, down: bool, repeat: bool },
    TextInput(String),
    TextEditing { text: String, start: i32, length: i32 },
    MouseMotion { x: f64, y: f64, dx: f64, dy: f64 },
    MouseButton { button: MouseButton, down: bool, x: f64, y: f64 },
    MouseWheel { x: f64, y: f64, direction: WheelDirection, mouse_x: f64, mouse_y: f64, integer_x: i32, integer_y: i32 },
    Pinch { phase: PinchPhase, scale: f64 },
    Drop { change: DropChange, x: f64, y: f64 },
    Dialog { id: i32, outcome: DialogOutcome },
    Scroll { x: f64, y: f64, phase: ScrollPhase, seconds: f64 },
}

#[repr(C)]
struct RawEvent {
    tag: i32,
    a: i32, b: i32, c: i32, d: i32, e: i32, _f: i32, _g: i32, _h: i32,
    integer_x: i32,
    integer_y: i32,
    id: i32,
    x: f64,
    y: f64,
    z: f64,
    w: f64,
    text: *mut c_char,
    text_len: usize,
}

struct EventBuffer(*mut RawEvent);
impl Drop for EventBuffer {
    fn drop(&mut self) {
        // SAFETY: the buffer was filled by rays_sdl3_poll_event and its payload is malloc-owned.
        unsafe { ffi::rays_sdl3_event_free(self.0) }
    }
}

unsafe fn event_text(event: &RawEvent) -> String {
    if event.text.is_null() { String::new() }
    else {
        // SAFETY: the bridge returns a malloc-owned NUL-terminated UTF-8 SDL string.
        unsafe { CStr::from_ptr(event.text) }.to_string_lossy().into_owned()
    }
}

unsafe fn chosen_paths(event: &RawEvent) -> Vec<String> {
    if event.text.is_null() || event.text_len == 0 { return Vec::new(); }
    // SAFETY: text_len is set by the native bridge for this malloc-owned payload.
    let bytes = unsafe { std::slice::from_raw_parts(event.text.cast::<u8>(), event.text_len) };
    bytes.split(|byte| *byte == 0).filter(|path| !path.is_empty())
        .map(|path| String::from_utf8_lossy(path).into_owned()).collect()
}

fn key(raw: i32, value: i32) -> Key {
    if raw == 0 { return char::from_u32(value as u32).map_or(Key::Unknown(value), Key::Char); }
    match raw {
        1 => Key::Unknown(value), 2 => Key::ArrowUp, 3 => Key::ArrowDown, 4 => Key::ArrowLeft,
        5 => Key::ArrowRight, 6 => Key::Space, 7 => Key::Enter, 8 => Key::Escape,
        9 => Key::Backspace, 10 => Key::Tab, 11 => Key::Shift, 12 => Key::Control,
        13 => Key::Alt, 14 => Key::Meta, 15 => Key::F1, 16 => Key::F2, 17 => Key::F3,
        18 => Key::F4, 19 => Key::F5, 20 => Key::F6, 21 => Key::F7, 22 => Key::F8,
        23 => Key::F9, 24 => Key::F10, 25 => Key::F11, 26 => Key::F12, 27 => Key::Home,
        28 => Key::End, 29 => Key::PageUp, 30 => Key::PageDown, 31 => Key::Insert,
        32 => Key::Delete, _ => Key::Unknown(value),
    }
}

fn modifiers(bits: i32) -> Vec<KeyModifier> {
    [(1, KeyModifier::Shift), (2, KeyModifier::Control), (4, KeyModifier::Alt),
     (8, KeyModifier::Meta), (16, KeyModifier::NumLock), (32, KeyModifier::CapsLock),
     (64, KeyModifier::ScrollLock)]
        .into_iter().filter_map(|(mask, modifier)| (bits & mask != 0).then_some(modifier)).collect()
}

unsafe fn decode(event: &RawEvent) -> Option<Event> {
    Some(match event.tag {
        0 => Event::Quit,
        1 => Event::Window(match event.a {
            0 => WindowChange::Shown, 1 => WindowChange::Hidden, 2 => WindowChange::Minimized,
            3 => WindowChange::Restored, 4 => WindowChange::Occluded, 5 => WindowChange::FocusGained,
            6 => WindowChange::FocusLost, 7 => WindowChange::CloseRequested,
            8 => WindowChange::Resized(event.b, event.c),
            9 => WindowChange::PixelSizeChanged(event.b, event.c), _ => return None,
        }),
        2 => Event::Key { key: key(event.a, event.b), modifiers: modifiers(event.c), down: event.d != 0, repeat: event.e != 0 },
        3 => Event::TextInput(unsafe { event_text(event) }),
        4 => Event::TextEditing { text: unsafe { event_text(event) }, start: event.a, length: event.b },
        5 => Event::MouseMotion { x: event.x, y: event.y, dx: event.z, dy: event.w },
        6 => Event::MouseButton {
            button: match event.a { 0 => MouseButton::Left, 1 => MouseButton::Middle,
                2 => MouseButton::Right, 3 => MouseButton::X1, 4 => MouseButton::X2, _ => return None },
            down: event.b != 0, x: event.x, y: event.y,
        },
        7 => Event::MouseWheel { x: event.x, y: event.y,
            direction: if event.a == 1 { WheelDirection::Flipped } else { WheelDirection::Normal },
            mouse_x: event.z, mouse_y: event.w, integer_x: event.integer_x, integer_y: event.integer_y },
        8 => Event::Pinch { phase: match event.a { 0 => PinchPhase::Began, 1 => PinchPhase::Updated,
            2 => PinchPhase::Ended, _ => return None }, scale: event.x },
        9 => Event::Drop { change: match event.a { 0 => DropChange::Begin, 1 => DropChange::Position,
            2 => DropChange::Complete, 3 => DropChange::File(unsafe { event_text(event) }), _ => return None },
            x: event.x, y: event.y },
        10 => Event::Dialog { id: event.id, outcome: match event.a {
            0 => DialogOutcome::Chosen(unsafe { chosen_paths(event) }), 1 => DialogOutcome::Cancelled,
            2 => DialogOutcome::Failed(unsafe { event_text(event) }), _ => return None,
        } },
        11 => Event::Scroll { x: event.x, y: event.y,
            phase: match event.a { 0 => ScrollPhase::Began, 1 => ScrollPhase::Changed,
                2 => ScrollPhase::Ended, 3 => ScrollPhase::Momentum, _ => return None }, seconds: event.z },
        _ => return None,
    })
}

pub fn set_live_resize_callback(callback: Option<extern "C" fn()>) {
    // SAFETY: SDL invokes this callback only while polling events on the main thread.
    unsafe { ffi::rays_sdl3_set_live_resize_callback(callback) }
}
pub fn poll_event() -> Option<Event> {
    // SAFETY: every zeroed field is valid in the C-compatible event buffer; SDL fills it on success.
    let mut raw: RawEvent = unsafe { std::mem::zeroed() };
    // SAFETY: a writable, correctly laid out event buffer is supplied.
    if !unsafe { ffi::rays_sdl3_poll_event(&mut raw) } { return None; }
    let guard = EventBuffer(&mut raw);
    // SAFETY: string pointers and payload lengths are owned and initialized by the bridge.
    let event = unsafe { decode(&raw) };
    drop(guard);
    event
}


pub fn wait_event_timeout(timeout_ms: i32) -> bool {
    // SAFETY: negative timeouts are normalized before SDL is called.
    unsafe { ffi::rays_sdl3_wait_event_timeout(timeout_ms.max(0)) }
}

/// Raw bridges to the SDL3_image and SDL3_mixer satellites (built and linked with SDL3).
#[cfg(feature = "image")]
pub mod image;
#[cfg(feature = "mixer")]
pub mod mixer;

#[cfg(feature = "test-util")]
pub mod test_util;

mod ffi {
    use super::{c_char, c_void};
    use std::ffi::c_int;

    #[repr(C)]
    pub struct SDLWindow { _private: [u8; 0] }
    #[repr(C)]
    pub struct SDLCursor { _private: [u8; 0] }

    unsafe extern "C" {
        pub fn rays_sdl3_linked_version() -> i32;
        pub fn rays_sdl3_compiled_version() -> i32;
        pub fn rays_sdl3_get_error() -> *const c_char;
        pub fn rays_sdl3_clear_error();
        pub fn rays_sdl3_platform_name() -> *const c_char;
        pub fn rays_sdl3_pref_path(org: *const c_char, app: *const c_char) -> *mut c_char;
        pub fn rays_sdl3_free(value: *mut c_void);
        pub fn rays_sdl3_is_main_thread() -> bool;
        pub fn rays_sdl3_configure_fatal_errors();
        pub fn rays_sdl3_init_subsystem(bits: u32) -> bool;
        pub fn rays_sdl3_quit_subsystem(bits: u32);
        pub fn rays_sdl3_set_control_click_right_click(enabled: bool) -> bool;
        pub fn rays_sdl3_set_mouse_relative_system_scale(enabled: bool) -> bool;
        pub fn rays_sdl3_set_hint(name: *const c_char, value: *const c_char) -> bool;
        pub fn rays_sdl3_clipboard_set_text(text: *const c_char) -> bool;
        pub fn rays_sdl3_clipboard_get_text() -> *mut c_char;
        pub fn rays_sdl3_create_cursor(shape: c_int) -> *mut SDLCursor;
        pub fn rays_sdl3_set_cursor(cursor: *mut SDLCursor) -> bool;
        pub fn rays_sdl3_destroy_cursor(cursor: *mut SDLCursor);
        pub fn rays_sdl3_create_window(title: *const c_char, width: c_int, height: c_int, flags: u32) -> *mut SDLWindow;
        pub fn rays_sdl3_destroy_window(window: *mut SDLWindow);
        pub fn rays_sdl3_window_size(window: *mut SDLWindow, width: *mut c_int, height: *mut c_int) -> bool;
        pub fn rays_sdl3_window_size_in_pixels(window: *mut SDLWindow, width: *mut c_int, height: *mut c_int) -> bool;
        pub fn rays_sdl3_window_state(window: *mut SDLWindow) -> u32;
        pub fn rays_sdl3_window_id(window: *mut SDLWindow) -> u32;
        pub fn rays_sdl3_window_pixel_density(window: *mut SDLWindow) -> f32;
        pub fn rays_sdl3_window_display_scale(window: *mut SDLWindow) -> f32;
        pub fn rays_sdl3_window_position(window: *mut SDLWindow, x: *mut c_int, y: *mut c_int) -> bool;
        pub fn rays_sdl3_window_title(window: *mut SDLWindow) -> *const c_char;
        pub fn rays_sdl3_center_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_set_window_size(window: *mut SDLWindow, width: c_int, height: c_int) -> bool;
        pub fn rays_sdl3_set_window_resizable(window: *mut SDLWindow, enabled: bool) -> bool;
        pub fn rays_sdl3_set_window_relative_mouse(window: *mut SDLWindow, enabled: bool) -> bool;
        pub fn rays_sdl3_show_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_raise_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_hide_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_restore_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_sync_window(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_set_window_background(window: *mut SDLWindow, red: f64, green: f64, blue: f64);
        pub fn rays_sdl3_start_text_input(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_stop_text_input(window: *mut SDLWindow) -> bool;
        pub fn rays_sdl3_set_text_input_area(window: *mut SDLWindow, x: c_int, y: c_int, width: c_int, height: c_int, cursor: c_int) -> bool;
        pub fn rays_sdl3_show_dialog(window: *mut SDLWindow, kind: c_int, names: *const *const c_char,
            patterns: *const *const c_char, count: c_int, default_location: *const c_char) -> c_int;
        pub fn rays_sdl3_create_metal_view(window: *mut SDLWindow) -> *mut c_void;
        pub fn rays_sdl3_destroy_metal_view(view: *mut c_void);
        pub fn rays_sdl3_metal_layer(view: *mut c_void) -> *mut c_void;
        pub fn rays_sdl3_retain_layer(layer: *mut c_void);
        pub fn rays_sdl3_release_layer(layer: *mut c_void);
        pub fn rays_sdl3_poll_event(event: *mut super::RawEvent) -> bool;
        pub fn rays_sdl3_event_free(event: *mut super::RawEvent);
        pub fn rays_sdl3_wait_event_timeout(timeout_ms: c_int) -> bool;
        pub fn rays_sdl3_set_live_resize_callback(callback: Option<extern "C" fn()>);
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_polling_event_queue() -> bool;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_queue_resize_during_poll();
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_push_event_trace() -> bool;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_mutate_event_sources();
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_push_resize_burst() -> bool;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_dialog_reserve() -> c_int;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_dialog_abandon(id: c_int);
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_dialog_finish(id: c_int, paths: *const *const c_char,
            count: c_int, failed: bool) -> bool;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_dialog_finish_huge(id: c_int) -> bool;
        #[cfg(feature = "test-util")]
        pub fn rays_sdl3_test_post_scroll(points: c_int, phase: c_int);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_number_matches_sdl_encoding() {
        assert_eq!(version(3_004_018), Version { major: 3, minor: 4, patch: 18 });
    }

    #[test]
    fn key_unknown_and_modifier_wire_values_are_typed() {
        assert_eq!(key(0, b'q' as i32), Key::Char('q'));
        assert_eq!(key(1, 0x444), Key::Unknown(0x444));
        assert_eq!(modifiers(1 | 8 | 32), vec![KeyModifier::Shift, KeyModifier::Meta, KeyModifier::CapsLock]);
    }

    #[test]
    fn event_wire_record_decodes_without_exposing_native_tags() {
        let raw = RawEvent {
            tag: 2, a: 0, b: b'a' as i32, c: 1 | 8, d: 1, e: 0, _f: 0, _g: 0, _h: 0,
            integer_x: 0, integer_y: 0, id: 0, x: 0.0, y: 0.0, z: 0.0, w: 0.0,
            text: std::ptr::null_mut(), text_len: 0,
        };
        // SAFETY: this synthetic record contains no borrowed string pointers.
        assert_eq!(unsafe { decode(&raw) }, Some(Event::Key {
            key: Key::Char('a'), modifiers: vec![KeyModifier::Shift, KeyModifier::Meta],
            down: true, repeat: false,
        }));
    }
}
