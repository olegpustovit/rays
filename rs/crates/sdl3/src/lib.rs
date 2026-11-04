use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::fmt;

use native_layer_token::Token as NativeLayerToken;
use sdl3_sys as sys;

pub use sdl3_sys::{
    DialogFilter, DialogKind, DialogOutcome, DropChange, Event, Key, KeyModifier, MouseButton,
    PinchPhase, Rect, ScrollPhase, Version, WheelDirection, WindowChange, WindowState,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    SdlError,
    WrongDomain,
    Destroyed,
    ParentHasDependents,
    IncompatibleVersion,
    InvalidArgument,
    Unsupported,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    operation: &'static str,
    kind: ErrorKind,
    message: String,
}

impl Error {
    pub fn operation(&self) -> &'static str { self.operation }
    pub fn kind(&self) -> ErrorKind { self.kind }
    pub fn message(&self) -> &str { &self.message }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.operation, self.message)
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rgba {
    pub width: i32,
    pub height: i32,
    pub pixels: Vec<u8>,
}

fn error(operation: &'static str, kind: ErrorKind, message: impl Into<String>) -> Error {
    Error { operation, kind, message: message.into() }
}

fn native_error(operation: &'static str, failure: sys::Error) -> Error {
    let kind = match failure.kind {
        sys::ErrorKind::Sdl => ErrorKind::SdlError,
        sys::ErrorKind::Unsupported => ErrorKind::Unsupported,
        sys::ErrorKind::InvalidArgument => ErrorKind::InvalidArgument,
        sys::ErrorKind::Destroyed => ErrorKind::Destroyed,
        sys::ErrorKind::ParentHasDependents => ErrorKind::ParentHasDependents,
    };
    error(operation, kind, failure.message)
}

fn before_main(operation: &'static str) -> Result<()> {
    if !thread::is_initial_domain() {
        Err(error(operation, ErrorKind::WrongDomain, "SDL operation must run on the initial thread"))
    } else if !thread::is_sdl_main_thread() {
        Err(error(operation, ErrorKind::WrongDomain, "SDL operation must run on the platform main thread"))
    } else {
        Ok(())
    }
}

fn on_main<T>(operation: &'static str, call: impl FnOnce() -> Result<T>) -> Result<T> {
    before_main(operation)?;
    call()
}

pub fn compiled_version() -> Version {
    sys::compiled_version()
}

pub fn linked_version() -> Version {
    sys::linked_version()
}

pub fn platform_name() -> String {
    sys::platform_name()
}

pub fn pref_path(org: &str, app: &str) -> Result<Option<String>> {
    sys::pref_path(org, app)
        .map(|path| path.filter(|path| !path.is_empty()))
        .map_err(|failure| native_error("SDL3.pref_path", failure))
}

pub fn validate_version_for(
    library: &str,
    compiled: Version,
    release: bool,
    linked: Version,
) -> Result<()> {
    let compatible_minor = linked.major == compiled.major && linked.minor == compiled.minor;
    let stable = |value: Version| value.minor % 2 == 0 && value.patch % 2 == 0;
    let version_string = |value: Version| format!("{}.{}.{}", value.major, value.minor, value.patch);
    if release && !stable(compiled) {
        Err(error(
            "SDL3.validate_version",
            ErrorKind::IncompatibleVersion,
            format!("compiled against prerelease {library} headers {}", version_string(compiled)),
        ))
    } else if !compatible_minor || linked.patch < compiled.patch {
        Err(error(
            "SDL3.validate_version",
            ErrorKind::IncompatibleVersion,
            format!("linked {library} {} does not match the compiled ABI floor {}", version_string(linked), version_string(compiled)),
        ))
    } else if release && !stable(linked) {
        Err(error(
            "SDL3.validate_version",
            ErrorKind::IncompatibleVersion,
            format!("linked {library} is a development release: {}", version_string(linked)),
        ))
    } else {
        Ok(())
    }
}

pub fn validate_version(release: bool, linked: Version) -> Result<()> {
    validate_version_for("SDL", compiled_version(), release, linked)
}

pub fn check_version(release: bool) -> Result<()> {
    validate_version(release, linked_version())
}

pub mod thread {
    pub fn is_initial_domain() -> bool {
        super::main_thread()
    }

    pub fn is_sdl_main_thread() -> bool {
        super::main_thread()
    }
}

// The test harness never runs a test on the process main thread, so a test claims that role.
#[cfg(not(test))]
fn main_thread() -> bool {
    sys::is_main_thread()
}

#[cfg(test)]
fn main_thread() -> bool {
    tests::is_main()
}

pub mod init {
    use super::{Result, before_main, check_version, native_error, sys};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Subsystem { Video, Events }

    fn bits(subsystems: &[Subsystem]) -> u32 {
        subsystems.iter().fold(0, |mask, subsystem| mask | match subsystem {
            Subsystem::Video => 1,
            Subsystem::Events => 2,
        })
    }

    pub fn init(subsystems: &[Subsystem]) -> Result<()> {
        const OPERATION: &str = "SDL3.Init.init";
        before_main(OPERATION)?;
        check_version(true)?;
        sys::clear_error();
        sys::init_subsystems(bits(subsystems)).map_err(|failure| native_error(OPERATION, failure))
    }

    pub fn quit_subsystems(subsystems: &[Subsystem]) -> Result<()> {
        const OPERATION: &str = "SDL3.Init.quit_subsystems";
        before_main(OPERATION)?;
        sys::quit_subsystems(bits(subsystems));
        Ok(())
    }

}

pub mod hint {
    use super::{Result, before_main, native_error, sys};

    pub fn control_click_is_right_click(enabled: bool) -> Result<()> {
        const OPERATION: &str = "SDL3.Hint.control_click_is_right_click";
        before_main(OPERATION)?;
        sys::clear_error();
        sys::set_control_click_right_click(enabled).map_err(|failure| native_error(OPERATION, failure))
    }

    pub fn mouse_relative_system_scale(enabled: bool) -> Result<()> {
        const OPERATION: &str = "SDL3.Hint.mouse_relative_system_scale";
        before_main(OPERATION)?;
        sys::clear_error();
        sys::set_mouse_relative_system_scale(enabled).map_err(|failure| native_error(OPERATION, failure))
    }
}

pub mod window {
    use super::{ErrorKind, Rc, RefCell, Result, before_main, error, native_error, on_main, sys};
    use super::{DialogFilter, DialogKind};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Flag { Hidden, HighPixelDensity, Metal, NativeView }

    pub(super) struct State {
        pub(super) native: RefCell<Option<sys::Window>>,
    }

    pub struct Window {
        state: Rc<State>,
    }

    impl Window {
        pub fn create(title: &str, width: i32, height: i32, flags: &[Flag]) -> Result<Self> {
            const OPERATION: &str = "SDL3.Window.create";
            if title.contains('\0') {
                return Err(error(OPERATION, ErrorKind::InvalidArgument, "window title contains a NUL byte"));
            }
            if width <= 0 || height <= 0 {
                return Err(error(OPERATION, ErrorKind::InvalidArgument, "window dimensions must be positive"));
            }
            on_main(OPERATION, || {
                sys::clear_error();
                let flags = flags.iter().map(|flag| match flag {
                    Flag::Hidden => sys::WindowFlag::Hidden,
                    Flag::HighPixelDensity => sys::WindowFlag::HighPixelDensity,
                    Flag::Metal | Flag::NativeView => sys::WindowFlag::Metal,
                });
                let native = sys::Window::create(title, width, height, flags)
                    .map_err(|failure| native_error(OPERATION, failure))?;
                Ok(Self { state: Rc::new(State { native: RefCell::new(Some(native)) }) })
            })
        }

        fn live<T>(&self, operation: &'static str, call: impl FnOnce(&sys::Window) -> sys::Result<T>) -> Result<T> {
            before_main(operation)?;
            let native = self.state.native.borrow();
            let native = native.as_ref().ok_or_else(|| error(operation, ErrorKind::Destroyed, "window is destroyed"))?;
            call(native).map_err(|failure| native_error(operation, failure))
        }

        pub fn size(&self) -> Result<(i32, i32)> { self.live("SDL3.Window.size", sys::Window::size) }
        pub fn size_in_pixels(&self) -> Result<(i32, i32)> { self.live("SDL3.Window.size_in_pixels", sys::Window::size_in_pixels) }

        pub fn pixel_density(&self) -> Result<f32> {
            let scale = self.live("SDL3.Window.pixel_density", sys::Window::pixel_density)?;
            if scale.is_finite() && scale > 0.0 { Ok(scale) }
            else { Err(error("SDL3.Window.pixel_density", ErrorKind::SdlError, "SDL returned an invalid pixel density")) }
        }

        pub fn display_scale(&self) -> Result<f32> {
            let scale = self.live("SDL3.Window.display_scale", sys::Window::display_scale)?;
            if scale.is_finite() && scale > 0.0 { Ok(scale) }
            else { Err(error("SDL3.Window.display_scale", ErrorKind::SdlError, "SDL returned an invalid display scale")) }
        }

        pub fn position(&self) -> Result<(i32, i32)> { self.live("SDL3.Window.position", sys::Window::position) }
        pub fn title(&self) -> Result<String> { self.live("SDL3.Window.title", sys::Window::title) }
        pub fn center(&self) -> Result<()> { self.live("SDL3.Window.center", sys::Window::center) }
        pub fn set_size(&self, width: i32, height: i32) -> Result<()> {
            if width <= 0 || height <= 0 {
                return Err(error("SDL3.Window.set_size", ErrorKind::InvalidArgument,
                    "window dimensions must be positive"));
            }
            self.live("SDL3.Window.set_size", |window| window.set_size(width, height))
        }
        pub fn set_resizable(&self, enabled: bool) -> Result<()> {
            self.live("SDL3.Window.set_resizable", |window| window.set_resizable(enabled))
        }
        pub fn set_background(&self, red: f32, green: f32, blue: f32) -> Result<()> {
            self.live("SDL3.Window.set_background", |window| window.set_background(red, green, blue))
        }
        pub fn set_relative_mouse(&self, enabled: bool) -> Result<()> {
            self.live("SDL3.Window.set_relative_mouse", |window| window.set_relative_mouse(enabled))
        }
        pub fn show(&self) -> Result<()> { self.live("SDL3.Window.show", sys::Window::show) }
        pub fn raise_window(&self) -> Result<()> { self.live("SDL3.Window.raise", sys::Window::raise) }
        pub fn hide(&self) -> Result<()> { self.live("SDL3.Window.hide", sys::Window::hide) }
        pub fn restore(&self) -> Result<()> { self.live("SDL3.Window.restore", sys::Window::restore) }
        pub fn state(&self) -> Result<super::WindowState> { self.live("SDL3.Window.state", sys::Window::state) }
        pub fn sync(&self) -> Result<()> { self.live("SDL3.Window.sync", sys::Window::sync) }
        pub fn start_text_input(&self) -> Result<()> {
            self.live("SDL3.Text_input.start", sys::Window::start_text_input)
        }
        pub fn stop_text_input(&self) -> Result<()> {
            self.live("SDL3.Text_input.stop", sys::Window::stop_text_input)
        }
        pub fn set_text_input_area(&self, area: super::Rect, cursor: i32) -> Result<()> {
            if cursor < 0 {
                return Err(error("SDL3.Text_input.set_area", ErrorKind::InvalidArgument,
                    "text-input cursor offset must be non-negative"));
            }
            if area.width <= 0 || area.height <= 0 {
                return Err(error("SDL3.Text_input.set_area", ErrorKind::InvalidArgument,
                    "text-input area dimensions must be positive"));
            }
            self.live("SDL3.Text_input.set_area", |window| window.set_text_input_area(area, cursor))
        }

        pub fn destroy(&mut self) -> Result<()> {
            const OPERATION: &str = "SDL3.Window.destroy";
            on_main(OPERATION, || {
                let mut native = self.state.native.borrow_mut();
                let Some(window) = native.as_mut() else { return Ok(()); };
                window.destroy().map_err(|failure| native_error(OPERATION, failure))?;
                drop(native.take());
                Ok(())
            })
        }

        fn with_native<T>(&self, operation: &'static str, call: impl FnOnce(&sys::Window) -> sys::Result<T>) -> Result<T> {
            before_main(operation)?;
            let native = self.state.native.borrow();
            let native = native.as_ref().ok_or_else(|| error(operation, ErrorKind::Destroyed, "window is destroyed"))?;
            call(native).map_err(|failure| native_error(operation, failure))
        }

        pub fn dialog_show(&self, kind: DialogKind, filters: &[DialogFilter], default_location: Option<&str>) -> Result<i32> {
            const OPERATION: &str = "SDL3.Dialog.show";
            if filters.iter().any(|filter| filter.name.contains('\0') || filter.pattern.contains('\0'))
                || default_location.is_some_and(|location| location.contains('\0')) {
                return Err(error(OPERATION, ErrorKind::InvalidArgument, "a dialog filter or location contains a NUL byte"));
            }
            if filters.len() > 32 {
                return Err(error(OPERATION, ErrorKind::InvalidArgument, "too many dialog filters"));
            }
            if filters.iter().any(|filter| filter.name.is_empty() || filter.pattern.is_empty()) {
                return Err(error(OPERATION, ErrorKind::InvalidArgument, "a dialog filter needs a name and a pattern"));
            }
            self.with_native(OPERATION, |window| window.show_dialog(kind, filters, default_location))
        }

        pub(super) fn ownership_state(&self) -> Rc<State> { Rc::clone(&self.state) }
    }
}

pub use window::{Flag as WindowFlag, Window};

pub mod cursor {
    use super::{ErrorKind, Result, before_main, error, native_error, sys};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Shape { Default, Text, EwResize, NsResize }

    pub struct Cursor { native: Option<sys::Cursor> }

    impl Cursor {
        pub fn create(shape: Shape) -> Result<Self> {
            const OPERATION: &str = "SDL3.Cursor.create";
            before_main(OPERATION)?;
            let shape = match shape {
                Shape::Default => sys::CursorShape::Default,
                Shape::Text => sys::CursorShape::Text,
                Shape::EwResize => sys::CursorShape::EwResize,
                Shape::NsResize => sys::CursorShape::NsResize,
            };
            sys::Cursor::create(shape).map(|native| Self { native: Some(native) })
                .map_err(|failure| native_error(OPERATION, failure))
        }

        pub fn set(&self) -> Result<()> {
            const OPERATION: &str = "SDL3.Cursor.set";
            before_main(OPERATION)?;
            self.native.as_ref().ok_or_else(|| error(OPERATION, ErrorKind::Destroyed, "cursor is destroyed"))?
                .set().map_err(|failure| native_error(OPERATION, failure))
        }

        pub fn destroy(&mut self) -> Result<()> {
            before_main("SDL3.Cursor.destroy")?;
            drop(self.native.take());
            Ok(())
        }
    }
}

pub use cursor::{Cursor, Shape as CursorShape};

pub mod metal_view {
    use super::{AtomicU64, ErrorKind, NativeLayerToken, Ordering, Rc, Result, before_main, error, native_error, sys};
    use super::window::Window;

    static NEXT_GENERATION: AtomicU64 = AtomicU64::new(1);

    pub struct MetalView {
        native: Option<sys::MetalView>,
        parent: Rc<super::window::State>,
        generation: i64,
        layer: Option<NativeLayerToken>,
    }

    impl MetalView {
        pub fn create(window: &Window) -> Result<Self> {
            const OPERATION: &str = "SDL3.Metal_view.create";
            before_main(OPERATION)?;
            let state = window.ownership_state();
            let native = {
                let parent = state.native.borrow();
                let parent = parent.as_ref().ok_or_else(|| error(OPERATION, ErrorKind::Destroyed, "parent window is destroyed"))?;
                sys::MetalView::create(parent).map_err(|failure| native_error(OPERATION, failure))?
            };
            Ok(Self {
                native: Some(native), parent: state,
                generation: NEXT_GENERATION.fetch_add(1, Ordering::Relaxed) as i64,
                layer: None,
            })
        }

        pub fn layer(&mut self) -> Result<NativeLayerToken> {
            const OPERATION: &str = "SDL3.Metal_view.layer";
            before_main(OPERATION)?;
            if self.native.is_none() { return Err(error(OPERATION, ErrorKind::Destroyed, "Metal view is destroyed")); }
            if self.parent.native.borrow().is_none() {
                return Err(error(OPERATION, ErrorKind::Destroyed, "parent window is destroyed"));
            }
            if let Some(token) = &self.layer {
                if token.alive() { return Ok(token.clone()); }
            }
            let native = self.native.as_mut().ok_or_else(|| error(OPERATION, ErrorKind::Destroyed, "Metal view is destroyed"))?;
            let token = native.layer_token(self.generation).map_err(|failure| native_error(OPERATION, failure))?;
            let token = NativeLayerToken::from_sys(token);
            self.layer = Some(token.clone());
            Ok(token)
        }

        pub fn destroy(&mut self) -> Result<()> {
            const OPERATION: &str = "SDL3.Metal_view.destroy";
            before_main(OPERATION)?;
            self.destroy_native();
            Ok(())
        }

        fn destroy_native(&mut self) {
            if self.native.is_none() { return; }
            if let Some(token) = self.layer.take() { token.invalidate(); }
            drop(self.native.take());
        }
    }

    impl Drop for MetalView {
        fn drop(&mut self) { self.destroy_native(); }
    }
}

pub use metal_view::MetalView;
pub type NativeView = MetalView;

pub mod clipboard {
    use super::{ErrorKind, Result, before_main, error, native_error, sys};

    pub fn set_text(text: &str) -> Result<()> {
        const OPERATION: &str = "SDL3.Clipboard.set_text";
        if text.contains('\0') {
            return Err(error(OPERATION, ErrorKind::InvalidArgument, "clipboard text contains a NUL byte"));
        }
        before_main(OPERATION)?;
        sys::clear_error();
        sys::clipboard_set_text(text).map_err(|failure| native_error(OPERATION, failure))
    }

    pub fn get_text() -> Result<String> {
        const OPERATION: &str = "SDL3.Clipboard.get_text";
        before_main(OPERATION)?;
        sys::clear_error();
        sys::clipboard_get_text().map_err(|failure| native_error(OPERATION, failure))
    }
}

pub mod text_input {
    use super::{Rect, Result, Window};

    pub fn start(window: &Window) -> Result<()> { window.start_text_input() }
    pub fn stop(window: &Window) -> Result<()> { window.stop_text_input() }
    pub fn set_area(window: &Window, area: Rect, cursor: i32) -> Result<()> {
        window.set_text_input_area(area, cursor)
    }
}

pub mod dialog {
    use super::{DialogFilter, DialogKind, Result, Window};

    pub fn show(window: &Window, kind: DialogKind, filters: &[DialogFilter], default_location: Option<&str>) -> Result<i32> {
        window.dialog_show(kind, filters, default_location)
    }
}

pub mod event {
    use super::{Cell, Event, Rc, RefCell, Result, before_main, sys};

    thread_local! {
        static LIVE_RESIZE_CALLBACK: RefCell<Option<Rc<RefCell<Box<dyn FnMut()>>>>> = const { RefCell::new(None) };
        static LIVE_RESIZE_FAILURE: RefCell<Option<Box<dyn std::any::Any + Send>>> = const { RefCell::new(None) };
        static IN_LIVE_RESIZE_CALLBACK: Cell<bool> = const { Cell::new(false) };
    }

    struct LiveResizeGuard;

    impl Drop for LiveResizeGuard {
        fn drop(&mut self) { IN_LIVE_RESIZE_CALLBACK.with(|active| active.set(false)); }
    }

    fn notify_live_resize() {
        if IN_LIVE_RESIZE_CALLBACK.with(|active| active.replace(true)) { return; }
        let _guard = LiveResizeGuard;
        let callback = LIVE_RESIZE_CALLBACK.with(|handler| handler.borrow().clone());
        if let Some(callback) = callback {
            if let Ok(mut callback) = callback.try_borrow_mut() { callback(); }
        }
    }

    extern "C" fn live_resize_trampoline() {
        if let Err(payload) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(notify_live_resize)) {
            LIVE_RESIZE_FAILURE.with(|failure| {
                let mut failure = failure.borrow_mut();
                if failure.is_none() { *failure = Some(payload); }
            });
        }
    }

    fn resume_live_resize_failure() {
        let payload = LIVE_RESIZE_FAILURE.with(|failure| failure.borrow_mut().take());
        if let Some(payload) = payload { std::panic::resume_unwind(payload); }
    }

    pub fn set_live_resize_callback(callback: Option<Box<dyn FnMut()>>) -> Result<()> {
        const OPERATION: &str = "SDL3.Event.set_live_resize_callback";
        before_main(OPERATION)?;
        let installed = callback.is_some();
        LIVE_RESIZE_CALLBACK.with(|current| {
            *current.borrow_mut() = callback.map(|callback| Rc::new(RefCell::new(callback)));
        });
        LIVE_RESIZE_FAILURE.with(|failure| drop(failure.borrow_mut().take()));
        let native_callback: Option<extern "C" fn()> = if installed { Some(live_resize_trampoline) } else { None };
        sys::set_live_resize_callback(native_callback);
        Ok(())
    }
    fn is_size(event: &Event) -> bool {
        matches!(event, Event::Window(super::WindowChange::Resized(..) | super::WindowChange::PixelSizeChanged(..)))
    }

    fn same_size_kind(left: &Event, right: &Event) -> bool {
        matches!((left, right),
            (Event::Window(super::WindowChange::Resized(..)), Event::Window(super::WindowChange::Resized(..)))
            | (Event::Window(super::WindowChange::PixelSizeChanged(..)), Event::Window(super::WindowChange::PixelSizeChanged(..))))
    }

    fn flush(output: &mut Vec<Event>, motion: &mut Option<Event>, sizes: &mut Vec<Event>) {
        if let Some(event) = motion.take() { output.push(event); }
        output.append(sizes);
    }

    fn coalesce(events: impl IntoIterator<Item = Event>) -> Vec<Event> {
        let mut output = Vec::new();
        let mut motion: Option<Event> = None;
        let mut sizes = Vec::new();
        for event in events {
            match event {
                Event::MouseMotion { x, y, dx, dy } => {
                    motion = Some(match motion.take() {
                        Some(Event::MouseMotion { dx: old_dx, dy: old_dy, .. }) => Event::MouseMotion { x, y, dx: dx + old_dx, dy: dy + old_dy },
                        _ => Event::MouseMotion { x, y, dx, dy },
                    });
                }
                event if is_size(&event) => {
                    if let Some(old) = sizes.iter_mut().find(|old| same_size_kind(old, &event)) { *old = event; }
                    else { sizes.push(event); }
                }
                event => {
                    flush(&mut output, &mut motion, &mut sizes);
                    output.push(event);
                }
            }
        }
        flush(&mut output, &mut motion, &mut sizes);
        output
    }

    pub fn poll_coalesced() -> Result<Vec<Event>> {
        const OPERATION: &str = "SDL3.Event.poll_coalesced";
        before_main(OPERATION)?;
        let events = std::iter::from_fn(|| {
            let event = sys::poll_event();
            resume_live_resize_failure();
            event
        });
        Ok(coalesce(events))
    }

    pub fn wait(timeout_ms: i32) -> Result<bool> {
        const OPERATION: &str = "SDL3.Event.wait";
        before_main(OPERATION)?;
        Ok(sys::wait_event_timeout(timeout_ms.max(0)))
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use super::super::WindowChange;

        #[test]
        fn motion_and_size_events_coalesce_without_reordering_other_events() {
            let events = [
                Event::MouseMotion { x: 1.0, y: 2.0, dx: 3.0, dy: 4.0 },
                Event::Window(WindowChange::Resized(10, 20)),
                Event::MouseMotion { x: 5.0, y: 6.0, dx: 7.0, dy: 8.0 },
                Event::Window(WindowChange::PixelSizeChanged(30, 40)),
                Event::Window(WindowChange::Resized(50, 60)),
                Event::Quit,
            ];
            assert_eq!(coalesce(events), vec![
                Event::MouseMotion { x: 5.0, y: 6.0, dx: 10.0, dy: 12.0 },
                Event::Window(WindowChange::Resized(50, 60)),
                Event::Window(WindowChange::PixelSizeChanged(30, 40)),
                Event::Quit,
            ]);
        }

        #[test]
        fn stable_event_flushes_pending_samples_before_it() {
            let events = [
                Event::MouseMotion { x: 1.0, y: 2.0, dx: 1.0, dy: 1.0 },
                Event::Quit,
                Event::MouseMotion { x: 3.0, y: 4.0, dx: 2.0, dy: 3.0 },
            ];
            assert_eq!(coalesce(events), vec![
                Event::MouseMotion { x: 1.0, y: 2.0, dx: 1.0, dy: 1.0 },
                Event::Quit,
                Event::MouseMotion { x: 3.0, y: 4.0, dx: 2.0, dy: 3.0 },
            ]);
        }
    }
}

#[cfg(test)]
mod tests;
