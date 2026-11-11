//! Native sketch lifecycle: update a model in place or return a replacement, then render its view.
//!
//! The model stays in the loop's storage while callbacks run, including during panic unwinding.
//! A callback that returns a replacement leaves the previous model intact until it succeeds;
//! mutations made in place are available to `crash_dump` if the callback panics. Callback panics are fatal: a crash report folder is written before the panic
//! propagates (the report is written by a drop guard from the panic hook's capture; nothing here
//! catches the unwind).
use crate::canvas_runtime;
use crate::frame;
use crate::input_state;
use crate::native_scene_lowering;
use crate::scene::{self, layer};
use crate::time;
use crate::world;
use input::{Event, Key, MouseButton};
use ogpu_core::error::{self as oerror, ErrorKind};
use renderer::renderer::{self as rr, native};
use rays_math::color;
use std::any::Any;
use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Clock {
    Realtime,
    Fixed(f64),
}

pub type Error = oerror::Error;

pub fn error_to_string(e: &Error) -> String {
    oerror::to_string(e)
}

/// `default_config` enables native resizing.
#[derive(Clone, Debug, PartialEq)]
pub struct Config {
    pub width: i64,
    pub height: i64,
    pub title: String,
    pub fps: Option<i64>,
    pub domains: Option<i64>,
    pub clock: Clock,
}

pub fn default_config() -> Config {
    Config { width: 800, height: 600, title: "Rays sketch".to_string(), fps: Some(60), domains: None, clock: Clock::Realtime }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Dialog {
    OpenFile,
    OpenFiles,
    SaveFile,
    OpenFolder,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cursor {
    Default,
    HorizontalResize,
    VerticalResize,
    Text,
}

pub type Model = Box<dyn Any>;

/// The sketch's callbacks. Updates borrow the stored model mutably; readers borrow it immutably.
pub struct Program {
    pub init: Box<dyn Fn(&frame::Frame) -> Model>,
    /// `None` keeps the model, including any mutations made by the callback.
    pub update: Box<dyn Fn(&mut dyn Any, &frame::Frame) -> Option<Model>>,
    pub view: Box<dyn Fn(&dyn Any, &frame::Frame) -> scene::Scene>,
    /// `None` keeps the model.
    pub after_present: Option<Box<dyn Fn(&mut dyn Any, &frame::Frame) -> Option<Model>>>,
    /// `crash_dump model directory` adds the sketch's own files to a crash report.
    pub crash_dump: Option<Box<dyn Fn(&dyn Any, &str)>>,
    /// Runs about every millisecond while an unchanged picture waits for the next frame (input,
    /// or at most 16 ms): it serves background work for the main thread and answers whether more
    /// is in flight; the wait ends when it answers `false`.
    pub idle: Option<Box<dyn Fn(&dyn Any) -> bool>>,
    pub on_stop: Option<Box<dyn Fn(&dyn Any)>>,
}

fn startup_error(operation: &str, kind: ErrorKind, message: &str) -> Error {
    oerror::make(operation, kind, message)
}

type RelativeFn = Rc<dyn Fn(bool) -> Result<(), String>>;
type CursorFn = Rc<dyn Fn(Cursor) -> Result<(), String>>;
type BackgroundFn = Rc<dyn Fn((f64, f64, f64)) -> Result<(), String>>;
type DialogFn = Rc<dyn Fn(Option<&[(String, Vec<String>)]>, Option<&str>, Dialog) -> Result<i64, String>>;

struct RunContext {
    stopped: Cell<bool>,
    input: RefCell<input_state::InputState>,
    time: RefCell<time::Time>,
    relative: RefCell<Option<RelativeFn>>,
    cursor: RefCell<Option<CursorFn>>,
    background: RefCell<Option<BackgroundFn>>,
    dialog: RefCell<Option<DialogFn>>,
}

thread_local! {
    static ACTIVE_RUN: RefCell<Option<Rc<RunContext>>> = const { RefCell::new(None) };
    static LAST_PANIC: RefCell<Option<(String, String)>> = const { RefCell::new(None) };
    static PENDING_FAILURE: RefCell<Option<Error>> = const { RefCell::new(None) };
}

fn current_run() -> Option<Rc<RunContext>> {
    ACTIVE_RUN.with(|r| r.borrow().clone())
}

pub fn quit() {
    if let Some(state) = current_run() {
        state.stopped.set(true);
    }
}

const NO_SKETCH: &str = "no sketch is running";

/// Whether a sketch is running: a best-effort caller on a per-frame path asks first, since the setters below
/// build an error string (an allocation) when none is.
pub fn running() -> bool {
    ACTIVE_RUN.with(|r| r.borrow().is_some())
}

/// Capture and hide the pointer: `Frame.mouse_delta` then reports device motion even at the
/// window edge (fly cameras). Released when the sketch stops; an error when no sketch is running.
pub fn set_relative_mouse(enabled: bool) -> Result<(), String> {
    let set = current_run().and_then(|s| s.relative.borrow().clone());
    match set {
        None => Err(format!("Sketch.set_relative_mouse: {NO_SKETCH}")),
        Some(set) => set(enabled),
    }
}

/// Set the active native pointer cursor; an error when no sketch is running.
pub fn set_cursor(shape: Cursor) -> Result<(), String> {
    let set = current_run().and_then(|s| s.cursor.borrow().clone());
    match set {
        None => Err(format!("Sketch.set_cursor: {NO_SKETCH}")),
        Some(set) => set(shape),
    }
}

/// The native window's own background (the title bar's colour on macOS; nothing elsewhere). An
/// error when no sketch is running.
pub fn set_window_background(c: color::T) -> Result<(), String> {
    let set = current_run().and_then(|s| s.background.borrow().clone());
    match set {
        None => Err(format!("Sketch.set_window_background: {NO_SKETCH}")),
        Some(set) => {
            let (r, g, b, _) = color::to_tuple(c);
            set((r as f64 / 255., g as f64 / 255., b as f64 / 255.))
        }
    }
}

/// Open the system's file dialog and return at once with its id. When the user chooses or
/// cancels, `Input.File_dialog` arrives in a later frame with the same id. `filters` are a label
/// with the extensions it lists, without dots; an empty list means every file. No more than 8
/// dialogs can be open; an error when no sketch is running.
pub fn show_file_dialog(filters: Option<&[(String, Vec<String>)]>, default_location: Option<&str>, kind: Dialog) -> Result<i64, String> {
    let show = current_run().and_then(|s| s.dialog.borrow().clone());
    match show {
        None => Err(format!("Sketch.show_file_dialog: {NO_SKETCH}")),
        Some(show) => show(filters, default_location, kind),
    }
}

fn make_frame(config_: &Config, input_: &input_state::InputState, count: i64, time_: f64, dt: f64, events: Vec<Event>) -> frame::Frame {
    let mut input = input::at_time(time_);
    input.dt = dt;
    input.frame = count;
    input.tick = count;
    input.size = (config_.width, config_.height);
    input.pointer = input_state::mouse(input_);
    input.buttons = input_state::buttons(input_);
    input.keys = input_state::keys(input_);
    input.events = events;
    frame::Frame { input, pixel_scale: (1., 1.), fps: if dt > 0. { 1. / dt } else { 0. }, mouse_delta: input_state::mouse_delta(input_) }
}

// ---- crash reports: every fatal error in a sketch leaves a folder under the platform temporary
// directory's rays-crash (or RAYS_CRASH_DIR) with the panic, backtrace, recent input and
// environment facts, and whatever the sketch dumps. ----

fn crash_root(dir: &Option<String>) -> std::path::PathBuf {
    match dir {
        Some(d) => std::path::PathBuf::from(d),
        None => std::env::temp_dir().join("rays-crash"),
    }
}

fn key_name(key: &Key) -> String {
    match key {
        Key::KeyChar(c) => format!("{c:?}"),
        Key::ArrowUp => "Up".to_string(),
        Key::ArrowDown => "Down".to_string(),
        Key::ArrowLeft => "Left".to_string(),
        Key::ArrowRight => "Right".to_string(),
        Key::Space => "Space".to_string(),
        Key::Enter => "Enter".to_string(),
        Key::Escape => "Escape".to_string(),
        Key::Backspace => "Backspace".to_string(),
        Key::Tab => "Tab".to_string(),
        Key::Shift => "Shift".to_string(),
        Key::Ctrl => "Ctrl".to_string(),
        Key::Alt => "Alt".to_string(),
        Key::Meta => "Meta".to_string(),
        Key::Home => "Home".to_string(),
        Key::End => "End".to_string(),
        Key::PageUp => "PageUp".to_string(),
        Key::PageDown => "PageDown".to_string(),
        Key::Insert => "Insert".to_string(),
        Key::Delete => "Delete".to_string(),
        Key::Unknown(code) => format!("key{code}"),
        Key::F1 => "F1".to_string(),
        Key::F2 => "F2".to_string(),
        Key::F3 => "F3".to_string(),
        Key::F4 => "F4".to_string(),
        Key::F5 => "F5".to_string(),
        Key::F6 => "F6".to_string(),
        Key::F7 => "F7".to_string(),
        Key::F8 => "F8".to_string(),
        Key::F9 => "F9".to_string(),
        Key::F10 => "F10".to_string(),
        Key::F11 => "F11".to_string(),
        Key::F12 => "F12".to_string(),
    }
}

fn button_name(b: &MouseButton) -> &'static str {
    match b {
        MouseButton::LeftButton => "left",
        MouseButton::RightButton => "right",
        MouseButton::MiddleButton => "middle",
        MouseButton::MouseX1 => "x1",
        MouseButton::MouseX2 => "x2",
    }
}

fn event_text(e: &Event) -> String {
    match e {
        Event::KeyPressed(k) => format!("press {}", key_name(k)),
        Event::KeyReleased(k) => format!("release {}", key_name(k)),
        Event::PointerMoved((x, y)) => format!("move {x:.0},{y:.0}"),
        Event::PointerPressed(b, (x, y)) => format!("down {} {x:.0},{y:.0}", button_name(b)),
        Event::PointerReleased(b, (x, y)) => format!("up {} {x:.0},{y:.0}", button_name(b)),
        Event::PointerCancelled(b) => format!("cancel {}", button_name(b)),
        Event::Scrolled((x, y)) => format!("scroll {x:.2},{y:.2}"),
        Event::TextInput(t) => format!("text {t:?}"),
        Event::Paste(t) => format!("paste {} bytes", t.len()),
        Event::TextEditing { text, .. } => format!("ime {text:?}"),
        Event::FileDialog { id, result: Ok(paths) } => format!("dialog {id} {}", paths.len()),
        Event::FileDialog { id, result: Err(message) } => format!("dialog {id} failed {message:?}"),
        Event::FileDropped(f) => format!("drop {f}"),
        Event::Resized((w, h)) => format!("resize {w}x{h}"),
        Event::FileDragMoved((x, y)) => format!("drag {x:.0},{y:.0}"),
        Event::FileDragEnded => "drag-end".to_string(),
        Event::Pinched(scale) => format!("pinch {scale:.3}"),
        Event::TrackpadScrolled { delta: (x, y), phase, .. } => {
            let phase = match phase {
                input::ScrollPhase::Touched => "touch",
                input::ScrollPhase::Moved => "move",
                input::ScrollPhase::Lifted => "lift",
                input::ScrollPhase::Momentum => "momentum",
            };
            format!("trackpad {phase} {x:.1},{y:.1}")
        }
        Event::DialogOpened(id) => format!("dialog-opened {id}"),
        Event::PixelSizeChanged((w, h)) => format!("pixel-size {w}x{h}"),
        Event::VisibilityChanged(visible) => format!("visible {visible}"),
        Event::FocusLost => "focus-lost".to_string(),
        Event::FocusGained => "focus-gained".to_string(),
        Event::Closed => "close".to_string(),
    }
}

/// UTC civil time from the system clock using days-from-civil inversion.
fn utc_now() -> (i64, i64, i64, i64, i64, i64) {
    let secs = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_or(0, |d| d.as_secs() as i64);
    let days = secs.div_euclid(86400);
    let rem = secs.rem_euclid(86400);
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z.rem_euclid(146097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (if m <= 2 { y + 1 } else { y }, m, d, rem / 3600, (rem % 3600) / 60, rem % 60)
}

fn write_crash(crash_dir: &Option<String>, title: &str, dump: &dyn Fn(&str), recent: &[frame::Frame], panic_message: &str, backtrace: &str) {
    let (year, month, day, hour, minute, second) = utc_now();
    let sanitized: String = title.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
    let name = format!("{sanitized}-{year:04}{month:02}{day:02}-{hour:02}{minute:02}{second:02}-{}", std::process::id());
    let directory = crash_root(crash_dir).join(name);
    let directory_text = directory.to_string_lossy().to_string();
    let write = |file: &str, text: &str| -> Result<(), String> {
        runtime_resources::write_text(&directory.join(file).to_string_lossy(), text)
    };
    let result = (|| -> Result<(), String> {
        runtime_resources::ensure_directory(&directory_text)?;
        let mut out = String::new();
        let mut line = |s: String| {
            out.push_str(&s);
            out.push('\n');
        };
        line("Rays crash report".to_string());
        line(format!("sketch: {title}"));
        line(format!("time: {year:04}-{month:02}-{day:02} {hour:02}:{minute:02}:{second:02} UTC"));
        line(format!("command: {}", std::env::args().collect::<Vec<_>>().join(" ")));
        line(format!("pid: {}", std::process::id()));
        line(String::new());
        line(format!("panic: {panic_message}"));
        line(String::new());
        line("backtrace:".to_string());
        line(backtrace.to_string());
        line("recent frames (oldest first; frames without input are counted, not listed):".to_string());
        let mut quiet = 0;
        for f in recent {
            if f.input.events.is_empty() && f.input.keys.is_empty() && f.input.buttons.is_empty() {
                quiet += 1;
            } else {
                if quiet > 0 {
                    line(format!("  ... {quiet} idle frames"));
                    quiet = 0;
                }
                line(format!(
                    "  #{} t={:.3} {}x{} mouse={:.0},{:.0} keys=[{}] buttons=[{}] events=[{}]",
                    f.input.frame,
                    f.input.t,
                    f.input.size.0,
                    f.input.size.1,
                    f.input.pointer.0,
                    f.input.pointer.1,
                    f.input.keys.iter().map(key_name).collect::<Vec<_>>().join(" "),
                    f.input.buttons.iter().map(|b| button_name(b).to_string()).collect::<Vec<_>>().join(" "),
                    f.input.events.iter().map(event_text).collect::<Vec<_>>().join("; "),
                ));
            }
        }
        if quiet > 0 {
            line(format!("  ... {quiet} idle frames"));
        }
        line(String::new());
        line("environment:".to_string());
        let mut vars: Vec<(String, String)> = std::env::vars().filter(|(k, _)| k.starts_with("RAYS_") || k.starts_with("SDL_")).collect();
        vars.sort();
        for (k, v) in vars {
            line(format!("  {k}={v}"));
        }
        line(String::new());
        line("native crashes (segfaults, GPU faults) are not caught here: the platform's crash log has them.".to_string());
        write("crash.txt", &out)?;
        dump(&directory_text);
        eprintln!("Rays crash report: {directory_text}");
        Ok(())
    })();
    if let Err(e) = result {
        eprintln!("Rays crash report could not be written: {e}");
    }
}

static HOOK: std::sync::Once = std::sync::Once::new();

fn install_panic_hook() {
    HOOK.call_once(|| {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let backtrace = std::backtrace::Backtrace::force_capture().to_string();
            LAST_PANIC.with(|p| *p.borrow_mut() = Some((info.to_string(), backtrace)));
            previous(info);
        }));
    });
}

/// The state a crash report needs, shared with the loop.
struct CrashState {
    crash_dir: Option<String>,
    title: String,
    recent: RefCell<VecDeque<frame::Frame>>,
    model: RefCell<Option<Model>>,
    program: Rc<Program>,
}

fn update_model(stored: &RefCell<Option<Model>>, update: &dyn Fn(&mut dyn Any, &frame::Frame) -> Option<Model>, facts: &frame::Frame, op: &str) -> Result<(), Error> {
    let next = {
        let mut guard = stored.borrow_mut();
        let model = guard.as_mut().ok_or_else(|| startup_error(op, ErrorKind::InvalidState, "model is gone"))?;
        update(model.as_mut(), facts)
    };
    if let Some(next) = next { *stored.borrow_mut() = Some(next); }
    Ok(())
}

fn report_crash(state: &CrashState) {
    let (message, backtrace) = LAST_PANIC.with(|p| p.borrow_mut().take()).unwrap_or_else(|| ("(no panic information)".to_string(), String::new()));
    let frames: Vec<frame::Frame> = state.recent.borrow().iter().filter(|f| f.input.frame > 0).cloned().collect();
    let model_ref = state.model.try_borrow();
    let dump = |dir: &str| {
        if let (Ok(m), Some(d)) = (&model_ref, &state.program.crash_dump)
            && let Some(m) = m.as_ref() {
                d(m.as_ref(), dir);
            }
    };
    write_crash(&state.crash_dir, &state.title, &dump, &frames, &message, &backtrace);
}

// ---- runtime access ----

fn with_runtime_facts(coordinator: &rr::Renderer) -> Result<runtime::PresentationFacts, Error> {
    let mut out = None;
    native::runtime_of_mut(coordinator, &mut |r| out = Some(runtime::presentation_facts(r)));
    out.unwrap_or_else(|| Err(startup_error("Sketch.run_state", ErrorKind::InvalidState, "no runtime")))
}

fn run_runtime(coordinator: &rr::Renderer, f: &dyn Fn(&mut runtime::Runtime) -> Result<(), Error>) -> Result<(), Error> {
    let mut out = None;
    native::runtime_of_mut(coordinator, &mut |r| out = Some(f(r)));
    out.unwrap_or_else(|| Err(startup_error("Sketch.run_state", ErrorKind::InvalidState, "no runtime")))
}

/// The renderer first, then the window this sketch opened for it. A renderer that refuses to
/// destroy (shared leases) leaves the window alone too.
fn destroy_run(coordinator: &rr::Renderer, target: &runtime::Target) {
    if rr::destroy(coordinator).is_ok() {
        let _ = runtime::destroy(&mut target.borrow_mut());
    }
}

struct Finish {
    state: Rc<RunContext>,
    previous: Option<Rc<RunContext>>,
    coordinator: rr::Renderer,
    target: runtime::Target,
    caches: layer::CacheContext,
    model: Rc<CrashState>,
    last_scene: Rc<RefCell<Option<scene::Scene>>>,
    done: bool,
}

impl Finish {
    fn run(&mut self) {
        if self.done {
            return;
        }
        self.done = true;
        ACTIVE_RUN.with(|r| *r.borrow_mut() = self.previous.take());
        if let Some(set) = self.state.relative.borrow_mut().take() {
            // Never leave the pointer captured after the sketch stops.
            let _ = set(false);
        }
        *self.state.cursor.borrow_mut() = None;
        *self.state.background.borrow_mut() = None;
        *self.state.dialog.borrow_mut() = None;
        if let Some(scene_) = self.last_scene.borrow_mut().take() {
            layer::release(&scene_);
        }
        layer::clear_cache_context(&self.caches);
        destroy_run(&self.coordinator, &self.target);
    }
}

impl Drop for Finish {
    fn drop(&mut self) {
        if std::thread::panicking() {
            report_crash(&self.model);
        }
        if !self.done {
            // Run on_stop before destroying the renderer.
            if let (Some(on_stop), Ok(m)) = (&self.model.program.on_stop, self.model.model.try_borrow())
                && let Some(m) = m.as_ref() {
                    on_stop(m.as_ref());
                }
            self.run();
        }
    }
}

fn run_state_internal(
    config_: &Config,
    max_frames: Option<i64>,
    crash_dir: Option<String>,
    profile: bool,
    high_density: bool,
    program_: Program,
) -> Result<Model, Error> {
    let op = "Sketch.run_state";
    let invalid = |m: &str| startup_error(op, ErrorKind::InvalidArgument, m);
    if config_.width <= 0 || config_.height <= 0 {
        return Err(invalid("dimensions must be positive"));
    }
    if let Some(frames) = max_frames
        && frames <= 0 {
            return Err(invalid("max_frames must be positive"));
        }
    if let Some(fps) = config_.fps
        && fps <= 0 {
            return Err(invalid("fps must be positive"));
        }
    if let Some(domains) = config_.domains
        && domains <= 0 {
            return Err(invalid("domains must be positive"));
        }
    let vsync = !matches!(config_.clock, Clock::Fixed(_));
    if let Clock::Fixed(dt) = config_.clock
        && !(dt.is_finite() && dt > 0.) {
            return Err(invalid("fixed dt must be finite and positive"));
        }
    let input = input_state::create().map_err(|m| startup_error(op, ErrorKind::InvalidState, &format!("input: {m}")))?;
    let time_ = time::create(config_.fps.unwrap_or(0), vsync);
    let caches = layer::create_cache_context();
    let world_caches = world::private::create_cache_context();
    let native_caches = native_scene_lowering::create_cache_context();
    let state = Rc::new(RunContext {
        stopped: Cell::new(false),
        input: RefCell::new(input),
        time: RefCell::new(time_),
        relative: RefCell::new(None),
        cursor: RefCell::new(None),
        background: RefCell::new(None),
        dialog: RefCell::new(None),
    });
    let with_run_caches = |f: &mut dyn FnMut()| {
        layer::with_cache_context(&caches, &mut || world::private::with_cache_context(&world_caches, &mut *f));
    };
    let first = make_frame(config_, &state.input.borrow(), 0, 0., 0., Vec::new());
    let configuration = rr::Configuration {
        logical_width: config_.width,
        logical_height: config_.height,
        drawable_width: config_.width,
        drawable_height: config_.height,
        title: config_.title.clone(),
        vsync,
        high_density,
    };
    let target = runtime::open_window(&configuration.title, configuration.logical_width, configuration.logical_height, vsync, high_density)?;
    let coordinator = rr::create(&configuration, &target).inspect_err(|_| {
        let _ = runtime::destroy(&mut target.borrow_mut());
    })?;
    run_runtime(&coordinator, &|r| runtime::show(r)).inspect_err(|_| destroy_run(&coordinator, &target))?;
    let initial_facts = with_runtime_facts(&coordinator).inspect_err(|_| destroy_run(&coordinator, &target))?;
    if let Err(message) = input_state::configure(&state.input.borrow(), initial_facts.logical_width, initial_facts.logical_height) {
        destroy_run(&coordinator, &target);
        return Err(startup_error(op, ErrorKind::InvalidState, &format!("input: {message}")));
    }
    let mut logical_width = config_.width;
    let mut logical_height = config_.height;
    install_panic_hook();

    let capture = {
        let coordinator = coordinator.clone();
        move || -> Result<(i64, i64, Vec<u8>), String> {
            let facts = with_runtime_facts(&coordinator).map_err(|e| error_to_string(&e))?;
            rr::capture(&coordinator).map(|bytes| (facts.drawable_width, facts.drawable_height, bytes)).map_err(|e| error_to_string(&e))
        }
    };
    let save: canvas_runtime::Save = Rc::new(move |filename: &str| {
        let (width, height, bytes) = capture()?;
        let canvas = runtime_resources::canvas::of_rgba_bytes(width, height, bytes).map_err(|e| error_to_string(&e))?;
        let saved = runtime_resources::canvas::save_png(&canvas, filename).map_err(|e| error_to_string(&e));
        let _ = runtime_resources::canvas::destroy(&canvas);
        saved
    });

    let to_message = |e: Error| error_to_string(&e);
    {
        let (coordinator, input_state_) = (coordinator.clone(), state.clone());
        *state.relative.borrow_mut() = Some(Rc::new(move |enabled| {
            run_runtime(&coordinator, &|r| runtime::set_relative_mouse(r, enabled)).map_err(to_message)?;
            input_state::set_relative(&mut input_state_.input.borrow_mut(), enabled);
            Ok(())
        }));
    }
    {
        let coordinator = coordinator.clone();
        *state.background.borrow_mut() =
            Some(Rc::new(move |color_| run_runtime(&coordinator, &|r| runtime::set_window_background(r, color_)).map_err(to_message)));
    }
    {
        let coordinator = coordinator.clone();
        *state.cursor.borrow_mut() = Some(Rc::new(move |shape| {
            let shape = match shape {
                Cursor::Default => runtime::CursorShape::Default,
                Cursor::HorizontalResize => runtime::CursorShape::HorizontalResize,
                Cursor::VerticalResize => runtime::CursorShape::VerticalResize,
                Cursor::Text => runtime::CursorShape::Text,
            };
            run_runtime(&coordinator, &|r| runtime::set_cursor(r, shape)).map_err(to_message)
        }));
    }
    {
        let coordinator = coordinator.clone();
        *state.dialog.borrow_mut() = Some(Rc::new(move |filters, default_location, kind| {
            let filters: Vec<runtime::DialogFilter> = filters
                .unwrap_or(&[])
                .iter()
                .map(|(name, extensions)| runtime::DialogFilter {
                    name: name.clone(),
                    pattern: if extensions.is_empty() { "*".to_string() } else { extensions.join(";") },
                })
                .collect();
            let kind = match kind {
                Dialog::OpenFile => runtime::DialogKind::OpenFile,
                Dialog::OpenFiles => runtime::DialogKind::OpenFiles,
                Dialog::SaveFile => runtime::DialogKind::SaveFile,
                Dialog::OpenFolder => runtime::DialogKind::OpenFolder,
            };
            let mut id = None;
            native::runtime_of_mut(&coordinator, &mut |r| id = Some(runtime::show_dialog(r, Some(&filters), default_location, kind)));
            match id {
                Some(Ok(id)) => Ok(id),
                Some(Err(e)) => Err(error_to_string(&e)),
                None => Err("no runtime".to_string()),
            }
        }));
    }

    // a render failure inside the Scene.render callback is kept for the loop to return
    let render_failure: Rc<RefCell<Option<Error>>> = Rc::new(RefCell::new(None));
    let render_scene: Rc<dyn Fn(&scene::Scene)> = {
        let coordinator = coordinator.clone();
        let render_failure = render_failure.clone();
        Rc::new(move |scene_: &scene::Scene| {
            // ponytail: scan scene metadata each frame; move the focused region into staged scene
            // facts if large retained scenes make this measurable.
            let area = layer::text_regions(scene_).into_iter().find(|r| r.4).map(|(x, y, w, h, _, cursor_)| ((x, y, w, h), cursor_));
            let outcome = (|| -> Result<(), Error> {
                run_runtime(&coordinator, &|r| runtime::set_text_input(r, area))?;
                let facts = with_runtime_facts(&coordinator)?;
                let from_drawable = facts.drawable_width as f64 / std::cmp::max(1, facts.logical_width) as f64;
                let density = std::cmp::max(1, crate::num::fmax(facts.pixel_density, from_drawable).round() as i64);
                let mut rendered = Ok(());
                native_scene_lowering::with_cache_context(&native_caches, &mut || {
                    rendered = native_scene_lowering::render(&coordinator, density, facts.logical_width, facts.logical_height, scene_);
                });
                rendered.map_err(|e| startup_error("Sketch.render", ErrorKind::InvalidState, &native_scene_lowering::error_to_string(&e)))
            })();
            if let Err(e) = outcome {
                *render_failure.borrow_mut() = Some(e);
            }
        })
    };

    let program_ = Rc::new(program_);
    let crash = Rc::new(CrashState {
        crash_dir,
        title: config_.title.clone(),
        recent: RefCell::new(VecDeque::with_capacity(120)),
        model: RefCell::new(None),
        program: program_.clone(),
    });
    let release_after_failed_init = |caches: &layer::CacheContext, coordinator: &rr::Renderer| {
        layer::clear_cache_context(caches);
        destroy_run(coordinator, &target);
    };
    // init: a panic here writes a crash report (no frames yet) and leaves nothing running
    struct InitGuard {
        caches: layer::CacheContext,
        coordinator: rr::Renderer,
        target: runtime::Target,
        crash: Rc<CrashState>,
        armed: bool,
    }
    impl Drop for InitGuard {
        fn drop(&mut self) {
            if self.armed {
                if std::thread::panicking() {
                    report_crash(&self.crash);
                }
                layer::clear_cache_context(&self.caches);
                destroy_run(&self.coordinator, &self.target);
            }
        }
    }
    let mut init_guard = InitGuard { caches: caches.clone(), coordinator: coordinator.clone(), target: target.clone(), crash: crash.clone(), armed: true };
    let mut initial: Option<Model> = None;
    with_run_caches(&mut || initial = Some((program_.init)(&first)));
    init_guard.armed = false;
    drop(init_guard);
    let Some(initial) = initial else {
        release_after_failed_init(&caches, &coordinator);
        return Err(startup_error(op, ErrorKind::InvalidState, "init produced no model"));
    };
    *crash.model.borrow_mut() = Some(initial);

    let last_scene: Rc<RefCell<Option<scene::Scene>>> = Rc::new(RefCell::new(None));
    let previous = current_run();
    let mut finish = Finish {
        state: state.clone(),
        previous,
        coordinator: coordinator.clone(),
        target: target.clone(),
        caches: caches.clone(),
        model: crash.clone(),
        last_scene: last_scene.clone(),
        done: false,
    };
    ACTIVE_RUN.with(|r| *r.borrow_mut() = Some(state.clone()));
    let looped: Result<(), Error> = {
        let mut result = Ok(());
        let renderer_callback = render_scene.clone();
        layer::with_renderer(renderer_callback, &mut || {
            canvas_runtime::with_save(save.clone(), &mut || {
                result = run_loop(
                    config_,
                    max_frames,
                    profile,
                    &state,
                    &crash,
                    &program_,
                    &coordinator,
                    &caches,
                    &world_caches,
                    &last_scene,
                    &render_failure,
                    &mut logical_width,
                    &mut logical_height,
                );
            });
        });
        result
    };
    // on_stop sees the final model, then the renderer goes
    if let (Some(on_stop), Ok(m)) = (&program_.on_stop, crash.model.try_borrow())
        && let Some(m) = m.as_ref() {
            on_stop(m.as_ref());
        }
    finish.run();
    drop(finish);
    looped?;
    crash.model.borrow_mut().take().ok_or_else(|| startup_error(op, ErrorKind::InvalidState, "model is gone"))
}

#[allow(clippy::too_many_arguments)]
fn run_loop(
    config_: &Config,
    limit: Option<i64>,
    profile: bool,
    state: &Rc<RunContext>,
    crash: &Rc<CrashState>,
    program_: &Rc<Program>,
    coordinator: &rr::Renderer,
    caches: &layer::CacheContext,
    world_caches: &world::private::CacheContext,
    last_scene: &Rc<RefCell<Option<scene::Scene>>>,
    render_failure: &Rc<RefCell<Option<Error>>>,
    logical_width: &mut i64,
    logical_height: &mut i64,
) -> Result<(), Error> {
    let op = "Sketch.input";
    let mut count = 0_i64;
    let mut shown: Option<scene::Scene> = None;
    let mut quiet = 0_i64;
    let mut failure: Option<Error> = None;
    let stopped_or_done = |count: i64| state.stopped.get() || limit.is_some_and(|l| count >= l);
    while !stopped_or_done(count) {
        if let Some(e) = failure.take() {
            return Err(e);
        }
        let mut step: Result<(), Error> = Ok(());
        layer::with_cache_context(caches, &mut || {
            world::private::with_cache_context(world_caches, &mut || {
                step = one_frame(
                    config_, limit, profile, state, crash, program_, coordinator, last_scene, render_failure, logical_width, logical_height, &mut count, &mut shown, &mut quiet,
                    op,
                );
            });
        });
        step?;
        // an unchanged frame skipped vsync: wait a frame for input, and after half a second of the
        // same picture up to 100 ms, so a still window costs a few ticks a second
        if quiet > 0 {
            let timeout_ms: i32 = if quiet > 30 { 100 } else { 16 };
            let busy = program_.idle.as_ref().is_some_and(|idle| crash.model.borrow().as_ref().is_some_and(|m| idle(m.as_ref())));
            if busy {
                // work for the main thread is in flight (a cook's device requests): serve it as it
                // arrives instead of at the next frame
                let deadline = std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms as u64);
                while program_.idle.as_ref().is_some_and(|idle| crash.model.borrow().as_ref().is_some_and(|m| idle(m.as_ref())))
                    && std::time::Instant::now() < deadline
                    && !input_state::wait(&state.input.borrow(), 1)
                {}
            } else {
                input_state::wait(&state.input.borrow(), timeout_ms);
            }
        } else {
            time::limit_frame_rate(&state.time.borrow());
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn one_frame(
    config_: &Config,
    limit: Option<i64>,
    profile: bool,
    state: &Rc<RunContext>,
    crash: &Rc<CrashState>,
    program_: &Rc<Program>,
    coordinator: &rr::Renderer,
    last_scene: &Rc<RefCell<Option<scene::Scene>>>,
    render_failure: &Rc<RefCell<Option<Error>>>,
    logical_width: &mut i64,
    logical_height: &mut i64,
    count: &mut i64,
    shown: &mut Option<scene::Scene>,
    quiet: &mut i64,
    op: &str,
) -> Result<(), Error> {
    time::update(&mut state.time.borrow_mut());
    let unit = {
        let mut u = 1.;
        native::runtime_of_mut(coordinator, &mut |r| u = runtime::window_unit_of(r));
        u
    };
    input_state::set_unit(&mut state.input.borrow_mut(), unit);
    let polled = input_state::poll(&mut state.input.borrow_mut()).map_err(|m| startup_error(op, ErrorKind::InvalidState, &m))?;
    let events = polled.events.clone();
    // SDL announces a size or density change; the window is not polled.
    if polled.window_changed {
        native::runtime_of_mut(coordinator, &mut |r| runtime::window_changed(r));
    }
    // A covered window has no end event: while it is hidden, ask once a frame whether it is back.
    let visible = polled.visible || {
        let mut seen = false;
        native::runtime_of_mut(coordinator, &mut |r| seen = matches!(runtime::visible(r), Ok(true)));
        if seen {
            input_state::set_visible(&mut state.input.borrow_mut(), true);
        }
        seen
    };
    if events.iter().any(|e| matches!(e, Event::Closed)) {
        state.stopped.set(true);
    }
    *count += 1;
    let dt = match config_.clock {
        Clock::Realtime => time::delta(&state.time.borrow()),
        Clock::Fixed(value) => value,
    };
    let base = make_frame(
        config_,
        &state.input.borrow(),
        *count,
        match config_.clock {
            Clock::Realtime => time::now(&state.time.borrow()),
            Clock::Fixed(_) => *count as f64 * dt,
        },
        dt,
        events,
    );
    let presentation = with_runtime_facts(coordinator)?;
    if presentation.logical_width != *logical_width || presentation.logical_height != *logical_height {
        input_state::configure(&state.input.borrow(), presentation.logical_width, presentation.logical_height)
            .map_err(|m| startup_error(op, ErrorKind::InvalidState, &m))?;
    }
    *logical_width = presentation.logical_width;
    *logical_height = presentation.logical_height;
    let scale_x = presentation.drawable_width as f64 / presentation.logical_width as f64;
    let scale_y = presentation.drawable_height as f64 / presentation.logical_height as f64;
    let mut input = base.input.clone();
    input.size = (presentation.logical_width, presentation.logical_height);
    let facts = frame::Frame { input, pixel_scale: (scale_x, scale_y), ..base };
    {
        let mut recent = crash.recent.borrow_mut();
        if recent.len() == 120 {
            recent.pop_front();
        }
        recent.push_back(facts.clone());
    }
    // Keep the model stored while callbacks run, so unwinding can still dump it.
    update_model(&crash.model, &*program_.update, &facts, op)?;
    let scene_ = {
        let guard = crash.model.borrow();
        let m = guard.as_ref().ok_or_else(|| startup_error(op, ErrorKind::InvalidState, "model is gone"))?;
        (program_.view)(m.as_ref(), &facts)
    };
    *last_scene.borrow_mut() = Some(scene_.clone());
    // An interactive window that is minimized or covered is not drawn and the loop idles. A finite
    // run (max_frames, RAYS_MAX_FRAMES, export) is a test or a capture: it always renders. A window
    // whose picture did not change keeps the last one on screen, as a native view does: no draw, no
    // present, and the loop waits for input instead of vsync.
    if limit.is_some() {
        scene::render(&scene_);
    } else if !visible {
        *shown = None;
        std::thread::sleep(std::time::Duration::from_millis(33));
    } else if polled.window_changed || shown.as_ref().is_none_or(|last| !scene::same(last, &scene_)) {
        scene::render(&scene_);
        *shown = Some(scene_);
        *quiet = 0;
    } else {
        *quiet += 1;
    }
    if let Some(e) = render_failure.borrow_mut().take() {
        return Err(e);
    }
    if let Some(after) = &program_.after_present {
        update_model(&crash.model, &**after, &facts, op)?;
    }
    if let Some(e) = PENDING_FAILURE.with(|p| p.borrow_mut().take()) {
        return Err(e);
    }
    if profile && *count % 30 == 0
        && let Ok(s) = rr::stats(coordinator) {
            eprintln!(
                "profile frame {}: gpu {:.2} ms, draws {}, passes {}, uploaded {} B, sun passes {}, plan hits {} misses {}",
                count,
                s.gpu_duration_seconds * 1000.,
                s.logical_draws,
                s.logical_passes,
                s.uploaded_bytes,
                s.sun_shadow_passes,
                s.retained_plan_hits,
                s.retained_plan_misses
            );
        }
    Ok(())
}

/// Named lifecycle controls for [`run_state`].
#[derive(Default)]
pub struct RunOptions {
    pub config: Option<Config>,
    pub max_frames: Option<i64>,
    pub crash_dir: Option<String>,
    pub profile: bool,
}

/// Run a sketch with immutable user state threaded through every frame. Native startup, runtime,
/// validation and export failures are returned as `error`. `max_frames` keeps one runtime alive
/// for exactly that many frames unless `quit` is requested first (the process environment is
/// never read). `after_present` runs after the native frame is rendered, can capture that frame,
/// and returns the model for the next frame and `on_stop`.
pub fn run_state(options: RunOptions, program_: Program) -> Result<Model, Error> {
    let RunOptions { config: config_, max_frames, crash_dir, profile } = options;
    let config_ = config_.unwrap_or_else(default_config);
    let domains = config_.domains;
    let out: RefCell<Option<Result<Model, Error>>> = RefCell::new(None);
    let program_cell = RefCell::new(Some(program_));
    let _ = parallel::run_unit(domains, &|| {
        let program_ = program_cell.borrow_mut().take();
        if let Some(program_) = program_ {
            *out.borrow_mut() = Some(run_state_internal(&config_, max_frames, crash_dir.clone(), profile, true, program_));
        }
        Ok(())
    });
    out.into_inner().unwrap_or_else(|| Err(startup_error("Sketch.run_state", ErrorKind::InvalidState, "parallel run did not start")))
}


/// Stateful deterministic PNG-sequence export named `prefix-NNNNNN.png`. Startup and file-write
/// errors are returned. Captured frames use the renderer's native backing dimensions.
pub fn export_state(config_: Option<Config>, fps: Option<i64>, prefix: Option<String>, directory: &str, frames: i64, program_: Program) -> Result<Model, Error> {
    let op = "Sketch.export_state";
    let invalid = |m: &str| startup_error(op, ErrorKind::InvalidArgument, m);
    let fps = fps.unwrap_or(60);
    let prefix = prefix.unwrap_or_else(|| "frame".to_string());
    if frames <= 0 {
        return Err(invalid("frames must be positive"));
    }
    if fps <= 0 {
        return Err(invalid("fps must be positive"));
    }
    let base = std::path::Path::new(&prefix).file_name().map(|n| n.to_string_lossy().to_string());
    if prefix.is_empty() || prefix == "." || prefix == ".." || base.as_deref() != Some(prefix.as_str()) {
        return Err(invalid("invalid prefix"));
    }
    let index = Rc::new(Cell::new(0_i64));
    let mut program_ = program_;
    let after_export = program_.after_present.take();
    let directory = directory.to_string();
    // the exported frame is saved first; the sketch's own after_present then runs
    program_.after_present = Some(Box::new(move |m: &mut dyn Any, f: &frame::Frame| {
        let filename = std::path::Path::new(&directory).join(format!("{prefix}-{:06}.png", index.get()));
        match canvas_runtime::save(&filename.to_string_lossy()) {
            Ok(()) => index.set(index.get() + 1),
            Err(message) => {
                PENDING_FAILURE.with(|p| *p.borrow_mut() = Some(startup_error("Sketch.export_state", ErrorKind::InvalidState, &format!("frame export failed: {message}"))));
                return None;
            }
        }
        after_export.as_ref().and_then(|after| after(m, f))
    }));
    let mut config_ = config_.unwrap_or_else(default_config);
    config_.clock = Clock::Fixed(1. / fps as f64);
    config_.fps = None;
    let domains = config_.domains;
    let out: RefCell<Option<Result<Model, Error>>> = RefCell::new(None);
    let program_cell = RefCell::new(Some(program_));
    let _ = parallel::run_unit(domains, &|| {
        let program_ = program_cell.borrow_mut().take();
        if let Some(program_) = program_ {
            *out.borrow_mut() = Some(run_state_internal(&config_, Some(frames), None, false, false, program_));
        }
        Ok(())
    });
    out.into_inner().unwrap_or_else(|| Err(startup_error(op, ErrorKind::InvalidState, "parallel run did not start")))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn model_updates_retain_state_for_crash_dump() {
        let mut bytes = Vec::<u8>::with_capacity(8);
        bytes.push(1);
        let identity = bytes.as_ptr();
        let stored = RefCell::new(Some(Box::new(bytes) as Model));
        let facts = frame::Frame { input: input::at_time(0.), pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        update_model(&stored, &|model, _| { model.downcast_mut::<Vec<u8>>().unwrap().push(2); None }, &facts, "test").unwrap();
        assert_eq!(stored.borrow().as_ref().unwrap().downcast_ref::<Vec<u8>>().unwrap().as_ptr(), identity);
        update_model(&stored, &|_, _| Some(Box::new(vec![3_u8])), &facts, "test").unwrap();
        let failed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            update_model(&stored, &|_, _| panic!("replacement failed"), &facts, "test").unwrap();
        }));
        assert!(failed.is_err());
        assert_eq!(stored.borrow().as_ref().unwrap().downcast_ref::<Vec<u8>>().unwrap(), &[3]);
        let failed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            update_model(&stored, &|model, _| {
                    model.downcast_mut::<Vec<u8>>().unwrap().push(4);
                    panic!("mutation failed");
            }, &facts, "test").unwrap();
        }));
        assert!(failed.is_err());
        let directory = std::env::temp_dir().join(format!("rays-crash-model-{}-{}", std::process::id(), std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()));
        let state = CrashState {
            crash_dir: Some(directory.to_string_lossy().into_owned()), title: "retained model".into(), recent: RefCell::new(VecDeque::new()), model: stored,
            program: Rc::new(Program {
                init: Box::new(|_| Box::new(())), update: Box::new(|_, _| None), view: Box::new(|_, _| Vec::new()), after_present: None,
                crash_dump: Some(Box::new(|model, directory| {
                    let bytes = model.downcast_ref::<Vec<u8>>().unwrap();
                    std::fs::write(std::path::Path::new(directory).join("model.txt"), format!("{bytes:?}")).unwrap();
                })), idle: None, on_stop: None,
            }),
        };
        LAST_PANIC.with(|panic| *panic.borrow_mut() = Some(("mutation failed".into(), String::new())));
        report_crash(&state);
        let report = std::fs::read_dir(&directory).unwrap().next().unwrap().unwrap().path();
        assert_eq!(std::fs::read_to_string(report.join("model.txt")).unwrap(), "[3, 4]");
        assert!(std::fs::read_to_string(report.join("crash.txt")).unwrap().contains("mutation failed"));
        std::fs::remove_dir_all(directory).unwrap();
    }
}
