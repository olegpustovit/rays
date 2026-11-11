//! The PXUI kit gallery as a window app: `cargo run -p pxui_gallery`. On macOS a `pxui_sdl` window
//! on Metal, on Windows `pxui_sdl` on Vulkan, on Linux a `pxui_wayland` window on Vulkan with the
//! `pxui_atspi` bridge when an accessibility bus answers. The loop honours the frame gate: it builds a
//! frame only for input, a due `repaint_at` (a toast's expiry among them) or the theme file's once-a-second poll,
//! draws only when the draw revision moved (a frame, or the busy widgets' patches), and otherwise
//! sleeps in the host's wait.
//!
//! Environment: `PXUI_GALLERY_PAGE=n` (0..3) and `PXUI_GALLERY_DUSK=1` pick the start;
//! `PXUI_GALLERY_RTL=1` and `PXUI_GALLERY_COMPACT=1` set those switches; `PXUI_GALLERY_THEME=path` names the second theme's file (default `crates/pxui/themes/dusk.theme`,
//! reloaded when its modification time changes); `PXUI_GALLERY_HIDDEN=1` keeps the window hidden and
//! draws offscreen only; `PXUI_GALLERY_SHOT=out.png` saves the third drawn frame and exits;
//! `PXUI_GALLERY_EXIT_AFTER=secs` exits after that long; `PXUI_GALLERY_COUNT=1` prints the frame
//! counters on exit (`frames`, `draws`, and `settled`, the frames built after the first half second).
#[path = "../../pxui_render/tests/png.rs"]
#[allow(dead_code)]
mod png;

use input::{Event, Key};
use ogpu_core::backend::{self as be, AcquireResult, ColorAttachment, RenderTarget, Surface};
use ogpu_core::render_pass::{Load, Store};
use ogpu_core::surface::{Configuration, Format, PresentMode};
use pxui::ui::{Theme, Ui};
use pxui_gallery::gpu::{self, fail, Gpu};
use pxui_gallery::*;
use std::time::{Instant, SystemTime};

const PRAGMASEVKA: &[u8] = include_bytes!("../../../assets/fonts/Pragmasevka-Regular.ttf");

/// The window host this platform links, behind the functions every PXUI host has.
#[cfg(any(target_os = "macos", windows))]
mod host {
    pub use pxui_sdl::{Extent, Window};
    use pxui_sdl::CursorShape;

    pub fn open(w: i64, h: i64, hidden: bool) -> Result<Window, String> {
        Window::open_with("PXUI gallery", w, h, true, hidden, true)
    }

    /// Sleep until an event arrives or `deadline` (window time) passes.
    // ponytail: pxui_sdl has no `wait` (pxui_wayland's `Window::wait`); SDL's own wait stands in.
    pub fn wait(win: &Window, deadline: Option<f64>) {
        let ms = deadline.map_or(-1, |at| ((at - win.time()).max(0.0) * 1000.0).ceil().min(i32::MAX as f64) as i32);
        let _ = if ms < 0 { sdl3::event::wait(i32::MAX) } else { sdl3::event::wait(ms) };
    }

    pub fn cursor(win: &mut Window, c: Option<pxui::ui::Cursor>) {
        let shape = match c {
            Some(pxui::ui::Cursor::HorizontalResize) => CursorShape::HorizontalResize,
            Some(pxui::ui::Cursor::VerticalResize) => CursorShape::VerticalResize,
            Some(pxui::ui::Cursor::Text) => CursorShape::Text,
            None => CursorShape::Default,
        };
        let _ = win.set_cursor(shape);
    }

    pub fn clipboard(_: &mut Window, text: &str) {
        let _ = pxui_sdl::clipboard_set_text(text);
    }

    /// The clipboard's text as the Paste event the UI asked for.
    pub fn paste(_: &mut Window, events: &mut Vec<input::Event>) {
        events.insert(0, input::Event::Paste(pxui_sdl::clipboard_get_text().unwrap_or_default()));
    }

    pub fn background(win: &mut Window, c: (f64, f64, f64)) {
        let _ = win.set_background(c);
    }

    /// No accessibility bridge on this platform yet.
    pub struct A11y;
    pub fn a11y(_: &mut Window) -> Option<A11y> {
        None
    }
    pub fn a11y_gone(_: &mut Window) {}
    pub fn a11y_update(_: &mut A11y, _: &pxui::a11y::A11yTree) {}
    pub fn a11y_pump(_: &mut A11y, _: &mut Vec<pxui::a11y::A11yAction>) -> bool {
        true
    }
}

#[cfg(target_os = "linux")]
mod host {
    pub use pxui_wayland::{Extent, Window};
    use pxui_wayland::CursorShape;

    pub fn open(w: i64, h: i64, hidden: bool) -> Result<Window, String> {
        Window::open_with("PXUI gallery", w, h, true, hidden, true)
    }

    pub fn wait(win: &mut Window, deadline: Option<f64>) {
        win.wait(deadline);
    }

    pub fn cursor(win: &mut Window, c: Option<pxui::ui::Cursor>) {
        let shape = match c {
            Some(pxui::ui::Cursor::HorizontalResize) => CursorShape::HorizontalResize,
            Some(pxui::ui::Cursor::VerticalResize) => CursorShape::VerticalResize,
            Some(pxui::ui::Cursor::Text) => CursorShape::Text,
            None => CursorShape::Default,
        };
        let _ = win.set_cursor(shape);
    }

    pub fn clipboard(win: &mut Window, text: &str) {
        let _ = win.clipboard_set_text(text);
    }

    /// Ask the compositor for the selection: its Paste event arrives with a later pump.
    pub fn paste(win: &mut Window, _: &mut Vec<input::Event>) {
        let _ = win.request_paste();
    }

    pub fn background(_: &mut Window, _: (f64, f64, f64)) {}

    pub use pxui_atspi::Bridge as A11y;
    /// The AT-SPI2 bridge when the session has an accessibility bus.
    pub fn a11y(win: &mut Window) -> Option<A11y> {
        let bridge = A11y::connect("PXUI gallery").ok()?;
        win.set_wake_fd(bridge.fd()); // a screen reader's call wakes the idle wait
        Some(bridge)
    }
    /// The bridge is being dropped: stop sleeping on its descriptor.
    pub fn a11y_gone(win: &mut Window) {
        win.set_wake_fd(-1);
    }
    pub fn a11y_update(b: &mut A11y, tree: &pxui::a11y::A11yTree) {
        b.update(tree);
    }
    /// False once the bus has gone.
    pub fn a11y_pump(b: &mut A11y, out: &mut Vec<pxui::a11y::A11yAction>) -> bool {
        b.pump(out)
    }
}

fn env(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.is_empty())
}

fn modified(path: &str) -> Option<SystemTime> {
    std::fs::metadata(path).and_then(|m| m.modified()).ok()
}

/// The second theme from its file over the default; the default with the errors printed when it
/// does not parse.
fn load_theme(path: &str) -> Theme {
    match std::fs::read_to_string(path).map_err(|e| e.to_string()).and_then(|text| Theme::parse(&text, &Theme::default()).map_err(|e| format!("{e:?}"))) {
        Ok(theme) => theme,
        Err(e) => {
            eprintln!("pxui_gallery: {path}: {e}");
            Theme::default()
        }
    }
}

fn configuration(win: &mut host::Window) -> Configuration {
    let extent: host::Extent = win.extent();
    Configuration {
        logical_width: extent.logical.0,
        logical_height: extent.logical.1,
        physical_width: extent.drawable.0,
        physical_height: extent.drawable.1,
        format: Format::Bgra8Unorm,
        present_mode: if win.vsync() { PresentMode::Fifo } else { PresentMode::Immediate },
        max_acquired: 1,
        layer: win.token().ok(),
    }
}

fn main() {
    let shot = env("PXUI_GALLERY_SHOT");
    let hidden = env("PXUI_GALLERY_HIDDEN").is_some();
    let exit_after = env("PXUI_GALLERY_EXIT_AFTER").and_then(|v| v.parse::<f64>().ok());
    let count = env("PXUI_GALLERY_COUNT").is_some();
    let theme_path = env("PXUI_GALLERY_THEME").unwrap_or_else(|| concat!(env!("CARGO_MANIFEST_DIR"), "/../pxui/themes/dusk.theme").to_owned());
    let mut s = Gallery::new();
    s.page = env("PXUI_GALLERY_PAGE").and_then(|v| v.parse().ok()).unwrap_or(0) % PAGES.len();
    s.dusk = env("PXUI_GALLERY_DUSK").is_some();
    (s.rtl, s.compact, s.host_dirty) = (env("PXUI_GALLERY_RTL").is_some(), env("PXUI_GALLERY_COMPACT").is_some(), true);
    let mut win = match host::open(WINDOW_W, WINDOW_H, hidden) {
        Ok(w) => w,
        Err(e) => {
            eprintln!("pxui_gallery: no window: {e}");
            std::process::exit(1)
        }
    };
    let mut gpu = Gpu::open().unwrap_or_else(|e| {
        eprintln!("pxui_gallery: no GPU: {e}");
        std::process::exit(1)
    });
    let surface: Option<Surface> = if hidden { None } else { Some(be::create_surface(&gpu.device, configuration(&mut win)).unwrap_or_else(|e| fail(e))) };
    let platform = if cfg!(target_os = "macos") { input::Platform::Apple } else { input::Platform::Other };
    let mut ui = Ui::create(platform, None, pxui::text::Text::new(&[PRAGMASEVKA]));
    let mut dusk = load_theme(&theme_path);
    let mut stamp = modified(&theme_path);
    ui.set_theme(s.theme(&dusk));
    s.image = Some(gpu.image(100, 60, &gpu::test_card(100, 60)));
    let mut painter = gpu.painter(&mut ui);
    let mut extent = win.extent();
    let mut target = gpu.target(extent.drawable.0 as u32, extent.drawable.1 as u32);
    let mut a11y = host::a11y(&mut win);
    ui.set_a11y(a11y.is_some()); // the tree costs nothing unless a bridge is listening
    let mut actions = Vec::with_capacity(16);
    let mut a11y_seen = 0;

    let mut events: Vec<Event> = Vec::with_capacity(256);
    let mut frame = input::frame::Frame { input: input::at_time(0.0), pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) };
    frame.input.keys.reserve(8);
    frame.input.buttons.reserve(4);
    let (mut frames, mut draws, mut settled) = (0u64, 0u64, 0u64);
    let mut drawn_revision = -1i64;
    let mut next_poll = 0.0;
    let mut last = 0.0;
    let mut cost = Cost::default();
    let mut text = String::with_capacity(160);
    let mut force = true;
    let started = Instant::now();
    mem_sys::local_reset();
    loop {
        events.clear();
        if !win.pump(&mut events) {
            break;
        }
        let now = win.time();
        // the second theme's file, polled once a second; a changed modification time reloads it
        if now >= next_poll || s.reload {
            next_poll = now + 1.0;
            let m = modified(&theme_path);
            if m != stamp || s.reload {
                (stamp, s.reload) = (m, false);
                dusk = load_theme(&theme_path);
                if s.dusk {
                    s.host_dirty = true;
                }
                s.toast("Theme file reloaded");
                force = true;
            }
        }
        if s.host_dirty {
            ui.set_theme(s.theme(&dusk));
            ui.set_dir(if s.rtl { pxui::ui::Direction::Rtl } else { pxui::ui::Direction::Ltr });
            (s.page_rev, s.chrome_rev, s.host_dirty, force) = (s.page_rev + 1, s.chrome_rev + 1, false, true);
        }
        if let Some(bridge) = a11y.as_mut() {
            actions.clear();
            let alive = host::a11y_pump(bridge, &mut actions);
            if !actions.is_empty() {
                ui.inject_a11y(&actions);
                force = true;
            }
            if !alive {
                host::a11y_gone(&mut win);
                (a11y, a11y_seen) = (None, 0);
                ui.set_a11y(false); // the bus is gone: no tree, and nothing left to wake the wait
            }
        }
        if win.extent() != extent {
            extent = win.extent();
            if let Some(surface) = &surface {
                be::configure(surface, configuration(&mut win)).unwrap_or_else(|e| fail(e));
            }
            be::destroy_texture(&target).unwrap_or_else(|e| fail(e));
            target = gpu.target(extent.drawable.0 as u32, extent.drawable.1 as u32);
            force = true;
        }
        let built = force || ui.wants_frame(&events, now);
        if built {
            force = false;
            if ui.output().platform.paste {
                host::paste(&mut win, &mut events);
            }
            let f = &mut frame.input;
            for e in &events {
                match e {
                    Event::PointerMoved(p) => f.pointer = *p,
                    Event::PointerPressed(b, _) if !f.buttons.contains(b) => f.buttons.push(*b),
                    Event::PointerReleased(b, _) => f.buttons.retain(|x| x != b),
                    Event::KeyPressed(k @ (Key::Shift | Key::Ctrl | Key::Alt | Key::Meta)) if !f.keys.contains(k) => f.keys.push(*k),
                    Event::KeyReleased(k) => f.keys.retain(|x| x != k),
                    Event::FocusLost => (f.keys.clear(), f.buttons.clear()).1,
                    _ => {}
                }
            }
            (f.t, f.dt, f.frame, f.tick) = (now, now - last, frames as i64, frames as i64);
            f.size = extent.logical;
            f.events = std::mem::take(&mut events);
            frame.pixel_scale = (win.scale(), win.scale());
            last = now;
            let a0 = mem_sys::local_alloc_count();
            let t0 = Instant::now();
            let revs = (s.page_rev, s.chrome_rev);
            ui.frame(&frame, &mut |ui| app(ui, &mut s));
            // a page switch inside the frame: build once more so the views and the accessibility tree show the new state
            force |= (s.page_rev, s.chrome_rev) != revs;
            cost.us = t0.elapsed().as_secs_f64() * 1e6;
            cost.allocs = Some(mem_sys::local_alloc_count() - a0);
            events = std::mem::take(&mut frame.input.events);
            (cost.nodes, cost.views, cost.laid_out) = (ui.built_nodes(), ui.rebuilt_views(), ui.arranged_nodes());
            frames += 1;
            cost.frames = frames;
            if started.elapsed().as_secs_f64() > 0.5 {
                settled += 1;
            }
            let out = ui.output();
            let (cursor, clip, ground) = (out.platform.cursor, out.platform.clipboard.clone(), out.platform.background);
            host::cursor(&mut win, cursor);
            if let Some(text) = clip {
                host::clipboard(&mut win, &text);
            }
            if let Some(c) = ground {
                host::background(&mut win, (c.r as f64 / 255.0, c.g as f64 / 255.0, c.b as f64 / 255.0));
            }
            if let Some(bridge) = a11y.as_mut() {
                let tree = ui.output().a11y;
                if tree.revision != a11y_seen {
                    a11y_seen = tree.revision;
                    host::a11y_update(bridge, tree);
                }
            }
            // the readout is one patched node: it changes no view
            readout(&mut text, &cost);
            if let Some(h) = s.readout {
                ui.patch_text(&h, &text);
            }
            if s.quit {
                break;
            }
        }
        // the busy widgets move through patches, without a frame
        let animating = s.page == 1 && !s.reduced;
        if animating {
            for i in 0..s.busy.len() {
                let h = s.busy[i];
                ui.busy_step(&h, now);
            }
        }
        if ui.draw_revision() != drawn_revision {
            drawn_revision = ui.draw_revision();
            draw(&gpu, &mut painter, &mut ui, &target, surface.as_ref(), extent, win.scale() as f32);
            draws += 1;
            cost.bytes = painter.retained_bytes();
            if let Some(path) = shot.as_deref().filter(|_| draws >= 3) {
                let (w, h) = (extent.drawable.0 as u32, extent.drawable.1 as u32);
                let pixels = be::read_texture(&target, (w * 4) as i64).unwrap_or_else(|e| fail(e));
                if let Err(e) = std::fs::write(path, png::encode(w, h, &pixels)) {
                    eprintln!("pxui_gallery: {path}: {e}");
                }
                break;
            }
        }
        if exit_after.is_some_and(|secs| now >= secs) {
            break;
        }
        // nothing to do: sleep until input, the UI's repaint time (a toast's end), the theme poll
        // or the next animation step
        let mut wake = ui.repaint_at().unwrap_or(f64::INFINITY).min(next_poll);
        if animating {
            wake = wake.min(now + 1.0 / 60.0);
        }
        if shot.is_some() && draws < 3 {
            (wake, force) = (now, true);
        }
        if let Some(secs) = exit_after {
            wake = wake.min(secs);
        }
        if wake > win.time() {
            host::wait(&mut win, wake.is_finite().then_some(wake));
        }
    }
    if count {
        println!("pxui_gallery: frames {frames} draws {draws} settled {settled}");
    }
    drop(painter);
    be::destroy_texture(&target).unwrap_or_else(|e| fail(e));
    if let Some(surface) = surface {
        be::destroy_surface(&surface).unwrap_or_else(|e| fail(e));
    }
    s.image = None;
    ui.destroy();
    gpu.close();
}

/// Draw the UI into `target` and, with a window surface, present it.
fn draw(gpu: &Gpu, painter: &mut pxui_render::Painter, ui: &mut Ui, target: &be::Texture, surface: Option<&Surface>, extent: host::Extent, scale: f32) {
    let ground = ui.theme().tokens.panel;
    let frame = match surface {
        Some(surface) => match be::acquire_sync(surface).unwrap_or_else(|e| fail(e)) {
            AcquireResult::Acquired(frame) => Some(frame),
            _ => return,
        },
        None => None,
    };
    let commands = be::begin_commands(&gpu.queue).unwrap_or_else(|e| fail(e));
    let encoder = be::render_encoder(&commands, &RenderTarget {
        colors: vec![ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear: (ground.r as f64 / 255.0, ground.g as f64 / 255.0, ground.b as f64 / 255.0, 1.0) }],
        depth: None,
    }).unwrap_or_else(|e| fail(e));
    gpu::paint(painter, ui, &encoder, [extent.drawable.0 as u32, extent.drawable.1 as u32], scale).unwrap_or_else(|e| fail(e));
    be::end_render(&encoder).unwrap_or_else(|e| fail(e));
    let receipt = match frame {
        Some(frame) => be::commit_present(&commands, target, &frame).unwrap_or_else(|e| fail(e)),
        None => be::commit(&commands).unwrap_or_else(|e| fail(e)),
    };
    be::complete_through(&gpu.queue, receipt.epoch).unwrap_or_else(|e| fail(e));
}
