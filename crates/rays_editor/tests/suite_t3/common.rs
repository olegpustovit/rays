//! Shared editor frame, workspace and crash-dump test helpers.
#![allow(dead_code)]
use std::sync::Arc;

use editor_document::workspace_doc as wd;
use input::{Event, Key, MouseButton, Platform};
use rays::frame;
use rays_editor::{editor, workspace};

pub fn key(k: Key) -> Event {
    Event::Key_pressed(k)
}
pub fn ch(c: char) -> Event {
    Event::Key_pressed(Key::KeyChar(c))
}

/// `Test_editor_input.frame ?buttons ?keys ?delta mouse events count` (900 x 640 points).
pub fn frame_full(buttons: &[MouseButton], keys: &[Key], delta: (f64, f64), mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::Frame {
    frame::Frame {
        input: input::Frame {
            dt: 1. / 60.,
            frame: count,
            tick: count,
            size: (900, 640),
            pointer: mouse,
            keys: keys.to_vec(),
            buttons: buttons.to_vec(),
            events,
            ..input::at_time(count as f64 / 60.)
        },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: delta,
    }
}

pub fn frame_at(mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::Frame {
    frame_full(&[], &[], (0., 0.), mouse, events, count)
}

pub fn frame_with(buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::Frame {
    frame_full(buttons, keys, (0., 0.), mouse, events, count)
}

/// `Rays_editor.Workspace.load text |> get_ok`.
pub fn load(text: &str) -> wd::T {
    workspace::load(None, Vec::new(), None, text).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")))
}

/// Ws_fixture.box: one unit box.
pub fn box_workspace() -> wd::T {
    load("(workspace box (graph geo :context sop (sop/box)))")
}

/// Test defaults: Apple platform, 11-point text, awaited cook.
pub fn options() -> editor::Create {
    editor::Create { platform: Some(Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(1), ..Default::default() }
}

pub fn create(o: editor::Create, ws: &wd::T) -> editor::Editor {
    editor::create(o, ws, None, editor::prepare_fn(), editor::draw_fn()).unwrap_or_else(|m| panic!("editor did not open: {m}"))
}

/// `E3.create ~platform:Apple ~ui_text_size:11 ~await:true ~domains:1 ~workspace` over the text.
pub fn open(text: &str) -> editor::Editor {
    create(editor::Create { domains: Some(1), ..options() }, &load(text))
}

/// `Workspace.edit`'s physical equality: the same checked document.
pub fn same_ws(a: &wd::T, b: &wd::T) -> bool {
    Arc::ptr_eq(&a.checked, &b.checked)
}

pub fn text_of(e: &editor::Editor) -> String {
    wd::to_text(&editor::workspace(e)).unwrap()
}

pub fn has(text: &str, piece: &str) -> bool {
    text.contains(piece)
}

/// Runs `f`, closing the editor after it (`Fun.protect ~finally:close`).
pub fn with_editor(text: &str, f: impl FnOnce(editor::Editor)) {
    let e = open(text);
    let guard = e.clone();
    f(e);
    editor::close(&guard);
}

/// A line of the crash dump's editor.txt after `key: `.
pub fn dump_line(e: &editor::Editor, key: &str) -> String {
    let dir = scratch_dir("rays-t3-dump");
    editor::crash_dump(e, &dir).unwrap_or_else(|m| panic!("crash_dump: {m}"));
    let text = std::fs::read_to_string(format!("{dir}/editor.txt")).expect("editor.txt");
    let _ = std::fs::remove_dir_all(&dir);
    let prefix = format!("{key}: ");
    text.lines().find_map(|l| l.strip_prefix(prefix.as_str()).map(str::to_string)).unwrap_or_else(|| panic!("no {key} in the dump"))
}

pub fn scratch_dir(prefix: &str) -> String {
    use std::sync::atomic::{AtomicU32, Ordering};
    static N: AtomicU32 = AtomicU32::new(0);
    let dir = std::env::temp_dir().join(format!("{prefix}-{}-{}", std::process::id(), N.fetch_add(1, Ordering::Relaxed)));
    std::fs::create_dir_all(&dir).expect("scratch dir");
    dir.to_string_lossy().into_owned()
}

/// A mutable editor with the frame counter, `let step ... = incr count; e := E3.update !e (frame ..)`.
pub struct Driver {
    cur: Option<editor::Editor>,
    pub count: i64,
}

impl Driver {
    pub fn new(e: editor::Editor) -> Driver {
        Driver { cur: Some(e), count: 0 }
    }
    pub fn e(&self) -> &editor::Editor {
        self.cur.as_ref().expect("editor")
    }
    pub fn set(&mut self, e: editor::Editor) {
        self.cur = Some(e);
    }
    pub fn take(&mut self) -> editor::Editor {
        self.cur.take().expect("editor")
    }
    pub fn step(&mut self, events: Vec<Event>) {
        self.step_full(&[], &[], (450., 300.), events, 1);
    }
    pub fn step_at(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(&[], &[], mouse, events, 1);
    }
    pub fn step_keys(&mut self, keys: &[Key], events: Vec<Event>) {
        self.step_full(&[], keys, (450., 300.), events, 1);
    }
    pub fn step_full(&mut self, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>, advance: i64) {
        self.count += advance;
        let f = frame_with(buttons, keys, mouse, events, self.count);
        let e = self.take();
        self.cur = Some(editor::update(e, &f));
    }
    pub fn close(&self) {
        editor::close(self.e());
    }
    pub fn text(&self) -> String {
        text_of(self.e())
    }
}

impl Driver {
    /// `e := f !e` outside a frame (a `Reduce`, a selection).
    pub fn map(&mut self, f: impl FnOnce(editor::Editor) -> editor::Editor) {
        let e = self.take();
        self.cur = Some(f(e));
    }
}
