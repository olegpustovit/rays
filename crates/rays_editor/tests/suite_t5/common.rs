//! Shared helpers of suite t3: ports of test_editor_input.ml's `frame`, ws_fixture.ml, the
//! `E3.create ... Editor_prepare.*` calls and the crash-dump readers the OCaml tests use.
#![allow(dead_code)]
use std::sync::Arc;

use editor_document::workspace_doc as wd;
use input::{Event, Key, MouseButton, Platform};
use rays::frame;
use rays_editor::{Editor, Workspace};

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
    Workspace::load(None, Vec::new(), None, text).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")))
}

/// Ws_fixture.box: one unit box.
pub fn box_workspace() -> wd::T {
    load("(workspace box (graph geo :context sop (sop/box)))")
}

/// The options every OCaml test passes: Apple platform, 11-point text, awaited cook.
pub fn options() -> Editor::Create {
    Editor::Create { platform: Some(Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(1), ..Default::default() }
}

pub fn create(o: Editor::Create, ws: &wd::T) -> Editor::t {
    Editor::create(o, ws, None, Editor::prepare_fn(), Editor::draw_fn()).unwrap_or_else(|m| panic!("editor did not open: {m}"))
}

/// `E3.create ~platform:Apple ~ui_text_size:11 ~await:true ~domains:1 ~workspace` over the text.
pub fn open(text: &str) -> Editor::t {
    create(Editor::Create { domains: Some(1), ..options() }, &load(text))
}

/// `Workspace.edit`'s physical equality: the same checked document.
pub fn same_ws(a: &wd::T, b: &wd::T) -> bool {
    Arc::ptr_eq(&a.checked, &b.checked)
}

pub fn text_of(e: &Editor::t) -> String {
    wd::to_text(&Editor::workspace(e)).unwrap()
}

pub fn has(text: &str, piece: &str) -> bool {
    text.contains(piece)
}

/// Runs `f`, closing the editor after it (`Fun.protect ~finally:close`).
pub fn with_editor(text: &str, f: impl FnOnce(Editor::t)) {
    let e = open(text);
    let guard = e.clone();
    f(e);
    Editor::close(&guard);
}

/// A line of the crash dump's editor.txt after `key: `.
pub fn dump_line(e: &Editor::t, key: &str) -> String {
    let dir = scratch_dir("rays-t3-dump");
    Editor::crash_dump(e, &dir).unwrap_or_else(|m| panic!("crash_dump: {m}"));
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
    cur: Option<Editor::t>,
    pub count: i64,
}

impl Driver {
    pub fn new(e: Editor::t) -> Driver {
        Driver { cur: Some(e), count: 0 }
    }
    pub fn e(&self) -> &Editor::t {
        self.cur.as_ref().expect("editor")
    }
    pub fn set(&mut self, e: Editor::t) {
        self.cur = Some(e);
    }
    pub fn take(&mut self) -> Editor::t {
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
        self.cur = Some(Editor::update(e, &f));
    }
    pub fn close(&self) {
        Editor::close(self.e());
    }
    pub fn text(&self) -> String {
        text_of(self.e())
    }
}

impl Driver {
    /// `e := f !e` outside a frame (a `Reduce`, a selection).
    pub fn map(&mut self, f: impl FnOnce(Editor::t) -> Editor::t) {
        let e = self.take();
        self.cur = Some(f(e));
    }
}

/// The whole crash dump's editor.txt.
pub fn dump_text(e: &Editor::t) -> String {
    let dir = scratch_dir("rays-t5-dump");
    Editor::crash_dump(e, &dir).unwrap_or_else(|m| panic!("crash_dump: {m}"));
    let text = std::fs::read_to_string(format!("{dir}/editor.txt")).expect("editor.txt");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// The text of a specification/workspace/cases fixture (`Test_text.case`).
pub fn case_text(name: &str) -> String {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = if name == "bloom" || name == "variations" { root.join("specification/workspace/cases").join(format!("{name}.lisp")) } else { root.join("sketches").join(format!("ws_{name}.rays")) };
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// The editor with its frame counter and pointer, as the OCaml tests' `step` / `at` / `click` closures.
pub struct Env {
    pub d: Driver,
    pub mouse: (f64, f64),
}

impl Env {
    pub fn new(e: Editor::t) -> Env {
        Env { d: Driver::new(e), mouse: (450., 320.) }
    }
    pub fn e(&self) -> &Editor::t {
        self.d.e()
    }
    pub fn step(&mut self, events: Vec<Event>) {
        self.step_kb(&[], &[], events);
    }
    pub fn step_k(&mut self, keys: &[Key], events: Vec<Event>) {
        self.step_kb(&[], keys, events);
    }
    pub fn step_kb(&mut self, buttons: &[MouseButton], keys: &[Key], events: Vec<Event>) {
        let m = self.mouse;
        self.d.step_full(buttons, keys, m, events, 1);
    }
    pub fn at(&mut self, p: (f64, f64), events: Vec<Event>) {
        self.mouse = p;
        self.step(events);
    }
    pub fn at_k(&mut self, keys: &[Key], p: (f64, f64), events: Vec<Event>) {
        self.mouse = p;
        self.step_k(keys, events);
    }
    pub fn at_b(&mut self, buttons: &[MouseButton], p: (f64, f64), events: Vec<Event>) {
        self.mouse = p;
        self.step_kb(buttons, &[], events);
    }
    /// moved, then pressed and released (no trailing frame)
    pub fn click(&mut self, p: (f64, f64)) {
        self.at(p, vec![Event::Pointer_moved(p)]);
        self.at(p, vec![Event::Pointer_pressed(MouseButton::LeftButton, p), Event::Pointer_released(MouseButton::LeftButton, p)]);
    }
    /// `click` and one more frame
    pub fn click_s(&mut self, p: (f64, f64)) {
        self.click(p);
        self.step(vec![]);
    }
    pub fn settle(&mut self) {
        for _ in 0..4 {
            self.step(vec![]);
        }
    }
    pub fn ws(&self) -> wd::T {
        Editor::workspace(self.e())
    }
    pub fn src(&self) -> String {
        flow_syntax::lisp::print(&self.ws().source).unwrap().0
    }
    pub fn dump(&self) -> String {
        dump_text(self.e())
    }
    pub fn line(&self, key: &str) -> String {
        dump_line(self.e(), key)
    }
    pub fn undo(&self) -> Option<String> {
        Editor::undo_label(self.e())
    }
    pub fn graph_pane(&self) -> (i64, i64, i64, i64) {
        Editor::panes(self.e(), &frame_at((450., 320.), vec![], 0)).graph
    }
    pub fn close(&self) {
        self.d.close();
    }
}

pub fn check(condition: bool, message: &str) {
    if !condition {
        panic!("test_text_pane: {message}");
    }
}

pub const LINE_PITCH: f64 = 17.;

/// The centre of a tab (Selection, Graph, Document) in the header over a text pane's body.
pub fn tab_at(narrow: bool, right: i64, top: i64, index: usize) -> (f64, f64) {
    let offsets: [i64; 3] = if narrow { [66, 127, 168] } else { [31, 92, 150] };
    ((right - 178 + offsets[index]) as f64, (top - 12) as f64)
}

/// `Editor.create` with the text-pane tests' options (the dummy-driver font, an isolated presets folder).
pub fn start_env(text: &str) -> (Env, String) {
    let presets = scratch_dir("rays-text-presets");
    let ui_font = ui_font();
    let e = create(Editor::Create { domains: Some(1), presets: Some(presets.clone()), ui_font, ..options() }, &load(text));
    (Env::new(e), presets)
}

pub fn index_of(hay: &str, needle: &str) -> usize {
    hay.find(needle).unwrap_or_else(|| panic!("no {needle}"))
}

pub fn replace(text: &str, from: &str, by: &str) -> String {
    let i = text.find(from).unwrap_or_else(|| panic!("no {from}"));
    format!("{}{}{}", &text[..i], by, &text[i + from.len()..])
}

pub fn catalog() -> flow_check::catalog::Catalog {
    (*rays_editor::workspace_catalog(None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))).clone()
}

/// `Editor_prepare.empty`.
pub fn prepare_empty() -> rays_editor::cook::Prepare_fn {
    use rdk_core::geometry as geo;
    std::rc::Rc::new(|_, _| {
        let positions = rdk_core::packed::Float3::of_owned(Vec::new(), Vec::new(), Vec::new()).map_err(|e| format!("{e:?}"))?;
        let topology = rdk_core::topology::empty(0).map_err(|e| format!("{e:?}"))?;
        let geometry = geo::create(positions, topology, None, None, None).map_err(|e| format!("{e:?}"))?;
        let surface = rdk_rays::surface::of_geometry(None, &geometry)?;
        Ok(rays_editor::cook::prepared { geometry, surface })
    })
}

/// The pointer scripts assume the golden face's advance: DepartureMono unless RAYS_UI_FONT says otherwise.
pub fn ui_font() -> Option<String> {
    std::env::var("RAYS_UI_FONT").ok().or_else(|| {
        let p = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../assets/fonts/DepartureMono-Regular.otf");
        p.canonicalize().ok().map(|p| p.to_string_lossy().trim_start_matches("\\?\\").to_string())
    })
}
