//! Ports of the helpers at the top of test_workspace_shell.ml / test_editor_input.ml.
#![allow(dead_code)]
use editor_document::document;
use editor_document::kinds;
use editor_document::lowering::{self, Options};
use editor_document::workspace_doc::{self, TextOptions};
use flow_check::catalog::Catalog;
use input::{Event, Key, MouseButton};
use rays::frame;
use rays_editor::Editor;

pub fn fail(message: &str) -> ! {
    panic!("test_workspace_shell: {message}")
}
pub fn check(condition: bool, message: &str) {
    if !condition {
        fail(message)
    }
}
pub fn has(text: &str, sub: &str) -> bool {
    text.contains(sub)
}
pub fn factories() -> Vec<sop::edit_graph::Factory> {
    sop::nodes::factories()
}
pub fn catalog() -> Catalog {
    thread_local! { static C: Catalog = kinds::catalog(flow_sop::manifest::VERSION, &factories()).expect("catalog"); }
    C.with(|c| c.clone())
}
pub fn show(ds: &[rays_diagnostic::Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")
}
pub fn read_case(name: &str) -> String {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = if matches!(name, "bloom" | "variations") {
        root.join("specification/workspace/cases").join(format!("{name}.lisp"))
    } else {
        root.join("sketches").join(format!("ws_{name}.rays"))
    };
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}
pub fn case(name: &str) -> String {
    read_case(name)
}
pub fn sketch(name: &str) -> String {
    let path = format!("{}/../../sketches/{name}.rays", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"))
}
pub fn of_text(text: &str) -> workspace_doc::T {
    match workspace_doc::of_text(&TextOptions::default(), &catalog(), text) {
        Ok(d) => d,
        Err(ds) => fail(&show(&ds)),
    }
}
pub fn build(previous: Option<&document::T>, ws: &workspace_doc::T) -> Result<document::T, rays_diagnostic::Diagnostic> {
    lowering::of_workspace(&Options { factories: Some(factories()), previous: previous.cloned(), ..Default::default() }, ws)
}
pub fn build_ok(ws: &workspace_doc::T) -> document::T {
    match build(None, ws) {
        Ok(d) => d,
        Err(d) => fail(&rays_diagnostic::to_string(&d)),
    }
}
pub fn replace(text: &str, from: &str, by: &str) -> String {
    match text.find(from) {
        Some(i) => format!("{}{}{}", &text[..i], by, &text[i + from.len()..]),
        None => fail(&format!("no {from}")),
    }
}
pub fn shell_of(doc: &document::T) -> document::Shell {
    doc.shell.as_deref().cloned().unwrap_or_else(|| fail("no editor graph"))
}
/// The variations workspace with its editor graph replaced.
pub fn with_editor(body: &str) -> String {
    let text = case("variations");
    let start = text.find("(graph editor :context").expect("editor graph");
    format!("{}(graph editor :context editor\n{body}))\n", &text[..start])
}
pub fn expect_error(name: &str, text: &str, needle: &str) {
    match workspace_doc::of_text(&TextOptions::default(), &catalog(), text) {
        Err(ds) => check(has(&show(&ds), needle), &format!("{name}: {}", show(&ds))),
        Ok(ws) => match build(None, &ws) {
            Err(d) => check(has(&rays_diagnostic::to_string(&d), needle), &format!("{name}: {}", rays_diagnostic::to_string(&d))),
            Ok(_) => fail(&format!("{name} was accepted")),
        },
    }
}

// ---- Test_editor_input.frame ----

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
pub fn frame(buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::Frame {
    frame_full(buttons, &[], (0., 0.), mouse, events, count)
}
pub fn idle_frame(count: i64) -> frame::Frame {
    frame(&[], (0., 0.), vec![], count)
}

// ---- editors ----

/// Every editor here awaits the cook its frame submits: what a frame shows is settled.
pub fn editor(text: &str) -> Editor::t {
    editor_with(text, Editor::Create::default())
}
pub fn editor_with(text: &str, options: Editor::Create) -> Editor::t {
    let options = Editor::Create { await_: Some(true), ui_text_size: Some(11), platform: Some(input::Platform::Apple), domains: options.domains.or(Some(1)), ..options };
    match Editor::create(options, &of_text(text), None, Editor::prepare_fn(), Editor::draw_fn()) {
        Ok(e) => e,
        Err(m) => fail(&m),
    }
}
pub fn source(e: &Editor::t) -> String {
    workspace_doc::print(&Editor::workspace(e)).unwrap().0
}
pub fn dump_line(e: &Editor::t, key: &str) -> String {
    let dir = std::env::temp_dir().join(format!("rays-shell-dump-{}-{}", std::process::id(), std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()));
    std::fs::create_dir_all(&dir).unwrap();
    Editor::crash_dump(e, dir.to_str().unwrap()).expect("crash dump");
    let text = std::fs::read_to_string(dir.join("editor.txt")).expect("editor.txt");
    let _ = std::fs::remove_dir_all(&dir);
    let prefix = format!("{key}: ");
    match text.lines().find(|l| l.starts_with(&prefix)) {
        Some(l) => l[prefix.len()..].to_string(),
        None => fail(&format!("no {key} in the dump")),
    }
}

/// The stepping closure every section builds: a counter, an editor, `step`.
pub struct Run {
    pub e: Editor::t,
    pub count: i64,
}
impl Run {
    pub fn new(e: Editor::t) -> Run {
        Run { e, count: 0 }
    }
    pub fn step_full(&mut self, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>) {
        self.count += 1;
        let f = frame_full(buttons, keys, (0., 0.), mouse, events, self.count);
        self.e = Editor::update(self.e.clone(), &f);
    }
    pub fn step(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(&[], &[], mouse, events)
    }
    pub fn stepb(&mut self, buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(buttons, &[], mouse, events)
    }
    pub fn click_button(&mut self, button: MouseButton, p: (f64, f64)) {
        self.step(p, vec![Event::Pointer_moved(p)]);
        self.stepb(&[button], p, vec![Event::Pointer_pressed(button, p)]);
        self.step(p, vec![Event::Pointer_released(button, p)]);
    }
    pub fn click(&mut self, p: (f64, f64)) {
        self.click_button(MouseButton::LeftButton, p)
    }
    pub fn panes(&self) -> pxui_shell::layout::Panes {
        Editor::panes(&self.e, &idle_frame(0))
    }
    pub fn undo_label(&self) -> Option<String> {
        Editor::undo_label(&self.e)
    }
    pub fn source(&self) -> String {
        source(&self.e)
    }
    pub fn dump(&self, key: &str) -> String {
        dump_line(&self.e, key)
    }
    pub fn close(&self) {
        Editor::close(&self.e)
    }
}
pub fn key(k: Key) -> Event {
    Event::Key_pressed(k)
}
pub fn ch(c: char) -> Event {
    Event::Key_pressed(Key::KeyChar(c))
}
pub fn text_input(s: &str) -> Event {
    Event::Text_input(s.to_string())
}
pub fn fpt(a: i64, b: i64) -> (f64, f64) {
    (a as f64, b as f64)
}
pub fn gpu() -> bool {
    crate::common::gpu()
}
pub fn studio_text() -> String {
    sketch("ws_layout")
}
