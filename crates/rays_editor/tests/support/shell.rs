//! Ports of the helpers at the top of  / .
#![allow(dead_code)]
use editor_document::document;
use editor_document::kinds;
use editor_document::lowering::{self, Options};
use editor_document::workspace_doc::{self, TextOptions};
use flow_check::catalog::Catalog;
use input::{Event, Key, MouseButton};
use rays::frame;
use rays_editor::editor;

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
pub fn editor(text: &str) -> editor::Editor {
    editor_with(text, editor::Create::default())
}
/// A preferences file of the test's own with the guide strip off (the node budgets of the frame-cost
/// cases count the plain status strip): a case does not read this machine's `~/.rays/preferences.rays`,
/// whose saved `guide` swaps the strip (+2 nodes a frame with it on).
fn no_preferences() -> Option<String> {
    let file = std::env::temp_dir().join("rays-editor-test-preferences.rays");
    // (cases run in parallel processes: renamed into place, never read half written)
    let part = file.with_extension(format!("{}", std::process::id()));
    let _ = std::fs::write(&part, "(settings :guide false)").and_then(|_| std::fs::rename(&part, &file));
    Some(file.to_string_lossy().to_string())
}
pub fn editor_with(text: &str, options: editor::Create) -> editor::Editor {
    let options = editor::Create { await_: Some(true), ui_text_size: Some(11), platform: Some(input::Platform::Apple), domains: options.domains.or(Some(1)), preferences: options.preferences.or_else(no_preferences), ..options };
    match editor::create(options, &of_text(text), None, editor::prepare_fn(), editor::draw_fn()) {
        Ok(e) => e,
        Err(m) => fail(&m),
    }
}
pub fn source(e: &editor::Editor) -> String {
    workspace_doc::print(&editor::workspace(e)).unwrap().0
}
pub fn dump_line(e: &editor::Editor, key: &str) -> String {
    // the dump's editor.txt, without writing the document, preset and memory report each time
    let text = editor::private::state_text(e);
    let prefix = format!("{key}: ");
    match text.lines().find(|l| l.starts_with(&prefix)) {
        Some(l) => l[prefix.len()..].to_string(),
        None => fail(&format!("no {key} in the dump")),
    }
}

/// The stepping closure every section builds: a counter, an editor, `step`.
pub struct Run {
    pub e: editor::Editor,
    pub count: i64,
}
impl Run {
    pub fn new(e: editor::Editor) -> Run {
        Run { e, count: 0 }
    }
    pub fn step_full(&mut self, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>) {
        self.count += 1;
        let f = frame_full(buttons, keys, (0., 0.), mouse, events, self.count);
        self.e = editor::update(self.e.clone(), &f);
    }
    pub fn step(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(&[], &[], mouse, events)
    }
    pub fn stepb(&mut self, buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(buttons, &[], mouse, events)
    }
    pub fn click_button(&mut self, button: MouseButton, p: (f64, f64)) {
        self.step(p, vec![Event::PointerMoved(p)]);
        self.stepb(&[button], p, vec![Event::PointerPressed(button, p)]);
        self.step(p, vec![Event::PointerReleased(button, p)]);
    }
    pub fn click(&mut self, p: (f64, f64)) {
        self.click_button(MouseButton::LeftButton, p)
    }
    pub fn panes(&self) -> rays_shell::layout::Panes {
        editor::panes(&self.e, &idle_frame(0))
    }
    pub fn undo_label(&self) -> Option<String> {
        editor::undo_label(&self.e)
    }
    pub fn source(&self) -> String {
        source(&self.e)
    }
    pub fn dump(&self, key: &str) -> String {
        dump_line(&self.e, key)
    }
    pub fn close(&self) {
        editor::close(&self.e)
    }
}
pub fn key(k: Key) -> Event {
    Event::KeyPressed(k)
}
pub fn ch(c: char) -> Event {
    Event::KeyPressed(Key::KeyChar(c))
}
pub fn text_input(s: &str) -> Event {
    Event::TextInput(s.to_string())
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
