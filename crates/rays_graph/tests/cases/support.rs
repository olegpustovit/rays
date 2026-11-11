//! Shared graph-pane fixture, frame and gesture builders.
#![allow(dead_code)]
use flow_syntax::symbol::Symbol;
use flow_check::catalog::Catalog;
use flow_check::workspace::Workspace;
use flow_graph::probe::{self, Probe};
use flow_graph::projection as p;
use input::{Event, Key, MouseButton, Platform};
use pxui::ui::*;
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::{self, Change, Pane, ScopeOpts};
use rays::frame as rframe;
use std::cell::RefCell;
use std::rc::Rc;

pub fn fail(message: &str) -> ! {
    panic!("{message}");
}

pub fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

pub fn pointer(p: (i64, i64)) -> (f64, f64) {
    (p.0 as f64, p.1 as f64)
}
pub fn mouse_press(button: MouseButton, point: (i64, i64)) -> Event {
    Event::PointerPressed(button, pointer(point))
}
pub fn mouse_release(button: MouseButton, point: (i64, i64)) -> Event {
    Event::PointerReleased(button, pointer(point))
}
pub fn mouse_move(point: (i64, i64)) -> Event {
    Event::PointerMoved(pointer(point))
}

/// `frame ~mouse ~keys ~events ()`: a 1000 x 700 frame.
pub fn frame(mouse: (i64, i64), keys: Vec<Key>, events: Vec<Event>) -> rframe::Frame {
    let mut input = input::at_time(0.0);
    input.frame = 0;
    input.tick = 0;
    input.size = (1000, 700);
    input.pointer = (mouse.0 as f64, mouse.1 as f64);
    input.keys = keys;
    input.events = events;
    rframe::Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// `frame ()`
pub fn frame0() -> rframe::Frame {
    frame((0, 0), Vec::new(), Vec::new())
}

/// `frame ~mouse ~events ()`
pub fn frame_ev(mouse: (i64, i64), events: Vec<Event>) -> rframe::Frame {
    frame(mouse, Vec::new(), events)
}

/// A UI over the default kit face at the 11-point kit size, as `Pxui.Ui.create ~platform:Apple ~font_size:11`.
pub fn new_ui() -> Ui {
    rays_shell::host::create(Platform::Apple, Some(11), None)
}

pub const CASES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../specification/workspace/cases");

pub fn scope_catalog() -> Catalog {
    thread_local! {
        static CATALOG: Catalog = editor_document::kinds::catalog(1, &sop::nodes::factories()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    }
    CATALOG.with(|c| c.clone())
}

/// `Workspace_doc.of_text scope_catalog text |> Result.get_ok` then `.checked`.
pub fn checked(text: &str) -> Workspace {
    let forms = flow_syntax::syntax::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    match flow_check::workspace::check(flow_sop::operators::all(), false, false, None, &scope_catalog(), &forms) {
        (Some(w), _) => w,
        (None, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

pub fn load_workspace(name: &str) -> Workspace {
    // Read the shared sketch sources directly.
    let path = if matches!(name, "bloom" | "variations") {
        format!("{CASES_DIR}/{name}.lisp")
    } else {
        format!("{}/../../sketches/ws_{name}.rays", env!("CARGO_MANIFEST_DIR"))
    };
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{name}: {e}"));
    checked(&text)
}

/// Run `f` over the pane inside one `Ui::frame` (the frame's closure returns nothing).
pub fn in_frame(ui: &mut Ui, fr: &rframe::Frame, mut view: Pane) -> (Pane, Vec<Change>) {
    let mut changes = Vec::new();
    rays_shell::host::build(ui, fr, &mut |ui: &mut Ui| {
        scope::update(&[], &mut view, ui, fr, &mut changes);
    });
    (view, changes)
}

/// `scope_step`: the layout settles in a frame without events, then the frame runs.
pub fn scope_step(ui: &mut Ui, view: Pane, fr: &rframe::Frame) -> (Pane, Vec<Change>) {
    let mut settle = fr.clone();
    settle.input.events = Vec::new();
    let (view, _) = in_frame(ui, &settle, view);
    in_frame(ui, fr, view)
}

pub fn recorded(workspace: &Workspace) -> Rc<RefCell<Probe>> {
    let eval = flow_eval::r#static(workspace, true, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    Rc::new(RefCell::new(probe::make(&eval, probe::Options::default())))
}

/// `scope_view ?probe ?at ?level ?collapsed workspace graph`: the pane over the graph, with records.
pub fn scope_view(opts: ScopeOpts, workspace: &Workspace, graph: &str) -> (Pane, Rc<p::Scope>) {
    let sc = Rc::new(p::of_graph(&scope_catalog(), workspace, graph).unwrap_or_else(|e| panic!("{e}")));
    let view = scope::create(Some(1000), Some(700));
    let view = { let mut r8_view = view; scope::with_scope(opts, graph, sc.clone(), &mut r8_view); r8_view };
    let view = { let mut r8_view = view; scope::with_records(recorded(workspace), &mut r8_view); r8_view };
    (view, sc)
}

pub fn click(point: (i64, i64)) -> Vec<Event> {
    vec![mouse_press(MouseButton::LeftButton, point), mouse_release(MouseButton::LeftButton, point)]
}

pub fn scope_click(ui: &mut Ui, view: Pane, point: (i64, i64)) -> (Pane, Vec<Change>) {
    let mut events = vec![mouse_move(point)];
    events.extend(click(point));
    scope_step(ui, view, &frame_ev(point, events))
}

pub fn rect_center(r: (f64, f64, f64, f64)) -> (i64, i64) {
    ((r.0 + r.2 / 2.0) as i64, (r.1 + r.3 / 2.0) as i64)
}

/// The instances the pane drew (`scope_paint`).
pub fn scope_paint(ui: &mut Ui, view: Pane) -> i64 {
    let _ = scope_step(ui, view, &frame0());
    ui.layers().map(|l| l.instances.len() as i64).sum()
}

/// `[n]` nodes in one scope: a chain with a long wire back at every seventh node.
pub fn big_text(n: i64) -> String {
    let mut b = String::from("(workspace big (graph g :context sop (let* [n0 (sop/box)");
    for i in 1..n {
        b.push_str(&format!(" n{} (sop/transform n{} :translate [{} 0 0])", i, if i % 7 == 0 { i / 2 } else { i - 1 }, i));
    }
    b.push_str(&format!("] n{})))", n - 1));
    b
}

pub fn big_scope(n: i64) -> p::Scope {
    let ws = checked(&big_text(n));
    p::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}"))
}

/// A large window keeps zoom at one so every graph field is built.
pub mod g {
    use super::*;

    pub fn syntax(op: flow_graph::flow_edit::Op) -> Change {
        Change::SyntaxEdit(op)
    }

    pub fn frame(mouse: (i64, i64), keys: Vec<Key>, events: Vec<Event>) -> rframe::Frame {
        let mut fr = super::frame(mouse, keys, events);
        fr.input.size = (3000, 2000);
        fr
    }

    pub fn frame0() -> rframe::Frame {
        frame((0, 0), Vec::new(), Vec::new())
    }

    pub fn frame_ev(mouse: (i64, i64), events: Vec<Event>) -> rframe::Frame {
        frame(mouse, Vec::new(), events)
    }

    pub fn scope_click(ui: &mut Ui, view: Pane, point: (i64, i64)) -> (Pane, Vec<Change>) {
        let mut events = vec![mouse_move(point)];
        events.extend(super::click(point));
        scope_step(ui, view, &frame_ev(point, events))
    }

    pub fn center_of(view: &Pane, path: &[Symbol]) -> (f64, f64, f64, f64) {
        scope::inspect::box_of(view, path).expect("box")
    }

    pub fn settled(ui: &mut Ui, view: Pane) -> Pane {
        scope_step(ui, view, &frame0()).0
    }

    pub fn scope_view(workspace: &Workspace, graph: &str) -> (Pane, Rc<p::Scope>) {
        let sc = Rc::new(p::of_graph(&scope_catalog(), workspace, graph).unwrap_or_else(|e| panic!("{e}")));
        let view = scope::create(Some(3000), Some(2000));
        let view = { let mut r8_view = view; scope::with_scope(ScopeOpts::default(), graph, sc.clone(), &mut r8_view); r8_view };
        let view = { let mut r8_view = view; scope::with_records(recorded(workspace), &mut r8_view); r8_view };
        (view, sc)
    }

    pub fn heart() -> flow_check::workspace::Path {
        vec!["flower".into(), "heart".into()]
    }

    pub fn bloom_view(ui: &mut Ui) -> Pane {
        settled(ui, scope_view(&load_workspace("bloom"), "flower").0)
    }

    pub fn double_click(ui: &mut Ui, view: Pane, point: (i64, i64)) -> (Pane, Vec<Change>) {
        let (view, _) = scope_click(ui, view, point);
        scope_click(ui, view, point)
    }

    pub fn type_text(ui: &mut Ui, view: Pane, text: &str) -> (Pane, Vec<Change>) {
        scope_step(ui, view, &frame_ev((0, 0), vec![Event::TextInput(text.to_owned()), Event::KeyPressed(Key::Enter)]))
    }
}

/// A path from its segments.
/// What a test's host has saved: positions, levels, probes, frames, folds and imports.
#[derive(Default)]
pub struct TestSaved {
    pub at: Vec<(flow_check::workspace::Path, (f64, f64))>,
    pub level: Vec<(flow_check::workspace::Path, (p::Level, bool))>,
    pub probe: Vec<(flow_check::workspace::Path, i64)>,
    pub frames: Vec<(flow_check::workspace::Path, Vec<scope_state::Frame3>)>,
    pub collapsed: Vec<flow_check::workspace::Path>,
    pub imported: Option<String>,
}

fn find<'a, V>(list: &'a [(flow_check::workspace::Path, V)], path: &[Symbol]) -> Option<&'a V> {
    list.iter().find(|(k, _)| k.as_slice() == path).map(|(_, v)| v)
}

impl scope_state::Saved for TestSaved {
    fn at(&self, path: &[Symbol]) -> Option<(f64, f64)> {
        find(&self.at, path).copied()
    }
    fn level(&self, path: &[Symbol]) -> Option<(p::Level, bool)> {
        find(&self.level, path).copied()
    }
    fn probe(&self, path: &[Symbol]) -> i64 {
        find(&self.probe, path).copied().unwrap_or(0)
    }
    fn frames(&self, path: &[Symbol]) -> Vec<scope_state::Frame3> {
        find(&self.frames, path).cloned().unwrap_or_default()
    }
    fn collapsed(&self, path: &[Symbol]) -> bool {
        self.collapsed.iter().any(|k| k.as_slice() == path)
    }
    fn imported(&self, _: &[Symbol]) -> Option<String> {
        self.imported.clone()
    }
}

/// A host whose saved positions are a function.
pub struct AtFn(pub Box<dyn Fn(&[Symbol]) -> Option<(f64, f64)>>);
impl scope_state::Saved for AtFn {
    fn at(&self, path: &[Symbol]) -> Option<(f64, f64)> {
        (self.0)(path)
    }
}
pub fn at_fn(f: impl Fn(&[Symbol]) -> Option<(f64, f64)> + 'static) -> ScopeOpts {
    ScopeOpts { saved: Rc::new(AtFn(Box::new(f))), ..Default::default() }
}

pub fn saved_opts(saved: TestSaved) -> ScopeOpts {
    ScopeOpts { saved: Rc::new(saved), ..Default::default() }
}

pub fn path(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|s| (*s).into()).collect()
}
