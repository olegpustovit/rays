//! : the ws_bloom sketch through the editor; the Navigator lists the document
//! and opens a graph or node from a click, the host bars run commands, Make defn writes a function.
use std::rc::Rc;

use crate::common;
use super::support::*;
use editor_core::panels::Panel;
use editor_document::document;
use editor_document::workspace_doc as wd;
use input::{Event, Key, MouseButton};
use rays_shell::layout;
use rays_editor::navigator as n;
use rays_editor::editor;

pub const CASES: &[(&str, fn())] = &[("bloom_studio_run", run)];
pub const SKIPPED: &[(&str, &str)] = &[];

const WIDTH: i64 = 2400;
const HEIGHT: i64 = 1400;

fn bloom() -> String {
    let path = format!("{}/../../sketches/ws_bloom.rays", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}")).replace("(petals : int 12)", "(petals : int 3)").replace(":petals 7 :seed 2", ":petals 2 :seed 2")
}

fn bframe(buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>, count: i64) -> rays::frame::Frame {
    let mut f = frame_full(buttons, keys, (0., 0.), mouse, events, count);
    f.input.size = (WIDTH, HEIGHT);
    f
}

fn dump_line(e: &Ed, key: &str) -> String {
    with_dir(|dir| {
        editor::crash_dump(e, dir).unwrap();
        let text = std::fs::read_to_string(format!("{dir}/editor.txt")).unwrap();
        let prefix = format!("{key}: ");
        text.lines().find(|l| l.starts_with(&prefix)).map(|l| l[prefix.len()..].to_owned()).unwrap_or_else(|| panic!("no {key} in the dump"))
    })
}

fn source(e: &Ed) -> String {
    flow_syntax::lisp::print(&editor::workspace(e).source).unwrap().0
}

fn run() {
    if !common::gpu() {
        return;
    }
    let ws = workspace(&bloom());
    let e = create(&ws, None, None, false);
    let mut r = Bloom { e, count: 0 };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| body(&mut r)));
    editor::close(&r.e);
    if let Err(p) = result {
        std::panic::resume_unwind(p);
    }
}

struct Bloom {
    e: Ed,
    count: i64,
}

impl Bloom {
    fn step(&mut self, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>) {
        self.count += 1;
        self.e = editor::update(self.e.clone(), &bframe(buttons, keys, mouse, events, self.count));
    }
    fn plain(&mut self, events: Vec<Event>) {
        self.step(&[], &[], (450., 300.), events);
    }
    fn click(&mut self, (x, y): (f64, f64)) {
        self.step(&[], &[], (x, y), vec![Event::PointerMoved((x, y))]);
        self.step(&[MouseButton::LeftButton], &[], (x, y), vec![Event::PointerPressed(MouseButton::LeftButton, (x, y))]);
        self.step(&[], &[], (x, y), vec![Event::PointerReleased(MouseButton::LeftButton, (x, y))]);
        self.plain(vec![]);
    }
}

fn body(r: &mut Bloom) {
    let catalog = catalog();
    for _ in 0..12 {
        r.plain(vec![]);
    }
    assert!(dump_line(&r.e, "projection") == "graph" && dump_line(&r.e, "pane graph") == "flower", "the sketch opens on the network of flower: {} / {}", dump_line(&r.e, "projection"), dump_line(&r.e, "pane graph"));
    let ws = editor::workspace(&r.e);
    let doc: document::T = build(&ws);
    let shell = doc.shell.clone().expect("shell");
    let geometry = |_r: &Bloom| layout::geometry(None, Some(&[Panel::Timeline]), &shell.tree, &bframe(&[], &[], (0., 0.), vec![], 0));
    let _ = r;
    let outline = |r: &Bloom| layout::find(&geometry(r), &Panel::Outline).expect("outline").body;
    let graph_leaf = |r: &Bloom| layout::find(&geometry(r), &Panel::Graph).expect("graph leaf");
    let scope_of = |name: &str| Some(Rc::new(flow_graph::projection::of_graph(&catalog, &ws.checked, name).unwrap()));
    let params = |r: &Bloom, active: &str, scope: Option<Rc<flow_graph::projection::Scope>>| n::Params {
        workspace: ws.checked.clone(),
        active: Some(active.to_owned()),
        scope,
        records: None,
        probes: Rc::new(|_| 0),
        selected: vec![],
        chips: vec![],
        objects: editor::scene_document(&r.e)
            .iter()
            .map(|_| n::Obj { depth: 0, letter: String::new(), name: String::new(), detail: String::new(), visible: None, render: None, lead: false, inert: false, chosen: false, home: None })
            .collect(),
        root_detail: String::new(),
        layouts: shell.switch.as_ref().map(|sw| (editor_core::panels::labels(&sw.layouts), sw.active)),
        notes: vec![],
    };
    let scope = scope_of("flower");
    let state = n::open_graph("flower", &n::initial());
    let lines: Vec<String> = n::rows(true, &state, &params(r, "flower", scope.clone())).iter().map(n::describe).collect();
    let listed = |t: &str| lines.iter().any(|l| l.contains(t));
    assert!(listed("input petals = 3") && listed("input seed = 7"), "the inputs of flower: {}", lines.join(" | "));
    assert!(listed("> flower · ") && listed("1 loop") && listed("R root (scene)"), "the composition lists every graph, the open one marked");
    assert!(listed("for ring"), "the open graph's loop is a tree row");
    assert!(listed("half · defn") && listed("×2") && listed("petal · defn") && listed("×1") && listed("λ twice · 1 use"), "the reusable functions and macros count their uses");
    assert!(listed("read by scene"), "the data flow names who reads flower");
    let row_centre = |r: &Bloom, text: &str| {
        let active = dump_line(&r.e, "pane graph");
        let rects = n::row_rects(&state, &params(r, &active, scope_of(&active)), outline(r));
        let (_, (x, y, w, h)) = rects.into_iter().find(|(row, _)| n::describe(row).contains(text)).unwrap_or_else(|| panic!("no Navigator row {text}"));
        (x + 20. + w / 8., y + h / 2.)
    };
    {
        let active = dump_line(&r.e, "pane graph");
        let rects = n::row_rects(&state, &params(r, &active, scope_of(&active)), outline(r));
        let (_, (x, y, _, h)) = rects.into_iter().find(|(row, _)| n::describe(row).contains("flower · ")).expect("no flower row");
        r.click((x + 5., y + h / 2.));
    }
    let p = row_centre(r, "R root (scene)");
    r.click(p);
    let p = row_centre(r, "flower · ");
    r.click(p);
    let p = row_centre(r, "half · defn");
    r.click(p);
    assert_eq!(dump_line(&r.e, "pane graph"), "def:half", "a function opens in the pane");
    assert_eq!(dump_line(&r.e, "projection"), "graph", "the function shows as a network");
    let p = row_centre(r, "R root (scene)");
    r.click(p);
    assert_eq!(dump_line(&r.e, "pane graph"), "scene", "a graph row opens that graph");
    let p = row_centre(r, "flower · ");
    r.click(p);
    assert_eq!(dump_line(&r.e, "pane graph"), "flower", "and back to flower");
    let p = row_centre(r, "wobble");
    r.click(p);
    assert_eq!(dump_line(&r.e, "scope selected"), "flower/ring/wobble", "a node row selects it in the pane");
    let p = row_centre(r, "heart");
    r.click(p);
    let (gx, gy, gw, _) = graph_leaf(r).body;
    let p = ((gx + gw - 40) as f64, (gy + 40) as f64);
    r.step(&[], &[], p, vec![Event::PointerMoved(p)]);
    r.step(&[MouseButton::LeftButton], &[Key::Shift], p, vec![Event::PointerPressed(MouseButton::LeftButton, p)]);
    r.step(&[], &[Key::Shift], p, vec![Event::PointerReleased(MouseButton::LeftButton, p)]);
    r.step(&[], &[], p, vec![]);
    assert_eq!(dump_line(&r.e, "scope selected"), "flower/heart", "heart stays selected");
    let before = editor::workspace(&r.e);
    let label = editor::undo_label(&r.e);
    r.step(&[], &[], p, vec![Event::KeyPressed(Key::KeyChar('v'))]);
    r.step(&[], &[], p, vec![]);
    r.plain(vec![]);
    assert!(same_ws(&editor::workspace(&r.e), &before) && editor::undo_label(&r.e) == label, "v changed the source or history");
    assert_eq!(sop::node::label(&editor::displayed_node(&r.e)), "uv_sphere", "the viewport shows the viewed node at the scene level");
    // an input's slider writes the graph input's default
    let rects = n::row_rects(&state, &params(r, "flower", scope.clone()), outline(r));
    let (_, (x, y, w, h)) = rects.into_iter().find(|(row, _)| n::describe(row).contains("input petals")).expect("no petals slider");
    let (slider_y, slider_x, slider_w) = (y + h / 2., x + 76., w - 86.);
    let from = (slider_x + slider_w * 0.3, slider_y);
    let to = (slider_x + slider_w * 0.9, slider_y);
    r.step(&[], &[], from, vec![Event::PointerMoved(from)]);
    r.step(&[MouseButton::LeftButton], &[], from, vec![Event::PointerPressed(MouseButton::LeftButton, from)]);
    r.step(&[MouseButton::LeftButton], &[], to, vec![Event::PointerMoved(to)]);
    r.step(&[], &[], to, vec![Event::PointerReleased(MouseButton::LeftButton, to)]);
    r.plain(vec![]);
    assert!(!source(&r.e).contains("(petals : int 3)") && source(&r.e).contains("(petals : int "), "the slider changed the input's default: {}", source(&r.e));
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Input default"), "named in history");
    assert_eq!(dump_line(&r.e, "edit error"), "-", "and checks");
    let name_at = (slider_x - 60., slider_y);
    r.click(name_at);
    r.click(name_at);
    r.plain(vec![Event::TextInput("7".into())]);
    r.plain(vec![Event::KeyPressed(Key::Enter)]);
    r.plain(vec![]);
    assert!(source(&r.e).contains("(petals : int 7)"), "a typed default is written as an int: {}", source(&r.e));
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Input default"), "the typed default is a history entry");
    let matches: Vec<String> = n::rows(true, &n::with_query("wob", &n::initial()), &params(r, "flower", scope.clone())).iter().map(n::describe).collect();
    assert!(matches.iter().any(|l| l.contains("wobble")) && !matches.iter().any(|l| l.contains("scene · scene")), "a search shows the matching bindings");
    // the toolbar: Defn makes a reusable function from the selection
    let header = graph_leaf(r).header;
    let mut ui = pxui::ui::Ui::create(input::Platform::Apple, Some(11), None, &rays_shell::host::BACKEND);
    let route = dump_line(&r.e, "route").split('>').map(|s| s.trim().to_owned()).collect::<Vec<_>>().join(" / ");
    let from = rays_shell::chrome::tools_start(&mut ui, true, false, false, &format!("Graph\t{route} / sop"));
    let tool = rays_editor::bars::tool_rect(false, &mut ui, header, from, rays_editor::bars::Tool::Defn).unwrap_or_else(|| panic!("the toolbar is cut off (header {header:?}, tools from {from:.0})"));
    let before = source(&r.e);
    let p = row_centre(r, "heart");
    r.click(p);
    assert_eq!(dump_line(&r.e, "scope selected"), "flower/heart", "the heart is selected");
    r.click((tool.0 + tool.2 / 2., tool.1 + tool.3 / 2.));
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Make reusable function"), "the defn tool made a function: {:?} / {}", editor::undo_label(&r.e), dump_line(&r.e, "edit error"));
    assert!(source(&r.e).contains("(defn heart_2 :context sop []"), "a defn with its own name: {}", source(&r.e));
    assert!(source(&r.e) != before, "the text changed");
    ui.destroy();
    let _ = wd::to_text;
}
