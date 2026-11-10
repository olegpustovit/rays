//! carry_tests and viewport_carry_tests of : a material, a SOP graph, a scene
//! graph and a camera carried by pointer and by keys onto nodes, panels and surfaces.
use super::t5c::*;
use editor_core::keymap::Trigger;
use editor_core::panels::{Axis, Panel, Size, Panels as Tree};
use input::{Event, Key, MouseButton};
use rays_shell::layout as L;
use rays_editor::navigator as N;
use rays_editor::editor;
use std::rc::Rc;

const LB: MouseButton = MouseButton::LeftButton;

fn fail(message: &str) -> ! {
    panic!("test_materials: {message}")
}

fn chk(c: bool, message: &str) {
    if !c {
        fail(message);
    }
}

/// `Test_materials.frame`-driven editor: `step ?advance ?buttons ?keys ?mouse events`, the mouse not sticky.
struct Ed {
    d: Driver,
}

impl Ed {
    fn new(e: editor::Editor) -> Ed {
        Ed { d: Driver::new(e) }
    }
    fn e(&self) -> &editor::Editor {
        self.d.e()
    }
    fn step_full(&mut self, advance: i64, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>) {
        self.d.step_full(buttons, keys, mouse, events, advance);
    }
    fn step(&mut self, events: Vec<Event>) {
        self.step_full(1, &[], &[], (450., 300.), events);
    }
    fn step_k(&mut self, keys: &[Key], events: Vec<Event>) {
        self.step_full(1, &[], keys, (450., 300.), events);
    }
    fn step_m(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(1, &[], &[], mouse, events);
    }
    fn step_b(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_full(1, &[LB], &[], mouse, events);
    }
    fn pane(&self) -> String {
        dump_line(self.e(), "pane graph")
    }
    fn line(&self) -> String {
        editor::carry_line(self.e()).unwrap_or_else(|| "-".into())
    }
    fn text(&self) -> String {
        text_of(self.e())
    }
    fn ws(&self) -> editor_document::workspace_doc::T {
        editor::workspace(self.e())
    }
    fn undo(&self) -> Option<String> {
        editor::undo_label(self.e())
    }
    fn jump(&mut self, name: &str) {
        self.step(vec![key(Key::KeyChar('/')), ch('j')]);
        self.step(vec![Event::TextInput(name.into()), key(Key::Enter)]);
        self.step(vec![]);
    }
}

const CARRY_SOURCE: &str = r##"(workspace carrying
  (graph cobalt :context material
    (material/standard :name "cobalt blue" :color "#2670f5" :roughness 0.3))
  (graph spare :context material (material/standard :name "spare" :color "#ff0000"))
  (graph shards :context sop
    (let* [m (sop/material (sop/box) :material (ref spare))] m))
  (graph plinth :context sop (sop/box))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")] (scene/merge obj))))"##;

const EDITOR: &str = r#"
  (graph editor :context editor
    (ui/workspace (ui/split_at "horizontal" 0.22 (ui/outline)
      (ui/split_at "horizontal" 0.45 (ui/viewport (ref scene))
        (ui/split_at "horizontal" 0.62 (ui/graph) (ui/inspector))))))"#;

fn with_editor_graph(source: &str) -> String {
    format!("{}{})", &source[..source.len() - 1], EDITOR)
}

fn make(source: &str, carry_budget: f64) -> editor::Editor {
    let ws = load(&with_editor_graph(source));
    editor::create(editor::Create { carry_budget: Some(carry_budget), ..options() }, &ws, None, prepare_empty(), editor::draw_fn()).unwrap_or_else(|m| fail(&m))
}

fn leaf(p: Panel) -> Box<Tree> {
    Box::new(Tree::Leaf(p))
}

fn split(ratio: f64, a: Box<Tree>, b: Box<Tree>) -> Box<Tree> {
    Box::new(Tree::Split { axis: Axis::H, size: Size::Ratio(ratio), a, b })
}

pub fn carry_tests() {
    let layout = split(0.22, leaf(Panel::Outline), split(0.45, leaf(Panel::View("main".into())), split(0.62, leaf(Panel::Graph), leaf(Panel::Inspector))));
    let mut e = Ed::new(make(CARRY_SOURCE, f64::INFINITY));
    e.step(vec![]);
    // y is a plain key nothing else uses
    let ys = rays_editor::editor_commands::keymap().iter().filter(|c| matches!(&c.trigger, Some(Trigger::Chord(Key::KeyChar('y'), m)) if m.is_empty())).count();
    chk(ys == 1, "y picks up and no other command takes it");
    let catalog = catalog();
    // the outline row of a graph, in screen points, as the Navigator draws it now
    let row_of = |e: &Ed, graph: &str| -> (f64, f64) {
        let doc = e.ws().checked.clone();
        let chips = match flow_eval::r#static(&doc, false, None) {
            Ok(ev) => N::chips(&ev),
            Err(d) => fail(&rays_diagnostic::to_string(&d)),
        };
        let active = e.pane();
        let scope = flow_graph::projection::of_graph(&catalog, &doc, &active).ok().map(Rc::new);
        let objects = (0..kg_len(e.e()))
            .map(|_| N::Obj { depth: 0, letter: String::new(), name: String::new(), detail: String::new(), visible: None, render: None, lead: false, inert: false, chosen: false, home: None })
            .collect();
        let params = N::Params {
            workspace: doc.clone(),
            active: Some(active),
            scope,
            records: None,
            probes: Rc::new(|_| 0),
            selected: Vec::new(),
            chips,
            objects,
            root_detail: String::new(),
            layouts: None,
            notes: Vec::new(),
        };
        let geometry = L::geometry(None, Some(&[Panel::Timeline]), &layout, &frame_at((0., 0.), vec![], 0));
        let bounds = L::find(&geometry, &Panel::Outline).expect("outline").body;
        for (row, (x, y, w, h)) in N::row_rects(&N::initial(), &params, bounds) {
            let text = N::describe(&row);
            let text = text.strip_prefix("> ").unwrap_or(&text).to_string();
            if text.starts_with(&format!("{graph} · ")) {
                return (f64::from(x + w / 2.), f64::from(y + h / 2.));
            }
        }
        fail(&format!("the outline has no row {graph}"))
    };
    let tile = |e: &Ed, path: &[&str]| -> (f64, f64) {
        let path: flow_check::workspace::Path = path.iter().map(|s| (*s).into()).collect();
        let (x, y, w, h) = editor::node_box(e.e(), &path).expect("box");
        (x as f64 + w as f64 / 2., y as f64 + h as f64 - 6.)
    };
    // the pointer carries cobalt from its outline row onto the node of shards that reads spare
    e.jump("shards");
    chk(e.pane() == "shards", "the pane shows shards");
    let ws = e.ws();
    let history = e.undo();
    let drag_to = |e: &mut Ed, target: (f64, f64)| {
        let from = row_of(e, "cobalt");
        e.step_m(from, vec![Event::PointerMoved(from)]);
        e.step_b(from, vec![Event::PointerPressed(LB, from)]);
        let (fx, fy) = from;
        e.step_b((fx + 2., fy + 1.), vec![Event::PointerMoved((fx + 2., fy + 1.))]);
        chk(editor::carrying(e.e()).is_none(), "inside the dead zone nothing is carried");
        e.step_b((fx + 12., fy + 6.), vec![Event::PointerMoved((fx + 12., fy + 6.))]);
        e.step_b((fx + 12., fy + 6.), vec![]);
        chk(editor::carrying(e.e()) == Some(("material".into(), "(ref cobalt)".into())), "past the dead zone the row is carried");
        e.step_b(target, vec![Event::PointerMoved(target)]);
        e.step_b(target, vec![]);
    };
    let sm = tile(&e, &["shards", "m"]);
    drag_to(&mut e, sm);
    chk(has(&e.text(), "(ref cobalt)") && !has(&e.text(), "(ref spare)"), &format!("hovering a target shows the edit everywhere: {}", e.text()));
    chk(e.undo() == history, "a preview writes nothing to the history");
    chk(has(&e.line(), "Preview") && has(&e.line(), ":material (ref cobalt) on shards/m"), &format!("the strip says what a release writes: {}", e.line()));
    let sm = tile(&e, &["shards", "m"]);
    e.step_b(sm, vec![key(Key::Escape)]);
    e.step_m(sm, vec![Event::PointerReleased(LB, sm)]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && same_ws(&e.ws(), &ws), "Escape leaves the document physically equal");
    chk(e.undo() == history, "and there is nothing to undo");
    let sm = tile(&e, &["shards", "m"]);
    drag_to(&mut e, sm);
    e.step_b((2000., 2000.), vec![Event::PointerMoved((2000., 2000.))]);
    e.step_m((2000., 2000.), vec![Event::PointerReleased(LB, (2000., 2000.))]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && same_ws(&e.ws(), &ws), "a release over nothing restores the document");
    let sm = tile(&e, &["shards", "m"]);
    drag_to(&mut e, sm);
    e.step(vec![Event::FocusLost]);
    e.step(vec![]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && same_ws(&e.ws(), &ws), "a lost window focus restores the document");
    let sm = tile(&e, &["shards", "m"]);
    drag_to(&mut e, sm);
    let at = tile(&e, &["shards", "m"]);
    e.step_m(at, vec![Event::PointerReleased(LB, at)]);
    e.step(vec![]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && e.undo().as_deref() == Some("Put"), &format!("a release puts it, as one entry: {:?}", e.undo()));
    chk(has(&e.text(), ":material (ref cobalt)") && !has(&e.text(), "(ref spare)"), "it wrote :material (ref cobalt)");
    let by_pointer = e.text();
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(e.undo() == history && same_ws(&e.ws(), &ws), "one undo gives the document back");

    // by keys: the same put, the same entry
    e.jump("cobalt");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    chk(editor::carrying(e.e()) == Some(("material".into(), "(ref cobalt)".into())), "y picks up the open material");
    chk(has(&e.line(), "a shards"), &format!("every place that takes it has a letter: {}", e.line()));
    e.step(vec![ch('a')]);
    e.step(vec![]);
    chk(has(&e.text(), "(ref cobalt)") && e.undo() == history, "a target letter previews, writing nothing");
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && e.undo().as_deref() == Some("Put"), "Enter writes one entry named Put");
    chk(e.text() == by_pointer, "the keys wrote the same text as the pointer");
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "and one undo gives it back");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_some() && has(&e.line(), "Carrying"), "Enter with no target chosen keeps the carry");
    e.step(vec![key(Key::Escape)]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && same_ws(&e.ws(), &ws), "Escape drops a key carry");

    // a SOP graph goes to the scene: one more object over the existing graph
    e.jump("shards");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    chk(editor::carrying(e.e()) == Some(("sop".into(), "(ref shards)".into())), "y picks up the open SOP graph");
    e.jump("scene");
    chk(e.pane() == "scene" && editor::carrying(e.e()).is_some(), "the carry survives navigation");
    e.step(vec![ch('a')]);
    e.step(vec![]);
    chk(has(&e.line(), "scene/geometry (ref shards)"), &format!("the scene takes it: {}", e.line()));
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(e.undo().as_deref() == Some("Put") && editor::carrying(e.e()).is_none(), "the put is one entry");
    let text = e.text();
    chk(text.matches("(scene/geometry (ref shards)").count() == 2, &format!("two objects now show shards: {text}"));
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "undone");

    // a place that does not take the payload refuses, with the checker's words
    e.jump("scene");
    let t = tile(&e, &["scene", "@result"]);
    drag_to(&mut e, t);
    chk(has(&e.line(), "Refused"), &format!("a merge takes no material, and the strip says why: {}", e.line()));
    chk(same_ws(&e.ws(), &ws), "a refused target shows nothing");
    let at = tile(&e, &["scene", "@result"]);
    e.step_m(at, vec![Event::PointerReleased(LB, at)]);
    e.step(vec![]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && e.undo() == history, "a release on a refusal writes nothing");
    // a SOP graph put on an object re-points its (ref ...)
    let drag_graph = |e: &mut Ed, graph: &str, target: (f64, f64)| {
        let from = row_of(e, graph);
        let (fx, fy) = from;
        e.step_m(from, vec![Event::PointerMoved(from)]);
        e.step_b(from, vec![Event::PointerPressed(LB, from)]);
        e.step_b((fx + 12., fy + 6.), vec![Event::PointerMoved((fx + 12., fy + 6.))]);
        e.step_b((fx + 12., fy + 6.), vec![]);
        e.step_b(target, vec![Event::PointerMoved(target)]);
        e.step_b(target, vec![]);
        e.step_m(target, vec![Event::PointerReleased(LB, target)]);
        e.step(vec![]);
        e.step(vec![]);
    };
    let t = tile(&e, &["scene", "obj"]);
    drag_graph(&mut e, "plinth", t);
    chk(e.undo().as_deref() == Some("Put") && has(&e.text(), "(scene/geometry (ref plinth)"), &format!("a SOP graph on an object re-points it: {}", e.text()));
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "and one undo gives it back");

    // timings of a preview: apply on the first hot frame, restore on the cancel
    e.jump("shards");
    let from = row_of(&e, "cobalt");
    e.step_m(from, vec![Event::PointerMoved(from)]);
    e.step_b(from, vec![Event::PointerPressed(LB, from)]);
    let (fx, fy) = from;
    e.step_b((fx + 12., fy + 6.), vec![Event::PointerMoved((fx + 12., fy + 6.))]);
    e.step_b((fx + 12., fy + 6.), vec![]);
    let target = tile(&e, &["shards", "m"]);
    e.step_b(target, vec![Event::PointerMoved(target)]);
    let t0 = std::time::Instant::now();
    e.step_b(target, vec![]);
    let apply = t0.elapsed().as_secs_f64() * 1000.;
    chk(has(&e.text(), "(ref cobalt)"), "the preview is on");
    let t0 = std::time::Instant::now();
    e.step_b(target, vec![key(Key::Escape)]);
    let restore = t0.elapsed().as_secs_f64() * 1000.;
    e.step_m(target, vec![Event::PointerReleased(LB, target)]);
    e.step(vec![]);
    chk(same_ws(&e.ws(), &ws), "restored");
    // resting on a node enters what it references after 0.6 s
    e.jump("shards");
    let from = row_of(&e, "cobalt");
    let (fx, fy) = from;
    e.step_m(from, vec![Event::PointerMoved(from)]);
    e.step_b(from, vec![Event::PointerPressed(LB, from)]);
    e.step_b((fx + 12., fy + 6.), vec![Event::PointerMoved((fx + 12., fy + 6.))]);
    let target = tile(&e, &["shards", "m"]);
    e.step_b(target, vec![Event::PointerMoved(target)]);
    e.step_full(35, &[LB], &[], target, vec![]);
    chk(e.pane() == "shards", "the pointer has not rested long enough to enter");
    e.step_full(40, &[LB], &[], target, vec![]);
    chk(e.pane() == "spare" && editor::carrying(e.e()).is_some(), &format!("resting on a node entered the graph it reads, still carrying: {}", e.pane()));
    e.step_b(target, vec![key(Key::Escape)]);
    e.step_m(target, vec![Event::PointerReleased(LB, target)]);
    e.step(vec![]);
    chk(same_ws(&e.ws(), &ws) && editor::carrying(e.e()).is_none(), "and Escape still gives the document back");
    e.step(vec![ch('u')]);
    e.step(vec![]);
    // a viewport is a place
    let view_center = |e: &Ed| -> (f64, f64) {
        let (vx, vy, vw, vh) = editor::panes(e.e(), &frame_at((0., 0.), vec![], e.d.count)).view;
        (vx as f64 + vw as f64 / 2., vy as f64 + vh as f64 / 2.)
    };
    let carry_from_row = |e: &mut Ed, graph: &str| {
        let from = row_of(e, graph);
        let (fx, fy) = from;
        e.step_m(from, vec![Event::PointerMoved(from)]);
        e.step_b(from, vec![Event::PointerPressed(LB, from)]);
        e.step_b((fx + 12., fy + 6.), vec![Event::PointerMoved((fx + 12., fy + 6.))]);
    };
    e.jump("scene");
    carry_from_row(&mut e, "cobalt");
    let over = view_center(&e);
    e.step_b(over, vec![Event::PointerMoved(over)]);
    e.step_b(over, vec![]);
    e.step_b(over, vec![]);
    chk(has(&e.line(), ":material (ref cobalt) on m in graph shards"), &format!("a surface names its node: {}", e.line()));
    e.step_m(over, vec![Event::PointerReleased(LB, over)]);
    e.step(vec![]);
    e.step(vec![]);
    chk(e.undo().as_deref() == Some("Put") && e.text() == by_pointer, "a put on a surface is the same entry as on the node");
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "undone");
    // with no material node in the graph, the put adds one after the result, in the same entry
    editor::close(e.e());
    let mut e = Ed::new(make(
        r##"(workspace bare
  (graph cobalt :context material (material/standard :name "cobalt blue" :color "#2670f5"))
  (graph plinth :context sop (let* [b (sop/box)] b))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref plinth) :name "plinth")] (scene/merge obj))))"##,
        f64::INFINITY,
    ));
    e.step(vec![]);
    let ws = e.ws();
    e.jump("scene");
    carry_from_row(&mut e, "cobalt");
    let over = view_center(&e);
    e.step_b(over, vec![Event::PointerMoved(over)]);
    e.step_b(over, vec![]);
    e.step_b(over, vec![]);
    chk(has(&e.line(), "a sop/material node and :material (ref cobalt) in graph plinth"), &format!("no material node yet: {}", e.line()));
    e.step_m(over, vec![Event::PointerReleased(LB, over)]);
    e.step(vec![]);
    e.step(vec![]);
    chk(e.undo().as_deref() == Some("Put") && has(&e.text(), "(sop/material b :material (ref cobalt))"), &format!("one entry added the node and its material: {}", e.text()));
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "undone as one");
    // over the budget
    editor::close(e.e());
    let mut e = Ed::new(make(CARRY_SOURCE, 0.));
    e.step(vec![]);
    let ws = e.ws();
    e.jump("shards");
    let sm = tile(&e, &["shards", "m"]);
    drag_to(&mut e, sm);
    chk(has(&e.line(), "Would write :material (ref cobalt) on shards/m") && has(&e.line(), "no preview") && has(&e.line(), "applying takes"), &format!("a slow put says what it would write and why it is not shown: {}", e.line()));
    chk(same_ws(&e.ws(), &ws) && has(&e.text(), "(ref spare)") && !has(&e.text(), "(ref cobalt)"), "a held-back put shows no picture: the document shown is the original");
    chk(editor::carrying(e.e()).is_some() && e.undo() == history, "the carry goes on, nothing is written");
    let at = tile(&e, &["shards", "m"]);
    e.step_m(at, vec![Event::PointerReleased(LB, at)]);
    e.step(vec![]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && e.undo().as_deref() == Some("Put") && has(&e.text(), ":material (ref cobalt)"), "the release writes it, as one Put entry");
    e.step_k(&[Key::Meta], vec![ch('z')]);
    chk(same_ws(&e.ws(), &ws), "and one undo gives it back");
    e.jump("cobalt");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(has(&e.line(), "Pick a target letter first") && has(&e.line(), "Carrying"), &format!("the reminder is on the strip with the prompt: {}", e.line()));
    e.step(vec![ch('a')]);
    e.step(vec![]);
    chk(!has(&e.line(), "Pick a target letter first"), "choosing a letter clears the reminder");
    e.step(vec![key(Key::Escape)]);
    e.step(vec![]);
    chk(apply < 500. && restore < 500., "a preview applies and restores inside the 500 ms budget");
    editor::close(e.e());
}

fn kg_len(e: &editor::Editor) -> usize {
    editor::scene_document(e).len()
}

const VIEWPORT_SOURCE: &str = r#"(workspace views
  (graph shards :context sop (sop/box))
  (graph day :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")
           cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           all (scene/merge obj cam side)]
      (scene/root all :camera cam)))
  (graph night :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")
           side (scene/camera :name "side" :eye [-6 2 0])
           all (scene/merge obj side)]
      all))
  (graph editor :context editor
    (let* [left (ui/viewport (ref day))
           right (ui/viewport (ref night))]
      (ui/workspace (ui/split_at "horizontal" 0.6 (ui/split_at "horizontal" 0.5 left right) (ui/graph))))))"#;

pub fn viewport_carry_tests() {
    let ws0 = load(VIEWPORT_SOURCE);
    let mut e = Ed::new(editor::create(editor::Create { ..options() }, &ws0, None, editor::prepare_fn(), editor::draw_fn()).unwrap_or_else(|m| fail(&m)));
    for _ in 0..12 {
        e.step(vec![]);
    }
    let ws = e.ws();
    let history = e.undo();
    let undo = |e: &mut Ed| {
        e.step_k(&[Key::Meta], vec![ch('z')]);
        e.step(vec![]);
    };
    e.jump("night");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    chk(editor::carrying(e.e()) == Some(("scene".into(), "(ref night)".into())), "y picks up the open scene graph");
    chk(has(&e.line(), "a viewport 1") && !has(&e.line(), "viewport 2"), &format!("the viewport that does not show it has a letter, the one that does has none: {}", e.line()));
    e.step(vec![ch('a')]);
    e.step(vec![]);
    chk(has(&e.line(), "(ui/viewport (ref night))") && has(&e.text(), "(ui/viewport (ref night))") && !has(&e.text(), "(ui/viewport (ref day))"), &format!("a letter previews the panel re-pointed: {}", e.line()));
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(
        editor::carrying(e.e()).is_none() && e.undo().as_deref() == Some("Put") && has(&e.text(), "left (ui/viewport (ref night))") && has(&e.text(), "right (ui/viewport (ref night))"),
        &format!("Enter writes one entry: {}", e.text()),
    );
    undo(&mut e);
    chk(same_ws(&e.ws(), &ws) && e.undo() == history, "one undo gives it back");
    // a camera object selected
    e.jump("day");
    let (bx, by, bw, bh) = editor::node_box(e.e(), &["day".into(), "side".into()]).expect("box");
    let at = (bx as f64 + bw as f64 / 2., by as f64 + bh as f64 - 6.);
    e.step_m(at, vec![Event::PointerMoved(at)]);
    e.step_m(at, vec![Event::PointerPressed(LB, at), Event::PointerReleased(LB, at)]);
    e.step(vec![]);
    e.step(vec![ch('y')]);
    e.step(vec![]);
    chk(editor::carrying(e.e()) == Some(("camera".into(), "side".into())), &format!("y picks up the selected camera: {:?}", editor::carrying(e.e())));
    e.step(vec![ch('a')]);
    e.step(vec![]);
    chk(has(&e.line(), ":camera side on all in graph day") || has(&e.line(), ":camera side on"), &format!("the root of day takes it: {}", e.line()));
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(e.undo().as_deref() == Some("Put") && has(&e.text(), ":camera side"), &format!("the root's :camera is written: {}", e.text()));
    undo(&mut e);
    chk(same_ws(&e.ws(), &ws), "one undo gives it back");
    // the second viewport shows a part, which has no root
    e.step(vec![ch('y')]);
    e.step(vec![]);
    e.step(vec![ch('s')]);
    e.step(vec![]);
    chk(has(&e.line(), "a scene/root with :camera side in graph night"), &format!("a part gets a root: {}", e.line()));
    e.step(vec![key(Key::Enter)]);
    e.step(vec![]);
    chk(e.undo().as_deref() == Some("Put") && has(&e.text(), "(scene/root") && has(&e.text(), ":camera side"), &format!("the root is added in the same entry: {}", e.text()));
    undo(&mut e);
    chk(same_ws(&e.ws(), &ws), "undone as one");
    // a left press on the panel while a scene is held is the put
    e.jump("night");
    e.step(vec![ch('y')]);
    e.step(vec![]);
    let (vx, vy, vw, vh) = editor::panes(e.e(), &frame_at((0., 0.), vec![], e.d.count)).view;
    let over = (vx as f64 + vw as f64 / 2., vy as f64 + vh as f64 / 2.);
    e.step_m(over, vec![Event::PointerMoved(over)]);
    e.step_m(over, vec![]);
    chk(has(&e.line(), "(ui/viewport (ref night))"), &format!("the pointer over the panel previews it: {}", e.line()));
    e.step_m(over, vec![Event::PointerPressed(LB, over), Event::PointerReleased(LB, over)]);
    e.step(vec![]);
    chk(editor::carrying(e.e()).is_none() && e.undo().as_deref() == Some("Put") && has(&e.text(), "left (ui/viewport (ref night))"), &format!("a press on the panel is the put: {}", e.text()));
    undo(&mut e);
    chk(same_ws(&e.ws(), &ws), "undone");
    editor::close(e.e());
}
