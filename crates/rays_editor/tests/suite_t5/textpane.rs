//! Editor-driven sections of lib/rays_editor/test/test_text_pane.ml: editor_text, editor_binding, editor_w9,
//! editor_active_scrub, editor_literal_scrub, editor_text_drop, editor_command_click, editor_text_find.
use super::t5c::*;
use editor_document::workspace_doc as wd;
use flow_graph::flow_edit::{self as fe, ArgKey};
use flow_syntax::syntax::{self as syn, Node};
use input::{Event, Key, MouseButton};
use rays_editor::text_pane as T;
use rays_editor::Editor;

const LB: MouseButton = MouseButton::LeftButton;

fn small_sunflower() -> String {
    case_text("sunflower").replace("(seeds : int 240)", "(seeds : int 8)")
}

fn small_rosette() -> String {
    let mut t = case_text("rosette");
    for (from, by) in [
        ("(petals : int 12)", "(petals : int 2)"),
        (":segments 12", ":segments 4"),
        (":rings 6", ":rings 3"),
        (":segments 8", ":segments 4"),
        (":rings 4", ":rings 2"),
        ("(radial j\n                         6", "(radial j\n                         1"),
    ] {
        t = t.replace(from, by);
    }
    t
}

fn num(s: &str) -> syn::Syntax {
    syn::make(syn::Draft::Num(s.into()))
}

fn printed(source: &[syn::Syntax]) -> String {
    flow_syntax::lisp::print(source).unwrap().0
}

pub fn editor_text() {
    let (mut env, presets) = start_env(&small_sunflower());
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    env.click(((gx + 50) as f64, (gy + 100) as f64));
    env.step(vec![key(Key::Home)]);
    env.step(vec![ch('i')]);
    env.settle();
    env.step(vec![key(Key::KeyChar('/')), ch('l'), ch('t')]);
    env.step(vec![]);
    check(has(&env.dump(), "projection: text"), &format!("/ l t did not reach the text pane\n{}", env.dump()));
    let original = env.ws();
    let base = env.undo();
    let applied = env.src();
    let tab_document = tab_at(gw < 400, gx + gw - 159, gy, 2);
    let area = ((gx + 200) as f64, (gy + 24 + 60) as f64);
    let bar_y = (gy + gh - if gw < 400 { 16 } else { 40 }) as f64;
    let apply = ((gx + 32) as f64, bar_y);
    let discard = ((gx + 180) as f64, bar_y);
    env.click(tab_document);
    check(has(&env.dump(), "document tab, draft no"), &format!("Document tab\n{}", env.dump()));
    let type_text = |env: &mut Env, text: &str| {
        env.click(area);
        env.step_k(&[Key::Meta], vec![ch('a')]);
        env.step(vec![Event::Text_input(text.into())]);
    };
    type_text(&mut env, &replace(&applied, "(seeds : int 8)", "(seeds : int 7)"));
    check(has(&env.dump(), "draft yes"), &format!("a draft was not kept\n{}", env.dump()));
    check(same_ws(&env.ws(), &original) && env.undo() == base, "a draft changed the document");
    env.click(discard);
    check(has(&env.dump(), "draft no"), "Discard kept the draft");
    check(same_ws(&env.ws(), &original), "Discard changed the document");
    let bad = replace(&applied, "(sqrt i)", "(sqrt nosuch)");
    let line = 1 + bad[..index_of(&bad, "(sqrt nosuch)")].split('\n').count() - 1;
    type_text(&mut env, &bad);
    env.click(apply);
    let text = env.dump();
    check(has(&text, "draft yes"), &format!("an invalid draft left the pane\n{text}"));
    check(has(&text, &format!("error at line {line}:")), &format!("error not at line {line}\n{text}"));
    check(same_ws(&env.ws(), &original) && env.undo() == base, "an invalid apply changed the document");
    env.click(area);
    env.step(vec![Event::Text_input("\n".into())]);
    check(!has(&env.dump(), ", error"), &format!("an edit left its stale error mark\n{}", env.dump()));
    env.click(apply);
    check(has(&env.dump(), "error at line"), "the next apply marks its line again");
    let marked = |env: &Env| env.dump().lines().find(|l| l.starts_with("text: ")).expect("text line").to_string();
    let before = marked(&env);
    env.click(area);
    env.step_k(&[Key::Meta], vec![key(Key::ArrowDown)]);
    env.step(vec![Event::Text_input("\n".into())]);
    check(has(&marked(&env), "error at line") && marked(&env) == before, &format!("typing below the error cleared it or moved it\n{}", env.dump()));
    env.click(discard);
    type_text(&mut env, &format!("{applied} \"open"));
    check(!has(&env.dump(), ", error"), "a draft was parsed before it rested");
    for _ in 0..30 {
        env.step(vec![]);
    }
    check(has(&env.dump(), ", error"), &format!("a rested draft was not parsed\n{}", env.dump()));
    env.step(vec![Event::Text_input("\"".into())]);
    check(!has(&env.dump(), ", error"), &format!("the parse of an older text outlived an edit\n{}", env.dump()));
    for _ in 0..30 {
        env.step(vec![]);
    }
    check(!has(&env.dump(), ", error"), &format!("a balanced draft was reported\n{}", env.dump()));
    env.click(discard);
    type_text(&mut env, &replace(&applied, "(seeds : int 8)", "(seeds : int 7)"));
    env.click(apply);
    check(has(&env.dump(), "draft no"), &format!("Check & apply kept the draft\n{}", env.dump()));
    check(!same_ws(&env.ws(), &original) && has(&env.src(), "(seeds : int 7)"), "Check & apply did not change the document");
    check(env.undo().as_deref() == Some("Edit text"), "the history label is not Edit text");
    env.step_k(&[Key::Meta], vec![ch('z')]);
    check(env.src() == applied, "undo did not restore the text");
    check(Editor::redo_label(env.e()).as_deref() == Some("Edit text"), "redo label");
    // the Graph tab
    let original = env.ws();
    let tab_graph = tab_at(gw < 400, gx + gw - 159, gy, 1);
    env.click(tab_graph);
    check(has(&env.dump(), "graph tab, draft no"), &format!("Graph tab\n{}", env.dump()));
    let graph_text = T::make_shown(&original.source, "sunflower", None, T::Tab::Graph).unwrap().text;
    check(has(&graph_text, "(graph sunflower"), "the Graph tab does not show the graph");
    env.click(area);
    env.step_k(&[Key::Meta], vec![ch('a')]);
    env.step(vec![Event::Text_input(replace(&graph_text, "(seeds : int 8)", "(seeds : int 6)"))]);
    check(has(&env.dump(), "graph tab, draft yes"), &format!("the Graph tab kept no draft\n{}", env.dump()));
    check(same_ws(&env.ws(), &original), "a Graph draft changed the document");
    env.step_k(&[Key::Meta], vec![key(Key::Enter)]);
    check(has(&env.dump(), "graph tab, draft no"), &format!("Command-Enter did not apply\n{}", env.dump()));
    check(has(&env.src(), "(seeds : int 6)") && env.undo().as_deref() == Some("Edit text"), "the Graph tab's apply is not one Edit text entry");
    let labelled = env.undo();
    env.click(area);
    env.step_k(&[Key::Meta], vec![ch('a')]);
    env.step(vec![Event::Text_input(replace(&graph_text, "(seeds : int 8)", "(seeds : int 5)"))]);
    check(has(&env.dump(), "graph tab, draft yes"), "the draft was not made");
    for _ in 0..3 {
        env.step_k(&[Key::Meta], vec![ch('z')]);
    }
    env.step(vec![]);
    check(env.undo() == labelled && has(&env.src(), "(seeds : int 6)"), &format!("an extra Command-Z undid the document under a draft: {}", env.dump()));
    env.click(discard);
    let applied_graph = env.ws();
    env.click(area);
    env.step_k(&[Key::Meta], vec![ch('a')]);
    env.step(vec![Event::Text_input(replace(&graph_text, "(sqrt i)", "(sqrt nosuch)"))]);
    env.step_k(&[Key::Meta], vec![key(Key::Enter)]);
    check(has(&env.dump(), "graph tab, draft yes, error"), &format!("a refused graph left the pane\n{}", env.dump()));
    check(same_ws(&env.ws(), &applied_graph), "a refused Graph apply changed the document");
    env.click(discard);
    let current_graph = T::make_shown(&env.ws().source, "sunflower", None, T::Tab::Graph).unwrap().text;
    type_text(&mut env, &replace(&current_graph, "(seeds : int 6)", "(seeds : int 3)"));
    env.step(vec![key(Key::Escape)]);
    env.step_k(&[Key::Meta], vec![ch('z')]);
    let undone = env.ws();
    let undo_label = env.undo();
    env.click(apply);
    check(same_ws(&env.ws(), &undone) && env.undo() == undo_label, "an old Graph draft overwrote undo");
    check(has(&env.dump(), "draft yes") && has(&env.dump(), "E_DRAFT_CONFLICT"), &format!("an old Graph draft was not retained as a conflict: {}", env.dump()));
    env.click(discard);
    env.click(tab_document);
    let t = replace(&wd::to_text(&env.ws()).unwrap(), "(seeds : int 8)", "(seeds : int 2)");
    type_text(&mut env, &t);
    env.d.map(|e| Editor::edit(e, &fe::Op::SetInputDefault { form: "sunflower".into(), input: "seeds".into(), value: num("4") }).expect("edit"));
    let host_edited = env.ws();
    let host_label = env.undo();
    env.click(apply);
    check(same_ws(&env.ws(), &host_edited) && env.undo() == host_label, "an old Document draft overwrote a host edit");
    check(has(&env.dump(), "draft yes") && has(&env.dump(), "E_DRAFT_CONFLICT"), &format!("an old Document draft was not kept as a conflict: {}", env.dump()));
    env.click(discard);
    let t = replace(&wd::to_text(&env.ws()).unwrap(), "(spread : float 0.062)", "(spread : float 0.07)");
    type_text(&mut env, &t);
    env.d.map(|e| Editor::edit(e, &fe::Op::SetInputDefault { form: "sunflower".into(), input: "seeds".into(), value: num("9") }).expect("edit"));
    env.click(apply);
    let merged = wd::to_text(&env.ws()).unwrap();
    check(has(&merged, "(seeds : int 9)") && has(&merged, "(spread : float 0.07)") && has(&env.dump(), "draft no"), &format!("a draft and a host edit of different values did not merge: {}{merged}", env.dump()));
    type_text(&mut env, &replace(&merged, ":segments 6", ":segments 8"));
    env.d.map(|e| {
        Editor::edit(e, &fe::Op::SetArg { node: vec!["sunflower".into(), "seeds_each".into(), "@result".into()], key: ArgKey::Kw("uniform_scale".into()), sub: vec![], value: num("0.5") }).unwrap_or_else(|m| panic!("{m}"))
    });
    env.click(apply);
    let merged = wd::to_text(&env.ws()).unwrap();
    check(has(&merged, ":segments 8") && has(&merged, ":uniform_scale 0.5") && has(&env.dump(), "draft no"), &format!("an added keyword argument and a draft did not merge: {}{merged}", env.dump()));
    type_text(&mut env, &replace(&merged, ":segments 8", ":segments 9"));
    env.step_k(&[Key::Meta], vec![ch('s')]);
    env.step(vec![]);
    check(has(&wd::to_text(&env.ws()).unwrap(), ":segments 9") && has(&env.dump(), "draft no"), &format!("Command-S with a draft in the text pane did not apply it: {}", env.dump()));
    check(editor_document::preset::list(&presets).len() == 1, "Command-S with the caret in the text pane saved nothing");
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}

pub fn editor_binding() {
    let (mut env, presets) = start_env(&small_sunflower());
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    env.click(((gx + 50) as f64, (gy + 100) as f64));
    env.step(vec![key(Key::Home)]);
    env.step(vec![ch('i')]);
    env.settle();
    for _ in 0..4 {
        env.step(vec![key(Key::ArrowRight)]);
    }
    env.step(vec![key(Key::KeyChar('/')), ch('l'), ch('t')]);
    env.step(vec![]);
    check(has(&env.dump(), "projection: text"), "/ l t did not reach the text pane");
    let area = ((gx + 200) as f64, (gy + 24 + 60) as f64);
    let apply = ((gx + 32) as f64, (gy + gh - if gw < 400 { 16 } else { 40 }) as f64);
    let type_closure = |env: &mut Env, text: &str| {
        env.click(area);
        env.step_k(&[Key::Meta], vec![ch('a')]);
        env.step(vec![Event::Text_input(text.into())]);
    };
    let original = env.ws();
    type_closure(&mut env, "(let* [head (sop/merge nosuch)] head)");
    env.click(apply);
    check(has(&env.dump(), "error at line 1:"), &format!("a refused closure has no line\n{}", env.dump()));
    check(same_ws(&env.ws(), &original), "a refused closure changed the document");
    env.click(area);
    env.step(vec![Event::Text_input(" ".into())]);
    check(!has(&env.dump(), ", error"), "typing in the closure left its error mark");
    type_closure(&mut env, "(let* [nosuch 1] nosuch)");
    env.click(apply);
    check(has(&env.dump(), "nosuch is not a binding of sunflower"), &format!("an unknown binding\n{}", env.dump()));
    type_closure(&mut env, "(let* [head (sop/merge seeds_each seeds_each)] seeds_each)");
    env.click(apply);
    check(env.undo().as_deref() == Some("Edit text") && has(&env.src(), "(sop/merge seeds_each seeds_each)"), &format!("the closure edit was not applied\n{}", env.dump()));
    let before = env.ws();
    let printed_binding = |ws: &wd::T, path: &[&str]| -> Option<String> {
        let path: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        T::binding(&ws.source, &path).map(|(_, v)| printed(&[v]))
    };
    let omitted = printed_binding(&original, &["sunflower", "seeds_each"]);
    check(omitted.is_some() && printed_binding(&before, &["sunflower", "seeds_each"]) == omitted, "Selection removed or changed an omitted binding");
    let refuse = |env: &mut Env, text: &str, message: &str| {
        type_closure(env, text);
        env.click(apply);
        check(same_ws(&env.ws(), &before), "refused Selection edit changed the document");
        check(has(&env.dump(), message), &format!("missing Selection refusal: {}", env.dump()));
    };
    refuse(&mut env, "(let* [head (sop/box)] head)", "result must stay unchanged");
    refuse(&mut env, "(let* [head (sop/box) head (sop/box)] seeds_each)", "Duplicate binding");
    type_closure(&mut env, "(let* [seeds_each (sop/box) head (sop/transform seeds_each)] seeds_each)");
    env.click(apply);
    let after = env.ws();
    check(!same_ws(&after, &before) && has(&env.dump(), "draft no"), &format!("coherent cross-binding type change was refused: {}", env.dump()));
    check(printed_binding(&after, &["sunflower", "seeds_each"]) != printed_binding(&before, &["sunflower", "seeds_each"]), "producer was not patched");
    let reloaded = load(&wd::to_text(&after).unwrap());
    check(printed_binding(&reloaded, &["sunflower", "head"]) == printed_binding(&after, &["sunflower", "head"]), "Selection patch did not survive serialized reload");
    env.click(((gx + 30) as f64, (gy + gh - 70) as f64));
    env.step(vec![key(Key::Escape)]);
    env.step_k(&[Key::Meta], vec![ch('z')]);
    check(same_ws(&env.ws(), &before), "Selection's related edits were not one undo entry");
    type_closure(&mut env, "(let* [head (sop/merge seeds_each)] seeds_each)");
    let form = flow_syntax::syntax::parse("(sop/merge seeds_each seeds_each seeds_each)").unwrap_or_else(|_| panic!("test expression")).remove(0);
    env.d.map(|e| Editor::edit(e, &fe::Op::SetArg { node: vec!["sunflower".into(), "head".into()], key: ArgKey::Whole, sub: vec![], value: form }).expect("edit"));
    let host_edited = env.ws();
    let host_label = env.undo();
    env.click(apply);
    check(same_ws(&env.ws(), &host_edited) && env.undo() == host_label, &format!("an old Selection draft overwrote a host edit: {}", env.dump()));
    check(has(&env.dump(), "draft yes") && has(&env.dump(), "E_DRAFT_CONFLICT"), &format!("an old Selection draft was not kept as a conflict: {}", env.dump()));
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}

// ---- editor_w9 ----------------------------------------------------------------------------------

type Script = Vec<(i64, Box<dyn Fn(&Editor::t) -> Vec<Event>>)>;

fn click_events(p: (f64, f64)) -> Vec<Event> {
    vec![Event::Pointer_moved(p), Event::Pointer_pressed(LB, p), Event::Pointer_released(LB, p)]
}

/// A card's header, where the graph pane put it.
fn head(e: &Editor::t, path: &[&str], (dx, dy): (f64, f64)) -> (f64, f64) {
    let path: Vec<String> = path.iter().map(|s| s.to_string()).collect();
    match Editor::node_box(e, &path) {
        Some((x, y, w, _)) => (x as f64 + if dx < 0. { w as f64 + dx } else { dx }, y as f64 + dy),
        None => panic!("the card has no box"),
    }
}

fn w9_frames(env0: Editor::t, first: i64, last: i64, script: &Script) -> Editor::t {
    let mut env = env0;
    for n in first..=last {
        let events = match script.iter().find(|(k, _)| *k == n) {
            Some((_, f)) => f(&env),
            None => match n {
                4 => click_events((900., 304.)),
                6 => vec![key(Key::ArrowDown)],
                8 => vec![ch('i')],
                12 => vec![Event::Pointer_moved((780., 380.)), Event::Scrolled((0., 9.))],
                16 => click_events((960., 470.)),
                _ => vec![],
            },
        };
        let mut f = frame_at((640., 360.), events, n);
        f.input.size = (1400, 800);
        env = Editor::update(env, &f);
    }
    env
}

pub fn editor_w9() {
    let presets = scratch_dir("rays-text-presets");
    let ui_font = ui_font();
    let start = || create(Editor::Create { domains: Some(1), presets: Some(presets.clone()), ui_font: ui_font.clone(), ..options() }, &load(&small_rosette()));
    let prefix = w9_frames(start(), 1, 21, &vec![]);
    let scenario = |prefix: &Editor::t, script: Script| -> (String, Option<String>) {
        let env = w9_frames(prefix.clone(), 22, 40, &script);
        (printed(&Editor::workspace(&env).source), Editor::undo_label(&env))
    };
    let untouched = printed(&Editor::workspace(&prefix).source);
    check(has(&untouched, "^:bypass (sop/subdivide inner"), "soft starts bypassed");
    
    let petals = ["rosette", ":petals"];
    let lab = |l: &Option<String>| l.clone().unwrap_or_else(|| "-".into());
    let soft_click = |e: &Editor::t| click_events(head(e, &["rosette", "soft"], (60., 12.)));
    let (text, label) = scenario(&prefix, vec![(22, Box::new(soft_click)), (24, Box::new(|_| vec![ch('b')]))]);
    check(!has(&text, "^:bypass") && label.as_deref() == Some("Bypass"), &format!("the b key: {}, bypass {}", lab(&label), has(&text, "^:bypass")));
    let (text, label) = scenario(&prefix, vec![(22, Box::new(soft_click)), (26, Box::new(|_| click_events((1216., 162.))))]);
    check(!has(&text, "^:bypass") && label.as_deref() == Some("Bypass"), &format!("the inspector's Bypass toggle: {}", lab(&label)));
    let (text, label) = scenario(
        &prefix,
        vec![
            (22, Box::new(soft_click)),
            (26, Box::new(|_| click_events((1220., 92.)))),
            (28, Box::new(|_| vec![ch('a')])),
            (30, Box::new(|_| vec![Event::Text_input("gentle".into())])),
            (32, Box::new(|_| vec![key(Key::Enter)])),
        ],
    );
    check(has(&text, "gentle ^:bypass (sop/subdivide") && !has(&text, "soft ^:bypass") && label.as_deref() == Some("Rename"), &format!("the inspector's name field: {}", lab(&label)));
    let (text, label) = scenario(
        &prefix,
        vec![
            (22, Box::new(move |e| click_events(head(e, &petals, (60., 12.))))),
            (
                26,
                Box::new(|_| {
                    let mut v = vec![key(Key::Alt)];
                    v.extend(click_events((1290., 165.)));
                    v.push(Event::Key_released(Key::Alt));
                    v
                }),
            ),
            (28, Box::new(|_| vec![ch('a')])),
            (30, Box::new(|_| vec![Event::Text_input("3".into())])),
            (32, Box::new(|_| vec![key(Key::Enter)])),
        ],
    );
    check(has(&text, "(petals : int 3)") && label.as_deref() == Some("Input default"), &format!("the inspector's input default: {}", lab(&label)));
    let (text, label) = scenario(&prefix, vec![(22, Box::new(soft_click)), (26, Box::new(|_| vec![ch('m')])), (30, Box::new(|_| click_events((500., 461.))))]);
    check(has(&text, "(defmacro soft_tpl [p1 inner]") && label.as_deref() == Some("Make macro"), &format!("the make-macro dialog: {}", lab(&label)));
    let fresh = w9_frames(start(), 1, 21, &vec![]);
    let (text, label) = scenario(
        &fresh,
        vec![(22, Box::new(soft_click)), (26, Box::new(|_| vec![ch('m')])), (30, Box::new(|_| click_events((700., 437.)))), (34, Box::new(|_| vec![key(Key::Enter)]))],
    );
    check(has(&text, "(defmacro soft_tpl [p1 inner]") && label.as_deref() == Some("Make macro"), &format!("Enter in the make-macro dialog: {}", lab(&label)));
    let wheel = || -> Box<dyn Fn(&Editor::t) -> Vec<Event>> { Box::new(|_| vec![Event::Pointer_moved((1260., 405.)), Event::Scrolled((0., -2.))]) };
    let (text, label) = scenario(
        &prefix,
        vec![
            (22, Box::new(soft_click)),
            (24, wheel()),
            (25, wheel()),
            (26, Box::new(|_| click_events((1250., 763.)))),
            (28, wheel()),
            (29, wheel()),
            (30, Box::new(|_| click_events((1290., 763.)))),
            (32, Box::new(|_| vec![ch('a')])),
            (34, Box::new(|_| vec![Event::Text_input("a fresh note".into())])),
            (36, Box::new(|_| vec![key(Key::Enter)])),
        ],
    );
    check(has(&text, "; a fresh note") && label.as_deref() == Some("Note"), &format!("the note edit: {}", lab(&label)));
    Editor::close(&prefix);
    let _ = std::fs::remove_dir_all(&presets);
}

// ---- scrubs --------------------------------------------------------------------------------------

fn find_col(l: &str, needle: &str, plus: usize) -> usize {
    l.find(needle).unwrap_or_else(|| panic!("no {needle}")) + plus
}

pub fn editor_active_scrub() {
    let text = r#"(workspace sw
  (graph g :context sop (sop/box))
  (graph scene :context scene (scene/merge (scene/geometry (ref g))))
  (graph editor :context editor
    (let* [wide (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp))
           split (ui/split-at "horizontal" 0.5 (ui/graph) (ui/inspector))
           three (ui/split-at "horizontal" 0.3 (ui/inspector) (ui/graph))]
      (ui/workspace (ui/switch wide split three :active 0)))))"#;
    let (mut env, presets) = start_env(text);
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    let gy = gy + gh + 1 + 28;
    let tab = tab_at(gw < 400, gx + gw - 36, gy, 2);
    env.at(tab, vec![Event::Pointer_moved(tab)]);
    env.at(tab, vec![Event::Pointer_pressed(LB, tab), Event::Pointer_released(LB, tab)]);
    env.step(vec![]);
    let printed_text = env.src();
    let lines: Vec<&str> = printed_text.split('\n').collect();
    let line = lines.iter().position(|l| has(l, ":active 0")).expect("line");
    let col = find_col(lines[line], ":active 0", 8);
    let char_w = 6.95;
    let number = (gx as f64 + 36. + col as f64 * char_w, gy as f64 + 6. + 8. + line as f64 * LINE_PITCH);
    let before = env.ws();
    let history = env.undo();
    let (nx, ny) = number;
    env.at(number, vec![Event::Pointer_moved(number)]);
    env.at_b(&[LB], number, vec![Event::Pointer_pressed(LB, number)]);
    env.at_b(&[LB], (nx + 10., ny), vec![Event::Pointer_moved((nx + 10., ny))]);
    check(has(&env.src(), ":active 2"), &format!("a drag of ten points did not set :active 2 live: {}", env.src()));
    env.at((nx + 10., ny), vec![Event::Pointer_released(LB, (nx + 10., ny))]);
    env.step(vec![]);
    check(env.undo().as_deref() == Some("Edit text"), &format!("the drag is one Edit text entry: {:?}", env.undo()));
    env.step_k(&[Key::Meta], vec![ch('z')]);
    check(same_ws(&env.ws(), &before) && env.undo() == history, "one undo returns the whole drag");
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}

pub fn editor_literal_scrub() {
    let text = r#"(workspace literal
    (graph g :context sop (let* [a (sop/points :points 1)] a))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))))
    (graph editor :context editor
      (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph "g") (ui/lisp)))))"#;
    let catalog = catalog();
    for (tab_index, tab) in [(0usize, T::Tab::Selection), (1, T::Tab::Graph), (2, T::Tab::Document)] {
        let presets = scratch_dir("rays-token-presets");
        let ui_font = ui_font();
        // Editor_prepare.empty
        let e = Editor::create(Editor::Create { domains: Some(1), presets: Some(presets.clone()), ui_font, ..options() }, &load(text), None, prepare_empty(), Editor::draw_fn()).unwrap_or_else(|m| panic!("{m}"));
        let mut env = Env::new(e);
        env.settle();
        let (x, y, w, h) = Editor::node_box(env.e(), &["g".to_string(), "a".to_string()]).expect("box");
        let point = ((x + w / 2) as f64, (y + 12.min(h / 2)) as f64);
        env.at(point, vec![Event::Pointer_moved(point)]);
        env.at(point, vec![Event::Pointer_pressed(LB, point), Event::Pointer_released(LB, point)]);
        env.step(vec![]);
        let (gx, gy, gw, gh) = env.graph_pane();
        let top = gy + gh + 1 + 28;
        let tab_point = tab_at(gw < 400, gx + gw - 36, top, tab_index);
        env.at(tab_point, vec![Event::Pointer_moved(tab_point)]);
        env.at(tab_point, vec![Event::Pointer_pressed(LB, tab_point), Event::Pointer_released(LB, tab_point)]);
        env.step(vec![]);
        let g_a = vec!["g".to_string(), "a".to_string()];
        let pts = ArgKey::Kw("points".into());
        let shown = T::make_shown(&env.ws().source, "g", Some(&g_a), tab).unwrap();
        let value = fe::arg_text(&shown.source, &g_a, &pts).expect("points");
        let span = shown.spans.iter().find(|(id, _)| *id == value.id).expect("span").1.clone();
        let position = rays_diagnostic::position_of_offset(&shown.text, span.start);
        let number = (gx as f64 + 36. + (position.col - 1) as f64 * 6.95 + 1., top as f64 + 14. + (position.line - 1) as f64 * LINE_PITCH);
        let (nx, ny) = number;
        let before = env.ws();
        let label = env.undo();
        env.at(number, vec![Event::Pointer_moved(number)]);
        env.at_b(&[LB], number, vec![Event::Pointer_pressed(LB, number)]);
        let drag = |env: &mut Env, dx: f64| -> flow_syntax::phase_timer::PhaseTimer {
            let t0 = std::time::Instant::now();
            let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || t0.elapsed().as_secs_f64()));
            env.at_b(&[LB], (nx + dx, ny), vec![Event::Pointer_moved((nx + dx, ny))]);
            guard.finish()
        };
        let scrubbed = |sample: &flow_syntax::phase_timer::PhaseTimer, what: &str| {
            for phase in ["print", "parse", "check", "project", "layout"] {
                check(flow_syntax::phase_timer::calls(sample, phase) == 0, &format!("{what} text token scrub ran {phase}"));
            }
            for phase in ["evaluate", "lower"] {
                check(flow_syntax::phase_timer::calls(sample, phase) <= 1, &format!("{what} text token scrub ran {phase} twice"));
            }
        };
        let s = drag(&mut env, 10.);
        scrubbed(&s, "a");
        let s = drag(&mut env, 20.);
        scrubbed(&s, "repeated");
        let s = drag(&mut env, 220.);
        scrubbed(&s, "wider");
        let valid = env.ws();
        let points = fe::arg_text(&valid.source, &g_a, &pts).expect("points");
        check(matches!(points.node, Node::Num(n) if points.name(n) == "45"), "a growing token lost its repaired span");
        let set = |v: &str| fe::Op::SetArg { node: g_a.clone(), key: pts.clone(), sub: vec![], value: num(v) };
        let (source, checked) = fe::apply_checked(&catalog, &before.source, &set("45")).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let mut alt = valid.clone();
        alt.source = source;
        alt.checked = std::sync::Arc::new(checked);
        check(wd::to_text(&valid).unwrap() == wd::to_text(&alt).unwrap(), "fast text token scrub differs from the full syntax edit");
        drag(&mut env, 520.);
        check(same_ws(&env.ws(), &valid), "an invalid text token scrub changed the document");
        drag(&mut env, 20.);
        let ws = env.ws();
        let points = fe::arg_text(&ws.source, &g_a, &pts).expect("points");
        check(matches!(points.node, Node::Num(n) if points.name(n) == "5"), &format!("text token scrub in tab {tab_index} did not apply its latest number: {} (at {nx:.1},{ny:.1}; text {:?})", flow_syntax::lisp::flat(&points), shown.text));
        let (source, checked) = fe::apply_checked(&catalog, &before.source, &set("5")).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let mut alt = ws.clone();
        alt.source = source;
        alt.checked = std::sync::Arc::new(checked);
        check(wd::to_text(&ws).unwrap() == wd::to_text(&alt).unwrap(), "text token scrub differs from the full syntax edit");
        env.at((nx + 20., ny), vec![Event::Pointer_released(LB, (nx + 20., ny))]);
        env.step(vec![]);
        check(env.undo().as_deref() == Some("Edit text"), "text token scrub lost its history label");
        let focus_graph = ((gx + gw - 12) as f64, (gy + 10.min(gh - 1)) as f64);
        env.at(focus_graph, vec![Event::Pointer_moved(focus_graph)]);
        env.at(focus_graph, vec![Event::Pointer_pressed(LB, focus_graph), Event::Pointer_released(LB, focus_graph)]);
        env.step(vec![]);
        env.step_k(&[Key::Meta], vec![ch('z')]);
        check(same_ws(&env.ws(), &before) && env.undo() == label, &format!("text token scrub in tab {tab_index} was not one undo entry: label {:?} (was {:?}), redo {}", env.undo(), label, Editor::can_redo(env.e())));
        env.close();
        let _ = std::fs::remove_dir_all(&presets);
    }
}

// ---- text drop / command click / find --------------------------------------------------------------

const DROP: &str = r#"(workspace drop
  (graph a :context sop (sop/box))
  (graph b :context sop (sop/merge (sop/box)))
  (graph scene :context scene (scene/merge (scene/geometry (ref b))))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))"#;

pub fn editor_text_drop() {
    let presets = scratch_dir("rays-text-presets");
    let ui_font = ui_font();
    let mut env = Env::new(create(Editor::Create { domains: Some(1), presets: Some(presets.clone()), ui_font, carry_budget: Some(f64::INFINITY), ..options() }, &load(DROP)));
    let jump = |env: &mut Env, name: &str| {
        env.step(vec![key(Key::KeyChar('/')), ch('j')]);
        env.step(vec![Event::Text_input(name.into())]);
        env.step(vec![key(Key::Enter)]);
        env.step(vec![]);
    };
    let line = |env: &Env| Editor::carry_line(env.e()).unwrap_or_else(|| "-".into());
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    let top = gy + gh + 1 + 28;
    jump(&mut env, "b");
    env.click_s(tab_at(gw < 400, gx + gw - 36, top, 1));
    jump(&mut env, "a");
    env.step(vec![ch('y')]);
    env.step(vec![]);
    check(Editor::carrying(env.e()) == Some(("sop".into(), "(ref a)".into())), "y holds graph a");
    jump(&mut env, "b");
    check(Editor::carrying(env.e()).is_some(), "the carry survives the jump");
    let original = env.ws();
    let history = env.undo();
    let point = |env: &Env, tab: T::Tab, needle: &str, after: usize| -> (f64, f64) {
        let text = T::make_shown(&env.ws().source, "b", None, tab).unwrap().text;
        let lines: Vec<&str> = text.split('\n').collect();
        let l = lines.iter().position(|l| has(l, needle)).expect("line");
        let col = lines[l].find(needle).expect("col");
        (gx as f64 + 36. + (col + after) as f64 * 6.95, top as f64 + 6. + 2. + l as f64 * LINE_PITCH)
    };
    let over = point(&env, T::Tab::Graph, "(sop/merge", 10);
    env.at(over, vec![Event::Pointer_moved(over)]);
    env.at(over, vec![]);
    check(has(&line(&env), "(ref a) at line"), &format!("the strip says where it goes: {}", line(&env)));
    check(has(&env.src(), "(sop/merge (ref a) (sop/box)"), &format!("the preview is the edit: {}", env.src()));
    check(env.undo() == history, "a preview writes nothing to the history");
    env.at(over, vec![Event::Pointer_pressed(LB, over), Event::Pointer_released(LB, over)]);
    env.step(vec![]);
    env.step(vec![]);
    check(Editor::carrying(env.e()).is_none() && env.undo().as_deref() == Some("Put") && has(&env.src(), "(sop/merge (ref a) (sop/box)"), &format!("a release writes it as one Put entry: {}", env.src()));
    env.step_k(&[Key::Meta], vec![ch('z')]);
    check(same_ws(&env.ws(), &original), "one undo gives it back");
    jump(&mut env, "a");
    env.step(vec![ch('y')]);
    env.step(vec![]);
    jump(&mut env, "b");
    let inside = point(&env, T::Tab::Graph, "(sop/merge", 5);
    env.at(inside, vec![Event::Pointer_moved(inside)]);
    env.at(inside, vec![]);
    check(has(&line(&env), "Refused"), &format!("a put that does not check is refused: {}", line(&env)));
    check(same_ws(&env.ws(), &original), "and the picture is the original");
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    check(Editor::carrying(env.e()).is_none() && same_ws(&env.ws(), &original), "Escape drops it");
    let a = ((gx + 200) as f64, (top + 24 + 60) as f64);
    env.at(a, vec![]);
    env.click_s(a);
    env.step(vec![Event::Text_input("x".into())]);
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    jump(&mut env, "a");
    env.step(vec![ch('y')]);
    env.step(vec![]);
    jump(&mut env, "b");
    let over = point(&env, T::Tab::Graph, "(sop/merge", 10);
    env.at(over, vec![Event::Pointer_moved(over)]);
    env.at(over, vec![]);
    check(has(&line(&env), "unapplied draft"), &format!("a draft refuses: {}", line(&env)));
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    env.click_s(tab_at(gw < 400, gx + gw - 36, top, 2));
    let original = env.ws();
    jump(&mut env, "a");
    env.step(vec![ch('y')]);
    env.step(vec![]);
    let over = point(&env, T::Tab::Document, "(sop/merge", 10);
    env.at(over, vec![Event::Pointer_moved(over)]);
    env.at(over, vec![]);
    check(has(&line(&env), "(ref a) at line"), &format!("the Document text takes it too: {}", line(&env)));
    env.at(over, vec![Event::Pointer_pressed(LB, over), Event::Pointer_released(LB, over)]);
    env.step(vec![]);
    env.step(vec![]);
    check(env.undo().as_deref() == Some("Put") && has(&env.src(), "(sop/merge (ref a) (sop/box)"), &format!("a put at a caret of the Document text is one Put entry: {}", env.src()));
    env.step_k(&[Key::Meta], vec![ch('z')]);
    check(same_ws(&env.ws(), &original), "and one undo gives it back");
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}

const CC: &str = r#"(workspace cc
  (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a)) c (ref h)] (+ a b)))
  (graph h :context value (let* [target 7.0] target))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))"#;

pub fn editor_command_click() {
    let (mut env, presets) = start_env(CC);
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    let gy = gy + gh + 1 + 28;
    let tab = tab_at(gw < 400, gx + gw - 36, gy, 2);
    env.at(tab, vec![Event::Pointer_moved(tab)]);
    env.at(tab, vec![Event::Pointer_pressed(LB, tab), Event::Pointer_released(LB, tab)]);
    env.step(vec![]);
    let printed_text = env.src();
    let lines: Vec<&str> = printed_text.split('\n').collect();
    let line = lines.iter().position(|l| has(l, "(+ a b)")).expect("line");
    let col = find_col(lines[line], "(+ a b)", 3);
    let char_w = 6.95;
    let name = (gx as f64 + 36. + col as f64 * char_w + 3., gy as f64 + 6. + 2. + line as f64 * LINE_PITCH);
    env.at(name, vec![Event::Pointer_moved(name)]);
    env.at(name, vec![Event::Pointer_pressed(LB, name), Event::Pointer_released(LB, name)]);
    env.step(vec![]);
    check(Editor::Private::inspector_subject(env.e()) == Some(vec!["g".to_string(), "@result".to_string()]), "an ordinary click in Document did not select its containing node");
    env.at_k(&[Key::Meta], name, vec![Event::Pointer_pressed(LB, name), Event::Pointer_released(LB, name)]);
    env.step(vec![]);
    env.step(vec![]);
    check(env.line("scope selected") == "g/a", &format!("a Command-click on a name did not select its binding: {}", env.line("scope selected")));
    check(Editor::Private::inspector_subject(env.e()) == Some(vec!["g".to_string(), "a".to_string()]), "the inspector does not show the Command-clicked binding");
    let graph_tab = tab_at(gw < 400, gx + gw - 36, gy, 1);
    env.at(graph_tab, vec![Event::Pointer_moved(graph_tab)]);
    env.at(graph_tab, vec![Event::Pointer_pressed(LB, graph_tab), Event::Pointer_released(LB, graph_tab)]);
    env.step(vec![]);
    let graph = T::make_shown(&env.ws().source, "g", None, T::Tab::Graph).unwrap();
    let lines: Vec<&str> = graph.text.split('\n').collect();
    let line = lines.iter().position(|l| has(l, "(ref h)")).expect("line");
    let col = find_col(lines[line], "(ref h)", 5);
    let reference = (gx as f64 + 36. + col as f64 * char_w + 3., gy as f64 + 8. + line as f64 * LINE_PITCH);
    env.at(reference, vec![Event::Pointer_moved(reference)]);
    env.at_k(&[Key::Ctrl], reference, vec![Event::Pointer_pressed(LB, reference), Event::Pointer_released(LB, reference)]);
    env.step(vec![]);
    env.step(vec![]);
    check(env.line("pane graph") == "h", "Ctrl-click did not open the referenced graph");
    check(has(&env.line("text"), "document tab"), "an out-of-Graph definition did not switch to Document");
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}

/// The dump's prompt line, or "" when there is none (`dump_line` panics on a missing key).
fn dump_prompt(env: &Env) -> String {
    env.dump().lines().find_map(|l| l.strip_prefix("prompt: ").map(str::to_string)).unwrap_or_default()
}

pub fn editor_text_find() {
    let (mut env, presets) = start_env(CC);
    let command = |env: &mut Env, c: char| {
        env.step_k(&[Key::Meta], vec![ch(c)]);
        env.step(vec![]);
    };
    let bar_at = |env: &Env| -> i64 {
        let line = env.line("text");
        let parts: Vec<&str> = line.split(' ').collect();
        for i in 0..parts.len().saturating_sub(1) {
            if parts[i] == "at" {
                if let Ok(n) = parts[i + 1].parse::<i64>() {
                    return n;
                }
            }
        }
        panic!("the bar is not open: {line}")
    };
    env.settle();
    let (gx, gy, gw, gh) = env.graph_pane();
    let gy = gy + gh + 1 + 28;
    let tab = tab_at(gw < 400, gx + gw - 36, gy, 2);
    env.at(tab, vec![Event::Pointer_moved(tab)]);
    env.at(tab, vec![Event::Pointer_pressed(LB, tab), Event::Pointer_released(LB, tab)]);
    env.step(vec![]);
    let document = |env: &Env| -> String { T::make_shown(&env.ws().source, "g", None, T::Tab::Document).unwrap().applied.text().unwrap().to_string() };
    env.click_s(((gx + 200) as f64, (gy + 8) as f64));
    command(&mut env, 'f');
    check(has(&env.line("text"), "find \"\""), &format!("Command-F did not open the bar: {}", env.line("text")));
    env.step(vec![Event::Text_input("(graph".into())]);
    env.step(vec![]);
    check(has(&env.line("text"), "find \"(graph\""), &format!("typing did not set the query: {}", env.line("text")));
    let matches = T::find_all(false, "(graph", &document(&env));
    check(matches.len() == 3, "the workspace has three graphs");
    let first_from = |at: i64| *matches.iter().find(|(a, _)| *a >= at).unwrap_or(&matches[0]);
    let start = bar_at(&env);
    let m0 = first_from(start);
    env.step(vec![key(Key::Enter)]);
    env.step(vec![]);
    check(bar_at(&env) == m0.0 + 1, &format!("Enter did not step past the current match: {}", env.line("text")));
    let m1 = first_from(m0.0 + 1);
    check(m1 != m0, "the next match is another one");
    env.step_k(&[Key::Shift], vec![key(Key::Enter)]);
    env.step(vec![]);
    check(bar_at(&env) == m0.0, &format!("Shift-Enter did not go back: {}", env.line("text")));
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    check(!has(&env.line("text"), "find"), &format!("Escape did not close the bar: {}", env.line("text")));
    let symbols = T::symbols(&document(&env));
    let names: Vec<(String, String)> = symbols.iter().map(|(n, k, _)| (n.clone(), k.clone())).collect();
    let want: Vec<(String, String)> = [("g", "graph"), ("a", "let*"), ("b", "let*"), ("c", "let*"), ("h", "graph"), ("target", "let*"), ("editor", "graph")].iter().map(|(a, b)| (a.to_string(), b.to_string())).collect();
    check(names == want, "the symbols are the graphs and the let* names, in order");
    command(&mut env, 'p');
    check(has(&dump_prompt(&env), "go to symbol"), &format!("Command-P did not open the symbols: {}", dump_prompt(&env)));
    env.step(vec![Event::Text_input("target".into())]);
    env.step(vec![key(Key::Enter)]);
    env.step(vec![]);
    env.step(vec![]);
    let target = symbols.iter().find(|(n, _, _)| n == "target").expect("target").2;
    command(&mut env, 'f');
    check(bar_at(&env) == target, &format!("the symbol pick did not place the caret: {}", env.line("text")));
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    check(T::symbol_at(&document(&env), target) == Some("target".into()) && T::symbol_at(&document(&env), 0).is_none(), "symbol_at reads the name under a byte");
    check(T::references(&document(&env), "target").len() == 2, "references finds both uses of target");
    env.step_k(&[Key::Shift], vec![key(Key::F12)]);
    env.step(vec![]);
    check(has(&dump_prompt(&env), "references"), &format!("Shift-F12 did not open the references: {}", dump_prompt(&env)));
    env.step(vec![key(Key::ArrowDown)]);
    env.step(vec![key(Key::Enter)]);
    env.step(vec![]);
    env.step(vec![]);
    check(!has(&dump_prompt(&env), "references"), "picking a use did not close the references");
    command(&mut env, 'f');
    check(bar_at(&env) == T::references(&document(&env), "target")[1].0, &format!("the pick did not place the caret on the second use: {}", env.line("text")));
    env.step(vec![key(Key::Escape)]);
    env.step(vec![]);
    let doc = document(&env);
    let lines: Vec<&str> = doc.split('\n').collect();
    let line = lines.iter().position(|l| has(l, "(ref h)")).expect("line");
    let col = find_col(lines[line], "(ref h)", 5);
    command(&mut env, 'l');
    check(has(&dump_prompt(&env), "go to line"), &format!("Command-L did not open the line prompt: {}", dump_prompt(&env)));
    let target_text = format!("{}:{}", line + 1, col + 1);
    env.step(vec![Event::Text_input(target_text.clone())]);
    env.step(vec![key(Key::Enter)]);
    env.step(vec![]);
    env.step(vec![]);
    let want: i64 = lines[..line].iter().map(|l| l.len() as i64 + 1).sum::<i64>() + col as i64;
    check(T::line_target(&document(&env), &target_text) == Some(want), "line_target counts the line and column");
    env.step(vec![key(Key::F12)]);
    env.step(vec![]);
    env.step(vec![]);
    check(env.line("pane graph") == "h", &format!("F12 did not follow the reference: {}", env.line("pane graph")));
    env.close();
    let _ = std::fs::remove_dir_all(&presets);
}
