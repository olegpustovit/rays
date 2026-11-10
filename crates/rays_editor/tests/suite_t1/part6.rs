//! , part 6: run_studio_keywords, run_studio_tail, run_panels,
//! run_host_scene_edit, run_panel_chain.
use super::support::*;
use editor_document::layout_edit::{Op as L, Size as LSize};
use editor_document::workspace_doc;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax::{self as s, Draft as Node};
use input::{Event, Key, MouseButton};
use rays_editor::editor;
use std::cell::Cell;
use std::rc::Rc;

const LB: MouseButton = MouseButton::LeftButton;

fn big(r: &mut Run, buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>) {
    r.count += 1;
    let mut f = frame(buttons, mouse, events, r.count);
    f.input.size = (1440, 900);
    r.e = editor::update(r.e.clone(), &f);
}

pub fn run_studio_keywords() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(&studio_text()));
    let click = |r: &mut Run, p: (f64, f64)| {
        big(r, &[], p, vec![Event::PointerMoved(p)]);
        big(r, &[LB], p, vec![Event::PointerPressed(LB, p)]);
        big(r, &[], p, vec![Event::PointerReleased(LB, p)]);
        big(r, &[], p, vec![]);
    };
    let at = (600., 700.);
    let rest = |r: &mut Run| {
        big(r, &[], at, vec![]);
        big(r, &[], at, vec![]);
    };
    let set = |r: &mut Run, name: &str, key: &str, value: Node| {
        r.e = match editor::edit(r.e.clone(), &Op::SetArg { node: vec!["editor".into(), name.into()], key: ArgKey::Kw(key.into()), sub: vec![], value: s::make(value) }) {
            Ok(e) => e,
            Err(m) => fail(&m),
        };
        rest(r);
    };
    rest(&mut r);
    check(r.dump("projection") == "graph" && editor::look_through(&r.e), "the studio opens as its keywords say");
    set(&mut r, "network", "view", Node::Str(b"list".to_vec()));
    check(r.dump("projection") == "list", "a changed :view was not followed");
    big(&mut r, &[], at, vec![key(Key::KeyChar('/')), key(Key::KeyChar('l'))]);
    big(&mut r, &[], at, vec![key(Key::KeyChar('g'))]);
    rest(&mut r);
    check(r.dump("projection") == "graph", "/ l g did not show the graph");
    r.e = match editor::edit_layout(r.e.clone(), &L::SetLayoutSize { node: vec!["editor".into(), "stage".into()], size: LSize::Ratio(0.4) }) {
        Ok(e) => e,
        Err(m) => fail(&m),
    };
    rest(&mut r);
    check(r.dump("projection") == "graph", "an edit that left :view alone reset the view the user chose");
    set(&mut r, "network", "view", Node::Str(b"graph".to_vec()));
    set(&mut r, "preview", "look_through", Node::Sym("false".into()));
    check(!editor::look_through(&r.e), "a changed :look_through was not followed");
    // the Size row of a split's card converts it, keeping the sizes it shows
    big(&mut r, &[], at, vec![key(Key::KeyChar('/')), key(Key::KeyChar('j'))]);
    big(&mut r, &[], at, vec![text_input("editor")]);
    big(&mut r, &[], at, vec![key(Key::Enter)]);
    rest(&mut r);
    let (x, y, w, h) = editor::node_box(&r.e, &["editor".into(), "studio".into()]).expect("studio card");
    click(&mut r, ((x + w / 2) as f64, (y + 1.max(h / 2)) as f64));
    check(r.dump("scope selected") == "editor/studio", &format!("the studio card is not where the test clicks: {}", r.dump("scope selected")));
    // Dismiss in the status strip: the graph background now clears selection.
    let pick = |r: &mut Run, dy: f64| {
        click(r, (700., 887.));
        click(r, (1390., 217.));
        click(r, (1390., 217. + dy));
    };
    pick(&mut r, 27.);
    check(has(&r.source(), "studio (ui/split_at \"horizontal\" 0.1505 outline work)") && r.undo_label() == Some("Resize panel".into()), &format!("the Size row did not size the split by its ratio: {}", r.source()));
    pick(&mut r, 75.);
    check(has(&r.source(), "studio (ui/split \"horizontal\" outline work :second_size 1223)"), &format!("the Size row did not fix the second side at its points: {}", r.source()));
    r.close();
}

pub fn run_studio_tail() {
    if !gpu() {
        return;
    }
    // a graph panel in text view is live without the focus, and takes a gesture on the first press
    let mut r = Run::new(editor(&with_editor("    (ui/workspace (ui/split \"horizontal\" (ui/graph \"garden\" :focus true) (ui/graph \"scene\" :view \"text\")))")));
    let a = (100., 300.);
    r.step(a, vec![]);
    r.step(a, vec![]);
    check(r.dump("pane graph") == "garden" && r.dump("projection") == "graph" && has(&r.dump("panels"), "graph 0*; graph 1: selection tab, draft no"), &format!("the second graph panel does not hold its own text pane: {}", r.dump("panels")));
    let point = (650., 300.);
    r.step(point, vec![Event::PointerMoved(point)]);
    r.stepb(&[LB], point, vec![Event::PointerPressed(LB, point)]);
    r.step(point, vec![Event::PointerReleased(LB, point)]);
    r.step(point, vec![text_input(" ")]);
    r.step(point, vec![]);
    r.step(point, vec![]);
    check(r.dump("pane graph") == "scene" && has(&r.dump("panels"), "graph 1*: selection tab, draft yes"), &format!("the first press in an unfocused text view did not start an edit there: {}", r.dump("panels")));
    r.close();
    // a float that is the bound name takes the window of its binding
    let floated = editor(&format!(
        "{}(layout (panel [\"editor\" \"win\"] :collapsed false :window [100 80 300 200]))",
        with_editor("    (let* [win (ui/floating (ui/inspector))]\n      (ui/workspace (ui/split \"horizontal\" (ui/graph) win)))")
    ));
    check(dump_line(&floated, "windows") == "inspector 100 80 300 200", &format!("a bound float's window: {}", dump_line(&floated, "windows")));
    editor::close(&floated);
}

pub fn run_panels() {
    if !gpu() {
        return;
    }
    let lowered = |body: &str| shell_of(&build_ok(&of_text(&with_editor(body))));
    // :of ties an inspector, a list or a text pane to the graph panel a binding names
    let tied = "    (let* [a (ui/graph \"garden\" :focus true)\n           b (ui/graph \"scene\")\n           ia (ui/inspector :of a)\n           ib (ui/inspector :of b)]\n      (ui/workspace (ui/split \"horizontal\" (ui/split \"vertical\" a b) (ui/split \"vertical\" ia ib))))";
    check(lowered(tied).follows == vec![(vec![1usize, 1], vec![0usize, 1]), (vec![1, 0], vec![0, 0])], "the inspectors are tied to their graph panels");
    expect_error("an :of that is not a graph panel", &with_editor("    (let* [c (ui/lisp)] (ui/workspace (ui/split \"vertical\" c (ui/inspector :of c))))"), "E_PANEL_OF");
    expect_error("an :of that is not a binding", &with_editor("    (ui/workspace (ui/split \"vertical\" (ui/graph) (ui/list :of (ui/graph))))"), "E_PANEL_OF");
    expect_error("an :of outside the layout shown", &with_editor("    (let* [g (ui/graph) h (ui/graph)] (ui/workspace (ui/split \"vertical\" h (ui/lisp :of g))))"), "E_PANEL_OF");
    let scope = flow_graph::projection::of_graph(&catalog(), &of_text(&with_editor(tied)).checked, "editor").expect("scope");
    let rows = match flow_graph::projection::find(&scope, &["editor".into(), "ia".into()]) {
        Some(n) => n.rows.iter().map(|r| r.label.clone()).collect::<Vec<_>>(),
        None => fail("no card ia"),
    };
    check(rows.iter().any(|r| r == "of"), "the :of keyword is not a row of the inspector's card");
    let retied = match workspace_doc::edit(&catalog(), &of_text(&with_editor(tied)), &Op::Connect { node: vec!["editor".into(), "ia".into()], key: ArgKey::Kw("of".into()), src: "b".into(), iter: false }) {
        Ok(ws) => shell_of(&build_ok(&ws)),
        Err(d) => fail(&rays_diagnostic::to_string(&d)),
    };
    check(retied.follows.iter().find(|(p, _)| *p == vec![1usize, 0]).map(|(_, t)| t.clone()) == Some(vec![0usize, 1]), "a wire to the of row did not tie the inspector to the other graph panel");
    let mut e = editor(&with_editor(tied));
    let mut count = 0i64;
    let drawn = Rc::new(Cell::new(0i64));
    let mut step = |e: &mut editor::Editor, buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>| {
        count += 1;
        drawn.set(0);
        let d = drawn.clone();
        let (next, _) = editor::update_with(e.clone(), &frame(buttons, mouse, events, count), &move |_| {
            d.set(d.get() + 1);
            Rc::new(())
        });
        *e = next;
    };
    let click = |step: &mut dyn FnMut(&mut editor::Editor, &[MouseButton], (f64, f64), Vec<Event>), e: &mut editor::Editor, p: (f64, f64)| {
        step(e, &[], p, vec![Event::PointerMoved(p)]);
        step(e, &[LB], p, vec![Event::PointerPressed(LB, p)]);
        step(e, &[], p, vec![Event::PointerReleased(LB, p)]);
        step(e, &[], p, vec![]);
    };
    step(&mut e, &[], (100., 100.), vec![]);
    step(&mut e, &[], (100., 100.), vec![]);
    check(drawn.get() == 2, "with nothing selected both inspectors show the sketch's rows");
    // a node selected in graph a: its inspector shows the node, b's still the sketch's rows
    let (x, y, w, h) = editor::node_box(&e, &["garden".into(), ":seed".into()]).expect("seed box");
    click(&mut step, &mut e, ((x + w / 2) as f64, (y + 1.max(h / 2)) as f64));
    check(dump_line(&e, "scope selected") == "garden/:seed" && drawn.get() == 1, &format!("the inspector tied to a did not follow a's selection: {}", dump_line(&e, "scope selected")));
    // graph b takes the focus: a's inspector keeps showing a's node
    click(&mut step, &mut e, (200., 600.));
    check(dump_line(&e, "pane graph") == "scene" && dump_line(&e, "scope selected") == "-" && drawn.get() == 1, "the inspector tied to a followed the focus to b");
    // a press in the tied inspector makes its graph panel the one in use
    click(&mut step, &mut e, (880., 300.));
    check(dump_line(&e, "pane graph") == "garden" && dump_line(&e, "scope selected") == "garden/:seed" && has(&dump_line(&e, "panels"), "inspector ia*"), "a press in a tied inspector did not bring its graph panel into use");
    editor::close(&e);
    // one binding used twice is two instances, keyed by place; the binding's saved entry is what each starts from
    let twice_body = "    (let* [g (ui/graph \"garden\")] (ui/workspace (ui/split \"horizontal\" g g)))";
    check(lowered(twice_body).repeated == vec!["g".to_string()], "a binding used twice is not marked");
    let mut r = Run::new(editor(&format!("{}(layout (panel [\"editor\" \"g\"] :collapsed false :window [10 30 430 400])\n        (panel [\"editor\" \"@panel\" \"1\"] :collapsed false :window [450 30 430 400]))", with_editor(twice_body))));
    r.step((700., 500.), vec![]);
    r.step((700., 500.), vec![]);
    check(r.dump("windows") == "graph 10 30 430 400, graph 450 30 430 400", &format!("the saved entries of a binding used twice: {}", r.dump("windows")));
    check(r.dump("panels") == "graph 0; graph 1", &format!("the leaves of one binding are not two instances: {}", r.dump("panels")));
    let (x, y, w, h) = editor::node_box(&r.e, &["garden".into(), ":seed".into()]).expect("seed box");
    r.click(((x + w / 2) as f64, (y + 1.max(h / 2)) as f64));
    let first = r.dump("scope selected");
    r.click((860., 410.));
    check(first != "-" && r.dump("scope selected") == "-" && r.dump("panels") == "graph 0; graph 1*", &format!("the second leaf shares the first one's selection: {first} / {}", r.dump("scope selected")));
    r.close();
    // a graph panel shows the view it says, with a lisp panel beside it
    let mut e = editor(&with_editor("    (ui/workspace (ui/split \"horizontal\" (ui/graph \"garden\" :view \"text\" :focus true) (ui/lisp)))"));
    e = editor::update(e, &frame(&[], (700., 500.), vec![], 1));
    e = editor::update(e, &frame(&[], (700., 500.), vec![], 2));
    check(dump_line(&e, "projection") == "text" && has(&dump_line(&e, "panels"), "graph 0*: selection tab"), &format!("a graph panel beside a lisp panel does not show its text view: {}", dump_line(&e, "panels")));
    editor::close(&e);
    // two lists and two outlines: each its own filter, focus row and search; a click focuses
    let mut r = Run::new(editor(&with_editor("    (let* [oa (ui/outline) ob (ui/outline) la (ui/list) lb (ui/list)]\n      (ui/workspace (ui/split \"horizontal\" (ui/split \"vertical\" oa ob)\n        (ui/split \"horizontal\" (ui/split \"vertical\" la lb) (ui/viewport (ref scene))))))")));
    r.step((800., 500.), vec![]);
    r.step((800., 500.), vec![]);
    let typed = |r: &mut Run, p: (f64, f64), text: &str| {
        r.click(p);
        r.step(p, vec![text_input(text)]);
        r.step(p, vec![key(Key::Enter)]);
        r.step(p, vec![]);
    };
    typed(&mut r, (100., 38.), "zz");
    check(has(&r.dump("panels"), "outline oa*: search \"zz\"; outline ob: search \"\""), &format!("the first outline's search: {}", r.dump("panels")));
    typed(&mut r, (100., 344.), "q");
    check(has(&r.dump("panels"), "outline oa: search \"zz\"; outline ob*: search \"q\""), &format!("a search typed in one outline shows in the other: {}", r.dump("panels")));
    r.click((550., 60.));
    r.step((550., 60.), vec![key(Key::KeyChar('s'))]);
    r.step((550., 60.), vec![text_input("g")]);
    r.step((550., 60.), vec![]);
    // the focused row is a node id, which depends on how many nodes were allocated before it
    let any_id = |r: &Run, line: &dyn Fn(&str) -> String| {
        let text = r.dump("panels");
        text.split("focus ").skip(1).filter_map(|rest| rest.split(|c: char| !c.is_ascii_digit()).next()).any(|id| !id.is_empty() && has(&text, &line(id)))
    };
    check(any_id(&r, &|id| format!("list la*: focus {id}, 0 folded, filter \"g\"; list lb: focus -, 0 folded, filter -")), &format!("the first list's focus row and filter: {}", r.dump("panels")));
    r.step((550., 60.), vec![key(Key::Enter)]);
    r.step((550., 60.), vec![]);
    r.click((550., 370.));
    check(any_id(&r, &|id| format!("list la: focus {id}, 0 folded, filter \"g\"; list lb*: focus {id}, 0 folded, filter -")), &format!("the second list does not keep its own filter: {}", r.dump("panels")));
    r.close();
}

pub fn run_host_scene_edit() {
    if !gpu() {
        return;
    }
    let e = editor("(workspace studio (graph g :context sop (sop/box)))");
    let value = match flow_syntax::syntax::parse("false") {
        Ok(v) if v.len() == 1 => v[0].clone(),
        _ => fail("bad literal"),
    };
    match editor::edit(e.clone(), &Op::SetArg { node: vec!["scene".into(), "camera1".into()], key: ArgKey::Kw("follow_viewport".into()), sub: vec![], value }) {
        Ok(e) => check(has(&source(&e), "(graph scene") && !has(&source(&e), ":follow_viewport true"), &format!("the host camera's edit did not reach the text: {}", source(&e))),
        Err(m) => fail(&format!("an edit of the host's camera was refused: {m}")),
    }
    editor::close(&e);
}

pub fn run_panel_chain() {
    if !gpu() {
        return;
    }
    let at = (200., 300.);
    let mut r = Run::new(editor("(workspace studio (graph g :context sop (sop/box)))"));
    for _ in 0..6 {
        r.step(at, vec![]);
    }
    r.step(at, vec![Event::PointerMoved(at)]);
    r.step(at, vec![Event::PointerPressed(LB, at), Event::PointerReleased(LB, at)]);
    let keys = |r: &mut Run, cs: &[char]| {
        for c in cs {
            r.step(at, vec![key(Key::KeyChar(if *c == ' ' { '/' } else { *c }))]);
            r.step(at, vec![]);
        }
        r.step(at, vec![]);
    };
    keys(&mut r, &[' ', 'o', 'v']);
    keys(&mut r, &[' ', 'l', 'u']);
    check(r.undo_label() == Some("Retype panel".into()) && has(&r.source(), "(ui/outline)"), &format!("a retype after splitting a viewport did nothing: {}", r.source()));
    keys(&mut r, &[' ', 'o', 'v']);
    check(r.undo_label() == Some("Split panel".into()) && has(&r.dump("panels"), "outline preview_a_a*"), &format!("a split after a retype did nothing: {}", r.dump("panels")));
    r.close();
}
