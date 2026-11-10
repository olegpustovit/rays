//! , part 1: run_lowering, run_editor, run_panel_keys, run_unbound_panels,
//! run_wire_gestures.
use super::support::*;
use editor_core::panels::{self, Panel, Size};
use editor_document::document::{Home, Origin};
use flow_graph::flow_edit::ArgKey;
use input::{Event, Key, MouseButton};
use rays_shell::layout;
use rays_editor::{editor, reduce};

pub fn run_lowering() {
    if !gpu() {
        return;
    }
    let t0 = std::time::Instant::now();
    let w = of_text(&case("variations"));
    eprintln!("of_text {:?}", t0.elapsed());
    let doc = build_ok(&w);
    eprintln!("lowered {:?}", t0.elapsed());
    let shell = shell_of(&doc);
    check(panels::to_string(&shell.tree) == "(h outline (h (v graph lisp) (tile view view view view)))", &format!("the variations shell: {}", panels::to_string(&shell.tree)));
    match &shell.tree {
        panels::Panels::Split { size: Size::Ratio(ratio), b, .. } => match &**b {
            panels::Panels::Split { size: Size::Ratio(inner), a, .. } => match &**a {
                panels::Panels::Split { size: Size::Ratio(left), .. } => check(*ratio == 0.13 && *inner == 0.5 && *left == 0.58, "ratios come from the editor graph"),
                _ => fail("root split"),
            },
            _ => fail("root split"),
        },
        _ => fail("root split"),
    }
    // four viewports: the default scene instance, and one scene instance for each override
    let keys: Vec<String> = panels::leaves(&shell.tree).into_iter().filter_map(|(_, p)| if let Panel::View(k) = p { Some(k) } else { None }).collect();
    check(keys == ["v1.1.0", "v1.1.1", "v1.1.2", "v1.1.3"], &format!("viewport keys are tile indices: {}", keys.join(",")));
    check(shell.views.iter().map(|(k, _)| k.as_str()).collect::<Vec<_>>() == ["v1.1.1", "v1.1.2", "v1.1.3"] && shell.views.iter().all(|(_, ids)| ids.len() == 1), "the default instance is the primary scene; each override has an object of its own");
    let mut ids: Vec<i64> = shell.views.iter().flat_map(|(_, ids)| ids.clone()).collect();
    ids.sort();
    ids.dedup();
    check(ids.len() == 3, "the objects of the overrides are distinct");
    // origins: named panels can be edited, looped ones say where they come from (E1)
    let origin = |path: &[usize]| shell.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o.clone());
    check(
        origin(&[0]) == Some(Origin::Bound("outline".into()))
            && origin(&[1, 0, 0]) == Some(Origin::Bound("network".into()))
            && origin(&[1, 0]) == Some(Origin::Bound("left".into()))
            && origin(&[]) == Some(Origin::Bound("panels".into())),
        "named panels keep their binding",
    );
    check(
        origin(&[1, 1, 2]) == Some(Origin::Loop(Home::BoundAt(vec!["editor".into(), "sheet".into()]), ArgKey::Pos(0))) && origin(&[1, 1]) == Some(Origin::Bound("sheet".into())),
        "a panel made by a loop names the loop",
    );
    // a count change keeps the first panels' keys
    let more = build_ok(&of_text(&replace(&case("variations"), "(range 4)", "(range 6)")));
    let keys6: Vec<String> = panels::leaves(&shell_of(&more).tree).into_iter().filter_map(|(_, p)| if let Panel::View(k) = p { Some(k) } else { None }).collect();
    check(keys6[..4] == keys[..] && keys6.len() == 6, "panel keys follow the iteration index");
    check(editor_document::document::scene_graph(&more).len() > 4, "more objects for more viewports");
    // the default shell of a workspace without an editor graph, and a named graph panel
    let plain = build_ok(&of_text(&case("tree")));
    check(plain.shell.is_none(), "no editor graph, no shell");
    let named = build_ok(&of_text(&with_editor("    (ui/workspace (ui/split \"vertical\" (ui/graph \"scene\") (ui/viewport (ref scene))))")));
    check(shell_of(&named).named == vec![(vec![0usize], "scene".to_string())], "ui/graph names its graph");
    check(panels::to_string(&shell_of(&named).tree) == "(v graph view)", "ui/split is an even split");
    match shell_of(&named).tree {
        panels::Panels::Split { size: Size::Ratio(ratio), .. } => check(ratio == 0.5, "ui/split halves"),
        _ => fail("split"),
    }
    // negative: what the checker or the evaluator refuses leaves no layout
    expect_error("a split axis", &with_editor("    (ui/workspace (ui/split \"diagonal\" (ui/graph) (ui/lisp)))"), "axis");
    expect_error("a computed ratio out of range", &with_editor("    (let* [r (* 3 0.5)] (ui/workspace (ui/split_at \"horizontal\" r (ui/graph) (ui/lisp))))"), "0.1");
    expect_error("an empty tile", &with_editor("    (ui/workspace (ui/tile (for [i (range 0)] (ui/graph))))"), "tile");
    // a selected layout that names no editor graph is dropped, not an error (a removed graph)
    check(of_text(&format!("{}\n(layout (editor \"missing\"))", with_editor("    (ui/workspace (ui/graph))"))).layout.editor.is_none(), "a dangling editor layout was kept");
}

pub fn run_frame_key() {
    if !gpu() {
        return;
    }
    let mut e = editor(&case("rosette"));
    for n in 1..=24 {
        let events = match n {
            4 => {
                let p = (900., 300.);
                vec![Event::PointerMoved(p), Event::PointerPressed(MouseButton::LeftButton, p), Event::PointerReleased(MouseButton::LeftButton, p)]
            }
            6 => vec![key(Key::ArrowDown)],
            8 => vec![key(Key::KeyChar('i'))],
            16 => vec![key(Key::ArrowRight)],
            18 => vec![key(Key::Shift), key(Key::KeyChar('g'))],
            _ => Vec::new(),
        };
        let mut at = frame_full(&[], if n == 18 { &[Key::Shift] } else { &[] }, (0., 0.), (640., 360.), events, n);
        at.input.size = (1400, 800);
        e = editor::update(e, &at);
    }
    check(editor::undo_label(&e).as_deref() == Some("Frame"), "Shift-G makes one Frame history entry");
    check(!editor::workspace(&e).layout.frames.is_empty(), "the frame is in the layout");
    editor::close(&e);
}

pub fn run_editor() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(&case("variations")));
    r.step((450., 300.), vec![]);
    r.step((450., 300.), vec![]);
    // the editor awaits each cook, so after a frame nothing is cooking: a check, not a wait
    let settle = |r: &Run| {
        let line = r.dump("cook");
        check(!(line.starts_with("Cooking") || line.starts_with("Waiting") || has(&line, "skipping frames")), &format!("the editor is still cooking: {line}"));
    };
    settle(&r);
    let drag = |r: &mut Run, from: (f64, f64), to: (f64, f64)| {
        r.step(from, vec![Event::PointerMoved(from)]);
        r.stepb(&[MouseButton::LeftButton], from, vec![Event::PointerPressed(MouseButton::LeftButton, from)]);
        r.stepb(&[MouseButton::LeftButton], to, vec![Event::PointerMoved(to)]);
        r.step(to, vec![Event::PointerReleased(MouseButton::LeftButton, to)]);
        r.step((450., 300.), vec![]);
    };
    // drag the outline | rest gutter: one rewrite of the editor graph, one history entry.
    // At 900 points the outline's 120-point minimum lifts its 13% to 169.
    drag(&mut r, (169.5, 300.), (300., 300.));
    check(has(&r.source(), "(ui/split_at \"horizontal\" 0.3333 outline right)"), &format!("the drag rewrote the outline split: {}", r.source()));
    check(r.undo_label() == Some("Resize panel".into()), "one history entry named Resize panel");
    // the header menu of the graph panel: split side by side
    r.click_button(MouseButton::RightButton, (400., 10.));
    r.step((450., 300.), vec![]);
    r.click((420., 28.));
    r.step((450., 300.), vec![]);
    check(r.undo_label() == Some("Split panel".into()), &format!("the menu split the graph panel: {:?}", r.undo_label()));
    check(has(&r.source(), "network_a") && has(&r.source(), "network_b"), "the split bound two new panels");
    // a panel made by a loop cannot be split: the status says where it comes from
    let geometry = layout::geometry(None, Some(&[Panel::Timeline]), &shell_of(&build_ok(&editor::workspace(&r.e))).tree, &frame(&[], (0., 0.), vec![], 0));
    let leaf = layout::find(&geometry, &Panel::View("v1.1.2".into())).expect("leaf");
    let (hx, hy, hw, _) = leaf.header;
    let depth = r.undo_label();
    r.click_button(MouseButton::RightButton, (hx as f64 + (hw / 2) as f64, hy as f64 + 10.));
    r.step((450., 300.), vec![]);
    r.click(((hx + hw / 2 + 20) as f64, (hy + 10 + 18) as f64));
    r.step((450., 300.), vec![]);
    check(r.undo_label() == depth, "a looped panel did not change the graph");
    settle(&r);
    check(has(&r.dump("cook"), "made by a loop in sheet"), &format!("the status names the loop: {} / {}", r.dump("cook"), r.dump("edit error")));
    // focus is a panel: a press in a panel focuses it
    let focus_after = |r: &mut Run, p: (f64, f64)| {
        r.click(p);
        r.step((450., 300.), vec![]);
        r.dump("focus")
    };
    check(focus_after(&mut r, (30., 300.)) == "Outline", "a press in the outline focuses it");
    check(focus_after(&mut r, ((hx + 30) as f64, (hy + 100) as f64)) == "View", "a press in a viewport focuses a viewport");
    r.close();
}

pub fn run_panel_keys() {
    if !gpu() {
        return;
    }
    let started = || {
        let mut r = Run::new(editor(&case("variations")));
        r.step((300., 150.), vec![]);
        r.step((300., 150.), vec![]);
        // a press in the graph panel focuses it
        r.step((300., 150.), vec![Event::PointerMoved((300., 150.))]);
        r.step((300., 150.), vec![Event::PointerPressed(MouseButton::LeftButton, (300., 150.))]);
        r.step((300., 150.), vec![Event::PointerReleased(MouseButton::LeftButton, (300., 150.))]);
        r.step((300., 150.), vec![]);
        check(r.dump("focus") == "Graph", &format!("focus is the graph panel: {}", r.dump("focus")));
        r
    };
    let slash = || key(Key::KeyChar('/'));
    let mut r = started();
    let m = (300., 150.);
    r.step(m, vec![slash(), ch('o'), ch('v')]);
    r.step(m, vec![]);
    check(r.undo_label() == Some("Split panel".into()) && has(&r.source(), "network_a"), "/ o v split the focused panel");
    r.step(m, vec![slash(), ch('o'), ch('x')]);
    r.step(m, vec![]);
    check(r.undo_label() == Some("Close panel".into()), "/ o x closed it again");
    r.close();
    // a document without an editor graph gets one written from its layout before the retype
    let mut r = Run::new(editor("(workspace w (graph g :context sop (sop/box)))"));
    let m = (600., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let (gx, gy, _, _) = r.panes().graph;
    let p = ((gx + 40) as f64, (gy + 40) as f64);
    r.step(p, vec![Event::PointerMoved(p)]);
    r.step(p, vec![Event::PointerPressed(MouseButton::LeftButton, p)]);
    r.step(p, vec![Event::PointerReleased(MouseButton::LeftButton, p)]);
    r.step(p, vec![]);
    check(!has(&r.source(), "(graph editor"), "the plain workspace has no editor graph");
    r.step(p, vec![slash(), ch('l'), ch('i')]);
    r.step(p, vec![]);
    check(
        has(&r.source(), "(graph editor :context editor") && has(&r.source(), "network (ui/inspector)") && has(&r.source(), "preview (ui/viewport (ref scene))") && has(&r.source(), "(graph scene :context scene") && r.undo_label() == Some("Retype panel".into()),
        &format!("retyping wrote the editor graph (and the scene it views) first: {}", r.source()),
    );
    r.close();
    // the sketch reads the pointer only on its picture
    let two = with_editor("    (let* [a (ui/graph) b (ui/viewport (ref scene)) both (ui/split_at \"horizontal\" 0.5 a b)] (ui/workspace both))");
    let mut r = Run::new(editor(&two));
    let held = [MouseButton::LeftButton];
    let (panel, picture) = ((200., 300.), (700., 300.));
    let live = |r: &Run| editor::private::live_frame(&r.e);
    r.step(picture, vec![]);
    r.step(picture, vec![Event::PointerMoved(picture)]);
    check(live(&r).pointer == picture, "the picture has the pointer");
    r.step(panel, vec![Event::PointerMoved(panel)]);
    r.stepb(&held, panel, vec![Event::PointerPressed(MouseButton::LeftButton, panel)]);
    check(live(&r).pointer == picture && live(&r).buttons.is_empty() && live(&r).events.is_empty(), "a press on a panel reached the sketch");
    r.stepb(&held, picture, vec![Event::PointerMoved(picture)]);
    check(live(&r).buttons.is_empty(), "a drag begun on a panel became the sketch's over the picture");
    r.step(picture, vec![Event::PointerReleased(MouseButton::LeftButton, picture)]);
    r.step(picture, vec![]);
    r.stepb(&held, picture, vec![Event::PointerPressed(MouseButton::LeftButton, picture)]);
    check(live(&r).buttons == [MouseButton::LeftButton] && !live(&r).events.is_empty(), "a press on the picture did not reach the sketch");
    r.stepb(&held, panel, vec![Event::PointerMoved(panel)]);
    check(live(&r).buttons == [MouseButton::LeftButton] && live(&r).pointer == panel, "a drag begun on the picture was cut at its edge");
    r.step(panel, vec![Event::PointerReleased(MouseButton::LeftButton, panel)]);
    check(!live(&r).events.is_empty(), "the sketch missed the release of its own drag");
    r.step(panel, vec![]);
    check(live(&r).pointer == panel && live(&r).buttons.is_empty(), "the pointer left the picture where the drag ended");
    r.close();
    // the sketch has the keyboard only while its picture is focused
    let mut r = Run::new(editor(&two));
    let esc = [Key::Escape];
    let click = |r: &mut Run, keys: &[Key], at: (f64, f64)| {
        r.step_full(&[], keys, at, vec![Event::PointerPressed(MouseButton::LeftButton, at)]);
        r.step_full(&[], keys, at, vec![Event::PointerReleased(MouseButton::LeftButton, at)]);
        r.step_full(&[], keys, at, vec![]);
    };
    let seen = |r: &Run| editor::private::live_frame(&r.e).keys;
    click(&mut r, &[], picture);
    r.step_full(&[], &esc, picture, vec![]);
    check(seen(&r) == [Key::Escape], "the focused picture did not get a held Esc");
    click(&mut r, &esc, panel);
    r.step_full(&[], &esc, panel, vec![]);
    check(seen(&r).is_empty(), "a panel's Esc reached the sketch");
    click(&mut r, &esc, picture);
    r.step_full(&[], &esc, picture, vec![]);
    check(seen(&r).is_empty(), "an Esc held from the panel reached the sketch after the focus changed");
    r.step(picture, vec![]);
    r.step_full(&[], &esc, picture, vec![]);
    check(seen(&r) == [Key::Escape], "a fresh Esc was hidden from the focused picture");
    r.close();
}

pub fn run_unbound_panels() {
    if !gpu() {
        return;
    }
    let started = |text: &str, at: (f64, f64)| {
        let mut r = Run::new(editor(text));
        r.step(at, vec![]);
        r.step(at, vec![]);
        r.step(at, vec![Event::PointerMoved(at)]);
        r.step(at, vec![Event::PointerPressed(MouseButton::LeftButton, at)]);
        r.step(at, vec![Event::PointerReleased(MouseButton::LeftButton, at)]);
        r.step(at, vec![]);
        r
    };
    let slash = || key(Key::KeyChar('/'));
    let inline = with_editor("    (ui/workspace (ui/split_at \"vertical\" 0.5 (ui/graph) (ui/viewport (ref scene))))");
    let at = (300., 100.);
    let mut r = started(&inline, at);
    check(r.dump("focus") == "Graph", &format!("focus is the inline graph panel: {}", r.dump("focus")));
    r.step(at, vec![slash(), ch('l'), ch('i')]);
    r.step(at, vec![]);
    check(r.undo_label() == Some("Retype panel".into()) && has(&r.source(), "(ui/inspector)") && has(&r.source(), "(ui/viewport (ref scene))"), &format!("an inline panel was retyped: {:?}\n{}", r.undo_label(), r.source()));
    r.close();
    let mut r = started(&inline, at);
    r.step(at, vec![slash(), ch('o'), ch('v')]);
    r.step(at, vec![]);
    check((r.undo_label() == Some("Split panel".into()) && has(&r.source(), "graph_a")) || has(&r.source(), "ui/split_at \"horizontal\" 0.5"), &format!("an inline panel was split: {:?}\n{}", r.undo_label(), r.source()));
    r.close();
    // a loop's panels: retyping edits the template, splitting them says why it cannot
    let at = (800., 100.);
    let mut r = started(&case("variations"), at);
    check(r.dump("focus") == "View", "focus is a looped viewport");
    r.step(at, vec![slash(), ch('o'), ch('x')]);
    r.step(at, vec![]);
    check(r.undo_label().is_none() && has(&r.dump("cook"), "copies made by a loop"), &format!("closing a looped panel said why not: {:?} / {}", r.undo_label(), r.dump("cook")));
    r.step(at, vec![slash(), ch('l'), ch('l')]);
    r.step(at, vec![]);
    check(r.undo_label() == Some("Retype panel".into()) && has(&r.source(), "(ui/list)"), &format!("retyping a looped panel edits its template: {:?}", r.undo_label()));
    r.close();
}

pub fn run_wire_gestures() {
    if !gpu() {
        return;
    }
    let started = |text: &str| {
        let mut r = Run::new(editor(text));
        r.step((450., 300.), vec![]);
        r.step((450., 300.), vec![]);
        r
    };
    let pick = |r: &mut Run, text: &str| {
        let m = (450., 300.);
        r.step(m, vec![key(Key::KeyChar('/')), ch('a')]);
        r.step(m, vec![text_input(text)]);
        r.step(m, vec![key(Key::Enter)]);
        r.step(m, vec![]);
    };
    let nested = "(workspace w (graph g :context sop (let* [m (sop/transform (sop/box))] m)))";
    let mut r = started(nested);
    r.e = reduce::select_path(r.e.clone(), &["g".into(), "m#0".into()]);
    pick(&mut r, "transform");
    check(
        r.undo_label() == Some("Add node".into()) && !has(&r.source(), "(sop/transform (sop/box))") && has(&r.source(), "(sop/box)") && has(&r.source(), "(sop/transform box)"),
        &format!("a node after a nested node: {}", r.source()),
    );
    r.count += 1;
    let undo_frame = frame_full(&[], &[Key::Meta], (0., 0.), (450., 300.), vec![ch('z')], r.count);
    r.e = editor::update(r.e.clone(), &undo_frame);
    check(has(&r.source(), "(sop/transform (sop/box))") && r.undo_label().is_none(), &format!("one undo took the name and the node back: {}", r.source()));
    r.close();
    let mut r = started("(workspace w (graph g :context sop (let* [a (sop/box) m (sop/transform a)] m)))");
    let (gx, gy, gw, gh) = r.panes().graph;
    let p = ((gx + 40) as f64, (gy + 40) as f64);
    r.step(p, vec![Event::PointerMoved(p)]);
    r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerPressed(MouseButton::LeftButton, p)]);
    r.step(p, vec![Event::PointerReleased(MouseButton::LeftButton, p)]);
    r.step(p, vec![key(Key::Home)]);
    r.step(p, vec![ch('i')]);
    r.step(p, vec![]);
    check(editor::level(&r.e) == Some("g".into()), "the graph is open");
    let bx = |r: &Run, path: &[&str]| editor::node_box(&r.e, &path.iter().map(|s| flow_syntax::symbol::Symbol::from(*s)).collect::<Vec<_>>()).expect("node box");
    let (ax, ay, aw, _) = bx(&r, &["g", "a"]);
    let (mx, my, _, _) = bx(&r, &["g", "m"]);
    // the wire runs from a's out-port to m's header in-port
    let z = (mx - ax) as f64 / 288.;
    let from = ((ax + aw) as f64, ay as f64 + 11.75 * z);
    let into = (mx as f64, my as f64 + 11.75 * z);
    let mid = ((from.0 + into.0) / 2., (from.1 + into.1) / 2.);
    let empty = ((gx + gw - 30) as f64, (gy + gh - 30) as f64);
    let held = [MouseButton::LeftButton];
    r.step(mid, vec![Event::PointerMoved(mid)]);
    r.stepb(&held, mid, vec![Event::PointerPressed(MouseButton::LeftButton, mid)]);
    r.stepb(&held, empty, vec![Event::PointerMoved(empty)]);
    r.step(empty, vec![Event::PointerReleased(MouseButton::LeftButton, empty)]);
    r.step(empty, vec![]);
    r.step(empty, vec![text_input("transform")]);
    r.step(empty, vec![key(Key::Enter)]);
    r.step(empty, vec![]);
    check(r.undo_label() == Some("Insert node".into()) && has(&r.source(), "(sop/transform a)") && has(&r.source(), "(sop/transform transform)"), &format!("a node inserted on a pulled wire: {}", r.source()));
    r.close();
}
