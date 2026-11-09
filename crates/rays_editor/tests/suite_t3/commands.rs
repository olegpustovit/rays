//! Port of lib/rays_editor/test/test_editor_commands.ml.
use super::t3c::*;
use editor_core::command;
use editor_core::guide_context::GuideContext as Ctx;
use editor_core::keymap::{self, Trigger};
use editor_core::panels::Panel;
use editor_core::router;
use flow_graph::flow_edit as fe;
use input::{Event, Key, MouseButton, Platform};
use rays_editor::editor_commands as l;
use rays_editor::{editor, reduce};

/// The widths and heights of the images a scene draws (render_ir `Image` destinations), read from
/// the command's Debug text: this crate does not depend on scene_command.
pub fn image_sizes(scene: &[rays::scene::node]) -> Vec<(f64, f64)> {
    let commands = rays::scene::layer::commands(None, scene).expect("scene commands");
    let mut out = Vec::new();
    for c in commands {
        let text = format!("{c:?}");
        if let Some(rest) = text.strip_prefix("Image(") {
            let at = rest.find("destination").expect("destination");
            let dest = &rest[at..];
            let num = |key: &str| -> f64 {
                let i = dest.find(key).unwrap_or_else(|| panic!("{key} in {dest}")) + key.len();
                let tail = &dest[i..];
                let end = tail.find([',', ' ', '}']).unwrap_or(tail.len());
                tail[..end].parse().unwrap_or_else(|_| panic!("number in {tail}"))
            };
            out.push((num("width: "), num("height: ")));
        }
    }
    out
}

fn widths(scene: &[rays::scene::node]) -> Vec<f64> {
    image_sizes(scene).into_iter().map(|(w, _)| w).collect()
}

pub fn path(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|s| s.to_string()).collect()
}

/// v on a drawing or image node shows it in the canvas panes; v again restores their pictures
pub fn canvas_preview() {
    let text = r##"(workspace v
    (graph image :context image
      (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 64 :height 8))
    (graph picture :context draw
      (draw/merge (draw/background "#0b0b0e") (draw/image (ref image) :at [0 0 0])))
    (graph settings :context settings (settings/config :title "v" :width 900 :height 640))
    (graph editor :context editor
      (let* [preview (ui/canvas (ref picture))
             network (ui/graph "image" :focus true)]
        (ui/workspace (ui/split "vertical" preview network :second_size 264)))))"##;
    let mut d = Driver::new(open(text));
    for _ in 0..3 {
        d.step(vec![]);
    }
    let shown = |d: &Driver| -> Vec<Vec<f64>> { editor::private::canvas_scenes(d.e()).iter().map(|s| widths(s)).collect() };
    let default = shown(&d);
    assert!(!default.is_empty() && default.iter().all(|w| w == &vec![64.]), "the default picture is not the 64 point image: {default:?}");
    d.map(|e| reduce::select_path(e, &path(&["image", "@result"])));
    d.step(vec![]);
    d.map(|e| reduce::view(e, &path(&["image", "@result"])));
    d.step(vec![]);
    d.step(vec![]);
    let now = shown(&d);
    assert!(now.len() == default.len() && now.iter().all(|w| w.len() == 1 && w[0] > 64.), "v on an image node did not show exactly its fitted image in the canvas pane: {now:?}");
    d.map(|e| reduce::view(e, &path(&["image", "@result"])));
    d.step(vec![]);
    d.step(vec![]);
    assert!(shown(&d) == default, "v again did not restore the canvas pane's own picture");
    d.map(|e| reduce::select_path(e, &path(&["picture", "@result#0"])));
    d.step(vec![]);
    d.map(|e| reduce::view(e, &path(&["picture", "@result#0"])));
    d.step(vec![]);
    assert!(shown(&d).iter().all(|w| w.is_empty()), "View on an inline background did not replace the canvas picture");
    d.map(|e| reduce::view(e, &path(&["picture", "@result#0"])));
    d.step(vec![]);
    assert!(shown(&d) == default, "View on the inline background again did not restore the picture");
    d.close();
}

/// frame/width and frame/height in a canvas pane are the pane's size, not the window's
pub fn canvas_frame_size() {
    let text = r#"(workspace f
    (graph picture :context draw
      (draw/image (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width (frame/width) :height (frame/height))
        :at [0 0 0]))
    (graph settings :context settings (settings/config :title "f" :width 900 :height 640))
    (graph editor :context editor
      (let* [preview (ui/canvas (ref picture))
             network (ui/graph "picture" :focus true)]
        (ui/workspace (ui/split "horizontal" preview network :second_size 500)))))"#;
    let mut d = Driver::new(open(text));
    for _ in 0..3 {
        d.step(vec![]);
    }
    let sizes: Vec<(f64, f64)> = editor::private::canvas_scenes(d.e()).iter().flat_map(|s| image_sizes(s)).collect();
    assert!(matches!(sizes.as_slice(), [(w, h)] if *w > 0. && *w < 500. && *h > 0. && *h < 640.), "frame/width and frame/height in a canvas pane are not the pane's size: {sizes:?}");
    d.close();
}

/// every language form of the add menu starts as an expression that checks
pub fn menu_forms() {
    let forms = editor::private::menu_forms();
    for form in ["if", "cond", "case", "for", "fold", "scan", "sum", "fn", "map", "filter", "reduce", "sort-by", "list", "record", "let*", "state"] {
        assert!(forms.iter().any(|(f, _)| f == form), "the add menu has no {form}");
    }
    for (form, text) in &forms {
        let source = format!("(workspace m (graph g :context value (let* [added {text}] 0)))");
        if let Err(ds) = rays_editor::workspace::load(None, Vec::new(), None, &source) {
            panic!("{form}: {}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; "));
        }
    }
}

pub fn world_command() {
    with_editor(
        r#"(workspace world_command
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g))))))"#,
        |editor| {
            let editor = editor::update(editor, &frame_at((450., 300.), vec![], 0));
            let before = editor::workspace(&editor);
            let step = |editor: editor::Editor, actions: &[l::action], count: i64| reduce::step(editor, None, None, actions, &frame_at((450., 300.), vec![], count)).expect("step");
            let created = step(editor, &[l::action::Go_world], 1);
            let source = text_of(&created);
            assert!(
                has(&source, "(scene/world (ref sky))") && has(&source, "(graph sky :context world") && has(&source, "(world/sun (world/sky))") && editor::undo_label(&created) == Some("Add World".to_string()),
                "/ e did not write the World and its graph as one Add World entry: {source}"
            );
            let repeated = step(created.clone(), &[l::action::Go_world], 2);
            assert!(same_ws(&editor::workspace(&repeated), &editor::workspace(&created)), "opening an existing World rewrote the document");
            let undone = step(repeated, &[l::action::Undo], 3);
            assert!(same_ws(&editor::workspace(&undone), &before), "one undo did not restore the World-free document");
        },
    );
}

/// A Shift-drag in the view rotates the World: the press latches the operation, the moves edit the
/// text, the release seals; the whole drag is one undo step.
pub fn world_drag() {
    let mut d = Driver::new(open(
        r#"(workspace world_drag
    (graph g :context sop (sop/box))
    (graph sky :context world (world/sun (world/sky)))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g)) (scene/world (ref sky))))))"#,
    ));
    d.step_at((450., 300.), vec![]);
    d.count = 0;
    let before = d.text();
    let (x, y, w, h) = editor::panes(d.e(), &frame_at((450., 300.), vec![], 0)).view;
    let p = ((x + w / 2) as f64, (y + h / 2) as f64);
    let at = |dx: f64| (p.0 + dx, p.1);
    let shift = [Key::Shift];
    let left = [MouseButton::LeftButton];
    d.step_full(&[], &shift, at(0.), vec![Event::Pointer_moved(at(0.))], 1);
    d.step_full(&left, &shift, at(0.), vec![Event::Pointer_pressed(MouseButton::LeftButton, at(0.))], 1);
    d.step_full(&left, &shift, at(20.), vec![Event::Pointer_moved(at(20.))], 1);
    d.step_full(&left, &shift, at(40.), vec![Event::Pointer_moved(at(40.))], 1);
    d.step_full(&[], &shift, at(40.), vec![Event::Pointer_released(MouseButton::LeftButton, at(40.))], 1);
    assert!(d.text() != before, "a Shift-drag in the view did not rotate the World");
    assert!(editor::undo_label(d.e()) == Some("Rotate World".to_string()), "the drag's undo entry is not Rotate World: {:?}", editor::undo_label(d.e()));
    let undone = reduce::step(d.e().clone(), None, None, &[l::action::Undo], &frame_at((450., 300.), vec![], d.count + 1)).expect("undo");
    assert!(text_of(&undone) == before, "one undo did not undo the whole World drag");
    d.close();
}

/// an image-context workspace is not a geometry object: the cook has nothing to reject
pub fn image_cook() {
    let text = r#"(workspace k
    (graph image :context image
      (let* [bias (* 0.2 (+ 1 (sin t)))
             pixels (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 16 :height 16)]
        pixels))
    (graph picture :context draw (draw/image (ref image) :at [0 0 0]))
    (graph settings :context settings (settings/config :title "k" :width 900 :height 600)))"#;
    with_editor(text, |mut e| {
        for count in 1..=2 {
            e = editor::update(e, &frame_at((450., 300.), vec![], count));
        }
        let status = editor::private::status_text(&e);
        assert!(!has(&status, "Cook rejected") && !has(&status, "E_PAYLOAD"), "an image graph made the cook reject: {status}");
    });
}

/// New graph: each context's default body checks, a scene reads the SOP graph, and Delete refuses a
/// graph that is read
pub fn new_graphs() {
    let text = r#"(workspace n
    (graph settings :context settings (settings/config :title "n" :width 900 :height 600)))"#;
    let contexts = ["sop", "scene", "draw", "image", "value", "material"];
    with_editor(text, |editor| {
        let names = |e: &editor::Editor| -> Vec<String> { editor::workspace(e).checked.graphs.iter().map(|g| g.name.clone()).collect() };
        let add = |e: editor::Editor, context: &str| -> editor::Editor {
            let op = editor::private::new_graph(&e, context);
            editor::edit(e, &op).unwrap_or_else(|m| panic!("New graph {context} refused: {m}"))
        };
        let mut e = editor;
        for c in contexts {
            e = add(e, c);
        }
        for n in contexts {
            assert!(names(&e).iter().any(|x| x == n), "New graph did not make {n}");
        }
        assert!(names(&add(e.clone(), "sop")).iter().any(|x| x == "sop_2"), "a second New graph sop is not named afresh");
        let source = flow_syntax::lisp::print(&editor::workspace(&e).source).unwrap().0;
        assert!(has(&source, "(ref sop)"), "the new scene does not read the SOP graph");
        for count in 1..=3 {
            e = editor::update(e, &frame_at((450., 300.), vec![], count));
        }
        assert!(!has(&editor::private::status_text(&e), "rejected"), "the new graphs do not cook: {}", editor::private::status_text(&e));
        match editor::edit(e.clone(), &fe::Op::RemoveGraph { name: "sop".into() }) {
            Ok(_) => panic!("a read SOP graph was deleted"),
            Err(m) => assert!(has(&m, "scene"), "the refusal does not name the scene: {m}"),
        }
        match editor::edit(e.clone(), &fe::Op::RemoveGraph { name: "draw".into() }) {
            Ok(removed) => assert!(!names(&removed).iter().any(|x| x == "draw"), "an unread graph stayed"),
            Err(m) => panic!("an unread graph was refused: {m}"),
        }
    });
}

/// Every chord the key sheet lists as the text area's own must change the text, selection or caret
/// of a real `text_area_submit` with the editor's Lisp language.
pub fn text_keys_are_handled() {
    use std::cell::RefCell;
    use std::rc::Rc;
    let language = rays_editor::lisp_text::language(None, None, None, false, pxui::theme::default());
    let keys = l::text_keys();
    assert!(!keys.is_empty(), "no text keys");
    for (label, trigger) in keys {
        let Trigger::Chord(k, mods) = trigger else { panic!("text key without a chord") };
        let mut ui = pxui::ui::Ui::create(Platform::Apple, Some(11), None);
        let text = Rc::new(RefCell::new("(a (b c))\n(d (e))".to_string()));
        let sel = Rc::new(RefCell::new((0i64, 0i64)));
        let caret = Rc::new(RefCell::new(0i64));
        let mut step = |keys: &[Key], events: Vec<Event>| {
            let mut f = frame_at((0., 0.), events, 0);
            f.input.keys = keys.to_vec();
            let (text, sel, caret, language) = (text.clone(), sel.clone(), caret.clone(), language.clone());
            ui.frame(&f, &mut |ui: &mut pxui::ui::Ui| {
                let (sel2, caret2) = (sel.clone(), caret.clone());
                let o = pxui::ui_text::TextAreaOpts {
                    language: Some(language.clone()),
                    on_select: Some(Rc::new(move |r| *sel2.borrow_mut() = r)),
                    on_caret: Some(Rc::new(move |c| *caret2.borrow_mut() = c)),
                    ..Default::default()
                };
                let current = text.borrow().clone();
                let (t, _) = ui.text_area_submit((0., 0.), 300., 96., &o, "area", &current);
                *text.borrow_mut() = t;
            });
        };
        step(&[], vec![]);
        step(&[], vec![Event::Pointer_pressed(MouseButton::LeftButton, (60., 31.)), Event::Pointer_released(MouseButton::LeftButton, (60., 31.))]);
        let before = (text.borrow().clone(), *sel.borrow(), *caret.borrow());
        step(&mods, vec![Event::Key_pressed(k)]);
        let after = (text.borrow().clone(), *sel.borrow(), *caret.borrow());
        assert!(before != after, "key sheet lists {label} but the text area ignores it");
        ui.destroy();
    }
}

fn router_actions(focus: &Panel, events: Vec<Event>) -> Vec<l::action> {
    let keymap = l::keymap();
    let refs: Vec<&l::Command> = keymap.iter().collect();
    let (_, actions, _) = router::step(
        &[],
        Platform::Apple,
        &refs,
        focus,
        false,
        &frame_at((500., 400.), events, 1),
        router::State::Idle,
    );
    actions.iter().map(|c| l::action_of(c).clone()).collect()
}

pub fn keys_and_guide() {
    use pxui_graph::scope_commands as scope;
    text_keys_are_handled();
    for focus in [Panel::View(String::new()), Panel::View("v1.0".into()), Panel::Graph, Panel::List, Panel::Lisp, Panel::Inspector, Panel::Outline, Panel::Timeline] {
        for (events, expected) in [
            (vec![key(Key::Shift), ch('/')], l::action::Guide_toggle),
            (vec![ch('?')], l::action::Guide_toggle),
            (vec![key(Key::KeyChar('/')), ch('?')], l::action::Guide_keys),
        ] {
            assert!(router_actions(&focus, events) == vec![expected], "guide key failed outside or inside graph focus ({focus:?})");
        }
    }
    // Tab is the graph's own key for the add menu, and no other pane's
    for (focus, expected) in [(Panel::Graph, vec![l::action::Add_node]), (Panel::View(String::new()), vec![]), (Panel::Outline, vec![]), (Panel::Inspector, vec![])] {
        assert!(router_actions(&focus, vec![key(Key::Tab)]) == expected, "Tab is not the graph's add key alone ({focus:?})");
    }
    assert!(
        keymap::label(Platform::Apple, &Trigger::Chord(Key::KeyChar('/'), vec![Key::Shift])) == "?"
            && keymap::label(Platform::Apple, &Trigger::Chord(Key::ArrowLeft, vec![])) == "←"
            && keymap::label(Platform::Apple, &Trigger::Chord(Key::KeyChar('z'), vec![Key::Shift, Key::Meta])) == "⌘⇧Z",
        "shared key labels lost a modifier or alias"
    );
    let bindings = scope::bindings();
    let cmds: Vec<command::Command> = bindings.iter().map(|(c, _)| c.clone()).collect();
    let ids = |ctx: Ctx| -> Vec<String> { command::for_guide(&cmds, &Panel::Graph, ctx).into_iter().map(|c| c.id).collect() };
    let mem = |v: &Vec<String>, s: &str| v.iter().any(|x| x == s);
    let (canvas, node, multi) = (ids(Ctx::Canvas), ids(Ctx::Node), ids(Ctx::Multi));
    assert!(
        mem(&canvas, "scope.walk.left")
            && mem(&canvas, "scope.frame-all")
            && !mem(&canvas, "scope.delete")
            && !mem(&canvas, "scope.duplicate")
            && mem(&node, "scope.delete")
            && mem(&node, "scope.rename")
            && mem(&node, "scope.display")
            && mem(&node, "scope.fold")
            && mem(&multi, "scope.duplicate")
            && mem(&multi, "scope.repeat")
            && !mem(&multi, "scope.display")
            && !mem(&multi, "scope.fold")
            && ids(Ctx::Search).is_empty()
            && ids(Ctx::Text).is_empty()
            && ids(Ctx::List).is_empty()
            && ids(Ctx::Leader).is_empty()
            && ids(Ctx::Hints).is_empty(),
        "the workspace pane's guide lost or misplaced a key for its contexts"
    );
    assert!(bindings.iter().all(|(c, _)| !c.guide.is_empty()), "a workspace pane key is listed in no guide context");
    let host_ids = |focus: &Panel, ctx: Ctx| -> Vec<String> { command::for_guide(l::keymap(), focus, ctx).into_iter().map(|c| c.id).collect() };
    let view = Panel::View(String::new());
    assert!(
        mem(&host_ids(&view, Ctx::Canvas), "guide.toggle")
            && !mem(&host_ids(&view, Ctx::Canvas), "graph.add")
            && mem(&host_ids(&Panel::Graph, Ctx::Leader), "panel.list")
            && mem(&host_ids(&Panel::Graph, Ctx::Leader), "preset.save"),
        "host guide lost a context or ignored command scope"
    );
    assert!(
        mem(&host_ids(&Panel::Graph, Ctx::Node), "scene.enter")
            && !mem(&host_ids(&Panel::Graph, Ctx::Canvas), "scene.enter")
            && mem(&host_ids(&Panel::Graph, Ctx::Canvas), "scene.up")
            && mem(&host_ids(&Panel::Graph, Ctx::List), "scene.enter")
            && mem(&host_ids(&Panel::Graph, Ctx::Hints), "guide.keys")
            && mem(&host_ids(&Panel::Graph, Ctx::List), "list.up")
            && !mem(&host_ids(&Panel::Graph, Ctx::Canvas), "list.up"),
        "the host guide contexts changed"
    );
    // the list walks with j and k as with the arrows
    let list_step = |c: char, expected: pxui_shell::tree::Command| router_actions(&Panel::Graph, vec![ch(c)]).iter().any(|a| *a == l::action::List_command(expected));
    assert!(list_step('j', pxui_shell::tree::Command::Down) && list_step('k', pxui_shell::tree::Command::Up), "j and k do not walk the list");
    // focus is a panel; commands are scoped by its kind
    assert!(
        [Panel::Graph, Panel::List, Panel::Lisp].iter().all(|p| l::scope(p) == Panel::Graph)
            && l::scope(&Panel::View("v1.2".into())) == view
            && l::scope(&Panel::View("main".into())) == l::scope(&Panel::View("v0".into()))
            && l::scope(&Panel::Inspector) == Panel::Inspector
            && l::scope(&Panel::Outline) == Panel::Outline,
        "panel kinds do not map to the scopes commands use"
    );
    assert!(
        host_ids(&l::scope(&Panel::Lisp), Ctx::Canvas) == host_ids(&Panel::Graph, Ctx::Canvas)
            && host_ids(&l::scope(&Panel::View("v3".into())), Ctx::Canvas) == host_ids(&view, Ctx::Canvas)
            && host_ids(&l::scope(&Panel::Outline), Ctx::Canvas) != host_ids(&Panel::Graph, Ctx::Canvas),
        "a lisp or second viewport panel routed to the wrong scope"
    );
    let table: Vec<command::Command> = bindings.iter().map(|(c, _)| command::Command { scope: Some(Panel::Graph), ..c.clone() }).collect();
    let refs: Vec<&command::Command> = table.iter().collect();
    use pxui_graph::scope_state::{Command as SC, Direction};
    let cases: Vec<(Key, Vec<Key>, SC)> = vec![
        (Key::ArrowLeft, vec![], SC::Walk(Direction::Left)),
        (Key::ArrowDown, vec![], SC::Walk(Direction::Down)),
        (Key::ArrowUp, vec![], SC::Walk(Direction::Up)),
        (Key::ArrowRight, vec![], SC::Walk(Direction::Right)),
        (Key::KeyChar('x'), vec![], SC::Delete),
        (Key::Delete, vec![], SC::Delete),
        (Key::Backspace, vec![], SC::Delete),
        (Key::KeyChar('b'), vec![], SC::Bypass),
        (Key::KeyChar('r'), vec![], SC::Wrap_repeat),
        (Key::KeyChar('r'), vec![Key::Shift], SC::Wrap_iterate),
        (Key::KeyChar('c'), vec![], SC::Collapse),
        (Key::KeyChar('m'), vec![], SC::Make_macro),
        (Key::Home, vec![], SC::Frame_all),
    ];
    for (pressed, modifiers, expected) in cases {
        let events: Vec<Event> = modifiers.iter().map(|m| key(m.clone())).chain(std::iter::once(key(pressed))).collect();
        let route = |focus: &Panel| -> usize {
            let (_, actions, _) = router::step(
                &[],
                Platform::Apple,
                &refs,
                focus,
                false,
                &frame_at((500., 400.), events.clone(), 1),
                router::State::Idle,
            );
            actions.len()
        };
        let (_, actions, _) = router::step(
            &[],
            Platform::Apple,
            &refs,
            &Panel::Graph,
            false,
            &frame_at((500., 400.), events.clone(), 1),
            router::State::Idle,
        );
        let routed: Vec<SC> = actions.iter().map(|c| bindings[table.iter().position(|t| t.id == c.id && t.trigger == c.trigger).expect("a binding")].1.clone()).collect();
        assert!(routed == vec![expected], "a graph pane key routed the wrong action");
        assert!(route(&view) == 0 && route(&Panel::Inspector) == 0, "a graph pane key escaped graph scope");
    }
}

/// The guide preference, the key sheet modal and the key HUD of the crash dump.
pub fn guide_preference() {
    use editor_core::store::settings as store;
    let dir = scratch_dir("rays-guide");
    let filename = format!("{dir}/preferences.rays");
    let ws = box_workspace();
    let create = || create(editor::Create { preferences: Some(filename.clone()), ..options() }, &ws);
    let mut d = Driver::new(create());
    let enabled = || -> Option<bool> { store::bool(&store::load("rays-editor", &filename).expect("preferences"), "guide") };
    let step = |d: &mut Driver, events: Vec<Event>, count: i64| {
        d.count = count - 1;
        d.step_at((100., 300.), events);
    };
    step(&mut d, vec![], 0);
    step(&mut d, vec![ch('?')], 1);
    assert!(enabled() == Some(false), "guide did not default on and persist off");
    assert!(!editor::can_undo(d.e()), "guide preference entered document history");
    d.close();
    d = Driver::new(create());
    step(&mut d, vec![], 2);
    step(&mut d, vec![ch('?')], 3);
    assert!(enabled() == Some(true), "a new host did not load the saved guide preference");
    // A sheet owns input until shared modal dismissal, then shortcuts resume.
    step(&mut d, vec![key(Key::KeyChar('/')), ch('?')], 4);
    step(&mut d, vec![ch('?')], 5);
    assert!(enabled() == Some(true), "guide key escaped the key-sheet modal");
    step(&mut d, vec![key(Key::Escape)], 6);
    step(&mut d, vec![ch('?')], 7);
    assert!(enabled() == Some(false), "key-sheet dismissal kept keyboard focus");
    // Toggle preserves unrelated preferences, and Hide follows the same save path.
    store::save("rays-editor", &filename, &[("guide".to_string(), flow_syntax::syntax::parse("false").unwrap().remove(0)), ("other".to_string(), flow_syntax::syntax::parse("7").unwrap().remove(0))]).expect("save");
    step(&mut d, vec![key(Key::Shift), ch('/'), Event::Key_released(Key::Shift)], 8);
    step(&mut d, vec![ch('?')], 9);
    let values = store::load("rays-editor", &filename).expect("load");
    assert!(store::bool(&values, "guide") == Some(false) && store::int(&values, "other") == Some(7), "Hide did not save off or discarded another preference");
    let contents = "invalid saved preferences";
    std::fs::write(&filename, contents).expect("write");
    step(&mut d, vec![ch('?')], 10);
    assert!(std::fs::read_to_string(&filename).expect("read") == contents, "guide toggle overwrote an unreadable preference file");
    let dump = |d: &Driver| -> String {
        editor::crash_dump(d.e(), &dir).expect("crash dump");
        std::fs::read_to_string(format!("{dir}/editor.txt")).expect("editor.txt")
    };
    assert!(has(&dump(&d), "key hud: ? · toggle guide\n"), "key HUD did not use the routed command label");
    step(&mut d, vec![key(Key::Ctrl), ch('z'), Event::Key_released(Key::Ctrl)], 13);
    assert!(has(&dump(&d), "key hud: ⌃Z · undo\n"), "key HUD displayed a different alias from the routed chord");
    step(&mut d, vec![], 200);
    assert!(has(&dump(&d), "key hud: -\n"), "key HUD outlived 1.5 seconds");
    d.close();
    let _ = std::fs::remove_dir_all(&dir);
}
