//! The command table: its codes name the actions, the sheet sections follow the ids.
use crate::editor_commands as c;
use crate::text_pane;
use editor_core::keymap::Trigger;
use editor_core::panels::Panel;

#[test]
fn every_command_names_its_action_and_scope() {
    let keymap = c::keymap();
    assert!(keymap.len() > 150, "the table is {} long", keymap.len());
    let find = |id: &str| keymap.iter().filter(|command| command.id == id).collect::<Vec<_>>();
    // an id shared by several keys is one command: same action, same label
    let undo = find("edit.undo");
    assert_eq!(undo.len(), 2);
    assert!(undo.iter().all(|u| *c::action_of(u) == c::action::Undo && u.label == "undo"));
    assert_eq!(find("edit.redo").len(), 4);
    // the leader keys are listed by the leader sheet
    let save = find("preset.save")[0];
    assert_eq!(save.trigger, Some(Trigger::Leader("s".to_string())));
    assert!(save.guide.contains(&editor_core::guide_context::t::Leader));
    // the graph pane's own keys carry the pane as their scope
    let tab = find("graph.add-after")[0];
    assert_eq!((tab.scope.clone(), c::action_of(tab).clone()), (Some(Panel::Graph), c::action::Add_node));
    assert_eq!(find("view.frame-camera")[0].scope, Some(Panel::View(String::new())));
    // the scope and list bindings are routed through the table
    assert!(keymap.iter().any(|command| matches!(c::action_of(command), c::action::Scope_command(_))));
    assert!(keymap.iter().any(|command| matches!(c::action_of(command), c::action::List_command(_))));
    assert!(keymap.iter().any(|command| *c::action_of(command) == c::action::Text_command(text_pane::Command::Definition)));
    // ten layouts, named by their index
    assert_eq!((0..10).filter(|i| !find(&format!("layout.{i}")).is_empty()).count(), 10);
}

#[test]
fn scope_maps_panels_to_the_pane_that_owns_their_keys() {
    assert_eq!(c::scope(&Panel::View("main".into())), Panel::View(String::new()));
    assert_eq!(c::scope(&Panel::List), Panel::Graph);
    assert_eq!(c::scope(&Panel::Lisp), Panel::Graph);
    assert_eq!(c::scope(&Panel::Timeline), Panel::Timeline);
}

#[test]
fn sheet_sections_follow_ids_and_scopes() {
    let keymap = c::keymap();
    let section = |id: &str| c::sheet_group(keymap.iter().find(|command| command.id == id).unwrap());
    assert_eq!(section("graph.add-node"), "Add");
    assert_eq!(section("panel.close"), "Panel");
    assert_eq!(section("timeline.play-pause"), "Time");
    assert_eq!(section("file.save"), "File");
    assert_eq!(section("scene.up"), "Go");
    assert_eq!(section("graph.add-after"), "Graph");
    assert_eq!(section("view.translate"), "Viewport");
    assert_eq!(c::order("File"), vec!["s", "b", "?", "/"]);
    assert_eq!(c::describe_prefix("o").as_deref(), Some("split, close, float"));
    assert_eq!(c::pane_name(&Panel::Spreadsheet), "Spreadsheet");
    assert!(!c::text_keys().is_empty());
}

#[test]
fn the_router_runs_the_table() {
    use editor_core::router;
    use input::{Event, Key, Platform};
    let keymap = c::keymap();
    let refs: Vec<&c::Command> = keymap.iter().collect();
    let mut frame = crate::test_timeline::frame_with(0.0, vec![Event::Key_pressed(Key::KeyChar('z'))], vec![]);
    frame.input.keys = vec![Key::Meta];
    let (_, matched, rest) =
        router::step(&[], Platform::Apple, &refs, &Panel::Graph, false, crate::ec_frame::to(&frame), router::State::Idle);
    assert_eq!(matched.iter().map(|m| c::action_of(m).clone()).collect::<Vec<_>>(), vec![c::action::Undo]);
    assert!(rest.input.events.is_empty());
    // / then s opens the preset save
    let frame = crate::test_timeline::frame_with(0.0, vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar('s'))], vec![]);
    let (state, matched, _) =
        router::step(&[], Platform::Apple, &refs, &Panel::Graph, false, crate::ec_frame::to(&frame), router::State::Idle);
    assert_eq!((state, matched.iter().map(|m| c::action_of(m).clone()).collect::<Vec<_>>()), (router::State::Idle, vec![c::action::Save_preset]));
}
