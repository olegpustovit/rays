//! Port of the pure sections of lib/pxui_shell/test_shell.ml: tree keys, the key sheet, scrubbing.
use crate::tree::{self as T, Command, Intent, Row};
use crate::which_key;
use editor_core::command;
use editor_core::keymap::Trigger;
use input::{Key, Platform};
use rays_math::color;

fn row(id: i64, depth: i64, label: &str) -> Row {
    Row { id, depth, label: label.to_string(), detail: String::new(), badge: ("S".to_string(), color::gray(90)), link: false, ghost: false, flags: vec![true] }
}

#[test]
fn tree_keys_through_run_command() {
    let rows = vec![row(1, 0, "a"), row(2, 1, "b"), row(3, 1, "c"), row(4, 0, "d")];
    let run = |t: T::Tree, c: Command| T::run_command(t, &rows, &[], c);
    let expect = |name: &str, got: Vec<Intent>, want: Vec<Intent>| assert_eq!(got, want, "tree: {name}");
    let (t, intents) = run(T::create(), Command::Down);
    expect("Down from nothing selects the first row", intents, vec![Intent::Select(vec![1])]);
    let (t, intents) = run(t, Command::Down);
    expect("Down moves to the child", intents, vec![Intent::Select(vec![2])]);
    let (_, intents) = run(t.clone(), Command::Extend_down);
    expect("Shift-Down extends from the anchor", intents, vec![Intent::Select(vec![3, 2])]);
    let (t, intents) = run(t, Command::Collapse);
    expect("Left on a leaf goes to its parent", intents, vec![Intent::Select(vec![1])]);
    let (t, intents) = run(t, Command::Collapse);
    expect("Left on an open parent folds it", intents, vec![]);
    let (t, intents) = run(t, Command::Down);
    expect("a folded parent hides its children", intents, vec![Intent::Select(vec![4])]);
    let (t, _) = run(t, Command::Up);
    let (t, intents) = run(t, Command::Expand);
    expect("Right unfolds", intents, vec![]);
    let (t, intents) = run(t, Command::Expand);
    expect("Right on an open parent goes to its first child", intents, vec![Intent::Select(vec![2])]);
    let (_, intents) = run(t.clone(), Command::Last);
    expect("End selects the last row", intents, vec![Intent::Select(vec![4])]);
    let (_, intents) = T::run_command(t.clone(), &rows, &[2, 3], Command::Indent_rows);
    expect("Tab reparents the selection", intents, vec![Intent::Indent(vec![2, 3])]);
    let (_, intents) = run(t.clone(), Command::Outdent_rows);
    expect("Shift-Tab outdents the focus", intents, vec![Intent::Outdent(vec![2])]);
    let (_, intents) = run(t.clone(), Command::Move_up);
    expect("Alt-Up reorders", intents, vec![Intent::Reorder { ids: vec![2], delta: -1 }]);
    let (_, intents) = run(t.clone(), Command::Hide);
    expect("h hides the focused row", intents, vec![Intent::Flag { ids: vec![2], column: 0, value: false }]);
    let (_, intents) = run(t.clone(), Command::Activate_row);
    expect("the enter key activates", intents, vec![Intent::Activate(2)]);
    let (filtering, _) = run(t, Command::Filter);
    assert!(T::editing(&filtering), "/ opens the filter");
}

#[test]
fn scrub_and_show() {
    use param::number as N;
    let got = [
        N::scrub(N::Kind::Float, None, "1", 10.0, false),
        N::scrub(N::Kind::Float, None, "1", 0.04, false),
        N::scrub(N::Kind::Float, None, "1234567.89", 2.0, false),
        N::scrub(N::Kind::Int, None, "3", 12.0, false),
        N::scrub(N::Kind::Float, Some((0.0, 2.0)), "1", 500.0, false),
        N::scrub(N::Kind::Int, Some((0.0, 5.0)), "9", 6.0, false),
        N::show(1e-7),
        N::show(0.5),
    ];
    assert_eq!(got.to_vec(), vec!["1.5", "1", "1234567.99", "5", "2.0", "9", "0.0000001", "0.5"]);
}

#[test]
fn key_sheet_lists_every_command_once_under_its_section() {
    // the key sheet lists every command a key reaches, once, under its host's section
    let command = |scope: Option<&str>, id: &str, trigger: Trigger| -> command::t<String, String> {
        command::make(Some(trigger), scope.map(|s| s.to_string()), Vec::new(), id, id, id.to_string())
    };
    let keymap = vec![
        command(None, "file.save", Trigger::Chord(Key::KeyChar('s'), vec![Key::Meta])),
        command(None, "file.save", Trigger::Chord(Key::KeyChar('s'), vec![Key::Ctrl])),
        command(Some("graph"), "scope.delete", Trigger::Chord(Key::Delete, vec![])),
        command(Some("graph"), "scope.delete", Trigger::Chord(Key::KeyChar('x'), vec![])),
        command(None, "panel.close", Trigger::Leader("ox".to_string())),
        command::make(None, None, Vec::new(), "palette.only", "no key", "palette".to_string()),
    ];
    let category = |c: &command::t<String, String>| -> String {
        if c.scope.is_some() {
            "Graph".to_string()
        } else if c.id.starts_with("panel.") {
            "Panel".to_string()
        } else {
            "File".to_string()
        }
    };
    let sections = which_key::sheet_sections(Platform::Apple, Some(&category), &[], &keymap);
    assert_eq!(sections.iter().map(|(t, _)| t.clone()).collect::<Vec<_>>(), vec!["Panel", "File", "Graph"]);
    assert_eq!(
        sections.iter().flat_map(|(_, rows)| rows.iter().map(|r| r.1.clone())).collect::<Vec<_>>(),
        vec!["panel.close", "file.save", "scope.delete"]
    );
    let graph = &sections.iter().find(|(t, _)| t == "Graph").unwrap().1;
    assert_eq!(graph, &vec![("x / \u{2326}".to_string(), "scope.delete".to_string())]);
}

#[test]
fn leader_pages_by_category_and_chords() {
    let c = |scope: Option<&str>, trigger: Trigger, id: &str, label: &str| -> command::t<String, String> {
        command::make(Some(trigger), scope.map(|s| s.to_string()), Vec::new(), id, label, id.to_string())
    };
    let keymap = vec![
        c(None, Trigger::Leader("ps".to_string()), "panel.split", "Split right"),
        c(None, Trigger::Leader("pc".to_string()), "panel.close", "Close panel"),
        c(None, Trigger::Leader("s".to_string()), "file.save", "Save"),
        c(Some("graph"), Trigger::Leader("g".to_string()), "graph.go", "Go there"),
    ];
    let category = |c: &command::t<String, String>| -> String { if c.id.starts_with("panel.") { "Panel".to_string() } else { "File".to_string() } };
    // the pure page builders are private; the key sheet is the public face of the same table
    let sections = which_key::sheet_sections(Platform::Other, Some(&category), &[], &keymap);
    assert_eq!(sections.iter().map(|(t, _)| t.as_str()).collect::<Vec<_>>(), vec!["Panel", "File"]);
}
