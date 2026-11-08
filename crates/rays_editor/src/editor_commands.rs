//! The editor's command table: one table drives dispatch, which-key, and the command palette.
//!
//! Specialisation of `Editor_core.Command`: the commands are `editor_core::command::t` (scope = a
//! kind of panel) and their `action` field is a code into this module's action table.
//! `action_of(&command)` gives the `action` back.
#![allow(non_camel_case_types)]
use std::sync::OnceLock;

use editor_core::command;
use editor_core::guide_context as gc;
use editor_core::keymap::Trigger;
use editor_core::panels::{Axis, Panel, Path};
use input::Key;
use pxui_graph::scope_commands as scope_commands;
use pxui_graph::scope_state::Command as Scope_command_t;
use pxui_shell::tree;

use crate::text_pane;

#[derive(Clone, Debug, PartialEq)]
pub enum action {
    Save_preset,
    Browse_presets,
    /// Command-S: rewrite the sketch's .rays, else a preset
    Save_source,
    /// the palette: take the source file's text over unsaved edits
    Reload_source,
    /// Command-O: choose a .rays in the system dialog and open it in this window
    Open_source,
    Toggle_timeline,
    Toggle_graph,
    Toggle_inspector,
    Hide_ui,
    Open_camera,
    Play_pause,
    Reset,
    Stop,
    Add_node,
    Frame_tile,
    Frame_camera,
    Look_through,
    Look_through_camera,
    Fly,
    /// 0 none, 1 translate, 2 rotate, 3 scale
    Tool(i64),
    /// the viewport header's tabs: 0 solid, 1 wire, 2 traced
    Render_mode(i64),
    Undo,
    Redo,
    Panel_split(Axis),
    Panel_close,
    /// / o ...: the focused panel, as the header menu does
    Panel_retype(Panel),
    Panel_pick,
    /// x, n: a menu of panel kinds at the pointer; 1-8 or a click picks, then x places a split, n
    /// opens a window
    Window_pick,
    /// the pending split, committed where the pointer clicked
    Panel_split_at(Path, Axis, f64, Panel),
    /// / m: in the World, the view pane flips to the lat-long map
    Toggle_map,
    /// Command +/-/0: the kit text of every panel but the graph and viewports
    Ui_scale(i64),
    Restore_layout,
    Enter,
    Up,
    Go_world,
    /// Esc, or a click on an earlier part of the graph header's route: that many steps back, only
    /// along the route
    Back(i64),
    /// I: the followed graph in a floating window
    Peek,
    /// y: carry the open material or SOP graph, or the selected object's graph (see carry.rs)
    Pick_up,
    /// / j: a filter over every graph
    Jump,
    /// Command-F, -Option-F, -G, -Shift-G, -E and F12: the find bar, its matches, the definition
    /// under the caret
    Text_command(text_pane::Command),
    /// Command-P: a filter over the bindings of the text
    Goto_symbol,
    /// Command-L: a line and column of the text
    Goto_line,
    Layout_switch(i64),
    Layout_new,
    /// / [ 0..9, n, x: the layouts of the editor graph's switch
    Layout_remove,
    Window_new(Panel),
    /// / n g/l/t/i/u/m/w: a floating window in the active layout; / o f floats or docks the
    /// focused panel
    Float_toggle,
    World_emit,
    World_reseed,
    World_time(f64),
    World_play,
    World_preset(i64),
    /// the workspace pane, see `Editor_panes.scope_name`
    Scope_command(Scope_command_t),
    List_command(tree::Command),
    Guide_toggle,
    Guide_keys,
    Command_palette,
    /// the palette: the workspace text, as Command-S writes it, on the clipboard
    Copy_lisp,
}

/// A command's scope is a kind of panel: any viewport is `View ""`, and the list and the lisp panel
/// are the graph pane's other projections.
pub type Command = command::Command;

pub fn scope(panel: &Panel) -> Panel {
    match panel {
        Panel::View(_) => Panel::View(String::new()),
        Panel::List | Panel::Lisp => Panel::Graph,
        other => other.clone(),
    }
}

pub use editor_core::router::State;

/// What the table is made of: each command with the action its `action` code names.
struct Table {
    commands: Vec<Command>,
    actions: Vec<action>,
}

struct Builder {
    commands: Vec<Command>,
    actions: Vec<action>,
}

impl Builder {
    /// `command ?trigger ?scope ?guide ~id ~label action`: a leader key is listed by the leader
    /// sheet.
    fn command(&mut self, id: &str, label: &str, trigger: Option<Trigger>, scope: Option<Panel>, guide: &[gc::GuideContext], act: action) {
        let mut g: Vec<gc::GuideContext> = Vec::new();
        if matches!(trigger, Some(Trigger::Leader(_))) {
            g.push(gc::GuideContext::Leader);
        }
        g.extend_from_slice(guide);
        let code = self.actions.len();
        self.actions.push(act);
        self.commands.push(command::make(trigger, scope, g, id, label, code));
    }
}

fn leader(s: &str) -> Option<Trigger> {
    Some(Trigger::Leader(s.to_string()))
}

fn chord(key: Key, modifiers: &[Key]) -> Option<Trigger> {
    Some(Trigger::Chord(key, modifiers.to_vec()))
}

fn build() -> Table {
    let mut b = Builder { commands: Vec::new(), actions: Vec::new() };
    let graph = Some(Panel::Graph);
    let view = Some(Panel::View(String::new()));
    let ch = Key::KeyChar;
    use gc::GuideContext::{Canvas, Hints, List, Multi, Node};
    use action::*;
    b.command("guide.toggle", "toggle guide", chord(ch('/'), &[Key::Shift]), None, &[Canvas, Node, Multi, Hints, gc::GuideContext::Leader, List], Guide_toggle);
    b.command("guide.toggle", "toggle guide", chord(ch('?'), &[]), None, &[], Guide_toggle);
    b.command("guide.keys", "all keys", leader("?"), None, &[Canvas, Node, Multi, Hints, List], Guide_keys);
    b.command("preset.save", "save preset", leader("s"), None, &[], Save_preset);
    b.command("preset.browse", "browse presets", leader("b"), None, &[], Browse_presets);
    b.command("workspace.toggle-timeline", "toggle timeline", leader("t"), None, &[], Toggle_timeline);
    b.command("workspace.toggle-graph", "toggle graph", leader("g"), None, &[], Toggle_graph);
    b.command("workspace.toggle-inspector", "toggle inspector", leader("i"), None, &[], Toggle_inspector);
    b.command("workspace.hide-ui", "hide all UI", leader("h"), None, &[], Hide_ui);
    b.command("workspace.camera-section", "camera section", leader("c"), None, &[], Open_camera);
    b.command("timeline.play-pause", "play / pause", leader("p"), None, &[], Play_pause);
    b.command("timeline.play-pause", "play / pause", chord(Key::Space, &[]), None, &[], Play_pause);
    b.command("timeline.reset", "reset", leader("r"), None, &[], Reset);
    b.command("timeline.reset", "reset", chord(ch('p'), &[Key::Shift]), None, &[], Reset);
    b.command("sketch.stop", "stop", leader("q"), None, &[], Stop);
    b.command("workspace.command-palette", "command palette", leader("/"), None, &[], Command_palette);
    b.command("file.copy-lisp", "Copy workspace as Lisp", None, None, &[], Copy_lisp);
    b.command("file.reload", "Reload sketch from its file", None, None, &[], Reload_source);
    b.command("world.map", "3D / map (World)", leader("m"), None, &[], Toggle_map);
    b.command("workspace.restore-layout", "restore layout", leader("z"), None, &[], Restore_layout);
    b.command("panel.split-right", "split panel, side by side", leader("ov"), None, &[], Panel_split(Axis::H));
    b.command("panel.split-below", "split panel, stacked", leader("oh"), None, &[], Panel_split(Axis::V));
    b.command("panel.close", "close panel", leader("ox"), None, &[], Panel_close);
    b.command("panel.pick-split", "split panel at the pointer (pick a kind)", leader("x"), None, &[], Panel_pick);
    b.command("window.pick", "new window (pick a kind)", leader("n"), None, &[], Window_pick);
    b.command("panel.float", "float / dock panel", leader("of"), None, &[], Float_toggle);
    // / [: the first nine rows are the layouts, named from their panels when which-key draws
    for i in 0..10 {
        b.command(&format!("layout.{i}"), "layout", leader(&format!("[{i}")), None, &[], Layout_switch(i));
    }
    b.command("layout.new", "layout: new, from this one", leader("[n"), None, &[], Layout_new);
    b.command("layout.remove", "layout: remove this one", leader("[x"), None, &[], Layout_remove);
    // / l: the focused panel becomes one of the kinds (a document without an editor graph gets one
    // written from its layout first)
    b.command("panel.graph", "panel: graph", leader("lg"), None, &[], Panel_retype(Panel::Graph));
    b.command("panel.list", "panel: list", leader("ll"), None, &[], Panel_retype(Panel::List));
    b.command("panel.lisp", "panel: lisp text", leader("lt"), None, &[], Panel_retype(Panel::Lisp));
    b.command("panel.inspector", "panel: inspector", leader("li"), None, &[], Panel_retype(Panel::Inspector));
    b.command("panel.outline", "panel: outline", leader("lu"), None, &[], Panel_retype(Panel::Outline));
    b.command("panel.timeline", "panel: timeline", leader("lm"), None, &[], Panel_retype(Panel::Timeline));
    b.command("panel.viewport", "panel: viewport", leader("lw"), None, &[], Panel_retype(Panel::View(String::new())));
    b.command("panel.canvas", "panel: canvas", leader("lc"), None, &[], Panel_retype(Panel::Canvas(String::new())));
    b.command("scene.world", "World", leader("e"), None, &[], Go_world);
    b.command("graph.add-node", "add (menu)", leader("a"), None, &[], Add_node);
    // the graph's own key for the same menu: with a node selected the pick is wired after it
    b.command("graph.add-after", "add after", chord(Key::Tab, &[]), graph.clone(), &[], Add_node);
    b.command("world.emit", "dome / light", chord(ch('t'), &[]), graph.clone(), &[Node, List], World_emit);
    b.command("world.reseed", "reseed", chord(ch('n'), &[]), graph.clone(), &[Node, List], World_reseed);
    b.command("world.earlier", "time -30 min", chord(ch('['), &[]), graph.clone(), &[Canvas, Node, Multi, List], World_time(-0.5));
    b.command("world.later", "time +30 min", chord(ch(']'), &[]), graph.clone(), &[Canvas, Node, Multi, List], World_time(0.5));
    b.command("world.play", "play day cycle", chord(ch('d'), &[]), graph.clone(), &[Canvas, Node, Multi, List], World_play);
    for (index, (name, _)) in rays::world::presets().iter().enumerate() {
        b.command(
            &format!("world.preset.{}", index + 1),
            &format!("preset {name}"),
            chord(ch((b'1' + index as u8) as char), &[]),
            graph.clone(),
            &[Canvas, Node, Multi, List],
            World_preset(index as i64),
        );
    }
    b.command("scene.enter", "follow / enter", chord(ch('i'), &[]), None, &[Node, List], Enter);
    b.command("scene.peek", "peek (floating graph)", chord(ch('i'), &[Key::Shift]), None, &[Node, List], Peek);
    b.command("scene.jump", "jump to graph", leader("j"), None, &[], Jump);
    b.command("text.definition", "go to definition", chord(Key::F12, &[]), None, &[], Text_command(text_pane::Command::Definition));
    b.command("text.references", "find references", chord(Key::F12, &[Key::Shift]), None, &[], Text_command(text_pane::Command::References));
    // carry: y picks up; the target letters, Enter and Escape are read by `editor_frame` while it
    // lasts
    b.command("carry.pick-up", "pick up (carry it onto a place)", chord(ch('y'), &[]), None, &[Canvas, Node, Multi, List], Pick_up);
    b.command("scene.up", "back (up a level)", chord(ch('u'), &[]), None, &[Canvas, Node, Multi, List], Up);
    b.command("scene.back", "back along the route", chord(Key::Escape, &[]), graph.clone(), &[Canvas, Node, Multi], Back(1));
    // Rare: the graph context menu and the palette, no leader key.
    b.command("graph.frame-tile", "frame displayed tile", leader("f"), graph.clone(), &[], Frame_tile);
    b.command("graph.frame-tile", "reveal list selection", chord(ch('f'), &[]), graph.clone(), &[List], Frame_tile);
    b.command("view.frame-camera", "frame the displayed node", chord(ch('f'), &[]), view.clone(), &[Canvas, Node, Multi], Frame_camera);
    for modifier in [Key::Meta, Key::Ctrl] {
        b.command("file.save", "save sketch", chord(ch('s'), &[modifier]), None, &[], Save_source);
        b.command("file.open", "open sketch", chord(ch('o'), &[modifier]), None, &[], Open_source);
        b.command("edit.undo", "undo", chord(ch('z'), &[modifier]), None, &[], Undo);
        b.command("edit.redo", "redo", chord(ch('z'), &[modifier, Key::Shift]), None, &[], Redo);
        b.command("edit.redo", "redo", chord(ch('y'), &[modifier]), None, &[], Redo);
        b.command("text.find", "find in text", chord(ch('f'), &[modifier]), None, &[], Text_command(text_pane::Command::Find));
        b.command("text.replace", "find and replace", chord(ch('f'), &[modifier, Key::Alt]), None, &[], Text_command(text_pane::Command::FindReplace));
        b.command("text.next", "next match", chord(ch('g'), &[modifier]), None, &[], Text_command(text_pane::Command::FindNext));
        b.command("text.previous", "previous match", chord(ch('g'), &[modifier, Key::Shift]), None, &[], Text_command(text_pane::Command::FindPrevious));
        b.command("text.find-selection", "find the selection", chord(ch('e'), &[modifier]), None, &[], Text_command(text_pane::Command::FindSelection));
        b.command("text.symbol", "go to symbol", chord(ch('p'), &[modifier]), None, &[], Goto_symbol);
        b.command("text.line", "go to line", chord(ch('l'), &[modifier]), None, &[], Goto_line);
        b.command("ui.larger", "larger panel text", chord(ch('='), &[modifier]), None, &[], Ui_scale(1));
        b.command("ui.larger", "larger panel text", chord(ch('+'), &[modifier]), None, &[], Ui_scale(1));
        b.command("ui.smaller", "smaller panel text", chord(ch('-'), &[modifier]), None, &[], Ui_scale(-1));
        b.command("ui.reset-size", "default panel text size", chord(ch('0'), &[modifier]), None, &[], Ui_scale(0));
    }
    for (c, scope_action) in scope_commands::bindings() {
        let mut c = c;
        c.scope = graph.clone();
        c.action = b.actions.len();
        b.actions.push(Scope_command(scope_action));
        b.commands.push(c);
    }
    for (c, tree_action) in tree::bindings() {
        let mut c = c;
        c.scope = graph.clone();
        c.guide = vec![List];
        c.action = b.actions.len();
        b.actions.push(List_command(tree_action));
        b.commands.push(c);
    }
    b.command("view.fly", "fly (WASD, Q/E, Esc)", leader("w"), view.clone(), &[], Fly);
    b.command("view.look-through", "look through render camera", leader("v"), view.clone(), &[], Look_through);
    b.command("view.translate", "move handles", chord(ch('w'), &[]), view.clone(), &[Canvas, Node, Multi], Tool(1));
    b.command("view.rotate", "rotate handles", chord(ch('e'), &[]), view.clone(), &[Canvas, Node, Multi], Tool(2));
    b.command("view.scale", "scale handles", chord(ch('r'), &[]), view.clone(), &[Canvas, Node, Multi], Tool(3));
    b.command("view.orbit", "orbit only (hide handles)", chord(Key::Escape, &[]), view.clone(), &[Canvas, Node, Multi], Tool(0));
    Table { commands: b.commands, actions: b.actions }
}

static TABLE: OnceLock<Table> = OnceLock::new();

fn table() -> &'static Table {
    TABLE.get_or_init(build)
}

/// One table drives dispatch, which-key, and the command palette.
pub fn keymap() -> &'static [Command] {
    &table().commands
}

/// What a command of this table does (its `action` code is an index into the table's actions).
pub fn action_of(c: &Command) -> &'static action {
    &table().actions[c.action]
}

/// The keys the text area handles itself (Command-/, Option-Up/Down, Command-Option-Up,
/// Command-Shift-\\): listed on the key sheet only. They are not in `keymap`, so the router never
/// takes them from the field.
pub fn text_keys() -> Vec<(&'static str, Trigger)> {
    vec![
        ("toggle comment on lines", Trigger::Chord(Key::KeyChar('/'), vec![Key::Meta])),
        ("move lines up or down", Trigger::Chord(Key::ArrowUp, vec![Key::Alt])),
        ("duplicate lines up or down", Trigger::Chord(Key::ArrowUp, vec![Key::Alt, Key::Shift])),
        ("select the enclosing form", Trigger::Chord(Key::ArrowUp, vec![Key::Meta, Key::Alt])),
        ("jump to the matching bracket", Trigger::Chord(Key::KeyChar('\\'), vec![Key::Meta, Key::Shift])),
    ]
}

/// The leader sheet's sections (`Pxui_shell.Which_key.panel`): what a command is for, from its id.
pub fn group(c: &Command) -> &'static str {
    let id = c.id.as_str();
    let starts = |prefix: &str| id.starts_with(prefix);
    if id == "graph.add-node" || starts("window.") {
        "Add"
    } else if starts("panel.") || starts("workspace.toggle-") || id == "workspace.hide-ui" {
        "Panel"
    } else if starts("text.") {
        "Text"
    } else if starts("layout.") || id == "workspace.restore-layout" {
        "Layout"
    } else if starts("timeline.") || id == "sketch.stop" {
        "Time"
    } else if starts("preset.") || starts("file.") || id == "guide.keys" || id == "workspace.command-palette" {
        "File"
    } else {
        "Go"
    }
}

/// The key sheet's sections (`Pxui_shell.Which_key.sheet`): a pane's own keys under the pane, the
/// rest as the leader sheet has them.
pub fn sheet_group(c: &Command) -> &'static str {
    match &c.scope {
        Some(Panel::Graph) => "Graph",
        Some(Panel::View(_)) => "Viewport",
        Some(_) | None => group(c),
    }
}

/// The keys of a section in the sheet's [09] order, not the keymap's.
pub fn order(section: &str) -> Vec<String> {
    let keys: &[&str] = match section {
        "Add" => &["a", "n"],
        "Panel" => &["o", "l"],
        "Layout" => &["["],
        "Go" => &["j", "e"],
        "Time" => &["p"],
        "File" => &["s", "b", "?", "/"],
        _ => &[],
    };
    keys.iter().map(|k| k.to_string()).collect()
}

/// What a leader key that continues into more keys stands for.
pub fn describe_prefix(prefix: &str) -> Option<String> {
    match prefix {
        "o" => Some("split, close, float".to_string()),
        "l" => Some("panel kind".to_string()),
        "n" => Some("window".to_string()),
        "[" => Some("switch layout".to_string()),
        _ => None,
    }
}

pub fn pane_name(panel: &Panel) -> &'static str {
    match panel {
        Panel::Canvas(_) => "Canvas",
        Panel::View(_) => "View",
        Panel::Graph => "Graph",
        Panel::List => "List",
        Panel::Lisp => "Lisp",
        Panel::Inspector => "Inspector",
        Panel::Spreadsheet => "Spreadsheet",
        Panel::Outline => "Outline",
        Panel::Timeline => "Timeline",
    }
}
