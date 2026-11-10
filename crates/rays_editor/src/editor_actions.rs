//! Leader actions owned by the workspace, the undo labels of intents, the World keys and the
//! routed keymap. #![allow(non_camel_case_types)]
use editor_core::keymap::Trigger;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use editor_core::panels::{self, Panel};
use editor_document::document::{self, Level, T as Document};
use editor_document::kind_graph as kg;
use editor_document::layers;
use editor_document::layout_edit;
use editor_document::scene_sync::{self as sync, Edit};
use flow_graph::flow_edit;
use param::Value;
use pxui_shell::{layout, tree};
use rays::world;

use crate::editor_commands::{self, action, action_of};
use crate::editor_inspect::kind;
use crate::editor_model::*;
use crate::editor_panes::{graph_family, listing, scope_name, texting};
use crate::editor_shell::{layouts, shell_tree};
use crate::selection;
use crate::timeline;

pub type Applied = (shell, selection::Selection, tree::Tree, timeline::Timeline, Vec<timeline::Change>);

/// Leader actions owned by the workspace; the environment handles the rest from `update.actions`.
pub fn apply_action(value: &EditorModel, state: Applied, action: &action) -> Applied {
    let (workspace, selection, mut tree, timeline, changes) = state;
    let timeline_step = |workspace: shell, selection, tree, timeline: timeline::Timeline, mut changes: Vec<timeline::Change>, step: fn(&timeline::Timeline) -> (timeline::Timeline, Vec<timeline::Change>)| {
        let (timeline, more) = step(&timeline);
        changes.extend(more);
        (workspace, selection, tree, timeline, changes)
    };
    let expand = |panel: Panel, mut workspace: shell| {
        workspace.hidden = layout::expand(&panel, &workspace.hidden);
        workspace
    };
    match action {
        action::Toggle_timeline | action::Toggle_graph | action::Toggle_inspector => (workspace, selection, tree, timeline, changes),
        action::Restore_layout => {
            let workspace = if workspace.restored {
                shell { restored: false, live: None, window_live: None, ..workspace }
            } else {
                shell { tree: layout::default(), hidden: vec![Panel::Timeline], live: None, window_live: None, restored: true }
            };
            (workspace, selection, tree, timeline, changes)
        }
        action::Open_camera => (expand(Panel::Inspector, workspace), selection::clear(), tree, timeline, changes),
        action::Play_pause => timeline_step(workspace, selection, tree, timeline, changes, timeline::toggle_pause),
        action::Reset => timeline_step(workspace, selection, tree, timeline, changes, timeline::reset),
        action::Stop => timeline_step(workspace, selection, tree, timeline, changes, timeline::stop),
        action::Add_node => (expand(Panel::Graph, workspace), selection, tree, timeline, changes),
        action::Frame_tile if crate::editor_panes::projection_of(&value.into()) == List_view => { tree::reveal(&mut tree); (workspace, selection, tree, timeline, changes) },
        action::World_play if timeline::mode(&timeline) != timeline::Mode::Playing => timeline_step(workspace, selection, tree, timeline, changes, timeline::toggle_pause),
        _ => (workspace, selection, tree, timeline, changes),
    }
}

/// The undo label a graph intent gives its document change.
pub fn intent_label(c: &change) -> Option<String> {
    match c {
        change::Dock_panels(..) => Some("Dock panel".to_string()),
        change::Panel_state(_, state) => Some(if state.collapsed { "Collapse panel" } else { "Arrange panel" }.to_string()),
        change::Select_layout(_) => Some("Switch layout".to_string()),
        change::Syntax_edit(op) => Some(flow_edit::label(op).to_string()),
        change::Layout_edit(op) => Some(layout_edit::label(op).to_string()),
        change::Syntax_batch(label, _) => Some(label.clone()),
        change::Syntax_inline { make, .. } => Some(flow_edit::label(&make(&Vec::new())).to_string()),
        change::Layout_inline { make, .. } => Some(layout_edit::label(&make(&Vec::new())).to_string()),
        change::Set_parameter { path, .. } => Some(format!("Set {path}")),
        change::Object_arg { .. } => Some("Edit expression".to_string()),
        change::Rename { .. } => Some("Rename node".to_string()),
        change::Pin_row { pin, .. } => Some(if *pin == Some(false) { "Unpin row from card" } else { "Pin row to card" }.to_string()),
        change::Follow_source(_) | change::Notice(_) | change::Declined(_) => None,
    }
}

pub fn in_world<'a>(value: &ModelView<'a>) -> bool {
    match pane_level(value) {
        Level::Inside(id) => kind(value, id).as_deref() == Some("world"),
        Level::Scene => false,
    }
}

/// World keys: edit the selected layer, or the World node in the scene. Returns the derived
/// document, the last label and the edits the text can write.
pub fn world_keys<'a>(value: &ModelView<'a>, doc: &Arc<Document>, selection: &selection::Selection, actions: &[action]) -> (Arc<Document>, Option<String>, Vec<(Level, Edit)>) {
    let world_id = match pane_level(value) {
        Level::Inside(id) => Some(id),
        Level::Scene => None,
    };
    // what a key asks, as an edit of the text at a level
    let edit_layer = |f: &dyn Fn(&kg::Node) -> Option<Vec<(String, Value)>>, label: &str| -> Option<(Level, Edit, String)> {
        let (id, layer) = (world_id?, selection::selected(selection)?);
        let found = kg::find(document::layers(doc, id)?, layer)?;
        f(&found).map(|values| (Level::Inside(id), Edit::Fields(vec![(layer, values)]), label.to_string()))
    };
    let edit_world = |f: &dyn Fn(&kg::Node) -> Option<Vec<(String, Value)>>, label: &str| -> Option<(Level, Edit, String)> {
        let id = world_id?;
        let found = kg::find(&doc.scene, id)?;
        f(&found).map(|values| (Level::Scene, Edit::Fields(vec![(id, values)]), label.to_string()))
    };
    // a World the text does not have is edited as a derived object, for reconcile to adopt
    let with_fields = |doc: &mut Arc<Document>, level: Level, edits: &[(i64, Vec<(String, Value)>)]| {
        if let Some(graph) = document::kinds(doc, level) {
            let graph = edits.iter().fold(graph, |graph, (id, values)| match kg::set_fields(&graph, *id, values) {
                Ok((graph, _)) => graph,
                Err(_) => graph,
            });
            let current = match level { Level::Scene => Some(&doc.scene), Level::Inside(id) => document::layers(doc, id) };
            if current.is_none_or(|current| !kg::same(current, &graph)) {
                let doc = Arc::make_mut(doc);
                match level { Level::Scene => doc.scene = graph, Level::Inside(id) => { doc.layers.insert(id, graph); } }
            }
        }
    };
    let derived = |doc: &mut Arc<Document>, level: Level, edit: &Edit| {
        match edit {
            Edit::Fields(edits) => with_fields(doc, level, edits),
            Edit::Layers(new_layers, values) => if let Level::Inside(id) = level {
                if document::layers(doc, id).is_none_or(|current| !kg::same(current, new_layers)) {
                    Arc::make_mut(doc).layers.insert(id, new_layers.clone());
                }
                with_fields(doc, Level::Scene, &[(id, values.clone())]);
            },
            _ => {}
        }
    };
    let field = |name: &str, node: &kg::Node| kg::field(node, name);
    let mut found: Vec<(Level, Edit, String)> = Vec::new();
    for a in actions {
        let item = match a {
            action::World_emit => edit_layer(
                &|node| match field("emit", node) {
                    Some(Value::Choice(c)) if c == "Light" => Some(vec![("emit".to_string(), Value::Choice("Dome".to_string()))]),
                    Some(Value::Choice(_)) => Some(vec![("emit".to_string(), Value::Choice("Light".to_string()))]),
                    _ => None,
                },
                "Dome / light",
            ),
            action::World_reseed => edit_layer(
                &|node| match field("seed", node) {
                    Some(Value::Int(seed)) => Some(vec![("seed".to_string(), Value::Int(seed + 1))]),
                    _ => None,
                },
                "Reseed",
            ),
            action::World_time(delta) => edit_world(
                &|node| match field("time_of_day", node) {
                    Some(Value::Float(hours)) => Some(vec![("time_of_day".to_string(), Value::Float((hours + delta).min(24.).max(0.)))]),
                    _ => None,
                },
                "Time of day",
            ),
            action::World_play => edit_world(
                &|node| match field("day_cycle", node) {
                    Some(Value::Float(v)) if v == 0. => Some(vec![("day_cycle".to_string(), Value::Float(1.))]),
                    _ => None,
                },
                "Day cycle",
            ),
            action::World_preset(index) => {
                let presets = world::presets();
                match (world_id, usize::try_from(*index).ok().and_then(|i| presets.get(i))) {
                    (Some(id), Some((name, w))) => match layers::graph_of_world(w) {
                        Ok(graph) => Some((Level::Inside(id), Edit::Layers(graph, layers::of_world(w).1), format!("Preset {name}"))),
                        Err(_) => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        };
        found.extend(item);
    }
    let mut derived_doc = doc.clone();
    let mut label = None;
    let mut edits = Vec::new();
    for (level, edit, name) in found {
        if sync::writes(doc, level, &edit) {
            edits.push((level, edit));
        } else {
            derived(&mut derived_doc, level, &edit);
        }
        label = Some(name);
    }
    (derived_doc, label, edits)
}

/// The ten digit commands take their labels from the layouts when which-key or the palette draws.
pub fn layout_labels(value: &EditorModel, keymap: Vec<editor_commands::Command>) -> Vec<editor_commands::Command> {
    let names = layouts(value);
    keymap
        .into_iter()
        .filter_map(|mut command| match action_of(&command) {
            action::Layout_switch(i) => {
                let (name, active) = usize::try_from(*i).ok().and_then(|i| names.get(i))?;
                command.label = if *active { format!("{name} (active)") } else { name.clone() };
                Some(command)
            }
            _ => Some(command),
        })
        .collect()
}

/// What `routed` keeps commands by: the keymap is a constant table, so equal facts route the same.
#[derive(PartialEq)]
struct Routing {
    graph_shown: bool,
    listing: bool,
    texting: bool,
    scope: bool,
    view: bool,
    graph: bool,
    world: bool,
    layouts: Vec<(String, bool)>,
}

/// Commands that only mean something on this level and projection; the last frame's list while
/// its facts hold.
pub fn routed<'a>(value: &ModelView<'a>) -> Rc<Vec<editor_commands::Command>> {
    let leaves = panels::leaves(&shell_tree(value, &value.workspace));
    let (listing, texting) = (listing(value), texting(value));
    let r = Routing {
        graph_shown: graph_family(value),
        listing,
        texting,
        scope: scope_name(value).is_some(),
        view: leaves.iter().any(|(_, p)| matches!(p, Panel::View(_))),
        graph: leaves.iter().any(|(_, p)| *p == Panel::Graph),
        world: in_world(value),
        layouts: layouts(value),
    };
    thread_local! { static LAST: RefCell<Option<(Routing, Rc<Vec<editor_commands::Command>>)>> = const { RefCell::new(None) }; }
    if let Some(hit) = LAST.with(|l| l.borrow().as_ref().filter(|(k, _)| *k == r).map(|(_, v)| v.clone())) {
        return hit;
    }
    let (f, a) = (Some(Trigger::Leader("f".to_string())), Some(Trigger::Leader("a".to_string())));
    let keep = |command: &&editor_commands::Command| -> bool {
        match action_of(command) {
            action::List_command(_) => r.listing && r.graph_shown,
            action::Scope_command(_) => r.scope && r.graph_shown && !r.texting,
            action::Frame_tile => !r.texting && r.graph_shown && (r.listing || command.trigger == f),
            action::Frame_camera => r.view,
            // Tab is the canvas's key for it; in the list Tab indents the row
            action::Add_node => !r.texting && (!r.listing || command.trigger == a) && r.graph,
            action::World_emit | action::World_reseed | action::World_time(_) | action::World_play | action::World_preset(_) => r.world,
            _ => true,
        }
    };
    let kept: Vec<editor_commands::Command> = editor_commands::keymap().iter().filter(keep).cloned().collect();
    let routed = Rc::new(layout_labels(value, kept));
    LAST.with(|l| *l.borrow_mut() = Some((r, routed.clone())));
    routed
}

