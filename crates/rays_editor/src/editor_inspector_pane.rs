//! Port of crates/rays_editor/editor_inspector_pane.ml: the Inspector panel of a frame. Which subject
//! it shows: the node selected in the graph pane, a scene object or World layer, the settings and
//! camera panel when nothing is selected. Bundle C.
#![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use std::cell::RefCell;
use std::rc::Rc;

use editor_document::document::{self as doc_mod, Level};
use editor_document::kind_graph;
use editor_document::settings;
use flow_graph::flow_edit::{self as fe, ArgKey};
use flow_graph::projection as p;
use flow_sop::port;
use flow_syntax::syntax::Node as SNode;
use param::{FieldView, Value};
use pxui::ui::{Drop, Ui, UiBox};
use pxui_graph::scope_state as scope;
use pxui_shell::inspector as insp;
use pxui_shell::layout;
use sop::edit_graph;
use sop::node;

use crate::carry;
use crate::editor_inspect::*;
use crate::editor_model::*;
use crate::editor_panes::*;
use crate::editor_scope::scene_target;
use crate::editor_shell::*;
use crate::editor_status::level_name;
use crate::selection;
use crate::text_pane;

/// What the inspector hands back for a frame: the sketch's panel result, the graph requests, the
/// settings changes, the live-cook flag, the probe moves and the places a carried payload hovers.
pub type Inspector_result = (Option<Panel_result>, Vec<change>, Vec<(String, Value)>, bool, Vec<scope::Change>, Vec<(carry::Place, bool)>);

/// The inspector panel inside `inspector_root` at `bounds`.
#[allow(clippy::too_many_arguments)]
pub fn inspect_panel<'a>(
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    camera_panel: &dyn Fn(&mut Ui) -> Panel_result,
    inspector_visible: bool,
    unchanged: &settings::T,
    resized: Resized,
    g: &layout::Geometry,
    ui: &mut Ui,
    input: (ModelView<'a>, &scope::Pane, &selection::Selection, bool),
    inspector_root: &UiBox,
    bounds: layout::Bounds,
) -> Inspector_result {
    let (value, scope_view, selection_, scope_active) = input;
    let value = &value;
    let ws = &value.doc.workspace.ws;
    let document = objects(value);
    let selected_ids: Vec<i64> = selection::selected_nodes(selection_).to_vec();
    let level = pane_level(value);
    let in_geometry_level = |level: Level| match level {
        Level::Inside(g_) => kind(value, g_).as_deref() == Some("geometry"),
        Level::Scene => false,
    };
    let target = match selected_ids.as_slice() {
        [id] if !in_geometry_level(level) => scene_target(value, *id),
        _ => None,
    };
    let lowered_level = in_geometry_level(level);
    // a row of a geometry object's list selects its node in the pane: the inspector follows it
    let scope_selected: Vec<flow_check::workspace::Path> = if pane_laid(value).map_or(false, |k| Some(&k.graph) == graph_name(value).as_ref()) || (lowered_level && pane_laid(value).is_some()) {
        inspector_paths(scope::selected(scope_view))
    } else {
        Vec::new()
    };
    let window = g.leaves.iter().any(|l| l.floating && l.panel == layout::Panel::Inspector && l.body == bounds);
    // a window keeps its 1-point border on the left, right and bottom: its panel lies inside
    let bounds = if window {
        let (x, y, w, h) = bounds;
        (x, y, w.max(1), (h - 1).max(1))
    } else {
        bounds
    };
    let width = bounds.2.max(1) as f64;
    let mut result: Option<Inspector_result> = None;
    on_sheet(ui, window, &mut |ui| {
        ui.within(inspector_root, &mut |ui| {
            let out: Inspector_result = if !inspector_visible {
                (None, vec![], vec![], value.live_cook, vec![], vec![])
            } else if !scope_selected.is_empty() {
                match scope_selected.as_slice() {
                    [path] => {
                        // the inspector reports each choice control it builds: the material one is
                        // a place a carried payload can be put, gathered here and returned with
                        // the rows' requests
                        let drops: Rc<RefCell<Vec<(carry::Place, bool)>>> = Rc::new(RefCell::new(Vec::new()));
                        let mut inspected = (vec![], vec![]);
                        inspector_panel(window, ui, bounds, &mut |ui| {
                            let follows = |pth: &[Symbol]| follow_target(value, Some(pth));
                            let opts = InspectorOpts { image, laid: None, window, on_choice: None, resized, follows: &follows };
                            inspected = workspace_inspector_with_drops(value, ui, width, path, &opts, &drops);
                        });
                        let drops = drops.borrow().clone();
                        (None, inspected.0, vec![], value.live_cook, inspected.1, drops)
                    }
                    paths => {
                        inspector_panel(window, ui, bounds, &mut |ui| {
                            ui.inspector_header("multi-header", None, None, None, None, &[], None, false, &format!("{} nodes", paths.len()), "Selected");
                        });
                        (None, vec![], vec![], value.live_cook, vec![], vec![])
                    }
                }
            } else if selected_ids.is_empty() {
                // Sketch settings above the environment's camera/render panel.
                let mut changes: Vec<(String, Value)> = Vec::new();
                let mut live = value.live_cook;
                let mut panel: Option<Panel_result> = None;
                inspector_panel(window, ui, bounds, &mut |ui| {
                    let (node_count, label): (usize, Box<dyn Fn(i64) -> Option<String>>) = match network(value) {
                        Some(network) => {
                            let geometry = network.graph.geometry.clone();
                            (edit_graph::inspect(&geometry).len(), Box::new(move |id| edit_graph::find(&geometry, id).map(|n| node::label(&n).to_owned())))
                        }
                        None => {
                            let document = document.clone();
                            (document.len(), Box::new(move |id| kind_graph::find(&document, id).map(|n| n.label.clone())))
                        }
                    };
                    let display = match displayed(value) {
                        Some(id) => label(id).unwrap_or_else(|| format!("#{id}")),
                        None => "none".to_owned(),
                    };
                    let (title, detail) = match pane_laid(value) {
                        Some(k) if scope_active && Some(&k.graph) == graph_name(value).as_ref() => {
                            fn count(s: &p::Scope) -> usize {
                                s.nodes.iter().fold(0, |n, x| n + 1 + x.zone.as_ref().map_or(0, |z| count(&z.scope)))
                            }
                            let zones = p::zones(&k.scope).len();
                            let nodes = count(&k.scope);
                            (
                                k.graph.clone(),
                                format!("{} node{} \u{b7} {} loop{} \u{b7} select a node to edit it", nodes, if nodes == 1 { "" } else { "s" }, zones, if zones == 1 { "" } else { "s" }),
                            )
                        }
                        _ => (level_name(value), format!("{} node{} \u{b7} display {}", node_count, if node_count == 1 { "" } else { "s" }, display)),
                    };
                    ui.inspector_header("network-header", None, None, None, None, &[], None, false, &title, &detail);
                    ui.inspector_body(&mut |ui| {
                        // the row sits under a section named for the graph, as every row of the sheet does
                        let mut section_live = None;
                        ui.inspector_section("live-cook-section", true, None, &title, &mut |ui| {
                            section_live = Some(ui.inspector_toggle("live-cook", "Live update", value.live_cook));
                        });
                        live = section_live.unwrap_or(value.live_cook);
                        let mut fields: Vec<FieldView> = settings::fields(unchanged).into_iter().filter(|f| f.name != "renderer").collect();
                        if !fields.is_empty() {
                            ui.scope("sketch-settings", &mut |ui| {
                                ui.inspector_section("sketch-settings-section", true, None, "Settings", &mut |ui| {
                                    changes = insp::fields(ui, &[], Some(bounds.2 as f64), std::mem::take(&mut fields));
                                });
                            });
                        }
                        panel = Some(camera_panel(ui));
                    });
                });
                (panel, vec![], changes, live, vec![], vec![])
            } else if selected_ids.len() >= 2 {
                inspector_panel(window, ui, bounds, &mut |ui| {
                    ui.inspector_header("multi-header", None, None, None, None, &[], None, false, &format!("{} nodes", selected_ids.len()), "Selected");
                });
                (None, vec![], vec![], value.live_cook, vec![], vec![])
            } else if lowered_level {
                let node_id = selected_ids[0];
                // the nodes of a geometry object are the lowering of its graph: the graph pane's
                // node is the one to edit (its arguments are the text), and its inspector is the
                // graph pane's
                let mut inspected: Inspected = (vec![], vec![]);
                inspector_panel(window, ui, bounds, &mut |ui| match node_path(value, node_id) {
                    Some(path) => {
                        let follows = |pth: &[Symbol]| follow_target(value, Some(pth));
                        let opts = InspectorOpts { image, laid: None, window, on_choice: None, resized, follows: &follows };
                        inspected = workspace_inspector(value, ui, width, &path, &opts);
                    }
                    None => ui.inspector_message("flow-derived", "Made from the graph's text. Select the node in the graph pane to edit its arguments."),
                });
                (None, inspected.0, vec![], value.live_cook, inspected.1, vec![])
            } else if let Some((laid, path)) = &target {
                // a scene object or World layer the text binds: the one inspector, over the
                // projection of its graph (the pane's own, or one made for the sheet)
                let mut inspected: Inspected = (vec![], vec![]);
                inspector_panel(window, ui, bounds, &mut |ui| {
                    let follows = |pth: &[Symbol]| follow_target(value, Some(pth));
                    let opts = InspectorOpts { image, laid: Some(laid.clone()), window, on_choice: None, resized, follows: &follows };
                    inspected = workspace_inspector(value, ui, width, &path, &opts);
                });
                (None, inspected.0, vec![], value.live_cook, inspected.1, vec![])
            } else {
                let node_id = selected_ids[0];
                let changes = object_inspector(value, ui, &document, node_id, bounds, window, ws);
                (None, changes, vec![], value.live_cook, vec![], vec![])
            };
            result = Some(out);
        });
    });
    result.expect("inspector drawn")
}

/// `workspace_inspector` for a node of the graph pane, gathering the places the choice controls
/// offer a carried payload (the material one): the inspector reports each choice control it builds.
fn workspace_inspector_with_drops<'a>(value: &ModelView<'a>, ui: &mut Ui, width: f64, path: &[Symbol], opts: &InspectorOpts, drops: &Rc<RefCell<Vec<(carry::Place, bool)>>>) -> Inspected {
    let place_path = path.to_vec();
    let drops_in = drops.clone();
    // the callback needs the Ui to read the drop target: collect the boxes, ask after
    let boxes: Rc<RefCell<Vec<(String, UiBox)>>> = Rc::new(RefCell::new(Vec::new()));
    let boxes_in = boxes.clone();
    let on_choice = move |name: &str, boxed: &UiBox| boxes_in.borrow_mut().push((name.to_owned(), boxed.clone()));
    let inner = InspectorOpts { image: opts.image, laid: opts.laid.clone(), window: opts.window, on_choice: Some(&on_choice), resized: opts.resized, follows: opts.follows };
    let out = workspace_inspector(value, ui, width, path, &inner);
    for (name, boxed) in boxes.borrow().iter() {
        if name == "@ref:material" {
            if let Some(d) = ui.drop_target(boxed) {
                drops_in.borrow_mut().push((carry::Place::Node(place_path.clone()), matches!(d, Drop::Dropped(_))));
            }
        }
    }
    out
}

/// The sheet of a scene object or World layer the text does not bind by name: its fields, with
/// the name edited in place.
fn object_inspector<'a>(value: &ModelView<'a>, ui: &mut Ui, document: &kind_graph::T, node_id: i64, bounds: layout::Bounds, window: bool, ws: &editor_document::workspace_doc::T) -> Vec<change> {
    let (fields, label, kind_) = match kind_graph::find(document, node_id) {
        Some(node) => (kind_graph::fields(&node), node.label.clone(), node.kind.clone()),
        None => (Vec::new(), format!("#{node_id}"), "unknown".to_owned()),
    };
    let preview = pane_level(value) == Level::Scene
        && value.doc.shell.as_ref().map_or(false, |shell| shell.views.iter().any(|(_, ids)| ids.contains(&node_id)));
    let mut changes: Vec<change> = Vec::new();
    inspector_panel(window, ui, bounds, &mut |ui| {
        ui.scope(&format!("node.{node_id}"), &mut |ui| {
            let inspector_width = bounds.2.max(1) as f64;
            let input = kind_graph::find(document, node_id).and_then(|n| n.input);
            let input_count = if kind_ == "camera" || kind_ == "root" || kind_ == "world" { 0 } else { 1 };
            // the head: the kind as the sheet writes it (scene/camera), VIEW while displayed, the
            // node's number; the name edited in place; what is true of the node
            let mut facts: Vec<String> = Vec::new();
            if value.doc.active_camera == Some(node_id) {
                facts.push("active camera".to_owned());
            }
            if input_count > 0 {
                facts.push(format!("{} input{}", input_count, if input_count == 1 { "" } else { "s" }));
            }
            if preview {
                facts.push("preview instance".to_owned());
            }
            let detail = if facts.is_empty() {
                format!("{} parameter{}", fields.len(), if fields.len() == 1 { "" } else { "s" })
            } else {
                facts.join(" \u{b7} ")
            };
            let rename: Option<Rc<dyn Fn(&str) -> bool>> = if preview { None } else { Some(Rc::new(|text: &str| !text.trim().is_empty())) };
            let head = ui.inspector_header(
                "flow-inspector-header",
                Some(&format!("{}/{}", if pane_level(value) == Level::Scene { "scene" } else { "world" }, kind_)),
                if displayed(value) == Some(node_id) { Some("view") } else { None },
                Some(&format!("NO. {node_id:04}")),
                rename,
                &[],
                None,
                false,
                &label,
                &detail,
            );
            if head.renamed != label && !preview {
                changes.push(change::Rename { node: node_id, label: head.renamed.clone() });
            }
            let mut body: Vec<change> = Vec::new();
            ui.inspector_body(&mut |ui| {
                if input_count > 0 {
                    ui.inspector_section("flow-input-heading", true, None, "Inputs", &mut |ui| {
                        let source = match input {
                            None => "empty".to_owned(),
                            Some(id) => match kind_graph::find(document, id) {
                                Some(n) => n.label.clone(),
                                None => format!("#{id}"),
                            },
                        };
                        ui.inspector_readout(Some(inspector_width), "flow-input-0", "in0", &format!("\u{2190} {source}"));
                    });
                }
                if preview {
                    ui.inspector_message("flow-preview", "Preview instance. Edit its viewport scene reference or source graph.");
                }
                let rows = match object_rows(preview, &fields) {
                    Err(diagnostic) => {
                        ui.inspector_message("flow-diagnostic", &rays_diagnostic::to_string(&diagnostic));
                        Vec::new()
                    }
                    Ok(rows) => rows,
                };
                if rows.is_empty() {
                    return;
                }
                let mut expanded: Vec<String> = fields.iter().filter_map(|f| f.folder.first().cloned()).collect();
                expanded.sort();
                expanded.dedup();
                let opts = insp::FlowOpts { expanded, width: Some(inspector_width), kind_label: Some(kind_label(&kind_)), ..Default::default() };
                for ev in insp::flow_fields(ui, &opts, None, &rows) {
                    match ev {
                        insp::FlowChange::Edited(path, v) => body.push(change::Set_parameter { node: node_id, path, value: v }),
                        insp::FlowChange::Expression(path, text) => {
                            // a row is a field of the node; its argument is the parameter holding it
                            // (a vector's field is one component of it)
                            let parameters = port::parameters(true, &fields).unwrap_or_default();
                            let key_sub = parameters.iter().find_map(|pm| match pm.fields.iter().position(|f| f.name == path) {
                                Some(i) => Some((pm.path.clone(), if pm.fields.len() == 3 { vec![i as i64] } else { vec![] })),
                                None => {
                                    // a vector's component row is named `row.axis`
                                    let found = ["x", "y", "z"].iter().position(|axis| format!("{}.{}", pm.path, axis) == path).map(|i| (pm.path.clone(), vec![i as i64]));
                                    if pm.fields.len() == 3 { found } else { None }
                                }
                            });
                            match (expression_text(&text), key_sub) {
                                (Ok(expr), Some((key, sub))) => body.push(change::Object_arg { node: node_id, key, sub, expr }),
                                (Err(message), _) => body.push(change::Declined(message)),
                                (Ok(_), None) => {}
                            }
                        }
                        insp::FlowChange::Follow(path) => {
                            let home = match pane_level(value) {
                                Level::Scene => value.doc.homes.objects.iter().find(|(i, _)| *i == node_id).map(|(_, h)| h.clone()),
                                Level::Inside(_) => value.doc.homes.layers.iter().find(|(i, _)| *i == node_id).map(|(_, h)| h.clone()),
                            };
                            if let Some(doc_mod::Home::BoundAt(from)) = home {
                                if let Some(form) = fe::arg_text(&ws.source, &from, &ArgKey::Kw(path.clone()))
                                    && let SNode::Sym(name) = form.node {
                                    if let Some(target) = text_pane::reference_target(&ws.source, &from, form.name(name)) {
                                        body.push(change::Follow_source(target));
                                    }
                                }
                            }
                        }
                        insp::FlowChange::Pinned(..) | insp::FlowChange::Reset(..) => {}
                    }
                }
            });
            changes.extend(body);
        });
    });
    changes
}
