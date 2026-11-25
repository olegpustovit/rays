//! the Inspector panel of a frame. Which subject
//! it shows: the node selected in the graph pane, a scene object or World layer, the settings and
//! camera panel when nothing is selected. Bundle C.

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
use pxui::ui::{Ui};
use rays_graph::scope_state as scope;
use rays_shell::inspector as insp;
use rays_shell::layout;
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
pub type InspectorResult = (Option<PanelResult>, Vec<Change>, Vec<(String, Value)>, bool, Vec<scope::Change>, Vec<(carry::Place, bool)>);

/// What a drag of the inspector's held number edits, made by the build its press built: the
/// frame's changes, or its settings changes.
pub type Follow = Rc<dyn Fn(&Value, &mut Vec<Change>, &mut Vec<(String, Value)>)>;

/// The inspector's held number (`insp::FlowOpts::kept_drag`): what its drag edits, the inspector
/// whose build made it (its root's key; `presses` counts the builds that made one), the revision
/// that inspector was last declared with, and whether the last frame followed the drag by a patch.
struct Followed {
    edit: Option<Follow>,
    presses: u64,
    root: i64,
    revision: u64,
    held: bool,
}

thread_local! {
    static FOLLOWED: RefCell<Followed> = const { RefCell::new(Followed { edit: None, presses: 0, root: 0, revision: 0, held: false }) };
}

/// The build of a press on a number of the rows: what its drag edits (`None`: nothing to follow).
pub fn follow(edit: Option<Follow>) {
    FOLLOWED.with(|f| {
        let mut f = f.borrow_mut();
        (f.edit, f.presses) = (edit, f.presses + 1);
    });
}

/// The inspector panel inside `inspector_root` at `bounds`.
#[allow(clippy::too_many_arguments)]
pub fn inspect_panel<'a>(
    frame: &input::frame::Frame,
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    camera_panel: &dyn Fn(&mut Ui) -> PanelResult,
    inspector_visible: bool,
    unchanged: &settings::T,
    resized: Resized,
    g: &layout::Geometry,
    ui: &mut Ui,
    input: (ModelView<'a>, &scope::Pane, &selection::Selection, bool),
    inspector_root: &pxui::Handle,
    bounds: layout::Bounds,
) -> InspectorResult {
    let (value, scope_view, selection_, scope_active) = input;
    let value = &value;
    let selected_ids = selection::selected_nodes(selection_);
    let level = pane_level(value);
    let window = g.leaves.iter().any(|l| l.floating && l.panel == layout::Panel::Inspector && l.body == bounds);
    // a window keeps its 1-point border on the left, right and bottom: its panel lies inside
    let bounds = if window {
        let (x, y, w, h) = bounds;
        (x, y, w.max(1), (h - 1).max(1))
    } else {
        bounds
    };
    let width = bounds.2.max(1) as f32;
    // The inspector is one cached view keyed by the revisions of what it reads: the document, the laid
    // out graph and its probe records, the zone probes and previews, the cook's results, the camera
    // panel's inputs, the selection and the image it previews (that image's identity and generation,
    // re-read every frame: a traced or animated image changes without the document). Input on its
    // boxes rebuilds it, and its intents arrive on that frame; a kept frame hands back no requests.
    // While a payload is carried, or a sketch builds its own widgets under the camera panel (no
    // revision), it is built every frame. Inside it each section is a content-sized view of its own
    // (`Ui::inspector_section_view`) keyed by what that section shows: input on one section (a
    // scrubbed value) rebuilds that section and this view around it; the other sections are kept and
    // move through their view offsets when one above them grows or shrinks.
    let previewed = ui.state(inspector_root, -1);
    let preview_key = (previewed >= 0).then(|| image(previewed)).flatten().map(|im| (rays::image::identity(&im), rays::image::generation(&im)));
    // the cook's results show only for a selected node (its counts, its compiled arguments)
    let cook_rev = if selected_ids.is_empty() && scope_view.selected.is_empty() { 0 } else { value.cook.results };
    let live = rays_shell::kit::revision(&|h| {
        use std::hash::Hash;
        let level_key = match level {
            Level::Scene => (0u8, 0i64),
            Level::Inside(id) => (1, id),
        };
        (value.revs, cook_rev, preview_key, inspector_visible, window, bounds, scope_active, value.live_cook).hash(h);
        (level_key, &pane(value).shown, selected_ids, selection_.primary, scope_view.selected.as_slice(), value.carry.is_some()).hash(h);
    });
    let cacheable = value.revs.panel.is_some() && value.carry.is_none();
    // while a number of the rows is dragged and followed by patches (below) the inspector stays as it
    // was declared: the document and the cook move with the drag's own edits; its release builds it
    // as they are now
    // ponytail: an edit from elsewhere landing mid-drag (a host's) shows at the release; key the
    // revision by what the drag's edit leaves out if that ever matters
    let (root, presses) = (inspector_root.box_key, FOLLOWED.with(|f| f.borrow().presses));
    let dragging = cacheable && FOLLOWED.with(|f| f.borrow().root == root) && insp::dragging(ui);
    let revision = FOLLOWED.with(|f| {
        let f = f.borrow();
        if dragging && f.held { f.revision } else { live }
    });
    let shown_image: std::cell::Cell<Option<i64>> = std::cell::Cell::new(None);
    let image_seen = |id: i64| {
        let im = image(id);
        if im.is_some() {
            shown_image.set(Some(id));
        }
        im
    };
    let image: &dyn Fn(i64) -> Option<rays::image::Image> = &image_seen;
    let mut result: Option<InspectorResult> = None;
    let mut build = |ui: &mut Ui| {
            let ws = &value.doc.workspace.ws;
            let document = objects(value);
            let lowered_level = matches!(level, Level::Inside(id) if kind(value, id).as_deref() == Some("geometry"));
            // A row of a geometry object's list selects its node in the pane.
            let scope_selected = if pane_laid(value).is_some_and(|k| lowered_level || Some(&k.graph) == graph_name(value).as_ref()) {
                inspector_paths(scope::selected(scope_view).to_vec())
            } else {
                Vec::new()
            };
            let out: InspectorResult = if !inspector_visible {
                (None, vec![], vec![], value.live_cook, vec![], vec![])
            } else if !scope_selected.is_empty() {
                match scope_selected.as_slice() {
                    [path] => {
                        let mut inspected = (vec![], vec![]);
                        let mut drops = Vec::new();
                        inspector_panel(window, ui, bounds, &mut |ui| {
                            let follows = |pth: &[Symbol]| follow_target(value, Some(pth));
                            let material = std::cell::Cell::new(None);
                            let choice = |name: &str, boxed: &pxui::Handle| {
                                if name == "@ref:material" {
                                    material.set(Some(*boxed));
                                }
                            };
                            let opts = InspectorOpts {
                                image, laid: None, window, resized, follows: &follows,
                                on_choice: value.carry.as_ref().map(|_| &choice as &dyn Fn(&str, &pxui::Handle)),
                            };
                            inspected = workspace_inspector(value, ui, width, path, &opts);
                            if let Some(boxed) = material.get() && let Some(drop) = ui.drop_target(&boxed) {
                                drops.push((carry::Place::Node(path.clone()), matches!(drop, pxui::DropRef::Dropped(_))));
                            }
                        });
                        (None, inspected.0, vec![], value.live_cook, inspected.1, drops)
                    }
                    paths => {
                        let mut changes = Vec::new();
                        inspector_panel(window, ui, bounds, &mut |ui| {
                            changes = shared_inspector(ui, width, paths.len(), &|i| SharedTarget::graph(value, pane_laid(value)?, &paths[i]));
                        });
                        (None, changes, vec![], value.live_cook, vec![], vec![])
                    }
                }
            } else if selected_ids.is_empty() {
                // Sketch settings above the environment's camera/render panel.
                let mut changes: Vec<(String, Value)> = Vec::new();
                let mut live = value.live_cook;
                let mut panel: Option<PanelResult> = None;
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
                    insp::header_context(ui, &title, false, false, &mut |ui| {
                        let _ = pxui::ext::inspector_header(ui, "network-header", None, None, None, None, &[], None, false, &title, &detail);
                    });
                    pxui::ext::inspector_body(ui, &mut |ui| {
                        // the row sits under a section named for the graph, as every row of the sheet does
                        let mut section_live = None;
                        let shown = rays_shell::kit::revision(&|h| std::hash::Hash::hash(&value.live_cook, h));
                        rays_shell::inspector::section(ui, "live-cook-section", shown, true, None, &title, &mut |ui| {
                            section_live = Some(pxui::ext::inspector_toggle(ui, "live-cook", "Live update", value.live_cook));
                        });
                        live = section_live.unwrap_or(value.live_cook);
                        let mut fields: Vec<FieldView> = settings::fields(unchanged).into_iter().filter(|f| f.name != "renderer").collect();
                        if !fields.is_empty() {
                            let shown = rays_shell::kit::debug_revision(&(&fields, bounds.2));
                            ui.id("sketch-settings", |ui| {
                                rays_shell::inspector::section(ui, "sketch-settings-section", shown, true, None, "Settings", &mut |ui| {
                                    changes = insp::fields(ui, &[], Some(bounds.2 as f32), true, std::mem::take(&mut fields));
                                    if let Some(name) = insp::pressed() {
                                        follow(Some(Rc::new(move |v: &Value, _: &mut Vec<Change>, settings: &mut Vec<(String, Value)>| settings.push((name.clone(), v.clone())))));
                                    }
                                });
                            });
                        }
                        panel = Some(camera_panel(ui));
                    });
                });
                (panel, vec![], changes, live, vec![], vec![])
            } else if selected_ids.len() >= 2 {
                let mut changes = Vec::new();
                inspector_panel(window, ui, bounds, &mut |ui| {
                    changes = shared_inspector(ui, width, selected_ids.len(), &|i| {
                        let id = selected_ids[i];
                        if lowered_level {
                            let path = node_path(value, id)?;
                            SharedTarget::graph(value, pane_laid(value)?, &path)
                        } else if let Some((laid, path)) = inspector_scene_target(value, id) {
                            SharedTarget::graph(value, &laid, &path)
                        } else {
                            document.iter().find(|n| n.id == id).map(SharedTarget::object)
                        }
                    });
                });
                (None, changes, vec![], value.live_cook, vec![], vec![])
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
                    None => pxui::ext::inspector_message(ui, "flow-derived", "Made from the graph's text. Select the node in the graph pane to edit its arguments."),
                });
                (None, inspected.0, vec![], value.live_cook, inspected.1, vec![])
            } else {
                let node_id = selected_ids[0];
                if let Some((laid, path)) = inspector_scene_target(value, node_id) {
                    let mut inspected: Inspected = (vec![], vec![]);
                    inspector_panel(window, ui, bounds, &mut |ui| {
                        let follows = |pth: &[Symbol]| follow_target(value, Some(pth));
                        let opts = InspectorOpts { image, laid: Some(&laid), window, on_choice: None, resized, follows: &follows };
                        inspected = workspace_inspector(value, ui, width, &path, &opts);
                    });
                    (None, inspected.0, vec![], value.live_cook, inspected.1, vec![])
                } else {
                    let changes = object_inspector(value, ui, &document, node_id, bounds, window, ws);
                    (None, changes, vec![], value.live_cook, vec![], vec![])
                }
            };
            result = Some(out);
    };
    let mut built = false;
    on_sheet(ui, window, &mut |ui| {
        rays_shell::chrome::within_pane(ui, inspector_root, frame, bounds, &mut |ui| {
            if cacheable {
                rays_shell::kit::cached(ui, "inspector-view", revision, &mut |ui| {
                    built = true;
                    build(ui);
                });
            } else {
                built = true;
                build(ui);
            }
        });
    });
    if built {
        ui.set_state(inspector_root, shown_image.get().unwrap_or(-1));
        // a held number no row made a follow for: its drag edits nothing (not an older press's edit)
        if insp::pressed().is_some() {
            follow(None);
        }
    }
    // a frame of the drag: the field patched, and on a kept frame its edit made here (a build made
    // it itself)
    let (mut followed, mut settings) = (Vec::new(), Vec::new());
    let held = dragging && match insp::dragged(ui) {
        Some((patched, edit)) => {
            FOLLOWED.with(|f| {
                if let (Some(follow), Some((_, v))) = (&f.borrow().edit, edit) {
                    follow(&v, &mut followed, &mut settings);
                }
            });
            patched
        }
        None => false,
    };
    FOLLOWED.with(|f| {
        let mut f = f.borrow_mut();
        if f.presses != presses {
            f.root = root;
        }
        if f.root == root {
            (f.revision, f.held) = (revision, held);
        }
    });
    result.unwrap_or_else(|| (None, followed, settings, value.live_cook, vec![], vec![]))
}

fn inspector_scene_target<'a>(value: &ModelView<'a>, node: i64) -> Option<(std::borrow::Cow<'a, ScopeKey>, std::borrow::Cow<'a, [Symbol]>)> {
    use std::borrow::Cow;
    let homes = match pane_level(value) {
        Level::Scene => &value.doc.homes.objects,
        Level::Inside(_) => &value.doc.homes.layers,
    };
    let (_, doc_mod::Home::BoundAt(path)) = homes.iter().find(|(id, _)| *id == node)? else { return None };
    if let Some(laid) = pane_laid(value) && path.len() >= 2 && path[0].as_str() == laid.graph {
        return inspector_node(&laid.scope, path).map(|_| (Cow::Borrowed(laid), Cow::Borrowed(path.as_slice())));
    }
    scene_target(value, node).map(|(laid, path)| (Cow::Owned(laid), Cow::Owned(path)))
}

/// The sheet of a scene object or World layer the text does not bind by name: its fields, with
/// the name edited in place.
fn object_inspector<'a>(value: &ModelView<'a>, ui: &mut Ui, document: &[kind_graph::Node], node_id: i64, bounds: layout::Bounds, window: bool, ws: &editor_document::workspace_doc::T) -> Vec<Change> {
    let (fields, label, kind_) = match kind_graph::find(document, node_id) {
        Some(node) => (kind_graph::fields(&node), node.label.clone(), node.kind.clone()),
        None => (Vec::new(), format!("#{node_id}"), "unknown".to_owned()),
    };
    let preview = pane_level(value) == Level::Scene
        && value.doc.shell.as_ref().map_or(false, |shell| shell.views.iter().any(|(_, ids)| ids.contains(&node_id)));
    let mut changes: Vec<Change> = Vec::new();
    inspector_panel(window, ui, bounds, &mut |ui| {
        ui.id(pxui::hash_of(&("node", node_id)), |ui| {
            let inspector_width = bounds.2.max(1) as f32;
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
            let (renamed, _) = insp::header_context(ui, &label, false, false, &mut |ui| {
                let head = pxui::ext::inspector_header(ui,
                "flow-inspector-header",
                Some(&format!("{}/{}", if pane_level(value) == Level::Scene { "scene" } else { "world" }, kind_)),
                if displayed(value) == Some(node_id) { Some("view") } else { None },
                Some(&format!("NO. {node_id:04}")),
                rename.clone(),
                &[],
                None,
                false,
                &label,
                &detail,
                );
                if preview { None } else { object_rename(&label, head.renamed) }
            });
            if let Some(label) = renamed {
                changes.push(Change::Rename { node: node_id, label });
            }
            let mut body: Vec<Change> = Vec::new();
            pxui::ext::inspector_body(ui, &mut |ui| {
                if input_count > 0 {
                    pxui::ext::inspector_section(ui, "flow-input-heading", true, None, "Inputs", &mut |ui| {
                        let source = match input {
                            None => "empty".to_owned(),
                            Some(id) => match kind_graph::find(document, id) {
                                Some(n) => n.label.clone(),
                                None => format!("#{id}"),
                            },
                        };
                        pxui::ext::inspector_readout(ui, Some(inspector_width), "flow-input-0", "in0", &format!("\u{2190} {source}"));
                    });
                }
                if preview {
                    pxui::ext::inspector_message(ui, "flow-preview", "Preview instance. Edit its viewport scene reference or source graph.");
                }
                let rows = match object_rows(preview, &fields) {
                    Err(diagnostic) => {
                        pxui::ext::inspector_message(ui, "flow-diagnostic", &rays_diagnostic::to_string(&diagnostic));
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
                let opts = insp::FlowOpts { expanded, width: Some(inspector_width), kind_label: Some(kind_label(&kind_)), kept_drag: true, ..Default::default() };
                let events = insp::flow_fields(ui, &opts, None, &rows);
                if let Some(path) = insp::pressed() {
                    follow(Some(Rc::new(move |v: &Value, changes: &mut Vec<Change>, _: &mut Vec<(String, Value)>| changes.push(Change::SetParameter { node: node_id, path: path.clone(), value: v.clone() }))));
                }
                for ev in events {
                    match ev {
                        insp::FlowChange::Edited(path, v) => body.push(Change::SetParameter { node: node_id, path, value: v }),
                        insp::FlowChange::EditedVector(path, channels) => {
                            if let Some(row) = rows.iter().find(|r| r.path == path) {
                                body.extend(row.fields.iter().zip(channels).map(|(field, v)| Change::SetParameter {
                                    node: node_id, path: field.name.clone(), value: Value::Float(v),
                                }));
                            }
                        }
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
                                (Ok(expr), Some((key, sub))) => body.push(Change::ObjectArg { node: node_id, key, sub, expr }),
                                (Err(message), _) => body.push(Change::Declined(message)),
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
                                        body.push(Change::FollowSource(target));
                                    }
                                }
                            }
                        }
                        insp::FlowChange::Pinned(..) | insp::FlowChange::Reset(..) | insp::FlowChange::Reveal(..) => {}
                    }
                }
            });
            changes.extend(body);
        });
    });
    changes
}

fn object_rename(current: &str, entered: &str) -> Option<String> {
    let entered = entered.trim();
    (!entered.is_empty() && entered != current).then(|| entered.to_owned())
}

#[cfg(test)]
#[path = "test_inspector_rename.rs"]
mod test_inspector_rename;
