//! The host's entry points into the model: scene bookkeeping, settings and root edits, host
//! gestures, the World, placed pieces, picking, reload and the carry's surface. Port of
//! lib/rays_editor/editor_host.ml.
//!
//! OCaml optional arguments are `Option` parameters, placed as in the OCaml signature.
#![allow(non_camel_case_types)]
use std::rc::Rc;
use std::sync::Arc;

use editor_document::document::{self, Level, T as Document};
use editor_document::kind_graph as kg;
use editor_document::layers;
use editor_document::objects;
use editor_document::scene_sync::{self, Edit};
use editor_document::{document_edit, layout_edit, settings};
use flow_graph::flow_edit as fe;
use flow_graph::probe;
use flow_graph::projection as fproj;
use flow_syntax::context;
use flow_syntax::phase_timer::SampleGuard;
use flow_syntax::value::Value as EvalValue;
use param::Value;
use rays::scene as ui_scene;
use rays::world;
use rays_diagnostic::Diagnostic;
use rays_math::{mat4, vec3};

use crate::carry::Place;
use crate::cook;
use crate::editor_history::{self as history, Merge};
use crate::editor_inspect::{scene, view_wants, View};
use crate::editor_list::pieces;
use crate::editor_model::*;
use crate::editor_panes::{go, scope_name};
use crate::editor_text::{adopt, commit, install, parameter_gesture, same_text};
use crate::text_pane;

/// How `scene_edit` records: `Reset` starts the history, `Amend` folds into the present entry and
/// keeps what redo would restore.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum scene_mode {
    Reset,
    Amend,
}

/// Editor_loop-owned scene edits (camera bookkeeping, a camera following the viewport): view
/// state, never an undo entry. `active_camera`: `None` is OCaml's omitted argument (the document's
/// own).
pub fn scene_edit(value: &mut EditorModel, mode: scene_mode, active_camera: Option<Option<i64>>, scene_graph: kg::T) {
    if value.carry.is_some() {
        return;
    }
    let active_camera = active_camera.unwrap_or(value.doc.active_camera);
    let mut edited = (*value.doc).clone();
    edited.scene = scene_graph;
    // a camera that follows the viewport is an edit of the text when the text declares it
    let result = document_edit::reconcile(Some(&value.factories), Some(false), &value.doc, &edited).and_then(|doc| {
        if active_camera == doc.active_camera {
            Ok(doc)
        } else {
            scene_sync::write(Some(&value.factories), &doc, Level::Scene, &Edit::Camera(active_camera)).map(|written| match written {
                Some((d, _)) => d,
                None => doc,
            })
        }
    });
    match result {
        Err(message) => {
            value.edit_error = Some(message);
        }
        Ok(doc) => {
            let doc = adopt(&value.history, Arc::new(doc));
            let history = match mode {
                scene_mode::Reset => history::restart(doc.clone(), value.history.clone()),
                scene_mode::Amend => commit(&doc, None, Some(Merge::Repair), value.history.clone()),
            };
            // camera bookkeeping is not unsaved work: a document that was the file's still is
            let filed = if same_text(&value.doc, &value.filed) { document::text(&doc) } else { value.filed.clone() };
            value.doc = doc;
            value.history = history;
            value.filed = filed;
        }
    }
}

pub fn machinery<'a>(value: &ModelView<'a>, under: Option<&dyn Fn(i64) -> Vec<ui_scene::node>>, all_ui_visible: bool) -> Vec<ui_scene::node> {
    if all_ui_visible || *value.leader != editor_core::router::State::Idle { value.ui.borrow().scene(under) } else { Vec::new() }
}

pub fn close(value: &EditorModel) {
    let _ = history::release(value.history.clone());
    value.ui.borrow_mut().destroy();
    cook::close(&value.cook);
}

/// A sketch-driven settings change: one undo step and a fresh cook, since `prepare` reads the
/// settings.
pub fn set_settings(value: &mut EditorModel, new_settings: &settings::T) {
    if settings::same_values(new_settings, &value.doc.settings) || value.carry.is_some() {
        return;
    }
    match scene_sync::write(Some(&value.factories), &value.doc, Level::Scene, &Edit::Settings(new_settings.clone())) {
        Err(message) => {
            value.edit_error = Some(message);
        }
        Ok(None) => {},
        Ok(Some((doc, _))) => {
            let doc = adopt(&value.history, Arc::new(doc));
            let history = commit(&doc, Some("Settings"), None, value.history.clone());
            value.cook = std::rc::Rc::new(cook::force(&value.cook));
            value.doc = doc;
            value.history = history;
        }
    }
}

/// An edit of the render settings (the root): written to the root's call, or the first edit writes
/// a root over the scene's result; one undo step, merged while scrubbed.
pub fn set_root(value: &mut EditorModel, root: &objects::root::Parameters) {
    if *root == value.doc.root || value.carry.is_some() {
        return;
    }
    match scene_sync::write(Some(&value.factories), &value.doc, Level::Scene, &Edit::Root(root.clone())) {
        Err(message) => {
            value.edit_error = Some(message);
        }
        Ok(None) => {},
        Ok(Some((doc, _))) => {
            let doc = adopt(&value.history, Arc::new(doc));
            let history = commit(&doc, Some("Render settings"), Some(Merge::Gesture("render settings".to_string())), value.history.clone());
            value.doc = doc;
            value.history = history;
        }
    }
}

/// A host-driven gesture on a workspace document (`Editor.edit`, W11's entry): the same reduction
/// as a `Syntax_edit` intent, committed as one history entry named by the op (`Gesture` merge for a
/// scrub).
pub fn host_edit(value: &mut EditorModel, label: &str, gesture: Option<String>, edited: &dyn Fn(Option<&[sop::edit_graph::Factory]>, &Document) -> Result<Document, Diagnostic>) -> Result<(), String> {
    let clock = value.clock.clone();
    let guard = SampleGuard::start_with_clock(Box::new(move || clock()));
    let result = edited(Some(&value.factories), &value.doc)
        .map(|doc| {
            install(value, doc, label, gesture.map(Merge::Gesture).unwrap_or(Merge::Step));
        })
        .map_err(|d| rays_diagnostic::to_string(&d));
    let phases = guard.finish();
    if result.is_ok() { value.edit_phases = phases; }
    result
}

pub fn syntax_edit(value: &mut EditorModel, op: &fe::Op) -> Result<(), String> {
    host_edit(value, fe::label(op), fe::gesture(op), &|factories, doc| document_edit::syntax_edit_result(factories, doc, op))
}

pub fn layout_edit_op(value: &mut EditorModel, op: &layout_edit::Op) -> Result<(), String> {
    host_edit(value, layout_edit::label(op), layout_edit::gesture(op).map(|s| s.to_string()), &|factories, doc| document_edit::layout_op_result(factories, doc, op))
}

/// The scene's World at timeline `time` (the day cycle advances with it).
pub fn world(view: Option<&View>, value: &EditorModel, time: Option<f64>) -> Option<world::World> {
    let view = view.cloned().unwrap_or(View::Primary);
    let own = match &view {
        View::Only(key) => value.doc.view_worlds.iter().find(|(k, _)| k == key).map(|(_, w)| w.clone()),
        _ => None,
    };
    match own {
        Some(None) => None,
        Some(Some(w)) => layers::to_world(time, &w.node, &w.layers),
        None => {
            let scene_graph = scene(value);
            let id = objects::ids("world", &scene_graph).into_iter().find(|id| view_wants(value, &View::Primary, *id))?;
            let node = kg::find(&scene_graph, id)?;
            let layer_nodes = document::layers(&value.doc, id)?;
            layers::to_world(time, &node, layer_nodes)
        }
    }
}

/// Visible objects' world transforms with their latest cook.  Objects that only a viewport over
/// another scene instance draws (`Document.shell`'s views) are left out unless asked for: `All`, or
/// `Only key` for that viewport. Lights and cameras follow the same instance membership as the
/// geometry.
pub fn scene_for_view(override_: Option<kg::T>, view: Option<&View>, value: &EditorModel) -> kg::T {
    let view = view.cloned().unwrap_or(View::Primary);
    let scene_graph = override_.unwrap_or_else(|| scene(value).clone());
    let unwanted: Vec<i64> = scene_graph.iter().filter(|n| !view_wants(value, &view, n.id)).map(|n| n.id).collect();
    if unwanted.is_empty() { scene_graph } else { kg::remove(&unwanted, &scene_graph) }
}

pub fn placed_pieces(render: bool, view: Option<&View>, value: &EditorModel) -> Vec<(mat4::T, cook::piece)> {
    placed_pieces_of(render, view, value, pieces(value))
}

fn placed_pieces_of(render: bool, view: Option<&View>, value: &EditorModel, pieces: &[cook::piece]) -> Vec<(mat4::T, cook::piece)> {
    let view = view.cloned().unwrap_or(View::Primary);
    let scene_graph = scene(value);
    pieces
        .iter()
        .filter_map(|piece| match kg::find(&scene_graph, piece.id) {
            Some(node) if view_wants(value, &view, piece.id) && objects::visible(&node) && (!render || objects::flag("render", &node)) => Some((objects::world(&scene_graph, piece.id), piece.clone())),
            _ => None,
        })
        .collect()
}

pub fn world_id(value: &EditorModel) -> Option<i64> {
    objects::ids("world", &scene(value)).first().copied()
}

/// A parameter edit made in the view (a map drag, a World rotation): one undo entry per pointer
/// gesture.
pub fn edit_node(value: &mut EditorModel, level: Level, node_id: i64, values: &[(String, Value)], label: &str) {
    if value.carry.is_some() {
        return;
    }
    let written = |value: &mut EditorModel, doc: Document| {
        let doc = adopt(&value.history, Arc::new(doc));
        cook::set_volatile(&value.cook, flow_sop::lower::is_volatile(&doc.workspace.lowered));
        let history = commit(&doc, Some(label), Some(Merge::Gesture(parameter_gesture(label, level, node_id, values))), value.history.clone());
        value.doc = doc;
        value.history = history;
    };
    let failed = |value: &mut EditorModel, message: String| {
        value.edit_error = Some(message);
    };
    // an object the text declares takes the values in its text; one only the host made is edited
    // as a derived node, which the reconciliation adopts
    match scene_sync::set_fields(Some(&value.factories), &value.doc, level, node_id, values) {
        Ok(Some((doc, _, _))) => written(value, doc),
        Err(message) => failed(value, message),
        Ok(None) => match document::kinds(&value.doc, level) {
            None => {},
            Some(graph) => match kg::set_fields(&graph, node_id, values) {
                Err(_) => {},
                Ok((graph, _)) => {
                    let edited = document::with_kinds(&value.doc, level, graph);
                    match document_edit::reconcile(Some(&value.factories), None, &value.doc, &edited) {
                        Err(message) => failed(value, message),
                        Ok(doc) => written(value, doc),
                    }
                }
            },
        },
    }
}

/// The graph of a `shop_materialpath`: the material graph of that name, else the one whose `:name`
/// it is.
pub fn material_graph<'a>(value: &ModelView<'a>, path: &str) -> Option<String> {
    let ws = &value.doc.workspace.ws;
    let materials: Vec<String> = ws.checked.graphs.iter().filter(|g| g.context == context::material()).map(|g| g.name.clone()).collect();
    if materials.iter().any(|m| m == path) {
        return Some(path.to_string());
    }
    let ev = pane_laid(value)?.evaluated.as_ref()?;
    ev.results.iter().find_map(|(graph, v)| match v {
        EvalValue::Struct(g) if g.name == "material/standard" && materials.contains(graph) && g.fields.iter().find(|(k, _)| k == "name").map(|(_, v)| v) == Some(&EvalValue::Text(path.as_bytes().into())) => Some(graph.clone()),
        _ => None,
    })
}

fn view_of(view: Option<&str>, value: &EditorModel) -> View {
    match (view, &value.doc.shell) {
        (Some(key), Some(shell)) if shell.views.iter().any(|(k, _)| k == key) => View::Only(key.to_string()),
        _ => View::Primary,
    }
}

#[derive(Clone)]
enum Hit {
    Tag(i64),
    Material(Option<String>),
}

/// The nearest hit over the placed pieces of `view`, by the ray parameter, each piece cast in its
/// own space: the distance, what `cast` found and the piece's object.
fn nearest_by(placed: &[(mat4::T, cook::piece)], origin: vec3::T, direction: vec3::T, cast: &dyn Fn(&cook::piece, vec3::T, vec3::T) -> Option<(f64, Hit)>) -> Option<(f64, Hit, i64)> {
    let mut best: Option<(f64, Hit, i64)> = None;
    for (matrix, piece) in placed {
        let Some(inverse) = mat4::inverse(matrix) else { continue };
        let hit = cast(piece, mat4::transform_point(&inverse, origin), mat4::transform_direction(&inverse, direction));
        best = match (hit, best.take()) {
            (Some((distance, _)), Some((nearer, h, id))) if distance >= nearer => Some((nearer, h, id)),
            (Some((distance, hit)), _) => Some((distance, hit, piece.id)),
            (None, previous) => previous,
        };
    }
    best
}

/// A click in the view: the primitive under the ray, its `__flow_src` tag, the merge input that
/// made it (`Lower.provenance`), then the selection is that node and every enclosing zone probes
/// that iteration, so the highlight (`lit_tags`), the selectors and the inspector agree.  A click
/// on nothing deselects.  A merge of merges keeps the innermost tag, so the origin is exact
/// through nested collecting merges.  Geometry drawn as instances is picked per instance
/// (`Cook::pick`), and hits compare by the ray parameter, so any scale is exact.
pub fn pick(view: Option<&str>, alt: bool, value: &mut EditorModel, origin: vec3::T, direction: vec3::T) {
    // the focused viewport's own scene instance, when it has objects of its own
    let view = view_of(view, &*value);
    let placed = placed_pieces(false, Some(&view), &*value);
    if alt {
        // Alt-click: the material on that primitive
        return match nearest_by(&placed, origin, direction, &|p, o, d| cook::pick_material(p, o, d).map(|(t, m)| (t, Hit::Material(m)))) {
            Some((_, Hit::Material(Some(path)), _)) => match material_graph(&(&*value).into(), &path) {
                Some(graph) => {
                    go(value, &graph);
                },
                None => {
                    value.notice = Some((notice_kind::Refusal, format!("No material graph for {path}")));
                }
            },
            _ => {},
        };
    }
    let Some(laid) = pane_laid(&(&*value).into()) else { return };
    if scope_name(&(&*value).into()).is_none() {
        return;
    }
    let scope = laid.scope.clone();
    let workspace = value.doc.workspace.clone();
    let lowered = &workspace.lowered;
    let nearest = nearest_by(&placed, origin, direction, &|p, o, d| cook::pick(p, o, d).map(|(t, tag)| (t, Hit::Tag(tag))));
    let nearest = match nearest {
        Some((d, Hit::Tag(tag), owner)) => Some((d, tag, owner)),
        _ => None,
    };
    let hit: Option<flow_sop::lower::Origin> = nearest.and_then(|(_, tag, owner)| {
        let original = flow_sop::lower::origin(lowered, tag);
        match value.previews.get(&owner) {
            Some(preview) if original.is_none() || preview.chain.iter().any(|path| lowered.zones.iter().any(|zone| zone.site == *path)) => {
                let root = value.viewed.get(&owner)?.1;
                Some(flow_sop::lower::Origin {
                    merge: root,
                    input: 0,
                    source: root,
                    site: preview.path.clone(),
                    iter: preview.chain.iter().map(|path| value.probes.get(path).copied().unwrap_or(0)).collect(),
                })
            }
            _ => original,
        }
    });
    match hit {
        Some(o) => {
            // geometry of another graph (a viewport over another scene instance draws its own
            // objects): the pane shows that graph, the node is selected there once it does
            let root = o.site[0].clone();
            let laid_graph = |v: &EditorModel| pane_laid(&v.into()).map(|k| k.graph.clone()).unwrap_or_else(|| root.clone());
            let scope = if root == laid_graph(value) {
                scope
            } else {
                match flow_catalog(&*value).map(|catalog| fproj::of_graph(&catalog, &workspace.ws.checked, &root)) {
                    Some(Ok(found)) => {
                        let shown = root.clone();
                        pane_mut(value).shown = Some(shown.clone());
                        Rc::new(found)
                    }
                    _ => scope,
                }
            };
            match probe::chains(&scope).get(&o.site) {
                Some(chain) => {
                    // the outermost collapsed loop stands for what is inside it
                    let collapsed = |zone: &Vec<String>| workspace.ws.layout.collapsed.get(zone) == Some(&true);
                    let shown = chain.iter().find(|zone| collapsed(zone)).cloned().unwrap_or_else(|| o.site.clone());
                    let mut probes = value.probes.clone();
                    let indices: Vec<i64> = if chain.len() == o.iter.len() { o.iter.clone() } else { chain.iter().map(|_| 0).collect() };
                    for (zone, index) in chain.iter().zip(indices) {
                        probes.insert(zone.clone(), index);
                    }
                    let in_pane = root == laid_graph(&*value);
                    value.probes = probes;
                    value.select_later = if in_pane { Vec::new() } else { vec![shown.clone()] };
                    if in_pane { pxui_graph::scope_state::select(vec![shown.clone()], &mut pane_mut(value).view); };
                }
                None => {},
            }
        }
        None => {
            pxui_graph::scope_state::clear_selection(&mut pane_mut(value).view);
        },
    }
}

/// A changed source file: the whole text replaces the document as one history entry; layout,
/// settings, probes and the selection (all keyed by path) stay.  A refused text changes nothing but
/// the pane: it shows the file's text with the diagnostics, and the status says the last good
/// document is kept.
pub fn reload(imports: Option<&[(String, String)]>, value: &mut EditorModel, name: &str, text: &str) -> Result<(), Vec<Diagnostic>> {
    let doc = document_edit::text_edit(imports, Some(&value.factories), &value.doc, text)?;
    install(value, doc, &format!("Reload {name}"), Merge::Step);
    cook::reset_state(true, &value.cook);
    let clean = |text: &text_pane::State| text_pane::State {
        draft: None,
        doc_base: None,
        binding_base: None,
        doc_errors: text_pane::no_errors(),
        binding_draft: None,
        binding_errors: text_pane::no_errors(),
        graphs: Vec::new(),
        ..text.clone()
    };
    value.notice = Some((notice_kind::Info, format!("Reloaded {name}")));
    value.text = clean(&value.text);
    value.filed = document::text(&value.doc);
    value.locals = value.locals.iter().map(|(key, l)| (key.clone(), local { code: clean(&l.code), ..l.clone() })).collect();
    Ok(())
}

pub fn reload_failed(value: &mut EditorModel, name: &str, text: &str, diagnostics: &[Diagnostic]) {
    let first = match diagnostics.first() {
        None => String::new(),
        Some(d) => match text_pane::line_of(text, d) {
            Some(line) => format!(": line {line}, {}", d.message),
            None => format!(": {}", d.message),
        },
    };
    value.notice = Some((notice_kind::Refusal, format!("{name} not reloaded{first}")));
    value.text = text_pane::State {
        tab: text_pane::Tab::Document,
        draft: Some(text.to_string()),
        doc_base: Some(value.doc.workspace.ws.clone()),
        doc_errors: text_pane::Checked { at: text.to_string(), found: diagnostics.to_vec() },
        ..value.text.clone()
    };
}

/// The carry's hovered viewport has been looked at by the host: the surface under the pointer (the
/// object it belongs to and the material graph its primitive reads) is where a material goes, and
/// the scene where a SOP graph does.  `view` is the viewport's own scene instance.
pub fn carry_over_surface(view: Option<&str>, value: &mut EditorModel, origin: vec3::T, direction: vec3::T) {
    let Some(c) = value.carry.clone() else { return };
    let Some(report) = c.report.clone() else { return };
    if !matches!(report.over, Place::Viewport(_)) || !(c.payload.kind == "material" || c.payload.kind == "sop") {
        return;
    }
    let view = view_of(view, &*value);
    let placed = placed_pieces_of(false, Some(&view), value, &c.settled);
    let nearest = nearest_by(&placed, origin, direction, &|p, o, d| cook::pick_material(p, o, d).map(|(t, m)| (t, Hit::Material(m))));
    let place = match nearest {
        Some((_, Hit::Material(path), object_)) => Place::Surface { object_, material: path.and_then(|p| material_graph(&(&*value).into(), &p)) },
        _ => {
            // nothing under the pointer: a SOP graph goes to the scene, a material has no place
            let scene_graph = value.doc.workspace.ws.checked.graphs.iter().find(|g| g.context == context::scene()).map(|g| g.name.clone());
            match scene_graph {
                Some(graph) if c.payload.kind == "sop" => Place::Graph(graph),
                _ => Place::Viewport(String::new()),
            }
        }
    };
    value.carry = Some(carry { report: Some(carry_report { over: place, dropped: report.dropped }), ..c });
}

/// A pointer carry is over a viewport: the host should look through the viewport's camera.
pub fn carry_viewport(value: &EditorModel) -> Option<String> {
    match &value.carry {
        Some(carry { report: Some(carry_report { over: Place::Viewport(key), .. }), payload, .. }) if payload.kind == "material" || payload.kind == "sop" => Some(key.clone()),
        _ => None,
    }
}

pub fn carrying(value: &EditorModel) -> bool {
    value.carry.is_some()
}
