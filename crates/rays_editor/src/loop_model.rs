//! The loop's model `t`: the editor core with the 3D viewport, the cameras, what was drawn, the
//! source file and the autosave bookkeeping; its accessors and the composition of the scene objects.
//! Port of lib/rays_editor/loop_model.ml.
//!
//! Conventions of this port (see also editor_model.rs):
//! - `t` is `Clone`; the host (`Flow_native.Workspace_host.t`) is shared behind `Rc<RefCell<..>>`.
//! - OCaml's physical comparisons of rendered scenes, bakes, camera orbits and the model core are
//!   `Rc` identity where a value is shared (`baked`, `drawn`), `Scene3::same` for a scene, and the
//!   `rest` bookkeeping: every function that replaces `core` or `extra` outside a frame (`edit`,
//!   `set_probe`, `set_settings`, `set_renderer`, `with_core`) drops `rest`, which is what OCaml's
//!   `rest.rested == value.core` meant.
//! - The `drawn` textures are never known to be the same ones (`rays::texture` has no identity), so
//!   a piece with a texture is composed again each frame it has one.
#![allow(non_camel_case_types)]
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::Weak;

use editor_core::panels::Panel;
use editor_document::document::{self, Level, T as Document};
use editor_document::kind_graph as kg;
use editor_document::layout_edit;
use editor_document::objects;
use editor_document::workspace_doc;
use flow_graph::flow_edit as fe;
use flow_native::workspace_gpu;
use flow_native::workspace_host;
use flow_native::workspace_images;
use flow_syntax::phase_timer::PhaseTimer;
use flow_syntax::syntax::Syntax;
use input::{Frame as Input, Platform};
use param::Effects;
use rays::frame;
use rays::image;
use rays::light;
use rays::scene3::{self as d3, walk};
use rays::texture;
use rays::world;
use rays_math::{color, mat4};
use rdk_rays::surface::ConversionError;
use sop::edit_graph::Factory;
use sop::node;

use crate::cook;
use crate::editor_host;
use crate::editor_inspect::{self, View};
use crate::editor_list;
use crate::editor_model::{self as m, picked, update};
use crate::editor_setup;
use crate::editor_shell;
use crate::editor_status;
use crate::loop_view::{canvas_picture, hidden_entry, hidden_scene_cache, hidden_only, view_keys};
use crate::source_file;
use crate::timeline;
use crate::viewport as V;
use crate::world_map::world_drag;

pub type layout = pxui_shell::layout::Panels;

/// How the editor draws a prepared object (the sketch's `draw`).
pub type Draw_fn = Rc<dyn Fn(&node::T, &cook::prepared) -> Result<d3::Scene3, ConversionError>>;

/// The last full frame, once nothing but the clock could change it: its picture, and how many full
/// frames in a row drew the same one.
#[derive(Clone)]
pub struct rest {
    pub picture: Vec<rays::scene::node>,
    pub still: i64,
    pub shown: frame::Frame,
}

/// A World's bake with the World it came from; shared, compared by identity.
pub type baked_pair = (world::World, Rc<world::baked>);

/// One object's drawing: its graph and prepared value (the cache key), the scene, its texture and
/// the conversion error.
#[derive(Clone)]
pub struct drawn_entry {
    pub graph: node::T,
    pub prepared: cook::Prepared_ref,
    pub rendered: d3::Scene3,
    pub texture: Option<texture::Texture>,
    pub error: Option<String>,
}

pub type Drawn = BTreeMap<i64, drawn_entry>;

#[derive(Clone)]
pub struct LoopModel {
    /// Shared only by explicit Editor snapshots; the native loop mutates its unique core.
    pub core: Rc<m::EditorModel>,
    pub rest: Option<rest>,
    /// the orbit of the focused viewport
    pub camera: V::camera,
    /// the orbits of the other viewports, by key
    pub cameras: Vec<(String, V::camera)>,
    /// the viewport `camera` belongs to
    pub viewing: Option<String>,
    pub control: V::control,
    pub draw: Draw_fn,
    pub rendered: Option<V::rendered>,
    /// viewports over another scene instance
    pub views: Vec<(String, V::rendered)>,
    /// per object
    pub drawn: Rc<Drawn>,
    pub composed: Option<(kg::T, Level, Vec<(String, Vec<i64>)>)>,
    pub resolved: Option<(std::sync::Arc<Document>, Input, kg::T, Option<String>)>,
    pub context_error: Option<String>,
    /// E_MEMORY while the process stays over its limit
    pub memory_error: Option<String>,
    /// the machine's budget and the pressure source are set
    pub memory_known: bool,
    pub baked: Option<Rc<world::baked>>,
    pub baked_from: Option<baked_pair>,
    /// the World of each viewport over another scene instance
    pub baked_views: Vec<(String, baked_pair)>,
    /// the lat-long view's upload
    pub map: Option<(Rc<world::baked>, image::Image)>,
    pub render_status: Option<String>,
    pub pending_render: Option<V::request>,
    pub background: color::T,
    pub extra: V::extra,
    pub hidden_scene_cache: Option<hidden_scene_cache>,
    pub world_drag: Option<world_drag>,
    /// a left press in the view that may become a click
    pub pick_press: Option<(f64, f64)>,
    /// the .rays the document came from: polled, saved over
    pub source: Option<source_file::SourceFile>,
    /// the file's changed text, waiting while the document has unsaved work
    pub held: Option<String>,
    /// Command-O's system dialog, until it answers
    pub open_dialog: Option<i64>,
    /// the file's text that did not check: the Document tab shows it
    pub refused: Option<String>,
    /// the document this session started from
    pub opened: document::Text,
    /// this session wrote the recovery file
    pub state_owned: bool,
    pub state_checked: f64,
    /// Identity marker only: recovery must not retain a previous cook's arrays.
    pub saved_doc: Weak<Document>,
    pub saved_view: Syntax,
    pub state_error: Option<String>,
    pub canvases: Vec<(String, canvas_picture)>,
    pub host: Rc<std::cell::RefCell<workspace_host::WorkspaceHost>>,
    /// when this frame's update ended, and the meshes it draws for the first time
    pub presenting: Option<(f64, Vec<rdk_core::geometry::T>)>,
}

/// Scene objects a sketch starts with: its lights as light objects.
pub fn seed_lights(lights: &[light::Light], scene: kg::T) -> kg::T {
    let mut scene = scene;
    for (index, l) in lights.iter().enumerate() {
        let node = kg::instance(None, &format!("light{}", index + 1), &objects::light::kind());
        if let Ok((node, _)) = kg::apply(&node, &objects::light_values(l)) {
            scene = kg::add(&scene, node);
        }
    }
    scene
}

/// OCaml's optional arguments of `create`.
#[derive(Default)]
pub struct create_options {
    pub inputs: Option<Rc<workspace_doc::Inputs>>,
    pub name: Option<String>,
    pub presets: Option<String>,
    pub factories: Option<Vec<Factory>>,
    pub gpu_owner: Option<workspace_gpu::WorkspaceGpu>,
    pub settings: Option<editor_document::settings::T>,
    pub lights: Vec<light::Light>,
    pub camera: Option<V::camera>,
    pub background: Option<color::T>,
    pub seed: Option<i64>,
    pub grain: Option<i64>,
    pub domains: Option<i64>,
    pub max_entries: Option<i64>,
    pub max_payload_bytes: Option<i64>,
    pub await_: Option<bool>,
    pub carry_budget: Option<f64>,
    pub preferences: Option<String>,
    pub ui_text_size: Option<i64>,
    pub platform: Option<Platform>,
    pub ui_font: Option<String>,
    pub opened: Option<Document>,
}

fn real_path(file: &str) -> String {
    match std::fs::canonicalize(file) {
        Ok(p) => {
            let s = p.to_string_lossy().into_owned();
            s.strip_prefix("\\\\?\\").map(|r| r.to_string()).unwrap_or(s)
        }
        Err(_) => {
            if std::path::Path::new(file).is_relative() {
                std::env::current_dir().map(|d| d.join(file).to_string_lossy().into_owned()).unwrap_or_else(|_| file.to_string())
            } else {
                file.to_string()
            }
        }
    }
}

pub fn create(o: create_options, workspace: &workspace_doc::T, source: Option<source_file::SourceFile>, prepare: cook::Prepare_fn, draw: Draw_fn) -> Result<LoopModel, String> {
    let mut workspace = workspace.clone();
    if let Some(inputs) = o.inputs {
        workspace.inputs = std::sync::Arc::new((*inputs).clone());
    }
    let source = source.map(|s| source_file::with_imports(&s, &workspace_doc::import_texts(&workspace)));
    let name = o.name.unwrap_or_else(|| workspace_doc::name(&workspace));
    let state_key = match &source {
        None => format!("workspace:{}", workspace_doc::name(&workspace)),
        Some(source) => format!("file:{}", real_path(source_file::file(source))),
    };
    let camera = o.camera.unwrap_or_else(V::default_camera);
    let host = workspace_host::create(workspace_host::CreateOptions { seed: o.seed, grain: o.grain, domains: o.domains, gpu: o.gpu_owner });
    let gpu = workspace_gpu::config(&host.gpu, false);
    let seed_camera = camera.clone();
    let lights = o.lights;
    let defaults = editor_setup::create_opts::default();
    let opts = editor_setup::create_opts {
        gpu: Some(gpu),
        settings: o.settings,
        seed_scene: Some(Rc::new(move |scene| V::seed_scene(&seed_camera, seed_lights(&lights, scene)))),
        name,
        presets: o.presets,
        state_key: Some(state_key),
        factories: o.factories.unwrap_or_default(),
        seed: o.seed.unwrap_or(defaults.seed),
        grain: o.grain.unwrap_or(defaults.grain),
        domains: o.domains,
        max_entries: o.max_entries.unwrap_or(defaults.max_entries),
        max_payload_bytes: o.max_payload_bytes.unwrap_or(defaults.max_payload_bytes),
        await_: o.await_,
        carry_budget: o.carry_budget.unwrap_or(defaults.carry_budget),
        clock: None,
        preferences: o.preferences,
        ui_text_size: o.ui_text_size.unwrap_or(defaults.ui_text_size),
        platform: o.platform,
        ui_font: o.ui_font,
        opened: o.opened,
    };
    let mut host = host;
    match editor_setup::create(opts, &workspace, prepare) {
        Err(error) => {
            workspace_host::close(&mut host);
            Err(error)
        }
        Ok(core) => {
            let (mut core, extra) = V::init(core, &camera);
            if let Some(source) = &source {
                core.file = std::path::Path::new(source_file::file(source)).file_name().map(|f| f.to_string_lossy().into_owned()).unwrap_or_default();
            }
            let saved_view = V::section(&camera, &extra);
            let opened = document::text(&core.doc);
            let saved_doc = std::sync::Arc::downgrade(&core.doc);
            Ok(LoopModel {
                core: Rc::new(core),
                rest: None,
                camera,
                control: V::create_control(),
                draw,
                rendered: None,
                views: Vec::new(),
                drawn: Rc::new(BTreeMap::new()),
                presenting: None,
                composed: None,
                resolved: None,
                context_error: None,
                memory_error: None,
                memory_known: false,
                canvases: Vec::new(),
                host: Rc::new(std::cell::RefCell::new(host)),
                baked: None,
                baked_from: None,
                baked_views: Vec::new(),
                map: None,
                render_status: None,
                pending_render: None,
                background: color::hex_exn("#f4f5f0").expect("background"),
                extra,
                hidden_scene_cache: None,
                world_drag: None,
                pick_press: None,
                source,
                held: None,
                open_dialog: None,
                refused: None,
                opened,
                state_owned: false,
                cameras: Vec::new(),
                viewing: None,
                state_checked: f64::NEG_INFINITY,
                saved_doc,
                saved_view,
                state_error: None,
            })
        }
    }
}

// ---- accessors ----

pub fn document(value: &LoopModel) -> kg::T {
    editor_inspect::objects(&value.core)
}

pub fn network(value: &LoopModel) -> Option<sop::edit_graph::T> {
    editor_inspect::network(&value.core).map(|n| n.graph.geometry.clone())
}

pub fn prepared(value: &LoopModel) -> Option<rdk_core::geometry::T> {
    editor_list::prepared(&value.core).map(|p| p.geometry.clone())
}

pub fn camera(value: &LoopModel) -> V::camera {
    value.camera.clone()
}
pub fn extra(value: &LoopModel) -> V::extra {
    value.extra.clone()
}
pub fn timeline(value: &LoopModel) -> timeline::Timeline {
    editor_list::timeline(&value.core)
}
pub fn selected_node(value: &LoopModel) -> Option<picked> {
    editor_list::selected_node(&value.core)
}
pub fn displayed_node(value: &LoopModel) -> node::T {
    editor_list::displayed_node(&value.core)
}
pub fn panes(value: &LoopModel, frame_: &frame::Frame) -> pxui_shell::layout::Panes {
    editor_shell::panes(&value.core, frame_)
}
pub fn can_undo(value: &LoopModel) -> bool {
    crate::editor_history::can_undo(&value.core.history)
}
pub fn can_redo(value: &LoopModel) -> bool {
    crate::editor_history::can_redo(&value.core.history)
}
pub fn undo_label(value: &LoopModel) -> Option<String> {
    if can_undo(value) { Some(crate::editor_history::label(&value.core.history).to_string()) } else { None }
}
pub fn redo_label(value: &LoopModel) -> Option<String> {
    crate::editor_history::redo_label(&value.core.history).map(|s| s.to_string())
}
pub fn workspace(value: &LoopModel) -> workspace_doc::T {
    value.core.doc.workspace.ws.clone()
}
pub fn probe(value: &LoopModel, zone: &[String]) -> Option<i64> {
    value.core.probes.get(zone).copied()
}

/// The model with a replaced core: what a test or tool does outside a frame. Drops `rest`.
pub fn with_core(value: LoopModel, core: m::EditorModel) -> LoopModel {
    LoopModel { core: Rc::new(core), rest: None, ..value }
}

pub fn set_probe(value: LoopModel, zone: &[String], index: i64) -> LoopModel {
    let mut core = (*value.core).clone();
    core.probes.insert(zone.to_vec(), index);
    with_core(value, core)
}

pub fn node_box(value: &LoopModel, path: &[String]) -> Option<(i64, i64, i64, i64)> {
    pxui_graph::scope_frame::inspect::box_of(m::pane_view(&value.core), path).map(|(x, y, w, h)| (x as i64, y as i64, w as i64, h as i64))
}

pub fn edit(mut value: LoopModel, op: &fe::Op) -> Result<LoopModel, String> {
    editor_host::syntax_edit(Rc::make_mut(&mut value.core), op)?;
    value.rest = None;
    Ok(value)
}

pub fn edit_layout(mut value: LoopModel, op: &layout_edit::Op) -> Result<LoopModel, String> {
    editor_host::layout_edit_op(Rc::make_mut(&mut value.core), op)?;
    value.rest = None;
    Ok(value)
}

pub fn edit_phases(value: &LoopModel) -> PhaseTimer {
    value.core.edit_phases.clone()
}

pub fn carrying(value: &LoopModel) -> Option<(String, String)> {
    value.core.carry.as_ref().map(|c| (c.payload.kind.clone(), c.payload.value.clone()))
}

pub fn carry_line(value: &LoopModel) -> Option<String> {
    value.core.carry.as_ref().map(editor_status::carry_line)
}

pub fn level(value: &LoopModel) -> Option<String> {
    match m::pane_level(&value.core) {
        Level::Scene => None,
        Level::Inside(id) => Some(kg::find(&editor_inspect::scene(&value.core), id).map(|n| n.label).unwrap_or_default()),
    }
}

pub fn scene_document(value: &LoopModel) -> kg::T {
    editor_inspect::scene(&value.core)
}

pub fn same_context(a: &Document, b: &Document) -> bool {
    std::ptr::eq(a, b) || (kg::same(&a.scene, &b.scene) && a.scene_drives == b.scene_drives && a.scene_textures == b.scene_textures)
}

pub fn resolved_scene(value: &LoopModel) -> kg::T {
    match &value.resolved {
        Some((doc, _, scene, _)) if same_context(doc, &value.core.doc) => scene.clone(),
        _ => editor_inspect::scene(&value.core),
    }
}

pub fn lights(value: &LoopModel) -> Vec<light::Light> {
    objects::lights(true, &resolved_scene(value))
}

pub fn world(value: &LoopModel) -> Option<world::baked> {
    value.baked.as_ref().map(|b| (**b).clone())
}

pub fn objects_placed(value: &LoopModel) -> Vec<(mat4::T, cook::Prepared_ref)> {
    editor_host::placed_pieces(true, None, &value.core).into_iter().map(|(matrix, piece)| (matrix, piece.prepared)).collect()
}

pub fn settings(value: &LoopModel) -> editor_document::settings::T {
    editor_list::settings(&value.core)
}

pub fn set_settings(mut value: LoopModel, settings: &editor_document::settings::T) -> LoopModel {
    editor_host::set_settings(Rc::make_mut(&mut value.core), settings);
    value.rest = None;
    value
}

/// the viewport the focus is in; the default layout's one before any focus
pub fn focus_key_of(viewing: &Option<String>) -> String {
    viewing.clone().unwrap_or_else(|| "main".to_string())
}

pub fn focus_key(value: &LoopModel) -> String {
    focus_key_of(&value.viewing)
}

/// the viewport whose root a view is read from: the hidden scene shows the focused viewport
pub fn look_key_of(viewing: &Option<String>, key: &str) -> String {
    if key == "@hidden" { focus_key_of(viewing) } else { key.to_string() }
}

pub fn look_key(value: &LoopModel, key: &str) -> String {
    look_key_of(&value.viewing, key)
}

/// the view of the viewport `key`: its own orbit, the focused one's being `camera`, through the
/// camera of its own root
pub fn camera_for(viewing: &Option<String>, camera: &V::camera, cameras: &[(String, V::camera)], extra: &V::extra, pending: bool, key: &str) -> rays::camera::Camera {
    let key = look_key_of(viewing, key);
    let orbit = if viewing.as_deref() == Some(key.as_str()) {
        camera.clone()
    } else {
        cameras.iter().find(|(k, _)| *k == key).map(|(_, c)| c.clone()).unwrap_or_else(|| camera.clone())
    };
    V::view_camera(&orbit, extra, &key, pending)
}

pub fn camera_of(value: &LoopModel, key: &str) -> rays::camera::Camera {
    camera_for(&value.viewing, &value.camera, &value.cameras, &value.extra, value.pending_render.is_some(), key)
}

pub fn view_camera(value: &LoopModel) -> rays::camera::Camera {
    camera_of(value, &focus_key(value))
}

/// Which viewport the orbit belongs to, and the orbits of the others.
pub struct Focus {
    pub viewing: Option<String>,
    pub camera: V::camera,
    pub cameras: Vec<(String, V::camera)>,
}

/// Focus moves to another viewport: the orbit of the one left is kept, the new one starts from the
/// orbit it last had, else from a copy of the one in use. `core` is the model whose panels say
/// which viewport has the focus.
pub fn follow_focus(value: &LoopModel, core: &m::EditorModel, frame_: &frame::Frame) -> Focus {
    let g = editor_shell::geometry(core, &core.workspace, frame_);
    let keys: Vec<String> = g
        .leaves
        .iter()
        .filter_map(|leaf| match &leaf.panel {
            Panel::View(key) => Some(key.clone()),
            _ => None,
        })
        .collect();
    let mut cameras = value.cameras.clone();
    cameras.retain(|(key, _)| keys.contains(key));
    let unchanged = Focus { viewing: value.viewing.clone(), camera: value.camera.clone(), cameras };
    match editor_shell::active_view(core, &g) {
        Some(leaf) => match leaf.panel {
            Panel::View(key) if Some(&key) != value.viewing.as_ref() => match &value.viewing {
                None => {
                    // the first focus: every other viewport starts as a copy of the orbit in use
                    let others: Vec<(String, V::camera)> = g
                        .leaves
                        .iter()
                        .filter_map(|leaf| match &leaf.panel {
                            Panel::View(k) if *k != key => Some((k.clone(), value.camera.clone())),
                            _ => None,
                        })
                        .collect();
                    Focus { viewing: Some(key), cameras: others, ..unchanged }
                }
                Some(previous) => {
                    let mut kept: Vec<(String, V::camera)> = if keys.contains(previous) {
                        let mut k = vec![(previous.clone(), value.camera.clone())];
                        k.extend(unchanged.cameras.iter().filter(|(p, _)| p != previous).cloned());
                        k
                    } else {
                        unchanged.cameras.clone()
                    };
                    let camera = kept.iter().find(|(k, _)| *k == key).map(|(_, c)| c.clone()).unwrap_or_else(|| value.camera.clone());
                    kept.retain(|(k, _)| *k != key);
                    Focus { viewing: Some(key), camera, cameras: kept }
                }
            },
            _ => unchanged,
        },
        None => unchanged,
    }
}

pub fn paint_view(value: &LoopModel, key: &str, viewport: pxui_shell::layout::Bounds, view_: &rays::camera::Camera, rendered: &d3::Scene3) -> Vec<rays::scene::node> {
    V::paint(&value.extra, key, V::film(&value.extra, &look_key(value, key), viewport), view_, rendered)
}

pub fn viewport_camera(value: &LoopModel, key: &str) -> rays::camera::Camera {
    camera_of(value, key)
}

pub fn focused_image_of(viewing: &Option<String>, views: &[(String, d3::Scene3)], rendered: &Option<d3::Scene3>) -> Option<d3::Scene3> {
    match viewing {
        Some(key) => views.iter().find(|(k, _)| k == key).map(|(_, i)| i.clone()).or_else(|| rendered.clone()),
        None => rendered.clone(),
    }
}

pub fn focused_image(value: &LoopModel) -> Option<d3::Scene3> {
    focused_image_of(&value.viewing, &value.views, &value.rendered)
}

pub fn film(value: &LoopModel, frame_: &frame::Frame) -> (i64, i64, i64, i64) {
    let pane = editor_shell::panes(&value.core, frame_).view;
    let (x, y, _, _) = pane;
    let (fx, fy, w, h) = V::film(&value.extra, &focus_key(value), pane);
    (fx - x, fy - y, w, h)
}

/// The hidden-scene cache is model state: refreshed here, read by `scene`.
pub fn refresh_hidden(value: &mut LoopModel, frame_: &frame::Frame) {
    let cache = if hidden_only(V::ui_visible(&value.control), &value.core) {
        let pv = |key: &str, vp, view_: &rays::camera::Camera, r: &d3::Scene3| paint_view(&value, key, vp, view_, r);
        Some(hidden_entry(value.background, focused_image(&value).as_ref(), &view_camera(&value), &pv, value.hidden_scene_cache.as_ref(), &value.core, frame_))
    } else {
        None
    };
    value.hidden_scene_cache = cache;
}

/// the device display keys the drawn scenes draw: a display left out is released
pub fn display_keys(drawn: &Drawn) -> Vec<String> {
    let mut keys = Vec::new();
    for entry in drawn.values() {
        walk::iter_batches(
            &mut |drawing, _| {
                if let Some((key, _)) = rays::mesh::view::device(&drawing.mesh) {
                    keys.push(key);
                }
            },
            &entry.rendered,
        );
    }
    keys
}

/// Recompose when a cook, the scene, or the World changed. Each object's drawing is reused while
/// its graph and prepared value are physically the same, so moving an object only re-places it.
pub fn composition_key(core: &m::EditorModel, scene: &kg::T) -> (kg::T, Level, Vec<(String, Vec<i64>)>) {
    (scene.clone(), m::pane_level(core), core.doc.shell.as_ref().map(|s| s.views.clone()).unwrap_or_default())
}

/// What `compose` hands back, and whether the drawings (`drawn`) are new ones.
pub struct composed_out {
    pub rendered: Option<V::rendered>,
    pub views: Vec<(String, V::rendered)>,
    pub drawn: Rc<Drawn>,
    pub errors: Vec<String>,
    pub drawn_changed: bool,
}

fn same_baked(a: &Option<Rc<world::baked>>, b: &Option<Rc<world::baked>>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        (None, None) => true,
        _ => false,
    }
}

fn same_baked_views(a: &[(String, baked_pair)], b: &[(String, baked_pair)]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|((k, (_, x)), (k2, (_, y)))| k == k2 && Rc::ptr_eq(x, y))
}

pub fn compose(value: &LoopModel, core: &m::EditorModel, changed: bool, baked: &Option<Rc<world::baked>>, baked_views: &[(String, baked_pair)], scene: &kg::T) -> composed_out {
    let draw_errors = |drawn: &Drawn, errors: &[String]| -> Vec<String> {
        let mut out: Vec<String> = errors.to_vec();
        for e in drawn.values() {
            if let Some(message) = &e.error {
                out.insert(0, message.clone());
            }
        }
        out
    };

    let timeline_ = editor_list::timeline(core);
    let live = Input { t: timeline::time(&timeline_), frame: timeline::frame(&timeline_), ..core.live_frame.clone() };
    let lowered = &core.doc.workspace.lowered;
    let (images, host_gpu) = {
        let host = value.host.borrow();
        (host.images.clone(), host.gpu.clone())
    };
    workspace_images::bind(&images, lowered);
    let plan = &lowered.plan;
    let mut textures: BTreeMap<i64, texture::Texture> = BTreeMap::new();
    let mut errors: Vec<String> = Vec::new();
    for (id, source) in core.doc.scene_textures.iter() {
        match workspace_images::texture(None, &images, &core.cook.state, &live, plan, source) {
            Ok(texture) => {
                textures.insert(*id, texture);
            }
            Err(diagnostic) => errors.insert(0, rays_diagnostic::to_string(&diagnostic)),
        }
    }
    // a texture has no identity here: a drawing with one, or one that had one, is drawn again
    let texture_changed = value.drawn.iter().any(|(id, e)| !(e.texture.is_none() && !textures.contains_key(id)));
    let (scene_, level_, views_) = composition_key(core, scene);
    let same = match &value.composed {
        Some((s, l, v)) => kg::same(s, &scene_) && *l == level_ && *v == views_,
        None => false,
    };
    if !(texture_changed
        || changed
        || !same
        || !same_baked(baked, &value.baked)
        || !same_baked_views(baked_views, &value.baked_views)
        || value.rendered.is_none())
    {
        return composed_out { rendered: value.rendered.clone(), views: value.views.clone(), drawn: value.drawn.clone(), errors: draw_errors(&value.drawn, &errors), drawn_changed: false };
    }
    let placed = editor_host::placed_pieces(false, Some(&View::All), core);
    let mut drawn: Drawn = BTreeMap::new();
    for (_, piece) in &placed {
        let texture = textures.get(&piece.id).cloned();
        match value.drawn.get(&piece.id) {
            Some(prev) if node::ptr_eq(&prev.graph, &piece.graph) && Rc::ptr_eq(&prev.prepared, &piece.prepared) && prev.texture.is_none() && texture.is_none() => {
                drawn.insert(piece.id, prev.clone());
            }
            _ => {
                let display = {
                    let gpu = host_gpu.clone();
                    let d: cook::Display = Rc::new(move |g| workspace_gpu::display(&gpu, g).map(|(k, n, i, w, keep)| (k, n, i, w, Rc::from(keep))));
                    d
                };
                let mut produced: Option<Result<d3::Scene3, ConversionError>> = None;
                cook::with_display(display, &mut || produced = Some((value.draw)(&piece.graph, &piece.prepared)));
                let (scene3, error) = match produced.expect("draw ran") {
                    Ok(s) => (s, None),
                    Err(ConversionError(message)) => (d3::empty(), Some(message)),
                };
                let scene3 = match &texture {
                    Some(tex) => walk::with_texture(&d3::textured(None, tex.clone()), &scene3),
                    None => scene3,
                };
                drawn.insert(piece.id, drawn_entry { graph: piece.graph.clone(), prepared: piece.prepared.clone(), rendered: scene3, texture, error });
            }
        }
    }
    let waiting = placed.is_empty() && !editor_list::geometry_objects(core).is_empty() && editor_list::pieces(core).is_empty();
    let ghost = |id: i64| -> bool {
        match m::pane_level(core) {
            Level::Inside(open_id) => editor_inspect::kind(core, open_id).as_deref() == Some("geometry") && id != open_id,
            Level::Scene => false,
        }
    };
    let compose_pieces = |world_: &Option<Rc<world::baked>>, view: &View, pieces: Vec<(mat4::T, cook::piece)>| -> d3::Scene3 {
        V::compose(
            &editor_host::scene_for_view(Some(scene.clone()), Some(view), core),
            world_.as_deref(),
            pieces
                .into_iter()
                .map(|(matrix, piece)| {
                    let rendered = &drawn.get(&piece.id).expect("drawn piece").rendered;
                    let culled = match kg::find(&scene, piece.id) {
                        Some(node) => walk::with_cull(objects::cull(&node), rendered),
                        None => rendered.clone(),
                    };
                    (matrix, ghost(piece.id), culled)
                })
                .collect(),
        )
    };
    // a viewport over another scene instance: its own objects, its own World. Viewports over the
    // very same instance with the same World show one picture: it is composed once, so the renderer
    // sees one scene and can give them one tracer
    let instance_of = |key: &str| -> Option<flow_syntax::value::Value> {
        core.doc.shell.as_ref().and_then(|shell| shell.preview_sources.iter().find(|(k, _)| k == key).map(|(_, s)| s.instance.clone()))
    };
    let views: Vec<(String, d3::Scene3)> = if waiting {
        Vec::new()
    } else {
        let mut composed: Vec<(String, d3::Scene3, Option<flow_syntax::value::Value>, Option<Rc<world::baked>>)> = Vec::new();
        for key in view_keys(core) {
            let world_ = baked_views.iter().find(|(k, _)| *k == key).map(|(_, (_, b))| b.clone());
            let instance = instance_of(&key);
            let twin = composed.iter().find_map(|(_, scene_, other, other_world)| {
                let same_instance = match (&instance, other) {
                    (Some(a), Some(b)) => a == b,
                    _ => false,
                };
                let same_world = match (&world_, other_world) {
                    (None, None) => true,
                    (Some(a), Some(b)) => Rc::ptr_eq(a, b),
                    _ => false,
                };
                if same_instance && same_world { Some(scene_.clone()) } else { None }
            });
            let scene_ = match twin {
                Some(s) => s,
                None => {
                    let own = core.doc.shell.as_ref().map(|shell| shell.views.iter().any(|(k, _)| *k == key)).unwrap_or(false);
                    let view = if own { View::Only(key.clone()) } else { View::Primary };
                    compose_pieces(&world_, &view, editor_host::placed_pieces(false, Some(&view), core))
                }
            };
            composed.insert(0, (key, scene_, instance, world_));
        }
        composed.into_iter().rev().map(|(k, s, _, _)| (k, s)).collect()
    };
    let rendered = if waiting { None } else { Some(compose_pieces(baked, &View::Primary, editor_host::placed_pieces(false, None, core))) };
    let errs = draw_errors(&drawn, &errors);
    composed_out { rendered, views, drawn: Rc::new(drawn), errors: errs, drawn_changed: true }
}

/// The frame that first drew a mesh has been presented: its time is the display sink's first draw.
pub fn observe_presented(value: &mut LoopModel) {
    if let Some((began, geometries)) = &value.presenting {
        let seconds = (value.core.clock)() - began;
        let gpu = workspace_gpu::config(&value.host.borrow().gpu, editor_document::lowering::fast(&value.core.doc));
        for geometry in geometries {
            flow_sop::lower::observe_draw(&gpu, geometry, seconds);
        }
    }
    value.presenting = None;
}

#[allow(dead_code)]
fn _unused(_: &Effects, _: &layout) {}
