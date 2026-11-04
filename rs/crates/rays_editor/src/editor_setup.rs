//! Creation of the model: the document a workspace starts from, the preferences and the state
//! folder. Port of lib/rays_editor/editor_setup.ml.
#![allow(non_camel_case_types)]
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels;
use editor_core::store;
use editor_document::document::{self, Level, T as Document};
use editor_document::kind_graph as kg;
use editor_document::workspace_doc;
use editor_document::{kinds, layout_by_path, lowering, objects, preset, settings};
use flow_native::workspace_gpu;
use flow_sop::{lower, manifest};
use flow_syntax::context;
use flow_syntax::phase_timer;
use input::Platform;
use pxui::ui::Ui;
use pxui_graph::scope_state as scope;
use pxui_shell::{layout, tree};
use sop::edit_graph::Factory;

use crate::cook;
use crate::editor_history as history;
use crate::editor_model::*;
use crate::editor_panes::{follow_hosts, follow_start, panel_hosts};
use crate::editor_shell::{follow_graph, graph_path, start_views};
use crate::navigator;
use crate::panel_map;
use crate::selection;
use crate::text_pane;
use crate::timeline;

// ---- creation ----

/// The document a workspace starts from: one geometry object per `sop` graph beside the sketch's
/// own scene objects (`seed_scene`). `opened` is the caller's `Lowering::of_workspace ~factories
/// workspace`, when it already built it to read the window and the camera (Workspace.run): the
/// workspace is evaluated once a start.
pub fn workspace_doc(
    gpu: Option<workspace_gpu::policy>,
    opened: Option<Document>,
    factories: &[Factory],
    seed_scene: &dyn Fn(kg::T) -> kg::T,
    workspace: &workspace_doc::T,
) -> Result<Document, String> {
    let mut doc = match opened {
        Some(doc) => doc,
        None => lowering::of_workspace(&lowering::Options { factories: Some(factories.to_vec()), gpu, ..Default::default() }, workspace).map_err(|d| rays_diagnostic::to_string(&d))?,
    };
    let declared = doc.scene.clone();
    // the host's camera and lights are defaults of a workspace with no scene graph; a scene graph
    // is authoritative (it may say there are none)
    let has_scene = lowering::graph_of(workspace, context::scene()).is_some();
    let scene = if has_scene { declared } else { seed_scene(declared) };
    if !has_scene {
        doc.active_camera = objects::ids("camera", &scene).first().copied();
    }
    doc.scene = scene;
    Ok(doc)
}

fn concat(dir: &str, file: &str) -> String {
    if dir.ends_with('/') || dir.ends_with('\\') { format!("{dir}{file}") } else { format!("{dir}/{file}") }
}

pub fn user_folder(platform: Platform) -> String {
    let pref = match platform {
        Platform::Apple => None,
        Platform::Other => sdl3::pref_path("rays", "editor").ok().flatten(),
    };
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    store::user_folder(platform, pref.as_deref(), &home)
}

pub fn preferences_file(platform: Platform, given: Option<&str>) -> String {
    match given {
        Some(path) if !path.is_empty() => path.to_string(),
        _ => concat(&user_folder(platform), "preferences.rays"),
    }
}

pub fn read_preferences(filename: &str) -> Result<store::settings::t, String> {
    store::settings::load("rays-editor", filename)
}

pub fn save_guide(filename: &str, guide: bool) -> Result<(), String> {
    let mut values = if std::path::Path::new(filename).exists() { read_preferences(filename)? } else { Vec::new() };
    values.retain(|(k, _)| k != "guide");
    values.insert(0, ("guide".to_string(), store::settings::Value::Bool(guide)));
    store::settings::save("rays-editor", filename, &values)
}

pub fn state_directory(value: &t) -> String {
    concat(&value.presets, "state")
}

pub fn state_file(value: &t) -> String {
    preset::path(&state_directory(value), &value.state_name)
}

pub fn browse(value: &t, query: &str) -> Option<prompt> {
    let last_state = std::fs::metadata(state_file(value)).ok().filter(|m| m.is_file()).and_then(|m| m.modified().ok()).and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok()).map(|d| d.as_secs_f64());
    Some(prompt::Browsing { query: query.to_string(), presets: preset::list(&value.presets), last_state })
}

/// OCaml's optional arguments of `create`.
pub struct create_opts {
    pub gpu: Option<workspace_gpu::policy>,
    pub settings: Option<settings::T>,
    pub seed_scene: Option<Rc<dyn Fn(kg::T) -> kg::T>>,
    pub name: String,
    pub presets: Option<String>,
    pub state_key: Option<String>,
    pub factories: Vec<Factory>,
    pub seed: i64,
    pub grain: i64,
    pub domains: Option<i64>,
    pub max_entries: i64,
    pub max_payload_bytes: i64,
    pub await_: Option<bool>,
    pub carry_budget: f64,
    pub clock: Option<cook::Clock>,
    pub preferences: Option<String>,
    pub ui_text_size: i64,
    pub platform: Option<Platform>,
    pub ui_font: Option<String>,
    pub opened: Option<Document>,
}

impl Default for create_opts {
    fn default() -> create_opts {
        create_opts {
            gpu: None,
            settings: None,
            seed_scene: None,
            name: "sketch".to_string(),
            presets: None,
            state_key: None,
            factories: Vec::new(),
            seed: 0,
            grain: 16_384,
            domains: None,
            max_entries: 512,
            max_payload_bytes: 256 * 1024 * 1024,
            await_: None,
            carry_budget: 0.5,
            clock: None,
            preferences: None,
            ui_text_size: default_text_size,
            platform: None,
            ui_font: None,
            opened: None,
        }
    }
}

fn wall_clock() -> f64 {
    std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_secs_f64()).unwrap_or(0.0)
}

pub fn create(opts: create_opts, workspace: &workspace_doc::T, prepare: cook::Prepare_fn) -> Result<t, String> {
    let create_opts {
        gpu, settings: given_settings, seed_scene, name, presets, state_key, factories, seed, grain, domains, max_entries, max_payload_bytes, await_, carry_budget, clock, preferences, ui_text_size, platform, ui_font, opened,
    } = opts;
    let factories = if factories.is_empty() { sop::nodes::factories() } else { factories };
    let platform = platform.unwrap_or_else(runtime_input::platform);
    let state_key = state_key.unwrap_or_else(|| name.clone());
    let clock: cook::Clock = clock.unwrap_or_else(|| Arc::new(wall_clock));
    let mut workspace = workspace.clone();
    workspace.settings = given_settings.unwrap_or_else(|| workspace.settings.clone());
    let gpu = gpu.unwrap_or_else(workspace_gpu::default_policy);
    let seed_scene = seed_scene.unwrap_or_else(|| Rc::new(|scene| scene));
    let doc = workspace_doc(Some(gpu.clone()), opened, &factories, &*seed_scene, &workspace)?;
    let cook = cook::create(prepare, seed, grain, domains, await_, Some(clock.clone()), max_entries, max_payload_bytes)?;
    let shell = shell { tree: layout_default(), hidden: vec![panels::Panel::Timeline], live: None, window_live: None, restored: false };
    let presets = match presets {
        Some(directory) => directory,
        None => concat(&user_folder(platform), &preset::sanitize(&name)),
    };
    let level = Level::Scene;
    let preferences = preferences_file(platform, preferences.as_deref());
    // a machine's setting, beside the guide: an override from the process still wins
    if let Ok(values) = read_preferences(&preferences) {
        if let Some(n) = store::settings::int(&values, "memory_budget") {
            mem::set_budget_from(mem::Source::Preference, n);
        }
    }
    let guide = match read_preferences(&preferences) {
        Ok(values) => store::settings::bool(&values, "guide").unwrap_or(true),
        Err(_) => true,
    };
    let state_name = lowering::sha256(&state_key);
    let doc = Arc::new(doc);
    let ui = {
        let mut ui = Ui::create(platform, None, ui_font);
        ui.set_font_size(ui_text_size);
        ui
    };
    let keep: Arc<dyn Fn(&Document) -> document::Snapshot> = Arc::new(document::snapshot);
    let bytes: Arc<dyn Fn(&document::Snapshot) -> i64> = Arc::new(|s| document::snapshot_bytes(s) as i64);
    let history = history::create(None, Some(64 * 1024 * 1024), Some(bytes), keep, doc.clone())?;
    let catalog_factories = factories.clone();
    let vocab = vocabulary(&factories, &doc);
    let file = format!("{}.rays", doc.workspace.ws.checked.name);
    let value = t {
        preferences,
        text_size: ui_text_size,
        guide,
        hud: None,
        presets,
        state_name,
        name,
        prompt: None,
        notice: None,
        notice_at: 0.0,
        filed: document::text(&doc),
        gpu,
        panes: panel_map::make(graph_pane { view: scope::create(None, None), laid: None, shown: None, route: Vec::new(), views: level_map_empty(), at: level, picked: selection::empty() }),
        text: text_pane::initial(),
        map_view: false,
        rows: Vec::new(),
        live_cook: true,
        factories,
        menu: None,
        probes: layout_by_path::PathMap::new(),
        lit: None,
        previews: Default::default(),
        viewed: Default::default(),
        canvas_preview: None,
        canvas_viewed: None,
        view_key: None,
        edit_phases: phase_timer::empty(),
        select_later: Vec::new(),
        open_import: None,
        flow_catalog: Lazy_catalog::new(Rc::new(move || kinds::catalog(manifest::VERSION, &catalog_factories).ok())),
        lisp_vocab: vocab,
        tree: tree::create(),
        outline: navigator::initial(),
        held_keys: Vec::new(),
        sketch_blocked: Vec::new(),
        window_focus: 2,
        ui: Rc::new(RefCell::new(ui)),
        workspace: shell,
        timeline: timeline::create(),
        live_frame: input::at_time(0.0),
        cook,
        edit_error: None,
        status_fps: None,
        status_fps_at: f64::NEG_INFINITY,
        last_dt: 0.0,
        steady: 0,
        history,
        focus: panels::main(),
        focus_path: None,
        pane_keys: Vec::new(),
        leader: editor_core::router::State::Idle,
        timeline_frames: 240,
        queued: Vec::new(),
        list_at: None,
        text_at: None,
        outline_at: None,
        locals: Vec::new(),
        started: started::default(),
        framing: None,
        carry: None,
        carry_budget,
        clock,
        traces: Vec::new(),
        view_tools: None,
        gates: Vec::new(),
        selected_box: None,
        file,
        doc,
    };
    cook::set_volatile(&value.cook, lower::is_volatile(&value.doc.workspace.lowered));
    // the panels open as their start keywords say; the first graph pane (the focused leaf, else the
    // first) opens on the graph it pins
    let on: Option<layout::Path> = value.doc.shell.as_ref().and_then(|s| s.start.focus.clone());
    let focus = if on.is_none() { value.focus.clone() } else { panels::Panel::Graph };
    let value = follow_graph(value, &focus, on.as_deref());
    let views = graph_path(&value).map(|p| start_views(&value.doc, level, &p)).unwrap_or_else(level_map_empty);
    let value = with_pane(value, &|pane| graph_pane { views: views.clone(), ..pane.clone() });
    let hosts = panel_hosts(&value);
    let value = follow_hosts(value, on.as_ref(), &hosts);
    Ok(follow_start(value))
}

fn layout_default() -> panels::t {
    layout::default()
}
