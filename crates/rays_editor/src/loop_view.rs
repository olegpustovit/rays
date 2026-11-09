//! What the loop draws of the views: the cursor, the World bake of a frame, the hidden scene and
//! the composition of the viewports, canvases and floating windows under the UI. Port of
//! lib/rays_editor/loop_view.ml.
//!
//! Gap against OCaml: the hidden scene is reused by value (width, height, the rendered scene by
//! `Scene3::same`, camera, background, visibility), where OCaml compared physically.
#![allow(non_camel_case_types)]
use std::rc::Rc;

use editor_core::panels::Panel;
use flow_native::drawing;
use flow_syntax::ty::Ty;
use pxui::ui::Ui;
use pxui_shell::layout::Bounds;
use rays::camera as rcam;
use rays::frame;
use rays::image;
use rays::scene::{self, node as scene_node};
use rays::scene3::{self as d3};
use rays::sketch;
use rays::world;
use rays_math::color;

use crate::editor_host;
use crate::editor_inspect::View;
use crate::editor_list;
use crate::editor_model::EditorModel as Model;
use crate::editor_shell;
use crate::timeline;
use crate::world_map::map_rect;

type Nodes = Vec<scene_node>;

/// The shape the editor asks of the platform for the pointer, or the default one while the UI is hidden.
pub fn set_ui_cursor(ui: &Ui, visible: bool) -> Result<(), String> {
    let shape = match if visible { ui.cursor() } else { None } {
        Some(pxui::ui::Cursor::Horizontal_resize) => sketch::cursor::Horizontal_resize,
        Some(pxui::ui::Cursor::Vertical_resize) => sketch::cursor::Vertical_resize,
        Some(pxui::ui::Cursor::Text) => sketch::cursor::Text,
        None => sketch::cursor::Default,
    };
    sketch::set_cursor(shape)
}

#[derive(Clone)]
pub struct hidden_scene_cache {
    pub width: i64,
    pub height: i64,
    pub rendered: Option<d3::Scene3>,
    pub camera: rcam::Camera,
    pub background: color::T,
    pub view_visible: bool,
    pub scene: Nodes,
}

#[derive(Clone)]
pub struct canvas_picture {
    pub size: (i64, i64),
    pub dynamic: bool,
    pub scene: Nodes,
    /// the plan this picture was made from, by identity (`workspace_images::plan_id`)
    pub plan: u64,
    pub prepared: Option<Rc<drawing::prepared>>,
    /// the node shown instead of the pane's own picture
    pub preview: Option<(Ty, i64)>,
}

/// A World bake for this frame: the preview size while a gesture or the day cycle is live, the
/// final size when idle.
pub fn bake_world(view: Option<&View>, previous: Option<&(world::World, Rc<world::baked>)>, core: &Model, live: bool) -> Option<(world::World, Rc<world::baked>)> {
    let time = timeline::time(&editor_list::timeline(core));
    editor_host::world(view, core, Some(time)).and_then(|w| {
        let (width, height) = if live { (512, 256) } else { (2048, 1024) };
        match previous {
            // The same World keeps the same bake value, so renderers see no change.
            Some((last, baked)) if *last == w && baked.camera.width == width => Some((w, baked.clone())),
            _ => world::bake_cached(width, height, &w).ok().map(|b| (w, Rc::new(b))),
        }
    })
}

/// The viewports over another scene instance: those that draw objects of their own and those that
/// show another World.
pub fn view_keys(core: &Model) -> Vec<String> {
    let own: Vec<String> = core.doc.shell.as_ref().map(|s| s.views.iter().map(|(k, _)| k.clone()).collect()).unwrap_or_default();
    let mut out = own.clone();
    out.extend(core.doc.view_worlds.iter().map(|(k, _)| k.clone()).filter(|k| !own.contains(k)));
    out
}

pub fn save_status<R>(save: &dyn Fn(&R) -> Result<(), String>, filename: &dyn Fn(&R) -> String, pending: Option<&R>, rendered: Option<&d3::Scene3>) -> Option<String> {
    match (pending, rendered) {
        (Some(request), Some(_)) => Some(match save(request) {
            Ok(()) => format!("Saved {}", filename(request)),
            Err(message) => format!("Render failed: {message}"),
        }),
        _ => None,
    }
}

/// How a viewport is painted: its key, its bounds, the camera and the image.
pub type PaintView<'a> = dyn Fn(&str, Bounds, &rcam::Camera, &d3::Scene3) -> Nodes + 'a;

pub fn world_nodes(paint_view: &PaintView, key: &str, camera: &rcam::Camera, rendered: Option<&d3::Scene3>, view_visible: bool, viewport: Bounds) -> Nodes {
    match rendered {
        Some(image) if view_visible => paint_view(key, viewport, camera, image),
        _ => Vec::new(),
    }
}

/// Each viewport shows its own scene instance's image, else the primary one.
pub fn worlds(paint_view: &PaintView, camera: &dyn Fn(&str) -> rcam::Camera, rendered: Option<&d3::Scene3>, views: &[(String, d3::Scene3)], view_visible: bool, bodies: &[(String, Bounds)]) -> Nodes {
    bodies
        .iter()
        .flat_map(|(key, bounds)| {
            let image = views.iter().find(|(k, _)| k == key).map(|(_, i)| i).or(rendered);
            world_nodes(paint_view, key, &camera(key), image, view_visible, *bounds)
        })
        .collect()
}

pub fn hidden_only(ui_visible: bool, core: &Model) -> bool {
    !ui_visible && core.leader == editor_core::router::State::Idle
}

fn same_rendered(a: &Option<d3::Scene3>, b: &Option<d3::Scene3>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => d3::same(a, b),
        (None, None) => true,
        _ => false,
    }
}

/// The fully hidden scene, reused while its inputs are unchanged so the renderer sees the same
/// `Scene.t`.
pub fn hidden_entry(background: color::T, rendered: Option<&d3::Scene3>, camera: &rcam::Camera, paint_view: &PaintView, cache: Option<&hidden_scene_cache>, core: &Model, frame_: &frame::Frame) -> hidden_scene_cache {
    let view_visible = editor_shell::view_visible(&core.into());
    let rendered_owned = rendered.cloned();
    if let Some(cached) = cache {
        if cached.width == frame_.input.size.0 && cached.height == frame_.input.size.1 && same_rendered(&cached.rendered, &rendered_owned) && cached.camera == *camera && cached.background == background && cached.view_visible == view_visible {
            return cached.clone();
        }
    }
    let mut scene = vec![scene::clear(background)];
    scene.extend(world_nodes(paint_view, "@hidden", camera, rendered, view_visible, (0, 0, frame_.input.size.0, frame_.input.size.1)));
    hidden_scene_cache { width: frame_.input.size.0, height: frame_.input.size.1, rendered: rendered_owned, camera: camera.clone(), background, view_visible, scene }
}

#[allow(clippy::too_many_arguments)]
pub fn compose_view(
    map: Option<&image::Image>,
    canvases: &[(String, canvas_picture)],
    ui_visible: bool,
    background: color::T,
    rendered: Option<&d3::Scene3>,
    focused: Option<&d3::Scene3>,
    views: &[(String, d3::Scene3)],
    camera: &rcam::Camera,
    camera_of: &dyn Fn(&str) -> rcam::Camera,
    paint_view: &PaintView,
    guides: &dyn Fn(Bounds) -> Nodes,
    cache: Option<&hidden_scene_cache>,
    core: &Model,
    frame_: &frame::Frame,
) -> Nodes {
    let view_visible = editor_shell::view_visible(&core.into());
    let paint_world = |viewport: Bounds| -> Nodes {
        match map {
            Some(image) if view_visible && core.map_view => {
                let (x, y, w, _) = map_rect(viewport);
                let scale = w as f64 / image::dimensions(image).0.max(1) as f64;
                scene::image(image, (x, y), Some(scale), None).ok().into_iter().collect()
            }
            _ => {
                let key = match &core.focus {
                    Panel::View(key) => key.clone(),
                    _ => "main".to_string(),
                };
                world_nodes(paint_view, &key, camera, focused, view_visible, viewport)
            }
        }
    };
    // each viewport is drawn from its own orbit
    if !ui_visible {
        if let Panel::Canvas(key) = &core.focus {
            let mut out = vec![scene::clear(background)];
            if let Some((_, picture)) = canvases.iter().find(|(k, _)| k == key) {
                out.extend(picture.scene.iter().cloned());
            }
            out.extend(editor_host::machinery(&core.into(), None, false));
            return out;
        }
    }
    if hidden_only(ui_visible, core) {
        return hidden_entry(background, focused, camera, paint_view, cache, core, frame_).scene;
    }
    if !ui_visible {
        let mut out = vec![scene::clear(background)];
        out.extend(world_nodes(paint_view, "@hidden", camera, focused, view_visible, (0, 0, frame_.input.size.0, frame_.input.size.1)));
        out.extend(editor_host::machinery(&core.into(), None, false));
        return out;
    }
    let geometry = editor_shell::geometry(&core.into(), &core.workspace, frame_);
    let viewport = editor_shell::panes_in(&core.into(), &geometry).view;
    let guides = if view_visible { guides(viewport) } else { Vec::new() };
    // every viewport shows its own scene instance's image; the map replaces the focused one
    let floating: Vec<(String, Vec<usize>, Bounds)> = geometry
        .leaves
        .iter()
        .filter_map(|leaf| match &leaf.panel {
            Panel::View(key) if leaf.floating => Some((key.clone(), leaf.path.clone(), leaf.body)),
            _ => None,
        })
        .collect();
    let root_of = |panel: &Panel, path: &Vec<usize>| -> Option<i64> {
        core.pane_keys.iter().find_map(|(root, (p, pp))| if p == panel && pp.as_ref() == Some(path) { Some(*root) } else { None })
    };
    // newest first, as OCaml's prepended lists are
    let mut painted: Vec<Nodes> = Vec::new();
    let mut windows: Vec<(Option<i64>, Nodes)> = Vec::new();
    for (key, bounds) in editor_shell::view_bodies_in(&core.into(), &geometry) {
        let image = if bounds == viewport && core.map_view { paint_world(viewport) } else { worlds(paint_view, camera_of, rendered, views, view_visible, &[(key.clone(), bounds)]) };
        match floating.iter().find(|(k, _, _)| *k == key) {
            None => painted.insert(0, image),
            Some((_, path, (x, y, w, h))) => {
                let root = root_of(&Panel::View(key.clone()), path);
                // inside the window's line-2 frame, which Chrome paints on the body's edge
                let mut with_fill: Nodes = scene::rect((x + 1, y + 1), w - 2, h - 2, Some(color::white()), None).ok().into_iter().collect();
                with_fill.extend(image);
                windows.insert(0, (root, with_fill));
            }
        }
    }
    let mut canvas_painted: Vec<Nodes> = Vec::new();
    let mut canvas_windows: Vec<(Option<i64>, Nodes)> = Vec::new();
    for leaf in &geometry.leaves {
        if let Panel::Canvas(key) = &leaf.panel {
            let (x, y, w, h) = leaf.body;
            let scene_ = canvases.iter().find(|(k, _)| k == key).map(|(_, p)| p.scene.clone()).unwrap_or_default();
            let root = root_of(&Panel::Canvas(key.clone()), &leaf.path);
            let picture: Nodes = vec![scene::clip((x, y), w, h, vec![scene::translate(x, y, scene_)])];
            if leaf.floating {
                canvas_windows.insert(0, (root, picture));
            } else {
                canvas_painted.insert(0, picture);
            }
        }
    }
    let mut all_windows = canvas_windows;
    all_windows.extend(windows);
    let under = |key: i64| -> Nodes { all_windows.iter().find(|(r, _)| *r == Some(key)).map(|(_, n)| n.clone()).unwrap_or_default() };
    let mut order: Vec<Nodes> = canvas_painted;
    order.extend(painted);
    order.reverse();
    let mut out = vec![scene::clear(background)];
    out.extend(order.into_iter().flatten());
    out.extend(guides);
    out.extend(editor_host::machinery(&core.into(), Some(&under), true));
    out
}
