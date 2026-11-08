//! The editor's frame coordinator and resource lifetime. Port of lib/rays_editor/editor_loop.ml.
//!
//! Conventions of this port (see loop_model.rs): `update_with`'s `inspector` answers `Rc<dyn Any>`
//! (OCaml's `'a`); frame closures receive its `Ui`. `Mem.root` attributes retained source and
//! checked-term buffers when the immutable document changes; allocator/Arc metadata and other
//! lowering owners remain outside that accounting.
#![allow(non_camel_case_types, non_upper_case_globals)]
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels::Panel;
use editor_document::document::Level;
use editor_document::lowering;
use editor_document::workspace_doc;
use flow_graph::probe;
use flow_native::drawing;
use flow_native::live_frame;
use flow_native::workspace_gpu;
use flow_native::workspace_host;
use flow_native::workspace_images;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::Value as EValue;
use input::{Event, Key, MouseButton};
use pxui::ui::Ui;
use pxui_shell::layout::Bounds;
use rays::frame;
use rays::image;
use rays::scene::{self, node as scene_node};
use rays::sketch;
use rays_diagnostic::Diagnostic;
use sop::context::dependencies::{self as deps, Fact};

use crate::cook;
use crate::editor_commands::{self, action};
use crate::editor_frame;
use crate::editor_history as history;
use crate::editor_host;
use crate::editor_inspect;
use crate::editor_list;
use crate::editor_model::{self as m, notice_kind, update, Panel_result};
use crate::editor_panes;
use crate::editor_shell;
use crate::editor_status;
use crate::editor_text;
use crate::loop_model::*;
use crate::loop_source::*;
use crate::loop_view::*;
use crate::source_file;
use crate::text_pane;
use crate::timeline;
use crate::viewport as V;
use crate::world_map::*;

type Nodes = Vec<scene_node>;

thread_local! {
    static MEMORY_DOCUMENT: RefCell<std::sync::Weak<editor_document::document::T>> = RefCell::new(std::sync::Weak::new());
}

fn document_memory(value: &Arc<editor_document::document::T>) {
    MEMORY_DOCUMENT.with(|last| {
        let mut last = last.borrow_mut();
        if last.upgrade().is_some_and(|old| Arc::ptr_eq(&old, value)) { return; }
        let (syntax, terms) = workspace_doc::memory_allocations(&value.workspace.ws);
        // Each callback owns the immutable root whose allocations its cached identities describe.
        let syntax_document = value.clone();
        mem::root(&mem::document("editor.document.syntax"), Arc::new(move |visitor| {
            std::hint::black_box(&syntax_document);
            for &(address, bytes) in &syntax { visitor.allocation(address, bytes); }
        }));
        let term_document = value.clone();
        mem::root(&mem::document("editor.document.terms"), Arc::new(move |visitor| {
            std::hint::black_box(&term_document);
            for &(address, bytes) in &terms { visitor.allocation(address, bytes); }
        }));
        *last = Arc::downgrade(value);
    });
}

fn host_gpu(value: &t) -> workspace_gpu::t {
    value.host.borrow().gpu.clone()
}
fn host_images(value: &t) -> workspace_images::t {
    value.host.borrow().images.clone()
}

/// each canvas panel's picture: reused while its plan, size and preview stand, else rendered
pub fn canvas_pictures(context_error: Option<String>, value: &t, input: &input::Frame, raw_frame: &frame::t, core: &m::t, control: &V::control) -> (Vec<(String, canvas_picture)>, Option<String>) {
    let mut pictures: Vec<(String, canvas_picture)> = Vec::new();
    let mut error = context_error;
    let geometry = editor_shell::geometry(core, &core.workspace, raw_frame);
    let images = host_images(value);
    let gpu_t = host_gpu(value);
    for leaf in &geometry.leaves {
        let key = match &leaf.panel {
            Panel::Canvas(key) => key.clone(),
            _ => continue,
        };
        let (_, _, w, h) = if !V::ui_visible(control) && core.focus == Panel::Canvas(key.clone()) { (0, 0, raw_frame.input.size.0, raw_frame.input.size.1) } else { leaf.body };
        // a picture's frame is its pane: frame/width and frame/height read the canvas, not the window
        let input = input::Frame { size: (w, h), ..input.clone() };
        let drawing_value = core.doc.shell.as_ref().and_then(|shell| shell.canvases.iter().find(|(k, _)| *k == key).map(|(_, v)| v.clone()));
        let lowered = &core.doc.workspace.lowered;
        let preview = core.canvas_viewed.clone();
        let drawing_value = match &preview {
            Some((t, id)) if *t == ty::drawing() => Some(EValue::Deferred(t.clone(), *id)),
            _ => drawing_value,
        };
        workspace_images::bind(&images, lowered);
        let previous = value.canvases.iter().find(|(k, _)| *k == key).map(|(_, p)| p.clone());
        let pid = workspace_images::plan_id(&lowered.plan);
        let same_plan = previous.as_ref().is_some_and(|p| p.plan == pid);
        let dynamic = match &previous {
            _ if preview.is_some() => true,
            Some(picture) if same_plan && picture.preview == preview => picture.dynamic,
            _ => !lowered.states.is_empty() || lowered.plan.nodes.iter().any(|n| n.args.iter().any(|(_, v)| flow_eval::is_live(v) || flow_eval::frame_dependent(v))),
        };
        let blank = |previous: &Option<canvas_picture>| -> canvas_picture {
            previous.clone().unwrap_or(canvas_picture { size: (w, h), dynamic, plan: pid, scene: Vec::new(), prepared: None, preview: preview.clone() })
        };
        // a canvas previewing an image node
        if let Some((pty, id)) = preview.clone().filter(|(pty, _)| *pty == ty::image()) {
            let result = workspace_images::image(None, true, &images, &core.cook.state, &input, &lowered.plan, &EValue::Deferred(pty, id));
            match result {
                Ok(img) => {
                    let (iw, ih) = image::dimensions(&img);
                    let scale = (w as f64 / iw as f64).min(h as f64 / ih as f64);
                    let mut scene_: Nodes = scene::rect((0, 0), w, h, Some(rays_math::color::black()), None).ok().into_iter().collect();
                    scene_.extend(scene::image(&img, (0, 0), Some(scale), None).ok());
                    pictures.insert(0, (key, canvas_picture { size: (w, h), dynamic, plan: pid, scene: scene_, prepared: None, preview: preview.clone() }));
                }
                Err(d) => {
                    pictures.insert(0, (key, blank(&previous)));
                    error = Some(rays_diagnostic::to_string(&d));
                }
            }
            continue;
        }
        let drawing_value = match drawing_value {
            Some(d) => d,
            None => continue,
        };
        if same_plan && !dynamic && previous.as_ref().is_some_and(|p| p.size == (w, h) && p.preview == preview) {
            pictures.insert(0, (key, previous.expect("previous")));
            continue;
        }
        let fast = lowering::fast(&core.doc);
        let gpu = workspace_gpu::config(&gpu_t, fast);
        let prepare = |previous: &Option<canvas_picture>| -> Result<Rc<drawing::prepared>, Diagnostic> {
            match previous {
                Some(canvas_picture { prepared: Some(p), preview: held, .. }) if same_plan && *held == preview => Ok(p.clone()),
                _ => drawing::prepare(
                    &drawing::PrepareOptions { gpu: Some(gpu.clone()), profile: Some(lowered.profile.clone()), approx: None, states: lowered.states.clone() },
                    &lowered.plan,
                    &drawing_value,
                )
                .map(Rc::new),
            }
        };
        let mut rendered: Option<Result<(Rc<drawing::prepared>, Nodes), Diagnostic>> = None;
        match prepare(&previous) {
            Err(d) => rendered = Some(Err(d)),
            Ok(p) => {
                workspace_gpu::with_backend(&gpu_t, fast, &mut |gpu: &flow_ir::flow_ir::Gpu::Config| {
                    let image_images = images.clone();
                    let state = core.cook.state.clone();
                    let live = input.clone();
                    let plan = lowered.plan.clone();
                    let gpu_for_image = gpu.clone();
                    let resolver: drawing::image_resolver = Rc::new(move |v: &EValue| workspace_images::image(Some(&gpu_for_image), true, &image_images, &state, &live, &plan, v));
                    let options = drawing::RenderOptions {
                        state: Some(core.cook.state.clone()),
                        image: Some(resolver),
                        gpu: Some(workspace_gpu::marks(&gpu_t)),
                        resident: workspace_gpu::resident(gpu, &gpu_t),
                    };
                    rendered = Some(drawing::render_prepared(&p, &input, (w, h), &options).map(|scene_| (p.clone(), scene_)));
                });
            }
        }
        match rendered.expect("rendered") {
            Ok((prepared, scene_)) => pictures.insert(0, (key, canvas_picture { size: (w, h), dynamic, plan: pid, scene: scene_, prepared: Some(prepared), preview: preview.clone() })),
            Err(d) => {
                // the last picture stays up, unless its mark sets bind GPU outputs this failed
                // render may already have replaced
                let previous = previous.map(|mut p| {
                    if p.prepared.as_ref().is_some_and(|prepared| drawing::borrows_gpu(prepared)) {
                        p.scene = Vec::new();
                    }
                    p
                });
                pictures.insert(0, (key, blank(&previous)));
                error = Some(rays_diagnostic::to_string(&d));
            }
        }
    }
    (pictures, error)
}

/// the images the selected nodes show are prepared before the frame; what failed is reported
pub fn preview_images(value: &t, frame_: &frame::t) -> Vec<String> {
    let lowered = &value.core.doc.workspace.lowered;
    let images = host_images(value);
    workspace_images::bind(&images, lowered);
    let gpu = workspace_gpu::config(&host_gpu(value), lowering::fast(&value.core.doc));
    let live = input::Frame { t: timeline::time(&editor_list::timeline(&value.core)), ..live_frame::of_frame(frame_) };
    let mut errors: Vec<String> = Vec::new();
    let mut preview = |image: EValue| {
        if let Err(diagnostic) = workspace_images::image(Some(&gpu), true, &images, &value.core.cook.state, &live, &lowered.plan, &image) {
            errors.push(rays_diagnostic::to_string(&diagnostic));
        }
    };
    if let Some(laid) = m::pane_laid(&value.core) {
        if let Some(records) = &laid.records {
            let chains = probe::chains(&laid.scope);
            for path in pxui_graph::scope_state::selected(m::pane_view(&value.core)) {
                let probes: Vec<i64> = chains.get(&path).cloned().unwrap_or_default().iter().map(|zone| value.core.probes.get(zone).copied().unwrap_or(0)).collect();
                let summary = records.borrow_mut().at(&path, &probes);
                match summary {
                    Some(probe::Summary::Value(v @ EValue::Deferred(Ty::Named(_), _))) if matches!(&v, EValue::Deferred(Ty::Named(n), _) if n == "image") => preview(v),
                    Some(probe::Summary::Image(image)) => preview(EValue::Deferred(ty::image(), image.node)),
                    _ => {}
                }
            }
        }
    }
    errors
}

/// The World drag of one frame, from the view pane's capture signal (the one press/capture path:
/// press point, held modifiers, motion and release point arrive per frame, a press and a far jump in
/// one frame included). The operation is latched at the press; movement and the release position
/// are reduced before sealing its history. Updates the drag and returns the input
/// with the drag's pointer events taken out, the held modifiers at the end of the frame, and the
/// pane's press and release for the camera's pointer grab.
pub fn world_drag_frame(value: &mut t, before: (Level, bool, &[Key], &[(i64, (Panel, Option<pxui_shell::layout::Path>))]), loaded: bool, actions: &[action], sent: &frame::t, area: Bounds) -> (frame::t, Vec<Key>, (bool, bool)) {
    let (previous_level, previous_map, held_keys, pane_keys) = before;
    let seal = |core: &mut m::t| { core.history = history::seal(false, core.history.clone()); };
    let world_drag = if loaded
        || m::pane_level(&value.core) != previous_level
        || value.core.map_view != previous_map
        || actions.iter().any(|a| matches!(a, action::Undo | action::Redo | action::Hide_ui))
    { None } else { value.world_drag.clone() };
    let modifiers = input::keys_before(held_keys, &sent.input.keys, &sent.input.events);
    let modifiers = sent.input.events.iter().fold(modifiers, |keys, e| input::keys_after(&keys, e));
    let ui = value.core.ui.clone();
    let ui = ui.borrow();
    let view = pane_keys.iter().find_map(|(key, (panel, _))| if matches!(panel, Panel::View(_)) { ui.box_of_key(*key) } else { None });
    let signal = view.as_ref().map(|b| ui.signal(b));
    let core = Rc::make_mut(&mut value.core);
    let (world_drag, engaged) = match (&signal, &view) {
        (Some(s), Some(box_)) => {
            let drag = match world_drag {
                None if s.pressed && s.button == Some(MouseButton::LeftButton) => match world_operation(core, area, s.press_point, ui.press_keys(box_).contains(&Key::Shift)) {
                    Some(operation) => { seal(core); Some(self::world_drag { operation, point: s.press_point, area }) },
                    None => None,
                },
                drag => drag,
            };
            match drag {
                Some(drag) if s.released => { move_world(core, &drag, s.release_point); seal(core); (None, true) },
                Some(drag) if s.held => { move_world(core, &drag, s.pointer); (Some(self::world_drag { point: s.pointer, ..drag }), true) },
                Some(drag) => { move_world(core, &drag, s.pointer); seal(core); (None, true) },
                None => (None, false),
            }
        }
        _ => {
            let engaged = world_drag.is_some();
            if engaged { seal(core); }
            (None, engaged)
        }
    };
    value.world_drag = world_drag;
    // the drag's pointer events are the drag's: the camera and picking see a cancelled press
    let input = if !engaged {
        sent.clone()
    } else {
        let mut events: Vec<Event> = sent.input.events.iter().filter(|e| !matches!(e, Event::Pointer_pressed(MouseButton::LeftButton, _) | Event::Pointer_moved(_) | Event::Pointer_released(MouseButton::LeftButton, _))).cloned().collect();
        events.push(Event::Pointer_cancelled(MouseButton::LeftButton));
        frame::t {
            input: input::Frame { events, buttons: sent.input.buttons.iter().filter(|b| **b != MouseButton::LeftButton).cloned().collect(), ..sent.input.clone() },
            mouse_delta: (0., 0.),
            ..sent.clone()
        }
    };
    let grab = match &signal {
        Some(s) => (s.pressed && !engaged, s.released && !engaged),
        None => (false, false),
    };
    (input, modifiers, grab)
}

/// what the panes draw over the views: header tools, the gate where a film is not the whole pane,
/// the selected object box in the focused view, and the render clock
#[allow(clippy::too_many_arguments)]
pub fn with_view_overlays(value: &mut t, now: f64, extra: &V::extra, viewing: &Option<String>, camera: &V::camera, cameras: &[(String, V::camera)], pending: bool, bodies: &[(String, Bounds)]) {
    let core = Rc::make_mut(&mut value.core);
    core.view_tools = V::header_tools(extra);
    // what the panes draw over a view: the gate where it is not the whole pane, and the selected
    // object's box in the focused view
    core.gates = bodies
        .iter()
        .filter_map(|(key, bounds)| {
            let film = V::film(extra, &look_key_of(viewing, key), *bounds);
            if film == *bounds || key == "@hidden" { None } else { Some((key.clone(), film)) }
        })
        .collect();
    core.selected_box = editor_list::selected_bounds(&*core).and_then(|(world, box_, name)| {
        let key = focus_key_of(viewing);
        bodies.iter().find(|(k, _)| *k == key).and_then(|(_, bounds)| {
            let view_ = camera_for(viewing, camera, cameras, extra, pending, &key);
            V::screen_box(&view_, V::film(extra, &look_key_of(viewing, &key), *bounds), &world, &box_).map(|rect| (key.clone(), rect, name))
        })
    });
    // the render clock: a restart of the samples starts it, reaching the cap stops it
    core.traces = bodies
        .iter()
        .filter_map(|(key, _)| {
            V::trace(extra, &look_key_of(viewing, key)).map(|(film, samples, cap, bounces)| {
                let (seconds, since) = match core.traces.iter().find(|(k, _)| k == key) {
                    Some((_, t)) if samples >= t.samples && t.film == film => (if t.samples >= cap { t.seconds } else { now - t.since }, t.since),
                    _ => (0., now),
                };
                (key.clone(), m::trace { film, samples, cap, bounces, seconds, since })
            })
        })
        .collect();
}

/// The frame's clicks pick in the view, and a payload carried over a viewport finds its surface.
/// Updates the core and the press still waiting for its release.
#[allow(clippy::too_many_arguments)]
pub fn pick_in_views(value: &mut t, focused: &Focus, camera: &V::camera, extra: &V::extra, area: Bounds, modifiers: &[Key], input: &frame::t, raw_frame: &frame::t) {
    // A press and release within the drag threshold is a click: the view picks.
    let mut press = value.pick_press;
    let mut clicks: Vec<(f64, f64)> = Vec::new();
    for event in &input.input.events {
        match event {
            Event::Pointer_pressed(MouseButton::LeftButton, point) => press = Some(*point),
            Event::Pointer_released(MouseButton::LeftButton, (qx, qy)) => {
                match press {
                    Some((px, py)) if (qx - px).hypot(qy - py) < pxui::ui::drag_threshold => clicks.insert(0, (*qx, *qy)),
                    _ => {}
                }
                press = None;
            }
            Event::Pointer_cancelled(MouseButton::LeftButton) | Event::Focus_lost => press = None,
            _ => {}
        }
    }
    let pending = value.pending_render.is_some();
    let core = Rc::make_mut(&mut value.core);
    if !core.map_view {
        for at in clicks.iter().rev() {
            let film = V::film(extra, &focus_key_of(&focused.viewing), area);
            let (_, _, w, h) = film;
            if w <= 0 || h <= 0 {
                continue;
            }
            let view_ = camera_for(&focused.viewing, camera, &focused.cameras, extra, pending, &focus_key_of(&focused.viewing));
            if let Some((origin, direction)) = V::pick_ray(film, &view_, *at) {
                editor_host::pick(focused.viewing.as_deref(), modifiers.contains(&Key::Alt), core, origin, direction);
            }
        }
    }
    // a payload carried over a viewport: the surface under the pointer is the place
    if let Some(key) = editor_host::carry_viewport(&core) {
        if !core.map_view {
            if let Some((_, bounds)) = editor_shell::view_bodies(&*core, raw_frame).into_iter().find(|(k, _)| *k == key) {
                let film = V::film(extra, &look_key_of(&focused.viewing, &key), bounds);
                let (_, _, w, h) = film;
                if w > 0 && h > 0 {
                    let view_ = camera_for(&focused.viewing, camera, &focused.cameras, extra, pending, &key);
                    if let Some((origin, direction)) = V::pick_ray(film, &view_, raw_frame.input.pointer) {
                        editor_host::carry_over_surface(Some(&key), core, origin, direction);
                    }
                }
            }
        }
    }
    value.pick_press = press
}

thread_local! {
    /// the images the frame's cook may ask the dimensions of; the probe records keep the closure
    /// past the frame, so it reads the images of the latest frame (never the model)
    static FRAME_IMAGES: RefCell<Option<workspace_images::t>> = const { RefCell::new(None) };
}

fn error_status_of(value: &t, image_errors: &[String]) -> Option<String> {
    if !image_errors.is_empty() {
        Some(image_errors.join("; "))
    } else if value.memory_error.is_some() {
        value.memory_error.clone()
    } else if value.state_error.is_some() {
        value.state_error.clone()
    } else {
        match (&value.context_error, V::render_status(&value.extra)) {
            (Some(live), Some(renderer)) => Some(format!("{live}; {renderer}")),
            (Some(e), None) => Some(e.clone()),
            (None, Some(e)) => Some(e),
            (None, None) => None,
        }
    }
}

pub fn update_with(value: &mut t, frame_: &frame::t, inspector: &dyn Fn(&mut Ui) -> Rc<dyn Any>) -> Option<Rc<dyn Any>> {
    let drawn_before = value.drawn.clone();
    let followed = follow_focus(&value, &value.core, frame_);
    (value.viewing, value.camera, value.cameras) = (followed.viewing, followed.camera, followed.cameras);
    let ui = value.core.ui.clone();
    let raw_frame = frame_.clone();
    let (extra, frame) = V::begin_frame(&value.extra, frame_.clone());
    let visible = V::ui_visible(&value.control);
    let panel_control = value.control.clone();
    let panel_camera = value.camera.clone();
    let panel_extra = extra.clone();
    let camera_panel = move |ui: &mut Ui| -> Panel_result { Rc::new(V::panel(ui, &panel_control, &panel_camera, &panel_extra, inspector)) };
    let handles_scene = editor_inspect::scene(&value.core);
    let handles_view = view_camera(&value);
    let handles_extra = extra.clone();
    let handles_key = focus_key(&value);
    let view_handles = move |ui: &mut Ui, selected: Option<&m::picked>, space: rays_math::mat4::T, bounds: Bounds| {
        // a handle is where the picture shows its point: in the view's film, not its pane
        V::handles(ui, selected, &handles_scene, &space, &handles_view, &handles_extra, V::film(&handles_extra, &handles_key, bounds))
    };
    let before_level = m::pane_level(&value.core);
    let before_map = value.core.map_view;
    let before_keys = value.core.held_keys.clone();
    let before_panes = value.core.pane_keys.clone();
    let document = value.core.doc.clone();
    let lowered = &document.workspace.lowered;
    let image_errors = preview_images(&value, &frame);
    let images = host_images(&value);
    FRAME_IMAGES.with(|f| *f.borrow_mut() = Some(images.clone()));
    let gpu_t = host_gpu(&value);
    let text_focus = ui.borrow().text_input_focused();
    let error_status = error_status_of(&value, &image_errors);
    let view_state: Rc<dyn Fn(Option<&Panel_result>) -> flow_syntax::syntax::Syntax> = {
        let (camera, extra) = (value.camera.clone(), value.extra.clone());
        Rc::new(move |panel| match panel.and_then(|p| (**p).downcast_ref::<V::panel_out>()) {
            Some(p) => V::section(&p.camera, &p.extra),
            None => V::section(&camera, &extra),
        })
    };
    let host_events = workspace_host::take_events(&mut value.host.borrow_mut());
    let mut updated: Option<update> = None;
    let mut host_events = Some(host_events);
    workspace_gpu::with_backend(&gpu_t, lowering::fast(&value.core.doc), &mut |gpu| {
        let metadata: flow_sop::lower::ImageMetadata = Arc::new(|plan, id| FRAME_IMAGES.with(|f| f.borrow().as_ref().and_then(|images| workspace_images::peek(images, plan, id)).map(|i| image::dimensions(&i))));
        flow_sop::lower::with_images(Some(gpu.clone()), Some(metadata), workspace_images::payload(&images), &mut || {
            let pump = || {
                workspace_gpu::pump(&gpu_t, false);
                workspace_gpu::wait(&gpu_t, 0.0005);
            };
            let image = |id: i64| workspace_images::peek(&images, &lowered.plan, id);
            updated = Some(editor_frame::update(
                Rc::make_mut(&mut value.core),
                visible,
                text_focus,
                &camera_panel,
                &view_handles,
                value.render_status.clone(),
                error_status.clone(),
                view_state.clone(),
                &frame,
                Some(gpu),
                Some(&image),
                host_events.take().unwrap_or_default(),
                Some(&pump),
            ));
        });
    });
    let mut update_ = updated.expect("update ran");
    open_dialog(value, &mut update_, &frame.input.events);
    open_import(value, &mut update_);
    reload_source(value, &mut update_, frame.input.t);
    let focused = follow_focus(&value, &value.core, &frame);
    let panes = editor_shell::panes(&value.core, &frame);
    let panel_out: Option<Rc<V::panel_out>> = update_.panel.take().and_then(|p| Rc::downcast::<V::panel_out>(p).ok());
    let (control, camera, requests, extra, inspected) = match panel_out {
        Some(p) => {
            let camera = if focused.viewing != value.viewing { focused.camera.clone() } else { p.camera.clone() };
            (p.control.clone(), camera, p.requests.clone(), p.extra.clone(), Some(p.inspected.clone()))
        }
        None => (value.control.clone(), focused.camera.clone(), Vec::new(), extra, None),
    };
    let (camera, extra) = match &update_.loaded_view {
        Some(json) => V::restore(&camera, &extra, json),
        None => (camera, extra),
    };
    let previous_camera = if update_.loaded_view.is_none() { focused.camera.clone() } else { camera.clone() };
    let mut control = control;
    let mut extra = extra;
    let mut render_status = value.render_status.clone();
    for a in &update_.actions {
        match a {
            action::Hide_ui => control = V::toggle_ui(&control),
            action::Open_camera => control = V::open_camera(&control),
            other => {
                let (e, notice) = V::apply_action(&camera, &extra, other);
                extra = e;
                if notice.is_some() {
                    render_status = notice;
                }
            }
        }
    }
    V::on_doc(&document, Rc::make_mut(&mut value.core), &camera);
    // Command-S applies drafts before writing; rejected drafts keep the save waiting.
    let source = if !update_.actions.contains(&action::Save_source) {
        value.source.clone()
    } else if editor_text::apply_drafts(value.refused.as_deref(), Rc::make_mut(&mut value.core)) {
        save_source(Rc::make_mut(&mut value.core), value.source.clone(), &V::section(&camera, &extra))
    } else {
        Rc::make_mut(&mut value.core).notice = Some((notice_kind::Refusal, "Not saved: the text pane's draft does not check \u{b7} fix or discard it, then save".to_string()));
        value.source.clone()
    };
    if source != value.source {
        value.held = None;
    }
    let area = if visible { panes.view } else { (0, 0, frame.input.size.0, frame.input.size.1) };
    let (input, modifiers, grab) = world_drag_frame(value, (before_level, before_map, &before_keys, &before_panes), update_.loaded_view.is_some(), &update_.actions, &update_.input, area);
    pick_in_views(value, &focused, &camera, &extra, area, &modifiers, &input, &raw_frame);
    let (camera, extra) = if value.core.map_view { (camera, V::release(&extra)) } else { V::navigate(area, grab, &control, &camera, &extra, &value.core, &raw_frame, &input) };
    let (camera, render_status) = match &update_.framed {
        Some(Some((min, max))) => {
            let (_, _, w, h) = panes.view;
            if w > 0 && h > 0 { (V::frame_bounds(*min, *max, &camera), render_status) } else { (camera, render_status) }
        }
        Some(None) => (camera, Some("Nothing to frame: no cooked points".to_string())),
        None => (camera, render_status),
    };
    let stepped = update_.actions.iter().any(|a| matches!(a, action::Undo | action::Redo));
    let (camera, extra) = V::on_view(Rc::make_mut(&mut value.core), &previous_camera, &focus_key_of(&focused.viewing), camera, extra, stepped, !raw_frame.input.buttons.is_empty());
    let ends_pointer = |e: &Event| matches!(e, Event::Pointer_released(MouseButton::LeftButton, _) | Event::Pointer_cancelled(MouseButton::LeftButton) | Event::Focus_lost);
    if !frame::mouse_down(MouseButton::LeftButton, &raw_frame) || frame::has_event(&ends_pointer, &raw_frame) || frame::has_event(&ends_pointer, &update_.input) {
        Rc::make_mut(&mut value.core).history = history::seal(true, value.core.history.clone());
    }
    let live = value.world_drag.is_some() || timeline::mode(&editor_list::timeline(&value.core)) == timeline::Mode::Playing;
    let baked_from = bake_world(None, value.baked_from.as_ref(), &value.core, live);
    let baked = baked_from.as_ref().map(|(_, b)| b.clone());
    let new_views: Vec<(String, baked_pair)> = view_keys(&value.core)
        .into_iter()
        .filter_map(|key| {
            let previous = value.baked_views.iter().find(|(k, _)| *k == key).map(|(_, p)| p.clone());
            bake_world(Some(&editor_inspect::View::Only(key.clone())), previous.as_ref(), &value.core, live).map(|bake| (key, bake))
        })
        .collect();
    // the same bakes as last frame: nothing to recompose for
    let baked_views = if new_views.len() == value.baked_views.len() && new_views.iter().zip(&value.baked_views).all(|((k, (_, b)), (k2, (_, b2)))| k == k2 && Rc::ptr_eq(b, b2)) { value.baked_views.clone() } else { new_views };
    let timeline_ = editor_list::timeline(&value.core);
    let time = timeline::time(&timeline_);
    let live_input = input::Frame { t: time, frame: timeline::frame(&timeline_), ..value.core.live_frame.clone() };
    let reuse = match &value.resolved {
        Some((doc, at, scene, error)) if same_context(doc, &value.core.doc) && !sop::context::input_changed(lowering::scene_reads(doc), at, &live_input) => Some((scene.clone(), error.clone())),
        _ => None,
    };
    let (scene, scene_error) = match reuse {
        Some(r) => r,
        None => {
            let previous = match &value.resolved {
                Some((doc, _, scene, _)) if same_context(doc, &value.core.doc) => Some(scene.clone()),
                _ => None,
            };
            let (scene, errors) = lowering::resolve_scene(previous.as_ref(), Some(&value.core.cook.state), Some(&live_input), &value.core.doc, time);
            (scene, if errors.is_empty() { None } else { Some(errors.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")) })
        }
    };
    let context_error = if image_errors.is_empty() { scene_error.clone() } else { Some(image_errors.join("; ")) };
    let (canvases, context_error) = canvas_pictures(context_error, &value, &live_input, &raw_frame, &value.core, &control);
    let composed_ = compose(&value, &value.core, update_.prepared_changed || update_.scene_changed || update_.effects.view || update_.effects.export, &baked, &baked_views, &scene);
    workspace_gpu::frame(&gpu_t, if composed_.drawn_changed { Some(display_keys(&composed_.drawn)) } else { None }.as_deref());
    let context_error = if composed_.errors.is_empty() { context_error } else { Some(composed_.errors.join("; ")) };
    let rendered = composed_.rendered.clone();
    let views = composed_.views.clone();
    let bodies: Vec<(String, Bounds)> = if V::ui_visible(&control) { editor_shell::view_bodies(&value.core, &raw_frame) } else { vec![("@hidden".to_string(), (0, 0, raw_frame.input.size.0, raw_frame.input.size.1))] };
    let pending = value.pending_render.is_some();
    let focused_rendered = focused_image_of(&focused.viewing, &views, &rendered);
    let render_views: Vec<(String, Bounds, rays::camera::t, rays::scene3::t)> = bodies
        .iter()
        .filter_map(|(key, bounds)| {
            let scene3 = if key == "@hidden" { focused_rendered.clone() } else { views.iter().find(|(k, _)| k == key).map(|(_, s)| s.clone()).or_else(|| rendered.clone()) };
            scene3.map(|scene3| (key.clone(), V::film(&extra, &look_key_of(&focused.viewing, key), *bounds), camera_for(&focused.viewing, &camera, &focused.cameras, &extra, pending, key), scene3))
        })
        .collect();
    let focus = focus_key_of(&focused.viewing);
    let extra = V::render(&extra, raw_frame.pixel_scale, Some(&focus), render_views);
    with_view_overlays(value, frame.input.t, &extra, &focused.viewing, &camera, &focused.cameras, pending, &bodies);
    let map = match (&baked, &value.map) {
        (Some(b), Some((source, image_))) if Rc::ptr_eq(b, source) || !value.core.map_view => Some((source.clone(), image_.clone())),
        (Some(b), previous) if value.core.map_view => {
            let (width, height, rgba) = map_rgba(b);
            match image::upload_rgba(previous.as_ref().map(|p| &p.1), width, height, &rgba) {
                Ok(img) => Some((b.clone(), img)),
                Err(_) => previous.clone(),
            }
        }
        (_, previous) => previous.clone(),
    };
    let pending_render = requests.last().cloned();
    let render_status = if pending_render.is_some() && rendered.is_none() { Some("Render unavailable until the first cook completes".to_string()) } else { render_status };
    value.camera = camera;
    value.cameras = focused.cameras.clone();
    value.viewing = focused.viewing.clone();
    value.control = control;
    value.rendered = rendered;
    value.views = views;
    value.drawn = composed_.drawn.clone();
    value.composed = Some(composition_key(&value.core, &scene));
    value.resolved = Some((value.core.doc.clone(), live_input.clone(), scene, scene_error));
    value.context_error = context_error;
    value.canvases = canvases;
    value.baked = baked;
    value.baked_from = baked_from;
    value.baked_views = baked_views;
    value.map = map;
    value.pending_render = pending_render;
    value.render_status = render_status;
    value.extra = extra;
    value.source = source;
    refresh_hidden(value, &raw_frame);
    {
        let workspace = &value.core.doc.workspace.ws;
        let lowered = &value.core.doc.workspace.lowered;
        let result = workspace_host::update(&mut value.host.borrow_mut(), &value.core.cook.state, &live_input, &workspace, &lowered.plan);
        if let Err(diagnostic) = result {
            value.context_error = Some(rays_diagnostic::to_string(&diagnostic));
        }
    }
    let mut first_draws: Vec<rdk_core::geometry::T> = Vec::new();
    for (id, entry) in value.drawn.iter() {
        match drawn_before.get(id) {
            Some(known) if Rc::ptr_eq(&known.prepared, &entry.prepared) => {}
            _ => first_draws.insert(0, entry.prepared.geometry.clone()),
        }
    }
    value.presenting = match (first_draws.is_empty(), value.presenting.take()) {
        (true, None) => None,
        (true, Some((_, pending))) => Some(((value.core.clock)(), pending)),
        (false, _) => Some(((value.core.clock)(), first_draws)),
    };
    // a held button defers the write to its release: the synced write is tens of ms (F_FULLFSYNC)
    // and stuttered every orbit and slider drag at 2 Hz
    if frame.input.buttons.is_empty() { autosave(value, false, frame.input.t); }
    inspected
}

pub fn compose_scene(value: &t, frame_: &frame::t) -> Nodes {
    let pv = |key: &str, vp: Bounds, view_: &rays::camera::t, r: &rays::scene3::t| paint_view(value, key, vp, view_, r);
    let guides = |bounds: Bounds| -> Nodes {
        if value.core.map_view {
            Vec::new()
        } else {
            V::guides(
                &editor_inspect::scene(&value.core),
                editor_list::selected_node(&value.core).as_ref(),
                &editor_list::space(&value.core),
                &view_camera(value),
                &value.extra,
                bounds,
                V::film(&value.extra, &focus_key(value), bounds),
            )
        }
    };
    compose_view(
        value.map.as_ref().map(|(_, i)| i),
        &value.canvases,
        V::ui_visible(&value.control),
        value.background,
        value.rendered.as_ref(),
        focused_image(value).as_ref(),
        &value.views,
        &view_camera(value),
        &|key| camera_of(value, key),
        &pv,
        &guides,
        value.hidden_scene_cache.as_ref(),
        &value.core,
        frame_,
    )
}

/// An idle frame is not built: with no input, no cook and a clock nothing reads, a frame is the last
/// one again and its work is only allocation (Dear ImGui's bar: an idle frame never calls malloc).
///
/// What only time moves is either declared in `may_rest`, slow enough for one full frame every
/// `rest_seconds` (the status bar's fps), or runs at rest on its own period (the source file's poll,
/// autosave), waking the frame when it has something to show. The picture must also have stood still
/// for `rest_frames` full frames, which covers what no clause names: a visible clock moving is a
/// picture changing.
pub const rest_seconds: f64 = 1.;
pub const rest_frames: i64 = 2;

pub fn same_input(a: &frame::t, b: &frame::t) -> bool {
    a.input.size == b.input.size && a.pixel_scale == b.pixel_scale && a.input.pointer == b.input.pointer && a.input.buttons == b.input.buttons && a.input.keys == b.input.keys
}

/// a playing clock moves nothing unless a graph, the scene, the plan or a World reads it
pub fn reads_clock(value: &t) -> bool {
    let moves = |dependencies: deps::T| [Fact::Time, Fact::Frame, Fact::Step, Fact::Input].iter().any(|fact| deps::mem(*fact, dependencies));
    let core = &value.core;
    let lowered = &core.doc.workspace.lowered;
    value.baked.is_some()
        || !value.baked_views.is_empty()
        || moves(cook::dependencies(&core.cook))
        || moves(lowering::scene_reads(&core.doc))
        || !lowered.states.is_empty()
        || lowered.plan.nodes.iter().any(|n| n.args.iter().any(|(_, v)| flow_eval::frame_dependent(v)))
}

pub fn may_rest(value: &t, frame_: &frame::t) -> bool {
    let core = &value.core;
    frame_.input.events.is_empty()
        && value.world_drag.is_none()
        && value.pending_render.is_none()
        && core.carry.is_none()
        && core.queued.is_empty()
        && core.hud.is_none()
        && (core.notice.is_none() || frame_.input.t - core.notice_at >= 3.)
        && core.traces.iter().all(|(_, t)| t.samples >= t.cap)
        && core.ui.borrow().at_rest()
        && cook::at_rest(&core.cook)
        && (timeline::mode(&editor_list::timeline(core)) != timeline::Mode::Playing || !reads_clock(value))
}

pub fn settle(before_rest: &Option<rest>, value: &mut t, frame_: &frame::t) {
    if !may_rest(&value, frame_) {
        value.rest = None;
        return;
    }
    let picture = compose_scene(&value, frame_);
    let still = match before_rest {
        Some(rest) if same_input(&rest.shown, frame_) && scene::Layer::same(&rest.picture, &picture) => rest.still + 1,
        _ => 0,
    };
    value.rest = Some(rest { picture, still, shown: frame_.clone() });
}

pub fn rests(value: &t, frame_: &frame::t) -> Option<rest> {
    match &value.rest {
        Some(rest_) if rest_.still >= rest_frames && frame_.input.events.is_empty() && same_input(&rest_.shown, frame_) && frame_.input.t >= rest_.shown.input.t && frame_.input.t - rest_.shown.input.t < rest_seconds => Some(rest_.clone()),
        _ => None,
    }
}

/// the poll a full frame would make; a change leaves it unmade, for that frame to make and act on
pub fn source_at_rest(value: &mut t, now: f64) -> bool {
    match value.source.clone() {
        None => true,
        Some(previous) => match source_file::poll(now, &previous) {
            (file, None) if file.error == previous.error => {
                if file != previous { value.source = Some(file); }
                true
            }
            _ => false,
        },
    }
}

pub fn update(value: &mut t, frame_: &frame::t) {
    if rests(value, frame_).is_none() || !source_at_rest(value, frame_.input.t) {
        let before = value.rest.clone();
        update_with(value, frame_, &|_ui| Rc::new(()));
        settle(&before, value, frame_);
    } else {
        let before_error = value.state_error.clone();
        autosave(value, false, frame_.input.t);
        // An unread clock still runs, for whatever comes to read it.
        let timeline = editor_list::timeline(&value.core);
        let (moved, _) = timeline::update(&timeline, frame_);
        if moved != timeline { Rc::make_mut(&mut value.core).timeline = moved; }
        if value.state_error != before_error { value.rest = None; }
    }
}

/// One frame boundary of the budget (the retreat's order is `Mem::retreat`'s). Nothing an editor's
/// renderer keeps counts as a plan nothing shows: a tracer no drawn view wants is destroyed when the
/// frame is rendered, so past the caches only undo beyond 16 entries is left. The document on screen
/// and the last good frame stay; the error says so until usage is back.
pub fn memory_boundary(value: &mut t) {
    // once per crossing: a window with no eyes on its status strip still says it
    fn said(current: &Option<String>, message: String) -> Option<String> {
        match current {
            Some(_) => current.clone(),
            None => {
                eprintln!("rays: {message}");
                Some(message)
            }
        }
    }
    let gpu_t = host_gpu(&value);
    let device_bytes = workspace_gpu::device_bytes(&gpu_t);
    if !value.memory_known {
        mem::pressure_start();
        // one pool: the working set bounds the footprint. A discrete device keeps its own limit (its
        // working set), and the footprint is host RAM's alone.
        if let Some((_, working_set, unified)) = workspace_gpu::device_limits(&gpu_t) {
            if unified {
                mem::set_budget_from(mem::Source::Machine, mem::machine_budget(working_set));
            } else {
                mem::set_budget_from(mem::Source::Machine, mem::machine_budget(0));
                mem::set_device_budget_from(mem::Source::Machine, working_set);
            }
        }
        // what the device holds at the first boundary beyond any owner: the driver's own, and the
        // first frame's pipelines
        let driver = workspace_gpu::device_limits(&gpu_t).map(|(bytes, _, _)| bytes).unwrap_or(0);
        mem::charge(&mem::plan_in(mem::Pool::Device, "gpu.driver"), driver - mem::attributed_device());
        value.memory_known = true;
    }
    // Immutable ownership roots only change on edits/reloads, never on an unchanged scene's frame.
    document_memory(&value.core.doc);
    let options = |device: i64| mem::RetreatOptions { device: Some(Arc::new(move || device)), usage: None, later: Vec::new() };
    let mut verdict = mem::retreat_with(options(device_bytes));
    let mut core_changed = false;
    if matches!(verdict, mem::Verdict::Over(_)) && history::depth(&value.core.history) > 16 {
        Rc::make_mut(&mut value.core).history = history::shed(16, value.core.history.clone());
        core_changed = true;
        verdict = mem::retreat_with(options(device_bytes));
    }
    match verdict {
        mem::Verdict::Over(overdraft) => {
            let message = rays_diagnostic::to_string(&rays_diagnostic::error("E_MEMORY".to_string(), mem::describe(&overdraft), None, None, None, Vec::new(), Vec::new()));
            value.memory_error = said(&value.memory_error, message);
        }
        mem::Verdict::Within => {
            if cook::out_of_memory(&value.core.cook) && mem::retry_worthwhile() {
                Rc::make_mut(&mut value.core).cook = std::rc::Rc::new(cook::force(&value.core.cook));
                core_changed = true;
            }
            // a refused cook stays refused, though the process is back under: the last good frame shows
            value.memory_error = if cook::out_of_memory(&value.core.cook) { said(&value.memory_error, value.core.cook.error.clone().unwrap_or_default()) } else { None };
        }
    }
    if core_changed {
        value.rest = None;
    }
}

pub fn after_present(value: &mut t, frame_: &frame::t) {
    observe_presented(value);
    let saved = workspace_host::save_pending(&mut value.host.borrow_mut(), &mut |path| rays::canvas::save_screen_png(path));
    if let Err(diagnostic) = saved {
        value.context_error = Some(rays_diagnostic::to_string(&diagnostic));
    }
    memory_boundary(value);
    match save_status(&V::save, &V::filename, value.pending_render.as_ref(), value.rendered.as_ref()) {
        Some(status) => {
            value.render_status = Some(status);
            value.pending_render = None;
            refresh_hidden(value, frame_)
        }
        None => {},
    }
}

pub fn scene_of(value: &t, frame_: &frame::t) -> Nodes {
    match &value.rest {
        Some(rest) if same_input(&rest.shown, frame_) => rest.picture.clone(),
        _ => compose_scene(value, frame_),
    }
}

/// A crash report's editor part: the document as a loadable preset and a summary of the view state
/// and undo history.
pub fn crash_dump(value: &t, directory: &str) -> Result<(), String> {
    let core = &value.core;
    let write = |name: &str, text: &str| -> Result<(), String> { editor_core::store::write_text(&format!("{directory}/{name}"), text) };
    // document.txt is the text of any document; document.rays is the loadable preset, written for
    // workspace documents
    write("document.txt", &editor_document::document::dump(&core.doc))?;
    editor_document::preset::save(directory, "document", &core.doc, &V::section(&value.camera, &value.extra))?;
    let hist = &core.history;
    let mut editor = String::new();
    let selected = match editor_list::selected_node(core) {
        Some(node) => format!("{} (#{}, {})", node.label, node.id, node.kind),
        None => "none".to_string(),
    };
    let scope_selected = {
        let paths = pxui_graph::scope_state::selected(m::pane_view(core));
        if paths.is_empty() { "-".to_string() } else { paths.iter().map(|p| p.join("/")).collect::<Vec<_>>().join(", ") }
    };
    let or_dash = |o: &Option<String>| o.clone().unwrap_or_else(|| "-".to_string());
    editor.push_str(&format!(
        "level: {}\nprojection: {}\npane graph: {}\ngraph panels: {}\nwindows: {}\npanels: {}\nroute: {}\ntext: {}\nmap view: {}\ngraph pan: {}\nguide: {}\nkey hud: {}\nselected: {}\nscope selected: {}\nfocus: {}\nprompt: {}\nundo: {} ({} entries)\nredo: {}\ncook: {}\nedit error: {}\nrenderer: {}\nlive scene: {}\nautosave: {}\nload document.rays with / b (workspace documents only) after copying it to {}\n",
        editor_status::level_name(core),
        match editor_panes::projection_of(core) {
            m::projection::List_view => "list",
            m::projection::Graph_view => "graph",
            m::projection::Text_view => "text",
        },
        or_dash(&editor_panes::graph_name(core)),
        editor_panes::graph_panels(core),
        editor_panes::windows(core),
        editor_panes::panels_line(core),
        editor_panes::route(core),
        text_pane::summary(&core.text),
        core.map_view,
        pxui_graph::scope_frame::Inspect::grabbed(m::pane_view(core)),
        core.guide,
        core.hud.as_ref().map(|(h, _)| h.clone()).unwrap_or_else(|| "-".to_string()),
        selected,
        scope_selected,
        editor_commands::pane_name(&core.focus),
        editor_status::prompt_name(core),
        history::label(hist),
        history::depth(hist),
        history::redo_label(hist).unwrap_or("-"),
        editor_status::status_text(core, false),
        or_dash(&core.edit_error),
        or_dash(&V::render_status(&value.extra)),
        or_dash(&value.context_error),
        or_dash(&value.state_error),
        core.presets,
    ));
    editor.push_str(&format!("last edit: {:.3} ms\nphases: {}\n", core.edit_phases.total * 1000., flow_syntax::phase_timer::summary(&core.edit_phases)));
    write("editor.txt", &editor)?;
    // no collection: a crashing process should not start one
    let device = workspace_gpu::device_limits(&host_gpu(value)).map(|(bytes, _, _)| bytes).unwrap_or(0);
    write("memory.txt", &mem::report_with(mem::ReportOptions { walk: false, device, budget: None }).to_string())?;
    Ok(())
}

pub fn close(value: &t) {
    for name in ["editor.document.syntax", "editor.document.terms"] {
        mem::root(&mem::document(name), Arc::new(|_| {}));
    }
    MEMORY_DOCUMENT.with(|last| *last.borrow_mut() = std::sync::Weak::new());
    let mut value = value.clone();
    let checked = value.state_checked;
    autosave(&mut value, true, checked);
    if let Some(error) = &value.state_error {
        eprintln!("{error}");
    }
    if let Some((_, image_)) = &value.map {
        let _ = image::destroy(image_);
    }
    workspace_host::close(&mut value.host.borrow_mut());
    V::close(&value.extra);
    editor_host::close(&value.core);
}

/// OCaml's `run`: the editor as a sketch program.
#[allow(clippy::too_many_arguments)]
pub fn run(
    options: create_options,
    max_frames: Option<i64>,
    crash_dir: Option<String>,
    profile: Option<bool>,
    config: sketch::config,
    workspace: workspace_doc::T,
    source: Option<source_file::t>,
    prepare: cook::Prepare_fn,
    draw: Draw_fn,
) -> Result<(), sketch::error> {
    let create_options { inputs, name, presets, factories, gpu_owner, settings, lights, camera, background, seed, grain, domains, max_entries, max_payload_bytes, await_: _, carry_budget, preferences, ui_text_size, platform, ui_font, opened } = options;
    let name = name.unwrap_or_else(|| workspace_doc::name(&workspace));
    // `opened`, the caller's document of this workspace, is handed to the editor once and not kept by
    // the loop: after an edit or a reload only the history decides how long it lives.
    let handed = Rc::new(RefCell::new(opened));
    let deterministic = matches!(config.clock, sketch::clock::Fixed(_));
    let init: Box<dyn Fn(&frame::t) -> sketch::model> = {
        let handed = handed.clone();
        let (workspace, source, prepare, draw) = (workspace.clone(), source.clone(), prepare.clone(), draw.clone());
        let slots = RefCell::new(Some((inputs, presets, factories, gpu_owner, settings, lights, camera, background, preferences, platform, ui_font)));
        Box::new(move |_frame| {
            let opened = handed.borrow_mut().take();
            let (inputs, presets, factories, gpu_owner, settings, lights, camera, background, preferences, platform, ui_font) = slots.borrow_mut().take().expect("init runs once");
            let value = create(
                create_options {
                    inputs,
                    name: Some(name.clone()),
                    presets,
                    factories,
                    gpu_owner,
                    settings,
                    lights,
                    camera,
                    background,
                    seed,
                    grain,
                    domains,
                    max_entries,
                    max_payload_bytes,
                    await_: max_frames.map(|_| true),
                    carry_budget,
                    preferences,
                    ui_text_size,
                    platform,
                    ui_font,
                    opened,
                },
                &workspace,
                source.clone(),
                prepare.clone(),
                draw.clone(),
            )
            .expect("the editor opens");
            value.host.borrow_mut().deterministic = deterministic;
            Box::new(value)
        })
    };
    let program = sketch::program {
        init,
        update: Box::new(move |model, frame_| {
            let value = model.downcast_mut::<t>().expect("editor model");
            update(value, frame_);
            // A cursor the platform refuses is said in the strip; the editor runs on.
            if let Err(message) = set_ui_cursor(&value.core.ui.borrow(), V::ui_visible(&value.control)) {
                value.render_status = Some(format!("Cursor: {message}"));
            }
            None
        }),
        view: Box::new(move |model, frame_| scene_of(model.downcast_ref::<t>().expect("editor model"), frame_)),
        after_present: Some(Box::new(move |model, frame_| {
            after_present(model.downcast_mut::<t>().expect("editor model"), frame_);
            None
        })),
        crash_dump: Some(Box::new(|model, directory| {
            if let Err(message) = crash_dump(model.downcast_ref::<t>().expect("editor model"), directory) {
                panic!("{message}");
            }
        })),
        idle: Some(Box::new(|model| {
            let value = model.downcast_ref::<t>().expect("editor model");
            workspace_gpu::pump(&host_gpu(value), true);
            cook::busy(&value.core.cook)
        })),
        on_stop: Some(Box::new(|model| close(model.downcast_ref::<t>().expect("editor model")))),
    };
    sketch::run_state(Some(config), max_frames, crash_dir, profile, program).map(|_| ())
}

#[allow(dead_code)]
fn _unused(_: &Level, _: &EValue) {}
