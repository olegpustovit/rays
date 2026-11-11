//! Viewport camera controls, panel requests, guides and handles return frame intents. Loop rest bookkeeping tracks changes to cloned extras; inspector results remain sketch-owned Rc values.

use std::any::Any;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;

use editor_core::panels::{self, Panel};
use editor_core::router;
use editor_document::document::{self, ViewRoot};
use editor_document::kind_graph as kg;
use editor_document::lowering;
use editor_document::objects::{self, camera as ocamera, root};
use editor_document::settings;
use flow_syntax::context;
use flow_syntax::syntax::{Node as SNode, Syntax};
use input::Event;
use param::{FieldView, KindView, Value};
use pxui::ui::{BoxOpts, Size, Ui, ValueFieldOpts, CLICKABLE};
use rays_shell::layout::Bounds;
use rays::camera as rcam;
use rays::easy_camera;
use rays::frame;
use rays::light;
use rays::path as rpath;
use rays::scene::{self, Node as scene_node};
use rays::scene3::{self as d3, walk};
use rays::sketch;
use rays::world;
use rays_math::{color, mat4, vec3};

use crate::cook;
use crate::editor_commands::Action;
use crate::editor_host;
use crate::editor_inspect;
use crate::editor_list;
use crate::editor_model::{Picked, EditorModel as Model};
use crate::editor_renderer as renderer;

type Nodes = Vec<scene_node>;

fn one(r: Result<scene_node, String>) -> Nodes {
    r.ok().into_iter().collect()
}

/// The camera control: the Camera and Render sections of the inspector.
pub mod cc {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Visibility {
        pub prefix: String,
        pub ui_visible: bool,
        pub filename: String,
        pub open_camera: Option<bool>,
    }

    pub type Control = Visibility;

    #[derive(Clone, Debug, PartialEq)]
    pub struct RenderRequest {
        pub filename: String,
    }

    pub fn make(prefix: &str) -> Visibility {
        assert!(!prefix.is_empty(), "Pxui camera control: prefix is empty");
        Visibility { prefix: prefix.to_string(), ui_visible: true, filename: "_out/rays-render.png".to_string(), open_camera: None }
    }

    pub fn create() -> Control {
        make("camera")
    }

    pub fn toggle_ui(control: &Control) -> Control {
        Control { ui_visible: !control.ui_visible, ..control.clone() }
    }

    /// Show the UI and toggle the Camera section, or open it if the UI was hidden.
    pub fn open_camera(control: &Control) -> Control {
        Control { ui_visible: true, open_camera: Some(!control.ui_visible), ..control.clone() }
    }

    pub fn number(ui: &mut Ui, key: &str, label: &str, low: f64, high: f64, value: f64) -> f64 {
        let (row, x, y, w) = ui.inspector_row(None, None, key, label);
        let mut result = value;
        ui.within(&row, &mut |ui| {
            let label_x = ui.inspector_label_x();
            let width = if y > 20. { w } else { (x - label_x - 8.).max(1.) };
            let label_hit = ui.box_("label-edit", BoxOpts::default().flags(CLICKABLE).at((label_x, 2.)).w(Size::Px(width)).h(Size::Px(20.)));
            let edit = ui.signal(&label_hit).double_clicked;
            let shown = cfmt::g(value, 6);
            let opts = ValueFieldOpts {
                fraction: Some(((value - low) / (high - low)) as f32),
                slide: Some(Rc::new(move |fraction: f32| cfmt::g(low + fraction as f64 * (high - low), 6))),
                edit,
                ..Default::default()
            };
            let (text, _) = ui.value_field((x, y), w, 20., &opts, &|text: &str| text.parse::<f64>().is_ok_and(f64::is_finite), &format!("{key}-value"), &shown);
            // the text shows 6 digits: an unedited field leaves the value as it is
            result = if text == shown {
                value
            } else {
                match text.parse::<f64>() {
                    Ok(n) if n.is_finite() => n,
                    _ => value,
                }
            };
        });
        result
    }

    pub fn text(ui: &mut Ui, key: &str, label: &str, value: &str) -> String {
        let (row, x, y, w) = ui.inspector_row(None, None, key, label);
        let mut result = value.to_string();
        ui.within(&row, &mut |ui| {
            let opts = ValueFieldOpts { left: true, ..Default::default() };
            result = ui.value_field((x, y), w, 20., &opts, &|_: &str| true, &format!("{key}-value"), value).0;
        });
        result
    }

    pub fn render_section(control: &Control, ui: &mut Ui) -> (Control, Vec<String>) {
        let mut out = (control.filename.clone(), false);
        ui.inspector_section("Render", false, None, "Render", &mut |ui| {
            let filename = text(ui, "Output", "Output", &control.filename);
            out = (filename, ui.inspector_button("Save", "Render / save PNG"));
        });
        let (filename, save) = out;
        (Control { filename: filename.clone(), ..control.clone() }, if save { vec![filename] } else { Vec::new() })
    }

    /// `open_camera` opens the section explicitly while hidden, otherwise it toggles.
    pub fn camera_section(control: &Control, ui: &mut Ui, build: &mut dyn FnMut(&mut Ui)) -> bool {
        let set_expanded = match control.open_camera {
            None => None,
            Some(true) => Some(true),
            Some(false) => Some(!ui.expanded("Camera").unwrap_or(false)),
        };
        ui.inspector_section("Camera", false, set_expanded, "Camera", build)
    }

    pub fn widgets(control: &Control, ui: &mut Ui, camera: &easy_camera::EasyCamera) -> (Control, easy_camera::EasyCamera, Vec<RenderRequest>) {
        let mut out_camera = camera.clone();
        let mut control_out = control.clone();
        let mut saves: Vec<String> = Vec::new();
        ui.scope(&control.prefix, &mut |ui| {
            let mut result: Option<easy_camera::EasyCamera> = None;
            camera_section(control, ui, &mut |ui| {
                let degrees = easy_camera::fov_y(camera) * 180. / std::f64::consts::PI;
                let edited = number(ui, "FOV", "FOV", 15., 120., degrees);
                let fov = if edited == degrees { easy_camera::fov_y(camera) } else { edited * std::f64::consts::PI / 180. };
                let distance = number(ui, "Distance", "Distance", 0.1, 100., easy_camera::distance(camera));
                let near = number(ui, "Near clip", "Near clip", 0.01, 10., easy_camera::near(camera));
                let far = number(ui, "Far clip", "Far clip", 10., 5000., easy_camera::far(camera));
                let inertia = ui.inspector_toggle("Inertia", "Inertia", easy_camera::inertia(camera));
                let reset = ui.inspector_button("Reset camera", "Reset camera");
                let current = easy_camera::fov_y(camera);
                let fov = if fov > 0. && fov < std::f64::consts::PI { fov } else { current };
                let distance = if distance > 0. { distance } else { easy_camera::distance(camera) };
                let near = if near > 0. { near } else { easy_camera::near(camera) };
                let far = far.max(near + 0.01);
                let built = easy_camera::with_fov_y(fov, camera)
                    .and_then(|c| easy_camera::with_distance(distance, &c))
                    .and_then(|c| easy_camera::with_clip(near, far, &c))
                    .map(|c| easy_camera::with_inertia(inertia, &c))
                    .unwrap_or_else(|_| camera.clone());
                result = Some(if reset { easy_camera::reset(&built) } else { built });
            });
            out_camera = result.unwrap_or_else(|| camera.clone());
            let (c, s) = render_section(&Control { open_camera: None, ..control.clone() }, ui);
            control_out = c;
            saves = s;
        });
        (control_out, out_camera, saves.into_iter().map(|filename| RenderRequest { filename }).collect())
    }

    pub fn navigate(control_area: Option<Bounds>, camera: &easy_camera::EasyCamera, frame_: &frame::Frame) -> easy_camera::EasyCamera {
        let area = control_area.unwrap_or((0, 0, frame_.input.size.0, frame_.input.size.1));
        easy_camera::with_control_area(Some(area), camera).and_then(|c| easy_camera::update(&c, frame_)).unwrap_or_else(|_| camera.clone())
    }

    pub fn ui_visible(control: &Control) -> bool {
        control.ui_visible
    }

    pub fn save(request: &RenderRequest) -> Result<(), String> {
        rays::canvas::save_screen_png(&request.filename)
    }
}

pub type Camera = easy_camera::EasyCamera;
pub type Control = cc::Control;
pub type Rendered = d3::Scene3;
pub type Request = cc::RenderRequest;
pub type View = rcam::Camera;

/// The view's handle tool: W translate, E rotate, R scale, Escape none.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tool {
    Move,
    Turn,
    Grow,
    NoTool,
}

/// What the view draws over the render while the UI shows.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Show {
    pub cameras: bool,
    pub axes: bool,
    pub handles: bool,
}

/// Look-through, the fly speed while the pointer is captured, the active camera node's view and
/// the free viewport's view with its lens, and the guides.
#[derive(Clone)]
pub struct Extra {
    /// the viewports that look through their render camera, by key
    pub looking: Vec<String>,
    /// the focused viewport: the one the toggle and the navigation mean
    pub key: String,
    /// the `:look_through true` viewports last followed
    pub started: Vec<String>,
    pub fly: Option<f64>,
    pub relative_grab: bool,
    /// the ACTIVE camera's view: a viewport with no root of its own
    pub document_camera: rcam::Camera,
    /// the viewports with a root of their own
    pub view_roots: Vec<(String, ViewRoot)>,
    pub free_view: rcam::Camera,
    /// the orbit camera `free_view` shows
    pub viewing: easy_camera::EasyCamera,
    /// the camera whose lens `free_view` carries
    pub free_camera: rcam::Camera,
    /// the document's root settings, the renderer as shown
    pub root: root::Parameters,
    root_rows: Rc<RefCell<(Option<root::Parameters>, Vec<rays_shell::inspector::FlowRow>)>>,
    /// the Render section's edit
    pub root_request: Option<root::Parameters>,
    /// the ACTIVE camera follows the viewport
    pub following: Option<bool>,
    /// the Viewport panel's toggle, applied on the next update
    pub follow_request: Option<bool>,
    /// the view the viewport last wrote to the camera node
    pub written: Option<rcam::Camera>,
    /// a held drag moved the following camera: its node is written on release
    pub follow_pending: bool,
    pub show: Show,
    pub tool: Tool,
    pub renderer: renderer::State,
    pub renderer_request: Option<renderer::Mode>,
}

pub fn default_camera() -> Camera {
    easy_camera::create(Some(vec3::zero()), Some(7.), None, None, None, None).expect("default camera")
}
pub fn create_control() -> Control {
    cc::create()
}
pub fn ui_visible(c: &Control) -> bool {
    cc::ui_visible(c)
}
pub fn toggle_ui(c: &Control) -> Control {
    cc::toggle_ui(c)
}
pub fn open_camera(c: &Control) -> Control {
    cc::open_camera(c)
}
pub fn set_relative(enabled: bool) {
    let _ = sketch::set_relative_mouse(enabled);
}

/// The one way out of a pointer grab: the orbit drag's relative mouse and the fly mode end
/// together, on every path that stops navigating.
pub fn release(extra_: &Extra) -> Extra {
    if extra_.relative_grab || extra_.fly.is_some() {
        set_relative(false);
        Extra { relative_grab: false, fly: None, ..extra_.clone() }
    } else {
        extra_.clone()
    }
}

// ---- camera objects: scene nodes with operation "camera" (Objects.Camera).

pub fn camera_ids(scene: &kg::T) -> Vec<i64> {
    objects::ids("camera", scene)
}

pub fn follows(node: &kg::Node) -> bool {
    ocamera::of_node(node).map(|(_, f)| f).unwrap_or(false)
}

pub fn node_camera(node: &kg::Node) -> Option<rcam::Camera> {
    ocamera::of_node(node).map(|(c, _)| c)
}

pub fn view_parameters(easy: &easy_camera::EasyCamera) -> Vec<(String, Value)> {
    let camera = easy_camera::camera(easy).expect("easy camera");
    let (eye, target) = (rcam::position(&camera), rcam::target(&camera));
    ocamera::to_values(eye, target, easy_camera::fov_y(easy))
}

/// The node's camera once `easy`'s view is written to it: eye, target and field of view from the
/// viewport, the rest from the node (`ocamera::of_node` over `view_parameters`).
fn following_view(node: &rcam::Camera, easy: &easy_camera::EasyCamera) -> Option<rcam::Camera> {
    let view = easy_camera::camera(easy).ok()?;
    let (near, far) = match rcam::projection(node) {
        rcam::Projection::Perspective { near, far, .. } | rcam::Projection::Orthographic { near, far, .. } => (near, far),
    };
    let fov = easy_camera::fov_y(easy) * 180. / std::f64::consts::PI;
    let camera = rcam::perspective(Some(fov * std::f64::consts::PI / 180.), Some(near), Some(far), rcam::position(&view), rcam::target(&view)).ok()?;
    rcam::with_lens(rcam::lens(node), &rcam::with_up(rcam::up(node), &camera).ok()?).ok()
}

pub fn same_view(a: &rcam::Camera, b: &rcam::Camera) -> bool {
    vec3::nearly_equal(rcam::position(a), rcam::position(b), 1e-6)
        && vec3::nearly_equal(rcam::target(a), rcam::target(b), 1e-6)
        && match (rcam::projection(a), rcam::projection(b)) {
            (rcam::Projection::Perspective { fov_y: a, .. }, rcam::Projection::Perspective { fov_y: b, .. }) => (a - b).abs() < 1e-6,
            _ => false,
        }
}

pub fn active_node(core: &Model) -> Option<kg::Node> {
    core.doc.active_camera.and_then(|id| kg::find(&editor_inspect::scene(core), id))
}

/// A default camera, following the viewport.
pub fn add_default_camera(scene: &kg::T, easy: &easy_camera::EasyCamera) -> Option<kg::T> {
    let mut changes = vec![("follow_viewport".to_string(), Value::Bool(true))];
    changes.extend(ocamera::lens_values(&rcam::pinhole()));
    changes.extend(view_parameters(easy));
    let node = kg::instance(None, "camera1", &ocamera::kind());
    kg::apply(&node, &changes).ok().map(|(node, _)| kg::add(scene, node))
}

pub fn seed_scene(easy: &easy_camera::EasyCamera, scene: kg::T) -> kg::T {
    if !camera_ids(&scene).is_empty() { scene } else { add_default_camera(&scene, easy).unwrap_or(scene) }
}

/// One ACTIVE camera whenever any exists; a nonempty scene of a workspace with no scene graph
/// losing the last camera re-adds the default within the same undo entry. An intentionally
/// empty scene stays empty.
pub fn sync_cameras(mode: editor_host::SceneMode, core: &mut Model, easy: &easy_camera::EasyCamera) {
    let scene = editor_inspect::scene(core);
    // A scene graph is authoritative: the host only supplies a camera for host-owned scenes.
    if camera_ids(&scene).is_empty() && !scene.is_empty() && lowering::graph_of(&core.doc.workspace.ws, context::scene()).is_none() {
        if let Some(scene) = add_default_camera(&scene, easy) {
            editor_host::scene_edit(core, mode, None, scene);
        }
    }
    let view = editor_inspect::View::Primary;
    let ids: Vec<i64> = camera_ids(&editor_inspect::scene(core)).into_iter().filter(|id| editor_inspect::view_wants(core, &view, *id)).collect();
    let active = match core.doc.active_camera {
        Some(id) if ids.contains(&id) => Some(id),
        _ => ids.first().copied(),
    };
    if active != core.doc.active_camera {
        let scene = editor_inspect::scene(core);
        editor_host::scene_edit(core, editor_host::SceneMode::Amend, Some(active), scene.clone());
    }
}

/// How the viewport `key` renders: through which camera and as which root says. A viewport over
/// its own scene instance reads that instance's root; any other renders as the document.
pub struct Look {
    pub through: rcam::Camera,
    pub settings: root::Parameters,
}

pub fn look(extra_: &Extra, key: &str) -> Look {
    match extra_.view_roots.iter().find(|(k, _)| k == key) {
        Some((_, ViewRoot { params, camera })) => Look { through: camera.clone().unwrap_or_else(|| extra_.document_camera.clone()), settings: params.clone() },
        None => Look { through: extra_.document_camera.clone(), settings: extra_.root.clone() },
    }
}

pub fn render_camera_of(core: &Model, easy: &easy_camera::EasyCamera) -> rcam::Camera {
    match active_node(core) {
        Some(node) => node_camera(&node).unwrap_or_else(|| easy_camera::camera(easy).expect("easy camera")),
        None => easy_camera::camera(easy).expect("easy camera"),
    }
}

/// The viewport camera carrying the ACTIVE camera's lens, focused on the orbit target unless that
/// camera follows the viewport.
pub fn free_view_of(render_camera: &rcam::Camera, previous: Option<&rcam::Camera>, camera: &easy_camera::EasyCamera) -> rcam::Camera {
    let view = easy_camera::camera(camera).expect("easy camera");
    let lens = rcam::lens(render_camera);
    let view = rcam::with_lens(if same_view(render_camera, &view) { lens } else { rcam::Lens { focus_distance: None, ..lens } }, &view).unwrap_or(view);
    match previous {
        Some(previous) if *previous == view => previous.clone(),
        _ => view,
    }
}

// the renderer the document's root names, when its text has a root
pub fn of_root(r: root::Renderer) -> renderer::Mode {
    match r {
        root::Renderer::Raster => renderer::Mode::Raster,
        root::Renderer::Wireframe => renderer::Mode::Wireframe,
        root::Renderer::PathTraced => renderer::Mode::PathTraced,
    }
}

pub fn to_root(m: renderer::Mode) -> root::Renderer {
    match m {
        renderer::Mode::Raster => root::Renderer::Raster,
        renderer::Mode::Wireframe => root::Renderer::Wireframe,
        renderer::Mode::PathTraced => root::Renderer::PathTraced,
    }
}

pub fn root_renderer(core: &Model) -> Option<renderer::Mode> {
    if core.doc.homes.root.is_none() { None } else { Some(of_root(core.doc.root.renderer)) }
}

pub fn renderer_setting(core: &Model) -> Option<renderer::Mode> {
    settings::fields(&editor_list::settings(core)).into_iter().find_map(|f: FieldView| match (f.name.as_str(), &f.current) {
        ("renderer", Value::Choice(label)) => renderer::of_label(label),
        _ => None,
    })
}

pub fn looks(extra_: &Extra, key: &str) -> bool {
    extra_.looking.iter().any(|k| k == key)
}
pub fn look_through(extra_: &Extra) -> bool {
    looks(extra_, &extra_.key)
}

pub fn set_look(extra_: &Extra, on: bool) -> Extra {
    let mut looking: Vec<String> = if on { vec![extra_.key.clone()] } else { Vec::new() };
    looking.extend(extra_.looking.iter().filter(|k| **k != extra_.key).cloned());
    Extra { looking, ..extra_.clone() }
}

pub fn init(mut core: Model, camera_: &easy_camera::EasyCamera) -> (Model, Extra) {
    sync_cameras(editor_host::SceneMode::Reset, &mut core, camera_);
    let render_camera = render_camera_of(&core, camera_);
    // the viewports whose `:look_through true` says so; the focus starts in the first viewport
    let looking = core.doc.shell.as_ref().map(|s| s.start.looking.clone()).unwrap_or_default();
    let key = match (&core.focus, core.doc.shell.as_ref().map(|s| panels::leaves(&s.tree))) {
        (Panel::View(key), _) => key.clone(),
        (_, Some(leaves)) => leaves
            .iter()
            .find_map(|(_, panel)| match panel {
                Panel::View(key) => Some(key.clone()),
                _ => None,
            })
            .unwrap_or_else(|| "main".to_string()),
        (_, None) => "main".to_string(),
    };
    let mode = root_renderer(&core).or_else(|| renderer_setting(&core)).unwrap_or(renderer::Mode::Raster);
    let custom = root_renderer(&core).is_none() && renderer_setting(&core).is_some();
    let extra_ = Extra {
        looking: looking.clone(),
        key,
        started: looking,
        fly: None,
        relative_grab: false,
        document_camera: render_camera.clone(),
        view_roots: core.doc.view_roots.clone(),
        free_view: free_view_of(&render_camera, None, camera_),
        viewing: camera_.clone(),
        free_camera: render_camera,
        root: core.doc.root.clone(),
        root_rows: Rc::new(RefCell::new((None, Vec::new()))),
        root_request: None,
        following: active_node(&core).map(|n| follows(&n)),
        follow_request: None,
        written: None,
        follow_pending: false,
        show: Show { cameras: true, axes: true, handles: true },
        tool: Tool::Move,
        renderer: renderer::State { mode, custom, ..renderer::empty() },
        renderer_request: None,
    };
    (core, extra_)
}

pub fn begin_frame<'a>(extra_: &Extra, frame_: &'a frame::Frame) -> (Extra, Cow<'a, frame::Frame>) {
    match extra_.fly {
        None => {
            let lost = frame_.input.events.iter().any(|e| matches!(e, Event::FocusLost));
            (if lost { release(extra_) } else { extra_.clone() }, Cow::Borrowed(frame_))
        }
        Some(_) => {
            let (ended, routed) = router::fly(frame_);
            (if ended { release(extra_) } else { extra_.clone() }, routed)
        }
    }
}

/// What a camera-panel callback hands back through `Panel_result`.
pub struct PanelOut {
    pub control: Control,
    pub camera: Camera,
    pub requests: Vec<Request>,
    pub extra: Extra,
    pub inspected: Rc<dyn Any>,
}

pub fn panel(ui: &mut Ui, control_: &Control, camera_: &Camera, extra_: &Extra, inspector: &dyn Fn(&mut Ui) -> Rc<dyn Any>) -> PanelOut {
    let mut next = extra_.clone();
    ui.inspector_section("Viewport", true, None, "Viewport", &mut |ui| {
        // the label column is 0.3 of the panel less 16: the rows say it short, their keys stay
        let through = ui.inspector_toggle("Look through render camera", "Look through", look_through(extra_));
        // The ACTIVE camera's own parameter, here so a look-through can be set up by orbiting: on,
        // the viewport drives the camera.
        let follow_request = match extra_.following {
            Some(following) => {
                let wanted = ui.inspector_toggle("Camera follows viewport", "Follows view", following);
                if wanted != following { Some(wanted) } else { None }
            }
            None => None,
        };
        let cameras = ui.inspector_toggle("Cameras and lights", "Cameras", extra_.show.cameras);
        let axes = ui.inspector_toggle("Axis gizmo", "Axis gizmo", extra_.show.axes);
        let handles = ui.inspector_toggle("Selected node handles", "Handles", extra_.show.handles);
        next = Extra { follow_request, show: Show { cameras, axes, handles }, ..set_look(extra_, through) };
    });
    let (control_out, camera_out, requests) = cc::widgets(control_, ui, camera_);
    // the scene's root: renderer, size and samples
    let mut root_ = extra_.root.clone();
    ui.inspector_section("renderer-section", true, None, "Scene root", &mut |ui| {
        use rays_shell::inspector::{self, FlowChange, FlowOpts, FlowRow};
        let mut cached = extra_.root_rows.borrow_mut();
        if cached.0.as_ref() != Some(&extra_.root) {
            let Ok(fields) = root::parameters_schema().view(&root::to_values(&extra_.root)) else { return; };
            cached.1 = fields.into_iter().map(|field| FlowRow { path: field.name.clone(), fields: vec![field], shown: false, locked: true, drive: None, live: None }).collect();
            cached.0 = Some(extra_.root.clone());
        }
        let changes: Vec<_> = inspector::flow_fields(ui, &FlowOpts::default(), None, &cached.1).into_iter().filter_map(|change| match change {
            FlowChange::Edited(name, value) => Some((name, value)),
            _ => None,
        }).collect();
        if !changes.is_empty() {
            if let Ok((values, _)) = root::parameters_schema().apply_all(&root::to_values(&extra_.root), &changes) {
                root_ = root::from_values(values);
            }
        }
    });
    if root_ != extra_.root {
        next.root_request = Some(root_);
    }
    let inspected = inspector(ui);
    PanelOut { control: control_out, camera: camera_out, requests, extra: next, inspected }
}

/// What `panel` shows of its inputs: the control, the camera's shown fields, the viewport switches
/// and the scene root. Compared frame to frame by `panel_revision`.
#[derive(Clone, PartialEq)]
pub struct PanelKey {
    control: Control,
    camera: [u64; 4],
    inertia: bool,
    look: bool,
    following: Option<bool>,
    show: Show,
    root: root::Parameters,
}

/// The camera panel's revision: `seen` keeps what the panel showed and its number, bumped when the
/// panel would show something else (compared in place; copied only on a change).
pub fn panel_revision(seen: &mut Option<(PanelKey, u64)>, control_: &Control, camera_: &Camera, extra_: &Extra) -> u64 {
    let camera = [easy_camera::fov_y(camera_).to_bits(), easy_camera::distance(camera_).to_bits(), easy_camera::near(camera_).to_bits(), easy_camera::far(camera_).to_bits()];
    let (inertia, look) = (easy_camera::inertia(camera_), look_through(extra_));
    let same = seen.as_ref().is_some_and(|(k, _)| {
        k.camera == camera && k.inertia == inertia && k.look == look && k.following == extra_.following && k.show == extra_.show && k.control == *control_ && k.root == extra_.root
    });
    if !same {
        let next = seen.as_ref().map_or(0, |(_, rev)| rev + 1);
        *seen = Some((PanelKey { control: control_.clone(), camera, inertia, look, following: extra_.following, show: extra_.show, root: extra_.root.clone() }, next));
    }
    seen.as_ref().map_or(0, |(_, rev)| *rev)
}

/// The values serialized by `section`, without constructing a syntax arena on an idle poll.
pub fn section_key(camera_: &Camera, extra_: &Extra) -> ([u64; 7], bool, renderer::Mode) {
    let camera = easy_camera::camera(camera_).expect("viewport form");
    let eye = rcam::position(&camera);
    let target = rcam::target(&camera);
    ([eye.x.to_bits(), eye.y.to_bits(), eye.z.to_bits(), target.x.to_bits(), target.y.to_bits(), target.z.to_bits(), easy_camera::fov_y(camera_).to_bits()], look_through(extra_), extra_.renderer_request.unwrap_or(extra_.renderer.mode))
}

pub fn section(camera_: &Camera, extra_: &Extra) -> Syntax {
    let form = crate::preset_viewport::encode(camera_, look_through(extra_)).expect("viewport form");
    match &form.node {
        SNode::Map(fields) => {
            let mut builder = flow_syntax::arena::SyntaxBuilder::new();
            let mut fields: Vec<_> = form.range(*fields).iter().map(|&id| builder.import(&form.child(id))).collect();
            fields.push(builder.with_node(&form, flow_syntax::arena::Draft::Kw("renderer".to_string())));
            fields.push(builder.with_node(&form, flow_syntax::arena::Draft::Str(renderer::label(extra_.renderer_request.unwrap_or(extra_.renderer.mode)).as_bytes().to_vec())));
            let id = builder.with_node(&form, flow_syntax::arena::Draft::Map(fields));
            builder.freeze().handle(id)
        }
        _ => form,
    }
}

pub fn restore(camera_: &Camera, extra_: &Extra, json: &Syntax) -> (Camera, Extra) {
    let (camera_, look) = crate::preset_viewport::decode(camera_.clone(), json);
    fn find(form: &Syntax, items: &[flow_syntax::arena::SyntaxId]) -> Option<renderer::Mode> {
        match items {
            [key, value, rest @ ..] => match (form.arena().row(*key).node, form.arena().row(*value).node) {
                (SNode::Kw(k), SNode::Str(label)) if form.name(k) == "renderer" => renderer::of_label(&String::from_utf8_lossy(form.bytes(label))),
                _ => find(form, rest),
            },
            _ => None,
        }
    }
    let mode = match &json.node {
        SNode::Map(fields) => find(json, json.range(*fields)),
        _ => None,
    };
    (camera_, Extra { renderer_request: mode, ..set_look(extra_, look) })
}

pub fn apply_action(camera_: &Camera, extra_: &Extra, a: &Action) -> (Extra, Option<String>) {
    match a {
        Action::Tool(0) => (Extra { tool: Tool::NoTool, ..extra_.clone() }, Some("Orbit (handles hidden)".to_string())),
        Action::Tool(1) => (Extra { tool: Tool::Move, ..extra_.clone() }, Some("Translate (W)".to_string())),
        Action::Tool(2) => (Extra { tool: Tool::Turn, ..extra_.clone() }, Some("Rotate (E)".to_string())),
        Action::Tool(3) => (Extra { tool: Tool::Grow, ..extra_.clone() }, Some("Scale (R)".to_string())),
        Action::LookThrough => (set_look(extra_, !look_through(extra_)), None),
        Action::LookThroughCamera => (set_look(extra_, true), None),
        // the header's tabs: the same request as the Scene root section's renderer row
        Action::RenderMode(index) => {
            let renderer = [root::Renderer::Raster, root::Renderer::Wireframe, root::Renderer::PathTraced][(*index).clamp(0, 2) as usize];
            (Extra { root_request: Some(root::Parameters { renderer, ..extra_.root.clone() }), ..extra_.clone() }, None)
        }
        Action::Fly if extra_.fly.is_none() => {
            set_relative(true);
            (Extra { fly: Some((easy_camera::distance(camera_) * 0.5).max(0.5)), ..extra_.clone() }, Some("Flying: WASD/QE move, Shift x4, wheel speed, Esc exits".to_string()))
        }
        _ => (extra_.clone(), None),
    }
}

pub fn on_doc(previous: &document::T, core: &mut Model, camera_: &Camera) {
    if !kg::same(&editor_inspect::scene(core), &document::scene_graph(previous)) || core.doc.active_camera != previous.active_camera {
        sync_cameras(editor_host::SceneMode::Amend, core, camera_);
    }
}

pub fn navigate(area: Bounds, grab: (bool, bool), _control: &Control, camera_: &Camera, extra_: &Extra, core: &Model, input: &frame::Frame) -> (Camera, Extra) {
    let (pressed, released) = grab;
    let active = active_node(core);
    // A fixed render camera owns the view while look-through is enabled.
    if look_through(extra_) && active.is_some() && !active.as_ref().is_some_and(follows) {
        return (camera_.clone(), release(extra_));
    }
    match extra_.fly {
        Some(speed) => {
            match easy_camera::fly(speed, camera_, input) {
                Ok((camera_, speed)) => (camera_, Extra { fly: Some(speed), ..extra_.clone() }),
                Err(_) => (camera_.clone(), extra_.clone()),
            }
        }
        None => {
            let released = released || input.input.events.iter().any(|e| matches!(e, Event::FocusLost));
            let grabbing = if pressed {
                true
            } else if released || input.input.buttons.is_empty() {
                false
            } else {
                extra_.relative_grab
            };
            if grabbing != extra_.relative_grab {
                set_relative(grabbing);
            }
            let camera_ = cc::navigate(Some(area), camera_, input);
            (camera_, Extra { relative_grab: grabbing, ..extra_.clone() })
        }
    }
}

pub fn frame_bounds(min: vec3::T, max: vec3::T, camera_: &Camera) -> Camera {
    easy_camera::frame_bounds(min, max, camera_)
}

/// Follow-viewport motion writes the camera node as view state (no undo entry); node edits and
/// undo pull the viewport back to the document. On a frame that stepped the history (`stepped`)
/// nothing is written: the document just restored is the one to show.
pub fn on_view(core: &mut Model, previous: &Camera, key: &str, camera_: Camera, extra_: Extra, stepped: bool, held: bool) -> (Camera, Extra) {
    // a change in the Render section: an authored root takes it; without one only the renderer is
    // the viewport's own choice, and any other setting writes the root
    let mut extra_ = match extra_.root_request.clone() {
        None => extra_,
        Some(root_) if root_renderer(&*core).is_none() && root::Parameters { renderer: core.doc.root.renderer, ..root_.clone() } == core.doc.root => {
            let mode = of_root(root_.renderer);
            Extra { root_request: None, renderer_request: Some(mode), ..extra_ }
        }
        Some(root_) => {
            editor_host::set_root(core, &root_);
            Extra { root_request: None, ..extra_ }
        },
    };
    let mode = match (extra_.renderer_request, renderer_setting(&*core)) {
        _ if root_renderer(&*core).is_some() => root_renderer(&*core).unwrap(),
        (Some(mode), Some(current)) if mode == current => mode,
        (Some(mode), Some(_)) => match settings::apply(&editor_list::settings(&*core), &[("renderer".to_string(), Value::Choice(renderer::label(mode).to_string()))]) {
            Ok((s, _)) => { editor_host::set_settings(core, &s); mode },
            Err(_) => extra_.renderer.mode,
        },
        (Some(mode), None) => mode,
        (None, Some(mode)) => mode,
        (None, None) => extra_.renderer.mode,
    };
    let custom = root_renderer(&*core).is_none() && renderer_setting(&*core).is_some();
    extra_ = Extra {
        renderer: renderer::State { mode, custom, ..extra_.renderer.clone() },
        renderer_request: None,
        root: root::Parameters { renderer: to_root(mode), ..core.doc.root.clone() },
        ..extra_
    };
    if let (Some(value), Some(node)) = (extra_.follow_request, active_node(core)) {
        editor_host::edit_node(core, document::Level::Scene, node.id, &[("follow_viewport".to_string(), Value::Bool(value))], "Follow viewport");
    }
    let mut written = extra_.written.clone();
    let mut pending = extra_.follow_pending && !stepped;
    let mut deferred = None;
    let active = active_node(&*core);
    let decoded = active.as_ref().and_then(ocamera::of_node);
    let camera_ = match (&active, &decoded) {
        (Some(node), Some((node_view, true))) => {
            let moved = !stepped && !same_view(&easy_camera::camera(previous).expect("camera"), &easy_camera::camera(&camera_).expect("camera"));
            if moved || pending || !same_view(node_view, &easy_camera::camera(&camera_).expect("camera")) {
                if (moved || pending) && held {
                    // a held drag writes the node once, on release: a document per frame was most
                    // of an orbit frame. Meanwhile renders look through what the write will say.
                    pending = true;
                    deferred = following_view(node_view, &camera_);
                    camera_
                } else if moved || pending {
                    pending = false;
                    match kg::set_fields(&editor_inspect::scene(&*core), node.id, &view_parameters(&camera_)) {
                        Ok((scene, _)) => {
                            written = Some(easy_camera::camera(&camera_).expect("camera"));
                            editor_host::scene_edit(core, editor_host::SceneMode::Amend, None, scene);
                            camera_
                        }
                        Err(_) => camera_,
                    }
                } else if written.as_ref().is_some_and(|w| same_view(w, node_view)) {
                    // the node shows what a viewport last wrote: another viewport took over, and
                    // keeps its own orbit
                    camera_
                } else {
                    written = Some(node_view.clone());
                    let lensed = match rcam::projection(node_view) {
                        rcam::Projection::Perspective { fov_y, .. } => easy_camera::with_fov_y(fov_y, &camera_).unwrap_or_else(|_| camera_.clone()),
                        _ => camera_.clone(),
                    };
                    easy_camera::of_view(rcam::position(node_view), rcam::target(node_view), &lensed)
                }
            } else {
                camera_
            }
        }
        _ => camera_,
    };
    // Reuse this frame's decode. A following viewport can write a new immutable node above; only
    // that replacement needs another schema/camera conversion.
    let current = active_node(&*core);
    let decoded = match (&current, &active) {
        (Some(c), Some(a)) if std::ptr::eq(c, a) => decoded,
        _ => current.as_ref().and_then(ocamera::of_node),
    };
    extra_ = Extra {
        document_camera: deferred.or_else(|| decoded.as_ref().map(|(c, _)| c.clone())).unwrap_or_else(|| easy_camera::camera(&camera_).expect("camera")),
        view_roots: core.doc.view_roots.clone(),
        ..extra_
    };
    // the focused viewport's free view carries the lens of its own camera
    let free_camera = look(&extra_, key).through;
    // a viewport whose `:look_through` changed in the text follows it; the others stay as they are
    let said = core.doc.shell.as_ref().map(|s| s.start.looking.clone()).unwrap_or_default();
    if said != extra_.started {
        let mut looking: Vec<String> = extra_.looking.iter().filter(|k| said.contains(k) || extra_.started.contains(k) == said.contains(k)).cloned().collect();
        looking.extend(said.iter().filter(|k| !extra_.started.contains(k) && !extra_.looking.contains(k)).cloned());
        extra_ = Extra { started: said, looking, ..extra_ };
    }
    let free_view = free_view_of(&free_camera, Some(&extra_.free_view), &camera_);
    let extra_ = Extra {
        key: key.to_string(),
        free_camera,
        free_view,
        viewing: camera_.clone(),
        following: current.as_ref().map(|_| decoded.as_ref().map(|(_, f)| *f).unwrap_or(false)),
        follow_request: None,
        written,
        follow_pending: pending,
        ..extra_
    };
    (camera_, extra_)
}

/// The view shows the render camera while looking through it and on the frame whose framebuffer a
/// PNG request captures; otherwise the viewport camera carries the ACTIVE camera's lens, focused on
/// the orbit target unless the node follows the viewport.
pub fn view_camera(camera_: &Camera, extra_: &Extra, key: &str, pending: bool) -> rcam::Camera {
    let Look { through, .. } = look(extra_, key);
    if looks(extra_, key) || pending { through } else { free_view_of(&through, None, camera_) }
}

pub fn pick_ray(viewport: Bounds, view_: &rcam::Camera, at: (f64, f64)) -> Option<(vec3::T, vec3::T)> {
    rcam::screen_ray(viewport, view_, at).ok().flatten()
}

/// A viewport shows the root's gate, not the whole pane, only while looking through the render
/// camera. Its own free view fills the pane whatever draws it.
pub fn film(extra_: &Extra, key: &str, (x, y, width, height): Bounds) -> Bounds {
    if !looks(extra_, key) {
        return (x, y, width, height);
    }
    let settings = look(extra_, key).settings;
    let aspect = settings.width as f64 / settings.height.max(1) as f64;
    let round = |v: f64| v.round() as i64;
    if width < 1000 && width > 160 && height > 160 {
        // the workspace sheet's frame (640 x 360 in 902 x 485): a side margin of 14.6 percent of the
        // body, 32 above, and clear of the gizmo (76 over the foot) and the readout row
        let side = round(0.1463 * width as f64);
        let (top, bottom) = (32, 93);
        let (avail_w, avail_h) = (width - 2 * side, (height - top - bottom).max(1));
        let w = avail_w.min(round(avail_h as f64 * aspect));
        let h = avail_h.min(round(avail_w as f64 / aspect));
        let (w, h) = (w.max(1), h.max(1));
        (x + ((width - w) / 2), y + top, w, h)
    } else {
        // viewport.html keeps the frame clear of the label row above it (40 points) and of the
        // readouts' row under it (36), and of 12 points at each side
        let top = if height > 160 { 40 } else { 0 };
        let bottom = if height > 160 { 36 } else { 0 };
        let side = if width > 160 { 12 } else { 0 };
        let (x, y, width, height) = (x + side, y + top, width - 2 * side, height - top - bottom);
        let w = width.min(round(height as f64 * aspect));
        let h = height.min(round(width as f64 / aspect));
        let (w, h) = (w.max(1), h.max(1));
        (x + ((width - w) / 2), y + ((height - h) / 2), w, h)
    }
}

/// Each viewport renders as its own root says. Looking through the render camera, the tracer's
/// film is the root's resolution in a step that fits the viewport's gate in drawable pixels; a free
/// view's film is the pane itself. `views` carry each viewport's rectangle (`film`).
pub fn render(extra_: &Extra, pixel_scale: (f64, f64), focus: Option<&str>, views: Vec<(String, Bounds, rcam::Camera, d3::Scene3)>) -> Extra {
    let (scale_x, scale_y) = pixel_scale;
    let views: Vec<renderer::View> = views
        .into_iter()
        .map(|(key, (_, _, width, height), camera_, scene_)| {
            let settings = look(extra_, &key).settings;
            let gate = ((width as f64 * scale_x).round() as i64, (height as f64 * scale_y).round() as i64);
            let (film_, step) = renderer::film(looks(extra_, &key), (settings.width, settings.height), gate);
            renderer::View {
                key,
                film: film_,
                step,
                camera: camera_,
                scene: scene_,
                cap: settings.max_spp,
                setting: renderer::Setting { mode: of_root(settings.renderer), bounces: settings.bounces, round_samples: settings.round_samples },
            }
        })
        .collect();
    Extra { renderer: renderer::update(&extra_.renderer, extra_.renderer.mode, extra_.renderer.custom, focus, &views), ..extra_.clone() }
}

/// A traced viewport's header: whether it looks through, and the renderer (0 solid, 1 wire, 2 traced).
pub fn header_tools(extra_: &Extra) -> Option<(bool, i64)> {
    Some((
        look_through(extra_),
        match extra_.renderer.mode {
            renderer::Mode::Raster => 0,
            renderer::Mode::Wireframe => 1,
            renderer::Mode::PathTraced => 2,
        },
    ))
}

/// A traced viewport's film in pixels, samples, cap and bounces.
pub fn trace(extra_: &Extra, key: &str) -> Option<((i64, i64), i64, i64, i64)> {
    let settings = look(extra_, key).settings;
    renderer::info(&extra_.renderer, key).map(|i| (i.size, i.samples, i.cap, settings.bounces))
}

pub fn render_status(extra_: &Extra) -> Option<String> {
    extra_.renderer.error.clone()
}

pub fn paint(extra_: &Extra, key: &str, viewport: Bounds, camera_: &rcam::Camera, rendered_: &d3::Scene3) -> Nodes {
    renderer::paint(&extra_.renderer, key, viewport, camera_, rendered_)
}

/// Objects under their world transforms. Light objects light the scene when there are any;
/// otherwise the first object's own lights do.
pub fn compose(scene_: &kg::T, world_: Option<&world::Baked>, pieces: Vec<(mat4::T, bool, d3::Scene3)>) -> d3::Scene3 {
    let lights = objects::lights(false, scene_);
    let base = pieces.first().map(|(_, _, first)| first.clone()).unwrap_or_else(d3::empty);
    let nodes: Vec<d3::Node> = pieces
        .iter()
        .map(|(matrix, ghost, scene3)| {
            let placed = d3::transform(matrix.clone(), d3::nodes(scene3));
            // ponytail: ghosts screen-blend without writing depth; a real override material would
            // need a Scene3 material-override node.
            if *ghost { d3::with_depth(d3::depth_state(Some(false)), vec![d3::with_blend(d3::Blend::Screen, vec![placed])]) } else { placed }
        })
        .collect();
    let lights = if !lights.is_empty() || !objects::ids("light", scene_).is_empty() { lights } else { walk::lights(&base) };
    let created = d3::create(Some(lights), Some(walk::ambient(&base)), Some(walk::separate_specular(&base)), Some(walk::samples(&base)), nodes).unwrap_or_else(|_| d3::empty());
    match world_ {
        Some(baked) => d3::with_world(baked.clone(), &created),
        None => created,
    }
}

// ---- guides and handles, in screen points over the view.

pub fn axes() -> [vec3::T; 3] {
    [vec3::create(1., 0., 0.), vec3::create(0., 1., 0.), vec3::create(0., 0., 1.)]
}

pub const AXIS_NAMES: [&str; 3] = ["x", "y", "z"];

fn hex(h: &str) -> color::T {
    color::hex_exn(h).expect("viewport colour")
}

// kit rev 3: the axes wear port colours (x bool, y int, z float), guides are ink-3 and the active
// camera and the lights the accent
pub fn axis_colors() -> [color::T; 3] {
    [hex("#b0435f"), hex("#3b7d4e"), hex("#285f77")]
}
pub fn guide_color() -> color::T {
    hex("#8f9492")
}
pub fn active_color() -> color::T {
    hex("#f0481f")
}

pub fn project(bounds: Bounds, camera_: &rcam::Camera, point: vec3::T) -> Option<(f64, f64)> {
    rcam::world_to_screen(bounds, camera_, point).ok().flatten().map(|s| (s.x, s.y))
}

pub fn screen_box(view_: &rcam::Camera, bounds: Bounds, world_: &mat4::T, box_: &cook::Bounds) -> Option<Bounds> {
    let mut corners: Vec<Option<(f64, f64)>> = Vec::new();
    cook::iter_corners(world_, box_, &mut |point| corners.push(project(bounds, view_, point)));
    if corners.iter().any(|c| c.is_none()) {
        return None;
    }
    let xs: Vec<f64> = corners.iter().filter_map(|c| c.map(|c| c.0)).collect();
    let ys: Vec<f64> = corners.iter().filter_map(|c| c.map(|c| c.1)).collect();
    let min = |v: &[f64]| v.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = |v: &[f64]| v.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let (x0, y0) = (min(&xs) as i64, min(&ys) as i64);
    Some((x0, y0, max(&xs) as i64 - x0, max(&ys) as i64 - y0))
}

pub fn line(width: i64, color_: color::T, (x0, y0): (f64, f64), (x1, y1): (f64, f64)) -> Nodes {
    one(scene::line((x0 as i64, y0 as i64), (x1 as i64, y1 as i64), Some(color_), Some(width)))
}

/// A line of a width in whole or half points: Scene lines have whole widths only, so the stroke is
/// a filled quad of the line's length.
pub fn thick_line(width: f64, color_: color::T, (x0, y0): (f64, f64), (x1, y1): (f64, f64)) -> Nodes {
    let (dx, dy) = (x1 - x0, y1 - y0);
    let length = dx.hypot(dy);
    if length < 1e-6 {
        one(scene::rect((x0 as i64, y0 as i64), 0, 0, None, None))
    } else {
        let (nx, ny) = (-dy / length * width / 2., dx / length * width / 2.);
        let p = |x: f64, y: f64| rpath::Point { x, y };
        one(scene::path(
            Some(color_),
            &rpath::of_commands(&[
                rpath::Command::MoveTo(p(x0 + nx, y0 + ny)),
                rpath::Command::LineTo(p(x1 + nx, y1 + ny)),
                rpath::Command::LineTo(p(x1 - nx, y1 - ny)),
                rpath::Command::LineTo(p(x0 - nx, y0 - ny)),
                rpath::Command::Close,
            ]),
        ))
    }
}

pub fn segment(bounds: Bounds, view_: &rcam::Camera, color_: color::T, a: vec3::T, b: vec3::T) -> Nodes {
    match (project(bounds, view_, a), project(bounds, view_, b)) {
        (Some(a), Some(b)) => line(1, color_, a, b),
        _ => Vec::new(),
    }
}

/// A pyramid half a unit deep with the view's aspect, and its aim.
pub fn frustum(bounds: Bounds, view_: &rcam::Camera, color_: color::T, camera_: &rcam::Camera) -> Nodes {
    let (eye, target) = (rcam::position(camera_), rcam::target(camera_));
    let forward = vec3::normalize(vec3::sub(target, eye));
    let side = vec3::cross(forward, rcam::up(camera_));
    if vec3::length(side) < 1e-6 {
        return Vec::new();
    }
    let right = vec3::normalize(side);
    let up = vec3::cross(right, forward);
    let fov_y = match rcam::projection(camera_) {
        rcam::Projection::Perspective { fov_y, .. } => fov_y,
        _ => 1.,
    };
    let (_, _, width, height) = bounds;
    let half_h = 0.5 * (fov_y / 2.).tan();
    let half_w = half_h * width as f64 / height.max(1) as f64;
    let centre = vec3::add(eye, vec3::scale(forward, 0.5));
    let corner = |sx: f64, sy: f64| vec3::add(centre, vec3::add(vec3::scale(right, sx * half_w), vec3::scale(up, sy * half_h)));
    let corners = [corner(-1., -1.), corner(1., -1.), corner(1., 1.), corner(-1., 1.)];
    let edge = |a: vec3::T, b: vec3::T| segment(bounds, view_, color_, a, b);
    let mut out = Vec::new();
    for index in 0..4 {
        out.extend(edge(eye, corners[index]));
        out.extend(edge(corners[index], corners[(index + 1) % 4]));
    }
    out.extend(edge(corner(0., 1.), corner(0., 1.6)));
    out.extend(edge(eye, target));
    out
}

/// Handles on the selected node's xyz parameter triples, found by name: W translates position-like
/// triples, E rotates `rotate_*`, R scales `scale_*`; Escape hides them so a drag only orbits. A
/// camera's target also slides along the view direction: its focus distance.
pub const HANDLE_PREFIXES: [&str; 5] = ["translate", "center", "origin", "eye", "target"];
pub const ARROW_LENGTH: f64 = 60.;

/// A handle: its box key, colour, tool, screen base and tip, `unit` (one world unit along its axis
/// on screen, so a drag maps back to a delta), and the parameter edits a delta makes.
#[derive(Clone)]
pub struct Arrow {
    pub name: String,
    pub color: color::T,
    pub tool: Tool,
    pub base: (f64, f64),
    pub tip: (f64, f64),
    pub unit: (f64, f64),
    pub edits: Rc<dyn Fn(f64) -> Vec<(String, Value)>>,
}

pub fn arrows(node: &Picked, view_: &rcam::Camera, bounds: Bounds, space: &mat4::T, tool_: Tool) -> Vec<Arrow> {
    let project_ = |point: vec3::T| project(bounds, view_, mat4::transform_point(space, point));
    let values = &node.fields;
    let find = |name: &str| -> Option<f64> {
        values.iter().find_map(|f| match &f.current {
            Value::Float(v) if f.name == name => Some(*v),
            _ => None,
        })
    };
    let range = |name: &str| -> Option<f64> {
        values.iter().find_map(|f| match &f.kind {
            KindView::Floating(r) if f.name == name => Some(r.soft_max - r.soft_min),
            _ => None,
        })
    };
    let triple = |prefix: &str| -> Option<(f64, f64, f64)> {
        match (find(&format!("{prefix}_x")), find(&format!("{prefix}_y")), find(&format!("{prefix}_z"))) {
            (Some(x), Some(y), Some(z)) => Some((x, y, z)),
            _ => None,
        }
    };
    let pivot = HANDLE_PREFIXES.iter().find_map(|p| triple(p));
    let make = |name: String, color_: color::T, at: vec3::T, direction: vec3::T, per_pixel: f64, edits: Rc<dyn Fn(f64) -> Vec<(String, Value)>>| -> Option<Arrow> {
        match (project_(at), project_(vec3::add(at, direction))) {
            (Some(base), Some((ux, uy))) => {
                let (dx, dy) = (ux - base.0, uy - base.1);
                let length = dx.hypot(dy);
                if length < 1e-3 {
                    None
                } else {
                    Some(Arrow {
                        name,
                        color: color_,
                        tool: tool_,
                        base,
                        unit: (dx, dy),
                        edits: Rc::new(move |delta| edits(delta * per_pixel)),
                        tip: (base.0 + dx / length * ARROW_LENGTH, base.1 + dy / length * ARROW_LENGTH),
                    })
                }
            }
            _ => None,
        }
    };
    let ax = axes();
    let colors = axis_colors();
    let handles = |prefix: &str, (x, y, z): (f64, f64, f64), at: vec3::T| -> Vec<Arrow> {
        (0..3)
            .filter_map(|axis| {
                let name = format!("{prefix}_{}", AXIS_NAMES[axis]);
                let value = [x, y, z][axis];
                let per_pixel = if tool_ == Tool::Turn { range(&name).unwrap_or(360.) / 360. } else { 1. };
                let n = name.clone();
                make(name, colors[axis], at, ax[axis], per_pixel, Rc::new(move |delta| vec![(n.clone(), Value::Float(value + delta))]))
            })
            .collect()
    };
    let at = match pivot {
        Some((x, y, z)) => vec3::create(x, y, z),
        None => vec3::zero(),
    };
    match tool_ {
        Tool::NoTool => Vec::new(),
        Tool::Move => {
            let distance: Vec<Arrow> = match (triple("eye"), triple("target")) {
                (Some((ex, ey, ez)), Some((tx, ty, tz))) => {
                    let target = vec3::create(tx, ty, tz);
                    let direction = vec3::sub(target, vec3::create(ex, ey, ez));
                    if vec3::length(direction) < 1e-6 {
                        Vec::new()
                    } else {
                        let direction = vec3::normalize(direction);
                        make(
                            "target_distance".to_string(),
                            active_color(),
                            target,
                            direction,
                            1.,
                            Rc::new(move |delta| {
                                let p = vec3::add(target, vec3::scale(direction, delta));
                                vec![("target_x".to_string(), Value::Float(p.x)), ("target_y".to_string(), Value::Float(p.y)), ("target_z".to_string(), Value::Float(p.z))]
                            }),
                        )
                        .into_iter()
                        .collect()
                    }
                }
                _ => Vec::new(),
            };
            let mut out: Vec<Arrow> = HANDLE_PREFIXES
                .iter()
                .flat_map(|prefix| match triple(prefix) {
                    Some(values) => handles(prefix, values, vec3::create(values.0, values.1, values.2)),
                    None => Vec::new(),
                })
                .collect();
            out.extend(distance);
            out
        }
        Tool::Turn => triple("rotate").map(|v| handles("rotate", v, at)).unwrap_or_default(),
        Tool::Grow => triple("scale").map(|v| handles("scale", v, at)).unwrap_or_default(),
    }
}

pub const PICK_SIZE: f64 = 16.;

/// Handle drags edit the selected node; a click on a camera guide's eye selects that camera object.
pub fn handles(ui: &mut Ui, selected: Option<&Picked>, scene_: &kg::T, space: &mat4::T, view_: &rcam::Camera, extra_: &Extra, bounds: Bounds) -> (Vec<(String, Value)>, bool, Option<i64>) {
    let mut edits: Vec<(String, Value)> = Vec::new();
    let mut grab = false;
    if let Some(node) = selected {
        if extra_.show.handles {
            for a in arrows(node, view_, bounds, space, extra_.tool) {
                let (tx, ty) = a.tip;
                let box_ = ui.box_fmt(format_args!("handle-{}", a.name), BoxOpts::default().flags(CLICKABLE).w(Size::Px(14.)).h(Size::Px(14.)).at(((tx - 7.) as f32, (ty - 7.) as f32)));
                let signal = ui.signal(&box_);
                let (mx, my) = pxui::geom::p64(signal.drag);
                let (ux, uy) = a.unit;
                let delta = match a.tool {
                    Tool::Turn => (mx * ux + my * uy) / ux.hypot(uy),
                    _ => (mx * ux + my * uy) / (ux * ux + uy * uy),
                };
                if signal.held && delta != 0. {
                    let mut next = (a.edits)(delta);
                    next.extend(edits);
                    edits = next;
                }
                grab = grab || signal.held || signal.released;
            }
        }
    }
    let mut picked_id = None;
    if extra_.show.cameras {
        for node_id in camera_ids(scene_) {
            let camera_ = kg::find(scene_, node_id).and_then(|n| node_camera(&n));
            if let Some(camera_) = camera_ {
                if !same_view(&camera_, view_) {
                    if let Some((x, y)) = project(bounds, view_, rcam::position(&camera_)) {
                        let box_ = ui.box_fmt(format_args!("pick-camera-{node_id}"), BoxOpts::default().flags(CLICKABLE).w(Size::Px(PICK_SIZE as f32)).h(Size::Px(PICK_SIZE as f32)).at(((x - PICK_SIZE / 2.) as f32, (y - PICK_SIZE / 2.) as f32)));
                        let signal = ui.signal(&box_);
                        grab = grab || signal.held;
                        if signal.clicked {
                            picked_id = Some(node_id);
                        }
                    }
                }
            }
        }
    }
    (edits, grab, picked_id)
}

/// The axis gizmo (viewport.html): its origin 48 points from the pane's left and 44 from its foot,
/// an axis is 40 points at full length in its port colour, and its letter a label at the tip.
pub fn gizmo((x, y, width, height): Bounds, view_: &rcam::Camera) -> Nodes {
    // a body about as wide as the workspace sheet's draws that sheet's gizmo: its origin 40 from the
    // left and 36 from the foot, axes 36 long, no letters
    let compact = width < 1000;
    let cx = x as f64 + if compact { 40. } else { 48. };
    let cy = (y + height) as f64 - if compact { 36. } else { 44. };
    let length = if compact { 36. } else { 40. };
    let origin = rcam::world_to_camera(view_, vec3::zero()).unwrap_or_else(|_| vec3::zero());
    let ax = axes();
    let colors = axis_colors();
    let mut out = Vec::new();
    for axis in 0..3 {
        let d = vec3::sub(rcam::world_to_camera(view_, ax[axis]).unwrap_or_else(|_| vec3::zero()), origin);
        let tip = (cx + d.x * length, cy - d.y * length);
        let (dx, dy) = [(3., 4.), (6., 4.), (-8., 7.)][axis];
        out.extend(thick_line(1.5, colors[axis], (cx, cy), tip));
        if !compact {
            out.push(scene::text(((tip.0 + dx) as i64, (tip.1 + dy) as i64 - 6), Some(colors[axis]), Some(pxui::theme::LABEL_SIZE), &AXIS_NAMES[axis].to_uppercase()));
        }
    }
    out
}

/// A light object: a marker at its position, a line to its target.
pub fn light_guide(bounds: Bounds, view_: &rcam::Camera, color_: color::T, light_: &light::Light) -> Nodes {
    let marker = |(x, y): (f64, f64)| one(scene::circle((x as i64, y as i64), 5, Some(color_), None));
    // a light whose marker is off the pane draws no stray line
    let inside = |(x, y): (f64, f64)| {
        let (bx, by, bw, bh) = bounds;
        x >= bx as f64 && x <= (bx + bw) as f64 && y >= by as f64 && y <= (by + bh) as f64
    };
    let aim = |at: vec3::T, direction: vec3::T| -> Nodes {
        match project(bounds, view_, at) {
            Some(point) if inside(point) => {
                let mut out = marker(point);
                out.extend(segment(bounds, view_, color_, at, vec3::add(at, vec3::scale(direction, 1.5))));
                out
            }
            _ => Vec::new(),
        }
    };
    match &light_.kind {
        light::Kind::Directional { direction } => aim(vec3::scale(*direction, -4.), *direction),
        light::Kind::Point { position, .. } => project(bounds, view_, *position).map(marker).unwrap_or_default(),
        light::Kind::Spot { position, direction, .. } => aim(*position, *direction),
        light::Kind::Area { position, direction, width, height, .. } => {
            let side = vec3::cross(*direction, vec3::create(0., 1., 0.));
            let side = if vec3::length(side) < 1e-6 { vec3::create(1., 0., 0.) } else { vec3::normalize(side) };
            let up = vec3::normalize(vec3::cross(side, *direction));
            let corner = |sx: f64, sy: f64| vec3::add(*position, vec3::add(vec3::scale(side, sx * width / 2.), vec3::scale(up, sy * height / 2.)));
            let corners = [corner(-1., -1.), corner(1., -1.), corner(1., 1.), corner(-1., 1.)];
            let mut out = aim(*position, *direction);
            for index in 0..4 {
                out.extend(segment(bounds, view_, color_, corners[index], corners[(index + 1) % 4]));
            }
            out
        }
    }
}

/// `bounds` is the rectangle the view is drawn in (`film`): what is projected lies on the picture.
/// The axis gizmo belongs to the `pane`'s corner.
pub fn guides(scene_: &kg::T, selected: Option<&Picked>, space: &mat4::T, view_: &rcam::Camera, extra_: &Extra, pane: Bounds, bounds: Bounds) -> Nodes {
    let mut cameras = Nodes::new();
    if extra_.show.cameras {
        for node_id in camera_ids(scene_) {
            let camera_ = kg::find(scene_, node_id).and_then(|n| node_camera(&n));
            if let Some(camera_) = camera_ {
                if !same_view(&camera_, view_) {
                    let active = rcam::position(&camera_) == rcam::position(&extra_.document_camera) && rcam::target(&camera_) == rcam::target(&extra_.document_camera);
                    let color_ = if active { active_color() } else { guide_color() };
                    if let Some((x, y)) = project(bounds, view_, rcam::position(&camera_)) {
                        cameras.extend(one(scene::circle((x as i64, y as i64), 4, Some(color_), None)));
                    }
                    cameras.extend(frustum(bounds, view_, color_, &camera_));
                }
            }
        }
    }
    let mut lights = Nodes::new();
    if extra_.show.cameras {
        for l in objects::lights(false, scene_) {
            lights.extend(light_guide(bounds, view_, active_color(), &l));
        }
    }
    let mut handles_ = Nodes::new();
    if let Some(node) = selected {
        if extra_.show.handles {
            for a in arrows(node, view_, bounds, space, extra_.tool) {
                let at = (a.tip.0 as i64, a.tip.1 as i64);
                handles_.extend(line(2, a.color, a.base, a.tip));
                match a.tool {
                    Tool::Turn => {
                        handles_.extend(one(scene::circle(at, 7, None, Some(a.color))));
                        handles_.extend(one(scene::circle(at, 6, None, Some(a.color))));
                    }
                    Tool::Grow => handles_.extend(one(scene::rect((at.0 - 5, at.1 - 5), 10, 10, Some(a.color), None))),
                    Tool::Move | Tool::NoTool => handles_.extend(one(scene::circle(at, 5, Some(a.color), None))),
                }
            }
        }
    }
    let mut out = cameras;
    out.extend(lights);
    if extra_.show.axes {
        out.extend(gizmo(pane, view_));
    }
    out.extend(handles_);
    out
}

pub fn save(request: &Request) -> Result<(), String> {
    cc::save(request)
}
pub fn filename(request: &Request) -> String {
    request.filename.clone()
}

pub fn close(extra_: &Extra) {
    renderer::close(&extra_.renderer);
    let _ = release(extra_);
}
