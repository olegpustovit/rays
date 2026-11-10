//! Path-traced editor viewports share the renderer settings. ponytail: converted meshes use a 64-entry MRU list with estimated byte charges outside global Lru retreat.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use rays::camera;
use rays::material;
use rays::mesh;
use rays::scene;
use rays::scene3::{self as d3, walk};
use rays_math::{color, mat4};
use rays_pathtracer::rays_pathtracer as p;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    PathTraced,
    Raster,
    Wireframe,
}

pub fn kind() -> Result<param::Kind, String> {
    param::choice(vec!["Path traced".to_string(), "Raster".to_string(), "Wireframe".to_string()])
}

pub fn field(default: Mode) -> Result<param::Field, String> {
    param::field("renderer".to_string(), kind()?, param::Value::Choice(label(default).to_string()))
}

pub fn wire_color(background: &color::T) -> color::T {
    let lightness = 299 * background.r + 587 * background.g + 114 * background.b;
    if lightness >= 128000 {
        color::hex_exn("#285f77").expect("wire colour")
    } else {
        color::hex_exn("#bed7e1").expect("wire colour")
    }
}

pub fn label(mode: Mode) -> &'static str {
    match mode {
        Mode::PathTraced => "Path traced",
        Mode::Raster => "Raster",
        Mode::Wireframe => "Wireframe",
    }
}

pub fn of_label(label: &str) -> Option<Mode> {
    match label {
        "Path traced" => Some(Mode::PathTraced),
        "Raster" => Some(Mode::Raster),
        "Wireframe" => Some(Mode::Wireframe),
        _ => None,
    }
}

#[derive(Clone)]
pub struct CachedMesh {
    pub trace_source: mesh::Mesh,
    pub traced: p::Mesh,
}

const MESH_ENTRIES: usize = 64;

/// The converted source meshes, most recent first, keyed by the mesh (physical identity) and its
/// material.
#[derive(Clone)]
pub struct MeshCache {
    entries: VecDeque<(mesh::Mesh, material::Material, Rc<CachedMesh>)>,
}

impl MeshCache {
    fn peek(&self, mesh_: &mesh::Mesh, material_: &material::Material) -> Option<Rc<CachedMesh>> {
        self.entries.iter().find(|(m, mat, _)| mesh::same(m, mesh_) && mat == material_).map(|(_, _, c)| c.clone())
    }

    fn add(&mut self, mesh_: mesh::Mesh, material_: material::Material, cached: CachedMesh) {
        if let Some(at) = self.entries.iter().position(|(m, mat, _)| mesh::same(m, &mesh_) && *mat == material_) {
            self.entries.remove(at);
        }
        self.entries.push_front((mesh_, material_, Rc::new(cached)));
        while self.entries.len() > MESH_ENTRIES {
            self.entries.pop_back();
        }
    }

    fn head(&self) -> Option<(&material::Material, &Rc<CachedMesh>)> {
        self.entries.front().map(|(_, mat, c)| (mat, c))
    }
}

pub fn empty_meshes() -> MeshCache {
    MeshCache { entries: VecDeque::new() }
}

/// How one viewport renders: the root of its scene instance, else the document's. The sample cap is
/// not part of it: a cap is read each frame, so raising one continues the accumulation while any
/// other change discards the tracer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Setting {
    pub mode: Mode,
    pub bounces: i64,
    pub round_samples: i64,
}

/// A viewport as one frame asks to draw it: `film` is the tracer's film in drawable pixels, `step`
/// the divisor of the root's resolution it came from, `cap` the root's samples per pixel.
#[derive(Clone)]
pub struct View {
    pub key: String,
    pub film: (i64, i64),
    pub step: i64,
    pub camera: camera::Camera,
    pub scene: d3::Scene3,
    pub setting: Setting,
    pub cap: i64,
}

pub type Tracer = Rc<RefCell<p::PathTracer>>;

/// One tracer, shared by every viewport that asks for the same picture. `scene`, `camera` and `film`
/// are what the tracer was last given: a slot differing from the frame's request is stale.
#[derive(Clone)]
pub struct Slot {
    pub keys: Vec<String>,
    pub film: (i64, i64),
    pub step: i64,
    pub scene: d3::Scene3,
    pub camera: Option<camera::Camera>,
    pub wire: Option<d3::Scene3>,
    pub tracer: Option<Tracer>,
    pub setting: Setting,
    pub cap: i64,
    /// why the last render of the slot failed, until one succeeds
    pub failure: Option<String>,
}

/// `mode` is the document's; `raster_only`: every viewport draws as raster, so there are no slots;
/// `turn`: the viewport whose slot last took a turn of the sample budget.
#[derive(Clone)]
pub struct State {
    pub mode: Mode,
    pub custom: bool,
    pub slots: Vec<Slot>,
    pub meshes: MeshCache,
    pub error: Option<String>,
    pub raster_only: bool,
    pub turn: Option<String>,
}

pub fn empty() -> State {
    State { mode: Mode::Raster, custom: false, slots: Vec::new(), meshes: empty_meshes(), error: None, raster_only: true, turn: None }
}

fn destroy(tracer: &Tracer) {
    p::destroy(&mut tracer.borrow_mut());
}

pub fn close(state: &State) {
    for slot in &state.slots {
        if let Some(tracer) = &slot.tracer {
            destroy(tracer);
        }
    }
}

/// The film of a traced viewport: the root's resolution divided by 1, 2, 4 or 8, the largest that
/// does not exceed the gate (in drawable pixels, the root's aspect fitted in the pane), the eighth
/// when even that is too big. A pane resize changes the film only by crossing a step. A viewport
/// that is not looking `through` the render camera shows its own free view, not the render: its film
/// is its rectangle, pixel for pixel, so the picture has the pane's aspect and lies where the raster
/// one does.
pub const FILM_STEPS: [i64; 4] = [1, 2, 4, 8];

pub fn film(through: bool, resolution: (i64, i64), gate: (i64, i64)) -> ((i64, i64), i64) {
    let (width, height) = resolution;
    let (gate_width, gate_height) = gate;
    if !through {
        return ((gate_width.max(1), gate_height.max(1)), 1);
    }
    let step = FILM_STEPS.iter().copied().find(|step| width / step <= gate_width && height / step <= gate_height).unwrap_or(8);
    (((width / step).max(1), (height / step).max(1)), step)
}

fn linear(c: &color::T) -> p::LinearColor {
    p::Linear_color::rgb(c.r as f64 / 255.0, c.g as f64 / 255.0, c.b as f64 / 255.0)
}

pub fn traced_material(m: &material::Material) -> p::Material {
    p::material(Some((2.0 / (m.shininess.max(0.0) + 2.0)).sqrt()), None, Some(linear(&m.emissive)), Some(m.round), linear(&m.diffuse))
}

pub fn wire_scene(scene: &d3::Scene3) -> d3::Scene3 {
    let mut nodes = Vec::new();
    walk::iter_batches(
        &mut |drawing, transforms| {
            let material = material::unlit(wire_color(&walk::ambient(scene)));
            let node = match transforms {
                None => d3::mesh(Some(material), None, Some(d3::RenderMode::Wireframe), Some(d3::Cull::CullNone), None, drawing.mesh.clone()),
                Some(transforms) => d3::instances_array(
                    Some(material),
                    Some(d3::RenderMode::Wireframe),
                    Some(d3::Cull::CullNone),
                    None,
                    drawing.mesh.clone(),
                    transforms,
                ),
            };
            nodes.push(d3::transform(drawing.transform.clone(), vec![node]));
        },
        scene,
    );
    d3::create(None, None, None, Some(1), nodes).unwrap_or_else(|_| d3::empty())
}

/// The scene's one drawing when it is a device mesh in place (one batch, no instances, no texture,
/// identity transform, normals of its own): the tracer reads it where the device holds it
/// (`Rays_pathtracer.mesh`), never converted or cached, so a deforming mesh refits its structure
/// instead of being read back.
pub fn device_drawing(scene: &d3::Scene3) -> Option<walk::Drawing> {
    let mut batches: Vec<(walk::Drawing, bool)> = Vec::new();
    walk::iter_batches(&mut |drawing, transforms| batches.push((drawing.clone(), transforms.is_some())), scene);
    match batches.as_slice() {
        [(drawing, false)]
            if drawing.texture.is_none()
                && mesh::view::device(&drawing.mesh).is_some()
                && !mesh::view::derived_normals(&drawing.mesh)
                && mat4::nearly_equal(&drawing.transform, &mat4::identity(), 0.0) =>
        {
            Some(drawing.clone())
        }
        _ => None,
    }
}


type Traced = (MeshCache, p::Mesh, (mesh::Mesh, p::Material));

/// The cache keeps at most 64 converted source meshes. Placement remains in the tracer's instance
/// structure, so an orbit does not convert or upload geometry. The scene's mesh and material a new
/// tracer starts from come with it.
pub fn traced_scene(meshes: &MeshCache, scene: &d3::Scene3) -> Result<Traced, String> {
    if let Some(drawing) = device_drawing(scene) {
        let first = (drawing.mesh.clone(), traced_material(&drawing.material));
        let traced = p::mesh(None, None, std::slice::from_ref(&first))?;
        return Ok((meshes.clone(), traced, first));
    }
    let mut cache = meshes.clone();
    let mut result: Result<Vec<(mat4::T, p::Mesh)>, String> = Ok(Vec::new());
    walk::iter_batches(
        &mut |drawing, transforms| {
            let Ok(placed) = &mut result else { return };
            let step = (|| -> Result<(mat4::T, p::Mesh), String> {
                if drawing.texture.is_some() {
                    return Err("Path tracing textured meshes is not supported.".to_string());
                }
                let cached = match cache.peek(&drawing.mesh, &drawing.material) {
                    Some(cached) => cached,
                    None => {
                        let geometry = rdk_rays::rays_mesh::of_mesh(&drawing.mesh).map_err(|e| rdk_rays::geometry_view::error_string(&e))?;
                        let trace_mesh = rdk_rays::rays_mesh::to_trace_mesh(&geometry).map_err(|e| rdk_rays::geometry_view::error_string(&e))?;
                        let traced = p::mesh(None, None, &[(trace_mesh.clone(), traced_material(&drawing.material))])?;
                        let cached = CachedMesh { trace_source: trace_mesh, traced };
                        cache.add(drawing.mesh.clone(), drawing.material.clone(), cached.clone());
                        Rc::new(cached)
                    }
                };
                let placed_mesh = match transforms {
                    None => cached.traced.clone(),
                    Some(transforms) => p::mesh_instanced(&cached.traced, None, None, transforms)?,
                };
                Ok((drawing.transform.clone(), placed_mesh))
            })();
            match step {
                Ok(one) => placed.push(one),
                Err(e) => result = Err(e),
            }
        },
        scene,
    );
    let placed = result?;
    let mesh_ = p::scene_mesh(&placed)?;
    let (source_material, cached) = cache.head().ok_or_else(|| "Path tracing needs a scene mesh.".to_string())?;
    let first = (cached.trace_source.clone(), traced_material(source_material));
    Ok((cache.clone(), mesh_, first))
}

/// Viewports asking for the same traced picture share one tracer: the same scene, camera, film and
/// setting.
pub fn shares(a: &View, b: &View) -> bool {
    a.setting.mode == Mode::PathTraced && a.setting == b.setting && d3::same(&a.scene, &b.scene) && a.film == b.film && a.camera == b.camera
}

/// The groups of viewports that share a tracer, the focused one first so it keeps the slot it had.
pub fn groups(focus: &str, views: &[View]) -> Vec<Vec<View>> {
    let mut ordered: Vec<&View> = views.iter().collect();
    if let Some(at) = views.iter().position(|v| v.key == focus) {
        ordered = std::iter::once(&views[at]).chain(views.iter().enumerate().filter(|(i, _)| *i != at).map(|(_, v)| v)).collect();
    }
    let mut groups: Vec<Vec<View>> = Vec::new();
    for v in ordered {
        match groups.iter_mut().find(|group| shares(&group[0], v)) {
            Some(group) => group.push(v.clone()),
            None => groups.push(vec![v.clone()]),
        }
    }
    groups
}

#[derive(Clone)]
pub struct Plan {
    pub view: View,
    pub names: Vec<String>,
    pub limit: i64,
    /// the index in the previous state's slots
    pub previous: Option<usize>,
    pub wants: bool,
    pub focused: bool,
}

fn no_batches(scene: &d3::Scene3) -> bool {
    let mut found = false;
    walk::iter_batches(&mut |_, _| found = true, scene);
    !found
}

/// Of the viewports that want a turn, the one after `last` in `order`, cyclically: each takes its
/// turn before any takes a second one.
pub fn next_turn(order: &[String], last: Option<&str>, wanting: &[String]) -> Option<String> {
    let count = order.len() as i64;
    let position = |key: &str| order.iter().position(|k| k == key).map_or(-1, |i| i as i64);
    let after = last.map_or(-1, position);
    let distance = |key: &str| (position(key) - after - 1 + count).rem_euclid(count.max(1));
    let mut best: Option<&String> = None;
    for key in wanting {
        best = match best {
            Some(b) if distance(b) <= distance(key) => Some(b),
            _ => Some(key),
        };
    }
    best.cloned()
}

/// `focus`: the viewport whose slot renders every frame. The others share the sample budget: of the
/// slots that want the GPU (their picture differs from the frame's, or they are short of their cap)
/// one takes a turn per frame, in rotation. A slot at its cap costs nothing.
pub fn update(state: &State, mode: Mode, custom: bool, focus: Option<&str>, views: &[View]) -> State {
    if custom || views.iter().all(|v| v.setting.mode == Mode::Raster) {
        close(state);
        return State { mode, custom, ..empty() };
    }
    if let Some(unavailable) = p::available() {
        close(state);
        return State { mode, custom, error: Some(unavailable), ..empty() };
    }
    if views.len() > 16 {
        close(state);
        return State {
            mode,
            raster_only: false,
            error: Some("At most 16 viewports can render together.".to_string()),
            ..empty()
        };
    }
    let focus: String = match focus {
        Some(key) if views.iter().any(|v| v.key == key) => key.to_string(),
        _ => views[0].key.clone(),
    };
    let mut claimed: Vec<usize> = Vec::new();
    // what each group had: a slot of the same setting that served one of its viewports, taken by one
    // group only
    let mut plans: Vec<Plan> = Vec::new();
    for group in groups(&focus, views) {
        let view = group[0].clone();
        let keys: Vec<String> = group.iter().map(|v| v.key.clone()).collect();
        let previous = state
            .slots
            .iter()
            .enumerate()
            .find(|(i, slot)| !claimed.contains(i) && slot.setting == view.setting && keys.iter().any(|key| slot.keys.contains(key)))
            .map(|(i, _)| i);
        if let Some(i) = previous {
            claimed.push(i);
        }
        let cap = group.iter().fold(1, |cap, v| cap.max(v.cap));
        let wants = view.setting.mode == Mode::PathTraced
            && match previous.map(|i| &state.slots[i]) {
                Some(Slot { tracer: Some(tracer), scene, camera, film, .. }) => {
                    !d3::same(scene, &view.scene) || camera.as_ref() != Some(&view.camera) || *film != view.film || p::samples(&tracer.borrow()) < cap
                }
                Some(Slot { tracer: None, .. }) | None => true,
            };
        let focused = keys.contains(&focus);
        plans.push(Plan { view, names: keys, limit: cap, previous, wants, focused });
    }
    let order: Vec<String> = views.iter().map(|v| v.key.clone()).collect();
    let wanting: Vec<String> = plans.iter().filter(|plan| plan.wants && !plan.focused).map(|plan| plan.view.key.clone()).collect();
    let chosen: Option<usize> = next_turn(&order, state.turn.as_deref(), &wanting).and_then(|key| plans.iter().position(|plan| plan.view.key == key));
    let mut meshes = state.meshes.clone();
    let mut slots: Vec<Slot> = Vec::new();
    for (index, plan) in plans.iter().enumerate() {
        slots.push(settle(state, plan, chosen == Some(index), &mut meshes));
    }
    for previous in &state.slots {
        if let Some(tracer) = &previous.tracer {
            if !slots.iter().any(|slot| slot.tracer.as_ref().is_some_and(|t| Rc::ptr_eq(t, tracer))) {
                destroy(tracer);
            }
        }
    }
    let failures: Vec<String> = slots.iter().filter_map(|slot| slot.failure.clone()).collect();
    State {
        mode,
        custom,
        slots,
        meshes,
        raster_only: false,
        turn: match chosen {
            Some(i) => Some(plans[i].view.key.clone()),
            None => state.turn.clone(),
        },
        error: if failures.is_empty() { None } else { Some(failures.join("; ")) },
    }
}

fn settle(state: &State, plan: &Plan, is_chosen: bool, meshes: &mut MeshCache) -> Slot {
    let view = &plan.view;
    let previous: Option<&Slot> = plan.previous.map(|i| &state.slots[i]);
    let setting = view.setting;
    let blank = Slot {
        keys: plan.names.clone(),
        film: view.film,
        step: view.step,
        scene: d3::empty(),
        camera: None,
        wire: None,
        tracer: None,
        setting,
        cap: plan.limit,
        failure: None,
    };
    let scene = &view.scene;
    match setting.mode {
        Mode::Raster => Slot { scene: scene.clone(), ..blank },
        Mode::Wireframe => Slot {
            scene: scene.clone(),
            wire: Some(match previous {
                Some(Slot { wire: Some(wire), scene: last, .. }) if d3::same(last, scene) => wire.clone(),
                Some(_) | None => wire_scene(scene),
            }),
            ..blank
        },
        // at its cap with nothing changed (the focused one too), or not this frame's turn: the
        // picture it had stays
        Mode::PathTraced if !plan.wants || !(plan.focused || is_chosen) => match previous {
            Some(slot) => {
                let mut slot = Slot { keys: plan.names.clone(), cap: plan.limit, ..slot.clone() };
                if let (Some(tracer), false) = (&slot.tracer, plan.wants) {
                    if let Err(message) = p::release_film(&mut tracer.borrow_mut()) {
                        slot.failure = Some(format!("Renderer [{}]: {message}", view.key));
                    }
                }
                slot
            }
            None => blank,
        },
        Mode::PathTraced => {
            let prior: Slot = previous.cloned().unwrap_or_else(|| blank.clone());
            if (!d3::same(&prior.scene, scene) || prior.tracer.is_none()) && no_batches(scene) {
                return Slot { scene: scene.clone(), ..blank };
            }
            let (width, height) = view.film;
            let mut created: Option<Tracer> = None;
            let mut render = || -> Result<Slot, String> {
                let (cache, mesh_) = if d3::same(&prior.scene, scene) && prior.tracer.is_some() {
                    (meshes.clone(), None)
                } else {
                    let (cache, mesh_, first) = traced_scene(meshes, scene)?;
                    (cache, Some((mesh_, first)))
                };
                *meshes = cache;
                let tracer: Tracer = match &prior.tracer {
                    Some(tracer) => tracer.clone(),
                    None => {
                        let first = mesh_.as_ref().map(|(_, first)| first.clone()).ok_or_else(|| "Path tracing needs a scene mesh.".to_string())?;
                        let options = p::CreateOptions { bounces: Some(setting.bounces), round_samples: Some(setting.round_samples), ..p::CreateOptions::default() };
                        let scene_ = p::Scene {
                            objects: vec![first],
                            spheres: Vec::new(),
                            strands: Vec::new(),
                            environment: p::Environment {
                                sky: linear(&color::rgb(130, 145, 170)),
                                ground: linear(&color::rgb(80, 80, 80)),
                                panels: Vec::new(),
                            },
                            lights: Vec::new(),
                        };
                        let tracer = Rc::new(RefCell::new(p::create(&options, width, height, &scene_)?));
                        created = Some(tracer.clone());
                        tracer
                    }
                };
                if let Some((mesh_, _)) = &mesh_ {
                    p::queue_mesh(&mut tracer.borrow_mut(), mesh_)?;
                }
                p::resize(&mut tracer.borrow_mut(), width, height)?;
                if prior.tracer.is_none() || walk::lights(&prior.scene) != walk::lights(scene) {
                    let lights: Vec<p::Light> = walk::lights(scene).iter().map(p::light_of).collect();
                    p::set_lights(&mut tracer.borrow_mut(), &lights)?;
                }
                p::set_world(&mut tracer.borrow_mut(), walk::world(scene).as_ref())?;
                p::render(&mut tracer.borrow_mut(), &view.camera)?;
                Ok(Slot { scene: scene.clone(), camera: Some(view.camera.clone()), tracer: Some(tracer), ..blank.clone() })
            };
            match render() {
                Ok(slot) => slot,
                Err(message) => {
                    if let Some(tracer) = &created {
                        destroy(tracer);
                    }
                    let stale = prior.tracer.is_some();
                    let failure = Some(format!(
                        "Renderer [{}]: {message} ({})",
                        view.key,
                        if stale { "stale output retained" } else { "no output" }
                    ));
                    if stale {
                        Slot { keys: plan.names.clone(), cap: plan.limit, failure, ..prior }
                    } else {
                        Slot { scene: scene.clone(), failure, ..blank }
                    }
                }
            }
        }
    }
}

pub fn slot_of<'a>(state: &'a State, key: &str) -> Option<&'a Slot> {
    state.slots.iter().find(|slot| slot.keys.iter().any(|k| k == key))
}

/// What a traced viewport's slot holds: the film in pixels, the step of the root's resolution it is
/// at, the samples per pixel accumulated and the cap they run to, and how many viewports it serves.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Info {
    pub size: (i64, i64),
    pub step: i64,
    pub samples: i64,
    pub cap: i64,
    pub viewports: i64,
}

pub fn info(state: &State, key: &str) -> Option<Info> {
    if state.custom || state.raster_only {
        return None;
    }
    match slot_of(state, key) {
        Some(Slot { tracer: Some(tracer), step, cap, keys, .. }) => {
            let tracer = tracer.borrow();
            Some(Info { size: p::size(&tracer), step: *step, samples: p::samples(&tracer), cap: *cap, viewports: keys.len() as i64 })
        }
        _ => None,
    }
}

pub fn paint(state: &State, key: &str, bounds: (i64, i64, i64, i64), camera: &camera::Camera, scene_: &d3::Scene3) -> Vec<scene::Node> {
    let raster = || vec![scene::view3d(Some(bounds), camera, scene_.clone())];
    if state.custom || state.raster_only {
        return raster();
    }
    match slot_of(state, key) {
        Some(Slot { setting: Setting { mode: Mode::Raster, .. }, .. }) => raster(),
        Some(Slot { wire: Some(wire), .. }) => vec![scene::view3d(Some(bounds), camera, wire.clone())],
        Some(Slot { tracer: Some(tracer), .. }) => {
            let (x, y, w, _) = bounds;
            let tracer = tracer.borrow();
            // the film is scaled to its pane's gate, so a resize inside a step never re-renders
            let (sw, _) = p::size(&tracer);
            match scene::image(&p::image(&tracer), (x, y), Some(w as f64 / sw.max(1) as f64), None) {
                Ok(node) => vec![node],
                Err(_) => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}
