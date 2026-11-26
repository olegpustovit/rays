//! Path-traced editor viewports share the renderer settings.

use std::cell::RefCell;
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

/// Converted meshes keyed by source and material. A scene change prunes the entries that no
/// current viewport uses, so old takes and material edits cannot pin CPU geometry or textures.
#[derive(Clone)]
pub struct MeshCache {
    entries: Vec<(mesh::Mesh, material::Material, Option<i64>, Rc<CachedMesh>)>,
}

impl MeshCache {
    fn peek(&self, source: &mesh::Mesh, material: &material::Material, texture: Option<i64>) -> Option<Rc<CachedMesh>> {
        self.entries.iter().find(|(m, mat, tex, _)| mesh::same(m, source) && mat == material && *tex == texture).map(|(_, _, _, c)| c.clone())
    }
}

pub fn empty_meshes() -> MeshCache {
    MeshCache { entries: Vec::new() }
}

/// How one viewport renders: the root of its scene instance, else the document's. The sample cap is
/// not part of it: a cap is read each frame, so raising one continues the accumulation while any
/// other change discards the tracer.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Setting {
    pub mode: Mode,
    pub bounces: i64,
    pub round_samples: i64,
    pub film: editor_document::objects::root::Film,
}

/// A viewport as one frame asks to draw it: `film` is the tracer's film in drawable pixels, `step`
/// the divisor of the viewport's drawable size, `cap` the root's samples per pixel.
#[derive(Clone)]
pub struct View {
    pub key: String,
    pub film: (i64, i64),
    pub step: i64,
    pub camera: camera::Camera,
    pub scene: d3::Scene3,
    pub setting: Setting,
    pub cap: i64,
    pub full_size_at: f64,
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
    graded: Option<(i64, rays::image::Image)>,
    pub setting: Setting,
    pub cap: i64,
    pub full_size_at: f64,
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

/// Use half the drawable dimensions during motion, then accumulate at full size.
pub fn film(gate: (i64, i64), moving: bool) -> ((i64, i64), i64) {
    let step = if moving { 2 } else { 1 };
    (((gate.0 / step).max(1), (gate.1 / step).max(1)), step)
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

/// Placement remains in the tracer's instance structure, so an orbit does not convert geometry.
/// The scene's first mesh and material are the bootstrap for a new tracer.
pub fn traced_scene(meshes: &MeshCache, scene: &d3::Scene3) -> Result<Traced, String> {
    if let Some(drawing) = device_drawing(scene) {
        let first = (drawing.mesh.clone(), traced_material(&drawing.material));
        let traced = p::mesh(None, None, std::slice::from_ref(&first))?;
        return Ok((meshes.clone(), traced, first));
    }
    let mut cache = meshes.clone();
    let mut result: Result<Vec<(mat4::T, p::Mesh)>, String> = Ok(Vec::new());
    let mut first = None;
    walk::iter_batches(
        &mut |drawing, transforms| {
            let Ok(placed) = &mut result else { return };
            let step = (|| -> Result<(mat4::T, p::Mesh), String> {
                let texture_id = |tex: &d3::Texture| rays::texture::private::identity(&tex.value);
                let key = drawing.texture.as_ref().map(texture_id);
                let cached = match cache.peek(&drawing.mesh, &drawing.material, key) {
                    Some(cached) => cached,
                    None => {
                        let geometry = rdk_rays::rays_mesh::of_mesh(&drawing.mesh).map_err(|e| rdk_rays::geometry_view::error_string(&e))?;
                        let trace_mesh = rdk_rays::rays_mesh::to_trace_mesh(&geometry).map_err(|e| rdk_rays::geometry_view::error_string(&e))?;
                        let mut material = traced_material(&drawing.material);
                        if let Some(texture) = &drawing.texture {
                            material.texture = Some(rays::texture::private::trace_pixels(&texture.value)?);
                        }
                        let traced = p::mesh(None, None, &[(trace_mesh.clone(), material)])?;
                        let cached = Rc::new(CachedMesh { trace_source: trace_mesh, traced });
                        cache.entries.push((drawing.mesh.clone(), drawing.material.clone(), key, cached.clone()));
                        cached
                    }
                };
                first.get_or_insert_with(|| (cached.trace_source.clone(), traced_material(&drawing.material)));
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
    let first = first.ok_or_else(|| "Path tracing needs a scene mesh.".to_string())?;
    Ok((cache, mesh_, first))
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
                    let tracer = tracer.borrow();
                    !d3::same(scene, &view.scene) || camera.as_ref() != Some(&view.camera) || *film != view.film
                        || p::samples(&tracer) < cap || tracer.building.is_some() || tracer.queued_mesh.is_some()
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
    if plans.iter().any(|plan| plan.previous.is_none_or(|i| !d3::same(&state.slots[i].scene, &plan.view.scene)))
        || state.slots.len() != slots.len()
    {
        meshes.entries.retain(|(mesh_, material, texture, _)| {
            let mut shown = false;
            for slot in &slots {
                walk::iter_batches(&mut |drawing, _| {
                    shown |= mesh::same(&drawing.mesh, mesh_) && &drawing.material == material
                        && drawing.texture.as_ref().map(|tex| rays::texture::private::identity(&tex.value)) == *texture;
                }, &slot.scene);
            }
            shown
        });
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
        graded: None,
        setting,
        cap: plan.limit,
        full_size_at: view.full_size_at,
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
                let mut slot = Slot { keys: plan.names.clone(), cap: plan.limit, full_size_at: view.full_size_at, ..slot.clone() };
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
                if let Some(tracer) = &prior.tracer
                    && !p::try_resize(&mut tracer.borrow_mut(), width, height)?
                {
                    return Ok(Slot { keys: plan.names.clone(), cap: plan.limit, full_size_at: view.full_size_at, ..prior.clone() });
                }
                *meshes = cache;
                let tracer: Tracer = match &prior.tracer {
                    Some(tracer) => tracer.clone(),
                    None => {
                        let first = mesh_.as_ref().map(|(_, first)| first.clone()).ok_or_else(|| "Path tracing needs a scene mesh.".to_string())?;
                        let options = p::CreateOptions { bounces: Some(setting.bounces), round_samples: Some(setting.round_samples), exposure: Some(2f64.powf(setting.film.exposure)), ..p::CreateOptions::default() };
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
                if prior.tracer.is_none() || walk::lights(&prior.scene) != walk::lights(scene) {
                    let lights: Vec<p::Light> = walk::lights(scene).iter().map(p::light_of).collect();
                    p::set_lights(&mut tracer.borrow_mut(), &lights)?;
                }
                p::set_world(&mut tracer.borrow_mut(), walk::world(scene).as_ref())?;
                p::render(&mut tracer.borrow_mut(), &view.camera)?;
                let graded = if crate::film_grade::enabled(setting.film) {
                    let t = tracer.borrow();
                    let raw = p::image(&t);
                    let generation = rays::image::generation(&raw);
                    match prior.graded.as_ref() {
                        Some((known, _)) if *known == generation => prior.graded.clone(),
                        // Read back the full film once at convergence. Preview and accumulation
                        // stay on the GPU while the viewport or its camera is moving.
                        _ if p::samples(&t) >= plan.limit => {
                            let into = prior.graded.as_ref().map(|(_, pixels)| pixels);
                            Some((generation, crate::film_grade::apply(&raw, setting.film, into)?))
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                Ok(Slot { scene: scene.clone(), camera: Some(view.camera.clone()), tracer: Some(tracer), graded, ..blank.clone() })
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

/// What a traced viewport's slot holds: the film in pixels, the divisor of its drawable size, the samples per pixel accumulated and the cap they run to, and how many viewports it serves.
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

pub fn paint(state: &State, key: &str, bounds: (i64, i64, i64, i64), camera: &camera::Camera, scene_: &d3::Scene3, previous: &[scene::Node]) -> Option<scene::Node> {
    let view = |scene_: &d3::Scene3| {
        previous.iter().find_map(|node| match node {
            scene::Node::View3d(view) if view.viewport == Some(bounds) && view.camera == *camera && d3::same(&view.scene, scene_) => Some(node.clone()),
            _ => None,
        }).unwrap_or_else(|| scene::view3d(Some(bounds), camera, scene_.clone()))
    };
    let raster = || Some(view(scene_));
    if state.custom || state.raster_only {
        return raster();
    }
    match slot_of(state, key) {
        Some(Slot { setting: Setting { mode: Mode::Raster, .. }, .. }) => raster(),
        Some(Slot { wire: Some(wire), .. }) => Some(view(wire)),
        Some(Slot { tracer: Some(tracer), graded, .. }) => {
            let (x, y, w, h) = bounds;
            let tracer = tracer.borrow();
            let image = graded.as_ref().map_or_else(|| p::image(&tracer), |(_, image)| image.clone());
            let (sw, sh) = rays::image::dimensions(&image);
            scene::image(&image, (0, 0), None, None).ok().map(|node| {
                let image = scene::scale(w as f64 / sw.max(1) as f64, h as f64 / sh.max(1) as f64, vec![node]);
                scene::translate(x, y, vec![image])
            })
        }
        _ => None,
    }
}
