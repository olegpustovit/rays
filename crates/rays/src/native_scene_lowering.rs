//! Renders a staged `Scene` through one `Renderer` execution: replay of a retained frame, or
//! lowering each layer into a submission and stepping it.
use crate::idlist::{self, IdList, Item};
use crate::scene::{self, layer};
use renderer::renderer::{self as rr, native};
use renderer::scene_execution as se;
use scene_command::display_list;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub enum error {
    Stage(String),
    Resource(ogpu_core::error::Error),
    Begin(ogpu_core::error::Error),
    Lower(ogpu_core::error::Error),
    Step(ogpu_core::error::Error),
}

pub fn pp_error(formatter: &mut dyn std::fmt::Write, e: &error) {
    let text = |e: &ogpu_core::error::Error| ogpu_core::error::to_string(e);
    let _ = match e {
        error::Stage(message) => write!(formatter, "native Scene staging: {message}"),
        error::Resource(e) => write!(formatter, "native Scene resource: {}", text(e)),
        error::Begin(e) => write!(formatter, "native Scene submission setup: {}", text(e)),
        error::Lower(e) => write!(formatter, "native Scene lowering: {}", text(e)),
        error::Step(e) => write!(formatter, "native Scene submission: {}", text(e)),
    };
}

pub fn error_to_string(e: &error) -> String {
    let mut s = String::new();
    pp_error(&mut s, e);
    s
}

fn draw_of_scene3_entry(entry: &se::Scene3Entry) -> rr::Draw {
    rr::prepared_draw(
        entry.family,
        Some(entry.blend),
        entry.texture.clone(),
        entry.auxiliary.clone(),
        entry.vertex_attributes.clone(),
        Some(entry.samples),
        entry.draw.clone(),
    )
}

struct retained_draws {
    /// a frame with several 3D layers caches each (held so the cached draws keep their layer alive)
    _layer: Rc<se::PreparedScene3>,
    draws: Vec<rr::Draw>,
}

const RETAINED_DRAW_CAPACITY: i64 = 16;

#[derive(Clone)]
pub struct cache_context(Rc<RefCell<IdList>>);

thread_local! {
    static CURRENT: RefCell<Option<cache_context>> = const { RefCell::new(None) };
}

pub fn create_cache_context() -> cache_context {
    // ponytail: 16 immutable layers keyed by identity; add stable IDs if this cache grows.
    cache_context(Rc::new(RefCell::new(idlist::create(
        lru::cache("rays.retained_draws"),
        RETAINED_DRAW_CAPACITY,
        Box::new(|a, b| match (a.downcast_ref::<se::PreparedScene3>(), b.downcast_ref::<se::PreparedScene3>()) {
            (Some(x), Some(y)) => std::ptr::eq(x, y),
            _ => false,
        }),
        None,
    ))))
}

struct Restore(Option<cache_context>);

impl Drop for Restore {
    fn drop(&mut self) {
        let previous = self.0.take();
        CURRENT.with(|c| *c.borrow_mut() = previous);
    }
}

pub fn with_cache_context(context: &cache_context, f: &mut dyn FnMut()) {
    let previous = CURRENT.with(|c| c.borrow_mut().replace(context.clone()));
    let _restore = Restore(previous);
    f()
}

fn current() -> Option<cache_context> {
    CURRENT.with(|c| c.borrow().clone())
}

/// A layer whose borrowed stream went is never drawn again; it does not wait for eviction.
fn sweep_retained(owner: &se::GpuVerticesOwner) {
    if let Some(context) = current() {
        let mut stale: Vec<Item> = Vec::new();
        idlist::iter(&mut context.0.borrow_mut(), &mut |layer, _| {
            if let Some(prepared) = layer.downcast_ref::<se::PreparedScene3>() {
                if se::borrows_unregistered(owner, prepared) {
                    stale.push(layer.clone());
                }
            }
        });
        for layer in stale {
            idlist::remove(&mut context.0.borrow_mut(), &layer);
        }
    }
}

pub fn draws_of_prepared(staged: &layer::staged_native, prepared: &Rc<se::PreparedScene3>) -> Vec<rr::Draw> {
    let direct = || prepared.entries.iter().map(draw_of_scene3_entry).collect::<Vec<_>>();
    if staged.retained.is_none() {
        return direct();
    }
    let Some(context) = current() else {
        return direct();
    };
    let key: Item = prepared.clone();
    let found = idlist::find(&mut context.0.borrow_mut(), &key).and_then(|v| v.downcast::<retained_draws>().ok());
    if let Some(cached) = found {
        return cached.draws.clone();
    }
    let draws = direct();
    // no bytes: draws over a layer the scene's own cache charges
    idlist::add(&mut context.0.borrow_mut(), 0, key, Rc::new(retained_draws { _layer: prepared.clone(), draws: draws.clone() }));
    draws
}

fn validate_images(resources: &[(i64, rr::Resource)]) -> Result<(), ogpu_core::error::Error> {
    for (_, r) in resources {
        if let rr::Resource::Image(image) = r {
            runtime_resources::image::gpu_snapshot(image)?;
        }
    }
    Ok(())
}

fn validate_layers(layers: &[layer::native_layer]) -> Result<(), ogpu_core::error::Error> {
    for layer in layers {
        match layer {
            layer::native_layer::Scene2_layer(_, r) | layer::native_layer::Scene2_segment(_, r) | layer::native_layer::Ui_layer(_, r) => validate_images(r)?,
            layer::native_layer::Scene3_layer(_) => {}
        }
    }
    Ok(())
}

fn validate_mesh_images(images: &[runtime_resources::image::Image]) -> Result<(), ogpu_core::error::Error> {
    for image in images {
        runtime_resources::image::gpu_snapshot(image)?;
    }
    Ok(())
}

struct Release(Vec<scene::node>);

impl Drop for Release {
    fn drop(&mut self) {
        layer::release(&self.0);
    }
}

struct Cancel(Option<rr::Submission>);

impl Drop for Cancel {
    fn drop(&mut self) {
        if let Some(submission) = self.0.take() {
            native::cancel(&submission);
        }
    }
}

enum outcome {
    Replayed,
    Prepared(rr::Submission, Option<(String, i64)>, (f64, f64, f64, f64), Vec<rr::Batch>),
}

fn lower_all(
    execution: &rr::Renderer,
    density: i64,
    staged: &layer::staged_native,
    submission: &rr::Submission,
) -> Result<Vec<rr::Batch>, error> {
    let mut batches = Vec::new();
    for layer in &staged.layers {
        let batch = match layer {
            layer::native_layer::Scene2_layer(ir, resources) => {
                native::lower_scene2(submission, density, &|id| resources.iter().find(|(i, _)| *i == id).map(|(_, r)| r.clone()), ir)
            }
            layer::native_layer::Scene2_segment(segment, resources) => {
                let cacheable = resources.iter().all(|(_, r)| matches!(r, rr::Resource::Text(_)));
                let version = layer::native_segment_version(segment, resources);
                let ir = scene::segment_ir(segment).map_err(error::Stage)?;
                native::lower_scene2_segment(
                    submission,
                    display_list::id(segment),
                    version,
                    cacheable,
                    density,
                    &|id| resources.iter().find(|(i, _)| *i == id).map(|(_, r)| r.clone()),
                    &ir,
                )
            }
            layer::native_layer::Ui_layer(ui, resources) => {
                native::lower_ui(submission, density, &|id| resources.iter().find(|(i, _)| *i == id).map(|(_, r)| r.clone()), ui)
            }
            layer::native_layer::Scene3_layer(prepared) => {
                sweep_retained(&native::gpu_vertices_owner(execution));
                let draws = draws_of_prepared(staged, prepared);
                rr::adopt_retained_view(submission, density, prepared, draws)
            }
        };
        batches.push(batch.map_err(error::Lower)?);
    }
    Ok(batches)
}

pub fn render(execution: &rr::Renderer, density: i64, width: i64, height: i64, scene: &[scene::node]) -> Result<(), error> {
    let mut cancel = Cancel(None);
    let prepared: Result<outcome, error> = {
        let _release = Release(scene.to_vec());
        match layer::stage_native_render_checked(Some(density), width, height, scene) {
            Err(layer::native_error::Message(message)) => Err(error::Stage(message)),
            Err(layer::native_error::Resource(e)) => Err(error::Resource(e)),
            Ok(staged) => {
                let replayed: Result<Option<()>, error> = match validate_layers(&staged.layers).and_then(|()| validate_mesh_images(&staged.mesh_images)) {
                    Err(e) => Err(error::Resource(e)),
                    Ok(()) => match &staged.retained {
                        None => Ok(None),
                        Some((identity, version)) => native::replay(Some(staged.clear), identity, *version, execution).map_err(error::Step),
                    },
                };
                match replayed {
                    Err(e) => Err(e),
                    Ok(Some(())) => Ok(outcome::Replayed),
                    Ok(None) => match rr::begin_submission(execution) {
                        Err(e) => Err(error::Begin(e)),
                        Ok(submission) => {
                            cancel.0 = Some(submission.clone());
                            lower_all(execution, density, &staged, &submission)
                                .map(|batches| outcome::Prepared(submission, staged.retained.clone(), staged.clear, batches))
                        }
                    },
                }
            }
        }
    };
    if prepared.is_err() {
        drop(cancel);
    } else {
        cancel.0 = None;
    }
    match prepared? {
        outcome::Replayed => Ok(()),
        outcome::Prepared(submission, retained, clear, batches) => {
            let stepped = match &retained {
                None => native::step(Some(clear), None, None, &submission, &batches),
                Some((identity, version)) => native::step(Some(clear), Some(identity), Some(*version), &submission, &batches),
            };
            stepped.map_err(error::Step)
        }
    }
}
