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
pub enum Error {
    Stage(String),
    Resource(ogpu_core::error::Error),
    Begin(ogpu_core::error::Error),
    Lower(ogpu_core::error::Error),
    Step(ogpu_core::error::Error),
}

pub(crate) fn pp_error(formatter: &mut dyn std::fmt::Write, e: &Error) {
    let text = |e: &ogpu_core::error::Error| ogpu_core::error::to_string(e);
    let _ = match e {
        Error::Stage(message) => write!(formatter, "native Scene staging: {message}"),
        Error::Resource(e) => write!(formatter, "native Scene resource: {}", text(e)),
        Error::Begin(e) => write!(formatter, "native Scene submission setup: {}", text(e)),
        Error::Lower(e) => write!(formatter, "native Scene lowering: {}", text(e)),
        Error::Step(e) => write!(formatter, "native Scene submission: {}", text(e)),
    };
}

pub(crate) fn error_to_string(e: &Error) -> String {
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

struct RetainedDraws {
    /// a frame with several 3D layers caches each (held so the cached draws keep their layer alive)
    _layer: Rc<se::PreparedScene3>,
    draws: Vec<rr::Draw>,
}

const RETAINED_DRAW_CAPACITY: i64 = 16;

#[derive(Clone)]
pub struct CacheContext(Rc<RefCell<IdList>>);

thread_local! {
    static CURRENT: RefCell<Option<CacheContext>> = const { RefCell::new(None) };
}

pub(crate) fn create_cache_context() -> CacheContext {
    // ponytail: 16 immutable layers keyed by identity; add stable IDs if this cache grows.
    CacheContext(Rc::new(RefCell::new(idlist::create(
        lru::cache("rays.retained_draws"),
        RETAINED_DRAW_CAPACITY,
        Box::new(|a, b| match (a.downcast_ref::<se::PreparedScene3>(), b.downcast_ref::<se::PreparedScene3>()) {
            (Some(x), Some(y)) => std::ptr::eq(x, y),
            _ => false,
        }),
        None,
    ))))
}

struct Restore(Option<CacheContext>);

impl Drop for Restore {
    fn drop(&mut self) {
        let previous = self.0.take();
        CURRENT.with(|c| *c.borrow_mut() = previous);
    }
}

pub(crate) fn with_cache_context(context: &CacheContext, f: &mut dyn FnMut()) {
    let previous = CURRENT.with(|c| c.borrow_mut().replace(context.clone()));
    let _restore = Restore(previous);
    f()
}

fn current() -> Option<CacheContext> {
    CURRENT.with(|c| c.borrow().clone())
}

/// A layer whose borrowed stream went is never drawn again; it does not wait for eviction.
fn sweep_retained(owner: &se::GpuVerticesOwner) {
    if let Some(context) = current() {
        let mut stale: Vec<Item> = Vec::new();
        idlist::iter(&mut context.0.borrow_mut(), &mut |layer, _| {
            if let Some(prepared) = layer.downcast_ref::<se::PreparedScene3>()
                && se::borrows_unregistered(owner, prepared) {
                    stale.push(layer.clone());
                }
        });
        for layer in stale {
            idlist::remove(&mut context.0.borrow_mut(), &layer);
        }
    }
}

pub(crate) fn draws_of_prepared(staged: &layer::StagedNative, prepared: &Rc<se::PreparedScene3>) -> Vec<rr::Draw> {
    let direct = || prepared.entries.iter().map(draw_of_scene3_entry).collect::<Vec<_>>();
    if staged.retained.is_none() {
        return direct();
    }
    let Some(context) = current() else {
        return direct();
    };
    let key: Item = prepared.clone();
    let found = idlist::find(&mut context.0.borrow_mut(), &key).and_then(|v| v.downcast::<RetainedDraws>().ok());
    if let Some(cached) = found {
        return cached.draws.clone();
    }
    let draws = direct();
    // no bytes: draws over a layer the scene's own cache charges
    idlist::add(&mut context.0.borrow_mut(), 0, key, Rc::new(RetainedDraws { _layer: prepared.clone(), draws: draws.clone() }));
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

fn validate_layers(layers: &[layer::NativeLayer]) -> Result<(), ogpu_core::error::Error> {
    for layer in layers {
        match layer {
            layer::NativeLayer::Scene2Layer(_, r) | layer::NativeLayer::Scene2Segment(_, r) => validate_images(r)?,
            layer::NativeLayer::Scene3Layer(_) | layer::NativeLayer::UiLayer(_) => {}
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

struct Release(Vec<scene::Node>);

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

enum Outcome {
    Replayed,
    Prepared(rr::Submission, Option<(String, i64)>, (f64, f64, f64, f64), Vec<rr::Batch>),
}

fn lower_all(
    execution: &rr::Renderer,
    density: i64,
    staged: &layer::StagedNative,
    submission: &rr::Submission,
) -> Result<Vec<rr::Batch>, Error> {
    let mut batches = Vec::new();
    for layer in &staged.layers {
        let batch = match layer {
            layer::NativeLayer::Scene2Layer(ir, resources) => {
                native::lower_scene2(submission, density, &|id| resources.iter().find(|(i, _)| *i == id).map(|(_, r)| r.clone()), ir)
            }
            layer::NativeLayer::Scene2Segment(segment, resources) => {
                let cacheable = resources.is_empty();
                let version = layer::native_segment_version(segment, resources);
                let ir = scene::segment_ir(segment).map_err(Error::Stage)?;
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
            layer::NativeLayer::UiLayer(ui) => native::ui_draw(submission, ui.clone()),
            layer::NativeLayer::Scene3Layer(prepared) => {
                sweep_retained(&native::gpu_vertices_owner(execution));
                let draws = draws_of_prepared(staged, prepared);
                rr::adopt_retained_view(submission, density, prepared, draws)
            }
        };
        batches.push(batch.map_err(Error::Lower)?);
    }
    Ok(batches)
}

pub(crate) fn render(execution: &rr::Renderer, density: i64, width: i64, height: i64, scene: &[scene::Node]) -> Result<(), Error> {
    let mut cancel = Cancel(None);
    let prepared: Result<Outcome, Error> = {
        let _release = Release(scene.to_vec());
        match layer::stage_native_render_checked(Some(density), width, height, scene) {
            Err(layer::NativeError::Message(message)) => Err(Error::Stage(message)),
            Err(layer::NativeError::Resource(e)) => Err(Error::Resource(e)),
            Ok(staged) => {
                let replayed: Result<Option<()>, Error> = match validate_layers(&staged.layers).and_then(|()| validate_mesh_images(&staged.mesh_images)) {
                    Err(e) => Err(Error::Resource(e)),
                    Ok(()) => match &staged.retained {
                        None => Ok(None),
                        Some((identity, version)) => native::replay(Some(staged.clear), identity, *version, execution).map_err(Error::Step),
                    },
                };
                match replayed {
                    Err(e) => Err(e),
                    Ok(Some(())) => Ok(Outcome::Replayed),
                    Ok(None) => match rr::begin_submission(execution) {
                        Err(e) => Err(Error::Begin(e)),
                        Ok(submission) => {
                            cancel.0 = Some(submission.clone());
                            lower_all(execution, density, &staged, &submission)
                                .map(|batches| Outcome::Prepared(submission, staged.retained.clone(), staged.clear, batches))
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
        Outcome::Replayed => Ok(()),
        Outcome::Prepared(submission, retained, clear, batches) => {
            let stepped = match &retained {
                None => native::step(Some(clear), None, None, &submission, &batches),
                Some((identity, version)) => native::step(Some(clear), Some(identity), Some(*version), &submission, &batches),
            };
            stepped.map_err(Error::Step)
        }
    }
}
