//! Offscreen canvases: a CPU-readable RGBA surface rendered through a reusable native target.
use crate::canvas_runtime;
use crate::image;
use crate::native_scene_lowering;
use crate::scene::{self, Layer};
use crate::world;
use ogpu_core::backend::Texture;
use rays_math::color;
use renderer::renderer as rr;
use runtime_resources::Canvas as RCanvas;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

pub struct canvas_data {
    resource: RCanvas::t,
    execution: RefCell<Option<rr::Renderer>>,
    destroyed: Cell<bool>,
    caches: Layer::cache_context,
    world_caches: world::Private::cache_context,
    native_caches: native_scene_lowering::cache_context,
    epoch: Cell<i64>,
    completed: Cell<bool>,
}

/// A canvas handle: clones share one canvas.
#[derive(Clone)]
pub struct t(Rc<canvas_data>);

fn message(operation: &str, error: &runtime_resources::error) -> String {
    format!("{operation}: {}", ogpu_core::error::to_string(error))
}

fn renderer_message(operation: &str, error: &ogpu_core::error::Error) -> String {
    format!("{operation}: {}", ogpu_core::error::to_string(error))
}

pub fn create(width: i64, height: i64) -> Result<t, String> {
    match RCanvas::create(width, height) {
        Ok(resource) => Ok(t(Rc::new(canvas_data {
            resource,
            execution: RefCell::new(None),
            destroyed: Cell::new(false),
            caches: Layer::create_cache_context(),
            world_caches: world::Private::create_cache_context(),
            native_caches: native_scene_lowering::create_cache_context(),
            epoch: Cell::new(0),
            completed: Cell::new(false),
        }))),
        Err(error) => Err(message("Canvas.create", &error)),
    }
}

pub fn size(value: &t) -> Result<(i64, i64), String> {
    RCanvas::size(&value.0.resource).map_err(|e| message("Canvas.size", &e))
}

/// Forget the completed GPU frame: the next read goes back to CPU storage.
pub fn invalidate(value: &t) -> Result<(), String> {
    size(value)?;
    value.0.epoch.set(value.0.epoch.get() + 1);
    value.0.completed.set(false);
    Ok(())
}

fn execution(density: i64, value: &t) -> Result<(), String> {
    if value.0.destroyed.get() {
        return Err("Canvas.render: canvas is destroyed".to_string());
    }
    if value.0.execution.borrow().is_some() {
        return Ok(());
    }
    let (width, height) = size(value)?;
    let configuration = rr::Configuration {
        logical_width: width / density,
        logical_height: height / density,
        drawable_width: width,
        drawable_height: height,
        title: "Rays Canvas".to_string(),
        vsync: false,
        high_density: true,
    };
    let execution_ = rr::create_offscreen(&configuration).map_err(|e| renderer_message("Canvas.render", &e))?;
    *value.0.execution.borrow_mut() = Some(execution_);
    Ok(())
}

fn render_uncached(density: i64, value: &t, scene_: &[scene::node]) -> Result<(), String> {
    invalidate(value)?;
    if density < 1 {
        return Err("Canvas.render: density must be positive".to_string());
    }
    execution(density, value)?;
    let (width, height) = size(value)?;
    let execution_ = value.0.execution.borrow().clone().ok_or_else(|| "Canvas.render: no execution".to_string())?;
    native_scene_lowering::render(&execution_, density, width / density, height / density, scene_)
        .map_err(|e| format!("Canvas.render: {}", native_scene_lowering::error_to_string(&e)))?;
    let texture = rr::offscreen_target(&execution_).map_err(|e| renderer_message("Canvas.render", &e))?;
    RCanvas::publish_gpu(&value.0.resource, &texture).map_err(|e| message("Canvas.render", &e))?;
    value.0.completed.set(true);
    Ok(())
}

/// Replace the canvas contents through a reusable, layerless native target owned by the canvas.
/// `density` (default 1) renders the scene at that many canvas pixels a logical point, as a
/// Retina window does: the scene is `width / density` by `height / density` points. A canvas
/// keeps the density of its first render.
pub fn render(density: Option<i64>, value: &t, scene_: &[scene::node]) -> Result<(), String> {
    let density = density.unwrap_or(1);
    let mut result: Result<(), String> = Ok(());
    Layer::with_cache_context(&value.0.caches, &mut || {
        world::Private::with_cache_context(&value.0.world_caches, &mut || {
            native_scene_lowering::with_cache_context(&value.0.native_caches, &mut || {
                result = render_uncached(density, value, scene_);
            });
        });
    });
    result
}

fn snapshot(value: &t) -> Result<Vec<u8>, String> {
    let image = RCanvas::capture(&value.0.resource).map_err(|e| message("Canvas.capture", &e))?;
    let pixels = runtime_resources::Image::pixels(&image).map_err(|e| message("Canvas.pixels", &e));
    let _ = runtime_resources::Image::destroy(&image);
    pixels
}

fn color_at(bytes: &[u8], offset: usize) -> color::T {
    color::rgba(bytes[offset] as i64, bytes[offset + 1] as i64, bytes[offset + 2] as i64, bytes[offset + 3] as i64)
}

pub fn pixel(value: &t, x: i64, y: i64) -> Result<Option<color::T>, String> {
    let (w, h) = size(value)?;
    if x < 0 || y < 0 || x >= w || y >= h {
        return Ok(None);
    }
    let bytes = snapshot(value)?;
    Ok(Some(color_at(&bytes, (((y * w) + x) * 4) as usize)))
}

pub fn pixels(value: &t) -> Result<Vec<color::T>, String> {
    let (w, h) = size(value)?;
    let bytes = snapshot(value)?;
    Ok((0..(w * h) as usize).map(|index| color_at(&bytes, index * 4)).collect())
}

pub fn to_image(value: &t) -> Result<image::t, String> {
    match RCanvas::capture(&value.0.resource) {
        Ok(img) => image::of_resource(img),
        Err(error) => Err(message("Canvas.to_image", &error)),
    }
}

pub fn pixel_stats(value: &t) -> (i64, i64) {
    RCanvas::pixel_stats(&value.0.resource)
}

/// Borrow the completed GPU frame until another render attempt, explicit invalidation, CPU
/// mutation or destruction. Requires the initial thread.
pub fn gpu_source(value: &t) -> Result<(i64, i64, Box<dyn Fn() -> Option<Texture>>), String> {
    RCanvas::size(&value.0.resource).map_err(|e| message("Canvas.gpu_source", &e))?;
    if value.0.destroyed.get() || !value.0.completed.get() {
        return Err("Canvas.gpu_source: no completed GPU frame".to_string());
    }
    match RCanvas::gpu_snapshot(&value.0.resource) {
        None => Err("Canvas.gpu_source: no completed GPU frame".to_string()),
        Some((width, height, generation, texture)) => {
            let epoch = value.0.epoch.get();
            let canvas = value.clone();
            Ok((
                width,
                height,
                Box::new(move || {
                    if canvas.0.destroyed.get() || !canvas.0.completed.get() || canvas.0.epoch.get() != epoch {
                        return None;
                    }
                    match RCanvas::gpu_snapshot(&canvas.0.resource) {
                        Some((_, _, current, source)) if current == generation && ogpu_core::backend::texture_id(&source) == ogpu_core::backend::texture_id(&texture) => {
                            Some(texture.clone())
                        }
                        _ => None,
                    }
                }),
            ))
        }
    }
}

#[allow(non_snake_case)]
pub mod Private {
    use super::*;

    pub type native_stats = rr::Stats;

    pub fn native_stats(value: &t) -> Result<rr::Stats, String> {
        match value.0.execution.borrow().as_ref() {
            None => Ok(rr::ZERO_STATS),
            Some(execution) => rr::stats(execution).map_err(|e| renderer_message("Canvas.Private.native_stats", &e)),
        }
    }
}

/// Releases the texture staging its renderer keeps while frame after frame uploads, which the
/// next upload makes again: a settled reading of the canvas then does not depend on the last two
/// frames.
pub fn release_staging(value: &t) -> Result<(), String> {
    match value.0.execution.borrow().as_ref() {
        None => Ok(()),
        Some(execution) => rr::release_staging(execution).map_err(|e| renderer_message("Canvas.release_staging", &e)),
    }
}

/// The device's GPU time so far, summed over completed command buffers: the difference across a
/// `render` is that frame's GPU time.
pub fn gpu_seconds(value: &t) -> Result<f64, String> {
    Private::native_stats(value).map(|s| s.gpu_duration_seconds)
}

pub fn save_png(value: &t, path: &str) -> Result<(), String> {
    runtime_resources::write_atomic(path, &|temporary| {
        RCanvas::save_png(&value.0.resource, temporary).map_err(|e| message("Canvas.save_png", &e))
    })
}

pub fn save_screen_png(path: &str) -> Result<(), String> {
    canvas_runtime::save(path)
}

pub fn destroy(value: &t) -> Result<(), String> {
    if value.0.destroyed.get() {
        return Ok(());
    }
    size(value)?;
    value.0.epoch.set(value.0.epoch.get() + 1);
    value.0.completed.set(false);
    let execution = value.0.execution.borrow().clone();
    if let Some(execution) = execution {
        rr::destroy(&execution).map_err(|e| renderer_message("Canvas.destroy", &e))?;
    }
    *value.0.execution.borrow_mut() = None;
    RCanvas::destroy(&value.0.resource).map_err(|e| message("Canvas.destroy", &e))?;
    Layer::clear_cache_context(&value.0.caches);
    value.0.destroyed.set(true);
    Ok(())
}
