//! Owned RGBA images: a sketch-level handle over `runtime_resources::Image`.
use rays_math::color;
use runtime_resources::image as Resource;
use std::cell::Cell;
use std::rc::Rc;

pub struct image_data {
    resource: Resource::Image,
    width: Cell<i64>,
    height: Cell<i64>,
}

/// Handle semantics: clones share one image; `same` is OCaml's physical equality.
#[derive(Clone)]
pub struct Image(Rc<image_data>);

pub fn same(a: &Image, b: &Image) -> bool {
    Rc::ptr_eq(&a.0, &b.0)
}

pub(crate) fn message(operation: &str, error: &runtime_resources::error) -> String {
    format!("{operation}: {}", ogpu_core::error::to_string(error))
}

pub fn of_resource(resource: Resource::Image) -> Result<Image, String> {
    let (width, height) = Resource::size(&resource).map_err(|e| message("Image.of_resource", &e))?;
    Ok(Image(Rc::new(image_data { resource, width: Cell::new(width), height: Cell::new(height) })))
}

pub fn load(path: &str) -> Result<Image, String> {
    match Resource::load_file(path) {
        Ok(resource) => of_resource(resource),
        Err(error) => Err(message("Image.load", &error)),
    }
}

/// `load`, failing with the message (OCaml's `load_exn` raised `Failure`).
pub fn load_exn(path: &str) -> Result<Image, String> {
    load(path)
}

pub fn create(width: i64, height: i64, color: Option<color::T>) -> Result<Image, String> {
    let color = color.unwrap_or_else(color::transparent);
    const MAX_INT: i64 = (1 << 62) - 1;
    if width <= 0 || height <= 0 || width > MAX_INT / 4 || height > MAX_INT / (width * 4) {
        return Err("Image.create: invalid image extent".to_string());
    }
    let pixel = [color.r as u8, color.g as u8, color.b as u8, color.a as u8];
    let mut rgba = Vec::with_capacity((width * height * 4) as usize);
    for _ in 0..(width * height) {
        rgba.extend_from_slice(&pixel);
    }
    match Resource::create(width, height, &rgba) {
        Ok(resource) => Ok(Image(Rc::new(image_data { resource, width: Cell::new(width), height: Cell::new(height) }))),
        Err(error) => Err(message("Image.create", &error)),
    }
}

/// Create or update an RGBA image. Updating preserves the image identity.
pub fn upload_rgba(into: Option<&Image>, width: i64, height: i64, rgba: &[u8]) -> Result<Image, String> {
    match into {
        None => match Resource::create(width, height, rgba) {
            Ok(resource) => Ok(Image(Rc::new(image_data { resource, width: Cell::new(width), height: Cell::new(height) }))),
            Err(error) => Err(message("Image.upload_rgba", &error)),
        },
        Some(image) => {
            Resource::replace(&image.0.resource, width, height, rgba).map_err(|e| message("Image.upload_rgba", &e))?;
            image.0.width.set(width);
            image.0.height.set(height);
            Ok(image.clone())
        }
    }
}

pub fn destroy(image: &Image) -> Result<(), String> {
    Resource::destroy(&image.0.resource).map_err(|e| message("Image.destroy", &e))
}

pub fn get_size(image: &Image) -> Result<(i64, i64), String> {
    Resource::size(&image.0.resource).map_err(|e| message("Image.get_size", &e))
}

pub fn get_width(image: &Image) -> Result<i64, String> {
    get_size(image).map(|s| s.0)
}

pub fn get_height(image: &Image) -> Result<i64, String> {
    get_size(image).map(|s| s.1)
}

pub fn replace(target: &Image, source: &Image) -> Result<(), String> {
    match Resource::replace_owned(&target.0.resource, &source.0.resource) {
        Ok(()) => {
            target.0.width.set(source.0.width.get());
            target.0.height.set(source.0.height.get());
            Ok(())
        }
        Err(error) => Err(message("Image.replace", &error)),
    }
}

pub fn identity(image: &Image) -> i64 {
    Resource::identity(&image.0.resource)
}

pub fn pixels(image: &Image) -> Result<Vec<u8>, String> {
    Resource::pixels(&image.0.resource).map_err(|e| message("Image.pixels", &e))
}

pub fn resource(image: &Image) -> Resource::Image {
    image.0.resource.clone()
}

pub fn dimensions(image: &Image) -> (i64, i64) {
    match get_size(image) {
        Ok(size) => size,
        Err(_) => (image.0.width.get(), image.0.height.get()),
    }
}
