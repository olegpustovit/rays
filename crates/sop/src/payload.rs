//! the three cooked payload kinds.
use rays_diagnostic::Diagnostic;

use crate::{image, kernel};

#[derive(Clone)]
pub enum T {
    Geometry(rdk_core::geometry::T),
    Image(image::T),
    Kernel(kernel::T),
}

pub fn data_id(value: &T) -> i64 {
    match value {
        T::Geometry(geometry) => rdk_core::geometry::data_id(geometry),
        T::Image(image) => image::data_id(image),
        T::Kernel(kernel) => kernel::data_id(kernel),
    }
}

pub fn payload_components(value: &T) -> Vec<(i64, i64)> {
    match value {
        T::Geometry(geometry) => rdk_core::geometry::payload_components(geometry),
        T::Image(image) => vec![(image::data_id(image), image::payload_bytes(image))],
        T::Kernel(kernel) => vec![(kernel::data_id(kernel), kernel::payload_bytes(kernel))],
    }
}

pub fn element_count(value: &T) -> i64 {
    match value {
        T::Geometry(geometry) => rdk_core::geometry::point_count(geometry),
        T::Image(image) => image::width(image) * image::height(image),
        T::Kernel(_) => 0,
    }
}

/// Payload identity requires the same variant and shared data ID.
pub fn ptr_eq(a: &T, b: &T) -> bool {
    match (a, b) {
        (T::Geometry(x), T::Geometry(y)) => rdk_core::geometry::data_id(x) == rdk_core::geometry::data_id(y),
        (T::Image(x), T::Image(y)) => image::ptr_eq(x, y),
        (T::Kernel(x), T::Kernel(y)) => kernel::data_id(x) == kernel::data_id(y),
        _ => false,
    }
}

fn wrong(expected: &str) -> Diagnostic {
    rays_diagnostic::error(
        "E_PAYLOAD".to_owned(),
        format!("Expected a cooked {expected} payload."),
        None,
        None,
        None,
        Vec::new(),
        Vec::new(),
    )
}

pub fn geometry(value: &T) -> Result<rdk_core::geometry::T, Diagnostic> {
    match value {
        T::Geometry(geometry) => Ok(geometry.clone()),
        _ => Err(wrong("geometry")),
    }
}
pub fn image(value: &T) -> Result<image::T, Diagnostic> {
    match value {
        T::Image(image) => Ok(image.clone()),
        _ => Err(wrong("image")),
    }
}
pub fn kernel(value: &T) -> Result<kernel::T, Diagnostic> {
    match value {
        T::Kernel(kernel) => Ok(kernel.clone()),
        _ => Err(wrong("kernel")),
    }
}
