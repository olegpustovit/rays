//! a deferred per-element computation carried as a payload.
use std::sync::Arc;

use rays_diagnostic::Diagnostic;

use crate::context;

#[derive(Clone, Debug, PartialEq)]
pub enum Column {
    Floats(Vec<f64>),
    Vec3s(Vec<f64>),
}

pub type Runner = Box<dyn Fn(&context::T) -> Result<Column, Diagnostic> + Send + Sync>;
pub type Prepare = Arc<dyn Fn(&[Column]) -> Result<Runner, Diagnostic> + Send + Sync>;
pub type GeometryCapture = Arc<dyn Fn(i64) -> Option<rdk_core::geometry::T> + Send + Sync>;

#[derive(Clone)]
pub struct T {
    id: i64,
    bytes: i64,
    prepare: Prepare,
    geometry_capture: GeometryCapture,
}

/// `geometry_capture` defaults to `|_| None` when absent.
pub fn create(geometry_capture: Option<GeometryCapture>, payload_bytes: i64, prepare: Prepare) -> T {
    assert!(payload_bytes >= 0, "Kernel.create: negative payload size");
    T {
        id: rdk_core::geometry::fresh_data_id(),
        bytes: payload_bytes,
        prepare,
        geometry_capture: geometry_capture.unwrap_or_else(|| Arc::new(|_| None)),
    }
}

pub fn data_id(t: &T) -> i64 {
    t.id
}
pub fn payload_bytes(t: &T) -> i64 {
    t.bytes
}
pub fn prepare(t: &T) -> &Prepare {
    &t.prepare
}
pub fn geometry_capture(t: &T) -> &GeometryCapture {
    &t.geometry_capture
}

pub fn remap_geometry_captures(sources: &[(i64, i64)], t: &T) -> T {
    if sources.is_empty() {
        return t.clone();
    }
    let sources = sources.to_vec();
    let inner = Arc::clone(&t.geometry_capture);
    T {
        geometry_capture: Arc::new(move |id| {
            sources.iter().find(|(_, mapped)| id == *mapped).and_then(|(original, _)| inner(*original))
        }),
        ..t.clone()
    }
}
