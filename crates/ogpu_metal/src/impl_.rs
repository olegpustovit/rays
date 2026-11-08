//! The Metal implementation of `ogpu_core::impl_`.
use crate::backend;
use ogpu_core::backend::{Driver, Implementation as Impl};

pub const BACKEND: &str = "metal";

pub fn create_driver() -> Box<dyn Driver> {
    backend::create()
}

/// Native handles still alive on this thread (the release queue drained first).
pub fn live_handles() -> i64 {
    backend::live_handles()
}

/// The record `ogpu_core::impl_::select` takes.
pub fn implementation() -> Impl {
    Impl {
        backend: BACKEND.to_owned(),
        create_driver: Box::new(create_driver),
        live_handles: Box::new(live_handles),
    }
}
