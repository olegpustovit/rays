//! The Metal implementation of `ogpu::impl_`.
use crate::backend;
use ogpu::impl_::Impl;
use ogpu_core::backend::Driver;

pub const BACKEND: &str = "metal";

pub fn create_driver() -> Driver {
    backend::create()
}

/// Native handles still alive on this thread (the release queue drained first).
pub fn live_handles() -> i64 {
    backend::live_handles()
}

/// The record `ogpu::impl_::select` takes.
pub fn implementation() -> Impl {
    Impl {
        backend: BACKEND.to_owned(),
        create_driver: Box::new(create_driver),
        live_handles: Box::new(live_handles),
    }
}
