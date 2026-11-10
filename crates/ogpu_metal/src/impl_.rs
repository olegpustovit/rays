//! The Metal implementation of `ogpu_core::impl_`.
use ogpu_core::backend::Implementation as Impl;

pub use crate::backend::{create as create_driver, live_handles};

pub const BACKEND: &str = "metal";

/// The record `ogpu_core::impl_::select` takes.
pub fn implementation() -> Impl {
    Impl { backend: BACKEND, create_driver, live_handles }
}
