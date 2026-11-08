//! The mock implementation of `ogpu_core::impl_`.
use crate::backend_mock::{self, Control};
use ogpu_core::backend::{Driver, Implementation as Impl};
use std::cell::RefCell;

thread_local! {
    // ponytail: controls are kept for the thread's lifetime; they are a few counters each
    static CONTROLS: RefCell<Vec<Control>> = const { RefCell::new(Vec::new()) };
}

pub const BACKEND: &str = "mock";

pub fn create_driver() -> Box<dyn Driver> {
    let (driver, control) = backend_mock::create();
    CONTROLS.with(|controls| controls.borrow_mut().push(control));
    driver
}

/// Native handles still alive, summed over every driver created on this thread.
pub fn live_handles() -> i64 {
    CONTROLS.with(|controls| {
        controls
            .borrow()
            .iter()
            .map(|control| {
                let (buffers, textures, pipelines, queues, surfaces) = backend_mock::live_counts(control);
                buffers + textures + pipelines + queues + surfaces
            })
            .sum()
    })
}

/// The record `ogpu_core::impl_::select` takes.
pub fn implementation() -> Impl {
    Impl {
        backend: BACKEND.to_owned(),
        create_driver: Box::new(create_driver),
        live_handles: Box::new(live_handles),
    }
}
