//! Select exactly one backend per thread at startup; an unselected backend returns a typed error.
use crate::{
    backend::Driver,
    error::{self, Error, ErrorKind},
};
use std::cell::Cell;

pub use crate::backend::Implementation as Impl;

thread_local! {
    static SELECTED: Cell<Option<Impl>> = const { Cell::new(None) };
}

pub fn select(implementation: Impl) {
    SELECTED.with(|cell| cell.set(Some(implementation)));
}

fn selected(operation: &str) -> Result<Impl, Error> {
    SELECTED.with(Cell::get).ok_or_else(|| {
        error::make(operation, ErrorKind::NoAdapter, "no OGPU implementation selected")
    })
}

pub fn backend() -> Result<String, Error> {
    Ok(selected("Ogpu.Impl.backend")?.backend.to_owned())
}

pub fn create_driver() -> Result<Box<dyn Driver>, Error> {
    Ok((selected("Ogpu.Impl.create_driver")?.create_driver)())
}

pub fn live_handles() -> Result<i64, Error> {
    Ok((selected("Ogpu.Impl.live_handles")?.live_handles)())
}
