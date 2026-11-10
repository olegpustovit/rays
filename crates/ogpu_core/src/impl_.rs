//! Select exactly one backend per thread at startup; an unselected backend returns a typed error.
use crate::{
    backend::Driver,
    error::{self, Error, ErrorKind},
};
use std::{cell::RefCell, rc::Rc};

pub use crate::backend::Implementation as Impl;

thread_local! {
    static SELECTED: RefCell<Option<Rc<Impl>>> = const { RefCell::new(None) };
}

pub fn select(implementation: Impl) {
    SELECTED.with(|cell| *cell.borrow_mut() = Some(Rc::new(implementation)));
}

fn selected(operation: &str) -> Result<Rc<Impl>, Error> {
    SELECTED.with(|cell| cell.borrow().clone()).ok_or_else(|| {
        error::make(operation, ErrorKind::NoAdapter, "no OGPU implementation selected")
    })
}

pub fn backend() -> Result<String, Error> {
    Ok(selected("Ogpu.Impl.backend")?.backend.clone())
}

pub fn create_driver() -> Result<Box<dyn Driver>, Error> {
    Ok((selected("Ogpu.Impl.create_driver")?.create_driver)())
}

pub fn live_handles() -> Result<i64, Error> {
    Ok((selected("Ogpu.Impl.live_handles")?.live_handles)())
}
