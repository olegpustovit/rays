//! Ogpu_metal.Library: a compiled Metal source library with a count of the pipelines made from it.
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    shader::{self, Shader},
};
use std::{cell::Cell, rc::Rc};

struct LibraryInner {
    metal: native::Library,
    device: Device,
    handle: ResourceHandle,
    pipelines: Cell<i64>,
    dead: Cell<bool>,
}

#[derive(Clone)]
pub struct Library(Rc<LibraryInner>);

pub fn compile_native(operation: &str, device: &Device, shader_value: &Shader) -> Result<native::Library, Error> {
    if shader::backend(shader_value) != "metal" {
        return Err(fail(operation, ErrorKind::InvalidArgument, "shader backend is not metal"));
    }
    let source = match std::str::from_utf8(shader::bytes(shader_value)) {
        Ok(text) => text,
        Err(_) => return Err(fail(operation, ErrorKind::InvalidArgument, "shader source is not UTF-8")),
    };
    native::Library::compile_source(device.metal(), source, shader::label(shader_value))
}

pub fn create(device: &Device, shader_value: &Shader) -> Result<Library, Error> {
    let op = "Ogpu_metal.Library.create";
    let metal = compile_native(op, device, shader_value)?;
    device.attach_resource();
    Ok(Library(Rc::new(LibraryInner {
        metal,
        device: device.clone(),
        handle: handle::create(device.handle()),
        pipelines: Cell::new(0),
        dead: Cell::new(false),
    })))
}

impl Library {
    pub fn destroy(&self) -> Result<(), Error> {
        let op = "Ogpu_metal.Library.destroy";
        if self.0.dead.get() {
            Ok(())
        } else if self.0.pipelines.get() > 0 {
            Err(fail(op, ErrorKind::InvalidState, "library has live pipelines"))
        } else {
            self.0.metal.destroy()?;
            self.0.dead.set(true);
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &native::Library {
        &self.0.metal
    }
    pub fn attach_pipeline(&self) {
        self.0.pipelines.set(self.0.pipelines.get() + 1);
    }
    pub fn detach_pipeline(&self) {
        self.0.pipelines.set(self.0.pipelines.get() - 1);
    }
}
