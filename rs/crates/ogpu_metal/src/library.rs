//! Ogpu_metal.Library: a compiled Metal source library with a count of the pipelines made from it.
use crate::device::{fail, native_of, Device, R};
use ogpu_core::{
    error::ErrorKind,
    handle::{self, ResourceHandle},
    shader::{self, Shader},
};
use std::{cell::Cell, rc::Rc};

struct LibraryInner {
    metal: metal::Library,
    device: Device,
    handle: ResourceHandle,
    pipelines: Cell<i64>,
    dead: Cell<bool>,
}

#[derive(Clone)]
pub struct Library(Rc<LibraryInner>);

pub fn compile_native(operation: &str, device: &Device, shader_value: &Shader) -> R<metal::Library> {
    if shader::backend(shader_value) != "metal" {
        return fail(operation, ErrorKind::InvalidArgument, "shader backend is not metal");
    }
    let source = match std::str::from_utf8(shader::bytes(shader_value)) {
        Ok(text) => text,
        Err(_) => return fail(operation, ErrorKind::InvalidArgument, "shader source is not UTF-8"),
    };
    native_of(operation, metal::Library::compile_source(device.metal(), source, shader::label(shader_value)))
}

pub fn create(device: &Device, shader_value: &Shader) -> R<Library> {
    let op = "Ogpu_metal.Library.create";
    if device.destroyed() {
        return fail(op, ErrorKind::StaleHandle, "device is destroyed");
    }
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
    pub fn validate(&self, device: &Device) -> R<()> {
        handle::validate_for("Ogpu_metal.Library.validate", device.handle(), &self.0.handle)
    }

    pub fn destroy(&self) -> R<()> {
        let op = "Ogpu_metal.Library.destroy";
        if self.0.dead.get() {
            Ok(())
        } else if self.0.pipelines.get() > 0 {
            fail(op, ErrorKind::InvalidState, "library has live pipelines")
        } else {
            native_of(op, self.0.metal.destroy())?;
            self.0.dead.set(true);
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &metal::Library {
        &self.0.metal
    }
    pub fn attach_pipeline(&self) {
        self.0.pipelines.set(self.0.pipelines.get() + 1);
    }
    pub fn detach_pipeline(&self) {
        self.0.pipelines.set(self.0.pipelines.get() - 1);
    }
}
