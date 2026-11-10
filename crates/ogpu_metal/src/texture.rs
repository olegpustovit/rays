//! Ogpu_metal.Texture
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
};
use std::{cell::Cell, rc::Rc};

struct TextureInner {
    metal: native::Texture,
    handle: ResourceHandle,
    device: Device,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Texture(Rc<TextureInner>);

pub(crate) fn create(device: &Device, descriptor: &ogpu_core::validation::AdmittedTexture) -> Result<Texture, Error> {
    let metal = native::Texture::create(device.metal(), descriptor.clone())?;
    device.attach_resource();
    Ok(Texture(Rc::new(TextureInner {
        metal,
        handle: handle::create(device.handle()),
        device: device.clone(),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
        retained_by: Cell::new(0),
    })))
}

impl Texture {
    pub fn destroyed(&self) -> bool { handle::destroyed(&self.0.handle) }

    pub(crate) fn read_bytes(&self, read: &ogpu_core::validation::TextureRead) -> Result<Vec<u8>, Error> {
        self.0.metal.read_bytes(read)
    }
    pub(crate) fn read_bytes_into(&self, read: &mut ogpu_core::validation::TextureReadInto<'_>) -> Result<(), Error> {
        self.0.metal.read_bytes_into(read)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        if self.destroyed() {
            Ok(())
        } else if self.0.submission_uses.get() > 0 {
            handle::destroy(&self.0.handle);
            self.0.destroy_requested.set(true);
            Ok(())
        } else {
            self.0.metal.destroy()?;
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &native::Texture {
        &self.0.metal
    }

    pub fn retain_submission(&self) -> Result<(), Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Texture.retain_submission", ErrorKind::StaleHandle, "texture is destroyed"));
        }
        self.0.submission_uses.set(self.0.submission_uses.get() + 1);
        Ok(())
    }

    /// Retains once per command buffer; `Ok(false)` when `commands` already holds it.
    pub fn retain_for(&self, commands: i64) -> Result<bool, Error> {
        if self.0.retained_by.get() == commands {
            Ok(false)
        } else {
            self.retain_submission()?;
            self.0.retained_by.set(commands);
            Ok(true)
        }
    }

    pub fn release_submission(&self) {
        self.0.submission_uses.set(self.0.submission_uses.get() - 1);
        if self.0.submission_uses.get() == 0 && self.0.destroy_requested.get() && self.0.metal.destroy().is_ok() {
            self.0.device.detach_resource();
        }
    }
}
