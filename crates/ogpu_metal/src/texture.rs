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
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
    released: Cell<bool>,
}

impl TextureInner {
    fn release(&self) -> Result<(), Error> {
        if !self.released.get() {
            self.metal.destroy()?;
            self.released.set(true);
            self.device.detach_resource();
        }
        Ok(())
    }
}

impl Drop for TextureInner {
    fn drop(&mut self) {
        let _ = self.release();
    }
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
        retained_by: Cell::new(0),
        released: Cell::new(false),
    })))
}

impl Texture {
    pub fn destroyed(&self) -> bool { handle::destroyed(&self.0.handle) }

    pub(crate) fn read_bytes(&self, read: &ogpu_core::validation::TextureRead) -> Result<Vec<u8>, Error> {
        self.0.metal.read_bytes(read)
    }
    pub(crate) fn read_bytes_into(&self, read: &ogpu_core::validation::TextureReadInto, destination: &mut [u8]) -> Result<(), Error> {
        self.0.metal.read_bytes_into(read, destination)
    }

    pub(crate) fn write_bytes(&self, write: &ogpu_core::validation::TextureRead, bytes: &[u8]) -> Result<(), Error> {
        self.0.metal.write_bytes(write, bytes)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        if !self.destroyed() {
            // Held elsewhere (a command buffer in flight): the last drop releases it.
            if Rc::strong_count(&self.0) == 1 {
                self.0.release()?;
            }
            handle::destroy(&self.0.handle);
        }
        Ok(())
    }

    pub fn metal(&self) -> &native::Texture {
        &self.0.metal
    }

    /// Retains once per command buffer; `Ok(false)` when `commands` already holds it.
    pub fn retain_for(&self, commands: i64) -> Result<bool, Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Texture.retain", ErrorKind::StaleHandle, "texture is destroyed"));
        }
        Ok(self.0.retained_by.replace(commands) != commands)
    }
}
