//! Ogpu_metal.Sampler
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},

};
use std::{cell::Cell, rc::Rc};

struct SamplerInner {
    metal: native::Sampler,
    handle: ResourceHandle,
    device: Device,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
    released: Cell<bool>,
}

impl SamplerInner {
    fn release(&self) -> Result<(), Error> {
        if !self.released.get() {
            self.metal.destroy()?;
            self.released.set(true);
            self.device.detach_resource();
        }
        Ok(())
    }
}

impl Drop for SamplerInner {
    fn drop(&mut self) {
        let _ = self.release();
    }
}

#[derive(Clone)]
pub struct Sampler(Rc<SamplerInner>);

pub fn create(device: &Device, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Sampler, Error> {
    let metal = native::Sampler::create(device.metal(), descriptor)?;
    device.attach_resource();
    Ok(Sampler(Rc::new(SamplerInner { metal, device: device.clone(), handle: handle::create(device.handle()), retained_by: Cell::new(0), released: Cell::new(false) })))
}

impl Sampler {
    pub fn destroyed(&self) -> bool {
        handle::destroyed(&self.0.handle)
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

    pub fn metal(&self) -> &native::Sampler {
        &self.0.metal
    }

    /// Retains once per command buffer; `Ok(false)` when `commands` already holds it.
    pub fn retain_for(&self, commands: i64) -> Result<bool, Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Sampler.retain", ErrorKind::StaleHandle, "sampler is destroyed"));
        }
        Ok(self.0.retained_by.replace(commands) != commands)
    }
}
