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
    submissions: Cell<i64>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Sampler(Rc<SamplerInner>);

pub fn create(device: &Device, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Sampler, Error> {
    let metal = native::Sampler::create(device.metal(), descriptor)?;
    device.attach_resource();
    Ok(Sampler(Rc::new(SamplerInner { metal, device: device.clone(), handle: handle::create(device.handle()), submissions: Cell::new(0), retained_by: Cell::new(0) })))
}

impl Sampler {
    pub fn destroyed(&self) -> bool {
        handle::destroyed(&self.0.handle)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let operation = "Ogpu_metal.Sampler.destroy";
        if self.destroyed() {
            Ok(())
        } else if self.0.submissions.get() > 0 {
            Err(fail(operation, ErrorKind::InvalidState, "sampler is retained by submitted work"))
        } else {
            self.0.metal.destroy()?;
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &native::Sampler {
        &self.0.metal
    }

    pub fn retain_submission(&self) -> Result<(), Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Sampler.retain", ErrorKind::StaleHandle, "sampler is destroyed"));
        }
        self.0.submissions.set(self.0.submissions.get() + 1);
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
        if self.0.submissions.get() > 0 {
            self.0.submissions.set(self.0.submissions.get() - 1);
        }
    }
}
