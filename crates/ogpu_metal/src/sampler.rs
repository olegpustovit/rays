//! Ogpu_metal.Sampler
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::{AddressMode, SamplerDescriptor},
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

fn address(value: AddressMode) -> native::AddressMode {
    match value {
        AddressMode::ClampToEdge => native::AddressMode::ClampToEdge,
        AddressMode::Repeat => native::AddressMode::Repeat,
        AddressMode::MirrorRepeat => native::AddressMode::MirrorRepeat,
    }
}

pub fn create(device: &Device, descriptor: &SamplerDescriptor) -> Result<Sampler, Error> {
    let native = native::SamplerDescriptor {
        min_filter: descriptor.min_filter,
        mag_filter: descriptor.mag_filter,
        mip_filter: descriptor.mip_filter,
        max_anisotropy: descriptor.max_anisotropy as u8,
        address_s: address(descriptor.address_u),
        address_t: address(descriptor.address_v),
        address_r: address(descriptor.address_v),
        lod_min: descriptor.lod_min as f32,
        lod_max: descriptor.lod_max as f32,
        support_argument_buffers: true,
        label: descriptor.label.clone(),
        ..native::SamplerDescriptor::default()
    };
    let metal = native::Sampler::create(device.metal(), native)?;
    device.attach_resource();
    Ok(Sampler(Rc::new(SamplerInner {
        metal,
        handle: handle::create(device.handle()),
        device: device.clone(),
        submissions: Cell::new(0),
        retained_by: Cell::new(0),
    })))
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
