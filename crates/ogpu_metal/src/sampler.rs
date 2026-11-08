//! Ogpu_metal.Sampler
use crate::device::{fail, of_metal_error, Device};
use ogpu::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter},
};
use std::{cell::Cell, rc::Rc};

struct SamplerInner {
    metal: metal::Sampler,
    handle: ResourceHandle,
    device: Device,
    submissions: Cell<i64>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Sampler(Rc<SamplerInner>);

fn filter(value: SamplerFilter) -> metal::Filter {
    match value {
        SamplerFilter::Nearest => metal::Filter::Nearest,
        SamplerFilter::Linear => metal::Filter::Linear,
    }
}

fn mip(value: MipFilter) -> metal::MipFilter {
    match value {
        MipFilter::NoMip => metal::MipFilter::None,
        MipFilter::NearestMip => metal::MipFilter::Nearest,
        MipFilter::LinearMip => metal::MipFilter::Linear,
    }
}

fn address(value: AddressMode) -> metal::AddressMode {
    match value {
        AddressMode::ClampToEdge => metal::AddressMode::ClampToEdge,
        AddressMode::Repeat => metal::AddressMode::Repeat,
        AddressMode::MirrorRepeat => metal::AddressMode::MirrorRepeat,
    }
}

pub fn create(device: &Device, descriptor: &SamplerDescriptor) -> Result<Sampler, Error> {
    let operation = "Ogpu_metal.Sampler.create";
    if device.destroyed() {
        return Err(fail(operation, ErrorKind::StaleHandle, "device is destroyed"));
    }
    if descriptor.max_anisotropy < 1
        || descriptor.max_anisotropy > 16
        || !descriptor.lod_min.is_finite()
        || !descriptor.lod_max.is_finite()
        || descriptor.lod_min < 0.
        || descriptor.lod_max < descriptor.lod_min
    {
        return Err(fail(operation, ErrorKind::InvalidArgument, "sampler limits are invalid"));
    }
    let native = metal::SamplerDescriptor {
        min_filter: filter(descriptor.min_filter),
        mag_filter: filter(descriptor.mag_filter),
        mip_filter: mip(descriptor.mip_filter),
        max_anisotropy: descriptor.max_anisotropy as u8,
        address_s: address(descriptor.address_u),
        address_t: address(descriptor.address_v),
        address_r: address(descriptor.address_v),
        lod_min: descriptor.lod_min as f32,
        lod_max: descriptor.lod_max as f32,
        support_argument_buffers: true,
        label: descriptor.label.clone(),
        ..metal::SamplerDescriptor::default()
    };
    let metal = metal::Sampler::create(device.metal(), native).map_err(|value| of_metal_error(operation, &value))?;
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
            self.0.metal.destroy().map_err(|value| of_metal_error(operation, &value))?;
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &metal::Sampler {
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
        if self.0.retained_by.get() == commands && !self.destroyed() {
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
