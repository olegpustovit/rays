//! Ogpu_metal.Buffer
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::BufferDescriptor,
};
use std::{
    cell::Cell,
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Memory {
    DeviceLocal,
    Shared,
    Upload,
    Readback,
}

struct BufferInner {
    metal: native::Buffer,
    handle: ResourceHandle,
    device: Device,
    descriptor: ogpu_core::validation::AdmittedBuffer,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Buffer(Rc<BufferInner>);

impl Memory {
    pub fn profile(self, descriptor: BufferDescriptor) -> ogpu_core::validation::BufferProfile {
        use ogpu_core::validation::{BufferKind, BufferProfile};
        BufferProfile { descriptor,
            storage: if self == Memory::DeviceLocal { native::StorageMode::Private } else { native::StorageMode::Shared },
            cpu_cache: if self == Memory::Upload { native::CpuCacheMode::WriteCombined } else { native::CpuCacheMode::DefaultCache },
            kind: if self == Memory::Readback { BufferKind::Readback } else { BufferKind::Ordinary } }
    }
}

fn wrap(device: &Device, descriptor: &ogpu_core::validation::AdmittedBuffer, metal: native::Buffer) -> Buffer {
    device.attach_resource();
    Buffer(Rc::new(BufferInner {
        metal,
        handle: handle::create(device.handle()),
        device: device.clone(),
        descriptor: descriptor.clone(),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
        retained_by: Cell::new(0),
    }))
}

pub fn create(device: &Device, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<Buffer, Error> {
    let native = native::Buffer::create(device.metal(), descriptor)?;
    Ok(wrap(device, descriptor, native))
}

impl Buffer {
    pub fn destroyed(&self) -> bool {
        handle::destroyed(&self.0.handle)
    }

    fn validate(&self, operation: &str, device: &Device) -> Result<(), Error> {
        handle::validate_for(operation, device.handle(), &self.0.handle)
    }

    pub fn descriptor(&self, device: &Device) -> Result<BufferDescriptor, Error> {
        self.validate("Ogpu_metal.Buffer.descriptor", device)?;
        Ok(self.0.descriptor.descriptor.clone())
    }

    pub fn write_bytes(&self, _device: &Device, dst_offset: i64, bytes: &[u8]) -> Result<(), Error> {
        self.0.metal.write_bytes(dst_offset as u64, bytes)
    }

    pub fn read_bytes(&self, _device: &Device, offset: i64, length: i64) -> Result<Vec<u8>, Error> {
        self.0.metal.read_bytes(offset as u64, length as usize)
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

    pub fn metal(&self) -> &native::Buffer {
        &self.0.metal
    }

    pub fn retain_submission(&self) -> Result<(), Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Buffer.retain_submission", ErrorKind::StaleHandle, "buffer is destroyed"));
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

