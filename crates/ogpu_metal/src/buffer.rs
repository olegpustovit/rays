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
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
    released: Cell<bool>,
}

impl BufferInner {
    fn release(&self) -> Result<(), Error> {
        if !self.released.get() {
            self.metal.destroy()?;
            self.released.set(true);
            self.device.detach_resource();
        }
        Ok(())
    }
}

/// The last owner, a command buffer in flight or the owner wrapper, finishes a destroy.
impl Drop for BufferInner {
    fn drop(&mut self) {
        let _ = self.release();
    }
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
        retained_by: Cell::new(0),
        released: Cell::new(false),
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
        if !self.destroyed() {
            // Held elsewhere (a command buffer in flight): the last drop releases it.
            if Rc::strong_count(&self.0) == 1 {
                self.0.release()?;
            }
            handle::destroy(&self.0.handle);
        }
        Ok(())
    }

    pub fn metal(&self) -> &native::Buffer {
        &self.0.metal
    }

    /// Retains once per command buffer; `Ok(false)` when `commands` already holds it.
    pub fn retain_for(&self, commands: i64) -> Result<bool, Error> {
        if self.destroyed() {
            return Err(fail("Ogpu_metal.Buffer.retain", ErrorKind::StaleHandle, "buffer is destroyed"));
        }
        Ok(self.0.retained_by.replace(commands) != commands)
    }
}
