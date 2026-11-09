//! Ogpu_metal.Buffer
use crate::device::{fail, of_metal_error, Device};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::{BufferDescriptor, BufferUsage},
    validation,
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
    metal: metal::Buffer,
    handle: ResourceHandle,
    device: Device,
    descriptor: BufferDescriptor,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Buffer(Rc<BufferInner>);

fn validate_memory(descriptor: &BufferDescriptor, memory: Memory) -> Result<(), Error> {
    match memory {
        Memory::DeviceLocal | Memory::Shared | Memory::Upload => Ok(()),
        Memory::Readback => {
            if descriptor.usage.contains(&BufferUsage::CopyDst) {
                Ok(())
            } else {
                Err(fail("Ogpu_metal.Buffer.create", ErrorKind::InvalidArgument, "readback buffers require Copy_dst usage"))
            }
        }
    }
}

fn wrap(device: &Device, descriptor: &BufferDescriptor, metal: metal::Buffer) -> Buffer {
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

pub fn create(device: &Device, memory: Memory, descriptor: &BufferDescriptor) -> Result<Buffer, Error> {
    let operation = "Ogpu_metal.Buffer.create";
    if device.destroyed() {
        return Err(fail(operation, ErrorKind::StaleHandle, "device is destroyed"));
    }
    validation::validate_buffer(&device.capabilities(), descriptor)?;
    validate_memory(descriptor, memory)?;
    let (storage, cache) = match memory {
        Memory::DeviceLocal => (metal::StorageMode::Private, metal::CpuCacheMode::DefaultCache),
        Memory::Shared | Memory::Readback => (metal::StorageMode::Shared, metal::CpuCacheMode::DefaultCache),
        Memory::Upload => (metal::StorageMode::Shared, metal::CpuCacheMode::WriteCombined),
    };
    let native = metal::Buffer::create(device.metal(), descriptor.size as u64, storage, cache, descriptor.label.as_deref()).map_err(|value| of_metal_error(operation, &value))?;
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
        Ok(self.0.descriptor.clone())
    }

    pub fn write_bytes(&self, _device: &Device, dst_offset: i64, bytes: &[u8]) -> Result<(), Error> {
        let operation = "Ogpu_metal.Buffer.write_bytes";
        self.0.metal.write_bytes(dst_offset as u64, bytes).map_err(|value| of_metal_error(operation, &value))
    }

    pub fn read_bytes(&self, _device: &Device, offset: i64, length: i64) -> Result<Vec<u8>, Error> {
        let operation = "Ogpu_metal.Buffer.read_bytes";
        self.0.metal.read_bytes(offset as u64, length as usize).map_err(|value| of_metal_error(operation, &value))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let operation = "Ogpu_metal.Buffer.destroy";
        if self.destroyed() {
            Ok(())
        } else if self.0.submission_uses.get() > 0 {
            handle::destroy(&self.0.handle);
            self.0.destroy_requested.set(true);
            Ok(())
        } else {
            self.0.metal.destroy().map_err(|value| of_metal_error(operation, &value))?;
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &metal::Buffer {
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

