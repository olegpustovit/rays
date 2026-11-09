//! Ogpu_metal.Texture
use crate::native;
use crate::device::{fail, Device};
use crate::native::{StorageMode, TextureKind, TextureRegion, TextureUsage as MetalUsage};
use ogpu_core::{
    error::Error,
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::{Memory, TextureDescriptor, TextureFormat, TextureUsage},
};
use std::{cell::Cell, rc::Rc};

struct TextureInner {
    metal: native::Texture,
    handle: ResourceHandle,
    device: Device,
    descriptor: TextureDescriptor,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
    /// Last command buffer that retained it.
    retained_by: Cell<i64>,
}

#[derive(Clone)]
pub struct Texture(Rc<TextureInner>);

fn usage(values: &[TextureUsage]) -> Vec<MetalUsage> {
    let mut native: Vec<MetalUsage> = Vec::new();
    for value in values {
        let mapped = match value {
            TextureUsage::TextureBinding => Some(MetalUsage::ShaderRead),
            TextureUsage::StorageBinding => Some(MetalUsage::ShaderWrite),
            TextureUsage::RenderAttachment => Some(MetalUsage::RenderTarget),
            TextureUsage::TextureCopySrc | TextureUsage::TextureCopyDst => None,
        };
        if let Some(m) = mapped {
            if !native.contains(&m) {
                native.insert(0, m);
            }
        }
    }
    // Metal expands an empty usage mask during creation; supply one explicit native bit for
    // copy-only textures so descriptor validation stays exact.
    if native.is_empty() { vec![MetalUsage::ShaderRead] } else { native }
}

/// The validated native descriptor for an OGPU texture.
fn native_descriptor(memory: Memory, format: TextureFormat, descriptor: &TextureDescriptor) -> native::TextureDescriptor {
    let storage = match memory {
        Memory::DeviceLocal => StorageMode::Private,
        Memory::Shared => StorageMode::Shared,
        Memory::Memoryless => StorageMode::Memoryless,
    };
    let mut base = native::TextureDescriptor::descriptor_2d(
        format,
        descriptor.width as usize,
        descriptor.height as usize,
        storage,
        usage(&descriptor.usage),
        descriptor.label.clone(),
    );
    base.kind = if descriptor.sample_count > 1 {
        TextureKind::Texture2dMultisample
    } else if descriptor.depth > 1 {
        TextureKind::Texture3d
    } else {
        TextureKind::Texture2d
    };
    base.depth = descriptor.depth as usize;
    base.mip_levels = descriptor.mip_levels as usize;
    base.sample_count = descriptor.sample_count as usize;
    base
}

pub(crate) fn create(device: &Device, memory: Memory, format: TextureFormat, descriptor: &TextureDescriptor) -> Result<Texture, Error> {
    let native = native_descriptor(memory, format, descriptor);
    let metal = native::Texture::create(device.metal(), native)?;
    device.attach_resource();
    Ok(Texture(Rc::new(TextureInner {
        metal,
        handle: handle::create(device.handle()),
        device: device.clone(),
        descriptor: descriptor.clone(),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
        retained_by: Cell::new(0),
    })))
}

impl Texture {
    pub fn destroyed(&self) -> bool { handle::destroyed(&self.0.handle) }
    pub(crate) fn descriptor(&self) -> &TextureDescriptor { &self.0.descriptor }

    // The core read admission already checked this base-level row layout.
    pub(crate) fn read_bytes(&self, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
        let descriptor = &self.0.descriptor;
        self.0.metal.read_bytes(TextureRegion { x: 0, y: 0, z: 0, width: descriptor.width as usize,
            height: descriptor.height as usize, depth: 1 }, 0, 0, bytes_per_row as usize,
            (bytes_per_row * descriptor.height) as usize)
    }
    pub(crate) fn read_bytes_into(&self, bytes_per_row: i64, destination: &mut [u8]) -> Result<(), Error> {
        let descriptor = &self.0.descriptor;
        self.0.metal.read_bytes_into(TextureRegion { x: 0, y: 0, z: 0, width: descriptor.width as usize,
            height: descriptor.height as usize, depth: 1 }, 0, 0, bytes_per_row as usize,
            (bytes_per_row * descriptor.height) as usize, destination)
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
