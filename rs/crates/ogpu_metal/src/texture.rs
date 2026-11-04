//! Ogpu_metal.Texture
use crate::device::{fail, native_of, Device, R};
use metal::{StorageMode, TextureKind, TextureRegion, TextureUsage as MetalUsage};
use ogpu_core::{
    error::ErrorKind,
    handle::{self, ResourceHandle},
    types::{Memory, TextureDescriptor, TextureFormat, TextureUsage},
    validation::{self, TextureProfile},
};
use std::{cell::Cell, rc::Rc};

struct TextureInner {
    metal: metal::Texture,
    handle: ResourceHandle,
    device: Device,
    descriptor: TextureDescriptor,
    format: TextureFormat,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
}

#[derive(Clone)]
pub struct Texture(Rc<TextureInner>);

fn metal_format(format: TextureFormat) -> metal::metal_format::Format {
    match format {
        TextureFormat::R8Unorm => metal::metal_format::Format::R8Unorm,
        TextureFormat::Rgba8Unorm => metal::metal_format::Format::Rgba8Unorm,
        TextureFormat::Bgra8Unorm => metal::metal_format::Format::Bgra8Unorm,
        TextureFormat::Bgra8UnormSrgb => metal::metal_format::Format::Bgra8UnormSrgb,
        TextureFormat::Rgba16Float => metal::metal_format::Format::Rgba16Float,
        TextureFormat::Rgba32Float => metal::metal_format::Format::Rgba32Float,
        TextureFormat::Depth32Float => metal::metal_format::Format::Depth32Float,
    }
}

fn bytes_per_pixel(format: TextureFormat) -> i64 {
    match format {
        TextureFormat::R8Unorm => 1,
        TextureFormat::Rgba8Unorm | TextureFormat::Bgra8Unorm | TextureFormat::Bgra8UnormSrgb | TextureFormat::Depth32Float => 4,
        TextureFormat::Rgba16Float => 8,
        TextureFormat::Rgba32Float => 16,
    }
}

fn validate_descriptor(value: &TextureDescriptor, format: TextureFormat) -> R<()> {
    let operation = "Ogpu_metal.Texture.create";
    let largest = value.width.max(value.height).max(value.depth).max(1);
    let max_mips = 1 + (63 - (largest as u64).leading_zeros() as i64);
    if value.mip_levels > max_mips {
        fail(operation, ErrorKind::InvalidArgument, "mip count exceeds texture extent")
    } else if ![1, 4, 9, 16].contains(&value.sample_count) {
        fail(operation, ErrorKind::InvalidArgument, "sample count must be 1, 4, 9, or 16")
    } else if value.sample_count > 1 && (value.depth != 1 || value.mip_levels != 1) {
        fail(operation, ErrorKind::InvalidArgument, "multisample textures are 2D and single-mip")
    } else if format == TextureFormat::Depth32Float && value.usage.contains(&TextureUsage::TextureBinding) {
        fail(operation, ErrorKind::InvalidArgument, "depth/stencil sampling is not exposed by this foundation")
    } else {
        Ok(())
    }
}

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
fn native_descriptor(operation: &str, device: &Device, memory: Memory, format: TextureFormat, descriptor: &TextureDescriptor) -> R<metal::TextureDescriptor> {
    if device.destroyed() {
        return fail(operation, ErrorKind::StaleHandle, "device is destroyed");
    }
    validation::validate_texture(&device.capabilities(), descriptor)?;
    let profile = TextureProfile {
        format,
        storage: memory,
        width: descriptor.width,
        height: descriptor.height,
        depth: descriptor.depth,
        mip_levels: descriptor.mip_levels,
        sample_count: descriptor.sample_count,
        usage: descriptor.usage.clone(),
    };
    validation::validate_texture_profile(&device.capabilities(), &profile)?;
    validate_descriptor(descriptor, format)?;
    let storage = match memory {
        Memory::DeviceLocal => StorageMode::Private,
        Memory::Shared => StorageMode::Shared,
        Memory::Memoryless => StorageMode::Memoryless,
    };
    let mut base = metal::TextureDescriptor::descriptor_2d(
        metal_format(format),
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
    Ok(base)
}

pub fn create(device: &Device, memory: Memory, format: TextureFormat, descriptor: &TextureDescriptor) -> R<Texture> {
    let operation = "Ogpu_metal.Texture.create";
    let native = native_descriptor(operation, device, memory, format, descriptor)?;
    let metal = native_of(operation, metal::Texture::create(device.metal(), native))?;
    device.attach_resource();
    Ok(Texture(Rc::new(TextureInner {
        metal,
        handle: handle::create(device.handle()),
        device: device.clone(),
        descriptor: descriptor.clone(),
        format,
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
    })))
}

impl Texture {
    fn validate(&self, operation: &str, device: &Device) -> R<()> {
        handle::validate_for(operation, device.handle(), &self.0.handle)
    }
    pub fn destroyed(&self) -> bool {
        handle::destroyed(&self.0.handle)
    }
    pub fn descriptor(&self, device: &Device) -> R<TextureDescriptor> {
        self.validate("Ogpu_metal.Texture.descriptor", device)?;
        Ok(self.0.descriptor.clone())
    }
    pub fn format(&self, device: &Device) -> R<TextureFormat> {
        self.validate("Ogpu_metal.Texture.format", device)?;
        Ok(self.0.format)
    }

    /// The level's extent, after the row stride check both reads share.
    fn level_layout(&self, operation: &str, mip_level: i64, bytes_per_row: i64) -> R<(i64, i64)> {
        if mip_level < 0 || mip_level >= self.0.descriptor.mip_levels {
            return fail(operation, ErrorKind::InvalidArgument, "mip level is outside the texture");
        }
        let width = (self.0.descriptor.width >> mip_level).max(1);
        let height = (self.0.descriptor.height >> mip_level).max(1);
        let minimum = width * bytes_per_pixel(self.0.format);
        if bytes_per_row < minimum || bytes_per_row > ((1i64 << 62) - 1) / height {
            return fail(operation, ErrorKind::InvalidArgument, "bytes_per_row is invalid");
        }
        Ok((width, height))
    }

    pub fn read_bytes(&self, device: &Device, mip_level: i64, bytes_per_row: i64) -> R<Vec<u8>> {
        let operation = "Ogpu_metal.Texture.read_bytes";
        self.validate(operation, device)?;
        let (width, height) = self.level_layout(operation, mip_level, bytes_per_row)?;
        native_of(
            operation,
            self.0.metal.read_bytes(
                TextureRegion { x: 0, y: 0, z: 0, width: width as usize, height: height as usize, depth: 1 },
                mip_level as usize,
                0,
                bytes_per_row as usize,
                (bytes_per_row * height) as usize,
            ),
        )
    }

    pub fn read_bytes_into(&self, device: &Device, mip_level: i64, bytes_per_row: i64, destination: &mut [u8]) -> R<()> {
        let operation = "Ogpu_metal.Texture.read_bytes_into";
        self.validate(operation, device)?;
        let (width, height) = self.level_layout(operation, mip_level, bytes_per_row)?;
        let required = bytes_per_row * height;
        if destination.len() as i64 != required {
            return fail(operation, ErrorKind::InvalidArgument, "destination length does not match the texture row layout");
        }
        native_of(
            operation,
            self.0.metal.read_bytes_into(
                TextureRegion { x: 0, y: 0, z: 0, width: width as usize, height: height as usize, depth: 1 },
                mip_level as usize,
                0,
                bytes_per_row as usize,
                required as usize,
                destination,
            ),
        )
    }

    pub fn destroy(&self) -> R<()> {
        let operation = "Ogpu_metal.Texture.destroy";
        if self.destroyed() {
            Ok(())
        } else if self.0.submission_uses.get() > 0 {
            handle::destroy(&self.0.handle);
            self.0.destroy_requested.set(true);
            Ok(())
        } else {
            native_of(operation, self.0.metal.destroy())?;
            handle::destroy(&self.0.handle);
            self.0.device.detach_resource();
            Ok(())
        }
    }

    pub fn metal(&self) -> &metal::Texture {
        &self.0.metal
    }

    pub fn retain_submission(&self) -> R<()> {
        if self.destroyed() {
            return fail("Ogpu_metal.Texture.retain_submission", ErrorKind::StaleHandle, "texture is destroyed");
        }
        self.0.submission_uses.set(self.0.submission_uses.get() + 1);
        Ok(())
    }

    pub fn release_submission(&self) {
        self.0.submission_uses.set(self.0.submission_uses.get() - 1);
        if self.0.submission_uses.get() == 0 && self.0.destroy_requested.get() && self.0.metal.destroy().is_ok() {
            self.0.device.detach_resource();
        }
    }
}
