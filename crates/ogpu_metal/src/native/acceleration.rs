use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Buffer, CommandBuffer, Device, Error, ErrorKind, Lifetime};
use metal_sys;


struct AccelerationStructureInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    size: u64,
    device_attached: AtomicBool,
}
impl Drop for AccelerationStructureInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
    }
}

#[derive(Clone)]
pub struct AccelerationStructure(Arc<AccelerationStructureInner>);
impl AccelerationStructure {
    pub fn create(device: &Device, size: std::num::NonZeroU64) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        let raw = metal_sys::registry::acceleration_structure_create(device.raw(), size).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(AccelerationStructureInner {
            raw,
            lifetime: Lifetime::new(),
            device: device.clone(),
            size:size.get(),
            device_attached: AtomicBool::new(true),
        })))
    }

    pub fn size(&self) -> u64 {
        self.0.size
    }

    pub fn is_destroyed(&self) -> bool {
        self.0.lifetime.destroyed.load(Ordering::Acquire)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_structure.destroy";
        self.0.lifetime.destroy_parent(OP)?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) {
            self.0.device.detach_child();
        }
        Ok(())
    }

    pub(crate) fn raw(&self) -> &metal_sys::Handle {
        &self.0.raw
    }
    pub(crate) fn lifetime(&self) -> &Lifetime {
        &self.0.lifetime
    }
}

pub use metal_sys::native::acceleration::Sizes;
pub use ogpu_core::acceleration::InstanceKind;
pub struct BuildDescriptor {
    lifetime: Lifetime,
    device: Device,
    data: metal_sys::native::acceleration::Descriptor,
    buffers: Vec<Buffer>,
    structures: Vec<AccelerationStructure>,
    device_attached: AtomicBool,
}
impl Drop for BuildDescriptor {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
    }
}
impl BuildDescriptor {
    pub(crate) fn create(device:&Device, profile:ogpu_core::acceleration::AdmittedBuild,
        buffers:Vec<Buffer>, structures:Vec<AccelerationStructure>) -> Result<Self,Error> {
        const OP:&str="Metal.Acceleration_structure.Build.create";
        device.ensure_live(OP)?;
        if !profile.admitted_for(&device.capabilities()?) {
            return Err(error(OP,ErrorKind::CrossDevice,"acceleration capabilities do not match native device"));
        }
        let data=metal_sys::native::acceleration::Descriptor::create(device.raw(),profile,
            buffers.iter().map(|b|b.raw().clone()).collect(),structures.iter().map(|s|s.raw().clone()).collect())
            .map_err(|failure|sys_error(OP,failure))?;
        device.attach_child(OP)?;
        Ok(Self { lifetime:Lifetime::new(), device:device.clone(),data,buffers,structures,device_attached:AtomicBool::new(true) })
    }

    pub(crate) fn sizes(&self) -> Result<Sizes, Error> {
        metal_sys::native::acceleration::descriptor_sizes(self.device.raw(), &self.data).map_err(|failure| sys_error("Metal.Acceleration_structure.Build.sizes", failure))
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.lifetime.destroy_parent("Metal.Acceleration_structure.Build.destroy")?;
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.detach_child(); }
        Ok(())
    }
    fn native_descriptor(&self) -> &metal_sys::native::acceleration::Descriptor { &self.data }
}

struct AccelerationEncoderInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    command_buffer: CommandBuffer,
    command_buffer_attached: AtomicBool,
}
impl Drop for AccelerationEncoderInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.command_buffer_attached.swap(false, Ordering::AcqRel) {
            self.command_buffer.lifetime().detach();
        }
    }
}

#[derive(Clone)]
pub struct AccelerationEncoder(Arc<AccelerationEncoderInner>);
pub use ogpu_core::validation::CompactedSizeType;
impl AccelerationEncoder {
    pub fn create(command_buffer: &CommandBuffer) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_encoder.create";
        let raw = metal_sys::registry::command_buffer_acceleration_encoder(command_buffer.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        Ok(Self(Arc::new(AccelerationEncoderInner {
            raw,
            lifetime: Lifetime::new(),
            command_buffer: command_buffer.clone(),
            command_buffer_attached: AtomicBool::new(true),
        })))
    }

    pub fn build_with(
        &self,
        destination: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
    ) -> Result<(), Error> {
        self.with_descriptor("Metal.Acceleration_encoder.build_with", destination, descriptor, scratch, scratch_offset, false)
    }

    pub fn refit_with(
        &self,
        source: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
    ) -> Result<(), Error> {
        self.with_descriptor("Metal.Acceleration_encoder.refit_with", source, descriptor, scratch, scratch_offset, true)
    }

    fn with_descriptor(
        &self,
        operation: &'static str,
        destination: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
        refit: bool,
    ) -> Result<(), Error> {
        metal_sys::native::acceleration::encoder_build(
                &self.0.raw,
                destination.raw(),
                descriptor.native_descriptor(),
                scratch.raw(),
                scratch_offset,
                refit,
            ).map_err(|failure| sys_error(operation, failure))?;
        self.0.command_buffer.retain_buffer(scratch);
        for buffer in &descriptor.buffers {
            self.0.command_buffer.retain_buffer(buffer);
        }
        self.0.command_buffer.retain_acceleration(destination);
        for structure in &descriptor.structures {
            self.0.command_buffer.retain_acceleration(structure);
        }
        Ok(())
    }

    pub fn copy_and_compact(
        &self,
        source: &AccelerationStructure,
        destination: &AccelerationStructure,
    ) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.copy_and_compact";
        metal_sys::registry::acceleration_encoder_copy_and_compact(
            &self.0.raw,
            source.raw(),
            destination.raw(),
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_acceleration(source);
        self.0.command_buffer.retain_acceleration(destination);
        Ok(())
    }

    pub fn write_compacted_size_typed(
        &self,
        source: &AccelerationStructure,
        destination: &Buffer,
        output: ogpu_core::validation::AdmittedCompactedSize,
    ) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.write_compacted_size_typed";
        metal_sys::native::acceleration::encoder_write_compacted_size(
            &self.0.raw,
            source.raw(),
            destination.raw(),
            output,
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_acceleration(source);
        self.0.command_buffer.retain_buffer(destination);
        Ok(())
    }

    pub fn end_encoding(&self) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.end_encoding";
        metal_sys::registry::acceleration_encoder_end(&self.0.raw).map_err(|failure| sys_error(OP, failure))?;
        self.0.lifetime.destroyed.store(true, Ordering::Release);
        if self.0.command_buffer_attached.swap(false, Ordering::AcqRel) {
            self.0.command_buffer.lifetime().detach();
        }
        Ok(())
    }
}
