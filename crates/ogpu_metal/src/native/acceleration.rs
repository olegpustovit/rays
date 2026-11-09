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
    pub fn create(device: &Device, size: u64) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        if size == 0 {
            return Err(error(OP, ErrorKind::InvalidArgument, "size must be positive"));
        }
        let raw = metal_sys::registry::acceleration_structure_create(device.raw(), size).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(AccelerationStructureInner {
            raw,
            lifetime: Lifetime::new(),
            device: device.clone(),
            size,
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

pub use metal_sys::native::acceleration::{Geometry, Keyframe, IndexBuffer as Index, GeometryCommon, Motion, Sizes};
pub use ogpu_core::acceleration::InstanceKind;
fn instance_kind_code(kind: InstanceKind) -> i32 { match kind { InstanceKind::UserIdInstances => 1, InstanceKind::MotionInstances => 2 } }

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
    // Input geometry has passed ogpu_core; these native references retain its allocations.
    pub(crate) fn primitive(device: &Device, geometries: Vec<Geometry>, motion: Option<Motion>, refit: bool, buffers: Vec<Buffer>) -> Result<Self, Error> {
        device.attach_child("Metal.Acceleration_structure.Build.primitive")?;
        Ok(Self {
            lifetime: Lifetime::new(), device: device.clone(),
            data: metal_sys::native::acceleration::Descriptor::Primitive(metal_sys::native::acceleration::PrimitiveDescriptor { geometries, motion, refit, fast_build: false }),
            buffers, structures: Vec::new(), device_attached: AtomicBool::new(true),
        })
    }

    pub(crate) fn instances(device: &Device, buffer: &Buffer, offset: u64, count: u64, kind: InstanceKind, motion_transforms: Option<(&Buffer, u64, u64)>, structures: &[AccelerationStructure]) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.instances";
        let layout = metal_sys::native::acceleration::instance_layout(instance_kind_code(kind)).map_err(|failure| sys_error(OP, failure))?;
        let stride = u64::try_from(layout[0]).map_err(|_| error(OP, ErrorKind::DeviceLost, "Metal returned an invalid instance descriptor size"))?;
        let mut buffers = vec![buffer.clone()];
        let native_transforms = motion_transforms.map(|(buffer, offset, count)| {
            buffers.push(buffer.clone());
            metal_sys::native::acceleration::MotionTransforms { buffer: buffer.raw().clone(), offset, count }
        });
        device.attach_child(OP)?;
        Ok(Self {
            lifetime: Lifetime::new(), device: device.clone(),
            data: metal_sys::native::acceleration::Descriptor::Instances(metal_sys::native::acceleration::InstancesDescriptor {
                buffer: buffer.raw().clone(), offset, stride, count, instance_kind: instance_kind_code(kind),
                structures: structures.iter().map(|value| value.raw().clone()).collect(), motion_transforms: native_transforms, refit: false,
            }),
            buffers, structures: structures.to_vec(), device_attached: AtomicBool::new(true),
        })
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
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompactedSizeType {
    Uint32,
    Uint64,
}
impl AccelerationEncoder {
    pub fn create(command_buffer: &CommandBuffer) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_encoder.create";
        enter_main(OP)?;
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
        if scratch_offset > scratch.length() {
            return Err(error(operation, ErrorKind::InvalidArgument, "scratch offset exceeds its buffer"));
        }
        let device = self.0.command_buffer.device();
        let sizes = metal_sys::native::acceleration::descriptor_sizes(device.raw(), descriptor.native_descriptor()).map_err(|failure| sys_error(operation, failure))?;
        let scratch_needed = if refit { sizes.refit_scratch_buffer_size } else { sizes.build_scratch_buffer_size };
        if destination.size() < sizes.acceleration_structure_size {
            return Err(error(operation, ErrorKind::InvalidArgument, "destination acceleration structure is too small"));
        }
        if scratch_needed > scratch.length() - scratch_offset {
            return Err(error(operation, ErrorKind::InvalidArgument, "scratch range is too small"));
        }
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
        offset: u64,
        size_type: CompactedSizeType,
    ) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.write_compacted_size_typed";
        let (bytes, uint64) = match size_type {
            CompactedSizeType::Uint32 => (4, false),
            CompactedSizeType::Uint64 => (8, true),
        };
        if offset % bytes != 0 || offset > destination.length() || bytes > destination.length() - offset {
            return Err(error(OP, ErrorKind::InvalidArgument, "compacted-size output range is invalid"));
        }
        metal_sys::native::acceleration::encoder_write_compacted_size(
            &self.0.raw,
            source.raw(),
            destination.raw(),
            offset,
            uint64,
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
