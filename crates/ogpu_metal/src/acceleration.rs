//! Ogpu_metal.Acceleration: acceleration structures (triangles, instances, procedural boxes,
//! curves, motion) built from the driver's token descriptors.
use crate::{
    buffer::Buffer,
    device::{fail, Device},
};
use crate::native::acceleration as ma;
use ogpu_core::{
    backend::{DriverAccel, DriverAccelDescriptor, DriverResource},
    error::{Error, ErrorKind},
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

struct AccelInner {
    device: Device,
    descriptor: RefCell<Option<ma::BuildDescriptor>>,
    metal: ma::AccelerationStructure,
    sizes: ma::Sizes,
    buffers: RefCell<Vec<Buffer>>,
    structures: RefCell<Vec<Acceleration>>,
    dead: Cell<bool>,
    released: Cell<bool>,
}

#[derive(Clone)]
pub struct Acceleration(Rc<AccelInner>);

fn primitive_build(operation:&str, device:&Device, descriptor:&DriverAccelDescriptor,
    resolve:&dyn Fn(&dyn DriverResource)->Result<Buffer,Error>) -> Result<(ma::BuildDescriptor,Vec<Buffer>),Error> {
    let DriverAccelDescriptor::Build { profile,buffers,structures }=descriptor else {
        return Err(fail(operation,ErrorKind::InvalidArgument,"a primitive descriptor is required"));
    };
    if !matches!(profile.layout(),ogpu_core::acceleration::BuildLayout::Primitive { .. }) || !structures.is_empty() {
        return Err(fail(operation,ErrorKind::InvalidArgument,"a primitive descriptor is required"));
    }
    let buffers=buffers.iter().map(|r|resolve(&**r)).collect::<Result<Vec<_>,_>>()?;
    let build=ma::BuildDescriptor::create(device.metal(),profile.clone(),buffers.iter().map(|b|b.metal().clone()).collect(),Vec::new())?;
    Ok((build,buffers))
}

fn allocate(device: &Device, descriptor: Option<ma::BuildDescriptor>, sizes: ma::Sizes, size: std::num::NonZeroU64, buffers: Vec<Buffer>, structures: Vec<Acceleration>) -> Result<Acceleration, Error> {
    let metal = ma::AccelerationStructure::create(device.metal(), size)?;
    device.attach_resource();
    Ok(Acceleration(Rc::new(AccelInner {
        device: device.clone(),
        descriptor: RefCell::new(descriptor),
        metal,
        sizes,
        buffers: RefCell::new(buffers),
        structures: RefCell::new(structures),
        dead: Cell::new(false),
        released: Cell::new(false),
    })))
}

pub(crate) fn create(device: &Device, descriptor: &DriverAccelDescriptor, resolve_buffer: &dyn Fn(&dyn DriverResource) -> Result<Buffer, Error>, resolve_structure: &dyn Fn(&dyn DriverAccel) -> Result<Acceleration, Error>) -> Result<Acceleration, Error> {
    let operation = "Ogpu_metal.Acceleration.create";
    match descriptor {
        DriverAccelDescriptor::Build { profile,buffers,structures } => {
            let buffers=buffers.iter().map(|r|resolve_buffer(&**r)).collect::<Result<Vec<_>,_>>()?;
            let structures=structures.iter().map(|a|resolve_structure(&**a)).collect::<Result<Vec<_>,_>>()?;
            let build=ma::BuildDescriptor::create(device.metal(),profile.clone(),
                buffers.iter().map(|b|b.metal().clone()).collect(),structures.iter().map(|s|s.0.metal.clone()).collect())?;
            match build.sizes() {
                Err(e)=>{ let _=build.destroy();Err(e) }
                Ok(sizes)=>allocate(device,Some(build),sizes,
                    ogpu_core::validation::admit_acceleration_size(operation,sizes.acceleration_structure_size)?,buffers,structures),
            }
        }
        DriverAccelDescriptor::Sized { size, template } => {
            let template = resolve_structure(&**template)?;
            let sizes = ma::Sizes { acceleration_structure_size: size.get(), ..template.0.sizes };
            allocate(device, None, sizes, *size, template.0.buffers.borrow().clone(), template.0.structures.borrow().clone())
        }
    }
}

impl Drop for AccelInner {
    fn drop(&mut self) {
        let _ = finish_destroy(self);
    }
}

fn finish_destroy(value: &AccelInner) -> Result<(), Error> {
    if value.released.replace(true) {
        return Ok(());
    }
    let released = match value.descriptor.borrow().as_ref() {
        None => Ok(()),
        Some(d) => d.destroy(),
    };
    released?;
    value.metal.destroy()?;
    value.device.detach_resource();
    // The structure no longer needs what it referenced.
    value.buffers.borrow_mut().clear();
    value.structures.borrow_mut().clear();
    Ok(())
}

impl Acceleration {
    pub fn sizes(&self) -> ma::Sizes {
        self.0.sizes
    }
    pub fn metal(&self) -> &ma::AccelerationStructure {
        &self.0.metal
    }

    pub(crate) fn encode_build(&self, encoder: &ma::AccelerationEncoder, scratch: &Buffer, scratch_offset: i64) -> Result<(), Error> {
        let operation = "Ogpu_metal.Acceleration.encode_build";
        match self.0.descriptor.borrow().as_ref() {
            None => Err(fail(operation, ErrorKind::InvalidState, "a sized structure is filled by copy or compaction")),
            Some(descriptor) => encoder.build_with(&self.0.metal, descriptor, scratch.metal(), scratch_offset as u64),
        }
    }

    /// The structure keeps the refit's descriptor (its buffers are the ones a later refit replaces).
    pub(crate) fn encode_refit(&self, encoder: &ma::AccelerationEncoder, device: &Device, descriptor: &DriverAccelDescriptor, resolve_buffer: &dyn Fn(&dyn DriverResource) -> Result<Buffer, Error>, scratch: &Buffer, scratch_offset: i64) -> Result<(), Error> {
        let operation = "Ogpu_metal.Acceleration.encode_refit";
        let (build, buffers) = primitive_build(operation, device, descriptor, resolve_buffer)?;
        match encoder.refit_with(&self.0.metal, &build, scratch.metal(), scratch_offset as u64) {
            Err(e) => {
                let _ = build.destroy();
                Err(e)
            }
            Ok(()) => {
                if let Some(old) = self.0.descriptor.borrow().as_ref() {
                    let _ = old.destroy();
                }
                *self.0.descriptor.borrow_mut() = Some(build);
                *self.0.buffers.borrow_mut() = buffers;
                Ok(())
            }
        }
    }

    pub(crate) fn encode_compact(encoder: &ma::AccelerationEncoder, src: &Acceleration, dst: &Acceleration) -> Result<(), Error> {
        encoder.copy_and_compact(&src.0.metal, &dst.0.metal)
    }

    pub(crate) fn encode_compacted_size(&self, encoder: &ma::AccelerationEncoder, destination: &Buffer, output: ogpu_core::validation::AdmittedCompactedSize) -> Result<(), Error> {
        encoder.write_compacted_size_typed(&self.0.metal, destination.metal(), output)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let value = &self.0;
        if !value.dead.get() {
            // Held elsewhere (a command buffer in flight): the last drop releases it.
            if Rc::strong_count(&self.0) == 1 {
                finish_destroy(value)?;
            }
            value.dead.set(true);
        }
        Ok(())
    }

    pub fn destroyed(&self) -> bool {
        self.0.dead.get()
    }

    pub fn buffers(&self) -> Vec<Buffer> {
        self.0.buffers.borrow().clone()
    }
}
