//! Ogpu_metal.Compute_encoder: the driver's compute encoder over a Metal compute encoder.
use crate::native;
use ogpu_core::error::Error;
use crate::{
    command_control::{CommandContext, OpenEncoder},
    device::{fail},
    pipeline::Native,
};
use ogpu_core::{backend::{DriverComputeEncoder, DriverPipeline, DriverResource, Token}, error::ErrorKind};
use std::rc::Rc;

fn size(t: (i64, i64, i64)) -> (usize, usize, usize) {
    (t.0 as usize, t.1 as usize, t.2 as usize)
}

struct Compute {
    context: Rc<CommandContext>,
    encoder: native::ComputeEncoder,
}

pub fn create(context: &Rc<CommandContext>) -> Result<Box<dyn DriverComputeEncoder>, Error> {
    let encoder = native::ComputeEncoder::create(&context.native)?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Compute(encoder.clone()));
    Ok(Box::new(Compute { context: context.clone(), encoder }))
}

impl DriverComputeEncoder for Compute {
    fn set_pipeline(&self, pipeline: &dyn DriverPipeline) -> Result<(), Error> {
        let c = &self.context;
        let Some(pipeline) = crate::backend::metal_pipeline(pipeline) else {
            return Err(fail(c.operation, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
        };
        match pipeline.native() {
            Native::Render(_) => Err(fail(c.operation, ErrorKind::InvalidArgument, "render pipeline cannot execute compute")),
            Native::Compute(compute) => {
                c.keep_pipeline(pipeline)?;
                self.encoder.set_pipeline(compute)
            }
        }
    }

    fn set_buffer(&self, index: i64, offset: i64, resource: &dyn DriverResource) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(resource)?;
        c.keep_buffer(buffer)?;
        self.encoder.set_buffer(index as usize, offset as u64, buffer.metal())
    }

    fn set_bytes(&self, index: i64, admitted: ogpu_core::validation::AdmittedBytes, bytes: &[u8]) -> Result<(), Error> {
        self.encoder.set_bytes(index as usize, admitted, bytes)
    }

    fn set_texture(&self, index: i64, resource: &dyn DriverResource) -> Result<(), Error> {
        let c = &self.context;
        let texture = c.find_texture(resource)?;
        c.keep_texture(texture)?;
        self.encoder.set_texture(index as usize, texture.metal())
    }

    fn set_accel(&self, index: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(token)?;
        c.keep_accel(&structure)?;
        self.encoder.set_acceleration_structure(index as usize, Some(structure.metal()))
    }

    fn dispatch_threads(&self, threads: (i64, i64, i64), threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        self.encoder.dispatch_threads(size(threads), size(threadgroup))
    }

    fn dispatch_indirect(&self, resource: &dyn DriverResource, offset: i64, threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(resource)?;
        c.keep_buffer(buffer)?;
        self.encoder.dispatch_threadgroups_indirect(buffer.metal(), offset as u64, size(threadgroup))
    }

    fn compute_use_accels(&self, tokens: &[Token]) -> Result<(), Error> {
        let c = &self.context;
        let mut structures = Vec::new();
        for token in tokens {
            let structure = c.find_accel(*token)?;
            c.keep_accel(&structure)?;
            structures.push(structure.metal().clone());
        }
        if structures.is_empty() {
            Ok(())
        } else {
            self.encoder.use_acceleration_structures(&structures)
        }
    }

    fn end_compute(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding()
    }
}
