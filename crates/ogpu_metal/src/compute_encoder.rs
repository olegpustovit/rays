//! Ogpu_metal.Compute_encoder: the driver's compute encoder over a Metal compute encoder.
use ogpu_core::error::Error;
use crate::{
    command_control::{CommandContext, OpenEncoder, Resource},
    device::{fail, of_metal_error},
    pipeline::Native,
};
use ogpu_core::{backend::{DriverComputeEncoder, Token}, error::ErrorKind};
use std::rc::Rc;

fn size(t: (i64, i64, i64)) -> (usize, usize, usize) {
    (t.0 as usize, t.1 as usize, t.2 as usize)
}

struct Compute {
    context: Rc<CommandContext>,
    encoder: metal::ComputeEncoder,
}

pub fn create(context: &Rc<CommandContext>) -> Result<Box<dyn DriverComputeEncoder>, Error> {
    context.recording()?;
    let encoder = metal::ComputeEncoder::create(&context.native).map_err(|value| of_metal_error(context.operation, &value))?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Compute(encoder.clone()));
    Ok(Box::new(Compute { context: context.clone(), encoder }))
}

impl DriverComputeEncoder for Compute {
    fn set_pipeline(&self, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let Some(pipeline) = c.pipeline(token) else {
            return Err(fail(c.operation, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
        };
        match pipeline.native() {
            Native::Render(_) => Err(fail(c.operation, ErrorKind::InvalidArgument, "render pipeline cannot execute compute")),
            Native::Compute(compute) => {
                let p = pipeline.clone();
                c.keep(pipeline.retain_submission(), Box::new(move || p.release_submission()))?;
                self.encoder.set_pipeline(compute).map_err(|value| of_metal_error(c.operation, &value))
            }
        }
    }

    fn set_buffer(&self, index: i64, offset: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(token)?;
        c.keep_buffer(&buffer)?;
        self.encoder.set_buffer(index as usize, offset as u64, buffer.metal()).map_err(|value| of_metal_error(c.operation, &value))
    }

    fn set_bytes(&self, index: i64, bytes: &[u8]) -> Result<(), Error> {
        self.encoder.set_bytes(index as usize, bytes).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_texture(&self, index: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        match c.resource(token) {
            Some(Resource::Texture(texture)) => {
                c.keep_texture(&texture)?;
                self.encoder.set_texture(index as usize, texture.metal()).map_err(|value| of_metal_error(c.operation, &value))
            }
            _ => Err(fail(c.operation, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }
    }

    fn set_accel(&self, index: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(token)?;
        c.keep_accel(&structure)?;
        self.encoder.set_acceleration_structure(index as usize, Some(structure.metal())).map_err(|value| of_metal_error(c.operation, &value))
    }

    fn dispatch_threads(&self, threads: (i64, i64, i64), threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        self.encoder.dispatch_threads(size(threads), size(threadgroup)).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn dispatch_indirect(&self, token: Token, offset: i64, threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(token)?;
        c.keep_buffer(&buffer)?;
        self.encoder.dispatch_threadgroups_indirect(buffer.metal(), offset as u64, size(threadgroup)).map_err(|value| of_metal_error(c.operation, &value))
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
            self.encoder.use_acceleration_structures(&structures).map_err(|value| of_metal_error(c.operation, &value))
        }
    }

    fn end_compute(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding().map_err(|value| of_metal_error(self.context.operation, &value))
    }
}
