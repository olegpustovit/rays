//! Ogpu_metal.Blit_encoder: buffer-to-texture copies.
use crate::{command_control::{CommandContext, OpenEncoder}, device::of_metal_error};
use ogpu_core::error::Error;
use ogpu_core::{backend::{DriverBlitEncoder, Token}, types::{Extent, Origin}};
use std::rc::Rc;

struct Blit {
    context: Rc<CommandContext>,
    encoder: metal::BlitEncoder,
}

pub fn create(context: &Rc<CommandContext>) -> Result<Box<dyn DriverBlitEncoder>, Error> {
    let encoder = metal::BlitEncoder::create(&context.native).map_err(|value| of_metal_error(context.operation, &value))?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Blit(encoder.clone()));
    Ok(Box::new(Blit { context: context.clone(), encoder }))
}

impl DriverBlitEncoder for Blit {
    fn buffer_to_texture(&self, src: Token, offset: i64, bytes_per_row: i64, bytes_per_image: i64, dst: Token, mip: i64, origin: Origin, extent: Extent) -> Result<(), Error> {
        let c = &self.context;
        let source = c.find_buffer(src)?;
        let destination = c.find_texture(dst)?;
        c.keep_buffer(&source)?;
        c.keep_texture(&destination)?;
        self.encoder.copy_buffer_to_texture(
            source.metal(),
            offset as u64,
            bytes_per_row as usize,
            bytes_per_image as usize,
            destination.metal(),
            0,
            mip as usize,
            metal::TextureRegion {
                x: origin.x as usize,
                y: origin.y as usize,
                z: origin.z as usize,
                width: extent.width as usize,
                height: extent.height as usize,
                depth: extent.depth as usize,
            },
        ).map_err(|value| of_metal_error(c.operation, &value))
    }

    fn end_blit(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding().map_err(|value| of_metal_error(self.context.operation, &value))
    }
}
