//! Ogpu_metal.Blit_encoder: buffer-to-texture copies.
use crate::native;
use crate::{command_control::{CommandContext, OpenEncoder}};
use ogpu_core::error::Error;
use ogpu_core::{backend::{DriverBlitEncoder, DriverResource}, types::{Extent, Origin}};
use std::rc::Rc;

struct Blit {
    context: Rc<CommandContext>,
    encoder: native::BlitEncoder,
}

pub fn create(context: &Rc<CommandContext>) -> Result<Box<dyn DriverBlitEncoder>, Error> {
    let encoder = native::BlitEncoder::create(&context.native)?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Blit(encoder.clone()));
    Ok(Box::new(Blit { context: context.clone(), encoder }))
}

impl DriverBlitEncoder for Blit {
    fn buffer_to_texture(&self, src: &dyn DriverResource, offset: i64, bytes_per_row: i64, bytes_per_image: i64, dst: &dyn DriverResource, mip: i64, origin: Origin, extent: Extent) -> Result<(), Error> {
        let c = &self.context;
        let source = c.find_buffer(src)?;
        let destination = c.find_texture(dst)?;
        c.keep_buffer(source)?;
        c.keep_texture(destination)?;
        self.encoder.copy_buffer_to_texture(
            source.metal(),
            offset as u64,
            bytes_per_row as usize,
            bytes_per_image as usize,
            destination.metal(),
            0,
            mip as usize,
            native::TextureRegion {
                x: origin.x as usize,
                y: origin.y as usize,
                z: origin.z as usize,
                width: extent.width as usize,
                height: extent.height as usize,
                depth: extent.depth as usize,
            },
        )
    }

    fn end_blit(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding()
    }
}
