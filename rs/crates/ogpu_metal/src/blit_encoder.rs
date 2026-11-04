//! Ogpu_metal.Blit_encoder: buffer-to-texture copies.
use crate::{command_control::CommandContext, device::R};
use ogpu_core::{backend::DriverBlitEncoder, types::{Extent, Origin}};
use std::rc::Rc;

pub fn create(context: &Rc<CommandContext>) -> R<DriverBlitEncoder> {
    context.recording()?;
    let encoder = context.native_of(metal::BlitEncoder::create(&context.native))?;
    let closing = encoder.clone();
    *context.open_encoder.borrow_mut() = Some(Box::new(move || {
        let _ = closing.end_encoding();
    }));
    let (c_copy, e_copy) = (context.clone(), encoder.clone());
    let (c_end, e_end) = (context.clone(), encoder);
    Ok(DriverBlitEncoder {
        buffer_to_texture: Box::new(move |src, offset, bytes_per_row, bytes_per_image, dst, mip, origin: Origin, extent: Extent| {
            let source = c_copy.find_buffer(src)?;
            let destination = c_copy.find_texture(dst)?;
            c_copy.keep_buffer(&source)?;
            c_copy.keep_texture(&destination)?;
            c_copy.native_of(e_copy.copy_buffer_to_texture(
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
            ))
        }),
        end_blit: Box::new(move || {
            *c_end.open_encoder.borrow_mut() = None;
            c_end.native_of(e_end.end_encoding())
        }),
    })
}
