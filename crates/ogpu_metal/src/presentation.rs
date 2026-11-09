//! Ogpu_metal.Presentation: the blit from an RGBA8 source texture into a BGRA8 drawable.
use crate::native;
use crate::{
    device::{fail, Device},
    present_source,
};
use crate::native::{LoadAction, RenderPassDescriptor, RenderEncoder, StoreAction};
use ogpu_core::error::{Error, ErrorKind};
use std::cell::Cell;

const OPERATION: &str = "Ogpu_metal.Presentation";

pub struct Presentation {
    device: native::Device,
    library: native::Library,
    pipeline: native::RenderPipeline,
    dead: Cell<bool>,
}

pub fn create(device: &Device) -> Result<Presentation, Error> {
    let native = device.metal();
    if device.destroyed() {
        return Err(fail(OPERATION, ErrorKind::StaleHandle, "device is destroyed"));
    }
    let library = native::Library::compile_source(native, present_source::SOURCE, Some("Rays presentation"))?;
    let compiler = match native::Compiler::create(native) {
        Ok(c) => c,
        Err(e) => {
            let _ = library.destroy();
            return Err(e);
        }
    };
    let options = native::RenderPipelineOptions {
        label: Some("Rays presentation".to_owned()),
        fragment: Some("present_fragment".to_owned()),
        color_formats: Some(vec![native::Format::Bgra8Unorm]),
        primitive_topology: native::PrimitiveTopology::TriangleList,
        ..native::RenderPipelineOptions::default()
    };
    let compiled = compiler.create_render_pipeline(&library, "present_vertex", &options);
    let _ = compiler.destroy();
    match compiled {
        Err(e) => {
            let _ = library.destroy();
            Err(e)
        }
        Ok(pipeline) => Ok(Presentation { device: native.clone(), library, pipeline, dead: Cell::new(false) }),
    }
}

impl Presentation {
    fn validate_resources(&self, device: &native::Device, source: &native::Texture, target: &native::Texture) -> Result<(), Error> {
        if self.dead.get() {
            return Err(fail(OPERATION, ErrorKind::StaleHandle, "presentation helper is destroyed"));
        }
        if !self.device.same(device) {
            return Err(fail(OPERATION, ErrorKind::CrossDevice, "presentation command belongs to another device"));
        }
        if source.is_destroyed() || target.is_destroyed() {
            return Err(fail(OPERATION, ErrorKind::StaleHandle, "presentation texture is destroyed"));
        }
        if !self.device.same(source.device()) || !self.device.same(target.device()) {
            return Err(fail(OPERATION, ErrorKind::CrossDevice, "presentation texture belongs to another device"));
        }
        let (s, t) = (source.descriptor(), target.descriptor());
        if s.kind != native::TextureKind::Texture2d
            || s.format != native::Format::Rgba8Unorm
            || s.depth != 1
            || s.mip_levels != 1
            || s.sample_count != 1
            || s.array_length != 1
        {
            Err(fail(OPERATION, ErrorKind::InvalidArgument, "presentation source must be a single-sample 2D RGBA8 texture"))
        } else if !s.usage.contains(&native::TextureUsage::ShaderRead) {
            Err(fail(OPERATION, ErrorKind::InvalidArgument, "presentation source must be shader-readable"))
        } else if t.kind != native::TextureKind::Texture2d
            || t.format != native::Format::Bgra8Unorm
            || t.depth != 1
            || t.mip_levels != 1
            || t.sample_count != 1
            || t.array_length != 1
        {
            Err(fail(OPERATION, ErrorKind::InvalidArgument, "presentation target must be a single-sample 2D BGRA8 texture"))
        } else if !t.usage.contains(&native::TextureUsage::RenderTarget) {
            Err(fail(OPERATION, ErrorKind::InvalidArgument, "presentation target must be renderable"))
        } else if s.width != t.width || s.height != t.height {
            Err(fail(OPERATION, ErrorKind::InvalidArgument, "presentation source and target extents differ"))
        } else {
            Ok(())
        }
    }

    /// Encodes the blit into `commands`; with `present`, also schedules that drawable.
    pub fn encode(&self, commands: &native::CommandBuffer, present: Option<&native::Drawable>, source: &native::Texture, target: &native::Texture) -> Result<(), Error> {
        if commands.destroyed() {
            return Err(fail(OPERATION, ErrorKind::StaleHandle, "presentation command buffer is destroyed"));
        }
        self.validate_resources(commands.device(), source, target)?;
        let pass = RenderPassDescriptor::create(&self.device, target.descriptor().width as u64, target.descriptor().height as u64, 1)?;
        let configured = pass
            .set_attachments(target, None, None, [0., 0., 0., 1.])
            .and_then(|()| pass.set_actions(LoadAction::Clear, StoreAction::Store, LoadAction::DontCare, StoreAction::DontCare, 1.0, LoadAction::DontCare, StoreAction::DontCare, 0));
        if let Err(e) = configured {
            let _ = pass.destroy();
            return Err(e);
        }
        let encoder = match RenderEncoder::create(commands, &pass) {
            Ok(e) => e,
            Err(e) => {
                let _ = pass.destroy();
                return Err(e);
            }
        };
        let drawn = encoder
            .set_pipeline(&self.pipeline)
            .and_then(|()| encoder.set_fragment_texture(0, source))
            .and_then(|()| encoder.draw_triangles(0, 3));
        // The encoder is ended on every path, as fail_encode does.
        let ended = match drawn {
            Ok(()) => encoder.end_encoding(),
            Err(e) => {
                if !encoder.destroyed() {
                    let _ = encoder.end_encoding();
                }
                let _ = pass.destroy();
                return Err(e);
            }
        };
        let _ = pass.destroy();
        ended?;
        match present {
            None => Ok(()),
            Some(drawable) => commands.present(drawable, native::PresentTime::Immediate),
        }
    }

    pub fn destroy(&self) {
        if !self.dead.get() {
            self.dead.set(true);
            let _ = self.pipeline.destroy();
            let _ = self.library.destroy();
        }
    }
}
