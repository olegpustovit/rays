//! The gallery's GPU half: a device on the platform backend (Metal on macOS, Vulkan elsewhere), a
//! painter fed from the UI's glyph atlases, the UI's retained layers drawn into a pass, host images
//! as textures, and an offscreen render read back (the goldens and the window capture).
use ogpu_core::backend::{self as be, ColorAttachment, Queue, RenderEncoder, RenderTarget, Texture};
use ogpu_core::error::{self, Error};
use ogpu_core::pipeline::{ColorFormat, DepthFormat};
use ogpu_core::render_pass::{Load, Store};
use ogpu_core::types::{Extent, Origin, TextureDescriptor, TextureFormat, TextureUsage};
use pxui::text::AtlasKind as TextKind;
use pxui::ui::{Image, Ui};
use pxui_render::{AtlasKind, Break, Painter, View};
use std::rc::Rc;

/// A GPU failure ends the app with its message.
pub fn fail(e: Error) -> ! {
    eprintln!("pxui_gallery: gpu: {}", error::to_string(&e));
    std::process::exit(1)
}

/// The device and its queue.
pub struct Gpu {
    pub device: be::Device,
    pub queue: Queue,
    /// textures made for host images, destroyed with the device
    textures: Vec<Texture>,
}

impl Gpu {
    /// Open the platform backend's device.
    pub fn open() -> Result<Gpu, String> {
        #[cfg(target_os = "macos")]
        ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
        #[cfg(any(windows, target_os = "linux"))]
        ogpu_core::impl_::select(ogpu_core::impl_::Impl {
            backend: ogpu_vulkan::impl_::BACKEND,
            create_driver: ogpu_vulkan::backend::create,
            live_handles: ogpu_vulkan::impl_::live_handles,
        });
        let driver = ogpu_core::impl_::create_driver().map_err(|e| error::to_string(&e))?;
        let device = be::create_device(driver).map_err(|e| error::to_string(&e))?;
        let queue = be::create_queue(&device).map_err(|e| error::to_string(&e))?;
        Ok(Gpu { device, queue, textures: Vec::new() })
    }

    /// A painter for `ui`'s glyph atlases, with the atlases uploaded whole.
    pub fn painter(&self, ui: &mut Ui) -> Painter {
        let text = ui.text_mut();
        let (mask, color) = (text.atlas_size(TextKind::Mask), text.atlas_size(TextKind::Color));
        let mut painter = Painter::new(&self.device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, mask, color).unwrap_or_else(|e| fail(e));
        for (kind, atlas, texel, (w, h)) in [(TextKind::Mask, AtlasKind::Mask, 1, mask), (TextKind::Color, AtlasKind::Color, 4, color)] {
            painter.upload(atlas, 0, 0, w, h, text.atlas_pixels(kind), (w * texel) as usize).unwrap_or_else(|e| fail(e));
        }
        text.drain_uploads(&mut |_, _, _, _, _, _, _| {});
        painter
    }

    /// An RGBA8 image `w` x `h` as a host image the UI can draw (`Ui::image`).
    pub fn image(&mut self, w: i64, h: i64, rgba: &[u8]) -> Image {
        let texture = be::create_texture(&self.device, TextureDescriptor {
            label: Some("pxui_gallery.image".into()), width: w, height: h, depth: 1, mip_levels: 1, sample_count: 1,
            format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopyDst],
        }).unwrap_or_else(|e| fail(e));
        be::write_texture(&texture, None, Origin { x: 0, y: 0, z: 0 }, Extent { width: w, height: h, depth: 1 }, w * 4, rgba).unwrap_or_else(|e| fail(e));
        self.textures.push(texture.clone());
        Image { handle: Rc::new(texture), width: w, height: h }
    }

    /// A render target `w` x `h` pixels that can be read back.
    pub fn target(&self, w: u32, h: u32) -> Texture {
        be::create_texture(&self.device, TextureDescriptor {
            label: Some("pxui_gallery.target".into()), width: w as i64, height: h as i64, depth: 1, mip_levels: 1, sample_count: 1,
            format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::RenderAttachment, TextureUsage::TextureBinding, TextureUsage::TextureCopySrc],
        }).unwrap_or_else(|e| fail(e))
    }

    /// The UI's last frame drawn over `ground` into a `size` x `density` texture: its RGBA pixels.
    pub fn render(&self, painter: &mut Painter, ui: &mut Ui, size: (u32, u32), density: u32, ground: [u8; 4]) -> Vec<u8> {
        let (w, h) = (size.0 * density, size.1 * density);
        let target = self.target(w, h);
        let commands = be::begin_commands(&self.queue).unwrap_or_else(|e| fail(e));
        let clear = ground.map(|c| c as f64 / 255.0);
        let encoder = be::render_encoder(&commands, &RenderTarget {
            colors: vec![ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear: (clear[0], clear[1], clear[2], clear[3]) }],
            depth: None,
        }).unwrap_or_else(|e| fail(e));
        paint(painter, ui, &encoder, [w, h], density as f32).unwrap_or_else(|e| fail(e));
        be::end_render(&encoder).unwrap_or_else(|e| fail(e));
        be::complete_through(&self.queue, be::commit(&commands).unwrap_or_else(|e| fail(e)).epoch).unwrap_or_else(|e| fail(e));
        let pixels = be::read_texture(&target, (w * 4) as i64).unwrap_or_else(|e| fail(e));
        be::destroy_texture(&target).unwrap_or_else(|e| fail(e));
        pixels
    }

    /// Release the device (after every painter and surface made from it).
    pub fn close(self) {
        for t in &self.textures {
            be::destroy_texture(t).unwrap_or_else(|e| fail(e));
        }
        be::destroy_queue(&self.queue).unwrap_or_else(|e| fail(e));
        be::destroy_device(&self.device).unwrap_or_else(|e| fail(e));
    }
}

/// Draw the UI's last frame as a host does, into the pass `encoder` is recording: new glyphs
/// uploaded, the view table set, each layer drawn retained with its dirty ranges (then marked
/// drawn), host images bound at their breaks.
pub fn paint(painter: &mut Painter, ui: &mut Ui, encoder: &RenderEncoder, size_px: [u32; 2], scale: f32) -> Result<(), Error> {
    let mut failed = None;
    ui.text_mut().drain_uploads(&mut |kind, x, y, w, h, bytes, stride| {
        let atlas = if kind == TextKind::Mask { AtlasKind::Mask } else { AtlasKind::Color };
        if let Err(e) = painter.upload(atlas, x, y, w, h, bytes, stride) {
            failed.get_or_insert(e);
        }
    });
    if let Some(e) = failed {
        return Err(e);
    }
    painter.begin_frame();
    for (index, v) in ui.output().views {
        painter.set_view(*index, View { offset: v.offset, opacity: v.opacity, clip: v.clip, scale: v.scale, transform: v.transform, pivot: v.pivot })?;
    }
    for index in 0..ui.output().draw.len() {
        let layer = &ui.output().draw[index];
        let breaks: Vec<Break> = layer.breaks.iter().map(|b| Break {
            at_instance: b.at_instance,
            texture: if b.image == pxui::draw::ATLAS { None } else { layer.images.get(b.image as usize).and_then(|i| i.handle.downcast_ref::<Texture>()) },
        }).collect();
        painter.draw_retained(encoder, size_px, scale, layer.id, &layer.instances, &layer.dirty, &breaks)?;
        ui.drawn(index);
    }
    Ok(())
}

/// A small test card: a diagonal gradient with a dark frame and a light disc, `w` x `h` RGBA8.
pub fn test_card(w: i64, h: i64) -> Vec<u8> {
    let mut px = Vec::with_capacity((w * h * 4) as usize);
    for y in 0..h {
        for x in 0..w {
            let (u, v) = (x as f32 / (w - 1) as f32, y as f32 / (h - 1) as f32);
            let (dx, dy) = (x as f32 - w as f32 / 2.0, y as f32 - h as f32 / 2.0);
            let disc = dx * dx + dy * dy < (h as f32 / 4.0).powi(2);
            let edge = x < 2 || y < 2 || x >= w - 2 || y >= h - 2;
            let c = if edge { [30, 34, 48] } else if disc { [250, 240, 220] } else { [(240.0 * u) as u8 + 15, (90.0 + 120.0 * v) as u8, (220.0 * (1.0 - u)) as u8 + 20] };
            px.extend_from_slice(&[c[0], c[1], c[2], 255]);
        }
    }
    px
}
