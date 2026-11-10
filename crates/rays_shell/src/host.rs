//! PXUI's host services from Rays: the kit font and GPU images in, scene layers and platform
//! requests out.
use input::{frame::Frame, Event};
use ogpu_core::backend::{self, Texture};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::{ColorFormat, DepthFormat};
use ogpu_core::types::{Extent, Origin, TextureDescriptor, TextureFormat, TextureUsage};
use pxui::text::{self, Text};
use pxui::ui::{Image, Ui};
use pxui_render::{AtlasKind, Break, Painter};
use rays::{clipboard, image, scene, sketch};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::sync::OnceLock;

/// The kit's text engine: the face in the file at `path` (`RAYS_UI_FONT`), else the bundled
/// Pragmasevka. Fonts come only from these bytes; nothing scans the system.
pub fn text(path: Option<&str>) -> Text {
    let bytes = path.and_then(|p| std::fs::read(p).ok());
    Text::new(&[bytes.as_deref().unwrap_or(text::PRAGMASEVKA)])
}

/// Run a frame as the Rays host does: the clipboard text the last frame asked for arrives first,
/// and the clipboard and window-ground requests apply after.
pub fn frame(ui: &mut Ui, frame: &Frame, f: &mut dyn FnMut(&mut Ui)) {
    let pasted = if ui.output().platform.paste { clipboard::get_text().ok() } else { None };
    match pasted {
        Some(text) => {
            let mut with = frame.clone();
            with.input.events.insert(0, Event::Paste(text));
            ui.frame(&with, f);
        }
        None => {
            ui.frame(frame, f);
        }
    }
    let out = ui.output().platform;
    if let Some(text) = &out.clipboard {
        let _ = clipboard::set_text(text);
    }
    // no window, no effect
    if let Some(ground) = out.background {
        let _ = sketch::set_window_background(rays_math::color::rgba(ground.r, ground.g, ground.b, ground.a));
    }
    charge(ui);
}

/// Destroy the UI and take what it held off the memory ledger.
pub fn destroy(ui: &mut Ui) {
    ui.destroy();
    charge(ui);
}

/// The ledger follows what the UI reports holding (after every `frame`).
pub fn charge(ui: &mut Ui) {
    static ATLAS: OnceLock<mem::Owner> = OnceLock::new();
    static TEXT_UNDO: OnceLock<mem::Owner> = OnceLock::new();
    let now = ui.bytes();
    let was = std::mem::replace(&mut ui.charged, now);
    for (owner, was, now) in [
        (ATLAS.get_or_init(|| mem::plan("pxui.atlas")), was.atlas, now.atlas),
        (TEXT_UNDO.get_or_init(|| mem::document("pxui.text_undo")), was.text_undo, now.text_undo),
    ] {
        mem::charge(owner, now - was);
        mem::release(owner, was - now);
    }
}

/// The last frame as scene nodes: each layer after what `under` puts beneath its floating root
/// (by box key), then the focused text's input-method area. The renderer has `pxui_render`'s
/// painter draw each layer, borrowed from the UI, in its place in the frame's pass.
pub fn scene(ui: &Rc<RefCell<Ui>>, under: Option<&dyn Fn(i64) -> Vec<scene::Node>>) -> Vec<scene::Node> {
    let paint = painter(ui);
    let ui = ui.borrow();
    let out = ui.output();
    let mut nodes = Vec::new();
    for (index, layer) in out.draw.iter().enumerate() {
        if let Some(under) = under {
            nodes.extend(under(layer.key));
        }
        if !layer.instances.is_empty() {
            nodes.push(scene::layer::ui(paint.clone(), index, ui.draw_revision()));
        }
    }
    if let Some(((x, y, w, h), caret)) = out.platform.ime {
        nodes.push(scene::text_input_region((x, y), w, h, Some(true), Some(caret)).expect("PXUI input area"));
    }
    nodes
}

thread_local! {
    /// One paint closure per live UI, so equal frames give equal scene nodes.
    static PAINTS: RefCell<Vec<(Weak<RefCell<Ui>>, scene::UiPaint)>> = const { RefCell::new(Vec::new()) };
    static PAINTER: RefCell<Option<Shared>> = const { RefCell::new(None) };
}

fn painter(ui: &Rc<RefCell<Ui>>) -> scene::UiPaint {
    PAINTS.with(|paints| {
        let mut paints = paints.borrow_mut();
        let weak = Rc::downgrade(ui);
        if let Some((_, paint)) = paints.iter().find(|(w, _)| w.ptr_eq(&weak)) {
            return paint.clone();
        }
        paints.retain(|(w, _)| w.strong_count() > 0);
        let held = weak.clone();
        let paint: scene::UiPaint = Rc::new(move |pass: &scene::UiPass, layer: usize| match held.upgrade() {
            None => Ok(()),
            Some(ui) => match ui.try_borrow_mut() {
                Ok(mut ui) => paint_layer(&mut ui, pass, layer),
                Err(_) => Err(error::make("Pxui.paint", ErrorKind::InvalidState, "the UI is being changed while its frame renders")),
            },
        });
        paints.push((weak, paint.clone()));
        paint
    })
}

/// Draw calls the thread's painter has issued.
pub fn draw_calls() -> u64 {
    PAINTER.with(|shared| shared.borrow().as_ref().map_or(0, |s| s.painter.draw_calls()))
}

/// The thread's painter and what its atlases and textures hold.
// ponytail: one painter per thread, re-uploading the glyph atlases when another UI paints; two UIs
// drawn into one frame would share them. A painter per UI if that ever happens.
struct Shared {
    device: i64,
    painter: Painter,
    /// the UI whose glyph atlases the painter holds (0: none)
    owner: usize,
    frame: u64,
    /// image textures by identity, with their generation and the frame that last drew them
    images: Vec<(i64, i64, Texture, u64)>,
}

fn paint_layer(ui: &mut Ui, pass: &scene::UiPass, layer: usize) -> Result<(), Error> {
    if ui.output().draw.get(layer).is_none() {
        return Ok(());
    }
    PAINTER.with(|shared| {
        let mut shared = shared.borrow_mut();
        let device = ogpu_core::handle::device_id(backend::device_handle(pass.device));
        if shared.as_ref().is_none_or(|s| s.device != device) {
            let t = ui.text_mut();
            let (mask, color) = (t.atlas_size(text::AtlasKind::Mask), t.atlas_size(text::AtlasKind::Color));
            let painter = Painter::new(pass.device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, mask, color)?;
            *shared = Some(Shared { device, painter, owner: 0, frame: 0, images: Vec::new() });
        }
        let s = shared.as_mut().expect("painter");
        if s.frame != pass.frame {
            s.frame = pass.frame;
            s.painter.begin_frame();
            // an image no frame drew for a while goes
            let frame = pass.frame;
            s.images.retain(|(_, _, texture, used)| {
                let keep = frame.saturating_sub(*used) < 8;
                if !keep {
                    let _ = backend::destroy_texture(texture);
                }
                keep
            });
        }
        sync_atlas(s, ui)?;
        let layer = &ui.output().draw[layer];
        if layer.breaks.is_empty() {
            return s.painter.draw(pass.encoder, pass.size_px, pass.scale, &layer.instances, &[]);
        }
        for image in &layer.images {
            image_texture(s, pass, image)?;
        }
        let textures: Vec<&Texture> = layer.images.iter().map(|i| {
            let id = as_rays_image(i).map_or(0, image::identity);
            s.images.iter().find(|(key, ..)| *key == id).map(|(_, _, texture, _)| texture).expect("image texture")
        }).collect();
        let breaks: Vec<Break> = layer.breaks.iter().map(|b| Break { at_instance: b.at_instance, texture: (b.image != pxui::draw::ATLAS).then(|| textures[b.image as usize]) }).collect();
        s.painter.draw(pass.encoder, pass.size_px, pass.scale, &layer.instances, &breaks)
    })
}

/// The UI's glyph atlases into the painter's: the rectangles rasterised since the last upload, or
/// both atlases whole when the painter last held another UI's.
fn sync_atlas(s: &mut Shared, ui: &mut Ui) -> Result<(), Error> {
    let owner = ui as *const Ui as usize;
    let t = ui.text_mut();
    let kind = |k: text::AtlasKind| if k == text::AtlasKind::Mask { AtlasKind::Mask } else { AtlasKind::Color };
    if s.owner != owner {
        s.owner = owner;
        for (k, texel) in [(text::AtlasKind::Mask, 1), (text::AtlasKind::Color, 4)] {
            let (w, h) = t.atlas_size(k);
            s.painter.upload(kind(k), 0, 0, w, h, t.atlas_pixels(k), w as usize * texel)?;
        }
        t.drain_uploads(&mut |_, _, _, _, _, _, _| {});
        return Ok(());
    }
    let mut result = Ok(());
    t.drain_uploads(&mut |k, x, y, w, h, bytes, stride| {
        if result.is_ok() {
            result = s.painter.upload(kind(k), x, y, w, h, bytes, stride);
        }
    });
    result
}

/// A texture holding the image's pixels at their generation.
// ponytail: images are read back to the CPU and uploaded once per generation (the inspector's
// one thumbnail); sample a GPU image's own texture if UI images become many or large.
fn image_texture(s: &mut Shared, pass: &scene::UiPass, i: &Image) -> Result<(), Error> {
    let Some(rays_image) = as_rays_image(i) else {
        return Err(error::make("Pxui.paint", ErrorKind::InvalidArgument, "PXUI image from another backend"));
    };
    let (id, generation) = (image::identity(rays_image), image::generation(rays_image));
    if let Some(entry) = s.images.iter_mut().find(|(key, ..)| *key == id) {
        entry.3 = pass.frame;
        if entry.1 == generation {
            return Ok(());
        }
        let _ = backend::destroy_texture(&entry.2);
        s.images.retain(|(key, ..)| *key != id);
    }
    let (width, height) = image::dimensions(rays_image);
    let pixels = image::pixels(rays_image).map_err(|m| error::make("Pxui.paint", ErrorKind::InvalidState, &m))?;
    let texture = backend::create_texture(pass.device, TextureDescriptor {
        label: Some("pxui.image".into()), width, height, depth: 1, mip_levels: 1, sample_count: 1,
        format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopyDst],
    })?;
    backend::write_texture(&texture, None, Origin { x: 0, y: 0, z: 0 }, Extent { width, height, depth: 1 }, width * 4, &pixels)?;
    s.images.push((id, generation, texture, pass.frame));
    Ok(())
}

/// A Rays image as PXUI's handle, for `Paint::image`.
pub fn image(i: &image::Image) -> Image {
    let (width, height) = image::dimensions(i);
    Image { handle: Rc::new(i.clone()), width, height }
}

/// The Rays image behind a PXUI handle (`None` for another backend's).
pub fn as_rays_image(i: &Image) -> Option<&image::Image> {
    i.handle.downcast_ref::<image::Image>()
}
