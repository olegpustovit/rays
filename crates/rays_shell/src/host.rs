//! PXUI's host services from Rays: SDL_ttf faces and GPU images in, scene layers and platform
//! requests out.
use input::{frame::Frame, Event};
use pxui::ui::{Backend, Font, Image, Raster, Ui};
use rays::{clipboard, font, image, scene, sketch};
use std::rc::Rc;
use std::sync::OnceLock;

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
/// (by box key), then the focused text's input-method area.
pub fn scene(ui: &Ui, under: Option<&dyn Fn(i64) -> Vec<scene::Node>>) -> Vec<scene::Node> {
    let out = ui.output();
    let mut nodes = Vec::new();
    for layer in out.draw {
        if let Some(under) = under {
            nodes.extend(under(layer.key));
        }
        if layer.batch.count() != 0 {
            let images = layer.images.iter().filter_map(|(id, i)| as_rays_image(i).map(|r| (*id, r.clone()))).collect();
            nodes.push(scene::layer::ui(Some(images), layer.batch.clone()).expect("PXUI layer"));
        }
    }
    if let Some(((x, y, w, h), caret)) = out.platform.ime {
        nodes.push(scene::text_input_region((x, y), w, h, Some(true), Some(caret)).expect("PXUI input area"));
    }
    nodes
}

fn face(f: &Font) -> &font::Font {
    f.0.downcast_ref::<font::Font>().expect("PXUI font from another backend")
}

fn rays_image(i: &Image) -> &image::Image {
    i.handle.downcast_ref::<image::Image>().expect("PXUI image from another backend")
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

fn open_font(path: Option<&str>, size: i64) -> Option<Font> {
    let opened = match path {
        Some(path) => font::load(path, size).ok().inspect(|f| {
            let _ = font::set_hinting(f, font::Hinting::NoneHinting);
        }),
        None => font::system(Some(size)).ok(),
    };
    opened.map(|f| Font(Rc::new(f)))
}

fn glyph(f: &Font, density: i64, code: i64) -> Option<Raster> {
    let g = font::raster::glyph(Some(density), face(f), code).ok()?;
    Some(Raster { width: g.glyph_width, height: g.glyph_height, advance: g.glyph_advance, alpha: g.glyph_alpha })
}

fn upload_rgba(into: Option<&Image>, width: i64, height: i64, rgba: &[u8]) -> Option<Image> {
    let uploaded = image::upload_rgba(into.map(rays_image), width, height, rgba).ok()?;
    Some(match into {
        Some(i) => Image { handle: i.handle.clone(), width, height },
        None => Image { handle: Rc::new(uploaded), width, height },
    })
}

pub static BACKEND: Backend = Backend {
    open_font,
    close_font: |f| font::destroy(face(f)),
    ascent: |f| font::get_ascent(face(f)).unwrap_or(0),
    glyph,
    upload_rgba,
    destroy_image: |i| {
        let _ = image::destroy(rays_image(i));
    },
};
