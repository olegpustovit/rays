//! PXUI's host services from Rays: SDL_ttf faces, GPU images.
use pxui::ui::{Backend, Font, Image, Raster};
use rays::{font, image};
use std::rc::Rc;

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
