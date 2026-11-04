//! Fonts and rendered text images, with per-font and per-context text caches.
//!
//! The OCaml `[`Msg of string]` errors are plain `String`s here.
use crate::image;
use crate::table::{self, Table, TableKey};
use rays_math::color;
use runtime_resources::Font as RFont;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum render_mode {
    Blended(color::T),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum hinting {
    Normal_hinting,
    Light_hinting,
    Mono_hinting,
    None_hinting,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum alignment {
    Left,
    Center,
    Right,
}

const TEXT_CACHE_CAPACITY: i64 = 256;

pub struct font_data {
    resource: RefCell<RFont::t>,
    generation: Cell<i64>,
    cache: RefCell<Table>,
}

/// An open font: clones share one font (the OCaml value's identity is `same`).
#[derive(Clone)]
pub struct t(Rc<font_data>);

pub fn same(a: &t, b: &t) -> bool {
    Rc::ptr_eq(&a.0, &b.0)
}

// the pixels an image of a cached text holds
fn image_bytes(img: &image::t) -> i64 {
    match (image::get_width(img), image::get_height(img)) {
        (Ok(width), Ok(height)) => width * height * 4,
        _ => 0,
    }
}

fn destroy_image_item(_key: &TableKey, item: table::Item) {
    if let Ok(img) = item.downcast::<image::t>() {
        let _ = image::destroy(&img);
    }
}

fn text_cache() -> Table {
    table::create(lru::cache("rays.font.text"), TEXT_CACHE_CAPACITY, None, Box::new(destroy_image_item))
}

fn message(operation: &str, error: &runtime_resources::error) -> String {
    format!("{operation}: {}", ogpu::error::to_string(error))
}

fn make(resource: Result<RFont::t, runtime_resources::error>) -> Result<t, String> {
    match resource {
        Ok(resource) => Ok(t(Rc::new(font_data {
            resource: RefCell::new(resource),
            generation: Cell::new(1),
            cache: RefCell::new(text_cache()),
        }))),
        Err(error) => Err(message("Font.load", &error)),
    }
}

pub fn load(path: &str, size: i64) -> Result<t, String> {
    make(RFont::open_file(path, size as f64))
}

pub fn system(size: Option<i64>) -> Result<t, String> {
    make(RFont::open_system(size.unwrap_or(16) as f64))
}

fn rgba(mode: render_mode) -> (i64, i64, i64, i64) {
    let render_mode::Blended(c) = mode;
    (c.r, c.g, c.b, c.a)
}

fn resource_align(a: alignment) -> RFont::alignment {
    match a {
        alignment::Left => RFont::alignment::Left,
        alignment::Center => RFont::alignment::Center,
        alignment::Right => RFont::alignment::Right,
    }
}

fn paint(density: i64, wrap: Option<i64>, align: alignment, font: &t, text: &str, mode: render_mode) -> Result<image::t, String> {
    let rendered = RFont::render(&mut font.0.resource.borrow_mut(), wrap, Some(resource_align(align)), density, rgba(mode), text.as_bytes());
    match rendered {
        Err(error) => Err(message("Font.render_text", &error)),
        Ok(None) => image::create(1, 1, None),
        Ok(Some(mut value)) => {
            // `into_image` hands the pixels over; the text is then dead, destroy is a no-op.
            let result = image_of_text(&mut value);
            let _ = runtime_resources::Text::destroy(&mut value);
            result
        }
    }
}

fn image_of_text(text: &mut runtime_resources::Text::t) -> Result<image::t, String> {
    match runtime_resources::Text::into_image(text) {
        Ok(img) => image::of_resource(img),
        Err(error) => Err(message("Font.image", &error)),
    }
}

fn key(wrap: Option<i64>, align: alignment, density: i64, size: i64, text: &str, mode: render_mode) -> TableKey {
    let (r, g, b, a) = rgba(mode);
    TableKey::Str(format!("{density}|{size}|{wrap:?}|{align:?}|{r},{g},{b},{a}|{text}"))
}

pub fn cached_text(wrap: Option<i64>, align: Option<alignment>, density: Option<i64>, font: &t, text: &str, mode: render_mode) -> Result<image::t, String> {
    let align = align.unwrap_or(alignment::Left);
    let density = density.unwrap_or(1);
    let k = key(wrap, align, density, 0, text, mode);
    if let Some(item) = table::find(&mut font.0.cache.borrow_mut(), &k) {
        if let Ok(img) = item.downcast::<image::t>() {
            return Ok((*img).clone());
        }
    }
    let img = paint(density, wrap, align, font, text, mode)?;
    table::add(&mut font.0.cache.borrow_mut(), image_bytes(&img), k, Rc::new(img.clone()), false);
    Ok(img)
}

fn clear_cache(font: &t) {
    table::clear(&mut font.0.cache.borrow_mut());
}

fn resource_hinting(h: hinting) -> RFont::hinting {
    match h {
        hinting::Normal_hinting => RFont::hinting::Normal_hinting,
        hinting::Light_hinting => RFont::hinting::Light_hinting,
        hinting::Mono_hinting => RFont::hinting::Mono_hinting,
        hinting::None_hinting => RFont::hinting::None_hinting,
    }
}

pub fn set_hinting(font: &t, value: hinting) -> Result<(), String> {
    let result = RFont::set_hinting(&mut font.0.resource.borrow_mut(), resource_hinting(value));
    match result {
        Ok(()) => {
            clear_cache(font);
            font.0.generation.set(font.0.generation.get() + 1);
            Ok(())
        }
        Err(error) => Err(message("Font.set_hinting", &error)),
    }
}

pub fn destroy(font: &t) {
    clear_cache(font);
    let _ = RFont::destroy(&mut font.0.resource.borrow_mut());
}

/// Vertical metrics in logical points; errors on a destroyed font.
pub fn get_ascent(font: &t) -> Result<i64, String> {
    match RFont::metrics(&mut font.0.resource.borrow_mut()) {
        Ok(m) => Ok(m.ascent),
        Err(error) => Err(message("Font.metrics", &error)),
    }
}

pub mod Raster {
    use super::*;

    struct automatic_entry {
        image: image::t,
        references: Cell<i64>,
        cached: Cell<bool>,
    }

    struct context_data {
        automatic_cache: RefCell<Table>,
        automatic_fonts: RefCell<Table>,
        automatic_references: Cell<i64>,
    }

    #[derive(Clone)]
    pub struct cache_context(Rc<context_data>);

    pub struct automatic {
        entry: Rc<automatic_entry>,
        context: Option<cache_context>,
        released: Cell<bool>,
    }

    const CAPACITY: i64 = 256;
    const FONT_CAPACITY: i64 = 32;

    thread_local! {
        static CURRENT_CACHE: RefCell<Option<cache_context>> = const { RefCell::new(None) };
    }

    pub fn create_cache_context() -> cache_context {
        cache_context(Rc::new(context_data {
            automatic_cache: RefCell::new(table::create(
                lru::cache("rays.font.text_ctx"),
                CAPACITY,
                None,
                Box::new(|_key, item| {
                    if let Ok(entry) = item.downcast::<automatic_entry>() {
                        entry.cached.set(false);
                        if entry.references.get() == 0 {
                            let _ = image::destroy(&entry.image);
                        }
                    }
                }),
            )),
            // count only: an open font per size, its memory in the font library
            automatic_fonts: RefCell::new(table::create(
                lru::cache("rays.font.auto"),
                FONT_CAPACITY,
                None,
                Box::new(|_key, item| {
                    if let Ok(font) = item.downcast::<super::t>() {
                        super::destroy(&font);
                    }
                }),
            )),
            automatic_references: Cell::new(0),
        }))
    }

    struct Restore(Option<cache_context>);

    impl Drop for Restore {
        fn drop(&mut self) {
            let previous = self.0.take();
            CURRENT_CACHE.with(|c| *c.borrow_mut() = previous);
        }
    }

    pub fn with_cache_context(context: &cache_context, f: &mut dyn FnMut()) {
        let previous = CURRENT_CACHE.with(|c| c.borrow_mut().replace(context.clone()));
        let _restore = Restore(previous);
        f()
    }

    pub fn clear_cache_context(context: &cache_context) {
        table::clear(&mut context.0.automatic_cache.borrow_mut());
        table::clear(&mut context.0.automatic_fonts.borrow_mut());
    }

    fn font(context: &cache_context, size: i64) -> Result<super::t, String> {
        let k = TableKey::Int(size);
        if let Some(item) = table::find(&mut context.0.automatic_fonts.borrow_mut(), &k) {
            if let Ok(value) = item.downcast::<super::t>() {
                return Ok((*value).clone());
            }
        }
        let value = system(Some(size))?;
        table::add(&mut context.0.automatic_fonts.borrow_mut(), 0, k, Rc::new(value.clone()), false);
        Ok(value)
    }

    pub fn borrow_automatic(
        wrap: Option<i64>,
        align: Option<alignment>,
        density: Option<i64>,
        size: i64,
        text: &str,
        mode: render_mode,
    ) -> Result<automatic, String> {
        let align = align.unwrap_or(alignment::Left);
        let density = density.unwrap_or(1);
        let k = key(wrap, align, density, size, text, mode);
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        let Some(context) = context else {
            let font = system(Some(size))?;
            let painted = paint(density, wrap, align, &font, text, mode);
            destroy(&font);
            return painted.map(|image| automatic {
                entry: Rc::new(automatic_entry { image, references: Cell::new(1), cached: Cell::new(false) }),
                context: None,
                released: Cell::new(false),
            });
        };
        let found = table::find(&mut context.0.automatic_cache.borrow_mut(), &k);
        if let Some(item) = found {
            if let Ok(entry) = item.downcast::<automatic_entry>() {
                entry.references.set(entry.references.get() + 1);
                table::set_pinned(&mut context.0.automatic_cache.borrow_mut(), &k, true);
                context.0.automatic_references.set(context.0.automatic_references.get() + 1);
                return Ok(automatic { entry, context: Some(context), released: Cell::new(false) });
            }
        }
        let font = font(&context, size)?;
        let image = paint(density, wrap, align, &font, text, mode)?;
        // Borrowed entries remain pinned until their scene is released.
        let can_cache = table::length(&mut context.0.automatic_cache.borrow_mut()) < CAPACITY
            || table::find_first(&mut context.0.automatic_cache.borrow_mut(), &|_, item| {
                item.downcast_ref::<automatic_entry>().is_some_and(|e| e.references.get() == 0)
            })
            .is_some();
        let entry = Rc::new(automatic_entry { image: image.clone(), references: Cell::new(1), cached: Cell::new(can_cache) });
        if can_cache {
            table::add(&mut context.0.automatic_cache.borrow_mut(), image_bytes(&image), k, entry.clone(), true);
        }
        context.0.automatic_references.set(context.0.automatic_references.get() + 1);
        Ok(automatic { entry, context: Some(context), released: Cell::new(false) })
    }

    pub fn automatic_image(handle: &automatic) -> image::t {
        handle.entry.image.clone()
    }

    pub fn release_automatic(handle: &automatic) {
        if handle.released.get() {
            return;
        }
        handle.released.set(true);
        let entry = &handle.entry;
        entry.references.set(entry.references.get() - 1);
        if let Some(context) = &handle.context {
            context.0.automatic_references.set(context.0.automatic_references.get() - 1);
        }
        if entry.references.get() == 0 {
            if entry.cached.get() {
                if let Some(context) = &handle.context {
                    unpin_entry(context, entry);
                }
            } else {
                let _ = image::destroy(&entry.image);
            }
        }
    }

    // the table is keyed by text: find this entry by scanning for its pointer
    fn unpin_entry(context: &cache_context, entry: &Rc<automatic_entry>) {
        let mut keys = Vec::new();
        table::iter(&mut context.0.automatic_cache.borrow_mut(), &mut |key, item| {
            if let Some(e) = item.downcast_ref::<automatic_entry>() {
                if std::ptr::eq(e, Rc::as_ptr(entry)) {
                    keys.push(key.clone());
                }
            }
        });
        for key in keys {
            table::set_pinned(&mut context.0.automatic_cache.borrow_mut(), &key, false);
        }
    }

    /// (cached texts, open fonts, outstanding borrows) of the current context.
    pub fn automatic_counts() -> (i64, i64, i64) {
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        match context {
            None => (0, 0, 0),
            Some(context) => (
                table::length(&mut context.0.automatic_cache.borrow_mut()),
                table::length(&mut context.0.automatic_fonts.borrow_mut()),
                context.0.automatic_references.get(),
            ),
        }
    }

    pub fn generation(font: &super::t) -> i64 {
        font.0.generation.get()
    }

    pub struct glyph {
        pub glyph_width: i64,
        pub glyph_height: i64,
        pub glyph_advance: i64,
        pub glyph_alpha: Vec<u8>,
    }

    /// Rasterize one code point at `density` exactly as it appears inside a rendered string;
    /// `glyph_advance` is the pen advance in backing pixels and `glyph_alpha` holds one coverage
    /// byte per pixel.
    pub fn glyph(density: Option<i64>, font: &super::t, code: i64) -> Result<glyph, String> {
        let density = density.unwrap_or(1);
        let Some(ch) = u32::try_from(code).ok().and_then(char::from_u32) else {
            return Err("Font.glyph: invalid code point".to_string());
        };
        let metrics = RFont::glyph_metrics_at(&mut font.0.resource.borrow_mut(), density, code).map_err(|e| message("Font.glyph", &e))?;
        let text = ch.to_string();
        let rendered = RFont::render(&mut font.0.resource.borrow_mut(), None, None, density, (255, 255, 255, 255), text.as_bytes())
            .map_err(|e| message("Font.glyph", &e))?;
        match rendered {
            None => Ok(glyph { glyph_width: 0, glyph_height: 0, glyph_advance: metrics.advance, glyph_alpha: Vec::new() }),
            Some(mut rendered) => {
                let size = runtime_resources::Text::size(&rendered);
                let pixels = runtime_resources::Text::pixels(&rendered);
                let _ = runtime_resources::Text::destroy(&mut rendered);
                match (size, pixels) {
                    (Ok((width, height)), Ok(rgba)) => Ok(glyph {
                        glyph_width: width,
                        glyph_height: height,
                        glyph_advance: metrics.advance,
                        glyph_alpha: (0..(width * height) as usize).map(|index| rgba[index * 4 + 3]).collect(),
                    }),
                    (Err(error), _) | (_, Err(error)) => Err(message("Font.glyph", &error)),
                }
            }
        }
    }
}
