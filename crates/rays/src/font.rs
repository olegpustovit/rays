//! Fonts and rendered images use per-font/per-context caches with String errors.
use crate::image;
use renderer::side_lru::{self, Table, TableKey};
use rays_math::color;
use runtime_resources::font as RFont;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RenderMode {
    Blended(color::T),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hinting {
    NormalHinting,
    LightHinting,
    MonoHinting,
    NoneHinting,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

const TEXT_CACHE_CAPACITY: i64 = 256;

pub struct FontData {
    resource: RefCell<RFont::Font>,
    generation: Cell<i64>,
    cache: RefCell<Table>,
}

/// Open font clones share one font; same tests that owner identity.
#[derive(Clone)]
pub struct Font(Rc<FontData>);

pub fn same(a: &Font, b: &Font) -> bool {
    Rc::ptr_eq(&a.0, &b.0)
}

// the pixels an image of a cached text holds
fn image_bytes(img: &image::Image) -> i64 {
    match (image::get_width(img), image::get_height(img)) {
        (Ok(width), Ok(height)) => width * height * 4,
        _ => 0,
    }
}

fn destroy_image_item(_key: &TableKey, item: side_lru::Item) {
    if let Ok(img) = item.downcast::<image::Image>() {
        let _ = image::destroy(&img);
    }
}

fn text_cache() -> Table {
    side_lru::create(lru::cache("rays.font.text"), true, side_lru::UNBOUNDED_BYTES, Box::new(destroy_image_item), TEXT_CACHE_CAPACITY)
}

fn message(operation: &str, error: &runtime_resources::Error) -> String {
    format!("{operation}: {}", ogpu_core::error::to_string(error))
}

fn make(resource: Result<RFont::Font, runtime_resources::Error>) -> Result<Font, String> {
    match resource {
        Ok(resource) => Ok(Font(Rc::new(FontData {
            resource: RefCell::new(resource),
            generation: Cell::new(1),
            cache: RefCell::new(text_cache()),
        }))),
        Err(error) => Err(message("Font.load", &error)),
    }
}

pub fn load(path: &str, size: i64) -> Result<Font, String> {
    make(RFont::open_file(path, size as f64))
}

pub fn system(size: Option<i64>) -> Result<Font, String> {
    make(RFont::open_system(size.unwrap_or(16) as f64))
}

fn rgba(mode: RenderMode) -> (i64, i64, i64, i64) {
    let RenderMode::Blended(c) = mode;
    (c.r, c.g, c.b, c.a)
}

fn resource_align(a: Alignment) -> RFont::Alignment {
    match a {
        Alignment::Left => RFont::Alignment::Left,
        Alignment::Center => RFont::Alignment::Center,
        Alignment::Right => RFont::Alignment::Right,
    }
}

fn paint(density: i64, wrap: Option<i64>, align: Alignment, font: &Font, text: &str, mode: RenderMode) -> Result<image::Image, String> {
    let rendered = RFont::render(&mut font.0.resource.borrow_mut(), wrap, Some(resource_align(align)), density, rgba(mode), text.as_bytes());
    match rendered {
        Err(error) => Err(message("Font.render_text", &error)),
        Ok(None) => image::create(1, 1, None),
        Ok(Some(mut value)) => {
            // `into_image` hands the pixels over; the text is then dead, destroy is a no-op.
            let result = image_of_text(&mut value);
            let _ = runtime_resources::text::destroy(&mut value);
            result
        }
    }
}

fn image_of_text(text: &mut runtime_resources::text::Text) -> Result<image::Image, String> {
    match runtime_resources::text::into_image(text) {
        Ok(img) => image::of_resource(img),
        Err(error) => Err(message("Font.image", &error)),
    }
}

fn key(wrap: Option<i64>, align: Alignment, density: i64, size: i64, text: &str, mode: RenderMode) -> TableKey {
    let (r, g, b, a) = rgba(mode);
    TableKey::Str(format!("{density}|{size}|{wrap:?}|{align:?}|{r},{g},{b},{a}|{text}"))
}

pub fn cached_text(wrap: Option<i64>, align: Option<Alignment>, density: Option<i64>, font: &Font, text: &str, mode: RenderMode) -> Result<image::Image, String> {
    let align = align.unwrap_or(Alignment::Left);
    let density = density.unwrap_or(1);
    let k = key(wrap, align, density, 0, text, mode);
    if let Some(item) = side_lru::find(&mut font.0.cache.borrow_mut(), &k)
        && let Ok(img) = item.downcast::<image::Image>() {
            return Ok((*img).clone());
        }
    let img = paint(density, wrap, align, font, text, mode)?;
    side_lru::add(&mut font.0.cache.borrow_mut(), image_bytes(&img), k, Rc::new(img.clone()));
    Ok(img)
}

fn clear_cache(font: &Font) {
    side_lru::clear(&mut font.0.cache.borrow_mut());
}

fn resource_hinting(h: Hinting) -> RFont::Hinting {
    match h {
        Hinting::NormalHinting => RFont::Hinting::NormalHinting,
        Hinting::LightHinting => RFont::Hinting::LightHinting,
        Hinting::MonoHinting => RFont::Hinting::MonoHinting,
        Hinting::NoneHinting => RFont::Hinting::NoneHinting,
    }
}

pub fn set_hinting(font: &Font, value: Hinting) -> Result<(), String> {
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

pub fn destroy(font: &Font) {
    clear_cache(font);
    let _ = RFont::destroy(&mut font.0.resource.borrow_mut());
}

/// Vertical metrics in logical points; errors on a destroyed font.
pub fn get_ascent(font: &Font) -> Result<i64, String> {
    match RFont::metrics(&mut font.0.resource.borrow_mut()) {
        Ok(m) => Ok(m.ascent),
        Err(error) => Err(message("Font.metrics", &error)),
    }
}


// Sketch text: pxui_text shapes and rasterises into its atlases; the glyph quads are composited on the
// CPU into one straight-alpha RGBA image per string (the scene's existing textured-quad path).
// ponytail: one image per string, like the SDL_ttf path; glyph quads straight into the scene
// pass when text lowering gets an instanced route.
const SYSTEM_FONTS: [&str; 8] = [
    "/System/Library/Fonts/SFNSMono.ttf",
    "/System/Library/Fonts/SFNS.ttf",
    "/System/Library/Fonts/SFCompact.ttf",
    "/System/Library/Fonts/HelveticaNeue.ttc",
    "/System/Library/Fonts/Helvetica.ttc",
    "/System/Library/Fonts/LucidaGrande.ttc",
    "C:\\Windows\\Fonts\\segoeui.ttf",
    "C:\\Windows\\Fonts\\arial.ttf",
];
const BUNDLED: &[u8] = include_bytes!("../../../assets/fonts/Pragmasevka-Regular.ttf");

thread_local! {
    // Text is shaped on whichever thread lowers the scene; cosmic-text is deterministic, so every
    // thread's instance agrees.
    static TEXT: RefCell<Option<pxui_text::Text>> = const { RefCell::new(None) };
}

fn text_engine() -> pxui_text::Text {
    let system = SYSTEM_FONTS.iter().find_map(|p| std::fs::read(p).ok());
    let mut fonts: Vec<&[u8]> = system.iter().map(|v| v.as_slice()).collect();
    fonts.push(BUNDLED);
    pxui_text::Text::new(&fonts)
}

/// `over` of straight-alpha `src` onto `dst`.
fn over(dst: &mut [u8], src: [u8; 4]) {
    let (sa, da) = (src[3] as u32, dst[3] as u32);
    let oa = sa * 255 + da * (255 - sa);
    if oa == 0 {
        return;
    }
    for i in 0..3 {
        dst[i] = ((src[i] as u32 * sa * 255 + dst[i] as u32 * da * (255 - sa)) / oa) as u8;
    }
    dst[3] = ((oa + 127) / 255) as u8;
}

fn paint_text(density: i64, wrap: Option<i64>, align: Alignment, size: i64, text: &str, mode: RenderMode) -> Result<image::Image, String> {
    use pxui_text::{AtlasKind, Style, Wrap};
    if text.is_empty() {
        return image::create(1, 1, None);
    }
    let RenderMode::Blended(c) = mode;
    let (ink, ca, scale) = ([c.r as u8, c.g as u8, c.b as u8], c.a as u32, density as f32);
    let style = Style {
        size: size as f32,
        wrap: if wrap.is_some() { Wrap::Word } else { Wrap::None },
        align: match align {
            Alignment::Left => pxui_text::Align::Left,
            Alignment::Center => pxui_text::Align::Center,
            Alignment::Right => pxui_text::Align::Right,
        },
        ..Style::default()
    };
    TEXT.with(|t| {
        let mut t = t.borrow_mut();
        let t = t.get_or_insert_with(text_engine);
        let m = t.layout(0, text, &style, wrap.map_or(0.0, |w| w as f32));
        // a wrapped string is as wide as its wrap width, so alignment has room to work
        let w = wrap.map_or((m.w * scale).ceil() as i64, |w| w * density).max(1);
        let h = ((m.h * scale).ceil() as i64).max(1);
        let mut px = vec![0u8; (w * h * 4) as usize];
        let mut quads = Vec::new();
        t.glyphs(0, (0.0, 0.0), scale, &mut |g| quads.push(g));
        t.drain_uploads(&mut |_, _, _, _, _, _, _| {});
        for g in quads {
            let (aw, ah) = t.atlas_size(g.atlas);
            let atlas = t.atlas_pixels(g.atlas);
            let (u, v) = ((g.uv.x * aw as f32).round() as i64, (g.uv.y * ah as f32).round() as i64);
            let (gw, gh, gx, gy) = (g.rect.w as i64, g.rect.h as i64, g.rect.x as i64, g.rect.y as i64);
            for row in 0..gh {
                let y = gy + row;
                if y < 0 || y >= h {
                    continue;
                }
                for col in 0..gw {
                    let x = gx + col;
                    if x < 0 || x >= w {
                        continue;
                    }
                    let at = (v + row) * aw as i64 + u + col;
                    let src = match g.atlas {
                        AtlasKind::Mask => [ink[0], ink[1], ink[2], ((atlas[at as usize] as u32 * ca + 127) / 255) as u8],
                        AtlasKind::Color => {
                            let i = at as usize * 4;
                            [atlas[i], atlas[i + 1], atlas[i + 2], ((atlas[i + 3] as u32 * ca + 127) / 255) as u8]
                        }
                    };
                    let o = ((y * w + x) * 4) as usize;
                    over(&mut px[o..o + 4], src);
                }
            }
        }
        image::upload_rgba(None, w, h, &px)
    })
}

pub mod raster {
    use super::paint_text;
    use super::*;

    struct AutomaticEntry {
        image: image::Image,
        references: Cell<i64>,
        cached: Cell<bool>,
    }

    struct ContextData {
        automatic_cache: RefCell<Table>,
        automatic_references: Cell<i64>,
    }

    #[derive(Clone)]
    pub struct CacheContext(Rc<ContextData>);

    pub struct Automatic {
        entry: Rc<AutomaticEntry>,
        context: Option<CacheContext>,
        released: Cell<bool>,
    }

    const CAPACITY: i64 = 256;

    thread_local! {
        static CURRENT_CACHE: RefCell<Option<CacheContext>> = const { RefCell::new(None) };
    }

    pub fn create_cache_context() -> CacheContext {
        CacheContext(Rc::new(ContextData {
            automatic_cache: RefCell::new(side_lru::create(
                lru::cache("rays.font.text_ctx"),
                true,
                side_lru::UNBOUNDED_BYTES,
                Box::new(|_key, item| {
                    if let Ok(entry) = item.downcast::<AutomaticEntry>() {
                        entry.cached.set(false);
                        if entry.references.get() == 0 {
                            let _ = image::destroy(&entry.image);
                        }
                    }
                }),
                CAPACITY,
            )),
            automatic_references: Cell::new(0),
        }))
    }

    struct Restore(Option<CacheContext>);

    impl Drop for Restore {
        fn drop(&mut self) {
            let previous = self.0.take();
            CURRENT_CACHE.with(|c| *c.borrow_mut() = previous);
        }
    }

    pub fn with_cache_context(context: &CacheContext, f: &mut dyn FnMut()) {
        let previous = CURRENT_CACHE.with(|c| c.borrow_mut().replace(context.clone()));
        let _restore = Restore(previous);
        f()
    }

    pub fn clear_cache_context(context: &CacheContext) {
        side_lru::clear(&mut context.0.automatic_cache.borrow_mut());
    }

    pub(crate) fn borrow_automatic(
        wrap: Option<i64>,
        align: Option<Alignment>,
        density: Option<i64>,
        size: i64,
        text: &str,
        mode: RenderMode,
    ) -> Result<Automatic, String> {
        let align = align.unwrap_or(Alignment::Left);
        let density = density.unwrap_or(1);
        let k = key(wrap, align, density, size, text, mode);
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        let Some(context) = context else {
            return paint_text(density, wrap, align, size, text, mode).map(|image| Automatic {
                entry: Rc::new(AutomaticEntry { image, references: Cell::new(1), cached: Cell::new(false) }),
                context: None,
                released: Cell::new(false),
            });
        };
        let found = side_lru::find(&mut context.0.automatic_cache.borrow_mut(), &k);
        if let Some(item) = found
            && let Ok(entry) = item.downcast::<AutomaticEntry>() {
                entry.references.set(entry.references.get() + 1);
                side_lru::set_pinned(&mut context.0.automatic_cache.borrow_mut(), &k, true);
                context.0.automatic_references.set(context.0.automatic_references.get() + 1);
                return Ok(Automatic { entry, context: Some(context), released: Cell::new(false) });
            }
        let image = paint_text(density, wrap, align, size, text, mode)?;
        // Borrowed entries remain pinned until their scene is released.
        let can_cache = side_lru::length(&mut context.0.automatic_cache.borrow_mut()) < CAPACITY
            || side_lru::find_first(&mut context.0.automatic_cache.borrow_mut(), &|_, item| {
                item.downcast_ref::<AutomaticEntry>().is_some_and(|e| e.references.get() == 0)
            })
            .is_some();
        let entry = Rc::new(AutomaticEntry { image: image.clone(), references: Cell::new(1), cached: Cell::new(can_cache) });
        if can_cache {
            side_lru::add_pinned(&mut context.0.automatic_cache.borrow_mut(), image_bytes(&image), k, entry.clone(), true);
        }
        context.0.automatic_references.set(context.0.automatic_references.get() + 1);
        Ok(Automatic { entry, context: Some(context), released: Cell::new(false) })
    }

    pub(crate) fn automatic_image(handle: &Automatic) -> image::Image {
        handle.entry.image.clone()
    }

    pub(crate) fn release_automatic(handle: &Automatic) {
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
    fn unpin_entry(context: &CacheContext, entry: &Rc<AutomaticEntry>) {
        let mut keys = Vec::new();
        side_lru::iter(&mut context.0.automatic_cache.borrow_mut(), &mut |key, item| {
            if let Some(e) = item.downcast_ref::<AutomaticEntry>()
                && std::ptr::eq(e, Rc::as_ptr(entry)) {
                    keys.push(key.clone());
                }
        });
        for key in keys {
            side_lru::set_pinned(&mut context.0.automatic_cache.borrow_mut(), &key, false);
        }
    }

    /// (cached texts, outstanding borrows) of the current context.
    pub fn automatic_counts() -> (i64, i64) {
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        match context {
            None => (0, 0),
            Some(context) => (side_lru::length(&mut context.0.automatic_cache.borrow_mut()), context.0.automatic_references.get()),
        }
    }

    pub fn generation(font: &super::Font) -> i64 {
        font.0.generation.get()
    }

    pub struct Glyph {
        pub glyph_width: i64,
        pub glyph_height: i64,
        pub glyph_advance: i64,
        pub glyph_alpha: Vec<u8>,
    }

    /// Rasterize one code point at `density` exactly as it appears inside a rendered string;
    /// `glyph_advance` is the pen advance in backing pixels and `glyph_alpha` holds one coverage
    /// byte per pixel.
    pub fn glyph(density: Option<i64>, font: &super::Font, code: i64) -> Result<Glyph, String> {
        let density = density.unwrap_or(1);
        let Some(ch) = u32::try_from(code).ok().and_then(char::from_u32) else {
            return Err("Font.glyph: invalid code point".to_string());
        };
        let metrics = RFont::glyph_metrics_at(&mut font.0.resource.borrow_mut(), density, code).map_err(|e| message("Font.glyph", &e))?;
        let text = ch.to_string();
        let rendered = RFont::render(&mut font.0.resource.borrow_mut(), None, None, density, (255, 255, 255, 255), text.as_bytes())
            .map_err(|e| message("Font.glyph", &e))?;
        match rendered {
            None => Ok(Glyph { glyph_width: 0, glyph_height: 0, glyph_advance: metrics.advance, glyph_alpha: Vec::new() }),
            Some(mut rendered) => {
                let size = runtime_resources::text::size(&rendered);
                let pixels = runtime_resources::text::pixels(&rendered);
                let _ = runtime_resources::text::destroy(&mut rendered);
                match (size, pixels) {
                    (Ok((width, height)), Ok(rgba)) => Ok(Glyph {
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
