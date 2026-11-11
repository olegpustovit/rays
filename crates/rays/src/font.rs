//! Sketch text: PXUI shapes and rasterises it into per-context cached images; String errors.
use crate::image;
use renderer::side_lru::{self, Table, TableKey};
use rays_math::color;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RenderMode {
    Blended(color::T),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

// the pixels an image of a cached text holds
fn image_bytes(img: &image::Image) -> i64 {
    match (image::get_width(img), image::get_height(img)) {
        (Ok(width), Ok(height)) => width * height * 4,
        _ => 0,
    }
}

fn rgba(mode: RenderMode) -> (i64, i64, i64, i64) {
    let RenderMode::Blended(c) = mode;
    (c.r, c.g, c.b, c.a)
}

fn key(wrap: Option<i64>, align: Alignment, density: i64, size: i64, text: &str, mode: RenderMode) -> TableKey {
    let (r, g, b, a) = rgba(mode);
    TableKey::Str(format!("{density}|{size}|{wrap:?}|{align:?}|{r},{g},{b},{a}|{text}"))
}

// Sketch text: PXUI shapes and rasterises into its atlases; the glyph quads are composited on the
// CPU into one straight-alpha RGBA image per string (the scene's existing textured-quad path).
// ponytail: one image per string; glyph quads straight into the scene
// pass when text lowering gets an instanced route.
const SYSTEM_FONTS: [&str; 11] = [
    "/System/Library/Fonts/SFNSMono.ttf",
    "/System/Library/Fonts/SFNS.ttf",
    "/System/Library/Fonts/SFCompact.ttf",
    "/System/Library/Fonts/HelveticaNeue.ttc",
    "/System/Library/Fonts/Helvetica.ttc",
    "/System/Library/Fonts/LucidaGrande.ttc",
    "C:\\Windows\\Fonts\\segoeui.ttf",
    "C:\\Windows\\Fonts\\arial.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
];

thread_local! {
    // Text is shaped on whichever thread lowers the scene; cosmic-text is deterministic, so every
    // thread's instance agrees.
    static TEXT: RefCell<Option<Result<pxui::Text, String>>> = const { RefCell::new(None) };
}

fn text_engine() -> Option<pxui::Text> {
    let system = SYSTEM_FONTS.iter().find_map(|p| std::fs::read(p).ok());
    let mut fonts: Vec<&[u8]> = system.iter().map(|v| v.as_slice()).collect();
    fonts.push(pxui::text::pragmasevka());
    pxui::Text::new(&fonts)
}

fn paint_text(density: i64, wrap: Option<i64>, align: Alignment, size: i64, text: &str, mode: RenderMode) -> Result<image::Image, String> {
    if text.is_empty() {
        return image::create(1, 1, None);
    }
    let RenderMode::Blended(c) = mode;
    let style = pxui::TextStyle {
        size_points: size as f32,
        wrap: if wrap.is_some() { pxui::TextStyle::WRAP_WORD } else { pxui::TextStyle::WRAP_NONE },
        align: match align {
            Alignment::Left => pxui::TextStyle::ALIGN_LEFT,
            Alignment::Center => pxui::TextStyle::ALIGN_CENTER,
            Alignment::Right => pxui::TextStyle::ALIGN_RIGHT,
        },
        ..pxui::TextStyle::default()
    };
    TEXT.with(|t| {
        let mut t = t.borrow_mut();
        let t = t.get_or_insert_with(|| text_engine().ok_or_else(|| "PXUI text initialization failed".to_owned()))
            .as_mut().map_err(|error| error.clone())?;
        let (w, h, px) = t.rasterize_rgba(0, text, &style, wrap.map_or(0.0, |w| w as f32), density as f32,
            [c.r as u8, c.g as u8, c.b as u8, c.a as u8]).map_err(|e| format!("PXUI text rasterization failed: {e:?}"))?;
        let (w, h) = (i64::from(w), i64::from(h));
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
}
