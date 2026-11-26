//! Sketch text: PXUI glyph atlas; changing strings cost only atlas misses. String errors.
use crate::image;
use renderer::side_lru::{self, Table, TableKey};
use rays_math::color;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
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

fn rgba(mode: RenderMode) -> (i64, i64, i64, i64) {
    let RenderMode::Blended(c) = mode;
    (c.r, c.g, c.b, c.a)
}

fn key(wrap: Option<i64>, align: Alignment, density: i64, size: i64, text: &str, mode: RenderMode, style: i64) -> TableKey {
    let (r, g, b, a) = rgba(mode);
    TableKey::Str(format!("{density}|{size}|{style}|{wrap:?}|{align:?}|{r},{g},{b},{a}|{text}"))
}

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

/// One glyph's atlas UV and logical destination (shared atlas `Image` lives on `TextDraw::Atlas`).
#[derive(Clone, Copy)]
pub struct GlyphQuad {
    pub source: (f64, f64, f64, f64),
    pub dest: (f64, f64, f64, f64),
}

/// A shaped string: atlas quads (preferred) or a single composited image (wrap / tracking fallback).
/// Atlas quads are `Rc` so cloning a draw (scene commands + resources) is a refcount bump.
#[derive(Clone)]
pub enum TextDraw {
    Atlas { image: image::Image, width: i64, height: i64, quads: std::rc::Rc<[GlyphQuad]> },
    Image(image::Image),
}

struct AtlasPage {
    image: image::Image,
    pixels: Vec<u8>,
    width: i64,
    height: i64,
    pen_x: i64,
    pen_y: i64,
    row_h: i64,
}

impl AtlasPage {
    fn new(size: i64) -> Result<Self, String> {
        let pixels = vec![0u8; (size * size * 4) as usize];
        let image = image::upload_rgba(None, size, size, &pixels)?;
        Ok(Self { image, pixels, width: size, height: size, pen_x: 0, pen_y: 0, row_h: 0 })
    }

    fn pack(&mut self, gw: i64, gh: i64, rgba: &[u8]) -> Result<(f64, f64, f64, f64), String> {
        if gw > self.width || gh > self.height {
            return Err("glyph larger than atlas page".into());
        }
        if self.pen_x + gw > self.width {
            self.pen_x = 0;
            self.pen_y += self.row_h;
            self.row_h = 0;
        }
        if self.pen_y + gh > self.height {
            return Err("atlas page full".into());
        }
        let (x, y) = (self.pen_x, self.pen_y);
        for row in 0..gh {
            let src = (row * gw * 4) as usize;
            let dst = ((y + row) * self.width * 4 + x * 4) as usize;
            self.pixels[dst..dst + (gw * 4) as usize].copy_from_slice(&rgba[src..src + (gw * 4) as usize]);
        }
        self.pen_x += gw;
        self.row_h = self.row_h.max(gh);
        image::upload_rgba(Some(&self.image), self.width, self.height, &self.pixels)?;
        Ok((x as f64, y as f64, gw as f64, gh as f64))
    }
}

struct TextEngine {
    ui: pxui::Ui,
    faces: Vec<u16>,
    scratch: Vec<u8>,
    glyphs: HashMap<(u16, i64, i64, u32, char), (usize, (f64, f64, f64, f64))>,
    pages: Vec<AtlasPage>,
    advance: HashMap<(u16, i64, i64, char), f32>,
    rects: Vec<GlyphQuad>,
    quad_pool: Vec<Rc<[GlyphQuad]>>,
}

thread_local! {
    static TEXT: RefCell<Option<Result<TextEngine, String>>> = const { RefCell::new(None) };
}

const FACES: [&[u8]; 11] = [
    include_bytes!("../../../pxui/assets/fonts/DepartureMono-Regular.otf"),
    include_bytes!("../fonts/ArchivoSheet-Regular.ttf"), include_bytes!("../fonts/ArchivoSheet-SemiBold.ttf"),
    include_bytes!("../fonts/ArchivoSheet-CondensedBold.ttf"), include_bytes!("../fonts/ArchivoSheet-ExpandedExtraBold.ttf"),
    include_bytes!("../fonts/NewsreaderPage-Regular.ttf"), include_bytes!("../fonts/NewsreaderPage-SemiBold.ttf"), include_bytes!("../fonts/NewsreaderPage-Display.ttf"),
    include_bytes!("../fonts/IBMPlexSansCondensed-Regular.ttf"), include_bytes!("../fonts/IBMPlexSansCondensed-SemiBold.ttf"), include_bytes!("../fonts/IBMPlexMono-Regular.ttf"),
];

fn text_engine() -> Option<TextEngine> {
    let system = SYSTEM_FONTS.iter().find_map(|p| std::fs::read(p).ok());
    let mut fonts: Vec<&[u8]> = system.iter().map(|v| v.as_slice()).collect();
    fonts.push(pxui::pragmasevka());
    let first_named = fonts.len() as u16 - 1;
    fonts.extend(FACES);
    let faces = (first_named..fonts.len() as u16).collect();
    let fonts: Vec<_> = fonts.into_iter().map(pxui::FontData).collect();
    let ui = pxui::Ui::new(&pxui::UiDesc::DEFAULT.fonts(&fonts)).ok()?;
    Some(TextEngine { ui, faces, scratch: Vec::new(), glyphs: HashMap::new(), pages: Vec::new(), advance: HashMap::new(), rects: Vec::new(), quad_pool: Vec::new() })
}

fn color_u32(c: color::T) -> u32 {
    ((c.r as u32) << 24) | ((c.g as u32) << 16) | ((c.b as u32) << 8) | (c.a as u32)
}

fn label_style(align: Alignment, size: i64, wrap: bool, face: u16, tracking: f32) -> pxui::Label {
    let style = pxui::Label::BODY.size(size as f32).align(match align {
        Alignment::Left => pxui::TextAlign::Start,
        Alignment::Center => pxui::TextAlign::Center,
        Alignment::Right => pxui::TextAlign::End,
    }).font(face).tracking(tracking);
    if wrap { style.wrap() } else { style }
}

fn paint_fallback(engine: &mut TextEngine, density: i64, wrap: Option<i64>, align: Alignment, size: i64, text: &str, mode: RenderMode, run: i64) -> Result<TextDraw, String> {
    let RenderMode::Blended(c) = mode;
    let (face_idx, _, tracking) = scene_command::marks::text_style_parts(run);
    let face = if run == 0 { 0 } else {
        *engine.faces.get(face_idx).ok_or_else(|| format!("Text face {face_idx} is not loaded"))?
    };
    let tracking = if run == 0 { 0.0 } else { tracking as f32 / size as f32 };
    let style = label_style(align, size, wrap.is_some(), face, tracking);
    let (w, h) = engine.ui.text_service().rasterize_rgba(text, style, wrap.map(|w| w as f32), density as f32,
        [c.r as u8, c.g as u8, c.b as u8, c.a as u8], &mut engine.scratch).map_err(|e| format!("PXUI text rasterization failed: {e:?}"))?;
    Ok(TextDraw::Image(image::upload_rgba(None, i64::from(w), i64::from(h), &engine.scratch)?))
}

fn glyph_slot(engine: &mut TextEngine, face: u16, size: i64, density: i64, c: color::T, ch: char, style: pxui::Label) -> Result<(usize, (f64, f64, f64, f64)), String> {
    let key = (face, size, density, color_u32(c), ch);
    if let Some(&(page, src)) = engine.glyphs.get(&key) {
        return Ok((page, src));
    }
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    let (w, h) = engine.ui.text_service().rasterize_rgba(s, style, None, density as f32,
        [c.r as u8, c.g as u8, c.b as u8, c.a as u8], &mut engine.scratch).map_err(|e| format!("PXUI glyph rasterization failed: {e:?}"))?;
    let (gw, gh) = (i64::from(w), i64::from(h));
    if engine.pages.is_empty() {
        engine.pages.push(AtlasPage::new(512)?);
    }
    let mut page = engine.pages.len() - 1;
    let src = match engine.pages[page].pack(gw, gh, &engine.scratch) {
        Ok(src) => src,
        Err(_) => {
            engine.pages.push(AtlasPage::new(512)?);
            page = engine.pages.len() - 1;
            engine.pages[page].pack(gw, gh, &engine.scratch)?
        }
    };
    engine.glyphs.insert(key, (page, src));
    Ok((page, src))
}

fn paint_atlas(engine: &mut TextEngine, density: i64, align: Alignment, size: i64, text: &str, mode: RenderMode, run: i64) -> Result<TextDraw, String> {
    let RenderMode::Blended(c) = mode;
    let (face_idx, _, tracking) = scene_command::marks::text_style_parts(run);
    if tracking != 0.0 {
        return paint_fallback(engine, density, None, align, size, text, mode, run);
    }
    let face = if run == 0 { 0 } else {
        *engine.faces.get(face_idx).ok_or_else(|| format!("Text face {face_idx} is not loaded"))?
    };
    let style = label_style(Alignment::Left, size, false, face, 0.0);
    engine.rects.clear();
    let mut x = 0.0f64;
    let mut height = 1i64;
    let mut page_i = 0usize;
    let dens = density.max(1) as f64;
    let space_adv = *engine.advance.entry((face, size, density, '0')).or_insert_with(|| {
        engine.ui.text_service().width("0", style, None).unwrap_or(size as f32 * 0.5)
    });
    for ch in text.chars() {
        if ch == ' ' || ch == '\u{00a0}' {
            x += f64::from(space_adv);
            continue;
        }
        let (page, src) = glyph_slot(engine, face, size, density, c, ch, style)?;
        page_i = page;
        let (sw, sh) = (src.2, src.3);
        height = height.max(sh as i64);
        let adv = *engine.advance.entry((face, size, density, ch)).or_insert_with(|| {
            let mut buf = [0u8; 4];
            engine.ui.text_service().width(ch.encode_utf8(&mut buf), style, None).unwrap_or(sw as f32)
        });
        engine.rects.push(GlyphQuad {
            source: src,
            dest: (x, 0.0, sw / dens, sh / dens),
        });
        x += f64::from(adv);
    }
    let _ = align;
    // Direct atlas quads: Z6b batches consecutive same-texture Images into one Scene2Textured
    // draw (marks.rays affine[20k..]), so multi-glyph text stays under the A_idle_basic ceiling
    // without a per-frame CPU blit/upload. Wrap/tracking still uses paint_fallback.
    let lw = x.max(1.0).ceil() as i64;
    let lh = (height as f64 / dens).ceil().max(1.0) as i64;
    if engine.pages.is_empty() {
        return Ok(TextDraw::Image(image::create(1, 1, None)?));
    }
    let page = &engine.pages[page_i];
    let quads = if let Some(slot) = engine.quad_pool.iter_mut().find(|slot| {
        Rc::strong_count(slot) == 1 && Rc::weak_count(slot) == 0 && slot.len() == engine.rects.len()
    }) {
        Rc::get_mut(slot).unwrap().copy_from_slice(&engine.rects);
        slot.clone()
    } else {
        let quads = Rc::from(engine.rects.as_slice());
        // ponytail: keep eight recent quad allocations; increase the pool if many live runs are
        // shaped together. A published draw keeps its old quads even when a pool entry is evicted.
        if engine.quad_pool.len() == 8 { engine.quad_pool.remove(0); }
        engine.quad_pool.push(Rc::clone(&quads));
        quads
    };
    Ok(TextDraw::Atlas {
        image: page.image.clone(),
        width: lw,
        height: lh,
        quads,
    })
}

fn paint_text(density: i64, wrap: Option<i64>, align: Alignment, size: i64, text: &str, mode: RenderMode, run: i64) -> Result<TextDraw, String> {
    if text.is_empty() {
        return Ok(TextDraw::Image(image::create(1, 1, None)?));
    }
    TEXT.with(|t| {
        let mut t = t.borrow_mut();
        let engine = t.get_or_insert_with(|| text_engine().ok_or_else(|| "PXUI text initialization failed".to_owned()))
            .as_mut().map_err(|error| error.clone())?;
        if wrap.is_some() {
            paint_fallback(engine, density, wrap, align, size, text, mode, run)
        } else {
            paint_atlas(engine, density, align, size, text, mode, run)
        }
    })
}

pub mod raster {
    use super::*;

    struct AutomaticEntry {
        draw: TextDraw,
        references: Cell<i64>,
        cached: Cell<bool>,
    }

    fn draw_bytes(draw: &TextDraw) -> i64 {
        match draw {
            TextDraw::Image(img) => match (image::get_width(img), image::get_height(img)) {
                (Ok(w), Ok(h)) => w * h * 4,
                _ => 0,
            },
            TextDraw::Atlas { quads, .. } => quads.len() as i64 * 32,
        }
    }

    struct ContextData {
        automatic_cache: RefCell<Table>,
        automatic_references: Cell<i64>,
    }

    #[derive(Clone)]
    pub struct CacheContext(Rc<ContextData>);

    enum AutomaticDraw {
        Atlas(TextDraw),
        Shared(Rc<AutomaticEntry>),
    }

    pub struct Automatic {
        draw: AutomaticDraw,
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
                            if let TextDraw::Image(img) = &entry.draw {
                                let _ = image::destroy(img);
                            }
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
        style: i64,
    ) -> Result<Automatic, String> {
        let align = align.unwrap_or(Alignment::Left);
        let density = density.unwrap_or(1);
        // Atlas path: glyph pages are the cache. Skipping the string-keyed LRU avoids a format!
        // key and a table insert on every changing "frame N" string.
        if wrap.is_none() {
            let draw = paint_text(density, wrap, align, size, text, mode, style)?;
            if matches!(draw, TextDraw::Atlas { .. }) {
                return Ok(Automatic {
                    draw: AutomaticDraw::Atlas(draw),
                    context: None,
                    released: Cell::new(false),
                });
            }
        }
        let k = key(wrap, align, density, size, text, mode, style);
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        let Some(context) = context else {
            return paint_text(density, wrap, align, size, text, mode, style).map(|draw| Automatic {
                draw: AutomaticDraw::Shared(Rc::new(AutomaticEntry { draw, references: Cell::new(1), cached: Cell::new(false) })),
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
                return Ok(Automatic { draw: AutomaticDraw::Shared(entry), context: Some(context), released: Cell::new(false) });
            }
        let draw = paint_text(density, wrap, align, size, text, mode, style)?;
        let can_cache = side_lru::length(&mut context.0.automatic_cache.borrow_mut()) < CAPACITY
            || side_lru::find_first(&mut context.0.automatic_cache.borrow_mut(), &|_, item| {
                item.downcast_ref::<AutomaticEntry>().is_some_and(|e| e.references.get() == 0)
            })
            .is_some();
        let entry = Rc::new(AutomaticEntry { draw: draw.clone(), references: Cell::new(1), cached: Cell::new(can_cache) });
        if can_cache {
            side_lru::add_pinned(&mut context.0.automatic_cache.borrow_mut(), draw_bytes(&draw), k, entry.clone(), true);
        }
        context.0.automatic_references.set(context.0.automatic_references.get() + 1);
        Ok(Automatic { draw: AutomaticDraw::Shared(entry), context: Some(context), released: Cell::new(false) })
    }

    pub(crate) fn automatic_draw(handle: &Automatic) -> TextDraw {
        match &handle.draw {
            AutomaticDraw::Atlas(draw) => draw.clone(),
            AutomaticDraw::Shared(entry) => entry.draw.clone(),
        }
    }

    pub(crate) fn release_automatic(handle: &Automatic) {
        if handle.released.get() { return; }
        handle.released.set(true);
        let AutomaticDraw::Shared(entry) = &handle.draw else { return };
        entry.references.set(entry.references.get() - 1);
        if let Some(context) = &handle.context {
            context.0.automatic_references.set(context.0.automatic_references.get() - 1);
        }
        if entry.references.get() == 0 {
            if entry.cached.get() {
                if let Some(context) = &handle.context {
                    unpin_entry(context, entry);
                }
            } else if let TextDraw::Image(img) = &entry.draw {
                let _ = image::destroy(img);
            }
        }
    }

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

    pub fn automatic_counts() -> (i64, i64) {
        let context = CURRENT_CACHE.with(|c| c.borrow().clone());
        match context {
            None => (0, 0),
            Some(context) => (side_lru::length(&mut context.0.automatic_cache.borrow_mut()), context.0.automatic_references.get()),
        }
    }
}
