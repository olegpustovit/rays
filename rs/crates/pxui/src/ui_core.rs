//! The engine of the immediate-mode PXUI: keys, the retained cache, boxes, layout, input routing,
//! the carry, the glyph atlas, painting and `frame`. Every call creates one box (a node with an
//! integer key, flags and a size rule per axis); per-key state lives in the retained cache owned by
//! `Ui` and is dropped once a key is not built for a frame. Input is routed against the previous
//! frame's rectangles; layout runs after the build; boxes paint into one packed instance list.
//!
//! Translation notes: callbacks that returned `'a` in OCaml return nothing here (capture the result
//! in a local), `within`/`scope`/`frame` also come as `enter`/`leave` pairs; painters get a `Paint`
//! that owns the glyph atlas and the instance builder for the duration of the paint.
use crate::theme::{self, Theme};
use input::{Event, Key, MouseButton, Platform, ScrollPhase};
use ocaml_int::{lsr, wrap_int63};
use rays_math::hash::{fnv_int_mix, fnv_int_range};
use rays::{font, frame as rframe, image, scene, sketch};
use rays_math::color::{self, T as Color};
use scene_command::ui_batch::{self, Clip};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::OnceLock;

pub type Rect = (f64, f64, f64, f64);
pub type Flags = i64;
/// "no box": the OCaml `-1` of parents, children and siblings.
pub const NONE: usize = usize::MAX;
/// OCaml's `min_int` (63-bit): the "unset" retained state.
pub const MIN_INT: i64 = -4611686018427387904;

// ---------------------------------------------------------------- keys

const ROOT_KEY: i64 = 0x2c1b3c6d;

/// 63-bit FNV-1a; keys 0 and 1 are the open-addressing empty/tombstone marks.
fn hash_range(seed: i64, text: &str, first: usize, last: usize) -> i64 {
    let hash = fnv_int_range(seed ^ 0x2bf29ce484222325, text, first as i64, last as i64);
    let hash = hash ^ lsr(hash, 29);
    if hash == 0 || hash == 1 { hash + 2 } else { hash }
}

fn find_marker(text: &str, marker: &str) -> Option<usize> {
    if text.contains('#') { text.find(marker) } else { None }
}

pub fn key_of(seed: i64, text: &str) -> i64 {
    // the id after ### replaces the label's hash, under the same parent: two panels that build the
    // same widget keep separate state
    match find_marker(text, "###") {
        Some(index) => hash_range(seed, text, index + 3, text.len()),
        None => hash_range(seed, text, 0, text.len()),
    }
}

/// The label without its `##` suffix.
pub fn display(text: &str) -> String {
    match find_marker(text, "##") {
        Some(index) => text[..index].to_owned(),
        None => text.to_owned(),
    }
}

// ------------------------------------------------------ retained cache

/// Open addressing over int keys; 0 and 1 are the empty and tombstone marks.
struct SlotTable {
    keys: Vec<i64>,
    slots: Vec<usize>,
    filled: usize,
    live: usize,
}

impl SlotTable {
    fn create() -> SlotTable {
        SlotTable { keys: vec![0; 256], slots: vec![0; 256], filled: 0, live: 0 }
    }

    fn index(&self, key: i64) -> usize {
        let mask = self.keys.len() - 1;
        let mut position = (key as usize) & mask;
        loop {
            let found = self.keys[position];
            if found == key || found == 0 {
                return position;
            }
            position = (position + 1) & mask;
        }
    }

    fn find(&self, key: i64) -> Option<usize> {
        let position = self.index(key);
        if self.keys[position] == key { Some(self.slots[position]) } else { None }
    }

    fn add(&mut self, key: i64, slot: usize) {
        if 10 * (self.filled + 1) > 7 * self.keys.len() {
            let keys = std::mem::take(&mut self.keys);
            let slots = std::mem::take(&mut self.slots);
            let capacity = if 10 * self.live > 3 * keys.len() { 2 * keys.len() } else { keys.len() };
            self.keys = vec![0; capacity];
            self.slots = vec![0; capacity];
            self.filled = 0;
            self.live = 0;
            for (position, k) in keys.iter().enumerate() {
                if *k != 0 && *k != 1 {
                    self.add(*k, slots[position]);
                }
            }
        }
        let position = self.index(key);
        if self.keys[position] != key {
            self.filled += 1;
            self.live += 1;
        }
        self.keys[position] = key;
        self.slots[position] = slot;
    }

    fn remove(&mut self, key: i64) {
        let position = self.index(key);
        if self.keys[position] == key {
            self.keys[position] = 1;
            self.live -= 1;
        }
    }
}

// ------------------------------------------------------------- boxes

#[derive(Clone)]
pub enum Size {
    /// fixed
    Px(f64),
    /// fraction of the parent's inner size
    Pct(f64),
    /// function of the parent's inner size
    Rel(Rc<dyn Fn(f64) -> f64>),
    /// share of the remaining space, or the full cross size
    Grow,
    /// children's extent plus padding
    Fit,
}

impl Size {
    pub fn is_grow(&self) -> bool {
        matches!(self, Size::Grow)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Axis {
    Row,
    Column,
}

pub const NO_FLAGS: Flags = 0;
pub const CLICKABLE: Flags = 1;
pub const FOCUSABLE: Flags = 2;
pub const SCROLL: Flags = 4;
pub const CLIP: Flags = 8;
pub const BLOCKING: Flags = 16;
/// Kit controls enter keyboard mode through Tab; pointer editing retains the existing host
/// shortcut behavior. Text-entry rows also focus on a press.
pub const TAB_ONLY: Flags = 32;
/// Keyboard traversal focus for a control whose pointer interaction leaves keyboard input with its
/// host. Escape relinquishes this keyboard focus.
pub const TAB_STOP: Flags = FOCUSABLE | TAB_ONLY;
/// a multiline editor keeps Tab for itself (two spaces) instead of moving the focus
pub const KEEP_TAB: Flags = 64;
/// a press on such a box leaves the keyboard focus where it is (a completion row under an editor)
pub const KEEP_FOCUS: Flags = 128;
/// a scrolling box whose thumb is drawn over its content (an inspector body): no gutter
pub const OVER_THUMB: Flags = 4096;
/// A press whose topmost hit box is not clickable is captured by the nearest ancestor with this
/// flag.
pub const CAPTURE_UNCLAIMED: Flags = 8192;
/// the control marks its own keyboard focus: the engine strokes no ring round it
pub const FOCUS_MARK: Flags = 256;
pub const TAB_STOP_MARKED: Flags = TAB_STOP | FOCUS_MARK;
const HIT_FLAGS: Flags = CLICKABLE | FOCUSABLE | SCROLL | BLOCKING | CAPTURE_UNCLAIMED;

/// What a carry holds: a `kind` and a `value`, both strings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Payload {
    pub kind: String,
    pub value: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Origin {
    From_pointer(i64),
    From_key,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Drop {
    /// the pointer is over the box while a payload is held
    Hover(Payload),
    /// the payload was released over the box, this frame only
    Dropped(Payload),
}

/// Points a pressed pointer travels before a press is a drag: a carry starts, a click ends.
pub const drag_threshold: f64 = 4.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cursor {
    Horizontal_resize,
    Vertical_resize,
    Text,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UndoKind {
    Undo,
    Redo,
}

/// A painter runs with the box's final laid-out rectangle.
pub type Painter = Rc<dyn Fn(&mut Paint, Rect)>;
pub type HitFn = Rc<dyn Fn(Rect) -> Rect>;

struct BoxRec {
    key: i64,
    slot: usize,
    parent: usize,
    first: usize,
    last: usize,
    next: usize,
    flags: Flags,
    w: Size,
    h: Size,
    max_h: f64,
    row: bool,
    padding: f64,
    at_x: f64,
    at_y: f64,
    scroll_step: f64,
    hit: Option<HitFn>,
    painters: Vec<Painter>,
    overlays: Vec<Painter>,
}

impl BoxRec {
    fn blank() -> BoxRec {
        BoxRec {
            key: 0,
            slot: 0,
            parent: NONE,
            first: NONE,
            last: NONE,
            next: NONE,
            flags: 0,
            w: Size::Grow,
            h: Size::Grow,
            max_h: f64::INFINITY,
            row: false,
            padding: 0.0,
            at_x: f64::NAN,
            at_y: f64::NAN,
            scroll_step: 0.0,
            hit: None,
            painters: Vec::new(),
            overlays: Vec::new(),
        }
    }
}

/// A box handle: its index in this frame's build (`NONE` for `box_of_key`), key and retained slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UiBox {
    pub index: usize,
    pub box_key: i64,
    pub box_slot: usize,
}

pub fn key(b: &UiBox) -> i64 {
    b.box_key
}

struct Slot {
    key: i64,
    touched: i64,
    rx: f64,
    ry: f64,
    rw: f64,
    rh: f64,
    scroll_raw: f64,
    scroll_visual: f64,
    scroll_event_time: f64,
    scroll_frame_time: f64,
    /// a trackpad's coast or its bounce off an edge: 0 neither, 1 coast, 2 bounce
    scroll_mode: i64,
    scroll_velocity: f64,
    scroll_start: f64,
    scroll_from: f64,
    state_value: i64,
    text_value: Option<String>,
    press_time: f64,
    press_x: f64,
    press_y: f64,
    /// consecutive left presses within 0.35 s and 5 points
    press_count: i64,
}

impl Slot {
    fn fresh() -> Slot {
        Slot {
            key: 0,
            touched: -1,
            rx: 0.0,
            ry: 0.0,
            rw: 0.0,
            rh: 0.0,
            scroll_raw: 0.0,
            scroll_visual: 0.0,
            scroll_event_time: f64::NEG_INFINITY,
            scroll_frame_time: 0.0,
            scroll_mode: 0,
            scroll_velocity: 0.0,
            scroll_start: 0.0,
            scroll_from: 0.0,
            state_value: MIN_INT,
            text_value: None,
            press_time: f64::NEG_INFINITY,
            press_x: 0.0,
            press_y: 0.0,
            press_count: 0,
        }
    }
}

#[derive(Clone, Copy)]
struct HitRec {
    key: i64,
    parent: i64,
    flags: Flags,
    x: f64,
    y: f64,
    w: f64,
    h: f64,
}

/// What a press, a drag, a scroll or a key did to a box this frame.
#[derive(Clone)]
pub struct Accumulator {
    pub pressed: bool,
    pub subtree_press: Option<i64>,
    pub released: bool,
    pub clicked: bool,
    pub double_clicked: bool,
    pub clicks: i64,
    pub moved: bool,
    pub drag_x: f64,
    pub drag_y: f64,
    pub press_point: (f64, f64),
    pub release_point: (f64, f64),
    pub button: Option<MouseButton>,
    pub scroll_x: f64,
    pub scroll_y_steps: f64,
    /// what moves a scroll box: an unphased wheel's steps, the fingers' points on a trackpad, and
    /// whether they touched or lifted this frame
    pub scroll_wheel: f64,
    pub scroll_drag: f64,
    pub scroll_touch: bool,
    pub scroll_lift: bool,
    /// product of this frame's pinch factors
    pub pinch: f64,
    pub keys: Vec<(Event, Vec<Key>)>,
    pub press_keys: Vec<Key>,
}

// ------------------------------------------------------ glyph atlas

pub const ATLAS_WIDTH: i64 = 1024;
const ATLAS_MAX_HEIGHT: i64 = 4096;

#[derive(Clone, Copy, Debug)]
pub struct Glyph {
    pub gx: i64,
    pub gy: i64,
    pub gw: i64,
    pub gh: i64,
    pub advance: i64,
}

pub struct Atlas {
    pub width: i64,
    pub height: i64,
    pub pixels: Vec<u8>,
    glyphs: HashMap<i64, Glyph>,
    shelf_x: i64,
    shelf_y: i64,
    shelf_h: i64,
    dirty: bool,
    pub image: Option<image::t>,
    fonts: Vec<(font::t, i64, i64)>,
    next_font: i64,
}

fn atlas_owner() -> &'static mem::Owner {
    static OWNER: OnceLock<mem::Owner> = OnceLock::new();
    OWNER.get_or_init(|| mem::plan("pxui.atlas"))
}

fn text_undo_owner() -> &'static mem::Owner {
    static OWNER: OnceLock<mem::Owner> = OnceLock::new();
    OWNER.get_or_init(|| mem::document("pxui.text_undo"))
}

pub(crate) fn text_undo_ledger() -> &'static mem::Owner {
    text_undo_owner()
}

impl Atlas {
    fn empty() -> Atlas {
        Atlas { width: ATLAS_WIDTH, height: 0, pixels: Vec::new(), glyphs: HashMap::new(), shelf_x: 0, shelf_y: 0, shelf_h: 0, dirty: false, image: None, fonts: Vec::new(), next_font: 1 }
    }

    fn create() -> Atlas {
        Atlas {
            width: ATLAS_WIDTH,
            height: 256,
            pixels: vec![0; (ATLAS_WIDTH * 256) as usize],
            glyphs: HashMap::new(),
            shelf_x: 0,
            shelf_y: 0,
            shelf_h: 0,
            dirty: false,
            image: None,
            fonts: Vec::new(),
            next_font: 1,
        }
    }

    fn font_id(&mut self, font: &font::t) -> i64 {
        let generation = font::Raster::generation(font);
        let found = self.fonts.iter().position(|(candidate, _, _)| font::same(candidate, font));
        match found {
            Some(at) if self.fonts[at].2 == generation => self.fonts[at].1,
            Some(at) => {
                // A mutated font (style, hinting) rasterizes differently: new glyph ids.
                let id = self.fonts[at].1;
                self.glyphs.retain(|key, _| lsr(*key, 26) != id);
                self.fonts.remove(at);
                self.fonts.insert(0, (font.clone(), id, generation));
                id
            }
            None => {
                let id = self.next_font;
                self.next_font += 1;
                self.fonts.insert(0, (font.clone(), id, generation));
                id
            }
        }
    }

    fn reset(&mut self) {
        self.glyphs.clear();
        for p in self.pixels.iter_mut() {
            *p = 0;
        }
        self.shelf_x = 0;
        self.shelf_y = 0;
        self.shelf_h = 0;
        self.dirty = true;
    }

    // ponytail: a single shelf-packed page that doubles in height up to 4096 rows; when that
    // fills, the atlas is cleared and glyphs re-rasterize.
    fn reserve(&mut self, width: i64, height: i64) -> Option<(i64, i64)> {
        let padded_w = width + 1;
        let padded_h = height + 1;
        if padded_w > self.width {
            return None;
        }
        if self.shelf_x + padded_w > self.width {
            self.shelf_y += self.shelf_h;
            self.shelf_x = 0;
            self.shelf_h = 0;
        }
        if self.shelf_y + padded_h > self.height {
            let mut h = self.height;
            while self.shelf_y + padded_h > h && h < ATLAS_MAX_HEIGHT {
                h *= 2;
            }
            if self.shelf_y + padded_h > h {
                self.reset();
            } else {
                self.pixels.resize((self.width * h) as usize, 0);
                self.height = h;
            }
        }
        let x = self.shelf_x;
        let y = self.shelf_y;
        self.shelf_x = x + padded_w;
        self.shelf_h = self.shelf_h.max(padded_h);
        Some((x, y))
    }

    fn glyph(&mut self, font: &font::t, id: i64, density: i64, code: i64) -> Option<Glyph> {
        let key = (((id * 32) + density) << 21) | code;
        if let Some(found) = self.glyphs.get(&key) {
            return Some(*found);
        }
        let raster = font::Raster::glyph(Some(density), font, code).ok()?;
        let glyph = if raster.glyph_width == 0 || raster.glyph_height == 0 {
            Some(Glyph { gx: 0, gy: 0, gw: 0, gh: 0, advance: raster.glyph_advance })
        } else {
            match self.reserve(raster.glyph_width, raster.glyph_height) {
                None => None,
                Some((gx, gy)) => {
                    for row in 0..raster.glyph_height {
                        for column in 0..raster.glyph_width {
                            self.pixels[((gy + row) * self.width + (gx + column)) as usize] =
                                raster.glyph_alpha[(row * raster.glyph_width + column) as usize];
                        }
                    }
                    self.dirty = true;
                    Some(Glyph {
                        gx,
                        gy,
                        gw: raster.glyph_width,
                        gh: raster.glyph_height,
                        advance: raster.glyph_advance,
                    })
                }
            }
        };
        if let Some(g) = glyph {
            self.glyphs.insert(key, g);
        }
        glyph
    }

    /// The mirror is alpha alone: a glyph's color is constant, so the upload writes it.
    fn publish(&mut self) {
        if self.dirty {
            let mut rgba = Vec::with_capacity(4 * self.pixels.len());
            for alpha in &self.pixels {
                rgba.extend_from_slice(&[0xff, 0xff, 0xff, *alpha]);
            }
            if let Ok(image) = image::upload_rgba(self.image.as_ref(), self.width, self.height, &rgba) {
                self.image = Some(image);
                self.dirty = false;
            }
        }
    }
}

// ------------------------------------------------------------- fonts

/// The kit faces by size, the glyph atlas and the text measure: the part of the UI that painting
/// shares. `Paint` owns it while a frame paints.
pub struct Fonts {
    faces: HashMap<i64, Option<font::t>>,
    font_path: Option<String>,
    kit_font: Option<Option<String>>,
    ascent_ratio: Option<f64>,
    pub atlas: Atlas,
    pub density: i64,
    pub font_size: i64,
}

impl Default for Fonts {
    fn default() -> Fonts {
        Fonts { faces: HashMap::new(), font_path: None, kit_font: None, ascent_ratio: None, atlas: Atlas::empty(), density: 1, font_size: 13 }
    }
}

fn iter_code_points(text: &str, visit: &mut dyn FnMut(i64)) {
    for c in text.chars() {
        visit(c as i64);
    }
}

fn upward(directory: &std::path::Path, relative: &std::path::Path) -> Option<String> {
    let mut dir = Some(directory.to_path_buf());
    while let Some(d) = dir {
        let candidate = d.join(relative);
        if candidate.exists() {
            return Some(candidate.to_string_lossy().into_owned());
        }
        dir = d.parent().map(|p| p.to_path_buf());
    }
    None
}

impl Fonts {
    fn create(font_path: Option<String>, font_size: i64) -> Fonts {
        Fonts {
            faces: HashMap::new(),
            font_path,
            kit_font: None,
            ascent_ratio: None,
            atlas: Atlas::create(),
            density: 1,
            font_size,
        }
    }

    /// The kit face: the caller's `font_path`, else Pragmasevka found from the working directory or
    /// the executable upward.
    fn kit_font(&mut self) -> Option<String> {
        if self.kit_font.is_none() {
            let found = match &self.font_path {
                Some(path) => Some(path.clone()),
                None => {
                    let relative: std::path::PathBuf =
                        ["assets", "fonts", "Pragmasevka-Regular.ttf"].iter().collect();
                    let cwd = std::env::current_dir().ok().and_then(|d| upward(&d, &relative));
                    cwd.or_else(|| {
                        std::env::current_exe()
                            .ok()
                            .and_then(|e| e.parent().map(|p| p.to_path_buf()))
                            .and_then(|d| upward(&d, &relative))
                    })
                }
            };
            self.kit_font = Some(found);
        }
        self.kit_font.clone().unwrap()
    }

    pub fn face(&mut self, size: Option<i64>) -> Option<font::t> {
        let size = size.unwrap_or(self.font_size);
        if let Some(face) = self.faces.get(&size) {
            return face.clone();
        }
        let loaded = match self.kit_font() {
            Some(path) => font::load(&path, size).map(|f| {
                // the sheets' renderer does not hint: unhinted glyphs have its weight and x-height
                let _ = font::set_hinting(&f, font::hinting::None_hinting);
                f
            }),
            None => Err("no kit font".to_owned()),
        };
        let face = match loaded {
            Ok(f) => Some(f),
            Err(_) => font::system(Some(size)).ok(),
        };
        self.faces.insert(size, face.clone());
        face
    }

    /// The kit face's ascent as a fraction of its size, measured once on a large face. SDL_ttf
    /// rounds a font's ascent up to a whole pixel; the sheets' renderer rounds it to the nearest.
    fn ascent_ratio(&mut self) -> f64 {
        if self.ascent_ratio.is_none() {
            let ratio = match self.kit_font() {
                Some(path) => match font::load(&path, 4096) {
                    Ok(f) => {
                        let ratio = font::get_ascent(&f).unwrap_or(0) as f64 / 4096.0;
                        font::destroy(&f);
                        ratio
                    }
                    Err(_) => 0.0,
                },
                None => 0.0,
            };
            self.ascent_ratio = Some(ratio);
        }
        self.ascent_ratio.unwrap()
    }

    /// the move, in points, that puts a glyph raster's baseline on the nearest pixel row of its
    /// ascent instead of the one above: 0 or one device pixel up
    fn ascent_shift(&mut self, size: i64, density: i64) -> f64 {
        let ratio = self.ascent_ratio();
        if ratio == 0.0 {
            0.0
        } else {
            let exact = ratio * size as f64 * density as f64;
            (ocaml_round(exact) - (exact - 0.02).ceil()) / density as f64
        }
    }

    pub fn atlas_font_id(&mut self, font: &font::t) -> i64 {
        self.atlas.font_id(font)
    }

    pub fn glyph(&mut self, font: &font::t, id: i64, code: i64) -> Option<Glyph> {
        let density = self.density;
        self.atlas.glyph(font, id, density, code)
    }

    pub fn text_width_px(&mut self, size: Option<i64>, text: &str) -> i64 {
        match self.face(size) {
            None => 0,
            Some(font) => {
                let id = if text.is_empty() { 0 } else { self.atlas.font_id(&font) };
                let mut total = 0;
                for c in text.chars() {
                    if let Some(g) = self.glyph(&font, id, c as i64) {
                        total += g.advance;
                    }
                }
                total
            }
        }
    }

    /// the width of kit text in points: the one measure of the build and of painting
    pub fn text_width(&mut self, size: Option<i64>, text: &str) -> f64 {
        self.text_width_px(size, text) as f64 / self.density as f64
    }

    pub fn destroy(&mut self) {
        for (_, face) in self.faces.drain() {
            if let Some(f) = face {
                font::destroy(&f);
            }
        }
        self.atlas.fonts.clear();
    }
}

/// OCaml's `Float.round`: half away from zero.
pub fn ocaml_round(x: f64) -> f64 {
    x.round()
}

// ----------------------------------------------------------------- Ui

pub struct Ui {
    theme: Theme,
    font_size: i64,
    platform: Platform,
    fonts: Fonts,
    table: SlotTable,
    slots: Vec<Slot>,
    free: Vec<usize>,
    scroll_y: Vec<f64>,
    // per-frame boxes
    count: usize,
    boxes: Vec<BoxRec>,
    l_x: Vec<f64>,
    l_y: Vec<f64>,
    l_w: Vec<f64>,
    l_h: Vec<f64>,
    l_content: Vec<f64>,
    l_gutter: Vec<f64>,
    // build state
    parents: Vec<usize>,
    overlays: Vec<usize>,
    foreground: Vec<(i64, usize)>,
    seeds: Vec<i64>,
    building: bool,
    frame_number: i64,
    density: i64,
    kit_row_height: i64,
    closed_section: usize,
    kit_window: bool,
    // input
    pointer: (f64, f64),
    hot: i64,
    hover_rest: Option<(String, (f64, f64), f64, i64)>,
    active: Option<i64>,
    active_button: MouseButton,
    active_press: (f64, f64),
    active_keys: Vec<Key>,
    focus: i64,
    keyboard_focus: bool,
    composition: String,
    previous_keys: Vec<Key>,
    pub(crate) edit_focus: i64,
    pub edit_value: String,
    pub(crate) edit_caret: usize,
    pub(crate) edit_anchor: usize,
    pub(crate) edit_scroll_x: f64,
    window_ground: Option<Color>,
    gesture_key: i64,
    gesture_time: f64,
    gesture_velocity: f64,
    pub edit_undo: Vec<(String, usize, usize)>,
    pub(crate) edit_undo_charged: i64,
    pub(crate) text_rows_seen: Vec<(Option<usize>, String, Rc<Vec<(usize, usize, usize)>>)>,
    pub(crate) text_colors_seen: Vec<(Theme, String, Rc<Vec<(i64, i64, Color)>>)>,
    pub(crate) edit_redo: Vec<(String, usize, usize)>,
    pub(crate) edit_group: i64,
    pub(crate) edit_shelf: Vec<ShelfEntry>,
    pub(crate) edit_unit: Option<(usize, usize, i64)>,
    pub(crate) edit_column: f64,
    pub(crate) scrub_origin: Option<(i64, String)>,
    payload: Option<(Payload, Origin)>,
    dropped: Option<(Payload, i64)>,
    carry_latch: Option<i64>,
    passed_undo: Option<UndoKind>,
    requested_cursor: Option<Cursor>,
    frame_events: Vec<Event>,
    input_frame: Option<rframe::t>,
    routed_events: Vec<(i64, Event, (f64, f64))>,
    cancelled: Vec<MouseButton>,
    modal_key: Option<i64>,
    modal_extra: Vec<i64>,
    modal_in_frame: bool,
    view_w: f64,
    view_h: f64,
    modal_heights: HashMap<i64, (f64, i64)>,
    signals: HashMap<i64, Accumulator>,
    hits: Vec<HitRec>,
    hit_count: usize,
    chain_key: i64,
    chain: Vec<i64>,
    batch_builder: Option<ui_batch::Builder>,
    regions: Vec<scene::node>,
    scene_layers: Vec<(i64, Vec<scene::node>)>,
    borrowed_images: Vec<(i64, image::t)>,
    scene: Vec<scene::node>,
    destroyed: bool,
}

/// One text left behind: its key, value and its undo and redo stacks.
pub type ShelfEntry = (i64, String, Vec<(String, usize, usize)>, Vec<(String, usize, usize)>);

pub type t = Ui;

fn nonfinite(x: f64) -> bool {
    !x.is_finite()
}

pub fn contains(rect: Rect, point: (f64, f64)) -> bool {
    let (x, y, w, h) = rect;
    let (px, py) = point;
    px >= x && py >= y && px < x + w && py < y + h
}

pub fn intersect(a: Rect, b: Rect) -> Rect {
    let (ax, ay, aw, ah) = a;
    let (bx, by, bw, bh) = b;
    let x = ax.max(bx);
    let y = ay.max(by);
    let right = (ax + aw).min(bx + bw);
    let bottom = (ay + ah).min(by + bh);
    (x, y, (right - x).max(0.0), (bottom - y).max(0.0))
}

fn packed(c: Color) -> i32 {
    (((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32) as i32
}

/// Integer-derived child keys: no label string per frame.
pub fn int_key(seed: i64, n: i64) -> i64 {
    let hash = fnv_int_mix(seed, wrap_int63(n.wrapping_mul(0x1e3779b97f4a7c15)));
    let hash = hash ^ lsr(hash, 29);
    if hash == 0 || hash == 1 { hash + 2 } else { hash }
}

impl Ui {
    /// `platform` is the host's modifier conventions: the text keys and every Command check follow
    /// it. `font_path` names the kit face's file (default: Pragmasevka found from the working
    /// directory or the executable upward; the environment is never read); `font_size` is the
    /// logical size of kit text.
    pub fn create(platform: Platform, font_size: Option<i64>, font_path: Option<String>) -> Ui {
        let font_size = font_size.unwrap_or(theme::FONT_SIZE);
        if font_size <= 0 {
            panic!("Ui.create: font_size must be positive");
        }
        let fonts = Fonts::create(font_path, font_size);
        mem::charge(atlas_owner(), fonts.atlas.pixels.len() as i64);
        Ui {
            theme: theme::default(),
            font_size,
            platform,
            fonts,
            table: SlotTable::create(),
            slots: Vec::new(),
            free: Vec::new(),
            scroll_y: Vec::new(),
            count: 0,
            boxes: Vec::new(),
            l_x: Vec::new(),
            l_y: Vec::new(),
            l_w: Vec::new(),
            l_h: Vec::new(),
            l_content: Vec::new(),
            l_gutter: Vec::new(),
            parents: Vec::new(),
            overlays: Vec::new(),
            foreground: Vec::new(),
            seeds: Vec::new(),
            building: false,
            frame_number: 0,
            density: 1,
            kit_row_height: 24,
            closed_section: NONE,
            kit_window: false,
            pointer: (f64::NAN, f64::NAN),
            hot: 0,
            hover_rest: None,
            active: None,
            active_button: MouseButton::LeftButton,
            active_press: (0.0, 0.0),
            active_keys: Vec::new(),
            focus: 0,
            keyboard_focus: false,
            composition: String::new(),
            previous_keys: Vec::new(),
            edit_focus: 0,
            edit_value: String::new(),
            edit_caret: 0,
            edit_anchor: 0,
            edit_scroll_x: 0.0,
            window_ground: None,
            gesture_key: 0,
            gesture_time: f64::NAN,
            gesture_velocity: 0.0,
            edit_undo: Vec::new(),
            edit_undo_charged: 0,
            text_rows_seen: Vec::new(),
            text_colors_seen: Vec::new(),
            edit_redo: Vec::new(),
            edit_group: -1,
            edit_shelf: Vec::new(),
            edit_unit: None,
            edit_column: -1.0,
            scrub_origin: None,
            payload: None,
            dropped: None,
            carry_latch: None,
            passed_undo: None,
            requested_cursor: None,
            frame_events: Vec::new(),
            input_frame: None,
            routed_events: Vec::new(),
            cancelled: Vec::new(),
            modal_key: None,
            modal_extra: Vec::new(),
            modal_in_frame: false,
            view_w: 0.0,
            view_h: 0.0,
            modal_heights: HashMap::new(),
            signals: HashMap::new(),
            hits: Vec::new(),
            hit_count: 0,
            chain_key: 0,
            chain: Vec::new(),
            batch_builder: Some(ui_batch::Builder::with_capacity(1024)),
            regions: Vec::new(),
            scene_layers: Vec::new(),
            borrowed_images: Vec::new(),
            scene: Vec::new(),
            destroyed: false,
        }
    }

    pub fn platform(&self) -> Platform {
        self.platform
    }

    /// Release the glyph atlas and the kit faces this UI loaded. The handle must not be used
    /// afterwards.
    pub fn destroy(&mut self) {
        mem::release(text_undo_owner(), self.edit_undo_charged);
        self.edit_undo_charged = 0;
        if !self.destroyed {
            mem::release(atlas_owner(), self.fonts.atlas.pixels.len() as i64);
            self.destroyed = true;
            if let Some(image) = self.fonts.atlas.image.take() {
                let _ = image::destroy(&image);
            }
            self.fonts.destroy();
            self.borrowed_images.clear();
            self.scene.clear();
        }
    }

    pub fn theme(&self) -> Theme {
        self.theme
    }

    pub fn set_theme(&mut self, theme: Theme) {
        self.theme = theme;
    }

    pub fn font_size(&self) -> i64 {
        self.font_size
    }

    /// The kit text size from the next frame on, and the row height with it (`max 24 (size + 11)`).
    /// Call it between frames, never inside `frame`.
    pub fn set_font_size(&mut self, size: i64) {
        if size <= 0 {
            panic!("Ui.set_font_size: size must be positive");
        }
        self.font_size = size;
        self.fonts.font_size = size;
        self.kit_row_height = 24.max(size + 11);
    }

    /// The most recently completed frame, including text-input regions. `under` inserts a host
    /// scene immediately before a floating root's paint, addressed by its box key.
    pub fn scene(&self, under: Option<&dyn Fn(i64) -> Vec<scene::node>>) -> Vec<scene::node> {
        match under {
            None => self.scene.clone(),
            Some(under) => {
                let mut out = Vec::new();
                for (key, layer) in &self.scene_layers {
                    out.extend(under(*key));
                    out.extend(layer.iter().cloned());
                }
                out.extend(self.regions.iter().cloned());
                out
            }
        }
    }

    /// How many keys the retained cache holds (live slots).
    pub fn retained_slots(&self) -> usize {
        self.slots.iter().filter(|s| s.key != 0).count()
    }

    pub fn row_height(&self) -> i64 {
        self.kit_row_height
    }

    pub fn view_size(&self) -> (f64, f64) {
        (self.view_w, self.view_h)
    }

    pub fn text_line_height(&self) -> i64 {
        self.font_size + (self.font_size + 1) / 2
    }

    pub fn atlas_mirror_bytes(&self) -> i64 {
        self.fonts.atlas.pixels.len() as i64
    }

    /// The width of kit text in points, for layout during the build (`Paint::text_width` while
    /// painting).
    pub fn text_width(&mut self, size: Option<i64>, text: &str) -> f64 {
        self.fonts.text_width(size, text)
    }

    pub(crate) fn fonts_mut(&mut self) -> &mut Fonts {
        &mut self.fonts
    }

    // ----------------------------------------------------- retained slots

    fn slot_of(&mut self, key: i64) -> usize {
        if let Some(slot) = self.table.find(key) {
            return slot;
        }
        let slot = match self.free.pop() {
            Some(slot) => slot,
            None => {
                self.slots.push(Slot::fresh());
                self.scroll_y.push(0.0);
                self.slots.len() - 1
            }
        };
        let s = &mut self.slots[slot];
        *s = Slot::fresh();
        s.key = key;
        s.touched = self.frame_number;
        self.scroll_y[slot] = 0.0;
        self.table.add(key, slot);
        slot
    }

    fn prune(&mut self) {
        for slot in 0..self.slots.len() {
            let key = self.slots[slot].key;
            if key != 0 && self.slots[slot].touched < self.frame_number {
                self.table.remove(key);
                self.slots[slot].key = 0;
                self.slots[slot].text_value = None;
                self.free.push(slot);
                if self.focus == key {
                    self.focus = 0;
                    self.composition.clear();
                    self.edit_focus = 0;
                }
                if self.active == Some(key) {
                    self.cancelled.insert(0, self.active_button);
                    self.active = None;
                }
                // the box that holds a carry's pointer is gone: the carry goes with it
                if let Some((_, Origin::From_pointer(owner))) = &self.payload {
                    if *owner == key {
                        self.payload = None;
                    }
                }
                if self.hot == key {
                    self.hot = 0;
                }
            }
        }
    }

    // -------------------------------------------------------------- input

    pub(crate) fn command_modifiers(&self, keys: &[Key]) -> bool {
        input::command_held(self.platform, keys)
    }

    /// Command moves and deletes by lines on Apple only; elsewhere Ctrl is the word modifier.
    pub(crate) fn line_modifiers(&self, keys: &[Key]) -> bool {
        self.platform == Platform::Apple && self.command_modifiers(keys)
    }

    fn accumulator(&mut self, key: i64) -> &mut Accumulator {
        if !self.signals.contains_key(&key) {
            let press_keys = if self.active == Some(key) { self.active_keys.clone() } else { Vec::new() };
            self.signals.insert(
                key,
                Accumulator {
                    pressed: false,
                    subtree_press: None,
                    released: false,
                    clicked: false,
                    double_clicked: false,
                    clicks: 0,
                    moved: false,
                    drag_x: 0.0,
                    drag_y: 0.0,
                    press_point: (0.0, 0.0),
                    release_point: (0.0, 0.0),
                    button: None,
                    scroll_x: 0.0,
                    scroll_y_steps: 0.0,
                    pinch: 1.0,
                    keys: Vec::new(),
                    scroll_wheel: 0.0,
                    scroll_drag: 0.0,
                    scroll_touch: false,
                    scroll_lift: false,
                    press_keys,
                },
            );
        }
        self.signals.get_mut(&key).unwrap()
    }

    fn hit_index(&self, key: i64) -> Option<usize> {
        (0..self.hit_count).rev().find(|i| self.hits[*i].key == key)
    }

    /// Calls `f` for `key` and each ancestor in the hit list, innermost first.
    fn hit_ancestors(&self, key: i64, f: &mut dyn FnMut(i64)) {
        let mut key = key;
        while key != 0 {
            f(key);
            match self.hit_index(key) {
                Some(index) => key = self.hits[index].parent,
                None => break,
            }
        }
    }

    /// `key` is `ancestor` or under it in the hit list. The chain of a key is walked once (each
    /// step scans the list) and kept until the list is rebuilt.
    fn hit_within(&mut self, key: i64, ancestor: i64) -> bool {
        if key == ancestor {
            return true;
        }
        if key == 0 {
            return false;
        }
        if self.chain_key != key {
            let mut chain = Vec::new();
            self.hit_ancestors(key, &mut |k| chain.insert(0, k));
            self.chain_key = key;
            self.chain = chain;
        }
        self.chain.contains(&ancestor)
    }

    fn hit_contains(&self, index: usize, point: (f64, f64)) -> bool {
        let h = &self.hits[index];
        contains((h.x, h.y, h.w, h.h), point)
    }

    /// Topmost hit entry under `point`; 0 when none.
    fn topmost(&self, point: (f64, f64)) -> i64 {
        for index in (0..self.hit_count).rev() {
            if self.hit_contains(index, point) {
                return self.hits[index].key;
            }
        }
        0
    }

    /// The root key whose box or child is topmost under the point in PXUI's previous hit tree.
    pub fn root_at(&self, point: (f64, f64), roots: &[i64]) -> Option<i64> {
        let mut found = None;
        self.hit_ancestors(self.topmost(point), &mut |key| {
            if found.is_none() && roots.contains(&key) {
                found = Some(key);
            }
        });
        found
    }

    /// Last root key pressed in `frame`, using PXUI's previous hit tree.
    pub fn last_press_within(&self, frame: &rframe::t, roots: &[i64]) -> Option<i64> {
        let mut last = None;
        for event in &frame.input.events {
            if let Event::Pointer_pressed(_, point) = event {
                if let Some(root) = self.root_at(*point, roots) {
                    last = Some(root);
                }
            }
        }
        last
    }

    fn flags_of_key(&self, key: i64) -> Flags {
        match self.hit_index(key) {
            Some(index) => self.hits[index].flags,
            None => 0,
        }
    }

    /// The scroll box the wheel goes to at `point`: the topmost one, unless a blocking box lies
    /// over it that is not its own content (a field inside a scrolling panel lets the panel
    /// scroll; a menu over it does not).
    fn scroll_target(&mut self, point: (f64, f64)) -> i64 {
        let mut blocker = 0;
        let mut index = self.hit_count as i64 - 1;
        while index >= 0 {
            let i = index as usize;
            if self.hit_contains(i, point) {
                let flags = self.hits[i].flags;
                let key = self.hits[i].key;
                if flags & SCROLL != 0 {
                    return if blocker == 0 || self.hit_within(blocker, key) { key } else { 0 };
                } else if blocker == 0 && flags & BLOCKING != 0 {
                    blocker = key;
                }
            }
            index -= 1;
        }
        0
    }

    fn eligible(&mut self, index: usize) -> bool {
        if self.hits[index].flags & FOCUSABLE == 0 {
            return false;
        }
        match self.modal_key {
            None => true,
            Some(modal) => {
                let key = self.hits[index].key;
                if self.hit_within(key, modal) {
                    return true;
                }
                let extra = self.modal_extra.clone();
                extra.iter().any(|e| self.hit_within(key, *e))
            }
        }
    }

    fn traverse_focus(&mut self, shift: bool) {
        let n = self.hit_count as i64;
        if n == 0 {
            return;
        }
        let current = self.hit_index(self.focus);
        let direction: i64 = if shift { -1 } else { 1 };
        let start: i64 = match current {
            Some(c) => c as i64,
            None => if direction < 0 { 0 } else { n - 1 },
        };
        let mut distance = 1;
        while distance <= n {
            let index = ((start + direction * distance + n) % n) as usize;
            if self.eligible(index) {
                self.focus = self.hits[index].key;
                self.keyboard_focus = true;
                self.composition.clear();
                self.edit_focus = 0;
                return;
            }
            distance += 1;
        }
    }

    fn modal_target(&mut self, target: i64) -> i64 {
        match self.modal_key {
            Some(modal) => {
                let extra = self.modal_extra.clone();
                if !(self.hit_within(target, modal) || extra.iter().any(|e| self.hit_within(target, *e))) {
                    modal
                } else {
                    target
                }
            }
            None => target,
        }
    }

    fn pointer_target(&mut self, point: (f64, f64)) -> i64 {
        match self.active {
            Some(target) => target,
            None => {
                let top = self.topmost(point);
                self.modal_target(top)
            }
        }
    }

    fn lift(&mut self) {
        if self.gesture_key != 0 {
            let key = self.gesture_key;
            self.accumulator(key).scroll_lift = true;
        }
        self.gesture_key = 0;
    }

    fn route(&mut self, frame: &rframe::t) {
        self.signals.clear();
        self.requested_cursor = None;
        self.frame_events = frame.input.events.clone();
        self.input_frame = Some(frame.clone());
        self.routed_events.clear();
        self.cancelled.clear();
        self.dropped = None;
        // a carry lives with the press that holds it: capture taken away since (a popup, a host
        // dismissal) takes the payload too, so no later release puts it
        if let Some((_, Origin::From_pointer(owner))) = &self.payload {
            if self.active != Some(*owner) {
                self.payload = None;
            }
        }
        let mut released_carry = false;
        self.passed_undo = None;
        self.modal_in_frame = self.modal_key.is_some();
        let mut modifiers = input::keys_before(&self.previous_keys, &frame.input.keys, &frame.input.events);
        self.previous_keys = frame.input.keys.clone();
        self.view_w = frame.input.size.0 as f64;
        self.view_h = frame.input.size.1 as f64;
        // a trackpad's motion also arrives as wheel steps: in such a frame those only feed
        // `signal.scroll`, and the gesture moves the scroll box
        let phased = frame.input.events.iter().any(|e| match e {
            Event::Trackpad_scrolled { delta: (dx, dy), .. } => *dx != 0.0 || *dy != 0.0,
            _ => false,
        });
        for (event_index, event) in frame.input.events.iter().enumerate() {
            modifiers = input::keys_after(&modifiers, event);
            let command = self.command_modifiers(&modifiers);
            if let Event::Key_pressed(Key::Tab) = event {
                if !command && self.flags_of_key(self.focus) & KEEP_TAB == 0 {
                    self.traverse_focus(modifiers.contains(&Key::Shift));
                }
            }
            let (owner, delta): (i64, (f64, f64)) = match event {
                Event::Pointer_moved(point) => {
                    let owner = self.pointer_target(*point);
                    let delta = if self.pointer.0.is_finite() {
                        (point.0 - self.pointer.0, point.1 - self.pointer.1)
                    } else {
                        (0.0, 0.0)
                    };
                    (owner, delta)
                }
                Event::Pointer_pressed(_, point) | Event::Pointer_released(_, point) => {
                    (self.pointer_target(*point), (0.0, 0.0))
                }
                Event::Scrolled(_) | Event::Pinched(_) => {
                    let target = match self.active {
                        Some(target) => target,
                        None => {
                            let scroll = self.scroll_target(self.pointer);
                            let fallback = if scroll != 0 { scroll } else { self.topmost(self.pointer) };
                            self.modal_target(fallback)
                        }
                    };
                    (target, (0.0, 0.0))
                }
                Event::Key_pressed(Key::Tab) if !command => {
                    (if self.focus == 0 { ROOT_KEY } else { self.focus }, (0.0, 0.0))
                }
                Event::Key_pressed(_) | Event::Key_released(_) | Event::Text_input(_) | Event::Text_editing { .. } => {
                    (self.focus, (0.0, 0.0))
                }
                _ => (0, (0.0, 0.0)),
            };
            self.routed_events.push((owner, event.clone(), delta));
            match event {
                Event::Pointer_moved(point) => {
                    let (px, py) = self.pointer;
                    self.pointer = *point;
                    if let Some(active) = self.active {
                        let (x, y) = self.pointer;
                        let value = self.accumulator(active);
                        if px.is_finite() {
                            value.drag_x += x - px;
                            value.drag_y += y - py;
                        }
                        value.moved = true;
                    }
                }
                // a carry started from a key: a left press is the put, on what is under it, and
                // nothing else sees the press
                Event::Pointer_pressed(MouseButton::LeftButton, point)
                    if matches!(self.payload, Some((_, Origin::From_key))) =>
                {
                    self.pointer = *point;
                    if let Some((payload, _)) = &self.payload {
                        self.dropped = Some((payload.clone(), self.topmost(*point)));
                    }
                    self.payload = None;
                    if let Some(last) = self.routed_events.last_mut() {
                        last.0 = -1;
                    }
                }
                Event::Pointer_pressed(button, point) => {
                    self.pointer = *point;
                    let target = if owner == 0 || self.flags_of_key(owner) & CLICKABLE != 0 {
                        owner
                    } else {
                        let mut found = None;
                        self.hit_ancestors(owner, &mut |key| {
                            if found.is_none() && self.flags_of_key(key) & CAPTURE_UNCLAIMED != 0 {
                                found = Some(key);
                            }
                        });
                        found.unwrap_or(owner)
                    };
                    let mut chain = Vec::new();
                    self.hit_ancestors(target, &mut |key| chain.push(key));
                    for key in chain {
                        self.accumulator(key).subtree_press = Some(event_index as i64);
                    }
                    let flags = self.flags_of_key(target);
                    if *button == MouseButton::LeftButton {
                        let focus = if flags & KEEP_FOCUS != 0 {
                            self.focus
                        } else if flags & FOCUSABLE != 0 && flags & TAB_ONLY == 0 {
                            target
                        } else {
                            0
                        };
                        if focus != self.focus {
                            self.composition.clear();
                            self.edit_focus = 0;
                        }
                        self.focus = focus;
                        self.keyboard_focus = false;
                    }
                    if (target == 0 || flags & (CLICKABLE | CAPTURE_UNCLAIMED) != 0) && self.active.is_none() {
                        self.active = Some(target);
                        self.active_button = *button;
                        self.active_press = self.pointer;
                        self.active_keys = modifiers.clone();
                        let pointer = self.pointer;
                        let mods = modifiers.clone();
                        let value = self.accumulator(target);
                        value.pressed = true;
                        value.press_point = pointer;
                        value.press_keys = mods;
                        value.button = Some(*button);
                        if let Some(slot) = self.table.find(target).filter(|s| *s < self.slots.len()) {
                            if *button == MouseButton::LeftButton {
                                let (x, y) = pointer;
                                let s = &self.slots[slot];
                                let dx = x - s.press_x;
                                let dy = y - s.press_y;
                                // the second press within 0.35 s and 5 points is a double click,
                                // the third a triple; the fourth starts over
                                let count = if frame.input.t >= s.press_time
                                    && frame.input.t - s.press_time <= 0.35
                                    && dx * dx + dy * dy <= 25.0
                                    && s.press_count < 3
                                {
                                    s.press_count + 1
                                } else {
                                    1
                                };
                                let s = &mut self.slots[slot];
                                s.press_count = count;
                                s.press_time = frame.input.t;
                                s.press_x = x;
                                s.press_y = y;
                                let value = self.accumulator(target);
                                value.clicks = count;
                                value.double_clicked = count == 2;
                            }
                        }
                    }
                }
                Event::Pointer_released(button, point) => {
                    self.pointer = *point;
                    if let Some(active) = self.active {
                        if *button == self.active_button {
                            if let Some((_, Origin::From_pointer(o))) = &self.payload {
                                if *o == active && *button == MouseButton::LeftButton {
                                    released_carry = true;
                                }
                            }
                            let pointer = self.pointer;
                            let active_press = self.active_press;
                            let active_keys = self.active_keys.clone();
                            let index = self.hit_index(active);
                            let clicked = *button == MouseButton::LeftButton
                                && index.is_some_and(|i| self.hit_contains(i, pointer) && self.hit_contains(i, active_press));
                            let value = self.accumulator(active);
                            value.released = true;
                            value.release_point = pointer;
                            value.press_point = active_press;
                            value.press_keys = active_keys;
                            value.button = Some(*button);
                            if clicked {
                                value.clicked = true;
                            }
                            self.active = None;
                        }
                    }
                }
                Event::Scrolled((horizontal, vertical)) => {
                    let scroll = self.scroll_target(self.pointer);
                    let target = self.modal_target(scroll);
                    if target != 0 {
                        let value = self.accumulator(target);
                        value.scroll_x += horizontal;
                        value.scroll_y_steps += vertical;
                        if !phased {
                            value.scroll_wheel += vertical;
                        }
                    }
                }
                // The fingers on a trackpad: the gesture stays with the box it began over, and the
                // velocity it is released with is its last motion over the time since the one
                // before. The system's own momentum is not used: the box coasts by itself.
                Event::Trackpad_scrolled { phase: ScrollPhase::Momentum, .. } => {}
                Event::Trackpad_scrolled { phase: ScrollPhase::Lifted, time, .. } => {
                    // fingers that rested before they lifted leave the content where it is
                    if time - self.gesture_time > 0.1 {
                        self.gesture_velocity = 0.0;
                    }
                    self.lift();
                }
                Event::Trackpad_scrolled { delta: (_, vertical), phase, time } => {
                    if *phase == ScrollPhase::Touched || self.gesture_key == 0 {
                        let scroll = self.scroll_target(self.pointer);
                        self.gesture_key = self.modal_target(scroll);
                        self.gesture_time = f64::NAN;
                        self.gesture_velocity = 0.0;
                    }
                    self.gesture_velocity =
                        if *time > self.gesture_time { -vertical / (time - self.gesture_time) } else { 0.0 };
                    self.gesture_time = *time;
                    if self.gesture_key != 0 {
                        let key = self.gesture_key;
                        let value = self.accumulator(key);
                        if *phase == ScrollPhase::Touched {
                            value.scroll_touch = true;
                            value.scroll_lift = false;
                        }
                        value.scroll_drag += vertical;
                    }
                }
                // a trackpad pinch goes where the wheel goes
                Event::Pinched(scale) => {
                    let scroll = self.scroll_target(self.pointer);
                    let target = self.modal_target(scroll);
                    if target != 0 {
                        self.accumulator(target).pinch *= scale;
                    }
                }
                // Cancellation ends capture without a release: no click, no commit.
                Event::Pointer_cancelled(button) => {
                    if *button == self.active_button {
                        self.active = None;
                    }
                    if let Some((_, Origin::From_pointer(_))) = &self.payload {
                        if *button == MouseButton::LeftButton {
                            self.payload = None;
                            released_carry = false;
                        }
                    }
                }
                Event::Focus_lost => {
                    self.gesture_velocity = 0.0;
                    self.lift();
                    self.payload = None;
                    released_carry = false;
                    self.active = None;
                    self.focus = 0;
                    self.hot = 0;
                    self.composition.clear();
                    self.edit_focus = 0;
                    self.keyboard_focus = false;
                }
                Event::Key_pressed(_) | Event::Key_released(_) | Event::Text_input(_) | Event::Text_editing { .. } => {
                    if self.focus != 0 {
                        let focus = self.focus;
                        let mods = modifiers.clone();
                        let value = self.accumulator(focus);
                        value.keys.push((event.clone(), mods));
                        if let Event::Key_pressed(Key::Enter) | Event::Key_pressed(Key::Space) | Event::Key_pressed(Key::KeyChar(' ')) = event {
                            if !command {
                                value.clicked = true;
                            }
                        }
                        match event {
                            Event::Text_editing { text, .. } => self.composition = text.clone(),
                            Event::Text_input(_) => self.composition.clear(),
                            Event::Key_pressed(Key::Escape) if self.flags_of_key(self.focus) & TAB_ONLY != 0 => {
                                self.focus = 0;
                                self.keyboard_focus = false;
                                self.edit_focus = 0;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        let (mouse_x, mouse_y) = frame.input.pointer;
        if !self.pointer.0.is_finite() {
            self.pointer = (mouse_x, mouse_y);
        }
        self.hot = if frame.input.events.iter().any(|e| matches!(e, Event::Focus_lost)) {
            0
        } else {
            self.topmost(self.pointer)
        };
        if self.active.is_none() {
            self.carry_latch = None;
        }
        // released: what the carry was put on is what was hot at the release
        if released_carry {
            if let Some((payload, _)) = &self.payload {
                self.dropped = Some((payload.clone(), self.hot));
            }
            self.payload = None;
        }
    }

    pub fn wants_pointer(&self) -> bool {
        self.hot != 0 || self.active.is_some_and(|a| a != 0)
    }

    pub fn cursor(&self) -> Option<Cursor> {
        self.requested_cursor
    }

    /// Cursor requested by a hovered or captured PXUI control; the last request of a frame wins.
    pub fn request_cursor(&mut self, shape: Cursor) {
        self.requested_cursor = Some(shape);
    }

    pub fn text_input_focused(&self) -> bool {
        self.focus != 0
    }

    pub fn passed_undo(&self) -> Option<UndoKind> {
        self.passed_undo
    }

    pub(crate) fn set_passed_undo(&mut self, kind: UndoKind) {
        self.passed_undo = Some(kind);
    }

    /// The key was pressed in this frame's events (a dialog's Enter).
    pub fn key_pressed(&self, key: Key) -> bool {
        self.frame_events.iter().any(|e| matches!(e, Event::Key_pressed(k) if *k == key))
    }

    pub fn unfocus(&mut self) {
        self.focus = 0;
        self.composition.clear();
        self.edit_focus = 0;
        self.keyboard_focus = false;
    }

    /// When the host closes a popup after accepting its result, release focus and capture. Its
    /// current frame remains consumed; next frame is unblocked.
    pub fn dismiss_popup(&mut self) {
        self.modal_key = None;
        self.modal_extra.clear();
        self.unfocus();
        if self.active.is_some() {
            self.cancelled.insert(0, self.active_button);
            self.active = None;
        }
    }

    pub(crate) fn composition(&self) -> &str {
        &self.composition
    }

    pub(crate) fn pointer(&self) -> (f64, f64) {
        self.pointer
    }

    pub(crate) fn frame_events(&self) -> &[Event] {
        &self.frame_events
    }

    pub(crate) fn input_frame(&self) -> Option<&rframe::t> {
        self.input_frame.as_ref()
    }

    pub(crate) fn active(&self) -> Option<i64> {
        self.active
    }

    pub(crate) fn focus_key(&self) -> i64 {
        self.focus
    }

    /// Event ownership is recorded before release/cancellation clears capture. A viewport passes
    /// its exact root key: child widgets retain their events.
    pub fn input(&self, owner: Option<i64>) -> rframe::t {
        let owner = owner.unwrap_or(0);
        let frame = match &self.input_frame {
            Some(frame) => frame,
            None => panic!("Ui.input: no frame has completed"),
        };
        let accepts = |target: i64| !self.modal_in_frame && (target == 0 || target == owner);
        let accepts_pointer = |target: i64| accepts(target) && (owner == 0 || target == owner);
        let mut events: Vec<Event> = Vec::new();
        let (mut dx, mut dy) = (0.0, 0.0);
        let mut complete = true;
        for (target, event, (mx, my)) in &self.routed_events {
            match event {
                Event::Focus_lost | Event::Pointer_cancelled(_) => events.push(event.clone()),
                _ => {
                    let ok = match event {
                        Event::Pointer_pressed(..) | Event::Pointer_released(..) | Event::Pointer_moved(_) | Event::Scrolled(_) => {
                            accepts_pointer(*target)
                        }
                        _ => accepts(*target),
                    };
                    if ok {
                        events.push(event.clone());
                        dx += mx;
                        dy += my;
                    } else if let Event::Pointer_released(button, _) = event {
                        events.push(Event::Pointer_cancelled(*button));
                        complete = false;
                    } else {
                        complete = false;
                    }
                }
            }
        }
        let held = self.active.is_some_and(|a| accepts_pointer(a));
        let pointer = held || (self.active.is_none() && frame.input.buttons.is_empty() && accepts_pointer(self.hot));
        let mut cancelled = self.cancelled.clone();
        if self.modal_in_frame {
            cancelled.extend(frame.input.buttons.iter().cloned());
        }
        for button in cancelled {
            events.push(Event::Pointer_cancelled(button));
        }
        let mut out = frame.clone();
        out.input.events = events;
        out.input.keys = if self.modal_in_frame || self.focus != 0 { Vec::new() } else { frame.input.keys.clone() };
        out.input.buttons = if held { frame.input.buttons.clone() } else { Vec::new() };
        out.mouse_delta = if !pointer {
            (0.0, 0.0)
        } else if complete {
            frame.mouse_delta
        } else {
            (dx, dy)
        };
        out
    }

    /// Time alone changes nothing in the last frame's UI: no gesture, focused text, scroll in
    /// motion or hover delay running. A host may skip frames while its input stands still.
    pub fn at_rest(&self) -> bool {
        self.active.is_none()
            && self.focus == 0
            && self.slots.iter().all(|s| s.scroll_mode == 0)
            && self.slots.iter().all(|s| s.scroll_visual == 0.0)
            && match (&self.hover_rest, &self.input_frame) {
                (Some((_, _, start, seen)), Some(frame)) if *seen == self.frame_number => {
                    frame.input.t - start >= HOVER_SECONDS
                }
                _ => true,
            }
    }
}

/// `label` cut to `limit` points wide with an ellipsis, on a character boundary. `width` measures
/// text as the sum of its characters, so one pass from the start measures each character once and
/// stops at the first that does not fit: the cost follows what is shown, not the label's length.
pub fn ellipsis(width: &mut dyn FnMut(&str) -> f64, limit: f64, label: &str) -> String {
    let dots = width("\u{2026}");
    // `cut`: the longest prefix that fits with the ellipsis after it; -1 when the ellipsis alone does
    // not
    let mut cut: i64 = if dots <= limit { 0 } else { -1 };
    let mut used = 0.0;
    for (index, c) in label.char_indices() {
        let next = index + c.len_utf8();
        used += width(&label[index..next]);
        if used > limit {
            return if cut < 0 { String::new() } else { format!("{}\u{2026}", &label[..cut as usize]) };
        }
        if used + dots <= limit {
            cut = next as i64;
        }
    }
    label.to_owned()
}

pub(crate) const HOVER_SECONDS: f64 = 0.380;

// ------------------------------------------------------------- boxes

#[derive(Clone)]
pub struct BoxOpts {
    pub flags: Flags,
    pub w: Size,
    pub h: Size,
    pub max_h: f64,
    pub axis: Axis,
    pub padding: f64,
    pub at: Option<(f64, f64)>,
    pub scroll_step: f64,
    pub hit: Option<HitFn>,
}

impl Default for BoxOpts {
    fn default() -> BoxOpts {
        BoxOpts {
            flags: NO_FLAGS,
            w: Size::Grow,
            h: Size::Fit,
            max_h: f64::INFINITY,
            axis: Axis::Column,
            padding: 0.0,
            at: None,
            scroll_step: 24.0,
            hit: None,
        }
    }
}

impl BoxOpts {
    pub fn flags(mut self, flags: Flags) -> BoxOpts {
        self.flags = flags;
        self
    }
    pub fn w(mut self, w: Size) -> BoxOpts {
        self.w = w;
        self
    }
    pub fn h(mut self, h: Size) -> BoxOpts {
        self.h = h;
        self
    }
    pub fn max_h(mut self, max_h: f64) -> BoxOpts {
        self.max_h = max_h;
        self
    }
    pub fn axis(mut self, axis: Axis) -> BoxOpts {
        self.axis = axis;
        self
    }
    pub fn padding(mut self, padding: f64) -> BoxOpts {
        self.padding = padding;
        self
    }
    pub fn at(mut self, at: (f64, f64)) -> BoxOpts {
        self.at = Some(at);
        self
    }
    pub fn scroll_step(mut self, step: f64) -> BoxOpts {
        self.scroll_step = step;
        self
    }
    pub fn hit(mut self, hit: HitFn) -> BoxOpts {
        self.hit = Some(hit);
        self
    }
}

/// What `enter`, `scope` and `enter_root` change; `leave` puts it back.
pub struct Saved {
    parents: Vec<usize>,
    seeds: Vec<i64>,
}

#[derive(Clone, Debug)]
pub struct Signal {
    /// topmost box under the pointer, or captured
    pub hovered: bool,
    /// a press on this box began this frame
    pub pressed: bool,
    /// Ordinal of the last press on this box or any hit descendant this frame.
    pub subtree_press: Option<i64>,
    /// holds pointer capture after this frame's events
    pub held: bool,
    /// capture ended this frame
    pub released: bool,
    /// left press and release both inside the hit rect
    pub clicked: bool,
    pub double_clicked: bool,
    /// on the frame of a left press: 1, or 2 and 3 for the second and third press within 0.35 s
    /// and 5 points; 0 otherwise
    pub clicks: i64,
    /// captured and the pointer moved this frame
    pub dragging: bool,
    /// captured pointer motion this frame
    pub drag: (f64, f64),
    /// latest pointer, screen space
    pub pointer: (f64, f64),
    /// where the current or last press began
    pub press_point: (f64, f64),
    pub release_point: (f64, f64),
    pub button: Option<MouseButton>,
    /// wheel steps routed to this box
    pub scroll: (f64, f64),
    /// product of the trackpad pinch factors routed to this box this frame
    pub pinch: f64,
    /// ordered key/text events while focused
    pub keys: Vec<Event>,
}

impl Ui {
    fn require_building(&self) {
        if !self.building {
            panic!("Ui: boxes must be built inside Ui.frame");
        }
    }

    pub(crate) fn current_parent(&self) -> usize {
        self.parents.last().copied().unwrap_or(0)
    }

    pub(crate) fn current_seed(&self) -> i64 {
        self.seeds.last().copied().unwrap_or(0)
    }

    /// Two boxes with one key in a frame get distinct, order-stable keys.
    fn unique_key(&self, key: i64) -> i64 {
        let mut key = key;
        loop {
            match self.table.find(key) {
                Some(slot) if self.slots[slot].touched == self.frame_number => {
                    key = hash_range(key, "#dup", 0, 4);
                }
                _ => return key,
            }
        }
    }

    fn append_box(&mut self, key: i64, parent: usize) -> usize {
        let index = self.count;
        if index == self.boxes.len() {
            self.boxes.push(BoxRec::blank());
            self.l_x.push(0.0);
            self.l_y.push(0.0);
            self.l_w.push(0.0);
            self.l_h.push(0.0);
            self.l_content.push(0.0);
            self.l_gutter.push(0.0);
        }
        self.count = index + 1;
        let slot = self.slot_of(key);
        self.slots[slot].touched = self.frame_number;
        let b = &mut self.boxes[index];
        b.key = key;
        b.slot = slot;
        b.parent = parent;
        b.first = NONE;
        b.last = NONE;
        b.next = NONE;
        if parent != NONE {
            let last = self.boxes[parent].last;
            if last == NONE {
                self.boxes[parent].first = index;
            } else {
                self.boxes[last].next = index;
            }
            self.boxes[parent].last = index;
        }
        index
    }

    #[allow(clippy::too_many_arguments)]
    fn set_box(&mut self, index: usize, flags: Flags, w: Size, h: Size, max_h: f64, row: bool, padding: f64, at_x: f64, at_y: f64, scroll_step: f64, hit: Option<HitFn>) {
        let b = &mut self.boxes[index];
        b.flags = flags;
        b.w = w;
        b.h = h;
        b.max_h = max_h;
        b.row = row;
        b.padding = padding;
        b.at_x = at_x;
        b.at_y = at_y;
        b.scroll_step = scroll_step;
        b.hit = hit;
        b.painters.clear();
        b.overlays.clear();
    }

    pub fn box_keyed(&mut self, key: i64, opts: BoxOpts) -> UiBox {
        self.require_building();
        let key = self.unique_key(key);
        let parent = self.current_parent();
        let index = self.append_box(key, parent);
        let (at_x, at_y) = opts.at.unwrap_or((f64::NAN, f64::NAN));
        self.set_box(index, opts.flags, opts.w, opts.h, opts.max_h, opts.axis == Axis::Row, opts.padding, at_x, at_y, opts.scroll_step, opts.hit);
        UiBox { index, box_key: key, box_slot: self.boxes[index].slot }
    }

    /// Create a box under the current parent. `at` positions it outside the flow at an offset from
    /// the parent's origin. `max_h` caps a `Fit` height; overflowing `scroll` boxes reserve a
    /// 10-point scrollbar gutter. `hit` maps the laid-out rectangle to the rectangle that receives
    /// input (the default is the whole box).
    pub fn box_(&mut self, label: &str, opts: BoxOpts) -> UiBox {
        self.require_building();
        let key = key_of(self.current_seed(), label);
        self.box_keyed(key, opts)
    }

    pub fn set_at(&mut self, b: &UiBox, at: (f64, f64)) {
        self.require_building();
        self.boxes[b.index].at_x = at.0;
        self.boxes[b.index].at_y = at.1;
    }

    /// Start building children of `b`; give the result to `leave`.
    pub fn enter(&mut self, b: &UiBox) -> Saved {
        self.require_building();
        let saved = Saved { parents: self.parents.clone(), seeds: self.seeds.clone() };
        self.parents.push(b.index);
        self.seeds.push(b.box_key);
        saved
    }

    pub fn leave(&mut self, saved: Saved) {
        self.parents = saved.parents;
        self.seeds = saved.seeds;
    }

    /// Build children of `b`.
    pub fn within(&mut self, b: &UiBox, f: &mut dyn FnMut(&mut Ui)) {
        let saved = self.enter(b);
        f(self);
        self.leave(saved);
    }

    /// Mix a label into the keys of boxes built inside, without a box.
    pub fn scope(&mut self, label: &str, f: &mut dyn FnMut(&mut Ui)) {
        self.require_building();
        let saved = Saved { parents: self.parents.clone(), seeds: self.seeds.clone() };
        let seed = key_of(self.current_seed(), label);
        self.seeds.push(seed);
        f(self);
        self.leave(saved);
    }

    /// Build at the root under the root's own seed, wherever the caller is in the tree: what is
    /// built is laid out in the window, clipped by no pane.
    pub(crate) fn enter_root(&mut self) -> Saved {
        Saved {
            parents: std::mem::replace(&mut self.parents, vec![0]),
            seeds: std::mem::replace(&mut self.seeds, vec![ROOT_KEY]),
        }
    }

    pub fn box_of_key(&self, key: i64) -> Option<UiBox> {
        self.table.find(key).map(|slot| UiBox { index: NONE, box_key: key, box_slot: slot })
    }

    /// The box's screen rectangle from the previous frame.
    pub fn rect(&self, b: &UiBox) -> Rect {
        let s = &self.slots[b.box_slot];
        (s.rx, s.ry, s.rw, s.rh)
    }

    pub fn signal(&self, b: &UiBox) -> Signal {
        let key = b.box_key;
        let held = self.active == Some(key);
        let slot = &self.slots[b.box_slot];
        match self.signals.get(&key) {
            None => Signal {
                hovered: self.hot == key,
                pressed: false,
                subtree_press: None,
                held,
                released: false,
                clicked: false,
                double_clicked: false,
                clicks: 0,
                dragging: false,
                drag: (0.0, 0.0),
                pointer: self.pointer,
                press_point: if held { self.active_press } else { (slot.press_x, slot.press_y) },
                release_point: self.pointer,
                button: if held { Some(self.active_button) } else { None },
                scroll: (0.0, 0.0),
                pinch: 1.0,
                keys: Vec::new(),
            },
            Some(value) => Signal {
                hovered: self.hot == key,
                pressed: value.pressed,
                subtree_press: value.subtree_press,
                held,
                released: value.released,
                clicked: value.clicked,
                double_clicked: value.double_clicked,
                clicks: value.clicks,
                dragging: value.moved && (held || value.released),
                drag: (value.drag_x, value.drag_y),
                pointer: self.pointer,
                press_point: if value.pressed || value.released {
                    value.press_point
                } else if held {
                    self.active_press
                } else {
                    (slot.press_x, slot.press_y)
                },
                release_point: value.release_point,
                button: match value.button {
                    Some(b) => Some(b),
                    None => if held { Some(self.active_button) } else { None },
                },
                scroll: (value.scroll_x, value.scroll_y_steps),
                pinch: value.pinch,
                keys: value.keys.iter().map(|(e, _)| e.clone()).collect(),
            },
        }
    }

    /// Key/text events of the focused box with the modifiers held at each.
    pub fn key_events(&self, b: &UiBox) -> Vec<(Event, Vec<Key>)> {
        match self.signals.get(&b.box_key) {
            None => Vec::new(),
            Some(value) => value.keys.clone(),
        }
    }

    /// Held keys at this box's captured press, retained through release.
    pub fn press_keys(&self, b: &UiBox) -> Vec<Key> {
        match self.signals.get(&b.box_key) {
            None => if self.active == Some(b.box_key) { self.active_keys.clone() } else { Vec::new() },
            Some(value) => value.press_keys.clone(),
        }
    }

    pub(crate) fn press_shift(&self, b: &UiBox) -> bool {
        self.press_keys(b).contains(&Key::Shift)
    }

    /// The released right-click in this box or a descendant, through shared capture.
    pub fn context_at(&mut self, b: &UiBox) -> Option<(f64, f64)> {
        let targets: Vec<(i64, (f64, f64))> = self
            .signals
            .iter()
            .filter(|(_, v)| {
                v.released && v.button == Some(MouseButton::RightButton) && {
                    let (px, py) = v.press_point;
                    let (rx, ry) = v.release_point;
                    (px - rx).hypot(py - ry) < 4.0
                }
            })
            .map(|(k, v)| (*k, v.release_point))
            .collect();
        for (target, point) in targets {
            if self.hit_within(target, b.box_key) {
                return Some(point);
            }
        }
        None
    }

    pub fn focused(&self, b: &UiBox) -> bool {
        self.focus == b.box_key
    }

    pub fn focus_box(&mut self, b: &UiBox) {
        if self.focus != b.box_key {
            self.composition.clear();
            self.edit_focus = 0;
        }
        self.focus = b.box_key;
    }

    /// The box or a hit descendant owns hover in the shared hit tree.
    pub fn hovered_within(&mut self, b: &UiBox) -> bool {
        let hot = self.hot;
        self.hit_within(hot, b.box_key)
    }

    // ------------------------------------------------------------- carry

    pub fn carrying(&self) -> Option<Payload> {
        self.payload.as_ref().map(|(p, _)| p.clone())
    }

    /// Hold a payload. With `from`, the box that owns the pressed left button: it is held once the
    /// pointer has moved 4 points from the press, so a widget calls it every frame while pressed.
    /// Without `from` (a key) it is held at once, with no capture.
    pub fn carry(&mut self, from: Option<&UiBox>, kind: &str, value: &str) {
        match from {
            None => {
                if let Some((held, Origin::From_key)) = &self.payload {
                    if held.kind == kind && held.value == value {
                        return;
                    }
                }
                self.payload = Some((Payload { kind: kind.to_owned(), value: value.to_owned() }, Origin::From_key));
            }
            Some(b) => {
                if self.payload.is_none()
                    && self.active == Some(b.box_key)
                    && self.carry_latch != self.active
                    && self.active_button == MouseButton::LeftButton
                {
                    let (px, py) = self.active_press;
                    let (x, y) = self.pointer;
                    if (x - px).hypot(y - py) >= drag_threshold {
                        self.payload = Some((
                            Payload { kind: kind.to_owned(), value: value.to_owned() },
                            Origin::From_pointer(b.box_key),
                        ));
                    }
                }
            }
        }
    }

    /// Forget the payload: the host cancelled (Escape) or finished a carry it started.
    pub fn cancel_carry(&mut self) {
        if self.payload.is_some() {
            self.carry_latch = self.active;
        }
        self.payload = None;
    }

    pub fn drop_target(&mut self, b: &UiBox) -> Option<Drop> {
        if let Some((payload, hot)) = self.dropped.clone() {
            if hot != 0 && self.hit_within(hot, b.box_key) { Some(Drop::Dropped(payload)) } else { None }
        } else if let Some((payload, _)) = self.payload.clone() {
            let hot = self.hot;
            if hot != 0 && self.hit_within(hot, b.box_key) { Some(Drop::Hover(payload)) } else { None }
        } else {
            None
        }
    }

    pub fn scroll_offset(&self, b: &UiBox) -> f64 {
        self.scroll_y[b.box_slot]
    }

    /// Current painted scroll position, including elastic edge movement.
    pub fn scroll_position(&self, b: &UiBox) -> f64 {
        self.scroll_y[b.box_slot] + self.slots[b.box_slot].scroll_visual
    }

    pub fn set_scroll_offset(&mut self, b: &UiBox, value: f64) {
        self.scroll_y[b.box_slot] = value;
        let s = &mut self.slots[b.box_slot];
        s.scroll_raw = 0.0;
        s.scroll_visual = 0.0;
        s.scroll_mode = 0;
    }

    /// Retained per-box scalar state for custom widgets.
    pub fn state(&self, b: &UiBox, default: i64) -> i64 {
        let value = self.slots[b.box_slot].state_value;
        if value == MIN_INT { default } else { value }
    }

    pub fn set_state(&mut self, b: &UiBox, value: i64) {
        self.slots[b.box_slot].state_value = value;
    }

    pub fn text_state(&self, b: &UiBox) -> Option<String> {
        self.slots[b.box_slot].text_value.clone()
    }

    pub fn set_text_state(&mut self, b: &UiBox, value: Option<String>) {
        self.slots[b.box_slot].text_value = value;
    }

    /// Paint with the box's final laid-out rectangle, before its children.
    pub fn draw(&mut self, b: &UiBox, painter: Box<dyn Fn(&mut Paint, Rect)>) {
        self.boxes[b.index].painters.push(Rc::from(painter));
    }

    /// Paint after the box's children, outside its content clip.
    pub fn draw_over(&mut self, b: &UiBox, painter: Box<dyn Fn(&mut Paint, Rect)>) {
        self.boxes[b.index].overlays.push(Rc::from(painter));
    }

    /// Raise a root box and its children above ordinary boxes, below modal popups. Higher `order`
    /// values paint later; equal values retain their order of calls.
    pub fn to_front(&mut self, order: i64, b: &UiBox) {
        self.require_building();
        if self.boxes[b.index].parent != 0 {
            panic!("Ui.to_front: expected a root box");
        }
        self.foreground.push((order, b.index));
    }

    pub(crate) fn box_flags(&self, b: &UiBox) -> Flags {
        self.boxes[b.index].flags
    }

    pub(crate) fn set_box_flags(&mut self, b: &UiBox, flags: Flags) {
        self.boxes[b.index].flags = flags;
    }

    pub(crate) fn set_box_h(&mut self, b: &UiBox, h: Size) {
        self.boxes[b.index].h = h;
    }

    /// The last child built under the current parent (`NONE` when it has none yet).
    pub(crate) fn last_child_of_parent(&self) -> usize {
        self.boxes[self.current_parent()].last
    }

    pub(crate) fn box_w_px(&self, index: usize) -> Option<f64> {
        match self.boxes[index].w {
            Size::Px(w) => Some(w),
            _ => None,
        }
    }

    pub(crate) fn box_parent(&self, index: usize) -> usize {
        self.boxes[index].parent
    }

    pub(crate) fn box_count(&self) -> usize {
        self.count
    }

    pub(crate) fn push_overlay(&mut self, index: usize) {
        self.overlays.push(index);
    }

    pub(crate) fn modal_key(&self) -> Option<i64> {
        self.modal_key
    }

    pub(crate) fn slot_rect(&self, slot: usize) -> Rect {
        let s = &self.slots[slot];
        (s.rx, s.ry, s.rw, s.rh)
    }

    pub(crate) fn slot_for(&self, key: i64) -> Option<usize> {
        self.table.find(key)
    }

    pub(crate) fn slot_state(&self, slot: usize) -> i64 {
        self.slots[slot].state_value
    }

    // ------------------------------------------------------------- layout

    fn flow(&self, index: usize) -> bool {
        self.boxes[index].at_x.is_nan()
    }

    /// Bottom-up: fixed and fit sizes. Children follow their parent in build order, so a reverse
    /// sweep sees every child first.
    fn intrinsic(&mut self) {
        for index in (0..self.count).rev() {
            let padding = self.boxes[index].padding;
            let row = self.boxes[index].row;
            let mut along = 0.0;
            let mut across: f64 = 0.0;
            let mut next = self.boxes[index].first;
            while next != NONE {
                let child = next;
                if self.flow(child) {
                    let cw = self.l_w[child];
                    let ch = self.l_h[child];
                    if row {
                        along += cw;
                        across = across.max(ch);
                    } else {
                        along += ch;
                        across = across.max(cw);
                    }
                }
                next = self.boxes[child].next;
            }
            let content_w = (if row { along } else { across }) + 2.0 * padding;
            let content_h = (if row { across } else { along }) + 2.0 * padding;
            self.l_content[index] = content_h;
            let resolve = |kind: &Size, content: f64| match kind {
                Size::Px(value) => *value,
                Size::Fit => content,
                _ => 0.0,
            };
            self.l_w[index] = resolve(&self.boxes[index].w, content_w);
            self.l_h[index] = self.boxes[index].max_h.min(resolve(&self.boxes[index].h, content_h));
        }
    }

    fn pull(&mut self, slot: usize, max_scroll: f64, height: f64, past: f64, delta: f64) {
        let old = self.slots[slot].scroll_raw;
        let next = self.scroll_y[slot] + old - delta;
        let bounded = 0.0f64.max(max_scroll.min(next));
        let over = next - bounded;
        let raw = if max_scroll <= 0.0 || over == 0.0 { 0.0 } else { old + (over - old) * past };
        self.scroll_y[slot] = bounded;
        self.slots[slot].scroll_raw = raw;
        self.slots[slot].scroll_visual = scroll_stretch(raw, height);
    }

    /// One scroll box, once its height is laid out (`arrange`): a height that comes from the
    /// parent (Grow, Pct, Rel) stretches and bounces as a fixed one does.
    pub(crate) fn apply_scroll(&mut self, consume: bool, time: f64, index: usize) {
        let slot = self.boxes[index].slot;
        let key = self.boxes[index].key;
        let height = self.l_h[index];
        let max_scroll = 0.0f64.max(self.l_content[index] - height);
        let (wheel, drag, touch, lift) = match self.signals.get(&key) {
            Some(v) => (v.scroll_wheel, v.scroll_drag, v.scroll_touch, v.scroll_lift),
            None => (0.0, 0.0, false, false),
        };
        // any input catches a coast or a bounce where it is
        if touch || wheel != 0.0 || drag != 0.0 {
            self.slots[slot].scroll_mode = 0;
        }
        if touch {
            self.slots[slot].scroll_event_time = f64::INFINITY;
        }
        if drag != 0.0 {
            self.pull(slot, max_scroll, height, 1.0, drag);
            // held by the fingers: it springs back when they lift
            self.slots[slot].scroll_event_time = f64::INFINITY;
        } else if wheel != 0.0 {
            let step = self.boxes[index].scroll_step;
            self.pull(slot, max_scroll, height, SCROLL_WHEEL_PAST, wheel * step);
            self.slots[slot].scroll_event_time = time;
        }
        if lift {
            if self.slots[slot].scroll_visual != 0.0 {
                self.slots[slot].scroll_event_time = f64::NEG_INFINITY;
            } else if max_scroll > 0.0 && self.gesture_velocity.abs() >= SCROLL_MIN_VELOCITY {
                let y = self.scroll_y[slot];
                let s = &mut self.slots[slot];
                s.scroll_mode = 1;
                s.scroll_velocity = self.gesture_velocity;
                s.scroll_start = time;
                s.scroll_from = y;
            }
        }
        if wheel == 0.0 && drag == 0.0 {
            if self.slots[slot].scroll_mode == 1 {
                // v(t) = v0 e^(-t/tau), so y(t) = y0 + v0 tau (1 - e^(-t/tau))
                let velocity = self.slots[slot].scroll_velocity;
                let from = self.slots[slot].scroll_from;
                let at = |t: f64| from + velocity * SCROLL_COAST_TAU * (1.0 - (-t / SCROLL_COAST_TAU).exp());
                let t = 0.0f64.max(time - self.slots[slot].scroll_start);
                let y = at(t);
                if y < 0.0 || y > max_scroll {
                    // the coast is monotonic: bisect for when it met the edge and bounce from then
                    let edge = if y < 0.0 { 0.0 } else { max_scroll };
                    let mut early = 0.0;
                    let mut late = t;
                    for _ in 0..24 {
                        let mid = (early + late) / 2.0;
                        if (at(mid) - edge) * (y - edge) > 0.0 { late = mid } else { early = mid }
                    }
                    let arrival = velocity * (-late / SCROLL_COAST_TAU).exp();
                    self.scroll_y[slot] = edge;
                    let s = &mut self.slots[slot];
                    s.scroll_mode = 2;
                    s.scroll_start += late;
                    s.scroll_velocity = SCROLL_BOUNCE_LAUNCH * arrival / (SCROLL_BOUNCE_B - SCROLL_BOUNCE_A);
                } else {
                    // on whole device pixels, so text is sharp where the coast ends
                    let density = self.density as f64;
                    self.scroll_y[slot] = ocaml_round(y * density) / density;
                    if (velocity * (-t / SCROLL_COAST_TAU).exp()).abs() < SCROLL_MIN_VELOCITY {
                        self.slots[slot].scroll_mode = 0;
                    }
                }
            }
            if self.slots[slot].scroll_mode == 2 {
                let t = 0.0f64.max(time - self.slots[slot].scroll_start);
                let visual = self.slots[slot].scroll_velocity
                    * ((-SCROLL_BOUNCE_A * t).exp() - (-SCROLL_BOUNCE_B * t).exp());
                let settled = t > 1.0 / SCROLL_BOUNCE_A && visual.abs() < 0.25;
                if settled {
                    self.slots[slot].scroll_mode = 0;
                }
                self.slots[slot].scroll_visual = if settled { 0.0 } else { visual };
                self.slots[slot].scroll_raw = scroll_travel(self.slots[slot].scroll_visual, height);
            } else if self.slots[slot].scroll_visual != 0.0 && time - self.slots[slot].scroll_event_time > 0.08 {
                let dt = 0.0f64.max(time - self.slots[slot].scroll_frame_time);
                let visual = self.slots[slot].scroll_visual * (-dt / SCROLL_SPRING_TAU).exp();
                self.slots[slot].scroll_visual = if visual.abs() < 0.25 { 0.0 } else { visual };
                self.slots[slot].scroll_raw = scroll_travel(self.slots[slot].scroll_visual, height);
            }
        }
        self.slots[slot].scroll_frame_time = time;
        if consume {
            if let Some(value) = self.signals.get_mut(&key) {
                value.scroll_wheel = 0.0;
                value.scroll_drag = 0.0;
                value.scroll_touch = false;
                value.scroll_lift = false;
            }
        }
    }

    /// Top-down: relative sizes, grow shares, scrolling, scroll gutters, and positions.
    fn arrange(&mut self, time: f64) {
        self.l_x[0] = 0.0;
        self.l_y[0] = 0.0;
        for index in 0..self.count {
            let padding = self.boxes[index].padding;
            let row = self.boxes[index].row;
            let flags = self.boxes[index].flags;
            let scrolls = flags & SCROLL != 0;
            let max_scroll = 0.0f64.max(self.l_content[index] - self.l_h[index]);
            let gutter = if scrolls && max_scroll > 0.0 && flags & OVER_THUMB == 0 { 10.0 } else { 0.0 };
            self.l_gutter[index] = gutter;
            let slot = self.boxes[index].slot;
            if scrolls {
                self.apply_scroll(false, time, index);
                self.scroll_y[slot] = 0.0f64.max(max_scroll.min(self.scroll_y[slot]));
            }
            let inner_w = self.l_w[index] - 2.0 * padding - gutter;
            let inner_h = self.l_h[index] - 2.0 * padding;
            let relative = |kind: &Size, inner: f64, current: f64| match kind {
                Size::Pct(fraction) => inner * fraction,
                Size::Rel(f) => f(inner),
                _ => current,
            };
            let mut fixed = 0.0;
            let mut grow_count = 0;
            let mut next = self.boxes[index].first;
            while next != NONE {
                let child = next;
                self.l_w[child] = relative(&self.boxes[child].w, inner_w, self.l_w[child]);
                self.l_h[child] = self.boxes[child].max_h.min(relative(&self.boxes[child].h, inner_h, self.l_h[child]));
                if self.flow(child) {
                    let grows = if row { self.boxes[child].w.is_grow() } else { self.boxes[child].h.is_grow() };
                    if grows {
                        grow_count += 1;
                    } else {
                        fixed += if row { self.l_w[child] } else { self.l_h[child] };
                    }
                }
                next = self.boxes[child].next;
            }
            let share = if grow_count == 0 {
                0.0
            } else {
                0.0f64.max(((if row { inner_w } else { inner_h }) - fixed) / grow_count as f64)
            };
            let mut cursor = padding;
            let scroll_offset = if scrolls { self.scroll_y[slot] + self.slots[slot].scroll_visual } else { 0.0 };
            next = self.boxes[index].first;
            while next != NONE {
                let child = next;
                if row {
                    if self.boxes[child].w.is_grow() && self.flow(child) {
                        self.l_w[child] = share;
                    }
                    if self.boxes[child].h.is_grow() {
                        self.l_h[child] = self.boxes[child].max_h.min(inner_h);
                    }
                } else {
                    if self.boxes[child].h.is_grow() && self.flow(child) {
                        self.l_h[child] = self.boxes[child].max_h.min(share);
                    }
                    if self.boxes[child].w.is_grow() {
                        self.l_w[child] = inner_w;
                    }
                }
                if self.flow(child) {
                    if row {
                        self.l_x[child] = self.l_x[index] + cursor;
                        self.l_y[child] = self.l_y[index] + padding - scroll_offset;
                        cursor += self.l_w[child];
                    } else {
                        self.l_x[child] = self.l_x[index] + padding;
                        self.l_y[child] = self.l_y[index] + cursor - scroll_offset;
                        cursor += self.l_h[child];
                    }
                } else {
                    self.l_x[child] = self.l_x[index] + self.boxes[child].at_x;
                    self.l_y[child] = self.l_y[index] + self.boxes[child].at_y;
                }
                next = self.boxes[child].next;
            }
        }
    }

    fn screen(&self, index: usize) -> Rect {
        (self.l_x[index], self.l_y[index], self.l_w[index], self.l_h[index])
    }

    pub(crate) fn set_l_h(&mut self, b: &UiBox, h: f64) {
        self.l_h[b.index] = h;
    }

    pub(crate) fn set_l_content(&mut self, b: &UiBox, v: f64) {
        self.l_content[b.index] = v;
    }
}

// Scrolling as NSScrollView does it, the constants fitted to a recording of one. Past an edge
// `travel` points of input show as `stretch travel height`; left alone the overscroll decays with
// tau 0.084 s. A trackpad's fingers move the content point for point and hold a stretch until
// they lift; lifted in motion, the box coasts on the release velocity decaying with tau 0.26 s,
// and reaching an edge it overshoots as the overdamped spring. Every motion is a closed form of
// the time since it began, so the frame rate does not change it.
const SCROLL_STIFFNESS: f64 = 0.55;
const SCROLL_WHEEL_PAST: f64 = 0.05 / 0.55;
const SCROLL_SPRING_TAU: f64 = 0.084;
const SCROLL_COAST_TAU: f64 = 0.26;
const SCROLL_BOUNCE_A: f64 = 9.25;
const SCROLL_BOUNCE_B: f64 = 19.0;
const SCROLL_BOUNCE_LAUNCH: f64 = 0.39;
const SCROLL_MIN_VELOCITY: f64 = 20.0;

fn scroll_stretch(travel: f64, height: f64) -> f64 {
    if height <= 0.0 {
        0.0
    } else {
        SCROLL_STIFFNESS * travel / (1.0 + SCROLL_STIFFNESS * travel.abs() / height)
    }
}

/// the travel that shows `overscroll`: the inverse of `scroll_stretch`
fn scroll_travel(overscroll: f64, height: f64) -> f64 {
    if height <= 0.0 || overscroll == 0.0 {
        0.0
    } else {
        let shown = overscroll.abs().min(height * 0.999);
        (shown / (SCROLL_STIFFNESS * (1.0 - shown / height))).copysign(overscroll)
    }
}

// ----------------------------------------------------------- painting

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Dir {
    Down,
    Up,
    Left,
    Right,
}

/// Where the top of kit text of `size` points goes to sit centred in a row at `y` of height `h`: its
/// top is half of what the row has over the text size. Measured against the kit's reference
/// renders: 13-point text in a 24-point row has its ascenders at 7 and its descenders at 18.
pub fn text_top_for(font_size: i64, size: Option<i64>, y: f64, h: f64) -> f64 {
    y + ((h - size.unwrap_or(font_size) as f64) / 2.0).floor()
}

/// The painter's handle for one frame. It owns the instance builder, the glyph atlas and the
/// layout arrays painters read while painting; `Ui::frame` hands them back afterwards.
pub struct Paint {
    pub builder: ui_batch::Builder,
    pub clip_rect: Rect,
    pub theme: Theme,
    pub font_size: i64,
    pub row_height: i64,
    pub fonts: Fonts,
    pub borrowed_images: Vec<(i64, image::t)>,
    pub regions: Vec<scene::node>,
    /// the horizontal scroll of the focused text; painters adjust it
    pub edit_scroll_x: f64,
    /// the content height of each box of this frame, by box index
    pub l_content: Vec<f64>,
    /// the scroll offset of each retained slot
    pub scroll_y: Vec<f64>,
}

impl Paint {
    fn prepare(&mut self) {
        let (x, y, w, h) = self.clip_rect;
        self.builder.set_clip(Some(Clip { x, y, width: w, height: h }));
    }

    /// Borrow a host-owned image for this frame through the normal UI texture batch.
    pub fn image(&mut self, x: f64, y: f64, w: f64, h: f64, image: &image::t) {
        // ponytail: at most 64 borrowed images per frame, linear identity lookup.
        let id = match self.borrowed_images.iter().find(|(_, other)| image::same(other, image)) {
            Some((id, _)) => *id,
            None => {
                if self.borrowed_images.len() >= 64 {
                    panic!("Ui.Paint.image: 64 images per frame");
                }
                let id = self.borrowed_images.len() as i64 + 2;
                self.borrowed_images.insert(0, (id, image.clone()));
                id
            }
        };
        self.prepare();
        let (iw, ih) = image::dimensions(image);
        self.builder
            .textured(id, x, y, w, h, 0.0, 0.0, iw as f64, ih as f64, -1)
            .expect("Ui.Paint.image");
    }

    /// Square fills cover exactly the pixels of the equivalent triangle rectangle.
    pub fn fill(&mut self, x: f64, y: f64, w: f64, h: f64, color: Color) {
        self.prepare();
        self.builder.rect(x, y, w, h, packed(color), 0, 0.0, 0.0, false).expect("Ui.Paint.fill");
    }

    /// A band of `width` centred on the rectangle's edges.
    pub fn stroke_w(&mut self, x: f64, y: f64, w: f64, h: f64, width: f64, color: Color) {
        self.prepare();
        self.builder.rect(x, y, w, h, 0, packed(color), width, 0.0, false).expect("Ui.Paint.stroke");
    }

    pub fn stroke(&mut self, x: f64, y: f64, w: f64, h: f64, color: Color) {
        self.stroke_w(x, y, w, h, 1.0, color);
    }

    /// A border drawn inside its box, as CSS draws one: `stroke` centres its line on the
    /// rectangle's edge, so a box of whole points gets the line half a width in; the border then
    /// covers whole points.
    pub fn frame_w(&mut self, x: f64, y: f64, w: f64, h: f64, width: f64, color: Color) {
        let half = width / 2.0;
        self.stroke_w(x + half, y + half, w - width, h - width, width, color);
    }

    pub fn frame(&mut self, x: f64, y: f64, w: f64, h: f64, color: Color) {
        self.frame_w(x, y, w, h, 1.0, color);
    }

    pub fn rect(&mut self, x: f64, y: f64, w: f64, h: f64, fill: Option<Color>, stroke: Option<Color>) {
        let fill = match (fill, stroke) {
            (None, None) => Some(color::white()),
            _ => fill,
        };
        if let Some(c) = fill {
            self.fill(x, y, w, h, c);
        }
        if let Some(c) = stroke {
            self.stroke(x, y, w, h, c);
        }
    }

    fn wire(&mut self, p0: (f64, f64), p1: (f64, f64), p2: (f64, f64), p3: (f64, f64), width: f64, color: Color) {
        self.prepare();
        self.builder.wire([p0, p1, p2, p3], width, packed(color)).expect("Ui.Paint.wire");
    }

    /// Butt-capped; axis-aligned lines are exact rectangles.
    pub fn line_w(&mut self, from: (f64, f64), to: (f64, f64), width: f64, color: Color) {
        let (x0, y0) = from;
        let (x1, y1) = to;
        let half = width / 2.0;
        if x0 == x1 {
            self.fill(x0 - half, y0.min(y1), width, (y1 - y0).abs(), color);
        } else if y0 == y1 {
            self.fill(x0.min(x1), y0 - half, (x1 - x0).abs(), width, color);
        } else {
            self.wire(
                (x0, y0),
                (x0 + (x1 - x0) / 3.0, y0 + (y1 - y0) / 3.0),
                (x0 + 2.0 * (x1 - x0) / 3.0, y0 + 2.0 * (y1 - y0) / 3.0),
                (x1, y1),
                width,
                color,
            );
        }
    }

    pub fn line(&mut self, from: (f64, f64), to: (f64, f64), color: Color) {
        self.line_w(from, to, 1.0, color);
    }

    pub fn circle(&mut self, at: (f64, f64), radius: f64, fill: Option<Color>, stroke: Option<Color>) {
        self.prepare();
        let fill = match (fill, stroke) {
            (None, None) => Some(color::white()),
            _ => fill,
        };
        self.builder
            .rect(
                at.0 - radius,
                at.1 - radius,
                2.0 * radius,
                2.0 * radius,
                fill.map(packed).unwrap_or(0),
                stroke.map(packed).unwrap_or(0),
                if stroke.is_some() { 1.0 } else { 0.0 },
                radius,
                true,
            )
            .expect("Ui.Paint.circle");
    }

    /// Dots every `spacing` points from `origin`, drawn by one quad.
    pub fn grid(&mut self, x: f64, y: f64, w: f64, h: f64, origin: (f64, f64), spacing: f64, dot: f64, color: Color) {
        self.prepare();
        self.builder.grid(x, y, w, h, origin.0, origin.1, spacing, dot, packed(color)).expect("Ui.Paint.grid");
    }

    /// Kit text with its top-left corner at `at`. Without `size` it uses the UI's own font; with
    /// `size` the kit face at that size. Glyphs stay on physical pixels. `tracking` adds points
    /// between glyphs.
    pub fn text_with(&mut self, at: (f64, f64), size: Option<i64>, tracking: f64, color: Option<Color>, text: &str) {
        if text.is_empty() {
            return;
        }
        let Some(font) = self.fonts.face(size) else { return };
        self.prepare();
        let id = self.fonts.atlas.font_id(&font);
        let packed_color = packed(color.unwrap_or(self.theme.foreground));
        let density = self.fonts.density as f64;
        // Snap the run's origin to a physical pixel so each glyph texel lands on exactly one
        // backing pixel, as whole-string text does.
        let snap = |value: f64| ocaml_round(value * density) / density;
        let screen_x = snap(at.0);
        let shift = self.fonts.ascent_shift(size.unwrap_or(self.font_size), self.fonts.density);
        let screen_y = snap(at.1 + shift);
        // the pen runs in fractional physical pixels (0.88 points of tracking is 1.76 at 2x); each
        // glyph is placed on a whole pixel
        let mut pen = 0.0;
        let tracking = tracking * density;
        for c in text.chars() {
            if let Some(glyph) = self.fonts.glyph(&font, id, c as i64) {
                if glyph.gw > 0 {
                    let gx = screen_x + ocaml_round(pen) / density;
                    let gw = glyph.gw as f64 / density;
                    let gh = glyph.gh as f64 / density;
                    self.builder
                        .textured(
                            1,
                            gx,
                            screen_y,
                            gw,
                            gh,
                            glyph.gx as f64,
                            glyph.gy as f64,
                            (glyph.gx + glyph.gw) as f64,
                            (glyph.gy + glyph.gh) as f64,
                            packed_color,
                        )
                        .expect("Ui.Paint.text");
                }
                pen += glyph.advance as f64 + tracking;
            }
        }
    }

    pub fn text(&mut self, at: (f64, f64), text: &str) {
        self.text_with(at, None, 0.0, None, text);
    }

    pub fn text_c(&mut self, at: (f64, f64), color: Color, text: &str) {
        self.text_with(at, None, 0.0, Some(color), text);
    }

    pub fn text_s(&mut self, at: (f64, f64), size: i64, color: Color, text: &str) {
        self.text_with(at, Some(size), 0.0, Some(color), text);
    }

    pub fn text_width(&mut self, size: Option<i64>, text: &str) -> f64 {
        self.fonts.text_width(size, text)
    }

    /// Where the top of kit text of `size` goes to sit centred in a row at `y` of height `h`.
    pub fn text_top(&self, size: Option<i64>, y: f64, h: f64) -> f64 {
        text_top_for(self.font_size, size, y, h)
    }

    /// The label style: upper case, two points under the body, 0.08 em of tracking.
    pub fn label_size(&self) -> i64 {
        8.max(self.font_size - 2)
    }

    pub fn cap_tracking(&self) -> f64 {
        0.08 * self.label_size() as f64
    }

    /// A label: upper case at `label_size` with 0.08 em of tracking, ink-2 by default.
    pub fn cap(&mut self, at: (f64, f64), color: Option<Color>, label: &str) {
        let color = color.unwrap_or(theme::ink_2(&self.theme));
        let size = self.label_size();
        let tracking = self.cap_tracking();
        self.text_with(at, Some(size), tracking, Some(color), &label.to_ascii_uppercase());
    }

    /// The tracking follows every letter, the last too (CSS letter-spacing): text after a label
    /// starts one step later, and a right-aligned label ends one step early.
    pub fn cap_width(&mut self, label: &str) -> f64 {
        let label = label.to_ascii_uppercase();
        let count = label.chars().count();
        let size = self.label_size();
        self.text_width(Some(size), &label) + self.cap_tracking() * count as f64
    }

    /// A chevron centred on `at`: its strokes span 7 x 3.5, which with the 1-point line is the
    /// sheet's 9 x 5.5 of ink.
    pub fn chevron(&mut self, at: (f64, f64), direction: Dir, color: Color) {
        let (a, b) = match direction {
            Dir::Down => ((-3.5, -1.75), (3.5, -1.75)),
            Dir::Up => ((-3.5, 1.75), (3.5, 1.75)),
            Dir::Right => ((-1.75, -3.5), (-1.75, 3.5)),
            Dir::Left => ((1.75, -3.5), (1.75, 3.5)),
        };
        let tip = match direction {
            Dir::Down => (0.0, 1.75),
            Dir::Up => (0.0, -1.75),
            Dir::Right => (1.75, 0.0),
            Dir::Left => (-1.75, 0.0),
        };
        let p = |d: (f64, f64)| (at.0 + d.0, at.1 + d.1);
        self.line(p(a), p(tip), color);
        self.line(p(tip), p(b), color);
    }

    /// Selection: four corner brackets `offset` outside the rectangle.
    pub fn brackets(&mut self, x: f64, y: f64, w: f64, h: f64, offset: f64, length: f64, width: f64, color: Color) {
        let x = x - offset;
        let y = y - offset;
        let w = w + 2.0 * offset;
        let h = h + 2.0 * offset;
        for (cx, cy, sx, sy) in [(x, y, 1.0, 1.0), (x + w, y, -1.0, 1.0), (x, y + h, 1.0, -1.0), (x + w, y + h, -1.0, -1.0)] {
            self.fill(
                if sx > 0.0 { cx } else { cx - length },
                if sy > 0.0 { cy } else { cy - width },
                length,
                width,
                color,
            );
            self.fill(
                if sx > 0.0 { cx } else { cx - width },
                if sy > 0.0 { cy } else { cy - length },
                width,
                length,
                color,
            );
        }
    }

    pub fn dashed_w(&mut self, from: (f64, f64), to: (f64, f64), width: f64, color: Color) {
        let (x0, y0) = from;
        let (x1, y1) = to;
        let length = (x1 - x0).hypot(y1 - y0);
        let steps = (length / 7.0).ceil() as i64;
        for step in 0..steps {
            let a = step as f64 * 7.0 / length;
            let b = 1.0f64.min((step as f64 * 7.0 + 4.0) / length);
            self.line_w((x0 + (x1 - x0) * a, y0 + (y1 - y0) * a), (x0 + (x1 - x0) * b, y0 + (y1 - y0) * b), width, color);
        }
    }

    pub fn dashed(&mut self, from: (f64, f64), to: (f64, f64), color: Color) {
        self.dashed_w(from, to, 1.0, color);
    }

    /// 4 on, 3 off: a zone, a drop target.
    pub fn dashed_rect(&mut self, x: f64, y: f64, w: f64, h: f64, color: Color) {
        self.dashed((x, y), (x + w, y), color);
        self.dashed((x, y + h), (x + w, y + h), color);
        self.dashed((x, y), (x, y + h), color);
        self.dashed((x + w, y), (x + w, y + h), color);
    }

    /// An outline flag centred at `at`: 12 points, the input fill, a line-3 edge inside the box
    /// and, when on, a 6-point mark in ink-2; square, or `round` for the columns after the first.
    pub fn flag(&mut self, at: (f64, f64), round: bool, on: bool) {
        let theme = self.theme;
        let (cx, cy) = at;
        if round {
            self.circle((cx, cy), 5.5, Some(theme.input), Some(theme::border(&theme)));
            if on {
                self.circle((cx, cy), 3.0, Some(theme::ink_2(&theme)), None);
            }
        } else {
            self.fill(cx - 6.0, cy - 6.0, 12.0, 12.0, theme.input);
            self.frame(cx - 6.0, cy - 6.0, 12.0, 12.0, theme::border(&theme));
            if on {
                self.fill(cx - 3.0, cy - 3.0, 6.0, 6.0, theme::ink_2(&theme));
            }
        }
    }

    /// The ratio of a splitter being dragged, `first / second`, as an accent label with no box.
    pub fn ratio(&mut self, at: (f64, f64), first: i64, second: i64) {
        let accent = self.theme.accent;
        self.cap(at, Some(accent), &format!("{} / {}", first, second));
    }

    pub fn input_region(&mut self, cursor: f64, x: f64, y: f64, w: f64, h: f64, focused: bool) {
        let region = scene::text_input_region(
            (x as i64, y as i64),
            w as i64,
            h as i64,
            Some(focused),
            Some(0.max(ocaml_round(cursor) as i64)),
        )
        .expect("Ui.Paint.input_region");
        self.regions.push(region);
    }
}

impl Ui {
    fn record_hit(&mut self, index: usize, parent: i64, rect: Rect) {
        let position = self.hit_count;
        let (x, y, w, h) = rect;
        let rec = HitRec { key: self.boxes[index].key, parent, flags: self.boxes[index].flags, x, y, w, h };
        if position >= self.hits.len() {
            self.hits.push(rec);
        } else {
            self.hits[position] = rec;
        }
        self.chain_key = 0;
        self.hit_count = position + 1;
    }

    fn run_painters(&self, painters: &[Painter], index: usize, clip_rect: Rect, paint: &mut Paint) {
        if !painters.is_empty() {
            paint.clip_rect = clip_rect;
            let local = self.screen(index);
            for painter in painters {
                painter(paint, local);
            }
        }
    }

    fn visit(&mut self, paint: &mut Paint, layers: &mut Vec<(i64, ui_batch::Table)>, layer_key: &mut i64, index: usize, clip_rect: Rect, parent_hit: i64) {
        if self.boxes[index].parent == 0 && self.foreground.iter().any(|(_, root)| *root == index) {
            layers.push((*layer_key, paint.builder.publish().expect("Ui.frame: publish")));
            paint.builder.reset();
            *layer_key = self.boxes[index].key;
        }
        let screen_rect = self.screen(index);
        let slot = self.boxes[index].slot;
        let (x, y, w, h) = screen_rect;
        {
            let s = &mut self.slots[slot];
            s.rx = x;
            s.ry = y;
            s.rw = w;
            s.rh = h;
        }
        let hit = match &self.boxes[index].hit {
            None => screen_rect,
            Some(f) => f(screen_rect),
        };
        let clipped_hit = intersect(hit, clip_rect);
        let (_, _, hw, hh) = clipped_hit;
        let visible = match &self.boxes[index].hit {
            None => hw > 0.0 && hh > 0.0,
            Some(_) => {
                let (_, _, vw, vh) = intersect(screen_rect, clip_rect);
                vw > 0.0 && vh > 0.0
            }
        };
        let flags = self.boxes[index].flags;
        let has_hit = flags & HIT_FLAGS != 0 && hw > 0.0 && hh > 0.0;
        if has_hit {
            self.record_hit(index, parent_hit, clipped_hit);
        }
        // a box with no extent of its own (a `Fit` box whose children are all placed with `at`) is
        // not out of view: its children show unless it clips them
        let shown = visible;
        if shown || ((w <= 0.0 || h <= 0.0) && flags & CLIP == 0) {
            if shown {
                let painters = self.boxes[index].painters.clone();
                self.run_painters(&painters, index, clip_rect, paint);
            }
            let child_clip = if flags & CLIP != 0 {
                let padding = self.boxes[index].padding;
                intersect(clip_rect, (x + padding, y + padding, 0.0f64.max(w - 2.0 * padding), 0.0f64.max(h - 2.0 * padding)))
            } else {
                clip_rect
            };
            let parent_hit = if has_hit { self.boxes[index].key } else { parent_hit };
            let mut child = self.boxes[index].first;
            while child != NONE {
                self.visit(paint, layers, layer_key, child, child_clip, parent_hit);
                child = self.boxes[child].next;
            }
            if shown {
                let overlays = self.boxes[index].overlays.clone();
                self.run_painters(&overlays, index, clip_rect, paint);
            }
            if shown && self.keyboard_focus && self.focus == self.boxes[index].key && flags & FOCUS_MARK == 0 {
                paint.clip_rect = clip_rect;
                let accent = self.theme.accent;
                paint.frame(self.l_x[index], self.l_y[index], 0.0f64.max(self.l_w[index]), 0.0f64.max(self.l_h[index]), accent);
            }
        } else {
            // Culled: keep retained rectangles current for `rect` queries.
            self.retain_rects(index);
        }
    }

    fn retain_rects(&mut self, index: usize) {
        let mut child = self.boxes[index].first;
        while child != NONE {
            let (x, y, w, h) = self.screen(child);
            let slot = self.boxes[child].slot;
            let s = &mut self.slots[slot];
            s.rx = x;
            s.ry = y;
            s.rw = w;
            s.rh = h;
            self.retain_rects(child);
            child = self.boxes[child].next;
        }
    }

    fn paint_all(&mut self, frame: &rframe::t) -> Vec<(i64, ui_batch::Table)> {
        let mut builder = self.batch_builder.take().expect("Ui: painting re-entered");
        builder.reset();
        self.regions.clear();
        self.hit_count = 0;
        self.chain_key = 0;
        self.fonts.density = self.density;
        let clip = (0.0, 0.0, frame.input.size.0 as f64, frame.input.size.1 as f64);
        let mut paint = Paint {
            builder,
            clip_rect: clip,
            theme: self.theme,
            font_size: self.font_size,
            row_height: self.kit_row_height,
            fonts: std::mem::take(&mut self.fonts),
            borrowed_images: std::mem::take(&mut self.borrowed_images),
            regions: Vec::new(),
            edit_scroll_x: self.edit_scroll_x,
            l_content: std::mem::take(&mut self.l_content),
            scroll_y: std::mem::take(&mut self.scroll_y),
        };
        let mut layers: Vec<(i64, ui_batch::Table)> = Vec::new();
        let mut key = 0i64;
        self.visit(&mut paint, &mut layers, &mut key, 0, clip, 0);
        layers.push((key, paint.builder.publish().expect("Ui.frame: publish")));
        paint.builder.reset();
        self.batch_builder = Some(paint.builder);
        self.fonts = paint.fonts;
        self.borrowed_images = paint.borrowed_images;
        self.regions = paint.regions;
        self.edit_scroll_x = paint.edit_scroll_x;
        self.l_content = paint.l_content;
        self.scroll_y = paint.scroll_y;
        layers
    }

    // -------------------------------------------------------------- frame

    /// The payload in flight follows the pointer as a small label above everything. Idle frames
    /// (nothing carried) build and paint nothing.
    fn carry_ghost(&mut self) {
        let (payload, px, py) = match &self.payload {
            Some((payload, _)) if self.pointer.0.is_finite() => (payload.clone(), self.pointer.0, self.pointer.1),
            _ => return,
        };
        let width = self.text_width(None, &payload.value) + 12.0;
        let saved = self.enter_root();
        // kit overlays [07]: a white 20-point chip with the payload in body-size ink, and under it
        // the sheet's tip with the key that drops it
        let ghost = self.box_("ui-carry-ghost", BoxOpts::default().w(Size::Px(width)).h(Size::Px(20.0)).at((px + 14.0, py + 14.0)));
        let theme = self.theme;
        let size = self.font_size;
        let small = 8.max(size - 2);
        let tip_text = "drop to put";
        let tip_key = "esc";
        let tw1 = self.text_width(Some(small), tip_text);
        let tw2 = self.text_width(Some(small), tip_key);
        let tip_w = 2.0 + 12.0 + tw1 + 6.0 + tw2;
        let tip = self.box_("ui-carry-tip", BoxOpts::default().w(Size::Px(tip_w)).h(Size::Px(20.0)).at((px + 14.0, py + 14.0 + 20.0 + 8.0)));
        let value = payload.value.clone();
        self.draw_over(
            &ghost,
            Box::new(move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                paint.text_s((x + 6.0, y + ((20 - size) as f64 / 2.0).floor()), size, theme.foreground, &value);
            }),
        );
        self.draw_over(
            &tip,
            Box::new(move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                paint.stroke(x + 0.5, y + 0.5, w - 1.0, h - 1.0, theme::edge(&theme));
                let ty = y + ((20 - small) as f64 / 2.0).floor();
                paint.text_s((x + 7.0, ty), small, theme.foreground, tip_text);
                paint.text_s((x + 7.0 + tw1 + 6.0, ty), small, theme::ink_3(&theme), tip_key);
            }),
        );
        // `(max_int, tip) :: (max_int, ghost) :: foreground` in OCaml's newest-first list
        self.foreground.push((i64::MAX, tip.index));
        self.foreground.push((i64::MAX, ghost.index));
        self.leave(saved);
    }

    fn clear_frame_callbacks(&mut self) {
        for index in 0..self.count {
            let b = &mut self.boxes[index];
            b.w = Size::Grow;
            b.h = Size::Grow;
            b.hit = None;
            b.painters.clear();
            b.overlays.clear();
        }
    }

    /// Route the frame's ordered events, run the builder, lay out, and paint. Tab/Shift-Tab
    /// traverse visible focusable boxes in presentation order, restricted to an open popup.
    /// Enter/Space activate controls.
    pub fn frame(&mut self, frame: &rframe::t, f: &mut dyn FnMut(&mut Ui)) {
        self.begin_frame(frame);
        f(self);
        self.end_frame(frame);
    }

    pub fn begin_frame(&mut self, frame: &rframe::t) {
        if self.destroyed {
            panic!("Ui.frame: the UI was destroyed");
        }
        if self.building {
            panic!("Ui.frame: frames cannot nest");
        }
        if !frame.input.t.is_finite() {
            panic!("Ui.frame: time must be finite");
        }
        let (scale_x, scale_y) = frame.pixel_scale;
        self.density = 1.max(ocaml_round(scale_x.max(scale_y)) as i64);
        self.fonts.density = self.density;
        self.frame_number += 1;
        // the window's title bar shows the ground the UI is painted on; no window, no effect
        if self.window_ground != Some(self.theme.panel) {
            self.window_ground = Some(self.theme.panel);
            let _ = sketch::set_window_background(self.theme.panel);
        }
        self.route(frame);
        self.modal_key = None;
        self.modal_extra.clear();
        // Also release callbacks left by a build that raised before publishing.
        self.clear_frame_callbacks();
        self.count = 0;
        self.closed_section = NONE;
        self.building = true;
        self.parents.clear();
        self.seeds.clear();
        let root = self.append_box(ROOT_KEY, NONE);
        self.set_box(
            root,
            NO_FLAGS,
            Size::Px(frame.input.size.0 as f64),
            Size::Px(frame.input.size.1 as f64),
            f64::INFINITY,
            false,
            0.0,
            0.0,
            0.0,
            0.0,
            None,
        );
        self.parents = vec![root];
        self.seeds = vec![ROOT_KEY];
    }

    pub fn end_frame(&mut self, frame: &rframe::t) {
        self.carry_ghost();
        self.building = false;
        self.parents.clear();
        self.seeds.clear();
        // Move popups to the end of the root's children, oldest first.
        let mut ordered = self.foreground.clone();
        ordered.sort_by(|a, b| a.0.cmp(&b.0));
        let mut moves: Vec<usize> = ordered.iter().map(|(_, i)| *i).collect();
        moves.extend(self.overlays.iter().copied());
        for index in moves {
            if self.boxes[index].parent == 0 && self.boxes[0].last != index {
                let mut previous = NONE;
                let mut child = self.boxes[0].first;
                while child != NONE {
                    if child == index {
                        if previous == NONE {
                            self.boxes[0].first = self.boxes[index].next;
                        } else {
                            self.boxes[previous].next = self.boxes[index].next;
                        }
                        break;
                    }
                    previous = child;
                    child = self.boxes[child].next;
                }
                let last = self.boxes[0].last;
                self.boxes[last].next = index;
                self.boxes[index].next = NONE;
                self.boxes[0].last = index;
            }
        }
        self.overlays.clear();
        self.intrinsic();
        self.arrange(frame.input.t);
        self.borrowed_images.clear();
        let layers = self.paint_all(frame);
        self.foreground.clear();
        self.prune();
        if self.focus != 0 && self.table.find(self.focus).is_none() {
            self.focus = 0;
            self.edit_focus = 0;
        }
        self.fonts.atlas.publish();
        let mut scene_layers = Vec::new();
        for (key, batch) in layers {
            let textures = batch.textures();
            let mut images: Vec<(i64, image::t)> = Vec::new();
            if let Some(atlas_image) = &self.fonts.atlas.image {
                if !textures.is_empty() {
                    images.push((1, atlas_image.clone()));
                }
            }
            for (id, img) in &self.borrowed_images {
                if textures.contains(id) {
                    images.push((*id, img.clone()));
                }
            }
            let batch = if images.is_empty() && !textures.is_empty() { ui_batch::empty() } else { batch };
            let layer = if batch.count() == 0 {
                Vec::new()
            } else {
                vec![scene::Layer::ui(Some(images), batch).expect("Ui.frame: layer")]
            };
            scene_layers.push((key, layer));
        }
        self.scene_layers = scene_layers;
        let mut all: Vec<scene::node> = Vec::new();
        for (_, layer) in &self.scene_layers {
            all.extend(layer.iter().cloned());
        }
        all.extend(self.regions.iter().cloned());
        self.scene = all;
        // Layout, hit rectangles and painting are published data now. The callbacks can capture
        // whole document versions, so keep them only while building.
        self.clear_frame_callbacks();
    }

    // ------------------------------------------------------ layout helpers

    pub fn hover_delay(&mut self, key: &str) -> bool {
        let time = match &self.input_frame {
            Some(frame) => frame.input.t,
            None => 0.0,
        };
        if self.active.is_some() || self.modal_in_frame || self.frame_events.iter().any(|e| matches!(e, Event::Focus_lost)) {
            self.hover_rest = None;
            return false;
        }
        match &self.hover_rest {
            Some((previous, pointer, start, seen))
                if previous.as_str() == key
                    && *pointer == self.pointer
                    && (*seen == self.frame_number || *seen == self.frame_number - 1)
                    && time >= *start =>
            {
                let start = *start;
                self.hover_rest = Some((key.to_owned(), self.pointer, start, self.frame_number));
                time - start >= HOVER_SECONDS
            }
            _ => {
                self.hover_rest = Some((key.to_owned(), self.pointer, time, self.frame_number));
                false
            }
        }
    }

    /// A tooltip: a 20-point sheet with a line-2 edge and 6 points of padding, text at the label
    /// size and long text wraps to further 14-point lines.
    pub fn tooltip(&mut self, key: &str, text: &str) {
        if !self.hover_delay(key) {
            return;
        }
        let size = 8.max(self.font_size - 2);
        let char_width = 1.0f64.max(self.text_width(Some(size), "0"));
        let width = 400.0f64.min(40.0f64.max(self.view_w - 16.0));
        let columns = 1.max(((width - 14.0) / char_width) as i64) as usize;
        let mut lines: Vec<String> = Vec::new();
        let mut line = String::new();
        for word in text.split(' ') {
            if line.is_empty() {
                line = word.to_owned();
            } else if line.len() + word.len() + 1 <= columns {
                line = format!("{} {}", line, word);
            } else {
                lines.push(std::mem::replace(&mut line, word.to_owned()));
            }
        }
        lines.push(line);
        let mut widest: f64 = 0.0;
        for l in &lines {
            widest = widest.max(self.text_width(Some(size), l));
        }
        let width = width.min(14.0 + widest);
        let height = 20.0 + 14.0 * (lines.len() as f64 - 1.0);
        let (px, py) = self.pointer;
        let x = 8.0f64.max((px + 12.0).min(self.view_w - width - 8.0));
        let y = if py + height + 24.0 <= self.view_h { py + 20.0 } else { 8.0f64.max(py - height - 8.0) };
        let saved = self.enter_root();
        let tip = self.box_("ui-tooltip", BoxOpts::default().w(Size::Px(width)).h(Size::Px(height)).at((x, y)));
        self.overlays.push(tip.index);
        let theme = self.theme;
        self.draw(
            &tip,
            Box::new(move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                paint.frame(x, y, w, h, theme::edge(&theme));
                for (i, line) in lines.iter().enumerate() {
                    paint.text_s(
                        (x + 7.0, y + 14.0 * i as f64 + ((20 - size) as f64 / 2.0).floor()),
                        size,
                        theme.foreground,
                        line,
                    );
                }
            }),
        );
        self.leave(saved);
    }

    pub fn col(&mut self, padding: f64, label: &str, f: &mut dyn FnMut(&mut Ui)) {
        let b = self.box_(label, BoxOpts::default().axis(Axis::Column).padding(padding));
        self.within(&b, f);
    }

    pub(crate) fn input_frame_time(&self) -> Option<f64> {
        self.input_frame.as_ref().map(|f| f.input.t)
    }
}

// ---- crate-private access for the kit and text widgets

impl Ui {
    pub(crate) fn kit_window_flag(&self) -> bool {
        self.kit_window
    }

    pub(crate) fn set_kit_window(&mut self, window: bool) {
        self.kit_window = window;
    }

    pub(crate) fn set_kit_row_height(&mut self, height: i64) {
        self.kit_row_height = height;
    }

    pub(crate) fn closed_section_index(&self) -> usize {
        self.closed_section
    }

    pub(crate) fn set_closed_section(&mut self, index: usize) {
        self.closed_section = index;
    }

    pub(crate) fn set_modal_in_frame(&mut self, value: bool) {
        self.modal_in_frame = value;
    }

    pub(crate) fn set_modal_key(&mut self, key: Option<i64>) {
        self.modal_key = key;
    }

    pub(crate) fn push_modal_extra(&mut self, key: i64) {
        self.modal_extra.insert(0, key);
    }

    /// Popups live at the root: the parents are the root alone, the seeds stay.
    pub(crate) fn enter_root_parent(&mut self) -> Saved {
        Saved { parents: std::mem::replace(&mut self.parents, vec![0]), seeds: self.seeds.clone() }
    }

    fn belongs(&mut self, target: i64, key: i64, related: &[i64]) -> bool {
        self.hit_within(target, key) || related.iter().any(|r| self.hit_within(target, *r))
    }

    /// A popup keeps only the signals, capture and focus that belong to it.
    pub(crate) fn retain_popup_state(&mut self, key: i64, related: &[i64]) {
        let targets: Vec<i64> = self.signals.keys().copied().collect();
        for target in targets {
            if !self.belongs(target, key, related) {
                self.signals.remove(&target);
            }
        }
        if let Some(active) = self.active {
            if !self.belongs(active, key, related) {
                self.cancelled.insert(0, self.active_button);
                self.active = None;
            }
        }
        if self.focus != 0 && !self.belongs(self.focus, key, related) {
            self.unfocus();
        }
    }

    pub(crate) fn remember_modal_height(&mut self, key: i64, slot: usize) {
        if !self.modal_heights.contains_key(&key) && self.modal_heights.len() >= 32 {
            // ponytail: scan at most 32 heights on insertion; use LRU if this small working set
            // ever needs a larger capacity.
            let oldest = self.modal_heights.iter().min_by_key(|(_, (_, seen))| *seen).map(|(k, _)| *k);
            if let Some(k) = oldest {
                self.modal_heights.remove(&k);
            }
        }
        let height = self.slots[slot].rh;
        self.modal_heights.insert(key, (height, self.frame_number));
    }

    pub(crate) fn modal_height(&self, key: i64) -> f64 {
        self.modal_heights.get(&key).map(|(h, _)| *h).unwrap_or(0.0)
    }

    pub(crate) fn hit_within_pub(&mut self, key: i64, ancestor: i64) -> bool {
        self.hit_within(key, ancestor)
    }

    pub(crate) fn slot_rw(&self, slot: usize) -> f64 {
        self.slots[slot].rw
    }

    pub(crate) fn set_flags_of_box(&mut self, b: &UiBox, flags: Flags) {
        self.boxes[b.index].flags = flags;
    }

    pub(crate) fn scrub_origin_get(&self) -> Option<(i64, String)> {
        self.scrub_origin.clone()
    }
}
