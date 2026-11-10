//! Each frame builds keyed boxes with flags and per-axis sizing. Per-key state lives in Ui's retained cache and expires when omitted for a frame. Input uses prior-frame rectangles; layout follows construction and painting packs instance lists. Painters own their Paint borrow for the paint duration.
use crate::theme::{self, Theme};
pub use crate::geom::Rect;
pub(crate) use crate::geom::p32;
use input::{Event, Key, MouseButton, Platform, ScrollPhase};
use crate::geom::hash;
use input::frame as rframe;
use crate::geom::{color, Rgba as Color};
use crate::draw::{Break, Instance};
use std::any::Any;
use pxui_text::{Metrics, Style, Text};
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

pub type Flags = i64;
/// Sentinel for no box in parent, child and sibling indices.
pub const NONE: usize = usize::MAX;

// ---------------------------------------------------------------- keys

const ROOT_KEY: i64 = 0x2c1b3c6d;

/// Keys 0 and 1 are the open-addressing empty/tombstone marks.
fn hash_range(seed: i64, text: &str, first: usize, last: usize) -> i64 {
    let hash = hash::bytes(seed as u64, &text.as_bytes()[first..last]) as i64;
    if hash == 0 || hash == 1 { hash + 2 } else { hash }
}

fn find_marker(text: &str, marker: &str) -> Option<usize> {
    if text.contains('#') { text.find(marker) } else { None }
}

thread_local! { static LABEL: std::cell::Cell<String> = const { std::cell::Cell::new(String::new()) }; }

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
    shown(text).to_owned()
}

pub(crate) fn shown(text: &str) -> &str {
    match find_marker(text, "##") {
        Some(index) => &text[..index],
        None => text,
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
    Px(f32),
    /// fraction of the parent's inner size
    Pct(f32),
    /// function of the parent's inner size
    Rel(Rc<dyn Fn(f32) -> f32>),
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
    FromPointer(i64),
    FromKey,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Drop {
    /// the pointer is over the box while a payload is held
    Hover(Payload),
    /// the payload was released over the box, this frame only
    Dropped(Payload),
}

/// Points a pressed pointer travels before a press is a drag: a carry starts, a click ends.
pub const DRAG_THRESHOLD: f32 = 4.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cursor {
    HorizontalResize,
    VerticalResize,
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

// Keep a box's allocation between frames, but release its captured document and resources as
// soon as painting finishes. A different closure type replaces this one row safely.
struct PaintRow {
    value: Box<dyn Any>,
    paint: fn(&dyn Any, &mut Paint, Rect),
    clear: fn(&mut dyn Any),
}

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
    max_h: f32,
    row: bool,
    padding: f32,
    at_x: f32,
    at_y: f32,
    scroll_step: f32,
    hit: Option<HitFn>,
    hit_rect: Option<Rect>,
    painters: Vec<PaintRow>,
    overlays: Vec<PaintRow>,
    painter_count: usize,
    overlay_count: usize,
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
            max_h: f32::INFINITY,
            row: false,
            padding: 0.0,
            at_x: f32::NAN,
            at_y: f32::NAN,
            scroll_step: 0.0,
            hit: None,
            hit_rect: None,
            painters: Vec::new(),
            overlays: Vec::new(),
            painter_count: 0,
            overlay_count: 0,
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
    rx: f32,
    ry: f32,
    rw: f32,
    rh: f32,
    scroll_raw: f64,
    scroll_visual: f64,
    scroll_event_time: f64,
    scroll_frame_time: f64,
    /// a trackpad's coast or its bounce off an edge: 0 neither, 1 coast, 2 bounce
    scroll_mode: i64,
    scroll_velocity: f64,
    scroll_start: f64,
    scroll_from: f64,
    state_value: Option<i64>,
    text_value: Option<String>,
    labels: Vec<Rc<str>>,
    label_count: usize,
    /// a hit shape a kit widget keeps between frames, by its parameters
    hit: Option<(u64, HitFn)>,
    press_time: f64,
    press_x: f32,
    press_y: f32,
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
            state_value: None,
            text_value: None,
            labels: Vec::new(),
            label_count: 0,
            hit: None,
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
    x: f32,
    y: f32,
    w: f32,
    h: f32,
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
    pub drag_x: f32,
    pub drag_y: f32,
    pub press_point: (f32, f32),
    pub release_point: (f32, f32),
    pub button: Option<MouseButton>,
    pub scroll_x: f32,
    pub scroll_y_steps: f32,
    /// what moves a scroll box: an unphased wheel's steps, the fingers' points on a trackpad, and
    /// whether they touched or lifted this frame
    pub scroll_wheel: f32,
    pub scroll_drag: f32,
    pub scroll_touch: bool,
    pub scroll_lift: bool,
    /// product of this frame's pinch factors
    pub pinch: f64,
    pub keys: Vec<(Event, Vec<Key>)>,
    pub press_keys: Vec<Key>,
}

/// A host texture with its size in pixels; PXUI only binds it to the instances that sample it.
#[derive(Clone)]
pub struct Image {
    pub handle: Rc<dyn Any>,
    pub width: i64,
    pub height: i64,
}

/// One paint layer: the base UI, then a layer per floating root (`key` is its box key; 0 for the
/// base). Image instances sample `images[break.image]` from each break on. The vectors are cleared
/// and refilled every frame, never dropped.
pub struct Layer {
    pub key: i64,
    pub instances: Vec<Instance>,
    pub breaks: Vec<Break>,
    pub images: Vec<Image>,
}

/// What the frame asks of the window system; the host applies it after the frame.
#[derive(Default)]
pub struct PlatformOut {
    /// the pointer shape a hovered or captured control asked for
    pub cursor: Option<Cursor>,
    /// the focused text's input area and the caret's offset in it, for the input method
    pub ime: Option<((i64, i64, i64, i64), i64)>,
    /// the ground the UI is painted on, on the frame it changed (the macOS title bar)
    pub background: Option<Color>,
    /// text to put on the clipboard
    pub clipboard: Option<String>,
    /// the UI wants the clipboard's text: the host sends `Event::Paste` with the next frame
    pub paste: bool,
    /// when the UI needs its next frame with no input (`Ui::repaint_at`)
    pub repaint_at: Option<f64>,
}

/// The completed frame: its paint layers and the platform requests.
pub struct FrameOut<'a> {
    pub draw: &'a [Layer],
    pub platform: &'a PlatformOut,
}

// -------------------------------------------------------------- text

/// The layout key of `text` in `style` at `width`: equal text in an equal style shares one cached
/// layout, so an unchanged label is a cache hit wherever it is drawn or measured.
fn text_key(style: &Style, width: f32, text: &str) -> u64 {
    let mut seed = hash::mix(style.font as i64, style.size.to_bits() as i64);
    for v in [style.weight as i64, style.italic as i64, style.line_height.to_bits() as i64, style.wrap as i64, style.align as i64, style.max_lines as i64, style.ellipsis as i64, style.tracking.to_bits() as i64, width.to_bits() as i64] {
        seed = hash::mix(seed, v);
    }
    hash::bytes(seed as u64, text.as_bytes())
}

/// Kit text: the UI face (font 0) at `size` points with `tracking` points after every glyph.
pub fn kit_style(size: i64, tracking: f32) -> Style {
    Style { size: size as f32, tracking, ..Style::default() }
}

/// Lay `text` out (a cache hit when unchanged) and return its key and metrics.
pub(crate) fn lay_out(t: &mut Text, style: &Style, width: f32, text: &str) -> (u64, Metrics) {
    let key = text_key(style, width, text);
    (key, t.layout(key, text, style, width))
}

/// The x of the caret before byte `index` of single-line kit text, in points from its start.
fn caret_x(t: &mut Text, size: i64, text: &str, index: usize) -> f32 {
    let (key, _) = lay_out(t, &kit_style(size, 0.0), 0.0, text);
    t.caret(key, index.min(text.len())).x
}

// ----------------------------------------------------------------- Ui

pub struct Ui {
    theme: Theme,
    font_size: i64,
    platform: Platform,
    /// the text engine; `Paint` holds it while a frame paints
    text: Option<Text>,
    table: SlotTable,
    slots: Vec<Slot>,
    free: Vec<usize>,
    scroll_y: Vec<f32>,
    // per-frame boxes
    count: usize,
    boxes: Vec<BoxRec>,
    l_x: Vec<f32>,
    l_y: Vec<f32>,
    l_w: Vec<f32>,
    l_h: Vec<f32>,
    l_content: Vec<f32>,
    l_gutter: Vec<f32>,
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
    pointer: (f32, f32),
    hot: i64,
    hover_rest: Option<(String, (f32, f32), f64, i64)>,
    active: Option<i64>,
    active_button: MouseButton,
    active_press: (f32, f32),
    active_keys: Vec<Key>,
    focus: i64,
    keyboard_focus: bool,
    composition: String,
    previous_keys: Vec<Key>,
    pub(crate) edit_focus: i64,
    pub edit_value: String,
    pub(crate) edit_caret: usize,
    pub(crate) edit_anchor: usize,
    pub(crate) edit_scroll_x: f32,
    window_ground: Option<Color>,
    gesture_key: i64,
    gesture_time: f64,
    gesture_velocity: f64,
    pub edit_undo: Vec<(String, usize, usize)>,
    pub(crate) text_undo_held: i64,
    /// What the host last charged to its memory ledger for this UI; PXUI never reads it.
    pub charged: Bytes,
    pub(crate) text_rows_seen: Vec<(Option<usize>, String, Rc<Vec<(usize, usize, usize)>>)>,
    pub(crate) text_colors_seen: Vec<(Theme, String, Rc<Vec<(i64, i64, Color)>>)>,
    pub(crate) edit_redo: Vec<(String, usize, usize)>,
    pub(crate) edit_group: i64,
    pub(crate) edit_shelf: Vec<ShelfEntry>,
    pub(crate) edit_unit: Option<(usize, usize, i64)>,
    pub(crate) edit_column: f32,
    pub(crate) scrub_origin: Option<(i64, String)>,
    payload: Option<(Payload, Origin)>,
    dropped: Option<(Payload, i64)>,
    carry_latch: Option<i64>,
    passed_undo: Option<UndoKind>,
    frame_events: Vec<Event>,
    input_frame: Option<rframe::Frame>,
    routed_events: Vec<(i64, Event, (f64, f64))>,
    cancelled: Vec<MouseButton>,
    modal_key: Option<i64>,
    modal_extra: Vec<i64>,
    modal_in_frame: bool,
    view_w: f32,
    view_h: f32,
    modal_heights: HashMap<i64, (f32, i64)>,
    signals: HashMap<i64, Accumulator>,
    hits: Vec<HitRec>,
    hit_count: usize,
    chain_key: i64,
    chain: Vec<i64>,
    label: String,
    /// the frame's layers are `draw[..draw_len]`; the rest keep their storage for later frames
    draw: Vec<Layer>,
    draw_len: usize,
    /// the frame before's layers, painted over by the next frame
    spare: Vec<Layer>,
    draw_revision: i64,
    painting: bool,
    frame_requested: bool,
    out: PlatformOut,
    destroyed: bool,
}

/// Bytes a UI holds: the glyph atlases' CPU mirrors and the text undo stacks and shelf.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Bytes {
    pub atlas: i64,
    pub text_undo: i64,
}

/// One text left behind: its key, value and its undo and redo stacks.
pub type ShelfEntry = (i64, String, Vec<(String, usize, usize)>, Vec<(String, usize, usize)>);



pub fn contains(rect: Rect, point: (f32, f32)) -> bool { rect.contains(point) }

pub fn intersect(a: Rect, b: Rect) -> Rect { a.intersect(b) }

fn rgba(c: Color) -> [u8; 4] {
    [c.r as u8, c.g as u8, c.b as u8, c.a as u8]
}

/// Integer-derived child keys: no label string per frame.
pub fn int_key(seed: i64, n: i64) -> i64 {
    let hash = hash::mix(seed, n);
    if hash == 0 || hash == 1 { hash + 2 } else { hash }
}

impl Ui {
    /// `platform` is the host's modifier conventions: the text keys and every Command check follow
    /// it. `text` holds the fonts the host loaded: font 0 is the kit face (`pxui_text::PRAGMASEVKA`
    /// in the default theme); `font_size` is the logical size of kit text.
    pub fn create(platform: Platform, font_size: Option<i64>, text: Text) -> Ui {
        let font_size = font_size.unwrap_or(theme::FONT_SIZE);
        if font_size <= 0 {
            panic!("Ui.create: font_size must be positive");
        }
        Ui {
            theme: theme::default(),
            font_size,
            platform,
            text: Some(text),
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
            pointer: (f32::NAN, f32::NAN),
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
            text_undo_held: 0,
            charged: Bytes::default(),
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
            label: String::new(),
            draw: Vec::new(),
            draw_len: 0,
            spare: Vec::new(),
            draw_revision: 0,
            painting: false,
            frame_requested: false,
            out: PlatformOut::default(),
            destroyed: false,
        }
    }

    pub fn platform(&self) -> Platform {
        self.platform
    }

    /// Release what this UI holds. The handle must not be used afterwards.
    pub fn destroy(&mut self) {
        if !self.destroyed {
            self.destroyed = true;
            self.draw.clear();
            self.spare.clear();
            self.draw_len = 0;
            self.boxes.clear();
            for slot in &mut self.slots { slot.labels.clear(); }
            self.count = 0;
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
        self.kit_row_height = 24.max(size + 11);
    }

    /// The most recently completed frame.
    pub fn output(&self) -> FrameOut<'_> {
        FrameOut { draw: &self.draw[..self.draw_len], platform: &self.out }
    }

    /// Ask the host for the clipboard's text; it arrives as `Event::Paste` with the next frame.
    pub(crate) fn request_paste(&mut self) {
        self.out.paste = true;
    }

    pub(crate) fn copy(&mut self, text: &str) {
        self.out.clipboard = Some(text.to_owned());
    }

    /// Changes exactly when a frame paints layers different from the frame before's.
    pub fn draw_revision(&self) -> i64 {
        self.draw_revision
    }

    /// The text engine: the host loads fonts into it and feeds its painter from `drain_uploads`
    /// (the glyph atlases the frame's glyph instances sample).
    pub fn text_mut(&mut self) -> &mut Text {
        self.text.as_mut().expect("Ui: text is out while painting")
    }

    /// How many keys the retained cache holds (live slots).
    pub fn retained_slots(&self) -> usize {
        self.slots.iter().filter(|s| s.key != 0).count()
    }

    pub fn row_height(&self) -> i64 {
        self.kit_row_height
    }

    pub fn view_size(&self) -> (f32, f32) {
        (self.view_w, self.view_h)
    }

    pub fn text_line_height(&self) -> i64 {
        self.font_size + (self.font_size + 1) / 2
    }

    /// The heap this UI holds for the host to account for; zero once destroyed.
    pub fn bytes(&self) -> Bytes {
        if self.destroyed {
            Bytes::default()
        } else {
            Bytes { atlas: self.text.as_ref().map_or(0, |t| t.bytes()) as i64, text_undo: self.text_undo_held }
        }
    }

    /// The width of kit text in points, for layout during the build (`Paint::text_width` while
    /// painting).
    pub fn text_width(&mut self, size: Option<i64>, text: &str) -> f32 {
        let size = size.unwrap_or(self.font_size);
        lay_out(self.text_mut(), &kit_style(size, 0.0), 0.0, text).1.w
    }

    /// Lay `text` out in any style (wrap, ellipsis, line limit, alignment, font) within `width`
    /// points (0: unbounded), as `Paint::text_styled` draws it.
    pub fn text_metrics(&mut self, style: &Style, width: f32, text: &str) -> Metrics {
        lay_out(self.text_mut(), style, width, text).1
    }

    /// The x of the caret before byte `index` of kit text, in points from the text's start.
    pub fn caret_x(&mut self, size: Option<i64>, text: &str, index: usize) -> f32 {
        let size = size.unwrap_or(self.font_size);
        caret_x(self.text_mut(), size, text, index)
    }

    /// Measure the same ASCII-uppercase label style as `Paint::cap`, reusing the frame's text buffer.
    pub fn cap_width(&mut self, text: &str) -> f32 {
        let mut buffer = std::mem::take(&mut self.label);
        buffer.clear();
        buffer.extend(text.chars().map(|c| c.to_ascii_uppercase()));
        let size = 8.max(self.font_size - 2);
        let width = lay_out(self.text_mut(), &kit_style(size, 0.08 * size as f32), 0.0, &buffer).1.w;
        self.label = buffer;
        width
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
            let label_count = self.slots[slot].label_count;
            self.slots[slot].labels.truncate(label_count);
            let key = self.slots[slot].key;
            if key != 0 && self.slots[slot].touched < self.frame_number {
                self.table.remove(key);
                self.slots[slot].key = 0;
                self.slots[slot].text_value = None;
                self.slots[slot].labels.clear();
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
                if let Some((_, Origin::FromPointer(owner))) = &self.payload {
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

    fn hit_contains(&self, index: usize, point: (f32, f32)) -> bool {
        let h = &self.hits[index];
        contains(Rect::new(h.x, h.y, h.w, h.h), point)
    }

    /// Topmost hit entry under `point`; 0 when none.
    fn topmost(&self, point: (f32, f32)) -> i64 {
        for index in (0..self.hit_count).rev() {
            if self.hit_contains(index, point) {
                return self.hits[index].key;
            }
        }
        0
    }

    /// The root key whose box or child is topmost under the point in PXUI's previous hit tree.
    pub fn root_at(&self, point: (f32, f32), roots: &[i64]) -> Option<i64> {
        let mut found = None;
        self.hit_ancestors(self.topmost(point), &mut |key| {
            if found.is_none() && roots.contains(&key) {
                found = Some(key);
            }
        });
        found
    }

    /// Last root key pressed in `frame`, using PXUI's previous hit tree.
    pub fn last_press_within(&self, frame: &rframe::Frame, roots: &[i64]) -> Option<i64> {
        let mut last = None;
        for event in &frame.input.events {
            if let Event::PointerPressed(_, point) = event {
                if let Some(root) = self.root_at(p32(*point), roots) {
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
    fn scroll_target(&mut self, point: (f32, f32)) -> i64 {
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

    fn pointer_target(&mut self, point: (f32, f32)) -> i64 {
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

    fn route(&mut self, frame: &rframe::Frame) {
        self.signals.clear();
        self.out.cursor = None;
        self.frame_events = frame.input.events.clone();
        self.input_frame = Some(frame.clone());
        self.routed_events.clear();
        self.cancelled.clear();
        self.dropped = None;
        // a carry lives with the press that holds it: capture taken away since (a popup, a host
        // dismissal) takes the payload too, so no later release puts it
        if let Some((_, Origin::FromPointer(owner))) = &self.payload {
            if self.active != Some(*owner) {
                self.payload = None;
            }
        }
        let mut released_carry = false;
        self.passed_undo = None;
        self.modal_in_frame = self.modal_key.is_some();
        let mut modifiers = input::keys_before(&self.previous_keys, &frame.input.keys, &frame.input.events);
        self.previous_keys = frame.input.keys.clone();
        self.view_w = frame.input.size.0 as f32;
        self.view_h = frame.input.size.1 as f32;
        // a trackpad's motion also arrives as wheel steps: in such a frame those only feed
        // `signal.scroll`, and the gesture moves the scroll box
        let phased = frame.input.events.iter().any(|e| match e {
            Event::TrackpadScrolled { delta: (dx, dy), .. } => *dx != 0.0 || *dy != 0.0,
            _ => false,
        });
        for (event_index, event) in frame.input.events.iter().enumerate() {
            modifiers = input::keys_after(&modifiers, event);
            let command = self.command_modifiers(&modifiers);
            if let Event::KeyPressed(Key::Tab) = event {
                if !command && self.flags_of_key(self.focus) & KEEP_TAB == 0 {
                    self.traverse_focus(modifiers.contains(&Key::Shift));
                }
            }
            let (owner, delta): (i64, (f64, f64)) = match event {
                Event::PointerMoved(point) => {
                    let owner = self.pointer_target(p32(*point));
                    let delta = if self.pointer.0.is_finite() {
                        (point.0 - self.pointer.0 as f64, point.1 - self.pointer.1 as f64)
                    } else {
                        (0.0, 0.0)
                    };
                    (owner, delta)
                }
                Event::PointerPressed(_, point) | Event::PointerReleased(_, point) => {
                    (self.pointer_target(p32(*point)), (0.0, 0.0))
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
                Event::KeyPressed(Key::Tab) if !command => {
                    (if self.focus == 0 { ROOT_KEY } else { self.focus }, (0.0, 0.0))
                }
                Event::KeyPressed(_) | Event::KeyReleased(_) | Event::TextInput(_) | Event::TextEditing { .. } | Event::Paste(_) => {
                    (self.focus, (0.0, 0.0))
                }
                _ => (0, (0.0, 0.0)),
            };
            self.routed_events.push((owner, event.clone(), delta));
            match event {
                Event::PointerMoved(point) => {
                    let (px, py) = self.pointer;
                    self.pointer = p32(*point);
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
                Event::PointerPressed(MouseButton::LeftButton, point)
                    if matches!(self.payload, Some((_, Origin::FromKey))) =>
                {
                    self.pointer = p32(*point);
                    if let Some((payload, _)) = &self.payload {
                        self.dropped = Some((payload.clone(), self.topmost(p32(*point))));
                    }
                    self.payload = None;
                    if let Some(last) = self.routed_events.last_mut() {
                        last.0 = -1;
                    }
                }
                Event::PointerPressed(button, point) => {
                    self.pointer = p32(*point);
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
                Event::PointerReleased(button, point) => {
                    self.pointer = p32(*point);
                    if let Some(active) = self.active {
                        if *button == self.active_button {
                            if let Some((_, Origin::FromPointer(o))) = &self.payload {
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
                        value.scroll_x += *horizontal as f32;
                        value.scroll_y_steps += *vertical as f32;
                        if !phased {
                            value.scroll_wheel += *vertical as f32;
                        }
                    }
                }
                // The fingers on a trackpad: the gesture stays with the box it began over, and the
                // velocity it is released with is its last motion over the time since the one
                // before. The system's own momentum is not used: the box coasts by itself.
                Event::TrackpadScrolled { phase: ScrollPhase::Momentum, .. } => {}
                Event::TrackpadScrolled { phase: ScrollPhase::Lifted, time, .. } => {
                    // fingers that rested before they lifted leave the content where it is
                    if time - self.gesture_time > 0.1 {
                        self.gesture_velocity = 0.0;
                    }
                    self.lift();
                }
                Event::TrackpadScrolled { delta: (_, vertical), phase, time } => {
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
                        value.scroll_drag += *vertical as f32;
                    }
                }
                // a trackpad pinch goes where the wheel goes
                Event::Pinched(scale) => {
                    let scroll = self.scroll_target(self.pointer);
                    let target = self.modal_target(scroll);
                    if target != 0 {
                        self.accumulator(target).pinch *= *scale;
                    }
                }
                // Cancellation ends capture without a release: no click, no commit.
                Event::PointerCancelled(button) => {
                    if *button == self.active_button {
                        self.active = None;
                    }
                    if let Some((_, Origin::FromPointer(_))) = &self.payload {
                        if *button == MouseButton::LeftButton {
                            self.payload = None;
                            released_carry = false;
                        }
                    }
                }
                Event::FocusLost => {
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
                Event::KeyPressed(_) | Event::KeyReleased(_) | Event::TextInput(_) | Event::TextEditing { .. } | Event::Paste(_) => {
                    if self.focus != 0 {
                        let focus = self.focus;
                        let mods = modifiers.clone();
                        let value = self.accumulator(focus);
                        value.keys.push((event.clone(), mods));
                        if let Event::KeyPressed(Key::Enter) | Event::KeyPressed(Key::Space) | Event::KeyPressed(Key::KeyChar(' ')) = event {
                            if !command {
                                value.clicked = true;
                            }
                        }
                        match event {
                            Event::TextEditing { text, .. } => self.composition = text.clone(),
                            Event::TextInput(_) => self.composition.clear(),
                            Event::KeyPressed(Key::Escape) if self.flags_of_key(self.focus) & TAB_ONLY != 0 => {
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
        let (mouse_x, mouse_y) = p32(frame.input.pointer);
        if !self.pointer.0.is_finite() {
            self.pointer = (mouse_x, mouse_y);
        }
        self.hot = if frame.input.events.iter().any(|e| matches!(e, Event::FocusLost)) {
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

    /// Cursor requested by a hovered or captured PXUI control; the last request of a frame wins.
    pub fn request_cursor(&mut self, shape: Cursor) {
        self.out.cursor = Some(shape);
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
        self.frame_events.iter().any(|e| matches!(e, Event::KeyPressed(k) if *k == key))
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

    pub(crate) fn pointer(&self) -> (f32, f32) {
        self.pointer
    }

    pub(crate) fn frame_events(&self) -> &[Event] {
        &self.frame_events
    }

    pub(crate) fn input_frame(&self) -> Option<&rframe::Frame> {
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
    pub fn input(&self, owner: Option<i64>) -> rframe::Frame {
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
                Event::FocusLost | Event::PointerCancelled(_) => events.push(event.clone()),
                _ => {
                    let ok = match event {
                        Event::PointerPressed(..) | Event::PointerReleased(..) | Event::PointerMoved(_) | Event::Scrolled(_) => {
                            accepts_pointer(*target)
                        }
                        _ => accepts(*target),
                    };
                    if ok {
                        events.push(event.clone());
                        dx += mx;
                        dy += my;
                    } else if let Event::PointerReleased(button, _) = event {
                        events.push(Event::PointerCancelled(*button));
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
            events.push(Event::PointerCancelled(button));
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

    /// When the last frame's UI needs another frame with no input: at once for a gesture,
    /// focused text, a scroll in motion or a requested frame, at the end of a running hover
    /// delay; `None` when time alone changes nothing.
    pub fn repaint_at(&self) -> Option<f64> {
        if self.frame_requested { Some(f64::NEG_INFINITY) } else { self.out.repaint_at }
    }

    /// The frame gate: build, lay out and paint only for input or a due repaint. A host that gets
    /// `false` keeps the last frame and waits for input or `repaint_at`.
    pub fn wants_frame(&self, events: &[Event], now: f64) -> bool {
        !events.is_empty() || self.repaint_at().is_some_and(|at| now >= at)
    }

    /// Ask for a frame: between frames, the next one; while building, the one after this (an
    /// animation or timer the app drives calls it every frame it runs).
    pub fn request_frame(&mut self) {
        self.frame_requested = true;
    }

    fn next_repaint(&self, now: f64) -> Option<f64> {
        if self.frame_requested
            || self.active.is_some()
            || self.focus != 0
            || self.slots.iter().any(|s| s.scroll_mode != 0 || s.scroll_visual != 0.0)
        {
            return Some(now);
        }
        match &self.hover_rest {
            Some((_, _, start, seen)) if *seen == self.frame_number && now - start < HOVER_SECONDS => Some(start + HOVER_SECONDS),
            _ => None,
        }
    }
}

/// `label` cut to `limit` points wide with an ellipsis, on a character boundary. `width` measures
/// text as the sum of its characters, so one pass from the start measures each character once and
/// stops at the first that does not fit: the cost follows what is shown, not the label's length.
pub fn ellipsis<'a>(width: &mut dyn FnMut(&str) -> f32, limit: f32, label: &'a str) -> Cow<'a, str> {
    let dots = width("\u{2026}");
    // `cut`: the longest prefix that fits with the ellipsis after it; -1 when the ellipsis alone does
    // not
    let mut cut: i64 = if dots <= limit { 0 } else { -1 };
    let mut used = 0.0;
    for (index, c) in label.char_indices() {
        let next = index + c.len_utf8();
        used += width(&label[index..next]);
        if used > limit {
            return if cut < 0 { Cow::Borrowed("") } else { Cow::Owned(format!("{}\u{2026}", &label[..cut as usize])) };
        }
        if used + dots <= limit {
            cut = next as i64;
        }
    }
    Cow::Borrowed(label)
}

pub(crate) const HOVER_SECONDS: f64 = 0.380;

// ------------------------------------------------------------- boxes

#[derive(Clone)]
pub struct BoxOpts {
    pub flags: Flags,
    pub w: Size,
    pub h: Size,
    pub max_h: f32,
    pub axis: Axis,
    pub padding: f32,
    pub at: Option<(f32, f32)>,
    pub scroll_step: f32,
    pub hit: Option<HitFn>,
    pub hit_rect: Option<Rect>,
}

impl Default for BoxOpts {
    fn default() -> BoxOpts {
        BoxOpts {
            flags: NO_FLAGS,
            w: Size::Grow,
            h: Size::Fit,
            max_h: f32::INFINITY,
            axis: Axis::Column,
            padding: 0.0,
            at: None,
            scroll_step: 24.0,
            hit: None,
            hit_rect: None,
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
    pub fn max_h(mut self, max_h: f32) -> BoxOpts {
        self.max_h = max_h;
        self
    }
    pub fn axis(mut self, axis: Axis) -> BoxOpts {
        self.axis = axis;
        self
    }
    pub fn padding(mut self, padding: f32) -> BoxOpts {
        self.padding = padding;
        self
    }
    pub fn at(mut self, at: (f32, f32)) -> BoxOpts {
        self.at = Some(at);
        self
    }
    pub fn scroll_step(mut self, step: f32) -> BoxOpts {
        self.scroll_step = step;
        self
    }
    pub fn hit(mut self, hit: HitFn) -> BoxOpts {
        self.hit = Some(hit);
        self
    }
}

/// What `enter`, `scope` and `enter_root` change; `leave` puts it back. `enter` and `scope` only
/// push, so they keep the depths; the roots replace the stacks and keep them whole.
pub enum Saved {
    Depths(usize, usize),
    Stacks(Vec<usize>, Vec<i64>),
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
    pub drag: (f32, f32),
    /// latest pointer, screen space
    pub pointer: (f32, f32),
    /// where the current or last press began
    pub press_point: (f32, f32),
    pub release_point: (f32, f32),
    pub button: Option<MouseButton>,
    /// wheel steps routed to this box
    pub scroll: (f32, f32),
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
        self.slots[slot].label_count = 0;
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
    fn set_box(&mut self, index: usize, flags: Flags, w: Size, h: Size, max_h: f32, row: bool, padding: f32, at_x: f32, at_y: f32, scroll_step: f32, hit: Option<HitFn>) {
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
        b.hit_rect = None;
        b.painter_count = 0;
        b.overlay_count = 0;
    }

    pub fn box_keyed(&mut self, key: i64, opts: BoxOpts) -> UiBox {
        self.require_building();
        let key = self.unique_key(key);
        let parent = self.current_parent();
        let index = self.append_box(key, parent);
        let (at_x, at_y) = opts.at.unwrap_or((f32::NAN, f32::NAN));
        self.set_box(index, opts.flags, opts.w, opts.h, opts.max_h, opts.axis == Axis::Row, opts.padding, at_x, at_y, opts.scroll_step, opts.hit);
        self.boxes[index].hit_rect = opts.hit_rect;
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

    /// `box_` of a label formatted into one reused buffer: keys hash the text, none keeps it.
    pub fn box_fmt(&mut self, label: std::fmt::Arguments, opts: BoxOpts) -> UiBox {
        match label.as_str() {
            Some(text) => self.box_(text, opts),
            None => {
                let mut text = LABEL.take();
                text.clear();
                let _ = std::fmt::Write::write_fmt(&mut text, label);
                let b = self.box_(&text, opts);
                LABEL.set(text);
                b
            }
        }
    }

    pub fn set_at(&mut self, b: &UiBox, at: (f32, f32)) {
        self.require_building();
        self.boxes[b.index].at_x = at.0;
        self.boxes[b.index].at_y = at.1;
    }

    /// Start building children of `b`; give the result to `leave`.
    pub fn enter(&mut self, b: &UiBox) -> Saved {
        self.require_building();
        let saved = Saved::Depths(self.parents.len(), self.seeds.len());
        self.parents.push(b.index);
        self.seeds.push(b.box_key);
        saved
    }

    pub fn leave(&mut self, saved: Saved) {
        match saved {
            Saved::Depths(parents, seeds) => {
                self.parents.truncate(parents);
                self.seeds.truncate(seeds);
            }
            Saved::Stacks(parents, seeds) => {
                self.parents = parents;
                self.seeds = seeds;
            }
        }
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
        let saved = Saved::Depths(self.parents.len(), self.seeds.len());
        let seed = key_of(self.current_seed(), label);
        self.seeds.push(seed);
        f(self);
        self.leave(saved);
    }

    /// Build at the root under the root's own seed, wherever the caller is in the tree: what is
    /// built is laid out in the window, clipped by no pane.
    pub(crate) fn enter_root(&mut self) -> Saved {
        Saved::Stacks(std::mem::replace(&mut self.parents, vec![0]), std::mem::replace(&mut self.seeds, vec![ROOT_KEY]))
    }

    pub fn box_of_key(&self, key: i64) -> Option<UiBox> {
        self.table.find(key).map(|slot| UiBox { index: NONE, box_key: key, box_slot: slot })
    }

    /// The box's screen rectangle from the previous frame.
    pub fn rect(&self, b: &UiBox) -> Rect {
        let s = &self.slots[b.box_slot];
        Rect::new(s.rx, s.ry, s.rw, s.rh)
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
    pub fn context_at(&mut self, b: &UiBox) -> Option<(f32, f32)> {
        let targets: Vec<(i64, (f32, f32))> = self
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
                if let Some((held, Origin::FromKey)) = &self.payload {
                    if held.kind == kind && held.value == value {
                        return;
                    }
                }
                self.payload = Some((Payload { kind: kind.to_owned(), value: value.to_owned() }, Origin::FromKey));
            }
            Some(b) => {
                if self.payload.is_none()
                    && self.active == Some(b.box_key)
                    && self.carry_latch != self.active
                    && self.active_button == MouseButton::LeftButton
                {
                    let (px, py) = self.active_press;
                    let (x, y) = self.pointer;
                    if (x - px).hypot(y - py) >= DRAG_THRESHOLD {
                        self.payload = Some((
                            Payload { kind: kind.to_owned(), value: value.to_owned() },
                            Origin::FromPointer(b.box_key),
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

    pub fn scroll_offset(&self, b: &UiBox) -> f32 {
        self.scroll_y[b.box_slot]
    }

    /// Current painted scroll position, including elastic edge movement.
    pub fn scroll_position(&self, b: &UiBox) -> f32 {
        self.scroll_y[b.box_slot] + self.slots[b.box_slot].scroll_visual as f32
    }

    pub fn set_scroll_offset(&mut self, b: &UiBox, value: f32) {
        self.scroll_y[b.box_slot] = value;
        let s = &mut self.slots[b.box_slot];
        s.scroll_raw = 0.0;
        s.scroll_visual = 0.0;
        s.scroll_mode = 0;
    }

    /// Retained per-box scalar state for custom widgets.
    pub fn state(&self, b: &UiBox, default: i64) -> i64 {
        self.slots[b.box_slot].state_value.unwrap_or(default)
    }

    pub fn set_state(&mut self, b: &UiBox, value: i64) {
        self.slots[b.box_slot].state_value = Some(value);
    }

    /// Stable paint text follows its existing retained box key and is discarded with that slot.
    pub fn cached_label(&mut self, b: &UiBox, index: usize, text: &str) -> Rc<str> {
        let slot = &mut self.slots[b.box_slot];
        slot.label_count = slot.label_count.max(index + 1);
        if index >= slot.labels.len() {
            slot.labels.resize_with(index + 1, || Rc::from(""));
        }
        if slot.labels[index].as_ref() != text { slot.labels[index] = Rc::from(text); }
        slot.labels[index].clone()
    }

    /// Give `b` the hit shape `make` builds, kept in its slot while `key` (the shape's parameters)
    /// is unchanged: no closure per frame.
    pub(crate) fn cached_hit(&mut self, b: &UiBox, key: u64, make: impl FnOnce() -> HitFn) {
        let slot = &mut self.slots[b.box_slot];
        let hit = match &slot.hit {
            Some((k, hit)) if *k == key => hit.clone(),
            _ => {
                let hit = make();
                slot.hit = Some((key, hit.clone()));
                hit
            }
        };
        self.boxes[b.index].hit = Some(hit);
    }

    pub fn text_state(&self, b: &UiBox) -> Option<String> {
        self.slots[b.box_slot].text_value.clone()
    }

    pub fn set_text_state(&mut self, b: &UiBox, value: Option<String>) {
        self.slots[b.box_slot].text_value = value;
    }

    /// Paint with the box's final laid-out rectangle, before its children.
    pub fn draw<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, painter: F) {
        let b = &mut self.boxes[b.index];
        let position = b.painter_count;
        b.painter_count += 1;
        if let Some(value) = b.painters.get_mut(position).and_then(|row| row.value.downcast_mut::<Option<F>>()) {
            *value = Some(painter);
        } else {
            let row = PaintRow {
                value: Box::new(Some(painter)),
                paint: |value, paint, rect| value.downcast_ref::<Option<F>>().unwrap().as_ref().unwrap()(paint, rect),
                clear: |value| { *value.downcast_mut::<Option<F>>().unwrap() = None; },
            };
            if position == b.painters.len() { b.painters.push(row); } else { b.painters[position] = row; }
        }
    }

    /// Paint after the box's children, outside its content clip.
    pub fn draw_over<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, painter: F) {
        let b = &mut self.boxes[b.index];
        let position = b.overlay_count;
        b.overlay_count += 1;
        if let Some(value) = b.overlays.get_mut(position).and_then(|row| row.value.downcast_mut::<Option<F>>()) {
            *value = Some(painter);
        } else {
            let row = PaintRow {
                value: Box::new(Some(painter)),
                paint: |value, paint, rect| value.downcast_ref::<Option<F>>().unwrap().as_ref().unwrap()(paint, rect),
                clear: |value| { *value.downcast_mut::<Option<F>>().unwrap() = None; },
            };
            if position == b.overlays.len() { b.overlays.push(row); } else { b.overlays[position] = row; }
        }
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



    pub(crate) fn set_box_h(&mut self, b: &UiBox, h: Size) {
        self.boxes[b.index].h = h;
    }

    /// The last child built under the current parent (`NONE` when it has none yet).
    pub(crate) fn last_child_of_parent(&self) -> usize {
        self.boxes[self.current_parent()].last
    }

    pub(crate) fn box_w_px(&self, index: usize) -> Option<f32> {
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


    pub(crate) fn slot_rect(&self, slot: usize) -> Rect {
        let s = &self.slots[slot];
        Rect::new(s.rx, s.ry, s.rw, s.rh)
    }

    pub(crate) fn slot_for(&self, key: i64) -> Option<usize> {
        self.table.find(key)
    }

    pub(crate) fn slot_state(&self, slot: usize) -> Option<i64> {
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
            let mut across: f32 = 0.0;
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
            let resolve = |kind: &Size, content: f32| match kind {
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
        let next = self.scroll_y[slot] as f64 + old - delta;
        let bounded = 0.0f64.max(max_scroll.min(next));
        let over = next - bounded;
        let raw = if max_scroll <= 0.0 || over == 0.0 { 0.0 } else { old + (over - old) * past };
        self.scroll_y[slot] = bounded as f32;
        self.slots[slot].scroll_raw = raw;
        self.slots[slot].scroll_visual = scroll_stretch(raw, height);
    }

    /// One scroll box, once its height is laid out (`arrange`): a height that comes from the
    /// parent (Grow, Pct, Rel) stretches and bounces as a fixed one does.
    pub(crate) fn apply_scroll(&mut self, consume: bool, time: f64, index: usize) {
        let slot = self.boxes[index].slot;
        let key = self.boxes[index].key;
        let height = self.l_h[index] as f64;
        let max_scroll = 0.0f64.max(self.l_content[index] as f64 - height);
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
            self.pull(slot, max_scroll, height, 1.0, drag as f64);
            // held by the fingers: it springs back when they lift
            self.slots[slot].scroll_event_time = f64::INFINITY;
        } else if wheel != 0.0 {
            let step = self.boxes[index].scroll_step;
            self.pull(slot, max_scroll, height, SCROLL_WHEEL_PAST, wheel as f64 * step as f64);
            self.slots[slot].scroll_event_time = time;
        }
        if lift {
            if self.slots[slot].scroll_visual != 0.0 {
                self.slots[slot].scroll_event_time = f64::NEG_INFINITY;
            } else if max_scroll > 0.0 && self.gesture_velocity.abs() >= SCROLL_MIN_VELOCITY {
                let y = self.scroll_y[slot] as f64;
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
                    self.scroll_y[slot] = edge as f32;
                    let s = &mut self.slots[slot];
                    s.scroll_mode = 2;
                    s.scroll_start += late;
                    s.scroll_velocity = SCROLL_BOUNCE_LAUNCH * arrival / (SCROLL_BOUNCE_B - SCROLL_BOUNCE_A);
                } else {
                    // on whole device pixels, so text is sharp where the coast ends
                    let density = self.density as f64;
                    self.scroll_y[slot] = ((y * density).round() / density) as f32;
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
            let max_scroll = 0.0f32.max(self.l_content[index] - self.l_h[index]);
            let gutter = if scrolls && max_scroll > 0.0 && flags & OVER_THUMB == 0 { 10.0 } else { 0.0 };
            self.l_gutter[index] = gutter;
            let slot = self.boxes[index].slot;
            if scrolls {
                self.apply_scroll(false, time, index);
                self.scroll_y[slot] = 0.0f32.max(max_scroll.min(self.scroll_y[slot]));
            }
            let inner_w = self.l_w[index] - 2.0 * padding - gutter;
            let inner_h = self.l_h[index] - 2.0 * padding;
            let relative = |kind: &Size, inner: f32, current: f32| match kind {
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
                0.0f32.max(((if row { inner_w } else { inner_h }) - fixed) / grow_count as f32)
            };
            let mut cursor = padding;
            let scroll_offset = if scrolls { self.scroll_y[slot] + self.slots[slot].scroll_visual as f32 } else { 0.0 };
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
        Rect::new(self.l_x[index], self.l_y[index], self.l_w[index], self.l_h[index])
    }

    pub(crate) fn set_l_h(&mut self, b: &UiBox, h: f32) {
        self.l_h[b.index] = h;
    }

    pub(crate) fn set_l_content(&mut self, b: &UiBox, v: f32) {
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
pub fn text_top_for(font_size: i64, size: Option<i64>, y: f32, h: f32) -> f32 {
    y + ((h - size.unwrap_or(font_size) as f32) / 2.0).floor()
}

/// The instance clip of `rect`: whole points covering it.
fn clip_of(Rect { x, y, w, h }: Rect) -> [i16; 4] {
    let whole = |v: f32| v.clamp(i16::MIN as f32, i16::MAX as f32) as i16;
    [whole(x.floor()), whole(y.floor()), whole((x + w).ceil()), whole((y + h).ceil())]
}

/// The painter's handle for one frame. It owns the layers being filled, the glyph atlas and the
/// layout arrays painters read while painting; `Ui::frame` hands them back afterwards.
pub struct Paint {
    layers: Vec<Layer>,
    layer: usize,
    pub clip_rect: Rect,
    pub theme: Theme,
    pub font_size: i64,
    pub row_height: i64,
    /// device pixels per point
    pub density: i64,
    pub text: Text,
    /// the focused text's input area and caret offset
    pub ime: Option<((i64, i64, i64, i64), i64)>,
    label: String,
    /// the horizontal scroll of the focused text; painters adjust it
    pub edit_scroll_x: f32,
    /// the content height of each box of this frame, by box index
    pub l_content: Vec<f32>,
    /// the scroll offset of each retained slot
    pub scroll_y: Vec<f32>,
}

impl Paint {
    /// Start the next layer (a floating root's), reusing a layer vector a previous frame left.
    fn next_layer(&mut self, key: i64) {
        self.layer = self.layer.wrapping_add(1);
        if self.layer == self.layers.len() {
            self.layers.push(Layer { key, instances: Vec::with_capacity(256), breaks: Vec::new(), images: Vec::new() });
        }
        let layer = &mut self.layers[self.layer];
        layer.key = key;
        layer.instances.clear();
        layer.breaks.clear();
        layer.images.clear();
    }

    /// Append `instance` under the current clip.
    fn push(&mut self, instance: Instance) {
        let clip = clip_of(self.clip_rect);
        self.layers[self.layer].instances.push(Instance { clip, ..instance });
    }

    /// An edge at `v` points as the rasteriser covers pixel centres (`a <= centre < b`): snapped
    /// to the pixel grid, so hard-edged fills keep the pixels a triangle rectangle covers.
    fn snap(&self, v: f32) -> f32 {
        let d = self.density as f32;
        (v * d - 0.5).ceil() / d
    }

    fn hard(&mut self, x0: f32, y0: f32, x1: f32, y1: f32, color: [u8; 4]) {
        let r = [self.snap(x0), self.snap(y0), self.snap(x1), self.snap(y1)];
        if r[2] > r[0] && r[3] > r[1] {
            self.push(Instance::rect(r, color));
        }
    }

    /// Draw a host-owned image for this frame; the layer breaks to its texture.
    pub fn image(&mut self, x: f32, y: f32, w: f32, h: f32, image: &Image) {
        if w > 0.0 && h > 0.0 {
            let layer = &mut self.layers[self.layer];
            let index = match layer.images.iter().position(|other| Rc::ptr_eq(&other.handle, &image.handle)) {
                Some(index) => index,
                None => {
                    layer.images.push(image.clone());
                    layer.images.len() - 1
                }
            } as u32;
            if layer.breaks.last().map(|b| b.image) != Some(index) {
                layer.breaks.push(Break { at_instance: layer.instances.len() as u32, image: index });
            }
            self.push(Instance::image([x, y, x + w, y + h], [0.0, 0.0, 1.0, 1.0], [255; 4]));
        }
    }

    /// Square fills cover exactly the pixels of the equivalent triangle rectangle.
    pub fn fill(&mut self, x: f32, y: f32, w: f32, h: f32, color: Color) {
        if w > 0.0 && h > 0.0 {
            self.hard(x, y, x + w, y + h, rgba(color));
        }
    }

    /// A band of `width` centred on the rectangle's edges.
    pub fn stroke_w(&mut self, x: f32, y: f32, w: f32, h: f32, width: f32, color: Color) {
        if !(w > 0.0 && h > 0.0) {
            return;
        }
        let half = width / 2.0;
        let c = rgba(color);
        let o = [self.snap(x - half), self.snap(y - half), self.snap(x + w + half), self.snap(y + h + half)];
        let i = [self.snap(x + half), self.snap(y + half), self.snap(x + w - half), self.snap(y + h - half)];
        if o[2] <= o[0] || o[3] <= o[1] {
            return;
        }
        let sides = [i[0] - o[0], i[1] - o[1], o[2] - i[2], o[3] - i[3]];
        if i[2] <= i[0] || i[3] <= i[1] {
            self.push(Instance::rect(o, c));
        } else if sides.iter().all(|side| *side == sides[0]) {
            self.push(Instance::rect(o, [0; 4]).border(sides[0], c));
        } else {
            self.hard(o[0], o[1], o[2], i[1], c);
            self.hard(o[0], i[3], o[2], o[3], c);
            self.hard(o[0], i[1], i[0], i[3], c);
            self.hard(i[2], i[1], o[2], i[3], c);
        }
    }

    pub fn stroke(&mut self, x: f32, y: f32, w: f32, h: f32, color: Color) {
        self.stroke_w(x, y, w, h, 1.0, color);
    }

    /// A border drawn inside its box, as CSS draws one: `stroke` centres its line on the
    /// rectangle's edge, so a box of whole points gets the line half a width in; the border then
    /// covers whole points.
    pub fn frame_w(&mut self, x: f32, y: f32, w: f32, h: f32, width: f32, color: Color) {
        let half = width / 2.0;
        self.stroke_w(x + half, y + half, w - width, h - width, width, color);
    }

    pub fn frame(&mut self, x: f32, y: f32, w: f32, h: f32, color: Color) {
        self.frame_w(x, y, w, h, 1.0, color);
    }

    pub fn rect(&mut self, x: f32, y: f32, w: f32, h: f32, fill: Option<Color>, stroke: Option<Color>) {
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

    /// Butt-capped; axis-aligned lines are exact rectangles.
    pub fn line_w(&mut self, from: (f32, f32), to: (f32, f32), width: f32, color: Color) {
        let (x0, y0) = from;
        let (x1, y1) = to;
        let half = width / 2.0;
        if x0 == x1 {
            self.fill(x0 - half, y0.min(y1), width, (y1 - y0).abs(), color);
        } else if y0 == y1 {
            self.fill(x0.min(x1), y0 - half, (x1 - x0).abs(), width, color);
        } else {
            self.push(Instance::line([x0, y0], [x1, y1], width, rgba(color)));
        }
    }

    pub fn line(&mut self, from: (f32, f32), to: (f32, f32), color: Color) {
        self.line_w(from, to, 1.0, color);
    }

    /// A disc with a one-point stroke centred on its edge.
    pub fn circle(&mut self, at: (f32, f32), radius: f32, fill: Option<Color>, stroke: Option<Color>) {
        if !(radius > 0.0) {
            return;
        }
        let fill = match (fill, stroke) {
            (None, None) => Some(color::white()),
            _ => fill,
        };
        let fill = fill.map(rgba).unwrap_or([0; 4]);
        let (cx, cy) = at;
        self.push(match stroke {
            None => Instance::rect([cx - radius, cy - radius, cx + radius, cy + radius], fill).radii([radius; 4]),
            Some(stroke) => {
                let r = radius + 0.5;
                Instance::rect([cx - r, cy - r, cx + r, cy + r], fill).radii([r; 4]).border(1.0, rgba(stroke))
            }
        });
    }

    /// `dot`-point squares every `spacing` points from `origin`, inside the rectangle.
    // ponytail: one instance per dot (a 2000 x 1200 pane at the graph's 24-point spacing is about
    // 4,200); a grid instance kind in the shader if the instance ring shows up in a profile.
    pub fn grid(&mut self, x: f32, y: f32, w: f32, h: f32, origin: (f32, f32), spacing: f32, dot: f32, color: Color) {
        if !(w > 0.0 && h > 0.0 && spacing > 0.0) {
            return;
        }
        let c = rgba(color);
        let saved = self.clip_rect;
        self.clip_rect = saved.intersect(Rect::new(x, y, w, h));
        let first = |lo: f32, o: f32| ((lo - o - dot) / spacing).floor() as i64 + 1;
        let last = |hi: f32, o: f32| ((hi - o) / spacing).ceil() as i64 - 1;
        for j in first(y, origin.1)..=last(y + h, origin.1) {
            let dy = origin.1 + j as f32 * spacing;
            for i in first(x, origin.0)..=last(x + w, origin.0) {
                let dx = origin.0 + i as f32 * spacing;
                self.push(Instance::rect([dx, dy, dx + dot, dy + dot], c));
            }
        }
        self.clip_rect = saved;
    }

    /// Kit text with its top-left corner at `at`. Without `size` it uses the UI's own font; with
    /// `size` the kit face at that size. `tracking` adds points after every glyph.
    pub fn text_with(&mut self, at: (f32, f32), size: Option<i64>, tracking: f32, color: Option<Color>, text: &str) {
        if !text.is_empty() {
            let style = kit_style(size.unwrap_or(self.font_size), tracking);
            self.text_styled(at, &style, 0.0, color, text);
        }
    }

    /// Text in any style (wrap, ellipsis, line limit, alignment, font) within `width` points (0:
    /// unbounded), its top-left corner at `at`: the first baseline sits the font's ascent below `at`,
    /// on a device pixel. Mask glyphs take `color` (the theme's foreground by default); colour glyphs
    /// keep their own colours under its alpha.
    pub fn text_styled(&mut self, at: (f32, f32), style: &Style, width: f32, color: Option<Color>, text: &str) -> Metrics {
        let (key, m) = lay_out(&mut self.text, style, width, text);
        let d = self.density as f32;
        let snap = |v: f32| (v * d).round() / d;
        let baseline = snap(at.1 + self.text.ascent(style.font) * style.size);
        let origin = (snap(at.0), baseline - m.baseline);
        let color = rgba(color.unwrap_or(self.theme.foreground));
        let clip = clip_of(self.clip_rect);
        let Paint { text: t, layers, layer, .. } = self;
        let layer = &mut layers[*layer];
        t.glyphs(key, origin, d, &mut |g| {
            let r = [g.rect.x / d, g.rect.y / d, (g.rect.x + g.rect.w) / d, (g.rect.y + g.rect.h) / d];
            let uv = [g.uv.x, g.uv.y, g.uv.x + g.uv.w, g.uv.y + g.uv.h];
            let instance = match g.atlas {
                pxui_text::AtlasKind::Mask => Instance::mask(r, uv, color),
                pxui_text::AtlasKind::Color => {
                    // image instances after an image's break sample that image: break back to the atlas
                    if layer.breaks.last().is_some_and(|b| b.image != crate::draw::ATLAS) {
                        layer.breaks.push(Break { at_instance: layer.instances.len() as u32, image: crate::draw::ATLAS });
                    }
                    Instance::image(r, uv, [255, 255, 255, color[3]])
                }
            };
            layer.instances.push(Instance { clip, ..instance });
        });
        m
    }

    pub fn text(&mut self, at: (f32, f32), text: &str) {
        self.text_with(at, None, 0.0, None, text);
    }

    pub fn text_c(&mut self, at: (f32, f32), color: Color, text: &str) {
        self.text_with(at, None, 0.0, Some(color), text);
    }

    pub fn text_s(&mut self, at: (f32, f32), size: i64, color: Color, text: &str) {
        self.text_with(at, Some(size), 0.0, Some(color), text);
    }

    pub fn text_width(&mut self, size: Option<i64>, text: &str) -> f32 {
        let size = size.unwrap_or(self.font_size);
        lay_out(&mut self.text, &kit_style(size, 0.0), 0.0, text).1.w
    }

    /// See `Ui::text_metrics`.
    pub fn text_metrics(&mut self, style: &Style, width: f32, text: &str) -> Metrics {
        lay_out(&mut self.text, style, width, text).1
    }

    /// The x of the caret before byte `index` of kit text, in points from the text's start.
    pub fn caret_x(&mut self, size: Option<i64>, text: &str, index: usize) -> f32 {
        let size = size.unwrap_or(self.font_size);
        caret_x(&mut self.text, size, text, index)
    }

    /// The highlight of bytes `a..b` of single-line kit text: one span per visual run (two or
    /// more across a direction change), as `(x, w)` in points from the text's start.
    pub fn selection(&mut self, size: Option<i64>, text: &str, a: usize, b: usize, f: &mut dyn FnMut(f32, f32)) {
        let size = size.unwrap_or(self.font_size);
        let (key, _) = lay_out(&mut self.text, &kit_style(size, 0.0), 0.0, text);
        self.text.selection(key, a, b, &mut |r| f(r.x, r.w));
    }

    /// Where the top of kit text of `size` goes to sit centred in a row at `y` of height `h`.
    pub fn text_top(&self, size: Option<i64>, y: f32, h: f32) -> f32 {
        text_top_for(self.font_size, size, y, h)
    }

    /// The label style: upper case, two points under the body, 0.08 em of tracking.
    pub fn label_size(&self) -> i64 {
        8.max(self.font_size - 2)
    }

    pub fn cap_tracking(&self) -> f32 {
        0.08 * self.label_size() as f32
    }

    /// A label: upper case at `label_size` with 0.08 em of tracking, ink-2 by default.
    pub fn cap(&mut self, at: (f32, f32), color: Option<Color>, label: &str) {
        let color = color.unwrap_or(theme::ink_2(&self.theme));
        let size = self.label_size();
        let tracking = self.cap_tracking();
        let mut buffer = std::mem::take(&mut self.label);
        buffer.clear();
        buffer.extend(label.chars().map(|c| c.to_ascii_uppercase()));
        self.text_with(at, Some(size), tracking, Some(color), &buffer);
        self.label = buffer;
    }

    /// The tracking follows every letter, the last too (CSS letter-spacing): text after a label
    /// starts one step later, and a right-aligned label ends one step early.
    pub fn cap_width(&mut self, label: &str) -> f32 {
        let mut buffer = std::mem::take(&mut self.label);
        buffer.clear();
        buffer.extend(label.chars().map(|c| c.to_ascii_uppercase()));
        let style = kit_style(self.label_size(), self.cap_tracking());
        let width = lay_out(&mut self.text, &style, 0.0, &buffer).1.w;
        self.label = buffer;
        width
    }

    /// A chevron centred on `at`: its strokes span 7 x 3.5, which with the 1-point line is the
    /// sheet's 9 x 5.5 of ink.
    pub fn chevron(&mut self, at: (f32, f32), direction: Dir, color: Color) {
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
        let p = |d: (f32, f32)| (at.0 + d.0, at.1 + d.1);
        self.line(p(a), p(tip), color);
        self.line(p(tip), p(b), color);
    }

    /// Selection: four corner brackets `offset` outside the rectangle.
    pub fn brackets(&mut self, x: f32, y: f32, w: f32, h: f32, offset: f32, length: f32, width: f32, color: Color) {
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

    pub fn dashed_w(&mut self, from: (f32, f32), to: (f32, f32), width: f32, color: Color) {
        let (x0, y0) = from;
        let (x1, y1) = to;
        let length = (x1 - x0).hypot(y1 - y0);
        let steps = (length / 7.0).ceil() as i64;
        for step in 0..steps {
            let a = step as f32 * 7.0 / length;
            let b = 1.0f32.min((step as f32 * 7.0 + 4.0) / length);
            self.line_w((x0 + (x1 - x0) * a, y0 + (y1 - y0) * a), (x0 + (x1 - x0) * b, y0 + (y1 - y0) * b), width, color);
        }
    }

    pub fn dashed(&mut self, from: (f32, f32), to: (f32, f32), color: Color) {
        self.dashed_w(from, to, 1.0, color);
    }

    /// 4 on, 3 off: a zone, a drop target.
    pub fn dashed_rect(&mut self, x: f32, y: f32, w: f32, h: f32, color: Color) {
        self.dashed((x, y), (x + w, y), color);
        self.dashed((x, y + h), (x + w, y + h), color);
        self.dashed((x, y), (x, y + h), color);
        self.dashed((x + w, y), (x + w, y + h), color);
    }

    /// An outline flag centred at `at`: 12 points, the input fill, a line-3 edge inside the box
    /// and, when on, a 6-point mark in ink-2; square, or `round` for the columns after the first.
    pub fn flag(&mut self, at: (f32, f32), round: bool, on: bool) {
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
    pub fn ratio(&mut self, at: (f32, f32), first: i64, second: i64) {
        let accent = self.theme.accent;
        self.cap(at, Some(accent), &format!("{} / {}", first, second));
    }

    /// The focused text's input area, for the input method; the first one painted wins.
    pub fn input_region(&mut self, cursor: f32, x: f32, y: f32, w: f32, h: f32) {
        if self.ime.is_none() {
            self.ime = Some(((x as i64, y as i64, w as i64, h as i64), 0.max(cursor.round() as i64)));
        }
    }
}

impl Ui {
    fn record_hit(&mut self, index: usize, parent: i64, rect: Rect) {
        let position = self.hit_count;
        let Rect { x, y, w, h } = rect;
        let rec = HitRec { key: self.boxes[index].key, parent, flags: self.boxes[index].flags, x, y, w, h };
        if position >= self.hits.len() {
            self.hits.push(rec);
        } else {
            self.hits[position] = rec;
        }
        self.chain_key = 0;
        self.hit_count = position + 1;
    }

    fn run_painters(&self, painters: &[PaintRow], index: usize, clip_rect: Rect, paint: &mut Paint) {
        if !painters.is_empty() {
            paint.clip_rect = clip_rect;
            let local = self.screen(index);
            for painter in painters {
                (painter.paint)(painter.value.as_ref(), paint, local);
            }
        }
    }

    fn visit(&mut self, paint: &mut Paint, index: usize, clip_rect: Rect, parent_hit: i64) {
        if self.boxes[index].parent == 0 && self.foreground.iter().any(|(_, root)| *root == index) {
            paint.next_layer(self.boxes[index].key);
        }
        let screen_rect = self.screen(index);
        let slot = self.boxes[index].slot;
        let Rect { x, y, w, h } = screen_rect;
        {
            let s = &mut self.slots[slot];
            s.rx = x;
            s.ry = y;
            s.rw = w;
            s.rh = h;
        }
        let hit = self.boxes[index].hit_rect.unwrap_or_else(|| match &self.boxes[index].hit {
            None => screen_rect,
            Some(f) => f(screen_rect),
        });
        let clipped_hit = intersect(hit, clip_rect);
        let Rect { w: hw, h: hh, .. } = clipped_hit;
        let visible = match (self.boxes[index].hit_rect, &self.boxes[index].hit) {
            (None, None) => hw > 0.0 && hh > 0.0,
            _ => {
                let Rect { w: vw, h: vh, .. } = intersect(screen_rect, clip_rect);
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
                self.run_painters(&self.boxes[index].painters[..self.boxes[index].painter_count], index, clip_rect, paint);
            }
            let child_clip = if flags & CLIP != 0 {
                let padding = self.boxes[index].padding;
                intersect(clip_rect, Rect::new(x + padding, y + padding, 0.0f32.max(w - 2.0 * padding), 0.0f32.max(h - 2.0 * padding)))
            } else {
                clip_rect
            };
            let parent_hit = if has_hit { self.boxes[index].key } else { parent_hit };
            let mut child = self.boxes[index].first;
            while child != NONE {
                self.visit(paint, child, child_clip, parent_hit);
                child = self.boxes[child].next;
            }
            if shown {
                self.run_painters(&self.boxes[index].overlays[..self.boxes[index].overlay_count], index, clip_rect, paint);
            }
            if shown && self.keyboard_focus && self.focus == self.boxes[index].key && flags & FOCUS_MARK == 0 {
                paint.clip_rect = clip_rect;
                let accent = self.theme.accent;
                paint.frame(self.l_x[index], self.l_y[index], 0.0f32.max(self.l_w[index]), 0.0f32.max(self.l_h[index]), accent);
            }
        } else {
            // Culled: keep retained rectangles current for `rect` queries.
            self.retain_rects(index);
        }
    }

    fn retain_rects(&mut self, index: usize) {
        let mut child = self.boxes[index].first;
        while child != NONE {
            let Rect { x, y, w, h } = self.screen(child);
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

    fn paint_all(&mut self, frame: &rframe::Frame) {
        assert!(!self.painting, "Ui: painting re-entered");
        self.painting = true;
        self.hit_count = 0;
        self.chain_key = 0;
        let clip = Rect::new(0.0, 0.0, frame.input.size.0 as f32, frame.input.size.1 as f32);
        // paint into the other buffer, keeping the last frame's layers to compare with
        let mut paint = Paint {
            layers: std::mem::take(&mut self.spare),
            // the base layer is the one after none
            layer: usize::MAX,
            clip_rect: clip,
            theme: self.theme,
            font_size: self.font_size,
            row_height: self.kit_row_height,
            density: self.density,
            text: self.text.take().expect("Ui: painting re-entered"),
            ime: None,
            label: std::mem::take(&mut self.label),
            edit_scroll_x: self.edit_scroll_x,
            l_content: std::mem::take(&mut self.l_content),
            scroll_y: std::mem::take(&mut self.scroll_y),
        };
        paint.next_layer(0);
        self.visit(&mut paint, 0, clip, 0);
        let len = paint.layer + 1;
        let same = |a: &Layer, b: &Layer| {
            a.key == b.key && a.instances == b.instances && a.breaks == b.breaks
                && a.images.len() == b.images.len() && a.images.iter().zip(&b.images).all(|(x, y)| Rc::ptr_eq(&x.handle, &y.handle))
        };
        if len != self.draw_len || !paint.layers[..len].iter().zip(&self.draw[..len]).all(|(a, b)| same(a, b)) {
            self.draw_revision += 1;
        }
        self.spare = std::mem::replace(&mut self.draw, paint.layers);
        self.draw_len = len;
        self.text = Some(paint.text);
        self.out.ime = paint.ime;
        self.label = paint.label;
        self.edit_scroll_x = paint.edit_scroll_x;
        self.l_content = paint.l_content;
        self.scroll_y = paint.scroll_y;
        self.painting = false;
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
        self.draw_over(&ghost, move |paint, Rect { x, y, w, h }| {
            paint.fill(x, y, w, h, theme.input);
            paint.text_s((x + 6.0, y + ((20 - size) as f32 / 2.0).floor()), size, theme.foreground, &value);
        });
        self.draw_over(&tip, move |paint, Rect { x, y, w, h }| {
            paint.fill(x, y, w, h, theme.input);
            paint.stroke(x + 0.5, y + 0.5, w - 1.0, h - 1.0, theme::edge(&theme));
            let ty = y + ((20 - small) as f32 / 2.0).floor();
            paint.text_s((x + 7.0, ty), small, theme.foreground, tip_text);
            paint.text_s((x + 7.0 + tw1 + 6.0, ty), small, theme::ink_3(&theme), tip_key);
        });
        // Tooltip and ghost layers precede existing foreground entries at maximum priority.
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
            b.hit_rect = None;
            for row in b.painters.iter_mut().chain(&mut b.overlays) {
                (row.clear)(row.value.as_mut());
            }
            b.painter_count = 0;
            b.overlay_count = 0;
        }
    }

    /// Route the frame's ordered events, run the builder, lay out, and paint. Tab/Shift-Tab
    /// traverse visible focusable boxes in presentation order, restricted to an open popup.
    /// Enter/Space activate controls.
    pub fn frame(&mut self, frame: &rframe::Frame, f: &mut dyn FnMut(&mut Ui)) -> FrameOut<'_> {
        self.begin_frame(frame);
        f(self);
        self.end_frame(frame);
        self.output()
    }

    pub fn begin_frame(&mut self, frame: &rframe::Frame) {
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
        self.density = 1.max(scale_x.max(scale_y).round() as i64);
        self.frame_number += 1;
        // a request made between frames is this frame; one made while building is the next
        self.frame_requested = false;
        self.out.clipboard = None;
        self.out.paste = false;
        self.out.background = None;
        // the window's title bar shows the ground the UI is painted on
        if self.window_ground != Some(self.theme.panel) {
            self.window_ground = Some(self.theme.panel);
            self.out.background = Some(self.theme.panel);
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
            Size::Px(frame.input.size.0 as f32),
            Size::Px(frame.input.size.1 as f32),
            f32::INFINITY,
            false,
            0.0,
            0.0,
            0.0,
            0.0,
            None,
        );
        self.parents.push(root);
        self.seeds.push(ROOT_KEY);
    }

    pub fn end_frame(&mut self, frame: &rframe::Frame) {
        self.carry_ghost();
        self.building = false;
        self.parents.clear();
        self.seeds.clear();
        // Move popups to the end of the root's children, oldest first.
        self.foreground.sort_by(|a, b| a.0.cmp(&b.0));
        for position in 0..self.foreground.len() + self.overlays.len() {
            let index = if position < self.foreground.len() {
                self.foreground[position].1
            } else {
                self.overlays[position - self.foreground.len()]
            };
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
        self.paint_all(frame);
        self.text_mut().end_frame();
        self.foreground.clear();
        self.prune();
        if self.focus != 0 && self.table.find(self.focus).is_none() {
            self.focus = 0;
            self.edit_focus = 0;
        }
        self.out.repaint_at = self.next_repaint(frame.input.t);
        self.frame_requested = false;
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
        if self.active.is_some() || self.modal_in_frame || self.frame_events.iter().any(|e| matches!(e, Event::FocusLost)) {
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
                self.hover_rest.as_mut().unwrap().3 = self.frame_number;
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
        let char_width = 1.0f32.max(self.text_width(Some(size), "0"));
        let width = 400.0f32.min(40.0f32.max(self.view_w - 16.0));
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
        let mut widest: f32 = 0.0;
        for l in &lines {
            widest = widest.max(self.text_width(Some(size), l));
        }
        let width = width.min(14.0 + widest);
        let height = 20.0 + 14.0 * (lines.len() as f32 - 1.0);
        let (px, py) = self.pointer;
        let x = 8.0f32.max((px + 12.0).min(self.view_w - width - 8.0));
        let y = if py + height + 24.0 <= self.view_h { py + 20.0 } else { 8.0f32.max(py - height - 8.0) };
        let saved = self.enter_root();
        let tip = self.box_("ui-tooltip", BoxOpts::default().w(Size::Px(width)).h(Size::Px(height)).at((x, y)));
        self.overlays.push(tip.index);
        let theme = self.theme;
        self.draw(&tip, move |paint, Rect { x, y, w, h }| {
            paint.fill(x, y, w, h, theme.input);
            paint.frame(x, y, w, h, theme::edge(&theme));
            for (i, line) in lines.iter().enumerate() {
                paint.text_s(
                    (x + 7.0, y + 14.0 * i as f32 + ((20 - size) as f32 / 2.0).floor()),
                    size,
                    theme.foreground,
                    line,
                );
            }
        });
        self.leave(saved);
    }

    pub fn col(&mut self, padding: f32, label: &str, f: &mut dyn FnMut(&mut Ui)) {
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
        Saved::Stacks(std::mem::replace(&mut self.parents, vec![0]), self.seeds.clone())
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

    pub(crate) fn modal_height(&self, key: i64) -> f32 {
        self.modal_heights.get(&key).map(|(h, _)| *h).unwrap_or(0.0)
    }


    pub(crate) fn slot_rw(&self, slot: usize) -> f32 {
        self.slots[slot].rw
    }

    pub(crate) fn set_flags_of_box(&mut self, b: &UiBox, flags: Flags) {
        self.boxes[b.index].flags = flags;
    }

}
