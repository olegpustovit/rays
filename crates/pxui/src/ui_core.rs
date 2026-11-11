//! Each frame declares keyed nodes. A node is a retained record with a stable `Handle` (slot index and
//! generation): its layout inputs, its laid-out rectangle, its retained input state, and its `Look`
//! and text as plain data the engine paints with no closure. A node lives until the build that owns
//! it stops declaring it. Input uses prior-frame rectangles; layout follows construction and painting
//! packs instance lists. Canvas closures live in the frame arena and run after layout. Between builds
//! the `patch_*` functions rewrite one node's instances in place (layer 1 of specification/pxui.md).
use crate::theme;
pub use crate::geom::Rect;
pub(crate) use crate::geom::p32;
use input::{Event, Key, MouseButton, Platform, ScrollPhase};
use crate::geom::hash;
use input::frame as rframe;
use crate::geom::{color, Rgba as Color};
use crate::draw::{Break, Instance};
use crate::layout::MAX_TRACKS;
pub use crate::draw::Fit;
use crate::arena::{self, Arena};
pub use crate::geom::Edges;
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

#[derive(Clone, Copy, Debug)]
pub enum Size {
    /// fixed
    Px(f32),
    /// fraction of the parent's inner size
    Pct(f32),
    /// function of the parent's inner size
    Rel(fn(f32) -> f32),
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
    /// children overlap at the padding origin, in `z` order (equal `z`: build order); `justify`
    /// places them across, `align` down
    Stack,
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
/// The node's text is cut with an ellipsis to the room its look leaves (the placed width less the
/// text inset) when it does not fit.
pub const FIT_TEXT: Flags = 16384;
/// scrolls sideways (the wheel's and the trackpad's horizontal motion); with `SCROLL` on both axes
pub const SCROLL_X: Flags = 32768;
/// a flow child of a scroll box that stops at its top edge instead of scrolling out (CSS sticky);
/// it paints over its siblings
pub const STICKY: Flags = 65536;
/// with `CLIP`: the children's clip takes the corner radius of the node's look (CSS `overflow:
/// hidden` with `border-radius`); without it a clip is square
pub const ROUND_CLIP: Flags = 131072;
const HIT_FLAGS: Flags = CLICKABLE | FOCUSABLE | SCROLL | SCROLL_X | BLOCKING | CAPTURE_UNCLAIMED;

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

pub type HitFn = Rc<dyn Fn(Rect) -> Rect>;

pub use crate::theme::{Curve, Font, Look, Ring, Shadow, State, States, Theme, Tokens};
pub use crate::a11y::{A11yAction, A11yTree, Role};
use crate::a11y::{actions as a11y_actions, state as a11y_state, Action as A11yKind, NO_PARENT};
pub use crate::layout::{place_anchored, Align, Anchor, Cell, Direction, Grid, Justify, Placement, Side, Track};

/// A drawn mark centred in a node's rectangle, in its look's text colour.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mark {
    None,
    Chevron(Dir),
}

/// A scroll node's thumb: `width` points wide, `gap` from the right edge, between `margin` points of
/// the top and bottom; `whole` keeps it on whole points (a panel's).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Bar {
    pub color: Color,
    pub radius: f32,
    pub width: f32,
    pub gap: f32,
    pub margin: f32,
    pub whole: bool,
}

/// A node's layout inputs and look. Sizes: `w`, `h` clamped to `min_*`/`max_*` (min wins); `aspect`
/// (w / h, 0 none) derives a `Fit` side from the other; a `Grow` size takes `grow` shares of the room
/// its line leaves, and when the line overflows a child gives up room by `shrink` times its size.
/// Children: `axis` (row, column or stack), or a `grid`; `padding` inside, `margin` outside (`l` is
/// the start edge), `gap` between children (`row_gap` between wrapped lines and grid rows), `justify`
/// along the axis, `align` across it (`align_self` overrides it for this node), `wrap` breaks a
/// flow into lines; `cell` is its place in a parent grid, `z` its order in a stack. `at` places it
/// outside the flow at an offset from the parent's origin, `anchor` by a window rectangle (a popup).
/// `inset` is where its text sits in it; `mark` is drawn
/// over its text; `bar` gives a scroll node its thumb; `state` is what the look was picked for (a
/// change of state starts the theme's transition).
#[derive(Clone, Copy, Debug)]
pub struct Node {
    pub flags: Flags,
    pub w: Size,
    pub h: Size,
    pub min_w: f32,
    pub min_h: f32,
    pub max_w: f32,
    pub max_h: f32,
    pub grow: f32,
    pub shrink: f32,
    pub aspect: f32,
    pub axis: Axis,
    pub padding: Edges,
    pub margin: Edges,
    pub gap: f32,
    pub row_gap: f32,
    pub justify: Justify,
    pub align: Align,
    pub align_self: Option<Align>,
    pub wrap: bool,
    pub grid: Option<Grid>,
    pub cell: Cell,
    pub z: i16,
    pub anchor: Option<Anchor>,
    /// what the node is to a screen reader (`Unknown`: not in the accessibility tree) and the states
    /// its widget declares (`a11y::state` bits: CHECKABLE, CHECKED, EXPANDABLE, EXPANDED, EDITABLE,
    /// MULTILINE, SELECTED, MODAL); focus, press, enabled and visibility come from the engine
    pub role: Role,
    pub a11y: u32,
    pub at: Option<(f32, f32)>,
    pub scroll_step: f32,
    pub look: Look,
    pub inset: Edges,
    pub mark: Mark,
    pub bar: Option<Bar>,
    pub state: State,
    /// the transition `set_look` picked for a change of state, started when the node is painted
    pub transition: Option<Curve>,
}

impl Node {
    pub const DEFAULT: Node = Node {
        flags: NO_FLAGS,
        w: Size::Grow,
        h: Size::Fit,
        min_w: 0.0,
        min_h: 0.0,
        max_w: f32::INFINITY,
        max_h: f32::INFINITY,
        grow: 1.0,
        shrink: 0.0,
        aspect: 0.0,
        axis: Axis::Column,
        padding: Edges::ZERO,
        margin: Edges::ZERO,
        gap: 0.0,
        row_gap: 0.0,
        justify: Justify::Start,
        align: Align::Start,
        align_self: None,
        wrap: false,
        grid: None,
        cell: Cell::DEFAULT,
        z: 0,
        anchor: None,
        role: Role::Unknown,
        a11y: 0,
        at: None,
        scroll_step: 24.0,
        look: Look::NONE,
        inset: Edges { l: 0.0, t: 0.0, r: 0.0, b: 0.0 },
        mark: Mark::None,
        bar: None,
        state: State { hovered: false, pressed: false, focused: false, disabled: false, checked: false },
        transition: None,
    };
}

/// One box of this frame's build: its place in the tree, its hit shape and its canvases. What
/// outlives the frame is in the node's slot.
struct BoxRec {
    key: i64,
    slot: usize,
    parent: usize,
    first: usize,
    last: usize,
    next: usize,
    hit: Option<HitFn>,
    hit_rect: Option<Rect>,
    paint: arena::List,
    over: arena::List,
    /// the view this box holds (`NO_VIEW`), and whether its build was skipped this frame
    view: u32,
    kept: bool,
}

impl BoxRec {
    fn blank() -> BoxRec {
        BoxRec { key: 0, slot: 0, parent: NONE, first: NONE, last: NONE, next: NONE, hit: None, hit_rect: None, paint: arena::EMPTY, over: arena::EMPTY, view: NO_VIEW, kept: false }
    }
}

/// No running transition.
const NO_TWEEN: u32 = u32::MAX;

/// A node's look and where `animate` puts it: offset and opacity.
#[derive(Clone, Copy, Debug, PartialEq)]
struct Pose {
    look: Look,
    dx: f32,
    dy: f32,
    opacity: f32,
}

/// A running transition of slot `slot` (while it has `generation`) from `from` to `to`.
#[derive(Clone, Copy)]
struct Tween {
    slot: usize,
    generation: u32,
    start: f64,
    curve: Curve,
    from: Pose,
    to: Pose,
}

/// What `Ui::animate` moves a node to.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Target {
    Look(Look),
    Offset(f32, f32),
    Opacity(f32),
}

/// A transparent colour.
const CLEAR_INK: Color = Color { r: 0, g: 0, b: 0, a: 0 };

/// No view: a box that holds none, a fresh piece of a layer.
const NO_VIEW: u32 = u32::MAX;
/// The clip inside a view with a table entry: its outer clip is the entry's.
const UNBOUNDED: Rect = Rect::new(-1.0e7, -1.0e7, 2.0e7, 2.0e7);
/// Instances that fill the room left before a kept view's run: they draw nothing.
const EMPTY: Instance = Instance::DEFAULT;
/// Room left before each kept run when a layer is compacted.
// ponytail: fixed slack per kept view; a view whose neighbours grow past it compacts the layer
// (every instance rewritten). Size the slack by the neighbours' churn if compaction shows up.
const SLACK: usize = 32;

/// A view's entry in the painter's view table (`pxui_render::View`): its instances move by `offset`,
/// their alpha scales by `opacity`, and `clip` (`[x0, y0, x1, y1]`, window points) bounds them.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ViewEntry {
    pub offset: [f32; 2],
    pub opacity: f32,
    pub clip: [f32; 4],
    /// then `pivot + scale * transform * (p - pivot)` (row-major 2x2, window points), as the painter's View
    pub scale: f32,
    pub transform: [f32; 4],
    pub pivot: [f32; 2],
}

impl ViewEntry {
    /// Where the painter shows view point `p` (already offset).
    fn map(&self, p: (f32, f32)) -> (f32, f32) {
        let [a, b, c, d] = self.transform;
        let (x, y) = (p.0 - self.pivot[0], p.1 - self.pivot[1]);
        (self.pivot[0] + self.scale * (a * x + b * y), self.pivot[1] + self.scale * (c * x + d * y))
    }

    /// The view point the painter shows at window point `q` (before the offset); `None` when the
    /// transform has no inverse.
    fn unmap(&self, q: (f32, f32)) -> Option<(f32, f32)> {
        let [a, b, c, d] = self.transform;
        let det = (a * d - b * c) * self.scale;
        if det == 0.0 || !det.is_finite() {
            return None;
        }
        let (x, y) = (q.0 - self.pivot[0], q.1 - self.pivot[1]);
        Some((self.pivot[0] + (d * x - b * y) / det, self.pivot[1] + (a * y - c * x) / det))
    }

    fn identity(&self) -> bool {
        let [a, b, c, d] = self.transform;
        self.scale == 1.0 && a == 1.0 && b == 0.0 && c == 0.0 && d == 1.0
    }

    /// The window rectangle that covers `r` (view points, offset) once transformed.
    fn cover(&self, r: Rect) -> Rect {
        if self.identity() {
            return r;
        }
        let pts = [(r.x, r.y), (r.x + r.w, r.y), (r.x, r.y + r.h), (r.x + r.w, r.y + r.h)].map(|p| self.map(p));
        let (x0, y0) = pts.iter().fold((f32::INFINITY, f32::INFINITY), |m, p| (m.0.min(p.0), m.1.min(p.1)));
        let (x1, y1) = pts.iter().fold((f32::NEG_INFINITY, f32::NEG_INFINITY), |m, p| (m.0.max(p.0), m.1.max(p.1)));
        Rect::new(x0, y0, x1 - x0, y1 - y0)
    }
}

/// A view that asked for its backdrop blurred (`Ui::view_backdrop_blur`): the host draws layer `layer`
/// up to `at_instance`, blurs what is under `rect` (window points) by `sigma` points with
/// `pxui_render::Painter::blur`, draws the result as an image there (its corners rounded by `radii`,
/// the view box look's, and its alpha scaled by the view's composed `opacity`), then draws the rest.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlurOut {
    pub layer: usize,
    pub at_instance: u32,
    pub rect: Rect,
    pub sigma: f32,
    pub radii: [f32; 4],
    pub opacity: f32,
}

/// A cached subtree (`Ui::view`): when it was built, where its output lives and how it is shown.
struct ViewRec {
    name: String,
    slot: usize,
    /// the enclosing view (0: the root, which rebuilds every frame)
    parent: u32,
    revision: u64,
    epoch: u64,
    /// the `themed` override it was built under (0: none)
    theme: u32,
    /// frames it was last built, kept (build skipped) and reached by the paint
    built: i64,
    kept: i64,
    visited: i64,
    dirty: bool,
    /// its last build put boxes at the root, asked for a frame or ran a hover delay: rebuild it
    escapes: bool,
    patched: bool,
    /// a clean view rebuilt by `verify_views`: its output must equal the kept one
    verify: bool,
    /// `run` in layer `layer` holds its output
    valid: bool,
    layer: i64,
    run: (u32, u32),
    /// this frame's pieces of it: scratch layer, first and end piece
    pieces: (usize, usize, usize),
    /// the texture in effect at the run's start, and the breaks inside it
    tex0: Option<Image>,
    breaks: Vec<(u32, Image)>,
    hits: Vec<HitRec>,
    /// the slots its last build declared, in order (the accessibility walk), and the build's
    /// before (what `prune` frees from)
    nodes: Vec<usize>,
    prev: Vec<usize>,
    hit_parent: i64,
    /// its box at its build (parent coordinates), how far layout has moved it since, and the clip
    /// at its box (parent coordinates)
    built_rect: Rect,
    delta: (f32, f32),
    box_clip: Rect,
    /// its painter table index (0: none, the table was full) and the view whose entry moves its
    /// instances (itself, or with no index its parent's)
    index: u16,
    xf: u32,
    offset: (f32, f32),
    opacity: f32,
    clip: Option<Rect>,
    /// its 2D transform about `pivot` (from its box's corner) and its backdrop blur (0: none)
    scale: f32,
    transform: [f32; 4],
    pivot: (f32, f32),
    blur: f32,
    /// when its last build asked to be rebuilt (`Ui::repaint_after`; infinity: never)
    wake: f64,
    /// the composed entry and the screen rectangle it shows
    shift: (f32, f32),
    alpha: f32,
    outer: Rect,
    screen: Rect,
    stamp: u64,
}

impl ViewRec {
    fn new() -> ViewRec {
        ViewRec {
            name: String::new(),
            nodes: Vec::new(),
            prev: Vec::new(),
            slot: NONE,
            parent: 0,
            revision: 0,
            epoch: 0,
            theme: 0,
            built: -1,
            kept: -1,
            visited: -1,
            dirty: false,
            escapes: false,
            patched: false,
            verify: false,
            valid: false,
            layer: 0,
            run: (0, 0),
            pieces: (0, 0, 0),
            tex0: None,
            breaks: Vec::new(),
            hits: Vec::new(),
            hit_parent: 0,
            built_rect: Rect::new(0.0, 0.0, 0.0, 0.0),
            delta: (0.0, 0.0),
            box_clip: UNBOUNDED,
            index: 0,
            xf: 0,
            offset: (0.0, 0.0),
            opacity: 1.0,
            clip: None,
            scale: 1.0,
            transform: [1.0, 0.0, 0.0, 1.0],
            pivot: (0.0, 0.0),
            blur: 0.0,
            wake: f64::INFINITY,
            shift: (0.0, 0.0),
            alpha: 1.0,
            outer: UNBOUNDED,
            screen: Rect::new(0.0, 0.0, 0.0, 0.0),
            stamp: 0,
        }
    }

    /// No scale or transform.
    fn plain(&self) -> bool {
        let [a, b, c, d] = self.transform;
        self.scale == 1.0 && a == 1.0 && b == 0.0 && c == 0.0 && d == 1.0
    }

    fn entry(&self) -> ViewEntry {
        let c = clip_of(self.outer);
        let b = self.built_rect;
        let pivot = [b.x + self.shift.0 + self.pivot.0, b.y + self.shift.1 + self.pivot.1];
        ViewEntry {
            offset: [self.shift.0, self.shift.1],
            opacity: self.alpha,
            clip: [c[0] as f32, c[1] as f32, c[2] as f32, c[3] as f32],
            scale: self.scale,
            transform: self.transform,
            pivot,
        }
    }
}

/// A run of a layer: fresh instances `a..b` of the frame's scratch layer, or a kept view's run;
/// `at` is where it lands in the retained layer.
#[derive(Clone, Copy)]
struct Piece {
    view: u32,
    a: u32,
    b: u32,
    at: u32,
}

/// A node handle: its index in this frame's build (`NONE` for `box_of_key`), key, retained slot and
/// the slot's generation. The slot and generation are stable while the node lives; a handle to a
/// node that is gone is stale, and the engine checks it before touching the slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UiBox {
    pub index: usize,
    pub box_key: i64,
    pub box_slot: usize,
    pub generation: u32,
}

pub type Handle = UiBox;

static VERIFY_NEW_VIEWS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub fn key(b: &UiBox) -> i64 {
    b.box_key
}

struct Slot {
    key: i64,
    /// bumped when the slot is freed: handles to the node it held are stale from then on
    generation: u32,
    touched: i64,
    /// the frame and the key of the last duplicate of this key declared in it (`unique_key`)
    dup: (i64, i64),
    node: Node,
    text: String,
    /// where the node's look was painted: the layer's key, the instance range, the clip, and the
    /// frame
    range_layer: i64,
    range: (u32, u32),
    clip: Rect,
    clip_radius: f32,
    painted: i64,
    /// the view that declares the node, and the view the node holds (`NO_VIEW`)
    owner: u32,
    view: u32,
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
    /// the look and state last painted while some theme animates (what a transition starts from),
    /// the running transition (`NO_TWEEN`), and where `animate` left the node: offset and opacity
    seen: Look,
    seen_state: State,
    tween: u32,
    pose: (f32, f32, f32),
    /// the slot of the node it was declared under (`NONE` for the root); its accessible name and
    /// value (the text when the name is empty) and its text selection (anchor, caret in chars)
    parent_slot: usize,
    a11y_name: String,
    a11y_value: String,
    a11y_selection: (u32, u32),
    /// a numeric value (now, min, max, step), over the text one
    a11y_number: Option<[f64; 4]>,
}

impl Slot {
    fn fresh() -> Slot {
        Slot {
            key: 0,
            generation: 0,
            touched: -1,
            dup: (-1, 0),
            node: Node::DEFAULT,
            text: String::new(),
            range_layer: 0,
            range: (0, 0),
            clip: Rect::new(0.0, 0.0, 0.0, 0.0),
            clip_radius: 0.0,
            painted: -1,
            owner: 0,
            view: NO_VIEW,
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
            seen: Look::NONE,
            seen_state: State::default(),
            tween: NO_TWEEN,
            pose: (0.0, 0.0, 1.0),
            parent_slot: NONE,
            a11y_name: String::new(),
            a11y_value: String::new(),
            a11y_number: None,
            a11y_selection: (0, 0),
        }
    }
}

/// A hit rectangle in the coordinates of view `view`'s instances: its entry moves and clips it.
#[derive(Clone, Copy)]
struct HitRec {
    key: i64,
    parent: i64,
    flags: Flags,
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    view: u32,
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
    /// the fingers' sideways points (a `SCROLL_X` box)
    pub scroll_drag_x: f32,
    /// a value an assistive technology set (`A11yAction` SetValue)
    pub a11y_value: Option<f64>,
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
/// base). Image instances sample `images[break.image]` from each break on. A layer is retained: a
/// frame rewrites only the instances that changed, and a kept view's run stays where it is.
/// `dirty` gathers the `[start, end)` instance ranges written since the host last called
/// `Ui::drawn` for this layer; `id` names the layer's storage (stable while it lives, below 64), the
/// painter's retained layer index.
#[derive(Default)]
pub struct Layer {
    pub key: i64,
    pub id: u32,
    pub instances: Vec<Instance>,
    pub breaks: Vec<Break>,
    pub images: Vec<Image>,
    pub dirty: Vec<(u32, u32)>,
    pieces: Vec<Piece>,
    live: bool,
}

impl Layer {
    fn new(key: i64, id: u32) -> Layer {
        Layer { key, id, instances: Vec::with_capacity(256), breaks: Vec::new(), images: Vec::new(), dirty: Vec::new(), pieces: Vec::new(), live: false }
    }
}

/// Add `[a, b)` to sorted, disjoint `ranges`, merging what it touches.
// ponytail: past 64 ranges the list collapses to its bounding range (the painter rewrites the span).
fn add_range(ranges: &mut Vec<(u32, u32)>, a: u32, b: u32) {
    if a >= b {
        return;
    }
    if let Some(last) = ranges.last_mut() {
        if a >= last.0 && a <= last.1 {
            last.1 = last.1.max(b);
            return;
        }
    }
    let at = ranges.partition_point(|r| r.1 < a);
    let mut end = at;
    let (mut lo, mut hi) = (a, b);
    while end < ranges.len() && ranges[end].0 <= hi {
        lo = lo.min(ranges[end].0);
        hi = hi.max(ranges[end].1);
        end += 1;
    }
    ranges.drain(at..end);
    if ranges.len() >= 64 {
        let (first, last) = (ranges[0].0.min(lo), ranges[ranges.len() - 1].1.max(hi));
        ranges.clear();
        ranges.push((first, last));
    } else {
        ranges.insert(at, (lo, hi));
    }
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

/// The completed frame: its paint layers, the platform requests, what changed on screen since the
/// output before (`damage`, window points; `None` when nothing did) and the view table entries the
/// layers' instances name (the host sets each with `pxui_render::Painter::set_view`).
pub struct FrameOut<'a> {
    pub draw: &'a [Layer],
    pub platform: &'a PlatformOut,
    pub damage: Option<Rect>,
    pub views: &'a [(u16, ViewEntry)],
    pub blurs: &'a [BlurOut],
    /// the accessibility tree (empty unless `Ui::set_a11y(true)`): its `revision` changes only when
    /// its content did
    pub a11y: &'a A11yTree,
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
    /// `themed` overrides in effect (`themes[..theme_depth]`, storage kept), the distinct themes views
    /// were built under (an id is an index) and the id in effect
    themes: Vec<Theme>,
    theme_depth: usize,
    theme_ids: Vec<Theme>,
    theme_id: u32,
    /// running transitions, the frame's time, and whether a theme in effect animates
    tweens: Vec<Tween>,
    now: f64,
    motion: bool,
    font_size: i64,
    platform: Platform,
    /// the text engine; `Paint` holds it while a frame paints
    text: Option<Text>,
    table: SlotTable,
    slots: Vec<Slot>,
    free: Vec<usize>,
    /// slots whose scroll may be in motion (`apply_scroll` adds, `prune` drops the settled), and
    /// `prune`'s scratch
    moving: Vec<usize>,
    gone: Vec<usize>,
    scroll_y: Vec<f32>,
    // per-frame boxes
    count: usize,
    boxes: Vec<BoxRec>,
    l_x: Vec<f32>,
    l_y: Vec<f32>,
    l_w: Vec<f32>,
    l_h: Vec<f32>,
    l_content: Vec<f32>,
    l_content_w: Vec<f32>,
    l_gutter: Vec<f32>,
    /// the horizontal scroll of each retained slot (`SCROLL_X`)
    scroll_xs: Vec<f32>,
    /// boxes the last layout ran over; row flow direction
    arranged: usize,
    dir: Direction,
    /// the published accessibility tree, the next one being built (published when it differs), the
    /// slot-to-tree-index map of the walk, and the actions a bridge handed in for the next frame
    a11y: A11yTree,
    a11y_next: A11yTree,
    a11y_map: Vec<u32>,
    a11y_pending: Vec<A11yAction>,
    /// something the tree shows may have changed outside the root's build (a view built, moved or
    /// was patched, an action came in), and the focus and capture the last walk saw
    a11y_dirty: bool,
    a11y_on: bool,
    timing: bool,
    a11y_seen: (i64, Option<i64>),
    /// how long the last frame's layout passes took
    layout_time: std::time::Duration,
    phases: [std::time::Duration; PHASES],
    phase_at: Option<std::time::Instant>,
    /// `order_stack`'s scratch: (z, build position, box)
    z_order: Vec<(i16, u32, usize)>,
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
    pub(crate) text_colors_seen: Vec<(Tokens, String, Rc<Vec<(i64, i64, Color)>>)>,
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
    /// the frame number of the last paint: node ranges painted then index `draw`
    painted: i64,
    /// the scratch layer a patch paints one node into
    patch_layers: Vec<Layer>,
    /// views by id (0: the root); free ids and painter table indices; the table index's view
    views: Vec<ViewRec>,
    free_views: Vec<u32>,
    free_indices: Vec<u16>,
    index_owner: Vec<u32>,
    /// the view being built, and the view whose entry the hits being recorded follow
    view_cur: u32,
    paint_xf: u32,
    /// bumped by what changes every view's output (theme, font size, density, window size)
    epoch: u64,
    verify: bool,
    stamp: u64,
    /// hot, focus, keyboard focus and carrying as the last frame left them
    seen: (i64, i64, bool, bool),
    marks: Vec<i64>,
    damage: Option<Rect>,
    view_out: Vec<(u16, ViewEntry)>,
    blur_out: Vec<BlurOut>,
    view_prev: Vec<(u16, ViewEntry)>,
    /// (slot, scratch layer) of every node the frame painted, and the compaction buffer
    painted_nodes: Vec<(usize, usize)>,
    compact: Vec<Instance>,
    temp_breaks: Vec<Break>,
    temp_images: Vec<Image>,
    evictions: u64,
    /// canvas closures and formatted labels of the frame being built
    arena: Arena,
    painting: bool,
    frame_requested: bool,
    /// views built (not kept) this frame
    rebuilt: usize,
    /// the earliest `repaint_after` the root asked for this frame
    wake: f64,
    out: PlatformOut,
    destroyed: bool,
}

/// Bytes a UI holds: the glyph atlases' CPU mirrors, the text layouts' glyph lists, and the text undo
/// stacks and shelf.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Bytes {
    pub atlas: i64,
    pub glyph_lists: i64,
    pub text_undo: i64,
}

/// One text left behind: its key, value and its undo and redo stacks.
pub type ShelfEntry = (i64, String, Vec<(String, usize, usize)>, Vec<(String, usize, usize)>);



pub fn contains(rect: Rect, point: (f32, f32)) -> bool { rect.contains(point) }

pub fn intersect(a: Rect, b: Rect) -> Rect { a.intersect(b) }

fn rgba(c: Color) -> [u8; 4] {
    [c.r, c.g, c.b, c.a]
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
            theme: Theme::default(),
            themes: Vec::new(),
            theme_depth: 0,
            theme_ids: Vec::new(),
            theme_id: 0,
            tweens: Vec::new(),
            now: 0.0,
            motion: false,
            font_size,
            platform,
            text: Some(text),
            table: SlotTable::create(),
            slots: Vec::new(),
            free: Vec::new(),
            moving: Vec::new(),
            gone: Vec::new(),
            scroll_y: Vec::new(),
            count: 0,
            boxes: Vec::new(),
            l_x: Vec::new(),
            l_y: Vec::new(),
            l_w: Vec::new(),
            l_h: Vec::new(),
            l_content: Vec::new(),
            l_content_w: Vec::new(),
            l_gutter: Vec::new(),
            scroll_xs: Vec::new(),
            arranged: 0,
            dir: Direction::Ltr,
            z_order: Vec::new(),
            layout_time: std::time::Duration::ZERO,
            phases: [std::time::Duration::ZERO; PHASES],
            phase_at: None,
            a11y: A11yTree::new(),
            a11y_next: A11yTree::new(),
            a11y_map: Vec::new(),
            a11y_pending: Vec::new(),
            a11y_dirty: true,
            a11y_on: false,
            timing: false,
            a11y_seen: (0, None),
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
            painted: -1,
            patch_layers: Vec::new(),
            views: vec![ViewRec::new()],
            free_views: Vec::new(),
            free_indices: (1..=255u16).rev().collect(),
            index_owner: vec![0; 256],
            view_cur: 0,
            paint_xf: 0,
            epoch: 0,
            verify: VERIFY_NEW_VIEWS.load(std::sync::atomic::Ordering::Relaxed),
            stamp: 0,
            seen: (0, 0, false, false),
            marks: Vec::new(),
            damage: None,
            view_out: Vec::new(),
            blur_out: Vec::new(),
            view_prev: Vec::new(),
            painted_nodes: Vec::new(),
            compact: Vec::new(),
            temp_breaks: Vec::new(),
            temp_images: Vec::new(),
            evictions: 0,
            arena: Arena::default(),
            painting: false,
            frame_requested: false,
            rebuilt: 0,
            wake: f64::INFINITY,
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
            self.arena.reset();
            for slot in &mut self.slots { slot.labels.clear(); }
            self.count = 0;
        }
    }

    /// The theme in effect: the UI's own, or the innermost `themed` override while building in one.
    pub fn theme(&self) -> &Theme {
        if self.theme_depth > 0 { &self.themes[self.theme_depth - 1] } else { &self.theme }
    }

    /// The theme in effect's tokens, by value: what a canvas closure captures.
    pub fn tokens(&self) -> Tokens {
        self.theme().tokens
    }

    /// Swap the UI's theme (light, dark, high contrast, an accent: each is a `Theme` value). A change
    /// rebuilds every view. Between frames, or inside one for what is built after it.
    // ponytail: a swap snaps; transitioning every node's look to the new theme's needs the swap to
    // re-resolve each node's state against the new States (a rebuild already does) plus a tween per
    // node; add when a host wants animated theme swaps.
    pub fn set_theme(&mut self, theme: Theme) {
        if theme != self.theme {
            self.epoch += 1;
            self.theme = theme;
        }
    }

    /// Build `f` under `theme` (a scoped override: what `f` builds reads `theme()` as `theme`). A view
    /// built inside rebuilds when the override it was built under changes.
    pub fn themed(&mut self, theme: &Theme, f: &mut dyn FnMut(&mut Ui)) {
        let saved = self.theme_id;
        // ponytail: ids by linear search over the distinct themes seen (at most 32, the last slot reused
        // past that); a theme revision supplied by the caller if apps juggle many themes.
        self.theme_id = match self.theme_ids.iter().position(|t| t == theme) {
            Some(i) => i as u32 + 1,
            None => {
                if self.theme_ids.len() >= 32 {
                    self.theme_ids.pop();
                    self.epoch += 1;
                }
                self.theme_ids.push(*theme);
                self.theme_ids.len() as u32
            }
        };
        self.motion |= theme.tokens.moves();
        if self.theme_depth == self.themes.len() {
            self.themes.push(*theme);
        } else {
            self.themes[self.theme_depth] = *theme;
        }
        self.theme_depth += 1;
        f(self);
        self.theme_depth -= 1;
        self.theme_id = saved;
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
        if size != self.font_size {
            self.epoch += 1;
        }
        self.font_size = size;
        self.kit_row_height = 24.max(size + 11);
    }

    /// The most recently completed frame.
    pub fn output(&self) -> FrameOut<'_> {
        FrameOut { draw: &self.draw[..self.draw_len], platform: &self.out, damage: self.damage, views: &self.view_out, blurs: &self.blur_out, a11y: &self.a11y }
    }

    /// The host drew layer `index` of the output with its `dirty` ranges: they start over empty.
    pub fn drawn(&mut self, index: usize) {
        if let Some(layer) = self.draw[..self.draw_len].get_mut(index) {
            layer.dirty.clear();
        }
    }

    /// Nodes declared by the last build (a kept view counts as its one box): what a view or a
    /// virtual list saves.
    pub fn built_nodes(&self) -> usize {
        self.count
    }

    /// Views (`Ui::view`) the last frame built rather than kept; the root is not counted.
    pub fn rebuilt_views(&self) -> usize {
        self.rebuilt
    }

    /// Declare what a node built this frame is to a screen reader, and the states its widget knows
    /// (`a11y::state` bits).
    pub fn set_role(&mut self, b: &Handle, role: Role, states: u32) {
        if self.live(b) {
            let n = &mut self.slots[b.box_slot].node;
            (n.role, n.a11y) = (role, states);
        }
    }

    /// The node's accessible name (its text by default), for a node declared this frame.
    pub fn a11y_name(&mut self, b: &Handle, name: &str) {
        if self.live(b) {
            let s = &mut self.slots[b.box_slot].a11y_name;
            s.clear();
            s.push_str(name);
        }
    }

    /// The node's accessible value (a field's text, a readout) and its selection (anchor and caret
    /// in chars).
    pub fn a11y_value(&mut self, b: &Handle, value: &str, selection: (u32, u32)) {
        if self.live(b) {
            let s = &mut self.slots[b.box_slot];
            s.a11y_value.clear();
            s.a11y_value.push_str(value);
            s.a11y_selection = selection;
        }
    }

    /// The node's accessible value as a number in `min..=max` moved by `step` (`a11y::Value::Number`:
    /// a slider, a determinate progress bar); it wins over a text value.
    pub fn a11y_number(&mut self, b: &Handle, now: f64, min: f64, max: f64, step: f64) {
        if self.live(b) {
            self.slots[b.box_slot].a11y_number = Some([now, min, max, step]);
        }
    }

    /// Actions from an assistive technology (`pxui_atspi::Bridge::pump`), applied at the start of
    /// the next frame as input: Press clicks, Focus focuses, Scroll scrolls by `payload` points,
    /// SetValue hands `payload` to the widget (`Signal::a11y_value`), Expand and Collapse click a
    /// disclosure whose state differs. Ids of nodes that are gone are dropped.
    pub fn inject_a11y(&mut self, actions: &[A11yAction]) {
        self.a11y_pending.extend_from_slice(actions);
    }

    fn apply_a11y(&mut self) {
        let mut pending = std::mem::take(&mut self.a11y_pending);
        for a in &pending {
            let (slot, generation) = ((a.id & 0xffff_ffff) as usize, (a.id >> 32) as u32);
            let Some(s) = self.slots.get(slot).filter(|s| s.generation == generation && s.key != 0) else { continue };
            let (key, expanded) = (s.key, s.node.a11y & a11y_state::EXPANDED != 0);
            match a.action {
                A11yKind::Press => self.accumulator(key).clicked = true,
                A11yKind::Focus => {
                    let b = UiBox { index: NONE, box_key: key, box_slot: slot, generation };
                    self.focus_box(&b);
                }
                A11yKind::Scroll => self.scroll_y[slot] = (self.scroll_y[slot] + a.payload as f32).max(0.0),
                A11yKind::SetValue => self.accumulator(key).a11y_value = Some(a.payload),
                A11yKind::Expand | A11yKind::Collapse => {
                    if expanded != (a.action == A11yKind::Expand) {
                        self.accumulator(key).clicked = true;
                    }
                }
            }
            self.mark_key(key);
        }
        self.a11y_dirty |= !pending.is_empty();
        pending.clear();
        self.a11y_pending = pending;
    }

    /// Build the accessibility tree (`FrameOut::a11y`) from this frame on. Off by default, so a UI
    /// nobody reads pays nothing for it; a host turns it on while an assistive technology listens
    /// (the AT-SPI bridge connected and a client is present). Off publishes an empty tree.
    pub fn set_a11y(&mut self, on: bool) {
        if on != self.a11y_on {
            self.a11y_on = on;
            self.a11y_dirty = true;
            if !on {
                (self.a11y, self.a11y_next) = (A11yTree::new(), A11yTree::new());
            }
        }
    }

    /// Walk the views' declared nodes (a kept view's from its last build) into the next tree and
    /// publish it when it differs: an unchanged frame leaves the published tree and its revision.
    // ponytail: a frame that rebuilds anything (or whose root declares roles, as the kit without
    // views does) walks and compares the whole tree, O(nodes); per-view subtrees if trees get large.
    fn emit_a11y(&mut self) {
        if !self.a11y_on {
            return;
        }
        // nothing a kept view shows changed and the root declared no role: the tree stands
        let seen = (self.focus, self.active);
        if !self.a11y_dirty && seen == self.a11y_seen && self.views[0].nodes.iter().all(|s| self.slots[*s].node.role == Role::Unknown) {
            return;
        }
        (self.a11y_dirty, self.a11y_seen) = (false, seen);
        let mut t = std::mem::replace(&mut self.a11y_next, A11yTree::new());
        let mut map = std::mem::take(&mut self.a11y_map);
        t.begin();
        if map.len() < self.slots.len() {
            map.resize(self.slots.len(), NO_PARENT);
        }
        self.emit_view(&mut t, &mut map, 0);
        if t.nodes != self.a11y.nodes || t.strings != self.a11y.strings {
            t.revision = self.a11y.revision;
            t.end();
            std::mem::swap(&mut t, &mut self.a11y);
        }
        self.a11y_next = t;
        self.a11y_map = map;
    }

    fn emit_view(&mut self, t: &mut A11yTree, map: &mut [u32], v: u32) {
        for k in 0..self.views[v as usize].nodes.len() {
            let slot = self.views[v as usize].nodes[k];
            let s = &self.slots[slot];
            let parent = if s.parent_slot == NONE { NO_PARENT } else { map[s.parent_slot] };
            map[slot] = parent;
            if s.node.role != Role::Unknown {
                let n = &s.node;
                let shift = self.views[self.views[s.owner as usize].xf as usize].shift;
                let rect = Rect::new(s.rx + shift.0, s.ry + shift.1, s.rw, s.rh);
                let shown = Rect::new(s.rx, s.ry, s.rw, s.rh).intersect(s.clip);
                let mut states = n.a11y;
                let mut acts = 0;
                if !n.state.disabled {
                    states |= a11y_state::ENABLED;
                }
                if shown.w > 0.0 && shown.h > 0.0 {
                    states |= a11y_state::VISIBLE;
                }
                if n.flags & FOCUSABLE != 0 {
                    states |= a11y_state::FOCUSABLE;
                    acts |= a11y_actions::FOCUS;
                }
                if s.key == self.focus {
                    states |= a11y_state::FOCUSED;
                }
                if self.active == Some(s.key) {
                    states |= a11y_state::PRESSED;
                }
                if n.flags & CLICKABLE != 0 {
                    acts |= a11y_actions::PRESS;
                }
                if n.flags & (SCROLL | SCROLL_X) != 0 {
                    acts |= a11y_actions::SCROLL;
                }
                if n.a11y & a11y_state::EDITABLE != 0 || n.role == Role::Slider {
                    acts |= a11y_actions::SET_VALUE;
                }
                if n.a11y & a11y_state::EXPANDABLE != 0 {
                    acts |= a11y_actions::EXPAND | a11y_actions::COLLAPSE;
                }
                let i = t.push(parent, crate::a11y::node_id(slot, s.generation), n.role, rect);
                t.set_name(i, if s.a11y_name.is_empty() { &s.text } else { &s.a11y_name });
                if let Some([now, min, max, step]) = s.a11y_number {
                    t.set_value_number(i, now, min, max, step);
                } else if !s.a11y_value.is_empty() || n.a11y & a11y_state::EDITABLE != 0 {
                    t.set_value(i, &s.a11y_value);
                    t.set_selection(i, s.a11y_selection.0, s.a11y_selection.1);
                }
                t.set_states(i, states);
                t.set_actions(i, acts);
                map[slot] = i;
            }
            let held = self.slots[slot].view;
            if held != NO_VIEW && held != v {
                self.emit_view(t, map, held);
            }
        }
    }

    /// Time the frame's phases from the next frame on (`phase_times`, `layout_time`): a clock read
    /// per phase, off by default.
    pub fn set_timing(&mut self, on: bool) {
        self.timing = on;
        self.phases = [std::time::Duration::ZERO; PHASES];
    }

    /// The last frame's time in each of its phases, in `PHASE_NAMES` order, while `set_timing` is
    /// on: begin (input routing), build, intrinsic, arrange, paint, a11y, finish.
    pub fn phase_times(&self) -> [std::time::Duration; PHASES] {
        self.phases
    }

    fn phase(&mut self, i: usize) {
        if let Some(at) = self.phase_at {
            let now = std::time::Instant::now();
            self.phases[i] = now - at;
            self.phase_at = Some(now);
        }
    }

    /// How long the last frame's layout (`intrinsic` and `arrange`) took, while `set_timing` is on.
    pub fn layout_time(&self) -> std::time::Duration {
        self.layout_time
    }

    /// Nodes the last frame's layout ran over: a kept view's are not among them.
    pub fn arranged_nodes(&self) -> usize {
        self.arranged
    }

    /// The row flow direction (`Rtl` mirrors flow children); every view rebuilds when it changes.
    pub fn set_dir(&mut self, dir: Direction) {
        if dir != self.dir {
            self.dir = dir;
            self.epoch += 1;
        }
    }

    pub fn dir(&self) -> Direction {
        self.dir
    }

    /// `verify_views(on)` for every `Ui` created from now on: a test harness turns it on once, for the
    /// UIs its code under test creates.
    pub fn verify_new_views(on: bool) {
        VERIFY_NEW_VIEWS.store(on, std::sync::atomic::Ordering::Relaxed);
    }

    /// Re-run every clean view's build and panic, naming the view, when its output differs from the
    /// kept one (a revision the app forgot to bump). For tests: it builds every view every frame.
    pub fn verify_views(&mut self, on: bool) {
        self.verify = on;
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

    /// The window's width in points: responsive layout is `if ui.width() < 600.0 { .. }`.
    pub fn width(&self) -> f32 {
        self.view_w
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
            let (all, lists) = self.text.as_ref().map_or((0, 0), |t| (t.bytes(), t.glyph_list_bytes()));
            Bytes { atlas: (all - lists) as i64, glyph_lists: lists as i64, text_undo: self.text_undo_held }
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

    fn slot_of(&mut self, key: i64, found: Option<usize>) -> usize {
        if let Some(slot) = found {
            return slot;
        }
        let slot = match self.free.pop() {
            Some(slot) => slot,
            None => {
                self.slots.push(Slot::fresh());
                self.scroll_y.push(0.0);
                self.scroll_xs.push(0.0);
                self.slots.len() - 1
            }
        };
        let s = &mut self.slots[slot];
        // a reused slot keeps its generation and its text's storage
        let (generation, mut text) = (s.generation, std::mem::take(&mut s.text));
        text.clear();
        *s = Slot::fresh();
        s.generation = generation;
        s.text = text;
        s.key = key;
        s.touched = self.frame_number;
        self.scroll_y[slot] = 0.0;
        self.scroll_xs[slot] = 0.0;
        self.table.add(key, slot);
        slot
    }

    /// `v` or a view around it kept its last output this frame (its nodes live on undeclared).
    fn kept_now(&self, mut v: u32) -> bool {
        while v != 0 {
            let r = &self.views[v as usize];
            if r.kept == self.frame_number {
                return true;
            }
            v = r.parent;
        }
        false
    }

    /// Free the nodes this frame did not declare, unless a kept view holds them: the nodes the
    /// views built this frame (the root among them) declared in their build before and not now,
    /// and with a view among those, every node it held. O(views + nodes built), not O(nodes).
    fn prune(&mut self) {
        for index in 0..self.count {
            let s = &mut self.slots[self.boxes[index].slot];
            s.labels.truncate(s.label_count);
        }
        // a scroll in motion moves its view's layout every frame
        let mut moving = std::mem::take(&mut self.moving);
        moving.retain(|slot| {
            let s = &self.slots[*slot];
            s.key != 0 && (s.scroll_mode != 0 || s.scroll_visual != 0.0)
        });
        for slot in &moving {
            self.mark_view(self.slots[*slot].owner);
        }
        self.moving = moving;
        let mut gone = std::mem::take(&mut self.gone);
        for v in 0..self.views.len() {
            if self.views[v].built == self.frame_number {
                gone.extend_from_slice(&self.views[v].prev);
            }
        }
        while let Some(slot) = gone.pop() {
            let s = &self.slots[slot];
            if s.key == 0 || s.touched == self.frame_number || self.kept_now(s.owner) {
                continue;
            }
            let view = std::mem::replace(&mut self.slots[slot].view, NO_VIEW);
            if view != NO_VIEW {
                let r = &self.views[view as usize];
                gone.extend_from_slice(&r.nodes);
                gone.extend_from_slice(&r.prev);
                self.free_view(view);
            }
            self.free_slot(slot);
        }
        self.gone = gone;
    }

    fn free_slot(&mut self, slot: usize) {
        let key = self.slots[slot].key;
        self.table.remove(key);
        let s = &mut self.slots[slot];
        s.key = 0;
        s.generation = s.generation.wrapping_add(1);
        s.text_value = None;
        s.labels.clear();
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
                    scroll_drag_x: 0.0,
                    a11y_value: None,
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
        let v = &self.views[h.view as usize];
        if !v.outer.contains(point) {
            return false;
        }
        let local = if v.plain() { Some(point) } else { v.entry().unmap(point) };
        local.is_some_and(|p| contains(Rect::new(h.x, h.y, h.w, h.h), (p.0 - v.shift.0, p.1 - v.shift.1)))
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
                if flags & (SCROLL | SCROLL_X) != 0 {
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
        self.frame_events.clone_from(&frame.input.events);
        keep_frame(&mut self.input_frame, frame);
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
        self.previous_keys.clone_from(&frame.input.keys);
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
                        // the nearest focusable box at or around the press takes the focus unless
                        // it is a Tab stop only (buttons, as on macOS); a list takes it from its rows
                        let mut holder = 0;
                        if flags & KEEP_FOCUS == 0 {
                            self.hit_ancestors(target, &mut |key| {
                                if holder == 0 && self.flags_of_key(key) & FOCUSABLE != 0 {
                                    holder = key;
                                }
                            });
                        }
                        let focus = if flags & KEEP_FOCUS != 0 {
                            self.focus
                        } else if holder != 0 && self.flags_of_key(holder) & TAB_ONLY == 0 {
                            holder
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
                Event::TrackpadScrolled { delta: (horizontal, vertical), phase, time } => {
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
                        value.scroll_drag_x += *horizontal as f32;
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

    /// When the last frame's UI needs another frame with no input: at once for a gesture, a
    /// running transition, a scroll in motion or a requested frame; at the end of a running hover
    /// delay or the earliest `repaint_after`; `None` when time alone changes nothing. Keyboard
    /// focus alone asks for nothing (no caret blinks).
    pub fn repaint_at(&self) -> Option<f64> {
        if self.frame_requested {
            Some(f64::NEG_INFINITY)
        } else {
            // a `repaint_after` made between frames
            Some(self.out.repaint_at.unwrap_or(f64::INFINITY).min(self.wake)).filter(|t| t.is_finite())
        }
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
        self.escape();
    }

    /// The view being built did what a skipped build would lose (a box at the root, a frame request,
    /// a hover delay): it rebuilds next frame too.
    pub(crate) fn escape(&mut self) {
        if self.building && self.view_cur != 0 {
            self.views[self.view_cur as usize].escapes = true;
        }
    }

    /// Ask for a frame `seconds` after this frame's time (a timer: a toast's expiry, a delayed
    /// reveal), building the view that asks again then; `repaint_at_time` takes the frame clock's
    /// time. A request stands until its time (a view's until the view rebuilds); the earliest is
    /// `repaint_at`.
    pub fn repaint_after(&mut self, seconds: f64) {
        self.repaint_at_time(self.now + seconds.max(0.0));
    }

    /// The time of the frame being built (the last frame's between frames), in the frame clock's
    /// seconds.
    pub fn now(&self) -> f64 {
        self.now
    }

    /// `repaint_after` at the frame clock's `time`.
    pub fn repaint_at_time(&mut self, time: f64) {
        if self.building && self.view_cur != 0 {
            let r = &mut self.views[self.view_cur as usize];
            r.wake = r.wake.min(time);
        } else {
            self.wake = self.wake.min(time);
        }
    }

    fn next_repaint(&self, now: f64) -> Option<f64> {
        if self.frame_requested || self.active.is_some() || !self.tweens.is_empty() || !self.moving.is_empty() {
            return Some(now);
        }
        let wake = self.views.iter().skip(1).fold(self.wake, |w, r| w.min(r.wake));
        let hover = match &self.hover_rest {
            Some((_, _, start, seen)) if *seen == self.frame_number && now - start < HOVER_SECONDS => start + HOVER_SECONDS,
            _ => f64::INFINITY,
        };
        Some(wake.min(hover)).filter(|t| t.is_finite())
    }
}

/// `label` cut to `limit` points wide with an ellipsis, on a character boundary. `width` measures
/// text as the sum of its characters, so one pass from the start measures each character once and
/// stops at the first that does not fit: the cost follows what is shown, not the label's length.
pub const PHASES: usize = 7;
pub const PHASE_NAMES: [&str; PHASES] = ["begin", "build", "intrinsic", "arrange", "paint", "a11y", "finish"];

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

/// What `enter`, `scope` and `enter_root` change; `leave` puts it back. Each only pushes (a root
/// pushes the root box), so the depths restore it with no allocation.
pub enum Saved {
    Depths(usize, usize),
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
    /// a value an assistive technology set this frame: the widget applies it as one edit
    pub a11y_value: Option<f64>,
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

    /// Two boxes with one key in a frame get distinct, order-stable keys: the n-th takes the key's
    /// n-th `#dup` hash. The first key's slot remembers the last duplicate taken this frame, so the
    /// n-th starts there (n repeated labels cost n lookups, not n squared). With the key, its slot
    /// when it has one.
    fn unique_key(&mut self, first: i64) -> (i64, Option<usize>) {
        let (mut key, mut head) = (first, None);
        loop {
            match self.table.find(key) {
                Some(slot) if self.slots[slot].touched == self.frame_number => {
                    let dup = self.slots[slot].dup;
                    if head.is_none() {
                        head = Some(slot);
                        if dup.0 == self.frame_number {
                            key = dup.1;
                            continue;
                        }
                    }
                    key = hash_range(key, "#dup", 0, 4);
                }
                found => {
                    if let Some(head) = head {
                        self.slots[head].dup = (self.frame_number, key);
                    }
                    return (key, found);
                }
            }
        }
    }

    fn append_box(&mut self, key: i64, found: Option<usize>, parent: usize) -> usize {
        let index = self.count;
        if index == self.boxes.len() {
            self.boxes.push(BoxRec::blank());
            self.l_x.push(0.0);
            self.l_y.push(0.0);
            self.l_w.push(0.0);
            self.l_h.push(0.0);
            self.l_content.push(0.0);
            self.l_content_w.push(0.0);
            self.l_gutter.push(0.0);
        }
        self.count = index + 1;
        let slot = self.slot_of(key, found);
        self.slots[slot].touched = self.frame_number;
        self.slots[slot].label_count = 0;
        self.slots[slot].owner = self.view_cur;
        self.slots[slot].parent_slot = if parent == NONE { NONE } else { self.boxes[parent].slot };
        self.views[self.view_cur as usize].nodes.push(slot);
        let b = &mut self.boxes[index];
        b.key = key;
        b.slot = slot;
        b.view = NO_VIEW;
        b.kept = false;
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

    /// Declare build box `index`: its node record takes `node`, and its text, hit shape and canvases
    /// start empty. A patch made since the node's last build ends here.
    fn set_box(&mut self, index: usize, node: Node) {
        let b = &mut self.boxes[index];
        b.hit = None;
        b.hit_rect = None;
        b.paint = arena::EMPTY;
        b.over = arena::EMPTY;
        let s = &mut self.slots[b.slot];
        s.node = node;
        s.text.clear();
        s.a11y_name.clear();
        s.a11y_value.clear();
        s.a11y_selection = (0, 0);
        s.a11y_number = None;
    }

    /// The layout inputs and look of build box `index`.
    fn nd(&self, index: usize) -> &Node {
        &self.slots[self.boxes[index].slot].node
    }

    /// Declare a node under the current parent by its key.
    pub fn node_keyed(&mut self, key: i64, node: Node) -> Handle {
        self.require_building();
        let (key, found) = self.unique_key(key);
        let parent = self.current_parent();
        let index = self.append_box(key, found, parent);
        self.set_box(index, node);
        let slot = self.boxes[index].slot;
        UiBox { index, box_key: key, box_slot: slot, generation: self.slots[slot].generation }
    }

    /// Declare a node under the current parent, keyed by `label` (see `key_of`).
    pub fn node(&mut self, label: &str, node: Node) -> Handle {
        self.require_building();
        let key = key_of(self.current_seed(), label);
        self.node_keyed(key, node)
    }

    pub fn box_keyed(&mut self, key: i64, opts: BoxOpts) -> UiBox {
        let BoxOpts { flags, w, h, max_h, axis, padding, at, scroll_step, hit, hit_rect } = opts;
        let b = self.node_keyed(key, Node { flags, w, h, max_h, axis, padding: Edges::all(padding), at, scroll_step, ..Node::DEFAULT });
        self.boxes[b.index].hit = hit;
        self.boxes[b.index].hit_rect = hit_rect;
        b
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

    /// `box_` of a label formatted into the frame arena: keys hash the text, none keeps it.
    pub fn box_fmt(&mut self, label: std::fmt::Arguments, opts: BoxOpts) -> UiBox {
        match label.as_str() {
            Some(text) => self.box_(text, opts),
            None => {
                self.require_building();
                let range = self.arena.fmt(label);
                let key = key_of(self.current_seed(), self.arena.str(range));
                self.box_keyed(key, opts)
            }
        }
    }

    pub fn set_at(&mut self, b: &UiBox, at: (f32, f32)) {
        self.require_building();
        self.slots[b.box_slot].node.at = Some(at);
    }

    /// Give a node declared this frame the look `look`, picked for `state` (what `node` takes, set once
    /// its signal is known).
    pub fn set_look(&mut self, b: &Handle, look: Look, state: State) {
        if self.live(b) {
            let s = &self.slots[b.box_slot];
            // a node shown last frame in another state moves to this look over the theme's transition
            let transition = if self.motion && s.seen_state != state && self.range_valid(b.box_slot) {
                Some(self.theme().tokens.transition(s.seen_state, state)).filter(|c| !c.instant())
            } else {
                None
            };
            let n = &mut self.slots[b.box_slot].node;
            (n.look, n.state, n.transition) = (look, state, transition);
        }
    }

    /// The record of a node declared this frame, to finish declaring it once its signal is known.
    pub fn node_mut(&mut self, b: &Handle) -> &mut Node {
        assert!(self.live(b), "Ui.node_mut: a stale handle");
        &mut self.slots[b.box_slot].node
    }

    /// `set_look` with the look `states` picks for `state`.
    pub fn style(&mut self, b: &Handle, states: &States, state: State) {
        self.set_look(b, *states.pick(state), state);
    }

    /// What the pointer and keyboard do to the node now: hovered, pressed (it holds the capture),
    /// focused. A widget adds `checked` and `disabled`.
    pub fn node_state(&self, b: &Handle) -> State {
        let key = if self.live(b) { b.box_key } else { 0 };
        State { hovered: key != 0 && self.hot == key, pressed: key != 0 && self.active == Some(key), focused: key != 0 && self.focus == key, ..State::default() }
    }

    /// The handle still names the node it was made for (that node has not gone since).
    pub fn live(&self, b: &Handle) -> bool {
        self.slots.get(b.box_slot).is_some_and(|s| s.generation == b.generation && s.key == b.box_key && s.key != 0)
    }

    /// Set the node's text (painted in its look's text colour and font).
    pub fn text(&mut self, b: &Handle, text: &str) {
        if self.live(b) {
            let s = &mut self.slots[b.box_slot].text;
            if s != text {
                s.clear();
                s.push_str(text);
            }
        }
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
        let Saved::Depths(parents, seeds) = saved;
        self.parents.truncate(parents);
        self.seeds.truncate(seeds);
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
        self.escape();
        let saved = Saved::Depths(self.parents.len(), self.seeds.len());
        self.parents.push(0);
        self.seeds.push(ROOT_KEY);
        saved
    }

    pub fn box_of_key(&self, key: i64) -> Option<UiBox> {
        self.table.find(key).map(|slot| UiBox { index: NONE, box_key: key, box_slot: slot, generation: self.slots[slot].generation })
    }

    /// The box's screen rectangle from the previous frame (empty for a stale handle).
    pub fn rect(&self, b: &UiBox) -> Rect {
        if !self.live(b) {
            return Rect::new(0.0, 0.0, 0.0, 0.0);
        }
        let s = &self.slots[b.box_slot];
        let shift = self.views[self.views[s.owner as usize].xf as usize].shift;
        Rect::new(s.rx + shift.0, s.ry + shift.1, s.rw, s.rh)
    }

    /// What input did to the node this frame; a stale handle has no signal.
    pub fn signal(&self, b: &UiBox) -> Signal {
        let key = if self.live(b) { b.box_key } else { 0 };
        let held = key != 0 && self.active == Some(key);
        let slot = &self.slots[b.box_slot];
        match self.signals.get(&key).filter(|_| key != 0) {
            None => Signal {
                hovered: key != 0 && self.hot == key,
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
                a11y_value: None,
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
                a11y_value: value.a11y_value,
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

    /// The horizontal scroll of a `SCROLL_X` box.
    pub fn scroll_offset_x(&self, b: &UiBox) -> f32 {
        self.scroll_xs[b.box_slot]
    }

    pub fn set_scroll_offset_x(&mut self, b: &UiBox, value: f32) {
        self.scroll_xs[b.box_slot] = value;
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
    pub(crate) fn cached_hit(&mut self, b: &UiBox, key: u64, make: &mut dyn FnMut() -> HitFn) {
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

    /// Free-form drawing with the node's final laid-out rectangle, after its look and before its
    /// children. The closure lives in the frame arena: it runs once after layout and is dropped when
    /// painting ends.
    pub fn canvas<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, f: F) {
        assert!(b.index < self.count && self.boxes[b.index].slot == b.box_slot, "Ui.canvas: the node was not built this frame");
        self.arena.add(&mut self.boxes[b.index].paint, f);
    }

    /// A canvas that draws after the node's children, outside its content clip.
    pub fn canvas_over<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, f: F) {
        assert!(b.index < self.count && self.boxes[b.index].slot == b.box_slot, "Ui.canvas_over: the node was not built this frame");
        self.arena.add(&mut self.boxes[b.index].over, f);
    }

    /// `canvas` under its earlier name.
    pub fn draw<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, f: F) {
        self.canvas(b, f);
    }

    /// `canvas_over` under its earlier name.
    pub fn draw_over<F: Fn(&mut Paint, Rect) + 'static>(&mut self, b: &UiBox, f: F) {
        self.canvas_over(b, f);
    }

    /// Raise a root box and its children above ordinary boxes, below modal popups. Higher `order`
    /// values paint later; equal values retain their order of calls.
    pub fn to_front(&mut self, order: i64, b: &UiBox) {
        self.require_building();
        if self.boxes[b.index].parent != 0 {
            panic!("Ui.to_front: expected a root box");
        }
        self.escape();
        self.foreground.push((order, b.index));
    }



    pub(crate) fn set_box_h(&mut self, b: &UiBox, h: Size) {
        self.slots[b.box_slot].node.h = h;
    }

    /// The last child built under the current parent (`NONE` when it has none yet).
    pub(crate) fn last_child_of_parent(&self) -> usize {
        self.boxes[self.current_parent()].last
    }

    pub(crate) fn box_w_px(&self, index: usize) -> Option<f32> {
        match self.nd(index).w {
            Size::Px(w) => Some(w),
            _ => None,
        }
    }

    pub(crate) fn node_at(&self, index: usize) -> &Node {
        self.nd(index)
    }

    pub(crate) fn box_parent(&self, index: usize) -> usize {
        self.boxes[index].parent
    }

    pub(crate) fn box_count(&self) -> usize {
        self.count
    }

    pub(crate) fn push_overlay(&mut self, index: usize) {
        self.escape();
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
        let n = self.nd(index);
        n.at.is_none() && n.anchor.is_none()
    }

    /// The first flow child at or after `child` in its parent's list.
    fn next_flow(&self, mut child: usize) -> usize {
        while child != NONE && !self.flow(child) {
            child = self.boxes[child].next;
        }
        child
    }

    /// Bottom-up: fixed and fit sizes. Children follow their parent in build order, so a reverse
    /// sweep sees every child first.
    fn intrinsic(&mut self) {
        for index in (0..self.count).rev() {
            if self.nd(index).axis == Axis::Stack {
                self.order_stack(index);
            }
            let n = self.nd(index);
            let (content_w, content_h) = match n.grid {
                Some(grid) => {
                    let (cols, nc, rows, nr) = self.grid_tracks(index, n, &grid, None);
                    (span(&cols[..nc], n.gap), span(&rows[..nr], n.row_gap))
                }
                None => self.flow_extent(index, n),
            };
            let pad = n.padding;
            let (cw, ch) = (content_w + pad.l + pad.r, content_h + pad.t + pad.b);
            let resolve = |kind: &Size, content: f32| match kind {
                Size::Px(value) => *value,
                Size::Fit => content,
                _ => 0.0,
            };
            let (mut w, mut h) = (resolve(&n.w, cw), resolve(&n.h, ch));
            if n.aspect > 0.0 {
                match (n.w, n.h) {
                    (Size::Px(_), Size::Fit) => h = w / n.aspect,
                    (Size::Fit, Size::Px(_)) => w = h * n.aspect,
                    _ => {}
                }
            }
            let (w, h) = (clamp(w, n.min_w, n.max_w), clamp(h, n.min_h, n.max_h));
            (self.l_content[index], self.l_content_w[index]) = (ch, cw);
            (self.l_w[index], self.l_h[index]) = (w, h);
        }
    }

    /// The extent of a row, column or stack's flow children (margins and gaps in, padding out). A
    /// wrapping flow breaks its lines at its fixed main size.
    // ponytail: a wrapping flow whose main size comes from its parent measures as one line (its Fit
    // cross size is that line's); give it a Px main size, or measure from the laid-out width, if a
    // wrapping Fit box nests in a Grow one.
    fn flow_extent(&self, index: usize, n: &Node) -> (f32, f32) {
        let row = n.axis == Axis::Row;
        let stack = n.axis == Axis::Stack;
        let limit = match (n.wrap, row, n.w, n.h) {
            (true, true, Size::Px(w), _) => w - n.padding.l - n.padding.r,
            (true, false, _, Size::Px(h)) => h - n.padding.t - n.padding.b,
            _ => f32::INFINITY,
        };
        let (mut along, mut across, mut line_main, mut line_cross, mut count) = (0.0f32, 0.0f32, 0.0f32, 0.0f32, 0);
        let mut child = self.next_flow(self.boxes[index].first);
        while child != NONE {
            let m = self.nd(child).margin;
            let (ow, oh) = (self.l_w[child] + m.l + m.r, self.l_h[child] + m.t + m.b);
            child = self.next_flow(self.boxes[child].next);
            if stack {
                (along, across) = (along.max(ow), across.max(oh));
                continue;
            }
            let (cm, cc) = if row { (ow, oh) } else { (oh, ow) };
            if count > 0 && line_main + n.gap + cm > limit {
                along = along.max(line_main);
                across += line_cross + n.row_gap;
                (line_main, line_cross, count) = (0.0, 0.0, 0);
            }
            line_main += if count > 0 { n.gap } else { 0.0 } + cm;
            line_cross = line_cross.max(cc);
            count += 1;
        }
        if stack {
            return (along, across);
        }
        along = along.max(line_main);
        across += line_cross;
        if row { (along, across) } else { (across, along) }
    }

    /// A stack's children in `z` order (stable): paint and hit order follow the list. Sorted in a
    /// scratch vector kept between frames (no allocation once warm), O(k log k).
    fn order_stack(&mut self, index: usize) {
        let mut order = std::mem::take(&mut self.z_order);
        order.clear();
        let mut child = self.boxes[index].first;
        while child != NONE {
            order.push((self.nd(child).z, order.len() as u32, child));
            child = self.boxes[child].next;
        }
        if order.iter().any(|o| o.0 != 0) {
            order.sort_unstable();
            for k in 0..order.len() {
                self.boxes[order[k].2].next = order.get(k + 1).map_or(NONE, |o| o.2);
            }
            self.boxes[index].first = order[0].2;
            self.boxes[index].last = order[order.len() - 1].2;
        }
        self.z_order = order;
    }

    /// The grid's column and row sizes: a content-sized track takes its largest single-span child
    /// (as does a fractional one when measuring, `room` None); with `room` (inner w, h) the
    /// fractional tracks share what the others leave.
    fn grid_tracks(&self, index: usize, n: &Node, g: &Grid, room: Option<(f32, f32)>) -> ([f32; MAX_TRACKS], usize, [f32; MAX_TRACKS], usize) {
        let nc = g.cols.len().clamp(1, MAX_TRACKS);
        let mut nr = g.rows.len().min(MAX_TRACKS);
        let (mut colc, mut rowc) = ([0.0f32; MAX_TRACKS], [0.0f32; MAX_TRACKS]);
        let mut cursor = (0, 0);
        let mut child = self.next_flow(self.boxes[index].first);
        while child != NONE {
            let cn = self.nd(child);
            let (c, r, cs, rs) = next_cell(cn.cell, &mut cursor, nc);
            nr = nr.max(r + rs);
            if cs == 1 {
                colc[c] = colc[c].max(self.l_w[child] + cn.margin.l + cn.margin.r);
            }
            if rs == 1 {
                rowc[r] = rowc[r].max(self.l_h[child] + cn.margin.t + cn.margin.b);
            }
            child = self.next_flow(self.boxes[child].next);
        }
        let size = |tracks: &[Track], n: usize, content: &[f32; MAX_TRACKS], room: Option<f32>, gap: f32| {
            let mut out = [0.0f32; MAX_TRACKS];
            let (mut fixed, mut fr) = (0.0, 0.0);
            for i in 0..n {
                match (tracks.get(i).copied().unwrap_or(Track::Fit), room) {
                    (Track::Px(v), _) => out[i] = v,
                    (Track::Fr(f), Some(_)) => fr += f,
                    _ => out[i] = content[i],
                }
                fixed += out[i];
            }
            if let Some(room) = room {
                let free = (room - fixed - gap * n.saturating_sub(1) as f32).max(0.0);
                for i in 0..n {
                    if let Some(Track::Fr(f)) = tracks.get(i) {
                        out[i] = if fr > 0.0 { free * f / fr } else { 0.0 };
                    }
                }
            }
            out
        };
        let cols = size(g.cols, nc, &colc, room.map(|r| r.0), n.gap);
        let rows = size(g.rows, nr, &rowc, room.map(|r| r.1), n.row_gap);
        (cols, nc, rows, nr)
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
            let step = self.nd(index).scroll_step;
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
        if (self.slots[slot].scroll_mode != 0 || self.slots[slot].scroll_visual != 0.0) && !self.moving.contains(&slot) {
            self.moving.push(slot);
        }
        if consume {
            if let Some(value) = self.signals.get_mut(&key) {
                value.scroll_wheel = 0.0;
                value.scroll_drag = 0.0;
                value.scroll_touch = false;
                value.scroll_lift = false;
            }
        }
    }

    /// Top-down: relative sizes, grow and shrink shares, alignment, scrolling, scroll gutters and
    /// positions. A kept view's nodes are not in the build, so their layout is not run again.
    fn arrange(&mut self, time: f64) {
        self.l_x[0] = 0.0;
        self.l_y[0] = 0.0;
        for index in 0..self.count {
            let n = *self.nd(index);
            let pad = n.padding;
            let scrolls = n.flags & SCROLL != 0;
            let max_scroll = 0.0f32.max(self.l_content[index] - self.l_h[index]);
            let gutter = if scrolls && max_scroll > 0.0 && n.flags & OVER_THUMB == 0 { 10.0 } else { 0.0 };
            self.l_gutter[index] = gutter;
            let slot = self.boxes[index].slot;
            if scrolls {
                self.apply_scroll(false, time, index);
                self.scroll_y[slot] = 0.0f32.max(max_scroll.min(self.scroll_y[slot]));
            }
            if n.flags & SCROLL_X != 0 {
                let key = self.boxes[index].key;
                let delta = self.signals.get(&key).map_or(0.0, |v| v.scroll_x * n.scroll_step + v.scroll_drag_x);
                // right to left the offset runs from the right edge: the content follows the fingers
                let delta = if self.dir == Direction::Rtl { -delta } else { delta };
                let max_x = 0.0f32.max(self.l_content_w[index] - self.l_w[index]);
                self.scroll_xs[slot] = 0.0f32.max(max_x.min(self.scroll_xs[slot] - delta));
            }
            let inner_w = self.l_w[index] - pad.l - pad.r - gutter;
            let inner_h = self.l_h[index] - pad.t - pad.b;
            let relative = |kind: &Size, inner: f32, current: f32| match kind {
                Size::Pct(fraction) => inner * fraction,
                Size::Rel(f) => f(inner),
                _ => current,
            };
            let mut next = self.boxes[index].first;
            while next != NONE {
                let c = self.nd(next);
                let w = clamp(relative(&c.w, inner_w, self.l_w[next]), c.min_w, c.max_w);
                let h = clamp(relative(&c.h, inner_h, self.l_h[next]), c.min_h, c.max_h);
                (self.l_w[next], self.l_h[next]) = (w, h);
                next = self.boxes[next].next;
            }
            let (bake_x, bake_y) = self.baked(index);
            let (origin_x, origin_y) = (self.l_x[index] + bake_x, self.l_y[index] + bake_y);
            let inner_at = (origin_x + pad.l, origin_y + pad.t);
            match (n.grid, n.axis) {
                (Some(grid), _) => self.arrange_grid(index, &n, &grid, (inner_w, inner_h), inner_at),
                (None, Axis::Stack) => self.arrange_stack(index, &n, (inner_w, inner_h), inner_at),
                (None, _) => self.arrange_flow(index, &n, (inner_w, inner_h), inner_at),
            }
            let scroll_y = if scrolls { self.scroll_y[slot] + self.slots[slot].scroll_visual as f32 } else { 0.0 };
            let scroll_x = if n.flags & SCROLL_X != 0 { self.scroll_xs[slot] } else { 0.0 };
            let rtl = self.dir == Direction::Rtl;
            let (root_w, root_h) = (self.l_w[0], self.l_h[0]);
            let last = self.boxes[index].last;
            let mut stuck = false;
            next = self.boxes[index].first;
            while next != NONE {
                let child = next;
                let c = self.nd(child);
                let c = Placed { flags: c.flags, margin_t: c.margin.t, anchor: c.anchor, w_grow: c.w.is_grow(), h_grow: c.h.is_grow(), at: c.at };
                if self.flow(child) {
                    self.l_y[child] -= scroll_y;
                    self.l_x[child] -= scroll_x;
                    if rtl {
                        self.l_x[child] = 2.0 * origin_x + self.l_w[index] - self.l_x[child] - self.l_w[child];
                    }
                    if c.flags & STICKY != 0 && scrolls {
                        self.l_y[child] = self.l_y[child].max(inner_at.1 + c.margin_t);
                        stuck = true;
                    }
                } else if let Some(a) = c.anchor {
                    let r = place_anchored(a.rect, (self.l_w[child], self.l_h[child]), a.placement, Rect::new(0.0, 0.0, root_w, root_h));
                    (self.l_x[child], self.l_y[child]) = (r.x, r.y);
                } else {
                    // an `at` child's Grow size fills the parent across its axis (along it, none)
                    if n.axis == Axis::Row && c.h_grow {
                        self.set_main(false, child, inner_h);
                    } else if n.axis != Axis::Row && c.w_grow {
                        self.set_main(true, child, inner_w);
                    }
                    let (at_x, at_y) = c.at.unwrap_or((0.0, 0.0));
                    self.l_x[child] = origin_x + at_x;
                    self.l_y[child] = origin_y + at_y;
                }
                next = self.boxes[child].next;
            }
            // stuck headers paint (and take input) over the rows scrolling under them
            if stuck {
                let mut child = self.boxes[index].first;
                while child != NONE {
                    let next = self.boxes[child].next;
                    if self.nd(child).flags & STICKY != 0 && child != self.boxes[index].last {
                        self.unlink(index, child);
                        let end = self.boxes[index].last;
                        self.boxes[end].next = child;
                        self.boxes[index].last = child;
                    }
                    if child == last {
                        break;
                    }
                    child = next;
                }
            }
        }
        self.arranged = self.count;
    }

    /// Take `child` out of `index`'s child list.
    fn unlink(&mut self, index: usize, child: usize) {
        let mut previous = NONE;
        let mut at = self.boxes[index].first;
        while at != child {
            previous = at;
            at = self.boxes[at].next;
        }
        let next = self.boxes[child].next;
        if previous == NONE {
            self.boxes[index].first = next;
        } else {
            self.boxes[previous].next = next;
        }
        if self.boxes[index].last == child {
            self.boxes[index].last = previous;
        }
        self.boxes[child].next = NONE;
    }

    fn main(&self, row: bool, c: usize) -> f32 {
        if row { self.l_w[c] } else { self.l_h[c] }
    }

    fn set_main(&mut self, row: bool, c: usize, v: f32) {
        let n = self.nd(c);
        if row {
            self.l_w[c] = clamp(v, n.min_w, n.max_w);
        } else {
            self.l_h[c] = clamp(v, n.min_h, n.max_h);
        }
    }

    fn set_cross(&mut self, row: bool, c: usize, v: f32) {
        self.set_main(!row, c, v);
    }

    /// The distance from a node's top to its first text baseline (its height when it has no text).
    // ponytail: a node without text of its own does not take its first child's baseline.
    fn baseline(&mut self, c: usize) -> f32 {
        let slot = self.boxes[c].slot;
        let h = self.l_h[c];
        if self.slots[slot].text.is_empty() {
            return h;
        }
        let (look, inset) = (self.slots[slot].node.look, self.slots[slot].node.inset);
        let size = look.font.points(self.font_size);
        let style = Style { font: look.font.family, size: size as f32, weight: look.font.weight, tracking: look.font.tracking * size as f32, ..Style::default() };
        let text = self.text.as_mut().expect("Ui: the text engine is lent out");
        let (_, m) = lay_out(text, &style, 0.0, &self.slots[slot].text);
        text_top_for(self.font_size, Some(size), inset.t, h - inset.t - inset.b) + m.baseline
    }

    /// A row or column: lines (one unless it wraps), each sharing its room by grow and shrink
    /// weights, placed along by `justify` and across by `align`.
    fn arrange_flow(&mut self, index: usize, n: &Node, inner: (f32, f32), at: (f32, f32)) {
        let row = n.axis == Axis::Row;
        let (main_room, cross_room) = if row { inner } else { (inner.1, inner.0) };
        let main_margins = |m: Edges| if row { (m.l, m.r) } else { (m.t, m.b) };
        let cross_margins = |m: Edges| if row { (m.t, m.b) } else { (m.l, m.r) };
        let grows = |c: &Node| if row { c.w.is_grow() } else { c.h.is_grow() };
        let mut start = self.next_flow(self.boxes[index].first);
        let mut cross_at = 0.0;
        while start != NONE {
            // the line: its children, their bases and the room they take
            let (mut used, mut count, mut end) = (0.0f32, 0usize, start);
            while end != NONE {
                let c = *self.nd(end);
                // a one-line flow knows its children's stretched cross sizes now, and an aspect
                // child's Fit main size follows from them
                let (cs, ce) = cross_margins(c.margin);
                let cross_size = if row { c.h } else { c.w };
                if !n.wrap && (cross_size.is_grow() || (c.align_self.unwrap_or(n.align) == Align::Stretch && matches!(cross_size, Size::Fit))) {
                    self.set_cross(row, end, cross_room - cs - ce);
                }
                if c.aspect > 0.0 && matches!(if row { c.w } else { c.h }, Size::Fit) {
                    let cross = self.main(!row, end);
                    self.set_main(row, end, if row { cross * c.aspect } else { cross / c.aspect });
                }
                // a Grow child starts from nothing (its minimum holds in `grow_line`)
                let basis = if grows(&c) { 0.0 } else { self.main(row, end) };
                let (ms, me) = main_margins(c.margin);
                let outer = basis + ms + me;
                if n.wrap && count > 0 && used + n.gap + outer > main_room {
                    break;
                }
                used += if count > 0 { n.gap } else { 0.0 } + outer;
                count += 1;
                self.set_main(row, end, basis);
                end = self.next_flow(self.boxes[end].next);
            }
            let free = main_room - used;
            self.grow_line(row, start, end, free);
            if free < 0.0 {
                self.shrink_line(row, start, end, -free);
            }
            // across: stretched, aspect-derived, the line's extent and its baseline
            let (mut line_cross, mut base_max) = (0.0f32, 0.0f32);
            let mut c = start;
            while c != end {
                let cn = *self.nd(c);
                let (cs, ce) = cross_margins(cn.margin);
                let a = cn.align_self.unwrap_or(n.align);
                let fit = matches!(if row { cn.h } else { cn.w }, Size::Fit);
                if cn.aspect > 0.0 && fit {
                    let main = self.main(row, c);
                    self.set_cross(row, c, if row { main / cn.aspect } else { main * cn.aspect });
                }
                line_cross = line_cross.max(self.main(!row, c) + cs + ce);
                if a == Align::Baseline {
                    base_max = base_max.max(cs + self.baseline(c));
                }
                c = self.next_flow(self.boxes[c].next);
            }
            if !n.wrap {
                line_cross = cross_room;
            }
            let mut used = 0.0;
            c = start;
            while c != end {
                let (ms, me) = main_margins(self.nd(c).margin);
                used += self.main(row, c) + ms + me;
                c = self.next_flow(self.boxes[c].next);
            }
            let rem = main_room - used - n.gap * count.saturating_sub(1) as f32;
            let k = count as f32;
            let (mut pos, between) = match n.justify {
                Justify::Start => (0.0, n.gap),
                Justify::Center => (rem / 2.0, n.gap),
                Justify::End => (rem, n.gap),
                Justify::SpaceBetween if rem > 0.0 && count > 1 => (0.0, n.gap + rem / (k - 1.0)),
                Justify::SpaceAround if rem > 0.0 => (rem / k / 2.0, n.gap + rem / k),
                Justify::SpaceEvenly if rem > 0.0 => (rem / (k + 1.0), n.gap + rem / (k + 1.0)),
                _ => (0.0, n.gap),
            };
            c = start;
            while c != end {
                let cn = *self.nd(c);
                let (ms, me) = main_margins(cn.margin);
                let (cs, ce) = cross_margins(cn.margin);
                let a = cn.align_self.unwrap_or(n.align);
                let cross_size = if row { cn.h } else { cn.w };
                if n.wrap && (cross_size.is_grow() || (a == Align::Stretch && matches!(cross_size, Size::Fit))) {
                    self.set_cross(row, c, line_cross - cs - ce);
                }
                let outer = self.main(!row, c) + cs + ce;
                let off = match a {
                    Align::Center => (line_cross - outer) / 2.0,
                    Align::End => line_cross - outer,
                    Align::Baseline => base_max - cs - self.baseline(c),
                    _ => 0.0,
                } + cs;
                let main_at = pos + ms;
                if row {
                    (self.l_x[c], self.l_y[c]) = (at.0 + main_at, at.1 + cross_at + off);
                } else {
                    (self.l_x[c], self.l_y[c]) = (at.0 + cross_at + off, at.1 + main_at);
                }
                pos = main_at + self.main(row, c) + me + between;
                c = self.next_flow(self.boxes[c].next);
            }
            cross_at += line_cross + n.row_gap;
            start = end;
        }
    }

    /// Share `free` points among the line's `Grow` children by their `grow` weights; a child held
    /// at its minimum or maximum leaves the rest to the others. With nothing free they keep their
    /// minimum.
    // ponytail: the freeze loop is O(children x clamped), and past 64 children in a line the later
    // ones are never frozen; fine for the short lines UI has.
    fn grow_line(&mut self, row: bool, start: usize, end: usize, mut free: f32) {
        let mut frozen = 0u64;
        loop {
            let (mut total, mut i, mut c) = (0.0, 0, start);
            while c != end {
                let n = self.nd(c);
                if (if row { n.w.is_grow() } else { n.h.is_grow() }) && n.grow > 0.0 && frozen & (1 << i.min(63)) == 0 {
                    total += n.grow;
                }
                (i, c) = (i + 1, self.next_flow(self.boxes[c].next));
            }
            if total <= 0.0 {
                return;
            }
            let unit = free.max(0.0) / total;
            let mut capped = false;
            (i, c) = (0, start);
            while c != end {
                let n = *self.nd(c);
                let (min, max) = if row { (n.min_w, n.max_w) } else { (n.min_h, n.max_h) };
                if (if row { n.w.is_grow() } else { n.h.is_grow() }) && n.grow > 0.0 && frozen & (1 << i.min(63)) == 0 {
                    let v = unit * n.grow;
                    if v < min || v > max {
                        let held = clamp(v, min, max);
                        free -= held;
                        self.set_main(row, c, held);
                        frozen |= 1 << i.min(63);
                        capped = true;
                    }
                }
                (i, c) = (i + 1, self.next_flow(self.boxes[c].next));
            }
            if !capped {
                (i, c) = (0, start);
                while c != end {
                    let n = *self.nd(c);
                    if (if row { n.w.is_grow() } else { n.h.is_grow() }) && n.grow > 0.0 && frozen & (1 << i.min(63)) == 0 {
                        self.set_main(row, c, unit * n.grow);
                    }
                    (i, c) = (i + 1, self.next_flow(self.boxes[c].next));
                }
                return;
            }
        }
    }

    /// Take `over` points from the line's children by `shrink` times their size, none below its
    /// minimum.
    fn shrink_line(&mut self, row: bool, start: usize, end: usize, mut over: f32) {
        let min = |ui: &Ui, c: usize| if row { ui.nd(c).min_w } else { ui.nd(c).min_h };
        loop {
            let mut total = 0.0;
            let mut c = start;
            while c != end {
                if self.nd(c).shrink > 0.0 && self.main(row, c) > min(self, c) {
                    total += self.nd(c).shrink * self.main(row, c);
                }
                c = self.next_flow(self.boxes[c].next);
            }
            if total <= 0.0 || over <= 0.0 {
                return;
            }
            let ratio = over / total;
            let mut capped = false;
            c = start;
            while c != end {
                let (s, v, lo) = (self.nd(c).shrink, self.main(row, c), min(self, c));
                if s > 0.0 && v > lo && v - ratio * s * v < lo {
                    over -= v - lo;
                    self.set_main(row, c, lo);
                    capped = true;
                }
                c = self.next_flow(self.boxes[c].next);
            }
            if !capped {
                c = start;
                while c != end {
                    let (s, v, lo) = (self.nd(c).shrink, self.main(row, c), min(self, c));
                    if s > 0.0 && v > lo {
                        self.set_main(row, c, v - ratio * s * v);
                    }
                    c = self.next_flow(self.boxes[c].next);
                }
                return;
            }
        }
    }

    /// A stack: every flow child at the inner origin, `Grow` sizes filling it, placed across by
    /// `justify` and down by `align`.
    fn arrange_stack(&mut self, index: usize, n: &Node, inner: (f32, f32), at: (f32, f32)) {
        let mut c = self.next_flow(self.boxes[index].first);
        while c != NONE {
            let cn = *self.nd(c);
            let m = cn.margin;
            if cn.w.is_grow() {
                self.set_main(true, c, inner.0 - m.l - m.r);
            }
            if cn.h.is_grow() {
                self.set_main(false, c, inner.1 - m.t - m.b);
            }
            if cn.aspect > 0.0 && matches!(cn.h, Size::Fit) {
                self.set_main(false, c, self.l_w[c] / cn.aspect);
            }
            let ow = self.l_w[c] + m.l + m.r;
            let oh = self.l_h[c] + m.t + m.b;
            let x = match n.justify {
                Justify::Center => (inner.0 - ow) / 2.0,
                Justify::End => inner.0 - ow,
                _ => 0.0,
            };
            let y = match cn.align_self.unwrap_or(n.align) {
                Align::Center => (inner.1 - oh) / 2.0,
                Align::End => inner.1 - oh,
                _ => 0.0,
            };
            (self.l_x[c], self.l_y[c]) = (at.0 + x + m.l, at.1 + y + m.t);
            c = self.next_flow(self.boxes[c].next);
        }
    }

    /// A grid: tracks sized by `grid_tracks`, each child in its cell (a `Grow` size fills the cell;
    /// `align` places it down).
    fn arrange_grid(&mut self, index: usize, n: &Node, g: &Grid, inner: (f32, f32), at: (f32, f32)) {
        let (cols, nc, rows, nr) = self.grid_tracks(index, n, g, Some(inner));
        let (mut col_x, mut row_y) = ([0.0f32; MAX_TRACKS + 1], [0.0f32; MAX_TRACKS + 1]);
        for i in 0..nc {
            col_x[i + 1] = col_x[i] + cols[i] + n.gap;
        }
        for i in 0..nr {
            row_y[i + 1] = row_y[i] + rows[i] + n.row_gap;
        }
        let mut cursor = (0, 0);
        let mut c = self.next_flow(self.boxes[index].first);
        while c != NONE {
            let cn = *self.nd(c);
            let m = cn.margin;
            let (col, row, cs, rs) = next_cell(cn.cell, &mut cursor, nc);
            let span_w = col_x[col + cs] - col_x[col] - n.gap;
            let span_h = row_y[row + rs] - row_y[row] - n.row_gap;
            let a = cn.align_self.unwrap_or(n.align);
            if cn.w.is_grow() {
                self.set_main(true, c, span_w - m.l - m.r);
            }
            if cn.h.is_grow() || (a == Align::Stretch && matches!(cn.h, Size::Fit)) {
                self.set_main(false, c, span_h - m.t - m.b);
            }
            if cn.aspect > 0.0 && matches!(cn.h, Size::Fit) {
                self.set_main(false, c, self.l_w[c] / cn.aspect);
            }
            let oh = self.l_h[c] + m.t + m.b;
            let y = match a {
                Align::Center => (span_h - oh) / 2.0,
                Align::End => span_h - oh,
                _ => 0.0,
            };
            (self.l_x[c], self.l_y[c]) = (at.0 + col_x[col] + m.l, at.1 + row_y[row] + y + m.t);
            c = self.next_flow(self.boxes[c].next);
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

/// `v` held to `[min, max]`; the minimum wins.
/// `kept` becomes a copy of `f` in the storage it already has (a frame with input allocated twice).
fn keep_frame(kept: &mut Option<rframe::Frame>, f: &rframe::Frame) {
    let Some(k) = kept else {
        *kept = Some(f.clone());
        return;
    };
    let rframe::Frame { input, pixel_scale, fps, mouse_delta } = f;
    let input::Frame { t, dt, frame, tick, size, pointer, buttons, keys, events } = input;
    let i = &mut k.input;
    (i.t, i.dt, i.frame, i.tick, i.size, i.pointer) = (*t, *dt, *frame, *tick, *size, *pointer);
    i.buttons.clone_from(buttons);
    i.keys.clone_from(keys);
    i.events.clone_from(events);
    (k.pixel_scale, k.fps, k.mouse_delta) = (*pixel_scale, *fps, *mouse_delta);
}

/// What `arrange` reads of a child while placing it (not the whole node).
struct Placed {
    flags: Flags,
    margin_t: f32,
    anchor: Option<Anchor>,
    w_grow: bool,
    h_grow: bool,
    at: Option<(f32, f32)>,
}

fn clamp(v: f32, min: f32, max: f32) -> f32 {
    v.min(max).max(min)
}

/// Tracks' total extent with `gap` between them.
fn span(tracks: &[f32], gap: f32) -> f32 {
    tracks.iter().sum::<f32>() + gap * tracks.len().saturating_sub(1) as f32
}

/// A grid child's (column, row, column span, row span): its own cell, or the next free one along
/// the rows from `cursor` (auto placement; explicit cells are not avoided).
fn next_cell(c: Cell, cursor: &mut (usize, usize), cols: usize) -> (usize, usize, usize, usize) {
    let cs = (c.col_span.max(1) as usize).min(cols);
    let (col, row) = if c.col == Cell::AUTO {
        if cursor.0 + cs > cols {
            *cursor = (0, cursor.1 + 1);
        }
        let at = *cursor;
        cursor.0 += cs;
        at
    } else {
        ((c.col as usize).min(cols - 1), c.row as usize)
    };
    let row = row.min(MAX_TRACKS - 1);
    (col, row, cs.min(cols - col), (c.row_span.max(1) as usize).min(MAX_TRACKS - row))
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
    /// the view table index instances take
    view: u16,
    pub clip_rect: Rect,
    /// the corner radius of `clip_rect` (a rounded `CLIP` node's children)
    pub clip_radius: f32,
    pub theme: Tokens,
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
    /// the content width and horizontal scroll offset, as `l_content` and `scroll_y`
    pub l_content_w: Vec<f32>,
    pub scroll_x: Vec<f32>,
    /// right to left: node text sits its start inset from the right, scroll thumbs mirror
    pub rtl: bool,
}

impl Paint {
    /// Start the next layer (a floating root's), reusing a layer vector a previous frame left.
    fn next_layer(&mut self, key: i64) {
        self.layer = self.layer.wrapping_add(1);
        if self.layer == self.layers.len() {
            self.layers.push(Layer::new(key, 0));
        }
        let layer = &mut self.layers[self.layer];
        layer.key = key;
        layer.instances.clear();
        layer.breaks.clear();
        layer.images.clear();
        layer.pieces.clear();
        layer.pieces.push(Piece { view: NO_VIEW, a: 0, b: 0, at: 0 });
    }

    /// End the current fresh piece and start the next where the instances stand.
    fn cut(&mut self) {
        let layer = &mut self.layers[self.layer];
        let n = layer.instances.len() as u32;
        layer.pieces.last_mut().expect("a layer starts with a piece").b = n;
        layer.pieces.push(Piece { view: NO_VIEW, a: n, b: n, at: 0 });
    }

    /// Kept view `v`'s run goes here.
    fn keep(&mut self, v: u32) {
        self.cut();
        let layer = &mut self.layers[self.layer];
        let n = layer.instances.len() as u32;
        let last = layer.pieces.len() - 1;
        layer.pieces[last].view = v;
        layer.pieces.push(Piece { view: NO_VIEW, a: n, b: n, at: 0 });
    }

    /// Append `instance` under the current clip and view.
    fn push(&mut self, instance: Instance) {
        let clip = clip_of(self.clip_rect);
        let clip_radius = if self.clip_radius > 0.0 { crate::draw::f16(self.clip_radius) } else { 0 };
        self.layers[self.layer].instances.push(Instance { clip, clip_radius, view: self.view, ..instance });
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

    /// Bands `width` wide inside `r` on `sides` (0: all four, the stroke `frame_w` draws).
    fn bands(&mut self, Rect { x, y, w, h }: Rect, width: f32, sides: u8, color: Color) {
        use crate::draw::flags::{BOTTOM, LEFT, RIGHT, TOP};
        if sides & 0xf0 == 0 || sides & 0xf0 == 0xf0 {
            self.frame_w(x, y, w, h, width, color);
            return;
        }
        if sides & TOP != 0 {
            self.fill(x, y, w, width, color);
        }
        if sides & RIGHT != 0 {
            self.fill(x + w - width, y, width, h, color);
        }
        if sides & BOTTOM != 0 {
            self.fill(x, y + h - width, w, width, color);
        }
        if sides & LEFT != 0 {
            self.fill(x, y, width, h, color);
        }
    }

    /// A node's look, text and mark in `r` (its ring too when `focused`): what the engine paints for it
    /// with no closure. Square, solid looks paint as hard rectangles on the pixel grid; rounded,
    /// gradient and shadowed ones as distance fields. `steady` (a running transition) emits the same
    /// instances whatever the colours, so each step rewrites the node's range in place.
    fn look(&mut self, r: Rect, node: &Node, look: &Look, text: &str, focused: bool, steady: bool) {
        let placed = r;
        let Rect { x, y, w, h } = placed;
        let o = look.opacity.clamp(0.0, 1.0);
        let fade = |c: Color| if o < 1.0 { Color { a: (c.a as f32 * o).round() as u8, ..c } } else { c };
        let corners = [x, y, x + w, y + h];
        let radii = look.radius;
        let area = w > 0.0 && h > 0.0;
        let shown = |c: Color| c.a > 0 || steady;
        let round = steady || look.gradient.is_some() || radii.iter().any(|r| *r > 0.0) || look.shadows.iter().any(|s| shown(s.color));
        let shadow = |s: &Shadow| Instance::shadow(corners, radii, rgba(fade(s.color)), [s.offset.0, s.offset.1], s.blur / 2.0, s.spread, s.inner);
        let bordered = steady || (look.border_w > 0.0 && look.border.a > 0);
        if area {
            for s in &look.shadows {
                if !s.inner && shown(s.color) {
                    self.push(shadow(s));
                }
            }
            let inner = look.shadows.iter().any(|s| s.inner && shown(s.color));
            if round {
                let mut shape = Instance::rect(corners, rgba(fade(look.fill))).radii(radii);
                if let Some(to) = look.gradient {
                    let (dx, dy) = (look.angle.to_radians().sin(), -look.angle.to_radians().cos());
                    shape = shape.linear([0.5 - dx / 2.0, 0.5 - dy / 2.0], [0.5 + dx / 2.0, 0.5 + dy / 2.0], rgba(fade(to)));
                }
                let edge = Instance::rect(corners, [0; 4]).radii(radii).border(look.border_w, rgba(fade(look.border))).sides(look.sides);
                if bordered && !inner {
                    shape = Instance { width: edge.width, color2: edge.color2, flags: shape.flags & 0x0f | edge.flags & 0xf0, ..shape };
                }
                if shown(look.fill) || look.gradient.is_some() || (bordered && !inner) {
                    self.push(shape);
                }
                for s in &look.shadows {
                    if s.inner && shown(s.color) {
                        self.push(shadow(s));
                    }
                }
                if bordered && inner {
                    self.push(edge);
                }
            } else {
                if look.fill.a > 0 {
                    self.fill(x, y, w, h, fade(look.fill));
                }
                if bordered {
                    self.bands(placed, look.border_w, look.sides, fade(look.border));
                }
            }
        }
        if !text.is_empty() {
            let size = look.font.points(self.font_size);
            let inset = node.inset;
            let at = (x + inset.l, text_top_for(self.font_size, Some(size), y + inset.t, h - inset.t - inset.b));
            let style = Style { font: look.font.family, size: size as f32, weight: look.font.weight, tracking: look.font.tracking * size as f32, ..Style::default() };
            let ink = Some(fade(look.text));
            let fitted;
            let text = if node.flags & FIT_TEXT != 0 {
                let limit = w - inset.l - inset.r;
                let plain = Style { tracking: 0.0, ..style };
                if lay_out(&mut self.text, &plain, 0.0, text).1.w <= limit {
                    text
                } else {
                    let shown = ellipsis(&mut |s| lay_out(&mut self.text, &plain, 0.0, s).1.w, limit, text);
                    fitted = if shown.is_empty() && !(text.is_empty() && limit >= 0.0) { Cow::Borrowed("\u{2026}") } else { shown };
                    &*fitted
                }
            } else {
                text
            };
            let mut buffer = std::mem::take(&mut self.label);
            buffer.clear();
            let text = if look.font.caps {
                buffer.extend(text.chars().map(|c| c.to_ascii_uppercase()));
                &buffer
            } else {
                text
            };
            // right to left the start inset is at the right: the text ends there
            let at = if self.rtl { (x + w - inset.l - lay_out(&mut self.text, &style, 0.0, text).1.w, at.1) } else { at };
            self.text_styled(at, &style, 0.0, ink, text);
            self.label = buffer;
        }
        if let Mark::Chevron(direction) = node.mark {
            self.chevron((x + w / 2.0, y + h / 2.0), direction, fade(look.text));
        }
        let ring = look.ring;
        if focused && area && (steady || (ring.width > 0.0 && ring.color.a > 0)) {
            if round {
                self.push(Instance::rect(corners, [0; 4]).radii(radii).border(ring.width, rgba(fade(ring.color))).sides(ring.sides));
            } else {
                self.bands(placed, ring.width, ring.sides, fade(ring.color));
            }
        }
    }

    /// A scroll node's thumb (`Node::bar`) for box `index` at `r`, where its content and scroll put it.
    fn bar(&mut self, r: Rect, index: usize, slot: usize, bar: &Bar) {
        let content = self.l_content[index];
        let (w, gap) = (bar.width, bar.gap);
        let thumb = if bar.whole {
            // a panel's thumb lands on whole points, as its integer layout did
            let (x, y, width, height) = (r.x as i64, r.y as i64, r.w as i64, r.h as i64);
            let (content, margin) = (content as i64, bar.margin as i64);
            let maximum = 0.max(content - height);
            (maximum > 0).then(|| {
                let track_h = 1.max(height - 2 * margin);
                let thumb_h = track_h.min(20.max(track_h * height / 1.max(content)));
                let thumb_y = y + margin + self.scroll_y[slot] as i64 * (track_h - thumb_h) / maximum;
                let x = if self.rtl { (x as f32) + gap } else { (x + width) as f32 - gap - w };
                Rect::new(x, thumb_y as f32, w, thumb_h as f32)
            })
        } else {
            (content > r.h).then(|| {
                let track_h = 1.0f32.max(r.h - 2.0 * bar.margin);
                let thumb_h = track_h.min(20.0f32.max(track_h * r.h / content));
                let thumb_y = r.y + bar.margin + self.scroll_y[slot] * (track_h - thumb_h) / (content - r.h);
                Rect::new(if self.rtl { r.x + gap } else { r.x + r.w - gap - w }, thumb_y, w, thumb_h)
            })
        };
        if let Some(t) = thumb {
            if bar.radius > 0.0 {
                self.push(Instance::rect([t.x, t.y, t.x + t.w, t.y + t.h], rgba(bar.color)).radii([bar.radius; 4]));
            } else {
                self.fill(t.x, t.y, t.w, t.h, bar.color);
            }
        }
    }

    /// A `SCROLL_X` node's thumb along its bottom edge, as `bar` along the right.
    fn bar_x(&mut self, r: Rect, index: usize, slot: usize, bar: &Bar) {
        let content = self.l_content_w[index];
        if content <= r.w {
            return;
        }
        let track_w = 1.0f32.max(r.w - 2.0 * bar.margin);
        let thumb_w = track_w.min(20.0f32.max(track_w * r.w / content));
        let along = bar.margin + self.scroll_x[slot] * (track_w - thumb_w) / (content - r.w);
        let x = if self.rtl { r.x + r.w - along - thumb_w } else { r.x + along };
        let t = Rect::new(x, r.y + r.h - bar.gap - bar.width, thumb_w, bar.width);
        if bar.radius > 0.0 {
            self.push(Instance::rect([t.x, t.y, t.x + t.w, t.y + t.h], rgba(bar.color)).radii([bar.radius; 4]));
        } else {
            self.fill(t.x, t.y, t.w, t.h, bar.color);
        }
    }

    /// Bind `image` for the instances pushed next: the layer breaks to its texture.
    fn bind(&mut self, image: &Image) {
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
    }

    /// Draw a host-owned image for this frame; the layer breaks to its texture.
    pub fn image(&mut self, x: f32, y: f32, w: f32, h: f32, image: &Image) {
        self.image_fit(x, y, w, h, image, Fit::Fill, color::white());
    }

    /// An image placed in the box as CSS `object-fit` places it, its texels multiplied by `tint`
    /// (white keeps it; see `Instance::image`).
    pub fn image_fit(&mut self, x: f32, y: f32, w: f32, h: f32, image: &Image, fit: Fit, tint: Color) {
        if w > 0.0 && h > 0.0 {
            self.bind(image);
            let (rect, uv) = crate::draw::fit(fit, [image.width as f32, image.height as f32], [x, y, x + w, y + h], [0.0, 0.0, 1.0, 1.0]);
            self.push(Instance::image(rect, uv, rgba(tint)));
        }
    }

    /// An image as a nine-slice frame (CSS `border-image`): corners `inset` texels in (`[left, top,
    /// right, bottom]`) stay 1:1, edges and centre stretch over the box.
    pub fn image_nine(&mut self, x: f32, y: f32, w: f32, h: f32, image: &Image, inset: [f32; 4], tint: Color) {
        if w > 0.0 && h > 0.0 {
            self.bind(image);
            for i in crate::draw::nine_slice([x, y, x + w, y + h], [0.0, 0.0, 1.0, 1.0], [image.width as f32, image.height as f32], inset, rgba(tint)) {
                self.push(i);
            }
        }
    }

    /// One glyph of font `font` (an icon font loaded into the UI's `Text`) at `size` points, its top-left
    /// at `at`, in `color`: its advance.
    pub fn icon(&mut self, at: (f32, f32), font: u16, size: f32, codepoint: char, color: Color) -> f32 {
        let mut buffer = [0u8; 4];
        let style = Style { font, size, ..Style::default() };
        self.text_styled(at, &style, 0.0, Some(color), codepoint.encode_utf8(&mut buffer)).w
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
        let (clip, view) = (clip_of(self.clip_rect), self.view);
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
            layer.instances.push(Instance { clip, view, ..instance });
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
        let Rect { x, y, w, h } = rect;
        let rec = HitRec { key: self.boxes[index].key, parent, flags: self.nd(index).flags, x, y, w, h, view: self.paint_xf };
        self.push_hit(rec);
    }

    fn push_hit(&mut self, rec: HitRec) {
        let position = self.hit_count;
        if position >= self.hits.len() {
            self.hits.push(rec);
        } else {
            self.hits[position] = rec;
        }
        self.chain_key = 0;
        self.hit_count = position + 1;
    }

    fn run_painters(&self, first: u32, index: usize, clip_rect: Rect, paint: &mut Paint) {
        if first != arena::END {
            paint.clip_rect = clip_rect;
            self.arena.run(first, paint, self.screen(index));
        }
    }

    /// The node marks its own focus (`FOCUS_MARK`) and holds it: its look's ring shows.
    fn marks_focus(&self, slot: usize) -> bool {
        let s = &self.slots[slot];
        s.key != 0 && s.key == self.focus && s.node.flags & FOCUS_MARK != 0
    }

    /// Paint the node's look and text, recording the instance range a patch rewrites.
    fn paint_node(&mut self, paint: &mut Paint, index: usize, clip_rect: Rect) {
        paint.clip_rect = clip_rect;
        let slot = self.boxes[index].slot;
        let start = paint.layers[paint.layer].instances.len() as u32;
        if let Some(curve) = self.slots[slot].node.transition.take() {
            let from = match self.tween_of(slot) {
                Some(_) => self.pose(slot).0,
                None => Pose { look: self.slots[slot].seen, ..self.rest(slot) },
            };
            let to = Pose { look: self.slots[slot].node.look, ..self.rest(slot) };
            self.start_tween(slot, curve, from, to);
        }
        // a declaration wins over an animated look
        if let Some(t) = self.tween_of(slot) {
            let look = self.slots[slot].node.look;
            self.tweens[t].to.look = look;
        }
        let (pose, steady) = self.pose(slot);
        let focused = self.marks_focus(slot);
        let r = self.screen(index);
        let s = &self.slots[slot];
        let look = Look { opacity: pose.look.opacity * pose.opacity, ..pose.look };
        paint.look(Rect::new(r.x + pose.dx, r.y + pose.dy, r.w, r.h), &s.node, &look, &s.text, focused, steady);
        let s = &mut self.slots[slot];
        if self.motion {
            (s.seen, s.seen_state) = (s.node.look, s.node.state);
        }
        // scratch positions until `settle` places them
        s.range = (start, paint.layers[paint.layer].instances.len() as u32);
        s.clip = clip_rect;
        s.clip_radius = paint.clip_radius;
        s.painted = self.frame_number;
        self.painted_nodes.push((slot, paint.layer));
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
        // a view box is painted wherever it is, so its output can be kept and scrolled into view
        let view = self.boxes[index].view;
        let visible = visible || (view != NO_VIEW && w > 0.0 && h > 0.0);
        let flags = self.nd(index).flags;
        let has_hit = flags & HIT_FLAGS != 0 && hw > 0.0 && hh > 0.0;
        if has_hit {
            self.record_hit(index, parent_hit, clipped_hit);
        }
        // a box with no extent of its own (a `Fit` box whose children are all placed with `at`) is
        // not out of view: its children show unless it clips them
        let shown = visible;
        if shown || ((w <= 0.0 || h <= 0.0) && flags & CLIP == 0) {
            if shown {
                self.paint_node(paint, index, clip_rect);
                self.run_painters(self.boxes[index].paint.0, index, clip_rect, paint);
            }
            let child_clip = if flags & CLIP != 0 {
                let p = self.nd(index).padding;
                intersect(clip_rect, Rect::new(x + p.l, y + p.t, 0.0f32.max(w - p.l - p.r), 0.0f32.max(h - p.t - p.b)))
            } else {
                clip_rect
            };
            let parent_hit = if has_hit { self.boxes[index].key } else { parent_hit };
            // a `ROUND_CLIP` node's corner radius rounds its children's clip; a square clip inside a
            // rounded one drops the rounding
            // ponytail: one radius (the largest corner's) for the clip, and a cached view inside takes
            // its outer clip square from the view table; per-corner radii when a design asks.
            let outer_radius = paint.clip_radius;
            if flags & CLIP != 0 {
                let r = self.nd(index).look.radius;
                paint.clip_radius = if flags & ROUND_CLIP != 0 { r[0].max(r[1]).max(r[2]).max(r[3]) } else { 0.0 };
            }
            if view != NO_VIEW {
                self.visit_view(paint, index, view, child_clip, parent_hit);
            } else {
                let mut child = self.boxes[index].first;
                while child != NONE {
                    self.visit(paint, child, child_clip, parent_hit);
                    child = self.boxes[child].next;
                }
            }
            paint.clip_radius = outer_radius;
            if shown {
                self.run_painters(self.boxes[index].over.0, index, clip_rect, paint);
                if let Some(bar) = self.nd(index).bar {
                    paint.clip_rect = clip_rect;
                    if flags & SCROLL != 0 || flags & SCROLL_X == 0 {
                        paint.bar(screen_rect, index, slot, &bar);
                    }
                    if flags & SCROLL_X != 0 {
                        paint.bar_x(screen_rect, index, slot, &bar);
                    }
                }
            }
            if shown && self.keyboard_focus && self.focus == self.boxes[index].key && flags & FOCUS_MARK == 0 {
                // a node that marks its own focus draws its look's ring; any other gets the theme's
                paint.clip_rect = clip_rect;
                let ring = self.theme.ring;
                let r = Rect::new(self.l_x[index], self.l_y[index], 0.0f32.max(self.l_w[index]), 0.0f32.max(self.l_h[index]));
                if ring.width > 0.0 && ring.color.a > 0 {
                    paint.bands(r, ring.width, ring.sides, ring.color);
                }
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
        self.paint_xf = 0;
        self.damage = None;
        let clip = Rect::new(0.0, 0.0, frame.input.size.0 as f32, frame.input.size.1 as f32);
        let scratch = std::mem::take(&mut self.spare);
        let mut paint = self.paint_begin(scratch, clip);
        self.painted = self.frame_number;
        self.views[0].visited = self.painted;
        self.visit(&mut paint, 0, clip, 0);
        self.out.ime = paint.ime;
        self.edit_scroll_x = paint.edit_scroll_x;
        let len = paint.layer + 1;
        let mut layers = self.paint_end(paint);
        if self.verify {
            self.verify_runs(&layers);
        }
        let mut changed = self.refresh_views();
        changed |= self.place_all(&mut layers, len);
        self.settle(&layers);
        self.collect_blurs();
        if changed {
            self.draw_revision += 1;
        }
        self.spare = layers;
        self.painting = false;
    }

    /// A painter over `layers` (its first layer cleared) with the text engine and the layout
    /// arrays lent to it; `paint_end` takes them back.
    fn paint_begin(&mut self, layers: Vec<Layer>, clip: Rect) -> Paint {
        let mut paint = Paint {
            layers,
            // the base layer is the one after none
            layer: usize::MAX,
            view: 0,
            clip_rect: clip,
            clip_radius: 0.0,
            theme: self.theme.tokens,
            font_size: self.font_size,
            row_height: self.kit_row_height,
            density: self.density,
            text: self.text.take().expect("Ui: painting re-entered"),
            ime: None,
            label: std::mem::take(&mut self.label),
            edit_scroll_x: self.edit_scroll_x,
            l_content: std::mem::take(&mut self.l_content),
            scroll_y: std::mem::take(&mut self.scroll_y),
            l_content_w: std::mem::take(&mut self.l_content_w),
            scroll_x: std::mem::take(&mut self.scroll_xs),
            rtl: self.dir == Direction::Rtl,
        };
        paint.next_layer(0);
        paint
    }

    fn paint_end(&mut self, paint: Paint) -> Vec<Layer> {
        let Paint { layers, text, label, l_content, scroll_y, l_content_w, scroll_x, .. } = paint;
        self.text = Some(text);
        self.label = label;
        self.l_content = l_content;
        self.scroll_y = scroll_y;
        self.l_content_w = l_content_w;
        self.scroll_xs = scroll_x;
        layers
    }

    // ----------------------------------------------------- direct patching

    /// Repaint one node's look and text over its range of the last frame's instances: in place when
    /// the count is unchanged, else the layer's later instances, node ranges and breaks shift.
    /// While building nothing is rewritten: the frame's paint emits the node anyway.
    fn repaint_node(&mut self, slot: usize) -> bool {
        if self.building || !self.range_valid(slot) {
            // not on screen (or about to be painted): the record holds the patch
            return true;
        }
        let (key, (start, end)) = (self.slots[slot].range_layer, self.slots[slot].range);
        let Some(layer) = self.draw[..self.draw_len].iter().position(|l| l.key == key) else { return true };
        let (start, end) = (start as usize, end as usize);
        let owner = self.slots[slot].owner;
        let scratch = std::mem::take(&mut self.patch_layers);
        let mut paint = self.paint_begin(scratch, self.slots[slot].clip);
        paint.clip_radius = self.slots[slot].clip_radius;
        paint.view = self.views[self.views[owner as usize].xf as usize].index;
        let (pose, steady) = self.pose(slot);
        let focused = self.marks_focus(slot);
        let s = &self.slots[slot];
        let look = Look { opacity: pose.look.opacity * pose.opacity, ..pose.look };
        paint.look(Rect::new(s.rx + pose.dx, s.ry + pose.dy, s.rw, s.rh), &s.node, &look, &s.text, focused, steady);
        let scratch = self.paint_end(paint);
        let fresh = &scratch[0].instances;
        // ponytail: a colour glyph in the patch, or a texture break inside the node, needs the
        // layer's breaks rebuilt; such a patch waits for the next frame (whose build wins).
        if fresh.iter().any(|i| i.kind == crate::draw::kind::IMAGE)
            || self.draw[layer].breaks.iter().any(|b| start < b.at_instance as usize && (b.at_instance as usize) < end)
        {
            self.patch_layers = scratch;
            self.request_frame();
            return false;
        }
        self.damage = None;
        let mut target = std::mem::take(&mut self.draw[layer]);
        if fresh.len() <= end - start {
            // in place; a shorter patch leaves empty instances in the rest of the range
            for k in start..end {
                let new = fresh.get(k - start).copied().unwrap_or(EMPTY);
                if target.instances[k] != new {
                    let old = target.instances[k];
                    self.damage_instance(&old);
                    self.damage_instance(&new);
                    target.instances[k] = new;
                    add_range(&mut target.dirty, k as u32, k as u32 + 1);
                }
            }
        } else {
            // ponytail: a longer patch shifts the layer's tail and scans every view and node,
            // O(instances + nodes); a node with slack of its own if growing patches get hot.
            for k in start..end {
                let old = target.instances[k];
                self.damage_instance(&old);
            }
            for new in fresh {
                self.damage_instance(new);
            }
            target.instances.splice(start..end, fresh.iter().copied());
            let delta = (fresh.len() - (end - start)) as u32;
            add_range(&mut target.dirty, start as u32, target.instances.len() as u32);
            for b in &mut target.breaks {
                if b.at_instance as usize >= end {
                    b.at_instance += delta;
                }
            }
            for (index, s) in self.slots.iter_mut().enumerate() {
                if index != slot && s.key != 0 && s.range_layer == key && s.range.0 as usize >= end {
                    s.range = (s.range.0 + delta, s.range.1 + delta);
                }
            }
            for r in &mut self.views[1..] {
                if r.valid && r.layer == key {
                    if r.run.0 as usize >= end {
                        r.run.0 += delta;
                    }
                    if r.run.1 as usize >= end {
                        r.run.1 += delta;
                    }
                }
            }
            self.slots[slot].range.1 = (start + fresh.len()) as u32;
        }
        self.draw[layer] = target;
        self.patch_layers = scratch;
        self.views[owner as usize].patched = true;
        self.draw_revision += 1;
        self.a11y_dirty = true;
        true
    }

    /// Between builds: set a node's text and rewrite only its instances, with no build or layout.
    /// The host sees `draw_revision` change, draws, and drains new glyphs from `text_mut`. The patch
    /// holds until the node's owner next declares it; then the build's value wins. `false` for a
    /// stale handle, or when the patch has to wait for the next frame.
    pub fn patch_text(&mut self, b: &Handle, text: &str) -> bool {
        if !self.live(b) {
            return false;
        }
        self.text(b, text);
        self.repaint_node(b.box_slot)
    }

    /// `patch_text` for the look's fill colour.
    pub fn patch_fill(&mut self, b: &Handle, fill: Color) -> bool {
        self.patch_look(b, &Look { fill, ..self.slots.get(b.box_slot).map_or(Look::NONE, |s| s.node.look) })
    }

    /// `patch_text` for the whole look.
    pub fn patch_look(&mut self, b: &Handle, look: &Look) -> bool {
        if !self.live(b) {
            return false;
        }
        self.slots[b.box_slot].node.look = *look;
        self.repaint_node(b.box_slot)
    }

    /// `patch_text` for the node's rectangle: its look and text move or resize. Its children, its
    /// canvases and its hit area stay until the next frame.
    pub fn patch_rect(&mut self, b: &Handle, rect: Rect) -> bool {
        if !self.live(b) {
            return false;
        }
        let s = &mut self.slots[b.box_slot];
        (s.rx, s.ry, s.rw, s.rh) = (rect.x, rect.y, rect.w, rect.h);
        self.repaint_node(b.box_slot)
    }

    /// `patch_rect` by an offset.
    pub fn patch_offset(&mut self, b: &Handle, dx: f32, dy: f32) -> bool {
        if !self.live(b) {
            return false;
        }
        let r = self.slot_rect(b.box_slot);
        self.patch_rect(b, Rect::new(r.x + dx, r.y + dy, r.w, r.h))
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
        let small = 8.max(self.font_size - 2);
        let tip_text = "drop to put";
        let tip_key = "esc";
        let tw1 = self.text_width(Some(small), tip_text);
        let tw2 = self.text_width(Some(small), tip_key);
        let tip_w = 2.0 + 12.0 + tw1 + 6.0 + tw2;
        let tip = self.box_("ui-carry-tip", BoxOpts::default().w(Size::Px(tip_w)).h(Size::Px(20.0)).at((px + 14.0, py + 14.0 + 20.0 + 8.0)));
        let t = self.theme();
        let (ghost_look, tip_look, key_look) = (t.ghost.idle, t.tooltip.idle, t.key.idle);
        self.set_look(&ghost, ghost_look, State::default());
        self.slots[ghost.box_slot].node.inset.l = 6.0;
        self.text(&ghost, &payload.value);
        self.set_look(&tip, tip_look, State::default());
        self.slots[tip.box_slot].node.inset.l = 7.0;
        self.text(&tip, tip_text);
        let inner = self.enter(&tip);
        let k = self.node_keyed(int_key(tip.box_key, 0), Node { w: Size::Px(tw2.max(1.0)), h: Size::Px(20.0), at: Some((7.0 + tw1 + 6.0, 0.0)), look: key_look, ..Node::DEFAULT });
        self.text(&k, tip_key);
        self.leave(inner);
        // Tooltip and ghost layers precede existing foreground entries at maximum priority.
        self.foreground.push((i64::MAX, tip.index));
        self.foreground.push((i64::MAX, ghost.index));
        self.leave(saved);
    }

    fn clear_frame_callbacks(&mut self) {
        for index in 0..self.count {
            let b = &mut self.boxes[index];
            b.hit = None;
            b.hit_rect = None;
            b.paint = arena::EMPTY;
            b.over = arena::EMPTY;
        }
        self.arena.reset();
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
        self.phase_at = self.timing.then(std::time::Instant::now);
        let (scale_x, scale_y) = frame.pixel_scale;
        let density = 1.max(scale_x.max(scale_y).round() as i64);
        let size = (frame.input.size.0 as f32, frame.input.size.1 as f32);
        if density != self.density || size != (self.view_w, self.view_h) {
            self.epoch += 1;
        }
        self.density = density;
        self.frame_number += 1;
        // a request made between frames is this frame; one made while building is the next
        self.frame_requested = false;
        self.out.clipboard = None;
        self.out.paste = false;
        self.out.background = None;
        // the window's title bar shows the ground the UI is painted on
        if self.window_ground != Some(self.theme.tokens.panel) {
            self.window_ground = Some(self.theme.tokens.panel);
            self.out.background = Some(self.theme.tokens.panel);
        }
        self.route(frame);
        self.apply_a11y();
        self.mark_input();
        // views whose `repaint_after` is due rebuild now (the root rebuilds anyway)
        if self.wake <= frame.input.t {
            self.wake = f64::INFINITY;
        }
        for v in 1..self.views.len() {
            if self.views[v].wake <= frame.input.t {
                self.views[v].wake = f64::INFINITY;
                self.mark_view(v as u32);
            }
        }
        self.views[0].built = self.frame_number;
        self.modal_key = None;
        self.modal_extra.clear();
        // Also release callbacks left by a build that raised before publishing.
        self.clear_frame_callbacks();
        self.count = 0;
        self.rebuilt = 0;
        self.closed_section = NONE;
        (self.theme_depth, self.theme_id) = (0, 0);
        (self.now, self.motion) = (frame.input.t, self.theme.tokens.moves());
        self.building = true;
        self.parents.clear();
        self.seeds.clear();
        let root = &mut self.views[0];
        std::mem::swap(&mut root.nodes, &mut root.prev);
        root.nodes.clear();
        let root = self.append_box(ROOT_KEY, self.table.find(ROOT_KEY), NONE);
        let (w, h) = (frame.input.size.0 as f32, frame.input.size.1 as f32);
        self.set_box(root, Node { w: Size::Px(w), h: Size::Px(h), at: Some((0.0, 0.0)), scroll_step: 0.0, ..Node::DEFAULT });
        self.parents.push(root);
        self.seeds.push(ROOT_KEY);
        self.phase(0);
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
        self.phase(1);
        self.intrinsic();
        self.phase(2);
        self.arrange(frame.input.t);
        self.phase(3);
        self.layout_time = self.phases[2] + self.phases[3];
        self.paint_all(frame);
        self.phase(4);
        self.emit_a11y();
        self.phase(5);
        self.step_tweens();
        self.text_mut().end_frame();
        // a glyph evicted from the atlas may be one a kept view's instances sample
        let evictions = self.text_mut().evictions();
        if evictions != self.evictions {
            self.evictions = evictions;
            self.epoch += 1;
            self.frame_requested = true;
        }
        self.foreground.clear();
        self.prune();
        if self.focus != 0 && self.table.find(self.focus).is_none() {
            self.focus = 0;
            self.edit_focus = 0;
        }
        // the build moved the focus: views already kept this frame show it next frame
        if (self.focus, self.keyboard_focus) != (self.seen.1, self.seen.2) {
            self.mark_key(self.seen.1);
            self.mark_key(self.focus);
            (self.seen.1, self.seen.2) = (self.focus, self.keyboard_focus);
        }
        self.out.repaint_at = self.next_repaint(frame.input.t);
        self.frame_requested = false;
        // Layout, hit rectangles and painting are published data now. The callbacks can capture
        // whole document versions, so keep them only while building.
        self.clear_frame_callbacks();
        self.phase(6);
    }

    // ------------------------------------------------------ layout helpers

    pub fn hover_delay(&mut self, key: &str) -> bool {
        self.escape();
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
        // 12 right of the pointer and 20 below it, or 8 above it when that does not fit
        let (px, py) = self.pointer;
        let placement = Placement { side: Side::Below, align: Align::Start, gap: 0.0, margin: 8.0, flip: true };
        let Rect { x, y, .. } = place_anchored(Rect::new(px + 12.0, py - 8.0, 0.0, 28.0), (width, height), placement, Rect::new(0.0, 0.0, self.view_w, self.view_h));
        let saved = self.enter_root();
        let tip = self.box_("ui-tooltip", BoxOpts::default().w(Size::Px(width)).h(Size::Px(height)).at((x, y)));
        self.set_role(&tip, Role::Tooltip, 0);
        self.a11y_name(&tip, text);
        self.overlays.push(tip.index);
        let look = self.theme().tooltip.idle;
        self.set_look(&tip, Look { text: CLEAR_INK, ..look }, State::default());
        let inner = self.enter(&tip);
        for (i, line) in lines.iter().enumerate() {
            let n = self.node_keyed(int_key(tip.box_key, i as i64), Node { w: Size::Px(width), h: Size::Px(20.0), at: Some((0.0, 14.0 * i as f32)), look: Look { fill: CLEAR_INK, border: CLEAR_INK, shadows: [Shadow::NONE; 2], ..look }, inset: Edges { l: 7.0, ..Edges::default() }, ..Node::DEFAULT });
            self.text(&n, line);
        }
        self.leave(inner);
        self.leave(saved);
    }

    /// A popup placed by `anchor` (window points, e.g. `ui.rect(&field)` or a point's empty rect):
    /// on the placement's side, flipped to the other when it does not fit there, kept inside the
    /// window. Its size may be `Fit`: placement runs in layout, after the size is known. Built at the
    /// root wherever it is called, and painted over the rest.
    pub fn anchored(&mut self, label: &str, anchor: Rect, placement: Placement, w: Size, h: Size, f: &mut dyn FnMut(&mut Ui)) -> Handle {
        let saved = self.enter_root_parent();
        let b = self.node(label, Node { w, h, anchor: Some(Anchor { rect: anchor, placement }), ..Node::DEFAULT });
        self.overlays.push(b.index);
        self.within(&b, f);
        self.leave(saved);
        b
    }

    pub fn col(&mut self, padding: f32, label: &str, f: &mut dyn FnMut(&mut Ui)) {
        let b = self.box_(label, BoxOpts::default().axis(Axis::Column).padding(padding));
        self.within(&b, f);
    }

    pub(crate) fn input_frame_time(&self) -> Option<f64> {
        self.input_frame.as_ref().map(|f| f.input.t)
    }
}

// ---------------------------------------------------------- transitions

impl Ui {
    /// The running transition of `slot`, if it is still that node's.
    fn tween_of(&self, slot: usize) -> Option<usize> {
        let s = &self.slots[slot];
        let t = s.tween as usize;
        (s.tween != NO_TWEEN && t < self.tweens.len() && self.tweens[t].slot == slot && self.tweens[t].generation == s.generation).then_some(t)
    }

    /// Where the node rests: its declared look, offset and opacity.
    fn rest(&self, slot: usize) -> Pose {
        let s = &self.slots[slot];
        Pose { look: s.node.look, dx: s.pose.0, dy: s.pose.1, opacity: s.pose.2 }
    }

    /// What the node shows now, and whether a transition is running (its instances stay steady).
    fn pose(&self, slot: usize) -> (Pose, bool) {
        match self.tween_of(slot) {
            None => (self.rest(slot), false),
            Some(i) => {
                let t = &self.tweens[i];
                let p = t.curve.at((self.now - t.start) as f32);
                let f = |a: f32, b: f32| a + (b - a) * p;
                let pose = Pose { look: t.from.look.lerp(&t.to.look, p), dx: f(t.from.dx, t.to.dx), dy: f(t.from.dy, t.to.dy), opacity: f(t.from.opacity, t.to.opacity) };
                (pose, true)
            }
        }
    }

    fn start_tween(&mut self, slot: usize, curve: Curve, from: Pose, to: Pose) {
        let tween = Tween { slot, generation: self.slots[slot].generation, start: self.now, curve, from, to };
        match self.tween_of(slot) {
            Some(i) => self.tweens[i] = tween,
            None => {
                self.slots[slot].tween = self.tweens.len() as u32;
                self.tweens.push(tween);
            }
        }
    }

    /// After a paint: finished transitions come to rest, and nodes the paint did not reach (in a kept
    /// view) are rewritten in place through the patch path, so a transition never rebuilds a view.
    fn step_tweens(&mut self) {
        let damage = self.damage;
        let mut i = 0;
        while i < self.tweens.len() {
            let t = self.tweens[i];
            let s = &self.slots[t.slot];
            if s.key == 0 || s.generation != t.generation || s.tween != i as u32 {
                self.drop_tween(i);
                continue;
            }
            let done = t.curve.at((self.now - t.start) as f32) >= 1.0;
            if done {
                let s = &mut self.slots[t.slot];
                (s.node.look, s.pose) = (t.to.look, (t.to.dx, t.to.dy, t.to.opacity));
                self.drop_tween(i);
            } else {
                i += 1;
            }
            if done || self.slots[t.slot].painted != self.frame_number {
                self.repaint_node(t.slot);
            }
        }
        self.damage = match (damage, self.damage) {
            (Some(a), b) => Some(union(b, a)),
            (None, b) => b,
        };
    }

    fn drop_tween(&mut self, i: usize) {
        let slot = self.tweens[i].slot;
        if self.slots[slot].tween == i as u32 {
            self.slots[slot].tween = NO_TWEEN;
        }
        self.tweens.swap_remove(i);
        if let Some(moved) = self.tweens.get(i) {
            if self.slots[moved.slot].tween == self.tweens.len() as u32 {
                self.slots[moved.slot].tween = i as u32;
            }
        }
    }

    /// Animate a node to `target` along `curve` from where it shows now: its look, its offset from
    /// where layout puts it, or its opacity. Driven by the patch path (the node's view does not
    /// rebuild); the frame gate asks for frames until it rests. An animated look holds until the
    /// node's next declaration (as `patch_look`); offset and opacity hold until animated again. The
    /// theme's `reduced_motion` makes it a jump. `false` for a stale handle.
    // ponytail: from -> to only, no keyframe lists; chain calls when a run ends if one is needed.
    // Offset and opacity move the node's own look and text, not its children (a view's offset and
    // opacity move a group), and hit testing stays where layout put it.
    pub fn animate(&mut self, b: &Handle, target: Target, curve: Curve) -> bool {
        if !self.live(b) {
            return false;
        }
        let slot = b.box_slot;
        let curve = if self.theme().tokens.reduced_motion { Curve::Bezier { duration: 0.0, points: [0.0, 0.0, 1.0, 1.0] } } else { curve };
        let from = self.pose(slot).0;
        let mut to = match self.tween_of(slot) {
            Some(i) => self.tweens[i].to,
            None => self.rest(slot),
        };
        match target {
            Target::Look(look) => to.look = look,
            Target::Offset(dx, dy) => (to.dx, to.dy) = (dx, dy),
            Target::Opacity(o) => to.opacity = o.clamp(0.0, 1.0),
        }
        self.start_tween(slot, curve, from, to);
        if !self.building {
            self.frame_requested = true;
        }
        true
    }

    /// Transitions running now.
    pub fn transitions(&self) -> usize {
        self.tweens.len()
    }
}

// ------------------------------------------------------ views (layer 2)

/// Rows built past each edge of a virtual list's visible rows.
const OVERSCAN: usize = 4;

/// IEEE half bits as f32.
fn half(h: u16) -> f32 {
    let (e, m) = (((h >> 10) & 0x1f) as i32, (h & 0x3ff) as f32);
    let v = if e == 0 { m * 2f32.powi(-24) } else if e == 31 { f32::INFINITY } else { (1.0 + m / 1024.0) * 2f32.powi(e - 15) };
    if h & 0x8000 != 0 { -v } else { v }
}

/// What an instance can cover, in its own view's points (an empty instance covers nothing).
fn bounds(i: &Instance) -> Option<Rect> {
    use crate::draw::kind;
    if *i == EMPTY {
        return None;
    }
    let [a, b, c, d] = i.rect;
    let span = |x0: f32, y0: f32, x1: f32, y1: f32, grow: f32| Rect::new(x0 - grow, y0 - grow, x1 - x0 + 2.0 * grow, y1 - y0 + 2.0 * grow);
    let r = match i.kind {
        kind::LINE => span(a.min(c), b.min(d), a.max(c), b.max(d), half(i.width)),
        kind::CURVE => {
            let (cx, cy) = (f32::from_bits(i.extra[0]), f32::from_bits(i.extra[1]));
            span(a.min(c).min(cx), b.min(d).min(cy), a.max(c).max(cx), b.max(d).max(cy), half(i.width))
        }
        kind::ARC => span(a - c, b - c, a + c, b + c, half(i.width)),
        kind::SHADOW => {
            let (dx, dy) = (half(i.extra[0] as u16), half((i.extra[0] >> 16) as u16));
            let grow = 3.0 * half(i.extra[1] as u16) + half((i.extra[1] >> 16) as u16).abs();
            span(a.min(a + dx), b.min(b + dy), c.max(c + dx), d.max(d + dy), grow)
        }
        _ => Rect::new(a, b, c - a, d - b),
    };
    let [x0, y0, x1, y1] = i.clip.map(|v| v as f32);
    let r = r.intersect(Rect::new(x0, y0, x1 - x0, y1 - y0));
    (r.w > 0.0 && r.h > 0.0 && r.x.is_finite() && r.y.is_finite()).then_some(r)
}

fn union(a: Option<Rect>, b: Rect) -> Rect {
    match a {
        None => b,
        Some(a) => {
            let (x0, y0) = (a.x.min(b.x), a.y.min(b.y));
            Rect::new(x0, y0, (a.x + a.w).max(b.x + b.w) - x0, (a.y + a.h).max(b.y + b.h) - y0)
        }
    }
}

impl Ui {
    /// A cached subtree: `f` builds its children, and is skipped entirely while `revision` is the
    /// one it was last built with and nothing else dirtied it; its nodes, their layout, hit areas
    /// and instance run stay as they were. Its size comes from outside (`Px`, `Pct`, `Rel` or
    /// `Grow`, never `Fit`). Besides the revision it rebuilds when the theme, the font size, the
    /// density or the window size change; when input reaches one of its nodes (press, release, drag,
    /// wheel, keys); when the hovered or focused node enters or leaves it, or carrying starts or
    /// stops over it; every frame it holds the keyboard focus or a scroll in motion; when its
    /// laid-out size changes (shown stale for that one frame); and on the frame after a build that
    /// put boxes at the root (popups, tooltips, `to_front`), asked for a frame or ran a hover delay.
    /// Pointer motion inside one node rebuilds nothing: a view that draws from the pointer's
    /// position bumps its own revision. A rebuild of the view rebuilds the views around it; the
    /// views inside it may still be kept. Returns the view's box, for `view_offset` and friends.
    pub fn view(&mut self, label: &str, revision: u64, w: Size, h: Size, f: &mut dyn FnMut(&mut Ui)) -> Handle {
        assert!(!matches!(w, Size::Fit) && !matches!(h, Size::Fit), "Ui.view {label}: a view's size comes from outside (Px, Pct, Rel or Grow), never Fit");
        let b = self.node(label, Node { w, h, ..Node::DEFAULT });
        let parent = self.view_cur;
        let v = match self.slots[b.box_slot].view {
            NO_VIEW => {
                let v = self.free_views.pop().unwrap_or_else(|| {
                    self.views.push(ViewRec::new());
                    self.views.len() as u32 - 1
                });
                let index = self.free_indices.pop().unwrap_or(0);
                let r = &mut self.views[v as usize];
                r.name.push_str(label);
                (r.slot, r.index) = (b.box_slot, index);
                if index != 0 {
                    self.index_owner[index as usize] = v;
                }
                self.slots[b.box_slot].view = v;
                v
            }
            v => v,
        };
        self.boxes[b.index].view = v;
        let xf = if self.views[v as usize].index != 0 { v } else { self.views[parent as usize].xf };
        let r = &mut self.views[v as usize];
        (r.parent, r.xf) = (parent, xf);
        let clean = r.built >= 0 && r.valid && !r.dirty && !r.escapes && r.revision == revision && r.epoch == self.epoch && r.theme == self.theme_id;
        if clean && !self.verify {
            r.kept = self.frame_number;
            self.boxes[b.index].kept = true;
            return b;
        }
        r.verify = clean;
        r.escapes = false;
        self.rebuilt += 1;
        r.wake = f64::INFINITY;
        std::mem::swap(&mut r.nodes, &mut r.prev);
        r.nodes.clear();
        self.a11y_dirty = true;
        self.view_cur = v;
        let saved = self.enter(&b);
        f(self);
        self.leave(saved);
        self.view_cur = parent;
        let r = &mut self.views[v as usize];
        (r.revision, r.epoch, r.theme, r.built, r.patched) = (revision, self.epoch, self.theme_id, self.frame_number, false);
        r.dirty = r.escapes;
        if r.escapes && parent != 0 {
            self.views[parent as usize].escapes = true;
        }
        b
    }

    /// A scrolling list of `count` rows `row_h` points high, filling its parent (`Grow` both ways),
    /// that builds only the rows in view and `OVERSCAN` more past each edge: `f(ui, i)` builds row
    /// `i`'s content inside a row box keyed by `i`, so a row's state and handles stay while it is
    /// built. Returns the list's scroll box.
    // ponytail: rows of one known height; a measured-height cache when rows vary. Rows are chosen
    // from the scroll position before layout: a fling past the overscan shows empty rows for a frame.
    pub fn list(&mut self, label: &str, count: usize, row_h: f32, f: &mut dyn FnMut(&mut Ui, usize)) -> Handle {
        assert!(row_h > 0.0, "Ui.list {label}: rows need a positive height");
        let body = self.node(label, Node { flags: SCROLL | CLIP, w: Size::Grow, h: Size::Grow, scroll_step: row_h, role: Role::List, ..Node::DEFAULT });
        let seen = self.slots[body.box_slot].rh;
        let height = if seen > 0.0 { seen } else { self.view_h };
        // the wheel and the trackpad move the list before its rows are chosen (as `table` does)
        self.set_l_h(&body, height);
        self.set_l_content(&body, count as f32 * row_h);
        if let Some(t) = self.input_frame_time() {
            self.apply_scroll(true, t, body.index);
        }
        let top = (self.scroll_offset(&body) / row_h).floor().max(0.0) as usize;
        let first = top.saturating_sub(OVERSCAN).min(count);
        let last = (top + (height / row_h).ceil() as usize + 1 + OVERSCAN).min(count);
        let saved = self.enter(&body);
        let extent = self.node_keyed(int_key(body.box_key, -1), Node { h: Size::Px(count as f32 * row_h), ..Node::DEFAULT });
        let inner = self.enter(&extent);
        for i in first..last {
            let row = self.node_keyed(int_key(body.box_key, i as i64), Node { h: Size::Px(row_h), at: Some((0.0, i as f32 * row_h)), role: Role::ListItem, ..Node::DEFAULT });
            let saved = self.enter(&row);
            f(self, i);
            self.leave(saved);
        }
        self.leave(inner);
        self.leave(saved);
        body
    }

    /// The view `b` holds, if it is a live view.
    fn view_id(&self, b: &Handle) -> Option<u32> {
        if self.live(b) { Some(self.slots[b.box_slot].view).filter(|v| *v != NO_VIEW) } else { None }
    }

    /// Move a view's output by `(dx, dy)` points from where layout put it: one table entry changes,
    /// nothing is rebuilt or rewritten. Hit testing and `rect` follow it.
    pub fn view_offset(&mut self, b: &Handle, dx: f32, dy: f32) {
        if let Some(v) = self.view_id(b) {
            self.views[v as usize].offset = (dx, dy);
            self.view_changed(v);
        }
    }

    /// Scale a view's alpha by `opacity` (on each instance; overlaps inside it show through).
    pub fn view_opacity(&mut self, b: &Handle, opacity: f32) {
        if let Some(v) = self.view_id(b) {
            self.views[v as usize].opacity = opacity;
            self.view_changed(v);
        }
    }

    /// Scale a view's output by `scale` about its box's centre (`view_transform` with no rotation).
    pub fn view_scale(&mut self, b: &Handle, scale: f32) {
        if let Some(v) = self.view_id(b) {
            let r = self.views[v as usize].built_rect;
            self.view_transform(b, scale, [1.0, 0.0, 0.0, 1.0], (r.w / 2.0, r.h / 2.0));
        }
    }

    /// Map a view's output through `pivot + scale * transform * (p - pivot)` (`transform` a row-major
    /// 2x2: rotation, skew, flip; `pivot` from its box's top-left corner): one table entry, no rebuild.
    /// Hit testing follows through the inverse.
    // ponytail: a view's transform applies to its own instances only, not to views nested in it (the
    // painter's table is flat), and `rect` reports the untransformed box. Compose transforms down the
    // view tree when nested transformed views are needed.
    pub fn view_transform(&mut self, b: &Handle, scale: f32, transform: [f32; 4], pivot: (f32, f32)) {
        if let Some(v) = self.view_id(b) {
            let r = &mut self.views[v as usize];
            (r.scale, r.transform, r.pivot) = (scale, transform, pivot);
            self.view_changed(v);
        }
    }

    /// Ask the host to blur what is under a view by `sigma` points before drawing it (CSS
    /// `backdrop-filter: blur`); 0 stops it. Reported in `FrameOut::blurs`, with the view box's look
    /// radius (`node_mut(&view).look.radius`) rounding the blurred image.
    pub fn view_backdrop_blur(&mut self, b: &Handle, sigma: f32) {
        if let Some(v) = self.view_id(b) {
            self.views[v as usize].blur = sigma.max(0.0);
            self.view_changed(v);
        }
    }

    /// The blur records of the shown views, where their runs now sit.
    fn collect_blurs(&mut self) {
        self.blur_out.clear();
        for r in &self.views[1..] {
            if r.blur > 0.0 && r.valid && r.visited == self.painted {
                if let Some(layer) = self.draw[..self.draw_len].iter().position(|l| l.key == r.layer) {
                    let rect = r.entry().cover(r.screen);
                    let radii = self.slots[r.slot].node.look.radius;
                    self.blur_out.push(BlurOut { layer, at_instance: r.run.0, rect, sigma: r.blur, radii, opacity: r.alpha });
                }
            }
        }
    }

    /// Clip a view's output to `clip` (window points), besides its parents' clips; `None` lifts it.
    pub fn view_clip(&mut self, b: &Handle, clip: Option<Rect>) {
        if let Some(v) = self.view_id(b) {
            self.views[v as usize].clip = clip;
            self.view_changed(v);
        }
    }

    /// A view's entry changed: between frames the table and the damage update now; a view with no
    /// table index bakes its offset into layout, so it rebuilds next frame.
    fn view_changed(&mut self, v: u32) {
        self.a11y_dirty = true;
        if self.views[v as usize].index == 0 {
            // ponytail: past 255 views the offset is baked by a rebuild (opacity and clip are not
            // applied); a larger table or nested tables if that many views move.
            self.mark_view(v);
            self.views[v as usize].dirty = true;
            self.frame_requested = true;
        } else if !self.building && !self.painting && self.painted >= 0 {
            self.damage = None;
            if self.refresh_views() {
                self.draw_revision += 1;
            }
            self.collect_blurs();
        }
    }

    /// Mark `v` and the views around it to rebuild at their next declaration; one already kept this
    /// frame rebuilds next frame.
    fn mark_view(&mut self, mut v: u32) {
        while v != 0 && v != NO_VIEW {
            let r = &mut self.views[v as usize];
            r.dirty = true;
            if r.kept == self.frame_number {
                self.frame_requested = true;
            }
            v = r.parent;
        }
    }

    fn mark_key(&mut self, key: i64) {
        if key != 0 {
            if let Some(slot) = self.table.find(key) {
                self.mark_view(self.slots[slot].owner);
            }
        }
    }

    /// Dirty the views whose look follows what input changed since the last frame left it.
    fn mark_input(&mut self) {
        let mut marks = std::mem::take(&mut self.marks);
        marks.clear();
        marks.extend(self.signals.keys().copied());
        let (hot, focus, keyboard, carrying) = self.seen;
        let now = (self.hot, self.focus, self.keyboard_focus, self.payload.is_some());
        if now.0 != hot || now.3 != carrying {
            marks.extend([hot, now.0]);
        }
        if now.1 != focus || now.2 != keyboard {
            marks.push(focus);
        }
        // the focused view paints the caret and the input method's area every frame
        marks.push(now.1);
        for key in &marks {
            self.mark_key(*key);
        }
        self.marks = marks;
        self.seen = now;
    }

    fn free_view(&mut self, v: u32) {
        let r = &mut self.views[v as usize];
        if r.index != 0 {
            self.free_indices.push(r.index);
        }
        let (mut name, mut breaks, mut hits, mut nodes, mut prev) = (std::mem::take(&mut r.name), std::mem::take(&mut r.breaks), std::mem::take(&mut r.hits), std::mem::take(&mut r.nodes), std::mem::take(&mut r.prev));
        name.clear();
        breaks.clear();
        hits.clear();
        nodes.clear();
        prev.clear();
        *r = ViewRec { name, breaks, hits, nodes, prev, outer: Rect::new(0.0, 0.0, 0.0, 0.0), ..ViewRec::new() };
        self.free_views.push(v);
    }

    /// The view's output is in the latest paint: its box was reached, or it sits in a kept view
    /// that was.
    fn shown(&self, v: u32) -> bool {
        if self.views[v as usize].visited == self.painted {
            return true;
        }
        let mut x = self.views[v as usize].parent;
        while x != 0 {
            let r = &self.views[x as usize];
            if r.visited == self.painted {
                return r.kept == self.painted;
            }
            x = r.parent;
        }
        false
    }

    /// Compose view `v`'s entry from its parent's.
    fn compose(&mut self, v: u32) {
        let stamp = self.stamp;
        if v == 0 || self.views[v as usize].stamp == stamp {
            return;
        }
        let parent = self.views[v as usize].parent;
        self.compose(parent);
        let pxf = self.views[parent as usize].xf;
        let (pshift, palpha, pouter) = {
            let p = &self.views[pxf as usize];
            (p.shift, p.alpha, p.outer)
        };
        let r = &mut self.views[v as usize];
        r.stamp = stamp;
        if r.index == 0 {
            (r.shift, r.alpha, r.outer) = (pshift, palpha, pouter);
        } else {
            r.shift = (pshift.0 + r.offset.0 + r.delta.0, pshift.1 + r.offset.1 + r.delta.1);
            r.alpha = palpha * r.opacity;
            let c = r.box_clip;
            r.outer = Rect::new(c.x + pshift.0, c.y + pshift.1, c.w, c.h).intersect(pouter).intersect(r.clip.unwrap_or(UNBOUNDED));
        }
        let b = r.built_rect;
        r.screen = Rect::new(b.x + r.shift.0, b.y + r.shift.1, b.w, b.h).intersect(r.outer);
    }

    /// Recompose every shown view's entry into `view_out`; the damage gets each moved view's old
    /// and new rectangles. Whether any entry changed.
    fn refresh_views(&mut self) -> bool {
        self.stamp += 1;
        let mut out = std::mem::take(&mut self.view_prev);
        out.clear();
        for v in 1..self.views.len() as u32 {
            if self.views[v as usize].index == 0 || !self.shown(v) {
                continue;
            }
            let (old_entry, old_screen) = (self.views[v as usize].entry(), self.views[v as usize].screen);
            self.compose(v);
            let r = &self.views[v as usize];
            if r.entry() != old_entry {
                let (screen, entry) = (r.screen, r.entry());
                self.add_damage(old_entry.cover(old_screen));
                self.add_damage(entry.cover(screen));
            }
            let r = &self.views[v as usize];
            out.push((r.index, r.entry()));
        }
        let changed = out != self.view_out;
        self.view_prev = std::mem::replace(&mut self.view_out, out);
        changed
    }

    /// Add an instance's cover to the damage, moved and clipped as its view shows it.
    fn damage_instance(&mut self, i: &Instance) {
        if let Some(r) = bounds(i) {
            let v = &self.views[self.index_owner[i.view as usize] as usize];
            let r = Rect::new(r.x + v.shift.0, r.y + v.shift.1, r.w, r.h);
            let r = if v.plain() { r } else { v.entry().cover(r) }.intersect(v.outer);
            self.add_damage(r);
        }
    }

    fn add_damage(&mut self, r: Rect) {
        if r.w > 0.0 && r.h > 0.0 {
            self.damage = Some(union(self.damage, r));
        }
    }

    /// Paint a view box's children: a kept view adds its run and its hit areas, a fresh one paints
    /// inside its own clip (its outer clip is its table entry's) and records both.
    fn visit_view(&mut self, paint: &mut Paint, index: usize, v: u32, clip: Rect, parent_hit: i64) {
        let rect = self.screen(index);
        let layer = paint.layers[paint.layer].key;
        let painted = self.painted;
        let r = &mut self.views[v as usize];
        r.visited = painted;
        r.box_clip = clip;
        if self.boxes[index].kept {
            r.delta = (rect.x - r.built_rect.x, rect.y - r.built_rect.y);
            let resized = rect.w != r.built_rect.w || rect.h != r.built_rect.h;
            let moved = r.index == 0 && r.delta != (0.0, 0.0);
            let here = r.valid && r.layer == layer;
            if resized || moved || !here {
                // ponytail: its old output shows for this frame (none when it changed layers); its
                // next build, a frame later, fixes it.
                r.dirty = true;
                self.frame_requested = true;
            }
            if here {
                paint.keep(v);
            }
            let hit_parent = r.hit_parent;
            for k in 0..self.views[v as usize].hits.len() {
                let mut h = self.views[v as usize].hits[k];
                if h.parent == hit_parent {
                    h.parent = parent_hit;
                }
                self.push_hit(h);
            }
            return;
        }
        if r.verify && r.built_rect != rect {
            r.verify = false;
        }
        r.built_rect = rect;
        r.delta = (0.0, 0.0);
        r.layer = layer;
        let (table, xf) = (r.index, r.xf);
        let saved = (paint.view, self.paint_xf);
        if table != 0 {
            paint.view = table;
        }
        self.paint_xf = xf;
        let child_clip = if table != 0 { UNBOUNDED } else { clip };
        paint.cut();
        let (first, hit_start) = (paint.layers[paint.layer].pieces.len() - 1, self.hit_count);
        let mut child = self.boxes[index].first;
        while child != NONE {
            self.visit(paint, child, child_clip, parent_hit);
            child = self.boxes[child].next;
        }
        paint.cut();
        let r = &mut self.views[v as usize];
        r.pieces = (paint.layer, first, paint.layers[paint.layer].pieces.len() - 1);
        r.hit_parent = parent_hit;
        r.hits.clear();
        r.hits.extend_from_slice(&self.hits[hit_start..self.hit_count]);
        (paint.view, self.paint_xf) = saved;
    }

    /// Children of a view box with no table index are laid out at its offset.
    fn baked(&self, index: usize) -> (f32, f32) {
        match self.boxes[index].view {
            NO_VIEW => (0.0, 0.0),
            v => {
                let r = &self.views[v as usize];
                if r.index == 0 { r.offset } else { (0.0, 0.0) }
            }
        }
    }

    /// The node's instance range indexes the current output: its view's run is shown and was
    /// painted by the build that painted the node.
    fn range_valid(&self, slot: usize) -> bool {
        let s = &self.slots[slot];
        if s.owner == 0 {
            return s.painted == self.painted;
        }
        let v = &self.views[s.owner as usize];
        v.valid && s.painted == v.built && self.shown(s.owner)
    }

    /// Re-run each verified view's build check: its fresh instances (empty ones aside) equal the
    /// run it kept.
    fn verify_runs(&self, scratch: &[Layer]) {
        for r in &self.views[1..] {
            if !(r.verify && r.valid && !r.patched && r.built == self.painted && r.visited == self.painted) {
                continue;
            }
            let Some(old) = self.draw[..self.draw_len].iter().find(|l| l.key == r.layer) else { continue };
            let layer = &scratch[r.pieces.0];
            let pieces = &layer.pieces[r.pieces.1..r.pieces.2];
            if pieces.iter().any(|p| p.view != NO_VIEW) {
                continue;
            }
            let new = pieces.iter().flat_map(|p| layer.instances[p.a as usize..p.b as usize].iter());
            let old = old.instances[r.run.0 as usize..r.run.1 as usize].iter();
            let same = old.filter(|i| **i != EMPTY).eq(new.filter(|i| **i != EMPTY));
            assert!(same, "Ui.verify_views: view {:?} built different output under an unchanged revision: its revision misses an input", r.name);
        }
    }

    /// Merge the frame's scratch layers into the retained ones: fresh pieces land where they fall,
    /// kept runs stay. Gathers dirty ranges and damage; whether anything changed.
    fn place_all(&mut self, scratch: &mut [Layer], len: usize) -> bool {
        let mut changed = len != self.draw_len;
        for layer in &mut self.draw {
            layer.live = false;
        }
        for i in 0..len {
            let key = scratch[i].key;
            let j = match (i..self.draw.len()).find(|j| self.draw[*j].key == key) {
                Some(j) => j,
                None => {
                    // a new layer: storage no later layer of this frame wants, emptied
                    changed = true;
                    let j = match (i..self.draw.len()).find(|j| !scratch[i + 1..len].iter().any(|s| s.key == self.draw[*j].key)) {
                        Some(j) => j,
                        None => {
                            self.draw.push(Layer::new(i64::MIN, self.draw.len() as u32));
                            self.draw.len() - 1
                        }
                    };
                    let old = std::mem::take(&mut self.draw[j].instances);
                    for k in 0..old.len() {
                        self.damage_instance(&old[k]);
                    }
                    let d = &mut self.draw[j];
                    d.instances = old;
                    d.instances.clear();
                    d.key = key;
                    d.breaks.clear();
                    d.images.clear();
                    d.dirty.clear();
                    j
                }
            };
            if i != j {
                changed = true;
                self.draw.swap(i, j);
            }
            self.draw[i].live = true;
            changed |= self.place(&mut scratch[i], i);
        }
        // layers of the last frame that are gone
        for j in len..self.draw.len() {
            if self.draw[j].live || self.draw[j].instances.is_empty() {
                continue;
            }
            let old = std::mem::take(&mut self.draw[j].instances);
            for k in 0..old.len() {
                self.damage_instance(&old[k]);
            }
            self.draw[j].instances = old;
            self.draw[j].instances.clear();
            self.draw[j].key = i64::MIN;
        }
        self.draw_len = len;
        changed
    }

    /// One layer: the fast path writes fresh pieces in place and fills the room before each kept run
    /// with empty instances; when a piece does not fit (or kept runs changed order) the layer is
    /// compacted with slack before every kept run.
    fn place(&mut self, s: &mut Layer, at: usize) -> bool {
        let n = s.instances.len() as u32;
        if let Some(last) = s.pieces.last_mut() {
            last.b = n;
        }
        let mut d = std::mem::take(&mut self.draw[at]);
        let old_len = d.instances.len();
        let mut changed = false;
        // the room each fresh piece has: up to the next non-empty kept run
        let mut limit = u32::MAX;
        for p in s.pieces.iter_mut().rev() {
            if p.view == NO_VIEW {
                p.at = limit;
            } else {
                let (a, b) = self.views[p.view as usize].run;
                if a < b {
                    limit = a;
                }
            }
        }
        let mut w = 0u32;
        let mut fits = true;
        for k in 0..s.pieces.len() {
            let p = s.pieces[k];
            if p.view == NO_VIEW {
                let len = p.b - p.a;
                if w + len > p.at {
                    fits = false;
                    break;
                }
                s.pieces[k].at = w;
                if (w + len) as usize > d.instances.len() {
                    d.instances.resize((w + len) as usize, EMPTY);
                }
                for x in 0..len as usize {
                    let (i, new) = (w as usize + x, s.instances[p.a as usize + x]);
                    if i >= old_len || d.instances[i] != new {
                        if i < old_len {
                            let old = d.instances[i];
                            self.damage_instance(&old);
                        }
                        self.damage_instance(&new);
                        d.instances[i] = new;
                        add_range(&mut d.dirty, i as u32, i as u32 + 1);
                        changed = true;
                    }
                }
                w += len;
            } else {
                let (a, b) = self.views[p.view as usize].run;
                s.pieces[k].at = a;
                if a == b {
                    s.pieces[k].at = w;
                    continue;
                }
                if a < w {
                    fits = false;
                    break;
                }
                for i in w as usize..a as usize {
                    if d.instances[i] != EMPTY {
                        let old = d.instances[i];
                        self.damage_instance(&old);
                        d.instances[i] = EMPTY;
                        add_range(&mut d.dirty, i as u32, i as u32 + 1);
                        changed = true;
                    }
                }
                w = b;
            }
        }
        if fits {
            for i in w as usize..d.instances.len() {
                let old = d.instances[i];
                self.damage_instance(&old);
                changed = true;
            }
            d.instances.truncate(w as usize);
        } else {
            changed = true;
            self.compact_layer(s, &mut d, old_len);
        }
        changed |= self.rebuild_breaks(s, &mut d);
        self.draw[at] = d;
        changed
    }

    fn compact_layer(&mut self, s: &mut Layer, d: &mut Layer, old_len: usize) {
        let mut out = std::mem::take(&mut self.compact);
        out.clear();
        for k in 0..s.pieces.len() {
            let p = s.pieces[k];
            if p.view == NO_VIEW {
                s.pieces[k].at = out.len() as u32;
                out.extend_from_slice(&s.instances[p.a as usize..p.b as usize]);
                continue;
            }
            let (a, b) = self.views[p.view as usize].run;
            if a == b {
                s.pieces[k].at = out.len() as u32;
                continue;
            }
            out.resize(out.len() + SLACK, EMPTY);
            let to = out.len() as u32;
            out.extend_from_slice(&d.instances[a as usize..b as usize]);
            s.pieces[k].at = to;
            self.move_run(d.key, a, b, to);
        }
        for i in 0..out.len().max(old_len) {
            let (old, new) = (d.instances.get(i).copied(), out.get(i).copied());
            if old != new {
                if let Some(old) = old {
                    self.damage_instance(&old);
                }
                if let Some(new) = new {
                    self.damage_instance(&new);
                    add_range(&mut d.dirty, i as u32, i as u32 + 1);
                }
            }
        }
        std::mem::swap(&mut d.instances, &mut out);
        self.compact = out;
    }

    /// A kept run `a..b` of layer `key` moved to `to`: the runs inside it and the ranges of nodes
    /// painted into it follow.
    // ponytail: scans every view and node, O(views + nodes), on compaction only.
    fn move_run(&mut self, key: i64, a: u32, b: u32, to: u32) {
        let shift = |v: u32| v - a + to;
        for r in &mut self.views[1..] {
            if r.valid && r.built != self.painted && r.layer == key && r.run.0 >= a && r.run.1 <= b {
                r.run = (shift(r.run.0), shift(r.run.1));
            }
        }
        for s in &mut self.slots {
            if s.key != 0 && s.painted != self.painted && s.range_layer == key && s.range.0 >= a && s.range.1 <= b {
                s.range = (shift(s.range.0), shift(s.range.1));
            }
        }
    }

    /// The layer's texture breaks and images from its pieces; whether they changed.
    fn rebuild_breaks(&mut self, s: &Layer, d: &mut Layer) -> bool {
        let kept_breaks = s.pieces.iter().any(|p| p.view != NO_VIEW && {
            let r = &self.views[p.view as usize];
            r.tex0.is_some() || !r.breaks.is_empty()
        });
        if s.breaks.is_empty() && d.breaks.is_empty() && !kept_breaks {
            return false;
        }
        let mut breaks = std::mem::take(&mut self.temp_breaks);
        let mut images = std::mem::take(&mut self.temp_images);
        breaks.clear();
        images.clear();
        let mut current: Option<Image> = None;
        let mut set = |at: u32, tex: &Option<Image>, breaks: &mut Vec<Break>, images: &mut Vec<Image>| {
            let same = match (&current, tex) {
                (None, None) => true,
                (Some(a), Some(b)) => Rc::ptr_eq(&a.handle, &b.handle),
                _ => false,
            };
            if !same {
                let image = match tex {
                    None => crate::draw::ATLAS,
                    Some(t) => match images.iter().position(|i| Rc::ptr_eq(&i.handle, &t.handle)) {
                        Some(k) => k as u32,
                        None => {
                            images.push(t.clone());
                            images.len() as u32 - 1
                        }
                    },
                };
                match breaks.last_mut() {
                    Some(last) if last.at_instance == at => last.image = image,
                    _ => breaks.push(Break { at_instance: at, image }),
                }
                current = tex.clone();
            }
        };
        let tex_of = |image: u32| if image == crate::draw::ATLAS { None } else { Some(s.images[image as usize].clone()) };
        let (mut si, mut stex) = (0, None);
        for p in &s.pieces {
            if p.view == NO_VIEW {
                while si < s.breaks.len() && s.breaks[si].at_instance <= p.a {
                    stex = tex_of(s.breaks[si].image);
                    si += 1;
                }
                set(p.at, &stex, &mut breaks, &mut images);
                while si < s.breaks.len() && s.breaks[si].at_instance < p.b {
                    stex = tex_of(s.breaks[si].image);
                    set(p.at + s.breaks[si].at_instance - p.a, &stex, &mut breaks, &mut images);
                    si += 1;
                }
            } else {
                let r = &self.views[p.view as usize];
                set(p.at, &r.tex0, &mut breaks, &mut images);
                for (at, image) in &r.breaks {
                    set(p.at + at, &Some(image.clone()), &mut breaks, &mut images);
                }
            }
        }
        // a texture break with no instance after it is dropped by the painter's rule anyway
        let changed = breaks != d.breaks || images.len() != d.images.len() || images.iter().zip(&d.images).any(|(a, b)| !Rc::ptr_eq(&a.handle, &b.handle));
        std::mem::swap(&mut breaks, &mut d.breaks);
        std::mem::swap(&mut images, &mut d.images);
        self.temp_breaks = breaks;
        self.temp_images = images;
        changed
    }

    /// After placement: fresh views take their runs (and the breaks inside them), views not shown
    /// lose theirs, and the nodes the frame painted take their places in the retained layers.
    fn settle(&mut self, scratch: &[Layer]) {
        let painted = self.painted;
        for v in 1..self.views.len() {
            let r = &self.views[v];
            if r.slot == NONE {
                continue;
            }
            if r.visited == painted && r.built == painted {
                let (li, first, end) = r.pieces;
                let pieces = &scratch[li].pieces;
                let start = pieces[first].at;
                let last = pieces[end - 1];
                let stop = match last.view {
                    NO_VIEW => last.at + (last.b - last.a),
                    k => {
                        let (a, b) = self.views[k as usize].run;
                        if a < b { b } else { last.at }
                    }
                };
                let key = scratch[li].key;
                let d = self.draw[..self.draw_len].iter().find(|l| l.key == key).expect("placed layer");
                let mut tex0 = None;
                let mut breaks = std::mem::take(&mut self.views[v].breaks);
                breaks.clear();
                for b in &d.breaks {
                    let tex = if b.image == crate::draw::ATLAS { None } else { Some(d.images[b.image as usize].clone()) };
                    if b.at_instance <= start {
                        tex0 = tex;
                    } else if b.at_instance < stop {
                        // ponytail: a break back to the atlas inside a kept view is not replayed;
                        // the image it follows keeps sampling. Store Option<Image> if a view mixes them.
                        if let Some(t) = tex {
                            breaks.push((b.at_instance - start, t));
                        }
                    }
                }
                let r = &mut self.views[v];
                (r.run, r.valid, r.tex0, r.breaks) = ((start, stop), true, tex0, breaks);
            } else if !self.shown(v as u32) {
                self.views[v].valid = false;
            }
        }
        for k in 0..self.painted_nodes.len() {
            let (slot, li) = self.painted_nodes[k];
            let pieces = &scratch[li].pieces;
            let (a, b) = self.slots[slot].range;
            // pieces are in scratch order (a kept piece is empty at its place): the first that ends
            // past the node's start holds it
            let p = pieces[pieces.partition_point(|p| p.b <= a).min(pieces.len() - 1)];
            let s = &mut self.slots[slot];
            s.range = (p.at + a - p.a, p.at + b - p.a);
            s.range_layer = scratch[li].key;
        }
        self.painted_nodes.clear();
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
        self.escape();
        self.modal_key = key;
    }

    pub(crate) fn push_modal_extra(&mut self, key: i64) {
        self.modal_extra.insert(0, key);
    }

    /// Popups live at the root: the parents are the root alone, the seeds stay.
    pub(crate) fn enter_root_parent(&mut self) -> Saved {
        self.escape();
        let saved = Saved::Depths(self.parents.len(), self.seeds.len());
        self.parents.push(0);
        saved
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
        self.slots[b.box_slot].node.flags = flags;
    }

}
