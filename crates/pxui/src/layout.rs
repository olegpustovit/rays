//! Layout data a node carries (plain `Copy` values, no allocation) and the pure placement rule of
//! anchored popups. The passes that read them are `Ui::intrinsic` and `Ui::arrange` in ui_core.
use crate::geom::Rect;

/// Where children sit along the main axis (CSS `justify-content`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Justify {
    #[default]
    Start,
    Center,
    End,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

/// Where a child sits across its line (CSS `align-items`, `align-self`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Align {
    #[default]
    Start,
    Center,
    End,
    /// a `Fit` cross size fills the line (a `Grow` one always does)
    Stretch,
    /// the first text baselines line up (a node with no text: its bottom edge)
    Baseline,
}

/// Row flow direction: `Rtl` mirrors flow children inside their parent (`Edges::l` is the start edge,
/// `Justify::Start` the right). `at` offsets stay physical.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Direction {
    #[default]
    Ltr,
    Rtl,
}

/// A grid track: fixed points, a share of the room left (`fr`), or its content: its largest
/// single-span child, plus an equal share of what a spanning child needs past its tracks.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Track {
    Px(f32),
    Fr(f32),
    Fit,
}

/// Grid tracks. `rows` empty: rows are added as children need them, each sized to its content.
/// Gaps are the node's `gap` (between columns) and `row_gap`.
// ponytail: at most MAX_TRACKS columns and rows (stack arrays in the passes, no allocation); raise
// the cap when a grid needs more. Spans are spread in build order, not smallest span first as CSS.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Grid {
    pub cols: &'static [Track],
    pub rows: &'static [Track],
}

pub const MAX_TRACKS: usize = 16;

/// A grid child's cell. `col == AUTO` places it at the next free cell in row order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cell {
    pub col: u16,
    pub row: u16,
    pub col_span: u16,
    pub row_span: u16,
}

impl Cell {
    pub const AUTO: u16 = u16::MAX;
    pub const DEFAULT: Cell = Cell { col: Cell::AUTO, row: 0, col_span: 1, row_span: 1 };
    pub const fn at(col: u16, row: u16) -> Cell {
        Cell { col, row, col_span: 1, row_span: 1 }
    }
    pub const fn span(self, col_span: u16, row_span: u16) -> Cell {
        Cell { col_span, row_span, ..self }
    }
}

/// Which side of its anchor a popup opens on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Below,
    Above,
    Right,
    Left,
}

/// How a popup sits by its anchor: on `side`, `gap` points off it, `align`ed along it (Start, Center,
/// End), kept `margin` points inside the bounds. With `flip` it opens on the opposite side when its
/// own does not fit and that one does; without, it is moved inside the bounds.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Placement {
    pub side: Side,
    pub align: Align,
    pub gap: f32,
    pub margin: f32,
    pub flip: bool,
}

impl Placement {
    pub const BELOW: Placement = Placement { side: Side::Below, align: Align::Start, gap: 0.0, margin: 0.0, flip: true };
}

/// A node placed by an anchor rectangle (window points) rather than its parent's flow.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Anchor {
    pub rect: Rect,
    pub placement: Placement,
}

/// The rectangle a popup of `size` takes by `anchor` inside `bounds`.
pub fn place_anchored(anchor: Rect, size: (f32, f32), p: Placement, bounds: Rect) -> Rect {
    let (w, h) = size;
    let m = p.margin;
    let vertical = matches!(p.side, Side::Below | Side::Above);
    // along the side: the side itself, else the opposite one when it fits and this does not
    let at = |side: Side| match side {
        Side::Below => anchor.y + anchor.h + p.gap,
        Side::Above => anchor.y - p.gap - h,
        Side::Right => anchor.x + anchor.w + p.gap,
        Side::Left => anchor.x - p.gap - w,
    };
    let fits = |side: Side, v: f32| match side {
        Side::Below => v + h <= bounds.y + bounds.h - m,
        Side::Above => v >= bounds.y + m,
        Side::Right => v + w <= bounds.x + bounds.w - m,
        Side::Left => v >= bounds.x + m,
    };
    let opposite = match p.side {
        Side::Below => Side::Above,
        Side::Above => Side::Below,
        Side::Right => Side::Left,
        Side::Left => Side::Right,
    };
    let mut main = at(p.side);
    if p.flip && !fits(p.side, main) {
        main = at(opposite);
    }
    let (lo, len, extent) = if vertical { (anchor.x, anchor.w, w) } else { (anchor.y, anchor.h, h) };
    let cross = match p.align {
        Align::Center => lo + (len - extent) / 2.0,
        Align::End => lo + len - extent,
        _ => lo,
    };
    let clamp = |v: f32, start: f32, room: f32, extent: f32| v.min(start + room - m - extent).max(start + m);
    let (x, y) = if vertical { (cross, main) } else { (main, cross) };
    Rect::new(clamp(x, bounds.x, bounds.w, w), clamp(y, bounds.y, bounds.h, h), w, h)
}
