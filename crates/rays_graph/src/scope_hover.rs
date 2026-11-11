//! The graph pane between content builds. Nothing on the content takes input: its cards and ports
//! are `BLOCKING | HOVER_FREE` boxes (the canvas captures a press on them) and its wires are only
//! geometry, so the pointer moving, a press, a selection and a drag of cards build no content. What
//! the pointer is over and what a press is on come from the boxes the last content build left
//! (`Hovers`) and the geometry. A card's ground (fill and edge: idle, hovered, selected), its hovered
//! row's band, its selection brackets and its recession behind a selected chain are nodes whose looks
//! change through the patch path; a dragged card is its own view moved by its offset. The lit wires
//! and tips are a view over the content keyed by what they show, and a drag's wires (under the
//! cards) and its band or rubber band (over them) are views of their own.
use crate::scope_frame::{overscan, row_at, sx, sy, Del};
use crate::scope_paint as sp;
use crate::scope_state::*;
use crate::scope_wires::*;
use flow_check::workspace::Path;
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level};
use flow_syntax::ty::Ty;
use pxui::draw::flags::{BOTTOM, LEFT, RIGHT, TOP};
use pxui::geom::{color::T as Color, p32};
use pxui::theme::{self, Look, State};
use pxui::ui::*;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub enum PortKind {
    /// what it carries and its type
    Out(String, Option<Ty>),
    /// what a click on it takes off
    In(Option<Del>),
}

pub struct Port {
    pub b: UiBox,
    pub kind: PortKind,
}

/// A card's ground node with its looks (idle, hovered, selected, selected and hovered) and its row
/// highlight: the node, the line it was built at and the row height it was built with.
#[derive(Clone, Copy)]
pub struct Ground {
    pub node: UiBox,
    pub looks: [Look; 4],
    pub row: Option<(UiBox, usize, f32)>,
}

/// What a card shows that the content does not build: patched since.
#[derive(Clone, Copy, Default, PartialEq)]
struct Shown {
    selected: bool,
    hovered: bool,
    line: Option<usize>,
    recede: bool,
}

/// A card the last build left: its item, its view and tile, the nodes its hover and selection patch.
pub struct Card {
    pub slot: usize,
    pub path: Path,
    pub view: UiBox,
    pub tile: UiBox,
    pub ground: Option<Ground>,
    /// the corners of its selection brackets (top left, top right, bottom left, bottom right)
    pub brackets: [UiBox; 4],
    /// the fill that recedes it behind a selected chain (a node or a folded zone)
    pub recede: Option<UiBox>,
    pub ports: Vec<Port>,
    shown: Shown,
    /// what it shows of its records (`sp::readout`): a node per piece and what the node shows, the
    /// footer they show, the one a cook brought (`cooked` patches the pieces) and the zoom and text
    /// size it was built at
    pub readout: Readout,
}

/// A card's record pieces as `Card::readout` keeps them.
#[derive(Default)]
pub struct Readout {
    pub nodes: Vec<(UiBox, Stamp)>,
    pub footer: Option<flow_graph::probe::Footer>,
    pub cooked: Option<flow_graph::probe::Footer>,
    pub at: (f64, i64),
}

/// What a piece's node shows: its rectangle, colour, text (a hash) and a line's direction.
#[derive(Clone, Copy, PartialEq)]
pub struct Stamp {
    rect: [f32; 4],
    color: Color,
    text: u64,
    /// text, fill, a falling line, a rising one
    kind: u8,
}

/// A piece's node: its rectangle, look, mark and text.
fn piece_node(piece: sp::Piece, text: &str) -> (Stamp, Look, Mark) {
    let hash = |s: &str| {
        let mut h = std::hash::DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    };
    let r = |x: f64, y: f64, w: f64, h: f64| [x as f32, y as f32, w as f32, h as f32];
    match piece {
        sp::Piece::Text { x, y, w, h, size, color } => {
            (Stamp { rect: r(x, y, w, h), color, text: hash(text), kind: 0 }, Look { text: color, font: Font { size, ..Font::BODY }, ..Look::NONE }, Mark::None)
        }
        // the line's ends half its width inside the node's rectangle (`Mark::Line`)
        sp::Piece::Line { a, b, color } => {
            let rising = (b.0 - a.0) * (b.1 - a.1) < 0.0;
            let stamp = Stamp { rect: r(a.0.min(b.0) - 0.5, a.1.min(b.1) - 0.5, (b.0 - a.0).abs() + 1.0, (b.1 - a.1).abs() + 1.0), color, text: 0, kind: 2 + rising as u8 };
            (stamp, Look { text: color, ..Look::NONE }, Mark::Line { width: 1.0, rising })
        }
        sp::Piece::Fill { x, y, w, h, color } => (Stamp { rect: r(x, y, w, h), color, text: 0, kind: 1 }, Look { fill: color, ..Look::NONE }, Mark::None),
    }
}

/// A card's record pieces as nodes, in its tile.
pub fn readout_nodes(t: &Pane, ui: &mut Ui, z: f64, fs: i64, slot: usize, footer: Option<&flow_graph::probe::Footer>) -> Readout {
    let mut nodes = Vec::new();
    if let (Some(f), Some((ghost, full, pad, ty))) = (footer, readout_of(t, slot)) {
        let (pl, _, _) = &t.geo.items[slot];
        sp::readout(ui, t, z, fs, ghost, full, f, &ty, pad, (0.0, 0.0, pl.w * z, pl.h * z), &mut |ui, piece, text| {
            let (stamp, look, mark) = piece_node(piece, text);
            let [x, y, w, h] = stamp.rect;
            let b = ui.node("r", Node { w: Size::Px(w), h: Size::Px(h), at: Some((x, y)), look, mark, ..Node::DEFAULT });
            ui.text(&b, text);
            nodes.push((b, stamp));
        });
    }
    Readout { nodes, footer: footer.cloned(), cooked: None, at: (z, fs) }
}

/// What `sp::readout` shows of the item at `slot`: the ghost value, the full footer, the footer's
/// padding (an open expansion panel under it) and the item's type; `None` for what shows nothing.
fn readout_of(t: &Pane, slot: usize) -> Option<(bool, bool, f64, Ty)> {
    let (pl, ax, ay) = &t.geo.items[slot];
    match &pl.item {
        Item::Item(n) if pl.shown != Level::Point && (n.zone.is_none() || pl.collapsed) => {
            let out_wired = t.geo.read.contains(&crate::scope_wires::fkey(out_anchor(pl, *ax, *ay)));
            let lh = match (&n.lens, lens_of(t, &n.path)) {
                (Some(l), Some(step)) => p::lens_height(l, step),
                _ => 0.0,
            };
            Some((sp::ghost_shown(t, n, pl.shown, out_wired), pl.shown == Level::Full, p::CARD_PAD + lh, n.ty.clone()))
        }
        _ => None,
    }
}

/// Whether the item at `slot` lies in the pane and its overscan where the pan and zoom are now (a
/// cook formats no footer for a card scrolled away: the content builds again before it shows).
fn in_view(t: &Pane, slot: usize) -> bool {
    let (pl, ax, ay) = &t.geo.items[slot];
    let (ox, oy) = overscan(t);
    let (x, y) = (sx(t, *ax) - t.x as f64, sy(t, *ay) - t.y as f64);
    x + pl.w * t.zoom > -ox && y + pl.h * t.zoom > -oy && x < t.width as f64 + ox && y < t.height as f64 + oy
}

/// What a cook cannot patch, for the cards and open zones in view: the tiers their marks show
/// and the arms the branches took (`with_records` builds the content again when it changes).
pub fn fixed(t: &Pane) -> Vec<Option<String>> {
    let hv = t.hovers.borrow();
    let mut out = Vec::new();
    for c in hv.cards.iter().filter(|c| in_view(t, c.slot)) {
        if let Item::Item(n) = &t.geo.items[c.slot].0.item {
            let tier = sp::execution_of(t, n).or_else(|| c.readout.footer.as_ref().and_then(|_| t.records.as_ref().and_then(|r| crate::scope_frame::footer_of(t, r, n).execution)));
            out.push(tier.map(|e| e.tier));
        }
    }
    for &z in &hv.zones {
        if let Item::Item(n) = &t.geo.items[z].0.item {
            if let Some(zn) = &n.zone {
                out.push(sp::zone_taken(t, n, zn).then(String::new));
            }
        }
    }
    out
}

/// A cook's footers for the cards that show one, left for `cooked` where they changed. False when a
/// card that shows one lies out of view (the content builds again, formatting only what shows).
pub fn cook(t: &Pane) -> bool {
    let Some(records) = &t.records else { return false };
    let mut hv = t.hovers.borrow_mut();
    if hv.cards.iter().any(|c| c.readout.footer.is_some() && !in_view(t, c.slot)) {
        return false;
    }
    for c in hv.cards.iter_mut() {
        if let (Item::Item(n), Some(shown)) = (&t.geo.items[c.slot].0.item, &c.readout.footer) {
            let f = crate::scope_frame::footer_of(t, records, n);
            c.readout.cooked = (f != *shown).then_some(f);
        }
    }
    true
}

/// The footers a cook left (`cook`) on the cards' nodes: each piece that changed patched. False
/// when a card's pieces no longer match its nodes (a tag or a sparkline came or went): nothing of
/// that card is patched and the content builds again.
pub fn cooked(t: &Pane, ui: &mut Ui) -> bool {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let hv = &mut *hv;
    let mut fits = true;
    for c in hv.cards.iter_mut() {
        let Some(f) = c.readout.cooked.take() else { continue };
        let Some((ghost, full, pad, ty)) = readout_of(t, c.slot) else { continue };
        let (z, fs) = c.readout.at;
        let (pl, _, _) = &t.geo.items[c.slot];
        let rect = (0.0, 0.0, pl.w * z, pl.h * z);
        // the same pieces in the same order
        let mut k = 0;
        let mut same = true;
        sp::readout(ui, t, z, fs, ghost, full, &f, &ty, pad, rect, &mut |_, piece, text| {
            same &= c.readout.nodes.get(k).is_some_and(|(_, s)| s.kind.min(2) == piece_node(piece, text).0.kind.min(2));
            k += 1;
        });
        if !same || k != c.readout.nodes.len() {
            fits = false;
            continue;
        }
        let mut k = 0;
        let patches = &mut hv.patches;
        sp::readout(ui, t, z, fs, ghost, full, &f, &ty, pad, rect, &mut |ui, piece, text| {
            let (stamp, look, mark) = piece_node(piece, text);
            let (b, was) = &mut c.readout.nodes[k];
            k += 1;
            if stamp == *was {
                return;
            }
            let mut ok = true;
            if stamp.text != was.text {
                ok &= ui.patch_text(b, text);
                *patches += 1;
            }
            if stamp.color != was.color {
                ok &= ui.patch_look(b, &look);
                *patches += 1;
            }
            if stamp.kind != was.kind {
                ok &= ui.patch_mark(b, mark);
                *patches += 1;
            }
            if stamp.rect != was.rect {
                // where the view lays the node out: its tile's corner plus the piece's place
                let [x, y, w, h] = stamp.rect;
                let r = ui.local_rect(b);
                let [x0, y0, _, _] = was.rect;
                ok &= ui.patch_rect(b, pxui::geom::Rect::new(r.x - x0 + x, r.y - y0 + y, w, h));
                *patches += 1;
            }
            fits &= ok;
            *was = stamp;
        });
        c.readout.footer = Some(f);
    }
    fits
}

/// The sides of each bracket corner.
pub const CORNERS: [u8; 4] = [TOP | LEFT, TOP | RIGHT, BOTTOM | LEFT, BOTTOM | RIGHT];

/// A bracket corner's look: its two arms, `width` thick, in `color` (clear when not selected).
pub fn corner(sides: u8, width: f32, color: Color) -> Look {
    Look { border: color, border_w: width, sides, ..Look::NONE }
}

/// The recession of a card behind the selected chain: the ground at 170 over it.
pub fn recede_look(t: &Pane, on: bool) -> Look {
    Look { fill: if on { pxui::geom::color::with_alpha(t.theme.panel, 170) } else { Look::NONE.fill }, ..Look::NONE }
}

#[derive(Default)]
pub struct Hovers {
    pub cards: Vec<Card>,
    /// the cards of the build before, while one builds
    spare: Vec<Card>,
    /// the tiles' and ports' keys (what `Ui::root_at` answers among)
    pub keys: Vec<i64>,
    /// the wires and the open zones in view, in the build's order, and the frames (their scope's
    /// place in the geometry's origins and their index there)
    pub reach: Vec<usize>,
    pub zones: Vec<usize>,
    pub frames: Vec<(usize, usize)>,
    /// the views of the open zones' bodies (by slot) and of the frames (as `frames`) the wires view
    /// built or kept last
    pub zone_views: Vec<(usize, UiBox)>,
    /// the cards in view, and the key the lists were made for
    pub visible: Vec<usize>,
    listed: Option<u64>,
    pub frame_views: Vec<UiBox>,
    /// the content builds so far (not counting verifying runs)
    pub builds: u64,
    /// the patches cooks made to the cards' record pieces (`cooked`)
    pub patches: u64,
    /// the geometry the build read (its items and wires), by address
    pub geo: (usize, usize),
    /// the selection and hover the cards show (`show`)
    sel: u64,
    hover: (i64, Option<usize>),
    /// the wires lit for the hover, and lists to compare it with
    lit: Vec<usize>,
    next: Vec<usize>,
    /// the lit wires the hover view drew
    drawn: Vec<usize>,
    /// the wires near the pointer
    near: Vec<usize>,
    /// a move of cards by their views: the cards (slots, ascending; a zone's with the cards it holds),
    /// the frames it carries (as `frames`), the wires that end on its cards (ascending), the wires the
    /// drag's view draws (those and the wires whose route crosses where the cards were or are, kept
    /// up to date by `carry_wires`), the offset the views show and the views showing it
    pub moving: Vec<usize>,
    moving_frames: Vec<usize>,
    pub attached: Vec<usize>,
    pub skip: Vec<usize>,
    shown_offset: Option<(f64, f64)>,
    offset: Vec<UiBox>,
    /// the frames' title strips (their keys, as `frames`): a press there carries the frame
    pub frame_titles: Vec<(i64, usize)>,
    /// the name a long wire shows at its use site, by wire (empty for the others): made with the
    /// content's build, read by the paintings of the wires
    tags: Vec<String>,
    /// the drag's wires where it puts them: a run of `pts` per wire
    route: Scratch,
    runs: Vec<(usize, usize, usize)>,
    pts: Vec<(f64, f64)>,
    buf: Vec<(f64, f64)>,
}

impl Hovers {
    /// A content build starts: the cards it keeps come from the build before.
    pub fn begin(&mut self, t: &Pane) {
        self.builds += 1;
        std::mem::swap(&mut self.cards, &mut self.spare);
        self.cards.clear();
        self.keys.clear();
        self.frame_titles.clear();
        self.geo = geo_key(t);
        self.sel = u64::MAX;
        self.hover = (0, None);
    }

    /// A card built again: it shows no hover, selection or recession until `show` patches it.
    pub fn card(&mut self, slot: usize, path: Path, view: UiBox, tile: UiBox, ground: Option<Ground>, brackets: [UiBox; 4], recede: Option<UiBox>, ports: Vec<Port>, readout: Readout) {
        self.cards.push(Card { slot, path, view, tile, ground, brackets, recede, ports, shown: Shown::default(), readout });
    }

    /// A card whose view was kept: as the build before left it.
    pub fn keep(&mut self, view: UiBox) {
        if let Some(k) = self.spare.iter().position(|c| c.view.box_key == view.box_key) {
            let c = self.spare.swap_remove(k);
            self.cards.push(c);
        }
    }

    /// A verifying run built a kept card again: it shows no hover or selection until `show` patches it
    /// again (as when it was built).
    pub fn redeclared(&mut self, view: UiBox) {
        if let Some(c) = self.cards.iter_mut().find(|c| c.view.box_key == view.box_key) {
            c.shown = Shown::default();
            (self.sel, self.hover) = (u64::MAX, (0, None));
        }
    }

    /// The build is over: the cards it left are what presses and the hover find.
    pub fn end(&mut self, t: &Pane, ui: &mut Ui) {
        self.tags.clear();
        self.tags.extend(t.geo.wires.iter().map(|wi| if wi.target.is_some() && (wi.b.0 - wi.a.0).abs() > TAG_SPAN { sp::tag_name(t, wi) } else { String::new() }));
        // room for every wire near the pointer and lit by it, so the hover never grows the lists
        for v in [&mut self.near, &mut self.lit, &mut self.next] {
            v.reserve(t.geo.wires.len());
        }
        // a card no longer built shows its place again
        for c in self.spare.drain(..) {
            ui.view_offset(&c.view, 0.0, 0.0);
        }
        self.offset.retain(|v| self.cards.iter().any(|c| c.view.box_key == v.box_key));
        for c in &self.cards {
            self.keys.push(c.tile.box_key);
            self.keys.extend(c.ports.iter().map(|q| q.b.box_key));
        }
    }
}

fn geo_key(t: &Pane) -> (usize, usize) {
    (Rc::as_ptr(&t.geo.items) as *const u8 as usize, Rc::as_ptr(&t.geo.wires) as *const u8 as usize)
}

/// The ground of a card painted as its node's look: its fill and a one-point edge inside it
/// (`paint_card`'s two instances), dashed for an expression card; hovered, the hover fill unless
/// selected (a graph input and the return card do not show the hover). A refused node's edge is the
/// invalid colour.
pub fn ground(t: &Pane, failed: bool, dashed: bool, hovers: bool) -> [Look; 4] {
    let theme = t.theme;
    let look = |hovered: bool, selected: bool| {
        let fill = if hovered && hovers && !selected { theme::hover_fill(&theme) } else { theme.input };
        Look { fill, border: if failed { theme::invalid() } else { sp::card_edge(t, fill, selected) }, border_w: 1.0, dashed, ..Look::NONE }
    };
    [look(false, false), look(true, false), look(false, true), look(true, true)]
}

/// What the pointer is over: the card shown hovered and its row's line, an expression row whose full
/// text shows, a live card's `t` and the displayed node's flag rested on.
#[derive(Clone, Copy, Default, PartialEq)]
pub struct Now {
    pub card: i64,
    pub line: Option<usize>,
    pub tip: Option<(usize, usize)>,
    pub live: Option<usize>,
    pub flag: bool,
}

/// What a press is on: a card (its slot), an out-port (its card's slot, what it carries, its type and
/// centre), an in-port (its card's slot, what a click takes off, its rectangle), a wire, the ground.
#[derive(Clone, Debug, PartialEq)]
pub enum On {
    Card(usize),
    Out(usize, String, Option<Ty>, (f64, f64)),
    In(usize, Option<Del>, (f64, f64, f64, f64)),
    Wire(usize),
    /// a frame's title strip (as `Hovers::frames`)
    Frame(usize),
    Ground,
}

/// Whether a move of cards by their views (a frame's too) is in progress.
pub fn moving_fast(t: &Pane) -> bool {
    matches!(t.drag, Some(Drag::Moving { moved: true, .. } | Drag::Carrying { .. }))
}

/// The offset a move puts its cards at: while it drags, and after it let go until the geometry has
/// them there.
pub fn drag_offset(t: &Pane) -> Option<(f64, f64)> {
    match (&t.drag, &t.dropped) {
        (Some(Drag::Moving { dx, dy, moved: true } | Drag::Carrying { dx, dy, .. }), _) => Some((*dx, *dy)),
        (_, Some((_, dx, dy, _))) => Some((*dx, *dy)),
        _ => None,
    }
}

/// The box of wire `i` the pointer is in, as the content's hit boxes were: its segments cut to `clip`
/// (window points, grown by 4) in 16-point steps, each a rectangle grown by 4. A wire drawn as a tag
/// has none.
fn wire_hit(t: &Pane, chain: &Option<Rc<Chains>>, i: usize, pt: (f64, f64), clip: (f64, f64, f64, f64)) -> bool {
    let wi = &t.geo.wires[i];
    if wi.target.is_none() || tagged_in(t.tags, chain, wi) {
        return false;
    }
    let (cx, cy, cw, ch) = clip;
    for seg in wi.pts.windows(2) {
        let a = (sx(t, seg[0].0), sy(t, seg[0].1));
        let b = (sx(t, seg[1].0), sy(t, seg[1].1));
        if let Some(((ax, ay), (bx, by))) = clip_segment(a, b, (cx - 4.0, cy - 4.0, cw + 8.0, ch + 8.0)) {
            let steps = if ax == bx || ay == by { 1 } else { 1.max(((bx - ax).hypot(by - ay) / 16.0) as i64) };
            for st in 0..steps {
                let u0 = st as f64 / steps as f64;
                let u1 = (st + 1) as f64 / steps as f64;
                let (x0, y0) = (ax + ((bx - ax) * u0), ay + ((by - ay) * u0));
                let (x1, y1) = (ax + ((bx - ax) * u1), ay + ((by - ay) * u1));
                let r = Rect::new((x0.min(x1) - 4.0) as f32, (y0.min(y1) - 4.0) as f32, ((x1 - x0).abs() + 8.0) as f32, ((y1 - y0).abs() + 8.0) as f32);
                if contains(r, p32(pt)) {
                    return true;
                }
            }
        }
    }
    false
}

/// The topmost wire at `pt`: the last of `order` hit (the one built last).
fn wire_at(t: &Pane, chain: &Option<Rc<Chains>>, order: &[usize], pt: (f64, f64), clip: (f64, f64, f64, f64)) -> Option<usize> {
    order.iter().rev().copied().find(|&i| wire_hit(t, chain, i, pt, clip))
}

/// The topmost wire at `pt` among the wires in view (`reach`, ascending): the last hit, of the wires
/// whose bounds meet the point (`near`, a kept list).
fn wire_on_canvas(t: &Pane, chain: &Option<Rc<Chains>>, reach: &[usize], near: &mut Vec<usize>, pt: (f64, f64), clip: (f64, f64, f64, f64)) -> Option<usize> {
    let z = t.zoom;
    let (gx, gy) = ((pt.0 - sx(t, 0.0)) / z, (pt.1 - sy(t, 0.0)) / z);
    let m = 8.0 / z;
    index_query_into(&t.geo.reach, (gx - m, gy - m, gx + m, gy + m), near);
    near.iter().rev().copied().find(|&i| reach.binary_search(&i).is_ok() && wire_hit(t, chain, i, pt, clip))
}

/// What the content shows past each side of the pane, in window points.
fn viewport(t: &Pane) -> (f64, f64, f64, f64) {
    let (ox, oy) = overscan(t);
    (t.x as f64 - ox, t.y as f64 - oy, t.width as f64 + 2.0 * ox, t.height as f64 + 2.0 * oy)
}

/// An open zone's body as a wire's hit boxes inside it are cut to: its card's rectangle in view.
fn zone_clip(t: &Pane, slot: usize) -> Option<(f64, f64, f64, f64)> {
    let (pl, ax, ay) = &t.geo.items[slot];
    match &pl.item {
        Item::Item(n) if n.zone.is_some() && !pl.collapsed => {
            let (vx, vy, vw, vh) = viewport(t);
            let (x, y) = (sx(t, *ax), sy(t, *ay));
            let (x0, y0) = (x.max(vx), y.max(vy));
            Some((x0, y0, (vx + vw).min(x + pl.w * t.zoom) - x0, (vy + vh).min(y + pl.h * t.zoom) - y0))
        }
        _ => None,
    }
}

/// The slot of a card the last build left, in the geometry now.
fn slot_now(t: &Pane, hv: &Hovers, c: &Card) -> Option<usize> {
    if hv.geo == geo_key(t) { Some(c.slot) } else { t.geo.slot.get(&c.path).copied() }
}

/// What a press at `pt` is on: the innermost port or card under it in the last frame's hit list, a
/// wire (over the bare canvas, or inside an open zone's body over its card), else the ground.
pub fn classify(t: &Pane, ui: &mut Ui, pt: (f64, f64)) -> On {
    let hovers = t.hovers.clone();
    let hv = hovers.borrow();
    let chain = chains_of(t);
    let fresh = hv.geo == geo_key(t);
    match ui.root_at(p32(pt), &hv.keys) {
        Some(k) if hv.frame_titles.iter().any(|(key, _)| *key == k) => match hv.frame_titles.iter().find(|(key, _)| *key == k) {
            Some(&(_, f)) if fresh => On::Frame(f),
            _ => On::Ground,
        },
        Some(k) => {
            if let Some((c, port)) = hv.cards.iter().find_map(|c| c.ports.iter().find(|q| q.b.box_key == k).map(|q| (c, q))) {
                let Some(slot) = slot_now(t, &hv, c) else { return On::Ground };
                if !fresh {
                    return On::Card(slot);
                }
                let r = ui.rect(&port.b);
                let (x, y, w, h) = (f64::from(r.x), f64::from(r.y), f64::from(r.w), f64::from(r.h));
                return match &port.kind {
                    PortKind::Out(src, ty) => On::Out(slot, src.clone(), ty.clone(), (x + w / 2.0, y + h / 2.0)),
                    PortKind::In(del) => On::In(slot, del.clone(), (x, y, w, h)),
                };
            }
            let Some(c) = hv.cards.iter().find(|c| c.tile.box_key == k) else { return On::Ground };
            let Some(slot) = slot_now(t, &hv, c) else { return On::Ground };
            if let Some(clip) = zone_clip(t, slot) {
                // the wires that end inside a zone's body lie over its card
                let empty = Vec::new();
                let inside = t.geo.inside.get(&t.geo.items[slot].0.path).unwrap_or(&empty);
                if let Some(i) = wire_at(t, &chain, inside, pt, clip) {
                    return On::Wire(i);
                }
            }
            On::Card(slot)
        }
        None => {
            let mut near = Vec::new();
            match wire_on_canvas(t, &chain, &hv.reach, &mut near, pt, viewport(t)) {
                Some(i) if fresh => On::Wire(i),
                _ => On::Ground,
            }
        }
    }
}

/// Before the build: what the pointer is over, from the boxes the last build left; the pane's
/// hovered row and lit wires follow it (rebuilt only when they change).
pub fn find(t: &mut Pane, ui: &mut Ui, canvas: &UiBox, mouse: (f64, f64)) -> Now {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let mut now = Now::default();
    let z = t.zoom;
    if hv.geo != geo_key(t) || moving_fast(t) || t.dropped.is_some() {
        // the geometry moved since the build (it builds this frame), or cards are carried: nothing
        // is hovered until it settles
        t.hovered_at = None;
        t.highlighted.clear();
        hv.lit.clear();
        return now;
    }
    let free = t.drag.is_none() && t.carry_hot.is_none();
    let mut row: Option<(usize, usize)> = None;
    let mut zone: Option<usize> = None;
    for c in &hv.cards {
        if !ui.hovered_within(&c.tile) {
            continue;
        }
        let (pl, ax, ay) = &t.geo.items[c.slot];
        if let Item::Item(n) = &pl.item {
            if n.zone.is_none() {
                let at = ((mouse.0 - sx(t, *ax)) / z, (mouse.1 - sy(t, *ay)) / z);
                if let Some(i) = row_at(14.0, pl, n, pl.shown, at.0, at.1).filter(|i| *i < n.rows.len()) {
                    row = Some((c.slot, i));
                }
            } else if ui.hovered_exactly(&c.tile) {
                zone = Some(c.slot);
            }
        }
        if free && c.ground.is_some() {
            now.card = c.tile.box_key;
            now.line = row.and_then(|(_, i)| pl.lines.iter().position(|l| matches!(l, p::Line::Row(j, _) if *j == i)));
        }
        if row.is_some() {
            break;
        }
    }
    // the hovered row (keys act on it) and an expression's full text
    t.hovered_at = row;
    if let Some((slot, i)) = row {
        if let Item::Item(n) = &t.geo.items[slot].0.item {
            if matches!(n.rows[i].chip, p::Chip::Inline { .. }) {
                now.tip = Some((slot, i));
            }
        }
    }
    // the wire under the pointer (over the bare canvas, or in an open zone's body), and those at the
    // port under it
    let port = hv.cards.iter().flat_map(|c| c.ports.iter()).find(|q| ui.hovered_within(&q.b)).map(|q| {
        let r = ui.rect(&q.b);
        (f64::from(r.x + r.w / 2.0), f64::from(r.y + r.h / 2.0))
    });
    let chain = chains_of(t);
    let hot = if ui.hovered_exactly(canvas) {
        let hv = &mut *hv;
        wire_on_canvas(t, &chain, &hv.reach, &mut hv.near, mouse, viewport(t))
    } else {
        zone.and_then(|slot| {
            let clip = zone_clip(t, slot)?;
            wire_at(t, &chain, t.geo.inside.get(&t.geo.items[slot].0.path)?, mouse, clip)
        })
    };
    let same_port = |pt: (f64, f64)| port.is_some_and(|(x, y)| (sx(t, pt.0) - x).abs() <= (1.5 * z) + 0.5 && (sy(t, pt.1) - y).abs() <= (1.5 * z) + 0.5);
    let mut next = std::mem::take(&mut hv.next);
    next.clear();
    if t.context.is_none() {
        for &i in &hv.reach {
            let wi = &t.geo.wires[i];
            if hot == Some(i) || same_port(wi.a) || same_port(wi.b) {
                next.push(i);
            }
        }
    }
    if !t.highlighted.iter().eq(next.iter().rev()) {
        t.highlighted.clear();
        t.highlighted.reserve(t.geo.wires.len());
        t.highlighted.extend(next.iter().rev());
    }
    std::mem::swap(&mut next, &mut hv.lit);
    hv.next = next;
    // the red `t` of a live card's header: a tip after the pointer rests on it, saying what it is
    for c in &hv.cards {
        let (pl, ax, ay) = &t.geo.items[c.slot];
        if let Item::Item(n) = &pl.item {
            if n.live && pl.shown != Level::Point && ui.hovered_within(&c.tile) {
                let right = sx(t, *ax) + ((pl.w - sp::HEAD_PAD) * z);
                let top = sy(t, *ay);
                let (mx, my) = mouse;
                if mx >= right - (14.0 * z) && mx <= right + (2.0 * z) && my >= top && my < top + (p::HEAD_HEIGHT * z) && ui.hover_delay("scope-live") {
                    now.live = Some(c.slot);
                    break;
                }
            }
        }
    }
    // the displayed node's flag: its tip once the pointer has rested on it
    if let Some(path) = &t.display {
        if let Some((pl, ax, ay)) = t.item_at(path) {
            if matches!(pl.item, Item::Item(_)) && pl.shown != Level::Point {
                let fx = sx(t, *ax) + ((pl.w - sp::HEAD_PAD - 12.0) * z);
                let fy = sy(t, *ay) + (6.0 * z);
                let over = contains(Rect::new(fx as f32, fy as f32, (12.0 * z) as f32, (12.0 * z) as f32), p32(mouse));
                now.flag = over && ui.hover_delay("scope-flag");
            }
        }
    }
    now
}

/// The selection as the cards show it: what it holds, over which geometry.
fn selection_key(t: &Pane) -> u64 {
    let mut h = std::hash::DefaultHasher::new();
    (geo_key(t), t.selected.as_slice()).hash(&mut h);
    h.finish()
}

/// After the build: the hovered card's ground and row highlight, and every card's selection
/// brackets and recession behind the selected chain, through the patch path (a card the build
/// declared again shows none of them until patched).
pub fn show(t: &Pane, ui: &mut Ui, now: &Now) {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let hover = (now.card, now.line);
    let sel = selection_key(t);
    if hv.hover == hover && hv.sel == sel {
        return;
    }
    let sel_changed = hv.sel != sel;
    let chain = if sel_changed { chains_of(t) } else { None };
    let curve = ui.tokens().transition(State::default(), State { hovered: true, ..State::default() });
    let set = |ui: &mut Ui, b: &UiBox, look: Look| {
        if curve.instant() {
            ui.patch_look(b, &look);
        } else {
            ui.animate(b, pxui::ui::Target::Look(look), curve);
        }
    };
    let accent = t.theme.accent;
    for c in hv.cards.iter_mut() {
        let hovered = c.ground.is_some() && c.tile.box_key == hover.0;
        let (selected, recede) = if sel_changed {
            let (pl, _, _) = &t.geo.items[c.slot];
            let recede = match &chain {
                Some(ch) => c.recede.is_some() && !node_lit(ch, &pl.path),
                None => false,
            };
            (t.selected.contains(&pl.path), recede)
        } else {
            (c.shown.selected, c.shown.recede)
        };
        let next = Shown { selected, hovered, line: if hovered { hover.1 } else { None }, recede };
        let was = c.shown;
        if next == was {
            continue;
        }
        if let Some(g) = &c.ground {
            let look = g.looks[2 * selected as usize + hovered as usize];
            if (was.selected, was.hovered) != (selected, hovered) {
                set(ui, &g.node, look);
            }
            if let Some((hl, k0, rh)) = g.row {
                if (was.selected, was.hovered, was.line) != (selected, hovered, next.line) {
                    let fill = if next.line.is_some() { theme::faint_border(&t.theme) } else { look.fill };
                    set(ui, &hl, Look { fill, ..Look::NONE });
                    ui.animate(&hl, pxui::ui::Target::Offset(0.0, next.line.map_or(0.0, |k| (k as f32 - k0 as f32) * rh)), curve);
                }
            }
        }
        if was.selected != selected {
            for (b, sides) in c.brackets.iter().zip(CORNERS) {
                ui.patch_look(b, &corner(sides, 2.0, if selected { accent } else { Look::NONE.fill }));
            }
            ui.set_role(&c.tile, pxui::a11y::Role::Group, if selected { pxui::a11y::state::SELECTED } else { 0 });
        }
        if was.recede != recede {
            if let Some(b) = &c.recede {
                ui.patch_look(b, &recede_look(t, recede));
            }
        }
        c.shown = next;
    }
    (hv.hover, hv.sel) = (hover, sel);
}

/// The cards a move carries show where it puts them: their views offset by the move (at the zoom the
/// content was built at), with the bodies of the zones and the frames it carries; the others where
/// they were.
pub fn place_cards(t: &Pane, ui: &mut Ui, built_zoom: f64) {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let want = drag_offset(t);
    if want == hv.shown_offset {
        return;
    }
    let hv = &mut *hv;
    for v in &hv.offset {
        ui.view_offset(v, 0.0, 0.0);
    }
    hv.offset.clear();
    if let Some((dx, dy)) = want {
        let (x, y) = ((dx * built_zoom) as f32, (dy * built_zoom) as f32);
        let moves = |slot: usize| hv.moving.binary_search(&slot).is_ok();
        for c in &hv.cards {
            if moves(c.slot) {
                ui.view_offset(&c.view, x, y);
                hv.offset.push(c.view);
            }
        }
        for &(slot, v) in &hv.zone_views {
            if moves(slot) {
                ui.view_offset(&v, x, y);
                hv.offset.push(v);
            }
        }
        for &k in &hv.moving_frames {
            if let Some(v) = hv.frame_views.get(k) {
                ui.view_offset(v, x, y);
                hv.offset.push(*v);
            }
        }
    }
    hv.shown_offset = want;
}

/// A move of cards by their views begins: the cards it carries (the pane's `moving`, and every card
/// inside a zone among them), the frames that go with them (in a zone it carries, or the frame a
/// title drag carries) and the wires that end on them, which the wires view leaves to the drag's own.
pub fn begin_move(t: &Pane) {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let hv = &mut *hv;
    let paths = &t.moving;
    let carried = |q: &Path| paths.iter().any(|a| q.starts_with(a));
    hv.moving.clear();
    hv.moving.extend(t.geo.items.iter().enumerate().filter(|(_, (pl, _, _))| carried(&pl.path)).map(|(k, _)| k));
    hv.moving_frames.clear();
    let frame = match &t.drag {
        Some(Drag::Carrying { scope, index, .. }) => Some((scope, *index as usize)),
        _ => None,
    };
    for (k, &(origin, index)) in hv.frames.iter().enumerate() {
        let scope = &t.geo.origins[origin].0;
        if carried(scope) || frame == Some((scope, index)) {
            hv.moving_frames.push(k);
        }
    }
    let moving = &hv.moving;
    let on = |q: &Option<Path>| q.as_ref().and_then(|q| t.geo.slot.get(q)).is_some_and(|k| moving.binary_search(k).is_ok());
    hv.attached.clear();
    hv.attached.extend(t.geo.wires.iter().enumerate().filter(|(_, wi)| on(&wi.from) || on(&wi.into)).map(|(i, _)| i));
    hv.skip.clear();
    hv.skip.extend_from_slice(&hv.attached);
    // room for every wire to join the drag, routed as long as it is now and twice as long again
    let (wires, points) = (t.geo.wires.len(), t.geo.wires.iter().map(|wi| wi.pts.len()).sum::<usize>());
    hv.skip.reserve(wires);
    hv.runs.reserve(wires);
    hv.pts.reserve(3 * points);
    let longest = t.geo.wires.iter().map(|wi| wi.pts.len()).max().unwrap_or(0);
    hv.buf.reserve(3 * longest + 8);
    hv.route.reserve(t.geo.items.len(), 3 * longest + 8);
}

/// The move is over (and the geometry has the cards where it put them).
pub fn end_move(t: &Pane) {
    let mut hv = t.hovers.borrow_mut();
    hv.moving.clear();
    hv.moving_frames.clear();
    hv.attached.clear();
    hv.skip.clear();
}

/// The wires a move draws in its own view, where it puts them: those that end on its cards and those
/// whose route meets what its cards sweep (where they were and where they are now, in graph units),
/// so every wire that would bend another way around them follows the move (a test of each route's
/// segment boxes, nothing allocated once warm).
pub fn carry_wires(t: &Pane) {
    let Some((dx, dy)) = drag_offset(t) else { return };
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let hv = &mut *hv;
    hv.skip.clear();
    hv.skip.extend_from_slice(&hv.attached);
    let swept = |k: usize| {
        let (pl, x, y) = &t.geo.items[k];
        (x.min(x + dx), y.min(y + dy), x.max(x + dx) + pl.w, y.max(y + dy) + pl.h)
    };
    for &i in &hv.reach {
        if hv.attached.binary_search(&i).is_ok() {
            continue;
        }
        let pts = &t.geo.wires[i].pts;
        let crosses = hv.moving.iter().any(|&k| {
            let (x0, y0, x1, y1) = swept(k);
            pts.windows(2).any(|s| s[0].0.min(s[1].0) <= x1 && s[0].0.max(s[1].0) >= x0 && s[0].1.min(s[1].1) <= y1 && s[0].1.max(s[1].1) >= y0)
        });
        if crosses {
            hv.skip.push(i);
        }
    }
    hv.skip.sort_unstable();
}

/// Drawn over the cards for the hover, in the pane's points at the pan and zoom now: each lit wire
/// widened and ringed at its ends, the full text of a hovered expression row, a live card's tip and
/// the displayed node's. Its key changes only with what it shows.
pub fn overlay_key(t: &Pane, now: &Now, under: u64) -> u64 {
    let hv = t.hovers.borrow();
    let mut h = std::hash::DefaultHasher::new();
    let shown = !hv.lit.is_empty() || now.tip.is_some() || now.live.is_some() || now.flag;
    (&hv.lit, now.tip, now.live, now.flag).hash(&mut h);
    if shown {
        let chain = chains_of(t).map(|c| (Rc::as_ptr(&c) as usize, c.rev));
        (t.pan_x.to_bits(), t.pan_y.to_bits(), t.zoom.to_bits(), under, t.x, t.y, t.width, t.height, chain, selected_wire_index(t)).hash(&mut h);
    }
    h.finish()
}

/// The pane's place now, for the views painted at the pan and zoom now from a kept pane.
#[derive(Clone, Copy)]
struct Place {
    x: f64,
    y: f64,
    pan_x: f64,
    pan_y: f64,
    zoom: f64,
}

impl Place {
    fn of(t: &Pane) -> Place {
        Place { x: t.x as f64, y: t.y as f64, pan_x: t.pan_x, pan_y: t.pan_y, zoom: t.zoom }
    }
    fn at(&self, q: (f64, f64)) -> (f64, f64) {
        (self.x + self.pan_x + q.0 * self.zoom, self.y + self.pan_y + q.1 * self.zoom)
    }
}

/// The selected wire, by its index in the geometry.
pub fn selected_wire_index(t: &Pane) -> Option<usize> {
    let (sp_, sk, _) = t.selected_wire.as_ref()?;
    t.geo.wires.iter().position(|wi| matches!(&wi.target, Some((tp, tk, _)) if tp == sp_ && tk == sk))
}

pub fn overlay(t: &Pane, ui: &mut Ui, now: &Now, font_size: i64) {
    let b = ui.box_("pxui-scope-hover", BoxOpts::default().w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((0.0, 0.0)));
    if t.highlighted.is_empty() && now.tip.is_none() && now.live.is_none() && !now.flag {
        return;
    }
    let Some(kept) = kept(t) else { return };
    {
        let mut hv = t.hovers.borrow_mut();
        let hv = &mut *hv;
        hv.drawn.clear();
        hv.drawn.extend_from_slice(&t.highlighted);
    }
    let hovers = t.hovers.clone();
    let (place, now) = (Place::of(t), *now);
    ui.draw(&b, move |paint, _| {
        let t = &*kept;
        let hv = hovers.borrow();
        let theme = t.theme;
        let z = place.zoom;
        let at = |q: (f64, f64)| place.at(q);
        let pane = (place.x, place.y, t.width as f64, t.height as f64);
        let fs = font_of(z);
        // (the lit wires themselves are widened in their own views, under the cards)
        for &i in &hv.drawn {
            let wi = &t.geo.wires[i];
            for pair in wi.pts.windows(2) {
                if let Some((a, b)) = clip_segment(at(pair[0]), at(pair[1]), pane) {
                    paint.line_w(p32(a), p32(b), 2.0, theme.foreground);
                }
            }
            for q in [wi.a, wi.b] {
                paint.circle(p32(at(q)), 6.0, None, Some(theme.foreground));
            }
        }
        if let Some(slot) = now.live {
            let (pl, ax, ay) = &t.geo.items[slot];
            let (fx, fy) = (at((*ax, 0.0)).0 + ((pl.w - sp::HEAD_PAD) * z) - 150.0, at((0.0, *ay)).1 + (p::HEAD_HEIGHT * z) + 2.0);
            let size = 4.max(font_size - 2);
            let label = "t  live: reads time or input, re-cooked every frame";
            let tw = f64::from(paint.text_width(Some(size), label)) + 14.0;
            sp::frame_in(paint, fx, fy, tw, 20.0, Some(theme.input), theme::edge(&theme));
            let ty = f64::from(paint.text_top(Some(size), fy as f32, 20.0));
            paint.text_s(((fx + 7.0) as f32, ty as f32), size, theme.foreground, label);
        }
        if now.flag {
            if let Some((_, ax, ay)) = t.display.as_ref().and_then(|path| t.item_at(path)) {
                let (fx, fy) = (at((*ax, 0.0)).0 + (116.0 * z), at((0.0, *ay)).1 + (32.0 * z));
                let size = 4.max(font_size - 2);
                let (label, key) = ("displayed", "V");
                let lw = f64::from(paint.text_width(Some(size), label));
                let kw = f64::from(paint.text_width(Some(size), key));
                let tw = 2.0 + 12.0 + lw + 6.0 + kw;
                sp::frame_in(paint, fx, fy, tw, 20.0, Some(theme.input), theme::edge(&theme));
                let ty = f64::from(paint.text_top(Some(size), fy as f32, 20.0));
                paint.text_s(((fx + 7.0) as f32, ty as f32), size, theme.foreground, label);
                paint.text_s(((fx + 7.0 + lw + 6.0) as f32, ty as f32), size, theme::ink_3(&theme), key);
            }
        }
        // the text hovering over an expression row: its glyph and text in a box, when the field cuts it
        if let Some((slot, i)) = now.tip {
            let (pl, ax, ay) = &t.geo.items[slot];
            if let Item::Item(n) = &pl.item {
                if let p::Chip::Inline { glyph, text } = &n.rows[i].chip {
                    let full = format!("{} {}", sp::shown_glyph(glyph), text);
                    let k = pl.lines.iter().position(|l| matches!(l, p::Line::Row(j, _) if *j == i)).unwrap_or(0);
                    let (x, y) = at((ax + sp::value_x(p::NODE_WIDTH), rows_top(n, *ay) + (k as f64 * p::ROW_HEIGHT)));
                    let size = 4.max(fs - 2);
                    let wd = f64::from(paint.text_width(Some(size), &full)) + (12.0 * z);
                    if wd > sp::FIELD_W * z {
                        sp::frame_in(paint, x, y + (2.0 * z), wd, 20.0 * z, Some(theme.input), theme::edge(&theme));
                        let ty = f64::from(paint.text_top(Some(size), (y + (2.0 * z)) as f32, (20.0 * z) as f32));
                        paint.text_s(((x + (6.0 * z)) as f32, ty as f32), size, theme.foreground, &full);
                    }
                }
            }
        }
    });
}

/// The wires a move of cards carries, where it puts them (`reroute`), at the pan and zoom now: under
/// the cards. Built every frame of the move; empty otherwise.
pub fn drag_wires_key(t: &Pane) -> u64 {
    let Some((dx, dy)) = drag_offset(t) else { return 0 };
    let mut h = std::hash::DefaultHasher::new();
    (dx.to_bits(), dy.to_bits(), t.pan_x.to_bits(), t.pan_y.to_bits(), t.zoom.to_bits(), revision(t), &t.hovers.borrow().skip).hash(&mut h);
    h.finish() | 1
}

pub fn drag_wires(t: &Pane, ui: &mut Ui) {
    let b = ui.box_("pxui-scope-drag-wires", BoxOpts::default().w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((0.0, 0.0)));
    let Some(d) = drag_offset(t) else { return };
    let Some(kept) = kept(t) else { return };
    {
        let mut hv = t.hovers.borrow_mut();
        let hv = &mut *hv;
        hv.runs.clear();
        hv.pts.clear();
        for &i in hv.skip.iter() {
            reroute(&t.geo, i, &hv.moving, d, &mut hv.route, &mut hv.buf);
            let start = hv.pts.len();
            hv.pts.extend_from_slice(&hv.buf);
            hv.runs.push((i, start, hv.pts.len()));
        }
    }
    let hovers = t.hovers.clone();
    let (place, chain, selected) = (Place::of(t), chains_of(t), selected_wire_index(t));
    ui.draw(&b, move |paint, _| {
        let t = &*kept;
        let hv = hovers.borrow();
        let z = place.zoom;
        let at = |q: (f64, f64)| place.at(q);
        let pane = (place.x, place.y, t.width as f64, t.height as f64);
        let tag_size = 5.max((11.0 * z).round() as i64);
        for &(i, a, b) in &hv.runs {
            sp::paint_wire(paint, t, z, &at, &chain, pane, tag_size, &t.geo.wires[i], hv.tags.get(i).map_or("", |s| s.as_str()), &hv.pts[a..b], false, selected == Some(i));
        }
    });
}

/// Over the cards while dragging: the wire pulled from a port to the pointer and the rubber band.
pub fn drag_marks_key(t: &Pane, mouse: (f64, f64), press: (f64, f64)) -> u64 {
    let mut h = std::hash::DefaultHasher::new();
    match &t.drag {
        Some(Drag::Wiring(wg)) => (1u8, wg.from.0.to_bits(), wg.from.1.to_bits(), mouse.0.to_bits(), mouse.1.to_bits()).hash(&mut h),
        Some(Drag::Marquee { .. }) => (2u8, press.0.to_bits(), press.1.to_bits(), mouse.0.to_bits(), mouse.1.to_bits()).hash(&mut h),
        _ => return 0,
    }
    (t.x, t.y, t.width, t.height, revision(t)).hash(&mut h);
    h.finish() | 1
}

pub fn drag_marks(t: &Pane, ui: &mut Ui, mouse: (f64, f64), press: (f64, f64)) {
    let b = ui.box_("pxui-scope-drag-marks", BoxOpts::default().w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((0.0, 0.0)));
    let pane = (t.x as f64, t.y as f64, t.width as f64, t.height as f64);
    let theme = t.theme;
    match &t.drag {
        Some(Drag::Wiring(wg)) => {
            let (from, color) = (wg.from, sp::ty_color(t, wg.ty.as_ref()));
            ui.draw(&b, move |paint, _| {
                if let Some((a, b)) = clip_segment(from, mouse, pane) {
                    paint.line_w(p32(a), p32(b), 1.5, color);
                }
            });
        }
        Some(Drag::Marquee { .. }) => {
            let (x0, y0) = press;
            let (mx, my) = mouse;
            ui.draw(&b, move |paint, _| {
                sp::frame_in(paint, x0.min(mx), y0.min(my), (mx - x0).abs(), (my - y0).abs(), Some(theme::tint(&theme)), theme.accent);
            });
        }
        _ => {}
    }
}

/// What the views built at `key` show, made once for it: the cards in view (the content box `ov`
/// past each side of the pane, grown by 20 points), the wires in it (grown by 4), its open zones and
/// every frame.
pub fn list(t: &Pane, ov: (f64, f64), key: u64) {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    if hv.listed == Some(key) {
        return;
    }
    hv.listed = Some(key);
    let z = t.zoom;
    let (ox, oy) = ov;
    let viewport = (t.x as f64 - ox, t.y as f64 - oy, t.width as f64 + 2.0 * ox, t.height as f64 + 2.0 * oy);
    // what the content box shows, grown by `margin` screen points, in graph units: the indices answer it
    let in_view = |margin: f64| -> (f64, f64, f64, f64) {
        ((-t.pan_x - ox - margin) / z, (-t.pan_y - oy - margin) / z, (t.width as f64 + ox - t.pan_x + margin) / z, (t.height as f64 + oy - t.pan_y + margin) / z)
    };
    let hv = &mut *hv;
    index_query_into(&t.geo.tiles, in_view(20.0), &mut hv.visible);
    hv.visible.retain(|&i| {
        let (pl, ax, ay) = &t.geo.items[i];
        let x = sx(t, *ax) - 20.0;
        let y = sy(t, *ay) - 20.0;
        let wd = (pl.w * z) + 40.0;
        let h = (pl.h * z) + 40.0;
        let (vx, vy, vw, vh) = viewport;
        x < vx + vw && x + wd > vx && y < vy + vh && y + h > vy
    });
    index_query_into(&t.geo.reach, in_view(4.0), &mut hv.reach);
    hv.zones.clear();
    hv.zones.extend(hv.visible.iter().copied().filter(|&i| {
        let (pl, _, _) = &t.geo.items[i];
        matches!(&pl.item, Item::Item(n) if n.zone.is_some()) && !pl.collapsed
    }));
    hv.frames.clear();
    for (origin, (scope, _)) in t.geo.origins.iter().enumerate() {
        hv.frames.extend((0..t.saved.frames(scope).len()).map(|i| (origin, i)));
    }
}

/// A view's label: `c` and the decimal digits of `n`, in `buf` (nothing allocated).
fn label(buf: &mut [u8; 24], c: u8, n: usize) -> &str {
    buf[0] = c;
    let mut digits = [0u8; 20];
    let (mut k, mut v) = (0, n);
    loop {
        digits[k] = b'0' + (v % 10) as u8;
        k += 1;
        v /= 10;
        if v == 0 {
            break;
        }
    }
    for j in 0..k {
        buf[1 + j] = digits[k - 1 - j];
    }
    std::str::from_utf8(&buf[..1 + k]).unwrap_or("?")
}

/// The wires view's build, in the old single painting's order: the ground, each open zone's body,
/// each frame, each wire in view, every one a view of its own over the whole content box, kept while
/// what it shows stays (`key`: the content's build; a wire's own look: lit by the selection's chain,
/// selected, lit for the pointer, carried by a move). A selection or a hover rebuilds the views of the
/// wires whose look changes and writes only their instances.
// ponytail: a view per wire takes one of the painter's 255 view table entries; past them views ride
// their parent's entry and a moved card view rebuilds instead of offsetting. A view kind with no
// entry of its own if graphs with hundreds of wires in view move slowly.
pub fn wires_view(t: &Pane, ui: &mut Ui, key: u64, w: Size, h: Size, chain: &Option<Rc<Chains>>, selected: Option<usize>) {
    let layer = ui.box_("pxui-scope-layer", BoxOpts::default().w(w).h(h).at((0.0, 0.0)));
    let saved = ui.enter(&layer);
    let built = t.built.clone();
    // (a zone's body and a frame move with a drag: views with table entries of their own)
    let full = |ui: &mut Ui, name: &str, rev: u64, moves: bool, paint: &mut dyn FnMut(&mut Ui, &UiBox)| -> UiBox {
        let mut build = |ui: &mut Ui| {
            let b = ui.box_("p", BoxOpts::default().w(w).h(h).at((0.0, 0.0)));
            paint(ui, &b);
        };
        let v = if moves { ui.view(name, rev, w, h, &mut build) } else { ui.view_flat(name, rev, w, h, &mut build) };
        ui.set_at(&v, (0.0, 0.0));
        v
    };
    let kept = |built: &Rc<std::cell::RefCell<Option<(Rc<Pane>, u64)>>>| built.borrow().as_ref().map(|(p, _)| p.clone());
    full(ui, "ground", key, false, &mut |ui, b| {
        let built = built.clone();
        ui.draw(b, move |paint, rect| {
            if let Some(k) = kept(&built) {
                sp::paint_ground(paint, &k, rect.f64());
            }
        });
    });
    let mut buf = [0u8; 24];
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let hv = &mut *hv;
    hv.zone_views.clear();
    for &slot in &hv.zones {
        let v = full(ui, label(&mut buf, b'z', slot), key, true, &mut |ui, b| {
            let built = built.clone();
            ui.draw(b, move |paint, _| {
                if let Some(k) = kept(&built) {
                    sp::paint_zone_body(paint, &k, slot);
                }
            });
        });
        hv.zone_views.push((slot, v));
    }
    hv.frame_views.clear();
    for (k, &(origin, index)) in hv.frames.iter().enumerate() {
        let v = full(ui, label(&mut buf, b'f', k), key, true, &mut |ui, b| {
            let built = built.clone();
            ui.draw(b, move |paint, _| {
                if let Some(k) = kept(&built) {
                    sp::paint_frame(paint, &k, origin, index);
                }
            });
        });
        hv.frame_views.push(v);
    }
    for &i in &hv.reach {
        let wi = &t.geo.wires[i];
        let (lit, is_selected, lit_now, carried) = (chain.as_ref().map(|c| wire_lit(c, wi)), selected == Some(i), t.highlighted.contains(&i), hv.skip.binary_search(&i).is_ok());
        let mut hh = std::hash::DefaultHasher::new();
        (key, lit, is_selected, lit_now, carried).hash(&mut hh);
        full(ui, label(&mut buf, b'w', i), hh.finish(), false, &mut |ui, b| {
            if carried {
                return;
            }
            let (built, chain, hovers) = (built.clone(), chain.clone(), hovers.clone());
            ui.draw(b, move |paint, rect| {
                if let Some(k) = kept(&built) {
                    let z = k.zoom;
                    let tag_size = 5.max((11.0 * z).round() as i64);
                    let wi = &k.geo.wires[i];
                    let hv = hovers.borrow();
                    sp::paint_wire(paint, &k, z, &|q| (sx(&k, q.0), sy(&k, q.1)), &chain, rect.f64(), tag_size, wi, hv.tags.get(i).map_or("", |s| s.as_str()), &wi.pts, lit_now, is_selected);
                }
            });
        });
    }
    ui.leave(saved);
}
