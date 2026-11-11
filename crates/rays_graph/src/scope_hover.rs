//! The graph pane's hover between builds. The content view's boxes are `HOVER_FREE`: the pointer
//! moving over cards, ports and wires builds nothing (a card painted whole for its hover is in the
//! content's revision instead). What it is over comes from the hit boxes the
//! last content build left (`Hovers`); a card's ground (its tile's look) and its row highlight change
//! through the patch path (the theme's hover transition; the default theme's is instant), and what
//! is drawn over the cards for it (the lit wires, the tips) is a small view of its own.
use crate::scope_frame::{row_at, sx, sy};
use crate::scope_paint as sp;
use crate::scope_state::*;
use crate::scope_wires::*;
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level};
use pxui::geom::p32;
use pxui::theme::{self, Look, State};
use pxui::ui::*;
use std::rc::Rc;

/// A card the last build left: its item, its tile, and the ground when its tile's look is the card.
pub struct Card {
    pub slot: usize,
    pub tile: UiBox,
    pub ground: Option<Ground>,
    /// painted whole for its hover (a chip, a folded zone, an expression card): the content builds
    /// again when it is hovered or left
    pub whole: bool,
}

/// A card's fill and edge, idle and hovered, and its row highlight: the node, the line it was built
/// at and the row height it was built with.
#[derive(Clone, Copy)]
pub struct Ground {
    pub idle: Look,
    pub hover: Look,
    pub row: Option<(UiBox, usize, f32)>,
}

#[derive(Default)]
pub struct Hovers {
    pub cards: Vec<Card>,
    /// out-ports, then in-ports, card by card
    pub ports: Vec<UiBox>,
    /// wire hit boxes and the wires in view, in the build's order
    pub wires: Vec<(UiBox, usize)>,
    pub reach: Vec<usize>,
    /// the geometry the build read (its items and wires), by address
    pub geo: (usize, usize),
    /// the card shown hovered (its tile's key) and its hovered row's line; `fresh` after a build,
    /// whose declarations put every card back to idle
    shown: (i64, Option<usize>),
    fresh: bool,
    /// the wires lit for the hover, and a list to compare it with
    lit: Vec<usize>,
    next: Vec<usize>,
    hot: Vec<usize>,
}

impl Hovers {
    /// A build declared every card idle again (a verifying run too, which keeps the lists).
    pub fn declared(&mut self) {
        self.fresh = true;
    }

    /// A build starts over (the lists keep their room).
    pub fn clear(&mut self, t: &Pane) {
        self.cards.clear();
        self.ports.clear();
        self.wires.clear();
        self.reach.clear();
        self.geo = (Rc::as_ptr(&t.geo.items) as *const u8 as usize, Rc::as_ptr(&t.geo.wires) as *const u8 as usize);
        self.fresh = true;
    }
}

/// The ground of a card painted as its tile's look: its fill and a one-point edge inside it
/// (`paint_card`'s two instances); hovered, the hover fill unless selected. A refused node's edge is
/// the invalid colour.
pub fn ground(t: &Pane, selected: bool, failed: bool) -> (Look, Look) {
    let theme = t.theme;
    let look = |fill| Look { fill, border: if failed { theme::invalid() } else { sp::card_edge(t, fill, selected) }, border_w: 1.0, ..Look::NONE };
    (look(theme.input), look(if selected { theme.input } else { theme::hover_fill(&theme) }))
}

/// What the pointer is over: the card shown hovered and its row's line, an expression row whose full
/// text shows, a live card's `t` and the displayed node's flag rested on.
#[derive(Clone, Copy, Default, PartialEq)]
pub struct Now {
    pub card: i64,
    /// the card painted whole that is hovered (its tile's key; the content's revision reads it)
    pub whole: i64,
    pub line: Option<usize>,
    pub tip: Option<(usize, usize)>,
    pub live: Option<usize>,
    pub flag: bool,
}

/// Before the build: what the pointer is over, from the boxes the last build left; the pane's
/// hovered row and lit wires follow it (rebuilt only when they change).
pub fn find(t: &mut Pane, ui: &mut Ui, mouse: (f64, f64)) -> Now {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let mut now = Now::default();
    let z = t.zoom;
    let same_geo = hv.geo == (Rc::as_ptr(&t.geo.items) as *const u8 as usize, Rc::as_ptr(&t.geo.wires) as *const u8 as usize);
    if !same_geo {
        // the geometry moved since the build (it builds this frame): nothing is hovered until then
        t.hovered_at = None;
        return now;
    }
    let free = t.drag.is_none() && t.carry_hot.is_none();
    let mut row: Option<(usize, usize)> = None;
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
            }
        }
        if free && c.whole {
            now.whole = c.tile.box_key;
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
    // the wires under the pointer, and those at the port under it
    let port = hv.ports.iter().find(|b| ui.hovered_within(b)).map(|b| {
        let r = ui.rect(b);
        (f64::from(r.x + r.w / 2.0), f64::from(r.y + r.h / 2.0))
    });
    let mut hot = std::mem::take(&mut hv.hot);
    hot.clear();
    for (b, i) in &hv.wires {
        if !hot.contains(i) && ui.hovered_within(b) {
            hot.push(*i);
        }
    }
    let same_port = |pt: (f64, f64)| port.is_some_and(|(x, y)| (sx(t, pt.0) - x).abs() <= (1.5 * z) + 0.5 && (sy(t, pt.1) - y).abs() <= (1.5 * z) + 0.5);
    let mut next = std::mem::take(&mut hv.next);
    next.clear();
    if t.context.is_none() {
        for &i in &hv.reach {
            let wi = &t.geo.wires[i];
            if hot.contains(&i) || same_port(wi.a) || same_port(wi.b) {
                next.push(i);
            }
        }
    }
    // (against the pane's own list: clones of a pane share these records)
    let same = t.highlighted.len() == next.len() && next.iter().rev().zip(&t.highlighted).all(|(&i, wi)| t.geo.wires[i] == *wi);
    if !same {
        t.highlighted = next.iter().rev().map(|&i| t.geo.wires[i].clone()).collect();
    }
    std::mem::swap(&mut next, &mut hv.lit);
    (hv.hot, hv.next) = (hot, next);
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

/// After the build: the hovered card's ground and row highlight, through the patch path (a card the
/// build declared again starts idle).
pub fn show(t: &Pane, ui: &mut Ui, now: &Now) {
    let hovers = t.hovers.clone();
    let mut hv = hovers.borrow_mut();
    let want = (now.card, now.line);
    if hv.shown == want && !hv.fresh {
        return;
    }
    let was = hv.shown.0;
    let curve = ui.tokens().transition(State::default(), State { hovered: true, ..State::default() });
    let set = |ui: &mut Ui, b: &UiBox, look: Look| {
        if curve.instant() {
            ui.patch_look(b, &look);
        } else {
            ui.animate(b, pxui::ui::Target::Look(look), curve);
        }
    };
    for c in &hv.cards {
        let key = c.tile.box_key;
        let Some(g) = &c.ground else { continue };
        if key != was && key != want.0 {
            continue;
        }
        let on = key == want.0;
        set(ui, &c.tile, if on { g.hover } else { g.idle });
        if let Some((hl, k0, rh)) = g.row {
            let line = if on { want.1 } else { None };
            let fill = if line.is_some() { theme::faint_border(&t.theme) } else if on { g.hover.fill } else { g.idle.fill };
            set(ui, &hl, Look { fill, ..Look::NONE });
            ui.animate(&hl, pxui::ui::Target::Offset(0.0, line.map_or(0.0, |k| (k as f32 - k0 as f32) * rh)), curve);
        }
    }
    (hv.shown, hv.fresh) = (want, false);
}

/// Drawn over the cards for the hover, in the pane's points at the pan and zoom now: each lit wire
/// widened and ringed at its ends, the full text of a hovered expression row, a live card's tip and
/// the displayed node's. Its key changes only with what it shows.
pub fn overlay_key(t: &Pane, now: &Now) -> u64 {
    use std::hash::{Hash, Hasher};
    let hv = t.hovers.borrow();
    let mut h = std::hash::DefaultHasher::new();
    let shown = !hv.lit.is_empty() || now.tip.is_some() || now.live.is_some() || now.flag;
    (&hv.lit, now.tip, now.live, now.flag).hash(&mut h);
    if shown {
        (t.pan_x.to_bits(), t.pan_y.to_bits(), t.zoom.to_bits(), revision(t), t.x, t.y, t.width, t.height).hash(&mut h);
    }
    h.finish()
}

pub fn overlay(t: &Pane, ui: &mut Ui, now: &Now, font_size: i64) {
    let b = ui.box_("pxui-scope-hover", BoxOpts::default().w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((0.0, 0.0)));
    if t.highlighted.is_empty() && now.tip.is_none() && now.live.is_none() && !now.flag {
        return;
    }
    let snap = Rc::new(t.clone());
    let now = *now;
    let z = t.zoom;
    let fs = font_of(z);
    ui.draw(&b, move |paint, _| {
        let t = &*snap;
        let theme = t.theme;
        let pane = (t.x as f64, t.y as f64, t.width as f64, t.height as f64);
        let chain = chains_of(t);
        let tag_size = 5.max((11.0 * z).round() as i64);
        for wi in &t.highlighted {
            sp::paint_wire(paint, t, &chain, pane, tag_size, wi, true);
        }
        for wi in &t.highlighted {
            let pts: Vec<(f64, f64)> = wi.pts.iter().map(|(x, y)| (sx(t, *x), sy(t, *y))).collect();
            sp::paint_polyline(pane, paint, 2.0, theme.foreground, &pts);
            for (x, y) in [wi.a, wi.b] {
                paint.circle(((sx(t, x)) as f32, (sy(t, y)) as f32), 6.0, None, Some(theme.foreground));
            }
        }
        if let Some(slot) = now.live {
            let (pl, ax, ay) = &t.geo.items[slot];
            let (fx, fy) = (sx(t, *ax) + ((pl.w - sp::HEAD_PAD) * z) - 150.0, sy(t, *ay) + (p::HEAD_HEIGHT * z) + 2.0);
            let size = 4.max(font_size - 2);
            let label = "t  live: reads time or input, re-cooked every frame";
            let tw = f64::from(paint.text_width(Some(size), label)) + 14.0;
            sp::frame_in(paint, fx, fy, tw, 20.0, Some(theme.input), theme::edge(&theme));
            let ty = f64::from(paint.text_top(Some(size), fy as f32, 20.0));
            paint.text_s(((fx + 7.0) as f32, ty as f32), size, theme.foreground, label);
        }
        if now.flag {
            if let Some((_, ax, ay)) = t.display.as_ref().and_then(|path| t.item_at(path)) {
                let (fx, fy) = (sx(t, *ax) + (116.0 * z), sy(t, *ay) + (32.0 * z));
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
                    let x = sx(t, ax + sp::value_x(p::NODE_WIDTH));
                    let y = sy(t, rows_top(n, *ay) + (k as f64 * p::ROW_HEIGHT));
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
