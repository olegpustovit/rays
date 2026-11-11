//! The desktop widget set (proposal P5 "Widget set"): checkbox, radio group, slider, progress,
//! spinner, tabs, segmented control, dropdown, menu bar, tooltip on a node, scroll view, virtual
//! list, tree and table, dialog, toasts, icon button, badge, search field and image. Each is a plain
//! method over `&mut Ui` taking a value and returning the new value (or an intent); each reads its
//! looks from one `States` of the theme and lays its parts out as flow children, so a theme file
//! restyles it and `set_dir(Rtl)` mirrors it. No standard appearance paints with a closure (the
//! image is a texture, the search field's caret the text widgets' canvas). Keyboard: every widget
//! is one Tab stop. As on macOS, a click focuses what edits or holds a selection (the slider, the
//! list, tree and table, the search field) and leaves buttons, boxes, tabs and menus `TAB_ONLY`;
//! Enter/Space activate, the arrows move within a group, Escape leaves. A warm frame allocates
//! nothing. Progress and spinner animate through patches (`busy_step`) without rebuilding their view.
use crate::a11y::state as st;
use crate::draw::Fit;
use crate::geom::color;
use crate::ui_core::*;
use crate::ui_kit::{text_only, SIDE};
use input::{Event, Key};
use std::ops::RangeInclusive;

/// A node of a tree, in preorder: its depth (0 at the top) and whether it has children.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub depth: u16,
    pub children: bool,
}

/// What a dialog's frame did: still open, dismissed (Escape, a press outside), or a button pressed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DialogOut {
    Open,
    Closed,
    Button(usize),
}

/// The key presses of `keys` (a focused node's signal), in order.
fn pressed(keys: &[Event], f: &mut dyn FnMut(Key)) {
    for e in keys {
        if let Event::KeyPressed(k) = e {
            f(*k);
        }
    }
}

/// `i` moved by `by` within `0..n`, wrapping.
fn wrap(i: usize, by: i64, n: usize) -> usize {
    if n == 0 { 0 } else { (i as i64 + by).rem_euclid(n as i64) as usize }
}

/// `i` moved by `by` within `0..n`, held at the ends.
fn clamp_step(i: usize, by: i64, n: usize) -> usize {
    if n == 0 { 0 } else { (i as i64 + by).clamp(0, n as i64 - 1) as usize }
}

const ROW_FLAGS: Flags = CLICKABLE | BLOCKING;
const STOP_FLAGS: Flags = CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY;

impl Ui {
    fn rtl(&self) -> bool {
        self.dir() == Direction::Rtl
    }

    /// Left and right swap under right-to-left: the step the arrow means along a row.
    fn along(&self, k: Key) -> i64 {
        let s = if self.rtl() { -1 } else { 1 };
        match k {
            Key::ArrowRight => s,
            Key::ArrowLeft => -s,
            _ => 0,
        }
    }

    /// The theme's scroll thumb, 2 points in from the ends.
    fn thumb(&self) -> Bar {
        let t = self.theme();
        let thumb = t.scrollbar.idle;
        Bar { color: thumb.fill, radius: thumb.radius[0], width: t.scrollbar_w, gap: t.scrollbar_gap, margin: 2.0, whole: true }
    }

    /// A kit row `rh` high laid out as a row: `SIDE` in from both ends, children centred, 8 apart.
    fn kit_line(&mut self, key: i64, flags: Flags, role: Role) -> Handle {
        let rh = self.row_height() as f32;
        let side = SIDE as f32;
        self.node_keyed(key, Node { flags, h: Size::Px(rh), axis: Axis::Row, align: Align::Center, padding: Edges { l: side, r: side, ..Edges::ZERO }, gap: self.space(1), role, ..Node::DEFAULT })
    }

    /// A text leaf as wide as its text, `h` high, in `look`'s text.
    fn text_leaf(&mut self, key: i64, h: f32, look: Look, text: &str) -> Handle {
        let w = self.text_width(Some(look.font.points(self.font_size())), text);
        let n = self.node_keyed(key, Node { w: Size::Px(w.max(1.0)), h: Size::Px(h), shrink: 1.0, flags: FIT_TEXT, look: text_only(&look), ..Node::DEFAULT });
        self.text(&n, text);
        n
    }

    /// A kit control: a 20-point box with its text 7 points in, the button's looks by default.
    fn push_button(&mut self, key: i64, text: &str, states: &States) -> bool {
        let w = self.text_width(None, text) + 14.0;
        let ch = self.control_h();
        let b = self.node_keyed(key, Node { flags: STOP_FLAGS, w: Size::Px(w), h: Size::Px(ch), shrink: 1.0, min_w: 1.0, inset: Edges { l: 7.0, ..Edges::ZERO }, role: Role::Button, ..Node::DEFAULT });
        let s = self.signal(&b);
        let state = State { hovered: s.hovered, pressed: s.held, ..State::default() };
        self.style(&b, states, state);
        self.node_mut(&b).look.ring = Ring::NONE;
        self.text(&b, text);
        s.clicked
    }

    /// A checkbox row: the 12-point box (an accent square inside when on) and the label.
    pub fn checkbox(&mut self, text: &str, value: bool) -> bool {
        let key = key_of(self.current_seed(), text);
        let row = self.kit_line(key, STOP_FLAGS, Role::CheckBox);
        let s = self.signal(&row);
        let value = value ^ s.clicked;
        self.set_role(&row, Role::CheckBox, st::CHECKABLE | if value { st::CHECKED } else { 0 });
        self.a11y_name(&row, shown(text));
        self.check_parts(&row, s.hovered, s.held, value, false, shown(text));
        value
    }

    /// The box (or ring) and label of a checkbox or radio row `row`.
    fn check_parts(&mut self, row: &Handle, hovered: bool, held: bool, on: bool, round: bool, text: &str) {
        let t = self.theme();
        let (states, inner, label) = (if round { t.radio } else { t.checkbox }, if round { t.notice.idle } else { t.mark.idle }, *t.toggle.pick(State { hovered, ..State::default() }));
        self.set_look(row, Look { text: color::transparent(), ..label }, State { hovered, ..State::default() });
        let rh = self.row_height() as f32;
        let saved = self.enter(row);
        let bx = self.node_keyed(int_key(row.box_key, 0), Node { w: Size::Px(12.0), h: Size::Px(12.0), padding: Edges::all(3.0), ..Node::DEFAULT });
        self.style(&bx, &states, State { hovered, pressed: held, checked: on, ..State::default() });
        if on {
            let inside = self.enter(&bx);
            self.node_keyed(int_key(row.box_key, 1), Node { w: Size::Px(6.0), h: Size::Px(6.0), look: inner, ..Node::DEFAULT });
            self.leave(inside);
        }
        self.text_leaf(int_key(row.box_key, 2), rh, label, text);
        self.leave(saved);
    }

    /// A radio group: one row per option, `selected` the chosen one. One Tab stop; the arrows move
    /// the choice. Returns the choice.
    pub fn radio(&mut self, label: &str, options: &[&str], selected: usize) -> usize {
        let group = self.node(label, Node { flags: FOCUSABLE | TAB_ONLY, role: Role::Group, ..Node::DEFAULT });
        self.a11y_name(&group, shown(label));
        let s = self.signal(&group);
        let mut sel = selected.min(options.len().saturating_sub(1));
        pressed(&s.keys, &mut |k| match k {
            Key::ArrowDown | Key::ArrowRight => sel = wrap(sel, 1, options.len()),
            Key::ArrowUp | Key::ArrowLeft => sel = wrap(sel, -1, options.len()),
            _ => {}
        });
        let saved = self.enter(&group);
        for (i, option) in options.iter().enumerate() {
            let row = self.kit_line(int_key(group.box_key, i as i64), ROW_FLAGS, Role::Radio);
            let rs = self.signal(&row);
            if rs.clicked {
                sel = i;
            }
            self.set_role(&row, Role::Radio, st::CHECKABLE | if sel == i { st::CHECKED } else { 0 });
            self.a11y_name(&row, option);
            self.check_parts(&row, rs.hovered, rs.held, sel == i, true, option);
        }
        self.leave(saved);
        sel
    }

    /// A horizontal slider row: the label, then a rail with the filled part and an 8 x 14 thumb.
    /// Dragging, the arrows (a hundredth of the range; Page keys a tenth; Home and End the ends) and
    /// an assistive technology's SetValue move it. Returns the value, held in `range`.
    pub fn slider(&mut self, text: &str, value: f32, range: RangeInclusive<f32>) -> f32 {
        let (lo, hi) = (*range.start(), *range.end());
        let span = (hi - lo).max(f32::MIN_POSITIVE);
        let key = key_of(self.current_seed(), text);
        let row = self.kit_line(key, NO_FLAGS, Role::Unknown);
        let rh = self.row_height() as f32;
        let ch = self.control_h();
        let label_look = self.theme().row.idle;
        let saved = self.enter(&row);
        self.text_leaf(int_key(key, 0), rh, label_look, shown(text));
        let track = self.node_keyed(int_key(key, 1), Node { flags: CLICKABLE | FOCUSABLE | BLOCKING, h: Size::Px(ch), axis: Axis::Stack, align: Align::Center, role: Role::Slider, ..Node::DEFAULT });
        let s = self.signal(&track);
        let mut v = value.clamp(lo.min(hi), hi.max(lo));
        let step = span / 100.0;
        pressed(&s.keys, &mut |k| {
            v += match k {
                Key::ArrowRight | Key::ArrowLeft => self.along(k) as f32 * step,
                Key::ArrowUp => step,
                Key::ArrowDown => -step,
                Key::PageUp => 10.0 * step,
                Key::PageDown => -10.0 * step,
                Key::Home => lo - v,
                Key::End => hi - v,
                _ => 0.0,
            };
        });
        if s.held || s.released {
            let r = self.rect(&track);
            let f = ((s.pointer.0 - r.x - 4.0) / (r.w - 8.0).max(1.0)).clamp(0.0, 1.0);
            v = lo + if self.rtl() { 1.0 - f } else { f } * span;
        }
        if let Some(a) = s.a11y_value {
            v = a as f32;
        }
        let v = v.clamp(lo.min(hi), hi.max(lo));
        let f = ((v - lo) / span).clamp(0.0, 1.0);
        self.a11y_name(&track, shown(text));
        self.a11y_number(&track, v as f64, lo.min(hi) as f64, hi.max(lo) as f64, step as f64);
        let t = self.theme();
        let (rail, fill, thumb) = (t.separator.idle, t.mark.idle, t.slider);
        let inner = self.enter(&track);
        self.node_keyed(int_key(key, 2), Node { h: Size::Px(2.0), look: rail, ..Node::DEFAULT });
        self.node_keyed(int_key(key, 3), Node { w: Size::Pct(f), h: Size::Px(2.0), look: fill, ..Node::DEFAULT });
        let lane = self.node_keyed(int_key(key, 4), Node { h: Size::Px(14.0), axis: Axis::Row, ..Node::DEFAULT });
        let tl = self.enter(&lane);
        self.node_keyed(int_key(key, 5), Node { h: Size::Px(1.0), grow: f, ..Node::DEFAULT });
        let th = self.node_keyed(int_key(key, 6), Node { w: Size::Px(8.0), h: Size::Px(14.0), ..Node::DEFAULT });
        self.style(&th, &thumb, State { hovered: s.hovered, pressed: s.held, ..State::default() });
        self.node_keyed(int_key(key, 7), Node { h: Size::Px(1.0), grow: 1.0 - f, ..Node::DEFAULT });
        self.leave(tl);
        self.leave(inner);
        self.leave(saved);
        v
    }

    /// A 4-point progress track across a row: a bar `value` (0 to 1) of the way, or with `None` a
    /// third-wide bar that `busy_step` sweeps across. Returns the track, for `busy_step`.
    pub fn progress(&mut self, label: &str, value: Option<f32>) -> Handle {
        let key = key_of(self.current_seed(), label);
        let row = self.kit_line(key, NO_FLAGS, Role::Unknown);
        let (track_look, bar, label_look) = (self.theme().progress.idle, self.theme().mark.idle, self.theme().row.idle);
        let rh = self.row_height() as f32;
        let saved = self.enter(&row);
        self.text_leaf(int_key(key, 1), rh, label_look, shown(label));
        let track = self.node_keyed(int_key(key, 0), Node { flags: CLIP, h: Size::Px(4.0), look: track_look, role: Role::Progress, ..Node::DEFAULT });
        self.a11y_name(&track, shown(label));
        let inner = self.enter(&track);
        match value {
            Some(v) => {
                let v = v.clamp(0.0, 1.0);
                self.node_keyed(int_key(track.box_key, 0), Node { w: Size::Pct(v), h: Size::Grow, look: bar, ..Node::DEFAULT });
                self.a11y_number(&track, (v * 100.0) as f64, 0.0, 100.0, 1.0);
            }
            None => {
                self.node_keyed(int_key(track.box_key, 1), Node { w: Size::Pct(0.3), h: Size::Grow, look: bar, ..Node::DEFAULT });
                self.set_role(&track, Role::Progress, st::BUSY);
            }
        }
        self.leave(inner);
        self.leave(saved);
        track
    }

    /// A 16-point spinner: eight dots round a circle, the lead dot in the accent. `busy_step` turns
    /// it. Returns its node.
    pub fn spinner(&mut self, label: &str) -> Handle {
        let (dim, lead) = (self.theme().spinner.idle, self.theme().spinner.checked);
        let n = self.node(label, Node { w: Size::Px(16.0), h: Size::Px(16.0), role: Role::Progress, a11y: st::BUSY, ..Node::DEFAULT });
        self.a11y_name(&n, shown(label));
        let saved = self.enter(&n);
        for i in 0..8 {
            let a = i as f32 * std::f32::consts::TAU / 8.0;
            let at = (8.0 + 6.0 * a.sin() - 1.5, 8.0 - 6.0 * a.cos() - 1.5);
            self.node_keyed(int_key(n.box_key, i), Node { w: Size::Px(3.0), h: Size::Px(3.0), at: Some(at), look: if i == 0 { lead } else { dim }, ..Node::DEFAULT });
        }
        self.leave(saved);
        n
    }

    /// Between frames: move an indeterminate `progress` bar or turn a `spinner` to time `now` through
    /// patches (no build, no layout; the view keeps its output). Returns whether it moves, so the
    /// host keeps drawing; `false` for a determinate bar, a stale handle or reduced motion.
    pub fn busy_step(&mut self, h: &Handle, now: f64) -> bool {
        if !self.live(h) || self.theme().tokens.reduced_motion {
            return false;
        }
        if self.node_mut(h).role == Role::Progress && self.node_mut(h).w.is_grow() {
            let Some(bar) = self.box_of_key(int_key(h.box_key, 1)).filter(|b| self.live(b)) else { return false };
            let r = self.rect(h);
            let w = r.w * 0.3;
            let p = (now.rem_euclid(1.4) / 1.4) as f32;
            let x = if self.rtl() { r.x + r.w - p * (r.w + w) } else { r.x - w + p * (r.w + w) };
            return self.patch_rect(&bar, Rect::new(x, r.y, w, r.h));
        }
        let t = self.theme();
        let (dim, lead) = (t.spinner.idle, t.spinner.checked);
        let at = (now * 10.0).rem_euclid(8.0) as i64;
        let mut moved = false;
        for i in 0..8 {
            let Some(dot) = self.box_of_key(int_key(h.box_key, i)).filter(|b| self.live(b)) else { return moved };
            let behind = (at - i).rem_euclid(8) as f32;
            moved |= self.patch_look(&dot, &lead.lerp(&dim, (behind / 3.0).min(1.0)));
        }
        moved
    }

    /// A row of tabs, the one in use underlined. One Tab stop; Left and Right (Home, End) switch.
    /// Returns the tab in use.
    pub fn tabs(&mut self, label: &str, tabs: &[&str], selected: usize) -> usize {
        let rh = self.row_height() as f32;
        let (bar_look, states) = (self.theme().header.idle, self.theme().tab);
        let bar = self.node(label, Node { flags: FOCUSABLE | TAB_ONLY, h: Size::Px(rh), axis: Axis::Row, padding: Edges { l: SIDE as f32, r: SIDE as f32, ..Edges::ZERO }, gap: self.space(2), look: bar_look, role: Role::TabList, ..Node::DEFAULT });
        self.a11y_name(&bar, shown(label));
        let s = self.signal(&bar);
        let mut sel = selected.min(tabs.len().saturating_sub(1));
        pressed(&s.keys, &mut |k| match k {
            Key::Home => sel = 0,
            Key::End => sel = tabs.len().saturating_sub(1),
            k => sel = wrap(sel, self.along(k), tabs.len()),
        });
        let saved = self.enter(&bar);
        for (i, tab) in tabs.iter().enumerate() {
            let w = self.text_width(None, tab);
            let n = self.node_keyed(int_key(bar.box_key, i as i64), Node { flags: ROW_FLAGS, w: Size::Px(w), h: Size::Grow, role: Role::Tab, ..Node::DEFAULT });
            let ts = self.signal(&n);
            if ts.clicked {
                sel = i;
            }
            self.style(&n, &states, State { hovered: ts.hovered, pressed: ts.held, checked: sel == i, ..State::default() });
            self.set_role(&n, Role::Tab, st::SELECTABLE | if sel == i { st::SELECTED } else { 0 });
            self.text(&n, tab);
        }
        self.leave(saved);
        sel
    }

    /// A segmented control: the options side by side in one outlined strip, the chosen one filled.
    /// One Tab stop; Left and Right move the choice. Returns the choice.
    pub fn segmented(&mut self, label: &str, options: &[&str], selected: usize) -> usize {
        let key = key_of(self.current_seed(), label);
        let line = self.kit_line(key, NO_FLAGS, Role::Unknown);
        let ch = self.control_h();
        let states = self.theme().segment;
        let saved = self.enter(&line);
        let strip = self.node_keyed(int_key(key, 0), Node { flags: FOCUSABLE | TAB_ONLY, w: Size::Fit, h: Size::Px(ch), axis: Axis::Row, role: Role::Group, ..Node::DEFAULT });
        self.a11y_name(&strip, shown(label));
        let s = self.signal(&strip);
        let mut sel = selected.min(options.len().saturating_sub(1));
        pressed(&s.keys, &mut |k| sel = clamp_step(sel, self.along(k), options.len()));
        let inner = self.enter(&strip);
        for (i, option) in options.iter().enumerate() {
            let w = self.text_width(None, option) + 16.0;
            let n = self.node_keyed(int_key(strip.box_key, i as i64), Node { flags: ROW_FLAGS, w: Size::Px(w), h: Size::Grow, margin: Edges { l: if i == 0 { 0.0 } else { -1.0 }, ..Edges::ZERO }, inset: Edges { l: 8.0, ..Edges::ZERO }, role: Role::Radio, ..Node::DEFAULT });
            let ns = self.signal(&n);
            if ns.clicked {
                sel = i;
            }
            self.style(&n, &states, State { hovered: ns.hovered, pressed: ns.held, checked: sel == i, ..State::default() });
            self.set_role(&n, Role::Radio, st::CHECKABLE | if sel == i { st::CHECKED } else { 0 });
            self.text(&n, option);
        }
        self.leave(inner);
        self.leave(saved);
        sel
    }

    /// Menu rows in an anchored sheet below `anchor`, `current` lit: the row clicked, if any.
    fn menu_sheet(&mut self, anchor: Rect, width: f32, items: &[&str], current: Option<usize>) -> Option<usize> {
        let t = self.theme();
        let (sheet, states) = (t.popup.idle, t.menu);
        let rh = self.row_height() as f32;
        let place = Placement { side: Side::Below, align: Align::Start, gap: 2.0, margin: 4.0, flip: true };
        let mut picked = None;
        let menu = self.anchored("menu", anchor, place, Size::Px(width), Size::Fit, &mut |ui| {
            for (i, item) in items.iter().enumerate() {
                let n = ui.node_keyed(int_key(ui.current_seed(), i as i64), Node { flags: ROW_FLAGS, h: Size::Px(rh), inset: Edges { l: SIDE as f32, ..Edges::ZERO }, role: Role::MenuItem, ..Node::DEFAULT });
                let s = ui.signal(&n);
                if s.clicked {
                    picked = Some(i);
                }
                ui.style(&n, &states, State { hovered: s.hovered, pressed: s.held, checked: current == Some(i), ..State::default() });
                ui.text(&n, item);
            }
        });
        let n = self.node_mut(&menu);
        (n.look, n.padding, n.role, n.flags) = (sheet, Edges { t: 4.0, b: 4.0, ..Edges::ZERO }, Role::Menu, BLOCKING);
        picked
    }

    /// Whether this frame has a press outside `a` and `b`.
    fn pressed_outside(&self, a: Rect, b: Rect) -> bool {
        self.frame_events().iter().any(|e| matches!(e, Event::PointerPressed(_, p) if { let p = (p.0 as f32, p.1 as f32); !a.contains(p) && !b.contains(p) }))
    }

    /// A dropdown row: the label, then a button showing the chosen option and a chevron. A click,
    /// Enter, Space or Down opens its menu below (Up and Down move in it, Enter picks, Escape or a
    /// press outside closes it). Returns the choice. While open its menu escapes a cached view, so
    /// that view rebuilds each frame until it closes.
    pub fn dropdown(&mut self, label: &str, options: &[&str], selected: usize) -> usize {
        let key = key_of(self.current_seed(), label);
        let row = self.kit_line(key, NO_FLAGS, Role::Unknown);
        let rh = self.row_height() as f32;
        let ch = self.control_h();
        let t = self.theme();
        let (label_look, states, chevron) = (t.row.idle, t.dropdown, t.chevron.idle);
        let saved = self.enter(&row);
        self.text_leaf(int_key(key, 0), rh, label_look, shown(label));
        let button = self.node_keyed(int_key(key, 1), Node { flags: STOP_FLAGS, h: Size::Px(ch), axis: Axis::Row, align: Align::Center, gap: 4.0, padding: Edges { l: 4.0, r: 6.0, ..Edges::ZERO }, role: Role::ComboBox, ..Node::DEFAULT });
        let s = self.signal(&button);
        let mut sel = selected.min(options.len().saturating_sub(1));
        // the open menu's lit row, or -1 closed
        let mut lit = self.state(&button, -1);
        let mut pick = false;
        let by_key = s.keys.iter().any(|e| matches!(e, Event::KeyPressed(Key::Enter | Key::Space | Key::KeyChar(' '))));
        pressed(&s.keys, &mut |k| match k {
            Key::ArrowDown if lit < 0 => lit = sel as i64,
            Key::ArrowDown => lit = clamp_step(lit as usize, 1, options.len()) as i64,
            Key::ArrowUp if lit >= 0 => lit = clamp_step(lit as usize, -1, options.len()) as i64,
            Key::Escape => lit = -1,
            _ => {}
        });
        if s.clicked {
            if by_key && lit >= 0 {
                pick = true;
            } else {
                lit = if lit < 0 { sel as i64 } else { -1 };
            }
        }
        if pick {
            sel = lit as usize;
            lit = -1;
        }
        let anchor = self.rect(&button);
        if lit >= 0 {
            let menu_rect = self.box_of_key(key_of(button.box_key, "menu")).map_or(anchor, |m| self.rect(&m));
            if !s.clicked && self.pressed_outside(anchor, menu_rect) {
                lit = -1;
            }
        }
        let open = lit >= 0;
        self.set_role(&button, Role::ComboBox, st::EXPANDABLE | if open { st::EXPANDED } else { 0 });
        self.a11y_name(&button, shown(label));
        self.style(&button, &states, State { hovered: s.hovered, pressed: s.held, focused: self.focused(&button), ..State::default() });
        let inner = self.enter(&button);
        let text = self.node_keyed(int_key(key, 2), Node { flags: FIT_TEXT, h: Size::Grow, look: text_only(&states.idle), ..Node::DEFAULT });
        self.text(&text, options.get(sel).copied().unwrap_or(""));
        let mark = self.node_keyed(int_key(key, 3), Node { w: Size::Px(6.0), h: Size::Grow, look: chevron, ..Node::DEFAULT });
        self.node_mut(&mark).mark = Mark::Chevron(Dir::Down);
        if open {
            if let Some(i) = self.menu_sheet(anchor, anchor.w.max(80.0), options, Some(lit as usize)) {
                sel = i;
                lit = -1;
            }
        }
        self.leave(inner);
        self.leave(saved);
        self.set_state(&button, lit);
        sel
    }

    /// A menu bar: each `(title, items)` a top-level item that opens its menu below it. While one
    /// is open, hovering another switches to it. Keys (Tab to the bar): Left and Right move between
    /// menus, Down, Enter or Space open one, Up and Down move in it, Enter picks, Escape closes.
    /// Returns `(menu, item)` on the frame an item is picked.
    pub fn menu_bar(&mut self, label: &str, menus: &[(&str, &[&str])]) -> Option<(usize, usize)> {
        let rh = self.row_height() as f32;
        let t = self.theme();
        let (bar_look, states) = (t.menubar.idle, t.menu);
        let bar = self.node(label, Node { flags: FOCUSABLE | TAB_ONLY | BLOCKING, h: Size::Px(rh), axis: Axis::Row, padding: Edges { l: 4.0, ..Edges::ZERO }, look: bar_look, role: Role::MenuBar, ..Node::DEFAULT });
        self.a11y_name(&bar, shown(label));
        let s = self.signal(&bar);
        // state: menu * 65536 + (lit + 1) * 2 + open
        let packed = self.state(&bar, 0);
        let (mut menu, mut lit, mut open) = ((packed >> 16) as usize, ((packed & 0xffff) >> 1) as i64 - 1, packed & 1 == 1);
        let count = |m: usize| menus.get(m).map_or(0, |(_, items)| items.len());
        let mut picked = None;
        let by_key = s.keys.iter().any(|e| matches!(e, Event::KeyPressed(Key::Enter | Key::Space | Key::KeyChar(' '))));
        pressed(&s.keys, &mut |k| match k {
            Key::ArrowLeft | Key::ArrowRight => {
                menu = wrap(menu, self.along(k), menus.len());
                lit = -1;
            }
            Key::ArrowDown if !open => (open, lit) = (true, 0),
            Key::ArrowDown => lit = clamp_step(lit.max(0) as usize, 1, count(menu)) as i64,
            Key::ArrowUp if open => lit = clamp_step(lit.max(0) as usize, -1, count(menu)) as i64,
            Key::Escape => open = false,
            _ => {}
        });
        if s.clicked && by_key {
            if open && lit >= 0 {
                picked = Some((menu, lit as usize));
                open = false;
            } else {
                (open, lit) = (true, 0);
            }
        }
        let saved = self.enter(&bar);
        let mut anchor = Rect::new(0.0, 0.0, 0.0, 0.0);
        for (i, (title, _)) in menus.iter().enumerate() {
            let w = self.text_width(None, title) + 16.0;
            let n = self.node_keyed(int_key(bar.box_key, i as i64), Node { flags: ROW_FLAGS, w: Size::Px(w), h: Size::Grow, inset: Edges { l: 8.0, ..Edges::ZERO }, role: Role::MenuItem, ..Node::DEFAULT });
            let ns = self.signal(&n);
            if ns.clicked {
                (open, menu, lit) = (!(open && menu == i), i, -1);
            } else if ns.hovered && open && menu != i {
                (menu, lit) = (i, -1);
            }
            if i == menu {
                anchor = self.rect(&n);
            }
            let on = open && menu == i;
            self.style(&n, &states, State { hovered: ns.hovered, pressed: ns.held, checked: on, ..State::default() });
            self.set_role(&n, Role::MenuItem, st::EXPANDABLE | if on { st::EXPANDED } else { 0 });
            self.text(&n, title);
        }
        if open {
            let menu_rect = self.box_of_key(key_of(bar.box_key, "menu")).map_or(anchor, |m| self.rect(&m));
            if self.pressed_outside(self.rect(&bar), menu_rect) {
                open = false;
            }
        }
        if open {
            if let Some((_, items)) = menus.get(menu) {
                if let Some(i) = self.menu_sheet(anchor, 160.0, items, (lit >= 0).then_some(lit as usize)) {
                    picked = Some((menu, i));
                    open = false;
                }
            }
        }
        self.leave(saved);
        self.set_state(&bar, ((menu as i64) << 16) | ((lit + 1) << 1) | open as i64);
        picked
    }

    /// The kit tooltip for node `h` while the pointer rests on it.
    pub fn tip(&mut self, h: &Handle, text: &str) {
        if self.live(h) && self.signal(h).hovered {
            self.tooltip(text, text);
        }
    }

    /// A box `w` x `h` whose content (`f`'s children, laid out as a column) scrolls on both axes,
    /// with the theme's thumbs. Returns its node.
    pub fn scroll_view(&mut self, label: &str, w: Size, h: Size, f: &mut dyn FnMut(&mut Ui)) -> Handle {
        let bar = self.thumb();
        let look = self.theme().scroll_view.idle;
        let rh = self.row_height() as f32;
        let n = self.node(label, Node { flags: SCROLL | SCROLL_X | CLIP | BLOCKING, w, h, look, bar: Some(bar), scroll_step: rh, padding: Edges::all(1.0), role: Role::ScrollArea, ..Node::DEFAULT });
        self.a11y_name(&n, shown(label));
        self.within(&n, f);
        n
    }

    /// The row being built by `ui.list`: its kit row look on a clickable child that fills it.
    fn virtual_row(&mut self, states: &States, checked: bool, cursor: bool, inset: f32, role: Role, a11y: u32) -> (Handle, Signal) {
        let rh = self.row_height() as f32;
        if let Some(row) = self.box_of_key(self.current_seed()) {
            self.set_role(&row, Role::Unknown, 0);
        }
        let n = self.node("row", Node { flags: ROW_FLAGS, h: Size::Px(rh), axis: Axis::Row, align: Align::Center, padding: Edges { l: inset, r: SIDE as f32, ..Edges::ZERO }, gap: 4.0, role, a11y, ..Node::DEFAULT });
        let s = self.signal(&n);
        self.style(&n, states, State { hovered: s.hovered, pressed: s.held, checked, focused: cursor, ..State::default() });
        (n, s)
    }

    /// Keys of a focused list over `count` rows: the new cursor and whether Enter or Space came.
    fn list_keys(&self, keys: &[Event], cursor: usize, count: usize, page: i64) -> (usize, bool, bool) {
        let (mut c, mut enter, mut space) = (cursor, false, false);
        pressed(keys, &mut |k| match k {
            Key::ArrowDown => c = clamp_step(c, 1, count),
            Key::ArrowUp => c = clamp_step(c, -1, count),
            Key::PageDown => c = clamp_step(c, page, count),
            Key::PageUp => c = clamp_step(c, -page, count),
            Key::Home => c = 0,
            Key::End => c = count.saturating_sub(1),
            Key::Enter => enter = true,
            Key::Space | Key::KeyChar(' ') => space = true,
            _ => {}
        });
        (c, enter, space)
    }

    /// Keep row `row` of the virtual list `body` in view.
    fn reveal_row(&mut self, body: &Handle, row: usize) {
        let rh = self.row_height() as f32;
        let (top, h) = (self.scroll_offset(body), self.rect(body).h);
        let y = row as f32 * rh;
        if y < top {
            self.set_scroll_offset(body, y);
        } else if y + rh > top + h && h > 0.0 {
            self.set_scroll_offset(body, y + rh - h);
        }
    }

    /// A virtual list of `count` rows filling its parent: only the rows in view are built, each
    /// row's text from `text(i, put)`. `selected` (one flag a row) is the selection: a click selects
    /// a row alone; with `multi`, Command (Ctrl) toggles one and Shift selects the run from the
    /// keyboard's row. One Tab stop; Up, Down, Page keys, Home and End move the keyboard's row
    /// (selecting it alone without `multi`; Space toggles it with `multi`). Returns the row a
    /// double click or Enter activated.
    pub fn list_view(&mut self, label: &str, count: usize, multi: bool, selected: &mut [bool], text: &dyn Fn(usize, &mut dyn FnMut(&str))) -> Option<usize> {
        let count = count.min(selected.len());
        let rh = self.row_height() as f32;
        let frame = self.node(label, Node { flags: FOCUSABLE | CLIP, w: Size::Grow, h: Size::Grow, look: self.theme().scroll_view.idle, padding: Edges::all(1.0), ..Node::DEFAULT });
        let s = self.signal(&frame);
        let focused = self.focused(&frame);
        let page = (self.rect(&frame).h / rh).floor().max(1.0) as i64;
        let cursor = (self.state(&frame, 0).max(0) as usize).min(count.saturating_sub(1));
        let (mut c, enter, space) = self.list_keys(&s.keys, cursor, count, page);
        if c != cursor && !multi {
            selected[..count].fill(false);
            selected[c] = true;
        }
        if space && multi && count > 0 {
            selected[c] = !selected[c];
        }
        let mut activated = enter.then_some(c).filter(|_| count > 0);
        let states = self.theme().listbox;
        let saved = self.enter(&frame);
        let body = self.list("rows", count, rh, &mut |ui, i| {
            let (n, rs) = ui.virtual_row(&states, selected[i], focused && i == c, SIDE as f32, Role::ListItem, st::SELECTABLE | if selected[i] { st::SELECTED } else { 0 });
            if rs.clicked {
                let keys = ui.press_keys(&n);
                let command = ui.command_modifiers(&keys);
                if multi && command {
                    selected[i] = !selected[i];
                } else if multi && keys.contains(&Key::Shift) {
                    let (a, b) = (c.min(i), c.max(i));
                    selected[..count].fill(false);
                    selected[a..=b].fill(true);
                } else {
                    selected[..count].fill(false);
                    selected[i] = true;
                }
                c = i;
            }
            if rs.double_clicked {
                activated = Some(i);
            }
            let inner = ui.enter(&n);
            text(i, &mut |t| {
                let (look, rh) = (ui.node_mut(&n).look, ui.row_height() as f32);
                ui.text_leaf(int_key(n.box_key, 1), rh, look, t);
            });
            ui.leave(inner);
        });
        self.leave(saved);
        self.node_mut(&body).bar = Some(self.thumb());
        self.set_role(&body, Role::List, if multi { st::SELECTABLE } else { 0 });
        self.a11y_name(&body, shown(label));
        if c != cursor {
            self.reveal_row(&body, c);
        }
        self.set_state(&frame, c as i64);
        activated
    }

    /// A tree over `nodes` (preorder), virtualised: the rows `visible` (a scratch the caller keeps)
    /// lists, the expanded (`open`) nodes' children under them, each row indented 14 points a level
    /// with a chevron when it has children. A click selects (`selected`, a node index), a click on
    /// the chevron or a double click expands or collapses. Keys: Up and Down move, Right expands or
    /// steps in, Left collapses or steps to the parent, Enter activates, Space toggles. Returns the
    /// node a double click or Enter activated.
    pub fn tree(&mut self, label: &str, nodes: &[TreeNode], open: &mut [bool], selected: &mut Option<usize>, visible: &mut Vec<u32>, text: &dyn Fn(usize, &mut dyn FnMut(&str))) -> Option<usize> {
        // ponytail: the visible rows are re-flattened, O(nodes), each frame the tree rebuilds; keep
        // a running index if trees past a million nodes rebuild often.
        let flatten = |visible: &mut Vec<u32>, open: &[bool]| {
            visible.clear();
            let mut i = 0;
            while i < nodes.len() {
                visible.push(i as u32);
                let mut j = i + 1;
                if nodes[i].children && !open.get(i).copied().unwrap_or(false) {
                    while j < nodes.len() && nodes[j].depth > nodes[i].depth {
                        j += 1;
                    }
                }
                i = j;
            }
        };
        flatten(visible, open);
        let rh = self.row_height() as f32;
        let frame = self.node(label, Node { flags: FOCUSABLE | CLIP, w: Size::Grow, h: Size::Grow, look: self.theme().scroll_view.idle, padding: Edges::all(1.0), ..Node::DEFAULT });
        let s = self.signal(&frame);
        let focused = self.focused(&frame);
        let page = (self.rect(&frame).h / rh).floor().max(1.0) as i64;
        let cursor_node = self.state(&frame, 0).max(0) as usize;
        let cursor = visible.iter().position(|v| *v as usize == cursor_node).unwrap_or(0);
        let (mut c, enter, space) = self.list_keys(&s.keys, cursor, visible.len(), page);
        let mut activated = None;
        if let Some(&node) = visible.get(c) {
            let node = node as usize;
            let is_open = open.get(node).copied().unwrap_or(false);
            let mut toggle = space && nodes[node].children;
            pressed(&s.keys, &mut |k| match self.along(k) {
                1 if nodes[node].children && !is_open => toggle = true,
                1 if nodes[node].children => c = clamp_step(c, 1, visible.len()),
                -1 if nodes[node].children && is_open => toggle = true,
                -1 => c = visible[..c].iter().rposition(|v| nodes[*v as usize].depth < nodes[node].depth).unwrap_or(c),
                _ => {}
            });
            if toggle {
                if let Some(o) = open.get_mut(node) {
                    *o = !*o;
                }
                flatten(visible, open);
            }
            if enter {
                activated = Some(node);
            }
            if c != cursor {
                *selected = visible.get(c).map(|v| *v as usize);
            }
        }
        let t = self.theme();
        let (states, chevron) = (t.tree, t.chevron.idle);
        let saved = self.enter(&frame);
        let mut toggled = None;
        let shown_rows = visible.len();
        let rows: &[u32] = visible;
        let body = self.list("rows", shown_rows, rh, &mut |ui, i| {
            let node = rows[i] as usize;
            let (has, is_open) = (nodes[node].children, open.get(node).copied().unwrap_or(false));
            let on = *selected == Some(node);
            let a11y = st::SELECTABLE | if on { st::SELECTED } else { 0 } | if has { st::EXPANDABLE | if is_open { st::EXPANDED } else { 0 } } else { 0 };
            let inset = SIDE as f32 + 14.0 * nodes[node].depth as f32;
            let (n, rs) = ui.virtual_row(&states, on, focused && i == c, inset, Role::TreeItem, a11y);
            let inner = ui.enter(&n);
            let disclosure = ui.node("open", Node { flags: if has { ROW_FLAGS } else { NO_FLAGS }, w: Size::Px(10.0), h: Size::Grow, look: if has { chevron } else { Look::NONE }, ..Node::DEFAULT });
            if has {
                ui.node_mut(&disclosure).mark = Mark::Chevron(if is_open { Dir::Down } else if ui.rtl() { Dir::Left } else { Dir::Right });
                if ui.signal(&disclosure).clicked {
                    toggled = Some(node);
                }
            }
            text(node, &mut |t| {
                let (look, rh) = (ui.node_mut(&n).look, ui.row_height() as f32);
                ui.text_leaf(int_key(n.box_key, 1), rh, look, t);
                ui.a11y_name(&n, t);
            });
            ui.leave(inner);
            if rs.clicked {
                *selected = Some(node);
                c = i;
            }
            if rs.double_clicked && has {
                toggled = Some(node);
            } else if rs.double_clicked {
                activated = Some(node);
            }
        });
        self.leave(saved);
        if let Some(o) = toggled.and_then(|node| open.get_mut(node)) {
            *o = !*o;
        }
        self.node_mut(&body).bar = Some(self.thumb());
        self.set_role(&body, Role::Tree, 0);
        self.a11y_name(&body, shown(label));
        if c != cursor {
            self.reveal_row(&body, c);
        }
        let cursor_node = visible.get(c).copied().unwrap_or(0);
        self.set_state(&frame, cursor_node as i64);
        activated
    }

    /// A table filling its parent: a header row of `titles` over a virtual list of `rows`, columns
    /// `widths` points wide. The header stays while the rows scroll; the table scrolls sideways when
    /// the columns are wider than it. Dragging the grip after a header resizes its column (32
    /// points at least; `widths` is updated). `sort` marks a column with a chevron (up when
    /// ascending). A click selects a row (`selected`); Up, Down, Page keys, Home and End move it.
    /// Returns the column whose header was clicked (the sort intent).
    #[allow(clippy::too_many_arguments)]
    pub fn table_view(&mut self, label: &str, titles: &[&str], widths: &mut [f32], rows: usize, sort: Option<(usize, bool)>, selected: &mut Option<usize>, cell: &dyn Fn(usize, usize, &mut dyn FnMut(&str))) -> Option<usize> {
        let rh = self.row_height() as f32;
        let columns = titles.len().min(widths.len());
        let t = self.theme();
        let (frame_look, head_look, column, chevron, states) = (t.scroll_view.idle, t.table_head.idle, t.column, t.chevron.idle, t.listbox);
        let bar = self.thumb();
        let frame = self.node(label, Node { flags: FOCUSABLE | SCROLL_X | CLIP, w: Size::Grow, h: Size::Grow, look: frame_look, bar: Some(bar), padding: Edges::all(1.0), role: Role::Table, ..Node::DEFAULT });
        self.a11y_name(&frame, shown(label));
        let s = self.signal(&frame);
        let focused = self.focused(&frame);
        let page = (self.rect(&frame).h / rh - 1.0).floor().max(1.0) as i64;
        let cursor = selected.unwrap_or(0).min(rows.saturating_sub(1));
        let (c, _, _) = self.list_keys(&s.keys, cursor, rows, page);
        if c != cursor || (selected.is_none() && s.keys.iter().any(|e| matches!(e, Event::KeyPressed(Key::ArrowDown | Key::ArrowUp)))) {
            *selected = (rows > 0).then_some(c);
        }
        let total: f32 = widths[..columns].iter().map(|w| w + 1.0).sum();
        let mut sorted = None;
        let saved = self.enter(&frame);
        let sheet = self.node("sheet", Node { w: Size::Grow, min_w: total, h: Size::Grow, ..Node::DEFAULT });
        let inner = self.enter(&sheet);
        let head = self.node("head", Node { h: Size::Px(rh), axis: Axis::Row, look: head_look, ..Node::DEFAULT });
        let hs = self.enter(&head);
        for i in 0..columns {
            let n = self.node_keyed(int_key(head.box_key, i as i64), Node { flags: ROW_FLAGS, w: Size::Px(widths[i]), h: Size::Grow, axis: Axis::Row, align: Align::Center, justify: Justify::SpaceBetween, padding: Edges { l: 8.0, r: 8.0, ..Edges::ZERO }, role: Role::ColumnHeader, ..Node::DEFAULT });
            let ns = self.signal(&n);
            if ns.clicked {
                sorted = Some(i);
            }
            let by = sort.filter(|(col, _)| *col == i);
            self.style(&n, &column, State { hovered: ns.hovered, pressed: ns.held, checked: by.is_some(), ..State::default() });
            self.a11y_name(&n, titles[i]);
            let ms = self.enter(&n);
            let look = self.node_mut(&n).look;
            self.text_leaf(int_key(n.box_key, 0), rh, look, titles[i]);
            if let Some((_, ascending)) = by {
                let m = self.node("sort", Node { w: Size::Px(6.0), h: Size::Grow, look: chevron, ..Node::DEFAULT });
                self.node_mut(&m).mark = Mark::Chevron(if ascending { Dir::Up } else { Dir::Down });
            }
            self.leave(ms);
            let (_, gs) = self.grip(format_args!("grip{}", i), Axis::Row, None);
            if gs.held && gs.drag.0 != 0.0 {
                widths[i] = (widths[i] + if self.rtl() { -gs.drag.0 } else { gs.drag.0 }).max(32.0);
            }
        }
        self.leave(hs);
        let w = widths;
        let body = self.list("rows", rows, rh, &mut |ui, i| {
            let on = *selected == Some(i);
            let (n, rs) = ui.virtual_row(&states, on, focused && on, 0.0, Role::Row, st::SELECTABLE | if on { st::SELECTED } else { 0 });
            if rs.clicked {
                *selected = Some(i);
            }
            let look = text_only(&ui.node_mut(&n).look);
            ui.node_mut(&n).gap = 1.0;
            let inner = ui.enter(&n);
            for col in 0..columns {
                cell(i, col, &mut |t| {
                    let c = ui.node_keyed(int_key(n.box_key, col as i64), Node { w: Size::Px(w[col]), h: Size::Grow, axis: Axis::Row, align: Align::Center, padding: Edges { l: 8.0, r: 8.0, ..Edges::ZERO }, role: Role::Cell, ..Node::DEFAULT });
                    ui.a11y_name(&c, t);
                    let inner = ui.enter(&c);
                    ui.text_leaf(int_key(c.box_key, 0), rh, look, t);
                    ui.leave(inner);
                });
            }
            ui.leave(inner);
        });
        self.node_mut(&body).bar = Some(self.thumb());
        self.set_role(&body, Role::Unknown, 0);
        self.leave(inner);
        self.leave(saved);
        if c != cursor {
            self.reveal_row(&body, c);
        }
        sorted
    }

    /// A modal dialog titled `title`: its title bar, `body`, then `buttons` at the end of a row (the
    /// last is the default: Enter presses it). Escape or a press outside closes it.
    pub fn dialog(&mut self, title: &str, buttons: &[&str], body: &mut dyn FnMut(&mut Ui)) -> DialogOut {
        let mut out = DialogOut::Open;
        let enter = self.key_pressed(Key::Enter);
        let shown_now = self.modal(Some(320.0), title, &mut |ui| {
            let t = ui.theme();
            let (bar, states) = (t.dialog.idle, t.button);
            let rh = ui.row_height() as f32;
            let head = ui.node("title", Node { h: Size::Px(rh + 8.0), look: bar, inset: Edges { l: SIDE as f32, ..Edges::ZERO }, role: Role::Heading, ..Node::DEFAULT });
            ui.text(&head, shown(title));
            body(ui);
            let row = ui.node("buttons", Node { h: Size::Px(rh + 12.0), axis: Axis::Row, align: Align::Center, justify: Justify::End, gap: 4.0, padding: Edges { r: 8.0, ..Edges::ZERO }, ..Node::DEFAULT });
            let saved = ui.enter(&row);
            for (i, b) in buttons.iter().enumerate() {
                if ui.push_button(int_key(row.box_key, i as i64), b, &states) {
                    out = DialogOut::Button(i);
                }
            }
            ui.leave(saved);
        });
        if !shown_now {
            return DialogOut::Closed;
        }
        if enter && out == DialogOut::Open && !buttons.is_empty() {
            out = DialogOut::Button(buttons.len() - 1);
        }
        out
    }

    /// Toasts stacked in the window's lower end corner (the newest lowest), 12 points from its
    /// edges. Each goes `seconds` after it first showed: the engine keeps that time by the toast's
    /// text and asks for the frame (`repaint_after`), so the host needs no timer. Returns a toast
    /// clicked away or expired (one a frame; the caller drops it).
    pub fn toasts(&mut self, label: &str, items: &[&str], seconds: f64) -> Option<usize> {
        if items.is_empty() {
            return None;
        }
        let (w, h) = self.view_size();
        let x = if self.rtl() { 12.0 } else { w - 12.0 };
        let place = Placement { side: Side::Above, align: if self.rtl() { Align::Start } else { Align::End }, gap: 0.0, margin: 12.0, flip: false };
        let (look, states) = (self.theme().toast.idle, self.theme().toast);
        let rh = self.row_height() as f32;
        let mut gone = None;
        let now = self.now();
        let stack = self.anchored(label, Rect::new(x, h - 12.0, 0.0, 0.0), place, Size::Px(260.0), Size::Fit, &mut |ui| {
            for (i, item) in items.iter().enumerate() {
                // keyed by text, so dropping one keeps the others' times
                let n = ui.node_keyed(key_of(ui.current_seed(), item), Node { flags: ROW_FLAGS | FIT_TEXT, h: Size::Px(rh + 12.0), look, inset: Edges { l: SIDE as f32, r: SIDE as f32, ..Edges::ZERO }, role: Role::StatusBar, ..Node::DEFAULT });
                let s = ui.signal(&n);
                ui.style(&n, &states, State { hovered: s.hovered, ..State::default() });
                ui.text(&n, item);
                let born = f64::from_bits(ui.state(&n, now.to_bits() as i64) as u64);
                ui.set_state(&n, born.to_bits() as i64);
                if s.clicked || now >= born + seconds {
                    gone = gone.or(Some(i));
                } else {
                    ui.repaint_at_time(born + seconds);
                }
            }
        });
        self.node_mut(&stack).gap = 8.0;
        gone
    }

    /// A 20-point square button showing `icon` (a glyph of any loaded font) centred, named `label`.
    pub fn icon_button(&mut self, label: &str, icon: &str) -> bool {
        let ch = self.control_h();
        let states = self.theme().icon_button;
        let b = self.node(label, Node { flags: STOP_FLAGS, w: Size::Px(ch), h: Size::Px(ch), role: Role::Button, ..Node::DEFAULT });
        let s = self.signal(&b);
        self.style(&b, &states, State { hovered: s.hovered, pressed: s.held, ..State::default() });
        let tw = self.text_width(None, icon);
        let n = self.node_mut(&b);
        n.look.ring = Ring::NONE;
        n.inset = Edges { l: ((ch - tw) / 2.0).floor(), ..Edges::ZERO };
        self.text(&b, icon);
        self.a11y_name(&b, shown(label));
        s.clicked
    }

    /// A small label in the badge look (`text`, keyed by `label` so `patch_text` can update a count
    /// without a rebuild). Returns its node.
    pub fn badge(&mut self, label: &str, text: &str) -> Handle {
        let look = self.theme().badge.idle;
        let w = self.text_width(Some(look.font.points(self.font_size())), text) + 8.0;
        let n = self.node(label, Node { w: Size::Px(w), h: Size::Px(16.0), look, inset: Edges { l: 4.0, ..Edges::ZERO }, role: Role::Label, ..Node::DEFAULT });
        self.text(&n, text);
        self.a11y_name(&n, shown(label));
        self.a11y_value(&n, text, (0, 0));
        n
    }

    /// A search row: a well holding `icon`, the query (`placeholder` while empty) and, with a query, a
    /// clear button. A click focuses it and the text widgets' editing applies; Escape clears it.
    /// Returns the new query on the frames that change it.
    pub fn search_field(&mut self, label: &str, icon: &str, value: &str, placeholder: &str) -> Option<String> {
        let key = key_of(self.current_seed(), label);
        let row = self.kit_line(key, NO_FLAGS, Role::Unknown);
        let ch = self.control_h();
        let t = self.theme();
        let (states, hint) = (t.search, t.placeholder.idle);
        let saved = self.enter(&row);
        let well = self.node_keyed(int_key(key, 0), Node { h: Size::Px(ch), axis: Axis::Row, align: Align::Center, gap: 4.0, padding: Edges { l: 4.0, r: 2.0, ..Edges::ZERO }, ..Node::DEFAULT });
        let inner = self.enter(&well);
        let glyph = self.text_leaf(int_key(key, 2), ch, hint, icon);
        let field = self.node_keyed(int_key(key, 1), Node { flags: CLICKABLE | FOCUSABLE | BLOCKING | FOCUS_MARK, h: Size::Grow, inset: Edges { l: 2.0, ..Edges::ZERO }, role: Role::TextField, a11y: st::EDITABLE, ..Node::DEFAULT });
        let s = self.signal(&field);
        let focused = self.focused(&field);
        let mut changed = None;
        let mut edit = None;
        if focused {
            let mut e = self.load_text_edit(field.box_key, value);
            let r = self.rect(&field);
            let shift = self.press_shift(&field);
            self.point_text_caret(None, &mut e, &s, shift, r.x + 2.0, r.x + r.w - 2.0);
            for (event, modifiers) in self.key_events(&field) {
                if event == Event::KeyPressed(Key::Escape) {
                    e.text.clear();
                    (e.caret, e.anchor) = (0, 0);
                } else {
                    self.edit_text_event(false, &mut e, &|_| true, &modifiers, &event);
                }
            }
            self.save_text_edit(&e);
            if e.text != value {
                changed = Some(e.text.clone());
            }
            edit = Some(e);
        }
        if s.hovered {
            self.request_cursor(Cursor::Text);
        }
        let state = State { focused, ..State::default() };
        self.style(&well, &states, state);
        let look = *states.pick(state);
        self.node_mut(&glyph).look.text = hint.text;
        let current = changed.as_deref().unwrap_or(value);
        self.a11y_name(&field, shown(label));
        self.a11y_value(&field, current, (0, 0));
        self.node_mut(&field).look = Look { text: if current.is_empty() { hint.text } else { look.text }, ..text_only(&look) };
        match edit {
            Some(edit) => {
                let font_size = self.font_size();
                let marks = Some((look.selection, look.caret));
                let composition = self.composition().to_owned();
                self.canvas(&field, move |paint, rect| {
                    let r = crate::ui_kit::ints(rect);
                    crate::ui_text::paint_text_edit(paint, None, false, 2.0, marks, r, crate::ui_kit::label_y(font_size, r.1, r.3), &composition, &edit)
                });
            }
            None => self.text(&field, if value.is_empty() { placeholder } else { value }),
        }
        if !current.is_empty() {
            let clear = self.node_keyed(int_key(key, 3), Node { flags: ROW_FLAGS, w: Size::Px(16.0), h: Size::Px(16.0), look: Look { text: hint.text, ..Look::NONE }, inset: Edges { l: 4.0, ..Edges::ZERO }, role: Role::Button, ..Node::DEFAULT });
            self.text(&clear, "\u{d7}");
            self.a11y_name(&clear, "Clear");
            if self.signal(&clear).clicked {
                changed = Some(String::new());
            }
        }
        self.leave(inner);
        self.leave(saved);
        changed
    }

    /// An image `w` x `h` placed by `fit` (CSS `object-fit`) in the image look's frame.
    pub fn image(&mut self, label: &str, image: &Image, fit: Fit, w: Size, h: Size) -> Handle {
        let look = self.theme().image.idle;
        let n = self.node(label, Node { w, h, look, padding: Edges::all(1.0), role: Role::Image, ..Node::DEFAULT });
        self.a11y_name(&n, shown(label));
        let image = image.clone();
        self.canvas(&n, move |p, r| p.image_fit(r.x + 1.0, r.y + 1.0, r.w - 2.0, r.h - 2.0, &image, fit, color::white()));
        n
    }
}
