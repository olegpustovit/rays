//! Kit rev 3 pieces the chrome shares: where text sits in a
//! 24-point bar, and the text button.
use std::borrow::Cow;
use pxui::theme;
use pxui::ui::*;
use pxui::geom::color::{self, T as Color};

/// The chrome's three graph inks, the same as `rays_graph::scope_paint::ports` (which the chrome
/// may not reach): an int port's green for a checked file, a float port's blue for an expression,
/// the port hint's yellow for a tip.
pub fn port_ink(theme: &theme::Tokens, port: Port) -> Color {
    let (light, night) = match port {
        Port::Int => ("#3b7d4e", "#74c28e"),
        Port::Float => ("#285f77", "#72b3cf"),
        Port::Hint => ("#f5cf4f", "#f5cf4f"),
    };
    color::hex_exn(if theme::dark(theme) { night } else { light }).expect("port ink")
}

#[derive(Clone, Copy)]
pub enum Port {
    Int,
    Float,
    Hint,
}

pub fn text_y(ui: &Ui, y: f32, h: f32) -> f32 {
    ui.text_top(None, y, h)
}

pub fn cap_size(ui: &Ui) -> i64 {
    8.max(ui.font_size() - 2)
}

pub fn cap_y(ui: &Ui, y: f32, h: f32) -> f32 {
    ui.text_top(Some(cap_size(ui)), y, h)
}

/// the width of a label (`Paint::cap`): upper case, 0.08 em of tracking after each letter
pub fn cap_width(ui: &mut Ui, text: &str) -> f32 {
    ui.cap_width(text)
}

/// the width of a text button, the kit's `.btn`: a transparent 1-point edge, 6, the label, 6 and
/// the key, 6, the edge
pub fn button_width(ui: &mut Ui, hint: Option<&str>, icon: bool, label: &str) -> f32 {
    let size = cap_size(ui);
    14.0 + ui.text_width(None, label)
        + if icon { 13.0 } else { 0.0 }
        + match hint {
            Some(hint) => 6.0 + ui.text_width(Some(size), hint),
            None => 0.0,
        }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Icon {
    Play,
    Stop,
}

/// Options of `button`.
#[derive(Clone, Debug)]
pub struct ButtonOpts {
    pub h: f32,
    pub enabled: bool,
    pub active: bool,
    pub primary: bool,
    pub centered: bool,
    pub hint: Option<Cow<'static, str>>,
    pub icon: Option<Icon>,
}

impl Default for ButtonOpts {
    fn default() -> ButtonOpts {
        ButtonOpts { h: 20.0, enabled: true, active: false, primary: false, centered: false, hint: None, icon: None }
    }
}

/// The chrome's text button as `States`: a fill on hover and press, the control fill while it is
/// checked (`active`), and for the `primary` (the one outlined button of a panel) a line-3 edge in
/// every state. Its text is in its child nodes, not its look.
pub fn button_states(theme: &theme::Tokens, primary: bool) -> theme::States {
    let idle = if primary { theme::Look { border: theme::border(theme), border_w: 1.0, ..theme::Look::NONE } } else { theme::Look::NONE };
    theme::States {
        idle,
        hover: theme::Look { fill: theme::hover_fill(theme), ..idle },
        pressed: theme::Look { fill: theme::pressed_fill(theme), ..idle },
        focus: idle,
        disabled: idle,
        checked: theme::Look { fill: theme.control, ..idle },
    }
}

/// A text node at `x` in its parent, `h` tall (the parent's height), in `look`'s ink at its font.
fn text_node(ui: &mut Ui, key: i64, x: f32, w: f32, h: f32, look: theme::Look, text: &str) {
    let node = ui.node_keyed(key, Node { w: Size::Px(w.max(1.0)), h: Size::Px(h), at: Some((x, 0.0)).into(), look, ..Node::DEFAULT });
    ui.text(&node, text);
}

/// A button is its text and, in ink-3, its key: a node whose look `button_states` picks (a fill on
/// hover and press, the control fill while `active`, the outline of the `primary`), its label and key
/// child nodes. `icon` draws a play triangle or a stop square before the label. True on a click.
pub fn button(ui: &mut Ui, key: &str, at: (f32, f32), w: f32, o: &ButtonOpts, label: &str) -> bool {
    let (bx, by) = at;
    let theme = ui.tokens();
    let boxed = ui.box_(key, BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(w)).h(Size::Px(o.h)).at((bx, by)));
    let signal = ui.signal(&boxed);
    let state = State { hovered: o.enabled && signal.hovered, pressed: o.enabled && signal.held, checked: o.active, ..State::default() };
    ui.node_mut(&boxed, |node| node.look = *button_states(&theme, o.primary).pick(state));
    let color = if o.enabled { theme.foreground } else { theme::ink_3(&theme) };
    let lw = ui.text_width(None, label);
    let tx = match o.icon {
        None if o.centered => ((w - lw) / 2.0).floor(),
        None => 7.0,
        Some(shape) => {
            ui.draw(&boxed, move |paint, Rect { x, y, w: _, h }| {
                let cy = y + (h / 2.0);
                match shape {
                    Icon::Play => {
                        // the sheet's 7 x 8 triangle: half-point scanlines, so a Retina pixel row is one
                        for row in 0..=15 {
                            let top = cy - 4.0 + (0.5 * row as f32);
                            let reach = 1.0 - ((top + 0.25 - cy).abs() / 4.0);
                            paint.fill(x + 7.0, top, 7.0 * reach, 0.5, color);
                        }
                    }
                    Icon::Stop => paint.fill(x + 7.0, cy - 4.0, 8.0, 8.0, color),
                }
            });
            20.0
        }
    };
    let saved = ui.enter(&boxed);
    text_node(ui, pxui::ui_core::int_key(boxed.box_key, 0), tx, lw, o.h, theme::Look { text: color, ..theme::Look::NONE }, label);
    if let Some(hint) = &o.hint {
        let size = cap_size(ui);
        let hw = ui.text_width(Some(size), hint);
        let font = theme::Font { size: -2, ..theme::Font::BODY };
        text_node(ui, pxui::ui_core::int_key(boxed.box_key, 1), tx + lw + 6.0, hw, o.h, theme::Look { text: theme::ink_3(&theme), font, ..theme::Look::NONE }, hint);
    }
    ui.leave(saved);
    signal.clicked && o.enabled
}

/// A colour: a 20-point swatch, then its hex field to the right edge of the control column; the
/// hex text typed (or `hex` unchanged). `key` names the pair; `at` is the control column.
pub fn colour(ui: &mut Ui, key: &str, at: (f32, f32), w: f32, swatch: Color, hex: &str) -> String {
    let (x, y) = at;
    let boxed = ui.box_fmt(format_args!("swatch-{key}"), BoxOpts::default().at((x, y)).w(Size::Px(20.0)).h(Size::Px(20.0)));
    let edge = theme::edge(&ui.tokens());
    ui.draw(&boxed, move |paint, Rect { x: sx, y: sy, w: sw, h: sh }| {
        // a 20-point square with its border inside
        paint.fill(sx, sy, sw, sh, swatch);
        paint.stroke(sx + 0.5, sy + 0.5, sw - 1.0, sh - 1.0, edge);
    });
    let opts = ValueFieldOpts { left: true, ..ValueFieldOpts::default() };
    let valid = |t: &str| color::hex(t).is_ok();
    ui.value_field((x + 28.0, y), w - 28.0, 20.0, &opts, &valid, &format!("hex-{key}"), hex).0
}

/// The kit's switch: 28 x 14, a line-3 edge, an 8-point knob that is ink-3 on the track at the left
/// and the accent on white at the right: the theme's `switch` and `knob` looks, checked when on. True
/// on a click.
pub fn switch(ui: &mut Ui, key: &str, at: (f32, f32), on: bool) -> bool {
    let (sx, sy) = at;
    let checked = State { checked: on, ..State::default() };
    let (track, knob) = (*ui.theme().switch.pick(checked), *ui.theme().knob.pick(checked));
    let boxed = ui.node(key, Node { flags: CLICKABLE | BLOCKING, w: Size::Px(28.0), h: Size::Px(14.0), at: Some((sx, sy)).into(), look: track, ..Node::DEFAULT });
    let saved = ui.enter(&boxed);
    ui.node_keyed(pxui::ui_core::int_key(boxed.box_key, 0), Node { w: Size::Px(8.0), h: Size::Px(8.0), at: Some((if on { 17.0 } else { 3.0 }, 3.0)).into(), look: knob, ..Node::DEFAULT });
    ui.leave(saved);
    ui.signal(&boxed).clicked
}

/// Text tabs: the one in use is ink with a 1-point underline, the others ink-3. Laid out leftwards
/// from `right`; the index clicked and where the row starts.
pub fn segments(ui: &mut Ui, key: &str, right: f32, y: f32, labels: &[&str], active: i64) -> (Option<i64>, f32) {
    let h = 20.0;
    let theme = ui.tokens();
    let total = 12.0 * (labels.len() as i64 - 1).max(0) as f32 + labels.iter().map(|label| ui.text_width(None, label)).sum::<f32>();
    let mut x = right - total;
    let mut clicked = None;
    for (index, label) in labels.iter().enumerate() {
        let w = ui.text_width(None, label);
        let boxed = ui.box_fmt(
            format_args!("{key}-{index}"),
            BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(w + 8.0)).h(Size::Px(h)).at((x - 4.0, y)),
        );
        let signal = ui.signal(&boxed);
        if signal.clicked {
            clicked = Some(index as i64);
        }
        // the one in use is ink with a 1-point underline 4 in from each side; another is ink-3, ink
        // while hovered
        let on = index as i64 == active;
        let ink = if on || signal.hovered { theme.foreground } else { theme::ink_3(&theme) };
        let saved = ui.enter(&boxed);
        text_node(ui, pxui::ui_core::int_key(boxed.box_key, 0), 4.0, w, h, theme::Look { text: ink, ..theme::Look::NONE }, label);
        if on {
            ui.node_keyed(pxui::ui_core::int_key(boxed.box_key, 1), Node { w: Size::Px(w), h: Size::Px(1.0), at: Some((4.0, h - 1.0)).into(), look: theme::Look { fill: theme.foreground, ..theme::Look::NONE }, ..Node::DEFAULT });
        }
        ui.leave(saved);
        x += w + 12.0;
    }
    (clicked, right - total)
}

/// A view's revision: the hash of what it shows (small values, hashed once a frame).
pub fn revision(parts: &dyn Fn(&mut std::hash::DefaultHasher)) -> u64 {
    use std::hash::Hasher;
    let mut hasher = std::hash::DefaultHasher::new();
    parts(&mut hasher);
    hasher.finish()
}

/// A view's revision from anything `Debug`: its debug text hashed as it is written (no allocation),
/// for data with floats and no `Hash`. Costs a formatting pass: called when the enclosing view builds.
pub fn debug_revision(value: &dyn std::fmt::Debug) -> u64 {
    use std::hash::Hasher;
    struct Sink(std::hash::DefaultHasher);
    impl std::fmt::Write for Sink {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.0.write(s.as_bytes());
            Ok(())
        }
    }
    let mut sink = Sink(std::hash::DefaultHasher::new());
    let _ = std::fmt::write(&mut sink, format_args!("{value:?}"));
    sink.0.finish()
}

/// A cached view as large as the window at its corner: `f` builds boxes at window positions, and runs
/// only when `revision` changes or the view is dirtied (input on its boxes, hover entering or leaving
/// them, the theme, the window's size).
pub fn cached(ui: &mut Ui, label: &str, revision: u64, f: &mut dyn FnMut(&mut Ui)) -> Handle {
    let (w, h) = ui.view_size();
    let view = ui.view(label, revision, Size::Px(w), Size::Px(h), f);
    ui.set_at(&view, (0.0, 0.0));
    view
}

/// `f` with `label` formatted on the stack (a heap string only past 96 bytes): a label made per frame
/// without an allocation.
pub fn with_label(label: std::fmt::Arguments, f: &mut dyn FnMut(&str)) {
    struct Buf {
        bytes: [u8; 96],
        len: usize,
        spill: Option<String>,
    }
    impl std::fmt::Write for Buf {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            match &mut self.spill {
                Some(spill) => spill.push_str(s),
                None if self.len + s.len() <= self.bytes.len() => {
                    self.bytes[self.len..self.len + s.len()].copy_from_slice(s.as_bytes());
                    self.len += s.len();
                }
                None => {
                    let mut spill = std::str::from_utf8(&self.bytes[..self.len]).expect("utf-8").to_owned();
                    spill.push_str(s);
                    self.spill = Some(spill);
                }
            }
            Ok(())
        }
    }
    let mut buf = Buf { bytes: [0; 96], len: 0, spill: None };
    let _ = std::fmt::Write::write_fmt(&mut buf, label);
    match &buf.spill {
        Some(spill) => f(spill),
        None => f(std::str::from_utf8(&buf.bytes[..buf.len]).expect("utf-8")),
    }
}

/// `cap_y` for a painter, which has the paint and not the handle.
pub fn cap_y_paint(paint: &Paint, y: f32, h: f32) -> f32 {
    paint.text_top(Some(8.max(paint.font_size - 2)), y, h)
}

/// A vector: cells in the control column, 8 between, each with its axis letter in ink-3 drawn on
/// `boxed` (the row); `cell index x w` makes the cell's own field (x, w relative to `boxed`).
/// `reserve` keeps room at the right for a row's toggle.
pub fn vector(ui: &mut Ui, boxed: &UiBox, at: (f32, f32), w: f32, reserve: f32, axes: &[&str], cell: &mut dyn FnMut(&mut Ui, usize, f32, f32)) {
    let (cx, cy) = at;
    let n = axes.len() as f32;
    let cell_w = (w - reserve - (8.0 * (n - 1.0))) / n;
    let theme = ui.tokens();
    let cap = cap_size(ui);
    for (index, axis) in axes.iter().enumerate() {
        // the cells' edges land on whole points, as the sheet's do
        let start = cx + (index as f32 * (cell_w + 8.0));
        let fx = start.round();
        let fw = (start + cell_w).round() - start.round();
        let axis = axis.to_string();
        ui.draw(boxed, move |paint, Rect { x, y, w: _, h: _ }| {
            let ty = cap_y_paint(paint, y + cy - 0.5, 20.0);
            paint.text_s((x + fx + 2.0, ty), cap, theme::ink_3(&theme), &axis);
        });
        cell(ui, index, fx, fw);
    }
}
