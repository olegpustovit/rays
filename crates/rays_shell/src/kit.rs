//! Kit rev 3 pieces the chrome shares: where text sits in a
//! 24-point bar, and the text button.
use std::borrow::Cow;
use pxui::theme;
use pxui::ui::*;
use rays_math::color::{self, T as Color};

/// The chrome's three graph inks, the same as `rays_graph::scope_paint::ports` (which the chrome
/// may not reach): an int port's green for a checked file, a float port's blue for an expression,
/// the port hint's yellow for a tip.
pub fn port_ink(theme: &theme::Theme, port: Port) -> Color {
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

pub fn text_y(ui: &Ui, y: f64, h: f64) -> f64 {
    ui.text_top(None, y, h)
}

pub fn cap_size(ui: &Ui) -> i64 {
    8.max(ui.font_size() - 2)
}

pub fn cap_y(ui: &Ui, y: f64, h: f64) -> f64 {
    ui.text_top(Some(cap_size(ui)), y, h)
}

/// the width of a label (`Paint::cap`): upper case, 0.08 em of tracking after each letter
pub fn cap_width(ui: &mut Ui, text: &str) -> f64 {
    ui.cap_width(text)
}

/// the width of a text button, the kit's `.btn`: a transparent 1-point edge, 6, the label, 6 and
/// the key, 6, the edge
pub fn button_width(ui: &mut Ui, hint: Option<&str>, icon: bool, label: &str) -> f64 {
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
    pub h: f64,
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

/// A button is its text and, in ink-3, its key: a fill on hover and press, the control fill
/// while `active`; `primary` is the one outlined button of a panel. `icon` draws a play triangle
/// or a stop square before the label. True on a click.
pub fn button(ui: &mut Ui, key: &str, at: (f64, f64), w: f64, o: &ButtonOpts, label: &str) -> bool {
    let (bx, by) = at;
    let theme = ui.theme();
    let boxed = ui.box_(key, BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(w)).h(Size::Px(o.h)).at((bx, by)));
    let signal = ui.signal(&boxed);
    let held = o.enabled && signal.held;
    let hovered = o.enabled && signal.hovered;
    let (enabled, active, primary, centered, icon) = (o.enabled, o.active, o.primary, o.centered, o.icon);
    let label = ui.cached_label(&boxed, 0, label);
    let hint = o.hint.as_ref().map(|hint| ui.cached_label(&boxed, 1, hint));
    let size = cap_size(ui);
    ui.draw(&boxed, move |paint, (x, y, w, h)| {
        paint_button_ground(paint, &theme, held, hovered, active, primary, (x, y, w, h));
        let color = if enabled { theme.foreground } else { theme::ink_3(&theme) };
        let tx = match icon {
            None if centered => x + ((w - paint.text_width(None, &label)) / 2.0).floor(),
            None => x + 7.0,
            Some(shape) => {
                let cy = y + (h / 2.0);
                match shape {
                    Icon::Play => {
                        // the sheet's 7 x 8 triangle: half-point scanlines, so a Retina pixel row is one
                        for row in 0..=15 {
                            let top = cy - 4.0 + (0.5 * row as f64);
                            let reach = 1.0 - ((top + 0.25 - cy).abs() / 4.0);
                            paint.fill(x + 7.0, top, 7.0 * reach, 0.5, color);
                        }
                    }
                    Icon::Stop => paint.fill(x + 7.0, cy - 4.0, 8.0, 8.0, color),
                }
                x + 20.0
            }
        };
        let ty = paint.text_top(None, y, h);
        paint.text_c((tx, ty), color, &label);
        if let Some(hint) = &hint {
            let hy = cap_y_of(paint, y, h);
            let lw = paint.text_width(None, &label);
            paint.text_s((tx + lw + 6.0, hy), size, theme::ink_3(&theme), hint);
        }
    });
    signal.clicked && enabled
}

fn cap_y_of(paint: &Paint, y: f64, h: f64) -> f64 {
    cap_y_paint(paint, y, h)
}

/// A colour: a 20-point swatch, then its hex field to the right edge of the control column; the
/// hex text typed (or `hex` unchanged). `key` names the pair; `at` is the control column.
pub fn colour(ui: &mut Ui, key: &str, at: (f64, f64), w: f64, swatch: Color, hex: &str) -> String {
    let (x, y) = at;
    let boxed = ui.box_fmt(format_args!("swatch-{key}"), BoxOpts::default().at((x, y)).w(Size::Px(20.0)).h(Size::Px(20.0)));
    let edge = theme::edge(&ui.theme());
    ui.draw(&boxed, move |paint, (sx, sy, sw, sh)| {
        // a 20-point square with its border inside
        paint.fill(sx, sy, sw, sh, swatch);
        paint.stroke(sx + 0.5, sy + 0.5, sw - 1.0, sh - 1.0, edge);
    });
    let opts = ValueFieldOpts { left: true, ..ValueFieldOpts::default() };
    let valid = |t: &str| color::hex(t).is_ok();
    ui.value_field((x + 28.0, y), w - 28.0, 20.0, &opts, &valid, &format!("hex-{key}"), hex).0
}

/// The kit's switch: 28 x 14, a line-3 edge, an 8-point knob that is ink-3 on the track at the left
/// and the accent on white at the right. True on a click.
pub fn switch(ui: &mut Ui, key: &str, at: (f64, f64), on: bool) -> bool {
    let (sx, sy) = at;
    let boxed = ui.box_(key, BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(28.0)).h(Size::Px(14.0)).at((sx, sy)));
    let signal = ui.signal(&boxed);
    let theme = ui.theme();
    ui.draw(&boxed, move |paint, (x, y, _, _)| paint_switch(paint, &theme, x, y, on));
    signal.clicked
}

/// Text tabs: the one in use is ink with a 1-point underline, the others ink-3. Laid out leftwards
/// from `right`; the index clicked and where the row starts.
pub fn segments(ui: &mut Ui, key: &str, right: f64, y: f64, labels: &[String], active: i64) -> (Option<i64>, f64) {
    let h = 20.0;
    let theme = ui.theme();
    let total = 12.0 * (labels.len() as i64 - 1).max(0) as f64 + labels.iter().map(|label| ui.text_width(None, label)).sum::<f64>();
    let mut x = right - total;
    let mut clicked = None;
    for (index, label) in labels.iter().enumerate() {
        let w = ui.text_width(None, label);
        let boxed = ui.box_(
            &format!("{key}-{index}"),
            BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(w + 8.0)).h(Size::Px(h)).at((x - 4.0, y)),
        );
        let signal = ui.signal(&boxed);
        if signal.clicked {
            clicked = Some(index as i64);
        }
        let hovered = signal.hovered;
        let label = ui.cached_label(&boxed, 0, label);
        ui.draw(&boxed, move |paint, (x, y, w, h)| {
            let on = index as i64 == active;
            let ty = paint.text_top(None, y, h);
            paint.text_c((x + 4.0, ty), if on || hovered { theme.foreground } else { theme::ink_3(&theme) }, &label);
            if on {
                paint.fill(x + 4.0, y + h - 1.0, w - 8.0, 1.0, theme.foreground);
            }
        });
        x += w + 12.0;
    }
    (clicked, right - total)
}

/// `cap_y` for a painter, which has the paint and not the handle.
pub fn cap_y_paint(paint: &Paint, y: f64, h: f64) -> f64 {
    paint.text_top(Some(8.max(paint.font_size - 2)), y, h)
}

/// A vector: cells in the control column, 8 between, each with its axis letter in ink-3 drawn on
/// `boxed` (the row); `cell index x w` makes the cell's own field (x, w relative to `boxed`).
/// `reserve` keeps room at the right for a row's toggle.
pub fn vector(ui: &mut Ui, boxed: &UiBox, at: (f64, f64), w: f64, reserve: f64, axes: &[&str], cell: &mut dyn FnMut(&mut Ui, usize, f64, f64)) {
    let (cx, cy) = at;
    let n = axes.len() as f64;
    let cell_w = (w - reserve - (8.0 * (n - 1.0))) / n;
    let theme = ui.theme();
    let cap = cap_size(ui);
    for (index, axis) in axes.iter().enumerate() {
        // the cells' edges land on whole points, as the sheet's do
        let start = cx + (index as f64 * (cell_w + 8.0));
        let fx = start.round();
        let fw = (start + cell_w).round() - start.round();
        let axis = axis.to_string();
        ui.draw(boxed, move |paint, (x, y, _, _)| {
            let ty = cap_y_paint(paint, y + cy - 0.5, 20.0);
            paint.text_s((x + fx + 2.0, ty), cap, theme::ink_3(&theme), &axis);
        });
        cell(ui, index, fx, fw);
    }
}
