// Draw cards, wires, zones and footers through Paint rather than Ui. Text uses Paint::text_top and the shared theme.
use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use crate::node_menu;
use crate::scope_state::{self as st, Fr, Pane};
use crate::scope_wires::{self as w, clip_segment, root_name, Wire};
use flow_graph::flow_edit as e;
use flow_graph::probe::{Execution, Footer};
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level, Placed, ZoneKind};
use flow_syntax::lisp;
use flow_syntax::syntax::{Node as SNode, Syntax};
use flow_syntax::ty::{self, Ty};
use pxui::theme::{self, Tokens as Theme};
use pxui::ui_core::{ellipsis, Paint};
use pxui::geom::{color::{self, T as Color}, p32};

type S = Syntax;

/// A port's colour is its type's (`node_menu::port_color`, the menu's squares too); an untyped one is
/// the output's.
pub fn port_color(theme: &Theme, ty: Option<&Ty>) -> Color {
    match ty {
        Some(ty) => node_menu::port_color(theme, ty),
        None => ports(theme).output,
    }
}

pub fn colors(t: &Pane) -> Ports {
    ports(&t.theme)
}

pub fn ty_color(t: &Pane, ty: Option<&Ty>) -> Color {
    port_color(&t.theme, ty)
}

fn wired(r: &p::Row) -> bool {
    !p::sources(r).is_empty()
}

/// ponytail: a private copy of `Scope_commands.frame_list` (the frames of a scope with a drag applied).
fn frame_list(t: &Pane, scope: &[Symbol]) -> Vec<Fr> {
    let frames = t.saved.frames(scope);
    match &t.drag {
        Some(st::Drag::Sizing { scope: s, index, dw, dh }) if s.as_slice() == scope => frames
            .into_iter()
            .enumerate()
            .map(|(i, (title, at, (w0, h0)))| if i as i64 == *index { (title, at, (48.0f64.max(w0 + dw), 32.0f64.max(h0 + dh))) } else { (title, at, (w0, h0)) })
            .collect(),
        Some(st::Drag::Carrying { scope: s, index, dx, dy, .. }) if s.as_slice() == scope => frames
            .into_iter()
            .enumerate()
            .map(|(i, (title, (x, y), size))| if i as i64 == *index { (title, (x + dx, y + dy), size) } else { (title, (x, y), size) })
            .collect(),
        _ => frames,
    }
}

pub fn fitted<'a>(paint: &mut Paint, size: i64, width: f64, text: &'a str) -> Cow<'a, str> {
    // as before, the mark alone when not even it fits
    if f64::from(paint.text_width(Some(size), text)) <= width {
        return Cow::Borrowed(text);
    }
    let t = ellipsis(&mut |s: &str| paint.text_width(Some(size), s), width as f32, text);
    if t.is_empty() { Cow::Borrowed("\u{2026}") } else { t }
}

fn dashed_line(paint: &mut Paint, width: f64, color: Color, a: (f64, f64), b: (f64, f64)) {
    paint.dashed_w(p32(a), p32(b), width as f32, color);
}

/// A 1-point frame inside the box, on whole points: a stroke is centred on its rectangle.
pub fn frame_in(paint: &mut Paint, x: f64, y: f64, w: f64, h: f64, fill: Option<Color>, color: Color) {
    if let Some(f) = fill {
        paint.fill(x as f32, y as f32, w as f32, h as f32, f);
    }
    paint.stroke((x + 0.5) as f32, (y + 0.5) as f32, (w - 1.0) as f32, (h - 1.0) as f32, color);
}

/// The same, dashed (a drop target, a dashed zone).
pub fn dashed_frame_in(paint: &mut Paint, x: f64, y: f64, w: f64, h: f64, color: Color) {
    paint.dashed_rect((x + 0.5) as f32, (y + 0.5) as f32, (w - 1.0) as f32, (h - 1.0) as f32, color);
}

/// Crisp 1-point lines on the pixel grid.
pub fn hline(paint: &mut Paint, x: f64, y: f64, w: f64, color: Color) {
    paint.fill(x as f32, y as f32, w as f32, 1.0, color);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Dir4 {
    Down,
    Up,
    Right,
    Left,
}

/// The kit's chevron scaled with the zoom.
pub fn draw_chevron(paint: &mut Paint, z: f64, at: (f64, f64), direction: Dir4, color: Color) {
    let (cx, cy) = at;
    let (a, b, tip) = match direction {
        Dir4::Down => ((-4.0, -2.0), (4.0, -2.0), (0.0, 2.0)),
        Dir4::Up => ((-4.0, 2.0), (4.0, 2.0), (0.0, -2.0)),
        Dir4::Right => ((-2.0, -4.0), (-2.0, 4.0), (2.0, 0.0)),
        Dir4::Left => ((2.0, -4.0), (2.0, 4.0), (-2.0, 0.0)),
    };
    let pt = |d: (f64, f64)| (cx + d.0 * z, cy + d.1 * z);
    paint.line(p32(pt(a)), p32(pt(tip)), color);
    paint.line(p32(pt(tip)), p32(pt(b)), color);
}

pub fn zone_style(theme: &Theme, kind: ZoneKind) -> Zone {
    match kind {
        ZoneKind::For => zone_for(theme),
        ZoneKind::Fold | ZoneKind::Scan | ZoneKind::State => zone_fold(theme),
        ZoneKind::Sum => zone_sum(theme),
        ZoneKind::Fn => zone_fn(theme),
        ZoneKind::Let => zone_let(theme),
        ZoneKind::Branch => zone_branch(theme, false),
    }
}

/// The tinted rectangle of an expanded zone: fill and a 1-point edge inside the box.
pub fn paint_zone(paint: &mut Paint, taken: bool, theme: &Theme, kind: ZoneKind, rect: (f64, f64, f64, f64)) {
    let (x, y, w, h) = rect;
    let s = if kind == ZoneKind::Branch { zone_branch(theme, taken) } else { zone_style(theme, kind) };
    paint.fill(x as f32, y as f32, w as f32, h as f32, s.fill);
    if s.dashed {
        dashed_frame_in(paint, x, y, w, h, s.edge);
    } else {
        paint.stroke((x + 0.5) as f32, (y + 0.5) as f32, (w - 1.0) as f32, (h - 1.0) as f32, s.edge);
    }
}

pub fn sx(t: &Pane, gx: f64) -> f64 {
    t.x as f64 + t.pan_x + gx * t.zoom
}
pub fn sy(t: &Pane, gy: f64) -> f64 {
    t.y as f64 + t.pan_y + gy * t.zoom
}

/// 1 border + 8 padding
pub const HEAD_PAD: f64 = 9.0;
/// 1 border + 12 padding
pub const LABEL_X: f64 = 13.0;
pub const FIELD_W: f64 = 76.0;
pub const VEC_W: f64 = 112.0;
pub const FIELD_H: f64 = 16.0;
pub const SWITCH_W: f64 = 28.0;
pub const SWITCH_H: f64 = 14.0;
/// the header's ports; a row's are centred on the row
pub const PORT_Y: f64 = 13.0;

pub fn value_x(w: f64) -> f64 {
    w - HEAD_PAD - FIELD_W
}

/// A socket: an 8-point circle (a 1-point ring in the port colour, filled once wired), a record a 10 by
/// 6 pill, a function the same 8-point square turned 45 degrees; a list is a plain circle.
pub fn paint_socket(paint: &mut Paint, theme: &Theme, ty: Option<&Ty>, connected: bool, z: f64, at: (f64, f64)) {
    let (cx, cy) = at;
    let color = port_color(theme, ty);
    let r = 4.0 * z;
    let fill = if connected { color } else { theme.input };
    // a diamond of half-diagonal `half`, one point of height per scan row
    let diamond = |paint: &mut Paint, half: f64, colour: Color| {
        let n = half.ceil() as i64;
        for k in -n..n {
            let hw = half - (k as f64 + 0.5).abs();
            if hw > 0.0 {
                paint.fill((cx - hw) as f32, (cy + k as f64) as f32, (2.0 * hw) as f32, 1.0, colour);
            }
        }
    };
    match ty {
        Some(Ty::Fn(_)) => {
            let half = 4.0 * 2.0f64.sqrt() * z;
            diamond(paint, half, color);
            if !connected {
                diamond(paint, half - 2.0f64.sqrt(), theme.input);
            }
        }
        Some(Ty::Record(_)) => frame_in(paint, cx - 5.0 * z, cy - 3.0 * z, 10.0 * z, 6.0 * z, Some(fill), color),
        _ => {
            paint.circle((cx as f32, cy as f32), r as f32, Some(color), None);
            if !connected {
                paint.circle((cx as f32, cy as f32), (r - 1.0) as f32, Some(theme.input), None);
            }
        }
    }
}

/// Kit text centred in a row: its top is `text_top`, scaled by the zoom through `h`.
pub fn text_in(paint: &mut Paint, size: i64, color: Color, x: f64, y: f64, h: f64, text: &str) {
    let top = f64::from(paint.text_top(Some(size), y as f32, h as f32));
    paint.text_s((x as f32, top as f32), size, color, text);
}

/// A label: upper case, tracked by a point; returns its width.
pub fn cap_in(paint: &mut Paint, size: i64, color: Color, x: f64, y: f64, h: f64, label: &str) -> f64 {
    let label = label.to_ascii_uppercase();
    let tracking = 0.08 * size as f64;
    let top = f64::from(paint.text_top(Some(size), y as f32, h as f32));
    paint.text_with((x as f32, top as f32), Some(size), tracking as f32, Some(color), &label);
    f64::from(paint.text_width(Some(size), &label)) + (label.len() as f64 * tracking)
}

pub fn kind_label(k: ZoneKind) -> &'static str {
    match k {
        ZoneKind::For => "for",
        ZoneKind::Fold => "fold",
        ZoneKind::Scan => "scan",
        ZoneKind::Sum => "sum",
        ZoneKind::Let => "let",
        ZoneKind::Fn => "fn",
        ZoneKind::State => "state",
        ZoneKind::Branch => "branch",
    }
}

fn hexc(s: &str) -> Color {
    color::hex_exn(s).expect("scope_paint: bad colour")
}

pub fn note_colors(theme: &Theme) -> (Color, Color) {
    if theme::dark(theme) { (hexc("#4a4220"), hexc("#f3e6a8")) } else { (hexc("#f3e6a8"), hexc("#4a3f10")) }
}

/// The marks a node or zone carries: live time, loop-invariant, macro, loop order.
pub fn marks(execution: Option<&Execution>, n: &p::Node) -> Vec<String> {
    let mut v: Vec<String> = Vec::new();
    if n.live {
        v.push("t".to_owned());
    }
    if let Some(e) = execution {
        v.push(e.tier.clone());
    }
    if n.macro_.is_some() {
        v.push("\u{25ca}".to_owned());
    }
    if let Some(z) = &n.zone {
        if let Some(o) = &z.order {
            v.push(o.clone());
        }
    }
    if n.expr {
        v.push("expr".to_owned());
    }
    v
}

pub fn marks_width(paint: &mut Paint, size: i64, gap: f64, marks: &[String]) -> f64 {
    marks.iter().fold(0.0, |a, m| a + f64::from(paint.text_width(Some(size), m)) + gap)
}

/// Marks right-aligned at `right`, accent, label size.
pub fn paint_marks(paint: &mut Paint, theme: &Theme, size: i64, gap: f64, right: f64, y: f64, h: f64, marks: &[String]) {
    let mut x = right;
    for m in marks {
        let wd = f64::from(paint.text_width(Some(size), m));
        text_in(paint, size, theme.accent, x - wd, y, h, m);
        x = x - wd - gap;
    }
}

/// DepartureMono has no the glyphs of the study: the pane draws them in their place.
pub fn shown_glyph(g: &str) -> String {
    match g {
        "\u{27f2}" => "\u{21b5}".to_owned(),
        "\u{25c6}" => "\u{25ca}".to_owned(),
        other => other.to_owned(),
    }
}

pub fn strip_parens(s: &str) -> String {
    let n = s.len();
    if n >= 2 && s.starts_with('(') && s.ends_with(')') { s[1..n - 1].to_owned() } else { s.to_owned() }
}

/// A graph input's default, what a wired name reads as live.
pub fn live_value(t: &Pane, name: &str) -> Option<String> {
    let scope = t.scope.as_ref()?;
    scope.inputs.iter().find(|i| i.name == name).and_then(|i| i.default.as_ref().map(lisp::flat))
}

fn float_opt(s: &str) -> Option<f64> {
    (s).parse::<f64>().ok()
}

/// "[0 1 0]" as its numbers.
pub fn number_words(text: &str) -> Option<Vec<String>> {
    let n = text.len();
    if n >= 2 && text.starts_with('[') && text.ends_with(']') {
        let parts: Vec<String> = text[1..n - 1].split(' ').map(|s| s.to_owned()).collect();
        if parts.iter().all(|s| float_opt(s).is_some()) { Some(parts) } else { None }
    } else {
        None
    }
}

pub fn fraction_of(r: &p::Row, text: &str) -> Option<f64> {
    match r.control {
        p::Control::Range(lo, hi) => param::number::fraction((lo, hi), text),
        _ => None,
    }
}

pub fn all_numbers(form: &S, elems: &[flow_syntax::arena::SyntaxId]) -> bool {
    elems.iter().all(|&id| matches!(form.arena().row(id).node, SNode::Num(_)))
}

/// A number or a vector of numbers at a zoom where the live editors are built: they paint themselves.
pub fn live_numbers(z: f64, r: &p::Row) -> bool {
    z >= 0.5
        && match (&r.chip, r.expr.as_ref().map(|x| &x.node)) {
            (p::Chip::Const, Some(SNode::Num(_))) => true,
            (p::Chip::Const, Some(SNode::Vec(elems))) => {
                let form = r.expr.as_ref().unwrap();
                let elems = form.range(*elems);
                let k = elems.len();
                (2..=4).contains(&k) && all_numbers(form, elems)
            }
            _ => false,
        }
}

/// A field painted without the live editor: a value on a line-2 hairline, label-size text; the position
/// line of a ranged number is 2 points above the hairline.
pub fn paint_field(paint: &mut Paint, z: f64, ls: i64, theme: &Theme, x: f64, y: f64, wd: f64, left: bool, fraction: Option<f64>, chevron: bool, color: Color, text: &str) {
    let h = FIELD_H * z;
    paint.fill(x as f32, (y + h - 1.0) as f32, wd as f32, 1.0, theme::edge(theme));
    if let Some(f) = fraction {
        paint.fill(x as f32, (y + h - 3.0) as f32, (wd * f) as f32, 2.0, theme::ink_2(theme));
    }
    let room = wd - (4.0 * z) - if chevron { 10.0 * z } else { 0.0 };
    let text = fitted(paint, ls, room, text);
    let tw = f64::from(paint.text_width(Some(ls), &text));
    let tx = if left || chevron { x + 2.0 * z } else { x + wd - (2.0 * z) - tw };
    text_in(paint, ls, color, tx, y, h, &text);
    if chevron {
        draw_chevron(paint, z, (x + wd - (5.0 * z), y + (h / 2.0) - (1.0 * z)), Dir4::Down, color);
    }
}

pub fn paint_switch(paint: &mut Paint, theme: &Theme, z: f64, x: f64, y: f64, on: bool) {
    frame_in(paint, x, y, SWITCH_W * z, SWITCH_H * z, Some(if on { theme.input } else { theme.track }), theme::border(theme));
    paint.fill((x + (if on { 17.0 } else { 3.0 } * z)) as f32, (y + (3.0 * z)) as f32, (8.0 * z) as f32, (8.0 * z) as f32, if on { theme.accent } else { theme::ink_3(theme) });
}

/// The cells of a vector: each element is a field, its number right-aligned, anything else the
/// expression's text.
pub fn paint_vector(paint: &mut Paint, z: f64, ls: i64, theme: &Theme, right: f64, y: f64, form: &S, elems: &[flow_syntax::arena::SyntaxId]) {
    let k = elems.len();
    let vw = VEC_W * z;
    let gap = 8.0 * z;
    let cw = (vw - (gap * (k as f64 - 1.0))) / k as f64;
    for (c, &id) in elems.iter().enumerate() {
        let (text, left) = match form.arena().row(id).node {
            SNode::Num(s) => (form.name(s).to_owned(), false),
            SNode::Str(s) => (String::from_utf8_lossy(form.bytes(s)).into_owned(), true),
            _ => (lisp::flat(&form.child(id)), true),
        };
        paint_field(paint, z, ls, theme, right - vw + (c as f64 * (cw + gap)), y, cw, left, None, false, theme.foreground, &text);
    }
}

/// A wired row's name: a nested node names its kind.
pub fn wired_name(r: &p::Row, s: &str) -> String {
    if let Some(form) = &r.expr {
        if let Some(head) = flow_syntax::syntax::head(form) {
            if e::nested(s) {
                return head.to_owned();
            }
        }
    }
    s.to_owned()
}

pub const FOLD_GLYPH: &str = "\u{192}";

/// A wired row, right-aligned: `ƒ` when its source can fold into it, `←`, the source and its live value.
/// Returns where the arrow starts, the fold glyph's width, the room the name has, the live value and its width.
pub fn wired_geo(measure: &mut dyn FnMut(i64, &str) -> f64, t: &Pane, z: f64, fs: i64, x: f64, wd: f64, fold: bool, s: &str) -> (f64, f64, f64, Option<String>, f64) {
    let right = x + wd - (HEAD_PAD * z);
    let gap = 6.0 * z;
    let live = live_value(t, s);
    let aw = measure(fs, "\u{2190}");
    let fw = if fold { measure(fs, FOLD_GLYPH) + gap } else { 0.0 };
    let live_w = match &live {
        Some(v) => measure(fs, v) + gap,
        None => 0.0,
    };
    let room = 0.0f64.max(right - x - ((LABEL_X + 40.0) * z) - aw - gap - live_w - fw);
    let left = right - live_w - measure(fs, s).min(room) - gap - aw;
    (left, fw, room, live, live_w)
}

/// Where the name of a row that reads a binding is: its left and its width, in the card's points.
pub fn ref_chip_geo(t: &Pane, z: f64, fs: i64, wd: f64, s: &str) -> (f64, f64) {
    let m = t.measure.clone();
    let (left, _, room, _, _) = wired_geo(&mut |size, s| m(size, s), t, z, fs, 0.0, wd, false, s);
    (left, m(fs, "\u{2190}") + (6.0 * z) + m(fs, s).min(room))
}

/// The value of a row, at the right of it (kit `.nr`). `live`: the live editor of a number or vector of
/// numbers is built over it, so it is not painted here.
pub fn paint_value(paint: &mut Paint, t: &Pane, z: f64, fs: i64, size: Option<i64>, fold: bool, x: f64, y: f64, wd: f64, live: bool, r: &p::Row) {
    let theme = t.theme;
    let ls = match size {
        Some(s) => s,
        None => 4.max(fs - 2),
    };
    let rh = p::ROW_HEIGHT * z;
    let right = x + wd - (HEAD_PAD * z);
    let fx = right - (FIELD_W * z);
    let fy = y + (4.0 * z);
    let dim = theme::ink_3(&theme);
    match &r.chip {
        p::Chip::NoValue => match r.default.as_deref() {
            Some(d @ ("true" | "false")) => paint_switch(paint, &theme, z, right - (SWITCH_W * z), y + (5.0 * z), d == "true"),
            Some(d) => match number_words(d) {
                Some(parts) => {
                    let k = parts.len();
                    let vw = VEC_W * z;
                    let gap = 8.0 * z;
                    let cw = (vw - (gap * (k as f64 - 1.0))) / k as f64;
                    for (c, s) in parts.iter().enumerate() {
                        paint_field(paint, z, ls, &theme, right - vw + (c as f64 * (cw + gap)), fy, cw, false, None, false, dim, s);
                    }
                }
                None => {
                    let numeric = float_opt(d).is_some();
                    paint_field(paint, z, ls, &theme, fx, fy, FIELD_W * z, !numeric && r.control != p::Control::Choice, fraction_of(r, d), r.control == p::Control::Choice, dim, d);
                }
            },
            None => {}
        },
        p::Chip::Const => match r.expr.as_ref().map(|x| &x.node) {
            Some(SNode::Num(text)) => {
                if !live {
                    let text = r.expr.as_ref().unwrap().name(*text);
                    paint_field(paint, z, ls, &theme, fx, fy, FIELD_W * z, false, fraction_of(r, text), false, theme.foreground, text);
                }
            }
            Some(SNode::Vec(elems)) => {
                if !live {
                    let form = r.expr.as_ref().unwrap();
                    paint_vector(paint, z, ls, &theme, right, fy, form, form.range(*elems));
                }
            }
            Some(SNode::Sym(b)) if matches!(r.expr.as_ref().unwrap().name(*b), "true" | "false") => paint_switch(paint, &theme, z, right - (SWITCH_W * z), y + (5.0 * z), r.expr.as_ref().unwrap().name(*b) == "true"),
            Some(_) => {
                let ex = r.expr.as_ref().unwrap();
                let text = match &ex.node {
                    SNode::Str(s) => String::from_utf8_lossy(ex.bytes(*s)).into_owned(),
                    _ => lisp::flat(ex),
                };
                paint_field(paint, z, ls, &theme, fx, fy, FIELD_W * z, r.control != p::Control::Choice, None, r.control == p::Control::Choice, theme.foreground, &text);
            }
            None => {}
        },
        p::Chip::Name(s) => {
            // a nested node is its own card: the row names its kind; a wired row reads `<- source` and the
            // live value at the right
            let s = wired_name(r, s);
            let gap = 6.0 * z;
            let arrow = "\u{2190}";
            let aw = f64::from(paint.text_width(Some(fs), arrow));
            let (left, fw, room, livev, live_w) = wired_geo(&mut |size, s| f64::from(paint.text_width(Some(size), s)), t, z, fs, x, wd, fold, &s);
            let s = fitted(paint, fs, room, &s);
            if fold {
                text_in(paint, fs, theme.accent, left - fw, y, rh, FOLD_GLYPH);
            }
            text_in(paint, fs, if fold { theme::ink_3(&theme) } else { theme.accent }, left, y, rh, arrow);
            text_in(paint, fs, theme::ink_2(&theme), left + aw + gap, y, rh, &s);
            if let Some(v) = livev {
                text_in(paint, fs, theme.foreground, right - live_w + gap, y, rh, &v);
            }
        }
        p::Chip::Inline { glyph, text } => {
            let glyph = shown_glyph(glyph);
            let gw = f64::from(paint.text_width(Some(ls), &glyph)) + (6.0 * z);
            text_in(paint, ls, theme.accent, fx, y, rh, &glyph);
            let fit = fitted(paint, ls, (FIELD_W * z) - gw, text);
            text_in(paint, ls, theme::ink_2(&theme), fx + gw, y, rh, &fit);
        }
    }
}

/// Where a row's label must stop: the control (or the wired value) takes the right of the row.
pub fn value_room(paint: &mut Paint, t: &Pane, z: f64, fs: i64, wd: f64, fold: bool, r: &p::Row) -> f64 {
    let numbers = match (&r.chip, r.expr.as_ref().map(|x| &x.node)) {
        (p::Chip::Const, Some(SNode::Vec(_))) => true,
        (p::Chip::NoValue, _) => r.default.as_deref().and_then(number_words).is_some(),
        _ => false,
    };
    let is_bool_sym = matches!(r.expr.as_ref().map(|x| &x.node), Some(SNode::Sym(b)) if matches!(r.expr.as_ref().unwrap().name(*b), "true" | "false"));
    if r.kind == p::RowKind::Add {
        return wd - ((LABEL_X + HEAD_PAD) * z);
    }
    match &r.chip {
        p::Chip::NoValue if r.default.is_none() => wd - ((LABEL_X + HEAD_PAD) * z),
        p::Chip::Const if is_bool_sym => wd - ((LABEL_X + HEAD_PAD + SWITCH_W + 6.0) * z),
        p::Chip::NoValue if matches!(r.default.as_deref(), Some("true" | "false")) => wd - ((LABEL_X + HEAD_PAD + SWITCH_W + 6.0) * z),
        p::Chip::Name(s) => {
            let s = wired_name(r, s);
            let live = live_value(t, &s);
            let aw = f64::from(paint.text_width(Some(fs), "\u{2190}"));
            let sw = f64::from(paint.text_width(Some(fs), &s)).min(wd * 0.4);
            let lw = match &live {
                Some(v) => f64::from(paint.text_width(Some(fs), v)) + (6.0 * z),
                None => 0.0,
            };
            wd - ((LABEL_X + HEAD_PAD) * z) - aw - sw - lw - (12.0 * z) - if fold { 14.0 * z } else { 0.0 }
        }
        _ => wd - ((LABEL_X + HEAD_PAD) * z) - ((if numbers { VEC_W } else { FIELD_W }) * z) - (6.0 * z),
    }
}

pub fn paint_rows(paint: &mut Paint, t: &Pane, z: f64, fs: i64, x: f64, y: f64, wd: f64, carry: bool, n: &p::Node, lines: &[p::Line], selected_row: Option<usize>) {
    let theme = t.theme;
    let ls = 4.max(fs - 2);
    let rh = p::ROW_HEIGHT * z;
    for (k, line) in lines.iter().enumerate() {
        let ry = y + (k as f64 * rh);
        match line {
            p::Line::Folder(_, name) => {
                // a folder's label row: its name in the foreground, 8 points in
                cap_in(paint, ls, theme.foreground, x + (9.0 * z), ry, rh, name);
            }
            p::Line::Row(i, r) => {
                let fold = t.folds.contains_key(&(n.path.clone(), *i));
                let target = carry && selected_row == Some(*i);
                if selected_row == Some(*i) {
                    paint.fill((x + 1.0) as f32, ry as f32, (wd - 2.0) as f32, rh as f32, if target { theme::hover_fill(&theme) } else { theme::faint_border(&theme) });
                }
                // a carried value over a row: the row takes the hover fill, an ink label and an accent underline
                if target {
                    hline(paint, x + (value_x(wd) * z), ry + (19.0 * z), FIELD_W * z, theme.accent);
                }
                let add = r.kind == p::RowKind::Add;
                let color = if target {
                    theme.foreground
                } else {
                    match r.kind {
                        p::RowKind::Add => theme::ink_2(&theme),
                        p::RowKind::Binder | p::RowKind::Hole => theme.accent,
                        _ => theme::ink_2(&theme),
                    }
                };
                let size = if add { ls } else { fs };
                let room = 0.0f64.max(value_room(paint, t, z, fs, wd, fold, r));
                let label = fitted(paint, size, room, &r.label);
                text_in(paint, size, color, x + (LABEL_X * z), ry, rh, &label);
                if r.socket {
                    paint_socket(paint, &theme, r.ty.as_ref(), wired(r), z, (x, ry + (12.0 * z)));
                }
                if !add {
                    paint_value(paint, t, z, fs, None, fold, x, ry, wd, live_numbers(z, r), r);
                }
            }
        }
    }
}

pub fn paint_outputs(paint: &mut Paint, t: &Pane, z: f64, fs: i64, x: f64, y: f64, wd: f64, n: &p::Node, nlines: usize) {
    let theme = t.theme;
    let ls = 4.max(fs - 2);
    let rh = p::ROW_HEIGHT * z;
    for (j, (name, ty)) in n.outputs.iter().enumerate() {
        let ry = y + ((nlines + j) as f64 * rh);
        text_in(paint, fs, colors(t).record, x + (LABEL_X * z), ry, rh, &format!("\u{2192} {}", name));
        let label = ty::to_string(ty);
        let lw = f64::from(paint.text_width(Some(ls), &label));
        text_in(paint, ls, theme::ink_2(&theme), x + wd - (HEAD_PAD * z) - lw, ry, rh, &label);
        paint_socket(paint, &theme, Some(ty), true, z, (x + wd, ry + (12.0 * z)));
    }
}

/// Rail rows of a zone: name, role, and what feeds it. Under an expanded zone's label row they are the
/// accumulators, further variables and parameters; a collapsed zone's card lists every row.
pub fn paint_rail(paint: &mut Paint, t: &Pane, z: f64, fs: i64, x: f64, y: f64, wd: f64, expanded: bool, rail: &[p::RailRow]) {
    let theme = t.theme;
    let palette = colors(t);
    let ls = 4.max(fs - 2);
    let rh = p::ROW_HEIGHT * z;
    for (i, r) in rail.iter().enumerate() {
        let ry = y + (i as f64 * rh);
        let color = match r.role {
            p::Role::Var => theme.accent,
            p::Role::Acc => palette.vec3,
            p::Role::Param => palette.fn_,
            p::Role::Capture => theme::muted(&theme),
        };
        // in an open zone the names stand right of the begin bar, whose ports are level with them
        let nx = x + ((if expanded { 32.0 } else { 13.0 }) * z);
        let nm = fitted(paint, fs, 56.0 * z, &r.name);
        text_in(paint, fs, color, nx, ry, rh, &nm);
        let role = match r.role {
            p::Role::Var => "in",
            p::Role::Acc => "acc",
            p::Role::Param => "param",
            p::Role::Capture => "from",
        };
        text_in(paint, ls, theme::ink_3(&theme), nx + (58.0 * z), ry, rh, role);
        if let Some(ex) = &r.expr {
            let sym = matches!(ex.node, SNode::Sym(_));
            let shown = match &ex.node {
                SNode::Sym(s) => ex.name(*s).to_owned(),
                _ => lisp::flat(ex),
            };
            let exx = nx + (79.0 * z);
            let room = 0.0f64.max(x + wd - ((if expanded { 24.0 } else { 8.0 }) * z) - exx);
            if sym {
                text_in(paint, ls, theme.accent, exx, ry, rh, "\u{2190}");
                let aw = f64::from(paint.text_width(Some(ls), "\u{2190} "));
                let f = fitted(paint, ls, 0.0f64.max(room - aw), &shown);
                text_in(paint, ls, theme::ink_2(&theme), exx + aw, ry, rh, &f);
            } else {
                let f = fitted(paint, ls, room, &shown);
                text_in(paint, ls, theme::ink_2(&theme), exx, ry, rh, &f);
            }
        }
        if r.key.is_some() || r.role == p::Role::Capture {
            paint_socket(paint, &theme, r.ty.as_ref(), r.role == p::Role::Capture || r.expr.is_some(), z, (x, ry + (12.0 * z)));
        }
    }
}

/// The zone's label row (kit `.zone` label): `FOR` in accent, the binder in ink, `in ...` in ink-2.
pub fn zone_binder(t: &Pane, n: &p::Node, zn: &p::Zone) -> (String, Option<String>) {
    match p::label_row(zn) {
        None if zn.kind == ZoneKind::Fn && n.path.last().map_or(false, |l| l.contains('#')) => {
            // a function written in a row names its owner: `map -> positions`, not `fn`
            let leaf = n.path.last().unwrap();
            let mut holder = n.path[..n.path.len() - 1].to_vec();
            holder.push(leaf[..leaf.find('#').unwrap()].into());
            match st::node_of(t, &holder) {
                Some(h) => (format!("{} -> {}", h.head, p::title(&h)), None),
                None => (p::title(n), None),
            }
        }
        Some(r) => match &r.expr {
            Some(ex) => (r.name.clone(), Some(strip_parens(&lisp::flat(ex)))),
            None => (r.name.clone(), None),
        },
        None => (p::title(n), None),
    }
}

pub fn paint_zone_label(paint: &mut Paint, t: &Pane, z: f64, fs: i64, x: f64, y: f64, wd: f64, execution: Option<&Execution>, n: &p::Node, zn: &p::Zone) {
    let theme = t.theme;
    let ls = 4.max(fs - 2);
    let hh = p::HEAD_HEIGHT * z;
    let gap = 8.0 * z;
    let label = format!("{}{}", kind_label(zn.kind), execution.map_or(String::new(), |e| format!(" \u{b7} {}", e.tier)));
    let cw = cap_in(paint, ls, theme.accent, x, y, hh, &label);
    let (name, in_) = zone_binder(t, n, zn);
    let nx = x + cw + gap;
    let room = 0.0f64.max(x + wd - (9.0 * z) - nx);
    let name = fitted(paint, fs, room, &name);
    text_in(paint, fs, theme.foreground, nx, y, hh, &name);
    if let Some(ex) = in_ {
        let ix = nx + f64::from(paint.text_width(Some(fs), &name)) + gap;
        let input = format!("in {}", ex);
        let f = fitted(paint, fs, 0.0f64.max(x + wd - (9.0 * z) - ix), &input);
        text_in(paint, fs, theme::ink_2(&theme), ix, y, hh, &f);
    }
}

/// The iteration selector at the zone's top right: `< 2 / 5 >` on the sheet fill, 9 points in from the
/// zone's edge and 3 down, 20 high; tile-relative points.
pub fn selector_geo(measure: &mut dyn FnMut(i64, &str) -> f64, z: f64, fs: i64, wd: f64, count: i64, probe: i64) -> (f64, f64, f64, f64, String) {
    let read = if count > 0 { format!("{} / {}", probe + 1, count) } else { "no runs".to_owned() };
    let tw = measure(fs, &read);
    let chev = 6.0 * z;
    let gap = 12.0 * z;
    let total = chev + gap + tw + gap + chev;
    (wd - (HEAD_PAD * z) - total, 3.0 * z, total, 20.0 * z, read)
}

pub fn paint_selector(paint: &mut Paint, t: &Pane, z: f64, fs: i64, count: i64, probe: i64, at: (f64, f64, f64)) {
    let theme = t.theme;
    let (x, y, wd) = at;
    let (sx0, sy0, sw, sh, read) = selector_geo(&mut |size, s| f64::from(paint.text_width(Some(size), s)), z, fs, wd, count, probe);
    let sx0 = x + sx0;
    let sy0 = y + sy0;
    paint.fill(sx0 as f32, sy0 as f32, sw as f32, sh as f32, theme.input);
    // the sheet's `.seg` children are ink-3
    let ink = theme::ink_3(&theme);
    draw_chevron(paint, z, (sx0 + (3.0 * z), sy0 + (sh / 2.0)), Dir4::Left, ink);
    draw_chevron(paint, z, (sx0 + sw - (3.0 * z), sy0 + (sh / 2.0)), Dir4::Right, ink);
    text_in(paint, fs, ink, sx0 + (18.0 * z), sy0, sh, &read);
}

#[derive(Clone)]
pub struct HeaderOpts {
    pub execution: Option<Execution>,
    pub lens_open: Option<bool>,
    pub failed: Option<String>,
    pub head: Option<p::Row>,
    pub shown: Level,
}

impl Default for HeaderOpts {
    fn default() -> HeaderOpts {
        HeaderOpts { execution: None, lens_open: None, failed: None, head: None, shown: Level::Card }
    }
}

/// The card's header: the in-
/// the view flag, the marks and the kind (a failed node says its code instead).
pub fn paint_header(paint: &mut Paint, t: &Pane, z: f64, fs: i64, x: f64, y: f64, wd: f64, n: &p::Node, o: &HeaderOpts) {
    let theme = t.theme;
    let ls = 4.max(fs - 2);
    let hh = p::HEAD_HEIGHT * z;
    let viewed = t.display.as_ref() == Some(&n.path);
    let ink_3 = theme::ink_3(&theme);
    let invalid = theme::invalid();
    let failed = o.failed.is_some();
    let square = if failed {
        invalid
    } else if n.bypass {
        ink_3
    } else if viewed {
        theme.foreground
    } else {
        ty_color(t, Some(&n.ty))
    };
    paint.fill((x + (HEAD_PAD * z)) as f32, (y + (8.0 * z)) as f32, (8.0 * z) as f32, (8.0 * z) as f32, square);
    if let Some(r) = &o.head {
        paint_socket(paint, &theme, r.ty.as_ref(), wired(r), z, (x + (1.0 * z), y + (PORT_Y * z)));
    }
    let name_x = x + (23.0 * z);
    let mut right = x + wd - (HEAD_PAD * z);
    if let Some(open_) = o.lens_open {
        draw_chevron(paint, z, (x + wd - (14.0 * z), y + (12.0 * z)), if open_ { Dir4::Down } else { Dir4::Right }, theme::muted(&theme));
        right = x + wd - (24.0 * z);
    }
    // the displayed node: the 12-point view flag, a line-3 frame and the accent square inside
    if viewed {
        let fx = right - (12.0 * z);
        let fy = y + (6.0 * z);
        frame_in(paint, fx, fy, 12.0 * z, 12.0 * z, Some(theme.input), theme::border(&theme));
        paint.fill((fx + (3.0 * z)) as f32, (fy + (3.0 * z)) as f32, (6.0 * z) as f32, (6.0 * z) as f32, theme.accent);
        right = fx - (6.0 * z);
    }
    let m = marks(o.execution.as_ref(), n);
    let gap = 5.0 * z;
    paint_marks(paint, &theme, ls, gap, right, y, hh, &m);
    right = right - marks_width(paint, ls, gap, &m) - if m.is_empty() { 0.0 } else { 1.0 * z };
    let title = p::title(n);
    let name_color = if failed {
        invalid
    } else if n.bypass {
        ink_3
    } else if n.synthetic || p::anonymous(n) {
        theme::muted(&theme)
    } else {
        theme.foreground
    };
    let name = fitted(paint, fs, 0.0f64.max(right - name_x), &title);
    text_in(paint, fs, name_color, name_x, y, hh, &name);
    let mut name_end = name_x + f64::from(paint.text_width(Some(fs), &name));
    // a chip: how many rows have something written, in the kind's place at the right
    if o.shown == Level::Chip && p::set_count(n) > 0 {
        let text = format!("+{}", p::set_count(n));
        text_in(paint, ls, theme::ink_2(&theme), name_end + (9.0 * z), y, hh, &text);
        name_end = name_end + (9.0 * z) + f64::from(paint.text_width(Some(ls), &text));
    }
    // where the name and the kind meet, the kind yields; a value card has its field there instead
    let kind_text: Option<String> = match &o.failed {
        Some(code) => Some(code.clone()),
        None => match t.saved.imported(&n.path) {
            Some(file) => Some(file),
            None => {
                if n.zone.is_none() && n.head != n.name && !p::value_card(n) {
                    Some(n.head.clone())
                } else {
                    None
                }
            }
        },
    };
    if let Some(text) = kind_text {
        let room = right - (name_end + (6.0 * z));
        if room >= 3.0 * f64::from(paint.text_width(Some(ls), "m")) {
            let kind = fitted(paint, ls, room, &text);
            let kw = f64::from(paint.text_width(Some(ls), &kind));
            text_in(paint, ls, theme::ink_2(&theme), right - kw, y, hh, &kind);
        }
    }
}

/// The footer row of a node card or a collapsed zone: the value at the probe, a sparkline across the
/// innermost zone, the tags; label-size ink-2 over a line-1 hairline.
pub fn paint_footer(paint: &mut Paint, t: &Pane, z: f64, fs: i64, f: &Footer, ty: &Ty, pad: f64, rect: (f64, f64, f64, f64)) {
    let theme = t.theme;
    let (x, y, wd, h) = rect;
    let ls = 4.max(fs - 2);
    let fh = p::FOOT_HEIGHT * z;
    let fy = y + h - (pad * z) - fh;
    paint.fill((x + 1.0) as f32, fy as f32, (wd - 2.0) as f32, 1.0, theme::faint_border(&theme));
    let ink_2 = theme::ink_2(&theme);
    let ty0 = fy + 1.0;
    let th = fh - 1.0;
    let mut right = x + wd - (HEAD_PAD * z);
    let put_right = |paint: &mut Paint, color: Color, s: &str, right: &mut f64| {
        let tw = f64::from(paint.text_width(Some(ls), s));
        text_in(paint, ls, color, *right - tw, ty0, th, s);
        *right = *right - tw - (6.0 * z);
    };
    if f.live {
        put_right(paint, theme.accent, "t", &mut right);
    }
    if let Some(ex) = &f.execution {
        put_right(paint, ink_2, &ex.tier, &mut right);
    }
    let mut tags_v: Vec<String> = Vec::new();
    if let Some(b) = &f.branch {
        tags_v.push(b.clone());
    }
    if let Some(k) = &f.kept {
        tags_v.push(k.clone());
    }
    if let Some(r) = f.runs {
        tags_v.push(format!("\u{d7}{}", r));
    }
    if let Some(ex) = &f.execution {
        if let Some(seconds) = ex.seconds {
            tags_v.push(format!("{:.3} ms", seconds * 1000.0));
        }
    }
    let tags = tags_v.join(" \u{b7} ");
    if !tags.is_empty() {
        let fit = fitted(paint, ls, 0.0f64.max(right - x) * 0.5, &tags);
        put_right(paint, ink_2, &fit, &mut right);
    }
    let left = x + (LABEL_X * z);
    if let Some((values, at)) = &f.spark {
        let sw = 36.0 * z;
        let sh = 10.0 * z;
        let sx0 = (left + (30.0 * z)).max(right - sw);
        let lo = values.iter().fold(f64::INFINITY, |a, b| a.min(*b));
        let hi = values.iter().fold(f64::NEG_INFINITY, |a, b| a.max(*b));
        let n = values.len();
        let px = |i: usize| sx0 + (i as f64 / (n as f64 - 1.0) * sw);
        // flat runs sit on a whole point plus a half, so a 1-point line is not blurred over two rows
        let py = |v: f64| -> f64 {
            (if hi == lo { fy + (7.0 * z) + (sh / 2.0) } else { fy + (7.0 * z) + sh - ((v - lo) / (hi - lo) * sh) }).floor() + 0.5
        };
        // at most 16 segments: a 240-iteration series draws as many as a 20-iteration one
        let step = 1.max(n / 16);
        let mut i = 0;
        while i + 1 < n {
            let j = (n - 1).min(i + step);
            paint.line_w(((px(i)) as f32, (py(values[i])) as f32), ((px(j)) as f32, (py(values[j])) as f32), 1.0, ty_color(t, Some(ty)));
            i = j;
        }
        let a = *at as usize;
        paint.fill((px(a) - (1.5 * z)) as f32, (py(values[a]) - (1.5 * z)) as f32, (3.0 * z) as f32, (3.0 * z) as f32, theme.accent);
        right = sx0 - (6.0 * z);
    }
    let fit = fitted(paint, ls, 0.0f64.max(right - left), &f.value);
    text_in(paint, ls, ink_2, left, ty0, th, &fit);
}

/// The expansion panel under a macro call, in graph units relative to its top: step buttons on the first
/// row ("call", 1, 2, ...), the printed step, and the replace button with the reading on the last row.
pub fn lens_button_box(len: i64, i: i64) -> ((f64, f64), (f64, f64)) {
    if i == 0 {
        ((8.0, 2.0), (36.0, 20.0))
    } else if i >= len {
        // the template
        ((8.0 + 40.0 + (0.max(len - 1) as f64 * 28.0) + 4.0, 2.0), (64.0, 20.0))
    } else {
        ((8.0 + 40.0 + ((i - 1) as f64 * 28.0), 2.0), (24.0, 20.0))
    }
}

/// a control is 20 high
pub fn lens_replace_box(lh: f64) -> ((f64, f64), (f64, f64)) {
    ((8.0, lh - p::ROW_HEIGHT + 2.0), (196.0, 20.0))
}

/// The panel's top inside a card of height `h`: above the padding.
pub fn lens_top(h: f64, lh: f64) -> f64 {
    h - lh - p::CARD_PAD
}

pub fn paint_lens(paint: &mut Paint, t: &Pane, z: f64, fs: i64, l: &p::Lens, step: i64, at: (f64, f64, f64), lh: f64) {
    let theme = t.theme;
    let (x, y, wd) = at;
    let ls = 4.max(fs - 2);
    paint.fill((x + 1.0) as f32, y as f32, (wd - 2.0) as f32, 1.0, theme::faint_border(&theme));
    let len = l.steps.len() as i64;
    for i in 0..=len {
        let ((bx, by), (bw, bh)) = lens_button_box(len, i);
        let on = i == step;
        if on {
            paint.fill((x + ((bx + 2.0) * z)) as f32, (y + ((by + bh) * z) - 1.0) as f32, ((bw - 4.0) * z) as f32, 1.0, theme.foreground);
        }
        let label = if i == 0 {
            "call".to_owned()
        } else if i == len {
            "Template".to_owned()
        } else {
            i.to_string()
        };
        let tw = f64::from(paint.text_width(Some(ls), &label));
        text_in(paint, ls, if on { theme.foreground } else { theme::ink_3(&theme) }, x + ((bx + (bw / 2.0)) * z) - (tw / 2.0), y + (by * z), bh * z, &label);
    }
    let text: String = if step >= len {
        if l.template.is_empty() { "(no definition)".to_owned() } else { l.template.clone() }
    } else {
        l.steps[0.max(step) as usize].clone()
    };
    let lines: Vec<&str> = text.split('\n').collect();
    for (k, line) in lines.iter().enumerate() {
        if k < 16 {
            let src = if k == 15 && lines.len() > 16 { format!("{} \u{2026}", line) } else { (*line).to_owned() };
            let fit = fitted(paint, fs, wd - (20.0 * z), &src);
            paint.text_s(((x + (10.0 * z)) as f32, (y + ((p::ROW_HEIGHT + 4.0 + (k as f64 * 20.0)) * z)) as f32), fs, theme.foreground, &fit);
        }
    }
    let ((rx, ry), (rw, rh)) = lens_replace_box(lh);
    frame_in(paint, x + (rx * z), y + (ry * z), rw * z, rh * z, None, theme::border(&theme));
    text_in(paint, ls, theme.foreground, x + ((rx + 8.0) * z), y + (ry * z), rh * z, "Replace call with expansion");
    let reading = if step >= len {
        "the macro's template".to_owned()
    } else {
        match &l.error {
            Some(message) if step >= len - 1 && step > 0 => message.clone(),
            _ => {
                if step == 0 {
                    "as written".to_owned()
                } else {
                    format!("after {} expansion step{}", step, if step > 1 { "s" } else { "" })
                }
            }
        }
    };
    let tw = f64::from(paint.text_width(Some(ls), &reading));
    let fit = fitted(paint, ls, wd - ((rx + rw + 24.0) * z), &reading);
    text_in(paint, ls, theme::muted(&theme), x + wd - (HEAD_PAD * z) - tw, y + (ry * z), rh * z, &fit);
}

/// A card's hairline: line-2, line-3 when selected, composed over the card's own fill.
pub fn card_edge(t: &Pane, fill: Color, selected: bool) -> Color {
    color::blend(t.theme.foreground, fill, if selected { 0.70 } else { 0.85 })
}

/// The card: its fill and a 1-point border inside its box (a stroke is centred on its rectangle).
pub fn paint_card(paint: &mut Paint, x: f64, y: f64, wd: f64, h: f64, fill: Color, edge: Color) {
    paint.fill(x as f32, y as f32, wd as f32, h as f32, fill);
    paint.stroke((x + 0.5) as f32, (y + 0.5) as f32, (wd - 1.0) as f32, (h - 1.0) as f32, edge);
}

/// A point: the 14-point disc in the node's colour with its name beside it (a compound is a ring with a
/// dot), brackets when selected; the displayed one wears its flag below the disc.
pub fn paint_point(paint: &mut Paint, t: &Pane, z: f64, fs: i64, pl: &Placed, failed: Option<&str>, selected: bool, rect: (f64, f64, f64, f64)) {
    let theme = t.theme;
    let (x, y, _, _) = rect;
    let c = (x + (7.0 * z), y + (7.0 * z));
    let (ty, macro_, bypass, title, viewed) = match &pl.item {
        p::Item::Item(n) => (n.ty.clone(), n.macro_.is_some(), n.bypass, p::title(n), t.display.as_ref() == Some(&n.path)),
        p::Item::Input(i) => (i.ty.clone(), false, false, i.name.clone(), false),
        p::Item::Return => (ty::geometry(), false, false, "return".to_owned(), false),
    };
    let color = if failed.is_some() { theme::invalid() } else { ty_color(t, Some(&ty)) };
    if macro_ {
        paint.circle(p32(c), (7.0 * z) as f32, Some(theme.panel), Some(color));
        paint.circle(p32(c), (2.0 * z) as f32, Some(color), None);
    } else {
        paint.circle(p32(c), (7.0 * z) as f32, Some(color), None);
    }
    let name_color = if failed.is_some() {
        theme::invalid()
    } else if bypass {
        theme::ink_3(&theme)
    } else {
        theme.foreground
    };
    text_in(paint, fs, name_color, x + (22.0 * z), y + (7.0 * z) - (12.0 * z), 24.0 * z, &title);
    if viewed {
        let fy = y + (21.0 * z);
        frame_in(paint, x, fy, 12.0 * z, 12.0 * z, Some(theme.input), theme::border(&theme));
        paint.fill((x + (3.0 * z)) as f32, (fy + (3.0 * z)) as f32, (6.0 * z) as f32, (6.0 * z) as f32, theme.accent);
    }
    if selected {
        paint.brackets(x as f32, y as f32, (14.0 * z) as f32, (14.0 * z) as f32, 5.0, 6.0, 2.0, theme.accent);
    }
}

/// The execution report of a node at the zones' pinned iterations.
fn execution_of(t: &Pane, n: &p::Node) -> Option<Execution> {
    let records = t.records.as_ref()?;
    let chain: Vec<i64> = t.chains.get(&n.path).map(|c| c.iter().map(|x| t.saved.probe(x)).collect()).unwrap_or_default();
    let r = records.borrow().execution(&n.path, &chain);
    r
}

#[derive(Clone)]
pub struct NodeOpts {
    pub footer: Option<Footer>,
    pub lens_step: Option<i64>,
    pub hovered: bool,
    pub carry: bool,
    pub out_wired: bool,
    pub shown: Level,
    pub failed: Option<String>,
}

impl Default for NodeOpts {
    fn default() -> NodeOpts {
        NodeOpts { footer: None, lens_step: None, hovered: false, carry: false, out_wired: true, shown: Level::Card, failed: None }
    }
}

pub fn paint_node(paint: &mut Paint, t: &Pane, z: f64, fs: i64, o: &NodeOpts, pl: &Placed, n: &p::Node, selected: bool, row_hover: Option<usize>, rect: (f64, f64, f64, f64)) {
    let (x, y, wd, h) = rect;
    let shown = o.shown;
    if shown == Level::Point {
        paint_point(paint, t, z, fs, pl, o.failed.as_deref(), selected, rect);
        return;
    }
    let theme = t.theme;
    let z_ = n.zone.as_ref();
    let execution = match execution_of(t, n) {
        Some(e) => Some(e),
        None => o.footer.as_ref().and_then(|f| f.execution.clone()),
    };
    let ls = 4.max(fs - 2);
    let stacked = match z_ {
        Some(_) => pl.collapsed,
        None => false,
    };
    if stacked {
        for d in [6.0, 3.0] {
            paint_card(paint, x + (d * z), y + (d * z), wd, h, theme.input, card_edge(t, theme.input, false));
        }
    }
    // a chip is the header of a card that is laid out larger: the zoom, not the document, shrank it
    let h = if shown == Level::Chip && z_.is_none() { p::HEAD_HEIGHT * z } else { h };
    let fill = if o.hovered && !selected { theme::hover_fill(&theme) } else { theme.input };
    let edge = if o.failed.is_some() { theme::invalid() } else { card_edge(t, fill, selected) };
    if n.expr {
        // an expression written in its reader's input: a dashed card, its `expr` mark in the header
        paint.fill(x as f32, y as f32, wd as f32, h as f32, fill);
        dashed_frame_in(paint, x, y, wd, h, edge);
    } else {
        paint_card(paint, x, y, wd, h, fill, edge);
    }
    let lens = match (&n.lens, o.lens_step) {
        (Some(l), Some(step)) => Some((l, step)),
        _ => None,
    };
    let lh = match lens {
        Some((l, step)) => p::lens_height(l, step),
        None => 0.0,
    };
    let head = n.rows.iter().find(|r| r.head).cloned();
    match z_ {
        Some(zn) => {
            // a collapsed zone: its kind as a label, the card's name, the toggle chevron
            paint_zone_label(paint, t, z, fs, x + (HEAD_PAD * z), y, wd - (HEAD_PAD * z), execution.as_ref(), n, zn);
        }
        None => {
            let ho = HeaderOpts {
                execution: execution.clone(),
                lens_open: if n.lens.is_some() && shown != Level::Chip { Some(lens.is_some()) } else { None },
                failed: o.failed.clone(),
                head,
                shown,
            };
            paint_header(paint, t, z, fs, x, y, wd, n, &ho);
        }
    }
    // the displayed result at the end of its chain has no out-port: the view flag takes the place
    if o.out_wired || t.display.as_ref() != Some(&n.path) {
        paint_socket(paint, &theme, Some(&n.ty), o.out_wired, z, (x + wd - (1.0 * z), y + (PORT_Y * z)));
    }
    // a ghost value: where the footer is not drawn, the value at the pinned iteration beside the out-port
    if let Some(f) = &o.footer {
        if shown != Level::Full && z_.is_none() && o.out_wired {
            let yields = t.geo.wires.iter().any(|wi: &Wire| {
                wi.from.as_ref() == Some(&n.path)
                    && match &wi.into {
                        Some(zone) => zone.len() < n.path.len() && n.path[..zone.len()] == zone[..],
                        None => false,
                    }
            });
            // not beside a card that yields to its zone's end bar: the bar's port is there
            if !yields {
                let ink_3 = theme::ink_3(&theme);
                let fit = fitted(paint, ls, 72.0 * z, &f.value);
                text_in(paint, ls, ink_3, x + wd + (8.0 * z), y + ((PORT_Y - 8.0) * z), 16.0 * z, &fit);
            }
        }
    }
    // a literal binding is the header alone: its value in the field at the right
    if n.rows.len() == 1 && p::value_card(n) {
        let r = &n.rows[0];
        paint_value(paint, t, z, fs, Some(fs), false, x, y, wd, live_numbers(z, r), r);
    }
    let body_y = y + (p::BODY_TOP * z);
    if shown != Level::Chip {
        if let Some(note) = &n.note {
            let (bg, ink) = note_colors(&theme);
            paint.fill((x + 1.0) as f32, body_y as f32, (wd - 2.0) as f32, (p::ROW_HEIGHT * z) as f32, bg);
            let first = match note.find('\n') {
                Some(i) => &note[..i],
                None => note.as_str(),
            };
            let fit = fitted(paint, ls, wd - ((LABEL_X + HEAD_PAD) * z), first);
            text_in(paint, ls, ink, x + (LABEL_X * z), body_y, p::ROW_HEIGHT * z, &fit);
        }
        let rows_y = body_y + if n.note.is_some() { p::ROW_HEIGHT * z } else { 0.0 };
        match z_ {
            None => {
                paint_rows(paint, t, z, fs, x, rows_y, wd, o.carry, n, &pl.lines, row_hover);
                if n.head == "list" || n.head == "str" {
                    for (k, line) in pl.lines.iter().enumerate() {
                        if let p::Line::Row(_, r) = line {
                            if let (p::RowKind::Rest, e::ArgKey::Pos(j)) = (r.kind, &r.key) {
                                if *j >= 1 {
                                    text_in(paint, fs, theme.accent, x + wd - (20.0 * z), rows_y + (k as f64 * p::ROW_HEIGHT * z), p::ROW_HEIGHT * z, "\u{2191}");
                                }
                            }
                        }
                    }
                }
                paint_outputs(paint, t, z, fs, x, rows_y, wd, n, pl.lines.len());
            }
            Some(zn) if pl.collapsed => paint_rail(paint, t, z, fs, x, body_y, wd, false, &zn.rail),
            Some(_) => {}
        }
        if shown == Level::Full {
            if let Some(f) = &o.footer {
                paint_footer(paint, t, z, fs, f, &n.ty, p::CARD_PAD + lh, (x, y, wd, h));
            }
        }
        if let Some((l, step)) = lens {
            paint_lens(paint, t, z, fs, l, step, (x, y + h - ((lh + p::CARD_PAD) * z), wd), lh);
        }
    }
    if selected {
        paint.brackets(x as f32, y as f32, wd as f32, h as f32, 3.0, 8.0, 2.0, theme.accent);
    }
}

/// Does anything inside the zone read this name (a loop variable shows its port only then)?
pub fn zone_uses(zn: &p::Zone, names: &[String]) -> bool {
    let roots: Vec<&str> = names.iter().map(|n| root_name(n)).collect();
    let reads = |name: &str| roots.contains(&root_name(name));
    fn scope(s: &p::Scope, reads: &dyn Fn(&str) -> bool) -> bool {
        s.nodes.iter().any(|n| {
            n.rows.iter().any(|r| p::sources(r).iter().any(|x| reads(x)))
                || match &n.zone {
                    Some(z) => z.rail.iter().any(|r| r.expr.as_ref().map_or(false, |ex| e::free_names(ex).iter().any(|x| reads(x)))) || scope(&z.scope, reads),
                    None => false,
                }
        }) || matches!(&s.result, p::ScopeResult::Link(l) if reads(l))
    }
    scope(&zn.scope, &reads)
}

/// The expanded zone's label row, selector, rail rows and ports, over its tint; the cards inside are
/// wired directly across its edge.
pub fn paint_zone_frame(paint: &mut Paint, t: &Pane, z: f64, fs: i64, footer: Option<&Footer>, n: &p::Node, zn: &p::Zone, selected: bool, out_wired: bool, rect: (f64, f64, f64, f64), probe: i64, count: i64) {
    let theme = t.theme;
    let (x, y, wd, h) = rect;
    let execution = execution_of(t, n);
    // the zone's own footer under its body
    let h = match footer {
        Some(f) => {
            paint_footer(paint, t, z, fs, f, &n.ty, 0.0, (x, y, wd, h));
            h - (p::FOOT_HEIGHT * z)
        }
        None => h,
    };
    // the label clears the collection's in-port when a name feeds it
    let plain = zn.kind == ZoneKind::For;
    let label = p::label_row(zn);
    let in_wired = !plain
        && match &label {
            Some(r) if r.key.is_some() => r.expr.as_ref().map_or(false, |ex| !e::free_names(ex).is_empty()),
            _ => false,
        };
    let ind = if in_wired { 8.0 * z } else { 0.0 };
    paint_zone_label(paint, t, z, fs, x + ind, y, wd - ind, execution.as_ref(), n, zn);
    // a plain `for` has no ports of its own on its edge: the label reads as the sheet's, and the yielded
    // card wires straight to its consumer
    if !plain {
        paint_socket(paint, &theme, Some(&n.ty), out_wired, z, (x + wd - (1.0 * z), y + (PORT_Y * z)));
    }
    if let (Some(r), Some(li)) = (&label, w::label_index(zn)) {
        // the collection's in-port when a name feeds it; the variable's out-port once something reads it
        if let Some(ex) = &r.expr {
            if !plain && r.key.is_some() && !e::free_names(ex).is_empty() {
                paint_socket(paint, &theme, r.ty.as_ref(), true, z, (x + (1.0 * z), y + (PORT_Y * z)));
            }
        }
        if zone_uses(zn, &r.names) {
            let (bx, by) = w::begin_port(n, zn, li);
            paint_socket(paint, &theme, r.ty.as_ref(), true, z, (x + (bx * z), y + (by * z)));
        }
    }
    for ri in w::extra_indices(zn) {
        let r = &zn.rail[ri];
        if r.role != p::Role::Capture {
            let (bx, by) = w::begin_port(n, zn, ri);
            paint_socket(paint, &theme, r.ty.as_ref(), true, z, (x + (bx * z), y + (by * z)));
        }
    }
    // the begin and end bars: a hairline inside each edge from the first variable down; the label's
    // variable is named beside its port in the air under the rails, the yield beside the end port
    if !plain {
        let top = y + ((p::HEAD_HEIGHT + 12.0) * z);
        let bh = y + h - top - (4.0 * z);
        if bh > 0.0 {
            let edge = zone_style(&theme, zn.kind).edge;
            let ls = 4.max(fs - 2);
            let ink = theme::ink_3(&theme);
            paint.fill((x + (20.0 * z)) as f32, top as f32, 1.0, bh as f32, edge);
            paint.fill((x + wd - (21.0 * z)) as f32, top as f32, 1.0, bh as f32, edge);
            let air = y + ((p::rail_top(n) - 22.0) * z);
            if let Some(r) = &label {
                if zone_uses(zn, &r.names) {
                    text_in(paint, ls, theme.accent, x + (32.0 * z), air, 16.0 * z, &r.names[0]);
                }
            }
            let yl = if zn.kind == ZoneKind::Fold || zn.kind == ZoneKind::Scan { format!("{} \u{21ba}", zn.yield_label) } else { zn.yield_label.clone() };
            let ylw = f64::from(paint.text_width(Some(ls), &yl));
            text_in(paint, ls, ink, x + wd - (32.0 * z) - ylw, air, 16.0 * z, &yl);
            let (ex, ey) = w::end_port(n, wd / z);
            paint_socket(paint, &theme, Some(&n.ty), true, z, (x + (ex * z), y + (ey * z)));
        }
    }
    if zn.kind != ZoneKind::Let && zn.kind != ZoneKind::State && zn.kind != ZoneKind::Branch {
        paint_selector(paint, t, z, fs, count, probe, (x, y, wd));
    }
    paint_rail(paint, t, z, fs, x, y + (p::HEAD_HEIGHT * z), wd, true, &p::extra_rails(zn));
    if selected {
        paint.brackets(x as f32, y as f32, wd as f32, h as f32, 3.0, 8.0, 2.0, theme.accent);
    }
}

/// A graph input is a value node: a header-only card with its value in a field at the right.
pub fn paint_input(paint: &mut Paint, t: &Pane, z: f64, fs: i64, i: &p::Input, selected: bool, rect: (f64, f64, f64, f64)) {
    let theme = t.theme;
    let (x, y, wd, h) = rect;
    let hh = p::HEAD_HEIGHT * z;
    paint_card(paint, x, y, wd, h, theme.input, card_edge(t, theme.input, selected));
    paint.fill((x + (HEAD_PAD * z)) as f32, (y + (8.0 * z)) as f32, (8.0 * z) as f32, (8.0 * z) as f32, ty_color(t, Some(&i.ty)));
    let fx = x + (value_x(p::NODE_WIDTH) * z);
    let nm = fitted(paint, fs, 0.0f64.max(fx - (6.0 * z) - (x + (23.0 * z))), &i.name);
    text_in(paint, fs, theme.foreground, x + (23.0 * z), y, hh, &nm);
    // a number is the live editor's (built in `update` at 13 points, like the sheet's header value);
    // anything else is painted here
    match &i.default {
        Some(form) if z >= 0.5 && matches!(form.node, SNode::Num(_)) => {}
        Some(d) => {
            let text = match &d.node {
                SNode::Str(s) => String::from_utf8_lossy(d.bytes(*s)).into_owned(),
                _ => lisp::flat(d),
            };
            let numeric = matches!(d.node, SNode::Num(_));
            paint_field(paint, z, fs, &theme, fx, y + (4.0 * z), FIELD_W * z, !numeric, None, false, theme.foreground, &text);
        }
        None => paint_field(paint, z, fs, &theme, fx, y + (4.0 * z), FIELD_W * z, true, None, false, theme::ink_3(&theme), &ty::to_string(&i.ty)),
    }
    paint_socket(paint, &theme, Some(&i.ty), true, z, (x + wd - (1.0 * z), y + (PORT_Y * z)));
    if selected {
        paint.brackets(x as f32, y as f32, wd as f32, h as f32, 3.0, 8.0, 2.0, theme.accent);
    }
}

pub fn paint_return(paint: &mut Paint, t: &Pane, z: f64, fs: i64, s: &p::Scope, selected: bool, rect: (f64, f64, f64, f64)) {
    let theme = t.theme;
    let (x, y, wd, h) = rect;
    let hh = p::HEAD_HEIGHT * z;
    let rh = p::ROW_HEIGHT * z;
    paint_card(paint, x, y, wd, h, theme.input, card_edge(t, theme.input, selected));
    paint.fill((x + (HEAD_PAD * z)) as f32, (y + (8.0 * z)) as f32, (8.0 * z) as f32, (8.0 * z) as f32, theme.foreground);
    text_in(paint, fs, theme.foreground, x + (23.0 * z), y, hh, "return");
    let ry = y + (p::BODY_TOP * z);
    text_in(paint, fs, theme::ink_2(&theme), x + (LABEL_X * z), ry, rh, "result");
    let right = x + wd - (HEAD_PAD * z);
    match &s.result {
        p::ScopeResult::Link(l) => {
            let arrow = "\u{2190}";
            let aw = f64::from(paint.text_width(Some(fs), arrow));
            let gap = 6.0 * z;
            let l = fitted(paint, fs, wd * 0.5, l);
            let lw = f64::from(paint.text_width(Some(fs), &l));
            text_in(paint, fs, theme.accent, right - lw - gap - aw, ry, rh, arrow);
            text_in(paint, fs, theme::ink_2(&theme), right - lw, ry, rh, &l);
        }
        p::ScopeResult::Node(_) => {}
        p::ScopeResult::Literal(ex) => {
            let flat = lisp::flat(ex);
            let text = fitted(paint, fs, wd * 0.5, &flat);
            let tw = f64::from(paint.text_width(Some(fs), &text));
            text_in(paint, fs, theme.foreground, right - tw, ry, rh, &text);
        }
    }
    let nil = matches!(&s.result, p::ScopeResult::Literal(ex) if matches!(&ex.node, SNode::Sym(n) if ex.name(*n) == "nil"));
    paint_socket(paint, &theme, None, !nil, z, (x, ry + (12.0 * z)));
    if selected {
        paint.brackets(x as f32, y as f32, wd as f32, h as f32, 3.0, 8.0, 2.0, theme.accent);
    }
}

pub fn paint_polyline(clip: (f64, f64, f64, f64), paint: &mut Paint, width: f64, color: Color, pts: &[(f64, f64)]) {
    for pair in pts.windows(2) {
        if let Some((a, b)) = clip_segment(pair[0], pair[1], clip) {
            paint.line_w(p32(a), p32(b), width as f32, color);
        }
    }
}

/// The dashes of a loop's feedback or a switch's inactive wire.
pub fn paint_dashed_polyline(paint: &mut Paint, width: f64, color: Color, pts: &[(f64, f64)]) {
    for pair in pts.windows(2) {
        dashed_line(paint, width, color, pair[0], pair[1]);
    }
}

pub fn tag_name(t: &Pane, wi: &Wire) -> String {
    match &wi.from {
        Some(pth) => {
            let n = match st::node_of(t, pth) {
                Some(n) => p::title(&n),
                None => pth.last().map_or_else(String::new, ToString::to_string),
            };
            if n.len() > 14 { format!("{}~", &n[..13]) } else { n }
        }
        None => String::new(),
    }
}

pub fn paint_background(paint: &mut Paint, t: &Pane, viewport: (f64, f64, f64, f64), wires: &[usize], zones: &[usize]) {
    let theme = t.theme;
    let (vx, vy, vw, vh) = viewport;
    let z = t.zoom;
    // a dot every 24 points, thinned by powers of two so the dots stay 24 screen points apart or more
    let thin = if z >= 1.0 { 1.0 } else { 2.0f64.powf(((1.0 / z).log2() - 1e-9).ceil()) };
    let spacing = 24.0 * z * thin;
    paint.fill(vx as f32, vy as f32, vw as f32, vh as f32, theme.panel);
    paint.grid(vx as f32, vy as f32, vw as f32, vh as f32, ((vx + (t.pan_x % spacing) - 1.0) as f32, (vy + (t.pan_y % spacing) - 1.0) as f32), spacing as f32, 2.0, color::with_alpha(theme.foreground, 66));
    if z >= 0.5 {
        let px = 480.0 * z;
        let py = 192.0 * z;
        let rem = |v: f64, m: f64| {
            let r = v % m;
            if r < 0.0 { r + m } else { r }
        };
        let x0 = vx + rem(t.pan_x + (84.0 * z), px);
        let y0 = vy + rem(t.pan_y + (84.0 * z), py);
        let line = theme::border(&theme);
        let mut cx = x0;
        while cx < vx + vw {
            let mut cy = y0;
            while cy < vy + vh {
                paint.fill((cx - 4.0) as f32, cy as f32, 9.0, 1.0, line);
                paint.fill(cx as f32, (cy - 4.0) as f32, 1.0, 9.0, line);
                cy += py;
            }
            cx += px;
        }
    }
    for &slot in zones {
        let (pl, ax, ay) = &t.geo.items[slot];
        if let Item::Item(node) = &pl.item {
            if let Some(zn) = &node.zone {
                let taken = zone_taken(t, node, zn);
                paint_zone(paint, taken, &theme, zn.kind, (sx(t, *ax), sy(t, *ay), pl.w * z, pl.h * z));
            }
        }
    }
    // titled frames sit under the wires
    for (scope_path, (ox, oy)) in t.geo.origins.iter() {
        for (title, (fx, fy), (fw, fh)) in frame_list(t, scope_path) {
            let x = sx(t, ox + fx);
            let y = sy(t, oy + fy);
            frame_in(paint, x, y, fw * z, fh * z, Some(color::with_alpha(theme.foreground, 8)), theme::edge(&theme));
            let fsz = 7.max((10.0 * z) as i64);
            paint.text_s(((x + 6.0) as f32, (y + 4.0) as f32), fsz, theme::muted(&theme), &title);
            // delete cross top right, resize grip bottom right
            paint.text_s(((x + (fw * z) - 14.0) as f32, (y + 4.0) as f32), fsz, theme::muted(&theme), "x");
            let ink = theme::ink_3(&theme);
            paint.fill((x + (fw * z) - 4.0) as f32, (y + (fh * z) - 11.0) as f32, 1.0, 8.0, ink);
            paint.fill((x + (fw * z) - 11.0) as f32, (y + (fh * z) - 4.0) as f32, 8.0, 1.0, ink);
        }
    }
    let chain = st::chains_of(t);
    let tag_size = 5.max((11.0 * z).round() as i64);
    for &i in wires {
        let wi = &t.geo.wires[i];
        let pts: Vec<(f64, f64)> = wi.pts.iter().map(|(x, y)| (sx(t, *x), sy(t, *y))).collect();
        let is_selected = match (&t.selected_wire, &wi.target) {
            (Some((sp, sk, _)), Some((tp, tk, _))) => sp == tp && sk == tk,
            _ => false,
        };
        let is_highlighted = t.highlighted.contains(wi);
        let lit = chain.as_ref().map(|c| st::wire_lit(c, wi));
        let mut width = if is_highlighted || lit == Some(true) { 2.0f64.max(2.4 * z) } else { 1.0f64.max(1.5 * z) };
        // a switch's active wire is solid and accented, the others dashed grey
        let layout: Option<bool> = match &wi.target {
            Some((pth, e::ArgKey::Pos(k), _)) => t.switches.iter().find(|(x, _)| x == pth).map(|(_, a)| k == a),
            _ => None,
        };
        let mut color = if is_selected {
            theme.foreground
        } else if layout == Some(true) {
            theme.accent
        } else if layout == Some(false) {
            theme::muted(&theme)
        } else {
            ty_color(t, wi.ty.as_ref())
        };
        // a formula's reads rest thin and faint; selecting either end draws them in full
        let faint = wi.refers && !(is_selected || is_highlighted || lit == Some(true));
        if lit == Some(false) || faint {
            color = color::with_alpha(color, if faint { 90 } else { 40 });
        }
        if faint {
            width = 1.0f64.max(z);
        }
        if st::tagged(t, wi) && !is_selected {
            // a long wire reads as a stub and the source's name at the use site
            let (ax, ay) = (sx(t, wi.a.0), sy(t, wi.a.1));
            let (bx, by) = (sx(t, wi.b.0), sy(t, wi.b.1));
            let stub = 14.0 * z;
            paint.line_w((ax as f32, ay as f32), ((ax + stub) as f32, ay as f32), width as f32, color);
            paint.line_w(((bx - stub) as f32, by as f32), (bx as f32, by as f32), width as f32, color);
            let name = tag_name(t, wi);
            let tw = f64::from(paint.text_width(Some(tag_size), &name));
            let ink = if lit == Some(false) { color::with_alpha(theme::ink_2(&theme), 60) } else { theme::ink_2(&theme) };
            paint.text_s(((bx - stub - 4.0 - tw) as f32, (by - (tag_size as f64 / 2.0) - 1.0) as f32), tag_size, ink, &name);
        } else if layout == Some(false) {
            paint_dashed_polyline(paint, width, color, &pts);
        } else {
            paint_polyline(viewport, paint, width, color, &pts);
        }
    }
}

/// Whether a branch arm is the one the probes took.
fn zone_taken(t: &Pane, node: &p::Node, zn: &p::Zone) -> bool {
    if zn.kind != ZoneKind::Branch {
        return false;
    }
    let records = match &t.records {
        Some(r) => r,
        None => return false,
    };
    let (leaf, outer) = match node.path.split_last() {
        Some(x) => x,
        None => return false,
    };
    let split = match leaf.rfind('#') {
        Some(s) => s,
        None => return false,
    };
    let mut parent_path: flow_check::workspace::Path = outer.to_vec();
    parent_path.push(leaf[..split].into());
    let arm = &leaf[split + 1..];
    let parent = match st::node_of(t, &parent_path) {
        Some(p) => p,
        None => return false,
    };
    let index: i64 = if arm == "else" {
        parent.rows.iter().filter(|r| matches!(r.key, e::ArgKey::Arm(i) if i >= 0)).count() as i64
    } else if arm == "then" {
        0
    } else {
        arm[5..].parse::<i64>().unwrap_or(1) - 1
    };
    let chain: Vec<i64> = t.chains.get(&parent.path).map(|c| c.iter().map(|x| t.saved.probe(x)).collect()).unwrap_or_default();
    let r = records.borrow_mut().taken_arm(&parent, &chain) == Some(index);
    r
}

// ---- the graph's theme tokens: port colours per Lisp type and zone colours

fn theme_hex(value: &str) -> Color {
    color::hex_exn(value).expect("Theme: bad colour constant")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ports {
    pub geometry: Color,
    pub float: Color,
    pub int: Color,
    pub vec3: Color,
    pub bool: Color,
    pub compound: Color,
    pub output: Color,
    pub hint: Color,
    pub text: Color,
    pub fn_: Color,
    pub record: Color,
}

const PORT_COLORS: [(&str, &str); 6] = [
    ("#6b50ae", "#a98cf5"),
    ("#285f77", "#72b3cf"),
    ("#3b7d4e", "#74c28e"),
    ("#b0435f", "#f08aa3"),
    ("#a3407a", "#e88ac0"),
    ("#4f6a5f", "#9cc2b2"),
];

pub fn port_hint() -> Color {
    theme_hex("#f5cf4f")
}

pub fn ports(theme: &Theme) -> Ports {
    let night = theme::dark(theme);
    let pick = |i: usize| {
        let (light, dark) = PORT_COLORS[i];
        theme_hex(if night { dark } else { light })
    };
    let vec3 = pick(0);
    Ports {
        geometry: theme.accent,
        float: pick(1),
        int: pick(2),
        vec3,
        bool: pick(3),
        compound: vec3,
        output: theme.foreground,
        hint: port_hint(),
        text: theme::muted(theme),
        fn_: pick(4),
        record: pick(5),
    }
}

/// Graph zones: a tinted rectangle with a border; `dashed` draws the border as dashes, and a hollow
/// zone has no fill worth the name. The colours are the study's `--z-*` tokens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Zone {
    pub fill: Color,
    pub edge: Color,
    pub dashed: bool,
}

fn zone(theme: &Theme, light: (i64, i64, i64, i64, i64), night: (i64, i64, i64, i64, i64), dashed: bool) -> Zone {
    let (r, g, b, fill, edge) = if theme::dark(theme) { night } else { light };
    Zone { fill: color::rgba(r, g, b, fill), edge: color::rgba(r, g, b, edge), dashed }
}

pub fn zone_for(theme: &Theme) -> Zone {
    zone(theme, (240, 72, 31, 15, 92), (255, 107, 69, 16, 100), false)
}

pub fn zone_fold(theme: &Theme) -> Zone {
    zone(theme, (107, 80, 174, 19, 128), (169, 140, 245, 20, 128), false)
}

pub fn zone_sum(theme: &Theme) -> Zone {
    zone(theme, (40, 95, 119, 20, 128), (114, 179, 207, 20, 128), false)
}

pub fn zone_fn(theme: &Theme) -> Zone {
    zone(theme, (163, 64, 122, 9, 140), (232, 138, 192, 10, 128), true)
}

pub fn zone_let(theme: &Theme) -> Zone {
    zone(theme, (34, 43, 43, 0, 87), (225, 231, 227, 0, 77), true)
}

pub fn zone_branch(theme: &Theme, taken: bool) -> Zone {
    zone(
        theme,
        (176, 104, 15, if taken { 26 } else { 5 }, if taken { 140 } else { 70 }),
        (240, 175, 80, if taken { 30 } else { 6 }, if taken { 150 } else { 80 }),
        false,
    )
}
