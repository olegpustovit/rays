// Geometry report priority remains stable within the existing bounded probe budget.
// the graph pane's frame gestures; private state, commands and
// painting compose here on the shared PXUI handle.
use flow_syntax::symbol::Symbol;
use crate::scope_commands::*;
use crate::scope_paint as sp;
use crate::scope_state::*;
use crate::scope_wires::*;
use flow_check::workspace::Path;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit as e;
use flow_graph::probe::Footer;
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level, Placed};
use flow_syntax::syntax::{self as s, Node as SNode};
use flow_syntax::ty::Ty;
use input::{Event, Key, MouseButton};
use param::number::Kind;
use pxui::theme;
use pxui::ui::*;
use rays::frame as rframe;
use pxui::geom::{color, p32, p64};
use std::collections::HashMap;
use std::rc::Rc;

fn number_syntax(text: String) -> s::Syntax {
    s::make(s::Draft::Num(text))
}

// Keep these tiny transforms local in the frame and painter.
pub fn sx(t: &Pane, gx: f64) -> f64 {
    t.x as f64 + t.pan_x + gx * t.zoom
}
pub fn sy(t: &Pane, gy: f64) -> f64 {
    t.y as f64 + t.pan_y + gy * t.zoom
}
fn left_button(sg: &Signal) -> bool {
    sg.button == Some(MouseButton::LeftButton.code() as u8)
}

/// The number field of a row, the kit's (`param::number`, as `Rays_shell.Kit.number`): the kind is the
/// row's type, and only a literal with no typed row behind it is read by its spelling. `Some text` the
/// frame the number changed. With `follow` (the key of the box it is built in and the change a value
/// makes) it is a `kept_drag` field: a press holds it and the pane follows its drag on kept cards
/// (`follow_drag`).
pub fn num_field(ui: &mut Ui, at: (f64, f64), w: f64, h: f64, size: Option<i64>, kind: Option<Kind>, range: Option<(f64, f64)>, nonfinite: bool, label: &str, text: &str, follow: Option<(i64, &dyn Fn(String) -> Change)>) -> Option<String> {
    let nonfinite = nonfinite && kind != Some(Kind::Int) && range.is_none();
    let kind = match kind {
        Some(k) => k,
        None => {
            if text.chars().any(|c| c == '.' || c == 'e') || ["NaN", "Infinity", "-Infinity"].contains(&text) {
                Kind::Float
            } else {
                Kind::Int
            }
        }
    };
    let valid = move |s: &str| match s::number_value(s) {
        Some(f) => f.is_finite() || nonfinite,
        None => false,
    };
    let field = follow.map(|(seed, _)| pxui::ui_core::key_of(seed, label));
    // a held number scrubs by the range it had at the press (a literal's range follows its value)
    let scrub_range = HELD.with(|h| h.borrow().as_ref().filter(|h| Some(h.field) == field).map_or(range, |h| h.range));
    let mut o = ValueFieldOpts::default();
    o.size = size;
    o.kept_drag = follow.is_some();
    o.fraction = range.and_then(|r| param::number::fraction(r, text)).map(|f| f as f32);
    o.scrub = Some(Rc::new(move |origin, dx, fine| param::number::scrub(kind, scrub_range, origin, dx as f64, fine)));
    let text2 = ui.value_field(p32(at), w as f32, h as f32, &o, &valid, label, text).0;
    // (a verifying run of a kept card leaves the held number to the pane's patch)
    if let (Some(field), Some((seed, edit)), false) = (field, follow, ui.verifying()) {
        let (pressed, holding) = ui.box_of_key(field).map(|b| ui.signal(&b)).map_or((false, false), |s| (s.pressed, s.held));
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            match h.as_mut() {
                _ if pressed => *h = Some(Held { field, tile: seed, kind, range, size, nonfinite, origin: text.to_owned(), last: text2.clone(), edit: edit(text.to_owned()), patched: false }),
                Some(mine) if mine.field == field && holding => mine.last = text2.clone(),
                Some(mine) if mine.field == field => *h = None,
                _ => {}
            }
        });
    }
    if text2 != text && valid(&text2) { Some(text2) } else { None }
}

/// The `kept_drag` number of a card the pointer holds, as its press built it: its field's key, the
/// tile it is in, how it scrubs and shows, the text the scrub starts from, the last text it wrote,
/// the change its press's value made, and whether the last frame of its drag was shown by a patch.
struct Held {
    field: i64,
    tile: i64,
    kind: Kind,
    range: Option<(f64, f64)>,
    size: Option<i64>,
    nonfinite: bool,
    origin: String,
    last: String,
    edit: Change,
    patched: bool,
}

thread_local! {
    // ponytail: one held number per thread (the pointer holds one field), found by its field's key
    static HELD: std::cell::RefCell<Option<Held>> = const { std::cell::RefCell::new(None) };
}

/// The held number is a card's of this pane and dragged this frame (`Ui::field_drag`).
fn held_drag(t: &Pane, ui: &Ui) -> bool {
    HELD.with(|h| {
        h.borrow().as_ref().is_some_and(|h| t.hovers.borrow().cards.iter().any(|c| c.tile.box_key == h.tile) && ui.box_of_key(h.field).is_some_and(|b| ui.field_drag(&b).is_some()))
    })
}

/// A frame of the held number's drag on kept cards: its value as its build would read it, the field
/// patched (`patched` false when a patch cannot show it: the content builds next frame) and the
/// change it makes, when the value moved.
fn follow_drag(ui: &mut Ui) -> Option<Change> {
    HELD.with(|h| {
        let mut h = h.borrow_mut();
        let h = h.as_mut()?;
        let field = ui.box_of_key(h.field)?;
        let (_, dx) = ui.field_drag(&field)?;
        let fine = ui.press_keys(&field).contains(&Key::Shift.code());
        let text = param::number::scrub(h.kind, h.range, &h.origin, dx as f64, fine);
        if text == h.last {
            h.patched = true;
            return None;
        }
        let o = ValueFieldOpts { size: h.size, fraction: h.range.and_then(|r| param::number::fraction(r, &text).map(|f| f as f32)), ..ValueFieldOpts::default() };
        h.patched = ui.patch_field(&field, &o, &text);
        let valid = s::number_value(&text).is_some_and(|f| f.is_finite() || h.nonfinite);
        h.last = text.clone();
        let mut edit = h.edit.clone();
        if let Change::SyntaxEdit(e::Op::SetArg { value, .. } | e::Op::SetInputDefault { value, .. }) = &mut edit {
            *value = number_syntax(text);
        }
        valid.then_some(edit)
    })
}

/// What a row's field is: its type's kind (a vector's cells are floats).
pub fn kind_of(r: &p::Row) -> Option<Kind> {
    match &r.ty {
        Some(Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) => Some(Kind::Float),
        Some(Ty::Int) => Some(Kind::Int),
        _ => None,
    }
}

/// and its soft range
pub fn range_of(r: &p::Row) -> Option<(f64, f64)> {
    match r.control {
        p::Control::Range(lo, hi) => Some((lo, hi)),
        _ => None,
    }
}

/// The row of a card at a point of it (`px`, `py` relative to the card, graph units): the header's in-port
/// row at the top left (anywhere in the header with `header` infinite), else the row of the line there;
/// nothing for a folder label or a card drawn as a chip or point.
pub fn row_at(header: f64, pl: &Placed, n: &p::Node, shown: Level, px: f64, py: f64) -> Option<usize> {
    if shown == Level::Point || shown == Level::Chip {
        if px < header && py < p::HEAD_HEIGHT { n.rows.iter().position(|r| r.head) } else { None }
    } else if py < p::BODY_TOP {
        if px < header { n.rows.iter().position(|r| r.head) } else { None }
    } else {
        let k = ((py - p::BODY_TOP - (note_rows(n) * p::ROW_HEIGHT)) / p::ROW_HEIGHT).floor() as i64;
        if k < 0 || k >= pl.lines.len() as i64 {
            None
        } else {
            match &pl.lines[k as usize] {
                p::Line::Row(i, _) => Some(*i),
                _ => None,
            }
        }
    }
}

/// The nodes of one scope a rubber band touches (an expanded zone only when it is inside it), by
/// their slots, into `hits` (cleared first; nothing allocated once it has room).
pub fn marquee_hits(t: &Pane, r: (f64, f64, f64, f64), hits: &mut Vec<usize>) {
    let (rx, ry, rw, rh) = r;
    let z = t.zoom;
    hits.clear();
    for (k, (pl, ax, ay)) in t.geo.items.iter().enumerate() {
        if let Item::Item(_) = pl.item {
            let (x, y, w, h) = (sx(t, *ax), sy(t, *ay), pl.w * z, pl.h * z);
            let touches = x < rx + rw && x + w > rx && y < ry + rh && y + h > ry;
            let inside = x >= rx && y >= ry && x + w <= rx + rw && y + h <= ry + rh;
            if ((pl.inner.is_none() || pl.collapsed) && touches) || inside {
                hits.push(k);
            }
        }
    }
    // one scope: the first hit's
    let scope_of = |k: usize| {
        let q = &t.geo.items[k].0.path;
        &q[..q.len().saturating_sub(1)]
    };
    if let Some(&first) = hits.first() {
        let first = scope_of(first);
        hits.retain(|&k| scope_of(k) == first);
    }
}

pub fn valid_name(s: &str) -> bool {
    !s.is_empty() && !s.chars().any(|c| [' ', '(', ')', '[', ']', '{', '}', '"', ';', '\''].contains(&c))
}

pub fn single_form(text: &str) -> Option<s::Syntax> {
    match s::parse(text) {
        Ok(mut forms) if forms.len() == 1 => Some(forms.remove(0)),
        _ => None,
    }
}

pub fn context_items(t: &Pane, path: &[Symbol]) -> Vec<(String, bool)> {
    let imported = t.saved.imported(path);
    if let Some(file) = imported {
        return vec![(format!("Open {}", file), true)];
    }
    let n = match node_of(t, path) {
        None => return Vec::new(),
        Some(n) => n,
    };
    let (parent, arm) = conditional_target(&n);
    let conditional = match node_of(t, &parent) {
        Some(pn) => pn.head == "cond" || pn.head == "case",
        None => false,
    };
    let zone: Option<&p::Node> = match &n.zone {
        Some(zn) if zn.kind != p::ZoneKind::Let => Some(&n),
        _ => None,
    };
    let collapse_label = match zone {
        Some(zn) => if t.saved.collapsed(&zn.path) { "Expand zone" } else { "Collapse zone" },
        None => "Collapse zone",
    };
    let delete_label = if parent == n.path { "Delete first arm" } else { "Delete arm" };
    let b = |s: &str, on: bool| (s.to_owned(), on);
    vec![
        b("Fold into its use", true),
        b("Unfold a call", true),
        b("Hoist out", n.invariant),
        b(collapse_label, zone.is_some()),
        b("Toggle bypass", true),
        b("Repeat (loop)", true),
        b("Iterate (feed back)", true),
        b("Make function", true),
        b("Make macro", true),
        b("Make defn", true),
        b("Delete", true),
        b("Wrap conditional", true),
        b("Add arm", conditional),
        b(delete_label, conditional && (parent == n.path || arm >= 0)),
        b("Wrap in map", true),
        b("Wrap in filter", true),
        b("Inline function", n.zone.is_none() && n.macro_.is_none() && !n.head.contains('/')),
        b("Move up in text", true),
        b("Move down in text", true),
    ]
}

pub fn context_command(i: i64) -> Command {
    match i {
        0 => Command::FoldInto,
        1 => Command::Unfold,
        2 => Command::Hoist,
        3 => Command::Collapse,
        4 => Command::Bypass,
        5 => Command::WrapRepeat,
        6 => Command::WrapIterate,
        7 => Command::MakeFn,
        8 => Command::MakeMacro,
        9 => Command::MakeDefn,
        11 => Command::WrapIf,
        12 => Command::AddArm,
        13 => Command::DeleteArm,
        14 => Command::WrapMap,
        15 => Command::WrapFilter,
        16 => Command::InlineFn,
        17 => Command::ItemUp,
        18 => Command::ItemDown,
        _ => Command::Delete,
    }
}

/// The glyph of an expression row opens it as cards; any other inline call unfolds into a binding.
pub fn unfold_row(n: &p::Node, r: &p::Row) -> Vec<Change> {
    if let Some(ex) = &r.expr {
        // only an input of a call holds a nested card: a record field or a binding unfolds into a binding
        if matches!(r.key, ArgKey::Pos(_) | ArgKey::Kw(_) | ArgKey::Arm(_)) && e::value_call(ex) {
            let last = n.path.len() - 1;
            let zone: Path = n
                .path
                .iter()
                .enumerate()
                .map(|(i, sg)| if i == last { e::nested_leaf(&n.name, &r.key).map(Into::into).unwrap_or(*sg) } else { sg.clone() })
                .collect();
            return vec![Change::ZoneCollapsed { zone, collapsed: false }];
        }
    }
    vec![Change::SyntaxEdit(e::Op::Unfold { node: n.path.clone(), key: r.key.clone(), sub: Vec::new() })]
}

/// The footer of a card at the iterations pinned in its zones: its value is the ghost beside the out-port.
pub fn footer_of(t: &Pane, records: &Rc<std::cell::RefCell<flow_graph::probe::Probe>>, n: &p::Node) -> Footer {
    let chain: Vec<Path> = t.chains.get(&n.path).cloned().unwrap_or_default();
    let probes: Vec<i64> = chain.iter().map(|c| t.saved.probe(c)).collect();
    records.borrow_mut().footer(n, &probes)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tap {
    Step(i64),
    Track,
    Toggle,
    Lens,
    LensStep(i64),
    Replace,
}

/// Deletable-input clicks return an unwire action that probes the previous value.
#[derive(Clone, Debug, PartialEq)]
pub enum Del {
    /// `n.rows[i]`
    Unwire(usize),
    Disconnect(ArgKey),
}

/// What a card's build leaves for the content's gestures: its zone and lens controls and the
/// changes its fields asked for.
pub struct Built {
    pub slot: usize,
    pub taps: Vec<(UiBox, Tap)>,
    pub fields: Vec<Change>,
}

/// A box over the content: no hover of it changes what the content builds (`scope_hover`).
fn box_in(ui: &mut Ui, flags: Flags, w: f64, h: f64, at: (f64, f64), name: &str) -> UiBox {
    ui.box_(name, BoxOpts::default().flags(flags | HOVER_FREE).w(Size::Px(w as f32)).h(Size::Px(h as f32)).at(p32(at)))
}

/// A port: in the hit list for the hover and for what a press is on, but the canvas takes its input.
const PORT: Flags = BLOCKING;

/// A tile's ports and taps: out-ports, inputs (with what a click on them takes off), zone controls.
pub fn tile_ports(t: &Pane, ui: &mut Ui, z: f64, fs: i64, pl: &Placed, tile: &UiBox, ports: &mut Vec<crate::scope_hover::Port>) -> Vec<(UiBox, Tap)> {
    use crate::scope_hover::{Port, PortKind};
    let shown = pl.shown;
    let saved = ui.enter(tile);
    let mut outs: Vec<Port> = Vec::new();
    let mut inputs: Vec<Port> = Vec::new();
    let mut taps: Vec<(UiBox, Tap)> = Vec::new();
    let mut out = |b: UiBox, src: String, ty: Option<Ty>| outs.push(Port { b, kind: PortKind::Out(src, ty) });
    match &pl.item {
        Item::Input(i) => {
            let pos = if shown == Level::Point { (pl.w, p::POINT_SIZE / 2.0) } else { (pl.w - 1.0, sp::PORT_Y) };
            let b = bxz(ui, z, PORT, "out", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
            out(b, i.name.clone(), Some(i.ty.clone()));
        }
        Item::Return => {
            let b = bxz(ui, z, PORT, "in", (0.0 - 7.0, p::BODY_TOP + 12.0 - 7.0), (14.0, 14.0));
            inputs.push(Port { b, kind: PortKind::In(None) });
        }
        Item::Item(n) => {
            let node_src = if n.binds.len() == 1 && n.binds[0] == n.name { Some(n.name.clone()) } else { None };
            let plain_for = match &n.zone {
                Some(zn) if zn.kind == p::ZoneKind::For => !pl.collapsed,
                _ => false,
            };
            if let Some(src) = &node_src {
                if !plain_for {
                    let pos = if shown == Level::Point { (pl.w, p::POINT_SIZE / 2.0) } else { (pl.w - 1.0, sp::PORT_Y) };
                    let b = bxz(ui, z, PORT, "out", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                    out(b, src.clone(), Some(n.ty.clone()));
                }
            }
            // the header's in-port: the first geometry slot
            if let Some(hi) = n.rows.iter().position(|r| r.head) {
                if n.zone.is_none() {
                    let pos = if shown == Level::Point { (0.0, p::POINT_SIZE / 2.0) } else { (1.0, sp::PORT_Y) };
                    let sink = bxz(ui, z, PORT, "d:head", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                    ui.a11y_name(&sink, &n.rows[hi].label);
                    inputs.push(Port { b: sink, kind: PortKind::In(wired(&n.rows[hi]).then_some(Del::Unwire(hi))) });
                }
            }
            match &n.zone {
                Some(zn) if !pl.collapsed => {
                    // the label row's variable: the collection's in-port at the edge, the variable's
                    // out-port in the left strip once something reads it
                    if let Some(li) = label_index(zn) {
                        let r = &zn.rail[li];
                        if r.key.is_some() && zn.kind != p::ZoneKind::For {
                            let sink = bxz(ui, z, PORT, "rd:label", (1.0 - 7.0, sp::PORT_Y - 7.0), (14.0, 14.0));
                            let del = match (&r.key, &r.expr) {
                                (Some(key), Some(ex)) if !e::free_names(ex).is_empty() => Some(Del::Disconnect(key.clone())),
                                _ => None,
                            };
                            inputs.push(Port { b: sink, kind: PortKind::In(del) });
                        }
                        if sp::zone_uses(zn, &r.names) {
                            let pos = begin_port(n, zn, li);
                            let b = bxz(ui, z, PORT, "rail:label", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            out(b, r.names.first().cloned().unwrap_or_default(), r.ty.clone());
                        }
                    }
                    for (i, ri) in extra_indices(zn).iter().enumerate() {
                        let r = &zn.rail[*ri];
                        let cy = p::HEAD_HEIGHT + (i as f64 * p::ROW_HEIGHT) + 12.0;
                        if r.role != p::Role::Capture {
                            let pos = begin_port(n, zn, *ri);
                            let b = bxzf(ui, z, PORT, format_args!("rail:{}", i), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            out(b, r.names.first().cloned().unwrap_or_default(), r.ty.clone());
                        }
                        // wired rail rows can be disconnected from their socket
                        if r.key.is_some() {
                            let b = bxzf(ui, z, PORT, format_args!("rd:{}", i), (0.0 - 7.0, cy - 7.0), (14.0, 14.0));
                            let del = match (&r.key, &r.expr) {
                                (Some(key), Some(ex)) if !e::free_names(ex).is_empty() => Some(Del::Disconnect(key.clone())),
                                _ => None,
                            };
                            inputs.push(Port { b, kind: PortKind::In(del) });
                        }
                    }
                    if zn.kind != p::ZoneKind::Let && zn.kind != p::ZoneKind::State && zn.kind != p::ZoneKind::Branch {
                        // the selector of the label row: a button each side of the reading
                        let (selx, sely, selw, selh, _) = sp::selector_geo(&mut |size, text| measure_ui(ui, t, size, text), z, fs, pl.w * z, count_of(t, &n.path), t.saved.probe(&n.path));
                        let (selx, sely, selw, selh) = (selx / z, sely / z, selw / z, selh / z);
                        let b = bxz(ui, z, CLICKABLE, "prev", (selx - 4.0, sely), (16.0, selh));
                        taps.push((b, Tap::Step(-1)));
                        let b = bxz(ui, z, CLICKABLE, "next", (selx + selw - 12.0, sely), (16.0, selh));
                        taps.push((b, Tap::Step(1)));
                        let b = bxz(ui, z, CLICKABLE, "track", (selx + 12.0, sely), (selw - 24.0, selh));
                        taps.push((b, Tap::Track));
                    }
                    if zn.kind != p::ZoneKind::Let {
                        let b = bxz(ui, z, CLICKABLE, "toggle", (0.0, 0.0), (64.0, p::HEAD_HEIGHT));
                        taps.push((b, Tap::Toggle));
                    }
                }
                Some(zn) => {
                    for (i, r) in zn.rail.iter().enumerate() {
                        if r.key.is_some() || r.role == p::Role::Capture {
                            let pos = (0.0, p::BODY_TOP + ((i as f64 + 0.5) * p::ROW_HEIGHT));
                            let b = bxzf(ui, z, PORT, format_args!("rd:{}", i), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            inputs.push(Port { b, kind: PortKind::In(None) });
                        }
                    }
                    if zn.kind != p::ZoneKind::Let {
                        let b = bxz(ui, z, CLICKABLE, "toggle", (sp::HEAD_PAD, 0.0), (64.0, p::HEAD_HEIGHT));
                        taps.push((b, Tap::Toggle));
                    }
                }
                None => {
                    if let Some(l) = &n.lens {
                        if shown != Level::Point && shown != Level::Chip {
                            let b = bxz(ui, z, CLICKABLE, "lens", (pl.w - 22.0, 2.0), (20.0, 20.0));
                            taps.push((b, Tap::Lens));
                            if let Some(step) = lens_of(t, &n.path) {
                                let lh = p::lens_height(l, step);
                                let top = sp::lens_top(pl.h, lh);
                                let len = l.steps.len() as i64;
                                for i in 0..=len {
                                    let ((bxx, byy), size) = sp::lens_button_box(len, i);
                                    let b = bxzf(ui, z, CLICKABLE, format_args!("ls{}", i), (bxx, top + byy), size);
                                    taps.push((b, Tap::LensStep(i)));
                                }
                                let ((rx, ry), size) = sp::lens_replace_box(lh);
                                let b = bxz(ui, z, CLICKABLE, "lr", (rx, top + ry), size);
                                taps.push((b, Tap::Replace));
                            }
                        }
                    }
                    if shown == Level::Card || shown == Level::Full {
                        let top = rows_top(n, 0.0);
                        let nlines = pl.lines.len();
                        for (j, (f, ty)) in n.outputs.iter().enumerate() {
                            let src = if n.binds.len() == 1 && n.binds[0] == n.name { format!("{}.{}", n.name, f) } else { f.clone() };
                            let pos = (pl.w, top + (((nlines + j) as f64 + 0.5) * p::ROW_HEIGHT));
                            let b = bxzf(ui, z, PORT, format_args!("o:{}", j), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            out(b, src, Some(ty.clone()));
                        }
                        for (k, line) in pl.lines.iter().enumerate() {
                            if let p::Line::Row(i, r) = line {
                                let ry = top + (k as f64 * p::ROW_HEIGHT);
                                if r.socket {
                                    let sink = bxzf(ui, z, PORT, format_args!("d:{}", i), (0.0 - 7.0, ry + 12.0 - 7.0), (14.0, 14.0));
                                    ui.a11y_name(&sink, &r.label);
                                    inputs.push(Port { b: sink, kind: PortKind::In(wired(r).then_some(Del::Unwire(*i))) });
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    ui.leave(saved);
    // to a screen reader a port is a button named by what it carries
    for q in &outs {
        ui.set_role(&q.b, pxui::a11y::Role::Button, 0);
        if let PortKind::Out(name, _) = &q.kind {
            ui.a11y_name(&q.b, name);
        }
    }
    for q in &inputs {
        ui.set_role(&q.b, pxui::a11y::Role::Button, 0);
    }
    ports.extend(outs);
    ports.extend(inputs);
    taps
}

/// The live editors of a row: a number, a vector of numbers, a flag.
pub fn row_fields_of(ui: &mut Ui, tile: i64, z: f64, pl: &Placed, n: &p::Node, r: &p::Row, name: &str, ry: f64, size: i64) -> Vec<Change> {
    let at = sp::value_x(pl.w) * z;
    let vw = sp::FIELD_W * z;
    let h = sp::FIELD_H * z;
    let ay = (ry + 4.0) * z;
    let nonfinite = !matches!(r.key, ArgKey::Kw(_));
    let set = |sub: Vec<i64>, value: s::Syntax| Change::SyntaxEdit(e::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub, value });
    if r.chip == p::Chip::Const {
        if let Some(ex) = &r.expr {
            match &ex.node {
                SNode::Num(text) => {
                    let edit = |t: String| set(Vec::new(), number_syntax(t));
                    return match num_field(ui, (at, ay), vw, h, Some(size), kind_of(r), range_of(r), nonfinite, &format!("f{}", name), ex.name(*text), Some((tile, &edit))) {
                        Some(text2) => vec![set(Vec::new(), number_syntax(text2))],
                        None => Vec::new(),
                    };
                }
                SNode::Vec(elems) if (2..=4).contains(&ex.range(*elems).len()) && sp::all_numbers(ex, ex.range(*elems)) => {
                    let elems = ex.range(*elems);
                    let k = elems.len();
                    // a vector is its fields 8 points apart, 112 wide, at the row's right
                    let at = (pl.w - sp::HEAD_PAD - sp::VEC_W) * z;
                    let gap = 8.0 * z;
                    let cw = ((sp::VEC_W * z) - ((k - 1) as f64 * gap)) / k as f64;
                    let mut out = Vec::new();
                    for (c, &el) in elems.iter().enumerate() {
                        if let SNode::Num(text) = ex.arena().row(el).node {
                            let edit = |t: String| set(vec![c as i64], number_syntax(t));
                            if let Some(text2) = num_field(ui, (at + (c as f64 * (cw + gap)), ay), cw, h, Some(size), kind_of(r), None, nonfinite, &format!("f{}.{}", name, c), ex.name(text), Some((tile, &edit))) {
                                out.push(set(vec![c as i64], number_syntax(text2)));
                            }
                        }
                    }
                    return out;
                }
                SNode::Sym(b) if matches!(ex.name(*b), "true" | "false") => {
                    let toggle = ui.box_(
                        &format!("b{}", name),
                        BoxOpts::default().flags(CLICKABLE | TAB_STOP | HOVER_FREE).w(Size::Px((sp::SWITCH_W * z) as f32)).h(Size::Px((sp::SWITCH_H * z) as f32)).at((((pl.w - sp::HEAD_PAD - sp::SWITCH_W) * z) as f32, ((ry + 5.0) * z) as f32)),
                    );
                    if ui.signal(&toggle).clicked {
                        let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                        let name = builder.intern(if ex.name(*b) == "true" { "false" } else { "true" });
                        let id = builder.push(0, SNode::Sym(name), Default::default(), None);
                        return vec![set(Vec::new(), builder.freeze().handle(id))];
                    }
                    return Vec::new();
                }
                _ => {}
            }
        }
    }
    Vec::new()
}

/// Frame-wide state of the content's build: an edit field that closed or opened, the text size, and
/// the overscan the content reaches past each side of the pane.
pub struct Ctx {
    pub finished: bool,
    pub opened: Option<Editing>,
    pub fs: i64,
    pub ov: (f64, f64),
    /// what a field's click selects (`Change::Selected` with it)
    pub select: Option<Vec<Path>>,
}

impl Ctx {
    /// A window point in the pane's points (the content box's origin is the pane's corner).
    fn local(&self, t: &Pane, pt: (f64, f64)) -> (f64, f64) {
        (pt.0 - t.x as f64, pt.1 - t.y as f64)
    }
}

/// A text field over a tile or frame: `Some change` the frame it commits.
pub fn edit_field(c: &mut Ctx, ui: &mut Ui, at: (f64, f64), w: f64, h: f64, key: &str, current: &str, valid: &dyn Fn(&str) -> bool, commit: &dyn Fn(String) -> Change) -> Vec<Change> {
    let mut o = ValueFieldOpts::default();
    o.size = Some(c.fs);
    o.edit = true;
    o.left = (current).parse::<f64>().ok().is_none();
    let (v, open) = ui.value_field(p32(at), w as f32, h as f32, &o, valid, key, current);
    if !open {
        c.finished = true;
    }
    if !open && v != current && valid(&v) { vec![commit(v)] } else { Vec::new() }
}

/// The live widgets of a card's rows and header (value fields, switches, add and fold buttons, read
/// names, reorder and layout buttons, an open name field): the changes they asked for this frame.
fn card_fields(c: &mut Ctx, t: &Pane, ui: &mut Ui, z: f64, fs: i64, pl: &Placed, tile: &UiBox) -> Vec<Change> {
    let tile = *tile;
    let shown = pl.shown;
    // fields, inside the tile
    let mut fields: Vec<Change> = Vec::new();
    match &pl.item {
        Item::Item(n) if n.zone.is_none() && z >= 0.5 && p::value_card(n) => {
            // a literal binding: its value is the field of its header, at body size
            let saved = ui.enter(&tile);
            for r in &n.rows {
                fields.extend(row_fields_of(ui, tile.box_key, z, pl, n, r, "v", 0.0, fs));
            }
            ui.leave(saved);
        }
        Item::Item(n) if n.zone.is_none() && z >= 0.5 && (shown == Level::Card || shown == Level::Full) => {
            let saved = ui.enter(&tile);
            let top = rows_top(n, 0.0);
            for (k, line) in pl.lines.iter().enumerate() {
                if let p::Line::Row(i, r) = line {
                    fields.extend(row_fields_of(ui, tile.box_key, z, pl, n, r, &i.to_string(), top + (k as f64 * p::ROW_HEIGHT), 4.max(fs - 2)));
                }
            }
            ui.leave(saved);
        }
        Item::Input(i) if z >= 0.5 && t.editing != Some(Editing::Default(pl.path.clone())) => {
            if let Some(d) = &i.default {
                if let SNode::Num(text) = &d.node {
                    // a value node: its number is a field in the header, at body size
                    let saved = ui.enter(&tile);
                    let edit = |t: String| Change::SyntaxEdit(e::Op::SetInputDefault { form: i.path[0].to_string(), input: i.name.clone(), value: number_syntax(t) });
                    if let Some(text2) = num_field(ui, (sp::value_x(pl.w) * z, 4.0 * z), sp::FIELD_W * z, sp::FIELD_H * z, Some(fs), None, None, false, "default", d.name(*text), Some((tile.box_key, &edit))) {
                        fields.push(edit(text2));
                    }
                    ui.leave(saved);
                }
            }
        }
        _ => {}
    }
    let mut add_clicks: Vec<Change> = Vec::new();
    if let Item::Item(n) = &pl.item {
        if n.zone.is_none() && z >= 0.5 && (shown == Level::Card || shown == Level::Full) {
            let saved = ui.enter(&tile);
            let top = rows_top(n, 0.0);
            for (k, line) in pl.lines.iter().enumerate() {
                let (i, r) = match line {
                    p::Line::Row(i, r) => (*i, r),
                    _ => continue,
                };
                let row_y = (top + (k as f64 * p::ROW_HEIGHT) + 4.0) * z;
                if r.kind == p::RowKind::Add || matches!(r.chip, p::Chip::Inline { .. }) {
                    let b = ui.box_(
                        &format!("a{}", i),
                        BoxOpts::default()
                            .flags(CLICKABLE | TAB_STOP | HOVER_FREE)
                            .w(Size::Px((if r.kind == p::RowKind::Add { (pl.w - sp::LABEL_X - sp::HEAD_PAD) * z } else { 24.0 * z }) as f32))
                            .h(Size::Px((16.0 * z) as f32))
                            .at((((if r.kind == p::RowKind::Add { sp::LABEL_X } else { sp::value_x(pl.w) }) * z) as f32, row_y as f32)),
                    );
                    // a click on the expression's text (not its glyph, which unfolds) makes the row the inspector's
                    // subject: the holder stays selected, the row is `holder @ [":" ^ label]`
                    let mut subject: Vec<Change> = Vec::new();
                    if let p::Chip::Inline { .. } = r.chip {
                        let cb = ui.box_fmt(format_args!("ex{}", i), BoxOpts::default().flags(CLICKABLE | HOVER_FREE).w(Size::Px(((sp::FIELD_W - 24.0) * z) as f32)).h(Size::Px((16.0 * z) as f32)).at((((sp::value_x(pl.w) + 24.0) * z) as f32, row_y as f32)));
                        let sg = ui.signal(&cb);
                        // a double click expands the call in place, as the glyph does
                        if sg.double_clicked {
                            subject = unfold_row(n, r);
                        } else if sg.clicked {
                            let mut row = n.path.clone();
                            row.push(format!(":{}", r.label).into());
                            c.select = Some(vec![n.path.clone(), row]);
                            subject = vec![Change::Selected];
                        }
                    }
                    fields.extend(Vec::new());
                    add_clicks.extend(subject);
                    if ui.signal(&b).clicked {
                        match (r.kind, &r.chip) {
                            (p::RowKind::Add, _) => match &r.key {
                                ArgKey::Field(_) => {
                                    // the first `fN` the record does not have
                                    let mut k2 = 1;
                                    let name = loop {
                                        let name = format!("f{}", k2);
                                        if n.rows.iter().any(|r2| r2.key == ArgKey::Field(name.clone())) {
                                            k2 += 1;
                                        } else {
                                            break name;
                                        }
                                    };
                                    add_clicks.push(Change::SyntaxEdit(e::Op::AddField { node: n.path.clone(), name, value: number_syntax("0".to_owned()) }));
                                }
                                ArgKey::Kw(kw) if kw.is_empty() => {
                                    c.opened = Some(Editing::Output(n.path.clone()));
                                    // a new output is named first
                                }
                                _ if n.head == "list" || n.head == "str" => {
                                    add_clicks.push(Change::SyntaxEdit(e::Op::AddItem { node: n.path.clone() }));
                                }
                                key => {
                                    // a type with no literal (geometry, a list, a function, a scene) is wired
                                    match &r.ty {
                                        Some(Ty::Named(nm)) if nm == "geometry" => add_clicks.push(Change::Notice("Wire a node onto this row".to_owned())),
                                        Some(ty) => match e::default_for(ty, &r.label, None) {
                                            Some(value) => add_clicks.push(Change::SyntaxEdit(e::Op::SetArg { node: n.path.clone(), key: key.clone(), sub: Vec::new(), value })),
                                            None => add_clicks.push(Change::Notice("Wire a node onto this row".to_owned())),
                                        },
                                        None => add_clicks.push(Change::SyntaxEdit(e::Op::SetArg { node: n.path.clone(), key: key.clone(), sub: Vec::new(), value: number_syntax("0".to_owned()) })),
                                    }
                                }
                            },
                            (_, p::Chip::Inline { .. }) => add_clicks.extend(unfold_row(n, r)),
                            _ => {}
                        }
                    }
                } else if let (p::Chip::Name(nm), true) = (&r.chip, t.folds.contains_key(&(n.path.clone(), i))) {
                    // the fold button of a wired row, where `paint_value` draws its glyph
                    let (left, fw, _, _, _) = sp::wired_geo(&mut |size, text| measure_ui(ui, t, size, text), t, z, fs, 0.0, pl.w * z, true, &sp::wired_name(r, nm));
                    let b = ui.box_fmt(format_args!("fold{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP | HOVER_FREE).w(Size::Px(fw as f32)).h(Size::Px((16.0 * z) as f32)).at(((left - fw - (2.0 * z)) as f32, row_y as f32)));
                    if ui.signal(&b).clicked {
                        add_clicks.push(Change::SyntaxEdit(e::Op::FoldInto { node: t.folds[&(n.path.clone(), i)].clone() }));
                    }
                } else if let p::Chip::Name(nm) = &r.chip {
                    if !e::nested(nm) {
                        // a row that reads a binding: a click on its name selects that binding
                        let root = nm.split('.').next().unwrap_or("").to_owned();
                        let mut target: Option<Path> = None;
                        let mut kk = n.path.len() as i64 - 1;
                        while kk >= 0 {
                            let mut cand: Path = n.path[..kk as usize].to_vec();
                            cand.push(root.clone().into());
                            if node_of(t, &cand).is_some() || input_of(t, &cand).is_some() {
                                target = Some(cand);
                                break;
                            }
                            kk -= 1;
                        }
                        if let Some(cand) = target {
                            let mut m = |size: i64, text: &str| measure_ui(ui, t, size, text);
                            let (left, _, room, _, _) = sp::wired_geo(&mut m, t, z, fs, 0.0, pl.w * z, false, nm);
                            let width = m(fs, "\u{2190}") + (6.0 * z) + m(fs, nm).min(room);
                            let b = ui.box_fmt(format_args!("ref{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP | HOVER_FREE).w(Size::Px(width as f32)).h(Size::Px((16.0 * z) as f32)).at((left as f32, row_y as f32)));
                            if ui.signal(&b).clicked {
                                c.select = Some(vec![cand]);
                                add_clicks.push(Change::Selected);
                            }
                        }
                    }
                }
            }
            ui.leave(saved);
        }
    }
    let mut editors: Vec<Change> = Vec::new();
    match (&t.editing, &pl.item) {
        (Some(Editing::Name(path)), Item::Item(n)) if *path == n.path => {
            let saved = ui.enter(&tile);
            let np = n.path.clone();
            editors = edit_field(c, ui, (21.0 * z, 3.0 * z), (pl.w - 21.0 - sp::HEAD_PAD) * z, 18.0 * z, "name", &p::title(n), &valid_name, &move |v| Change::SyntaxEdit(e::Op::Rename { node: np.clone(), to_: v }));
            ui.leave(saved);
        }
        (Some(Editing::Output(path)), Item::Item(n)) if *path == n.path => {
            match pl.lines.iter().position(|l| matches!(l, p::Line::Row(_, r) if r.kind == p::RowKind::Add && r.key == ArgKey::Kw(String::new()))) {
                Some(k) => {
                    let saved = ui.enter(&tile);
                    let np = n.path.clone();
                    editors = edit_field(c, ui, (sp::LABEL_X * z, (rows_top(n, 0.0) + (k as f64 * p::ROW_HEIGHT) + 4.0) * z), (pl.w - sp::LABEL_X - sp::HEAD_PAD) * z, sp::FIELD_H * z, "output", "", &valid_name, &move |v| {
                        Change::SyntaxEdit(e::Op::AddField { node: np.clone(), name: v, value: number_syntax("0".to_owned()) })
                    });
                    ui.leave(saved);
                }
                None => {
                    c.finished = true;
                }
            }
        }
        (Some(Editing::Default(path)), Item::Input(i)) if *path == i.path => {
            let saved = ui.enter(&tile);
            let text = match &i.default {
                Some(d) => flow_syntax::lisp::flat(d),
                None => String::new(),
            };
            let (form, input) = (i.path[0].to_string(), i.name.clone());
            editors = edit_field(c, ui, (sp::value_x(pl.w) * z, 4.0 * z), sp::FIELD_W * z, sp::FIELD_H * z, "default", &text, &|v: &str| single_form(v).is_some(), &move |v| {
                Change::SyntaxEdit(e::Op::SetInputDefault { form: form.clone(), input: input.clone(), value: single_form(&v).expect("validated") })
            });
            ui.leave(saved);
        }
        _ => {}
    }
    let mut movers: Vec<Change> = Vec::new();
    if let Item::Item(n) = &pl.item {
        if p::reorderable(n) && z >= 0.5 && (shown == Level::Card || shown == Level::Full) {
            let saved = ui.enter(&tile);
            let top = rows_top(n, 0.0);
            for (k, line) in pl.lines.iter().enumerate() {
                if let p::Line::Row(i, r) = line {
                    if let (p::RowKind::Rest, ArgKey::Pos(j)) = (r.kind, &r.key) {
                        if *j >= 1 {
                            let b = ui.box_fmt(format_args!("mv{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP | HOVER_FREE).w(Size::Px((16.0 * z) as f32)).h(Size::Px((16.0 * z) as f32)).at((((pl.w - 22.0) * z) as f32, ((top + (k as f64 * p::ROW_HEIGHT) + 4.0) * z) as f32)));
                            if ui.signal(&b).clicked {
                                movers.push(Change::SyntaxEdit(e::Op::MoveItem { node: n.path.clone(), pos: *j }));
                            }
                        }
                    }
                }
            }
            ui.leave(saved);
        }
    }
    // a switch's rows: a click on a layout's name makes it the active one
    let mut switchers: Vec<Change> = Vec::new();
    if let Item::Item(n) = &pl.item {
        if let Some((_, active)) = t.switches.iter().find(|(sp_, _)| *sp_ == pl.path) {
            if z >= 0.5 && (shown == Level::Card || shown == Level::Full) {
                let saved = ui.enter(&tile);
                let top = rows_top(n, 0.0);
                for (k, line) in pl.lines.iter().enumerate() {
                    if let p::Line::Row(i, r) = line {
                        if let ArgKey::Pos(j) = &r.key {
                            if r.kind != p::RowKind::Add && *j != *active {
                                let b = ui.box_fmt(format_args!("sw{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP | HOVER_FREE).w(Size::Px(((pl.w - 24.0) * z) as f32)).h(Size::Px((16.0 * z) as f32)).at(((12.0 * z) as f32, ((top + (k as f64 * p::ROW_HEIGHT) + 4.0) * z) as f32)));
                                if ui.signal(&b).clicked {
                                    switchers.push(Change::LayoutActive { graph: t.key.clone(), index: *j });
                                }
                            }
                        }
                    }
                }
                ui.leave(saved);
            }
        }
    }
    let mut all = fields;
    all.extend(add_clicks);
    all.extend(editors);
    all.extend(movers);
    all.extend(switchers);
    all
}

/// One visible card in a view of its own (a move of cards offsets it): its tile, its ground, row
/// highlight and painting, its selection brackets, dimming and recession (nodes the hover and the
/// selection patch), its ports and the live widgets of its rows. A card built this frame leaves its
/// controls in `built`; a kept one is as it was.
fn build_card(c: &mut Ctx, t: &Pane, ui: &mut Ui, z: f64, fs: i64, slot: usize, snap: &Rc<Pane>, footers: &HashMap<Path, Footer>, rev: u64, built: &mut Vec<Built>) {
    use std::hash::{Hash, Hasher};
    let (pl, ax, ay) = &t.geo.items[slot];
    let (w, h) = (pl.w * z, pl.h * z);
    let at = c.local(t, (sx(t, *ax), sy(t, *ay)));
    let key = format!("t:{}", flow_syntax::symbol::join_path(&pl.path, "/"));
    let mut hh = std::hash::DefaultHasher::new();
    (rev, slot).hash(&mut hh);
    let mut rec = None;
    let view = ui.view(&key, hh.finish(), Size::Px(w as f32), Size::Px(h as f32), &mut |ui| {
        rec = Some(card(c, t, ui, z, fs, slot, snap, footers, &key, built));
    });
    ui.set_at(&view, p32(at));
    let mut hv = t.hovers.borrow_mut();
    if ui.verifying() {
        if rec.is_some() {
            hv.redeclared(view);
        }
        return;
    }
    match rec {
        Some((tile, ground, brackets, recede, ports, readout)) => hv.card(slot, pl.path.clone(), view, tile, ground, brackets, recede, ports, readout),
        None => hv.keep(view),
    }
}

type CardRec = (UiBox, Option<crate::scope_hover::Ground>, [UiBox; 4], Option<UiBox>, Vec<crate::scope_hover::Port>, crate::scope_hover::Readout);

fn card(c: &mut Ctx, t: &Pane, ui: &mut Ui, z: f64, fs: i64, slot: usize, snap: &Rc<Pane>, footers: &HashMap<Path, Footer>, key: &str, built: &mut Vec<Built>) -> CardRec {
    use crate::scope_hover as sh;
    let (pl, ax, ay) = &t.geo.items[slot];
    let (ax, ay) = (*ax, *ay);
    let (w, h) = (pl.w * z, pl.h * z);
    let shown = pl.shown;
    let tile = box_in(ui, BLOCKING, w, h, (0.0, 0.0), key);
    let failed = |path: &Path| t.failed.iter().any(|(q, _)| q == path);
    // the card's ground: its fill and edge as a node (a collapsed zone's stack painted under it); a
    // point and an open zone have none
    let (looks, band, stack) = match &pl.item {
        _ if shown == Level::Point => (None, false, false),
        Item::Item(n) if n.zone.is_some() && !pl.collapsed => (None, false, false),
        Item::Item(n) => (Some(sh::ground(t, failed(&n.path), n.expr, true)), n.zone.is_none() && (shown == Level::Card || shown == Level::Full), n.zone.is_some()),
        Item::Input(_) | Item::Return => (Some(sh::ground(t, false, false, false)), false, false),
    };
    let node = |ui: &mut Ui, name: &str, at: (f64, f64), size: (f64, f64), look: Look, flags: Flags| {
        ui.node(name, Node { w: Size::Px(size.0 as f32), h: Size::Px(size.1 as f32), at: Some((at.0 as f32, at.1 as f32)).into(), look, flags, ..Node::DEFAULT })
    };
    let saved = ui.enter(&tile);
    if stack {
        let st = ui.box_("stack", BoxOpts::default().w(Size::Px(w as f32)).h(Size::Px(h as f32)).at((0.0, 0.0)));
        let snap = snap.clone();
        ui.draw(&st, move |paint, rect| sp::paint_stack(paint, &snap, z, rect.f64()));
    }
    let ground = looks.map(|looks| {
        let g = node(ui, "g", (0.0, 0.0), (w, h), looks[0], NO_FLAGS);
        // the first row line's band, behind the card's text; idle it is the ground's colour
        let row = match &pl.item {
            Item::Item(n) if band => pl.lines.iter().position(|l| matches!(l, p::Line::Row(..))).map(|k| {
                let ry = (rows_top(n, 0.0) + (k as f64 * p::ROW_HEIGHT)) * z;
                let hl = node(ui, "hl", (1.0, ry), (w - 2.0, p::ROW_HEIGHT * z), Look { fill: looks[0].fill, ..Look::NONE }, NO_FLAGS);
                (hl, k, (p::ROW_HEIGHT * z) as f32)
            }),
            _ => None,
        };
        sh::Ground { node: g, looks, row }
    });
    let painted = ui.box_("card", BoxOpts::default().w(Size::Px(w as f32)).h(Size::Px(h as f32)).at((0.0, 0.0)));
    paint_card(t, ui, &painted, z, fs, slot, snap, footers);
    // what it shows of its records, over its painting
    let readout = crate::scope_hover::readout_nodes(t, ui, z, fs, slot, match &pl.item {
        Item::Item(n) => footers.get(&n.path),
        _ => None,
    });
    // over the painting: the selection's brackets, a scene object's dimming, the recession
    let (bx, by, bw, bh, off, len, wid) = sp::brackets_of(pl, z, w, h);
    let corners = [(bx - off, by - off), (bx + bw + off - len, by - off), (bx - off, by + bh + off - len), (bx + bw + off - len, by + bh + off - len)];
    let brackets: [UiBox; 4] = std::array::from_fn(|k| node(ui, "b", corners[k], (len, len), sh::corner(sh::CORNERS[k], wid as f32, Look::NONE.fill), KEEP_ROOM));
    let mut recede = None;
    if let Item::Item(n) = &pl.item {
        if n.zone.is_none() || pl.collapsed {
            if dimmed(t, n) {
                node(ui, "dim", (0.0, 0.0), (w, h), Look { fill: color::with_alpha(t.theme.panel, 150), ..Look::NONE }, NO_FLAGS);
            }
            recede = Some(node(ui, "recede", (0.0, 0.0), (w, h), sh::recede_look(t, false), KEEP_ROOM));
        }
    }
    ui.leave(saved);
    // an expanded zone's tile covers its body; the wires that end inside it are hit over it
    let mut ports = Vec::new();
    let taps = tile_ports(t, ui, z, fs, pl, &tile, &mut ports);
    // to a screen reader a card is a group named by its title
    let title = match &pl.item {
        Item::Item(n) => p::title(n),
        Item::Input(i) => i.name.clone(),
        Item::Return => "return".to_owned(),
    };
    ui.set_role(&tile, pxui::a11y::Role::Group, 0);
    ui.a11y_name(&tile, &title);
    let fields = card_fields(c, t, ui, z, fs, pl, &tile);
    let _ = (ax, ay);
    built.push(Built { slot, taps, fields });
    (tile, ground, brackets, recede, ports, readout)
}

/// A scene object nothing reads (taken out of its merge) is dimmed; a wire from it brings it back.
fn dimmed(t: &Pane, n: &p::Node) -> bool {
    !n.synthetic
        && n.zone.is_none()
        && n.head.starts_with("scene/")
        && n.head != "scene/merge"
        && n.head != "scene/root"
        && match box_at(t, &n.path) {
            Some((ax, ay, wd, _)) => !t.geo.read.contains(&fkey((ax + wd, ay + WIRE_HEAD_Y))),
            None => false,
        }
}

/// The card's painting over its ground: what the item shows at its level.
fn paint_card(t: &Pane, ui: &mut Ui, on: &UiBox, z: f64, fs: i64, slot: usize, snap: &Rc<Pane>, footers: &HashMap<Path, Footer>) {
    let (pl, ax, ay) = &t.geo.items[slot];
    // the hovered row is the row highlight's (`scope_hover`), but for a carried value over it: the
    // carry builds every frame and marks its target row here
    let rh: Option<usize> = match (hovered_row(t), &t.carry_hot) {
        (Some((path, key)), Some((hot, _))) if path == *hot && path == pl.path => match &pl.item {
            Item::Item(n) => n.rows.iter().position(|r| r.key == key),
            _ => None,
        },
        _ => None,
    };
    let mut footer: Option<Footer> = None;
    let mut lens_step: Option<i64> = None;
    let mut carry = false;
    let mut out_wired = false;
    let mut failed: Option<String> = None;
    let mut probe = 0;
    let mut count = 0;
    if let Item::Item(n) = &pl.item {
        footer = footers.get(&n.path).cloned();
        lens_step = lens_of(t, &n.path);
        carry = matches!(&t.carry_hot, Some((hp, _)) if *hp == n.path);
        out_wired = t.geo.read.contains(&fkey(out_anchor(pl, *ax, *ay)));
        failed = t.failed.iter().find(|(q, _)| *q == n.path).map(|(_, c)| c.clone());
        probe = t.saved.probe(&n.path);
        count = count_of(t, &n.path);
    }
    let snap = snap.clone();
    ui.draw(on, move |paint, rect| {
        let rect = rect.f64();
        let (pl, _, _) = &snap.geo.items[slot];
        match &pl.item {
            Item::Input(_) if pl.shown == Level::Point => sp::paint_point(paint, &snap, z, fs, pl, None, rect),
            Item::Input(i) => sp::paint_input(paint, &snap, z, fs, i, rect),
            Item::Return => {
                if let Some(s) = &snap.scope {
                    sp::paint_return(paint, &snap, z, fs, s, rect);
                }
            }
            Item::Item(n) => match &n.zone {
                Some(zn) if !pl.collapsed => sp::paint_zone_frame(paint, &snap, z, fs, n, zn, out_wired, rect, probe, count),
                _ => {
                    let o = sp::NodeOpts { footer: footer.clone(), lens_step, carry, out_wired, shown: pl.shown, failed: failed.clone() };
                    sp::paint_node(paint, &snap, z, fs, &o, pl, n, rh, rect);
                }
            },
        }
    });
}

/// A payload in flight: the innermost node under the pointer (a zone's tile covers its body), else the
/// canvas, is where it would be put; the host decides whether it takes it.
pub fn drop_change(t: &Pane, ui: &mut Ui, canvas: &UiBox) -> Option<Change> {
    let on_canvas = ui.drop_target(canvas)?;
    let mut found: Option<(usize, Drop)> = None;
    let hovers = t.hovers.clone();
    let hv = hovers.borrow();
    for c in &hv.cards {
        let pl = &t.geo.items[c.slot].0;
        if let Some(d) = ui.drop_target(&c.tile) {
            let skip = match &found {
                Some((q, _)) => {
                    let qp = &t.geo.items[*q].0;
                    qp.w * qp.h <= pl.w * pl.h
                }
                None => false,
            };
            if !skip {
                found = Some((c.slot, d));
            }
        }
    }
    let (path, drop) = match found {
        Some((slot, d)) => (t.geo.items[slot].0.path.clone(), d),
        None => (vec![t.key.clone().into()], on_canvas),
    };
    Some(match drop {
        Drop::Hover(pay) => Change::DropOver { path, kind: pay.kind, value: pay.value },
        Drop::Dropped(pay) => Change::Dropped { path, kind: pay.kind, value: pay.value },
    })
}

/// The open context menu, every frame it is open.
pub fn context_menu(t: &mut Pane, ui: &mut Ui, emit: &mut Vec<Change>) {
    if let Some((at, path)) = t.context.clone() {
        let items = context_items(t, &path);
        match ui.context_menu(p32(at), &MenuOpts::default(), "pxui-scope-context", &items) {
            MenuResult::Open => {}
            MenuResult::Dismiss => { t.context = None; },
            MenuResult::Pick(i) => {
                match t.saved.imported(&path) {
                    Some(file) => emit.push(Change::OpenImport(file)),
                    None => emit.extend(action_changes(t, context_command(i))),
                }
                t.context = None;
            }
        }
    }
}

/// A wire let go: on an input row it connects; on empty canvas it opens the add menu after its source
/// (pulled off an existing wire, the host inserts the new node on the selected wire).
pub fn finish_wire(t: &mut Pane, ui: &mut Ui, z: f64, mouse: (f64, f64), emit: &mut Vec<Change>, w: &Wiring, src_path: &Path, release_point: (f64, f64)) {
    let mut target: Option<(Path, ArgKey)> = None;
    let mut any_hover = false;
    {
        let hovers = t.hovers.clone();
        let hv = hovers.borrow();
        for c in &hv.cards {
            if !ui.hovered_within(&c.tile) {
                continue;
            }
            any_hover = true;
            let (tp, tax, tay) = &t.geo.items[c.slot];
            let (mx, my) = mouse;
            let gx = (mx - sx(t, 0.0)) / z;
            let gy = (my - sy(t, 0.0)) / z;
            let lx = gx - tax;
            let ly = gy - tay;
            let found: Option<(Path, ArgKey)> = match &tp.item {
                Item::Return => t.scope.as_ref().map(|s| {
                    let mut pth = s.path.clone();
                    pth.push("@result".into());
                    (pth, ArgKey::Whole)
                }),
                Item::Input(_) => None,
                Item::Item(n) => match &n.zone {
                    None => {
                        // anywhere on the header is the in-
                        match row_at(f64::INFINITY, tp, n, tp.shown, lx, ly) {
                            Some(i) if i < n.rows.len() => {
                                let r = &n.rows[i];
                                if r.socket { Some((n.path.clone(), r.key.clone())) } else { None }
                            }
                            _ => None,
                        }
                    }
                    Some(zn) if tp.collapsed => {
                        let i = ((ly - p::HEAD_HEIGHT) / p::ROW_HEIGHT) as i64;
                        match if i >= 0 { zn.rail.get(i as usize) } else { None } {
                            Some(r) if r.key.is_some() && ly >= p::HEAD_HEIGHT => Some((n.path.clone(), r.key.clone().unwrap())),
                            _ => None,
                        }
                    }
                    Some(zn) => {
                        let extras = extra_indices(zn);
                        if ly < p::HEAD_HEIGHT {
                            match label_index(zn).map(|li| &zn.rail[li]) {
                                Some(r) if r.key.is_some() && lx < 24.0 => Some((n.path.clone(), r.key.clone().unwrap())),
                                _ => None,
                            }
                        } else if ly < p::rail_top(n) - 28.0 {
                            let i = ((ly - p::HEAD_HEIGHT) / p::ROW_HEIGHT) as i64;
                            match if i >= 0 { extras.get(i as usize).map(|ri| &zn.rail[*ri]) } else { None } {
                                Some(r) if r.key.is_some() && lx < 24.0 => Some((n.path.clone(), r.key.clone().unwrap())),
                                _ => None,
                            }
                        } else if lx > tp.w - 24.0 && ly >= p::rail_top(n) {
                            let mut pth = zn.scope.path.clone();
                            pth.push("@result".into());
                            Some((pth, ArgKey::Whole))
                        } else {
                            None
                        }
                    }
                },
            };
            if found.is_some() {
                target = found;
                break;
            }
        }
    }
    // a wire let go on empty canvas: the add menu there, after the node it came from
    let on_canvas = target.is_none() && src_path.last().is_some_and(|name| name.as_str() == w.src) && !any_hover;
    // an Alt-drag moves the wire: it comes off the inputs it was lifted from
    let lifted: Vec<(Path, ArgKey)> = w.lift.iter().filter(|i| Some((*i).clone()) != target).cloned().collect();
    let off = unwire_at(t, &lifted);
    match &target {
        Some((node, key)) => {
            emit.push(Change::SyntaxEdit(e::Op::Connect { node: node.clone(), key: key.clone(), src: w.src.clone(), iter: w.iter }));
            for op in off {
                emit.push(Change::SyntaxEdit(op));
            }
        }
        None if !w.lift.is_empty() && !any_hover => {
            for op in off {
                emit.push(Change::SyntaxEdit(op));
            }
        }
        None if on_canvas => {
            emit.push(Change::Selected);
            let (x, y) = release_point;
            emit.push(Change::MenuRequested(x, y));
        }
        None => emit.push(Change::Notice("Drop the wire on an input row".to_owned())),
    }
    if on_canvas {
        t.selected.clear();
        t.selected.insert(src_path);
    }
    t.drag = None;
}

/// Whether a move of these cards carries others with them (a zone's): the content builds it.
/// A move let go: the cards (and what they carry) stay where it put them until the geometry has
/// them there (`settle`).
fn dropped(t: &mut Pane, ui: &Ui, dx: f64, dy: f64) {
    let slots = t.moving.iter().filter_map(|q| t.geo.slot.get(q).copied()).collect();
    t.dropped = Some((slots, dx, dy, ui.frame_index()));
}

/// The pointer on the content, before it builds: a press on a card selects it and starts its move, on
/// an out-port starts a wire, on an input with a wire takes it off when clicked, on a wire selects it
/// (pulled, it is a new wire from its source), on the ground starts a rubber band; the moves, wires and
/// bands that follow and their release; a double click and a right click. The canvas holds every
/// press on the content (`scope_hover::classify` says what it is on).
fn gestures(t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, canvas: &UiBox, sig: &Signal, emit: &mut Vec<Change>) {
    use crate::scope_hover::On;
    let z = t.zoom;
    let mouse = frame.input.pointer;
    let left = left_button(sig);
    // the pan's pointer grab (a right or middle drag pans in `view_step`)
    let non_left = sig.button.is_some() && !left;
    let panning = (sig.held || sig.released) && non_left;
    let lost = frame.input.events.iter().any(|ev| matches!(ev, Event::FocusLost));
    let grab = if (sig.pressed && non_left) || panning {
        true
    } else if (sig.released && non_left) || lost || frame.input.buttons.is_empty() {
        false
    } else {
        t.panning_grab
    };
    if grab != t.panning_grab && rays::sketch::running() {
        let _ = rays::sketch::set_relative_mouse(grab);
    }
    t.panning_grab = grab;
    if sig.pressed {
        t.press = Some(crate::scope_hover::classify(t, ui, p64(sig.press_point)));
    }
    let Some(on) = t.press.take() else { return };
    let slot_path = |t: &Pane, slot: usize| t.geo.items[slot].0.path.clone();
    // (the items held apart from the pane, which the gestures change)
    let items = t.geo.items.clone();
    match &on {
        On::Card(slot) => {
            let path = &items[*slot].0.path;
            if sig.pressed && left && t.context.is_none() {
                let additive = ui.press_keys(canvas).contains(&Key::Shift.code());
                if additive {
                    if !t.selected.remove(path) {
                        t.selected.insert(path);
                    }
                } else if !(t.selected.contains(path) && t.selected.iter().all(|q| t.geo.slot.contains_key(q))) {
                    t.selected.clear();
                    t.selected.insert(path);
                }
                emit.push(Change::Selected);
                // the move carries the selection but the cards inside others it holds
                let (moving, sel) = (&mut t.moving, &t.selected);
                moving.assign(sel);
                moving.retain(&mut |q| !sel.iter().any(|a| a.len() < q.len() && q.starts_with(a)));
                (t.selected_wire, t.drag) = (None, Some(Drag::Moving { dx: 0.0, dy: 0.0, moved: false }));
            }
            if sig.double_clicked && left && t.context.is_none() && t.editing.is_none() {
                let (pl, _, ay) = &t.geo.items[*slot];
                let top = sy(t, *ay);
                match &pl.item {
                    Item::Item(n) if !n.synthetic && f64::from(sig.press_point.1) < top + (p::HEAD_HEIGHT * z) => {
                        t.editing = Some(Editing::Name(pl.path.clone()));
                    }
                    // a folded function or loop opens in place; anything else follows what it references
                    Item::Item(n) if n.zone.is_some() && t.saved.collapsed(&pl.path) => {
                        emit.push(Change::ZoneCollapsed { zone: pl.path.clone(), collapsed: false });
                    }
                    Item::Item(n) if !n.synthetic => {
                        emit.push(Change::Activated(pl.path.clone()));
                    }
                    Item::Input(_) => {
                        t.editing = Some(Editing::Default(pl.path.clone()));
                    }
                    _ => {}
                }
            }
            let carried = t.selected.contains(path);
            let mut begin = false;
            if let Some(Drag::Moving { dx, dy, moved }) = &mut t.drag {
                if sig.held && left && carried {
                    let (ddx, ddy) = sig.drag;
                    *dx += f64::from(ddx) / z;
                    *dy += f64::from(ddy) / z;
                    begin = !*moved && dx.hypot(*dy) > 3.0 / z;
                    *moved = *moved || begin;
                }
            }
            if begin {
                crate::scope_hover::begin_move(t);
            }
            if sig.released && left {
                if let Some(Drag::Moving { dx, dy, moved }) = t.drag.take() {
                    if moved {
                        let mv: Vec<(Path, f64, f64)> = t.moving.iter().filter_map(|path| rel_at(t, path).map(|(rx, ry)| (path.clone(), p::snap(rx + dx), p::snap(ry + dy)))).collect();
                        emit.push(Change::Moved(mv));
                        dropped(t, ui, dx, dy);
                    }
                }
            }
        }
        On::Out(slot, src, ty, at) => {
            if sig.pressed && left {
                let iter = match &t.scope {
                    Some(scope) => p::zones(scope).iter().any(|n| match &n.zone {
                        Some(zn) => zn.rail.iter().any(|r| r.role == p::Role::Var && r.names.contains(src)),
                        None => false,
                    }),
                    None => false,
                };
                // Alt on an out-port lifts every wire it feeds
                let path = slot_path(t, *slot);
                let lift: Vec<(Path, ArgKey)> = if !ui.press_keys(canvas).contains(&Key::Alt.code()) {
                    Vec::new()
                } else {
                    let mut acc: Vec<(Path, ArgKey)> = Vec::new();
                    for wi in t.geo.wires.iter() {
                        if let (Some(f), Some((tp, key, _))) = (&wi.from, &wi.target) {
                            if *f == path {
                                acc.insert(0, (tp.clone(), key.clone()));
                            }
                        }
                    }
                    acc
                };
                t.drag = Some(Drag::Wiring(Wiring { src: src.clone(), ty: ty.clone(), iter, from: *at, lift }));
            }
            if sig.released && left {
                match t.drag.take() {
                    Some(Drag::Wiring(wg)) => {
                        let path = slot_path(t, *slot);
                        finish_wire(t, ui, z, mouse, emit, &wg, &path, p64(sig.release_point));
                    }
                    _ => {}
                }
            }
        }
        On::In(slot, del, rect) => {
            let (x, y, w, h) = *rect;
            let inside = contains(Rect::new(x as f32, y as f32, w as f32, h as f32), sig.release_point);
            if sig.clicked && inside {
                let ops: Vec<e::Op> = match (del, &t.geo.items[*slot].0.item) {
                    (Some(Del::Unwire(i)), Item::Item(n)) => unwire(t, n, &n.rows[*i]),
                    (Some(Del::Disconnect(key)), Item::Item(n)) => vec![e::Op::Disconnect { node: n.path.clone(), key: key.clone(), fallback: None }],
                    _ => Vec::new(),
                };
                for op in ops {
                    emit.push(Change::SyntaxEdit(op));
                }
            }
        }
        On::Wire(i) => {
            if sig.pressed && left && t.context.is_none() {
                if !t.selected.is_empty() {
                    emit.push(Change::Selected);
                }
                t.selected.clear();
                t.selected_wire = t.geo.wires[*i].target.clone();
            }
            // a wire pulled past 6 points is a new wire from its source: the same drag as from the out-port
            let (px, py) = p64(sig.pointer);
            let (qx, qy) = p64(sig.press_point);
            if t.drag.is_none() && t.context.is_none() && sig.held && left && (px - qx).hypot(py - qy) > 6.0 {
                let wi = &t.geo.wires[*i];
                if let (Some(tg), Some(from)) = (&wi.target, &wi.from) {
                    let lift = if ui.press_keys(canvas).contains(&Key::Alt.code()) { vec![(tg.0.clone(), tg.1.clone())] } else { Vec::new() };
                    let src = match t.item_at(from).map(|(pl, _, _)| &pl.item) {
                        Some(Item::Item(n)) if n.zone.is_none() && n.binds.len() == 1 && n.binds[0] == n.name => Some(n.name.clone()),
                        Some(Item::Input(inp)) => Some(inp.name.clone()),
                        _ => None,
                    };
                    if let Some(src) = src {
                        t.drag = Some(Drag::Wiring(Wiring { src, ty: wi.ty.clone(), iter: false, from: (sx(t, wi.a.0), sy(t, wi.a.1)), lift }));
                    }
                }
            }
            // ...and let go, it finishes like an out-port's wire
            if sig.released && left && t.selected_wire.is_some() {
                if let Some(Drag::Wiring(wg)) = t.drag.clone() {
                    let src_path = t.geo.wires[*i].from.clone().unwrap_or_default();
                    finish_wire(t, ui, z, mouse, emit, &wg, &src_path, p64(sig.release_point));
                }
            }
        }
        On::Frame(k) => {
            // a frame by its title: a double click edits the title; a drag past 3 points carries it with
            // the nodes of its scope whose centres lie inside it (their views move)
            let Some((origin, index)) = t.hovers.borrow().frames.get(*k).copied() else { return };
            let scope = &t.geo.origins[origin].0;
            let (px, py) = p64(sig.pointer);
            let (qx, qy) = p64(sig.press_point);
            if sig.double_clicked && left && t.editing.is_none() {
                (t.drag, t.editing) = (None, Some(Editing::Title(scope.clone(), index as i64)));
            } else if left && (sig.held || sig.released) && t.editing.is_none() {
                let (dx, dy) = ((px - qx) / z, (py - qy) / z);
                match &mut t.drag {
                    None if sig.held && (px - qx).hypot(py - qy) > 3.0 => {
                        let (ox, oy) = t.geo.origins[origin].1;
                        if let Some((_, (fx, fy), (fw, fh))) = t.saved.frames(scope).get(index).cloned() {
                            let inside = |pl: &Placed, bx: f64, by: f64| {
                                let (cx, cy) = (bx + (pl.w / 2.0), by + (pl.h / 2.0));
                                scope_of_path(&pl.path) == *scope && cx >= ox + fx && cx <= ox + fx + fw && cy >= oy + fy && cy <= oy + fy + fh
                            };
                            t.moving.clear();
                            for (pl, bx, by) in t.geo.items.iter() {
                                if inside(pl, *bx, *by) {
                                    t.moving.insert(&pl.path);
                                }
                            }
                            t.drag = Some(Drag::Carrying { scope: scope.clone(), index: index as i64, dx, dy });
                            crate::scope_hover::begin_move(t);
                        }
                    }
                    Some(Drag::Carrying { dx: cx, dy: cy, .. }) => (*cx, *cy) = (dx, dy),
                    _ => {}
                }
                if sig.released {
                    if let Some(Drag::Carrying { scope, dx, dy, .. }) = &t.drag {
                        if dx.hypot(*dy) > 3.0 / z {
                            let (scope, dx, dy) = (scope.clone(), *dx, *dy);
                            let mv: Vec<(Path, f64, f64)> = t.moving.iter().filter_map(|path| rel_at(t, path).map(|(rx, ry)| (path.clone(), p::snap(rx + dx), p::snap(ry + dy)))).collect();
                            emit.push(Change::Moved(mv));
                            emit.push(Change::FramesSet { scope: scope.clone(), frames: crate::scope_commands::frame_list(t, &scope) });
                            dropped(t, ui, dx, dy);
                        } else {
                            crate::scope_hover::end_move(t);
                        }
                        t.drag = None;
                    }
                }
            }
        }
        On::Ground => {
            if sig.pressed && left && t.context.is_none() {
                let additive = ui.press_keys(canvas).contains(&Key::Shift.code());
                if !additive && !t.selected.is_empty() {
                    emit.push(Change::Selected);
                }
                if !additive {
                    t.selected.clear();
                }
                t.marquee_base.assign(&t.selected);
                t.marquee.reserve(t.geo.items.len());
                (t.selected_wire, t.drag) = (None, Some(Drag::Marquee {}));
            }
        }
    }
    // the rubber band selects the nodes of one scope it touches
    if let Some(Drag::Marquee {}) = &t.drag {
        if sig.held || sig.released {
            let (px, py) = if sig.released { p64(sig.release_point) } else { p64(sig.pointer) };
            let (x0, y0) = p64(sig.press_point);
            let rw = (px - x0).abs();
            let rh = (py - y0).abs();
            let mut hits = std::mem::take(&mut t.marquee);
            if rw < 3.0 && rh < 3.0 {
                hits.clear();
            } else {
                marquee_hits(t, (x0.min(px), y0.min(py), rw, rh), &mut hits);
            }
            // the selection it makes is the base and the hits: built only when it differs
            let (items, base) = (&t.geo.items, &t.marquee_base);
            let path = |k: usize| &items[k].0.path;
            let more = hits.iter().filter(|&&k| !base.contains(path(k))).count();
            let same = base.len() + more == t.selected.len() && base.iter().all(|q| t.selected.contains(q)) && hits.iter().all(|&k| t.selected.contains(path(k)));
            if !same {
                t.selected.assign(base);
                for &k in &hits {
                    t.selected.insert(path(k));
                }
                emit.push(Change::Selected);
            }
            t.marquee = hits;
            if sig.released {
                t.drag = None;
            }
        }
    }
    // a right click: a card's menu, or the add menu on the ground
    if t.context.is_none() && context_clicked(sig) {
        match &on {
            On::Card(slot) => {
                let path = slot_path(t, *slot);
                // the menu acts on the nodes: a wire selected before is no longer the selection
                if !t.selected.contains(&path) {
                    t.selected.clear();
                    t.selected.insert(&path);
                    emit.push(Change::Selected);
                }
                (t.context, t.selected_wire) = (Some((p64(sig.release_point), path)), None);
            }
            On::Ground => {
                let (x, y) = p64(sig.release_point);
                emit.push(Change::MenuRequested(x, y));
            }
            _ => {}
        }
    }
    if !sig.released {
        t.press = Some(on);
    }
}

/// The content's own controls on the cards built this frame: what their fields asked (a read name or an
/// expression selects what it names), the zone and lens controls.
fn card_gestures(t: &mut Pane, ui: &mut Ui, built: &[Built], emit: &mut Vec<Change>) -> (Vec<(Path, i64)>, bool) {
    let mut lens_next: Vec<(Path, i64)> = t.lens.clone();
    let mut lens_changed = false;
    for b in built {
        let geo = t.geo.items.clone();
        let (pl, _, _) = &geo[b.slot];
        emit.extend(b.fields.iter().cloned());
        // zone controls
        for (bx, on) in &b.taps {
            let bs = ui.signal(bx);
            if let Item::Item(n) = &pl.item {
                match on {
                    Tap::Toggle if bs.clicked => emit.push(Change::ZoneCollapsed { zone: n.path.clone(), collapsed: !pl.collapsed }),
                    Tap::Replace if bs.clicked => emit.push(Change::SyntaxEdit(e::Op::InlineMacro { node: n.path.clone() })),
                    Tap::Lens if bs.clicked => {
                        lens_changed = true;
                        if lens_next.iter().any(|(q, _)| *q == n.path) {
                            lens_next.retain(|(q, _)| *q != n.path);
                        } else {
                            let step = match &n.lens {
                                Some(l) => l.steps.len() as i64 - 1,
                                None => 0,
                            };
                            lens_next.insert(0, (n.path.clone(), step));
                        }
                    }
                    Tap::LensStep(i) if bs.clicked => {
                        lens_changed = true;
                        lens_next.retain(|(q, _)| *q != n.path);
                        lens_next.insert(0, (n.path.clone(), *i));
                    }
                    Tap::Step(d) if bs.clicked => {
                        let count = count_of(t, &n.path);
                        let index = 0.max((count - 1).min(t.saved.probe(&n.path) + d));
                        if count > 0 && index != t.saved.probe(&n.path) {
                            emit.push(Change::ProbeSet { zone: n.path.clone(), index });
                        }
                    }
                    Tap::Track if bs.pressed || bs.held => {
                        let count = count_of(t, &n.path);
                        let (x, _, wd, _) = ui.rect(bx).f64();
                        if count > 0 && wd > 0.0 {
                            let px = if bs.released { f64::from(bs.release_point.0) } else { f64::from(bs.pointer.0) };
                            let index = 0.max((count - 1).min((((px - x) / wd) * count as f64) as i64));
                            if index != t.saved.probe(&n.path) {
                                emit.push(Change::ProbeSet { zone: n.path.clone(), index });
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    (lens_next, lens_changed)
}

pub fn frames_step(t: &mut Pane, ui: &mut Ui, ctx: &mut Ctx, z: f64, emit: &mut Vec<Change>) {
    // frames: a title strip, a delete cross and a resize corner, over the tiles (a zone's tile covers its
    // whole body) and clear of the nodes, which keep 12 points inside the frame
    struct FrameBox {
        scope: Path,
        i: usize,
        title: String,
        rect: (f64, f64, f64, f64),
        xb: UiBox,
        rb: UiBox,
    }
    let mut frame_boxes: Vec<FrameBox> = Vec::new();
    for (scope, (ox, oy)) in t.geo.origins.iter() {
        for (i, (title, (fx, fy), (fw, fh))) in t.saved.frames(scope).iter().enumerate() {
            let x = sx(t, ox + fx);
            let y = sy(t, oy + fy);
            let wd = fw * z;
            let h = fh * z;
            let mk = |ui: &mut Ui, flags: Flags, k: &str, pos: (f64, f64), size: (f64, f64)| {
                box_in(ui, flags, size.0, size.1, ctx.local(t, pos), &format!("fr:{}:{}:{}", flow_syntax::symbol::join_path(&scope, "/"), i, k))
            };
            // the title strip takes no input of its own: the canvas carries the frame from it
            // (`gestures`), so a carry builds nothing
            let tb = mk(ui, BLOCKING | HOVER_FREE, "t", (x, y), ((wd - 20.0).max(8.0), 18.0 * z));
            let xb = mk(ui, CLICKABLE, "x", (x + wd - 18.0, y + 2.0), (16.0, 16.0));
            let rb = mk(ui, CLICKABLE, "r", (x + wd - 12.0, y + h - 12.0), (12.0, 12.0));
            if !ui.verifying() {
                let mut hv = t.hovers.borrow_mut();
                let k = hv.frame_titles.len();
                hv.keys.push(tb.box_key);
                hv.frame_titles.push((tb.box_key, k));
            }
            frame_boxes.push(FrameBox { scope: scope.clone(), i, title: title.clone(), rect: (x, y, wd, h), xb, rb });
        }
    }
    for fb in frame_boxes {
        let FrameBox { scope, i, title, rect: (x, y, wd, _), xb, rb } = fb;
        let xs = ui.signal(&xb);
        let rs = ui.signal(&rb);
        let all = t.saved.frames(&scope);
        if let Some(Editing::Title(sp_, k)) = &t.editing {
            if *sp_ == scope && *k == i as i64 {
                let all2 = all.clone();
                let scope2 = scope.clone();
                let at = ctx.local(t, (x + 4.0, y + 2.0));
                let ch = edit_field(ctx, ui, at, (wd - 24.0).max(40.0), 16.0 * z, &format!("frt{}", i), &title, &|s: &str| !s.is_empty() && !s.contains('"'), &move |v| {
                    Change::FramesSet {
                        scope: scope2.clone(),
                        frames: all2.iter().enumerate().map(|(j, f)| if j == i { (v.clone(), f.1, f.2) } else { f.clone() }).collect(),
                    }
                });
                emit.extend(ch);
            }
        }
        if xs.clicked {
            emit.push(Change::FramesSet { scope: scope.clone(), frames: all.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, f)| f.clone()).collect() });
        } else if rs.pressed && left_button(&rs) {
            { t.drag = Some(Drag::Sizing { scope: scope.clone(), index: i as i64, dw: 0.0, dh: 0.0 }); };
        } else {
            match t.drag.clone() {
                Some(Drag::Sizing { scope: ss, index, dw, dh }) if ss == scope && index == i as i64 && (rs.held || rs.released) => {
                    let (dx, dy) = p64(rs.drag);
                    { t.drag = Some(Drag::Sizing { scope: scope.clone(), index: i as i64, dw: dw + (dx / z), dh: dh + (dy / z) }); };
                    if rs.released {
                        emit.push(Change::FramesSet { scope: scope.clone(), frames: frame_list(t, &scope) });
                        t.drag = None;
                    }
                }
                _ => {}
            }
        }
    }
}

/// Measure with the UI's fonts, remembering the width for the pane's outside-a-frame queries.
fn measure_ui(ui: &mut Ui, t: &Pane, size: i64, text: &str) -> f64 {
    let v = f64::from(ui.text_width(Some(size), text));
    t.measured.borrow_mut().insert(text_key(size, text), v);
    v
}

type Marks = Vec<((f64, f64, f64, f64), Option<String>, Option<bool>)>;

/// Over the cards, in the content: a carried value's places and the one under it, the letter hints.
fn paint_over(t: &Pane, ui: &mut Ui, z: f64, host: &UiBox, visible: &[usize]) {
    let mut marks: Marks = Vec::new();
    if !(t.carry_lit.is_empty() && t.carry_hot.is_none()) {
        for &slot in visible {
            let (pl, ax, ay) = &t.geo.items[slot];
            let rect = (sx(t, *ax), sy(t, *ay), pl.w * z, pl.h * z);
            let letter = t.carry_lit.iter().find(|(q, _)| *q == pl.path).map(|(_, l)| l.clone());
            match (&letter, &t.carry_hot) {
                (_, Some((hot, ok))) if *hot == pl.path => marks.push((rect, letter.clone(), Some(*ok))),
                (Some(l), _) => marks.push((rect, Some(l.clone()), None)),
                _ => {}
            }
        }
    }
    let canvas_hot: Option<bool> = match &t.carry_hot {
        Some((q, ok)) if q.len() == 1 && q[0] == t.key => Some(*ok),
        _ => None,
    };
    if marks.is_empty() && canvas_hot.is_none() && t.hinting.is_none() {
        return;
    }
    let font_size = ui.font_size();
    let snap = Rc::new(t.clone());
    // over the cards, in the content box (it moves with them); what is the pane's is the pane's rectangle
    let pane = (t.x as f64, t.y as f64, t.width as f64, t.height as f64);
    ui.draw_over(host, move |paint, _| {
        let (cx, cy, cw, ch) = pane;
        let refused = theme::invalid();
        let hint_chip = |paint: &mut pxui::ui::Paint, bg: color::T, at: (f64, f64), letter: &str| {
            let letter = letter.to_uppercase();
            let size = 8.max(font_size - 2);
            let lw = 14.0f64.max(f64::from(paint.text_width(Some(size), &letter)) + 6.0);
            paint.fill((at.0 - 7.0) as f32, (at.1 - 7.0) as f32, lw as f32, 14.0, bg);
            let tw = f64::from(paint.text_width(Some(size), &letter));
            let ty = f64::from(paint.text_top(Some(size), (at.1 - 7.0) as f32, 14.0));
            paint.text_s(((at.0 - 7.0 + ((lw - tw) / 2.0)) as f32, ty as f32), size, snap.theme.foreground, &letter);
        };
        if let Some(ok) = canvas_hot {
            paint.dashed_rect((cx + 1.5) as f32, (cy + 1.5) as f32, (cw - 3.0) as f32, (ch - 3.0) as f32, if ok { snap.theme.accent } else { refused });
        }
        for ((x, y, w, h), letter, hot) in &marks {
            let color = match hot {
                Some(false) => refused,
                _ => snap.theme.accent,
            };
            // a drop target: a dashed accent edge on the card
            if hot.is_some() {
                paint.dashed_rect((x + 0.5) as f32, (y + 0.5) as f32, (w - 1.0) as f32, (h - 1.0) as f32, color);
            } else {
                paint.stroke((x + 0.5) as f32, (y + 0.5) as f32, (w - 1.0) as f32, (h - 1.0) as f32, color);
            }
            // a letter of the key route: the hint colour under ink, at the card's top left (-7, -7)
            if let Some(letter) = letter {
                hint_chip(paint, if *hot == Some(false) { refused } else { crate::scope_paint::ports(&snap.theme).hint }, (*x, *y), letter);
            }
        }
        // letter hints: a chip per target still matching what was typed, the rest of its letters shown
        if let Some(h) = &snap.hinting {
            for tg in &h.targets {
                if tg.label.starts_with(&h.typed) {
                    let rest = &tg.label[h.typed.len()..];
                    hint_chip(paint, crate::scope_paint::ports(&snap.theme).hint, (sx(&snap, tg.pos.0), sy(&snap, tg.pos.1)), rest);
                }
            }
        }
    });
}

/// A point's box is as wide as its name, drawn at the zoom's font: measured again when the font
/// changes.
fn refont(t: &mut Pane, ui: &mut Ui) {
    if let Some(scope) = t.scope.clone() {
        let fs0 = font_of(t.zoom);
        if fs0 != t.geo_fs || t.geo_cold {
            let points: Vec<String> = t.geo.items.iter().filter(|(pl, _, _)| pl.shown == Level::Point).map(|(pl, _, _)| point_name(pl)).collect();
            if !points.is_empty() {
                for name in &points {
                    measure_ui(ui, t, fs0, name);
                }
                let layout = t.layout.clone();
                regeo(t, &scope, &layout, &no_shift);
            } else {
                (t.geo_fs, t.geo_cold) = (fs0, false);
            }
        }
    }
}

/// The overscan of a cached pane's content: half the pane past each side, at most 400 points.
pub fn overscan(t: &Pane) -> (f64, f64) {
    ((t.width as f64 / 2.0).min(400.0).round(), (t.height as f64 / 2.0).min(400.0).round())
}

/// The pane's view, before anything is built: a non-left drag on the bare canvas pans, the wheel (a
/// tenth a step) and a pinch zoom at the pointer. The content follows by its view's transform.
fn view_step(t: &mut Pane, sig: &Signal, frame: &rframe::Frame) {
    if (sig.held || sig.released) && sig.button.is_some_and(|b| u32::from(b) != MouseButton::LeftButton.code()) {
        let (dx, dy) = p64(sig.drag);
        (t.pan_x, t.pan_y) = (t.pan_x + dx, t.pan_y + dy);
    }
    let (_, wheel) = p64(sig.scroll);
    if !((wheel == 0.0 && sig.pinch == 1.0) || t.context.is_some()) {
        let (mx, my) = frame.input.pointer;
        let zoom = 0.25f64.max(2.5f64.min(t.zoom * (1.0 + (wheel * 0.1)) * sig.pinch));
        let k = zoom / t.zoom;
        let pan_x = mx - t.x as f64 - ((mx - t.x as f64 - t.pan_x) * k);
        let pan_y = my - t.y as f64 - ((my - t.y as f64 - t.pan_y) * k);
        (t.zoom, t.pan_x, t.pan_y, t.zoom_t) = (zoom, pan_x, pan_y, frame.input.t);
    }
}

/// The canvas: the bare ground's hit box (presses, pans, the wheel), clipping what it holds. It takes
/// the presses on the cards, ports and wires too (they are `BLOCKING`, not clickable), so a press, a
/// drag or a selection on the content builds none of it.
fn canvas_box(t: &Pane, ui: &mut Ui) -> UiBox {
    ui.box_(
        "pxui-scope",
        BoxOpts::default().flags(CLICKABLE | SCROLL | CLIP | BLOCKING | CAPTURE_UNCLAIMED).w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((t.x as f32, t.y as f32)),
    )
}

/// The pan and zoom a build of the content used: those of the kept pane.
fn anchor_of(t: &Pane) -> Option<(f64, f64, f64)> {
    t.built.borrow().as_ref().map(|(last, _)| (last.pan_x, last.pan_y, last.zoom))
}

/// Cards a move let go of stay where it put them (their views' offsets) until the geometry has them
/// there: the host's move arrives with the next frame; with no host, the pane moves them itself then.
fn settle(t: &mut Pane, ui: &Ui) {
    let Some((slots, dx, dy, at)) = &t.dropped else { return };
    let built = t.hovers.borrow().geo == (Rc::as_ptr(&t.geo.items) as *const u8 as usize, Rc::as_ptr(&t.geo.wires) as *const u8 as usize);
    if built && ui.frame_index() <= *at {
        return;
    }
    if built {
        if let Some(scope) = t.scope.clone() {
            let (dx, dy) = (*dx, *dy);
            let moving = PathSet::of(&slots.iter().map(|&k| t.geo.items[k].0.path.clone()).collect::<Vec<_>>());
            let layout = t.layout.clone();
            regeo(t, &scope, &layout, &|q: &[Symbol]| if moving.contains(q) { (dx, dy) } else { (0.0, 0.0) });
        }
    }
    t.dropped = None;
    crate::scope_hover::end_move(t);
}

/// `update` with the canvas in the pane root (built every frame: one box) and everything on it in
/// cached views, in paint order: the wires (grid, zones, frames, wires), a move's wires, the content
/// (one view of the cards, each card a view of its own), a dragged wire or rubber band, and the hover's
/// view. The content builds again only when the pane differs from what its last build left
/// (`scope_state::revision`: not the pan, zoom, hover, selection, a move of cards, a dragged wire or
/// band) or input reaches a widget on it (a field, a zone's control); the wires when that or the
/// selection's chains change. The pointer's gestures run before anything builds (`gestures`), from
/// the boxes the last build left. Kept, the pane is unchanged and no changes are emitted. The anchored
/// views hold the pane and an overscan past each side (`overscan`) at the pan and zoom they were built
/// at; a pan offsets them and a zoom scales them about the pan (`view_offset`, `view_transform`: hit
/// testing follows), so a drag or a wheel on the canvas builds nothing. They build again at the new pan
/// and zoom once the pane would show past their overscan, and once the zoom has rested 0.1 s (text
/// shaped and strokes snapped at the zoom's own size).
pub fn update_cached(label: &str, pressed: &[Key], t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, changes: &mut Vec<Change>) {
    pane(label, 0, pressed, t, ui, frame, changes);
}

/// The graph canvas for one frame, every view of it built; its canvas box.
pub fn update(pressed: &[Key], t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, changes: &mut Vec<Change>) -> UiBox {
    pane("graph-view", ui.frame_index(), pressed, t, ui, frame, changes)
}

/// The pane's views; with a `salt` changing every frame none is kept.
fn pane(label: &str, salt: u64, pressed: &[Key], t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, changes: &mut Vec<Change>) -> UiBox {
    use std::hash::{Hash, Hasher};
    // the first view shows the whole graph, at zoom 1 when it fits (Home frames everything)
    if !t.framed {
        frame_all(t);
        t.framed = true;
    }
    let canvas = canvas_box(t, ui);
    let sig = ui.signal(&canvas);
    view_step(t, &sig, frame);
    settle(t, ui);
    let ov = overscan(t);
    let (w, h) = (t.width as f64, t.height as f64);
    // rebuilt at the pan and zoom now: the zoom rested, or the pane shows past the content's edge
    let moved = match anchor_of(t) {
        None => true,
        Some((bx, by, bz)) => {
            let k = t.zoom / bz;
            // the pane's corners in the content's points (pane-relative) as it was built
            let back = |q: f64, pan: f64, built: f64| built + ((q - pan) / k);
            (t.zoom != bz && frame.input.t - t.zoom_t >= 0.1)
                || back(0.0, t.pan_x, bx) < -ov.0
                || back(w, t.pan_x, bx) > w + ov.0
                || back(0.0, t.pan_y, by) < -ov.1
                || back(h, t.pan_y, by) > h + ov.1
        }
    };
    if t.zoom != anchor_of(t).map_or(t.zoom, |a| a.2) && !moved {
        ui.repaint_after(0.1);
    }
    if moved {
        // at the zoom it builds at, before the revision reads the geometry
        refont(t, ui);
    }
    step_hints(t, pressed, sig.subtree_press.is_some(), changes);
    // what the pointer is over, from the boxes the last build left (a carry's target row is the build's)
    let now = crate::scope_hover::find(t, ui, &canvas, frame.input.pointer);
    gestures(t, ui, frame, &canvas, &sig, changes);
    context_menu(t, ui, changes);
    // a cook's values on the cards that show them; what no patch can show builds the content again
    if !crate::scope_hover::cooked(t, ui) {
        t.cook += 1;
    }
    // a card's number dragged and followed by patches (below): the content stays as built while the
    // pane differs from it only by the arguments the drag wrote; the release builds it as it is now
    // ponytail: only a host that writes the drag by `with_arguments` keeps it (the editor's literal
    // path: catalog nodes); one it lays out again (`with_scope`: a draw node's argument) builds the
    // content every frame as before; diff the projections in `sync_scope` if those scrubs matter
    let held = !moved && held_drag(t, ui);
    let kept_rev = t.built.borrow().as_ref().filter(|(last, _)| held && HELD.with(|h| h.borrow().as_ref().is_some_and(|h| h.patched)) && same_but_arguments(last, t)).map(|(_, rev)| *rev);
    let rev = if moved { revision(t).wrapping_add(1) } else { kept_rev.unwrap_or_else(|| revision(t)) };
    // a carried payload's places follow the pointer: the content builds every frame of a carry
    if ui.carrying().is_some() {
        t.busy_t = frame.input.t;
    }
    let mut hh = std::hash::DefaultHasher::new();
    (rev, t.busy_t.to_bits(), salt).hash(&mut hh);
    let key = hh.finish();
    let (cw, ch) = (Size::Px((w + 2.0 * ov.0) as f32), Size::Px((h + 2.0 * ov.1) as f32));
    // what the views build at this key shows: the cards, wires, open zones and frames in view
    crate::scope_hover::list(t, ov, key);
    let chain = chains_of(t);
    let selected = crate::scope_hover::selected_wire_index(t);
    // the wires a move draws in its own view
    crate::scope_hover::carry_wires(t);
    let saved = ui.enter(&canvas);
    // the grid, the zones, the frames and the wires, under the cards: they build again with the
    // content and when the selection's chains or the selected wire change
    let mut hh = std::hash::DefaultHasher::new();
    (key, chain.as_ref().map(|c| (Rc::as_ptr(c) as usize, c.rev)), selected, &t.highlighted, &t.hovers.borrow().skip).hash(&mut hh);
    let wires = ui.view("graph-wires", hh.finish(), cw, ch, &mut |ui| crate::scope_hover::wires_view(t, ui, key, cw, ch, &chain, selected));
    ui.set_at(&wires, (-ov.0 as f32, -ov.1 as f32));
    // the wires a move of cards carries, where it puts them
    let under = ui.view("graph-drag-wires", crate::scope_hover::drag_wires_key(t) ^ salt, Size::Px(w as f32), Size::Px(h as f32), &mut |ui| crate::scope_hover::drag_wires(t, ui));
    ui.set_at(&under, (0.0, 0.0));
    let (mut built, mut verified) = (None, false);
    let view = ui.view(label, key, cw, ch, &mut |ui| {
        verified = ui.verifying();
        // a verifying run of the kept view builds as the kept build did
        let now = (t.pan_x, t.pan_y, t.zoom);
        if ui.verifying() {
            if let Some(a) = anchor_of(t) {
                (t.pan_x, t.pan_y, t.zoom) = a;
            }
        }
        let before = t.clone();
        let at = content(t, ui, &canvas, ov, key, changes);
        // a build that changed the pane (a field's click selected) painted part of it from the state
        // before: it is kept as of before, so the next frame builds again from the state now
        let settled = builds_same(&before, t);
        if !settled {
            ui.request_frame();
        }
        if ui.verifying() {
            (t.pan_x, t.pan_y, t.zoom) = now;
        }
        built = Some((at, if settled { None } else { Some(before) }));
    });
    ui.set_at(&view, (-ov.0 as f32, -ov.1 as f32));
    let fresh = built.is_some() && !verified;
    if let Some((at, before)) = built {
        let mut kept = before.unwrap_or_else(|| t.clone());
        (kept.pan_x, kept.pan_y, kept.zoom) = at;
        keep_built(&kept, rev);
    }
    if held && fresh {
        HELD.with(|h| h.borrow_mut().iter_mut().for_each(|h| h.patched = false));
    } else if held {
        // a verifying run of the kept content wrote the drag's change itself; it shows the patch
        let edit = follow_drag(ui);
        if !verified {
            changes.extend(edit);
        }
    }
    // a dragged wire or rubber band, over the cards
    let (mouse, press) = (frame.input.pointer, p64(sig.press_point));
    let marks = ui.view("graph-drag-marks", crate::scope_hover::drag_marks_key(t, mouse, press) ^ salt, Size::Px(w as f32), Size::Px(h as f32), &mut |ui| crate::scope_hover::drag_marks(t, ui, mouse, press));
    ui.set_at(&marks, (0.0, 0.0));
    // the anchored views where the pan and zoom now put them
    // (about the window's corner: a built point p shows at k (p + o), the pane's pan point where the
    // build had it scaled by k about it; with k one, p + o exactly)
    let anchor = anchor_of(t);
    if let Some((bx, by, bz)) = anchor {
        let k = t.zoom / bz;
        let shift = |o: f64, pan: f64, built: f64| if k == 1.0 { pan - built } else { (o + pan) / k - (o + built) };
        for v in [&wires, &view] {
            ui.view_offset(v, shift(t.x as f64, t.pan_x, bx) as f32, shift(t.y as f64, t.pan_y, by) as f32);
            ui.view_transform_about(v, k as f32, [1.0, 0.0, 0.0, 1.0], (0.0, 0.0));
        }
    }
    // a move of cards: their views offset; the hover's and the selection's looks through the patch
    // path, and over the cards a view of the hover that builds only when what it shows changes
    crate::scope_hover::place_cards(t, ui, anchor.map_or(t.zoom, |a| a.2));
    crate::scope_hover::show(t, ui, &now);
    let font_size = ui.font_size();
    let over = ui.view("graph-hover", crate::scope_hover::overlay_key(t, &now, key) ^ salt, Size::Px(w as f32), Size::Px(h as f32), &mut |ui| crate::scope_hover::overlay(t, ui, &now, font_size));
    ui.set_at(&over, (0.0, 0.0));
    ui.leave(saved);
    canvas
}

/// The cards on the canvas, in a content box `ov` past each side of the pane, and the gestures of the
/// widgets on them. The pan and zoom it was built at.
fn content(t: &mut Pane, ui: &mut Ui, canvas: &UiBox, ov: (f64, f64), rev: u64, changes: &mut Vec<Change>) -> (f64, f64, f64) {
    let at = (t.pan_x, t.pan_y, t.zoom);
    // at the pane's corner, so its children land where they would on the canvas (to the bit)
    let host = ui.box_("pxui-scope-content", BoxOpts::default().w(Size::Px(t.width as f32)).h(Size::Px(t.height as f32)).at((ov.0 as f32, ov.1 as f32)));
    let saved = ui.enter(&host);
    refont(t, ui);
    let z = t.zoom;
    // text follows the zoom down to 5 points (a 6-point row at zoom 0.25 holds it)
    let fs = font_of(z);
    // the cards in view (`scope_hover::list`)
    let visible = t.hovers.borrow().visible.clone();
    // footers: only for what is in view, and not when too small to read
    let mut footers: HashMap<Path, Footer> = HashMap::new();
    if let Some(records) = &t.records {
        if z >= 0.4 {
            for &i in &visible {
                let (pl, _, _) = &t.geo.items[i];
                if let Item::Item(n) = &pl.item {
                    if if n.zone.is_some() { pl.collapsed } else { true } {
                        footers.insert(n.path.clone(), footer_of(t, records, n));
                    }
                }
            }
        }
    }
    // the cards, each a view of its own; what the hover and the gestures read between builds (a
    // verifying run leaves the kept build's)
    let snap = Rc::new(t.clone());
    let mut ctx = Ctx { finished: false, opened: None, fs, ov, select: None };
    let mut built: Vec<Built> = Vec::new();
    if !ui.verifying() {
        t.hovers.borrow_mut().begin(t);
    }
    for &i in &visible {
        build_card(&mut ctx, t, ui, z, fs, i, &snap, &footers, rev, &mut built);
    }
    if !ui.verifying() {
        t.hovers.borrow_mut().end(t, ui);
    }
    if ctx.finished {
        t.editing = None;
    }
    if let Some(c) = drop_change(t, ui, canvas) {
        changes.push(c);
    }
    frames_step(t, ui, &mut ctx, z, changes);
    if ctx.finished { t.editing = None; }
    if let Some(editing) = ctx.opened.take() { t.editing = Some(editing); }
    let (lens_next, lens_changed) = card_gestures(t, ui, &built, changes);
    if let Some(paths) = ctx.select.take() {
        select(paths, t);
    }
    paint_over(t, ui, z, &host, &visible);
    let drawn_zones = visible.iter().filter(|&&i| matches!(&t.geo.items[i].0.item, Item::Item(n) if n.zone.is_some()) && !t.geo.items[i].0.collapsed).count() as i64;
    t.stats = Stats { drawn_items: visible.len() as i64, drawn_zones, ..t.stats };
    // an opened or stepped expansion panel changes its card's size: lay out again for the next frame
    if lens_changed {
        t.lens = lens_next;
        relayout(None, t);
    }
    ui.leave(saved);
    at
}

#[doc(hidden)]
#[path = "scope_frame_inspect_hooks.rs"]
mod inspect_hooks;

// ------------------------------------------------------------- test hooks

pub fn selected_wire(t: &Pane) -> Option<(Path, ArgKey)> {
    t.selected_wire.as_ref().map(|(path, key, _)| (path.clone(), key.clone()))
}

pub mod inspect {
    use super::*;

    pub fn grabbed(t: &Pane) -> bool {
        t.panning_grab
    }

    /// Where an item shows, in window points: its box, moved by a move dragging it (let go, the
    /// geometry has it where the move put it or will have it the next frame).
    pub fn box_of(t: &Pane, path: &[Symbol]) -> Option<(f64, f64, f64, f64)> {
        let carried = crate::scope_hover::moving_fast(t) && t.geo.slot.get(path).is_some_and(|k| t.hovers.borrow().moving.binary_search(k).is_ok());
        let (dx, dy) = crate::scope_hover::drag_offset(t).filter(|_| carried).unwrap_or((0.0, 0.0));
        box_at(t, path).map(|(x, y, w, h)| (sx(t, x + dx), sy(t, y + dy), w * t.zoom, h * t.zoom))
    }

    /// the nodes whose footer the pane draws: in view and not too small to read. A zone's run count reads
    /// the geometry its calls take, so while an open zone is in view every geometry node of the scope is
    /// counted, on screen or not.
    pub fn footer_paths(t: &Pane) -> Vec<Path> {
        if t.zoom < 0.4 {
            return Vec::new();
        }
        let z = t.zoom;
        let mut zone_open = false;
        let mut seen: Vec<Path> = Vec::new();
        for i in index_query(&t.geo.tiles, (-t.pan_x / z, -t.pan_y / z, (t.width as f64 - t.pan_x) / z, (t.height as f64 - t.pan_y) / z)) {
            let (pl, _, _) = &t.geo.items[i];
            if let Item::Item(n) = &pl.item {
                if if n.zone.is_some() { pl.collapsed } else { true } {
                    seen.push(n.path.clone());
                } else {
                    zone_open = true;
                }
            }
        }
        if !zone_open {
            return seen;
        }
        // Geometry reports precede other reports, with later items first within that priority.
        let mut out: Vec<Path> = Vec::new();
        for (pl, _, _) in t.geo.items.iter() {
            if let Item::Item(n) = &pl.item {
                if n.zone.is_none() && n.ty == flow_syntax::ty::geometry() {
                    out.insert(0, n.path.clone());
                }
            }
        }
        out.extend(seen);
        out
    }

    #[doc(hidden)]
    pub use super::inspect_hooks::*;
}

/// A tile-relative port or tap box: `pos` and `size` in card points, scaled by the zoom.
fn bxz(ui: &mut Ui, z: f64, flags: Flags, name: &str, pos: (f64, f64), size: (f64, f64)) -> UiBox {
    bxzf(ui, z, flags, format_args!("{name}"), pos, size)
}

/// `bxz` with a formatted name (nothing allocated for it).
fn bxzf(ui: &mut Ui, z: f64, flags: Flags, name: std::fmt::Arguments, pos: (f64, f64), size: (f64, f64)) -> UiBox {
    ui.box_fmt(name, BoxOpts::default().flags(flags | HOVER_FREE).w(Size::Px((size.0 * z) as f32)).h(Size::Px((size.1 * z) as f32)).at(((pos.0 * z) as f32, (pos.1 * z) as f32)))
}
