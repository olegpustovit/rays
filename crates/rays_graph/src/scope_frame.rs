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
use rays_math::color;
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
    sg.button == Some(MouseButton::LeftButton)
}

/// The number field of a row, the kit's (`param::number`, as `Rays_shell.Kit.number`): the kind is the
/// row's type, and only a literal with no typed row behind it is read by its spelling. `Some text` the
/// frame the number changed.
pub fn num_field(ui: &mut Ui, at: (f64, f64), w: f64, h: f64, size: Option<i64>, kind: Option<Kind>, range: Option<(f64, f64)>, nonfinite: bool, label: &str, text: &str) -> Option<String> {
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
    let valid: Rc<dyn Fn(&str) -> bool> = Rc::new(move |s: &str| match s::number_value(s) {
        Some(f) => f.is_finite() || nonfinite,
        None => false,
    });
    let mut o = ValueFieldOpts::default();
    o.size = size;
    o.fraction = range.and_then(|r| param::number::fraction(r, text));
    o.scrub = Some(Rc::new(move |origin, dx, fine| param::number::scrub(kind, range, origin, dx, fine)));
    let text2 = ui.value_field(at, w, h, &o, &*valid, label, text).0;
    if text2 != text && valid(&text2) { Some(text2) } else { None }
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

/// The nodes of one scope a rubber band touches (an expanded zone only when it is inside it).
pub fn marquee_hits(t: &Pane, r: (f64, f64, f64, f64)) -> PathSet {
    let (rx, ry, rw, rh) = r;
    let z = t.zoom;
    let mut hits: Vec<Path> = Vec::new();
    for (pl, ax, ay) in t.geo.items.iter() {
        if let Item::Item(_) = pl.item {
            let (x, y, w, h) = (sx(t, *ax), sy(t, *ay), pl.w * z, pl.h * z);
            let touches = x < rx + rw && x + w > rx && y < ry + rh && y + h > ry;
            let inside = x >= rx && y >= ry && x + w <= rx + rw && y + h <= ry + rh;
            if ((pl.inner.is_none() || pl.collapsed) && touches) || inside {
                hits.push(pl.path.clone());
            }
        }
    }
    match hits.first() {
        None => PathSet::new(),
        Some(first) => {
            let scope = scope_of_path(first);
            hits.iter().filter(|q| scope_of_path(q) == scope).cloned().collect()
        }
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

pub struct Sub {
    pub outs: Vec<(UiBox, String, Option<Ty>)>,
    pub dels: Vec<(UiBox, Del)>,
    pub taps: Vec<(UiBox, Tap)>,
    pub inputs: Vec<UiBox>,
}

/// A visible item: its place in `t.geo.items`, its tile box and signal, its ports and the changes its fields asked for.
pub struct Tile {
    pub slot: usize,
    pub ax: f64,
    pub ay: f64,
    pub tile: UiBox,
    pub sig: Signal,
    pub sub: Sub,
    pub fields: Vec<Change>,
}

fn box_in(ui: &mut Ui, flags: Flags, w: f64, h: f64, at: (f64, f64), name: &str) -> UiBox {
    ui.box_(name, BoxOpts::default().flags(flags).w(Size::Px(w)).h(Size::Px(h)).at(at))
}

/// A tile's ports and taps: out-ports, deletable inputs, zone controls.
pub fn tile_ports(t: &Pane, ui: &mut Ui, z: f64, fs: i64, pl: &Placed, tile: &UiBox) -> Sub {
    let shown = pl.shown;
    let saved = ui.enter(tile);
    let mut outs: Vec<(UiBox, String, Option<Ty>)> = Vec::new();
    let mut dels: Vec<(UiBox, Del)> = Vec::new();
    let mut taps: Vec<(UiBox, Tap)> = Vec::new();
    let mut inputs: Vec<UiBox> = Vec::new();
    match &pl.item {
        Item::Input(i) => {
            let pos = if shown == Level::Point { (pl.w, p::POINT_SIZE / 2.0) } else { (pl.w - 1.0, sp::PORT_Y) };
            let b = bxz(ui, z, "out", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
            outs.push((b, i.name.clone(), Some(i.ty.clone())));
        }
        Item::Return => {
            let b = bxz(ui, z, "in", (0.0 - 7.0, p::BODY_TOP + 12.0 - 7.0), (14.0, 14.0));
            inputs.insert(0, b);
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
                    let b = bxz(ui, z, "out", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                    outs.push((b, src.clone(), Some(n.ty.clone())));
                }
            }
            // the header's in-port: the first geometry slot
            if let Some(hi) = n.rows.iter().position(|r| r.head) {
                if n.zone.is_none() {
                    let pos = if shown == Level::Point { (0.0, p::POINT_SIZE / 2.0) } else { (1.0, sp::PORT_Y) };
                    let sink = bxz(ui, z, "d:head", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                    inputs.insert(0, sink);
                    if wired(&n.rows[hi]) {
                        dels.insert(0, (sink, Del::Unwire(hi)));
                    }
                }
            }
            match &n.zone {
                Some(zn) if !pl.collapsed => {
                    // the label row's variable: the collection's in-port at the edge, the variable's
                    // out-port in the left strip once something reads it
                    if let Some(li) = label_index(zn) {
                        let r = &zn.rail[li];
                        if r.key.is_some() && zn.kind != p::ZoneKind::For {
                            let sink = bxz(ui, z, "rd:label", (1.0 - 7.0, sp::PORT_Y - 7.0), (14.0, 14.0));
                            inputs.insert(0, sink);
                            if let (Some(key), Some(ex)) = (&r.key, &r.expr) {
                                if !e::free_names(ex).is_empty() {
                                    dels.insert(0, (sink, Del::Disconnect(key.clone())));
                                }
                            }
                        }
                        if sp::zone_uses(zn, &r.names) {
                            let pos = begin_port(n, zn, li);
                            let b = bxz(ui, z, "rail:label", (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            outs.push((b, r.names.first().cloned().unwrap_or_default(), r.ty.clone()));
                        }
                    }
                    for (i, ri) in extra_indices(zn).iter().enumerate() {
                        let r = &zn.rail[*ri];
                        let cy = p::HEAD_HEIGHT + (i as f64 * p::ROW_HEIGHT) + 12.0;
                        if r.role != p::Role::Capture {
                            let pos = begin_port(n, zn, *ri);
                            let b = bxz(ui, z, &format!("rail:{}", i), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            outs.push((b, r.names.first().cloned().unwrap_or_default(), r.ty.clone()));
                        }
                        // wired rail rows can be disconnected from their socket
                        let sink = if r.key.is_some() {
                            let b = bxz(ui, z, &format!("rd:{}", i), (0.0 - 7.0, cy - 7.0), (14.0, 14.0));
                            inputs.insert(0, b);
                            Some(b)
                        } else {
                            None
                        };
                        if let (Some(key), Some(ex), Some(sink)) = (&r.key, &r.expr, sink) {
                            if !e::free_names(ex).is_empty() {
                                dels.insert(0, (sink, Del::Disconnect(key.clone())));
                            }
                        }
                    }
                    if zn.kind != p::ZoneKind::Let && zn.kind != p::ZoneKind::State && zn.kind != p::ZoneKind::Branch {
                        // the selector of the label row: a button each side of the reading
                        let (selx, sely, selw, selh, _) = sp::selector_geo(&mut |size, text| measure_ui(ui, t, size, text), z, fs, pl.w * z, count_of(t, &n.path), t.saved.probe(&n.path));
                        let (selx, sely, selw, selh) = (selx / z, sely / z, selw / z, selh / z);
                        let b = bxz(ui, z, "prev", (selx - 4.0, sely), (16.0, selh));
                        taps.push((b, Tap::Step(-1)));
                        let b = bxz(ui, z, "next", (selx + selw - 12.0, sely), (16.0, selh));
                        taps.push((b, Tap::Step(1)));
                        let b = bxz(ui, z, "track", (selx + 12.0, sely), (selw - 24.0, selh));
                        taps.push((b, Tap::Track));
                    }
                    if zn.kind != p::ZoneKind::Let {
                        let b = bxz(ui, z, "toggle", (0.0, 0.0), (64.0, p::HEAD_HEIGHT));
                        taps.push((b, Tap::Toggle));
                    }
                }
                Some(zn) => {
                    for (i, r) in zn.rail.iter().enumerate() {
                        if r.key.is_some() || r.role == p::Role::Capture {
                            let pos = (0.0, p::BODY_TOP + ((i as f64 + 0.5) * p::ROW_HEIGHT));
                            let b = bxz(ui, z, &format!("rd:{}", i), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            inputs.insert(0, b);
                        }
                    }
                    if zn.kind != p::ZoneKind::Let {
                        let b = bxz(ui, z, "toggle", (sp::HEAD_PAD, 0.0), (64.0, p::HEAD_HEIGHT));
                        taps.push((b, Tap::Toggle));
                    }
                }
                None => {
                    if let Some(l) = &n.lens {
                        if shown != Level::Point && shown != Level::Chip {
                            let b = bxz(ui, z, "lens", (pl.w - 22.0, 2.0), (20.0, 20.0));
                            taps.push((b, Tap::Lens));
                            if let Some(step) = lens_of(t, &n.path) {
                                let lh = p::lens_height(l, step);
                                let top = sp::lens_top(pl.h, lh);
                                let len = l.steps.len() as i64;
                                for i in 0..=len {
                                    let ((bxx, byy), size) = sp::lens_button_box(len, i);
                                    let b = bxz(ui, z, &format!("ls{}", i), (bxx, top + byy), size);
                                    taps.push((b, Tap::LensStep(i)));
                                }
                                let ((rx, ry), size) = sp::lens_replace_box(lh);
                                let b = bxz(ui, z, "lr", (rx, top + ry), size);
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
                            let b = bxz(ui, z, &format!("o:{}", j), (pos.0 - 7.0, pos.1 - 7.0), (14.0, 14.0));
                            outs.push((b, src, Some(ty.clone())));
                        }
                        for (k, line) in pl.lines.iter().enumerate() {
                            if let p::Line::Row(i, r) = line {
                                let ry = top + (k as f64 * p::ROW_HEIGHT);
                                if r.socket {
                                    let sink = bxz(ui, z, &format!("d:{}", i), (0.0 - 7.0, ry + 12.0 - 7.0), (14.0, 14.0));
                                    inputs.insert(0, sink);
                                    if wired(r) {
                                        dels.insert(0, (sink, Del::Unwire(*i)));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    ui.leave(saved);
    dels.reverse();
    Sub { outs, dels, taps, inputs }
}

/// The live editors of a row: a number, a vector of numbers, a flag.
pub fn row_fields_of(ui: &mut Ui, z: f64, pl: &Placed, n: &p::Node, r: &p::Row, name: &str, ry: f64, size: i64) -> Vec<Change> {
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
                    return match num_field(ui, (at, ay), vw, h, Some(size), kind_of(r), range_of(r), nonfinite, &format!("f{}", name), ex.name(*text)) {
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
                            if let Some(text2) = num_field(ui, (at + (c as f64 * (cw + gap)), ay), cw, h, Some(size), kind_of(r), None, nonfinite, &format!("f{}.{}", name, c), ex.name(text)) {
                                out.push(set(vec![c as i64], number_syntax(text2)));
                            }
                        }
                    }
                    return out;
                }
                SNode::Sym(b) if matches!(ex.name(*b), "true" | "false") => {
                    let toggle = ui.box_(
                        &format!("b{}", name),
                        BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Px(sp::SWITCH_W * z)).h(Size::Px(sp::SWITCH_H * z)).at(((pl.w - sp::HEAD_PAD - sp::SWITCH_W) * z, (ry + 5.0) * z)),
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

/// Frame-wide gesture state shared by wire hit testing and completion.
pub struct Ctx {
    pub wire_hits: HashMap<usize, Vec<UiBox>>,
    pub finished: bool,
    pub opened: Option<Editing>,
    pub fs: i64,
}

impl Ctx {
    pub fn hits(&self, i: usize) -> Vec<UiBox> {
        self.wire_hits.get(&i).cloned().unwrap_or_default()
    }
}

/// A wire's hit boxes: its segments cut to `clip`, as 16-point rectangles in the shared hit tree.
pub fn wire_boxes(c: &mut Ctx, t: &Pane, ui: &mut Ui, origin: (f64, f64), name: &str, clip: (f64, f64, f64, f64), i: usize) {
    let (cx, cy, cw, ch) = clip;
    let wi = &t.geo.wires[i];
    if !(wi.target.is_some() && !tagged(t, wi)) {
        return;
    }
    let mut count = 0;
    let prefix = format!("{}:{}:", name, i);
    for (j, seg) in wi.pts.windows(2).enumerate() {
        let (ax, ay) = seg[0];
        let (bx, by) = seg[1];
        if let Some(((ax, ay), (bx, by))) = clip_segment((sx(t, ax) - t.x as f64, sy(t, ay) - t.y as f64), (sx(t, bx) - t.x as f64, sy(t, by) - t.y as f64), (cx - 4.0, cy - 4.0, cw + 8.0, ch + 8.0)) {
            let steps = if ax == bx || ay == by { 1 } else { 1.max(((bx - ax).hypot(by - ay) / 16.0) as i64) };
            let prefix = format!("{}{}-", prefix, j);
            for st in 0..steps {
                let u0 = st as f64 / steps as f64;
                let u1 = (st + 1) as f64 / steps as f64;
                let x0 = ax + ((bx - ax) * u0);
                let y0 = ay + ((by - ay) * u0);
                let x1 = ax + ((bx - ax) * u1);
                let y1 = ay + ((by - ay) * u1);
                count += 1;
                let b = ui.box_fmt(format_args!("{}{}", prefix, count), BoxOpts::default().flags(CLICKABLE).at((x0.min(x1) - 4.0 - origin.0, y0.min(y1) - 4.0 - origin.1)).w(Size::Px((x1 - x0).abs() + 8.0)).h(Size::Px((y1 - y0).abs() + 8.0)));
                c.wire_hits.entry(i).or_default().insert(0, b);
            }
        }
    }
}

/// A text field over a tile or frame: `Some change` the frame it commits.
pub fn edit_field(c: &mut Ctx, ui: &mut Ui, at: (f64, f64), w: f64, h: f64, key: &str, current: &str, valid: &dyn Fn(&str) -> bool, commit: &dyn Fn(String) -> Change) -> Vec<Change> {
    let mut o = ValueFieldOpts::default();
    o.size = Some(c.fs);
    o.edit = true;
    o.left = (current).parse::<f64>().ok().is_none();
    let (v, open) = ui.value_field(at, w, h, &o, valid, key, current);
    if !open {
        c.finished = true;
    }
    if !open && v != current && valid(&v) { vec![commit(v)] } else { Vec::new() }
}

fn local(t: &Pane, pt: (f64, f64)) -> (f64, f64) {
    (pt.0 - t.x as f64, pt.1 - t.y as f64)
}

/// One visible node: its tile box, its ports and the live widgets of its rows.
pub fn build_tile(c: &mut Ctx, t: &Pane, ui: &mut Ui, z: f64, fs: i64, slot: usize) -> Tile {
    let (pl, ax, ay) = &t.geo.items[slot];
    let (ax, ay) = (*ax, *ay);
    let (bx, by) = (sx(t, ax), sy(t, ay));
    let key = format!("t:{}", flow_syntax::symbol::join_path(&pl.path, "/"));
    let w = pl.w * z;
    let h = pl.h * z;
    let shown = pl.shown;
    let tile = box_in(ui, CLICKABLE, w, h, local(t, (bx, by)), &key);
    // an expanded zone's tile covers its body: the wires that end inside it get their boxes again here,
    // over the tile and under the cards built after it
    if let Item::Item(n) = &pl.item {
        if n.zone.is_some() && !pl.collapsed {
            let (lx, ly) = local(t, (bx, by));
            let x0 = lx.max(0.0);
            let y0 = ly.max(0.0);
            let clip = (x0, y0, (t.width as f64).min(lx + w) - x0, (t.height as f64).min(ly + h) - y0);
            let saved = ui.enter(&tile);
            for i in t.geo.inside.get(&pl.path).cloned().unwrap_or_default() {
                wire_boxes(c, t, ui, (lx, ly), "zwire", clip, i);
            }
            ui.leave(saved);
        }
    }
    let sub = tile_ports(t, ui, z, fs, pl, &tile);
    // fields, inside the tile
    let mut fields: Vec<Change> = Vec::new();
    match &pl.item {
        Item::Item(n) if n.zone.is_none() && z >= 0.5 && p::value_card(n) => {
            // a literal binding: its value is the field of its header, at body size
            let saved = ui.enter(&tile);
            for r in &n.rows {
                fields.extend(row_fields_of(ui, z, pl, n, r, "v", 0.0, fs));
            }
            ui.leave(saved);
        }
        Item::Item(n) if n.zone.is_none() && z >= 0.5 && (shown == Level::Card || shown == Level::Full) => {
            let saved = ui.enter(&tile);
            let top = rows_top(n, 0.0);
            for (k, line) in pl.lines.iter().enumerate() {
                if let p::Line::Row(i, r) = line {
                    fields.extend(row_fields_of(ui, z, pl, n, r, &i.to_string(), top + (k as f64 * p::ROW_HEIGHT), 4.max(fs - 2)));
                }
            }
            ui.leave(saved);
        }
        Item::Input(i) if z >= 0.5 && t.editing != Some(Editing::Default(pl.path.clone())) => {
            if let Some(d) = &i.default {
                if let SNode::Num(text) = &d.node {
                    // a value node: its number is a field in the header, at body size
                    let saved = ui.enter(&tile);
                    if let Some(text2) = num_field(ui, (sp::value_x(pl.w) * z, 4.0 * z), sp::FIELD_W * z, sp::FIELD_H * z, Some(fs), None, None, false, "default", d.name(*text)) {
                        fields.push(Change::SyntaxEdit(e::Op::SetInputDefault { form: i.path[0].to_string(), input: i.name.clone(), value: number_syntax(text2) }));
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
                            .flags(CLICKABLE | TAB_STOP)
                            .w(Size::Px(if r.kind == p::RowKind::Add { (pl.w - sp::LABEL_X - sp::HEAD_PAD) * z } else { 24.0 * z }))
                            .h(Size::Px(16.0 * z))
                            .at(((if r.kind == p::RowKind::Add { sp::LABEL_X } else { sp::value_x(pl.w) }) * z, row_y)),
                    );
                    // a click on the expression's text (not its glyph, which unfolds) makes the row the inspector's
                    // subject: the holder stays selected, the row is `holder @ [":" ^ label]`
                    let mut subject: Vec<Change> = Vec::new();
                    if let p::Chip::Inline { .. } = r.chip {
                        let cb = ui.box_fmt(format_args!("ex{}", i), BoxOpts::default().flags(CLICKABLE).w(Size::Px((sp::FIELD_W - 24.0) * z)).h(Size::Px(16.0 * z)).at(((sp::value_x(pl.w) + 24.0) * z, row_y)));
                        let sg = ui.signal(&cb);
                        // a double click expands the call in place, as the glyph does
                        if sg.double_clicked {
                            subject = unfold_row(n, r);
                        } else if sg.clicked {
                            let mut row = n.path.clone();
                            row.push(format!(":{}", r.label).into());
                            subject = vec![Change::Selected(vec![n.path.clone(), row])];
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
                    let b = ui.box_fmt(format_args!("fold{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Px(fw)).h(Size::Px(16.0 * z)).at((left - fw - (2.0 * z), row_y)));
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
                            let b = ui.box_fmt(format_args!("ref{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Px(width)).h(Size::Px(16.0 * z)).at((left, row_y)));
                            if ui.signal(&b).clicked {
                                add_clicks.push(Change::Selected(vec![cand]));
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
                            let b = ui.box_fmt(format_args!("mv{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Px(16.0 * z)).h(Size::Px(16.0 * z)).at(((pl.w - 22.0) * z, (top + (k as f64 * p::ROW_HEIGHT) + 4.0) * z)));
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
                                let b = ui.box_fmt(format_args!("sw{}", i), BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Px((pl.w - 24.0) * z)).h(Size::Px(16.0 * z)).at((12.0 * z, (top + (k as f64 * p::ROW_HEIGHT) + 4.0) * z)));
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
    let sig = ui.signal(&tile);
    Tile { slot, ax, ay, tile, sig, sub, fields: all }
}

/// A payload in flight: the innermost node under the pointer (a zone's tile covers its body), else the
/// canvas, is where it would be put; the host decides whether it takes it.
pub fn drop_change(t: &Pane, ui: &mut Ui, canvas: &UiBox, tiles: &[Tile]) -> Option<Change> {
    let on_canvas = ui.drop_target(canvas)?;
    let mut found: Option<(usize, Drop)> = None;
    for tl in tiles {
        let pl = &t.geo.items[tl.slot].0;
        let d = ui.drop_target(&tl.tile);
        if let Some(d) = d {
            let skip = match &found {
                Some((q, _)) => {
                    let qp = &t.geo.items[*q].0;
                    qp.w * qp.h <= pl.w * pl.h
                }
                None => false,
            };
            if !skip {
                found = Some((tl.slot, d));
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

pub fn context_step(t: &mut Pane, ui: &mut Ui, tiles: &[Tile], canvas_signal: &Signal, emit: &mut Vec<Change>) {
    // the context menu
    if t.context.is_none() {
        let mut found: Option<((f64, f64), Path)> = None;
        for tl in tiles {
            if context_clicked(&tl.sig) {
                found = Some((tl.sig.release_point, t.geo.items[tl.slot].0.path.clone()));
                break;
            }
        }
        if let Some(((x, y), path)) = found {
            // the menu acts on the nodes: a wire selected before is no longer the selection
            let selected = if t.selected.contains(&path) { t.selected.clone() } else { std::iter::once(path.clone()).collect() };
            if selected != t.selected {
                emit.push(Change::Selected(selected.iter().cloned().collect()));
            }
            (t.context, t.selected, t.selected_wire) = (Some(((x, y), path)), selected, None);
        }
    }
    if t.context.is_none() && context_clicked(canvas_signal) {
        let (x, y) = canvas_signal.release_point;
        emit.push(Change::MenuRequested(x, y));
    }
    if let Some((at, path)) = t.context.clone() {
        let items = context_items(t, &path);
        match ui.context_menu(at, &MenuOpts::default(), "pxui-scope-context", &items) {
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

/// Whether a tile's pointer is on it (a zone's tile covers its body).
fn tile_hovered(ui: &mut Ui, tl: &Tile) -> bool {
    ui.hovered_within(&tl.tile)
}

/// A wire let go: on an input row it connects; on empty canvas it opens the add menu after its source
/// (pulled off an existing wire, the host inserts the new node on the selected wire).
pub fn finish_wire(t: &mut Pane, ui: &mut Ui, z: f64, mouse: (f64, f64), emit: &mut Vec<Change>, tiles: &[Tile], w: &Wiring, src_path: &Path, release_point: (f64, f64)) {
    let mut target: Option<(Path, ArgKey)> = None;
    for tl in tiles {
        if !tile_hovered(ui, tl) {
            continue;
        }
        let (tp, tax, tay) = &t.geo.items[tl.slot];
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
    // a wire let go on empty canvas: the add menu there, after the node it came from
    let mut any_hover = false;
    for tl in tiles {
        if tile_hovered(ui, tl) {
            any_hover = true;
            break;
        }
    }
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
            emit.push(Change::Selected(vec![src_path.clone()]));
            let (x, y) = release_point;
            emit.push(Change::MenuRequested(x, y));
        }
        None => emit.push(Change::Notice("Drop the wire on an input row".to_owned())),
    }
    let selected = if on_canvas { std::iter::once(src_path.clone()).collect() } else { t.selected.clone() };
    (t.drag, t.selected) = (None, selected);
}

pub fn wire_pull(t: &mut Pane, ui: &mut Ui, ctx: &Ctx, z: f64, mouse: (f64, f64), tiles: &[Tile], wires_in_view: &[usize], emit: &mut Vec<Change>) {
    // a wire pulled past 6 points is a new wire from its source: the same drag as from the out-port
    if t.drag.is_none() && t.context.is_none() {
        let mut found: Option<(usize, Path, Vec<(Path, ArgKey)>)> = None;
        'outer: for &i in wires_in_view {
            let wi = &t.geo.wires[i];
            if let (Some(tg), Some(from)) = (&wi.target, &wi.from) {
                for b in ctx.hits(i) {
                    let sg = ui.signal(&b);
                    let (px, py) = sg.pointer;
                    let (qx, qy) = sg.press_point;
                    if sg.held && left_button(&sg) && (px - qx).hypot(py - qy) > 6.0 {
                        let lift = if ui.press_keys(&b).contains(&Key::Alt) { vec![(tg.0.clone(), tg.1.clone())] } else { Vec::new() };
                        found = Some((i, from.clone(), lift));
                        break 'outer;
                    }
                }
            }
        }
        if let Some((i, from, lift)) = found {
            let wi = &t.geo.wires[i];
            let src_ty = wi.ty.clone();
            let fromp = (sx(t, wi.a.0), sy(t, wi.a.1));
            let src = match t.item_at(&from).map(|(pl, _, _)| &pl.item) {
                Some(Item::Item(n)) if n.zone.is_none() && n.binds.len() == 1 && n.binds[0] == n.name => Some(n.name.clone()),
                Some(Item::Input(inp)) => Some(inp.name.clone()),
                _ => None,
            };
            if let Some(src) = src {
                { t.drag = Some(Drag::Wiring(Wiring { src, ty: src_ty, iter: false, from: fromp, lift })); };
            }
        }
    }
    // ...and let go, it finishes like an out-port's wire
    if let (Some(Drag::Wiring(wg)), Some(_)) = (t.drag.clone(), t.selected_wire.clone()) {
        let mut released = false;
        'a: for &i in wires_in_view {
            for b in ctx.hits(i) {
                let sg = ui.signal(&b);
                if sg.released && left_button(&sg) {
                    released = true;
                    break 'a;
                }
            }
        }
        if released {
            let mut from_wire: Option<(usize, (f64, f64))> = None;
            'b: for &i in wires_in_view {
                for b in ctx.hits(i) {
                    let sg = ui.signal(&b);
                    if sg.released && left_button(&sg) {
                        from_wire = Some((i, sg.release_point));
                        break 'b;
                    }
                }
            }
            if let Some((i, release_point)) = from_wire {
                let src_path = t.geo.wires[i].from.clone().unwrap_or_default();
                finish_wire(t, ui, z, mouse, emit, tiles, &wg, &src_path, release_point);
            }
        }
    }
}

pub fn tile_gestures(t: &mut Pane, ui: &mut Ui, z: f64, mouse: (f64, f64), tiles: &[Tile], emit: &mut Vec<Change>) -> (Vec<(Path, i64)>, bool) {
    let mut lens_next: Vec<(Path, i64)> = t.lens.clone();
    let mut lens_changed = false;
    // tile presses select and start a move; sockets start a wire
    for tl in tiles {
        let geo = t.geo.clone();
        let (pl, _, _) = &geo.items[tl.slot];
        let s = &tl.sig;
        let sub = &tl.sub;
        emit.extend(tl.fields.iter().cloned());
        // a click on a read name or an expression chip selects what it names
        if let Some(paths) = tl.fields.iter().find_map(|c| match c {
            Change::Selected(paths) => Some(paths.clone()),
            _ => None,
        }) {
            select(paths, t);
        }
        if s.pressed && left_button(s) && t.context.is_none() {
            let additive = ui.press_keys(&tl.tile).contains(&Key::Shift);
            let selected: PathSet = if additive {
                let mut sel = t.selected.clone();
                if !sel.remove(&pl.path) {
                    sel.insert(pl.path.clone());
                }
                sel
            } else if t.selected.contains(&pl.path) && t.selected.iter().all(|q| t.geo.slot.contains_key(q)) {
                t.selected.clone()
            } else {
                std::iter::once(pl.path.clone()).collect()
            };
            emit.push(Change::Selected(selected.iter().cloned().collect()));
            let paths = parents_removed(&selected.iter().cloned().collect::<Vec<_>>());
            { (t.selected, t.selected_wire, t.drag) = (selected, None, Some(Drag::Moving { paths, dx: 0.0, dy: 0.0, moved: false })); };
        }
        if s.double_clicked && left_button(s) && t.context.is_none() && t.editing.is_none() {
            let (_, top, _, _) = ui.rect(&tl.tile);
            match &pl.item {
                Item::Item(n) if !n.synthetic && s.press_point.1 < top + (p::HEAD_HEIGHT * z) => {
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
        if let Some(Drag::Moving { paths, dx, dy, moved }) = t.drag.clone() {
            if s.held && left_button(s) && t.selected.contains(&pl.path) {
                let (ddx, ddy) = s.drag;
                let ndx = dx + (ddx / z);
                let ndy = dy + (ddy / z);
                { t.drag = Some(Drag::Moving { paths, dx: ndx, dy: ndy, moved: moved || ndx.hypot(ndy) > 3.0 / z }); };
            }
        }
        if let Some(Drag::Moving { paths, dx, dy, moved }) = t.drag.clone() {
            if s.released && left_button(s) {
                if moved {
                    let mv: Vec<(Path, f64, f64)> = paths.iter().filter_map(|path| rel_at(t, path).map(|(rx, ry)| (path.clone(), p::snap(rx + dx), p::snap(ry + dy)))).collect();
                    emit.push(Change::Moved(mv));
                }
                t.drag = None;
            }
        }
        // zone controls
        for (b, on) in &sub.taps {
            let bs = ui.signal(b);
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
                        let (x, _, wd, _) = ui.rect(b);
                        if count > 0 && wd > 0.0 {
                            let px = if bs.released { bs.release_point.0 } else { bs.pointer.0 };
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
        for (b, del) in &sub.dels {
            if ui.signal(b).clicked {
                let ops: Vec<e::Op> = match (del, &pl.item) {
                    (Del::Unwire(i), Item::Item(n)) => unwire(t, n, &n.rows[*i]),
                    (Del::Disconnect(key), Item::Item(n)) => vec![e::Op::Disconnect { node: n.path.clone(), key: key.clone(), fallback: None }],
                    _ => Vec::new(),
                };
                for op in ops {
                    emit.push(Change::SyntaxEdit(op));
                }
            }
        }
        // wires
        for (b, src, ty) in &sub.outs {
            let bs = ui.signal(b);
            if bs.pressed && left_button(&bs) {
                let iter = match &t.scope {
                    Some(scope) => p::zones(scope).iter().any(|n| match &n.zone {
                        Some(zn) => zn.rail.iter().any(|r| r.role == p::Role::Var && r.names.contains(src)),
                        None => false,
                    }),
                    None => false,
                };
                // Alt on an out-port lifts every wire it feeds
                let lift: Vec<(Path, ArgKey)> = if !ui.press_keys(b).contains(&Key::Alt) {
                    Vec::new()
                } else {
                    let mut acc: Vec<(Path, ArgKey)> = Vec::new();
                    for wi in t.geo.wires.iter() {
                        if let (Some(f), Some((path, key, _))) = (&wi.from, &wi.target) {
                            if *f == pl.path {
                                acc.insert(0, (path.clone(), key.clone()));
                            }
                        }
                    }
                    acc
                };
                let (x, y, wd, h) = ui.rect(b);
                { t.drag = Some(Drag::Wiring(Wiring { src: src.clone(), ty: ty.clone(), iter, from: (x + (wd / 2.0), y + (h / 2.0)), lift })); };
            }
            if bs.released && left_button(&bs) {
                match t.drag.clone() {
                    Some(Drag::Wiring(wg)) => {
                        finish_wire(t, ui, z, mouse, emit, tiles, &wg, &pl.path, bs.release_point);
                    }
                    _ => { t.drag = None; },
                }
            }
        }
    }
    (lens_next, lens_changed)
}

/// The pointer: pan, a press on a wire or the canvas, the rubber band.
pub fn pointer_step(t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, canvas: &UiBox, canvas_signal: &Signal, tiles: &[Tile], ctx: &Ctx, wires_in_view: &[usize], emit: &mut Vec<Change>) {
    let non_left = |sg: &Signal| sg.button != Some(MouseButton::LeftButton) && sg.button.is_some();
    let non_left_pressed = |sg: &Signal| sg.pressed && non_left(sg);
    let non_left_held = |sg: &Signal| sg.held && non_left(sg);
    let non_left_released = |sg: &Signal| sg.released && non_left(sg);
    let mut all_signals: Vec<&Signal> = vec![canvas_signal];
    all_signals.extend(tiles.iter().map(|tl| &tl.sig));
    let panning = all_signals.iter().any(|sg| non_left_held(sg) || non_left_released(sg));
    let pan_pressed = all_signals.iter().any(|sg| non_left_pressed(sg));
    let pan_released = all_signals.iter().any(|sg| non_left_released(sg));
    let lost = frame.input.events.iter().any(|ev| matches!(ev, Event::FocusLost));
    let panning_grab = if pan_pressed || panning {
        true
    } else if pan_released || lost || frame.input.buttons.is_empty() {
        false
    } else {
        t.panning_grab
    };
    if panning_grab != t.panning_grab {
        let _ = rays::sketch::set_relative_mouse(panning_grab);
    }
    t.panning_grab = panning_grab;
    if panning {
        let (dx, dy) = if canvas_signal.held || canvas_signal.released {
            canvas_signal.drag
        } else {
            tiles.iter().fold((0.0, 0.0), |(dx, dy), tl| (dx + tl.sig.drag.0, dy + tl.sig.drag.1))
        };
        (t.pan_x, t.pan_y) = (t.pan_x + dx, t.pan_y + dy);
    }
    let tile_pressed = tiles.iter().any(|tl| tl.sig.pressed);
    let mut wire_pressed: Option<usize> = None;
    if !(t.context.is_some() || tile_pressed || canvas_signal.subtree_press.is_none()) {
        'o: for &i in wires_in_view {
            for b in ctx.hits(i) {
                let sg = ui.signal(&b);
                if sg.pressed && left_button(&sg) {
                    wire_pressed = Some(i);
                    break 'o;
                }
            }
        }
    }
    if let Some(i) = wire_pressed {
        if !t.selected.is_empty() {
            emit.push(Change::Selected(Vec::new()));
        }
        let target = t.geo.wires[i].target.clone();
        (t.selected, t.selected_wire) = (PathSet::new(), target);
    }
    if canvas_signal.pressed && left_button(canvas_signal) && t.context.is_none() && !tile_pressed && wire_pressed.is_none() {
        let additive = ui.press_keys(canvas).contains(&Key::Shift);
        if !additive && !t.selected.is_empty() {
            emit.push(Change::Selected(Vec::new()));
        }
        let base = if additive { t.selected.clone() } else { PathSet::new() };
        { (t.selected, t.selected_wire, t.drag) = (base.clone(), None, Some(Drag::Marquee { base })); };
    }
    // the rubber band selects the nodes of one scope it touches
    if let Some(Drag::Marquee { base }) = t.drag.clone() {
        if canvas_signal.held || canvas_signal.released {
            let (px, py) = if canvas_signal.released { canvas_signal.release_point } else { canvas_signal.pointer };
            let (x0, y0) = canvas_signal.press_point;
            let rw = (px - x0).abs();
            let rh = (py - y0).abs();
            let selected: PathSet = if rw < 3.0 && rh < 3.0 { base } else { base.union(&marquee_hits(t, (x0.min(px), y0.min(py), rw, rh))).cloned().collect() };
            if selected != t.selected {
                emit.push(Change::Selected(selected.iter().cloned().collect()));
            }
            let drag = if canvas_signal.released { None } else { t.drag.clone() };
            (t.selected, t.drag) = (selected, drag);
        }
    }
}

pub struct Hover {
    pub slot: usize,
    pub ax: f64,
    pub ay: f64,
    pub i: usize,
}

pub fn hover_step(t: &mut Pane, ui: &mut Ui, z: f64, mouse: (f64, f64), tiles: &[Tile], ctx: &Ctx, wires_in_view: &[usize]) -> Option<Hover> {
    // hovered row and chip expansion
    let mut hover: Option<Hover> = None;
    for tl in tiles {
        let (pl, ax, ay) = &t.geo.items[tl.slot];
        if let Item::Item(n) = &pl.item {
            if n.zone.is_none() && ui.hovered_within(&tl.tile) {
                let px = (mouse.0 - sx(t, *ax)) / z;
                let py = (mouse.1 - sy(t, *ay)) / z;
                match row_at(14.0, pl, n, pl.shown, px, py) {
                    Some(i) if i < n.rows.len() => {
                        hover = Some(Hover { slot: tl.slot, ax: *ax, ay: *ay, i });
                        break;
                    }
                    _ => {}
                }
            }
        }
    }
    let hovered_row = hover.as_ref().map(|h| {
        let n = match &t.geo.items[h.slot].0.item {
            Item::Item(n) => n,
            _ => unreachable!("hover is over a node"),
        };
        (n.path.clone(), n.rows[h.i].key.clone())
    });
    let port_centre = |ui: &mut Ui, b: &UiBox| -> Option<(f64, f64)> {
        if ui.hovered_within(b) {
            let (x, y, wd, h) = ui.rect(b);
            Some((x + (wd / 2.0), y + (h / 2.0)))
        } else {
            None
        }
    };
    let mut hovered_port: Option<(f64, f64)> = None;
    'p: for tl in tiles {
        for (b, _, _) in &tl.sub.outs {
            if let Some(pc) = port_centre(ui, b) {
                hovered_port = Some(pc);
                break 'p;
            }
        }
        for b in &tl.sub.inputs {
            if let Some(pc) = port_centre(ui, b) {
                hovered_port = Some(pc);
                break 'p;
            }
        }
    }
    let same_port = |point: (f64, f64)| -> bool {
        match hovered_port {
            None => false,
            Some((x, y)) => (sx(t, point.0) - x).abs() <= (1.5 * z) + 0.5 && (sy(t, point.1) - y).abs() <= (1.5 * z) + 0.5,
        }
    };
    let mut highlighted: Vec<Wire> = Vec::new();
    for &i in wires_in_view {
        let wi = &t.geo.wires[i];
        let mut any = false;
        for b in ctx.hits(i) {
            if ui.hovered_within(&b) {
                any = true;
                break;
            }
        }
        if t.context.is_none() && (any || same_port(wi.a) || same_port(wi.b)) {
            highlighted.insert(0, wi.clone());
        }
    }
    (t.hovered_row, t.highlighted) = (hovered_row, highlighted);
    hover
}

pub fn frames_step(t: &mut Pane, ui: &mut Ui, ctx: &mut Ctx, z: f64, canvas: &UiBox, emit: &mut Vec<Change>) {
    // frames: a title strip, a delete cross and a resize corner, over the tiles (a zone's tile covers its
    // whole body) and clear of the nodes, which keep 12 points inside the frame
    struct FrameBox {
        scope: Path,
        i: usize,
        title: String,
        rect: (f64, f64, f64, f64),
        tb: UiBox,
        xb: UiBox,
        rb: UiBox,
    }
    let mut frame_boxes: Vec<FrameBox> = Vec::new();
    let saved = ui.enter(canvas);
    for (scope, (ox, oy)) in t.geo.origins.iter() {
        for (i, (title, (fx, fy), (fw, fh))) in t.saved.frames(scope).iter().enumerate() {
            let x = sx(t, ox + fx);
            let y = sy(t, oy + fy);
            let wd = fw * z;
            let h = fh * z;
            let mk = |ui: &mut Ui, k: &str, pos: (f64, f64), size: (f64, f64)| {
                box_in(ui, CLICKABLE, size.0, size.1, local(t, pos), &format!("fr:{}:{}:{}", flow_syntax::symbol::join_path(&scope, "/"), i, k))
            };
            let tb = mk(ui, "t", (x, y), ((wd - 20.0).max(8.0), 18.0 * z));
            let xb = mk(ui, "x", (x + wd - 18.0, y + 2.0), (16.0, 16.0));
            let rb = mk(ui, "r", (x + wd - 12.0, y + h - 12.0), (12.0, 12.0));
            frame_boxes.push(FrameBox { scope: scope.clone(), i, title: title.clone(), rect: (x, y, wd, h), tb, xb, rb });
        }
    }
    ui.leave(saved);
    for fb in frame_boxes {
        let FrameBox { scope, i, title, rect: (x, y, wd, _), tb, xb, rb } = fb;
        let ts = ui.signal(&tb);
        let xs = ui.signal(&xb);
        let rs = ui.signal(&rb);
        let all = t.saved.frames(&scope);
        if let Some(Editing::Title(sp_, k)) = &t.editing {
            if *sp_ == scope && *k == i as i64 {
                let saved = ui.enter(canvas);
                let all2 = all.clone();
                let scope2 = scope.clone();
                let ch = edit_field(ctx, ui, local(t, (x + 4.0, y + 2.0)), (wd - 24.0).max(40.0), 16.0 * z, &format!("frt{}", i), &title, &|s: &str| !s.is_empty() && !s.contains('"'), &move |v| {
                    Change::FramesSet {
                        scope: scope2.clone(),
                        frames: all2.iter().enumerate().map(|(j, f)| if j == i { (v.clone(), f.1, f.2) } else { f.clone() }).collect(),
                    }
                });
                ui.leave(saved);
                emit.extend(ch);
            }
        }
        if xs.clicked {
            emit.push(Change::FramesSet { scope: scope.clone(), frames: all.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, f)| f.clone()).collect() });
        } else if ts.double_clicked && t.editing.is_none() {
            (t.drag, t.editing) = (None, Some(Editing::Title(scope.clone(), i as i64)));
        } else if rs.pressed && left_button(&rs) {
            { t.drag = Some(Drag::Sizing { scope: scope.clone(), index: i as i64, dw: 0.0, dh: 0.0 }); };
        } else {
            match t.drag.clone() {
                None if ts.held && left_button(&ts) && t.editing.is_none() && (ts.pointer.0 - ts.press_point.0).hypot(ts.pointer.1 - ts.press_point.1) > 3.0 => {
                    // a drag by the title (a click or a double-click stays one): the nodes of the scope whose
                    // centres lie inside the frame travel with it
                    let (ox, oy) = t.geo.origins.iter().find(|(q, _)| *q == scope).map(|(_, o)| *o).unwrap_or((0.0, 0.0));
                    let (_, (fx, fy), (fw, fh)) = t.saved.frames(&scope)[i].clone();
                    let inside = |path: &Path| -> bool {
                        scope_of_path(path) == scope
                            && match box_at(t, path) {
                                Some((bx, by, bw, bh)) => {
                                    let cx = bx + (bw / 2.0);
                                    let cy = by + (bh / 2.0);
                                    cx >= ox + fx && cx <= ox + fx + fw && cy >= oy + fy && cy <= oy + fy + fh
                                }
                                None => false,
                            }
                    };
                    let mut paths: Vec<Path> = Vec::new();
                    for (pl, _, _) in t.geo.items.iter() {
                        if inside(&pl.path) {
                            paths.insert(0, pl.path.clone());
                        }
                    }
                    let dx = (ts.pointer.0 - ts.press_point.0) / z;
                    let dy = (ts.pointer.1 - ts.press_point.1) / z;
                    { t.drag = Some(Drag::Carrying { scope: scope.clone(), index: i as i64, paths, dx, dy }); };
                }
                Some(Drag::Carrying { scope: cs, index, paths, dx, dy }) if cs == scope && index == i as i64 && (ts.held || ts.released) => {
                    let (ddx, ddy) = ts.drag;
                    let ndx = dx + (ddx / z);
                    let ndy = dy + (ddy / z);
                    { t.drag = Some(Drag::Carrying { scope: cs.clone(), index, paths: paths.clone(), dx: ndx, dy: ndy }); };
                    if ts.released {
                        if ndx.hypot(ndy) > 3.0 / z {
                            let mv: Vec<(Path, f64, f64)> = paths.iter().filter_map(|path| rel_at(t, path).map(|(rx, ry)| (path.clone(), p::snap(rx + ndx), p::snap(ry + ndy)))).collect();
                            emit.push(Change::Moved(mv));
                            emit.push(Change::FramesSet { scope: scope.clone(), frames: frame_list(t, &scope) });
                        }
                        t.drag = None;
                    }
                }
                Some(Drag::Sizing { scope: ss, index, dw, dh }) if ss == scope && index == i as i64 && (rs.held || rs.released) => {
                    let (dx, dy) = rs.drag;
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
    let v = ui.text_width(Some(size), text);
    t.measured.borrow_mut().insert((size, text.to_owned()), v);
    v
}

type Marks = Vec<((f64, f64, f64, f64), Option<String>, Option<bool>)>;

/// The frame's paint: grid and wires under the cards, the cards, then the overlays.
pub fn paint_frame(t: &Pane, ui: &mut Ui, z: f64, fs: i64, layer: &UiBox, canvas: &UiBox, canvas_signal: &Signal, tiles: &[Tile], zones: &[usize], wires_in_view: &[usize], footers: &HashMap<Path, Footer>, chain: &Option<Chains>, mouse: (f64, f64), hover: &Option<Hover>) -> (i64, i64) {
    let snapshot: Rc<Pane> = Rc::new(t.clone());
    let row_hover: Option<(Path, usize)> = hover.as_ref().map(|h| {
        let n = match &t.geo.items[h.slot].0.item {
            Item::Item(n) => n,
            _ => unreachable!("hover is over a node"),
        };
        (n.path.clone(), h.i)
    });
    // a scene object nothing reads (taken out of its merge) is dimmed; a wire from it brings it back
    let read = t.geo.read.clone();
    let dimmed = |n: &p::Node| -> bool {
        !n.synthetic
            && n.zone.is_none()
            && n.head.starts_with("scene/")
            && n.head != "scene/merge"
            && n.head != "scene/root"
            && match box_at(t, &n.path) {
                Some((ax, ay, wd, _)) => !read.contains(&fkey((ax + wd, ay + WIRE_HEAD_Y))),
                None => false,
            }
    };
    {
        let snap = snapshot.clone();
        let zone_slots: Vec<usize> = zones.to_vec();
        let wires: Vec<usize> = wires_in_view.to_vec();
        ui.draw(layer, move |paint, rect| {
            sp::paint_background(paint, &snap, rect, &wires, &zone_slots);
        });
    }
    for tl in tiles {
        let (pl, ax, ay) = &t.geo.items[tl.slot];
        let (ax, ay) = (*ax, *ay);
        let isel = t.selected.contains(&pl.path);
        let hovered = t.drag.is_none() && t.carry_hot.is_none() && ui.hovered_within(&tl.tile);
        let snap = snapshot.clone();
        let slot = tl.slot;
        // the per-item facts the painter needs, decided here
        let mut footer: Option<Footer> = None;
        let mut lens_step: Option<i64> = None;
        let mut carry = false;
        let mut out_wired = false;
        let mut failed: Option<String> = None;
        let mut dim = false;
        let mut rh: Option<usize> = None;
        let mut probe = 0;
        let mut count = 0;
        let mut recede = false;
        if let Item::Item(n) = &pl.item {
            footer = footers.get(&n.path).cloned();
            lens_step = lens_of(t, &n.path);
            carry = matches!(&t.carry_hot, Some((hp, _)) if *hp == n.path);
            out_wired = read.contains(&fkey(out_anchor(pl, ax, ay)));
            failed = t.failed.iter().find(|(q, _)| *q == n.path).map(|(_, c)| c.clone());
            dim = n.zone.is_none() || pl.collapsed;
            dim = dim && dimmed(n);
            rh = match &row_hover {
                Some((rp, i)) if *rp == n.path => Some(*i),
                _ => None,
            };
            probe = t.saved.probe(&n.path);
            count = count_of(t, &n.path);
            // the chain of the selection stays; the rest of the cards recede
            recede = match chain {
                Some(c) => !node_lit(c, &pl.path) && (if n.zone.is_some() { pl.collapsed } else { true }),
                None => false,
            };
        }
        ui.draw(&tl.tile, move |paint, rect| {
            let (pl, _, _) = &snap.geo.items[slot];
            let (x, y, w, h) = rect;
            match &pl.item {
                Item::Input(_) if pl.shown == Level::Point => sp::paint_point(paint, &snap, z, fs, pl, None, isel, rect),
                Item::Input(i) => sp::paint_input(paint, &snap, z, fs, i, isel, rect),
                Item::Return => {
                    if let Some(s) = &snap.scope {
                        sp::paint_return(paint, &snap, z, fs, s, isel, rect);
                    }
                }
                Item::Item(n) => {
                    match &n.zone {
                        Some(zn) if !pl.collapsed => {
                            sp::paint_zone_frame(paint, &snap, z, fs, footer.as_ref(), n, zn, isel, out_wired, rect, probe, count);
                        }
                        _ => {
                            let o = sp::NodeOpts { footer: footer.clone(), lens_step, hovered, carry, out_wired, shown: pl.shown, failed: failed.clone() };
                            sp::paint_node(paint, &snap, z, fs, &o, pl, n, isel, rh, rect);
                            if dim {
                                paint.fill(x, y, w, h, color::with_alpha(snap.theme.panel, 150));
                            }
                        }
                    }
                    if recede {
                        paint.fill(x, y, w, h, color::with_alpha(snap.theme.panel, 170));
                    }
                }
            }
        });
    }
    let drawn_items = tiles.len() as i64;
    let drawn_zones = zones.len() as i64;
    // the rubber band and the expanded chip, over the tiles
    let band: Option<Wiring> = match &t.drag {
        Some(Drag::Wiring(wg)) => Some(wg.clone()),
        _ => None,
    };
    let mut marks: Marks = Vec::new();
    if !(t.carry_lit.is_empty() && t.carry_hot.is_none()) {
        for tl in tiles {
            let (pl, ax, ay) = &t.geo.items[tl.slot];
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
    // the view flag of the displayed node: a tip below it after the pointer rests there
    let mut tip_at: Option<(f64, f64)> = None;
    let mut tip_open = false;
    if let Some(path) = &t.display {
        if let Some((pl, ax, ay)) = t.item_at(path) {
            if matches!(pl.item, Item::Item(_)) && pl.shown != Level::Point {
                let fx = sx(t, *ax) + ((pl.w - sp::HEAD_PAD - 12.0) * z);
                let fy = sy(t, *ay) + (6.0 * z);
                let over = contains((fx, fy, 12.0 * z, 12.0 * z), mouse);
                tip_at = Some((sx(t, *ax) + (116.0 * z), sy(t, *ay) + (32.0 * z)));
                tip_open = over && ui.hover_delay("scope-flag");
            }
        }
    }
    // the red `t` of a header: a tip after the pointer rests on it, saying what it is
    let mut live_tip: Option<(f64, f64)> = None;
    for tl in tiles {
        let (pl, ax, ay) = &t.geo.items[tl.slot];
        if let Item::Item(n) = &pl.item {
            if n.live && pl.shown != Level::Point && ui.hovered_within(&tl.tile) {
                let right = sx(t, *ax) + ((pl.w - sp::HEAD_PAD) * z);
                let (mx, my) = mouse;
                let top = sy(t, *ay);
                if mx >= right - (14.0 * z) && mx <= right + (2.0 * z) && my >= top && my < top + (p::HEAD_HEIGHT * z) && ui.hover_delay("scope-live") {
                    live_tip = Some((right - 150.0, top + (p::HEAD_HEIGHT * z) + 2.0));
                    break;
                }
            }
        }
    }
    let font_size = ui.font_size();
    // the text hovering over an expression row: its glyph and text in a box
    let hover_tip: Option<(String, f64, f64)> = match hover {
        Some(h) => {
            let (pl, _, _) = &t.geo.items[h.slot];
            match &pl.item {
                Item::Item(n) => match &n.rows[h.i].chip {
                    p::Chip::Inline { glyph, text } => {
                        let full = format!("{} {}", sp::shown_glyph(glyph), text);
                        let k = line_of_row(&pl.lines).get(&h.i).copied().unwrap_or(0);
                        let x = sx(t, h.ax + sp::value_x(p::NODE_WIDTH));
                        let y = sy(t, rows_top(n, h.ay) + (k as f64 * p::ROW_HEIGHT));
                        Some((full, x, y))
                    }
                    _ => None,
                },
                _ => None,
            }
        }
        None => None,
    };
    let snap = snapshot;
    let press_point = canvas_signal.press_point;
    ui.draw_over(canvas, move |paint, rect| {
        let (cx, cy, cw, ch) = rect;
        let refused = theme::invalid();
        let hint_chip = |paint: &mut pxui::ui::Paint, bg: color::T, at: (f64, f64), letter: &str| {
            let letter = letter.to_uppercase();
            let size = 8.max(font_size - 2);
            let lw = 14.0f64.max(paint.text_width(Some(size), &letter) + 6.0);
            paint.fill(at.0 - 7.0, at.1 - 7.0, lw, 14.0, bg);
            let tw = paint.text_width(Some(size), &letter);
            let ty = paint.text_top(Some(size), at.1 - 7.0, 14.0);
            paint.text_s((at.0 - 7.0 + ((lw - tw) / 2.0), ty), size, snap.theme.foreground, &letter);
        };
        if let Some(ok) = canvas_hot {
            paint.dashed_rect(cx + 1.5, cy + 1.5, cw - 3.0, ch - 3.0, if ok { snap.theme.accent } else { refused });
        }
        for ((x, y, w, h), letter, hot) in &marks {
            let color = match hot {
                Some(false) => refused,
                _ => snap.theme.accent,
            };
            // a drop target: a dashed accent edge on the card
            if hot.is_some() {
                paint.dashed_rect(x + 0.5, y + 0.5, w - 1.0, h - 1.0, color);
            } else {
                paint.stroke(x + 0.5, y + 0.5, w - 1.0, h - 1.0, color);
            }
            // a letter of the key route: the hint colour under ink, at the card's top left (-7, -7)
            if let Some(letter) = letter {
                hint_chip(paint, if *hot == Some(false) { refused } else { theme::ports(&snap.theme).hint }, (*x, *y), letter);
            }
        }
        // letter hints: a chip per target still matching what was typed, the rest of its letters shown
        if let Some(h) = &snap.hinting {
            for tg in &h.targets {
                if tg.label.starts_with(&h.typed) {
                    let rest = &tg.label[h.typed.len()..];
                    hint_chip(paint, theme::ports(&snap.theme).hint, (sx(&snap, tg.pos.0), sy(&snap, tg.pos.1)), rest);
                }
            }
        }
        if let Some((fx, fy)) = live_tip {
            let size = 4.max(font_size - 2);
            let label = "t  live: reads time or input, re-cooked every frame";
            let tw = paint.text_width(Some(size), label) + 14.0;
            sp::frame_in(paint, fx, fy, tw, 20.0, Some(snap.theme.input), theme::edge(&snap.theme));
            let ty = paint.text_top(Some(size), fy, 20.0);
            paint.text_s((fx + 7.0, ty), size, snap.theme.foreground, label);
        }
        // the displayed node's tip, once the pointer has rested on its flag
        if tip_open {
            if let Some((fx, fy)) = tip_at {
                let size = 4.max(font_size - 2);
                let label = "displayed";
                let key = "V";
                let lw = paint.text_width(Some(size), label);
                let kw = paint.text_width(Some(size), key);
                let tw = 2.0 + 12.0 + lw + 6.0 + kw;
                sp::frame_in(paint, fx, fy, tw, 20.0, Some(snap.theme.input), theme::edge(&snap.theme));
                let ty = paint.text_top(Some(size), fy, 20.0);
                paint.text_s((fx + 7.0, ty), size, snap.theme.foreground, label);
                paint.text_s((fx + 7.0 + lw + 6.0, ty), size, theme::ink_3(&snap.theme), key);
            }
        }
        for wi in &snap.highlighted {
            let pts: Vec<(f64, f64)> = wi.pts.iter().map(|(x, y)| (sx(&snap, *x), sy(&snap, *y))).collect();
            sp::paint_polyline((cx, cy, cw, ch), paint, 2.0, snap.theme.foreground, &pts);
            for (x, y) in [wi.a, wi.b] {
                paint.circle((sx(&snap, x), sy(&snap, y)), 6.0, None, Some(snap.theme.foreground));
            }
        }
        if let Some(wg) = &band {
            if let Some((a, b)) = clip_segment(wg.from, mouse, (cx, cy, cw, ch)) {
                paint.line_w(a, b, 1.5, sp::ty_color(&snap, wg.ty.as_ref()));
            }
        }
        if let Some(Drag::Marquee { .. }) = &snap.drag {
            let (x0, y0) = press_point;
            let (mx, my) = mouse;
            sp::frame_in(paint, x0.min(mx), y0.min(my), (mx - x0).abs(), (my - y0).abs(), Some(theme::tint(&snap.theme)), snap.theme.accent);
        }
        if let Some((full, x, y)) = &hover_tip {
            let size = 4.max(fs - 2);
            let wd = paint.text_width(Some(size), full) + (12.0 * z);
            if wd > sp::FIELD_W * z {
                sp::frame_in(paint, *x, y + (2.0 * z), wd, 20.0 * z, Some(snap.theme.input), theme::edge(&snap.theme));
                let ty = paint.text_top(Some(size), y + (2.0 * z), 20.0 * z);
                paint.text_s((x + (6.0 * z), ty), size, snap.theme.foreground, full);
            }
        }
    });
    (drawn_items, drawn_zones)
}

pub fn update(pressed: &[Key], t: &mut Pane, ui: &mut Ui, frame: &rframe::Frame, changes: &mut Vec<Change>) {
    // the first view shows the whole graph, at zoom 1 when it fits (Home frames everything)
    if !t.framed {
        frame_all(t);
        t.framed = true;
    }
    let canvas = ui.box_(
        "pxui-scope",
        BoxOpts::default().flags(CLICKABLE | SCROLL | CLIP | BLOCKING).w(Size::Px(t.width as f64)).h(Size::Px(t.height as f64)).at((t.x as f64, t.y as f64)),
    );
    let canvas_signal = ui.signal(&canvas);
    step_hints(t, pressed, canvas_signal.subtree_press.is_some(), changes);
    // the grid, zones and wires paint in a child, so the canvas's clip holds them too
    let saved = ui.enter(&canvas);
    let layer = ui.box_("pxui-scope-layer", BoxOpts::default().w(Size::Px(t.width as f64)).h(Size::Px(t.height as f64)).at((0.0, 0.0)));
    ui.leave(saved);
    {
        let (_, wheel) = canvas_signal.scroll;
        let pinch = canvas_signal.pinch;
        if !((wheel == 0.0 && pinch == 1.0) || t.context.is_some()) {
            let (mx, my) = frame.input.pointer;
            // the wheel steps by a tenth; a pinch carries its own factor
            let zoom = 0.25f64.max(2.5f64.min(t.zoom * (1.0 + (wheel * 0.1)) * pinch));
            let k = zoom / t.zoom;
            let pan_x = mx - t.x as f64 - ((mx - t.x as f64 - t.pan_x) * k);
            let pan_y = my - t.y as f64 - ((my - t.y as f64 - t.pan_y) * k);
            (t.zoom, t.pan_x, t.pan_y) = (zoom, pan_x, pan_y);
        }
    }
    // a point's box is as wide as its name, drawn at the zoom's font
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
    // a drag in progress moves its items in a fresh geometry
    if let Some(scope) = t.scope.clone() {
        match t.drag.clone() {
            Some(Drag::Moving { paths, dx, dy, moved }) if moved => {
                let moving: PathSet = paths.into_iter().collect();
                let layout = t.layout.clone();
                regeo(t, &scope, &layout, &|q: &[Symbol]| if moving.contains(q) { (dx, dy) } else { (0.0, 0.0) });
            }
            Some(Drag::Carrying { paths, dx, dy, .. }) if dx.hypot(dy) > 0.0 => {
                let moving: PathSet = paths.into_iter().collect();
                let layout = t.layout.clone();
                regeo(t, &scope, &layout, &|q: &[Symbol]| if moving.contains(q) { (dx, dy) } else { (0.0, 0.0) });
            }
            _ => {}
        }
    }
    let z = t.zoom;
    // text follows the zoom down to 5 points (a 6-point row at zoom 0.25 holds it)
    let fs = font_of(z);
    let viewport = (t.x as f64, t.y as f64, t.width as f64, t.height as f64);
    // what the pane shows, grown by `margin` screen points, in graph units: the indices answer it
    let in_view = |t: &Pane, margin: f64| -> (f64, f64, f64, f64) {
        ((-t.pan_x - margin) / z, (-t.pan_y - margin) / z, (t.width as f64 - t.pan_x + margin) / z, (t.height as f64 - t.pan_y + margin) / z)
    };
    let visible: Vec<usize> = index_query(&t.geo.tiles, in_view(t, 20.0))
        .into_iter()
        .filter(|&i| {
            let (pl, ax, ay) = &t.geo.items[i];
            let x = sx(t, *ax) - 20.0;
            let y = sy(t, *ay) - 20.0;
            let wd = (pl.w * z) + 40.0;
            let h = (pl.h * z) + 40.0;
            let (vx, vy, vw, vh) = viewport;
            x < vx + vw && x + wd > vx && y < vy + vh && y + h > vy
        })
        .collect();
    let wires_in_view = index_query(&t.geo.reach, in_view(t, 4.0));
    let zones: Vec<usize> = visible
        .iter()
        .copied()
        .filter(|&i| {
            let (pl, _, _) = &t.geo.items[i];
            matches!(&pl.item, Item::Item(n) if n.zone.is_some()) && !pl.collapsed
        })
        .collect();
    let mouse = frame.input.pointer;
    // Segment rectangles live in the shared hit tree, behind the cards; ports built below take hover
    // precedence. A segment is cut to `clip` (pane points) first, so what is out of view costs one clip;
    // a diagonal is then a run of 16-point boxes.
    let chain = chains_of(t);
    let mut ctx = Ctx { wire_hits: HashMap::new(), finished: false, opened: None, fs };
    let saved = ui.enter(&canvas);
    for &i in &wires_in_view {
        wire_boxes(&mut ctx, t, ui, (0.0, 0.0), "wire", (0.0, 0.0, t.width as f64, t.height as f64), i);
    }
    ui.leave(saved);
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
    // tiles
    let saved = ui.enter(&canvas);
    let mut tiles: Vec<Tile> = Vec::new();
    for &i in &visible {
        tiles.push(build_tile(&mut ctx, t, ui, z, fs, i));
    }
    ui.leave(saved);
    if ctx.finished {
        t.editing = None;
    }
    if let Some(c) = drop_change(t, ui, &canvas, &tiles) {
        changes.push(c);
    }
    frames_step(t, ui, &mut ctx, z, &canvas, changes);
    if ctx.finished { t.editing = None; }
    if let Some(editing) = ctx.opened.take() { t.editing = Some(editing); }
    let hover = hover_step(t, ui, z, mouse, &tiles, &ctx, &wires_in_view);
    pointer_step(t, ui, frame, &canvas, &canvas_signal, &tiles, &ctx, &wires_in_view, changes);
    wire_pull(t, ui, &ctx, z, mouse, &tiles, &wires_in_view, changes);
    let (lens_next, lens_changed) = tile_gestures(t, ui, z, mouse, &tiles, changes);
    context_step(t, ui, &tiles, &canvas_signal, changes);
    let (drawn_items, drawn_zones) = paint_frame(t, ui, z, fs, &layer, &canvas, &canvas_signal, &tiles, &zones, &wires_in_view, &footers, &chain, mouse, &hover);
    { t.stats = Stats { drawn_items, drawn_zones, ..t.stats }; };
    // an opened or stepped expansion panel changes its card's size: lay out again for the next frame
    if lens_changed {
        t.lens = lens_next;
        relayout(None, t);
    }
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

    pub fn box_of(t: &Pane, path: &[Symbol]) -> Option<(f64, f64, f64, f64)> {
        box_at(t, path).map(|(x, y, w, h)| (sx(t, x), sy(t, y), w * t.zoom, h * t.zoom))
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
fn bxz(ui: &mut Ui, z: f64, name: &str, pos: (f64, f64), size: (f64, f64)) -> UiBox {
    box_in(ui, CLICKABLE, size.0 * z, size.1 * z, (pos.0 * z, pos.1 * z), name)
}
