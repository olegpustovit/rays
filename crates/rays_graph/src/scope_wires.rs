// Inner scopes retain their existing paint and hit priority over ancestor frames.
// the pane's geometry (items, wires, curve sampling) and the
// bucket indices over tiles and wire bounds.
use flow_syntax::symbol::Symbol;
use flow_check::workspace::Path;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit as e;
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level, Placed, ScopeResult, ZoneKind};
use flow_syntax::syntax::{self as s, Syntax};
use flow_syntax::ty::Ty;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub type S = Syntax;

pub const TAG_SPAN: f64 = 3.0 * 288.0;
pub const INDEX_CELL: f64 = 256.0;

/// a cell's key: one int (cells are far fewer than 32768 high)
pub fn cell_key(cx: i64, cy: i64) -> i64 {
    cx.wrapping_mul(65536).wrapping_add(cy)
}

/// A bucket grid over boxes: a box over many cells is asked every time.
#[derive(Clone, Debug, Default)]
pub struct Index {
    pub buckets: HashMap<i64, Vec<usize>>,
    pub large: Vec<usize>,
}

fn cell(v: f64) -> i64 {
    (v / INDEX_CELL).floor() as i64
}

pub fn index_of(n: usize, boxf: &dyn Fn(usize) -> (f64, f64, f64, f64)) -> Index {
    let mut buckets: HashMap<i64, Vec<usize>> = HashMap::new();
    let mut large = Vec::new();
    for i in 0..n {
        let (x0, y0, x1, y1) = boxf(i);
        if (cell(x1) - cell(x0) + 1) * (cell(y1) - cell(y0) + 1) > 64 {
            large.push(i);
        } else {
            for cx in cell(x0)..=cell(x1) {
                for cy in cell(y0)..=cell(y1) {
                    buckets.entry(cell_key(cx, cy)).or_default().push(i);
                }
            }
        }
    }
    Index { buckets, large }
}

/// The boxes that may meet the rectangle, in index order, each once.
pub fn index_query(ix: &Index, r: (f64, f64, f64, f64)) -> Vec<usize> {
    let mut found = Vec::new();
    index_query_into(ix, r, &mut found);
    found
}

/// `index_query` into a kept list (cleared first).
pub fn index_query_into(ix: &Index, r: (f64, f64, f64, f64), found: &mut Vec<usize>) {
    let (x0, y0, x1, y1) = r;
    found.clear();
    found.extend_from_slice(&ix.large);
    if (cell(x1) - cell(x0) + 1) * (cell(y1) - cell(y0) + 1) >= ix.buckets.len() as i64 {
        for l in ix.buckets.values() {
            found.extend_from_slice(l);
        }
    } else {
        for cx in cell(x0)..=cell(x1) {
            for cy in cell(y0)..=cell(y1) {
                if let Some(l) = ix.buckets.get(&cell_key(cx, cy)) {
                    found.extend_from_slice(l);
                }
            }
        }
    }
    found.sort_unstable();
    found.dedup();
}

#[derive(Clone, Debug, PartialEq)]
pub struct Src {
    pub pos: (f64, f64),
    pub ty: Option<Ty>,
    pub owner: Option<Path>,
}

pub type Target = (Path, ArgKey, Option<S>);

#[derive(Clone, Debug, PartialEq)]
pub struct Wire {
    pub a: (f64, f64),
    pub b: (f64, f64),
    pub ty: Option<Ty>,
    pub target: Option<Target>,
    /// the drawn polyline, `a` .. `b`: straight, or bent clear of the cards
    pub pts: Vec<(f64, f64)>,
    /// a name read inside a formula, not the value a row takes: drawn faint until lit
    pub refers: bool,
    /// the node (or zone) it reads from, and the one it ends at: the dataflow chains
    pub from: Option<Path>,
    pub into: Option<Path>,
    /// it leaves a begin bar's port down the bar (`curve_points` with `drop`)
    pub drop: bool,
}

/// A float point as a hash key (0.0 and -0.0 are one point).
pub fn fkey(pt: (f64, f64)) -> (u64, u64) {
    ((pt.0 + 0.0).to_bits(), (pt.1 + 0.0).to_bits())
}

pub type Item3 = (Placed, f64, f64);

#[derive(Clone, Debug)]
pub struct Geo {
    /// absolute top-left, graph units
    pub items: Rc<Vec<Item3>>,
    pub wires: Rc<Vec<Wire>>,
    /// where each scope's own coordinates start
    pub origins: Rc<Vec<(Path, (f64, f64))>>,
    /// the wires that end at a node of a zone's body, by the zone's path
    pub inside: Rc<HashMap<Path, Vec<usize>>>,
    /// an item's place in `items`
    pub slot: Rc<HashMap<Path, usize>>,
    pub tiles: Rc<Index>,
    pub reach: Rc<Index>,
    /// the points a wire starts from: an out-port there is wired
    pub read: Rc<HashSet<(u64, u64)>>,
}

pub fn empty_geo() -> Geo {
    Geo {
        items: Rc::new(Vec::new()),
        wires: Rc::new(Vec::new()),
        origins: Rc::new(Vec::new()),
        inside: Rc::new(HashMap::new()),
        slot: Rc::new(HashMap::new()),
        tiles: Rc::new(Index::default()),
        reach: Rc::new(Index::default()),
        read: Rc::new(HashSet::new()),
    }
}

pub fn root_name(name: &str) -> &str {
    match name.find('.') {
        Some(i) => &name[..i],
        None => name,
    }
}

/// What a row reads once its wire is taken off.
pub fn fallback(r: &p::Row) -> Option<S> {
    if r.kind == p::RowKind::Rest {
        return None;
    }
    match &r.key {
        ArgKey::Kw(_) | ArgKey::Field(_) => r.default.as_ref().and_then(|d| match s::parse(d) {
            Ok(mut forms) if forms.len() == 1 => Some(forms.remove(0)),
            _ => None,
        }),
        _ => r.ty.as_ref().and_then(|ty| e::default_for(ty, &r.label, None)),
    }
}

pub const WIRE_HEAD_Y: f64 = 11.75;
pub const WIRE_ROW_Y: f64 = 12.75;
pub const BAR_X: f64 = 20.5;

/// Index of the zone's label row in `z.rail` (its first loop variable).
pub fn label_index(z: &p::Zone) -> Option<usize> {
    z.rail.iter().position(|r| r.role == p::Role::Var)
}

/// The indices into `z.rail` of the rows drawn under the label row.
pub fn extra_indices(z: &p::Zone) -> Vec<usize> {
    let label = label_index(z);
    (0..z.rail.len()).filter(|i| z.rail[*i].role != p::Role::Capture && Some(*i) != label).collect()
}

/// A begin bar's port for the rail row `ri` (an index into `z.rail`). Tile-relative.
pub fn begin_port(n: &p::Node, z: &p::Zone, ri: usize) -> (f64, f64) {
    if z.kind == ZoneKind::For && label_index(z) == Some(ri) {
        return (12.0, 36.0);
    }
    match extra_indices(z).iter().position(|x| *x == ri) {
        Some(k) => (BAR_X, p::HEAD_HEIGHT + 24.0 * k as f64 + 12.0),
        None => (BAR_X, p::rail_top(n) - 14.0),
    }
}

pub fn end_port(n: &p::Node, w: f64) -> (f64, f64) {
    (w - BAR_X, p::rail_top(n) + 12.0)
}

pub fn note_rows(n: &p::Node) -> f64 {
    if n.note.is_some() { 1.0 } else { 0.0 }
}

pub fn rows_top(n: &p::Node, ay: f64) -> f64 {
    ay + p::BODY_TOP + note_rows(n) * p::ROW_HEIGHT
}

/// The line each row of a card sits on, by row index.
pub fn line_of_row(lines: &[p::Line]) -> HashMap<usize, usize> {
    let mut tbl = HashMap::new();
    for (k, l) in lines.iter().enumerate() {
        if let p::Line::Row(i, _) = l {
            tbl.insert(*i, k);
        }
    }
    tbl
}

/// The part of a segment inside a rectangle, as the parameters of its two ends (Liang-Barsky);
/// `t0 > t1` when it misses.
pub fn clip_span(x0: f64, y0: f64, x1: f64, y1: f64, xmin: f64, ymin: f64, xmax: f64, ymax: f64) -> (f64, f64) {
    let dx = x1 - x0;
    let dy = y1 - y0;
    let mut t0 = 0.0;
    let mut t1 = 1.0;
    let mut ok = true;
    let mut clip = |pp: f64, q: f64| {
        if pp == 0.0 {
            if q < 0.0 {
                ok = false;
            }
        } else {
            let r = q / pp;
            if pp < 0.0 {
                if r > t1 {
                    ok = false;
                } else if r > t0 {
                    t0 = r;
                }
            } else if r < t0 {
                ok = false;
            } else if r < t1 {
                t1 = r;
            }
        }
    };
    clip(-dx, x0 - xmin);
    clip(dx, xmax - x0);
    clip(-dy, y0 - ymin);
    clip(dy, ymax - y0);
    if ok { (t0, t1) } else { (1.0, 0.0) }
}

pub type Seg = ((f64, f64), (f64, f64));

pub fn clip_segment(a: (f64, f64), b: (f64, f64), r: (f64, f64, f64, f64)) -> Option<Seg> {
    let (x0, y0) = a;
    let (x1, y1) = b;
    let (rx, ry, rw, rh) = r;
    let (t0, t1) = clip_span(x0, y0, x1, y1, rx, ry, rx + rw, ry + rh);
    if t0 <= t1 {
        Some(((x0 + (x1 - x0) * t0, y0 + (y1 - y0) * t0), (x0 + (x1 - x0) * t1, y0 + (y1 - y0) * t1)))
    } else {
        None
    }
}

fn bernstein(i: usize) -> (f64, f64, f64, f64) {
    let t = i as f64 / 16.0;
    let u = 1.0 - t;
    (u * u * u, 3.0 * u * u * t, 3.0 * u * t * t, t * t * t)
}

/// A wire is a 12-point lead out of and into each port and a cubic between them whose pull is capped.
/// `drop`: from a begin bar's port, down along the bar, then level into the row.
pub fn curve_points(drop: bool, a: (f64, f64), b: (f64, f64)) -> Vec<(f64, f64)> {
    let mut out = Vec::new();
    curve_into(drop, a, b, &mut out);
    out
}

/// `curve_points` appended to `out`.
pub fn curve_into(drop: bool, a: (f64, f64), b: (f64, f64), out: &mut Vec<(f64, f64)>) {
    let (ax, ay) = a;
    let (bx, by) = b;
    if drop && by > ay + 12.0 && bx > ax {
        let k = (by - ay).min(bx - ax);
        out.extend((0..17).map(|i| {
            let (b0, b1, b2, b3) = bernstein(i);
            (((b0 + b1) * ax) + (b2 * (bx - k)) + (b3 * bx), (b0 * ay) + (b1 * (ay + k)) + ((b2 + b3) * by))
        }));
    } else {
        let dx = bx - ax;
        let lead = if dx.abs() < 24.0 { 0.0 } else { 12.0 };
        let x0 = ax + lead;
        let x1 = bx - lead;
        let k = if dx >= 24.0 {
            96.0f64.min(12.0f64.max((x1 - x0) / 2.0))
        } else if dx > -24.0 {
            12.0
        } else {
            160.0f64.min(48.0f64.max((by - ay).abs() / 2.0))
        };
        if lead != 0.0 {
            out.push((ax, ay));
        }
        out.extend((0..17).map(|i| {
            let (b0, b1, b2, b3) = bernstein(i);
            ((b0 * x0) + (b1 * (x0 + k)) + (b2 * (x1 - k)) + (b3 * x1), ((b0 + b1) * ay) + ((b2 + b3) * by))
        }));
        if lead != 0.0 {
            out.push((bx, by));
        }
    }
}

fn len(a: (f64, f64), b: (f64, f64)) -> f64 {
    (b.0 - a.0).hypot(b.1 - a.1)
}

fn toward(a: (f64, f64), b: (f64, f64), d: f64) -> (f64, f64) {
    let l = len(a, b);
    (a.0 + (b.0 - a.0) * d / l, a.1 + (b.1 - a.1) * d / l)
}

/// A polyline of axis-aligned legs with its corners rounded (radius 8, less on short legs), appended
/// to `out`.
pub fn rounded_into(pts: &[(f64, f64)], out: &mut Vec<(f64, f64)>) {
    let Some(&first) = pts.first() else { return };
    out.push(first);
    let mut pp = first;
    for k in 1..pts.len().saturating_sub(1) {
        let (c, n) = (pts[k], pts[k + 1]);
        let r = 8.0f64.min(len(pp, c).min(len(c, n)) / 2.0);
        let i = toward(c, pp, r);
        let o = toward(c, n, r);
        for q in 0..5 {
            let t = q as f64 / 4.0;
            let u = 1.0 - t;
            out.push((u * u * i.0 + 2.0 * u * t * c.0 + t * t * o.0, u * u * i.1 + 2.0 * u * t * c.1 + t * t * o.1));
        }
        pp = o;
    }
    if pts.len() >= 2 {
        out.push(pts[pts.len() - 1]);
    }
}

pub fn out_anchor(pl: &Placed, ax: f64, ay: f64) -> (f64, f64) {
    if pl.shown == Level::Point { (ax + pl.w, ay + p::POINT_SIZE / 2.0) } else { (ax + pl.w, ay + WIRE_HEAD_Y) }
}

pub fn in_anchor(pl: &Placed, ax: f64, ay: f64) -> (f64, f64) {
    if pl.shown == Level::Point { (ax, ay + p::POINT_SIZE / 2.0) } else { (ax, ay + WIRE_HEAD_Y) }
}

/// The level a card is drawn at: the one it was given, at every zoom. A zone and the return card keep
/// their card.
pub fn shown_of(pl: &Placed) -> Level {
    match &pl.item {
        Item::Item(n) if n.zone.is_some() => Level::Card,
        Item::Return => Level::Card,
        _ => pl.level,
    }
}

pub fn point_name(pl: &Placed) -> String {
    match &pl.item {
        Item::Item(n) => p::point_title(n),
        Item::Input(i) => i.name.clone(),
        Item::Return => "return".to_owned(),
    }
}

pub fn point_box_w(name_w: &dyn Fn(&str) -> f64, name: &str) -> f64 {
    p::POINT_SIZE + 8.0 + name_w(name)
}

fn on_path(o: &Option<Path>, pth: &Path) -> bool {
    matches!(o, Some(x) if x == pth)
}

struct Items {
    items: Vec<Item3>,
    slot: HashMap<Path, usize>,
    tiles: Index,
}

impl Items {
    fn at(&self, path: &Path) -> Option<&Item3> {
        self.slot.get(path).map(|i| &self.items[*i])
    }
}

struct Out {
    wires: Vec<Wire>,
    bars: HashSet<(u64, u64)>,
    scratch: Scratch,
}

struct Walk {
    items: Vec<Item3>,
    slot: HashMap<Path, usize>,
    origins: Vec<(Path, (f64, f64))>,
}

fn walk(name_w: &dyn Fn(&str) -> f64, shown: &dyn Fn(&Placed) -> Level, shift: &dyn Fn(&[Symbol]) -> (f64, f64), w: &mut Walk, ox: f64, oy: f64, l: &p::Layout) {
    for pl in &l.placed {
        let (dx, dy) = shift(&pl.path);
        let ax = ox + pl.x + dx;
        let ay = oy + pl.y + dy;
        let lv = shown(pl);
        let mut q = pl.clone();
        let mut ay2 = ay;
        q.shown = lv;
        match lv {
            Level::Point => {
                q.w = point_box_w(name_w, &point_name(pl));
                q.h = p::POINT_SIZE;
                ay2 = if pl.level == Level::Point && !matches!(pl.item, Item::Input(_)) { ay } else { ay + 5.0 };
            }
            Level::Chip => {
                q.h = p::HEAD_HEIGHT;
            }
            _ => {}
        }
        w.slot.insert(q.path.clone(), w.items.len());
        if let (Some(inner), Item::Item(n)) = (&pl.inner, &pl.item) {
            w.items.push((q, ax, ay2));
            let pad = match &n.zone {
                Some(z) => p::zone_pad_in(z),
                None => p::ZONE_PAD_X,
            };
            w.origins.push((n.path.clone(), (ax + pad, ay2 + p::rail_top(n))));
            walk(name_w, shown, shift, w, ax + pad, ay2 + p::rail_top(n), inner);
        } else {
            w.items.push((q, ax, ay2));
        }
    }
}

/// Lists kept between routes, so a drag re-routing its wires allocates nothing once warm.
#[derive(Default)]
pub struct Scratch {
    cand: Vec<usize>,
    blockers: Vec<(f64, f64, f64, f64)>,
    merged: Vec<(f64, f64, f64, f64)>,
    way: Vec<(f64, f64)>,
    pts: Vec<(f64, f64)>,
}

/// The cards a wire from `a` to `b` bends clear of: `cards` lists those that may meet a rectangle,
/// `item` is one card and where it sits. Appended to `out`.
fn route_into<'g>(cards: &mut dyn FnMut((f64, f64, f64, f64), &mut Vec<usize>), item: &dyn Fn(usize) -> (&'g Placed, f64, f64), a: (f64, f64), b: (f64, f64), from: &Option<Path>, into: &Option<Path>, s: &mut Scratch, out: &mut Vec<(f64, f64)>) {
    let (ax, ay) = a;
    let (bx, by) = b;
    let inside = |pt: (f64, f64), pl: &Placed, cx: f64, cy: f64| pt.0 >= cx && pt.0 <= cx + pl.w && pt.1 >= cy && pt.1 <= cy + pl.h;
    s.pts.clear();
    curve_into(false, (ax, ay), (bx, by), &mut s.pts);
    s.blockers.clear();
    cards((ax.min(bx), ay.min(by), ax.max(bx), ay.max(by)), &mut s.cand);
    for &i in &s.cand {
        let (pl, cx, cy) = item(i);
        if on_path(from, &pl.path) || on_path(into, &pl.path) || inside((ax, ay), pl, cx, cy) || inside((bx, by), pl, cx, cy) || matches!(pl.item, Item::Return) {
            continue;
        }
        // a zone a wire does not enter is an obstacle too, kept a lattice step clear
        let m = if pl.inner.is_some() { 12.0 } else { 0.0 };
        let boxr = (cx - m, cy - m, pl.w + 2.0 * m, pl.h + 2.0 * m);
        if s.pts.windows(2).any(|w| clip_segment(w[0], w[1], (cx, cy, pl.w, pl.h)).is_some()) {
            s.blockers.push(boxr);
        }
    }
    // (blockers at one x merge into one obstacle whatever their order)
    s.blockers.sort_unstable_by(|x, y| x.0.total_cmp(&y.0));
    // cards stacked in the same columns are one obstacle
    s.merged.clear();
    for &(cx, cy, w, h) in &s.blockers {
        match s.merged.last().copied() {
            Some((px, py, pw, ph)) if cx < px + pw => {
                let x1 = (px + pw).max(cx + w);
                let y1 = (py + ph).max(cy + h);
                let y0 = py.min(cy);
                *s.merged.last_mut().unwrap() = (px, y0, x1 - px, y1 - y0);
            }
            _ => s.merged.push((cx, cy, w, h)),
        }
    }
    if s.merged.is_empty() {
        out.extend_from_slice(&s.pts);
        return;
    }
    s.way.clear();
    for (cx, cy, w, h) in &s.merged {
        let mx = cx + w / 2.0;
        let line = if bx == ax { ay } else { ay + ((by - ay) * (mx - ax) / (bx - ax)) };
        let y = if line <= cy + h / 2.0 { cy - 12.0 } else { cy + h + 12.0 };
        // level past both sides of the card
        s.way.push((cx - 12.0, y));
        s.way.push((cx + w + 12.0, y));
    }
    // a stop behind an end would send the wire back
    s.way.retain(|(x, _)| *x > ax + 12.0 && *x < bx - 12.0);
    let mut from_pt = (ax, ay);
    for (k, w) in s.way.iter().copied().chain(std::iter::once((bx, by))).enumerate() {
        // the first leg whole, each later one without its first point (the stop before it)
        let start = out.len();
        curve_into(false, from_pt, w, out);
        if k > 0 {
            out.remove(start);
        }
        from_pt = w;
    }
}

/// A card stacked above the card it feeds is joined to it by an elbow: its corners, from the source
/// card `o` and the fed card `i` (each with where it sits).
fn spine_at(o: (&Placed, f64, f64), i: (&Placed, f64, f64), a: (f64, f64), b: (f64, f64)) -> Option<[(f64, f64); 6]> {
    let ((op, ox, oy), (ip, ix, iy)) = (o, i);
    if ix < ox + op.w - 24.0 && ix + ip.w > ox + 24.0 && iy >= oy + op.h && iy - (oy + op.h) <= 3.0 * p::LATTICE {
        let (ax, ay) = a;
        let (bx, by) = b;
        let x1 = ax + 12.0;
        let x2 = bx - 12.0;
        let gy = (oy + op.h + iy) / 2.0;
        Some([(ax, ay), (x1, ay), (x1, gy), (x2, gy), (x2, by), (bx, by)])
    } else {
        None
    }
}

fn spine(items: &Items, src: &Src, into: &Path, b: (f64, f64)) -> Option<[(f64, f64); 6]> {
    let o = items.at(src.owner.as_ref()?)?;
    let i = items.at(into)?;
    spine_at((&o.0, o.1, o.2), (&i.0, i.1, i.2), src.pos, b)
}

fn add_wire(items: &Items, out: &mut Out, target: Option<Target>, refers: bool, src: Option<&Src>, b: (f64, f64), into: &Path) {
    let s = match src {
        Some(s) => s,
        None => return,
    };
    let mut pts = Vec::new();
    let drop = out.bars.contains(&fkey(s.pos));
    if let Some(corners) = spine(items, s, into, b) {
        rounded_into(&corners, &mut pts);
        let (a, b) = (pts[0], *pts.last().unwrap());
        out.wires.push(Wire { a, b, ty: s.ty.clone(), target, pts, refers, from: s.owner.clone(), into: Some(into.clone()), drop });
        return;
    } else if drop || (b.0 - s.pos.0).abs() > TAG_SPAN {
        curve_into(drop, s.pos, b, &mut pts);
    } else {
        route_into(&mut |r, found| index_query_into(&items.tiles, r, found), &|i| { let (pl, x, y) = &items.items[i]; (pl, *x, *y) }, s.pos, b, &s.owner, &Some(into.clone()), &mut out.scratch, &mut pts);
    }
    out.wires.push(Wire { a: s.pos, b, ty: s.ty.clone(), target, pts, refers, from: s.owner.clone(), into: Some(into.clone()), drop });
}

/// Wire `i` of `geo` with the cards at `moving` (slots, ascending; none inside another) moved by `d`:
/// its ends follow their cards and it bends clear of the cards as `compute` would bend it, into
/// `out` (cleared first). Allocates nothing once `s` and `out` are warm.
pub fn reroute(geo: &Geo, i: usize, moving: &[usize], d: (f64, f64), s: &mut Scratch, out: &mut Vec<(f64, f64)>) {
    out.clear();
    let wi = &geo.wires[i];
    let moves = |k: usize| moving.binary_search(&k).is_ok();
    let slot = |path: &Option<Path>| path.as_ref().and_then(|q| geo.slot.get(q).copied());
    let shift = |k: Option<usize>| if k.is_some_and(moves) { d } else { (0.0, 0.0) };
    let (o, k) = (slot(&wi.from), slot(&wi.into));
    let (fa, fb) = (shift(o), shift(k));
    let (a, b) = ((wi.a.0 + fa.0, wi.a.1 + fa.1), (wi.b.0 + fb.0, wi.b.1 + fb.1));
    let at = |k: usize| {
        let (pl, x, y) = &geo.items[k];
        let (dx, dy) = shift(Some(k));
        (pl, x + dx, y + dy)
    };
    if let (Some(o), Some(k)) = (o, k) {
        if let Some(corners) = spine_at(at(o), at(k), a, b) {
            rounded_into(&corners, out);
            return;
        }
    }
    if wi.drop || (b.0 - a.0).abs() > TAG_SPAN {
        curve_into(wi.drop, a, b, out);
        return;
    }
    // the index holds the cards where they were: a moved card is asked for wherever it went
    let mut cards = |r: (f64, f64, f64, f64), found: &mut Vec<usize>| {
        index_query_into(&geo.tiles, r, found);
        found.retain(|&k| !moves(k));
        found.extend_from_slice(moving);
        found.sort_unstable();
    };
    route_into(&mut cards, &at, a, b, &wi.from, &wi.into, s, out);
}

type Table = HashMap<String, Src>;

fn resolve(chain: &[Table], name: &str) -> Option<Src> {
    for tbl in chain.iter().rev() {
        if let Some(s) = tbl.get(name) {
            return Some(s.clone());
        }
        if let Some(s) = tbl.get(root_name(name)) {
            return Some(s.clone());
        }
    }
    None
}

fn names_of(e: &Option<S>) -> Vec<String> {
    match e {
        Some(e) => e::free_names(e),
        None => Vec::new(),
    }
}

/// A plain `for` has no out-what it yields wires from the yielded card.
fn inner_out(items: &Items, n: &p::Node) -> Option<((f64, f64), Path)> {
    let z = n.zone.as_ref()?;
    if z.kind != ZoneKind::For {
        return None;
    }
    if let Some((pl, _, _)) = items.at(&n.path) {
        if pl.collapsed {
            return None;
        }
    }
    let target: Option<Path> = match &z.scope.result {
        ScopeResult::Link(l) => z.scope.nodes.iter().find(|m| m.binds.iter().any(|b| b == root_name(l))).map(|m| m.path.clone()),
        ScopeResult::Node(pth) => Some(pth.clone()),
        ScopeResult::Literal(_) => None,
    };
    let path = target?;
    let (pl, ax, ay) = items.at(&path)?;
    Some((out_anchor(pl, *ax, *ay), path))
}

fn declared(items: &Items, sc: &p::Scope) -> Table {
    let mut tbl: Table = HashMap::new();
    for i in &sc.inputs {
        if let Some((pl, ax, ay)) = items.at(&i.path) {
            tbl.insert(i.name.clone(), Src { pos: out_anchor(pl, *ax, *ay), ty: Some(i.ty.clone()), owner: Some(i.path.clone()) });
        }
    }
    for n in &sc.nodes {
        if let Some((pl, ax, ay)) = items.at(&n.path) {
            let (ax, ay) = (*ax, *ay);
            let w = pl.w;
            let lines = pl.lines.len();
            let (main, main_owner) = match inner_out(items, n) {
                Some((pos, owner)) => (pos, owner),
                None => (out_anchor(pl, ax, ay), n.path.clone()),
            };
            // a point or a chip has the one out end; its named outputs are listed on a card
            let out = |j: usize| -> (f64, f64) {
                if pl.shown == Level::Point || pl.shown == Level::Chip {
                    main
                } else {
                    (ax + w, rows_top(n, ay) + (((lines + j) as f64 + 0.5) * p::ROW_HEIGHT) + 0.75)
                }
            };
            if n.binds.len() == 1 && n.binds[0] == n.name {
                let b = &n.binds[0];
                tbl.insert(b.clone(), Src { pos: main, ty: Some(n.ty.clone()), owner: Some(main_owner) });
                for (j, (f, ty)) in n.outputs.iter().enumerate() {
                    tbl.insert(format!("{}.{}", b, f), Src { pos: out(j), ty: Some(ty.clone()), owner: Some(n.path.clone()) });
                }
            } else {
                for (j, (f, ty)) in n.outputs.iter().enumerate() {
                    tbl.insert(f.clone(), Src { pos: out(j), ty: Some(ty.clone()), owner: Some(n.path.clone()) });
                }
            }
        }
    }
    tbl
}

fn scope_wires(items: &Items, out: &mut Out, chain: &mut Vec<Table>, sc: &p::Scope) {
    chain.push(declared(items, sc));
    for n in &sc.nodes {
        let (pl, ax, ay) = match items.at(&n.path) {
            Some((pl, ax, ay)) => (pl, *ax, *ay),
            None => continue,
        };
        let w = pl.w;
        match &n.zone {
            None => {
                for (i, r) in n.rows.iter().enumerate() {
                    let target_pos = if r.head {
                        in_anchor(pl, ax, ay)
                    } else {
                        match pl.shown {
                            Level::Point | Level::Chip => in_anchor(pl, ax, ay),
                            _ => match pl.lines.iter().position(|l| matches!(l, p::Line::Row(j, _) if *j == i)) {
                                Some(k) => (ax, rows_top(n, ay) + (k as f64 * p::ROW_HEIGHT) + WIRE_ROW_Y),
                                None => (ax, ay + WIRE_HEAD_Y),
                            },
                        }
                    };
                    // a binding cannot read itself: a name that resolves to this node is one from outside
                    // its scope (a `ref` to a graph of the same name), not a wire
                    let names = p::sources(r);
                    if names.is_empty() {
                        continue;
                    }
                    let target0: Option<Target> = Some((n.path.clone(), r.key.clone(), fallback(r)));
                    for name in &names {
                        // a call in a vector's cell: the wire ends at that cell of the vector
                        let target = match r.cells.iter().find(|(_, leaf)| leaf == name) {
                            Some((ci, _)) => Some(p::cell_wire(n, r, *ci)),
                            None => target0.clone(),
                        };
                        let src = resolve(chain, name);
                        if let Some(Src { owner: Some(o), .. }) = &src {
                            if *o == n.path {
                                continue;
                            }
                        }
                        let refers = matches!(r.chip, p::Chip::Inline { .. });
                        add_wire(items, out, target, refers, src.as_ref(), target_pos, &n.path);
                    }
                }
            }
            Some(z) => {
                let collapsed = pl.collapsed;
                let label = label_index(z);
                let extras = extra_indices(z);
                let rail_pos = |ri: usize| -> (f64, f64) {
                    if collapsed {
                        (ax, ay + p::BODY_TOP + ri as f64 * p::ROW_HEIGHT + WIRE_ROW_Y)
                    } else if label == Some(ri) {
                        (ax, ay + WIRE_HEAD_Y)
                    } else {
                        let i = extras.iter().position(|x| *x == ri).unwrap_or(0);
                        (ax, ay + p::HEAD_HEIGHT + i as f64 * p::ROW_HEIGHT + WIRE_ROW_Y)
                    }
                };
                for (ri, r) in z.rail.iter().enumerate() {
                    let plain_label = !collapsed && z.kind == ZoneKind::For && label == Some(ri);
                    if (collapsed || r.role != p::Role::Capture) && !plain_label {
                        let names = if r.role == p::Role::Capture { vec![r.name.clone()] } else { names_of(&r.expr) };
                        let target: Option<Target> = r.key.as_ref().map(|key| (n.path.clone(), key.clone(), None));
                        for name in names {
                            let src = resolve(chain, &name);
                            add_wire(items, out, target.clone(), false, src.as_ref(), rail_pos(ri), &n.path);
                        }
                    }
                }
                if !collapsed {
                    // the loop's variables: a port in the zone's left strip (the label's), or at the right
                    // of the rail row
                    let mut rail: Table = HashMap::new();
                    for (ri, r) in z.rail.iter().enumerate() {
                        if r.role != p::Role::Capture {
                            let (bx, by) = begin_port(n, z, ri);
                            let pos = (ax + bx, ay + by);
                            let src = Src { pos, ty: r.ty.clone(), owner: Some(n.path.clone()) };
                            // a loop's variables leave down the bar; a function's parameters are ordinary
                            // left-to-right wires
                            if z.kind != ZoneKind::Fn {
                                out.bars.insert(fkey(pos));
                            }
                            for name in &r.names {
                                rail.insert(name.clone(), src.clone());
                            }
                        }
                    }
                    chain.push(rail);
                    scope_wires(items, out, chain, &z.scope);
                    chain.push(declared(items, &z.scope));
                    let source = match &z.scope.result {
                        ScopeResult::Link(l) => resolve(chain, l),
                        ScopeResult::Node(_) => resolve(chain, "@result"),
                        ScopeResult::Literal(_) => None,
                    };
                    chain.pop();
                    chain.pop();
                    // the body's result ends at the end bar; a fold's goes round to its accumulator from
                    // there, so no wire runs back across the body
                    if z.kind != ZoneKind::For {
                        let (ex, ey) = end_port(n, w);
                        add_wire(items, out, None, false, source.as_ref(), (ax + ex, ay + ey), &n.path);
                    }
                }
            }
        }
    }
    chain.pop();
}

/// The geometry of a scope: one item per node at the level it is shown, the wires between ports, and
/// the indices over them. `shift` moves a placed item by its path (a drag in progress); a zone's
/// children follow through the origin they are laid out from.
pub fn compute(name_w: &dyn Fn(&str) -> f64, shown: &dyn Fn(&Placed) -> Level, scope: &p::Scope, layout: &p::Layout, shift: &dyn Fn(&[Symbol]) -> (f64, f64)) -> Geo {
    let mut w = Walk { items: Vec::new(), slot: HashMap::new(), origins: vec![(scope.path.clone(), (0.0, 0.0))] };
    walk(name_w, shown, shift, &mut w, 0.0, 0.0, layout);
    // Later authored origins paint and hit-test before ancestors.
    let mut origins = w.origins;
    origins.reverse();
    let tiles = index_of(w.items.len(), &|i| {
        let (pl, ax, ay) = &w.items[i];
        (*ax, *ay, ax + pl.w, ay + pl.h)
    });
    let items = Items { items: w.items, slot: w.slot, tiles };
    let mut out = Out { wires: Vec::new(), bars: HashSet::new(), scratch: Scratch::default() };
    let mut chain: Vec<Table> = Vec::new();
    scope_wires(&items, &mut out, &mut chain, scope);
    if !scope.inputs.is_empty() {
        let chain = vec![declared(&items, scope)];
        let mut rpath = scope.path.clone();
        rpath.push("@return".into());
        if let Some((_, ax, ay)) = items.at(&rpath) {
            let source = match &scope.result {
                ScopeResult::Link(l) => resolve(&chain, l),
                ScopeResult::Node(_) => resolve(&chain, "@result"),
                ScopeResult::Literal(_) => None,
            };
            let target = (*ax, ay + p::BODY_TOP + WIRE_ROW_Y);
            add_wire(&items, &mut out, None, false, source.as_ref(), target, &rpath);
        }
    }
    let wires = out.wires;
    let mut inside: HashMap<Path, Vec<usize>> = HashMap::new();
    for (i, wi) in wires.iter().enumerate() {
        if let Some((path, _, _)) = &wi.target {
            if path.len() >= 3 {
                let zone = path[..path.len() - 1].to_vec();
                inside.entry(zone).or_default().insert(0, i);
            }
        }
    }
    let reach = index_of(wires.len(), &|i| {
        wires[i].pts.iter().fold((f64::INFINITY, f64::INFINITY, f64::NEG_INFINITY, f64::NEG_INFINITY), |(x0, y0, x1, y1), (x, y)| (x0.min(*x), y0.min(*y), x1.max(*x), y1.max(*y)))
    });
    let mut read: HashSet<(u64, u64)> = HashSet::new();
    for wi in &wires {
        read.insert(fkey(wi.a));
    }
    let Items { items: its, slot, tiles } = items;
    Geo { items: Rc::new(its), wires: Rc::new(wires), origins: Rc::new(origins), inside: Rc::new(inside), slot: Rc::new(slot), tiles: Rc::new(tiles), reach: Rc::new(reach), read: Rc::new(read) }
}
