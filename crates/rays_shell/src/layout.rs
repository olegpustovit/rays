//! the shell's panels as a tree and its geometry.
use editor_core::panels::{self, State};
pub use editor_core::panels::{Axis, Panel, Size, Panels};
use rays::frame;

pub type Path = Vec<usize>;
pub type Bounds = (i64, i64, i64, i64);

pub fn default() -> Panels {
    panels::default()
}

pub const SPLITTER_WIDTH: i64 = 1;
pub const COLLAPSED_WIDTH: i64 = 24;
/// One unit, 24 points: a panel header, the status strip, a docked timeline.
pub const HEADER_HEIGHT: i64 = 24;
pub const HEADER_MARGIN: i64 = 4;
/// the line-2 hairline and the 24-point bar
pub const STATUS_HEIGHT: i64 = 25;
pub const TIMELINE_HEIGHT: i64 = 24;
/// the strip under a tree without a timeline: hairline and bar
pub const TIMELINE_STRIP: i64 = 25;

/// a collapsed panel is its header: as tall in a stack, a 28-point column in a row
fn strip(axis: Axis) -> i64 {
    if axis == Axis::V { HEADER_HEIGHT } else { COLLAPSED_WIDTH }
}

/// One panel: its `frame`, the 24-point `header` row under a 4-point margin, and the body below
/// it. A docked timeline under 90 points has no header: it is its strip. `floating` covers both
/// authored `Float` trees and panels undocked through saved window state.
#[derive(Clone, Debug, PartialEq)]
pub struct Leaf {
    pub path: Path,
    pub panel: Panel,
    pub frame: Bounds,
    pub header: Bounds,
    pub body: Bounds,
    pub floating: bool,
}

/// A gutter. `node` is the split it resizes (none for a tile's fixed gutters) and `size` what
/// that split is sized by; `start` and `span` are the extent, along `axis`, of the columns that
/// split divides.
#[derive(Clone, Debug, PartialEq)]
pub struct Splitter {
    pub node: Option<Path>,
    pub axis: Axis,
    pub size: Size,
    pub bounds: Bounds,
    pub start: i64,
    pub span: i64,
}

/// The points its split gives each side now, along the axis.
pub fn sides(s: &Splitter) -> (i64, i64) {
    let (x, y, _, _) = s.bounds;
    let first = (if s.axis == Axis::H { x } else { y }) - s.start;
    (first, s.span - first - SPLITTER_WIDTH)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Way {
    Ratio,
    First,
    Second,
}

/// The split sized another way, keeping what it shows: by ratio, or with one side fixed.
pub fn resized(s: &Splitter, how: Way) -> Size {
    let (first, second) = sides(s);
    panels::clamp_size(match how {
        // the middle of the point the first side ends on, so the ratio places it there again
        Way::Ratio => Size::Ratio((first as f64 + 0.5) / ((s.span - SPLITTER_WIDTH).max(1) as f64)),
        Way::First => Size::First(first),
        Way::Second => Size::Second(second),
    })
}

/// the rows of a size menu: the three ways, the one in use greyed
pub fn size_rows(s: &Splitter) -> Vec<(String, bool)> {
    let (a, b) = if s.axis == Axis::H { ("left", "right") } else { ("top", "bottom") };
    let now = match s.size {
        Size::Ratio(_) => 0,
        Size::First(_) => 1,
        Size::Second(_) => 2,
    };
    ["By ratio".to_string(), format!("Fix {a} side"), format!("Fix {b} side")]
        .into_iter()
        .enumerate()
        .map(|(i, label)| (label, i != now))
        .collect()
}

pub const SIZE_WAYS: [Way; 3] = [Way::Ratio, Way::First, Way::Second];

/// Leaves in tree order, floats last (drawn over the rest).
#[derive(Clone, Debug, PartialEq)]
pub struct Geometry {
    pub leaves: Vec<Leaf>,
    pub splitters: Vec<Splitter>,
    pub status_at: Bounds,
    pub timeline_at: Bounds,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Panes {
    pub view: Bounds,
    pub graph: Bounds,
    pub inspector: Bounds,
    pub status: Bounds,
    pub timeline: Bounds,
}

pub fn expand(panel: &Panel, hidden: &[Panel]) -> Vec<Panel> {
    hidden.iter().filter(|p| *p != panel).cloned().collect()
}

/// A hidden panel vanishes; a hidden viewport keeps a strip with its expand button.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Presence {
    Live,
    Strip,
    Gone,
}

fn join(x: Presence, y: Presence) -> Presence {
    use Presence::*;
    match (x, y) {
        (Live, _) | (_, Live) => Live,
        (Strip, _) | (_, Strip) => Strip,
        (Gone, Gone) => Gone,
    }
}


fn child(path: &[usize], i: usize) -> Path {
    let mut p = path.to_vec();
    p.push(i);
    p
}

/// `f` with `path` extended by `more`; layout walks one path buffer.
fn at<R>(path: &mut Path, more: &[usize], f: impl FnOnce(&mut Path) -> R) -> R {
    let n = path.len();
    path.extend_from_slice(more);
    let r = f(path);
    path.truncate(n);
    r
}

fn presence(state: &dyn Fn(&[usize]) -> State, hidden: &[Panel], path: &mut Path, tree: &Panels) -> Presence {
    let under = |path: &mut Path, i: usize, sub: &Panels| at(path, &[i], |path| presence(state, hidden, path, sub));
    match tree {
        Panels::Leaf(_) if state(path).window.is_some() => Presence::Gone,
        // it has no header to keep
        Panels::Leaf(Panel::Timeline) if state(path).collapsed => Presence::Gone,
        Panels::Leaf(_) if state(path).collapsed => Presence::Strip,
        Panels::Leaf(p) => {
            if !hidden.contains(p) {
                Presence::Live
            } else if matches!(p, Panel::View(_)) {
                Presence::Strip
            } else {
                Presence::Gone
            }
        }
        Panels::Float(_) => Presence::Gone,
        Panels::Split { a, b, .. } => {
            let pa = under(path, 0, a);
            join(pa, under(path, 1, b))
        }
        Panels::Tile(cells) => {
            let mut acc = Presence::Gone;
            for (i, c) in cells.iter().enumerate() {
                acc = join(acc, under(path, i, c));
            }
            acc
        }
    }
}

fn is_float(tree: &Panels) -> bool {
    matches!(tree, Panels::Float(_))
}

/// A run of splits by ratio along one axis is one row of columns whose weights are the products
/// of the ratios, so `[0.45 | 0.55 * 0.6364 | ...]` divides the width once. A split with a fixed
/// side is one column of the run and is placed on its own.
fn columns<'a>(axis: Axis, weight: f64, path: Path, tree: &'a Panels, out: &mut Vec<(f64, Path, &'a Panels)>) {
    match tree {
        Panels::Split { axis: a, size: Size::Ratio(ratio), a: ta, b: tb } if *a == axis && !(is_float(ta) || is_float(tb)) => {
            columns(axis, weight * ratio, child(&path, 0), ta, out);
            columns(axis, weight * (1.0 - ratio), child(&path, 1), tb, out);
        }
        _ => out.push((weight, path, tree)),
    }
}

fn split_size(path: &[usize], tree: &Panels) -> Size {
    match (path, tree) {
        ([], Panels::Split { size, .. }) => *size,
        ([0, rest @ ..], Panels::Split { a, .. }) => split_size(rest, a),
        ([1, rest @ ..], Panels::Split { b, .. }) => split_size(rest, b),
        _ => Size::Ratio(0.5),
    }
}

fn minimum(axis: Axis, tree: &Panels) -> i64 {
    match tree {
        Panels::Leaf(p) if axis == Axis::H => match p {
            Panel::View(_) => 220,
            Panel::Graph | Panel::List | Panel::Lisp => 180,
            _ => 120,
        },
        Panels::Split { a, b, .. } => minimum(axis, a).max(minimum(axis, b)),
        Panels::Tile(cells) => cells.iter().fold(0, |m, c| m.max(minimum(axis, c))),
        Panels::Leaf(_) | Panels::Float(_) => 0,
    }
}

/// Sizes of the columns of one row. A column is (presence, weight, minimum, fixed): a fixed
/// extent (a strip, the timeline's own height) is kept, the rest share what is left by weight,
/// each at least its minimum when the minimums fit, the last weighted one taking the rounding
/// remainder.
fn distribute(total: i64, cols: &[(Presence, f64, i64, i64)]) -> Vec<i64> {
    let n = cols.len();
    let present = cols.iter().filter(|c| c.0 != Presence::Gone).count() as i64;
    let available = 3.max(total - ((present - 1).max(0) * SPLITTER_WIDTH));
    let mut sizes = vec![0i64; n];
    let mut fixed = 0i64;
    let mut weight = 0.0f64;
    let mut required = 0i64;
    let mut live: i64 = -1;
    let mut last: i64 = -1;
    let weighted = |c: &(Presence, f64, i64, i64)| c.0 != Presence::Gone && c.3 == 0;
    for (i, col) in cols.iter().enumerate() {
        if col.0 != Presence::Gone {
            last = i as i64;
        }
        if weighted(col) {
            weight += col.1;
            required += col.2;
            live = i as i64;
        } else if col.0 != Presence::Gone {
            sizes[i] = col.3;
            fixed += col.3;
        }
    }
    let flexible = 3.max(available - fixed);
    for (i, col) in cols.iter().enumerate() {
        if weighted(col) {
            sizes[i] = 1.max((flexible as f64 * col.1 / weight) as i64);
        }
    }
    let rest = if live >= 0 { live } else { last };
    if rest >= 0 {
        let used: i64 = sizes.iter().sum();
        let r = rest as usize;
        sizes[r] = 1.max(sizes[r] + available - used);
    }
    let mut short = false;
    for (i, col) in cols.iter().enumerate() {
        if weighted(col) && sizes[i] < col.2 {
            short = true;
        }
    }
    if live >= 0 && required <= flexible && short {
        let extra = flexible - required;
        let mut assigned = 0i64;
        for (i, col) in cols.iter().enumerate() {
            if weighted(col) {
                sizes[i] = col.2 + (extra as f64 * col.1 / weight) as i64;
                assigned += sizes[i];
            }
        }
        sizes[live as usize] += flexible - assigned;
    }
    sizes
}

fn common(a: &[usize], b: &[usize]) -> Path {
    let mut out = Vec::new();
    for (x, y) in a.iter().zip(b.iter()) {
        if x != y {
            break;
        }
        out.push(*x);
    }
    out
}

fn prefix(under: &[usize], path: &[usize]) -> bool {
    path.len() >= under.len() && path[..under.len()] == *under
}

struct Acc<'a> {
    leaves: Vec<Leaf>,
    splitters: Vec<Splitter>,
    floats: Vec<(Bounds, Path, &'a Panels)>,
}

fn leaf(acc: &mut Acc, state: &dyn Fn(&[usize]) -> State, floating: bool, path: &[usize], panel: &Panel, outer: Bounds) {
    let collapsed = state(path).collapsed;
    // a collapsed window is a short tab, its title and the expand button, wherever its place
    // came from: a saved window or the default one of a ui/floating
    let outer = match outer {
        (x, y, w, _) if floating && collapsed => (
            x,
            y,
            w.min(90.max(60 + 7 * panels::name(panel).len() as i64)),
            HEADER_MARGIN + HEADER_HEIGHT + 2,
        ),
        o => o,
    };
    let (fx, fy, fw, fh) = outer;
    // a window keeps its 1-point line-3 edge on all four sides: header and body lie inside it
    let (x, y, w, h) = if floating { (fx + 1, fy + 1, 1.max(fw - 2), 1.max(fh - 2)) } else { outer };
    // a docked timeline under 90 points is its strip alone; a taller one has a header too
    let is_strip = matches!(panel, Panel::Timeline) && !floating && h < 90;
    let margin = if (is_strip || collapsed) && !floating { 0 } else { HEADER_MARGIN.min(0.max(h - 2)) };
    let hh = if is_strip {
        0
    } else {
        HEADER_HEIGHT.min(0.max(if collapsed { h - margin } else { h - margin - 1 }))
    };
    let body = if collapsed { 0 } else { 1.max(h - margin - hh) };
    // a viewport window's picture stands 12 points in from the sheet, under a frame
    let inset = floating && matches!(panel, Panel::View(_)) && !collapsed && w > 40 && body > 40;
    acc.leaves.push(Leaf {
        path: path.to_vec(),
        panel: panel.clone(),
        floating,
        frame: outer,
        header: (x, y + margin, w, hh),
        body: if inset { (x + 12, y + margin + hh, w - 24, body - 12) } else { (x, y + margin + hh, w, body) },
    });
}

/// n cells and n-1 gutters over `total`
fn cut(total: i64, n: i64) -> Vec<i64> {
    let cell = 1.max((total - (n - 1) * SPLITTER_WIDTH) / n);
    (0..n).map(|i| if i == n - 1 { 1.max(total - i * (cell + SPLITTER_WIDTH)) } else { cell }).collect()
}

fn rect_at(horizontal: bool, x: i64, y: i64, w: i64, h: i64, from: i64, extent: i64) -> Bounds {
    if horizontal { (from, y, extent, h) } else { (x, from, w, extent) }
}

fn place<'a>(acc: &mut Acc<'a>, state: &dyn Fn(&[usize]) -> State, hidden: &[Panel], floating: bool, rect: Bounds, path: &mut Path, tree: &'a Panels) {
    let (x, y, w, h) = rect;
    match tree {
        Panels::Leaf(p) => {
            if presence(state, hidden, path, tree) != Presence::Gone {
                leaf(acc, state, floating, path, p, rect);
            }
        }
        Panels::Float(inner) => {
            acc.floats.push(((x + w / 8, y + h / 8, w - w / 4, h - h / 4), child(path, 0), &**inner));
        }
        Panels::Split { a, b, .. } if is_float(a) || is_float(b) => {
            at(path, &[0], |path| place(acc, state, hidden, floating, rect, path, a));
            at(path, &[1], |path| place(acc, state, hidden, floating, rect, path, b));
        }
        Panels::Split { axis, size: size @ (Size::First(_) | Size::Second(_)), a, b } => {
            let points = match size {
                Size::First(p) | Size::Second(p) => *p,
                Size::Ratio(_) => 0,
            };
            // one side keeps its points, the other takes the rest: the other keeps its minimum
            // first, then the fixed side shrinks, never below one point
            let pa = at(path, &[0], |path| presence(state, hidden, path, a));
            let pb = at(path, &[1], |path| presence(state, hidden, path, b));
            if pb == Presence::Gone {
                if pa != Presence::Gone {
                    at(path, &[0], |path| place(acc, state, hidden, floating, rect, path, a));
                }
            } else if pa == Presence::Gone {
                at(path, &[1], |path| place(acc, state, hidden, floating, rect, path, b));
            } else {
                let axis = *axis;
                let horizontal = axis == Axis::H;
                let total = if horizontal { w } else { h };
                let available = 2.max(total - SPLITTER_WIDTH);
                let first = matches!(size, Size::First(_));
                let (fixed_presence, other, other_presence) = if first { (pa, b, pb) } else { (pb, a, pa) };
                let fixed = if other_presence == Presence::Strip {
                    available - strip(axis).min(available - 1)
                } else {
                    let want = if fixed_presence == Presence::Strip { strip(axis) } else { points };
                    let room = available - (1.max(minimum(axis, other))).min(available - 1);
                    1.max(want.min(room))
                };
                let size_a = if first { fixed } else { available - fixed };
                let origin = if horizontal { x } else { y };
                at(path, &[0], |path| place(acc, state, hidden, floating, rect_at(horizontal, x, y, w, h, origin, size_a), path, a));
                acc.splitters.push(Splitter {
                    node: Some(path.to_vec()),
                    axis,
                    size: *size,
                    bounds: rect_at(horizontal, x, y, w, h, origin + size_a, SPLITTER_WIDTH),
                    start: origin,
                    span: total,
                });
                let rest = rect_at(horizontal, x, y, w, h, origin + size_a + SPLITTER_WIDTH, available - size_a);
                at(path, &[1], |path| place(acc, state, hidden, floating, rest, path, b));
            }
        }
        Panels::Split { axis, .. } => {
            let axis = *axis;
            let mut cols: Vec<(f64, Path, &Panels)> = Vec::new();
            columns(axis, 1.0, Vec::new(), tree, &mut cols);
            let info: Vec<(Presence, f64, i64, i64)> = cols
                .iter()
                .map(|(weight, col_path, sub)| {
                    let p = at(path, col_path, |path| presence(state, hidden, path, sub));
                    let fixed = match (p, sub) {
                        (Presence::Strip, _) => strip(axis),
                        (Presence::Live, Panels::Leaf(Panel::Timeline)) if axis == Axis::V => TIMELINE_HEIGHT,
                        _ => 0,
                    };
                    (p, *weight, minimum(axis, sub), fixed)
                })
                .collect();
            let horizontal = axis == Axis::H;
            let sizes = distribute(if horizontal { w } else { h }, &info);
            let mut cursor = if horizontal { x } else { y };
            let mut prior: Option<Path> = None;
            let mut placed: Vec<(Path, (i64, i64))> = Vec::new();
            let mut gutters: Vec<(Path, Bounds)> = Vec::new();
            for (i, (_, col_path, sub)) in cols.iter().enumerate() {
                if info[i].0 != Presence::Gone {
                    if let Some(before) = &prior {
                        gutters.push((
                            common(before, col_path),
                            if horizontal { (cursor, y, SPLITTER_WIDTH, h) } else { (x, cursor, w, SPLITTER_WIDTH) },
                        ));
                        cursor += SPLITTER_WIDTH;
                    }
                    let r = if horizontal { (cursor, y, sizes[i], h) } else { (x, cursor, w, sizes[i]) };
                    at(path, col_path, |path| place(acc, state, hidden, floating, r, path, sub));
                    placed.push((col_path.clone(), (cursor, sizes[i])));
                    cursor += sizes[i];
                    prior = Some(col_path.clone());
                }
            }
            let extent = |node: &[usize]| -> (i64, i64) {
                let under: Vec<&(Path, (i64, i64))> = placed.iter().filter(|(cp, _)| prefix(node, cp)).collect();
                if under.is_empty() {
                    (0, 1)
                } else {
                    let start = under[0].1.0;
                    let (s, len) = under[under.len() - 1].1;
                    (start, s + len - start)
                }
            };
            for (node, bounds) in gutters {
                let (start, span) = extent(&node);
                let mut full = path.to_vec();
                full.extend_from_slice(&node);
                acc.splitters.push(Splitter {
                    node: Some(full),
                    axis,
                    size: split_size(&node, tree),
                    bounds,
                    start,
                    span,
                });
            }
        }
        Panels::Tile(cells) => {
            let cells: Vec<(usize, &Panels)> = cells
                .iter()
                .enumerate()
                .filter(|(i, cell)| is_float(cell) || at(path, &[*i], |path| presence(state, hidden, path, cell)) != Presence::Gone)
                .collect();
            let n = cells.len() as i64;
            if n > 0 {
                let columns = (n as f64).sqrt().ceil() as i64;
                let rows = (n + columns - 1) / columns;
                let heights = cut(h, rows);
                let offset = |sizes: &[i64], k: i64| -> i64 {
                    let mut s = 0;
                    for j in 0..k {
                        s += sizes[j as usize] + SPLITTER_WIDTH;
                    }
                    s
                };
                // a short last row stretches its cells over the whole width
                let widths: Vec<Vec<i64>> = (0..rows).map(|r| cut(w, columns.min(n - r * columns))).collect();
                for (i, (original, cell)) in cells.iter().enumerate() {
                    let i = i as i64;
                    let c = i % columns;
                    let r = i / columns;
                    let cx = x + offset(&widths[r as usize], c);
                    let cy = y + offset(&heights, r);
                    if c > 0 {
                        acc.splitters.push(Splitter {
                            node: None,
                            axis: Axis::H,
                            size: Size::Ratio(0.5),
                            bounds: (cx - SPLITTER_WIDTH, cy, SPLITTER_WIDTH, heights[r as usize]),
                            start: 0,
                            span: 1,
                        });
                    }
                    if r > 0 && c == 0 {
                        acc.splitters.push(Splitter {
                            node: None,
                            axis: Axis::V,
                            size: Size::Ratio(0.5),
                            bounds: (x, cy - SPLITTER_WIDTH, w, SPLITTER_WIDTH),
                            start: 0,
                            span: 1,
                        });
                    }
                    let rect = (cx, cy, widths[r as usize][c as usize], heights[r as usize]);
                    at(path, &[*original], |path| place(acc, state, hidden, floating, rect, path, cell));
                }
            }
        }
    }
}

fn no_state(_: &[usize]) -> State {
    panels::default_state()
}

/// `hidden` (default the timeline) panels vanish; a hidden viewport and a collapsed panel keep a
/// strip with the expand button (the 24-point header in a stack, a 24-point column in a row). A
/// split with a fixed side gives that side its points and the other the rest; when both do not
/// fit, the other side keeps its minimum first and the fixed side shrinks, never below one point.
/// A `Timeline` leaf in a stack is 24 points; a tree without one has a 25-point strip under it. The
/// 25-point status strip spans the frame below both. Every point of the frame is covered exactly
/// once, floats aside.
pub fn geometry(state: Option<&dyn Fn(&[usize]) -> State>, hidden: Option<&[Panel]>, tree: &Panels, frame: &frame::Frame) -> Geometry {
    let state: &dyn Fn(&[usize]) -> State = state.unwrap_or(&no_state);
    let default_hidden = [Panel::Timeline];
    let hidden: &[Panel] = hidden.unwrap_or(&default_hidden);
    let (fw, fh) = frame.input.size;
    let header = HEADER_HEIGHT.min(0.max(fh - 1));
    let timeline = if hidden.contains(&Panel::Timeline) || panels::each_leaf(tree, &mut |_, p| *p == Panel::Timeline) {
        0
    } else {
        TIMELINE_STRIP.min(0.max(fh - header - 1))
    };
    // the status strip spans the window below the tree and the timeline strip
    let status = STATUS_HEIGHT.min(0.max(fh - header - timeline - 1));
    let bottom = fh - status - timeline;
    let mut acc = Acc { leaves: Vec::new(), splitters: Vec::new(), floats: Vec::new() };
    place(&mut acc, state, hidden, false, (0, 0, fw, 1.max(bottom)), &mut Vec::new(), tree);
    while !acc.floats.is_empty() {
        let queue = std::mem::take(&mut acc.floats);
        for (rect, mut path, sub) in queue {
            place(&mut acc, state, hidden, true, rect, &mut path, sub);
        }
    }
    panels::each_leaf(tree, &mut |path, panel| {
        if let Some((x, y, w, h)) = state(path).window {
            let w = fw.min(w);
            let h = fh.min(h);
            let x = 0.max(x.min(fw - w));
            let y = 0.max(y.min(fh - HEADER_HEIGHT));
            leaf(&mut acc, state, true, path, panel, (x, y, w, h.min(fh - y)));
        }
        false
    });
    Geometry {
        leaves: acc.leaves,
        splitters: acc.splitters,
        status_at: (0, fh - status, fw, status),
        timeline_at: (0, bottom, fw, timeline),
    }
}

pub fn find(geometry: &Geometry, panel: &Panel) -> Option<Leaf> {
    geometry.leaves.iter().find(|l| l.panel == *panel && l.body.3 > 0).cloned()
}

pub fn first_view(geometry: &Geometry) -> Option<Leaf> {
    geometry.leaves.iter().find(|l| l.body.3 > 0 && matches!(l.panel, Panel::View(_))).cloned()
}

const ZERO: Bounds = (0, 0, 0, 0);

/// The bodies of the first viewport, graph and inspector panels (all zero when absent).
pub fn panes(g: &Geometry) -> Panes {
    let body = |panel: Panel| find(g, &panel).map(|l| l.body).unwrap_or(ZERO);
    Panes {
        view: first_view(g).map(|l| l.body).unwrap_or(ZERO),
        graph: body(Panel::Graph),
        inspector: body(Panel::Inspector),
        status: g.status_at,
        timeline: g.timeline_at,
    }
}
