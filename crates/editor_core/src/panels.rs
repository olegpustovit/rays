//! Port of lib/editor_core/panels.ml: the shell as data, a tree of panels. Pure and
//! presentation-free.

use flow_syntax::packed_ops::{float_min, float_max};
#[derive(Clone, Debug, PartialEq)]
pub enum Panel {
    View(String),
    Canvas(String),
    Graph,
    List,
    Lisp,
    Inspector,
    Spreadsheet,
    Outline,
    Timeline,
}

/// `H puts a left of b, `V puts a above b.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Axis {
    H,
    V,
}

/// How a split divides its extent: a's share of it, or one side fixed in logical points.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Size {
    Ratio(f64),
    First(i64),
    Second(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side { Left, Right, Top, Bottom }

#[derive(Clone, Debug, PartialEq)]
pub enum Panels {
    Leaf(Panel),
    Split { axis: Axis, size: Size, a: Box<Panels>, b: Box<Panels> },
    /// A grid of equal cells.
    Tile(Vec<Panels>),
    /// An overlay on its parent's rectangle, never a window.
    Float(Box<Panels>),
}

/// Child indices from the root: a/b are 0/1, a tile's cells and a float's content are numbered
/// from 0.
pub type Path = Vec<usize>;

/// A panel's saved disclosure and optional floating window bounds in logical points.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct State {
    pub collapsed: bool,
    pub window: Option<(i64, i64, i64, i64)>,
}

pub fn default_state() -> State {
    State { collapsed: false, window: None }
}

/// `View "main"`, the viewport of the default layout.
pub fn main() -> Panel {
    Panel::View("main".into())
}

/// The three columns: view 45%, graph 35%, inspector 20%.
pub fn default() -> Panels {
    // graph and inspector widths of the default 45/35/20 columns
    Panels::Split {
        axis: Axis::H,
        size: Size::Ratio(0.45),
        a: Box::new(Panels::Leaf(main())),
        b: Box::new(Panels::Split {
            axis: Axis::H,
            size: Size::Ratio(0.35 / 0.55),
            a: Box::new(Panels::Leaf(Panel::Graph)),
            b: Box::new(Panels::Leaf(Panel::Inspector)),
        }),
    }
}

pub fn name(panel: &Panel) -> &'static str {
    match panel {
        Panel::Canvas(_) => "CANVAS",
        Panel::View(_) => "VIEW",
        Panel::Graph => "GRAPH",
        Panel::List => "LIST",
        Panel::Lisp => "LISP",
        Panel::Inspector => "INSPECTOR",
        Panel::Spreadsheet => "SPREADSHEET",
        Panel::Outline => "OUTLINE",
        Panel::Timeline => "TIMELINE",
    }
}

// Leaves outside floats in tree order (`docked`), else only the floats met outside other floats.
fn walk(path: &mut Path, tree: &Panels, docked: bool, f: &mut dyn FnMut(&[usize], &Panel) -> bool) -> bool {
    let mut under = |path: &mut Path, i: usize, sub: &Panels, f: &mut dyn FnMut(&[usize], &Panel) -> bool| {
        path.push(i);
        let stop = walk(path, sub, docked, f);
        path.pop();
        stop
    };
    match tree {
        Panels::Leaf(p) => docked && f(path, p),
        Panels::Split { a, b, .. } => under(path, 0, a, f) || under(path, 1, b, f),
        Panels::Tile(cells) => cells.iter().enumerate().any(|(i, c)| under(path, i, c, f)),
        Panels::Float(inner) => !docked && {
            path.push(0);
            let stop = each_leaf_at(path, inner, f);
            path.pop();
            stop
        },
    }
}

fn each_leaf_at(path: &mut Path, tree: &Panels, f: &mut dyn FnMut(&[usize], &Panel) -> bool) -> bool {
    walk(path, tree, true, f) || walk(path, tree, false, f)
}

/// Every leaf in [`leaves`] order, without allocating, until `f` returns true; whether it did.
pub fn each_leaf(tree: &Panels, f: &mut dyn FnMut(&[usize], &Panel) -> bool) -> bool {
    each_leaf_at(&mut Vec::new(), tree, f)
}

/// Every leaf in tree order, floats last: a float's own leaves before the floats inside it.
pub fn leaves(tree: &Panels) -> Vec<(Path, Panel)> {
    let mut out = Vec::new();
    each_leaf(tree, &mut |path, p| {
        out.push((path.to_vec(), p.clone()));
        false
    });
    out
}

/// The subtree at a path of [`leaves`]; `None` when the tree has no such place.
pub fn at_ref<'a>(path: &[usize], tree: &'a Panels) -> Option<&'a Panels> {
    match (path, tree) {
        ([], tree) => Some(tree),
        ([0, rest @ ..], Panels::Split { a, .. }) => at_ref(rest, a),
        ([1, rest @ ..], Panels::Split { b, .. }) => at_ref(rest, b),
        ([i, rest @ ..], Panels::Tile(cells)) => cells.get(*i).and_then(|cell| at_ref(rest, cell)),
        ([0, rest @ ..], Panels::Float(inner)) => at_ref(rest, inner),
        _ => None,
    }
}

/// Owned copy of the subtree at `path` (prefer [`at_ref`] on hot paths).
pub fn at(path: &[usize], tree: &Panels) -> Option<Panels> {
    at_ref(path, tree).cloned()
}

/// A ratio within 0.1-0.9, a fixed side of at least one point.
pub fn clamp_size(size: Size) -> Size {
    match size {
        Size::Ratio(r) => Size::Ratio(float_max(0.1, float_min(0.9, r))),
        Size::First(n) => Size::First(n.max(1)),
        Size::Second(n) => Size::Second(n.max(1)),
    }
}

/// The split at `path` gets this size ([`clamp_size`]); anything else is unchanged.
pub fn set_size(path: &[usize], size: Size, tree: Panels) -> Panels {
    match (path, tree) {
        ([], Panels::Split { axis, a, b, .. }) => Panels::Split { axis, size: clamp_size(size), a, b },
        ([0, rest @ ..], Panels::Split { axis, size: current, a, b }) => {
            Panels::Split { axis, size: current, a: Box::new(set_size(rest, size, *a)), b }
        }
        ([1, rest @ ..], Panels::Split { axis, size: current, a, b }) => {
            Panels::Split { axis, size: current, a, b: Box::new(set_size(rest, size, *b)) }
        }
        ([i, rest @ ..], Panels::Tile(cells)) => Panels::Tile(
            cells
                .into_iter()
                .enumerate()
                .map(|(j, cell)| if *i == j { set_size(rest, size, cell) } else { cell })
                .collect(),
        ),
        ([0, rest @ ..], Panels::Float(inner)) => Panels::Float(Box::new(set_size(rest, size, *inner))),
        (_, tree) => tree,
    }
}

/// Ratios in 0.1-0.9, fixed sides of one point or more, tiles of 1-16 cells, no empty tree.
pub fn valid(tree: &Panels) -> Result<(), String> {
    match tree {
        Panels::Leaf(_) => Ok(()),
        Panels::Split { size, a, b, .. } => match size {
            Size::Ratio(r) if *r < 0.1 || *r > 0.9 => Err("Split ratio is 0.1-0.9.".into()),
            Size::First(n) | Size::Second(n) if *n < 1 => {
                Err("A fixed split size is 1 point or more.".into())
            }
            _ => valid(a).and_then(|()| valid(b)),
        },
        Panels::Tile(cells) => {
            let n = cells.len();
            if !(1..=16).contains(&n) {
                Err("A tile holds 1-16 panels.".into())
            } else {
                cells.iter().try_for_each(valid)
            }
        }
        Panels::Float(inner) => valid(inner),
    }
}

/// A one-line shape such as `(h view (h graph inspector))`, for messages and tests.
pub fn to_string(tree: &Panels) -> String {
    match tree {
        Panels::Leaf(p) => name(p).to_ascii_lowercase(),
        Panels::Split { axis, a, b, .. } => format!(
            "({} {} {})",
            if *axis == Axis::H { "h" } else { "v" },
            to_string(a),
            to_string(b)
        ),
        Panels::Tile(cells) => {
            let inner: Vec<String> = cells.iter().map(to_string).collect();
            format!("(tile {})", inner.join(" "))
        }
        Panels::Float(inner) => format!("(float {})", to_string(inner)),
    }
}

// ---- layout names: a pure reading of the tree ----

fn word(panel: &Panel) -> &'static str {
    match panel {
        Panel::Canvas(_) => "Canvas",
        Panel::View(_) => "View",
        Panel::Graph => "Graph",
        Panel::List => "List",
        Panel::Lisp => "Lisp",
        Panel::Inspector => "Inspector",
        Panel::Spreadsheet => "Spreadsheet",
        Panel::Outline => "Outline",
        Panel::Timeline => "Timeline",
    }
}

/// The tree without its floats; `None` when nothing is docked.
fn docked(tree: &Panels) -> Option<Panels> {
    match tree {
        Panels::Leaf(_) => Some(tree.clone()),
        Panels::Float(_) => None,
        Panels::Split { axis, size, a, b } => match (docked(a), docked(b)) {
            (Some(a), Some(b)) => Some(Panels::Split { axis: *axis, size: *size, a: Box::new(a), b: Box::new(b) }),
            (Some(x), None) | (None, Some(x)) => Some(x),
            (None, None) => None,
        },
        Panels::Tile(cells) => {
            let cs: Vec<Panels> = cells.iter().filter_map(docked).collect();
            if cs.is_empty() { None } else { Some(Panels::Tile(cs)) }
        }
    }
}

fn floats(tree: &Panels) -> Vec<Panels> {
    match tree {
        Panels::Leaf(_) => Vec::new(),
        Panels::Float(inner) => vec![(**inner).clone()],
        Panels::Split { a, b, .. } => {
            let mut all = floats(a);
            all.extend(floats(b));
            all
        }
        Panels::Tile(cells) => cells.iter().flat_map(floats).collect(),
    }
}

/// Consecutive equal names with their counts.
fn runs(names: Vec<String>) -> Vec<(String, i64)> {
    let mut out: Vec<(String, i64)> = Vec::new();
    for name in names {
        match out.last_mut() {
            Some((last, count)) if *last == name => *count += 1,
            _ => out.push((name, 1)),
        }
    }
    out
}

fn with_count(name: &str, count: i64) -> String {
    if count > 1 { format!("{} \u{d7}{}", name, count) } else { name.to_string() }
}

/// The panels of a stack or tile in order; a tile reads as a row (H).
fn parts(axis: Axis, tree: &Panels) -> Vec<&Panels> {
    match tree {
        Panels::Split { axis: own, a, b, .. } if *own == axis => {
            let mut all = parts(axis, a);
            all.extend(parts(axis, b));
            all
        }
        Panels::Tile(cells) if axis == Axis::H => cells.iter().flat_map(|c| parts(axis, c)).collect(),
        _ => vec![tree],
    }
}

// a tile reads as a row; neighbours of one name collapse to [name ×n]; a row inside a stack is
// bracketed
fn shape(parent: Option<Axis>, tree: &Panels) -> String {
    match tree {
        Panels::Leaf(p) => word(p).to_string(),
        Panels::Float(inner) => shape(parent, inner),
        _ => {
            let axis = match tree {
                Panels::Split { axis, .. } => *axis,
                _ => Axis::H,
            };
            let names: Vec<String> = parts(axis, tree).into_iter().map(|p| shape(Some(axis), p)).collect();
            let grouped = runs(names);
            let separator = if axis == Axis::H { " | " } else { " / " };
            let text = grouped
                .iter()
                .map(|(n, c)| with_count(n, *c))
                .collect::<Vec<String>>()
                .join(separator);
            if axis == Axis::H && parent == Some(Axis::V) && grouped.len() > 1 {
                format!("({})", text)
            } else {
                text
            }
        }
    }
}

/// A layout's name read from its tree: side by side is `a | b`, stacked `a / b`, floating
/// windows come last after `+`.
pub fn label(tree: &Panels) -> String {
    let mut all: Vec<String> = match docked(tree) {
        Some(d) => vec![shape(None, &d)],
        None => Vec::new(),
    };
    all.extend(floats(tree).iter().map(|f| shape(None, f)));
    all.join(" + ")
}

/// The first panel of a subtree, in tree order.
fn first(tree: &Panels) -> Option<Panel> {
    match tree {
        Panels::Leaf(p) => Some(p.clone()),
        Panels::Float(_) => None,
        Panels::Split { a, b, .. } => first(a).or_else(|| first(b)),
        Panels::Tile(cells) => cells.iter().find_map(first),
    }
}

/// The short reading, one name per column group: the docked panels side by side, each stack or
/// tile as its first panel, neighbours of one name collapsed; floating windows count apart.
pub fn summary(tree: &Panels) -> String {
    fn columns(tree: &Panels) -> Vec<String> {
        match tree {
            Panels::Split { axis: Axis::H, a, b, .. } => {
                let mut all = columns(a);
                all.extend(columns(b));
                all
            }
            Panels::Tile(cells) => cells.iter().flat_map(columns).collect(),
            Panels::Float(_) => Vec::new(),
            other => first(other).iter().map(|p| word(p).to_string()).collect(),
        }
    }
    runs(columns(tree))
        .iter()
        .map(|(n, c)| with_count(n, *c))
        .collect::<Vec<String>>()
        .join(" | ")
}

/// The largest docked panel and its share, in percent.
fn largest(tree: &Panels) -> Option<(Panel, i64)> {
    fn areas(share: f64, tree: &Panels) -> Vec<(Panel, f64)> {
        match tree {
            Panels::Leaf(p) => vec![(p.clone(), share)],
            Panels::Split { size, a, b, .. } => {
                // ponytail: a fixed side has no share without a window size; it reads as half
                let r = match size {
                    Size::Ratio(r) => *r,
                    Size::First(_) | Size::Second(_) => 0.5,
                };
                let mut all = areas(share * r, a);
                all.extend(areas(share * (1. - r), b));
                all
            }
            Panels::Tile(cells) => {
                let each = share / cells.len() as f64;
                cells.iter().flat_map(|c| areas(each, c)).collect()
            }
            Panels::Float(_) => Vec::new(),
        }
    }
    let docked_tree = docked(tree)?;
    let mut list = areas(1., &docked_tree).into_iter();
    let (mut best, mut best_share) = list.next()?;
    for (p, share) in list {
        if share > best_share {
            best = p;
            best_share = share;
        }
    }
    Some((best, (best_share * 100.).round() as i64))
}

/// [`label`] of each layout; layouts that read the same add their largest panel and its share,
/// and any that still match get a number.
pub fn labels(trees: &[Panels]) -> Vec<String> {
    let base: Vec<String> = trees.iter().map(label).collect();
    let step: Vec<String> = trees
        .iter()
        .zip(base.iter())
        .map(|(tree, n)| {
            if base.iter().filter(|other| *other == n).count() < 2 {
                return n.clone();
            }
            match largest(tree) {
                Some((p, share)) => format!("{} \u{b7} {} {}%", n, word(&p), share),
                None => n.clone(),
            }
        })
        .collect();
    let mut seen: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
    step.into_iter()
        .map(|n| {
            let k = 1 + seen.get(&n).copied().unwrap_or(0);
            seen.insert(n.clone(), k);
            if k > 1 { format!("{} ({})", n, k) } else { n }
        })
        .collect()
}
