//! Port of lib/editor_core/panels.ml: the shell as data, a tree of panels. Pure and
//! presentation-free.

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

#[derive(Clone, Debug, PartialEq)]
pub enum t {
    Leaf(Panel),
    Split { axis: Axis, size: Size, a: Box<t>, b: Box<t> },
    /// A grid of equal cells.
    Tile(Vec<t>),
    /// An overlay on its parent's rectangle, never a window.
    Float(Box<t>),
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
pub fn default() -> t {
    // graph and inspector widths of the default 45/35/20 columns
    t::Split {
        axis: Axis::H,
        size: Size::Ratio(0.45),
        a: Box::new(t::Leaf(main())),
        b: Box::new(t::Split {
            axis: Axis::H,
            size: Size::Ratio(0.35 / 0.55),
            a: Box::new(t::Leaf(Panel::Graph)),
            b: Box::new(t::Leaf(Panel::Inspector)),
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

fn concat_pairs(
    (a, af): (Vec<(Path, Panel)>, Vec<(Path, Panel)>),
    (b, bf): (Vec<(Path, Panel)>, Vec<(Path, Panel)>),
) -> (Vec<(Path, Panel)>, Vec<(Path, Panel)>) {
    let mut docked = a;
    docked.extend(b);
    let mut floats = af;
    floats.extend(bf);
    (docked, floats)
}

type Walked = (Vec<(Path, Panel)>, Vec<(Path, Panel)>);

fn walk(path: &[usize], tree: &t) -> Walked {
    // ponytail: a plain recursive walk; floats are collected after the rest
    match tree {
        t::Leaf(p) => (vec![(path.to_vec(), p.clone())], Vec::new()),
        t::Split { a, b, .. } => both(path, &[a.as_ref(), b.as_ref()]),
        t::Tile(cells) => {
            let refs: Vec<&t> = cells.iter().collect();
            both(path, &refs)
        }
        t::Float(inner) => {
            let mut child = path.to_vec();
            child.push(0);
            let (r, f) = walk(&child, inner);
            let mut floats = r;
            floats.extend(f);
            (Vec::new(), floats)
        }
    }
}

fn both(path: &[usize], children: &[&t]) -> Walked {
    let mut acc: Walked = (Vec::new(), Vec::new());
    for (i, child) in children.iter().enumerate() {
        let mut child_path = path.to_vec();
        child_path.push(i);
        acc = concat_pairs(acc, walk(&child_path, child));
    }
    acc
}

/// Every leaf in tree order, floats last.
pub fn leaves(tree: &t) -> Vec<(Path, Panel)> {
    let (mut docked, floats) = walk(&[], tree);
    docked.extend(floats);
    docked
}

/// The subtree at a path of [`leaves`]; `None` when the tree has no such place.
pub fn at(path: &[usize], tree: &t) -> Option<t> {
    match (path, tree) {
        ([], tree) => Some(tree.clone()),
        ([0, rest @ ..], t::Split { a, .. }) => at(rest, a),
        ([1, rest @ ..], t::Split { b, .. }) => at(rest, b),
        ([i, rest @ ..], t::Tile(cells)) => cells.get(*i).and_then(|cell| at(rest, cell)),
        ([0, rest @ ..], t::Float(inner)) => at(rest, inner),
        _ => None,
    }
}

// OCaml's Float.min and Float.max propagate NaN; f64::min and f64::max do not.
fn float_min(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else if a < b {
        a
    } else {
        b
    }
}

fn float_max(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else if a > b {
        a
    } else {
        b
    }
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
pub fn set_size(path: &[usize], size: Size, tree: t) -> t {
    match (path, tree) {
        ([], t::Split { axis, a, b, .. }) => t::Split { axis, size: clamp_size(size), a, b },
        ([0, rest @ ..], t::Split { axis, size: current, a, b }) => {
            t::Split { axis, size: current, a: Box::new(set_size(rest, size, *a)), b }
        }
        ([1, rest @ ..], t::Split { axis, size: current, a, b }) => {
            t::Split { axis, size: current, a, b: Box::new(set_size(rest, size, *b)) }
        }
        ([i, rest @ ..], t::Tile(cells)) => t::Tile(
            cells
                .into_iter()
                .enumerate()
                .map(|(j, cell)| if *i == j { set_size(rest, size, cell) } else { cell })
                .collect(),
        ),
        ([0, rest @ ..], t::Float(inner)) => t::Float(Box::new(set_size(rest, size, *inner))),
        (_, tree) => tree,
    }
}

/// Ratios in 0.1-0.9, fixed sides of one point or more, tiles of 1-16 cells, no empty tree.
pub fn valid(tree: &t) -> Result<(), String> {
    match tree {
        t::Leaf(_) => Ok(()),
        t::Split { size, a, b, .. } => match size {
            Size::Ratio(r) if *r < 0.1 || *r > 0.9 => Err("Split ratio is 0.1-0.9.".into()),
            Size::First(n) | Size::Second(n) if *n < 1 => {
                Err("A fixed split size is 1 point or more.".into())
            }
            _ => valid(a).and_then(|()| valid(b)),
        },
        t::Tile(cells) => {
            let n = cells.len();
            if !(1..=16).contains(&n) {
                Err("A tile holds 1-16 panels.".into())
            } else {
                cells.iter().try_for_each(valid)
            }
        }
        t::Float(inner) => valid(inner),
    }
}

/// A one-line shape such as `(h view (h graph inspector))`, for messages and tests.
pub fn to_string(tree: &t) -> String {
    match tree {
        t::Leaf(p) => name(p).to_ascii_lowercase(),
        t::Split { axis, a, b, .. } => format!(
            "({} {} {})",
            if *axis == Axis::H { "h" } else { "v" },
            to_string(a),
            to_string(b)
        ),
        t::Tile(cells) => {
            let inner: Vec<String> = cells.iter().map(to_string).collect();
            format!("(tile {})", inner.join(" "))
        }
        t::Float(inner) => format!("(float {})", to_string(inner)),
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
fn docked(tree: &t) -> Option<t> {
    match tree {
        t::Leaf(_) => Some(tree.clone()),
        t::Float(_) => None,
        t::Split { axis, size, a, b } => match (docked(a), docked(b)) {
            (Some(a), Some(b)) => Some(t::Split { axis: *axis, size: *size, a: Box::new(a), b: Box::new(b) }),
            (Some(x), None) | (None, Some(x)) => Some(x),
            (None, None) => None,
        },
        t::Tile(cells) => {
            let cs: Vec<t> = cells.iter().filter_map(docked).collect();
            if cs.is_empty() { None } else { Some(t::Tile(cs)) }
        }
    }
}

fn floats(tree: &t) -> Vec<t> {
    match tree {
        t::Leaf(_) => Vec::new(),
        t::Float(inner) => vec![(**inner).clone()],
        t::Split { a, b, .. } => {
            let mut all = floats(a);
            all.extend(floats(b));
            all
        }
        t::Tile(cells) => cells.iter().flat_map(floats).collect(),
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
fn parts(axis: Axis, tree: &t) -> Vec<&t> {
    match tree {
        t::Split { axis: own, a, b, .. } if *own == axis => {
            let mut all = parts(axis, a);
            all.extend(parts(axis, b));
            all
        }
        t::Tile(cells) if axis == Axis::H => cells.iter().flat_map(|c| parts(axis, c)).collect(),
        _ => vec![tree],
    }
}

// a tile reads as a row; neighbours of one name collapse to [name ×n]; a row inside a stack is
// bracketed
fn shape(parent: Option<Axis>, tree: &t) -> String {
    match tree {
        t::Leaf(p) => word(p).to_string(),
        t::Float(inner) => shape(parent, inner),
        _ => {
            let axis = match tree {
                t::Split { axis, .. } => *axis,
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
pub fn label(tree: &t) -> String {
    let mut all: Vec<String> = match docked(tree) {
        Some(d) => vec![shape(None, &d)],
        None => Vec::new(),
    };
    all.extend(floats(tree).iter().map(|f| shape(None, f)));
    all.join(" + ")
}

/// The first panel of a subtree, in tree order.
fn first(tree: &t) -> Option<Panel> {
    match tree {
        t::Leaf(p) => Some(p.clone()),
        t::Float(_) => None,
        t::Split { a, b, .. } => first(a).or_else(|| first(b)),
        t::Tile(cells) => cells.iter().find_map(first),
    }
}

/// The short reading, one name per column group: the docked panels side by side, each stack or
/// tile as its first panel, neighbours of one name collapsed; floating windows count apart.
pub fn summary(tree: &t) -> String {
    fn columns(tree: &t) -> Vec<String> {
        match tree {
            t::Split { axis: Axis::H, a, b, .. } => {
                let mut all = columns(a);
                all.extend(columns(b));
                all
            }
            t::Tile(cells) => cells.iter().flat_map(columns).collect(),
            t::Float(_) => Vec::new(),
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
fn largest(tree: &t) -> Option<(Panel, i64)> {
    fn areas(share: f64, tree: &t) -> Vec<(Panel, f64)> {
        match tree {
            t::Leaf(p) => vec![(p.clone(), share)],
            t::Split { size, a, b, .. } => {
                // ponytail: a fixed side has no share without a window size; it reads as half
                let r = match size {
                    Size::Ratio(r) => *r,
                    Size::First(_) | Size::Second(_) => 0.5,
                };
                let mut all = areas(share * r, a);
                all.extend(areas(share * (1. - r), b));
                all
            }
            t::Tile(cells) => {
                let each = share / cells.len() as f64;
                cells.iter().flat_map(|c| areas(each, c)).collect()
            }
            t::Float(_) => Vec::new(),
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
pub fn labels(trees: &[t]) -> Vec<String> {
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
