// the graph pane's immutable view state, layout and culling
// queries. The pane draws one `Projection::Scope` with its zones, rails, iteration selectors, chips and
// output rows on the shared PXUI handle and returns typed requests; it never edits.
use flow_syntax::symbol::Symbol;
use crate::scope_wires::{self as w, Geo, Wire};
use flow_check::workspace::Path;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit as e;
use flow_graph::probe::{self, Probe};
use flow_graph::projection as p;
use flow_graph::projection::{Item, Level, Placed};
use flow_syntax::syntax::{Node as SNode, Syntax};
use flow_syntax::ty::Ty;
use pxui::theme::{self, Tokens as Theme};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

pub type S = Syntax;
pub type PathSet = BTreeSet<Path>;
pub type Frame3 = (String, (f64, f64), (f64, f64));

#[derive(Clone, Debug, PartialEq)]
pub enum Change {
    SyntaxEdit(e::Op),
    /// a click on a layout of an editor graph's switch: the host makes it the active one
    LayoutActive { graph: String, index: i64 },
    ProbeSet { zone: Path, index: i64 },
    ZoneCollapsed { zone: Path, collapsed: bool },
    Selected(Vec<Path>),
    Moved(Vec<(Path, f64, f64)>),
    /// nodes opened or pointed: their new level (`None`: back to the default, `Card`) and whether it is pinned
    LevelSet(Vec<(Path, Option<Level>, bool)>),
    /// the host puts these bindings' text on the clipboard
    CopyRequested(Vec<Path>),
    /// the host adds the clipboard's bindings here
    PasteRequested,
    /// a right-click on empty canvas: the host opens its add menu there
    MenuRequested(f64, f64),
    OpenImport(String),
    /// the host opens the make-macro dialog over these nodes
    MacroRequested(Vec<Path>),
    /// the host types the outside names and writes the `defn`
    DefnRequested(Vec<Path>),
    /// the frames of one scope after a gesture (create, resize, retitle, delete)
    FramesSet { scope: Path, frames: Vec<Frame3> },
    DisplaySet(Path),
    Activated(Path),
    DropOver { path: Path, kind: String, value: String },
    Dropped { path: Path, kind: String, value: String },
    Notice(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Left,
    Down,
    Up,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    Delete,
    FoldInto,
    Unfold,
    /// every call written in a row of the selected card becomes its own card
    UnfoldAll,
    Hoist,
    Bypass,
    WrapRepeat,
    WrapIterate,
    WrapIf,
    WrapMap,
    WrapFilter,
    InlineFn,
    AddArm,
    DeleteArm,
    MakeFn,
    MakeMacro,
    MakeDefn,
    Collapse,
    ProbeStep(i64),
    FrameAll,
    Tidy,
    ToggleTags,
    Walk(Direction),
    /// rename the selected node, or edit the default of a selected graph input
    EditName,
    ItemUp,
    /// move the hovered list item
    ItemDown,
    /// a titled frame around the selected nodes
    MakeFrame,
    Duplicate,
    Display,
    Copy,
    Cut,
    Paste,
    /// `f`: pan and zoom to the selected nodes (all, with none selected)
    FrameSelection,
    /// `o`: the selection one level more detailed, pinned
    OpenLevel,
    /// `p`: the selection to points, or back to its previous level
    PointLevel,
    /// every node to card
    OpenAll,
    /// every node to a point, or every node back
    PointAll,
    /// letter hints; a letter connects the selected node's output
    ShowHints,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Stats {
    pub nodes: i64,
    pub zones: i64,
    pub rows: i64,
    pub drawn_items: i64,
    pub drawn_zones: i64,
}

/// `lift`: the inputs an Alt-drag picked the wire up from; letting go takes it off them.
#[derive(Clone, Debug, PartialEq)]
pub struct Wiring {
    pub src: String,
    pub ty: Option<Ty>,
    pub iter: bool,
    pub from: (f64, f64),
    pub lift: Vec<(Path, ArgKey)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Drag {
    Moving { paths: Vec<Path>, dx: f64, dy: f64, moved: bool },
    Wiring(Wiring),
    /// from the canvas press point to the pointer, screen space
    Marquee { base: PathSet },
    /// a frame's corner
    Sizing { scope: Path, index: i64, dw: f64, dh: f64 },
    /// a frame by its title, with the nodes inside it
    Carrying { scope: Path, index: i64, paths: Vec<Path>, dx: f64, dy: f64 },
}

/// The text field open over the pane: a node's name, a graph input's default, a frame's title.
#[derive(Clone, Debug, PartialEq)]
pub enum Editing {
    Name(Path),
    Default(Path),
    Title(Path, i64),
    /// the name of a new output, over the `+ output` row of a `values`
    Output(Path),
}

pub type Fr = Frame3;

/// Letter hints (flow.md 7.5).
#[derive(Clone, Debug, PartialEq)]
pub struct HintTarget {
    pub label: String,
    pub node: Path,
    pub key: Option<ArgKey>,
    pub pos: (f64, f64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hinting {
    pub src: String,
    pub src_ty: Ty,
    pub typed: String,
    pub targets: Vec<HintTarget>,
    pub stage: Option<Path>,
    pub armed: bool,
}

/// What the host keeps of each node by path: its saved layout (position, level, pinned rows, folds,
/// frames), the iteration each zone shows, and the imported files that define graphs. The pane only
/// asks; a host that keeps none of it uses `Nothing`.
pub trait Saved {
    fn at(&self, _path: &[Symbol]) -> Option<(f64, f64)> {
        None
    }
    /// a node's saved level and whether it is pinned
    fn level(&self, _path: &[Symbol]) -> Option<(Level, bool)> {
        None
    }
    /// a node's row pinned onto (or off) its card, by label
    fn pin(&self, _path: &[Symbol], _label: &str) -> Option<bool> {
        None
    }
    fn collapsed(&self, _path: &[Symbol]) -> bool {
        false
    }
    fn probe(&self, _path: &[Symbol]) -> i64 {
        0
    }
    fn frames(&self, _path: &[Symbol]) -> Vec<Frame3> {
        Vec::new()
    }
    fn imported(&self, _path: &[Symbol]) -> Option<String> {
        None
    }
}

pub struct Nothing;
impl Saved for Nothing {}

pub type Chains = (PathSet, PathSet);
pub type ChainCache = Option<(PathSet, Rc<Vec<Wire>>, Chains)>;

#[derive(Clone)]
pub struct Pane {
    pub x: i64,
    pub y: i64,
    pub width: i64,
    pub height: i64,
    pub theme: Theme,
    pub guide: bool,
    pub key: String,
    pub scope: Option<Rc<p::Scope>>,
    /// what the host keeps per node: saved layout, probes, imported files
    pub saved: Rc<dyn Saved>,
    /// macro calls whose expansion panel is open, and its step
    pub lens: Vec<(Path, i64)>,
    pub records: Option<Rc<RefCell<Probe>>>,
    /// the iterating zones around each node
    pub chains: Rc<HashMap<Path, Vec<Path>>>,
    /// a wired row (node, row index) whose one source can fold into it: that node
    pub folds: Rc<HashMap<(Path, usize), Path>>,
    /// iterations each zone ran, under the probes
    pub counts: Rc<HashMap<Path, i64>>,
    /// the node the viewport shows instead of the graph's result
    pub display: Option<Path>,
    pub framed: bool,
    pub layout: Rc<p::Layout>,
    pub geo: Geo,
    /// the font size point names were measured at
    pub geo_fs: i64,
    /// a point name was laid out with a guessed width (no frame had measured it yet)
    pub geo_cold: bool,
    pub pan_x: f64,
    pub pan_y: f64,
    pub zoom: f64,
    pub selected: PathSet,
    pub selected_wire: Option<(Path, ArgKey, Option<S>)>,
    pub panning_grab: bool,
    pub hovered_row: Option<(Path, ArgKey)>,
    pub highlighted: Vec<Wire>,
    pub drag: Option<Drag>,
    pub editing: Option<Editing>,
    pub context: Option<((f64, f64), Path)>,
    pub stats: Stats,
    /// the `ui/switch` nodes and their active layout
    pub switches: Vec<(Path, i64)>,
    /// the places a carried payload can be put, with their letters
    pub carry_lit: Vec<(Path, String)>,
    /// the place under the pointer, and whether it takes the payload
    pub carry_hot: Option<(Path, bool)>,
    /// nodes a cook or the checker refused, with the diagnostic's code
    pub failed: Vec<(Path, String)>,
    pub hinting: Option<Hinting>,
    /// the level a node had before `p` pointed it
    pub back: Vec<(Path, Level)>,
    /// text width in points, from the last built frame
    pub measure: Rc<dyn Fn(i64, &str) -> f64>,
    /// widths measured by the last built frames (a pane outside a frame has no `Ui` to ask)
    pub measured: Rc<RefCell<HashMap<(i64, String), f64>>>,
    /// a long wire is a named tag at its use site, unless its chain is lit (flow.md 6.4)
    pub tags: bool,
    /// one slot: the chains of the last selection over the last wires
    pub chain: Rc<RefCell<ChainCache>>,
    /// the pane as its last build left it, and that build's revision: `revision` bumps it when the
    /// pane differs from it in anything a build reads (shared by clones of the pane)
    pub built: Rc<RefCell<Option<(Box<Pane>, u64, pxui::ui::UiBox)>>>,
}

/// Whether two panes build the same frame: the same graph, layout, geometry, records and settings
/// (shared parts by identity; the last build's copy keeps them alive, so an address is never reused
/// under it), and the same view, selection, hover, drag, edit and marks. Text measurements, the
/// chain memo and the stats are caches or outputs, not inputs.
pub fn builds_same(a: &Pane, b: &Pane) -> bool {
    let (g, h) = (&a.geo, &b.geo);
    let geo = Rc::ptr_eq(&g.items, &h.items) && Rc::ptr_eq(&g.wires, &h.wires) && Rc::ptr_eq(&g.origins, &h.origins) && Rc::ptr_eq(&g.inside, &h.inside)
        && Rc::ptr_eq(&g.slot, &h.slot) && Rc::ptr_eq(&g.tiles, &h.tiles) && Rc::ptr_eq(&g.reach, &h.reach) && Rc::ptr_eq(&g.read, &h.read);
    geo && (a.x, a.y, a.width, a.height, a.guide, a.framed, a.geo_fs, a.geo_cold, a.tags) == (b.x, b.y, b.width, b.height, b.guide, b.framed, b.geo_fs, b.geo_cold, b.tags)
        && (a.pan_x.to_bits(), a.pan_y.to_bits(), a.zoom.to_bits(), a.panning_grab) == (b.pan_x.to_bits(), b.pan_y.to_bits(), b.zoom.to_bits(), b.panning_grab)
        && a.theme == b.theme && a.key == b.key && a.scope.as_ref().map(Rc::as_ptr) == b.scope.as_ref().map(Rc::as_ptr) && Rc::ptr_eq(&a.saved, &b.saved) && a.records.as_ref().map(Rc::as_ptr) == b.records.as_ref().map(Rc::as_ptr)
        && Rc::ptr_eq(&a.chains, &b.chains) && Rc::ptr_eq(&a.folds, &b.folds) && Rc::ptr_eq(&a.counts, &b.counts) && Rc::ptr_eq(&a.layout, &b.layout)
        && a.lens == b.lens && a.display == b.display && a.selected == b.selected && a.selected_wire == b.selected_wire && a.hovered_row == b.hovered_row
        && a.highlighted == b.highlighted && a.drag == b.drag && a.editing == b.editing && a.context == b.context && a.switches == b.switches
        && a.carry_lit == b.carry_lit && a.carry_hot == b.carry_hot && a.failed == b.failed && a.hinting == b.hinting && a.back == b.back
}

/// The pane's revision for a cached view of it: the same number while it builds the same frame as
/// the last build (compared in place, copied only after a build, `keep_built`).
pub fn revision(t: &Pane) -> u64 {
    match &*t.built.borrow() {
        Some((last, rev, _)) if builds_same(last, t) => *rev,
        Some((_, rev, _)) => rev + 1,
        None => 0,
    }
}

/// After a build at `rev`: the pane as it left it, and its canvas box.
pub fn keep_built(t: &Pane, rev: u64, canvas: pxui::ui::UiBox) {
    let kept = Pane { built: Rc::new(RefCell::new(None)), ..t.clone() };
    *t.built.borrow_mut() = Some((Box::new(kept), rev, canvas));
}

/// The canvas box of the pane's last build.
pub fn built_canvas(t: &Pane) -> Option<pxui::ui::UiBox> {
    t.built.borrow().as_ref().map(|(_, _, canvas)| canvas.clone())
}


/// The font size a zoom draws text in.
pub fn font_of(zoom: f64) -> i64 {
    5.max((13.0 * zoom).round() as i64)
}

pub fn regeo(t: &mut Pane, scope: &p::Scope, layout: &p::Layout, shift: &dyn Fn(&[Symbol]) -> (f64, f64)) {
    let fs = font_of(t.zoom);
    let measured = t.measured.clone();
    let zoom = t.zoom;
    let cold = std::cell::Cell::new(false);
    let geo = w::compute(
        &|s: &str| match measured.borrow().get(&(fs, s.to_owned())) {
            Some(v) => *v / zoom,
            None => {
                cold.set(true);
                0.6 * (fs * s.len() as i64) as f64 / zoom
            }
        },
        &w::shown_of,
        scope,
        layout,
        shift,
    );
    (t.geo_fs, t.geo, t.geo_cold) = (fs, geo, cold.get());
}

pub fn create(width: Option<i64>, height: Option<i64>) -> Pane {
    let measured: Rc<RefCell<HashMap<(i64, String), f64>>> = Rc::new(RefCell::new(HashMap::new()));
    let shared = measured.clone();
    Pane {
        x: 0,
        y: 0,
        width: width.unwrap_or(640),
        height: height.unwrap_or(360),
        theme: theme::default(),
        guide: false,
        key: String::new(),
        scope: None,
        saved: Rc::new(Nothing),
        lens: Vec::new(),
        records: None,
        chains: Rc::new(HashMap::new()),
        folds: Rc::new(HashMap::new()),
        counts: Rc::new(HashMap::new()),
        display: None,
        framed: true,
        layout: Rc::new(p::Layout { placed: Vec::new(), w: 0.0, h: 0.0 }),
        geo: w::empty_geo(),
        geo_fs: 13,
        geo_cold: false,
        pan_x: 12.0,
        pan_y: 12.0,
        zoom: 1.0,
        selected: PathSet::new(),
        selected_wire: None,
        panning_grab: false,
        hovered_row: None,
        highlighted: Vec::new(),
        drag: None,
        editing: None,
        context: None,
        stats: Stats::default(),
        switches: Vec::new(),
        carry_lit: Vec::new(),
        carry_hot: None,
        failed: Vec::new(),
        hinting: None,
        back: Vec::new(),
        measure: Rc::new(move |size, s| match shared.borrow().get(&(size, s.to_owned())) {
            Some(v) => *v,
            None => 0.6 * (size * s.len() as i64) as f64,
        }),
        measured,
        tags: true,
        chain: Rc::new(RefCell::new(None)),
        built: Rc::new(RefCell::new(None)),
    }
}

/// The nodes upstream and downstream of the selection (it included), along the wires; `None` with
/// nothing selected. A wire is lit when both its ends are in the same side.
/// ponytail: one pass over the wires per selection or geometry change; index them if a 10k-wire graph drags.
pub fn chains_of(t: &Pane) -> Option<Chains> {
    if t.selected.is_empty() {
        return None;
    }
    if let Some((sel, wires, r)) = &*t.chain.borrow() {
        if *sel == t.selected && Rc::ptr_eq(wires, &t.geo.wires) {
            return Some(r.clone());
        }
    }
    let reach = |upstream: bool| -> PathSet {
        let mut seen = t.selected.clone();
        let mut todo: Vec<Path> = t.selected.iter().cloned().collect();
        while !todo.is_empty() {
            let batch = std::mem::take(&mut todo);
            for pth in batch {
                for wi in t.geo.wires.iter() {
                    if let (Some(into), Some(from)) = (&wi.into, &wi.from) {
                        // `Some (from, into) when path_equal from p`: the first of the pair is matched
                        // against the path, the second joins the set
                        let (first, second) = if upstream { (into, from) } else { (from, into) };
                        if *first == pth && !seen.contains(second) {
                            seen.insert(second.clone());
                            todo.push(second.clone());
                        }
                    }
                }
            }
        }
        seen
    };
    let up = reach(true);
    let down = reach(false);
    *t.chain.borrow_mut() = Some((t.selected.clone(), t.geo.wires.clone(), (up.clone(), down.clone())));
    Some((up, down))
}

pub fn wire_lit(c: &Chains, wi: &Wire) -> bool {
    let (up, down) = c;
    match (&wi.from, &wi.into) {
        (Some(f), Some(i)) => (up.contains(f) && up.contains(i)) || (down.contains(f) && down.contains(i)),
        _ => false,
    }
}

pub fn node_lit(c: &Chains, pth: &Path) -> bool {
    c.0.contains(pth) || c.1.contains(pth)
}

/// A wire longer than three columns reads as a name tag at its use site unless its chain is lit (flow.md 6.4).
pub fn tagged(t: &Pane, wi: &Wire) -> bool {
    t.tags && wi.target.is_some() && (wi.b.0 - wi.a.0).abs() > w::TAG_SPAN && !matches!(chains_of(t), Some(c) if wire_lit(&c, wi))
}

pub fn with_bounds(x: i64, y: i64, width: i64, height: i64, t: &mut Pane) {
    (t.x, t.y, t.width, t.height) = (x, y, width, height);
}

/// The pane is not drawn this frame: nothing it began stays open, and the pointer is given back.
pub fn suspend(t: &mut Pane) {
    if t.panning_grab {
        let _ = rays::sketch::set_relative_mouse(false);
    }
    t.editing = None;
    t.hinting = None;
    t.drag = None;
    t.context = None;
    t.highlighted = Vec::new();
    t.panning_grab = false;
}

pub fn with_guide(guide: bool, t: &mut Pane) {
    t.guide = guide;
}

pub fn with_theme(theme: Theme, t: &mut Pane) {
    t.theme = theme;
}

pub fn with_failed(failed: Vec<(Path, String)>, t: &mut Pane) {
    if t.failed != failed { t.failed = failed; }
}

pub fn with_carry(lit: Vec<(Path, String)>, hot: Option<(Path, bool)>, t: &mut Pane) {
    if t.carry_lit != lit || t.carry_hot != hot { t.carry_lit = lit; t.carry_hot = hot; }
}

pub fn selected(t: &Pane) -> Vec<Path> {
    t.selected.iter().cloned().collect()
}

pub fn editing(t: &Pane) -> bool {
    t.editing.is_some() || t.hinting.is_some()
}

pub fn select(paths: Vec<Path>, t: &mut Pane) {
    t.selected = paths.into_iter().collect();
    t.selected_wire = None;
}

pub fn clear_selection(t: &mut Pane) {
    t.selected.clear();
    t.selected_wire = None;
}

pub fn stats(t: &Pane) -> Stats {
    t.stats
}

pub fn zoom(t: &Pane) -> f64 {
    t.zoom
}

/// A screen point as a position inside `scope`, snapped to the 24-point dot lattice (where a node
/// added from the menu opened there goes).
pub fn scope_point(t: &Pane, scope: &[Symbol], mx: f64, my: f64) -> Option<(f64, f64)> {
    let (ox, oy) = t.geo.origins.iter().find(|(pth, _)| pth.as_slice() == scope).map(|(_, o)| *o)?;
    Some((p::snap(((mx - t.x as f64 - t.pan_x) / t.zoom) - ox), p::snap(((my - t.y as f64 - t.pan_y) / t.zoom) - oy)))
}

pub fn count_scope(s: &p::Scope) -> (i64, i64, i64) {
    let mut n = 0;
    let mut z = 0;
    let mut r = 0;
    for node in &s.nodes {
        let (n2, z2, r2) = match &node.zone {
            Some(zn) => count_scope(&zn.scope),
            None => (0, 0, 0),
        };
        n += 1 + n2;
        z += (if node.zone.is_some() { 1 } else { 0 }) + z2;
        r += node.rows.len() as i64 + r2;
    }
    (n, z, r)
}

pub fn frame_all(t: &mut Pane) {
    if t.layout.placed.is_empty() {
        return;
    }
    let wd = 1.0f64.max(t.layout.w);
    let ht = 1.0f64.max(t.layout.h);
    let zoom = 0.25f64.max(1.0f64.min((t.width as f64 / wd).min(t.height as f64 / ht)));
    // whole points, so the hairlines of the cards land on pixels
    let pan_x = ((t.width as f64 - (wd * zoom)) / 2.0).round();
    let pan_y = 8.0f64.max((t.height as f64 - (ht * zoom)) / 2.0).round();
    (t.zoom, t.pan_x, t.pan_y) = (zoom, pan_x, pan_y);
}

pub fn count_of(t: &Pane, path: &[Symbol]) -> i64 {
    t.counts.get(path).copied().unwrap_or(0)
}

/// The iteration counts follow the scope, the probes and what was recorded.
pub fn refresh(t: &mut Pane) {
    match (&t.scope, &t.records) {
        (Some(scope), Some(records)) => {
            let counts: HashMap<Path, i64> = records.borrow_mut().counts(scope, &|path: &[Symbol]| t.saved.probe(path)).into_iter().collect();
            t.counts = Rc::new(counts);
        }
        _ => { t.counts = Rc::new(HashMap::new()); },
    }
}

pub fn no_shift(_: &[Symbol]) -> (f64, f64) {
    (0.0, 0.0)
}

pub fn lens_of(t: &Pane, path: &[Symbol]) -> Option<i64> {
    t.lens.iter().find(|(pth, _)| pth.as_slice() == path).map(|(_, s)| *s)
}

/// The cards reserve a footer row once the host has probe records to show.
pub fn lay(t: &Pane, saved: &Rc<dyn Saved>, sticky: bool, scope: &p::Scope) -> p::Layout {
    // the cards stay where they were (flow.md 6.1): an edit places only the nodes it adds; a card whose
    // saved position was dropped (an undone drag) is a full layout again
    let sticky = sticky && !t.layout.placed.iter().any(|b| t.saved.at(&b.path).is_some() && saved.at(&b.path).is_none());
    let prev: Rc<dyn Fn(&[Symbol]) -> Option<(f64, f64, f64, f64)>> = if !sticky || t.layout.placed.is_empty() {
        Rc::new(|_| None)
    } else {
        let mut h: HashMap<Path, (f64, f64, f64, f64)> = HashMap::new();
        for b in &t.layout.placed {
            h.insert(b.path.clone(), (b.x, b.y, b.w, b.h));
        }
        Rc::new(move |pth: &[Symbol]| h.get(pth).copied())
    };
    let lens = t.lens.clone();
    let mut o = p::LayoutOptions::new();
    o.foot = t.records.is_some();
    let s = saved.clone();
    o.at = Rc::new(move |pth: &[Symbol]| s.at(pth));
    o.prev = prev;
    let s = saved.clone();
    o.collapsed = Rc::new(move |pth: &[Symbol]| s.collapsed(pth));
    o.lens = Rc::new(move |pth: &[Symbol]| lens.iter().find(|(x, _)| x.as_slice() == pth).map(|(_, s)| *s));
    let s = saved.clone();
    o.level = Rc::new(move |pth: &[Symbol]| s.level(pth).map_or(Level::Card, |(l, _)| l));
    let s = saved.clone();
    o.pin = Rc::new(move |pth: &[Symbol], label: &str| s.pin(pth, label));
    p::layout(&o, scope)
}

fn drop_last(v: &[Symbol]) -> Path {
    if v.is_empty() { Vec::new() } else { v[..v.len() - 1].to_vec() }
}

/// The rows a fold button is drawn on (flow.md 7.7): a row wired from one named node of its scope or an
/// enclosing one, read nowhere else. The edit checks again and says why when it refuses.
/// ponytail: uses are counted by name over the whole graph, so a name shadowed in another scope gets no
/// button (the context menu still folds it); count per scope if that bites.
pub fn fold_sources(scope: &p::Scope) -> HashMap<(Path, usize), Path> {
    let mut uses: HashMap<String, i64> = HashMap::new();
    let mut binders: BTreeSet<Path> = BTreeSet::new();
    fn add(uses: &mut HashMap<String, i64>, name: &str) {
        *uses.entry(w::root_name(name).to_owned()).or_insert(0) += 1;
    }
    fn count(s: &p::Scope, uses: &mut HashMap<String, i64>, binders: &mut BTreeSet<Path>) {
        if let p::ScopeResult::Link(target) = &s.result {
            add(uses, target);
        }
        for n in &s.nodes {
            if n.binds.len() == 1 && n.binds[0] == n.name && !n.synthetic && !e::nested(&n.name) {
                binders.insert(n.path.clone());
            }
            for r in &n.rows {
                for name in p::sources(r) {
                    add(uses, &name);
                }
            }
            if let Some(z) = &n.zone {
                for r in &z.rail {
                    if let Some(ex) = &r.expr {
                        for name in e::free_names(ex) {
                            add(uses, &name);
                        }
                    }
                }
                count(&z.scope, uses, binders);
            }
        }
    }
    count(scope, &mut uses, &mut binders);
    fn binder(binders: &BTreeSet<Path>, sc: &[Symbol], name: &str) -> Option<Path> {
        let mut sc: Path = sc.to_vec();
        loop {
            if sc.is_empty() {
                return None;
            }
            let mut cand = sc.clone();
            cand.push(name.into());
            if binders.contains(&cand) {
                return Some(cand);
            }
            sc = drop_last(&sc);
        }
    }
    let mut out: HashMap<(Path, usize), Path> = HashMap::new();
    fn mark(s: &p::Scope, uses: &HashMap<String, i64>, binders: &BTreeSet<Path>, out: &mut HashMap<(Path, usize), Path>) {
        for n in &s.nodes {
            for (i, r) in n.rows.iter().enumerate() {
                if let p::Chip::Name(_) = &r.chip {
                    let srcs = p::sources(r);
                    if srcs.len() == 1 && !r.head && uses.get(&srcs[0]) == Some(&1) {
                        if let Some(b) = binder(binders, &drop_last(&n.path), &srcs[0]) {
                            out.insert((n.path.clone(), i), b);
                        }
                    }
                }
            }
            if let Some(z) = &n.zone {
                mark(&z.scope, uses, binders, out);
            }
        }
    }
    mark(scope, &uses, &binders, &mut out);
    out
}

/// A switch's rows read as the names of its layouts; these are its nodes with the active input.
pub fn switch_nodes(scope: &p::Scope) -> Vec<(Path, i64)> {
    scope
        .nodes
        .iter()
        .filter(|n| n.head == "ui/switch" && n.zone.is_none())
        .map(|n| {
            let active = n
                .rows
                .iter()
                .find(|r| r.key == ArgKey::Kw("active".to_owned()))
                .and_then(|r| match r.expr.as_ref().map(|x| &x.node) {
                    Some(SNode::Num(s)) => r.expr.as_ref().unwrap().name(*s).parse::<i64>().ok(),
                    _ => None,
                })
                .unwrap_or(0);
            (n.path.clone(), active)
        })
        .collect()
}

/// The optional arguments of `with_scope`.
#[derive(Clone)]
pub struct ScopeOpts {
    pub saved: Rc<dyn Saved>,
    pub display: Option<Path>,
    pub layouts: Vec<String>,
}

impl Default for ScopeOpts {
    fn default() -> ScopeOpts {
        ScopeOpts { saved: Rc::new(Nothing), display: None, layouts: Vec::new() }
    }
}

pub fn with_scope(o: ScopeOpts, key: &str, scope: Rc<p::Scope>, t: &mut Pane) {
    let ScopeOpts { saved, display, layouts } = o;
    let scope: Rc<p::Scope> = if layouts.is_empty() {
        scope
    } else {
        let mut sc = (*scope).clone();
        for n in sc.nodes.iter_mut() {
            let inputs = n.rows.iter().filter(|r| matches!(r.key, ArgKey::Pos(_)) && r.kind != p::RowKind::Add).count();
            if n.head != "ui/switch" || inputs != layouts.len() {
                continue;
            }
            for r in n.rows.iter_mut() {
                if let ArgKey::Pos(i) = r.key {
                    if r.kind != p::RowKind::Add {
                        r.label = layouts[i as usize].clone();
                    }
                }
            }
        }
        Rc::new(sc)
    };
    // letter hints, an open context menu and the hover name nodes and wires of the scope they were made over
    let same = matches!(&t.scope, Some(previous) if Rc::ptr_eq(previous, &scope));
    if !same {
        (t.hinting, t.context, t.highlighted) = (None, None, Vec::new());
    }
    let layout = lay(t, &saved, key == t.key, &scope);
    let earlier: Vec<Placed> = if key == t.key { t.layout.placed.clone() } else { Vec::new() };
    let (n, z, r) = count_scope(&scope);
    let stats = Stats { nodes: n, zones: z, rows: r, ..t.stats };
    let chains = Rc::new(probe::chains(&scope));
    let folds = Rc::new(fold_sources(&scope));
    let switches = switch_nodes(&scope);
    let layout = Rc::new(layout);
    (t.scope, t.saved, t.display, t.layout, t.switches, t.chains, t.folds, t.stats) = (Some(scope.clone()), saved, display, layout.clone(), switches, chains, folds, stats);
    regeo(t, &scope, &layout, &no_shift);
    // a card an edit added (an unfolded call lands a column left of its owner) that falls outside the view
    // brings the pane to it, nothing else moves
    if !(earlier.is_empty() || layout.placed.len() <= earlier.len() || t.width == 0) {
        let known: std::collections::HashSet<&Path> = earlier.iter().map(|b| &b.path).collect();
        if let Some(b) = layout.placed.iter().find(|b| !known.contains(&b.path)) {
            // the pan that puts [lo, hi] (graph units) inside [0, size] with a 24-point margin
            let zoom = t.zoom;
            let fit = |pan: f64, size: f64, lo: f64, hi: f64| -> f64 {
                if pan + (lo * zoom) < 8.0 {
                    24.0 - (lo * zoom)
                } else if pan + (hi * zoom) > size - 8.0 {
                    size - 24.0 - (hi * zoom)
                } else {
                    pan
                }
            };
            let pan_x = fit(t.pan_x, t.width as f64, b.x, b.x + b.w);
            let pan_y = fit(t.pan_y, t.height as f64, b.y, b.y + b.h);
            (t.pan_x, t.pan_y) = (pan_x, pan_y);
        }
    }
    // an edit that removed or moved a node drops it from the selection
    refresh(t);
    let kept: PathSet = t
        .selected
        .iter()
        .filter(|pth| {
            t.geo.slot.contains_key(*pth)
                // a row of a node (the inspector's subject, `holder @ [":b"]`) lives while its holder does
                || match pth.split_last() {
                    Some((last, holder)) if !holder.is_empty() => last.starts_with(':') && t.geo.slot.contains_key(holder),
                    _ => false,
                }
        })
        .cloned()
        .collect();
    t.selected = kept;
    let keep_wire = match &t.selected_wire {
        Some((sp, sk, _)) => t.geo.wires.iter().any(|wi| matches!(&wi.target, Some((tp, tk, _)) if sp == tp && sk == tk)),
        None => false,
    };
    if !keep_wire {
        t.selected_wire = None;
    }
    if let Some(Editing::Name(pth) | Editing::Default(pth) | Editing::Output(pth)) = &t.editing {
        if !t.geo.slot.contains_key(pth) {
            t.editing = None;
        }
    }
    if key != t.key {
        suspend(t);
        t.key = key.to_owned();
        t.framed = false;
        t.selected.clear();
        t.selected_wire = None;
    }
}

/// The panel of a macro call opened or stepped: its card changes size; `sticky = false` (Tidy) puts every
/// card back to the computed layout, dragged ones too.
pub fn relayout(sticky: Option<bool>, t: &mut Pane) {
    if let Some(scope) = t.scope.clone() {
        let layout = Rc::new(lay(t, &t.saved, sticky.unwrap_or(true), &scope));
        t.layout = layout.clone();
        regeo(t, &scope, &layout, &no_shift);
    }
}

pub fn tidy(t: &mut Pane) {
    relayout(Some(false), t)
}

pub fn with_display(display: Option<Path>, t: &mut Pane) {
    t.display = display;
}

pub fn with_arguments(changes: &[(Path, ArgKey, S)], t: &mut Pane) {
    let scope = match t.scope.clone() {
        None => return,
        Some(s) => s,
    };
    let changes: Vec<(Path, ArgKey, S)> = changes.iter().filter(|(pth, _, _)| t.geo.slot.contains_key(pth)).cloned().collect();
    if changes.is_empty() {
        return;
    }
    let scope = Rc::new(p::with_arguments(&changes, &scope));
    let mut nodes: HashMap<Path, p::Node> = HashMap::new();
    fn index(sc: &p::Scope, nodes: &mut HashMap<Path, p::Node>) {
        for node in &sc.nodes {
            nodes.insert(node.path.clone(), node.clone());
            if let Some(z) = &node.zone {
                index(&z.scope, nodes);
            }
        }
    }
    index(&scope, &mut nodes);
    let mut places: HashMap<Path, Placed> = HashMap::new();
    fn patch_layout(layout: &p::Layout, nodes: &HashMap<Path, p::Node>, places: &mut HashMap<Path, Placed>) -> p::Layout {
        let placed: Vec<Placed> = layout
            .placed
            .iter()
            .map(|pl| {
                let item = match &pl.item {
                    Item::Item(node) => match nodes.get(&node.path) {
                        Some(next) if next != node => Item::Item(next.clone()),
                        _ => pl.item.clone(),
                    },
                    other => other.clone(),
                };
                let lines = match (&pl.item, &item) {
                    (Item::Item(old), Item::Item(node)) if old.rows != node.rows => pl
                        .lines
                        .iter()
                        .map(|l| match l {
                            p::Line::Row(i, _) => p::Line::Row(*i, node.rows[*i].clone()),
                            other => other.clone(),
                        })
                        .collect(),
                    _ => pl.lines.clone(),
                };
                let inner = pl.inner.as_ref().map(|l| patch_layout(l, nodes, places));
                let np = Placed { item, lines, inner, ..pl.clone() };
                places.insert(np.path.clone(), np.clone());
                np
            })
            .collect();
        p::Layout { placed, ..layout.clone() }
    }
    let layout = patch_layout(&t.layout, &nodes, &mut places);
    let items: Vec<w::Item3> = t.geo.items.iter().map(|(pl, x, y)| (places.get(&pl.path).cloned().unwrap_or_else(|| pl.clone()), *x, *y)).collect();
    let geo = Geo { items: Rc::new(items), ..t.geo.clone() };
    (t.scope, t.layout, t.geo) = (Some(scope), Rc::new(layout), geo);
}

pub fn with_records(records: Rc<RefCell<Probe>>, t: &mut Pane) {
    let first = t.records.is_none();
    let same = t.records.as_ref().is_some_and(|previous| previous.borrow().same_eval(&records.borrow()));
    t.records = Some(records);
    if !same {
        refresh(t);
        if first { relayout(None, t); }
    }
}

// ------------------------------------------------------------- lookups

pub fn node_of(t: &Pane, path: &[Symbol]) -> Option<p::Node> {
    t.scope.as_ref().and_then(|s| p::find(s, path))
}

pub fn collapsed_zone(t: &Pane, path: &[Symbol]) -> bool {
    t.saved.collapsed(path) && matches!(node_of(t, path), Some(n) if n.zone.is_some())
}

impl Pane {
    pub fn item_at(&self, path: &[Symbol]) -> Option<&w::Item3> {
        self.geo.slot.get(path).map(|i| &self.geo.items[*i])
    }
}

/// An item's box in graph units, and where it sits inside its scope.
pub fn box_at(t: &Pane, path: &[Symbol]) -> Option<(f64, f64, f64, f64)> {
    t.item_at(path).map(|(pl, ax, ay)| (*ax, *ay, pl.w, pl.h))
}

pub fn rel_at(t: &Pane, path: &[Symbol]) -> Option<(f64, f64)> {
    t.item_at(path).map(|(pl, _, _)| (pl.x, pl.y))
}

pub fn selected_nodes(t: &Pane) -> Vec<p::Node> {
    selected(t).iter().filter_map(|pth| node_of(t, pth)).collect()
}
