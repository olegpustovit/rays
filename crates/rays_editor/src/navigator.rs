//! the Outline panel, a map of the document. A search
//! field, then a section per kind of graph (the scene with its objects and their visible and render
//! flags, geometry and materials with how many graphs use each, the World, the layouts), the active
//! graph unfolded into its loops and bindings, the reusable functions and macros, the active graph's
//! inputs as sliders and the data flow around it. Pure state and rows; `view` draws inside
//! `Ui::frame` and returns intents that `Editor_frame` applies after the frame.
use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use flow_check::workspace::{self as w, Graph, Workspace};
use flow_graph::probe::{self, Probe};
use flow_graph::projection as p;
use flow_syntax::context::{self, Context};
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::ty::{self, ColorKind, Ty};
use flow_syntax::value::Value;
use input::Key;
use pxui::theme::{self, Tokens as Theme};
use pxui::ui::*;
use rays_shell::kit::{self, ButtonOpts};
use pxui::geom::color::{self, T as Color};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

type S = Syntax;
pub type Path = flow_check::workspace::Path;

#[derive(Clone, Debug, PartialEq)]
pub enum Intent {
    /// show a graph (a `defn` is `"def:name"`) in the graph pane, selecting `node` there
    Open { graph: String, node: Option<Path> },
    /// a graph input's default, dragged or typed in its slider
    SetDefault { graph: String, input: String, value: f64, integer: bool },
    /// a macro row: its definition in the text pane
    Macro(String),
    /// F2 on a graph row
    Rename { graph: String, to: String },
    /// Delete on a graph row
    Remove(String),
    /// a row of the Layout section: show that layout
    Layout(i64),
    /// the context menu's "New graph" entry: a graph of this context name (`"sop"`, `"scene"` ...)
    NewGraph(String),
    /// the `+` beside the search field: the add menu
    Add,
    /// a press on an object's visible or render flag: that argument of its binding
    Flag { node: Path, name: String, value: bool },
}

/// `rename`: the graph whose name the field holds, and whether the field has opened yet
#[derive(Clone, Debug, PartialEq)]
pub struct State {
    query: String,
    typing: bool,
    rename: Option<(String, bool)>,
    /// the scene's root row is folded: its objects are hidden
    scene_closed: bool,
    /// graphs whose node rows are unfolded under their row
    opened: Vec<String>,
    /// the right-click menu: its point and the graph row it was opened on
    context: Option<(f32, f32, Option<String>)>,
}

/// The contexts "New graph" offers: sop, scene, draw, image, value, material.
pub fn new_graph_contexts() -> Vec<String> {
    ["sop", "scene", "draw", "image", "value", "material"].iter().map(|s| s.to_string()).collect()
}

pub fn initial() -> State {
    State { query: String::new(), typing: false, rename: None, scene_closed: false, opened: Vec::new(), context: None }
}

/// Unfold the node rows of a graph under its row (they are folded by default).
pub fn open_graph(graph: &str, s: &State) -> State {
    if s.opened.iter().any(|g| g == graph) {
        s.clone()
    } else {
        let mut next = s.clone();
        next.opened.insert(0, graph.to_owned());
        next
    }
}

pub fn with_query(query: &str, s: &State) -> State {
    State { query: query.to_owned(), ..s.clone() }
}

/// The search field holding this text.
pub fn query(s: &State) -> &str {
    &s.query
}

/// A scene object's row: its kind's letter, name and detail, its visible and render flags (where it
/// has them), `lead` for the render camera, `inert` when the render mark only shows (the camera's,
/// the World's: a press on it opens the row), `chosen` when selected, and the binding that places
/// it (a click opens its graph on it).
#[derive(Clone, Debug, PartialEq)]
pub struct Obj {
    pub depth: i64,
    pub letter: String,
    pub name: String,
    pub detail: String,
    pub visible: Option<bool>,
    pub render: Option<bool>,
    pub lead: bool,
    pub inert: bool,
    pub chosen: bool,
    pub home: Option<Path>,
}

#[derive(Clone)]
pub struct Params {
    pub workspace: Arc<Workspace>,
    /// the graph the pane shows
    pub active: Option<String>,
    /// the projection of `active`
    pub scope: Option<Rc<p::Scope>>,
    pub records: Option<Rc<RefCell<Probe>>>,
    /// the iteration each zone shows
    pub probes: Rc<dyn Fn(&[Symbol]) -> i64>,
    pub selected: Vec<Path>,
    /// the evaluated colour of each material graph
    pub chips: Vec<(String, Color)>,
    /// the scene's objects, in tree order
    pub objects: Vec<Obj>,
    /// what the scene's root row says beside its name
    pub root_detail: String,
    /// the layouts of the editor graph, and the one shown
    pub layouts: Option<(Vec<String>, i64)>,
    /// what a material graph says beside its swatch (`notes`)
    pub notes: Vec<(String, String)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Row {
    /// a section's label and what its right column is
    Head(String, String),
    ObjectRow(Obj),
    RootRow { graph: String, detail: String, folded: bool, active: bool },
    LayoutRow { index: i64, label: String, active: bool },
    InputRow { graph: String, name: String, integer: bool, value: f64 },
    GraphRow { graph: String, label: String, context: Option<Context>, detail: String, active: bool, chip: Option<Color>, dim: bool, used: Option<i64> },
    NodeRow { graph: String, path: Path, depth: i64, label: String, detail: String, zone: Option<String>, ty: Ty, result: bool, selected: bool },
    MacroRow(String, i64),
    LinkRow { label: String, graph: String, outgoing: bool },
    Empty(String),
}

// ---- reading the source ----

fn walk(f: &mut dyn FnMut(&S), form: &S) {
    f(form);
    for &c in form.children() {
        walk(f, &form.child(c));
    }
}

fn count_where(pred: &dyn Fn(&S) -> bool, form: &S) -> i64 {
    let mut n = 0;
    walk(&mut |f| if pred(f) { n += 1 }, form);
    n
}

fn loops(form: &S) -> i64 {
    count_where(&|f| matches!(s::head(f), Some("for" | "fold" | "scan" | "sum")), form)
}

/// the graphs a graph form reads with `(ref name ...)`
fn reads(form: &S) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    walk(
        &mut |f| {
            if s::head(f) == Some("ref")
                && let Some(&id) = f.children().get(1)
                && let Node::Sym(n) = f.arena().row(id).node
                && !names.iter().any(|name| name == f.name(n))
            {
                names.push(f.name(n).to_owned());
            }
        },
        form,
    );
    names
}

fn byte(f: f64) -> i64 {
    (255.0 * f.min(1.0).max(0.0)).round() as i64
}


/// The evaluated colour of each material graph (a vector or hex text; white when unset).
pub fn chips(ev: &flow_eval::Eval) -> Vec<(String, Color)> {
    ev.results
        .iter()
        .filter_map(|(name, v)| match v {
            Value::Struct(p) if p.name == "material/standard" => {
                let fields = &p.fields;
                let colour = match fields.get_str("color") {
                    Some(Value::Vec3(r, g, b)) => Some(color::rgb(byte(*r), byte(*g), byte(*b))),
                    Some(Value::Text(hex)) => color::hex(&String::from_utf8_lossy(hex)).ok(),
                    None => Some(color::white()),
                    Some(_) => None,
                };
                colour.map(|c| (name.as_str().to_owned(), c))
            }
            _ => None,
        })
        .collect()
}

/// The roughness of each evaluated material graph, as its row's detail.
pub fn notes(ev: &flow_eval::Eval) -> Vec<(String, String)> {
    ev.results
        .iter()
        .filter_map(|(name, v)| match v {
            Value::Struct(p) if p.name == "material/standard" => match p.fields.get_str("roughness") {
                Some(Value::Float(r)) => Some((name.as_str().to_owned(), format!("rough {}", cfmt::g(*r, 2)))),
                Some(Value::Int(r)) => Some((name.as_str().to_owned(), format!("rough {}", r))),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// The outline's groups, in dependency order.
fn group_of(c: Context) -> &'static str {
    context::descriptor(c).group
}

fn group_order() -> Vec<&'static str> {
    let mut groups: Vec<&'static str> = Vec::new();
    for c in context::all() {
        let g = group_of(c);
        if !groups.contains(&g) {
            groups.push(g);
        }
    }
    groups
}

fn grouped(ws: &Workspace) -> Vec<(&'static str, Vec<&Graph>)> {
    group_order()
        .into_iter()
        .filter_map(|group| {
            let graphs: Vec<&Graph> = ws.graphs.iter().filter(|g| group_of(g.context) == group).collect();
            if graphs.is_empty() { None } else { Some((group, graphs)) }
        })
        .collect()
}

/// Every graph with its outline group (Scene, Geometry, Materials, World, Layout, ...), in the
/// outline's order: what `/ j` filters.
pub fn jump_rows(ws: &Workspace) -> Vec<(String, String)> {
    grouped(ws).into_iter().flat_map(|(group, graphs)| graphs.into_iter().map(move |g| (g.name.clone(), group.to_owned()))).collect()
}

/// how many other graphs read `name`
fn readers(ws: &Workspace, name: &str) -> i64 {
    ws.graphs.iter().filter(|g| g.name != name && reads(&g.form).iter().any(|n| n == name)).count() as i64
}

fn calls(ws: &Workspace, defs: &[&Graph], macros: &[&S], name: &str) -> i64 {
    let mut forms: Vec<&S> = ws.graphs.iter().map(|g| &g.form).collect();
    forms.extend(defs.iter().filter(|d| d.name != name).map(|d| &d.form));
    forms.extend(macros.iter().copied());
    forms.iter().fold(0, |n, form| n + count_where(&|f| s::head(f) == Some(name), form))
}

fn macro_name(form: &S) -> Option<String> {
    if s::head(form) == Some("defmacro")
        && let Some(&id) = form.children().get(1)
        && let Node::Sym(n) = form.arena().row(id).node
    {
        return Some(form.name(n).to_owned());
    }
    None
}

pub(crate) fn plural(n: i64, what: &str) -> String {
    format!("{} {}{}", n, what, if n == 1 { "" } else { "s" })
}

fn zone_glyph(kind: p::ZoneKind) -> &'static str {
    match kind {
        p::ZoneKind::For => "for",
        p::ZoneKind::Fold => "fold",
        p::ZoneKind::Scan => "scan",
        p::ZoneKind::Sum => "sum",
        p::ZoneKind::Let => "let*",
        p::ZoneKind::Fn => "\u{3bb}",
        p::ZoneKind::State => "state",
        p::ZoneKind::Branch => "?",
    }
}

fn number(form: &S) -> Option<f64> {
    match &form.node {
        Node::Num(text) => (form.name(*text)).parse::<f64>().ok(),
        _ => None,
    }
}

// ---- rows ----

type Chains = HashMap<Path, Vec<Path>>;

fn node_rows(graph: &str, depth: i64, pa: &Params, scope: &p::Scope, chains: &Chains, counts: &[(Path, i64)], acc: &mut Vec<Row>) {
    for n in &scope.nodes {
        let chain: Vec<Path> = chains.get(&n.path).cloned().unwrap_or_default();
        let detail = match (&n.zone, &pa.records) {
            (Some(z), _) if z.kind == p::ZoneKind::Let => "scope".to_owned(),
            (Some(_), _) => match counts.iter().find(|(path, _)| *path == n.path) {
                Some((_, c)) => format!("{}\u{d7}", c),
                None => String::new(),
            },
            (None, Some(records)) => {
                let probes: Vec<i64> = chain.iter().map(|c| (pa.probes)(c)).collect();
                records.borrow_mut().footer(n, &probes).value
            }
            (None, None) => String::new(),
        };
        acc.push(Row::NodeRow {
            graph: graph.to_owned(),
            path: n.path.clone(),
            depth,
            detail,
            ty: n.ty.clone(),
            result: n.synthetic,
            label: p::title(n),
            zone: n.zone.as_ref().map(|z| zone_glyph(z.kind).to_owned()),
            selected: pa.selected.contains(&n.path),
        });
        if let Some(z) = &n.zone {
            node_rows(graph, depth + 1, pa, &z.scope, chains, counts, acc);
        }
    }
}

fn contains(query: &str, text: &str) -> bool {
    let (q, t) = (query.to_ascii_lowercase(), text.to_ascii_lowercase());
    q.is_empty() || t.contains(&q)
}

fn graph_row(pa: &Params, g: &Graph, detail: String) -> Row {
    let mut used = readers(&pa.workspace, &g.name);
    // the scene's own reference counts: its World object reads the graph
    if g.context == context::world() {
        let objects = pa.objects.iter().filter(|o| o.letter == "W" && o.detail == format!("ref {}", g.name)).count() as i64;
        used = used.max(objects);
    }
    let unused = used == 0 && (g.context == context::material() || g.context == context::sop());
    Row::GraphRow {
        graph: g.name.clone(),
        label: g.name.clone(),
        context: Some(g.context),
        active: pa.active.as_deref() == Some(g.name.as_str()),
        chip: if g.context == context::material() { pa.chips.iter().find(|(n, _)| *n == g.name).map(|(_, c)| *c) } else { None },
        dim: unused,
        // how many graphs read it, in the section's right column
        used: if g.context == context::material() || g.context == context::sop() || g.context == context::world() { Some(used) } else { None },
        detail: if unused {
            "unused".to_owned()
        } else if g.context == context::material() {
            pa.notes.iter().find(|(n, _)| *n == g.name).map(|(_, d)| d.clone()).unwrap_or_default()
        } else {
            detail
        },
    }
}

fn search(state: &State, pa: &Params, defs: &[&Graph], chains: &Chains, counts: &[(Path, i64)]) -> Vec<Row> {
    let q = state.query.trim().to_owned();
    let mut acc: Vec<Row> = Vec::new();
    let ws = &pa.workspace;
    for g in ws.graphs.iter() {
        if contains(&q, &g.name) {
            acc.push(graph_row(pa, g, group_of(g.context).to_owned()));
        }
    }
    for d in defs {
        if contains(&q, &d.name) {
            let graph = format!("def:{}", d.name);
            acc.push(Row::GraphRow {
                active: pa.active.as_deref() == Some(graph.as_str()),
                graph,
                label: d.name.clone(),
                context: None,
                detail: "function".to_owned(),
                chip: None,
                dim: false,
                used: None,
            });
        }
    }
    if let (Some(scope), Some(graph)) = (&pa.scope, &pa.active) {
        let mut inner = Vec::new();
        node_rows(graph, 0, pa, scope, chains, counts, &mut inner);
        for row in inner {
            if let Row::NodeRow { label, .. } = &row
                && contains(&q, label)
            {
                acc.push(row);
            }
        }
    }
    let hits = acc.len() as i64;
    let mut rows = vec![Row::Head(format!("{} \u{b7} click opens", plural(hits, "match")), String::new())];
    rows.extend(acc);
    if hits == 0 {
        rows.push(Row::Empty("nothing matches".to_owned()));
    }
    rows
}

fn def_prefix(graph: &str) -> bool {
    graph.len() > 4 && graph.starts_with("def:")
}

/// Every row in order; a search shows the matching bindings instead. Under 300 points (not
/// `wide`) the outline is the workspace sheet's: no node rows, no Layout, no Data flow.
pub fn rows(wide: bool, state: &State, pa: &Params) -> Vec<Row> {
    let ws: &Workspace = &pa.workspace;
    // only the definitions and macros the document itself authors (not an import's)
    let authored: HashSet<i64> = ws
        .source
        .iter()
        .flat_map(|form| match &form.node {
            Node::List(l) if form.range(*l).len() >= 2 && s::head(form) == Some("workspace") => form.range(*l)[2..].iter().map(|&id| form.arena().row(id).id).collect::<Vec<_>>(),
            _ => vec![form.id],
        })
        .collect();
    let (defs, macros): (Vec<&Graph>, Vec<&S>) = if ws.defs.is_empty() && ws.macros.is_empty() {
        (Vec::new(), Vec::new())
    } else {
        (ws.defs.iter().filter(|d| authored.contains(&d.form.id)).collect(), ws.macros.iter().filter(|m| authored.contains(&m.id)).collect())
    };
    let chains: Chains = match &pa.scope {
        Some(sc) => probe::chains(sc),
        None => HashMap::new(),
    };
    let counts: Vec<(Path, i64)> = match (&pa.scope, &pa.records) {
        (Some(scope), Some(records)) => {
            let probes = pa.probes.clone();
            records.borrow_mut().counts(scope, &|path| probes(path))
        }
        _ => Vec::new(),
    };
    if !state.query.trim().is_empty() {
        return search(state, pa, &defs, &chains, &counts);
    }
    let mut acc: Vec<Row> = Vec::new();
    let active_tree = |acc: &mut Vec<Row>, graph: &str| {
        if let Some(scope) = &pa.scope
            && wide
            && pa.active.as_deref() == Some(graph)
            && state.opened.iter().any(|g| g == graph)
        {
            node_rows(graph, 1, pa, scope, &chains, &counts, acc);
        }
    };
    let nodes = |form: &S| count_where(&|f| matches!(s::head(f), Some(h) if h.contains('/')), form);
    let groups = grouped(ws);
    // the narrow sheet has no Layout section, and so none of the editor graphs
    let groups: Vec<(&str, Vec<&Graph>)> = if wide { groups } else { groups.into_iter().filter(|(g, _)| *g != "Layout").collect() };
    // the scene's first graph is the root row; its objects are its tree
    let root_graph = ws.graphs.iter().find(|g| g.context == context::scene());
    for (group, graphs) in &groups {
        acc.push(Row::Head(
            group.to_string(),
            match *group {
                "Scene" => "v  r",
                "Geometry" | "Materials" => "used",
                "Layout" => "/ [",
                _ => "",
            }
            .to_owned(),
        ));
        let graph_rows = |acc: &mut Vec<Row>, graphs: &[&Graph]| {
            for g in graphs {
                let n = loops(&g.form);
                let detail = if g.context == context::sop() {
                    format!("{}{}", plural(nodes(&g.form), "node"), if n > 0 { format!(" \u{b7} {}", plural(n, "loop")) } else { String::new() })
                } else if n > 0 {
                    plural(n, "loop")
                } else {
                    w::context_name(&g.context)
                };
                acc.push(graph_row(pa, g, detail));
                active_tree(acc, &g.name);
            }
        };
        if *group == "Scene" {
            match root_graph {
                Some(g) => {
                    acc.push(Row::RootRow {
                        graph: g.name.clone(),
                        detail: pa.root_detail.clone(),
                        folded: state.scene_closed,
                        active: pa.active.as_deref() == Some(g.name.as_str()),
                    });
                    if !state.scene_closed {
                        acc.extend(pa.objects.iter().cloned().map(Row::ObjectRow));
                    }
                    let rest: Vec<&Graph> = graphs.iter().filter(|x| !std::ptr::eq(**x, g)).copied().collect();
                    graph_rows(&mut acc, &rest);
                }
                None => {
                    graph_rows(&mut acc, graphs);
                    acc.extend(pa.objects.iter().cloned().map(Row::ObjectRow));
                }
            }
        } else if *group == "Layout" {
            // the numbered layouts, then the editor graphs that hold them
            if let Some((labels, active)) = &pa.layouts {
                for (index, label) in labels.iter().enumerate() {
                    acc.push(Row::LayoutRow { index: index as i64, label: label.clone(), active: index as i64 == *active });
                }
            }
            graph_rows(&mut acc, graphs);
        } else {
            graph_rows(&mut acc, graphs);
        }
    }
    if !defs.is_empty() || !macros.is_empty() {
        acc.push(Row::Head("Definitions".to_owned(), "used".to_owned()));
        for d in &defs {
            let n = calls(ws, &defs, &macros, &d.name);
            let graph = format!("def:{}", d.name);
            acc.push(Row::GraphRow {
                label: d.name.clone(),
                context: None,
                detail: if d.inputs.is_empty() {
                    "defn".to_owned()
                } else {
                    format!("defn \u{b7} {}", d.inputs.iter().map(|(name, _, _)| name.as_str()).collect::<Vec<_>>().join(" "))
                },
                active: pa.active.as_deref() == Some(graph.as_str()),
                chip: None,
                dim: false,
                used: Some(n),
                graph: graph.clone(),
            });
            active_tree(&mut acc, &graph);
        }
        for m in &macros {
            if let Some(name) = macro_name(m) {
                let uses = calls(ws, &defs, &macros, &name);
                acc.push(Row::MacroRow(name, uses));
            }
        }
    }
    // the active graph's inputs
    if let (Some(scope), Some(graph)) = (&pa.scope, &pa.active)
        && !scope.inputs.is_empty()
    {
        acc.push(Row::Head(
            "Inputs".to_owned(),
            if !wide {
                String::new()
            } else if def_prefix(graph) {
                graph[4..].to_owned()
            } else {
                graph.clone()
            },
        ));
        for i in &scope.inputs {
            if let Some(value) = i.default.as_ref().and_then(number)
                && (i.ty == Ty::Int || i.ty == Ty::Float)
            {
                acc.push(Row::InputRow { graph: graph.clone(), name: i.name.clone(), integer: i.ty == Ty::Int, value });
            }
        }
    }
    if let Some(graph) = &pa.active
        && wide
        && !def_prefix(graph)
    {
        let out: Vec<String> = match ws.graphs.iter().find(|g| g.name == *graph) {
            Some(g) => reads(&g.form).into_iter().filter(|n| ws.graphs.iter().any(|x| x.name == *n)).collect(),
            None => Vec::new(),
        };
        let by: Vec<String> =
            ws.graphs.iter().filter(|g| g.name != *graph && reads(&g.form).contains(graph)).map(|g| g.name.clone()).collect();
        acc.push(Row::Head("Data flow".to_owned(), graph.clone()));
        for n in &out {
            acc.push(Row::LinkRow { label: "reads".to_owned(), graph: n.clone(), outgoing: true });
        }
        for n in &by {
            acc.push(Row::LinkRow { label: "read by".to_owned(), graph: n.clone(), outgoing: false });
        }
        if out.is_empty() && by.is_empty() {
            acc.push(Row::Empty("no other graph reads or is read".to_owned()));
        }
    }
    acc
}

/// One line per row, for tests and crash reports.
pub fn describe(row: &Row) -> String {
    match row {
        Row::Head(s, right) => {
            if right.is_empty() {
                s.to_ascii_uppercase()
            } else {
                format!("{} \u{b7} {}", s.to_ascii_uppercase(), right)
            }
        }
        Row::ObjectRow(o) => format!(
            "{}{} {} \u{b7} {}{}{}",
            " ".repeat(2 * o.depth.max(0) as usize),
            o.letter,
            o.name,
            o.detail,
            if o.visible == Some(true) { " \u{b7} v" } else { "" },
            if o.render == Some(true) { " \u{b7} r" } else { "" }
        ),
        Row::RootRow { graph, detail, folded, .. } => format!("R root ({}) \u{b7} {}{}", graph, detail, if *folded { " \u{b7} folded" } else { "" }),
        Row::LayoutRow { index, label, active } => format!("layout {} {}{}", index, label, if *active { " *" } else { "" }),
        Row::InputRow { name, value, .. } => format!("input {} = {}", name, cfmt::g(*value, 6)),
        Row::GraphRow { label, detail, active, chip, used, .. } => {
            // an unused graph says so in its detail, any other how often it is read
            let parts = [
                format!("{}{}", if *active { "> " } else { "" }, label),
                detail.clone(),
                match used {
                    Some(n) if detail != "unused" => format!("\u{d7}{}", n),
                    _ => String::new(),
                },
                match chip {
                    Some(c) => color::to_hex_string(color::with_alpha(*c, 255)),
                    None => String::new(),
                },
            ];
            parts.into_iter().filter(|p| !p.is_empty()).collect::<Vec<_>>().join(" \u{b7} ")
        }
        Row::NodeRow { label, detail, depth, zone, .. } => format!(
            "{}{}{} \u{b7} {}",
            " ".repeat(2 * (*depth).max(0) as usize),
            match zone {
                Some(z) => format!("{} ", z),
                None => String::new(),
            },
            label,
            detail
        ),
        Row::MacroRow(name, n) => format!("\u{3bb} {} \u{b7} {}", name, plural(*n, "use")),
        Row::LinkRow { label, graph, .. } => format!("{} {}", label, graph),
        Row::Empty(s) => s.clone(),
    }
}

// ---- drawing ----

/// a graph's square wears a port colour of the kit: geometry for a SOP graph, record for a World
fn context_color(theme: &Theme, c: Option<Context>) -> Color {
    let c = c.unwrap_or(context::sop());
    rays_graph::node_menu::color(theme, context::descriptor(c).color)
}

fn type_color(theme: &Theme, t: &Ty) -> Color {
    let ports = rays_graph::scope_paint::ports(theme);
    match t {
        Ty::Named(_) => rays_graph::node_menu::color(theme, match ty::color(t) { ColorKind::Output => ColorKind::Compound, c => c }),
        Ty::Float => ports.float,
        Ty::Int => ports.int,
        Ty::Bool => ports.bool,
        Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => ports.vec3,
        Ty::Text => ports.text,
        Ty::Fn(_) => ports.fn_,
        Ty::Record(_) => ports.record,
        Ty::List(_) | Ty::Array(_) | Ty::Color | Ty::Any => ports.compound,
    }
}

fn height_of(rh: f32, row: &Row) -> f32 {
    match row {
        Row::Head(..) => rh + 16.0,
        _ => rh,
    }
}

/// the top of every row below the search field, and the total
fn tops(rh: f32, rows: &[Row]) -> Vec<f32> {
    let mut tops = vec![0.0; rows.len() + 1];
    // a section has 16 points of space above it, the first one 4
    for (i, r) in rows.iter().enumerate() {
        tops[i + 1] = tops[i] + height_of(rh, r) - if i == 0 && matches!(r, Row::Head(..)) { 12.0 } else { 0.0 };
    }
    tops
}

/// the search row: 4 points of margin over a 24-point row
const SEARCH_HEIGHT: f32 = 28.0;

/// Every row with its rectangle when not scrolled: where `view` draws it and where a click on it lands.
pub fn row_rects(state: &State, pa: &Params, bounds: (i64, i64, i64, i64)) -> Vec<(Row, (f32, f32, f32, f32))> {
    let (x, y, w, _) = bounds;
    let (rh, x, y, w) = (24.0, x as f32, y as f32, w as f32);
    let rows = rows(w >= 300.0, state, pa);
    let t = tops(rh, &rows);
    rows.into_iter().enumerate().map(|(i, r)| (r, (x, y + SEARCH_HEIGHT + t[i], w, t[i + 1] - t[i]))).collect()
}

struct PaintCtx {
    rows: Vec<Row>,
    tops: Vec<f32>,
    theme: Theme,
    muted: Color,
    hovered: Option<usize>,
    rh: f32,
    body: f32,
    w: f32,
    top: f32,
    x: f32,
    scroll: f32,
    wide: bool,
    label_w: f32,
    has_scope: bool,
    opened: Vec<String>,
    small: i64,
}

fn ell<'a>(paint: &mut Paint, limit: f32, label: &'a str) -> Cow<'a, str> {
    ellipsis(&mut |s| paint.text_width(None, s), limit, label)
}

/// hover is the faintest line, the current row the control fill, the selected one adds the accent brackets
fn shade(paint: &mut Paint, c: &PaintCtx, k: usize, ry: f32, rhh: f32, selected: bool, current: bool) {
    let (x, w, theme) = (c.x, c.w, &c.theme);
    if selected {
        paint.fill(x + 4.0, ry, w - 8.0, rhh, theme.control);
        paint.brackets(x + 4.0, ry, w - 8.0, rhh, 3.0, 8.0, 2.0, theme.accent);
    } else if current {
        paint.fill(x, ry, w, rhh, theme.control);
    } else if c.hovered == Some(k) {
        paint.fill(x, ry, w, rhh, theme::faint_border(theme));
    }
}

/// the detail at the row's end takes at most half the row; the label keeps the rest
fn right_text(paint: &mut Paint, c: &PaintCtx, ty: f32, label: &str) -> f32 {
    let label = ell(paint, (c.w - 24.0) / 2.0, label);
    let tw = paint.text_width(None, &label);
    paint.text_c((c.x + c.w - tw - 12.0, ty), c.muted, &label);
    c.x + c.w - tw - 20.0
}

/// a label with a detail at the right stops short of it instead of running under it
fn labelled(paint: &mut Paint, c: &PaintCtx, ty: f32, color: Color, from: f32, label: &str, detail: &str) {
    let edge = right_text(paint, c, ty, detail);
    let cut = ell(paint, edge - from, label);
    paint.text_c((from, ty), color, &cut);
}

fn square(paint: &mut Paint, c: &PaintCtx, ry: f32, sx: f32, color: Color) {
    paint.fill(sx, ry + (c.rh / 2.0) - 4.0, 8.0, 8.0, color);
}

fn paint_row(paint: &mut Paint, c: &PaintCtx, k: usize, row: &Row) {
    let (x, w, rh, theme, muted, wide) = (c.x, c.w, c.rh, &c.theme, c.muted, c.wide);
    let ry = c.top + c.tops[k] - c.scroll;
    let rhh = c.tops[k + 1] - c.tops[k];
    let ink_3 = theme::ink_3(theme);
    let ty = paint.text_top(None, ry, rh);
    // a zone's kind: an accent label, no fill
    let tag = |paint: &mut Paint, tx: f32, glyph: &str| -> f32 {
        paint.cap((tx, kit::cap_y_paint(paint, ry, rh)), Some(theme.accent), glyph);
        tx + paint.cap_width(glyph) + 8.0
    };
    // the right columns: a 20-point count, or the two 12-point flags 8 apart
    let flag_x = |column: i64| x + w - 12.0 - 12.0 - ((1 - column) as f32 * 20.0);
    match row {
        Row::Head(s, right) => {
            let cy = kit::cap_y_paint(paint, ry + rhh - rh, rh);
            paint.cap((x + 12.0, cy), Some(ink_3), s);
            // the right column is a label in ink-3 too: two 12-point columns over the flags, a word, or the leader
            // keys as the kit's key text
            if right == "v  r" {
                for (column, name) in ["v", "r"].iter().enumerate() {
                    let cw = paint.cap_width(name);
                    paint.cap((flag_x(column as i64) + 6.0 - (cw / 2.0), cy), Some(ink_3), name);
                }
            } else if right == "/ [" {
                let kw = paint.text_width(Some(c.small), right);
                paint.text_s((x + w - 12.0 - kw, cy), c.small, ink_3, right);
            } else if !right.is_empty() && (wide || right != "used") {
                let cw = paint.cap_width(right);
                paint.cap((x + w - 12.0 - cw, cy), Some(ink_3), right);
            }
        }
        Row::ObjectRow(o) => {
            shade(paint, c, k, ry, rhh, o.chosen, false);
            // the objects are the children of the root row (its letter at 26 wide, 12 narrow), each level 8 points in; wide,
            // the root's chevron takes the first 14; the selected row sits in a 4-point wrapper with its own
            // padding (15 and 3), so its letter is a point left and its flags 5 points right
            let ox = if o.chosen && wide {
                x + 26.5
            } else {
                x + (if wide { 38.0 } else { 20.0 }) + (8.0 * o.depth as f32) - if o.chosen { 1.0 } else { 0.0 }
            };
            // the selected object stands in the root's indent with a chevron of its own
            if o.chosen && wide {
                paint.chevron((x + 15.0, ry + (rh / 2.0)), Dir::Down, theme.foreground);
            }
            let right = x + w - if o.chosen { 7.0 } else { 12.0 };
            let flag_x = |column: i64| right - 12.0 - ((1 - column) as f32 * 20.0);
            paint.cap((ox, kit::cap_y_paint(paint, ry, rh)), Some(if o.chosen { theme.foreground } else { muted }), &o.letter);
            let cy = ry + (rh / 2.0);
            // a flag is a 12-point box with its 1-point line and a 6-point mark inset 2 inside it: the render camera's is the accent
            if let Some(on) = o.visible {
                let fx = flag_x(0);
                // a stroke is centred on its edge: inset half a point to keep the box 12 points
                paint.fill(fx, cy - 6.0, 12.0, 12.0, theme.input);
                paint.stroke(fx + 0.5, cy - 5.5, 11.0, 11.0, theme::border(theme));
                if on {
                    paint.fill(fx + 3.0, cy - 3.0, 6.0, 6.0, muted);
                }
            }
            if let Some(on) = o.render {
                let fx = flag_x(1) + 6.0;
                paint.circle((fx, cy), 5.5, Some(theme.input), Some(theme::border(theme)));
                if on {
                    paint.circle((fx, cy), 3.0, Some(if o.lead { theme.accent } else { muted }), None);
                }
            }
            let edge = flag_x(0) - 8.0;
            let dw = paint.text_width(None, &o.detail);
            let name_x = ox + paint.cap_width(&o.letter) + 8.0;
            let name = ell(paint, edge - name_x, &o.name);
            paint.text_c((name_x, ty), theme.foreground, &name);
            if wide && name_x + paint.text_width(None, &name) + 8.0 < edge - dw {
                paint.text_c((edge - dw, ty), muted, &o.detail);
            }
        }
        Row::RootRow { detail, folded, active, .. } => {
            shade(paint, c, k, ry, rhh, false, *active);
            // the chevron opens or folds the objects; wide only, where the sheet draws it
            if wide {
                paint.chevron((x + 15.0, ry + (rh / 2.0)), if *folded { Dir::Right } else { Dir::Down }, theme.foreground);
            }
            let ox = x + if wide { 26.0 } else { 12.0 };
            paint.cap((ox, kit::cap_y_paint(paint, ry, rh)), Some(muted), "R");
            let name_x = ox + paint.cap_width("R") + 8.0;
            paint.text_c((name_x, ty), theme.foreground, "root");
            if wide {
                let edge = flag_x(0) - 8.0;
                let dw = paint.text_width(None, detail);
                paint.text_c((edge - dw, ty), muted, detail);
            }
        }
        Row::LayoutRow { index, label, active } => {
            shade(paint, c, k, ry, rhh, false, false);
            // its key as the kit's key text, then the name 8 points on
            let key = index.to_string();
            paint.text_s((x + 12.0, kit::cap_y_paint(paint, ry, rh)), c.small, theme::ink_3(theme), &key);
            let name_x = x + 12.0 + paint.text_width(Some(c.small), &key) + 8.0;
            let cut = ell(paint, x + w - 38.0 - name_x, label);
            paint.text_c((name_x, ty), theme.foreground, &cut);
            if *active {
                paint.fill(x + w - 18.0, ry + (rh / 2.0) - 3.0, 6.0, 6.0, theme.accent);
            }
        }
        Row::InputRow { name, .. } => {
            let cut = ell(paint, c.label_w, name);
            paint.text_c((x + 12.0, ty), muted, &cut);
        }
        Row::GraphRow { label, context, detail, active, chip, dim, used, graph } => {
            shade(paint, c, k, ry, rhh, false, *active);
            if wide && *active && c.has_scope {
                paint.chevron((x + 5.0, ry + (rh / 2.0)), if c.opened.contains(graph) { Dir::Down } else { Dir::Right }, ink_3);
            }
            match chip {
                Some(col) => {
                    paint.fill(x + 12.0, ry + (rh / 2.0) - 6.0, 12.0, 12.0, *col);
                    paint.stroke(x + 12.5, ry + (rh / 2.0) - 5.5, 11.0, 11.0, theme::edge(theme));
                }
                None if graph.starts_with("def:") => {
                    // a function: the diamond of its port, a 7-point square turned 45 degrees (10 across), in half-point rows
                    let (cx, cy) = (x + 16.0, ry + (rh / 2.0));
                    for kk in 0..20 {
                        let off = ((kk as f32 + 0.5) * 0.5) - 5.0;
                        let half = 5.0 - off.abs();
                        paint.fill(cx - half, cy + off - 0.25, 2.0 * half, 0.5, rays_graph::scope_paint::ports(theme).fn_);
                    }
                }
                None => square(paint, c, ry, x + 12.0, context_color(theme, *context)),
            }
            let from = x + if chip.is_some() {
                32.0
            } else if graph.starts_with("def:") {
                27.0
            } else {
                28.0
            };
            let color = if *dim { ink_3 } else { theme.foreground };
            match used {
                Some(n) => {
                    let count = n.to_string();
                    // a count is ink on the wide sheet, ink-2 on the narrow one
                    let cw = paint.text_width(None, &count);
                    paint.text_c((x + w - 12.0 - cw, ty), if wide { color } else if *dim { ink_3 } else { muted }, &count);
                    let edge = x + w - 40.0;
                    let dw = paint.text_width(None, detail);
                    let name = ell(paint, edge - from, label);
                    paint.text_c((from, ty), color, &name);
                    if wide && from + paint.text_width(None, &name) + 8.0 < edge - dw {
                        paint.text_c((edge - dw, ty), if *dim { ink_3 } else { muted }, detail);
                    }
                }
                None => labelled(paint, c, ty, color, from, label, if wide { detail } else { "" }),
            }
        }
        Row::NodeRow { depth, label, detail, zone, ty: node_ty, result, selected, .. } => {
            shade(paint, c, k, ry, rhh, *selected, false);
            let nx = x + 28.0 + (12.0 * *depth as f32);
            match zone {
                Some(glyph) => {
                    let from = tag(paint, nx, glyph);
                    labelled(paint, c, ty, theme.foreground, from, label, detail);
                }
                None => {
                    square(paint, c, ry, nx, type_color(theme, node_ty));
                    labelled(paint, c, ty, if *result { muted } else { theme.foreground }, nx + 16.0, label, detail);
                }
            }
        }
        Row::MacroRow(name, uses) => {
            shade(paint, c, k, ry, rhh, false, false);
            paint.text_c((x + 12.0, ty), muted, "\u{3bb}");
            labelled(paint, c, ty, theme.foreground, x + 28.0, name, &plural(*uses, "use"));
        }
        Row::LinkRow { label, graph, .. } => {
            shade(paint, c, k, ry, rhh, false, false);
            paint.text_c((x + 12.0, ty), muted, label);
            paint.text_c((x + 20.0 + c.label_w, ty), theme.foreground, graph);
        }
        Row::Empty(s) => paint.text_c((x + 12.0, ty), ink_3, s),
    }
}

/// the list's rows, painted in the content child so the list's clip holds them
fn paint_rows(ui: &mut Ui, content: &UiBox, c: PaintCtx) {
    let c = Rc::new(c);
    let _ = ui;
    let c2 = c.clone();
    ui.draw(content, move |paint, _| {
        let c = &c2;
        paint.fill(c.x, c.top, c.w, c.body, c.theme.panel);
        for (k, row) in c.rows.iter().enumerate() {
            let ry = c.top + c.tops[k] - c.scroll;
            let rhh = c.tops[k + 1] - c.tops[k];
            if ry + rhh > c.top && ry < c.top + c.body {
                paint_row(paint, c, k, row);
            }
        }
    });
}

/// the wide sheet's bar of its keys and the graph count
fn paint_footer(ui: &mut Ui, theme: Theme, muted: Color, rh: f32, bounds: (f32, f32, f32, f32), footer: f32, graphs: usize) {
    if footer <= 0.0 {
        return;
    }
    let (x, y, w, h) = bounds;
    let boxed = ui.box_("navigator-footer", BoxOpts::default().w(Size::Px(w)).h(Size::Px(footer)).at((x, y + h - footer)));
    let small = kit::cap_size(ui);
    ui.draw(&boxed, move |paint, Rect { x: fx, y: fy, w: fw, h: _ }| {
        paint.fill(fx, fy, fw, footer, theme.panel);
        paint.fill(fx, fy, fw, 1.0, theme::edge(&theme));
        let mut tx = fx + 12.0;
        // each key in ink-3 at the label size, what it does in ink-2, 8 apart
        for (key, what) in [("s", "filter"), ("i", "enter"), ("/ j", "jump")] {
            paint.text_s((tx, kit::cap_y_paint(paint, fy + 1.0, rh)), small, theme::ink_3(&theme), key);
            tx += paint.text_width(Some(small), key) + 8.0;
            paint.text_c((tx, paint.text_top(None, fy + 1.0, rh)), muted, what);
            tx += paint.text_width(None, what) + 8.0;
        }
        let count = plural(graphs as i64, "graph");
        let cw = paint.cap_width(&count);
        paint.cap((fx + fw - 12.0 - cw, kit::cap_y_paint(paint, fy + 1.0, rh)), None, &count);
    });
}

fn paint_thumb(ui: &mut Ui, theme: Theme, total: f32, rh: f32, boxed: &UiBox) {
    // the scroll thumb: 4 points wide, 2 from the edge, line-3, over a track 6 points in from both ends
    let scroll = ui.scroll_position(boxed);
    ui.draw_over(boxed, move |paint, Rect { x: bx, y: by, w: bw, h: bh }| {
        let content = total + rh;
        if content > bh {
            let track = (bh - 12.0).max(1.0);
            let thumb = track.min((track * bh / content).max(20.0));
            let y = by + 6.0 + (track - thumb) * (scroll / (content - bh).max(1.0)).max(0.0).min(1.0);
            paint.fill(bx + bw - 6.0, y, 4.0, thumb, theme::border(&theme));
        }
    });
}

/// An input's value as its slider shows it.
fn show(integer: bool, v: f64) -> String {
    if integer { format!("{}", v.round() as i64) } else { cfmt::g(v, 3) }
}

/// A slider's scrub from the value at the press, like a card's field: 1 per 6 points for an integer, a
/// hundredth of the magnitude per point for a float, Shift is finer.
fn scrub(integer: bool, origin: &str, dx: f32, shift: bool) -> String {
    match origin.parse::<f64>().ok() {
        None => origin.to_owned(),
        Some(o) if integer => show(integer, o + (dx as f64 / if shift { 24.0 } else { 6.0 }).round()),
        Some(o) => show(integer, o + (dx as f64 * 0.01 * o.abs().max(1.0) * if shift { 0.1 } else { 1.0 })),
    }
}

/// A slider's field label (its key under the navigator's view).
fn slider_label(graph: &str, name: &str) -> String {
    format!("navigator-input-{}-{}", graph, name)
}

#[allow(clippy::too_many_arguments)]
fn input_sliders(
    ui: &mut Ui,
    emit: &mut Vec<Intent>,
    rows: &[Row],
    tops: &[f32],
    top: f32,
    scroll: f32,
    rh: f32,
    body: f32,
    x: f32,
    w: f32,
    label_w: f32,
    typed_row: Option<usize>,
) {
    // the sliders of the inputs, over their rows: a drag scrubs and Option-click types, as every numeric field of
    // the kit; a double click on the row's name types too, as an inspector row's label does (the field itself keeps
    // its drag)
    for (k, row) in rows.iter().enumerate() {
        if let Row::InputRow { graph, name, integer, value } = row {
            let ry = top + tops[k] - scroll;
            if ry >= top && ry + rh <= top + body {
                let integer = *integer;
                let scrub: Rc<dyn Fn(&str, f32, bool) -> String> = Rc::new(move |origin, dx, shift| scrub(integer, origin, dx, shift));
                let text = show(integer, *value);
                let o = ValueFieldOpts { scrub: Some(scrub), edit: typed_row == Some(k), kept_drag: true, ..ValueFieldOpts::default() };
                let valid = move |t: &str| match (t).parse::<f64>().ok() {
                    Some(v) => v.is_finite() && (!integer || v.fract() == 0.0),
                    None => false,
                };
                let (changed, _) = ui.value_field((x + 20.0 + label_w, ry + 2.0), w - 32.0 - label_w, 20.0, &o, &valid, &slider_label(graph, name), &text);
                if changed != text
                    && let Some(v) = (&changed).parse::<f64>().ok()
                {
                    emit.push(Intent::SetDefault { graph: graph.clone(), input: name.clone(), value: v, integer });
                }
            }
        }
    }
}

/// What a navigator's last build showed, by its pane's key (the caller's root): its state and rows,
/// their revision, and the keys of the view and of its list box.
struct Shown {
    pane: i64,
    state: State,
    rows: Vec<Row>,
    rev: u64,
    view: i64,
    list: i64,
}

/// The input slider a press holds (`kept_drag`), as its press built it: its pane, its field's key, its input,
/// the text its scrub starts from, the last text it showed, and whether the last frame of its drag
/// was shown by a patch.
struct Held {
    pane: i64,
    field: i64,
    graph: String,
    input: String,
    integer: bool,
    origin: String,
    last: String,
    patched: bool,
}

thread_local! {
    // ponytail: one entry per navigator pane ever shown, never pruned; prune with the pane when panes churn
    static SHOWN: RefCell<Vec<Shown>> = const { RefCell::new(Vec::new()) };
    // one held slider per thread (the pointer holds one field), found by its field's key
    static HELD: RefCell<Option<Held>> = const { RefCell::new(None) };
}

/// The row of the list at window height `py` (`tops` from the list's top, `scroll` into it).
fn row_of(tops: &[f32], top: f32, scroll: f32, py: f32) -> Option<usize> {
    let off = py - top + scroll;
    (py >= top && off < tops[tops.len() - 1]).then(|| tops.partition_point(|&t| t <= off).saturating_sub(1))
}

/// The held slider's scrub as its field reads it this frame (`dx` from the press).
fn held_text(h: &Held, ui: &Ui, field: &UiBox, dx: f32) -> String {
    scrub(h.integer, &h.origin, dx, ui.press_keys(field).contains(&Key::Shift.code()))
}

/// Draw the panel in `bounds` (scrolling, searching, sliders) of the pane keyed `pane`, in a cached
/// view: it builds when what it shows changes (its state and rows, the scroll, the hovered row, the
/// keys it reads) or input reaches it. A held input slider (`kept_drag`) is followed between builds:
/// each frame of its drag patches the field and sets the input's default, the view kept at the
/// revision it was built with; its release builds it as the document is then.
pub fn view(state: &State, ui: &mut Ui, bounds: (i64, i64, i64, i64), pa: &Params, pane: i64) -> (State, Vec<Intent>) {
    use std::hash::Hash;
    let rows = rows(bounds.2 >= 300, state, pa);
    let dragged = HELD.with(|h| h.borrow().as_ref().filter(|h| h.pane == pane).and_then(|h| ui.box_of_key(h.field).filter(|b| ui.field_drag(b).is_some()).map(|_| h.patched)));
    let (rev, list) = SHOWN.with(|s| {
        let mut s = s.borrow_mut();
        let k = s.iter().position(|e| e.pane == pane).unwrap_or_else(|| {
            s.push(Shown { pane, state: state.clone(), rows: rows.clone(), rev: 0, view: 0, list: 0 });
            s.len() - 1
        });
        let e = &mut s[k];
        // while a held slider's drag is shown by patches its own edits change no revision
        if dragged != Some(true) && (e.state != *state || e.rows != rows) {
            (e.state, e.rows, e.rev) = (state.clone(), rows.clone(), e.rev + 1);
        }
        (e.rev, ui.box_of_key(e.list))
    });
    let scroll = list.map_or(0.0, |b| ui.scroll_position(&b));
    let rh = ui.row_height() as f32;
    let hovered = list.map(|b| ui.signal(&b)).filter(|sg| sg.hovered).and_then(|sg| row_of(&tops(rh, &rows), bounds.1 as f32 + SEARCH_HEIGHT, scroll, sg.pointer.1));
    let keys = [Key::F2, Key::Delete, Key::Enter].map(|k| ui.key_pressed(k));
    let revision = kit::revision(&|h| (rev, bounds, pa.scope.is_some(), pa.workspace.graphs.len(), scroll.to_bits(), hovered, keys).hash(h));
    let (mut out, mut verified) = (None, false);
    let v = kit::cached(ui, "navigator-view", revision, &mut |ui| {
        verified = ui.verifying();
        out = Some(build(state, ui, bounds, pa, rows.clone()));
    });
    let (state, mut intents) = match out {
        Some((next, intents, list)) if !verified => {
            SHOWN.with(|s| s.borrow_mut().iter_mut().filter(|e| e.pane == pane).for_each(|e| (e.view, e.list) = (v.box_key, list)));
            // a press on a slider holds it; its release (or a build of it not held) lets it go
            HELD.with(|h| {
                let mut h = h.borrow_mut();
                for row in &rows {
                    let Row::InputRow { graph, name, integer, value } = row else { continue };
                    let field = pxui::ui_core::key_of(v.box_key, &slider_label(graph, name));
                    if ui.box_of_key(field).is_some_and(|b| ui.signal(&b).pressed) {
                        let origin = show(*integer, *value);
                        *h = Some(Held { pane, field, graph: graph.clone(), input: name.clone(), integer: *integer, last: origin.clone(), origin, patched: false });
                    }
                }
                let signal = h.as_ref().and_then(|held| ui.box_of_key(held.field).map(|b| (b, ui.signal(&b))));
                match (h.as_mut(), signal) {
                    (Some(held), Some((b, sg))) if held.pane == pane && sg.held => (held.last, held.patched) = (held_text(held, ui, &b, sg.pointer.0 - sg.press_point.0), false),
                    (Some(held), _) if held.pane == pane => *h = None,
                    _ => {}
                }
            });
            (next, intents)
        }
        // a verifying run of the kept view wrote the drag's default itself
        Some((_, intents, _)) => (state.clone(), intents),
        None => (state.clone(), Vec::new()),
    };
    // a frame of the held slider's drag on the kept view: the field patched (a patch that cannot show
    // it builds the view next frame) and the default set
    let followed = HELD.with(|h| {
        let mut h = h.borrow_mut();
        let h = h.as_mut().filter(|h| h.pane == pane)?;
        let field = ui.box_of_key(h.field)?;
        let (_, dx) = ui.field_drag(&field)?;
        let text = held_text(h, ui, &field, dx);
        if text == h.last {
            h.patched = true;
            return None;
        }
        h.patched = ui.patch_field(&field, &ValueFieldOpts::default(), &text);
        h.last = text;
        h.last.parse::<f64>().ok().filter(|v| v.is_finite()).map(|value| Intent::SetDefault { graph: h.graph.clone(), input: h.input.clone(), value, integer: h.integer })
    });
    intents.extend(followed.filter(|_| !verified));
    (state, intents)
}

fn build(state: &State, ui: &mut Ui, bounds: (i64, i64, i64, i64), pa: &Params, rows: Vec<Row>) -> (State, Vec<Intent>, i64) {
    let theme = ui.tokens();
    let rh = ui.row_height() as f32;
    let (x, y, w, h) = bounds;
    let (x, y, w, h) = (x as f32, y as f32, w as f32, h as f32);
    let muted = theme::muted(&theme);
    // the search field above the list
    let (was_typing, query_in) = (state.typing, state.query.clone());
    // the wide sheet's field leaves room for the add button; the narrow one has no button
    let field_w = if w >= 300.0 { w - 40.0 } else { w - 16.0 };
    let mut state = state.clone();
    let mut renamed: Vec<Intent> = Vec::new();
    // F2 puts the graph's name in this field instead of the search
    match state.rename.clone() {
        Some((graph, seen)) => {
            let o = ValueFieldOpts { left: true, edit: !seen, ..ValueFieldOpts::default() };
            let (text, open) = ui.value_field((x + 8.0, y + 6.0), field_w, 20.0, &o, &|t| flow_syntax::symbol::valid_name(t), "navigator-rename", &graph);
            if text != graph {
                state.rename = None;
                renamed.push(Intent::Rename { graph, to: text });
            } else if open {
                state.rename = Some((graph, true));
            } else if seen {
                state.rename = None;
            }
        }
        None => {
            let o = ValueFieldOpts { left: true, ..ValueFieldOpts::default() };
            let (query, typing) = ui.value_field((x + 8.0, y + 6.0), field_w, 20.0, &o, &|_| true, "navigator-search", &state.query);
            state.query = query;
            state.typing = typing;
        }
    }
    let typing = state.typing;
    if state.query.is_empty() && !typing && state.rename.is_none() {
        let boxed = ui.box_("navigator-placeholder", BoxOpts::default().flags(CLIP).w(Size::Px(field_w - 4.0)).h(Size::Px(20.0)).at((x + 10.0, y + 6.0)));
        let wide_field = w >= 300.0;
        ui.draw(&boxed, move |paint, Rect { x: px, y: py, w: pw, h }| {
            let ty = paint.text_top(None, py, h);
            paint.text_c((px, ty), muted, "/");
            let cut = ell(paint, pw - 12.5, if wide_field { "go to node, graph, function" } else { "go to" });
            paint.text_c((px + 12.5, ty), theme::ink_3(&theme), &cut);
        });
    }
    // (the search field's text moves the rows the frame it changes)
    let rows = if state.query == query_in { rows } else { self::rows(w >= 300.0, &state, pa) };
    let tops = tops(rh, &rows);
    let total = tops[rows.len()];
    let top = y + SEARCH_HEIGHT;
    // the wide sheet closes with a hairline and a 24-point bar of its keys and the graph count
    let footer = if w >= 300.0 { rh + 1.0 } else { 0.0 };
    let body = rh.max(h - SEARCH_HEIGHT - footer);
    let boxed = ui.box_(
        "navigator-list",
        BoxOpts::default().flags(CLICKABLE | SCROLL | CLIP | BLOCKING).w(Size::Px(w)).h(Size::Px(body)).at((x, top)).scroll_step(rh),
    );
    let signal = ui.signal(&boxed);
    // the painted position, elastic edge movement included: a click lands on the row it sees
    let scroll = ui.scroll_position(&boxed);
    // the rows paint in the child so the list's clip holds them: a box's own painter is not clipped
    let mut content: Option<UiBox> = None;
    ui.within(&boxed, &mut |ui: &mut Ui| content = Some(ui.box_("navigator-content", BoxOpts::default().w(Size::Px(w)).h(Size::Px(total + rh)))));
    let content = content.expect("navigator content");
    let row_at = |(_, py): (f32, f32)| row_of(&tops, top, scroll, py);
    let mut intents: Vec<Intent> = renamed;
    // the two sheets of the kit: a column under 300 points (the workspace) drops the details and keeps 56 points for an
    // input's label, a wide one (the outline sheet) shows them and keeps 112
    let wide = w >= 300.0;
    let label_w = if wide { 112.0 } else { 56.0 };
    // add an object, a graph, a node: the add menu
    if w >= 300.0 && kit::button(ui, "navigator-add", (x + w - 28.0, y + 6.0), 20.0, &ButtonOpts { centered: true, ..ButtonOpts::default() }, "+") {
        intents.push(Intent::Add);
    }
    let mut fold = false;
    let mut toggle: Option<String> = None;
    let mut begin_rename: Option<(String, bool)> = None;
    // a material or a SOP graph is a source: pressed and moved 4 points it is carried, as the Flow value that reads it
    if signal.held {
        match row_at(signal.press_point).map(|k| &rows[k]) {
            Some(Row::RootRow { graph, .. }) => ui.carry(Some(&boxed), "scene", &format!("(ref {})", graph)),
            Some(Row::GraphRow { graph, context: Some(context), .. }) => ui.carry(Some(&boxed), context::name(*context), &format!("(ref {})", graph)),
            _ => {}
        }
    }
    let put = matches!(ui.drop_target(&boxed), Some(Drop::Dropped(_)));
    if signal.clicked && !put {
        match row_at(signal.release_point).map(|k| &rows[k]) {
            Some(Row::GraphRow { graph, active, .. }) => {
                // the chevron of the open graph's row folds its node rows; the rest of the row opens it
                if wide && *active && pa.scope.is_some() && signal.release_point.0 < x + 10.0 {
                    toggle = Some(graph.clone());
                } else {
                    intents.push(Intent::Open { graph: graph.clone(), node: None });
                }
            }
            Some(Row::RootRow { graph, .. }) => {
                // the chevron folds the scene's objects; the rest of the row opens the scene graph
                if wide && signal.release_point.0 < x + 24.0 {
                    fold = true;
                } else {
                    intents.push(Intent::Open { graph: graph.clone(), node: None });
                }
            }
            Some(Row::NodeRow { graph, path, .. }) => intents.push(Intent::Open { graph: graph.clone(), node: Some(path.clone()) }),
            Some(Row::LinkRow { graph, .. }) => intents.push(Intent::Open { graph: graph.clone(), node: None }),
            Some(Row::MacroRow(name, _)) => intents.push(Intent::Macro(name.clone())),
            Some(Row::ObjectRow(o)) if matches!(&o.home, Some(path) if !path.is_empty()) => {
                // a press on a flag toggles it in the text; anywhere else on the row opens the object
                let path = o.home.clone().unwrap();
                let px = signal.release_point.0;
                let flag = |column: i64| {
                    let fx = x + w - 12.0 - 12.0 - ((1 - column) as f32 * 20.0);
                    px >= fx - 4.0 && px < fx + 16.0
                };
                match (o.visible, o.render) {
                    (Some(on), _) if flag(0) && !o.inert => intents.push(Intent::Flag { node: path, name: "visible".to_owned(), value: !on }),
                    (_, Some(on)) if flag(1) && !o.inert => intents.push(Intent::Flag { node: path, name: "render".to_owned(), value: !on }),
                    _ => intents.push(Intent::Open { graph: path[0].to_string(), node: Some(path) }),
                }
            }
            Some(Row::LayoutRow { index, .. }) => intents.push(Intent::Layout(*index)),
            _ => {}
        }
    }
    if (typing || was_typing) && ui.key_pressed(Key::Enter) {
        let first = rows.iter().find_map(|row| match row {
            Row::GraphRow { graph, .. } => Some(Intent::Open { graph: graph.clone(), node: None }),
            Row::NodeRow { graph, path, .. } => Some(Intent::Open { graph: graph.clone(), node: Some(path.clone()) }),
            _ => None,
        });
        if let Some(intent) = first {
            intents.push(intent);
        }
    }
    let hovered = if signal.hovered { row_at(signal.pointer) } else { None };
    // F2 renames and Delete removes the graph under the pointer (a refused removal says who reads it)
    if let Some(Row::GraphRow { graph, .. } | Row::RootRow { graph, .. }) = hovered.map(|k| &rows[k])
        && !graph.starts_with("def:")
        && !typing
        && state.rename.is_none()
    {
        if ui.key_pressed(Key::F2) {
            begin_rename = Some((graph.clone(), false));
        } else if ui.key_pressed(Key::Delete) {
            intents.push(Intent::Remove(graph.clone()));
        }
    }
    // a right-click opens the context menu: New graph (a context each), and on a graph row Rename and Delete.
    // The row's own graph is kept with the menu; a function row has neither.
    let mut context = state.context.clone();
    if context_clicked(&signal) {
        let (cx, cy) = signal.release_point;
        let graph = match row_at(signal.release_point).map(|k| &rows[k]) {
            Some(Row::GraphRow { graph, .. } | Row::RootRow { graph, .. }) => Some(graph.clone()),
            _ => None,
        };
        context = Some((cx, cy, graph));
    }
    if let Some((cx, cy, graph)) = context.clone() {
        let editable = match &graph {
            Some(g) => !g.starts_with("def:"),
            None => false,
        };
        let items = vec![("New graph".to_owned(), true), ("Rename".to_owned(), editable), ("Delete".to_owned(), editable)];
        let contexts = new_graph_contexts();
        let submenu = Submenu { row: 0, rows: contexts.iter().map(|c| (c.clone(), true)).collect(), keys: Vec::new(), current: None };
        let o = MenuOpts { danger: vec![2], submenus: vec![submenu], ..MenuOpts::default() };
        match ui.context_menu((cx, cy), &o, "navigator-context", &items) {
            MenuResult::Open => {}
            MenuResult::Dismiss => context = None,
            MenuResult::Pick(n) => {
                context = None;
                match (n, graph) {
                    (1, Some(g)) => begin_rename = Some((g, false)),
                    (2, Some(g)) => intents.push(Intent::Remove(g)),
                    (n, _) if n >= 3 => intents.push(Intent::NewGraph(contexts[(n - 3) as usize].clone())),
                    _ => {}
                }
            }
        }
    }
    let ctx = PaintCtx {
        rows: rows.clone(),
        tops: tops.clone(),
        theme,
        muted,
        hovered,
        rh,
        body,
        w,
        top,
        x,
        scroll,
        wide,
        label_w,
        has_scope: pa.scope.is_some(),
        opened: state.opened.clone(),
        small: kit::cap_size(ui),
    };
    paint_rows(ui, &content, ctx);
    paint_footer(ui, theme, muted, rh, (x, y, w, h), footer, pa.workspace.graphs.len());
    paint_thumb(ui, theme, total, rh, &boxed);
    let typed_row = if signal.double_clicked { row_at(signal.press_point) } else { None };
    input_sliders(ui, &mut intents, &rows, &tops, top, scroll, rh, body, x, w, label_w, typed_row);
    if let Some(graph) = toggle {
        if state.opened.contains(&graph) {
            state.opened.retain(|g| *g != graph);
        } else {
            state.opened.insert(0, graph);
        }
    }
    state.context = context;
    if begin_rename.is_some() {
        state.rename = begin_rename;
    }
    if fold {
        state.scene_closed = !state.scene_closed;
    }
    (state, intents, boxed.box_key)
}
