//! Port of lib/editor_document/document.ml: everything the user edits and saves, as one immutable
//! value: the one thing `Editor_core.History` snapshots. Selection, hover and the open level are
//! view state and stay outside. The document is a workspace (its text is the authored truth); the
//! scene, the networks and the shell are what lowering derives from it.
use crate::kind_graph::{self as kg};
use crate::layout_by_path::Path;
use crate::objects::root;
use crate::settings;
use crate::workspace_doc;
use editor_core::panels;
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{arg_of, arg_text, ArgKey};
use flow_sop::lower as l;
use flow_sop::network as net;
use flow_sop::port;
use flow_syntax::context::{self, Context};
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::value::Value as E;
use param::Value as PV;
use rays::camera as rcamera;
use rays_diagnostic::Diagnostic;
use sop::edit_graph;
use std::collections::BTreeMap;
use std::sync::{Arc, OnceLock};

type S = Syntax;

pub type IntMap<V> = BTreeMap<i64, V>;

/// A geometry object's SOP network with its display node.
#[derive(Clone)]
pub struct Network {
    pub context: Context,
    pub graph: net::Network,
    /// None when no node has a geometry output.
    pub displayed: Option<i64>,
}

pub fn of_geometry(context: Context, graph: &edit_graph::T, displayed: Option<i64>) -> Network {
    Network { context, graph: net::of_geometry(graph), displayed }
}

/// Where the text of a scene object, a World layer or the settings lives: a binding of its graph,
/// an argument of another home (an inline call, written in place), a copy of a loop's template, or
/// nowhere an edit can reach. The copies of a loop are instances of ONE template: `Copy` names the
/// loop (its binding, or the argument holding it), the template's path inside the loop body
/// (`["@result"]` or a binding of the body) and which copy this is.
#[derive(Clone, Debug, PartialEq)]
pub enum Home {
    BoundAt(Vec<String>),
    InlineIn(Box<Home>, ArgKey),
    Copy { loop_: Box<Home>, rel: Vec<String>, index: i64 },
    Looped,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Homes {
    /// Scene object id.
    pub objects: Vec<(i64, Home)>,
    /// The `scene/world` call.
    pub world: Option<Home>,
    /// World layer id.
    pub layers: Vec<(i64, Home)>,
    /// The `settings/config` call.
    pub settings: Option<Home>,
    /// The `scene/root` call; none: the scene is a part with a default root.
    pub root: Option<Home>,
    /// The world graph the `scene/world` call references.
    pub world_graph: Option<String>,
}

pub fn no_homes() -> Homes {
    Homes::default()
}

/// Where a panel of the shell tree came from in the editor graph.
#[derive(Clone, Debug, PartialEq)]
pub enum Origin {
    /// Its binding.
    Bound(String),
    /// The tile call a loop makes panels for, and the loop's argument in it.
    Loop(Home, ArgKey),
    /// Written in place: an argument of this call.
    Inline(Home, ArgKey),
}

#[derive(Clone)]
pub struct PreviewSource {
    pub editor_graph: String,
    pub panel: Option<Origin>,
    pub scene_ref: Option<S>,
    pub instance: E,
}

/// The World a viewport over another scene instance shows: the object holding its fields and its
/// layers (they are not in the scene).
#[derive(Clone)]
pub struct ViewWorld {
    pub node: kg::Node,
    pub layers: kg::T,
}

/// How a viewport over another scene instance renders: the settings of its `scene/root` and the
/// view of the camera object that root's `:camera` slot names (none: the root names no camera).
#[derive(Clone)]
pub struct ViewRoot {
    pub params: root::Parameters,
    pub camera: Option<rcamera::t>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Switch {
    pub layouts: Vec<panels::t>,
    pub active: i64,
}

/// What the panels' keywords say the editor opens with: read once, not kept in step.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Start {
    /// The first leaf with `:focus true`.
    pub focus: Option<panels::Path>,
    /// The viewports with `:look_through true`, by key.
    pub looking: Vec<String>,
    /// Each graph leaf's `:view`.
    pub graph_views: Vec<(panels::Path, String)>,
    /// Each lisp leaf's `:tab`.
    pub tabs: Vec<(panels::Path, String)>,
}

/// The lowered editor graph: the shell tree, the origin of each named or looped panel (any other
/// panel is inline), the graph each `(ui/graph "name")` leaf shows and, for each viewport over a
/// scene other than the default one, the scene objects of that instance.
#[derive(Clone)]
pub struct Shell {
    pub tree: panels::t,
    pub origins: Vec<(panels::Path, Origin)>,
    pub named: Vec<(panels::Path, String)>,
    /// The bindings several leaves are made from: those leaves are keyed by place.
    pub repeated: Vec<String>,
    /// An inspector, list or lisp leaf with `:of`: the graph leaf it shows.
    pub follows: Vec<(panels::Path, panels::Path)>,
    pub start: Start,
    pub views: Vec<(String, Vec<i64>)>,
    pub canvases: Vec<(String, E)>,
    pub preview_sources: Vec<(String, PreviewSource)>,
    /// The first `ui/switch`: every layout it holds, and the active one.
    pub switch: Option<Switch>,
}

/// A value computed on first use and kept (OCaml `Lazy.t`), for the window's precision.
#[derive(Clone)]
pub struct LazyBool {
    cell: Arc<OnceLock<bool>>,
    init: Arc<dyn Fn() -> bool + Send + Sync>,
}

impl LazyBool {
    pub fn new(init: Arc<dyn Fn() -> bool + Send + Sync>) -> LazyBool {
        LazyBool { cell: Arc::new(OnceLock::new()), init }
    }
    pub fn forced(value: bool) -> LazyBool {
        LazyBool::new(Arc::new(move || value))
    }
    pub fn force(&self) -> bool {
        *self.cell.get_or_init(|| (self.init)())
    }
}

/// The factories' kinds, built when an edit first asks.
#[derive(Clone)]
pub struct LazyCatalog {
    cell: Arc<OnceLock<Result<Catalog, Diagnostic>>>,
    init: Arc<dyn Fn() -> Result<Catalog, Diagnostic> + Send + Sync>,
}

impl LazyCatalog {
    pub fn new(init: Arc<dyn Fn() -> Result<Catalog, Diagnostic> + Send + Sync>) -> LazyCatalog {
        LazyCatalog { cell: Arc::new(OnceLock::new()), init }
    }
    pub fn force(&self) -> &Result<Catalog, Diagnostic> {
        self.cell.get_or_init(|| (self.init)())
    }
}

/// The authored document, its lowering and the window's precision, read once with the lowering.
#[derive(Clone)]
pub struct Workspace {
    pub ws: workspace_doc::T,
    pub lowered: l::T,
    pub fast: LazyBool,
    pub catalog: LazyCatalog,
}

#[derive(Clone)]
pub struct T {
    /// Objects: the input is the parent.
    pub scene: kg::T,
    /// By geometry object id: its SOP network.
    pub networks: IntMap<Network>,
    /// By World id: its layers, each on the one below.
    pub layers: IntMap<kg::T>,
    /// A camera object.
    pub active_camera: Option<i64>,
    /// How the scene renders: the root's settings, else the defaults.
    pub root: root::Parameters,
    pub settings: settings::T,
    /// The `editor` graph of the workspace, if it has one.
    pub shell: Option<Shell>,
    /// A viewport over a scene instance that names another World than the document's: its World
    /// (none: that instance has none); a viewport not listed shows the document's.
    pub view_worlds: Vec<(String, Option<ViewWorld>)>,
    /// A viewport over a scene instance with a `scene/root` of its own.
    pub view_roots: Vec<(String, ViewRoot)>,
    /// Where each derived object is written in the text.
    pub homes: Homes,
    pub scene_drives: IntMap<(String, Vec<(port::Parameter, E)>)>,
    /// Residual light fields, resolved for composition; never written into history.
    pub scene_textures: IntMap<E>,
    /// The authored document and its lowering.
    pub workspace: Workspace,
}

/// A document's text as of a save or an opening: nothing evaluated.
#[derive(Clone)]
pub struct Text {
    pub text_ws: workspace_doc::T,
    pub text_settings: settings::T,
}

pub fn text(value: &T) -> Text {
    Text { text_ws: value.workspace.ws.clone(), text_settings: value.settings.clone() }
}

/// An undo entry: the text and what the text does not say (what the host made, the ids a lowering
/// gave). No evaluated value, lowering or GPU handle; `Lowering::restore` makes the document again.
#[derive(Clone)]
pub struct Snapshot {
    pub saved_ws: workspace_doc::T,
    pub saved_scene: kg::T,
    pub saved_layers: IntMap<kg::T>,
    pub saved_camera: Option<i64>,
    pub saved_homes: Homes,
    pub saved_settings: settings::T,
    pub saved_ids: l::PathMap,
    pub saved_sites: Vec<Vec<String>>,
}

pub fn snapshot(value: &T) -> Snapshot {
    Snapshot {
        saved_ws: value.workspace.ws.clone(),
        saved_scene: value.scene.clone(),
        saved_layers: value.layers.clone(),
        saved_camera: value.active_camera,
        saved_homes: value.homes.clone(),
        saved_settings: value.settings.clone(),
        saved_ids: value.workspace.lowered.compiled_ids.clone(),
        saved_sites: value.workspace.lowered.sites.clone(),
    }
}

pub fn snapshot_bytes(saved: &Snapshot) -> usize {
    // the checked workspace and the ids hold about as much again
    let mut all = saved.saved_ws.source.clone();
    all.extend(saved.saved_ws.extra.iter().cloned());
    2 * s::memory_bytes(&all)
}

/// What the list shows: the scene's objects, or inside an object its SOP network or the World's layers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Scene,
    Inside(i64),
}

/// A geometry object's SOP network.
pub fn network(value: &T, id: i64) -> Option<&Network> {
    value.networks.get(&id)
}

/// The World's layers.
pub fn layers(value: &T, id: i64) -> Option<&kg::T> {
    value.layers.get(&id)
}

pub fn with_layers(value: &T, id: i64, layers: kg::T) -> T {
    let mut next = value.clone();
    next.layers.insert(id, layers);
    next
}

/// The objects or layers of a level; none inside a geometry object.
pub fn kinds(value: &T, level: Level) -> Option<kg::T> {
    match level {
        Level::Scene => Some(value.scene.clone()),
        Level::Inside(id) => layers(value, id).cloned(),
    }
}

pub fn with_kinds(value: &T, level: Level, graph: kg::T) -> T {
    match level {
        Level::Scene => T { scene: graph, ..value.clone() },
        Level::Inside(id) => with_layers(value, id, graph),
    }
}

/// Resolve navigation after load, undo, or object removal. The scene is always a valid level,
/// including an empty scene.
pub fn resolve_level(value: &T, preferred: Level) -> Level {
    match preferred {
        Level::Inside(id) if (value.networks.contains_key(&id) || value.layers.contains_key(&id)) && kg::find(&value.scene, id).is_some() => Level::Inside(id),
        _ => Level::Scene,
    }
}

fn finite_fields(fields: &[param::FieldView]) -> bool {
    fields.iter().all(|f| match &f.current {
        PV::Float(x) => x.is_finite(),
        _ => true,
    })
}

fn validate_network(name: &str, network: &Network) -> Result<(), String> {
    net::validate(&network.graph).map_err(|d| rays_diagnostic::to_string(&d))?;
    let nodes = edit_graph::inspect(&network.graph.geometry);
    let exists = |id: i64| edit_graph::find(&network.graph.geometry, id).is_some();
    if network.displayed.map_or(false, |id| !exists(id)) {
        return Err(format!("{name} has an invalid display node"));
    }
    if !nodes.iter().all(|info| info.inputs.iter().all(|i| i.map_or(true, exists)) && finite_fields(&sop::node::parameter_fields(&info.node))) {
        return Err(format!("{name} has invalid inputs or nonfinite parameters"));
    }
    Ok(())
}

fn validate_kinds(name: &str, graph: &[kg::Node]) -> Result<(), String> {
    let exists = |id: i64| kg::find(graph, id).is_some();
    if graph.iter().all(|node| node.input.map_or(true, exists) && finite_fields(&kg::fields(node))) {
        Ok(())
    } else {
        Err(format!("{name} has invalid inputs or nonfinite parameters"))
    }
}

/// The saved/loaded document boundary. Disconnected SOPs are editable and valid; compiling their
/// display is a separate cook-time check.
pub fn validate(value: &T) -> Result<(), String> {
    validate_kinds("scene", &value.scene)?;
    for node in &value.scene {
        match (node.kind.as_str(), value.networks.contains_key(&node.id), value.layers.contains_key(&node.id)) {
            ("geometry", true, false) | ("world", false, true) | ("camera" | "light", false, false) => {}
            ("geometry" | "world", _, _) => return Err(format!("object {} (#{}) has no network", sop::node::ocaml_quote(&node.label), node.id)),
            ("camera" | "light", _, _) => return Err(format!("object #{} cannot own a network", node.id)),
            _ => return Err(format!("invalid scene object {}", sop::node::ocaml_quote(&node.kind))),
        }
    }
    let owner = |id: i64, kind: &str| -> Result<(), String> {
        match kg::find(&value.scene, id) {
            None => Err(format!("network has missing owner #{id}")),
            Some(node) if node.kind != kind => Err(format!("network #{id} has the wrong context")),
            Some(_) => Ok(()),
        }
    };
    for (id, network) in &value.networks {
        owner(*id, "geometry")?;
        if network.context != context::sop() {
            return Err(format!("network #{id} has the wrong context"));
        }
        validate_network(&format!("network of object #{id}"), network)?;
    }
    for (id, layers) in &value.layers {
        owner(*id, "world")?;
        validate_kinds(&format!("layers of object #{id}"), layers)?;
    }
    if !finite_fields(&settings::fields(&value.settings)) {
        return Err("settings contain nonfinite parameters".to_owned());
    }
    match value.active_camera {
        None => Ok(()),
        Some(id) => match kg::find(&value.scene, id) {
            Some(node) if node.kind == "camera" => Ok(()),
            _ => Err(format!("active camera #{id} is not a camera object")),
        },
    }
}

/// Networks follow their objects: a deleted object's network goes.
pub fn prune(value: &T) -> T {
    let alive = |id: i64| kg::find(&value.scene, id).is_some();
    let networks: IntMap<Network> = value.networks.iter().filter(|(id, _)| alive(**id)).map(|(k, v)| (*k, v.clone())).collect();
    let layers: IntMap<kg::T> = value.layers.iter().filter(|(id, _)| alive(**id)).map(|(k, v)| (*k, v.clone())).collect();
    let active_camera = value.active_camera.filter(|id| alive(*id));
    if networks.len() == value.networks.len() && layers.len() == value.layers.len() && active_camera == value.active_camera {
        value.clone()
    } else {
        T { networks, layers, active_camera, ..value.clone() }
    }
}

/// The lowered sop graph a geometry object's network is. An unchanged network is kept across
/// lowerings while the lowering is rebuilt, so identity alone is not enough: the compiled root
/// still names the graph.
pub fn object_graph(value: &T, id: i64) -> Option<String> {
    let lowered = &value.workspace.lowered;
    let n = value.networks.get(&id)?;
    lowered
        .graphs
        .iter()
        .find(|g| {
            net::ptr_eq(&g.network, &n.graph)
                || match (g.root, edit_graph::root(&n.graph.geometry)) {
                    (Some(a), Some(b)) => a == b,
                    _ => false,
                }
        })
        .map(|g| g.name.clone())
}

/// Read-only views for tests and tools.
pub fn scene_graph(value: &T) -> kg::T {
    value.scene.clone()
}

pub fn object_network(value: &T, id: i64) -> Option<(net::Network, Option<i64>)> {
    value.networks.get(&id).map(|n| (n.graph.clone(), n.displayed))
}

/// OCaml `Printf "%h"`.
pub fn hex_float(x: f64) -> String {
    if x.is_nan() {
        return "nan".to_owned();
    }
    if x.is_infinite() {
        return if x < 0. { "-infinity" } else { "infinity" }.to_owned();
    }
    let bits = x.to_bits();
    let sign = if bits >> 63 == 1 { "-" } else { "" };
    let exponent = ((bits >> 52) & 0x7ff) as i64;
    let mantissa = bits & ((1u64 << 52) - 1);
    let (lead, exp) = if exponent == 0 {
        if mantissa == 0 { (0, 0) } else { (0, -1022) }
    } else {
        (1, exponent - 1023)
    };
    let mut digits = format!("{mantissa:013x}");
    while digits.ends_with('0') {
        digits.pop();
    }
    let frac = if digits.is_empty() { String::new() } else { format!(".{digits}") };
    format!("{sign}0x{lead}{frac}p{}{}", if exp < 0 { "-" } else { "+" }, exp.abs())
}

fn pv(v: &PV) -> String {
    match v {
        PV::Bool(x) => x.to_string(),
        PV::Int(x) => x.to_string(),
        PV::Float(x) => hex_float(*x),
        PV::Text(x) => sop::node::ocaml_quote(&String::from_utf8_lossy(x)),
        PV::Choice(x) => sop::node::ocaml_quote(x),
    }
}

fn fields_text(fs: &[param::FieldView]) -> String {
    fs.iter().map(|f| format!("{}={}", f.name, pv(&f.current))).collect::<Vec<_>>().join(" ")
}

/// A deterministic text of everything the editor keeps in a document, for crash reports and for
/// tests that compare two documents. Not loadable: the workspace text is the saved form.
pub fn dump(value: &T) -> String {
    let mut b = String::new();
    let mut line = |text: String| {
        b.push_str(&text);
        b.push('\n');
    };
    fn graph(line: &mut dyn FnMut(String), n: &net::Network) {
        for info in edit_graph::inspect(&n.geometry) {
            let key = edit_graph::node_factory_key(&n.geometry, info.id).unwrap_or_else(|| info.operation.clone());
            let slots = edit_graph::node_slot_names(&n.geometry, info.id).unwrap_or_default().join(",");
            let inputs = info.inputs.iter().map(|i| i.map_or("_".to_owned(), |i| i.to_string())).collect::<Vec<_>>().join(",");
            let root = match edit_graph::root(&n.geometry) {
                Some(r) if r == info.id => " root",
                _ => "",
            };
            line(format!(
                "  node {} {} {} slots={} inputs={}{} {}",
                info.id,
                key,
                sop::node::ocaml_quote(&info.label),
                slots,
                inputs,
                root,
                fields_text(&sop::node::parameter_fields(&info.node))
            ));
        }
        for p in n.drives.keys() {
            line(format!("  drive {}.{} live", p.node, p.path));
        }
    }
    fn net_line(line: &mut dyn FnMut(String), name: &str, n: &Network) {
        line(format!("{} {} display={}", name, context::name(n.context), n.displayed.map_or("-".to_owned(), |d| d.to_string())));
        graph(line, &n.graph);
    }
    fn kinds_line(line: &mut dyn FnMut(String), name: &str, context: &str, g: &[kg::Node]) {
        line(format!("{name} {context}"));
        for node in g {
            line(format!(
                "  node {} {} {} input={} {}",
                node.id,
                node.kind,
                sop::node::ocaml_quote(&node.label),
                node.input.map_or("_".to_owned(), |i| i.to_string()),
                fields_text(&kg::fields(node))
            ));
        }
    }
    kinds_line(&mut line, "scene", "scene", &value.scene);
    for (id, n) in &value.networks {
        net_line(&mut line, &format!("object {id}"), n);
    }
    for (id, l) in &value.layers {
        kinds_line(&mut line, &format!("object {id}"), "world", l);
    }
    for (key, world) in &value.view_worlds {
        line(format!("view {} world {}", key, world.as_ref().map_or("-".to_owned(), |w| sop::node::ocaml_quote(&w.node.label))));
    }
    for (key, r) in &value.view_roots {
        let camera = match &r.camera {
            Some(c) => {
                let p = rcamera::position(c);
                format!(" camera {} {} {}", cfmt::g(p.x, 6), cfmt::g(p.y, 6), cfmt::g(p.z, 6))
            }
            None => String::new(),
        };
        line(format!("view {} root {}{}", key, fields_text(&root::fields(&r.params)), camera));
    }
    line(format!("camera {}", value.active_camera.map_or("-".to_owned(), |c| c.to_string())));
    line(format!("root {}", fields_text(&root::fields(&value.root))));
    line(format!("settings {}", fields_text(&settings::fields(&value.settings))));
    b
}

// ---- naming a home for a message: structural, read from the workspace source ----

fn last<V: Clone>(l: &[V]) -> Option<V> {
    l.last().cloned()
}

/// The form a home is written as, when the source can be walked to it: a loop's body is its last
/// child, and a copy's `rel` walks the body's `let*` bindings to the template.
pub fn syntax_of(source: &[S], home: &Home) -> Option<S> {
    match home {
        Home::BoundAt(path) => arg_text(source, path, &ArgKey::Whole),
        Home::InlineIn(parent, key) => syntax_of(source, parent).and_then(|e| arg_of(&e, key)),
        Home::Copy { loop_, rel, .. } => {
            fn walk(e: &S, steps: &[String]) -> Option<S> {
                match steps.split_first() {
                    None => Some(e.clone()),
                    Some((step, rest)) => match &e.node {
                        Node::List(items) if items.len() >= 2 && matches!(&items[0].node, Node::Sym(h) if h == "let*") && matches!(items[1].node, Node::Vec(_)) => {
                            let Node::Vec(bs) = &items[1].node else { return None };
                            if step == "@result" {
                                last(&items[2..]).and_then(|b| walk(&b, rest))
                            } else {
                                let mut i = 0;
                                let mut found = None;
                                while i + 1 < bs.len() {
                                    if matches!(&bs[i].node, Node::Sym(n) if n == step) {
                                        found = Some(bs[i + 1].clone());
                                        break;
                                    }
                                    i += 2;
                                }
                                found.and_then(|v| walk(&v, rest))
                            }
                        }
                        _ => {
                            if step == "@result" { walk(e, rest) } else { None }
                        }
                    },
                }
            }
            let e = syntax_of(source, loop_)?;
            match &e.node {
                Node::List(items) if !items.is_empty() => last(&items[1..]).and_then(|body| walk(&body, rel)),
                _ => None,
            }
        }
        Home::Looped => None,
    }
}

/// The nearest named binding around a home: the graph, or the let* binding.
fn enclosing(home: &Home) -> Option<String> {
    match home {
        Home::BoundAt(path) => path.iter().rev().find(|n| !n.is_empty() && !n.starts_with('@')).cloned(),
        Home::Copy { loop_, .. } => enclosing(loop_),
        Home::InlineIn(parent, _) => enclosing(parent),
        Home::Looped => None,
    }
}

fn truncate(n: usize, text: &str) -> String {
    if text.len() <= n {
        text.to_owned()
    } else {
        String::from_utf8_lossy(&text.as_bytes()[..n - 3]).into_owned() + "..."
    }
}

/// The head of a form for a message: `for [j (range 2)]`, `scene/light`.
fn head_text(e: &S) -> String {
    if let Node::List(items) = &e.node {
        if let Some(Node::Sym(h)) = items.first().map(|x| &x.node) {
            if let Some(v @ S { node: Node::Vec(_), .. }) = items.get(1) {
                return format!("{h} {}", lisp::flat(&S { node: v.node.clone(), span: v.span.clone(), id: v.id, ..e.clone() }));
            }
            return h.clone();
        }
    }
    lisp::flat(e)
}

/// The line of a form in the printed text of the workspace, when it is part of it.
fn line_of(source: &[S], e: &S) -> Option<usize> {
    let (text, spans) = lisp::print(source);
    let span = spans.iter().find(|(id, _)| *id == e.id).map(|(_, sp)| sp.clone())?;
    if span.start as usize <= text.len() {
        Some(1 + text.as_bytes()[..span.start as usize].iter().filter(|c| **c == b'\n').count())
    } else {
        None
    }
}

/// What a loop or an expression is called in a message: its binding if it has one, else its head
/// form, with its line and the binding around it. A copy is named by the loop that makes it.
pub fn describe(source: &[S], home: &Home) -> String {
    match home {
        Home::BoundAt(path) if !path.is_empty() => path[path.len() - 1].clone(),
        Home::Copy { loop_, .. } => describe(source, loop_),
        home => match syntax_of(source, home) {
            None => match enclosing(home) {
                Some(n) => format!("a form in {}", sop::node::ocaml_quote(&n)),
                None => "a form".to_owned(),
            },
            Some(e) => {
                let mut wh: Vec<String> = Vec::new();
                if let Some(l) = line_of(source, &e) {
                    wh.push(format!("line {l}"));
                }
                if let Some(n) = enclosing(home) {
                    wh.push(format!("in {}", sop::node::ocaml_quote(&n)));
                }
                format!("`{}`{}", truncate(40, &head_text(&e)), if wh.is_empty() { String::new() } else { format!(" ({})", wh.join(", ")) })
            }
        },
    }
}

/// A home with its copy indices erased: the copies of one loop share it.
pub fn template(home: &Home) -> Home {
    match home {
        Home::Copy { loop_, rel, .. } => Home::Copy { loop_: Box::new(template(loop_)), rel: rel.clone(), index: 0 },
        Home::InlineIn(h, key) => Home::InlineIn(Box::new(template(h)), key.clone()),
        h => h.clone(),
    }
}

/// The loop that holds a home's template, erased.
pub fn loop_of(home: &Home) -> Option<Home> {
    match home {
        Home::Copy { loop_, .. } => Some(template(loop_)),
        Home::InlineIn(h, _) => loop_of(h),
        Home::BoundAt(_) | Home::Looped => None,
    }
}

/// How many of the document's objects are copies from the loop of `home`.
pub fn copies(doc: &T, home: &Home) -> usize {
    match loop_of(home) {
        None => 0,
        key => doc.homes.objects.iter().filter(|(_, h)| loop_of(h) == key).count(),
    }
}
