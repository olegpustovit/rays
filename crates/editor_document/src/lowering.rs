//! Lower scene, World and settings results into the editor document without cooking SOPs. Values compare structurally; networks and SOP nodes use pointer identity, settings use same_values. Fast settings are computed once with lowering.
use flow_syntax::symbol::Symbol;
use crate::document::{self, Home, ViewRoot, ViewWorld};
use crate::kind_graph as kg;
use crate::kinds::{ports, scene_kinds, window_default, window_of_values, window_schema, world_kinds, Window};
use crate::literal_edit;
use crate::objects::{self, root};
use crate::settings;
use crate::shell_eval;
use crate::workspace_doc;
use flow_check::workspace::{self as w, Graph, LoopKind, Node as WNode, Term};
use flow_check::term_arena::{Field, PatternNode, RangeId, TermArena, TermId};
use flow_eval::Plan;
use flow_graph::flow_edit::ArgKey;
use flow_ir::flow_ir::{gpu, StaticOptions};
use flow_sop::lower as lower_mod;
use flow_sop_catalog::network as net;
use flow_sop_catalog::port;
use flow_syntax::context::{self, Context};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{Record, Value as E, struct_value};
use param::Value as PV;
use scene_core::camera as rcamera;
use rays_diagnostic::Diagnostic;
use sop::edit_graph::{self, Factory};
use std::sync::{Arc, OnceLock};

type R<T> = Result<T, Diagnostic>;

fn diag(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}

fn flow<T>(r: Result<T, String>) -> R<T> {
    r.map_err(|m| diag("E_DOCUMENT", &m))
}

#[derive(Clone, Default)]
pub struct Options {
    /// None: `sop::nodes::factories()`.
    pub factories: Option<Vec<Factory>>,
    pub gpu: Option<gpu::Config>,
    pub defer: Option<bool>,
    /// None keeps editor probe recording enabled.
    pub record: Option<bool>,
    pub previous: Option<document::T>,
}

// ---- lowering a struct ----

const SLOT_NAMES: [&str; 10] = ["geometry", "texture", "layers", "below", "name", "parent", "active", "world", "scene", "camera"];

/// The field changes a struct's keywords make, checked against the schema's bounds.
fn changes(qualified: &str, args: &Record) -> R<Vec<(String, PV)>> {
    let ports = ports(qualified);
    let mut acc = Vec::new();
    for (key, value) in args {
        if SLOT_NAMES.contains(&key.as_str()) {
            continue;
        }
        let parameter = port::find_parameter(&ports, key)?;
        acc.extend(lower_mod::changes(&parameter, value)?);
    }
    Ok(acc)
}

fn label_arg(args: &Record) -> Option<String> {
    match args.get_str("name") {
        Some(E::Text(s)) if !s.is_empty() => Some(String::from_utf8_lossy(s).into_owned()),
        _ => None,
    }
}

fn live_light_field(kind: &str, key: &str) -> bool {
    kind == "scene/light" && (key == "intensity" || key == "color")
}

fn force0(value: &E) -> R<E> {
    flow_eval::force(value, &input::at_time(0.), None)
}

/// Geometry references are static `Deferred (Geometry, id)` identities even when their SOP
/// parameters are live. Residuals in the surrounding structs would otherwise be silently frozen
/// when we materialize these contexts at startup.
fn check_context_time(workspace: &workspace_doc::T, plan: &Plan) -> R<()> {
    fn live_field(path: &str, v: &E) -> Option<String> {
        match v {
            E::Residual(_) => Some(path.to_owned()),
            E::Struct(g) => g.fields.iter().find_map(|(key, value)| if live_light_field(&g.name, key) { None } else { live_field(&format!("{}.{key}", g.name), value) }),
            E::Record(args) => args.iter().find_map(|(key, value)| live_field(&format!("{path}.{key}"), value)),
            E::List(values) => values.iter().find_map(|v| live_field(path, v)),
            _ => None,
        }
    }
    for instance in &plan.instances {
        let graph = workspace.checked.graphs.iter().find(|g| g.name == instance.graph && !context::supports_values(g.context));
        if let Some(graph) = graph {
            if let Some(field) = live_field("result", &instance.result) {
                return Err(rays_diagnostic::error(
                    "E_CONTEXT_TIME".to_owned(),
                    format!(
                        "Graph {}: {} depends on t, but {} fields are static. Use a literal or a SOP/value drive.",
                        graph.name,
                        field,
                        w::context_name(&graph.context)
                    ),
                    Some(graph.body.form().span.clone()),
                    None,
                    None,
                    Vec::new(),
                    Vec::new(),
                ));
            }
        }
    }
    Ok(())
}

pub fn has_settings(workspace: &workspace_doc::T) -> bool {
    workspace.checked.graphs.iter().any(|g| g.context == context::settings())
}

fn settings_of(value: &E) -> R<settings::T> {
    match value {
        E::Struct(g) if g.name == "settings/config" => {
            let args = &g.fields;
            let changes = changes("settings/config", args)?;
            let schema = window_schema();
            let base = settings::make(&schema, schema.default().to_vec());
            let (settings, _) = settings::apply(&base, &changes).map_err(|m| diag("E_RANGE", &m))?;
            Ok(settings)
        }
        _ => Err(diag("E_LOWER", "A settings graph returns a (settings/config ...) call.")),
    }
}

/// The window the settings graph asks for; defaults without one.
pub fn plan_window(workspace: &workspace_doc::T, plan: &Plan) -> R<Window> {
    check_context_time(workspace, plan)?;
    match workspace_doc::context_value(true, workspace, plan, context::settings())? {
        None => Ok(window_default()),
        Some(value) => {
            let st = settings_of(&value)?;
            let values = settings::get(&window_schema(), &st).map_err(|m| diag("E_DOCUMENT", &m))?;
            Ok(window_of_values(&values))
        }
    }
}

/// The window of a checked workspace: its settings graph evaluated, defaults without one.
pub fn window(workspace: &workspace_doc::T) -> R<Window> {
    let evaluated = flow_ir::flow_ir::static_(&StaticOptions { inputs: Some((*workspace.inputs).clone()), ..Default::default() }, &workspace.checked)?;
    plan_window(workspace, &evaluated.plan)
}

/// The same window from a document's own lowering, whose plan is the one `window` evaluates.
pub fn document_window(doc: &document::T) -> R<Window> {
    plan_window(&doc.workspace.ws, &doc.workspace.lowered.plan)
}

/// The precision of a document's workspace, read once with its lowering.
pub fn fast(doc: &document::T) -> bool {
    doc.workspace.fast.force()
}

// ---- where things are written ----

/// A call of interest in an evaluated result, with where its text is.
#[derive(Clone)]
struct Call {
    kind: String,
    args: Record,
    home: Home,
    /// The graph a `(ref g)` argument names, for `scene/world`.
    via: Option<String>,
}

const SLOT_ARGS: [&str; 5] = ["geometry", "layers", "below", "scene", "world"];

/// Argument `i` of a call as an edit addresses it: positional for a slot, else its keyword.
fn arg_key(arena: &TermArena, args: &[Field], i: usize) -> ArgKey {
    let name = arena.name(args[i].name);
    if SLOT_ARGS.contains(&name) {
        ArgKey::Pos(args[..i].iter().filter(|field| SLOT_ARGS.contains(&arena.name(field.name))).count() as i64)
    } else {
        ArgKey::Kw(name.to_owned())
    }
}

type Want<'a> = &'a dyn Fn(&str) -> bool;
type Below<'a> = &'a dyn Fn(&str) -> Option<&'static str>;

/// Calls with no term to read (graph inputs, loops, macro expansions): reachable but unwritable.
fn loose(want: Want, below: Below, v: &E) -> Vec<Call> {
    match v {
        E::Struct(g) if g.name == "scene/merge" => g.fields.iter().flat_map(|(_, v)| loose(want, below, v)).collect(),
        E::Struct(g) if want(&g.name) => {
            let (kind, args) = (&g.name, &g.fields);
            let mut out = match below(kind).and_then(|slot| args.iter().find(|(n, _)| n == slot)) {
                Some((_, under)) => loose(want, below, under),
                None => Vec::new(),
            };
            out.push(Call { kind: kind.clone(), args: args.clone(), home: Home::Looped, via: None });
            out
        }
        E::List(xs) => xs.iter().flat_map(|x| loose(want, below, x)).collect(),
        _ => Vec::new(),
    }
}

/// The home of a call in a loop body, as a copy: the template's path goes relative to the loop, a
/// call written inside another call of the body keeps that nesting, a loop in a loop nests.
fn copy(zone: &[Symbol], loop_: &Home, index: i64, home: Home) -> Home {
    match home {
        Home::BoundAt(p) if p.len() > zone.len() && p[..zone.len()] == *zone => Home::Copy { loop_: Box::new(loop_.clone()), rel: p[zone.len()..].to_vec(), index },
        Home::InlineIn(h, k) => Home::InlineIn(Box::new(copy(zone, loop_, index, *h)), k),
        Home::Copy { loop_: inner, rel, index: i } => Home::Copy { loop_: Box::new(copy(zone, loop_, index, *inner)), rel, index: i },
        h => h,
    }
}

type Env<'a> = [(&'a str, TermId)];

fn is_for(t: &Term) -> bool {
    matches!(t.node, WNode::Loop(id) if t.arena().loop_node(id).kind == LoopKind::For)
}

fn is_loop(env: &Env, t: &Term) -> bool {
    match t.node {
        WNode::Loop(id) => t.arena().loop_node(id).kind == LoopKind::For,
        WNode::RefBinding(id) if t.arena().name_ids(t.arena().ref_binding(id).fields).is_empty() => env.iter().find(|(k, _)| *k == t.name(t.arena().ref_binding(id).name)).is_some_and(|(_, id)| is_for(&t.child(*id))),
        _ => false,
    }
}

/// A merge's values line up with its arguments: one each, and at most one loop takes the rest.
fn merge_fits(env: &Env, owner: &Term, args: &[Field], kept: &[usize], values: usize) -> bool {
    let loops = kept.iter().filter(|i| is_loop(env, &owner.child(args[**i].term))).count() as i64;
    let extra = values as i64 - kept.len() as i64;
    if loops == 0 {
        extra == 0
    } else {
        loops == 1 && extra >= -1
    }
}

/// The running index of each copy of a loop at the iteration tuple `iter` (its enclosing loops'
/// indices): the copies are the iterations not in `skip`, so a copy keeps its index when others
/// are deleted.
fn real_indices(arena: &TermArena, skip: &[RangeId], iter: &[usize], n: usize) -> Vec<usize> {
    let mut k = 0usize;
    (0..n)
        .map(|_| {
            loop {
                if skipped(arena, skip, iter, k) {
                    k += 1;
                } else {
                    break;
                }
            }
            let r = k;
            k += 1;
            r
        })
        .collect()
}

/// The arguments of a merge that stay, with their written positions.
fn kept(arena: &TermArena, iter: &[usize], skip: &[RangeId], args: &[Field]) -> Vec<usize> {
    (0..args.len()).filter(|i| !skipped(arena, skip, iter, *i)).collect()
}

fn skipped(arena: &TermArena, skip: &[RangeId], iter: &[usize], index: usize) -> bool {
    skip.iter().any(|range| arena.indices(*range).iter().copied().eq(iter.iter().copied().chain(std::iter::once(index))))
}

/// The graph a `scene/world`'s World argument references: written in place or by a binding.
fn referenced(env: &Env, t: &Term) -> Option<String> {
    match t.node {
        WNode::GraphRef(id) => Some(t.name(t.arena().graph_ref(id).graph).to_owned()),
        WNode::RefBinding(id) if t.arena().name_ids(t.arena().ref_binding(id).fields).is_empty() => env.iter().find(|(k, _)| *k == t.name(t.arena().ref_binding(id).name)).and_then(|(_, id)| referenced(env, &t.child(*id))),
        _ => None,
    }
}

/// Walk the terms of a graph beside its evaluated result: each wanted call, bottom layer first,
/// with its binding, or the argument of another call that holds it. `below kind` names the slot a
/// call stacks its predecessor in.
fn walk(want: Want, below: Below, iter: &[usize], env: &Env, place: Option<(Home, ArgKey)>, t: &Term, v: &E) -> Vec<Call> {
    let arena = t.arena();
    let home = || match (t.path(), &place) {
        (Some(path), _) => Home::BoundAt(path.iter().map(|name| t.symbol(*name)).collect()),
        (None, Some((parent, key))) => Home::InlineIn(Box::new(parent.clone()), key.clone()),
        (None, None) => Home::Looped,
    };
    match (t.node, v) {
        (WNode::Let(id), _) => {
            let node = arena.let_node(id);
            let mut env2: Vec<(&str, TermId)> = arena.bindings(node.bindings).iter().filter_map(|binding| match arena.pattern(binding.pattern) {
                PatternNode::Name(name) => Some((arena.name(name), binding.term)),
                _ => None,
            }).collect();
            env2.extend_from_slice(env);
            walk(want, below, iter, &env2, place, &t.child(node.body), v)
        }
        (WNode::RefBinding(id), _) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => match env.iter().find(|(name, _)| *name == t.name(arena.ref_binding(id).name)) {
            Some((_, bound)) => walk(want, below, iter, env, None, &t.child(*bound), v),
            None => loose(want, below, v),
        },
        (WNode::Call(id), E::Struct(vg)) if vg.name == t.name(arena.call(id).kind) && want(&vg.name) => {
            let vargs = &vg.fields;
            let call = arena.call(id);
            let kind = t.name(call.kind);
            let targs = arena.fields(call.args);
            let here = home();
            let under = match below(kind) {
                Some(slot) => match (targs.iter().position(|field| t.name(field.name) == slot), vargs.iter().find(|(name, _)| name == slot)) {
                    (Some(i), Some((_, under_value))) => walk(want, below, iter, env, Some((here.clone(), arg_key(arena, targs, i))), &t.child(targs[i].term), under_value),
                    _ => Vec::new(),
                },
                None => Vec::new(),
            };
            let via = if kind != "scene/world" { None } else {
                targs.iter().find(|field| t.name(field.name) == "world").and_then(|field| referenced(env, &t.child(field.term)))
            };
            let mut out = under;
            out.push(Call { kind: kind.to_owned(), args: vargs.clone(), home: here, via });
            out
        }
        (WNode::Op(id), E::Struct(vg)) if t.name(arena.op(id).op) == "scene/merge" && vg.name == "scene/merge" => {
            let vargs = &vg.fields;
            let op = arena.op(id);
            let all = arena.fields(op.args);
            let kp = kept(arena, iter, arena.skip(op.skip), all);
            if !merge_fits(env, t, all, &kp, vargs.len()) { return loose(want, below, v); }
            let here = home();
            // an argument gives one value, a loop the rest: its copies
            let extra = vargs.len() as i64 - kp.len() as i64;
            let mut at = 0usize;
            let mut found = Vec::new();
            for orig in kp {
                let term = t.child(all[orig].term);
                let looped = is_loop(env, &term);
                let n = if looped { (extra + 1).max(0) as usize } else { 1 };
                let value = if looped {
                    E::List(Arc::new(vargs.iter().skip(at).take(n).map(|(_, v)| v.clone()).collect()))
                } else {
                    match vargs.field_at(at) {
                        Some((_, v)) => v.clone(),
                        None => { at += n; continue; }
                    }
                };
                at += n;
                found.extend(walk(want, below, iter, env, Some((here.clone(), ArgKey::Pos(orig as i64))), &term, &value));
            }
            found
        }
        (WNode::Loop(id), E::List(xs)) if arena.loop_node(id).kind == LoopKind::For => {
            let node = arena.loop_node(id);
            let loop_ = home();
            let real = real_indices(arena, arena.skip(node.skip), iter, xs.len());
            let zone: flow_check::workspace::Path = arena.path(node.zone).iter().map(|name| arena.symbol(*name)).collect();
            let mut out = Vec::new();
            for (pos, x) in xs.iter().enumerate() {
                let i = real[pos];
                let mut it = iter.to_vec();
                it.push(i);
                for mut c in walk(want, below, &it, env, None, &t.child(node.body), x) {
                    c.home = copy(&zone, &loop_, i as i64, c.home);
                    out.push(c);
                }
            }
            out
        }
        _ => loose(want, below, v),
    }
}

fn graph_ref(workspace: &workspace_doc::T, ctx: Context) -> Option<&Graph> {
    workspace.checked.graphs.iter().find(|g| g.context == ctx)
}

/// The first graph of a context.
pub fn graph_of(workspace: &workspace_doc::T, ctx: Context) -> Option<Graph> {
    graph_ref(workspace, ctx).cloned()
}

fn graph_of_name<'a>(workspace: &'a workspace_doc::T, name: &str) -> Option<&'a Graph> {
    workspace.checked.graphs.iter().find(|g| g.name == name && g.context == context::world())
}

/// Scene calls retain their light residuals beside their homes; other contexts are materialized at
/// zero after unsupported live fields have been refused.
fn graph_calls(want: Want, below: Below, plan: &Plan, ctx: Context, graph: &Graph) -> R<Vec<Call>> {
    match plan.instances.iter().find(|i| i.default && i.graph == graph.name) {
        None => Ok(Vec::new()),
        Some(instance) => {
            let value = if ctx == context::scene() { instance.result.clone() } else { force0(&instance.result)? };
            Ok(walk(want, below, &[], &[], None, &graph.body, &value))
        }
    }
}

fn calls(want: Want, below: Below, workspace: &workspace_doc::T, plan: &Plan, ctx: Context) -> R<Vec<Call>> {
    match graph_ref(workspace, ctx) {
        None => Ok(Vec::new()),
        Some(graph) => graph_calls(want, below, plan, ctx, graph),
    }
}

fn no_below(_: &str) -> Option<&'static str> {
    None
}

/// A `scene/root` holds the rest of the scene in its `scene` slot.
fn scene_below(kind: &str) -> Option<&'static str> {
    if kind == "scene/root" { Some("scene") } else { None }
}

// ---- the scene ----

#[derive(Clone)]
struct Item {
    kind_of: kg::Kind,
    label: String,
    values: Vec<(String, PV)>,
    texture: Option<E>,
    geometry: Option<lower_mod::Graph>,
    home: Home,
    parent: Option<String>,
    active: bool,
    group: Option<String>,
    drives: Option<(String, Vec<(port::Parameter, E)>)>,
    /// The world graph a `scene/world` references.
    via: Option<String>,
}

fn scene_table() -> &'static Vec<(String, kg::Kind)> {
    static K: OnceLock<Vec<(String, kg::Kind)>> = OnceLock::new();
    K.get_or_init(scene_kinds)
}

fn world_table() -> &'static Vec<(String, kg::Kind)> {
    static K: OnceLock<Vec<(String, kg::Kind)>> = OnceLock::new();
    K.get_or_init(world_kinds)
}

fn scene_kind(qualified: &str) -> kg::Kind {
    scene_table().iter().find(|(k, _)| k == qualified).map(|(_, k)| k.clone()).unwrap_or_else(|| panic!("no scene kind {qualified}"))
}

fn world_kind(qualified: &str) -> kg::Kind {
    world_table().iter().find(|(k, _)| k == qualified).map(|(_, k)| k.clone()).unwrap_or_else(|| panic!("no world kind {qualified}"))
}

fn forced_args(forced: E) -> Record {
    match forced {
        E::Struct(g) => g.fields.clone(),
        _ => Record::default(),
    }
}

fn item_of(lowered: &lower_mod::T, call: &Call) -> R<Item> {
    let Call { kind, args, home, via } = call;
    let dynamic: Vec<&(Symbol, E)> = args.iter().filter(|(key, value)| live_light_field(kind, key) && flow_eval::is_live(value)).collect();
    // The first failing input from the right wins.
    let mut dyn_params: Vec<(port::Parameter, E)> = Vec::new();
    let kind_ports = ports(kind);
    for (key, value) in dynamic.iter().rev() {
        let parameter = port::find_parameter(&kind_ports, key)?;
        dyn_params.push((parameter, value.clone()));
    }
    dyn_params.reverse();
    let drives = if dyn_params.is_empty() { None } else { Some((kind.clone(), dyn_params)) };
    let forced = force0(&struct_value(kind.clone(), ty::scene(), args.clone()))?;
    let args = forced_args(forced);
    let kind_of = scene_kind(kind);
    let values = changes(kind, &args)?;
    let geometry = match args.iter().find(|(n, _)| n == "geometry") {
        Some((_, E::Deferred(Ty::Named(t), id))) if t == "geometry" => {
            let inst = lowered.plan.nodes[*id as usize].inst;
            match lowered.graphs.iter().find(|g| g.instance as i64 == inst) {
                Some(g) => Some(g.clone()),
                None => return Err(diag("E_LOWER", "A scene object's geometry comes from a sop graph.")),
            }
        }
        _ => None,
    };
    let label = match (label_arg(&args), &geometry) {
        (Some(label), _) => label,
        (None, Some(g)) => g.name.clone(),
        (None, None) => {
            if kind == "scene/world" {
                "World".to_owned()
            } else {
                kind_of.label.to_ascii_lowercase()
            }
        }
    };
    let parent = match args.iter().find(|(n, _)| n == "parent") {
        Some((_, E::Text(s))) if !s.is_empty() => Some(String::from_utf8_lossy(s).into_owned()),
        _ => None,
    };
    let active = args.iter().find(|(n, _)| n == "active").map(|(_, v)| v) == Some(&E::Bool(true));
    let texture = args.iter().find(|(n, _)| n == "texture").map(|(_, v)| v.clone());
    Ok(Item { kind_of, label, values, geometry, home: home.clone(), parent, active, group: None, drives, via: via.clone(), texture })
}

fn is_scene_kind(kind: &str) -> bool {
    scene_table().iter().any(|(k, _)| k == kind)
}

/// How a scene renders: the settings of the scene graph's `scene/root`, where the root's text is,
/// and the object its `:camera` slot names (an index among the objects).
struct Root {
    params: root::Parameters,
    root_home: Option<Home>,
    camera: Option<usize>,
}

fn root_params(values: &[(String, PV)]) -> R<root::Parameters> {
    let (record, _) = root::parameters_schema().apply_all(&root::to_values(&root::default()), values).map_err(|m| diag("E_RANGE", &m))?;
    Ok(root::from_values(record))
}

/// The item a camera value is: the call whose arguments it carries.
fn find_call(calls: &[&Call], value: &E) -> Option<usize> {
    match value {
        E::Struct(g) => calls.iter().position(|c| c.args == g.fields),
        _ => None,
    }
}

fn root_of(all: &[Call]) -> R<Root> {
    match all.iter().find(|c| c.kind == "scene/root") {
        None => Ok(Root { params: root::default(), root_home: None, camera: None }),
        Some(call) => {
            let own: Vec<(Symbol, E)> = call.args.iter().filter(|(key, _)| !SLOT_NAMES.contains(&key.as_str())).cloned().collect();
            let forced = force0(&struct_value("scene/root", ty::scene(), own.into()))?;
            let values = changes("scene/root", &forced_args(forced))?;
            let params = root_params(&values)?;
            let others: Vec<&Call> = all.iter().filter(|c| c.kind != "scene/root").collect();
            Ok(Root { params, root_home: Some(call.home.clone()), camera: call.args.iter().find(|(n, _)| n == "camera").and_then(|(_, v)| find_call(&others, v)) })
        }
    }
}

/// The objects of a workspace: its scene graph's calls, else one geometry object per sop graph;
/// and its root.
fn items(workspace: &workspace_doc::T, lowered: &lower_mod::T) -> R<(Vec<Item>, Root)> {
    match graph_ref(workspace, context::scene()) {
        Some(_) => {
            let scene = calls(&is_scene_kind, &scene_below, workspace, &lowered.plan, context::scene())?;
            let mut objects: Vec<Item> = Vec::new();
            for call in scene.iter().rev() {
                if call.kind == "scene/root" {
                    continue;
                }
                objects.push(item_of(lowered, call)?);
            }
            objects.reverse();
            let root = root_of(&scene)?;
            for (i, item) in objects.iter_mut().enumerate() {
                if Some(i) == root.camera {
                    item.active = true;
                }
            }
            Ok((objects, root))
        }
        None => {
            let params = root_params(&[])?;
            let objects = lowered
                .graphs
                .iter()
                .filter(|g| g.default)
                .map(|g| Item {
                    kind_of: objects::geometry::kind(),
                    label: g.name.clone(),
                    values: Vec::new(),
                    geometry: Some(g.clone()),
                    texture: None,
                    home: Home::Looped,
                    parent: None,
                    active: false,
                    group: None,
                    drives: None,
                    via: None,
                })
                .collect();
            Ok((objects, Root { params, root_home: None, camera: None }))
        }
    }
}

/// The scene value of a graph, at t = 0, as loose calls (a viewport's own scene instance).
fn scene_calls(value: &E) -> Vec<Call> {
    loose(&is_scene_kind, &scene_below, value)
}

// ---- what a scene graph refuses ----

/// Everything a scene value holds, through merges, roots and parts.
fn members(v: &E) -> Vec<E> {
    match v {
        E::Struct(g) if g.name == "scene/merge" => g.fields.iter().flat_map(|(_, v)| members(v)).collect(),
        E::Struct(g) if g.name == "scene/root" => g.fields.iter().find(|(n, _)| n == "scene").map_or(Vec::new(), |(_, v)| members(v)),
        E::List(xs) => xs.iter().flat_map(members).collect(),
        E::Struct(..) => vec![v.clone()],
        _ => Vec::new(),
    }
}

/// A root wired into a merge or another root: it renders, so it is always last.
fn nested_root(under: bool, v: &E) -> bool {
    match v {
        E::Struct(g) if g.name == "scene/root" => under || g.fields.iter().find(|(n, _)| n == "scene").map_or(false, |(_, v)| nested_root(true, v)),
        E::Struct(g) if g.name == "scene/merge" => g.fields.iter().any(|(_, v)| nested_root(true, v)),
        E::List(xs) => xs.iter().any(|x| nested_root(under, x)),
        _ => false,
    }
}

/// E_SCENE_ROOT, E_SCENE_WORLD, E_SCENE_CAMERA over every scene graph of the workspace.
fn check_scene(workspace: &workspace_doc::T, plan: &Plan) -> R<()> {
    for g in workspace.checked.graphs.iter() {
        let Some(instance) = plan.instances.iter().find(|i| i.default && i.graph == g.name) else { continue };
        if g.context != context::scene() {
            continue;
        }
        let refuse = |code: &str, message: String| -> R<()> {
            Err(rays_diagnostic::error(code.to_owned(), message, Some(g.body.form().span.clone()), None, None, Vec::new(), Vec::new()))
        };
        let value = &instance.result;
        if nested_root(false, value) {
            return refuse("E_SCENE_ROOT", format!("Graph {}: a scene/root is wired into a merge or another root. A root renders the scene, so it is always last.", g.name));
        }
        let worlds = graph_calls(&|k| k == "scene/world" || k == "scene/root", &scene_below, plan, context::scene(), g)?;
        let worlds: Vec<&Call> = worlds.iter().filter(|c| c.kind == "scene/world").collect();
        if worlds.len() >= 2 {
            let name = |c: &Call| document::describe(&workspace.source, &c.home);
            return refuse("E_SCENE_WORLD", format!("Graph {}: two Worlds reach one root, {} and {}. A scene has one World; take one out of the merge.", g.name, name(worlds[0]), name(worlds[1])));
        }
        if let E::Struct(g) = value {
            if g.name == "scene/root" {
                match g.fields.iter().find(|(n, _)| n == "camera").map(|(_, v)| v) {
                    Some(camera @ E::Struct(c)) if c.name == "scene/camera" && members(value).iter().any(|m| m == camera) => {}
                    Some(_) => return refuse("E_SCENE_CAMERA", format!("Graph {}: the root's :camera is not a camera in its scene.", g.name)),
                    None => {}
                }
            }
        }
    }
    Ok(())
}

fn find_node(graph: &[kg::Node], used: &[i64], operation: &str, label: &str) -> Option<i64> {
    graph.iter().find(|node| node.kind == operation && node.label == label && !used.contains(&node.id)).map(|n| n.id)
}

/// The node of a previous document that stands for this text: the one at the same home (so a
/// rename keeps the id), else the one of the same kind and label.
fn find_claim(graph: &[kg::Node], used: &[i64], operation: &str, home: &Home, label: &str, homes: &[(i64, Home)]) -> Option<i64> {
    let by_home = if *home == Home::Looped {
        None
    } else {
        homes.iter().find_map(|(id, h)| {
            if h == home && !used.contains(id) {
                match kg::find(graph, *id) {
                    Some(node) if node.kind == operation => Some(*id),
                    _ => None,
                }
            } else {
                None
            }
        })
    };
    by_home.or_else(|| find_node(graph, used, operation, label))
}

fn current_values(node: &kg::Node) -> Vec<(String, PV)> {
    kg::fields(node).into_iter().map(|f| (f.name, f.current)).collect()
}

/// What the text says is the whole truth of a declared node: fields it does not name are the
/// schema's defaults. A node whose values are already these is kept (the node passed in).
fn with_values(kind: &kg::Kind, node: &kg::Node, values: &[(String, PV)]) -> R<kg::Node> {
    let (fresh, _) = flow(kg::apply(&kg::Node { values: kind.defaults.clone(), ..node.clone() }, values))?;
    Ok(if current_values(&fresh) == current_values(node) { node.clone() } else { fresh })
}

/// A node of `kind` at `label` with the values the text gives it, under an id of its own or `id`.
fn instance(id: Option<i64>, kind: &kg::Kind, label: &str, values: &[(String, PV)]) -> R<kg::Node> {
    with_values(kind, &kg::instance(id, label, kind), values)
}

/// The view of the camera object a scene instance's `scene/root` names in its `:camera` slot: the
/// object is built like any scene object, from the fields its call writes.
fn instance_camera(value: Option<&E>) -> Option<rcamera::Camera> {
    match value {
        Some(v @ E::Struct(g)) if g.name == "scene/camera" => {
            let built = (|| -> R<Option<rcamera::Camera>> {
                let forced = force0(v)?;
                let values = changes("scene/camera", &forced_args(forced))?;
                let node = instance(None, &scene_kind("scene/camera"), "camera", &values)?;
                Ok(objects::camera::of_node(&node).map(|x| x.0))
            })();
            built.unwrap_or(None)
        }
        _ => None,
    }
}

/// How a viewport's own scene instance renders: its `scene/root`'s settings and camera, none for a
/// part.
pub fn instance_root(value: &E) -> Option<ViewRoot> {
    match value {
        E::Struct(g) if g.name == "scene/root" => {
            let args = &g.fields;
            let own: Vec<(Symbol, E)> = args.iter().filter(|(key, _)| !SLOT_NAMES.contains(&key.as_str())).cloned().collect();
            match force0(&struct_value("scene/root", ty::scene(), own.into())) {
                Ok(E::Struct(forced)) => match changes("scene/root", &forced.fields) {
                    Ok(values) => root_params(&values).ok().map(|params| ViewRoot { params, camera: instance_camera(args.iter().find(|(n, _)| n == "camera").map(|(_, v)| v)) }),
                    Err(_) => None,
                },
                _ => None,
            }
        }
        _ => None,
    }
}

/// Parents by name: `:parent "label"` links an object under the object of that label (of its own
/// scene instance).
fn link_parents(graph: kg::T, objects_: &[(i64, Item)]) -> R<kg::T> {
    let mut graph = graph;
    for (id, item) in objects_ {
        let want = match &item.parent {
            None => None,
            Some(name) => {
                let label = match &item.group {
                    Some(key) => format!("{name} ({key})"),
                    None => name.clone(),
                };
                match objects_.iter().find_map(|(pid, p)| if pid != id && p.label == label { Some(*pid) } else { None }) {
                    Some(pid) => Some(pid),
                    None => return Err(diag("E_LOWER", &format!("{}: there is no object named {} to be its parent.", item.label, format!("{:?}", name)))),
                }
            }
        };
        if objects::parent(&graph, *id) == want {
            continue;
        }
        if let Some(parent) = want {
            if objects::within(&graph, *id, parent) {
                return Err(diag("E_DOCUMENT", "connection would create a procedural cycle"));
            }
        }
        graph = kg::update(&graph, *id, &|node| kg::Node { input: want, ..node });
    }
    Ok(graph)
}

// ---- the World ----

fn is_world_kind(kind: &str) -> bool {
    world_table().iter().any(|(k, _)| k == kind)
}

/// The slot a world call stacks its predecessor in.
fn world_below(kind: &str) -> Option<&'static str> {
    if is_world_kind(kind) { Some("below") } else { None }
}

/// The layers (bottom first), each on the one below; ids come from `previous` by home, else by
/// label.
fn layer_graph(previous: Option<&kg::T>, homes: &[(i64, Home)], layers: &[Call]) -> R<(kg::T, Vec<(i64, Home)>)> {
    let empty = kg::empty();
    let previous = previous.unwrap_or(&empty);
    let mut graph = kg::empty();
    let mut used: Vec<i64> = Vec::new();
    let mut below: Option<i64> = None;
    let mut made: Vec<(i64, Home)> = Vec::new();
    for call in layers {
        let kind = world_kind(&call.kind);
        let label = label_arg(&call.args).unwrap_or_else(|| kind.label.clone());
        let id = find_claim(previous, &used, &kind.key, &call.home, &label, homes);
        let values = changes(&call.kind, &call.args)?;
        let node = instance(id, &kind, &label, &values)?;
        let node = kg::Node { input: below, ..node };
        graph = kg::add(&graph, node.clone());
        used.push(node.id);
        below = Some(node.id);
        made.push((node.id, call.home.clone()));
    }
    Ok((graph, made))
}

// ---- the document of a workspace ----

/// A network the previous document already has, field for field, is kept physically: a gesture
/// that moves a scene object does not look like an edit of its geometry, so nothing re-prepares
/// or recooks.
fn same_network(a: &document::Network, b: &document::Network) -> bool {
    let same = |a: &edit_graph::NodeInfo, b: &edit_graph::NodeInfo| -> bool {
        a.id == b.id
            && a.operation == b.operation
            && a.label == b.label
            && a.inputs == b.inputs
            && (sop::node::ptr_eq(&a.node, &b.node)
                // a kernel node (an inlined card's body) has no fields: its identity is its key
                || (a.parameters == b.parameters
                    && sop::node::parameter_fields(&a.node).iter().map(|f| (&f.name, &f.current)).collect::<Vec<_>>()
                        == sop::node::parameter_fields(&b.node).iter().map(|f| (&f.name, &f.current)).collect::<Vec<_>>()))
    };
    a.context == b.context
        && a.displayed == b.displayed
        && (net::ptr_eq(&a.graph, &b.graph) || (a.graph.drives.is_empty() && b.graph.drives.is_empty()))
        && edit_graph::root(&a.graph.geometry) == edit_graph::root(&b.graph.geometry)
        && (edit_graph::ptr_eq(&a.graph.geometry, &b.graph.geometry) || {
            let (ia, ib) = (edit_graph::inspect(&a.graph.geometry), edit_graph::inspect(&b.graph.geometry));
            ia.len() == ib.len() && ia.iter().zip(ib.iter()).all(|(x, y)| same(x, y))
        })
}

fn same_settings(a: &settings::T, b: &settings::T) -> bool {
    let values = |s: &settings::T| -> Vec<(String, PV)> { settings::fields(s).into_iter().map(|f| (f.name, f.current)).collect() };
    values(a) == values(b)
}

fn same_gpu(a: &gpu::Config, b: &gpu::Config) -> bool {
    a.fast == b.fast
        && a.policy == b.policy
        && match (&a.backend, &b.backend) {
            (None, None) => true,
            (Some(x), Some(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
}

fn same_calls(a: &[Call], b: &[Call]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.kind == y.kind && x.args == y.args)
}

fn objects_only(all: Vec<Call>) -> Vec<Call> {
    all.into_iter().filter(|c| c.kind != "scene/root" && c.kind != "scene/world").collect()
}

/// One geometry object per geometry item beside the sketch's own objects, the World of a world
/// graph, the settings of a settings graph. `previous` keeps object ids (matched by home, then by
/// operation and label), tile layouts and the objects the workspace does not declare (the host's
/// camera and lights): the workspace owns geometry always, every object when it has a scene graph
/// (an empty one means none), and the World when a `scene/world` references a world graph.
/// Whatever a declared object's text does not say is the schema's default, so the text is the
/// whole truth of it.
pub fn of_workspace(options: &Options, workspace: &workspace_doc::T) -> R<document::T> {
    let _phase = flow_syntax::phase_timer::measure("document.lower");
    flow_ir::fallback::reset();
    let previous = options.previous.as_ref();
    let defer = options.defer.unwrap_or(true);
    let factories: Vec<Factory> = options.factories.clone().unwrap_or_else(sop::nodes::factories);
    let mut gpu = options.gpu.clone().unwrap_or_else(|| match previous {
        Some(doc) => doc.workspace.lowered.gpu.clone(),
        None => gpu::default(),
    });
    // A device's SOP writes follow the workspace precision (fast unless the settings say exact):
    // the previous document's, or fast; a first lowering that reads exact is lowered again.
    if gpu.backend.is_some() {
        gpu.fast = match previous {
            Some(doc) => fast(doc),
            None => true,
        };
    }
    let (compiled_ids, sites) = match previous {
        Some(doc) => (doc.workspace.lowered.compiled_ids.clone(), doc.workspace.lowered.sites.clone()),
        None => (Default::default(), Vec::new()),
    };
    let old_homes = match previous {
        Some(doc) => doc.homes.clone(),
        None => document::no_homes(),
    };
    let previous_literal: Option<R<lower_mod::T>> = previous.and_then(|doc| {
        let old = &doc.workspace.ws;
        let lowered = &doc.workspace.lowered;
        let changes = if same_gpu(&gpu, &lowered.gpu) && lowered.defer == defer { workspace_doc::literal_changes(old, workspace) } else { None }?;
        let mut result: Option<R<lower_mod::T>> = Some(Ok((**lowered).clone()));
        for change in &changes {
            result = match result {
                None => None,
                Some(Err(e)) => Some(Err(e)),
                Some(Ok(l)) => literal_edit::lower(&l, change),
            };
        }
        result
    });
    let lowered = match previous_literal {
        Some(result) => result?,
        None => {
            let lower = |gpu: &gpu::Config| -> R<lower_mod::T> {
                let o = lower_mod::Options {
                    record: options.record.unwrap_or(true),
                    gpu: gpu.clone(),
                    defer,
                    compiled_ids: compiled_ids.clone(),
                    sites: sites.clone(),
                    inputs: Some((*workspace.inputs).clone()),
                    ..Default::default()
                };
                lower_mod::of_checked(Some(factories.clone()), o, &workspace.checked)
            };
            let lowered = lower(&gpu)?;
            match (gpu.backend.is_some(), plan_window(workspace, &lowered.plan)) {
                (true, Ok(window)) if window.fast != gpu.fast => lower(&gpu::Config { fast: window.fast, ..gpu })?,
                _ => lowered,
            }
        }
    };
    let workspace: workspace_doc::T = if workspace.checked.approx == lowered.approx && workspace.checked.approx_reasons == lowered.approx_reasons {
        workspace.clone()
    } else {
        let mut checked = (*workspace.checked).clone();
        checked.approx = lowered.approx.clone();
        checked.approx_reasons = lowered.approx_reasons.clone();
        workspace_doc::T { checked: Arc::new(checked), ..workspace.clone() }
    };
    let workspace = &workspace;
    check_context_time(workspace, &lowered.plan)?;
    check_scene(workspace, &lowered.plan)?;
    let (items, root) = items(workspace, &lowered)?;
    // a viewport over another instance of the scene draws objects of its own
    let editor = shell_eval::editor(workspace, &lowered.plan)?;
    let default_scene: Option<E> = workspace_doc::context_value(false, workspace, &lowered.plan, context::scene())?;
    let viewports: Vec<(String, E)> = editor.as_ref().map_or(Vec::new(), |e| e.viewports.clone());
    let is_default = |scene: &E| default_scene.as_ref().map_or(false, |d| d == scene);
    // a viewport's own scene draws its objects; its root and World are not the document's
    let default_calls = objects_only(default_scene.as_ref().map_or(Vec::new(), scene_calls));
    let mut aux: Vec<(String, Vec<Item>)> = Vec::new();
    for (key, scene) in viewports.iter().rev() {
        let calls = objects_only(scene_calls(scene));
        if is_default(scene) || same_calls(&calls, &default_calls) {
            continue;
        }
        let mut extra: Vec<Item> = Vec::new();
        for call in calls.iter().rev() {
            let item = item_of(&lowered, call)?;
            extra.push(Item { label: format!("{} ({})", item.label, key), group: Some(key.clone()), ..item });
        }
        extra.reverse();
        aux.push((key.clone(), extra));
    }
    aux.reverse();
    // a viewport over an instance that names another World shows that one (or none)
    let world_calls = |scene: &E| -> Vec<Call> { scene_calls(scene).into_iter().filter(|c| c.kind == "scene/world").collect() };
    let default_worlds = default_scene.as_ref().map_or(Vec::new(), world_calls);
    let mut view_worlds: Vec<(String, Option<ViewWorld>)> = Vec::new();
    for (key, scene) in viewports.iter().rev() {
        let own = world_calls(scene);
        if default_scene.is_none() || is_default(scene) || same_calls(&own, &default_worlds) {
            continue;
        }
        match own.first() {
            None => view_worlds.push((key.clone(), None)),
            Some(call) => {
                let item = item_of(&lowered, call)?;
                // the instance is a value: the world graph it references is the call's `world` argument
                let stack = match call.args.iter().find(|(n, _)| n == "world") {
                    Some((_, layers)) => loose(&is_world_kind, &world_below, layers),
                    None => Vec::new(),
                };
                let node = instance(None, &item.kind_of, &item.label, &item.values)?;
                let (layers, _) = layer_graph(None, &[], &stack)?;
                view_worlds.push((key.clone(), Some(ViewWorld { node, layers })));
            }
        }
    }
    view_worlds.reverse();
    // a viewport over another scene instance that has a root renders as that root says
    let view_roots: Vec<(String, ViewRoot)> = viewports.iter().filter_map(|(key, scene)| if is_default(scene) { None } else { instance_root(scene).map(|r| (key.clone(), r)) }).collect();
    let primary = items.len();
    let mut items = items;
    for (_, extra) in &aux {
        items.extend(extra.iter().cloned());
    }
    let scene = match previous {
        Some(doc) => doc.scene.clone(),
        None => kg::empty(),
    };
    // a scene graph is authoritative for every object kind: what it does not say is not there
    // (no camera, no lights), and the host seeds nothing
    let has_scene = graph_ref(workspace, context::scene()).is_some();
    // the World is a merge member (`scene/world`)
    let scene_world: Option<Item> = items.iter().find(|item| item.group.is_none() && item.kind_of.key == "world").cloned();
    let owned = |operation: &str| operation == "geometry" || has_scene;
    // claim or create a node per item, then drop the owned nodes nothing claimed
    let mut graph = scene;
    let mut used: Vec<i64> = Vec::new();
    let mut objects_: Vec<(i64, Item)> = Vec::new();
    for item in &items {
        let home = if item.group.is_none() { item.home.clone() } else { Home::Looped };
        match find_claim(&graph, &used, &item.kind_of.key, &home, &item.label, &old_homes.objects).and_then(|id| kg::find(&graph, id)) {
            Some(node) => {
                let claimed = with_values(&item.kind_of, &kg::Node { label: item.label.clone(), ..node.clone() }, &item.values)?;
                // Updating the label allocates a distinct claimed node.
                graph = kg::replace(&graph, &claimed);
                used.push(node.id);
                objects_.push((node.id, item.clone()));
            }
            None => {
                let node = instance(None, &item.kind_of, &item.label, &item.values)?;
                graph = kg::add(&graph, node.clone());
                used.push(node.id);
                objects_.push((node.id, item.clone()));
            }
        }
    }
    let mut views: Vec<(String, Vec<i64>)> = Vec::new();
    let mut from = primary;
    for (key, extra) in &aux {
        let n = extra.len();
        views.push((key.clone(), objects_.iter().enumerate().filter(|(i, _)| *i >= from && *i < from + n).map(|(_, (id, _))| *id).collect()));
        from += n;
    }
    let stale: Vec<i64> = graph.iter().filter(|node| owned(&node.kind) && !used.contains(&node.id)).map(|n| n.id).collect();
    if !stale.is_empty() {
        graph = kg::remove(&stale, &graph);
    }
    let graph = link_parents(graph, &objects_)?;
    // the World node
    let (world_id, world_network, layer_homes): (Option<i64>, Option<kg::T>, Vec<(i64, Home)>) = match &scene_world {
        Some(_) => {
            // the node the objects made; its layers are the referenced world graph's
            let (id, item) = objects_.iter().find(|(_, item)| item.group.is_none() && item.kind_of.key == "world").expect("the world item made an object");
            let named = item.via.clone().ok_or_else(|| diag("E_LOWER", "A scene/world references a world graph: (scene/world (ref sky))."))?;
            let stack = match graph_of_name(workspace, &named) {
                Some(g) => graph_calls(&is_world_kind, &world_below, &lowered.plan, context::world(), g)?,
                None => return Err(diag("E_LOWER", &format!("scene/world references {named}, which is not a world graph."))),
            };
            let prev_layers = previous.and_then(|doc| document::layers(doc, *id));
            let (layers, made) = layer_graph(prev_layers, &old_homes.layers, &stack)?;
            (Some(*id), Some(layers), made)
        }
        None => (None, None, Vec::new()),
    };
    // an edit that leaves the scene's objects as they were (a root's settings, a splitter, a rename
    // elsewhere) keeps the scene physically, so nothing recomposes, nothing re-uploads and no
    // accumulating render restarts
    let scene = match previous {
        Some(doc) if kg::same(&doc.scene, &graph) => doc.scene.clone(),
        _ => graph.clone(),
    };
    // networks: geometry objects' lowered graphs and the previous networks of objects that survive;
    // layers: the World's, and the previous layers of a World that survives
    let alive = |id: i64| kg::find(&graph, id).is_some() && !objects_.iter().any(|(i, _)| *i == id) && Some(id) != world_id;
    let mut networks: document::IntMap<document::Network> = match previous {
        None => document::IntMap::new(),
        Some(doc) => doc.networks.iter().filter(|(id, _)| alive(**id)).map(|(k, v)| (*k, v.clone())).collect(),
    };
    for (id, item) in &objects_ {
        if item.kind_of.key != "geometry" {
            continue;
        }
        let network = match &item.geometry {
            Some(g) => document::Network { context: context::sop(), graph: g.network.clone(), displayed: g.root },
            None => document::Network { context: context::sop(), graph: net::of_geometry(&edit_graph::empty()), displayed: None },
        };
        networks.insert(*id, network);
    }
    let mut layers: document::IntMap<kg::T> = match previous {
        None => document::IntMap::new(),
        Some(doc) => doc.layers.iter().filter(|(id, _)| alive(**id)).map(|(k, v)| (*k, v.clone())).collect(),
    };
    if let (Some(id), Some(world)) = (world_id, world_network) {
        let kept = previous.and_then(|doc| document::layers(doc, id)).filter(|old| kg::same(old, &world)).cloned();
        layers.insert(id, kept.unwrap_or(world));
    }
    if let Some(doc) = previous {
        for (id, network) in networks.iter_mut() {
            if let Some(old) = doc.networks.get(id) {
                if same_network(old, network) {
                    *network = old.clone();
                }
            }
        }
    }
    let networks = match previous {
        Some(doc) if doc.networks.len() == networks.len() && networks.iter().all(|(id, n)| doc.networks.get(id).is_some_and(|old| same_network(old, n))) => doc.networks.clone(),
        _ => Arc::new(networks),
    };
    let settings_calls = calls(&|k| k == "settings/config", &no_below, workspace, &lowered.plan, context::settings())?;
    let settings = match settings_calls.first() {
        Some(Call { kind, args, .. }) => settings_of(&struct_value(kind.clone(), ty::settings(), args.clone()))?,
        None => workspace.settings.clone(),
    };
    // settings the previous document already has are kept physically, like a network: the cook
    // and every prepared piece are keyed by them, so a camera move must not look like an edit
    let settings = match previous {
        Some(doc) if settings::same_values(&doc.settings, &settings) || same_settings(&doc.settings, &settings) => doc.settings.clone(),
        _ => settings,
    };
    let cameras: Vec<i64> = objects::ids("camera", &graph).into_iter().filter(|id| !views.iter().any(|(_, ids)| ids.contains(id))).collect();
    let active_camera = match objects_.iter().find(|(id, item)| item.active && item.group.is_none() && cameras.contains(id)) {
        Some((id, _)) => Some(*id),
        None if has_scene => cameras.first().copied(),
        None => match previous.and_then(|doc| doc.active_camera) {
            Some(id) if cameras.contains(&id) => Some(id),
            _ => None,
        },
    };
    let workspace: workspace_doc::T = if has_settings(workspace) { workspace.clone() } else { workspace_doc::T { settings: settings.clone(), ..workspace.clone() } };
    let homes = document::Homes {
        objects: objects_.iter().map(|(id, item)| (*id, if item.group.is_none() { item.home.clone() } else { Home::Looped })).collect(),
        world: scene_world.as_ref().map(|item| item.home.clone()),
        layers: layer_homes,
        root: root.root_home.clone(),
        world_graph: scene_world.as_ref().and_then(|item| item.via.clone()),
        settings: settings_calls.first().map(|c| c.home.clone()),
    };
    let mut scene_drives: document::IntMap<(String, Vec<(port::Parameter, E)>)> = document::IntMap::new();
    for (id, item) in &objects_ {
        if let Some(drive) = &item.drives {
            scene_drives.insert(*id, drive.clone());
        }
    }
    let mut scene_textures: document::IntMap<E> = document::IntMap::new();
    for (id, item) in &objects_ {
        if let Some(texture) = &item.texture {
            scene_textures.insert(*id, texture.clone());
        }
    }
    let shell = editor.map(|e| document::Shell {
        tree: e.tree,
        origins: e.origins,
        named: e.named,
        repeated: e.repeated,
        follows: e.follows,
        start: e.start,
        views,
        canvases: e.canvases,
        preview_sources: e.preview_sources,
        switch: e.switch,
    });
    let (fast_ws, fast_plan) = (workspace.clone(), lowered.plan.clone());
    let fast = document::LazyBool::new(Arc::new(move || match plan_window(&fast_ws, &fast_plan) {
        Ok(w) => w.fast,
        Err(_) => true,
    }));
    Ok(document::T {
        scene,
        networks,
        layers,
        active_camera,
        root: root.params,
        settings,
        shell: shell.map(Arc::new),
        view_worlds,
        view_roots,
        homes,
        scene_drives,
        scene_textures,
        workspace: document::Workspace {
            ws: workspace,
            lowered: Arc::new(lowered),
            fast,
        },
    })
}

/// An undo entry live again: the entry's text lowered under the ids it had, on top of the document
/// now, so what did not change stays physically. What the text does not say comes from the entry.
pub fn restore(factories: Option<&[Factory]>, current: &document::T, saved: &document::Snapshot) -> R<document::T> {
    let mut previous = current.clone();
    previous.scene = saved.saved_scene.clone();
    previous.layers = saved.saved_layers.clone();
    previous.active_camera = saved.saved_camera;
    previous.homes = saved.saved_homes.clone();
    let lowered = Arc::make_mut(&mut previous.workspace.lowered);
    lowered.compiled_ids = saved.saved_ids.clone();
    lowered.sites = saved.saved_sites.clone();
    let options = Options { factories: factories.map(|f| f.to_vec()), previous: Some(previous), ..Default::default() };
    let mut doc = of_workspace(&options, &saved.saved_ws)?;
    doc.scene = saved.saved_scene.clone();
    doc.layers = saved.saved_layers.clone();
    doc.active_camera = saved.saved_camera;
    doc.homes = saved.saved_homes.clone();
    if same_settings(&doc.settings, &saved.saved_settings) {
        doc.settings = saved.saved_settings.clone();
    }
    Ok(doc)
}

/// Only the recorded live fields run; SOP networks, source and panel identities stay untouched.
/// Errors leave the caller's last successful picture available.
pub fn scene_reads(doc: &document::T) -> sop::context::dependencies::T {
    use sop::context::dependencies as d;
    let mut found = d::STATIC;
    for (_, args) in doc.scene_drives.values() {
        for (_, value) in args {
            found = d::union(found, flow_sop::attribute_kernel::input_reads(value));
        }
    }
    found
}

pub fn resolve_scene(previous: Option<&kg::T>, state: Option<&flow_eval::State>, live: Option<&input::Frame>, doc: &document::T, time: f64) -> (kg::T, Vec<Diagnostic>) {
    let live_frame = match live {
        Some(l) => l.clone(),
        None => input::at_time(time),
    };
    let mut graph = doc.scene.clone();
    let mut errors: Vec<Diagnostic> = Vec::new();
    for (id, (kind, args)) in &doc.scene_drives {
        let resolve = || -> R<kg::T> {
            let mut values: Vec<(String, PV)> = Vec::new();
            for (parameter, value) in args {
                let forced = flow_eval::force(value, &live_frame, state)?;
                values.extend(lower_mod::changes(parameter, &forced)?);
            }
            flow(kg::set_fields(&graph, *id, &values)).map(|x| x.0)
        };
        match resolve() {
            Ok(next) => graph = next,
            Err(error) => {
                if let Some(node) = previous.and_then(|p| kg::find(p, *id)) {
                    graph = kg::replace(&graph, &node);
                }
                let views: Vec<String> = doc.shell.as_ref().map_or(Vec::new(), |shell| shell.views.iter().filter(|(_, ids)| ids.contains(id)).map(|(key, _)| key.clone()).collect());
                let view = if views.is_empty() { "primary".to_owned() } else { views.join(", ") };
                errors.push(diag(
                    "E_CONTEXT_LIVE",
                    &format!(
                        "{} #{} [{}] ({}): {} (last successful light retained)",
                        kind,
                        id,
                        view,
                        args.iter().map(|(p, _)| p.path.clone()).collect::<Vec<_>>().join(", "),
                        rays_diagnostic::to_string(&error)
                    ),
                ));
            }
        }
    }
    (graph, errors)
}


const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

/// Lower-case hex SHA-256: the digest of a sketch source.
pub fn sha256(text: &str) -> String {
    let mut h: [u32; 8] = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19];
    let mut data = text.as_bytes().to_vec();
    let bits = (data.len() as u64).wrapping_mul(8);
    data.push(0x80);
    while data.len() % 64 != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bits.to_be_bytes());
    for chunk in data.chunks(64) {
        let mut m = [0u32; 64];
        for i in 0..16 {
            m[i] = u32::from_be_bytes([chunk[4 * i], chunk[4 * i + 1], chunk[4 * i + 2], chunk[4 * i + 3]]);
        }
        for i in 16..64 {
            let s0 = m[i - 15].rotate_right(7) ^ m[i - 15].rotate_right(18) ^ (m[i - 15] >> 3);
            let s1 = m[i - 2].rotate_right(17) ^ m[i - 2].rotate_right(19) ^ (m[i - 2] >> 10);
            m[i] = m[i - 16].wrapping_add(s0).wrapping_add(m[i - 7]).wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] = h;
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ (!e & g);
            let t1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(K[i]).wrapping_add(m[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let t2 = s0.wrapping_add(maj);
            hh = g;
            g = f;
            f = e;
            e = d.wrapping_add(t1);
            d = c;
            c = b;
            b = a;
            a = t1.wrapping_add(t2);
        }
        for (x, y) in h.iter_mut().zip([a, b, c, d, e, f, g, hh]) {
            *x = x.wrapping_add(y);
        }
    }
    h.iter().map(|x| format!("{x:08x}")).collect()
}
