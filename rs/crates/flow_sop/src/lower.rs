//! Port of lib/flow_sop/lower.ml: lowering of a checked workspace to SOP networks.
//!
//! The OCaml `Lowerer` functor becomes the `Lowerer` struct (one per `lower_checked` call): its
//! module-level `let`s are fields, its mutable tables are `Mutex`es, and the closures it hands to
//! nodes capture an `Arc<Lowerer>`. OCaml exceptions (`Diagnostic.Error`) are `Result`s.
use crate::{
    attribute_kernel as ak, attribute_metadata as am, catalog, context_kernel as ck, curve,
    encoded_parameters as ep, function_kernel as fk, group_kernel as gk, lisp_node as ln, network,
    operators as ops, port, topology_kernel as tk, topology_kernel_impl as tki, value_lane as vl,
};
use flow_check::catalog::Slot;
use flow_check::workspace::{self as W, Graph as WGraph, NodeDefinition, Term, Workspace};
use flow_eval::{self as E, Instance, Node as ENode, Plan, State, Value};
use input::Frame;
use flow_ir::flow_ir::{Cost, Executor, Gpu, Placement, PlacementReason, Profile, Sink};
use flow_syntax::context as fctx;
use flow_syntax::syntax::{self, Node as SNode, Syntax};
use flow_syntax::ty::{self, FnSignature, Ty};
use flow_syntax::value::{FnId, ResidualId};
use rays_diagnostic::Diagnostic;
use sop::context::{self as sctx, dependencies as deps_mod};
use sop::edit_graph::{self as edit, Factory, InputRequirement};
use sop::node::{self, build, CookMode, Elementwise, Facts, InputUse, Topology};
use sop::payload;
use sop::zone as szone;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex, OnceLock, Weak};

type R<T> = Result<T, Diagnostic>;
pub type PathMap = BTreeMap<Vec<i64>, i64>;
pub type Clock = Arc<dyn Fn() -> f64 + Send + Sync>;
pub type OnProfile = Arc<dyn Fn(&Arc<Profile::T>) + Send + Sync>;

/// `Rdk.Mesh_merge.provenance_attribute`: the primitive int attribute every collecting merge writes.
pub const SOURCE_ATTRIBUTE: &str = "__flow_src";
pub fn source_attribute() -> &'static str {
    SOURCE_ATTRIBUTE
}

/// Serial for host-call nodes: they are cooked once in a session that keeps no cache entries, so
/// their arguments need no content key.
static HOST_SERIAL: AtomicI64 = AtomicI64::new(0);

#[derive(Clone, Debug)]
pub struct Pending {
    pub node: i64,
    pub field: String,
    pub value: Value,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Origin {
    pub merge: i64,
    pub input: i64,
    pub source: i64,
    pub site: W::Path,
    pub iter: Vec<i64>,
}

#[derive(Clone)]
pub struct Zone {
    pub cid: i64,
    pub site: W::Path,
    pub iter: Vec<i64>,
    pub body_site: W::Path,
    pub base: i64,
    pub count: Arc<AtomicI64>,
    pub ekey: Option<String>,
    pub positions: Arc<Mutex<Arc<Vec<(f64, f64, f64)>>>>,
}

#[derive(Clone)]
pub struct Graph {
    pub name: String,
    /// index in `Plan.instances`
    pub instance: usize,
    /// the graph with its own default inputs, not a `ref` override
    pub default: bool,
    pub network: network::Network,
    /// displayed node (the graph result); `None` for `nil`
    pub root: Option<i64>,
}

pub type PreviewFn = Arc<dyn Fn(i64, &[i64], &network::Network) -> Option<(network::Network, i64)> + Send + Sync>;
pub type CardNodeFn = Arc<dyn Fn(i64) -> Result<node::T, String> + Send + Sync>;

#[derive(Clone)]
pub struct T {
    pub gpu: Gpu::Config,
    pub defer: bool,
    pub graphs: Vec<Graph>,
    pub factories: Vec<Factory>,
    pub compiled_ids: PathMap,
    pub sites: Vec<W::Path>,
    pub compiled: BTreeMap<i64, i64>,
    pub pending: Vec<Pending>,
    pub provenance: BTreeMap<i64, Origin>,
    pub zones: Vec<Zone>,
    pub volatile: BTreeMap<i64, ()>,
    pub plan: Plan,
    pub image_sites: Vec<Option<W::Path>>,
    pub reads: Arc<std::sync::LazyLock<deps_mod::T, Box<dyn FnOnce() -> deps_mod::T + Send + Sync>>>,
    pub states: Vec<Value>,
    pub evaluated: Arc<E::Eval>,
    pub approx: W::Paths,
    pub approx_reasons: Vec<(W::Path, Vec<Diagnostic>)>,
    pub profile: Arc<Profile::T>,
    pub preview: PreviewFn,
    pub card_node: CardNodeFn,
    /// The lowering behind `preview` and `card_node`; weak, so a test can see it freed once every `T` clone is gone.
    pub lowering: Weak<Lowerer>,
}

fn fail(code: &str, message: &str) -> Diagnostic {
    port::error(code, message)
}
thread_local! {
    static LAST_BUILD_FAILURE: std::cell::RefCell<Option<Diagnostic>> = const { std::cell::RefCell::new(None) };
}
/// A factory build fails with text only (`Edit_graph` factories return `Result<_, string>`); OCaml raises
/// the diagnostic through them, so remember it here and let `edit_r` hand it back (code and span kept).
// ponytail: one slot per thread, matched by message; a build and its `edit_r` run on one thread.
fn keep_diagnostic(d: Diagnostic) -> String {
    let message = d.message.clone();
    LAST_BUILD_FAILURE.with(|slot| *slot.borrow_mut() = Some(d));
    message
}
fn edit_r<A>(r: Result<A, String>) -> R<A> {
    r.map_err(|message| LAST_BUILD_FAILURE.with(|slot| slot.borrow_mut().take()).filter(|d| d.message == message).unwrap_or_else(|| fail("E_LOWER", &message)))
}
/// A geometry lookup by plan id over the cooked inputs of a node.
fn lookup(geometries: Vec<rdk::geometry::T>, sources: Vec<i64>) -> ak::GeometryLookup {
    Arc::new(move |id: i64| sources.iter().position(|s| *s == id).map(|i| geometries[i].clone()))
}
fn key_text(v: &Value) -> String {
    txt(&flow_syntax::value::key_of(&|r: ResidualId| E::Residual::residual_id(r), v))
}
fn txt(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}
fn text_arg(args: &[(String, Value)], key: &str) -> Option<String> {
    match get(args, key) {
        Some(Value::Text(t)) => Some(txt(t)),
        _ => None,
    }
}
fn get<'a>(args: &'a [(String, Value)], key: &str) -> Option<&'a Value> {
    args.iter().find(|(k, _)| k == key).map(|(_, v)| v)
}
fn has(args: &[(String, Value)], key: &str) -> bool {
    args.iter().any(|(k, _)| k == key)
}
fn without(args: &[(String, Value)], key: &str) -> Vec<(String, Value)> {
    args.iter().filter(|(k, _)| k != key).cloned().collect()
}
fn is_geometry_ty(t: &Ty) -> bool {
    *t == ty::geometry()
}
fn dep_union(a: deps_mod::T, b: deps_mod::T) -> deps_mod::T {
    deps_mod::union(a, b)
}

#[derive(Clone)]
pub struct ImageContext {
    pub compiled: BTreeMap<i64, i64>,
    pub network: network::Network,
}

pub type ImageResolver = Arc<
    dyn Fn(Option<&Gpu::Config>, Option<&ImageContext>, &Plan, &State, &Frame, &Value) -> Result<sop::image::T, Diagnostic>
        + Send
        + Sync,
>;
pub type ImageMetadata = Arc<dyn Fn(&Plan, i64) -> Option<(i64, i64)> + Send + Sync>;

#[derive(Clone)]
struct Images {
    resolve: ImageResolver,
    metadata: ImageMetadata,
}

thread_local! {
    static IMAGE_PROVIDER: RefCell<Option<Images>> = const { RefCell::new(None) };
}

/// Scope an initial-domain resource resolver around value-lane resolution.
pub fn with_images(gpu: Option<Gpu::Config>, metadata: Option<ImageMetadata>, resolver: ImageResolver, run: &mut dyn FnMut()) {
    let metadata: ImageMetadata = metadata.unwrap_or_else(|| Arc::new(|_, _| None));
    let wrapped: ImageResolver = Arc::new(move |requested, context, plan, state, live, value| {
        let used = requested.cloned().or_else(|| gpu.clone());
        resolver(used.as_ref(), context, plan, state, live, value)
    });
    let previous = IMAGE_PROVIDER.with(|p| p.borrow_mut().replace(Images { resolve: wrapped, metadata }));
    run();
    IMAGE_PROVIDER.with(|p| *p.borrow_mut() = previous);
}

pub fn image_metadata(plan: &Plan, node: i64) -> Option<(i64, i64)> {
    IMAGE_PROVIDER.with(|p| p.borrow().as_ref().and_then(|images| (images.metadata)(plan, node)))
}

fn image_provider() -> Option<Images> {
    IMAGE_PROVIDER.with(|p| p.borrow().clone())
}

fn resource_image(operation: &str, result: Result<sop::image::T, Diagnostic>) -> node::T {
    let parameters = match &result {
        Ok(image) => sop::image::data_id(image).to_string().into_bytes(),
        Err(_) => b"unbound".to_vec(),
    };
    build::make(
        build::Args { parameters, cook_mode: CookMode::Generator, ..build::args(operation) },
        None,
        Arc::new(move |_, _, _| match &result {
            Ok(image) => Ok(node::Cooked { payload: payload::T::Image(image.clone()), instances: None }),
            Err(d) => Err(d.clone()),
        }),
    )
}

#[derive(Clone)]
pub struct KernelInput {
    pub index: usize,
    pub cid: i64,
    pub identity: i64,
    pub signature: FnSignature,
    pub fun: FnId,
    pub sources: Vec<i64>,
}

#[derive(Clone)]
pub struct Zinfo {
    pub plan_id: i64,
    pub lo: i64,
    pub hi: i64,
    pub preview: Option<(i64, Vec<i64>)>,
    pub root: i64,
    pub order: Vec<i64>,
    pub live: bool,
    /// the input slices the zone's arguments read
    pub reads: deps_mod::T,
    pub stateful: bool,
    pub captures: Vec<i64>,
    pub ekey: Option<String>,
    pub kind: szone::Kind,
    pub key: Option<String>,
    pub base: i64,
    pub count: Arc<AtomicI64>,
    pub positions: Arc<Mutex<Arc<Vec<(f64, f64, f64)>>>>,
}

pub type Dynamic = (Value, Arc<dyn Fn(&Value) -> R<Vec<(String, param::Value)>> + Send + Sync>);

#[derive(Clone)]
pub struct Prepared {
    pub cid: i64,
    pub factory: Factory,
    /// slot index, source plan id
    pub slots: Vec<(usize, i64)>,
    pub arity: usize,
    pub changes: Vec<(String, param::Value)>,
    /// a template node: arguments that read the element, forced for each one
    pub dynamic: Vec<Dynamic>,
    pub zone: Option<Zinfo>,
    pub kernels: Vec<KernelInput>,
}

fn hex(text: &str) -> R<Value> {
    match port::color_of_text(text) {
        Some((r, g, b)) => Ok(Value::Vec3(r, g, b)),
        None => Err(fail("E_LOWER", &format!("Bad colour {text}"))),
    }
}

fn typed(value: &Value) -> R<flow_check::port_type::PortValue> {
    use flow_check::port_type::PortValue;
    match value {
        Value::Int(n) => Ok(PortValue::Int(*n)),
        Value::Float(f) => Ok(PortValue::Float(*f)),
        Value::Bool(b) => Ok(PortValue::Bool(*b)),
        Value::Vec3(x, y, z) => Ok(PortValue::Vec3(*x, *y, *z)),
        _ => Err(fail("E_LOWER", "Argument is not a number, bool or vec3")),
    }
}

pub fn changes_of(parameter: &port::Parameter, value: &Value) -> R<Vec<(String, param::Value)>> {
    match (&parameter.ty, value) {
        (None, Value::Text(text)) => {
            let literal = match parameter.fields.as_slice() {
                [f] if matches!(f.kind, param::KindView::Choice(_)) => param::Value::Choice(txt(text)),
                _ => param::Value::Text(text.clone()),
            };
            port::literal_changes(parameter, &port::Literal::Scalar(literal))
        }
        (None, _) => Err(fail("E_LOWER", &format!("Port {} takes text", parameter.path))),
        (Some(_), Value::Text(text)) => Ok(port::normalize(parameter, &typed(&hex(&txt(text))?)?)?.1),
        (Some(_), value) => {
            let finite = match value {
                Value::Float(f) => f.is_finite(),
                Value::Vec3(x, y, z) => x.is_finite() && y.is_finite() && z.is_finite(),
                _ => true,
            };
            if !finite {
                return Err(fail("E_LOWER", &format!("Port {} takes a finite number", parameter.path)));
            }
            Ok(port::normalize(parameter, &typed(value)?)?.1)
        }
    }
}

/// The field values a catalog parameter takes from an evaluated argument (colour text, number,
/// vec3, choice or text), hard bounds applied.
pub fn changes(parameter: &port::Parameter, value: &Value) -> R<Vec<(String, param::Value)>> {
    changes_of(parameter, value)
}

/// sop/material takes a material value as its fields.
fn material_args(kind: &str, args: Vec<(String, Value)>) -> Vec<(String, Value)> {
    if kind != "sop/material" {
        return args;
    }
    let mut out = Vec::new();
    for arg in args {
        match &arg {
            (name, Value::Struct(s, _, fields)) if name == "material" && s == "material/standard" => {
                let field = |n: &str, default: Value| get(fields, n).cloned().unwrap_or(default);
                out.push(("material".into(), field("name", Value::Text(Vec::new()))));
                out.push(("color".into(), field("color", Value::Vec3(1., 1., 1.))));
                out.push(("roughness".into(), field("roughness", Value::Float(0.4))));
                out.push(("emission".into(), field("emission", Value::Vec3(0., 0., 0.))));
            }
            _ => out.push(arg),
        }
    }
    out
}

/// A library definition's cone digest is kept by kind and first form id: library forms have fixed
/// ids and the library is part of the binary. An authored one is digested afresh.
static LIBRARY_KEYS: Mutex<BTreeMap<(String, i64), String>> = Mutex::new(BTreeMap::new());

fn digest(text: &str) -> String {
    // the OCaml Digest is MD5; only equality of keys matters here, never the bytes
    let mut h: u64 = 0xcbf29ce484222325;
    let mut g: u64 = 0x9e3779b97f4a7c15;
    for b in text.bytes() {
        h = (h ^ b as u64).wrapping_mul(0x100000001b3);
        g = (g.rotate_left(5) ^ b as u64).wrapping_mul(0xff51afd7ed558ccd);
    }
    format!("{h:016x}{g:016x}")
}

pub fn declaration_key(checked: &Workspace, d: &NodeDefinition) -> String {
    let compute = || digest(&ck::declaration_source(checked, d));
    let id = d.body.body.form.id;
    if id < catalog::library_base() {
        return compute();
    }
    let key = (d.kind.qualified.clone(), id);
    if let Some(found) = LIBRARY_KEYS.lock().unwrap().get(&key) {
        return found.clone();
    }
    let value = compute();
    LIBRARY_KEYS.lock().unwrap().insert(key, value.clone());
    value
}


fn empty_form() -> Syntax {
    syntax::make(SNode::List(Vec::new()))
}

/// An inlined Lisp call's parameters take the node path: the call's changes over the schema
/// defaults, then the node's arguments. The body's context operators carry the card's cook label.
fn inline_policy(checked: &Workspace, d: &NodeDefinition) -> E::InlineParameterPolicy {
    let normalizers = Arc::new(ep::for_node(checked, d));
    let key = d.kind.qualified[4..].to_owned();
    let (body, context_inputs) = ck::label_context(&key, &d.body);
    let mut definition = d.clone();
    definition.body = body.clone();
    let checked_c = checked.clone();
    let d_c = d.clone();
    let source_key = declaration_key(checked, d);
    let canonicalize: E::InlineParameterCanonicalizer = Arc::new(move |vals: &[(String, Value)]| {
        let d = &d_c;
        let checked = &checked_c;
        let schema = ln::schema("parameters", &d.fields, &normalizers);
        let parameters = port::parameters(false, &d.fields)?;
        let vals = material_args(&d.kind.qualified, vals.to_vec());
        let (live, vals): (Vec<_>, Vec<_>) = vals.into_iter().partition(|(_, v)| E::is_live(v));
        // a live vec3 group is its three components, each a program input of its field
        let mut grouped: Vec<(String, Value)> = Vec::new();
        for (name, value) in live {
            let components: Vec<(i64, String)> = d
                .fields
                .iter()
                .filter_map(|f| match &f.vec3 {
                    Some((group, axis)) if *group == name => Some((*axis, f.name.clone())),
                    _ => None,
                })
                .collect();
            let component = |axis: i64| -> Option<Value> {
                match &value {
                    Value::List(xs) if xs.len() == 3 => Some(xs[axis as usize].clone()),
                    Value::Residual(r) => {
                        let view = E::Residual::residual_view(*r).ok()?;
                        let term = Term {
                            path: None,
                            ty: Ty::Float,
                            node: W::Node::RefBinding("$vec3".into(), vec![["x", "y", "z"][axis as usize].to_owned()]),
                            form: view.term.form.clone(),
                        };
                        E::Residual::with_bindings(*r, &term, &[("$vec3".to_owned(), value.clone())]).ok().map(Value::Residual)
                    }
                    _ => None,
                }
            };
            if components.len() != 3 {
                grouped.push((name, value));
            } else {
                let parts: Vec<(String, Option<Value>)> = components.iter().map(|(axis, field)| (field.clone(), component(*axis))).collect();
                if parts.iter().all(|(_, v)| v.is_some()) {
                    grouped.push((name, value.clone()));
                    grouped.extend(parts.into_iter().map(|(f, v)| (f, v.unwrap())));
                } else {
                    grouped.push((name, value));
                }
            }
        }
        let mut live = grouped;
        // a live Int with hard bounds is clamped in the program, as the node's parameters clamp it
        let mut clamped: Vec<String> = Vec::new();
        let mut next_live = Vec::new();
        for (name, value) in live.drain(..) {
            let field = d.fields.iter().find(|f| f.name == name);
            match (field.map(|f| &f.kind), &value) {
                (Some(param::KindView::Integer(range)), Value::Residual(r))
                    if (range.hard_min.is_some() || range.hard_max.is_some()) && !normalizers.iter().any(|(n, _)| *n == name) =>
                {
                    let form = E::Residual::residual_view(*r)?.term.form.clone();
                    let term = |ty: Ty, node: W::Node| Term { path: None, ty, node, form: form.clone() };
                    let lit = |n: i64| term(Ty::Int, W::Node::Lit(param::Value::Int(n)));
                    let bound = |op: &str, limit: Option<i64>, inner: Term| -> Term {
                        match limit {
                            None => inner,
                            Some(n) => {
                                let test = term(
                                    Ty::Bool,
                                    W::Node::Op { op: op.to_owned(), args: vec![("a".into(), inner.clone()), ("b".into(), lit(n))], skip: Vec::new() },
                                );
                                term(Ty::Int, W::Node::If(Box::new(test), Box::new(lit(n)), Box::new(inner)))
                            }
                        }
                    };
                    let v = term(Ty::Int, W::Node::RefBinding("$int".into(), Vec::new()));
                    clamped.push(name.clone());
                    let bounded = bound("int/gt", range.hard_max, bound("int/lt", range.hard_min, v));
                    let r2 = E::Residual::with_bindings(*r, &bounded, &[("$int".to_owned(), value.clone())])?;
                    next_live.push((name, Value::Residual(r2)));
                }
                _ => next_live.push((name, value)),
            }
        }
        let live = next_live;
        let mut changes = Vec::new();
        for (name, value) in &vals {
            if let Ok(parameter) = port::find_parameter(&parameters, name) {
                changes.extend(changes_of(&parameter, value)?);
            }
        }
        // a live argument is passed as it is, a program input: its field must have nothing to canonicalize
        let raw = |(name, _): &(String, Value)| -> bool {
            match d.fields.iter().find(|f| f.name == *name) {
                None if d.fields.iter().any(|f| live.iter().any(|(n, _)| *n == f.name) && f.vec3.is_some() && f.vec3.as_ref().map(|v| &v.0) == Some(name)) => true,
                None => !d.fields.iter().any(|f| f.vec3.is_some() && f.vec3.as_ref().map(|v| &v.0) == Some(name)),
                Some(f) => {
                    !normalizers.iter().any(|(n, _)| n == name)
                        && match &f.kind {
                            param::KindView::Toggle => true,
                            param::KindView::Integer(_) if clamped.contains(name) => true,
                            param::KindView::Integer(r) => r.hard_min.is_none() && r.hard_max.is_none(),
                            param::KindView::Floating(r) => r.hard_min.is_none() && r.hard_max.is_none(),
                            _ => false,
                        }
                }
            }
        };
        if !live.iter().all(raw) {
            return Ok(None);
        }
        let live_names: Vec<String> = live.iter().map(|(n, _)| n.clone()).collect();
        let (values, _) = schema.apply_all(schema.default(), &changes).map_err(|m| fail("E_LOWER", &m))?;
        let arguments = ln::arguments(&d.fields, &values).map_err(|m| fail("E_LOWER", &m))?;
        // a context input the call does not reach is bound to a placeholder, so no kernel captures its source
        let reached = if context_inputs.is_empty() { Vec::new() } else { ck::context_ops_reached(&arguments, &checked.defs, &d.body) };
        let mut out: Vec<(String, Value)> = arguments.into_iter().filter(|(n, _)| !live_names.contains(n)).collect();
        out.extend(live);
        out.extend(context_inputs.iter().filter(|(_, op)| !reached.contains(op)).map(|(n, _)| (n.clone(), Value::Int(0))));
        Ok(Some(out))
    });
    E::InlineParameterPolicy { definition, key: source_key, canonicalize }
}

fn at_zero(value: &Value) -> R<Value> {
    if E::is_live(value) {
        E::force(value, &input::at_time(0.0), None)
    } else {
        Ok(value.clone())
    }
}
fn is_zone(kind: &str) -> bool {
    ops::lowering(kind) == Some(ops::Lowering::Zone)
}

fn metadata_factory(operation: &str, args: &[(String, Value)], sources: &[i64]) -> R<Factory> {
    let text = |name: &str, default: &str| -> R<String> {
        match get(args, name) {
            None => Ok(default.to_owned()),
            Some(Value::Text(t)) => Ok(txt(t)),
            _ => Err(fail("E_METADATA_TEXT", "Metadata names and patterns must be static text")),
        }
    };
    let flag = |name: &str| -> R<bool> {
        match get(args, name) {
            None => Ok(false),
            Some(Value::Bool(b)) => Ok(*b),
            _ => Err(fail("E_METADATA_BOOL", "Metadata flags must be static booleans")),
        }
    };
    // every argument is read here, as the OCaml reads them lazily per rebuild but raises at lowering
    let (owner, pattern, replacement, conflict) = (text("owner", "any")?, text("pattern", "")?, text("replacement", "")?, text("conflict", "error")?);
    let (owner_p, source, destination, method) = (text("owner", "point")?, text("source", "")?, text("destination", "")?, text("method", "swap")?);
    let (inverse, unused) = (flag("inverse")?, flag("unused")?);
    let (point, vertex, primitive, detail) = (text("point", "")?, text("vertex", "")?, text("primitive", "")?, text("detail", "")?);
    let name = text("name", "")?;
    let op = operation.to_owned();
    let build_fn: edit::BuildFn = Arc::new(move |inputs: &[Option<node::T>]| {
        let inputs: Vec<node::T> = inputs.iter().flatten().cloned().collect();
        let Some(input) = inputs.first().cloned() else {
            panic!("Metadata operations require geometry");
        };
        Ok(match op.as_str() {
            "sop/rename_attr" => am::rename(&owner, &pattern, &replacement, &conflict, &input).map_err(|d| d.message)?,
            "sop/swap_attr" => am::swap(&owner_p, &source, &destination, &method, &input).map_err(|d| d.message)?,
            "sop/delete_attrs" => am::delete_attributes(inputs.get(1), inverse, &point, &vertex, &primitive, &detail, &input),
            "sop/rename_group" => am::rename_group(&owner, &pattern, &replacement, &conflict, &input).map_err(|d| d.message)?,
            "sop/delete_group" => am::delete_group(&owner, &pattern, unused, &input).map_err(|d| d.message)?,
            "sop/rename_edge" => am::rename_edge(&pattern, &replacement, &input),
            "sop/delete_edge" => am::delete_edge(&name, &input),
            _ => return Err(format!("No metadata lowering for {op}")),
        })
    });
    let _ = build_fn.clone();
    let key = operation.to_owned();
    let inner = build_fn;
    Ok(edit::factory(
        edit::factory_args(&key, &key, &["Flow", "Metadata"]),
        sources.len(),
        Arc::new(move |inputs: &[node::T]| {
            let options: Vec<Option<node::T>> = inputs.iter().cloned().map(Some).collect();
            inner(&options)
        }),
    ))
}

fn is_geometry_arrays(kind: &str) -> bool {
    ops::lowering(kind) == Some(ops::Lowering::GeometryArrays)
}
fn geometry_arrays_key(kind: &str) -> &'static str {
    match kind {
        "sop/remap" => "flow.remap",
        "sop/concat" => "flow.concat",
        "sop/with_planes" => "flow.with_planes",
        _ => "flow.mesh",
    }
}
fn geometry_arrays_label(kind: &str) -> &'static str {
    match kind {
        "sop/remap" => "Remap",
        "sop/concat" => "Concat",
        "sop/with_planes" => "With planes",
        _ => "Mesh",
    }
}
fn geometry_arrays_data(args: &[(String, Value)]) -> Value {
    Value::Record(args.iter().filter(|(n, _)| !["source", "target", "sources", "geometry"].contains(&n.as_str())).cloned().collect())
}

/// A kernel over data that reads its cook context is built per context from the specialized
/// data: the last context's build is kept.
fn cook_wrapped(n: &node::T, node_id: i64, context: &sctx::T, payloads: &[payload::T]) -> R<node::Cooked> {
    let context = sctx::with_cpu_consumers(context, node::id(n), sctx::cpu_consumers(context, node_id));
    build::cook(n, &context, payloads)
}

type Build = dyn Fn(&dyn Fn(&Value) -> Value) -> node::T + Send + Sync;

fn per_context(value: &Value, build_with: Arc<Build>) -> node::T {
    let reads = ak::context_reads(value);
    if reads == deps_mod::STATIC {
        return build_with(&|v: &Value| v.clone());
    }
    let make = {
        let build_with = build_with.clone();
        move |context: &sctx::T| build_with(&|v: &Value| ak::specialize(context, v))
    };
    let base = make(&sctx::create(sctx::Options::default()).expect("context"));
    let key = move |context: &sctx::T| -> (i64, i64, u64) {
        let has = |fact| deps_mod::mem(fact, reads);
        (
            if has(deps_mod::Fact::Seed) { sctx::seed(context) } else { 0 },
            if has(deps_mod::Fact::Frame) { sctx::frame(context) } else { 0 },
            if has(deps_mod::Fact::Time) { sctx::time(context).to_bits() } else { 0 },
        )
    };
    let last: Arc<Mutex<Option<((i64, i64, u64), node::T)>>> = Arc::new(Mutex::new(None));
    let facts = node::facts(&base).clone();
    let wrapped = build::make(
        build::Args {
            label: Some(node::label(&base).to_owned()),
            operation: node::operation(&base).to_owned(),
            version: node::version(&base),
            parameters: node::parameters(&base).to_vec(),
            cook_mode: facts.cook_mode,
            dependencies: dep_union(node::dependencies(&base), reads),
            input_policy: Some(build::input_policy(&base)),
            inputs: build::input_array(&base),
        },
        None,
        Arc::new(move |node_id, context, payloads| {
            let k = key(context);
            let cached = last.lock().unwrap().as_ref().filter(|(kk, _)| *kk == k).map(|(_, n)| n.clone());
            let n = match cached {
                Some(n) => n,
                None => {
                    let n = make(context);
                    *last.lock().unwrap() = Some((k, n.clone()));
                    n
                }
            };
            cook_wrapped(&n, node_id, context, payloads)
        }),
    );
    build::with_facts(facts, &build::with_input_use(node::input_use(&base), &wrapped))
}

// ---------------------------------------------------------------------------------------------
// Whole-chain placement of a resident mesh scope, measured rather than assumed. Per backend and
// authored site, each route keeps the seconds-per-output-float of its recent cooks, with the output
// size they had. The state is keyed by the backend, held weakly, so a closed owner's evidence goes
// with it.
pub mod chain {
    use super::*;

    #[derive(Default)]
    pub struct Samples {
        pub cpu: Vec<(i64, f64)>,
        pub device: Vec<(i64, f64)>,
        /// the CPU cooks that finished: an abandoned probe is only a lower bound
        pub measured: i64,
    }

    pub struct State {
        pub sites: HashMap<String, Samples>,
        pub last: HashMap<String, (i64, i64)>,
        pub materialise: f64,
    }

    static REGISTRY: Mutex<Vec<(Weak<Gpu::Backend>, Arc<Mutex<State>>)>> = Mutex::new(Vec::new());
    const SITES_CAP: usize = 256;

    pub fn state_of(backend: &Arc<Gpu::Backend>) -> Arc<Mutex<State>> {
        let mut registry = REGISTRY.lock().unwrap();
        registry.retain(|(weak, _)| weak.strong_count() > 0);
        if let Some((_, state)) = registry.iter().find(|(weak, _)| weak.upgrade().is_some_and(|b| Arc::ptr_eq(&b, backend))) {
            return state.clone();
        }
        let state = Arc::new(Mutex::new(State { sites: HashMap::new(), last: HashMap::new(), materialise: f64::INFINITY }));
        registry.push((Arc::downgrade(backend), state.clone()));
        state
    }

    pub fn states() -> usize {
        let mut registry = REGISTRY.lock().unwrap();
        registry.retain(|(weak, _)| weak.strong_count() > 0);
        registry.len()
    }

    pub fn floats_of(geometry: &rdk::geometry::T) -> i64 {
        rdk::geometry::point_count(geometry) * 9 + rdk::geometry::primitive_count(geometry) * 6
    }

    pub fn site_of(value: &Value) -> Option<(W::Path, Vec<i64>)> {
        match value {
            Value::Residual(r) => {
                let view = E::Residual::residual_view(*r).ok()?;
                Some((view.site.clone(), view.iter.clone()))
            }
            Value::Record(fields) => fields.iter().rev().find_map(|(_, v)| site_of(v)),
            _ => None,
        }
    }

    /// what a CPU reader of the mesh forces: positions, topology and every attribute
    pub fn force_host(geometry: &rdk::geometry::T) {
        let positions = rdk::geometry::positions(geometry);
        if rdk::packed::Float3::length(positions) > 0 {
            let _ = rdk::packed::Float3::get(positions, 0);
        }
        let topology = rdk::geometry::topology(geometry);
        if rdk::topology::primitive_count(topology) > 0 {
            let _ = rdk::topology::point_of_vertex(topology, 0);
            let _ = rdk::topology::primitive_vertex_range(topology, 0);
            let _ = rdk::topology::primitive_kind(topology, 0);
        }
        for a in rdk::geometry::attributes(geometry) {
            let _ = rdk::attribute::storage(&a);
        }
    }

    pub fn observe_display(gpu: &Gpu::Config, geometry: &rdk::geometry::T, seconds: f64) {
        if let (Some(backend), Gpu::Policy::Measured) = (&gpu.backend, gpu.policy) {
            ak::Display_sink::observe(backend, rdk::packed::Float3::resident(rdk::geometry::positions(geometry)), floats_of(geometry), seconds);
        }
    }

    pub fn observe_draw(gpu: &Gpu::Config, geometry: &rdk::geometry::T, seconds: f64) {
        if let (Some(backend), Gpu::Policy::Measured) = (&gpu.backend, gpu.policy) {
            if !rdk::packed::Float3::resident(rdk::geometry::positions(geometry)) {
                ak::Display_sink::observe_draw(backend, floats_of(geometry), seconds);
            }
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Route {
        Device,
        Cpu,
        Probe(f64),
    }

    pub struct Placed {
        pub route: Route,
        pub record: Box<dyn Fn(&rdk::geometry::T, bool, f64) + Send + Sync>,
        /// A `Probe` ran past its budget.
        pub abandoned: Box<dyn Fn() + Send + Sync>,
    }

    pub fn unplaced(route: Route) -> Placed {
        Placed { route, record: Box::new(|_, _, _| {}), abandoned: Box::new(|| {}) }
    }

    pub fn place(gpu: &Gpu::Config, profile: &Arc<Profile::T>, clock: &Clock, owner: (W::Path, Vec<i64>), readers: Option<i64>) -> Placed {
        if gpu.policy == Gpu::Policy::Qualification {
            return unplaced(Route::Device);
        }
        let site = format!("{}#{}", owner.0.join("/"), owner.1.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(","));
        let report = {
            let (profile, owner, readers) = (profile.clone(), owner.clone(), readers);
            move |points: Option<i64>, reason: PlacementReason, cpu_seconds: Option<f64>, device_seconds: Option<f64>| {
                Profile::place(Some(&profile), &Placement { owner: owner.clone(), count: points, reason, cpu_seconds, device_seconds, cpu_consumers: readers });
            }
        };
        let state = gpu.backend.as_ref().map(state_of);
        let gpu_c = gpu.clone();
        let device_cost = {
            let gpu = gpu.clone();
            move |state: &State, samples: &Samples, floats: i64| -> Option<f64> {
                let moved = Cost::readback(Some(&gpu), floats, 1);
                let rate = |r: f64, fallback: f64| if r < f64::INFINITY { r * floats as f64 } else { fallback };
                let (_, resident_sink) = gpu.backend.as_ref().map_or((f64::INFINITY, f64::INFINITY), ak::Display_sink::rates);
                ak::Display_sink::priced(&samples.device, floats).map(|s| {
                    s + rate(resident_sink, 0.0) + if readers.unwrap_or(0) == 0 { 0.0 } else { rate(state.materialise, moved) }
                })
            }
        };
        let device_cost = Arc::new(device_cost);
        let decide = {
            let (gpu, device_cost, report, site) = (gpu_c.clone(), device_cost.clone(), report.clone(), site.clone());
            move |state: &Arc<Mutex<State>>, floats: i64, points: i64| -> PlacementReason {
                let cpu_consumers = readers.unwrap_or(0);
                let mut guard = state.lock().unwrap();
                let st = &mut *guard;
                let samples = st.sites.get(&site).expect("site evidence");
                let moved = Cost::readback(Some(&gpu), floats, 1);
                let rate = |r: f64, fallback: f64| if r < f64::INFINITY { r * floats as f64 } else { fallback };
                let (host_sink, _) = gpu.backend.as_ref().map_or((f64::INFINITY, f64::INFINITY), ak::Display_sink::rates);
                let cpu_seconds = ak::Display_sink::priced(&samples.cpu, floats).map(|s| s + rate(host_sink, moved));
                let device_seconds = device_cost(st, samples, floats);
                let reason = Cost::choose(cpu_seconds, device_seconds, false, cpu_consumers);
                report(Some(points), reason, cpu_seconds, device_seconds);
                reason
            }
        };
        let decide = Arc::new(decide);
        let evidence = {
            let site = site.clone();
            move |state: &Arc<Mutex<State>>| {
                let mut st = state.lock().unwrap();
                if !st.sites.contains_key(&site) {
                    if st.sites.len() >= SITES_CAP {
                        st.sites.clear();
                        st.last.clear();
                    }
                    st.sites.insert(site.clone(), Samples::default());
                }
            }
        };
        let evidence = Arc::new(evidence);
        let record: Box<dyn Fn(&rdk::geometry::T, bool, f64) + Send + Sync> = {
            let (state, site, clock, evidence, decide) = (state.clone(), site.clone(), clock.clone(), evidence.clone(), decide.clone());
            Box::new(move |geometry: &rdk::geometry::T, device: bool, seconds: f64| {
                let Some(state) = &state else { return };
                let points = rdk::geometry::point_count(geometry);
                let floats = floats_of(geometry);
                if floats <= 0 {
                    return;
                }
                evidence(state);
                let materialise = {
                    let n_device = state.lock().unwrap().sites.get(&site).map_or(0, |s| s.device.len());
                    if device && readers.is_some_and(|n| n > 0) && n_device < 2 {
                        let began = clock();
                        force_host(geometry);
                        Some((clock() - began) / floats as f64)
                    } else {
                        None
                    }
                };
                {
                    let mut st = state.lock().unwrap();
                    if let Some(r) = materialise {
                        st.materialise = st.materialise.min(r);
                    }
                    let sample = (floats, seconds / floats as f64);
                    let samples = st.sites.get_mut(&site).unwrap();
                    let keep = |l: &Vec<(i64, f64)>| -> Vec<(i64, f64)> { std::iter::once(sample).chain(l.iter().take(7).copied()).collect() };
                    if device {
                        samples.device = keep(&samples.device);
                    } else {
                        samples.cpu = keep(&samples.cpu);
                        samples.measured += 1;
                    }
                    st.last.insert(site.clone(), (floats, points));
                }
                if readers.is_some() {
                    decide(state, floats, points);
                }
            })
        };
        let none_placed = |reason, route| {
            report(None, reason, None, None);
            Placed { route, record: Box::new(|_, _, _| {}), abandoned: Box::new(|| {}) }
        };
        let rebuild_record = |route: Route, abandoned: Box<dyn Fn() + Send + Sync>, record: Box<dyn Fn(&rdk::geometry::T, bool, f64) + Send + Sync>| Placed { route, record, abandoned };
        match (&state, readers) {
            (None, _) => {
                report(None, PlacementReason::No_cost_evidence, None, None);
                rebuild_record(Route::Cpu, Box::new(|| {}), record)
            }
            (_, None) => {
                report(None, PlacementReason::Missing_consumer, None, None);
                rebuild_record(Route::Cpu, Box::new(|| {}), record)
            }
            (Some(state), Some(_)) => {
                evidence(state);
                let (size, n_device, n_cpu, measured) = {
                    let st = state.lock().unwrap();
                    let samples = st.sites.get(&site).unwrap();
                    (st.last.get(&site).copied(), samples.device.len(), samples.cpu.len(), samples.measured)
                };
                match size {
                    Some((floats, points)) if n_device >= 2 => {
                        // The CPU runs unbudgeted only on measured cooks: a bound left by an abandoned probe
                        // can lose to one noisy device sample while the true cost is minutes, so it is probed
                        // again under a budget that follows the device's present cost.
                        let on_device = if n_cpu >= 2 { Some(Cost::on_device(decide(state, floats, points))) } else { None };
                        match on_device {
                            Some(true) => rebuild_record(Route::Device, Box::new(|| {}), record),
                            Some(false) if measured >= 2 => rebuild_record(Route::Cpu, Box::new(|| {}), record),
                            _ => {
                                let budget = {
                                    let st = state.lock().unwrap();
                                    let samples = st.sites.get(&site).unwrap();
                                    Cost::probe_budget(device_cost(&st, samples, floats).unwrap_or(0.0))
                                };
                                let abandoned: Box<dyn Fn() + Send + Sync> = {
                                    let (state, site, decide) = (state.clone(), site.clone(), decide.clone());
                                    Box::new(move || {
                                        {
                                            let mut st = state.lock().unwrap();
                                            let samples = st.sites.get_mut(&site).unwrap();
                                            let sample = (floats, budget / floats as f64);
                                            samples.cpu.insert(0, sample);
                                            samples.cpu.insert(0, sample);
                                        }
                                        decide(&state, floats, points);
                                    })
                                };
                                rebuild_record(Route::Probe(budget), abandoned, record)
                            }
                        }
                    }
                    _ => {
                        let _ = none_placed;
                        report(None, PlacementReason::No_cost_evidence, None, None);
                        rebuild_record(Route::Device, Box::new(|| {}), record)
                    }
                }
            }
        }
    }
}

/// The point attributes `fold_mesh_writes` folded into a mesh's record, in write order.
pub const MESH_ATTRIBUTE_PREFIX: &str = "$attr:";

fn mesh_attributes(fields: &[(String, Value)]) -> Vec<(String, String, Value)> {
    fields
        .iter()
        .filter_map(|(key, value)| {
            if !key.starts_with(MESH_ATTRIBUTE_PREFIX) {
                return None;
            }
            let parts: Vec<&str> = key.split(':').collect();
            match parts.as_slice() {
                [_, name, kind] => Some(((*name).to_owned(), (*kind).to_owned(), value.clone())),
                _ => None,
            }
        })
        .collect()
}

fn mesh_host_attributes(grain: i64, fields: &[(String, Value)], geometry: rdk::geometry::T) -> R<rdk::geometry::T> {
    let mut geometry = geometry;
    for (name, kind, values) in mesh_attributes(fields) {
        geometry = ak::write(grain, rdk::attribute::Owner::Point, &kind, &name, &values, &geometry)?;
    }
    Ok(geometry)
}

/// On the device each attribute keeps its buffer. A stream that is not resident, or whose count is
/// not the mesh's, takes the host writer, which reports the count error as a CPU cook does.
fn mesh_resident_attributes(grain: i64, scope: &Gpu::Residency, fields: &[(String, Value)], geometry: rdk::geometry::T) -> R<rdk::geometry::T> {
    let points = rdk::geometry::point_count(&geometry);
    let mut current = geometry.clone();
    let mut result: R<()> = Ok(());
    for (name, kind, values) in mesh_attributes(fields) {
        let retained = if (scope.executor.count)(&values)? == Some(points) { (scope.retain_float)(&values) } else { None };
        let next = match retained {
            Some(planes) => ak::install_deferred(rdk::attribute::Owner::Point, &kind, &name, &Arc::new(planes), &current),
            None => flow_ir::core_exec::host_value(&scope.executor, &values).and_then(|v| ak::write(grain, rdk::attribute::Owner::Point, &kind, &name, &v, &current)),
        };
        match next {
            Ok(g) => current = g,
            Err(e) => {
                result = Err(e);
                break;
            }
        }
    }
    match result {
        Ok(()) => Ok(current),
        Err(e) => {
            for (_, release) in rdk::geometry::releasers(&geometry) {
                release(false);
            }
            Err(e)
        }
    }
}

type StatefulKey = (Vec<(String, Value)>, Vec<node::T>);
type StatefulPrograms = (R<Executor::Program>, Arc<std::sync::OnceLock<R<Executor::Program>>>);
static STATEFUL_PROGRAMS: Mutex<Vec<(StatefulKey, StatefulPrograms)>> = Mutex::new(Vec::new());

/// Settings shared by every kernel-node builder (the labelled arguments of the OCaml functions).
#[derive(Clone)]
pub struct KernelEnv {
    pub gpu: Gpu::Config,
    pub profile: Arc<Profile::T>,
    pub clock: Clock,
    pub strict_body: bool,
    pub reference: bool,
    pub approx: W::Paths,
    pub kernel_source: Arc<dyn Fn() -> String + Send + Sync>,
}

#[derive(Clone, Default)]
pub struct NodeOpts {
    pub check: Option<bool>,
    pub host: bool,
    pub state: Option<State>,
    pub elems: Option<Vec<(String, Value)>>,
}

fn static_topology_of(operation: &str, args: &[(String, Value)]) -> bool {
    operation == "sop/mesh"
        && ["vertex_points", "primitive_offsets", "kinds"].iter().all(|name| match get(args, name) {
            Some(v) => !(E::frame_dependent(v) || E::state_dependent(v)),
            None => false,
        })
}

pub fn geometry_arrays_node(env: &KernelEnv, opts: &NodeOpts, operation: &str, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
    let operation = operation.to_owned();
    let (env_c, opts_c, sources_c, inputs_c) = (env.clone(), opts.clone(), sources.to_vec(), inputs.to_vec());
    let args_all = args.to_vec();
    let result: Arc<Mutex<Option<R<node::T>>>> = Arc::new(Mutex::new(None));
    let slot = result.clone();
    let built = per_context(
        &Value::Record(args.to_vec()),
        Arc::new(move |specialize: &dyn Fn(&Value) -> Value| {
            let args = match specialize(&Value::Record(args_all.clone())) {
                Value::Record(a) => a,
                _ => unreachable!("a record specializes to a record"),
            };
            match geometry_arrays_node_in(&env_c, &opts_c, &operation, &args, &sources_c, &inputs_c) {
                Ok(n) => n,
                Err(e) => {
                    *slot.lock().unwrap() = Some(Err(e));
                    build::make(build::args("flow.failed"), None, Arc::new(|_, _, _| Err(fail("E_LOWER", "failed node"))))
                }
            }
        }),
    );
    match result.lock().unwrap().take() {
        Some(Err(e)) => Err(e),
        _ => Ok(built),
    }
}

fn geometry_arrays_node_in(env: &KernelEnv, opts: &NodeOpts, operation: &str, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
    let check = opts.check.unwrap_or(true);
    let data = geometry_arrays_data(args);
    let prepare = {
        let (env, sources, inputs) = (env.clone(), sources.to_vec(), inputs.to_vec());
        move |data: &Value| ak::prepare(Some(env.profile.clone()), env.strict_body, Some(env.approx.clone()), Some(Sink::Sop_input), &sources, &inputs, data)
    };
    // a topology no frame or state changes is built once (Topology_kernel_impl.static_cached): later
    // cooks run `points_program`, without it
    let static_topology = static_topology_of(operation, args);
    let make_programs = || -> (R<Executor::Program>, Arc<OnceLock<R<Executor::Program>>>) {
        let lazy = Arc::new(OnceLock::new());
        let first = prepare(&data);
        let points = match &data {
            Value::Record(fields) => prepare(&Value::Record(
                fields.iter().filter(|(n, _)| !["vertex_points", "primitive_offsets", "kinds"].contains(&n.as_str())).cloned().collect(),
            )),
            _ => prepare(&data),
        };
        let _ = lazy.set(points);
        (first, lazy)
    };
    // a stateful mesh is rebuilt each frame over the same arguments: its programs, and so its kernels'
    // device sites, slots and buffers, are the last frame's
    let (program, points_program) = if opts.state.is_none() {
        make_programs()
    } else {
        let key: StatefulKey = (args.to_vec(), inputs.to_vec());
        let same = |k: &StatefulKey| k.0.len() == key.0.len() && k.0 == key.0 && k.1.len() == key.1.len() && k.1.iter().zip(key.1.iter()).all(|(a, b)| node::ptr_eq(a, b));
        let found = STATEFUL_PROGRAMS.lock().unwrap().iter().find(|(k, _)| same(k)).map(|(_, p)| p.clone());
        match found {
            Some(p) => p,
            None => {
                let made = make_programs();
                // ponytail: the 8 newest; key a weak table if more stateful meshes live at once
                let mut guard = STATEFUL_PROGRAMS.lock().unwrap();
                guard.insert(0, (key, made.clone()));
                guard.truncate(8);
                made
            }
        }
    };
    if env.strict_body && check {
        program.clone()?;
    }
    let mut parameters = (env.kernel_source)();
    parameters.push_str(match operation {
        "sop/remap" => ";remap;",
        "sop/concat" => ";concat;",
        "sop/with_planes" => ";planes;",
        _ => ";mesh;",
    });
    if opts.host {
        parameters.push_str(&format!("host{}", HOST_SERIAL.fetch_add(1, Ordering::SeqCst)));
    } else {
        parameters.push_str(&key_text(&data));
    }
    if let Some(state) = &opts.state {
        parameters.push_str(&E::state_stamp(state));
    }
    if let Some(fields) = &opts.elems {
        parameters.push_str(&key_text(&Value::Record(fields.clone())));
    }
    let generic = ["sop/remap", "sop/concat", "sop/with_planes"].contains(&operation);
    let (env_k, opts_k, args_k, sources_k, operation_k, data_k) = (env.clone(), opts.clone(), args.to_vec(), sources.to_vec(), operation.to_owned(), data.clone());
    let n = build::make_geometry(
        build::Args {
            label: Some(geometry_arrays_label(operation).to_owned()),
            parameters: parameters.into_bytes(),
            cook_mode: if generic { CookMode::Generic } else { CookMode::Generator },
            dependencies: ak::dependencies(&data),
            inputs: inputs.to_vec(),
            ..build::args(geometry_arrays_key(operation))
        },
        None,
        Arc::new(move |node_id, context, geometries| {
            mesh_cook(&env_k, &opts_k, &operation_k, &args_k, &sources_k, &data_k, static_topology, &program, &points_program, node_id, context, geometries)
        }),
    );
    let facts = Facts {
        reads: if generic { vec!["*".to_owned()] } else { ak::reads(&data) },
        writes: vec!["*".to_owned()],
        topology: Topology::Changed,
        elementwise: Elementwise::None,
        ..node::facts(&n).clone()
    };
    Ok(build::with_facts(facts, &n))
}

#[allow(clippy::too_many_arguments)]
fn mesh_cook(
    env: &KernelEnv,
    opts: &NodeOpts,
    operation: &str,
    args: &[(String, Value)],
    sources: &[i64],
    data: &Value,
    static_topology: bool,
    program: &R<Executor::Program>,
    points_program: &Arc<OnceLock<R<Executor::Program>>>,
    node_id: i64,
    context: &sctx::T,
    geometries: &[rdk::geometry::T],
) -> R<node::GeometryCooked> {
    let cancelled = || sctx::cancelled(context);
    let error = || Err(fail("E_CANCELLED", "Mesh kernel cancelled."));
    // planes never stay resident (`mesh_resident` keeps Vec3 points), so a device cook of them
    // reads its topology back: a static one, cooked once, has nothing to win
    let eligible = !env.reference && env.gpu.fast && (operation == "sop/mesh" || (operation == "sop/mesh_planes" && E::frame_dependent(data)));
    let placed = if eligible {
        chain::place(
            &env.gpu,
            &env.profile,
            &env.clock,
            chain::site_of(&Value::Record(args.to_vec())).unwrap_or((vec![operation.to_owned()], Vec::new())),
            sctx::cpu_consumers(context, node_id),
        )
    } else {
        chain::unplaced(chain::Route::Cpu)
    };
    let began = std::cell::Cell::new((env.clock)());
    let on_device = std::cell::Cell::new(false);
    let geometry = |id: i64| -> Option<rdk::geometry::T> { sources.iter().position(|s| *s == id).map(|i| geometries[i].clone()) };
    let cooked = |device: bool| -> R<rdk::geometry::T> {
        if cancelled() {
            return error();
        }
        // keyed by its vertices' value: the same while the node's arguments are, across a
        // stateful node's per-frame rebuilds too
        let topology_key = get(args, "vertex_points").cloned().unwrap_or_else(|| data.clone());
        let cached = static_topology && device && tki::static_cached(&topology_key);
        let program: Executor::Program = if cached { points_program.get().cloned().unwrap_or_else(|| program.clone())? } else { program.clone()? };
        let force = |resident: Option<Arc<flow_ir::core_exec::Resident>>| -> R<Value> {
            let resolve: flow_ir::core_exec::ResolveFn = {
                let (context, geoms, sources) = (context.clone(), geometries.to_vec(), sources.to_vec());
                Arc::new(move |v: &Value| {
                    ak::resolve(Some(&context), &lookup(geoms.clone(), sources.clone()), v)
                })
            };
            Executor::force(
                &program,
                &Executor::ForceOptions { resident, state: opts.state.clone(), elems: opts.elems.clone(), resolve: Some(resolve), reference: env.reference, strict: env.strict_body },
                sctx::input(context),
            )
        };
        let scope = if device { env.gpu.backend.as_ref().and_then(|b| (b.residency)(true)) } else { None };
        match scope {
            Some(scope) => {
                on_device.set(true);
                let out = (|| -> R<rdk::geometry::T> {
                    match force(Some(scope.executor.clone()))? {
                        Value::Record(fields) => {
                            if cancelled() {
                                return error();
                            }
                            let static_key = if static_topology { Some(&topology_key) } else { None };
                            let result = match tki::mesh_resident(static_key, &scope, &fields) {
                                Some(r) => r,
                                None => {
                                    let host: R<Vec<(String, Value)>> =
                                        fields.iter().map(|(k, v)| Ok((k.clone(), flow_ir::core_exec::host_value(&scope.executor, v)?))).collect();
                                    tk::mesh(&host?)
                                }
                            };
                            let result = result.and_then(|g| mesh_resident_attributes(sctx::grain(context), &scope, &fields, g));
                            result.and_then(|geometry| {
                                if cancelled() {
                                    for (_, release) in rdk::geometry::releasers(&geometry) {
                                        release(false);
                                    }
                                    error()
                                } else {
                                    Ok(geometry)
                                }
                            })
                        }
                        _ => Err(fail("E_TOPOLOGY_TYPE", "Mesh data must be a record.")),
                    }
                })();
                (scope.executor.close)();
                out
            }
            None => match force(None)? {
                Value::Record(fields) => {
                    if cancelled() {
                        return error();
                    }
                    let result = if operation == "sop/with_planes" {
                        match get(args, "geometry") {
                            Some(Value::Deferred(t, id)) if is_geometry_ty(t) && geometry(*id).is_some() => {
                                ak::with_planes(sctx::grain(context), &geometry(*id).unwrap(), &fields)
                            }
                            _ => Err(fail("E_DATA_SOURCE", "With planes needs a cooked geometry.")),
                        }
                    } else if operation == "sop/concat" {
                        let collect = |values: &[Value]| -> R<Vec<rdk::geometry::T>> {
                            let mut out = Vec::new();
                            for v in values {
                                match v {
                                    Value::No_geo => {}
                                    Value::Deferred(t, id) if is_geometry_ty(t) => match geometry(*id) {
                                        Some(g) => out.push(g),
                                        None => return Err(fail("E_DATA_SOURCE", "Concat needs cooked source geometries.")),
                                    },
                                    _ => return Err(fail("E_CONCAT_TYPE", "Concat sources must be a geometry list.")),
                                }
                            }
                            Ok(out)
                        };
                        match get(args, "sources") {
                            Some(Value::List(values)) => collect(values)
                                .and_then(|sources| tki::concat(Some(sctx::cancel_token(context).clone()), sctx::grain(context), &sources, &fields)),
                            _ => Err(fail("E_CONCAT_TYPE", "Concat sources must be a geometry list.")),
                        }
                    } else if operation != "sop/remap" {
                        tk::mesh(&fields).and_then(|g| mesh_host_attributes(sctx::grain(context), &fields, g))
                    } else {
                        let cooked_of = |name: &str| -> R<rdk::geometry::T> {
                            match get(args, name) {
                                Some(Value::Deferred(t, id)) if is_geometry_ty(t) => {
                                    geometry(*id).ok_or_else(|| fail("E_DATA_SOURCE", "Remap needs two cooked geometries."))
                                }
                                _ => Err(fail("E_DATA_SOURCE", "Remap needs two cooked geometries.")),
                            }
                        };
                        cooked_of("source").and_then(|source| {
                            cooked_of("target").and_then(|target| tki::remap(Some(&**sctx::cancel_token(context)), sctx::grain(context), &source, &target, &fields))
                        })
                    };
                    result.and_then(|g| if cancelled() { error() } else { Ok(g) })
                }
                _ => Err(fail("E_TOPOLOGY_TYPE", "Mesh data must be a record.")),
            },
        }
    };
    let result = match placed.route {
        chain::Route::Device => cooked(true),
        chain::Route::Cpu => cooked(false),
        chain::Route::Probe(budget) => {
            let stop = began.get() + budget;
            let clock = env.clock.clone();
            let mut out: Option<R<rdk::geometry::T>> = None;
            let finished = E::with_interrupt(Arc::new(move || clock() > stop), &mut || out = Some(cooked(false)));
            if finished {
                out.unwrap()
            } else {
                (placed.abandoned)();
                began.set((env.clock)());
                cooked(true)
            }
        }
    };
    if eligible && !cancelled() {
        if let Ok(g) = &result {
            (placed.record)(g, on_device.get(), (env.clock)() - began.get());
        }
    }
    result.map(|geometry| node::GeometryCooked { geometry, instances: None })
}

fn attribute_options(args: &[(String, Value)]) -> R<(rdk::attribute::Owner, String)> {
    let text = |key: &str, default: &str, code: &str| -> R<String> {
        match get(args, key) {
            None => Ok(default.to_owned()),
            Some(Value::Text(t)) => Ok(txt(t)),
            _ => Err(fail(code, &format!("Attribute {key} must be static text"))),
        }
    };
    let owner = ak::owner(&text("owner", "point", "E_ATTR_OWNER")?)?;
    let kind = text("type", "vec3", "E_ATTR_TYPE")?;
    ak::value_type(&kind)?;
    Ok((owner, kind))
}

fn group_data(args: &[(String, Value)]) -> Value {
    Value::Record(args.iter().filter(|(n, _)| n == "values" || n == "order").cloned().collect())
}

/// the arguments `fold_writes` adds to a write it fuses
pub const FUSED_DELETES: &str = "$deletes";
pub const FUSED_WRITES: &str = "$writes";
pub const FUSED_CHAIN: &str = "$chain";
pub const FUSED_GUARDS: &str = "$guards";

/// A card's deferred checks: a write folded through the card checks them as it cooks.
fn guards_of(key: &str, args: &[(String, Value)]) -> Vec<Value> {
    match get(args, key) {
        Some(Value::List(guards)) => (**guards).clone(),
        _ => Vec::new(),
    }
}

/// The diagnostic a guard's `error` text holds. ponytail: the OCaml side marshals a `Diagnostic.t`;
/// the Rust evaluator stores `code: message`, decoded here.
fn decode_error(text: &[u8]) -> Diagnostic {
    let s = txt(text);
    match s.split_once(": ") {
        Some((code, message)) => fail(code, message),
        None => fail("E_LOWER", &s),
    }
}

fn check_guards(guards: &[Value], context: &sctx::T) -> R<()> {
    let live = sctx::input(context);
    for guard in guards {
        match guard {
            Value::Record(f) if f.len() == 3 && f[0].0 == "test" && f[1].0 == "fails" && f[2].0 == "error" => {
                if let (test, Value::Bool(fails), Value::Text(error)) = (&f[0].1, &f[1].1, &f[2].1) {
                    match E::force(test, live, None)? {
                        Value::Bool(b) if b == *fails => return Err(decode_error(error)),
                        _ => {}
                    }
                    continue;
                }
                return Err(fail("E_LOWER", "A card's deferred check is malformed"));
            }
            _ => return Err(fail("E_LOWER", "A card's deferred check is malformed")),
        }
    }
    Ok(())
}

/// `n` checking `guards` before it cooks.
fn guarded(guards: Vec<Value>, n: node::T) -> node::T {
    if guards.is_empty() {
        return n;
    }
    let dependencies = guards.iter().fold(node::dependencies(&n), |found, guard| dep_union(found, ak::input_reads(guard)));
    let inner = n.clone();
    let wrapped = build::make(
        build::Args {
            label: Some(node::label(&n).to_owned()),
            operation: node::operation(&n).to_owned(),
            version: node::version(&n),
            parameters: node::parameters(&n).to_vec(),
            cook_mode: node::facts(&n).cook_mode,
            dependencies,
            input_policy: Some(build::input_policy(&n)),
            inputs: build::input_array(&n),
        },
        None,
        Arc::new(move |node_id, context, payloads| {
            check_guards(&guards, context)?;
            cook_wrapped(&inner, node_id, context, payloads)
        }),
    );
    build::with_facts(node::facts(&n).clone(), &build::with_input_use(node::input_use(&n), &wrapped))
}

fn attribute_data(args: &[(String, Value)]) -> Value {
    let data = ["selection", "default", "when", FUSED_WRITES];
    if !data.iter().any(|k| has(args, k)) {
        return get(args, "values").cloned().expect("a write has values");
    }
    Value::Record(args.iter().filter(|(n, _)| n == "values" || data.contains(&n.as_str())).cloned().collect())
}

fn kernel_data(n: &ENode) -> Option<Value> {
    match ops::lowering(&n.kind) {
        Some(ops::Lowering::AttributeWrite) => Some(attribute_data(&n.args)),
        Some(ops::Lowering::GroupWrite) => Some(group_data(&n.args)),
        Some(ops::Lowering::GeometryArrays) => Some(geometry_arrays_data(&n.args)),
        _ => None,
    }
}

fn is_resource(n: &ENode) -> bool {
    ops::lowering(&n.kind) == Some(ops::Lowering::Resource) && (n.kind != "exact" || n.ty == ty::image())
}

#[derive(Clone, Default)]
pub struct AttrOpts {
    pub check: Option<bool>,
    pub state: Option<State>,
    pub elems: Option<Vec<(String, Value)>>,
    pub selection: Option<Value>,
    pub default: Option<Value>,
    pub guard: Option<Value>,
    pub prior: Vec<(String, String, Value)>,
    pub owner: Option<rdk::attribute::Owner>,
    pub kind: Option<String>,
    pub deletes: Option<Vec<(String, String)>>,
}

pub fn attribute_node(env: &KernelEnv, o: &AttrOpts, name: &str, values: &Value, sources: &[i64], inputs: &[node::T]) -> R<node::T> {
    let mut fields = vec![("values".to_owned(), values.clone())];
    for (i, (_, _, v)) in o.prior.iter().enumerate() {
        fields.push((format!("prior{i}"), v.clone()));
    }
    if let Some(v) = &o.selection {
        fields.push(("selection".into(), v.clone()));
    }
    if let Some(v) = &o.default {
        fields.push(("default".into(), v.clone()));
    }
    if let Some(v) = &o.guard {
        fields.push(("when".into(), v.clone()));
    }
    let data = Value::Record(fields);
    let failure: Arc<Mutex<Option<Diagnostic>>> = Arc::new(Mutex::new(None));
    let (env_c, o_c, name_c, values_c, sources_c, inputs_c, slot) = (env.clone(), o.clone(), name.to_owned(), values.clone(), sources.to_vec(), inputs.to_vec(), failure.clone());
    let n = per_context(
        &data,
        Arc::new(move |specialize: &dyn Fn(&Value) -> Value| {
            let values = specialize(&values_c);
            let o = AttrOpts {
                selection: o_c.selection.as_ref().map(specialize),
                default: o_c.default.as_ref().map(specialize),
                guard: o_c.guard.as_ref().map(specialize),
                prior: o_c.prior.iter().map(|(n, k, v)| (n.clone(), k.clone(), specialize(v))).collect(),
                ..o_c.clone()
            };
            let go = || -> R<node::T> {
                if env_c.strict_body && o_c.check.unwrap_or(true) {
                    ak::prepare(None, true, None, None, &sources_c, &inputs_c, &values)?;
                }
                Ok(ak::node(
                    Some(env_c.clock.clone()),
                    &ak::NodeOptions {
                        state: o.state.clone(),
                        reference: env_c.reference,
                        strict: env_c.strict_body,
                        elems: o.elems.clone(),
                        profile: Some(env_c.profile.clone()),
                        approx: env_c.approx.clone(),
                        source: (env_c.kernel_source)(),
                        gpu: Some(env_c.gpu.clone()),
                        selection: o.selection.clone(),
                        default: o.default.clone(),
                        guard: o.guard.clone(),
                        prior: o.prior.clone(),
                        owner: o.owner.unwrap_or(rdk::attribute::Owner::Point),
                        kind: o.kind.clone().unwrap_or_else(|| "vec3".into()),
                        deletes: o.deletes.clone().unwrap_or_default(),
                    },
                    &name_c,
                    &values,
                    &sources_c,
                    &inputs_c,
                ))
            };
            match go() {
                Ok(n) => n,
                Err(e) => {
                    *slot.lock().unwrap() = Some(e);
                    build::make(build::args("flow.failed"), None, Arc::new(|_, _, _| Err(fail("E_LOWER", "failed node"))))
                }
            }
        }),
    );
    match failure.lock().unwrap().take() {
        Some(e) => Err(e),
        None => Ok(n),
    }
}

pub fn group_node(env: &KernelEnv, o: &NodeOpts, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
    let failure: Arc<Mutex<Option<Diagnostic>>> = Arc::new(Mutex::new(None));
    let (env_c, o_c, args_c, sources_c, inputs_c, slot) = (env.clone(), o.clone(), args.to_vec(), sources.to_vec(), inputs.to_vec(), failure.clone());
    let n = per_context(
        &Value::Record(args.to_vec()),
        Arc::new(move |specialize: &dyn Fn(&Value) -> Value| {
            let args = match specialize(&Value::Record(args_c.clone())) {
                Value::Record(a) => a,
                _ => unreachable!("a record specializes to a record"),
            };
            match group_node_in(&env_c, &o_c, &args, &sources_c, &inputs_c) {
                Ok(n) => n,
                Err(e) => {
                    *slot.lock().unwrap() = Some(e);
                    build::make(build::args("flow.failed"), None, Arc::new(|_, _, _| Err(fail("E_LOWER", "failed node"))))
                }
            }
        }),
    );
    match failure.lock().unwrap().take() {
        Some(e) => Err(e),
        None => Ok(n),
    }
}

fn group_node_in(env: &KernelEnv, o: &NodeOpts, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
    let text = |key: &str, default: &str| -> R<String> {
        match get(args, key) {
            None => Ok(default.to_owned()),
            Some(Value::Text(t)) => Ok(txt(t)),
            _ => Err(fail("E_GROUP_TYPE", &format!("Group {key} must be static text"))),
        }
    };
    let owner_name = text("owner", "point")?;
    let name = text("name", "")?;
    let owner = gk::owner(&owner_name)?;
    let values = group_data(args);
    let program = ak::prepare(Some(env.profile.clone()), env.strict_body, None, None, sources, inputs, &values);
    if env.strict_body && o.check.unwrap_or(true) {
        program.clone()?;
    }
    let parameters = format!(
        "{}{}:{}{}{}",
        (env.kernel_source)(),
        owner_name,
        name,
        key_text(&values),
        o.state.as_ref().map_or(String::new(), E::state_stamp)
    );
    let (env_c, o_c, args_c, sources_c) = (env.clone(), o.clone(), args.to_vec(), sources.to_vec());
    let n = build::make_geometry(
        build::Args {
            label: Some(format!("Write group {name}")),
            parameters: parameters.into_bytes(),
            cook_mode: CookMode::Generic,
            dependencies: ak::dependencies(&values),
            inputs: inputs.to_vec(),
            ..build::args("flow.with_group")
        },
        None,
        Arc::new(move |_, context, geometries| {
            let result = if geometries.is_empty() || get(&args_c, "geometry") == Some(&Value::No_geo) {
                Err(fail("E_DATA_SOURCE", "Group write needs geometry."))
            } else if sctx::cancelled(context) {
                Err(fail("E_CANCELLED", "Group kernel cancelled."))
            } else {
                program.clone().and_then(|program| {
                    let geoms = geometries.to_vec();
                    let sources = sources_c.clone();
                    let resolve: flow_ir::core_exec::ResolveFn = {
                        let context = context.clone();
                        Arc::new(move |v: &Value| {
                            ak::resolve(Some(&context), &lookup(geoms.clone(), sources.clone()), v)
                        })
                    };
                    let forced = Executor::force(
                        &program,
                        &Executor::ForceOptions { resident: None, state: o_c.state.clone(), elems: o_c.elems.clone(), resolve: Some(resolve), reference: env_c.reference, strict: env_c.strict_body },
                        sctx::input(context),
                    )?;
                    match forced {
                        Value::Record(fields) => {
                            let order: R<Option<Vec<i64>>> = match get(&fields, "order") {
                                None => Ok(None),
                                Some(Value::Int_array(order)) => Ok(Some((**order).clone())),
                                Some(Value::Record(f)) => match (get(f, "ordered"), get(f, "indices")) {
                                    (Some(Value::Bool(ordered)), Some(Value::Int_array(indices))) => Ok(if *ordered { Some((**indices).clone()) } else { None }),
                                    _ => Err(fail("E_GROUP_ORDER", "Group order requires an integer array or an ordered/indices record.")),
                                },
                                _ => Err(fail("E_GROUP_ORDER", "Group order requires an integer array or an ordered/indices record.")),
                            };
                            let order = order?;
                            match get(&fields, "values") {
                                Some(Value::Int_array(mask)) => gk::write(order.as_deref(), owner, &name, mask, &geometries[0]),
                                _ => Err(fail("E_GROUP_MASK", "Group mask requires an integer array.")),
                            }
                        }
                        _ => unreachable!("a group program returns a record"),
                    }
                })
            };
            result.map(|geometry| node::GeometryCooked { geometry, instances: None })
        }),
    );
    let facts = Facts { topology: Topology::Preserved, reads: vec!["*".into()], writes: vec!["*".into()], ..node::facts(&n).clone() };
    Ok(build::with_facts(facts, &n))
}

type BodyResult = (Arc<dyn Fn(&sctx::T) -> R<node::T> + Send + Sync>, deps_mod::T, Facts);

/// An evaluated body error belongs to cooking: the node keeps its inputs and declared facts.
fn lisp_body_failed(checked: &Workspace, definition: &NodeDefinition, inputs: &[node::T], parameters: &[(String, Value)], diagnostic: Diagnostic) -> BodyResult {
    let mut dependencies = deps_mod::STATIC;
    for (op, fact) in [("sop/context_seed", deps_mod::Fact::Seed), ("sop/context_frame", deps_mod::Fact::Frame), ("sop/context_time", deps_mod::Fact::Time)] {
        if ck::depends(Some(op), parameters, &checked.defs, &definition.body) {
            dependencies = dep_union(dependencies, deps_mod::one(fact));
        }
    }
    let cook_mode = if inputs.is_empty() { CookMode::Generator } else { CookMode::Generic };
    let exact = definition.kind.facts.as_ref().is_none_or(|f| f.exact);
    let facts = Facts { cook_mode, elementwise: Elementwise::None, reads: vec!["*".into()], writes: vec!["*".into()], topology: Topology::Changed, exact };
    (Arc::new(move |_| Err(diagnostic.clone())), dependencies, facts)
}

fn function_elements(value: &Value) -> Vec<FnId> {
    match value {
        Value::Fn(f) => vec![*f],
        Value::List(values) => values.iter().flat_map(function_elements).collect(),
        _ => Vec::new(),
    }
}

fn blank_term(ty: Ty, node: W::Node) -> Term {
    Term { path: None, ty, node, form: std::sync::Arc::new(empty_form()) }
}

pub struct CallEnv {
    pub gpu: Gpu::Config,
    pub lower_call: Arc<dyn Fn(&Gpu::Config, Vec<(FnId, i64)>, Vec<Factory>, Vec<(String, Vec<(String, Value)>)>, Workspace) -> R<T> + Send + Sync>,
    pub record_profile: Arc<dyn Fn(&Arc<Profile::T>) + Send + Sync>,
    pub checked: Arc<Workspace>,
    pub provenance: Option<i64>,
    pub factories: Vec<Factory>,
    pub def_context_use: Arc<dyn Fn() -> Vec<(WGraph, bool)> + Send + Sync>,
}

/// Builds the body of one Lisp-node call: validation, then one lowering of the body over the
/// node's parameters, slots and forwarded captures.
#[allow(clippy::too_many_arguments)]
pub fn build_call(
    env: &CallEnv,
    definition: &NodeDefinition,
    functions: &[(String, Value)],
    bindings: &[(String, Vec<usize>)],
    label: &str,
    context: &sctx::T,
    inputs: &[node::T],
    parameters: &[(String, Value)],
) -> Result<BodyResult, String> {
    let checked: &Workspace = &env.checked;
    let make_capture_factories = |aliases: &[(usize, Vec<(i64, i64)>)]| -> Vec<(String, Factory)> {
        inputs
            .iter()
            .enumerate()
            .map(|(index, input)| {
                let key = format!("lisp_capture_{index}");
                let sources = aliases.iter().find(|(i, _)| *i == index).map(|(_, s)| s.clone());
                let id = node::id(input);
                let factory = edit::factory(
                    edit::factory_args(&key, &key, &["Flow"]),
                    0,
                    Arc::new({
                        let key = key.clone();
                        move |_| {
                            Ok(build::make(
                                build::Args { parameters: id.to_string().into_bytes(), cook_mode: CookMode::Generator, ..build::args(&key) },
                                None,
                                Arc::new({
                                    let sources = sources.clone();
                                    move |_, _, _| {
                                        let p = ln::input(index)?;
                                        let p = match (&p, &sources) {
                                            (payload::T::Kernel(k), Some(sources)) => payload::T::Kernel(sop::kernel::remap_geometry_captures(sources, k)),
                                            _ => p,
                                        };
                                        Ok(node::Cooked { payload: p, instances: None })
                                    }
                                }),
                            ))
                        }
                    }),
                );
                (key, factory)
            })
            .collect()
    };
    let capture_factories = make_capture_factories(&[]);
    let mut body = ck::specialize(env.provenance, context, label, &definition.body)?;
    let call_node = |kind: String, ctx| W::Node::Call { kind, ctx, args: Vec::new() };
    // inputs bound to captured cooked inputs
    body.inputs = body
        .inputs
        .iter()
        .map(|(name, ty, default)| {
            let default = match bindings.iter().find(|(n, _)| n == name) {
                None => default.clone(),
                Some((_, indices)) => {
                    let element_ty = match ty {
                        Ty::List(element) => (**element).clone(),
                        _ => ty.clone(),
                    };
                    let capture = |index: usize| -> Term {
                        let (key, _) = &capture_factories[index];
                        blank_term(element_ty.clone(), call_node(format!("sop/{key}"), if element_ty == ty::image() { fctx::image() } else { fctx::sop() }))
                    };
                    let node = match (ty, indices.as_slice()) {
                        (Ty::List(_), _) => W::Node::ListLit(indices.iter().map(|i| capture(*i)).collect()),
                        (_, [index]) => capture(*index).node,
                        (_, []) => W::Node::Nil,
                        _ => return Err("A fixed Lisp slot has several inputs".to_owned()),
                    };
                    Some(blank_term(ty.clone(), node))
                }
            };
            Ok((name.clone(), ty.clone(), default))
        })
        .collect::<Result<Vec<_>, String>>()?;
    // callable slots forwarded as kernels
    let mut forwarded: Vec<(String, FnId, usize)> = Vec::new();
    for (name, value) in functions {
        let fns = function_elements(value);
        let indices = bindings.iter().find(|(n, _)| n == name).map(|(_, i)| i.clone()).unwrap_or_default();
        if fns.len() != indices.len() {
            return Err("E_LOWER: Callable slots do not match their cooked inputs".to_owned());
        }
        for (offset, (f, index)) in fns.into_iter().zip(indices).enumerate() {
            forwarded.push((format!("lisp_kernel_{name}_{offset}"), f, index));
        }
    }
    let mut geometry_captures: Vec<(String, i64, usize, Factory)> = Vec::new();
    for (_, f, index) in &forwarded {
        for source in ak::sources(&Value::Fn(*f)) {
            let key = format!("lisp_capture_geometry_{index}_{source}");
            let index = *index;
            let input_id = node::id(&inputs[index]);
            let factory = edit::factory(
                edit::factory_args(&key, &key, &["Flow"]),
                0,
                Arc::new({
                    let key = key.clone();
                    move |_| {
                        Ok(build::make_geometry(
                            build::Args { parameters: input_id.to_string().into_bytes(), cook_mode: CookMode::Generator, ..build::args(&key) },
                            None,
                            Arc::new(move |_, _, _| {
                                let payload = ln::input(index)?;
                                let kernel = payload::kernel(&payload)?;
                                match sop::kernel::geometry_capture(&kernel)(source) {
                                    Some(geometry) => Ok(node::GeometryCooked { geometry, instances: None }),
                                    None => Err(fail("E_DATA_SOURCE", "A forwarded function geometry capture is unavailable")),
                                }
                            }),
                        ))
                    }
                }),
            );
            geometry_captures.push((key, source, index, factory));
        }
    }
    let geo_input = |name: String, key: String| (name, ty::geometry(), Some(blank_term(ty::geometry(), call_node(key, fctx::sop()))));
    let mut new_inputs: Vec<(String, Ty, Option<Term>)> = forwarded.iter().map(|(name, _, index)| geo_input(name.clone(), format!("sop/{}", capture_factories[*index].0))).collect();
    new_inputs.extend(geometry_captures.iter().map(|(name, ..)| geo_input(name.clone(), format!("sop/{name}"))));
    new_inputs.extend(body.inputs.iter().cloned());
    body.inputs = new_inputs;
    let validations: Vec<WGraph> = definition
        .validations
        .iter()
        .enumerate()
        .map(|(index, (predicate, _, _))| WGraph { name: format!("lisp_validate_{index}"), context: fctx::value(), body: predicate.clone(), ..body.clone() })
        .collect();
    let normalizers: Vec<WGraph> = definition.normalizers.iter().map(|(_, term)| WGraph { body: term.clone(), ..body.clone() }).collect();
    let mut roots: Vec<WGraph> = vec![body.clone()];
    roots.extend(validations.iter().cloned());
    roots.extend(normalizers);
    let reached = ck::reachable_defs(checked, &roots);
    let reached_has = |def: &WGraph| reached.iter().any(|r| r.name == def.name && r.body == def.body);
    let mut graphs = vec![body.clone()];
    graphs.extend(validations.iter().cloned());
    let library = Workspace {
        graphs: graphs.into(),
        defs: (env.def_context_use)()
            .into_iter()
            .filter_map(|(def, uses)| if !reached_has(&def) { None } else { Some(if uses { ck::specialize(None, context, label, &def).unwrap_or_else(|_| def.clone()) } else { def }) })
            .collect::<Vec<_>>().into(),
        nodes: checked.nodes.iter().filter(|d| d.kind.qualified != definition.kind.qualified).cloned().collect::<Vec<_>>().into(),
        ..checked.clone()
    };
    let overrides: Vec<(String, Vec<(String, Value)>)> = library
        .graphs
        .iter()
        .map(|g| (g.name.clone(), parameters.iter().chain(functions.iter()).filter(|(name, _)| g.inputs.iter().any(|(input, _, _)| input == name)).cloned().collect()))
        .collect();
    let invalid_parameter = |code: &str, message: &str| -> String { if code == "E_PARAM" { message.to_owned() } else { format!("{code}: {message}") } };
    let validated = E::r#static(&Workspace { graphs: validations.clone().into(), ..library.clone() }, false, Some(&overrides[1..]));
    match validated {
        Err(d) => return Err(invalid_parameter(&d.code, &d.message)),
        Ok(validated) => {
            for (index, (_, code, message)) in definition.validations.iter().enumerate() {
                match validated.results.iter().find(|(n, _)| *n == format!("lisp_validate_{index}")).map(|(_, v)| v) {
                    Some(Value::Bool(true)) => {}
                    Some(Value::Bool(false)) => return Err(invalid_parameter(code, message)),
                    _ => panic!("A Lisp node validation must return bool"),
                }
            }
        }
    }
    // Header validation is a constructor contract; evaluated body errors belong to cooking. Strict
    // compiler refusals remain construction errors.
    let outcome = (|| -> R<BodyResult> {
        let (body_overrides, kernel_bindings, aliases): (_, Vec<(FnId, i64)>, Vec<(usize, Vec<(i64, i64)>)>) = if forwarded.is_empty() {
            (overrides[0].clone(), Vec::new(), Vec::new())
        } else {
            let forwarded_evaluation = E::r#static(&Workspace { graphs: vec![body.clone()].into(), ..library.clone() }, false, Some(&[overrides[0].clone()]))?;
            let marker = |name: &str| -> R<i64> {
                match get(&forwarded_evaluation.plan.instances[0].inputs, name) {
                    Some(Value::Deferred(_, source)) => Ok(*source),
                    _ => Err(fail("E_LOWER", "A forwarded kernel needs a cooked source")),
                }
            };
            let mut remapped: Vec<(String, FnId, i64, usize, Vec<(i64, i64)>)> = Vec::new();
            for (name, f, index) in &forwarded {
                let sources: R<Vec<(i64, i64)>> = geometry_captures.iter().filter(|(_, _, slot, _)| slot == index).map(|(n, source, _, _)| Ok((*source, marker(n)?))).collect();
                let sources = sources?;
                remapped.push((name.clone(), E::Host::remap_function_geometry(&sources, *f), marker(name)?, *index, sources));
            }
            let functions2: Vec<(String, Value)> = functions
                .iter()
                .map(|(name, value)| {
                    let remap = |offset: usize, f: FnId| -> Value {
                        let key = format!("lisp_kernel_{name}_{offset}");
                        match remapped.iter().find(|(m, ..)| *m == key) {
                            Some((_, f2, _, _, _)) => Value::Fn(*f2),
                            None => Value::Fn(f),
                        }
                    };
                    let v = match value {
                        Value::Fn(f) => remap(0, *f),
                        Value::List(_) => Value::List(Arc::new(function_elements(value).into_iter().enumerate().map(|(i, f)| remap(i, f)).collect())),
                        _ => value.clone(),
                    };
                    (name.clone(), v)
                })
                .collect();
            let selected: Vec<(String, Value)> =
                parameters.iter().chain(functions2.iter()).filter(|(name, _)| body.inputs.iter().any(|(input, _, _)| input == name)).cloned().collect();
            (
                (body.name.clone(), selected),
                remapped.iter().map(|(_, f, source, _, _)| (*f, *source)).collect(),
                remapped.iter().map(|(_, _, _, index, sources)| (*index, sources.clone())).collect(),
            )
        };
        let capture_factories = make_capture_factories(&aliases);
        // one program of one call: the graph's inputs feed a call of the kind, which inlines in the
        // calling program, its context read through the cook context as a card's is
        let call_args: Vec<(String, Term)> = definition
            .body
            .inputs
            .iter()
            .filter_map(|(name, ty, _)| {
                let read = |name: &str| blank_term(Ty::Float, W::Node::RefBinding(name.to_owned(), Vec::new()));
                match definition.fields.iter().find_map(|f| if f.name == *name { f.vec3.clone() } else { None }) {
                    None => Some((name.clone(), blank_term(ty.clone(), W::Node::RefBinding(name.clone(), Vec::new())))),
                    Some((group, 0)) => {
                        let component = |i: i64| definition.fields.iter().find_map(|f| if f.vec3 == Some((group.clone(), i)) { Some(read(&f.name)) } else { None }).expect("vec3 component");
                        Some((group.clone(), blank_term(Ty::Vec3, W::Node::Vec(vec![component(0), component(1), component(2)]))))
                    }
                    Some(_) => None,
                }
            })
            .collect();
        let call = WGraph {
            body: blank_term(ty::geometry(), W::Node::Call { kind: definition.kind.qualified.clone(), ctx: fctx::sop(), args: call_args }),
            ..body.clone()
        };
        let mut call_factories: Vec<Factory> = capture_factories.iter().map(|(_, f)| f.clone()).collect();
        call_factories.extend(geometry_captures.iter().map(|(_, _, _, f)| f.clone()));
        call_factories.extend(env.factories.iter().cloned());
        let lowered = (env.lower_call)(
            &env.gpu,
            kernel_bindings,
            call_factories,
            vec![body_overrides],
            Workspace {
                graphs: vec![call].into(),
                defs: checked.defs.iter().filter(|def| reached_has(def)).cloned().collect::<Vec<_>>().into(),
                nodes: checked.nodes.clone(),
                ..library.clone()
            },
        )?;
        (env.record_profile)(&lowered.profile);
        let [graph] = lowered.graphs.as_slice() else {
            return Err(fail("E_LOWER", "A Lisp node body needs one SOP graph"));
        };
        let root = graph.root.ok_or_else(|| fail("E_LOWER", "A Lisp node body must return geometry"))?;
        let compiled = edit_r(edit::compile_node(&graph.network.geometry, root))?;
        let mut dependencies = node::graph_dependencies(&compiled);
        let driven = !graph.network.states.is_empty() || !graph.network.drives.is_empty() || !graph.network.frame_nodes.is_empty();
        dependencies = dep_union(dependencies, vl::dependencies(&graph.network));
        let reached_ops = ck::context_ops_reached(parameters, &checked.defs, &definition.body);
        for (op, fact) in [("sop/context_seed", deps_mod::Fact::Seed), ("sop/context_frame", deps_mod::Fact::Frame), ("sop/context_time", deps_mod::Fact::Time)] {
            if reached_ops.iter().any(|o| o == op) {
                dependencies = dep_union(dependencies, deps_mod::one(fact));
            }
        }
        let owned: Vec<node::Info> = node::inspect(&compiled).into_iter().filter(|n| !n.operation.starts_with("lisp_capture_")).collect();
        let declared = definition.kind.facts.clone();
        // a driven body is relowered per frame, so this lowering's nodes speak only for it: without a
        // declaration its facts are the conservative ones
        let conservative = driven && declared.is_none();
        // A declared `:facts` is the node's contract: the body's selection root reads "*" and replaces
        // topology on paper, which would otherwise defeat component caching for every node that
        // declares it. Session still refuses an undeclared write or a replaced topology at cook time.
        let preserved = match &declared {
            Some(f) => f.preserves_topology,
            None => !conservative && owned.iter().all(|n| n.facts.topology == Topology::Preserved),
        };
        let input_count = inputs.len();
        let resolve = |names: &[String]| -> Vec<String> {
            flow_check::catalog::resolve_component_names(
                &|name| match get(parameters, name) {
                    Some(Value::Text(t)) => Some(txt(t)),
                    _ => None,
                },
                names,
            )
        };
        // Declared names (parameter-named ones resolved), then what the body's attribute kernels name
        // themselves. Behind a wildcard (a selection root) only the declaration speaks; otherwise
        // only what the body reads counts.
        let components = |declared: Option<&Vec<String>>, names: &dyn Fn(&Facts) -> Vec<String>| -> Vec<String> {
            let mut inferred: Vec<String> = owned.iter().flat_map(|n| names(&n.facts)).collect();
            inferred.sort();
            inferred.dedup();
            match declared {
                None => if conservative { vec!["*".to_owned()] } else { inferred },
                Some(declared) => {
                    let declared = resolve(declared);
                    if inferred.iter().any(|n| n == "*") {
                        let mut out = declared.clone();
                        out.extend(inferred.iter().filter(|n| *n != "*" && !declared.contains(n)).cloned());
                        out
                    } else {
                        let mut out: Vec<String> = declared.iter().filter(|n| inferred.contains(n)).cloned().collect();
                        out.extend(inferred.iter().filter(|n| !declared.contains(n)).cloned());
                        out
                    }
                }
            }
        };
        let facts = Facts {
            cook_mode: if input_count == 0 {
                CookMode::Generator
            } else if preserved && input_count == 1 {
                CookMode::DuplicateInput(0)
            } else {
                CookMode::Generic
            },
            elementwise: match &declared {
                Some(f) if preserved => match f.elementwise {
                    flow_check::catalog::KernelElements::Points => Elementwise::Points,
                    flow_check::catalog::KernelElements::Primitives => Elementwise::Primitives,
                    flow_check::catalog::KernelElements::Irregular => Elementwise::None,
                },
                _ => if preserved { node::facts(&compiled).elementwise } else { Elementwise::None },
            },
            reads: components(declared.as_ref().map(|f| &f.reads), &|f| f.reads.clone()),
            writes: components(declared.as_ref().map(|f| &f.writes), &|f| f.writes.clone()),
            topology: if preserved { Topology::Preserved } else { Topology::Changed },
            exact: definition.kind.facts.as_ref().is_none_or(|f| f.exact) && owned.iter().all(|n| n.facts.exact),
        };
        let gpu = env.gpu.clone();
        let network = graph.network.clone();
        // the body's nodes read residuals and functions of the library lowering's evaluation: keep it as long as they live
        let keep = Arc::new(lowered);
        let body_fn: Arc<dyn Fn(&sctx::T) -> R<node::T> + Send + Sync> = Arc::new(move |context: &sctx::T| {
            let _keep = &keep;
            if !driven {
                return Ok(compiled.clone());
            }
            let lane = vl::create(None);
            let resolved = vl::resolve(Some(&gpu), Some(sctx::input(context)), &lane, sctx::time(context), &network)?;
            if edit::ptr_eq(&resolved.geometry, &network.geometry) {
                Ok(compiled.clone())
            } else {
                edit_r(edit::compile_node(&resolved.geometry, root))
            }
        });
        Ok((body_fn, dependencies, facts))
    })();
    Ok(match outcome {
        Ok(r) => r,
        Err(d) => lisp_body_failed(checked, definition, inputs, parameters, d),
    })
}

// ponytail: a plain 64-entry most-recently-used list standing in for the owner-registered `Lru.cache`
// ("flow_sop.built"); register it with the ledger when `lru::Owner` takes a typed entry list.
const BUILT_CAPACITY: usize = 64;
type BuiltEntry = (Option<Arc<Gpu::Backend>>, node::T);
static BUILT: Mutex<Vec<(String, BuiltEntry)>> = Mutex::new(Vec::new());

/// Drops the inlined-body nodes built for a closed owner's backend.
pub fn forget_backend(backend: &Arc<Gpu::Backend>) {
    BUILT.lock().unwrap().retain(|(_, (held, _))| !held.as_ref().is_some_and(|b| Arc::ptr_eq(b, backend)));
}

fn built_find(key: &str) -> Option<BuiltEntry> {
    let mut cache = BUILT.lock().unwrap();
    let i = cache.iter().position(|(k, _)| k == key)?;
    let entry = cache.remove(i);
    let found = entry.1.clone();
    cache.insert(0, entry);
    Some(found)
}

fn built_add(key: String, entry: BuiltEntry) {
    let mut cache = BUILT.lock().unwrap();
    cache.retain(|(k, _)| *k != key);
    cache.insert(0, (key, entry));
    cache.truncate(BUILT_CAPACITY);
}

/// Placement states still owned by a live backend, for the close checks.
pub fn chain_states() -> usize {
    chain::states()
}

/// The seconds from a host mesh's frame to its first presentation, added to its display sink.
pub fn observe_draw(gpu: &Gpu::Config, geometry: &rdk::geometry::T, seconds: f64) {
    chain::observe_draw(gpu, geometry, seconds)
}

/// The seconds the editor spent preparing a mesh for display.
pub fn observe_display(gpu: &Gpu::Config, geometry: &rdk::geometry::T, seconds: f64) {
    chain::observe_display(gpu, geometry, seconds)
}

fn geo_of(args: &[(String, Value)]) -> Vec<i64> {
    fn sources(v: &Value, out: &mut Vec<i64>) {
        match v {
            Value::Deferred(t, j) if ty::is_cooked(t) => out.push(*j),
            Value::List(values) => values.iter().for_each(|v| sources(v, out)),
            _ => {}
        }
    }
    let mut out = Vec::new();
    for (_, v) in args {
        sources(v, &mut out);
    }
    out
}

/// The cooked handles (geometry and images) a value reads, through captures: what a structural node
/// over it takes as inputs.
fn cooked_sources(value: &Value) -> Vec<i64> {
    fn visit(v: &Value, residuals: &mut BTreeSet<i64>, functions: &mut BTreeSet<i64>, ids: &mut Vec<i64>) {
        match v {
            Value::Deferred(t, id) if ty::is_cooked(t) => ids.push(*id),
            Value::Residual(r) => {
                if residuals.insert(E::Residual::residual_id(*r)) {
                    if let Ok(view) = E::Residual::residual_view(*r) {
                        for (_, b) in &view.bindings {
                            visit(b, residuals, functions, ids);
                        }
                    }
                }
            }
            Value::Fn(f) => {
                if functions.insert(E::Residual::function_id(*f)) {
                    for (_, b) in E::Residual::function_bindings(*f) {
                        visit(&b, residuals, functions, ids);
                    }
                }
            }
            Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().for_each(|(_, v)| visit(v, residuals, functions, ids)),
            Value::List(values) => values.iter().for_each(|v| visit(v, residuals, functions, ids)),
            _ => {}
        }
    }
    let (mut residuals, mut functions, mut ids) = (BTreeSet::new(), BTreeSet::new(), Vec::new());
    visit(value, &mut residuals, &mut functions, &mut ids);
    ids.sort();
    ids.dedup();
    ids
}

/// The set alone: the worker keeps this predicate past the lowering it came from.
pub fn is_volatile(lowered: &T) -> Arc<dyn Fn(i64) -> bool + Send + Sync> {
    let volatile = lowered.volatile.clone();
    Arc::new(move |id| volatile.contains_key(&id))
}

// Fusion across cards (single-program design, E4). A point write whose geometry is, through card
// markers and deletions of other attributes, a point write that nothing else reads is rewritten to
// write over that write's input: its reads of an attribute the chain writes become the values
// written, its other attribute reads and counts of the chain read the input, the earlier writes run
// first in the same node (`FUSED_WRITES`, one record program) and the chain's deletions run after
// them (`FUSED_DELETES`; they commute with writes of other attributes). The earlier write is no
// longer cooked, so the maps are one region; folding left to right chains any number of writes. The
// intermediate cards are unchanged, for display and probes.
fn literal_pattern(pattern: &str) -> bool {
    !pattern.chars().any(|c| matches!(c, '*' | '?' | '[' | '\\' | '^' | ' ' | '\t' | '\n' | '\r'))
}

#[derive(Clone, Debug, PartialEq)]
pub enum Static {
    Write(String, String, String),
    Delete(String, String),
    Mesh,
    Open,
}

/// What a geometry's producers decide statically, newest first: literal writes (one with a `:when`
/// still to cook decides nothing), literal deletions, and a mesh, which holds positions only. Card
/// markers and group writes pass through; any other producer, or a chain over 64 steps, ends in
/// `Open`. `sop/has_attr` is answered from it, and an inlined body keys on it.
pub fn static_attributes(node: &dyn Fn(i64) -> Option<ENode>, id: i64) -> Vec<Static> {
    fn text(n: &ENode, key: &str, default: &str) -> Option<String> {
        match get(&n.args, key) {
            None => Some(default.to_owned()),
            Some(Value::Text(t)) => Some(txt(t)),
            Some(_) => None,
        }
    }
    let mut acc: Vec<Static> = Vec::new();
    let mut id = id;
    let mut depth = 0;
    loop {
        let Some(n) = node(id) else {
            acc.push(Static::Open);
            return acc;
        };
        if depth >= 64 {
            acc.push(Static::Open);
            return acc;
        }
        let next = |key: &str| -> Option<i64> {
            match get(&n.args, key) {
                Some(Value::Deferred(_, x)) if *x < id => Some(*x),
                _ => None,
            }
        };
        let step: Option<i64> = match n.kind.as_str() {
            k if k == E::card_kind() => next("value"),
            "sop/with_group" => next("geometry"),
            "sop/mesh" | "sop/mesh_planes" => {
                acc.push(Static::Mesh);
                return acc;
            }
            "sop/with_attr" if !has(&n.args, "when") => match (text(&n, "attribute", ""), text(&n, "owner", "point"), text(&n, "type", "vec3")) {
                (Some(name), Some(owner), Some(kind)) if !name.is_empty() => {
                    acc.push(Static::Write(owner, name, kind));
                    let x = next("geometry");
                    x
                }
                _ => {
                    acc.push(Static::Open);
                    return acc;
                }
            },
            "sop/delete_attr" if !has(&n.args, "when") => match (text(&n, "attribute", ""), text(&n, "owner", "point")) {
                (Some(pattern), Some(owner)) if !pattern.is_empty() && literal_pattern(&pattern) => {
                    acc.push(Static::Delete(owner, pattern));
                    let x = next("geometry");
                    x
                }
                _ => {
                    acc.push(Static::Open);
                    return acc;
                }
            },
            _ => {
                acc.push(Static::Open);
                return acc;
            }
        };
        match step {
            Some(x) => {
                id = x;
                depth += 1;
            }
            None => {
                acc.push(Static::Open);
                return acc;
            }
        }
    }
}

fn static_facts_key(node: &dyn Fn(i64) -> Option<ENode>, id: i64) -> String {
    static_attributes(node, id)
        .iter()
        .map(|s| match s {
            Static::Write(o, n, k) => format!("w{o}:{k}:{n}"),
            Static::Delete(o, p) => format!("d{o}:{p}"),
            Static::Mesh => "m".to_owned(),
            Static::Open => "?".to_owned(),
        })
        .collect::<Vec<_>>()
        .join(";")
}

fn static_source_of(node: &dyn Fn(i64) -> Option<ENode>, value: &Value) -> Option<Value> {
    const OWNERS: [&str; 4] = ["point", "vertex", "primitive", "detail"];
    const KINDS: [&str; 8] = ["float", "int", "vec2", "vec3", "vec4", "text", "float_array", "int_array"];
    let Value::Source(op, _, args) = value else { return None };
    if op != "sop/has_attr" {
        return None;
    }
    let text = |key: &str, default: &str| -> Option<String> {
        match get(args, key) {
            None => Some(default.to_owned()),
            Some(Value::Text(t)) => Some(txt(t)),
            Some(_) => None,
        }
    };
    let kind: Option<Option<String>> = match get(args, "type") {
        None => Some(None),
        Some(Value::Text(k)) if KINDS.contains(&txt(k).as_str()) => Some(Some(txt(k))),
        Some(_) => None,
    };
    match (get(args, "geometry"), text("attribute", ""), text("owner", "point"), kind) {
        (Some(Value::Deferred(t, id)), Some(name), Some(owner), Some(kind)) if is_geometry_ty(t) && !name.is_empty() && OWNERS.contains(&owner.as_str()) => {
            let answer = static_attributes(node, *id).into_iter().find_map(|s| match s {
                Static::Write(o, n, k) if o == owner && n == name => Some(Some(kind.is_none() || kind.as_deref() == Some(k.as_str()))),
                Static::Delete(o, p) if o == owner && p == name => Some(Some(false)),
                Static::Write(..) | Static::Delete(..) => None,
                Static::Mesh => Some(Some(owner == "point" && name == "P" && (kind.is_none() || kind.as_deref() == Some("vec3")))),
                Static::Open => Some(None),
            });
            Some(match answer {
                Some(Some(b)) => Value::Bool(b),
                _ => value.clone(),
            })
        }
        _ => Some(value.clone()),
    }
}

static HOOKS: std::sync::Once = std::sync::Once::new();

/// The evaluator's host hooks (inline parameters, live kernels, static facts and sources), installed
/// once before the first lowering. They are module-level side effects in the OCaml.
pub fn install_hooks() {
    HOOKS.call_once(|| {
        E::set_inline_parameters(Arc::new(|checked: &Workspace, d: &NodeDefinition| inline_policy(checked, d)));
        // an inlined call's live arguments stay its kernels' data only where a strict kernel takes
        // them: a noise's seed is a region constant, so such a call stays a node of its own
        E::set_live_kernels(Arc::new(|nodes: &[ENode]| {
            nodes.iter().all(|n| match kernel_data(n) {
                Some(data) if E::frame_dependent(&data) => {
                    Executor::compile(&data, &Executor::CompileOptions { strict: true, sink: Some(Sink::Sop_input), ..Default::default() }).is_ok()
                }
                _ => true,
            })
        }));
        E::set_static_facts(Arc::new(|node: &dyn Fn(i64) -> Option<ENode>, id: i64| static_facts_key(node, id)));
        E::set_static_source(Arc::new(|node: &dyn Fn(i64) -> Option<ENode>, value: &Value| static_source_of(node, value)));
    });
}

fn fused_entry(name: &str, kind: &str, values: Value) -> Value {
    Value::Record(vec![("attribute".into(), Value::Text(name.as_bytes().to_vec())), ("type".into(), Value::Text(kind.as_bytes().to_vec())), ("values".into(), values)])
}

fn fused_entries(args: &[(String, Value)]) -> R<Vec<(String, String, Value)>> {
    match get(args, FUSED_WRITES) {
        Some(Value::List(entries)) => entries
            .iter()
            .map(|e| match e {
                Value::Record(f) if f.len() == 3 && f[0].0 == "attribute" && f[1].0 == "type" && f[2].0 == "values" => match (&f[0].1, &f[1].1) {
                    (Value::Text(n), Value::Text(k)) => Ok((txt(n), txt(k), f[2].1.clone())),
                    _ => Err(fail("E_LOWER", "A fused write's earlier writes are attribute/type/values records")),
                },
                _ => Err(fail("E_LOWER", "A fused write's earlier writes are attribute/type/values records")),
            })
            .collect(),
        _ => Ok(Vec::new()),
    }
}

/// whether a write's values map an index (`array/range`); one that reads no attribute too is a
/// generated attribute (a grid's P)
fn indexed(value: &Value) -> bool {
    fn term(t: &Term) -> bool {
        match &t.node {
            W::Node::Op { op, .. } if op == "array/range" => true,
            _ => W::children(t).iter().any(term),
        }
    }
    fn visit(v: &Value, seen: &mut BTreeSet<i64>) -> bool {
        match v {
            Value::Residual(r) => {
                let id = E::Residual::residual_id(*r);
                if !seen.insert(id) {
                    return false;
                }
                let Ok(view) = E::Residual::residual_view(*r) else { return false };
                term(&view.term) || view.bindings.iter().any(|(_, v)| visit(v, seen))
            }
            Value::Record(fs) => fs.iter().any(|(_, v)| visit(v, seen)),
            Value::List(vs) => vs.iter().any(|v| visit(v, seen)),
            _ => false,
        }
    }
    visit(value, &mut BTreeSet::new())
}

/// `device`: a fused write may run on a device. Without one an indexed earlier write (a grid's P) is
/// not folded: the CPU would compute it again with every recook of the chain, where its own node is
/// cooked once and cached; a device makes it from the element index instead of an upload.
fn static_cpu_data(data: &Value) -> bool {
    !(E::frame_dependent(data) || E::state_dependent(data)) && ak::sources(data).is_empty() && ak::context_reads(data) == deps_mod::STATIC
}

fn template_flags(nodes: &[ENode]) -> Vec<bool> {
    let count = nodes.len() as i64;
    let mut templ = vec![false; nodes.len()];
    for n in nodes {
        if is_zone(&n.kind) {
            if let (Some(Value::Int(lo)), Some(Value::Int(hi))) = (get(&n.args, "lo"), get(&n.args, "hi")) {
                for id in (*lo).max(0)..count.min(*hi) {
                    templ[id as usize] = true;
                }
            }
        }
    }
    templ
}

fn usage_counts(plan: &Plan, nodes: &[ENode], skip: &[bool]) -> Vec<i64> {
    let count = nodes.len() as i64;
    let mut uses = vec![0i64; nodes.len()];
    let mut note = |values: Vec<&Value>| {
        let mut ids: Vec<i64> = values.into_iter().flat_map(ak::sources).collect();
        ids.sort();
        ids.dedup();
        for id in ids {
            if id >= 0 && id < count {
                uses[id as usize] += 1;
            }
        }
    };
    for n in nodes {
        if !skip[n.id as usize] {
            note(n.args.iter().map(|(_, v)| v).collect());
        }
    }
    for i in &plan.instances {
        note(std::iter::once(&i.result).chain(i.inputs.iter().map(|(_, v)| v)).collect());
    }
    uses
}

/// A chain of point writes over a `sop/mesh` that nothing else reads becomes fields of the mesh's
/// record, so one program builds them: a compacted face list their maps gather is one producer on
/// the device, rather than evaluated on the CPU and uploaded for each write. A static mesh under a
/// frame-driven write keeps its own node, or the mesh would cook again every frame.
fn fold_mesh_writes(plan: &Plan) -> Plan {
    let mut nodes = plan.nodes.clone();
    let count = nodes.len() as i64;
    let templ = template_flags(&nodes);
    // an earlier write `fold_writes` folded into a later one stays in the plan, unread: not a reader
    let mut folded = vec![false; nodes.len()];
    for n in &nodes {
        if let Some(Value::List(ids)) = get(&n.args, FUSED_CHAIN) {
            for id in ids.iter() {
                if let Value::Int(id) = id {
                    if *id >= 0 && *id < count {
                        folded[*id as usize] = true;
                    }
                }
            }
        }
    }
    let uses = usage_counts(plan, &nodes, &folded);
    let mesh_under = |nodes: &Vec<ENode>, id: i64| -> Option<ENode> {
        let mut id = id;
        loop {
            if id < 0 || id >= count || uses[id as usize] != 1 || templ[id as usize] {
                return None;
            }
            let n = &nodes[id as usize];
            if n.kind == "sop/mesh" {
                return Some(n.clone());
            }
            if n.kind == E::card_kind() && guards_of("guards", &n.args).is_empty() {
                match get(&n.args, "value") {
                    Some(Value::Deferred(_, below)) if *below < id => id = *below,
                    _ => return None,
                }
            } else {
                return None;
            }
        }
    };
    let text = |n: &ENode, key: &str| -> Option<String> {
        match get(&n.args, key) {
            Some(Value::Text(t)) => Some(txt(t)),
            _ => None,
        }
    };
    for id in 0..nodes.len() {
        let w = nodes[id].clone();
        if w.kind == "sop/with_attr"
            && !templ[id]
            && w.args.iter().all(|(k, _)| ["geometry", "attribute", "values", "owner", "type", FUSED_WRITES, FUSED_CHAIN].contains(&k.as_str()))
            && text(&w, "owner").is_none_or(|o| o == "point")
        {
            if let (Some(Value::Deferred(_, g)), Some(name), Some(values)) = (get(&w.args, "geometry"), text(&w, "attribute"), get(&w.args, "values")) {
                let kind = text(&w, "type").unwrap_or_else(|| "vec3".into());
                let Ok(mut writes) = fused_entries(&w.args) else { continue };
                writes.push((name, kind, values.clone()));
                let plain = |(name, _, values): &(String, String, Value)| !name.contains(':') && name != "P" && ak::sources(values).is_empty() && !matches!(values, Value::Record(_));
                if let Some(mesh) = mesh_under(&nodes, *g) {
                    let data = geometry_arrays_data(&mesh.args);
                    if writes.iter().all(plain)
                        && (E::frame_dependent(&data) || !writes.iter().any(|(_, _, v)| E::frame_dependent(v)))
                        // a stateful write folds when its mesh is stateful too: one node cooks both over the state's fork
                        && (E::state_dependent(&data) || !writes.iter().any(|(_, _, v)| E::state_dependent(v)))
                    {
                        let mut args = mesh.args.clone();
                        args.extend(writes.into_iter().map(|(name, kind, values)| (format!("{MESH_ATTRIBUTE_PREFIX}{name}:{kind}"), values)));
                        nodes[id] = ENode { kind: mesh.kind.clone(), args, ..w };
                    }
                }
            }
        }
    }
    Plan { nodes, ..plan.clone() }
}

fn fold_writes(device: bool, defer: bool, plan: &Plan) -> R<Plan> {
    let mut nodes = plan.nodes.clone();
    let count = nodes.len() as i64;
    let templ = template_flags(&nodes);
    // the copies' residual ids are cache keys: deterministic per plan, outside the evaluator's
    let next_fresh = Arc::new(AtomicI64::new(0));
    let fresh = {
        let next_fresh = next_fresh.clone();
        move || -(1i64 << 50) - (next_fresh.fetch_add(1, Ordering::SeqCst) + 1)
    };
    // A count of a geometry that only writes or deletes attributes (or a card over one) is its
    // input's count: counting the input instead, a read of counts alone keeps no chain alive.
    let root = |nodes: &Vec<ENode>, id: i64| -> i64 {
        let mut id = id;
        loop {
            if id < 0 || id >= count || templ[id as usize] {
                return id;
            }
            let n = &nodes[id as usize];
            let input = |key: &str| -> Option<i64> {
                match get(&n.args, key) {
                    Some(Value::Deferred(t, x)) if is_geometry_ty(t) && *x < id => Some(*x),
                    _ => None,
                }
            };
            let next = if n.kind == E::card_kind() {
                input("value").or(Some(id))
            } else if ["sop/with_attr", "sop/delete_attr", "sop/with_group"].contains(&n.kind.as_str()) {
                input("geometry").or(Some(id))
            } else {
                return id;
            };
            match next {
                Some(x) if x != id => id = x,
                _ => return id,
            }
        }
    };
    let recount = |nodes: &Vec<ENode>, v: &Value| -> Option<Value> {
        match v {
            Value::Source(op, ty, fields) | Value::Struct(op, ty, fields) if op == "sop/count" => match get(fields, "geometry") {
                Some(Value::Deferred(gty, x)) if root(nodes, *x) != *x => {
                    let fields: Vec<(String, Value)> =
                        fields.iter().map(|(k, v)| if k == "geometry" { (k.clone(), Value::Deferred(gty.clone(), root(nodes, *x))) } else { (k.clone(), v.clone()) }).collect();
                    Some(if matches!(v, Value::Source(..)) { Value::Source(op.clone(), ty.clone(), fields) } else { Value::Struct(op.clone(), ty.clone(), fields) })
                }
                _ => None,
            },
            _ => None,
        }
    };
    fn recounts(nodes: &Vec<ENode>, v: &Value, recount: &dyn Fn(&Vec<ENode>, &Value) -> Option<Value>) -> bool {
        match v {
            Value::Source(..) | Value::Struct(..) if recount(nodes, v).is_some() => true,
            Value::Residual(r) => E::Residual::residual_view(*r).map(|view| view.bindings.iter().any(|(_, v)| recounts(nodes, v, recount))).unwrap_or(false),
            Value::Fn(f) => E::Residual::function_bindings(*f).iter().any(|(_, v)| recounts(nodes, v, recount)),
            Value::List(vs) => vs.iter().any(|v| recounts(nodes, v, recount)),
            Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => fs.iter().any(|(_, v)| recounts(nodes, v, recount)),
            _ => false,
        }
    }
    for id in 0..nodes.len() {
        if !templ[id] && nodes[id].args.iter().any(|(_, v)| recounts(&nodes, v, &recount)) {
            let snapshot = nodes.clone();
            let args: Vec<(String, Value)> =
                nodes[id].args.iter().map(|(k, v)| (k.clone(), E::Host::rewrite_with(v, &|x: &Value| recount(&snapshot, x), &fresh, &|_, _| None))).collect();
            nodes[id].args = args;
        }
    }
    let uses = usage_counts(plan, &nodes, &vec![false; nodes.len()]);
    let text = |n: &ENode, k: &str| -> Option<String> {
        match get(&n.args, k) {
            Some(Value::Text(t)) => Some(txt(t)),
            _ => None,
        }
    };
    let kind_of = |n: &ENode| text(n, "type").unwrap_or_else(|| "vec3".into());
    let point_write = |n: &ENode| -> bool {
        n.kind == "sop/with_attr"
            && !templ[n.id as usize]
            && n.args.iter().all(|(k, _)| ["geometry", "attribute", "values", "owner", "type", FUSED_DELETES, FUSED_WRITES, FUSED_CHAIN, FUSED_GUARDS].contains(&k.as_str()))
            && text(n, "owner").is_none_or(|o| o == "point")
            && text(n, "attribute").is_some()
            // a write of planes ({:x .. :y ..}) is not a value a read of the attribute could take
            && !matches!(get(&n.args, "values"), Some(Value::Record(_)))
    };
    let deletes = |n: &ENode| -> Vec<Value> {
        match get(&n.args, FUSED_DELETES) {
            Some(Value::List(ds)) => (**ds).clone(),
            _ => Vec::new(),
        }
    };
    let pattern = |v: &Value| -> Option<String> {
        match v {
            Value::Record(f) if f.len() == 2 && f[1].0 == "pattern" => match &f[1].1 {
                Value::Text(p) => Some(txt(p)),
                _ => None,
            },
            _ => None,
        }
    };
    // back from a write's geometry to the earlier write: the ids passed and the deletions
    let back = |nodes: &Vec<ENode>, name: &str, start: i64| -> Option<(ENode, i64, Vec<i64>, Vec<Value>)> {
        let mut id = start;
        let mut chain: Vec<i64> = Vec::new();
        let mut dels: Vec<Value> = Vec::new();
        loop {
            if id < 0 || id >= count || uses[id as usize] != 1 || templ[id as usize] {
                return None;
            }
            let u = &nodes[id as usize];
            if u.kind == E::card_kind() {
                match get(&u.args, "value") {
                    Some(Value::Deferred(_, x)) => {
                        chain.insert(0, id);
                        id = *x;
                    }
                    _ => return None,
                }
            } else if u.kind == "sop/delete_attr" {
                match (get(&u.args, "geometry"), text(u, "attribute")) {
                    (Some(Value::Deferred(_, x)), Some(pat)) if pat != name && literal_pattern(&pat) && u.args.iter().all(|(k, _)| ["geometry", "attribute", "owner"].contains(&k.as_str())) => {
                        let owner = text(u, "owner").unwrap_or_else(|| "point".into());
                        chain.insert(0, id);
                        dels.insert(0, Value::Record(vec![("owner".into(), Value::Text(owner.into_bytes())), ("pattern".into(), Value::Text(pat.into_bytes()))]));
                        id = *x;
                    }
                    _ => return None,
                }
            } else if point_write(u) && !deletes(u).iter().any(|d| pattern(d).as_deref() == Some(name)) {
                return match get(&u.args, "geometry") {
                    Some(Value::Deferred(_, base)) => {
                        chain.insert(0, id);
                        let mut all = deletes(u);
                        all.extend(dels);
                        Some((u.clone(), *base, chain, all))
                    }
                    _ => None,
                };
            } else {
                return None;
            }
        }
    };
    // the write each write would fold into (a look-ahead: the read checks are not made)
    let mut next = vec![-1i64; nodes.len()];
    for id in 0..nodes.len() {
        let w = &nodes[id];
        if let (Some(Value::Deferred(_, g)), Some(name)) = (get(&w.args, "geometry"), text(w, "attribute")) {
            if point_write(w) {
                if let Some((u, ..)) = back(&nodes, &name, *g) {
                    next[u.id as usize] = id as i64;
                }
            }
        }
    }
    let writes_from = |nodes: &Vec<ENode>, mut id: i64| -> Vec<String> {
        let mut out = Vec::new();
        while id >= 0 {
            out.extend(text(&nodes[id as usize], "attribute"));
            id = next[id as usize];
        }
        out
    };
    // An indexed earlier write (a grid's P) folds only where a device makes it, and only into a chain
    // that writes its attribute again: otherwise the chain's output would carry it, a new array with
    // every recook (a driven noise would make P new each frame, and every reader of P cook again).
    let indexed_fold = |nodes: &Vec<ENode>, u: &ENode, w: &ENode, id: i64| -> bool {
        let values = get(&u.args, "values").expect("write values");
        let fixed = (defer || device) && static_cpu_data(values);
        let next_fixed = static_cpu_data(get(&w.args, "values").expect("write values"));
        let rewritten = device && writes_from(nodes, id).iter().any(|a| Some(a) == text(u, "attribute").as_ref());
        // Keep a static prefix cached when the following write changes. An indexed field rewritten
        // later can still fold into that device chain.
        (!(fixed && !next_fixed) || rewritten)
            && (has(&u.args, FUSED_CHAIN) || !(indexed(values) && ak::reads(values).is_empty()) || (fixed && next_fixed) || rewritten)
    };
    for id in 0..nodes.len() {
        let w = nodes[id].clone();
        let (Some(Value::Deferred(_, g)), Some(name)) = (get(&w.args, "geometry"), text(&w, "attribute")) else { continue };
        if !point_write(&w) {
            continue;
        }
        let Some((u, base, chain, dels)) = back(&nodes, &name, *g) else { continue };
        if !indexed_fold(&nodes, &u, &w, id as i64) {
            continue;
        }
        let deleted: Vec<String> = dels.iter().filter_map(&pattern).collect();
        let reads = ak::reads(get(&w.args, "values").expect("write values"));
        if reads.iter().any(|r| r == "*") || deleted.iter().any(|p| reads.contains(p)) {
            continue;
        }
        // what the chain writes, in order, over `base`
        let mut entries = fused_entries(&u.args)?;
        entries.push((text(&u, "attribute").expect("write attribute"), kind_of(&u), get(&u.args, "values").expect("write values").clone()));
        let written = |a: &str| entries.iter().rev().find(|(n, _, _)| n == a).cloned();
        let fits = Arc::new(std::sync::atomic::AtomicBool::new(true));
        let on_base = |fields: &[(String, Value)]| -> Vec<(String, Value)> {
            let mut out = vec![("geometry".to_owned(), Value::Deferred(ty::geometry(), base))];
            out.extend(fields.iter().filter(|(k, _)| k != "geometry").cloned());
            out
        };
        // A lazy term that reads the chain itself (`(map f (sop/attr in0 :P))`): its reads of a
        // written attribute become captures of the values written, and the geometry it captures is
        // the input. Any other use of the capture refuses.
        let retarget = {
            let (chain, entries, fresh, fits) = (chain.clone(), entries.clone(), fresh.clone(), fits.clone());
            move |term: &Term, bindings: &[(String, Value)]| -> Option<(Term, Vec<(String, Value)>)> {
            let written = |a: &str| entries.iter().rev().find(|(n, _, _)| n == a).cloned();
            let chained: Vec<String> =
                bindings.iter().filter_map(|b| match b {
                    (name, Value::Deferred(ty, x)) if is_geometry_ty(ty) && chain.contains(x) => Some(name.clone()),
                    _ => None,
                }).collect();
            if chained.is_empty() {
                return None;
            }
            let extra: RefCell<Vec<(String, Value)>> = RefCell::new(Vec::new());
            let exit = std::cell::Cell::new(false);
            fn binds(p: &W::Pattern, chained: &[String]) -> bool {
                match p {
                    W::Pattern::Name(n) => chained.contains(n),
                    W::Pattern::Seq(ps) => ps.iter().any(|p| binds(p, chained)),
                    W::Pattern::Keys(ns) => ns.iter().any(|n| chained.contains(n)),
                }
            }
            let literal = |t: &Term| -> Option<String> {
                match &t.node {
                    W::Node::Text(s) => Some(txt(s)),
                    _ => None,
                }
            };
            fn map(t: &Term, chained: &[String], extra: &RefCell<Vec<(String, Value)>>, exit: &std::cell::Cell<bool>, written: &dyn Fn(&str) -> Option<(String, String, Value)>, literal: &dyn Fn(&Term) -> Option<String>, fresh: &dyn Fn() -> i64) -> Term {
                let rec = |t: &Term| map(t, chained, extra, exit, written, literal, fresh);
                let reads_chain = |args: &Vec<(String, Term)>| matches!(args.iter().find(|(k, _)| k == "geometry"), Some((_, Term { node: W::Node::RefBinding(x, v), .. })) if v.is_empty() && chained.contains(x));
                match &t.node {
                    W::Node::RefBinding(x, _) if chained.contains(x) => {
                        exit.set(true);
                        t.clone()
                    }
                    W::Node::Op { op, args, .. } if op == "sop/count" && reads_chain(args) => t.clone(),
                    W::Node::Op { op, args, .. } if op == "sop/attr" && reads_chain(args) => {
                        let attribute = args.iter().find(|(k, _)| k == "attribute").and_then(|(_, a)| literal(a));
                        match attribute.as_deref().map(written) {
                            Some(None) | None if attribute.is_some() => t.clone(),
                            Some(Some((_, kind, earlier)))
                                if args.iter().all(|(k, v)| match k.as_str() {
                                    "geometry" | "attribute" => true,
                                    "owner" => literal(v).as_deref() == Some("point"),
                                    "type" => literal(v).as_deref() == Some(kind.as_str()),
                                    _ => false,
                                }) =>
                            {
                                let name = format!("$fold{}", extra.borrow().len());
                                extra.borrow_mut().push((name.clone(), E::Host::rewrite_with(&earlier, &|_| None, fresh, &|_, _| None)));
                                Term { node: W::Node::RefBinding(name, Vec::new()), ..t.clone() }
                            }
                            _ => {
                                exit.set(true);
                                t.clone()
                            }
                        }
                    }
                    W::Node::Let(steps, _) if steps.iter().any(|(p, _)| binds(p, chained)) => {
                        exit.set(true);
                        t.clone()
                    }
                    W::Node::Loop { accs, clauses, .. } if accs.iter().chain(clauses.iter()).any(|(p, _)| binds(p, chained)) => {
                        exit.set(true);
                        t.clone()
                    }
                    W::Node::Fn { params, .. } if params.iter().any(|(p, _)| binds(p, chained)) => {
                        exit.set(true);
                        t.clone()
                    }
                    W::Node::State { binder, .. } if binds(binder, chained) => {
                        exit.set(true);
                        t.clone()
                    }
                    _ => W::map_children(&rec, t),
                }
            }
            let mapped = map(term, &chained, &extra, &exit, &|a| written(a), &literal, &fresh);
            if exit.get() {
                fits.store(false, Ordering::SeqCst);
                return None;
            }
            let mut bound: Vec<(String, Value)> = chained.iter().map(|name| (name.clone(), Value::Deferred(ty::geometry(), base))).collect();
            bound.extend(extra.into_inner());
            Some((mapped, bound))
            }
        };
        let values = E::Host::rewrite_with(
            get(&w.args, "values").expect("write values"),
            &|v: &Value| match v {
                Value::Struct(op, ty, fields) | Value::Source(op, ty, fields) => {
                    let chained = matches!(get(fields, "geometry"), Some(Value::Deferred(_, x)) if chain.contains(x));
                    let rebuilt = |fields: Vec<(String, Value)>| if matches!(v, Value::Struct(..)) { Value::Struct(op.clone(), ty.clone(), fields) } else { Value::Source(op.clone(), ty.clone(), fields) };
                    if !chained {
                        return None;
                    }
                    match (op.as_str(), get(fields, "attribute")) {
                        ("sop/attr", Some(Value::Text(a))) => match written(&txt(a)) {
                            None => Some(rebuilt(on_base(fields))),
                            Some((_, kind, earlier))
                                if fields.iter().all(|(k, v)| match k.as_str() {
                                    "geometry" | "attribute" => true,
                                    "owner" => *v == Value::Text(b"point".to_vec()),
                                    "type" => *v == Value::Text(kind.clone().into_bytes()),
                                    _ => false,
                                }) =>
                            {
                                Some(E::Host::rewrite_with(&earlier, &|_| None, &fresh, &|_, _| None))
                            }
                            Some(_) => {
                                fits.store(false, Ordering::SeqCst);
                                Some(v.clone())
                            }
                        },
                        ("sop/count", _) => Some(rebuilt(on_base(fields))),
                        _ => {
                            fits.store(false, Ordering::SeqCst);
                            Some(v.clone())
                        }
                    }
                }
                Value::Deferred(ty, x) if is_geometry_ty(ty) && chain.contains(x) => {
                    fits.store(false, Ordering::SeqCst);
                    None
                }
                _ => None,
            },
            &fresh,
            &retarget,
        );
        // an attribute's last write wins; this write supersedes the chain's of `name`
        fn last(entries: &[(String, String, Value)], name: &str) -> Vec<(String, String, Value)> {
            let mut out = Vec::new();
            for (i, e) in entries.iter().enumerate() {
                if e.0 == name || entries[i + 1..].iter().any(|m| m.0 == e.0) {
                    continue;
                }
                out.push(e.clone());
            }
            out
        }
        let earlier: Vec<Value> = last(&entries, &name).into_iter().map(|(n, k, v)| fused_entry(&n, &k, v)).collect();
        // the folded writes, whose cards key this one's kernels with its own
        let mut chain_ids: Vec<Value> = match get(&u.args, FUSED_CHAIN) {
            Some(Value::List(ids)) => (**ids).clone(),
            _ => Vec::new(),
        };
        chain_ids.push(Value::Int(u.id));
        // the deferred checks of the folded writes and of the cards passed
        let mut guards = guards_of(FUSED_GUARDS, &w.args);
        guards.extend(guards_of(FUSED_GUARDS, &u.args));
        for i in &chain {
            if nodes[*i as usize].kind == E::card_kind() {
                guards.extend(guards_of("guards", &nodes[*i as usize].args));
            }
        }
        if fits.load(Ordering::SeqCst) {
            let mut args: Vec<(String, Value)> = w
                .args
                .iter()
                .filter_map(|(k, v)| {
                    if k == "geometry" {
                        Some((k.clone(), Value::Deferred(ty::geometry(), base)))
                    } else if k == "values" {
                        Some((k.clone(), values.clone()))
                    } else if [FUSED_DELETES, FUSED_WRITES, FUSED_CHAIN, FUSED_GUARDS].contains(&k.as_str()) {
                        None
                    } else {
                        Some((k.clone(), v.clone()))
                    }
                })
                .collect();
            args.push((FUSED_CHAIN.into(), Value::List(Arc::new(chain_ids))));
            if !earlier.is_empty() {
                args.push((FUSED_WRITES.into(), Value::List(Arc::new(earlier))));
            }
            if !dels.is_empty() {
                args.push((FUSED_DELETES.into(), Value::List(Arc::new(dels))));
            }
            if !guards.is_empty() {
                args.push((FUSED_GUARDS.into(), Value::List(Arc::new(guards))));
            }
            nodes[id].args = args;
        }
    }
    Ok(Plan { nodes, ..plan.clone() })
}

fn same_function(a: FnId, b: FnId) -> bool {
    a == b
        || (E::Residual::function_id(a) == E::Residual::function_id(b)
            && E::Residual::function_origin(a) == E::Residual::function_origin(b)
            && E::Host::function_scope(a) == E::Host::function_scope(b)
            && E::Residual::function_name(a) == E::Residual::function_name(b)
            && {
                let (x, y) = (E::Residual::function_bindings(a), E::Residual::function_bindings(b));
                x.len() == y.len() && x.iter().zip(y.iter()).all(|((an, av), (bn, bv))| an == bn && av == bv)
            })
}

pub fn objects(lowered: &T) -> Vec<(usize, network::Network, i64)> {
    lowered.graphs.iter().filter_map(|g| g.root.map(|root| (g.instance, g.network.clone(), root))).collect()
}

/// One host call lowers a synthetic workspace. Memoize the source digest by the checked workspace
/// content: 8 entries (each pins a workspace's forms).
static DIGEST_CACHE: Mutex<Vec<(Vec<usize>, String)>> = Mutex::new(Vec::new());

fn cached_digest(checked: &Workspace, extra: &[NodeDefinition], compute: &dyn Fn() -> String) -> String {
    // Keyed by the identity (address) of the forms the lowering shares: a one-call lowering rebuilds the
    // lists (graphs = [call]) but shares their forms.
    let mut key: Vec<usize> = Vec::new();
    key.extend(checked.source.iter().map(|s| s as *const Syntax as usize));
    key.push(0);
    key.extend(checked.macros.iter().map(|s| s as *const Syntax as usize));
    key.push(1);
    key.extend(checked.defs.iter().map(|g| g as *const WGraph as usize));
    key.push(2);
    key.extend(checked.nodes.iter().map(|g| g as *const NodeDefinition as usize));
    key.push(3);
    key.extend(checked.graphs.iter().map(|g| g as *const WGraph as usize));
    key.push(4);
    key.extend(extra.iter().map(|g| g as *const NodeDefinition as usize));
    if let Some((_, d)) = DIGEST_CACHE.lock().unwrap().iter().find(|(k, _)| *k == key) {
        return d.clone();
    }
    let d = compute();
    let mut cache = DIGEST_CACHE.lock().unwrap();
    cache.insert(0, (key, d.clone()));
    cache.truncate(8);
    d
}

// ---------------------------------------------------------------------------------------------
// One lowering of a checked workspace: the qualified plan, the per-kind node builders and the zone,
// kernel and preview machinery over it. `lower_checked` recurses for cooked calls and a node's own
// cook (one inlined call).

#[derive(Clone)]
pub struct Options {
    pub provenance_tags: bool,
    pub tag_base: i64,
    pub reference: bool,
    pub gpu: Gpu::Config,
    pub defer: bool,
    pub strict_body: bool,
    pub defer_body_errors: bool,
    pub kernel_bindings: Vec<(FnId, i64)>,
    pub clock: Clock,
    pub on_profile: Option<OnProfile>,
    pub compiled_ids: PathMap,
    pub sites: Vec<W::Path>,
    pub inputs: Option<Vec<(String, Vec<(String, Value)>)>>,
}

pub fn default_clock() -> Clock {
    Arc::new(|| std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_or(0.0, |d| d.as_secs_f64()))
}

impl Default for Options {
    fn default() -> Options {
        Options {
            provenance_tags: true,
            tag_base: 0,
            reference: false,
            gpu: Gpu::default(),
            defer: false,
            strict_body: false,
            defer_body_errors: false,
            kernel_bindings: Vec::new(),
            clock: default_clock(),
            on_profile: None,
            compiled_ids: PathMap::new(),
            sites: Vec::new(),
            inputs: None,
        }
    }
}

type Lazy = Arc<dyn Fn() -> String + Send + Sync>;

fn lazy(f: Box<dyn FnOnce() -> String + Send + Sync>) -> Lazy {
    let cell = Arc::new(std::sync::LazyLock::new(f));
    Arc::new(move || (**cell).clone())
}

#[derive(Clone)]
enum Kind {
    Arrays,
    Metadata,
    Attribute,
    Group,
    Delete,
    Curve,
    Zone,
    Resource,
    Card,
}

struct Ids {
    ids: PathMap,
    site_table: HashMap<W::Path, i64>,
    site_list: Vec<W::Path>,
}

type HostHandle = (Ty, Option<node::T>, Option<sop::session::Output>);

pub struct Lowerer {
    o: Options,
    factories: Vec<Factory>,
    checked: Arc<Workspace>,
    evaluated: Arc<E::Eval>,
    profile: Arc<Profile::T>,
    plan: Plan,
    templ: Vec<bool>,
    compiled: Vec<i64>,
    kernel_source: Lazy,
    card_sources: OnceLock<HashMap<W::Path, Vec<(Vec<i64>, Lazy)>>>,
    ids: Mutex<Ids>,
    zinfos: Mutex<HashMap<i64, Zinfo>>,
    params_cache: Mutex<HashMap<String, Vec<port::Parameter>>>,
    pending: Mutex<Vec<Pending>>,
    provenance: Mutex<BTreeMap<i64, Origin>>,
    tags: AtomicI64,
    zones: Mutex<Vec<Zone>>,
    prepared: Mutex<HashMap<i64, Prepared>>,
    concat_tags: Mutex<HashMap<(W::Path, Vec<i64>), i64>>,
    lisp_factories: Vec<Factory>,
    call_env: Arc<CallEnvShared>,
    image_origins: Arc<Mutex<HashMap<(i64, W::Path, Vec<i64>), Option<W::Path>>>>,
    volatile_nodes: Mutex<BTreeMap<i64, ()>>,
    volatile: Mutex<Vec<bool>>,
}

/// What `build_call` needs of the enclosing lowering, shared by every Lisp factory of it.
/// A node's back-reference to its lowering: weak, because the lowering stores its nodes (a strong one is a cycle
/// that leaks every lowering). The `T` that `finish` returns keeps the lowering alive through its `preview` and
/// `card_node`; a node used after that `T` is gone is a bug and panics.
#[derive(Clone)]
struct Wk(Weak<Lowerer>);
impl Wk {
    fn new(l: &Arc<Lowerer>) -> Wk {
        Wk(Arc::downgrade(l))
    }
    fn get(&self) -> Arc<Lowerer> {
        self.0.upgrade().expect("a lowered node outlived its lowering")
    }
}

pub struct CallEnvShared {
    checked: Arc<Workspace>,
    factories: Vec<Factory>,
    o: Options,
    def_context_use: Mutex<Option<Vec<(WGraph, bool)>>>,
}


impl Lowerer {
    fn deferred_cpu(&self, data: &Value) -> bool {
        self.o.defer && E::is_live(data) && static_cpu_data(data)
    }

    /// A node of an inlined body (its site is under a card's `node:kind`) compiles strictly; its
    /// refusals cook as errors rather than refusing the program.
    fn inlined(node: Option<&ENode>) -> bool {
        node.is_some_and(|n| n.site.iter().any(|s| s.starts_with("node:")))
    }

    /// An inlined body's kernels key on its card (declaration cone and canonical parameters), not on
    /// the whole authored source, so an edit elsewhere keeps them cached, as the card was.
    fn card_sources(&self) -> &HashMap<W::Path, Vec<(Vec<i64>, Lazy)>> {
        self.card_sources.get_or_init(|| {
            let mut table: HashMap<W::Path, Vec<(Vec<i64>, Lazy)>> = HashMap::new();
            let checked = self.checked.clone();
            let authored: Lazy = {
                let checked = checked.clone();
                lazy(Box::new(move || E::authored_key(&checked)))
            };
            for n in &self.evaluated.plan.nodes {
                if n.kind != E::card_kind() {
                    continue;
                }
                if let (Some(Value::Text(kind)), Some(parameters)) = (get(&n.args, "kind"), get(&n.args, "parameters")) {
                    let kind = txt(kind);
                    if let Some(definition) = checked.nodes.iter().find(|d| d.kind.qualified == kind) {
                        // terms (a live argument's, a closure's) are their structure and what authored code they may call
                        fn terms(v: &Value) -> bool {
                            match v {
                                Value::Residual(_) | Value::Fn(_) => true,
                                Value::List(xs) => xs.iter().any(terms),
                                Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => fs.iter().any(|(_, v)| terms(v)),
                                _ => false,
                            }
                        }
                        let (checked2, definition2, parameters2, authored2) = (checked.clone(), definition.clone(), parameters.clone(), authored.clone());
                        let source = lazy(Box::new(move || {
                            let key = if terms(&parameters2) {
                                format!("{}{}", E::structure(&parameters2), authored2())
                            } else {
                                txt(&flow_syntax::value::key_of(&|_| unreachable!("no residual in a static key"), &parameters2))
                            };
                            digest(&format!("{}{}", declaration_key(&checked2, &definition2), key))
                        }));
                        let mut site = n.site.clone();
                        site.push(format!("node:{kind}"));
                        table.entry(site).or_default().push((n.iter.clone(), source));
                    }
                }
            }
            table
        })
    }

    fn kernel_source_of(&self, node: Option<&ENode>) -> Lazy {
        if let Some(n) = node {
            if has(&n.args, FUSED_CHAIN) {
                // a fused write keys on every card it folds
                let chain: Vec<ENode> = match get(&n.args, FUSED_CHAIN) {
                    Some(Value::List(ids)) => ids
                        .iter()
                        .filter_map(|v| match v {
                            Value::Int(id) => Some(self.evaluated.plan.nodes[*id as usize].clone()),
                            _ => None,
                        })
                        .collect(),
                    _ => Vec::new(),
                };
                let own = self.kernel_source_of(Some(&ENode { args: without(&n.args, FUSED_CHAIN), ..n.clone() }));
                let sources: Vec<Lazy> = chain.iter().map(|u| self.kernel_source_of(Some(u))).collect();
                return lazy(Box::new(move || {
                    let mut parts: Vec<String> = sources.iter().map(|s| s()).collect();
                    parts.push(own());
                    digest(&parts.join(";"))
                }));
            }
            if Self::inlined(Some(n)) || n.kind == E::card_kind() {
                // the innermost card: the site up to its last node: segment
                let site: Option<W::Path> = match get(&n.args, "kind") {
                    Some(Value::Text(kind)) if n.kind == E::card_kind() => {
                        let mut s = n.site.clone();
                        s.push(format!("node:{}", txt(kind)));
                        Some(s)
                    }
                    _ => {
                        let mut best = None;
                        for i in 0..n.site.len() {
                            if n.site[i].starts_with("node:") {
                                best = Some(n.site[..=i].to_vec());
                            }
                        }
                        best
                    }
                };
                return match site {
                    None => self.kernel_source.clone(),
                    Some(site) => match self.card_sources().get(&site).and_then(|entries| entries.iter().find(|(iter, _)| n.iter.starts_with(iter))) {
                        Some((_, source)) => source.clone(),
                        None => self.kernel_source.clone(),
                    },
                };
            }
        }
        self.kernel_source.clone()
    }

    /// An inlined body's kernel node, built once for its card: an edit elsewhere relowers the program
    /// but takes the node it built (over its new inputs), and a device keeps the kernels it compiled
    /// for it. The key is what the build reads: the card's key, the data's structure, and of each
    /// input its id and the facts along its pass-throughs; a node built for a device is taken only
    /// for that device.
    fn reuse(&self, node: Option<&ENode>, state: &Option<State>, elems: &Option<Vec<(String, Value)>>, what: &str, data: &Value, inputs: &[node::T], build_fn: &dyn Fn() -> R<node::T>) -> R<node::T> {
        fn input_key(input: &node::T) -> String {
            let facts = node::facts(input);
            let inputs = build::input_array(input);
            let mut s = format!("{}{:?}", node::id(input), facts);
            if let CookMode::DuplicateInput(i) | CookMode::InPlace(i) | CookMode::InstanceInput(i) | CookMode::Passthrough(i) = facts.cook_mode {
                if i >= 0 && (i as usize) < inputs.len() {
                    s.push('<');
                    s.push_str(&input_key(&inputs[i as usize]));
                }
            }
            s
        }
        let card = node.is_some_and(|n| n.kind == E::card_kind());
        if !(Self::inlined(node) || card) || state.is_some() || elems.is_some() {
            return build_fn();
        }
        let mut parts = vec![
            (self.kernel_source_of(node))(),
            what.to_owned(),
            E::structure(data),
            self.o.reference.to_string(),
            self.o.strict_body.to_string(),
            self.o.defer.to_string(),
            self.o.gpu.fast.to_string(),
            (self.o.gpu.policy == Gpu::Policy::Qualification).to_string(),
        ];
        parts.extend(inputs.iter().map(input_key));
        let key = parts.join("\0");
        if let Some((backend, n)) = built_find(&key) {
            let same = match (&backend, &self.o.gpu.backend) {
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                (None, None) => true,
                _ => false,
            };
            if same {
                return Ok(build::rebuild_with_inputs(&n, inputs));
            }
        }
        let n = build_fn()?;
        built_add(key, (self.o.gpu.backend.clone(), n.clone()));
        Ok(n)
    }

    fn kenv(&self, node: Option<&ENode>, strict: bool) -> KernelEnv {
        KernelEnv {
            gpu: self.o.gpu.clone(),
            profile: self.profile.clone(),
            clock: self.o.clock.clone(),
            strict_body: strict || Self::inlined(node),
            reference: self.o.reference,
            approx: self.checked.approx.clone(),
            kernel_source: self.kernel_source_of(node),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn arrays_node(&self, node: Option<&ENode>, strict: Option<bool>, host: bool, state: Option<State>, elems: Option<Vec<(String, Value)>>, operation: &str, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
        let strict_param = strict.unwrap_or(self.o.strict_body);
        let what = format!("arrays {operation}{}", if host { "true" } else { "" });
        let env = self.kenv(node, strict_param);
        let opts = NodeOpts { check: Some(strict_param), host, state: state.clone(), elems: elems.clone() };
        self.reuse(node, &state, &elems, &what, &Value::Record(args.to_vec()), inputs, &|| geometry_arrays_node(&env, &opts, operation, args, sources, inputs))
    }

    #[allow(clippy::too_many_arguments)]
    fn attr_node(&self, node: Option<&ENode>, mut o: AttrOpts, name: &str, values: &Value, sources: &[i64], inputs: &[node::T]) -> R<node::T> {
        let mut fields = vec![("values".to_owned(), values.clone())];
        for (_, _, v) in &o.prior {
            fields.push(("prior".into(), v.clone()));
        }
        if let Some(v) = &o.guard {
            fields.push(("when".into(), v.clone()));
        }
        if let Some(v) = &o.selection {
            fields.push(("selection".into(), v.clone()));
        }
        if let Some(v) = &o.default {
            fields.push(("default".into(), v.clone()));
        }
        let data = Value::Record(fields);
        let guards = node.map(|n| guards_of(FUSED_GUARDS, &n.args)).unwrap_or_default();
        let owner = o.owner.unwrap_or(rdk::attribute::Owner::Point);
        let kind = o.kind.clone().unwrap_or_else(|| "vec3".into());
        let deletes = o.deletes.clone().unwrap_or_default();
        let what = format!(
            "attribute {:?} {} {} {} {}",
            owner,
            kind,
            name,
            deletes.iter().map(|(a, b)| format!("{a}:{b}")).collect::<Vec<_>>().join(","),
            o.prior.iter().map(|(n, k, _)| format!("{n}:{k}")).collect::<Vec<_>>().join(",")
        );
        o.check = Some(self.o.strict_body);
        let env = self.kenv(node, self.o.strict_body);
        let (state, elems) = (o.state.clone(), o.elems.clone());
        let built = self.reuse(node, &state, &elems, &what, &data, inputs, &|| attribute_node(&env, &o, name, values, sources, inputs))?;
        Ok(guarded(guards, built))
    }

    fn grp_node(&self, node: Option<&ENode>, state: Option<State>, elems: Option<Vec<(String, Value)>>, args: &[(String, Value)], sources: &[i64], inputs: &[node::T]) -> R<node::T> {
        let env = self.kenv(node, self.o.strict_body);
        let opts = NodeOpts { check: Some(self.o.strict_body), host: false, state: state.clone(), elems: elems.clone() };
        self.reuse(node, &state, &elems, "group", &Value::Record(args.to_vec()), inputs, &|| group_node(&env, &opts, args, sources, inputs))
    }

    fn external_kernel(&self, f: FnId) -> Option<i64> {
        self.o.kernel_bindings.iter().find_map(|(candidate, source)| if same_function(*candidate, f) { Some(*source) } else { None })
    }

    fn int_arg(n: &ENode, k: &str) -> R<i64> {
        match get(&n.args, k) {
            Some(Value::Int(i)) => Ok(*i),
            _ => Err(fail("E_LOWER", &format!("Zone node without {k}"))),
        }
    }

    fn compiled_id(&self, node: &ENode) -> i64 {
        let mut ids = self.ids.lock().unwrap();
        let site = match ids.site_table.get(&node.site) {
            Some(i) => *i,
            None => {
                let i = ids.site_list.len() as i64;
                ids.site_table.insert(node.site.clone(), i);
                ids.site_list.push(node.site.clone());
                i
            }
        };
        let mut key = vec![node.inst, site];
        key.extend(node.iter.iter().map(|k| -(k + 1)));
        if let Some(id) = ids.ids.get(&key) {
            return *id;
        }
        let id = build::fresh_id();
        ids.ids.insert(key, id);
        id
    }

    /// What a zone reads: its collection and the outside geometry of its template.
    fn deps(&self, n: &ENode) -> R<Vec<i64>> {
        if is_zone(&n.kind) {
            let src = match n.args.first() {
                Some((_, Value::Deferred(Ty::Named(name), src))) if name == "geometry" => *src,
                _ => return Err(fail("E_LOWER", "Zone without geometry")),
            };
            let mut out = vec![src];
            out.extend(self.zinfo(n)?.captures);
            Ok(out)
        } else if is_geometry_arrays(&n.kind) {
            let direct = geo_of(&n.args);
            let mut out = direct.clone();
            out.extend(ak::sources(&geometry_arrays_data(&n.args)).into_iter().filter(|id| !direct.contains(id)));
            Ok(out)
        } else if n.kind == "sop/delete_attr" && has(&n.args, "when") {
            let direct = geo_of(&[("geometry".to_owned(), get(&n.args, "geometry").cloned().expect("geometry"))]);
            let mut out = direct.clone();
            out.extend(ak::sources(get(&n.args, "when").expect("when")).into_iter().filter(|id| !direct.contains(id)));
            Ok(out)
        } else if n.kind == "sop/with_attr" || n.kind == "sop/with_group" {
            let geometry = get(&n.args, "geometry").expect("geometry");
            let root = match geometry {
                Value::Deferred(_, id) => Some(*id),
                Value::No_geo => None,
                _ => return Err(fail("E_LOWER", "Attribute write without geometry")),
            };
            let mut out: Vec<i64> = root.into_iter().collect();
            let data = if n.kind == "sop/with_group" { group_data(&n.args) } else { attribute_data(&n.args) };
            out.extend(ak::sources(&data).into_iter().filter(|id| Some(*id) != root));
            Ok(out)
        } else if n.kind == E::card_kind() {
            Ok(cooked_sources(get(&n.args, "value").expect("card value")))
        } else {
            let direct = geo_of(&n.args);
            fn captures(this: &Lowerer, v: &Value) -> Vec<i64> {
                match v {
                    Value::Fn(f) => match this.external_kernel(*f) {
                        Some(source) => vec![source],
                        None => ak::sources(v),
                    },
                    Value::List(values) => values.iter().flat_map(|v| captures(this, v)).collect(),
                    _ => Vec::new(),
                }
            }
            let mut caps: Vec<i64> = n.args.iter().flat_map(|(_, v)| captures(self, v)).collect();
            caps.sort();
            caps.dedup();
            let mut out = direct.clone();
            out.extend(caps.into_iter().filter(|id| !direct.contains(id)));
            Ok(out)
        }
    }

    fn zinfo(&self, n: &ENode) -> R<Zinfo> {
        if let Some(z) = self.zinfos.lock().unwrap().get(&n.id) {
            return Ok(z.clone());
        }
        let (lo, hi, root) = (Self::int_arg(n, "lo")?, Self::int_arg(n, "hi")?, Self::int_arg(n, "body")?);
        let mut seen: BTreeSet<i64> = BTreeSet::new();
        let mut caps: Vec<i64> = Vec::new();
        fn visit(this: &Lowerer, j: i64, lo: i64, hi: i64, seen: &mut BTreeSet<i64>, caps: &mut Vec<i64>) -> R<()> {
            if j >= lo && j < hi {
                if seen.insert(j) {
                    for d in this.deps(&this.plan.nodes[j as usize])? {
                        visit(this, d, lo, hi, seen, caps)?;
                    }
                }
            } else if !caps.contains(&j) {
                caps.push(j);
            }
            Ok(())
        }
        visit(self, root, lo, hi, &mut seen, &mut caps)?;
        let ekey = text_arg(&n.args, "element");
        let live = (lo..hi).any(|id| self.plan.nodes[id as usize].args.iter().any(|(_, v)| E::frame_dependent(v)));
        let mut reads = deps_mod::STATIC;
        for id in lo..hi {
            for (_, v) in &self.plan.nodes[id as usize].args {
                reads = dep_union(reads, ak::input_reads(v));
            }
        }
        let stateful = (lo..hi).any(|id| self.plan.nodes[id as usize].args.iter().any(|(_, v)| E::state_dependent(v)));
        let z = Zinfo {
            plan_id: n.id,
            lo,
            hi,
            preview: None,
            root,
            live,
            reads,
            stateful,
            order: seen.into_iter().collect(),
            captures: caps,
            ekey,
            kind: if n.kind == "zone/points" { szone::Kind::Points } else { szone::Kind::Pieces },
            key: text_arg(&n.args, "key"),
            base: 0,
            count: Arc::new(AtomicI64::new(-1)),
            positions: Arc::new(Mutex::new(Arc::new(Vec::new()))),
        };
        self.zinfos.lock().unwrap().insert(n.id, z.clone());
        Ok(z)
    }

    fn parameters(&self, factory: &Factory) -> R<Vec<port::Parameter>> {
        let key = edit::factory_key(factory).to_owned();
        if let Some(p) = self.params_cache.lock().unwrap().get(&key) {
            return Ok(p.clone());
        }
        let parameters = port::parameters(false, edit::factory_fields(factory))?;
        self.params_cache.lock().unwrap().insert(key, parameters.clone());
        Ok(parameters)
    }

    fn lisp_definition(&self, kind: &str) -> Option<NodeDefinition> {
        self.checked.nodes.iter().find(|d| d.kind.qualified == kind).cloned()
    }

    /// The editor's provenance: a merge tags its input i with base + i.
    fn tag_sources(&self, cid: i64, args: &[(String, Value)]) -> i64 {
        let sources = geo_of(args);
        let base = self.tags.load(Ordering::SeqCst);
        self.tags.store(base + sources.len() as i64, Ordering::SeqCst);
        let mut provenance = self.provenance.lock().unwrap();
        for (index, source) in sources.iter().enumerate() {
            let s = &self.plan.nodes[*source as usize];
            provenance.insert(base + index as i64, Origin { merge: cid, input: index as i64, source: self.compiled[*source as usize], site: s.site.clone(), iter: s.iter.clone() });
        }
        base
    }

    fn structural_node(&self, node: Option<&ENode>, residual: ResidualId, captures: &[(i64, Ty, node::T)]) -> R<node::T> {
        let kernel_source = self.kernel_source_of(node);
        let (profile, gpu, reference, strict_clock, factories) = (self.profile.clone(), self.o.gpu.clone(), self.o.reference, self.o.clock.clone(), self.factories.clone());
        let checked = self.checked.clone();
        let captures: Vec<(i64, Ty, node::T)> = captures.to_vec();
        let failure: Arc<Mutex<Option<Diagnostic>>> = Arc::new(Mutex::new(None));
        let slot = failure.clone();
        let built = per_context(
            &Value::Residual(residual),
            Arc::new(move |specialize: &dyn Fn(&Value) -> Value| {
                let go = || -> R<node::T> {
                    let residual = match specialize(&Value::Residual(residual)) {
                        Value::Residual(r) => r,
                        _ => unreachable!("a residual specializes to a residual"),
                    };
                    let program = flow_ir::core_lower::lower(Some(Value::Residual(residual)), None, true, Some(32768), None, residual)
                        .map_err(|name| fail("E_PACKED_FORM", &format!("Cooked selection cannot compile {name}.")))?;
                    let prepared = Arc::new(flow_ir::core_exec::prepare(&program, false, false, &W::Paths::new(), false));
                    let inputs: Vec<node::T> = captures.iter().map(|(_, _, n)| n.clone()).collect();
                    let state_slots = program.state_slots.len();
                    let n = build::make(
                        build::Args {
                            parameters: kernel_source().into_bytes(),
                            cook_mode: CookMode::Generic,
                            inputs,
                            dependencies: ak::dependencies(&Value::Residual(residual)),
                            ..build::args("flow.geometry_select")
                        },
                        None,
                        {
                            let (profile, gpu, checked, captures, prepared, clock, factories) = (profile.clone(), gpu.clone(), checked.clone(), captures.clone(), prepared.clone(), strict_clock.clone(), factories.clone());
                            Arc::new(move |node_id, context, payloads| {
                                structural_cook(&StructEnv { profile: profile.clone(), gpu: gpu.clone(), reference, checked: checked.clone(), captures: captures.clone(), prepared: prepared.clone(), clock: clock.clone(), residual, state_slots, factories: factories.clone() }, node_id, context, payloads)
                            })
                        },
                    );
                    Ok(build::with_facts(Facts { reads: vec!["*".into()], writes: vec!["*".into()], ..node::facts(&n).clone() }, &n))
                };
                match go() {
                    Ok(n) => n,
                    Err(e) => {
                        *slot.lock().unwrap() = Some(e);
                        build::make(build::args("flow.failed"), None, Arc::new(|_, _, _| Err(fail("E_LOWER", "failed node"))))
                    }
                }
            }),
        );
        match failure.lock().unwrap().take() {
            Some(e) => Err(e),
            None => Ok(built),
        }
    }
}

// ---------------------------------------------------------------------------------------------
// A cooked selection (a geometry decided by cooked data) executes its core program on the host,
// resolving cooked handles and host calls through a one-cook session.

#[derive(Clone)]
struct StructEnv {
    profile: Arc<Profile::T>,
    gpu: Gpu::Config,
    reference: bool,
    checked: Arc<Workspace>,
    captures: Vec<(i64, Ty, node::T)>,
    prepared: Arc<flow_ir::core_exec::Prepared>,
    clock: Clock,
    residual: ResidualId,
    state_slots: usize,
    factories: Vec<Factory>,
}

struct HostRun {
    env: StructEnv,
    context: sctx::T,
    handles: Mutex<HashMap<i64, HostHandle>>,
    geometries: Mutex<HashMap<i64, rdk::geometry::T>>,
    next: AtomicI64,
    session: Mutex<Option<sop::session::T>>,
    /// a failure raised inside a `geometry` lookup, reported once the resolver returns
    stash: Mutex<Option<Diagnostic>>,
}

fn data_source(message: &str) -> Diagnostic {
    fail("E_DATA_SOURCE", message)
}

impl HostRun {
    fn handle_exists(&self, ty: &Ty, id: i64) -> R<()> {
        match self.handles.lock().unwrap().get(&id) {
            Some((actual, _, _)) if actual == ty => Ok(()),
            Some(_) => Err(data_source("A cooked handle has the wrong declared type.")),
            None => Err(data_source("A cooked handle is missing from this execution.")),
        }
    }

    fn snapshot(&self, ty: &Ty, id: i64) -> R<sop::session::Output> {
        self.handle_exists(ty, id)?;
        let validate = |value: sop::session::Output| -> R<sop::session::Output> {
            match (&value.payload, is_geometry_ty(ty), *ty == ty::image()) {
                (payload::T::Geometry(_), true, _) | (payload::T::Image(_), _, true) => Ok(value),
                _ => Err(data_source("A cooked handle returned a payload of the wrong type.")),
            }
        };
        let (node, output) = {
            let handles = self.handles.lock().unwrap();
            let (_, node, output) = handles.get(&id).unwrap();
            (node.clone(), output.clone())
        };
        match (output, node) {
            (Some(output), _) => validate(output),
            (None, None) => Err(data_source("A cooked handle has no result or prepared node.")),
            (None, Some(node)) => {
                let value = {
                    let mut session = self.session.lock().unwrap();
                    let session = session.get_or_insert_with(|| {
                        let mut s = sop::session::create(0, 0).expect("a session with no entries");
                        sop::session::set_parallel_override(&mut s, Some(false));
                        s
                    });
                    sop::session::cook(session, None, &self.context, &node)?
                };
                let value = validate(value)?;
                self.handles.lock().unwrap().get_mut(&id).unwrap().2 = Some(value.clone());
                Ok(value)
            }
        }
    }

    fn geometry_r(&self, id: i64) -> R<rdk::geometry::T> {
        if let Some(g) = self.geometries.lock().unwrap().get(&id) {
            return Ok(g.clone());
        }
        let output = self.snapshot(&ty::geometry(), id)?;
        let geometry = match (output.payload, output.instances) {
            (payload::T::Geometry(g), None) => g,
            (payload::T::Geometry(source), Some(transforms)) => {
                rdk::instance_copy::materialize_instances(&transforms, &source).map_err(|e| data_source(&rdk::error::to_string(&e)))?
            }
            _ => return Err(data_source("A geometry source has an image payload.")),
        };
        self.geometries.lock().unwrap().insert(id, geometry.clone());
        Ok(geometry)
    }

    /// `geometry id` of the OCaml: a failure is stashed and reported when the resolver returns.
    fn geometry(&self, id: i64) -> Option<rdk::geometry::T> {
        match self.geometry_r(id) {
            Ok(g) => Some(g),
            Err(e) => {
                self.stash.lock().unwrap().get_or_insert(e);
                None
            }
        }
    }

    fn register_output(&self, ty: Ty, output: sop::session::Output) -> Value {
        let id = self.next.fetch_sub(1, Ordering::SeqCst);
        self.handles.lock().unwrap().insert(id, (ty.clone(), None, Some(output)));
        Value::Deferred(ty, id)
    }

    fn register_node(&self, ty: Ty, n: node::T) -> Value {
        let id = self.next.fetch_sub(1, Ordering::SeqCst);
        self.handles.lock().unwrap().insert(id, (ty.clone(), Some(n), None));
        Value::Deferred(ty, id)
    }

    fn cooked_term(self: &Arc<Self>, factories: &mut Vec<Factory>, ty: &Ty, value: &Value) -> R<Option<Term>> {
        match value {
            Value::Deferred(actual, id) if ty::is_cooked(actual) => {
                if actual != ty {
                    return Err(data_source("A cooked argument has the wrong declared type."));
                }
                self.handle_exists(actual, *id)?;
                let key = format!("lisp_host_capture_{}", factories.len());
                // Editable zero-input factories must return zero-input nodes: compiling a borrowed
                // source directly would disconnect it.
                let (run, actual_c, id_c, key_c) = (Arc::downgrade(self), actual.clone(), *id, key.clone());
                let factory = edit::factory(
                    edit::factory_args(&key, &key, &["Lisp"]),
                    0,
                    Arc::new(move |_| {
                        let (run, actual, key) = (run.clone(), actual_c.clone(), key_c.clone());
                        Ok(build::make(
                            build::Args { parameters: id_c.to_string().into_bytes(), cook_mode: CookMode::Generator, ..build::args(&key) },
                            None,
                            Arc::new(move |_, _, _| run.upgrade().ok_or_else(|| data_source("A cooked handle outlived its execution."))?.snapshot(&actual, id_c).map(|o| node::Cooked { payload: o.payload, instances: o.instances })),
                        ))
                    }),
                );
                factories.push(factory);
                let kind = format!("sop/{key}");
                let ctx = if *actual == ty::image() { fctx::image() } else { fctx::sop() };
                Ok(Some(blank_term(ty.clone(), W::Node::Call { kind, ctx, args: Vec::new() })))
            }
            Value::No_geo if is_geometry_ty(ty) => Ok(Some(Term { path: None, ty: ty.clone(), form: std::sync::Arc::new(syntax::make(SNode::Sym("nil".into()))), node: W::Node::Nil })),
            Value::List(values) => {
                let element = match ty {
                    Ty::List(element) => (**element).clone(),
                    _ => return Err(data_source("A cooked list has the wrong type.")),
                };
                let mut terms = Vec::new();
                for v in values.iter() {
                    terms.push(self.cooked_term(factories, &element, v)?);
                }
                if terms.iter().all(|t| t.is_some()) {
                    Ok(Some(blank_term(ty.clone(), W::Node::ListLit(terms.into_iter().map(Option::unwrap).collect()))))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn deferred(v: &Value) -> bool {
        match v {
            Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => true,
            Value::Record(fields) => fields.iter().any(|(_, v)| Self::deferred(v)),
            Value::List(values) => values.iter().any(Self::deferred),
            _ => false,
        }
    }

    fn constant(v: &Value) -> bool {
        matches!(v, Value::Source(name, _, _) if name.starts_with("$core/constant-array/"))
    }

    fn resolve_fn(self: &Arc<Self>) -> flow_ir::core_exec::ResolveFn {
        let run = self.clone();
        Arc::new(move |v: &Value| run.resolve(v))
    }

    fn force_args(self: &Arc<Self>, keep_constant: bool, args: &[(String, Value)]) -> R<Vec<(String, Value)>> {
        args.iter()
            .map(|(name, value)| {
                let forced = if Self::deferred(value) && !(keep_constant && name == "values" && Self::constant(value)) {
                    let r = E::Host::force_reference(value, sctx::input(&self.context), None, None, Some(self.resolve_fn()));
                    self.take_stash(r)?
                } else {
                    value.clone()
                };
                Ok((name.clone(), forced))
            })
            .collect()
    }

    fn take_stash<A>(&self, r: R<A>) -> R<A> {
        match self.stash.lock().unwrap().take() {
            Some(e) if r.is_err() => Err(e),
            _ => r,
        }
    }

    /// sop/mesh, sop/mesh_planes and sop/remap build their node directly from the evaluated
    /// arguments: the former synthetic one-node workspace was checked, evaluated and lowered per call.
    fn host_geometry_arrays(self: &Arc<Self>, lowerer: &HostLowering, ty: Ty, name: &str, args: &[(String, Value)]) -> R<Value> {
        let op = flow_check::op::find(&self.env.checked.ops, name, &fctx::sop()).ok_or_else(|| data_source("A host call has no checked operator."))?;
        (op.check)(args)?;
        let scalar = |v: &Value| -> Option<param::Value> {
            match v {
                Value::Int(n) => Some(param::Value::Int(*n)),
                Value::Float(n) => Some(param::Value::Float(*n)),
                Value::Bool(b) => Some(param::Value::Bool(*b)),
                Value::Text(t) => Some(param::Value::Text(t.clone())),
                _ => None,
            }
        };
        flow_check::op::result_type(&op, &args.iter().map(|(n, v)| (n.clone(), flow_syntax::value::ty_of(v), scalar(v))).collect::<Vec<_>>())?;
        let direct = geo_of(args);
        let mut sources = direct.clone();
        sources.extend(ak::sources(&geometry_arrays_data(args)).into_iter().filter(|id| !direct.contains(id)));
        let mut inputs = Vec::new();
        for id in &sources {
            let actual = match self.handles.lock().unwrap().get(id) {
                Some((actual, _, _)) => actual.clone(),
                None => return Err(data_source("A cooked handle is missing from this execution.")),
            };
            self.handle_exists(&actual, *id)?;
            let (run, id_c, actual_c) = (Arc::downgrade(self), *id, actual);
            inputs.push(build::make(
                build::Args { parameters: id.to_string().into_bytes(), cook_mode: CookMode::Generator, ..build::args("lisp_host_capture") },
                None,
                Arc::new(move |_, _, _| run.upgrade().ok_or_else(|| data_source("A cooked handle outlived its execution."))?.snapshot(&actual_c, id_c).map(|o| node::Cooked { payload: o.payload, instances: o.instances })),
            ));
        }
        let n = (lowerer.arrays)(name, args, &sources, &inputs)?;
        Ok(self.register_node(ty, n))
    }

    fn call(self: &Arc<Self>, lowerer: &HostLowering, ty: Ty, catalog: bool, name: &str, args: Vec<(String, Value)>) -> R<Value> {
        let checked = &self.env.checked;
        if catalog && !checked.nodes.iter().any(|n| n.kind.qualified == name) && !self.env.factories.iter().any(|f| format!("sop/{}", edit::factory_key(f)) == name) {
            return Err(data_source("A host call has no checked catalog kind."));
        }
        if !catalog && flow_check::op::find(&checked.ops, name, &fctx::sop()).is_none() {
            return Err(data_source("A host call has no checked operator."));
        }
        let guard = if catalog || (name != "sop/with_attr" && name != "sop/delete_attr") {
            None
        } else {
            match get(&args, "when") {
                None => None,
                Some(guard) => match self.force_args(false, &[("when".to_owned(), guard.clone())])?.as_slice() {
                    [(_, Value::Bool(b))] => Some(*b),
                    _ => return Err(fail("E_ATTR_TYPE", &format!("{name} :when must be a bool"))),
                },
            }
        };
        if guard == Some(false) {
            return Ok(get(&args, "geometry").cloned().expect("a write has geometry"));
        }
        if !catalog && name == "sop/delete_attr" {
            // as its node cooks it: a one-call workspace would requalify the catalog
            let args = self.force_args(false, &args)?;
            let owner = get(&args, "owner").cloned().unwrap_or(Value::Text(b"point".to_vec()));
            return match (get(&args, "geometry"), get(&args, "attribute"), owner) {
                (Some(Value::Deferred(actual, id)), Some(Value::Text(pattern)), Value::Text(owner)) if is_geometry_ty(actual) => {
                    let target = self.geometry_r(*id)?;
                    let geometry = ak::delete_after(&[(txt(&owner), txt(pattern))], sctx::cancel_token(&self.context), &target)?;
                    Ok(self.register_output(ty::geometry(), sop::session::Output { payload: payload::T::Geometry(geometry), instances: None }))
                }
                _ => Err(data_source("sop/delete_attr needs geometry and text.")),
            };
        }
        let mut args = without(&args, "when");
        let selected = has(&args, "selection") || has(&args, "default");
        let materialize = !catalog
            && name == "sop/with_attr"
            && if selected {
                match get(&args, "values") {
                    Some(Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_)) => true,
                    Some(v) => Self::constant(v),
                    None => false,
                }
            } else {
                match (get(&args, "geometry"), get(&args, "values")) {
                    (Some(Value::Deferred(t, _)), Some(values @ Value::Record(_))) if is_geometry_ty(t) && Self::deferred(values) => match get(&args, "type") {
                        None => true,
                        Some(Value::Text(kind)) => ["vec2", "vec3", "vec4"].contains(&txt(kind).as_str()),
                        _ => false,
                    },
                    _ => false,
                }
            };
        if materialize {
            if sctx::cancelled(&self.context) {
                return Err(fail("E_CANCELLED", "Attribute kernel cancelled."));
            }
            if !selected {
                attribute_options(&args)?;
            }
            args = self.force_args(true, &args)?;
        }
        let borrowed: Option<Value> = if catalog || name != "sop/with_attr" {
            None
        } else {
            match (get(&args, "geometry"), get(&args, "attribute"), get(&args, "values")) {
                (Some(Value::Deferred(actual, id)), Some(Value::Text(attr)), Some(values)) if is_geometry_ty(actual) => {
                    if sctx::cancelled(&self.context) {
                        return Err(fail("E_CANCELLED", "Attribute kernel cancelled."));
                    }
                    let (owner, kind) = attribute_options(&args)?;
                    let attr = txt(attr);
                    let target = self.geometry_r(*id)?;
                    let materialized = match values {
                        Value::Float_array(_) | Value::Int_array(_) | Value::Text_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_) => true,
                        Value::Record(fields) => {
                            if !selected && ["vec2", "vec3", "vec4"].contains(&kind.as_str()) {
                                !E::is_live(values)
                            } else {
                                matches!((get(fields, "offsets"), get(fields, "values")), (Some(Value::Int_array(_)), Some(Value::Float_array(_) | Value::Int_array(_))))
                            }
                        }
                        _ => false,
                    };
                    let selection = get(&args, "selection");
                    let default = get(&args, "default");
                    let this = self.clone();
                    let geometry_fn = move |gid: i64| this.geometry(gid);
                    let first = if selection.is_some() || default.is_some() { None } else { ak::write_source(&geometry_fn, owner, &kind, &attr, values, &target)? };
                    let selected_materialized = match (selection, default) {
                        (None, None) => true,
                        (Some(Value::Int_array(_)), Some(Value::Float(_) | Value::Int(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..))) => true,
                        _ => false,
                    };
                    let grain = sctx::grain(&self.context);
                    let result: Option<rdk::geometry::T> = match first {
                        Some(g) => Some(g),
                        None if Self::constant(values) && selected_materialized => ak::write_constant_source(grain, owner, &kind, &attr, selection, default, values, &target)?,
                        None if materialized && selected_materialized => {
                            flow_syntax::value::validate(values)?;
                            Some(match (selection, default) {
                                (None, None) => ak::write(grain, owner, &kind, &attr, values, &target)?,
                                (Some(selection), Some(default)) => {
                                    flow_syntax::value::validate(selection)?;
                                    flow_syntax::value::validate(default)?;
                                    ak::write_selected(grain, owner, &kind, &attr, values, selection, default, &target)?
                                }
                                _ => return Err(fail("E_ATTR_SELECTION", "Selected writes require both selection and default.")),
                            })
                        }
                        None => None,
                    };
                    if sctx::cancelled(&self.context) {
                        return Err(fail("E_CANCELLED", "Attribute kernel cancelled."));
                    }
                    match result {
                        Some(geometry) => {
                            self.snapshot(&ty::geometry(), *id)?;
                            Some(self.register_output(ty::geometry(), sop::session::Output { payload: payload::T::Geometry(geometry), instances: None }))
                        }
                        None => None,
                    }
                }
                _ => None,
            }
        };
        if let Some(result) = borrowed {
            return Ok(result);
        }
        let args = self.force_args(false, &args)?;
        if !catalog && is_geometry_arrays(name) {
            return self.host_geometry_arrays(lowerer, ty, name, &args);
        }
        let call_context = if ty == ty::image() { fctx::image() } else { fctx::sop() };
        let form = empty_form();
        let mut capture_factories: Vec<Factory> = Vec::new();
        let mut overrides: Vec<(String, Value)> = Vec::new();
        let mut inputs: Vec<(String, Ty, Option<Term>)> = Vec::new();
        for (n, value) in &args {
            let vty = flow_syntax::value::ty_of(value);
            let default = self.cooked_term(&mut capture_factories, &vty, value)?;
            if default.is_none() {
                overrides.insert(0, (n.clone(), value.clone()));
            }
            inputs.push((n.clone(), vty, default));
        }
        let call_args: Vec<(String, Term)> = inputs.iter().map(|(n, t, _)| (n.clone(), Term { path: None, ty: t.clone(), form: std::sync::Arc::new(form.clone()), node: W::Node::RefBinding(n.clone(), Vec::new()) })).collect();
        let body = Term {
            path: None,
            ty: ty.clone(),
            form: std::sync::Arc::new(form.clone()),
            node: if catalog { W::Node::Call { kind: name.to_owned(), ctx: call_context, args: call_args } } else { W::Node::Op { op: name.to_owned(), args: call_args, skip: Vec::new() } },
        };
        let graph = WGraph { name: "lisp_host_call".into(), context: fctx::sop(), inputs, body, form };
        // an operator call with no function argument needs none of the library: qualifying it would
        // walk every definition per call
        fn callable(v: &Value) -> bool {
            match v {
                Value::Fn(_) => true,
                Value::List(vs) => vs.iter().any(callable),
                Value::Record(fs) | Value::Struct(_, _, fs) => fs.iter().any(|(_, v)| callable(v)),
                _ => false,
            }
        }
        let library = if catalog || overrides.iter().any(|(_, v)| callable(v)) {
            (**checked).clone()
        } else {
            Workspace { defs: Vec::new().into(), nodes: Vec::new().into(), packed: Vec::new(), packed_roots: Vec::new(), ..(**checked).clone() }
        };
        let mut factories = capture_factories;
        factories.extend(self.env.factories.iter().cloned());
        let lowered = lower_checked(
            factories,
            Options { reference: self.env.reference, gpu: self.env.gpu.clone(), strict_body: true, inputs: Some(vec![(graph.name.clone(), overrides)]), ..Options::default() },
            Workspace { graphs: vec![graph].into(), ..library },
        )?;
        match lowered.graphs.as_slice() {
            [graph] => match graph.root {
                None => Err(data_source("A cooked call returned no result.")),
                Some(root) => {
                    let n = edit::compile_node(&graph.network.geometry, root).map_err(|m| fail("E_LOWER", &m))?;
                    Ok(self.register_node(ty, n))
                }
            },
            _ => Err(data_source("A cooked call must return one typed result.")),
        }
    }

    fn resolve(self: &Arc<Self>, value: &Value) -> R<Value> {
        let result = (|| -> R<Value> {
            match value {
                Value::Source(name, Ty::Int, args) if name == "$core/diagnostic_index" && args.len() == 2 && args[0].0 == "rows" && args[1].0 == "policy" => {
                    let Value::Text(policy) = &args[1].1 else { return Err(fail("E_DIAGNOSTIC_CONTEXT", "Host diagnosis needs typed rows and policy.")) };
                    let grain = match txt(policy).as_str() {
                        "first" => None,
                        "range-last" => Some(sctx::grain(&self.context)),
                        _ => return Err(fail("E_DIAGNOSTIC_CONTEXT", "Unknown host diagnostic policy.")),
                    };
                    let check = || -> R<()> {
                        if sctx::cancelled(&self.context) {
                            Err(flow_syntax::value::fail("cancelled", "geometry operation was cancelled"))
                        } else {
                            Ok(())
                        }
                    };
                    match flow_syntax::value::error_row_index(&args[0].1, grain, Some(&check))? {
                        Some(index) => Ok(Value::Int(index)),
                        None => Err(fail("E_DIAGNOSTIC_CONTEXT", "Host diagnosis needs a failing row.")),
                    }
                }
                Value::Source(name, _, _) if name == "$core/diagnostic_index" => Err(fail("E_DIAGNOSTIC_CONTEXT", "Host diagnosis needs typed rows and policy.")),
                Value::Source(name, ty, args) if name == "$core/complete" => match args.as_slice() {
                    [(k, Value::Deferred(actual, id))] if k == "value" && actual == ty && ty::is_cooked(ty) => {
                        self.snapshot(ty, *id)?;
                        Ok(Value::Deferred(ty.clone(), *id))
                    }
                    [(k, Value::No_geo)] if k == "value" && is_geometry_ty(ty) => Ok(Value::No_geo),
                    _ => Err(data_source("Protected completion needs one matching cooked handle.")),
                },
                Value::Source(name, ty, args) if name == "$core/cooked" && args.len() == 1 && args[0].0 == "id" => match &args[0].1 {
                    Value::Int(id) => {
                        self.handle_exists(ty, *id)?;
                        Ok(Value::Deferred(ty.clone(), *id))
                    }
                    _ => Err(data_source("A cooked handle is missing from this execution.")),
                },
                Value::Source(kind, ty, args) if kind == "$core/operator" || kind == "$core/catalog" => {
                    if !ty::is_cooked(ty) {
                        return Err(data_source("Host calls return a declared cooked type."));
                    }
                    match get(args, "$name") {
                        Some(Value::Text(name)) => {
                            let rest: Vec<(String, Value)> = args.iter().filter(|(n, _)| n != "$name" && n != "$context").cloned().collect();
                            self.call(&self.lowering(), ty.clone(), kind == "$core/catalog", &txt(name), rest)
                        }
                        _ => Err(data_source("A host call needs its checked kind.")),
                    }
                }
                value => {
                    let this = self.clone();
                    ak::resolve(Some(&self.context), &(Arc::new(move |id| this.geometry(id)) as ak::GeometryLookup), value)
                }
            }
        })();
        self.take_stash(result)
    }

    /// The pieces `host_geometry_arrays` needs of the enclosing lowering.
    fn lowering(self: &Arc<Self>) -> HostLowering {
        let run = self.clone();
        HostLowering {
            arrays: Box::new(move |name, args, sources, inputs| {
                let env = KernelEnv {
                    gpu: run.env.gpu.clone(),
                    profile: run.env.profile.clone(),
                    clock: run.env.clock.clone(),
                    strict_body: true,
                    reference: run.env.reference,
                    approx: run.env.checked.approx.clone(),
                    kernel_source: Arc::new(String::new),
                };
                geometry_arrays_node(&env, &NodeOpts { check: Some(true), host: true, ..NodeOpts::default() }, name, args, sources, inputs)
            }),
        }
    }
}

struct HostLowering {
    arrays: Box<dyn Fn(&str, &[(String, Value)], &[i64], &[node::T]) -> R<node::T>>,
}

fn structural_cook(env: &StructEnv, node_id: i64, context: &sctx::T, payloads: &[payload::T]) -> R<node::Cooked> {
    let eligible = !env.reference && env.gpu.fast && env.state_slots == 0;
    let placed = if eligible {
        chain::place(&env.gpu, &env.profile, &env.clock, chain::site_of(&Value::Residual(env.residual)).expect("a residual has a site"), sctx::cpu_consumers(context, node_id))
    } else {
        chain::unplaced(chain::Route::Cpu)
    };
    let clock = env.clock.clone();
    let began = std::cell::Cell::new(clock());
    let used_device = std::cell::Cell::new(false);
    let scope = if placed.route != chain::Route::Cpu { env.gpu.backend.as_ref().and_then(|b| (b.residency)(true)).map(Arc::new) } else { None };
    let run = Arc::new(HostRun {
        env: env.clone(),
        context: context.clone(),
        handles: Mutex::new(HashMap::new()),
        geometries: Mutex::new(HashMap::new()),
        next: AtomicI64::new(-1),
        session: Mutex::new(None),
        stash: Mutex::new(None),
    });
    for (index, (id, ty, _)) in env.captures.iter().enumerate() {
        run.handles.lock().unwrap().insert(*id, (ty.clone(), None, Some(sop::session::Output { payload: payloads[index].clone(), instances: None })));
    }
    let execute = |scope: Option<&Arc<Gpu::Residency>>| -> R<Value> {
        let host_source = scope.map(|scope| {
            let (run, scope) = (run.clone(), scope.clone());
            move |value: &Value| -> R<Option<Value>> {
                match value {
                    Value::Source(name, ty, args) if name == "$core/operator" && is_geometry_ty(ty) && get(args, "$name") == Some(&Value::Text(b"sop/mesh".to_vec())) => {
                        let points = match get(args, "points") {
                            Some(p) => scope_count(&scope, p)?,
                            None => None,
                        };
                        if points.is_none() {
                            Ok(None)
                        } else if sctx::cancelled(&run.context) {
                            Err(fail("E_CANCELLED", "Mesh kernel cancelled."))
                        } else {
                            match tki::mesh_resident(None, &scope, args) {
                                None => Ok(None),
                                Some(result) => {
                                    let geometry = result?;
                                    if sctx::cancelled(&run.context) {
                                        for (_, release) in rdk::geometry::releasers(&geometry) {
                                            release(false);
                                        }
                                        Err(fail("E_CANCELLED", "Mesh kernel cancelled."))
                                    } else {
                                        Ok(Some(run.register_output(ty.clone(), sop::session::Output { payload: payload::T::Geometry(geometry), instances: None })))
                                    }
                                }
                            }
                        }
                    }
                    _ => Ok(None),
                }
            }
        });
        flow_ir::core_exec::execute(
            &env.prepared,
            sctx::input(context),
            &flow_ir::core_exec::Options {
                host_source: host_source.map(|f| Arc::new(f) as flow_ir::core_exec::HostSourceFn),
                resident: scope.map(|s| s.executor.clone()),
                resolve: Some(run.resolve_fn()),
                strict: true,
                force: Some(Arc::new({
                    let run = run.clone();
                    let live = sctx::input(context).clone();
                    move |program: &flow_ir::packed::T| flow_ir::packed::force(program, &live, &flow_ir::packed::ForceOptions { resolve: Some(run.resolve_fn()), strict: true, ..Default::default() })
                })),
                ..Default::default()
            },
        )
    };
    let on_device = || -> R<Value> {
        used_device.set(scope.is_some());
        match execute(scope.as_ref()) {
            Err(_) if scope.is_some() && !sctx::cancelled(context) => {
                if let Some(s) = &scope {
                    (s.executor.close)();
                }
                used_device.set(false);
                execute(None)
            }
            result => result,
        }
    };
    let result = match placed.route {
        chain::Route::Device => on_device(),
        chain::Route::Cpu => execute(None),
        chain::Route::Probe(budget) => {
            let stop = began.get() + budget;
            let clock2 = env.clock.clone();
            let mut out: Option<R<Value>> = None;
            if E::with_interrupt(Arc::new(move || clock2() > stop), &mut || out = Some(execute(None))) {
                out.unwrap()
            } else {
                (placed.abandoned)();
                began.set(clock());
                on_device()
            }
        }
    };
    let result = result.and_then(|v| match v {
        Value::Deferred(ty, id) if is_geometry_ty(&ty) => run.snapshot(&ty, id),
        Value::No_geo => Err(fail("E_LOWER", "Cooked selection returned empty geometry.")),
        _ => Err(data_source("Cooked selection must return a geometry handle.")),
    });
    if let Some(s) = &scope {
        (s.executor.close)();
    }
    if let Some(session) = run.session.lock().unwrap().as_mut() {
        sop::session::close(session);
    }
    result.map(|output| {
        if eligible && !sctx::cancelled(context) {
            if let payload::T::Geometry(g) = &output.payload {
                (placed.record)(g, used_device.get(), clock() - began.get());
            }
        }
        node::Cooked { payload: output.payload, instances: output.instances }
    })
}

fn scope_count(scope: &Gpu::Residency, value: &Value) -> R<Option<i64>> {
    (scope.executor.count)(value)
}

fn failed_node(d: Diagnostic) -> node::T {
    build::make(build::args("flow.failed"), None, Arc::new(move |_, _, _| Err(d.clone())))
}

fn kind_key(qualified: &str) -> String {
    qualified[4..].to_owned()
}

/// The factory of a checked Lisp declaration. `functions` are the call's function arguments;
/// `error` is an evaluation error of the call's own parameters, reported when it cooks.
fn lisp_factory(shared: &Arc<CallEnvShared>, kernel_source: &Lazy, error: Option<Diagnostic>, functions: &[(String, Value)], provenance: Option<i64>, definition: &NodeDefinition) -> Factory {
    let checked = shared.checked.clone();
    let declaration_source: Lazy = {
        let (checked, definition) = (checked.clone(), definition.clone());
        lazy(Box::new(move || cached_digest(&checked, std::slice::from_ref(&definition), &|| ck::declaration_source(&checked, &definition))))
    };
    let functions: Vec<(String, Value)> = definition
        .slots
        .iter()
        .filter_map(|slot: &Slot| {
            let values: Vec<Value> = functions.iter().filter(|(name, _)| *name == slot.name).flat_map(|(_, v)| function_elements(v)).map(Value::Fn).collect();
            match values.len() {
                0 => None,
                1 if !slot.rest => Some((slot.name.clone(), values[0].clone())),
                _ => Some((slot.name.clone(), Value::List(Arc::new(values)))),
            }
        })
        .collect();
    let key = kind_key(&definition.kind.qualified);
    let errored: Arc<Mutex<Option<Vec<(String, Value)>>>> = Arc::new(Mutex::new(None));
    let category: Vec<String> = if definition.category.is_empty() { vec!["Custom".into()] } else { definition.category.clone() };
    let (shared_c, definition_c, functions_c, kernel_source_c, key_c) = (shared.clone(), definition.clone(), functions.clone(), kernel_source.clone(), key.clone());
    let construct = move |inputs: &[Option<node::T>]| -> Result<node::T, String> {
        let definition = &definition_c;
        let mut next = 0usize;
        let connected: Vec<(usize, Option<usize>)> = inputs
            .iter()
            .enumerate()
            .map(|(slot, input)| {
                (
                    slot,
                    input.as_ref().map(|_| {
                        let index = next;
                        next += 1;
                        index
                    }),
                )
            })
            .collect();
        let bindings: Vec<(String, Vec<usize>)> = definition
            .slots
            .iter()
            .enumerate()
            .map(|(slot, s)| (s.name.clone(), connected.iter().filter_map(|(index, input)| if *index == slot || (s.rest && *index >= slot) { *input } else { None }).collect()))
            .collect();
        let slots = &definition.slots;
        let geometry_inputs: Vec<bool> = inputs
            .iter()
            .enumerate()
            .filter_map(|(position, input)| {
                input.as_ref().map(|_| {
                    let slot = &slots[position.min(slots.len().saturating_sub(1))];
                    W::slot_ty(Some(slot), &definition.kind) == ty::geometry()
                })
            })
            .collect();
        let unbound_function = slots.iter().any(|slot| match &slot.ty {
            Some(Ty::Fn(_)) => !functions_c.iter().any(|(n, _)| *n == slot.name) && (slot.required || bindings.iter().find(|(n, _)| *n == slot.name).is_some_and(|(_, v)| !v.is_empty())),
            _ => false,
        });
        let source = format!(
            "{}{}",
            if functions_c.is_empty() { declaration_source() } else { kernel_source_c() },
            provenance.map_or(String::new(), |p| format!(";provenance={p}"))
        );
        let (shared, definition2, functions2, bindings2, error2, errored2) = (shared_c.clone(), definition.clone(), functions_c.clone(), bindings, error.clone(), errored.clone());
        let build_body: ln::BuildFn = Arc::new(move |label: &str, context: &sctx::T, inputs: &[node::T], parameters: &[(String, Value)]| {
            let failed = match (&error2, &*errored2.lock().unwrap()) {
                (Some(_), None) => true,
                (Some(_), Some(original)) => original.as_slice() == parameters,
                (None, _) => false,
            };
            if error2.is_some() && errored2.lock().unwrap().is_none() {
                *errored2.lock().unwrap() = Some(parameters.to_vec());
            }
            if let (Some(diagnostic), true) = (&error2, failed) {
                return Ok(lisp_body_failed(&shared.checked, &definition2, inputs, parameters, diagnostic.clone()));
            }
            let o = shared.o.clone();
            let lower_call: Arc<dyn Fn(&Gpu::Config, Vec<(FnId, i64)>, Vec<Factory>, Vec<(String, Vec<(String, Value)>)>, Workspace) -> R<T> + Send + Sync> = {
                Arc::new(move |gpu: &Gpu::Config, kernel_bindings, factories, inputs, checked| {
                    lower_checked(
                        factories,
                        Options { reference: o.reference, gpu: gpu.clone(), strict_body: true, defer_body_errors: true, kernel_bindings, provenance_tags: o.provenance_tags, tag_base: provenance.unwrap_or(0), on_profile: o.on_profile.clone(), inputs: Some(inputs), ..Options::default() },
                        checked,
                    )
                })
            };
            let on_profile = shared.o.on_profile.clone();
            let shared2 = shared.clone();
            let env = CallEnv {
                gpu: shared.o.gpu.clone(),
                lower_call,
                record_profile: Arc::new(move |p| {
                    if let Some(record) = &on_profile {
                        record(p)
                    }
                }),
                checked: shared.checked.clone(),
                provenance,
                factories: shared.factories.clone(),
                def_context_use: Arc::new(move || shared2.def_context_use()),
            };
            build_call(&env, &definition2, &functions2, &bindings2, label, context, inputs, parameters)
        });
        ln::node(
            &ln::NodeOptions {
                unbound_function,
                geometry_inputs,
                operation: definition.operation.clone(),
                version: definition.version,
                label: key_c.clone(),
                fields: definition.fields.clone(),
                normalizers: ep::for_node(&shared_c.checked, definition),
                source,
            },
            build_body,
            &inputs.iter().flatten().cloned().collect::<Vec<_>>(),
        )
    };
    let args = edit::FactoryArgs {
        operation: Some(definition.operation.clone()),
        fields: definition.fields.clone(),
        ..edit::factory_args(&key, &definition.label, &category.iter().map(String::as_str).collect::<Vec<_>>())
    };
    if definition.slots.is_empty() {
        edit::factory(args, 0, Arc::new(move |_| construct(&[])))
    } else {
        let args = edit::FactoryArgs {
            slots: Some(definition.slots.iter().map(|s| s.name.clone()).collect()),
            keyword_inputs: definition.keyword_inputs.clone(),
            input_types: Some(definition.slots.iter().map(|s| ty::to_string(&W::slot_ty(Some(s), &definition.kind))).collect()),
            ..args
        };
        let requirements: Vec<InputRequirement> = definition
            .slots
            .iter()
            .map(|s| match (s.required, s.rest) {
                (true, false) => InputRequirement::Required,
                (false, false) => InputRequirement::Optional,
                (true, true) => InputRequirement::Rest,
                (false, true) => InputRequirement::OptionalRest,
            })
            .collect();
        edit::factory_slots(args, requirements, Arc::new(move |inputs| construct(inputs)))
    }
}

impl CallEnvShared {
    /// A helper that names no context operator is its own specialization: shared across instances.
    fn def_context_use(&self) -> Vec<(WGraph, bool)> {
        let mut guard = self.def_context_use.lock().unwrap();
        guard.get_or_insert_with(|| self.checked.defs.iter().map(|def| (def.clone(), ck::uses_context(def))).collect()).clone()
    }
}

/// A card's canonical parameters as the values of its schema's fields.
fn card_values(fields: &[param::FieldView], defaults: &[(String, param::Value)], parameters: &[(String, Value)]) -> Vec<(String, param::Value)> {
    // a vec3 group's argument is one vec3
    let mut parameters = parameters.to_vec();
    for f in fields {
        if let Some((group, i)) = &f.vec3 {
            if let Some(Value::Vec3(x, y, z)) = get(&parameters.clone(), group) {
                parameters.push((f.name.clone(), Value::Float([*x, *y, *z][*i as usize])));
            }
        }
    }
    defaults
        .iter()
        .map(|(name, default)| {
            let v = match (get(&parameters, name), default) {
                (Some(Value::Bool(b)), _) => param::Value::Bool(*b),
                (Some(Value::Int(n)), _) => param::Value::Int(*n),
                (Some(Value::Float(f)), _) => param::Value::Float(*f),
                (Some(Value::Text(t)), param::Value::Choice(_)) => param::Value::Choice(txt(t)),
                (Some(Value::Text(t)), _) => param::Value::Text(t.clone()),
                (_, value) => value.clone(),
            };
            (name.clone(), v)
        })
        .collect()
}

fn schema_defaults(schema: &param::Schema) -> Vec<(String, param::Value)> {
    schema.fields().iter().zip(schema.default().iter()).map(|(f, v)| (f.name().to_owned(), v.clone())).collect()
}

impl Lowerer {
    fn find_factory(&self, key: &str) -> R<Factory> {
        self.factories
            .iter()
            .chain(self.lisp_factories.iter())
            .find(|f| edit::factory_key(f) == key)
            .cloned()
            .ok_or_else(|| fail("E_LOWER", &format!("Linked catalog has no {key}")))
    }

    fn lisp_factory_for(&self, error: Option<Diagnostic>, functions: &[(String, Value)], provenance: Option<i64>, definition: &NodeDefinition) -> Factory {
        lisp_factory(&self.call_env, &self.kernel_source, error, functions, provenance, definition)
    }

    fn curve_points(args: &[(String, Value)]) -> R<Vec<(f64, f64, f64)>> {
        match get(args, "points") {
            Some(Value::List(values)) => values
                .iter()
                .map(|v| match v {
                    Value::Vec3(x, y, z) => Ok((*x, *y, *z)),
                    _ => Err(fail("E_LOWER", "sop/curve points must be vec3")),
                })
                .collect(),
            _ => Err(fail("E_LOWER", "sop/curve needs :points")),
        }
    }

    fn lower_default(&self, tag_base: Option<i64>, node: &ENode, args: &[(String, Value)], cid: i64) -> R<Prepared> {
        let kind = node.kind.clone();
        let args: Vec<(String, Value)> = if kind == "image/noise" { args.iter().map(|(n, v)| (if n == "freq" { "frequency".to_owned() } else { n.clone() }, v.clone())).collect() } else { args.to_vec() };
        let key = match kind.split('/').collect::<Vec<_>>().as_slice() {
            ["sop", key] => (*key).to_owned(),
            ["image", "noise"] => "image_noise".to_owned(),
            _ => return Err(fail("E_LOWER", &format!("Cannot lower {kind}"))),
        };
        let factory = if key == "image_noise" {
            sop::image_nodes::noise_factory()
        } else {
            match self.lisp_definition(&kind) {
                Some(_) if self.o.defer_body_errors && node.error.is_none() && self.checked.graphs.iter().any(|g| node.site == vec![g.name.clone(), "@result".to_owned()]) => {
                    // a node's own cook is one inlined call: that call staying a node would cook the same lowering again
                    return Err(fail("E_LOWER", &format!("{kind} does not inline in its own cook")));
                }
                Some(definition) => {
                    // the editor's provenance: a Lisp sop/merge tags input i with base + i
                    let tag_base = if kind != "sop/merge" || !self.o.provenance_tags {
                        None
                    } else if tag_base.is_some() {
                        tag_base
                    } else {
                        Some(self.tag_sources(cid, &args))
                    };
                    self.lisp_factory_for(node.error.clone(), &args, tag_base, &definition)
                }
                None => self.find_factory(&key)?,
            }
        };
        let names = edit::factory_slot_names(&factory).to_vec();
        let parameters = self.parameters(&factory)?;
        let requirements = edit::factory_inputs(&factory);
        let rest = requirements.iter().position(|r| matches!(r, InputRequirement::Rest | InputRequirement::OptionalRest));
        let mut arity = names.len();
        let mut next_rest = rest.unwrap_or(0);
        let mut slots: Vec<(usize, i64)> = Vec::new();
        let mut changes: Vec<(String, param::Value)> = Vec::new();
        let mut kernels: Vec<KernelInput> = Vec::new();
        let mut dynamic: Vec<Dynamic> = Vec::new();
        let input_types = edit::factory_input_types(&factory).to_vec();
        let mut add_function = |index: usize, declared_index: usize, name: &str, f: FnId, slots: &mut Vec<(usize, i64)>, kernels: &mut Vec<KernelInput>| -> R<()> {
            match self.external_kernel(f) {
                Some(source) => slots.push((index, source)),
                None => {
                    let signature = match ty::of_string(&input_types[declared_index]) {
                        Some(Ty::Fn(Some(signature))) => *signature,
                        _ => return Err(fail("E_LOWER", &format!("Input {name} does not declare a function signature"))),
                    };
                    let mut site = node.site.clone();
                    site.push(format!("~kernel:{name}:{index}"));
                    let cid = self.compiled_id(&ENode { site, ..node.clone() });
                    kernels.push(KernelInput { index, cid, signature, fun: f, identity: build::fresh_id(), sources: ak::sources(&Value::Fn(f)) });
                }
            }
            Ok(())
        };
        fn add_rest(name: &str, value: &Value, next_rest: &mut usize, arity: &mut usize, slots: &mut Vec<(usize, i64)>, kernels: &mut Vec<KernelInput>, rest: usize, add_function: &mut dyn FnMut(usize, usize, &str, FnId, &mut Vec<(usize, i64)>, &mut Vec<KernelInput>) -> R<()>) -> R<()> {
            match value {
                Value::No_geo => {}
                Value::Deferred(ty, id) if ty::is_cooked(ty) => {
                    slots.push((*next_rest, *id));
                    *next_rest += 1;
                    *arity = (*arity).max(*next_rest);
                }
                Value::Fn(f) => {
                    add_function(*next_rest, rest, name, *f, slots, kernels)?;
                    *next_rest += 1;
                    *arity = (*arity).max(*next_rest);
                }
                Value::List(values) => {
                    for v in values.iter() {
                        add_rest(name, v, next_rest, arity, slots, kernels, rest, add_function)?;
                    }
                }
                _ => return Err(fail("E_LOWER", "Repeated inputs need cooked values or functions")),
            }
            Ok(())
        }
        for (name, value) in &args {
            match (names.iter().position(|n| n == name), value) {
                (Some(index), value) if Some(index) == rest => add_rest(name, value, &mut next_rest, &mut arity, &mut slots, &mut kernels, rest.unwrap(), &mut add_function)?,
                (Some(_), Value::No_geo) => {}
                (Some(index), Value::Deferred(ty, id)) if ty::is_cooked(ty) => slots.push((index, *id)),
                (Some(index), Value::Fn(f)) => add_function(index, index, name, *f, &mut slots, &mut kernels)?,
                (Some(_), _) => return Err(fail("E_LOWER", &format!("Slot {name} needs geometry"))),
                (None, value) => {
                    let parameter = port::find_parameter(&parameters, name)?;
                    if E::is_live(value) {
                        let p = parameter.clone();
                        dynamic.push((value.clone(), Arc::new(move |v: &Value| changes_of(&p, v))));
                    } else {
                        changes.extend(changes_of(&parameter, value)?);
                    }
                }
            }
        }
        Ok(Prepared { cid, factory, arity, dynamic, zone: None, slots, changes, kernels })
    }

    /// An inlined card's marker: a pass-through of the body's result, or the structural node of a
    /// body whose geometry is decided by cooked data.
    fn lower_card(self: &Arc<Self>, node: &ENode, args: &[(String, Value)], cid: i64) -> R<Prepared> {
        let kind = match get(args, "kind") {
            Some(Value::Text(k)) => txt(k),
            _ => String::new(),
        };
        let key = match kind.find('/') {
            Some(i) => kind[i + 1..].to_owned(),
            None => kind.clone(),
        };
        let sources = self.deps(node)?;
        let this = Wk::new(self);
        let node_c = node.clone();
        let args_c = args.to_vec();
        let (key_c, kind_c, sources_c) = (key.clone(), kind.clone(), sources.clone());
        let build_inner: Arc<dyn Fn(&[node::T]) -> R<node::T> + Send + Sync> = Arc::new(move |inputs: &[node::T]| {
            match get(&args_c, "value") {
                Some(Value::Residual(residual)) => {
                    let this = this.get();
                    let captures: Vec<(i64, Ty, node::T)> = sources_c.iter().zip(inputs.iter()).map(|(id, input)| (*id, this.plan.nodes[*id as usize].ty.clone(), input.clone())).collect();
                    let n = this.reuse(Some(&node_c), &None, &None, "structural", &Value::Residual(*residual), inputs, &|| this.structural_node(Some(&node_c), *residual, &captures))?;
                    Ok(node::relabel(&key_c, &n))
                }
                _ => {
                    // a deferred check of a live argument fails the card's cook as the inlined call would have refused or failed
                    let n = build::make(
                        build::Args { label: Some(key_c.clone()), parameters: kind_c.clone().into_bytes(), cook_mode: CookMode::Passthrough(0), inputs: inputs.to_vec(), ..build::args("flow.card") },
                        None,
                        Arc::new(|_, _, payloads| Ok(node::Cooked { payload: payloads[0].clone(), instances: None })),
                    );
                    let n = build::with_input_use(InputUse::Forward(0), &n);
                    let n = build::with_facts(Facts { cook_mode: CookMode::Passthrough(0), elementwise: Elementwise::None, reads: Vec::new(), writes: Vec::new(), topology: Topology::Preserved, exact: true }, &n);
                    Ok(guarded(guards_of("guards", &args_c), n))
                }
            }
        });
        // the card's fields, for the inspector, handles and display; an edit goes through its text
        let schema_fn = {
            let (this, args_c, kind_c, build_inner) = (Wk::new(self), args.to_vec(), kind.clone(), build_inner.clone());
            move |n: node::T, inputs: &[node::T]| -> node::T {
                let _ = inputs;
                let this = this.get();
                match (this.lisp_definition(&kind_c), get(&args_c, "parameters")) {
                    (Some(d), Some(Value::Record(parameters))) => {
                        let schema = Arc::new(ln::schema("parameters", &d.fields, &ep::for_node(&this.checked, &d)));
                        let values = card_values(&d.fields, &schema_defaults(&schema), parameters);
                        fn attach(n: node::T, schema: &Arc<param::Schema>, values: Vec<(String, param::Value)>, build_inner: &Arc<dyn Fn(&[node::T]) -> R<node::T> + Send + Sync>) -> node::T {
                            let positional: Vec<param::Value> = values.iter().map(|(_, v)| v.clone()).collect();
                            let (schema_c, values_c, build_c) = (schema.clone(), values.clone(), build_inner.clone());
                            let rebuild: node::RebuildFn = Arc::new(move |_label: &str, inputs: &[node::T], changed: &[param::Value]| {
                                // new inputs keep the card; a new value is an edit of its text
                                if changed == values_c.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>().as_slice() {
                                    let rebuilt = build_c(inputs).map_err(keep_diagnostic)?;
                                    Ok(attach(rebuilt, &schema_c, values_c.clone(), &build_c))
                                } else {
                                    Err("An inlined card is edited through its text".to_owned())
                                }
                            });
                            node::parameterize_with_key(None, schema.clone(), positional, rebuild, &n).unwrap_or(n)
                        }
                        attach(n, &schema, values, &build_inner)
                    }
                    _ => n,
                }
            }
        };
        let factory = edit::factory(
            edit::factory_args("flow_card", &key, &["Flow"]),
            sources.len(),
            Arc::new(move |inputs: &[node::T]| {
                let n = build_inner(inputs).map_err(keep_diagnostic)?;
                Ok(schema_fn(n, inputs))
            }),
        );
        Ok(Prepared { cid, factory, arity: sources.len(), changes: Vec::new(), dynamic: Vec::new(), zone: None, kernels: Vec::new(), slots: sources.iter().enumerate().map(|(i, s)| (i, *s)).collect() })
    }

    fn native(&self, node: &ENode) -> Option<Kind> {
        if node.kind == E::card_kind() {
            return Some(Kind::Card);
        }
        if node.kind == "exact" && node.ty != ty::image() {
            return None;
        }
        ops::lowering(&node.kind).map(|l| match l {
            ops::Lowering::GeometryArrays => Kind::Arrays,
            ops::Lowering::Metadata => Kind::Metadata,
            ops::Lowering::AttributeWrite => Kind::Attribute,
            ops::Lowering::GroupWrite => Kind::Group,
            ops::Lowering::AttributeDelete => Kind::Delete,
            ops::Lowering::Curve => Kind::Curve,
            ops::Lowering::Zone => Kind::Zone,
            ops::Lowering::Resource => Kind::Resource,
        })
    }

    fn prepare(self: &Arc<Self>, node: &ENode) -> R<Prepared> {
        if let Some(p) = self.prepared.lock().unwrap().get(&node.id) {
            return Ok(p.clone());
        }
        let template = self.templ[node.id as usize];
        let cid = self.compiled[node.id as usize];
        // an inlined merge body's provenance base (its sop/concat), allocated here as its card was
        let mut node = node.clone();
        if node.args.iter().any(|(k, v)| k == "tag_base" && matches!(v, Value::Int(n) if *n == ops::PROVENANCE_SENTINEL)) {
            let base = if self.o.provenance_tags { self.tag_sources(cid, &node.args) } else { -1 };
            self.concat_tags.lock().unwrap().insert((node.site.clone(), node.iter.clone()), base);
            node.args = node.args.iter().map(|(k, v)| if k == "tag_base" && matches!(v, Value::Int(n) if *n == ops::PROVENANCE_SENTINEL) { (k.clone(), Value::Int(base)) } else { (k.clone(), v.clone()) }).collect();
        }
        let live: Vec<(String, Value)> = node.args.iter().filter(|(_, v)| E::is_live(v)).cloned().collect();
        let resource = is_resource(&node);
        // a template node keeps its live arguments: each element forces them
        let args: Vec<(String, Value)> = if template || kernel_data(&node).is_some() || resource || node.kind == E::card_kind() || (node.kind == "sop/delete_attr" && has(&node.args, "when")) {
            node.args.clone()
        } else {
            node.args.iter().map(|(n, v)| Ok((n.clone(), at_zero(v)?))).collect::<R<Vec<_>>>()?
        };
        let args = material_args(&node.kind, args);
        let p = match self.native(&node) {
            Some(kind) => self.lower_native(kind, &node, &args, cid)?,
            None => self.lower_default(None, &node, &args, cid)?,
        };
        if !template && kernel_data(&node).is_none() && !resource {
            let parameters = self.parameters(&p.factory)?;
            let mut pending = self.pending.lock().unwrap();
            for (field, value) in live {
                let field = if node.kind == "image/noise" && field == "freq" { "frequency".to_owned() } else { field };
                if parameters.iter().any(|parameter| parameter.path == field) {
                    pending.insert(0, Pending { node: cid, field, value });
                }
            }
        }
        self.prepared.lock().unwrap().insert(node.id, p.clone());
        Ok(p)
    }

    fn lower_native(self: &Arc<Self>, kind: Kind, node: &ENode, args: &[(String, Value)], cid: i64) -> R<Prepared> {
        let plain = |factory: Factory, sources: &[i64]| Prepared { cid, factory, arity: sources.len(), changes: Vec::new(), dynamic: Vec::new(), zone: None, kernels: Vec::new(), slots: sources.iter().enumerate().map(|(i, s)| (i, *s)).collect() };
        match kind {
            Kind::Card => self.lower_card(node, args, cid),
            Kind::Arrays => {
                let operation = node.kind.clone();
                let sources = self.deps(node)?;
                let (this, node_c, args_c, sources_c, op_c) = (Wk::new(self), node.clone(), args.to_vec(), sources.clone(), operation.clone());
                let factory = edit::factory(
                    edit::factory_args(geometry_arrays_key(&operation), geometry_arrays_label(&operation), &["Flow", "Topology"]),
                    sources.len(),
                    Arc::new(move |inputs: &[node::T]| this.get().arrays_node(Some(&node_c), None, false, None, None, &op_c, &args_c, &sources_c, inputs).map_err(keep_diagnostic)),
                );
                Ok(plain(factory, &sources))
            }
            Kind::Metadata => {
                let sources = geo_of(args);
                let factory = metadata_factory(&node.kind, args, &sources)?;
                Ok(plain(factory, &sources))
            }
            Kind::Group => {
                let sources = self.deps(node)?;
                let (this, node_c, args_c, sources_c) = (Wk::new(self), node.clone(), args.to_vec(), sources.clone());
                let factory = edit::factory(
                    edit::factory_args("flow.with_group", "Write group", &["Flow"]),
                    sources.len(),
                    Arc::new(move |inputs: &[node::T]| this.get().grp_node(Some(&node_c), None, None, &args_c, &sources_c, inputs).map_err(keep_diagnostic)),
                );
                Ok(plain(factory, &sources))
            }
            Kind::Resource => {
                let op = node.kind.clone();
                let factory = edit::factory(
                    edit::factory_args(&node.kind, &node.kind, &["Image"]),
                    0,
                    Arc::new(move |_| Ok(resource_image(&op, Err(fail("E_IMAGE", "This image needs an initial-domain image resolver."))))),
                );
                Ok(Prepared { cid, factory, slots: Vec::new(), arity: 0, changes: Vec::new(), dynamic: Vec::new(), zone: None, kernels: Vec::new() })
            }
            Kind::Attribute => {
                let deletes: Vec<(String, String)> = match get(args, FUSED_DELETES) {
                    Some(Value::List(deletes)) => deletes
                        .iter()
                        .map(|d| match d {
                            Value::Record(f) if f.len() == 2 && f[0].0 == "owner" && f[1].0 == "pattern" => match (&f[0].1, &f[1].1) {
                                (Value::Text(o), Value::Text(p)) => Ok((txt(o), txt(p))),
                                _ => Err(fail("E_LOWER", "A fused write's deletions are owner/pattern records")),
                            },
                            _ => Err(fail("E_LOWER", "A fused write's deletions are owner/pattern records")),
                        })
                        .collect::<R<Vec<_>>>()?,
                    _ => Vec::new(),
                };
                let prior = fused_entries(args)?;
                let args: Vec<(String, Value)> = args.iter().filter(|(k, _)| ![FUSED_WRITES, FUSED_DELETES, FUSED_CHAIN].contains(&k.as_str())).cloned().collect();
                let (owner, kind) = attribute_options(&args)?;
                let name = match get(&args, "attribute") {
                    Some(Value::Text(n)) => txt(n),
                    _ => return Err(fail("E_LOWER", "Attribute name must be static text")),
                };
                let values = get(&args, "values").cloned().expect("a write has values");
                let mut sources = self.deps(node)?;
                if matches!(get(&args, "geometry"), Some(Value::No_geo)) {
                    sources.insert(0, -1);
                }
                let (this, node_c, sources_c) = (Wk::new(self), node.clone(), sources.clone());
                let o = AttrOpts {
                    selection: get(&args, "selection").cloned(),
                    default: get(&args, "default").cloned(),
                    guard: get(&args, "when").cloned(),
                    prior,
                    owner: Some(owner),
                    kind: Some(kind),
                    deletes: Some(deletes),
                    ..AttrOpts::default()
                };
                let factory = edit::factory(
                    edit::factory_args("flow.with_attr", "Write attribute", &["Flow"]),
                    sources.len(),
                    Arc::new(move |inputs: &[node::T]| this.get().attr_node(Some(&node_c), o.clone(), &name, &values, &sources_c, inputs).map_err(keep_diagnostic)),
                );
                Ok(Prepared {
                    cid,
                    factory,
                    arity: sources.len(),
                    changes: Vec::new(),
                    dynamic: Vec::new(),
                    zone: None,
                    kernels: Vec::new(),
                    slots: sources.iter().enumerate().filter(|(_, s)| **s >= 0).map(|(i, s)| (i, *s)).collect(),
                })
            }
            Kind::Delete => {
                let name = match get(args, "attribute") {
                    Some(Value::Text(n)) => txt(n),
                    _ => return Err(fail("E_ATTR_NAME", "An attribute pattern must be text")),
                };
                let owner = match get(args, "owner") {
                    None => "point".to_owned(),
                    Some(Value::Text(o)) => txt(o),
                    Some(_) => return Err(fail("E_ATTR_OWNER", "An attribute owner must be text")),
                };
                let guard = get(args, "when").cloned();
                let sources = if guard.is_none() { geo_of(args) } else { self.deps(node)? };
                let arity = sources.len();
                let sources_c = sources.clone();
                let factory = edit::factory(
                    edit::factory_args("flow.delete_attr", "Delete attribute", &["Flow"]),
                    arity,
                    Arc::new(move |inputs: &[node::T]| {
                        if inputs.is_empty() || inputs.len() - 1 != arity - 1 {
                            return Err("Attribute deletion needs one geometry".to_owned());
                        }
                        let extra = inputs[1..].to_vec();
                        let guarded = guard.as_ref().map(|guard| {
                            let (g, sources) = (guard.clone(), sources_c.clone());
                            let decide: Arc<dyn Fn(&sctx::T, &[rdk::geometry::T]) -> R<bool> + Send + Sync> = Arc::new(move |context, geometries| {
                                let geoms = geometries.to_vec();
                                let sources = sources.clone();
                                let resolve: flow_ir::core_exec::ResolveFn = {
                                    let context = context.clone();
                                    Arc::new(move |v: &Value| {
                                        ak::resolve(Some(&context), &lookup(geoms.clone(), sources.clone()), v)
                                    })
                                };
                                match E::Host::force_reference(&g, sctx::input(context), None, None, Some(resolve))? {
                                    Value::Bool(b) => Ok(b),
                                    _ => Err(fail("E_ATTR_TYPE", "sop/delete_attr :when must be a bool")),
                                }
                            });
                            am::Guard { key: key_text(guard), dependencies: ak::dependencies(guard), inputs: extra.clone(), decide }
                        });
                        Ok(am::delete(guarded, &owner, &name, &inputs[0]))
                    }),
                );
                Ok(plain(factory, &sources))
            }
            Kind::Curve => {
                let mut dynamic: Vec<Dynamic> = Vec::new();
                let encode = |points: &[(String, Value)]| -> R<Vec<(String, param::Value)>> {
                    Ok(vec![(curve::PARAMETER.to_owned(), param::Value::Text(curve::encode(&Self::curve_points(points)?).into_bytes()))])
                };
                let closed = match get(args, "closed") {
                    None => Vec::new(),
                    Some(Value::Bool(c)) => vec![(curve::CLOSED_PARAMETER.to_owned(), param::Value::Bool(*c))],
                    Some(_) => return Err(fail("E_LOWER", "sop/curve :closed must be a constant bool")),
                };
                let filled = match get(args, "filled") {
                    None => Vec::new(),
                    Some(Value::Bool(f)) => vec![(curve::FILLED_PARAMETER.to_owned(), param::Value::Bool(*f))],
                    Some(_) => return Err(fail("E_LOWER", "sop/curve :filled must be a constant bool")),
                };
                let mut changes = closed;
                changes.extend(filled);
                match get(args, "points") {
                    Some(points) if E::is_live(points) => {
                        dynamic.push((points.clone(), Arc::new(move |v: &Value| Ok(vec![(curve::PARAMETER.to_owned(), param::Value::Text(curve::encode(&Lowerer::curve_points(&[("points".to_owned(), v.clone())])?).into_bytes()))]))));
                    }
                    _ => changes.extend(encode(args)?),
                }
                Ok(Prepared { cid, factory: curve::factory(), arity: 0, slots: Vec::new(), changes, dynamic, zone: None, kernels: Vec::new() })
            }
            Kind::Zone => {
                let mut z = self.zinfo(node)?;
                z.base = self.tags.load(Ordering::SeqCst);
                z.count = Arc::new(AtomicI64::new(-1));
                z.positions = Arc::new(Mutex::new(Arc::new(Vec::new())));
                self.tags.store(z.base + szone::MAX_ELEMENTS, Ordering::SeqCst);
                let src = match geo_of(&args.iter().filter(|(k, _)| k == "geometry").cloned().collect::<Vec<_>>()).as_slice() {
                    [src] => *src,
                    _ => return Err(fail("E_LOWER", "Zone without geometry")),
                };
                let mut sources = vec![src];
                sources.extend(z.captures.iter().cloned());
                for id in &z.order {
                    let n = &self.plan.nodes[*id as usize];
                    if n.kind != "zone/element" {
                        self.prepare(n)?;
                    }
                }
                let base_site = self.plan.nodes[z.root as usize].site.clone();
                self.zones.lock().unwrap().push(Zone { cid, site: node.site.clone(), iter: node.iter.clone(), body_site: base_site, base: z.base, count: z.count.clone(), ekey: z.ekey.clone(), positions: z.positions.clone() });
                let (this, z_c) = (Wk::new(self), z.clone());
                let factory = edit::factory_slots(
                    edit::FactoryArgs { slots: Some(vec!["input".to_owned()]), ..edit::factory_args("zone", "Loop over geometry", &["Copy"]) },
                    vec![InputRequirement::Rest],
                    Arc::new(move |nodes: &[Option<node::T>]| Ok(this.get().make_zone(None, Vec::new(), &z_c, nodes.iter().flatten().cloned().collect()))),
                );
                Ok(Prepared { cid, factory, arity: sources.len(), changes: Vec::new(), dynamic: Vec::new(), kernels: Vec::new(), zone: Some(z), slots: sources.iter().enumerate().map(|(i, s)| (i, *s)).collect() })
            }
        }
    }

    /// A kernel node of a function-valued input.
    fn kernel_node(&self, state: Option<State>, elems: Option<Vec<(String, Value)>>, k: &KernelInput, inputs: &[node::T]) -> R<node::T> {
        let n = fk::node(state.as_ref(), self.o.reference, elems.as_deref(), k.identity, &k.signature, k.fun, &k.sources, inputs.to_vec());
        edit_r(build::restore_id(k.cid, &n))
    }

    /// The zone node of a lowered graph: it cooks `z.root` once per element.
    fn make_zone(self: &Arc<Self>, state: Option<State>, outer: Vec<(String, Value)>, z: &Zinfo, inputs: Vec<node::T>) -> node::T {
        let mut stamp = state.as_ref().map_or(String::new(), E::state_stamp);
        if let Some(choice) = &z.preview {
            stamp.push_str(&format!(";preview={}", digest(&format!("{choice:?}"))));
        }
        let select = z.preview.as_ref().and_then(|(_, probes)| probes.get(self.plan.nodes[z.plan_id as usize].iter.len()).copied());
        let (count, positions) = (z.count.clone(), z.positions.clone());
        let mut args = szone::args(z.kind, source_attribute(), z.base);
        args.report = Some(Arc::new(move |elements: &[szone::Element]| {
            count.store(elements.len() as i64, Ordering::SeqCst);
            *positions.lock().unwrap() = Arc::new(elements.iter().map(|e| e.position).collect());
        }));
        args.live = z.live;
        args.reads = z.reads;
        args.stamp = stamp;
        args.key = z.key.clone();
        args.select = select;
        let (this, z_c) = (Wk::new(self), z.clone());
        szone::node(
            args,
            Arc::new(move |inputs: &[node::T], context: &sctx::T| this.get().instantiate(state.clone(), outer.clone(), sctx::input(context).clone(), &z_c, inputs)),
            inputs,
        )
    }

    /// Re-root a geometry-loop template at the selected node. Unused bindings may have different
    /// captures from the authored result, so follow its cone.
    fn focus(self: &Arc<Self>, z: &Zinfo, target: i64, probes: &[i64]) -> R<Zinfo> {
        let contains = |z: &Zinfo| target >= z.lo && target < z.hi;
        let mut inner: Option<Zinfo> = None;
        for id in z.lo..z.hi {
            if is_zone(&self.plan.nodes[id as usize].kind) {
                let candidate = self.prepare(&self.plan.nodes[id as usize])?.zone.expect("a zone node has a zone");
                if contains(&candidate) && !inner.as_ref().is_some_and(|old| old.hi - old.lo >= candidate.hi - candidate.lo) {
                    inner = Some(candidate);
                }
            }
        }
        let root = inner.as_ref().map_or(target, |i| i.plan_id);
        let mut seen: BTreeSet<i64> = BTreeSet::new();
        let mut captures: Vec<i64> = Vec::new();
        fn visit(this: &Arc<Lowerer>, z: &Zinfo, id: i64, root: i64, target: i64, probes: &[i64], seen: &mut BTreeSet<i64>, captures: &mut Vec<i64>) -> R<()> {
            if id >= z.lo && id < z.hi {
                if seen.insert(id) {
                    let node = &this.plan.nodes[id as usize];
                    let sources = if id == root && is_zone(&node.kind) {
                        let inner = this.focus(&this.prepare(node)?.zone.expect("a zone node has a zone"), target, probes)?;
                        let mut s = vec![this.int_geo(node)?];
                        s.extend(inner.captures);
                        s
                    } else {
                        this.deps(node)?
                    };
                    for s in sources {
                        visit(this, z, s, root, target, probes, seen, captures)?;
                    }
                }
            } else if !captures.contains(&id) {
                captures.push(id);
            }
            Ok(())
        }
        visit(self, z, root, root, target, probes, &mut seen, &mut captures)?;
        Ok(Zinfo { root, preview: Some((target, probes.to_vec())), captures, order: seen.into_iter().collect(), ..z.clone() })
    }

    fn int_geo(&self, node: &ENode) -> R<i64> {
        match geo_of(&node.args.iter().filter(|(k, _)| k == "geometry").cloned().collect::<Vec<_>>()).as_slice() {
            [id] => Ok(*id),
            _ => Err(fail("E_LOWER", "Zone without geometry")),
        }
    }

    /// One element's copy of the template, over the zone's own inputs.
    fn instantiate(self: &Arc<Self>, state: Option<State>, outer: Vec<(String, Value)>, live: Frame, z: &Zinfo, inputs: &[node::T]) -> szone::ElementBody {
        let this = self.clone();
        let z = z.clone();
        let build_one = {
            let (this, z, state, live) = (this.clone(), z.clone(), state.clone(), live.clone());
            move |outer: &[(String, Value)], bound: &mut HashMap<i64, node::T>, id: i64| -> R<()> {
                let n = this.plan.nodes[id as usize].clone();
                let p = this.prepare(&n)?;
                let mut options: Vec<Option<node::T>> = Vec::with_capacity(p.arity);
                for i in 0..p.arity {
                    if let Some(k) = p.kernels.iter().find(|k| k.index == i) {
                        let ins: Vec<node::T> = k.sources.iter().map(|s| bound[s].clone()).collect();
                        options.push(Some(this.kernel_node(state.clone(), Some(outer.to_vec()), k, &ins)?));
                    } else {
                        options.push(p.slots.iter().find(|(slot, _)| *slot == i).and_then(|(_, src)| bound.get(src).cloned()));
                    }
                }
                let present: Vec<node::T> = options.iter().flatten().cloned().collect();
                let made = if let Some(inner) = &p.zone {
                    let inner = match &z.preview {
                        Some((target, probes)) if *target >= inner.lo && *target < inner.hi => this.focus(inner, *target, probes)?,
                        _ => inner.clone(),
                    };
                    let mut opts = vec![this.int_geo(&n)?];
                    opts.extend(inner.captures.iter().cloned());
                    let ins: Vec<node::T> = opts.iter().filter_map(|i| bound.get(i).cloned()).collect();
                    this.make_zone(state.clone(), outer.to_vec(), &inner, ins)
                } else if n.kind == "sop/with_attr" && get(&n.args, "geometry") != Some(&Value::No_geo) {
                    let name = match get(&n.args, "attribute") {
                        Some(Value::Text(name)) => txt(name),
                        _ => return Err(fail("E_LOWER", "Attribute name must be static text")),
                    };
                    let (owner, kind) = attribute_options(&n.args)?;
                    this.attr_node(
                        Some(&n),
                        AttrOpts { state: state.clone(), elems: Some(outer.to_vec()), selection: get(&n.args, "selection").cloned(), default: get(&n.args, "default").cloned(), guard: get(&n.args, "when").cloned(), owner: Some(owner), kind: Some(kind), ..AttrOpts::default() },
                        &name,
                        get(&n.args, "values").expect("a write has values"),
                        &this.deps(&n)?,
                        &present,
                    )?
                } else if n.kind == "sop/with_group" {
                    this.grp_node(Some(&n), state.clone(), Some(outer.to_vec()), &n.args, &this.deps(&n)?, &present)?
                } else if is_geometry_arrays(&n.kind) {
                    this.arrays_node(Some(&n), None, false, state.clone(), Some(outer.to_vec()), &n.kind, &n.args, &this.deps(&n)?, &present)?
                } else {
                    edit_r(ln::with_initial_changes(edit::factory_operation(&p.factory), &p.changes, &mut || edit::instantiate_optional(&p.factory, &options)))?
                };
                let made = edit_r(build::restore_id(p.cid, &made))?;
                let mut changes = p.changes.clone();
                for (v, f) in &p.dynamic {
                    let forced = E::force_with(v, &live, state.as_ref().map(E::fork_state).as_ref(), Some(outer), None)?;
                    changes.extend(f(&forced)?);
                }
                let made = if changes.is_empty() { made } else { edit_r(node::apply_parameters(&made, &changes))?.0 };
                bound.insert(id, made);
                Ok(())
            }
        };
        // what does not read the element is built once per cook of the zone
        let mut shared: HashMap<i64, node::T> = HashMap::new();
        let mut varies: BTreeSet<i64> = BTreeSet::new();
        let mut failure: Option<Diagnostic> = None;
        for (k, j) in z.captures.iter().enumerate() {
            shared.insert(*j, inputs[k + 1].clone());
        }
        for id in &z.order {
            let n = &this.plan.nodes[*id as usize];
            let step = (|| -> R<()> {
                let special = n.kind == "zone/element" || is_zone(&n.kind);
                let p = if special { None } else { Some(this.prepare(n)?) };
                if special
                    || p.as_ref().is_some_and(|p| !p.dynamic.is_empty())
                    || kernel_data(n).is_some_and(|d| E::is_live(&d))
                    || p.as_ref().is_some_and(|p| !p.kernels.is_empty())
                    || this.deps(n)?.iter().any(|d| varies.contains(d))
                {
                    varies.insert(*id);
                    Ok(())
                } else {
                    build_one(&[], &mut shared, *id)
                }
            })();
            if let Err(e) = step {
                failure = Some(e);
                break;
            }
        }
        let (order, root, ekey) = (z.order.clone(), z.root, z.ekey.clone());
        let shared = Arc::new(shared);
        let varies = Arc::new(varies);
        let failure = Arc::new(failure);
        Arc::new(move |element: &szone::Element| {
            if let Some(e) = failure.as_ref() {
                return failed_node(e.clone());
            }
            let mut outer2 = outer.clone();
            if let Some(key) = &ekey {
                let (x, y, z) = element.position;
                outer2.insert(0, (key.clone(), Value::Vec3(x, y, z)));
            }
            let mut bound: HashMap<i64, node::T> = (*shared).clone();
            for id in &order {
                if varies.contains(id) {
                    if this.plan.nodes[*id as usize].kind == "zone/element" {
                        bound.insert(*id, szone::element_node(element));
                    } else if let Err(e) = build_one(&outer2, &mut bound, *id) {
                        return failed_node(e);
                    }
                }
            }
            bound.get(&root).cloned().unwrap_or_else(|| failed_node(fail("E_LOWER", "Zone body was not built")))
        })
    }
}

impl Lowerer {
    fn sop_instance(&self, i: &Instance) -> bool {
        self.checked.graphs.iter().find(|g| g.name == i.graph).is_some_and(|g| ty::is_cooked(&fctx::result(g.context)))
    }

    fn reads_t(v: &Value) -> bool {
        E::frame_dependent(v)
    }

    /// volatile: live, or fed by a volatile node (plan order: inputs first). A loop over geometry
    /// whose body reads `t` is live, and so are the template nodes its elements are copies of (a copy
    /// keeps the template's id, so the elements share one volatile slot per node).
    fn compute_volatile(&self) -> R<()> {
        let mut volatile = vec![false; self.plan.nodes.len()];
        let mut nodes = self.volatile_nodes.lock().unwrap();
        for node in &self.plan.nodes {
            let id = node.id as usize;
            if self.compiled[id] == 0 {
                continue;
            }
            let mut v = self.deps(node)?.iter().any(|j| volatile[*j as usize]) || (is_zone(&node.kind) && self.zinfo(node)?.live);
            if !v && !is_zone(&node.kind) {
                v = node.args.iter().any(|(_, a)| {
                    if self.templ[id] || kernel_data(node).is_some() || node.kind == E::card_kind() {
                        Self::reads_t(a) || E::state_dependent(a)
                    } else {
                        E::is_live(a) || matches!(a, Value::Fn(_)) && (Self::reads_t(a) || E::state_dependent(a))
                    }
                });
            }
            if v {
                volatile[id] = true;
                nodes.insert(self.compiled[id], ());
            }
        }
        *self.volatile.lock().unwrap() = volatile;
        Ok(())
    }

    fn compiled_context(&self) -> BTreeMap<i64, i64> {
        self.compiled.iter().enumerate().filter(|(i, id)| **id != 0 && !self.templ[*i]).map(|(i, id)| (i as i64, *id)).collect()
    }

    fn live_network(self: &Arc<Self>, network: network::Network, graph: &edit::T) -> R<network::Network> {
        let mut network = network::with_approx(self.checked.approx.clone(), &network::with_reference(self.o.reference, &network::with_profile(self.profile.clone(), &network::with_states(self.evaluated.states.clone(), &network))));
        let prepared: Vec<Prepared> = self.prepared.lock().unwrap().values().cloned().collect();
        let mut frame_nodes: BTreeMap<i64, network::FrameNode> = BTreeMap::new();
        for p in &prepared {
            if let Some(z) = &p.zone {
                if z.stateful && edit::find(graph, p.cid).is_some() {
                    let (this, z) = (Wk::new(self), z.clone());
                    frame_nodes.insert(
                        p.cid,
                        Arc::new(move |_network: &network::Network, state: &State, _live: &Frame, n: &node::T| {
                            let snapshot = E::fork_state(state);
                            Ok(build::adopt_identity(n, &this.get().make_zone(Some(snapshot), Vec::new(), &z, build::input_array(n))))
                        }),
                    );
                }
            }
        }
        for p in &prepared {
            for k in &p.kernels {
                if !E::state_dependent(&Value::Fn(k.fun)) || edit::find(graph, k.cid).is_none() {
                    continue;
                }
                let (this, k) = (Wk::new(self), k.clone());
                frame_nodes.insert(
                    k.cid,
                    Arc::new(move |_network: &network::Network, state: &State, _live: &Frame, n: &node::T| this.get().kernel_node(Some(E::fork_state(state)), None, &k, &node::inputs(n))),
                );
            }
        }
        for n in &self.plan.nodes {
            if !is_geometry_arrays(&n.kind) || edit::find(graph, self.compiled[n.id as usize]).is_none() || !E::state_dependent(&geometry_arrays_data(&n.args)) {
                continue;
            }
            let (this, n) = (Wk::new(self), n.clone());
            frame_nodes.insert(
                self.compiled[n.id as usize],
                Arc::new(move |_network: &network::Network, state: &State, _live: &Frame, node: &node::T| {
                    let this = this.get();
                    let sources = this.deps(&n)?;
                    let made = this.arrays_node(None, None, false, Some(E::fork_state(state)), None, &n.kind, &n.args, &sources, &node::inputs(node))?;
                    Ok(build::adopt_identity(node, &made))
                }),
            );
        }
        for n in &self.plan.nodes {
            if n.kind != "sop/with_group" || edit::find(graph, self.compiled[n.id as usize]).is_none() || !E::state_dependent(&group_data(&n.args)) {
                continue;
            }
            let (this, n) = (Wk::new(self), n.clone());
            frame_nodes.insert(
                self.compiled[n.id as usize],
                Arc::new(move |_network: &network::Network, state: &State, _live: &Frame, node: &node::T| {
                    let this = this.get();
                    let sources = this.deps(&n)?;
                    let made = this.grp_node(None, Some(E::fork_state(state)), None, &n.args, &sources, &node::inputs(node))?;
                    Ok(build::adopt_identity(node, &made))
                }),
            );
        }
        for n in &self.plan.nodes {
            let Some(values) = get(&n.args, "values") else { continue };
            if n.kind != "sop/with_attr" || edit::find(graph, self.compiled[n.id as usize]).is_none() {
                continue;
            }
            let program = Arc::new(Executor::compile(
                values,
                &Executor::CompileOptions { profile: Some(self.profile.clone()), approx: self.checked.approx.clone(), sink: Some(Sink::Sop_input), ..Default::default() },
            )?);
            // Fast writes choose placement in their cook, where downstream consumers are known,
            // stateful ones too: the lane has advanced the state, and the rebuilt node cooks over its
            // fork. Without the device a write materializes before cooking.
            let eligible = (!self.o.gpu.fast || self.o.gpu.backend.is_none())
                && !self.deferred_cpu(&attribute_data(&n.args))
                && ak::sources(values).is_empty()
                && Executor::root_is_packed_or_readback(&program);
            if !eligible && !E::state_dependent(&attribute_data(&n.args)) {
                continue;
            }
            let (this, n, values, program) = (Wk::new(self), n.clone(), values.clone(), program);
            frame_nodes.insert(
                self.compiled[n.id as usize],
                Arc::new(move |_network: &network::Network, state: &State, live: &Frame, node: &node::T| {
                    let this = this.get();
                    let name = match get(&n.args, "attribute") {
                        Some(Value::Text(name)) => txt(name),
                        _ => unreachable!("an attribute write names its attribute"),
                    };
                    // Only input-independent producers can materialize before SOP cooking.
                    // Attribute-reading cones keep their existing cooked-input CPU path.
                    let selected = if eligible { Executor::try_display(Some(state), None, this.o.reference, &this.o.gpu, &program, live)? } else { None };
                    let values = match &selected {
                        Some(Executor::Displayed::Cpu(value)) => value.clone(),
                        None => values.clone(),
                        Some(_) => unreachable!("a displayed write is on the CPU"),
                    };
                    if selected.is_none() && !E::state_dependent(&attribute_data(&n.args)) {
                        return Ok(node.clone());
                    }
                    let (owner, kind) = attribute_options(&n.args)?;
                    let sources = this.deps(&n)?;
                    let o = AttrOpts { state: Some(E::fork_state(state)), selection: get(&n.args, "selection").cloned(), default: get(&n.args, "default").cloned(), owner: Some(owner), kind: Some(kind), ..AttrOpts::default() };
                    let made = this.attr_node(None, o, &name, &values, &sources, &node::inputs(node))?;
                    Ok(build::adopt_identity(node, &guarded(guards_of(FUSED_GUARDS, &n.args), made)))
                }),
            );
        }
        for n in &self.plan.nodes {
            if !is_resource(n) || edit::find(graph, self.compiled[n.id as usize]).is_none() {
                continue;
            }
            let (this, n) = (Wk::new(self), n.clone());
            frame_nodes.insert(
                self.compiled[n.id as usize],
                Arc::new(move |network: &network::Network, state: &State, live: &Frame, node: &node::T| {
                    let this = this.get();
                    let Some(images) = image_provider() else {
                        return Err(fail("E_IMAGE", "Image resources need an initial-domain resolver."));
                    };
                    let context = ImageContext { compiled: this.compiled_context(), network: network.clone() };
                    let image = (images.resolve)(Some(&this.o.gpu), Some(&context), &this.evaluated.plan, state, live, &Value::Deferred(ty::image(), n.id))?;
                    if node::parameters(node) == sop::image::data_id(&image).to_string().as_bytes() {
                        Ok(node.clone())
                    } else {
                        Ok(build::adopt_identity(node, &resource_image(&n.kind, Ok(image))))
                    }
                }),
            );
        }
        network = network::with_frame_nodes(frame_nodes, &network);
        let mut drives: BTreeMap<port::Port, Value> = BTreeMap::new();
        for p in self.pending.lock().unwrap().iter().rev() {
            if edit::find(graph, p.node).is_some() {
                drives.insert(port::Port { node: p.node, path: p.field.clone() }, p.value.clone());
            }
        }
        if drives.is_empty() {
            Ok(network)
        } else {
            network::with_drives(drives, &network)
        }
    }

    fn build_graph(self: &Arc<Self>, index: usize, instance: &Instance) -> R<Graph> {
        let mut seen: BTreeSet<i64> = BTreeSet::new();
        fn reach(this: &Lowerer, id: i64, seen: &mut BTreeSet<i64>) -> R<()> {
            if seen.insert(id) {
                for d in this.deps(&this.plan.nodes[id as usize])? {
                    reach(this, d, seen)?;
                }
            }
            Ok(())
        }
        for node in &self.plan.nodes {
            if node.inst == index as i64 && ty::is_cooked(&node.ty) && !self.templ[node.id as usize] {
                reach(self, node.id, &mut seen)?;
            }
        }
        if let Value::Deferred(t, j) = &instance.result {
            if ty::is_cooked(t) {
                reach(self, *j, &mut seen)?;
            }
        }
        let mut graph = edit::empty();
        for id in &seen {
            let p = self.prepare(&self.plan.nodes[*id as usize])?;
            let mut resources: Vec<(usize, node::T)> = Vec::new();
            for k in &p.kernels {
                let inputs: R<Vec<node::T>> = k
                    .sources
                    .iter()
                    .map(|source| edit::find(&graph, self.compiled[*source as usize]).ok_or_else(|| fail("E_LOWER", "Function capture was not built before its consumer")))
                    .collect();
                resources.push((k.index, self.kernel_node(None, None, k, &inputs?)?));
            }
            for (_, n) in &resources {
                graph = edit_r(edit::add_node(None, None, n, &graph))?;
            }
            let options: Vec<Option<node::T>> = (0..p.arity)
                .map(|i| match resources.iter().find(|(k, _)| *k == i) {
                    Some((_, n)) => Some(n.clone()),
                    None => p.slots.iter().find(|(s, _)| *s == i).and_then(|(_, source)| edit::find(&graph, self.compiled[*source as usize])),
                })
                .collect();
            let n = edit_r(ln::with_initial_changes(edit::factory_operation(&p.factory), &p.changes, &mut || edit::instantiate_optional(&p.factory, &options)))?;
            let n = edit_r(build::restore_id(p.cid, &n))?;
            let inputs: Vec<Option<i64>> = options.iter().map(|o| o.as_ref().map(node::id)).collect();
            graph = edit_r(edit::add_node(Some(&inputs), Some(&p.factory), &n, &graph))?;
            if !p.changes.is_empty() {
                graph = edit_r(edit::apply_parameters(&graph, p.cid, &p.changes))?.0;
            }
        }
        let (mut graph, root) = match &instance.result {
            Value::Deferred(t, id) if ty::is_cooked(t) => (graph, Some(self.compiled[*id as usize])),
            Value::No_geo => (graph, None),
            Value::Residual(r) if E::Residual::residual_view(*r)?.term.ty == ty::geometry() => {
                let ids = cooked_sources(&instance.result);
                let captures: R<Vec<(i64, Ty, node::T)>> = ids
                    .iter()
                    .map(|id| {
                        edit::find(&graph, self.compiled[*id as usize])
                            .map(|n| (*id, self.plan.nodes[*id as usize].ty.clone(), n))
                            .ok_or_else(|| fail("E_DATA_SOURCE", "A cooked selection capture was not built."))
                    })
                    .collect();
                let n = self.structural_node(None, *r, &captures?)?;
                (edit_r(edit::add_node(None, None, &n, &graph))?, Some(node::id(&n)))
            }
            _ => return Err(fail("E_LOWER", &format!("Graph {} does not return geometry", instance.graph))),
        };
        if let Some(id) = root {
            graph = edit_r(edit::set_root(id, &graph))?;
        }
        Ok(Graph { name: instance.graph.clone(), instance: index, default: instance.default, network: self.live_network(network::of_geometry(&graph), &graph)?, root })
    }

    /// Previewing an unused template must not allocate provenance or zones after this immutable
    /// lowering has been handed to the document.
    fn preview(self: &Arc<Self>, target: i64, probes: &[i64], net: &network::Network) -> R<Option<(network::Network, i64)>> {
        if target < 0 || target as usize >= self.compiled.len() || self.compiled[target as usize] == 0 || probes.len() != self.plan.nodes[target as usize].iter.len() {
            return Ok(None);
        }
        if !self.templ[target as usize] {
            return Ok(edit::find(&net.geometry, self.compiled[target as usize]).map(|_| (net.clone(), self.compiled[target as usize])));
        }
        let mut outer: Option<ENode> = None;
        for node in &self.plan.nodes {
            if is_zone(&node.kind)
                && !self.templ[node.id as usize]
                && target >= Self::int_arg(node, "lo")?
                && target < Self::int_arg(node, "hi")?
                && edit::find(&net.geometry, self.compiled[node.id as usize]).is_some()
            {
                outer = Some(node.clone());
                break;
            }
        }
        let Some(node) = outer else { return Ok(None) };
        let original = self.prepare(&node)?.zone.expect("a zone node has a zone");
        let z = self.focus(&original, target, probes)?;
        let mut sources = vec![self.int_geo(&node)?];
        sources.extend(z.captures.iter().cloned());
        let inputs: R<Vec<node::T>> = sources
            .iter()
            .map(|id| edit::find(&net.geometry, self.compiled[*id as usize]).ok_or_else(|| fail("E_LOWER", "Preview capture is not in the owning network")))
            .collect();
        let viewed = self.make_zone(None, Vec::new(), &z, inputs?);
        let root = node::id(&viewed);
        let geometry = edit_r(edit::add_node(None, None, &viewed, &net.geometry))?;
        let net = network::with_geometry(&geometry, net)?;
        let mut frames = net.frame_nodes.clone();
        if z.stateful {
            let (this, z) = (Wk::new(self), z.clone());
            frames.insert(
                root,
                Arc::new(move |_network: &network::Network, state: &State, _live: &Frame, n: &node::T| Ok(build::adopt_identity(n, &this.get().make_zone(Some(E::fork_state(state)), Vec::new(), &z, build::input_array(n))))),
            );
        }
        Ok(Some((network::with_frame_nodes(frames, &net), root)))
    }

    /// A card's factory node (the node menu's and inspector's), over the nodes its call's slots name
    /// in the graph that holds the card, with the call's parameters and functions.
    fn card_node(self: &Arc<Self>, graphs: &[Graph], cid: i64) -> Result<node::T, String> {
        let Some(n) = self.evaluated.plan.nodes.iter().find(|n| n.kind == E::card_kind() && self.compiled[n.id as usize] == cid) else {
            return Err("No card has this compiled id".to_owned());
        };
        let graph = graphs.iter().find(|g| edit::find(&g.network.geometry, cid).is_some());
        let (Some(Value::Text(kind)), Some(Value::Record(parameters)), Some(Value::Record(slots)), Some(graph)) = (get(&n.args, "kind"), get(&n.args, "parameters"), get(&n.args, "slots"), graph) else {
            return Err("The card has no kind, parameters, slots or graph".to_owned());
        };
        let kind = txt(kind);
        let run = || -> R<node::T> {
            let slot_value = |v: &Value| -> Value {
                match v {
                    Value::Int(id) => Value::Deferred(self.plan.nodes[*id as usize].ty.clone(), *id),
                    _ => Value::No_geo,
                }
            };
            let rest: Vec<String> = self.lisp_definition(&kind).map(|d| d.slots.iter().filter(|s| s.rest).map(|s| s.name.clone()).collect()).unwrap_or_default();
            let mut args: Vec<(String, Value)> = slots
                .iter()
                .filter_map(|(name, ids)| match ids {
                    Value::List(ids) if ids.is_empty() => None,
                    Value::List(ids) if ids.len() == 1 && !rest.contains(name) => Some((name.clone(), slot_value(&ids[0]))),
                    Value::List(ids) => Some((name.clone(), Value::List(Arc::new(ids.iter().map(slot_value).collect())))),
                    _ => None,
                })
                .collect();
            args.extend(n.args.iter().filter(|(name, _)| !["value", "kind", "parameters", "slots"].contains(&name.as_str())).cloned());
            let tag_base = {
                let mut card = n.site.clone();
                card.push(format!("node:{kind}"));
                let mut found = None;
                for ((site, iter), base) in self.concat_tags.lock().unwrap().iter() {
                    if *iter == n.iter && site.len() > card.len() && site[..card.len()] == card[..] {
                        found = Some(*base);
                    }
                }
                found
            };
            // its own site: the call's kernels must not take the ids of the body's
            let mut site = n.site.clone();
            site.push("~factory".to_owned());
            let p = self.lower_default(tag_base, &ENode { kind: kind.clone(), args: args.clone(), site, ..n.clone() }, &args, cid)?;
            let changes = match self.lisp_definition(&kind) {
                Some(d) => card_values(&d.fields, &schema_defaults(&ln::schema("parameters", &d.fields, &ep::for_node(&self.checked, &d))), parameters),
                None => Vec::new(),
            };
            let compile = |id: i64| edit_r(edit::compile_node(&graph.network.geometry, self.compiled[id as usize]));
            let mut options: Vec<Option<node::T>> = Vec::new();
            for i in 0..p.arity {
                options.push(match p.kernels.iter().find(|k| k.index == i) {
                    Some(k) => {
                        let ins: R<Vec<node::T>> = k.sources.iter().map(|s| compile(*s)).collect();
                        Some(self.kernel_node(None, None, k, &ins?)?)
                    }
                    None => match p.slots.iter().find(|(s, _)| *s == i) {
                        Some((_, id)) => Some(compile(*id)?),
                        None => None,
                    },
                });
            }
            let made = edit_r(ln::with_initial_changes(edit::factory_operation(&p.factory), &changes, &mut || edit::instantiate_optional(&p.factory, &options)))?;
            edit_r(build::restore_id(cid, &made))
        };
        run().map_err(|d| rays_diagnostic::to_string(&d))
    }
}

fn make_lowerer(factories: Vec<Factory>, o: Options, checked_in: Workspace) -> R<Arc<Lowerer>> {
    install_hooks();
    let image_origins: Arc<Mutex<HashMap<(i64, W::Path, Vec<i64>), Option<W::Path>>>> = Arc::new(Mutex::new(HashMap::new()));
    let observe: Arc<dyn Fn(Option<W::Path>, ResidualId) + Send + Sync> = {
        let origins = image_origins.clone();
        Arc::new(move |producer: Option<W::Path>, residual: ResidualId| {
            let Ok(view) = E::Residual::residual_view(residual) else { return };
            let key = (view.instance, view.site.clone(), view.iter.clone());
            let mut origins = origins.lock().unwrap();
            let origin = match origins.get(&key) {
                None => producer,
                Some(Some(previous)) if producer.as_ref() == Some(previous) => producer,
                _ => None,
            };
            origins.insert(key, origin);
        })
    };
    let (checked, evaluated) = flow_ir::flow_ir::qualify_workspace(
        &flow_ir::flow_ir::QualifyOptions { record: true, inputs: o.inputs.clone(), observe: Some(observe), replay: true, verify: false, defer: o.defer, gpu: Some(o.gpu.clone()) },
        &checked_in,
    )?;
    let checked = Arc::new(checked);
    let evaluated = Arc::new(evaluated);
    let profile = Profile::create(o.clock.clone());
    // Kernel bodies are cache inputs as well as their captures. The digest is lazy so ordinary
    // catalog edits retain their existing pipeline counts. ponytail: the whole source invalidates
    // kernels; narrow to body/defn cones if unrelated structural edits measurably recook them.
    let kernel_source: Lazy = {
        let checked = checked.clone();
        lazy(Box::new(move || cached_digest(&checked, &[], &|| ck::kernel_source(&checked))))
    };
    let plan = if o.reference {
        evaluated.plan.clone()
    } else {
        let plan = fold_writes(o.gpu.fast && o.gpu.backend.is_some(), o.defer, &evaluated.plan)?;
        if o.gpu.fast && o.gpu.backend.is_some() { fold_mesh_writes(&plan) } else { plan }
    };
    let plan = Plan { nodes: plan.nodes.iter().map(|n| failed_card_as_call(n, &plan, &checked)).collect(), ..plan.clone() };
    let image_sites: Vec<Option<W::Path>> = plan
        .nodes
        .iter()
        .map(|node| if node.kind == "image/map" { image_origins.lock().unwrap().get(&(node.inst, node.site.clone(), node.iter.clone())).cloned().flatten() } else { None })
        .collect();
    let shared = Arc::new(CallEnvShared { checked: checked.clone(), factories: factories.clone(), o: o.clone(), def_context_use: Mutex::new(None) });
    let lisp_factories: Vec<Factory> = checked.nodes.iter().map(|d| lisp_factory(&shared, &kernel_source, None, &[], None, d)).collect();
    let templ = template_flags(&plan.nodes);
    let sites = o.sites.clone();
    let site_table: HashMap<W::Path, i64> = sites.iter().enumerate().map(|(i, p)| (p.clone(), i as i64)).collect();
    let mut lowerer = Lowerer {
        o: o.clone(),
        factories,
        checked,
        evaluated,
        profile,
        compiled: vec![0; plan.nodes.len()],
        plan,
        templ,
        kernel_source,
        card_sources: OnceLock::new(),
        ids: Mutex::new(Ids { ids: o.compiled_ids.clone(), site_table, site_list: sites }),
        zinfos: Mutex::new(HashMap::new()),
        params_cache: Mutex::new(HashMap::new()),
        pending: Mutex::new(Vec::new()),
        provenance: Mutex::new(BTreeMap::new()),
        tags: AtomicI64::new(o.tag_base),
        zones: Mutex::new(Vec::new()),
        prepared: Mutex::new(HashMap::new()),
        concat_tags: Mutex::new(HashMap::new()),
        lisp_factories,
        call_env: shared,
        image_origins,
        volatile_nodes: Mutex::new(BTreeMap::new()),
        volatile: Mutex::new(Vec::new()),
    };
    // every plan node gets its id up front, in plan order, so ids do not depend on which network asks first
    for node in lowerer.plan.nodes.clone() {
        if lowerer.sop_instance(&lowerer.plan.instances[node.inst as usize]) && ty::is_cooked(&node.ty) {
            let id = lowerer.compiled_id(&node);
            lowerer.compiled[node.id as usize] = id;
        }
    }
    lowerer.compute_volatile()?;
    let lowerer = Arc::new(lowerer);
    let _ = image_sites;
    Ok(lowerer)
}

fn finish(lowerer: &Arc<Lowerer>) -> R<T> {
    let l = lowerer;
    let mut graphs: Vec<Graph> = Vec::new();
    for (index, instance) in l.plan.instances.iter().enumerate() {
        if l.sop_instance(instance) {
            graphs.push(l.build_graph(index, instance)?);
        }
    }
    {
        let volatile = l.volatile.lock().unwrap().clone();
        let prepared: Vec<Prepared> = l.prepared.lock().unwrap().values().cloned().collect();
        let mut nodes = l.volatile_nodes.lock().unwrap();
        for p in &prepared {
            for k in &p.kernels {
                if E::frame_dependent(&Value::Fn(k.fun)) || E::state_dependent(&Value::Fn(k.fun)) || k.sources.iter().any(|id| volatile[*id as usize]) {
                    nodes.insert(k.cid, ());
                }
            }
        }
    }
    // Previewing an unused template must not allocate provenance or zones after this immutable
    // lowering has been handed to the document.
    for node in &l.plan.nodes {
        if l.compiled[node.id as usize] != 0 && node.kind != "zone/element" && ty::is_cooked(&node.ty) {
            l.prepare(node)?;
        }
    }
    let image_sites: Vec<Option<W::Path>> = l
        .plan
        .nodes
        .iter()
        .map(|node| if node.kind == "image/map" { l.image_origins.lock().unwrap().get(&(node.inst, node.site.clone(), node.iter.clone())).cloned().flatten() } else { None })
        .collect();
    let (preview, card, graphs_for_card) = (l.clone(), l.clone(), graphs.clone());
    let ids = l.ids.lock().unwrap();
    let evaluated = l.evaluated.clone();
    let reads = {
        let evaluated = evaluated.clone();
        Arc::new(std::sync::LazyLock::new(Box::new(move || {
            let mut found = deps_mod::STATIC;
            for n in &evaluated.plan.nodes {
                for (_, v) in &n.args {
                    found = dep_union(found, ak::input_reads(v));
                }
            }
            for v in &evaluated.states {
                found = dep_union(found, ak::input_reads(v));
            }
            found
        }) as Box<dyn FnOnce() -> deps_mod::T + Send + Sync>))
    };
    Ok(T {
        gpu: l.o.gpu.clone(),
        defer: l.o.defer,
        graphs,
        factories: l.lisp_factories.clone(),
        card_node: Arc::new(move |cid| card.card_node(&graphs_for_card, cid)),
        compiled_ids: ids.ids.clone(),
        sites: ids.site_list.clone(),
        compiled: l.compiled_context(),
        pending: l.pending.lock().unwrap().iter().rev().cloned().collect(),
        provenance: l.provenance.lock().unwrap().clone(),
        zones: l.zones.lock().unwrap().iter().rev().cloned().collect(),
        volatile: l.volatile_nodes.lock().unwrap().clone(),
        plan: evaluated.plan.clone(),
        image_sites,
        reads,
        states: evaluated.states.clone(),
        evaluated: evaluated.clone(),
        approx: l.checked.approx.clone(),
        approx_reasons: l.checked.approx_reasons.clone(),
        profile: l.profile.clone(),
        preview: Arc::new(move |target, probes, net| preview.preview(target, probes, net).ok().flatten()),
        lowering: Arc::downgrade(l),
    })
}

/// One lowering of an already-qualified-by-`lower_checked` workspace (the OCaml `lower_checked`).
pub fn lower_checked(factories: Vec<Factory>, o: Options, checked: Workspace) -> R<T> {
    let lowerer = make_lowerer(factories, o, checked)?;
    finish(&lowerer)
}

/// Lower an already checked document, without rebuilding a catalog or checking its source again.
/// `provenance_tags` (default true) numbers a Lisp `sop/merge`'s inputs for the editor (`tags`).
pub fn of_checked(factories: Option<Vec<Factory>>, o: Options, checked: &Workspace) -> R<T> {
    let checked = ep::checked(checked.clone())?;
    lower_checked(factories.unwrap_or_else(sop::nodes::factories), o, checked)
}

/// `factories` is the SOP catalog (default `sop::nodes::factories`). Errors: the checker's first
/// error, the evaluator's, or `E_LOWER`.
pub fn workspace(factories: Option<Vec<Factory>>, extra: Vec<catalog::Descriptor>, o: Options, source: &[Syntax]) -> R<T> {
    let factories = factories.unwrap_or_else(sop::nodes::factories);
    let cat = catalog::of_factories(crate::manifest::VERSION, &extra, None, &factories)?;
    match catalog::check_workspace(ops::all(), false, None, &cat, source) {
        (Some(checked), _) => of_checked(Some(factories), o, &checked),
        (None, diagnostics) => Err(diagnostics.into_iter().find(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity).unwrap_or_else(|| fail("E_LOWER", "Workspace did not check"))),
    }
}

/// The captured geometry node's own instance, including graph overrides.
pub fn source_context(lowered: &T, node: i64) -> R<ImageContext> {
    if node < 0 || node as usize >= lowered.plan.nodes.len() || !is_geometry_ty(&lowered.plan.nodes[node as usize].ty) {
        return Err(data_source("Capture needs a geometry plan node."));
    }
    let Some(id) = lowered.compiled.get(&node) else { return Err(data_source("Capture source was not compiled.")) };
    match lowered.graphs.iter().find(|g| g.instance as i64 == lowered.plan.nodes[node as usize].inst && edit::find(&g.network.geometry, *id).is_some()) {
        None => Err(data_source("Capture source has no owning network.")),
        Some(graph) => Ok(ImageContext { compiled: lowered.compiled.clone(), network: graph.network.clone() }),
    }
}

/// Keep only the source's upstream inputs, drives and frame callbacks; retain the enclosing context
/// separately for any nested image captures.
pub fn source_cone(context: &ImageContext, node: i64) -> R<(network::Network, i64)> {
    let Some(root) = context.compiled.get(&node).copied() else { return Err(data_source("Capture source was not compiled.")) };
    if edit::find(&context.network.geometry, root).is_none() {
        return Err(data_source("Capture source is outside its owning network."));
    }
    let mut seen: BTreeSet<i64> = BTreeSet::new();
    let mut stack = vec![root];
    while let Some(id) = stack.pop() {
        if seen.insert(id) {
            if let Some(inputs) = edit::inputs(&context.network.geometry, id) {
                stack.extend(inputs.into_iter().flatten());
            }
        }
    }
    let removed: Vec<i64> = edit::inspect(&context.network.geometry).into_iter().filter(|n| !seen.contains(&n.id)).map(|n| n.id).collect();
    Ok((network::remove_nodes(&removed, &context.network)?, root))
}

/// (live, cached) node counts.
pub fn counts(lowered: &T) -> (i64, i64) {
    let live = lowered.compiled.values().filter(|id| lowered.volatile.contains_key(id)).count() as i64;
    (live, lowered.compiled.len() as i64 - live)
}

/// The viewport header `t N live · M cached · frame cook X ms`; `None` when nothing is live.
pub fn status(lowered: &T, seconds: f64) -> Option<String> {
    match counts(lowered) {
        (0, _) => None,
        (live, cached) => Some(format!("t {live} live · {cached} cached · frame cook {} ms", cfmt::f(seconds * 1000.0, 1))),
    }
}

/// The origin of a tag: provenance, else a loop over geometry, whose element `i` has tag `base + i`.
pub fn origin(lowered: &T, tag: i64) -> Option<Origin> {
    if let Some(found) = lowered.provenance.get(&tag) {
        return Some(found.clone());
    }
    lowered.zones.iter().find_map(|z| {
        let index = tag - z.base;
        if index >= 0 && index < szone::MAX_ELEMENTS {
            let mut iter = z.iter.clone();
            iter.push(index);
            Some(Origin { merge: z.cid, input: index, source: z.cid, site: z.body_site.clone(), iter })
        } else {
            None
        }
    })
}

/// Every tag whose origin is this site at this iteration tuple.
pub fn tags(lowered: &T, site: &W::Path, iter: &[i64]) -> Vec<i64> {
    let mut tags: Vec<i64> = lowered.provenance.iter().rev().filter(|(_, o)| o.site == *site && o.iter == iter).map(|(tag, _)| *tag).collect();
    let n = iter.len();
    for z in &lowered.zones {
        if z.site == *site && z.iter == iter {
            let mut head: Vec<i64> = (0..z.count.load(Ordering::SeqCst).max(0)).map(|i| z.base + i).collect();
            head.extend(tags);
            tags = head;
        } else if z.body_site == *site && n >= 1 && z.iter.len() == n - 1 && z.iter[..] == iter[..n - 1] {
            tags.insert(0, z.base + iter[n - 1]);
        }
    }
    tags
}

pub fn zone_count(lowered: &T, site: &W::Path) -> Option<i64> {
    lowered.zones.iter().find_map(|z| if z.site == *site && z.count.load(Ordering::SeqCst) >= 0 { Some(z.count.load(Ordering::SeqCst)) } else { None })
}

pub fn zone_element(lowered: &T, site: &W::Path, k: i64) -> Option<(String, (f64, f64, f64))> {
    lowered.zones.iter().find_map(|z| {
        if z.site != *site {
            return None;
        }
        let key = z.ekey.as_ref()?;
        let positions = z.positions.lock().unwrap().clone();
        if k >= 0 && (k as usize) < positions.len() { Some((key.clone(), positions[k as usize])) } else { None }
    })
}

pub type FieldCall = (i64, i64, Arc<dyn Fn(i64) -> R<Vec<(W::Path, Vec<(Vec<i64>, Value)>)>> + Send + Sync>);

/// Exact selected-call probes for the sampled field domains. Compute one lattice position and
/// reference-evaluate one function call; never allocate or force the complete grid.
pub fn field_calls(state: Option<&State>, live: &Frame, resolve: flow_ir::core_exec::ResolveFn, lowered: &T, zone: &W::Path, outer: &[i64]) -> Vec<FieldCall> {
    let state = state.map(E::fork_state);
    let defaults: Vec<param::FieldView> = lowered.factories.iter().find(|f| edit::factory_key(f) == "iso_surface").map(|f| edit::factory_fields(f).to_vec()).unwrap_or_default();
    let vector = |args: &[(String, Value)], name: &str| -> Option<(f64, f64, f64)> {
        match get(args, name) {
            Some(value) => match E::Host::force_reference(value, live, state.as_ref(), None, Some(resolve.clone())) {
                Ok(Value::Vec3(x, y, z)) => Some((x, y, z)),
                _ => None,
            },
            None => {
                let component = |suffix: &str| -> Option<f64> {
                    defaults.iter().find_map(|f| match (&f.current, f.name == format!("{name}{suffix}")) {
                        (param::Value::Float(v), true) => Some(*v),
                        _ => None,
                    })
                };
                Some((component("_x")?, component("_y")?, component("_z")?))
            }
        }
    };
    let mut offset: i64 = 0;
    let mut out: Vec<FieldCall> = Vec::new();
    for node in &lowered.plan.nodes {
        let args: Option<Vec<(String, Value)>> = if node.kind == E::card_kind() && get(&node.args, "kind") == Some(&Value::Text(b"sop/iso_surface".to_vec())) {
            match get(&node.args, "parameters") {
                Some(Value::Record(parameters)) => Some(node.args.iter().chain(parameters.iter()).cloned().collect()),
                _ => None,
            }
        } else {
            None
        };
        let Some(args) = args else { continue };
        let Some(Value::Fn(f)) = get(&args, "field") else { continue };
        if E::Host::function_scope(*f) != Some((zone.clone(), outer.to_vec())) {
            continue;
        }
        let (Some((rx, ry, rz)), Some((lx, ly, lz)), Some((hx, hy, hz))) = (vector(&args, "resolution"), vector(&args, "min"), vector(&args, "max")) else { continue };
        let limit = (1i64 << 54) / 3;
        let valid = |n: f64| n.is_finite() && n >= 1.0 && n.floor() == n && n < limit as f64;
        if !(valid(rx) && valid(ry) && valid(rz)) {
            continue;
        }
        let (nx, ny, nz) = (rx as i64 + 1, ry as i64 + 1, rz as i64 + 1);
        if nx > limit / ny || nx * ny > limit / nz {
            continue;
        }
        let count = nx * ny * nz;
        let start = offset;
        if count > i64::MAX >> 1 - start {
            continue;
        }
        offset = start + count;
        let (f, live, state, resolve) = (*f, live.clone(), state.clone(), resolve.clone());
        let at: Arc<dyn Fn(i64) -> R<Vec<(W::Path, Vec<(Vec<i64>, Value)>)>> + Send + Sync> = Arc::new(move |k: i64| {
            if k < 0 || k >= count {
                return Err(fail("E_LIST_RANGE", "Probe index is outside the field grid."));
            }
            let p = vec![
                rays_math::fma::fma((k % nx) as f64, (hx - lx) / rx, lx),
                rays_math::fma::fma((k / nx % ny) as f64, (hy - ly) / ry, ly),
                rays_math::fma::fma((k / (nx * ny)) as f64, (hz - lz) / rz, lz),
            ];
            let signature = FnSignature { params: vec![Ty::Vec3], result: Ty::Float };
            match E::Host::map_function(None, None, &signature, f, &[Value::Vec3_array(Arc::new(p))])? {
                Value::Residual(residual) => {
                    let (_, at) = E::Host::map_probe(residual, &live, state.as_ref(), Some(resolve.clone()), None, (start + k) as usize)?;
                    at(0)
                }
                _ => unreachable!("a mapped function is a residual"),
            }
        });
        out.push((start, count, at));
    }
    out
}

/// The Rust evaluator marks a Lisp call whose body failed as a `$card` with no value and the
/// diagnostic attached; the lowering reads it as the OCaml evaluator leaves it, the written-out
/// call (`kind` with its slot inputs and canonical parameters) carrying the error, so its factory
/// node reports the body's failure at cook.
fn failed_card_as_call(n: &ENode, plan: &Plan, checked: &Workspace) -> ENode {
    if n.kind != E::card_kind() || n.error.is_none() {
        return n.clone();
    }
    let (Some(Value::Text(kind)), Some(Value::Record(parameters)), Some(Value::Record(slots))) = (get(&n.args, "kind"), get(&n.args, "parameters"), get(&n.args, "slots")) else {
        return n.clone();
    };
    let kind = txt(kind);
    let rest: Vec<String> = checked.nodes.iter().find(|d| d.kind.qualified == kind).map(|d| d.slots.iter().filter(|s| s.rest).map(|s| s.name.clone()).collect()).unwrap_or_default();
    let slot_value = |v: &Value| -> Value {
        match v {
            Value::Int(id) => Value::Deferred(plan.nodes[*id as usize].ty.clone(), *id),
            _ => Value::No_geo,
        }
    };
    let mut args: Vec<(String, Value)> = slots
        .iter()
        .filter_map(|(name, ids)| match ids {
            Value::List(ids) if ids.is_empty() => None,
            Value::List(ids) if ids.len() == 1 && !rest.contains(name) => Some((name.clone(), slot_value(&ids[0]))),
            Value::List(ids) => Some((name.clone(), Value::List(Arc::new(ids.iter().map(slot_value).collect())))),
            _ => None,
        })
        .collect();
    args.extend(parameters.iter().cloned());
    args.extend(n.args.iter().filter(|(name, _)| !["value", "kind", "parameters", "slots"].contains(&name.as_str())).cloned());
    ENode { kind, args, ..n.clone() }
}
