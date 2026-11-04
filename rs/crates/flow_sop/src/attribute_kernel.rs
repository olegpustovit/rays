//! Port of lib/flow_sop/attribute_kernel.ml: cook-time connection between immutable geometry and
//! packed Flow data, and the attribute write node. The OCaml mention of Lower (fold_writes) is only
//! a comment, so no forward hook is needed: Lower calls this module, never the reverse.
use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

use flow_check::op;
use flow_check::workspace::{self as W, HofKind, Node as TNode, Term};
use flow_eval as E;
use flow_ir::flow_ir::{Cost, Executor, Gpu, Sink};
use flow_ir::packed::Column;
use flow_syntax::context as fctx;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self as V, Value};
use rays_diagnostic::Diagnostic;
use rdk::attribute::{self, Owner, Storage};
use rdk::cancel;
use rdk::error as rerror;
use rdk::geometry;
use rdk::packed::{Float2, Float3, Float4, FloatArray, IntArray};
use sop::context::{self, dependencies as D};
use sop::node::{self, build, CookMode, Elementwise, Facts, InputUse, Topology};

use flow_ir::flow_ir::Id as FId;
use crate::{attribute_metadata, graph_kernel, group_kernel, topology_kernel_impl};

/// OCaml `geometry : int -> Rdk.Geometry.t option`.
pub type GeometryFn<'a> = &'a (dyn Fn(i64) -> Option<geometry::T> + Send + Sync);
pub type GeometryLookup = Arc<dyn Fn(i64) -> Option<geometry::T> + Send + Sync>;

fn fail(code: &str, message: &str) -> Diagnostic {
    V::fail(code, message)
}
fn err<X>(code: &str, message: &str) -> Result<X, Diagnostic> {
    Err(fail(code, message))
}
fn assoc<'a>(fields: &'a [(String, Value)], key: &str) -> Option<&'a Value> {
    fields.iter().find(|(k, _)| k == key).map(|(_, v)| v)
}
fn text_of(value: &Value) -> Option<String> {
    match value {
        Value::Text(bytes) => Some(String::from_utf8_lossy(bytes).into_owned()),
        _ => None,
    }
}
fn geometry_id(value: &Value) -> Option<i64> {
    match value {
        Value::Deferred(t, id) if ty::is_geometry(t) => Some(*id),
        _ => None,
    }
}
fn float_array(v: Vec<f64>) -> Value {
    Value::Float_array(Arc::new(v))
}
fn max_floats(width: usize) -> usize {
    op::MAX_WOSIZE / width.max(1)
}
fn rerr(e: &rerror::T) -> String {
    rerror::message(e).to_owned()
}
fn sorted_unique<X: Ord>(mut v: Vec<X>) -> Vec<X> {
    v.sort();
    v.dedup();
    v
}

// ---- sources / reads -------------------------------------------------------------------------

#[derive(Default)]
struct Seen {
    residuals: HashSet<i64>,
    functions: HashSet<i64>,
}

pub fn sources(value: &Value) -> Vec<i64> {
    fn visit(value: &Value, seen: &mut Seen, ids: &mut Vec<i64>) {
        match value {
            Value::Deferred(t, id) if ty::is_geometry(t) => ids.push(*id),
            Value::Residual(r) => {
                if seen.residuals.insert(r.0) {
                    if let Ok(view) = E::Residual::residual_view(*r) {
                        for (_, v) in &view.bindings {
                            visit(v, seen, ids);
                        }
                    }
                }
            }
            Value::List(vs) => vs.iter().for_each(|v| visit(v, seen, ids)),
            Value::Fn(f) => {
                if seen.functions.insert(f.0) {
                    for (_, v) in E::Residual::function_bindings(*f) {
                        visit(&v, seen, ids);
                    }
                }
            }
            Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => {
                fs.iter().for_each(|(_, v)| visit(v, seen, ids))
            }
            _ => {}
        }
    }
    let mut ids = Vec::new();
    visit(value, &mut Seen::default(), &mut ids);
    sorted_unique(ids)
}

const TOPOLOGY_OPS: [&str; 11] = [
    "sop/count", "sop/edges", "sop/topology", "sop/adjacency", "sop/components", "sop/triangles", "sop/fans",
    "sop/group", "sop/group_names", "sop/group_order", "sop/group_entries",
];
const ATTR_OPS: [&str; 5] = ["sop/attr", "sop/attr_checked", "sop/attr_planes", "sop/has_attr", "sop/attr_type"];

fn field_of(value: Option<Value>, field: &str) -> Option<Value> {
    match value? {
        Value::Record(fs) | Value::Struct(_, _, fs) => assoc(&fs, field).cloned(),
        _ => None,
    }
}

/// The captured value a term names (`captured` / `lookup` of the OCaml).
fn lookup(bindings: &[(String, Value)], term: &Term) -> Option<Value> {
    match &term.node {
        TNode::Text(name) => Some(Value::Text(name.clone())),
        TNode::RefBinding(name, fields) => {
            let mut value = assoc(bindings, name).cloned();
            for field in fields {
                value = field_of(value, field);
            }
            value
        }
        TNode::Get(inner, field) => field_of(lookup(bindings, inner), field),
        TNode::Expanded { body, .. } | TNode::Bypass(body) => lookup(bindings, body),
        _ => None,
    }
}

/// `captured`: like `lookup` but a literal `Text` term is not a capture.
pub fn captured(bindings: &[(String, Value)], term: &Term) -> Option<Value> {
    match &term.node {
        TNode::RefBinding(name, fields) => {
            let mut value = assoc(bindings, name).cloned();
            for field in fields {
                value = field_of(value, field);
            }
            value
        }
        TNode::Get(inner, field) => field_of(captured(bindings, inner), field),
        TNode::Bypass(body) | TNode::Expanded { body, .. } => captured(bindings, body),
        _ => None,
    }
}

struct Reads {
    seen: Seen,
    names: Vec<String>,
    opaque: bool,
}

fn arg_geometry(bindings: &[(String, Value)], args: &[(String, Term)]) -> Option<i64> {
    args.iter().find(|(k, _)| k == "geometry").and_then(|(_, t)| lookup(bindings, t)).as_ref().and_then(geometry_id)
}

fn checked_reads(bindings: &[(String, Value)], term: &Term, st: &mut Reads) -> Vec<i64> {
    fn scan(bindings: &[(String, Value)], term: &Term, st: &mut Reads, safe: &mut Vec<i64>, unsafe_: &mut Vec<i64>) {
        match &term.node {
            TNode::Op { op, args, .. } if ATTR_OPS.contains(&op.as_str()) => {
                let name = args.iter().find(|(k, _)| k == "attribute").and_then(|(_, t)| lookup(bindings, t));
                match (name.as_ref().and_then(text_of), arg_geometry(bindings, args)) {
                    (Some(name), Some(id)) => {
                        st.names.push(name);
                        safe.push(id);
                    }
                    _ => st.opaque = true,
                }
            }
            TNode::Op { op, args, .. } if TOPOLOGY_OPS.contains(&op.as_str()) => match arg_geometry(bindings, args) {
                Some(id) => safe.push(id),
                None => st.opaque = true,
            },
            TNode::Op { op, args, .. } if op == "sop/attr_names" || op == "sop/attr_entries" => {
                st.names.push("*".to_owned());
                match arg_geometry(bindings, args) {
                    Some(id) => safe.push(id),
                    None => st.opaque = true,
                }
            }
            TNode::RefBinding(..) if ty::is_geometry(&term.ty) => {
                if let Some(Value::Deferred(_, id)) = lookup(bindings, term) {
                    unsafe_.push(id);
                }
            }
            TNode::Bypass(call) => {
                if let Some(input) = W::bypass_input(call) {
                    scan(bindings, &input, st, safe, unsafe_);
                }
            }
            _ => {
                for child in W::children(term) {
                    scan(bindings, &child, st, safe, unsafe_);
                }
            }
        }
    }
    let (mut safe, mut unsafe_) = (Vec::new(), Vec::new());
    scan(bindings, term, st, &mut safe, &mut unsafe_);
    safe.retain(|id| !unsafe_.contains(id));
    safe
}

fn attr_read(args: &[(String, Value)], st: &mut Reads) {
    match (assoc(args, "attribute").and_then(text_of), assoc(args, "geometry").and_then(geometry_id)) {
        (Some(name), Some(_)) => st.names.push(name),
        _ => st.opaque = true,
    }
}

fn visit_reads(safe: &[i64], value: &Value, st: &mut Reads) {
    match value {
        Value::Source(op, _, args) if TOPOLOGY_OPS.contains(&op.as_str()) => {
            if assoc(args, "geometry").and_then(geometry_id).is_none() {
                st.opaque = true;
            }
        }
        Value::Source(op, _, args) if op == "sop/attr_names" || op == "sop/attr_entries" => {
            st.names.push("*".to_owned());
            if assoc(args, "geometry").and_then(geometry_id).is_none() {
                st.opaque = true;
            }
        }
        Value::Struct(op, _, args) if op == "sop/attr" => attr_read(args, st),
        Value::Source(op, _, args) if ATTR_OPS.contains(&op.as_str()) => attr_read(args, st),
        Value::Deferred(t, id) if ty::is_geometry(t) => {
            if !safe.contains(id) {
                st.opaque = true;
            }
        }
        Value::Residual(r) => {
            if st.seen.residuals.insert(r.0) {
                if let Ok(view) = E::Residual::residual_view(*r) {
                    let safe = checked_reads(&view.bindings, &view.term, st);
                    for (_, v) in &view.bindings {
                        visit_reads(&safe, v, st);
                    }
                }
            }
        }
        Value::Fn(f) => {
            if st.seen.functions.insert(f.0) {
                let bindings = E::Residual::function_bindings(*f);
                let safe = match E::Residual::function_body(*f) {
                    None => {
                        st.opaque = true;
                        Vec::new()
                    }
                    Some((_, body)) => checked_reads(&bindings, &body, st),
                };
                for (_, v) in &bindings {
                    visit_reads(&safe, v, st);
                }
            }
        }
        Value::List(values) => values.iter().for_each(|v| visit_reads(safe, v, st)),
        Value::Record(fs) | Value::Struct(_, _, fs) | Value::Source(_, _, fs) => {
            fs.iter().for_each(|(_, v)| visit_reads(safe, v, st))
        }
        _ => {}
    }
}

pub fn reads(value: &Value) -> Vec<String> {
    let mut st = Reads { seen: Seen::default(), names: Vec::new(), opaque: false };
    visit_reads(&[], value, &mut st);
    if st.opaque {
        vec!["*".to_owned()]
    } else {
        sorted_unique(st.names)
    }
}

// ---- owner / type names ----------------------------------------------------------------------

pub fn owner(name: &str) -> Result<Owner, Diagnostic> {
    match name {
        "point" => Ok(Owner::Point),
        "vertex" => Ok(Owner::Vertex),
        "primitive" => Ok(Owner::Primitive),
        "detail" => Ok(Owner::Detail),
        _ => err("E_ATTR_OWNER", "Attribute owner is point, vertex, primitive or detail."),
    }
}

fn owner_name(owner: Owner) -> &'static str {
    match owner {
        Owner::Point => "point",
        Owner::Vertex => "vertex",
        Owner::Primitive => "primitive",
        Owner::Detail => "detail",
    }
}

fn array_of(t: Ty) -> Ty {
    Ty::Array(Box::new(t))
}

pub fn value_type(name: &str) -> Result<Ty, Diagnostic> {
    match name {
        "float" => Ok(array_of(Ty::Float)),
        "int" => Ok(array_of(Ty::Int)),
        "vec2" => Ok(array_of(Ty::Vec2)),
        "vec3" => Ok(array_of(Ty::Vec3)),
        "vec4" => Ok(array_of(Ty::Vec4)),
        "text" => Ok(array_of(Ty::Text)),
        "float_array" => Ok(Ty::Record(vec![("offsets".into(), array_of(Ty::Int)), ("values".into(), array_of(Ty::Float))])),
        "int_array" => Ok(Ty::Record(vec![("offsets".into(), array_of(Ty::Int)), ("values".into(), array_of(Ty::Int))])),
        _ => err("E_ATTR_TYPE", "Attribute type is float, int, vec2, vec3, vec4, text, float_array or int_array."),
    }
}

fn owner_count(g: &geometry::T, owner: Owner) -> usize {
    (match owner {
        Owner::Point => geometry::point_count(g),
        Owner::Vertex => geometry::vertex_count(g),
        Owner::Primitive => geometry::primitive_count(g),
        Owner::Detail => 1,
    }) as usize
}

fn width(kind: &str) -> usize {
    match kind {
        "float" => 1,
        "vec2" => 2,
        "vec3" => 3,
        "vec4" => 4,
        _ => 0,
    }
}

fn options(args: &[(String, Value)]) -> Result<(Owner, String), Diagnostic> {
    let text = |key: &str, default: &str, code: &str| -> Result<String, Diagnostic> {
        match assoc(args, key) {
            None => Ok(default.to_owned()),
            Some(Value::Text(t)) => Ok(String::from_utf8_lossy(t).into_owned()),
            _ => err(code, &format!("Attribute {key} must be text.")),
        }
    };
    let owner = owner(&text("owner", "point", "E_ATTR_OWNER")?)?;
    let kind = text("type", "vec3", "E_ATTR_TYPE")?;
    value_type(&kind)?;
    Ok((owner, kind))
}

// ---- storage ---------------------------------------------------------------------------------

/// What `storage` found: a named attribute, or canonical positions.
enum Held {
    Attr(attribute::T),
    Positions(Storage),
}

fn held_of(owner: Owner, name: &str, g: &geometry::T) -> Option<Held> {
    if owner == Owner::Point && name == "P" {
        Some(Held::Positions(Storage::Float3(geometry::positions(g).clone())))
    } else {
        geometry::find_attribute(owner, name, g).map(Held::Attr)
    }
}
fn held_storage(held: &Held) -> Result<&Storage, Diagnostic> {
    match held {
        Held::Positions(s) => Ok(s),
        Held::Attr(a) => attribute::View::storage(a).map_err(|e| fail("E_ATTR_TYPE", &rerr(&e))),
    }
}

fn storage_kind(storage: &Storage) -> &'static str {
    match storage {
        Storage::Float(_) => "float",
        Storage::Int(_) => "int",
        Storage::Float2(_) => "vec2",
        Storage::Float3(_) => "vec3",
        Storage::Float4(_) => "vec4",
        Storage::Text(_) => "text",
        Storage::FloatArray(_) => "float_array",
        Storage::IntArray(_) => "int_array",
    }
}

/// The float planes of a storage, borrowed (OCaml `columns`).
fn columns(storage: &Storage) -> Result<Option<Vec<&[f64]>>, Diagnostic> {
    attribute::float_planes(storage).map_err(|e| fail("E_ATTR_TYPE", &rerr(&e)))
}

/// Shared planes of a storage (`None` for a plain float vector, which has no identity to key on).
fn shared_planes(storage: &Storage) -> Option<Vec<Arc<[f64]>>> {
    match storage {
        Storage::Float2(d) => {
            let v = Float2::View::view(d);
            Some(vec![v.x, v.y])
        }
        Storage::Float3(d) => Float3::View::view(d).ok().map(|v| vec![v.x, v.y, v.z]),
        Storage::Float4(d) => {
            let v = Float4::View::view(d);
            Some(vec![v.x, v.y, v.z, v.w])
        }
        _ => None,
    }
}

fn typed_storage(owner: Owner, name: &str, kind: &str, g: &geometry::T) -> Result<Held, Diagnostic> {
    if let Some(held) = held_of(owner, name, g) {
        if storage_kind(held_storage(&held)?) == kind {
            return Ok(held);
        }
    }
    err("E_ATTR_TYPE", &format!("{} attribute {} is missing or is not {}.", owner_name(owner), name, kind))
}

// ---- parallel pieces -------------------------------------------------------------------------

/// Run `piece(first, last)` for each grain-sized chunk of `0..count` on the shared pool; a piece is
/// `columns` vectors of `(last - first) * stride[c]` floats. The pieces are stitched in chunk order,
/// so the result is byte-identical to the sequential path.
fn ranges(grain: i64, count: usize, strides: &[usize], piece: &(dyn Fn(usize, usize) -> Vec<Vec<f64>> + Send + Sync)) -> Vec<Vec<f64>> {
    let grain = grain.max(1) as usize;
    let chunks = if count == 0 { 0 } else { (count - 1) / grain + 1 };
    let slots: Vec<Mutex<Option<Vec<Vec<f64>>>>> = (0..chunks).map(|_| Mutex::new(None)).collect();
    if chunks > 0 {
        parallel::for_(1, 0, chunks as i64 - 1, &|chunk| {
            let first = chunk as usize * grain;
            let last = count.min(first + grain);
            *slots[chunk as usize].lock().unwrap() = Some(piece(first, last));
            Ok(())
        })
        .expect("attribute_kernel ranges");
    }
    let mut out: Vec<Vec<f64>> = strides.iter().map(|s| Vec::with_capacity(count * s)).collect();
    for slot in slots {
        let piece = slot.into_inner().unwrap().expect("chunk ran");
        for (column, part) in out.iter_mut().zip(piece) {
            column.extend_from_slice(&part);
        }
    }
    out
}

/// A bounded ring keyed by the identity of a first plane (OCaml `Flow.Value.remember ()`): a hit
/// whose other planes differ is a miss that does not replace the entry.
struct Ring<X> {
    slots: Mutex<Vec<(Vec<Weak<[f64]>>, X)>>,
}
const RING: usize = 32;

fn same_planes(seen: &[Weak<[f64]>], planes: &[Arc<[f64]>]) -> bool {
    seen.len() == planes.len() && seen.iter().zip(planes).all(|(w, p)| w.upgrade().is_some_and(|a| Arc::ptr_eq(&a, p)))
}

impl<X: Clone> Ring<X> {
    const fn new() -> Ring<X> {
        Ring { slots: Mutex::new(Vec::new()) }
    }
    fn recall(&self, planes: &[Arc<[f64]>], build: &dyn Fn() -> X) -> X {
        {
            let slots = self.slots.lock().unwrap();
            if let Some((seen, value)) = slots.iter().find(|(seen, _)| seen[0].upgrade().is_some_and(|a| Arc::ptr_eq(&a, &planes[0]))) {
                return if same_planes(seen, planes) { value.clone() } else { drop(slots); build() };
            }
        }
        let value = build();
        let mut slots = self.slots.lock().unwrap();
        slots.retain(|(seen, _)| seen[0].strong_count() > 0);
        if slots.len() >= RING {
            slots.remove(0);
        }
        slots.push((planes.iter().map(Arc::downgrade).collect(), value.clone()));
        value
    }
}

static INTERLEAVED: Ring<Value> = Ring::new();
static CHECKED_VALUES: Ring<Result<Value, Diagnostic>> = Ring::new();
static CHECKED_VALID: Ring<Result<Value, Diagnostic>> = Ring::new();

// ---- reading attributes as Flow values --------------------------------------------------------

fn interleave(count: usize, planes: &[&[f64]], grain: i64) -> Value {
    let width = planes.len();
    let planes_owned: Vec<&[f64]> = planes.to_vec();
    let columns = ranges(grain, count, &[width], &move |first, last| {
        // One loop per width: no inner component loop for the common widths.
        let mut out = Vec::with_capacity((last - first) * width);
        match planes_owned.as_slice() {
            [x, y, z] => {
                for i in first..last {
                    out.push(x[i]);
                    out.push(y[i]);
                    out.push(z[i]);
                }
            }
            [x, y] => {
                for i in first..last {
                    out.push(x[i]);
                    out.push(y[i]);
                }
            }
            [x, y, z, w] => {
                for i in first..last {
                    out.push(x[i]);
                    out.push(y[i]);
                    out.push(z[i]);
                    out.push(w[i]);
                }
            }
            _ => {
                for i in first..last {
                    for plane in &planes_owned {
                        out.push(plane[i]);
                    }
                }
            }
        }
        vec![out]
    });
    let values = columns.into_iter().next().unwrap_or_default();
    wide_array(width, values)
}

fn wide_array(width: usize, values: Vec<f64>) -> Value {
    match width {
        1 => Value::Float_array(Arc::new(values)),
        2 => Value::Vec2_array(Arc::new(values)),
        3 => Value::Vec3_array(Arc::new(values)),
        _ => Value::Vec4_array(Arc::new(values)),
    }
}

fn read(component: Option<&str>, owner: Owner, kind: &str, name: &str, g: &geometry::T) -> Result<Value, Diagnostic> {
    let held = typed_storage(owner, name, kind, g)?;
    let storage = held_storage(&held)?;
    let Some(planes) = columns(storage)? else {
        return match (storage, component) {
            (Storage::Int(data), None) => Ok(Value::Int_array(Arc::new(data.clone()))),
            (Storage::Text(data), None) => Ok(Value::Text_array(Arc::new(data.iter().map(|s| s.as_bytes().to_vec()).collect()))),
            (Storage::FloatArray(data), Some("offsets")) => Ok(Value::Int_array(Arc::new(FloatArray::View::view(data).offsets.to_vec()))),
            (Storage::FloatArray(data), Some("values")) => Ok(float_array(FloatArray::View::view(data).values.to_vec())),
            (Storage::IntArray(data), Some("offsets")) => Ok(Value::Int_array(Arc::new(IntArray::View::view(data).offsets.to_vec()))),
            (Storage::IntArray(data), Some("values")) => Ok(Value::Int_array(Arc::new(IntArray::View::view(data).values.to_vec()))),
            _ => err("E_ATTR_TYPE", "This attribute storage requires its typed array foundation."),
        };
    };
    let count = owner_count(g, owner);
    let width = planes.len();
    if count > max_floats(width) {
        return err("E_ATTR_COUNT", "Attribute buffer exceeds array limits.");
    }
    if count * width > 4096 && planes.iter().all(|p| p.len() == count) {
        if let Some(arcs) = shared_planes(storage) {
            return Ok(INTERLEAVED.recall(&arcs, &|| interleave(count, &planes, 16_384)));
        }
    }
    Ok(interleave(count, &planes, 16_384))
}

fn read_checked(component: &str, owner: Owner, kind: &str, name: &str, g: &geometry::T) -> Result<Value, Diagnostic> {
    let held = typed_storage(owner, name, kind, g)?;
    let storage = held_storage(&held)?;
    let Some(planes) = columns(storage)? else {
        return err("E_ATTR_TYPE", "Checked attributes require float, vec2, vec3 or vec4 storage.");
    };
    let count = owner_count(g, owner);
    let width = planes.len();
    if component != "values" && component != "valid" {
        return err("E_ATTR_TYPE", "Checked attribute component is values or valid.");
    }
    if count > max_floats(width) {
        return err("E_ATTR_COUNT", "Attribute buffer exceeds array limits.");
    }
    let valid_only = component == "valid";
    let build = || -> Result<Value, Diagnostic> {
        let stride = if valid_only { 1 } else { width };
        let columns = ranges(16_384, count, &[stride], &|first, last| {
            let mut out = Vec::with_capacity((last - first) * stride);
            for i in first..last {
                let finite = planes.iter().all(|p| p[i].is_finite());
                if valid_only {
                    out.push(if finite { 1.0 } else { 0.0 });
                } else {
                    for p in &planes {
                        out.push(if finite { p[i] } else { 0.0 });
                    }
                }
            }
            vec![out]
        });
        let data = columns.into_iter().next().unwrap_or_default();
        Ok(if valid_only { Value::Int_array(Arc::new(data.into_iter().map(|x| x as i64).collect())) } else { wide_array(width, data) })
    };
    if count * width <= 4096 {
        return build();
    }
    match shared_planes(storage) {
        Some(arcs) => (if valid_only { &CHECKED_VALID } else { &CHECKED_VALUES }).recall(&arcs, &build),
        None => build(),
    }
}

fn read_planes(component: &str, owner: Owner, name: &str, g: &geometry::T) -> Result<Value, Diagnostic> {
    let Some(held) = held_of(owner, name, g) else {
        return err("E_ATTR_TYPE", &format!("{} attribute {} is missing.", owner_name(owner), name));
    };
    let Some(planes) = columns(held_storage(&held)?)? else {
        return err("E_ATTR_TYPE", &format!("{} attribute {} is not float, vec2, vec3 or vec4.", owner_name(owner), name));
    };
    if component == "width" {
        return Ok(Value::Int(planes.len() as i64));
    }
    let index = if component.len() == 1 { "xyzw".find(component) } else { None };
    match index {
        None => err("E_ATTR_TYPE", "Plane component is width, x, y, z or w."),
        Some(c) => Ok(float_array(if c < planes.len() { planes[c].to_vec() } else { vec![0.0; owner_count(g, owner)] })),
    }
}

/// `(operation, count, value)` of a constant-array descriptor source.
fn constant_source(value: &Value) -> Option<Result<(String, i64, Value), Diagnostic>> {
    let Value::Source(name, ty, args) = value else { return None };
    let operation = name.strip_prefix("$core/constant-array/")?;
    Some(match op::constant_array_descriptor(operation, args) {
        Ok(Value::Source(_, actual, fields)) if actual == *ty => match (assoc(&fields, "count"), assoc(&fields, "value")) {
            (Some(Value::Int(count)), Some(value)) => Ok((operation.to_owned(), *count, value.clone())),
            _ => err("E_ARRAY_TYPE", "Malformed constant array descriptor."),
        },
        Ok(_) => err("E_ARRAY_TYPE", "Constant array descriptor has the wrong declared type."),
        Err(d) => Err(d),
    })
}

// ---- cook context ------------------------------------------------------------------------------

/// `Operators.seed_for`: the salt is the low 63 bits of a 64-bit word, xor-ed into the identity
/// before mixing.
pub fn seed_for(salt: i64, seed: i64, namespace: &str, label: &str) -> i64 {
    let identity = rays_math::hash::string(&format!("{namespace}:{label}"));
    let salt = salt & i64::MAX;
    let mixed = seed ^ (identity ^ salt).wrapping_mul(0x9e3779b97f4a7c15_u64 as i64);
    ((mixed ^ (((mixed as u64) >> 32) as i64)) << 1) >> 1
}

pub fn context_label() -> &'static str {
    E::context_label()
}

/// The cook context an inlined body reads: a kernel node declares it ([dependencies]) and
/// resolves it from its cook context, as the nested card specialized it.
fn context_value(context: &context::T, value: &Value) -> Option<Value> {
    let Value::Source(name, _, args) = value else { return None };
    match name.as_str() {
        "sop/context_seed" => {
            let text = |key: &str| assoc(args, key).and_then(text_of).unwrap_or_default();
            let salt = match assoc(args, "salt") {
                Some(Value::Int(n)) => *n,
                _ => 0,
            };
            Some(Value::Int(seed_for(salt, context::seed(context), &text("namespace"), &text(context_label()))))
        }
        "sop/context_frame" => Some(Value::Float(context::frame(context) as f64)),
        "sop/context_time" => Some(Value::Float(context::time(context))),
        _ => None,
    }
}

fn context_fact(name: &str) -> Option<D::Fact> {
    match name {
        "sop/context_seed" => Some(D::Fact::Seed),
        "sop/context_frame" => Some(D::Fact::Frame),
        "sop/context_time" => Some(D::Fact::Time),
        _ => None,
    }
}

/// The seed, frame and time alone.
pub fn context_reads(value: &Value) -> D::T {
    fn visit(value: &Value, seen: &mut Seen, found: &mut D::T) {
        match value {
            Value::Source(name, _, fs) => {
                if let Some(fact) = context_fact(name) {
                    *found = D::union(*found, D::one(fact));
                }
                fs.iter().for_each(|(_, v)| visit(v, seen, found));
            }
            Value::Residual(r) => {
                if seen.residuals.insert(r.0) {
                    if let Ok(view) = E::Residual::residual_view(*r) {
                        view.bindings.iter().for_each(|(_, v)| visit(v, seen, found));
                    }
                }
            }
            Value::List(vs) => vs.iter().for_each(|v| visit(v, seen, found)),
            Value::Fn(f) => {
                if seen.functions.insert(f.0) {
                    E::Residual::function_bindings(*f).iter().for_each(|(_, v)| visit(v, seen, found));
                }
            }
            Value::Record(fs) | Value::Struct(_, _, fs) => fs.iter().for_each(|(_, v)| visit(v, seen, found)),
            _ => {}
        }
    }
    let mut found = D::STATIC;
    visit(value, &mut Seen::default(), &mut found);
    found
}

/// The host-input slices a frame-dependent value reads: `t` is the clock, a fold steps with the
/// frame, and each live operator names its own slice.
pub fn input_reads(value: &Value) -> D::T {
    E::frame_reads(value).iter().fold(D::STATIC, |found, name| {
        let fact = match name.as_str() {
            "time" => D::Fact::Time,
            "state" | "frame/dt" | "frame/index" => D::Fact::Step,
            "frame/width" | "frame/height" => D::Fact::Size,
            "key/down" => D::Fact::Keys,
            n if n.starts_with("pointer/") => D::Fact::Pointer,
            _ => D::Fact::Input,
        };
        D::union(found, D::one(fact))
    })
}

pub fn dependencies(value: &Value) -> D::T {
    D::union(context_reads(value), input_reads(value))
}

struct Specializer<'a> {
    context: &'a context::T,
    residuals: std::cell::RefCell<std::collections::HashMap<i64, (Value, bool)>>,
    functions: std::cell::RefCell<std::collections::HashMap<i64, (Value, bool)>>,
}

impl Specializer<'_> {
    /// The mapped value and whether it differs from the input.
    fn map(&self, value: &Value) -> (Value, bool) {
        if let Some(v) = context_value(self.context, value) {
            return (v, true);
        }
        match value {
            Value::Residual(r) => {
                if let Some(done) = self.residuals.borrow().get(&r.0) {
                    return done.clone();
                }
                let done = match E::Residual::residual_view(*r) {
                    Err(_) => (value.clone(), false),
                    Ok(view) => {
                        let mapped: Vec<(String, (Value, bool))> = view.bindings.iter().map(|(n, v)| (n.clone(), self.map(v))).collect();
                        if mapped.iter().all(|(_, (_, changed))| !changed) {
                            (value.clone(), false)
                        } else {
                            let bindings: Vec<(String, Value)> = mapped.into_iter().map(|(n, (v, _))| (n, v)).collect();
                            match E::Residual::with_bindings(*r, &view.term, &bindings) {
                                Ok(id) => (Value::Residual(id), true),
                                Err(_) => (value.clone(), false),
                            }
                        }
                    }
                };
                self.residuals.borrow_mut().insert(r.0, done.clone());
                done
            }
            Value::Fn(f) => {
                if let Some(done) = self.functions.borrow().get(&f.0) {
                    return done.clone();
                }
                let changed = std::cell::Cell::new(false);
                let mapped = E::Host::map_function_bindings(*f, &|v| {
                    let (m, c) = self.map(v);
                    if c {
                        changed.set(true);
                    }
                    m
                });
                let done = (Value::Fn(mapped), changed.get());
                self.functions.borrow_mut().insert(f.0, done.clone());
                done
            }
            Value::List(vs) => {
                let m: Vec<(Value, bool)> = vs.iter().map(|v| self.map(v)).collect();
                let changed = m.iter().any(|(_, c)| *c);
                (Value::List(m.into_iter().map(|(v, _)| v).collect()), changed)
            }
            Value::Record(fs) => {
                let (fs, c) = self.fields(fs);
                (Value::Record(fs), c)
            }
            Value::Struct(n, t, fs) => {
                let (fs, c) = self.fields(fs);
                (Value::Struct(n.clone(), t.clone(), fs), c)
            }
            Value::Source(n, t, fs) => {
                let (fs, c) = self.fields(fs);
                (Value::Source(n.clone(), t.clone(), fs), c)
            }
            other => (other.clone(), false),
        }
    }
    fn fields(&self, fs: &[(String, Value)]) -> (Vec<(String, Value)>, bool) {
        let mut changed = false;
        let out = fs
            .iter()
            .map(|(n, v)| {
                let (m, c) = self.map(v);
                changed |= c;
                (n.clone(), m)
            })
            .collect();
        (out, changed)
    }
}

/// The data with its context sources replaced by this cook context's values: a kernel over it
/// compiles them as constants (a noise seed is a region immediate).
pub fn specialize(context: &context::T, value: &Value) -> Value {
    let s = Specializer { context, residuals: Default::default(), functions: Default::default() };
    s.map(value).0
}

// ---- resolve -----------------------------------------------------------------------------------

fn not_cooked<X>() -> Result<X, Diagnostic> {
    err("E_DATA_SOURCE", "Attribute source has not been cooked.")
}

fn component_sources(args: &[(String, Value)], what: &str) -> Result<(i64, String, String), Diagnostic> {
    match (assoc(args, "geometry").and_then(geometry_id), assoc(args, "attribute").and_then(text_of), assoc(args, "component").and_then(text_of)) {
        (Some(id), Some(name), Some(component)) => Ok((id, name, component)),
        _ => err("E_ATTR_TYPE", &format!("{what} needs geometry, an attribute name and a component.")),
    }
}

/// Resolve immutable cooked attributes and owner/type predicates, shared by kernels and reference
/// probes. CSR reads expose typed offsets and values.
pub fn resolve(context: Option<&context::T>, geometry: &GeometryLookup, value: &Value) -> Result<Value, Diagnostic> {
    if let Some(v) = context.and_then(|c| context_value(c, value)) {
        return Ok(v);
    }
    match constant_source(value) {
        Some(result) => {
            let (operation, count, value) = result?;
            let found = op::find(&[], &operation, &fctx::value()).ok_or_else(|| fail("E_ARRAY_TYPE", "Unknown constant array constructor."))?;
            let args = vec![("count".to_owned(), Value::Int(count)), ("value".to_owned(), value)];
            (found.body)(&input::at_time(0.0), &|_, _| unreachable!("constant array bodies run no nodes"), &args)
        }
        None => match value {
            Value::Source(name, _, args) if name == "sop/attr_checked" => {
                let (id, attribute, component) = component_sources(args, "sop/attr_checked")?;
                let g = geometry(id).map_or_else(not_cooked, Ok)?;
                let (owner, kind) = options(args)?;
                read_checked(&component, owner, &kind, &attribute, &g)
            }
            Value::Source(name, _, args) if name == "sop/attr_planes" => {
                let (id, attribute, component) = component_sources(args, "sop/attr_planes")?;
                let g = geometry(id).map_or_else(not_cooked, Ok)?;
                let (owner, _) = options(args)?;
                read_planes(&component, owner, &attribute, &g)
            }
            Value::Struct(name, _, args) | Value::Source(name, _, args) if name == "sop/attr" => {
                match (assoc(args, "geometry").and_then(geometry_id), assoc(args, "attribute").and_then(text_of)) {
                    (Some(id), Some(attribute)) => match geometry(id) {
                        Some(g) => {
                            let (owner, kind) = options(args)?;
                            let component = assoc(args, "component").and_then(text_of);
                            read(component.as_deref(), owner, &kind, &attribute, &g)
                        }
                        None => not_cooked(),
                    },
                    _ => err("E_ATTR_TYPE", "sop/attr needs geometry and an attribute name."),
                }
            }
            Value::Source(name, _, args) if name == "sop/has_attr" => {
                match (assoc(args, "geometry").and_then(geometry_id), assoc(args, "attribute").and_then(text_of)) {
                    (Some(id), Some(attribute)) => {
                        let g = geometry(id).map_or_else(not_cooked, Ok)?;
                        let (owner, _) = options(args)?;
                        let requested = assoc(args, "type").and_then(text_of);
                        Ok(Value::Bool(match attribute_metadata::kind(owner, &attribute, &g) {
                            None => false,
                            Some(kind) => requested.is_none_or(|r| r == kind),
                        }))
                    }
                    _ => err("E_ATTR_TYPE", "sop/has_attr needs geometry and an attribute name."),
                }
            }
            Value::Source(name, ty, args) if name == "sop/components" => {
                let args = concrete_args(context, geometry, args)?;
                topology_kernel_impl::resolve(&**geometry, &Value::Source(name.clone(), ty.clone(), args))
            }
            Value::Source(name, _, _) if ["sop/count", "sop/edges", "sop/topology", "sop/adjacency", "sop/triangles", "sop/fans"].contains(&name.as_str()) => {
                topology_kernel_impl::resolve(&**geometry, value)
            }
            Value::Source(name, _, _) if ["sop/attr_type", "sop/attr_names", "sop/attr_entries"].contains(&name.as_str()) => {
                attribute_metadata::resolve(&**geometry, value)
            }
            Value::Source(name, ty, args) if ["graph/shortest_paths", "graph/forest", "graph/color", "graph/chains"].contains(&name.as_str()) => {
                let args = concrete_args(context, geometry, args)?;
                graph_kernel::resolve(&Value::Source(name.clone(), ty.clone(), args))
            }
            Value::Source(name, ty, args) if name == "$core/cooked" && ty::is_geometry(ty) && args.len() == 1 && args[0].0 == "id" => {
                // a geometry a lowered program passes to a host source (Core_lower)
                match &args[0].1 {
                    Value::Int(id) if geometry(*id).is_some() => Ok(Value::Deferred(ty.clone(), *id)),
                    Value::Int(_) => not_cooked(),
                    _ => group_kernel::resolve(&**geometry, value),
                }
            }
            source => group_kernel::resolve(&**geometry, source),
        },
    }
}

/// Array arguments of a pure source may still be host sources (a sop/adjacency record): resolve
/// them so the algorithm sees concrete arrays.
fn concrete_args(context: Option<&context::T>, geometry: &GeometryLookup, args: &[(String, Value)]) -> Result<Vec<(String, Value)>, Diagnostic> {
    fn concrete(context: Option<&context::T>, geometry: &GeometryLookup, value: &Value) -> Result<Value, Diagnostic> {
        match value {
            Value::Residual(_) => {
                let live = context.map_or_else(|| input::at_time(0.0), |c| context::input(c).clone());
                let program = Executor::compile(value, &Executor::CompileOptions { strict: true, ..Default::default() })?;
                let (ctx, geo) = (context.cloned(), geometry.clone());
                let resolver: flow_ir::core_exec::ResolveFn = Arc::new(move |v| resolve(ctx.as_ref(), &geo, v));
                let forced = Executor::force(&program, &Executor::ForceOptions { strict: true, resolve: Some(resolver), ..Default::default() }, &live)?;
                concrete(context, geometry, &forced)
            }
            Value::Source(..) => concrete(context, geometry, &resolve(context, geometry, value)?),
            Value::Struct(name, _, _) if name == "sop/attr" => concrete(context, geometry, &resolve(context, geometry, value)?),
            Value::Record(fields) => Ok(Value::Record(
                fields.iter().map(|(n, v)| Ok((n.clone(), concrete(context, geometry, v)?))).collect::<Result<Vec<_>, Diagnostic>>()?,
            )),
            other => Ok(other.clone()),
        }
    }
    args.iter().map(|(n, v)| Ok((n.clone(), concrete(context, geometry, v)?))).collect()
}

// ---- writing attributes ------------------------------------------------------------------------

/// Plane data to install: freshly computed vectors, or shared copies whose identity a recook keeps.
enum Planes {
    Owned(Vec<Vec<f64>>),
    Shared(Vec<Arc<[f64]>>),
}

fn make_storage(kind: &str, planes: Planes) -> Result<Storage, String> {
    let owned = |planes: Planes| -> Vec<Vec<f64>> {
        match planes {
            Planes::Owned(v) => v,
            Planes::Shared(v) => v.iter().map(|p| p.to_vec()).collect(),
        }
    };
    match kind {
        "float" => Ok(Storage::Float(owned(planes).swap_remove(0))),
        "vec2" => {
            let mut p = owned(planes).into_iter();
            let (x, y) = (p.next().unwrap(), p.next().unwrap());
            Float2::of_owned(x, y).map(Storage::Float2).map_err(|e| rerr(&e))
        }
        "vec3" => match planes {
            Planes::Shared(p) => Float3::View::of_shared_exn(p[0].clone(), p[1].clone(), p[2].clone()).map(Storage::Float3).map_err(|e| rerr(&e)),
            Planes::Owned(p) => {
                let mut p = p.into_iter();
                let (x, y, z) = (p.next().unwrap(), p.next().unwrap(), p.next().unwrap());
                Float3::of_owned(x, y, z).map(Storage::Float3).map_err(|e| rerr(&e))
            }
        },
        "vec4" => {
            let mut p = owned(planes).into_iter();
            let (x, y, z, w) = (p.next().unwrap(), p.next().unwrap(), p.next().unwrap(), p.next().unwrap());
            Float4::of_owned(x, y, z, w).map(Storage::Float4).map_err(|e| rerr(&e))
        }
        _ => Err("Unsupported floating attribute type".to_owned()),
    }
}

fn attr_err(message: String) -> Diagnostic {
    fail("E_ATTR_TYPE", &message)
}

/// `G.with_positions`/`with_attribute` of an already made storage.
fn place(owner: Owner, name: &str, storage: Storage, g: &geometry::T) -> Result<geometry::T, String> {
    if owner == Owner::Point && name == "P" {
        match storage {
            Storage::Float3(positions) => geometry::with_positions(positions, g).map_err(|e| rerr(&e)),
            _ => Err("Canonical point P requires vec3 storage.".to_owned()),
        }
    } else {
        let attr = attribute::create_owned(name, owner, storage).map_err(|e| rerr(&e))?;
        geometry::with_attribute(attr, g).map_err(|e| rerr(&e))
    }
}

fn install_planes(owner: Owner, kind: &str, name: &str, planes: Planes, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    make_storage(kind, planes).and_then(|s| place(owner, name, s, g)).map_err(attr_err)
}

fn install(owner: Owner, kind: &str, name: &str, planes: Vec<Vec<f64>>, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    install_planes(owner, kind, name, Planes::Owned(planes), g)
}

static POINT_HOLDERS: Mutex<Vec<(usize, Weak<topology_kernel_impl::ResidentPoints>)>> = Mutex::new(Vec::new());

fn holder_address(holder: &Arc<dyn rdk::packed::Deferred::Holder>) -> usize {
    Arc::as_ptr(holder) as *const () as usize
}

/// OCaml `Resident (v, count)` read back from a holder: the ones this module made, then the mesh's.
fn resident_of(holder: &Arc<dyn rdk::packed::Deferred::Holder>) -> Option<(Value, i64)> {
    let at = holder_address(holder);
    let ours = POINT_HOLDERS.lock().unwrap().iter().find(|(key, _)| *key == at).and_then(|(_, w)| w.upgrade());
    ours.or_else(|| topology_kernel_impl::resident_points_of(holder)).map(|r| (r.value.clone(), r.count))
}

fn new_holder(value: Value, count: i64) -> Arc<dyn rdk::packed::Deferred::Holder> {
    let holder = Arc::new(topology_kernel_impl::ResidentPoints { value, count });
    let mut all = POINT_HOLDERS.lock().unwrap();
    all.retain(|(_, w)| w.strong_count() > 0);
    all.push((Arc::as_ptr(&holder) as *const () as usize, Arc::downgrade(&holder)));
    holder
}

/// A device output as an attribute's storage, read from the device only when a CPU reader first
/// needs it (any domain); until then the geometry holds the device's buffer.
pub fn install_deferred(owner: Owner, kind: &str, name: &str, planes: &Arc<Gpu::Planes>, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    let holder = planes.resident.clone().map(|v| new_holder(v, planes.count));
    let release: rdk::packed::Deferred::Release = {
        let p = planes.clone();
        Arc::new(move |held| (p.release)(held))
    };
    let materialise = {
        let p = planes.clone();
        move || (p.materialise)()
    };
    let like = make_storage(kind, Planes::Owned(vec![Vec::new(); width(kind).max(1)])).map_err(attr_err)?;
    if owner == Owner::Point && name == "P" {
        match like {
            Storage::Float3(_) => {
                let bounds: Float3::Bounds = {
                    let p = planes.clone();
                    Arc::new(move || (p.bounds)())
                };
                let positions = Float3::deferred(holder, Some(release), Some(bounds), planes.count, Arc::new(move || Ok(materialise())))
                    .map_err(|e| attr_err(rerr(&e)))?;
                geometry::with_positions(positions, g).map_err(|e| attr_err(rerr(&e)))
            }
            _ => err("E_ATTR_TYPE", "Canonical point P requires vec3 storage."),
        }
    } else {
        let kind = kind.to_owned();
        let attr = attribute::create_deferred(
            holder,
            Some(release),
            name,
            owner,
            &like,
            planes.count,
            Arc::new(move || make_storage(&kind, Planes::Owned(materialise())).expect("deferred storage")),
        )
        .map_err(|e| attr_err(rerr(&e)))?;
        geometry::with_attribute(attr, g).map_err(|e| attr_err(rerr(&e)))
    }
}

/// A whole-attribute read (`sop/attr` without a component) of storage a device still holds: its
/// placeholder and count, which the device binds to the held buffer without reading it back.
fn resident_read(geometry: &GeometryLookup, value: &Value) -> Option<(Value, i64)> {
    let (Value::Struct(name, _, args) | Value::Source(name, _, args)) = value else { return None };
    if name != "sop/attr" || assoc(args, "component").is_some() {
        return None;
    }
    let id = assoc(args, "geometry").and_then(geometry_id)?;
    let attribute_name = assoc(args, "attribute").and_then(text_of)?;
    let g = geometry(id)?;
    let (owner, kind) = options(args).ok()?;
    let holder = if owner == Owner::Point && attribute_name == "P" {
        Float3::holder(geometry::positions(&g))
    } else {
        geometry::find_attribute(owner, &attribute_name, &g).and_then(|a| attribute::holder(&a))
    }?;
    let (v, count) = resident_of(&holder)?;
    let ty = value_type(&kind).ok()?;
    (V::ty_of(&v) == ty && count == owner_count(&g, owner) as i64).then_some((v, count))
}

/// The placeholder of point P or a point vec3 attribute `name` that a device still holds unread,
/// or `None`: asked without materialising it.
pub fn resident_vec3(g: &geometry::T, name: &str) -> Option<Vec<f64>> {
    let holder = if name == "P" {
        Float3::holder(geometry::positions(g))
    } else {
        geometry::find_attribute(Owner::Point, name, g).and_then(|a| attribute::holder(&a))
    }?;
    match resident_of(&holder)? {
        (Value::Vec3_array(placeholder), count) if count == geometry::point_count(g) => Some(placeholder.to_vec()),
        _ => None,
    }
}

/// Unread point Cd placeholder and its component width (3 or 4), without materialisation.
pub fn resident_color(g: &geometry::T) -> Option<(Vec<f64>, usize)> {
    let holder = geometry::find_attribute(Owner::Point, "Cd", g).and_then(|a| attribute::holder(&a))?;
    match resident_of(&holder)? {
        (Value::Vec3_array(p), count) if count == geometry::point_count(g) => Some((p.to_vec(), 3)),
        (Value::Vec4_array(p), count) if count == geometry::point_count(g) => Some((p.to_vec(), 4)),
        _ => None,
    }
}

fn descriptor(value: &Value) -> Option<(Ty, &Vec<(String, Value)>)> {
    match value {
        Value::Struct(name, ty, args) | Value::Source(name, ty, args) if name == "sop/attr" => Some((ty.clone(), args)),
        _ => None,
    }
}

fn borrowed_attr(geometry: GeometryFn, args: &[(String, Value)]) -> Result<(Storage, usize), Diagnostic> {
    match (assoc(args, "geometry").and_then(geometry_id), assoc(args, "attribute").and_then(text_of)) {
        (Some(id), Some(name)) => {
            let g = geometry(id).map_or_else(not_cooked, Ok)?;
            let (owner, kind) = options(args)?;
            let held = typed_storage(owner, &name, &kind, &g)?;
            Ok((held_storage(&held)?.clone(), owner_count(&g, owner)))
        }
        _ => err("E_ATTR_TYPE", "sop/attr needs geometry and an attribute name."),
    }
}

fn typed_array_foundation<X>() -> Result<X, Diagnostic> {
    err("E_ATTR_TYPE", "This attribute storage requires its typed array foundation.")
}

fn borrowed_storage(geometry: GeometryFn, values: &Value) -> Result<Option<(Storage, usize)>, Diagnostic> {
    if let Some((ty, args)) = descriptor(values) {
        let (storage, count) = borrowed_attr(geometry, args)?;
        let component = assoc(args, "component");
        let (storage, count) = match (&storage, component) {
            (Storage::FloatArray(_) | Storage::IntArray(_), None) => return typed_array_foundation(),
            (_, None) => (storage, count),
            (Storage::FloatArray(data), Some(Value::Text(c))) if c == b"offsets" => {
                let data = FloatArray::View::view(data).offsets.to_vec();
                let n = data.len();
                (Storage::Int(data), n)
            }
            (Storage::FloatArray(data), Some(Value::Text(c))) if c == b"values" => {
                let data = FloatArray::View::view(data).values.to_vec();
                let n = data.len();
                (Storage::Float(data), n)
            }
            (Storage::IntArray(data), Some(Value::Text(c))) if c == b"offsets" => {
                let data = IntArray::View::view(data).offsets.to_vec();
                let n = data.len();
                (Storage::Int(data), n)
            }
            (Storage::IntArray(data), Some(Value::Text(c))) if c == b"values" => {
                let data = IntArray::View::view(data).values.to_vec();
                let n = data.len();
                (Storage::Int(data), n)
            }
            _ => return typed_array_foundation(),
        };
        let actual = value_type(storage_kind(&storage))?;
        if ty != Ty::Any && ty != actual {
            return err("E_DATA_SOURCE", "The host returned a data source of the wrong type.");
        }
        return Ok(Some((storage, count)));
    }
    let Value::Record(fields) = values else { return Ok(None) };
    let (Some((offset_ty, offsets)), Some((value_ty, vals))) =
        (assoc(fields, "offsets").and_then(descriptor), assoc(fields, "values").and_then(descriptor))
    else {
        return Ok(None);
    };
    let is = |args: &Vec<(String, Value)>, c: &[u8]| matches!(assoc(args, "component"), Some(Value::Text(t)) if t == c);
    let without = |args: &Vec<(String, Value)>| args.iter().filter(|(k, _)| k != "component").cloned().collect::<Vec<_>>();
    if !(is(offsets, b"offsets") && is(vals, b"values") && without(offsets) == without(vals)) {
        return Ok(None);
    }
    let (storage, count) = borrowed_attr(geometry, &without(offsets))?;
    let actual = match &storage {
        Storage::FloatArray(_) => Some(array_of(Ty::Float)),
        Storage::IntArray(_) => Some(array_of(Ty::Int)),
        _ => None,
    };
    match actual {
        None => typed_array_foundation(),
        Some(actual) if (offset_ty == Ty::Any || offset_ty == array_of(Ty::Int)) && (value_ty == Ty::Any || value_ty == actual) => Ok(Some((storage, count))),
        Some(_) => err("E_DATA_SOURCE", "The host returned a data source of the wrong type."),
    }
}

/// Share an unchanged typed attribute query's immutable storage after checking owner/type/cardinality.
/// `None` retains the ordinary computed-array path.
pub fn write_source(geometry: GeometryFn, owner: Owner, kind: &str, name: &str, values: &Value, target: &geometry::T) -> Result<Option<geometry::T>, Diagnostic> {
    let Some((storage, count)) = borrowed_storage(geometry, values)? else { return Ok(None) };
    if storage_kind(&storage) != kind {
        return err("E_ATTR_TYPE", &format!("sop/with_attr requires {kind} storage."));
    }
    let wanted = owner_count(target, owner);
    if count != wanted {
        return err(
            "E_ATTR_COUNT",
            &match kind {
                "int" => "Integer attribute count differs from owner count.".to_owned(),
                "text" => "Text attribute count differs from owner count.".to_owned(),
                "float_array" | "int_array" => "CSR row count differs from owner count.".to_owned(),
                _ => format!("{name} needs {wanted} {} values; got {count}.", owner_name(owner)),
            },
        );
    }
    // This transfers immutable RDK storage. Native storage transfers also retain nonfinite planes;
    // computed numeric arrays remain checked.
    place(owner, name, storage, target).map(Some).map_err(attr_err)
}

fn all_finite(data: &[f64]) -> bool {
    data.iter().all(|x| x.is_finite())
}

fn semantic_owned(owner: Owner, name: &str, storage: Storage, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    place(owner, name, storage, g).map_err(attr_err)
}

/// Write concrete typed arrays into fresh owned storage, checking owner count, storage layout and
/// finite numeric values. `owned_fields`: CSR fields of `values` already fresh (not copied again).
pub fn write_storage(owned_fields: &[&str], grain: i64, owner: Owner, kind: &str, name: &str, values: &Value, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    let count = owner_count(g, owner);
    match (kind, values) {
        ("int", Value::Int_array(data)) => {
            if data.len() != count {
                return err("E_ATTR_COUNT", "Integer attribute count differs from owner count.");
            }
            semantic_owned(owner, name, Storage::Int(data.to_vec()), g)
        }
        ("text", Value::Text_array(data)) => {
            if data.len() != count {
                return err("E_ATTR_COUNT", "Text attribute count differs from owner count.");
            }
            semantic_owned(owner, name, Storage::Text(data.iter().map(|t| String::from_utf8_lossy(t).into_owned()).collect()), g)
        }
        ("vec2" | "vec3" | "vec4", Value::Record(fields)) => {
            let names: &[&str] = match kind {
                "vec2" => &["x", "y"],
                "vec3" => &["x", "y", "z"],
                _ => &["x", "y", "z", "w"],
            };
            let mut have: Vec<&str> = fields.iter().map(|(k, _)| k.as_str()).collect();
            have.sort();
            let mut want = names.to_vec();
            want.sort();
            if have != want || !fields.iter().all(|(_, v)| matches!(v, Value::Float_array(_))) {
                return err("E_ATTR_TYPE", &format!("sop/with_attr requires {kind} storage."));
            }
            let planes: Vec<&Arc<Vec<f64>>> = names
                .iter()
                .filter_map(|n| match assoc(fields, n) {
                    Some(Value::Float_array(d)) => Some(d),
                    _ => None,
                })
                .collect();
            if let Some(bad) = planes.iter().find(|d| d.len() != count) {
                return err("E_ATTR_COUNT", &format!("{name} needs {count} {} values; got {}.", owner_name(owner), bad.len()));
            }
            // a recook writing the same planes reuses their copies and their check; the Flow
            // arrays are not Arc<[f64]>, so the copy itself is the cached unit here
            match owned_copies_of_vectors(&planes) {
                None => err("E_NONFINITE", "Attribute values must be finite."),
                Some(copies) => install_planes(owner, kind, name, Planes::Shared(copies), g),
            }
        }
        ("float_array" | "int_array", Value::Record(fields)) => match (assoc(fields, "offsets"), assoc(fields, "values")) {
            (Some(Value::Int_array(offsets)), Some(values)) => {
                if offsets.len() != count + 1 {
                    return err("E_ATTR_COUNT", "CSR row count differs from owner count.");
                }
                let offsets = offsets.to_vec();
                let values_owned = owned_fields.contains(&"values");
                match (kind, values) {
                    ("float_array", Value::Float_array(data)) => {
                        if !values_owned && !all_finite(data) {
                            return err("E_NONFINITE", "Attribute values must be finite.");
                        }
                        let csr = FloatArray::create_owned(offsets, data.to_vec()).map_err(|e| fail("E_ATTR_COUNT", &rerr(&e)))?;
                        semantic_owned(owner, name, Storage::FloatArray(csr), g)
                    }
                    ("int_array", Value::Int_array(data)) => {
                        let csr = IntArray::create_owned(offsets, data.to_vec()).map_err(|e| fail("E_ATTR_COUNT", &rerr(&e)))?;
                        semantic_owned(owner, name, Storage::IntArray(csr), g)
                    }
                    _ => err("E_ATTR_TYPE", "CSR values do not match the requested storage type."),
                }
            }
            _ => err("E_ATTR_TYPE", "CSR attributes require integer offsets and typed values."),
        },
        _ => {
            let data = match (kind, values) {
                ("float", Value::Float_array(d)) | ("vec2", Value::Vec2_array(d)) | ("vec3", Value::Vec3_array(d)) | ("vec4", Value::Vec4_array(d)) => Some(d),
                _ => None,
            };
            let Some(values) = data else { return err("E_ATTR_TYPE", &format!("sop/with_attr requires {kind} storage.")) };
            let w = width(kind);
            if count > max_floats(w) || values.len() / w != count || values.len() % w != 0 {
                return err("E_ATTR_COUNT", &format!("{name} needs {count} {} values; got {}.", owner_name(owner), values.len() / w));
            }
            if count == 1 {
                if !values[..w].iter().all(|x| x.is_finite()) {
                    return err("E_NONFINITE", "Attribute values must be finite.");
                }
                return install(owner, kind, name, (0..w).map(|c| vec![values[c]]).collect(), g);
            }
            let finite = AtomicBool::new(true);
            let planes = ranges(grain, count, &vec![1; w], &|first, last| {
                let mut cols: Vec<Vec<f64>> = (0..w).map(|_| Vec::with_capacity(last - first)).collect();
                for i in first..last {
                    for (c, col) in cols.iter_mut().enumerate() {
                        let v = values[i * w + c];
                        if !v.is_finite() {
                            finite.store(false, Ordering::Relaxed);
                        }
                        col.push(v);
                    }
                }
                cols
            });
            if !finite.load(Ordering::Relaxed) {
                return err("E_NONFINITE", "Attribute values must be finite.");
            }
            install(owner, kind, name, planes, g)
        }
    }
}

/// `owned_copies` for Flow's `Arc<Vec<f64>>` planes: the cache is keyed by the identity of the
/// copies' first plane, so a recook of the same Flow arrays copies and checks them once. The key is
/// the Flow array itself (its address, held weakly by `FLOW_COPIES`).
fn owned_copies_of_vectors(planes: &[&Arc<Vec<f64>>]) -> Option<Vec<Arc<[f64]>>> {
    let copy = || -> Vec<Arc<[f64]>> { planes.iter().map(|p| Arc::from(p.as_slice())).collect() };
    if planes.is_empty() || planes[0].len() <= 4096 {
        return if planes.iter().all(|p| all_finite(p)) { Some(copy()) } else { None };
    }
    let mut cache = FLOW_COPIES.lock().unwrap();
    cache.retain(|(keys, _)| keys[0].strong_count() > 0);
    if let Some((_, copies)) = cache.iter().find(|(keys, _)| {
        keys.len() == planes.len() && keys.iter().zip(planes).all(|(w, p)| w.upgrade().is_some_and(|a| Arc::ptr_eq(&a, p)))
    }) {
        return if copies.is_empty() { None } else { Some(copies.clone()) };
    }
    let copies: Vec<Arc<[f64]>> = if planes.iter().all(|p| all_finite(p)) { copy() } else { Vec::new() };
    if cache.len() >= RING {
        cache.remove(0);
    }
    cache.push((planes.iter().map(|p| Arc::downgrade(p)).collect(), copies.clone()));
    if copies.is_empty() { None } else { Some(copies) }
}

type FlowCopies = Vec<(Vec<Weak<Vec<f64>>>, Vec<Arc<[f64]>>)>;
static FLOW_COPIES: Mutex<FlowCopies> = Mutex::new(Vec::new());

/// Write concrete typed arrays into fresh owned storage.
pub fn write(grain: i64, owner: Owner, kind: &str, name: &str, values: &Value, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    write_storage(&[], grain, owner, kind, name, values, g)
}

/// `[:attribute]`, `[:owner]` and the `[:planes]` record (width and x/y/z/w planes, as
/// `sop/attr_planes` reads them) with the matching float, vec2, vec3 or vec4 storage.
pub fn with_planes(grain: i64, g: &geometry::T, fields: &[(String, Value)]) -> Result<geometry::T, Diagnostic> {
    let Some(name) = assoc(fields, "attribute").and_then(text_of) else {
        return err("E_ATTR_NAME", "sop/with_planes needs attribute as text.");
    };
    let owner = owner(&assoc(fields, "owner").and_then(text_of).unwrap_or_else(|| "point".to_owned()))?;
    match assoc(fields, "planes") {
        Some(Value::Record(planes)) => match assoc(planes, "width") {
            Some(Value::Int(w)) if (1..=4).contains(w) => {
                let w = *w as usize;
                let names = &["x", "y", "z", "w"][..w];
                let kind = ["float", "vec2", "vec3", "vec4"][w - 1];
                let values = if w == 1 {
                    assoc(planes, "x").cloned().ok_or_else(|| fail("E_ATTR_TYPE", "sop/with_planes needs a planes record."))?
                } else {
                    let mut out = Vec::new();
                    for n in names {
                        out.push(((*n).to_owned(), assoc(planes, n).cloned().ok_or_else(|| fail("E_ATTR_TYPE", "sop/with_planes needs a planes record."))?));
                    }
                    Value::Record(out)
                };
                write(grain, owner, kind, &name, &values, g)
            }
            Some(Value::Int(_)) => err("E_ATTR_WIDTH", "Plane width is 1 to 4."),
            _ => err("E_ATTR_TYPE", "sop/with_planes needs an integer width."),
        },
        _ => err("E_ATTR_TYPE", "sop/with_planes needs a planes record."),
    }
}

/// Write finite numeric replacements at a zero/one integer selection. Unselected rows retain
/// matching immutable native storage, including nonfinite bytes; missing or differently typed
/// storage uses the explicit finite default row.
pub fn write_selected(grain: i64, owner: Owner, kind: &str, name: &str, values: &Value, selection: &Value, default: &Value, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    let w = width(kind);
    let count = owner_count(g, owner);
    let constant = constant_source(values);
    // (row data, constant count)
    let vals: Option<(Vec<f64>, Option<usize>)> = match (kind, values) {
        ("float", Value::Float_array(d)) | ("vec2", Value::Vec2_array(d)) | ("vec3", Value::Vec3_array(d)) | ("vec4", Value::Vec4_array(d)) => {
            Some((d.to_vec(), None))
        }
        (_, Value::Source(..)) => match &constant {
            Some(Ok((operation, n, value))) if *operation == format!("array/{kind}") => {
                let row = match value {
                    Value::Float(x) => vec![*x],
                    Value::Vec3(x, y, z) => vec![*x, *y, *z],
                    Value::Vec4(x, y, z, q) => vec![*x, *y, *z, *q],
                    _ => Vec::new(),
                };
                (row.len() == w && w > 0).then_some((row, Some(*n as usize)))
            }
            _ => None,
        },
        _ => None,
    };
    let default_row: Option<Vec<f64>> = match (kind, default) {
        ("float", Value::Float(x)) => Some(vec![*x]),
        ("float", Value::Int(x)) => Some(vec![*x as f64]),
        ("vec2", Value::Vec2(x, y)) => Some(vec![*x, *y]),
        ("vec3", Value::Vec3(x, y, z)) => Some(vec![*x, *y, *z]),
        ("vec4", Value::Vec4(x, y, z, q)) => Some(vec![*x, *y, *z, *q]),
        _ => None,
    };
    let (Some((vals, constant_count)), Value::Int_array(selection), Some(default_row)) = (vals, selection, default_row) else {
        return match constant {
            Some(Err(d)) => Err(d),
            _ => err("E_ATTR_TYPE", "Selected writes require numeric arrays, an integer selection and a matching default value."),
        };
    };
    let value_count = constant_count.unwrap_or(vals.len() / w);
    if count > max_floats(w) || value_count != count || (constant_count.is_none() && vals.len() % w != 0) {
        return err("E_ATTR_COUNT", &format!("{name} needs {count} {} values; got {value_count}.", owner_name(owner)));
    }
    if selection.len() != count {
        return err("E_ATTR_COUNT", "Attribute selection count differs from owner count.");
    }
    if !selection.iter().all(|&s| s == 0 || s == 1) {
        return err("E_ATTR_SELECTION", "Attribute selection must contain only zero or one.");
    }
    if !(all_finite(&vals) && all_finite(&default_row)) {
        return err("E_NONFINITE", "Attribute values must be finite.");
    }
    let all_selected = constant_count.is_some() && selection.iter().all(|&s| s == 1);
    let held = held_of(owner, name, g);
    let old: Option<Vec<&[f64]>> = match &held {
        Some(h) => match held_storage(h) {
            Ok(s) if storage_kind(s) == kind => columns(s)?,
            _ => None,
        },
        None => None,
    };
    let planes: Vec<Vec<f64>> = if all_selected {
        (0..w).map(|c| vec![vals[c]; count]).collect()
    } else {
        ranges(grain, count, &vec![1; w], &|first, last| {
            let mut cols: Vec<Vec<f64>> = (0..w).map(|_| Vec::with_capacity(last - first)).collect();
            for i in first..last {
                for (c, col) in cols.iter_mut().enumerate() {
                    col.push(if selection[i] == 1 {
                        let row = if constant_count.is_none() { i * w } else { 0 };
                        vals[row + c]
                    } else {
                        match &old {
                            Some(old) => old[c][i],
                            None => default_row[c],
                        }
                    });
                }
            }
            cols
        })
    };
    install(owner, kind, name, planes, g)
}

/// Consume validated constant-array metadata without dense intermediate storage. Other values
/// return `None` and retain the ordinary writer path.
pub fn write_constant_source(
    grain: i64,
    owner: Owner,
    kind: &str,
    name: &str,
    selection: Option<&Value>,
    default: Option<&Value>,
    values: &Value,
    g: &geometry::T,
) -> Result<Option<geometry::T>, Diagnostic> {
    let Some(result) = constant_source(values) else { return Ok(None) };
    let (operation, count, value) = result?;
    if operation != format!("array/{kind}") {
        return err("E_ATTR_TYPE", &format!("sop/with_attr requires {kind} storage."));
    }
    let wanted = owner_count(g, owner);
    if count != wanted as i64 {
        return err("E_ATTR_COUNT", &format!("{name} needs {wanted} {} values; got {count}.", owner_name(owner)));
    }
    let count = wanted;
    match (selection, default) {
        (Some(selection), Some(default)) => write_selected(grain, owner, kind, name, values, selection, default, g).map(Some),
        (None, None) => match &value {
            Value::Int(n) => semantic_owned(owner, name, Storage::Int(vec![*n; count]), g).map(Some),
            Value::Text(t) => semantic_owned(owner, name, Storage::Text(vec![String::from_utf8_lossy(t).into_owned(); count]), g).map(Some),
            Value::Float(x) => install(owner, kind, name, vec![vec![*x; count]], g).map(Some),
            Value::Vec3(x, y, z) => install(owner, kind, name, [*x, *y, *z].iter().map(|v| vec![*v; count]).collect(), g).map(Some),
            Value::Vec4(x, y, z, q) => install(owner, kind, name, [*x, *y, *z, *q].iter().map(|v| vec![*v; count]).collect(), g).map(Some),
            _ => err("E_ATTR_TYPE", "Constant array fill has the wrong storage."),
        },
        _ => err("E_ATTR_SELECTION", "Selected writes require both selection and default."),
    }
}

// ---- source origins and program preparation ---------------------------------------------------

/// The ordered source-to-topology-origin proof used by [prepare].
pub fn source_origins(sources: &[i64], inputs: &[node::T]) -> Result<Vec<(i64, i64)>, Diagnostic> {
    if sources.len() != inputs.len() {
        return err("E_DATA_SOURCE", "Attribute source ids and inputs must correspond.");
    }
    fn point_origin(n: &node::T, origins: &mut std::collections::HashMap<i64, i64>) -> i64 {
        if let Some(origin) = origins.get(&node::id(n)) {
            return *origin;
        }
        let facts = node::facts(n);
        let input = match facts.cook_mode {
            CookMode::DuplicateInput(i) | CookMode::InPlace(i) | CookMode::InstanceInput(i) | CookMode::Passthrough(i) => Some(i),
            _ => None,
        };
        // a pass-through (a card marker) has its input's elements
        let passthrough = matches!(facts.cook_mode, CookMode::Passthrough(_));
        let origin = match input {
            Some(i) if facts.topology == Topology::Preserved && (passthrough || facts.elementwise != Elementwise::None) => {
                let inputs = build::input_array(n);
                if i >= 0 && (i as usize) < inputs.len() {
                    point_origin(&inputs[i as usize], origins)
                } else {
                    node::id(n)
                }
            }
            _ => node::id(n),
        };
        origins.insert(node::id(n), origin);
        origin
    }
    let mut origins = std::collections::HashMap::new();
    Ok(sources.iter().zip(inputs).map(|(id, n)| (*id, point_origin(n, &mut origins))).collect())
}

/// A normal geometry consumer: cooking it through Session expands upstream packed instances at the
/// existing input boundary. Retain it with its source.
pub fn materialized_source(source: &node::T) -> node::T {
    let made = build::make_geometry(
        build::Args {
            parameters: Vec::new(),
            cook_mode: CookMode::Generic,
            dependencies: D::STATIC,
            inputs: vec![source.clone()],
            ..build::args("flow.capture")
        },
        None,
        Arc::new(|_, _, geometries| Ok(node::GeometryCooked { geometry: geometries[0].clone(), instances: None })),
    );
    build::with_input_use(InputUse::Cpu, &made)
}

type Origin = (Vec<String>, Vec<i64>);

fn origin_of(by_source: &[(i64, i64)], id: i64) -> Option<i64> {
    by_source.iter().find(|(k, _)| *k == id).map(|(_, o)| *o)
}

fn captured_args(bindings: &[(String, Value)], args: &[(String, Term)]) -> Vec<(String, Value)> {
    args.iter().filter_map(|(n, t)| captured(bindings, t).map(|v| (n.clone(), v))).collect()
}

fn promoted<X: Clone>(args: &[(String, X)], target_of: &dyn Fn(&X) -> X) -> Vec<(String, X)> {
    match args.iter().find(|(k, _)| k == "as_owner") {
        None => args.to_vec(),
        Some((_, target)) => {
            let mut out = vec![("owner".to_owned(), target_of(target))];
            out.extend(args.iter().filter(|(k, _)| k != "owner").cloned());
            out
        }
    }
}

fn owner_origin(by_source: &[(i64, i64)], id: i64, parts: &[&str]) -> Option<Origin> {
    origin_of(by_source, id).map(|o| {
        let mut p: Vec<String> = parts.iter().map(|s| (*s).to_owned()).collect();
        p.push(o.to_string());
        (p, Vec::new())
    })
}

fn count_source(by_source: &[(i64, i64)], residual: ResidualIdAlias, term: &Term) -> Option<Origin> {
    let bindings = E::Residual::residual_view(residual).ok()?.bindings;
    let topology_component: Option<(String, Vec<(String, Value)>)> = match &term.node {
        TNode::Get(inner, component) if matches!(&inner.node, TNode::Op { op, .. } if op == "sop/topology") => match &inner.node {
            TNode::Op { args, .. } => Some((component.clone(), captured_args(&bindings, args))),
            _ => None,
        },
        _ => match captured(&bindings, term) {
            Some(Value::Source(n, _, args)) if n == "sop/topology" => assoc(&args, "component").and_then(text_of).map(|c| (c, args.clone())),
            _ => None,
        },
    };
    // a fill of one value per point ([array/int (sop/count g :points) v]) or its index
    // ([array/range (sop/count g :points)]) counts g's points
    fn filled(by_source: &[(i64, i64)], residual: ResidualIdAlias, term: &Term) -> Option<Origin> {
        let bindings = E::Residual::residual_view(residual).ok()?.bindings;
        match &term.node {
            TNode::Op { op, args, .. } if ["array/int", "array/float", "array/vec2", "array/vec3", "array/vec4", "array/range"].contains(&op.as_str()) => {
                match args.iter().find(|(k, _)| k == "count").and_then(|(_, t)| captured(&bindings, t)) {
                    Some(Value::Source(n, _, counted) | Value::Struct(n, _, counted)) if n == "sop/count" => {
                        match (assoc(&counted, "geometry").and_then(geometry_id), assoc(&counted, "element").and_then(text_of)) {
                            (Some(id), Some(element)) if element == "points" => owner_origin(by_source, id, &["@sop-owner", "point"]),
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            TNode::Expanded { body, .. } => filled(by_source, residual, body),
            TNode::Op { op, args, .. } if op == "exact" && args.len() == 1 => count_source(by_source, residual, &args[0].1),
            TNode::Hof(HofKind::Map, terms) if terms.len() >= 2 => {
                // a map zips its arrays: when they share an origin it has it
                let arrays = &terms[1..];
                match count_source(by_source, residual, &arrays[0]) {
                    Some(origin) if arrays.iter().all(|a| count_source(by_source, residual, a).as_ref() == Some(&origin)) => Some(origin),
                    _ => None,
                }
            }
            TNode::RefBinding(..) | TNode::Get(..) => match captured(&bindings, term) {
                Some(Value::Residual(r)) => {
                    let view = E::Residual::residual_view(r).ok()?;
                    count_source(by_source, r, &view.term)
                }
                _ => None,
            },
            _ => None,
        }
    }
    let filled = filled(by_source, residual, term);
    if filled.is_some() {
        return filled;
    }
    if let Some((component, args)) = topology_component {
        let id = assoc(&args, "geometry").and_then(geometry_id)?;
        return match component.as_str() {
            "vertex_points" => owner_origin(by_source, id, &["@sop-owner", "vertex"]),
            "kinds" => owner_origin(by_source, id, &["@sop-owner", "primitive"]),
            "primitive_offsets" => owner_origin(by_source, id, &["@sop-csr-offsets", "primitive"]),
            _ => None,
        };
    }
    let owner_edge = |v: Option<&Option<Value>>| matches!(v, Some(Some(Value::Text(t))) if t == b"edge");
    let (edge, args): (bool, Option<Vec<(String, Value)>>) = match &term.node {
        TNode::Get(inner, c) if (c == "a" || c == "b") && matches!(&inner.node, TNode::Op { op, .. } if op == "sop/edges") => match &inner.node {
            TNode::Op { args, .. } => (true, Some(captured_args(&bindings, args))),
            _ => (false, None),
        },
        TNode::Op { op, args, .. } if op == "sop/attr" || op == "sop/group" => {
            let resolved: Vec<(String, Option<Value>)> = args
                .iter()
                .map(|(n, t)| {
                    (
                        n.clone(),
                        match &t.node {
                            TNode::Text(text) => Some(Value::Text(text.clone())),
                            _ => captured(&bindings, t),
                        },
                    )
                })
                .collect();
            let resolved = if op == "sop/group" { promoted(&resolved, &|x: &Option<Value>| x.clone()) } else { resolved };
            let edge = op == "sop/group" && owner_edge(resolved.iter().find(|(k, _)| k == "owner").map(|(_, v)| v));
            let missing = resolved.iter().any(|(n, v)| ["owner", "type", "as_owner"].contains(&n.as_str()) && v.is_none());
            (edge, if missing { None } else { Some(resolved.into_iter().filter_map(|(n, v)| v.map(|v| (n, v))).collect()) })
        }
        _ => match captured(&bindings, term) {
            Some(Value::Struct(n, _, args)) if n == "sop/attr" => (false, Some(args)),
            Some(Value::Source(n, _, args)) if n == "sop/attr" || n == "sop/attr_checked" => (false, Some(args)),
            Some(Value::Source(n, _, args)) if n == "sop/edges" => (true, Some(args)),
            Some(Value::Source(n, _, args)) if n == "sop/group" => {
                let args = promoted(&args, &|x: &Value| x.clone());
                (matches!(assoc(&args, "owner"), Some(Value::Text(t)) if t == b"edge"), Some(args))
            }
            _ => (false, None),
        },
    };
    let args = args?;
    let id = assoc(&args, "geometry").and_then(geometry_id)?;
    if edge {
        return origin_of(by_source, id).map(|o| (vec!["@sop-edges".to_owned(), o.to_string()], Vec::new()));
    }
    let (owner, kind) = options(&args).ok()?;
    if ["float", "int", "vec2", "vec3", "vec4", "text"].contains(&kind.as_str()) {
        owner_origin(by_source, id, &["@sop-owner", owner_name(owner)])
    } else if kind == "float_array" || kind == "int_array" {
        match assoc(&args, "component") {
            Some(Value::Text(c)) if c == b"offsets" => owner_origin(by_source, id, &["@sop-csr-offsets", owner_name(owner)]),
            Some(Value::Text(c)) if c == b"values" => {
                let name = assoc(&args, "attribute").and_then(text_of)?;
                Some((vec!["@sop-csr-values".into(), owner_name(owner).into(), name, kind.clone(), id.to_string()], Vec::new()))
            }
            _ => None,
        }
    } else {
        None
    }
}

type ResidualIdAlias = flow_syntax::value::ResidualId;

/// Use instantiated, parameter-sensitive topology and elementwise facts to prove equal owner-array
/// counts before dynamic multi-input fusion. CSR row offsets and flattened values use separate
/// count origins.
pub fn prepare(
    profile: Option<Arc<flow_ir::flow_ir::Profile::T>>,
    strict: bool,
    approx: Option<flow_check::workspace::Paths>,
    sink: Option<Sink>,
    sources: &[i64],
    inputs: &[node::T],
    values: &Value,
) -> Result<Executor::Program, Diagnostic> {
    let by_source = source_origins(sources, inputs)?;
    let count = flow_ir::packed::CountSource::new(Arc::new(move |residual, term| count_source(&by_source, residual, term)));
    Executor::compile(values, &Executor::CompileOptions { trace: None, profile, strict, approx: approx.unwrap_or_default(), sink, count_source: Some(count) })
}

fn column_arc(plane: &Arc<[f64]>) -> Arc<Vec<f64>> {
    let mut cache = COLUMNS.lock().unwrap();
    cache.retain(|(w, _)| w.strong_count() > 0);
    if let Some((_, col)) = cache.iter().find(|(w, _)| w.upgrade().is_some_and(|a| Arc::ptr_eq(&a, plane))) {
        return col.clone();
    }
    let col = Arc::new(plane.to_vec());
    if cache.len() >= 64 {
        cache.remove(0);
    }
    cache.push((Arc::downgrade(plane), col.clone()));
    col
}
// ponytail: Flow columns are Arc<Vec<f64>> where geometry planes are Arc<[f64]>, so a borrowed
// column is a cached copy keyed by the plane's identity; the ring keeps 64.
static COLUMNS: Mutex<Vec<(Weak<[f64]>, Arc<Vec<f64>>)>> = Mutex::new(Vec::new());

fn columns_of(storage: &Storage) -> Result<Option<Vec<Column>>, Diagnostic> {
    let column = |data: Arc<Vec<f64>>| Column { data, offset: 0, stride: 1 };
    Ok(match shared_planes(storage) {
        Some(arcs) => Some(arcs.iter().map(|p| column(column_arc(p))).collect()),
        None => columns(storage)?.map(|ps| ps.into_iter().map(|p| column(Arc::new(p.to_vec()))).collect()),
    })
}

fn borrowed_value(geometry: &GeometryLookup, value: &Value) -> Result<Option<Vec<Column>>, Diagnostic> {
    match value {
        Value::Struct(n, _, args) | Value::Source(n, _, args) if n == "sop/attr" => {
            match (assoc(args, "geometry").and_then(geometry_id), assoc(args, "attribute").and_then(text_of)) {
                (Some(id), Some(name)) => match geometry(id) {
                    None => not_cooked(),
                    Some(g) => {
                        let (owner, kind) = options(args)?;
                        let held = typed_storage(owner, &name, &kind, &g)?;
                        columns_of(held_storage(&held)?)
                    }
                },
                _ => Ok(None),
            }
        }
        Value::Residual(r) => match E::Residual::residual_view(*r) {
            Ok(view) => borrowed_source(geometry, *r, &view.term),
            Err(_) => Ok(None),
        },
        _ => Ok(None),
    }
}

fn borrowed_source(geometry: &GeometryLookup, residual: ResidualIdAlias, term: &Term) -> Result<Option<Vec<Column>>, Diagnostic> {
    let Ok(view) = E::Residual::residual_view(residual) else { return Ok(None) };
    let bindings = view.bindings;
    match &term.node {
        TNode::Op { op, args, .. } if op == "sop/attr" => {
            let argument = |name: &str| -> Option<Value> {
                args.iter().find(|(k, _)| k == name).and_then(|(_, t)| match &t.node {
                    TNode::Text(text) => Some(Value::Text(text.clone())),
                    _ => captured(&bindings, t),
                })
            };
            if ["owner", "type"].iter().any(|key| args.iter().any(|(k, _)| k == key) && argument(key).is_none()) {
                return Ok(None);
            }
            match (argument("geometry"), argument("attribute")) {
                (Some(g), Some(a)) => {
                    let mut fields = vec![("geometry".to_owned(), g), ("attribute".to_owned(), a)];
                    fields.extend(["owner", "type"].iter().filter_map(|k| argument(k).map(|v| ((*k).to_owned(), v))));
                    borrowed_value(geometry, &Value::Struct("sop/attr".into(), term.ty.clone(), fields))
                }
                _ => Ok(None),
            }
        }
        TNode::Expanded { body, .. } => borrowed_source(geometry, residual, body),
        TNode::Bypass(body) => match W::bypass_input(body) {
            Some(input) => borrowed_source(geometry, residual, &input),
            None => Ok(None),
        },
        _ => match captured(&bindings, term) {
            Some(v) => borrowed_value(geometry, &v),
            None => Ok(None),
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn write_planes(
    state: Option<&E::State>,
    elems: Option<&[(String, Value)]>,
    gpu: Option<&Gpu::Config>,
    strict: bool,
    resolve: &flow_ir::core_exec::ResolveFn,
    geometry: &GeometryLookup,
    owner: Owner,
    kind: &str,
    name: &str,
    program: &Executor::Program,
    target: &geometry::T,
    live: &input::Frame,
) -> Result<Option<geometry::T>, Diagnostic> {
    let Some(region) = Executor::packed_root(program) else { return Ok(None) };
    if !flow_ir::packed::elementwise(&region) {
        return Ok(None);
    }
    let w = width(kind);
    if w == 0 || region.outputs.len() != 1 || region.outputs[0].len() != w {
        return Ok(None);
    }
    let is_p = owner == Owner::Point && kind == "vec3" && name == "P";
    let original = Float3::View::view(geometry::positions(target)).map_err(|e| attr_err(rerr(&e)))?;
    let original = [original.x, original.y, original.z];
    let count = owner_count(target, owner);
    let source_geometry = geometry.clone();
    let column_source: flow_ir::packed::ColumnSource = Arc::new(move |residual, term| borrowed_source(&source_geometry, residual, term));
    let unchanged = |component: usize, register: i64| -> bool {
        if !is_p {
            return false;
        }
        use flow_ir::core_ir::Region::Instruction as I;
        match &region.code[register as usize] {
            I::Input(input, _, index) => {
                let (residual, term) = &region.sources[*input as usize];
                match borrowed_source(geometry, *residual, term) {
                    Ok(Some(columns)) => {
                        let column = &columns[*index as usize];
                        Arc::ptr_eq(&column.data, &column_arc(&original[component])) && column.offset == 0 && column.stride == 1
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    };
    let keep: Vec<bool> = region.outputs[0].iter().enumerate().map(|(c, r)| unchanged(c, *r)).collect();
    let outputs = Arc::new(Mutex::new(vec![keep.iter().map(|k| if *k { None } else { Some(vec![0.0; count]) }).collect::<Vec<Option<Vec<f64>>>>()]));
    let options = Executor::ForceOptions { state: state.cloned(), elems: elems.map(|e| e.to_vec()), resolve: Some(resolve.clone()), strict, ..Default::default() };
    let forced = Executor::force_into(&options, gpu.cloned(), column_source, count as i64, outputs.clone(), program, live).map_err(|mut d| {
        if d.code == "E_ARRAY_COUNT" {
            d.code = "E_ATTR_COUNT".to_owned();
        }
        d
    })?;
    if forced.is_none() {
        return Ok(None);
    }
    let mut filled = std::mem::take(&mut outputs.lock().unwrap()[0]);
    if is_p {
        let planes: Vec<Arc<[f64]>> = (0..3).map(|c| match filled[c].take() {
            Some(data) => Arc::from(data),
            None => original[c].clone(),
        }).collect();
        install_planes(owner, kind, name, Planes::Shared(planes), target).map(Some)
    } else {
        let planes: Vec<Vec<f64>> = filled.into_iter().map(|p| p.unwrap_or_default()).collect();
        install(owner, kind, name, planes, target).map(Some)
    }
}

#[derive(Clone)]
struct Constant {
    op: op::Op,
    count: Executor::Program,
    value: Option<Executor::Program>,
}

fn prepare_constant(kind: &str, values: &Value) -> Option<Constant> {
    let constructors = [("array/int", "int"), ("array/text", "text"), ("array/float", "float"), ("array/vec3", "vec3"), ("array/vec4", "vec4")];
    fn find(constructors: &[(&str, &str)], kind: &str, seen: &mut Vec<i64>, residual: ResidualIdAlias, term: &Term) -> Option<(ResidualIdAlias, String, Vec<(String, Term)>)> {
        match &term.node {
            TNode::Expanded { body, .. } => find(constructors, kind, seen, residual, body),
            TNode::Op { op, args, .. } if op == "exact" && args.len() == 1 && args[0].0 == "value" => find(constructors, kind, seen, residual, &args[0].1),
            TNode::RefBinding(..) | TNode::Get(..) => {
                let bindings = E::Residual::residual_view(residual).ok()?.bindings;
                match captured(&bindings, term) {
                    Some(Value::Residual(nested)) if !seen.contains(&nested.0) => {
                        seen.push(nested.0);
                        let view = E::Residual::residual_view(nested).ok()?;
                        find(constructors, kind, seen, nested, &view.term)
                    }
                    _ => None,
                }
            }
            TNode::Op { op, args, .. } if constructors.iter().any(|(o, k)| o == op && *k == kind) => Some((residual, op.clone(), args.clone())),
            _ => None,
        }
    }
    let Value::Residual(residual) = values else { return None };
    let view = E::Residual::residual_view(*residual).ok()?;
    let (residual, operation, args) = find(&constructors, kind, &mut vec![residual.0], *residual, &view.term)?;
    let compile = |term: &Term| -> Result<Executor::Program, Diagnostic> {
        let id = E::Residual::with_bindings(residual, term, &[])?;
        Executor::compile(&Value::Residual(id), &Executor::CompileOptions { strict: true, ..Default::default() })
    };
    let count = compile(&args.iter().find(|(k, _)| k == "count")?.1);
    let value = args.iter().find(|(k, _)| k == "value").map(|(_, t)| compile(t));
    match (count, value) {
        (Ok(count), None) => Some(Constant { op: op::find(&[], &operation, &fctx::value())?, count, value: None }),
        (Ok(count), Some(Ok(value))) => Some(Constant { op: op::find(&[], &operation, &fctx::value())?, count, value: Some(value) }),
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn write_constant(
    state: Option<&E::State>,
    elems: Option<&[(String, Value)]>,
    resolve: &flow_ir::core_exec::ResolveFn,
    owner: Owner,
    kind: &str,
    name: &str,
    prepared: Option<&Constant>,
    target: &geometry::T,
    live: &input::Frame,
) -> Result<Option<geometry::T>, Diagnostic> {
    let Some(c) = prepared else { return Ok(None) };
    let options = Executor::ForceOptions { state: state.cloned(), elems: elems.map(|e| e.to_vec()), resolve: Some(resolve.clone()), strict: true, ..Default::default() };
    let count = Executor::force(&c.count, &options, live)?;
    let mut args = vec![("count".to_owned(), count)];
    if let Some(program) = &c.value {
        args.push(("value".to_owned(), Executor::force(program, &options, live)?));
    }
    let sample = op::constant_array_descriptor(&c.op.name, &args)?;
    write_constant_source(4096, owner, kind, name, None, None, &sample, target)
}

/// Delete each (owner, pattern) in order, as `sop/delete_attr` does.
pub fn delete_after(deletes: &[(String, String)], cancel_token: &Arc<cancel::T>, g: &geometry::T) -> Result<geometry::T, Diagnostic> {
    let mut g = g.clone();
    for (owner, pattern) in deletes {
        let pattern_for = |selected: &str| Some(if owner == selected { pattern.clone() } else { String::new() });
        let params = rdk::attribute_lifecycle::DeleteParams {
            cancel: Some(cancel_token.clone()),
            reference: None,
            delete_non_selected: false,
            point_pattern: pattern_for("point"),
            vertex_pattern: pattern_for("vertex"),
            primitive_pattern: pattern_for("primitive"),
            detail_pattern: pattern_for("detail"),
        };
        g = rdk::attribute_ops::delete(&params, &g).map_err(|e| fail("E_ATTR_DELETE", &rerror::to_string(&e)))?;
    }
    Ok(g)
}

/// The measured display sinks of a backend: seconds per float the editor spent preparing a host
/// mesh for display, and a resident one. Weakly keyed, so a closed owner's rates go with it.
#[allow(non_snake_case)]
pub mod Display_sink {
    use super::*;
    use std::collections::HashMap;

    struct Rates {
        host: f64,
        draw: f64,
        resident: f64,
        /// per write site, its recent CPU cooks (elements, seconds per element), at most 256 sites
        writes: HashMap<String, Vec<(i64, f64)>>,
        /// per write site, its priced device cooks
        devices: HashMap<String, i64>,
    }

    static REGISTRY: Mutex<Vec<(Weak<Gpu::Backend>, Rates)>> = Mutex::new(Vec::new());

    fn with_rates<R>(backend: &Arc<Gpu::Backend>, f: impl FnOnce(&mut Rates) -> R) -> R {
        let mut registry = REGISTRY.lock().unwrap();
        registry.retain(|(weak, _)| weak.strong_count() > 0);
        let at = Arc::as_ptr(backend);
        let index = match registry.iter().position(|(weak, _)| std::ptr::eq(weak.as_ptr(), at)) {
            Some(i) => i,
            None => {
                let rates = Rates { host: f64::INFINITY, draw: f64::INFINITY, resident: f64::INFINITY, writes: HashMap::new(), devices: HashMap::new() };
                registry.push((Arc::downgrade(backend), rates));
                registry.len() - 1
            }
        };
        f(&mut registry[index].1)
    }

    /// Seconds the editor spent preparing a mesh of `floats` floats for display.
    pub fn observe(backend: &Arc<Gpu::Backend>, resident: bool, floats: i64, seconds: f64) {
        if floats > 0 {
            let rate = seconds / floats as f64;
            with_rates(backend, |r| {
                if resident {
                    r.resident = r.resident.min(rate)
                } else {
                    r.host = r.host.min(rate)
                }
            });
        }
    }

    /// Seconds from a host mesh's frame to its presentation: the renderer's conversion and upload,
    /// which preparation does not see. The smallest rate is kept.
    pub fn observe_draw(backend: &Arc<Gpu::Backend>, floats: i64, seconds: f64) {
        if floats > 0 {
            let rate = seconds / floats as f64;
            with_rates(backend, |r| r.draw = r.draw.min(rate));
        }
    }

    /// Seconds per float (host mesh, resident mesh); infinity while unmeasured. A host mesh's
    /// includes its first draw once seen.
    pub fn rates(backend: &Arc<Gpu::Backend>) -> (f64, f64) {
        with_rates(backend, |r| (r.host + if r.draw < f64::INFINITY { r.draw } else { 0.0 }, r.resident))
    }

    /// The cheaper rate of the two `(size, seconds per unit)` samples nearest `size`, priced at
    /// `size`: a route's first cook pays one-time preparation. `None` below two samples.
    pub fn priced(samples: &[(i64, f64)], size: i64) -> Option<f64> {
        if samples.len() < 2 {
            return None;
        }
        let distance = |n: i64| ((n.max(1) as f64) / (size.max(1) as f64)).ln().abs();
        let mut nearest = samples.to_vec();
        nearest.sort_by(|a, b| distance(a.0).total_cmp(&distance(b.0)));
        let rate = nearest.iter().take(2).fold(f64::INFINITY, |m, (_, r)| m.min(*r));
        Some(rate * size as f64)
    }

    /// Whether a write site with no CPU measurement stays on the device: for its first two priced
    /// cooks, or while the device cannot price it. False: sample the CPU under a budget.
    pub fn device_first(backend: &Arc<Gpu::Backend>, site: &str, device_seconds: Option<f64>) -> bool {
        if device_seconds.is_none() {
            return true;
        }
        with_rates(backend, |r| {
            let seen = r.devices.get(site).copied().unwrap_or(0);
            if seen >= 2 {
                false
            } else {
                if seen == 0 && r.devices.len() >= 256 {
                    r.devices.clear();
                }
                r.devices.insert(site.to_owned(), seen + 1);
                true
            }
        })
    }

    /// The measured seconds of an attribute write site's CPU route at `count` elements.
    pub fn cpu_write(backend: &Arc<Gpu::Backend>, site: &str, count: i64) -> Option<f64> {
        with_rates(backend, |r| priced(r.writes.get(site).map_or(&[][..], |v| v.as_slice()), count))
    }

    /// One CPU cook of a write site.
    pub fn observe_cpu_write(backend: &Arc<Gpu::Backend>, site: &str, count: i64, seconds: f64) {
        if count > 0 {
            with_rates(backend, |r| {
                let mut old = r.writes.get(site).cloned().unwrap_or_default();
                if old.is_empty() && r.writes.len() >= 256 {
                    r.writes.clear();
                }
                old.truncate(7);
                old.insert(0, (count, seconds / count as f64));
                r.writes.insert(site.to_owned(), old);
            });
        }
    }

    /// Backends still owning rates, for the close checks.
    pub fn live() -> usize {
        let mut registry = REGISTRY.lock().unwrap();
        registry.retain(|(weak, _)| weak.strong_count() > 0);
        registry.len()
    }
}

// ---- the node ----------------------------------------------------------------------------------

/// The optional arguments of OCaml `node`.
#[derive(Clone)]
pub struct NodeOptions {
    pub state: Option<E::State>,
    pub reference: bool,
    pub strict: bool,
    pub elems: Option<Vec<(String, Value)>>,
    pub profile: Option<Arc<flow_ir::flow_ir::Profile::T>>,
    pub approx: flow_check::workspace::Paths,
    /// the cache identity text of the kernel that built the node (`kernel_source`)
    pub source: String,
    pub gpu: Option<Gpu::Config>,
    pub selection: Option<Value>,
    pub default: Option<Value>,
    pub guard: Option<Value>,
    pub prior: Vec<(String, String, Value)>,
    pub owner: Owner,
    pub kind: String,
    pub deletes: Vec<(String, String)>,
}

impl Default for NodeOptions {
    fn default() -> NodeOptions {
        NodeOptions {
            state: None,
            reference: false,
            strict: false,
            elems: None,
            profile: None,
            approx: flow_check::workspace::Paths::new(),
            source: String::new(),
            gpu: None,
            selection: None,
            default: None,
            guard: None,
            prior: Vec::new(),
            owner: Owner::Point,
            kind: "vec3".to_owned(),
            deletes: Vec::new(),
        }
    }
}

struct Kernel {
    clock: Arc<dyn Fn() -> f64 + Send + Sync>,
    state: Option<E::State>,
    reference: bool,
    strict: bool,
    elems: Option<Vec<(String, Value)>>,
    gpu: Option<Gpu::Config>,
    guard: Option<Value>,
    prior: Vec<(String, String, Value)>,
    owner: Owner,
    kind: String,
    name: String,
    deletes: Vec<(String, String)>,
    sources: Vec<i64>,
    values: Value,
    selected: bool,
    program: Result<Executor::Program, Diagnostic>,
    lone_record: Option<Result<Executor::Program, Diagnostic>>,
    owned_fields: Vec<String>,
    constant: Option<Constant>,
    selected_constant: Option<(Constant, Result<Executor::Program, Diagnostic>)>,
}

fn site_of(id: &FId) -> String {
    format!("{}#{}", id.0.join("/"), id.1.iter().map(i64::to_string).collect::<Vec<_>>().join(","))
}

impl Kernel {

    fn cook(&self, node_id: i64, context: &context::T, geometries: &[geometry::T]) -> Result<node::GeometryCooked, Diagnostic> {
        let geo: GeometryLookup = {
            let gs = Arc::new(geometries.to_vec());
            let sources = self.sources.clone();
            Arc::new(move |id| sources.iter().position(|s| *s == id).map(|i| gs[i].clone()))
        };
        let resolver: flow_ir::core_exec::ResolveFn = {
            let (g, c) = (geo.clone(), context.clone());
            Arc::new(move |v| resolve(Some(&c), &g, v))
        };
        let live = context::input(context).clone();
        // the site and count a measured decision asked about, and when it left this cook to the CPU
        let asked: Arc<Mutex<Option<(Arc<Gpu::Backend>, String, i64)>>> = Arc::new(Mutex::new(None));
        let cpu_began: Mutex<Option<f64>> = Mutex::new(None);
        // set when this cook samples the CPU: when that sample has lost, and its cost
        let probe: Arc<Mutex<Option<(f64, f64)>>> = Arc::new(Mutex::new(None));
        // a cooked [:when] is decided before the values, which may read what it tests for
        let guarded: Result<bool, Diagnostic> = match &self.guard {
            None => Ok(true),
            Some(guard) => E::Host::force_reference(guard, &live, self.state.as_ref(), self.elems.as_deref(), Some(resolver.clone())).and_then(|v| match v {
                Value::Bool(b) => Ok(b),
                _ => err("E_ATTR_TYPE", "sop/with_attr :when must be a bool"),
            }),
        };
        let attempt = || -> Result<geometry::T, Diagnostic> {
            if geometries.is_empty() {
                return err("E_DATA_SOURCE", "Attribute write needs geometry.");
            }
            if context::cancelled(context) {
                return cancelled_err();
            }
            let guarded = guarded.clone()?;
            let grain = context::grain(context);
            let (owner, kind, name) = (self.owner, self.kind.as_str(), self.name.as_str());
            let mut writes: Vec<(String, String, String)> = self.prior.iter().enumerate().map(|(i, (n, k, _))| (format!("w{i}"), n.clone(), k.clone())).collect();
            writes.push(("values".to_owned(), self.name.clone(), self.kind.clone()));
            let write = |install: &dyn Fn(&str, &str, &str, &geometry::T) -> Result<geometry::T, Diagnostic>| -> Result<geometry::T, Diagnostic> {
                let mut g = geometries[0].clone();
                for (key, n, k) in &writes {
                    g = install(key, n, k, &g)?;
                }
                Ok(g)
            };
            // the record's outputs kept on the device, installed as deferred storage; `None` leaves
            // it to the CPU
            let on_device = |program: &Executor::Program| -> Result<Option<geometry::T>, Diagnostic> {
                let device: Option<Vec<(String, Gpu::Planes)>> = if self.reference || self.elems.is_some() || context::device_refused(context, node_id) {
                    None
                } else {
                    let resident: Option<Arc<dyn Fn(&Value) -> Option<(Value, i64)> + Send + Sync>> = if geometries.iter().all(|g| geometry::releasers(g).is_empty()) {
                        None
                    } else {
                        let g = geo.clone();
                        Some(Arc::new(move |v| resident_read(&g, v)))
                    };
                    // a display borrows only a mesh resident whole: positions held already or written
                    // here, with this write's other outputs
                    let whole = Float3::resident(geometry::positions(&geometries[0])) || name == "P" || self.prior.iter().any(|(n, _, _)| n == "P");
                    // the CPU route leaves a host or mixed mesh, which the display converts and uploads
                    let cpu_penalty = match (context::cpu_consumers(context, node_id), &self.gpu) {
                        (Some(0), Some(gpu)) if gpu.backend.is_some() && whole => {
                            let g = &geometries[0];
                            let floats = geometry::point_count(g) * 9 + geometry::primitive_count(g) * 6;
                            let (host, _) = Display_sink::rates(gpu.backend.as_ref().unwrap());
                            if host < f64::INFINITY { host * floats as f64 } else { Cost::readback(Some(gpu), floats, 1) }
                        }
                        _ => 0.0,
                    };
                    let cpu_cost: Option<Arc<dyn Fn(&FId, i64) -> Option<f64> + Send + Sync>> = self.gpu.as_ref().and_then(|gpu| gpu.backend.clone()).map(|backend| {
                        let asked = asked.clone();
                        Arc::new(move |owner: &FId, count: i64| {
                            let site = site_of(owner);
                            *asked.lock().unwrap() = Some((backend.clone(), site.clone(), count));
                            Display_sink::cpu_write(&backend, &site, count)
                        }) as Arc<dyn Fn(&FId, i64) -> Option<f64> + Send + Sync>
                    });
                    let device_first: Option<Arc<dyn Fn(&FId, i64, Option<f64>) -> bool + Send + Sync>> = self.gpu.as_ref().and_then(|gpu| gpu.backend.clone()).map(|backend| {
                        let asked = asked.clone();
                        let probe = probe.clone();
                        let clock = self.clock.clone();
                        let consumers = context::cpu_consumers(context, node_id).unwrap_or(0);
                        Arc::new(move |owner: &FId, count: i64, device_seconds: Option<f64>| {
                            let site = site_of(owner);
                            *asked.lock().unwrap() = Some((backend.clone(), site.clone(), count));
                            // with a declared CPU reader the device route reads back: a write the CPU
                            // is expected to cook faster than the device dispatches measures the CPU first
                            let cpu_first = consumers > 0 && device_seconds.is_some_and(|device| Cost::estimate(Cost::packed(count), count) < device);
                            if !cpu_first && Display_sink::device_first(&backend, &site, device_seconds) {
                                return true;
                            }
                            let budget = Cost::probe_budget(device_seconds.unwrap_or(0.0));
                            *probe.lock().unwrap() = Some((clock() + budget, budget));
                            false
                        }) as Arc<dyn Fn(&FId, i64, Option<f64>) -> bool + Send + Sync>
                    });
                    let options = Executor::RecordOptions {
                        state: self.state.clone(),
                        resolve: Some(resolver.clone()),
                        resident,
                        expected_count: Some(owner_count(&geometries[0], owner) as i64),
                        cpu_consumers: match context::cpu_consumers(context, node_id) {
                            // beside host positions the written field is read back to be drawn
                            Some(0) if !whole => Some(1),
                            readers => readers,
                        },
                        cpu_penalty,
                        cpu_cost,
                        device_first,
                        gpu: self.gpu.clone(),
                    };
                    Executor::record_on_device(program, &options, &live)?
                };
                let Some(fields) = device else {
                    *cpu_began.lock().unwrap() = Some((self.clock)());
                    return Ok(None);
                };
                // in flight: the session settles the dispatch before a CPU reader, and recooks this
                // write on the CPU if its status fails
                let kept: Vec<(String, Arc<Gpu::Planes>)> = fields.into_iter().map(|(k, p)| (k, Arc::new(p))).collect();
                if let Some((_, first)) = kept.first() {
                    let first = first.clone();
                    context::defer(context, node_id, Box::new(move || (first.settle)()));
                }
                write(&|key, n, k, g| {
                    let planes = &kept.iter().find(|(f, _)| f == key).expect("record field").1;
                    let count = owner_count(g, owner);
                    if width(k) as i64 == planes.width && planes.count == count as i64 {
                        install_deferred(owner, k, n, planes, g)
                    } else {
                        let _ = (planes.settle)();
                        let data = (planes.materialise)();
                        let values = match (k, data.len()) {
                            ("int", 1) => Value::Int_array(Arc::new(data[0].iter().map(|x| *x as i64).collect())),
                            (_, 1) => float_array(data.into_iter().next().unwrap()),
                            _ => Value::Record(Vec::new()),
                        };
                        write_storage(&[], grain, owner, k, n, &values, g)
                    }
                })
                .map(Some)
            };
            // a lone write over data the device holds (a gather of a resident P) stays there
            let lone = || -> Result<Option<geometry::T>, Diagnostic> {
                match &self.lone_record {
                    Some(Ok(program)) => on_device(program),
                    _ => Ok(None),
                }
            };
            let force_options = || Executor::ForceOptions {
                state: self.state.clone(),
                elems: self.elems.clone(),
                resolve: Some(resolver.clone()),
                reference: self.reference,
                strict: self.strict,
                ..Default::default()
            };
            if !guarded {
                return Ok(geometries[0].clone());
            }
            if !self.prior.is_empty() {
                let program = self.program.clone()?;
                if let Some(g) = on_device(&program)? {
                    return Ok(g);
                }
                return match Executor::force(&program, &force_options(), &live)? {
                    Value::Record(fields) => write(&|key, n, k, g| {
                        let owned: Vec<&str> = if key == "values" { self.owned_fields.iter().map(String::as_str).collect() } else { Vec::new() };
                        let value = assoc(&fields, key).ok_or_else(|| fail("E_ATTR_TYPE", "A fused write's data must be a record."))?;
                        write_storage(&owned, grain, owner, k, n, value, g)
                    }),
                    _ => err("E_ATTR_TYPE", "A fused write's data must be a record."),
                };
            }
            let program = self.program.clone()?;
            if self.selected {
                let force = |program: &Executor::Program| Executor::force(program, &force_options(), &live);
                let fields = match &self.selected_constant {
                    None => force(&program)?,
                    Some((c, rest)) => {
                        let mut args = vec![("count".to_owned(), force(&c.count)?)];
                        if let Some(v) = &c.value {
                            args.push(("value".to_owned(), force(v)?));
                        }
                        let count = op::constant_array_descriptor(&c.op.name, &args)?;
                        let rest = rest.clone()?;
                        match force(&rest)? {
                            Value::Record(mut fields) => {
                                fields.insert(0, ("values".to_owned(), count));
                                Value::Record(fields)
                            }
                            _ => return err("E_ATTR_TYPE", "Selected write data must be a record."),
                        }
                    }
                };
                let Value::Record(fields) = fields else { return err("E_ATTR_TYPE", "Selected write data must be a record.") };
                return match (assoc(&fields, "values"), assoc(&fields, "selection"), assoc(&fields, "default")) {
                    (Some(values), Some(selection), Some(default)) => {
                        if context::cancelled(context) {
                            cancelled_err()
                        } else {
                            write_selected(grain, owner, kind, name, values, selection, default, &geometries[0])
                        }
                    }
                    _ => err("E_ATTR_SELECTION", "Selected writes require both selection and default."),
                };
            }
            if let Some(g) = lone()? {
                return Ok(g);
            }
            let borrowed = write_source(&*geo, owner, kind, name, &self.values, &geometries[0])?;
            let direct = match borrowed {
                Some(g) => Some(g),
                None if self.reference => None,
                None => match write_constant(self.state.as_ref(), self.elems.as_deref(), &resolver, owner, kind, name, self.constant.as_ref(), &geometries[0], &live)? {
                    Some(g) => Some(g),
                    None => write_planes(self.state.as_ref(), self.elems.as_deref(), self.gpu.as_ref(), self.strict, &resolver, &geo, owner, kind, name, &program, &geometries[0], &live)?,
                },
            };
            if context::cancelled(context) {
                return cancelled_err();
            }
            if let Some(g) = direct {
                return Ok(g);
            }
            let values = Executor::force(&program, &force_options(), &live)?;
            if context::cancelled(context) {
                return cancelled_err();
            }
            let owned: Vec<&str> = self.owned_fields.iter().map(String::as_str).collect();
            write_storage(&owned, grain, owner, kind, name, &values, &geometries[0])
        };
        let expired: Arc<dyn Fn() -> bool + Send + Sync> = {
            let (probe, clock) = (probe.clone(), self.clock.clone());
            Arc::new(move || probe.lock().unwrap().is_some_and(|(stop, _)| clock() > stop))
        };
        let mut outcome: Option<Result<geometry::T, Diagnostic>> = None;
        let completed = E::with_interrupt(expired, &mut || outcome = Some(attempt()));
        let result = match outcome {
            Some(result) if completed => result,
            _ => {
                // the CPU has lost: this is its measurement, and the cook runs on the device
                let (asked_now, probe_now) = (asked.lock().unwrap().clone(), *probe.lock().unwrap());
                if let (Some((backend, site, count)), Some((_, budget))) = (asked_now, probe_now) {
                    Display_sink::observe_cpu_write(&backend, &site, count, budget);
                    Display_sink::observe_cpu_write(&backend, &site, count, budget);
                }
                *probe.lock().unwrap() = None;
                *cpu_began.lock().unwrap() = None;
                attempt()
            }
        };
        let (asked_now, began) = (asked.lock().unwrap().clone(), *cpu_began.lock().unwrap());
        if let (Some((backend, site, count)), Some(began), Ok(_)) = (asked_now, began, &result) {
            Display_sink::observe_cpu_write(&backend, &site, count, (self.clock)() - began);
        }
        let result = if self.deletes.is_empty() { result } else { result.and_then(|g| delete_after(&self.deletes, context::cancel_token(context), &g)) };
        result.map(|geometry| node::GeometryCooked { geometry, instances: None })
    }
}

/// The attribute write node (OCaml `Attribute_kernel.node`).
pub fn node(clock: Option<Arc<dyn Fn() -> f64 + Send + Sync>>, o: &NodeOptions, name: &str, values: &Value, sources: &[i64], inputs: &[node::T]) -> node::T {
    let (o, values, sources, inputs) = (o.clone(), values.clone(), sources.to_vec(), inputs.to_vec());
    let source = o.source.clone();
    let source = source.as_str();
    let clock = clock.unwrap_or_else(|| Arc::new(|| std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_or(0.0, |d| d.as_secs_f64())));
    let selected = o.selection.is_some() || o.default.is_some();
    assert!(!(selected && !o.prior.is_empty()), "Attribute_kernel.node: a selected write with prior");
    let data = if !o.prior.is_empty() {
        // a fused chain's writes, one record program: the earlier ones, then this one
        let mut fields: Vec<(String, Value)> = o.prior.iter().enumerate().map(|(i, (_, _, v))| (format!("w{i}"), v.clone())).collect();
        fields.push(("values".to_owned(), values.clone()));
        Value::Record(fields)
    } else if !selected {
        values.clone()
    } else {
        let mut fields = vec![("values".to_owned(), values.clone())];
        fields.extend(o.selection.clone().map(|v| ("selection".to_owned(), v)));
        fields.extend(o.default.clone().map(|v| ("default".to_owned(), v)));
        Value::Record(fields)
    };
    let program = prepare(o.profile.clone(), o.strict, Some(o.approx.clone()), Some(Sink::Sop_input), &sources, &inputs, &data);
    // a lone write as a one-field record, for the device (Executor::record_on_device)
    let lone_record = match &o.gpu {
        Some(gpu) if gpu.fast && gpu.backend.is_some() && o.prior.is_empty() && !selected && !o.reference => Some(prepare(
            o.profile.clone(),
            o.strict,
            Some(o.approx.clone()),
            Some(Sink::Sop_input),
            &sources,
            &inputs,
            &Value::Record(vec![("values".to_owned(), values.clone())]),
        )),
        _ => None,
    };
    let owned_fields = if o.strict && !o.reference && (o.kind == "float_array" || o.kind == "int_array") {
        program.as_ref().map(Executor::fresh_csr_fields).unwrap_or_default()
    } else {
        Vec::new()
    };
    let constant = if o.reference { None } else { prepare_constant(&o.kind, &values) };
    let selected_constant = match (selected, constant.clone(), &o.selection, &o.default) {
        (true, Some(constructor), Some(selection), Some(default)) => Some((
            constructor,
            prepare(None, o.strict, None, None, &sources, &inputs, &Value::Record(vec![("selection".to_owned(), selection.clone()), ("default".to_owned(), default.clone())])),
        )),
        _ => None,
    };
    let key = |v: &Value| V::key_of(&|id| E::Residual::residual_id(id), v);
    let mut parameters = format!(
        "{};{};{};{};{};{};",
        source,
        owner_name(o.owner),
        o.kind,
        name,
        o.prior.iter().map(|(n, k, _)| format!("{n}:{k}")).collect::<Vec<_>>().join(","),
        o.deletes.iter().map(|(ow, p)| format!("{ow}:{p}")).collect::<Vec<_>>().join(","),
    )
    .into_bytes();
    parameters.extend(key(&data));
    if let Some(guard) = &o.guard {
        parameters.extend(b";when ");
        parameters.extend(key(guard));
    }
    if let Some(elems) = &o.elems {
        parameters.extend(key(&Value::Record(elems.clone())));
    }
    if let Some(state) = &o.state {
        parameters.extend(E::state_stamp(state).into_bytes());
    }
    let deps = dependencies(&match &o.guard {
        None => data.clone(),
        Some(g) => Value::Record(vec![("d".to_owned(), data.clone()), ("w".to_owned(), g.clone())]),
    });
    let reads_of = if selected {
        let mut r = reads(&data);
        r.push(name.to_owned());
        sorted_unique(r)
    } else {
        reads(&data)
    };
    let writes = sorted_unique(o.prior.iter().map(|(n, _, _)| n.clone()).chain(std::iter::once(name.to_owned())).chain(o.deletes.iter().map(|(_, p)| p.clone())).collect());
    let deletes_empty = o.deletes.is_empty();
    let owner = o.owner;
    let kernel = Arc::new(Kernel {
        clock,
        state: o.state,
        reference: o.reference,
        strict: o.strict,
        elems: o.elems,
        gpu: o.gpu,
        guard: o.guard,
        prior: o.prior,
        owner: o.owner,
        kind: o.kind,
        name: name.to_owned(),
        deletes: o.deletes,
        sources,
        values,
        selected,
        program,
        lone_record,
        owned_fields,
        constant,
        selected_constant,
    });
    let made = build::make_geometry(
        build::Args {
            label: Some(format!("Write {name}")),
            parameters,
            cook_mode: CookMode::DuplicateInput(0),
            dependencies: deps,
            inputs,
            ..build::args("flow.with_attr")
        },
        None,
        Arc::new(move |node_id, context, geometries| kernel.cook(node_id, context, geometries)),
    );
    // a write that reads no attribute and deletes none carries its input's storage through unread
    let mut facts: Facts = node::facts(&made).clone();
    facts.elementwise = match owner {
        Owner::Point => Elementwise::Points,
        Owner::Primitive => Elementwise::Primitives,
        _ => Elementwise::None,
    };
    facts.topology = Topology::Preserved;
    let forward = reads_of.is_empty() && deletes_empty;
    facts.reads = reads_of;
    facts.writes = writes;
    let made = build::with_facts(facts, &made);
    build::with_input_use(if forward { InputUse::Forward(0) } else { InputUse::Cpu }, &made)
}

fn cancelled_err<X>() -> Result<X, Diagnostic> {
    err("E_CANCELLED", "Attribute kernel cancelled.")
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk::{line_geometry, topology};

    /// Bit-exact description of positions and attributes (OCaml `geometry_bytes`, minus groups).
    fn bits(g: &geometry::T) -> Vec<u64> {
        let mut out = Vec::new();
        let view = Float3::View::view(geometry::positions(g)).unwrap();
        for plane in [&view.x, &view.y, &view.z] {
            out.extend(plane.iter().map(|x| x.to_bits()));
            out.push(u64::MAX);
        }
        for item in geometry::attributes(g) {
            out.push(attribute::owner(&item) as u64);
            out.extend(attribute::name(&item).bytes().map(u64::from));
            out.push(u64::MAX - 1);
            match attribute::storage(&item).unwrap() {
                Storage::Float(v) => out.extend(v.iter().map(|x| x.to_bits())),
                Storage::Int(v) => out.extend(v.iter().map(|x| *x as u64)),
                Storage::Text(v) => v.iter().for_each(|s| out.extend(s.bytes().map(u64::from))),
                Storage::Float2(v) => {
                    let w = Float2::View::view(&v);
                    [&w.x, &w.y].iter().for_each(|p| out.extend(p.iter().map(|x| x.to_bits())))
                }
                Storage::Float3(v) => {
                    let w = Float3::View::view(&v).unwrap();
                    [&w.x, &w.y, &w.z].iter().for_each(|p| out.extend(p.iter().map(|x| x.to_bits())))
                }
                Storage::Float4(v) => {
                    let w = Float4::View::view(&v);
                    [&w.x, &w.y, &w.z, &w.w].iter().for_each(|p| out.extend(p.iter().map(|x| x.to_bits())))
                }
                Storage::IntArray(v) => {
                    let w = IntArray::View::view(&v);
                    out.extend(w.offsets.iter().map(|x| *x as u64));
                    out.extend(w.values.iter().map(|x| *x as u64));
                }
                Storage::FloatArray(v) => {
                    let w = FloatArray::View::view(&v);
                    out.extend(w.offsets.iter().map(|x| *x as u64));
                    out.extend(w.values.iter().map(|x| x.to_bits()));
                }
            }
            out.push(u64::MAX - 2);
        }
        out
    }

    fn points(n: usize) -> geometry::T {
        line_geometry::points(&(0..n).map(|i| (i as f64, 0.0, 0.0)).collect::<Vec<_>>()).unwrap()
    }

    fn pack(width: usize, planes: &[Vec<f64>]) -> Storage {
        make_storage(["float", "vec2", "vec3", "vec4"][width - 1], Planes::Owned(planes.to_vec())).unwrap()
    }

    fn array(width: usize, values: Vec<f64>) -> Value {
        wide_array(width, values)
    }

    fn row(width: usize, v: &[f64]) -> Value {
        match width {
            1 => Value::Float(v[0]),
            2 => Value::Vec2(v[0], v[1]),
            3 => Value::Vec3(v[0], v[1], v[2]),
            _ => Value::Vec4(v[0], v[1], v[2], v[3]),
        }
    }

    fn with_attr(g: &geometry::T, owner: Owner, name: &str, storage: Storage) -> geometry::T {
        geometry::with_attribute(attribute::create_owned(name, owner, storage).unwrap(), g).unwrap()
    }

    fn on_domains(domains: i64, work: &(dyn Fn() + Send + Sync)) {
        parallel::run_unit(Some(domains), &|| {
            work();
            Ok(())
        })
        .unwrap();
    }

    fn no_geometry() -> GeometryLookup {
        Arc::new(|_| None)
    }

    fn code<X>(r: Result<X, Diagnostic>) -> String {
        match r {
            Err(d) => d.code,
            Ok(_) => panic!("expected an error"),
        }
    }

    /// Selected attribute writes: all numeric owners, native nonfinite untouched bytes, explicit
    /// defaults and one/eight-domain ownership/refusals.
    #[test]
    fn selected_writes_all_numeric_owners() {
        for count in [0usize, 6, 32769] {
            let mut base = points(count);
            if count == 6 {
                let t = topology::create_owned(6, vec![0, 1, 2, 3, 4, 5], vec![0, 3, 6], vec![topology::PrimitiveKind::Polygon; 2]).unwrap();
                base = geometry::create(geometry::positions(&base).clone(), t, None, None, None).unwrap();
            }
            let owners: Vec<(Owner, usize)> = if count == 6 {
                vec![(Owner::Point, 6), (Owner::Vertex, 6), (Owner::Primitive, 2), (Owner::Detail, 1)]
            } else {
                vec![(Owner::Point, count)]
            };
            for (owner, count) in owners {
                for (kind, width) in [("float", 1usize), ("vec2", 2), ("vec3", 3), ("vec4", 4)] {
                    let replacements: Vec<f64> = (0..count * width).map(|i| if i % width == 0 { -0.0 } else { (i % width) as f64 + 0.25 }).collect();
                    let defaults: Vec<f64> = (0..width).map(|i| i as f64 + 0.75).collect();
                    let values = array(width, replacements.clone());
                    let default = row(width, &defaults);
                    let special = [f64::from_bits(0x7ff8000000001234), f64::INFINITY, f64::NEG_INFINITY, -0.0];
                    let old: Vec<Vec<f64>> = (0..width)
                        .map(|c| (0..count).map(|i| if c == width - 1 { special[i % 4] } else { i as f64 + c as f64 }).collect())
                        .collect();
                    for mode in ["absent", "wrong", "matching"] {
                        let original = match mode {
                            "absent" => base.clone(),
                            "wrong" => with_attr(&base, owner, "selected", Storage::Int(vec![7; count])),
                            _ => with_attr(&base, owner, "selected", pack(width, &old)),
                        };
                        let before = bits(&original);
                        let selects: [&dyn Fn(usize) -> i64; 3] = [&|_| 0, &|_| 1, &|i| (i % 2) as i64];
                        for select in selects {
                            let mask: Vec<i64> = (0..count).map(select).collect();
                            let expected_planes: Vec<Vec<f64>> = (0..width)
                                .map(|c| {
                                    (0..count)
                                        .map(|i| {
                                            if mask[i] == 1 {
                                                replacements[i * width + c]
                                            } else if mode == "matching" {
                                                old[c][i]
                                            } else {
                                                defaults[c]
                                            }
                                        })
                                        .collect()
                                })
                                .collect();
                            let expected = bits(&with_attr(&original, owner, "selected", pack(width, &expected_planes)));
                            for domains in [1, 8] {
                                on_domains(domains, &|| {
                                    let out = write_selected(97, owner, kind, "selected", &values, &Value::Int_array(Arc::new(mask.clone())), &default, &original).unwrap();
                                    assert_eq!(bits(&out), expected);
                                    assert_eq!(bits(&original), before);
                                });
                            }
                        }
                        let invalid = |selection: Value, default: &Value, values: &Value, want: &str| {
                            assert_eq!(code(write_selected(97, owner, kind, "selected", values, &selection, default, &original)), want);
                            assert_eq!(bits(&original), before);
                        };
                        invalid(Value::Int_array(Arc::new(vec![0; count + 1])), &default, &values, "E_ATTR_COUNT");
                        invalid(Value::Int_array(Arc::new(vec![0; count])), &Value::Text(b"wrong".to_vec()), &values, "E_ATTR_TYPE");
                        if count > 0 {
                            invalid(Value::Int_array(Arc::new(vec![2; count])), &default, &values, "E_ATTR_SELECTION");
                            let mut bad = replacements.clone();
                            bad[0] = f64::NAN;
                            invalid(Value::Int_array(Arc::new(vec![0; count])), &default, &array(width, bad.clone()), "E_NONFINITE");
                            let mut late = vec![0; count];
                            late[count - 1] = -1;
                            invalid(Value::Int_array(Arc::new(late)), &default, &array(width, bad), "E_ATTR_SELECTION");
                        }
                    }
                }
            }
        }
    }

    /// CSR writer finite scan: empty/finite signed-zero owned rows and first/last nonfinite refusals.
    #[test]
    fn csr_writer_finite_scan() {
        let geo = points(1);
        let write = |data: &[f64]| {
            write(97, Owner::Point, "float_array", "csr", &Value::Record(vec![("offsets".into(), Value::Int_array(Arc::new(vec![0, data.len() as i64]))), ("values".into(), float_array(data.to_vec()))]), &geo)
        };
        let data = [-0.0, 0.0, f64::MAX];
        let written = write(&data).unwrap();
        let stored = geometry::find_attribute(Owner::Point, "csr", &written).unwrap();
        match attribute::View::storage(&stored).unwrap() {
            Storage::FloatArray(s) => {
                let s = FloatArray::View::view(s);
                assert_eq!(&*s.offsets, &[0, 3]);
                for (i, x) in s.values.iter().enumerate() {
                    assert_eq!(x.to_bits(), data[i].to_bits());
                }
            }
            _ => panic!("not a CSR attribute"),
        }
        for (index, value) in [(0, f64::NAN), (2, f64::INFINITY), (2, f64::NEG_INFINITY)] {
            let mut invalid = data;
            invalid[index] = value;
            let d = write(&invalid).err().expect("nonfinite refused");
            assert!(d.code == "E_NONFINITE" && d.message == "Attribute values must be finite.");
        }
        assert!(geometry::find_attribute(Owner::Point, "csr", &geo).is_none());
        let empty = write_storage(
            &[],
            97,
            Owner::Point,
            "float_array",
            "csr",
            &Value::Record(vec![("offsets".into(), Value::Int_array(Arc::new(vec![0]))), ("values".into(), float_array(vec![]))]),
            &points(0),
        );
        assert!(empty.is_ok());
    }

    fn input_node(original: &geometry::T) -> node::T {
        let held = original.clone();
        build::make_geometry(
            build::Args { cook_mode: CookMode::Generator, ..build::args("test.xyz_write") },
            None,
            Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: held.clone(), instances: None })),
        )
    }

    fn cook(session: &mut sop::session::T, domains: i64, node: &node::T) -> Result<geometry::T, Diagnostic> {
        let context = context::create(context::Options { domains: Some(domains), grain: Some(257), time: Some(0.0), ..Default::default() }).unwrap();
        sop::session::cook(session, None, &context, node).and_then(|o| sop::payload::geometry(&o.payload))
    }

    /// XYZ attribute writes: empty/multi-block, signed-zero/domain bytes, ownership and error
    /// immutability.
    #[test]
    fn xyz_attribute_writes_by_domain() {
        for count in [0usize, 1, 32769] {
            let original = points(count);
            let before = bits(&original);
            let input = input_node(&original);
            let values: Vec<f64> = (0..count * 3)
                .map(|j| {
                    if j % 17 == 0 {
                        -0.0
                    } else {
                        match j % 3 {
                            0 => (j / 3) as f64 + 0.125,
                            1 => -((j / 3) as f64) - 0.25,
                            _ => (j / 3) as f64 * 0.5 + 0.375,
                        }
                    }
                })
                .collect();
            let mut first: Option<Vec<u64>> = None;
            for domains in [1, 8] {
                let mut session = sop::session::create(32, 64 * 1024 * 1024).unwrap();
                let source = values.clone();
                let node_for = |v: Vec<f64>| node(None, &NodeOptions { source: "xyz-write".into(), ..Default::default() }, "Cd", &Value::Vec3_array(Arc::new(v)), &[0], &[input.clone()]);
                let output = cook(&mut session, domains, &node_for(source.clone())).unwrap();
                let packed = Float3::View::view(&match attribute::storage(&geometry::find_attribute(Owner::Point, "Cd", &output).unwrap()).unwrap() {
                    Storage::Float3(p) => p,
                    _ => panic!("Cd is vec3"),
                })
                .unwrap();
                for i in 0..count {
                    assert_eq!(packed.x[i].to_bits(), values[3 * i].to_bits());
                    assert_eq!(packed.y[i].to_bits(), values[3 * i + 1].to_bits());
                    assert_eq!(packed.z[i].to_bits(), values[3 * i + 2].to_bits());
                }
                let b = bits(&output);
                match &first {
                    None => first = Some(b.clone()),
                    Some(f) => assert_eq!(*f, b),
                }
                assert_eq!(source, values);
                assert!(bits(&output) == b && bits(&original) == before);
                let mut failure = |v: Vec<f64>, want: &str| {
                    let d = cook(&mut session, domains, &node_for(v)).err().expect("write refused");
                    assert_eq!(d.code, want);
                    assert_eq!(bits(&original), before);
                };
                failure(vec![0.0; count * 3 + 3], "E_ATTR_COUNT");
                if count > 0 {
                    let mut bad = values.clone();
                    *bad.last_mut().unwrap() = f64::INFINITY;
                    failure(bad, "E_NONFINITE");
                }
                sop::session::close(&mut session);
            }
        }
    }

    /// Constant storage descriptors: full/selected numeric and exact Int/Text, resolver backstop,
    /// malformed types, at 1/4/8 domains.
    #[test]
    fn constant_storage_descriptors() {
        let geo = no_geometry();
        for count in [0usize, 1, 32769] {
            let original = points(count);
            let before = bits(&original);
            let fills: Vec<(&str, Value)> = vec![
                ("float", Value::Float(-0.0)),
                ("vec3", Value::Vec3(-0.0, 1.0, f64::from_bits(1))),
                ("vec4", Value::Vec4(-0.0, 1.0, 2.0, f64::from_bits(1))),
                ("int", Value::Int(9007199254740993)),
                ("text", Value::Text("猫😀".as_bytes().to_vec())),
            ];
            for (kind, fill) in fills {
                let name = format!("array/{kind}");
                let args = vec![("count".to_owned(), Value::Int(count as i64)), ("value".to_owned(), fill.clone())];
                let marker = op::constant_array_descriptor(&name, &args).unwrap();
                if kind == "float" {
                    let selection = Value::Int_array(Arc::new(vec![1; count]));
                    assert_eq!(code(write_selected(97, Owner::Point, kind, "constant", &marker, &selection, &Value::Float(f64::NAN), &original)), "E_NONFINITE");
                    assert_eq!(bits(&original), before);
                }
                let dense = (op::find(&[], &name, &fctx::value()).unwrap().body)(&input::at_time(0.0), &|_, _| unreachable!(), &args).unwrap();
                let resolved = resolve(None, &geo, &marker).unwrap();
                let key = |v: &Value| V::key_of(&|id| E::Residual::residual_id(id), v);
                assert_eq!(key(&resolved), key(&dense));
                for domains in [1, 4, 8] {
                    on_domains(domains, &|| {
                        let actual = write_constant_source(97, Owner::Point, kind, "constant", None, None, &marker, &original).unwrap().unwrap();
                        let expected = write(97, Owner::Point, kind, "constant", &dense, &original).unwrap();
                        assert_eq!(bits(&actual), bits(&expected));
                        if ["float", "vec3", "vec4"].contains(&kind) {
                            let selects: [&dyn Fn(usize) -> i64; 3] = [&|_| 0, &|_| 1, &|i| (i % 2) as i64];
                            for select in selects {
                                let selection = Value::Int_array(Arc::new((0..count).map(select).collect()));
                                let actual = write_constant_source(97, Owner::Point, kind, "constant", Some(&selection), Some(&fill), &marker, &original).unwrap().unwrap();
                                let expected = write_selected(97, Owner::Point, kind, "constant", &dense, &selection, &fill, &original).unwrap();
                                assert_eq!(bits(&actual), bits(&expected));
                            }
                        }
                        assert_eq!(bits(&original), before);
                    });
                }
            }
        }
        for source in [
            Value::Source("$core/constant-array/array/vec4".into(), array_of(Ty::Int), vec![("count".into(), Value::Int(1)), ("value".into(), Value::Vec4(0.0, 0.0, 0.0, 0.0))]),
            Value::Source("$core/constant-array/array/vec4".into(), array_of(Ty::Vec4), vec![]),
            Value::Source("$core/constant-array/missing".into(), array_of(Ty::Float), vec![("count".into(), Value::Int(1))]),
        ] {
            assert_eq!(code(resolve(None, &geo, &source)), "E_ARRAY_TYPE");
        }
    }

    fn planar_record(planes: &[Vec<f64>]) -> Value {
        Value::Record(planes.iter().enumerate().map(|(i, d)| (["x", "y", "z", "w"][i].to_owned(), float_array(d.clone()))).collect())
    }

    fn planar_oracle(owner: Owner, name: &str, planes: &[Vec<f64>], original: &geometry::T) -> geometry::T {
        let storage = pack(planes.len(), planes);
        if owner == Owner::Point && name == "P" {
            match storage {
                Storage::Float3(p) => geometry::with_positions(p, original).unwrap(),
                _ => panic!("P is vec3"),
            }
        } else {
            with_attr(original, owner, name, storage)
        }
    }

    /// Full planar attributes: all owners/vec widths, native raw payload bytes, capacity type
    /// boundary and exact 1/4/8-domain errors. (OCaml's physical-identity asserts on the Flow
    /// planes have no Rust meaning: Flow arrays are `Arc<Vec>`, geometry planes are `Arc<[f64]>`.)
    #[test]
    fn full_planar_attributes() {
        for n in [0usize, 1, 6, 32769] {
            let mut base = points(n);
            if n == 6 {
                let t = topology::create_owned(6, vec![0, 1, 2, 3, 4, 5], vec![0, 3, 6], vec![topology::PrimitiveKind::Polygon, topology::PrimitiveKind::OpenPolyline]).unwrap();
                base = geometry::create(geometry::positions(&base).clone(), t, None, None, None).unwrap();
            }
            let raw = attribute::create_owned("untouched", Owner::Detail, Storage::Float(vec![f64::from_bits(0x7ff8000000001234)])).unwrap();
            let text = attribute::create_owned("bytes", Owner::Detail, Storage::Text(vec!["é".to_owned()])).unwrap();
            let matrix = FloatArray::create_owned(vec![0, 16], (0..16).map(|i| if i == 0 { -0.0 } else { i as f64 }).collect()).unwrap();
            let mut original = base;
            for a in [raw, text, attribute::create_owned("matrix", Owner::Detail, Storage::FloatArray(matrix)).unwrap()] {
                original = geometry::with_attribute(a, &original).unwrap();
            }
            let before = bits(&original);
            let owners = [
                (Owner::Point, "point", n),
                (Owner::Vertex, "vertex", geometry::vertex_count(&original) as usize),
                (Owner::Primitive, "primitive", geometry::primitive_count(&original) as usize),
                (Owner::Detail, "detail", 1),
            ];
            for (owner, owner_label, count) in owners {
                for (kind, width) in [("vec2", 2usize), ("vec3", 3), ("vec4", 4)] {
                    let data: Vec<Vec<f64>> = (0..width)
                        .map(|c| (0..count).map(|i| if i % 3 == 0 { -0.0 } else if i % 3 == 1 { f64::from_bits(1) } else { c as f64 + i as f64 * 0.125 }).collect())
                        .collect();
                    let values = planar_record(&data);
                    let expected = bits(&planar_oracle(owner, "planes", &data, &original));
                    let grains: &[i64] = if count > 6 { &[97, 4096] } else { &[1, 97, 4096] };
                    for domains in [1, 4, 8] {
                        for grain in grains {
                            on_domains(domains, &|| {
                                let output = write(*grain, owner, kind, "planes", &values, &original).unwrap();
                                assert!(bits(&output) == expected && bits(&original) == before);
                            });
                        }
                    }
                    let error = |values: &Value, want: &str, message: Option<String>| {
                        let d = write(97, owner, kind, "bad", values, &original).err().expect("refused");
                        assert_eq!(d.code, want);
                        if let Some(m) = message {
                            assert_eq!(d.message, m);
                        }
                    };
                    let Value::Record(fields) = &values else { unreachable!() };
                    let rest: Vec<_> = fields[1..].to_vec();
                    let mut extra = vec![("extra".to_owned(), float_array(vec![]))];
                    extra.extend(fields.clone());
                    let mut dup = vec![fields[0].clone()];
                    dup.extend(fields.clone());
                    let mut int_x = vec![("x".to_owned(), Value::Int_array(Arc::new(vec![0; count])))];
                    int_x.extend(fields[1..].iter().cloned());
                    let mut vec3_x = vec![("x".to_owned(), Value::Vec3_array(Arc::new(vec![])))];
                    vec3_x.extend(fields[1..].iter().cloned());
                    for f in [rest, extra, dup, int_x, vec3_x] {
                        error(&Value::Record(f), "E_ATTR_TYPE", None);
                    }
                    let mut bad = data.clone();
                    bad[0] = vec![f64::NAN; count + 1];
                    error(&planar_record(&bad), "E_ATTR_COUNT", Some(format!("bad needs {count} {owner_label} values; got {}.", count + 1)));
                    if count > 0 {
                        let mut bad = data.clone();
                        bad[width - 1][count - 1] = f64::INFINITY;
                        error(&planar_record(&bad), "E_NONFINITE", Some("Attribute values must be finite.".to_owned()));
                        let alias = planar_record(&vec![data[0].clone(); width]);
                        let output = write(97, owner, kind, "aliased", &alias, &original).unwrap();
assert_eq!(bits(&output), bits(&with_attr(&original, owner, "aliased", pack(width, &vec![data[0].clone(); width]))));
                    }
                    if owner == Owner::Point {
                        if width == 3 {
                            assert_eq!(bits(&write(97, owner, kind, "P", &values, &original).unwrap()), bits(&planar_oracle(owner, "P", &data, &original)));
                        } else {
                            let d = write(97, owner, kind, "P", &values, &original).err().expect("P is vec3");
                            assert!(d.code == "E_ATTR_TYPE" && d.message == "Canonical point P requires vec3 storage.");
                        }
                    }
                    let default = match width {
                        2 => Value::Vec2(0.0, 0.0),
                        3 => Value::Vec3(0.0, 0.0, 0.0),
                        _ => Value::Vec4(0.0, 0.0, 0.0, 0.0),
                    };
                    let selection = Value::Int_array(Arc::new(vec![1; count]));
                    assert_eq!(code(write_selected(97, owner, kind, "planes", &values, &selection, &default, &original)), "E_ATTR_TYPE");
                    assert_eq!(bits(&original), before);
                }
            }
        }
    }

    use flow_check::catalog::Catalog;
    use flow_syntax::syntax;

    fn fixture(index: usize) -> geometry::T {
        let make = |count: i64, vertices: Vec<i64>, offsets: Vec<i64>, kinds: Vec<topology::PrimitiveKind>| {
            let t = topology::create_owned(count, vertices, offsets, kinds).unwrap();
            let x: Vec<f64> = (0..count).map(|i| i as f64).collect();
            let y: Vec<f64> = (0..count).map(|i| if i % 2 == 0 { -0.0 } else { 0.25 }).collect();
            geometry::create(Float3::of_owned(x, y, vec![2.0; count as usize]).unwrap(), t, None, None, None).unwrap()
        };
        use topology::PrimitiveKind::{ClosedPolyline, OpenPolyline, Polygon};
        match index {
            1 => make(5, vec![0, 1, 2, 2, 1, 3], vec![0, 3, 6], vec![Polygon, Polygon]),
            2 => make(3, vec![0, 1, 1, 2, 1], vec![0, 5], vec![OpenPolyline]),
            _ => make(4, vec![3, 2, 1, 0], vec![0, 4], vec![ClosedPolyline]),
        }
    }

    fn evaluate(expression: &str) -> Value {
        let source = format!("(workspace query (graph g :context sop [(geo : geometry nil)] (let* [tested {expression}] geo)))");
        let forms = syntax::parse(&source).unwrap();
        let (checked, ds) = W::check(crate::operators::all(), false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms);
        let checked = checked.unwrap_or_else(|| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")));
        let inputs = vec![("g".to_owned(), vec![("geo".to_owned(), Value::Deferred(ty::geometry(), 7))])];
        let evaluation = E::r#static(&checked, true, Some(&inputs)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        evaluation.records.iter().find(|(p, _)| p == &vec!["g".to_owned(), "tested".to_owned()]).expect("tested record").1[0].1.clone()
    }

    /// OCaml `fields` of a geometry without its points: the topology planes as Flow arrays.
    fn topology_fields(g: &geometry::T) -> Vec<(String, Value)> {
        let view = topology::View::view(geometry::topology(g)).unwrap();
        vec![
            ("vertex_points".to_owned(), Value::Int_array(Arc::new(view.vertex_points.to_vec()))),
            ("primitive_offsets".to_owned(), Value::Int_array(Arc::new(view.primitive_offsets.to_vec()))),
            ("kinds".to_owned(), Value::Int_array(Arc::new(view.primitive_kinds.iter().map(|k| i64::from(*k)).collect()))),
        ]
    }

    fn resolver(original: geometry::T) -> flow_ir::core_exec::ResolveFn {
        let lookup: GeometryLookup = Arc::new(move |id| (id == 7).then(|| original.clone()));
        Arc::new(move |v| resolve(None, &lookup, v))
    }

    /// Topology queries resolve to the planes in order, readers leave the source alone, captured
    /// and direct counts have the same, distinct origins, and the compiled form equals the reference.
    #[test]
    fn topology_queries_origins_and_readers() {
        let original = fixture(1);
        let resolve_fn = resolver(original.clone());
        let queried = evaluate("(sop/topology geo)");
        let live = input::at_time(0.0);
        let expected = Value::Record(topology_fields(&original));
        let result = E::Host::force_reference(&queried, &live, None, None, Some(resolve_fn.clone())).unwrap();
        assert_eq!(result, expected, "topology kind codes or plane order differ");
        let before = bits(&original);
        for read in [
            "(map (fn [(i : int)] (int/add i 1)) topo.vertex_points)",
            "(array/grade topo.vertex_points)",
            "(array/scan_sum topo.vertex_points)",
            "(array/slice topo.primitive_offsets 0 1)",
            "(array/concat topo.vertex_points topo.primitive_offsets)",
            "(array/compact (map (fn [(i : int)] (if (= i 0) 1 0)) topo.vertex_points))",
            "(array/segment_sum topo.vertex_points topo.primitive_offsets)",
            "(array/scatter_add topo.vertex_points topo.vertex_points 64)",
            "(array/sum topo.vertex_points)",
        ] {
            let expression = format!("(let* [topo (sop/topology geo)] {read})");
            E::Host::force_reference(&evaluate(&expression), &live, None, None, Some(resolve_fn.clone())).unwrap();
            assert_eq!(bits(&original), before, "topology reader wrote its source: {read}");
        }
        let source = input_node(&original);
        let mut origins = Vec::new();
        for component in ["vertex_points", "primitive_offsets", "kinds"] {
            let captured = format!("(let* [topology (sop/topology geo)] (map (fn [(i : int)] i) topology.{component}))");
            let direct = format!("(map (fn [(i : int)] i) (get (sop/topology geo) :{component}))");
            let origin = |expression: &str| {
                let values = evaluate(expression);
                assert_eq!(sources(&values), vec![7], "topology capture lost its geometry dependency");
                let program = prepare(None, true, None, None, &[7], &[source.clone()], &values).unwrap();
                let packed = Executor::packed_region(&program).expect("packed region");
                flow_ir::packed::count_origin(&packed).unwrap_or_else(|| panic!("topology count origin missing for {expression}"))
            };
            let (captured, direct) = (origin(&captured), origin(&direct));
            assert_eq!(captured, direct, "direct topology Get and captured field have different origins");
            origins.push(captured);
        }
        origins.sort();
        origins.dedup();
        assert_eq!(origins.len(), 3, "distinct topology plane counts share a false origin");
        for domains in [1, 8] {
            parallel::run_unit(Some(domains), &|| {
                let program = Executor::compile(&queried, &Executor::CompileOptions { strict: true, ..Default::default() }).unwrap();
                let forced = Executor::force(&program, &Executor::ForceOptions { strict: true, resolve: Some(resolve_fn.clone()), ..Default::default() }, &live).unwrap();
                assert_eq!(forced, expected, "compiled topology differs from reference");
                Ok(())
            })
            .unwrap();
        }
    }

    /// A topology-only capture reads no attribute, and its cache survives position and unrelated
    /// attribute changes but not layout or topology changes.
    #[test]
    fn topology_capture_reads_nothing_and_caches() {
        let mut input = fixture(1);
        let n = geometry::point_count(&input) as usize;
        input = with_attr(&input, Owner::Point, "unrelated", Storage::Float(vec![0.0; n]));
        let value = evaluate("(array/int 1 (array/count (get (sop/topology geo) :kinds)))");
        let kernel = node(
            None,
            &NodeOptions { strict: true, owner: Owner::Detail, kind: "int".into(), source: "C1 topology".into(), ..Default::default() },
            "primitive_count",
            &value,
            &[7],
            &[input_node(&input)],
        );
        assert!(node::facts(&kernel).reads.is_empty(), "topology-only capture reads unrelated attributes");
        let mut session = sop::session::create(32, 64 * 1024 * 1024).unwrap();
        cook_ok(&mut session, &kernel);
        let positions = Float3::of_owned(vec![8.0; n], vec![4.0; n], vec![2.0; n]).unwrap();
        let changed = geometry::with_positions(positions.clone(), &input).unwrap();
        let changed = with_attr(&changed, Owner::Point, "unrelated", Storage::Float(vec![1.0; n]));
        let source = input_node(&changed);
        cook_ok(&mut session, &source);
        let before = sop::session::stats(&session);
        let rebuilt = |source: &node::T| build::adopt_identity(&kernel, &build::rebuild_with_inputs(&kernel, std::slice::from_ref(source)));
        let output = cook_ok(&mut session, &rebuilt(&source));
        assert_eq!(sop::session::stats(&session).misses, before.misses, "topology capture missed for position/attribute-only change");
        assert_eq!(Float3::data_id(geometry::positions(&output)), Float3::data_id(&positions), "cached capture failed to carry fresh positions");
        match attribute::storage(&geometry::find_attribute(Owner::Point, "unrelated", &output).unwrap()).unwrap() {
            Storage::Float(v) => assert_eq!(v, vec![1.0; n], "cached capture failed to carry fresh unread attribute values"),
            _ => panic!("unrelated is float"),
        }
        let changed = with_attr(&changed, Owner::Point, "added", Storage::Float(vec![2.0; n]));
        let source = input_node(&changed);
        cook_ok(&mut session, &source);
        let before = sop::session::stats(&session);
        cook_ok(&mut session, &rebuilt(&source));
        assert_eq!(sop::session::stats(&session).misses, before.misses + 1, "changed attribute layout failed to invalidate the write cache");
        let other = fixture(2);
        let before = sop::session::stats(&session);
        let output = cook_ok(&mut session, &rebuilt(&input_node(&other)));
        assert!(sop::session::stats(&session).misses > before.misses, "topology change did not invalidate query");
        match attribute::storage(&geometry::find_attribute(Owner::Detail, "primitive_count", &output).unwrap()).unwrap() {
            Storage::Int(v) => assert_eq!(v, vec![1], "topology query carried stale count"),
            _ => panic!("primitive_count is int"),
        }
        let _ = line_geometry::points;
    }

    fn cook_ok(session: &mut sop::session::T, n: &node::T) -> geometry::T {
        let context = context::create(context::Options { domains: Some(1), grain: Some(97), time: Some(0.0), ..Default::default() }).unwrap();
        sop::session::cook(session, None, &context, n).and_then(|o| sop::payload::geometry(&o.payload)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
    }

    /// An unavailable geometry (missing, wrong type or not geometry) is E_DATA_SOURCE.
    #[test]
    fn missing_geometry_source_is_refused() {
        let lookup: GeometryLookup = Arc::new(|_| None);
        for geometry in [Value::No_geo, Value::Deferred(ty::geometry(), 7), Value::Deferred(Ty::Float, 7)] {
            let source = Value::Source("sop/topology".into(), array_of(Ty::Int), vec![("geometry".into(), geometry), ("component".into(), Value::Text(b"vertex_points".to_vec()))]);
            assert_eq!(code(resolve(None, &lookup, &source)), "E_DATA_SOURCE");
        }
    }



    fn wide_bits(v: &Value) -> Vec<u64> {
        match v {
            Value::Vec3_array(d) | Value::Float_array(d) => d.iter().map(|x| x.to_bits()).collect(),
            _ => panic!("a float array"),
        }
    }

    fn attr_source(name: &str, attribute: &str, extra: &[(&str, Value)]) -> Value {
        let mut args = vec![("geometry".to_owned(), Value::Deferred(ty::geometry(), 7)), ("attribute".to_owned(), Value::Text(attribute.as_bytes().to_vec()))];
        args.extend(extra.iter().map(|(k, v)| ((*k).to_owned(), v.clone())));
        Value::Source(name.to_owned(), array_of(Ty::Vec3), args)
    }

    /// `sources` and `reads` of plain source values: named attributes, topology-only readers and
    /// opaque geometry.
    #[test]
    fn sources_and_reads_of_values() {
        let value = Value::Record(vec![("a".into(), attr_source("sop/attr", "P", &[])), ("b".into(), Value::Deferred(ty::geometry(), 3))]);
        assert_eq!(sources(&value), vec![3, 7]);
        assert_eq!(reads(&value), vec!["*".to_owned()], "a bare geometry is opaque");
        let named = Value::List(vec![attr_source("sop/attr", "P", &[]), attr_source("sop/attr_checked", "Cd", &[]), attr_source("sop/attr", "P", &[])]);
        assert_eq!(reads(&named), vec!["Cd".to_owned(), "P".to_owned()]);
        let topology = Value::Source("sop/count".into(), Ty::Int, vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("element".into(), Value::Text(b"points".to_vec()))]);
        assert!(reads(&topology).is_empty(), "topology-only readers read no attribute");
        let names = Value::Source("sop/attr_names".into(), array_of(Ty::Text), vec![("geometry".into(), Value::Deferred(ty::geometry(), 7))]);
        assert_eq!(reads(&names), vec!["*".to_owned()]);
        assert!(sources(&Value::Int(1)).is_empty());
    }

    /// Attribute reads resolve to interleaved arrays whose bytes do not depend on the domain count;
    /// checked reads zero nonfinite rows; planes read per component.
    #[test]
    fn attribute_reads_resolve_deterministically() {
        for count in [0usize, 5, 32769] {
            let x: Vec<f64> = (0..count).map(|i| if i % 7 == 3 { f64::NAN } else { i as f64 }).collect();
            let y: Vec<f64> = (0..count).map(|i| -(i as f64)).collect();
            let z: Vec<f64> = (0..count).map(|_| -0.0).collect();
            let g = geometry::create(Float3::of_owned(x, y.clone(), z).unwrap(), topology::empty(count as i64).unwrap(), None, None, None).unwrap();
            let lookup: GeometryLookup = {
                let g = g.clone();
                Arc::new(move |id| (id == 7).then(|| g.clone()))
            };
            let read = |value: &Value| resolve(None, &lookup, value).unwrap();
            let mut first: Option<Value> = None;
            for domains in [1, 8] {
                let out = Mutex::new(None);
                parallel::run_unit(Some(domains), &|| {
                    *out.lock().unwrap() = Some(read(&attr_source("sop/attr", "P", &[])));
                    Ok(())
                })
                .unwrap();
                let value = out.into_inner().unwrap().unwrap();
                match &first {
                    None => first = Some(value),
                    Some(f) => assert_eq!(wide_bits(f), wide_bits(&value)),
                }
            }
            match first.unwrap() {
                Value::Vec3_array(data) => {
                    assert_eq!(data.len(), count * 3);
                    for i in 0..count {
                        assert_eq!(data[3 * i + 1].to_bits(), y[i].to_bits());
                        assert_eq!(data[3 * i + 2].to_bits(), (-0.0f64).to_bits());
                    }
                }
                other => panic!("P reads as a vec3 array, got {other:?}"),
            }
            let component = |name: &str| Value::Text(name.as_bytes().to_vec());
            match read(&attr_source("sop/attr_checked", "P", &[("component", component("valid"))])) {
                Value::Int_array(valid) => {
                    assert_eq!(valid.len(), count);
                    for i in 0..count {
                        assert_eq!(valid[i], i64::from(i % 7 != 3));
                    }
                }
                other => panic!("valid reads as ints, got {other:?}"),
            }
            match read(&attr_source("sop/attr_checked", "P", &[("component", component("values"))])) {
                Value::Vec3_array(v) => assert!(v.iter().all(|x| x.is_finite())),
                other => panic!("values read as vec3, got {other:?}"),
            }
            assert_eq!(read(&attr_source("sop/attr_planes", "P", &[("component", component("width"))])), Value::Int(3));
            match read(&attr_source("sop/attr_planes", "P", &[("component", component("y"))])) {
                Value::Float_array(p) => assert_eq!(&p[..], &y[..]),
                other => panic!("plane reads as floats, got {other:?}"),
            }
            assert_eq!(read(&attr_source("sop/has_attr", "P", &[])), Value::Bool(true));
            assert_eq!(read(&attr_source("sop/has_attr", "missing", &[])), Value::Bool(false));
            assert_eq!(code(resolve(None, &lookup, &attr_source("sop/attr_checked", "P", &[("component", component("nope"))]))), "E_ATTR_TYPE");
            assert_eq!(code(resolve(None, &lookup, &attr_source("sop/attr", "missing", &[]))), "E_ATTR_TYPE");
            assert_eq!(code(resolve(None, &lookup, &attr_source("sop/attr_planes", "P", &[("component", component("q"))]))), "E_ATTR_TYPE");
        }
    }

    /// Context sources resolve from the cook context and specialize into constants.
    #[test]
    fn context_sources_specialize() {
        let context = context::create(context::Options { frame: Some(5), time: Some(1.5), seed: Some(42), ..Default::default() }).unwrap();
        let lookup = no_geometry();
        let source = |name: &str, extra: &[(&str, Value)]| Value::Source(name.to_owned(), Ty::Float, extra.iter().map(|(k, v)| ((*k).to_owned(), v.clone())).collect());
        assert_eq!(resolve(Some(&context), &lookup, &source("sop/context_frame", &[])).unwrap(), Value::Float(5.0));
        assert_eq!(resolve(Some(&context), &lookup, &source("sop/context_time", &[])).unwrap(), Value::Float(1.5));
        let seeded = source("sop/context_seed", &[("namespace", Value::Text(b"n".to_vec())), ("salt", Value::Int(3))]);
        let expected = seed_for(3, 42, "n", "");
        assert_eq!(resolve(Some(&context), &lookup, &seeded).unwrap(), Value::Int(expected));
        let record = Value::Record(vec![("t".into(), source("sop/context_time", &[])), ("k".into(), Value::Int(1))]);
        assert_eq!(specialize(&context, &record), Value::Record(vec![("t".into(), Value::Float(1.5)), ("k".into(), Value::Int(1))]));
        assert_eq!(context_reads(&record), D::one(D::Fact::Time));
        assert_eq!(D::to_list(context_reads(&seeded)), vec![D::Fact::Seed]);
        // the salt is the low 63 bits of a word
        assert_eq!(seed_for(-1, 7, "a", "b"), seed_for(i64::MAX, 7, "a", "b"));
        assert!(seed_for(0, 1, "x", "y") != seed_for(0, 2, "x", "y"));
    }

    /// The measured display sinks: keyed by backend, dropped with it, priced from the two nearest
    /// samples, with a bounded count of device-first cooks per site.
    #[test]
    fn display_sink_prices_and_forgets() {
        assert_eq!(Display_sink::priced(&[(100, 2.0)], 10), None);
        // the two samples nearest 400 are 100 and 1000; the cheaper per-unit rate wins
        assert_eq!(Display_sink::priced(&[(100, 3.0), (1000, 2.0), (1_000_000, 0.5)], 400), Some(2.0 * 400.0));
        let backend = Arc::new(Gpu::Backend {
            residency: Box::new(|_| None),
            cost: Box::new(|_, _, _, _| None),
            prepare: Box::new(|_, _| Err(fail("E_TEST", "no kernel"))),
        });
        let live = Display_sink::live();
        assert_eq!(Display_sink::rates(&backend), (f64::INFINITY, f64::INFINITY));
        Display_sink::observe(&backend, false, 100, 1.0);
        Display_sink::observe(&backend, false, 100, 2.0);
        Display_sink::observe(&backend, true, 50, 1.0);
        assert_eq!(Display_sink::rates(&backend), (0.01, 0.02));
        Display_sink::observe_draw(&backend, 100, 0.5);
        assert_eq!(Display_sink::rates(&backend).0, 0.01 + 0.005);
        assert!(Display_sink::device_first(&backend, "site", None));
        assert!(Display_sink::device_first(&backend, "site", Some(1.0)));
        assert!(Display_sink::device_first(&backend, "site", Some(1.0)));
        assert!(!Display_sink::device_first(&backend, "site", Some(1.0)), "the third priced cook samples the CPU");
        assert_eq!(Display_sink::cpu_write(&backend, "site", 10), None);
        Display_sink::observe_cpu_write(&backend, "site", 10, 1.0);
        Display_sink::observe_cpu_write(&backend, "site", 20, 4.0);
        assert_eq!(Display_sink::cpu_write(&backend, "site", 10), Some(1.0));
        assert_eq!(Display_sink::live(), live + 1);
        drop(backend);
        assert_eq!(Display_sink::live(), live);
    }
}
