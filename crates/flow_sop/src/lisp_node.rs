//! A checked Lisp geometry body owns its schema and cook identity and cooks in its own session. Cooked inputs and pending initial changes are thread-local; nested bodies use the same cook thread.
use flow_syntax::symbol::Symbol;
use flow_eval::{self as E, Value};
use flow_ir::flow_ir::executor;
use flow_syntax::{ty::Ty, value};
use param::{Field, FieldView, Kind, KindView, Schema, Value as ParamValue};
use rays_diagnostic::Diagnostic;
use sop::context::{self, dependencies};
use sop::custom;
use sop::node::{self, build, Cooked, Facts};
use sop::payload;
use sop::session;
use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use crate::encoded_parameters::Normalizer;
use crate::port::{self, Literal};

thread_local! {
    static COOKED_INPUTS: RefCell<Vec<(usize, payload::T)>> = const { RefCell::new(Vec::new()) };
    static INITIAL: RefCell<Option<(String, Vec<(String, ParamValue)>, Vec<String>)>> = const { RefCell::new(None) };
    static ARGUMENTS: RefCell<ArgumentBindings> = RefCell::new(Arc::new(Vec::new()));
    static BODY_CACHE: RefCell<Option<BodyCache>> = const { RefCell::new(None) };
    static INVOCATION: RefCell<Option<Invocation>> = const { RefCell::new(None) };
}

/// Immutable cooked input in the enclosing body's caller-domain scope.
pub fn input(index: usize) -> Result<payload::T, Diagnostic> {
    COOKED_INPUTS.with(|inputs| {
        inputs.borrow().iter().find(|(i, _)| *i == index).map(|(_, p)| p.clone())
    })
    .ok_or_else(|| port::error("E_DATA_SOURCE", "A Lisp body input is only available while its node cooks"))
}

fn invalid(name: &str) -> ! {
    unreachable!("Invalid Lisp parameter field {name}")
}

fn field_of(f: &FieldView, normalizers: &[(String, Normalizer)]) -> Field {
    let kind = match (&f.kind, &f.default) {
        (KindView::Toggle, ParamValue::Bool(_)) => Kind::Toggle,
        (KindView::Integer(range), ParamValue::Int(_)) => Kind::Integer(*range),
        (KindView::Floating(range), ParamValue::Float(_)) => Kind::Floating(*range),
        (KindView::Text, ParamValue::Text(_)) => match normalizers.iter().find(|(name, _)| *name == f.name) {
            None => Kind::Text,
            Some((_, normalize)) => {
                let normalize = normalize.clone();
                Kind::Encoded {
                    equal: Box::new(|a, b| a == b),
                    normalize: Box::new(move |raw| normalize(raw).map_err(|d| d.message)),
                }
            }
        },
        (KindView::Choice(labels), ParamValue::Choice(_)) => {
            Kind::Choice { options: labels.clone() }
        }
        _ => invalid(&f.name),
    };
    let mut field = param::field(f.name.clone(), kind, f.default.clone())
        .and_then(|field| field.label(f.label.clone()))
        .and_then(|field| field.folder(f.folder.clone()))
        .unwrap_or_else(|_| invalid(&f.name));
    if let Some(description) = &f.description {
        field = field.description(description.clone());
    }
    field = field.impact(f.impact);
    field = field.primary(f.primary);
    if let Some(unit) = &f.unit {
        field = field.unit(unit.clone());
    }
    if let Some((group, component)) = &f.vec3 {
        field = field.vec3(group.clone(), *component).unwrap_or_else(|_| invalid(&f.name));
    }
    field
}

/// The node's parameter schema: fields, hard ranges and encoded text through the normalizers.
pub fn schema(name: &str, fields: &[FieldView], normalizers: &[(String, Normalizer)]) -> Schema {
    let fields: Vec<Field> = fields.iter().map(|f| field_of(f, normalizers)).collect();
    param::schema(name.to_owned(), fields).unwrap_or_else(|message| unreachable!("Lisp parameter schema: {message}"))
}

fn scalar(value: &ParamValue) -> Value {
    match value {
        ParamValue::Bool(v) => Value::Bool(*v),
        ParamValue::Int(v) => Value::Int(*v),
        ParamValue::Float(v) => Value::Float(*v),
        ParamValue::Text(v) => Value::Text(v.clone()),
        ParamValue::Choice(v) => Value::Text(v.clone().into_bytes().into()),
    }
}

/// Body arguments of schema values (positional, one per field): vec3 groups and their components.
pub fn arguments(fields: &[FieldView], values: &[ParamValue]) -> Result<Vec<(Symbol, Value)>, String> {
    let current: Vec<FieldView> = fields
        .iter()
        .zip(values)
        .map(|(f, value)| FieldView { current: value.clone(), ..f.clone() })
        .collect();
    let ports = port::parameters(false, &current).map_err(|d| rays_diagnostic::to_string(&d))?;
    let mut out: Vec<(Symbol, Value)> = ports
        .iter()
        .map(|p| {
            (
                p.path.as_str().into(),
                match port::literal(p) {
                    Literal::Vector(x, y, z) => Value::Vec3(x, y, z),
                    Literal::Scalar(v) => scalar(&v),
                },
            )
        })
        .collect();
    out.extend(current.iter().filter(|f| f.vec3.is_some()).map(|f| (f.name.as_str().into(), scalar(&f.current))));
    Ok(out)
}

/// Run an instantiation whose first Lisp node of `operation` starts from these parameter changes
/// instead of its defaults, so a node that is about to be edited is built once, not twice.
pub fn with_initial_changes(
    operation: &str,
    changes: &[(String, ParamValue)],
    run: &mut dyn FnMut() -> Result<node::T, String>,
) -> Result<node::T, String> {
    with_driven_initial_changes(operation, changes, &[], run)
}

/// As `with_initial_changes`; `driven` names the fields a live expression sets on every cook
/// (the clock's value lane): the node binds them as cook-time slots from its first build.
pub fn with_driven_initial_changes(
    operation: &str,
    changes: &[(String, ParamValue)],
    driven: &[String],
    run: &mut dyn FnMut() -> Result<node::T, String>,
) -> Result<node::T, String> {
    if changes.is_empty() && driven.is_empty() {
        return run();
    }
    INITIAL.with(|initial| *initial.borrow_mut() = Some((operation.to_owned(), changes.to_vec(), driven.to_vec())));
    let result = run();
    INITIAL.with(|initial| *initial.borrow_mut() = None);
    result
}

/// A body: cooked in the node's own session to the node that produces the payload.
pub type Body = Arc<dyn Fn(&context::T) -> Result<node::T, Diagnostic> + Send + Sync>;

/// A built body with its dependencies and facts.
pub type Built = (Body, dependencies::T, Facts);

/// `build ~label ~context inputs arguments`: the body of the node for these arguments.
pub type BuildFn = Arc<dyn Fn(&str, &context::T, &[node::T], &[(Symbol, Value)]) -> Result<Built, String> + Send + Sync>;

pub struct NodeOptions {
    pub kind: String,
    pub unbound_function: bool,
    /// Per connected input, a slot that must carry geometry: a cook that finds another payload
    /// there fails with `E_PAYLOAD` before the body runs, as a native geometry node does.
    pub geometry_inputs: Vec<bool>,
    pub operation: String,
    pub version: i64,
    pub label: String,
    pub fields: Vec<FieldView>,
    pub normalizers: Vec<(String, Normalizer)>,
    pub source: String,
}

type ArgumentBindings = Arc<Vec<(Value, Value)>>;
type BodyCache = Arc<Mutex<Option<Arc<Prepared>>>>;
type Invocation = (Arc<str>, Arc<Vec<(Symbol, Value)>>, Arc<Vec<(Symbol, Value)>>);

struct InputsScope {
    previous: Option<Vec<(usize, payload::T)>>,
    arguments: Option<ArgumentBindings>,
    cache: Option<Option<BodyCache>>,
    invocation: Option<Option<Invocation>>,
}

impl Drop for InputsScope {
    fn drop(&mut self) {
        if let Some(previous) = self.previous.take() {
            COOKED_INPUTS.with(|inputs| *inputs.borrow_mut() = previous);
        }
        if let Some(previous) = self.arguments.take() {
            ARGUMENTS.with(|arguments| *arguments.borrow_mut() = previous);
        }
        if let Some(previous) = self.cache.take() {
            BODY_CACHE.with(|cache| *cache.borrow_mut() = previous);
        }
        if let Some(previous) = self.invocation.take() {
            INVOCATION.with(|invocation| *invocation.borrow_mut() = previous);
        }
    }
}

fn argument_scope(kind: Arc<str>, slots: Arc<Vec<(Symbol, Value)>>, arguments: Arc<Vec<(Symbol, Value)>>, cache: BodyCache) -> InputsScope {
    let previous = ARGUMENTS.with(|active| {
        let mut active = active.borrow_mut();
        let mut next = (**active).clone();
        for (name, slot) in slots.iter() {
            if let Some((_, value)) = arguments.iter().find(|(field, _)| field == name) {
                next.push((slot.clone(), value.clone()));
            }
        }
        std::mem::replace(&mut *active, Arc::new(next))
    });
    let cache = BODY_CACHE.with(|active| active.borrow_mut().replace(cache));
    let invocation = INVOCATION.with(|active| active.borrow_mut().replace((kind, arguments, slots)));
    InputsScope { previous: None, arguments: Some(previous), cache: Some(cache), invocation: Some(invocation) }
}

/// Parameters have already passed the owning custom node's schema. The slot retains its owner,
/// so a nested node with the same field name cannot capture the enclosing node's current values.
pub(crate) fn parameter_values(arguments: &[(Symbol, Value)]) -> Vec<(Symbol, Value)> {
    let invocation = INVOCATION.with(|active| active.borrow().clone());
    let Some((_, _, slots)) = invocation.filter(|(_, actual, _)| actual.len() == arguments.len() && actual.as_ptr() == arguments.as_ptr()) else {
        return arguments.to_vec();
    };
    arguments.iter().map(|(name, value)| (*name, slots.iter().find(|(field, _)| field == name).map_or_else(|| value.clone(), |(_, slot)| slot.clone()))).collect()
}

pub(crate) fn owns_parameter(kind: &str, name: Symbol, value: &Value) -> bool {
    let Value::Source(source) = value else { return false };
    INVOCATION.with(|active| active.borrow().as_ref().is_some_and(|(owner, _, slots)| owner.as_ref() == kind
        && slots.iter().any(|(field, slot)| *field == name && matches!(slot, Value::Source(candidate) if Arc::ptr_eq(source, candidate)))))
}

pub(crate) fn parameter_component(kind: &str, group: Symbol, value: &Value, field: &str) -> Option<Value> {
    if !matches!(value, Value::Source(source) if source.ty == Ty::Vec3) || !owns_parameter(kind, group, value) { return None; }
    INVOCATION.with(|active| active.borrow().as_ref().and_then(|(_, _, slots)|
        slots.iter().find(|(name, _)| *name == field).map(|(_, value)| value.clone())))
}

pub(crate) fn is_parameter(value: &Value) -> bool {
    matches!(value, Value::Source(g) if g.name == "$lisp/parameter" && g.fields.len() == 1
        && matches!(g.fields.iter().next(), Some((_, Value::No_geo)))
        && matches!(g.ty, Ty::Bool | Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4))
}

pub(crate) fn resolve_parameter(value: &Value) -> Option<Result<Value, Diagnostic>> {
    if !is_parameter(value) { return None; }
    let Value::Source(source) = value else { unreachable!() };
    Some(ARGUMENTS.with(|active| {
        let active = active.borrow();
        let value = active.iter().rev().find_map(|(slot, value)| match slot {
            Value::Source(candidate) if Arc::ptr_eq(source, candidate) => Some(value.clone()),
            _ => None,
        }).ok_or_else(|| port::error("E_DATA_SOURCE", "A Lisp parameter is only available in its owning cook"))?;
        if value::ty_of(&value) != source.ty {
            return Err(port::error("E_DATA_SOURCE", "A Lisp parameter has the wrong type"));
        }
        Ok(value)
    }))
}

/// Capture on the caller domain. A worker installs this immutable snapshot only while resolving;
/// it never reads another cook's ambient parameter state.
pub(crate) fn resolver(fallback: flow_ir::core_exec::ResolveFn) -> flow_ir::core_exec::ResolveFn {
    let arguments = ARGUMENTS.with(|active| active.borrow().clone());
    if arguments.is_empty() { return fallback; }
    Arc::new(move |value| {
        let previous = ARGUMENTS.with(|active| std::mem::replace(&mut *active.borrow_mut(), arguments.clone()));
        let _scope = InputsScope { previous: None, arguments: Some(previous), cache: None, invocation: None };
        match resolve_parameter(value) { Some(value) => value, None => fallback(value) }
    })
}

/// Header validation precedes this lookup in build_call. Only exact construction projections
/// invalidate the retained body; ordinary parameter and context values remain cook-time sources.
pub(crate) fn cached_body(key: Vec<u8>, context: &context::T, build: &mut dyn FnMut() -> Result<Built, String>) -> Result<Built, String> {
    let Some(cache) = BODY_CACHE.with(|active| active.borrow().clone()) else { return build(); };
    let previous = cache.lock().unwrap_or_else(std::sync::PoisonError::into_inner).clone();
    let resolve = {
        let context = context.clone();
        let empty: crate::attribute_kernel::GeometryLookup = Arc::new(|_| None);
        resolver(Arc::new(move |value| crate::attribute_kernel::resolve(Some(&context), &empty, value)))
    };
    if let Some(previous) = previous.filter(|previous| previous.0 == key) {
        let mut same = true;
        for (program, expected) in &previous.1 {
            let actual = executor::force(program, &executor::ForceOptions { resolve: Some(resolve.clone()), ..Default::default() }, context::input(context))
                .map_err(|d| rays_diagnostic::to_string(&d))?;
            if value::key_of(&|r| r.0, &actual) != *expected { same = false; break; }
        }
        if same { return Ok(previous.3.clone()); }
    }
    let observations = Arc::new(Mutex::new(Vec::new()));
    let shape = {
        let (observations, live, resolve) = (observations.clone(), context::input(context).clone(), resolve);
        Arc::new(move |residual| {
            // The compiled program forces the decision as the cook would (packed loops, not the
            // reference walker) and is the projection a later cook re-forces.
            let options = executor::ForceOptions { resolve: Some(resolve.clone()), ..Default::default() };
            let value = match executor::compile(&Value::Residual(residual), &executor::CompileOptions::default())
                .and_then(|program| executor::force(&program, &options, &live).map(|value| (program, value)))
            {
                Ok(done) => done,
                Err(d) if d.code == "E_DATA_SOURCE" => return Ok(None),
                Err(d) => return Err(d),
            };
            let result = value.1.clone();
            observations.lock().unwrap().push((residual, value));
            Ok(Some(result))
        })
    };
    let mut result = Err("A Lisp body was not built".to_owned());
    E::residual::with_shape(shape, &mut || result = build());
    let result = result?;
    let observations = observations.lock().unwrap();
    let values: Vec<Value> = observations.iter().map(|(id, _)| Value::Residual(*id)).collect();
    let roots = E::roots(values.iter());
    let projections = observations.iter().map(|(_, (program, value))| (program.clone(), value::key_of(&|r| r.0, value))).collect();
    *cache.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(Arc::new((key, projections, roots, result.clone())));
    Ok(result)
}

fn cook_body(
    body: &Body,
    node_id: i64,
    context: &context::T,
    payloads: &[payload::T],
) -> Result<Cooked, Diagnostic> {
    let previous = COOKED_INPUTS.with(|inputs| {
        std::mem::replace(&mut *inputs.borrow_mut(), payloads.iter().cloned().enumerate().collect())
    });
    let _scope = InputsScope { previous: Some(previous), arguments: None, cache: None, invocation: None };
    let body = body(context)?;
    // The public node owns the cache entry. The body uses Session's payload/instance rules with
    // only per-cook sharing retained.
    let mut evaluator = session::create(0, 0).expect("an empty session");
    session::set_parallel_override(&mut evaluator, Some(false));
    // The body's kernel nodes (labelled flow.xxx or lisp_xxx) are implementation: the enclosing
    // session traces the failure to this node, not through a card the author never wrote.
    let output = session::cook(&mut evaluator, context::cpu_consumers(context, node_id), context, &body);
    session::close(&mut evaluator);
    match output {
        Ok(output) => Ok(Cooked { payload: output.payload, instances: output.instances }),
        Err(mut error) => {
            error.trace.retain(|frame| !(frame.label.starts_with("flow.") || frame.label.starts_with("lisp_")));
            Err(error)
        }
    }
}

type Prepared = (Vec<u8>, Vec<(executor::Program, Vec<u8>)>, E::Roots, Built);

/// Mark a field live. A vec3 group and its scalar aliases stay one kind of binding: all concrete
/// or all slots.
fn promote(fields: &[FieldView], name: &str, live: &mut Vec<Symbol>) {
    let group = fields.iter().find_map(|f| f.vec3.as_ref().filter(|(group, _)| f.name == name || group == name).map(|(group, _)| group.clone()));
    let names: Vec<Symbol> = match group {
        Some(group) => std::iter::once(Symbol::from(group.as_str()))
            .chain(fields.iter().filter(|f| f.vec3.as_ref().is_some_and(|(g, _)| *g == group)).map(|f| Symbol::from(f.name.as_str()))).collect(),
        None => vec![Symbol::from(name)],
    };
    for name in names { if !live.contains(&name) { live.push(name); } }
}

/// A node whose body is built from its parameters (and rebuilt when a context fact it reads
/// changes). Edits rebuild it through the same operator.
pub fn node(options: &NodeOptions, build_body: BuildFn, inputs: &[node::T]) -> Result<node::T, String> {
    let schema = Arc::new(schema("parameters", &options.fields, &options.normalizers));
    let defaults = schema.default().to_vec();
    let pending = INITIAL.with(|initial| {
        let mut initial = initial.borrow_mut();
        match initial.as_ref() {
            Some((target, _, _)) if *target == options.operation => initial.take().map(|(_, changes, driven)| (changes, driven)),
            _ => None,
        }
    });
    let (pending, driven) = match pending { Some((changes, driven)) => (Some(changes), driven), None => (None, Vec::new()) };
    let values = match pending {
        Some(changes) => match schema.apply_all(&defaults, &changes) {
            Ok((values, effects)) if effects.cook => values,
            Ok(_) | Err(_) => defaults,
        },
        None => defaults,
    };
    let unbound_function = options.unbound_function;
    let geometry_inputs = options.geometry_inputs.clone();
    let (operation, version, fields, source) =
        (options.operation.clone(), options.version, options.fields.clone(), options.source.clone());
    let kind: Arc<str> = options.kind.as_str().into();
    let slots: Arc<Vec<(Symbol, Value)>> = Arc::new(arguments(&fields, &values)?.into_iter().filter_map(|(name, value)| {
        matches!(value, Value::Bool(_) | Value::Float(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..))
            .then(|| (name, value::source_value("$lisp/parameter", value::ty_of(&value), vec![(name, Value::No_geo)].into())))
    }).collect());
    let cache: BodyCache = Arc::new(Mutex::new(None));
    // A first build specializes every value, so static nodes cook as fast as a fully constant
    // body. A Bool/Float/vector field is a cook-time slot instead when a live expression drives it
    // (`driven`), when the node is made under a parent's live slots, or from the first build that
    // changes it (a scrubbed value: one more lowering, then none). A slot's shape decisions are
    // projected and rechecked per cook. Ints stay concrete: a live count leaves loops interpreted
    // (debug spiral cooks ~12x slower), so changing one relowers.
    // ponytail: promotion is sticky per node; demote if a once-scrubbed node's cooks matter.
    let mut inherited = if ARGUMENTS.with(|active| active.borrow().is_empty()) { Vec::new() } else { slots.iter().map(|(name, _)| *name).collect() };
    for name in &driven { promote(&fields, name, &mut inherited); }
    let changed: Arc<Mutex<(Option<Arc<Vec<(Symbol, Value)>>>, Vec<Symbol>)>> = Arc::new(Mutex::new((None, inherited)));
    let operator: custom::Operator = Arc::new(move |label: &str, inputs: &[node::T], parameters: &[ParamValue]| {
        if unbound_function {
            return Ok(build::make(
                build::Args {
                    label: Some(label.to_owned()),
                    version,
                    parameters: source.clone().into_bytes(),
                    inputs: inputs.to_vec(),
                    ..build::args(&operation)
                },
                None,
                Arc::new(|_, _, _| {
                    Err(port::error(
                        "disconnected_input",
                        "A Lisp function input needs its checked callable before cooking",
                    ))
                }),
            ));
        }
        let arguments = Arc::new(arguments(&fields, parameters)?);
        let slots: Arc<Vec<(Symbol, Value)>> = {
            let mut changed = changed.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
            let (previous, live) = &mut *changed;
            if let Some(previous) = previous.as_ref() {
                for ((name, value), (_, before)) in arguments.iter().zip(previous.iter()) {
                    if value != before { promote(&fields, name.as_str(), live); }
                }
            }
            *previous = Some(arguments.clone());
            Arc::new(slots.iter().filter(|(name, _)| live.contains(name)).cloned().collect())
        };
        let _scope = argument_scope(kind.clone(), slots.clone(), arguments.clone(), cache.clone());
        let context = context::create(context::Options::default()).expect("the default context");
        let (body, dependencies, facts) = build_body(label, &context, inputs, &arguments)?;
        let prepared =
            Arc::new(Mutex::new(((context::seed(&context), context::frame(&context), context::time(&context).to_bits()), body)));
        let context_seeded = dependencies::mem(dependencies::Fact::Seed, dependencies);
        let framed = dependencies::mem(dependencies::Fact::Frame, dependencies);
        let timed = dependencies::mem(dependencies::Fact::Time, dependencies);
        let (label_owned, inputs_owned, arguments_owned) = (label.to_owned(), inputs.to_vec(), arguments);
        let (build_body, geometry_inputs, cache, slots, kind) = (build_body.clone(), geometry_inputs.clone(), cache.clone(), slots.clone(), kind.clone());
        let made = build::make(
            build::Args {
                label: Some(label.to_owned()),
                version,
                parameters: source.clone().into_bytes(),
                cook_mode: facts.cook_mode,
                dependencies,
                inputs: inputs.to_vec(),
                ..build::args(&operation)
            },
            None,
            Arc::new(move |node_id, context, payloads| {
                let _scope = argument_scope(kind.clone(), slots.clone(), arguments_owned.clone(), cache.clone());
                let wrong = geometry_inputs
                    .iter()
                    .enumerate()
                    .filter(|(i, geometry)| **geometry && *i < payloads.len())
                    .find_map(|(i, _)| payload::geometry(&payloads[i]).err());
                if let Some(error) = wrong {
                    return Err(error);
                }
                let current = (context::seed(context), context::frame(context), context::time(context).to_bits());
                let (key, previous_body) = {
                    let guard = prepared.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                    (guard.0, guard.1.clone())
                };
                let body = if (!context_seeded || key.0 == current.0)
                    && (!framed || key.1 == current.1)
                    && (!timed || key.2 == current.2)
                {
                    previous_body
                } else {
                    match build_body(&label_owned, context, &inputs_owned, &arguments_owned) {
                        Ok((body, _, _)) => {
                            *prepared.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = (current, body.clone());
                            body
                        }
                        Err(message) => return Err(port::error("E_PARAM", &message)),
                    }
                };
                cook_body(&body, node_id, context, payloads)
            }),
        );
        Ok(build::with_facts(facts, &made))
    });
    custom::node(Some(&options.label), &options.operation, schema, values, inputs.to_vec(), operator)
}

#[cfg(test)]
mod tests {
    use super::*;
    use param::{IntRange, Impact};
    use rdk_ops::line_geometry;
    use sop::node::{conservative, CookMode, GeometryCooked};

    fn count_field() -> FieldView {
        let range = IntRange { soft_min: 0, soft_max: 10, hard_min: Some(0), hard_max: Some(10) };
        FieldView {
            name: "count".to_owned(),
            label: "Count".to_owned(),
            description: None,
            folder: Vec::new(),
            impact: Impact::Cook,
            primary: false,
            unit: None,
            vec3: None,
            kind: KindView::Integer(range),
            default: ParamValue::Int(3),
            current: ParamValue::Int(3),
        }
    }

    fn options(unbound_function: bool, geometry_inputs: Vec<bool>) -> NodeOptions {
        NodeOptions {
            kind: "sop/test_lisp".into(),
            unbound_function,
            geometry_inputs,
            operation: "test_lisp".to_owned(),
            version: 2,
            label: "Test Lisp".to_owned(),
            fields: vec![count_field()],
            normalizers: Vec::new(),
            source: "(test_lisp)".to_owned(),
        }
    }

    /// A body of `count` points, or (when it has an input) a copy of input 0 read through `input`.
    fn build_points() -> BuildFn {
        Arc::new(|_label, _context, inputs, arguments| {
            let count = match arguments.iter().find(|(name, _)| name == "count") {
                Some((_, Value::Int(count))) => *count as usize,
                _ => return Err("count missing".to_owned()),
            };
            let reads_input = !inputs.is_empty();
            let body: Body = Arc::new(move |_context| {
                Ok(build::make_geometry(
                    build::Args { cook_mode: CookMode::Generator, parameters: format!("n={count}").into_bytes(), ..build::args("test_body") },
                    None,
                    Arc::new(move |_, _, _| {
                        if reads_input {
                            let geometry = payload::geometry(&input(0)?)?;
                            return Ok(GeometryCooked { geometry, instances: None });
                        }
                        let points: Vec<(f64, f64, f64)> = (0..count).map(|i| (i as f64, 0.0, 0.0)).collect();
                        let geometry = line_geometry::points(&points).map_err(|e| port::error("E_TEST", &rdk_core::error::to_string(&e)))?;
                        Ok(GeometryCooked { geometry, instances: None })
                    }),
                ))
            });
            Ok((body, dependencies::STATIC, conservative(CookMode::Generator)))
        })
    }

    fn snapshot_points(n: usize) -> node::T {
        let points: Vec<(f64, f64, f64)> = (0..n).map(|i| (i as f64, 1.0, 0.0)).collect();
        let held = line_geometry::points(&points).expect("points");
        build::make_geometry(
            build::Args { cook_mode: CookMode::Generator, parameters: format!("n={n}").into_bytes(), ..build::args("snapshot") },
            None,
            Arc::new(move |_, _, _| Ok(GeometryCooked { geometry: held.clone(), instances: None })),
        )
    }

    fn cook(n: &node::T) -> Result<rdk_core::geometry::T, Diagnostic> {
        let mut evaluator = session::create(8, 1_000_000).expect("session");
        let context = context::create(context::Options { domains: Some(1), ..Default::default() }).expect("context");
        let output = session::cook(&mut evaluator, None, &context, n);
        session::close(&mut evaluator);
        output.and_then(|o| payload::geometry(&o.payload))
    }

    #[test]
    fn schema_and_arguments_follow_the_fields() {
        let schema = schema("parameters", &[count_field()], &[]);
        assert_eq!(schema.default(), [ParamValue::Int(3)]);
        let (values, effects) = schema.apply_all(schema.default(), &[("count".into(), ParamValue::Int(99))]).expect("clamps");
        assert!(effects.cook);
        assert_eq!(values, [ParamValue::Int(10)]);
        assert_eq!(arguments(&[count_field()], &values).expect("arguments"), vec![("count".into(), Value::Int(10))]);
    }

    #[test]
    fn a_lisp_node_builds_cooks_edits_and_keeps_identity() {
        let made = node(&options(false, Vec::new()), build_points(), &[]).expect("node");
        assert_eq!((node::operation(&made), node::version(&made), node::label(&made)), ("test_lisp", 2, "Test Lisp"));
        assert_eq!(rdk_core::geometry::point_count(&cook(&made).expect("cooks")), 3);
        let (edited, effects) = node::apply_parameters(&made, &[("count".into(), ParamValue::Int(5))]).expect("edit");
        assert!(effects.cook && node::id(&edited) == node::id(&made));
        assert_eq!(rdk_core::geometry::point_count(&cook(&edited).expect("cooks")), 5);
        assert!(node::apply_parameters(&made, &[("count".into(), ParamValue::Text(b"x".as_slice().into()))]).is_err());
    }

    #[test]
    fn initial_changes_are_consumed_by_the_first_node_of_their_operation() {
        let changes = [("count".into(), ParamValue::Int(7))];
        let first = with_initial_changes("test_lisp", &changes, &mut || node(&options(false, Vec::new()), build_points(), &[])).expect("first");
        assert_eq!(node::parameter_fields(&first)[0].current, ParamValue::Int(7));
        let later = node(&options(false, Vec::new()), build_points(), &[]).expect("later");
        assert_eq!(node::parameter_fields(&later)[0].current, ParamValue::Int(3));
        let other = with_initial_changes("other_operation", &changes, &mut || node(&options(false, Vec::new()), build_points(), &[])).expect("other");
        assert_eq!(node::parameter_fields(&other)[0].current, ParamValue::Int(3));
    }

    #[test]
    fn cooked_inputs_are_readable_only_while_the_node_cooks() {
        assert_eq!(input(0).err().expect("no inputs outside a cook").code, "E_DATA_SOURCE");
        let reading = node(&options(false, vec![true]), build_points(), &[snapshot_points(4)]).expect("node");
        assert_eq!(rdk_core::geometry::point_count(&cook(&reading).expect("cooks")), 4);
        assert_eq!(input(0).err().expect("no inputs outside a cook").code, "E_DATA_SOURCE");
    }

    #[test]
    fn unbound_function_nodes_refuse_to_cook() {
        let unbound = node(&options(true, Vec::new()), build_points(), &[]).expect("node");
        assert_eq!(cook(&unbound).err().expect("refuses").code, "disconnected_input");
    }

    #[test]
    fn geometry_inputs_must_carry_geometry() {
        // the check precedes the body, as a native geometry node refuses an image
        let picture = sop::image::of_owned_rgba(1, 1, vec![0.0, 0.0, 0.0, 1.0]).expect("image");
        let image_node = build::make(
            build::Args { cook_mode: CookMode::Generator, ..build::args("test_image") },
            None,
            Arc::new(move |_, _, _| Ok(Cooked { payload: payload::T::Image(picture.clone()), instances: None })),
        );
        let guarded = node(&options(false, vec![true]), build_points(), &[image_node.clone()]).expect("node");
        assert_eq!(cook(&guarded).err().expect("refuses").code, "E_PAYLOAD");
        let unguarded = node(&options(false, vec![false]), build_points(), &[image_node]).expect("node");
        assert!(cook(&unguarded).is_err(), "the body reads input 0 as geometry itself");
    }
    #[test]
    fn parameter_descriptors_keep_owner_trust_and_worker_snapshots() {
        let name: Symbol = "amount".into();
        let descriptor = || value::source_value("$lisp/parameter", Ty::Float, vec![(name, Value::No_geo)].into());
        let outer = descriptor();
        let inner = descriptor();
        let forged = descriptor();
        assert_eq!(value::key_of(&|r| r.0, &outer), value::key_of(&|r| r.0, &forged), "durable keys contain no process owner id");
        let cache = || Arc::new(Mutex::new(None));
        let slots = Arc::new(vec![(name, outer.clone())]);
        let args = Arc::new(vec![(name, Value::Float(1.0))]);
        let scope = argument_scope("sop/outer".into(), slots.clone(), args.clone(), cache());
        assert!(owns_parameter("sop/outer", name, &outer));
        assert!(!owns_parameter("sop/outer", name, &forged));
        assert!(!owns_parameter("sop/inner", name, &outer), "an outer clamp cannot certify another node's bounds");
        assert!(!owns_parameter("sop/outer", "different".into(), &outer));
        let bound = parameter_values(&args);
        assert!(matches!(&bound[0].1, Value::Source(g) if matches!(&outer, Value::Source(expected) if Arc::ptr_eq(g, expected))));
        assert_eq!(parameter_values(&[(name, Value::Float(99.0))])[0].1, Value::Float(99.0), "same-name literals are not the invocation's normalized slice");
        let fallback = || Arc::new(|_: &Value| Err(port::error("E_TEST", "unknown source"))) as flow_ir::core_exec::ResolveFn;
        let old = resolver(fallback());
        let nested_scope = argument_scope("sop/inner".into(), Arc::new(vec![(name, inner.clone())]), Arc::new(vec![(name, Value::Float(10.0))]), cache());
        assert!(!owns_parameter("sop/inner", name, &outer));
        let nested = resolver(fallback());
        drop(nested_scope);
        assert_eq!(resolve_parameter(&outer).unwrap().unwrap(), Value::Float(1.0));
        drop(scope);
        let new_scope = argument_scope("sop/outer".into(), slots, Arc::new(vec![(name, Value::Float(2.0))]), cache());
        let new = resolver(fallback());
        drop(new_scope);
        assert_eq!(resolve_parameter(&outer).unwrap().unwrap_err().code, "E_DATA_SOURCE");
        let main = std::thread::current().id();
        let worker = std::sync::atomic::AtomicBool::new(false);
        // A loaded machine can drain one fill on the caller before a worker wakes: retry until a
        // worker has taken part (every attempt checks the snapshots).
        for _ in 0..200 {
            if worker.load(std::sync::atomic::Ordering::Relaxed) { break; }
            parallel::run_unit(Some(4), &|| {
                let mut values = [0i64; 64];
                rdk_core::support::par_fill(None, 1, &mut values, &|_| {
                    if std::thread::current().id() != main { worker.store(true, std::sync::atomic::Ordering::Relaxed); }
                    assert_eq!(old(&outer).unwrap(), Value::Float(1.0));
                    assert_eq!(new(&outer).unwrap(), Value::Float(2.0));
                    assert_eq!(nested(&outer).unwrap(), Value::Float(1.0));
                    assert_eq!(nested(&inner).unwrap(), Value::Float(10.0));
                    assert!(nested(&forged).is_err());
                    assert_eq!(resolve_parameter(&outer).unwrap().unwrap_err().code, "E_DATA_SOURCE", "resolver scope leaked into worker state");
                    Ok(1)
                }).map_err(|e| rdk_core::error::to_string(&e))?;
                assert_eq!(values, [1; 64]);
                Ok(())
            }).unwrap();
        }
        if parallel::recommended_domains() > 1 {
            assert!(worker.load(std::sync::atomic::Ordering::Relaxed), "the binding snapshot was not exercised on a pool worker");
        }
    }

}
