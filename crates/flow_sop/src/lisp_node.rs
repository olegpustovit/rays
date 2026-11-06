//! Port of lib/flow_sop/lisp_node.ml: a checked Lisp geometry body as a SOP node. Node-owned
//! schemas and cook identity; the body is built from the node's parameter values and cooked in its
//! own session.
//!
//! The OCaml domain-local storage (`Domain.DLS`) of the cooked inputs and of the pending initial
//! changes are thread-locals here: a node cooks on one thread, nested bodies on the same one.
use flow_eval::Value;
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
    static INITIAL: RefCell<Option<(String, Vec<(String, ParamValue)>)>> = const { RefCell::new(None) };
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
            Kind::Choice { options: labels.clone(), equal: Box::new(|a, b| a == b) }
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
        ParamValue::Choice(v) => Value::Text(v.clone().into_bytes()),
    }
}

/// Body arguments of schema values (positional, one per field): vec3 groups and their components.
pub fn arguments(fields: &[FieldView], values: &[ParamValue]) -> Result<Vec<(String, Value)>, String> {
    let current: Vec<FieldView> = fields
        .iter()
        .zip(values)
        .map(|(f, value)| FieldView { current: value.clone(), ..f.clone() })
        .collect();
    let ports = port::parameters(false, &current).map_err(|d| rays_diagnostic::to_string(&d))?;
    let mut out: Vec<(String, Value)> = ports
        .iter()
        .map(|p| {
            (
                p.path.clone(),
                match port::literal(p) {
                    Literal::Vector(x, y, z) => Value::Vec3(x, y, z),
                    Literal::Scalar(v) => scalar(&v),
                },
            )
        })
        .collect();
    out.extend(current.iter().filter(|f| f.vec3.is_some()).map(|f| (f.name.clone(), scalar(&f.current))));
    Ok(out)
}

/// Run an instantiation whose first Lisp node of `operation` starts from these parameter changes
/// instead of its defaults, so a node that is about to be edited is built once, not twice.
pub fn with_initial_changes(
    operation: &str,
    changes: &[(String, ParamValue)],
    run: &mut dyn FnMut() -> Result<node::T, String>,
) -> Result<node::T, String> {
    if changes.is_empty() {
        return run();
    }
    INITIAL.with(|initial| *initial.borrow_mut() = Some((operation.to_owned(), changes.to_vec())));
    let result = run();
    INITIAL.with(|initial| *initial.borrow_mut() = None);
    result
}

/// A body: cooked in the node's own session to the node that produces the payload.
pub type Body = Arc<dyn Fn(&context::T) -> Result<node::T, Diagnostic> + Send + Sync>;

/// A built body with its dependencies and facts.
pub type Built = (Body, dependencies::T, Facts);

/// `build ~label ~context inputs arguments`: the body of the node for these arguments.
pub type BuildFn = Arc<dyn Fn(&str, &context::T, &[node::T], &[(String, Value)]) -> Result<Built, String> + Send + Sync>;

pub struct NodeOptions {
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

fn restore_inputs(previous: Vec<(usize, payload::T)>) {
    COOKED_INPUTS.with(|inputs| *inputs.borrow_mut() = previous);
}

struct InputsScope {
    previous: Option<Vec<(usize, payload::T)>>,
}

impl Drop for InputsScope {
    fn drop(&mut self) {
        if let Some(previous) = self.previous.take() {
            restore_inputs(previous);
        }
    }
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
    let _scope = InputsScope { previous: Some(previous) };
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

type Prepared = ((i64, i64, u64), Body);

/// A node whose body is built from its parameters (and rebuilt when a context fact it reads
/// changes). Edits rebuild it through the same operator.
pub fn node(options: &NodeOptions, build_body: BuildFn, inputs: &[node::T]) -> Result<node::T, String> {
    let schema = Arc::new(schema("parameters", &options.fields, &options.normalizers));
    let defaults = schema.default().to_vec();
    let pending = INITIAL.with(|initial| {
        let mut initial = initial.borrow_mut();
        match initial.as_ref() {
            Some((target, _)) if *target == options.operation => initial.take().map(|(_, changes)| changes),
            _ => None,
        }
    });
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
        let arguments = arguments(&fields, parameters)?;
        let context = context::create(context::Options::default()).expect("the default context");
        let (body, dependencies, facts) = build_body(label, &context, inputs, &arguments)?;
        let prepared: Arc<Mutex<Prepared>> =
            Arc::new(Mutex::new(((context::seed(&context), context::frame(&context), context::time(&context).to_bits()), body)));
        let context_seeded = dependencies::mem(dependencies::Fact::Seed, dependencies);
        let framed = dependencies::mem(dependencies::Fact::Frame, dependencies);
        let timed = dependencies::mem(dependencies::Fact::Time, dependencies);
        let (label_owned, inputs_owned, arguments_owned) = (label.to_owned(), inputs.to_vec(), arguments);
        let (build_body, geometry_inputs) = (build_body.clone(), geometry_inputs.clone());
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
    use rdk::line_geometry;
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
                        let geometry = line_geometry::points(&points).map_err(|e| port::error("E_TEST", &rdk::error::to_string(&e)))?;
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

    fn cook(n: &node::T) -> Result<rdk::geometry::T, Diagnostic> {
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
        let (values, effects) = schema.apply_all(schema.default(), &[("count".to_owned(), ParamValue::Int(99))]).expect("clamps");
        assert!(effects.cook);
        assert_eq!(values, [ParamValue::Int(10)]);
        assert_eq!(arguments(&[count_field()], &values).expect("arguments"), vec![("count".to_owned(), Value::Int(10))]);
    }

    #[test]
    fn a_lisp_node_builds_cooks_edits_and_keeps_identity() {
        let made = node(&options(false, Vec::new()), build_points(), &[]).expect("node");
        assert_eq!((node::operation(&made), node::version(&made), node::label(&made)), ("test_lisp", 2, "Test Lisp"));
        assert_eq!(rdk::geometry::point_count(&cook(&made).expect("cooks")), 3);
        let (edited, effects) = node::apply_parameters(&made, &[("count".to_owned(), ParamValue::Int(5))]).expect("edit");
        assert!(effects.cook && node::id(&edited) == node::id(&made));
        assert_eq!(rdk::geometry::point_count(&cook(&edited).expect("cooks")), 5);
        assert!(node::apply_parameters(&made, &[("count".to_owned(), ParamValue::Text(b"x".to_vec()))]).is_err());
    }

    #[test]
    fn initial_changes_are_consumed_by_the_first_node_of_their_operation() {
        let changes = [("count".to_owned(), ParamValue::Int(7))];
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
        assert_eq!(rdk::geometry::point_count(&cook(&reading).expect("cooks")), 4);
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
}
