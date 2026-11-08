//! Port of lib/sop/node.ml: immutable procedural DAG nodes. Shared node values represent shared
//! graph subgraphs; `ptr_eq` is OCaml's physical equality.
use std::collections::HashSet;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use param::{Effects, FieldView, Schema, Value};
use rays_diagnostic::{Diagnostic, Trace};
use rays_math::mat4;

use crate::context::{self, dependencies};
use crate::payload;
use crate::support;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CookMode {
    Generator,
    DuplicateInput(i64),
    InPlace(i64),
    InstanceInput(i64),
    Passthrough(i64),
    Generic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Elementwise {
    Points,
    Primitives,
    None,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Topology {
    Preserved,
    Changed,
}

/// Payload use, separate from output-sharing facts. `Cpu` is a CPU reader of each selected input.
/// `Forward(i)` only forwards input `i`, without payload reads, and ignores other payloads.
/// `Unknown` supplies no placement proof. Owners must explicitly promise the use.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputUse {
    Unknown,
    Cpu,
    Forward(i64),
}

/// Kernel legality and component dependencies. "P" names canonical positions; other names select
/// all matching attribute owners. "*" means opaque access to the whole input/output.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Facts {
    pub cook_mode: CookMode,
    pub elementwise: Elementwise,
    pub reads: Vec<String>,
    pub writes: Vec<String>,
    pub topology: Topology,
    pub exact: bool,
}

pub fn conservative(cook_mode: CookMode) -> Facts {
    Facts {
        cook_mode,
        elementwise: Elementwise::None,
        reads: vec!["*".to_owned()],
        writes: vec!["*".to_owned()],
        topology: Topology::Changed,
        exact: true,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputPolicy {
    All,
    Only(i64),
}

#[derive(Clone)]
pub struct Cooked {
    pub payload: payload::T,
    pub instances: Option<Arc<Vec<mat4::T>>>,
}

#[derive(Clone)]
pub struct GeometryCooked {
    pub geometry: rdk::geometry::T,
    pub instances: Option<Arc<Vec<mat4::T>>>,
}

pub type CookFn = Arc<dyn Fn(i64, &context::T, &[payload::T]) -> Result<Cooked, Diagnostic> + Send + Sync>;
pub type ExpandFn = Arc<dyn Fn(&context::T, &[T], &[payload::T]) -> Result<Vec<T>, Diagnostic> + Send + Sync>;
pub type GeometryCookFn =
    Arc<dyn Fn(i64, &context::T, &[rdk::geometry::T]) -> Result<GeometryCooked, Diagnostic> + Send + Sync>;
pub type GeometryExpandFn =
    Arc<dyn Fn(&context::T, &[T], &[rdk::geometry::T]) -> Result<Vec<T>, Diagnostic> + Send + Sync>;
/// `(label, current inputs, values) -> rebuilt node`.
pub type RebuildFn = Arc<dyn Fn(&str, &[T], &[Value]) -> Result<T, String> + Send + Sync>;

#[derive(Clone)]
struct Parameterization {
    cache_parameters: Vec<u8>,
    schema: Arc<Schema>,
    values: Vec<Value>,
    rebuild: RebuildFn,
}

#[derive(Clone)]
struct Data {
    id: i64,
    label: String,
    operation: String,
    version: i64,
    parameters: Vec<u8>,
    parameter_key: Vec<u8>,
    facts: Facts,
    facts_key: Vec<u8>,
    dependencies: dependencies::T,
    input_policy: InputPolicy,
    input_use: InputUse,
    inputs: Vec<T>,
    cook: CookFn,
    expand: Option<ExpandFn>,
    parameterization: Option<Parameterization>,
    owner: Option<Arc<dyn std::any::Any + Send + Sync>>,
}

/// A node. Cloning shares the node; `ptr_eq` tells shared from rebuilt.
#[derive(Clone)]
pub struct T(Arc<Data>);

static NEXT_ID: AtomicI64 = AtomicI64::new(1);
const MAX_INT: i64 = (1_i64 << 62) - 1;

fn fresh_id() -> i64 {
    NEXT_ID.fetch_add(1, Ordering::SeqCst)
}

/// OCaml `==` on nodes.
pub fn ptr_eq(a: &T, b: &T) -> bool {
    Arc::ptr_eq(&a.0, &b.0)
}

pub fn id(value: &T) -> i64 {
    value.0.id
}
pub fn label(value: &T) -> &str {
    &value.0.label
}
pub fn operation(value: &T) -> &str {
    &value.0.operation
}
pub fn version(value: &T) -> i64 {
    value.0.version
}
/// The hand-written cache text of an unparameterized node, or `Param.cook_text` of its schema
/// values (`name=value;...`) for a parameterized node that has none.
pub fn parameters(value: &T) -> &[u8] {
    &value.0.parameters
}
/// `Param.cook_key` of the attached schema values, computed once when parameterized; empty for an
/// unparameterized node. Session cache identity includes it.
pub fn parameter_key(value: &T) -> &[u8] {
    &value.0.parameter_key
}
pub fn facts(value: &T) -> &Facts {
    &value.0.facts
}
pub fn dependencies(value: &T) -> dependencies::T {
    value.0.dependencies
}
pub fn inputs(value: &T) -> Vec<T> {
    value.0.inputs.clone()
}
pub fn input_use(value: &T) -> InputUse {
    value.0.input_use
}

pub fn trace(value: &T) -> Trace {
    Trace { node_id: value.0.id, label: value.0.label.clone(), operation: value.0.operation.clone() }
}

/// Concrete, type-erased inspector fields owned by this node instance.
pub fn parameter_fields(value: &T) -> Vec<FieldView> {
    match &value.0.parameterization {
        None => Vec::new(),
        Some(p) => p.schema.view(&p.values).unwrap_or_default(),
    }
}

pub fn has_parameters(value: &T) -> bool {
    value.0.parameterization.is_some()
}

/// The same node (id, parameters, cook identity) under a new display label; a blank label is ignored.
pub fn relabel(label: &str, value: &T) -> T {
    if support::ocaml_trim(label).is_empty() {
        return value.clone();
    }
    let mut data = (*value.0).clone();
    data.label = support::ocaml_trim(label).to_owned();
    T(Arc::new(data))
}

/// `Node.parameterize_with_key`: attach typed parameter values while supplying their exact opaque
/// key (`None`: `Param.cook_key`).
pub fn parameterize_with_key(
    parameter_key: Option<Vec<u8>>,
    schema: Arc<Schema>,
    mut values: Vec<Value>,
    rebuild: RebuildFn,
    value: &T,
) -> Result<T, String> {
    schema.normalize(&mut values).map_err(|message| format!("Node.parameterize: {message}"))?;
    let mut data = (*value.0).clone();
    // an operator with no hand key of its own reads as the schema's text
    if data.parameters.is_empty() {
        data.parameters = schema.cook_text(&values)?;
    }
    data.parameter_key = match parameter_key {
        Some(key) => key,
        None => schema.cook_key(&values)?,
    };
    data.parameterization =
        Some(Parameterization { cache_parameters: value.0.parameters.clone(), schema, values, rebuild });
    Ok(T(Arc::new(data)))
}

/// Attach a typed immutable parameter record and its pure reconstruction function to a node.
pub fn parameterize(schema: Arc<Schema>, values: Vec<Value>, rebuild: RebuildFn, value: &T) -> Result<T, String> {
    parameterize_with_key(None, schema, values, rebuild, value)
}

/// Apply inspector writes to this node only. Cook-affecting changes rebuild its operator closure
/// while preserving the logical node id; view/export changes update metadata without invalidating
/// cooked geometry.
pub fn apply_parameters(value: &T, changes: &[(String, Value)]) -> Result<(T, Effects), String> {
    let Some(p) = &value.0.parameterization else {
        if changes.is_empty() {
            return Ok((value.clone(), param::NO_EFFECTS));
        }
        return Err(format!("node {} has no exposed parameters", ocaml_quote(&value.0.label)));
    };
    let (values, effects) = p.schema.apply_all(&p.values, changes)?;
    if !param::has_effects(effects) {
        return Ok((value.clone(), effects));
    }
    if effects.cook {
        let rebuilt = (p.rebuild)(&value.0.label, &value.0.inputs, &values)?;
        let mut data = (*rebuilt.0).clone();
        data.id = value.0.id;
        data.owner = data.owner.or_else(|| value.0.owner.clone());
        Ok((T(Arc::new(data)), effects))
    } else {
        let mut data = (*value.0).clone();
        data.parameterization = Some(Parameterization { values, ..p.clone() });
        Ok((T(Arc::new(data)), effects))
    }
}

/// OCaml `%S` for a plain string.
pub fn ocaml_quote(text: &str) -> String {
    let mut out = String::from("\"");
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 32 || c as u32 == 127 => out.push_str(&format!("\\{:03}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn facts_key(facts: &Facts) -> Vec<u8> {
    let mut out = Vec::new();
    let int = |out: &mut Vec<u8>, n: i64| out.extend_from_slice(&n.to_le_bytes());
    match facts.cook_mode {
        CookMode::Generator => int(&mut out, 0),
        CookMode::DuplicateInput(i) => {
            int(&mut out, 1);
            int(&mut out, i)
        }
        CookMode::InPlace(i) => {
            int(&mut out, 2);
            int(&mut out, i)
        }
        CookMode::InstanceInput(i) => {
            int(&mut out, 3);
            int(&mut out, i)
        }
        CookMode::Passthrough(i) => {
            int(&mut out, 4);
            int(&mut out, i)
        }
        CookMode::Generic => int(&mut out, 5),
    }
    int(&mut out, facts.elementwise as i64);
    int(&mut out, facts.topology as i64);
    int(&mut out, i64::from(facts.exact));
    for names in [&facts.reads, &facts.writes] {
        int(&mut out, names.len() as i64);
        for name in names {
            int(&mut out, name.len() as i64);
            out.extend_from_slice(name.as_bytes());
        }
    }
    out
}

pub mod build {
    use super::*;

    pub fn fresh_id() -> i64 {
        super::fresh_id()
    }

    pub fn reserve_id(id: i64) -> Result<(), String> {
        if id < 1 || id == MAX_INT {
            return Err("node id is out of range".to_owned());
        }
        NEXT_ID.fetch_max(id + 1, Ordering::SeqCst);
        Ok(())
    }

    /// Restore a saved logical id and reserve it in the shared allocator.
    pub fn restore_id(id: i64, value: &T) -> Result<T, String> {
        reserve_id(id)?;
        let mut data = (*value.0).clone();
        data.id = id;
        Ok(T(Arc::new(data)))
    }

    /// Arguments of `make`/`make_geometry` (OCaml labelled arguments). Start from `args(operation)`
    /// and override fields with struct update syntax.
    pub struct Args {
        pub label: Option<String>,
        pub operation: String,
        pub version: i64,
        pub parameters: Vec<u8>,
        pub cook_mode: CookMode,
        pub dependencies: dependencies::T,
        pub input_policy: Option<InputPolicy>,
        pub inputs: Vec<T>,
    }

    pub fn args(operation: &str) -> Args {
        Args {
            label: None,
            operation: operation.to_owned(),
            version: 1,
            parameters: Vec::new(),
            cook_mode: CookMode::Generic,
            dependencies: dependencies::STATIC,
            input_policy: None,
            inputs: Vec::new(),
        }
    }

    pub fn make(args: Args, expand: Option<ExpandFn>, cook: CookFn) -> T {
        assert!(args.version >= 0, "Node.make: version must be non-negative");
        assert!(!support::ocaml_trim(&args.operation).is_empty(), "Node.make: empty operation");
        let label = match args.label {
            Some(label) if !support::ocaml_trim(&label).is_empty() => label,
            _ => args.operation.clone(),
        };
        let input_policy = args.input_policy.unwrap_or(InputPolicy::All);
        if let InputPolicy::Only(index) = input_policy {
            assert!(index >= 0 && (index as usize) < args.inputs.len(), "Node.make: selected input is out of bounds");
        }
        let facts = conservative(args.cook_mode);
        T(Arc::new(Data {
            id: super::fresh_id(),
            label,
            operation: args.operation,
            version: args.version,
            parameters: args.parameters,
            parameter_key: Vec::new(),
            facts_key: facts_key(&facts),
            facts,
            dependencies: args.dependencies,
            input_policy,
            input_use: InputUse::Unknown,
            inputs: args.inputs,
            cook,
            expand,
            parameterization: None,
            owner: None,
        }))
    }

    fn geometries(inputs: &[payload::T]) -> Result<Vec<rdk::geometry::T>, Diagnostic> {
        inputs.iter().map(payload::geometry).collect()
    }

    /// Geometry SOP adapter. An Image or Kernel input returns `E_PAYLOAD` before calling either the
    /// expansion or cook callback.
    pub fn make_geometry(args: Args, expand: Option<GeometryExpandFn>, cook: GeometryCookFn) -> T {
        let expand: Option<ExpandFn> = expand.map(|expand| {
            Arc::new(move |context: &context::T, nodes: &[T], inputs: &[payload::T]| {
                expand(context, nodes, &geometries(inputs)?)
            }) as ExpandFn
        });
        make(
            args,
            expand,
            Arc::new(move |node_id, context, inputs| {
                let cooked = cook(node_id, context, &geometries(inputs)?)?;
                Ok(Cooked { payload: payload::T::Geometry(cooked.geometry), instances: cooked.instances })
            }),
        )
    }

    /// Keep a cook dependency alive with this node, including parameter/input rebuilds.
    /// The dependency must not own this node or a cache containing it.
    pub fn with_owner(owner: Arc<dyn std::any::Any + Send + Sync>, mut value: T) -> T {
        let data = Arc::make_mut(&mut value.0);
        data.owner = Some(match data.owner.take() {
            None => owner,
            Some(previous) => Arc::new((previous, owner)),
        });
        value
    }

    /// Intrinsic operator identity before schema display text is attached.
    pub fn cache_parameters(value: &T) -> &[u8] {
        match &value.0.parameterization {
            None => &value.0.parameters,
            Some(p) => &p.cache_parameters,
        }
    }

    /// Process-local binary declaration identity, encoded once at construction.
    pub fn cache_facts(value: &T) -> &[u8] {
        &value.0.facts_key
    }

    /// Refine the conservative declaration without changing the cook mode.
    pub fn with_facts(facts: Facts, value: &T) -> T {
        assert!(facts.cook_mode == value.0.facts.cook_mode, "Node.with_facts: cook mode cannot change");
        assert!(
            !facts
                .reads
                .iter()
                .chain(facts.writes.iter())
                .any(|name| support::ocaml_trim(name).is_empty()),
            "Node.with_facts: component names must not be blank"
        );
        let mut data = (*value.0).clone();
        data.facts_key = facts_key(&facts);
        data.facts = facts;
        T(Arc::new(data))
    }

    pub fn input_policy(value: &T) -> InputPolicy {
        value.0.input_policy
    }

    pub fn with_input_use(input_use: InputUse, value: &T) -> T {
        if let InputUse::Forward(i) = input_use {
            assert!(
                i >= 0 && (i as usize) < value.0.inputs.len(),
                "Node.with_input_use: forwarded input is out of bounds"
            );
            assert!(
                value.0.input_policy == InputPolicy::All || value.0.input_policy == InputPolicy::Only(i),
                "Node.with_input_use: forwarded input is not selected"
            );
        }
        let mut data = (*value.0).clone();
        data.input_use = input_use;
        T(Arc::new(data))
    }

    pub fn input_array(value: &T) -> Vec<T> {
        value.0.inputs.clone()
    }

    pub fn with_inputs(value: &T, inputs: &[T]) -> T {
        if let InputPolicy::Only(index) = value.0.input_policy {
            assert!(index >= 0 && (index as usize) < inputs.len(), "Node.with_inputs: selected input is out of bounds");
        }
        if let InputUse::Forward(i) = value.0.input_use {
            assert!((i as usize) < inputs.len(), "Node.with_inputs: forwarded input is out of bounds");
        }
        let mut data = (*value.0).clone();
        data.inputs = inputs.to_vec();
        T(Arc::new(data))
    }

    /// Rebuild a parameterized node against new inputs so input-dependent schemas stay current.
    /// The logical id is retained. Unparameterized nodes only swap their inputs.
    pub fn rebuild_with_inputs(value: &T, inputs: &[T]) -> T {
        match &value.0.parameterization {
            None => with_inputs(value, inputs),
            Some(p) => match (p.rebuild)(&value.0.label, inputs, &p.values) {
                Ok(rebuilt) => {
                    let mut data = (*rebuilt.0).clone();
                    data.id = value.0.id;
                    data.owner = data.owner.or_else(|| value.0.owner.clone());
                    T(Arc::new(data))
                }
                Err(message) => unreachable!("{message}"),
            },
        }
    }

    /// Retain a document node's logical identity and label on a freshly rebuilt operator.
    pub fn adopt_identity(source: &T, value: &T) -> T {
        let mut data = (*value.0).clone();
        data.id = source.0.id;
        data.label = source.0.label.clone();
        T(Arc::new(data))
    }

    pub fn cook(value: &T, context: &context::T, inputs: &[payload::T]) -> Result<Cooked, Diagnostic> {
        (value.0.cook)(value.0.id, context, inputs)
    }

    /// A zone node: given its current input nodes and their cooked outputs it returns the roots of
    /// the sub-graphs to cook, one per element.
    pub fn expand(value: &T) -> Option<ExpandFn> {
        value.0.expand.clone()
    }

    pub fn has_expand(value: &T) -> bool {
        value.0.expand.is_some()
    }
}

#[derive(Clone)]
pub struct Info {
    pub id: i64,
    pub label: String,
    pub operation: String,
    pub version: i64,
    pub parameters: Vec<u8>,
    pub facts: Facts,
    pub dependencies: dependencies::T,
    pub input_ids: Vec<i64>,
    pub has_parameters: bool,
}

/// Deterministic input-before-consumer order. Shared nodes appear once.
pub fn inspect(root: &T) -> Vec<Info> {
    fn visit(node: &T, seen: &mut HashSet<i64>, result: &mut Vec<Info>) {
        if !seen.insert(id(node)) {
            return;
        }
        for input in &node.0.inputs {
            visit(input, seen, result);
        }
        result.push(Info {
            id: id(node),
            label: label(node).to_owned(),
            operation: operation(node).to_owned(),
            version: version(node),
            parameters: parameters(node).to_vec(),
            facts: facts(node).clone(),
            dependencies: dependencies(node),
            input_ids: node.0.inputs.iter().map(id).collect(),
            has_parameters: has_parameters(node),
        });
    }
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    visit(root, &mut seen, &mut result);
    result
}

pub fn find(root: &T, node_id: i64) -> Option<T> {
    fn visit(node: &T, node_id: i64, seen: &mut HashSet<i64>) -> Option<T> {
        if id(node) == node_id {
            return Some(node.clone());
        }
        if !seen.insert(id(node)) {
            return None;
        }
        node.0.inputs.iter().find_map(|input| visit(input, node_id, seen))
    }
    visit(root, node_id, &mut HashSet::new())
}

/// Union of context facts consumed anywhere in the reachable DAG.
pub fn graph_dependencies(root: &T) -> dependencies::T {
    inspect(root)
        .into_iter()
        .fold(dependencies::STATIC, |all, info| dependencies::union(all, info.dependencies))
}
