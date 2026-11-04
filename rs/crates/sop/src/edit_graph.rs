//! Port of lib/sop/edit_graph.ml: immutable editable representation of a procedural network.
//!
//! `node::T` remains the compiled, cookable DAG. This document can also hold disconnected input
//! slots and nodes outside the current display path, then compile any valid node back to an
//! ordinary `node::T`. It owns topology only.
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::{Arc, OnceLock};

use param::{Effects, FieldView, Value};

use crate::context::dependencies;
use crate::node::{self, build, ocaml_quote, Facts};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputRequirement {
    Required,
    Optional,
    /// Only valid last: the slot repeats, so a node holds any number of inputs at least the slot
    /// count. The first rest input is required, the others optional; extras are named `name_2`,
    /// `name_3`, ... `connect` one past the last input appends one.
    Rest,
    /// Also repeats only at the end, but permits zero connected elements.
    OptionalRest,
}

use InputRequirement::{Optional, OptionalRest, Required, Rest};

pub type BuildFn = Arc<dyn Fn(&[Option<node::T>]) -> Result<node::T, String> + Send + Sync>;

struct FactoryData {
    key: String,
    operation: String,
    label: String,
    category: Vec<String>,
    fields: Vec<FieldView>,
    requirements: Vec<InputRequirement>,
    slots: Vec<String>,
    input_types: Vec<String>,
    keyword_inputs: Vec<String>,
    build: BuildFn,
    facts: OnceLock<Facts>,
}

#[derive(Clone)]
pub struct Factory(Arc<FactoryData>);

#[derive(Clone)]
struct Entry {
    node: node::T,
    inputs: Vec<Option<i64>>,
    factory: Option<Factory>,
}

#[derive(Clone)]
struct Inner {
    entries: BTreeMap<i64, Arc<Entry>>,
    order_rev: Vec<i64>,
    root: Option<i64>,
}

/// ponytail: the OCaml document is a persistent map; this clones the (Arc-sharing) map per edit.
/// Use a persistent map if documents grow past a few thousand nodes.
#[derive(Clone)]
pub struct T(Arc<Inner>);

/// OCaml `==` on documents.
pub fn ptr_eq(a: &T, b: &T) -> bool {
    Arc::ptr_eq(&a.0, &b.0)
}

#[derive(Clone)]
pub struct NodeInfo {
    pub node: node::T,
    pub id: i64,
    pub label: String,
    pub operation: String,
    pub version: i64,
    pub parameters: Vec<u8>,
    pub facts: Facts,
    pub dependencies: dependencies::T,
    pub inputs: Vec<Option<i64>>,
    pub has_parameters: bool,
}

fn repeating(requirement: InputRequirement) -> bool {
    matches!(requirement, Rest | OptionalRest)
}

fn last_requirement(f: &FactoryData) -> InputRequirement {
    f.requirements[f.requirements.len() - 1]
}

fn arity_ok(f: &FactoryData, n: usize) -> bool {
    let len = f.requirements.len();
    n == len
        || (len > 0
            && repeating(last_requirement(f))
            && n + usize::from(last_requirement(f) == OptionalRest) >= len)
}

fn requirement(f: &FactoryData, index: usize) -> InputRequirement {
    let last = f.requirements.len() - 1;
    match f.requirements[index.min(last)] {
        Rest => {
            if index == last {
                Required
            } else {
                Optional
            }
        }
        OptionalRest => Optional,
        r => r,
    }
}

fn has_optional(f: &FactoryData) -> bool {
    f.requirements.iter().any(|r| *r != Required)
}

fn slot_names_of(f: &FactoryData, n: usize) -> Vec<String> {
    let len = f.slots.len();
    (0..n)
        .map(|i| if i < len { f.slots[i].clone() } else { format!("{}_{}", f.slots[len - 1], i - len + 2) })
        .collect()
}

pub fn of_graph(graph: &node::T) -> T {
    let infos = node::inspect(graph);
    let mut entries = BTreeMap::new();
    for info in &infos {
        let n = node::find(graph, info.id).expect("inspected node");
        let inputs = info.input_ids.iter().map(|id| Some(*id)).collect();
        entries.insert(info.id, Arc::new(Entry { node: n, inputs, factory: None }));
    }
    T(Arc::new(Inner {
        entries,
        order_rev: infos.iter().rev().map(|info| info.id).collect(),
        root: Some(node::id(graph)),
    }))
}

/// No nodes; build it up with `add_node` so every node keeps its factory.
pub fn empty() -> T {
    T(Arc::new(Inner { entries: BTreeMap::new(), order_rev: Vec::new(), root: None }))
}

pub fn root(value: &T) -> Option<i64> {
    value.0.root
}

pub fn set_root(node_id: i64, value: &T) -> Result<T, String> {
    if value.0.entries.contains_key(&node_id) {
        let mut inner = (*value.0).clone();
        inner.root = Some(node_id);
        Ok(T(Arc::new(inner)))
    } else {
        Err(format!("editable graph has no node #{node_id}"))
    }
}

fn info(entry: &Entry) -> NodeInfo {
    let n = &entry.node;
    NodeInfo {
        node: n.clone(),
        id: node::id(n),
        label: node::label(n).to_owned(),
        operation: node::operation(n).to_owned(),
        version: node::version(n),
        parameters: node::parameters(n).to_vec(),
        facts: node::facts(n).clone(),
        dependencies: node::dependencies(n),
        inputs: entry.inputs.clone(),
        has_parameters: node::has_parameters(n),
    }
}

pub fn inspect(value: &T) -> Vec<NodeInfo> {
    value.0.order_rev.iter().rev().filter_map(|id| value.0.entries.get(id).map(|e| info(e))).collect()
}

pub fn find(value: &T, node_id: i64) -> Option<node::T> {
    value.0.entries.get(&node_id).map(|e| e.node.clone())
}

pub fn inputs(value: &T, node_id: i64) -> Option<Vec<Option<i64>>> {
    value.0.entries.get(&node_id).map(|e| e.inputs.clone())
}

/// Input names retained by the entry's factory, or canonical `in0`, `in1`, ... for a graph
/// imported from code.
pub fn node_slot_names(value: &T, node_id: i64) -> Option<Vec<String>> {
    value.0.entries.get(&node_id).map(|entry| match &entry.factory {
        Some(factory) => slot_names_of(&factory.0, entry.inputs.len()),
        None => (0..entry.inputs.len()).map(|i| format!("in{i}")).collect(),
    })
}

pub fn node_factory_key(value: &T, node_id: i64) -> Option<String> {
    value.0.entries.get(&node_id).and_then(|e| e.factory.as_ref().map(|f| f.0.key.clone()))
}

pub fn node_factory_fields(value: &T, node_id: i64) -> Vec<FieldView> {
    value
        .0
        .entries
        .get(&node_id)
        .and_then(|e| e.factory.as_ref().map(|f| f.0.fields.clone()))
        .unwrap_or_default()
}

fn empty_geometry() -> node::T {
    static EMPTY: OnceLock<node::T> = OnceLock::new();
    EMPTY
        .get_or_init(|| {
            let positions = rdk::packed::Float3::of_owned(Vec::new(), Vec::new(), Vec::new()).expect("empty positions");
            let geometry = rdk::geometry::create(positions, rdk::topology::empty(0).expect("empty topology"), None, None, None)
                .expect("empty geometry");
            let parameters = format!(
                "data_id={};bytes={}",
                rdk::geometry::data_id(&geometry),
                rdk::geometry::payload_bytes(&geometry)
            );
            build::make_geometry(
                build::Args {
                    parameters: parameters.into_bytes(),
                    cook_mode: node::CookMode::Generator,
                    ..build::args("snapshot")
                },
                None,
                Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: geometry.clone(), instances: None })),
            )
        })
        .clone()
}

/// One compiled node per document entry. `compiled_inputs` are the compiled input nodes it was
/// built from, so a later compile can reuse `built` when both the entry and every input are
/// physically unchanged.
#[derive(Clone)]
struct CompiledEntry {
    source: Arc<Entry>,
    compiled_inputs: Vec<Option<node::T>>,
    built: Result<node::T, String>,
}

#[derive(Clone)]
pub struct Compiled(BTreeMap<i64, CompiledEntry>);

fn same_inputs(left: &[Option<node::T>], right: &[Option<node::T>]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(a, b)| match (a, b) {
            (Some(a), Some(b)) => node::ptr_eq(a, b),
            (None, None) => true,
            _ => false,
        })
}

/// The node already carries exactly these inputs: no rebuild needed.
fn wired_to(n: &node::T, inputs: &[Option<node::T>]) -> bool {
    let node_inputs = node::inputs(n);
    node_inputs.len() == inputs.len()
        && node_inputs.iter().zip(inputs).all(|(input, compiled)| matches!(compiled, Some(c) if node::ptr_eq(c, input)))
}

fn rebuild(entry: &Entry, compiled_inputs: &[Option<node::T>]) -> Result<node::T, String> {
    if entry.inputs.iter().all(Option::is_some) && wired_to(&entry.node, compiled_inputs) {
        return Ok(entry.node.clone());
    }
    match &entry.factory {
        Some(factory) if has_optional(&factory.0) => {
            let built = (factory.0.build)(compiled_inputs)?;
            let changes: Vec<(String, Value)> =
                node::parameter_fields(&entry.node).into_iter().map(|f| (f.name, f.current)).collect();
            let (n, _) = node::apply_parameters(&built, &changes)?;
            Ok(build::adopt_identity(&entry.node, &n))
        }
        _ => {
            let all: Vec<node::T> = compiled_inputs.iter().map(|n| n.clone().expect("connected input")).collect();
            Ok(build::rebuild_with_inputs(&entry.node, &all))
        }
    }
}

struct Compile<'a> {
    value: &'a T,
    previous: &'a BTreeMap<i64, CompiledEntry>,
    table: HashMap<i64, CompiledEntry>,
    visiting: HashSet<i64>,
    cycle_seen: bool,
}

impl Compile<'_> {
    fn build(&mut self, id: i64) -> Result<node::T, String> {
        if let Some(compiled) = self.table.get(&id) {
            return compiled.built.clone();
        }
        if self.visiting.contains(&id) {
            self.cycle_seen = true;
            return Err(format!("editable graph contains a cycle through node #{id}"));
        }
        let Some(entry) = self.value.0.entries.get(&id).cloned() else {
            return Err(format!("editable graph references missing node #{id}"));
        };
        self.visiting.insert(id);
        let count = entry.inputs.len();
        let mut compiled_inputs: Vec<Option<node::T>> = vec![None; count];
        let mut outcome: Result<(), String> = Ok(());
        for index in 0..count {
            match entry.inputs[index] {
                None => {
                    let optional = match &entry.factory {
                        Some(factory) => requirement(&factory.0, index) == Optional,
                        None => false,
                    };
                    if !optional {
                        outcome = Err(format!(
                            "node {} input {} is disconnected",
                            ocaml_quote(node::label(&entry.node)),
                            index
                        ));
                        break;
                    }
                }
                Some(input_id) => match self.build(input_id) {
                    Ok(input) => compiled_inputs[index] = Some(input),
                    Err(e) => {
                        outcome = Err(e);
                        break;
                    }
                },
            }
        }
        let built = outcome.and_then(|()| match self.previous.get(&id) {
            Some(reused) if Arc::ptr_eq(&reused.source, &entry) && same_inputs(&reused.compiled_inputs, &compiled_inputs) => {
                reused.built.clone()
            }
            _ => rebuild(&entry, &compiled_inputs),
        });
        self.visiting.remove(&id);
        // A cycle error names where the walk entered the cycle, so once one is seen only
        // successes are memoized.
        if built.is_ok() || !self.cycle_seen {
            self.table.insert(id, CompiledEntry { source: entry, compiled_inputs, built: built.clone() });
        }
        built
    }
}

/// Every node of the document compiled once. Pass the previous result to reuse each node whose
/// entry and compiled inputs are physically unchanged.
pub fn compile_all(previous: Option<&Compiled>, value: &T) -> Compiled {
    let empty = BTreeMap::new();
    let mut state = Compile {
        value,
        previous: previous.map_or(&empty, |c| &c.0),
        table: HashMap::new(),
        visiting: HashSet::new(),
        cycle_seen: false,
    };
    for (id, source) in value.0.entries.iter() {
        let built = state.build(*id);
        if !state.table.contains_key(id) {
            state.table.insert(*id, CompiledEntry { source: source.clone(), compiled_inputs: Vec::new(), built });
        }
    }
    Compiled(state.table.into_iter().collect())
}

pub fn compiled_node(compiled: &Compiled, node_id: i64) -> Result<node::T, String> {
    match compiled.0.get(&node_id) {
        Some(c) => c.built.clone(),
        None => Err(format!("editable graph has no node #{node_id}")),
    }
}

pub fn compile_node(value: &T, node_id: i64) -> Result<node::T, String> {
    if !value.0.entries.contains_key(&node_id) {
        return Err(format!("editable graph has no node #{node_id}"));
    }
    let empty = BTreeMap::new();
    let mut state = Compile { value, previous: &empty, table: HashMap::new(), visiting: HashSet::new(), cycle_seen: false };
    state.build(node_id)
}

/// Compile the document root.
pub fn compile(value: &T) -> Result<node::T, String> {
    match value.0.root {
        None => Err("editable graph has no output node".to_owned()),
        Some(node_id) => compile_node(value, node_id),
    }
}

/// Replace only the node payload. Its logical id and input arity must match.
pub fn replace_node(n: &node::T, value: &T) -> Result<T, String> {
    let id = node::id(n);
    let Some(entry) = value.0.entries.get(&id) else {
        return Err(format!("editable graph has no node #{id}"));
    };
    if node::ptr_eq(&entry.node, n) {
        return Ok(value.clone());
    }
    let arity = node::inputs(n).len();
    if entry.factory.is_none() && arity != entry.inputs.len() {
        return Err(format!(
            "replacement node {} changed input arity from {} to {}",
            ocaml_quote(node::label(n)),
            entry.inputs.len(),
            arity
        ));
    }
    let mut inner = (*value.0).clone();
    inner.entries.insert(id, Arc::new(Entry { node: n.clone(), ..(**entry).clone() }));
    Ok(T(Arc::new(inner)))
}

pub fn apply_parameters(value: &T, node_id: i64, changes: &[(String, Value)]) -> Result<(T, Effects), String> {
    let Some(n) = find(value, node_id) else {
        return Err(format!("editable graph has no node #{node_id}"));
    };
    let (n, effects) = node::apply_parameters(&n, changes)?;
    Ok((replace_node(&n, value)?, effects))
}

/// Add a node. By default (`inputs` absent) its existing node inputs are adopted when those ids
/// already belong to the document; other slots start disconnected.
pub fn add_node(inputs: Option<&[Option<i64>]>, factory: Option<&Factory>, n: &node::T, value: &T) -> Result<T, String> {
    let id = node::id(n);
    if value.0.entries.contains_key(&id) {
        return Err(format!("editable graph already contains node #{id}"));
    }
    let arity = match factory {
        None => node::inputs(n).len(),
        Some(factory) => factory.0.requirements.len(),
    };
    let inputs: Vec<Option<i64>> = match inputs {
        Some(inputs) => inputs.to_vec(),
        None => node::inputs(n)
            .iter()
            .map(|input| {
                let id = node::id(input);
                value.0.entries.contains_key(&id).then_some(id)
            })
            .collect(),
    };
    let fits = match factory {
        None => inputs.len() == arity,
        Some(factory) => arity_ok(&factory.0, inputs.len()),
    };
    if !fits {
        return Err(format!(
            "new node {} expects {} inputs, received {} slots",
            ocaml_quote(node::label(n)),
            arity,
            inputs.len()
        ));
    }
    if let Some(source) = inputs.iter().flatten().find(|source| !value.0.entries.contains_key(source)) {
        return Err(format!("new node {} references missing node #{}", ocaml_quote(node::label(n)), source));
    }
    let mut inner = (*value.0).clone();
    inner.entries.insert(id, Arc::new(Entry { node: n.clone(), inputs, factory: factory.cloned() }));
    inner.order_rev.insert(0, id);
    Ok(T(Arc::new(inner)))
}

pub fn remove_nodes(ids: &[i64], value: &T) -> T {
    let removed: BTreeSet<i64> = ids.iter().copied().collect();
    let entries = value
        .0
        .entries
        .iter()
        .filter(|(id, _)| !removed.contains(id))
        .map(|(id, entry)| {
            let inputs = entry
                .inputs
                .iter()
                .map(|input| match input {
                    Some(input) if removed.contains(input) => None,
                    other => *other,
                })
                .collect();
            (*id, Arc::new(Entry { inputs, ..(**entry).clone() }))
        })
        .collect();
    T(Arc::new(Inner {
        entries,
        order_rev: value.0.order_rev.iter().copied().filter(|id| !removed.contains(id)).collect(),
        root: value.0.root.filter(|id| !removed.contains(id)),
    }))
}

fn depends_on(value: &T, node_id: i64, candidate: i64) -> bool {
    fn visit(value: &T, id: i64, candidate: i64, seen: &mut HashSet<i64>) -> bool {
        if id == candidate {
            return true;
        }
        if !seen.insert(id) {
            return false;
        }
        match value.0.entries.get(&id) {
            None => false,
            Some(entry) => entry.inputs.iter().flatten().any(|input| visit(value, *input, candidate, seen)),
        }
    }
    visit(value, node_id, candidate, &mut HashSet::new())
}

fn rebuild_if_connected(entries: &BTreeMap<i64, Arc<Entry>>, entry: &Entry, inputs: &[Option<i64>]) -> node::T {
    let lookup = |input: &Option<i64>| input.and_then(|id| entries.get(&id).map(|source| source.node.clone()));
    match &entry.factory {
        Some(factory) if has_optional(&factory.0) => {
            // Optional slots rebuild through the factory, which knows presence.
            let nodes: Vec<Option<node::T>> = inputs.iter().map(lookup).collect();
            rebuild(entry, &nodes).unwrap_or_else(|_| entry.node.clone())
        }
        _ => {
            let nodes: Vec<Option<node::T>> = inputs.iter().map(lookup).collect();
            if nodes.iter().all(Option::is_some) {
                let all: Vec<node::T> = nodes.into_iter().flatten().collect();
                build::rebuild_with_inputs(&entry.node, &all)
            } else {
                entry.node.clone()
            }
        }
    }
}

pub fn connect(source: i64, consumer: i64, input_index: i64, value: &T) -> Result<T, String> {
    if !value.0.entries.contains_key(&source) {
        return Err(format!("editable graph has no source node #{source}"));
    }
    let Some(entry) = value.0.entries.get(&consumer) else {
        return Err(format!("editable graph has no consumer node #{consumer}"));
    };
    let len = entry.inputs.len() as i64;
    let grows = match &entry.factory {
        Some(f) => !f.0.requirements.is_empty() && repeating(last_requirement(&f.0)),
        None => false,
    };
    if input_index < 0 || input_index > len || (input_index == len && !grows) {
        return Err(format!("node {} has no input {}", ocaml_quote(node::label(&entry.node)), input_index));
    }
    if depends_on(value, source, consumer) {
        return Err("connection would create a procedural cycle".to_owned());
    }
    // a rest slot grows by connecting one past its last input
    let mut inputs = entry.inputs.clone();
    if input_index == len {
        inputs.push(None);
    }
    inputs[input_index as usize] = Some(source);
    let n = rebuild_if_connected(&value.0.entries, entry, &inputs);
    let mut inner = (*value.0).clone();
    inner.entries.insert(consumer, Arc::new(Entry { node: n, inputs, factory: entry.factory.clone() }));
    Ok(T(Arc::new(inner)))
}

pub fn disconnect(consumer: i64, input_index: i64, value: &T) -> Result<T, String> {
    let Some(entry) = value.0.entries.get(&consumer) else {
        return Err(format!("editable graph has no consumer node #{consumer}"));
    };
    if input_index < 0 || input_index >= entry.inputs.len() as i64 {
        return Err(format!("node {} has no input {}", ocaml_quote(node::label(&entry.node)), input_index));
    }
    let mut inputs = entry.inputs.clone();
    inputs[input_index as usize] = None;
    let mut inner = (*value.0).clone();
    inner.entries.insert(consumer, Arc::new(Entry { inputs, ..(**entry).clone() }));
    Ok(T(Arc::new(inner)))
}

// ---------------------------------------------------------------- factories

fn slot_names(arity: usize, names: Option<&[String]>) -> Vec<String> {
    match names {
        None => (0..arity).map(|i| format!("in{i}")).collect(),
        Some(names) => {
            let distinct: BTreeSet<&String> = names.iter().collect();
            assert!(
                names.len() == arity && names.iter().all(|n| !n.trim().is_empty()) && distinct.len() == arity,
                "Edit_graph factory slots must have one distinct name per input"
            );
            names.to_vec()
        }
    }
}

fn slot_types(arity: usize, names: Option<&[String]>) -> Vec<String> {
    match names {
        None => vec!["geometry".to_owned(); arity],
        Some(names) => {
            let valid = |n: &String| !n.is_empty() && !n.chars().any(|c| matches!(c, ' ' | '\t' | '\n' | '\r'));
            assert!(
                names.len() == arity && names.iter().all(valid),
                "Edit_graph factory input_types must have one valid type name per input"
            );
            names.to_vec()
        }
    }
}

fn keyword_slots(slots: &[String], requirements: &[InputRequirement], fields: &[FieldView], names: &[String]) -> Vec<String> {
    let distinct: BTreeSet<&String> = names.iter().collect();
    let bad = distinct.len() != names.len()
        || names.iter().any(|name| match slots.iter().position(|s| s == name) {
            None => true,
            Some(index) => {
                requirements[index] != Required
                    || fields.iter().any(|f| &f.name == name || f.vec3.as_ref().is_some_and(|(group, _)| group == name))
            }
        });
    assert!(
        !bad,
        "Edit_graph factory keyword_inputs must name distinct required slots without field conflicts"
    );
    names.to_vec()
}

/// Declaration of a factory (OCaml labelled arguments of `factory`/`factory_slots`).
pub struct FactoryArgs {
    pub operation: Option<String>,
    pub slots: Option<Vec<String>>,
    pub input_types: Option<Vec<String>>,
    pub keyword_inputs: Vec<String>,
    pub fields: Vec<FieldView>,
    pub key: String,
    pub label: String,
    pub category: Vec<String>,
}

pub fn factory_args(key: &str, label: &str, category: &[&str]) -> FactoryArgs {
    FactoryArgs {
        operation: None,
        slots: None,
        input_types: None,
        keyword_inputs: Vec::new(),
        fields: Vec::new(),
        key: key.to_owned(),
        label: label.to_owned(),
        category: category.iter().map(|c| (*c).to_owned()).collect(),
    }
}

fn make_factory(what: &str, args: FactoryArgs, requirements: Vec<InputRequirement>, build: BuildFn) -> Factory {
    let operation = args.operation.unwrap_or_else(|| args.key.clone());
    assert!(
        !args.key.trim().is_empty()
            && !operation.trim().is_empty()
            && !args.label.trim().is_empty()
            && !args.category.is_empty()
            && !args.category.iter().any(|item| item.trim().is_empty()),
        "Edit_graph.{what} names must not be blank"
    );
    let arity = requirements.len();
    let slots = slot_names(arity, args.slots.as_deref());
    let input_types = slot_types(arity, args.input_types.as_deref());
    let keyword_inputs = keyword_slots(&slots, &requirements, &args.fields, &args.keyword_inputs);
    Factory(Arc::new(FactoryData {
        key: args.key,
        operation,
        label: args.label,
        category: args.category,
        // share_default_values is an OCaml memory optimisation; values here are owned.
        fields: args.fields,
        requirements,
        slots,
        input_types,
        keyword_inputs,
        build,
        facts: OnceLock::new(),
    }))
}

/// A factory of `arity` required inputs.
pub fn factory(args: FactoryArgs, arity: usize, build: Arc<dyn Fn(&[node::T]) -> Result<node::T, String> + Send + Sync>) -> Factory {
    let build: BuildFn = Arc::new(move |inputs: &[Option<node::T>]| {
        let all: Vec<node::T> = inputs.iter().map(|n| n.clone().expect("required input")).collect();
        build(&all)
    });
    make_factory("factory", args, vec![Required; arity], build)
}

/// A factory with explicit per-slot requirements.
pub fn factory_slots(args: FactoryArgs, inputs: Vec<InputRequirement>, build: BuildFn) -> Factory {
    assert!(!inputs.is_empty(), "Edit_graph.factory_slots requires at least one input slot");
    assert!(
        !inputs[..inputs.len() - 1].iter().any(|r| repeating(*r)),
        "Edit_graph.factory_slots: only the last input may be Rest"
    );
    make_factory("factory_slots", args, inputs, build)
}

pub fn factory_key(value: &Factory) -> &str {
    &value.0.key
}
pub fn factory_operation(value: &Factory) -> &str {
    &value.0.operation
}
pub fn factory_label(value: &Factory) -> &str {
    &value.0.label
}
pub fn factory_category(value: &Factory) -> &[String] {
    &value.0.category
}
pub fn factory_fields(value: &Factory) -> &[FieldView] {
    &value.0.fields
}

/// Declaration at default parameters, computed once without cooking. Instance facts are rebuilt
/// with the node's parameters and may select other names.
pub fn factory_facts(value: &Factory) -> &Facts {
    value.0.facts.get_or_init(|| {
        let inputs: Vec<Option<node::T>> = value
            .0
            .requirements
            .iter()
            .map(|r| match r {
                Required | Rest => Some(empty_geometry()),
                Optional | OptionalRest => None,
            })
            .collect();
        match (value.0.build)(&inputs) {
            Ok(n) => node::facts(&n).clone(),
            Err(message) => panic!("Edit_graph factory defaults: {message}"),
        }
    })
}

pub fn factory_arity(value: &Factory) -> usize {
    value.0.requirements.len()
}
pub fn factory_inputs(value: &Factory) -> Vec<InputRequirement> {
    value.0.requirements.clone()
}
/// Named inputs, in slot order; defaults to `in0`, `in1`, ...
pub fn factory_slot_names(value: &Factory) -> &[String] {
    &value.0.slots
}
/// Serialized input type names, defaulting to `geometry`.
pub fn factory_input_types(value: &Factory) -> &[String] {
    &value.0.input_types
}
/// Required physical slots exposed as typed keyword parameters by Flow.
pub fn factory_keyword_inputs(value: &Factory) -> &[String] {
    &value.0.keyword_inputs
}

pub fn instantiate(value: &Factory, inputs: &[node::T]) -> Result<node::T, String> {
    let arity = value.0.requirements.len();
    if !arity_ok(&value.0, inputs.len()) {
        return Err(format!("{} expects {} input{}", value.0.label, arity, if arity == 1 { "" } else { "s" }));
    }
    let options: Vec<Option<node::T>> = inputs.iter().cloned().map(Some).collect();
    (value.0.build)(&options)
}

fn disconnected_placeholder() -> node::T {
    build::make_geometry(
        build::Args {
            label: Some("disconnected input".to_owned()),
            cook_mode: node::CookMode::Generator,
            ..build::args("disconnected_input")
        },
        None,
        Arc::new(|_, _, _| {
            Err(crate::support::diagnostic("disconnected_input", "node input is not connected"))
        }),
    )
}

pub fn instantiate_optional(value: &Factory, inputs: &[Option<node::T>]) -> Result<node::T, String> {
    let arity = value.0.requirements.len();
    if !arity_ok(&value.0, inputs.len()) {
        return Err(format!("{} expects {} input slot{}", value.0.label, arity, if arity == 1 { "" } else { "s" }));
    }
    let mut placeholder: Option<node::T> = None;
    let resolved: Vec<Option<node::T>> = inputs
        .iter()
        .enumerate()
        .map(|(index, input)| match (requirement(&value.0, index), input) {
            (_, Some(n)) => Some(n.clone()),
            (Optional | Rest | OptionalRest, None) => None,
            (Required, None) => Some(placeholder.get_or_insert_with(disconnected_placeholder).clone()),
        })
        .collect();
    (value.0.build)(&resolved)
}
