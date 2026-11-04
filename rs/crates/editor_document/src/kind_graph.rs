//! Port of lib/editor_document/kind_graph.ml: the scene's objects and the World's layers as plain
//! data, each an instance of a kind (a typed schema and its descriptor), with its values and at
//! most one input. No SOP node, no cook.
use crate::settings;
use param::{Effects, FieldView, Schema, Value};
use sop::decl;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(Clone)]
pub struct Kind {
    pub key: String,
    pub label: String,
    pub category: Vec<String>,
    pub defaults: settings::T,
}

pub fn kind_make(key: &str, label: &str, category: &[&str], schema: &Arc<Schema>) -> Kind {
    Kind {
        key: key.to_owned(),
        label: label.to_owned(),
        category: category.iter().map(|c| (*c).to_owned()).collect(),
        defaults: settings::make(schema, schema.default().to_vec()),
    }
}

/// The schema's fields at their defaults.
pub fn kind_fields(kind: &Kind) -> Vec<FieldView> {
    settings::fields(&kind.defaults)
}

#[derive(Clone)]
pub struct Node {
    pub id: i64,
    pub kind: String,
    pub label: String,
    pub values: settings::T,
    pub input: Option<i64>,
}

/// In lowering order. A list: scenes hold tens of objects.
pub type T = Vec<Node>;

pub fn empty() -> T {
    Vec::new()
}

static NEXT_ID: AtomicI64 = AtomicI64::new(1);

pub fn instance(id: Option<i64>, label: &str, kind: &Kind) -> Node {
    Node {
        id: id.unwrap_or_else(|| NEXT_ID.fetch_add(1, Ordering::SeqCst)),
        kind: kind.key.clone(),
        label: label.to_owned(),
        values: kind.defaults.clone(),
        input: None,
    }
}

pub fn fields(node: &Node) -> Vec<FieldView> {
    settings::fields(&node.values)
}

pub fn field(node: &Node, name: &str) -> Option<Value> {
    fields(node).into_iter().find(|f| f.name == name).map(|f| f.current)
}

pub fn find(graph: &[Node], id: i64) -> Option<Node> {
    graph.iter().find(|node| node.id == id).cloned()
}

pub fn ids(kind: &str, graph: &[Node]) -> Vec<i64> {
    graph.iter().filter(|node| node.kind == kind).map(|node| node.id).collect()
}

pub fn add(graph: &[Node], node: Node) -> T {
    let mut out = graph.to_vec();
    out.push(node);
    out
}

pub fn replace(graph: &[Node], node: &Node) -> T {
    graph.iter().map(|n| if n.id == node.id { node.clone() } else { n.clone() }).collect()
}

pub fn update(graph: &[Node], id: i64, f: &dyn Fn(Node) -> Node) -> T {
    graph.iter().map(|n| if n.id == id { f(n.clone()) } else { n.clone() }).collect()
}

/// The nodes gone, and every input that named one of them.
pub fn remove(ids: &[i64], graph: &[Node]) -> T {
    graph
        .iter()
        .filter(|n| !ids.contains(&n.id))
        .map(|n| match n.input {
            Some(i) if ids.contains(&i) => Node { input: None, ..n.clone() },
            _ => n.clone(),
        })
        .collect()
}

/// Field values checked by the kind's schema.
pub fn apply(node: &Node, changes: &[(String, Value)]) -> Result<(Node, Effects), String> {
    settings::apply(&node.values, changes).map(|(values, effects)| (Node { values, ..node.clone() }, effects))
}

pub fn set_fields(graph: &[Node], id: i64, changes: &[(String, Value)]) -> Result<(T, Effects), String> {
    match find(graph, id) {
        None => Err(format!("no object #{id}")),
        Some(node) => apply(&node, changes).map(|(node, effects)| (replace(graph, &node), effects)),
    }
}

/// From the node no other takes as input down its inputs: a World's layers, top first.
pub fn stack(graph: &[Node]) -> Vec<Node> {
    let consumed: Vec<i64> = graph.iter().filter_map(|n| n.input).collect();
    let Some(top) = graph.iter().find(|n| !consumed.contains(&n.id)) else {
        return Vec::new();
    };
    let mut acc: Vec<Node> = vec![top.clone()];
    let mut node = top.clone();
    loop {
        match node.input.and_then(|i| find(graph, i)) {
            Some(below) if !acc.iter().any(|a| a.id == below.id) => {
                acc.push(below.clone());
                node = below;
            }
            _ => break,
        }
    }
    acc
}

fn current(node: &Node) -> Vec<(String, Value)> {
    fields(node).into_iter().map(|f| (f.name, f.current)).collect()
}

/// The same ids, kinds, labels, inputs and values in the same order.
pub fn same(a: &[Node], b: &[Node]) -> bool {
    a.len() == b.len()
        && a.iter().zip(b).all(|(a, b)| {
            a.id == b.id
                && a.kind == b.kind
                && a.label == b.label
                && a.input == b.input
                && (settings::same_values(&a.values, &b.values) || current(a) == current(b))
        })
}

// ---- schema construction helpers (the `[@sop.*]` attributes of the parameters records) ----

/// A float field: name, label, folder, default, min, max, hard_min, hard_max.
#[allow(clippy::too_many_arguments)]
pub fn fl(name: &'static str, label: &'static str, folder: &'static str, default: f64, min: f64, max: f64, hard_min: Option<f64>, hard_max: Option<f64>) -> decl::Spec {
    decl::Spec { label, folder, ..decl::spec(name, decl::float_kind(min, max, hard_min, hard_max), Value::Float(default)) }
}

#[allow(clippy::too_many_arguments)]
pub fn int(name: &'static str, label: &'static str, folder: &'static str, default: i64, min: i64, max: i64, hard_min: Option<i64>, hard_max: Option<i64>) -> decl::Spec {
    decl::Spec { label, folder, ..decl::spec(name, decl::int_kind(min, max, hard_min, hard_max), Value::Int(default)) }
}

pub fn tog(name: &'static str, label: &'static str, folder: &'static str, default: bool) -> decl::Spec {
    decl::Spec { label, folder, ..decl::spec(name, decl::toggle_kind(), Value::Bool(default)) }
}

pub fn chc(name: &'static str, label: &'static str, options: &[&str], default: &str) -> decl::Spec {
    let kind = param::choice(options.iter().map(|o| (*o).to_owned()).collect()).expect("valid choice");
    decl::Spec { label, ..decl::spec(name, kind, Value::Choice(default.to_owned())) }
}

pub fn schema_of(specs: Vec<decl::Spec>) -> Arc<Schema> {
    decl::schema(&decl::META, specs)
}

/// A record read by field name (the typed `parameters` records of the OCaml, positional here).
#[derive(Clone, Debug)]
pub struct Rec {
    pub names: Vec<String>,
    pub values: Vec<Value>,
}

impl Rec {
    pub fn of(schema: &Schema, values: Vec<Value>) -> Rec {
        Rec { names: schema.fields().iter().map(|f| f.name().to_owned()).collect(), values }
    }
    fn at(&self, name: &str) -> &Value {
        let i = self.names.iter().position(|n| n == name).unwrap_or_else(|| panic!("no field {name}"));
        &self.values[i]
    }
    pub fn f(&self, name: &str) -> f64 {
        match self.at(name) {
            Value::Float(x) => *x,
            other => panic!("field {name} is {other:?}"),
        }
    }
    pub fn i(&self, name: &str) -> i64 {
        match self.at(name) {
            Value::Int(x) => *x,
            other => panic!("field {name} is {other:?}"),
        }
    }
    pub fn b(&self, name: &str) -> bool {
        match self.at(name) {
            Value::Bool(x) => *x,
            other => panic!("field {name} is {other:?}"),
        }
    }
    pub fn c(&self, name: &str) -> &str {
        match self.at(name) {
            Value::Choice(x) => x,
            other => panic!("field {name} is {other:?}"),
        }
    }
}
