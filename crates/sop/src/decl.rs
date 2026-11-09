//! Rust twin of what `[@@deriving sop_params, sop_node]` (ppx/ppx_rays) generated: the schema of a
//! node's parameters and its arity-checked, rebuildable constructor and factory.
//!
//! A node declaration is plain data: a `Meta` const (the `[@@sop.node_*]` attributes), a
//! `Vec<Spec>` (one `[@sop.*]`-annotated record field each), and an `operator` fn that reads
//! schema values into the native kernel parameters. See PORTING.md for the pattern.
use std::sync::Arc;

use param::{Field, Impact, Kind, Schema, Value};

use crate::edit_graph::{self, Factory, InputRequirement};
use crate::node::{self, build, Elementwise, Facts, Topology};

/// `[@@sop.node_facts { ... }]`: the refinement of the conservative facts.
#[derive(Clone, Copy)]
pub struct FactsDecl {
    pub elementwise: Elementwise,
    pub reads: &'static [&'static str],
    pub writes: &'static [&'static str],
    pub topology: Topology,
    pub exact: bool,
}

/// `(label, parameter values, inputs one per slot) -> unparameterized operator node`. The slice
/// holds one entry per slot; a required slot is always `Some`; a rest slot continues to the end
/// (use `connected`). The operator may refuse by wiring, so it returns a `Result`.
pub type Operator = fn(&str, &[Value], &[Option<node::T>]) -> Result<node::T, String>;
/// `[@@sop.validate f]`.
pub type Validate = fn(&[Value]) -> Result<(), String>;
/// The optional `?schema` callback of `<t>_build`: presentation derived from the parameters and the
/// constructed node (e.g. Switch's choices).
pub type DynamicSchema = fn(&[Value], &node::T) -> Arc<Schema>;

/// The `[@@sop.node_*]` attributes of one node.
#[derive(Clone, Copy)]
pub struct Meta {
    /// `sop.node_key`: also the default `operation`.
    pub key: &'static str,
    /// `sop.node_operation`.
    pub operation: Option<&'static str>,
    pub label: &'static str,
    /// `sop.node_category`, `/`-separated.
    pub category: &'static str,
    /// `sop.node_inputs`.
    pub inputs: usize,
    /// `sop.node_slots`, comma separated; absent: `in0`, `in1`, ...
    pub slots: Option<&'static str>,
    /// `sop.node_optional`, slot indices.
    pub optional: &'static [usize],
    /// `sop.node_rest`: the repeating last slot.
    pub rest: Option<usize>,
    /// `sop.node_types`.
    pub types: Option<&'static [&'static str]>,
    /// `sop.node_keywords`.
    pub keywords: &'static [&'static str],
    pub facts: Option<FactsDecl>,
    pub validate: Option<Validate>,
    /// The record type's name: the schema name.
    pub schema_name: &'static str,
}

pub const META: Meta = Meta {
    key: "",
    operation: None,
    label: "",
    category: "",
    inputs: 0,
    slots: None,
    optional: &[],
    rest: None,
    types: None,
    keywords: &[],
    facts: None,
    validate: None,
    schema_name: "parameters",
};

/// One `[@sop.*]`-annotated record field (`Param.field`). Build with `spec` and struct update.
pub struct Spec {
    pub name: &'static str,
    /// `sop.label`; empty keeps the field name.
    pub label: &'static str,
    pub description: Option<&'static str>,
    /// `sop.folder`, `/`-separated.
    pub folder: &'static str,
    pub impact: Impact,
    pub primary: bool,
    pub unit: Option<&'static str>,
    /// `sop.vec3 "group"` with the component index 0..2.
    pub vec3: Option<(&'static str, i64)>,
    pub kind: Kind,
    pub default: Value,
}

pub fn spec(name: &'static str, kind: Kind, default: Value) -> Spec {
    Spec {
        name,
        label: "",
        description: None,
        folder: "",
        impact: Impact::Cook,
        primary: false,
        unit: None,
        vec3: None,
        kind,
        default,
    }
}

/// `[@sop.min a] [@sop.max b] [@sop.hard_min c] [@sop.hard_max d]` on an `int` field.
pub fn int_kind(min: i64, max: i64, hard_min: Option<i64>, hard_max: Option<i64>) -> Kind {
    param::integer(min, max, hard_min, hard_max).expect("valid integer range")
}

/// The same on a `float` field.
pub fn float_kind(min: f64, max: f64, hard_min: Option<f64>, hard_max: Option<f64>) -> Kind {
    param::floating(min, max, hard_min, hard_max).expect("valid float range")
}

pub fn toggle_kind() -> Kind {
    param::toggle()
}

pub fn text_kind() -> Kind {
    param::text()
}

fn field(spec: Spec) -> Field {
    let mut field = param::field(spec.name.to_owned(), spec.kind, spec.default).expect("valid parameter field");
    if !spec.label.is_empty() {
        field = field.label(spec.label.to_owned()).expect("valid label");
    }
    if let Some(description) = spec.description {
        field = field.description(description.to_owned());
    }
    let folder: Vec<String> =
        spec.folder
            .split('/')
            .filter(|item| !item.trim().is_empty())
            .map(str::to_owned)
            .collect();
    if !folder.is_empty() {
        field = field.folder(folder).expect("valid folder");
    }
    field = field.impact(spec.impact).primary(spec.primary);
    if let Some(unit) = spec.unit {
        field = field.unit(unit.to_owned());
    }
    if let Some((group, component)) = spec.vec3 {
        field = field.vec3(group.to_owned(), component).expect("valid vec3 component");
    }
    field
}

/// `<t>_schema`: the schema of a declaration's fields.
pub fn schema(meta: &Meta, specs: Vec<Spec>) -> Arc<Schema> {
    Arc::new(param::schema(meta.schema_name.to_owned(), specs.into_iter().map(field).collect()).expect("valid schema"))
}

/// The connected nodes of a (rest) slice, in order.
pub fn connected(inputs: &[Option<node::T>]) -> Vec<node::T> {
    inputs.iter().flatten().cloned().collect()
}

fn slot_names(meta: &Meta) -> Vec<String> {
    match meta.slots {
        None => (0..meta.inputs).map(|i| format!("in{i}")).collect(),
        Some(text) => text.split(',').map(|s| s.trim().to_owned()).collect(),
    }
}

fn requirements(meta: &Meta) -> Vec<InputRequirement> {
    (0..meta.inputs)
        .map(|index| {
            let optional = meta.optional.contains(&index);
            if Some(index) == meta.rest {
                if optional { InputRequirement::OptionalRest } else { InputRequirement::Rest }
            } else if optional {
                InputRequirement::Optional
            } else {
                InputRequirement::Required
            }
        })
        .collect()
}

/// The declaration's static part: schema, metadata and operator.
#[derive(Clone)]
pub struct Decl {
    pub meta: Meta,
    pub schema: Arc<Schema>,
    pub operator: Operator,
    pub dynamic_schema: Option<DynamicSchema>,
}

/// `<t>_build ?schema operator`: arity-check the inputs, run the operator, refine its facts, attach
/// the schema through `Node.parameterize` (which derives the cache identity from every cook
/// field), and rebuild itself after edits. `inputs` is the physical slot list: `Some` for a
/// connected slot, `None` for an absent optional one.
pub fn build(decl: &Decl, label: &str, inputs: &[Option<node::T>], values: &[Value]) -> Result<node::T, String> {
    let meta = &decl.meta;
    let rest = meta.rest;
    let optional: Vec<usize> = meta.optional.iter().copied().filter(|i| Some(*i) != rest).collect();
    let fixed = rest.unwrap_or(meta.inputs);
    let plural = if meta.inputs == 1 { "" } else { "s" };
    // shape check and the arity message of each case
    if rest.is_some() && meta.optional.is_empty() {
        if inputs.len() < meta.inputs || inputs.iter().any(Option::is_none) {
            return Err(format!("{} expects at least {} inputs", meta.label, meta.inputs));
        }
    } else if meta.optional.is_empty() && rest.is_none() {
        if inputs.len() != meta.inputs || inputs.iter().any(Option::is_none) {
            return Err(format!("{} expects {} input{}", meta.label, meta.inputs, plural));
        }
    } else {
        let shape_ok = if rest.is_some() { inputs.len() >= fixed } else { inputs.len() == fixed };
        let required_ok = (0..fixed).all(|i| optional.contains(&i) || inputs[i].is_some());
        if !shape_ok || !required_ok {
            return Err(format!(
                "{} expects {} input slot{} with every required input connected",
                meta.label, meta.inputs, plural
            ));
        }
    }
    if let Some(validate) = meta.validate {
        validate(values)?;
    }
    let mut made = (decl.operator)(label, values, inputs)?;
    if let Some(facts) = &meta.facts {
        let refined = Facts {
            elementwise: facts.elementwise,
            reads: facts.reads.iter().map(|s| (*s).to_owned()).collect(),
            writes: facts.writes.iter().map(|s| (*s).to_owned()).collect(),
            topology: facts.topology,
            exact: facts.exact,
            ..node::facts(&made).clone()
        };
        made = build::with_facts(refined, &made);
    }
    let schema = match decl.dynamic_schema {
        Some(dynamic) => dynamic(values, &made),
        None => Arc::clone(&decl.schema),
    };
    // Optional slots rebuild from the physical input list using the presence captured now, so
    // sparse connections keep their slot.
    let present: Vec<bool> = optional.iter().map(|i| inputs[*i].is_some()).collect();
    let rebuild_decl = decl.clone();
    let has_optional = !meta.optional.is_empty();
    let rebuild: node::RebuildFn = Arc::new(move |label: &str, physical: &[node::T], values: &[Value]| {
        if !has_optional {
            let slots: Vec<Option<node::T>> = physical.iter().cloned().map(Some).collect();
            return build(&rebuild_decl, label, &slots, values);
        }
        let meta = &rebuild_decl.meta;
        let lost = || format!("{} lost a connected input while rebuilding", meta.label);
        let mut cursor = 0;
        let mut slots: Vec<Option<node::T>> = Vec::new();
        for index in 0..fixed {
            let take = match optional.iter().position(|i| *i == index) {
                Some(flag) => present[flag],
                None => true,
            };
            if take {
                slots.push(Some(physical.get(cursor).ok_or_else(lost)?.clone()));
                cursor += 1;
            } else {
                slots.push(None);
            }
        }
        if rest.is_some() {
            slots.extend(physical[cursor..].iter().cloned().map(Some));
        } else if cursor != physical.len() {
            return Err(format!("{} received extra inputs while rebuilding", meta.label));
        }
        build(&rebuild_decl, label, &slots, values)
    });
    node::parameterize(schema, values.to_vec(), rebuild, &made)
}

/// A native node with a static schema; construction and rebuilds share the declaration path.
pub fn native(meta: &Meta, specs: Vec<Spec>, operator: Operator) -> Factory {
    factory(&Decl { meta: *meta, schema: schema(meta, specs), operator, dynamic_schema: None })
}

/// `<t>_factory`: the editor factory of a declaration, built at the default parameter values.
pub fn factory(decl: &Decl) -> Factory {
    let meta = decl.meta;
    let key = meta.key;
    let defaults: Vec<Value> = decl.schema.default().to_vec();
    let fields = decl.schema.view(&defaults).expect("default parameter view");
    let category: Vec<String> =
        meta.category.split('/').map(|s| s.trim().to_owned()).filter(|s| !s.is_empty()).collect();
    assert!(
        !meta.key.trim().is_empty()
            && !meta.label.trim().is_empty()
            && !category.is_empty(),
        "sop_node key, label, and category must not be blank"
    );
    let mut args = edit_graph::FactoryArgs {
        operation: meta.operation.map(str::to_owned),
        slots: meta.slots.map(|_| slot_names(&meta)),
        input_types: meta.types.map(|types| types.iter().map(|t| (*t).to_owned()).collect()),
        keyword_inputs: meta.keywords.iter().map(|k| (*k).to_owned()).collect(),
        fields,
        key: key.to_owned(),
        label: meta.label.to_owned(),
        category,
    };
    args.keyword_inputs.dedup();
    let decl = decl.clone();
    let filter_rest = meta.rest.is_some() && meta.optional.is_empty();
    let construct = move |inputs: &[Option<node::T>]| -> Result<node::T, String> {
        if filter_rest {
            let kept: Vec<Option<node::T>> = inputs.iter().filter(|n| n.is_some()).cloned().collect();
            build(&decl, key, &kept, &defaults)
        } else {
            build(&decl, key, inputs, &defaults)
        }
    };
    if meta.optional.is_empty() && meta.rest.is_none() {
        let n = meta.inputs;
        edit_graph::factory(
            args,
            n,
            Arc::new(move |nodes: &[node::T]| {
                let slots: Vec<Option<node::T>> = nodes.iter().cloned().map(Some).collect();
                construct(&slots)
            }),
        )
    } else {
        edit_graph::factory_slots(args, requirements(&meta), Arc::new(construct))
    }
}
