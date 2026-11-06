//! Port of lib/flow_sop/attribute_metadata.ml: topology-preserving attribute metadata operations
//! over cooked geometry.
//!
//! The OCaml node constructors raise a Flow diagnostic on a bad owner or conflict word; here they
//! return `Result<node::T, Diagnostic>` (the same diagnostic, as a value).
use flow_eval::Value;
use flow_syntax::lisp;
use flow_syntax::arena::{Node, SyntaxBuilder};
use flow_syntax::ty;
use rays_diagnostic::Diagnostic;
use rdk::attribute::{self, Owner};
use rdk::attribute_lifecycle::{self, RenameConflict, RenameRule, SwapMethod, SwapRule};
use rdk::attribute_ops;
use rdk::attribute_pattern;
use rdk::geometry;
use rdk::group_ops::{self, DeleteParams, DeleteRule};
use rdk::{error as rdk_error, group_ops::Owner as GroupOwner};
use sop::context::{self, dependencies};
use sop::node::{self, build, CookMode, Elementwise, Facts, GeometryCooked, InputUse, Topology};
use std::sync::Arc;

use crate::port::error;

/// `Flow.Lisp.flat` of a vec of strings: the cache text of a node's text parameters.
fn key(cells: &[&str]) -> String {
    let mut builder = SyntaxBuilder::new();
    let items: Vec<_> = cells.iter().map(|cell| {
        let bytes = builder.bytes(cell.as_bytes());
        builder.push(0, Node::Str(bytes), rays_diagnostic::Span { start: 0, finish: 0 }, None)
    }).collect();
    let range = builder.range(&items);
    let root = builder.push(0, Node::Vec(range), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    lisp::flat(&builder.freeze().handle(root))
}

fn pattern_error(message: String) -> Diagnostic {
    error("E_METADATA_PATTERN", &message)
}

pub fn glob_rewrite(pattern: &str, replacement: &str, text: &str) -> Result<String, Diagnostic> {
    let rewrite =
        attribute_pattern::compile_rewrite(pattern, replacement).map_err(|e| pattern_error(rdk_error::message(&e).to_owned()))?;
    Ok(match attribute_pattern::rewrite(&rewrite, text) {
        Some(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
        None => text.to_owned(),
    })
}

pub fn glob_rewrite_set(pattern: &str, replacement: &str, text: &str) -> Result<String, Diagnostic> {
    let rewrite = attribute_pattern::compile_rewrite_set(pattern, replacement)
        .map_err(|e| pattern_error(rdk_error::message(&e).to_owned()))?;
    Ok(match attribute_pattern::rewrite_set(&rewrite, text) {
        Some(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
        None => text.to_owned(),
    })
}

pub fn glob_rewrite_set_matches(pattern: &str, replacement: &str, text: &str) -> Result<bool, Diagnostic> {
    let rewrite = attribute_pattern::compile_rewrite_set(pattern, replacement)
        .map_err(|e| pattern_error(rdk_error::message(&e).to_owned()))?;
    Ok(attribute_pattern::rewrite_set(&rewrite, text).is_some())
}

pub fn glob_literal(text: &str) -> Result<String, Diagnostic> {
    if text.is_empty() {
        return Err(pattern_error("A literal glob must not be empty.".into()));
    }
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        if matches!(c, '*' | '?' | '[' | ']' | '\\' | '^' | ' ' | '\t' | '\n' | '\r' | '\u{c}') {
            out.push('\\');
        }
        out.push(c);
    }
    Ok(out)
}

/// The decision of a guarded deletion over every input as the node cooks.
pub type GuardDecision = Arc<dyn Fn(&context::T, &[geometry::T]) -> Result<bool, Diagnostic> + Send + Sync>;

/// A guard: its cache key, dependencies, further inputs and decision over every input. It
/// deletes only when it decides so as the node cooks.
#[derive(Clone)]
pub struct Guard {
    pub key: String,
    pub dependencies: dependencies::T,
    pub inputs: Vec<node::T>,
    pub decide: GuardDecision,
}

fn structured_error(failure: &rdk_error::T) -> Diagnostic {
    rays_diagnostic::error(
        rdk_error::code(failure).to_owned(),
        format!("{} could not produce valid geometry", rdk_error::operation(failure)),
        None,
        None,
        Some(rdk_error::to_string(failure).into_bytes()),
        rdk_error::hints(failure).to_vec(),
        Vec::new(),
    )
}

/// Delete one attribute pattern of one owner (`flow.delete_attr`).
pub fn delete(guard: Option<Guard>, owner: &str, pattern: &str, input: &node::T) -> node::T {
    let literal = !pattern.is_empty()
        && !pattern.chars().any(|c| matches!(c, '*' | '?' | '[' | '\\' | '^' | ' ' | '\t' | '\n' | '\r' | '\u{c}'));
    let owner_text = owner.to_owned();
    let pattern_text = pattern.to_owned();
    let decide = guard.as_ref().map(|g| g.decide.clone());
    let mut inputs = vec![input.clone()];
    if let Some(g) = &guard {
        inputs.extend(g.inputs.iter().cloned());
    }
    let mut parameters = key(&[owner, pattern]);
    if let Some(g) = &guard {
        parameters.push_str(";when ");
        parameters.push_str(&g.key);
    }
    let args = build::Args {
        parameters: parameters.into_bytes(),
        cook_mode: CookMode::DuplicateInput(0),
        dependencies: guard.as_ref().map_or(dependencies::STATIC, |g| g.dependencies),
        inputs,
        ..build::args("flow.delete_attr")
    };
    let made = build::make_geometry(
        args,
        None,
        Arc::new(move |_node_id, context, inputs| {
            // a guarded deletion decides as it cooks: the guard reads the frame or cooked data
            let deletes = match &decide {
                None => true,
                Some(decide) => decide(context, inputs)?,
            };
            if !deletes {
                return Ok(GeometryCooked { geometry: inputs[0].clone(), instances: None });
            }
            let chosen = |selected: &str| Some(if owner_text == selected { pattern_text.clone() } else { String::new() });
            let params = attribute_lifecycle::DeleteParams {
                cancel: Some(context::cancel_token(context).clone()),
                reference: None,
                delete_non_selected: false,
                point_pattern: chosen("point"),
                vertex_pattern: chosen("vertex"),
                primitive_pattern: chosen("primitive"),
                detail_pattern: chosen("detail"),
            };
            match attribute_ops::delete(&params, &inputs[0]) {
                Ok(geometry) => Ok(GeometryCooked { geometry, instances: None }),
                Err(failure) => Err(error("E_ATTR_DELETE", &rdk_error::to_string(&failure))),
            }
        }),
    );
    // dropping attributes reads no payload; a guard reads its own sources as it decides
    let made = build::with_input_use(if guard.is_none() { InputUse::Forward(0) } else { InputUse::Cpu }, &made);
    build::with_facts(
        Facts {
            cook_mode: CookMode::DuplicateInput(0),
            elementwise: Elementwise::Points,
            reads: if literal { Vec::new() } else { vec!["*".to_owned()] },
            writes: if literal { vec![pattern.to_owned()] } else { vec!["*".to_owned()] },
            topology: Topology::Preserved,
            exact: true,
        },
        &made,
    )
}

fn ocaml_trim(text: &str) -> &str {
    text.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\u{c}'))
}

fn lowered(text: &str) -> String {
    ocaml_trim(text).to_ascii_lowercase()
}

pub fn attribute_owner(text: &str) -> Result<Option<Owner>, Diagnostic> {
    match lowered(text).as_str() {
        "point" | "points" => Ok(Some(Owner::Point)),
        "vertex" | "vertices" => Ok(Some(Owner::Vertex)),
        "primitive" | "primitives" => Ok(Some(Owner::Primitive)),
        "detail" => Ok(Some(Owner::Detail)),
        "any" | "*" => Ok(None),
        _ => Err(error("E_ATTR_OWNER", &format!("Unknown attribute owner {}.", node::ocaml_quote(text)))),
    }
}

pub fn group_owner(text: &str) -> Result<Option<GroupOwner>, Diagnostic> {
    match lowered(text).as_str() {
        "point" | "points" => Ok(Some(GroupOwner::GroupPoints)),
        "vertex" | "vertices" => Ok(Some(GroupOwner::GroupVertices)),
        "primitive" | "primitives" => Ok(Some(GroupOwner::GroupPrimitives)),
        "edge" | "edges" => Ok(Some(GroupOwner::GroupEdges)),
        "any" | "*" => Ok(None),
        _ => Err(error("E_GROUP_OWNER", &format!("Unknown group owner {}.", node::ocaml_quote(text)))),
    }
}

pub fn attr_conflict(text: &str) -> Result<RenameConflict, Diagnostic> {
    match lowered(text).as_str() {
        "skip" => Ok(RenameConflict::AttributeRenameSkip),
        "error" => Ok(RenameConflict::AttributeRenameError),
        "overwrite" => Ok(RenameConflict::AttributeRenameOverwrite),
        _ => Err(error(
            "E_METADATA_CONFLICT",
            &format!("Unknown attribute rename conflict {}.", node::ocaml_quote(text)),
        )),
    }
}

pub fn group_conflict(text: &str) -> Result<group_ops::RenameConflict, Diagnostic> {
    match lowered(text).as_str() {
        "skip" => Ok(group_ops::RenameConflict::RenameSkip),
        "error" => Ok(group_ops::RenameConflict::RenameError),
        "overwrite" => Ok(group_ops::RenameConflict::RenameOverwrite),
        "union" => Ok(group_ops::RenameConflict::RenameUnion),
        _ => Err(error("E_METADATA_CONFLICT", &format!("Unknown group rename conflict {}.", node::ocaml_quote(text)))),
    }
}

/// A metadata node over `inputs`: generic cook mode, reads and writes everything, keeps topology.
fn metadata_node(
    operation: &str,
    parameters: String,
    inputs: Vec<node::T>,
    cook: Arc<dyn Fn(&context::T, &[geometry::T]) -> Result<geometry::T, Diagnostic> + Send + Sync>,
) -> node::T {
    let args = build::Args {
        parameters: parameters.into_bytes(),
        cook_mode: CookMode::Generic,
        dependencies: dependencies::STATIC,
        inputs,
        ..build::args(operation)
    };
    let made = build::make_geometry(
        args,
        None,
        Arc::new(move |_node_id, context, inputs| {
            Ok(GeometryCooked { geometry: cook(context, inputs)?, instances: None })
        }),
    );
    build::with_facts(
        Facts {
            cook_mode: CookMode::Generic,
            elementwise: Elementwise::None,
            reads: vec!["*".to_owned()],
            writes: vec!["*".to_owned()],
            topology: Topology::Preserved,
            exact: true,
        },
        &made,
    )
}

fn owner_word(owner: Option<Owner>) -> &'static str {
    match owner {
        None => "any",
        Some(Owner::Point) => "point",
        Some(Owner::Vertex) => "vertex",
        Some(Owner::Primitive) => "primitive",
        Some(Owner::Detail) => "detail",
    }
}

pub fn rename(owner: &str, pattern: &str, replacement: &str, conflict: &str, input: &node::T) -> Result<node::T, Diagnostic> {
    let owner = attribute_owner(owner)?;
    let conflict = attr_conflict(conflict)?;
    let rule = RenameRule {
        rename_attribute_owner: owner,
        rename_attribute_pattern: pattern.to_owned(),
        rename_attribute_replacement: replacement.to_owned(),
        rename_attribute_conflict: conflict,
    };
    let conflict_word = match conflict {
        RenameConflict::AttributeRenameSkip => "skip",
        RenameConflict::AttributeRenameError => "error",
        RenameConflict::AttributeRenameOverwrite => "overwrite",
    };
    Ok(metadata_node(
        "flow.rename_attr",
        key(&[pattern, replacement, owner_word(owner), conflict_word]),
        vec![input.clone()],
        Arc::new(move |context, inputs| {
            attribute_ops::rename(Some(context::cancel_token(context)), std::slice::from_ref(&rule), &inputs[0])
                .map_err(|failure| structured_error(&failure))
        }),
    ))
}

pub fn swap(owner: &str, source: &str, destination: &str, method: &str, input: &node::T) -> Result<node::T, Diagnostic> {
    let owner = match attribute_owner(owner)? {
        Some(owner) => owner,
        None => return Err(error("E_ATTR_OWNER", "Attribute swap requires one owner.")),
    };
    let method = match lowered(method).as_str() {
        "swap" => SwapMethod::AttributeSwap,
        "move" => SwapMethod::AttributeMove,
        "copy" => SwapMethod::AttributeCopy,
        _ => return Err(error("E_METADATA_METHOD", "Attribute method is swap, move or copy.")),
    };
    let rule = SwapRule {
        swap_attribute_owner: owner,
        swap_attribute_source: source.to_owned(),
        swap_attribute_destination: destination.to_owned(),
        swap_attribute_method: method,
    };
    let method_word = match method {
        SwapMethod::AttributeSwap => "swap",
        SwapMethod::AttributeMove => "move",
        SwapMethod::AttributeCopy => "copy",
    };
    Ok(metadata_node(
        "flow.swap_attr",
        key(&[source, destination, owner_word(Some(owner)), method_word]),
        vec![input.clone()],
        Arc::new(move |context, inputs| {
            attribute_ops::swap(Some(context::cancel_token(context)), std::slice::from_ref(&rule), &inputs[0])
                .map_err(|failure| structured_error(&failure))
        }),
    ))
}

pub fn delete_attributes(
    reference: Option<&node::T>,
    inverse: bool,
    point: &str,
    vertex: &str,
    primitive: &str,
    detail: &str,
    input: &node::T,
) -> node::T {
    let mut inputs = vec![input.clone()];
    inputs.extend(reference.cloned());
    let (point_text, vertex_text, primitive_text, detail_text) =
        (point.to_owned(), vertex.to_owned(), primitive.to_owned(), detail.to_owned());
    metadata_node(
        "flow.delete_attrs",
        key(&[&inverse.to_string(), point, vertex, primitive, detail]),
        inputs,
        Arc::new(move |context, inputs| {
            let params = attribute_lifecycle::DeleteParams {
                cancel: Some(context::cancel_token(context).clone()),
                reference: if inputs.len() == 2 { Some(inputs[1].clone()) } else { None },
                delete_non_selected: inverse,
                point_pattern: Some(point_text.clone()),
                vertex_pattern: Some(vertex_text.clone()),
                primitive_pattern: Some(primitive_text.clone()),
                detail_pattern: Some(detail_text.clone()),
            };
            attribute_ops::delete(&params, &inputs[0]).map_err(|failure| structured_error(&failure))
        }),
    )
}

pub fn rename_group(owner: &str, pattern: &str, replacement: &str, conflict: &str, input: &node::T) -> Result<node::T, Diagnostic> {
    let selected = group_owner(owner)?;
    let policy = group_conflict(conflict)?;
    let rule = group_ops::RenameRule {
        rename_owner: selected,
        rename_pattern: pattern.to_owned(),
        rename_replacement: replacement.to_owned(),
        rename_conflict: policy,
    };
    Ok(metadata_node(
        "flow.rename_group",
        key(&[owner, pattern, replacement, conflict]),
        vec![input.clone()],
        Arc::new(move |_, inputs| {
            group_ops::rename(std::slice::from_ref(&rule), &inputs[0]).map_err(|failure| structured_error(&failure))
        }),
    ))
}

pub fn delete_group(owner: &str, pattern: &str, unused: bool, input: &node::T) -> Result<node::T, Diagnostic> {
    let selected = group_owner(owner)?;
    let rule = DeleteRule { delete_owner: selected, delete_pattern: pattern.to_owned() };
    Ok(metadata_node(
        "flow.delete_group",
        key(&[owner, pattern, &unused.to_string()]),
        vec![input.clone()],
        Arc::new(move |_, inputs| {
            let params = DeleteParams { rules: vec![rule.clone()], delete_unused: unused };
            group_ops::delete(&params, &inputs[0]).map_err(|failure| structured_error(&failure))
        }),
    ))
}

pub fn rename_edge(from: &str, into: &str, input: &node::T) -> node::T {
    let (from_text, into_text) = (from.to_owned(), into.to_owned());
    metadata_node(
        "flow.rename_edge",
        key(&[from, into]),
        vec![input.clone()],
        Arc::new(move |_, inputs| {
            geometry::rename_edge_group(&from_text, &into_text, &inputs[0]).map_err(|failure| {
                rays_diagnostic::error(
                    "rename_edge_group_failed".into(),
                    "rename_edge_group could not produce valid geometry".into(),
                    None,
                    None,
                    Some(rdk_error::message(&failure).as_bytes().to_vec()),
                    Vec::new(),
                    Vec::new(),
                )
            })
        }),
    )
}

pub fn delete_edge(name: &str, input: &node::T) -> node::T {
    let name_text = name.to_owned();
    metadata_node(
        "flow.delete_edge",
        key(&[name]),
        vec![input.clone()],
        Arc::new(move |_, inputs| Ok(geometry::without_edge_group(&name_text, &inputs[0]))),
    )
}

fn storage_kind(storage: &attribute::Storage) -> &'static str {
    match storage {
        attribute::Storage::Float(_) => "float",
        attribute::Storage::Int(_) => "int",
        attribute::Storage::Float2(_) => "vec2",
        attribute::Storage::Float3(_) => "vec3",
        attribute::Storage::Float4(_) => "vec4",
        attribute::Storage::Text(_) => "text",
        attribute::Storage::FloatArray(_) => "float_array",
        attribute::Storage::IntArray(_) => "int_array",
    }
}

/// An attribute's type name (float, int, vec2-4, text, float_array, int_array), without reading
/// its storage (a device-resident one stays there).
pub fn kind(owner: Owner, name: &str, value: &geometry::T) -> Option<String> {
    if owner == Owner::Point && name == "P" {
        return Some("vec3".to_owned());
    }
    geometry::find_attribute(owner, name, value).map(|found| {
        match attribute::kind_name(&found) {
            "float2" => "vec2",
            "float3" => "vec3",
            "float4" => "vec4",
            other => other,
        }
        .to_owned()
    })
}

fn type_of(owner: Owner, name: &str, value: &geometry::T) -> Result<Value, Diagnostic> {
    match kind(owner, name, value) {
        Some(kind) => Ok(Value::Text(kind.into_bytes())),
        None => Err(error("E_ATTR_TYPE", &format!("Attribute {} is missing.", node::ocaml_quote(name)))),
    }
}

fn names(owner: Owner, pattern: &str, value: &geometry::T) -> Result<Value, Diagnostic> {
    let compiled = attribute_pattern::compile(pattern)
        .map_err(|failure| error("E_ATTR_PATTERN", rdk_error::message(&failure)))?;
    let found: Vec<Vec<u8>> = geometry::View::attributes(value)
        .iter()
        .filter(|a| attribute::owner(a) == owner && attribute_pattern::matches(&compiled, attribute::name(a)))
        .map(|a| attribute::name(a).as_bytes().to_vec())
        .collect();
    Ok(Value::Text_array(Arc::new(found)))
}

/// Parallel name/type/owner columns of the stored attributes, in native attribute order.
pub fn entries(owner: Option<Owner>, component: &str, value: &geometry::T) -> Result<Value, Diagnostic> {
    let column: fn(&attribute::T) -> Result<String, Diagnostic> = match component {
        "name" => |a| Ok(attribute::name(a).to_owned()),
        "type" => |a| match attribute::View::storage(a) {
            Ok(storage) => Ok(storage_kind(storage).to_owned()),
            Err(failure) => Err(error("E_ATTR_ENTRIES", &rdk_error::to_string(&failure))),
        },
        "owner" => |a| Ok(owner_word(Some(attribute::owner(a))).to_owned()),
        _ => return Err(error("E_ATTR_ENTRIES", "Attribute entry component is name, type or owner.")),
    };
    let mut out: Vec<Vec<u8>> = Vec::new();
    for attribute in geometry::View::attributes(value) {
        if owner.is_none() || owner == Some(attribute::owner(attribute)) {
            out.push(column(attribute)?.into_bytes());
        }
    }
    Ok(Value::Text_array(Arc::new(out)))
}

// a method, not a free fn: its output borrows from `self` without a lifetime parameter
trait Named {
    fn named(&self, name: &str) -> Option<&Value>;
}

impl Named for [(String, Value)] {
    fn named(&self, name: &str) -> Option<&Value> {
        self.iter().find(|(n, _)| n == name).map(|(_, v)| v)
    }
}

fn owner_argument(args: &[(String, Value)]) -> Result<Option<Owner>, Diagnostic> {
    match args.named("owner") {
        None => Ok(Some(Owner::Point)),
        Some(Value::Text(owner)) => attribute_owner(std::str::from_utf8(owner).map_err(|_| error("E_ATTR_OWNER", "Attribute owner must be UTF-8 text."))?),
        _ => Err(error("E_ATTR_OWNER", "Attribute owner must be text.")),
    }
}

/// Evaluate a metadata source (`sop/attr_entries`, `sop/attr_type`, `sop/attr_names`) over a
/// cooked geometry; `geometry` finds a cooked geometry by deferred id.
pub fn resolve(geometry: &dyn Fn(i64) -> Option<geometry::T>, value: &Value) -> Result<Value, Diagnostic> {
    let Value::Source(operation, _, args) = value else {
        return Err(error("E_DATA_SOURCE", "Unknown metadata source."));
    };
    let operation = operation.as_str();
    if !matches!(operation, "sop/attr_entries" | "sop/attr_type" | "sop/attr_names") {
        return Err(error("E_DATA_SOURCE", "Unknown metadata source."));
    }
    let id = match args.named("geometry") {
        Some(Value::Deferred(t, id)) if ty::is_geometry(t) => *id,
        _ => return Err(error("E_DATA_SOURCE", "Metadata source requires cooked geometry.")),
    };
    let Some(found) = geometry(id) else {
        return Err(error("E_DATA_SOURCE", "Metadata source has not been cooked."));
    };
    let owner = owner_argument(args)?;
    if operation == "sop/attr_entries" {
        return match args.named("component") {
            Some(Value::Text(component)) => entries(owner, std::str::from_utf8(component).map_err(|_| error("E_ATTR_ENTRIES", "Attribute entry component must be UTF-8 text."))?, &found),
            _ => Err(error("E_ATTR_ENTRIES", "Attribute entry component is name, type or owner.")),
        };
    }
    let Some(owner) = owner else {
        return Err(error("E_ATTR_OWNER", "Metadata queries require one owner."));
    };
    let argument = args.named(if operation == "sop/attr_type" { "attribute" } else { "pattern" });
    match (operation, argument) {
        ("sop/attr_type", Some(Value::Text(name))) => type_of(owner, std::str::from_utf8(name).map_err(|_| error("E_ATTR_NAME", "Attribute name must be UTF-8 text."))?, &found),
        ("sop/attr_names", Some(Value::Text(pattern))) => names(owner, std::str::from_utf8(pattern).map_err(|_| error("E_ATTR_NAME", "Attribute pattern must be UTF-8 text."))?, &found),
        _ => Err(error("E_ATTR_NAME", "Metadata names and patterns must be text.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use flow_syntax::ty::Ty;
    use rdk::line_geometry;

    fn snapshot(g: &geometry::T) -> node::T {
        let held = g.clone();
        build::make_geometry(
            build::Args {
                parameters: format!("data_id={}", geometry::data_id(g)).into_bytes(),
                cook_mode: CookMode::Generator,
                ..build::args("snapshot")
            },
            None,
            Arc::new(move |_, _, _| Ok(GeometryCooked { geometry: held.clone(), instances: None })),
        )
    }

    fn cooked(n: &node::T) -> geometry::T {
        let mut session = sop::session::create(8, 1_000_000).expect("session");
        let context = context::create(context::Options { domains: Some(1), ..Default::default() }).expect("context");
        let output = sop::session::cook(&mut session, None, &context, n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
        sop::session::close(&mut session);
        sop::payload::geometry(&output.payload).expect("geometry")
    }

    fn fixture() -> geometry::T {
        let points = line_geometry::points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0)]).expect("points");
        let colour = attribute::create_owned("Cd", Owner::Point, attribute::Storage::Float(vec![0.25; 3])).expect("Cd");
        let tag = attribute::create_owned("tag", Owner::Detail, attribute::Storage::Text(vec![b"a".to_vec()])).expect("tag");
        let geometry = geometry::with_attribute(colour, &points).expect("with Cd");
        geometry::with_attribute(tag, &geometry).expect("with tag")
    }

    #[test]
    fn globs() {
        assert_eq!(glob_literal("a b*").expect("literal"), r"a\ b\*");
        assert_eq!(glob_literal("").unwrap_err().code, "E_METADATA_PATTERN");
        assert_eq!(glob_rewrite("foo_*", "bar_*", "foo_x").expect("rewrite"), "bar_x");
        assert_eq!(glob_rewrite("foo_*", "bar_*", "zzz").expect("no match keeps"), "zzz");
        assert!(glob_rewrite("foo_*", "bar", "foo_x").is_err());
        assert!(glob_rewrite_set_matches("foo_*", "bar_*", "foo_x").expect("set matches"));
        assert!(!glob_rewrite_set_matches("foo_*", "bar_*", "zzz").expect("set misses"));
        assert_eq!(glob_rewrite_set("foo_*", "bar_*", "foo_x").expect("set"), "bar_x");
    }

    #[test]
    fn owners_and_conflicts() {
        assert_eq!(attribute_owner(" Points ").expect("owner"), Some(Owner::Point));
        assert_eq!(attribute_owner("*").expect("any"), None);
        let bad = attribute_owner("edge").unwrap_err();
        assert_eq!((bad.code.as_str(), bad.message.as_str()), ("E_ATTR_OWNER", "Unknown attribute owner \"edge\"."));
        assert_eq!(group_owner("edges").expect("group"), Some(GroupOwner::GroupEdges));
        assert_eq!(group_owner("detail").unwrap_err().code, "E_GROUP_OWNER");
        assert_eq!(attr_conflict("Overwrite").expect("conflict"), RenameConflict::AttributeRenameOverwrite);
        assert_eq!(group_conflict("union").expect("group conflict"), group_ops::RenameConflict::RenameUnion);
        assert_eq!(group_conflict("merge").unwrap_err().code, "E_METADATA_CONFLICT");
    }

    #[test]
    fn kind_entries_and_sources() {
        let geometry = fixture();
        assert_eq!(kind(Owner::Point, "P", &geometry).as_deref(), Some("vec3"));
        assert_eq!(kind(Owner::Point, "Cd", &geometry).as_deref(), Some("float"));
        assert_eq!(kind(Owner::Detail, "tag", &geometry).as_deref(), Some("text"));
        assert_eq!(kind(Owner::Point, "nope", &geometry), None);
        let names = entries(Some(Owner::Point), "name", &geometry).expect("names");
        assert_eq!(names, Value::Text_array(Arc::new(vec![b"Cd".to_vec()])));
        let types = entries(None, "type", &geometry).expect("types");
        assert_eq!(types, Value::Text_array(Arc::new(vec![b"float".to_vec(), b"text".to_vec()])));
        assert_eq!(entries(None, "bogus", &geometry).unwrap_err().code, "E_ATTR_ENTRIES");
        let found = geometry.clone();
        let lookup = move |id: i64| if id == 7 { Some(found.clone()) } else { None };
        let source = |operation: &str, extra: Vec<(&str, Value)>| {
            let mut args = vec![("geometry".to_owned(), Value::Deferred(flow_syntax::ty::geometry(), 7))];
            args.extend(extra.into_iter().map(|(n, v)| (n.to_owned(), v)));
            Value::Source(operation.to_owned(), Ty::Any, args)
        };
        let text = |s: &str| Value::Text(s.as_bytes().to_vec());
        let attr_type = source("sop/attr_type", vec![("attribute", text("Cd"))]);
        assert_eq!(resolve(&lookup, &attr_type).expect("type"), text("float"));
        let missing = source("sop/attr_type", vec![("attribute", text("zz"))]);
        assert_eq!(resolve(&lookup, &missing).unwrap_err().code, "E_ATTR_TYPE");
        let listing = source("sop/attr_names", vec![("pattern", text("*"))]);
        assert_eq!(resolve(&lookup, &listing).expect("names"), Value::Text_array(Arc::new(vec![b"Cd".to_vec()])));
        let table = source("sop/attr_entries", vec![("component", text("owner")), ("owner", text("any"))]);
        assert_eq!(resolve(&lookup, &table).expect("owners"), Value::Text_array(Arc::new(vec![b"point".to_vec(), b"detail".to_vec()])));
        let uncooked = |_: i64| None;
        assert_eq!(resolve(&uncooked, &attr_type).unwrap_err().code, "E_DATA_SOURCE");
        assert_eq!(resolve(&lookup, &Value::Int(1)).unwrap_err().code, "E_DATA_SOURCE");
    }

    #[test]
    fn metadata_nodes_cook_and_declare_their_facts() {
        let source = snapshot(&fixture());
        let renamed = rename("point", "Cd", "Color", "error", &source).expect("rename node");
        let out = cooked(&renamed);
        assert!(geometry::find_attribute(Owner::Point, "Color", &out).is_some());
        assert!(geometry::find_attribute(Owner::Point, "Cd", &out).is_none());
        assert!(rename("edge", "Cd", "Color", "error", &source).is_err());
        let swapped = swap("point", "Cd", "Cd2", "copy", &source).expect("swap node");
        let out = cooked(&swapped);
        assert!(geometry::find_attribute(Owner::Point, "Cd", &out).is_some() && geometry::find_attribute(Owner::Point, "Cd2", &out).is_some());
        assert!(swap("*", "Cd", "Cd2", "copy", &source).is_err());
        let deleted = delete(None, "point", "Cd", &source);
        let facts = node::facts(&deleted);
        assert_eq!((facts.reads.clone(), facts.writes.clone()), (Vec::<String>::new(), vec!["Cd".to_owned()]));
        assert!(geometry::find_attribute(Owner::Point, "Cd", &cooked(&deleted)).is_none());
        let glob = delete(None, "point", "C*", &source);
        assert_eq!(node::facts(&glob).writes, vec!["*".to_owned()]);
        let guard = Guard {
            key: "never".to_owned(),
            dependencies: dependencies::STATIC,
            inputs: Vec::new(),
            decide: Arc::new(|_, _| Ok(false)),
        };
        let kept = delete(Some(guard), "point", "Cd", &source);
        assert!(geometry::find_attribute(Owner::Point, "Cd", &cooked(&kept)).is_some());
        let dropped = delete_attributes(None, false, "Cd", "", "", "tag", &source);
        let out = cooked(&dropped);
        assert!(geometry::find_attribute(Owner::Point, "Cd", &out).is_none());
        assert_eq!(node::parameter_key(&source), node::parameter_key(&source));
    }
}
