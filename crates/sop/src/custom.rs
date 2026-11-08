//! Port of lib/sop/custom.ml: the public extension point for custom procedural SOPs.
use std::sync::Arc;

use param::{Schema, Value};
use rays_diagnostic::Diagnostic;

use crate::context::{self, dependencies};
use crate::node::{self, build, CookMode};
use crate::support;

fn resolved_label(operation: &str, label: Option<&str>) -> String {
    match label {
        Some(label) if !support::ocaml_trim(label).is_empty() => label.to_owned(),
        _ => operation.to_owned(),
    }
}

/// `cook ~context geometries`; a failure message becomes an `<operation>_failed` diagnostic.
pub type PlainCook = Arc<dyn Fn(&context::T, &[rdk_core::geometry::T]) -> Result<rdk_core::geometry::T, String> + Send + Sync>;

pub struct PlainArgs {
    pub label: Option<String>,
    pub version: i64,
    pub parameters: Vec<u8>,
    pub cook_mode: CookMode,
    pub dependencies: dependencies::T,
    pub operation: String,
}

pub fn plain_args(operation: &str) -> PlainArgs {
    PlainArgs {
        label: None,
        version: 1,
        parameters: Vec::new(),
        cook_mode: CookMode::Generic,
        dependencies: dependencies::STATIC,
        operation: operation.to_owned(),
    }
}

pub fn plain(args: PlainArgs, inputs: Vec<node::T>, cook: PlainCook) -> node::T {
    let operation = args.operation.clone();
    build::make_geometry(
        build::Args {
            label: args.label,
            version: args.version,
            parameters: args.parameters,
            cook_mode: args.cook_mode,
            dependencies: args.dependencies,
            inputs,
            ..build::args(&args.operation)
        },
        None,
        Arc::new(move |_, context, geometries| {
            if context::cancelled(context) {
                return Err(support::diagnostic("cancelled", "custom procedural node was cancelled before cooking"));
            }
            match cook(context, geometries) {
                Ok(geometry) => support::cooked(geometry),
                Err(message) => Err::<_, Diagnostic>(support::rdk_error(Vec::new(), &operation, &message)),
            }
        }),
    )
}

/// `(label, current inputs, parameter values) -> unparameterized operator`.
pub type Operator = Arc<dyn Fn(&str, &[node::T], &[Value]) -> Result<node::T, String> + Send + Sync>;

fn build_node(operation: &str, schema: &Arc<Schema>, operator: &Operator, label: &str, inputs: &[node::T], values: &[Value]) -> Result<node::T, String> {
    let made = operator(label, inputs, values)?;
    let (operation, schema, operator) = (operation.to_owned(), Arc::clone(schema), Arc::clone(operator));
    let rebuild_schema = Arc::clone(&schema);
    node::parameterize(
        schema,
        values.to_vec(),
        Arc::new(move |label: &str, inputs: &[node::T], values: &[Value]| {
            build_node(&operation, &rebuild_schema, &operator, label, inputs, values)
        }),
        &made,
    )
}

/// A parameterized custom node built by `operator`; edits rebuild it through the same operator.
pub fn node(label: Option<&str>, operation: &str, schema: Arc<Schema>, values: Vec<Value>, inputs: Vec<node::T>, operator: Operator) -> Result<node::T, String> {
    assert!(!support::ocaml_trim(operation).is_empty(), "Custom.node: operation must not be blank");
    build_node(operation, &schema, &operator, &resolved_label(operation, label), &inputs, &values)
}

/// `cook ~parameters ~context geometries`.
pub type Cook = Arc<dyn Fn(&[Value], &context::T, &[rdk_core::geometry::T]) -> Result<rdk_core::geometry::T, String> + Send + Sync>;

pub struct CreateArgs {
    pub label: Option<String>,
    pub version: i64,
    pub cook_mode: CookMode,
    pub dependencies: dependencies::T,
    pub operation: String,
}

pub fn create_args(operation: &str) -> CreateArgs {
    CreateArgs {
        label: None,
        version: 1,
        cook_mode: CookMode::Generic,
        dependencies: dependencies::STATIC,
        operation: operation.to_owned(),
    }
}

pub fn create(args: CreateArgs, schema: Arc<Schema>, values: Vec<Value>, inputs: Vec<node::T>, cook: Cook) -> Result<node::T, String> {
    let key_schema = Arc::clone(&schema);
    let operation = args.operation.clone();
    let (version, cook_mode, dependencies) = (args.version, args.cook_mode, args.dependencies);
    let operator: Operator = Arc::new(move |label, inputs, parameters| {
        let cook = Arc::clone(&cook);
        let bound: Vec<Value> = parameters.to_vec();
        Ok(plain(
            PlainArgs {
                label: Some(label.to_owned()),
                version,
                parameters: key_schema.key(parameters)?,
                cook_mode,
                dependencies,
                operation: operation.clone(),
            },
            inputs.to_vec(),
            Arc::new(move |context, geometries| cook(&bound, context, geometries)),
        ))
    });
    node(args.label.as_deref(), &args.operation, schema, values, inputs, operator)
}
