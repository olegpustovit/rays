//! Port of lib/sop/shapes.ml: shape nodes, declared once. This wave ports `null` and `switch`, the
//! worked examples of PORTING.md; the rest of shapes.ml is filled by a later wave in the same
//! pattern (one `pub mod <name>` each, listed in `nodes::factories`).
use std::sync::Arc;

use param::Value;

use crate::context::dependencies;
use crate::decl::{self, Decl, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, CookMode, InputPolicy};
use crate::support;

pub mod null {
    use super::*;

    // `type parameters = unit`: no fields, so no Parameters struct and no conversion.
    pub const META: Meta = Meta {
        key: "null",
        label: "Null",
        category: "Utility",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        Vec::new()
    }

    pub fn operator(label: &str, _values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Passthrough(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("null")
            },
            None,
            Arc::new(|_, _, inputs| support::cooked(inputs[0].clone())),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

pub mod switch {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub input: i64,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        match &values[0] {
            Value::Int(input) => Parameters { input: *input },
            _ => panic!("switch: parameter record has the wrong shape"),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![Value::Int(p.input)]
    }

    pub const META: Meta = Meta {
        key: "switch",
        label: "Switch",
        category: "Utility",
        inputs: 3,
        slots: Some("a, b, inputs"),
        optional: &[2],
        rest: Some(2),
        ..decl::META
    };

    /// `choices`: the connected branches by index and label, once the node exists (the dynamic
    /// schema); `None` for the static schema.
    pub fn specs(choices: Option<Vec<String>>) -> Vec<Spec> {
        let kind = match choices {
            Some(labels) => param::index_choice(labels).expect("switch choices"),
            None => decl::int_kind(0, 9999, Some(0), None),
        };
        vec![Spec {
            label: "Source",
            description: Some("Input branch displayed and cooked by this Switch SOP"),
            ..decl::spec("input", kind, Value::Int(0))
        }]
    }

    /// The choice lists the connected branches by index and label.
    pub fn schema(_values: &[Value], made: &node::T) -> Arc<param::Schema> {
        let labels = node::inputs(made)
            .iter()
            .enumerate()
            .map(|(index, input)| format!("{index} \u{b7} {}", node::label(input)))
            .collect();
        decl::schema(&META, specs(Some(labels)))
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let nodes = decl::connected(inputs);
        let index = from_values(values).input;
        if index < 0 || index as usize >= nodes.len() {
            return Err("sop/switch: input is outside the connected branches".to_owned());
        }
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Passthrough(index),
                dependencies: dependencies::STATIC,
                input_policy: Some(InputPolicy::Only(index)),
                inputs: nodes,
                ..build::args("switch")
            },
            None,
            Arc::new(|_, _, selected| support::cooked(selected[0].clone())),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs(None)), operator, dynamic_schema: Some(schema) }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}
