//! Port of lib/sop/groups.ml: group nodes, declared once (metadata, fields, parameters, operator).
use std::sync::Arc;

use param::Value;
use rdk::group_ops::{self, CopyConflict, Owner, TransferRule};

use crate::context::{self, dependencies};
use crate::decl::{self, Decl, Meta, Spec};
use crate::node::{self, build, CookMode};
use crate::support;

/// The declared default of the transfer node's rules.
pub fn default_transfer_rules() -> Vec<TransferRule> {
    [Owner::GroupPrimitives, Owner::GroupPoints, Owner::GroupEdges]
        .into_iter()
        .map(|transfer_owner| TransferRule {
            transfer_owner,
            transfer_pattern: "*".to_owned(),
            transfer_prefix: String::new(),
        })
        .collect()
}

pub mod group_transfer {
    use super::*;

    pub fn encode_rule(rule: &TransferRule) -> Vec<Vec<u8>> {
        vec![
            support::group_owner_token(rule.transfer_owner).as_bytes().to_vec(),
            rule.transfer_pattern.as_bytes().to_vec(),
            rule.transfer_prefix.as_bytes().to_vec(),
        ]
    }

    pub fn decode_rule(row: &[String]) -> Result<TransferRule, String> {
        match row {
            [owner, pattern, prefix] => Ok(TransferRule {
                transfer_owner: support::group_owner_of_token(&owner.trim().to_ascii_lowercase())?,
                transfer_pattern: pattern.clone(),
                transfer_prefix: prefix.clone(),
            }),
            row => Err(format!(
                "Group Transfer rule needs owner, pattern, and prefix; got {} columns",
                row.len()
            )),
        }
    }

    pub fn encode(rules: &[TransferRule]) -> Vec<u8> {
        support::encode_table(&rules.iter().map(encode_rule).collect::<Vec<_>>())
    }

    pub fn decode(text: &[u8]) -> Result<Vec<TransferRule>, String> {
        support::decode_table(text)?.iter().map(|row| decode_rule(row)).collect()
    }

    /// `rules_parameter`: an encoded table whose canonical form is decode then encode.
    pub fn rules_kind() -> param::Kind {
        param::encoded_with(
            Box::new(|a, b| matches!((decode(a), decode(b)), (Ok(a), Ok(b)) if encode(&a) == encode(&b))),
            Box::new(|text| decode(text).map(|rules| encode(&rules))),
        )
    }

    /// The `parameters` record. Field order is the schema's field order.
    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub use_rules: bool,
        pub rules: Vec<TransferRule>,
        pub conflict: CopyConflict,
        pub create_empty: bool,
        pub distance: f64,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        let (Value::Bool(use_rules), Value::Text(rules), Value::Bool(create_empty), Value::Float(distance)) =
            (&values[0], &values[1], &values[3], &values[4])
        else {
            unreachable!("group_transfer: parameter record has the wrong shape")
        };
        Parameters {
            use_rules: *use_rules,
            rules: decode(rules).expect("rules were normalized by the schema"),
            conflict: support::GROUP_COPY_CONFLICT.of(&values[2]),
            create_empty: *create_empty,
            distance: *distance,
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            Value::Bool(p.use_rules),
            Value::Text(encode(&p.rules)),
            support::GROUP_COPY_CONFLICT.value(&p.conflict),
            Value::Bool(p.create_empty),
            Value::Float(p.distance),
        ]
    }

    pub const META: Meta = Meta {
        key: "group_transfer",
        label: "Group Transfer",
        category: "Group/Transfer",
        inputs: 2,
        slots: Some("source, target"),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec { label: "Use rules", ..decl::spec("use_rules", decl::toggle_kind(), Value::Bool(false)) },
            Spec {
                label: "Rules (owner, pattern, prefix)",
                ..decl::spec("rules", rules_kind(), Value::Text(encode(&[])))
            },
            Spec {
                label: "Conflict",
                ..decl::spec(
                    "conflict",
                    support::GROUP_COPY_CONFLICT.kind(),
                    support::GROUP_COPY_CONFLICT.value(&CopyConflict::CopyOverwrite),
                )
            },
            Spec { label: "Create empty groups", ..decl::spec("create_empty", decl::toggle_kind(), Value::Bool(false)) },
            Spec {
                label: "Maximum distance",
                ..decl::spec("distance", decl::float_kind(0.0, 10.0, Some(0.0), None), Value::Float(0.001))
            },
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let rules = if p.use_rules { p.rules } else { default_transfer_rules() };
        let (conflict, create_empty, distance) = (p.conflict, p.create_empty, p.distance);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Generic,
                dependencies: dependencies::STATIC,
                inputs: vec![source, target],
                ..build::args("group_transfer")
            },
            None,
            Arc::new(move |_, context, inputs| {
                match group_ops::transfer(&group_ops::TransferParams {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    rules: rules.clone(),
                    conflict,
                    create_empty,
                    distance,
                    source: inputs[0].clone(),
                    target: inputs[1].clone(),
                }) {
                    Ok(geometry) => support::cooked(geometry),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> crate::edit_graph::Factory {
        decl::factory(&decl())
    }
}
