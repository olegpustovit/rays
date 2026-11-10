//! cooked immutable group boundary; native RDK owns
//! membership and order.
use flow_syntax::symbol::Symbol;
use crate::attribute_metadata;
use flow_syntax::ty;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use rays_math::text_array::TextArray;
use rdk_core::geometry::{self, T as Geometry};
use rdk_ops::group_ops;
use rdk_core::{attribute_pattern, edge_group, element_selection, group, topology_index};
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    Ordinary(group::Owner),
    Edge,
}

fn diag(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}
fn fail<R>(code: &str, message: &str) -> Result<R, Diagnostic> {
    Err(diag(code, message))
}
fn arg<'a>(args: &'a flow_syntax::value::Record, name: &str) -> Option<&'a Value> {
    args.get_str(name)
}
fn text_of(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}
fn text_value(text: String) -> Value {
    Value::Text(text.into_bytes().into())
}

pub fn owner(name: &str) -> Result<Owner, Diagnostic> {
    match name {
        "point" => Ok(Owner::Ordinary(group::Owner::Point)),
        "vertex" => Ok(Owner::Ordinary(group::Owner::Vertex)),
        "primitive" => Ok(Owner::Ordinary(group::Owner::Primitive)),
        "edge" => Ok(Owner::Edge),
        _ => fail("E_GROUP_OWNER", "Group owner is point, vertex, primitive or edge."),
    }
}

fn count(geometry: &Geometry, owner: Owner) -> Result<i64, Diagnostic> {
    Ok(match owner {
        Owner::Ordinary(group::Owner::Point) => geometry::point_count(geometry),
        Owner::Ordinary(group::Owner::Vertex) => geometry::vertex_count(geometry),
        Owner::Ordinary(group::Owner::Primitive) => geometry::primitive_count(geometry),
        Owner::Edge => {
            let index = topology_index::create(None, geometry::topology(geometry)).map_err(|e| diag("E_TOPOLOGY", rdk_core::error::message(&e)))?;
            topology_index::edge_count(&index)
        }
    })
}

fn name_check(name: &str) -> Result<(), Diagnostic> {
    if name.trim().is_empty() {
        fail("E_GROUP_NAME", "Group name must not be blank.")
    } else {
        Ok(())
    }
}

fn missing<R>(name: &str) -> Result<R, Diagnostic> {
    fail("E_GROUP_MISSING", &format!("Missing group {name}."))
}

fn mask(group: &group::T) -> Value {
    let length = group::length(group) as usize;
    let bits = group::view::bits_view(group);
    let mut values = vec![0i64; length];
    for (byte, value) in bits.iter().enumerate() {
        let first = byte << 3;
        let count = 8.min(length - first);
        if *value == 255 {
            for slot in &mut values[first..first + count] {
                *slot = 1;
            }
        } else if *value != 0 {
            for bit in 0..count {
                values[first + bit] = ((*value >> bit) & 1) as i64;
            }
        }
    }
    Value::Int_array(Arc::new(values))
}

pub fn read(owner: Owner, name: &str, geometry: &Geometry) -> Result<Value, Diagnostic> {
    match owner {
        Owner::Ordinary(owner) => match geometry::find_group(owner, name, geometry) {
            None => missing(name),
            Some(found) => Ok(mask(&found)),
        },
        Owner::Edge => match geometry::find_edge_group(name, geometry) {
            None => missing(name),
            Some(found) => Ok(Value::Int_array(Arc::new(
                (0..edge_group::length(&found)).map(|index| i64::from(edge_group::mem(index, &found))).collect(),
            ))),
        },
    }
}

fn read_points(owner: Owner, name: &str, geometry: &Geometry) -> Result<Value, Diagnostic> {
    let selection = match owner {
        Owner::Ordinary(owner) => match geometry::find_group(owner, name, geometry) {
            None => return missing(name),
            Some(found) => match owner {
                group::Owner::Point => element_selection::T::SelectedPoints(found),
                group::Owner::Vertex => element_selection::T::SelectedVertices(found),
                group::Owner::Primitive => element_selection::T::SelectedPrimitives(found),
            },
        },
        Owner::Edge => match geometry::find_edge_group(name, geometry) {
            None => return missing(name),
            Some(found) => element_selection::T::SelectedEdges(found),
        },
    };
    element_selection::promote(None, 4096, None, group::Owner::Point, &selection, geometry::topology(geometry))
        .map(|promoted| mask(&promoted))
        .map_err(|e| diag("E_GROUP_SOURCE", rdk_core::error::message(&e)))
}

fn order(owner: Owner, name: &str, geometry: &Geometry) -> Result<Value, Diagnostic> {
    match owner {
        Owner::Edge => match geometry::find_edge_group(name, geometry) {
            None => missing(name),
            Some(_) => Ok(Value::Record(vec![
                ("ordered".into(), Value::Bool(false)),
                ("indices".into(), Value::Int_array(Arc::new(Vec::new()))),
            ].into())),
        },
        Owner::Ordinary(owner) => match geometry::find_group(owner, name, geometry) {
            None => missing(name),
            Some(found) => Ok(Value::Record(vec![
                ("ordered".into(), Value::Bool(group::is_ordered(&found))),
                ("indices".into(), Value::Int_array(Arc::new(group::ordered_elements(&found).unwrap_or_default()))),
            ].into())),
        },
    }
}

fn names(owner: Owner, pattern: &str, geometry: &Geometry) -> Result<Value, Diagnostic> {
    let pattern = attribute_pattern::compile(pattern).map_err(|e| diag("E_GROUP_PATTERN", rdk_core::error::message(&e)))?;
    let found: Vec<String> = match owner {
        Owner::Ordinary(owner) => geometry::groups(geometry).iter().filter(|g| group::owner(g) == owner).map(|g| group::name(g).to_owned()).collect(),
        Owner::Edge => geometry::edge_groups(geometry).iter().map(|g| edge_group::name(g).to_owned()).collect(),
    };
    Ok(Value::Text_array(Arc::new(
        found.into_iter().filter(|name| attribute_pattern::matches(&pattern, name)).map(String::into_bytes).collect(),
    )))
}

fn entries(selected_owner: &str, pattern: &str, component: &str, geometry: &Geometry) -> Result<Value, Diagnostic> {
    let selected = attribute_metadata::group_owner(selected_owner)?;
    let pattern = attribute_pattern::compile(pattern).map_err(|e| diag("E_GROUP_PATTERN", rdk_core::error::message(&e)))?;
    let mut all: Vec<(group_ops::Owner, String)> = Vec::new();
    for item in geometry::groups(geometry) {
        let owner = match group::owner(&item) {
            group::Owner::Point => group_ops::Owner::GroupPoints,
            group::Owner::Vertex => group_ops::Owner::GroupVertices,
            group::Owner::Primitive => group_ops::Owner::GroupPrimitives,
        };
        all.push((owner, group::name(&item).to_owned()));
    }
    for item in geometry::edge_groups(geometry) {
        all.push((group_ops::Owner::GroupEdges, edge_group::name(&item).to_owned()));
    }
    let values: TextArray = all
        .into_iter()
        .filter(|(owner, name)| (selected.is_none() || selected == Some(*owner)) && attribute_pattern::matches(&pattern, name))
        .map(|(owner, name)| {
            if component == "names" {
                name.into_bytes()
            } else {
                match owner {
                    group_ops::Owner::GroupPoints => b"point".to_vec(),
                    group_ops::Owner::GroupVertices => b"vertex".to_vec(),
                    group_ops::Owner::GroupPrimitives => b"primitive".to_vec(),
                    group_ops::Owner::GroupEdges => b"edge".to_vec(),
                }
            }
        })
        .collect();
    Ok(Value::Text_array(Arc::new(values)))
}

pub fn write(order: Option<&[i64]>, owner: Owner, name: &str, mask: &[i64], geometry: &Geometry) -> Result<Geometry, Diagnostic> {
    name_check(name)?;
    if mask.len() as i64 != count(geometry, owner)? {
        return fail("E_GROUP_COUNT", "Group mask must match its owner element count.");
    }
    if mask.iter().any(|value| *value != 0 && *value != 1) {
        return fail("E_GROUP_MASK", "Group mask values must be 0 or 1.");
    }
    let order_error = |e: rdk_core::error::T| diag("E_GROUP_ORDER", rdk_core::error::message(&e));
    match (owner, order) {
        (Owner::Edge, Some(_)) => fail("E_GROUP_OWNER", "Edge groups do not have an explicit traversal order."),
        (Owner::Edge, None) => {
            let topology = geometry::topology(geometry);
            let index = topology_index::create(None, topology).map_err(order_error)?;
            let made = edge_group::init(None, topology, &index, name, &|i| Ok(mask[i as usize] == 1)).map_err(order_error)?;
            geometry::with_edge_group(made, geometry).map_err(order_error)
        }
        (Owner::Ordinary(owner), None) => {
            let made = group::init(None, owner, name, mask.len() as i64, &|i| Ok(mask[i as usize] == 1)).map_err(order_error)?;
            geometry::with_group(made, geometry).map_err(order_error)
        }
        (Owner::Ordinary(owner), Some(order)) => {
            let made = group::ordered(owner, name, mask.len() as i64, order).map_err(order_error)?;
            let total: i64 = mask.iter().sum();
            if group::cardinality(&made) != total || !order.iter().all(|index| mask.get(*index as usize) == Some(&1)) {
                return fail("E_GROUP_ORDER", "Explicit order must contain each selected mask index exactly once.");
            }
            geometry::with_group(made, geometry).map_err(order_error)
        }
    }
}

fn metadata_text(args: &flow_syntax::value::Record, key: &str) -> Result<String, Diagnostic> {
    match arg(args, key) {
        Some(Value::Text(text)) => Ok(text_of(text)),
        _ => fail("E_METADATA_TEXT", "Metadata names and patterns must be text."),
    }
}

fn group_text(args: &flow_syntax::value::Record, key: &str, default: &str) -> Result<String, Diagnostic> {
    match arg(args, key) {
        None => Ok(default.to_owned()),
        Some(Value::Text(text)) => Ok(text_of(text)),
        _ => fail("E_GROUP_TYPE", &format!("Group {key} must be text.")),
    }
}

fn cooked(args: &flow_syntax::value::Record, geometry: &dyn Fn(i64) -> Option<Geometry>) -> Result<Geometry, Diagnostic> {
    match arg(args, "geometry") {
        Some(Value::Deferred(t, id)) if ty::is_geometry(t) => match geometry(*id) {
            None => fail("E_DATA_SOURCE", "Group source has not been cooked."),
            Some(found) => Ok(found),
        },
        _ => fail("E_DATA_SOURCE", "Group source requires cooked geometry."),
    }
}

/// Resolve a `text/glob_*`, `text/group_name_*`, `sop/group*` data source.
pub fn resolve(geometry: &dyn Fn(i64) -> Option<Geometry>, source: &Value) -> Result<Value, Diagnostic> {
    let Value::Source(g) = source else {
        return fail("E_DATA_SOURCE", "Unknown group data source.");
    };
    let (operation, args) = (&g.name, &g.fields);
    match operation.as_str() {
        "text/glob_rewrite" | "text/glob_rewrite_set" | "text/glob_literal" => {
            let value = metadata_text(args, "text")?;
            if operation == "text/glob_literal" {
                attribute_metadata::glob_literal(&value).map(text_value)
            } else {
                let pattern = metadata_text(args, "pattern")?;
                let replacement = metadata_text(args, "replacement")?;
                let rewritten = if operation == "text/glob_rewrite_set" {
                    attribute_metadata::glob_rewrite_set(&pattern, &replacement, &value)
                } else {
                    attribute_metadata::glob_rewrite(&pattern, &replacement, &value)
                };
                rewritten.map(text_value)
            }
        }
        "text/glob_rewrite_set_matches" => {
            let value = metadata_text(args, "text")?;
            let pattern = metadata_text(args, "pattern")?;
            let replacement = metadata_text(args, "replacement")?;
            attribute_metadata::glob_rewrite_set_matches(&pattern, &replacement, &value).map(Value::Bool)
        }
        "text/group_name_valid" | "text/group_name_forced" => match arg(args, "text") {
            Some(Value::Text(bytes)) => {
                let value = text_of(bytes);
                Ok(if operation == "text/group_name_valid" {
                    Value::Bool(group_ops::valid_group_name(&value))
                } else {
                    text_value(group_ops::force_valid_group_name(&value))
                })
            }
            _ => fail("E_METADATA_TEXT", "Metadata names and patterns must be text."),
        },
        "sop/group_entries" => {
            let component = group_text(args, "component", "")?;
            if component != "owners" && component != "names" {
                return fail("E_GROUP_TYPE", "Group entry component is owners or names.");
            }
            let selected_owner = group_text(args, "owner", "any")?;
            let pattern = group_text(args, "pattern", "")?;
            let found = cooked(args, geometry)?;
            entries(&selected_owner, &pattern, &component, &found)
        }
        "sop/group" | "sop/group_names" | "sop/group_order" => {
            let owner_name = group_text(args, "owner", "point")?;
            let owner = owner(&owner_name)?;
            let found = cooked(args, geometry)?;
            if operation == "sop/group_names" {
                let pattern = group_text(args, "pattern", "")?;
                return names(owner, &pattern, &found);
            }
            let group_name = group_text(args, "name", "")?;
            name_check(&group_name)?;
            if operation == "sop/group" {
                match arg(args, "as_owner") {
                    None => read(owner, &group_name, &found),
                    Some(Value::Text(target)) if target.as_ref() == b"point" => read_points(owner, &group_name, &found),
                    Some(Value::Text(_)) => fail("E_GROUP_TARGET", "Group promotion target is point."),
                    Some(_) => fail("E_GROUP_TYPE", "Group as_owner must be text."),
                }
            } else {
                let Value::Record(fields) = order(owner, &group_name, &found)? else {
                    unreachable!("order returns a record");
                };
                match arg(args, "component") {
                    Some(Value::Text(component)) => match fields.iter().find(|(key, _)| key.as_bytes() == component.as_ref()) {
                        Some((_, value)) => Ok(value.clone()),
                        None => fail("E_GROUP_TYPE", "Group order component is ordered or indices."),
                    },
                    _ => fail("E_GROUP_TYPE", "Group order component is ordered or indices."),
                }
            }
        }
        other => fail("E_DATA_SOURCE", &format!("Unknown group data source {other}.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::topology_kernel_impl::mesh;

    fn triangle() -> Geometry {
        mesh(&[
            ("points".into(), Value::Floats(3, Arc::new(vec![0., 0., 0., 1., 0., 0., 0., 1., 0.]))),
            ("vertex_points".into(), Value::Int_array(Arc::new(vec![0, 1, 2]))),
            ("primitive_offsets".into(), Value::Int_array(Arc::new(vec![0, 3]))),
            ("kinds".into(), Value::Int_array(Arc::new(vec![0]))),
        ])
        .unwrap()
    }

    #[test]
    fn owners_and_names() {
        assert_eq!(owner("edge").unwrap(), Owner::Edge);
        assert_eq!(owner("x").err().unwrap().code, "E_GROUP_OWNER");
        assert_eq!(name_check("  ").err().unwrap().code, "E_GROUP_NAME");
    }

    #[test]
    fn write_then_read_round_trip() {
        let g = triangle();
        let p = Owner::Ordinary(group::Owner::Point);
        let written = write(None, p, "hot", &[1, 0, 1], &g).unwrap();
        assert_eq!(read(p, "hot", &written).unwrap(), Value::Int_array(Arc::new(vec![1, 0, 1])));
        assert_eq!(read(p, "cold", &written).err().unwrap().code, "E_GROUP_MISSING");
        assert_eq!(write(None, p, "hot", &[1, 0], &g).err().unwrap().code, "E_GROUP_COUNT");
        assert_eq!(write(None, p, "hot", &[1, 2, 0], &g).err().unwrap().code, "E_GROUP_MASK");
        assert_eq!(write(Some(&[0]), Owner::Edge, "e", &[0, 0, 0], &g).err().unwrap().code, "E_GROUP_OWNER");
        let ordered = write(Some(&[2, 0]), p, "ord", &[1, 0, 1], &g).unwrap();
        assert!(group::is_ordered(&geometry::find_group(group::Owner::Point, "ord", &ordered).unwrap()));
        assert_eq!(write(Some(&[2]), p, "ord", &[1, 0, 1], &g).err().unwrap().code, "E_GROUP_ORDER");
    }

    #[test]
    fn names_and_entries() {
        let g = write(None, Owner::Ordinary(group::Owner::Primitive), "quad", &[1], &triangle()).unwrap();
        assert_eq!(
            names(Owner::Ordinary(group::Owner::Primitive), "*", &g).unwrap(),
            Value::Text_array(Arc::new(vec![b"quad".to_vec()].into()))
        );
        assert_eq!(
            entries("any", "*", "owners", &g).unwrap(),
            Value::Text_array(Arc::new(vec![b"primitive".to_vec()].into()))
        );
    }
}
