//! Concatenate geometries with matching attribute schemas. Group schemas are unioned in first-seen
//! order; a missing group contributes no members. Detail attributes still require an explicit merge
//! policy.
//!
//! `source_attribute` also writes a primitive int attribute holding, per primitive, `source_base`
//! (default 0) plus the index of the input it came from. An input that already has the attribute
//! keeps its values, so a merge of merges keeps the innermost tag.
use std::sync::Arc;
use rays_math::text_array::TextArray;

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, ragged_ops, support, topology, topology_index,
};
use rdk_core::error::Code;

const OPERATION: &str = "merge";


pub struct RunParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub source_attribute: Option<String>,
    pub source_base: Option<i64>,
}

pub struct RunTaggedParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub tag_base: i64,
    pub source_attribute: Option<String>,
    pub source_base: i64,
}

/// The primitive int attribute the editor's merge lowering writes.
pub(crate) const PROVENANCE_ATTRIBUTE: &str = "__flow_src";

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

/// Merge storage/schema failures use schema_mismatch under operation merge.
fn fail(message: &str) -> error::T {
    make(Code::SchemaMismatch, message.to_owned())
}

/// Recode a shared helper failure under merge.
fn lift(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        error::cancelled(OPERATION)
    } else {
        make(Code::SchemaMismatch, error::message(&value).to_owned())
    }
}

fn same_attribute_schema(left: &attribute::T, right: &attribute::T) -> bool {
    attribute::owner(left) == attribute::owner(right)
        && attribute::name(left) == attribute::name(right)
        && attribute::kind_name(left) == attribute::kind_name(right)
}

fn find_matching(template: &attribute::T, geometry_value: &geometry::T) -> Option<attribute::T> {
    geometry::attributes(geometry_value).into_iter().find(|item| same_attribute_schema(template, item))
}

fn same_group_schema(left: &group::T, right: &group::T) -> bool {
    group::owner(left) == group::owner(right) && group::name(left) == group::name(right)
}

fn find_matching_group(template: &group::T, geometry_value: &geometry::T) -> Option<group::T> {
    geometry::groups(geometry_value).into_iter().find(|item| same_group_schema(template, item))
}

fn find_matching_edge_group(template: &edge_group::T, geometry_value: &geometry::T) -> Option<edge_group::T> {
    geometry::find_edge_group(edge_group::name(template), geometry_value)
}

fn empty_points() -> Result<geometry::T, error::T> {
    let positions = packed::float3::view::of_owned_exn(Vec::new(), Vec::new(), Vec::new()).map_err(lift)?;
    geometry::create(positions, topology::empty(0).map_err(lift)?, None, None, None).map_err(lift)
}

fn mismatch() -> error::T {
    fail("sop/merge: attribute schemas must match exactly")
}

fn concatenate_attribute(template: &attribute::T, attributes: &[attribute::T]) -> Result<attribute::T, error::T> {
    use attribute::Storage;
    let storages: Vec<&Storage> =
        attributes.iter().map(|item| attribute::view::storage(item).map_err(lift)).collect::<Result<Vec<_>, error::T>>()?;
    let template_storage = attribute::view::storage(template).map_err(lift)?;
    let storage = match template_storage {
        Storage::Float(_) | Storage::Float2(_) | Storage::Float3(_) | Storage::Float4(_) => {
            let mut output: Vec<Vec<f64>> = attribute::float_planes(template_storage).map_err(lift)?
                .expect("a float-kind storage has planes").iter().map(|_| Vec::new()).collect();
            for item in &storages {
                let planes = attribute::float_planes(item).map_err(lift)?.ok_or_else(mismatch)?;
                if planes.len() != output.len() { return Err(mismatch()); }
                for (target, plane) in output.iter_mut().zip(planes) { target.extend_from_slice(plane); }
            }
            attribute::of_float_planes(template_storage, output).map_err(lift)?
        }
        Storage::Int(_) => {
            let mut out = Vec::new();
            for item in &storages {
                match item {
                    Storage::Int(values) => out.extend_from_slice(values),
                    _ => return Err(mismatch()),
                }
            }
            Storage::Int(Arc::new(out))
        }
        Storage::Text(_) => {
            let mut out = TextArray::new();
            for item in &storages {
                match item {
                    Storage::Text(values) => out.extend(values.iter()),
                    _ => return Err(mismatch()),
                }
            }
            Storage::Text(Arc::new(out))
        }
        Storage::IntArray(_) => {
            let mut sources = Vec::new();
            for item in &storages {
                match item {
                    Storage::IntArray(values) => sources.push(values.clone()),
                    _ => return Err(mismatch()),
                }
            }
            Storage::IntArray(ragged_ops::concat_int(&sources).map_err(lift)?)
        }
        Storage::FloatArray(_) => {
            let mut sources = Vec::new();
            for item in &storages {
                match item {
                    Storage::FloatArray(values) => sources.push(values.clone()),
                    _ => return Err(mismatch()),
                }
            }
            Storage::FloatArray(ragged_ops::concat_float(&sources).map_err(lift)?)
        }
    };
    attribute::create_owned(attribute::name(template), attribute::owner(template), storage).map_err(lift)
}

use rdk_core::geometry::group_count as owner_count;

/// Members of a group in its preferred (ordered, else ascending) order.
fn members_preferred(value: &group::T) -> Vec<i64> {
    match group::ordered_elements(value) {
        Some(order) => order,
        None => (0..group::length(value)).filter(|&index| group::mem(index, value)).collect(),
    }
}

fn merge_plain(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    geometries: &[geometry::T],
) -> Result<geometry::T, error::T> {
    let grain = grain.unwrap_or(16_384);
    support::check_positive(grain).map_err(|_| make(Code::InvalidArgument, "sop/merge: grain must be positive".to_owned()))?;
    let Some(first) = geometries.first() else {
        return empty_points();
    };
    let first_attributes = geometry::attributes(first);
    let mut groups: Vec<group::T> = Vec::new();
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    for item in geometries {
        for candidate in geometry::groups(item) {
            if !groups.iter().any(|existing| same_group_schema(&candidate, existing)) {
                groups.push(candidate);
            }
        }
        for candidate in geometry::edge_groups(item) {
            if !edge_groups.iter().any(|existing| edge_group::name(existing) == edge_group::name(&candidate)) {
                edge_groups.push(candidate);
            }
        }
    }
    let exact_schema = |item: &geometry::T| {
        let attributes = geometry::attributes(item);
        attributes.len() == first_attributes.len()
            && first_attributes.iter().all(|template| find_matching(template, item).is_some())
    };
    if !geometries.iter().all(exact_schema) {
        return Err(fail("sop/merge: attribute schemas must match exactly"));
    }
    if first_attributes.iter().any(|item| attribute::owner(item) == attribute::Owner::Detail) {
        return Err(fail("sop/merge: detail attributes need an explicit merge policy"));
    }
    let point_count: i64 = geometries.iter().map(geometry::point_count).sum();
    let vertex_count: i64 = geometries.iter().map(geometry::vertex_count).sum();
    let primitive_count: i64 = geometries.iter().map(geometry::primitive_count).sum();
    let mut px = Vec::with_capacity(point_count as usize);
    let mut py = Vec::with_capacity(point_count as usize);
    let mut pz = Vec::with_capacity(point_count as usize);
    let mut vertex_points: Vec<u32> = Vec::with_capacity(vertex_count as usize);
    let mut primitive_offsets: Vec<u32> = Vec::with_capacity(primitive_count as usize + 1);
    primitive_offsets.push(0);
    let mut primitive_kinds: Vec<u8> = Vec::with_capacity(primitive_count as usize);
    let (mut point_at, mut vertex_at) = (0_i64, 0_i64);
    for item in geometries {
        cancel::check_opt(cancel_value).map_err(lift)?;
        let positions = packed::float3::view::view(geometry::positions(item)).map_err(lift)?;
        let view = topology::view::view(geometry::topology(item)).map_err(lift)?;
        px.extend_from_slice(&positions.x);
        py.extend_from_slice(&positions.y);
        pz.extend_from_slice(&positions.z);
        vertex_points.extend(view.vertex_points.iter().map(|&point| point + point_at as u32));
        primitive_offsets.extend(view.primitive_offsets[1..].iter().map(|&offset| offset + vertex_at as u32));
        primitive_kinds.extend_from_slice(&view.primitive_kinds);
        point_at += geometry::point_count(item);
        vertex_at += geometry::vertex_count(item);
    }
    let positions = packed::float3::view::of_owned_exn(px, py, pz).map_err(lift)?;
    let topology_value =
        topology::view::create_validated_owned(point_count, vertex_points, primitive_offsets, primitive_kinds).map_err(lift)?;
    let mut attributes = Vec::new();
    for template in &first_attributes {
        let values: Vec<attribute::T> =
            geometries.iter().map(|item| find_matching(template, item).ok_or_else(mismatch)).collect::<Result<_, error::T>>()?;
        attributes.push(concatenate_attribute(template, &values)?);
    }
    let mut merged_groups = Vec::new();
    for template in &groups {
        let owner = group::owner(template);
        let total: i64 = geometries.iter().map(|item| owner_count(item, owner)).sum();
        let mut builder = group::builder::create(owner, group::name(template), total).map_err(lift)?;
        let mut offset = 0_i64;
        let mut sources: Vec<(i64, Option<group::T>)> = Vec::new();
        for item in geometries {
            sources.push((offset, find_matching_group(template, item)));
            offset += owner_count(item, owner);
        }
        for (source_offset, member) in &sources {
            if let Some(member) = member {
                for index in 0..group::length(member) {
                    if group::mem(index, member) {
                        group::builder::set(&mut builder, source_offset + index, true).map_err(lift)?;
                    }
                }
            }
        }
        let target = group::builder::freeze(&mut builder).map_err(lift)?;
        if !sources.iter().any(|(_, member)| member.as_ref().is_some_and(group::is_ordered)) {
            merged_groups.push(target);
        } else {
            let mut order = Vec::with_capacity(group::cardinality(&target) as usize);
            for (source_offset, member) in &sources {
                if let Some(member) = member {
                    order.extend(members_preferred(member).into_iter().map(|element| source_offset + element));
                }
            }
            merged_groups.push(group::view::with_owned_order(order, &target).map_err(lift)?);
        }
    }
    let mut merged_edge_groups = Vec::new();
    if !edge_groups.is_empty() {
        let target_index = topology_index::create(cancel_value, &topology_value).map_err(lift)?;
        for template in &edge_groups {
            let mut builder =
                edge_group::builder::create(&topology_value, &target_index, edge_group::name(template)).map_err(lift)?;
            let mut point_offset = 0_i64;
            for item in geometries {
                let source_index = topology_index::create(cancel_value, geometry::topology(item)).map_err(lift)?;
                if let Some(member) = find_matching_edge_group(template, item) {
                    for edge in 0..edge_group::length(&member) {
                        if edge_group::mem(edge, &member) {
                            let (a, b) = topology_index::edge_points(&source_index, edge).map_err(lift)?;
                            if let Some(target) = topology_index::find_edge(&target_index, a + point_offset, b + point_offset) {
                                edge_group::builder::set(&mut builder, target, true).map_err(lift)?;
                            }
                        }
                    }
                }
                point_offset += geometry::point_count(item);
            }
            merged_edge_groups.push(edge_group::builder::freeze(&mut builder).map_err(lift)?);
        }
    }
    geometry::create(positions, topology_value, Some(attributes), Some(merged_groups), Some(merged_edge_groups))
        .map_err(lift)
}

/// An input without `name` is tagged `base + its index`; one that has it keeps its values, so
/// nested merges keep the innermost tag.
fn tag_inputs(name: &str, base: i64, geometries: &[geometry::T]) -> Result<Vec<geometry::T>, error::T> {
    let owner = attribute::Owner::Primitive;
    let mut tagged = Vec::with_capacity(geometries.len());
    for (index, item) in geometries.iter().enumerate() {
        if geometry::find_attribute(owner, name, item).is_some() {
            tagged.push(item.clone());
        } else {
            let values = vec![base + index as i64; geometry::primitive_count(item) as usize];
            let created = attribute::create_owned(name, owner, attribute::Storage::Int(Arc::new(values))).map_err(lift)?;
            tagged.push(geometry::with_attribute(created, item).map_err(lift)?);
        }
    }
    Ok(tagged)
}

/// `source_base + input index` per primitive, with `source_base` for input zero.
fn source_values(count: i64, source_base: i64, geometries: &[geometry::T], cancel_value: Option<&cancel::T>) -> Result<Vec<i64>, error::T> {
    let mut values = vec![source_base; count as usize];
    let mut offset = 0_usize;
    for (index, item) in geometries.iter().enumerate() {
        cancel::check_opt(cancel_value).map_err(lift)?;
        let here = geometry::primitive_count(item) as usize;
        if index > 0 {
            values[offset..offset + here].fill(source_base + index as i64);
        }
        offset += here;
    }
    Ok(values)
}

fn merge(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    source_attribute: Option<&str>,
    source_base: i64,
    geometries: &[geometry::T],
) -> Result<geometry::T, error::T> {
    let owner = attribute::Owner::Primitive;
    match source_attribute {
        None => merge_plain(cancel_value, grain, geometries),
        Some(name)
            if !geometries.is_empty()
                && !(name).trim().is_empty()
                && geometries.iter().all(|item| geometry::find_attribute(owner, name, item).is_none()) =>
        {
            let merged = merge_plain(cancel_value, grain, geometries)?;
            cancel::check_opt(cancel_value).map_err(lift)?;
            let values = source_values(geometry::primitive_count(&merged), source_base, geometries, cancel_value)?;
            let created = attribute::create_owned(name, owner, attribute::Storage::Int(Arc::new(values))).map_err(lift)?;
            geometry::with_attribute(created, &merged).map_err(lift)
        }
        Some(name) => {
            let tagged = tag_inputs(name, source_base, geometries)?;
            merge_plain(cancel_value, grain, &tagged)
        }
    }
}

/// Concatenate geometries with matching attribute schemas (see the module notes).
pub fn run(params: &RunParams, geometries: &[geometry::T]) -> Result<geometry::T, error::T> {
    merge(
        params.cancel.as_deref(),
        params.grain,
        params.source_attribute.as_deref(),
        params.source_base.unwrap_or(0),
        geometries,
    )
}

/// `run` plus the provenance attribute [`PROVENANCE_ATTRIBUTE`] numbered from `tag_base` (negative:
/// none) beside an authored `source_attribute`/`source_base`. Without an authored attribute it is
/// `run` with the provenance attribute and `source_base = tag_base`.
pub fn run_tagged(params: &RunTaggedParams, geometries: &[geometry::T]) -> Result<geometry::T, error::T> {
    let authored = match &params.source_attribute {
        Some(name) if !(name).trim().is_empty() => Some((name).trim().to_owned()),
        _ => None,
    };
    let call = |source_attribute: Option<String>, source_base: i64| {
        run(
            &RunParams {
                cancel: params.cancel.clone(),
                grain: params.grain,
                source_attribute,
                source_base: Some(source_base),
            },
            geometries,
        )
    };
    if params.tag_base < 0 {
        return call(params.source_attribute.clone(), params.source_base);
    }
    let Some(attribute_name) = authored else {
        return call(Some(PROVENANCE_ATTRIBUTE.to_owned()), params.tag_base);
    };
    let tag = PROVENANCE_ATTRIBUTE;
    let tagged = call(Some(tag.to_owned()), params.tag_base)?;
    let cancel_value = params.cancel.as_deref();
    cancel::check_opt(cancel_value).map_err(lift)?;
    let owner = attribute::Owner::Primitive;
    let has_tag = |item: &geometry::T| geometry::find_attribute(owner, tag, item).is_some();
    if attribute_name != tag && geometries.iter().any(has_tag) && geometries.iter().any(|item| !has_tag(item)) {
        return Err(mismatch());
    }
    if geometries.is_empty() || geometry::find_attribute(owner, &attribute_name, &tagged).is_some() {
        return Ok(tagged);
    }
    let values = source_values(geometry::primitive_count(&tagged), params.source_base, geometries, cancel_value)?;
    let created = attribute::create_owned(&attribute_name, owner, attribute::Storage::Int(Arc::new(values))).map_err(lift)?;
    geometry::with_attribute(created, &tagged).map_err(lift)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;

    fn polyline(points: &[(f64, f64, f64)]) -> geometry::T {
        let topology_value = topology::create_owned(
            points.len() as i64,
            (0..points.len() as u32).collect(),
            vec![0, points.len() as u32],
            vec![PrimitiveKind::OpenPolyline],
        ).unwrap();
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None).unwrap()
    }

    fn params(source_attribute: Option<&str>, source_base: Option<i64>) -> RunParams {
        RunParams { cancel: None, grain: None, source_attribute: source_attribute.map(str::to_owned), source_base }
    }

    fn ints(name: &str, owner: attribute::Owner, value: &geometry::T) -> Vec<i64> {
        match attribute::view::storage(&geometry::find_attribute(owner, name, value).unwrap()).unwrap() {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("{name} storage"),
        }
    }

    fn pair() -> Vec<geometry::T> {
        let weight = |values: Vec<f64>| {
            attribute::create_owned("weight", attribute::Owner::Point, attribute::Storage::Float(Arc::new(values))).unwrap()
        };
        let a = geometry::with_attribute(weight(vec![0., 1.]), &polyline(&[(0., 0., 0.), (1., 0., 0.)])).unwrap();
        let b = geometry::with_attribute(weight(vec![10., 12.]), &polyline(&[(10., 0., 0.), (12., 0., 0.)])).unwrap();
        let ordered = group::ordered(group::Owner::Primitive, "second", 1, &[0]).unwrap();
        vec![a, geometry::with_group(ordered, &b).unwrap()]
    }

    #[test]
    fn concatenates_payload_and_unions_groups() {
        let merged = run(&params(None, None), &pair()).unwrap();
        assert_eq!(geometry::point_count(&merged), 4);
        assert_eq!(geometry::primitive_count(&merged), 2);
        let topology_value = topology::view::view(geometry::topology(&merged)).unwrap();
        assert_eq!(topology_value.vertex_points[..], [0, 1, 2, 3]);
        assert_eq!(topology_value.primitive_offsets[..], [0, 2, 4]);
        match attribute::view::storage(&geometry::find_attribute(attribute::Owner::Point, "weight", &merged).unwrap()).unwrap() {
            attribute::Storage::Float(values) => assert_eq!(values[..], [0., 1., 10., 12.]),
            _ => panic!("weight storage"),
        }
        let second = geometry::find_group(group::Owner::Primitive, "second", &merged).unwrap();
        assert!(!group::mem(0, &second) && group::mem(1, &second));
        assert_eq!(group::ordered_elements(&second), Some(vec![1]));
    }

    #[test]
    fn source_attribute_tags_inputs() {
        let merged = run(&params(Some("src"), Some(5)), &pair()).unwrap();
        assert_eq!(ints("src", attribute::Owner::Primitive, &merged), [5, 6]);
        let tagged = RunTaggedParams { cancel: None, grain: None, tag_base: 3, source_attribute: None, source_base: 0 };
        let merged = run_tagged(&tagged, &pair()).unwrap();
        assert_eq!(ints(PROVENANCE_ATTRIBUTE, attribute::Owner::Primitive, &merged), [3, 4]);
        let both = RunTaggedParams {
            cancel: None,
            grain: None,
            tag_base: 3,
            source_attribute: Some("authored".to_owned()),
            source_base: 20,
        };
        let merged = run_tagged(&both, &pair()).unwrap();
        assert_eq!(ints(PROVENANCE_ATTRIBUTE, attribute::Owner::Primitive, &merged), [3, 4]);
        assert_eq!(ints("authored", attribute::Owner::Primitive, &merged), [20, 21]);
    }

    #[test]
    fn schema_mismatch_and_empty() {
        let mut inputs = pair();
        inputs[1] = polyline(&[(10., 0., 0.), (12., 0., 0.)]);
        match run(&params(None, None), &inputs) {
            Err(failure) => {
                assert_eq!(error::code(&failure), Code::SchemaMismatch);
                assert_eq!(error::message(&failure), "sop/merge: attribute schemas must match exactly");
            }
            Ok(_) => panic!("expected schema mismatch"),
        }
        let empty = run(&params(None, None), &[]).unwrap();
        assert_eq!(geometry::point_count(&empty), 0);
    }
}
