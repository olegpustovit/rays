use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::{attribute, edge_group, error, group, packed, support, topology};

#[derive(Clone)]
pub struct T {
    positions: packed::Float3::T,
    topology: topology::T,
    attributes: Arc<Vec<attribute::T>>,
    groups: Arc<Vec<group::T>>,
    edge_groups: Arc<Vec<edge_group::T>>,
    data_id: i64,
}

fn owner_count(value: &topology::T, owner: attribute::Owner) -> i64 {
    match owner {
        attribute::Owner::Point => topology::point_count(value),
        attribute::Owner::Vertex => topology::vertex_count(value),
        attribute::Owner::Primitive => topology::primitive_count(value),
        attribute::Owner::Detail => 1,
    }
}

fn group_count(value: &topology::T, owner: group::Owner) -> i64 {
    match owner {
        group::Owner::Point => topology::point_count(value),
        group::Owner::Vertex => topology::vertex_count(value),
        group::Owner::Primitive => topology::primitive_count(value),
    }
}

fn geometry_error(message: String) -> error::T {
    error::internal("geometry", "invalid_geometry", message)
}

fn attribute_owner_tag(owner: attribute::Owner) -> u8 {
    match owner {
        attribute::Owner::Point => 0,
        attribute::Owner::Vertex => 1,
        attribute::Owner::Primitive => 2,
        attribute::Owner::Detail => 3,
    }
}

fn group_owner_tag(owner: group::Owner) -> u8 {
    match owner {
        group::Owner::Point => 0,
        group::Owner::Vertex => 1,
        group::Owner::Primitive => 2,
    }
}

fn validate(
    positions: &packed::Float3::T,
    topology_value: &topology::T,
    attributes: &[attribute::T],
    groups: &[group::T],
    edge_groups: &[edge_group::T],
) -> Result<(), error::T> {
    if packed::Float3::length(positions) != topology::point_count(topology_value) {
        return Err(geometry_error(
            "Geometry.create: position count must match topology point count".to_owned(),
        ));
    }
    if attributes.iter().any(|item| {
        attribute::owner(item) == attribute::Owner::Point && attribute::name(item) == "P"
    }) {
        return Err(geometry_error(
            "Geometry.create: P is the canonical position buffer, not an ordinary attribute".to_owned(),
        ));
    }
    for item in attributes {
        if attribute::length(item) != owner_count(topology_value, attribute::owner(item)) {
            return Err(geometry_error(format!(
                "Geometry.create: attribute {} has the wrong element count",
                attribute::name(item)
            )));
        }
    }
    for item in groups {
        if group::length(item) != group_count(topology_value, group::owner(item)) {
            return Err(geometry_error(format!(
                "Geometry.create: group {} has the wrong element count",
                group::name(item)
            )));
        }
    }
    let mut attribute_keys = HashSet::with_capacity(attributes.len());
    for item in attributes {
        let key = (attribute_owner_tag(attribute::owner(item)), attribute::name(item));
        if !attribute_keys.insert(key) {
            return Err(geometry_error(format!(
                "Geometry.create: duplicate attribute {}",
                attribute::name(item)
            )));
        }
    }
    let mut group_keys = HashSet::with_capacity(groups.len());
    for item in groups {
        let key = (group_owner_tag(group::owner(item)), group::name(item));
        if !group_keys.insert(key) {
            return Err(geometry_error(format!(
                "Geometry.create: duplicate group {}",
                group::name(item)
            )));
        }
    }
    for item in edge_groups {
        if edge_group::topology_data_id(item) != topology::data_id(topology_value) {
            return Err(geometry_error(format!(
                "Geometry.create: edge group {} belongs to a different topology",
                edge_group::name(item)
            )));
        }
    }
    let mut edge_group_names = HashSet::with_capacity(edge_groups.len());
    for item in edge_groups {
        if !edge_group_names.insert(edge_group::name(item)) {
            return Err(geometry_error(format!(
                "Geometry.create: duplicate edge group {}",
                edge_group::name(item)
            )));
        }
    }
    Ok(())
}

fn make(
    positions: packed::Float3::T,
    topology_value: topology::T,
    attributes: Vec<attribute::T>,
    groups: Vec<group::T>,
    edge_groups: Vec<edge_group::T>,
) -> T {
    T {
        positions,
        topology: topology_value,
        attributes: Arc::new(attributes),
        groups: Arc::new(groups),
        edge_groups: Arc::new(edge_groups),
        data_id: support::fresh_data_id(),
    }
}

pub fn create(
    positions: packed::Float3::T,
    topology_value: topology::T,
    attributes: Option<Vec<attribute::T>>,
    groups: Option<Vec<group::T>>,
    edge_groups: Option<Vec<edge_group::T>>,
)-> Result<T, error::T> {
    let attributes = attributes.unwrap_or_default();
    let groups = groups.unwrap_or_default();
    let edge_groups = edge_groups.unwrap_or_default();
    validate(&positions, &topology_value, &attributes, &groups, &edge_groups)
        ?;
    Ok(make(positions, topology_value, attributes, groups, edge_groups))
}

pub fn data_id(value: &T) -> i64 {
    value.data_id
}

pub fn fresh_data_id() -> i64 {
    support::fresh_data_id()
}

pub fn positions(value: &T) -> &packed::Float3::T {
    &value.positions
}

pub fn topology(value: &T) -> &topology::T {
    &value.topology
}

pub fn point_count(value: &T) -> i64 {
    topology::point_count(&value.topology)
}

pub fn vertex_count(value: &T) -> i64 {
    topology::vertex_count(&value.topology)
}

pub fn primitive_count(value: &T) -> i64 {
    topology::primitive_count(&value.topology)
}

pub fn payload_bytes(value: &T) -> i64 {
    packed::Float3::payload_bytes(&value.positions)
        + topology::payload_bytes(&value.topology)
        + value.attributes.iter().map(attribute::payload_bytes).sum::<i64>()
        + value.groups.iter().map(group::payload_bytes).sum::<i64>()
        + value.edge_groups.iter().map(edge_group::payload_bytes).sum::<i64>()
}

pub fn payload_components(value: &T) -> Vec<(i64, i64)> {
    let mut components = Vec::with_capacity(
        2 + value.attributes.len() + value.groups.len() + value.edge_groups.len(),
    );
    components.push((
        packed::Float3::data_id(&value.positions),
        packed::Float3::payload_bytes(&value.positions),
    ));
    components.push((
        topology::data_id(&value.topology),
        topology::payload_bytes(&value.topology),
    ));
    components.extend(value.attributes.iter().map(|item| {
        (attribute::storage_id(item), attribute::payload_bytes(item))
    }));
    components.extend(value.groups.iter().map(|item| (group::data_id(item), group::payload_bytes(item))));
    components.extend(value.edge_groups.iter().map(|item| {
        (edge_group::data_id(item), edge_group::payload_bytes(item))
    }));
    components
}

pub fn releasers(value: &T) -> Vec<(i64, Arc<dyn Fn(bool) + Send + Sync>)> {
    let mut releasers = Vec::new();
    if let Some(release) = packed::Float3::releaser(&value.positions) {
        releasers.push((packed::Float3::data_id(&value.positions), release));
    }
    if let Some(release) = topology::releaser(&value.topology) {
        releasers.push((topology::data_id(&value.topology), release));
    }
    for item in value.attributes.iter() {
        if let Some(release) = attribute::releaser(item) {
            releasers.push((attribute::storage_id(item), release));
        }
    }
    releasers
}

pub fn attributes(value: &T) -> Vec<attribute::T> {
    value.attributes.as_ref().clone()
}

pub fn groups(value: &T) -> Vec<group::T> {
    value.groups.as_ref().clone()
}

pub fn edge_groups(value: &T) -> Vec<edge_group::T> {
    value.edge_groups.as_ref().clone()
}

pub fn find_attribute(owner: attribute::Owner, name: &str, value: &T) -> Option<attribute::T> {
    value
        .attributes
        .iter()
        .find(|item| attribute::owner(item) == owner && attribute::name(item) == name)
        .cloned()
}

pub fn find_group(owner: group::Owner, name: &str, value: &T) -> Option<group::T> {
    value
        .groups
        .iter()
        .find(|item| group::owner(item) == owner && group::name(item) == name)
        .cloned()
}

pub fn find_edge_group(name: &str, value: &T) -> Option<edge_group::T> {
    value
        .edge_groups
        .iter()
        .find(|item| edge_group::name(item) == name)
        .cloned()
}

pub fn with_positions(positions_value: packed::Float3::T, value: &T) -> Result<T, error::T> {
    if packed::Float3::length(&positions_value) != point_count(value) {
        return Err(geometry_error("Geometry.with_positions: point count changed".to_owned()));
    }
    Ok(make(
        positions_value,
        value.topology.clone(),
        value.attributes.as_ref().clone(),
        value.groups.as_ref().clone(),
        value.edge_groups.as_ref().clone(),
    ))
}

fn replace_attribute(item: attribute::T, values: &[attribute::T]) -> Vec<attribute::T> {
    let owner = attribute::owner(&item);
    let name = attribute::name(&item);
    let mut output = values.to_vec();
    if let Some(index) = output
        .iter()
        .position(|value| attribute::owner(value) == owner && attribute::name(value) == name)
    {
        output[index] = item;
    } else {
        output.push(item);
    }
    output
}

fn replace_group(item: group::T, values: &[group::T]) -> Vec<group::T> {
    let owner = group::owner(&item);
    let name = group::name(&item);
    let mut output = values.to_vec();
    if let Some(index) = output
        .iter()
        .position(|value| group::owner(value) == owner && group::name(value) == name)
    {
        output[index] = item;
    } else {
        output.push(item);
    }
    output
}

fn replace_edge_group(item: edge_group::T, values: &[edge_group::T]) -> Vec<edge_group::T> {
    let name = edge_group::name(&item);
    let mut output = values.to_vec();
    if let Some(index) = output
        .iter()
        .position(|value| edge_group::name(value) == name)
    {
        output[index] = item;
    } else {
        output.push(item);
    }
    output
}

pub fn with_attribute(item: attribute::T, value: &T) -> Result<T, error::T> {
    if attribute::owner(&item) == attribute::Owner::Point && attribute::name(&item) == "P" {
        return Err(geometry_error(
            "Geometry.with_attribute: use with_positions to replace canonical P".to_owned(),
        ));
    }
    if attribute::length(&item) != owner_count(&value.topology, attribute::owner(&item)) {
        return Err(geometry_error(
            "Geometry.with_attribute: element count does not match owner".to_owned(),
        ));
    }
    Ok(make(
        value.positions.clone(),
        value.topology.clone(),
        replace_attribute(item, &value.attributes),
        value.groups.as_ref().clone(),
        value.edge_groups.as_ref().clone(),
    ))
}

pub fn with_group(item: group::T, value: &T) -> Result<T, error::T> {
    if group::length(&item) != group_count(&value.topology, group::owner(&item)) {
        return Err(geometry_error(
            "Geometry.with_group: element count does not match owner".to_owned(),
        ));
    }
    Ok(make(
        value.positions.clone(),
        value.topology.clone(),
        value.attributes.as_ref().clone(),
        replace_group(item, &value.groups),
        value.edge_groups.as_ref().clone(),
    ))
}
pub fn with_edge_group(item: edge_group::T, value: &T) -> Result<T, error::T> {
    if edge_group::topology_data_id(&item) != topology::data_id(&value.topology) {
        return Err(geometry_error(
            "Geometry.with_edge_group: group belongs to a different topology".to_owned(),
        ));
    }
    Ok(make(
        value.positions.clone(),
        value.topology.clone(),
        value.attributes.as_ref().clone(),
        value.groups.as_ref().clone(),
        replace_edge_group(item, &value.edge_groups),
    ))
}

pub fn without_attribute(owner: attribute::Owner, name: &str, value: &T) -> T {
    if !value
        .attributes
        .iter()
        .any(|item| attribute::owner(item) == owner && attribute::name(item) == name)
    {
        return value.clone();
    }
    make(
        value.positions.clone(),
        value.topology.clone(),
        value
            .attributes
            .iter()
            .filter(|item| attribute::owner(item) != owner || attribute::name(item) != name)
            .cloned()
            .collect(),
        value.groups.as_ref().clone(),
        value.edge_groups.as_ref().clone(),
    )
}

pub fn without_group(owner: group::Owner, name: &str, value: &T) -> T {
    if !value
        .groups
        .iter()
        .any(|item| group::owner(item) == owner && group::name(item) == name)
    {
        return value.clone();
    }
    make(
        value.positions.clone(),
        value.topology.clone(),
        value.attributes.as_ref().clone(),
        value
            .groups
            .iter()
            .filter(|item| group::owner(item) != owner || group::name(item) != name)
            .cloned()
            .collect(),
        value.edge_groups.as_ref().clone(),
    )
}

pub fn without_edge_group(name: &str, value: &T) -> T {
    if !value
        .edge_groups
        .iter()
        .any(|item| edge_group::name(item) == name)
    {
        return value.clone();
    }
    make(
        value.positions.clone(),
        value.topology.clone(),
        value.attributes.as_ref().clone(),
        value.groups.as_ref().clone(),
        value
            .edge_groups
            .iter()
            .filter(|item| edge_group::name(item) != name)
            .cloned()
            .collect(),
    )
}

pub fn rename_edge_group(from: &str, into: &str, value: &T) -> Result<T, error::T> {
    if from == into {
        return if find_edge_group(from, value).is_some() {
            Ok(value.clone())
        } else {
            Err(geometry_error(format!("Geometry.rename_edge_group: missing group {from}")))
        };
    }
    if find_edge_group(into, value).is_some() {
        return Err(geometry_error(format!(
            "Geometry.rename_edge_group: destination already exists: {into}"
        )));
    }
    let Some(item) = find_edge_group(from, value) else {
        return Err(geometry_error(format!("Geometry.rename_edge_group: missing group {from}")));
    };
    let renamed = edge_group::with_name(into, &item)?;
    with_edge_group(renamed, &without_edge_group(from, value))
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    pub fn attributes(value: &T) -> &[attribute::T] {
        value.attributes.as_slice()
    }

    pub fn with_attributes_owned(attributes: Vec<attribute::T>, value: &T) -> Result<T, error::T> {
        validate(
            &value.positions,
            &value.topology,
            &attributes,
            &value.groups,
            &value.edge_groups,
        )
        ?;
        Ok(make(
            value.positions.clone(),
            value.topology.clone(),
            attributes,
            value.groups.as_ref().clone(),
            value.edge_groups.as_ref().clone(),
        ))
    }

    fn merge_attributes(
        existing: &[attribute::T],
        replacements: Vec<attribute::T>,
    ) -> Vec<attribute::T> {
        if replacements.is_empty() {
            return existing.to_vec();
        }
        let mut slots = HashMap::with_capacity(existing.len() + replacements.len());
        for (index, item) in existing.iter().enumerate() {
            slots.insert(
                (attribute_owner_tag(attribute::owner(item)), attribute::name(item).to_owned()),
                index,
            );
        }
        let mut output = existing.to_vec();
        for replacement in replacements {
            let key = (
                attribute_owner_tag(attribute::owner(&replacement)),
                attribute::name(&replacement).to_owned(),
            );
            if let Some(&index) = slots.get(&key) {
                output[index] = replacement;
            } else {
                let index = output.len();
                slots.insert(key, index);
                output.push(replacement);
            }
        }
        output
    }

    fn merge_groups(existing: &[group::T], replacements: Vec<group::T>) -> Vec<group::T> {
        if replacements.is_empty() {
            return existing.to_vec();
        }
        let mut slots = HashMap::with_capacity(existing.len() + replacements.len());
        for (index, item) in existing.iter().enumerate() {
            slots.insert(
                (group_owner_tag(group::owner(item)), group::name(item).to_owned()),
                index,
            );
        }
        let mut output = existing.to_vec();
        for replacement in replacements {
            let key = (
                group_owner_tag(group::owner(&replacement)),
                group::name(&replacement).to_owned(),
            );
            if let Some(&index) = slots.get(&key) {
                output[index] = replacement;
            } else {
                let index = output.len();
                slots.insert(key, index);
                output.push(replacement);
            }
        }
        output
    }

    pub fn with_merged_attributes_owned(
        replacements: Vec<attribute::T>,
        value: &T,
    ) -> Result<T, error::T> {
        if replacements.is_empty() {
            return Ok(value.clone());
        }
        let attributes = merge_attributes(&value.attributes, replacements);
        validate(
            &value.positions,
            &value.topology,
            &attributes,
            &value.groups,
            &value.edge_groups,
        )
        ?;
        Ok(make(
            value.positions.clone(),
            value.topology.clone(),
            attributes,
            value.groups.as_ref().clone(),
            value.edge_groups.as_ref().clone(),
        ))
    }

    pub fn with_merged_owned(
        positions: Option<packed::Float3::T>,
        attributes: Vec<attribute::T>,
        groups: Vec<group::T>,
        value: &T,
    ) -> Result<T, error::T> {
        if positions.is_none() && attributes.is_empty() && groups.is_empty() {
            return Ok(value.clone());
        }
        let positions = positions.unwrap_or_else(|| value.positions.clone());
        let attributes = merge_attributes(&value.attributes, attributes);
        let groups = merge_groups(&value.groups, groups);
        validate(&positions, &value.topology, &attributes, &groups, &value.edge_groups)
            ?;
        Ok(make(
            positions,
            value.topology.clone(),
            attributes,
            groups,
            value.edge_groups.as_ref().clone(),
        ))
    }

    pub fn with_positions_and_attributes_owned(
        positions: packed::Float3::T,
        attributes: Vec<attribute::T>,
        value: &T,
    ) -> Result<T, error::T> {
        validate(
            &positions,
            &value.topology,
            &attributes,
            &value.groups,
            &value.edge_groups,
        )
        ?;
        Ok(make(
            positions,
            value.topology.clone(),
            attributes,
            value.groups.as_ref().clone(),
            value.edge_groups.as_ref().clone(),
        ))
    }
}

pub fn generate_point_ranges(
    count: i64,
    operation: &mut dyn FnMut(i64, i64, &mut [f64], &mut [f64], &mut [f64]),
) -> Result<T, error::T> {
    if count < 0 {
        return Err(geometry_error(
            "Geometry.generate_point_ranges: negative point count".to_owned(),
        ));
    }
    let count = usize::try_from(count)
        .map_err(|_| geometry_error("Geometry.generate_point_ranges: point count is too large".to_owned()))?;
    let mut x = vec![0.0; count];
    let mut y = vec![0.0; count];
    let mut z = vec![0.0; count];
    let grain = 16_384usize;
    // ponytail: serial whole-plane callbacks avoid aliasing; use disjoint slices if generation becomes a bottleneck.
    for first in (0..count).step_by(grain) {
        let last = (first + grain).min(count);
        operation(first as i64, last as i64, &mut x[..], &mut y[..], &mut z[..]);
    }
    let positions = packed::Float3::View::of_owned_exn(x, y, z)
        .map_err(|value| geometry_error(error::message(&value).to_owned()))?;
    let topology_value = topology::empty(packed::Float3::length(&positions))
        .map_err(|value| geometry_error(error::message(&value).to_owned()))?;
    Ok(make(positions, topology_value, Vec::new(), Vec::new(), Vec::new()))
}

#[cfg(test)]
mod tests {
    use super::*;

    // Top-level deferred-storage assertions in lib/rdk/test_attribute_lifecycle.ml.
    #[test]
    fn deferred_positions_and_attribute_metadata_stay_lazy_across_racing_readers() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        let count = 4_i64;
        let reads = Arc::new(AtomicUsize::new(0));
        let noise_reads = reads.clone();
        let noise = attribute::create_deferred(
            None, None, "noise", attribute::Owner::Point, &attribute::Storage::Float(Arc::new(vec![])), count,
            Arc::new(move || {
                noise_reads.fetch_add(1, Ordering::SeqCst);
                attribute::Storage::Float(Arc::new((0..count).map(|i| i as f64).collect()))
            }),
        ).unwrap();
        let position_reads = reads.clone();
        let positions = packed::Float3::deferred(None, None, None, count, Arc::new(move || {
            position_reads.fetch_add(1, Ordering::SeqCst);
            Ok((0..3).map(|component| (0..count).map(|i| (component * 10 + i) as f64).collect()).collect())
        })).unwrap();
        let geometry = create(positions, topology::polygons_owned(count, vec![0, 1, 2, 3], vec![0, 4]).unwrap(), Some(vec![noise.clone()]), None, None).unwrap();
        assert_eq!(attribute::length(&noise), count);
        assert_eq!(attribute::kind_name(&noise), "float");
        assert_eq!(attribute::payload_bytes(&noise), count * 8);
        assert_eq!(packed::Float3::length(super::positions(&geometry)), count);
        assert!(attribute::resident(&noise));
        assert!(packed::Float3::resident(super::positions(&geometry)));
        assert_eq!(reads.load(Ordering::SeqCst), 0);
        let positions = super::positions(&geometry);
        parallel::run_unit(Some(4), &|| parallel::for_(1, 0, 63, &|index| {
            packed::Float3::get(positions, index % count).unwrap();
            Ok(())
        })).unwrap();
        assert_eq!(packed::Float3::get(positions, 1).unwrap(), (1.0, 11.0, 21.0));
        assert_eq!(packed::Float3::get(positions, 2).unwrap(), (2.0, 12.0, 22.0));
        let raced = reads.load(Ordering::SeqCst);
        assert!(raced >= 1);
        packed::Float3::get(positions, 0).unwrap();
        assert_eq!(reads.load(Ordering::SeqCst), raced);
        for _ in 0..2 {
            let attribute::Storage::Float(values) = attribute::storage(&noise).unwrap() else {
                panic!("deferred noise returned non-float storage");
            };
            assert_eq!(*values, vec![0.0, 1.0, 2.0, 3.0]);
        }
        assert_eq!(reads.load(Ordering::SeqCst), raced + 1);
        assert!(!attribute::resident(&noise));
    }

    // lib/rdk/test_rdk_sources.ml: retain the original million-point/domain matrix.
    #[test]
    fn million_point_range_kernel_is_exact_in_each_domain_context() {
        for domains in [1, 4] {
            parallel::run_unit(Some(domains), &|| {
                let value = generate_point_ranges(1_000_000, &mut |first, last, x, y, z| {
                    for index in first..last {
                        let index = index as usize;
                        let value = index as f64;
                        x[index] = value;
                        y[index] = value * 0.5;
                        z[index] = -value;
                    }
                }).unwrap();
                let planes = packed::Float3::View::view(super::positions(&value)).unwrap();
                assert_eq!(planes.x.len(), 1_000_000);
                assert_eq!(planes.y.len(), 1_000_000);
                assert_eq!(planes.z.len(), 1_000_000);
                for index in 0..1_000_000 {
                    let value = index as f64;
                    assert_eq!(planes.x[index].to_bits(), value.to_bits());
                    assert_eq!(planes.y[index].to_bits(), (value * 0.5).to_bits());
                    assert_eq!(planes.z[index].to_bits(), (-value).to_bits());
                }
                Ok(())
            }).unwrap();
        }
    }

    fn positions() -> packed::Float3::T {
        packed::Float3::View::of_owned_exn(
            vec![0.0, 1.0],
            vec![0.0, 0.0],
            vec![0.0, 0.0],
        )
        .unwrap()
    }

    #[test]
    fn snapshots_validate_canonical_positions_and_keep_component_identity() {
        let topology_value = topology::empty(2).unwrap();
        let geometry = create(positions(), topology_value.clone(), None, None, None).unwrap();
        assert_eq!(point_count(&geometry), 2);
        let components = payload_components(&geometry);
        assert_eq!(
            payload_bytes(&geometry),
            components.iter().map(|component| component.1).sum::<i64>()
        );

        let moved = with_positions(positions(), &geometry).unwrap();
        assert_ne!(data_id(&geometry), data_id(&moved));
        assert_eq!(topology::data_id(topology(&geometry)), topology::data_id(topology(&moved)));

        let canonical = attribute::create_owned(
            "P",
            attribute::Owner::Point,
            attribute::Storage::Float(Arc::new(vec![1.0, 2.0])),
        )
        .unwrap();
        let error = create(
            positions(),
            topology_value,
            Some(vec![canonical]),
            None,
            None,
        )
        .err().unwrap();
        assert_eq!(error::code(&error), "invalid_geometry");
    }
    #[test]
    fn point_range_generator_keeps_fixed_plane_lengths() {
        let count = 16_385i64;
        let mut ranges = 0;
        let value = generate_point_ranges(count, &mut |first, last, x, y, z| {
            assert_eq!(x.len(), count as usize);
            assert_eq!(y.len(), count as usize);
            assert_eq!(z.len(), count as usize);
            ranges += 1;
            for index in first..last {
                let index = index as usize;
                x[index] = index as f64;
                y[index] = 2.0 * index as f64;
                z[index] = 3.0 * index as f64;
            }
        })
        .unwrap();
        assert_eq!(ranges, 2);
        assert_eq!(point_count(&value), count);
        let planes = packed::Float3::View::view(super::positions(&value)).unwrap();
        let last = (count - 1) as usize;
        assert_eq!(planes.x[last], last as f64);
        assert_eq!(planes.y[last], 2.0 * last as f64);
        assert_eq!(planes.z[last], 3.0 * last as f64);
    }
}
