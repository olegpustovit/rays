use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::{attribute, cancel, edge_group, error, geometry, group, packed, ragged_ops, support, topology, topology_edge_lookup, topology_index};

fn invalid_remap(message: String) -> error::T {
    error::internal("topology_remap", "invalid_remap", message)
}

fn cancelled_error() -> error::T {
    error::internal("topology_remap", "cancelled", "topology remap was cancelled".to_owned())
}
fn run_indexed_ranges(
    grain: i64,
    count: i64,
    work: &(dyn Fn(i64, i64, i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    support::run_indexed_ranges(grain, count, work)
}

fn checked_count(length: usize, operation: &str) -> Result<i64, error::T> {
    i64::try_from(length).map_err(|_| invalid_remap(format!("{operation}: element count overflow")))
}

fn validate_mapping(operation: &str, source_count: i64, mapping: &[i64], allow_missing: bool) -> Result<(), error::T> {
    for &source in mapping {
        if (source < 0 && !(allow_missing && source == -1)) || source >= source_count {
            return Err(invalid_remap(format!("{operation}: source mapping index is out of bounds")));
        }
    }
    Ok(())
}

fn select<T: Clone + Default + Send + Sync>(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    (operation, grain_error): (&str, &str),
    missing: Option<T>,
    mapping: &[i64],
    source: &[T],
) -> Result<Vec<T>, error::T> {
    if mapping.is_empty() {
        return Ok(Vec::new());
    }
    if grain <= 0 {
        return Err(invalid_remap(grain_error.to_owned()));
    }
    cancel::poll(cancel_value, 0, 4095).map_err(|_| cancelled_error())?;
    validate_mapping(operation, checked_count(source.len(), operation)?, mapping, missing.is_some())?;
    let count = checked_count(mapping.len(), operation)?;
    let mut output = vec![T::default(); mapping.len()];
    let windows = support::Windows::even(&mut output, grain);
    run_indexed_ranges(grain, count, &|range, first, last| {
        for (element, slot) in (first..last).zip(windows.take(range)) {
            cancel::poll(cancel_value, element, 4095).map_err(|_| cancelled_error())?;
            let source_index = mapping[element as usize];
            *slot = if source_index < 0 {
                missing.clone().unwrap_or_default()
            } else {
                source[source_index as usize].clone()
            };
        }
        Ok(())
    })?;
    Ok(output)
}

pub fn select_int(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    missing: Option<i64>,
    mapping: &[i64],
    source: &[i64],
) -> Result<Vec<i64>, error::T> {
    select(cancel_value, grain, ("Topology_remap.select", "Support.run_ranges: grain must be positive"), missing, mapping, source)
}

pub fn select_text(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    missing: Option<&[u8]>,
    mapping: &[i64],
    source: &[Vec<u8>],
) -> Result<Vec<Vec<u8>>, error::T> {
    select(cancel_value, grain, ("Topology_remap.select", "Support.run_ranges: grain must be positive"), missing.map(<[u8]>::to_vec), mapping, source)
}

pub fn select_float(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    missing: Option<f64>,
    mapping: &[i64],
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    select(cancel_value, grain, ("Topology_remap.select_float", "Parallel.for_: chunk size must be positive"), missing, mapping, source)
}

fn attribute_result(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    defaults: bool,
    mapping: &[i64],
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let owner = attribute::owner(value);
    let storage = match attribute::View::storage(value)? {
        attribute::Storage::Float(values) => attribute::Storage::Float(Arc::new(select_float(
            cancel_value, grain, defaults.then_some(0.0), mapping, values,
        )?)),
        attribute::Storage::Int(values) => attribute::Storage::Int(Arc::new(select_int(
            cancel_value, grain, defaults.then_some(0), mapping, values,
        )?)),
        attribute::Storage::Text(values) => attribute::Storage::Text(select_text(
            cancel_value, grain, defaults.then_some(&[][..]), mapping, values,
        )?),
        attribute::Storage::IntArray(values) => attribute::Storage::IntArray(ragged_ops::remap_int(
            cancel_value, Some(grain), mapping, values,
        )?),
        attribute::Storage::FloatArray(values) => attribute::Storage::FloatArray(ragged_ops::remap_float(
            cancel_value, Some(grain), mapping, values,
        )?),
        attribute::Storage::Float2(values) => {
            let view = packed::Float2::View::view(values);
            attribute::Storage::Float2(packed::Float2::of_owned(
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.x)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.y)?,
            )?)
        }
        attribute::Storage::Float3(values) => {
            let view = packed::Float3::View::view(values)?;
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.x)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.y)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.z)?,
            )?)
        }
        attribute::Storage::Float4(values) => {
            let view = packed::Float4::View::view(values);
            attribute::Storage::Float4(packed::Float4::of_owned(
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.x)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.y)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.z)?,
                select_float(cancel_value, grain, defaults.then_some(0.0), mapping, &view.w)?,
            )?)
        }
    };
    attribute::create_owned(attribute::name(value), owner, storage)
}

pub fn attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    defaults: Option<bool>,
    mapping: &[i64],
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    attribute_result(cancel_value, grain, defaults.unwrap_or(false), mapping, value)
}

pub fn drop_point_vertex_normals() -> Vec<(attribute::Owner, String)> {
    vec![(attribute::Owner::Point, "N".to_owned()), (attribute::Owner::Vertex, "N".to_owned())]
}

pub fn mapped_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    drop: &[(attribute::Owner, String)],
    value: &attribute::T,
) -> Result<Option<attribute::T>, error::T> {
    let owner = attribute::owner(value);
    if drop.iter().any(|(drop_owner, name)| *drop_owner == owner && name == attribute::name(value)) {
        return Ok(None);
    }
    let mapping = match owner {
        attribute::Owner::Detail => return Ok(Some(value.clone())),
        attribute::Owner::Point => point_map,
        attribute::Owner::Vertex => vertex_map,
        attribute::Owner::Primitive => primitive_map,
    };
    attribute_result(cancel_value, grain, false, mapping, value).map(Some)
}

pub fn group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    value: &group::T,
) -> Result<group::T, error::T> {
    if grain <= 0 {
        return Err(invalid_remap("Topology_remap.group: grain must be positive".to_owned()));
    }
    let count = checked_count(mapping.len(), "Topology_remap.group")?;
    let source_count = group::length(value);
    let invalid_mapping = AtomicBool::new(false);
    let cancelled = AtomicBool::new(false);
    let owner = group::owner(value);
    let name = group::name(value);
    let output = group::init(Some(grain), owner, name, count, &|target| {
        let source = mapping[target as usize];
        if cancel::poll(cancel_value, target, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
            false
        } else if source < -1 || source >= source_count {
            invalid_mapping.store(true, Ordering::Relaxed);
            false
        } else {
            source >= 0 && group::mem(source, value)
        }
    })?;
    if cancelled.load(Ordering::Relaxed) {
        return Err(cancelled_error());
    }
    if invalid_mapping.load(Ordering::Relaxed) {
        return Err(invalid_remap("Topology_remap.group: source mapping index is out of bounds".to_owned()));
    }
    group::View::remap_order(value, mapping, &output)
}

pub fn blend_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    left: &[i64],
    right: &[i64],
    weight: &[f64],
    value: &group::T,
) -> Result<group::T, error::T> {
    let mapping = nearest_rows(left, right, weight)?;
    group(cancel_value, grain, &mapping, value)
}

pub fn mapped_group(
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    value: &group::T,
) -> Result<group::T, error::T> {
    let mapping = match group::owner(value) {
        group::Owner::Point => point_map,
        group::Owner::Vertex => vertex_map,
        group::Owner::Primitive => primitive_map,
    };
    group(None, grain, mapping, value)
}

pub fn attributes(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    value: &geometry::T,
) -> Result<Vec<attribute::T>, error::T> {
    let mut output = Vec::new();
    for item in geometry::attributes(value) {
        let mapping = match attribute::owner(&item) {
            attribute::Owner::Point => point_map,
            attribute::Owner::Vertex => vertex_map,
            attribute::Owner::Primitive => primitive_map,
            attribute::Owner::Detail => {
                output.push(item);
                continue;
            }
        };
        output.push(attribute_result(cancel_value, grain, false, mapping, &item)?);
    }
    Ok(output)
}

pub fn groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    value: &geometry::T,
) -> Result<Vec<group::T>, error::T> {
    let mut output = Vec::new();
    for item in geometry::groups(value) {
        let mapping = match group::owner(&item) {
            group::Owner::Point => point_map,
            group::Owner::Vertex => vertex_map,
            group::Owner::Primitive => primitive_map,
        };
        output.push(group(cancel_value, grain, mapping, &item)?);
    }
    Ok(output)
}

pub fn edge_groups(
    cancel_value: Option<&cancel::T>,
    source_topology: &topology::T,
    target_topology: &topology::T,
    point_map: &[i64],
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let source_index = topology_index::create(cancel_value, source_topology)?;
    let target_index = topology_index::create(cancel_value, target_topology)?;
    let mut output = Vec::with_capacity(groups.len());
    for value in groups {
        output.push(edge_group::remap(
            cancel_value, &source_index, target_topology, &target_index, point_map, value,
        )?);
    }
    Ok(output)
}

pub fn preserving_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    topology_value: &topology::T,
    vertex_map: &[i64],
    primitive_map: &[i64],
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(value);
    if topology::point_count(topology_value) != geometry::point_count(value) {
        return Err(invalid_remap("Topology_remap.preserving_points: point count changed".to_owned()));
    }
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        let remapped = match attribute::owner(&item) {
            attribute::Owner::Point | attribute::Owner::Detail => item,
            attribute::Owner::Vertex => attribute_result(cancel_value, grain, false, vertex_map, &item)?,
            attribute::Owner::Primitive => attribute_result(cancel_value, grain, false, primitive_map, &item)?,
        };
        attributes.push(remapped);
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        let remapped = match group::owner(&item) {
            group::Owner::Point => item,
            group::Owner::Vertex => group(cancel_value, grain, vertex_map, &item)?,
            group::Owner::Primitive => group(cancel_value, grain, primitive_map, &item)?,
        };
        groups.push(remapped);
    }
    let source_edge_groups = geometry::edge_groups(value);
    let remapped_edge_groups = if source_edge_groups.is_empty() {
        Vec::new()
    } else {
        let point_map: Vec<i64> = (0..geometry::point_count(value)).collect();
        edge_groups(cancel_value, source_topology, topology_value, &point_map, &source_edge_groups)?
    };
    geometry::create(
        geometry::positions(value).clone(),
        topology_value.clone(),
        Some(attributes),
        Some(groups),
        Some(remapped_edge_groups),
    )
}

pub fn split_point_edge_groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if grain <= 0 {
        return Err(invalid_remap("Topology_remap.split_point_edge_groups: grain must be positive".to_owned()));
    }
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let source = topology_index::View::view(source_index);
    let target = topology::View::view(target_topology)?;
    if target.vertex_points.len() != source.next_vertex.len() {
        return Err(invalid_remap("split-point edge remap requires unchanged corner cardinality".to_owned()));
    }
    for value in groups {
        if edge_group::topology_data_id(value) != topology_index::topology_data_id(source_index) {
            return Err(invalid_remap(format!(
                "split-point edge group {:?} belongs to a different topology",
                edge_group::name(value)
            )));
        }
    }
    let lookup = topology_edge_lookup::create(cancel_value, &target)?;
    let edge_count = topology_edge_lookup::count(&lookup);
    let mut source_of_target = vec![-1_i64; edge_count as usize];
    let mut failure = None;
    for source_edge in 0..source.edge_a.len() {
        cancel::poll(cancel_value, source_edge as i64, 4095).map_err(|_| cancelled_error())?;
        for at in source.edge_offsets[source_edge]..source.edge_offsets[source_edge + 1] {
            let vertex = source.edge_vertices[at as usize];
            let next = source.next_vertex[vertex as usize];
            if next < 0 {
                failure = Some("source topology exposed an incomplete edge".to_owned());
            } else {
                let target_edge = topology_edge_lookup::find(
                    &lookup,
                    target.vertex_points[vertex as usize],
                    target.vertex_points[next as usize],
                );
                if target_edge < 0 {
                    failure = Some("target topology lost a source corner edge".to_owned());
                } else if source_of_target[target_edge as usize] >= 0
                    && source_of_target[target_edge as usize] != source_edge as i64
                {
                    failure = Some("target split edge has conflicting source ancestry".to_owned());
                } else {
                    source_of_target[target_edge as usize] = source_edge as i64;
                }
            }
        }
    }
    if let Some(message) = failure {
        return Err(invalid_remap(message));
    }
    let mut missing = -1_i64;
    for (edge, &source) in source_of_target.iter().enumerate() {
        if source < 0 {
            missing = edge as i64;
        }
    }
    if missing >= 0 {
        return Err(invalid_remap(format!("target split edge {missing} has no source ancestry")));
    }

    let byte_count = support::ceiling_div(edge_count, 8)?;
    let chunk = (grain / 8).max(1);
    let mut output = Vec::with_capacity(groups.len());
    for value in groups {
        let mut bits = vec![0_u8; byte_count as usize];
        support::par_fill(cancel_value, chunk, &mut bits, &|byte| {
            cancel::poll(cancel_value, byte, 511).map_err(|_| cancelled_error())?;
            let mut packed = 0_u8;
            for bit in 0..8_i64 {
                let edge = byte * 8 + bit;
                if edge < edge_count && edge_group::mem(source_of_target[edge as usize], value) {
                    packed |= 1 << bit;
                }
            }
            Ok(packed)
        })?;
        output.push(edge_group::View::of_owned_bits(target_topology, edge_count, edge_group::name(value), bits)?);
    }
    Ok(output)
}

pub fn edge_groups_from_ancestry(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    target_index: &topology_index::T,
    offsets: &[i64],
    ancestors: &[i64],
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    cancel::check_opt(cancel_value)?;
    let target_edges = topology_index::edge_count(target_index);
    let source_edges = topology_index::edge_count(source_index);
    if grain <= 0 {
        return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: grain must be positive".to_owned()));
    }
    if topology::data_id(target_topology) != topology_index::topology_data_id(target_index) {
        return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: target index affinity mismatch".to_owned()));
    }
    if offsets.len() != target_edges as usize + 1
        || offsets.first() != Some(&0)
        || offsets[target_edges as usize] != ancestors.len() as i64
    {
        return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: invalid offsets".to_owned()));
    }
    for edge in 0..target_edges {
        cancel::poll(cancel_value, edge, 4095).map_err(|_| cancelled_error())?;
        if offsets[edge as usize] < 0 || offsets[edge as usize + 1] < offsets[edge as usize] {
            return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: invalid source ancestry".to_owned()));
        }
    }
    for (at, &ancestor) in ancestors.iter().enumerate() {
        cancel::poll(cancel_value, at as i64, 4095).map_err(|_| cancelled_error())?;
        if ancestor < 0 || ancestor >= source_edges {
            return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: invalid source ancestry".to_owned()));
        }
    }
    if groups.iter().any(|value| {
        edge_group::topology_data_id(value) != topology_index::topology_data_id(source_index)
            || edge_group::length(value) != source_edges
    }) {
        return Err(invalid_remap("Topology_remap.edge_groups_from_ancestry: source group affinity mismatch".to_owned()));
    }
    let byte_count = support::ceiling_div(target_edges, 8)?;
    let mut output = Vec::with_capacity(groups.len());
    for value in groups {
        let mut bits = vec![0_u8; byte_count as usize];
        support::par_fill(cancel_value, grain, &mut bits, &|byte| {
            let mut packed = 0_u8;
            for bit in 0..8_i64 {
                let edge = byte * 8 + bit;
                if edge < target_edges {
                    let mut member = false;
                    let mut at = offsets[edge as usize];
                    while !member && at < offsets[edge as usize + 1] {
                        cancel::poll(cancel_value, at, 4095).map_err(|_| cancelled_error())?;
                        member = edge_group::mem(ancestors[at as usize], value);
                        at += 1;
                    }
                    if member {
                        packed |= 1 << bit;
                    }
                }
            }
            Ok(packed)
        })?;
        output.push(edge_group::View::of_owned_bits(target_topology, target_edges, edge_group::name(value), bits)?);
    }
    Ok(output)
}

fn nearest_rows(left: &[i64], right: &[i64], weight: &[f64]) -> Result<Vec<i64>, error::T> {
    if left.len() != weight.len() || right.len() != weight.len() {
        return Err(invalid_remap("Topology_remap.blend: mapping and weight lengths differ".to_owned()));
    }
    Ok((0..weight.len())
        .map(|index| if weight[index] < 0.5 { left[index] } else { right[index] })
        .collect())
}

fn blend_float(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    left: &[i64],
    right: &[i64],
    weight: &[f64],
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let operation = "Topology_remap.blend";
    if grain <= 0 {
        return Err(invalid_remap(format!("{operation}: grain must be positive")));
    }
    if left.len() != weight.len() || right.len() != weight.len() {
        return Err(invalid_remap(format!("{operation}: mapping and weight lengths differ")));
    }
    validate_mapping(operation, checked_count(source.len(), operation)?, left, false)?;
    validate_mapping(operation, checked_count(source.len(), operation)?, right, false)?;
    let mut output = vec![0.0; weight.len()];
    support::par_fill(cancel_value, grain, &mut output, &|index| {
        cancel::check_opt(cancel_value)?;
        let index = index as usize;
        let a = source[left[index] as usize];
        let b = source[right[index] as usize];
        let delta = b - a;
        Ok(if delta.is_finite() {
            a + delta * weight[index]
        } else {
            a * (1.0 - weight[index]) + b * weight[index]
        })
    })?;
    Ok(output)
}

pub fn blend_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    left: &[i64],
    right: &[i64],
    weight: &[f64],
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let nearest = || nearest_rows(left, right, weight);
    let owner = attribute::owner(value);
    let float = |source: &[f64]| blend_float(cancel_value, grain, left, right, weight, source);
    let storage = match attribute::View::storage(value)? {
        attribute::Storage::Float(source) => attribute::Storage::Float(Arc::new(float(source)?)),
        attribute::Storage::Int(source) => {
            let mapping = nearest()?;
            attribute::Storage::Int(Arc::new(select_int(cancel_value, grain, None, &mapping, source)?))
        }
        attribute::Storage::Text(source) => {
            let mapping = nearest()?;
            attribute::Storage::Text(select_text(cancel_value, grain, None, &mapping, source)?)
        }
        attribute::Storage::IntArray(source) => {
            let mapping = nearest()?;
            attribute::Storage::IntArray(ragged_ops::remap_int(cancel_value, Some(grain), &mapping, source)?)
        }
        attribute::Storage::FloatArray(source) => {
            let mapping = nearest()?;
            attribute::Storage::FloatArray(ragged_ops::remap_float(cancel_value, Some(grain), &mapping, source)?)
        }
        attribute::Storage::Float2(source) => {
            let view = packed::Float2::View::view(source);
            attribute::Storage::Float2(packed::Float2::of_owned(float(&view.x)?, float(&view.y)?)?)
        }
        attribute::Storage::Float3(source) => {
            let view = packed::Float3::View::view(source)?;
            let mut x = float(&view.x)?;
            let mut y = float(&view.y)?;
            let mut z = float(&view.z)?;
            if attribute::name(value) == "N" {
                let chunk = grain as usize;
                let mut first = 0;
                while first < x.len() {
                    cancel::check_opt(cancel_value)?;
                    let last = first.saturating_add(chunk).min(x.len());
                    for index in first..last {
                        let length = (x[index] * x[index] + y[index] * y[index] + z[index] * z[index]).sqrt();
                        if length > 1e-20 {
                            x[index] /= length;
                            y[index] /= length;
                            z[index] /= length;
                        }
                    }
                    first = last;
                }
            }
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(x, y, z)?)
        }
        attribute::Storage::Float4(source) => {
            let view = packed::Float4::View::view(source);
            attribute::Storage::Float4(packed::Float4::of_owned(
                float(&view.x)?, float(&view.y)?, float(&view.z)?, float(&view.w)?,
            )?)
        }
    };
    attribute::create_owned(attribute::name(value), owner, storage)
}

pub fn blend_positions(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    left: &[i64],
    right: &[i64],
    weight: &[f64],
    positions: &packed::Float3::T,
) -> Result<packed::Float3::T, error::T> {
    let view = packed::Float3::View::view(positions)?;
    packed::Float3::View::of_owned_exn(
        blend_float(cancel_value, grain, left, right, weight, &view.x)?,
        blend_float(cancel_value, grain, left, right, weight, &view.y)?,
        blend_float(cancel_value, grain, left, right, weight, &view.z)?,
    )
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn selection_defaults_and_normal_blending() {
        assert_eq!(
            select_float(None, 2, Some(0.0), &[2, -1, 0], &[10.0, 20.0, 30.0]).unwrap(),
            vec![30.0, 0.0, 10.0]
        );

        let storage = attribute::Storage::Float3(
            packed::Float3::View::of_owned_exn(
                vec![1.0, 0.0],
                vec![0.0, 1.0],
                vec![0.0, 0.0],
            )
            .unwrap(),
        );
        let source = attribute::create_owned("N", attribute::Owner::Point, storage).unwrap();
        let blended = blend_attribute(None, 1, &[0], &[1], &[0.5], &source).unwrap();
        let attribute::Storage::Float3(positions) = attribute::View::storage(&blended).unwrap() else {
            panic!("Float3 storage was not preserved");
        };
        let view = packed::Float3::View::view(positions).unwrap();
        let expected = std::f64::consts::FRAC_1_SQRT_2;
        assert!((view.x[0] - expected).abs() < 1e-12);
        assert!((view.y[0] - expected).abs() < 1e-12);
        assert_eq!(view.z[0], 0.0);
    }

    #[test]
    fn shared_edge_group_keeps_exact_ancestry() {
        let topology = topology::polygons_owned(
            6,
            vec![0, 1, 4, 3, 1, 2, 5, 4],
            vec![0, 4, 8],
        )
        .unwrap();
        let index = topology_index::create(None, &topology).unwrap();
        let shared = topology_index::find_edge(&index, 1, 4).unwrap();
        let seam = edge_group::init(None, &topology, &index, "seam", &|edge| edge == shared).unwrap();

        let point_map = [0, 1, 2, 3, 4, 5];
        let remapped = edge_groups(None, &topology, &topology, &point_map, &[seam.clone()]).unwrap();
        assert!(edge_group::mem(shared, &remapped[0]));
        assert_eq!(edge_group::name(&remapped[0]), "seam");

        let edge_count = topology_index::edge_count(&index);
        let offsets: Vec<i64> = (0..=edge_count).collect();
        let ancestors: Vec<i64> = (0..edge_count).collect();
        let remapped = edge_groups_from_ancestry(
            None,
            64,
            &index,
            &topology,
            &index,
            &offsets,
            &ancestors,
            &[seam],
        )
        .unwrap();
        assert!(edge_group::mem(shared, &remapped[0]));
        let boundary = topology_index::find_edge(&index, 0, 1).unwrap();
        assert!(!edge_group::mem(boundary, &remapped[0]));
    }
    #[test]
    fn float_selection_preserves_mapping_and_cancellation_errors() {
        let invalid = select_float(None, 1, None, &[-1], &[1.0]).unwrap_err();
        assert_eq!(error::code(&invalid), "invalid_remap");

        let token = cancel::create();
        cancel::cancel(&token);
        let invalid_grain = select_float(Some(&token), 0, None, &[0], &[1.0]).unwrap_err();
        assert_eq!(error::code(&invalid_grain), "invalid_remap");
        let cancelled = select_float(Some(&token), 1, None, &[-1], &[1.0]).unwrap_err();
        assert_eq!(error::code(&cancelled), "cancelled");
    }

    #[test]
    fn integer_and_text_selection_cancellation_precedes_invalid_mapping() {
        let token = cancel::create();
        cancel::cancel(&token);
        let integer = select_int(Some(&token), 1, None, &[-1], &[1]).unwrap_err();
        assert_eq!(error::code(&integer), "cancelled");
        let text = select_text(Some(&token), 1, None, &[-1], &[b"value".to_vec()]).unwrap_err();
        assert_eq!(error::code(&text), "cancelled");
    }
    #[test]
    fn range_worker_errors_are_not_reclassified_as_cancellation() {
        let worker_error = error::internal("worker", "worker_failure", "range failure".to_owned());
        let result = run_indexed_ranges(1, 1, &|_, _, _| Err(worker_error.clone()));
        let error = result.unwrap_err();
        assert_eq!(error::code(&error), "worker_failure");
        assert_eq!(error::message(&error), "range failure");
    }
}
