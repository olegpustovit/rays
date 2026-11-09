use std::sync::{Arc, Mutex};

use crate::{cancel, error, support, topology, topology_index};
use crate::error::Code;

pub struct T {
    topology_id: i64,
    name: String,
    length: i64,
    bits: Arc<[u8]>,
    data_id: i64,
}
pub type EdgeGroup = T;


impl Clone for T {
    fn clone(&self) -> Self {
        Self {
            topology_id: self.topology_id,
            name: self.name.clone(),
            length: self.length,
            bits: Arc::clone(&self.bits),
            data_id: self.data_id,
        }
    }
}

fn byte_count(length: i64) -> usize {
    let length = usize::try_from(length).expect("validated nonnegative edge count");
    length / 8 + usize::from(length % 8 != 0)
}

fn require_name(operation: &str, name: &str) -> Result<(), error::T> {
    if support::ocaml_trim_is_empty(name) {
        Err(invalid_edge_group(operation, "Edge_group: empty name"))
    } else {
        Ok(())
    }
}
fn invalid_edge_group(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidEdgeGroup, message.to_owned())
}

fn require_index(
    operation: &str,
    topology: &topology::T,
    index: &topology_index::T,
) -> Result<(), error::T> {
    if topology::data_id(topology) != topology_index::topology_data_id(index) {
        return Err(invalid_edge_group(
            operation,
            "Edge_group: topology index belongs to a different topology",
        ));
    }
    Ok(())
}

fn new(topology_id: i64, name: String, length: i64, bits: Vec<u8>) -> T {
    T {
        topology_id,
        name,
        length,
        bits: Arc::from(bits),
        data_id: support::fresh_data_id(),
    }
}

pub fn name(value: &T) -> &str {
    &value.name
}

pub fn topology_data_id(value: &T) -> i64 {
    value.topology_id
}

pub fn length(value: &T) -> i64 {
    value.length
}

pub fn data_id(value: &T) -> i64 {
    value.data_id
}

pub fn with_name(name: &str, value: &T) -> Result<T, error::T> {
    require_name("Edge_group.with_name", name)?;
    if name == value.name {
        return Ok(value.clone());
    }
    Ok(T {
        topology_id: value.topology_id,
        name: name.to_owned(),
        length: value.length,
        bits: Arc::clone(&value.bits),
        data_id: support::fresh_data_id(),
    })
}

pub fn payload_bytes(value: &T) -> i64 {
    i64::try_from(value.bits.len()).unwrap_or(i64::MAX)
}

pub fn mem(edge: i64, value: &T) -> bool {
    if edge < 0 || edge >= value.length {
        return false;
    }
    let edge = edge as usize;
    value.bits[edge >> 3] & (1 << (edge & 7)) != 0
}

// ponytail: shared byte output uses one mutex; split mutable ranges if store contention matters.
pub fn init(
    grain: Option<i64>,
    topology_value: &topology::T,
    index: &topology_index::T,
    name: &str,
    predicate: &(dyn Fn(i64) -> bool + Send + Sync),
) -> Result<T, error::T> {
    require_name("Edge_group.init", name)?;
    require_index("Edge_group.init", topology_value, index)?;
    let grain = support::grain("Edge_group.init", grain.or(Some(4096)))?;
    let length = topology_index::edge_count(index);
    let count = byte_count(length);
    let bits = Mutex::new(vec![0u8; count]);
    if count > 0 {
        let count_i64 = i64::try_from(count).expect("edge group byte count exceeds i64");
        let chunk = (grain / 8).max(1);
        let work = |byte: i64| {
            let base = byte * 8;
            let mut value = 0u8;
            for bit in 0..8 {
                let edge = base + bit;
                if edge < length && predicate(edge) {
                    value |= 1 << bit;
                }
            }
            bits.lock().unwrap_or_else(std::sync::PoisonError::into_inner)[byte as usize] = value;
            Ok(())
        };
        support::parallel_for(None, chunk, count_i64, &work)?;
    }
    Ok(new(
        topology::data_id(topology_value),
        name.to_owned(),
        length,
        bits.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner),
    ))
}

pub fn cardinality(value: &T) -> i64 {
    value.bits.iter().map(|byte| i64::from(byte.count_ones())).sum()
}

#[derive(Clone, Copy)]
enum Operation {
    Union,
    Intersection,
    Difference,
    Xor,
}

fn apply(operation: Operation, left: u8, right: u8) -> u8 {
    match operation {
        Operation::Union => left | right,
        Operation::Intersection => left & right,
        Operation::Difference => left & !right,
        Operation::Xor => left ^ right,
    }
}

fn combine(operation_name: &str, operation: Operation, left: &T, right: &T) -> Result<T, error::T> {
    if left.topology_id != right.topology_id || left.length != right.length {
        return Err(invalid_edge_group(
            operation_name,
            "Edge_group: topology identities and lengths must match",
        ));
    }
    let bits = Mutex::new(vec![0u8; left.bits.len()]);
    let count = i64::try_from(left.bits.len()).expect("edge group byte count exceeds i64");
    let work = |index: i64| {
        let index = index as usize;
        let byte = apply(operation, left.bits[index], right.bits[index]);
        bits.lock().unwrap_or_else(std::sync::PoisonError::into_inner)[index] = byte;
        Ok(())
    };
    support::parallel_for(None, 4096, count, &work)?;
    let bits = bits
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    Ok(new(left.topology_id, left.name.clone(), left.length, bits))
}

/// Union into the first matching group, preserving table and member order.
pub fn merge(item: T, mut groups: Vec<T>) -> Result<Vec<T>, error::T> {
    for existing in &mut groups {
        if name(existing) == name(&item) {
            *existing = union(existing, &item)?;
            return Ok(groups);
        }
    }
    groups.push(item);
    Ok(groups)
}

pub fn union(left: &T, right: &T) -> Result<T, error::T> {
    combine("Edge_group.union", Operation::Union, left, right)
}

pub fn intersection(left: &T, right: &T) -> Result<T, error::T> {
    combine("Edge_group.intersection", Operation::Intersection, left, right)
}

pub fn difference(left: &T, right: &T) -> Result<T, error::T> {
    combine("Edge_group.difference", Operation::Difference, left, right)
}

pub fn symmetric_difference(left: &T, right: &T) -> Result<T, error::T> {
    combine("Edge_group.symmetric_difference", Operation::Xor, left, right)
}

pub fn iter(operation: &dyn Fn(i64), value: &T) {
    for edge in 0..value.length {
        if mem(edge, value) {
            operation(edge);
        }
    }
}

pub mod builder {
    use super::{
        byte_count, invalid_edge_group, new, require_index, require_name, topology, topology_index,
    };
    use crate::error;

    pub struct T {
        topology_id: i64,
        name: String,
        length: i64,
        bits: Vec<u8>,
        frozen: bool,
    }

    pub fn create(
        topology_value: &topology::T,
        index: &topology_index::T,
        name: &str,
    ) -> Result<T, error::T> {
        require_name("Edge_group.Builder.create", name)?;
        require_index("Edge_group.Builder.create", topology_value, index)?;
        let length = topology_index::edge_count(index);
        Ok(T {
            topology_id: topology::data_id(topology_value),
            name: name.to_owned(),
            length,
            bits: vec![0u8; byte_count(length)],
            frozen: false,
        })
    }

    pub fn set(value: &mut T, edge: i64, member: bool) -> Result<(), error::T> {
        if value.frozen {
            return Err(invalid_edge_group(
                "Edge_group.Builder.set",
                "Edge_group.Builder.set: builder is frozen",
            ));
        }
        if edge < 0 || edge >= value.length {
            return Err(invalid_edge_group(
                "Edge_group.Builder.set",
                "Edge_group.Builder.set: invalid edge",
            ));
        }
        let edge = edge as usize;
        let byte = edge >> 3;
        let mask = 1 << (edge & 7);
        if member {
            value.bits[byte] |= mask;
        } else {
            value.bits[byte] &= !mask;
        }
        Ok(())
    }

    pub fn freeze(value: &mut T) -> Result<super::T, error::T> {
        if value.frozen {
            return Err(invalid_edge_group(
                "Edge_group.Builder.freeze",
                "Edge_group.Builder.freeze: already frozen",
            ));
        }
        value.frozen = true;
        let bits = std::mem::take(&mut value.bits);
        Ok(new(value.topology_id, value.name.clone(), value.length, bits))
    }
}

pub mod view {
    use super::{byte_count, invalid_edge_group, new, require_name, topology, T};
    use crate::error;

    pub fn of_owned_bits(
        topology_value: &topology::T,
        edge_count: i64,
        name: &str,
        bits: Vec<u8>,
    ) -> Result<T, error::T> {
        require_name("Edge_group.View.of_owned_bits", name)?;
        if edge_count < 0 || bits.len() != byte_count(edge_count) {
            return Err(invalid_edge_group(
                "Edge_group.View.of_owned_bits",
                "Edge_group.View.of_owned_bits: invalid packed length",
            ));
        }
        if edge_count & 7 != 0 && !bits.is_empty() {
            let valid_mask = (1u8 << (edge_count & 7)) - 1;
            if bits[bits.len() - 1] & !valid_mask != 0 {
                return Err(invalid_edge_group(
                    "Edge_group.View.of_owned_bits",
                    "Edge_group.View.of_owned_bits: set padding bits",
                ));
            }
        }
        Ok(new(
            topology::data_id(topology_value),
            name.to_owned(),
            edge_count,
            bits,
        ))
    }
}

fn cancelled_error(operation: &str) -> error::T {
    error::cancelled(operation)
}

pub fn remap(
    cancel_value: Option<&cancel::T>,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    target_index: &topology_index::T,
    point_map: &[i64],
    value: &T,
) -> Result<T, error::T> {
    if value.topology_id != topology_index::topology_data_id(source_index) {
        return Err(invalid_edge_group(
            "Edge_group.remap",
            "Edge_group.remap: source index does not match the group topology",
        ));
    }
    if topology::data_id(target_topology) != topology_index::topology_data_id(target_index) {
        return Err(invalid_edge_group(
            "Edge_group.remap",
            "Edge_group.remap: target index does not match target topology",
        ));
    }
    if i64::try_from(point_map.len()).unwrap_or(i64::MAX)
        != topology_index::point_count(source_index)
    {
        return Err(invalid_edge_group(
            "Edge_group.remap",
            "Edge_group.remap: point map length does not match source topology",
        ));
    }
    let mut builder = builder::create(target_topology, target_index, &value.name)?;
    for edge in 0..value.length {
        cancel::poll(cancel_value, edge, 16_383)
            .map_err(|_| cancelled_error("Edge_group.remap"))?;
        if mem(edge, value) {
            let (a, b) = topology_index::edge_points(source_index, edge)?;
            let a = point_map[a as usize];
            let b = point_map[b as usize];
            if a >= 0 && b >= 0 {
                if let Some(target_edge) = topology_index::find_edge(target_index, a, b) {
                    builder::set(&mut builder, target_edge, true)?;
                }
            }
        }
    }
    builder::freeze(&mut builder)
}

pub fn replicate_exact_copies(
    cancel_value: Option<&cancel::T>,
    source_topology: &topology::T,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    copies: i64,
    value: &T,
) -> Result<T, error::T> {
    let operation = "Edge_group.replicate_exact_copies";
    if copies < 0 {
        return Err(invalid_edge_group(operation, "Edge_group.replicate_exact_copies: negative copy count"));
    }
    if topology::data_id(source_topology) != topology_index::topology_data_id(source_index)
        || value.topology_id != topology::data_id(source_topology)
    {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: source topology/index/group mismatch",
        ));
    }
    let source = topology::view::view(source_topology)?;
    let target = topology::view::view(target_topology)?;
    let source_vertices = i64::try_from(source.vertex_points.len()).unwrap_or(i64::MAX);
    let source_primitives = i64::try_from(source.primitive_kinds.len()).unwrap_or(i64::MAX);
    let Some(expected_points) = source.point_count.checked_mul(copies) else {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: output cardinality overflow",
        ));
    };
    let Some(expected_vertices) = source_vertices.checked_mul(copies) else {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: output cardinality overflow",
        ));
    };
    let Some(expected_primitives) = source_primitives.checked_mul(copies) else {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: output cardinality overflow",
        ));
    };
    if target.point_count != expected_points
        || i64::try_from(target.vertex_points.len()).unwrap_or(i64::MAX) != expected_vertices
        || i64::try_from(target.primitive_kinds.len()).unwrap_or(i64::MAX) != expected_primitives
    {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: target cardinality mismatch",
        ));
    }
    let expected_offsets = expected_primitives.checked_add(1).ok_or_else(|| {
        invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: output cardinality overflow",
        )
    })?;
    if i64::try_from(target.primitive_offsets.len()).unwrap_or(i64::MAX) != expected_offsets {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: target cardinality mismatch",
        ));
    }
    let mut valid = true;
    for copy in 0..copies {
        cancel::poll(cancel_value, copy, 255).map_err(|_| cancelled_error(operation))?;
        let point_offset = copy * source.point_count;
        let vertex_offset = copy * source_vertices;
        let primitive_offset = copy * source_primitives;
        for vertex in 0..source_vertices {
            let target_vertex = vertex_offset + vertex;
            let expected_point = source.vertex_points[vertex as usize] + point_offset;
            if target.vertex_points[target_vertex as usize] != expected_point {
                valid = false;
                break;
            }
        }
        if !valid {
            break;
        }
        for primitive in 0..source_primitives {
            let target_primitive = primitive_offset + primitive;
            if target.primitive_offsets[target_primitive as usize]
                != source.primitive_offsets[primitive as usize] + vertex_offset
                || target.primitive_kinds[target_primitive as usize]
                    != source.primitive_kinds[primitive as usize]
            {
                valid = false;
                break;
            }
        }
        if !valid {
            break;
        }
    }
    if valid {
        valid = target.primitive_offsets[expected_primitives as usize] == expected_vertices;
    }
    if !valid {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: target is not exact copy-major topology",
        ));
    }
    let source_edges = topology_index::edge_count(source_index);
    if value.length != source_edges {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: source edge count mismatch",
        ));
    }
    let Some(length) = source_edges.checked_mul(copies) else {
        return Err(invalid_edge_group(
            operation,
            "Edge_group.replicate_exact_copies: edge cardinality overflow",
        ));
    };
    let mut bits = vec![0u8; byte_count(length)];
    for edge in 0..source_edges {
        cancel::poll(cancel_value, edge, 16_383).map_err(|_| cancelled_error(operation))?;
        if mem(edge, value) {
            for copy in 0..copies {
                let target_edge = (copy * source_edges + edge) as usize;
                bits[target_edge >> 3] |= 1 << (target_edge & 7);
            }
        }
    }
    Ok(new(
        topology::data_id(target_topology),
        value.name.clone(),
        length,
        bits,
    ))
}

#[cfg(test)]
mod tests {
    use crate::{topology, topology_index};

    use super::with_name;

    #[test]
    fn merge_preserves_slots_and_rejects_foreign_topology() {
        let topology = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let index = topology_index::create(None, &topology).unwrap();
        let make = |name, selected| super::init(None, &topology, &index, name, &|edge| edge == selected).unwrap();
        let merged = super::merge(make("selected", 1), vec![make("other", 2), make("selected", 0), make("selected", 2)]).unwrap();
        assert_eq!(super::name(&merged[0]), "other");
        assert!(super::mem(0, &merged[1]) && super::mem(1, &merged[1]));
        assert_eq!(super::cardinality(&merged[1]), 2);
        assert_eq!(super::cardinality(&merged[2]), 1);
        let foreign = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let foreign_index = topology_index::create(None, &foreign).unwrap();
        let item = super::init(None, &foreign, &foreign_index, "selected", &|_| false).unwrap();
        let expected = super::union(&merged[1], &item).err().unwrap();
        let actual = super::merge(item, merged).err().unwrap();
        assert_eq!(crate::error::code(&actual), crate::error::code(&expected));
        assert_eq!(crate::error::message(&actual), crate::error::message(&expected));
    }

    #[test]
    fn edge_membership_is_topology_affine_and_rename_shares_storage() {
        let topology = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let index = topology_index::create(None, &topology).unwrap();
        assert!(super::init(None, &topology, &index, " \t\n\r\u{000c}", &|_| false).is_err());
        let group =
            super::init(None, &topology, &index, "boundary", &|edge| edge == 0).unwrap();
        assert_eq!(super::cardinality(&group), 1);
        assert!(super::mem(0, &group));
        assert!(!super::mem(1, &group));
        let renamed = with_name("\u{00a0}", &group).unwrap();
        assert_eq!(super::topology_data_id(&renamed), topology::data_id(&topology));
        assert_ne!(super::data_id(&renamed), super::data_id(&group));
        assert_eq!(super::payload_bytes(&renamed), super::payload_bytes(&group));
    }
}
