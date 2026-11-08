use std::sync::{Arc, Mutex};

use crate::{cancel, error, support};
use crate::error::Code;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    Point,
    Vertex,
    Primitive,
}

#[derive(Clone)]
pub struct T {
    owner: Owner,
    name: String,
    length: i64,
    bits: Arc<[u8]>,
    order: Option<Arc<[i64]>>,
    data_id: i64,
}

pub type Group = T;

fn byte_count(length: i64) -> usize {
    let length = usize::try_from(length).expect("validated nonnegative group length");
    length / 8 + usize::from(length % 8 != 0)
}

fn name_is_blank(name: &str) -> bool {
    name.as_bytes().iter().all(|byte| {
        matches!(*byte, b' ' | b'\t' | b'\n' | b'\r' | 0x0c)
    })
}

fn require_name(operation: &str, name: &str) -> Result<(), error::T> {
    if name_is_blank(name) {
        Err(invalid_group(operation, "Group: empty name"))
    } else {
        Ok(())
    }
}

fn invalid_group(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidGroup, message.to_owned())
}

fn new(owner: Owner, name: String, length: i64, bits: Vec<u8>, order: Option<Vec<i64>>) -> T {
    T {
        owner,
        name,
        length,
        bits: Arc::from(bits),
        order: order.map(Arc::from),
        data_id: support::fresh_data_id(),
    }
}

pub fn name(value: &T) -> &str {
    &value.name
}

pub fn owner(value: &T) -> Owner {
    value.owner
}

pub fn length(value: &T) -> i64 {
    value.length
}

pub fn data_id(value: &T) -> i64 {
    value.data_id
}

pub fn with_name(name: &str, value: &T) -> Result<T, error::T> {
    require_name("Group.with_name", name)?;
    if name == value.name {
        return Ok(value.clone());
    }
    Ok(T {
        owner: value.owner,
        name: name.to_owned(),
        length: value.length,
        bits: Arc::clone(&value.bits),
        order: value.order.as_ref().map(Arc::clone),
        data_id: support::fresh_data_id(),
    })
}

pub fn payload_bytes(value: &T) -> i64 {
    let bytes = value.bits.len() + value.order.as_ref().map_or(0, |order| {
        order.len() * std::mem::size_of::<i64>()
    });
    i64::try_from(bytes).unwrap_or(i64::MAX)
}

pub fn is_ordered(value: &T) -> bool {
    value.order.is_some()
}

pub fn ordered_elements(value: &T) -> Option<Vec<i64>> {
    value.order.as_ref().map(|order| order.to_vec())
}

pub fn mem(index: i64, value: &T) -> bool {
    if index < 0 || index >= value.length {
        return false;
    }
    let index = index as usize;
    value.bits[index >> 3] & (1 << (index & 7)) != 0
}

pub fn cardinality(value: &T) -> i64 {
    value.bits.iter().map(|byte| i64::from(byte.count_ones())).sum()
}

// ponytail: shared byte output uses one mutex; split mutable ranges if store contention matters.
pub fn init(
    grain: Option<i64>,
    owner: Owner,
    name: &str,
    length: i64,
    predicate: &(dyn Fn(i64) -> bool + Send + Sync),
) -> Result<T, error::T> {
    require_name("Group.init", name)?;
    if length < 0 {
        return Err(invalid_group("Group.init", "Group.init: negative length"));
    }
    let grain = grain.unwrap_or(4096);
    if grain <= 0 {
        return Err(invalid_group(
            "Group.init",
            "Group.init: grain must be positive",
        ));
    }
    let count = byte_count(length);
    let bits = Mutex::new(vec![0u8; count]);
    if count > 0 {
        let byte_count_i64 = i64::try_from(count).expect("group byte count exceeds i64");
        let chunk = (grain / 8).max(1);
        let work = |byte: i64| {
            let base = byte * 8;
            let mut bits_byte = 0u8;
            for bit in 0..8 {
                let index = base + bit;
                if index < length && predicate(index) {
                    bits_byte |= 1 << bit;
                }
            }
            bits.lock().unwrap_or_else(std::sync::PoisonError::into_inner)[byte as usize] =
                bits_byte;
            Ok(())
        };
        support::parallel_for(None, chunk, byte_count_i64, &work)?;
    }
    Ok(new(
        owner,
        name.to_owned(),
        length,
        bits.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner),
        None,
    ))
}

pub fn ordered(
    owner: Owner,
    name: &str,
    length: i64,
    elements: &[i64],
) -> Result<T, error::T> {
    if name_is_blank(name) {
        return Err(invalid_group("Group.ordered", "Group: empty name"));
    }
    if length < 0 {
        return Err(invalid_group("Group.ordered", "Group.ordered: negative length"));
    }
    let mut bits = vec![0u8; byte_count(length)];
    for &element in elements {
        if element < 0 || element >= length {
            return Err(invalid_group(
                "Group.ordered",
                "Group.ordered: element index out of bounds",
            ));
        }
        let element = element as usize;
        let byte = element >> 3;
        let mask = 1 << (element & 7);
        if bits[byte] & mask != 0 {
            return Err(invalid_group(
                "Group.ordered",
                "Group.ordered: duplicate element index",
            ));
        }
        bits[byte] |= mask;
    }
    Ok(new(owner, name.to_owned(), length, bits, Some(elements.to_vec())))
}

fn iter_membership(operation: &mut dyn FnMut(i64), value: &T) {
    for index in 0..value.length {
        if mem(index, value) {
            operation(index);
        }
    }
}

fn iter_preferred(operation: &mut dyn FnMut(i64), value: &T) {
    if let Some(order) = &value.order {
        for &element in order.iter() {
            operation(element);
        }
    } else {
        iter_membership(operation, value);
    }
}

pub fn iter(operation: &dyn Fn(i64), value: &T) {
    iter_membership(&mut |e| operation(e), value);
}

pub fn iter_ordered(operation: &dyn Fn(i64), value: &T) {
    iter_preferred(&mut |e| operation(e), value);
}

#[derive(Clone, Copy)]
enum CombineOrder {
    Union,
    Intersection,
    Difference,
    Xor,
}

#[derive(Clone, Copy)]
enum BitOperation {
    Or,
    And,
    Difference,
    Xor,
}

fn apply(operation: BitOperation, left: u8, right: u8) -> u8 {
    match operation {
        BitOperation::Or => left | right,
        BitOperation::And => left & right,
        BitOperation::Difference => left & !right,
        BitOperation::Xor => left ^ right,
    }
}

fn combine(
    operation: &str,
    order_kind: CombineOrder,
    bit_operation: BitOperation,
    left: &T,
    right: &T,
) -> Result<T, error::T> {
    if left.owner != right.owner || left.length != right.length {
        return Err(invalid_group(operation, "Group: owners and lengths must match"));
    }
    let bits = Mutex::new(vec![0u8; left.bits.len()]);
    let count = i64::try_from(left.bits.len()).expect("group byte count exceeds i64");
    let work = |index: i64| {
        let index = index as usize;
        let byte = apply(bit_operation, left.bits[index], right.bits[index]);
        bits.lock().unwrap_or_else(std::sync::PoisonError::into_inner)[index] = byte;
        Ok(())
    };
    support::parallel_for(None, 4096, count, &work)?;
    let bits = bits
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let mut base = new(left.owner, left.name.clone(), left.length, bits, None);
    let order = match order_kind {
        CombineOrder::Difference => left.order.as_ref().map(|left_order| {
            left_order
                .iter()
                .copied()
                .filter(|&element| mem(element, &base))
                .collect::<Vec<_>>()
        }),
        CombineOrder::Intersection => {
            let source = if left.order.is_some() {
                Some(left)
            } else if right.order.is_some() {
                Some(right)
            } else {
                None
            };
            source.map(|source| {
                let mut result = Vec::with_capacity(cardinality(&base) as usize);
                iter_preferred(
                    &mut |element| {
                        if mem(element, &base) {
                            result.push(element);
                        }
                    },
                    source,
                );
                result
            })
        }
        CombineOrder::Union | CombineOrder::Xor => {
            if left.order.is_none() && right.order.is_none() {
                None
            } else {
                let mut result = Vec::with_capacity(cardinality(&base) as usize);
                let mut emitted = vec![0u8; byte_count(base.length)];
                let mut append = |element: i64| {
                    if mem(element, &base) {
                        let element = element as usize;
                        let byte = element >> 3;
                        let mask = 1 << (element & 7);
                        if emitted[byte] & mask == 0 {
                            emitted[byte] |= mask;
                            result.push(element as i64);
                        }
                    }
                };
                iter_preferred(&mut append, left);
                iter_preferred(&mut append, right);
                Some(result)
            }
        }
    };
    base.order = order.map(Arc::from);
    Ok(base)
}

pub fn union(left: &T, right: &T) -> Result<T, error::T> {
    combine("Group.union", CombineOrder::Union, BitOperation::Or, left, right)
}

pub fn union_many(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    name: &str,
    groups: &[T],
) -> Result<T, error::T> {
    require_name("Group.union_many", name)?;
    let grain = grain.unwrap_or(32_768);
    if grain <= 0 {
        return Err(invalid_group(
            "Group.union_many",
            "Group.union_many: grain must be positive",
        ));
    }
    let Some(first) = groups.first() else {
        return Err(invalid_group(
            "Group.union_many",
            "Group.union_many: at least one group is required",
        ));
    };
    if groups
        .iter()
        .skip(1)
        .any(|group| group.owner != first.owner || group.length != first.length)
    {
        return Err(invalid_group(
            "Group.union_many",
            "Group.union_many: owners and lengths must match",
        ));
    }
    if groups.len() == 1 {
        return with_name(name, first);
    }
    let bits = Mutex::new(vec![0u8; first.bits.len()]);
    let count = i64::try_from(first.bits.len()).expect("group byte count exceeds i64");
    let chunk = (grain / 8).max(1);
    let work = |byte: i64| {
        let mut combined = 0;
        for group in groups {
            combined |= group.bits[byte as usize];
        }
        bits.lock().unwrap_or_else(std::sync::PoisonError::into_inner)[byte as usize] = combined;
        Ok(())
    };
    support::parallel_for(cancel_value, chunk, count, &work).map_err(|failure| {
        if error::is_cancelled(&failure) {
            error::cancelled("Group.union_many")
        } else {
            failure
        }
    })?;
    let bits = bits
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    Ok(new(first.owner, name.to_owned(), first.length, bits, None))
}

pub fn intersection(left: &T, right: &T) -> Result<T, error::T> {
    combine(
        "Group.intersection",
        CombineOrder::Intersection,
        BitOperation::And,
        left,
        right,
    )
}

pub fn difference(left: &T, right: &T) -> Result<T, error::T> {
    combine(
        "Group.difference",
        CombineOrder::Difference,
        BitOperation::Difference,
        left,
        right,
    )
}

pub fn symmetric_difference(left: &T, right: &T) -> Result<T, error::T> {
    combine(
        "Group.symmetric_difference",
        CombineOrder::Xor,
        BitOperation::Xor,
        left,
        right,
    )
}

#[allow(non_snake_case)]
pub mod Builder {
    use super::{byte_count, error, new, require_name, Owner};
    use crate::error::Code;

    pub struct T {
        owner: Owner,
        name: String,
        length: i64,
        bits: Vec<u8>,
        frozen: bool,
    }

    pub fn create(owner: Owner, name: &str, length: i64) -> Result<T, error::T> {
        require_name("Group.Builder.create", name)?;
        if length < 0 {
            return Err(error::new(
                "Group.Builder.create",
                Code::InvalidGroup,
                "Group.Builder.create: negative length".to_owned(),
            ));
        }
        Ok(T {
            owner,
            name: name.to_owned(),
            length,
            bits: vec![0u8; byte_count(length)],
            frozen: false,
        })
    }

    pub fn set(value: &mut T, index: i64, member: bool) -> Result<(), error::T> {
        if value.frozen {
            return Err(error::new(
                "Group.Builder.set",
                Code::InvalidGroup,
                "Group.Builder.set: builder is frozen".to_owned(),
            ));
        }
        if index < 0 || index >= value.length {
            return Err(error::new(
                "Group.Builder.set",
                Code::InvalidGroup,
                "Group.Builder.set: invalid index".to_owned(),
            ));
        }
        let index = index as usize;
        let byte = index >> 3;
        let mask = 1 << (index & 7);
        if member {
            value.bits[byte] |= mask;
        } else {
            value.bits[byte] &= !mask;
        }
        Ok(())
    }

    pub fn freeze(value: &mut T) -> Result<Group, error::T> {
        if value.frozen {
            return Err(error::new(
                "Group.Builder.freeze",
                Code::InvalidGroup,
                "Group.Builder.freeze: already frozen".to_owned(),
            ));
        }
        value.frozen = true;
        let bits = std::mem::take(&mut value.bits);
        Ok(new(value.owner, value.name.clone(), value.length, bits, None))
    }

    pub type Group = super::T;
}

#[allow(non_snake_case)]
pub mod View {
    use std::sync::Arc;

    use super::{byte_count, cardinality, error, invalid_group, mem, new, require_name, Owner, T};

    pub fn bits_view(value: &T) -> &[u8] {
        &value.bits
    }

    pub fn with_owner(owner: Owner, value: &T) -> T {
        T {
            owner,
            name: value.name.clone(),
            length: value.length,
            bits: Arc::clone(&value.bits),
            order: value.order.as_ref().map(Arc::clone),
            data_id: value.data_id,
        }
    }

    pub fn of_owned_bits(
        owner: Owner,
        name: &str,
        length: i64,
        bits: Vec<u8>,
    ) -> Result<T, error::T> {
        require_name("Group.View.of_owned_bits", name)?;
        if length < 0 || bits.len() != byte_count(length) {
            return Err(invalid_group(
                "Group.View.of_owned_bits",
                "Group.View.of_owned_bits: invalid packed length",
            ));
        }
        if length & 7 != 0 && !bits.is_empty() {
            let valid_mask = (1u8 << (length & 7)) - 1;
            if bits[bits.len() - 1] & !valid_mask != 0 {
                return Err(invalid_group(
                    "Group.View.of_owned_bits",
                    "Group.View.of_owned_bits: set padding bits",
                ));
            }
        }
        Ok(new(owner, name.to_owned(), length, bits, None))
    }

    pub fn order_view(value: &T) -> Option<&[i64]> {
        value.order.as_deref()
    }

    pub fn with_owned_order(order: Vec<i64>, value: &T) -> Result<T, error::T> {
        if order.len() as i64 != cardinality(value) {
            return Err(invalid_group(
                "Group.View.with_owned_order",
                "Group.View.with_owned_order: order does not match membership",
            ));
        }
        let mut seen = vec![0u8; byte_count(value.length)];
        for &element in &order {
            if element < 0 || element >= value.length || !mem(element, value) {
                return Err(invalid_group(
                    "Group.View.with_owned_order",
                    "Group.View.with_owned_order: invalid element",
                ));
            }
            let element = element as usize;
            let byte = element >> 3;
            let mask = 1 << (element & 7);
            if seen[byte] & mask != 0 {
                return Err(invalid_group(
                    "Group.View.with_owned_order",
                    "Group.View.with_owned_order: duplicate element",
                ));
            }
            seen[byte] |= mask;
        }
        Ok(T {
            owner: value.owner,
            name: value.name.clone(),
            length: value.length,
            bits: Arc::clone(&value.bits),
            order: Some(Arc::from(order)),
            data_id: crate::support::fresh_data_id(),
        })
    }

    pub fn remap_order(
        source: &T,
        source_of_target: &[i64],
        target: &T,
    ) -> Result<T, error::T> {
        let Some(source_order) = source.order.as_ref() else {
            return Ok(target.clone());
        };
        if source.owner != target.owner {
            return Err(invalid_group(
                "Group.View.remap_order",
                "Group.View.remap_order: owners do not match",
            ));
        }
        if source_of_target.len() as i64 != target.length {
            return Err(invalid_group(
                "Group.View.remap_order",
                "Group.View.remap_order: mapping length does not match target",
            ));
        }
        let mut rank = vec![-1i64; source.length as usize];
        for (index, &element) in source_order.iter().enumerate() {
            rank[element as usize] = index as i64;
        }
        let mut counts = vec![0usize; source_order.len()];
        for target_element in 0..target.length {
            let source_element = source_of_target[target_element as usize];
            if source_element < -1 || source_element >= source.length {
                return Err(invalid_group(
                    "Group.View.remap_order",
                    "Group.View.remap_order: source index out of bounds",
                ));
            }
            if mem(target_element, target) && source_element >= 0 {
                let source_rank = rank[source_element as usize];
                if source_rank >= 0 {
                    counts[source_rank as usize] += 1;
                }
            }
        }
        let mut offsets = vec![0usize; counts.len() + 1];
        for index in 0..counts.len() {
            offsets[index + 1] = offsets[index] + counts[index];
        }
        let mapped_count = offsets[counts.len()];
        let mut order = vec![0i64; cardinality(target) as usize];
        let mut next = offsets.clone();
        let mut emitted = vec![0u8; byte_count(target.length)];
        for target_element in 0..target.length {
            let source_element = source_of_target[target_element as usize];
            if mem(target_element, target) && source_element >= 0 {
                let source_rank = rank[source_element as usize];
                if source_rank >= 0 {
                    let source_rank = source_rank as usize;
                    let output = next[source_rank];
                    order[output] = target_element;
                    next[source_rank] += 1;
                    let element = target_element as usize;
                    emitted[element >> 3] |= 1 << (element & 7);
                }
            }
        }
        let mut output = mapped_count;
        for target_element in 0..target.length {
            let element = target_element as usize;
            let mask = 1 << (element & 7);
            if mem(target_element, target) && emitted[element >> 3] & mask == 0 {
                order[output] = target_element;
                output += 1;
            }
        }
        with_owned_order(order, target)
    }

}

#[cfg(test)]
mod tests {
    use crate::{cancel, error};

    use super::{Builder, Owner, View};

    // The only behavioral assertion in lib/rdk/test_point_jitter.ml; its runner never jitters.
    #[test]
    fn point_jitter_source_selection_membership() {
        let selected = super::init(Some(1), Owner::Point, "selected", 4, &|point| point < 3).unwrap();
        assert!(super::mem(0, &selected));
        assert!(super::mem(1, &selected));
        assert!(super::mem(2, &selected));
        assert!(!super::mem(3, &selected));
    }

    #[test]
    fn ordered_union_preserves_order_and_membership() {
        let left = super::ordered(Owner::Point, "left", 5, &[3, 1]).unwrap();
        let right = super::ordered(Owner::Point, "right", 5, &[1, 4]).unwrap();
        let union = super::union(&left, &right).unwrap();
        assert_eq!(super::ordered_elements(&union), Some(vec![3, 1, 4]));
        assert_eq!(super::cardinality(&union), 3);
        assert!(super::mem(4, &union));
        assert!(!super::mem(2, &union));
    }

    #[test]
    fn names_use_ocaml_ascii_whitespace() {
        assert!(super::ordered(Owner::Point, " \t\n\r\u{000c}", 0, &[]).is_err());
        assert!(super::ordered(Owner::Point, "\u{00a0}", 0, &[]).is_ok());
    }

    #[test]
    fn builder_and_owned_order_keep_packed_membership() {
        let mut builder = Builder::create(Owner::Vertex, "corners", 10).unwrap();
        Builder::set(&mut builder, 2, true).unwrap();
        Builder::set(&mut builder, 9, true).unwrap();
        let group = Builder::freeze(&mut builder).unwrap();
        let group = View::with_owned_order(vec![9, 2], &group).unwrap();
        assert_eq!(View::bits_view(&group), &[4, 2]);
        assert_eq!(super::ordered_elements(&group), Some(vec![9, 2]));
    }

    #[test]
    fn union_many_matches_expected_cardinality_and_cancels() {
        let make = |name, divisor| {
            super::init(
                Some(1),
                Owner::Point,
                name,
                100_003,
                &|index| index % divisor == 0,
            )
            .unwrap()
        };
        let groups = [make("a", 2), make("b", 3), make("c", 5)];
        let union = super::union_many(None, Some(257), "combined", &groups).unwrap();
        assert_eq!(super::cardinality(&union), 73_336);
        assert!(!super::is_ordered(&union));
        for domains in [1, 4] {
            parallel::run_unit(Some(domains), &|| {
                let actual = super::union_many(None, Some(257), "combined", &groups).unwrap();
                assert_eq!(View::bits_view(&actual), View::bits_view(&union));
                assert_eq!(super::cardinality(&actual), 73_336);
                assert!(!super::is_ordered(&actual));
                Ok(())
            }).unwrap();
        }

        let cancelled = cancel::create();
        cancel::cancel(&cancelled);
        let result = super::union_many(
            Some(&cancelled),
            Some(1),
            "cancelled",
            &groups[..2],
        );
        match result {
            Err(value) => assert_eq!(error::code(&value), "cancelled"),
            Ok(_) => panic!("group union-many ignored cancellation"),
        }
    }
}
