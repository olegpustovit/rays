use std::sync::Arc;

use crate::{cancel, error, topology, topology_index};
use crate::error::Code;

pub struct T {
    point_count: i64,
    edge_a: Arc<[u32]>,
    edge_b: Arc<[u32]>,
    slots: Vec<u32>,
}

fn cancelled_error() -> error::T {
    error::cancelled("Topology_edge_lookup.create")
}

fn invalid_table_error(message: &str) -> error::T {
    error::new(
        "Topology_edge_lookup.create",
        Code::InvalidTopologyEdgeLookup,
        message.to_owned(),
    )
}
pub fn create(
    cancel_value: Option<&cancel::T>,
    source: &topology::view::View,
) -> Result<T, error::T> {
    let primitive_count = source.primitive_kinds.len();
    let mut outgoing_count = 0_usize;
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 4095).map_err(|_| cancelled_error())?;
        let first = source.primitive_offsets[primitive] as usize;
        let last = source.primitive_offsets[primitive + 1] as usize;
        let size = last - first;
        outgoing_count = outgoing_count
            .checked_add(if source.primitive_kinds[primitive] == 1 {
                size.saturating_sub(1)
            } else {
                size
            })
            .ok_or_else(|| invalid_table_error("Topology edge lookup exceeds array limits"))?;
    }
    let requested_slots = outgoing_count
        .checked_mul(2)
        .ok_or_else(|| invalid_table_error("Topology edge lookup exceeds array limits"))?
        .max(8);
    let slot_count = requested_slots.checked_next_power_of_two().ok_or_else(|| {
        invalid_table_error("Topology edge lookup exceeds array limits")
    })?;
    let mut slots = vec![topology_index::NONE; slot_count];
    let mut edge_a = Vec::with_capacity(outgoing_count);
    let mut edge_b = Vec::with_capacity(outgoing_count);
    let mut edge_count = 0_usize;
    let mask = slot_count - 1;

    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 4095).map_err(|_| cancelled_error())?;
        let first = source.primitive_offsets[primitive] as usize;
        let last = source.primitive_offsets[primitive + 1] as usize;
        for vertex in first..last {
            let next = if vertex + 1 < last {
                Some(vertex + 1)
            } else if source.primitive_kinds[primitive] == 1 {
                None
            } else {
                Some(first)
            };
            let Some(next) = next else {
                continue;
            };
            let left = source.vertex_points[vertex];
            let right = source.vertex_points[next];
            let (a, b) = if left <= right { (left, right) } else { (right, left) };
            let mut slot = topology_index::hash_pair(i64::from(a), i64::from(b)) & mask;
            loop {
                let candidate = slots[slot];
                if candidate == topology_index::NONE {
                    slots[slot] = edge_count as u32;
                    edge_a.push(a);
                    edge_b.push(b);
                    edge_count += 1;
                    break;
                }
                let candidate = candidate as usize;
                if edge_a[candidate] == a && edge_b[candidate] == b {
                    break;
                }
                slot = (slot + 1) & mask;
            }
        }
    }

    Ok(T {
        point_count: source.point_count,
        edge_a: Arc::from(edge_a),
        edge_b: Arc::from(edge_b),
        slots,
    })
}

pub fn count(value: &T) -> i64 {
    value.edge_a.len() as i64
}

pub fn endpoints(value: &T, edge: i64) -> Result<(i64, i64), error::T> {
    let edge = usize::try_from(edge)
        .ok()
        .filter(|&edge| edge < value.edge_a.len())
        .ok_or_else(|| {
            error::new(
                "Topology_edge_lookup.endpoints",
                Code::InvalidTopologyEdgeLookup,
                "Topology_edge_lookup.endpoints: index out of bounds".to_owned(),
            )
        })?;
    Ok((i64::from(value.edge_a[edge]), i64::from(value.edge_b[edge])))
}

pub fn find(value: &T, a: impl Into<i64>, b: impl Into<i64>) -> i64 {
    let (a, b) = (a.into(), b.into());
    if a < 0 || b < 0 || a >= value.point_count || b >= value.point_count {
        return -1;
    }
    let (a, b) = if a <= b { (a, b) } else { (b, a) };
    let mask = value.slots.len() - 1;
    let mut slot = topology_index::hash_pair(a, b) & mask;
    loop {
        let edge = value.slots[slot];
        if edge == topology_index::NONE {
            return -1;
        }
        let edge = edge as usize;
        if i64::from(value.edge_a[edge]) == a && i64::from(value.edge_b[edge]) == b {
            return edge as i64;
        }
        slot = (slot + 1) & mask;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::topology::PrimitiveKind;

    #[test]
    fn edge_order_matches_topology_index_and_lookup_is_undirected() {
        let value = topology::create_owned(
            4,
            vec![0, 1, 2, 2, 1, 3],
            vec![0, 3, 6],
            vec![PrimitiveKind::Polygon, PrimitiveKind::Polygon],
        )
        .unwrap();
        let view = topology::view::view(&value).unwrap();
        let lookup = create(None, &view).unwrap();
        let index = topology_index::create(None, &value).unwrap();
        assert_eq!(count(&lookup), topology_index::edge_count(&index));
        for edge in 0..count(&lookup) {
            assert_eq!(
                endpoints(&lookup, edge).unwrap(),
                topology_index::edge_points(&index, edge).unwrap()
            );
        }
        assert_eq!(find(&lookup, 1, 2), 1);
        assert_eq!(find(&lookup, 2, 1), 1);
        assert_eq!(find(&lookup, 7, 1), -1);
        assert!(matches!(
            endpoints(&lookup, 5),
            Err(error) if error::code(&error) == Code::InvalidTopologyEdgeLookup
        ));
    }

    #[test]
    fn open_polyline_has_no_closing_edge() {
        let value = topology::create_owned(
            3,
            vec![0, 1, 2],
            vec![0, 3],
            vec![PrimitiveKind::OpenPolyline],
        )
        .unwrap();
        let lookup = create(None, &topology::view::view(&value).unwrap()).unwrap();
        assert_eq!(count(&lookup), 2);
        assert_eq!(find(&lookup, 0, 2), -1);
    }

    #[test]
    fn cancellation_is_structured() {
        let value = topology::create_owned(
            3,
            vec![0, 1, 2],
            vec![0, 3],
            vec![PrimitiveKind::Polygon],
        )
        .unwrap();
        let token = cancel::create();
        cancel::cancel(&token);
        let result = create(
            Some(&token),
            &topology::view::view(&value).unwrap(),
        );
        assert!(matches!(
            &result,
            Err(error) if crate::error::is_cancelled(error)
        ));
    }
}
