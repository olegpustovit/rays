//! Packed median-split bounds shared by the linear-piece and proximity trees.
use std::sync::Mutex;

use rdk_core::{cancel, error, support};
use rdk_core::error::Code;

/// Per-node data computed by a caller's `extend` callback (the surface index's shared-point
/// fan and diagonal K-DOP slabs).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Extra {
    pub common: [i64; 3],
    pub min_d: [f64; 4],
    pub max_d: [f64; 4],
}

pub const EMPTY_EXTRA: Extra = Extra {
    common: [-1; 3],
    min_d: [0.0; 4],
    max_d: [0.0; 4],
};

pub struct T {
    pub order: Vec<i64>,
    pub min_x: Vec<f64>,
    pub min_y: Vec<f64>,
    pub min_z: Vec<f64>,
    pub max_x: Vec<f64>,
    pub max_y: Vec<f64>,
    pub max_z: Vec<f64>,
    pub left: Vec<i64>,
    pub right: Vec<i64>,
    pub first: Vec<i64>,
    pub count: Vec<i64>,
    /// One entry per node when an `extend` callback was given, otherwise empty.
    pub extra: Vec<Extra>,
}

pub const LEAF_SIZE: i64 = 8;

#[derive(Clone, Copy)]
struct Node {
    min: [f64; 3],
    max: [f64; 3],
    left: i64,
    right: i64,
    first: i64,
    count: i64,
    extra: Extra,
}

fn pool_error(message: String) -> error::T {
    error::new("Bounds3_index.create", Code::Internal, message)
}

/// Builds the subtree for `order` (the range's slice) into `nodes`; `node` is the absolute index of
/// `nodes[0]` and `range_first` the absolute position of `order[0]`.
fn build(
    cancel_value: Option<&cancel::T>,
    arrays: &([&[f64]; 3], [&[f64]; 3], [&[f64]; 3], &[i64], i64),
    extend: Option<&(dyn Fn(&[i64]) -> Extra + Sync)>,
    nodes: &mut [Node],
    order: &mut [i64],
    node: i64,
    range_first: i64,
) -> Result<(), error::T> {
    let (centroid, item_min, item_max, subtree_nodes, grain) = *arrays;
    cancel::check_opt(cancel_value)?;
    let mut min = [f64::INFINITY; 3];
    let mut max = [f64::NEG_INFINITY; 3];
    for &item in order.iter() {
        let item = item as usize;
        for axis in 0..3 {
            if item_min[axis][item] < min[axis] {
                min[axis] = item_min[axis][item];
            }
            if item_max[axis][item] > max[axis] {
                max[axis] = item_max[axis][item];
            }
        }
    }
    let extra = match extend {
        Some(extend) => extend(order),
        None => EMPTY_EXTRA,
    };
    let range_count = order.len() as i64;
    let mut entry = Node {
        min,
        max,
        left: -1,
        right: -1,
        first: 0,
        count: 0,
        extra,
    };
    if range_count <= LEAF_SIZE {
        entry.first = range_first;
        entry.count = range_count;
        nodes[0] = entry;
        return Ok(());
    }
    let ex = max[0] - min[0];
    let ey = max[1] - min[1];
    let ez = max[2] - min[2];
    let axis = if ex >= ey && ex >= ez {
        0
    } else if ey >= ez {
        1
    } else {
        2
    };
    let half = range_count / 2;
    support::select(cancel_value, centroid[axis], order, 0, range_count - 1, half)?;
    let left_nodes = subtree_nodes[half as usize];
    let left_node = node + 1;
    let right_node = node + 1 + left_nodes;
    entry.left = left_node;
    entry.right = right_node;
    nodes[0] = entry;
    let (left_slice, right_slice) = nodes[1..].split_at_mut(left_nodes as usize);
    let (left_order, right_order) = order.split_at_mut(half as usize);
    if half >= grain {
        let left_slot = Mutex::new((left_slice, left_order));
        let right_slot = Mutex::new((right_slice, right_order));
        let failure: Mutex<Option<error::T>> = Mutex::new(None);
        let record = |result: Result<(), error::T>| -> Result<(), String> {
            if let Err(error) = result {
                let mut slot = failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                if slot.is_none() {
                    *slot = Some(error);
                }
            }
            Ok(())
        };
        parallel::both_unit(
            &|| {
                let mut guard = left_slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                let (nodes, order) = &mut *guard;
                record(build(cancel_value, arrays, extend, nodes, order, left_node, range_first))
            },
            &|| {
                let mut guard = right_slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                let (nodes, order) = &mut *guard;
                record(build(
                    cancel_value,
                    arrays,
                    extend,
                    nodes,
                    order,
                    right_node,
                    range_first + half,
                ))
            },
        )
        .map_err(pool_error)?;
        if let Some(error) = failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner).take() {
            return Err(error);
        }
    } else {
        build(cancel_value, arrays, extend, left_slice, left_order, left_node, range_first)?;
        build(cancel_value, arrays, extend, right_slice, right_order, right_node, range_first + half)?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn create(
    cancel_value: Option<&cancel::T>,
    extend: Option<&(dyn Fn(&[i64]) -> Extra + Sync)>,
    grain: i64,
    centroid_x: &[f64],
    centroid_y: &[f64],
    centroid_z: &[f64],
    item_min_x: &[f64],
    item_min_y: &[f64],
    item_min_z: &[f64],
    item_max_x: &[f64],
    item_max_y: &[f64],
    item_max_z: &[f64],
) -> Result<T, error::T> {
    let total = centroid_x.len();
    let mut subtree_nodes = vec![0_i64; total + 1];
    for size in 1..=total {
        subtree_nodes[size] = if size as i64 <= LEAF_SIZE {
            1
        } else {
            let left_size = size / 2;
            1 + subtree_nodes[left_size] + subtree_nodes[size - left_size]
        };
    }
    let capacity = subtree_nodes[total] as usize;
    let blank = Node {
        min: [0.0; 3],
        max: [0.0; 3],
        left: -1,
        right: -1,
        first: 0,
        count: 0,
        extra: EMPTY_EXTRA,
    };
    let mut nodes = vec![blank; capacity];
    let mut order: Vec<i64> = (0..total as i64).collect();
    let arrays = (
        [centroid_x, centroid_y, centroid_z],
        [item_min_x, item_min_y, item_min_z],
        [item_max_x, item_max_y, item_max_z],
        &subtree_nodes[..],
        grain,
    );
    if total > 0 {
        build(cancel_value, &arrays, extend, &mut nodes, &mut order, 0, 0)?;
    }
    Ok(T {
        order,
        min_x: nodes.iter().map(|node| node.min[0]).collect(),
        min_y: nodes.iter().map(|node| node.min[1]).collect(),
        min_z: nodes.iter().map(|node| node.min[2]).collect(),
        max_x: nodes.iter().map(|node| node.max[0]).collect(),
        max_y: nodes.iter().map(|node| node.max[1]).collect(),
        max_z: nodes.iter().map(|node| node.max[2]).collect(),
        left: nodes.iter().map(|node| node.left).collect(),
        right: nodes.iter().map(|node| node.right).collect(),
        first: nodes.iter().map(|node| node.first).collect(),
        count: nodes.iter().map(|node| node.count).collect(),
        extra: if extend.is_some() {
            nodes.iter().map(|node| node.extra).collect()
        } else {
            Vec::new()
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_with(count: usize, grain: i64) -> T {
        let x: Vec<f64> = (0..count).map(|i| ((i * 7919) % 1009) as f64).collect();
        let y: Vec<f64> = (0..count).map(|i| ((i * 104729) % 997) as f64).collect();
        let z: Vec<f64> = (0..count).map(|i| (i % 13) as f64).collect();
        let x1: Vec<f64> = x.iter().map(|v| v + 1.0).collect();
        let y1: Vec<f64> = y.iter().map(|v| v + 1.0).collect();
        let z1: Vec<f64> = z.iter().map(|v| v + 1.0).collect();
        create(None, None, grain, &x, &y, &z, &x, &y, &z, &x1, &y1, &z1).unwrap()
    }

    #[test]
    fn parallel_tree_equals_sequential_tree() {
        let sequential = build_with(5000, i64::MAX);
        let parallel = build_with(5000, 64);
        assert_eq!(sequential.order, parallel.order);
        assert_eq!(sequential.left, parallel.left);
        assert_eq!(sequential.right, parallel.right);
        assert_eq!(sequential.first, parallel.first);
        assert_eq!(sequential.count, parallel.count);
        assert_eq!(sequential.min_x, parallel.min_x);
        assert_eq!(sequential.count.iter().sum::<i64>(), 5000);
        assert!(build_with(0, 4).left.is_empty());
    }
}
