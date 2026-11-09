//! Breadth-first tree scan over a parent array: the shared core of Edge Transport Parent and the
//! Lisp `graph/forest`.
use std::sync::atomic::{AtomicBool, Ordering};

use rdk_core::{error, support};

const GRAIN: i64 = 16_384;

pub struct T {
    /// Normalised parents: a root is its own parent, an unselected point is `-1`.
    pub tree_parent: Vec<i64>,
    pub child_offsets: Vec<i64>,
    /// Children CSR over every non-root selected point, in index order (members of parent cycles
    /// included).
    pub children: Vec<i64>,
    /// First `order_count` entries: roots by increasing index, each followed by its tree breadth
    /// first. Cycle members and unselected points are absent; the array may be longer than
    /// `order_count`.
    pub order: Vec<i64>,
    pub order_count: i64,
    /// Start of each tree in `order`; `root_count + 1` entries.
    pub root_offsets: Vec<i64>,
    pub root_count: i64,
}


pub struct Params {
    pub selected: Option<Box<dyn Fn(i64) -> bool + Send + Sync>>,
    pub selected_count: i64,
}

fn normalize(
    point_count: i64,
    value: &(dyn Fn(i64) -> i64 + Sync),
) -> Result<Vec<i64>, error::T> {
    if point_count <= 0 {
        return Ok(Vec::new());
    }
    let mut output = vec![0; point_count as usize];
    let windows = support::Windows::even(&mut output, GRAIN);
    support::run_indexed_ranges(GRAIN, point_count, &|range, first, last| {
        for (point, slot) in (first..last).zip(windows.take(range)) {
            *slot = value(point);
        }
        Ok(())
    })?;
    Ok(output)
}

/// `selected_count` is the number of selected points, sizing `order`. A parent that is out of range,
/// the point itself or (with `selected`) unselected makes the point a root.
pub fn scan(params: &Params, parents: &[i64]) -> Result<T, error::T> {
    let selected_count = params.selected_count;
    let point_count = parents.len() as i64;
    let tree_parent: Vec<i64> = match params.selected.as_ref() {
        None => {
            let needs_normalization = AtomicBool::new(false);
            if point_count > 0 {
                support::run_indexed_ranges(GRAIN, point_count, &|_, first, last| {
                    for point in first..last {
                        let parent = parents[point as usize];
                        if parent < 0 || parent >= point_count {
                            needs_normalization.store(true, Ordering::Relaxed);
                        }
                    }
                    Ok(())
                })?;
            }
            if !needs_normalization.load(Ordering::Relaxed) {
                parents.to_vec()
            } else {
                normalize(point_count, &|point| {
                    let parent = parents[point as usize];
                    if parent < 0 || parent >= point_count { point } else { parent }
                })?
            }
        }
        Some(selected) => normalize(point_count, &|point| {
            if selected(point) {
                let parent = parents[point as usize];
                if parent < 0 || parent >= point_count || parent == point || !selected(parent) {
                    point
                } else {
                    parent
                }
            } else {
                -1
            }
        })?,
    };
    let pc = point_count as usize;
    let mut child_count = vec![0_i64; pc];
    let mut root_count = 0_i64;
    for point in 0..pc {
        let parent = tree_parent[point];
        if parent == point as i64 {
            root_count += 1;
        } else if parent >= 0 {
            child_count[parent as usize] += 1;
        }
    }
    let mut child_offsets = vec![0_i64; pc + 1];
    for point in 0..pc {
        child_offsets[point + 1] = child_offsets[point] + child_count[point];
    }
    let mut children = vec![0_i64; child_offsets[pc] as usize];
    child_count.fill(0);
    for point in 0..pc {
        let parent = tree_parent[point];
        if parent >= 0 && parent != point as i64 {
            let target = child_offsets[parent as usize] + child_count[parent as usize];
            children[target as usize] = point as i64;
            child_count[parent as usize] += 1;
        }
    }
    let mut order = vec![0_i64; selected_count.max(0) as usize];
    let mut root_offsets = vec![0_i64; root_count as usize + 1];
    let mut order_count = 0_usize;
    let mut root_index = 0_usize;
    for root in 0..pc {
        if tree_parent[root] == root as i64 {
            let first = order_count;
            root_offsets[root_index] = first as i64;
            order[order_count] = root as i64;
            order_count += 1;
            let mut read = first;
            while read < order_count {
                let point = order[read] as usize;
                for at in child_offsets[point]..child_offsets[point + 1] {
                    order[order_count] = children[at as usize];
                    order_count += 1;
                }
                read += 1;
            }
            root_index += 1;
            root_offsets[root_index] = order_count as i64;
        }
    }
    Ok(T {
        tree_parent,
        child_offsets,
        children,
        order,
        order_count: order_count as i64,
        root_offsets,
        root_count,
    })
}

/// `(depth, root)` per point: hops from the tree root and the root's point index; both `-1` for
/// points outside every tree.
pub fn labels(value: &T) -> (Vec<i64>, Vec<i64>) {
    let point_count = value.tree_parent.len();
    let mut depth = vec![-1_i64; point_count];
    let mut root = vec![-1_i64; point_count];
    for tree in 0..value.root_count as usize {
        for at in value.root_offsets[tree]..value.root_offsets[tree + 1] {
            let point = value.order[at as usize] as usize;
            let parent = value.tree_parent[point];
            if parent == point as i64 {
                depth[point] = 0;
                root[point] = point as i64;
            } else {
                depth[point] = depth[parent as usize] + 1;
                root[point] = root[parent as usize];
            }
        }
    }
    (depth, root)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_orders_trees_breadth_first_and_normalises_parents() {
        // 0 root; 1,2 children of 0; 3 child of 1; 4 -> 4 (root); 5 has an out-of-range parent;
        // 6 and 7 form a cycle and are in no tree.
        let parents = [0, 0, 0, 1, 4, 99, 7, 6];
        let forest = scan(&Params { selected: None, selected_count: 8 }, &parents).unwrap();
        assert_eq!(forest.tree_parent, vec![0, 0, 0, 1, 4, 5, 7, 6]);
        assert_eq!(forest.root_count, 3);
        assert_eq!(&forest.order[..forest.order_count as usize], &[0, 1, 2, 3, 4, 5]);
        assert_eq!(forest.root_offsets, vec![0, 4, 5, 6]);
        assert_eq!(forest.child_offsets, vec![0, 2, 3, 3, 3, 3, 3, 4, 5]);
        let (depth, root) = labels(&forest);
        assert_eq!(depth, vec![0, 1, 1, 2, 0, 0, -1, -1]);
        assert_eq!(root, vec![0, 0, 0, 0, 4, 5, -1, -1]);
    }

    #[test]
    fn selection_makes_unselected_parents_roots_and_unselected_points_absent() {
        let parents = [1, 1, 1, 0];
        let params = Params { selected: Some(Box::new(|point| point != 1)), selected_count: 3 };
        let forest = scan(&params, &parents).unwrap();
        assert_eq!(forest.tree_parent, vec![0, -1, 2, 0]);
        assert_eq!(&forest.order[..forest.order_count as usize], &[0, 3, 2]);
        let (depth, _) = labels(&forest);
        assert_eq!(depth, vec![0, -1, 0, 1]);
    }
}
