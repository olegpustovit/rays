//! Packed immutable BVH over two-dimensional closed bounds.
use rays_math::float::{min as omin, max as omax};
use rdk_core::{cancel, error, support};
use rdk_core::error::Code;

#[derive(Clone, Debug)]
pub struct T {
    order: Vec<i64>,
    item_min_x: Vec<f64>,
    item_min_y: Vec<f64>,
    item_max_x: Vec<f64>,
    item_max_y: Vec<f64>,
    min_x: Vec<f64>,
    min_y: Vec<f64>,
    max_x: Vec<f64>,
    max_y: Vec<f64>,
    left: Vec<i64>,
    right: Vec<i64>,
    first: Vec<i64>,
    count: Vec<i64>,
    nodes: i64,
}

fn invalid(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidArgument, message.to_owned())
}

pub fn midpoint(lower: f64, upper: f64) -> f64 {
    if lower.is_finite() && upper.is_finite() {
        lower * 0.5 + upper * 0.5
    } else if lower == f64::NEG_INFINITY && upper == f64::INFINITY {
        0.
    } else if upper == f64::INFINITY {
        f64::MAX
    } else if lower == f64::NEG_INFINITY {
        -f64::MAX
    } else {
        0.
    }
}

fn build(
    tree: &mut T,
    cancel_value: Option<&cancel::T>,
    cx: &[f64],
    cy: &[f64],
    root_axis: i64,
    depth: i64,
    first: i64,
    count: i64,
) -> Result<i64, error::T> {
    cancel::check_opt(cancel_value)?;
    let node = tree.nodes;
    tree.nodes = node + 1;
    let n = node as usize;
    tree.first[n] = first;
    tree.count[n] = count;
    if count > 8 {
        let axis = (root_axis + depth) & 1;
        let left_count = count / 2;
        let middle = first + left_count;
        support::select(
            cancel_value,
            if axis == 0 { cx } else { cy },
            &mut tree.order,
            first,
            first + count - 1,
            middle,
        )?;
        let left = build(tree, cancel_value, cx, cy, root_axis, depth + 1, first, left_count)?;
        let right = build(
            tree,
            cancel_value,
            cx,
            cy,
            root_axis,
            depth + 1,
            middle,
            count - left_count,
        )?;
        let (l, r) = (left as usize, right as usize);
        tree.left[n] = left;
        tree.right[n] = right;
        tree.min_x[n] = omin(tree.min_x[l], tree.min_x[r]);
        tree.min_y[n] = omin(tree.min_y[l], tree.min_y[r]);
        tree.max_x[n] = omax(tree.max_x[l], tree.max_x[r]);
        tree.max_y[n] = omax(tree.max_y[l], tree.max_y[r]);
    } else {
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut max_y = f64::NEG_INFINITY;
        for slot in first..first + count {
            let item = tree.order[slot as usize] as usize;
            min_x = omin(min_x, tree.item_min_x[item]);
            min_y = omin(min_y, tree.item_min_y[item]);
            max_x = omax(max_x, tree.item_max_x[item]);
            max_y = omax(max_y, tree.item_max_y[item]);
        }
        tree.min_x[n] = min_x;
        tree.min_y[n] = min_y;
        tree.max_x[n] = max_x;
        tree.max_y[n] = max_y;
    }
    Ok(node)
}

pub fn create(
    cancel_value: Option<&cancel::T>,
    item_min_x: &[f64],
    item_min_y: &[f64],
    item_max_x: &[f64],
    item_max_y: &[f64],
) -> Result<T, error::T> {
    let items = item_min_x.len();
    if item_min_y.len() != items || item_max_x.len() != items || item_max_y.len() != items {
        return Err(invalid(
            "Bounds2_index.create",
            "Bounds2_index: bound plane sizes differ",
        ));
    }
    for item in 0..items {
        cancel::poll(cancel_value, item as i64, 16_383)?;
        if item_min_x[item].is_nan()
            || item_min_y[item].is_nan()
            || item_max_x[item].is_nan()
            || item_max_y[item].is_nan()
            || item_min_x[item] > item_max_x[item]
            || item_min_y[item] > item_max_y[item]
        {
            return Err(invalid(
                "Bounds2_index.create",
                "Bounds2_index: malformed bound",
            ));
        }
    }
    let capacity = std::cmp::max(1, items * 2);
    let mut tree = T {
        order: (0..items as i64).collect(),
        item_min_x: item_min_x.to_vec(),
        item_min_y: item_min_y.to_vec(),
        item_max_x: item_max_x.to_vec(),
        item_max_y: item_max_y.to_vec(),
        min_x: vec![0.; capacity],
        min_y: vec![0.; capacity],
        max_x: vec![0.; capacity],
        max_y: vec![0.; capacity],
        left: vec![-1; capacity],
        right: vec![-1; capacity],
        first: vec![0; capacity],
        count: vec![0; capacity],
        nodes: 0,
    };
    let cx: Vec<f64> = (0..items)
        .map(|item| midpoint(item_min_x[item], item_max_x[item]))
        .collect();
    let cy: Vec<f64> = (0..items)
        .map(|item| midpoint(item_min_y[item], item_max_y[item]))
        .collect();
    let mut global_min_x = f64::INFINITY;
    let mut global_min_y = f64::INFINITY;
    let mut global_max_x = f64::NEG_INFINITY;
    let mut global_max_y = f64::NEG_INFINITY;
    for item in 0..items {
        global_min_x = omin(global_min_x, item_min_x[item]);
        global_min_y = omin(global_min_y, item_min_y[item]);
        global_max_x = omax(global_max_x, item_max_x[item]);
        global_max_y = omax(global_max_y, item_max_y[item]);
    }
    let root_axis = if global_max_x - global_min_x >= global_max_y - global_min_y {
        0
    } else {
        1
    };
    if items > 0 {
        build(&mut tree, cancel_value, &cx, &cy, root_axis, 0, 0, items as i64)?;
    }
    Ok(tree)
}

fn node_overlap(tree: &T, node: usize, min_x: f64, min_y: f64, max_x: f64, max_y: f64) -> bool {
    tree.max_x[node] >= min_x
        && max_x >= tree.min_x[node]
        && tree.max_y[node] >= min_y
        && max_y >= tree.min_y[node]
}

fn item_overlap(tree: &T, item: usize, min_x: f64, min_y: f64, max_x: f64, max_y: f64) -> bool {
    tree.item_max_x[item] >= min_x
        && max_x >= tree.item_min_x[item]
        && tree.item_max_y[item] >= min_y
        && max_y >= tree.item_min_y[item]
}

fn query_node(
    tree: &T,
    cancel_value: Option<&cancel::T>,
    visited: &mut i64,
    index: usize,
    bound: [f64; 4],
    visit: &mut dyn FnMut(i64) -> Result<(), error::T>,
) -> Result<(), error::T> {
    let [min_x, min_y, max_x, max_y] = bound;
    *visited += 1;
    cancel::poll(cancel_value, *visited, 4095)?;
    if node_overlap(tree, index, min_x, min_y, max_x, max_y) {
        if tree.left[index] < 0 {
            let last = tree.first[index] + tree.count[index];
            for slot in tree.first[index]..last {
                let item = tree.order[slot as usize];
                if item_overlap(tree, item as usize, min_x, min_y, max_x, max_y) {
                    visit(item)?;
                }
            }
        } else {
            query_node(tree, cancel_value, visited, tree.left[index] as usize, bound, visit)?;
            query_node(tree, cancel_value, visited, tree.right[index] as usize, bound, visit)?;
        }
    }
    Ok(())
}

pub fn query(
    cancel_value: Option<&cancel::T>,
    tree: &T,
    min_x: f64,
    min_y: f64,
    max_x: f64,
    max_y: f64,
    visit: &mut dyn FnMut(i64) -> Result<(), error::T>,
) -> Result<(), error::T> {
    if min_x.is_nan()
        || min_y.is_nan()
        || max_x.is_nan()
        || max_y.is_nan()
        || min_x > max_x
        || min_y > max_y
    {
        return Err(invalid(
            "Bounds2_index.query",
            "Bounds2_index.query: malformed bound",
        ));
    }
    let mut visited = 0;
    if tree.nodes > 0 {
        query_node(tree, cancel_value, &mut visited, 0, [min_x, min_y, max_x, max_y], visit)?;
    }
    Ok(())
}

fn node_pair_overlap(tree: &T, left: usize, right: usize) -> bool {
    tree.max_x[left] >= tree.min_x[right]
        && tree.max_x[right] >= tree.min_x[left]
        && tree.max_y[left] >= tree.min_y[right]
        && tree.max_y[right] >= tree.min_y[left]
}

fn source_pair_overlap(tree: &T, left: usize, right: usize) -> bool {
    tree.item_max_x[left] >= tree.item_min_x[right]
        && tree.item_max_x[right] >= tree.item_min_x[left]
        && tree.item_max_y[left] >= tree.item_min_y[right]
        && tree.item_max_y[right] >= tree.item_min_y[left]
}

fn pairs(
    tree: &T,
    cancel_value: Option<&cancel::T>,
    visited_nodes: &mut i64,
    left: usize,
    right: usize,
    visit: &mut dyn FnMut(i64, i64) -> Result<(), error::T>,
) -> Result<(), error::T> {
    *visited_nodes += 1;
    cancel::poll(cancel_value, *visited_nodes, 4095)?;
    if !node_pair_overlap(tree, left, right) {
        return Ok(());
    }
    if left == right {
        if tree.left[left] < 0 {
            let first = tree.first[left];
            let last = first + tree.count[left];
            for a in first..last {
                for b in a + 1..last {
                    let l = tree.order[a as usize];
                    let r = tree.order[b as usize];
                    if source_pair_overlap(tree, l as usize, r as usize) {
                        visit(l, r)?;
                    }
                }
            }
        } else {
            let (l, r) = (tree.left[left] as usize, tree.right[left] as usize);
            pairs(tree, cancel_value, visited_nodes, l, l, visit)?;
            pairs(tree, cancel_value, visited_nodes, l, r, visit)?;
            pairs(tree, cancel_value, visited_nodes, r, r, visit)?;
        }
    } else if tree.left[left] < 0 && tree.left[right] < 0 {
        let left_last = tree.first[left] + tree.count[left];
        let right_last = tree.first[right] + tree.count[right];
        for a in tree.first[left]..left_last {
            for b in tree.first[right]..right_last {
                let first = tree.order[a as usize];
                let second = tree.order[b as usize];
                if source_pair_overlap(tree, first as usize, second as usize) {
                    if first < second {
                        visit(first, second)?;
                    } else {
                        visit(second, first)?;
                    }
                }
            }
        }
    } else if tree.left[right] < 0
        || (tree.left[left] >= 0 && tree.count[left] >= tree.count[right])
    {
        pairs(tree, cancel_value, visited_nodes, tree.left[left] as usize, right, visit)?;
        pairs(tree, cancel_value, visited_nodes, tree.right[left] as usize, right, visit)?;
    } else {
        pairs(tree, cancel_value, visited_nodes, left, tree.left[right] as usize, visit)?;
        pairs(tree, cancel_value, visited_nodes, left, tree.right[right] as usize, visit)?;
    }
    Ok(())
}

pub fn candidate_pairs(
    cancel_value: Option<&cancel::T>,
    tree: &T,
    visit: &mut dyn FnMut(i64, i64) -> Result<(), error::T>,
) -> Result<(), error::T> {
    if tree.nodes > 0 {
        cancel::check_opt(cancel_value)?;
        let mut visited_nodes = 0;
        pairs(tree, cancel_value, &mut visited_nodes, 0, 0, visit)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pairs_and_queries_match_brute_force() {
        let n = 200usize;
        let mut lo_x = vec![];
        let mut hi_x = vec![];
        let mut lo_y = vec![];
        let mut hi_y = vec![];
        let mut s: u64 = 12345;
        let mut r = || {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) % 1000) as f64 / 10.
        };
        for _ in 0..n {
            let (x, y) = (r(), r());
            lo_x.push(x);
            lo_y.push(y);
            hi_x.push(x + r() / 10.);
            hi_y.push(y + r() / 10.);
        }
        let t = create(None, &lo_x, &lo_y, &hi_x, &hi_y).unwrap();
        let mut got = vec![];
        candidate_pairs(None, &t, &mut |a, b| {
            // Faithful to OCaml: pairs inside one leaf are visited in tree order, so normalize.
            got.push((a.min(b), a.max(b)));
            Ok(())
        })
        .unwrap();
        got.sort();
        let mut want = vec![];
        for a in 0..n {
            for b in a + 1..n {
                if source_pair_overlap(&t, a, b) {
                    want.push((a as i64, b as i64));
                }
            }
        }
        assert_eq!(got, want);
        let mut hit = vec![];
        query(None, &t, 10., 10., 40., 40., &mut |i| {
            hit.push(i);
            Ok(())
        })
        .unwrap();
        hit.sort();
        let w: Vec<i64> = (0..n)
            .filter(|&i| item_overlap(&t, i, 10., 10., 40., 40.))
            .map(|i| i as i64)
            .collect();
        assert_eq!(hit, w);
        assert!(create(None, &[1.], &[0.], &[0.], &[1.]).is_err());
    }
}
