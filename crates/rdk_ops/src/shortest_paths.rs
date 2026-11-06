//! Shortest paths over the unique-edge graph of a `topology_index`: heap-ordered single-source-set
//! search over point/edge adjacency. Ties on distance are broken by the lower point index, so the
//! pop order, the distances and the parents are a pure function of the inputs.
use std::sync::Arc;

use rdk_core::{cancel, error};

/// An indexed decrease-key heap that admits each point at most once. `positions` is -1 before
/// insertion, a heap slot while queued, and -2 after pop. Points order by distance, then (with
/// `roots`) by tree root, then by point index.
struct Heap {
    points: Vec<i64>,
    positions: Vec<i64>,
    size: i64,
}

impl Heap {
    fn new(count: usize) -> Heap {
        Heap { points: vec![0; count], positions: vec![-1; count], size: 0 }
    }

    fn assign(&mut self, slot: i64, point: i64) {
        self.points[slot as usize] = point;
        self.positions[point as usize] = slot;
    }

    fn less(distance: &[f64], roots: Option<&[i64]>, left: i64, right: i64) -> bool {
        let (l, r) = (left as usize, right as usize);
        if distance[l] < distance[r] {
            return true;
        }
        if distance[l] != distance[r] {
            return false;
        }
        match roots {
            None => left < right,
            Some(root) => root[l] < root[r] || (root[l] == root[r] && left < right),
        }
    }

    fn bubble_up(&mut self, start: i64, point: i64, distance: &[f64], roots: Option<&[i64]>) {
        let mut slot = start;
        loop {
            if slot == 0 {
                self.assign(0, point);
                return;
            }
            let parent = (slot - 1) / 2;
            let parent_point = self.points[parent as usize];
            if Heap::less(distance, roots, point, parent_point) {
                self.assign(slot, parent_point);
                slot = parent;
            } else {
                self.assign(slot, point);
                return;
            }
        }
    }

    fn enqueue(&mut self, point: i64, distance: &[f64], roots: Option<&[i64]>) {
        let slot = self.positions[point as usize];
        if slot == -1 {
            let slot = self.size;
            self.size += 1;
            self.bubble_up(slot, point, distance, roots);
        } else if slot >= 0 {
            self.bubble_up(slot, point, distance, roots);
        }
    }

    fn sift_down(&mut self, start: i64, point: i64, distance: &[f64], roots: Option<&[i64]>) {
        let mut slot = start;
        loop {
            let left = slot * 2 + 1;
            if left >= self.size {
                self.assign(slot, point);
                return;
            }
            let right = left + 1;
            let child = if right < self.size
                && Heap::less(distance, roots, self.points[right as usize], self.points[left as usize])
            {
                right
            } else {
                left
            };
            let child_point = self.points[child as usize];
            if Heap::less(distance, roots, child_point, point) {
                self.assign(slot, child_point);
                slot = child;
            } else {
                self.assign(slot, point);
                return;
            }
        }
    }

    fn pop(&mut self, distance: &[f64], roots: Option<&[i64]>) -> i64 {
        let point = self.points[0];
        self.positions[point as usize] = -2;
        self.size -= 1;
        if self.size > 0 {
            let last = self.points[self.size as usize];
            self.sift_down(0, last, distance, roots);
        }
        point
    }
}

pub struct SearchParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub radius: f64,
    pub point_edge_offsets: Arc<[i64]>,
    pub point_edges: Arc<[i64]>,
    pub edge_a: Arc<[i64]>,
    pub edge_b: Arc<[i64]>,
    pub lengths: Arc<[f64]>,
}

/// Returns `(distances, parents)`, one entry per point. `lengths` holds one non-negative length per
/// edge. Roots have distance `0.` and parent `-1`; points not reached within `radius` keep
/// `infinity` and parent `-1`. Equal distances are settled by the lower point index, and a point's
/// parent is the first settled neighbour that gave its final distance. Roots must be valid point
/// indices.
pub fn search(params: &SearchParams, roots: &[i64]) -> Result<(Vec<f64>, Vec<i64>), error::T> {
    let cancel_value = params.cancel.as_deref();
    let count = params.point_edge_offsets.len() - 1;
    let mut distances = vec![f64::INFINITY; count];
    let mut parents = vec![-1_i64; count];
    let mut heap = Heap::new(count);
    for (index, &point) in roots.iter().enumerate() {
        cancel::poll(cancel_value, index as i64, 16_383)?;
        distances[point as usize] = 0.0;
        heap.enqueue(point, &distances, None);
    }
    let mut visits = 0_i64;
    while heap.size > 0 {
        cancel::poll(cancel_value, visits, 16_383)?;
        visits += 1;
        let point = heap.pop(&distances, None);
        let distance = distances[point as usize];
        if distance <= params.radius {
            for slot in params.point_edge_offsets[point as usize]..params.point_edge_offsets[point as usize + 1] {
                let edge = params.point_edges[slot as usize] as usize;
                let (a, b) = (params.edge_a[edge], params.edge_b[edge]);
                let neighbor = if a == point { b } else { a };
                if neighbor != point {
                    let candidate = distance + params.lengths[edge];
                    if candidate <= params.radius && candidate < distances[neighbor as usize] {
                        distances[neighbor as usize] = candidate;
                        parents[neighbor as usize] = point;
                        heap.enqueue(neighbor, &distances, None);
                    }
                }
            }
        }
    }
    Ok((distances, parents))
}

/// An accumulated length was not finite.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HopOverflow;

pub struct HopSearchParams {
    pub blocked_point: Box<dyn Fn(i64) -> bool + Send + Sync>,
    pub blocked_edge: Box<dyn Fn(i64) -> bool + Send + Sync>,
    pub target: i64,
    pub point_edge_offsets: Arc<[i64]>,
    pub point_edges: Arc<[i64]>,
    pub edge_a: Arc<[i64]>,
    pub edge_b: Arc<[i64]>,
    pub lengths: Arc<[f64]>,
}

/// Hop-first search: layer-synchronous breadth-first levels, then the smaller accumulated length,
/// then the lower predecessor index. Fills the caller-owned scratch arrays (each one entry per
/// point; `queue` at least that long). `depths` is `-1` for unreached points, whose `distances` and
/// `previous` are stale. A point for which `blocked_point` holds is never entered, an edge for which
/// `blocked_edge` holds is never crossed, except that `target` (negative for none) is always
/// enterable. With a target the search stops once the target's predecessor layer is settled.
pub fn hop_search(
    params: &HopSearchParams,
    depths: &mut [i64],
    distances: &mut [f64],
    previous: &mut [i64],
    queue: &mut [i64],
    roots: &[i64],
) -> Result<(), HopOverflow> {
    let count = params.point_edge_offsets.len() - 1;
    depths[..count].fill(-1);
    let target = params.target;
    let (mut head, mut tail) = (0_usize, 0_usize);
    let mut overflow = false;
    for &root in roots {
        if depths[root as usize] < 0 {
            depths[root as usize] = 0;
            distances[root as usize] = 0.0;
            previous[root as usize] = -1;
            queue[tail] = root;
            tail += 1;
        }
    }
    while head < tail
        && (target < 0 || depths[target as usize] < 0 || depths[queue[head] as usize] < depths[target as usize])
    {
        let element = queue[head];
        head += 1;
        let eu = element as usize;
        for slot in params.point_edge_offsets[eu]..params.point_edge_offsets[eu + 1] {
            let edge = params.point_edges[slot as usize];
            if !(params.blocked_edge)(edge) {
                let (a, b) = (params.edge_a[edge as usize], params.edge_b[edge as usize]);
                let next = if a == element { b } else { a };
                if next != element && (next == target || !(params.blocked_point)(next)) {
                    let nu = next as usize;
                    let next_depth = depths[eu] + 1;
                    let next_distance = distances[eu] + params.lengths[edge as usize];
                    if !next_distance.is_finite() {
                        overflow = true;
                    } else if depths[nu] < 0
                        || next_depth < depths[nu]
                        || (next_depth == depths[nu]
                            && (next_distance < distances[nu]
                                || (next_distance == distances[nu] && element < previous[nu])))
                    {
                        if depths[nu] < 0 {
                            queue[tail] = next;
                            tail += 1;
                        }
                        depths[nu] = next_depth;
                        distances[nu] = next_distance;
                        previous[nu] = element;
                    }
                }
            }
        }
    }
    if overflow {
        Err(HopOverflow)
    } else {
        Ok(())
    }
}

pub struct Forest {
    /// `infinity` where unreached
    pub distance: Vec<f64>,
    /// a root is its own parent, unreached is `-1`
    pub parent: Vec<i64>,
    /// the edge into each point, `-1` for roots and unreached
    pub edge: Vec<i64>,
    /// the root of each point's tree, `-1` when unreached
    pub root: Vec<i64>,
    /// the first `order_count` entries are the settle order
    pub order: Vec<i64>,
    pub order_count: i64,
}

/// The Edge Transport shortest-path forest: roots are numbered by their own point index, the heap
/// orders by (distance, root, point), and at an equal distance a still unsettled neighbour takes the
/// smaller root and then the smaller parent. `capacity` sizes `order`; it must reach the number of
/// settled points.
pub fn forest_search(
    valid_edge: &dyn Fn(i64) -> bool,
    capacity: i64,
    point_edge_offsets: &[i64],
    point_edges: &[i64],
    edge_a: &[i64],
    edge_b: &[i64],
    lengths: &[f64],
    roots: &[i64],
) -> Forest {
    let point_count = point_edge_offsets.len() - 1;
    let mut distance = vec![f64::INFINITY; point_count];
    let mut tree_parent = vec![-1_i64; point_count];
    let mut tree_edge = vec![-1_i64; point_count];
    let mut tree_root = vec![-1_i64; point_count];
    let mut order = vec![0_i64; capacity.max(0) as usize];
    let mut heap = Heap::new(point_count);
    let mut order_count = 0_usize;
    for &root in roots {
        let point = root as usize;
        if distance[point] == f64::INFINITY {
            distance[point] = 0.0;
            tree_parent[point] = root;
            tree_root[point] = root;
            // The heap reads the root array through a copy-free borrow of the current state.
            heap.enqueue(root, &distance, Some(&tree_root));
        }
    }
    while heap.size > 0 {
        let point = heap.pop(&distance, Some(&tree_root));
        order[order_count] = point;
        order_count += 1;
        let pu = point as usize;
        for at in point_edge_offsets[pu]..point_edge_offsets[pu + 1] {
            let edge = point_edges[at as usize];
            if valid_edge(edge) {
                let (a, b) = (edge_a[edge as usize], edge_b[edge as usize]);
                let neighbor = if a == point { b } else { a };
                let nu = neighbor as usize;
                if heap.positions[nu] != -2 {
                    let candidate = distance[pu] + lengths[edge as usize];
                    let better = candidate < distance[nu]
                        || (candidate == distance[nu]
                            && (tree_root[pu] < tree_root[nu]
                                || (tree_root[pu] == tree_root[nu] && point < tree_parent[nu])));
                    if better {
                        distance[nu] = candidate;
                        tree_parent[nu] = point;
                        tree_edge[nu] = edge;
                        tree_root[nu] = tree_root[pu];
                        heap.enqueue(neighbor, &distance, Some(&tree_root));
                    }
                }
            }
        }
    }
    Forest {
        distance,
        parent: tree_parent,
        edge: tree_edge,
        root: tree_root,
        order,
        order_count: order_count as i64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // A path 0-1-2-3 of unit edges plus a long chord 0-3 of length 2.5.
    fn path() -> (Arc<[i64]>, Arc<[i64]>, Arc<[i64]>, Arc<[i64]>, Arc<[f64]>) {
        let edge_a: Arc<[i64]> = Arc::from(vec![0, 1, 2, 0]);
        let edge_b: Arc<[i64]> = Arc::from(vec![1, 2, 3, 3]);
        let offsets: Arc<[i64]> = Arc::from(vec![0, 2, 4, 6, 8]);
        let edges: Arc<[i64]> = Arc::from(vec![0, 3, 0, 1, 1, 2, 2, 3]);
        let lengths: Arc<[f64]> = Arc::from(vec![1.0, 1.0, 1.0, 2.5]);
        (offsets, edges, edge_a, edge_b, lengths)
    }

    fn params(radius: f64) -> SearchParams {
        let (offsets, edges, edge_a, edge_b, lengths) = path();
        SearchParams { cancel: None, radius, point_edge_offsets: offsets, point_edges: edges, edge_a, edge_b, lengths }
    }

    #[test]
    fn search_settles_by_distance_and_honours_radius() {
        let (distances, parents) = search(&params(f64::INFINITY), &[0]).unwrap();
        assert_eq!(distances, vec![0.0, 1.0, 2.0, 2.5]);
        assert_eq!(parents, vec![-1, 0, 1, 0]);
        let (distances, parents) = search(&params(1.5), &[0]).unwrap();
        assert_eq!(distances[2], f64::INFINITY);
        assert_eq!(parents[3], -1);
        assert_eq!(distances[1], 1.0);
    }

    #[test]
    fn hop_search_prefers_fewer_hops_and_reports_overflow() {
        let (offsets, edges, edge_a, edge_b, lengths) = path();
        let base = HopSearchParams {
            blocked_point: Box::new(|_| false),
            blocked_edge: Box::new(|_| false),
            target: -1,
            point_edge_offsets: offsets.clone(),
            point_edges: edges.clone(),
            edge_a: edge_a.clone(),
            edge_b: edge_b.clone(),
            lengths: lengths.clone(),
        };
        let (mut depths, mut distances, mut previous, mut queue) =
            (vec![0; 4], vec![0.0; 4], vec![0; 4], vec![0; 4]);
        hop_search(&base, &mut depths, &mut distances, &mut previous, &mut queue, &[0]).unwrap();
        assert_eq!(depths, vec![0, 1, 2, 1]);
        assert_eq!(previous[3], 0);
        assert_eq!(distances[3], 2.5);
        let huge: Arc<[f64]> = Arc::from(vec![f64::MAX, f64::MAX, f64::MAX, f64::MAX]);
        let overflow = HopSearchParams { lengths: huge, ..base };
        let both = hop_search(&overflow, &mut depths, &mut distances, &mut previous, &mut queue, &[0, 1]);
        assert_eq!(both, Err(HopOverflow));
    }

    #[test]
    fn forest_assigns_roots_and_parents() {
        let (offsets, edges, edge_a, edge_b, lengths) = path();
        let forest = forest_search(&|_| true, 4, &offsets, &edges, &edge_a, &edge_b, &lengths, &[0, 3]);
        assert_eq!(forest.order_count, 4);
        assert_eq!(forest.root, vec![0, 0, 3, 3]);
        assert_eq!(forest.parent, vec![0, 0, 3, 3]);
        assert_eq!(forest.edge, vec![-1, 0, 2, -1]);
        assert_eq!(forest.distance, vec![0.0, 1.0, 1.0, 0.0]);
        let blocked = forest_search(&|edge| edge != 1 && edge != 2, 4, &offsets, &edges, &edge_a, &edge_b, &lengths, &[0]);
        assert_eq!(blocked.order_count, 3);
        assert_eq!(blocked.root[2], -1);
    }
}
