//! Maximal point chains of a unique-edge graph.
//!
//! OCaml returns `(.., string) result`; here a failure is an `error::T` with operation
//! `Poly_path.chains`, code `invalid_parameter` and the OCaml message.
use rdk_core::error;
use rdk_core::error::Code;

fn fail(message: &str) -> error::T {
    error::new("Poly_path.chains", Code::InvalidParameter, message.to_owned())
}

struct Traced {
    path_count: usize,
    edge_order: Vec<i64>,
    path_offsets: Vec<i64>,
    path_start: Vec<i64>,
    isolated_loop: Vec<bool>,
}

/// Graph incidence (CSR by point) plus the walk state shared by every traced path.
struct Walk {
    graph_a: Vec<i64>,
    graph_b: Vec<i64>,
    point_offsets: Vec<i64>,
    point_edges: Vec<i64>,
    visited: Vec<bool>,
    edge_order: Vec<i64>,
    path_offsets: Vec<i64>,
    path_start: Vec<i64>,
    isolated_loop: Vec<bool>,
    path_count: usize,
    edge_at: usize,
}

impl Walk {
    fn degree(&self, point: usize) -> i64 {
        self.point_offsets[point + 1] - self.point_offsets[point]
    }

    fn other(&self, edge: usize, point: i64) -> i64 {
        if self.graph_a[edge] == point { self.graph_b[edge] } else { self.graph_a[edge] }
    }

    fn trace(&mut self, isolated: bool, start: i64, first_edge: i64) {
        let path = self.path_count;
        self.path_count += 1;
        self.path_offsets[path] = self.edge_at as i64;
        self.path_start[path] = start;
        if isolated {
            self.isolated_loop[path] = true;
        }
        let mut walk_current = start;
        let mut walk_edge = first_edge;
        while walk_edge >= 0 {
            let edge = walk_edge as usize;
            if self.visited[edge] {
                walk_edge = -1;
            } else {
                self.visited[edge] = true;
                self.edge_order[self.edge_at] = walk_edge;
                self.edge_at += 1;
                let next_point = self.other(edge, walk_current);
                if self.degree(next_point as usize) != 2 {
                    walk_current = next_point;
                    walk_edge = -1;
                } else {
                    let first = self.point_offsets[next_point as usize] as usize;
                    let (left, right) = (self.point_edges[first], self.point_edges[first + 1]);
                    let next_edge = if left == walk_edge { right } else { left };
                    walk_current = next_point;
                    walk_edge = if self.visited[next_edge as usize] { -1 } else { next_edge };
                }
            }
        }
        self.path_offsets[path + 1] = self.edge_at as i64;
    }
}

/// A compact edge permutation describes every maximal path of a graph whose edges are
/// `graph_a[e]`-`graph_b[e]` (no self edges). Branch paths are emitted by point number; isolated
/// degree-two loops follow by their first remaining edge. Work-stealing therefore cannot affect
/// ordering.
fn trace_paths(point_count: usize, graph_a: Vec<i64>, graph_b: Vec<i64>) -> Result<Traced, error::T> {
    let graph_count = graph_a.len();
    let mut point_offsets = vec![0_i64; point_count + 1];
    for edge in 0..graph_count {
        point_offsets[graph_a[edge] as usize + 1] += 1;
        point_offsets[graph_b[edge] as usize + 1] += 1;
    }
    for point in 0..point_count {
        point_offsets[point + 1] += point_offsets[point];
    }
    let mut point_edges = vec![0_i64; graph_count * 2];
    let mut cursor = point_offsets.clone();
    for edge in 0..graph_count {
        let (a, b) = (graph_a[edge] as usize, graph_b[edge] as usize);
        point_edges[cursor[a] as usize] = edge as i64;
        cursor[a] += 1;
        point_edges[cursor[b] as usize] = edge as i64;
        cursor[b] += 1;
    }
    let mut walk = Walk {
        graph_a,
        graph_b,
        point_offsets,
        point_edges,
        visited: vec![false; graph_count],
        edge_order: vec![0; graph_count],
        path_offsets: vec![0; graph_count + 1],
        path_start: vec![0; graph_count],
        isolated_loop: vec![false; graph_count],
        path_count: 0,
        edge_at: 0,
    };
    for point in 0..point_count {
        if walk.degree(point) != 2 {
            for incidence in walk.point_offsets[point]..walk.point_offsets[point + 1] {
                let edge = walk.point_edges[incidence as usize];
                if !walk.visited[edge as usize] {
                    walk.trace(false, point as i64, edge);
                }
            }
        }
    }
    for edge in 0..graph_count {
        if !walk.visited[edge] {
            let start = walk.graph_a[edge];
            walk.trace(true, start, edge as i64);
        }
    }
    if walk.edge_at != graph_count {
        return Err(fail("PolyPath internal graph traversal did not consume every edge"));
    }
    Ok(Traced {
        path_count: walk.path_count,
        edge_order: walk.edge_order,
        path_offsets: walk.path_offsets,
        path_start: walk.path_start,
        isolated_loop: walk.isolated_loop,
    })
}

/// Point chains of the selected non-self edges of a unique-edge graph, as the open polylines `run`
/// emits: returns `(offsets, values, closed)`, ragged point rows, with `closed[i] = 1` for an isolated
/// loop (its first point repeats last).
pub fn chains(
    point_count: i64,
    edge_a: &[i64],
    edge_b: &[i64],
    selected: &dyn Fn(i64) -> bool,
) -> Result<(Vec<i64>, Vec<i64>, Vec<i64>), error::T> {
    let total = edge_a.len();
    let mut graph_a = Vec::new();
    let mut graph_b = Vec::new();
    for edge in 0..total {
        if selected(edge as i64) && edge_a[edge] != edge_b[edge] {
            graph_a.push(edge_a[edge]);
            graph_b.push(edge_b[edge]);
        }
    }
    let traced = trace_paths(point_count as usize, graph_a.clone(), graph_b.clone())?;
    let mut offsets = vec![0_i64; traced.path_count + 1];
    for path in 0..traced.path_count {
        offsets[path + 1] = offsets[path] + traced.path_offsets[path + 1] - traced.path_offsets[path] + 1;
    }
    let mut values = vec![0_i64; offsets[traced.path_count] as usize];
    for path in 0..traced.path_count {
        let mut current = traced.path_start[path];
        let mut at = offsets[path] as usize;
        for order in traced.path_offsets[path]..traced.path_offsets[path + 1] {
            let edge = traced.edge_order[order as usize] as usize;
            values[at] = current;
            at += 1;
            current = if graph_a[edge] == current { graph_b[edge] } else { graph_a[edge] };
        }
        values[at] = current;
    }
    let closed = (0..traced.path_count).map(|path| i64::from(traced.isolated_loop[path])).collect();
    Ok((offsets, values, closed))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn traces_open_chains_branches_and_isolated_loops() {
        // path 0-1-2, a self edge on 2 (ignored), a loop 3-4-5-3, and a fork 6-7, 6-8, 6-9.
        let edge_a = [0, 1, 2, 3, 4, 5, 6, 6, 6];
        let edge_b = [1, 2, 2, 4, 5, 3, 7, 8, 9];
        let (offsets, values, closed) = chains(10, &edge_a, &edge_b, &|_| true).unwrap();
        assert_eq!(offsets, vec![0, 3, 5, 7, 9, 13]);
        assert_eq!(values, vec![0, 1, 2, 6, 7, 6, 8, 6, 9, 3, 4, 5, 3]);
        assert_eq!(closed, vec![0, 0, 0, 0, 1]);
    }
}
