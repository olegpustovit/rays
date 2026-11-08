//! Port of lib/flow_sop/graph_kernel.ml: private boundary for the pure `graph/` sources.
//! `graph/shortest_paths`, `graph/forest`, `graph/color` and `graph/chains` resolve from their
//! array arguments alone, with owned results, and never read geometry. The adjacency arrives as
//! the record `sop/adjacency` returns (or a CSR row record for colouring).
use flow_eval::Value;
use rays_diagnostic::Diagnostic;
use rdk::error as rdk_error;
use rdk::forest;
use rdk::graph_color;
use rdk::poly_path;
use rdk::shortest_paths;
use std::sync::Arc;

use crate::port::error as refuse;

// methods, not free fns: an output borrows from `self` without a lifetime parameter
trait Named {
    fn named(&self, name: &str) -> Option<&Value>;
    fn has(&self, name: &str) -> bool;
}

impl Named for [(String, Value)] {
    fn named(&self, name: &str) -> Option<&Value> {
        self.iter().find(|(n, _)| n == name).map(|(_, v)| v)
    }

    fn has(&self, name: &str) -> bool {
        self.iter().any(|(n, _)| n == name)
    }
}

fn int_array(record: &[(String, Value)], name: &str) -> Result<Arc<Vec<i64>>, Diagnostic> {
    match record.named(name) {
        Some(Value::Int_array(values)) => Ok(values.clone()),
        _ => Err(refuse("E_GRAPH_ADJACENCY", &format!("Graph adjacency needs the integer array {name}."))),
    }
}

fn record(value: Option<&Value>) -> Result<&[(String, Value)], Diagnostic> {
    match value {
        Some(Value::Record(fields)) => Ok(fields),
        _ => Err(refuse("E_GRAPH_ADJACENCY", "Graph adjacency must be a record of integer arrays.")),
    }
}

fn rdk_failure(code: &str, failure: &rdk_error::T) -> Diagnostic {
    let _ = code;
    refuse(rdk_error::code(failure), &rdk_error::to_string(failure))
}

/// CSR rows: monotone offsets from zero to the value count, values in range.
fn check_rows(label: &str, rows: usize, limit: usize, offsets: &[i64], values: &[i64]) -> Result<(), Diagnostic> {
    if offsets.len() != rows + 1 || offsets[0] != 0 || offsets[rows] != values.len() as i64 {
        return Err(refuse("E_GRAPH_ADJACENCY", &format!("{label} offsets must start at zero and end at the value count.")));
    }
    for row in 0..rows {
        if offsets[row + 1] < offsets[row] {
            return Err(refuse("E_GRAPH_ADJACENCY", &format!("{label} offsets must be monotone.")));
        }
    }
    for &value in values {
        if value < 0 || value >= limit as i64 {
            return Err(refuse("E_GRAPH_ADJACENCY", &format!("{label} value is out of range.")));
        }
    }
    Ok(())
}

struct PointGraph {
    points: usize,
    edges: usize,
    point_edge_offsets: Arc<Vec<i64>>,
    point_edges: Arc<Vec<i64>>,
    edge_a: Arc<Vec<i64>>,
    edge_b: Arc<Vec<i64>>,
}

/// The point/edge part of a sop/adjacency record.
fn point_graph(adjacency: Option<&Value>) -> Result<PointGraph, Diagnostic> {
    let fields = record(adjacency)?;
    let point_edge_offsets = int_array(fields, "point_edge_offsets")?;
    let point_edges = int_array(fields, "point_edges")?;
    let edge_a = int_array(fields, "edge_a")?;
    let edge_b = int_array(fields, "edge_b")?;
    if point_edge_offsets.is_empty() {
        return Err(refuse("E_GRAPH_ADJACENCY", "Graph adjacency point_edge_offsets must hold the point count plus one."));
    }
    let points = point_edge_offsets.len() - 1;
    let edges = edge_a.len();
    if edge_b.len() != edges {
        return Err(refuse("E_GRAPH_ADJACENCY", "Graph adjacency edge_a and edge_b differ in length."));
    }
    for p in edge_a.iter().chain(edge_b.iter()) {
        if *p < 0 || *p >= points as i64 {
            return Err(refuse("E_GRAPH_ADJACENCY", "Graph adjacency edge endpoint is out of range."));
        }
    }
    check_rows("Graph adjacency point_edges", points, edges, &point_edge_offsets, &point_edges)?;
    Ok(PointGraph { points, edges, point_edge_offsets, point_edges, edge_a, edge_b })
}

fn binary_mask(per: &str, label: &str, count: usize, value: Option<&Value>) -> Result<Arc<Vec<i64>>, Diagnostic> {
    match value {
        Some(Value::Int_array(mask)) if mask.len() == count => {
            if mask.iter().any(|bit| *bit != 0 && *bit != 1) {
                return Err(refuse("E_GRAPH_MASK", &format!("{label} holds only 0 and 1.")));
            }
            Ok(mask.clone())
        }
        _ => Err(refuse("E_GRAPH_MASK", &format!("{label} is an integer array with one entry per {per}."))),
    }
}

fn flag(name: &str, args: &[(String, Value)]) -> Result<bool, Diagnostic> {
    match args.named(name) {
        None => Ok(false),
        Some(Value::Bool(value)) => Ok(*value),
        Some(_) => Err(refuse("E_GRAPH_OPTION", &format!("Graph {name} is a boolean."))),
    }
}

/// The tree path from the target back to its root, root first; empty when no target is given or it
/// was not reached.
fn tree_path(parent: &[i64], reached: &dyn Fn(i64) -> bool, target: i64) -> Vec<i64> {
    if target < 0 || !reached(target) {
        return Vec::new();
    }
    let (mut length, mut at) = (1_usize, target);
    while parent[at as usize] >= 0 {
        length += 1;
        at = parent[at as usize];
    }
    let mut path = vec![target; length];
    let mut at = target;
    for slot in (0..length).rev() {
        path[slot] = at;
        at = parent[at as usize];
    }
    path
}

fn shared(values: &[i64]) -> Arc<[i64]> {
    Arc::from(values)
}

fn shared_floats(values: &[f64]) -> Arc<[f64]> {
    Arc::from(values)
}

/// `(distance, parent, path, order)`.
fn shortest_paths(args: &[(String, Value)]) -> Result<(Vec<f64>, Vec<i64>, Vec<i64>, Vec<i64>), Diagnostic> {
    let PointGraph { points, edges, point_edge_offsets, point_edges, edge_a, edge_b } =
        point_graph(args.named("adjacency"))?;
    let lengths = match args.named("lengths") {
        Some(Value::Float_array(lengths)) if lengths.len() == edges => lengths.clone(),
        _ => return Err(refuse("E_GRAPH_LENGTH", "Graph lengths is a float array with one entry per edge.")),
    };
    if lengths.iter().any(|length| !(*length >= 0.0)) {
        return Err(refuse("E_GRAPH_LENGTH", "Graph lengths must not be negative."));
    }
    let roots = match args.named("roots") {
        Some(Value::Int_array(roots)) => roots.clone(),
        _ => return Err(refuse("E_GRAPH_ROOT", "Graph roots is an integer array of point indices.")),
    };
    if roots.iter().any(|root| *root < 0 || *root >= points as i64) {
        return Err(refuse("E_GRAPH_ROOT", "Graph root is not a point index."));
    }
    let hops = flag("hops", args)?;
    let lowest = match args.named("ties") {
        None => false,
        Some(Value::Text(t)) if t.as_slice() == b"first" => false,
        Some(Value::Text(t)) if t.as_slice() == b"lowest" => true,
        Some(_) => return Err(refuse("E_GRAPH_OPTION", "Graph ties is first or lowest.")),
    };
    let target = match args.named("target") {
        None => -1,
        Some(Value::Int(target)) if *target >= 0 && *target < points as i64 => *target,
        Some(_) => return Err(refuse("E_GRAPH_TARGET", "Graph target is a point index.")),
    };
    let masked = args.has("blocked_points") || args.has("blocked_edges");
    if !hops && !lowest && masked {
        return Err(refuse("E_GRAPH_OPTION", "Graph blocked_points and blocked_edges need :hops true or :ties \"lowest\"."));
    }
    if (hops || lowest) && args.has("radius") {
        return Err(refuse("E_GRAPH_RADIUS", "Graph radius does not combine with :hops true or :ties \"lowest\"."));
    }
    if hops && lowest {
        return Err(refuse("E_GRAPH_OPTION", "Graph :hops true does not combine with :ties \"lowest\"."));
    }
    if lowest && args.has("blocked_points") {
        return Err(refuse("E_GRAPH_OPTION", "Graph :ties \"lowest\" takes blocked_edges only."));
    }
    let blocked_edge: Option<Arc<Vec<i64>>> = match args.named("blocked_edges") {
        None => None,
        Some(mask) => Some(binary_mask("edge", "Graph blocked_edges", edges, Some(mask))?),
    };
    if hops {
        let blocked_point: Option<Arc<Vec<i64>>> = match args.named("blocked_points") {
            None => None,
            Some(mask) => Some(binary_mask("point", "Graph blocked_points", points, Some(mask))?),
        };
        let mut depths = vec![-1_i64; points];
        let mut distances = vec![0.0_f64; points];
        let mut previous = vec![-1_i64; points];
        let mut queue = vec![0_i64; points];
        let params = shortest_paths::HopSearchParams {
            blocked_point: Box::new(move |point| blocked_point.as_ref().is_some_and(|mask| mask[point as usize] == 1)),
            blocked_edge: Box::new(move |edge| blocked_edge.as_ref().is_some_and(|mask| mask[edge as usize] == 1)),
            target,
            point_edge_offsets: shared(&point_edge_offsets),
            point_edges: shared(&point_edges),
            edge_a: shared(&edge_a),
            edge_b: shared(&edge_b),
            lengths: shared_floats(&lengths),
        };
        if shortest_paths::hop_search(&params, &mut depths, &mut distances, &mut previous, &mut queue, &roots).is_err() {
            return Err(refuse("E_GRAPH_LENGTH", "Graph accumulated path length is not representable."));
        }
        let parent: Vec<i64> =
            depths.iter().enumerate().map(|(point, depth)| if *depth < 0 { -1 } else { previous[point] }).collect();
        let reached = depths.iter().filter(|depth| **depth >= 0).count();
        let distance: Vec<f64> =
            depths.iter().enumerate().map(|(point, depth)| if *depth < 0 { -1.0 } else { distances[point] }).collect();
        let path = tree_path(&parent, &|point| depths[point as usize] >= 0, target);
        return Ok((distance, parent, path, queue[..reached].to_vec()));
    }
    if lowest {
        let valid = move |edge: i64| match &blocked_edge {
            None => true,
            Some(mask) => mask[edge as usize] != 1,
        };
        let forest = shortest_paths::forest_search(
            &valid,
            points as i64,
            &point_edge_offsets,
            &point_edges,
            &edge_a,
            &edge_b,
            &lengths,
            &roots,
        );
        let parent: Vec<i64> =
            forest.parent.iter().enumerate().map(|(point, p)| if *p == point as i64 { -1 } else { *p }).collect();
        let distance: Vec<f64> = forest.distance.iter().map(|d| if *d == f64::INFINITY { -1.0 } else { *d }).collect();
        let path = tree_path(&parent, &|point| forest.distance[point as usize] < f64::INFINITY, target);
        return Ok((distance, parent, path, forest.order[..forest.order_count as usize].to_vec()));
    }
    let radius = match args.named("radius") {
        None => f64::INFINITY,
        Some(Value::Float(radius)) if *radius >= 0.0 => *radius,
        Some(_) => return Err(refuse("E_GRAPH_RADIUS", "Graph radius is a non-negative float.")),
    };
    let params = shortest_paths::SearchParams {
        cancel: None,
        radius,
        point_edge_offsets: shared(&point_edge_offsets),
        point_edges: shared(&point_edges),
        edge_a: shared(&edge_a),
        edge_b: shared(&edge_b),
        lengths: shared_floats(&lengths),
    };
    let (distances, parents) =
        shortest_paths::search(&params, &roots).map_err(|failure| rdk_failure("E_GRAPH", &failure))?;
    // Flow floats are finite: a point outside every root's reach reports -1.
    let path = tree_path(&parents, &|point| distances[point as usize] < f64::INFINITY, target);
    let distance = distances.iter().map(|d| if *d == f64::INFINITY { -1.0 } else { *d }).collect();
    Ok((distance, parents, path, Vec::new()))
}

/// `(order, depth, root, child_offsets, children)`.
fn forest(args: &[(String, Value)]) -> Result<(Vec<i64>, Vec<i64>, Vec<i64>, Vec<i64>, Vec<i64>), Diagnostic> {
    let parents = match args.named("parents") {
        Some(Value::Int_array(parents)) => parents.clone(),
        _ => return Err(refuse("E_GRAPH_PARENT", "Graph parents is an integer array with one entry per point.")),
    };
    let points = parents.len();
    let selected: Option<Arc<Vec<i64>>> = match args.named("mask") {
        None => None,
        Some(mask) => Some(binary_mask("point", "Graph forest mask", points, Some(mask))?),
    };
    let selected_count = match &selected {
        None => points as i64,
        Some(mask) => mask.iter().sum(),
    };
    let params = forest::Params {
        selected: selected.map(|mask| {
            Box::new(move |point: i64| mask[point as usize] == 1) as Box<dyn Fn(i64) -> bool + Send + Sync>
        }),
        selected_count,
    };
    let scanned = forest::scan(&params, &parents).map_err(|failure| rdk_failure("E_GRAPH_PARENT", &failure))?;
    let (depth, root) = forest::labels(&scanned);
    Ok((scanned.order[..scanned.order_count as usize].to_vec(), depth, root, scanned.child_offsets, scanned.children))
}

fn color(args: &[(String, Value)]) -> Result<(Vec<i64>, i64), Diagnostic> {
    let fields = record(args.named("adjacency"))?;
    let offsets = int_array(fields, "offsets")?;
    let neighbors = int_array(fields, "values")?;
    if offsets.is_empty() {
        return Err(refuse("E_GRAPH_ADJACENCY", "Graph adjacency offsets must hold the node count plus one."));
    }
    let nodes = offsets.len() - 1;
    check_rows("Graph adjacency", nodes, nodes, &offsets, &neighbors)?;
    let order = match args.named("order") {
        Some(Value::Int_array(order)) => order.clone(),
        _ => return Err(refuse("E_GRAPH_ORDER", "Graph order is an integer array of node indices.")),
    };
    let mut seen = vec![false; nodes];
    for &node in order.iter() {
        if node < 0 || node >= nodes as i64 {
            return Err(refuse("E_GRAPH_ORDER", "Graph order holds a node outside the graph."));
        }
        if seen[node as usize] {
            return Err(refuse("E_GRAPH_ORDER", "Graph order lists a node twice."));
        }
        seen[node as usize] = true;
    }
    let colors = graph_color::greedy(&offsets, &neighbors, &order).map_err(|failure| rdk_failure("E_GRAPH_ORDER", &failure))?;
    let count = 1 + colors.iter().copied().fold(-1, i64::max);
    Ok((colors, count))
}

fn chains(args: &[(String, Value)]) -> Result<(Vec<i64>, Vec<i64>, Vec<i64>), Diagnostic> {
    let graph = point_graph(args.named("adjacency"))?;
    let selected: Arc<Option<Arc<Vec<i64>>>> = Arc::new(match args.named("mask") {
        None => None,
        mask => Some(binary_mask("edge", "Graph chains mask", graph.edges, mask)?),
    });
    let keep = move |edge: i64| match selected.as_ref() {
        None => true,
        Some(mask) => mask[edge as usize] == 1,
    };
    poly_path::chains(graph.points as i64, &graph.edge_a, &graph.edge_b, &keep)
        .map_err(|failure| refuse("E_GRAPH_CHAINS", rdk_error::message(&failure)))
}

fn ints(values: Vec<i64>) -> Value {
    Value::Int_array(Arc::new(values))
}

/// Resolve a `graph/` source value to the component it names.
pub fn resolve(value: &Value) -> Result<Value, Diagnostic> {
    let Value::Source(g) = value else {
        return Err(refuse("E_DATA_SOURCE", "Unknown graph data source."));
    };
    let (name, args) = (&g.name, &g.fields);
    let component = match args.named("component") {
        Some(Value::Text(component)) => String::from_utf8_lossy(component).into_owned(),
        _ => return Err(refuse("E_DATA_SOURCE", "Graph source requires a component.")),
    };
    let unknown = || refuse("E_DATA_SOURCE", "Unknown graph component.");
    match name.as_str() {
        "graph/shortest_paths" => {
            let (distance, parent, path, order) = shortest_paths(args)?;
            match component.as_str() {
                "distance" => Ok(Value::Float_array(Arc::new(distance))),
                "parent" => Ok(ints(parent)),
                "path" => Ok(ints(path)),
                "order" => Ok(ints(order)),
                _ => Err(unknown()),
            }
        }
        "graph/forest" => {
            let (order, depth, root, offsets, children) = forest(args)?;
            match component.as_str() {
                "order" => Ok(ints(order)),
                "depth" => Ok(ints(depth)),
                "root" => Ok(ints(root)),
                "offsets" => Ok(ints(offsets)),
                "children" => Ok(ints(children)),
                _ => Err(unknown()),
            }
        }
        "graph/color" => {
            let (colors, count) = color(args)?;
            match component.as_str() {
                "color" => Ok(ints(colors)),
                "count" => Ok(Value::Int(count)),
                _ => Err(unknown()),
            }
        }
        "graph/chains" => {
            let (offsets, values, closed) = chains(args)?;
            match component.as_str() {
                "offsets" => Ok(ints(offsets)),
                "values" => Ok(ints(values)),
                "closed" => Ok(ints(closed)),
                _ => Err(unknown()),
            }
        }
        _ => Err(refuse("E_DATA_SOURCE", "Unknown graph data source.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source(name: &str, component: &str, mut args: Vec<(String, Value)>) -> Value {
        args.push(("component".to_owned(), Value::Text(component.as_bytes().to_vec())));
        Value::Source(name.to_owned(), flow_syntax::ty::Ty::Any, args)
    }

    fn path_adjacency() -> Value {
        // 0 - 1 - 2, edges (0,1) and (1,2)
        let ints = |v: &[i64]| Value::Int_array(Arc::new(v.to_vec()));
        Value::Record(vec![
            ("point_edge_offsets".to_owned(), ints(&[0, 1, 3, 4])),
            ("point_edges".to_owned(), ints(&[0, 0, 1, 1])),
            ("edge_a".to_owned(), ints(&[0, 1])),
            ("edge_b".to_owned(), ints(&[1, 2])),
        ])
    }

    #[test]
    fn shortest_paths_on_a_path_graph() {
        let args = vec![
            ("adjacency".to_owned(), path_adjacency()),
            ("lengths".to_owned(), Value::Float_array(Arc::new(vec![1.0, 2.0]))),
            ("roots".to_owned(), Value::Int_array(Arc::new(vec![0]))),
            ("target".to_owned(), Value::Int(2)),
        ];
        let distance = resolve(&source("graph/shortest_paths", "distance", args.clone())).expect("distance");
        assert_eq!(distance, Value::Float_array(Arc::new(vec![0.0, 1.0, 3.0])));
        let path = resolve(&source("graph/shortest_paths", "path", args.clone())).expect("path");
        assert_eq!(path, Value::Int_array(Arc::new(vec![0, 1, 2])));
        let mut hopped = args.clone();
        hopped.push(("hops".to_owned(), Value::Bool(true)));
        let order = resolve(&source("graph/shortest_paths", "order", hopped)).expect("order");
        assert_eq!(order, Value::Int_array(Arc::new(vec![0, 1, 2])));
    }

    #[test]
    fn refusals_name_their_codes() {
        let bad = source("graph/shortest_paths", "distance", vec![("adjacency".to_owned(), Value::No_geo)]);
        assert_eq!(resolve(&bad).unwrap_err().code, "E_GRAPH_ADJACENCY");
        let unknown = source("graph/nowhere", "x", Vec::new());
        assert_eq!(resolve(&unknown).unwrap_err().code, "E_DATA_SOURCE");
        assert_eq!(resolve(&Value::Int(1)).unwrap_err().code, "E_DATA_SOURCE");
    }

    #[test]
    fn colouring_and_chains() {
        let ints = |v: &[i64]| Value::Int_array(Arc::new(v.to_vec()));
        let adjacency = Value::Record(vec![("offsets".to_owned(), ints(&[0, 1, 3, 4])), ("values".to_owned(), ints(&[1, 0, 2, 1]))]);
        let args = vec![("adjacency".to_owned(), adjacency), ("order".to_owned(), ints(&[0, 1, 2]))];
        assert_eq!(resolve(&source("graph/color", "count", args.clone())).expect("count"), Value::Int(2));
        assert_eq!(resolve(&source("graph/color", "color", args)).expect("color"), ints(&[0, 1, 0]));
        let chain = vec![("adjacency".to_owned(), path_adjacency())];
        let values = resolve(&source("graph/chains", "values", chain.clone())).expect("values");
        assert_eq!(values, ints(&[0, 1, 2]));
    }
}
