//! Port of lib/flow_sop/test_graph.ml: graph/shortest_paths, graph/color, graph/chains, graph/forest
//! against independent oracles and the RDK callers they wrap. The reference interpreter and the strict
//! compiled tier must agree with the oracle at one and eight domains.
use crate::{attribute_kernel as ak, catalog, operators};
use flow_check::catalog::Catalog;
use flow_eval::{self as e, Value};
use flow_ir::flow_ir::executor;
use flow_syntax::syntax;
use flow_syntax::ty;
use rdk_core::geometry as geo;
use rdk_core::topology::{self as topo, PrimitiveKind as K};
use rdk_core::topology_index::{self as tix, view::View};
use rdk_core::packed;
use rays_diagnostic::Diagnostic;
use std::collections::HashMap;
use std::sync::Arc;

type Exp = Value;

fn ints(a: &[i64]) -> Value {
    Value::Int_array(Arc::new(a.to_vec()))
}
fn floats(a: Vec<f64>) -> Value {
    Value::Floats(1, Arc::new(a))
}
fn rec(fields: Vec<(&str, Value)>) -> Value {
    Value::Record(fields.into_iter().map(|(n, v)| (n.into(), v)).collect::<Vec<_>>().into())
}

fn fixtures() -> Vec<(&'static str, geo::T)> {
    let topology = |points: i64, vertex_points: &[i64], offsets: &[i64], kinds: &[K]| topo::create_owned(points, vertex_points.to_vec(), offsets.to_vec(), kinds.to_vec()).unwrap();
    vec![
        ("empty", topo::empty(0).unwrap()),
        ("shared-polygons", topology(5, &[0, 1, 2, 2, 1, 3], &[0, 3, 6], &[K::Polygon, K::Polygon])),
        ("open-repeated", topology(3, &[0, 1, 1, 2, 1], &[0, 5], &[K::OpenPolyline])),
        ("closed", topology(4, &[3, 2, 1, 0], &[0, 4], &[K::ClosedPolyline])),
        (
            "mixed-nonmanifold",
            topology(6, &[0, 1, 2, 0, 1, 3, 1, 0, 4, 0, 1, 5], &[0, 3, 6, 9, 12], &[K::Polygon, K::OpenPolyline, K::ClosedPolyline, K::Polygon]),
        ),
    ]
    .into_iter()
    .map(|(name, topology)| {
        let count = topo::point_count(&topology) as usize;
        let positions = packed::float3::view::of_owned_exn((0..count).map(|i| i as f64).collect(), vec![0.0; count], vec![0.0; count]).unwrap();
        (name, geo::create(positions, topology, None, None, None).unwrap())
    })
    .collect()
}

fn evaluate(geometry: &geo::T, decls: &str, inputs: &[(&str, Value)], body: &str) -> (Value, flow_ir::core_exec::ResolveFn) {
    let source = format!("(workspace graph (graph g :context sop [(geo : geometry nil) {decls}] (let* [tested {body}] geo)))");
    let forms = syntax::parse(&source).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let checked = match catalog::check_workspace(operators::all(), false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms) {
        (Some(w), d) if d.is_empty() => w,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let mut named = vec![("geo".into(), Value::Deferred(ty::geometry(), 7))];
    named.extend(inputs.iter().map(|(n, v)| (flow_syntax::symbol::intern(n), v.clone())));
    let evaluation = e::static_with(&checked, true, Some(&[("g".into(), named)]), None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let (_, values) = evaluation.records.iter().find(|(path, _)| path.as_slice() == ["g", "tested"]).expect("tested record");
    let value = values[0].1.clone();
    let geometry = geometry.clone();
    let lookup: ak::GeometryLookup = Arc::new(move |id| if id == 7 { Some(geometry.clone()) } else { None });
    (value, Arc::new(move |v: &Value| ak::resolve(None, &lookup, v)))
}

fn agree(geometry: &geo::T, decls: &str, inputs: &[(&str, Value)], body: &str, expected: &Exp) {
    let (value, resolve) = evaluate(geometry, decls, inputs, body);
    let live = input::at_time(0.25);
    let program = executor::compile(&value, &flow_ir::flow_ir::executor::CompileOptions { strict: true, ..Default::default() }).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    for domains in [1, 8] {
        parallel::run_unit(Some(domains), &|| {
            let reference = e::host::force_reference(&value, &live, None, None, Some(resolve.clone())).map_err(|d| rays_diagnostic::to_string(&d))?;
            if &reference != expected {
                return Err(format!("reference differs for {body}: {reference:?} <> {expected:?}"));
            }
            let compiled = executor::force(&program, &flow_ir::flow_ir::executor::ForceOptions { resolve: Some(resolve.clone()), strict: true, ..Default::default() }, &live).map_err(|d| rays_diagnostic::to_string(&d))?;
            if &compiled != expected {
                return Err(format!("strict tier differs for {body}: {compiled:?} <> {expected:?}"));
            }
            Ok(())
        })
        .unwrap();
    }
}

fn refused(geometry: &geo::T, decls: &str, inputs: &[(&str, Value)], body: &str, code: &str) {
    let (value, resolve) = evaluate(geometry, decls, inputs, body);
    match e::host::force_reference(&value, &input::at_time(0.0), None, None, Some(resolve)) {
        Err(Diagnostic { code: c, .. }) => assert_eq!(c, code),
        Ok(_) => panic!("expected {code}"),
    }
}

fn view_of(geometry: &geo::T) -> View {
    tix::view::view(&tix::create(None, geo::topology(geometry)).unwrap())
}

fn other(view: &View, p: i64, e: i64) -> i64 {
    if view.edge_a[e as usize] == p { view.edge_b[e as usize] } else { view.edge_a[e as usize] }
}

fn walk(parent: &[i64], target: i64) -> Vec<i64> {
    let mut path = vec![target];
    let mut p = target;
    while parent[p as usize] >= 0 {
        p = parent[p as usize];
        path.push(p);
    }
    path.reverse();
    path
}

/// Textbook O(n^2) Dijkstra: settle the unsettled point of least (distance, index), relax strictly smaller
/// candidates so the first settled neighbour is the parent.
fn oracle_paths(view: &View, radius: f64, lengths: &[f64], roots: &[i64]) -> Exp {
    let n = view.point_edge_offsets.len() - 1;
    let mut dist = vec![f64::INFINITY; n];
    let mut parent = vec![-1i64; n];
    let mut settled = vec![false; n];
    for r in roots {
        dist[*r as usize] = 0.0;
    }
    loop {
        let mut best: Option<usize> = None;
        for p in 0..n {
            if !settled[p] && dist[p] < f64::INFINITY && best.is_none_or(|b| dist[p] < dist[b]) {
                best = Some(p);
            }
        }
        let Some(p) = best else { break };
        settled[p] = true;
        for slot in view.point_edge_offsets[p]..view.point_edge_offsets[p + 1] {
            let e = view.point_edges[slot as usize];
            let q = other(view, p as i64, e) as usize;
            let candidate = dist[p] + lengths[e as usize];
            if q != p && candidate <= radius && candidate < dist[q] {
                dist[q] = candidate;
                parent[q] = p as i64;
            }
        }
    }
    rec(vec![
        ("distance", floats(dist.iter().map(|d| if *d == f64::INFINITY { -1.0 } else { *d }).collect())),
        ("parent", ints(&parent)),
        ("path", ints(&[])),
        ("order", ints(&[])),
    ])
}

fn range(n: i64) -> Vec<i64> {
    (0..n).collect()
}

fn alternating(edges: usize) -> (String, Vec<f64>) {
    (
        format!("(exact (map (fn [(i : int)] (float (int/add 1 (int/mod i 2)))) (array/range {edges} :type int)))"),
        (0..edges).map(|i| (1 + i % 2) as f64).collect(),
    )
}

#[test]
fn shortest_paths() {
    for (_, geometry) in fixtures() {
        let view = view_of(&geometry);
        let points = geo::point_count(&geometry);
        let edges = view.edge_a.len();
        for roots in [0i64, 1, 2] {
            if roots <= points {
                for (lisp, lengths) in [(format!("(array/float {edges} 1.0)"), vec![1.0; edges]), alternating(edges)] {
                    agree(&geometry, "", &[], &format!("(graph/shortest_paths (sop/adjacency geo) {lisp} (array/range {roots} :type int))"), &oracle_paths(&view, f64::INFINITY, &lengths, &range(roots)));
                }
            }
        }
    }
    // A radius cuts the search; malformed input is refused.
    let geometry = fixtures().remove(3).1;
    let view = view_of(&geometry);
    agree(&geometry, "", &[], "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :radius 1.5)", &oracle_paths(&view, 1.5, &[1.0; 4], &[0]));
    refused(&geometry, "", &[], "(graph/shortest_paths (sop/adjacency geo) (array/float 3 1.0) (array/int 1 0))", "E_GRAPH_LENGTH");
    refused(&geometry, "", &[], "(graph/shortest_paths (sop/adjacency geo) (array/float 4 -1.0) (array/int 1 0))", "E_GRAPH_LENGTH");
    refused(&geometry, "", &[], "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 9))", "E_GRAPH_ROOT");
}

#[test]
fn color() {
    let decls = "(rows : {:offsets (array int) :values (array int)} {:offsets (array/int 1) :values (array/int 0)}) (order : (array int) (array/int 0))";
    let geometry = fixtures().remove(1).1;
    let bad = |rows: Value, order: Value, code: &str| refused(&geometry, decls, &[("rows", rows), ("order", order)], "(graph/color rows order)", code);
    let one = rec(vec![("offsets", ints(&[0, 0])), ("values", ints(&[]))]);
    bad(one.clone(), ints(&[0, 0]), "E_GRAPH_ORDER");
    bad(one, ints(&[1]), "E_GRAPH_ORDER");
    bad(rec(vec![("offsets", ints(&[0, 1])), ("values", ints(&[5]))]), ints(&[0]), "E_GRAPH_ADJACENCY");
}

#[test]
fn chains() {
    let geometry = fixtures().remove(3).1;
    refused(&geometry, "", &[], "(graph/chains (sop/adjacency geo) :mask (array/int 3 1))", "E_GRAPH_MASK");
    refused(&geometry, "", &[], "(graph/chains (sop/adjacency geo) :mask (array/int 4 2))", "E_GRAPH_MASK");
}

/// Layer by layer oracle: the next layer takes, per point, the least (accumulated length, predecessor
/// index) over the current layer.
fn oracle_hops(view: &View, blocked_point: &dyn Fn(i64) -> bool, blocked_edge: &dyn Fn(i64) -> bool, target: i64, lengths: &[f64], roots: &[i64]) -> Exp {
    let n = view.point_edge_offsets.len() - 1;
    let mut depth = vec![-1i64; n];
    let mut dist = vec![0.0f64; n];
    let mut parent = vec![-1i64; n];
    let mut layer: Vec<i64> = Vec::new();
    for r in roots {
        if depth[*r as usize] < 0 {
            depth[*r as usize] = 0;
            layer.push(*r);
        }
    }
    let mut discovered: Vec<i64> = layer.clone();
    let mut level = 0;
    while !layer.is_empty() && (target < 0 || depth[target as usize] < 0) {
        let mut best: HashMap<i64, (f64, i64)> = HashMap::new();
        let mut found: Vec<i64> = Vec::new();
        for &p in &layer {
            for slot in view.point_edge_offsets[p as usize]..view.point_edge_offsets[p as usize + 1] {
                let e = view.point_edges[slot as usize];
                let q = other(view, p, e);
                if q != p && depth[q as usize] < 0 && !blocked_edge(e) && (q == target || !blocked_point(q)) {
                    let candidate = dist[p as usize] + lengths[e as usize];
                    if !best.contains_key(&q) {
                        found.push(q);
                    }
                    match best.get(&q) {
                        Some((d, w)) if *d < candidate || (*d == candidate && *w <= p) => {}
                        _ => {
                            best.insert(q, (candidate, p));
                        }
                    }
                }
            }
        }
        level += 1;
        layer = found;
        for &q in &layer {
            let (d, p) = best[&q];
            depth[q as usize] = level;
            dist[q as usize] = d;
            parent[q as usize] = p;
            discovered.push(q);
        }
    }
    let path = if target < 0 || depth[target as usize] < 0 { Vec::new() } else { walk(&parent, target) };
    rec(vec![
        ("distance", floats((0..n).map(|p| if depth[p] < 0 { -1.0 } else { dist[p] }).collect())),
        ("parent", ints(&parent)),
        ("path", ints(&path)),
        ("order", ints(&discovered)),
    ])
}

fn mask_lisp(count: usize, modulus: i64) -> String {
    format!("(exact (map (fn [(i : int)] (if (int/eq (int/mod i {modulus}) 0) 1 0)) (array/range {count} :type int)))")
}

#[test]
fn hops() {
    for (_, geometry) in fixtures() {
        let view = view_of(&geometry);
        let points = geo::point_count(&geometry);
        let edges = view.edge_a.len();
        for (lengths_lisp, lengths) in [(format!("(array/float {edges} 1.0)"), vec![1.0; edges]), alternating(edges)] {
            for roots in [1i64, 2] {
                if roots <= points && points > 0 {
                    for target in [-1i64, 0, points - 1, 2] {
                        if target < points {
                            for (point_blocks, edge_blocks) in [(None, None), (Some(2i64), None), (None, Some(2i64)), (Some(3), Some(2))] {
                                let blocked_point = move |p: i64| point_blocks.is_some_and(|m| p % m == 0);
                                let blocked_edge = move |e: i64| edge_blocks.is_some_and(|m| e % m == 0);
                                let options = format!(
                                    ":hops true{}{}{}",
                                    if target >= 0 { format!(" :target {target}") } else { String::new() },
                                    point_blocks.map_or(String::new(), |m| format!(" :blocked_points {}", mask_lisp(points as usize, m))),
                                    edge_blocks.map_or(String::new(), |m| format!(" :blocked_edges {}", mask_lisp(edges, m))),
                                );
                                agree(
                                    &geometry,
                                    "",
                                    &[],
                                    &format!("(graph/shortest_paths (sop/adjacency geo) {lengths_lisp} (array/range {roots} :type int) {options})"),
                                    &oracle_hops(&view, &blocked_point, &blocked_edge, target, &lengths, &range(roots)),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    // Hop count beats length: the one-hop edge of length 9 wins over two short hops.
    let geometry = fixtures().remove(3).1;
    let nobody = |_: i64| false;
    agree(
        &geometry,
        "",
        &[],
        "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true :target 2)",
        &oracle_hops(&view_of(&geometry), &nobody, &nobody, 2, &[1.0; 4], &[0]),
    );
    let base = "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0)";
    refused(&geometry, "", &[], &format!("{base} :hops true :target 9)"), "E_GRAPH_TARGET");
    refused(&geometry, "", &[], &format!("{base} :hops true :radius 1.0)"), "E_GRAPH_RADIUS");
    refused(&geometry, "", &[], &format!("{base} :blocked_points (array/int 4 0))"), "E_GRAPH_OPTION");
    refused(&geometry, "", &[], &format!("{base} :hops true :blocked_points (array/int 3 0))"), "E_GRAPH_MASK");
    refused(&geometry, "", &[], &format!("{base} :hops true :blocked_edges (array/int 4 2))"), "E_GRAPH_MASK");
}

/// O(n^2) oracle of the Edge Transport forest: settle the queued point of least (distance, root, index); at
/// an equal distance an unsettled neighbour takes the smaller root, then the smaller parent.
fn oracle_lowest(view: &View, blocked_edge: &dyn Fn(i64) -> bool, target: i64, lengths: &[f64], roots: &[i64]) -> Exp {
    let n = view.point_edge_offsets.len() - 1;
    let mut dist = vec![f64::INFINITY; n];
    let mut parent = vec![-1i64; n];
    let mut root = vec![-1i64; n];
    let mut settled = vec![false; n];
    let mut order: Vec<i64> = Vec::new();
    for r in roots {
        let r = *r as usize;
        if dist[r] == f64::INFINITY {
            dist[r] = 0.0;
            parent[r] = r as i64;
            root[r] = r as i64;
        }
    }
    loop {
        let mut best: Option<usize> = None;
        for p in 0..n {
            if !settled[p] && dist[p] < f64::INFINITY {
                let better = match best {
                    None => true,
                    Some(b) => (dist[p], root[p], p as i64) < (dist[b], root[b], b as i64),
                };
                if better {
                    best = Some(p);
                }
            }
        }
        let Some(p) = best else { break };
        settled[p] = true;
        order.push(p as i64);
        for slot in view.point_edge_offsets[p]..view.point_edge_offsets[p + 1] {
            let e = view.point_edges[slot as usize];
            let q = other(view, p as i64, e) as usize;
            if !blocked_edge(e) && !settled[q] {
                let candidate = dist[p] + lengths[e as usize];
                if candidate < dist[q] || (candidate == dist[q] && (root[p] < root[q] || (root[p] == root[q] && (p as i64) < parent[q]))) {
                    dist[q] = candidate;
                    parent[q] = p as i64;
                    root[q] = root[p];
                }
            }
        }
    }
    let parent: Vec<i64> = parent.iter().enumerate().map(|(p, q)| if p as i64 == *q { -1 } else { *q }).collect();
    let path = if target < 0 || dist[target as usize] == f64::INFINITY { Vec::new() } else { walk(&parent, target) };
    rec(vec![
        ("distance", floats(dist.iter().map(|d| if *d == f64::INFINITY { -1.0 } else { *d }).collect())),
        ("parent", ints(&parent)),
        ("path", ints(&path)),
        ("order", ints(&order)),
    ])
}

#[test]
fn lowest() {
    for (_, geometry) in fixtures() {
        let view = view_of(&geometry);
        let points = geo::point_count(&geometry);
        let edges = view.edge_a.len();
        let mod3 = (
            format!("(exact (map (fn [(i : int)] (float (int/mod i 3))) (array/range {edges} :type int)))"),
            (0..edges).map(|i| (i % 3) as f64).collect::<Vec<_>>(),
        );
        for (lengths_lisp, lengths) in [(format!("(array/float {edges} 1.0)"), vec![1.0; edges]), mod3] {
            for roots in [1i64, 2, 3] {
                if roots <= points && points > 0 {
                    for target in [-1i64, 0, points - 1] {
                        if target < points {
                            for blocks in [None, Some(2i64), Some(3)] {
                                let blocked_edge = move |e: i64| blocks.is_some_and(|m| e % m == 0);
                                let options = format!(
                                    ":ties \"lowest\"{}{}",
                                    if target >= 0 { format!(" :target {target}") } else { String::new() },
                                    blocks.map_or(String::new(), |m| format!(" :blocked_edges {}", mask_lisp(edges, m))),
                                );
                                agree(
                                    &geometry,
                                    "",
                                    &[],
                                    &format!("(graph/shortest_paths (sop/adjacency geo) {lengths_lisp} (array/range {roots} :type int) {options})"),
                                    &oracle_lowest(&view, &blocked_edge, target, &lengths, &range(roots)),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    let geometry = fixtures().remove(3).1;
    let base = "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0)";
    refused(&geometry, "", &[], &format!("{base} :ties \"lowest\" :blocked_points (array/int 4 0))"), "E_GRAPH_OPTION");
    refused(&geometry, "", &[], &format!("{base} :ties \"lowest\" :hops true)"), "E_GRAPH_OPTION");
    refused(&geometry, "", &[], &format!("{base} :ties \"other\")"), "E_GRAPH_OPTION");
}

/// Independent forest: follow parents with a visited-in-this-walk check.
fn oracle_forest(mask: Option<&[i64]>, parents: &[i64]) -> Exp {
    let n = parents.len();
    let selected = |p: usize| mask.is_none_or(|m| m[p] == 1);
    let tree: Vec<i64> = (0..n)
        .map(|p| {
            if !selected(p) {
                -1
            } else {
                let q = parents[p];
                if q < 0 || q >= n as i64 || q == p as i64 || !selected(q as usize) { p as i64 } else { q }
            }
        })
        .collect();
    let mut counts = vec![0i64; n + 1];
    for (p, q) in tree.iter().enumerate() {
        if *q >= 0 && *q != p as i64 {
            counts[*q as usize + 1] += 1;
        }
    }
    for p in 0..n {
        counts[p + 1] += counts[p];
    }
    let mut children = vec![0i64; counts[n] as usize];
    let mut fill: Vec<i64> = counts[..n].to_vec();
    for (p, q) in tree.iter().enumerate() {
        if *q >= 0 && *q != p as i64 {
            children[fill[*q as usize] as usize] = p as i64;
            fill[*q as usize] += 1;
        }
    }
    let mut depth = vec![-1i64; n];
    let mut root = vec![-1i64; n];
    let mut order: Vec<i64> = Vec::new();
    for r in 0..n {
        if tree[r] == r as i64 {
            let mut queue = std::collections::VecDeque::new();
            depth[r] = 0;
            root[r] = r as i64;
            queue.push_back(r);
            while let Some(p) = queue.pop_front() {
                order.push(p as i64);
                for at in counts[p]..counts[p + 1] {
                    let c = children[at as usize] as usize;
                    depth[c] = depth[p] + 1;
                    root[c] = r as i64;
                    queue.push_back(c);
                }
            }
        }
    }
    rec(vec![("order", ints(&order)), ("depth", ints(&depth)), ("root", ints(&root)), ("offsets", ints(&counts)), ("children", ints(&children))])
}

#[test]
fn forest() {
    let decls = "(parents : (array int) (array/int 0)) (mask : (array int) (array/int 0))";
    let geometry = fixtures().remove(0).1;
    let all: Vec<Vec<i64>> = vec![vec![], vec![0], vec![-1], vec![-1, 0, 0, 1, 1, 2], vec![5, 0, 1, 2, 3, 4], vec![1, 0, 1, 9, -4, 4], vec![-1, -1, 1, 1, 3, 2, 2, 7], vec![3, 3, 3, 3, 0, 4, 4, 5]];
    for parents in &all {
        let n = parents.len();
        let masks: Vec<Option<Vec<i64>>> = vec![
            None,
            Some(vec![1; n]),
            Some((0..n as i64).map(|i| i & 1).collect()),
            Some((0..n as i64).map(|i| if i % 3 == 1 { 0 } else { 1 }).collect()),
            Some(vec![0; n]),
        ];
        for mask in masks {
            let (body, inputs) = match &mask {
                None => ("(graph/forest parents)", vec![("parents", ints(parents))]),
                Some(m) => ("(graph/forest parents :mask mask)", vec![("parents", ints(parents)), ("mask", ints(m))]),
            };
            agree(&geometry, decls, &inputs, body, &oracle_forest(mask.as_deref(), parents));
        }
    }
    let bad = |parents: &[i64], mask: &[i64], code: &str| refused(&geometry, decls, &[("parents", ints(parents)), ("mask", ints(mask))], "(graph/forest parents :mask mask)", code);
    bad(&[0, 0], &[1], "E_GRAPH_MASK");
    bad(&[0, 0], &[1, 2], "E_GRAPH_MASK");
}
