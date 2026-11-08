use std::sync::Arc;
use rays_math::text_array::TextArray;

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, packed, support, topology, topology_index,
};
use rdk_core::error::Code;
use rdk_geom::point_clusters::Clusters;

use crate::deform;
use crate::fuse_cleanup;
use crate::fuse_reduce::{self, Position};
use crate::fuse_rules::DefaultAttributes;

fn make(operation: &str, code: Code, message: String) -> error::T {
    error::new(operation, code, message)
}

fn fail(message: &str) -> error::T {
    make("Rdk.Edge_collapse", Code::InvalidParameter, message.to_owned())
}

fn same_ragged_i64(offsets: &[i64], values: &[i64], left: usize, right: usize) -> bool {
    let (lf, ll, rf, rl) = (offsets[left], offsets[left + 1], offsets[right], offsets[right + 1]);
    ll - lf == rl - rf && (0..ll - lf).all(|local| values[(lf + local) as usize] == values[(rf + local) as usize])
}

fn same_ragged_f64(offsets: &[i64], values: &[f64], left: usize, right: usize) -> bool {
    let (lf, ll, rf, rl) = (offsets[left], offsets[left + 1], offsets[right], offsets[right + 1]);
    ll - lf == rl - rf && (0..ll - lf).all(|local| values[(lf + local) as usize] == values[(rf + local) as usize])
}

enum Connectivity {
    Always,
    Float(Arc<Vec<f64>>),
    Int(Arc<Vec<i64>>),
    Text(Arc<TextArray>),
    Float2(Arc<Vec<f64>>, Arc<Vec<f64>>),
    Float3(Arc<Vec<f64>>, Arc<Vec<f64>>, Arc<Vec<f64>>),
    Float4(Arc<Vec<f64>>, Arc<Vec<f64>>, Arc<Vec<f64>>, Arc<Vec<f64>>),
    IntArray(Arc<Vec<i64>>, Arc<Vec<i64>>),
    FloatArray(Arc<Vec<i64>>, Arc<Vec<f64>>),
}

impl Connectivity {
    fn same(&self, a: usize, b: usize) -> bool {
        match self {
            Connectivity::Always => true,
            Connectivity::Float(v) => v[a] == v[b],
            Connectivity::Int(v) => v[a] == v[b],
            Connectivity::Text(v) => v[a] == v[b],
            Connectivity::Float2(x, y) => x[a] == x[b] && y[a] == y[b],
            Connectivity::Float3(x, y, z) => x[a] == x[b] && y[a] == y[b] && z[a] == z[b],
            Connectivity::Float4(x, y, z, w) => x[a] == x[b] && y[a] == y[b] && z[a] == z[b] && w[a] == w[b],
            Connectivity::IntArray(o, v) => same_ragged_i64(o, v, a, b),
            Connectivity::FloatArray(o, v) => same_ragged_f64(o, v, a, b),
        }
    }
}

fn find(parent: &mut [usize], point: usize) -> usize {
    let mut root = point;
    while parent[root] != root {
        root = parent[root];
    }
    let mut at = point;
    while parent[at] != root {
        let next = parent[at];
        parent[at] = root;
        at = next;
    }
    root
}

fn union(parent: &mut [usize], rank: &mut [u8], left: usize, right: usize) -> bool {
    let left_root = find(parent, left);
    let right_root = find(parent, right);
    if left_root == right_root {
        return false;
    }
    let (left_rank, right_rank) = (rank[left_root], rank[right_root]);
    if left_rank < right_rank {
        parent[left_root] = right_root;
    } else if right_rank < left_rank {
        parent[right_root] = left_root;
    } else {
        parent[right_root] = left_root;
        rank[left_root] = left_rank + 1;
    }
    true
}

#[allow(clippy::too_many_arguments)]
pub fn raw(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    edges: Option<&edge_group::T>,
    connectivity_attribute: Option<&str>,
    position: Option<Position>,
    remove_degenerate_primitives: Option<bool>,
    recompute_point_normals: Option<bool>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = grain.unwrap_or(16_384);
    let position = position.unwrap_or(Position::Average);
    let remove_degenerate_primitives = remove_degenerate_primitives.unwrap_or(true);
    let recompute_point_normals = recompute_point_normals.unwrap_or(true);
    if grain <= 0 {
        return Err(make(
            "Rdk.Edge_collapse",
            Code::InvalidArgument,
            "Rdk.Edge_collapse.edge_collapse: grain must be positive".to_owned(),
        ));
    }
    let topology_value = geometry::topology(geometry_value);
    let index_value = topology_index::create(cancel_value, topology_value)?;
    let index = topology_index::View::view(&index_value);
    let edge_count = index.edge_a.len();
    let point_count = geometry::point_count(geometry_value) as usize;
    let selected = |edge: usize| match edges {
        None => true,
        Some(item) => edge_group::mem(edge as i64, item),
    };
    if let Some(item) = edges {
        if edge_group::topology_data_id(item) != topology::data_id(topology_value) {
            return Err(fail("Rdk.Edge_collapse.edge_collapse: edge selection belongs to a different topology"));
        }
        if edge_group::length(item) != edge_count as i64 {
            return Err(fail(
                "Rdk.Edge_collapse.edge_collapse: edge selection length does not match topology edge count",
            ));
        }
    }
    let connectivity = match connectivity_attribute {
        None => Connectivity::Always,
        Some(name) if support::ocaml_trim_is_empty(name) => {
            return Err(fail(
                "Rdk.Edge_collapse.edge_collapse: connectivity attribute name must not be empty",
            ));
        }
        Some(name) => {
            let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
                return Err(fail(&format!(
                    "Rdk.Edge_collapse.edge_collapse: point connectivity attribute {name:?} is missing"
                )));
            };
            match attribute::View::storage(&found)? {
                attribute::Storage::Float(v) => Connectivity::Float(v.clone()),
                attribute::Storage::Int(v) => Connectivity::Int(v.clone()),
                attribute::Storage::Text(v) => Connectivity::Text(v.clone()),
                attribute::Storage::Float2(v) => {
                    let v = packed::Float2::View::view(v);
                    Connectivity::Float2(v.x, v.y)
                }
                attribute::Storage::Float3(v) => {
                    let v = packed::Float3::View::view(v)?;
                    Connectivity::Float3(v.x, v.y, v.z)
                }
                attribute::Storage::Float4(v) => {
                    let v = packed::Float4::View::view(v);
                    Connectivity::Float4(v.x, v.y, v.z, v.w)
                }
                attribute::Storage::IntArray(v) => {
                    let v = packed::IntArray::View::view(v);
                    Connectivity::IntArray(v.offsets, v.values)
                }
                attribute::Storage::FloatArray(v) => {
                    let v = packed::FloatArray::View::view(v);
                    Connectivity::FloatArray(v.offsets, v.values)
                }
            }
        }
    };
    let mut parent: Vec<usize> = (0..point_count).collect();
    let mut rank = vec![0u8; point_count];
    let mut merged = 0i64;
    let mut selected_self_edge = false;
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge as i64, 4095)?;
        if selected(edge) {
            let a = index.edge_a[edge] as usize;
            let b = index.edge_b[edge] as usize;
            if a == b {
                selected_self_edge = true;
            } else if connectivity.same(a, b) && union(&mut parent, &mut rank, a, b) {
                merged += 1;
            }
        }
    }
    if merged == 0 {
        return if selected_self_edge && remove_degenerate_primitives {
            fuse_cleanup::apply(cancel_value, grain, true, true, false, geometry_value)
        } else {
            Ok(geometry_value.clone())
        };
    }
    let mut root_to_cluster = vec![-1i64; point_count];
    let mut of_point = vec![0i64; point_count];
    let mut cluster_count = 0i64;
    for point in 0..point_count {
        let root = find(&mut parent, point);
        if root_to_cluster[root] < 0 {
            root_to_cluster[root] = cluster_count;
            cluster_count += 1;
        }
        of_point[point] = root_to_cluster[root];
    }
    let mut sizes = vec![0i64; cluster_count as usize];
    for point in 0..point_count {
        sizes[of_point[point] as usize] += 1;
    }
    let mut offsets = vec![0i64; cluster_count as usize + 1];
    for cluster in 0..cluster_count as usize {
        offsets[cluster + 1] = offsets[cluster] + sizes[cluster];
    }
    let mut members = vec![0i64; point_count];
    let mut cursors = offsets.clone();
    for point in 0..point_count {
        let cluster = of_point[point] as usize;
        members[cursors[cluster] as usize] = point as i64;
        cursors[cluster] += 1;
    }
    let representatives: Vec<i64> =
        (0..cluster_count as usize).map(|cluster| members[offsets[cluster] as usize]).collect();
    let clusters = Clusters { count: cluster_count, of_point, offsets, members, representatives };
    let had_point_normals = geometry::find_attribute(attribute::Owner::Point, "N", geometry_value).is_some();
    let source_edge_groups = geometry::edge_groups(geometry_value);
    let reduced = fuse_reduce::apply(
        cancel_value,
        &fuse_reduce::Params {
            grain,
            position,
            weight_attribute: None,
            attributes: DefaultAttributes::KeepFirst,
            attribute_rules: Vec::new(),
            group_rules: Vec::new(),
            compact: true,
            rewire: true,
            remap_edge_groups: false,
        },
        &clusters,
        geometry_value,
    )?;
    let (output, fused_to_output) = fuse_cleanup::apply_with_mapping(
        cancel_value,
        grain,
        remove_degenerate_primitives,
        remove_degenerate_primitives,
        false,
        &reduced,
    )?;
    let mut output = output;
    if !source_edge_groups.is_empty() {
        let target_topology = geometry::topology(&output).clone();
        let target_index = topology_index::create(cancel_value, &target_topology)?;
        let source_to_output: Vec<i64> = (0..point_count)
            .map(|point| {
                let fused = clusters.of_point[point];
                if fused < 0 || fused >= fused_to_output.len() as i64 {
                    -1
                } else {
                    fused_to_output[fused as usize]
                }
            })
            .collect();
        for item in &source_edge_groups {
            let remapped = edge_group::remap(
                cancel_value, &index_value, &target_topology, &target_index, &source_to_output, item,
            )?;
            output = geometry::with_edge_group(remapped, &output)?;
        }
    }
    let output = geometry::without_attribute(attribute::Owner::Point, "N", &output);
    let output = geometry::without_attribute(attribute::Owner::Vertex, "N", &output);
    if !recompute_point_normals || !had_point_normals {
        return Ok(output);
    }
    cancel::check_opt(cancel_value)?;
    deform::normals(
        deform::Params {
            cancel: None,
            grain,
            primitives: None,
        },
        &output,
    )
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub edges: Option<edge_group::T>,
    pub connectivity_attribute: Option<String>,
    pub position: Option<Position>,
    pub remove_degenerate_primitives: Option<bool>,
    pub recompute_point_normals: Option<bool>,
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    raw(
        params.cancel.as_deref(),
        params.grain,
        params.edges.as_ref(),
        params.connectivity_attribute.as_deref(),
        params.position,
        params.remove_degenerate_primitives,
        params.recompute_point_normals,
        geometry_value,
    )
    .map_err(|failure| {
        if error::is_cancelled(&failure) {
            error::cancelled("edge_collapse")
        } else {
            make("edge_collapse", Code::InvalidTopology, error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::group;
    use rdk_core::topology::PrimitiveKind;

    fn params() -> Params {
        Params {
            cancel: None,
            grain: Some(1),
            edges: None,
            connectivity_attribute: None,
            position: None,
            remove_degenerate_primitives: None,
            recompute_point_normals: None,
        }
    }

    fn code(result: Result<geometry::T, error::T>) -> String {
        error::code(&err(result)).to_owned()
    }

    fn quad() -> geometry::T {
        let g = geom(&[0., 2., 2., 0.], &[0., 0., 2., 2.], &[0.; 4], &[0, 1, 2, 3], &[0, 4], None);
        let g = with_attr(&g, Owner::Point, "weight", Storage::Float(Arc::new(vec![0., 2., 4., 6.])));
        let g = with_attr(&g, Owner::Point, "piece", Storage::Int(Arc::new(vec![0, 0, 1, 1])));
        let g = with_attr(
            &g,
            Owner::Point,
            "label",
            Storage::Text(Arc::new(["a", "b", "c", "d"].iter().map(|s| s.as_bytes()).collect())),
        );
        let rows = packed::IntArray::create_owned(vec![0, 1, 2, 3, 4], vec![10, 11, 12, 13]).unwrap();
        let g = with_attr(&g, Owner::Point, "rows", Storage::IntArray(rows));
        let g = with_attr(&g, Owner::Point, "__rdk_edge_collapse_target", Storage::Int(Arc::new(vec![9, 8, 7, 6])));
        let g = with_attr(&g, Owner::Point, "N", float3_attr(vec![1.; 4], vec![0.; 4], vec![0.; 4]));
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Float(Arc::new(vec![10., 20., 30., 40.])));
        let g = with_attr(&g, Owner::Primitive, "material", Storage::Text(std::sync::Arc::new(vec![b"panel".to_vec()].into())));
        let g = with_attr(&g, Owner::Detail, "meta", Storage::Int(Arc::new(vec![42])));
        let ordered = group::ordered(group::Owner::Point, "ordered", 4, &[1, 0, 2]).unwrap();
        let corners = group::init(None, group::Owner::Vertex, "corners", 4, &|v| v < 3).unwrap();
        let g = geometry::with_group(ordered, &g).unwrap();
        let g = geometry::with_group(corners, &g).unwrap();
        let t = geometry::topology(&g).clone();
        let collapse = edge_group_pairs(&t, "collapse", &[(0, 1)]);
        let all = edge_group_pairs(&t, "all", &[(0, 1), (1, 2), (2, 3), (3, 0)]);
        let g = geometry::with_edge_group(collapse, &g).unwrap();
        geometry::with_edge_group(all, &g).unwrap()
    }

    fn edge_cardinality(name: &str, g: &geometry::T) -> i64 {
        edge_group::cardinality(&geometry::find_edge_group(name, g).unwrap())
    }

    #[test]
    fn collapses_one_edge_with_ancestry() {
        let source = quad();
        let collapse = geometry::find_edge_group("collapse", &source).unwrap();
        let out = run(&Params { edges: Some(collapse), ..params() }, &source).unwrap();
        assert_eq!(
            (geometry::point_count(&out), geometry::vertex_count(&out), geometry::primitive_count(&out)),
            (3, 3, 1)
        );
        let (x, y, z) = positions(&out);
        assert_eq!((x, y, z), (vec![1., 2., 0.], vec![0., 2., 2.], vec![0., 0., 0.]));
        let (vp, offsets, _) = topo(&out);
        assert_eq!((vp, offsets), (vec![0, 1, 2], vec![0, 3]));
        assert_eq!(floats(Owner::Point, "weight", &out), vec![0., 4., 6.]);
        assert_eq!(ints(Owner::Point, "piece", &out), vec![0, 1, 1]);
        assert_eq!(ints(Owner::Point, "__rdk_edge_collapse_target", &out), vec![9, 7, 6]);
        assert_eq!(floats(Owner::Vertex, "corner", &out), vec![10., 30., 40.]);
        assert_eq!((edge_cardinality("collapse", &out), edge_cardinality("all", &out)), (0, 3));
        let ordered = geometry::find_group(group::Owner::Point, "ordered", &out).unwrap();
        assert_eq!(group::ordered_elements(&ordered), Some(vec![0, 1]));
        let normal = geometry::find_attribute(Owner::Point, "N", &out).unwrap();
        let Storage::Float3(values) = attribute::View::storage(&normal).unwrap() else { panic!("storage") };
        let v = packed::Float3::View::view(values).unwrap();
        assert_eq!((v.x.to_vec(), v.y.to_vec(), v.z.to_vec()), (vec![0.; 3], vec![0.; 3], vec![1.; 3]));
    }

    #[test]
    fn connectivity_attribute_blocks_merges() {
        let source = quad();
        let t = geometry::topology(&source).clone();
        let path = edge_group_pairs(&t, "path", &[(0, 1), (1, 2)]);
        let separated = run(
            &Params { edges: Some(path.clone()), connectivity_attribute: Some("piece".to_owned()), ..params() },
            &source,
        )
        .unwrap();
        assert_eq!(geometry::point_count(&separated), 3);
        assert_eq!(positions(&separated).0, vec![1., 2., 0.]);
        assert_eq!(positions(&separated).1, vec![0., 2., 2.]);
        let ragged = run(
            &Params { edges: Some(path), connectivity_attribute: Some("rows".to_owned()), ..params() },
            &source,
        )
        .unwrap();
        assert_eq!(geometry::data_id(&ragged), geometry::data_id(&source));
    }

    #[test]
    fn options_and_identities() {
        let source = quad();
        let collapse = geometry::find_edge_group("collapse", &source).unwrap();
        let no_cleanup = run(
            &Params {
                edges: Some(collapse),
                remove_degenerate_primitives: Some(false),
                recompute_point_normals: Some(false),
                ..params()
            },
            &source,
        )
        .unwrap();
        assert_eq!((geometry::point_count(&no_cleanup), geometry::vertex_count(&no_cleanup)), (3, 4));
        assert_eq!(topo(&no_cleanup).0, vec![0, 0, 1, 2]);
        assert!(geometry::find_attribute(Owner::Point, "N", &no_cleanup).is_none());
        let without_normals = geometry::without_attribute(Owner::Point, "N", &source);
        let collapse = geometry::find_edge_group("collapse", &without_normals).unwrap();
        let invented = run(&Params { edges: Some(collapse), ..params() }, &without_normals).unwrap();
        assert!(geometry::find_attribute(Owner::Point, "N", &invented).is_none());
        let t = geometry::topology(&source).clone();
        let empty = edge_group::init(
            Some(1),
            &t,
            &topology_index::create(None, &t).unwrap(),
            "empty",
            &|_| false,
        )
        .unwrap();
        let same = run(&Params { edges: Some(empty), ..params() }, &source).unwrap();
        assert_eq!(geometry::data_id(&same), geometry::data_id(&source));
        let curve = polyline(&[(0., 0., 0.), (1., 0., 0.), (2., 0., 0.), (3., 0., 0.)]);
        let gone = run(&params(), &curve).unwrap();
        assert_eq!(
            (geometry::point_count(&gone), geometry::vertex_count(&gone), geometry::primitive_count(&gone)),
            (0, 0, 0)
        );
    }

    #[test]
    fn diagnostics() {
        let source = quad();
        let collapse = geometry::find_edge_group("collapse", &source).unwrap();
        for name in ["missing", " "] {
            assert_eq!(
                code(run(
                    &Params {
                        edges: Some(collapse.clone()),
                        connectivity_attribute: Some(name.to_owned()),
                        ..params()
                    },
                    &source
                )),
                "invalid_topology"
            );
        }
        let non_finite = geom(
            &[f64::NAN, 1.],
            &[0.; 2],
            &[0.; 2],
            &[0, 1],
            &[0, 2],
            Some(vec![PrimitiveKind::OpenPolyline]),
        );
        assert_eq!(code(run(&params(), &non_finite)), "invalid_topology");
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        assert_eq!(code(run(&Params { cancel: Some(token), ..params() }, &source)), "cancelled");
        let foreign = geometry::find_edge_group("collapse", &grid(2, 1, 1.0, true));
        assert!(foreign.is_none());
        let other = edge_group_pairs(geometry::topology(&grid(2, 1, 1.0, true)), "x", &[(0, 1)]);
        assert_eq!(code(run(&Params { edges: Some(other), ..params() }, &source)), "invalid_topology");
    }

    #[test]
    fn disjoint_edges_on_a_grid() {
        let large = grid(180, 140, 20.0, true);
        let disjoint = edge_group_where(&large, "disjoint", &|_, a, b| {
            let (low, high) = (a.min(b), a.max(b));
            high == low + 1 && low % 4 == 0
        });
        let out = exact_at_one_and_four(&|| {
            run(&Params { grain: Some(127), edges: Some(disjoint.clone()), ..params() }, &large).unwrap()
        });
        assert!(geometry::point_count(&out) < geometry::point_count(&large) && geometry::point_count(&out) > 0);
    }
}
