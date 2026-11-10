// Geometry builders shared by geom and ops tests.
use rdk_core::topology::PrimitiveKind;
use rdk_core::{attribute, edge_group, geometry, group, packed, topology, topology_index};

pub(crate) fn geom(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    vertex_points: &[u32],
    offsets: &[u32],
    kinds: Option<Vec<PrimitiveKind>>,
) -> geometry::T {
    let positions = packed::float3::view::of_owned_exn(x.to_vec(), y.to_vec(), z.to_vec()).unwrap();
    let kinds = kinds.unwrap_or_else(|| vec![PrimitiveKind::Polygon; offsets.len() - 1]);
    let topology = topology::create_owned(x.len() as i64, vertex_points.to_vec(), offsets.to_vec(), kinds).unwrap();
    geometry::create(positions, topology, None, None, None).unwrap()
}

pub(crate) fn err(result: Result<geometry::T, rdk_core::error::T>) -> rdk_core::error::T {
    match result {
        Err(failure) => failure,
        Ok(_) => panic!("expected an error"),
    }
}

pub(crate) fn attr(owner: attribute::Owner, name: &str, storage: attribute::Storage) -> attribute::T {
    attribute::create_owned(name, owner, storage).unwrap()
}

pub(crate) fn with_attr(g: &geometry::T, owner: attribute::Owner, name: &str, storage: attribute::Storage) -> geometry::T {
    geometry::with_attribute(attr(owner, name, storage), g).unwrap()
}

pub(crate) fn floats(owner: attribute::Owner, name: &str, g: &geometry::T) -> Vec<f64> {
    let found = geometry::find_attribute(owner, name, g).unwrap_or_else(|| panic!("missing {name}"));
    match attribute::view::storage(&found).unwrap() {
        attribute::Storage::Float(values) => values.to_vec(),
        _ => panic!("{name} is not float"),
    }
}

pub(crate) fn ints(owner: attribute::Owner, name: &str, g: &geometry::T) -> Vec<i64> {
    let found = geometry::find_attribute(owner, name, g).unwrap_or_else(|| panic!("missing {name}"));
    match attribute::view::storage(&found).unwrap() {
        attribute::Storage::Int(values) => values.to_vec(),
        _ => panic!("{name} is not int"),
    }
}

pub(crate) fn positions(g: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let v = packed::float3::view::view(geometry::positions(g)).unwrap();
    (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
}

pub(crate) fn topo(g: &geometry::T) -> (Vec<u32>, Vec<u32>, Vec<u8>) {
    let v = topology::view::view(geometry::topology(g)).unwrap();
    (v.vertex_points.to_vec(), v.primitive_offsets.to_vec(), v.primitive_kinds.to_vec())
}

pub(crate) fn float3_attr(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> attribute::Storage {
    attribute::Storage::Float3(packed::float3::view::of_owned_exn(x, y, z).unwrap())
}

// Points on a (columns+1) by (rows+1) lattice; quads or two triangles per cell, counter-clockwise.
pub(crate) fn grid(columns: i64, rows: i64, size: f64, quads: bool) -> geometry::T {
    let stride = columns + 1;
    let (mut x, mut y) = (Vec::new(), Vec::new());
    for j in 0..=rows {
        for i in 0..=columns {
            x.push((i as f64 / columns as f64 - 0.5) * size);
            y.push((j as f64 / rows as f64 - 0.5) * size);
        }
    }
    let z = vec![0.0; x.len()];
    let (mut vp, mut offsets) = (Vec::new(), vec![0u32]);
    for j in 0..rows {
        for i in 0..columns {
            let a = (j * stride + i) as u32;
            let (b, c, d) = (a + 1, a + 1 + stride as u32, a + stride as u32);
            if quads {
                vp.extend_from_slice(&[a, b, c, d]);
                offsets.push(vp.len() as u32);
            } else {
                vp.extend_from_slice(&[a, b, c]);
                offsets.push(vp.len() as u32);
                vp.extend_from_slice(&[a, c, d]);
                offsets.push(vp.len() as u32);
            }
        }
    }
    geom(&x, &y, &z, &vp, &offsets, None)
}

pub(crate) fn polyline(points: &[(f64, f64, f64)]) -> geometry::T {
    let x: Vec<f64> = points.iter().map(|p| p.0).collect();
    let y: Vec<f64> = points.iter().map(|p| p.1).collect();
    let z: Vec<f64> = points.iter().map(|p| p.2).collect();
    let vp: Vec<u32> = (0..points.len() as u32).collect();
    geom(&x, &y, &z, &vp, &[0, points.len() as u32], Some(vec![PrimitiveKind::OpenPolyline]))
}

pub(crate) fn edge_group_where(
    g: &geometry::T,
    name: &str,
    predicate: &(dyn Fn(i64, i64, i64) -> bool + Send + Sync),
) -> edge_group::T {
    let topology = geometry::topology(g);
    let index = topology_index::create(None, topology).unwrap();
    let probe = index.clone();
    edge_group::init(Some(1), topology, &index, name, &|edge| Ok({
        let (a, b) = topology_index::edge_points(&probe, edge).unwrap();
        predicate(edge, a, b)
    }))
    .unwrap()
}

pub(crate) fn edge_group_pairs(topology_value: &topology::T, name: &str, pairs: &[(i64, i64)]) -> edge_group::T {
    let index = topology_index::create(None, topology_value).unwrap();
    let edges: Vec<i64> = pairs
        .iter()
        .map(|&(a, b)| {
            let edge = topology_index::find_edge_index(&index, a, b);
            assert!(edge >= 0, "test edge is absent");
            edge
        })
        .collect();
    edge_group::init(Some(1), topology_value, &index, name, &|edge| Ok(edges.contains(&edge))).unwrap()
}


/// Runs `work` under `domains` worker domains and returns its geometry.
pub(crate) use parallel::with_domains as at_domains;

/// The one-domain result of `work`, after asserting four domains give the same geometry.
pub(crate) fn exact_at_one_and_four(work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
    let one = at_domains(1, work);
    let four = at_domains(4, work);
    assert!(geometry_signature(&one) == geometry_signature(&four), "one/four-domain output drift");
    one
}

/// Runs `work` under `domains` worker domains and returns its value.
pub(crate) use parallel::with_domains as at_domains_strings;

/// The one-domain value of `work`, after asserting four domains give an equal value.
pub(crate) fn same_at_one_and_four_strings(work: &(dyn Fn() -> Vec<String> + Sync)) -> Vec<String> {
    let one = at_domains_strings(1, work);
    let four = at_domains_strings(4, work);
    assert!(one == four, "one/four-domain output drift");
    one
}

/// Bit-exact text of positions, topology, attributes and groups (NaN-safe, unlike `geometry_equal`).
pub(crate) fn geometry_signature(g: &geometry::T) -> String {
    let (x, y, z) = positions(g);
    let bits = |v: &Vec<f64>| v.iter().map(|f| f.to_bits()).collect::<Vec<_>>();
    let attributes: Vec<String> = geometry::attributes(g).iter().map(|a| format!("{:?}{}", attribute::owner(a), attribute_signature(a))).collect();
    let groups: Vec<String> = geometry::groups(g)
        .iter()
        .map(|g| format!("{:?}:{}:{}:{:?}:{:?}", group::owner(g), group::name(g), group::length(g),
            group::ordered_elements(g), (0..group::length(g)).filter(|&e| group::mem(e, g)).collect::<Vec<_>>()))
        .collect();
    let edges: Vec<String> = geometry::edge_groups(g).iter().map(|group| {
        format!("{}:{}:{:?}", edge_group::name(group), edge_group::length(group),
            (0..edge_group::length(group)).filter(|&edge| edge_group::mem(edge, group)).collect::<Vec<_>>())
    }).collect();
    format!("{}:{:?}{:?}{:?}{:?}{attributes:?}{groups:?}{edges:?}", geometry::point_count(g), bits(&x), bits(&y), bits(&z), topo(g))
}

/// Exact text of a stored attribute (floats as bit patterns) for equality checks.
pub(crate) fn attribute_signature(item: &attribute::T) -> String {
    let storage = match attribute::view::storage(item).unwrap() {
        attribute::Storage::Float(v) => format!("{:?}", v.iter().map(|x| x.to_bits()).collect::<Vec<_>>()),
        attribute::Storage::Int(v) => format!("{v:?}"),
        attribute::Storage::Text(v) => format!("{v:?}"),
        attribute::Storage::FloatArray(a) => {
            let v = packed::float_array::view::view(a);
            format!("{:?}{:?}", v.offsets, v.values.iter().map(|x| x.to_bits()).collect::<Vec<_>>())
        }
        attribute::Storage::IntArray(a) => {
            let v = packed::int_array::view::view(a);
            format!("{:?}{:?}", v.offsets, v.values)
        }
        attribute::Storage::Float2(values) => {
            let v = packed::float2::view::view(values);
            format!("{:?}{:?}", v.x.iter().map(|x| x.to_bits()).collect::<Vec<_>>(),
                v.y.iter().map(|x| x.to_bits()).collect::<Vec<_>>())
        }
        attribute::Storage::Float3(values) => {
            let v = packed::float3::view::view(values).unwrap();
            format!("{:?}{:?}{:?}", v.x.iter().map(|x| x.to_bits()).collect::<Vec<_>>(),
                v.y.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), v.z.iter().map(|x| x.to_bits()).collect::<Vec<_>>())
        }
        attribute::Storage::Float4(values) => {
            let v = packed::float4::view::view(values);
            format!("{:?}{:?}{:?}{:?}", v.x.iter().map(|x| x.to_bits()).collect::<Vec<_>>(),
                v.y.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), v.z.iter().map(|x| x.to_bits()).collect::<Vec<_>>(),
                v.w.iter().map(|x| x.to_bits()).collect::<Vec<_>>())
        }
    };
    format!("{}:{}:{storage}", attribute::kind_name(item), attribute::name(item))
}
