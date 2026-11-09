use rdk_core::{
    attribute, cancel, error, geometry, group, packed, support, topology, topology_remap,
};
use rdk_core::error::Code;

fn make(operation: &str, code: Code, message: String) -> error::T {
    error::new(operation, code, message)
}

fn compact(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    support::check_positive(grain).map_err(|_| make(
            "Rdk.Compact_points",
            Code::InvalidArgument,
            "Rdk.Compact_points.compact_points: grain must be positive".to_owned(),
        ))?;
    cancel::check_opt(cancel_value)?;
    let point_count = geometry::point_count(value) as usize;
    let source_topology_value = geometry::topology(value);
    let source = topology::view::view(source_topology_value)?;
    let mut used = vec![false; point_count];
    for (vertex, &point) in source.vertex_points.iter().enumerate() {
        cancel::poll(cancel_value, vertex as i64, 16_383)?;
        used[point as usize] = true;
    }
    let mut old_to_new = vec![-1i64; point_count];
    let mut retained = 0i64;
    for point in 0..point_count {
        cancel::poll(cancel_value, point as i64, 16_383)?;
        if used[point] {
            old_to_new[point] = retained;
            retained += 1;
        }
    }
    if retained as usize == point_count {
        return Ok(value.clone());
    }
    let new_to_old: Vec<i64> = (0..point_count).filter(|&p| used[p]).map(|p| p as i64).collect();
    let positions = packed::float3::view::view(geometry::positions(value))?;
    let px = topology_remap::select_float(cancel_value, grain, None, &new_to_old, &positions.x)?;
    let py = topology_remap::select_float(cancel_value, grain, None, &new_to_old, &positions.y)?;
    let pz = topology_remap::select_float(cancel_value, grain, None, &new_to_old, &positions.z)?;
    let positions = packed::float3::view::of_owned_exn(px, py, pz)?;
    let mut vertex_points = vec![0i64; source.vertex_points.len()];
    support::par_fill(cancel_value, grain, &mut vertex_points, &|vertex| {
        Ok(old_to_new[source.vertex_points[vertex as usize] as usize])
    })?;
    let new_topology = topology::view::create_validated_owned(
        retained,
        vertex_points,
        source.primitive_offsets.to_vec(),
        source.primitive_kinds.to_vec(),
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        if attribute::owner(&item) != attribute::Owner::Point {
            attributes.push(item);
        } else {
            attributes.push(topology_remap::attribute(cancel_value, grain, None, &new_to_old, &item)?);
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        if group::owner(&item) != group::Owner::Point {
            groups.push(item);
        } else {
            groups.push(topology_remap::group(cancel_value, grain, &new_to_old, &item)?);
        }
    }
    let edge_groups = topology_remap::edge_groups(
        cancel_value,
        source_topology_value,
        &new_topology,
        &old_to_new,
        &geometry::edge_groups(value),
    )?;
    geometry::create(positions, new_topology, Some(attributes), Some(groups), Some(edge_groups))
}

pub fn run(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    compact(cancel_value, grain.unwrap_or(16_384), value).map_err(|failure| {
        if error::is_cancelled(&failure) {
            error::cancelled("compact_points")
        } else {
            make("compact_points", Code::InvalidGeometry, error::message(&failure).to_owned())
        }
    })
}

// Small geometry builders shared by the tests of the mesh modules ported with this file.
#[cfg(test)]
pub mod fixtures {
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{attribute, edge_group, geometry, group, packed, topology, topology_index};

    pub fn geom(
        x: &[f64],
        y: &[f64],
        z: &[f64],
        vertex_points: &[i64],
        offsets: &[i64],
        kinds: Option<Vec<PrimitiveKind>>,
    ) -> geometry::T {
        let positions = packed::float3::view::of_owned_exn(x.to_vec(), y.to_vec(), z.to_vec()).unwrap();
        let kinds = kinds.unwrap_or_else(|| vec![PrimitiveKind::Polygon; offsets.len() - 1]);
        let topology = topology::create_owned(x.len() as i64, vertex_points.to_vec(), offsets.to_vec(), kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    pub fn err(result: Result<geometry::T, rdk_core::error::T>) -> rdk_core::error::T {
        match result {
            Err(failure) => failure,
            Ok(_) => panic!("expected an error"),
        }
    }

    pub fn attr(owner: attribute::Owner, name: &str, storage: attribute::Storage) -> attribute::T {
        attribute::create_owned(name, owner, storage).unwrap()
    }

    pub fn with_attr(g: &geometry::T, owner: attribute::Owner, name: &str, storage: attribute::Storage) -> geometry::T {
        geometry::with_attribute(attr(owner, name, storage), g).unwrap()
    }

    pub fn floats(owner: attribute::Owner, name: &str, g: &geometry::T) -> Vec<f64> {
        let found = geometry::find_attribute(owner, name, g).unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&found).unwrap() {
            attribute::Storage::Float(values) => values.to_vec(),
            _ => panic!("{name} is not float"),
        }
    }

    pub fn ints(owner: attribute::Owner, name: &str, g: &geometry::T) -> Vec<i64> {
        let found = geometry::find_attribute(owner, name, g).unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&found).unwrap() {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("{name} is not int"),
        }
    }

    pub fn positions(g: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        let v = packed::float3::view::view(geometry::positions(g)).unwrap();
        (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
    }

    pub fn topo(g: &geometry::T) -> (Vec<i64>, Vec<i64>, Vec<u8>) {
        let v = topology::view::view(geometry::topology(g)).unwrap();
        (v.vertex_points.to_vec(), v.primitive_offsets.to_vec(), v.primitive_kinds.to_vec())
    }

    pub fn float3_attr(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> attribute::Storage {
        attribute::Storage::Float3(packed::float3::view::of_owned_exn(x, y, z).unwrap())
    }

    // Points on a (columns+1) by (rows+1) lattice; quads or two triangles per cell, counter-clockwise.
    pub fn grid(columns: i64, rows: i64, size: f64, quads: bool) -> geometry::T {
        let stride = columns + 1;
        let (mut x, mut y) = (Vec::new(), Vec::new());
        for j in 0..=rows {
            for i in 0..=columns {
                x.push((i as f64 / columns as f64 - 0.5) * size);
                y.push((j as f64 / rows as f64 - 0.5) * size);
            }
        }
        let z = vec![0.0; x.len()];
        let (mut vp, mut offsets) = (Vec::new(), vec![0i64]);
        for j in 0..rows {
            for i in 0..columns {
                let a = j * stride + i;
                let (b, c, d) = (a + 1, a + 1 + stride, a + stride);
                if quads {
                    vp.extend_from_slice(&[a, b, c, d]);
                    offsets.push(vp.len() as i64);
                } else {
                    vp.extend_from_slice(&[a, b, c]);
                    offsets.push(vp.len() as i64);
                    vp.extend_from_slice(&[a, c, d]);
                    offsets.push(vp.len() as i64);
                }
            }
        }
        geom(&x, &y, &z, &vp, &offsets, None)
    }

    pub fn polyline(points: &[(f64, f64, f64)]) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let vp: Vec<i64> = (0..points.len() as i64).collect();
        geom(&x, &y, &z, &vp, &[0, points.len() as i64], Some(vec![PrimitiveKind::OpenPolyline]))
    }

    pub fn edge_group_where(
        g: &geometry::T,
        name: &str,
        predicate: &(dyn Fn(i64, i64, i64) -> bool + Send + Sync),
    ) -> edge_group::T {
        let topology = geometry::topology(g);
        let index = topology_index::create(None, topology).unwrap();
        let probe = index.clone();
        edge_group::init(Some(1), topology, &index, name, &|edge| {
            let (a, b) = topology_index::edge_points(&probe, edge).unwrap();
            predicate(edge, a, b)
        })
        .unwrap()
    }

    pub fn edge_group_pairs(topology_value: &topology::T, name: &str, pairs: &[(i64, i64)]) -> edge_group::T {
        let index = topology_index::create(None, topology_value).unwrap();
        let edges: Vec<i64> = pairs
            .iter()
            .map(|&(a, b)| {
                let edge = topology_index::find_edge_index(&index, a, b);
                assert!(edge >= 0, "test edge is absent");
                edge
            })
            .collect();
        edge_group::init(Some(1), topology_value, &index, name, &|edge| edges.contains(&edge)).unwrap()
    }

    fn storage_equal(left: &attribute::T, right: &attribute::T) -> bool {
        use attribute::Storage as S;
        match (attribute::view::storage(left).unwrap(), attribute::view::storage(right).unwrap()) {
            (S::Float(a), S::Float(b)) => a == b,
            (S::Int(a), S::Int(b)) => a == b,
            (S::Text(a), S::Text(b)) => a == b,
            (S::Float2(a), S::Float2(b)) => {
                let (a, b) = (packed::float2::view::view(a), packed::float2::view::view(b));
                a.x == b.x && a.y == b.y
            }
            (S::Float3(a), S::Float3(b)) => {
                let (a, b) = (packed::float3::view::view(a).unwrap(), packed::float3::view::view(b).unwrap());
                a.x == b.x && a.y == b.y && a.z == b.z
            }
            (S::Float4(a), S::Float4(b)) => {
                let (a, b) = (packed::float4::view::view(a), packed::float4::view::view(b));
                a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w
            }
            (S::IntArray(a), S::IntArray(b)) => {
                let (a, b) = (packed::int_array::view::view(a), packed::int_array::view::view(b));
                a.offsets == b.offsets && a.values == b.values
            }
            (S::FloatArray(a), S::FloatArray(b)) => {
                let (a, b) = (packed::float_array::view::view(a), packed::float_array::view::view(b));
                a.offsets == b.offsets && a.values == b.values
            }
            _ => false,
        }
    }

    /// Runs `work` under `domains` worker domains and returns its geometry.
    pub fn at_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot = std::cell::Cell::new(None);
        parallel::run_unit(Some(domains), &|| {
            slot.set(Some(work()));
            Ok(())
        })
        .unwrap();
        slot.take().unwrap()
    }

    /// The one-domain result of `work`, after asserting four domains give the same geometry.
    pub fn exact_at_one_and_four(work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let one = at_domains(1, work);
        let four = at_domains(4, work);
        assert!(geometry_signature(&one) == geometry_signature(&four), "one/four-domain output drift");
        one
    }

    /// Runs `work` under `domains` worker domains and returns its value.
    pub fn at_domains_strings(domains: i64, work: &(dyn Fn() -> Vec<String> + Sync)) -> Vec<String> {
        let slot = std::cell::Cell::new(None);
        parallel::run_unit(Some(domains), &|| {
            slot.set(Some(work()));
            Ok(())
        })
        .unwrap();
        slot.take().unwrap()
    }

    /// The one-domain value of `work`, after asserting four domains give an equal value.
    pub fn same_at_one_and_four_strings(work: &(dyn Fn() -> Vec<String> + Sync)) -> Vec<String> {
        let one = at_domains_strings(1, work);
        let four = at_domains_strings(4, work);
        assert!(one == four, "one/four-domain output drift");
        one
    }

    /// Bit-exact text of positions, topology, attributes and groups (NaN-safe, unlike `geometry_equal`).
    pub fn geometry_signature(g: &geometry::T) -> String {
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
    pub fn attribute_signature(item: &attribute::T) -> String {
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

    pub fn geometry_equal(left: &geometry::T, right: &geometry::T) -> bool {
        if positions(left) != positions(right) || topo(left) != topo(right) {
            return false;
        }
        if geometry::point_count(left) != geometry::point_count(right) {
            return false;
        }
        let (la, ra) = (geometry::attributes(left), geometry::attributes(right));
        if la.len() != ra.len()
            || !la.iter().zip(&ra).all(|(l, r)| {
                attribute::owner(l) == attribute::owner(r)
                    && attribute::name(l) == attribute::name(r)
                    && storage_equal(l, r)
            })
        {
            return false;
        }
        let (lg, rg) = (geometry::groups(left), geometry::groups(right));
        if lg.len() != rg.len()
            || !lg.iter().zip(&rg).all(|(l, r)| {
                group::owner(l) == group::owner(r)
                    && group::name(l) == group::name(r)
                    && group::ordered_elements(l) == group::ordered_elements(r)
                    && group::length(l) == group::length(r)
                    && (0..group::length(l)).all(|e| group::mem(e, l) == group::mem(e, r))
            })
        {
            return false;
        }
        let (le, re) = (geometry::edge_groups(left), geometry::edge_groups(right));
        le.len() == re.len()
            && le.iter().zip(&re).all(|(l, r)| {
                edge_group::name(l) == edge_group::name(r)
                    && edge_group::length(l) == edge_group::length(r)
                    && (0..edge_group::length(l)).all(|e| edge_group::mem(e, l) == edge_group::mem(e, r))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::fixtures::*;
    use super::*;
    use std::sync::Arc;
    use rdk_core::attribute::{Owner, Storage};

    #[test]
    fn parity_signature_includes_tuple_bits_group_order_and_edges() {
        let source = grid(1, 1, 2.0, true);
        for dimension in [2, 3, 4] {
            let tuple = |first| match dimension {
                2 => Storage::Float2(packed::float2::of_owned(vec![first; 4], vec![0.0; 4]).unwrap()),
                3 => Storage::Float3(packed::float3::of_owned(vec![first; 4], vec![0.0; 4], vec![0.0; 4]).unwrap()),
                _ => Storage::Float4(packed::float4::of_owned(vec![first; 4], vec![0.0; 4], vec![0.0; 4], vec![0.0; 4]).unwrap()),
            };
            let left = with_attr(&source, Owner::Point, "tuple", tuple(0.0));
            let right = with_attr(&source, Owner::Point, "tuple", tuple(-0.0));
            assert_ne!(geometry_signature(&left), geometry_signature(&right));
        }
        let ordered = |members: &[i64]| geometry::with_group(
            group::ordered(group::Owner::Point, "ordered", 4, members).unwrap(), &source,
        ).unwrap();
        assert_ne!(geometry_signature(&ordered(&[0, 1])), geometry_signature(&ordered(&[1, 0])));
        let with_edge = geometry::with_edge_group(
            edge_group_pairs(geometry::topology(&source), "edge", &[(0, 1)]), &source,
        ).unwrap();
        assert_ne!(geometry_signature(&source), geometry_signature(&with_edge));
    }

    #[test]
    fn compacts_unused_points_and_remaps_payload() {
        // Points 1 and 3 are unreferenced.
        let g = geom(&[0., 9., 1., 9., 0.], &[0., 9., 0., 9., 1.], &[0.; 5], &[0, 2, 4], &[0, 3], None);
        let g = with_attr(&g, Owner::Point, "id", Storage::Int(Arc::new(vec![10, 11, 12, 13, 14])));
        let out = run(None, None, &g).unwrap();
        assert_eq!(geometry::point_count(&out), 3);
        assert_eq!(ints(Owner::Point, "id", &out), vec![10, 12, 14]);
        assert_eq!(topo(&out).0, vec![0, 1, 2]);
        assert_eq!(positions(&out).0, vec![0., 1., 0.]);
        // Nothing to drop returns the same geometry.
        let again = run(None, None, &out).unwrap();
        assert_eq!(geometry::data_id(&again), geometry::data_id(&out));
    }

    #[test]
    fn cancellation_and_grain_are_diagnosed() {
        let g = grid(2, 2, 1.0, true);
        let token = cancel::create();
        cancel::cancel(&token);
        let failure = err(run(Some(&token), None, &g));
        assert_eq!(error::code(&failure), Code::Cancelled);
        assert_eq!(error::code(&err(run(None, Some(0), &g))).as_str(), "invalid_geometry");
    }

    #[test]
    fn parallel_exact() {
        let g = grid(150, 150, 2.0, true);
        let (x, y, z) = positions(&g);
        let (vertex_points, offsets, _) = topo(&g);
        let shift = |v: &Vec<f64>| std::iter::once(9.0).chain(v.iter().copied()).collect::<Vec<f64>>();
        let vp: Vec<i64> = vertex_points.iter().map(|p| p + 1).collect();
        let g = geom(&shift(&x), &shift(&y), &shift(&z), &vp, &offsets, None);
        let count = geometry::point_count(&g);
        let g = with_attr(&g, Owner::Point, "id", Storage::Int(Arc::new((0..count).collect())));
        let out = exact_at_one_and_four(&|| run(None, Some(1000), &g).unwrap());
        assert_eq!(geometry::point_count(&out), count - 1);
    }
}
