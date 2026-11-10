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
    let new_topology = topology::view::create_validated_shared(
        retained,
        vertex_points,
        source.primitive_offsets.clone(),
        source.primitive_kinds,
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

pub(crate) fn run(
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
#[allow(dead_code)]
pub mod fixtures {
    include!("../../rdk_geom/src/test_support.rs");
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
