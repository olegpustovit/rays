use rdk_core::support::Windows;
use rdk_core::{attribute, cancel, error, geometry, group, support, topology, topology_remap};
use rdk_core::error::Code;

fn make(operation: &str, code: Code, message: String) -> error::T {
    error::new(operation, code, message)
}

fn reverse(
    cancel_value: Option<&cancel::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = 16_384i64;
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(geometry_value);
    let source = topology::view::view(topology_value)?;
    let vertex_count = topology::vertex_count(topology_value) as usize;
    let primitive_count = topology::primitive_count(topology_value) as usize;
    if primitive_count == 0 {
        return Ok(geometry_value.clone());
    }
    let (mut vertex_points, mut vertex_map) = (vec![0; vertex_count], vec![0; vertex_count]);
    let average_primitive_size = (vertex_count / primitive_count).max(1);
    let primitive_grain = (grain / average_primitive_size as i64).max(1);
    {
        // Primitive range `r` owns the vertices of its primitives: a disjoint window per range.
        let bound = |range: i64| {
            source.primitive_offsets[((range * primitive_grain) as usize).min(primitive_count)] as usize
        };
        let ranges = support::range_count(primitive_count as i64, primitive_grain);
        let points = Windows::new(&mut vertex_points, ranges, bound);
        let map = Windows::new(&mut vertex_map, ranges, bound);
        support::run_indexed_ranges(primitive_grain, primitive_count as i64, &|range, first, last| {
            let (points, map) = (points.take(range), map.take(range));
            let base = source.primitive_offsets[first as usize];
            for primitive in first..last {
                support::poll(cancel_value, primitive)?;
                let primitive = primitive as usize;
                let first = source.primitive_offsets[primitive];
                let count = source.primitive_offsets[primitive + 1] - first;
                for local in 0..count {
                    let old_local = count - local - 1;
                    let at = (first + local - base) as usize;
                    let old_vertex = first + old_local;
                    points[at] = source.vertex_points[old_vertex as usize];
                    map[at] = old_vertex;
                }
            }
            Ok(())
        })?;
    }
    let new_topology = topology::view::create_validated_owned(
        geometry::point_count(geometry_value),
        vertex_points,
        source.primitive_offsets.to_vec(),
        source.primitive_kinds.to_vec(),
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        let owner = attribute::owner(&item);
        if attribute::name(&item) == "N"
            && (owner == attribute::Owner::Point || owner == attribute::Owner::Vertex)
        {
            continue;
        }
        if owner != attribute::Owner::Vertex {
            attributes.push(item);
        } else {
            attributes.push(topology_remap::attribute(cancel_value, grain, None, &vertex_map, &item)?);
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        if group::owner(&item) != group::Owner::Vertex {
            groups.push(item);
        } else {
            groups.push(topology_remap::group(cancel_value, grain, &vertex_map, &item)?);
        }
    }
    let point_map: Vec<i64> = (0..geometry::point_count(geometry_value)).collect();
    let edge_groups = topology_remap::edge_groups(
        cancel_value,
        topology_value,
        &new_topology,
        &point_map,
        &geometry::edge_groups(geometry_value),
    )?;
    geometry::create(
        geometry::positions(geometry_value).clone(),
        new_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

pub(crate) fn run(
    cancel_value: Option<&cancel::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    reverse(cancel_value, geometry_value).map_err(|failure| {
        if error::is_cancelled(&failure) {
            error::cancelled("reverse")
        } else {
            make("reverse", Code::InvalidTopology, error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    #[test]
    fn reverse_flips_winding_and_remaps_vertex_data() {
        let g = grid(1, 1, 2.0, true);
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Int(Arc::new(vec![10, 11, 12, 13])));
        let g = with_attr(&g, Owner::Point, "N", float3_attr(vec![0.; 4], vec![0.; 4], vec![1.; 4]));
        let g = with_attr(&g, Owner::Point, "keep", Storage::Int(Arc::new(vec![0, 1, 2, 3])));
        let ordered = group::ordered(group::Owner::Vertex, "corners", 4, &[0, 1]).unwrap();
        let g = geometry::with_group(ordered, &g).unwrap();
        let out = run(None, &g).unwrap();
        assert_eq!(topo(&out).0, vec![2, 3, 1, 0]);
        assert_eq!(ints(Owner::Vertex, "corner", &out), vec![13, 12, 11, 10]);
        assert!(geometry::find_attribute(Owner::Point, "N", &out).is_none());
        assert_eq!(ints(Owner::Point, "keep", &out), vec![0, 1, 2, 3]);
        let corners = geometry::find_group(group::Owner::Vertex, "corners", &out).unwrap();
        assert!((0..4).map(|v| group::mem(v, &corners)).collect::<Vec<_>>() == vec![false, false, true, true]);
        // Reversing twice restores the winding.
        let back = run(None, &out).unwrap();
        assert_eq!(topo(&back).0, vec![0, 1, 3, 2]);
    }

    #[test]
    fn reverse_of_empty_geometry_is_identity_and_cancel_is_reported() {
        let empty = geom(&[0.], &[0.], &[0.], &[], &[0], None);
        let out = run(None, &empty).unwrap();
        assert_eq!(geometry::data_id(&out), geometry::data_id(&empty));
        let token = cancel::create();
        cancel::cancel(&token);
        let failure = err(run(Some(&token), &grid(1, 1, 1.0, true)));
        assert_eq!(error::code(&failure), Code::Cancelled);
        assert_eq!(error::operation(&failure), "reverse");
    }

    #[test]
    fn parallel_exact() {
        let g = grid(200, 200, 2.0, true);
        let n = geometry::topology(&g);
        let corners = topology::vertex_count(n);
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Int(Arc::new((0..corners).collect())));
        let out = exact_at_one_and_four(&|| run(None, &g).unwrap());
        assert_ne!(topo(&out).0, topo(&g).0);
    }
}
