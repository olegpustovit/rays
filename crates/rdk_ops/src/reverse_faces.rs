use crate::subdivide_refine::AInts;
use rdk_core::{attribute, cancel, error, geometry, group, support, topology, topology_remap};

fn make(operation: &str, code: &str, message: String) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message, None).unwrap_or_else(|e| e)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    ReverseVertices,
    ShiftVertices(i64),
}

fn reverse(
    cancel_value: Option<&cancel::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = 16_384i64;
    let operation = Operation::ReverseVertices;
    let primitives: Option<&group::T> = None;
    let invalid = |m: &str| make("reverse", "invalid_topology", m.to_owned());
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(geometry_value);
    let source = topology::View::view(topology_value)?;
    let vertex_count = topology::vertex_count(topology_value) as usize;
    let primitive_count = topology::primitive_count(topology_value) as usize;
    if let Some(selection) = primitives {
        if group::owner(selection) != group::Owner::Primitive {
            return Err(invalid("Rdk_mesh.Reverse_faces.reverse: selection must own primitives"));
        }
        if group::length(selection) != primitive_count as i64 {
            return Err(invalid(
                "Rdk_mesh.Reverse_faces.reverse: selection length does not match primitive count",
            ));
        }
    }
    let selected = |primitive: usize| match primitives {
        None => true,
        Some(selection) => group::mem(primitive as i64, selection),
    };
    let normalized_shift = |count: i64, offset: i64| {
        let shift = offset % count;
        if shift < 0 { shift + count } else { shift }
    };
    let changes = match operation {
        Operation::ReverseVertices => match primitives {
            None => primitive_count > 0,
            Some(selection) => group::cardinality(selection) > 0,
        },
        Operation::ShiftVertices(0) => false,
        Operation::ShiftVertices(offset) => (0..primitive_count).any(|primitive| {
            selected(primitive) && {
                let count = source.primitive_offsets[primitive + 1] - source.primitive_offsets[primitive];
                normalized_shift(count, offset) != 0
            }
        }),
    };
    if !changes {
        return Ok(geometry_value.clone());
    }
    let full_selection = match primitives {
        None => true,
        Some(selection) => group::cardinality(selection) == primitive_count as i64,
    };
    let vertex_points = AInts::new(vertex_count as i64, 0);
    let vertex_map = AInts::new(vertex_count as i64, 0);
    if !full_selection {
        for vertex in 0..vertex_count {
            vertex_points.set(vertex as i64, source.vertex_points[vertex]);
            vertex_map.set(vertex as i64, vertex as i64);
        }
    }
    let average_primitive_size = if primitive_count == 0 { 1 } else { (vertex_count / primitive_count).max(1) };
    let primitive_grain = (grain / average_primitive_size as i64).max(1);
    support::parallel_for(cancel_value, primitive_grain, primitive_count as i64, &|primitive| {
        let primitive = primitive as usize;
        if !selected(primitive) {
            return Ok(());
        }
        let first = source.primitive_offsets[primitive];
        let count = source.primitive_offsets[primitive + 1] - first;
        let shift = match operation {
            Operation::ReverseVertices => 0,
            Operation::ShiftVertices(offset) => normalized_shift(count, offset),
        };
        for local in 0..count {
            let old_local = match operation {
                Operation::ReverseVertices => count - local - 1,
                Operation::ShiftVertices(_) => (local + shift) % count,
            };
            let vertex = first + local;
            let old_vertex = first + old_local;
            vertex_points.set(vertex, source.vertex_points[old_vertex as usize]);
            vertex_map.set(vertex, old_vertex);
        }
        Ok(())
    })?;
    let vertex_points = vertex_points.into_vec();
    let vertex_map = vertex_map.into_vec();
    let new_topology = topology::View::create_validated_owned(
        geometry::point_count(geometry_value),
        vertex_points,
        source.primitive_offsets.to_vec(),
        source.primitive_kinds.to_vec(),
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        let owner = attribute::owner(&item);
        if operation == Operation::ReverseVertices
            && attribute::name(&item) == "N"
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

pub fn run(
    cancel_value: Option<&cancel::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    reverse(cancel_value, geometry_value).map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            make("reverse", "cancelled", "geometry operation was cancelled".to_owned())
        } else {
            make("reverse", "invalid_topology", error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    #[test]
    fn reverse_flips_winding_and_remaps_vertex_data() {
        let g = grid(1, 1, 2.0, true);
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Int(vec![10, 11, 12, 13]));
        let g = with_attr(&g, Owner::Point, "N", float3_attr(vec![0.; 4], vec![0.; 4], vec![1.; 4]));
        let g = with_attr(&g, Owner::Point, "keep", Storage::Int(vec![0, 1, 2, 3]));
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
        assert_eq!(error::code(&failure), "cancelled");
        assert_eq!(error::operation(&failure), "reverse");
    }

    #[test]
    fn parallel_exact() {
        let g = grid(200, 200, 2.0, true);
        let n = geometry::topology(&g);
        let corners = topology::vertex_count(n);
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Int((0..corners).collect()));
        let out = exact_at_one_and_four(&|| run(None, &g).unwrap());
        assert_ne!(topo(&out).0, topo(&g).0);
    }
}
