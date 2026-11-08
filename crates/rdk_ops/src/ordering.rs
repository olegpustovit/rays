//! Stable reorder of primitives by an integer primitive attribute.
use std::sync::Mutex;

use rdk_core::{attribute, cancel, error, geometry, group, support, topology, topology_remap};
use rdk_core::error::Code;

type Slot = Mutex<Option<(Vec<i64>, Vec<i64>)>>;

fn fail(message: String) -> error::T {
    error::new("Error.Fail", Code::InvalidParameter, message)
}

fn apply_primitives(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    permutation: &[i64],
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(value);
    let source = topology::View::view(source_topology)?;
    let primitive_count = permutation.len();
    let mut offsets = vec![0_i64; primitive_count + 1];
    for primitive in 0..primitive_count {
        let source_primitive = permutation[primitive] as usize;
        offsets[primitive + 1] = offsets[primitive] + source.primitive_offsets[source_primitive + 1]
            - source.primitive_offsets[source_primitive];
    }
    let vertex_count = offsets[primitive_count] as usize;
    let mut vertex_points = Vec::with_capacity(vertex_count);
    let mut vertex_map = Vec::with_capacity(vertex_count);
    let mut kinds = vec![0_u8; primitive_count];
    if primitive_count > 0 {
        let chunk = (grain / 8).max(1);
        let ranges = ((primitive_count as i64 - 1) / chunk + 1) as usize;
        let slots: Vec<Slot> = (0..ranges).map(|_| Mutex::new(None)).collect();
        support::run_indexed_ranges(chunk, primitive_count as i64, &|range, first, last| {
            let mut points = Vec::new();
            let mut map = Vec::new();
            for primitive in first..last {
                cancel::poll(cancel_value, primitive, 4095)?;
                let source_primitive = permutation[primitive as usize] as usize;
                for source_vertex in
                    source.primitive_offsets[source_primitive]..source.primitive_offsets[source_primitive + 1]
                {
                    points.push(source.vertex_points[source_vertex as usize]);
                    map.push(source_vertex);
                }
            }
            *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) =
                Some((points, map));
            Ok(())
        })?;
        for slot in slots {
            let (points, map) = slot
                .into_inner()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .unwrap_or_default();
            vertex_points.extend(points);
            vertex_map.extend(map);
        }
        for primitive in 0..primitive_count {
            kinds[primitive] = source.primitive_kinds[permutation[primitive] as usize];
        }
    }
    let new_topology = topology::View::create_validated_owned(
        geometry::point_count(value),
        vertex_points,
        offsets,
        kinds,
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        attributes.push(match attribute::owner(&item) {
            attribute::Owner::Vertex => {
                topology_remap::attribute(cancel_value, grain, None, &vertex_map, &item)?
            }
            attribute::Owner::Primitive => {
                topology_remap::attribute(cancel_value, grain, None, permutation, &item)?
            }
            attribute::Owner::Point | attribute::Owner::Detail => item,
        });
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        groups.push(match group::owner(&item) {
            group::Owner::Vertex => topology_remap::group(cancel_value, grain, &vertex_map, &item)?,
            group::Owner::Primitive => {
                topology_remap::group(cancel_value, grain, permutation, &item)?
            }
            group::Owner::Point => item,
        });
    }
    let point_map: Vec<i64> = (0..geometry::point_count(value)).collect();
    let edge_groups = topology_remap::edge_groups(
        cancel_value,
        source_topology,
        &new_topology,
        &point_map,
        &geometry::edge_groups(value),
    )?;
    geometry::create(
        geometry::positions(value).clone(),
        new_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

/// Stable primitive reorder by an integer primitive attribute.
pub fn sort_primitives(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    name: &str,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = grain.unwrap_or(16_384);
    let run = || -> Result<geometry::T, error::T> {
        cancel::check_opt(cancel_value)?;
        let attribute_value = match geometry::find_attribute(attribute::Owner::Primitive, name, value) {
            Some(item) => item,
            None => return Err(fail(format!("missing sort attribute {name}"))),
        };
        let keys = match attribute::View::storage(&attribute_value)? {
            attribute::Storage::Int(keys) => keys.clone(),
            _ => return Err(fail(format!("sort attribute {name} must be integer"))),
        };
        let mut order: Vec<i64> = (0..keys.len() as i64).collect();
        order.sort_by(|&a, &b| keys[a as usize].cmp(&keys[b as usize]));
        apply_primitives(cancel_value, grain, &order, value)
    };
    run().map_err(|failure| error::guard(failure, "sort", Code::InvalidSort))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use rdk_core::packed;

    fn triangles() -> geometry::T {
        let positions =
            packed::Float3::of_owned(vec![0.0, 1.0, 2.0, 3.0], vec![0.0; 4], vec![0.0; 4]).unwrap();
        let topo =
            topology::polygons_owned(4, vec![0, 1, 2, 1, 2, 3, 0, 2, 3], vec![0, 3, 6, 9]).unwrap();
        let keys = attribute::create_owned(
            "k",
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new(vec![2, 0, 1])),
        )
        .unwrap();
        geometry::create(positions, topo, Some(vec![keys]), None, None).unwrap()
    }

    #[test]
    fn sorts_primitives_stably_and_reports_failures() {
        let sorted = sort_primitives(None, None, "k", &triangles()).unwrap();
        let view = topology::View::view(geometry::topology(&sorted)).unwrap();
        assert_eq!(&view.vertex_points[..], &[1, 2, 3, 0, 2, 3, 0, 1, 2]);
        let keys = geometry::find_attribute(attribute::Owner::Primitive, "k", &sorted).unwrap();
        match attribute::View::storage(&keys).unwrap() {
            attribute::Storage::Int(values) => assert_eq!(**values, vec![0, 1, 2]),
            _ => panic!("int"),
        }
        let Err(missing) = sort_primitives(None, None, "nope", &triangles()) else { panic!("expected failure") };
        assert_eq!(error::operation(&missing), "sort");
        assert_eq!(error::kind(&missing), Code::InvalidSort);
        assert_eq!(error::message(&missing), "missing sort attribute nope");
    }

    #[test]
    fn small_grain_ranges_join_in_order() {
        let tiny = sort_primitives(None, Some(8), "k", &triangles()).unwrap();
        let view = topology::View::view(geometry::topology(&tiny)).unwrap();
        assert_eq!(&view.vertex_points[..], &[1, 2, 3, 0, 2, 3, 0, 1, 2]);
    }
}
