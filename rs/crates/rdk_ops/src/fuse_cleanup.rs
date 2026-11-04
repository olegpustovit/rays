use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, topology, topology_index, topology_remap,
};

const OCAML_MAX_INT: i64 = (1i64 << 62) - 1;

fn fail(message: &str) -> error::T {
    error::make(
        "Rdk_mesh.Fuse_cleanup".to_owned(),
        "invalid_parameter".to_owned(),
        format!("Rdk_mesh.Fuse_cleanup.fuse: {message}"),
        None,
    )
    .unwrap_or_else(|e| e)
}

fn minimum_vertices(kind: u8) -> i64 {
    if kind == 1 { 2 } else { 3 }
}

struct Maps {
    point_map: Vec<i64>,
    vertex_map: Vec<i64>,
    primitive_map: Vec<i64>,
    point_identity: bool,
    vertex_identity: bool,
    primitive_identity: bool,
}

fn remap_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    topology_changed: bool,
    maps: &Maps,
    item: &attribute::T,
) -> Result<Option<attribute::T>, error::T> {
    let owner = attribute::owner(item);
    if topology_changed
        && attribute::name(item) == "N"
        && (owner == attribute::Owner::Point || owner == attribute::Owner::Vertex)
    {
        return Ok(None);
    }
    let (mapping, identity) = match owner {
        attribute::Owner::Point => (&maps.point_map[..], maps.point_identity),
        attribute::Owner::Vertex => (&maps.vertex_map[..], maps.vertex_identity),
        attribute::Owner::Primitive => (&maps.primitive_map[..], maps.primitive_identity),
        attribute::Owner::Detail => return Ok(Some(item.clone())),
    };
    if identity {
        return Ok(Some(item.clone()));
    }
    topology_remap::attribute(cancel_value, grain, None, mapping, item).map(Some)
}

fn remap_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    maps: &Maps,
    item: &group::T,
) -> Result<group::T, error::T> {
    let (mapping, identity) = match group::owner(item) {
        group::Owner::Point => (&maps.point_map[..], maps.point_identity),
        group::Owner::Vertex => (&maps.vertex_map[..], maps.vertex_identity),
        group::Owner::Primitive => (&maps.primitive_map[..], maps.primitive_identity),
    };
    if identity {
        Ok(item.clone())
    } else {
        topology_remap::group(cancel_value, grain, mapping, item)
    }
}

fn identity_mapping(mapping: &[i64], source_count: usize) -> bool {
    mapping.len() == source_count && mapping.iter().enumerate().all(|(index, &value)| value == index as i64)
}

pub fn apply_with_mapping(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    remove_degenerate_primitives: bool,
    remove_unused_points_from_degenerate_primitives: bool,
    remove_all_unused_points: bool,
    geometry_value: &geometry::T,
) -> Result<(geometry::T, Vec<i64>), error::T> {
    cancel::check_opt(cancel_value)?;
    let source_topology_value = geometry::topology(geometry_value);
    let source = topology::View::view(source_topology_value)?;
    let source_points = source.point_count as usize;
    let source_vertices = source.vertex_points.len();
    let source_primitives = source.primitive_kinds.len();
    if !remove_degenerate_primitives && !remove_all_unused_points {
        return Ok((geometry_value.clone(), (0..source_points as i64).collect()));
    }
    let mut keep_vertex = vec![false; source_vertices];
    let mut retained_counts = vec![0i64; source_primitives];
    // ponytail: serial primitive loops; every primitive writes only its own slots, so results match the parallel original.
    for primitive in 0..source_primitives {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let first = source.primitive_offsets[primitive];
        let last = source.primitive_offsets[primitive + 1];
        if remove_degenerate_primitives {
            let mut retained = 0i64;
            let mut previous_point = -1i64;
            let mut first_vertex = -1i64;
            let mut last_vertex = -1i64;
            for vertex in first..last {
                let point = source.vertex_points[vertex as usize];
                if retained == 0 || point != previous_point {
                    keep_vertex[vertex as usize] = true;
                    if retained == 0 {
                        first_vertex = vertex;
                    }
                    last_vertex = vertex;
                    previous_point = point;
                    retained += 1;
                }
            }
            let kind = source.primitive_kinds[primitive];
            if kind != 1
                && retained > 1
                && source.vertex_points[first_vertex as usize] == source.vertex_points[last_vertex as usize]
            {
                keep_vertex[last_vertex as usize] = false;
                retained -= 1;
            }
            if retained >= minimum_vertices(kind) {
                retained_counts[primitive] = retained;
            }
        } else {
            retained_counts[primitive] = last - first;
            for vertex in first..last {
                keep_vertex[vertex as usize] = true;
            }
        }
    }
    let mut primitive_target = vec![-1i64; source_primitives];
    let mut output_vertex_first = vec![0i64; source_primitives];
    let mut output_primitives = 0i64;
    let mut output_vertices = 0i64;
    for primitive in 0..source_primitives {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let count = retained_counts[primitive];
        if count > 0 {
            if output_vertices > OCAML_MAX_INT - count {
                return Err(fail("cleanup vertex cardinality exceeds integer limits"));
            }
            primitive_target[primitive] = output_primitives;
            output_vertex_first[primitive] = output_vertices;
            output_primitives += 1;
            output_vertices += count;
        }
    }
    let mut vertex_map = vec![0i64; output_vertices as usize];
    let mut primitive_map = vec![0i64; output_primitives as usize];
    let mut vertex_points = vec![0i64; output_vertices as usize];
    let mut primitive_offsets = vec![0i64; output_primitives as usize + 1];
    let mut primitive_kinds = vec![0u8; output_primitives as usize];
    for primitive in 0..source_primitives {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let target = primitive_target[primitive];
        if target >= 0 {
            let target = target as usize;
            let mut output = output_vertex_first[primitive] as usize;
            primitive_map[target] = primitive as i64;
            primitive_kinds[target] = source.primitive_kinds[primitive];
            for vertex in source.primitive_offsets[primitive]..source.primitive_offsets[primitive + 1] {
                if keep_vertex[vertex as usize] {
                    vertex_map[output] = vertex;
                    vertex_points[output] = source.vertex_points[vertex as usize];
                    output += 1;
                }
            }
            primitive_offsets[target + 1] = output as i64;
        }
    }
    let (point_map, old_to_new): (Vec<i64>, Vec<i64>) =
        if !remove_all_unused_points && !remove_unused_points_from_degenerate_primitives {
            ((0..source_points as i64).collect(), (0..source_points as i64).collect())
        } else {
            let mut used_after = vec![false; source_points];
            for &point in &vertex_points {
                used_after[point as usize] = true;
            }
            let mut used_before = Vec::new();
            if !remove_all_unused_points {
                used_before = vec![false; source_points];
                for &point in source.vertex_points.iter() {
                    used_before[point as usize] = true;
                }
            }
            let mut retained = 0i64;
            let mut old_to_new = vec![-1i64; source_points];
            for point in 0..source_points {
                cancel::poll(cancel_value, point as i64, 16383)?;
                let keep = if remove_all_unused_points {
                    used_after[point]
                } else {
                    used_after[point] || !used_before[point]
                };
                if keep {
                    old_to_new[point] = retained;
                    retained += 1;
                }
            }
            let point_map: Vec<i64> = (0..source_points).filter(|&p| old_to_new[p] >= 0).map(|p| p as i64).collect();
            (point_map, old_to_new)
        };
    let point_identity = identity_mapping(&point_map, source_points);
    let vertex_identity = identity_mapping(&vertex_map, source_vertices);
    let primitive_identity = identity_mapping(&primitive_map, source_primitives);
    let topology_changed = !vertex_identity || !primitive_identity;
    if point_identity && !topology_changed {
        return Ok((geometry_value.clone(), old_to_new));
    }
    if !point_identity {
        for vertex in 0..output_vertices as usize {
            let point = old_to_new[vertex_points[vertex] as usize];
            if point < 0 {
                return Err(fail("cleanup retained a deleted point"));
            }
            vertex_points[vertex] = point;
        }
    }
    let output_topology = topology::View::create_validated_owned(
        point_map.len() as i64,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let positions = if point_identity {
        geometry::positions(geometry_value).clone()
    } else {
        let source_positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
        packed::Float3::View::of_owned_exn(
            topology_remap::select_float(cancel_value, grain, None, &point_map, &source_positions.x)?,
            topology_remap::select_float(cancel_value, grain, None, &point_map, &source_positions.y)?,
            topology_remap::select_float(cancel_value, grain, None, &point_map, &source_positions.z)?,
        )?
    };
    let maps = Maps {
        point_map: point_map.clone(),
        vertex_map: vertex_map.clone(),
        primitive_map: primitive_map.clone(),
        point_identity,
        vertex_identity,
        primitive_identity,
    };
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        if let Some(remapped) = remap_attribute(cancel_value, grain, topology_changed, &maps, &item)? {
            attributes.push(remapped);
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        groups.push(remap_group(cancel_value, grain, &maps, &item)?);
    }
    let source_groups = geometry::edge_groups(geometry_value);
    let mut edge_groups = Vec::new();
    if !source_groups.is_empty() {
        let source_index = topology_index::create(cancel_value, source_topology_value)?;
        let target_index = topology_index::create(cancel_value, &output_topology)?;
        for item in &source_groups {
            edge_groups.push(edge_group::remap(
                cancel_value, &source_index, &output_topology, &target_index, &old_to_new, item,
            )?);
        }
    }
    let result = geometry::create(positions, output_topology, Some(attributes), Some(groups), Some(edge_groups))?;
    Ok((result, old_to_new))
}

pub fn apply(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    remove_degenerate_primitives: bool,
    remove_unused_points_from_degenerate_primitives: bool,
    remove_all_unused_points: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    apply_with_mapping(
        cancel_value,
        grain,
        remove_degenerate_primitives,
        remove_unused_points_from_degenerate_primitives,
        remove_all_unused_points,
        geometry_value,
    )
    .map(|(result, _)| result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::topology::PrimitiveKind;

    fn fixture() -> geometry::T {
        // Polygon with a repeated corner, a closed ring that repeats its first point, a collapsed
        // triangle and an open line that collapses to one point.
        let g = geom(
            &[0., 1., 2., 3., 4., 5.],
            &[0.; 6],
            &[0.; 6],
            &[0, 0, 1, 2, 0, 1, 2, 0, 3, 3, 3, 4, 4],
            &[0, 4, 8, 11, 13],
            Some(vec![
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::OpenPolyline,
            ]),
        );
        let g = with_attr(&g, Owner::Point, "pid", Storage::Int(vec![0, 1, 2, 3, 4, 5]));
        let g = with_attr(&g, Owner::Vertex, "vid", Storage::Int((0..13).collect()));
        with_attr(&g, Owner::Primitive, "fid", Storage::Int(vec![0, 1, 2, 3]))
    }

    #[test]
    fn removes_repeats_and_degenerate_primitives() {
        let source = fixture();
        let (out, map) = apply_with_mapping(None, 1, true, false, false, &source).unwrap();
        // The ring [0,1,2,0] drops its closing point; [3,3,3] and [4,4] fall below the minimum.
        assert_eq!(topo(&out).0, vec![0, 1, 2, 0, 1, 2]);
        assert_eq!(topo(&out).1, vec![0, 3, 6]);
        assert_eq!(ints(Owner::Primitive, "fid", &out), vec![0, 1]);
        assert_eq!(ints(Owner::Vertex, "vid", &out), vec![0, 2, 3, 4, 5, 6]);
        assert_eq!(geometry::point_count(&out), 6);
        assert_eq!(map, vec![0, 1, 2, 3, 4, 5]);
    }

    #[test]
    fn unused_point_policies_report_the_mapping() {
        let source = fixture();
        // Only points that the removed primitives used alone are dropped.
        let (selective, map) = apply_with_mapping(None, 1, true, true, false, &source).unwrap();
        assert_eq!(ints(Owner::Point, "pid", &selective), vec![0, 1, 2, 5]);
        assert_eq!(map, vec![0, 1, 2, -1, -1, 3]);
        // Every unreferenced point goes with remove_all_unused_points.
        let (all, map) = apply_with_mapping(None, 1, true, false, true, &source).unwrap();
        assert_eq!(ints(Owner::Point, "pid", &all), vec![0, 1, 2]);
        assert_eq!(map, vec![0, 1, 2, -1, -1, -1]);
        assert_eq!(topo(&all).0, vec![0, 1, 2, 0, 1, 2]);
    }

    #[test]
    fn no_work_is_an_identity() {
        let source = fixture();
        let (out, map) = apply_with_mapping(None, 1, false, true, false, &source).unwrap();
        assert_eq!(geometry::data_id(&out), geometry::data_id(&source));
        assert_eq!(map, (0..6).collect::<Vec<_>>());
        let clean = grid(2, 2, 1.0, true);
        let out = apply(None, 1, true, true, true, &clean).unwrap();
        assert_eq!(geometry::data_id(&out), geometry::data_id(&clean));
        let token = cancel::create();
        cancel::cancel(&token);
        assert_eq!(error::code(&apply(Some(&token), 1, true, false, false, &source).err().unwrap()), "cancelled");
    }
}
