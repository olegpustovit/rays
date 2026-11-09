use crate::support::DisjointSet;
use crate::{attribute, edge_group, error, geometry, group, topology, topology_index};
use crate::error::Code;

pub fn signed_volume(geometry_value: &geometry::T) -> Result<f64, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology = topology::view::view(topology_value)?;
    let positions = crate::packed::float3::view::view(geometry::positions(geometry_value))?;
    let mut volume = 0.0;
    for primitive in 0..topology.primitive_kinds.len() {
        let first = topology.primitive_offsets[primitive] as usize;
        let last = topology.primitive_offsets[primitive + 1] as usize;
        let a = topology.vertex_points[first] as usize;
        for k in first + 1..last.saturating_sub(1) {
            let b = topology.vertex_points[k] as usize;
            let c = topology.vertex_points[k + 1] as usize;
            let bx = positions.x[b];
            let by = positions.y[b];
            let bz = positions.z[b];
            let cx = positions.x[c];
            let cy = positions.y[c];
            let cz = positions.z[c];
            volume += positions.x[a] * (by * cz - bz * cy)
                + positions.y[a] * (bz * cx - bx * cz)
                + positions.z[a] * (bx * cy - by * cx);
        }
    }
    Ok(volume / 6.0)
}

// Union is finished: reuse the parent and rank storage for first-member-ordered class IDs.
fn component_classes(mut set: DisjointSet) -> (Vec<i64>, i64) {
    if set.rank.is_empty() { set.rank = vec![0; set.parent.len()]; }
    let mut next_class = 0;
    for element in 0..set.parent.len() {
        if set.parent[element] >= 0 {
            let mut representative = element;
            while set.rank[representative] != u8::MAX
                && set.parent[representative] != representative as i64
            {
                representative = set.parent[representative] as usize;
            }
            let class_id = if set.rank[representative] == u8::MAX {
                set.parent[representative]
            } else {
                let class_id = next_class;
                next_class += 1;
                set.parent[representative] = class_id;
                set.rank[representative] = u8::MAX;
                class_id
            };
            let mut cursor = element;
            while set.rank[cursor] != u8::MAX {
                let next = set.parent[cursor] as usize;
                set.parent[cursor] = class_id;
                set.rank[cursor] = u8::MAX;
                cursor = next;
            }
        }
    }
    (set.parent, next_class)
}

fn make_error(message: String) -> error::T {
    error::new("connectivity", Code::InvalidConnectivity, message)
}
fn connectivity_error(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        error::cancelled("connectivity")
    } else {
        make_error(error::message(&value).to_owned())
    }
}
fn require_group(
    label: &str,
    owner: group::Owner,
    count: i64,
    value: Option<&group::T>,
) -> Result<(), error::T> {
    let Some(value) = value else {
        return Ok(());
    };
    if group::owner(value) != owner {
        let expected = match owner {
            group::Owner::Point => "point",
            group::Owner::Vertex => "vertex",
            group::Owner::Primitive => "primitive",
        };
        return Err(make_error(format!("Connectivity: {label} must be {expected}-owned")));
    }
    if group::length(value) != count {
        return Err(make_error(format!("Connectivity: {label} length does not match geometry")));
    }
    Ok(())
}


fn allowed(value: Option<&group::T>, index: i64) -> bool {
    value.map_or(true, |group_value| group::mem(index, group_value))
}

pub fn classify_connectivity(
    primitives: Option<&group::T>,
    points: Option<&group::T>,
    seams: Option<&edge_group::T>,
    owner: attribute::Owner,
    geometry_value: &geometry::T,
) -> Result<(Vec<i64>, i64), error::T> {
    let point_count = geometry::point_count(geometry_value);
    let primitive_count = geometry::primitive_count(geometry_value);
    let topology_value = geometry::topology(geometry_value);
    let validation = if owner != attribute::Owner::Point && owner != attribute::Owner::Primitive {
        Err(make_error("Connectivity: owner must be point or primitive".to_owned()))
    } else if owner == attribute::Owner::Primitive && points.is_some() {
        Err(make_error(
            "Connectivity: point include group is available only in point mode".to_owned(),
        ))
    } else {
        require_group(
            "primitive include group",
            group::Owner::Primitive,
            primitive_count,
            primitives,
        )
        .and_then(|()| require_group("point include group", group::Owner::Point, point_count, points))
    };
    validation?;
    if seams.is_some_and(|value| edge_group::topology_data_id(value) != topology::data_id(topology_value)) {
        return Err(make_error("Connectivity: seam group belongs to a different topology".to_owned()));
    }

    let topology_view =
        topology::view::view(topology_value).map_err(connectivity_error)?;
    match owner {
        attribute::Owner::Point => {
            let index = topology_index::create(None, topology_value).map_err(connectivity_error)?;
            let reverse = topology_index::view::view(&index);
            if seams.is_some_and(|value| edge_group::length(value) != topology_index::edge_count(&index)) {
                return Err(make_error("Connectivity: seam group length does not match topology".to_owned()));
            }
            let mut set = DisjointSet::from_parents((0..point_count).map(|point| if allowed(points, point) { point } else { -1 }).collect());
            for primitive in 0..primitive_count {
                if allowed(primitives, primitive) {
                    let first = topology_view.primitive_offsets[primitive as usize];
                    let last = topology_view.primitive_offsets[(primitive + 1) as usize];
                    for vertex in first..last {
                        let edge = reverse.edge_of_vertex[vertex as usize];
                        let is_seam = edge >= 0
                            && seams.map_or(false, |value| edge_group::mem(edge, value));
                        if edge >= 0 && !is_seam {
                            set.union_rank(
                                reverse.edge_a[edge as usize] as usize,
                                reverse.edge_b[edge as usize] as usize,
                                false,
                            );
                        }
                    }
                }
            }
            Ok(component_classes(set))
        }
        attribute::Owner::Primitive => {
            let mut set = DisjointSet::from_parents((0..primitive_count).map(|primitive| if allowed(primitives, primitive) { primitive } else { -1 }).collect());
            match seams {
                None => {
                    let mut first_primitive = vec![-1; point_count as usize];
                    for primitive in 0..primitive_count {
                        if allowed(primitives, primitive) {
                            let first = topology_view.primitive_offsets[primitive as usize];
                            let last = topology_view.primitive_offsets[(primitive + 1) as usize];
                            for vertex in first..last {
                                let point = topology_view.vertex_points[vertex as usize];
                                let previous = first_primitive[point as usize];
                                if previous < 0 {
                                    first_primitive[point as usize] = primitive;
                                } else {
                                    set.union_rank(primitive as usize, previous as usize, false);
                                }
                            }
                        }
                    }
                    Ok(component_classes(set))
                }
                Some(seam_group) => {
                    let index = topology_index::create(None, topology_value).map_err(connectivity_error)?;
                    let reverse = topology_index::view::view(&index);
                    if edge_group::length(seam_group) != topology_index::edge_count(&index) {
                        return Err(make_error("Connectivity: seam group length does not match topology".to_owned()));
                    }
                    for edge in 0..topology_index::edge_count(&index) {
                        if !edge_group::mem(edge, seam_group) {
                            let first = reverse.edge_offsets[edge as usize];
                            let last = reverse.edge_offsets[(edge + 1) as usize];
                            for left_slot in first..last.saturating_sub(1) {
                                let left_vertex = reverse.edge_vertices[left_slot as usize];
                                let left = reverse.primitive_of_vertex[left_vertex as usize];
                                if allowed(primitives, left) {
                                    for right_slot in left_slot + 1..last {
                                        let right_vertex = reverse.edge_vertices[right_slot as usize];
                                        let right = reverse.primitive_of_vertex[right_vertex as usize];
                                        if allowed(primitives, right) {
                                            set.union_rank(left as usize, right as usize, false);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Ok(component_classes(set))
                }
            }
        }
        attribute::Owner::Vertex | attribute::Owner::Detail => unreachable!(),
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    fn two_quads() -> geometry::T {
        let topology_value = topology::polygons_owned(
            6,
            vec![0, 1, 4, 3, 1, 2, 5, 4],
            vec![0, 4, 8],
        )
        .unwrap();
        let positions = crate::packed::float3::view::of_owned_exn(
            vec![0.0, 1.0, 2.0, 0.0, 1.0, 2.0],
            vec![0.0, 0.0, 0.0, 1.0, 1.0, 1.0],
            vec![0.0; 6],
        )
        .unwrap();
        geometry::create(positions, topology_value, None, None, None).unwrap()
    }

    #[test]
    fn connectivity_is_deterministic_and_respects_include_masks() {
        let geometry_value = two_quads();
        let (classes, count) = classify_connectivity(
            None,
            None,
            None,
            attribute::Owner::Primitive,
            &geometry_value,
        )
        .unwrap();
        assert_eq!(classes, vec![0, 0]);
        assert_eq!(count, 1);

        let points = group::init(
            Some(1),
            group::Owner::Point,
            "without_bridge",
            6,
            &|point| Ok(point != 1 && point != 4),
        )
        .unwrap();
        let (classes, count) = classify_connectivity(
            None,
            Some(&points),
            None,
            attribute::Owner::Point,
            &geometry_value,
        )
        .unwrap();
        assert_eq!(classes, vec![0, -1, 1, 0, -1, 1]);
        assert_eq!(count, 2);
    }


    #[test]
    fn seam_edges_split_shared_edge_primitive_components() {
        let geometry_value = two_quads();
        let topology_value = geometry::topology(&geometry_value);
        let index = topology_index::create(None, topology_value).unwrap();
        let seams = edge_group::init(
            Some(1),
            topology_value,
            &index,
            "seam",
            &|edge| Ok(topology_index::edge_points(&index, edge).map_or(false, |points| points == (1, 4))),
        )
        .unwrap();
        let (classes, count) = classify_connectivity(
            None,
            None,
            Some(&seams),
            attribute::Owner::Primitive,
            &geometry_value,
        )
        .unwrap();
        assert_eq!(classes, vec![0, 1]);
        assert_eq!(count, 2);
    }

    /// The rest of lib/rdk/test_connectivity.ml: unmasked point mode, the primitive sentinel,
    /// point seams and the validation errors.
    #[test]
    fn modes_sentinels_point_seams_and_validation() {
        let geometry_value = two_quads();
        let (classes, count) = classify_connectivity(None, None, None, attribute::Owner::Point, &geometry_value).unwrap();
        assert_eq!((classes, count), (vec![0; 6], 1), "edge-connected point connectivity");
        let right = group::init(Some(1), group::Owner::Primitive, "right", 2, &|primitive| Ok(primitive == 1)).unwrap();
        let (classes, count) =
            classify_connectivity(Some(&right), None, None, attribute::Owner::Primitive, &geometry_value).unwrap();
        assert_eq!((classes, count), (vec![-1, 0], 1), "excluded primitives retain the minus-one sentinel");

        let topology_value = geometry::topology(&geometry_value);
        let index = topology_index::create(None, topology_value).unwrap();
        let seam = |topology_value: &topology::T, index: &topology_index::T| {
            edge_group::init(Some(1), topology_value, index, "seam", &|edge| Ok({
                topology_index::edge_points(index, edge).map_or(false, |points| points == (1, 4))
            }))
            .unwrap()
        };
        let seams = seam(topology_value, &index);
        let (classes, count) =
            classify_connectivity(None, None, Some(&seams), attribute::Owner::Point, &geometry_value).unwrap();
        assert!(classes.len() as i64 == geometry::point_count(&geometry_value) && count == 1, "point seam classification");

        let code = |result: Result<(Vec<i64>, i64), error::T>| error::code(&result.unwrap_err()).as_str().to_owned();
        let point_group = group::init(Some(1), group::Owner::Point, "points", 6, &|_| Ok(true)).unwrap();
        let wrong_length = group::init(Some(1), group::Owner::Primitive, "bad", 1, &|_| Ok(true)).unwrap();
        assert_eq!(
            code(classify_connectivity(None, Some(&point_group), None, attribute::Owner::Primitive, &geometry_value)),
            "invalid_connectivity"
        );
        assert_eq!(
            code(classify_connectivity(Some(&wrong_length), None, None, attribute::Owner::Primitive, &geometry_value)),
            "invalid_connectivity"
        );
        let unrelated = two_quads();
        let unrelated_topology = geometry::topology(&unrelated);
        let unrelated_seam = seam(unrelated_topology, &topology_index::create(None, unrelated_topology).unwrap());
        assert_eq!(
            code(classify_connectivity(None, None, Some(&unrelated_seam), attribute::Owner::Primitive, &geometry_value)),
            "invalid_connectivity"
        );
    }
    #[test]
    fn signed_volume_matches_oriented_tetrahedron() {
        let topology_value = topology::polygons_owned(
            4,
            vec![0, 2, 1, 0, 1, 3, 0, 3, 2, 1, 2, 3],
            vec![0, 3, 6, 9, 12],
        )
        .unwrap();
        let positions = crate::packed::float3::view::of_owned_exn(
            vec![0.0, 1.0, 0.0, 0.0],
            vec![0.0, 0.0, 1.0, 0.0],
            vec![0.0, 0.0, 0.0, 1.0],
        )
        .unwrap();
        let geometry_value = geometry::create(positions, topology_value, None, None, None).unwrap();
        assert!((signed_volume(&geometry_value).unwrap() - 1.0 / 6.0).abs() < 1e-12);
    }
}
