use crate::{support, cancel, edge_group, error, group, point_index, topology, topology_index};
use crate::error::Code;

pub enum T {
    SelectedPoints(group::T),
    SelectedVertices(group::T),
    SelectedPrimitives(group::T),
    SelectedEdges(edge_group::T),
}
fn selection_error(operation: &str, message: String) -> error::T {
    error::new(operation, Code::InvalidSelection, message)
}

fn cancelled_error() -> error::T {
    error::cancelled("element selection")
}

pub fn validate(
    operation: &str,
    topology_value: &topology::T,
    selection: Option<&T>,
) -> Result<(), error::T> {
    let Some(selection) = selection else {
        return Ok(());
    };
    let message = match selection {
        T::SelectedPoints(value) => {
            if group::owner(value) != group::Owner::Point {
                Some(format!("{operation}: point selection must own points"))
            } else if group::length(value) != topology::point_count(topology_value) {
                Some(format!("{operation}: point selection length does not match point count"))
            } else {
                None
            }
        }
        T::SelectedVertices(value) => {
            if group::owner(value) != group::Owner::Vertex {
                Some(format!("{operation}: vertex selection must own vertices"))
            } else if group::length(value) != topology::vertex_count(topology_value) {
                Some(format!("{operation}: vertex selection length does not match vertex count"))
            } else {
                None
            }
        }
        T::SelectedPrimitives(value) => {
            if group::owner(value) != group::Owner::Primitive {
                Some(format!("{operation}: primitive selection must own primitives"))
            } else if group::length(value) != topology::primitive_count(topology_value) {
                Some(format!("{operation}: primitive selection length does not match primitive count"))
            } else {
                None
            }
        }
        T::SelectedEdges(value) => {
            if edge_group::topology_data_id(value) != topology::data_id(topology_value) {
                Some(format!("{operation}: edge selection belongs to different topology"))
            } else {
                None
            }
        }
    };
    match message {
        Some(message) => Err(selection_error(operation, message)),
        None => Ok(()),
    }
}

fn point_selected(
    point: i64,
    selection: &T,
    _topology: &topology::view::View,
    points: Option<&point_index::view::View>,
    edges: Option<&topology_index::view::View>,
) -> bool {
    match selection {
        T::SelectedPoints(value) => group::mem(point, value),
        T::SelectedVertices(value) => {
            let reverse = points.expect("point selection index required");
            let mut at = reverse.point_offsets[point as usize];
            let last = reverse.point_offsets[(point + 1) as usize];
            while at < last {
                if group::mem(reverse.point_vertices[at as usize], value) {
                    return true;
                }
                at += 1;
            }
            false
        }
        T::SelectedPrimitives(value) => {
            let reverse = points.expect("point selection index required");
            let mut at = reverse.point_offsets[point as usize];
            let last = reverse.point_offsets[(point + 1) as usize];
            while at < last {
                let vertex = reverse.point_vertices[at as usize];
                if group::mem(reverse.primitive_of_vertex[vertex as usize], value) {
                    return true;
                }
                at += 1;
            }
            false
        }
        T::SelectedEdges(value) => {
            let reverse = edges.expect("edge selection index required");
            let mut at = reverse.point_edge_offsets[point as usize];
            let last = reverse.point_edge_offsets[(point + 1) as usize];
            while at < last {
                if edge_group::mem(reverse.point_edges[at as usize], value) {
                    return true;
                }
                at += 1;
            }
            false
        }
    }
}

fn vertex_selected(
    vertex: i64,
    selection: &T,
    topology: &topology::view::View,
    points: Option<&point_index::view::View>,
    edges: Option<&topology_index::view::View>,
) -> bool {
    match selection {
        T::SelectedPoints(value) => group::mem(topology.vertex_points[vertex as usize], value),
        T::SelectedVertices(value) => group::mem(vertex, value),
        T::SelectedPrimitives(value) => {
            group::mem(points.expect("point selection index required").primitive_of_vertex[vertex as usize], value)
        }
        T::SelectedEdges(value) => {
            let reverse = edges.expect("edge selection index required");
            let outgoing = reverse.edge_of_vertex[vertex as usize];
            let previous = reverse.previous_vertex[vertex as usize];
            (outgoing >= 0 && edge_group::mem(outgoing, value))
                || (previous >= 0
                    && reverse.edge_of_vertex[previous as usize] >= 0
                    && edge_group::mem(reverse.edge_of_vertex[previous as usize], value))
        }
    }
}

fn primitive_selected(
    primitive: i64,
    selection: &T,
    topology: &topology::view::View,
    edges: Option<&topology_index::view::View>,
) -> bool {
    match selection {
        T::SelectedPrimitives(value) => group::mem(primitive, value),
        T::SelectedPoints(value) => {
            let mut vertex = topology.primitive_offsets[primitive as usize];
            let last = topology.primitive_offsets[(primitive + 1) as usize];
            while vertex < last {
                if group::mem(topology.vertex_points[vertex as usize], value) {
                    return true;
                }
                vertex += 1;
            }
            false
        }
        T::SelectedVertices(value) => {
            let mut vertex = topology.primitive_offsets[primitive as usize];
            let last = topology.primitive_offsets[(primitive + 1) as usize];
            while vertex < last {
                if group::mem(vertex, value) {
                    return true;
                }
                vertex += 1;
            }
            false
        }
        T::SelectedEdges(value) => {
            let reverse = edges.expect("edge selection index required");
            let mut vertex = topology.primitive_offsets[primitive as usize];
            let last = topology.primitive_offsets[(primitive + 1) as usize];
            while vertex < last {
                let edge = reverse.edge_of_vertex[vertex as usize];
                if edge >= 0 && edge_group::mem(edge, value) {
                    return true;
                }
                vertex += 1;
            }
            false
        }
    }
}

pub fn promote(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    name: Option<&str>,
    destination: group::Owner,
    selection: &T,
    topology_value: &topology::T,
) -> Result<group::T, error::T> {
    validate("element selection", topology_value, Some(selection))?;
    let name = name.unwrap_or("__element_selection");
    match (destination, selection) {
        (group::Owner::Point, T::SelectedPoints(value))
        | (group::Owner::Vertex, T::SelectedVertices(value))
        | (group::Owner::Primitive, T::SelectedPrimitives(value)) => {
            return group::with_name(name, value);
        }
        _ => {}
    }
    support::check_positive(grain).map_err(|_| selection_error(
            "element selection",
            "Group.init: grain must be positive".to_owned(),
        ))?;

    let topology = topology::view::view(topology_value)?;
    let needs_point_index = matches!(
        (destination, selection),
        (group::Owner::Point, T::SelectedVertices(_) | T::SelectedPrimitives(_))
            | (group::Owner::Vertex, T::SelectedPrimitives(_))
    );
    let point_index = if needs_point_index {
        let index = point_index::create(cancel_value, topology_value)?;
        Some(point_index::view::view(&index))
    } else {
        None
    };
    let edge_index = if matches!(selection, T::SelectedEdges(_)) {
        let index = topology_index::create(cancel_value, topology_value)?;
        Some(topology_index::view::view(&index))
    } else {
        None
    };
    let count = match destination {
        group::Owner::Point => topology.point_count,
        group::Owner::Vertex => topology.vertex_points.len() as i64,
        group::Owner::Primitive => topology.primitive_offsets.len() as i64 - 1,
    };
    let mut builder = group::builder::create(destination, name, count)?;
    for index in 0..count {
        if cancel::poll(cancel_value, index, 4095).is_err() {
            return Err(cancelled_error());
        }
        let member = match destination {
            group::Owner::Point => {
                point_selected(index, selection, &topology, point_index.as_ref(), edge_index.as_ref())
            }
            group::Owner::Vertex => {
                vertex_selected(index, selection, &topology, point_index.as_ref(), edge_index.as_ref())
            }
            group::Owner::Primitive => {
                primitive_selected(index, selection, &topology, edge_index.as_ref())
            }
        };
        group::builder::set(&mut builder, index, member)?;
    }
    group::builder::freeze(&mut builder)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn point_selection_promotes_to_incident_vertices_and_primitives() {
        let topology_value =
            topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let points = group::init(
            Some(1),
            group::Owner::Point,
            "selected",
            3,
            &|point| point == 1,
        )
        .unwrap();
        let selection = T::SelectedPoints(points);
        let vertices = promote(
            None,
            1,
            None,
            group::Owner::Vertex,
            &selection,
            &topology_value,
        )
        .unwrap();
        assert!(!group::mem(0, &vertices));
        assert!(group::mem(1, &vertices));
        assert!(!group::mem(2, &vertices));

        let primitives = promote(
            None,
            1,
            None,
            group::Owner::Primitive,
            &selection,
            &topology_value,
        )
        .unwrap();
        assert!(group::mem(0, &primitives));
    }

    #[test]
    fn validation_rejects_a_selection_with_the_wrong_owner() {
        let topology_value = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let vertices =
            group::init(None, group::Owner::Vertex, "corners", 3, &|_| true).unwrap();
        let selection = T::SelectedPoints(vertices);
        let error = validate("normals", &topology_value, Some(&selection)).unwrap_err();
        assert_eq!(error::code(&error), Code::InvalidSelection);
    }
}
