use rdk_core::{
    attribute, cancel, edge_group, element_selection, error, face_normals, geometry, group, normal_ops,
    packed, topology, topology_index,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Planes {
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    pub z: Vec<f64>,
}

fn fail(message: String) -> error::T {
    error::make("Rdk.Deform".to_owned(), "invalid_parameter".to_owned(), message, None)
        .unwrap_or_else(|e| e)
}

pub fn validate_selection(
    topology_value: &topology::T,
    selection: Option<&element_selection::T>,
) -> Result<(), error::T> {
    let Some(selection) = selection else {
        return Ok(());
    };
    match selection {
        element_selection::T::SelectedPoints(item) => {
            if group::owner(item) != group::Owner::Point {
                Err(fail("Rdk.Deform: point selection must own points".to_owned()))
            } else if group::length(item) != topology::point_count(topology_value) {
                Err(fail("Rdk.Deform: point selection length does not match point count".to_owned()))
            } else {
                Ok(())
            }
        }
        element_selection::T::SelectedVertices(item) => {
            if group::owner(item) != group::Owner::Vertex {
                Err(fail("Rdk.Deform: vertex selection must own vertices".to_owned()))
            } else if group::length(item) != topology::vertex_count(topology_value) {
                Err(fail("Rdk.Deform: vertex selection length does not match vertex count".to_owned()))
            } else {
                Ok(())
            }
        }
        element_selection::T::SelectedPrimitives(item) => {
            if group::owner(item) != group::Owner::Primitive {
                Err(fail("Rdk.Deform: primitive selection must own primitives".to_owned()))
            } else if group::length(item) != topology::primitive_count(topology_value) {
                Err(fail(
                    "Rdk.Deform: primitive selection length does not match primitive count".to_owned(),
                ))
            } else {
                Ok(())
            }
        }
        element_selection::T::SelectedEdges(item) => {
            if edge_group::topology_data_id(item) != topology::data_id(topology_value) {
                Err(fail("Rdk.Deform: edge selection belongs to different topology".to_owned()))
            } else {
                Ok(())
            }
        }
    }
}

// The index is required for every selection but None and point groups.
pub fn point_selected(
    selection: Option<&element_selection::T>,
    index: Option<&topology_index::T>,
    point: i64,
) -> Result<bool, error::T> {
    let need_index = || index.ok_or_else(|| fail("Rdk.Deform: selection needs a topology index".to_owned()));
    match selection {
        None => Ok(true),
        Some(element_selection::T::SelectedPoints(item)) => Ok(group::mem(point, item)),
        Some(element_selection::T::SelectedVertices(item)) => {
            let index = need_index()?;
            for local in 0..topology_index::point_incidence_count(index, point)? {
                if group::mem(topology_index::point_vertex(index, point, local)?, item) {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        Some(element_selection::T::SelectedPrimitives(item)) => {
            let index = need_index()?;
            for local in 0..topology_index::point_incidence_count(index, point)? {
                let vertex = topology_index::point_vertex(index, point, local)?;
                if group::mem(topology_index::primitive_of_vertex(index, vertex)?, item) {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        Some(element_selection::T::SelectedEdges(item)) => {
            let index = need_index()?;
            for local in 0..topology_index::point_edge_count(index, point)? {
                if edge_group::mem(topology_index::point_edge(index, point, local)?, item) {
                    return Ok(true);
                }
            }
            Ok(false)
        }
    }
}

fn planes_of(values: &packed::Float3::T) -> Result<Planes, error::T> {
    let values = packed::Float3::View::view(values)?;
    Ok(Planes { x: values.x.to_vec(), y: values.y.to_vec(), z: values.z.to_vec() })
}

pub fn point_vector_attribute(
    operation: &str,
    name: &str,
    geometry_value: &geometry::T,
) -> Result<Planes, error::T> {
    if name == "P" {
        return planes_of(geometry::positions(geometry_value));
    }
    let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
        return Err(fail(format!("{operation}: missing point float3 attribute {name}")));
    };
    match attribute::View::storage(&found)? {
        attribute::Storage::Float3(values) => planes_of(values),
        _ => Err(fail(format!("{operation}: point attribute {name} must have float3 storage"))),
    }
}

fn vertex_normals(geometry_value: &geometry::T) -> Result<Option<Planes>, error::T> {
    let Some(found) = geometry::find_attribute(attribute::Owner::Vertex, "N", geometry_value) else {
        return Ok(None);
    };
    match attribute::View::storage(&found)? {
        attribute::Storage::Float3(values) => Ok(Some(planes_of(values)?)),
        _ => Err(fail("Rdk.Deform: vertex N must have float3 storage".to_owned())),
    }
}

fn face_vectors(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<Planes, error::T> {
    let ((x, y, z), _) = face_normals::polygon_area_vector(
        cancel_value, grain, false, true, None, "Rdk.Deform", geometry_value,
    )
    .map_err(|e| fail(error::message(&e).to_owned()))?;
    Ok(Planes { x, y, z })
}

fn point_vectors_from_vertices(
    cancel_value: Option<&cancel::T>,
    topology_value: &topology::T,
    source: &Planes,
) -> Result<Planes, error::T> {
    let view = topology::View::view(topology_value)?;
    let count = view.point_count as usize;
    let mut x = vec![0.0; count];
    let mut y = vec![0.0; count];
    let mut z = vec![0.0; count];
    let mut incidence = vec![0i64; count];
    for (vertex, &point) in view.vertex_points.iter().enumerate() {
        cancel::poll(cancel_value, vertex as i64, 16_383)?;
        let point = point as usize;
        x[point] += source.x[vertex];
        y[point] += source.y[vertex];
        z[point] += source.z[vertex];
        incidence[point] += 1;
    }
    for point in 0..count {
        cancel::poll(cancel_value, point as i64, 4095)?;
        if incidence[point] > 1 {
            let inverse = 1.0 / incidence[point] as f64;
            x[point] *= inverse;
            y[point] *= inverse;
            z[point] *= inverse;
        }
    }
    Ok(Planes { x, y, z })
}

fn geometric_point_vectors(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<Planes, error::T> {
    let faces = face_vectors(cancel_value, grain, geometry_value)?;
    let view = topology::View::view(geometry::topology(geometry_value))?;
    let count = view.point_count as usize;
    let mut x = vec![0.0; count];
    let mut y = vec![0.0; count];
    let mut z = vec![0.0; count];
    let mut primitive = 0usize;
    for (vertex, &point) in view.vertex_points.iter().enumerate() {
        cancel::poll(cancel_value, vertex as i64, 16_383)?;
        while vertex as i64 >= view.primitive_offsets[primitive + 1] {
            primitive += 1;
        }
        let point = point as usize;
        x[point] += faces.x[primitive];
        y[point] += faces.y[primitive];
        z[point] += faces.z[primitive];
    }
    Ok(Planes { x, y, z })
}

pub fn resolve_directions(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<Planes, error::T> {
    if let Some(found) = geometry::find_attribute(attribute::Owner::Point, "N", geometry_value) {
        return match attribute::View::storage(&found)? {
            attribute::Storage::Float3(values) => planes_of(values),
            _ => Err(fail("Rdk.Deform: point N must have float3 storage".to_owned())),
        };
    }
    match vertex_normals(geometry_value)? {
        Some(values) => {
            point_vectors_from_vertices(cancel_value, geometry::topology(geometry_value), &values)
        }
        None => geometric_point_vectors(cancel_value, grain, geometry_value),
    }
}

pub struct Params {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: i64,
    pub primitives: Option<group::T>,
}

pub fn normals(params: Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    if params.grain <= 0 {
        return Err(fail("Rdk_mesh.Deform.normals: grain must be positive".to_owned()));
    }
    // ponytail: core Normal_ops owns its cancel token, so an Arc token is checked here and not forwarded.
    cancel::check_opt(params.cancel.as_deref())?;
    let selection = params.primitives.clone().map(element_selection::T::SelectedPrimitives);
    normal_ops::run(
        &normal_ops::Params {
            cancel: None,
            grain: Some(params.grain),
            selection,
            primitives: params.primitives,
            owner: attribute::Owner::Point,
            weighting: normal_ops::Weighting::FaceArea,
            cusp_angle: std::f64::consts::PI,
            attribute: "N".to_owned(),
        },
        geometry_value,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    fn code(result: Result<(), error::T>) -> String {
        match result {
            Err(failure) => error::message(&failure).to_owned(),
            Ok(()) => String::new(),
        }
    }

    #[test]
    fn selection_validation_messages() {
        let g = grid(1, 1, 2.0, true);
        let t = geometry::topology(&g);
        let ok = group::init(None, group::Owner::Point, "p", 4, &|_| true).unwrap();
        assert!(validate_selection(t, Some(&element_selection::T::SelectedPoints(ok))).is_ok());
        assert!(validate_selection(t, None).is_ok());
        let short = group::init(None, group::Owner::Point, "p", 3, &|_| true).unwrap();
        assert_eq!(
            code(validate_selection(t, Some(&element_selection::T::SelectedPoints(short)))),
            "Rdk.Deform: point selection length does not match point count"
        );
        let wrong_owner = group::init(None, group::Owner::Vertex, "v", 4, &|_| true).unwrap();
        assert_eq!(
            code(validate_selection(t, Some(&element_selection::T::SelectedPoints(wrong_owner)))),
            "Rdk.Deform: point selection must own points"
        );
        let faces = group::init(None, group::Owner::Primitive, "f", 2, &|_| true).unwrap();
        assert_eq!(
            code(validate_selection(t, Some(&element_selection::T::SelectedPrimitives(faces)))),
            "Rdk.Deform: primitive selection length does not match primitive count"
        );
        let other = edge_group_pairs(geometry::topology(&grid(2, 1, 1.0, true)), "e", &[(0, 1)]);
        assert_eq!(
            code(validate_selection(t, Some(&element_selection::T::SelectedEdges(other)))),
            "Rdk.Deform: edge selection belongs to different topology"
        );
    }

    #[test]
    fn point_selection_follows_incidence() {
        let g = grid(2, 1, 2.0, true);
        let index = topology_index::create(None, geometry::topology(&g)).unwrap();
        let faces = group::init(None, group::Owner::Primitive, "f", 2, &|p| p == 1).unwrap();
        let selection = element_selection::T::SelectedPrimitives(faces);
        let picked: Vec<bool> = (0..6)
            .map(|p| point_selected(Some(&selection), Some(&index), p).unwrap())
            .collect();
        assert_eq!(picked, vec![false, true, true, false, true, true]);
        assert!(point_selected(None, None, 3).unwrap());
        let corners = group::init(None, group::Owner::Vertex, "c", 8, &|v| v == 0).unwrap();
        let selection = element_selection::T::SelectedVertices(corners);
        assert!(point_selected(Some(&selection), Some(&index), 0).unwrap());
        assert!(!point_selected(Some(&selection), Some(&index), 1).unwrap());
        let edges = edge_group_pairs(geometry::topology(&g), "e", &[(1, 4)]);
        let selection = element_selection::T::SelectedEdges(edges);
        assert!(point_selected(Some(&selection), Some(&index), 4).unwrap());
        assert!(!point_selected(Some(&selection), Some(&index), 0).unwrap());
    }

    #[test]
    fn directions_prefer_point_then_vertex_then_geometric_normals() {
        let g = grid(1, 1, 2.0, true);
        let geometric = resolve_directions(None, 16_384, &g).unwrap();
        assert_eq!(geometric.z, vec![8.0; 4]);
        assert_eq!(geometric.x, vec![0.0; 4]);
        let vertex = with_attr(&g, Owner::Vertex, "N", float3_attr(vec![1., 3., 0., 0.], vec![0.; 4], vec![0.; 4]));
        let from_vertex = resolve_directions(None, 16_384, &vertex).unwrap();
        assert_eq!(from_vertex.x, vec![1., 3., 0., 0.]);
        let point = with_attr(&g, Owner::Point, "N", float3_attr(vec![0.; 4], vec![2.; 4], vec![0.; 4]));
        assert_eq!(resolve_directions(None, 16_384, &point).unwrap().y, vec![2.0; 4]);
        let bad = with_attr(&g, Owner::Point, "N", Storage::Float(Arc::new(vec![0.; 4])));
        assert!(resolve_directions(None, 16_384, &bad).is_err());
    }

    #[test]
    fn named_vectors_and_normals() {
        let g = grid(1, 1, 2.0, true);
        let p = point_vector_attribute("Test", "P", &g).unwrap();
        assert_eq!(p.x, vec![-1., 1., -1., 1.]);
        let failure = point_vector_attribute("Test", "missing", &g).unwrap_err();
        assert_eq!(error::message(&failure), "Test: missing point float3 attribute missing");
        let out = normals(Params { cancel: None, grain: 16_384, primitives: None }, &g).unwrap();
        let n = point_vector_attribute("Test", "N", &out).unwrap();
        assert_eq!(n.z, vec![1.0; 4]);
        let failure = normals(Params { cancel: None, grain: 0, primitives: None }, &g).err().unwrap();
        assert_eq!(error::message(&failure), "Rdk_mesh.Deform.normals: grain must be positive");
    }
}
