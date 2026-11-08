//! Checked UV flatten and relax over the mean-value parameterization.
use rays_math::vec3;
use rdk_core::{error, geometry};
use rdk_core::error::Code;

use crate::uv_parameterize::{self, BoundaryMode};

pub use crate::uv_parameterize::Params;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Projection {
    Planar { origin: vec3::T, u_axis: vec3::T, v_axis: vec3::T },
    Cylindrical { origin: vec3::T, axis: vec3::T, seam: vec3::T, height: f64 },
    Spherical { origin: vec3::T, axis: vec3::T, seam: vec3::T },
}

pub fn flatten(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    uv_parameterize::solve(params, BoundaryMode::Circle, value)
        .map_err(|failure| error::guard(failure, "uv_flatten", Code::InvalidUv))
}

pub fn relax(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    uv_parameterize::solve(params, BoundaryMode::Preserve, value)
        .map_err(|failure| error::guard(failure, "uv_relax", Code::InvalidUv))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::{group, packed, topology};
    use std::sync::{Arc, Mutex};

    fn grid(columns: i64, rows: i64) -> geometry::T {
        let (mut x, mut z, mut vertices, mut offsets) = (Vec::new(), Vec::new(), Vec::new(), vec![0_i64]);
        for row in 0..=rows {
            for column in 0..=columns {
                x.push(column as f64);
                z.push(row as f64);
            }
        }
        let width = columns + 1;
        for row in 0..rows {
            for column in 0..columns {
                let a = row * width + column;
                let (b, c, d) = (a + 1, a + width + 1, a + width);
                vertices.extend([a, b, c, a, c, d]);
                offsets.push(vertices.len() as i64 - 3);
                offsets.push(vertices.len() as i64);
            }
        }
        let count = x.len();
        let positions = packed::Float3::of_owned(x, vec![0.0; count], z).unwrap();
        let topo = topology::polygons_owned(count as i64, vertices, offsets).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    fn params() -> Params {
        Params {
            cancel: None,
            grain: None,
            name: "uv".to_owned(),
            seams: None,
            edge_seams: None,
            uv_tolerance: 1e-9,
            iterations: 500,
            tolerance: 1e-7,
        }
    }

    fn uv(value: &geometry::T) -> (Vec<f64>, Vec<f64>) {
        let item = geometry::find_attribute(rdk_core::attribute::Owner::Vertex, "uv", value).expect("uv");
        match rdk_core::attribute::View::storage(&item).unwrap() {
            rdk_core::attribute::Storage::Float2(values) => {
                let view = packed::Float2::View::view(values);
                (view.x.to_vec(), view.y.to_vec())
            }
            _ => panic!("uv storage"),
        }
    }

    fn message(result: Result<geometry::T, error::T>) -> String {
        match result {
            Ok(_) => "ok".to_owned(),
            Err(failure) => format!("{}/{}", error::code(&failure), error::message(&failure)),
        }
    }

    #[test]
    fn flatten_puts_the_boundary_on_a_circle_and_the_interior_inside() {
        let mesh = grid(6, 6);
        let flat = flatten(&params(), &mesh).unwrap();
        let (u, v) = uv(&flat);
        let view = topology::View::view(geometry::topology(&flat)).unwrap();
        for corner in 0..view.vertex_points.len() {
            let point = view.vertex_points[corner];
            let (row, column) = (point / 7, point % 7);
            let radius = ((u[corner] - 0.5).powi(2) + (v[corner] - 0.5).powi(2)).sqrt();
            if row == 0 || row == 6 || column == 0 || column == 6 {
                assert!((radius - 0.45).abs() < 1e-9, "boundary corner on the circle");
            } else {
                assert!(radius < 0.45, "interior corner inside the circle");
            }
        }
        // Corners of one point share one chart variable.
        let mut seen = std::collections::HashMap::new();
        for corner in 0..view.vertex_points.len() {
            let entry = seen.entry(view.vertex_points[corner]).or_insert((u[corner], v[corner]));
            assert_eq!(*entry, (u[corner], v[corner]));
        }
    }

    #[test]
    fn relax_reproduces_a_flattened_chart_and_honours_the_input_boundary() {
        let mesh = grid(5, 4);
        let flat = flatten(&params(), &mesh).unwrap();
        let relaxed = relax(&params(), &flat).unwrap();
        let (u0, v0) = uv(&flat);
        let (u1, v1) = uv(&relaxed);
        for corner in 0..u0.len() {
            assert!((u0[corner] - u1[corner]).abs() < 1e-5 && (v0[corner] - v1[corner]).abs() < 1e-5);
        }
        let Err(missing) = relax(&params(), &mesh) else { panic!("expected failure") };
        assert_eq!(error::operation(&missing), "uv_relax");
        assert_eq!(error::kind(&missing), Code::InvalidUv);
        assert_eq!(error::message(&missing), "UV Relax requires vertex float2 attribute \"uv\"");
    }

    #[test]
    fn rejects_bad_parameters_and_unseamed_closed_meshes() {
        let mesh = grid(2, 2);
        let bad = |p: Params| message(flatten(&p, &mesh));
        assert_eq!(bad(Params { grain: Some(0), ..params() }), "invalid_uv/grain must be positive");
        assert_eq!(bad(Params { name: "  ".to_owned(), ..params() }), "invalid_uv/UV attribute name must not be empty");
        assert_eq!(bad(Params { iterations: 0, ..params() }), "invalid_uv/iterations must be positive");
        assert_eq!(
            bad(Params { tolerance: f64::NAN, ..params() }),
            "invalid_uv/solver tolerance must be finite and non-negative"
        );
        // A tetrahedron is closed: every edge has two faces and no seam is selected.
        let positions = packed::Float3::of_owned(vec![0.0, 1.0, 0.0, 0.0], vec![0.0, 0.0, 1.0, 0.0], vec![0.0, 0.0, 0.0, 1.0]).unwrap();
        let topo = topology::polygons_owned(4, vec![0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3], vec![0, 3, 6, 9, 12]).unwrap();
        let tetra = geometry::create(positions, topo, None, None, None).unwrap();
        assert_eq!(message(flatten(&params(), &tetra)), "invalid_uv/closed UV island requires at least one seam");
        let token = Arc::new(rdk_core::cancel::create());
        rdk_core::cancel::cancel(&token);
        let Err(stopped) = flatten(&Params { cancel: Some(token), ..params() }, &mesh) else { panic!("expected cancel") };
        assert_eq!(error::kind(&stopped), Code::Cancelled);
        let wrong_seams = group::init(Some(1), group::Owner::Point, "s", 9, &|_| false).unwrap();
        assert_eq!(
            message(flatten(&Params { seams: Some(wrong_seams), ..params() }, &mesh)),
            "invalid_uv/UV seam group must own one outgoing-edge entry per vertex"
        );
    }

    #[test]
    fn one_and_four_domains_agree_exactly() {
        let mesh = grid(12, 10);
        let run = |domains: i64| -> (Vec<f64>, Vec<f64>) {
            let output = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let result = flatten(&Params { grain: Some(7), ..params() }, &mesh).map_err(|f| error::to_string(&f))?;
                *output.lock().unwrap() = Some(uv(&result));
                Ok(())
            })
            .unwrap();
            output.into_inner().unwrap().unwrap()
        };
        assert_eq!(run(1), run(4));
    }
}
