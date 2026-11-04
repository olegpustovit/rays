//! Port of lib/rdk/ops/mesh/smooth.ml: constrained, iterated Laplacian smoothing of points.
use std::sync::Arc;

use rdk_core::attribute::Owner;
use rdk_core::{attribute, attribute_pattern, cancel, error, geometry, group, packed, topology_index};

use crate::attribute_blur;
use crate::attribute_ops;
use crate::deform;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Boundary {
    SmoothFree,
    SmoothUnshared,
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub constrained_points: Option<group::T>,
    pub boundary: Boundary,
    pub iterations: i64,
    pub mode: attribute_blur::Mode,
    pub recompute_normals: bool,
    pub attributes: String,
}

fn fail(code: &str, message: &str) -> error::T {
    attribute_ops::make_error("smooth", code, message)
}

fn validate_point_group(
    label: &str,
    geometry_value: &geometry::T,
    selection: Option<&group::T>,
) -> Result<(), error::T> {
    match selection {
        None => Ok(()),
        Some(selection) if group::owner(selection) != group::Owner::Point => {
            Err(fail("invalid_selection", &format!("{label} must own points")))
        }
        Some(selection) if group::length(selection) != geometry::point_count(geometry_value) => {
            Err(fail(
                "invalid_selection",
                &format!("{label} length does not match point count"),
            ))
        }
        Some(_) => Ok(()),
    }
}

fn point_on_unshared_edge(index: &topology_index::View::View, point: i64) -> bool {
    let first = index.point_edge_offsets[point as usize];
    let last = index.point_edge_offsets[point as usize + 1];
    let mut boundary = false;
    let mut incidence = first;
    while incidence < last && !boundary {
        let edge = index.point_edges[incidence as usize] as usize;
        boundary = index.edge_offsets[edge + 1] - index.edge_offsets[edge] == 1;
        incidence += 1;
    }
    boundary
}

fn make_update_selection(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    boundary: Boundary,
    index: &topology_index::T,
    constrained_points: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, error::T> {
    let point_count = geometry::point_count(geometry_value);
    let unrestricted = constrained_points.is_none() && boundary == Boundary::SmoothFree;
    if unrestricted {
        return Ok(None);
    }
    let index = topology_index::View::view(index);
    cancel::check_opt(cancel_token)?;
    Ok(Some(group::init(
        Some(grain),
        group::Owner::Point,
        "__smooth_update",
        point_count,
        &|point| {
            let constrained = match constrained_points {
                None => false,
                Some(selection) => group::mem(point, selection),
            };
            if constrained {
                false
            } else {
                match boundary {
                    Boundary::SmoothFree => true,
                    Boundary::SmoothUnshared => !point_on_unshared_edge(&index, point),
                }
            }
        },
    )?))
}

fn remap_error(failure: &error::T) -> error::T {
    error::make(
        "smooth".to_owned(),
        error::code(failure).to_owned(),
        error::message(failure).to_owned(),
        Some(error::hints(failure).to_vec()),
    )
    .unwrap_or_else(|built| built)
}

fn cancelled() -> error::T {
    fail("cancelled", "smoothing was cancelled")
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(fail("invalid_parameter", "grain must be positive"));
    }
    let cancel_token = params.cancel.as_deref();
    let in_run = || -> Result<geometry::T, error::T> {
        validate_point_group(
            "constrained point group",
            geometry_value,
            params.constrained_points.as_ref(),
        )?;
        let attribute_pattern = match attribute_pattern::compile(&params.attributes) {
            Err(failure) => return Err(fail("invalid_blur", error::message(&failure))),
            Ok(pattern) => pattern,
        };
        let topology_value = geometry::topology(geometry_value);
        let index = topology_index::create(cancel_token, topology_value)?;
        let selection = make_update_selection(
            cancel_token,
            grain,
            params.boundary,
            &index,
            params.constrained_points.as_ref(),
            geometry_value,
        )?;
        let source_position_id = packed::Float3::data_id(geometry::positions(geometry_value));
        let had_point_normals =
            geometry::find_attribute(attribute::Owner::Point, "N", geometry_value).is_some();
        let had_vertex_normals =
            geometry::find_attribute(Owner::Vertex, "N", geometry_value).is_some();
        let point_normals_smoothed =
            had_point_normals && attribute_pattern::matches(&attribute_pattern, "N");
        let output = attribute_ops::blur_points(
            &attribute_blur::Params {
                cancel: params.cancel.clone(),
                grain: Some(grain),
                selection,
                iterations: params.iterations,
                mode: params.mode,
                pattern: params.attributes.clone(),
            },
            geometry_value,
        )
        .map_err(|failure| remap_error(&failure))?;
        let positions_changed =
            packed::Float3::data_id(geometry::positions(&output)) != source_position_id;
        if params.recompute_normals
            && (had_point_normals || had_vertex_normals)
            && positions_changed
            && !point_normals_smoothed
        {
            deform::normals(
                deform::Params { cancel: params.cancel.clone(), grain, primitives: None },
                &output,
            )
            .map_err(|failure| fail("invalid_normals", error::message(&failure)))
        } else {
            Ok(output)
        }
    };
    in_run().map_err(|failure| {
        if error::code(&failure) == "cancelled" && error::operation(&failure) != "smooth" {
            cancelled()
        } else {
            failure
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::attribute::Storage;
    use rdk_core::normal_ops;
    use std::sync::Mutex;

    fn run_with(
        source: &geometry::T,
        configure: &dyn Fn(&mut Params),
    ) -> Result<geometry::T, error::T> {
        let mut params = Params {
            cancel: None,
            grain: None,
            constrained_points: None,
            boundary: Boundary::SmoothFree,
            iterations: 1,
            mode: attribute_blur::Mode::Laplacian(0.5),
            recompute_normals: true,
            attributes: "P".to_owned(),
        };
        configure(&mut params);
        run(&params, source)
    }

    fn ok(source: &geometry::T, configure: &dyn Fn(&mut Params)) -> geometry::T {
        run_with(source, configure).unwrap_or_else(|failure| panic!("{}", error::to_string(&failure)))
    }

    fn positions(value: &geometry::T) -> packed::Float3::View::View {
        packed::Float3::View::view(geometry::positions(value)).unwrap()
    }

    fn point_float(name: &str, value: &geometry::T) -> Vec<f64> {
        let found = geometry::find_attribute(Owner::Point, name, value).unwrap();
        match attribute::View::storage(&found).unwrap() {
            Storage::Float(values) => values.clone(),
            _ => panic!("{name} has unexpected storage"),
        }
    }

    fn with_attribute(value: &geometry::T, item: attribute::T) -> geometry::T {
        geometry::with_attribute(item, value).unwrap()
    }

    fn two_curves() -> geometry::T {
        let mut builder = rdk_core::topology::Builder::create(6, Some(6), Some(2)).unwrap();
        rdk_core::topology::Builder::add_open_polyline(&mut builder, &[0, 1, 2]).unwrap();
        rdk_core::topology::Builder::add_open_polyline(&mut builder, &[3, 4, 5]).unwrap();
        geometry::create(
            packed::Float3::View::of_owned_exn(
                vec![0.0, 1.0, 2.0, 10.0, 11.0, 12.0],
                vec![0.0, 3.0, 0.0, 0.0, 4.0, 0.0],
                vec![0.0; 6],
            )
            .unwrap(),
            rdk_core::topology::Builder::freeze(&mut builder).unwrap(),
            None,
            None,
            None,
        )
        .unwrap()
    }

    /// A flat triangulated grid in the xz plane, row-major points.
    fn grid(columns: i64, rows: i64, size: f64) -> geometry::T {
        let mut x = Vec::new();
        let mut z = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                x.push(-size / 2.0 + size * (column as f64) / (columns as f64));
                z.push(-size / 2.0 + size * (row as f64) / (rows as f64));
            }
        }
        let stride = columns + 1;
        let mut vertex_points = Vec::new();
        let mut offsets = vec![0];
        for row in 0..rows {
            for column in 0..columns {
                let p00 = row * stride + column;
                let (p01, p10) = (p00 + 1, p00 + stride);
                let p11 = p10 + 1;
                vertex_points.extend([p00, p01, p11, p00, p11, p10]);
                offsets.push(vertex_points.len() as i64 - 3);
                offsets.push(vertex_points.len() as i64);
            }
        }
        let count = x.len();
        geometry::create(
            packed::Float3::View::of_owned_exn(x, vec![0.0; count], z).unwrap(),
            rdk_core::topology::polygons_owned(count as i64, vertex_points, offsets).unwrap(),
            None,
            None,
            None,
        )
        .unwrap()
    }

    fn with_y(value: &geometry::T, y: Vec<f64>) -> geometry::T {
        let source = positions(value);
        geometry::with_positions(
            packed::Float3::View::of_owned_exn(source.x.to_vec(), y, source.z.to_vec()).unwrap(),
            value,
        )
        .unwrap()
    }

    fn expect_code(result: Result<geometry::T, error::T>, code: &str) {
        match result {
            Err(failure) if error::code(&failure) == code => {}
            Err(failure) => panic!("expected {code}, got {}", error::to_string(&failure)),
            Ok(_) => panic!("expected {code}, got success"),
        }
    }

    fn bits_equal(left: &geometry::T, right: &geometry::T) -> bool {
        let (l, r) = (positions(left), positions(right));
        let same = |a: &[f64], b: &[f64]| {
            a.len() == b.len() && a.iter().zip(b).all(|(p, q)| p.to_bits() == q.to_bits())
        };
        same(&l.x, &r.x)
            && same(&l.y, &r.y)
            && same(&l.z, &r.z)
            && same(&point_float("weight", left), &point_float("weight", right))
    }

    #[test]
    fn smoothing_curves_constraints_boundaries_normals_and_errors() {
        let curves = two_curves();
        let heat = attribute::create_owned(
            "heat",
            Owner::Point,
            Storage::Float(vec![0.0, 6.0, 0.0, 0.0, 8.0, 0.0]),
        )
        .unwrap();
        let curves = with_attribute(&curves, heat);
        let smooth = ok(&curves, &|p| {
            p.iterations = 1;
            p.mode = attribute_blur::Mode::Laplacian(1.0);
            p.attributes = "P heat".to_owned();
        });
        assert_eq!(
            positions(&smooth).y.to_vec(),
            vec![3.0, 0.0, 3.0, 4.0, 0.0, 4.0],
            "uniform Smooth did not average curve neighbors"
        );
        assert_eq!(point_float("heat", &smooth), vec![6.0, 0.0, 6.0, 8.0, 0.0, 8.0]);

        let constrained =
            group::init(None, group::Owner::Point, "locked", 6, &|point| point == 1 || point == 4).unwrap();
        let locked = ok(&curves, &|p| {
            p.constrained_points = Some(constrained.clone());
            p.iterations = 3;
            p.mode = attribute_blur::Mode::Laplacian(0.5);
        });
        assert!(positions(&locked).y[1] == 3.0 && positions(&locked).y[4] == 4.0, "constrained points moved");

        let mut y = positions(&grid(2, 2, 2.0)).y.to_vec();
        y[4] = 3.0;
        let grid_value = with_y(&grid(2, 2, 2.0), y);
        let free = ok(&grid_value, &|p| p.mode = attribute_blur::Mode::Laplacian(1.0));
        let pinned = ok(&grid_value, &|p| {
            p.boundary = Boundary::SmoothUnshared;
            p.mode = attribute_blur::Mode::Laplacian(1.0);
        });
        let (source, free, pinned) = (positions(&grid_value), positions(&free), positions(&pinned));
        assert!(pinned.y[4] < source.y[4], "interior point was not smoothed");
        for point in (0..9).filter(|&point| point != 4) {
            assert!(
                pinned.x[point] == source.x[point]
                    && pinned.y[point] == source.y[point]
                    && pinned.z[point] == source.z[point],
                "unshared-boundary Smooth moved a boundary point"
            );
        }
        assert!(free.y.iter().zip(source.y.iter()).any(|(a, b)| a != b), "free Smooth was an identity");

        let with_normals = normal_ops::run(
            &normal_ops::Params {
                cancel: None,
                grain: None,
                selection: None,
                primitives: None,
                owner: Owner::Point,
                weighting: normal_ops::Weighting::FaceArea,
                cusp_angle: std::f64::consts::PI,
                attribute: "N".to_owned(),
            },
            &grid_value,
        )
        .unwrap();
        let recomputed = ok(&with_normals, &|p| {
            p.boundary = Boundary::SmoothUnshared;
            p.mode = attribute_blur::Mode::Laplacian(0.5);
        });
        assert!(
            geometry::find_attribute(Owner::Point, "N", &recomputed).is_some(),
            "Smooth did not recompute existing normals"
        );
        let invalidated = ok(&with_normals, &|p| {
            p.recompute_normals = false;
            p.mode = attribute_blur::Mode::Laplacian(0.5);
        });
        assert!(
            geometry::find_attribute(Owner::Point, "N", &invalidated).is_none()
                && geometry::find_attribute(Owner::Vertex, "N", &invalidated).is_none(),
            "Smooth retained stale normals"
        );
        let authored_n = attribute::create_owned(
            "N",
            Owner::Point,
            Storage::Float3(
                packed::Float3::View::of_owned_exn(
                    vec![0.0, 1.0, 0.0, 0.0, 1.0, 0.0],
                    vec![0.0; 6],
                    vec![0.0; 6],
                )
                .unwrap(),
            ),
        )
        .unwrap();
        let smoothed_n = ok(&with_attribute(&curves, authored_n), &|p| {
            p.mode = attribute_blur::Mode::Laplacian(1.0);
            p.attributes = "P N".to_owned();
        });
        let found = geometry::find_attribute(Owner::Point, "N", &smoothed_n).unwrap();
        match attribute::View::storage(&found).unwrap() {
            Storage::Float3(values) => {
                let values = packed::Float3::View::view(values).unwrap();
                assert_eq!(
                    values.x.to_vec(),
                    vec![1.0, 0.0, 1.0, 1.0, 0.0, 1.0],
                    "explicitly smoothed N was replaced by geometric normals"
                );
            }
            _ => panic!("smoothed N has unexpected storage"),
        }

        let wrong_length = group::init(None, group::Owner::Point, "wrong_length", 1, &|_| true).unwrap();
        expect_code(
            run_with(&grid_value, &|p| p.constrained_points = Some(wrong_length.clone())),
            "invalid_selection",
        );
        expect_code(run_with(&grid_value, &|p| p.grain = Some(0)), "invalid_parameter");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        expect_code(run_with(&grid_value, &|p| p.cancel = Some(token.clone())), "cancelled");
    }

    #[test]
    fn one_and_four_domains_agree_on_a_displaced_grid() {
        let base = grid(40, 30, 4.0);
        let count = geometry::point_count(&base);
        let y: Vec<f64> = (0..count).map(|point| ((point * 37) % 101) as f64 / 101.0 * 0.8).collect();
        let weight = attribute::create_owned(
            "weight",
            Owner::Point,
            Storage::Float((0..count).map(|point| 0.25 + ((point % 257) as f64 / 256.0)).collect()),
        )
        .unwrap();
        let scale = with_attribute(&with_y(&base, y), weight);
        let run_domains = |domains: i64| {
            let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                *slot.lock().unwrap() = Some(ok(&scale, &|p| {
                    p.grain = Some(64);
                    p.iterations = 8;
                    p.mode = attribute_blur::Mode::CustomSteps { odd: 0.43, even: -0.45 };
                    p.boundary = Boundary::SmoothUnshared;
                    p.attributes = "P weight".to_owned();
                }));
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        let one = run_domains(1);
        let many = run_domains(4);
        assert!(bits_equal(&one, &many), "one-domain and four-domain Smooth geometry differ");
        assert!(
            geometry::point_count(&one) == count
                && geometry::vertex_count(&one) == geometry::vertex_count(&scale)
                && geometry::primitive_count(&one) == geometry::primitive_count(&scale),
            "Smooth changed topology cardinality"
        );
    }
}
