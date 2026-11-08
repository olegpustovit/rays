//! Blur canonical `P` and matching point-owned float attributes over topology connectivity.

use std::sync::Arc;
use rdk_core::{
    attribute, attribute_pattern, cancel, error, geometry, group, packed, support, topology_index,
};
use rdk_core::error::Code;

use crate::attribute_ops::{make_error};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mode {
    Laplacian(f64),
    CustomSteps { odd: f64, even: f64 },
}

pub struct Params {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<group::T>,
    pub iterations: i64,
    pub mode: Mode,
    pub pattern: String,
}

enum Target {
    Position,
    Float(String, usize),
    Float2(String, usize),
    Float3(String, usize),
    Float4(String, usize),
}

const OPERATION: &str = "attribute_blur";
const PREFIX: &str = "Rdk.Attribute_ops.blur_points: ";

fn invalid(message: &str) -> error::T {
    make_error(OPERATION, Code::InvalidBlur, &format!("{PREFIX}{message}"))
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run_inner(params, geometry_value)
        .map_err(|failure| {
            if error::code(&failure) == Code::InvalidParameter {
                failure
            } else {
                error::guard(failure, OPERATION, Code::InvalidBlur)
            }
        })
}

fn run_inner(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(make_error(
            OPERATION,
            Code::InvalidParameter,
            &format!("{PREFIX}grain must be positive"),
        ));
    }
    if params.iterations < 0 {
        return Err(invalid("iterations must be non-negative"));
    }
    let steps_valid = match params.mode {
        Mode::Laplacian(step) => step.is_finite(),
        Mode::CustomSteps { odd, even } => odd.is_finite() && even.is_finite(),
    };
    if !steps_valid {
        return Err(invalid("step sizes must be finite"));
    }
    let point_count = geometry::point_count(geometry_value);
    if let Some(selection) = params.selection.as_ref() {
        if group::owner(selection) != group::Owner::Point || group::length(selection) != point_count {
            return Err(invalid("selection must be a matching point group"));
        }
    }
    let nothing = crate::attribute_ops::is_blank(&params.pattern)
        || params.iterations == 0
        || match params.selection.as_ref() {
            Some(selection) => group::cardinality(selection) == 0,
            None => point_count == 0,
        };
    if nothing {
        return Ok(geometry_value.clone());
    }
    let compiled = attribute_pattern::compile(&params.pattern)?;
    let mut targets: Vec<Target> = Vec::new();
    let mut original: Vec<Vec<f64>> = Vec::new();
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    if attribute_pattern::matches(&compiled, "P") {
        targets.push(Target::Position);
        original.push(positions.x.to_vec());
        original.push(positions.y.to_vec());
        original.push(positions.z.to_vec());
    }
    for item in geometry::attributes(geometry_value) {
        if attribute::owner(&item) != attribute::Owner::Point
            || !attribute_pattern::matches(&compiled, attribute::name(&item))
        {
            continue;
        }
        let name = attribute::name(&item).to_owned();
        let first = original.len();
        match attribute::view::storage(&item)? {
            attribute::Storage::Float(values) => {
                targets.push(Target::Float(name, first));
                original.push(values.to_vec());
            }
            attribute::Storage::Float2(values) => {
                let view = packed::float2::view::view(values);
                targets.push(Target::Float2(name, first));
                original.push(view.x.to_vec());
                original.push(view.y.to_vec());
            }
            attribute::Storage::Float3(values) => {
                let view = packed::float3::view::view(values)?;
                targets.push(Target::Float3(name, first));
                original.push(view.x.to_vec());
                original.push(view.y.to_vec());
                original.push(view.z.to_vec());
            }
            attribute::Storage::Float4(values) => {
                let view = packed::float4::view::view(values);
                targets.push(Target::Float4(name, first));
                original.push(view.x.to_vec());
                original.push(view.y.to_vec());
                original.push(view.z.to_vec());
                original.push(view.w.to_vec());
            }
            _ => {}
        }
    }
    if targets.is_empty() {
        return Ok(geometry_value.clone());
    }
    let count = point_count as usize;
    for (plane, values) in original.iter().enumerate() {
        for point in 0..count {
            cancel::poll(cancel_token, point as i64, 16_383)?;
            if !values[point].is_finite() {
                return Err(invalid(&format!(
                    "plane {plane} contains a non-finite value at point {point}"
                )));
            }
        }
    }
    let topology_index_value = topology_index::create(cancel_token, geometry::topology(geometry_value))?;
    let topology = topology_index::view::view(&topology_index_value);
    let mut current = original.clone();
    let mut next = original.clone();
    for iteration in 0..params.iterations {
        cancel::check_opt(cancel_token)?;
        let step = match params.mode {
            Mode::Laplacian(step) => step,
            Mode::CustomSteps { odd, even } => {
                if iteration & 1 == 0 {
                    odd
                } else {
                    even
                }
            }
        };
        {
            let source_planes = &current;
            let selection = params.selection.as_ref();
            let topology = &topology;
            let windows: Vec<support::Windows<f64>> =
                next.iter_mut().map(|plane| support::Windows::even(plane, grain)).collect();
            support::run_indexed_ranges(grain, point_count, &|range, first_point, last_point| {
                let mut out: Vec<&mut [f64]> = windows.iter().map(|window| window.take(range)).collect();
                for point in first_point..last_point {
                    cancel::poll(cancel_token, point, 4095)?;
                    let update = match selection {
                        None => true,
                        Some(selection) => group::mem(point, selection),
                    };
                    let at = (point - first_point) as usize;
                    for (plane, source) in source_planes.iter().enumerate() {
                        let here = source[point as usize];
                        if !update {
                            out[plane][at] = here;
                            continue;
                        }
                        let first = topology.point_edge_offsets[point as usize] as usize;
                        let last = topology.point_edge_offsets[point as usize + 1] as usize;
                        let mut sum = 0.0_f64;
                        let mut total = 0.0_f64;
                        for local in first..last {
                            let edge = topology.point_edges[local] as usize;
                            let a = topology.edge_a[edge];
                            let b = topology.edge_b[edge];
                            let neighbor = if a == point { b } else { a };
                            if neighbor != point {
                                sum += source[neighbor as usize];
                                total += 1.0;
                            }
                        }
                        if total == 0.0 {
                            out[plane][at] = here;
                        } else {
                            let average = sum / total;
                            out[plane][at] = here + step * (average - here);
                        }
                    }
                }
                Ok(())
            })?;
        }
        std::mem::swap(&mut current, &mut next);
    }
    let output = current;
    for (plane, values) in output.iter().enumerate() {
        if let Some(point) = values.iter().position(|value| !value.is_finite()) {
            return Err(invalid(&format!("output plane {plane} is non-finite at point {point}")));
        }
    }
    let mut positions_changed = false;
    for target in &targets {
        if let Target::Position = target {
            positions_changed = (0..count).any(|point| {
                output[0][point] != positions.x[point]
                    || output[1][point] != positions.y[point]
                    || output[2][point] != positions.z[point]
            });
        }
    }
    let mut result = if positions_changed {
        let moved = geometry::with_positions(
            packed::float3::view::of_owned_exn(output[0].clone(), output[1].clone(), output[2].clone())?,
            geometry_value,
        )?;
        let moved = geometry::without_attribute(attribute::Owner::Point, "N", &moved);
        geometry::without_attribute(attribute::Owner::Vertex, "N", &moved)
    } else {
        geometry_value.clone()
    };
    for target in &targets {
        let (name, storage) = match target {
            Target::Position => continue,
            Target::Float(name, first) => (name, attribute::Storage::Float(Arc::new(output[*first].clone()))),
            Target::Float2(name, first) => (
                name,
                attribute::Storage::Float2(packed::float2::of_owned(
                    output[*first].clone(),
                    output[first + 1].clone(),
                )?),
            ),
            Target::Float3(name, first) => (
                name,
                attribute::Storage::Float3(packed::float3::view::of_owned_exn(
                    output[*first].clone(),
                    output[first + 1].clone(),
                    output[first + 2].clone(),
                )?),
            ),
            Target::Float4(name, first) => (
                name,
                attribute::Storage::Float4(packed::float4::of_owned(
                    output[*first].clone(),
                    output[first + 1].clone(),
                    output[first + 2].clone(),
                    output[first + 3].clone(),
                )?),
            ),
        };
        let item = attribute::create_owned(name, attribute::Owner::Point, storage)?;
        result = geometry::with_attribute(item, &result)?;
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::{self, fixtures::*, fixtures2::*};
    use std::sync::Arc;
    use attribute::Owner::{Point, Vertex};

    fn path(count: i64) -> geometry::T {
        let values: Vec<(f64, f64, f64)> = (0..count).map(|i| (i as f64, 0.0, 0.0)).collect();
        crate::line_geometry::polyline(&crate::line_geometry::Params { closed: false }, &values).unwrap()
    }

    fn params(pattern: &str, iterations: i64, mode: Mode) -> Params {
        Params { cancel: None, grain: None, selection: None, iterations, mode, pattern: pattern.to_owned() }
    }

    #[test]
    fn laplacian_blur_of_positions_and_attributes() {
        let g = add_values(Point, "v", vec![0.0, 10.0, 0.0, 10.0, 0.0], &path(5));
        let g = add_float3s(Point, "N", vec![0.0; 5], vec![1.0; 5], vec![0.0; 5], &g);
        let g = add_float3s(Vertex, "N", vec![0.0; 5], vec![1.0; 5], vec![0.0; 5], &g);
        let attribute_blurred = attribute_ops::blur_points(&params("v", 1, Mode::Laplacian(1.0)), &g).unwrap();
        assert_eq!(floats(Point, "v", &attribute_blurred), vec![10.0, 0.0, 10.0, 0.0, 10.0]);
        assert!(has(Point, "N", &attribute_blurred), "attribute-only blur keeps normals");
        let moved = attribute_ops::blur_points(&params("P", 1, Mode::Laplacian(0.5)), &g).unwrap();
        let (x, _, _) = positions(&moved);
        assert_eq!(x, vec![0.5, 1.0, 2.0, 3.0, 3.5]);
        assert!(!has(Point, "N", &moved) && !has(Vertex, "N", &moved), "moving P drops stale normals");
        let alternating =
            attribute_ops::blur_points(&params("v", 2, Mode::CustomSteps { odd: 1.0, even: 0.0 }), &g).unwrap();
        assert_eq!(floats(Point, "v", &alternating), vec![10.0, 0.0, 10.0, 0.0, 10.0]);
        let identity = attribute_ops::blur_points(&params("  ", 3, Mode::Laplacian(0.5)), &g).unwrap();
        assert_eq!(geometry::data_id(&identity), geometry::data_id(&g));
        let none = attribute_ops::blur_points(&params("v", 0, Mode::Laplacian(0.5)), &g).unwrap();
        assert_eq!(geometry::data_id(&none), geometry::data_id(&g));
        let nothing_matches = attribute_ops::blur_points(&params("missing", 2, Mode::Laplacian(0.5)), &g).unwrap();
        assert_eq!(geometry::data_id(&nothing_matches), geometry::data_id(&g));
    }

    #[test]
    fn selection_limits_which_points_move() {
        let g = add_values(Point, "v", vec![0.0, 10.0, 0.0, 10.0, 0.0], &path(5));
        let mut p = params("v", 1, Mode::Laplacian(1.0));
        p.selection = Some(group_of(group::Owner::Point, "sel", &[0, 1], 5));
        let blurred = attribute_ops::blur_points(&p, &g).unwrap();
        assert_eq!(floats(Point, "v", &blurred), vec![10.0, 0.0, 0.0, 10.0, 0.0]);
        let mut empty = params("v", 1, Mode::Laplacian(1.0));
        empty.selection = Some(group_of(group::Owner::Point, "none", &[], 5));
        assert_eq!(geometry::data_id(&attribute_ops::blur_points(&empty, &g).unwrap()), geometry::data_id(&g));
        let mut wrong = params("v", 1, Mode::Laplacian(1.0));
        wrong.selection = Some(group_of(group::Owner::Primitive, "bad", &[], 1));
        assert_eq!(err_code(attribute_ops::blur_points(&wrong, &g)), "invalid_blur");
    }

    #[test]
    fn invalid_parameters_and_cancellation() {
        let g = add_values(Point, "v", vec![0.0, 10.0, 0.0, 10.0, 0.0], &path(5));
        assert_eq!(err_code(attribute_ops::blur_points(&params("v", -1, Mode::Laplacian(0.5)), &g)), "invalid_blur");
        assert_eq!(err_code(attribute_ops::blur_points(&params("v", 1, Mode::Laplacian(f64::NAN)), &g)), "invalid_blur");
        let mut grain = params("v", 1, Mode::Laplacian(0.5));
        grain.grain = Some(0);
        assert_eq!(err_code(attribute_ops::blur_points(&grain, &g)), "invalid_parameter");
        assert_eq!(err_code(attribute_ops::blur_points(&params("[", 1, Mode::Laplacian(0.5)), &g)), "invalid_blur");
        let bad = add_values(Point, "v", vec![0.0, f64::NAN, 0.0, 0.0, 0.0], &g);
        assert_eq!(err_code(attribute_ops::blur_points(&params("v", 1, Mode::Laplacian(0.5)), &bad)), "invalid_blur");
        let overflow = add_values(Point, "v", vec![f64::MAX, f64::MAX, f64::MIN, f64::MAX, f64::MAX], &g);
        assert_eq!(err_code(attribute_ops::blur_points(&params("v", 1, Mode::Laplacian(1e300)), &overflow)), "invalid_blur");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut cancelled = params("v", 1, Mode::Laplacian(0.5));
        cancelled.cancel = Some(token);
        assert_eq!(err_code(attribute_ops::blur_points(&cancelled, &g)), "cancelled");
    }

    /// `lib/rdk/test_rdk_misc.ml`: the tuple planes of a float2 and a float4 attribute blur together.
    #[test]
    fn tuple_attributes_blur_plane_by_plane() {
        let g = add_values(Point, "value", vec![0.0, 10.0, 0.0], &path(3));
        let uniform = attribute_ops::blur_points(&params("value", 1, Mode::Laplacian(1.0)), &g).unwrap();
        assert_eq!(floats(Point, "value", &uniform), vec![10.0, 0.0, 10.0]);
        let custom = attribute_ops::blur_points(
            &params("value", 2, Mode::CustomSteps { odd: 1.0, even: 0.0 }), &g,
        ).unwrap();
        assert_eq!(floats(Point, "value", &custom), vec![10.0, 0.0, 10.0]);
        let uv = attribute::Storage::Float2(packed::float2::of_owned(vec![0.0, 10.0, 0.0], vec![2.0, 4.0, 2.0]).unwrap());
        let color = attribute::Storage::Float4(
            packed::float4::of_owned(
                vec![0.0, 1.0, 0.0],
                vec![0.0, 2.0, 0.0],
                vec![0.0, 3.0, 0.0],
                vec![1.0, 1.0, 1.0],
            )
            .unwrap(),
        );
        let g = add_storage(Point, "f_uv", uv, &g);
        let g = add_storage(Point, "f_color", color, &g);
        let blurred = attribute_ops::blur_points(&params("f_*", 1, Mode::Laplacian(1.0)), &g).unwrap();
        let uv = match attribute::storage(&find(Point, "f_uv", &blurred)).unwrap() {
            attribute::Storage::Float2(values) => packed::float2::view::view(&values),
            _ => panic!("Attribute Blur float2 storage"),
        };
        let color = match attribute::storage(&find(Point, "f_color", &blurred)).unwrap() {
            attribute::Storage::Float4(values) => packed::float4::view::view(&values),
            _ => panic!("Attribute Blur float4 storage"),
        };
        assert_eq!(uv.x.to_vec(), vec![10.0, 0.0, 10.0]);
        assert_eq!(uv.y.to_vec(), vec![4.0, 2.0, 4.0]);
        assert_eq!(color.z.to_vec(), vec![3.0, 0.0, 3.0]);
        assert_eq!(color.w.to_vec(), vec![1.0, 1.0, 1.0]);
    }

    #[test]
    fn blur_is_exact_across_domains() {
        let count = 5_000_i64;
        let g = add_values(Point, "v", (0..count).map(|i| ((i * 37) % 11) as f64).collect(), &path(count));
        let run = |domains: i64| {
            on_domains(domains, &|| {
                let mut p = params("P v", 6, Mode::CustomSteps { odd: 0.7, even: 0.3 });
                p.grain = Some(97);
                attribute_ops::blur_points(&p, &g).unwrap()
            })
        };
        let (one, four) = (run(1), run(4));
        assert_eq!(signature(&one), signature(&four));
        assert_ne!(signature(&one), signature(&g));
    }
}
