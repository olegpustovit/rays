//! Packed instance materialization, duplication, and copy-to-points.
//!
//! Output ranges are filled by disjoint parallel ranges joined in range order, so every result is
//! byte-identical to the sequential path.
use std::sync::Arc;

use rays_math::mat4;
use rays_math::text_array::TextArray;
use rdk_core::{attribute, cancel, edge_group, error, geometry, group, packed, ragged_ops, support, topology, topology_index};
use rdk_core::error::Code;

const OPERATION: &str = "materialize_instances";
const NAME: &str = "Rdk.Instance_copy.materialize_instances";
const DEFAULT_GRAIN: i64 = 16_384;


fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

fn fail(message: String) -> error::T {
    make(Code::InvalidParameter, message)
}

fn lift(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        error::cancelled(OPERATION)
    } else {
        fail(error::message(&value).to_owned())
    }
}

fn build<T: Default + Clone + Send>(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    length: i64,
    value: &(dyn Fn(i64) -> T + Sync),
) -> Result<Vec<T>, error::T> {
    let mut output = vec![T::default(); length.max(0) as usize];
    let windows = support::Windows::even(&mut output, grain);
    support::run_indexed_ranges(grain, length, &|range, first, last| {
        cancel::check_opt(cancel_value)?;
        for (index, slot) in (first..last).zip(windows.take(range)) {
            *slot = value(index);
        }
        Ok(())
    })
    .map_err(lift)?;
    Ok(output)
}

type Xyz = (Vec<f64>, Vec<f64>, Vec<f64>);

fn build_xyz(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    length: i64,
    value: &(dyn Fn(i64) -> (f64, f64, f64) + Sync),
) -> Result<Xyz, error::T> {
    let length_usize = length.max(0) as usize;
    let (mut x, mut y, mut z) = (vec![0.0; length_usize], vec![0.0; length_usize], vec![0.0; length_usize]);
    let (wx, wy, wz) = (
        support::Windows::even(&mut x, grain),
        support::Windows::even(&mut y, grain),
        support::Windows::even(&mut z, grain),
    );
    support::run_indexed_ranges(grain, length, &|range, first, last| {
        cancel::check_opt(cancel_value)?;
        let (ox, oy, oz) = (wx.take(range), wy.take(range), wz.take(range));
        for index in first..last {
            let at = (index - first) as usize;
            (ox[at], oy[at], oz[at]) = value(index);
        }
        Ok(())
    })
    .map_err(lift)?;
    Ok((x, y, z))
}

fn repeat_f64(cancel_value: Option<&cancel::T>, grain: i64, copies: i64, source: &[f64]) -> Result<Vec<f64>, error::T> {
    let length = source.len() as i64;
    if copies == 0 || length == 0 {
        return Ok(Vec::new());
    }
    build(cancel_value, grain, copies * length, &|index| source[(index % length) as usize])
}

fn repeat_i64(cancel_value: Option<&cancel::T>, grain: i64, copies: i64, source: &[i64]) -> Result<Vec<i64>, error::T> {
    let length = source.len() as i64;
    if copies == 0 || length == 0 {
        return Ok(Vec::new());
    }
    build(cancel_value, grain, copies * length, &|index| source[(index % length) as usize])
}

fn repeat_text(copies: i64, source: &TextArray) -> TextArray {
    let copies = copies.max(0) as usize;
    let mut output = TextArray::with_capacity(source.len() * copies, source.byte_len() * copies);
    for _ in 0..copies {
        output.extend(source.iter());
    }
    output
}

fn repeat_attribute(cancel_value: Option<&cancel::T>, grain: i64, copies: i64, item: &attribute::T) -> Result<attribute::T, error::T> {
    use attribute::Storage;
    if attribute::owner(item) == attribute::Owner::Detail {
        return Ok(item.clone());
    }
    let source = attribute::view::storage(item).map_err(lift)?;
    let storage = match source {
        Storage::Float(_) | Storage::Float2(_) | Storage::Float3(_) | Storage::Float4(_) => {
            attribute::map_float_planes(source, |plane| repeat_f64(cancel_value, grain, copies, plane))
                .map_err(lift)?.expect("a float-kind storage has planes")
        }
        Storage::Int(values) => Storage::Int(Arc::new(repeat_i64(cancel_value, grain, copies, values)?)),
        Storage::Text(values) => Storage::Text(Arc::new(repeat_text(copies, values))),
        Storage::IntArray(values) => {
            let source_count = packed::int_array::length(values);
            let mapping: Vec<i64> = (0..copies * source_count).map(|index| index % source_count).collect();
            Storage::IntArray(ragged_ops::remap_int(cancel_value, Some(grain), &mapping, values).map_err(lift)?)
        }
        Storage::FloatArray(values) => {
            let source_count = packed::float_array::length(values);
            let mapping: Vec<i64> = (0..copies * source_count).map(|index| index % source_count).collect();
            Storage::FloatArray(ragged_ops::remap_float(cancel_value, Some(grain), &mapping, values).map_err(lift)?)
        }
    };
    attribute::create_owned(attribute::name(item), attribute::owner(item), storage).map_err(lift)
}

fn repeat_group(cancel_value: Option<&cancel::T>, grain: i64, copies: i64, source: &group::T) -> Result<group::T, error::T> {
    let source_count = group::length(source);
    let target = group::init(
        Some(grain),
        group::owner(source),
        group::name(source),
        copies * source_count,
        &|index| Ok({
            cancel::poll(cancel_value, index, 4095)?;
            group::mem(index % source_count, source)
        }),
    )
    .map_err(lift)?;
    match group::view::order_view(source) {
        None => Ok(target),
        Some(source_order) => {
            let mut order = Vec::with_capacity((copies as usize) * source_order.len());
            for copy in 0..copies {
                cancel::poll(cancel_value, copy, 255).map_err(lift)?;
                order.extend(source_order.iter().map(|&element| copy * source_count + element));
            }
            group::view::with_owned_order(order, &target).map_err(lift)
        }
    }
}

type Rows = ((f64, f64, f64, f64), (f64, f64, f64, f64), (f64, f64, f64, f64), (f64, f64, f64, f64));

fn apply_row(rows: &Rows, px: f64, py: f64, pz: f64) -> (f64, f64, f64) {
    let ((m00, m01, m02, m03), (m10, m11, m12, m13), (m20, m21, m22, m23), (m30, m31, m32, m33)) = *rows;
    let ox = (m00 * px) + (m01 * py) + (m02 * pz) + m03;
    let oy = (m10 * px) + (m11 * py) + (m12 * pz) + m13;
    let oz = (m20 * px) + (m21 * py) + (m22 * pz) + m23;
    let ow = (m30 * px) + (m31 * py) + (m32 * pz) + m33;
    if ow.abs() <= 1e-12 { (ox, oy, oz) } else { (ox / ow, oy / ow, oz / ow) }
}

fn normal_row(rows: &Rows, sx: f64, sy: f64, sz: f64) -> (f64, f64, f64) {
    let ((m00, m01, m02, _), (m10, m11, m12, _), (m20, m21, m22, _), _) = *rows;
    let ox = (m00 * sx) + (m01 * sy) + (m02 * sz);
    let oy = (m10 * sx) + (m11 * sy) + (m12 * sz);
    let oz = (m20 * sx) + (m21 * sy) + (m22 * sz);
    let length = ((ox * ox) + (oy * oy) + (oz * oz)).sqrt();
    if length > 1e-20 { (ox / length, oy / length, oz / length) } else { (0.0, 0.0, 0.0) }
}

fn typed_normal(item: &attribute::T) -> Result<Option<packed::float3::T>, error::T> {
    let owner = attribute::owner(item);
    if attribute::name(item) == "N" && (owner == attribute::Owner::Point || owner == attribute::Owner::Vertex) {
        match attribute::get(&attribute::normal(owner), item).map_err(lift)? {
            Some(attribute::Storage::Float3(value)) => Ok(Some(value)),
            _ => Ok(None),
        }
    } else {
        Ok(None)
    }
}

fn transform_single_instance(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    matrix: &mat4::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source = packed::float3::view::view(geometry::positions(geometry_value)).map_err(lift)?;
    let count = source.x.len() as i64;
    let rows = mat4::to_rows(matrix);
    let (x, y, z) = build_xyz(cancel_value, grain, count, &|point| {
        let p = point as usize;
        apply_row(&rows, source.x[p], source.y[p], source.z[p])
    })?;
    let positions = packed::float3::view::of_owned_exn(x, y, z).map_err(lift)?;
    let mut output = geometry::with_positions(positions, geometry_value).map_err(lift)?;
    let owners = [attribute::Owner::Point, attribute::Owner::Vertex];
    match mat4::inverse(matrix) {
        None => {
            for owner in owners {
                if let Some(item) = geometry::find_attribute(owner, "N", geometry_value) {
                    if typed_normal(&item)?.is_some() {
                        output = geometry::without_attribute(owner, "N", &output);
                    }
                }
            }
        }
        Some(inverse) => {
            let normal_matrix = mat4::transpose(&inverse);
            let normal_rows = mat4::to_rows(&normal_matrix);
            for owner in owners {
                let Some(item) = geometry::find_attribute(owner, "N", geometry_value) else {
                    continue;
                };
                let Some(source) = typed_normal(&item)? else {
                    continue;
                };
                let source = packed::float3::view::view(&source).map_err(lift)?;
                let length = source.x.len() as i64;
                let (x, y, z) = build_xyz(cancel_value, grain, length, &|index| {
                    let i = index as usize;
                    normal_row(&normal_rows, source.x[i], source.y[i], source.z[i])
                })?;
                let value = packed::float3::view::of_owned_exn(x, y, z).map_err(lift)?;
                let created = attribute::create_key_owned(&attribute::normal(owner), attribute::Storage::Float3(value))
                    .map_err(lift)?;
                output = geometry::with_attribute(created, &output).map_err(lift)?;
            }
        }
    }
    Ok(output)
}

fn product(label: &str, total: i64, count: i64) -> Result<i64, error::T> {
    total
        .checked_mul(count)
        .ok_or_else(|| fail(format!("{NAME}: {label} output exceeds array limits")))
}

fn materialize(transforms: &[mat4::T], geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value: Option<&cancel::T> = None;
    let grain = DEFAULT_GRAIN;
    let matrices = transforms;
    for (instance, matrix) in matrices.iter().enumerate() {
        let ((a, b, c, d), (e, f, g, h), (i, j, k, l), (m, n, o, p)) = mat4::to_rows(matrix);
        if ![a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p].iter().all(|v| v.is_finite()) {
            return Err(fail(format!("{NAME}: transform {instance} must be finite")));
        }
    }
    let total = matrices.len() as i64;
    if total == 1 && mat4::nearly_equal(&matrices[0], &mat4::identity(), 0.0) {
        return Ok(geometry_value.clone());
    }
    if total == 1 {
        return transform_single_instance(cancel_value, grain, &matrices[0], geometry_value);
    }
    let source_points = geometry::point_count(geometry_value);
    let source_vertices = geometry::vertex_count(geometry_value);
    let source_primitives = geometry::primitive_count(geometry_value);
    if source_points == 0 && source_vertices == 0 && source_primitives == 0 {
        return Ok(geometry_value.clone());
    }
    let output_points = product("point", total, source_points)?;
    let output_vertices = product("vertex", total, source_vertices)?;
    let output_primitives = product("primitive", total, source_primitives)?;
    let source_positions = packed::float3::view::view(geometry::positions(geometry_value)).map_err(lift)?;
    let rows: Vec<Rows> = matrices.iter().map(mat4::to_rows).collect();
    let (x, y, z) = build_xyz(cancel_value, grain, output_points, &|index| {
        let copy = index / source_points;
        let point = (index - copy * source_points) as usize;
        apply_row(&rows[copy as usize], source_positions.x[point], source_positions.y[point], source_positions.z[point])
    })?;
    let source_topology = topology::view::view(geometry::topology(geometry_value)).map_err(lift)?;
    let vertex_points = build(cancel_value, grain, output_vertices, &|index| {
        let copy = index / source_vertices;
        let at = (index - copy * source_vertices) as usize;
        source_topology.vertex_points[at] + (copy * source_points) as u32
    })?;
    let mut primitive_offsets = build(cancel_value, grain, output_primitives, &|index| {
        let copy = index / source_primitives;
        let at = (index - copy * source_primitives) as usize;
        source_topology.primitive_offsets[at] + (copy * source_vertices) as u32
    })?;
    primitive_offsets.push(output_vertices as u32);
    let mut primitive_kinds: Vec<u8> = Vec::with_capacity(output_primitives as usize);
    for _ in 0..total {
        primitive_kinds.extend_from_slice(&source_topology.primitive_kinds);
    }
    let topology_value =
        topology::view::create_validated_owned(output_points, vertex_points, primitive_offsets, primitive_kinds)?;
    let source_attributes = geometry::attributes(geometry_value);
    let mut has_normals = false;
    for item in &source_attributes {
        if typed_normal(item)?.is_some() {
            has_normals = true;
        }
    }
    let normal_matrices: Option<Vec<Rows>> = if !has_normals {
        None
    } else {
        let mut output = Vec::with_capacity(matrices.len());
        let mut valid = true;
        for matrix in matrices {
            match mat4::inverse(matrix) {
                None => {
                    valid = false;
                    break;
                }
                Some(inverse) => output.push(mat4::to_rows(&mat4::transpose(&inverse))),
            }
        }
        if valid { Some(output) } else { None }
    };
    let mut attributes = Vec::new();
    for item in &source_attributes {
        match (typed_normal(item)?, &normal_matrices) {
            (Some(_), None) => {}
            (Some(source), Some(normal_matrices)) => {
                let source = packed::float3::view::view(&source).map_err(lift)?;
                let source_count = source.x.len() as i64;
                let (nx, ny, nz) = build_xyz(cancel_value, grain, total * source_count, &|index| {
                    let copy = index / source_count;
                    let at = (index - copy * source_count) as usize;
                    normal_row(&normal_matrices[copy as usize], source.x[at], source.y[at], source.z[at])
                })?;
                let value = packed::float3::view::of_owned_exn(nx, ny, nz).map_err(lift)?;
                let owner = attribute::owner(item);
                attributes.push(
                    attribute::create_key_owned(&attribute::normal(owner), attribute::Storage::Float3(value))
                        .map_err(lift)?,
                );
            }
            (None, _) => attributes.push(repeat_attribute(cancel_value, grain, total, item)?),
        }
    }
    let positions = packed::float3::view::of_owned_exn(x, y, z).map_err(lift)?;
    let mut edge_groups = Vec::new();
    let source_edge_groups = geometry::edge_groups(geometry_value);
    if !source_edge_groups.is_empty() {
        let source_index = topology_index::create(cancel_value, geometry::topology(geometry_value)).map_err(lift)?;
        for item in &source_edge_groups {
            edge_groups.push(
                edge_group::replicate_exact_copies(
                    cancel_value,
                    geometry::topology(geometry_value),
                    &source_index,
                    &topology_value,
                    total,
                    item,
                )
                .map_err(lift)?,
            );
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        groups.push(repeat_group(cancel_value, grain, total, &item)?);
    }
    geometry::create(positions, topology_value, Some(attributes), Some(groups), Some(edge_groups)).map_err(lift)
}

/// Materialize transform-major copies with exact topology and group ancestry.
pub fn materialize_instances(transforms: &[mat4::T], geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    materialize(transforms, geometry_value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mesh_merge;
    use rays_math::vec3;
    use rdk_core::topology::PrimitiveKind;
    use std::sync::Mutex;

    type P3 = (f64, f64, f64);

    fn polyline(points: &[P3], closed: bool) -> geometry::T {
        let kind = if closed { PrimitiveKind::ClosedPolyline } else { PrimitiveKind::OpenPolyline };
        let topology_value =
            topology::create_owned(points.len() as i64, (0..points.len() as u32).collect(), vec![0, points.len() as u32], vec![kind]).unwrap();
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None).unwrap()
    }

    fn point_cloud(points: &[P3]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        geometry::create(
            packed::float3::view::of_owned_exn(x, y, z).unwrap(),
            topology::empty(points.len() as i64).unwrap(),
            None,
            None,
            None,
        ).unwrap()
    }

    fn transforms(count: usize) -> Vec<mat4::T> {
        (0..count)
            .map(|instance| {
                let angle = instance as f64 * 0.17;
                mat4::mul(
                    &mat4::translation(vec3::create(instance as f64 * 1.25, angle.sin() * 0.5, angle.cos() * 0.25)),
                    &mat4::mul(
                        &mat4::rotation_y(angle),
                        &mat4::scaling(vec3::create(1.0, 0.75 + (instance % 3) as f64 * 0.125, 1.0)),
                    ),
                )
            })
            .collect()
    }

    /// Positions of `source` under `matrix` (one instance, no normals).
    fn transformed(matrix: &mat4::T, source: &geometry::T) -> geometry::T {
        let view = packed::float3::view::view(geometry::positions(source)).unwrap();
        let rows = mat4::to_rows(matrix);
        let moved: Vec<P3> = (0..view.x.len()).map(|i| apply_row(&rows, view.x[i], view.y[i], view.z[i])).collect();
        let x = moved.iter().map(|p| p.0).collect();
        let y = moved.iter().map(|p| p.1).collect();
        let z = moved.iter().map(|p| p.2).collect();
        geometry::with_positions(packed::float3::view::of_owned_exn(x, y, z).unwrap(), source).unwrap()
    }

    fn same_positions_and_topology(left: &geometry::T, right: &geometry::T) -> bool {
        let lp = packed::float3::view::view(geometry::positions(left)).unwrap();
        let rp = packed::float3::view::view(geometry::positions(right)).unwrap();
        let lt = topology::view::view(geometry::topology(left)).unwrap();
        let rt = topology::view::view(geometry::topology(right)).unwrap();
        lp.x == rp.x
            && lp.y == rp.y
            && lp.z == rp.z
            && lt.point_count == rt.point_count
            && lt.vertex_points == rt.vertex_points
            && lt.primitive_offsets == rt.primitive_offsets
            && lt.primitive_kinds == rt.primitive_kinds
    }

    #[test]
    fn instance_materialization_cardinality() {
        let triangle = polyline(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)], true);
        let scale_count = 100_000;
        let scaled = materialize_instances(&vec![mat4::identity(); scale_count], &triangle).unwrap();
        assert!(
            geometry::point_count(&scaled) == scale_count as i64 * 3
                && geometry::vertex_count(&scaled) == scale_count as i64 * 3
                && geometry::primitive_count(&scaled) == scale_count as i64,
            "scale materialization cardinality is not exact"
        );
    }

    #[test]
    fn matches_merged_transformed_copies() {
        let source = polyline(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)], false);
        let matrices = transforms(7);
        let output = materialize_instances(&matrices, &source).unwrap();
        let copies: Vec<geometry::T> = matrices.iter().map(|matrix| transformed(matrix, &source)).collect();
        let merged = mesh_merge::run(
            &mesh_merge::RunParams { cancel: None, grain: Some(1), source_attribute: None, source_base: None },
            &copies,
        ).unwrap();
        assert!(same_positions_and_topology(&output, &merged), "legacy merge of transformed copies differs");
    }

    #[test]
    fn single_instance_and_errors() {
        let source = polyline(&[(0., 0., 0.), (1., 0., 0.)], false);
        let same = materialize_instances(&[mat4::identity()], &source).unwrap();
        assert!(same_positions_and_topology(&same, &source), "identity single instance");
        let moved = materialize_instances(&[mat4::translation(vec3::create(1.0, 2.0, 3.0))], &source).unwrap();
        let view = packed::float3::view::view(geometry::positions(&moved)).unwrap();
        assert!(view.x[..] == [1., 2.] && view.y[..] == [2., 2.] && view.z[..] == [3., 3.], "translated single instance");
        let bad = mat4::of_rows((f64::NAN, 0., 0., 0.), (0., 1., 0., 0.), (0., 0., 1., 0.), (0., 0., 0., 1.));
        match materialize_instances(&[mat4::identity(), bad], &source) {
            Err(failure) => {
                assert_eq!(error::code(&failure), Code::InvalidParameter);
                assert_eq!(
                    error::message(&failure),
                    "Rdk.Instance_copy.materialize_instances: transform 1 must be finite"
                );
            }
            Ok(_) => panic!("expected error"),
        }
        let empty = materialize_instances(&[], &source).unwrap();
        assert_eq!(geometry::point_count(&empty), 0);
    }

    #[test]
    fn payload_repeats_per_instance() {
        let source = polyline(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)], true);
        let weight = attribute::create_owned("weight", attribute::Owner::Point, attribute::Storage::Float(Arc::new(vec![1., 2., 3.]))).unwrap();
        let detail = attribute::create_owned("tag", attribute::Owner::Detail, attribute::Storage::Int(Arc::new(vec![5]))).unwrap();
        let ordered = group::ordered(group::Owner::Point, "pts", 3, &[2, 0]).unwrap();
        let mut source = geometry::with_attribute(weight, &source).unwrap();
        source = geometry::with_attribute(detail, &source).unwrap();
        source = geometry::with_group(ordered, &source).unwrap();
        for width in 2..=4 {
            let template = attribute::Storage::Float4(packed::float4::of_owned(vec![], vec![], vec![], vec![]).unwrap());
            let planes = (0..width).map(|p| vec![p as f64, 10.0 + p as f64, 20.0 + p as f64]).collect();
            let storage = match width {
                2 => attribute::Storage::Float2(packed::float2::of_owned(vec![], vec![]).unwrap()),
                3 => attribute::Storage::Float3(packed::float3::of_owned(vec![], vec![], vec![]).unwrap()),
                _ => template,
            };
            let item = attribute::create_owned(&format!("tuple{width}"), attribute::Owner::Point, attribute::of_float_planes(&storage, planes).unwrap()).unwrap();
            source = geometry::with_attribute(item, &source).unwrap();
        }
        let output = materialize_instances(&transforms(3), &source).unwrap();
        let weights = geometry::find_attribute(attribute::Owner::Point, "weight", &output).unwrap();
        match attribute::view::storage(&weights).unwrap() {
            attribute::Storage::Float(values) => assert_eq!(values[..], [1., 2., 3., 1., 2., 3., 1., 2., 3.]),
            _ => panic!("weight storage"),
        }
        for item in geometry::attributes(&source) {
            let storage = attribute::view::storage(&item).unwrap();
            if let Some(planes) = attribute::float_planes(storage).unwrap() {
                let target = geometry::find_attribute(attribute::Owner::Point, attribute::name(&item), &output).unwrap();
                assert_eq!(attribute::kind_name(&target), attribute::kind_name(&item));
                let target_planes = attribute::float_planes(attribute::view::storage(&target).unwrap()).unwrap().unwrap();
                for (source, target) in planes.iter().zip(target_planes) { assert_eq!(target, source.repeat(3)); }
            }
        }
        assert!(geometry::find_attribute(attribute::Owner::Detail, "tag", &output).is_some());
        let members = geometry::find_group(group::Owner::Point, "pts", &output).unwrap();
        assert_eq!(group::ordered_elements(&members), Some(vec![2, 0, 5, 3, 8, 6]));
        assert_eq!(group::cardinality(&members), 6);
    }

    fn one_and_four_domains(work: &(dyn Fn() -> geometry::T + Sync)) -> (geometry::T, geometry::T) {
        let run = |domains: i64| {
            let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                *slot.lock().unwrap() = Some(work());
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        (run(1), run(4))
    }

    #[test]
    fn parallel_exact() {
        let source = polyline(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.), (1., 1., 1.)], true);
        let matrices = transforms(5_000);
        let (one, four) = one_and_four_domains(&|| materialize_instances(&matrices, &source).unwrap());
        assert!(same_positions_and_topology(&one, &four), "one/four-domain mismatch");
    }

    /// it builds the fixtures a Copy to Points run consumes and
    /// asserts nothing on them; here the fixtures must still construct and merge.
    #[test]
    fn copy_to_points_fixtures_construct() {
        let float3 = |x: f64, y: f64, z: f64| {
            attribute::Storage::Float3(packed::float3::view::of_owned_exn(vec![x], vec![y], vec![z]).unwrap())
        };
        let target_n = attribute::create_owned(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(packed::float3::view::of_owned_exn(vec![1., 0.], vec![0., 1.], vec![0., 0.]).unwrap()),
        ).unwrap();
        assert_eq!(attribute::length(&target_n), 2);
        let quarter_turn = 0.5_f64.sqrt();
        let _rot = attribute::create_owned(
            "rot",
            attribute::Owner::Point,
            attribute::Storage::Float4(
                packed::float4::of_owned(vec![0.], vec![0.], vec![quarter_turn], vec![quarter_turn]).map_err(lift).unwrap(),
            ),
        ).unwrap();
        let _trans = attribute::create_owned("trans", attribute::Owner::Point, float3(3., 0., 0.)).unwrap();
        let _pivot = attribute::create_owned("pivot", attribute::Owner::Point, float3(1., 0., 0.)).unwrap();
        let restricted_source = mesh_merge::run(
            &mesh_merge::RunParams { cancel: None, grain: None, source_attribute: None, source_base: None },
            &[polyline(&[(0., 0., 0.), (1., 0., 0.)], false), polyline(&[(10., 0., 0.), (12., 0., 0.)], false)],
        ).unwrap();
        let source_second = group::ordered(group::Owner::Primitive, "source_second", 2, &[1]).unwrap();
        let restricted_source = geometry::with_group(source_second, &restricted_source).unwrap();
        let restricted_targets = point_cloud(&[(0., 0., 0.), (0., 10., 0.), (0., 20., 0.)]);
        let scale = attribute::create_owned("pscale", attribute::Owner::Point, attribute::Storage::Float(Arc::new(vec![1., 9., 2.]))).unwrap();
        let restricted_targets = geometry::with_attribute(scale, &restricted_targets).unwrap();
        assert!(geometry::payload_bytes(&restricted_source) > 0 && geometry::payload_bytes(&restricted_targets) > 0);
        let profile = packed::float_array::create_owned(vec![0, 2, 4], vec![10., 20., 30., 40.]).unwrap();
        let profile = attribute::create_owned("profile", attribute::Owner::Point, attribute::Storage::FloatArray(profile)).unwrap();
        let transfer_source = geometry::with_attribute(profile, &polyline(&[(0., 0., 0.), (1., 0., 0.)], false)).unwrap();
        let transfer_source = geometry::with_group(
            group::init(None, group::Owner::Point, "union_mask", 2, &|point| Ok(point == 0)).unwrap(),
            &transfer_source,
        ).unwrap();
        assert!(geometry::payload_bytes(&transfer_source) > 0);
    }
}
