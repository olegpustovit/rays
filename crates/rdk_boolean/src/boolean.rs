//! Port of lib/rdk/boolean/boolean.ml: exact treatment-aware polygon Boolean products.
use std::sync::Arc;

use rdk_core::{attribute, cancel, error, geometry, group};

use crate::boolean_extract;
use crate::boolean_materialization::{self, Detriangulation};
use crate::boolean_payload::{self, PointConflict};
use crate::boolean_seam;
use crate::boolean_solid::{self, Operation, Treatment};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeamPoints {
    SharedSeamPoints,
    SplitSeamPoints,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeamOutput {
    SeamCurves,
    CoincidentPatches,
}

fn make(operation: &str, code: &str, message: String) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message, None).unwrap_or_else(|e| e)
}

fn fail_with(code: &str, message: &str) -> error::T {
    make("boolean", code, message.to_owned())
}

fn invalid_output(operation: &str, failure: &error::T) -> error::T {
    make(operation, "invalid_output", error::message(failure).to_owned())
}

/// OCaml `String.trim`.
fn trimmed_empty(name: &str) -> bool {
    name.trim_matches(|c| matches!(c, ' ' | '\x0c' | '\n' | '\r' | '\t')).is_empty()
}

fn has_duplicate(names: &[String]) -> bool {
    let mut ordered: Vec<&String> = names.iter().collect();
    ordered.sort();
    ordered.windows(2).any(|pair| pair[0] == pair[1])
}

fn has_corner_payload(geometry_value: &geometry::T) -> bool {
    geometry::View::attributes(geometry_value).iter().any(|item| {
        matches!(attribute::owner(item), attribute::Owner::Point | attribute::Owner::Vertex)
    }) || geometry::groups(geometry_value)
        .iter()
        .any(|item| matches!(group::owner(item), group::Owner::Point | group::Owner::Vertex))
        || !geometry::edge_groups(geometry_value).is_empty()
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub operation: Operation,
    pub left_treatment: Treatment,
    pub right_treatment: Treatment,
    pub resolve_left_self_intersections: bool,
    pub resolve_right_self_intersections: bool,
    pub point_conflict: PointConflict,
    pub point_tolerance: f64,
    pub tiny_seam_threshold: f64,
    pub cleanup_max_batches: i64,
    pub strict_cleanup: bool,
    pub seam_points: SeamPoints,
    pub detriangulation: Detriangulation,
    pub assume_flat: bool,
    pub require_closed: Option<bool>,
    pub piece_attribute: Option<String>,
    pub left_piece_group: Option<String>,
    pub overlap_piece_group: Option<String>,
    pub right_piece_group: Option<String>,
    pub right: geometry::T,
}

/// Build one exact two-operand arrangement and evaluate a polygon Boolean. Both operands may be
/// oriented solids or zero-volume surfaces. Point/vertex/primitive attributes and ordinary/native
/// groups transfer through exact source ancestry; conflicting point fields are rejected or
/// promoted explicitly. Output is triangle-only by default. `Shatter` requires two solids and
/// emits A-only, overlap, and B-only boundaries in that stable order.
pub fn run(params: &Params, left: &geometry::T) -> Result<geometry::T, error::T> {
    if params.grain <= 0 {
        return Err(fail_with("invalid_parameter", "grain must be positive"));
    }
    if !params.point_tolerance.is_finite() || params.point_tolerance < 0. {
        return Err(fail_with("invalid_parameter", "point_tolerance must be finite and non-negative"));
    }
    if !params.tiny_seam_threshold.is_finite() || params.tiny_seam_threshold < 0. {
        return Err(fail_with("invalid_parameter", "tiny_seam_threshold must be finite and non-negative"));
    }
    if params.cleanup_max_batches <= 0 {
        return Err(fail_with("invalid_parameter", "cleanup_max_batches must be positive"));
    }
    if let Some(name) = &params.piece_attribute {
        if trimmed_empty(name) {
            return Err(fail_with("invalid_parameter", "piece attribute name must not be empty"));
        }
    }
    let group_names: Vec<String> = [&params.left_piece_group, &params.overlap_piece_group, &params.right_piece_group]
        .into_iter()
        .flatten()
        .cloned()
        .collect();
    let shatter = params.operation == Operation::Shatter;
    if shatter && group_names.iter().any(|name| trimmed_empty(name)) {
        return Err(fail_with("invalid_parameter", "shatter group names must not be empty"));
    }
    if shatter && has_duplicate(&group_names) {
        return Err(fail_with("invalid_parameter", "shatter group names must be distinct"));
    }
    let left_kind = params.left_treatment;
    let right_kind = params.right_treatment;
    let corner_payload = has_corner_payload(left) || has_corner_payload(&params.right);
    let require_closed = params
        .require_closed
        .unwrap_or(left_kind == Treatment::Solid && right_kind == Treatment::Solid);
    let operation = params.operation;
    let allow_opposite_duplicates = (operation == Operation::Difference
        && left_kind == Treatment::Solid
        && right_kind == Treatment::Surface)
        || (operation == Operation::ReverseDifference
            && left_kind == Treatment::Surface
            && right_kind == Treatment::Solid)
        || (operation == Operation::Xor && left_kind != right_kind)
        || operation == Operation::Shatter;
    if shatter && (left_kind != Treatment::Solid || right_kind != Treatment::Solid) {
        return Err(fail_with("invalid_parameter", "shatter currently requires two solid operands"));
    }
    let cancel_arc = params.cancel.as_ref();
    let cancel_ref = params.cancel.as_deref();
    let grain = params.grain;
    let prepared = boolean_solid::prepare(
        cancel_arc,
        Some(params.resolve_left_self_intersections),
        Some(params.resolve_right_self_intersections),
        Some(left_kind),
        Some(right_kind),
        grain,
        left,
        &params.right,
    )?;
    let (ancestry, shatter_counts): (boolean_extract::Ancestry, Option<Vec<i64>>) = if shatter {
        let pieces = boolean_solid::shatter_with_ancestry(
            cancel_arc,
            Some(false),
            Some(true),
            Some(corner_payload),
            &prepared,
        )?;
        let concatenated = boolean_extract::View::concatenate_ancestries(cancel_ref, &pieces)?;
        let counts: Vec<i64> = pieces
            .iter()
            .map(|piece| geometry::primitive_count(&boolean_extract::geometry(piece)))
            .collect();
        (concatenated, Some(counts))
    } else {
        let ancestry = boolean_solid::extract_product_with_ancestry(
            cancel_arc,
            Some(false),
            Some(true),
            Some(corner_payload),
            operation,
            &prepared,
        )?;
        (ancestry, None)
    };
    let primitive_payload = boolean_payload::copy_primitives(cancel_ref, grain, &ancestry)?;
    let payload = if corner_payload {
        boolean_payload::copy_points_and_vertices(
            cancel_ref,
            grain,
            params.point_conflict,
            params.point_tolerance,
            &ancestry,
            &primitive_payload,
        )?
    } else {
        primitive_payload
    };
    let finalize = |source: &(dyn Fn(i64) -> i64 + Sync), geometry_value: geometry::T| -> Result<geometry::T, error::T> {
        let geometry_value = match &params.piece_attribute {
            None => geometry_value,
            Some(name) => {
                let values = boolean_extract::piece_ids(
                    boolean_solid::View::weiler(&prepared),
                    &ancestry,
                    source,
                    geometry::primitive_count(&geometry_value),
                )?;
                let item = attribute::create_owned(name, attribute::Owner::Primitive, attribute::Storage::Int(Arc::new(values)))
                    .map_err(|failure| invalid_output("boolean", &failure))?;
                geometry::with_attribute(item, &geometry_value).map_err(|failure| invalid_output("boolean", &failure))?
            }
        };
        let Some(counts) = &shatter_counts else {
            return Ok(geometry_value);
        };
        let first_overlap = counts[0];
        let first_right = counts[0] + counts[1];
        let add = |name: &Option<String>,
                   predicate: &(dyn Fn(i64) -> bool + Send + Sync),
                   geometry_value: geometry::T|
         -> Result<geometry::T, error::T> {
            match name {
                None => Ok(geometry_value),
                Some(name) => {
                    let item = group::init(
                        Some(grain),
                        group::Owner::Primitive,
                        name,
                        geometry::primitive_count(&geometry_value),
                        predicate,
                    )
                    .map_err(|failure| invalid_output("boolean", &failure))?;
                    geometry::with_group(item, &geometry_value)
                        .map_err(|failure| invalid_output("boolean", &failure))
                }
            }
        };
        let geometry_value = add(
            &params.left_piece_group,
            &|primitive| source(primitive) < first_overlap,
            geometry_value,
        )?;
        let geometry_value = add(
            &params.overlap_piece_group,
            &|primitive| {
                let value = source(primitive);
                value >= first_overlap && value < first_right
            },
            geometry_value,
        )?;
        add(&params.right_piece_group, &|primitive| source(primitive) >= first_right, geometry_value)
    };
    let cleanup = || -> Result<geometry::T, error::T> {
        let seams = boolean_solid::seams(cancel_arc, Some(grain), Some(false), &prepared)?;
        let cleanup = boolean_materialization::collapse_tiny_seams(
            cancel_arc,
            grain,
            params.tiny_seam_threshold,
            require_closed,
            Some(allow_opposite_duplicates),
            Some(params.cleanup_max_batches),
            Some(params.strict_cleanup),
            &ancestry,
            &seams,
            &payload,
        )?;
        let cleanup = match params.detriangulation {
            Detriangulation::Triangles => cleanup,
            Detriangulation::UnchangedPolygons | Detriangulation::AllPolygons => {
                boolean_materialization::detriangulate(
                    cancel_arc,
                    grain,
                    params.assume_flat,
                    params.detriangulation,
                    &ancestry,
                    &cleanup,
                )?
            }
        };
        let cleanup = match params.seam_points {
            SeamPoints::SharedSeamPoints => cleanup,
            SeamPoints::SplitSeamPoints => boolean_materialization::split_seam_points(cancel_arc, grain, &cleanup)?,
        };
        finalize(
            &|primitive| boolean_materialization::cleanup_primitive_source(&cleanup, primitive),
            boolean_materialization::cleanup_geometry(&cleanup).clone(),
        )
    };
    if params.tiny_seam_threshold == 0.
        && params.detriangulation == Detriangulation::Triangles
        && params.seam_points == SeamPoints::SharedSeamPoints
    {
        let accepted = boolean_materialization::verify_unchanged_extraction(
            cancel_arc,
            grain,
            require_closed,
            allow_opposite_duplicates,
            &ancestry,
            &payload,
        )?;
        if accepted {
            finalize(&|primitive| primitive, payload.clone())
        } else {
            cleanup()
        }
    } else {
        cleanup()
    }
}

pub struct SeamParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub output: SeamOutput,
    pub left_treatment: Treatment,
    pub right_treatment: Treatment,
    pub resolve_left_self_intersections: bool,
    pub resolve_right_self_intersections: bool,
    pub left_self_group: Option<String>,
    pub between_group: Option<String>,
    pub right_self_group: Option<String>,
    pub coincident_group: Option<String>,
    pub right: geometry::T,
}

/// The OCaml optional-argument defaults of `Boolean.seam`.
pub fn default_seam_params(right: geometry::T) -> SeamParams {
    SeamParams {
        cancel: None,
        grain: 16_384,
        output: SeamOutput::SeamCurves,
        left_treatment: Treatment::Solid,
        right_treatment: Treatment::Solid,
        resolve_left_self_intersections: false,
        resolve_right_self_intersections: false,
        left_self_group: Some("boolean_left_self_seam".to_owned()),
        between_group: Some("boolean_seam".to_owned()),
        right_self_group: Some("boolean_right_self_seam".to_owned()),
        coincident_group: Some("boolean_coincident".to_owned()),
        right,
    }
}

/// Build the same exact arrangement and materialize either deterministic chained seam curves or
/// coincident-area triangles. Curve primitives may be named independently as left
/// self-intersection, between-input, and right self-intersection products. The coincident output
/// can receive one complete primitive group. No approximate proximity edge is introduced.
pub fn seam(params: &SeamParams, left: &geometry::T) -> Result<geometry::T, error::T> {
    if params.grain <= 0 {
        return Err(fail_with("invalid_parameter", "grain must be positive"));
    }
    let names: Vec<String> = match params.output {
        SeamOutput::SeamCurves => {
            [&params.left_self_group, &params.between_group, &params.right_self_group]
                .into_iter()
                .flatten()
                .cloned()
                .collect()
        }
        SeamOutput::CoincidentPatches => params.coincident_group.iter().cloned().collect(),
    };
    if names.iter().any(|name| trimmed_empty(name)) {
        return Err(fail_with("invalid_parameter", "Boolean seam group names must not be empty"));
    }
    if has_duplicate(&names) {
        return Err(fail_with("invalid_parameter", "Boolean seam group names must be distinct"));
    }
    let cancel_arc = params.cancel.as_ref();
    let grain = params.grain;
    let prepared = boolean_solid::prepare(
        cancel_arc,
        Some(params.resolve_left_self_intersections),
        Some(params.resolve_right_self_intersections),
        Some(params.left_treatment),
        Some(params.right_treatment),
        grain,
        left,
        &params.right,
    )?;
    let seams = boolean_solid::seams(cancel_arc, Some(grain), None, &prepared)?;
    let output = match params.output {
        SeamOutput::SeamCurves => boolean_seam::curves(&seams).clone(),
        SeamOutput::CoincidentPatches => boolean_seam::coincident(&seams).clone(),
    };
    let add_group = |name: &Option<String>,
                     predicate: &(dyn Fn(i64) -> bool + Send + Sync),
                     geometry_value: geometry::T|
     -> Result<geometry::T, error::T> {
        match name {
            None => Ok(geometry_value),
            Some(name) => {
                let item = group::init(
                    Some(grain),
                    group::Owner::Primitive,
                    name,
                    geometry::primitive_count(&geometry_value),
                    predicate,
                )
                .map_err(|failure| invalid_output("boolean_seam", &failure))?;
                geometry::with_group(item, &geometry_value).map_err(|failure| invalid_output("boolean_seam", &failure))
            }
        }
    };
    match params.output {
        SeamOutput::CoincidentPatches => add_group(&params.coincident_group, &|_| true, output),
        SeamOutput::SeamCurves => {
            let kind = |curve: i64| boolean_seam::curve_kind(&seams, curve);
            let output = add_group(
                &params.left_self_group,
                &|curve| kind(curve) == boolean_seam::Kind::LeftSelf,
                output,
            )?;
            let output = add_group(
                &params.between_group,
                &|curve| kind(curve) == boolean_seam::Kind::Between,
                output,
            )?;
            add_group(&params.right_self_group, &|curve| kind(curve) == boolean_seam::Kind::RightSelf, output)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Mutex;

    use rdk_core::{normal_ops, packed, topology};
    use rdk_ops::triangulate;

    use super::*;

    fn geometry_of(points: &[(f64, f64, f64)], vertex_points: Vec<i64>, offsets: Vec<i64>) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let topology_value = topology::polygons_owned(points.len() as i64, vertex_points, offsets).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None)
            .unwrap()
    }

    fn triangles(points: &[(f64, f64, f64)], indices: Vec<i64>) -> geometry::T {
        let offsets: Vec<i64> = (0..=(indices.len() / 3) as i64).map(|primitive| primitive * 3).collect();
        geometry_of(points, indices, offsets)
    }

    const TETRA: [i64; 12] = [0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3];

    fn tetra(origin: (f64, f64, f64), size: f64) -> geometry::T {
        let (ox, oy, oz) = origin;
        triangles(&[(ox, oy, oz), (ox + size, oy, oz), (ox, oy + size, oz), (ox, oy, oz + size)], TETRA.to_vec())
    }

    fn tetra_batch(count: usize, shift: f64) -> geometry::T {
        let mut points = vec![(0., 0., 0.); count * 4];
        let mut indices = vec![0i64; count * 12];
        for item in 0..count {
            let first = item * 4;
            let x = item as f64 * 6. + shift;
            points[first] = (x, 0., 0.);
            points[first + 1] = (x + 2., 0., 0.);
            points[first + 2] = (x, 2., 0.);
            points[first + 3] = (x, 0., 2.);
            for corner in 0..12 {
                indices[item * 12 + corner] = first as i64 + TETRA[corner];
            }
        }
        triangles(&points, indices)
    }

    fn cube_quads(origin: (f64, f64, f64), size: f64) -> geometry::T {
        let (ox, oy, oz) = origin;
        geometry_of(
            &[
                (ox, oy, oz),
                (ox + size, oy, oz),
                (ox + size, oy + size, oz),
                (ox, oy + size, oz),
                (ox, oy, oz + size),
                (ox + size, oy, oz + size),
                (ox + size, oy + size, oz + size),
                (ox, oy + size, oz + size),
            ],
            vec![0, 3, 2, 1, 4, 5, 6, 7, 0, 1, 5, 4, 3, 7, 6, 2, 0, 4, 7, 3, 1, 2, 6, 5],
            vec![0, 4, 8, 12, 16, 20, 24],
        )
    }

    /// `Box_generator.box ~connectivity:Box_quads ~consolidate_points:true` for one division per axis.
    fn box_quads(size: (f64, f64, f64), center: (f64, f64, f64), rotation: (f64, f64, f64)) -> geometry::T {
        fn rx(a: f64) -> [[f64; 3]; 3] {
            [[1., 0., 0.], [0., a.cos(), -a.sin()], [0., a.sin(), a.cos()]]
        }
        fn ry(a: f64) -> [[f64; 3]; 3] {
            [[a.cos(), 0., a.sin()], [0., 1., 0.], [-a.sin(), 0., a.cos()]]
        }
        fn rz(a: f64) -> [[f64; 3]; 3] {
            [[a.cos(), -a.sin(), 0.], [a.sin(), a.cos(), 0.], [0., 0., 1.]]
        }
        fn mul(a: [[f64; 3]; 3], b: [[f64; 3]; 3]) -> [[f64; 3]; 3] {
            let mut out = [[0.; 3]; 3];
            for i in 0..3 {
                for j in 0..3 {
                    out[i][j] = a[i][0] * b[0][j] + a[i][1] * b[1][j] + a[i][2] * b[2][j];
                }
            }
            out
        }
        let m = mul(rz(rotation.2), mul(ry(rotation.1), rx(rotation.0)));
        let rotated = rotation.0 != 0. || rotation.1 != 0. || rotation.2 != 0.;
        let half = (size.0 * 0.5, size.1 * 0.5, size.2 * 0.5);
        let mut points = Vec::new();
        for z in 0..2 {
            for y in 0..2 {
                for x in 0..2 {
                    let lx = if x == 0 { -half.0 } else { half.0 };
                    let ly = if y == 0 { -half.1 } else { half.1 };
                    let lz = if z == 0 { -half.2 } else { half.2 };
                    let (px, py, pz) = if rotated {
                        (
                            m[0][0] * lx + m[0][1] * ly + m[0][2] * lz,
                            m[1][0] * lx + m[1][1] * ly + m[1][2] * lz,
                            m[2][0] * lx + m[2][1] * ly + m[2][2] * lz,
                        )
                    } else {
                        (lx, ly, lz)
                    };
                    points.push((center.0 + px, center.1 + py, center.2 + pz));
                }
            }
        }
        let boundary = |x: i64, y: i64, z: i64| if z == 0 { y * 2 + x } else { 4 + y * 2 + x };
        let face_point = |face: i64, row: i64, column: i64| match face {
            0 => boundary(1, column, row),
            1 => boundary(0, column, 1 - row),
            2 => boundary(row, 1, column),
            3 => boundary(row, 0, 1 - column),
            4 => boundary(column, row, 1),
            _ => boundary(1 - column, row, 0),
        };
        let mut vertices = Vec::new();
        for face in 0..6 {
            vertices.extend([face_point(face, 0, 0), face_point(face, 0, 1), face_point(face, 1, 1), face_point(face, 1, 0)]);
        }
        geometry_of(&points, vertices, vec![0, 4, 8, 12, 16, 20, 24])
    }

    type Signature = (Vec<f64>, Vec<f64>, Vec<f64>, Vec<i64>, Vec<i64>, Vec<u8>);

    fn signature(geometry_value: &geometry::T) -> Signature {
        let positions = packed::Float3::View::view(geometry::positions(geometry_value)).unwrap();
        let topology_view = topology::View::view(geometry::topology(geometry_value)).unwrap();
        (
            positions.x.to_vec(),
            positions.y.to_vec(),
            positions.z.to_vec(),
            topology_view.vertex_points.to_vec(),
            topology_view.primitive_offsets.to_vec(),
            topology_view.primitive_kinds.to_vec(),
        )
    }

    fn with_point_weight(value: f64, geometry_value: &geometry::T) -> geometry::T {
        let item = attribute::create_owned(
            "weight",
            attribute::Owner::Point,
            attribute::Storage::Float(Arc::new(vec![value; geometry::point_count(geometry_value) as usize])),
        )
        .unwrap();
        geometry::with_attribute(item, geometry_value).unwrap()
    }

    fn with_vertex_normals(cusp_angle: f64, geometry_value: &geometry::T) -> geometry::T {
        normal_ops::run(
            &normal_ops::Params {
                cancel: None,
                grain: Some(1),
                selection: None,
                primitives: None,
                owner: attribute::Owner::Vertex,
                weighting: normal_ops::Weighting::FaceArea,
                cusp_angle,
                attribute: "N".to_owned(),
            },
            geometry_value,
        )
        .unwrap()
    }

    /// `Rdk_test_support.Boolean_front.run` defaults, with `~grain:1` which every test passes.
    fn front(right: geometry::T) -> Params {
        Params {
            cancel: None,
            grain: 1,
            operation: Operation::Union,
            left_treatment: Treatment::Solid,
            right_treatment: Treatment::Solid,
            resolve_left_self_intersections: false,
            resolve_right_self_intersections: false,
            point_conflict: PointConflict::PromoteToVertex,
            point_tolerance: 0.,
            tiny_seam_threshold: 0.,
            cleanup_max_batches: 8,
            strict_cleanup: true,
            seam_points: SeamPoints::SharedSeamPoints,
            detriangulation: Detriangulation::Triangles,
            assume_flat: false,
            require_closed: None,
            piece_attribute: None,
            left_piece_group: Some("boolean_left".to_owned()),
            overlap_piece_group: Some("boolean_overlap".to_owned()),
            right_piece_group: Some("boolean_right".to_owned()),
            right,
        }
    }

    /// `Parallel.run ~domains`.
    fn with_domains(domains: i64, work: &mut (dyn FnMut() + Send)) {
        let cell = Mutex::new(work);
        parallel::run_unit(Some(domains), &|| {
            (cell.lock().unwrap())();
            Ok(())
        })
        .unwrap();
    }

    fn group_flags(name: &str, geometry_value: &geometry::T) -> Vec<bool> {
        let item = geometry::find_group(group::Owner::Primitive, name, geometry_value).unwrap();
        (0..group::length(&item)).map(|primitive| group::mem(primitive, &item)).collect()
    }

    fn assert_code(result: Result<geometry::T, error::T>, code: &str, what: &str) {
        match result {
            Err(failure) if error::code(&failure) == code => {}
            Err(failure) => panic!("unexpected {what}: {}", error::to_string(&failure)),
            Ok(_) => panic!("{what}: expected {code}"),
        }
    }

    // test_solid_products
    #[test]
    fn solid_products() {
        let left = tetra((0., 0., 0.), 2.);
        let right = tetra((0.5, 0.2, 0.2), 2.);
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                [Operation::Union, Operation::Intersection, Operation::Difference, Operation::ReverseDifference, Operation::Xor]
                    .iter()
                    .map(|operation| {
                        let mut params = front(right.clone());
                        params.operation = *operation;
                        signature(&run(&params, &left).unwrap())
                    })
                    .collect::<Vec<_>>()
            }); }); out.unwrap() }
        };
        let products = run_with(1);
        assert!(products.iter().all(|product| !product.3.is_empty()), "public Boolean lost a non-empty solid product");
        assert!(products == run_with(4), "public solid Boolean differs between one and four domains");
    }

    // test_surface_cut_and_payload
    #[test]
    fn surface_cut_and_payload() {
        let left = with_point_weight(1., &cube_quads((0., 0., 0.), 2.));
        let right = with_point_weight(
            2.,
            &triangles(&[(-1., 1., 0.5), (3., 1., 0.5), (-1., 1., 1.5), (3., 1., 1.5)], vec![0, 1, 2, 1, 3, 2]),
        );
        let mut params = front(right.clone());
        params.operation = Operation::Difference;
        params.right_treatment = Treatment::Surface;
        let output = run(&params, &left).unwrap();
        assert!(geometry::primitive_count(&output) > 12, "public solid-minus-surface Boolean lost its cut walls");
        assert!(
            geometry::find_attribute(attribute::Owner::Point, "weight", &output).is_none()
                && geometry::find_attribute(attribute::Owner::Vertex, "weight", &output).is_some(),
            "public Boolean did not promote conflicting point payload"
        );
        params.point_conflict = PointConflict::Reject;
        match run(&params, &left) {
            Err(failure) if error::code(&failure) == "point_payload_conflict" => {}
            Err(failure) => panic!("unexpected public point conflict: {}", error::to_string(&failure)),
            Ok(_) => panic!("public Boolean ignored its reject point-conflict policy"),
        }
    }

    // test_normal_payload_orientation
    #[test]
    fn normal_payload_orientation() {
        let left = with_vertex_normals(0.1, &cube_quads((0., 0., 0.), 2.));
        let right = with_vertex_normals(
            std::f64::consts::PI,
            &triangles(&[(-1., 1., -1.), (3., 1., -1.), (-1., 1., 3.), (3., 1., 3.)], vec![0, 1, 2, 1, 3, 2]),
        );
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = front(right.clone());
                params.operation = Operation::Difference;
                params.right_treatment = Treatment::Surface;
                run(&params, &left).unwrap()
            }); }); out.unwrap() }
        };
        let output = run_with(1);
        let parallel_output = run_with(4);
        let normals_of = |geometry_value: &geometry::T| {
            let item = geometry::find_attribute(attribute::Owner::Vertex, "N", geometry_value).unwrap();
            match attribute::storage(&item).unwrap() {
                attribute::Storage::Float3(values) => packed::Float3::View::view(&values).unwrap(),
                _ => panic!("Boolean vertex normal payload is not float3"),
            }
        };
        assert!(
            signature(&output) == signature(&parallel_output),
            "Boolean normal topology differs between one and four domains"
        );
        let a = normals_of(&output);
        let b = normals_of(&parallel_output);
        if a.x != b.x || a.y != b.y || a.z != b.z {
            let mut differences = 0;
            let mut maximum = 0f64;
            for index in 0..a.x.len() {
                for delta in [
                    (a.x[index] - b.x[index]).abs(),
                    (a.y[index] - b.y[index]).abs(),
                    (a.z[index] - b.z[index]).abs(),
                ] {
                    if delta != 0. {
                        differences += 1;
                        maximum = maximum.max(delta);
                    }
                }
            }
            panic!("Boolean normal payload differs between one and four domains ({differences} components, max {maximum})");
        }
        let normals = normals_of(&output);
        let positions = packed::Float3::View::view(geometry::positions(&output)).unwrap();
        let topology_view = topology::View::view(geometry::topology(&output)).unwrap();
        for primitive in 0..geometry::primitive_count(&output) as usize {
            let first = topology_view.primitive_offsets[primitive] as usize;
            let last = topology_view.primitive_offsets[primitive + 1] as usize;
            assert!(last - first == 3, "Boolean normal orientation regression expected triangular output");
            let point = |vertex: usize| topology_view.vertex_points[vertex] as usize;
            let (a, b, c) = (point(first), point(first + 1), point(first + 2));
            let (ux, uy, uz) = (positions.x[b] - positions.x[a], positions.y[b] - positions.y[a], positions.z[b] - positions.z[a]);
            let (vx, vy, vz) = (positions.x[c] - positions.x[a], positions.y[c] - positions.y[a], positions.z[c] - positions.z[a]);
            let fx = uy * vz - uz * vy;
            let fy = uz * vx - ux * vz;
            let fz = ux * vy - uy * vx;
            for vertex in first..last {
                let dot = fx * normals.x[vertex] + fy * normals.y[vertex] + fz * normals.z[vertex];
                assert!(dot > 1e-10, "Boolean transferred a normal opposite to its output winding");
            }
        }
    }

    // test_surface_fracture_pieces
    #[test]
    fn surface_fracture_pieces() {
        let left = cube_quads((0., 0., 0.), 2.);
        let right =
            triangles(&[(-1., 1., -1.), (3., 1., -1.), (-1., 1., 3.), (3., 1., 3.)], vec![0, 1, 2, 1, 3, 2]);
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = front(right.clone());
                params.operation = Operation::Difference;
                params.right_treatment = Treatment::Surface;
                params.piece_attribute = Some("piece".to_owned());
                params.require_closed = Some(true);
                run(&params, &left).unwrap()
            }); }); out.unwrap() }
        };
        let output = run_with(1);
        let values_for = |geometry_value: &geometry::T| {
            let item = geometry::find_attribute(attribute::Owner::Primitive, "piece", geometry_value).unwrap();
            match attribute::storage(&item).unwrap() {
                attribute::Storage::Int(values) => values,
                _ => panic!("public Boolean fracture piece plane is not integer-valued"),
            }
        };
        let values = values_for(&output);
        assert!(
            values.iter().copied().fold(-1, i64::max) == 1,
            "one surface cutter did not produce exactly two Boolean-cell pieces"
        );
        let topology_view = topology::View::view(geometry::topology(&output)).unwrap();
        for piece in 0..2 {
            let mut edges: HashMap<(i64, i64), i64> = HashMap::new();
            let mut primitives = 0;
            for primitive in 0..geometry::primitive_count(&output) as usize {
                if values[primitive] == piece {
                    primitives += 1;
                    let first = topology_view.primitive_offsets[primitive];
                    let last = topology_view.primitive_offsets[primitive + 1];
                    for vertex in first..last {
                        let next = if vertex + 1 == last { first } else { vertex + 1 };
                        let a = topology_view.vertex_points[vertex as usize];
                        let b = topology_view.vertex_points[next as usize];
                        let edge = if a < b { (a, b) } else { (b, a) };
                        *edges.entry(edge).or_insert(0) += 1;
                    }
                }
            }
            assert!(primitives > 1, "Boolean fracture assigned an individual polygon as a piece");
            for incidence in edges.values() {
                assert!(*incidence == 2, "Boolean fracture piece is not a closed two-manifold shell");
            }
        }
        assert!(values == values_for(&run_with(4)), "Boolean fracture piece identities differ between one and four domains");
    }

    // test_shatter
    #[test]
    fn shatter() {
        let left = tetra((0., 0., 0.), 2.);
        let right = tetra((0.5, 0.2, 0.2), 2.);
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = front(right.clone());
                params.operation = Operation::Shatter;
                let output = run(&params, &left).unwrap();
                let groups: Vec<Vec<bool>> = ["boolean_left", "boolean_overlap", "boolean_right"]
                    .iter()
                    .map(|name| group_flags(name, &output))
                    .collect();
                for primitive in 0..geometry::primitive_count(&output) as usize {
                    let memberships = groups.iter().filter(|group| group[primitive]).count();
                    assert!(memberships == 1, "public Boolean shatter groups do not partition output primitives");
                }
                assert!(
                    groups.iter().all(|group| group.iter().any(|flag| *flag)),
                    "public Boolean shatter lost a non-empty product"
                );
                (signature(&output), groups)
            }); }); out.unwrap() }
        };
        assert!(run_with(1) == run_with(4), "public Boolean shatter differs between one and four domains");
        let mut params = front(right.clone());
        params.operation = Operation::Shatter;
        params.left_piece_group = Some("same".to_owned());
        params.overlap_piece_group = Some("same".to_owned());
        assert_code(run(&params, &left), "invalid_parameter", "shatter-name error");
        let mut params = front(right.clone());
        params.operation = Operation::Shatter;
        params.right_treatment = Treatment::Surface;
        assert_code(run(&params, &left), "invalid_parameter", "surface-shatter error");
    }

    // test_shatter_scale
    #[test]
    fn shatter_scale() {
        let left = tetra_batch(16, 0.);
        let right = tetra_batch(16, 0.5);
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = front(right.clone());
                params.operation = Operation::Shatter;
                let output = run(&params, &left).unwrap();
                let counts: Vec<i64> = ["boolean_left", "boolean_overlap", "boolean_right"]
                    .iter()
                    .map(|name| {
                        group::cardinality(&geometry::find_group(group::Owner::Primitive, name, &output).unwrap())
                    })
                    .collect();
                (signature(&output), counts)
            }); }); out.unwrap() }
        };
        let one = run_with(1);
        assert!(one.1.iter().all(|count| *count > 0), "multi-pair shatter lost a region product");
        assert!(one == run_with(4), "multi-pair shatter differs between one and four domains");
    }

    // test_output_policies
    #[test]
    fn output_policies() {
        let left = cube_quads((0., 0., 0.), 1.);
        let right = tetra((10., 0., 0.), 1.);
        let mut params = front(right);
        params.detriangulation = Detriangulation::UnchangedPolygons;
        let polygons = run(&params, &left).unwrap();
        assert!(
            geometry::primitive_count(&polygons) == 10,
            "public Boolean unchanged-polygon detriangulation has wrong cardinality"
        );
        let overlap = cube_quads((0.5, 0.5, 0.5), 1.);
        let shared = run(&front(overlap.clone()), &left).unwrap();
        let mut params = front(overlap);
        params.seam_points = SeamPoints::SplitSeamPoints;
        let split = run(&params, &left).unwrap();
        let split_one = { let mut out = None; with_domains(1, &mut || { out = Some(signature(&run(&params, &left).unwrap())); }); out.unwrap() };
        let split_four = { let mut out = None; with_domains(4, &mut || { out = Some(signature(&run(&params, &left).unwrap())); }); out.unwrap() };
        assert!(split_one == split_four, "split seam points differ between domain counts");
        assert!(
            geometry::point_count(&split) > geometry::point_count(&shared)
                && geometry::primitive_count(&split) == geometry::primitive_count(&shared),
            "public Boolean seam-point policy did not split topology components"
        );
        let host = box_quads((2.8, 2.2, 2.2), (0., 0., 0.), (0., 0., 0.));
        let cutter = box_quads((2.2, 1.15, 1.15), (0.85, 0., 0.), (0.35, 0.42, 0.12));
        let mut params = front(cutter);
        params.operation = Operation::Difference;
        params.detriangulation = Detriangulation::AllPolygons;
        let cut = run(&params, &host).unwrap();
        triangulate::run(&triangulate::Params { cancel: None, grain: Some(1), primitives: None }, &cut).unwrap();
    }

    // test_errors
    #[test]
    fn errors() {
        let left = tetra((0., 0., 0.), 1.);
        let right = tetra((0.25, 0.25, 0.25), 1.);
        let mut params = front(right.clone());
        params.grain = 0;
        assert_code(run(&params, &left), "invalid_parameter", "public Boolean grain error");
        let mut params = front(right.clone());
        params.cleanup_max_batches = 0;
        assert_code(run(&params, &left), "invalid_parameter", "public cleanup-bound error");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut params = front(right);
        params.cancel = Some(token);
        assert_code(run(&params, &left), "cancelled", "public Boolean cancellation");
    }

    // test_bounded_cleanup
    #[test]
    fn bounded_cleanup() {
        let left = cube_quads((0., 0., 0.), 2.);
        let right = cube_quads(((0.6f64).next_up(), 0.60000000000000009, 0.60000000000000009), 2.);
        let threshold = 0.60000000000000009;
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = front(right.clone());
                params.tiny_seam_threshold = threshold;
                params.cleanup_max_batches = 8;
                params.strict_cleanup = false;
                signature(&run(&params, &left).unwrap())
            }); }); out.unwrap() }
        };
        assert!(run_with(1) == run_with(4), "public bounded Boolean cleanup differs between one and four domains");
    }

    // test_seam_products
    #[test]
    fn seam_products() {
        let left = tetra((0., 0., 0.), 2.);
        let right = tetra((0.5, 0.2, 0.2), 2.);
        let run_with = |domains: i64| {
            { let mut out = None; with_domains(domains, &mut || { out = Some({
                let mut params = default_seam_params(right.clone());
                params.grain = 1;
                let output = seam(&params, &left).unwrap();
                let between = geometry::find_group(group::Owner::Primitive, "boolean_seam", &output).unwrap();
                assert!(
                    geometry::primitive_count(&output) > 0 && group::cardinality(&between) > 0,
                    "public Boolean seam lost its between-input curves"
                );
                (
                    signature(&output),
                    (0..group::length(&between)).map(|primitive| group::mem(primitive, &between)).collect::<Vec<_>>(),
                )
            }); }); out.unwrap() }
        };
        assert!(run_with(1) == run_with(4), "public Boolean seam curves differ between one and four domains");
        let mut params = default_seam_params(left.clone());
        params.grain = 1;
        params.output = SeamOutput::CoincidentPatches;
        let coincident = seam(&params, &left).unwrap();
        let item = geometry::find_group(group::Owner::Primitive, "boolean_coincident", &coincident).unwrap();
        assert!(
            geometry::primitive_count(&coincident) > 0 && group::cardinality(&item) == geometry::primitive_count(&coincident),
            "public Boolean coincident product lost facets or naming"
        );
        let mut params = default_seam_params(right);
        params.grain = 1;
        params.left_self_group = Some("same".to_owned());
        params.between_group = Some("same".to_owned());
        assert_code(seam(&params, &left), "invalid_parameter", "seam naming error");
    }
}
