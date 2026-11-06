use crate::{
    attribute, cancel, element_selection, error, face_normals, geometry, group, packed, point_index,
    topology,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Weighting {
    VertexAngle,
    EachVertex,
    FaceArea,
}

pub struct Params {
    pub cancel: Option<cancel::T>,
    pub grain: Option<i64>,
    pub selection: Option<element_selection::T>,
    pub primitives: Option<group::T>,
    pub owner: attribute::Owner,
    pub weighting: Weighting,
    pub cusp_angle: f64,
    pub attribute: String,
}

struct Planes {
    x: Vec<f64>,
    y: Vec<f64>,
    z: Vec<f64>,
}

struct Faces {
    raw: Planes,
    inverse_length: Vec<f64>,
}

const OPERATION: &str = "Rdk.Normal_ops.normals";

fn max_abs3(x: f64, y: f64, z: f64) -> f64 {
    x.abs().max(y.abs()).max(z.abs())
}

fn make_error(code: &str, message: String) -> error::T {
    error::internal("normals", code, message)
}

fn blank_attribute_name(name: &str) -> bool {
    name.as_bytes()
        .iter()
        .all(|byte| matches!(*byte, b' ' | b'\t' | b'\n' | b'\r' | 0x0c))
}

fn remap_error(value: error::T) -> error::T {
    if error::code(&value) == "cancelled" {
        make_error("cancelled", "geometry operation was cancelled".to_owned())
    } else {
        make_error("invalid_topology", error::message(&value).to_owned())
    }
}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, mask)
        .map_err(|_| make_error("cancelled", "geometry operation was cancelled".to_owned()))
}

fn validate_primitives(
    topology_value: &topology::T,
    primitives: Option<&group::T>,
) -> Result<(), error::T> {
    let Some(value) = primitives else {
        return Ok(());
    };
    if group::owner(value) != group::Owner::Primitive {
        return Err(make_error(
            "invalid_topology",
            format!("{OPERATION}: contribution selection must own primitives"),
        ));
    }
    if group::length(value) != topology::primitive_count(topology_value) {
        return Err(make_error(
            "invalid_topology",
            format!("{OPERATION}: contribution selection length does not match primitive count"),
        ));
    }
    Ok(())
}

fn compute_faces(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    need_inverse: bool,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Faces, error::T> {
    let ((x, y, z), inverse_length) = face_normals::polygon_area_vector(
        cancel_value,
        grain,
        need_inverse,
        false,
        primitives,
        OPERATION,
        geometry_value,
    )
    .map_err(remap_error)?;
    Ok(Faces { raw: Planes { x, y, z }, inverse_length })
}

fn corner_angle(
    positions: &packed::Float3::View::View,
    topology: &topology::View::View,
    primitive: usize,
    vertex: usize,
) -> f64 {
    let first = topology.primitive_offsets[primitive] as usize;
    let last = topology.primitive_offsets[primitive + 1] as usize;
    let previous = if vertex == first { last - 1 } else { vertex - 1 };
    let next = if vertex + 1 == last { first } else { vertex + 1 };
    let point = topology.vertex_points[vertex] as usize;
    let previous_point = topology.vertex_points[previous] as usize;
    let next_point = topology.vertex_points[next] as usize;
    let ux = positions.x[previous_point] - positions.x[point];
    let uy = positions.y[previous_point] - positions.y[point];
    let uz = positions.z[previous_point] - positions.z[point];
    let vx = positions.x[next_point] - positions.x[point];
    let vy = positions.y[next_point] - positions.y[point];
    let vz = positions.z[next_point] - positions.z[point];
    let us = max_abs3(ux, uy, uz);
    let vs = max_abs3(vx, vy, vz);
    if us == 0.0 || vs == 0.0 {
        return 0.0;
    }
    let ux = ux / us;
    let uy = uy / us;
    let uz = uz / us;
    let vx = vx / vs;
    let vy = vy / vs;
    let vz = vz / vs;
    let cx = uy * vz - uz * vy;
    let cy = uz * vx - ux * vz;
    let cz = ux * vy - uy * vx;
    (cx * cx + cy * cy + cz * cz).sqrt().atan2(ux * vx + uy * vy + uz * vz)
}

fn accumulate_points(
    cancel_value: Option<&cancel::T>,
    weighting: Weighting,
    geometry_value: &geometry::T,
    faces: &Faces,
)-> Result<Planes, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology = topology::View::view(topology_value).map_err(remap_error)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))
        .map_err(remap_error)?;
    let count = usize::try_from(topology.point_count)
        .map_err(|_| make_error("invalid_topology", format!("{OPERATION}: invalid point count")))?;
    let mut x = vec![0.0; count];
    let mut y = vec![0.0; count];
    let mut z = vec![0.0; count];
    let primitive_count = topology.primitive_kinds.len();
    for primitive in 0..primitive_count {
        poll(cancel_value, primitive as i64, 4095)?;
        if topology.primitive_kinds[primitive] != 0 {
            continue;
        }
        let first = topology.primitive_offsets[primitive] as usize;
        let last = topology.primitive_offsets[primitive + 1] as usize;
        let inverse = faces.inverse_length.get(primitive).copied().unwrap_or(0.0);
        if weighting != Weighting::FaceArea && inverse == 0.0 {
            continue;
        }
        let (fx, fy, fz) = match weighting {
            Weighting::FaceArea => (faces.raw.x[primitive], faces.raw.y[primitive], faces.raw.z[primitive]),
            Weighting::EachVertex => (
                faces.raw.x[primitive] * inverse,
                faces.raw.y[primitive] * inverse,
                faces.raw.z[primitive] * inverse,
            ),
            Weighting::VertexAngle => (faces.raw.x[primitive], faces.raw.y[primitive], faces.raw.z[primitive]),
        };
        for vertex in first..last {
            let point = topology.vertex_points[vertex] as usize;
            let weight = if weighting == Weighting::VertexAngle {
                inverse * corner_angle(&positions, &topology, primitive, vertex)
            } else {
                1.0
            };
            x[point] += fx * weight;
            y[point] += fy * weight;
            z[point] += fz * weight;
        }
    }
    Ok(Planes { x, y, z })
}

fn selection_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    topology_value: &topology::T,
    selection: Option<&element_selection::T>,
    owner: attribute::Owner,
)-> Result<Option<group::T>, error::T> {
    let Some(selection) = selection else {
        return Ok(None);
    };
    let destination = match owner {
        attribute::Owner::Point => group::Owner::Point,
        attribute::Owner::Vertex => group::Owner::Vertex,
        attribute::Owner::Primitive => group::Owner::Primitive,
        attribute::Owner::Detail => return Ok(None),
    };
    element_selection::promote(
        cancel_value,
        grain,
        Some("normal_selection"),
        destination,
        selection,
        topology_value,
    )
    .map_err(remap_error)
    .map(Some)
}

fn existing_planes(
    owner: attribute::Owner,
    name: &str,
    geometry_value: &geometry::T,
)-> Result<Option<Planes>, error::T> {
    let Some(value) = geometry::find_attribute(owner, name, geometry_value) else {
        return Ok(None);
    };
    match attribute::View::storage(&value).map_err(remap_error)? {
        attribute::Storage::Float3(values) => {
            let values = packed::Float3::View::view(values).map_err(remap_error)?;
            Ok(Some(Planes {
                x: values.x.to_vec(),
                y: values.y.to_vec(),
                z: values.z.to_vec(),
            }))
        }
        _ => {
            let owner_name = match owner {
                attribute::Owner::Point => "point",
                attribute::Owner::Vertex => "vertex",
                attribute::Owner::Primitive => "primitive",
                attribute::Owner::Detail => "detail",
            };
            Err(make_error(
                "invalid_topology",
                format!("{OPERATION}: {owner_name} {name} must have float3 storage"),
            ))
        }
    }
}

fn initial(count: usize, existing: Option<&Planes>) -> Planes {
    match existing {
        Some(value) => Planes {
            x: value.x.clone(),
            y: value.y.clone(),
            z: value.z.clone(),
        },
        None => Planes {
            x: vec![0.0; count],
            y: vec![0.0; count],
            z: vec![0.0; count],
        },
    }
}

fn store(output: &mut Planes, index: usize, x: f64, y: f64, z: f64) -> bool {
    if !(x.is_finite() && y.is_finite() && z.is_finite()) {
        return false;
    }
    let scale = max_abs3(x, y, z);
    if scale == 0.0 {
        output.x[index] = 0.0;
        output.y[index] = 0.0;
        output.z[index] = 0.0;
    } else {
        let sx = x / scale;
        let sy = y / scale;
        let sz = z / scale;
        let inverse = 1.0 / (sx * sx + sy * sy + sz * sz).sqrt();
        output.x[index] = sx * inverse;
        output.y[index] = sy * inverse;
        output.z[index] = sz * inverse;
    }
    true
}

fn selected(selection: Option<&group::T>, index: i64) -> bool {
    selection.map_or(true, |value| group::mem(index, value))
}

fn first_error(errors: Vec<i64>) -> Option<i64> {
    errors.into_iter().find(|&value| value >= 0)
}

fn write_points(
    cancel_value: Option<&cancel::T>,
    grain: usize,
    selection: Option<&group::T>,
    output: &mut Planes,
    sums: &Planes,
)-> Result<(), error::T> {
    let count = output.x.len();
    let mut errors = vec![-1i64; count.div_ceil(grain)];
    // ponytail: serial writes avoid per-range plane copies; parallelize after a disjoint-slice output API exists.
    for (range, first) in (0..count).step_by(grain).enumerate() {
        let last = (first + grain).min(count);
        for point in first..last {
            poll(cancel_value, point as i64, 4095)?;
            if selected(selection, point as i64) {
                let x = sums.x[point];
                let y = sums.y[point];
                let z = sums.z[point];
                if !store(output, point, x, y, z) {
                    errors[range] = point as i64;
                }
            }
        }
    }
    if let Some(point) = first_error(errors) {
        Err(make_error("invalid_topology", format!("{OPERATION}: non-finite point normal at {point}")))
    } else {
        Ok(())
    }
}

fn write_smooth_vertices(
    cancel_value: Option<&cancel::T>,
    grain: usize,
    selection: Option<&group::T>,
    topology_value: &topology::T,
    output: &mut Planes,
    sums: &Planes,
)-> Result<(), error::T> {
    let topology = topology::View::view(topology_value).map_err(remap_error)?;
    let count = topology.vertex_points.len();
    let mut errors = vec![-1i64; count.div_ceil(grain)];
    for (range, first) in (0..count).step_by(grain).enumerate() {
        let last = (first + grain).min(count);
        for vertex in first..last {
            poll(cancel_value, vertex as i64, 4095)?;
            if selected(selection, vertex as i64) {
                let point = topology.vertex_points[vertex] as usize;
                if !store(output, vertex, sums.x[point], sums.y[point], sums.z[point]) {
                    errors[range] = vertex as i64;
                }
            }
        }
    }
    if let Some(vertex) = first_error(errors) {
        Err(make_error("invalid_topology", format!("{OPERATION}: non-finite vertex normal at {vertex}")))
    } else {
        Ok(())
    }
}

fn write_cusped_vertices(
    cancel_value: Option<&cancel::T>,
    grain: usize,
    weighting: Weighting,
    cusp_angle: f64,
    selection: Option<&group::T>,
    geometry_value: &geometry::T,
    index: &point_index::T,
    faces: &Faces,
    output: &mut Planes,
)-> Result<(), error::T> {
    let topology = topology::View::view(geometry::topology(geometry_value))
        .map_err(remap_error)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))
        .map_err(remap_error)?;
    let reverse = point_index::View::view(index);
    let count = topology.vertex_points.len();
    let mut angles = if weighting == Weighting::VertexAngle { vec![0.0; count] } else { Vec::new() };
    if weighting == Weighting::VertexAngle {
        for vertex in 0..count {
            poll(cancel_value, vertex as i64, 4095)?;
            let primitive = reverse.primitive_of_vertex[vertex] as usize;
            if topology.primitive_kinds[primitive] == 0 {
                angles[vertex] = corner_angle(&positions, &topology, primitive, vertex);
            }
        }
    }
    let cosine = cusp_angle.cos();
    let mut errors = vec![-1i64; count.div_ceil(grain)];
    for (range, first) in (0..count).step_by(grain).enumerate() {
        let last = (first + grain).min(count);
        for vertex in first..last {
            poll(cancel_value, vertex as i64, 4095)?;
            if !selected(selection, vertex as i64) {
                continue;
            }
            let primitive = reverse.primitive_of_vertex[vertex] as usize;
            let current_inverse = faces.inverse_length[primitive];
            let mut nx = 0.0;
            let mut ny = 0.0;
            let mut nz = 0.0;
            if current_inverse != 0.0 {
                let current_x = faces.raw.x[primitive] * current_inverse;
                let current_y = faces.raw.y[primitive] * current_inverse;
                let current_z = faces.raw.z[primitive] * current_inverse;
                let point = topology.vertex_points[vertex] as usize;
                let mut at = reverse.point_offsets[point] as usize;
                let incidence_last = reverse.point_offsets[point + 1] as usize;
                while at < incidence_last {
                    let source_vertex = reverse.point_vertices[at] as usize;
                    let source_primitive = reverse.primitive_of_vertex[source_vertex] as usize;
                    let inverse = faces.inverse_length[source_primitive];
                    if inverse != 0.0 {
                        let x = faces.raw.x[source_primitive] * inverse;
                        let y = faces.raw.y[source_primitive] * inverse;
                        let z = faces.raw.z[source_primitive] * inverse;
                        let dot = current_x * x + current_y * y + current_z * z;
                        if dot + 1e-12 >= cosine {
                            let weight = match weighting {
                                Weighting::EachVertex => inverse,
                                Weighting::FaceArea => 1.0,
                                Weighting::VertexAngle => inverse * angles[source_vertex],
                            };
                            nx += faces.raw.x[source_primitive] * weight;
                            ny += faces.raw.y[source_primitive] * weight;
                            nz += faces.raw.z[source_primitive] * weight;
                        }
                    }
                    at += 1;
                }
            }
            if !store(output, vertex, nx, ny, nz) {
                errors[range] = vertex as i64;
            }
        }
    }
    if let Some(vertex) = first_error(errors) {
        Err(make_error("invalid_topology", format!("{OPERATION}: non-finite vertex normal at {vertex}")))
    } else {
        Ok(())
    }
}

fn install(
    owner: attribute::Owner,
    name: &str,
    values: Planes,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut attributes: Vec<_> = geometry::attributes(geometry_value)
        .into_iter()
        .filter(|item| attribute::name(item) != name)
        .collect();
    let packed = packed::Float3::View::of_owned_exn(values.x, values.y, values.z).map_err(remap_error)?;
    let attribute_value = attribute::create_owned(
        name,
        owner,
        attribute::Storage::Float3(packed),
    )
    .map_err(remap_error)?;
    attributes.push(attribute_value);
    geometry::View::with_attributes_owned(attributes, geometry_value).map_err(remap_error)
}

fn fast_point_area(
    cancel_value: Option<&cancel::T>,
    grain: usize,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology = topology::View::view(geometry::topology(geometry_value)).map_err(remap_error)?;
    let ((face_x, face_y, face_z), _) = face_normals::polygon_area_vector(
        cancel_value,
        grain as i64,
        false,
        true,
        None,
        OPERATION,
        geometry_value,
    )
    .map_err(remap_error)?;
    let point_count = usize::try_from(topology.point_count)
        .map_err(|_| make_error("invalid_topology", format!("{OPERATION}: invalid point count")))?;
    let mut x = vec![0.0; point_count];
    let mut y = vec![0.0; point_count];
    let mut z = vec![0.0; point_count];
    let mut primitive = 0usize;
    for vertex in 0..topology.vertex_points.len() {
        poll(cancel_value, vertex as i64, 16_383)?;
        while vertex >= topology.primitive_offsets[primitive + 1] as usize {
            primitive += 1;
        }
        let point = topology.vertex_points[vertex] as usize;
        x[point] += face_x[primitive];
        y[point] += face_y[primitive];
        z[point] += face_z[primitive];
    }
    let mut errors = vec![-1i64; point_count.div_ceil(grain)];
    for (range, first) in (0..point_count).step_by(grain).enumerate() {
        let last = (first + grain).min(point_count);
        for point in first..last {
            poll(cancel_value, point as i64, 4095)?;
            let vx = x[point];
            let vy = y[point];
            let vz = z[point];
            if !(vx.is_finite() && vy.is_finite() && vz.is_finite()) {
                errors[range] = point as i64;
            } else {
                let scale = max_abs3(vx, vy, vz);
                if scale > 0.0 {
                    let sx = vx / scale;
                    let sy = vy / scale;
                    let sz = vz / scale;
                    let inverse = 1.0 / (sx * sx + sy * sy + sz * sz).sqrt();
                    x[point] = sx * inverse;
                    y[point] = sy * inverse;
                    z[point] = sz * inverse;
                }
            }
        }
    }
    if let Some(point) = first_error(errors) {
        return Err(make_error(
            "invalid_topology",
            format!("{OPERATION}: non-finite point normal at {point}"),
        ));
    }
    install(attribute::Owner::Point, "N", Planes { x, y, z }, geometry_value)
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    let cancel_value = params.cancel.as_ref();
    if grain <= 0 {
        return Err(make_error("invalid_topology", format!("{OPERATION}: grain must be positive")));
    }
    if blank_attribute_name(&params.attribute) {
        return Err(make_error("invalid_topology", format!("{OPERATION}: attribute name must not be empty")));
    }
    if !params.cusp_angle.is_finite()
        || params.cusp_angle < 0.0
        || params.cusp_angle > std::f64::consts::PI
    {
        return Err(make_error(
            "invalid_topology",
            format!("{OPERATION}: cusp angle must be finite and in [0, pi]"),
        ));
    }
    cancel::check_opt(cancel_value)
        .map_err(|_| make_error("cancelled", "geometry operation was cancelled".to_owned()))?;
    let topology_value = geometry::topology(geometry_value);
    element_selection::validate("normals", topology_value, params.selection.as_ref())
        .map_err(remap_error)?;
    validate_primitives(topology_value, params.primitives.as_ref())?;

    if params.selection.is_none()
        && params.primitives.is_none()
        && params.owner == attribute::Owner::Point
        && params.weighting == Weighting::FaceArea
        && params.cusp_angle == std::f64::consts::PI
        && params.attribute == "N"
    {
        match geometry::find_attribute(attribute::Owner::Point, "N", geometry_value) {
            None => return fast_point_area(cancel_value, grain as usize, geometry_value),
            Some(value) => {
                if matches!(
                    attribute::View::storage(&value).map_err(remap_error)?,
                    attribute::Storage::Float3(_)
                ) {
                    return fast_point_area(cancel_value, grain as usize, geometry_value);
                }
            }
        }
    }

    let existing = existing_planes(params.owner, &params.attribute, geometry_value)?;
    let authored_selection = selection_group(
        cancel_value,
        grain,
        topology_value,
        params.selection.as_ref(),
        params.owner,
    )?;
    let effective_selection = match (&existing, params.owner) {
        (None, attribute::Owner::Vertex) => authored_selection,
        (None, _) => None,
        (Some(_), _) => authored_selection,
    };
    let need_inverse = params.owner == attribute::Owner::Vertex
        || params.owner == attribute::Owner::Primitive
        || params.weighting != Weighting::FaceArea;
    let faces = compute_faces(
        cancel_value,
        grain,
        need_inverse,
        params.primitives.as_ref(),
        geometry_value,
    )?;
    let grain_usize = grain as usize;
    match params.owner {
        attribute::Owner::Point => {
            let sums = accumulate_points(cancel_value, params.weighting, geometry_value, &faces)?;
            let count = usize::try_from(geometry::point_count(geometry_value))
                .map_err(|_| make_error("invalid_topology", format!("{OPERATION}: invalid point count")))?;
            let mut output = if effective_selection.is_none() {
                initial(count, None)
            } else {
                initial(count, existing.as_ref())
            };
            write_points(
                cancel_value,
                grain_usize,
                effective_selection.as_ref(),
                &mut output,
                &sums,
            )?;
            install(params.owner, &params.attribute, output, geometry_value)
        }
        attribute::Owner::Vertex => {
            let count = usize::try_from(geometry::vertex_count(geometry_value))
                .map_err(|_| make_error("invalid_topology", format!("{OPERATION}: invalid vertex count")))?;
            let mut output = initial(count, existing.as_ref());
            let write_smooth = |selection: Option<&group::T>,
                                output: &mut Planes|
             -> Result<(), error::T> {
                let sums = accumulate_points(cancel_value, params.weighting, geometry_value, &faces)?;
                write_smooth_vertices(
                    cancel_value,
                    grain_usize,
                    selection,
                    topology_value,
                    output,
                    &sums,
                )
            };
            let write_requested = |selection: Option<&group::T>,
                                   output: &mut Planes|
             -> Result<(), error::T> {
                if params.cusp_angle >= std::f64::consts::PI - 1e-12 {
                    write_smooth(selection, output)
                } else {
                    let index = point_index::create(cancel_value, topology_value)
                        .map_err(remap_error)?;
                    write_cusped_vertices(
                        cancel_value,
                        grain_usize,
                        params.weighting,
                        params.cusp_angle,
                        selection,
                        geometry_value,
                        &index,
                        &faces,
                        output,
                    )
                }
            };
            if existing.is_none() && effective_selection.is_some() {
                write_smooth(None, &mut output)?;
            }
            write_requested(effective_selection.as_ref(), &mut output)?;
            install(params.owner, &params.attribute, output, geometry_value)
        }
        attribute::Owner::Primitive => {
            let count = usize::try_from(geometry::primitive_count(geometry_value))
                .map_err(|_| make_error("invalid_topology", format!("{OPERATION}: invalid primitive count")))?;
            let mut output = if effective_selection.is_none() {
                initial(count, None)
            } else {
                initial(count, existing.as_ref())
            };
            let mut errors = vec![-1i64; count.div_ceil(grain_usize)];
            for (range, first) in (0..count).step_by(grain_usize).enumerate() {
                let last = (first + grain_usize).min(count);
                for primitive in first..last {
                    poll(cancel_value, primitive as i64, 4095)?;
                    if selected(effective_selection.as_ref(), primitive as i64)
                        && !store(
                            &mut output,
                            primitive,
                            faces.raw.x[primitive],
                            faces.raw.y[primitive],
                            faces.raw.z[primitive],
                        )
                    {
                        errors[range] = primitive as i64;
                    }
                }
            }
            if let Some(primitive) = first_error(errors) {
                return Err(make_error(
                    "invalid_topology",
                    format!("{OPERATION}: non-finite primitive normal at {primitive}"),
                ));
            }
            install(params.owner, &params.attribute, output, geometry_value)
        }
        attribute::Owner::Detail => {
            let mut sx = 0.0;
            let mut sy = 0.0;
            let mut sz = 0.0;
            for primitive in 0..geometry::primitive_count(geometry_value) {
                poll(cancel_value, primitive, 4095)?;
                sx += faces.raw.x[primitive as usize];
                sy += faces.raw.y[primitive as usize];
                sz += faces.raw.z[primitive as usize];
            }
            let mut output = initial(1, existing.as_ref());
            if !store(&mut output, 0, sx, sy, sz) {
                return Err(make_error(
                    "invalid_topology",
                    format!("{OPERATION}: non-finite detail normal"),
                ));
            }
            install(params.owner, &params.attribute, output, geometry_value)
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    fn triangle() -> geometry::T {
        let topology_value =
            topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let positions = packed::Float3::View::of_owned_exn(
            vec![0.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0],
        )
        .unwrap();
        geometry::create(positions, topology_value, None, None, None).unwrap()
    }

    fn check_up_normals(owner: attribute::Owner, geometry_value: &geometry::T) {
        let value = geometry::find_attribute(owner, "N", geometry_value).unwrap();
        let attribute::Storage::Float3(values) = attribute::View::storage(&value).unwrap() else {
            panic!("normal output is not float3");
        };
        let values = packed::Float3::View::view(&values).unwrap();
        for z in values.z.iter() {
            assert_eq!(*z, 1.0);
        }
        for x in values.x.iter().chain(values.y.iter()) {
            assert_eq!(*x, 0.0);
        }
    }

    #[test]
    fn area_weighted_point_and_angle_weighted_vertex_normals_follow_face_winding() {
        let geometry_value = triangle();
        let points = Params {
            cancel: None,
            grain: Some(1),
            selection: None,
            primitives: None,
            owner: attribute::Owner::Point,
            weighting: Weighting::FaceArea,
            cusp_angle: std::f64::consts::PI,
            attribute: "N".to_owned(),
        };
        let point_normals = run(&points, &geometry_value).unwrap();
        check_up_normals(attribute::Owner::Point, &point_normals);

        let vertices = Params {
            owner: attribute::Owner::Vertex,
            weighting: Weighting::VertexAngle,
            ..points
        };
        let vertex_normals = run(&vertices, &geometry_value).unwrap();
        check_up_normals(attribute::Owner::Vertex, &vertex_normals);
    }
    fn params(cancel: Option<cancel::T>, grain: Option<i64>) -> Params {
        Params {
            cancel,
            grain,
            selection: None,
            primitives: None,
            owner: attribute::Owner::Point,
            weighting: Weighting::FaceArea,
            cusp_angle: std::f64::consts::PI,
            attribute: "N".to_owned(),
        }
    }

    #[test]
    fn invalid_arguments_precede_cancellation() {
        let geometry_value = triangle();
        let token = cancel::create();
        cancel::cancel(&token);
        let error = run(&params(Some(token), Some(0)), &geometry_value).err().unwrap();
        assert_eq!(error::code(&error), "invalid_topology");

        let token = cancel::create();
        cancel::cancel(&token);
        let error = run(&params(Some(token), Some(1)), &geometry_value).err().unwrap();
        assert_eq!(error::code(&error), "cancelled");
    }

    #[test]
    fn nonbreaking_space_is_a_valid_attribute_name() {
        let mut parameters = params(None, Some(1));
        parameters.attribute = "\u{00a0}".to_owned();
        let output = run(&parameters, &triangle()).unwrap();
        assert!(geometry::find_attribute(attribute::Owner::Point, "\u{00a0}", &output).is_some());
    }
}
