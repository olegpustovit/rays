//! Explicit bridge to Rays's renderer mesh. Point and vertex `N`, `Cd`, and `uv` attributes are
//! preserved; primitive `Cd` is expanded to corners when neither vertex nor point `Cd` is present.
//! RGB float3 `Cd` gets opaque alpha; float4 retains alpha. Vertex-owned attributes cause
//! deterministic corner expansion because Rays's render mesh has one attribute tuple per render
//! vertex. Both values remain immutable.
//!
//! Gap against OCaml: rays' `Mesh` planes are `Arc<Vec<f64>>` and rdk's are `Arc<Vec<f64>>`, so the
//! conversion copies the planes and indices where OCaml shared them.
use std::sync::Arc;

use rays::mesh::{self, Vec3_buffer};
use rays_math::{color, vec2};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::topology::{self, PrimitiveKind};
use rdk_core::{error, geometry, normal_ops, packed, topology_remap};
use rdk_ops::triangulate;

enum Planes {
    F2(packed::Float2::T),
    F3(packed::Float3::T),
    F4(packed::Float4::T),
}

enum Source {
    Point(Planes),
    Vertex(Planes),
}

fn text(e: error::T) -> String {
    error::to_string(&e)
}

fn mesh_vec3(view: &packed::Float3::View::View) -> Vec3_buffer {
    Vec3_buffer::of_planes(view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
}

fn owner_name(owner: Owner) -> &'static str {
    match owner {
        Owner::Point => "point",
        Owner::Vertex => "vertex",
        Owner::Primitive => "primitive",
        Owner::Detail => "detail",
    }
}

fn standard(
    g: &geometry::T,
    name: &str,
    point_kind: fn(&Storage) -> Option<Planes>,
    vertex_kind: fn(&Storage) -> Option<Planes>,
) -> Result<Option<Source>, String> {
    let read = |owner: Owner, expected: fn(&Storage) -> Option<Planes>| -> Result<Option<Planes>, String> {
        let found = if owner == Owner::Vertex && geometry::vertex_count(g) == 0 {
            None
        } else {
            geometry::find_attribute(owner, name, g)
        };
        let Some(a) = found else { return Ok(None) };
        let storage = attribute::View::storage(&a).map_err(text)?;
        match expected(storage) {
            Some(value) => Ok(Some(value)),
            None => Err(format!(
                "Rdk.Rays_mesh.to_mesh: {} {} attribute has storage {}",
                owner_name(owner),
                name,
                attribute::kind_name(&a)
            )),
        }
    };
    if let Some(value) = read(Owner::Vertex, vertex_kind)? {
        return Ok(Some(Source::Vertex(value)));
    }
    Ok(read(Owner::Point, point_kind)?.map(Source::Point))
}

fn float2(s: &Storage) -> Option<Planes> {
    match s {
        Storage::Float2(v) => Some(Planes::F2(v.clone())),
        _ => None,
    }
}

fn float3(s: &Storage) -> Option<Planes> {
    match s {
        Storage::Float3(v) => Some(Planes::F3(v.clone())),
        _ => None,
    }
}

fn float4(s: &Storage) -> Option<Planes> {
    match s {
        Storage::Float4(v) => Some(Planes::F4(v.clone())),
        Storage::Float3(v) => {
            let view = packed::Float3::View::view(v).ok()?;
            packed::Float4::of_owned(view.x.to_vec(), view.y.to_vec(), view.z.to_vec(), vec![1.0; view.x.len()]).ok().map(Planes::F4)
        }
        _ => None,
    }
}

fn source_has_vertex(s: &Option<Source>) -> bool {
    matches!(s, Some(Source::Vertex(_)))
}

/// Index `i` of an expanded array: a point source reads through the corner's point.
fn source_index(source: &Source, vertex_points: &[i64], vertex: usize) -> usize {
    match source {
        Source::Point(_) => vertex_points[vertex] as usize,
        Source::Vertex(_) => vertex,
    }
}

fn vertex_points_of(topo: &topology::T) -> Result<Arc<Vec<i64>>, String> {
    Ok(topology::View::view(topo).map_err(text)?.vertex_points)
}

fn expanded_vec3(vertex_points: &[i64], count: usize, source: &Source) -> Result<Vec3_buffer, String> {
    let values = match source {
        Source::Point(Planes::F3(v)) | Source::Vertex(Planes::F3(v)) => v,
        _ => unreachable!("position and normal sources are Float3"),
    };
    let input = packed::Float3::View::view(values).map_err(text)?;
    let (mut x, mut y, mut z) = (vec![0.0; count], vec![0.0; count], vec![0.0; count]);
    for vertex in 0..count {
        let i = source_index(source, vertex_points, vertex);
        x[vertex] = input.x[i];
        y[vertex] = input.y[i];
        z[vertex] = input.z[i];
    }
    Ok(Vec3_buffer::of_planes(x, y, z))
}

fn color_array(expanded: bool, vertex_points: &[i64], count: usize, source: &Source) -> Vec<color::T> {
    let values = match source {
        Source::Point(Planes::F4(v)) | Source::Vertex(Planes::F4(v)) => v,
        _ => unreachable!("color sources are Float4"),
    };
    let input = packed::Float4::View::view(values);
    (0..count)
        .map(|vertex| {
            let i = match source {
                Source::Point(_) if expanded => vertex_points[vertex] as usize,
                Source::Point(_) | Source::Vertex(_) => vertex,
            };
            color::of_floats(input.x[i], input.y[i], input.z[i], input.w[i])
        })
        .collect()
}

fn tex_coord_array(expanded: bool, vertex_points: &[i64], count: usize, source: &Source) -> Vec<vec2::T> {
    let values = match source {
        Source::Point(Planes::F2(v)) | Source::Vertex(Planes::F2(v)) => v,
        _ => unreachable!("tex-coord sources are Float2"),
    };
    let input = packed::Float2::View::view(values);
    (0..count)
        .map(|vertex| {
            let i = match source {
                Source::Point(_) if expanded => vertex_points[vertex] as usize,
                Source::Point(_) | Source::Vertex(_) => vertex,
            };
            vec2::create(input.x[i], input.y[i])
        })
        .collect()
}

fn primitive_kinds(topo: &topology::T) -> Result<Vec<PrimitiveKind>, String> {
    (0..topology::primitive_count(topo)).map(|p| topology::primitive_kind(topo, p).map_err(text)).collect()
}

fn all_curves(kinds: &[PrimitiveKind]) -> bool {
    kinds.iter().all(|k| *k != PrimitiveKind::Polygon)
}

fn curve_indices(topo: &topology::T, kinds: &[PrimitiveKind], expanded: bool) -> Result<Vec<i64>, String> {
    let vertex_points = vertex_points_of(topo)?;
    let mut edge_count = 0i64;
    for (p, kind) in kinds.iter().enumerate() {
        let size = topology::primitive_size(topo, p as i64).map_err(text)?;
        edge_count += size - 1 + i64::from(*kind == PrimitiveKind::ClosedPolyline);
    }
    let mut indices = Vec::with_capacity((edge_count.max(0) * 2) as usize);
    let index = |vertex: i64| if expanded { vertex } else { vertex_points[vertex as usize] };
    for (p, kind) in kinds.iter().enumerate() {
        let (first, last) = topology::primitive_vertex_range(topo, p as i64).map_err(text)?;
        for vertex in first..last - 1 {
            indices.push(index(vertex));
            indices.push(index(vertex + 1));
        }
        if *kind == PrimitiveKind::ClosedPolyline {
            indices.push(index(last - 1));
            indices.push(index(first));
        }
    }
    Ok(indices)
}

type Standard = (
    Option<Source>,
    Option<Source>,
    Option<Source>,
);

fn standards(g: &geometry::T) -> Result<Standard, String> {
    // OCaml evaluates the tuple right to left; the first error reported is the same set of
    // failures, and each read is independent, so report in N, Cd, uv order of the pattern match.
    let n = standard(g, "N", float3, float3);
    let cd = standard(g, "Cd", float4, float4);
    let uv = standard(g, "uv", float2, float2);
    Ok((n?, cd?, uv?))
}

fn shared_normals(normals: Option<Source>) -> Result<Option<Vec3_buffer>, String> {
    match normals {
        None => Ok(None),
        Some(Source::Point(Planes::F3(values))) => Ok(Some(mesh_vec3(&packed::Float3::View::view(&values).map_err(text)?))),
        Some(Source::Vertex(_)) => unreachable!("vertex normals take the expanded path"),
        Some(Source::Point(_)) => unreachable!("normal sources are Float3"),
    }
}

fn expanded_normals(
    vertex_points: &[i64],
    count: usize,
    normals: &Option<Source>,
) -> Result<Option<Vec3_buffer>, String> {
    match normals {
        None => Ok(None),
        Some(source) => expanded_vec3(vertex_points, count, source).map(Some),
    }
}

fn curve_mesh(g: &geometry::T, kinds: &[PrimitiveKind]) -> Result<mesh::t, String> {
    let topo = geometry::topology(g);
    let (normals, colors, tex_coords) = standards(g)?;
    let expand = source_has_vertex(&normals) || source_has_vertex(&colors) || source_has_vertex(&tex_coords);
    let count = (if expand { geometry::vertex_count(g) } else { geometry::point_count(g) }) as usize;
    let indices = curve_indices(topo, kinds, expand)?;
    let vertex_points = vertex_points_of(topo)?;
    let colors_out = colors.as_ref().map(|s| color_array(expand, &vertex_points, count, s));
    let tex_out = tex_coords.as_ref().map(|s| tex_coord_array(expand, &vertex_points, count, s));
    if expand {
        let positions = expanded_vec3(&vertex_points, count, &Source::Point(Planes::F3(geometry::positions(g).clone())))?;
        let normals = expanded_normals(&vertex_points, count, &normals)?;
        mesh::View::create_packed_owned(Some(mesh::mode::Lines), Some(indices), normals, colors_out, tex_out, positions)
    } else {
        let positions = mesh_vec3(&packed::Float3::View::view(geometry::positions(g)).map_err(text)?);
        let normals = shared_normals(normals)?;
        mesh::View::create_packed_shared(
            Some(mesh::mode::Lines),
            Some(Arc::new(indices)),
            normals,
            colors_out.map(Arc::new),
            tex_out.map(Arc::new),
            positions,
        )
    }
}

fn to_mesh_raw_impl(g: &geometry::T) -> Result<mesh::t, String> {
    let topo = geometry::topology(g);
    let primitives = topology::primitive_count(topo);
    if primitives != 0 {
        let kinds = primitive_kinds(topo)?;
        if all_curves(&kinds) {
            return curve_mesh(g, &kinds);
        }
        if !topology::all_triangles(topo).map_err(text)? {
            let triangulated = triangulate::run(&triangulate::Params { cancel: None, grain: None, primitives: None }, g).map_err(text)?;
            return to_mesh_raw_impl(&triangulated);
        }
    }
    let (normals, colors, tex_coords) = standards(g)?;
    let has_primitives = primitives != 0;
    let expand = has_primitives && (source_has_vertex(&normals) || source_has_vertex(&colors) || source_has_vertex(&tex_coords));
    let vertex_points = vertex_points_of(topo)?;
    if expand {
        let count = topology::vertex_count(topo) as usize;
        let positions = expanded_vec3(&vertex_points, count, &Source::Point(Planes::F3(geometry::positions(g).clone())))?;
        let normals = expanded_normals(&vertex_points, count, &normals)?;
        let colors = colors.as_ref().map(|s| color_array(true, &vertex_points, count, s));
        let tex_coords = tex_coords.as_ref().map(|s| tex_coord_array(true, &vertex_points, count, s));
        let indices: Vec<i64> = (0..count as i64).collect();
        mesh::View::create_packed_owned(Some(mesh::mode::Triangles), Some(indices), normals, colors, tex_coords, positions)
    } else {
        let positions = mesh_vec3(&packed::Float3::View::view(geometry::positions(g)).map_err(text)?);
        let normals = shared_normals(normals)?;
        let count = geometry::point_count(g) as usize;
        let colors = colors.as_ref().map(|s| Arc::new(color_array(false, &vertex_points, count, s)));
        let tex_coords = tex_coords.as_ref().map(|s| Arc::new(tex_coord_array(false, &vertex_points, count, s)));
        if !has_primitives {
            mesh::View::create_packed_shared(Some(mesh::mode::Points), None, normals, colors, tex_coords, positions)
        } else {
            mesh::View::create_packed_shared(
                Some(mesh::mode::Triangles),
                Some(Arc::new(vertex_points.to_vec())),
                normals,
                colors,
                tex_coords,
                positions,
            )
        }
    }
}

fn protected(operation: &str, work: &dyn Fn() -> Result<mesh::t, String>) -> Result<mesh::t, error::T> {
    work().map_err(|message| {
        error::make(operation.to_owned(), "mesh_conversion".to_owned(), message, None).unwrap_or_else(|e| e)
    })
}

/// Triangulate a geometry, compute the path tracer's smoothed vertex normals, and return the packed
/// triangle mesh it consumes.
pub fn to_trace_mesh(g: &geometry::T) -> Result<mesh::t, error::T> {
    protected("to_trace_mesh", &|| {
        let triangles = triangulate::run(&triangulate::Params { cancel: None, grain: None, primitives: None }, g).map_err(text)?;
        let triangles = normal_ops::run(
            &normal_ops::Params {
                cancel: None,
                grain: None,
                selection: None,
                primitives: None,
                owner: Owner::Vertex,
                weighting: normal_ops::Weighting::FaceArea,
                cusp_angle: std::f64::consts::PI / 4.5,
                attribute: "N".to_owned(),
            },
            &triangles,
        )
        .map_err(text)?;
        to_mesh_raw_impl(&triangles)
    })
}

fn rdk_float3(view: &Vec3_buffer) -> Result<packed::Float3::T, String> {
    packed::Float3::View::of_shared_exn(Arc::new(view.x.to_vec()), Arc::new(view.y.to_vec()), Arc::new(view.z.to_vec())).map_err(text)
}

fn color_attribute(colors: &[color::T]) -> Result<attribute::T, String> {
    let n = colors.len();
    let (mut r, mut g, mut b, mut a) = (vec![0.0; n], vec![0.0; n], vec![0.0; n], vec![0.0; n]);
    for (i, c) in colors.iter().enumerate() {
        (r[i], g[i], b[i], a[i]) = color::to_floats(*c);
    }
    let values = packed::Float4::of_owned(r, g, b, a).map_err(text)?;
    attribute::create_key_owned(&attribute::color(Owner::Point), Storage::Float4(values)).map_err(text)
}

fn tex_coord_attribute(tex_coords: &[vec2::T]) -> Result<attribute::T, String> {
    let u: Vec<f64> = tex_coords.iter().map(|p| p.x).collect();
    let v: Vec<f64> = tex_coords.iter().map(|p| p.y).collect();
    let values = packed::Float2::of_owned(u, v).map_err(text)?;
    attribute::create_key_owned(&attribute::tex_coord(Owner::Point), Storage::Float2(values)).map_err(text)
}

fn of_mesh_raw_impl(m: &mesh::t) -> Result<geometry::T, String> {
    let view = mesh::View::packed_view(m)?;
    match view.mode {
        mesh::mode::Points | mesh::mode::Triangles => {
            let positions = rdk_float3(&view.vertices)?;
            let point_count = packed::Float3::length(&positions);
            let topo = if view.mode == mesh::mode::Points {
                topology::empty(point_count).map_err(text)
            } else if view.indices.len() % 3 != 0 {
                Err("Rdk.Rays_mesh.of_mesh: malformed triangle index count".to_owned())
            } else {
                let primitive_count = view.indices.len() / 3;
                let offsets: Vec<i64> = (0..=primitive_count as i64).map(|i| i * 3).collect();
                topology::View::polygons_shared(point_count, view.indices.to_vec(), offsets).map_err(text)
            }?;
            let mut attributes = Vec::new();
            if let Some(normals) = &view.normals {
                let values = rdk_float3(normals)?;
                attributes.push(attribute::create_key_owned(&attribute::normal(Owner::Point), Storage::Float3(values)).map_err(text)?);
            }
            if let Some(colors) = &view.colors {
                attributes.push(color_attribute(colors)?);
            }
            if let Some(tex_coords) = &view.tex_coords {
                attributes.push(tex_coord_attribute(tex_coords)?);
            }
            geometry::create(positions, topo, Some(attributes), None, None).map_err(text)
        }
        _ => Err("Rdk.Rays_mesh.of_mesh: expand non-point/non-triangle modes first".to_owned()),
    }
}

fn to_geometry_mesh(g: &geometry::T) -> Result<mesh::t, String> {
    // A primitive color belongs to all of its corners, never to a shared point: expanding it
    // preserves material boundaries between faces.
    let expand = geometry::primitive_count(g) != 0
        && geometry::find_attribute(Owner::Vertex, "Cd", g).is_none()
        && geometry::find_attribute(Owner::Point, "Cd", g).is_none()
        && geometry::find_attribute(Owner::Primitive, "Cd", g).is_some();
    if !expand {
        return to_mesh_raw_impl(g);
    }
    let view = topology::View::view(geometry::topology(g)).map_err(text)?;
    let mut corner_primitive = vec![0i64; view.vertex_points.len()];
    for primitive in 0..view.primitive_kinds.len() {
        let (from, to) = (view.primitive_offsets[primitive] as usize, view.primitive_offsets[primitive + 1] as usize);
        corner_primitive[from..to].fill(primitive as i64);
    }
    let cd = geometry::find_attribute(Owner::Primitive, "Cd", g).expect("checked above");
    let spread = topology_remap::attribute(None, 16_384, None, &corner_primitive, &cd).map_err(text)?;
    let storage = attribute::storage(&spread).map_err(text)?;
    let corners = attribute::create_owned("Cd", Owner::Vertex, storage).map_err(text)?;
    let g = geometry::with_attribute(corners, g).map_err(text)?;
    to_mesh_raw_impl(&g)
}

pub fn to_mesh(g: &geometry::T) -> Result<mesh::t, error::T> {
    protected("to_mesh", &|| to_geometry_mesh(g))
}

pub fn of_mesh(m: &mesh::t) -> Result<geometry::T, error::T> {
    of_mesh_raw_impl(m).map_err(|message| {
        error::make("of_mesh".to_owned(), "mesh_conversion".to_owned(), message, None).unwrap_or_else(|e| e)
    })
}
