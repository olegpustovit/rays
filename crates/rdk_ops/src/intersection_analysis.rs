//! Emit one welded point per triangle/triangle intersection location. One input performs AxA
//! analysis; two inputs perform AxB analysis. Provenance attributes are aligned point-owned CSR
//! rows: input number, primitive number, primitive barycentric triplets, and an incident source
//! point number or `-1`. Inputs are not passed through.
use std::sync::Arc;

use rdk_core::{attribute, cancel, error, geometry, group, packed, support, topology};
use rdk_core::error::Code;
use rdk_geom::curve_intersection;
use rdk_geom::linear_piece_index::{self, Kind};
use rdk_geom::point_clusters::{self, Metric};
use rdk_geom::point_grid::{float_max, float_min};
use rdk_geom::triangle_intersection::{self, EVENT_STRIDE, MAX_EVENTS};

const OPERATION: &str = "intersection_analysis";


pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub source_primitives: Option<group::T>,
    pub collision_primitives: Option<group::T>,
    pub tolerance: f64,
    pub include_coplanar: bool,
    pub input_attribute: Option<String>,
    pub primitive_attribute: Option<String>,
    pub primitive_uvw_attribute: Option<String>,
    pub point_attribute: Option<String>,
    pub collision: Option<geometry::T>,
}

impl Default for Params {
    fn default() -> Params {
        Params {
            cancel: None,
            grain: None,
            source_primitives: None,
            collision_primitives: None,
            tolerance: 0.0,
            include_coplanar: true,
            input_attribute: Some("sourceinput".to_owned()),
            primitive_attribute: Some("sourceprim".to_owned()),
            primitive_uvw_attribute: Some("sourceprimuv".to_owned()),
            point_attribute: Some("sourcepoint".to_owned()),
            collision: None,
        }
    }
}

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

fn lift(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        error::cancelled(OPERATION)
    } else {
        make(Code::InvalidGeometry, error::message(&value).to_owned())
    }
}

fn index_result(role: &str, value: error::T) -> error::T {
    make(error::code(&value), format!("{role}: {}", error::message(&value)))
}

fn duplicate_name(names: &[Option<&str>]) -> bool {
    let mut names: Vec<&str> = names.iter().flatten().copied().collect();
    names.sort();
    names.windows(2).any(|pair| pair[0] == pair[1])
}

struct Raw {
    x: Vec<f64>,
    y: Vec<f64>,
    z: Vec<f64>,
    au: Vec<f64>,
    av: Vec<f64>,
    aw: Vec<f64>,
    bu: Vec<f64>,
    bv: Vec<f64>,
    bw: Vec<f64>,
    source_piece: Vec<i64>,
    collision_piece: Vec<i64>,
}

#[allow(clippy::too_many_arguments)]
fn narrow(
    scratch: &mut [f64],
    point_info: &mut [i64],
    events: &mut [f64],
    is_self: bool,
    tolerance: f64,
    include_coplanar: bool,
    source_index: &linear_piece_index::T,
    collision_index: &linear_piece_index::T,
    source_piece: i64,
    collision_piece: i64,
) -> Result<usize, error::T> {
    use linear_piece_index::view as V;
    let source_positions = V::positions(source_index);
    let collision_positions = V::positions(collision_index);
    match (V::kind(source_index, source_piece), V::kind(collision_index, collision_piece)) {
        (Kind::Triangle, Kind::Triangle) => triangle_intersection::events_points_into(
            scratch,
            point_info,
            events,
            is_self,
            tolerance,
            include_coplanar,
            source_positions,
            V::point(source_index, source_piece, 0)?,
            V::point(source_index, source_piece, 1)?,
            V::point(source_index, source_piece, 2)?,
            collision_positions,
            V::point(collision_index, collision_piece, 0)?,
            V::point(collision_index, collision_piece, 1)?,
            V::point(collision_index, collision_piece, 2)?,
        ),
        (Kind::Segment, Kind::Segment) => curve_intersection::segment_segment_events_into(
            scratch,
            events,
            is_self,
            tolerance,
            source_positions,
            V::point(source_index, source_piece, 0)?,
            V::point(source_index, source_piece, 1)?,
            V::curve_u(source_index, source_piece, 0)?,
            V::curve_u(source_index, source_piece, 1)?,
            collision_positions,
            V::point(collision_index, collision_piece, 0)?,
            V::point(collision_index, collision_piece, 1)?,
            V::curve_u(collision_index, collision_piece, 0)?,
            V::curve_u(collision_index, collision_piece, 1)?,
        ),
        (Kind::Segment, Kind::Triangle) => curve_intersection::segment_triangle_events_into(
            scratch,
            events,
            is_self,
            tolerance,
            include_coplanar,
            true,
            source_positions,
            V::point(source_index, source_piece, 0)?,
            V::point(source_index, source_piece, 1)?,
            V::curve_u(source_index, source_piece, 0)?,
            V::curve_u(source_index, source_piece, 1)?,
            collision_positions,
            V::point(collision_index, collision_piece, 0)?,
            V::point(collision_index, collision_piece, 1)?,
            V::point(collision_index, collision_piece, 2)?,
        ),
        (Kind::Triangle, Kind::Segment) => curve_intersection::segment_triangle_events_into(
            scratch,
            events,
            is_self,
            tolerance,
            include_coplanar,
            false,
            collision_positions,
            V::point(collision_index, collision_piece, 0)?,
            V::point(collision_index, collision_piece, 1)?,
            V::curve_u(collision_index, collision_piece, 0)?,
            V::curve_u(collision_index, collision_piece, 1)?,
            source_positions,
            V::point(source_index, source_piece, 0)?,
            V::point(source_index, source_piece, 1)?,
            V::point(source_index, source_piece, 2)?,
        ),
    }
}

/// Events of one contiguous candidate range, joined in range order.
#[derive(Default)]
struct RangeEvents {
    floats: Vec<f64>,
    source_piece: Vec<i64>,
    collision_piece: Vec<i64>,
}

fn event_pass(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    is_self: bool,
    tolerance: f64,
    include_coplanar: bool,
    source_index: &linear_piece_index::T,
    collision_index: &linear_piece_index::T,
) -> Result<Raw, error::T> {
    let (source_pieces, collision_pieces) = if is_self {
        linear_piece_index::view::overlapping_self_pairs(cancel_value, grain, tolerance, source_index)?
    } else {
        linear_piece_index::view::overlapping_pairs(cancel_value, grain, tolerance, source_index, collision_index)?
    };
    let candidate_count = source_pieces.len() as i64;
    let chunk = grain.max(1);
    // ponytail: event counts are unknown until narrow runs; retain range buffers to avoid running it twice.
    // A count-only narrow API would allow prefix-sized final output windows.
    let mut parts: Vec<_> = (0..support::range_count(candidate_count, chunk)).map(|_| RangeEvents::default()).collect();
    {
        let windows = support::Windows::even(&mut parts, 1);
        support::run_indexed_ranges(chunk, candidate_count, &|range, first, last| {
            let mut scratch = [0.0_f64; 26];
            let mut point_info = [0_i64; 10];
            let mut events = [0.0_f64; MAX_EVENTS * EVENT_STRIDE];
            let part = &mut windows.take(range)[0];
            for candidate in first..last {
                cancel::poll(cancel_value, candidate, 4095)?;
                let source_piece = source_pieces[candidate as usize];
                let collision_piece = collision_pieces[candidate as usize];
                let count = narrow(
                    &mut scratch,
                    &mut point_info,
                    &mut events,
                    is_self,
                    tolerance,
                    include_coplanar,
                    source_index,
                    collision_index,
                    source_piece,
                    collision_piece,
                )?;
                part.floats.extend_from_slice(&events[..count * EVENT_STRIDE]);
                for _ in 0..count {
                    part.source_piece.push(source_piece);
                    part.collision_piece.push(collision_piece);
                }
            }
            Ok(())
        })?;
    }
    let count = parts.iter().map(|part| part.source_piece.len()).sum();
    let mut raw = Raw {
        x: Vec::with_capacity(count),
        y: Vec::with_capacity(count),
        z: Vec::with_capacity(count),
        au: Vec::with_capacity(count),
        av: Vec::with_capacity(count),
        aw: Vec::with_capacity(count),
        bu: Vec::with_capacity(count),
        bv: Vec::with_capacity(count),
        bw: Vec::with_capacity(count),
        source_piece: Vec::with_capacity(count),
        collision_piece: Vec::with_capacity(count),
    };
    for part in parts {
        for event in part.floats.chunks_exact(EVENT_STRIDE) {
            raw.x.push(event[0]);
            raw.y.push(event[1]);
            raw.z.push(event[2]);
            raw.au.push(event[3]);
            raw.av.push(event[4]);
            raw.aw.push(event[5]);
            raw.bu.push(event[6]);
            raw.bv.push(event[7]);
            raw.bw.push(event[8]);
        }
        raw.source_piece.extend(part.source_piece);
        raw.collision_piece.extend(part.collision_piece);
    }
    Ok(raw)
}

fn numerical_weld_tolerance(requested: f64, raw: &Raw) -> f64 {
    let mut extent = 0.0_f64;
    let mut magnitude = 0.0_f64;
    if !raw.x.is_empty() {
        let (mut min_x, mut max_x) = (raw.x[0], raw.x[0]);
        let (mut min_y, mut max_y) = (raw.y[0], raw.y[0]);
        let (mut min_z, mut max_z) = (raw.z[0], raw.z[0]);
        for point in 0..raw.x.len() {
            let (x, y, z) = (raw.x[point], raw.y[point], raw.z[point]);
            min_x = float_min(min_x, x);
            max_x = float_max(max_x, x);
            min_y = float_min(min_y, y);
            max_y = float_max(max_y, y);
            min_z = float_min(min_z, z);
            max_z = float_max(max_z, z);
            magnitude = float_max(magnitude, float_max(x.abs(), float_max(y.abs(), z.abs())));
        }
        extent = float_max(max_x - min_x, float_max(max_y - min_y, max_z - min_z));
    }
    let next = magnitude.next_up();
    let magnitude_ulp = if next.is_finite() { next - magnitude } else { 0.0 };
    float_max(requested, float_max(8.0 * magnitude_ulp, 256.0 * f64::EPSILON * float_max(1.0, extent)))
}

fn cluster_events(cancel_value: Option<&cancel::T>, tolerance: f64, raw: &Raw) -> Result<point_clusters::T, error::T> {
    let positions = packed::float3::view::of_shared_exn(Arc::new(raw.x.clone()), Arc::new(raw.y.clone()), Arc::new(raw.z.clone())).map_err(lift)?;
    let temporary =
        geometry::create(positions, topology::empty(raw.x.len() as i64).map_err(lift)?, None, None, None)
            .map_err(lift)?;
    point_clusters::create(
        cancel_value,
        None,
        Metric::Euclidean,
        true,
        "Intersection Analysis weld",
        tolerance,
        &|_, _| true,
        &temporary,
    )
    .map_err(lift)
}

fn representative_data(clusters: &point_clusters::T, raw: &Raw) -> (i64, Vec<i64>, Vec<i64>) {
    match clusters {
        point_clusters::T::Identity => {
            let n = raw.x.len() as i64;
            (n, (0..n).collect(), (0..n).collect())
        }
        point_clusters::T::Clusters(value) => (value.count, value.of_point.clone(), value.representatives.clone()),
    }
}

fn point_of_piece_vertex(index: &linear_piece_index::T, piece: i64, u: f64, v: f64, w: f64) -> Result<i64, error::T> {
    use linear_piece_index::view as V;
    let epsilon = 512.0 * f64::EPSILON;
    match V::kind(index, piece) {
        Kind::Triangle => {
            let local = if u >= 1.0 - epsilon && v <= epsilon && w <= epsilon {
                0
            } else if v >= 1.0 - epsilon && u <= epsilon && w <= epsilon {
                1
            } else if w >= 1.0 - epsilon && u <= epsilon && v <= epsilon {
                2
            } else {
                -1
            };
            if local < 0 { Ok(-1) } else { V::point(index, piece, local) }
        }
        Kind::Segment => {
            let u0 = V::curve_u(index, piece, 0)?;
            let u1 = V::curve_u(index, piece, 1)?;
            if (u - u0).abs() <= epsilon {
                V::point(index, piece, 0)
            } else if (u - u1).abs() <= epsilon {
                V::point(index, piece, 1)
            } else {
                Ok(-1)
            }
        }
    }
}

fn provenance_piece(index: &linear_piece_index::T, piece: i64, u: f64) -> Result<i64, error::T> {
    use linear_piece_index::view as V;
    match V::kind(index, piece) {
        Kind::Triangle => Ok(piece),
        Kind::Segment => {
            let epsilon = 512.0 * f64::EPSILON;
            let u0 = V::curve_u(index, piece, 0)?;
            if V::local(index, piece) > 0 && (u - u0).abs() <= epsilon { Ok(piece - 1) } else { Ok(piece) }
        }
    }
}

struct Provenance {
    x: Vec<f64>,
    y: Vec<f64>,
    z: Vec<f64>,
    offsets: Vec<i64>,
    inputs: Vec<i64>,
    primitives: Vec<i64>,
    uvw: Vec<f64>,
    point_numbers: Vec<i64>,
}

fn build_provenance(
    cancel_value: Option<&cancel::T>,
    is_self: bool,
    source_index: &linear_piece_index::T,
    collision_index: &linear_piece_index::T,
    clusters: &point_clusters::T,
    raw: &Raw,
) -> Result<Provenance, error::T> {
    use linear_piece_index::view as V;
    let (point_count, of_event, representatives) = representative_data(clusters, raw);
    let source_count = linear_piece_index::piece_count(source_index);
    let collision_count = if is_self { 0 } else { linear_piece_index::piece_count(collision_index) };
    let key_count = (source_count + collision_count) as usize;
    let mut seen = vec![-1_i64; key_count];
    let mut counts = vec![0_i64; point_count as usize];
    let event_count = raw.x.len();
    let collision_input = if is_self { 0 } else { 1 };
    // Collision records preserve source-record then collision order.
    for event in 0..event_count {
        cancel::poll(cancel_value, event as i64, 4095)?;
        let point = of_event[event];
        for (input, piece, index, u) in [
            (0_i64, raw.source_piece[event], source_index, raw.au[event]),
            (collision_input, raw.collision_piece[event], collision_index, raw.bu[event]),
        ] {
            let canonical = provenance_piece(index, piece, u)?;
            let key = if input == 0 { canonical } else { source_count + canonical } as usize;
            if seen[key] != point {
                seen[key] = point;
                counts[point as usize] += 1;
            }
        }
    }
    let mut offsets = vec![0_i64; point_count as usize + 1];
    for point in 0..point_count as usize {
        offsets[point + 1] = offsets[point] + counts[point];
    }
    seen.iter_mut().for_each(|slot| *slot = -1);
    let total = offsets[point_count as usize] as usize;
    let mut inputs = vec![0_i64; total];
    let mut primitives = vec![0_i64; total];
    let mut point_numbers = vec![-1_i64; total];
    let mut uvw = vec![0.0_f64; total * 3];
    let mut cursors = offsets.clone();
    for event in 0..event_count {
        cancel::poll(cancel_value, event as i64, 4095)?;
        let point = of_event[event];
        for (input, piece, index, u, v, w) in [
            (0_i64, raw.source_piece[event], source_index, raw.au[event], raw.av[event], raw.aw[event]),
            (collision_input, raw.collision_piece[event], collision_index, raw.bu[event], raw.bv[event], raw.bw[event]),
        ] {
            let canonical = provenance_piece(index, piece, u)?;
            let key = if input == 0 { canonical } else { source_count + canonical } as usize;
            if seen[key] != point {
                seen[key] = point;
                let output = cursors[point as usize] as usize;
                cursors[point as usize] = output as i64 + 1;
                inputs[output] = input;
                primitives[output] = V::primitive(index, piece);
                point_numbers[output] = point_of_piece_vertex(index, piece, u, v, w)?;
                uvw[output * 3] = u;
                uvw[output * 3 + 1] = v;
                uvw[output * 3 + 2] = w;
            }
        }
    }
    let x = representatives.iter().map(|&event| raw.x[event as usize]).collect();
    let y = representatives.iter().map(|&event| raw.y[event as usize]).collect();
    let z = representatives.iter().map(|&event| raw.z[event as usize]).collect();
    Ok(Provenance { x, y, z, offsets, inputs, primitives, uvw, point_numbers })
}

fn create_output(params: &Params, data: Provenance) -> Result<geometry::T, error::T> {
    let point_count = data.x.len() as i64;
    let mut attributes = Vec::new();
    let offsets = &data.offsets;
    let int_array = |name: &String, values: &Vec<i64>| -> Result<attribute::T, error::T> {
        let rows = packed::int_array::create_owned(offsets.clone(), values.clone()).map_err(lift)?;
        attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::IntArray(rows)).map_err(lift)
    };
    if let Some(name) = &params.input_attribute {
        attributes.push(int_array(name, &data.inputs)?);
    }
    if let Some(name) = &params.primitive_attribute {
        attributes.push(int_array(name, &data.primitives)?);
    }
    if let Some(name) = &params.primitive_uvw_attribute {
        let uv_offsets: Vec<i64> = data.offsets.iter().map(|offset| offset * 3).collect();
        let rows = packed::float_array::create_owned(uv_offsets, data.uvw.clone()).map_err(lift)?;
        attributes.push(
            attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::FloatArray(rows))
                .map_err(lift)?,
        );
    }
    if let Some(name) = &params.point_attribute {
        attributes.push(int_array(name, &data.point_numbers)?);
    }
    let positions = packed::float3::view::of_owned_exn(data.x, data.y, data.z).map_err(lift)?;
    geometry::create(positions, topology::empty(point_count).map_err(lift)?, Some(attributes), None, None)
        .map_err(lift)
}

fn validate_name(name: &Option<String>) -> bool {
    match name {
        None => true,
        Some(name) => !(name).trim().is_empty(),
    }
}

/// Emit one welded point per triangle/triangle intersection location. One input performs AxA
/// analysis; two inputs perform AxB analysis.
pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run_inner(params, geometry_value).map_err(|value| {
        if error::is_cancelled(&value) {
            error::cancelled(OPERATION)
        } else {
            value
        }
    })
}

fn run_inner(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let tolerance = params.tolerance;
    support::check_positive(grain).map_err(|_| make(Code::InvalidParameter, "grain must be positive".to_owned()))?;
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(make(Code::InvalidParameter, "tolerance must be finite and non-negative".to_owned()));
    }
    let names = [
        &params.input_attribute,
        &params.primitive_attribute,
        &params.primitive_uvw_attribute,
        &params.point_attribute,
    ];
    if !names.iter().all(|name| validate_name(name)) {
        return Err(make(Code::InvalidParameter, "output attribute names must not be empty".to_owned()));
    }
    let optional: Vec<Option<&str>> = names.iter().map(|name| name.as_deref()).collect();
    if duplicate_name(&optional) {
        return Err(make(Code::InvalidParameter, "output attribute names must be distinct".to_owned()));
    }
    cancel::check_opt(cancel_value)?;
    let is_self = params.collision.is_none();
    let collision_geometry = params.collision.as_ref().unwrap_or(geometry_value);
    let source_index = linear_piece_index::create(
        cancel_value,
        Some(grain),
        params.source_primitives.as_ref(),
        geometry_value,
    )
    .map_err(|value| index_result("source", value))?;
    let collision_index = if is_self {
        None
    } else {
        Some(
            linear_piece_index::create(
                cancel_value,
                Some(grain),
                params.collision_primitives.as_ref(),
                collision_geometry,
            )
            .map_err(|value| index_result("collision", value))?,
        )
    };
    let collision_ref = collision_index.as_ref().unwrap_or(&source_index);
    let raw = event_pass(cancel_value, grain, is_self, tolerance, params.include_coplanar, &source_index, collision_ref)?;
    let weld = numerical_weld_tolerance(tolerance, &raw);
    let clusters = cluster_events(cancel_value, weld, &raw)?;
    let data = build_provenance(cancel_value, is_self, &source_index, collision_ref, &clusters, &raw)?;
    create_output(params, data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;

    type P3 = (f64, f64, f64);

    fn with_topology(points: &[P3], vertex_points: Vec<i64>, offsets: Vec<i64>, kinds: Vec<PrimitiveKind>) -> geometry::T {
        let narrow = |v: Vec<i64>| v.into_iter().map(|p| p as u32).collect::<Vec<u32>>();
        let (vertex_points, offsets) = (narrow(vertex_points), narrow(offsets));
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let topology_value = topology::create_owned(points.len() as i64, vertex_points, offsets, kinds).unwrap();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None).unwrap()
    }

    fn mesh(points: &[P3], triangles: &[i64]) -> geometry::T {
        let count = triangles.len() / 3;
        with_topology(
            points,
            triangles.to_vec(),
            (0..=count as i64).map(|p| p * 3).collect(),
            vec![PrimitiveKind::Polygon; count],
        )
    }

    fn curve(points: &[P3], kind: PrimitiveKind) -> geometry::T {
        with_topology(points, (0..points.len() as i64).collect(), vec![0, points.len() as i64], vec![kind])
    }

    fn open_curve(points: &[P3]) -> geometry::T {
        curve(points, PrimitiveKind::OpenPolyline)
    }

    fn segment_network(count: usize, endpoints: &dyn Fn(usize) -> (P3, P3)) -> geometry::T {
        let points: Vec<P3> = (0..count * 2)
            .map(|point| {
                let (first, second) = endpoints(point / 2);
                if point & 1 == 0 { first } else { second }
            })
            .collect();
        with_topology(
            &points,
            (0..count as i64 * 2).collect(),
            (0..=count as i64).map(|p| p * 2).collect(),
            vec![PrimitiveKind::OpenPolyline; count],
        )
    }

    fn source() -> geometry::T {
        mesh(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (10., 0., 0.), (12., 0., 0.), (10., 2., 0.)], &[0, 1, 2, 3, 4, 5])
    }

    fn collision() -> geometry::T {
        mesh(
            &[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.), (0.25, 0.25, 0.), (0.5, 0.25, 0.), (0.25, 0.5, 0.)],
            &[0, 1, 2, 3, 4, 5],
        )
    }

    fn int_rows(name: &str, value: &geometry::T) -> (Vec<i64>, Vec<i64>) {
        let item = geometry::find_attribute(attribute::Owner::Point, name, value).unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&item).unwrap() {
            attribute::Storage::IntArray(rows) => {
                let view = packed::int_array::view::view(rows);
                (view.offsets.to_vec(), view.values.to_vec())
            }
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn float_rows(name: &str, value: &geometry::T) -> (Vec<i64>, Vec<f64>) {
        let item = geometry::find_attribute(attribute::Owner::Point, name, value).unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&item).unwrap() {
            attribute::Storage::FloatArray(rows) => {
                let view = packed::float_array::view::view(rows);
                (view.offsets.to_vec(), view.values.to_vec())
            }
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn positions(value: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        let view = packed::float3::view::view(geometry::positions(value)).unwrap();
        (view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
    }

    fn close(left: f64, right: f64) -> bool {
        (left - right).abs() <= 1e-12
    }

    fn equal_output(left: &geometry::T, right: &geometry::T) -> bool {
        positions(left) == positions(right)
            && ["sourceinput", "sourceprim", "sourcepoint"].iter().all(|name| int_rows(name, left) == int_rows(name, right))
            && float_rows("sourceprimuv", left) == float_rows("sourceprimuv", right)
    }

    fn run_with(params: Params, value: &geometry::T) -> geometry::T {
        run(&params, value).unwrap()
    }

    fn count(value: &geometry::T) -> i64 {
        geometry::point_count(value)
    }

    #[test]
    fn crossing_and_provenance() {
        let output = run_with(
            Params { grain: Some(1), include_coplanar: false, collision: Some(collision()), ..Params::default() },
            &source(),
        );
        assert!(
            count(&output) == 2 && geometry::vertex_count(&output) == 0 && geometry::primitive_count(&output) == 0,
            "crossing segment endpoints"
        );
        let (x, y, z) = positions(&output);
        assert!(x == [0.5, 0.5] && y == [1.5, 0.5] && z == [0., 0.], "crossing positions {x:?} {y:?} {z:?}");
        let inputs = int_rows("sourceinput", &output);
        let primitives = int_rows("sourceprim", &output);
        let uvw = float_rows("sourceprimuv", &output);
        let points = int_rows("sourcepoint", &output);
        assert!(
            inputs.0 == [0, 2, 4]
                && inputs.1 == [0, 1, 0, 1]
                && primitives.0 == inputs.0
                && primitives.1 == [0, 0, 0, 0]
                && uvw.0 == [0, 6, 12]
                && uvw.1.len() == 12
                && points.0 == inputs.0
                && points.1 == [-1, -1, -1, -1],
            "aligned provenance"
        );
        for row in 0..count(&output) as usize {
            let (first, last) = (uvw.0[row] as usize, uvw.0[row + 1] as usize);
            for slot in first / 3..last / 3 {
                let base = slot * 3;
                assert!((uvw.1[base] + uvw.1[base + 1] + uvw.1[base + 2] - 1.0).abs() < 1e-12, "barycentric row sum");
            }
        }
        let no_attributes = run_with(
            Params {
                collision: Some(collision()),
                include_coplanar: false,
                input_attribute: None,
                primitive_attribute: None,
                primitive_uvw_attribute: None,
                point_attribute: None,
                ..Params::default()
            },
            &source(),
        );
        assert!(count(&no_attributes) == 2 && geometry::attributes(&no_attributes).is_empty(), "optional provenance outputs");
    }

    #[test]
    fn coplanar_and_self() {
        let output = run_with(Params { collision: Some(collision()), ..Params::default() }, &source());
        assert!(count(&output) == 5, "coplanar overlap polygon");
        assert!(int_rows("sourcepoint", &output).1.iter().any(|&point| point >= 3), "incident-point provenance");
        let tri = [(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)];
        let duplicate = mesh(&tri, &[0, 1, 2, 2, 1, 0]);
        assert!(count(&run_with(Params::default(), &duplicate)) == 3, "duplicate-face overlap points");
        assert!(
            count(&run_with(Params { include_coplanar: false, ..Params::default() }, &duplicate)) == 0,
            "include_coplanar=false"
        );
        let adjacent = mesh(&[(0., 0., 0.), (1., 0., 0.), (1., 1., 0.), (0., 1., 0.)], &[0, 1, 2, 0, 2, 3]);
        assert!(count(&run_with(Params::default(), &adjacent)) == 0, "ordinary shared edge");
        let crossing = run_with(
            Params::default(),
            &mesh(
                &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)],
                &[0, 1, 2, 3, 4, 5],
            ),
        );
        assert!(
            count(&crossing) == 2
                && int_rows("sourceinput", &crossing).1 == [0, 0, 0, 0]
                && int_rows("sourceprim", &crossing).1 == [0, 1, 0, 1],
            "AxA provenance"
        );
    }

    #[test]
    fn curve_intersections() {
        let horizontal = open_curve(&[(-1., 0., 0.), (1., 0., 0.)]);
        let vertical = open_curve(&[(0., -1., 0.), (0., 1., 0.)]);
        let crossing = run_with(Params { collision: Some(vertical.clone()), ..Params::default() }, &horizontal);
        assert!(count(&crossing) == 1, "curve/curve crossing cardinality");
        let (x, y, z) = positions(&crossing);
        let inputs = int_rows("sourceinput", &crossing);
        let primitives = int_rows("sourceprim", &crossing);
        let uvw = float_rows("sourceprimuv", &crossing);
        let points = int_rows("sourcepoint", &crossing);
        assert!(
            x == [0.] && y == [0.] && z == [0.]
                && inputs.0 == [0, 2]
                && inputs.1 == [0, 1]
                && primitives.1 == [0, 0]
                && uvw.0 == [0, 6]
                && uvw.1 == [0.5, 0., 0., 0.5, 0., 0.]
                && points.1 == [-1, -1],
            "curve/curve aligned provenance"
        );
        let joint = open_curve(&[(-1., 0., 0.), (0., 0., 0.), (1., 0., 0.)]);
        let joint_hit = run_with(Params { collision: Some(vertical.clone()), ..Params::default() }, &joint);
        assert!(
            count(&joint_hit) == 1
                && int_rows("sourceinput", &joint_hit).1 == [0, 1]
                && int_rows("sourceprim", &joint_hit).1 == [0, 0]
                && float_rows("sourceprimuv", &joint_hit).1 == [0.5, 0., 0., 0.5, 0., 0.]
                && int_rows("sourcepoint", &joint_hit).1 == [1, -1],
            "duplicated an internal curve-vertex incidence"
        );
        let overlap = run_with(
            Params { collision: Some(open_curve(&[(0., 0., 0.), (2., 0., 0.)])), ..Params::default() },
            &horizontal,
        );
        let (ox, oy, oz) = positions(&overlap);
        let opoints = int_rows("sourcepoint", &overlap);
        assert!(
            count(&overlap) == 2
                && close(ox[0], 0.)
                && close(ox[1], 1.)
                && oy.iter().all(|&v| close(v, 0.))
                && oz.iter().all(|&v| close(v, 0.))
                && opoints.1 == [-1, 0, 1, -1],
            "collinear curve overlap endpoints: count={} x={ox:?} points={:?}",
            count(&overlap),
            opoints.1
        );
        let bow = open_curve(&[(-1., -1., 0.), (1., 1., 0.), (-1., 1., 0.), (1., -1., 0.)]);
        let own = run_with(Params::default(), &bow);
        let (sx, sy, sz) = positions(&own);
        let sprim = int_rows("sourceprim", &own);
        let sinput = int_rows("sourceinput", &own);
        let suv = float_rows("sourceprimuv", &own).1;
        assert!(
            count(&own) == 1
                && sx == [0.] && sy == [0.] && sz == [0.]
                && sinput.1 == [0, 0]
                && sprim.1 == [0, 0]
                && suv.len() == 6
                && close(suv[0], 1. / 6.)
                && suv[1] == 0.
                && suv[2] == 0.
                && close(suv[3], 5. / 6.)
                && suv[4] == 0.
                && suv[5] == 0.,
            "same-primitive curve self-crossing provenance: {suv:?}"
        );
        let square = curve(&[(0., 0., 0.), (1., 0., 0.), (1., 1., 0.), (0., 1., 0.)], PrimitiveKind::ClosedPolyline);
        assert!(count(&run_with(Params::default(), &square)) == 0, "closed-curve adjacency contacts");
    }

    #[test]
    fn curve_triangle_intersections() {
        let triangle = mesh(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)], &[0, 1, 2]);
        let piercing = open_curve(&[(0.25, 0.25, -1.), (0.25, 0.25, 1.)]);
        let hit = run_with(Params { collision: Some(triangle.clone()), ..Params::default() }, &piercing);
        let (x, y, z) = positions(&hit);
        let uvw = float_rows("sourceprimuv", &hit).1;
        assert!(
            count(&hit) == 1 && x == [0.25] && y == [0.25] && z == [0.] && uvw == [0.5, 0., 0., 0.5, 0.25, 0.25],
            "segment/triangle crossing provenance"
        );
        let reverse = run_with(Params { collision: Some(piercing.clone()), ..Params::default() }, &triangle);
        assert!(
            count(&reverse) == 1 && float_rows("sourceprimuv", &reverse).1 == [0.5, 0.25, 0.25, 0.5, 0., 0.],
            "triangle/segment provenance ordering"
        );
        let coplanar = open_curve(&[(-1., 0.25, 0.), (1., 0.25, 0.)]);
        let clipped = run_with(Params { collision: Some(triangle.clone()), ..Params::default() }, &coplanar);
        let (cx, cy, cz) = positions(&clipped);
        assert!(
            count(&clipped) == 2 && cx == [0., 0.75] && cy == [0.25, 0.25] && cz == [0., 0.],
            "coplanar segment/triangle clipping"
        );
        assert!(
            count(&run_with(
                Params { include_coplanar: false, collision: Some(triangle.clone()), ..Params::default() },
                &coplanar
            )) == 0,
            "ignored include_coplanar for segment/triangle"
        );
        let mixed = with_topology(
            &[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.), (0.25, 0.25, -1.), (0.25, 0.25, 1.)],
            vec![0, 1, 2, 3, 4],
            vec![0, 3, 5],
            vec![PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline],
        );
        let mixed_hit = run_with(Params::default(), &mixed);
        assert!(
            count(&mixed_hit) == 1
                && int_rows("sourceinput", &mixed_hit).1 == [0, 0]
                && int_rows("sourceprim", &mixed_hit).1 == [0, 1]
                && float_rows("sourceprimuv", &mixed_hit).1 == [0.5, 0.25, 0.25, 0.5, 0., 0.],
            "mixed-geometry self analysis"
        );
    }

    fn grid(columns: i64, rows: i64, size: f64) -> (Vec<P3>, Vec<i64>) {
        let mut points = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                points.push(((column as f64 / columns as f64 - 0.5) * size, (row as f64 / rows as f64 - 0.5) * size, 0.0));
            }
        }
        let stride = columns + 1;
        let mut triangles = Vec::new();
        for row in 0..rows {
            for column in 0..columns {
                let a = row * stride + column;
                let (b, c, d) = (a + 1, a + stride, a + stride + 1);
                if (row + column) % 2 == 0 {
                    triangles.extend_from_slice(&[a, b, d, a, d, c]);
                } else {
                    triangles.extend_from_slice(&[a, b, c, b, d, c]);
                }
            }
        }
        (points, triangles)
    }

    use parallel::with_domains as at_domains;

    fn parallel_exact_fixture(source_fixture: bool) {
        let (mut points, mut triangles) = if source_fixture { grid(79, 59, 20.0) } else { grid(80, 60, 20.0) };
        if source_fixture {
            for point in &mut points {
                *point = (point.0, 0., point.1);
            }
            for triangle in triangles.chunks_exact_mut(3) {
                triangle.swap(1, 2);
            }
        }
        let source = mesh(&points, &triangles);
        let angle = std::f64::consts::PI / 2.0;
        let rotated: Vec<P3> =
            points.iter().map(|&(x, y, z)| (x, y * angle.cos() - z * angle.sin(), y * angle.sin() + z * angle.cos())).collect();
        let collision = mesh(&rotated, &triangles);
        let run_surface = |domains: i64| {
            at_domains(domains, &|| {
                run_with(
                    Params { grain: Some(31), include_coplanar: false, collision: Some(collision.clone()), ..Params::default() },
                    &source,
                )
            })
        };
        let one = run_surface(1);
        let four = run_surface(4);
        assert!(count(&one) > 0, "scale fixture produced no points");
        assert!(equal_output(&one, &four), "differs across one and four domains");
        let (horizontal_count, vertical_count) = (96_usize, 72_usize);
        let horizontal = segment_network(horizontal_count, &|line| {
            let y = -1. + 2. * line as f64 / (horizontal_count - 1) as f64;
            ((-1., y, 0.), (1., y, 0.))
        });
        let vertical = segment_network(vertical_count, &|line| {
            let x = -1. + 2. * line as f64 / (vertical_count - 1) as f64;
            ((x, -1., 0.), (x, 1., 0.))
        });
        let run_curves = |domains: i64| {
            at_domains(domains, &|| {
                run_with(Params { grain: Some(17), collision: Some(vertical.clone()), ..Params::default() }, &horizontal)
            })
        };
        let one = run_curves(1);
        let four = run_curves(4);
        assert!(count(&one) == (horizontal_count * vertical_count) as i64, "curve scale fixture cardinality");
        assert!(equal_output(&one, &four), "curve output differs across one and four domains");
    }

    #[test]
    fn parallel_exact() {
        parallel_exact_fixture(false);
        parallel_exact_fixture(true);
    }
}
