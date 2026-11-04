//! Deterministic convex hull construction over packed point positions.
//!
//! Exact predicates own every affine, visibility, and horizon decision. Floating-point face
//! distance is used only to choose a legal pivot from an already exactly classified outside set.
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{self, AtomicI64};
use std::sync::{Arc, Mutex};

use rdk_core::{
    attribute, cancel, element_selection, error, geometry, group, packed, support, topology,
    topology_remap,
};
use rdk_geom::predicates::{self, Sign};

const OPERATION: &str = "Rdk_mesh.Convex_hull.convex_hull";
const GUARD_OPERATION: &str = "convex_hull";


#[derive(Default)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<element_selection::T>,
    pub preserve_point_payload: bool,
    pub source_point_attribute: Option<String>,
    pub hull_group: Option<String>,
}

fn make(code: &str, message: String) -> error::T {
    error::make(GUARD_OPERATION.to_owned(), code.to_owned(), message, None).unwrap_or_else(|bad| bad)
}

/// OCaml `invalid_arg`/`Fail`: reported as `invalid_geometry` under this operation.
fn fail(message: String) -> error::T {
    make("invalid_geometry", message)
}

/// An error that was a plain string in OCaml: re-coded under this operation.
fn lift(value: error::T) -> error::T {
    if error::code(&value) == "cancelled" {
        make("cancelled", "geometry operation was cancelled".to_owned())
    } else {
        fail(error::message(&value).to_owned())
    }
}

fn norm(value: error::T) -> error::T {
    if error::code(&value) == "cancelled" {
        make("cancelled", "geometry operation was cancelled".to_owned())
    } else {
        value
    }
}

fn fcmp(a: f64, b: f64) -> Ordering {
    match a.partial_cmp(&b) {
        Some(order) => order,
        None => match (a.is_nan(), b.is_nan()) {
            (true, true) => Ordering::Equal,
            (true, false) => Ordering::Less,
            _ => Ordering::Greater,
        },
    }
}

struct Positions {
    x: Arc<[f64]>,
    y: Arc<[f64]>,
    z: Arc<[f64]>,
}

fn positions_of(geometry_value: &geometry::T) -> Result<Positions, error::T> {
    let view = packed::Float3::View::view(geometry::positions(geometry_value))?;
    Ok(Positions { x: view.x, y: view.y, z: view.z })
}

fn wrap63(value: i64) -> i64 {
    (value << 1) >> 1
}

fn lsr(value: i64, amount: u32) -> i64 {
    wrap63((((value as u64) & ((1_u64 << 63) - 1)) >> amount) as i64)
}

fn float_hash(value: f64) -> i64 {
    if value == 0.0 {
        0
    } else {
        let bits = wrap63(value.to_bits() as i64);
        if value < 0.0 { bits ^ (-(1_i64 << 62)) } else { bits }
    }
}

fn mix(hash: i64) -> i64 {
    wrap63((hash ^ lsr(hash, 16)).wrapping_mul(0x45d9f3b))
}

fn hash_position(positions: &Positions, point: usize, mask: i64) -> i64 {
    let hash = 0x9e3779b9_i64 ^ float_hash(positions.x[point]);
    let hash = mix(hash) ^ float_hash(positions.y[point]);
    let hash = mix(hash) ^ float_hash(positions.z[point]);
    mix(hash) & mask
}

fn next_table_size(count: usize) -> Result<usize, error::T> {
    if count > (i64::MAX as usize) / 2 {
        return Err(fail(format!("{OPERATION}: selected point set exceeds hash-table limits")));
    }
    let wanted = 16.max(count * 2);
    let mut size = 16;
    while size < wanted {
        size *= 2;
    }
    Ok(size)
}

fn selected_unique_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    let point_count = geometry::point_count(geometry_value);
    let positions = positions_of(geometry_value)?;
    let selected = |point: i64| match selection {
        None => true,
        Some(group_value) => group::mem(point, group_value),
    };
    let mut selected_count = 0_usize;
    for point in 0..point_count {
        cancel::poll(cancel_value, point, 4095)?;
        if selected(point) {
            selected_count += 1;
        }
    }
    if selected_count == 0 {
        return Err(fail(format!("{OPERATION}: selection contains no points")));
    }
    let first_invalid = AtomicI64::new(i64::MAX);
    let validate = |point: i64| -> Result<(), error::T> {
        let index = point as usize;
        if selected(point)
            && !(positions.x[index].is_finite()
                && positions.y[index].is_finite()
                && positions.z[index].is_finite())
        {
            first_invalid.fetch_min(point, atomic::Ordering::Relaxed);
        }
        Ok(())
    };
    support::parallel_for(cancel_value, grain, point_count, &validate)?;
    let invalid = first_invalid.load(atomic::Ordering::Relaxed);
    if invalid != i64::MAX {
        return Err(fail(format!("{OPERATION}: selected point {invalid} has a non-finite position")));
    }
    let table_size = next_table_size(selected_count)?;
    let mut table = vec![0_i64; table_size];
    let mask = table_size as i64 - 1;
    let mut unique = Vec::with_capacity(selected_count);
    for point in 0..point_count {
        cancel::poll(cancel_value, point, 4095)?;
        if selected(point) {
            let index = point as usize;
            let mut slot = hash_position(&positions, index, mask);
            let mut found = false;
            while table[slot as usize] != 0 && !found {
                let previous = (table[slot as usize] - 1) as usize;
                if positions.x[previous] == positions.x[index]
                    && positions.y[previous] == positions.y[index]
                    && positions.z[previous] == positions.z[index]
                {
                    found = true;
                } else {
                    slot = (slot + 1) & mask;
                }
            }
            if !found {
                table[slot as usize] = point + 1;
                unique.push(point);
            }
        }
    }
    Ok(unique)
}

fn sign_nonzero(sign: Sign) -> bool {
    sign != Sign::Zero
}

fn non_collinear(p: &Positions, a: i64, b: i64, c: i64) -> bool {
    sign_nonzero(predicates::orient2d_packed(&p.x, &p.y, a, b, c))
        || sign_nonzero(predicates::orient2d_packed(&p.x, &p.z, a, b, c))
        || sign_nonzero(predicates::orient2d_packed(&p.y, &p.z, a, b, c))
}

#[derive(Clone, Copy, PartialEq)]
enum Projection {
    XY,
    XZ,
    YZ,
}

fn projection(p: &Positions, a: i64, b: i64, c: i64) -> Projection {
    let determinant = |u: &[f64], v: &[f64]| -> f64 {
        let (a, b, c) = (a as usize, b as usize, c as usize);
        let au = u[a];
        let av = v[a];
        let ab_u = u[b] - au;
        let ab_v = v[b] - av;
        let ac_u = u[c] - au;
        let ac_v = v[c] - av;
        let scale = ab_u.abs().max(ab_v.abs().max(ac_u.abs().max(ac_v.abs())));
        if scale == 0.0 || !scale.is_finite() {
            0.0
        } else {
            ((ab_u / scale * (ac_v / scale)) - (ab_v / scale * (ac_u / scale))).abs()
        }
    };
    let candidates = [
        (Projection::XY, determinant(&p.x, &p.y)),
        (Projection::XZ, determinant(&p.x, &p.z)),
        (Projection::YZ, determinant(&p.y, &p.z)),
    ];
    let mut best = 0;
    for index in 1..3 {
        if candidates[index].1 > candidates[best].1 {
            best = index;
        }
    }
    candidates[best].0
}

fn planar_hull(p: &Positions, unique: &[i64], a: i64, b: i64, c: i64) -> Vec<i64> {
    let (u, v) = match projection(p, a, b, c) {
        Projection::XY => (&p.x, &p.y),
        Projection::XZ => (&p.x, &p.z),
        Projection::YZ => (&p.y, &p.z),
    };
    let mut points = unique.to_vec();
    points.sort_by(|&left, &right| {
        fcmp(u[left as usize], u[right as usize])
            .then_with(|| fcmp(v[left as usize], v[right as usize]))
            .then_with(|| left.cmp(&right))
    });
    let build = |source: &[i64]| -> (Vec<i64>, usize) {
        let mut result = vec![0_i64; source.len()];
        let mut length = 0_usize;
        for &point in source {
            while length >= 2
                && predicates::orient2d_packed(u, v, result[length - 2], result[length - 1], point)
                    != Sign::Positive
            {
                length -= 1;
            }
            result[length] = point;
            length += 1;
        }
        (result, length)
    };
    let (lower, lower_length) = build(&points);
    let reversed: Vec<i64> = points.iter().rev().copied().collect();
    let (upper, upper_length) = build(&reversed);
    let mut hull = Vec::with_capacity(lower_length + upper_length - 2);
    hull.extend_from_slice(&lower[..lower_length - 1]);
    hull.extend_from_slice(&upper[..upper_length - 1]);
    hull
}

fn lexicographic(p: &Positions, left: i64, right: i64) -> Ordering {
    let (l, r) = (left as usize, right as usize);
    fcmp(p.x[l], p.x[r])
        .then_with(|| fcmp(p.y[l], p.y[r]))
        .then_with(|| fcmp(p.z[l], p.z[r]))
        .then_with(|| left.cmp(&right))
}

fn line_endpoints(p: &Positions, unique: &[i64]) -> Vec<i64> {
    let mut minimum = unique[0];
    let mut maximum = unique[0];
    for &point in unique {
        if lexicographic(p, point, minimum) == Ordering::Less {
            minimum = point;
        }
        if lexicographic(p, point, maximum) == Ordering::Greater {
            maximum = point;
        }
    }
    vec![minimum, maximum]
}

struct Faces {
    face_a: Vec<i64>,
    face_b: Vec<i64>,
    face_c: Vec<i64>,
    neighbor: [Vec<i64>; 3],
    alive: Vec<bool>,
    mark: Vec<i64>,
    outside: Vec<Vec<i64>>,
}

impl Faces {
    fn new() -> Faces {
        Faces {
            face_a: Vec::new(),
            face_b: Vec::new(),
            face_c: Vec::new(),
            neighbor: [Vec::new(), Vec::new(), Vec::new()],
            alive: Vec::new(),
            mark: Vec::new(),
            outside: Vec::new(),
        }
    }

    fn face_count(&self) -> usize {
        self.face_a.len()
    }

    fn add(&mut self, a: i64, b: i64, c: i64) -> usize {
        let face = self.face_a.len();
        self.face_a.push(a);
        self.face_b.push(b);
        self.face_c.push(c);
        for slot in self.neighbor.iter_mut() {
            slot.push(-1);
        }
        self.alive.push(true);
        self.mark.push(0);
        self.outside.push(Vec::new());
        face
    }

    fn edge(&self, face: usize, edge: usize) -> (i64, i64) {
        match edge {
            0 => (self.face_a[face], self.face_b[face]),
            1 => (self.face_b[face], self.face_c[face]),
            _ => (self.face_c[face], self.face_a[face]),
        }
    }

    fn neighbor_slot(&self, face: usize, neighbor: i64) -> Result<usize, error::T> {
        for slot in 0..3 {
            if self.neighbor[slot][face] == neighbor {
                return Ok(slot);
            }
        }
        Err(fail(format!("{OPERATION}: asymmetric hull adjacency")))
    }
}

fn visible(p: &Positions, faces: &Faces, face: usize, point: i64) -> bool {
    predicates::orient3d_packed(
        &p.x,
        &p.y,
        &p.z,
        faces.face_a[face],
        faces.face_b[face],
        faces.face_c[face],
        point,
    ) == Sign::Negative
}

fn face_measure(p: &Positions, faces: &Faces, face: usize, point: i64) -> f64 {
    let (a, b, c) = (faces.face_a[face] as usize, faces.face_b[face] as usize, faces.face_c[face] as usize);
    let q = point as usize;
    let ax = p.x[a] - p.x[q];
    let ay = p.y[a] - p.y[q];
    let az = p.z[a] - p.z[q];
    let bx = p.x[b] - p.x[q];
    let by = p.y[b] - p.y[q];
    let bz = p.z[b] - p.z[q];
    let cx = p.x[c] - p.x[q];
    let cy = p.y[c] - p.y[q];
    let cz = p.z[c] - p.z[q];
    let scale_ab = ax.abs().max(ay.abs()).max(az.abs().max(bx.abs()));
    let scale_bc = by.abs().max(bz.abs()).max(cx.abs().max(cy.abs()));
    let scale = scale_ab.max(scale_bc).max(cz.abs());
    if scale == 0.0 || !scale.is_finite() {
        return 0.0;
    }
    let (ax, ay, az) = (ax / scale, ay / scale, az / scale);
    let (bx, by, bz) = (bx / scale, by / scale, bz / scale);
    let (cx, cy, cz) = (cx / scale, cy / scale, cz / scale);
    ((ax * ((by * cz) - (bz * cy))) - (ay * ((bx * cz) - (bz * cx))) + (az * ((bx * cy) - (by * cx))))
        .abs()
}

fn connect_initial(faces: &mut Faces) -> Result<(), error::T> {
    for left in 0..4 {
        for left_edge in 0..3 {
            if faces.neighbor[left_edge][left] < 0 {
                let (a, b) = faces.edge(left, left_edge);
                let mut found = false;
                for right in (left + 1)..4 {
                    for right_edge in 0..3 {
                        let (c, d) = faces.edge(right, right_edge);
                        if a == d && b == c {
                            faces.neighbor[left_edge][left] = right as i64;
                            faces.neighbor[right_edge][right] = left as i64;
                            found = true;
                        }
                    }
                }
                if !found && faces.neighbor[left_edge][left] < 0 {
                    return Err(fail(format!("{OPERATION}: initial simplex adjacency is incomplete")));
                }
            }
        }
    }
    Ok(())
}

fn assign_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    p: &Positions,
    faces: &mut Faces,
    face_ids: &[usize],
    points: &[i64],
    queue: &mut VecDeque<usize>,
) -> Result<(), error::T> {
    let point_count = points.len();
    let assignments: Vec<AtomicI64> = (0..point_count).map(|_| AtomicI64::new(-1)).collect();
    {
        let faces_ref: &Faces = faces;
        let assign = |index: i64| -> Result<(), error::T> {
            let point = points[index as usize];
            for &face in face_ids {
                if visible(p, faces_ref, face, point) {
                    assignments[index as usize].store(face as i64, atomic::Ordering::Relaxed);
                    break;
                }
            }
            Ok(())
        };
        support::parallel_for(cancel_value, grain, point_count as i64, &assign)?;
    }
    for index in 0..point_count {
        let face = assignments[index].load(atomic::Ordering::Relaxed);
        if face >= 0 {
            faces.outside[face as usize].push(points[index]);
        }
    }
    for &face in face_ids {
        if !faces.outside[face].is_empty() {
            queue.push_back(face);
        }
    }
    Ok(())
}

fn oriented_face(p: &Positions, faces: &mut Faces, a: i64, b: i64, c: i64, opposite: i64) -> Result<usize, error::T> {
    match predicates::orient3d_packed(&p.x, &p.y, &p.z, a, b, c, opposite) {
        Sign::Positive => Ok(faces.add(a, b, c)),
        Sign::Negative => Ok(faces.add(b, a, c)),
        Sign::Zero => Err(fail(format!("{OPERATION}: degenerate initial simplex"))),
    }
}

fn farthest_outside(p: &Positions, faces: &Faces, face: usize) -> Result<i64, error::T> {
    let outside = &faces.outside[face];
    if outside.is_empty() {
        return Err(fail(format!("{OPERATION}: empty outside set")));
    }
    let mut chosen = outside[0];
    let mut measure = face_measure(p, faces, face, outside[0]);
    for &point in &outside[1..] {
        let candidate = face_measure(p, faces, face, point);
        if candidate > measure || (candidate == measure && point < chosen) {
            chosen = point;
            measure = candidate;
        }
    }
    Ok(chosen)
}

#[allow(clippy::too_many_arguments)]
fn expand_hull(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    p: &Positions,
    faces: &mut Faces,
    queue: &mut VecDeque<usize>,
    radial: &mut HashMap<(i64, i64), (usize, usize)>,
    generation: i64,
    seed_face: usize,
    eye: i64,
) -> Result<(), error::T> {
    let mut visible_faces: Vec<usize> = Vec::new();
    let mut stack: Vec<usize> = vec![seed_face];
    faces.mark[seed_face] = generation;
    while let Some(face) = stack.pop() {
        cancel::check_opt(cancel_value)?;
        visible_faces.push(face);
        for edge in 0..3 {
            let neighbor = faces.neighbor[edge][face];
            if neighbor < 0 {
                return Err(fail(format!("{OPERATION}: open 3D hull adjacency")));
            }
            let neighbor = neighbor as usize;
            if faces.alive[neighbor] && faces.mark[neighbor] != generation && visible(p, faces, neighbor, eye) {
                faces.mark[neighbor] = generation;
                stack.push(neighbor);
            }
        }
    }
    let mut horizon: Vec<(i64, i64, i64, i64)> = Vec::new();
    let mut reassigned: Vec<i64> = Vec::new();
    for &face in &visible_faces {
        for edge in 0..3 {
            let neighbor = faces.neighbor[edge][face];
            if faces.mark[neighbor as usize] != generation {
                let (a, b) = faces.edge(face, edge);
                horizon.push((a, b, neighbor, face as i64));
            }
        }
        for &point in &faces.outside[face] {
            if point != eye {
                reassigned.push(point);
            }
        }
        faces.outside[face] = Vec::new();
        faces.alive[face] = false;
    }
    if horizon.len() < 3 {
        return Err(fail(format!("{OPERATION}: a hull expansion has fewer than three horizon edges")));
    }
    horizon.sort();
    let mut new_faces: Vec<usize> = Vec::with_capacity(horizon.len());
    for &(a, b, neighbor, visible_face) in &horizon {
        let face = faces.add(a, b, eye);
        faces.neighbor[0][face] = neighbor;
        let slot = faces.neighbor_slot(neighbor as usize, visible_face)?;
        faces.neighbor[slot][neighbor as usize] = face as i64;
        new_faces.push(face);
    }
    radial.clear();
    for &face in &new_faces {
        for edge in 1..3 {
            let (a, b) = faces.edge(face, edge);
            if let Some((other, other_edge)) = radial.remove(&(b, a)) {
                faces.neighbor[edge][face] = other as i64;
                faces.neighbor[other_edge][other] = face as i64;
            } else {
                radial.insert((a, b), (face, edge));
            }
        }
    }
    if !radial.is_empty() {
        return Err(fail(format!("{OPERATION}: horizon cycle did not close")));
    }
    assign_points(cancel_value, grain, p, faces, &new_faces, &reassigned, queue)
}

#[allow(clippy::too_many_arguments)]
fn three_dimensional_hull(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    p: &Positions,
    unique: &[i64],
    a: i64,
    b: i64,
    c: i64,
    d: i64,
) -> Result<(Faces, Vec<usize>), error::T> {
    let mut faces = Faces::new();
    oriented_face(p, &mut faces, a, b, c, d)?;
    oriented_face(p, &mut faces, a, d, b, c)?;
    oriented_face(p, &mut faces, a, c, d, b)?;
    oriented_face(p, &mut faces, b, d, c, a)?;
    connect_initial(&mut faces)?;
    let candidates: Vec<i64> =
        unique.iter().copied().filter(|&point| !(point == a || point == b || point == c || point == d)).collect();
    if candidates.len() + 4 != unique.len() {
        return Err(fail(format!("{OPERATION}: initial simplex point accounting failed")));
    }
    let mut queue: VecDeque<usize> = VecDeque::new();
    let mut radial: HashMap<(i64, i64), (usize, usize)> = HashMap::new();
    assign_points(cancel_value, grain, p, &mut faces, &[0, 1, 2, 3], &candidates, &mut queue)?;
    let mut generation = 0_i64;
    while let Some(face) = queue.pop_front() {
        if !faces.alive[face] || faces.outside[face].is_empty() {
            continue;
        }
        generation += 1;
        let eye = farthest_outside(p, &faces, face)?;
        expand_hull(cancel_value, grain, p, &mut faces, &mut queue, &mut radial, generation, face, eye)?;
    }
    let output: Vec<usize> = (0..faces.face_count()).filter(|&face| faces.alive[face]).collect();
    if output.len() < 4 {
        return Err(fail(format!("{OPERATION}: full-dimensional hull has fewer than four faces")));
    }
    Ok((faces, output))
}

enum Hull {
    Point(i64),
    Line(Vec<i64>),
    Plane(Vec<i64>),
    Solid(Faces, Vec<usize>),
}

fn gather(cancel_value: Option<&cancel::T>, grain: i64, point_map: &[i64], source: &[f64]) -> Result<Vec<f64>, error::T> {
    let count = point_map.len() as i64;
    let slots: Vec<Mutex<Vec<f64>>> =
        (0..((count + grain - 1) / grain).max(0)).map(|_| Mutex::new(Vec::new())).collect();
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        cancel::poll(cancel_value, first, 4095)?;
        let part: Vec<f64> = (first..last).map(|point| source[point_map[point as usize] as usize]).collect();
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = part;
        Ok(())
    })?;
    let mut output = Vec::with_capacity(point_map.len());
    for slot in slots {
        output.extend(slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner));
    }
    Ok(output)
}

#[allow(clippy::too_many_arguments)]
fn remap_output(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    preserve_point_payload: bool,
    source_point_attribute: Option<&str>,
    hull_group: Option<&str>,
    geometry_value: &geometry::T,
    point_map: &[i64],
    topology_value: topology::T,
) -> Result<geometry::T, error::T> {
    let source = positions_of(geometry_value)?;
    let positions = packed::Float3::View::of_owned_exn(
        gather(cancel_value, grain, point_map, &source.x)?,
        gather(cancel_value, grain, point_map, &source.y)?,
        gather(cancel_value, grain, point_map, &source.z)?,
    )?;
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        match attribute::owner(&item) {
            attribute::Owner::Detail => attributes.push(item),
            attribute::Owner::Point if preserve_point_payload && attribute::name(&item) != "N" => {
                attributes.push(topology_remap::attribute(cancel_value, grain, None, point_map, &item)?);
            }
            _ => {}
        }
    }
    let mut groups = Vec::new();
    if preserve_point_payload {
        for item in geometry::groups(geometry_value) {
            if group::owner(&item) == group::Owner::Point {
                groups.push(topology_remap::group(cancel_value, grain, point_map, &item)?);
            }
        }
    }
    let mut output =
        geometry::create(positions, topology_value, Some(attributes), Some(groups), None).map_err(lift)?;
    if let Some(name) = source_point_attribute {
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Int(point_map.to_vec()))?;
        output = geometry::with_attribute(item, &output)?;
    }
    if let Some(name) = hull_group {
        let item = group::init(Some(grain), group::Owner::Primitive, name, geometry::primitive_count(&output), &|_| true)?;
        output = geometry::with_group(item, &output)?;
    }
    Ok(output)
}

fn materialize(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    params: &Params,
    geometry_value: &geometry::T,
    hull: Hull,
) -> Result<geometry::T, error::T> {
    let preserve = params.preserve_point_payload;
    let source_attr = params.source_point_attribute.as_deref();
    let hull_group = params.hull_group.as_deref();
    let remap = |point_map: &[i64], topology_value: topology::T| {
        remap_output(cancel_value, grain, preserve, source_attr, hull_group, geometry_value, point_map, topology_value)
    };
    match hull {
        Hull::Point(source) => remap(&[source], topology::empty(1)?),
        Hull::Line(point_map) => {
            let topology_value = topology::create_owned(
                2,
                vec![0, 1],
                vec![0, 2],
                vec![topology::PrimitiveKind::OpenPolyline],
            )?;
            remap(&point_map, topology_value)
        }
        Hull::Plane(point_map) => {
            let count = point_map.len() as i64;
            let topology_value = topology::polygons_owned(count, (0..count).collect(), vec![0, count])?;
            remap(&point_map, topology_value)
        }
        Hull::Solid(faces, alive_faces) => {
            let source_point_count = geometry::point_count(geometry_value) as usize;
            let mut used = vec![false; source_point_count];
            for &face in &alive_faces {
                used[faces.face_a[face] as usize] = true;
                used[faces.face_b[face] as usize] = true;
                used[faces.face_c[face] as usize] = true;
            }
            let mut point_map: Vec<i64> = Vec::new();
            let mut source_to_output = vec![-1_i64; source_point_count];
            for point in 0..source_point_count {
                if used[point] {
                    source_to_output[point] = point_map.len() as i64;
                    point_map.push(point as i64);
                }
            }
            let output_points = point_map.len() as i64;
            let mut triangles: Vec<(i64, i64, i64)> = alive_faces
                .iter()
                .map(|&face| {
                    let a = source_to_output[faces.face_a[face] as usize];
                    let b = source_to_output[faces.face_b[face] as usize];
                    let c = source_to_output[faces.face_c[face] as usize];
                    if a <= b && a <= c {
                        (a, b, c)
                    } else if b <= a && b <= c {
                        (b, c, a)
                    } else {
                        (c, a, b)
                    }
                })
                .collect();
            triangles.sort();
            let primitive_count = triangles.len();
            let mut vertex_points = Vec::with_capacity(primitive_count * 3);
            let mut primitive_offsets = Vec::with_capacity(primitive_count + 1);
            for (primitive, &(a, b, c)) in triangles.iter().enumerate() {
                cancel::poll(cancel_value, primitive as i64, 4095)?;
                primitive_offsets.push(vertex_points.len() as i64);
                vertex_points.extend_from_slice(&[a, b, c]);
            }
            primitive_offsets.push((primitive_count * 3) as i64);
            let topology_value = topology::View::create_validated_owned(
                output_points,
                vertex_points,
                primitive_offsets,
                vec![0_u8; primitive_count],
            );
            remap(&point_map, topology_value)
        }
    }
}

/// Build the exact combinatorial convex hull of the selected input points. One unique point
/// produces a free point, collinear inputs produce one open endpoint segment, coplanar inputs
/// produce one closed convex polygon, and a full-dimensional input produces an outward closed
/// triangle surface.
pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run_inner(params, geometry_value).map_err(norm)
}

fn run_inner(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(fail(format!("{OPERATION}: grain must be positive")));
    }
    if let Some(name) = &params.source_point_attribute {
        if name.trim().is_empty() || name == "P" {
            return Err(fail(format!("{OPERATION}: source point attribute name must be non-empty and not P")));
        }
    }
    if let Some(name) = &params.hull_group {
        if name.trim().is_empty() {
            return Err(fail(format!("{OPERATION}: hull group name must be non-empty")));
        }
    }
    let topology_value = geometry::topology(geometry_value);
    element_selection::validate(OPERATION, topology_value, params.selection.as_ref()).map_err(lift)?;
    let selected = match &params.selection {
        None => None,
        Some(selection) => Some(
            element_selection::promote(cancel_value, grain, None, group::Owner::Point, selection, topology_value)
                .map_err(lift)?,
        ),
    };
    let unique = selected_unique_points(cancel_value, grain, selected.as_ref(), geometry_value)?;
    let p = positions_of(geometry_value)?;
    let result = if unique.len() == 1 {
        Hull::Point(unique[0])
    } else {
        let a = unique[0];
        let b = unique[1];
        let mut c = -1_i64;
        let mut at = 2;
        while c < 0 && at < unique.len() {
            if non_collinear(&p, a, b, unique[at]) {
                c = unique[at];
            }
            at += 1;
        }
        if c < 0 {
            Hull::Line(line_endpoints(&p, &unique))
        } else {
            let mut d = -1_i64;
            let mut at = 0;
            while d < 0 && at < unique.len() {
                let point = unique[at];
                if point != a
                    && point != b
                    && point != c
                    && predicates::orient3d_packed(&p.x, &p.y, &p.z, a, b, c, point) != Sign::Zero
                {
                    d = point;
                }
                at += 1;
            }
            if d < 0 {
                Hull::Plane(planar_hull(&p, &unique, a, b, c))
            } else {
                let (faces, alive) = three_dimensional_hull(cancel_value, grain, &p, &unique, a, b, c, d)?;
                Hull::Solid(faces, alive)
            }
        }
    };
    materialize(cancel_value, grain, params, geometry_value, result)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn point_cloud(values: &[(f64, f64, f64)]) -> geometry::T {
        let x = values.iter().map(|v| v.0).collect();
        let y = values.iter().map(|v| v.1).collect();
        let z = values.iter().map(|v| v.2).collect();
        geometry::create(
            packed::Float3::View::of_owned_exn(x, y, z).unwrap(),
            topology::empty(values.len() as i64).unwrap(),
            None,
            None,
            None,
        ).unwrap()
    }

    fn expect_error(value: Result<geometry::T, error::T>, code: &str) {
        match value {
            Err(failure) => assert_eq!(error::code(&failure), code, "{}", error::to_string(&failure)),
            Ok(_) => panic!("expected {code} error"),
        }
    }

    fn same_storage(left: &attribute::T, right: &attribute::T) -> bool {
        match (attribute::View::storage(left).unwrap(), attribute::View::storage(right).unwrap()) {
            (attribute::Storage::Float(a), attribute::Storage::Float(b)) => a == b,
            (attribute::Storage::Int(a), attribute::Storage::Int(b)) => a == b,
            (attribute::Storage::Text(a), attribute::Storage::Text(b)) => a == b,
            (attribute::Storage::Float3(a), attribute::Storage::Float3(b)) => {
                let a = packed::Float3::View::view(a).unwrap();
                let b = packed::Float3::View::view(b).unwrap();
                a.x == b.x && a.y == b.y && a.z == b.z
            }
            _ => false,
        }
    }

    fn same_group(a: &group::T, b: &group::T) -> bool {
        group::name(a) == group::name(b)
            && group::owner(a) == group::owner(b)
            && group::ordered_elements(a) == group::ordered_elements(b)
            && (0..group::length(a)).all(|element| group::mem(element, a) == group::mem(element, b))
    }

    fn same_geometry(left: &geometry::T, right: &geometry::T) -> bool {
        let lp = positions_of(left).unwrap();
        let rp = positions_of(right).unwrap();
        let lt = topology::View::view(geometry::topology(left)).unwrap();
        let rt = topology::View::view(geometry::topology(right)).unwrap();
        let (la, ra) = (geometry::attributes(left), geometry::attributes(right));
        let (lg, rg) = (geometry::groups(left), geometry::groups(right));
        lp.x == rp.x
            && lp.y == rp.y
            && lp.z == rp.z
            && lt.vertex_points == rt.vertex_points
            && lt.primitive_offsets == rt.primitive_offsets
            && lt.primitive_kinds == rt.primitive_kinds
            && la.len() == ra.len()
            && la.iter().zip(&ra).all(|(a, b)| {
                attribute::name(a) == attribute::name(b)
                    && attribute::owner(a) == attribute::owner(b)
                    && same_storage(a, b)
            })
            && lg.len() == rg.len()
            && lg.iter().zip(&rg).all(|(a, b)| same_group(a, b))
    }

    /// (boundary edges, non-manifold edges, undirected edge count)
    fn edge_stats(geometry_value: &geometry::T) -> (usize, usize, usize) {
        let view = topology::View::view(geometry::topology(geometry_value)).unwrap();
        let mut counts: HashMap<(i64, i64), usize> = HashMap::new();
        for primitive in 0..view.primitive_kinds.len() {
            let first = view.primitive_offsets[primitive] as usize;
            let last = view.primitive_offsets[primitive + 1] as usize;
            for at in first..last {
                let a = view.vertex_points[at];
                let b = view.vertex_points[if at + 1 == last { first } else { at + 1 }];
                *counts.entry((a.min(b), a.max(b))).or_insert(0) += 1;
            }
        }
        let boundary = counts.values().filter(|&&n| n == 1).count();
        let non_manifold = counts.values().filter(|&&n| n > 2).count();
        (boundary, non_manifold, counts.len())
    }

    fn check_closed(geometry_value: &geometry::T, message: &str) {
        let (boundary, non_manifold, _) = edge_stats(geometry_value);
        assert!(boundary == 0 && non_manifold == 0, "{message}");
    }

    fn check_contains(source: &geometry::T, hull: &geometry::T, message: &str) {
        let sp = positions_of(source).unwrap();
        let hp = positions_of(hull).unwrap();
        let topology_value = topology::View::view(geometry::topology(hull)).unwrap();
        for primitive in 0..geometry::primitive_count(hull) as usize {
            let first = topology_value.primitive_offsets[primitive] as usize;
            assert_eq!(topology_value.primitive_offsets[primitive + 1] as usize - first, 3, "{message}: non-triangle face");
            let a = topology_value.vertex_points[first] as usize;
            let b = topology_value.vertex_points[first + 1] as usize;
            let c = topology_value.vertex_points[first + 2] as usize;
            for point in 0..geometry::point_count(source) as usize {
                let sign = predicates::orient3d(
                    hp.x[a], hp.y[a], hp.z[a], hp.x[b], hp.y[b], hp.z[b], hp.x[c], hp.y[c], hp.z[c],
                    sp.x[point], sp.y[point], sp.z[point],
                );
                assert!(sign != Sign::Negative, "{message}: point {point} is outside face {primitive}");
            }
        }
    }

    fn run_with(params: Params, source: &geometry::T) -> Result<geometry::T, error::T> {
        run(&params, source)
    }

    #[test]
    fn affine_dimensions() {
        let point = run_with(
            Params { source_point_attribute: Some("source".to_owned()), ..Params::default() },
            &point_cloud(&[(2., 3., 4.), (2., 3., 4.), (2., 3., 4.)]),
        ).unwrap();
        assert!(
            geometry::point_count(&point) == 1
                && geometry::vertex_count(&point) == 0
                && geometry::primitive_count(&point) == 0,
            "singleton hull"
        );
        let line = run_with(
            Params::default(),
            &point_cloud(&[(2., 0., 0.), (-1., 0., 0.), (5., 0., 0.), (0., 0., 0.), (5., 0., 0.)]),
        ).unwrap();
        let line_positions = positions_of(&line).unwrap();
        assert!(
            geometry::point_count(&line) == 2
                && geometry::vertex_count(&line) == 2
                && geometry::primitive_count(&line) == 1
                && topology::primitive_kind(geometry::topology(&line), 0).unwrap()
                    == topology::PrimitiveKind::OpenPolyline
                && line_positions.x[..] == [-1., 5.],
            "collinear endpoint hull"
        );
        let plane = run_with(
            Params::default(),
            &point_cloud(&[
                (0., 0., 0.),
                (1., 0., 0.),
                (1., 1., 0.),
                (0., 1., 0.),
                (0.5, 0.5, 0.),
                (0.5, 0., 0.),
                (1., 1., 0.),
            ]),
        ).unwrap();
        assert!(
            geometry::point_count(&plane) == 4
                && geometry::vertex_count(&plane) == 4
                && geometry::primitive_count(&plane) == 1,
            "coplanar polygon hull"
        );
        assert!(
            topology::primitive_kind(geometry::topology(&plane), 0).unwrap() == topology::PrimitiveKind::Polygon,
            "coplanar hull is not a polygon"
        );
    }

    fn decorated_cube() -> geometry::T {
        let values = [
            (-1., -1., -1.),
            (1., -1., -1.),
            (1., 1., -1.),
            (-1., 1., -1.),
            (-1., -1., 1.),
            (1., -1., 1.),
            (1., 1., 1.),
            (-1., 1., 1.),
            (0., 0., 0.),
            (0.25, 0.2, -0.1),
            (-1., -1., -1.),
        ];
        let geometry_value = point_cloud(&values);
        let points = values.len();
        let id = attribute::create_owned(
            "id",
            attribute::Owner::Point,
            attribute::Storage::Int((0..points as i64).map(|point| 100 + point).collect()),
        ).unwrap();
        let normal = attribute::create_owned(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(
                vec![1.; points],
                vec![0.; points],
                vec![0.; points],
            ).unwrap()),
        ).unwrap();
        let detail = attribute::create_owned(
            "tag",
            attribute::Owner::Detail,
            attribute::Storage::Text(vec!["cube".to_owned()]),
        ).unwrap();
        let corners = group::ordered(group::Owner::Point, "corners", points as i64, &[7, 0, 3, 4]).unwrap();
        geometry::create(
            geometry::positions(&geometry_value).clone(),
            geometry::topology(&geometry_value).clone(),
            Some(vec![id, normal, detail]),
            Some(vec![corners]),
            None,
        ).unwrap()
    }

    fn int_storage(value: Option<attribute::T>) -> Vec<i64> {
        match attribute::View::storage(&value.unwrap()).unwrap() {
            attribute::Storage::Int(values) => values.clone(),
            _ => panic!("int storage"),
        }
    }

    #[test]
    fn solid_payload_and_selection() {
        let source = decorated_cube();
        let hull = run_with(
            Params {
                grain: Some(1),
                source_point_attribute: Some("source_point".to_owned()),
                hull_group: Some("hull".to_owned()),
                preserve_point_payload: true,
                ..Params::default()
            },
            &source,
        ).unwrap();
        assert!(
            geometry::point_count(&hull) == 8
                && geometry::vertex_count(&hull) == 36
                && geometry::primitive_count(&hull) == 12,
            "cube hull cardinality"
        );
        check_closed(&hull, "cube hull is not closed two-manifold");
        check_contains(&source, &hull, "cube hull containment");
        assert!(analysis_volume(&hull) > 0.0, "cube hull winding");
        assert!(
            geometry::find_attribute(attribute::Owner::Point, "N", &hull).is_none(),
            "stale point normal survived"
        );
        assert!(
            geometry::find_attribute(attribute::Owner::Detail, "tag", &hull).is_some(),
            "detail payload was dropped"
        );
        let ids = int_storage(geometry::find_attribute(attribute::Owner::Point, "id", &hull));
        let ancestry = int_storage(geometry::find_attribute(attribute::Owner::Point, "source_point", &hull));
        assert!(
            ancestry == [0, 1, 2, 3, 4, 5, 6, 7] && ids == ancestry.iter().map(|p| 100 + p).collect::<Vec<_>>(),
            "source-point ancestry"
        );
        assert_eq!(
            group::cardinality(&geometry::find_group(group::Owner::Primitive, "hull", &hull).unwrap()),
            12,
            "hull output group"
        );
        assert_eq!(
            group::ordered_elements(&geometry::find_group(group::Owner::Point, "corners", &hull).unwrap()),
            Some(vec![7, 0, 3, 4]),
            "ordered point-group ancestry"
        );
        let selected = group::init(Some(1), group::Owner::Point, "bottom", 11, &|point| point < 4).unwrap();
        let plane = run_with(
            Params { selection: Some(element_selection::T::SelectedPoints(selected)), ..Params::default() },
            &source,
        ).unwrap();
        assert!(geometry::point_count(&plane) == 4 && geometry::primitive_count(&plane) == 1, "typed selected hull");
        let stripped = run_with(Params::default(), &source).unwrap();
        assert!(
            geometry::find_attribute(attribute::Owner::Point, "id", &stripped).is_none()
                && geometry::find_attribute(attribute::Owner::Detail, "tag", &stripped).is_some()
                && geometry::groups(&stripped).is_empty(),
            "payload suppression"
        );
    }

    fn analysis_volume(geometry_value: &geometry::T) -> f64 {
        rdk_core::analysis::signed_volume(geometry_value).unwrap()
    }

    #[test]
    fn exact_and_errors() {
        let tiny = f64::from_bits(1);
        let exact = run_with(
            Params::default(),
            &point_cloud(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.), (0., 0., tiny)]),
        ).unwrap();
        assert!(geometry::point_count(&exact) == 4 && geometry::primitive_count(&exact) == 4, "subnormal hull");
        check_closed(&exact, "subnormal hull incidence");
        let exact_source = point_cloud(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.), (0., 0., tiny)]);
        expect_error(run_with(Params::default(), &point_cloud(&[])), "invalid_geometry");
        expect_error(run_with(Params { grain: Some(0), ..Params::default() }, &exact_source), "invalid_geometry");
        expect_error(
            run_with(Params { source_point_attribute: Some("P".to_owned()), ..Params::default() }, &exact_source),
            "invalid_geometry",
        );
        let bad = point_cloud(&[(0., 0., 0.), (f64::NAN, 0., 0.)]);
        expect_error(run_with(Params::default(), &bad), "invalid_geometry");
        let wrong = group::init(Some(1), group::Owner::Point, "wrong", 3, &|_| true).unwrap();
        expect_error(
            run_with(
                Params { selection: Some(element_selection::T::SelectedPoints(wrong)), ..Params::default() },
                &exact_source,
            ),
            "invalid_geometry",
        );
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        expect_error(run_with(Params { cancel: Some(token), ..Params::default() }, &exact_source), "cancelled");
    }

    #[test]
    fn randomized_containment() {
        for case in 0..32_i64 {
            let points: Vec<(f64, f64, f64)> = (0..72_i64)
                .map(|point| {
                    let coordinate = |multiplier: i64, offset: i64| {
                        let value = ((point + case * offset) * multiplier + case * 97) % 10_007;
                        2.0 * value as f64 / 10_007.0 - 1.0
                    };
                    (coordinate(7919, 17), coordinate(6841, 29), coordinate(5503, 43))
                })
                .collect();
            let source = point_cloud(&points);
            let hull = run_with(Params { grain: Some(7), ..Params::default() }, &source).unwrap();
            check_closed(&hull, &format!("random hull {case} incidence"));
            check_contains(&source, &hull, &format!("random hull {case} containment"));
            let (_, _, edges) = edge_stats(&hull);
            assert_eq!(
                geometry::point_count(&hull) - edges as i64 + geometry::primitive_count(&hull),
                2,
                "random hull {case} Euler characteristic"
            );
        }
    }

    fn sphere_cloud(rings: i64, columns: i64) -> Vec<(f64, f64, f64)> {
        let surface = rings * columns;
        (0..surface + 20_000)
            .map(|point| {
                if point < surface {
                    let ring = point / columns;
                    let column = point % columns;
                    let theta = std::f64::consts::PI * (ring as f64 + 0.5) / rings as f64;
                    let phi = 2.0 * std::f64::consts::PI * column as f64 / columns as f64;
                    (theta.sin() * phi.cos(), theta.cos(), theta.sin() * phi.sin())
                } else {
                    let value = point - surface;
                    (
                        ((value * 17 % 997) as f64 / 997.0) - 0.5,
                        ((value * 29 % 991) as f64 / 991.0) - 0.5,
                        ((value * 43 % 983) as f64 / 983.0) - 0.5,
                    )
                }
            })
            .collect()
    }

    #[test]
    fn parallel_exact() {
        let source = point_cloud(&sphere_cloud(16, 32));
        let run_domains = |domains: i64| -> geometry::T {
            let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let params = Params {
                    grain: Some(127),
                    source_point_attribute: Some("source".to_owned()),
                    preserve_point_payload: true,
                    ..Params::default()
                };
                *slot.lock().unwrap() = Some(run(&params, &source).unwrap());
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        let one = run_domains(1);
        let four = run_domains(4);
        assert!(same_geometry(&one, &four), "one/four-domain hull mismatch");
        assert!(
            geometry::point_count(&one) == 512 && geometry::primitive_count(&one) == 1020,
            "sphere hull cardinality"
        );
        check_closed(&one, "sphere hull incidence");
    }
}
