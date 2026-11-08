//! Port of lib/flow_sop/topology_kernel_impl.ml: topology data sources over cooked geometry, the
//! mesh constructor (host and device-resident), `sop/remap` and `sop/concat`.
use flow_ir::flow_ir::Gpu::{self, Integers, Planes, Residency};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self, Value};
use rays_diagnostic::Diagnostic;
use rdk_core::geometry::{self, T as Geometry};
use rdk_core::packed::{Deferred, Float3};
use rdk_core::{analysis, attribute, cancel, edge_group, error, group, point_index, topology, topology_index};
use rdk_core::{polygon_triangulation, topology_remap as remap_core};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock, Weak};

pub type Args = [(String, Value)];
pub type Positions = Box<dyn Fn() -> Result<Float3::T, Diagnostic> + Send + Sync>;

fn diag(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}
fn fail<R>(code: &str, message: &str) -> Result<R, Diagnostic> {
    Err(diag(code, message))
}
/// A device-read failure as the error a deferred rdk holder reports.
pub(crate) fn rdk_error(d: Diagnostic) -> error::T {
    error::new("gpu.materialise", error::Code::Gpu, d.message)
}
fn rdk_message(e: &error::T) -> String {
    error::message(e).to_owned()
}
fn arg<'a>(args: &'a Args, name: &str) -> Option<&'a Value> {
    args.iter().find(|(key, _)| key == name).map(|(_, value)| value)
}
fn is_text(value: Option<&Value>, text: &str) -> bool {
    matches!(value, Some(Value::Text(bytes)) if bytes.as_slice() == text.as_bytes())
}
fn ints(values: Vec<i64>) -> Value {
    Value::Int_array(Arc::new(values))
}
/// OCaml `String.trim`: ' ', '\012', '\n', '\r', '\t'.
fn trim(bytes: &[u8]) -> String {
    let blank = |b: &u8| matches!(*b, b' ' | 0x0c | b'\n' | b'\r' | b'\t');
    let start = bytes.iter().position(|b| !blank(b)).unwrap_or(bytes.len());
    let end = bytes.iter().rposition(|b| !blank(b)).map_or(start, |i| i + 1);
    String::from_utf8_lossy(&bytes[start..end]).into_owned()
}
fn admit(bytes: i64) -> Result<(), Diagnostic> {
    mem::admit_bytes(mem::Pool::Host, bytes).map_err(|r| value::fail("E_ARRAY_MEMORY", &r.to_string()))
}
fn index_of(topology: &topology::T) -> Result<topology_index::T, Diagnostic> {
    topology_index::create(None, topology).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
}
fn points_of(topology: &topology::T) -> Result<point_index::T, Diagnostic> {
    point_index::create(None, topology).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
}
fn topo_view(topology: &topology::T) -> Result<topology::View::View, Diagnostic> {
    topology::View::view(topology).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
}

// A topology's component arrays, remembered by the topology's identity (immutable): a recook over
// the same topology reads the same arrays. ponytail: 16 topologies, cleared when full.
type Kept = Arc<Mutex<HashMap<String, Value>>>;
static KEPT: Mutex<Vec<(i64, Kept)>> = Mutex::new(Vec::new());

fn kept_table(geometry: &Geometry) -> Kept {
    let id = topology::data_id(geometry::topology(geometry));
    let mut all = KEPT.lock().unwrap();
    if let Some((_, table)) = all.iter().find(|(key, _)| *key == id) {
        return table.clone();
    }
    if all.len() >= 16 {
        all.remove(0);
    }
    let table: Kept = Arc::new(Mutex::new(HashMap::new()));
    all.push((id, table.clone()));
    table
}

fn kept(
    geometry: &Geometry,
    kind: &str,
    component: &str,
    make: &dyn Fn(&Geometry, &str) -> Result<Value, Diagnostic>,
) -> Result<Value, Diagnostic> {
    let table = kept_table(geometry);
    let key = format!("{kind}{component}");
    if let Some(value) = table.lock().unwrap().get(&key) {
        return Ok(value.clone());
    }
    let value = make(geometry, component)?;
    table.lock().unwrap().insert(key, value.clone());
    Ok(value)
}

pub fn topology_component(geometry: &Geometry, component: &str) -> Result<Value, Diagnostic> {
    let view = topo_view(geometry::topology(geometry))?;
    match component {
        "vertex_points" => Ok(Value::Int_array(view.vertex_points)),
        "primitive_offsets" => Ok(Value::Int_array(view.primitive_offsets)),
        "kinds" => Ok(ints(view.primitive_kinds.iter().map(|k| *k as i64).collect())),
        _ => fail("E_DATA_SOURCE", "Topology source requires vertex_points, primitive_offsets or kinds."),
    }
}

/// The point incidence (CSR of vertices by point) is the Point_index's, without the edges a
/// Topology_index hashes.
pub fn adjacency(geometry: &Geometry, component: &str) -> Result<Value, Diagnostic> {
    let topology = geometry::topology(geometry);
    let array = |a: &[i64]| Ok(ints(a.to_vec()));
    match component {
        "primitive_of_vertex" => array(&point_index::View::view(&points_of(topology)?).primitive_of_vertex),
        "point_vertex_offsets" => array(&point_index::View::view(&points_of(topology)?).point_offsets),
        "point_vertices" => array(&point_index::View::view(&points_of(topology)?).point_vertices),
        "next_vertex" | "previous_vertex" | "edge_of_vertex" | "opposite_vertex" | "edge_a" | "edge_b"
        | "point_edge_offsets" | "point_edges" => {
            let view = topology_index::View::view(&index_of(topology)?);
            match component {
                "next_vertex" => array(&view.next_vertex),
                "previous_vertex" => array(&view.previous_vertex),
                "edge_of_vertex" => array(&view.edge_of_vertex),
                "opposite_vertex" => array(&view.opposite_vertex),
                "edge_a" => array(&view.edge_a),
                "edge_b" => array(&view.edge_b),
                "point_edge_offsets" => array(&view.point_edge_offsets),
                _ => array(&view.point_edges),
            }
        }
        _ => fail("E_DATA_SOURCE", "Unknown adjacency component."),
    }
}

/// `sop/fans`: per slot of the adjacency's point_vertices, the face's first vertex and the corners
/// its fan covers (0 unless a polygon of three corners or more).
pub fn fans(geometry: &Geometry, component: &str) -> Result<Value, Diagnostic> {
    let t = topo_view(geometry::topology(geometry))?;
    let p = point_index::View::view(&points_of(geometry::topology(geometry))?);
    let slots = |pick: &dyn Fn(i64, bool, i64) -> i64| -> Result<Value, Diagnostic> {
        let bytes = p.point_vertices.len() as i64 * 8;
        if bytes >= 1 << 20 {
            admit(bytes)?;
        }
        Ok(ints(
            p.point_vertices
                .iter()
                .map(|vertex| {
                    let primitive = p.primitive_of_vertex[*vertex as usize] as usize;
                    let first = t.primitive_offsets[primitive];
                    let corners = t.primitive_offsets[primitive + 1] - first;
                    pick(first, t.primitive_kinds[primitive] == 0 && corners >= 3, corners)
                })
                .collect(),
        ))
    };
    match component {
        "first" => slots(&|first, _, _| first),
        "corners" => slots(&|_, polygon, corners| if polygon { corners } else { 0 }),
        _ => fail("E_DATA_SOURCE", "Fans component is first or corners."),
    }
}

/// Rdk's shared ear clipping, every primitive in order: vertex corners per triangle, primitive
/// offsets into them, and ok 0 where the clipper refused.
pub fn triangles(geometry: &Geometry, component: &str) -> Result<Value, Diagnostic> {
    let topology = topo_view(geometry::topology(geometry))?;
    let positions = Float3::View::view(geometry::positions(geometry)).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))?;
    let count = topology.primitive_kinds.len();
    let mut offsets = vec![0i64; count + 1];
    let mut ok = vec![1i64; count];
    let (mut primitive, mut a, mut b, mut c) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    let mut scratch = polygon_triangulation::create_scratch();
    for p in 0..count {
        let mut rows: Vec<(i64, i64, i64)> = Vec::new();
        let result = polygon_triangulation::primitive(
            None,
            &positions,
            &topology,
            Some(&mut scratch),
            p as i64,
            &mut |_, x, y, z| rows.push((x, y, z)),
        );
        match result {
            Ok(()) => {
                for (x, y, z) in &rows {
                    primitive.push(p as i64);
                    a.push(*x);
                    b.push(*y);
                    c.push(*z);
                }
            }
            Err(_) => {
                ok[p] = 0;
                rows.clear();
            }
        }
        offsets[p + 1] = offsets[p] + rows.len() as i64;
    }
    match component {
        "offsets" => Ok(ints(offsets)),
        "ok" => Ok(ints(ok)),
        "primitive" => Ok(ints(primitive)),
        "a" => Ok(ints(a)),
        "b" => Ok(ints(b)),
        "c" => Ok(ints(c)),
        _ => fail("E_DATA_SOURCE", "Triangles source requires offsets, ok, primitive, a, b or c."),
    }
}

/// The union-find of `Analysis.classify_connectivity`: `over` edges labels points, `over` points
/// labels primitives; a 0/1 mask keeps the labelled elements.
pub fn components(geometry: &Geometry, args: &Args) -> Result<Value, Diagnostic> {
    let (owner, group_owner, count) = match arg(args, "over") {
        None => (attribute::Owner::Point, group::Owner::Point, geometry::point_count(geometry)),
        Some(v) if is_text(Some(v), "edges") => {
            (attribute::Owner::Point, group::Owner::Point, geometry::point_count(geometry))
        }
        Some(v) if is_text(Some(v), "points") => {
            (attribute::Owner::Primitive, group::Owner::Primitive, geometry::primitive_count(geometry))
        }
        _ => return fail("E_COMPONENTS_OVER", "Components :over is edges or points."),
    };
    let selected = match arg(args, "mask") {
        None => None,
        Some(Value::Int_array(mask)) if mask.len() as i64 == count => {
            if mask.iter().any(|bit| *bit != 0 && *bit != 1) {
                return fail("E_COMPONENTS_MASK", "Components mask holds only 0 and 1.");
            }
            let mask = mask.clone();
            Some(
                group::init(None, group_owner, "mask", count, &move |i| mask[i as usize] == 1)
                    .map_err(|e| diag("E_COMPONENTS_MASK", &rdk_message(&e)))?,
            )
        }
        Some(Value::Int_array(_)) => {
            return fail("E_COMPONENTS_MASK", "Components mask length differs from the labelled element count.");
        }
        Some(_) => return fail("E_COMPONENTS_MASK", "Components mask is an integer array."),
    };
    // `:seams` is a 0/1 edge mask (one entry per `sop/edges` edge): a seam edge joins nothing.
    let seams = match arg(args, "seams") {
        None => None,
        Some(Value::Int_array(mask)) => {
            let topology = geometry::topology(geometry);
            let index = index_of(topology)?;
            if mask.len() as i64 != topology_index::edge_count(&index) {
                return fail("E_COMPONENTS_SEAMS", "Components seams length differs from the edge count.");
            }
            if mask.iter().any(|bit| *bit != 0 && *bit != 1) {
                return fail("E_COMPONENTS_SEAMS", "Components seams hold only 0 and 1.");
            }
            let mask = mask.clone();
            Some(
                edge_group::init(None, topology, &index, "seams", &move |i| mask[i as usize] == 1)
                    .map_err(|e| diag("E_COMPONENTS_SEAMS", &rdk_message(&e)))?,
            )
        }
        Some(_) => return fail("E_COMPONENTS_SEAMS", "Components seams is an integer array."),
    };
    let primitives = if owner == attribute::Owner::Primitive { selected.as_ref() } else { None };
    let points = if owner == attribute::Owner::Point { selected.as_ref() } else { None };
    match analysis::classify_connectivity(primitives, points, seams.as_ref(), owner, geometry) {
        Err(e) => fail("E_COMPONENTS", &error::to_string(&e)),
        Ok((ids, n)) => match arg(args, "component") {
            Some(v) if is_text(Some(v), "count") => Ok(Value::Int(n)),
            Some(v) if is_text(Some(v), "id") => Ok(ints(ids)),
            _ => fail("E_DATA_SOURCE", "Components source requires id or count."),
        },
    }
}

// A static point array's planes, remembered by its identity: a recook of the same grid builds the
// same positions. ponytail: 16 entries, oldest dropped; dead keys pruned on access.
static POINT_PLANES: Mutex<Vec<(Weak<Vec<f64>>, Float3::T)>> = Mutex::new(Vec::new());
struct PlaneCopy {
    x: Weak<Vec<f64>>,
    y: Weak<Vec<f64>>,
    z: Weak<Vec<f64>>,
    positions: Float3::T,
}
static PLANE_COPIES: Mutex<Vec<PlaneCopy>> = Mutex::new(Vec::new());

fn alive(weak: &Weak<Vec<f64>>, key: &Arc<Vec<f64>>) -> bool {
    weak.upgrade().is_some_and(|a| Arc::ptr_eq(&a, key))
}

fn point_planes(points: &Arc<Vec<f64>>, make: &dyn Fn() -> Result<Float3::T, Diagnostic>) -> Result<Float3::T, Diagnostic> {
    {
        let mut all = POINT_PLANES.lock().unwrap();
        all.retain(|(key, _)| key.strong_count() > 0);
        if let Some((_, found)) = all.iter().find(|(key, _)| alive(key, points)) {
            return Ok(found.clone());
        }
    }
    let made = make()?;
    let mut all = POINT_PLANES.lock().unwrap();
    if all.len() >= 16 {
        all.remove(0);
    }
    all.push((Arc::downgrade(points), made.clone()));
    Ok(made)
}

/// `Some(positions)` when x's remembered copy was made from this y and z; the copy is remembered if
/// x had none.
fn plane_copies(
    x: &Arc<Vec<f64>>,
    y: &Arc<Vec<f64>>,
    z: &Arc<Vec<f64>>,
    copy: &dyn Fn() -> Result<Float3::T, Diagnostic>,
) -> Result<Float3::T, Diagnostic> {
    {
        let mut all = PLANE_COPIES.lock().unwrap();
        all.retain(|entry| entry.x.strong_count() > 0);
        if let Some(entry) = all.iter().find(|entry| alive(&entry.x, x)) {
            return if alive(&entry.y, y) && alive(&entry.z, z) { Ok(entry.positions.clone()) } else { copy() };
        }
    }
    let made = copy()?;
    let mut all = PLANE_COPIES.lock().unwrap();
    if all.len() >= 16 {
        all.remove(0);
    }
    all.push(PlaneCopy { x: Arc::downgrade(x), y: Arc::downgrade(y), z: Arc::downgrade(z), positions: made.clone() });
    Ok(made)
}

fn owned_planes(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> Result<Float3::T, Diagnostic> {
    Float3::View::of_owned_exn(x, y, z).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
}

const MESH_TYPE_MESSAGE: &str = "Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays.";
const PLANES_MESSAGE: &str = "Mesh planes require exactly x, y and z float arrays.";

/// The point count and a thunk building the packed positions (copies; retained Flow planes are
/// never adopted by geometry).
pub fn mesh_points(points: &Value) -> Result<(i64, Positions), Diagnostic> {
    match points {
        Value::Vec3_array(array) => {
            value::validate(points)?;
            let count = array.len() / 3;
            let array = array.clone();
            let planes = move || -> Result<Float3::T, Diagnostic> {
                let mut x = Vec::with_capacity(count);
                let mut y = Vec::with_capacity(count);
                let mut z = Vec::with_capacity(count);
                for i in 0..count {
                    x.push(array[3 * i]);
                    y.push(array[3 * i + 1]);
                    z.push(array[3 * i + 2]);
                }
                owned_planes(x, y, z)
            };
            let key = match points {
                Value::Vec3_array(a) => a.clone(),
                _ => unreachable!("matched above"),
            };
            Ok((count as i64, Box::new(move || if count > 4096 { point_planes(&key, &planes) } else { planes() })))
        }
        Value::Record(fields) if fields.len() == 3 => {
            let find = |name: &str| fields.iter().find(|(key, _)| key == name).map(|(_, v)| v);
            match (find("x"), find("y"), find("z")) {
                (Some(Value::Float_array(x)), Some(Value::Float_array(y)), Some(Value::Float_array(z))) => {
                    // Validation is per field in order: the first failing field's error. The
                    // OCaml parallel path above 65536 elements reports the same error.
                    for (_, field) in fields {
                        value::validate(field)?;
                    }
                    if x.len() != y.len() || x.len() != z.len() {
                        return fail("E_ARRAY_COUNT", "Mesh coordinate planes need the same element count.");
                    }
                    let count = x.len();
                    let (x, y, z) = (x.clone(), y.clone(), z.clone());
                    Ok((
                        count as i64,
                        Box::new(move || {
                            let copy = || owned_planes((*x).clone(), (*y).clone(), (*z).clone());
                            if count <= 4096 { copy() } else { plane_copies(&x, &y, &z, &copy) }
                        }),
                    ))
                }
                _ => fail("E_TOPOLOGY_TYPE", PLANES_MESSAGE),
            }
        }
        Value::Record(_) => fail("E_TOPOLOGY_TYPE", PLANES_MESSAGE),
        _ => fail("E_TOPOLOGY_TYPE", MESH_TYPE_MESSAGE),
    }
}

fn kind_of(code: i64) -> topology::PrimitiveKind {
    match code {
        0 => topology::PrimitiveKind::Polygon,
        1 => topology::PrimitiveKind::OpenPolyline,
        _ => topology::PrimitiveKind::ClosedPolyline,
    }
}

pub fn mesh(args: &Args) -> Result<Geometry, Diagnostic> {
    match (arg(args, "points"), arg(args, "vertex_points"), arg(args, "primitive_offsets"), arg(args, "kinds")) {
        (Some(points), Some(Value::Int_array(vertices)), Some(Value::Int_array(offsets)), Some(Value::Int_array(kinds))) => {
            let (count, positions) = mesh_points(points)?;
            if kinds.iter().any(|kind| *kind < 0 || *kind > 2) {
                return fail("E_TOPOLOGY_KIND", "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).");
            }
            let bytes = (vertices.len() + offsets.len() + kinds.len()) as i64 * 8;
            if bytes >= 1 << 20 {
                admit(bytes)?;
            }
            let kinds: Vec<topology::PrimitiveKind> = kinds.iter().map(|k| kind_of(*k)).collect();
            let topology = topology::create_owned(count, (**vertices).clone(), (**offsets).clone(), kinds)
                .map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))?;
            let positions = positions()?;
            geometry::create(positions, topology, None, None, None).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
        }
        _ => fail("E_TOPOLOGY_TYPE", MESH_TYPE_MESSAGE),
    }
}

/// The unread resident points of a mesh (OCaml `Resident (value, count)`).
pub struct ResidentPoints {
    pub value: Value,
    pub count: i64,
}
impl Deferred::Holder for ResidentPoints {}

/// The resident vertex points, primitive offsets and kinds of a device-computed topology.
pub struct ResidentTopology {
    pub vertices: Arc<Integers>,
    pub offsets: Arc<Integers>,
    pub kinds: Arc<Integers>,
    point_count: i64,
    incidence: OnceLock<Result<Option<Gpu::Incidence>, Diagnostic>>,
    build_incidence: Gpu::IncidenceBuilder,
}
impl Deferred::Holder for ResidentTopology {}

/// A positions holder made by `mesh_resident`.
pub fn resident_points_of(holder: &Arc<dyn Deferred::Holder>) -> Option<Arc<ResidentPoints>> {
    holder.clone().downcast()
}

/// Unread resident vertex points, primitive offsets and kinds, with their logical lengths. Counts
/// and holder lookup do not materialise topology.
fn resident_topology(geometry: &Geometry) -> Option<Arc<ResidentTopology>> {
    let topology = geometry::topology(geometry);
    let found = topology::holder(topology)?.downcast::<ResidentTopology>()?;
    if found.vertices.count == topology::vertex_count(topology)
        && found.offsets.count == topology::primitive_count(topology) + 1
        && found.kinds.count == topology::primitive_count(topology)
    {
        Some(found)
    } else {
        None
    }
}

pub fn resident(geometry: &Geometry) -> Option<(Arc<Integers>, Arc<Integers>, Arc<Integers>)> {
    resident_topology(geometry).map(|h| (h.vertices.clone(), h.offsets.clone(), h.kinds.clone()))
}

/// A topology source bound directly by a device consumer. The CPU resolver remains materialising.
pub fn resident_source(geometry: &Geometry, operation: &str, component: &str) -> Option<(Value, i64)> {
    let holder = resident_topology(geometry)?;
    let stream = match (operation, component) {
        ("sop/topology", "vertex_points") => &holder.vertices,
        ("sop/topology", "primitive_offsets") => &holder.offsets,
        ("sop/topology", "kinds") => &holder.kinds,
        ("sop/adjacency", "point_vertex_offsets" | "point_vertices" | "primitive_of_vertex") | ("sop/fans", "first" | "corners") => {
            let index = holder.incidence.get_or_init(|| (holder.build_incidence)(holder.point_count, [holder.vertices.clone(), holder.offsets.clone(), holder.kinds.clone()]));
            let index = index.as_ref().ok()?.as_ref()?;
            match component {
                "point_vertex_offsets" => &index.point_offsets,
                "point_vertices" => &index.point_vertices,
                "primitive_of_vertex" => &index.primitive_of_vertex,
                "first" => &index.first,
                _ => &index.corners,
            }
        }
        _ => return None,
    };
    stream.resident().map(|value| (value, stream.count))
}

/// A host topology beside points the device computes: built here, so the points stay where they are.
fn host_topology(point_count: i64, vertices: &[i64], offsets: &[i64], kinds: &[i64]) -> Result<topology::T, String> {
    if kinds.iter().any(|kind| *kind < 0 || *kind > 2) {
        return Err("Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).".to_owned());
    }
    topology::create_owned(point_count, vertices.to_vec(), offsets.to_vec(), kinds.iter().map(|k| kind_of(*k)).collect())
        .map_err(|e| rdk_message(&e))
}

/// A topology that depends on nothing a frame changes, kept from its mesh's first device cook by the
/// mesh's data (the same value while its node lives). Its owners keep the storage alive.
enum StaticKey {
    Floats(Weak<Vec<f64>>),
    Ints(Weak<Vec<i64>>),
    // a lazy residual is the physical value OCaml compares with ==: stable across frames, so its id and home arena
    Residual(i64, i64),
}
struct StaticTopology {
    key: StaticKey,
    point_count: i64,
    topology: topology::WeakTopology,
}
static STATIC_TOPOLOGIES: Mutex<Vec<StaticTopology>> = Mutex::new(Vec::new());

fn key_matches(key: &StaticKey, value: &Value) -> bool {
    match (key, value) {
        (StaticKey::Floats(weak), Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a)) => alive(weak, a),
        (StaticKey::Ints(weak), Value::Int_array(a)) => weak.upgrade().is_some_and(|k| Arc::ptr_eq(&k, a)),
        (StaticKey::Residual(id, home), Value::Residual(r)) => r.0 == *id && r.1 == *home,
        _ => false,
    }
}
fn key_dead(key: &StaticKey) -> bool {
    match key {
        StaticKey::Floats(weak) => weak.strong_count() == 0,
        StaticKey::Ints(weak) => weak.strong_count() == 0,
        StaticKey::Residual(..) => false,
    }
}
fn key_of(value: &Value) -> Option<StaticKey> {
    match value {
        Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a) => Some(StaticKey::Floats(Arc::downgrade(a))),
        Value::Int_array(a) => Some(StaticKey::Ints(Arc::downgrade(a))),
        Value::Residual(r) => Some(StaticKey::Residual(r.0, r.1)),
        _ => None,
    }
}

fn find_static(key: &Value) -> Option<(i64, topology::T)> {
    let mut all = STATIC_TOPOLOGIES.lock().unwrap();
    all.retain(|entry| !key_dead(&entry.key) && topology::upgrade(&entry.topology).is_some());
    all.iter().find(|entry| key_matches(&entry.key, key)).and_then(|entry| topology::upgrade(&entry.topology).map(|t| (entry.point_count, t)))
}

/// Whether a static mesh's topology is kept already: its cook need not compute it again.
pub fn static_cached(key: &Value) -> bool {
    find_static(key).is_some()
}

fn keep_static(key: &Value, point_count: i64, topology: &topology::T) {
    let Some(stored) = key_of(key) else { return };
    let mut all = STATIC_TOPOLOGIES.lock().unwrap();
    all.retain(|entry| !key_dead(&entry.key) && topology::upgrade(&entry.topology).is_some());
    all.retain(|entry| !key_matches(&entry.key, key));
    // ponytail: 64 meshes, oldest dropped
    if all.len() >= 64 {
        all.remove(0);
    }
    all.push(StaticTopology { key: stored, point_count, topology: topology::downgrade(topology) });
}

fn positions_of(planes: Arc<Planes>, count: i64) -> Result<(Float3::T, Option<Arc<ResidentPoints>>), Diagnostic> {
    let holder = planes.resident.clone().map(|value| Arc::new(ResidentPoints { value, count }));
    let dyn_holder: Option<Arc<dyn Deferred::Holder>> = holder.clone().map(|h| h as Arc<dyn Deferred::Holder>);
    let release: Deferred::Release = {
        let planes = planes.clone();
        Arc::new(move |held| planes.release(held))
    };
    let bounds: Float3::Bounds = {
        let planes = planes.clone();
        Arc::new(move || planes.bounds())
    };
    let materialise: Float3::Materialise = {
        let planes = planes.clone();
        Arc::new(move || planes.materialise().map_err(rdk_error))
    };
    let positions = Float3::deferred(dyn_holder, Some(release), Some(bounds), count, materialise)
        .map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))?;
    Ok((positions, holder))
}

fn count_of(scope: &dyn Residency, value: &Value) -> Option<i64> {
    scope.executor().count(value).ok().flatten()
}


/// The device cook of a mesh: `None` leaves it to the host path. `static_key` is the mesh's data
/// when its topology depends on nothing a frame changes.
pub fn mesh_resident(static_key: Option<&Value>, scope: &dyn Residency, args: &Args) -> Option<Result<Geometry, Diagnostic>> {
    if let Some(Value::Record(fields)) = arg(args, "points") {
        if fields.len() != 3 { return None; }
        let planes: Option<Vec<Value>> = ["x", "y", "z"].iter().map(|name| arg(fields, name).filter(|v| matches!(v, Value::Float_array(_))).cloned()).collect();
        let planes: [Value; 3] = planes?.try_into().ok()?;
        let mut counts = Vec::new();
        for (_, value) in fields {
            let n = match scope.executor().count(value) {
                Ok(Some(n)) => n,
                Ok(None) => {
                    if let Err(d) = value::validate(value) { return Some(Err(d)); }
                    value::array_length(value).ok()? as i64
                }
                Err(d) => return Some(Err(d)),
            };
            // All fields are validated before the shape error, matching mesh_points.
            counts.push(n);
        }
        if counts.iter().any(|n| *n != counts[0]) { return Some(fail("E_ARRAY_COUNT", "Mesh coordinate planes need the same element count.")); }
        let points = match scope.pack_planes(&planes) {
            Ok(Some(value)) => value,
            Ok(None) => return None,
            Err(d) => return Some(Err(d)),
        };
        let args: Vec<_> = args.iter().map(|(n, v)| (n.clone(), if n == "points" { points.clone() } else { v.clone() })).collect();
        return mesh_resident(static_key, scope, &args);
    }
    let cached = static_key.and_then(find_static);
    if let (Some((n, topology)), Some(points @ Value::Vec3_array(_))) = (&cached, arg(args, "points")) {
        if count_of(scope, points) == Some(*n) {
            let planes = Arc::new(scope.retain_float(points)?);
            let (positions, _) = match positions_of(planes.clone(), *n) {
                Ok(v) => v,
                Err(e) => {
                    planes.release(false);
                    return Some(Err(e));
                }
            };
            let result = geometry::create(positions, topology.clone(), None, None, None).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)));
            if result.is_err() {
                planes.release(false);
            }
            return Some(result);
        }
    }
    let remember = |topology: &topology::T, point_count: i64| {
        let Some(key) = static_key else { return };
        keep_static(key, point_count, topology);
    };
    let (points, vertices, offsets, kinds) = (arg(args, "points"), arg(args, "vertex_points"), arg(args, "primitive_offsets"), arg(args, "kinds"));
    if let (Some(points @ Value::Vec3_array(_)), Some(Value::Int_array(v)), Some(Value::Int_array(o)), Some(Value::Int_array(k))) =
        (points, vertices, offsets, kinds)
    {
        if count_of(scope, points).is_some()
            && [v, o, k].iter().all(|a| count_of(scope, &Value::Int_array((*a).clone())).is_none())
        {
            // device points, host topology: the points stay where they are
            let point_count = count_of(scope, points)?;
            let topology = match host_topology(point_count, v, o, k) {
                Ok(t) => t,
                Err(message) => return Some(Err(diag("E_TOPOLOGY", &message))),
            };
            let planes = Arc::new(scope.retain_float(points)?);
            let result = positions_of(planes.clone(), point_count).and_then(|(positions, _)| {
                geometry::create(positions, topology, None, None, None).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
            });
            if result.is_err() {
                planes.release(false);
            }
            return Some(result);
        }
    }
    let (Some(points @ Value::Vec3_array(device_points)), Some(vertices), Some(offsets), Some(kinds)) = (points, vertices, offsets, kinds) else {
        return None;
    };
    if device_points.is_empty() {
        // Empty maps may return owned arrays while their one-word offset stays resident. Validate
        // that stream before constructing the canonical empty mesh.
        let stream = |value: &Value| -> Option<Value> {
            if count_of(scope, value).is_some() {
                return Some(value.clone());
            }
            match value {
                Value::Int_array(a) if a.is_empty() => scope.executor().fill(&Ty::Array(Box::new(Ty::Int)), 0, &Value::Int(0)).ok().flatten(),
                _ => None,
            }
        };
        let (v, o, k) = (stream(vertices)?, stream(offsets)?, stream(kinds)?);
        let valid = scope.validate_topology(0, &v, &o, &k, 4294967295)?;
        return Some(valid.and_then(|()| {
            mesh(&[
                ("points".to_owned(), points.clone()),
                ("vertex_points".to_owned(), ints(Vec::new())),
                ("primitive_offsets".to_owned(), ints(vec![0])),
                ("kinds".to_owned(), ints(Vec::new())),
            ])
        }));
    }
    // a host stream of one repeated integer (all polygons, say) beside device streams: the device
    // fills it rather than the whole mesh coming back
    let on_device = count_of(scope, points).is_some();
    let device = |v: &Value| -> Value {
        if let Value::Int_array(a) = v {
            if on_device && count_of(scope, v).is_none() && !a.is_empty() && a.iter().all(|x| *x == a[0]) {
                if let Ok(Some(filled)) = scope.executor().fill(&Ty::Array(Box::new(Ty::Int)), a.len() as i64, &Value::Int(a[0])) {
                    return filled;
                }
            }
        }
        v.clone()
    };
    let (vertices, offsets, kinds) = (device(vertices), device(offsets), device(kinds));
    let (Some(point_count), Some(vertex_count), Some(_), Some(primitive_count)) =
        (count_of(scope, points), count_of(scope, &vertices), count_of(scope, &offsets), count_of(scope, &kinds))
    else {
        return None;
    };
    match scope.validate_topology(point_count, &vertices, &offsets, &kinds, 4294967295)? {
        Err(e) => return Some(Err(e)),
        Ok(()) => {}
    }
    let mut acquired_floats: Vec<Arc<Planes>> = Vec::new();
    let mut acquired_ints: Vec<Arc<Integers>> = Vec::new();
    let built = (|| -> Result<Geometry, Diagnostic> {
        let retain_error = || diag("E_GPU", "Validated mesh stream could not be retained.");
        let planes = Arc::new(scope.retain_float(points).ok_or_else(retain_error)?);
        acquired_floats.push(planes.clone());
        let mut integer = |value: &Value| -> Result<Arc<Integers>, Diagnostic> {
            let kept = Arc::new(scope.retain_uint32(value).ok_or_else(retain_error)?);
            acquired_ints.push(kept.clone());
            Ok(kept)
        };
        let (iv, io, ik) = (integer(&vertices)?, integer(&offsets)?, integer(&kinds)?);
        // the session keeps this: the streams' releases, not the streams
        let holder = Arc::new(ResidentTopology { vertices: iv.clone(), offsets: io.clone(), kinds: ik.clone(),
            point_count, incidence: OnceLock::new(), build_incidence: scope.incidence() });
        let release: Deferred::Release = {
            let holder = holder.clone();
            Arc::new(move |held| {
                holder.vertices.release(held);
                holder.offsets.release(held);
                holder.kinds.release(held);
                if let Some(Ok(Some(index))) = holder.incidence.get() {
                    for stream in [&index.point_offsets, &index.point_vertices, &index.primitive_of_vertex, &index.first, &index.corners] { stream.release(held); }
                }
            })
        };
        let materialise = {
            let holder = holder.clone();
            Arc::new(move || {
                let kinds: Vec<topology::PrimitiveKind> = holder.kinds.materialise().map_err(rdk_error)?.iter().map(|k| kind_of(*k)).collect();
                topology::create_owned(point_count, holder.vertices.materialise().map_err(rdk_error)?, holder.offsets.materialise().map_err(rdk_error)?, kinds)
            })
        };
        let topology = topology::deferred(None, Some(holder.clone() as Arc<dyn Deferred::Holder>), Some(release), point_count, vertex_count, primitive_count, materialise)
            .map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))?;
        remember(&topology, point_count);
        let (positions, _) = positions_of(planes, point_count)?;
        geometry::create(positions, topology, None, None, None).map_err(|e| diag("E_TOPOLOGY", &rdk_message(&e)))
    })();
    if built.is_err() {
        for planes in &acquired_floats {
            planes.release(false);
        }
        for kept in &acquired_ints {
            kept.release(false);
        }
    }
    Some(built)
}

pub fn count(geometry: &Geometry, element: &str) -> Result<i64, Diagnostic> {
    match element {
        "points" => Ok(geometry::point_count(geometry)),
        "vertices" => Ok(geometry::vertex_count(geometry)),
        "primitives" => Ok(geometry::primitive_count(geometry)),
        "edges" => Ok(topology_index::edge_count(&index_of(geometry::topology(geometry))?)),
        _ => fail("E_TOPOLOGY_COUNT", "Count element is points, vertices, primitives or edges."),
    }
}

/// Resolve one `sop/count`, `sop/edges`, `sop/topology`, `sop/adjacency`, `sop/components`,
/// `sop/triangles` or `sop/fans` source over cooked geometry.
pub fn resolve(geometry: &dyn Fn(i64) -> Option<Geometry>, source: &Value) -> Result<Value, Diagnostic> {
    let Value::Source(g) = source else {
        return fail("E_DATA_SOURCE", "Unknown topology data source.");
    };
    let (name, args) = (g.name.as_str(), &g.fields);
    if !matches!(name, "sop/count" | "sop/edges" | "sop/topology" | "sop/adjacency" | "sop/components" | "sop/triangles" | "sop/fans") {
        return fail("E_DATA_SOURCE", "Unknown topology data source.");
    }
    let Some(Value::Deferred(ty, id)) = arg(args, "geometry") else {
        return fail("E_DATA_SOURCE", "Topology source requires cooked geometry.");
    };
    if !ty::is_geometry(ty) {
        return fail("E_DATA_SOURCE", "Topology source requires cooked geometry.");
    }
    let Some(geometry) = geometry(*id) else {
        return fail("E_DATA_SOURCE", "Topology source has not been cooked.");
    };
    let component = match arg(args, "component") {
        Some(Value::Text(bytes)) => Some(String::from_utf8_lossy(bytes).into_owned()),
        _ => None,
    };
    match name {
        "sop/count" => match arg(args, "element") {
            Some(Value::Text(element)) => count(&geometry, &String::from_utf8_lossy(element)).map(Value::Int),
            _ => fail("E_TOPOLOGY_COUNT", "Count element is points, vertices, primitives or edges."),
        },
        "sop/topology" => match component {
            Some(c) => kept(&geometry, "topology:", &c, &topology_component),
            None => fail("E_DATA_SOURCE", "Topology source requires vertex_points, primitive_offsets or kinds."),
        },
        "sop/components" => components(&geometry, args),
        "sop/triangles" => match component {
            Some(c) => triangles(&geometry, &c),
            None => fail("E_DATA_SOURCE", "Triangles source requires a component name."),
        },
        "sop/fans" => match component {
            Some(c) => kept(&geometry, "fans:", &c, &fans),
            None => fail("E_DATA_SOURCE", "Fans source requires a component name."),
        },
        "sop/adjacency" => match component {
            Some(c) => kept(&geometry, "adjacency:", &c, &adjacency),
            None => fail("E_DATA_SOURCE", "Adjacency source requires a component name."),
        },
        _ => {
            let index = topology_index::View::view(&index_of(geometry::topology(&geometry))?);
            match component.as_deref() {
                Some("a") => Ok(ints(index.edge_a.to_vec())),
                Some("b") => Ok(ints(index.edge_b.to_vec())),
                _ => fail("E_DATA_SOURCE", "Edge source requires endpoint component a or b."),
            }
        }
    }
}

enum Carry {
    Plain(Arc<Vec<i64>>),
    Weighted(Arc<Vec<i64>>, Arc<Vec<i64>>, Arc<Vec<f64>>),
}

fn check(cancel: Option<&cancel::T>) -> Result<(), Diagnostic> {
    cancel::check_opt(cancel).map_err(|e| diag("E_CANCELLED", &rdk_message(&e)))
}

fn mapping(cancel: Option<&cancel::T>, args: &Args, name: &str, source_count: i64, target_count: i64) -> Result<Carry, Diagnostic> {
    let bounded = |label: &str, map: &[i64]| -> Result<(), Diagnostic> {
        if map.len() as i64 != target_count {
            return fail("E_REMAP_COUNT", &format!("Remap {name} map must match target owner count."));
        }
        for (row, index) in map.iter().enumerate() {
            if row & 4095 == 0 {
                check(cancel)?;
            }
            let low = if label.is_empty() { -1 } else { 0 };
            if *index < low || *index >= source_count {
                return fail("E_REMAP_INDEX", &format!("Remap {name} source index is out of bounds."));
            }
        }
        Ok(())
    };
    let mix = match name {
        "points" => "point_mix",
        "vertices" => "vertex_mix",
        _ => "primitive_mix",
    };
    match arg(args, name).or_else(|| arg(args, mix)) {
        Some(Value::Int_array(map)) => {
            bounded("", map)?;
            Ok(Carry::Plain(map.clone()))
        }
        Some(Value::Record(fields)) => {
            let mut names: Vec<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            names.sort();
            let find = |key: &str| fields.iter().find(|(n, _)| n == key).map(|(_, v)| v);
            match (names.as_slice(), find("a"), find("b"), find("t")) {
                (["a", "b", "t"], Some(Value::Int_array(a)), Some(Value::Int_array(b)), Some(Value::Float_array(t))) => {
                    if t.len() as i64 != target_count {
                        return fail("E_REMAP_COUNT", &format!("Remap {name} map must match target owner count."));
                    }
                    bounded("a", a)?;
                    bounded("b", b)?;
                    for (row, weight) in t.iter().enumerate() {
                        if row & 4095 == 0 {
                            check(cancel)?;
                        }
                        if !weight.is_finite() {
                            return fail("E_REMAP_WEIGHT", &format!("Remap {name} weight must be finite."));
                        }
                    }
                    Ok(Carry::Weighted(a.clone(), b.clone(), t.clone()))
                }
                _ => fail("E_REMAP_TYPE", &format!("Remap {mix} is {{:a :b :t}} integer, integer and float arrays.")),
            }
        }
        _ => fail("E_REMAP_TYPE", &format!("Remap {name} requires an integer array.")),
    }
}

fn same(cancel: Option<&cancel::T>, carry: &Carry, count: i64) -> Result<bool, Diagnostic> {
    match carry {
        Carry::Plain(map) if map.len() as i64 == count => {
            let mut same = true;
            for (i, value) in map.iter().enumerate() {
                if i & 4095 == 0 {
                    check(cancel)?;
                }
                if i as i64 != *value {
                    same = false;
                }
            }
            Ok(same)
        }
        _ => Ok(false),
    }
}

fn remap_error(e: &error::T) -> Diagnostic {
    diag("E_REMAP", &rdk_message(e))
}

/// `sop/remap`: carry source attributes and groups onto the target's elements by index maps.
pub fn remap(cancel: Option<&cancel::T>, grain: i64, source: &Geometry, target: &Geometry, args: &Args) -> Result<Geometry, Diagnostic> {
    check(cancel)?;
    if grain <= 0 {
        return fail("E_REMAP", "Remap grain must be positive.");
    }
    let use_mapped = match arg(args, "positions") {
        None => false,
        Some(v) if is_text(Some(v), "target") => false,
        Some(v) if is_text(Some(v), "mapped") => true,
        _ => return fail("E_REMAP_POLICY", "Remap positions is target or mapped."),
    };
    let point_carry = mapping(cancel, args, "points", geometry::point_count(source), geometry::point_count(target))?;
    let vertex_carry = mapping(cancel, args, "vertices", geometry::vertex_count(source), geometry::vertex_count(target))?;
    let primitive_carry = mapping(cancel, args, "primitives", geometry::primitive_count(source), geometry::primitive_count(target))?;
    let target_topology = geometry::topology(target);
    let source_topology = geometry::topology(source);
    let source_edge_groups = geometry::edge_groups(source);
    // Explicit CSR is checked even if there are no source edge groups.
    let edge_groups = match arg(args, "edges") {
        None if source_edge_groups.is_empty() => Vec::new(),
        None if topology::data_id(source_topology) == topology::data_id(target_topology) => source_edge_groups,
        None => return fail("E_REMAP_EDGES", "Remap changed topology requires explicit edge ancestry."),
        Some(Value::Record(fields)) => {
            let pair = match fields.as_slice() {
                [(a, Value::Int_array(x)), (b, Value::Int_array(y))] if a == "offsets" && b == "values" => Some((x, y)),
                [(a, Value::Int_array(y)), (b, Value::Int_array(x))] if a == "values" && b == "offsets" => Some((x, y)),
                _ => None,
            };
            let Some((offsets, ancestors)) = pair else {
                return fail("E_REMAP_EDGES", "Remap edges requires offsets and values integer arrays.");
            };
            if source_edge_groups.is_empty() {
                Vec::new()
            } else {
                let source_index = topology_index::create(cancel, source_topology).map_err(|e| remap_error(&e))?;
                let target_index = topology_index::create(cancel, target_topology).map_err(|e| remap_error(&e))?;
                remap_core::edge_groups_from_ancestry(cancel, grain, &source_index, target_topology, &target_index, offsets, ancestors, &source_edge_groups)
                    .map_err(|e| diag("E_REMAP_EDGES", &rdk_message(&e)))?
            }
        }
        Some(_) => return fail("E_REMAP_EDGES", "Remap edges requires offsets and values integer arrays."),
    };
    let point_same = same(cancel, &point_carry, geometry::point_count(source))?;
    let vertex_same = same(cancel, &vertex_carry, geometry::vertex_count(source))?;
    let primitive_same = same(cancel, &primitive_carry, geometry::primitive_count(source))?;
    let mut attributes = Vec::new();
    for item in geometry::attributes(source) {
        check(cancel)?;
        let owner = attribute::owner(&item);
        let (unchanged, carry) = match owner {
            attribute::Owner::Detail => {
                attributes.push(item);
                continue;
            }
            attribute::Owner::Point => (point_same, &point_carry),
            attribute::Owner::Vertex => (vertex_same, &vertex_carry),
            attribute::Owner::Primitive => (primitive_same, &primitive_carry),
        };
        if unchanged {
            attributes.push(item);
            continue;
        }
        attributes.push(
            match carry {
                Carry::Plain(map) => remap_core::attribute(cancel, grain, Some(true), map, &item),
                Carry::Weighted(a, b, t) => remap_core::blend_attribute(cancel, grain, a, b, t, &item),
            }
            .map_err(|e| remap_error(&e))?,
        );
    }
    let mut groups = Vec::new();
    for item in geometry::groups(source) {
        check(cancel)?;
        let (unchanged, carry) = match group::owner(&item) {
            group::Owner::Point => (point_same, &point_carry),
            group::Owner::Vertex => (vertex_same, &vertex_carry),
            group::Owner::Primitive => (primitive_same, &primitive_carry),
        };
        if unchanged {
            groups.push(item);
            continue;
        }
        groups.push(
            match carry {
                Carry::Plain(map) => remap_core::group(cancel, grain, map, &item),
                Carry::Weighted(a, b, t) => remap_core::blend_group(cancel, grain, a, b, t, &item),
            }
            .map_err(|e| remap_error(&e))?,
        );
    }
    let positions = if !use_mapped {
        geometry::positions(target).clone()
    } else if point_same {
        geometry::positions(source).clone()
    } else {
        match &point_carry {
            Carry::Weighted(a, b, t) => remap_core::blend_positions(cancel, grain, a, b, t, geometry::positions(source)).map_err(|e| remap_error(&e))?,
            Carry::Plain(map) => {
                let p = Float3::View::view(geometry::positions(source)).map_err(|e| remap_error(&e))?;
                let x = remap_core::select_float(cancel, grain, Some(0.0), map, &p.x).map_err(|e| remap_error(&e))?;
                let y = remap_core::select_float(cancel, grain, Some(0.0), map, &p.y).map_err(|e| remap_error(&e))?;
                let z = remap_core::select_float(cancel, grain, Some(0.0), map, &p.z).map_err(|e| remap_error(&e))?;
                Float3::View::of_owned_exn(x, y, z).map_err(|e| remap_error(&e))?
            }
        }
    };
    // `:lead true`: attributes already on the target come first, ahead of the carried ones.
    let attributes = match arg(args, "lead") {
        Some(Value::Bool(true)) => geometry::attributes(target).into_iter().chain(attributes).collect(),
        None | Some(Value::Bool(false)) => attributes,
        _ => return fail("E_REMAP_LEAD", "Remap lead is a Boolean."),
    };
    // `:lead_groups true`: the groups and edge groups already on the target come first.
    let (groups, edge_groups) = match arg(args, "lead_groups") {
        Some(Value::Bool(true)) => (
            geometry::groups(target).into_iter().chain(groups).collect(),
            geometry::edge_groups(target).into_iter().chain(edge_groups).collect(),
        ),
        None | Some(Value::Bool(false)) => (groups, edge_groups),
        _ => return fail("E_REMAP_LEAD", "Remap lead_groups is a Boolean."),
    };
    check(cancel)?;
    let output = geometry::create(positions, target_topology.clone(), Some(attributes), Some(groups), Some(edge_groups)).map_err(|e| remap_error(&e))?;
    check(cancel)?;
    Ok(output)
}

/// Native `merge` on a list of cooked geometries: the same `Mesh_merge.run_tagged`.
pub fn concat(cancel: Option<Arc<cancel::T>>, grain: i64, sources: &[Geometry], args: &Args) -> Result<Geometry, Diagnostic> {
    let text = |name: &str| -> Result<String, Diagnostic> {
        match arg(args, name) {
            None => Ok(String::new()),
            Some(Value::Text(bytes)) => Ok(trim(bytes)),
            Some(_) => fail("E_CONCAT_TYPE", &format!("Concat {name} must be text.")),
        }
    };
    let integer = |name: &str, default: i64| -> Result<i64, Diagnostic> {
        match arg(args, name) {
            None => Ok(default),
            Some(Value::Int(n)) => Ok(*n),
            Some(_) => fail("E_CONCAT_TYPE", &format!("Concat {name} must be an integer.")),
        }
    };
    let attribute = text("source_attribute")?;
    let source_base = integer("source_base", 0)?;
    let tag_base = integer("tag_base", -1)?;
    if grain <= 0 {
        return fail("E_CONCAT", "Concat grain must be positive.");
    }
    let params = rdk_ops::mesh_merge::RunTaggedParams {
        cancel,
        grain: Some(grain),
        tag_base,
        source_attribute: if attribute.is_empty() { None } else { Some(attribute) },
        source_base,
    };
    rdk_ops::mesh_merge::run_tagged(&params, sources).map_err(|e| {
        let mut d = diag(error::code(&e).as_str(), &format!("{} could not produce valid geometry", error::operation(&e)));
        d.cause = Some(error::to_string(&e).into_bytes());
        d
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use flow_syntax::value::source_value;

    fn text(s: &str) -> Value {
        Value::Text(s.as_bytes().to_vec())
    }
    fn floats(v: Vec<f64>) -> Value {
        Value::Vec3_array(Arc::new(v))
    }
    fn triangle() -> Geometry {
        mesh(&[
            ("points".into(), floats(vec![0., 0., 0., 1., 0., 0., 0., 1., 0.])),
            ("vertex_points".into(), ints(vec![0, 1, 2])),
            ("primitive_offsets".into(), ints(vec![0, 3])),
            ("kinds".into(), ints(vec![0])),
        ])
        .unwrap()
    }

    #[test]
    fn mesh_and_counts() {
        let g = triangle();
        assert_eq!(count(&g, "points").unwrap(), 3);
        assert_eq!(count(&g, "edges").unwrap(), 3);
        assert_eq!(count(&g, "bogus").err().unwrap().code, "E_TOPOLOGY_COUNT");
        let bad = mesh(&[
            ("points".into(), floats(vec![0., 0., 0.])),
            ("vertex_points".into(), ints(vec![0])),
            ("primitive_offsets".into(), ints(vec![0, 1])),
            ("kinds".into(), ints(vec![3])),
        ]);
        assert_eq!(bad.err().unwrap().code, "E_TOPOLOGY_KIND");
    }

    #[test]
    fn topology_and_triangles() {
        let g = triangle();
        assert_eq!(topology_component(&g, "vertex_points").unwrap(), ints(vec![0, 1, 2]));
        assert_eq!(topology_component(&g, "kinds").unwrap(), ints(vec![0]));
        assert_eq!(triangles(&g, "offsets").unwrap(), ints(vec![0, 1]));
        assert_eq!(triangles(&g, "a").unwrap(), ints(vec![0]));
        assert_eq!(fans(&g, "corners").unwrap(), ints(vec![3, 3, 3]));
        assert_eq!(adjacency(&g, "edge_a").unwrap(), ints(vec![0, 1, 0]));
    }

    #[test]
    fn components_count() {
        let g = triangle();
        let args = vec![("component".to_owned(), text("count"))];
        assert_eq!(components(&g, &args).unwrap(), Value::Int(1));
        let bad = vec![("over".to_owned(), text("faces"))];
        assert_eq!(components(&g, &bad).err().unwrap().code, "E_COMPONENTS_OVER");
    }

    #[test]
    fn remap_identity_and_concat() {
        let g = triangle();
        let args = vec![
            ("points".to_owned(), ints(vec![0, 1, 2])),
            ("vertices".to_owned(), ints(vec![0, 1, 2])),
            ("primitives".to_owned(), ints(vec![0])),
        ];
        let out = remap(None, 4096, &g, &g, &args).unwrap();
        assert_eq!(geometry::point_count(&out), 3);
        let bad = vec![("points".to_owned(), ints(vec![0, 1, 5])), ("vertices".to_owned(), ints(vec![0, 1, 2])), ("primitives".to_owned(), ints(vec![0]))];
        assert_eq!(remap(None, 4096, &g, &g, &bad).err().unwrap().code, "E_REMAP_INDEX");
        let merged = concat(None, 4096, &[g.clone(), g.clone()], &[]).unwrap();
        assert_eq!(geometry::point_count(&merged), 6);
        assert_eq!(concat(None, 0, &[g], &[]).err().unwrap().code, "E_CONCAT");
    }

    #[test]
    fn resolve_sources() {
        let g = triangle();
        let lookup = move |id: i64| if id == 7 { Some(g.clone()) } else { None };
        let source = source_value(
            "sop/count",
            Ty::Int,
            vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("element".into(), text("points"))],
        );
        assert_eq!(resolve(&lookup, &source).unwrap(), Value::Int(3));
        let missing = source_value("sop/count", Ty::Int, vec![("geometry".into(), Value::Deferred(ty::geometry(), 8))]);
        assert_eq!(resolve(&lookup, &missing).err().unwrap().message, "Topology source has not been cooked.");
    }
}

/// Ports of lib/flow_sop/test_topology_kernel.ml and test_adjacency.ml (the parts that need no
/// Lisp_sop): the authored operators against the native indices, both execution tiers, at one and
/// eight domains.
#[cfg(test)]
mod integration {
    use super::*;
    use flow_syntax::value::source_value;
    use crate::{attribute_kernel, operators};
    use flow_check::catalog::Catalog;
    use flow_ir::flow_ir::Executor;
    use rdk_core::analysis;

    fn ok<R>(r: Result<R, error::T>) -> R {
        match r {
            Ok(v) => v,
            Err(e) => panic!("{}", error::to_string(&e)),
        }
    }
    fn flow_ok<R>(r: Result<R, Diagnostic>) -> R {
        match r {
            Ok(v) => v,
            Err(d) => panic!("{}: {}", d.code, d.message),
        }
    }
    fn ints(a: &[i64]) -> Value {
        Value::Int_array(Arc::new(a.to_vec()))
    }
    fn text(s: &str) -> Value {
        Value::Text(s.as_bytes().to_vec())
    }
    fn lookup_of(geometry: Option<Geometry>) -> attribute_kernel::GeometryLookup {
        Arc::new(move |id| if id == 7 { geometry.clone() } else { None })
    }

    fn fixtures() -> Vec<(&'static str, Geometry)> {
        use topology::PrimitiveKind::{ClosedPolyline, OpenPolyline, Polygon};
        let make = |points: i64, vertices: Vec<i64>, offsets: Vec<i64>, kinds: Vec<topology::PrimitiveKind>| {
            ok(topology::create_owned(points, vertices, offsets, kinds))
        };
        let all = vec![
            ("empty", ok(topology::empty(0))),
            ("shared-polygons", make(5, vec![0, 1, 2, 2, 1, 3], vec![0, 3, 6], vec![Polygon, Polygon])),
            ("open-repeated", make(3, vec![0, 1, 1, 2, 1], vec![0, 5], vec![OpenPolyline])),
            ("closed", make(4, vec![3, 2, 1, 0], vec![0, 4], vec![ClosedPolyline])),
            (
                "mixed-nonmanifold",
                make(6, vec![0, 1, 2, 0, 1, 3, 1, 0, 4, 0, 1, 5], vec![0, 3, 6, 9, 12], vec![Polygon, OpenPolyline, ClosedPolyline, Polygon]),
            ),
        ];
        all.into_iter()
            .map(|(name, topology)| {
                let count = topology::point_count(&topology) as usize;
                let positions = ok(Float3::View::of_owned_exn((0..count).map(|i| i as f64).collect(), vec![0.0; count], vec![0.0; count]));
                (name, ok(geometry::create(positions, topology, None, None, None)))
            })
            .collect()
    }

    fn catalog() -> Catalog {
        Catalog { version: 1, kinds: Vec::new() }
    }

    fn checked(body: &str) -> flow_check::workspace::Workspace {
        let source = format!("(workspace adjacency (graph g :context sop [(geo : geometry nil)] (let* [tested {body}] geo)))");
        let forms = flow_ok(flow_syntax::syntax::parse(&source));
        match flow_check::workspace::check(operators::all(), false, false, None, &catalog(), &forms) {
            (Some(checked), d) if d.is_empty() => checked,
            (_, d) => panic!("{}", d.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("; ")),
        }
    }

    fn evaluate(geometry: &Geometry, body: &str) -> (Value, flow_ir::core_exec::ResolveFn) {
        let inputs = vec![("g".to_owned(), vec![("geo".to_owned(), Value::Deferred(ty::geometry(), 7))])];
        let evaluation = flow_ok(flow_eval::static_with(&checked(body), true, Some(&inputs), None));
        let value = evaluation
            .records
            .iter()
            .find(|(path, _)| path == &vec!["g".to_owned(), "tested".to_owned()])
            .map(|(_, values)| values[0].1.clone())
            .expect("tested record");
        let lookup = lookup_of(Some(geometry.clone()));
        (value, Arc::new(move |v| attribute_kernel::resolve(None, &lookup, v)))
    }

    fn over_domains(work: &dyn Fn()) {
        for domains in [1, 8] {
            parallel::run_unit(Some(domains), &|| {
                work();
                Ok(())
            })
            .unwrap();
        }
    }

    /// Both execution tiers at one and eight domains must return the same value.
    fn agree(geometry: &Geometry, body: &str, expected: &Value) {
        let (value, resolve) = evaluate(geometry, body);
        let live = input::at_time(0.25);
        let core = flow_ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
        over_domains(&|| {
            assert_eq!(&flow_ok(flow_eval::Host::force_reference(&value, &live, None, None, Some(resolve.clone()))), expected, "{body}");
            let options = Executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() };
            assert_eq!(&flow_ok(Executor::force(&core, &options, &live)), expected, "{body}");
        });
    }

    #[test]
    fn counts_and_edges_match_the_native_index() {
        for (name, geometry) in fixtures() {
            let index = flow_ok(index_of(geometry::topology(&geometry)));
            let view = topology_index::View::view(&index);
            for (element, expected) in [
                ("points", geometry::point_count(&geometry)),
                ("vertices", geometry::vertex_count(&geometry)),
                ("primitives", geometry::primitive_count(&geometry)),
                ("edges", topology_index::edge_count(&index)),
            ] {
                agree(&geometry, &format!("(sop/count geo :{element})"), &Value::Int(expected));
            }
            let expected = Value::Record(vec![("a".into(), ints(&view.edge_a)), ("b".into(), ints(&view.edge_b))]);
            agree(&geometry, "(sop/edges geo)", &expected);
            let mut live = input::at_time(0.25);
            live.frame = 3;
            let (value, resolve) = evaluate(&geometry, "(map (fn [(x : int)] (int/add x (frame/index))) (get (sop/edges geo) :a))");
            let core = flow_ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
            let expected = ints(&view.edge_a.iter().map(|x| x + 3).collect::<Vec<_>>());
            over_domains(&|| {
                assert_eq!(flow_ok(flow_eval::Host::force_reference(&value, &live, None, None, Some(resolve.clone()))), expected);
                let options = Executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() };
                assert_eq!(flow_ok(Executor::force(&core, &options, &live)), expected);
            });
            let (value, resolve) = evaluate(&geometry, "(let* [n (sop/count geo :points)] (array/int n 9007199254740993))");
            let core = flow_ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
            let options = Executor::ForceOptions { strict: true, resolve: Some(resolve), ..Default::default() };
            let expected = ints(&vec![9007199254740993; geometry::point_count(&geometry) as usize]);
            assert_eq!(flow_ok(Executor::force(&core, &options, &live)), expected, "{name}");
        }
    }

    #[test]
    fn adjacency_fans_and_triangles() {
        for (_, geometry) in fixtures() {
            let index = flow_ok(index_of(geometry::topology(&geometry)));
            let v = topology_index::View::view(&index);
            let expected = Value::Record(vec![
                ("primitive_of_vertex".into(), ints(&v.primitive_of_vertex)),
                ("next_vertex".into(), ints(&v.next_vertex)),
                ("previous_vertex".into(), ints(&v.previous_vertex)),
                ("edge_of_vertex".into(), ints(&v.edge_of_vertex)),
                ("opposite_vertex".into(), ints(&v.opposite_vertex)),
                ("edge_a".into(), ints(&v.edge_a)),
                ("edge_b".into(), ints(&v.edge_b)),
                ("point_edge_offsets".into(), ints(&v.point_edge_offsets)),
                ("point_edges".into(), ints(&v.point_edges)),
                ("point_vertex_offsets".into(), ints(&v.point_offsets)),
                ("point_vertices".into(), ints(&v.point_vertices)),
            ]);
            agree(&geometry, "(sop/adjacency geo)", &expected);
            agree(&geometry, "(get (sop/adjacency geo) :edge_a)", &ints(&v.edge_a));
            // sop/fans: the face of each incident corner, as the gather of area normals folds it
            let t = flow_ok(topo_view(geometry::topology(&geometry)));
            let face = |f: &dyn Fn(usize) -> i64| ints(&v.point_vertices.iter().map(|vertex| f(v.primitive_of_vertex[*vertex as usize] as usize)).collect::<Vec<_>>());
            agree(&geometry, "(get (sop/fans geo) :first)", &face(&|p| t.primitive_offsets[p]));
            agree(
                &geometry,
                "(get (sop/fans geo) :corners)",
                &face(&|p| {
                    let corners = t.primitive_offsets[p + 1] - t.primitive_offsets[p];
                    if t.primitive_kinds[p] == 0 && corners >= 3 { corners } else { 0 }
                }),
            );
        }
        let resolve = |geometry: Value| {
            let lookup = lookup_of(None);
            attribute_kernel::resolve(None, &lookup, &source_value("sop/adjacency", Ty::Array(Box::new(Ty::Int)), vec![("geometry".into(), geometry), ("component".into(), text("next_vertex"))]))
        };
        for geometry in [Value::No_geo, Value::Deferred(ty::geometry(), 7)] {
            assert_eq!(resolve(geometry).err().unwrap().code, "E_DATA_SOURCE");
        }
    }

    #[test]
    fn components_equal_classify_connectivity() {
        for (_, geometry) in fixtures() {
            for (over, owner, group_owner, count) in [
                ("edges", attribute::Owner::Point, group::Owner::Point, geometry::point_count(&geometry)),
                ("points", attribute::Owner::Primitive, group::Owner::Primitive, geometry::primitive_count(&geometry)),
            ] {
                let classify = |mask: Option<&[i64]>| {
                    let made = mask.map(|mask| {
                        let mask = mask.to_vec();
                        ok(group::init(None, group_owner, "m", count, &move |i| mask[i as usize] == 1))
                    });
                    let (primitives, points) = if owner == attribute::Owner::Primitive { (made.as_ref(), None) } else { (None, made.as_ref()) };
                    let (ids, n) = ok(analysis::classify_connectivity(primitives, points, None, owner, &geometry));
                    Value::Record(vec![("id".into(), ints(&ids)), ("count".into(), Value::Int(n))])
                };
                agree(&geometry, &format!("(sop/components geo :over \"{over}\")"), &classify(None));
                let all = vec![1; count as usize];
                let element = if over == "edges" { "points" } else { "primitives" };
                agree(&geometry, &format!("(sop/components geo :over \"{over}\" :mask (array/int (sop/count geo :{element}) 1))"), &classify(Some(&all)));
                if count > 0 {
                    let mask: Vec<i64> = (0..count).map(|i| if i % 3 == 1 { 0 } else { 1 }).collect();
                    agree(
                        &geometry,
                        &format!("(sop/components geo :over \"{over}\" :mask (exact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 1) 0 1)) (array/range {count} :type int))))"),
                        &classify(Some(&mask)),
                    );
                }
            }
        }
    }

    #[test]
    fn components_seams() {
        for (_, geometry) in fixtures() {
            let topology = geometry::topology(&geometry);
            let index = flow_ok(index_of(topology));
            let edges = topology_index::edge_count(&index);
            let seams = ok(edge_group::init(None, topology, &index, "seams", &|i| i % 3 == 1));
            let seam_text = format!("(exact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 1) 1 0)) (array/range {edges} :type int)))");
            for (over, owner, group_owner, count) in [
                ("edges", attribute::Owner::Point, group::Owner::Point, geometry::point_count(&geometry)),
                ("points", attribute::Owner::Primitive, group::Owner::Primitive, geometry::primitive_count(&geometry)),
            ] {
                let classify = |mask: Option<Vec<i64>>| {
                    let made = mask.map(|mask| ok(group::init(None, group_owner, "m", count, &move |i| mask[i as usize] == 1)));
                    let (primitives, points) = if owner == attribute::Owner::Primitive { (made.as_ref(), None) } else { (None, made.as_ref()) };
                    let (ids, n) = ok(analysis::classify_connectivity(primitives, points, Some(&seams), owner, &geometry));
                    Value::Record(vec![("id".into(), ints(&ids)), ("count".into(), Value::Int(n))])
                };
                if edges > 0 {
                    agree(&geometry, &format!("(sop/components geo :over \"{over}\" :seams {seam_text})"), &classify(None));
                    if count > 0 {
                        let mask: Vec<i64> = (0..count).map(|i| if i % 4 == 2 { 0 } else { 1 }).collect();
                        agree(
                            &geometry,
                            &format!("(sop/components geo :over \"{over}\" :seams {seam_text} :mask (exact (map (fn [(i : int)] (if (int/eq (int/mod i 4) 2) 0 1)) (array/range {count} :type int))))"),
                            &classify(Some(mask)),
                        );
                    }
                }
            }
            let refused = |body: &str, code: &str| {
                let (value, resolve) = evaluate(&geometry, body);
                let result = flow_eval::Host::force_reference(&value, &input::at_time(0.25), None, None, Some(resolve));
                assert_eq!(result.err().unwrap().code, code, "{body}");
            };
            refused(&format!("(sop/components geo :seams (array/int {} 0))", edges + 1), "E_COMPONENTS_SEAMS");
            if edges > 0 {
                refused(&format!("(sop/components geo :seams (array/int {edges} 2))"), "E_COMPONENTS_SEAMS");
            }
        }
    }

    #[test]
    fn triangles_ear_order_per_primitive() {
        let topology = ok(topology::create_owned(
            9,
            vec![0, 1, 2, 3, 4, 5, 0, 1, 6, 7, 8],
            vec![0, 6, 8, 11],
            vec![topology::PrimitiveKind::Polygon, topology::PrimitiveKind::OpenPolyline, topology::PrimitiveKind::Polygon],
        ));
        // an L hexagon (concave at point 3), a curve, then a collinear triangle
        let positions = ok(Float3::View::of_owned_exn(
            vec![0., 2., 2., 1., 1., 0., 0., 1., 2.],
            vec![0., 0., 1., 1., 2., 2., 5., 5., 5.],
            vec![0.0; 9],
        ));
        let geometry = ok(geometry::create(positions.clone(), topology.clone(), None, None, None));
        let view = ok(Float3::View::view(&positions));
        let top = ok(topology::View::view(&topology));
        let mut rows: Vec<(i64, i64, i64)> = Vec::new();
        ok(polygon_triangulation::primitive(None, &view, &top, None, 0, &mut |_, a, b, c| rows.push((a, b, c))));
        // a three-corner polygon passes through unclipped, even collinear
        rows.push((8, 9, 10));
        assert_eq!(rows.len(), 5);
        let corner = |f: &dyn Fn(&(i64, i64, i64)) -> i64| ints(&rows.iter().map(f).collect::<Vec<_>>());
        agree(
            &geometry,
            "(sop/triangles geo)",
            &Value::Record(vec![
                ("offsets".into(), ints(&[0, 4, 4, 5])),
                ("ok".into(), ints(&[1, 0, 1])),
                ("primitive".into(), ints(&[0, 0, 0, 0, 2])),
                ("a".into(), corner(&|r| r.0)),
                ("b".into(), corner(&|r| r.1)),
                ("c".into(), corner(&|r| r.2)),
            ]),
        );
    }

    #[test]
    fn missing_and_unknown_sources_refuse() {
        let refused = |lookup: attribute_kernel::GeometryLookup, source: Value, code: &str| {
            assert_eq!(attribute_kernel::resolve(None, &lookup, &source).err().unwrap().code, code);
        };
        let geometry = fixtures().into_iter().find(|(n, _)| *n == "shared-polygons").unwrap().1;
        let handle = |id| Value::Deferred(ty::geometry(), id);
        let count = |geometry: Value, element: &str| source_value("sop/count", Ty::Int, vec![("geometry".into(), geometry), ("element".into(), text(element))]);
        refused(lookup_of(None), count(handle(7), "points"), "E_DATA_SOURCE");
        refused(
            lookup_of(None),
            source_value("sop/edges", Ty::Array(Box::new(Ty::Int)), vec![("geometry".into(), Value::No_geo), ("component".into(), text("a"))]),
            "E_DATA_SOURCE",
        );
        refused(lookup_of(Some(geometry.clone())), count(handle(7), "detail"), "E_TOPOLOGY_COUNT");
        refused(
            lookup_of(Some(geometry)),
            source_value("sop/edges", Ty::Array(Box::new(Ty::Int)), vec![("geometry".into(), handle(7)), ("component".into(), text("unknown"))]),
            "E_DATA_SOURCE",
        );
    }
}

#[cfg(test)]
mod static_residual {
    use super::*;

    // A static mesh's vertex_points is a lazy residual (same id every frame): its host topology must
    // be found again, or each frame reads the whole topology back from the device (swarm: 10+ GB).
    #[test]
    fn residual_key_hits_across_frames() {
        let key = Value::Residual(value::ResidualId(9_001, 3));
        let again = Value::Residual(value::ResidualId(9_001, 3));
        let other_arena = Value::Residual(value::ResidualId(9_001, 4));
        assert!(!static_cached(&key));
        let topology = topology::create_owned(3, vec![0, 1, 2], vec![0, 3], vec![topology::PrimitiveKind::Polygon]).unwrap();
        keep_static(&key, 3, &topology);
        assert!(static_cached(&again));
        assert!(!static_cached(&other_arena));
        for i in 0..10 {
            keep_static(&Value::Residual(value::ResidualId(9_100 + i, 3)), 3, &topology);
        }
        let held = STATIC_TOPOLOGIES.lock().unwrap().iter().filter(|e| matches!(e.key, StaticKey::Residual(..))).count();
        assert!(held <= 64, "weak topology cache is bounded: {held}");
        drop(topology);
        assert!(!static_cached(&again), "the cache retained unowned topology storage");
    }
}

#[cfg(test)]
mod resident_identity {
    use super::*;

    // The device display pins a buffer by the placeholder's own `Arc` (`Arc::ptr_eq`): a copy never
    // finds it and the whole mesh falls back to the host (swarm: a 983 MB host allocation).
    #[test]
    fn resident_vec3_returns_the_placeholder_itself() {
        let placeholder = Arc::new(vec![7.0, 0.0, 0.0, 8.0, 0.0, 0.0]);
        let holder = Arc::new(ResidentPoints { value: Value::Vec3_array(placeholder.clone()), count: 2 });
        let dynamic: Arc<dyn Deferred::Holder> = holder.clone();
        let materialise: Float3::Materialise = Arc::new(|| Ok(vec![vec![7.0, 8.0], vec![0.0, 0.0], vec![0.0, 0.0]]));
        let positions = Float3::deferred(Some(dynamic), None, None, 2, materialise).unwrap();
        let topology = topology::create_owned(2, vec![0, 1], vec![0, 2], vec![topology::PrimitiveKind::OpenPolyline]).unwrap();
        let g = geometry::create(positions, topology, None, None, None).unwrap();
        let found = crate::attribute_kernel::resident_vec3(&g, "P").expect("resident positions");
        assert!(Arc::ptr_eq(&found, &placeholder));
    }
}
