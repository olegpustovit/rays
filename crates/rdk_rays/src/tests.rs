//! Bridge tests cover surface conversion failures, quad/extrusion topology, piece rendering and geometry inspection.
use std::sync::Arc;

use param::{FieldView, Impact, KindView, Value};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::packed::{self, deferred};
use rdk_core::{geometry, topology};
use rdk_ops::poly_extrude;

use crate::geometry_view;
use crate::packed_pieces::{self, Explosion};
use crate::rays_mesh;
use crate::surface;

struct Fixture;
impl deferred::Holder for Fixture {}

fn holder() -> Option<Arc<dyn deferred::Holder>> {
    Some(Arc::new(Fixture))
}

#[test]
fn animated_device_surfaces_share_unchanged_topology() {
    use std::rc::Rc;
    let topo=topology::polygons_owned(3,vec![0,1,2],vec![0,3]).unwrap();
    let corners=topology::view::view(&topo).unwrap().vertex_points;
    let camera=rays::camera::perspective(None,None,None,rays_math::vec3::create(0.,0.,3.),rays_math::vec3::zero()).unwrap();
    let mut previous=None;
    for frame in 0..3 {
        let positions=packed::float3::deferred(holder(),None,None,3,Arc::new(||panic!("device preview read positions back"))).unwrap();
        let geometry=geometry::create(positions,topo.clone(),None,None,None).unwrap();
        let surface=surface::of_geometry(None,&geometry).unwrap();
        let device=|_:&geometry::T|Some((format!("animated-{frame}"),false,None,0,Rc::new(||{}) as Rc<dyn Fn()>));
        let scene=surface::scene3_result(None,Some(&device),&surface).unwrap();
        let drawing=rays::scene3::walk::drawings(&scene).remove(0);
        assert!(Arc::ptr_eq(&rays::mesh::view::unread(&drawing.mesh).indices,&corners),"a new position stream copied the topology");
        let prepared=rays::scene::layer::stage_native(32,32,&[rays::scene::view3d(None,&camera,scene)]).unwrap();
        let indices=prepared.scene3[0].entries[0].draw.mesh.indices.clone();
        if let Some(previous)=previous { assert!(Rc::ptr_eq(&indices,&previous),"unchanged topology was packed again"); }
        previous=Some(indices);
    }
}

#[test]
fn surface_failure() {
    let x = vec![0.0, 1.0, 0.0, 1.0];
    let y = vec![0.0, 1.0, 1.0, 0.0];
    let z = vec![0.0; 4];
    let positions = packed::float3::of_owned(x.clone(), y.clone(), z.clone()).unwrap();
    let deferred_positions = || {
        let planes = vec![x.clone(), y.clone(), z.clone()];
        packed::float3::deferred(holder(), None, None, 4, Arc::new(move || Ok(planes.clone()))).unwrap()
    };
    let uv = attribute::create_owned(
        "uv",
        Owner::Point,
        Storage::Float2(packed::float2::of_owned(vec![0.0; 4], vec![0.0; 4]).unwrap()),
    )
    .unwrap();
    let points = geometry::create(deferred_positions(), topology::empty(4).unwrap(), Some(vec![uv]), None, None).unwrap();
    let host = surface::of_geometry(None, &points).unwrap();
    assert_eq!(
        surface::host_reason(&host),
        Some("Attribute uv requires the host bridge.".to_owned()),
        "unsupported surface did not record its host reason"
    );
    let empty_colors = Storage::Float3(packed::float3::of_owned(vec![], vec![], vec![]).unwrap());
    let points = geometry::with_attribute(attribute::create_owned("Cd", Owner::Primitive, empty_colors.clone()).unwrap(), &points).unwrap();
    let points = geometry::with_attribute(attribute::create_owned("N", Owner::Vertex, empty_colors).unwrap(), &points).unwrap();
    let mesh = rays_mesh::to_mesh(&points).unwrap();
    assert_eq!(rays::mesh::vertex_count(&mesh), 4, "empty corner attributes broke a point-cloud host reader");

    let topo = topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap();
    let owned = geometry::create(positions.clone(), topo.clone(), None, None, None).unwrap();
    let expected = match surface::of_geometry(None, &owned) {
        Err(message) => message,
        Ok(_) => panic!("bow-tie surface was accepted"),
    };
    let like = Storage::Float3(positions.clone());
    let normals_source = positions;
    let normals = attribute::create_deferred(
        None,
        None,
        "N",
        Owner::Point,
        &like,
        4,
        Arc::new(move || Storage::Float3(normals_source.clone())),
    )
    .unwrap();
    let deferred_topology = topology::deferred(None, holder(), None, 4, 4, 1, Arc::new(move || Ok(topo.clone()))).unwrap();
    let deferred = geometry::create(deferred_positions(), deferred_topology, Some(vec![normals]), None, None).unwrap();
    let prepared = surface::of_geometry(None, &deferred).unwrap();
    assert_eq!(surface::host_reason(&prepared), None, "resident surface was refused");
    assert!(packed::float3::resident(geometry::positions(&deferred)), "surface preparation forced deferred P");
    assert_eq!(
        surface::scene3_result(None, None, &prepared).err(),
        Some(expected.clone()),
        "deferred surface fallback lost the owned triangulation error"
    );
    match surface::scene3(None, None, &prepared) {
        Err(error) => assert_eq!(error.0, expected, "surface convenience error changed its cause"),
        Ok(_) => panic!("invalid deferred surface silently drew an empty mesh"),
    }
}

fn quad() -> geometry::T {
    let mut builder = packed::float3::builder::create(4).unwrap();
    for (i, (x, y)) in [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)].iter().enumerate() {
        packed::float3::builder::set(&mut builder, i as i64, *x, *y, 0.0).unwrap();
    }
    let positions = packed::float3::builder::freeze(&mut builder).unwrap();
    let topo = topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap();
    geometry::create(positions, topo, None, None, None).unwrap()
}

#[test]
fn quad_and_extrude_bridge() {
    let g = quad();
    let mesh = rays_mesh::to_mesh(&g).unwrap();
    assert_eq!(rays::mesh::index_count(&mesh), 6, "quad bridge triangulation");
    let extruded = poly_extrude::run(&poly_extrude::Params::new(2.0), &g).unwrap();
    assert_eq!(
        (geometry::point_count(&extruded), geometry::primitive_count(&extruded), geometry::vertex_count(&extruded)),
        (8, 6, 24),
        "poly extrude cardinality"
    );
    assert_eq!(rays::mesh::index_count(&rays_mesh::to_mesh(&extruded).unwrap()), 36, "poly extrude bridge");
}

#[test]
fn mesh_round_trip_and_trace_mesh() {
    let g = quad();
    let mesh = rays_mesh::to_mesh(&g).unwrap();
    let back = rays_mesh::of_mesh(&mesh).unwrap();
    assert_eq!((geometry::point_count(&back), geometry::primitive_count(&back)), (4, 2));
    let trace = rays_mesh::to_trace_mesh(&g).unwrap();
    let view = rays::mesh::view::packed_view(&trace).unwrap();
    assert!(view.normals.is_some(), "trace mesh has smoothed normals");
    assert_eq!(view.indices.len(), 6);
    let lines = rays_mesh::of_mesh(&geometry_view::wire_mesh(&g).unwrap());
    assert!(lines.is_err(), "of_mesh refuses a line mesh");
}

// ---- Packed_pieces ----

fn two_pieces() -> geometry::T {
    let positions = packed::float3::view::of_owned_exn(
        vec![-2.5, -1.5, -2.0, 1.5, 2.5, 2.0],
        vec![-0.5, -0.5, 0.5, -0.5, -0.5, 0.5],
        vec![0.0; 6],
    )
    .unwrap();
    let topo = topology::polygons_owned(6, vec![0, 1, 2, 3, 4, 5], vec![0, 3, 6]).unwrap();
    let g = geometry::create(positions, topo, None, None, None).unwrap();
    let normals = attribute::create_owned(
        "N",
        Owner::Vertex,
        Storage::Float3(packed::float3::view::of_owned_exn(vec![0.0; 6], vec![1.0; 6], vec![0.0; 6]).unwrap()),
    )
    .unwrap();
    let g = geometry::with_attribute(normals, &g).unwrap();
    let pieces = attribute::create_owned("class", Owner::Primitive, Storage::Int(Arc::new(vec![0, 1]))).unwrap();
    geometry::with_attribute(pieces, &g).unwrap()
}

fn field(name: &str, current: Value) -> FieldView {
    FieldView {
        name: name.to_owned(),
        label: name.to_owned(),
        description: None,
        folder: vec![],
        impact: Impact::Cook,
        primary: false,
        unit: None,
        vec3: None,
        kind: KindView::Text,
        default: current.clone(),
        current,
    }
}

fn explosion_of(amount: f64, scale_x: f64, noise: Option<(f64, f64, i64)>) -> Option<Explosion> {
    let mut fields = vec![field("amount", Value::Float(amount)), field("scale_x", Value::Float(scale_x)), field("piece_attribute", Value::Text(b"class".as_slice().into()))];
    if let Some((a, f, s)) = noise {
        fields.push(field("noise_amount", Value::Float(a)));
        fields.push(field("noise_frequency", Value::Float(f)));
        fields.push(field("noise_seed", Value::Int(s)));
    }
    packed_pieces::explosion("exploded_view", &fields)
}

fn xs(mesh: &rays::mesh::Mesh) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let v = rays::mesh::view::packed_view(mesh).unwrap().vertices;
    (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
}

#[test]
fn packed_pieces_explosion() {
    let pieces = packed_pieces::of_geometry("class", &two_pieces()).unwrap();
    assert_eq!(packed_pieces::piece_count(&pieces), 2, "did not preserve two connected pieces");
    assert!(packed_pieces::explosion("box", &[field("amount", Value::Float(1.0))]).is_none(), "a node that is not an Exploded View carried an explosion");
    let exploded = |e: Option<Explosion>| packed_pieces::mesh_for_explosion(e.as_ref(), &pieces).unwrap();
    let base = xs(&exploded(explosion_of(0.0, 1.0, None)));
    let moved = xs(&exploded(explosion_of(1.0, 1.0, None)));
    assert_eq!(xs(&packed_pieces::mesh_for_explosion(None, &pieces).unwrap()), base, "a zero explosion moved the pieces");
    let normals = rays::mesh::view::packed_view(&exploded(explosion_of(0.0, 1.0, None))).unwrap().normals.unwrap();
    assert!(
        normals.x.iter().all(|v| *v == 0.0) && normals.y.iter().all(|v| *v == 1.0) && normals.z.iter().all(|v| *v == 0.0),
        "replaced authored vertex normals with flat faces"
    );
    assert_eq!(base.0.len(), 6, "changed the vertex count");
    for vertex in 0..6 {
        let expected = if base.0[vertex] < 0.0 { -2.0 } else { 2.0 };
        assert!((moved.0[vertex] - base.0[vertex] - expected).abs() <= 1e-12, "explosion was not a rigid center translation");
        assert!(moved.1[vertex] == base.1[vertex] && moved.2[vertex] == base.2[vertex], "explosion changed an orthogonal coordinate");
    }
    // The per-axis scale multiplies the offset: half as far along X.
    let half = xs(&exploded(explosion_of(1.0, 0.5, None)));
    for (vertex, x) in base.0.iter().enumerate() {
        let expected = if *x < 0.0 { -1.0 } else { 1.0 };
        assert!((half.0[vertex] - x - expected).abs() <= 1e-12, "ignored the explosion's axis scale");
    }
    let noisy = |seed| xs(&exploded(explosion_of(0.7, 1.0, Some((0.4, 1.3, seed)))));
    let (a, b) = (noisy(19), noisy(19));
    assert!(a.0 == b.0 && a.1 == b.1 && a.2 == b.2, "noise is not deterministic");
    let quiet = xs(&exploded(explosion_of(0.7, 1.0, None)));
    assert!(!(a.0 == quiet.0 && a.1 == quiet.1 && a.2 == quiet.2), "noise moved nothing");
}

#[test]
fn packed_pieces_refusals() {
    let g = two_pieces();
    assert!(packed_pieces::of_geometry(" ", &g).err().unwrap().contains("empty piece attribute"));
    assert!(packed_pieces::of_geometry("missing", &g).err().unwrap().contains("missing primitive piece attribute"));
    let pieces = packed_pieces::of_geometry("class", &g).unwrap();
    let bad = Explosion { amount: f64::NAN, ..explosion_of(1.0, 1.0, None).unwrap() };
    assert!(packed_pieces::mesh_for_explosion(Some(&bad), &pieces).is_err());
}

// ---- Surface ----

#[test]
fn surface_materials_and_pieces_draw() {
    // two triangles with distinct material colours split into two batches sharing one mesh
    let g = two_pieces();
    let colors = attribute::create_owned(
        "material_color",
        Owner::Primitive,
        Storage::Float4(packed::float4::of_owned(vec![1.0, 0.0], vec![0.0, 1.0], vec![0.0, 0.0], vec![1.0, 1.0]).unwrap()),
    )
    .unwrap();
    let g = geometry::with_attribute(colors, &g).unwrap();
    let prepared = surface::of_geometry(None, &g).unwrap();
    assert_eq!(surface::host_reason(&prepared), Some("Positions are on the CPU.".to_owned()));
    assert!(surface::scene3(None, None, &prepared).is_ok());
    let bad = attribute::create_owned("material_roughness", Owner::Primitive, Storage::Float(Arc::new(vec![2.0, 0.0]))).unwrap();
    let g = geometry::with_attribute(bad, &g).unwrap();
    assert_eq!(surface::of_geometry(None, &g).err(), Some("material channels must be finite and in [0,1]".to_owned()));
}

// ---- Geometry_view ----

#[test]
fn geometry_view_reads() {
    let g = two_pieces();
    let s = geometry_view::summary(&g);
    assert_eq!((s.points, s.prims), (6, 2));
    assert_eq!(s.attributes[0].0, "vertex");
    assert_eq!(geometry_view::triangles(&g), 2);
    assert_eq!(geometry_view::count(2, &g), 2);
    let (lo, hi) = geometry_view::bounds(&g).unwrap();
    assert_eq!((lo.x, hi.x), (-2.5, 2.5));
    let columns = geometry_view::columns(0, &g);
    let names: Vec<&str> = columns.iter().map(|c| c.0.as_str()).collect();
    assert_eq!(names, ["index", "P.x", "P.y", "P.z"]);
    assert_eq!((columns[1].1)(0), "-2.5");
    let prims = geometry_view::columns(2, &g);
    assert_eq!((prims[1].0.as_str(), (prims[1].1)(1).as_str()), ("class", "1"));
    assert_eq!(geometry_view::tags("class", &g), Some(vec![0, 1]));
    let tinted = geometry_view::tint("class", (1.0, 0.0, 0.0), &|tag| tag == 1, &g);
    let cd = geometry::find_attribute(Owner::Vertex, "Cd", &tinted).expect("tint writes Cd");
    let (r, gr, _, a) = match attribute::storage(&cd).unwrap() {
        Storage::Float4(c) => packed::float4::get(&c, 3).unwrap(),
        _ => panic!("Cd is float4"),
    };
    assert!((r - 1.0 * 0.4 - 0.6).abs() < 1e-12 && (gr - 0.4).abs() < 1e-12 && a == 1.0);
    let dim = match attribute::storage(&cd).unwrap() {
        Storage::Float4(c) => packed::float4::get(&c, 0).unwrap(),
        _ => unreachable!(),
    };
    assert!((dim.0 - 0.3).abs() < 1e-12);
    let surface = geometry_view::surface(&g);
    let hit = geometry_view::cast("class", surface.as_ref(), &g, rays_math::vec3::create(2.0, 0.0, 5.0), rays_math::vec3::create(0.0, 0.0, -1.0));
    assert!(hit.is_some());
    assert_eq!(hit.unwrap().1, 1);
    assert!(geometry_view::points_of_nothing().is_ok());
}

#[test]
fn geometry_view_keeps_opaque_bytes_and_validates_material_identifiers() {
    use rays_math::vec3;
    let bytes: Vec<u8> = (0..=255).collect();
    let g = geometry::with_attribute(attribute::create_owned("tag", Owner::Primitive, Storage::Text(std::sync::Arc::new(vec![bytes.clone(), "猫".as_bytes().to_vec()].into()))).unwrap(), &two_pieces()).unwrap();
    let columns = geometry_view::columns(2, &g);
    let (_, cell) = columns.iter().find(|(name, _)| name == "tag").unwrap();
    assert_eq!(cell(0), rdk_core::attribute::quote_bytes((&bytes).as_ref()));
    assert_eq!(cell(1), "猫");
    let surface = geometry_view::surface(&g);
    let origin = vec3::create(2.0, 0.0, 5.0);
    let direction = vec3::create(0.0, 0.0, -1.0);
    let paths = |path| geometry::with_attribute(attribute::create_owned("shop_materialpath", Owner::Primitive, Storage::Text(std::sync::Arc::new(vec![Vec::new(), path].into()))).unwrap(), &g).unwrap();
    assert_eq!(geometry_view::cast_material(surface.as_ref(), &paths("猫".as_bytes().to_vec()), origin, direction).unwrap().1, Some("猫".to_owned()));
    assert!(geometry_view::cast_material(surface.as_ref(), &paths(bytes), origin, direction).is_none());
    let fields = vec![field("amount", Value::Float(1.0)), field("piece_attribute", Value::Text(vec![255].into()))];
    assert!(packed_pieces::explosion("exploded_view", &fields).is_none());
}

/// Remaining checks of `run` in : packed group cardinality and difference, and
/// the extrude's point-group remap, canonical-P refusal and undersized-polygon refusal.
#[test]
fn groups_extrude_remap_and_construction_refusals() {
    use rdk_core::group;
    let even = group::init(None, group::Owner::Point, "even", 4, &|i| Ok(i & 1 == 0)).unwrap();
    let all = group::init(None, group::Owner::Point, "all", 4, &|_| Ok(true)).unwrap();
    let odd = group::difference(&all, &even).unwrap();
    assert!(group::cardinality(&odd) == 2 && !group::mem(0, &odd) && group::mem(1, &odd), "packed group difference");
    assert_eq!(group::cardinality(&even), 2, "packed group cardinality");
    let base = quad();
    let source = geometry::create(geometry::positions(&base).clone(), geometry::topology(&base).clone(), None, Some(vec![even]), None).unwrap();
    let extruded = poly_extrude::run(&poly_extrude::Params::new(2.0), &source).unwrap();
    match geometry::find_group(group::Owner::Point, "even", &extruded) {
        Some(found) if group::cardinality(&found) == 4 => {}
        _ => panic!("poly extrude point group remap"),
    }
    let positions = geometry::positions(&base).clone();
    let reserved = attribute::create_owned("P", Owner::Point, Storage::Float3(positions.clone())).unwrap();
    assert!(
        geometry::create(positions, geometry::topology(&base).clone(), Some(vec![reserved]), None, None).is_err(),
        "duplicate canonical P was accepted"
    );
    assert!(topology::polygons_owned(2, vec![0, 1], vec![0, 2]).is_err(), "undersized polygon was accepted");
}
