use crate::camera;
use crate::mesh;
use crate::scene::{self, Node, layer};
use crate::scene3 as d3;
use rays_math::color;
use rays_math::vec3;
use scene_command::render_ir;
use std::rc::Rc;

fn require(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

fn serialize(scene: &[Node]) -> Vec<u8> {
    let ir = layer::to_ir(scene).expect("to_ir");
    render_ir::serialize(&ir)
}

/// The cached geometry nodes a constructor returned: identity of the immutable arrays is the
/// identity of their `Rc<Geometry>` (the IR copies them into commands).
fn geometry_rcs(n: &Node) -> Vec<Rc<render_ir::Geometry>> {
    match n {
        Node::Geometry(g) => vec![g.clone()],
        Node::Group(children) | Node::Translate(_, _, children) => children.iter().flat_map(geometry_rcs).collect(),
        _ => Vec::new(),
    }
}

fn first_geometry(n: &Node) -> Rc<render_ir::Geometry> {
    // the IR must also stage (the constructor's output is a valid scene)
    let _ = serialize(std::slice::from_ref(n));
    geometry_rcs(n).first().cloned().expect("geometry")
}

fn require_geometry_reuse(label: &str, make: &dyn Fn() -> Node) {
    let first = first_geometry(&make());
    let second = first_geometry(&make());
    require(Rc::ptr_eq(&first, &second), &format!("{label} rebuilt tessellated geometry arrays"));
}

/// a window presents only a picture that is not the same as the last one
fn same_picture() {
    let picture = |label: &str| vec![scene::clear(color::black()), scene::translate(4, 8, vec![scene::text((1, 2), None, None, label)])];
    require(layer::same(&picture("a"), &picture("a")), "an equal picture rebuilt counts as changed");
    require(!layer::same(&picture("a"), &picture("b")), "a changed label counts as the same picture");
}

/// a 3D view is the same picture when its meshes are the same arrays, decided without walking them
fn same_view3d() {
    let view = |m: &mesh::Mesh| {
        let cam = camera::orthographic(2., vec3::zero(), vec3::unit_z()).expect("camera");
        vec![scene::view3d(None, &cam, d3::create(None, None, None, None, vec![d3::mesh(None, None, None, None, None, m.clone())]).expect("scene3"))]
    };
    let plane = mesh::plane(1., 1.).expect("plane");
    require(layer::same(&view(&plane), &view(&plane)), "a view of the same mesh counts as changed");
    require(!layer::same(&view(&plane), &view(&mesh::plane(1., 1.).expect("plane"))), "a view of a rebuilt mesh counts as the same picture");
}

#[test]
fn rounded_and_path_caches_are_deterministic_and_bounded() {
    let context = layer::create_cache_context();
    layer::with_cache_context(&context, &mut || {
        same_picture();
        same_view3d();
        let make = |x: i64, y: i64| {
            vec![scene::rounded_rect((x, y), 84, 31, 7, Some(color::rgba(12, 34, 56, 220)), Some(color::rgb(90, 80, 70))).expect("rounded_rect")]
        };
        let baseline = serialize(&make(17, 23));
        require(baseline == serialize(&make(17, 23)), "cached rounded geometry changed IR");
        require(baseline != serialize(&make(18, 23)), "translation was omitted from cache key lowering");
        for index in 0..300 {
            serialize(&[scene::rounded_rect((index & 7, index & 3), 20 + index, 31, 7, Some(color::red()), None).expect("rounded_rect")]);
        }
        require(baseline == serialize(&make(17, 23)), "bounded eviction changed rounded geometry");
        let primitive = || scene::circle((44, 37), 19, Some(color::rgba(11, 22, 33, 210)), None).expect("circle");
        let first = first_geometry(&primitive());
        let second = first_geometry(&primitive());
        require(Rc::ptr_eq(&first, &second), "structurally identical primitive rebuilt geometry arrays");
        let primitive_baseline = serialize(&[primitive()]);
        for index in 0..300 {
            serialize(&[scene::rect((index, 0), index + 1, 3, Some(color::rgba(index & 255, 20, 30, 255)), None).expect("rect")]);
        }
        require(primitive_baseline == serialize(&[primitive()]), "bounded primitive cache eviction changed geometry");
        let ellipse_first = primitive();
        let ellipse_second = primitive();
        let same_group = match (&ellipse_first, &ellipse_second) {
            (Node::Group(a), Node::Group(b)) => Rc::ptr_eq(a, b) || (a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| matches!((x, y), (Node::Geometry(p), Node::Geometry(q)) if Rc::ptr_eq(p, q)))),
            _ => false,
        };
        require(same_group, "stable ellipse did not reuse its immutable scene node");
        for index in 0..300 {
            let _ = scene::circle((index, index & 15), index + 2, Some(color::rgb(0, 0, 255)), None).expect("circle");
        }
        require(primitive_baseline == serialize(&[primitive()]), "bounded ellipse cache eviction changed geometry");
        require_geometry_reuse("polygon", &|| {
            scene::polygon(&[(11, 7), (83, 13), (72, 61), (19, 55)], Some(color::rgba(17, 41, 89, 213)), Some(color::rgb(220, 170, 90))).expect("polygon")
        });
        require_geometry_reuse("rect", &|| scene::rect((13, 17), 71, 29, Some(color::rgba(27, 51, 93, 207)), Some(color::rgb(201, 151, 71))).expect("rect"));
        require_geometry_reuse("line", &|| scene::line((7, 9), (93, 67), Some(color::rgba(61, 103, 149, 219)), Some(5)).expect("line"));
        let native_paths = || {
            vec![
                scene::rect((8, 12), 63, 37, Some(color::rgba(19, 47, 83, 229)), Some(color::rgb(191, 157, 113))).expect("rect"),
                scene::polygon(&[(17, 71), (41, 19), (89, 27), (101, 78), (53, 91)], Some(color::rgba(13, 79, 127, 197)), None).expect("polygon"),
                scene::line((3, 97), (117, 43), Some(color::rgba(149, 83, 31, 221)), Some(7)).expect("line"),
            ]
        };
        let native_baseline = serialize(&native_paths());
        require(native_baseline == serialize(&native_paths()), "cached native path geometry changed exact serialized IR");
        require(
            serialize(&[scene::line((3, 97), (117, 43), Some(color::rgba(149, 83, 31, 221)), Some(6)).expect("line")])
                != serialize(&[scene::line((3, 97), (117, 43), Some(color::rgba(149, 83, 31, 221)), Some(7)).expect("line")]),
            "native path cache omitted stroke width from its semantic key",
        );
        require(
            serialize(&[scene::polygon(&[(4, 5), (31, 7), (29, 37)], Some(color::rgb(20, 40, 60)), None).expect("polygon")])
                != serialize(&[scene::polygon(&[(4, 5), (31, 7), (29, 37)], Some(color::rgb(21, 40, 60)), None).expect("polygon")]),
            "native path cache omitted packed fill from its semantic key",
        );
        let evicting = || scene::polygon(&[(-71, -53), (-19, -47), (-23, -11)], Some(color::rgba(23, 59, 101, 227)), None).expect("polygon");
        let evicted_before = first_geometry(&evicting());
        for index in 0..300 {
            let _ = scene::polygon(&[(index, 0), (index + 17, 3), (index + 11, 29)], Some(color::rgba(index & 255, 37, 73, 211)), None).expect("polygon");
        }
        let evicted_after = first_geometry(&evicting());
        require(!Rc::ptr_eq(&evicted_before, &evicted_after), "native path cache did not deterministically evict its oldest geometry");
        require(native_baseline == serialize(&native_paths()), "native path cache eviction changed exact serialized IR");
    });
}
