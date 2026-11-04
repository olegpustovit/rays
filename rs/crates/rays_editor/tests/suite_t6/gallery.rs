//! test_gallery_shapes.ml: the gallery's Lisp graphs against independent OCaml-side inputs.
#![allow(dead_code, unused_imports)]
use flow_syntax::syntax::{self as S, Node, Syntax};
use rdk::packed::Float3;
use rdk_rays::rays_mesh;
use rays::scene::{self, Layer};
use rays::scene3;
use rays_math::vec3;
use sop::{context, node, session};

use crate::support::read_repo;

fn sym(s: &str) -> Syntax {
    S::make(Node::Sym(s.into()))
}

fn graph(forms: &[Syntax], name: &str) -> Syntax {
    let roots = S::children(&forms[0]);
    let form = roots
        .into_iter()
        .find(|form| match &form.node {
            Node::List(xs) => xs.len() >= 2 && matches!((&xs[0].node, &xs[1].node), (Node::Sym(g), Node::Sym(n)) if g == "graph" && n == name),
            _ => false,
        })
        .unwrap_or_else(|| panic!("graph {name}"));
    S::children(form).last().map(|s| (*s).clone()).unwrap()
}

fn replace(head: &str, replacement: &Syntax, form: &Syntax) -> Syntax {
    match &form.node {
        Node::List(xs) if matches!(xs.first().map(|x| &x.node), Some(Node::Sym(n)) if n == head) => replacement.clone(),
        Node::List(xs) => Syntax { node: Node::List(xs.iter().map(|x| replace(head, replacement, x)).collect()), ..form.clone() },
        Node::Vec(xs) => Syntax { node: Node::Vec(xs.iter().map(|x| replace(head, replacement, x)).collect()), ..form.clone() },
        _ => form.clone(),
    }
}

fn polygon(points: &[(f64, f64, f64)]) -> rdk::geometry::T {
    let count = points.len() as i64;
    let positions = Float3::View::of_owned_exn(points.iter().map(|p| p.0).collect(), points.iter().map(|p| p.1).collect(), points.iter().map(|p| p.2).collect()).unwrap();
    let topology = rdk::topology::polygons_owned(count, (0..count).collect(), vec![0, count]).unwrap();
    rdk::geometry::create(positions, topology, None, None, None).unwrap()
}

type Fixture = (&'static str, node::T, node::T);

fn fixtures() -> Vec<Fixture> {
    let text = read_repo("sketches/sop_gallery.rays");
    let forms = S::parse(&text).unwrap();
    let star = polygon(&(0..18).map(|i| {
        let a = std::f64::consts::PI * i as f64 / 9.;
        let r = if i % 2 == 0 { 1. } else { 0.45 };
        (r * a.cos(), 0., r * a.sin())
    }).collect::<Vec<_>>());
    let fibonacci = rdk::line_geometry::points(&(0..1000).map(|i| {
        let a = i as f64 * 2.399963;
        let h = 1. - 2. * i as f64 / 999.;
        let r = (1. - h * h).max(0.).sqrt();
        (r * a.cos(), h, r * a.sin())
    }).collect::<Vec<_>>()).unwrap();
    let disc = rdk::line_geometry::points(&(0..80).map(|i| {
        let a = i as f64 * 2.399963;
        let r = 1.8 * (i as f64 / 79.).sqrt();
        (r * a.cos(), r * a.sin(), 0.)
    }).collect::<Vec<_>>()).unwrap();
    let external = S::parse("(sop/ext_source)").unwrap().remove(0);
    let pair = |name: &'static str, input: &rdk::geometry::T, body: Syntax, head: &str| -> Fixture {
        (
            name,
            flow_sop::lisp_sop::node(&[], &flow_syntax::lisp::flat(&body)),
            flow_sop::lisp_sop::node(&[("source".to_owned(), flow_sop::lisp_sop::snapshot(input))], &flow_syntax::lisp::flat(&replace(head, &external, &body))),
        )
    };
    let mesh_body = match &graph(&forms, "geom_meshes").node {
        Node::List(xs) if xs.len() == 3 => S::make(Node::List(vec![xs[0].clone(), xs[1].clone(), sym("extruded")])),
        _ => panic!("geom_meshes shape"),
    };
    let reference_star = flow_sop::lisp_sop::node(&[("source".to_owned(), flow_sop::lisp_sop::snapshot(&star))], "(sop/poly_extrude (sop/ext_source) :distance 0.65)");
    vec![
        pair("fibonacci", &fibonacci, graph(&forms, "convex_hull"), "sop/curve"),
        pair("disc", &disc, graph(&forms, "triangulate_2d"), "sop/curve"),
        ("star", flow_sop::lisp_sop::node(&[], &flow_syntax::lisp::flat(&mesh_body)), reference_star),
    ]
}

fn cook(s: &mut session::T, n: &node::T) -> rdk::geometry::T {
    let c = context::create(context::Options { domains: Some(1), ..Default::default() }).unwrap();
    let out = session::cook(s, None, &c, n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    sop::payload::geometry(&out.payload).unwrap()
}

fn bounds(g: &rdk::geometry::T) -> [f64; 6] {
    let p = Float3::View::view(rdk::geometry::positions(g)).unwrap();
    let range = |a: &[f64]| a.iter().fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), v| (lo.min(*v), hi.max(*v)));
    let (x, y, z) = (range(&p.x), range(&p.y), range(&p.z));
    [x.0, y.0, z.0, x.1, y.1, z.1]
}

fn scene_of(g: &rdk::geometry::T) -> scene3::t {
    let m = rays_mesh::to_mesh(g).unwrap();
    scene3::create(None, None, None, None, vec![scene3::mesh(Some(rays::material::unlit(rays_math::color::white())), None, None, Some(scene3::cull::Cull_none), None, m)]).unwrap()
}

fn packed(g: &rdk::geometry::T) -> std::rc::Rc<renderer::scene_execution::sampled_draw> {
    let camera = rays::camera::orthographic(4., vec3::create(0., 0., 4.), vec3::zero()).unwrap();
    let staged = Layer::stage_native(128, 128, &[scene::view3d(None, &camera, scene_of(g))]).unwrap();
    staged.scene3[0].entries[0].clone()
}

fn geometry_check(name: &str, actual: &rdk::geometry::T, expected: &rdk::geometry::T) {
    let (a, e) = (bounds(actual), bounds(expected));
    for (a, e) in a.iter().zip(e.iter()) {
        assert!((a - e).abs() <= 1e-10, "{name} gallery bounds: {a:.17} <> {e:.17}");
    }
    if name != "star" {
        let (a, e) = (packed(actual), packed(expected));
        assert!(
            a.draw.mesh.vertex_count == e.draw.mesh.vertex_count
                && a.draw.mesh.index_count == e.draw.mesh.index_count
                && *a.draw.mesh.vertices.borrow() == *e.draw.mesh.vertices.borrow()
                && *a.draw.mesh.indices.borrow() == *e.draw.mesh.indices.borrow()
                && a.vertex_attributes.as_ref().map(|v| v.1.borrow().clone()) == e.vertex_attributes.as_ref().map(|v| v.1.borrow().clone())
                && a.draw.state.transform_uniforms.as_ref().map(|v| v.borrow().clone()) == e.draw.state.transform_uniforms.as_ref().map(|v| v.borrow().clone()),
            "{name} gallery packed GPU inputs differ"
        );
    } else {
        assert!((rdk::analysis::signed_volume(actual).unwrap() - rdk::analysis::signed_volume(expected).unwrap()).abs() < 1e-9);
    }
}

fn pixels(canvas: &rays::canvas::t, camera: &rays::camera::t, g: &rdk::geometry::T) -> Vec<rays_math::color::T> {
    rays::canvas::render(None, canvas, &[scene::clear(rays_math::color::black()), scene::view3d(None, camera, scene_of(g))]).unwrap();
    rays::canvas::pixels(canvas).unwrap()
}

fn run(native: bool) {
    let mut s = session::create(64, 64_000_000).unwrap();
    for (name, actual, expected) in fixtures() {
        let (actual, expected) = (cook(&mut s, &actual), cook(&mut s, &expected));
        geometry_check(name, &actual, &expected);
        if native {
            let [x0, y0, z0, x1, y1, z1] = bounds(&expected);
            let size = (x1 - x0).max((y1 - y0).max(z1 - z0));
            let center = vec3::create((x0 + x1) / 2., (y0 + y1) / 2., (z0 + z1) / 2.);
            let canvas = rays::canvas::create(128, 128).unwrap();
            for (x, y, z) in [(3., 2., 4.), (-3., 2., 4.), (0., 3., 4.), (0., -3., 4.)] {
                let camera = rays::camera::orthographic(size * 3., vec3::add(center, vec3::create(x * size, y * size, z * size)), center).unwrap();
                let e = pixels(&canvas, &camera, &expected);
                let a = pixels(&canvas, &camera, &actual);
                assert!(a.len() == 128 * 128 && e == a, "{name}: native pixels differ");
                let visible = a.iter().filter(|c| c.r > 0).count();
                assert!(visible > 100 && visible < a.len() / 2, "{name}: {visible} visible pixels");
            }
            let _ = rays::canvas::destroy(&canvas);
        }
    }
    session::close(&mut s);
}

pub fn gallery_shapes() {
    run(false);
}

pub fn gallery_shapes_native() {
    if !crate::common::gpu() {
        return;
    }
    run(true);
}
