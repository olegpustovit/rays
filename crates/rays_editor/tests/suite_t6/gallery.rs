//! test_gallery_shapes.ml: the gallery's Lisp graphs against independent OCaml-side inputs.
#![allow(dead_code, unused_imports)]
use flow_syntax::syntax::{self as S, Node, Syntax};
use rdk_core::packed::Float3;
use rdk_rays::rays_mesh;
use rays::scene::{self, Layer};
use rays::scene3;
use rays_math::vec3;
use sop::{context, node, session};

use super::super::support::read_repo;


fn graph(forms: &[Syntax], name: &str) -> Syntax {
    let root = &forms[0];
    let &id = root.children().iter().find(|&&id| {
        let row = root.arena().row(id);
        if let Node::List(xs) = row.node {
            let xs = root.range(xs);
            xs.len() >= 2 && matches!((root.arena().row(xs[0]).node, root.arena().row(xs[1]).node), (Node::Sym(g), Node::Sym(n)) if root.name(g) == "graph" && root.name(n) == name)
        } else { false }
    }).unwrap_or_else(|| panic!("graph {name}"));
    let form = root.child(id);
    form.child(*form.children().last().unwrap())
}

fn replace(head: &str, replacement: &Syntax, form: &Syntax) -> Syntax {
    fn go(builder: &mut S::SyntaxBuilder, head: &str, replacement: &Syntax, form: &Syntax) -> S::SyntaxId {
        if S::head(form) == Some(head) { return builder.import(replacement); }
        match form.node {
            Node::List(xs) | Node::Vec(xs) => {
                let items = form.range(xs).iter().map(|&id| go(builder, head, replacement, &form.child(id))).collect();
                let draft = if matches!(form.node, Node::List(_)) { S::Draft::List(items) } else { S::Draft::Vec(items) };
                builder.with_node(form, draft)
            },
            _ => builder.import(form),
        }
    }
    let mut builder = S::SyntaxBuilder::new();
    let id = go(&mut builder, head, replacement, form);
    builder.freeze().handle(id)
}

fn polygon(points: &[(f64, f64, f64)]) -> rdk_core::geometry::T {
    let count = points.len() as i64;
    let positions = Float3::View::of_owned_exn(points.iter().map(|p| p.0).collect(), points.iter().map(|p| p.1).collect(), points.iter().map(|p| p.2).collect()).unwrap();
    let topology = rdk_core::topology::polygons_owned(count, (0..count).collect(), vec![0, count]).unwrap();
    rdk_core::geometry::create(positions, topology, None, None, None).unwrap()
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
    let fibonacci = rdk_ops::line_geometry::points(&(0..1000).map(|i| {
        let a = i as f64 * 2.399963;
        let h = 1. - 2. * i as f64 / 999.;
        let r = (1. - h * h).max(0.).sqrt();
        (r * a.cos(), h, r * a.sin())
    }).collect::<Vec<_>>()).unwrap();
    let disc = rdk_ops::line_geometry::points(&(0..80).map(|i| {
        let a = i as f64 * 2.399963;
        let r = 1.8 * (i as f64 / 79.).sqrt();
        (r * a.cos(), r * a.sin(), 0.)
    }).collect::<Vec<_>>()).unwrap();
    let external = S::parse("(sop/ext_source)").unwrap().remove(0);
    let pair = |name: &'static str, input: &rdk_core::geometry::T, body: Syntax, head: &str| -> Fixture {
        (
            name,
            flow_sop::lisp_sop::node(&[], &flow_syntax::lisp::flat(&body)),
            flow_sop::lisp_sop::node(&[("source".to_owned(), flow_sop::lisp_sop::snapshot(input))], &flow_syntax::lisp::flat(&replace(head, &external, &body))),
        )
    };
    let mesh_form = graph(&forms, "geom_meshes");
    let mesh_body = match mesh_form.node {
        Node::List(xs) if mesh_form.range(xs).len() == 3 => {
            let mut builder = S::SyntaxBuilder::new();
            let xs = mesh_form.range(xs);
            let first = builder.import(&mesh_form.child(xs[0]));
            let second = builder.import(&mesh_form.child(xs[1]));
            let result = builder.push_draft(0, S::Draft::Sym("extruded".into()), Default::default(), None);
            let id = builder.push_draft(0, S::Draft::List(vec![first, second, result]), Default::default(), None);
            builder.freeze().handle(id)
        },
        _ => panic!("geom_meshes shape"),
    };
    let reference_star = flow_sop::lisp_sop::node(&[("source".to_owned(), flow_sop::lisp_sop::snapshot(&star))], "(sop/poly_extrude (sop/ext_source) :distance 0.65)");
    vec![
        pair("fibonacci", &fibonacci, graph(&forms, "convex_hull"), "sop/curve"),
        pair("disc", &disc, graph(&forms, "triangulate_2d"), "sop/curve"),
        ("star", flow_sop::lisp_sop::node(&[], &flow_syntax::lisp::flat(&mesh_body)), reference_star),
    ]
}

fn cook(s: &mut session::T, n: &node::T) -> rdk_core::geometry::T {
    let c = context::create(context::Options { domains: Some(1), ..Default::default() }).unwrap();
    let out = session::cook(s, None, &c, n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    sop::payload::geometry(&out.payload).unwrap()
}

fn bounds(g: &rdk_core::geometry::T) -> [f64; 6] {
    let p = Float3::View::view(rdk_core::geometry::positions(g)).unwrap();
    let range = |a: &[f64]| a.iter().fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), v| (lo.min(*v), hi.max(*v)));
    let (x, y, z) = (range(&p.x), range(&p.y), range(&p.z));
    [x.0, y.0, z.0, x.1, y.1, z.1]
}

fn scene_of(g: &rdk_core::geometry::T) -> scene3::t {
    let m = rays_mesh::to_mesh(g).unwrap();
    scene3::create(None, None, None, None, vec![scene3::mesh(Some(rays::material::unlit(rays_math::color::white())), None, None, Some(scene3::cull::Cull_none), None, m)]).unwrap()
}

fn packed(g: &rdk_core::geometry::T) -> std::rc::Rc<renderer::scene_execution::sampled_draw> {
    let camera = rays::camera::orthographic(4., vec3::create(0., 0., 4.), vec3::zero()).unwrap();
    let staged = Layer::stage_native(128, 128, &[scene::view3d(None, &camera, scene_of(g))]).unwrap();
    staged.scene3[0].entries[0].clone()
}

fn geometry_check(name: &str, actual: &rdk_core::geometry::T, expected: &rdk_core::geometry::T) {
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
        assert!((rdk_core::analysis::signed_volume(actual).unwrap() - rdk_core::analysis::signed_volume(expected).unwrap()).abs() < 1e-9);
    }
}

fn pixels(canvas: &rays::canvas::t, camera: &rays::camera::t, g: &rdk_core::geometry::T) -> Vec<rays_math::color::T> {
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
