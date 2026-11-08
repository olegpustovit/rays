//! test_terminal_boolean_normals.ml and test_custom_sop.ml.
#![allow(dead_code, unused_imports)]
use std::sync::Arc;

use param::Value;
use rdk::packed::Float3;
use sop::node::{self, CookMode};
use sop::{context, custom, edit_graph, session};

use super::super::support::*;

fn tri() -> rdk::geometry::T {
    let positions = Float3::View::of_owned_exn(vec![-0.5, 0.5, 0.], vec![-0.5, -0.5, 0.5], vec![0., 0., 0.]).unwrap();
    let topology = rdk::topology::polygons_owned(3, vec![0, 2, 1], vec![0, 3]).unwrap();
    let values = Float3::View::of_owned_exn(vec![0.; 3], vec![0.; 3], vec![-1.; 3]).unwrap();
    let normal = rdk::attribute::create_owned("N", rdk::attribute::Owner::Vertex, rdk::attribute::Storage::Float3(values)).unwrap();
    rdk::geometry::create(positions, topology, Some(vec![normal]), None, None).unwrap()
}

fn mesh(g: &rdk::geometry::T) -> Result<rays::mesh::t, String> {
    rdk_rays::rays_mesh::to_mesh(g).map_err(|e| format!("{e:?}"))
}

type Snap = (Vec<i64>, Vec<f64>, Vec<f64>, Vec<f64>);

fn snapshot(domains: i64) -> Snap {
    let out = std::cell::RefCell::new(None);
    parallel::run_unit(Some(domains), &|| {
        let m = mesh(&tri()).unwrap();
        let view = rays::mesh::View::packed_view(&m).unwrap();
        let n = view.normals.as_ref().expect("normals");
        assert!(n.x.as_slice() == [0., 0., 0.] && n.y.as_slice() == [0., 0., 0.] && n.z.as_slice() == [-1., -1., -1.], "reversed authored N was overwritten");
        assert!(view.indices.as_slice() == [0, 1, 2], "terminal mesh reordered its one triangle");
        // Render vertices follow the primitive's vertex order: points 0, 2, 1.
        assert!(view.vertices.x.as_slice() == [-0.5, 0., 0.5] && view.vertices.y.as_slice() == [-0.5, 0.5, -0.5], "terminal mesh rewound the reversed triangle");
        *out.borrow_mut() = Some(((*view.indices).clone(), (*n.x).clone(), (*n.y).clone(), (*n.z).clone()));
        Ok(())
    })
    .unwrap();
    out.into_inner().unwrap()
}

pub fn terminal_normals() {
    let one = snapshot(1);
    assert!(one == snapshot(4), "terminal normal domain drift");
    let workers: Vec<_> = (0..4).map(|_| std::thread::spawn(|| snapshot(4))).collect();
    for w in workers {
        assert!(w.join().unwrap() == one, "terminal normal worker drift");
    }
    let source = tri();
    let bad = rdk::attribute::create_owned("N", rdk::attribute::Owner::Vertex, rdk::attribute::Storage::Float2(rdk::packed::Float2::of_owned(vec![0.; 3], vec![0.; 3]).unwrap())).unwrap();
    let source = rdk::geometry::without_attribute(rdk::attribute::Owner::Vertex, "N", &source);
    let source = rdk::geometry::with_attribute(bad, &source).unwrap();
    assert!(mesh(&source).is_err(), "malformed terminal N was accepted");
}

// ---- test_custom_sop.ml

fn controls_schema() -> Arc<param::Schema> {
    let tx = param::field("translate_x".into(), param::floating(-5., 5., None, None).unwrap(), Value::Float(0.)).unwrap().label("Translate X".into()).unwrap().folder(vec!["Transform".into()]).unwrap();
    let ts = param::field("time_scale".into(), param::floating(-2., 2., None, None).unwrap(), Value::Float(0.)).unwrap().label("Time scale".into()).unwrap().folder(vec!["Animation".into()]).unwrap();
    Arc::new(param::schema("controls".into(), vec![tx, ts]).unwrap())
}

fn float_of(v: &Value) -> f64 {
    match v {
        Value::Float(f) => *f,
        other => panic!("{other:?}"),
    }
}

fn x_of(g: &rdk::geometry::T) -> f64 {
    Float3::get(rdk::geometry::positions(g), 0).unwrap().0
}

fn cook(s: &mut session::T, c: &context::T, n: &node::T) -> rdk::geometry::T {
    match session::cook(s, None, c, n) {
        Ok(out) => sop::payload::geometry(&out.payload).unwrap(),
        Err(e) => panic!("{}", rays_diagnostic::error_to_string(&e)),
    }
}

pub fn custom_sop() {
    let input = flow_sop::lisp_sop::snapshot(&rdk::line_geometry::points(&[(0., 0., 0.)]).unwrap());
    let dependencies = context::dependencies::one(context::dependencies::Fact::Time);
    let schema = controls_schema();
    let key_schema = schema.clone();
    let node = custom::node(
        Some("animated wrangle"),
        "animated_translate",
        schema.clone(),
        vec![Value::Float(1.), Value::Float(2.)],
        vec![input],
        Arc::new(move |label, inputs, parameters| {
            let params = parameters.to_vec();
            Ok(custom::plain(
                custom::PlainArgs { label: Some(label.to_owned()), version: 2, parameters: key_schema.key(parameters)?, cook_mode: CookMode::DuplicateInput(0), dependencies, operation: "animated_translate".into() },
                inputs.to_vec(),
                Arc::new(move |context, geometries| {
                    if geometries.len() != 1 {
                        return Err("expected one input".into());
                    }
                    let offset = float_of(&params[0]) + float_of(&params[1]) * context::time(context);
                    let p = rdk::geometry::positions(&geometries[0]);
                    let n = Float3::length(p);
                    let (mut xs, mut ys, mut zs) = (vec![], vec![], vec![]);
                    for i in 0..n {
                        let (x, y, z) = Float3::get(p, i).unwrap();
                        xs.push(x + offset);
                        ys.push(y);
                        zs.push(z);
                    }
                    let positions = Float3::of_owned(xs, ys, zs).map_err(|e| format!("{e:?}"))?;
                    rdk::geometry::with_positions(positions, &geometries[0]).map_err(|e| format!("{e:?}"))
                }),
            ))
        }),
    )
    .unwrap();
    assert!(node::parameter_fields(&node).len() == 2 && node::has_parameters(&node) && context::dependencies::mem(context::dependencies::Fact::Time, node::dependencies(&node)), "custom SOP did not own PPX parameters or dependencies");
    let mut s = session::create(8, 1_000_000).unwrap();
    let ctx = |t: f64| context::create(context::Options { time: Some(t), ..Default::default() }).unwrap();
    assert!(x_of(&cook(&mut s, &ctx(3.), &node)) == 7., "custom SOP did not receive parameters and timeline context");
    let before_key = node::parameters(&node).to_vec();
    let node_id = node::id(&node);
    let (document, effects) = edit_graph::apply_parameters(&edit_graph::of_graph(&node), node_id, &[("translate_x".into(), Value::Float(4.))]).unwrap();
    let edited = edit_graph::compile(&document).unwrap();
    assert!(effects.cook && node::id(&edited) == node_id && node::parameters(&edited) != before_key.as_slice(), "custom SOP edit lost stable identity or cache-key invalidation");
    assert!(x_of(&cook(&mut s, &ctx(3.), &edited)) == 10., "custom SOP edit did not rebuild its cook closure");
    let fields = node::parameter_fields(&edited);
    let translated = fields.iter().find(|f| f.name == "translate_x").unwrap();
    assert!(translated.current == Value::Float(4.), "custom SOP inspector metadata did not retain edited values");
    session::close(&mut s);
    let mut factories = vec![edit_graph::factory(
        edit_graph::factory_args("my_custom_sop", "My Custom SOP", &["Custom"]),
        0,
        Arc::new(|inputs| if inputs.is_empty() { Ok(flow_sop::lisp_sop::snapshot(&rdk::line_geometry::points(&[(0., 0., 0.)]).unwrap())) } else { Err("none".into()) }),
    )];
    factories.extend(sop::nodes::factories());
    let text = "(workspace test_custom
    (graph scene :context scene
      (let* [cam (scene/camera :eye [1 2 3] :target [0 0 0])
             geom (scene/geometry (ref g))]
        (scene/merge cam geom)))
    (graph g :context sop
      (sop/my_custom_sop)))";
    let doc = rays_editor::Workspace::load(None, Vec::new(), Some(&factories), text).unwrap_or_else(|ds| panic!("{}", diag_text(&ds)));
    let base = rays::easy_camera::create(Some(rays_math::vec3::zero()), Some(5.), None, None, None, None).unwrap();
    let dec = rays_editor::Workspace::declared_camera(Some(factories.clone()), &doc, &base);
    let eye = rays::camera::position(&rays::easy_camera::camera(&dec).unwrap());
    assert!(rays_math::vec3::nearly_equal(eye, rays_math::vec3::create(1., 2., 3.), 0.001), "declared_camera with custom factories did not read camera");
}
