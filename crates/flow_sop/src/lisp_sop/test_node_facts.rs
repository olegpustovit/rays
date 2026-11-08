//! Port of lib/flow_sop/test_node_facts.ml: each Lisp node's declared facts, checked on its factory
//! node (this module lowers calls as that node).
use super::support::*;
use crate::{lisp_sop, lower};
use rdk_core::attribute;
use rdk_core::geometry as geo;
use rdk_core::normal_ops;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node::{self, CookMode, Topology};
use sop::session;

fn ctx(domains: i64) -> sctx::T {
    sctx::create(ContextOptions { domains: Some(domains), grain: Some(37), ..Default::default() }).unwrap()
}

fn new_eval_session() -> session::T {
    session::create(64, 16_000_000).unwrap()
}

fn cook_out(s: &mut session::T, domains: i64, n: &node::T) -> geo::T {
    cook(s, &ctx(domains), n)
}

fn rdk_ok<A>(r: Result<A, rdk_core::error::T>) -> A {
    r.unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)))
}

/// Plane_generators.grid's default triangles in the XZ plane, retaining the OCaml fixture bounds.
fn plane_grid(columns: i64, rows: i64, size: f64) -> geo::T {
    let stride = columns + 1;
    let (mut x, mut y) = (Vec::new(), Vec::new());
    for j in 0..=rows {
        for i in 0..=columns {
            x.push((i as f64 / columns as f64 - 0.5) * size);
            y.push((j as f64 / rows as f64 - 0.5) * size);
        }
    }
    let z = vec![0.0; x.len()];
    let (mut vp, mut offsets) = (Vec::new(), vec![0i64]);
    for j in 0..rows {
        for i in 0..columns {
            let a = j * stride + i;
            vp.extend_from_slice(&[a, a + stride, a + stride + 1, a, a + stride + 1, a + 1]);
            offsets.extend([vp.len() as i64 - 3, vp.len() as i64]);
        }
    }
    let positions = rdk_ok(rdk_core::packed::float3::of_owned(x, z, y));
    let topology = rdk_ok(rdk_core::topology::polygons_owned(stride * (rows + 1), vp, offsets));
    rdk_ok(geo::create(positions, topology, None, None, None))
}

fn normals(owner: attribute::Owner, g: &geo::T) -> geo::T {
    let params = normal_ops::Params {
        cancel: None,
        grain: None,
        selection: None,
        primitives: None,
        owner,
        weighting: normal_ops::Weighting::VertexAngle,
        cusp_angle: std::f64::consts::PI,
        attribute: "N".to_owned(),
    };
    rdk_ok(normal_ops::run(&params, g))
}

fn fixture() -> geo::T {
    let g = plane_grid(8, 6, 2.0);
    let g = normals(attribute::Owner::Point, &g);
    let point_normal = geo::find_attribute(attribute::Owner::Point, "N", &g).expect("point N");
    let g = normals(attribute::Owner::Vertex, &g);
    rdk_ok(geo::with_attribute(point_normal, &g))
}

#[test]

fn declared_preserved_topology_holds_at_one_and_eight_domains() {
    let native = sop::nodes::factories();
    let lowered = lower::workspace(Some(native.clone()), Vec::new(), lower::Options::default(), &parse("(workspace facts (graph unused :context value 0.0))")).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let mut factories = native.clone();
    factories.extend(lowered.factories.iter().cloned());
    let g = fixture();
    let topology = geo::topology(&g);
    let index = rdk_ok(rdk_core::topology_index::create(None, topology));
    let edges = rdk_ok(rdk_core::edge_group::init(None, topology, &index, "edges", &|_| true));
    let g = rdk_ok(geo::with_edge_group(edges, &g));
    let count = geo::point_count(&g) as usize;
    let uv = rdk_ok(rdk_core::packed::float2::of_owned(vec![0.0; count], vec![0.0; count]));
    let uv = rdk_ok(attribute::create_owned("uv", attribute::Owner::Point, attribute::Storage::Float2(uv)));
    let source = lisp_sop::snapshot(&rdk_ok(geo::with_attribute(uv, &g)));
    let image = edit::instantiate(&sop::image_nodes::noise_factory(), &[]).unwrap();
    let (image, _) = node::apply_parameters(&image, &[("width".to_owned(), param::Value::Int(1)), ("height".to_owned(), param::Value::Int(1))]).unwrap();
    let mut evaluator = new_eval_session();
    let mut preserved: Vec<String> = Vec::new();
    let mut verify = |key: String, n: &node::T| {
        let declaration = node::facts(n);
        if declaration.topology == Topology::Preserved && declaration.cook_mode == CookMode::DuplicateInput(0) {
            let input = cook_out(&mut evaluator, 1, &source);
            let one = cook_out(&mut evaluator, 1, n);
            let mut cold = new_eval_session();
            let eight = cook_out(&mut cold, 8, n);
            session::close(&mut cold);
            assert!(rdk_core::topology::data_id(geo::topology(&one)) == rdk_core::topology::data_id(geo::topology(&input)), "{} replaced Preserved topology", node::operation(n));
            assert!(geometry_bytes(&one) == geometry_bytes(&eight), "{} differs at eight domains", node::operation(n));
            preserved.push(key);
        }
    };
    for factory in &factories {
        let inputs: Vec<Option<node::T>> = edit::factory_input_types(factory).iter().map(|t| Some(if t == "image" { image.clone() } else { source.clone() })).collect();
        let n = edit::instantiate_optional(factory, &inputs).unwrap();
        verify(edit::factory_key(factory).to_owned(), &n);
    }
    for key in ["transform", "noise_displace", "peak", "bend", "mountain", "normals", "attribute_noise", "attribute_noise_quaternion", "color_by_height"] {
        let n = lisp_sop::node(&[("source".to_owned(), source.clone())], &format!("(sop/{key} (sop/ext_source))"));
        verify(key.to_owned(), &n);
    }
    for name in ["transform", "soft_transform", "noise_displace", "peak", "bend", "mountain", "normals", "attribute_noise", "attribute_noise_quaternion", "color_by_height"] {
        assert!(preserved.iter().any(|p| p == name), "{name} lacks a Preserved declaration");
    }
    let with = [("source".to_owned(), source.clone())];
    let peak = lisp_sop::node(&with, "(sop/peak (sop/ext_source))");
    let noise = lisp_sop::node(&with, "(sop/noise_displace (sop/ext_source))");
    assert!(node::facts(&noise).reads == vec!["P".to_owned()], "Height 2D noise reads unexpected planes");
    let (noise, _) = node::apply_parameters(&noise, &[("mode".to_owned(), param::Value::Choice("normal_3d".to_owned()))]).unwrap();
    assert!(node::facts(&noise).reads == vec!["P".to_owned(), "N".to_owned()], "Normal 3D noise facts omit N");
    let one = cook_out(&mut evaluator, 1, &noise);
    let mut cold = new_eval_session();
    let eight = cook_out(&mut cold, 8, &noise);
    assert!(geometry_bytes(&one) == geometry_bytes(&eight), "Normal 3D SOP differs across domains");
    session::close(&mut cold);
    let (updated, _) = node::apply_parameters(&peak, &[("direction_attribute".to_owned(), param::Value::Text(b"direction".to_vec()))]).unwrap();
    let reads = &node::facts(&updated).reads;
    assert!(reads.iter().any(|r| r == "direction") && !reads.iter().any(|r| r == "N"), "PPX rebuild retained stale facts");
    session::close(&mut evaluator);
}

/// A P write that computes every component replaces positions without reading them: positions a
/// device still holds stay unread (no payload readback) on the CPU route.
#[test]
fn replacing_p_leaves_held_positions_unread() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    let reads = Arc::new(AtomicUsize::new(0));
    let counted = reads.clone();
    let plain = rdk_ok(rdk_ops::line_geometry::points(&[(0., 0., 0.); 8]));
    let held = rdk_ok(rdk_core::packed::float3::deferred(None, None, None, 8, Arc::new(move || {
        counted.fetch_add(1, Ordering::SeqCst);
        Ok(vec![vec![0.0; 8]; 3])
    })));
    let g = rdk_ok(geo::create(held, geo::topology(&plain).clone(), None, None, None));
    let source = lisp_sop::snapshot(&g);
    let n = lisp_sop::node(&[("source".to_owned(), source)], "(sop/with_attr (sop/ext_source) :P (map (fn [i] [i (+ i t) 0]) (array/range 8)))");
    let mut s = new_eval_session();
    let out = cook(&mut s, &ctx(1), &n);
    assert_eq!(rdk_core::packed::float3::get(geo::positions(&out), 3).unwrap().0, 3.0);
    assert_eq!(reads.load(Ordering::SeqCst), 0, "the replaced positions were read back");
    session::close(&mut s);
}
