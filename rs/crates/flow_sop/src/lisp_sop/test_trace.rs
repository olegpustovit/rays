//! Port of the kernel half of lib/flow_sop/test_trace.ml (the host half lives in trace_scene.rs): a `.rays`
//! kernel traces a lattice of rays at a quad; the packed kernel is bit-identical to the reference walker
//! at 1 and 8 domains, its t and prim match a direct raycast, and one host query is made per ray.
//! The real JavaScript region exporter refuses the native trace source with E_WEB_SOURCE.
use crate::{catalog, operators, trace_scene};
use flow_check::catalog::Catalog;
use flow_check::ray_host as host;
use flow_eval::{self as e, Value};
use flow_ir::packed;
use flow_syntax::syntax;
use rdk::packed::Float3;
use rdk::surface_index as surface;
use rdk::{geometry, topology};
use rays_math::vec3;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

const ID: i64 = 7;
const COUNT: usize = 256;

fn quad() -> geometry::T {
    let positions = Float3::of_owned(vec![-1.0, 1.0, 1.0, -1.0], vec![-1.0, -1.0, 1.0, 1.0], vec![0.0; 4]).unwrap();
    geometry::create(positions, topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap(), None, None, None).unwrap()
}
fn x(i: usize) -> f64 {
    0.15 * (i % 16) as f64 - 1.2
}
fn y(i: usize) -> f64 {
    0.15 * (i / 16) as f64 - 1.2
}

/// Test_program.compile: the residual of `tested` in a value graph, compiled.
fn compile(body: &str) -> (Value, packed::T) {
    let source = format!("(workspace w (graph g :context value (let* [tested {body}] 0.0)))");
    let forms = syntax::parse(&source).unwrap();
    let checked = match catalog::check_workspace(operators::all(), false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms) {
        (Some(w), d) if d.is_empty() => w,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let evaluation = e::static_with(&checked, true, None, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let (_, values) = evaluation.records.iter().find(|(p, _)| p == &vec!["g".to_owned(), "tested".to_owned()]).expect("tested");
    let value = values[0].1.clone();
    let Value::Residual(r) = &value else { panic!("a live residual expected") };
    let term = e::Residual::residual_view(*r).unwrap().term;
    let program = packed::compile_result(&packed::CompileOptions::default(), *r, &term).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
    (value, program)
}

#[test]
fn packed_trace_matches_the_walker_and_a_direct_raycast() {
    let quad = quad();
    trace_scene::register(&[], &[], None, ID, &quad).unwrap();
    let (value, program) = compile(&format!(
        "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 (float (int/div i 16))) 1.2)\n h (scene/trace {ID} [x y 5.] [0. 0. -1.] 0. 100. t)]\n [h.t (float h.prim) (+ h.u h.v) (if h.hit (+ 1. (* h.normal.z h.normal.z)) 0.)])) (array/range {COUNT} :type int))"
    ));
    assert_eq!(flow_web::js::region(false,&program).unwrap_err().code,"E_WEB_SOURCE");
    let live = input::at_time(1.0);
    let reference = e::Host::force_reference(&value, &live, None, None, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    for domains in [1, 8] {
        parallel::run_unit(Some(domains), &|| {
            let forced = packed::force(&program, &live, &packed::ForceOptions { strict: true, ..Default::default() }).map_err(|d| rays_diagnostic::to_string(&d))?;
            if format!("{forced:?}") != format!("{reference:?}") {
                return Err("the packed kernel differs from the reference walker".to_owned());
            }
            Ok(())
        })
        .unwrap();
    }
    let Value::Vec4_array(values) = &reference else { panic!("vec4 array expected, got {reference:?}") };
    let index = surface::create(None, None, None, None, None, &quad).unwrap();
    let mut hits = 0;
    for i in 0..COUNT {
        match surface::raycast(&index, vec3::create(x(i), y(i), 5.0), vec3::create(0.0, 0.0, -1.0)).unwrap() {
            Some(hit) => {
                hits += 1;
                assert!(values[4 * i] == hit.distance && values[4 * i + 1] == hit.primitive as f64 && values[4 * i + 3] > 0.0, "ray {i}");
            }
            None => assert!(values[4 * i + 3] == 0.0 && values[4 * i + 1] == -1.0, "ray {i}"),
        }
    }
    assert!(hits > 0 && hits < COUNT);
    trace_scene::unregister(ID);
}

#[test]
fn one_host_query_per_ray_not_one_per_field() {
    let quad = quad();
    trace_scene::register(&[], &[], None, ID, &quad).unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    let counter = calls.clone();
    host::register(8, Arc::new(move |q: &[f64]| {
        counter.fetch_add(1, Ordering::SeqCst);
        host::query(ID, q).unwrap()
    }));
    let (_, counted) = compile(&format!(
        "(map (fn [(i : int)] (let* [h (scene/trace 8 [(* 0.01 (float i)) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t (float h.prim) h.u h.normal.z])) (array/range {COUNT} :type int))"
    ));
    packed::force(&counted, &input::at_time(1.0), &packed::ForceOptions { strict: true, ..Default::default() }).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert_eq!(calls.load(Ordering::SeqCst), COUNT);
    host::unregister(8);
    trace_scene::unregister(ID);
}
