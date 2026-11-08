//! Port of lib/flow_sop/test_planes.ml: `sop/attr_planes` reads any float-family attribute as x/y/z/w
//! planes with a cook-time width; `sop/with_planes` writes them back with the matching storage type.
//!
//! One workspace per owner writes all four widths back under new names (the OCaml test lowers once per
//! owner, name, tier and setting); the refusals share one workspace with a graph each. The full OCaml
//! matrix runs: reference and CPU tiers, (domains, grain) (1, 97), (4, 1), (4, 4096).
use super::support::*;
use crate::{lisp_sop, lower};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::geometry as geo;
use rdk_core::packed::{Float2, Float3, Float4};
use sop::edit_graph as edit;
use sop::{context as sctx, payload, session};
use std::sync::Arc;

fn rk<A>(r: Result<A, rdk_core::error::T>) -> A {
    r.unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)))
}

fn raw() -> [f64; 6] {
    [0.0, -0.0, 1.5, f64::MIN_POSITIVE / 4.0, -3.25e300, 7.0]
}

fn plane(k: usize, n: usize) -> Vec<f64> {
    let r = raw();
    (0..n).map(|i| r[(i + k) % r.len()]).collect()
}

fn make() -> geo::T {
    let mut g = fixture_grid(3, 2, 2.0);
    for owner in [Owner::Point, Owner::Vertex, Owner::Primitive, Owner::Detail] {
        let n = match owner {
            Owner::Point => geo::point_count(&g),
            Owner::Vertex => geo::vertex_count(&g),
            Owner::Primitive => geo::primitive_count(&g),
            Owner::Detail => 1,
        } as usize;
        let mut add = |name: &str, storage: Storage| {
            g = rk(geo::with_attribute(rk(attribute::create_owned(name, owner, storage)), &g));
        };
        add("f", Storage::Float(Arc::new(plane(0, n))));
        add("v2", Storage::Float2(rk(Float2::of_owned(plane(1, n), plane(2, n)))));
        add("v3", Storage::Float3(rk(Float3::View::of_owned_exn(plane(1, n), plane(2, n), plane(3, n)))));
        add("v4", Storage::Float4(rk(Float4::of_owned(plane(1, n), plane(2, n), plane(3, n), plane(4, n)))));
        add("i", Storage::Int(Arc::new((0..n as i64).collect())));
        add("nf", Storage::Float(Arc::new((0..n).map(|i| if i == 0 { f64::NAN } else { 1.0 }).collect())));
    }
    g
}

/// Lower `graphs` (name, body) over the `src` node; each body sees `src`. The first graph is `g`.
fn lowered(reference: bool, src: &geo::T, graphs: &[(&str, String)]) -> Vec<(String, sop::node::T)> {
    let snapshot = lisp_sop::snapshot(src);
    let mut factories = vec![edit::factory(edit::factory_args("src", "src", &["Test"]), 0, Arc::new(move |_| Ok(snapshot.clone())))];
    factories.extend(sop::nodes::factories());
    let body: String = graphs.iter().map(|(name, b)| format!("(graph {name} :context sop (let* [src (sop/src)] {b}))")).collect::<Vec<_>>().join("\n");
    let options = lower::Options { reference, inputs: Some(vec![("g".to_owned(), Vec::new())]), ..lower::Options::default() };
    let l = lower::workspace(Some(factories), Vec::new(), options, &parse(&format!("(workspace planes_test\n{body})"))).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    graphs
        .iter()
        .map(|(name, _)| {
            let graph = l.graphs.iter().find(|g| g.name == *name).unwrap_or_else(|| panic!("graph {name} lowered")).clone();
            (name.to_string(), edit::compile_node(&graph.network.geometry, graph.root.expect("a result")).unwrap_or_else(|m| panic!("{m}")))
        })
        .collect()
}

fn cooked(node: &sop::node::T, domains: i64, grain: i64) -> Result<geo::T, rays_diagnostic::Diagnostic> {
    let mut s = session::create(32, 64_000_000).unwrap();
    let context = sctx::create(sctx::Options { domains: Some(domains), grain: Some(grain), ..Default::default() }).unwrap();
    let out = session::cook(&mut s, None, &context, node).map(|o| payload::geometry(&o.payload).unwrap());
    session::close(&mut s);
    out
}

fn bits(a: &[f64]) -> Vec<u64> {
    a.iter().map(|x| x.to_bits()).collect()
}

fn planes_of(s: &Storage) -> Vec<Vec<u64>> {
    match s {
        Storage::Float(a) => vec![bits(a)],
        Storage::Float2(d) => {
            let v = Float2::View::view(d);
            vec![bits(&v.x), bits(&v.y)]
        }
        Storage::Float3(d) => {
            let v = rk(Float3::View::view(d));
            vec![bits(&v.x), bits(&v.y), bits(&v.z)]
        }
        Storage::Float4(d) => {
            let v = Float4::View::view(d);
            vec![bits(&v.x), bits(&v.y), bits(&v.z), bits(&v.w)]
        }
        _ => Vec::new(),
    }
}

fn storage_of(owner: Owner, name: &str, g: &geo::T) -> Storage {
    rk(attribute::storage(&geo::find_attribute(owner, name, g).unwrap_or_else(|| panic!("attribute {name}"))))
}

fn settings() -> Vec<(i64, i64)> {
    vec![(1, 97), (4, 1), (4, 4096)]
}

/// Every owner and width writes back identical storage under a new name.
fn round_trip(oname: &str, owner: Owner) {
    let src = make();
    let names = ["f", "v2", "v3", "v4"];
    let mut body = "src".to_owned();
    for name in names {
        body = format!("(sop/with_planes {body} \"copy_{name}\" (sop/attr_planes src {name:?} :owner {oname:?}) :owner {oname:?})");
    }
    let tiers: &[bool] = &[false, true];
    for &reference in tiers {
        let nodes = lowered(reference, &src, &[("g", body.clone())]);
        for (domains, grain) in settings() {
            let out = cooked(&nodes[0].1, domains, grain).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
            for name in names {
                let copy = planes_of(&storage_of(owner, &format!("copy_{name}"), &out));
                assert!(copy == planes_of(&storage_of(owner, name, &src)) && !copy.is_empty(), "{oname} {name} planes round trip changed storage");
            }
        }
    }
}

#[test]
fn planes_round_trip_point() {
    round_trip("point", Owner::Point);
}

#[test]
fn planes_round_trip_vertex() {
    round_trip("vertex", Owner::Vertex);
}

#[test]
fn planes_round_trip_primitive() {
    round_trip("primitive", Owner::Primitive);
}

#[test]
fn planes_round_trip_detail() {
    round_trip("detail", Owner::Detail);
}

/// The cook-time width reaches the checker as a plain Int and a cooked branch can use it; computed
/// planes, a literal width and canonical P.
#[test]
fn planes_width_and_computed_p() {
    let src = make();
    let nodes = lowered(
        false,
        &src,
        &[
            ("g", "(sop/with_planes src \"w\" (sop/attr_planes src \"v3\") :owner \"point\")".to_owned()),
            (
                "computed",
                "(let* [p (sop/attr_planes src \"P\" :owner \"point\")\n      doubled {:width p.width :x (exact (map (fn [(v : float)] (* v 2.)) p.x)) :y p.y :z p.z :w p.w}]\n      (sop/with_planes src \"P\" doubled :owner \"point\"))".to_owned(),
            ),
        ],
    );
    let out = cooked(&nodes[0].1, 1, 97).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    assert!(planes_of(&storage_of(Owner::Point, "w", &out)) == planes_of(&storage_of(Owner::Point, "v3", &src)), "width 3 lost");
    let out = cooked(&nodes[1].1, 1, 97).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let a = rk(Float3::View::view(geo::positions(&out)));
    let b = rk(Float3::View::view(geo::positions(&src)));
    assert!(a.x.iter().zip(b.x.iter()).all(|(x, y)| *x == 2.0 * *y) && a.y == b.y && a.z == b.z, "computed P planes");
}

/// Refusals keep typed codes.
#[test]
fn planes_refusals_keep_typed_codes() {
    let src = make();
    let zeros = "(array/float 7)";
    let cases: Vec<(&str, String, &str)> = vec![
        ("int", r#"(sop/with_planes src "x" (sop/attr_planes src "i" :owner "point") :owner "point")"#.to_owned(), "E_ATTR_TYPE"),
        ("missing", r#"(sop/with_planes src "x" (sop/attr_planes src "nope" :owner "point") :owner "point")"#.to_owned(), "E_ATTR_TYPE"),
        ("width 0", format!(r#"(sop/with_planes src "x" {{:width 0 :x {zeros} :y {zeros} :z {zeros} :w {zeros}}} :owner "point")"#), "E_ATTR_WIDTH"),
        ("width 5", format!(r#"(sop/with_planes src "x" {{:width 5 :x {zeros} :y {zeros} :z {zeros} :w {zeros}}} :owner "point")"#), "E_ATTR_WIDTH"),
        ("count", r#"(sop/with_planes src "x" {:width 1 :x (array/float 2) :y (array/float 2) :z (array/float 2) :w (array/float 2)} :owner "point")"#.to_owned(), "E_ATTR_COUNT"),
        ("nonfinite", r#"(sop/with_planes src "x" (sop/attr_planes src "nf" :owner "point") :owner "point")"#.to_owned(), "E_NONFINITE"),
    ];
    let names: Vec<String> = (0..cases.len()).map(|i| if i == 0 { "g".to_owned() } else { format!("r{i}") }).collect();
    let graphs: Vec<(&str, String)> = names.iter().zip(cases.iter()).map(|(n, (_, b, _))| (n.as_str(), b.clone())).collect();
    let nodes = lowered(false, &src, &graphs);
    for ((what, _, code), (_, node)) in cases.iter().zip(nodes.iter()) {
        match cooked(node, 1, 97) {
            Err(d) => assert_eq!(d.code, *code, "{what}: code {}, expected {code}", d.code),
            Ok(_) => panic!("{what} was accepted"),
        }
    }
}
