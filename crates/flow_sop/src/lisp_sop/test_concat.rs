//! Port of lib/flow_sop/test_concat.ml: `sop/concat` is `Rdk.Mesh_merge.run` on cooked geometries.
//! Bytes equal the native merge for one, two and three inputs with matching, mismatched and padded
//! attribute/group sets; errors share the native merge's code; inputs are not mutated.
//! The full OCaml matrix: reference and CPU tiers, domains {1, 4}, grains {1, 97, 4096}; one `#[test]` per
//! case so each lowers once.
use super::support::*;
use crate::{lisp_sop, lower};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::geometry as geo;
use rdk_core::group;
use rdk_core::packed::float3;
use sop::edit_graph as edit;
use sop::{context as sctx, payload, session};
use std::sync::Arc;

fn rk<A>(r: Result<A, rdk_core::error::T>) -> A {
    r.unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)))
}

fn grid(c: i64, r: i64) -> geo::T {
    fixture_grid(c, r, 2.0)
}

fn count_of(owner: Owner, g: &geo::T) -> usize {
    (match owner {
        Owner::Point => geo::point_count(g),
        Owner::Vertex => geo::vertex_count(g),
        Owner::Primitive => geo::primitive_count(g),
        Owner::Detail => 1,
    }) as usize
}

fn with_float(name: &str, owner: Owner, k: i64, g: &geo::T) -> geo::T {
    let n = count_of(owner, g);
    let storage = Storage::Float(Arc::new((0..n).map(|i| (i as i64 + k) as f64 / 3.0).collect()));
    rk(geo::with_attribute(rk(attribute::create_owned(name, owner, storage)), g))
}

fn with_vec3(name: &str, k: i64, g: &geo::T) -> geo::T {
    let n = geo::point_count(g) as usize;
    let storage = Storage::Float3(rk(float3::view::of_owned_exn((0..n).map(|i| (i as i64 + k) as f64).collect(), vec![-0.0; n], vec![0.5; n])));
    rk(geo::with_attribute(rk(attribute::create_owned(name, Owner::Point, storage)), g))
}

fn with_text(name: &str, g: &geo::T) -> geo::T {
    let n = geo::primitive_count(g) as usize;
    let storage = Storage::Text(Arc::new((0..n).map(|i| if i % 2 == 0 { "猫".as_bytes().to_vec() } else { b"x\0".to_vec() }).collect()));
    rk(geo::with_attribute(rk(attribute::create_owned(name, Owner::Primitive, storage)), g))
}

fn with_group(name: &str, phase: i64, g: &geo::T) -> geo::T {
    let n = geo::point_count(g);
    rk(geo::with_group(rk(group::init(None, group::Owner::Point, name, n, &|i| Ok(i % 2 == phase))), g))
}

fn with_ordered(name: &str, g: &geo::T) -> geo::T {
    let n = geo::point_count(g);
    let order: Vec<i64> = (0..n).map(|i| n - 1 - i).collect();
    rk(geo::with_group(rk(group::ordered(group::Owner::Point, name, n, &order)), g))
}

fn a() -> geo::T {
    with_ordered("o", &with_group("g", 0, &with_vec3("v", 0, &with_float("h", Owner::Point, 0, &grid(3, 2)))))
}

fn b() -> geo::T {
    with_group("g", 1, &with_vec3("v", 5, &with_float("h", Owner::Point, 7, &grid(2, 2))))
}

fn c() -> geo::T {
    with_text("t", &with_vec3("v", 1, &with_float("h", Owner::Point, 2, &grid(1, 4))))
}

type Native = Result<geo::T, rdk_core::error::T>;

fn native(source_attribute: Option<&str>, source_base: Option<i64>, gs: &[geo::T]) -> Native {
    let params = rdk_ops::mesh_merge::RunParams { cancel: None, grain: None, source_attribute: source_attribute.map(str::to_owned), source_base };
    rdk_ops::mesh_merge::run(&params, gs)
}

/// One lowering of `(sop/concat (list ...) extra)` over the named sources.
fn lowered(reference: bool, named: &[(String, geo::T)], extra: &str) -> sop::node::T {
    let mut factories: Vec<edit::Factory> = named
        .iter()
        .map(|(k, g)| {
            let snapshot = lisp_sop::snapshot(g);
            edit::factory(edit::factory_args(k, k, &["Test"]), 0, Arc::new(move |_| Ok(snapshot.clone())))
        })
        .collect();
    factories.extend(sop::nodes::factories());
    let calls = named.iter().map(|(k, _)| format!("(sop/{k})")).collect::<Vec<_>>().join(" ");
    let text = format!("(workspace concat_test (graph g :context sop (sop/concat (list {calls}){extra})))");
    let options = lower::Options { reference, inputs: Some(vec![("g".into(), Vec::new())]), ..lower::Options::default() };
    let l = lower::workspace(Some(factories), Vec::new(), options, &parse(&text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = l.graphs[0].clone();
    edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap_or_else(|m| panic!("{m}"))
}

fn cooked(node: &sop::node::T, domains: i64, grain: i64) -> Result<geo::T, rays_diagnostic::Diagnostic> {
    let mut s = session::create(32, 64_000_000).unwrap();
    let context = sctx::create(sctx::Options { domains: Some(domains), grain: Some(grain), ..Default::default() }).unwrap();
    let out = session::cook(&mut s, None, &context, node).map(|o| payload::geometry(&o.payload).unwrap());
    session::close(&mut s);
    out
}

fn same(what: &str, expected: &Native, actual: &Result<geo::T, rays_diagnostic::Diagnostic>) {
    match (expected, actual) {
        (Ok(e), Ok(a)) => assert!(geometry_bytes(e) == geometry_bytes(a), "{what}: bytes differ from native merge"),
        (Err(e), Err(a)) => assert_eq!(rdk_core::error::code(e).as_str(), a.code, "{what}: error code differs"),
        _ => panic!("{what}: result kind differs"),
    }
}

fn case(what: &str, sources: Vec<geo::T>, extra: &str, expected: Native) {
    let named: Vec<(String, geo::T)> = sources.into_iter().enumerate().map(|(i, g)| (["src", "dst", "aux"][i].to_owned(), g)).collect();
    let tiers: &[bool] = &[false, true];
    let grains: &[i64] = &[1, 97, 4096];
    for &reference in tiers {
        let node = lowered(reference, &named, extra);
        for domains in [1, 4] {
            for &grain in grains {
                let before: Vec<Vec<u8>> = named.iter().map(|(_, g)| geometry_bytes(g)).collect();
                same(what, &expected, &cooked(&node, domains, grain));
                assert!(before == named.iter().map(|(_, g)| geometry_bytes(g)).collect::<Vec<_>>(), "{what}: inputs mutated");
            }
        }
    }
}

fn plain(what: &str, gs: Vec<geo::T>) {
    let expected = native(None, None, &gs);
    case(what, gs, "", expected);
}

#[test]
fn concat_one() {
    plain("one", vec![a()]);
}

#[test]
fn concat_two() {
    plain("two", vec![a(), b()]);
}

#[test]
fn concat_three_padded() {
    plain("three-padded", vec![a(), b(), c()]);
}

#[test]
fn concat_empty() {
    plain("empty", vec![]);
}

#[test]
fn concat_reordered() {
    plain("reordered", vec![c(), b(), a()]);
}

#[test]
fn concat_tagged() {
    let expected = native(Some("src"), Some(5), &[a(), b(), c()]);
    case("tagged", vec![a(), b(), c()], " :source_attribute \"src\" :source_base 5", expected);
}

#[test]
fn concat_blank_tag() {
    let expected = native(None, None, &[a(), b()]);
    case("blank-tag", vec![a(), b()], " :source_attribute \"  \"", expected);
}

fn mismatched(what: &str, x: geo::T, y: geo::T) {
    let expected = native(None, None, &[x.clone(), y.clone()]);
    case(what, vec![x, y], "", expected);
}

#[test]
fn concat_float_vs_vec3() {
    mismatched("float-vs-vec3", with_float("h", Owner::Point, 0, &grid(2, 2)), with_vec3("h", 0, &grid(2, 2)));
}

#[test]
fn concat_missing_attribute() {
    mismatched("missing-attribute", with_float("h", Owner::Point, 0, &grid(2, 2)), grid(2, 2));
}

#[test]
fn concat_detail() {
    mismatched("detail", with_float("d", Owner::Detail, 0, &grid(2, 2)), with_float("d", Owner::Detail, 1, &grid(2, 2)));
}
