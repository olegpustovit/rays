//! Port of lib/flow_sop/test_remap.ml: `sop/remap` against the native Topology_remap helpers, across
//! the reference and CPU tiers, domains and grains.
//!
//! Text fixtures retain the original invalid 0xff byte and embedded NUL without Unicode substitution.
use super::support::*;
use super::test_group_kernel::{flow_ok, ok};
use crate::{catalog, lisp_sop, lower, operators};
use flow_graph::flow_edit::{apply_checked_with, ArgKey, Op};
use flow_graph::projection as proj;
use flow_syntax::syntax;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::geometry as geo;
use rdk_core::packed::{Float2, Float3, Float4, FloatArray, IntArray};
use rdk_core::{cancel, edge_group, group, topology, topology_index};
use rdk_core::topology_remap as remap;
use sop::context::Options as ContextOptions;
use sop::edit_graph as edit;
use sop::node::{self, build, CookMode};
use sop::{context as sctx, payload, session};
use std::sync::Arc;

type Args = Vec<(String, Value)>;

fn grains() -> Vec<i64> { vec![1, 97, 4096] }
fn domains_list() -> Vec<i64> { vec![1, 4, 8] }

const WORDS: [u64; 7] = [0, 0x8000000000000000, 1, 0x7ff0000000000000, 0xfff0000000000000, 0x7ff8000000000011, 0x7ff8000000000022];

/// Weighted arithmetic does not define which NaN payload survives, so its test canonicalises them.
fn raw_with(canonical: bool, i: usize) -> f64 {
    let v = f64::from_bits(WORDS[i % WORDS.len()]);
    if canonical && v.is_nan() { f64::from_bits(0x7ff8000000000000) } else { v }
}

fn source(count: i64) -> geo::T {
    source_with(false, count)
}

fn source_with(canonical: bool, count: i64) -> geo::T {
    let raw = |i: usize| raw_with(canonical, i);
    let t = if count < 3 {
        ok(topology::empty(count))
    } else if count == 4 {
        ok(topology::create_owned(count, vec![0, 1, 2, 1, 2, 3], vec![0, 3, 6], vec![topology::PrimitiveKind::Polygon; 2]))
    } else {
        ok(topology::create_owned(count, vec![0, 1, 2], vec![0, 3], vec![topology::PrimitiveKind::Polygon]))
    };
    let mut attrs = Vec::new();
    for owner in [Owner::Point, Owner::Vertex, Owner::Primitive, Owner::Detail] {
        let n = match owner {
            Owner::Point => count,
            Owner::Vertex => topology::vertex_count(&t),
            Owner::Primitive => topology::primitive_count(&t),
            Owner::Detail => 1,
        } as usize;
        let floats = |k: usize| (0..n).map(|i| raw(i + k)).collect::<Vec<f64>>();
        let offsets: Vec<i64> = (0..=n as i64).map(|i| i * 16).collect();
        let ragged_offsets = |i: i64| 3 * (i / 3) + (i % 3) * ((i % 3) - 1) / 2;
        let kinds: Vec<(&str, Storage)> = vec![
            (
                "N",
                Storage::Float3(ok(Float3::View::of_owned_exn(
                    (0..n).map(|i| i as f64 - 1.0).collect(),
                    (0..n).map(|i| 0.25 * (i % 3) as f64).collect(),
                    (0..n).map(|i| if i % 4 == 0 { 0.0 } else { 2.0 }).collect(),
                ))),
            ),
            ("float", Storage::Float(Arc::new(floats(0)))),
            ("int", Storage::Int(Arc::new((0..n as i64).map(|i| (1 << 55) + i).collect()))),
            ("text", Storage::Text(Arc::new((0..n).map(|i| if i % 2 == 0 { [0xe7,0x8c,0xab,0xff].to_vec() } else { [0xf0,0x9f,0x98,0x80,0].to_vec() }).collect()))),
            ("vec2", Storage::Float2(ok(Float2::of_owned(floats(1), floats(2))))),
            ("vec3", Storage::Float3(ok(Float3::View::of_owned_exn(floats(3), floats(4), floats(5))))),
            ("vec4", Storage::Float4(ok(Float4::of_owned(floats(2), floats(3), floats(4), floats(5))))),
            ("mat16", Storage::FloatArray(ok(FloatArray::create_owned(offsets, (0..n * 16).map(raw).collect())))),
            ("ragged_float", Storage::FloatArray(ok(FloatArray::create_owned((0..=n as i64).map(|i| i / 2).collect(), (0..n / 2).map(raw).collect())))),
            ("ragged", Storage::IntArray(ok(IntArray::create_owned((0..=n as i64).map(ragged_offsets).collect(), (0..ragged_offsets(n as i64)).map(|i| (1 << 55) + i).collect())))),
        ];
        for (name, storage) in kinds {
            attrs.push(ok(attribute::create_owned(name, owner, storage)));
        }
    }
    let mut groups = Vec::new();
    for owner in [group::Owner::Point, group::Owner::Vertex, group::Owner::Primitive] {
        let n = match owner {
            group::Owner::Point => count,
            group::Owner::Vertex => topology::vertex_count(&t),
            group::Owner::Primitive => topology::primitive_count(&t),
        };
        groups.push(ok(group::init(None, owner, "plain", n, &|i| i % 2 == 0)));
        groups.push(ok(group::ordered(owner, "ordered", n, &(0..n).map(|i| n - 1 - i).collect::<Vec<_>>())));
        groups.push(ok(group::ordered(owner, "ordered_empty", n, &[])));
    }
    let index = ok(topology_index::create(None, &t));
    let edge_groups = vec![ok(edge_group::init(None, &t, &index, "edges", &|i| i % 2 == 0))];
    let n = count as usize;
    ok(geo::create(
        ok(Float3::View::of_owned_exn((0..n).map(raw).collect(), (0..n).map(|i| raw(i + 1)).collect(), (0..n).map(|i| raw(i + 2)).collect())),
        t,
        Some(attrs),
        Some(groups),
        Some(edge_groups),
    ))
}

fn target(copies: i64, src: &geo::T) -> geo::T {
    let t = ok(topology::View::view(geo::topology(src)));
    let points = geo::point_count(src);
    let vertices = geo::vertex_count(src);
    let primitives = geo::primitive_count(src);
    let made = ok(topology::create_owned(
        copies * points,
        (0..copies * vertices).map(|i| t.vertex_points[(i % vertices) as usize] + i / vertices * points).collect(),
        (0..=copies * primitives).map(|i| if i == copies * primitives { copies * vertices } else { t.primitive_offsets[(i % primitives) as usize] + i / primitives * vertices }).collect(),
        vec![topology::PrimitiveKind::Polygon; (copies * primitives) as usize],
    ));
    let n = (copies * points) as usize;
    ok(geo::create(ok(Float3::View::of_owned_exn(vec![3.0; n], vec![-0.0; n], vec![7.0; n])), made, None, None, None))
}

fn factory(key: &str, g: &geo::T) -> edit::Factory {
    let snapshot = lisp_sop::snapshot(g);
    edit::factory(edit::factory_args(key, key, &["Test"]), 0, Arc::new(move |_| Ok(snapshot.clone())))
}

fn factories(src: &geo::T, dst: &geo::T) -> Vec<edit::Factory> {
    let mut all = vec![factory("src", src), factory("dst", dst)];
    all.extend(sop::nodes::factories());
    all
}

fn source_text(edge: bool, body: &str) -> String {
    let body = if body.is_empty() {
        format!("(sop/remap source target :points points :vertices vertices :primitives primitives :positions positions{}", if edge { " :edges edge_map)" } else { ")" })
    } else {
        body.to_owned()
    };
    r#"(workspace remap_test
  (graph g :context sop [(points : (array int) (array/int 0))
    (vertices : (array int) (array/int 0)) (primitives : (array int) (array/int 0))
    (positions : text "target")
    (pa : (array int) (array/int 0)) (pb : (array int) (array/int 0)) (pt : (array float) (array/float 0 0.0))
    (va : (array int) (array/int 0)) (vb : (array int) (array/int 0)) (vt : (array float) (array/float 0 0.0))
    (ra : (array int) (array/int 0)) (rb : (array int) (array/int 0)) (rt : (array float) (array/float 0 0.0))
    (edge_map : {:offsets (array int) :values (array int)} {:offsets (array/int 1) :values (array/int 0)})]
    (let* [source (sop/src) target (sop/dst) tested %s] tested)))"#
        .replace("%s", &body)
}

fn compile_result(reference: bool, edge: bool, body: &str, src: &geo::T, dst: &geo::T, args: &Args) -> Result<node::T, String> {
    let options = lower::Options { reference, inputs: Some(vec![("g".to_owned(), args.clone())]), ..lower::Options::default() };
    let lowered = lower::workspace(Some(factories(src, dst)), Vec::new(), options, &parse(&source_text(edge, body))).map_err(|d| rays_diagnostic::to_string(&d))?;
    let graph = lowered.graphs[0].clone();
    edit::compile_node(&graph.network.geometry, graph.root.unwrap())
}

fn compile_with(reference: bool, edge: bool, body: &str, src: &geo::T, dst: &geo::T, args: &Args) -> node::T {
    compile_result(reference, edge, body, src, dst, args).unwrap_or_else(|m| panic!("{m}"))
}

fn compile(src: &geo::T, dst: &geo::T, args: &Args) -> node::T {
    compile_with(false, true, "", src, dst, args)
}

fn run_with(domains: i64, grain: i64, cancel: Option<Arc<cancel::T>>, n: &node::T) -> Result<session::Output, Diagnostic> {
    let mut s = session::create(32, 64_000_000).unwrap();
    let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), cancel, ..Default::default() }).unwrap();
    let out = session::cook(&mut s, None, &context, n);
    session::close(&mut s);
    out
}

fn run(n: &node::T) -> Result<session::Output, Diagnostic> {
    run_with(1, 97, None, n)
}

fn geometry(r: Result<session::Output, Diagnostic>) -> geo::T {
    match r {
        Ok(o) => flow_ok(payload::geometry(&o.payload)),
        Err(d) => panic!("{}", rays_diagnostic::error_to_string(&d)),
    }
}

fn args(p: &[i64], v: &[i64], prim: &[i64], offsets: &[i64], values: &[i64], policy: &str) -> Args {
    let ints = |a: &[i64]| Value::Int_array(Arc::new(a.to_vec()));
    vec![
        ("points".into(), ints(p)),
        ("vertices".into(), ints(v)),
        ("primitives".into(), ints(prim)),
        ("positions".into(), Value::Text(policy.as_bytes().to_vec())),
        ("edge_map".into(), Value::Record(vec![("offsets".into(), ints(offsets)), ("values".into(), ints(values))])),
    ]
}

fn replace(a: &Args, key: &str, value: Value) -> Args {
    a.iter().map(|(k, old)| (k.clone(), if k == key { value.clone() } else { old.clone() })).collect()
}

fn edges_of(g: &geo::T) -> i64 {
    topology_index::edge_count(&ok(topology_index::create(None, geo::topology(g))))
}

#[allow(clippy::too_many_arguments)]
fn oracle(src: &geo::T, dst: &geo::T, p: &[i64], v: &[i64], prim: &[i64], offsets: &[i64], ancestors: &[i64], mapped: bool) -> geo::T {
    let attrs = ok(remap::attributes(None, 97, p, v, prim, src));
    let groups = ok(remap::groups(None, 97, p, v, prim, src));
    let t = geo::topology(dst);
    let index = ok(topology_index::create(None, t));
    let edges: Vec<edge_group::T> = geo::edge_groups(src)
        .iter()
        .map(|g| {
            ok(edge_group::init(None, t, &index, edge_group::name(g), &|edge| {
                (offsets[edge as usize]..offsets[edge as usize + 1]).any(|at| edge_group::mem(ancestors[at as usize], g))
            }))
        })
        .collect();
    let positions = if !mapped {
        geo::positions(dst).clone()
    } else {
        let view = ok(Float3::View::view(geo::positions(src)));
        ok(Float3::View::of_owned_exn(p.iter().map(|i| view.x[*i as usize]).collect(), p.iter().map(|i| view.y[*i as usize]).collect(), p.iter().map(|i| view.z[*i as usize]).collect()))
    };
    ok(geo::create(positions, t.clone(), Some(attrs), Some(groups), Some(edges)))
}

#[test]
fn all_owner_storage_raw_bytes_order_and_edge_copies() {
    let (grains, domains) = (grains(), domains_list());
    for count in [0i64, 3] {
        let src = source(count);
        for copies in [0i64, 1, 2] {
            let dst = target(copies, &src);
            let p: Vec<i64> = (0..geo::point_count(&dst)).map(|i| i % count).collect();
            let v: Vec<i64> = (0..geo::vertex_count(&dst)).map(|i| i % geo::vertex_count(&src)).collect();
            let prim: Vec<i64> = (0..geo::primitive_count(&dst)).map(|i| i % geo::primitive_count(&src)).collect();
            let edges = edges_of(&dst);
            let se = edges_of(&src);
            let offsets: Vec<i64> = (0..=edges).collect();
            let ancestors: Vec<i64> = (0..edges).map(|i| i % se).collect();
            for policy in ["target", "mapped"] {
                let expected = oracle(&src, &dst, &p, &v, &prim, &offsets, &ancestors, policy == "mapped");
                for reference in [false, true] {
                    let node = compile_with(reference, true, "", &src, &dst, &args(&p, &v, &prim, &offsets, &ancestors, policy));
                    for &d in &domains {
                        for &grain in &grains {
                            let before = geometry_bytes(&src);
                            let target_before = geometry_bytes(&dst);
                            let actual = geometry(run_with(d, grain, None, &node));
                            assert!(geometry_bytes(&actual) == geometry_bytes(&expected), "remap native raw storage/order/edge bytes differ");
                            assert!(geometry_bytes(&src) == before && geometry_bytes(&dst) == target_before, "remap mutated inputs");
                            for a in geo::attributes(&src) {
                                if attribute::owner(&a) == Owner::Detail {
                                    let found = geo::find_attribute(Owner::Detail, attribute::name(&a), &actual).unwrap();
                                    assert!(attribute::data_id(&found) == attribute::data_id(&a), "Detail payload changed");
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn bits(x: f64) -> u64 {
    x.to_bits()
}

fn eqf(map: &[i64], a: &[f64], b: &[f64]) {
    for (i, x) in a.iter().enumerate() {
        let expected = if map[i] >= 0 { b[i] } else { 0.0 };
        assert!(bits(*x) == bits(expected), "created float row is not zero or copied row differs");
    }
}

/// Ragged rows as (offsets, value words).
fn ragged(map: &[i64], offs: &[i64], vals: &[u64], expected_offs: &[i64], expected_vals: &[u64]) {
    for i in 0..map.len() {
        let w = offs[i + 1] - offs[i];
        if map[i] >= 0 {
            assert!(w == expected_offs[i + 1] - expected_offs[i], "copied ragged row width differs");
            for k in 0..w {
                assert!(vals[(offs[i] + k) as usize] == expected_vals[(expected_offs[i] + k) as usize], "copied ragged value differs");
            }
        } else {
            assert!(w == 0, "created ragged row is not empty");
        }
    }
}

#[test]
fn minus_one_rows_are_defaults_and_in_no_group() {
    // A source index of -1 is a created element: every storage at its default, in no group. Other rows
    // are the native copy (the same Topology_remap helper with -1 replaced by row 0).
    let src = source(3);
    for copies in [1i64, 2] {
        let dst = target(copies, &src);
        let created = |n: i64| move |i: i64| i % 3 == 1 && i < n;
        let mk = |n: i64, real: &dyn Fn(i64) -> i64| -> Vec<i64> { (0..n).map(|i| if created(n)(i) { -1 } else { real(i) }).collect() };
        let (np, nv, nr) = (geo::point_count(&dst), geo::vertex_count(&dst), geo::primitive_count(&dst));
        let p = mk(np, &|i| (i + 2) % 3);
        let v = mk(nv, &|i| i % 3);
        let mut prim = mk(nr, &|i| i % geo::primitive_count(&src));
        if nr > 0 && copies == 1 {
            prim = vec![-1];
        }
        let zero = |m: &[i64]| m.iter().map(|i| (*i).max(0)).collect::<Vec<_>>();
        let edges = edges_of(&dst);
        let se = edges_of(&src);
        let offsets: Vec<i64> = (0..=edges).collect();
        let ancestors: Vec<i64> = (0..edges).map(|i| i % se).collect();
        let native = oracle(&src, &dst, &zero(&p), &zero(&v), &zero(&prim), &offsets, &ancestors, true);
        for reference in [false, true] {
            let node = compile_with(reference, true, "", &src, &dst, &args(&p, &v, &prim, &offsets, &ancestors, "mapped"));
            for domains in [1i64, 8] {
                let actual = geometry(run_with(domains, 97, None, &node));
                for a in geo::attributes(&actual) {
                    let owner = attribute::owner(&a);
                    if owner == Owner::Detail {
                        continue;
                    }
                    let map: &[i64] = match owner {
                        Owner::Point => &p,
                        Owner::Vertex => &v,
                        _ => &prim,
                    };
                    let n = geo::find_attribute(owner, attribute::name(&a), &native).unwrap();
                    match (ok(attribute::storage(&a)), ok(attribute::storage(&n))) {
                        (Storage::Float(x), Storage::Float(y)) => eqf(map, &x, &y),
                        (Storage::Int(x), Storage::Int(y)) => {
                            for (i, e) in x.iter().enumerate() {
                                assert!(*e == if map[i] >= 0 { y[i] } else { 0 }, "created int row is not 0");
                            }
                        }
                        (Storage::Text(x), Storage::Text(y)) => {
                            for (i, e) in x.iter().enumerate() {
                                assert!(e == if map[i] >= 0 { &y[i] } else { &[][..] }, "created text row is not empty");
                            }
                        }
                        (Storage::Float2(x), Storage::Float2(y)) => {
                            let (x, y) = (Float2::View::view(&x), Float2::View::view(&y));
                            eqf(map, &x.x, &y.x);
                            eqf(map, &x.y, &y.y);
                        }
                        (Storage::Float3(x), Storage::Float3(y)) => {
                            let (x, y) = (ok(Float3::View::view(&x)), ok(Float3::View::view(&y)));
                            eqf(map, &x.x, &y.x);
                            eqf(map, &x.y, &y.y);
                            eqf(map, &x.z, &y.z);
                        }
                        (Storage::Float4(x), Storage::Float4(y)) => {
                            let (x, y) = (Float4::View::view(&x), Float4::View::view(&y));
                            eqf(map, &x.x, &y.x);
                            eqf(map, &x.y, &y.y);
                            eqf(map, &x.z, &y.z);
                            eqf(map, &x.w, &y.w);
                        }
                        (Storage::IntArray(x), Storage::IntArray(y)) => {
                            let (x, y) = (IntArray::View::view(&x), IntArray::View::view(&y));
                            ragged(map, &x.offsets, &x.values.iter().map(|v| *v as u64).collect::<Vec<_>>(), &y.offsets, &y.values.iter().map(|v| *v as u64).collect::<Vec<_>>());
                        }
                        (Storage::FloatArray(x), Storage::FloatArray(y)) => {
                            let (x, y) = (FloatArray::View::view(&x), FloatArray::View::view(&y));
                            ragged(map, &x.offsets, &x.values.iter().map(|v| bits(*v)).collect::<Vec<_>>(), &y.offsets, &y.values.iter().map(|v| bits(*v)).collect::<Vec<_>>());
                        }
                        _ => panic!("remap changed a storage kind"),
                    }
                }
                for g in geo::groups(&actual) {
                    let map: &[i64] = match group::owner(&g) {
                        group::Owner::Point => &p,
                        group::Owner::Vertex => &v,
                        group::Owner::Primitive => &prim,
                    };
                    let n = geo::find_group(group::owner(&g), group::name(&g), &native).unwrap();
                    for i in 0..map.len() {
                        assert!(group::mem(i as i64, &g) == (map[i] >= 0 && group::mem(i as i64, &n)), "created row is in a group or copied membership differs");
                    }
                }
                let view = ok(Float3::View::view(geo::positions(&actual)));
                let expect = ok(Float3::View::view(geo::positions(&native)));
                eqf(&p, &view.x, &expect.x);
                eqf(&p, &view.y, &expect.y);
                eqf(&p, &view.z, &expect.z);
            }
        }
    }
}

#[test]
fn permutation_edge_or_identity_and_copy_major_order() {
    // Independent permutation and many-to-one edge ancestry; empty CSR rows and repeated ancestors must
    // be valid.
    let src = source(3);
    let dst = target(1, &source(3));
    let (p, v, prim) = ([2i64, 1, 0], [1i64, 2, 0], [0i64]);
    let (offsets, ancestors) = ([0i64, 0, 2, 4], [0i64, 1, 1, 1]);
    let expected = oracle(&src, &dst, &p, &v, &prim, &offsets, &ancestors, true);
    for reference in [false, true] {
        let node = compile_with(reference, true, "", &src, &dst, &args(&p, &v, &prim, &offsets, &ancestors, "mapped"));
        assert!(geometry_bytes(&geometry(run(&node))) == geometry_bytes(&expected), "permutation/edge OR bytes differ");
    }
    let src = source(3);
    let identity = args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "mapped");
    assert!(geometry_bytes(&geometry(run(&compile_with(false, false, "", &src, &src, &identity)))) == geometry_bytes(&src), "identity changed payload");
    // Actual Lisp composition retains native copy-major ordered groups.
    let dst = target(2, &src);
    let inputs = args(&[0, 1, 2, 0, 1, 2], &[0, 1, 2, 0, 1, 2], &[0, 0], &[0, 1, 2, 3, 4, 5, 6], &[0, 1, 2, 0, 1, 2], "mapped");
    let body = r#"(let* [out (sop/remap source target :points points :vertices vertices :primitives primitives
      :positions positions :edges edge_map) old_order (sop/group_order source :name "ordered" :owner "point")]
    (sop/with_group out (sop/group out :name "ordered" :owner "point") :name "ordered" :owner "point"
      :order (array/concat old_order.indices (exact (map (fn [(i : int)] (int/add i 3)) old_order.indices)))))"#;
    for reference in [false, true] {
        let actual = geometry(run(&compile_with(reference, true, body, &src, &dst, &inputs)));
        let order = group::ordered_elements(&geo::find_group(group::Owner::Point, "ordered", &actual).unwrap()).unwrap();
        assert!(order == [2, 1, 0, 5, 4, 3], "existing Lisp group-order override is not copy-major");
    }
}

#[test]
fn malformed_arguments_get_typed_codes_and_cancellation_is_honoured() {
    let src = source(3);
    let dst = target(1, &source(3));
    let valid = args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "target");
    let ints = |a: &[i64]| Value::Int_array(Arc::new(a.to_vec()));
    for (key, value, code) in [
        ("positions", Value::Text(b"other".to_vec()), "E_REMAP_POLICY"),
        ("points", ints(&[]), "E_REMAP_COUNT"),
        ("points", ints(&[-2, 1, 2]), "E_REMAP_INDEX"),
        ("vertices", ints(&[0, 1, 9]), "E_REMAP_INDEX"),
        ("primitives", ints(&[1]), "E_REMAP_INDEX"),
        ("edge_map", Value::Record(vec![("offsets".into(), ints(&[0, 2, 1, 3])), ("values".into(), ints(&[0, 1, 2]))]), "E_REMAP_EDGES"),
    ] {
        match run(&compile(&src, &dst, &replace(&valid, key, value))) {
            Err(d) => assert!(d.code == code, "remap typed validation code changed: {} is not {code}", d.code),
            Ok(_) => panic!("remap accepted malformed argument"),
        }
    }
    match run(&compile_with(false, false, "", &src, &dst, &valid)) {
        Err(d) => assert!(d.code == "E_REMAP_EDGES", "edge relation omission code"),
        Ok(_) => panic!("changed edge topology accepted implicit ancestry"),
    }
    let token = Arc::new(cancel::create());
    cancel::cancel(&token);
    match run_with(1, 97, Some(token), &compile(&src, &dst, &valid)) {
        Err(d) => assert!(d.code == "cancelled" || d.code == "E_CANCELLED", "cancellation changed: {}", d.code),
        Ok(_) => panic!("cancelled remap cooked"),
    }
}

#[test]
fn unchanged_remap_hits_the_cache_and_source_metadata_misses() {
    let src = source(3);
    let dst = target(1, &source(3));
    let node = compile(&src, &dst, &args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "target"));
    let mut s = session::create(32, 64_000_000).unwrap();
    let context = sctx::create(ContextOptions::default()).unwrap();
    let _ = session::cook(&mut s, None, &context, &node);
    let first = session::stats(&s);
    let _ = session::cook(&mut s, None, &context, &node);
    assert!(session::stats(&s).misses == first.misses, "unchanged remap cache missed");
    let payload_geometry = ok(geo::with_attribute(ok(attribute::create_owned("fresh", Owner::Point, Storage::Int(Arc::new(vec![1, 2, 3])))), &src));
    let rebuild = build::adopt_identity(&node, &build::rebuild_with_inputs(&node, &[lisp_sop::snapshot(&payload_geometry), lisp_sop::snapshot(&dst)]));
    let actual = geometry(session::cook(&mut s, None, &context, &rebuild));
    assert!(geo::find_attribute(Owner::Point, "fresh", &actual).is_some(), "cached remap stale source payload");
    assert!(session::stats(&s).misses > first.misses, "metadata-only source update did not miss");
    session::close(&mut s);
}

#[test]
fn remap_has_a_graph_card_and_checked_gestures() {
    let src = source(3);
    let dst = target(1, &source(3));
    let fs = factories(&src, &dst);
    let cat = flow_ok(catalog::of_factories(crate::manifest::VERSION, &[], None, &fs));
    let forms = parse(&source_text(true, ""));
    let checker = |c: &flow_check::catalog::Catalog, f: &[syntax::Syntax]| catalog::check_workspace(operators::all(), false, None, c, f);
    let checked = match checker(&cat, &forms) {
        (Some(c), ds) if !ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) => c,
        _ => panic!("remap projection did not check"),
    };
    let projection = proj::of_graph(&cat, &checked, "g").unwrap();
    assert!(projection.nodes.iter().any(|n| n.head == "sop/remap"), "remap has no graph card");
    let set = |key: &str, text: &str| Op::SetArg { node: vec!["g".into(), "tested".into()], key: ArgKey::Kw(key.to_owned()), sub: vec![], value: parse(text).remove(0) };
    for (key, text) in [
        ("points", "(array/int 0)"),
        ("vertices", "(array/int 0)"),
        ("primitives", "(array/int 0)"),
        ("positions", "\"mapped\""),
        ("edges", "{:offsets (array/int 1) :values (array/int 0)}"),
    ] {
        flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &set(key, text)));
    }
    assert!(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &set("points", "(array/text 0)")).is_err(), "remap graph type-changing malformed map edit accepted");
}

fn packed_cutoff_with_all_native_storage_kept_opaque(reference:bool,domains:i64) {
    // Original 32769-row boundary, independently registered per tier/domain.
    let n = 32769usize;
    let src = source(n as i64);
    let dst = ok(geo::with_positions(ok(Float3::View::of_owned_exn(vec![1.0; n], vec![2.0; n], vec![3.0; n])), &src));
    let p: Vec<i64> = (0..n as i64).map(|i| (i + 1) % n as i64).collect();
    let (v, prim, offsets, ancestors) = ([0i64, 1, 2], [0i64], [0i64, 1, 2, 3], [0i64, 1, 2]);
    let expected = oracle(&src, &dst, &p, &v, &prim, &offsets, &ancestors, true);
        let node = compile_with(reference, true, "", &src, &dst, &args(&p, &v, &prim, &offsets, &ancestors, "mapped"));
            for &grain in &grains() {
                assert!(geometry_bytes(&geometry(run_with(domains, grain, None, &node))) == geometry_bytes(&expected), "32769-row all-storage remap differs across CPU/reference/domains/grains");
            }
}

#[test]
fn outputs_are_owned_untaken_branches_are_not_evaluated_and_live_maps_follow_time() {
    let src = source(3);
    let dst = target(2, &source(3));
    let inputs = args(&[0, 1, 2, 0, 1, 2], &[0, 1, 2, 0, 1, 2], &[0, 0], &[0, 1, 2, 3, 4, 5, 6], &[0, 1, 2, 0, 1, 2], "mapped");
    let actual = geometry(run(&compile(&src, &dst, &inputs)));
    let before = geometry_bytes(&src);
    for a in geo::attributes(&src) {
        let copied = geo::find_attribute(attribute::owner(&a), attribute::name(&a), &actual).unwrap();
        if attribute::owner(&a) == Owner::Detail {
            assert!(attribute::data_id(&copied) == attribute::data_id(&a), "Detail storage identity changed");
        } else {
            assert!(attribute::storage_id(&copied) != attribute::storage_id(&a), "duplicated owner storage is not owned");
        }
    }
    // the output's storage is read back as a copy here: writing to it cannot reach the source
    if let Ok(Storage::Float(mut values)) = attribute::storage(&geo::find_attribute(Owner::Point, "float", &actual).unwrap()) {
        Arc::make_mut(&mut values)[0] = 99.0;
    }
    assert!(geometry_bytes(&src) == before, "mutating an owned output changed source bytes");
    let body = r#"(if false
    (sop/remap source target :points (array/int 1 -1) :vertices vertices :primitives primitives :edges edge_map)
    source)"#;
    for reference in [false, true] {
        assert!(geometry_bytes(&geometry(run(&compile_with(reference, true, body, &src, &dst, &inputs)))) == before, "untaken malformed remap branch was evaluated");
    }
    let body = r#"(let* [apply_remap (fn [(p : (array int))]
    (sop/remap source target :points p :vertices vertices :primitives primitives :edges edge_map :positions "mapped"))]
    (apply_remap (exact (map (fn [(i : int)] i) points))))"#;
    for reference in [false, true] {
        geometry(run(&compile_with(reference, true, body, &src, &dst, &inputs)));
    }
    let body = r#"(let* [chosen (if (< t 1.0) points (exact (map (fn [(i : int)] (int/mod (int/add i 1) 3)) points)))]
    (sop/remap source target :points chosen :vertices vertices :primitives primitives :edges edge_map :positions "mapped"))"#;
    let options = lower::Options { inputs: Some(vec![("g".to_owned(), inputs.clone())]), ..lower::Options::default() };
    let lowered = flow_ok(lower::workspace(Some(factories(&src, &dst)), Vec::new(), options, &parse(&source_text(true, body))));
    let graph = lowered.graphs[0].clone();
    let mut s = session::create(32, 64_000_000).unwrap();
    let node = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    let at = |s: &mut session::T, time: f64| geometry(session::cook(s, None, &sctx::create(ContextOptions { time: Some(time), ..Default::default() }).unwrap(), &node));
    let first = at(&mut s, 0.0);
    let second = at(&mut s, 2.0);
    assert!(geometry_bytes(&first) != geometry_bytes(&second), "live remap map remained stale");
    session::close(&mut s);
}

fn strict_public_defnode_computes_maps_through_typed_kernels(reference:bool,domains:i64) {
    // The strict public defnode path computes the maps through genuine typed integer kernels, then
    // transfers raw source payload without numeric reads.
    let src = source(3);
    let dst = target(10923, &source(3));
    let text = r#"(workspace strict_remap
    (defnode sop/strict_remap [(src : geometry) (dst : geometry)]
      (let* [points (exact (map (fn [(i : int)] (int/mod i 3))
                    (array/range (sop/count dst :points) :type int)))
             vertices (exact (map (fn [(i : int)] (int/mod i 3))
                      (array/range (sop/count dst :vertices) :type int)))
             primitives (array/int (sop/count dst :primitives) 0)
             edge_count (sop/count dst :edges)
             ancestry (exact (map (fn [(i : int)] (int/mod i 3))
                      (array/range edge_count :type int)))]
        (sop/remap src dst :points points :vertices vertices :primitives primitives
          :positions "mapped" :edges {:offsets (array/range (int/add edge_count 1) :type int) :values ancestry})))
    (graph g :context sop (sop/strict_remap (sop/src) (sop/dst))))"#;
    let p: Vec<i64> = (0..32769).map(|i| i % 3).collect();
    let prim = vec![0i64; 10923];
    let offsets: Vec<i64> = (0..32770).collect();
    let expected = oracle(&src, &dst, &p, &p, &prim, &offsets, &p, true);
        let lowered = flow_ok(lower::workspace(Some(factories(&src, &dst)), Vec::new(), lower::Options { reference, ..lower::Options::default() }, &parse(text)));
        let graph = lowered.graphs[0].clone();
        let node = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
            for &grain in &grains() {
                assert!(geometry_bytes(&geometry(run_with(domains, grain, None, &node))) == geometry_bytes(&expected), "strict generated maps/raw payload differ from native oracle");
            }
}

macro_rules! remap_matrix_case {
    ($module:ident,$reference:expr,$domains:expr)=>{
        mod $module {
            use super::*;
            #[test]fn packed_cutoff(){packed_cutoff_with_all_native_storage_kept_opaque($reference,$domains);}
            #[test]fn strict_public_defnode(){strict_public_defnode_computes_maps_through_typed_kernels($reference,$domains);}
        }
    };
}
remap_matrix_case!(cpu_one,false,1);remap_matrix_case!(cpu_four,false,4);remap_matrix_case!(cpu_eight,false,8);
remap_matrix_case!(reference_one,true,1);remap_matrix_case!(reference_four,true,4);remap_matrix_case!(reference_eight,true,8);

#[test]
fn a_failed_child_remains_the_first_failure() {
    // A real failed child remains the first failure, with its diagnostic envelope and provenance
    // unchanged; remap does not recast it as a mapping error.
    let mut child = rays_diagnostic::error("E_CHILD".into(), "child failed".into(), None, None, Some("raw child cause".into()), vec!["keep this hint".into()], Vec::new());
    child.severity = rays_diagnostic::Severity::ErrorSeverity;
    let failing = child.clone();
    let bad = build::make_geometry(
        build::Args { label: Some("Child".into()), cook_mode: CookMode::Generator, ..build::args("test.remap_child") },
        None,
        Arc::new(move |_, _, _| Err(failing.clone())),
    );
    let src = source(3);
    let dst = target(1, &source(3));
    let n = compile(&src, &dst, &args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "target"));
    let n = build::rebuild_with_inputs(&n, &[bad, lisp_sop::snapshot(&dst)]);
    for domains in [1i64, 4, 8] {
        match run_with(domains, 97, None, &n) {
            Ok(_) => panic!("remap swallowed child error"),
            Err(d) => assert!(
                d.code == child.code && d.message == child.message && d.cause == child.cause && d.hints == child.hints && d.trace.iter().any(|t| t.operation == "test.remap_child"),
                "remap changed a failed child diagnostic: {}",
                rays_diagnostic::error_to_string(&d)
            ),
        }
    }
}

#[test]
fn explicit_authored_target_topology() {
    // Target connectivity need not be obtainable by permuting the source. It is supplied explicitly; only
    // owner payload and edge ancestry come from src.
    let src = source(3);
    let t = ok(topology::create_owned(4, vec![0, 1, 2, 3, 3, 2], vec![0, 4, 6], vec![topology::PrimitiveKind::ClosedPolyline, topology::PrimitiveKind::OpenPolyline]));
    let dst = ok(geo::create(ok(Float3::View::of_owned_exn(vec![1.0, 2.0, 3.0, 4.0], vec![0.0; 4], vec![-0.0, 0.0, -0.0, 0.0])), t.clone(), None, None, None));
    let (p, v, prim) = ([2i64, 1, 0, 2], [2i64, 1, 0, 2, 1, 0], [0i64, 0]);
    let edges = topology_index::edge_count(&ok(topology_index::create(None, &t)));
    let offsets: Vec<i64> = (0..=edges).map(|i| 2 * i).collect();
    let ancestors: Vec<i64> = (0..2 * edges).map(|i| i % 3).collect();
    for policy in ["target", "mapped"] {
        let expected = oracle(&src, &dst, &p, &v, &prim, &offsets, &ancestors, policy == "mapped");
        for reference in [false, true] {
            let node = compile_with(reference, true, "", &src, &dst, &args(&p, &v, &prim, &offsets, &ancestors, policy));
            for &domains in &domains_list() {
                for &grain in &grains() {
                    assert!(geometry_bytes(&geometry(run_with(domains, grain, None, &node))) == geometry_bytes(&expected), "explicit authored target topology or raw P policy differs");
                }
            }
        }
    }
}

#[test]
fn changing_any_semantic_input_misses_the_cache() {
    // Preserve one logical consumer identity while changing every semantic input independently; the warm
    // result must equal a fresh-session oracle.
    let src = source(3);
    let dst = target(1, &source(3));
    let ints = |a: &[i64]| Value::Int_array(Arc::new(a.to_vec()));
    let base = args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "target");
    let target_p = ok(geo::with_positions(ok(Float3::View::of_owned_exn(vec![9.0, 8.0, 7.0], vec![0.0; 3], vec![0.0; 3])), &dst));
    let t = ok(topology::create_owned(3, vec![2, 1, 0], vec![0, 3], vec![topology::PrimitiveKind::Polygon]));
    let target_topology = ok(geo::create(geo::positions(&dst).clone(), t, None, None, None));
    let source_order = ok(geo::with_group(ok(group::ordered(group::Owner::Point, "ordered", 3, &[1, 0, 2])), &src));
    let st = geo::topology(&src);
    let source_edges = ok(geo::with_edge_group(ok(edge_group::init(None, st, &ok(topology_index::create(None, st)), "edges", &|i| i == 1)), &src));
    let variants: Vec<(geo::T, geo::T, Args)> = vec![
        (src.clone(), dst.clone(), replace(&base, "points", ints(&[1, 2, 0]))),
        (src.clone(), dst.clone(), replace(&base, "vertices", ints(&[1, 2, 0]))),
        (src.clone(), dst.clone(), replace(&base, "positions", Value::Text(b"mapped".to_vec()))),
        (src.clone(), dst.clone(), replace(&base, "edge_map", Value::Record(vec![("offsets".into(), ints(&[0, 1, 2, 3])), ("values".into(), ints(&[2, 1, 0]))]))),
        (src.clone(), target_p, base.clone()),
        (src.clone(), target_topology, base.clone()),
        (source_order, dst.clone(), base.clone()),
        (source_edges, dst.clone(), base.clone()),
    ];
    let node = compile(&src, &dst, &base);
    let mut s = session::create(128, 64_000_000).unwrap();
    let context = sctx::create(ContextOptions::default()).unwrap();
    let _ = session::cook(&mut s, None, &context, &node);
    for (source, target, values) in variants {
        let changed = build::adopt_identity(&node, &compile(&source, &target, &values));
        let before = session::stats(&s);
        let output = geometry(session::cook(&mut s, None, &context, &changed));
        assert!(session::stats(&s).misses > before.misses, "changed remap semantic input hit stale cache");
        assert!(geometry_bytes(&output) == geometry_bytes(&geometry(run(&changed))), "warm remap differs from fresh output");
        let warm = session::stats(&s);
        let _ = session::cook(&mut s, None, &context, &changed);
        assert!(session::stats(&s).misses == warm.misses, "unchanged remap variant did not hit");
    }
    session::close(&mut s);
}

#[test]
fn implicit_edge_affinity_follows_the_target_topology_identity() {
    let src = source(3);
    let changed_p = ok(geo::with_positions(ok(Float3::View::of_owned_exn(vec![8.0, 7.0, 6.0], vec![-0.0, 0.0, -0.0], vec![2.0, 3.0, 4.0])), &src));
    let inputs = args(&[0, 1, 2], &[0, 1, 2], &[0], &[0, 1, 2, 3], &[0, 1, 2], "target");
    for reference in [false, true] {
        let actual = geometry(run(&compile_with(reference, false, "", &src, &changed_p, &inputs)));
        assert!(geometry_bytes(&actual) == geometry_bytes(&changed_p), "same topology/changed P lost implicit edge affinity");
        match run(&compile_with(reference, false, "", &src, &target(1, &src), &inputs)) {
            Err(d) => assert!(d.code == "E_REMAP_EDGES", "equal distinct topology did not refuse implicit ancestry"),
            Ok(_) => panic!("value-equal distinct topology implicitly inherited edge affinity"),
        }
    }
}

#[test]
fn primitive_ancestry_permutation_and_nan_payload() {
    let original = source(3);
    let repeated = target(2, &source(3));
    let map = [0i64, 1, 2, 0, 1, 2];
    let offsets: Vec<i64> = (0..7).collect();
    let src = oracle(&original, &repeated, &map, &map, &[0, 0], &offsets, &map, true);
    let src = ok(geo::with_attribute(ok(attribute::create_owned("float", Owner::Primitive, Storage::Float(Arc::new(vec![raw_with(false, 5), raw_with(false, 6)])))), &src));
    let src = ok(geo::with_group(ok(group::ordered(group::Owner::Primitive, "ordered", 2, &[1, 0])), &src));
    let dst = target(1, &src);
    let identity: Vec<i64> = (0..6).collect();
    let base = args(&identity, &identity, &[0, 1], &offsets, &identity, "mapped");
    let permuted = args(&identity, &identity, &[1, 0], &offsets, &identity, "mapped");
    let expected = oracle(&src, &dst, &identity, &identity, &[1, 0], &offsets, &identity, true);
    let mut s = session::create(32, 64_000_000).unwrap();
    let node = compile(&src, &dst, &base);
    let context = sctx::create(ContextOptions::default()).unwrap();
    let _ = session::cook(&mut s, None, &context, &node);
    let before = session::stats(&s);
    let node = build::adopt_identity(&node, &compile(&src, &dst, &permuted));
    let actual = geometry(session::cook(&mut s, None, &context, &node));
    assert!(session::stats(&s).misses > before.misses, "primitive ancestry cache input ignored");
    assert!(geometry_bytes(&actual) == geometry_bytes(&expected), "primitive NaN payload/order permutation differs");
    session::close(&mut s);
    for reference in [false, true] {
        let node = compile_with(reference, true, "", &src, &dst, &permuted);
        for &domains in &domains_list() {
            for &grain in &grains() {
                assert!(geometry_bytes(&geometry(run_with(domains, grain, None, &node))) == geometry_bytes(&expected), "primitive permutation differs by domain/grain/reference");
            }
        }
    }
}

#[test]
fn edge_ancestry_helper_trust_boundary_diagnostics() {
    // Valid geometry-host values cannot reach these native helper trust guards.
    let src = source(3);
    let dst = target(1, &src);
    let unrelated = source(3);
    let source_index = ok(topology_index::create(None, geo::topology(&src)));
    let target_index = ok(topology_index::create(None, geo::topology(&dst)));
    let (offsets, ancestors) = ([0i64, 1, 2, 3], [0i64, 1, 2]);
    let invoke = |grain: i64, t: &topology::T, groups: &[edge_group::T]| remap::edge_groups_from_ancestry(None, grain, &source_index, t, &target_index, &offsets, &ancestors, groups);
    let mine = geo::edge_groups(&src);
    let theirs = geo::edge_groups(&unrelated);
    for (result, message) in [
        (invoke(0, geo::topology(&dst), &mine), "grain must be positive"),
        (invoke(-1, geo::topology(&dst), &mine), "grain must be positive"),
        (invoke(97, geo::topology(&src), &mine), "target index affinity mismatch"),
        (invoke(97, geo::topology(&dst), &theirs), "source group affinity mismatch"),
    ] {
        match result {
            Err(actual) => assert!(rdk_core::error::message(&actual) == format!("Topology_remap.edge_groups_from_ancestry: {message}"), "edge ancestry helper trust-boundary diagnostic changed: {}", rdk_core::error::message(&actual)),
            Ok(_) => panic!("edge ancestry helper accepted an invalid native boundary"),
        }
    }
}

// Weighted carry: every owner and storage against an independent reference, the shared helper's bytes
// against Curve_join.interpolate_attribute on point and vertex attributes, mixed plain and weighted
// owners, reference/CPU, 1 and 8 domains, and the refusals.
type Mix = (Vec<i64>, Vec<i64>, Vec<f64>);

fn mix_of(count: i64, shift: i64, n: i64) -> Mix {
    let weights = [0.0, 0.25, 0.5, 0.75, 1.0, 0.4999999999, 1.5, -0.5, 0.5000000001];
    (
        (0..n).map(|i| (i * shift + shift) % count).collect(),
        (0..n).map(|i| (i * 2 + 1 + shift) % count).collect(),
        (0..n).map(|i| weights[((i + shift) as usize) % weights.len()]).collect(),
    )
}

fn mix_args(pc: &Mix, vc: &Mix, rc: &Mix, policy: &str, offsets: &[i64], ancestors: &[i64]) -> Args {
    let ints = |a: &[i64]| Value::Int_array(Arc::new(a.to_vec()));
    let floats = |a: &[f64]| Value::Floats(1, Arc::new(a.to_vec()));
    vec![
        ("pa".into(), ints(&pc.0)),
        ("pb".into(), ints(&pc.1)),
        ("pt".into(), floats(&pc.2)),
        ("va".into(), ints(&vc.0)),
        ("vb".into(), ints(&vc.1)),
        ("vt".into(), floats(&vc.2)),
        ("ra".into(), ints(&rc.0)),
        ("rb".into(), ints(&rc.1)),
        ("rt".into(), floats(&rc.2)),
        ("positions".into(), Value::Text(policy.as_bytes().to_vec())),
        ("edge_map".into(), Value::Record(vec![("offsets".into(), ints(offsets)), ("values".into(), ints(ancestors))])),
    ]
}

fn mix_body(points: bool, vertices: bool, primitives: bool) -> String {
    format!(
        "(sop/remap source target {} {} {} :positions positions :edges edge_map)",
        if points { ":point_mix {:a pa :b pb :t pt}" } else { ":points pa" },
        if vertices { ":vertex_mix {:a va :b vb :t vt}" } else { ":vertices va" },
        if primitives { ":primitive_mix {:a ra :b rb :t rt}" } else { ":primitives ra" },
    )
}

fn nearest(m: &Mix) -> Vec<i64> {
    m.2.iter().enumerate().map(|(i, w)| if *w < 0.5 { m.0[i] } else { m.1[i] }).collect()
}

fn mix(a: f64, b: f64, t: f64) -> f64 {
    let d = b - a;
    if d.is_finite() { a + d * t } else { a * (1.0 - t) + b * t }
}

fn blend_plane(m: &Mix, plane: &[f64]) -> Vec<f64> {
    m.2.iter().enumerate().map(|(i, w)| mix(plane[m.0[i] as usize], plane[m.1[i] as usize], *w)).collect()
}

fn reference_attribute(carry: &Mix, a: &attribute::T) -> attribute::T {
    let plane = |v: &[f64]| blend_plane(carry, v);
    let storage = match ok(attribute::storage(a)) {
        Storage::Float(v) => Storage::Float(Arc::new(plane(&v))),
        Storage::Float2(v) => {
            let v = Float2::View::view(&v);
            Storage::Float2(ok(Float2::of_owned(plane(&v.x), plane(&v.y))))
        }
        Storage::Float3(v) => {
            let v = ok(Float3::View::view(&v));
            let (mut x, mut y, mut z) = (plane(&v.x), plane(&v.y), plane(&v.z));
            if attribute::name(a) == "N" {
                for i in 0..x.len() {
                    let length = (x[i] * x[i] + y[i] * y[i] + z[i] * z[i]).sqrt();
                    if length > 1e-20 {
                        x[i] /= length;
                        y[i] /= length;
                        z[i] /= length;
                    }
                }
            }
            Storage::Float3(ok(Float3::View::of_owned_exn(x, y, z)))
        }
        Storage::Float4(v) => {
            let v = Float4::View::view(&v);
            Storage::Float4(ok(Float4::of_owned(plane(&v.x), plane(&v.y), plane(&v.z), plane(&v.w))))
        }
        _ => ok(attribute::storage(&ok(remap::attribute(None, 97, None, &nearest(carry), a)))),
    };
    ok(attribute::create_owned(attribute::name(a), attribute::owner(a), storage))
}

/// Weighted arithmetic does not define which NaN payload (or sign) survives, so a NaN is compared as a NaN.
fn nan_free(g: &geo::T) -> Vec<u8> {
    let canon = |x: f64| if x.is_nan() { f64::from_bits(0x7ff8000000000000) } else { x };
    let cv = |v: &[f64]| v.iter().map(|x| canon(*x)).collect::<Vec<_>>();
    let attrs: Vec<attribute::T> = geo::attributes(g)
        .iter()
        .map(|a| {
            let storage = match ok(attribute::storage(a)) {
                Storage::Float(v) => Storage::Float(Arc::new(cv(&v))),
                Storage::Float2(v) => {
                    let v = Float2::View::view(&v);
                    Storage::Float2(ok(Float2::of_owned(cv(&v.x), cv(&v.y))))
                }
                Storage::Float3(v) => {
                    let v = ok(Float3::View::view(&v));
                    Storage::Float3(ok(Float3::View::of_owned_exn(cv(&v.x), cv(&v.y), cv(&v.z))))
                }
                Storage::Float4(v) => {
                    let v = Float4::View::view(&v);
                    Storage::Float4(ok(Float4::of_owned(cv(&v.x), cv(&v.y), cv(&v.z), cv(&v.w))))
                }
                Storage::FloatArray(v) => {
                    let v = FloatArray::View::view(&v);
                    Storage::FloatArray(ok(FloatArray::create_owned(v.offsets.to_vec(), cv(&v.values))))
                }
                other => other,
            };
            ok(attribute::create_owned(attribute::name(a), attribute::owner(a), storage))
        })
        .collect();
    let p = ok(Float3::View::view(geo::positions(g)));
    let positions = ok(Float3::View::of_owned_exn(cv(&p.x), cv(&p.y), cv(&p.z)));
    geometry_bytes(&ok(geo::create(positions, geo::topology(g).clone(), Some(attrs), Some(geo::groups(g)), Some(geo::edge_groups(g)))))
}

#[test]
fn weighted_remap_every_owner_and_storage() {
    let src = source_with(true, 4);
    let (grains, domains): (Vec<i64>, Vec<i64>) = (vec![1, 97], vec![1, 8]);
    for copies in [0i64, 1, 2] {
        let dst = target(copies, &src);
        let (np, nv, nr) = (geo::point_count(&dst), geo::vertex_count(&dst), geo::primitive_count(&dst));
        let pc = mix_of(geo::point_count(&src), 1, np);
        let vc = mix_of(geo::vertex_count(&src), 2, nv);
        let rc = mix_of(geo::primitive_count(&src), 3, nr);
        let edges = edges_of(&dst);
        let se = edges_of(&src);
        let offsets: Vec<i64> = (0..=edges).collect();
        let ancestors: Vec<i64> = (0..edges).map(|i| i % se).collect();
        for (points, vertices, primitives) in [(true, true, true), (true, false, false), (false, true, false), (false, false, true), (true, true, false)] {
            let carry = |owner: Owner, flagged: bool| -> Mix {
                match (owner, flagged) {
                    (Owner::Point, true) => pc.clone(),
                    (Owner::Vertex, true) => vc.clone(),
                    (_, true) => rc.clone(),
                    (Owner::Point, false) => (pc.0.clone(), pc.0.clone(), vec![0.0; np as usize]),
                    (Owner::Vertex, false) => (vc.0.clone(), vc.0.clone(), vec![0.0; nv as usize]),
                    (_, false) => (rc.0.clone(), rc.0.clone(), vec![0.0; nr as usize]),
                }
            };
            let flag = |owner: Owner| match owner {
                Owner::Point => points,
                Owner::Vertex => vertices,
                Owner::Primitive => primitives,
                Owner::Detail => false,
            };
            let group_flag = |owner: group::Owner| match owner {
                group::Owner::Point => points,
                group::Owner::Vertex => vertices,
                group::Owner::Primitive => primitives,
            };
            let base = oracle(&src, &dst, &pc.0, &vc.0, &rc.0, &offsets, &ancestors, true);
            let attributes: Vec<attribute::T> = geo::attributes(&src)
                .iter()
                .map(|a| match attribute::owner(a) {
                    Owner::Detail => a.clone(),
                    owner => {
                        if flag(owner) {
                            reference_attribute(&carry(owner, true), a)
                        } else {
                            let map = match owner {
                                Owner::Point => &pc.0,
                                Owner::Vertex => &vc.0,
                                _ => &rc.0,
                            };
                            ok(attribute::create_owned(attribute::name(a), owner, ok(attribute::storage(&ok(remap::attribute(None, 97, None, map, a))))))
                        }
                    }
                })
                .collect();
            let groups: Vec<group::T> = geo::groups(&src)
                .iter()
                .map(|g| {
                    let owner = group::owner(g);
                    let m = match owner {
                        group::Owner::Point => &pc,
                        group::Owner::Vertex => &vc,
                        group::Owner::Primitive => &rc,
                    };
                    ok(remap::group(None, 97, &if group_flag(owner) { nearest(m) } else { m.0.clone() }, g))
                })
                .collect();
            let v = ok(Float3::View::view(geo::positions(&src)));
            let positions = if points {
                ok(Float3::View::of_owned_exn(blend_plane(&pc, &v.x), blend_plane(&pc, &v.y), blend_plane(&pc, &v.z)))
            } else {
                ok(Float3::View::of_owned_exn(pc.0.iter().map(|i| v.x[*i as usize]).collect(), pc.0.iter().map(|i| v.y[*i as usize]).collect(), pc.0.iter().map(|i| v.z[*i as usize]).collect()))
            };
            let expected = ok(geo::create(positions, geo::topology(&dst).clone(), Some(attributes), Some(groups), Some(geo::edge_groups(&base))));
            // the shared helper is Curve_join' rule, byte for byte
            if points && vertices {
                for a in geo::attributes(&src) {
                    if matches!(attribute::owner(&a), Owner::Point | Owner::Vertex) {
                        let curve = ok(rdk_ops::curve_join::interpolate_attribute(None, None, &pc.0, &pc.1, &pc.2, &vc.0, &vc.1, &vc.2, &a));
                        let got = geo::find_attribute(attribute::owner(&a), attribute::name(&a), &expected).unwrap();
                        let attach = |x: attribute::T| nan_free(&ok(geo::create(geo::positions(&base).clone(), geo::topology(&base).clone(), Some(vec![x]), None, None)));
                        assert!(attach(curve) == attach(got), "weighted remap differs from Curve_join.interpolate_attribute ({} copies {copies})", attribute::name(&a));
                    }
                }
            }
            for reference in [false, true] {
                let node = compile_with(reference, true, &format!("(let* [out {}] out)", mix_body(points, vertices, primitives)), &src, &dst, &mix_args(&pc, &vc, &rc, "mapped", &offsets, &ancestors));
                for &d in &domains {
                    for &grain in &grains {
                        let before = geometry_bytes(&src);
                        assert!(nan_free(&geometry(run_with(d, grain, None, &node))) == nan_free(&expected), "weighted remap bytes differ from the reference");
                        assert!(geometry_bytes(&src) == before, "weighted remap mutated its source");
                    }
                }
            }
        }
    }
}

#[test]
fn weighted_remap_refusals() {
    let src = source_with(true, 4);
    let dst = target(1, &src);
    let (np, nv, nr) = (geo::point_count(&dst), geo::vertex_count(&dst), geo::primitive_count(&dst));
    let edges = edges_of(&dst);
    let ok_args = mix_args(
        &mix_of(4, 1, np),
        &mix_of(6, 2, nv),
        &mix_of(2, 3, nr),
        "target",
        &(0..=edges).collect::<Vec<_>>(),
        &(0..edges).map(|i| i % 3).collect::<Vec<_>>(),
    );
    let with = |key: &str, value: Value| replace(&ok_args, key, value);
    let all = mix_body(true, true, true);
    let ints = |a: Vec<i64>| Value::Int_array(Arc::new(a));
    for (key, value, code) in [
        ("pt", Value::Floats(1, Arc::new(vec![0.5; np as usize - 1])), "E_REMAP_COUNT"),
        ("pa", ints(vec![4; np as usize]), "E_REMAP_INDEX"),
        ("pb", ints(vec![-1; np as usize]), "E_REMAP_INDEX"),
        ("va", ints(vec![6; nv as usize]), "E_REMAP_INDEX"),
        ("rb", ints(vec![-1; nr as usize]), "E_REMAP_INDEX"),
    ] {
        match run(&compile_with(false, true, &all, &src, &dst, &with(key, value))) {
            Err(d) => assert!(d.code == code, "weighted remap {key} code {} is not {code}", d.code),
            Ok(_) => panic!("weighted remap accepted a bad {key}"),
        }
    }
    // a non-finite weight never reaches the kernel: the array input is refused first
    for weight in [f64::NAN, f64::INFINITY] {
        match compile_result(false, true, &all, &src, &dst, &with("pt", Value::Floats(1, Arc::new(vec![weight; np as usize])))) {
            Err(m) => assert!(m.starts_with("E_NONFINITE"), "non-finite weight refused with another code: {m}"),
            Ok(node) => assert!(run(&node).is_err(), "non-finite weight accepted"),
        }
    }
    for text in [
        "(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :points (array/int 0) :point_mix {:a (array/int 0) :b (array/int 0) :t (array/float 0 0.0)} :vertices (array/int 0) :primitives (array/int 0))))",
        "(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :vertices (array/int 0) :primitives (array/int 0))))",
        "(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :point_mix {:a (array/int 0) :b (array/int 0)} :vertices (array/int 0) :primitives (array/int 0))))",
    ] {
        let forms = flow_ok(syntax::parse(text));
        assert!(lower::workspace(Some(factories(&src, &dst)), Vec::new(), lower::Options::default(), &forms).is_err(), "malformed weighted remap lowered");
    }
}

#[test]
fn lead_groups_lists_target_groups_first() {
    // [:lead_groups true] lists the target's own groups and edge groups ahead of the carried ones.
    let src = source(3);
    let dst = target(1, &src);
    let dst = ok(geo::with_group(ok(group::init(None, group::Owner::Point, "lead", 3, &|i| i == 1)), &dst));
    let t = geo::topology(&dst).clone();
    let index = ok(topology_index::create(None, &t));
    let dst = ok(geo::with_edge_group(ok(edge_group::init(None, &t, &index, "lead_edges", &|i| i == 0)), &dst));
    let identity = [0i64, 1, 2];
    let inputs = args(&identity, &identity, &[0], &[0, 1, 2, 3], &[0, 1, 2], "target");
    let lead = "(sop/remap source target :points points :vertices vertices :primitives primitives :positions positions :edges edge_map :lead_groups true)";
    for reference in [false, true] {
        let carried = geometry(run(&compile_with(reference, true, "", &src, &dst, &inputs)));
        let led = geometry(run(&compile_with(reference, true, lead, &src, &dst, &inputs)));
        let names = |g: &geo::T| geo::groups(g).iter().map(|x| group::name(x).to_owned()).collect::<Vec<_>>();
        let mut expected = vec!["lead".to_owned()];
        expected.extend(names(&carried));
        assert!(names(&led) == expected, "lead_groups did not list the target groups first");
        assert!(!names(&carried).contains(&"lead".to_owned()), "remap kept target groups without lead_groups");
        let edge_names = |g: &geo::T| geo::edge_groups(g).iter().map(|x| edge_group::name(x).to_owned()).collect::<Vec<_>>();
        let mut expected = vec!["lead_edges".to_owned()];
        expected.extend(edge_names(&carried));
        assert!(edge_names(&led) == expected, "lead_groups did not list the target edge groups first");
    }
}
