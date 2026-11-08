//! Port of lib/flow_sop/test_group_kernel.ml: authored group writes, queries, promotion and folds
//! against the native group storage, at one and eight domains. The OCaml file's `--bench-promotion`
//! section (a one-million-point benchmark run only with that flag) is not ported.
use super::support::*;
use crate::{attribute_kernel, catalog, lisp_sop, lower, manifest, operators, value_lane};
use flow_graph::flow_edit::{apply_checked_with, ArgKey, Op};
use flow_graph::projection as proj;
use flow_ir::flow_ir::Executor;
use flow_syntax::syntax::{self, Node};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{Value, source_value};
use rays_diagnostic::Diagnostic;
use rdk::attribute::{self, Owner, Storage};
use rdk::geometry as geo;
use rdk::packed::Float3;
use rdk::{edge_group, group, topology, topology_index};
use rdk_core::element_selection as selection;
use sop::context::Options as ContextOptions;
use sop::edit_graph as edit;
use sop::node::{self, build, CookMode};
use sop::{context as sctx, payload, session};
use std::sync::Arc;

pub(super) fn ok<R>(r: Result<R, rdk::error::T>) -> R {
    r.unwrap_or_else(|e| panic!("{}", rdk::error::to_string(&e)))
}

pub(super) fn flow_ok<R>(r: Result<R, Diagnostic>) -> R {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

type Owners = [(&'static str, Option<group::Owner>); 4];
const OWNERS: Owners = [("point", Some(group::Owner::Point)), ("vertex", Some(group::Owner::Vertex)), ("primitive", Some(group::Owner::Primitive)), ("edge", None)];

fn bytes(g: &geo::T) -> Vec<u8> {
    geometry_bytes(g)
}

fn text(s: &str) -> Value {
    Value::Text(s.as_bytes().to_vec())
}

fn ints(a: &[i64]) -> Value {
    Value::Int_array(Arc::new(a.to_vec()))
}

fn edge_count(g: &geo::T) -> i64 {
    topology_index::edge_count(&ok(topology_index::create(None, geo::topology(g))))
}

fn count(g: &geo::T, owner: Option<group::Owner>) -> i64 {
    match owner {
        Some(group::Owner::Point) => geo::point_count(g),
        Some(group::Owner::Vertex) => geo::vertex_count(g),
        Some(group::Owner::Primitive) => geo::primitive_count(g),
        None => edge_count(g),
    }
}

fn fixtures() -> Vec<geo::T> {
    let empty = ok(topology::empty(0));
    let two = ok(topology::create_owned(4, vec![0, 1, 2, 2, 1, 0], vec![0, 3, 6], vec![topology::PrimitiveKind::Polygon, topology::PrimitiveKind::OpenPolyline]));
    [empty, two]
        .into_iter()
        .map(|t| {
            let n = topology::point_count(&t) as usize;
            let positions = ok(Float3::View::of_owned_exn((0..n).map(|i| i as f64).collect(), vec![0.0; n], vec![0.0; n]));
            ok(geo::create(positions, t, None, None, None))
        })
        .collect()
}

fn native_write(order: Option<&[i64]>, owner: Option<group::Owner>, name: &str, mask: &[i64], g: &geo::T) -> geo::T {
    match owner {
        None => {
            assert!(order.is_none());
            let t = geo::topology(g);
            let index = ok(topology_index::create(None, t));
            ok(geo::with_edge_group(ok(edge_group::init(None, t, &index, name, &|i| mask[i as usize] == 1)), g))
        }
        Some(owner) => {
            let made = match order {
                None => ok(group::init(None, owner, name, mask.len() as i64, &|i| mask[i as usize] == 1)),
                Some(order) => ok(group::ordered(owner, name, mask.len() as i64, order)),
            };
            ok(geo::with_group(made, g))
        }
    }
}

fn lookup_of(g: Option<geo::T>) -> attribute_kernel::GeometryLookup {
    Arc::new(move |_| g.clone())
}

fn group_source(name: &str, args: Vec<(&str, Value)>) -> Value {
    source_value(name, Ty::Any, [("geometry", Value::Deferred(ty::geometry(), 7))].into_iter().chain(args).map(|(k, v)| (k.to_owned(), v)).collect())
}

fn query(g: &geo::T, owner: &str, name: &str, component: Option<&str>) -> Value {
    let mut args = vec![("owner", text(owner)), ("name", text(name))];
    if let Some(c) = component {
        args.push(("component", text(c)));
    }
    let source = group_source(if component.is_none() { "sop/group" } else { "sop/group_order" }, args);
    flow_ok(attribute_kernel::resolve(None, &lookup_of(Some(g.clone())), &source))
}

fn catalog0() -> flow_check::catalog::Catalog {
    flow_check::catalog::Catalog { version: 1, kinds: Vec::new() }
}

fn checked_in(catalog: &flow_check::catalog::Catalog, source: &str) -> flow_check::workspace::Workspace {
    let forms = parse(source);
    match flow_check::workspace::check(operators::all(), false, false, None, catalog, &forms) {
        (Some(checked), d) if d.is_empty() => checked,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

/// `body` bound as `tested` in a graph whose only input is the geometry `geo`.
pub(super) fn evaluated(g: &geo::T, body: &str) -> (Value, flow_ir::core_exec::ResolveFn) {
    let checked = checked_in(&catalog0(), &format!("(workspace groups (graph g :context sop [(geo : geometry nil)] (let* [tested {body}] geo)))"));
    let inputs = vec![("g".to_owned(), vec![("geo".to_owned(), Value::Deferred(ty::geometry(), 7))])];
    let evaluation = flow_ok(flow_eval::static_with(&checked, true, Some(&inputs), None));
    let value = evaluation.records.iter().find(|(path, _)| path == &vec!["g".to_owned(), "tested".to_owned()]).map(|(_, values)| values[0].1.clone()).expect("tested record");
    let lookup = lookup_of(Some(g.clone()));
    (value, Arc::new(move |v| attribute_kernel::resolve(None, &lookup, v)))
}

fn exact(g: &geo::T, body: &str, expected: &Value) {
    let (value, resolve) = evaluated(g, body);
    let program = flow_ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
    exact_prepared(&value, &program, resolve, body, expected);
}

fn exact_prepared(value: &Value, program: &Executor::Program, resolve: flow_ir::core_exec::ResolveFn, body: &str, expected: &Value) {
    for domains in [1i64, 8] {
        parallel::run_unit(Some(domains), &|| {
            let live = input::at_time(0.25);
            assert!(&flow_ok(flow_eval::Host::force_reference(value, &live, None, None, Some(resolve.clone()))) == expected, "{body}: reference");
            let options = Executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() };
            assert!(&flow_ok(Executor::force(program, &options, &live)) == expected, "{body}: strict");
            Ok(())
        })
        .unwrap();
    }
}

fn create_session() -> session::T {
    session::create(32, 1_000_000).unwrap()
}

fn cook_at(s: &mut session::T, time: f64, domains: i64, n: &node::T) -> geo::T {
    let context = sctx::create(ContextOptions { domains: Some(domains), time: Some(time), ..Default::default() }).unwrap();
    match session::cook(s, None, &context, n) {
        Ok(output) => flow_ok(payload::geometry(&output.payload)),
        Err(d) => panic!("{}", rays_diagnostic::error_to_string(&d)),
    }
}

fn cook(s: &mut session::T, domains: i64, n: &node::T) -> geo::T {
    cook_at(s, 0.0, domains, n)
}

fn int_array(data: &[i64]) -> String {
    data.iter().fold("(array/int 0 0)".to_owned(), |array, value| format!("(array/concat {array} (array/int 1 {value}))"))
}

fn write_node(g: &geo::T, owner: &str, mask: &[i64], order: Option<&str>) -> node::T {
    let order = order.map_or(String::new(), |o| format!(" :order {o}"));
    lisp_sop::node(&[("source".to_owned(), lisp_sop::snapshot(g))], &format!("(sop/with_group (sop/ext_source) {} :name \"selected\" :owner {owner}{order})", int_array(mask)))
}

#[test]
fn native_owner_masks_stable_order_and_round_trips() {
    native_owner_masks(false);
}

#[test]
fn native_owner_masks_defnode_round_trip() {
    native_owner_masks(true);
}

fn native_owner_masks(defnode_round_trip: bool) {
    for geometry in fixtures() {
        for (owner, native_owner) in OWNERS {
            let n = count(&geometry, native_owner);
            let mask: Vec<i64> = (0..n).map(|i| i64::from(i % 2 == 0)).collect();
            let expected = native_write(None, native_owner, "selected", &mask, &geometry);
            let node = write_node(&geometry, owner, &mask, None);
            let facts = node::facts(&node);
            assert!(facts.cook_mode == CookMode::Generic);
            assert!(facts.reads == ["*"] && facts.writes == ["*"]);
            let mut s = create_session();
            for domains in [1i64, 8] {
                let output = cook(&mut s, domains, &node);
                assert!(bytes(&output) == bytes(&expected));
                exact(&output, &format!("(sop/group geo :name \"selected\" :owner {owner})"), &ints(&mask));
                exact(&output, &format!("(sop/group_order geo :name \"selected\" :owner {owner})"), &Value::Record(vec![("ordered".into(), Value::Bool(false)), ("indices".into(), ints(&[]))]));
                if defnode_round_trip {
                let copy = lisp_sop::workspace_node(
                    &[("source".to_owned(), lisp_sop::snapshot(&output))],
                    &format!(
                        "(workspace groups (defnode sop/test_mask_roundtrip [(geo : geometry)] (sop/with_group geo (exact (map (fn [(value : int)] value) (sop/group geo :name \"selected\" :owner {owner}))) :name \"selected\" :owner {owner} :order (sop/group_order geo :name \"selected\" :owner {owner}))) (graph g :context sop (sop/test_mask_roundtrip (sop/ext_source))))"
                    ),
                );
                assert!(bytes(&cook(&mut s, domains, &copy)) == bytes(&output));
                }
            }
            session::close(&mut s);
            if native_owner.is_some() {
                let selected: Vec<i64> = mask.iter().enumerate().filter(|(_, v)| **v == 1).map(|(i, _)| i as i64).rev().collect();
                for (mask, order) in [(mask.clone(), selected), (vec![0; n as usize], vec![])] {
                    let expected = native_write(Some(&order), native_owner, "selected", &mask, &geometry);
                    let node = write_node(&geometry, owner, &mask, Some(&int_array(&order)));
                    let mut s = create_session();
                    for domains in [1i64, 8] {
                        let output = cook(&mut s, domains, &node);
                        assert!(bytes(&output) == bytes(&expected));
                        exact(&output, &format!("(sop/group_order geo :name \"selected\" :owner {owner})"), &Value::Record(vec![("ordered".into(), Value::Bool(true)), ("indices".into(), ints(&order))]));
                        let copy = lisp_sop::node(
                            &[("source".to_owned(), lisp_sop::snapshot(&output))],
                            &format!("(let* [geo (sop/ext_source)] (sop/with_group geo (sop/group geo :name \"selected\" :owner {owner}) :name \"selected\" :owner {owner} :order (sop/group_order geo :name \"selected\" :owner {owner})))"),
                        );
                        assert!(bytes(&cook(&mut s, domains, &copy)) == bytes(&output));
                    }
                    session::close(&mut s);
                }
            }
            let mask_value = ints(&mask);
            assert!(query(&expected, owner, "selected", None) == mask_value);
            // the query hands out a copy: changing it leaves the geometry's group alone
            if let Value::Int_array(data) = query(&expected, owner, "selected", None) {
                let mut copy = (*data).clone();
                copy.iter_mut().for_each(|v| *v = 0);
            }
            assert!(query(&expected, owner, "selected", None) == mask_value);
        }
    }
}

#[test]
fn wildcard_names_and_refusals() {
    let geometry = fixtures().remove(1);
    let names = ["wall_a", "wall_b", "wall_1", "floor", "literal*", "\u{17e}arek"];
    for (owner, native_owner) in OWNERS {
        let mut g = geometry.clone();
        for name in names {
            g = native_write(None, native_owner, name, &vec![1; count(&geometry, native_owner) as usize], &g);
        }
        for pattern in ["", "*", "wall*", "wall?", "wall_[a-b]", "wall_[!0-9]", "* ^wall*", "^wall* wall_b", "literal\\*", "\u{17e}*"] {
            let compiled = ok(rdk::attribute_pattern::compile(pattern));
            let printed = flow_syntax::lisp::quote_string(pattern.as_bytes());
            let expected: Vec<Vec<u8>> = names.iter().filter(|n| rdk::attribute_pattern::matches(&compiled, n)).map(|n| n.as_bytes().to_vec()).collect();
            exact(&g, &format!("(sop/group_names geo {printed} :owner {owner})"), &Value::Text_array(Arc::new(expected)));
        }
    }
    let mut s = create_session();
    let refused = |s: &mut session::T, code: &str, node: &node::T| {
        let context = sctx::create(ContextOptions::default()).unwrap();
        match session::cook(s, None, &context, node) {
            Err(d) => assert!(d.code == code, "{} is not {code}", d.code),
            Ok(_) => panic!("cook accepted a bad group write ({code})"),
        }
    };
    for (owner, native_owner) in OWNERS {
        let n = count(&geometry, native_owner) as usize;
        refused(&mut s, "E_GROUP_COUNT", &write_node(&geometry, owner, &vec![0; n + 1], None));
        refused(&mut s, "E_GROUP_MASK", &write_node(&geometry, owner, &vec![2; n], None));
        match native_owner {
            None => refused(&mut s, "E_GROUP_OWNER", &write_node(&geometry, owner, &vec![0; n], Some("(array/int 0 0)"))),
            Some(_) => {
                for order in [vec![n as i64], vec![0, 0], vec![]] {
                    refused(&mut s, "E_GROUP_ORDER", &write_node(&geometry, owner, &vec![1; n], Some(&int_array(&order))));
                }
            }
        }
    }
    session::close(&mut s);
    let source = |name: &str, args: Vec<(&str, Value)>| group_source(name, args);
    let refused_value = |code: &str, r: Result<Value, Diagnostic>| match r {
        Err(d) => assert!(d.code == code, "{} is not {code}", d.code),
        Ok(_) => panic!("resolve accepted a bad group query ({code})"),
    };
    refused_value("E_DATA_SOURCE", attribute_kernel::resolve(None, &lookup_of(None), &source("sop/group", vec![("name", text("missing"))])));
    let present = lookup_of(Some(geometry.clone()));
    refused_value("E_GROUP_MISSING", attribute_kernel::resolve(None, &present, &source("sop/group", vec![("name", text("missing"))])));
    refused_value("E_GROUP_OWNER", attribute_kernel::resolve(None, &present, &source("sop/group", vec![("name", text("missing")), ("owner", text("detail"))])));
    for pattern in ["[", "\\", "[z-a]"] {
        refused_value("E_GROUP_PATTERN", attribute_kernel::resolve(None, &present, &source("sop/group_names", vec![("pattern", text(pattern))])));
    }
}

fn path(binding: &str) -> Vec<String> {
    vec!["g".to_owned(), binding.to_owned()]
}

#[test]
fn group_cards_expose_typed_masks_names_and_order() {
    let cat = flow_ok(catalog::of_factories(manifest::VERSION, &[], None, &sop::nodes::factories()));
    let checker = |c: &flow_check::catalog::Catalog, forms: &[syntax::Syntax]| catalog::check_workspace(operators::all(), false, None, c, forms);
    let forms = parse(
        r#"(workspace group_projection (graph g :context sop
    (let* [geo (sop/line) mask (sop/group geo :name "selected" :owner point)
      names (sop/group_names geo "wall*" :owner vertex)
      order (sop/group_order geo :name "selected" :owner primitive)
      result (sop/with_group geo mask :name "copy" :owner point)] result)))"#,
    );
    let checked = match checker(&cat, &forms) {
        (Some(c), d) if d.is_empty() => c,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let pane = proj::of_graph(&cat, &checked, "g").unwrap();
    for (binding, ty) in [
        ("mask", Ty::Array(Box::new(Ty::Int))),
        ("names", Ty::Array(Box::new(Ty::Text))),
        ("order", Ty::Record(vec![("ordered".into(), Ty::Bool), ("indices".into(), Ty::Array(Box::new(Ty::Int)))])),
        ("result", ty::geometry()),
    ] {
        let node = proj::find(&pane, &path(binding)).unwrap();
        assert!(node.ty == ty);
        assert!(node.rows.iter().any(|row| row.label == "owner"));
    }
    for binding in ["mask", "names", "order", "result"] {
        let mut b = flow_syntax::arena::SyntaxBuilder::new();
        let name = b.intern("edge");
        let id = b.push(0, Node::Sym(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
        let op = Op::SetArg { node: path(binding), key: ArgKey::Kw("owner".to_owned()), sub: vec![], value: b.freeze().handle(id) };
        let (_, checked) = flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &op));
        let pane = proj::of_graph(&cat, &checked, "g").unwrap();
        let node = proj::find(&pane, &path(binding)).unwrap();
        let row = node.rows.iter().find(|row| row.label == "owner").unwrap();
        assert!(flow_syntax::lisp::flat(row.expr.as_ref().unwrap()) == "\"edge\"");
    }
    let forms = parse(
        r#"(workspace group_wire (graph g :context sop
    (let* [geo (sop/line) mask (sop/group geo :name "selected" :owner point)
      result (sop/with_group geo (array/int 0 0) :name "copy" :owner point)] result)))"#,
    );
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let name = b.intern("mask");
    let id = b.push(0, Node::Sym(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    let op = Op::SetArg { node: path("result"), key: ArgKey::Pos(1), sub: vec![], value: b.freeze().handle(id) };
    flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &op));
    for text in [
        "(workspace bad (graph g :context sop (sop/with_group nil (array/int 0 0) :name \"x\" :order 2)))",
        "(workspace bad (graph g :context sop (sop/group nil :name \"\")))",
        "(workspace bad (graph g :context sop (sop/group nil :name \"x\" :owner detail)))",
    ] {
        let (_, diagnostics) = checker(&cat, &parse(text));
        assert!(diagnostics.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity), "{text}");
    }
}

#[test]
fn captured_geometry_and_group_changes_invalidate_caches() {
    let geometry = fixtures().remove(1);
    let n = geo::point_count(&geometry);
    let source = |mask: &[i64]| native_write(None, Some(group::Owner::Point), "selected", mask, &geometry);
    let first = source(&vec![1; n as usize]);
    let second = source(&vec![0; n as usize]);
    let node = lisp_sop::node(
        &[("target".to_owned(), lisp_sop::snapshot(&geometry)), ("source".to_owned(), lisp_sop::snapshot(&first))],
        "(sop/with_group (sop/ext_target) (sop/group (sop/ext_source) :name \"selected\") :name \"copy\")",
    );
    assert!(node::inputs(&node).len() == 2);
    let expected = native_write(None, Some(group::Owner::Point), "copy", &vec![1; n as usize], &geometry);
    let mut s = create_session();
    assert!(bytes(&cook(&mut s, 1, &node)) == bytes(&expected));
    let before = session::stats(&s);
    assert!(bytes(&cook(&mut s, 8, &node)) == bytes(&expected));
    assert!(session::stats(&s).misses == before.misses);
    let mut inputs = node::inputs(&node);
    inputs[1] = lisp_sop::snapshot(&second);
    let changed = build::adopt_identity(&node, &build::rebuild_with_inputs(&node, &inputs));
    let expected = native_write(None, Some(group::Owner::Point), "copy", &vec![0; n as usize], &geometry);
    assert!(bytes(&cook(&mut s, 8, &changed)) == bytes(&expected));
    assert!(session::stats(&s).misses > before.misses);
    session::close(&mut s);

    let (values, _) = evaluated(&first, "(sop/group geo :name \"selected\")");
    let read = attribute_kernel::node(
        None,
        &attribute_kernel::NodeOptions { owner: Owner::Point, kind: "int".to_owned(), source: "group-query".to_owned(), ..Default::default() },
        "mark",
        &values,
        &[7],
        &[lisp_sop::snapshot(&first)],
    );
    assert!(node::facts(&read).reads.is_empty());
    let mut s = create_session();
    let assert_mask = |output: &geo::T, mask: Vec<i64>| match attribute::storage(&geo::find_attribute(Owner::Point, "mark", output).unwrap()) {
        Ok(Storage::Int(data)) => assert!(data == mask),
        _ => panic!("mark is not an int attribute"),
    };
    assert_mask(&cook(&mut s, 1, &read), vec![1; n as usize]);
    let before = session::stats(&s);
    let changed = build::adopt_identity(&read, &build::rebuild_with_inputs(&read, &[lisp_sop::snapshot(&second)]));
    assert_mask(&cook(&mut s, 8, &changed), vec![0; n as usize]);
    assert!(session::stats(&s).misses > before.misses);
    session::close(&mut s);
}

#[test]
fn live_strict_multi_block_masks_preserve_native_bytes() {
    let n = 5001usize;
    let geometry = ok(geo::create(ok(Float3::View::of_owned_exn(vec![0.0; n], vec![0.0; n], vec![0.0; n])), ok(topology::empty(n as i64)), None, None, None));
    let node = lisp_sop::workspace_node(
        &[("source".to_owned(), lisp_sop::snapshot(&geometry))],
        r#"(workspace live_groups
    (defnode sop/test_live_group [(geo : geometry)]
      (sop/with_group geo (exact (map (fn [(value : int)] (if (< t 0.5) value 0))
        (array/int (sop/count geo :points) 1))) :name "selected"))
    (graph g :context sop (sop/test_live_group (sop/ext_source))))"#,
    );
    for domains in [1i64, 8] {
        let mut s = create_session();
        for (time, selected) in [(0.0, 1i64), (0.75, 0), (0.25, 1)] {
            let output = cook_at(&mut s, time, domains, &node);
            assert!(bytes(&output) == bytes(&native_write(None, Some(group::Owner::Point), "selected", &vec![selected; n], &geometry)));
        }
        session::close(&mut s);
    }
}

#[test]
fn group_frame_folds_recook_exact_masks() {
    let forms = parse(
        r#"(workspace folded_group (graph g :context sop
    (let* [geo (sop/line) counter (state [previous 0] (int/add previous 1))]
      (sop/with_group geo (exact (map (fn [(value : int)]
        (if (int/eq (int/mod counter 2) 1) value 0)) (array/int (sop/count geo :points) 1)))
        :name "selected"))))"#,
    );
    for domains in [1i64, 8] {
        let lowered = flow_ok(lower::workspace(None, Vec::new(), lower::Options::default(), &forms));
        let graph = lowered.graphs[0].clone();
        let lane = value_lane::create(None);
        let mut s = create_session();
        for frame in [0i64, 1, 2] {
            let time = frame as f64 * 0.25;
            let live = input::Frame { frame, ..input::at_time(time) };
            let resolved = flow_ok(value_lane::resolve(None, Some(&live), &lane, time, &graph.network));
            let node = edit::compile_node(&resolved.geometry, graph.root.unwrap()).unwrap();
            let context = sctx::create(ContextOptions { input: Some(live), time: Some(time), frame: Some(frame), domains: Some(domains), ..Default::default() }).unwrap();
            let get = |s: &mut session::T, n: &node::T| match session::cook(s, None, &context, n) {
                Ok(output) => flow_ok(payload::geometry(&output.payload)),
                Err(d) => panic!("{}", rays_diagnostic::error_to_string(&d)),
            };
            let original = get(&mut s, &node::inputs(&node)[0]);
            let expected = native_write(None, Some(group::Owner::Point), "selected", &vec![(frame + 1) % 2; geo::point_count(&original) as usize], &original);
            let got = get(&mut s, &node);
            assert!(bytes(&got) == bytes(&expected), "frame {frame} domains {domains}");
        }
        session::close(&mut s);
    }
}

#[test]
fn glob_literal_rewrite_and_group_entries() {
    let geometry = fixtures().remove(1);
    exact(&geometry, r#"(map (fn [(value : text)] (text/glob_literal value)) (array/text 5001 "a*"))"#, &Value::Text_array(Arc::new(vec![b"a\\*".to_vec(); 5001])));
    exact(&geometry, r#"(map (fn [(value : text)] (text/glob_rewrite "a*" "b*" value)) (array/text 5001 "a猫"))"#, &Value::Text_array(Arc::new(vec!["b猫".as_bytes().to_vec(); 5001])));
    let cat = catalog0();
    let checker = |c: &flow_check::catalog::Catalog, forms: &[syntax::Syntax]| flow_check::workspace::check(operators::all(), false, false, None, c, forms);
    let forms = parse(
        r#"(workspace glob_cards (graph g :context sop [(geo : geometry nil)]
    (let* [literal (text/glob_literal "a*") rewritten (text/glob_rewrite "a*" "b*" "abc")
      entries (sop/group_entries geo "*" :owner "any")] geo)))"#,
    );
    let checked = match checker(&cat, &forms) {
        (Some(c), d) if d.is_empty() => c,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let pane = proj::of_graph(&cat, &checked, "g").unwrap();
    for (binding, ty) in [
        ("literal", Ty::Text),
        ("rewritten", Ty::Text),
        ("entries", Ty::Record(vec![("owners".into(), Ty::Array(Box::new(Ty::Text))), ("names".into(), Ty::Array(Box::new(Ty::Text)))])),
    ] {
        assert!(proj::find(&pane, &path(binding)).unwrap().ty == ty);
    }
    for (binding, key, value) in [
        ("literal", ArgKey::Pos(0), "é*"),
        ("rewritten", ArgKey::Pos(0), "x*"),
        ("rewritten", ArgKey::Pos(1), "y*"),
        ("rewritten", ArgKey::Pos(2), "xyz"),
        ("entries", ArgKey::Pos(1), "wall*"),
        ("entries", ArgKey::Kw("owner".to_owned()), "edge"),
    ] {
        let mut b = flow_syntax::arena::SyntaxBuilder::new();
        let bytes = b.bytes(value.as_bytes());
        let id = b.push(0, Node::Str(bytes), rays_diagnostic::Span { start: 0, finish: 0 }, None);
        let replacement = b.freeze().handle(id);
        let op = Op::SetArg { node: path(binding), key: key.clone(), sub: vec![], value: replacement.clone() };
        let (_, changed) = flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &op));
        let card = proj::find(&proj::of_graph(&cat, &changed, "g").unwrap(), &path(binding)).unwrap();
        let row = card.rows.iter().find(|row| row.key == key).unwrap();
        assert!(row.expr.as_ref().map(flow_syntax::lisp::flat) == Some(flow_syntax::lisp::flat(&replacement)));
    }
}

#[test]
fn public_geometry_folds_and_collecting_forms_are_refused() {
    let cat = catalog0();
    let refuses = |body: &str| {
        let (_, ds) = flow_check::workspace::check(operators::all(), false, false, None, &cat, &parse(body));
        assert!(ds.iter().any(|d| d.code == "E_ARRAY_TYPE"), "{body}");
    };
    refuses(
        r#"(workspace public_fold (graph g :context sop [(geo : geometry nil)]
    (fold [previous geo] [i (array/int 3)] (sop/delete_group previous ""))))"#,
    );
    refuses(
        r#"(workspace public_any_fold (graph g :context sop [(geo : geometry nil)]
    (fold [previous geo] [i (array/int 3)] (error "E_PUBLIC_FOLD" "public geometry fold"))))"#,
    );
    for body in [
        "(for [i (array/int 3)] (sop/delete_group geo \"\"))",
        "(scan [previous geo] [i (array/int 3)] (sop/delete_group previous \"\"))",
        "(map (fn [(i : int)] (sop/delete_group geo \"\")) (array/int 3))",
    ] {
        refuses(&format!("(workspace rejected (defnode sop/rejected [(geo : geometry)] {body}) (graph g :context sop (sop/rejected nil)))"));
    }
}

#[test]

fn cooked_folds_traverse_inventory() {
    let groups: Vec<group::T> = (0..5001).map(|i| ok(group::init(None, group::Owner::Point, &format!("entry_{i}"), 0, &|_| unreachable!()))).collect();
    let geometry = ok(geo::create(geo::positions(&fixtures()[0]).clone(), ok(topology::empty(0)), None, Some(groups), None));
    let node = lisp_sop::workspace_node(
        &[("source".to_owned(), lisp_sop::snapshot(&geometry))],
        r#"(workspace cooked_inventory
    (defnode sop/cooked_inventory [(geo : geometry)]
      (let* [entries (sop/group_entries geo "*" :owner "point")
             result (fold [state {:geometry geo :seen 0}]
               [name entries.names]
               (let* [seen (int/add state.seen 1)]
                 {:geometry (sop/with_attr state.geometry "iteration" (array/int 1 seen) :owner detail :type int)
                  :seen seen}))]
        result.geometry))
    (graph g :context sop (sop/cooked_inventory (sop/ext_source))))"#,
    );
    let mut s = create_session();
    for domains in [1i64, 8] {
        let output = cook(&mut s, domains, &node);
        assert!(geo::groups(&output).len() == 5001);
        match attribute::storage(&geo::find_attribute(Owner::Detail, "iteration", &output).unwrap()) {
            Ok(Storage::Int(values)) => assert!(values == vec![5001]),
            _ => panic!("iteration is not an int attribute"),
        }
    }
    session::close(&mut s);
    // a node that reads the fold's geometry, and a nested fold over a list that depends on the outer step,
    // are deferred with the fold to cook time
    let node = lisp_sop::workspace_node(
        &[("source".to_owned(), lisp_sop::snapshot(&geometry))],
        r#"(workspace cooked_consumer
    (defnode sop/cooked_consumer [(geo : geometry)]
      (let* [outer (fold [state {:geometry geo :seen 0}] [pattern (array/text 1 "entry_1*")]
               (let* [names (sop/group_entries state.geometry pattern :owner "point")
                      inner (fold [step {:geometry state.geometry :seen state.seen}] [name names.names]
                              {:geometry (sop/with_attr step.geometry "iteration" (array/int 1 1) :owner detail :type int)
                               :seen (int/add step.seen 1)})]
                 inner))
             marked (sop/with_attr outer.geometry "seen" (array/int 1 outer.seen) :owner detail :type int)]
        (sop/delete_attr marked "iteration" :owner "detail")))
    (graph g :context sop (sop/cooked_consumer (sop/ext_source))))"#,
    );
    let mut s = create_session();
    let output = cook(&mut s, 1, &node);
    assert!(geo::find_attribute(Owner::Detail, "iteration", &output).is_none());
    match attribute::storage(&geo::find_attribute(Owner::Detail, "seen", &output).unwrap()) {
        Ok(Storage::Int(values)) => assert!(values == vec![1111]),
        _ => panic!("seen is not an int attribute"),
    }
    session::close(&mut s);
}

#[test]
fn ordinary_group_byte_masks_preserve_immutable_storage() {
    for n in [0i64, 1, 7, 8, 9, 63, 64, 65, 32769] {
        let t = ok(topology::create_owned(n, (0..n * 2).map(|i| i / 2).collect(), (0..=n).map(|i| i * 2).collect(), vec![topology::PrimitiveKind::OpenPolyline; n as usize]));
        let nn = n as usize;
        let geometry = ok(geo::create(ok(Float3::View::of_owned_exn(vec![0.0; nn], vec![0.0; nn], vec![0.0; nn])), t, None, None, None));
        for (owner, native_owner) in OWNERS {
            let Some(native_owner) = native_owner else { continue };
            let length = count(&geometry, Some(native_owner));
            for ordered in [false, true] {
                let selected = |i: i64| match (i >> 3) % 3 {
                    0 => false,
                    1 => true,
                    _ => 0xa5 & (1 << (i & 7)) != 0,
                };
                let made = if ordered {
                    let members: Vec<i64> = (0..length).map(|i| length - i - 1).filter(|i| selected(*i)).collect();
                    ok(group::ordered(native_owner, "bytes", length, &members))
                } else {
                    ok(group::init(None, native_owner, "bytes", length, &|i| selected(i)))
                };
                let geometry = ok(geo::with_group(made.clone(), &geometry));
                let bits = group::View::bits_view(&made).to_vec();
                let order = group::ordered_elements(&made);
                let identity = group::data_id(&made);
                let expected: Vec<i64> = (0..length).map(|i| i64::from(group::mem(i, &made))).collect();
                for domains in [1i64, 4, 8] {
                    parallel::run_unit(Some(domains), &|| {
                        match query(&geometry, owner, "bytes", None) {
                            Value::Int_array(actual) => {
                                assert!(*actual == expected);
                                // the array is a copy: flipping it must not reach the group
                                if length > 0 {
                                    let mut copy = (*actual).clone();
                                    copy[0] = 1 - copy[0];
                                }
                            }
                            _ => panic!("group query is not an int array"),
                        }
                        Ok(())
                    })
                    .unwrap();
                }
                assert!(query(&geometry, owner, "bytes", None) == ints(&expected));
                assert!(group::View::bits_view(&made) == &bits[..] && group::ordered_elements(&made) == order && group::data_id(&made) == identity);
            }
        }
    }
}

fn promoted_query(g: &geo::T, owner: &str, name: &str) -> Result<Value, Diagnostic> {
    let source = source_value(
        "sop/group".to_owned(),
        Ty::Array(Box::new(Ty::Int)),
        vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("owner".into(), text(owner)), ("name".into(), text(name)), ("as_owner".into(), text("point"))],
    );
    attribute_kernel::resolve(None, &lookup_of(Some(g.clone())), &source)
}

fn native_promoted(g: &geo::T, owner: &str, name: &str, grain: i64) -> Value {
    let find = |o| geo::find_group(o, name, g).unwrap();
    let chosen = match owner {
        "point" => selection::T::SelectedPoints(find(group::Owner::Point)),
        "vertex" => selection::T::SelectedVertices(find(group::Owner::Vertex)),
        "primitive" => selection::T::SelectedPrimitives(find(group::Owner::Primitive)),
        "edge" => selection::T::SelectedEdges(geo::find_edge_group(name, g).unwrap()),
        _ => unreachable!(),
    };
    let promoted = ok(selection::promote(None, grain, None, group::Owner::Point, &chosen, geo::topology(g)));
    ints(&(0..geo::point_count(g)).map(|i| i64::from(group::mem(i, &promoted))).collect::<Vec<_>>())
}

fn promotion_fixture(n: i64) -> geo::T {
    let t = if n < 6 {
        ok(topology::empty(n))
    } else {
        ok(topology::create_owned(n, vec![0, 1, 2, 2, 1, 3, 4, 4, 0], vec![0, 3, 6, 9], vec![topology::PrimitiveKind::Polygon, topology::PrimitiveKind::Polygon, topology::PrimitiveKind::OpenPolyline]))
    };
    let nn = n as usize;
    ok(geo::create(ok(Float3::View::of_owned_exn((0..nn).map(|i| i as f64).collect(), vec![0.0; nn], vec![0.0; nn])), t, None, None, None))
}

fn promotion_matches(n: i64, owner: &str, native_owner: Option<group::Owner>, selectors: std::ops::Range<usize>) {
    let geometry = promotion_fixture(n);
    let body = format!("(exact (map (fn [(value : int)] value) (sop/group geo :name \"promoted\" :owner {owner} :as_owner point)))");
    // Only the resolver's immutable geometry changes between masks: check and compile once.
    let (value, _) = evaluated(&geometry, &body);
    let program = flow_ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
    let length = count(&geometry, native_owner);
    let selected: [&dyn Fn(i64) -> bool; 4] = [&|_| false, &|_| true, &|i| i % 2 == 0, &|i| i == length - 1];
    for selector in selectors {
        let mask: Vec<i64> = (0..length).map(|i| i64::from(selected[selector](i))).collect();
        for ordered in [false, true] {
            let order: Option<Vec<i64>> = (ordered && native_owner.is_some()).then(|| mask.iter().enumerate().filter(|(_, v)| **v == 1).map(|(i, _)| i as i64).rev().collect());
            let geometry = native_write(order.as_deref(), native_owner, "promoted", &mask, &geometry);
            let before = bytes(&geometry);
            let original = query(&geometry, owner, "promoted", None);
            let expected = native_promoted(&geometry, owner, "promoted", 97);
            for domains in [1i64, 4, 8] {
                parallel::run_unit(Some(domains), &|| {
                    for grain in [1i64, 97, 4096] {
                        assert!(native_promoted(&geometry, owner, "promoted", grain) == expected);
                    }
                    assert!(flow_ok(promoted_query(&geometry, owner, "promoted")) == expected);
                    if let Value::Int_array(values) = flow_ok(promoted_query(&geometry, owner, "promoted")) {
                        if n > 0 {
                            let mut copy = (*values).clone();
                            copy[0] = 1 - copy[0];
                        }
                    }
                    assert!(flow_ok(promoted_query(&geometry, owner, "promoted")) == expected);
                    assert!(query(&geometry, owner, "promoted", None) == original);
                    Ok(())
                })
                .unwrap();
            }
            let lookup = lookup_of(Some(geometry.clone()));
            exact_prepared(&value, &program, Arc::new(move |value| attribute_kernel::resolve(None, &lookup, value)), &body, &expected);
            assert!(bytes(&geometry) == before);
        }
    }
}

macro_rules! promotion_owners {
    ($($test:ident $owner:literal $native:expr),* $(,)?) => {$(
        #[test]
        fn $test() {
            for n in [0i64, 1, 7, 8, 9] { promotion_matches(n, $owner, $native, 0..4); }
        }
    )*};
}
promotion_owners!(
    group_query_promotion_point "point" Some(group::Owner::Point),
    group_query_promotion_vertex "vertex" Some(group::Owner::Vertex),
    group_query_promotion_primitive "primitive" Some(group::Owner::Primitive),
    group_query_promotion_edge "edge" None,
);

// The full packed-cutoff fixture remains in the default run, split by owner and mask.
macro_rules! promotion_cutoff {
    ($($test:ident $owner:literal $native:expr, $selector:expr);* $(;)?) => {$(
        #[test]
        fn $test() { promotion_matches(32769, $owner, $native, $selector..$selector + 1); }
    )*};
}
promotion_cutoff!(
    group_query_promotion_cutoff_point_none "point" Some(group::Owner::Point), 0;
    group_query_promotion_cutoff_point_all "point" Some(group::Owner::Point), 1;
    group_query_promotion_cutoff_point_even "point" Some(group::Owner::Point), 2;
    group_query_promotion_cutoff_point_last "point" Some(group::Owner::Point), 3;
    group_query_promotion_cutoff_vertex_none "vertex" Some(group::Owner::Vertex), 0;
    group_query_promotion_cutoff_vertex_all "vertex" Some(group::Owner::Vertex), 1;
    group_query_promotion_cutoff_vertex_even "vertex" Some(group::Owner::Vertex), 2;
    group_query_promotion_cutoff_vertex_last "vertex" Some(group::Owner::Vertex), 3;
    group_query_promotion_cutoff_primitive_none "primitive" Some(group::Owner::Primitive), 0;
    group_query_promotion_cutoff_primitive_all "primitive" Some(group::Owner::Primitive), 1;
    group_query_promotion_cutoff_primitive_even "primitive" Some(group::Owner::Primitive), 2;
    group_query_promotion_cutoff_primitive_last "primitive" Some(group::Owner::Primitive), 3;
    group_query_promotion_cutoff_edge_none "edge" None, 0;
    group_query_promotion_cutoff_edge_all "edge" None, 1;
    group_query_promotion_cutoff_edge_even "edge" None, 2;
    group_query_promotion_cutoff_edge_last "edge" None, 3;
);

#[test]
fn group_query_promotion_names_and_refusals() {
    let geometry = promotion_fixture(7);
    for name in ["\u{a0}", "\u{3000}"] {
        let g = native_write(None, Some(group::Owner::Vertex), name, &vec![1; geo::vertex_count(&geometry) as usize], &geometry);
        assert!(flow_ok(promoted_query(&g, "vertex", name)) == native_promoted(&g, "vertex", name, 97));
    }
    for (name, owner, target, code) in [
        ("absent", "vertex", text("point"), "E_GROUP_MISSING"),
        (" ", "vertex", text("point"), "E_GROUP_NAME"),
        ("absent", "detail", text("point"), "E_GROUP_OWNER"),
        ("absent", "vertex", text("edge"), "E_GROUP_TARGET"),
        ("absent", "vertex", Value::Int(0), "E_GROUP_TYPE"),
    ] {
        let source = source_value(
            "sop/group".to_owned(),
            Ty::Array(Box::new(Ty::Int)),
            vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("name".into(), text(name)), ("owner".into(), text(owner)), ("as_owner".into(), target)],
        );
        match attribute_kernel::resolve(None, &lookup_of(Some(geometry.clone())), &source) {
            Err(d) => assert!(d.code == code, "{} is not {code}", d.code),
            Ok(_) => panic!("promotion accepted {name}/{owner} ({code})"),
        }
    }
    let point_only = native_write(None, Some(group::Owner::Point), "other_owner", &[1; 7], &geometry);
    let source = source_value(
        "sop/group".to_owned(),
        Ty::Array(Box::new(Ty::Int)),
        vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("name".into(), text("other_owner")), ("owner".into(), text("vertex")), ("as_owner".into(), text("point"))],
    );
    match attribute_kernel::resolve(None, &lookup_of(Some(point_only)), &source) {
        Err(d) => assert!(d.code == "E_GROUP_MISSING"),
        Ok(_) => panic!("promotion crossed owners"),
    }
    let foreign = ok(topology::empty(7));
    let index = ok(topology_index::create(None, &foreign));
    let edge = ok(edge_group::init(None, &foreign, &index, "foreign", &|_| true));
    assert!(geo::with_edge_group(edge, &geometry).is_err());
}

#[test]
fn promotion_origin_defnodes_cook_the_promoted_mask() {
    let geometry = promotion_fixture(7);
    for (owner, native_owner) in OWNERS {
        let n = count(&geometry, native_owner);
        let g = native_write(None, native_owner, "promoted", &(0..n).map(|i| i64::from(i % 2 == 0)).collect::<Vec<_>>(), &geometry);
        let expected = native_promoted(&g, owner, "promoted", 97);
        for captured in [false, true] {
            let body = if captured {
                format!("(let* [mask (sop/group geo :name \"promoted\" :owner {owner} :as_owner point)] (sop/with_attr geo \"mark\" (exact (map (fn [(p : vec3) (m : int)] m) (sop/attr geo :P) mask)) :type int))")
            } else {
                format!("(sop/with_attr geo \"mark\" (exact (map (fn [(p : vec3) (m : int)] m) (sop/attr geo :P) (sop/group geo :name \"promoted\" :owner {owner} :as_owner point))) :type int)")
            };
            let node = lisp_sop::workspace_node(
                &[("source".to_owned(), lisp_sop::snapshot(&g))],
                &format!("(workspace promotion_origin (defnode sop/promoted [(geo : geometry)] {body}) (graph g :context sop (sop/promoted (sop/ext_source))))"),
            );
            let mut s = create_session();
            for domains in [1i64, 4, 8] {
                session::clear(&mut s);
                let output = cook(&mut s, domains, &node);
                assert!(node::facts(&node).reads == ["P"]);
                match (attribute::storage(&geo::find_attribute(Owner::Point, "mark", &output).unwrap()), &expected) {
                    (Ok(Storage::Int(actual)), Value::Int_array(expected)) => assert!(actual == **expected),
                    _ => panic!("mark is not the promoted mask"),
                }
            }
            session::close(&mut s);
        }
    }
}

#[test]
fn promotion_optional_target_graph_gestures() {
    let cat = catalog0();
    let checker = |c: &flow_check::catalog::Catalog, forms: &[syntax::Syntax]| flow_check::workspace::check(operators::all(), false, false, None, c, forms);
    let forms = parse(
        r#"(workspace promotion_cards (graph g :context sop [(geo : geometry nil)]
    (let* [mask (sop/group geo :name "selected" :owner vertex)] geo)))"#,
    );
    let initial = match checker(&cat, &forms) {
        (Some(c), d) if d.is_empty() => c,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let row_expr = |w: &flow_check::workspace::Workspace| {
        let card = proj::find(&proj::of_graph(&cat, w, "g").unwrap(), &path("mask")).unwrap();
        card.rows.iter().find(|row| row.key == ArgKey::Kw("as_owner".to_owned())).unwrap().expr.as_ref().map(flow_syntax::lisp::flat)
    };
    assert!(row_expr(&initial).is_none());
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let bytes = b.bytes(b"point");
    let id = b.push(0, Node::Str(bytes), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    let set = Op::SetArg { node: path("mask"), key: ArgKey::Kw("as_owner".to_owned()), sub: vec![], value: b.freeze().handle(id) };
    let (changed_forms, changed) = flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &forms, &set));
    assert!(row_expr(&changed).as_deref() == Some("\"point\""));
    let disconnect = Op::Disconnect { node: path("mask"), key: ArgKey::Kw("as_owner".to_owned()), fallback: None };
    let (_, cleared) = flow_ok(apply_checked_with(&[], false, Some(&checker), &cat, &changed_forms, &disconnect));
    assert!(row_expr(&cleared).is_none());
    let forms = parse(
        r#"(workspace invalid_target (graph g :context sop [(geo : geometry nil)]
    (let* [mask (sop/group_order geo :name "selected" :as_owner point)] geo)))"#,
    );
    assert!(checker(&cat, &forms).0.is_none());
}

#[test]
fn promoted_group_descriptors_share_point_fusion_origins() {
    let geometry = promotion_fixture(7);
    let cat = catalog0();
    let checked = checked_in(
        &cat,
        r#"(workspace origins (graph g :context value
    [(p : (array vec3) (array/vec3 0)) (mask : (array int) (array/int 0 0))]
    (let* [a (map (fn [(position : vec3)] (+ position [t 0 0])) p)
           b (map (fn [(m : int)] [(float m) t 0]) mask)
           tested (exact (map (fn [(left : vec3) (right : vec3)] (+ left right)) a b))] 0.0)))"#,
    );
    for (owner, native_owner) in OWNERS {
        let n = count(&geometry, native_owner);
        let g = native_write(None, native_owner, "promoted", &(0..n).map(|i| i % 2).collect::<Vec<_>>(), &geometry);
        for promoted in [false, true] {
            let mut args = vec![("owner", text(owner)), ("name", text("promoted"))];
            if promoted {
                args.push(("as_owner", text("point")));
            }
            let marker = source_value("sop/group", Ty::Array(Box::new(Ty::Int)), [("geometry", Value::Deferred(ty::geometry(), 7))].into_iter().chain(args).map(|(k, v)| (k.to_owned(), v)).collect());
            let positions = source_value(
                "sop/attr".to_owned(),
                Ty::Array(Box::new(Ty::Vec3)),
                vec![("geometry".into(), Value::Deferred(ty::geometry(), 7)), ("attribute".into(), text("P"))],
            );
            let evaluation = flow_ok(flow_eval::static_with(&checked, true, Some(&[("g".to_owned(), vec![("p".to_owned(), positions), ("mask".to_owned(), marker)])]), None));
            let values = evaluation.records.iter().find(|(path, _)| path == &vec!["g".to_owned(), "tested".to_owned()]).map(|(_, v)| v[0].1.clone()).unwrap();
            let program = flow_ok(attribute_kernel::prepare(None, true, None, None, &[7], &[lisp_sop::snapshot(&g)], &values));
            let stages = Executor::packed_root(&program).map_or(0, |r| r.stages.len());
            assert!(stages == if promoted || owner == "point" { 3 } else { 1 }, "{owner} promoted={promoted}: {stages} stages");
            let resolve = {
                let lookup = lookup_of(Some(g.clone()));
                Arc::new(move |v: &Value| attribute_kernel::resolve(None, &lookup, v)) as flow_ir::core_exec::ResolveFn
            };
            for domains in [1i64, 4, 8] {
                parallel::run_unit(Some(domains), &|| {
                    let live = input::at_time(0.25);
                    let expected = flow_eval::Host::force_reference(&values, &live, None, None, Some(resolve.clone()));
                    let actual = Executor::force(&program, &Executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() }, &live);
                    assert!(actual == expected);
                    Ok(())
                })
                .unwrap();
            }
        }
    }
}
