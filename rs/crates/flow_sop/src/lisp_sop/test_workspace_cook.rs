//! Port of lib/flow_sop/test_workspace_cook.ml (W2): every workspace fixture lowers and cooks, byte
//! for byte the same at 1 and 3 domains; node counts; provenance.
//!
//! The OCaml catalog and `Lower.workspace ~extra` carry `Editor_document.Kinds`; flow_sop cannot
//! depend on editor_document, so the fixtures that write scene/world/settings/ui kinds (`bloom`,
//! `variations`) are `#[ignore = "needs editor_document"]`. OCaml's `RAYS_SLOW=1` sections are
//! `#[ignore = "slow ..."]` tests here (lowering costs 0.5-1 s per sphere-like node).
use super::support::*;
use crate::{catalog as fcat, lisp_sop, lower, operators};
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{self as e, ArgKey, Op};
use flow_graph::projection as p;
use flow_syntax::syntax::{self as s, Syntax};
use flow_syntax::{lisp, phase_timer};
use rdk::geometry as geo;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph::{self as edit, Factory, InputRequirement};
use sop::node;
use sop::session;
use std::sync::Arc;

fn factories() -> Vec<Factory> {
    lisp_sop::factories()
}

fn catalog_of(factories: &[Factory]) -> Catalog {
    fcat::of_factories(1, &[], None, factories).unwrap()
}

fn diag(d: rays_diagnostic::Diagnostic) -> String {
    rays_diagnostic::to_string(&d)
}

fn sv(l: &[&str]) -> Vec<String> {
    l.iter().map(|x| (*x).to_owned()).collect()
}

fn merge(nodes: &[node::T]) -> node::T {
    let with_: Vec<(String, node::T)> = nodes.iter().enumerate().map(|(i, n)| (format!("i{i}"), n.clone())).collect();
    let text = format!("(sop/merge {})", with_.iter().map(|(k, _)| format!("(sop/ext_{k})")).collect::<Vec<_>>().join(" "));
    lisp_sop::node(&with_, &text)
}

fn lower_forms(options: lower::Options, forms: &[Syntax]) -> lower::T {
    lower::workspace(Some(factories()), Vec::new(), options, forms).unwrap_or_else(|d| panic!("{}", diag(d)))
}

fn lower_case(name: &str) -> lower::T {
    lower_forms(lower::Options::default(), &parse(&case_text(name)))
}

fn nodes_of(graph: &lower::Graph) -> usize {
    edit::inspect(&graph.network.geometry).len()
}

fn operations(graph: &lower::Graph, operation: &str) -> usize {
    edit::inspect(&graph.network.geometry).iter().filter(|n| n.operation == operation).count()
}

fn context_with(domains: i64, grain: i64) -> sctx::T {
    sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), seed: Some(42), ..Default::default() }).unwrap()
}

/// One cook of a graph's displayed node at `domains`; `None` for a graph with no result.
fn cook_graph(domains: i64, graph: &lower::Graph) -> Option<geo::T> {
    let root = graph.root?;
    let compiled = edit::compile_node(&graph.network.geometry, root).unwrap_or_else(|m| panic!("{m}"));
    let mut s = session::create(512, 256 * 1024 * 1024).unwrap();
    let out = session::cook(&mut s, None, &context_with(domains, 97), &compiled).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let g = sop::payload::geometry(&out.payload).unwrap();
    session::close(&mut s);
    Some(g)
}

fn cook_node(n: &node::T) -> geo::T {
    let mut s = session::create(16, 10_000_000).unwrap();
    let out = session::cook(&mut s, None, &context_with(1, 1), n).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let g = sop::payload::geometry(&out.payload).unwrap();
    session::close(&mut s);
    g
}

fn bytes_at(domains: i64, graph: &lower::Graph) -> Vec<u8> {
    geometry_bytes(&cook_graph(domains, graph).expect("a result"))
}

fn kw(name: &str) -> ArgKey {
    ArgKey::Kw(name.to_owned())
}

fn checked_in(cat: &Catalog, forms: &[Syntax], what: &str) -> flow_check::workspace::Workspace {
    match flow_check::workspace::check(&[], false, false, None, cat, forms) {
        (Some(w), _) => w,
        _ => panic!("{what} did not check"),
    }
}

fn node_of(cat: &Catalog, w: &flow_check::workspace::Workspace, leaf: &str) -> p::Node {
    p::find(&p::of_graph(cat, w, "g").unwrap(), &sv(&["g", leaf])).expect("the card")
}

fn text_expr(t: &str) -> Syntax {
    parse(t).remove(0)
}

#[test]
fn image_noise_lowers_cooks_and_edits() {
    let cat = catalog_of(&factories());
    let forms = parse("(workspace pixels (graph g :context image [] (let* [img (image/noise :width 17 :height 9 :frequency 0.12 :seed 31)] img)))");
    let checked = match flow_check::workspace::check(&[], false, false, None, &cat, &forms) {
        (Some(c), ds) if ds.is_empty() => c,
        (_, ds) => panic!("{}", ds.into_iter().map(diag).collect::<Vec<_>>().join("\n")),
    };
    let card = p::find(&p::of_graph(&cat, &checked, "g").unwrap(), &sv(&["g", "img"])).unwrap();
    for name in ["width", "height", "frequency", "seed"] {
        assert!(card.rows.iter().any(|r| r.key == kw(name)), "image/noise projects {name}");
    }
    let lowered = lower::of_checked(Some(factories()), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", diag(d)));
    let graph = &lowered.graphs[0];
    let compiled = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    let mut s = session::create(16, 65536).unwrap();
    let context = sctx::create(ContextOptions { domains: Some(8), grain: Some(17), ..Default::default() }).unwrap();
    let out = session::cook(&mut s, None, &context, &compiled).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let image = sop::payload::image(&out.payload).unwrap();
    session::close(&mut s);
    assert!(sop::image::width(&image) == 17 && sop::image::height(&image) == 9, "Lisp image/noise cooked dimensions");
    let changed = e::apply(&cat, &forms, &Op::SetArg { node: sv(&["g", "img"]), key: kw("width"), sub: vec![], value: text_expr("19") });
    assert!(changed.is_ok(), "image/noise width edits through the graph");
}

// register L16: a skipped iteration makes no node; the others keep compiled ids, plan keys and
// provenance, so the cook keeps hitting its cache
#[test]
#[ignore = "slow lowering: perf agent (1-3 s alone, over 5 s under the runner's parallelism)"]
fn skips() {
    let text = |skip: &str| format!("(workspace w (graph g :context sop (sop/merge (for [i (range 4)] {skip}(sop/transform (sop/box) :translate [i 0 0])))))");
    let lower_text = |previous: Option<&lower::T>, source: &str| {
        let options = match previous {
            Some(l) => lower::Options { compiled_ids: l.compiled_ids.clone(), sites: l.sites.clone(), ..lower::Options::default() },
            None => lower::Options::default(),
        };
        lower_forms(options, &parse(source))
    };
    let all = lower_text(None, &text(""));
    let some = lower_text(Some(&all), &text(":skip [2] "));
    let graph = |l: &lower::T| l.graphs.iter().find(|g| g.name == "g").unwrap().clone();
    // inlined, each Lisp card is a marker node over its body
    let cards = |l: &lower::T| operations(&graph(l), "flow.card");
    assert!(cards(&some) + 2 == cards(&all), "a skipped iteration did not remove its box and transform cards");
    let two = lower_text(Some(&all), &text(":skip [1 2] "));
    assert!(
        nodes_of(&graph(&all)) - nodes_of(&graph(&two)) == 2 * (nodes_of(&graph(&all)) - nodes_of(&graph(&some))),
        "a skipped iteration did not remove its own nodes"
    );
    for (path, id) in &some.compiled_ids {
        match all.compiled_ids.get(path) {
            Some(before) => assert!(before == id, "a surviving node changed its compiled id"),
            None => panic!("a skip minted a compiled id"),
        }
    }
    // provenance: what the merge took names the iteration that made it, not its position
    let iters = |l: &lower::T| -> Vec<Vec<i64>> {
        let mut out: Vec<Vec<i64>> = l.provenance.values().filter(|o| !o.iter.is_empty() && !o.site.is_empty()).map(|o| o.iter.clone()).collect();
        out.sort();
        out.dedup();
        out
    };
    assert!(iters(&all) == vec![vec![0], vec![1], vec![2], vec![3]] && iters(&some) == vec![vec![0], vec![1], vec![3]], "the provenance of a skipped loop does not name the iterations that ran");
    let g = graph(&some);
    assert!(bytes_at(1, &g) == bytes_at(3, &g), "a skipped loop cooks differently at 1 and 3 domains");
    assert!(bytes_at(1, &g) != bytes_at(1, &graph(&all)), "a skipped loop cooked like the whole");
}

fn rest_fixture() -> Factory {
    let mut args = edit::factory_args("rest_fixture", "Rest fixture", &["Test"]);
    args.operation = Some("merge".to_owned());
    args.slots = Some(sv(&["base", "fixed", "extras"]));
    edit::factory_slots(
        args,
        vec![InputRequirement::Required, InputRequirement::Optional, InputRequirement::OptionalRest],
        Arc::new(|inputs| Ok(merge(&inputs.iter().filter_map(|i| i.clone()).collect::<Vec<_>>()))),
    )
}

fn boxes() -> (node::T, node::T) {
    (lisp_sop::node(&[], "(sop/box)"), lisp_sop::node(&[], "(sop/box :center [2.0 0.0 0.0])"))
}

fn source3(result: &str) -> String {
    format!("(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) r {result}] r)))")
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): lowers a merge of several boxes per case"]
fn optional_rest() {
    let mut all = vec![rest_fixture()];
    all.extend(factories());
    let cat = catalog_of(&all);
    let (manifest, _) = crate::manifest::generate(&[], None, &all).unwrap_or_else(|d| panic!("{}", diag(d)));
    let manifest_catalog = flow_check::catalog::catalog_of_manifest(&manifest).unwrap_or_else(|d| panic!("{}", diag(d))).0;
    let fixture = manifest_catalog.kinds.iter().find(|k| k.qualified == "sop/rest_fixture");
    assert!(fixture.is_some_and(|k| k.slots.iter().any(|slot| slot.rest && !slot.required)), "optional rest survives manifest serialization");
    let lower_all = |previous: Option<&lower::T>, text: &str| {
        let options = match previous {
            Some(l) => lower::Options { compiled_ids: l.compiled_ids.clone(), sites: l.sites.clone(), ..lower::Options::default() },
            None => lower::Options::default(),
        };
        lower::workspace(Some(all.clone()), Vec::new(), options, &parse(text)).unwrap_or_else(|d| panic!("{}", diag(d)))
    };
    let (a, b) = boxes();
    let cases: Vec<(&str, Vec<node::T>)> = vec![
        ("(sop/rest_fixture a)", vec![a.clone()]),
        ("(sop/rest_fixture a b)", vec![a.clone(), b.clone()]),
        ("(sop/rest_fixture a nil b a)", vec![a.clone(), b.clone(), a.clone()]),
        ("(sop/rest_fixture a :extras (list b a))", vec![a.clone(), b.clone(), a.clone()]),
        ("(sop/rest_fixture :extras (list b a) :base a)", vec![a.clone(), b.clone(), a.clone()]),
        ("(first (map sop/rest_fixture (list a) (list nil) (list (list b a))))", vec![a.clone(), b.clone(), a.clone()]),
    ];
    for (result, nodes) in &cases {
        let lowered = lower_all(None, &source3(result));
        let expected = geometry_bytes(&cook_node(&merge(nodes)));
        for domains in [1, 3] {
            assert!(geometry_bytes(&cook_graph(domains, &lowered.graphs[0]).unwrap()) == expected, "fixed/optional/rest inputs retain order when lowered");
        }
    }
    let bad_function_call = "(workspace w (defn caller :context sop [(make : fn) (a : geometry) (b : geometry)] (first (map make (list a) (list b)))) (graph g :context sop (caller sop/null (sop/box) (sop/box))))";
    match lower::workspace(Some(all.clone()), Vec::new(), lower::Options::default(), &parse(bad_function_call)) {
        Err(d) => assert!(d.code == "E_EXTRA_POSITIONAL", "kind functions retain their input bound: {}", diag(d)),
        Ok(_) => panic!("fixed kind function silently ignored an extra geometry input"),
    }
    let text = source3("(sop/rest_fixture a)");
    let forms = parse(&text);
    let workspace = checked_in(&cat, &forms, "optional rest");
    let node = node_of(&cat, &workspace, "r");
    assert!(node.rows.iter().any(|r| r.kind == p::RowKind::Add && r.key == ArgKey::Pos(2)), "optional rest projects an add row after the fixed prefix");
    let before = lower_all(None, &text);
    let edited = e::apply(&cat, &forms, &Op::Connect { node: sv(&["g", "r"]), key: ArgKey::Pos(2), src: "b".into(), iter: false }).unwrap();
    let after = lower_all(Some(&before), &lisp::print(&edited).0);
    assert!(before.graphs[0].root == after.graphs[0].root, "connecting optional rest preserves compiled identity");
    assert!(geometry_bytes(&cook_graph(1, &after.graphs[0]).unwrap()) == geometry_bytes(&cook_node(&merge(&[a.clone(), b.clone()]))), "Flow_edit pads disconnected fixed slots before the rest connection");
    let edited = e::apply(&cat, &edited, &Op::Disconnect { node: sv(&["g", "r"]), key: ArgKey::Pos(2), fallback: None }).unwrap();
    let disconnected = lower_all(Some(&after), &lisp::print(&edited).0);
    assert!(geometry_bytes(&cook_graph(1, &disconnected.graphs[0]).unwrap()) == geometry_bytes(&cook_node(&merge(&[a.clone()]))), "Flow_edit disconnect restores zero extras");
    let workspace = checked_in(&cat, &parse(&source3("(sop/rest_fixture a :extras (list b a))")), "named rest");
    let node = node_of(&cat, &workspace, "r");
    assert!(node.rows.iter().any(|r| r.key == kw("extras") && r.kind == p::RowKind::Rest), "named rest lists project as editable rows");
}

fn expected_bytes(with_: &[(&str, &node::T)], text: &str) -> Vec<u8> {
    let with_: Vec<(String, node::T)> = with_.iter().map(|(k, n)| ((*k).to_owned(), (*n).clone())).collect();
    geometry_bytes(&cook_node(&lisp_sop::node(&with_, text)))
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): lowers and cooks several boxes per case"]
fn attribute_composite() {
    let (a, b) = boxes();
    let expected = expected_bytes(
        &[("a", &a), ("b", &b)],
        r#"(sop/attribute_composite (sop/ext_a) nil (sop/ext_b) nil nil (sop/ext_b) (sop/ext_a) :allow_position true :weight2 0.5 :weights "0.2\n0.3")"#,
    );
    let cat = catalog_of(&factories());
    for result in [
        r#"(sop/attribute_composite a nil b nil nil b a :allow_position true :weight2 0.5 :weights "0.2\n0.3")"#,
        r#"(sop/attribute_composite a :layer2 b :layers (list b a) :allow_position true :weight2 0.5 :weights "0.2\n0.3")"#,
    ] {
        let forms = parse(&source3(result));
        let lowered = lower_forms(lower::Options::default(), &forms);
        for domains in [1, 4] {
            assert!(geometry_bytes(&cook_graph(domains, &lowered.graphs[0]).unwrap()) == expected, "Attribute Composite Lisp fixed and repeated layer parity");
        }
        let workspace = checked_in(&cat, &forms, "Attribute Composite");
        assert!(node_of(&cat, &workspace, "r").rows.iter().any(|r| r.key == kw("weights")), "Attribute Composite extra weights project as an editable field");
        let edited = e::apply(&cat, &forms, &Op::SetArg { node: sv(&["g", "r"]), key: kw("weights"), sub: vec![], value: text_expr("\"0.4\\n0.3\"") }).unwrap();
        let after = lower_forms(lower::Options { compiled_ids: lowered.compiled_ids.clone(), sites: lowered.sites.clone(), ..lower::Options::default() }, &edited);
        assert!(after.graphs[0].root == lowered.graphs[0].root, "Attribute Composite weight edit preserves node identity");
        assert!(geometry_bytes(&cook_graph(1, &after.graphs[0]).unwrap()) != expected, "Attribute Composite weight edit reaches the native cook");
    }
}

fn switch_source(result: &str) -> String {
    format!("(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) c (sop/box :center [4 0 0]) r {result}] r)))")
}

#[test]
#[ignore = "slow: 4-6 s (over the 5 s per-test budget; lowers five box graphs)"]
fn switch() {
    let cat = catalog_of(&factories());
    let cooked = |result: &str| -> Vec<Vec<u8>> {
        let forms = parse(&switch_source(result));
        let lowered = lower_forms(lower::Options::default(), &forms);
        let workspace = checked_in(&cat, &forms, "Switch");
        let node = node_of(&cat, &workspace, "r");
        assert!(
            !result.starts_with('(') || node.rows.iter().any(|r| r.kind == p::RowKind::Add && r.label == "+ inputs"),
            "Switch offers a further branch as an add row"
        );
        [1, 4].into_iter().map(|d| bytes_at(d, &lowered.graphs[0])).collect()
    };
    let third = cooked("c");
    for result in ["(sop/switch a b c :input 2)", "(sop/switch a b :inputs (list c) :input 2)"] {
        assert!(cooked(result) == third, "Switch did not cook its third branch: {result}");
    }
    assert!(cooked("(sop/switch a b c :input 1)") == cooked("b"), "Switch did not cook its second branch");
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): lowers and cooks several boxes per case"]
fn blend_shapes() {
    let (a, b) = boxes();
    let c = lisp_sop::node(&[], "(sop/box :center [4.0 0.0 0.0])");
    let expected = expected_bytes(
        &[("a", &a), ("b", &b), ("c", &c)],
        r#"(sop/blend_shapes (sop/ext_a) nil (sop/ext_b) nil nil (sop/ext_c) (sop/ext_a) :weight2 0.5 :weights "0.2\n0.3" :shape_masks "5\tother_mask\tshape")"#,
    );
    let cat = catalog_of(&factories());
    for result in [
        r#"(sop/blend_shapes a nil b nil nil c a :weight2 0.5 :weights "0.2\n0.3" :shape_masks "5\tother_mask\tshape")"#,
        r#"(sop/blend_shapes a :shape2 b :shapes (list c a) :weight2 0.5 :weights "0.2\n0.3" :shape_masks "5\tother_mask\tshape")"#,
    ] {
        let forms = parse(&switch_source(result));
        let lowered = lower_forms(lower::Options::default(), &forms);
        for domains in [1, 4] {
            assert!(geometry_bytes(&cook_graph(domains, &lowered.graphs[0]).unwrap()) == expected, "Blend Shapes positional/named rest Lisp parity");
        }
        let workspace = checked_in(&cat, &forms, "Blend Shapes");
        let projected = node_of(&cat, &workspace, "r");
        for name in ["weights", "shape_masks"] {
            assert!(projected.rows.iter().any(|r| r.key == kw(name)), "Blend Shapes projects {name}");
        }
        let edited = e::apply(&cat, &forms, &Op::SetArg { node: sv(&["g", "r"]), key: kw("weights"), sub: vec![], value: text_expr("\"0.4\\n0.3\"") }).unwrap();
        let edited = e::apply(&cat, &edited, &Op::SetArg { node: sv(&["g", "r"]), key: kw("shape_masks"), sub: vec![], value: text_expr("\"5\\tmask\\tfirst\"") }).unwrap();
        let after = lower_forms(lower::Options { compiled_ids: lowered.compiled_ids.clone(), sites: lowered.sites.clone(), ..lower::Options::default() }, &edited);
        assert!(after.graphs[0].root == lowered.graphs[0].root, "Blend Shapes parameter edit preserves identity");
        assert!(geometry_bytes(&cook_graph(1, &after.graphs[0]).unwrap()) != expected, "Blend Shapes weight edit reaches the native cook");
        let workspace = checked_in(&cat, &edited, "Blend Shapes mask table edit");
        assert!(node_of(&cat, &workspace, "r").rows.iter().any(|r| r.key == kw("shape_masks") && r.expr.is_some()), "Blend Shapes mask table edits remain projected");
    }
}

// ---- the fixtures ----

fn same_value(a: &flow_eval::Value, b: &flow_eval::Value) -> bool {
    use flow_eval::Value::*;
    match (a, b) {
        (Fn(_), Fn(_)) => true,
        (List(a), List(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| same_value(a, b)),
        (Record(a), Record(b)) | (Struct(_, _, a), Struct(_, _, b)) => a.len() == b.len() && a.iter().zip(b).all(|((n, a), (m, b))| n == m && same_value(a, b)),
        // Marshal compares NaN bits, `==` does not: fall back to the printed form
        _ => a == b || format!("{a:?}") == format!("{b:?}"),
    }
}

fn force_same(left: &flow_eval::State, right: &flow_eval::State, live: &input::Frame, a: &flow_eval::Value, b: &flow_eval::Value) -> bool {
    match (flow_eval::force(a, live, Some(left)), flow_eval::force(b, live, Some(right))) {
        (Ok(a), Ok(b)) => same_value(&a, &b),
        (Err(a), Err(b)) => a == b,
        _ => false,
    }
}

fn pairs_same<K: PartialEq>(left: &flow_eval::State, right: &flow_eval::State, live: &input::Frame, a: &[(K, flow_eval::Value)], b: &[(K, flow_eval::Value)]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|((n, a), (m, b))| n == m && force_same(left, right, live, a, b))
}

fn same_evaluation(left: &flow_eval::State, right: &flow_eval::State, live: &input::Frame, a: &flow_eval::Eval, b: &flow_eval::Eval) -> bool {
    a.plan.nodes.len() == b.plan.nodes.len()
        && a.plan.nodes.iter().zip(&b.plan.nodes).all(|(a, b)| {
            a.id == b.id && a.inst == b.inst && a.kind == b.kind && a.ty == b.ty && a.site == b.site && a.iter == b.iter && pairs_same(left, right, live, &a.args, &b.args)
        })
        && a.plan.instances.len() == b.plan.instances.len()
        && a.plan.instances.iter().zip(&b.plan.instances).all(|(a, b)| {
            a.graph == b.graph && a.default == b.default && pairs_same(left, right, live, &a.inputs, &b.inputs) && force_same(left, right, live, &a.result, &b.result)
        })
        && pairs_same(left, right, live, &a.results, &b.results)
}

fn check_fixture(name: &str, extra: bool) -> usize {
    let _ = extra;
    let lowered = lower_case(name);
    let cat = catalog_of(&factories());
    let forms = parse(&case_text(name));
    let checked = match fcat::check_workspace(operators::all(), false, None, &cat, &forms) {
        (Some(c), _) => c,
        _ => panic!("{name} did not check"),
    };
    let guard = phase_timer::SampleGuard::start_with_clock(Box::new(|| std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_or(0.0, |d| d.as_secs_f64())));
    let from_checked = lower::of_checked(Some(factories()), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", diag(d)));
    let phases = guard.finish();
    assert!(
        phase_timer::calls(&phases, "check") == 0 && phase_timer::calls(&phases, "evaluate") == 1 && phase_timer::calls(&phases, "lower") == 1,
        "{name}: checked lowering repeated a check or evaluation"
    );
    // OCaml compares physically (plan ==, states ==); the Rust plan is held by value and the
    // evaluation by Arc, so the retained evaluation is compared structurally.
    assert!(
        from_checked.plan.nodes.len() == from_checked.evaluated.plan.nodes.len() && from_checked.states == from_checked.evaluated.states,
        "{name}: lowering did not retain its evaluation"
    );
    let reference = flow_eval::static_eval(&checked).unwrap_or_else(|d| panic!("{}", diag(d)));
    let (left, right) = (flow_eval::create_state(), flow_eval::create_state());
    for (frame, time) in [0.0, 0.125, 1.25, 7.0].into_iter().enumerate() {
        let live = input::Frame { frame: frame as i64, ..input::at_time(time) };
        let (a, b) = (&*lowered.evaluated, &*from_checked.evaluated);
        let records_same = a.records.len() == b.records.len() && a.records.iter().zip(&b.records).all(|((p, x), (q, y))| p == q && pairs_same(&left, &right, &live, x, y));
        assert!(
            same_evaluation(&left, &right, &live, a, b) && same_evaluation(&left, &right, &live, &reference, b) && records_same,
            "{name}: checked/source lowering differ at {time}"
        );
    }
    let mut cooked = 0;
    for graph in &lowered.graphs {
        match (cook_graph(1, graph), cook_graph(3, graph)) {
            (None, None) => {}
            (Some(a), Some(b)) => {
                cooked += 1;
                assert!(geometry_bytes(&a) == geometry_bytes(&b), "{name}/{} differs at 1 and 3 domains", graph.name);
                // the source and checked lowerings give the same geometry (fresh ids do not leak)
                let again = from_checked.graphs.iter().find(|g| g.instance == graph.instance).expect("the same instance");
                match cook_graph(1, again) {
                    Some(c) => assert!(geometry_bytes(&a) == geometry_bytes(&c), "{name}/{} is not deterministic", graph.name),
                    None => panic!("second lowering lost its root"),
                }
            }
            _ => panic!("root differs"),
        }
    }
    cooked
}

#[test]
fn kit_fixture_lowers_and_cooks_the_same_at_1_and_3_domains() {
    check_fixture("kit", false);
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): lowers the fixtures tree, tunnel and wave"]
fn the_other_sop_fixtures_lower_and_cook() {
    let mut cooked = 0;
    for name in ["tree", "tunnel", "wave"] {
        cooked += check_fixture(name, false);
    }
    assert!(cooked >= 3, "only {cooked} graphs cooked");
}

#[test]
#[ignore = "needs editor_document"]
fn variations_fixture_lowers_and_cooks() {
    check_fixture("variations", true);
}

#[test]
fn lowering_accepts_only_a_checked_workspace() {
    match lower::workspace(Some(factories()), Vec::new(), lower::Options::default(), &parse("(workspace w (graph g :context sop (sop/nope)))")) {
        Err(_) => {}
        Ok(_) => panic!("lowering accepted an unchecked workspace"),
    }
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): six uv_spheres lower in seconds"]
fn a_merge_loop_tags_its_primitives_with_the_input_index() {
    let small = lower_forms(lower::Options::default(), &parse("(workspace w (graph g :context sop (sop/merge (for [i (range 6)] (sop/uv_sphere :center [i 0 0])))))"));
    let graph = &small.graphs[0];
    assert!(operations(graph, "flow.card") == 7, "merge loop card count");
    let geometry = cook_graph(1, graph).unwrap();
    match geo::find_attribute(rdk::attribute::Owner::Primitive, lower::source_attribute(), &geometry) {
        Some(attribute) => match rdk::attribute::storage(&attribute) {
            Ok(rdk::attribute::Storage::Int(values)) => assert!(
                values.len() as i64 == geo::primitive_count(&geometry) && values[0] == 0 && values[values.len() - 1] == 5,
                "merge loop source attribute"
            ),
            _ => panic!("source attribute is not int"),
        },
        None => panic!("merge wrote no source attribute"),
    }
}

#[test]
#[ignore = "slow (RAYS_SLOW=1 in OCaml): the wave fixture lowers in seconds"]
fn wave_pending_parameters_and_stable_compiled_ids() {
    // Wave: the curve is a native node; t-dependent points are pending and the static cook uses t = 0
    let wave = lower_case("wave");
    assert!(wave.pending.len() >= 6 && wave.pending.iter().all(|p| p.field == "points"), "wave pending live parameters");
    assert!(cook_graph(1, &wave.graphs[0]).is_some(), "wave cooks");
    // ids are stable when the previous ids and sites are passed back
    let kit = lower_case("kit");
    let again = lower_forms(lower::Options { compiled_ids: kit.compiled_ids.clone(), sites: kit.sites.clone(), ..lower::Options::default() }, &parse(&case_text("kit")));
    assert!(again.compiled_ids == kit.compiled_ids && again.compiled == kit.compiled, "compiled ids are stable");
}

#[test]
#[ignore = "needs editor_document"]
fn bloom_node_counts_and_provenance() {
    // Bloom (3 petals, the ref override 2): flower is 3 petals of (uv_sphere, transform, set_color,
    // transform), a merge, the heart (uv_sphere, set_color) and the result merge; the ref override
    // with 7 petals is its own network. A Lisp node inlines as its writes under one card, so the
    // authored calls are the cards, the merges flow.concat.
    let text = case_text("bloom").replace("(petals : int 12)", "(petals : int 3)").replace(":petals 7", ":petals 2");
    let bloom = lower_forms(lower::Options::default(), &parse(&text));
    let cards = |g: &lower::Graph| operations(g, "flow.card");
    match &bloom.graphs[..] {
        [flower, small] => {
            assert!(cards(flower) == (3 * 4) + 1 + 2 + 1, "bloom flower card count");
            assert!(operations(flower, "flow.concat") == 2, "bloom flower merges");
            assert!(cards(small) == (2 * 4) + 1 + 2 + 1, "bloom ref override card count");
            assert!(small.instance != flower.instance, "distinct instances");
            // provenance: the ring merge's input k is petal k's last transform
            let ring = edit::inspect(&flower.network.geometry).into_iter().find(|n| n.operation == "flow.concat" && n.inputs.len() == 3).expect("the ring merge").id;
            for k in 0..3 {
                let origin = bloom.provenance.values().find(|o| o.merge == ring && o.input == k).expect("an origin");
                assert!(origin.iter == vec![k], "bloom provenance iteration");
                assert!(Some(origin.source) == edit::inputs(&flower.network.geometry, ring).unwrap()[k as usize], "bloom provenance source");
            }
            assert!(bloom.provenance.len() == 3 + 2 + 2 + 2, "bloom provenance size");
        }
        graphs => panic!("bloom has {} sop networks", graphs.len()),
    }
    let _ = s::make;
}
