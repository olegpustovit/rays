//! Port of lib/flow_sop/test_lisp_fusion.ml. A chain of Lisp-bodied point deformers cooks as one
//! kernel run, with the bytes of the chain run one deformer at a time.
use super::support::*;
use crate::lower;
use flow_eval as E;
use flow_ir::flow_ir::{Profile, Tier};
use sop::edit_graph as edit;
use sop::node;
use sop::session;
use std::sync::{Arc, Mutex};

const COLUMNS: i64 = 1025;
const STAGES: [(f64, f64, i64); 4] = [(0.3, 0.7, 0), (-0.2, 1.3, 3), (0.15, 2.1, 5), (0.05, 0.4, 7)];

fn grid() -> String {
    format!("(sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns {COLUMNS} :rows 2)")
}

fn stage_text(i: usize, (amplitude, frequency, seed): (f64, f64, i64)) -> String {
    let source = if i == 0 { "g".to_owned() } else { format!("d{i}") };
    format!("d{} (sop/noise_displace {source} :amplitude {amplitude} :frequency {frequency} :seed {seed})", i + 1)
}

fn chain_text(stages: &[(f64, f64, i64)]) -> String {
    let body = stages.iter().enumerate().map(|(i, s)| stage_text(i, *s)).collect::<Vec<_>>().join(" ");
    format!("(workspace fused (graph g :context sop (let* [g {} {}] d{})))", grid(), body, stages.len())
}

type Profiles = Arc<Mutex<Vec<Arc<Profile::T>>>>;

fn lower_with(profiles: &Profiles, reference: bool, previous: Option<&lower::T>, text: &str) -> lower::T {
    let sink = profiles.clone();
    let options = lower::Options {
        reference,
        on_profile: Some(Arc::new(move |p| sink.lock().unwrap().push(p.clone()))),
        compiled_ids: previous.map(|l| l.compiled_ids.clone()).unwrap_or_default(),
        sites: previous.map(|l| l.sites.clone()).unwrap_or_default(),
        ..lower::Options::default()
    };
    lower::workspace(None, Vec::new(), options, &parse(text)).unwrap_or_else(|d| panic!("{text}\n{}", rays_diagnostic::to_string(&d)))
}

fn compiled(l: &lower::T) -> node::T {
    let graph = &l.graphs[0];
    edit::compile_node(&graph.network.geometry, graph.root.expect("a graph result")).expect("compile")
}

fn cook_fresh(domains: i64, n: &node::T) -> Vec<u8> {
    let mut s = new_session(32, 64 * 1024 * 1024);
    let g = cook(&mut s, &context(domains, 257), n);
    session::close(&mut s);
    geometry_bytes(&g)
}

fn kernel_runs(profiles: &Profiles) -> usize {
    profiles.lock().unwrap().iter().map(|p| Profile::executions(p).iter().filter(|e| e.tier == Tier::Cpu_kernel).count()).sum()
}

fn with_attr_writes(root: &node::T) -> usize {
    node::inspect(root).iter().filter(|n| n.operation == "flow.with_attr").count()
}

#[test]
#[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock (exceeds 5 s)"]
fn four_noise_displace_nodes_cook_as_at_most_one_kernel_run() {
    let profiles: Profiles = Arc::new(Mutex::new(Vec::new()));
    let mut reference = Vec::new();
    for domains in [1i64, 4] {
        profiles.lock().unwrap().clear();
        let l = lower_with(&profiles, false, None, &chain_text(&STAGES));
        let bytes = cook_fresh(domains, &compiled(&l));
        if domains == 1 {
            reference = bytes;
        } else {
            assert!(bytes == reference);
        }
        // the grid is not a kernel run; the four deformers are at most one
        assert!(kernel_runs(&profiles) <= 1);
    }
    let l = lower_with(&profiles, false, None, &chain_text(&STAGES));
    // editing one node's parameter recooks the whole fused chain
    let edited: Vec<(f64, f64, i64)> = STAGES.iter().enumerate().map(|(i, (a, f, s))| if i == 1 { (a * 2.0, *f, *s) } else { (*a, *f, *s) }).collect();
    let l2 = lower_with(&profiles, false, Some(&l), &chain_text(&edited));
    let mut s = new_session(32, 64 * 1024 * 1024);
    let first = geometry_bytes(&cook(&mut s, &context(1, 257), &compiled(&l)));
    let second = geometry_bytes(&cook(&mut s, &context(1, 257), &compiled(&l2)));
    session::close(&mut s);
    assert!(first != second);
    // an intermediate with a second reader is not folded away: it still cooks for that reader
    profiles.lock().unwrap().clear();
    let text = chain_text(&STAGES);
    let split = lower_with(&profiles, false, None, &format!("{}(sop/merge d4 d2))))", &text[..text.len() - 5]));
    cook_fresh(1, &compiled(&split));
    assert!(kernel_runs(&profiles) <= 2);
}

/// E4: the chain over a cooked (native) input folds into one region, with the bytes of the unfolded
/// (reference) lowering at one and four domains, and the intermediate cards keep their own.
#[test]
#[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock (exceeds 5 s)"]
fn chain_over_a_cooked_input_folds_into_one_region() {
    let base = format!("(sop/soft_transform {})", grid());
    let body = STAGES.iter().enumerate().map(|(i, s)| stage_text(i, *s)).collect::<Vec<_>>().join(" ");
    let text = format!("(workspace cooked (graph g :context sop (let* [g {base} {body}] d4)))");
    let cook_root = |reference: bool, domains: i64| {
        let profiles: Profiles = Arc::new(Mutex::new(Vec::new()));
        let l = lower_with(&profiles, reference, None, &text);
        let bytes = cook_fresh(domains, &compiled(&l));
        (bytes, kernel_runs(&profiles), l)
    };
    let (expected, _, _) = cook_root(true, 1);
    for domains in [1i64, 4] {
        let (bytes, runs, l) = cook_root(false, domains);
        assert!(bytes == expected);
        assert!(runs <= 1);
        // the second card still cooks as two displacements
        if domains == 1 {
            let graph = &l.graphs[0];
            let cards: Vec<&E::Node> = l.plan.nodes.iter().filter(|n| n.kind == E::card_kind() && n.args.iter().find(|(k, _)| k == "kind").map(|(_, v)| v) == Some(&E::Value::Text(b"sop/noise_displace".to_vec()))).collect();
            let d2 = cards[1];
            let n = edit::compile_node(&graph.network.geometry, *l.compiled.get(&d2.id).expect("compiled card")).expect("compile");
            let two = cook_fresh(domains, &n);
            let two_body = STAGES.iter().take(2).enumerate().map(|(i, s)| stage_text(i, *s)).collect::<Vec<_>>().join(" ");
            let profiles: Profiles = Arc::new(Mutex::new(Vec::new()));
            let r = lower_with(&profiles, false, None, &format!("(workspace two (graph g :context sop (let* [g {base} {two_body}] d2)))"));
            assert!(two == cook_fresh(domains, &compiled(&r)));
        }
    }
}

/// Writes of different attributes fuse: one record program writes Cd, then P (over a transform
/// card), with the unfolded bytes.
#[test]
#[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock (exceeds 5 s)"]
fn writes_of_different_attributes_fuse() {
    let text = format!(
        "(workspace mixed (graph g :context sop (let* [g (sop/soft_transform {}) c (sop/color_by_height g) d (sop/noise_displace c :amplitude 0.3 :frequency 0.7) tr (sop/transform d :translate [0.5 0 0] :rotate [0 0.3 0])] tr)))",
        grid()
    );
    let cook_root = |reference: bool, domains: i64| {
        let profiles: Profiles = Arc::new(Mutex::new(Vec::new()));
        let l = lower_with(&profiles, reference, None, &text);
        let root = compiled(&l);
        let bytes = cook_fresh(domains, &root);
        // Cd, P and P fold into one, beside the grid's own N and P writes
        (bytes, with_attr_writes(&root))
    };
    let (expected, unfolded) = cook_root(true, 1);
    for domains in [1i64, 4] {
        let (bytes, writes) = cook_root(false, domains);
        assert!(bytes == expected);
        assert!(writes == 3, "mixed chain: {writes} writes cooked, {unfolded} unfolded");
    }
}

/// A map that reads the chain inside its term (no let* binding) folds too: its read of the written
/// Cd takes the values, its read of P the input.
#[test]
#[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock (exceeds 5 s)"]
fn a_map_reading_the_chain_in_its_term_folds() {
    let text = format!(
        "(workspace lazy (graph g :context sop (let* [g (sop/soft_transform {}) c (sop/color_by_height g)] (sop/with_attr c :P (exact (map (fn [(p : vec3) (k : vec4)] (+ p [k.x k.y 0.0])) (sop/attr c :P) (sop/attr c \"Cd\" :type vec4)))))))",
        grid()
    );
    let cook_ref = |reference: bool| {
        let profiles: Profiles = Arc::new(Mutex::new(Vec::new()));
        let l = lower_with(&profiles, reference, None, &text);
        let root = compiled(&l);
        (cook_fresh(4, &root), with_attr_writes(&root))
    };
    let (expected, unfolded) = cook_ref(true);
    let (bytes, writes) = cook_ref(false);
    assert!(bytes == expected);
    assert!(writes == 3, "lazy reads: {writes} writes, {unfolded} unfolded");
}
