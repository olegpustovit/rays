//! Port of lib/flow_sop/test_workspace_zone.ml (W8): loops over geometry. A Garden-like graph
//! (scatter, then a for over its points) cooks deterministically at 1 and 3 domains; moving one
//! point recooks only its element; pieces, keys and provenance.
use super::support::*;
use crate::curve;
use crate::lisp_sop;
use crate::lower;
use crate::network::{self, Network};
use crate::port::Port;
use crate::value_lane;
use flow_eval::Value;
use flow_graph::probe;
use rdk::geometry as geo;
use rdk::packed::Float3;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node;
use sop::session;
use std::rc::Rc;
use std::sync::Arc;

fn garden(result: &str, count: i64) -> String {
    format!(
        r#"
(workspace garden
  (graph garden :context sop [(seed : int 1) (count : int {count})]
    (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 12)
           spots (sop/scatter bed :count count :seed seed)
           dots (for [p (sop/point_list spots)]
                  (let* [ball (sop/uv_sphere :radius 0.06 :segments 8 :rings 4)
                         moved (sop/transform ball :translate p)]
                    moved))
           result {result}]
      result)))"#
    )
}

fn lower_source(source: &str) -> lower::T {
    lower::workspace(None, Vec::new(), lower::Options::default(), &parse(source)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn zctx(domains: i64) -> sctx::T {
    sctx::create(ContextOptions { domains: Some(domains), grain: Some(97), seed: Some(42), ..Default::default() }).unwrap()
}

fn open_session() -> session::T {
    session::create(512, 256 * 1024 * 1024).unwrap()
}

fn cook_domains(s: &mut session::T, domains: i64, graph: &lower::Graph) -> geo::T {
    let root = graph.root.expect("a graph result");
    let compiled = edit::compile_node(&graph.network.geometry, root).unwrap();
    let out = session::cook(s, None, &zctx(domains), &compiled).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    sop::payload::geometry(&out.payload).unwrap()
}

fn cook1(graph: &lower::Graph) -> geo::T {
    let mut s = open_session();
    let g = cook_domains(&mut s, 1, graph);
    session::close(&mut s);
    g
}

fn prims(g: &geo::T) -> i64 {
    geo::primitive_count(g)
}

fn mean_xy(g: &geo::T) -> (f64, f64) {
    let positions = geo::positions(g);
    let n = Float3::length(positions);
    let (mut x, mut y) = (0.0, 0.0);
    for i in 0..n {
        let (a, b, _) = Float3::get(positions, i).unwrap();
        x += a;
        y += b;
    }
    (x / n as f64, y / n as f64)
}

fn path(l: &[&str]) -> Vec<String> {
    l.iter().map(|x| (*x).to_owned()).collect()
}

fn graph_with(graph: &lower::Graph, network: Network, root: Option<i64>) -> lower::Graph {
    lower::Graph { network, root: root.or(graph.root), ..graph.clone() }
}

fn verify(source: &str, site: &[&str], iter: &[i64], choices: &[Vec<i64>], expected_x: &[f64], expected_prims: i64) {
    let lowered = lower_source(source);
    let graph = lowered.graphs[0].clone();
    let site = path(site);
    let target = lowered.plan.nodes.iter().find(|n| n.site == site && n.iter == iter).expect("a plan node").clone();
    let mut s = open_session();
    let original = cook_domains(&mut s, 1, &graph);
    let l = lowered.clone();
    let records = probe::make(&lowered.evaluated, probe::Options { dynamic: Some(Rc::new(move |p: &[String]| lower::zone_count(&l, &p.to_vec()))), ..Default::default() });
    for (probes, expected_x) in choices.iter().zip(expected_x) {
        assert!(records.plan_node(None, &site, probes) == Some(target.id), "a geometry preview resolves the template at the selected tuple");
        let (network, root) = (lowered.preview)(target.id, probes, &graph.network).expect("a preview");
        let viewed = graph_with(&graph, network.clone(), Some(root));
        assert!(
            geometry_bytes(&original) == geometry_bytes(&cook_domains(&mut s, 1, &graph_with(&graph, network, None))),
            "the scratch preview disconnected the authored result's inputs"
        );
        let output = cook_domains(&mut s, 1, &viewed);
        assert!(prims(&output) == expected_prims, "a preview contains only the selected element");
        let (x, _) = mean_xy(&output);
        assert!((x - expected_x).abs() < 1e-6, "preview {} at {probes:?}: expected {expected_x:.1}, got {x:.1}", site.join("/"));
        assert!(geometry_bytes(&output) == geometry_bytes(&cook_domains(&mut s, 3, &viewed)), "a preview is exact across domains");
    }
    assert!(geometry_bytes(&original) == geometry_bytes(&cook_domains(&mut s, 1, &graph)), "preview cache entries contaminated the authored result");
    assert!((lowered.preview)(target.id, &[], &graph.network).is_none(), "a preview accepted an incomplete iteration tuple");
    let missing: Vec<i64> = iter.iter().map(|_| -1).collect();
    let (network, root) = (lowered.preview)(target.id, &missing, &graph.network).expect("a preview");
    assert!(prims(&cook_domains(&mut s, 1, &graph_with(&graph, network, Some(root)))) == 0, "a missing geometry element did not produce an empty preview");
    session::close(&mut s);
}

#[test]

fn previews_resolve_a_template_at_the_selected_tuple() {
    verify(
        r#"(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
           a (sop/box :center [2 0 0])
           dots (for [p (sop/point_list field)]
             (let* [unused (sop/transform a :translate p) shown (sop/box :center [100 0 0])] shown))]
      (sop/merge dots))))"#,
        &["g", "dots", "unused"],
        &[0],
        &[vec![0], vec![2], vec![1], vec![0]],
        &[2.0, 22.0, 12.0, 2.0],
        6,
    );
}

#[test]

fn previews_resolve_nested_templates() {
    verify(
        r#"(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0])) a (sop/box :center [2 0 0])
           dots (for [p (sop/point_list field)]
             (let* [inner (for [q (sop/point_list field)]
                       (let* [moved (sop/transform a :translate (+ p q))] moved))]
               (sop/merge inner)))] (sop/merge dots))))"#,
        &["g", "dots", "inner", "moved"],
        &[0, 0],
        &[vec![0, 0], vec![2, 1], vec![1, 2]],
        &[2.0, 32.0, 32.0],
        6,
    );
    verify(
        r#"(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
           rows (for [i (range 2)]
             (let* [dots (for [p (sop/point_list field)]
                       (sop/transform (sop/box) :translate (+ p [(* i 100) 0 0])))]
               (sop/merge dots)))] (sop/merge rows))))"#,
        &["g", "rows", "dots", "@result"],
        &[1, 0],
        &[vec![1, 0], vec![1, 2], vec![1, 1]],
        &[100.0, 120.0, 110.0],
        6,
    );
}

#[test]

fn previews_resolve_piece_templates() {
    verify(
        r#"(workspace preview (graph g :context sop
    (let* [a (sop/merge (sop/box) (sop/box :center [10 0 0]))
           pieces (for [piece (sop/piece_list a)]
             (let* [moved (sop/transform piece :translate [2 0 0])] moved))]
      (sop/merge pieces))))"#,
        &["g", "pieces", "moved"],
        &[0],
        &[vec![0], vec![6]],
        &[2.5, 12.5],
        1,
    );
}

fn live_preview_lowered() -> (lower::T, lower::Graph, i64) {
    live_preview_lowered_over("(list [0 0 0] [10 0 0])")
}

fn live_preview_lowered_over(points: &str) -> (lower::T, lower::Graph, i64) {
    // Live arguments and folds use the same frame and environment as the authored graph. Switching
    // selectors cannot advance or reset the fold.
    let lowered = lower_source(
        &r#"(workspace live_preview (graph g :context sop
    (let* [ticks (state [n 0.0] (+ n (frame/dt)))
           field (sop/curve {points})
           dots (for [p (sop/point_list field)]
             (let* [moved (sop/transform (sop/box) :translate (+ p [ticks t 0]))] moved))]
      (sop/merge dots))))"#
        .replace("{points}", points),
    );
    let graph = lowered.graphs[0].clone();
    let target = lowered.plan.nodes.iter().find(|n| n.site == path(&["g", "dots", "moved"])).expect("a plan node").id;
    (lowered, graph, target)
}

fn frame_at(frame: i64, dt: f64, t: f64) -> input::Frame {
    input::Frame { frame, dt, ..input::at_time(t) }
}

fn cook_frame(s: &mut session::T, domains: i64, input: &input::Frame, geometry: &edit::T, root: i64) -> geo::T {
    let compiled = edit::compile_node(geometry, root).unwrap();
    let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(1), input: Some(input.clone()), time: Some(input.t), frame: Some(input.frame), ..Default::default() }).unwrap();
    let out = session::cook(s, None, &context, &compiled).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    sop::payload::geometry(&out.payload).unwrap()
}

#[test]
fn a_same_session_never_reuses_a_zone_result_across_divergent_fold_histories() {
    let (lowered, graph, target) = live_preview_lowered();
    let seed = flow_eval::create_state();
    let left = value_lane::create(Some(flow_eval::fork_state(&seed)));
    let right = value_lane::create(Some(flow_eval::fork_state(&seed)));
    let mut s = open_session();
    // frame 1 only advances each lane's fold (the divergent histories); the zone is cooked at frame 2 in
    // the one session, where a result cached for one history must not serve the other
    let mut resolve = |lane: &value_lane::Lane, frame: i64, dt: f64, cook: bool| -> f64 {
        let input = frame_at(frame, dt, frame as f64 * 0.25);
        let (network, root) = (lowered.preview)(target, &[0], &graph.network).expect("a preview");
        let resolved = value_lane::resolve(None, Some(&input), lane, input.t, &network).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        if cook { mean_xy(&cook_frame(&mut s, 1, &input, &resolved.geometry, root)).0 } else { 0.0 }
    };
    resolve(&left, 1, 0.25, false);
    resolve(&right, 1, 0.5, false);
    let left_x = resolve(&left, 2, 0.25, true);
    let right_x = resolve(&right, 2, 0.25, true);
    assert!(left_x == 0.5 && right_x == 0.75, "the same session reused a zone result across divergent fold histories");
}

#[test]

fn live_previews_and_frame_folds_agree_across_domains() {
    let (lowered, graph, target) = live_preview_lowered();
    let play = |domains: i64| -> Vec<Vec<u8>> {
        let lane = value_lane::create(None);
        let mut s = open_session();
        let out = (0..3)
            .map(|index| {
                let input = frame_at(index + 1, 0.25, (index + 1) as f64 * 0.25);
                let (network, root) = (lowered.preview)(target, &[index % 2], &graph.network).expect("a preview");
                let resolved = value_lane::resolve(None, Some(&input), &lane, input.t, &network).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
                let geometry = cook_frame(&mut s, domains, &input, &resolved.geometry, root);
                let (x, y) = mean_xy(&geometry);
                assert!(x == (index % 2 * 10) as f64 + input.t && y == input.t, "preview selectors changed the live frame or fold's accumulated value");
                geometry_bytes(&geometry)
            })
            .collect();
        session::close(&mut s);
        out
    };
    assert!(play(1) == play(3), "live previews and frame folds differ across domains");
}

/// the graph with its zero-input node `operation` replaced by a snapshot of its cook whose point
/// `index` is lifted by `by`
fn move_point(s: &mut session::T, graph: &lower::Graph, operation: &str, index: i64, by: f64) -> (geo::T, edit::T) {
    // an inlined card: its body's result, behind its marker, or the structural marker itself
    let nodes = edit::inspect(&graph.network.geometry);
    let field = nodes
        .iter()
        .find(|n| n.operation == operation || (n.label == operation && (n.operation == "flow.card" || n.operation == "flow.geometry_select")))
        .expect("the field node")
        .clone();
    let field = if field.operation != "flow.card" { field } else { nodes.iter().find(|n| Some(n.id) == field.inputs[0]).expect("the card's body").clone() };
    let compiled = edit::compile_node(&graph.network.geometry, field.id).unwrap();
    let out = session::cook(s, None, &zctx(1), &compiled).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    let base = sop::payload::geometry(&out.payload).unwrap();
    let positions = geo::positions(&base);
    let n = Float3::length(positions);
    let (mut x, mut y, mut z) = (Vec::new(), Vec::new(), Vec::new());
    for i in 0..n {
        let (a, b, c) = Float3::get(positions, i).unwrap();
        x.push(a);
        y.push(if i == index { b + by } else { b });
        z.push(c);
    }
    let moved = geo::with_positions(Float3::of_owned(x, y, z).unwrap(), &base).unwrap();
    let replaced = node::build::restore_id(field.id, &lisp_sop::snapshot(&moved)).unwrap();
    (base, edit::replace_node(&replaced, &graph.network.geometry).unwrap())
}

fn frames_source(ticks: &str) -> String {
    format!(
        r#"(workspace frames
    (graph g :context sop
      (let* [ticks {ticks}
             bed (sop/curve (list [0 0 0] [1 0 0]))
             dots (for [p (sop/point_list bed)]
                    (sop/transform (sop/box :size 0.1)
                      :translate (+ p [ticks 0 0])))] (sop/merge dots))))"#
    )
}

#[test]

fn a_captured_fold_accumulates_in_the_zone_and_is_exact_across_domains() {
    let live = lower_source(&frames_source("(state [n 0.0] (+ n (frame/dt)))"));
    let graph = live.graphs[0].clone();
    assert!(graph.network.frame_nodes.len() == 1, "zone captures the fold snapshot");
    let playback = |domains: i64| -> Vec<Vec<u8>> {
        let lane = value_lane::create(None);
        let mut s = open_session();
        let mut frame = |i: i64| -> Vec<u8> {
            let input = frame_at(i, 0.25, i as f64 * 0.25);
            let resolved = value_lane::resolve(None, Some(&input), &lane, input.t, &graph.network).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
            geometry_bytes(&cook_frame(&mut s, domains, &input, &resolved.geometry, graph.root.unwrap()))
        };
        let values: Vec<Vec<u8>> = (1..=3).map(&mut frame).collect();
        for (i, bytes) in values.iter().enumerate() {
            let oracle = lower_source(&frames_source(&flow_syntax::lisp::float((i + 1) as f64 * 0.25)));
            assert!(*bytes == geometry_bytes(&cook1(&oracle.graphs[0])), "captured fold accumulates in the zone");
        }
        value_lane::reset(false, &lane);
        assert!(frame(3) == values[0], "fold reset invalidates the same-frame zone cache");
        session::close(&mut s);
        values
    };
    assert!(playback(1) == playback(3), "captured fold is byte-exact across domains");
}

#[test]

fn a_frame_fold_inside_a_geometry_worker_is_refused() {
    let nested = r#"(workspace invalid (graph g :context sop
    (let* [bed (sop/curve (list [0 0 0]))]
      (sop/merge (for [p (sop/point_list bed)]
        (let* [ticks (state [n 0.0] (+ n 1))]
          (sop/transform (sop/box) :translate (+ p [ticks 0 0]))))))))"#;
    match lower::workspace(None, Vec::new(), lower::Options::default(), &parse(nested)) {
        Err(d) => assert_eq!(d.code, "E_STATE_ELEMENT", "per-element fold diagnostic"),
        Ok(_) => panic!("a cooked-geometry worker cannot own a frame fold"),
    }
}

fn session_cook(graph: &lower::Graph, domains: i64) -> geo::T {
    let mut s = open_session();
    let g = cook_domains(&mut s, domains, graph);
    session::close(&mut s);
    g
}

#[test]

fn a_garden_zone_cooks_deterministically_with_provenance() {
    let lowered = lower_source(&garden("(sop/merge bed dots)", 4));
    let graph = lowered.graphs[0].clone();
    assert!(lower::zone_count(&lowered, &path(&["garden", "dots"])).is_none(), "no count before a cook");
    let one = session_cook(&graph, 1);
    let three = session_cook(&graph, 3);
    assert!(geometry_bytes(&one) == geometry_bytes(&three), "zone differs at 1 and 3 domains");
    assert!(geometry_bytes(&one) == geometry_bytes(&session_cook(&lower_source(&garden("(sop/merge bed dots)", 4)).graphs[0], 1)), "zone is not deterministic");
    assert!(lower::zone_count(&lowered, &path(&["garden", "dots"])) == Some(4), "count after the cook");
    let bed = prims(&session_cook(&lower_source(&garden("bed", 1)).graphs[0], 1));
    let ball = prims(&session_cook(&lower_source("(workspace b (graph b :context sop (sop/uv_sphere :radius 0.06 :segments 8 :rings 4)))").graphs[0], 1));
    assert!(prims(&one) == bed + 4 * ball, "{} prims, want {}", prims(&one), bed + 4 * ball);
    // only the template is lowered: the zone node, not four copies of its body
    let ops: Vec<String> = edit::inspect(&graph.network.geometry).into_iter().map(|n| n.operation).collect();
    assert!(ops.iter().filter(|o| *o == "zone").count() == 1 && !ops.iter().any(|o| o == "uv_sphere"), "the template body stays out of the network");
    // provenance: every tag names an element of the zone, at its index
    let attribute = geo::find_attribute(rdk::attribute::Owner::Primitive, lower::source_attribute(), &one).expect("the source attribute");
    let tags = match rdk::attribute::storage(&attribute) {
        Ok(rdk::attribute::Storage::Int(values)) => values,
        _ => panic!("tag storage"),
    };
    let mut sorted = tags.clone();
    sorted.sort();
    sorted.dedup();
    let elements: Vec<Vec<i64>> = sorted
        .iter()
        .filter_map(|tag| match lower::origin(&lowered, *tag) {
            Some(o) if o.site == path(&["garden", "dots", "moved"]) => Some(o.iter),
            _ => None,
        })
        .collect();
    assert!(elements == (0..4).map(|i| vec![i]).collect::<Vec<_>>(), "every element resolves to its iteration");
    assert!(lower::tags(&lowered, &path(&["garden", "dots", "moved"]), &[3]).len() == 1, "one tag per element");
    // the four elements, and the tag the final merge gave its input (its primitives keep the zone's)
    assert!(lower::tags(&lowered, &path(&["garden", "dots"]), &[]).len() == 5, "the loop lights every element");
}

#[test]

fn moving_one_point_recooks_only_its_element() {
    // cache hits after moving one point: the collection is a grid whose point 5 moves
    let source = r#"
(workspace grove
  (graph grove :context sop
    (let* [field (sop/uv_sphere :radius 1 :segments 5 :rings 3)
           trees (for [p (sop/point_list field)]
                   (sop/transform (sop/uv_sphere :radius 0.05 :segments 6 :rings 3) :translate p))]
      (sop/merge field trees))))"#;
    let lowered = lower_source(source);
    let graph = lowered.graphs[0].clone();
    let mut s = open_session();
    let first = cook_domains(&mut s, 1, &graph);
    let a = session::stats(&s);
    let (base, graph2) = move_point(&mut s, &graph, "uv_sphere", 5, 0.25);
    let compiled = edit::compile_node(&graph2, graph.root.unwrap()).unwrap();
    let before = session::stats(&s);
    let out = session::cook(&mut s, None, &zctx(1), &compiled).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
    let second = sop::payload::geometry(&out.payload).unwrap();
    let after = session::stats(&s);
    let n = geo::point_count(&base);
    assert!(a.cooks > 0 && n > 4, "the collection has points");
    assert!(geometry_bytes(&first) != geometry_bytes(&second), "moving a point changes the result");
    let (misses, hits) = (after.misses - before.misses, after.hits - before.hits);
    // the snapshot, the zone, its merge, the moved element's transform, the final merge
    assert!(misses <= 5, "{misses} misses after moving one point");
    assert!(hits >= n - 1, "only {hits} hits after moving one point");
    session::close(&mut s);
}

// a body that reads t: the zone depends on the time, its elements are cooked at it, and the result
// is what the same graph gives with t replaced by a number; the elements' copies share their
// template's volatile slot, so the cache does not churn
#[test]

fn a_live_zone_body_follows_t_and_shares_one_volatile_slot() {
    let buzz = |t_text: &str| {
        lower_source(&format!(
            r#"
(workspace buzz
  (graph buzz :context sop
    (let* [field (sop/uv_sphere :radius 1 :segments 6 :rings 3)
           bees (for [p (sop/point_list field)]
                  (sop/transform (sop/uv_sphere :radius 0.05 :segments 6 :rings 3)
                                 :translate (+ p [0 (* 0.3 (sin {t_text})) 0])))]
      (sop/merge field bees))))"#
        ))
    };
    let live = buzz("t");
    let live_graph = live.graphs[0].clone();
    assert!(lower::counts(&live).0 > 0 && lower::counts(&live).1 >= 0, "a live loop body is counted as live");
    let at = |time: f64, graph: &lower::Graph, lowered: &lower::T| -> (geo::T, session::Stats) {
        let mut s = open_session();
        session::set_volatile(&mut s, lower::is_volatile(lowered));
        let context = sctx::create(ContextOptions { domains: Some(1), grain: Some(97), seed: Some(42), time: Some(time), ..Default::default() }).unwrap();
        let compiled = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
        let out = session::cook(&mut s, None, &context, &compiled).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
        let result = (sop::payload::geometry(&out.payload).unwrap(), session::stats(&s));
        session::close(&mut s);
        result
    };
    let (g0, _) = at(0.0, &live_graph, &live);
    let (g1, stats) = at(1.25, &live_graph, &live);
    assert!(geometry_bytes(&g0) != geometry_bytes(&g1), "a loop body that reads t did not move with t");
    let fixed = buzz("1.25");
    assert!(lower::counts(&fixed).0 == 0, "a body with a number is not live");
    let (f1, _) = at(0.0, &fixed.graphs[0], &fixed);
    assert!(geometry_bytes(&g1) == geometry_bytes(&f1), "a live loop differs from the same loop with t replaced");
    assert!(stats.volatile_entries <= 6, "a live zone keeps {} volatile slots", stats.volatile_entries);
}

#[test]

fn pieces_are_primitives_and_keys_order_the_elements() {
    let pieces = lower_source(
        r#"
(workspace bricks
  (graph bricks :context sop
    (let* [two (sop/merge (sop/box) (sop/transform (sop/box) :translate [3 0 0]))
           lifted (for [q (sop/piece_list two)] (sop/transform q :translate [0 1 0]))]
      (sop/merge lifted))))"#,
    );
    let g = session_cook(&pieces.graphs[0], 1);
    assert!(prims(&g) == 12, "12 pieces in, {} out", prims(&g));
    assert!(lower::zone_count(&pieces, &path(&["bricks", "lifted"])) == Some(12), "piece count");
    assert!(geometry_bytes(&g) == geometry_bytes(&session_cook(&pieces.graphs[0], 3)), "pieces differ at 1 and 3 domains");
    // keys order the elements
    let base = session_cook(
        &lower_source("(workspace w (graph g :context sop (sop/uv_sphere :radius 1 :segments 5 :rings 3)))").graphs[0],
        1,
    );
    let n = geo::point_count(&base);
    let ids = rdk::attribute::create_owned("id", rdk::attribute::Owner::Point, rdk::attribute::Storage::Int((0..n).map(|i| n - 1 - i).collect())).unwrap();
    let keyed = geo::with_attribute(ids, &base).unwrap();
    let ordered = sop::zone::elements(sop::zone::Kind::Points, Some("id"), &keyed).unwrap();
    assert!(ordered[0].key == 0 && Ok(ordered[0].position) == Float3::get(geo::positions(&base), n - 1), "elements follow the key attribute");
    let plain = sop::zone::elements(sop::zone::Kind::Points, None, &keyed).unwrap();
    assert!(Ok(plain[0].position) == Float3::get(geo::positions(&base), 0), "index order without a key");
}

fn cook_result(geometry: &edit::T, node_id: i64, time: f64) -> Result<geo::T, rays_diagnostic::Diagnostic> {
    let node = edit::compile_node(geometry, node_id).unwrap();
    let mut s = open_session();
    let context = sctx::create(ContextOptions { domains: Some(1), grain: Some(97), seed: Some(42), time: Some(time), ..Default::default() }).unwrap();
    let result = session::cook(&mut s, None, &context, &node).and_then(|out| sop::payload::geometry(&out.payload));
    session::close(&mut s);
    result
}

#[test]
fn live_curve_points_stay_typed_and_match_the_authored_curve() {
    // Live Curve points remain a typed Vec3 array in Network.drives and are installed without text
    // serialization. Simultaneous option drives survive.
    let moving_curve = lower_source("\n(workspace moving_curve\n  (graph g :context sop\n    (sop/curve (list [t 0 0] [1 0 0] [1 1 0]))))");
    let curve_graph = moving_curve.graphs[0].clone();
    let curve_id = curve_graph.root.unwrap();
    let points_port = Port { node: curve_id, path: curve::PARAMETER.to_owned() };
    match curve_graph.network.drives.get(&points_port) {
        Some(Value::List(values)) => assert!(values.iter().all(|v| matches!(v, Value::Vec3(..) | Value::Residual(_))), "curve drive did not retain a typed Vec3 list"),
        Some(value) => panic!("curve points drive is not a list: {:?}", value),
        None => panic!("curve points are absent from Network.drives"),
    }
    let mut drives = curve_graph.network.drives.clone();
    drives.insert(Port { node: curve_id, path: curve::CLOSED_PARAMETER.to_owned() }, Value::Bool(true));
    drives.insert(Port { node: curve_id, path: curve::FILLED_PARAMETER.to_owned() }, Value::Bool(true));
    let with = |drives: std::collections::BTreeMap<Port, Value>| network::with_drives(drives, &curve_graph.network);
    let curve_network = with(drives.clone()).unwrap();
    let live = input::at_time(0.5);
    let resolve = |network: &Network| value_lane::resolve(None, Some(&live), &value_lane::create(None), 0.5, network);
    let resolved = resolve(&curve_network).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let expected_curve = lower_source("\n(workspace expected_curve\n  (graph g :context sop\n    (sop/curve (list [0.5 0 0] [1 0 0] [1 1 0]) :closed true :filled true)))");
    let expected_graph = expected_curve.graphs[0].clone();
    let actual = cook_result(&resolved.geometry, curve_id, 0.5).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let expected = cook_result(&expected_graph.network.geometry, expected_graph.root.unwrap(), 0.5).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    assert!(geometry_bytes(&actual) == geometry_bytes(&expected), "typed live curve points/options differ from authored Lisp curve");
    let actual_node = edit::find(&resolved.geometry, curve_id).unwrap();
    assert!(node::parameter_key(&actual_node).starts_with(b"flow.curve\0"), "live curve points were text-encoded into the parameter key");
    let mut inf = drives.clone();
    inf.insert(points_port.clone(), Value::List(Arc::new(vec![Value::Vec3(f64::INFINITY, 0.0, 0.0)])));
    let nonfinite = resolve(&with(inf).unwrap()).unwrap();
    let static_node = edit::find(&expected_graph.network.geometry, expected_graph.root.unwrap()).unwrap();
    let (static_node, _) = node::apply_parameters(&static_node, &[(curve::PARAMETER.to_owned(), param::Value::Text(curve::encode(&[(f64::INFINITY, 0.0, 0.0)]).into_bytes()))]).unwrap();
    let static_geometry = edit::replace_node(&static_node, &expected_graph.network.geometry).unwrap();
    match (cook_result(&nonfinite.geometry, curve_id, 0.5), cook_result(&static_geometry, expected_graph.root.unwrap(), 0.5)) {
        (Err(actual), Err(expected)) => assert!(actual.code == expected.code && actual.message == expected.message, "nonfinite typed curve refusal differs from authored curve"),
        _ => panic!("nonfinite curve points were not refused consistently"),
    }
    let mut malformed = drives.clone();
    malformed.insert(points_port.clone(), Value::List(Arc::new(vec![Value::Int(1)])));
    match resolve(&with(malformed.clone()).unwrap()) {
        Err(d) => assert_eq!(d.code, "E_LIVE", "malformed curve list error code"),
        Ok(_) => panic!("malformed live curve points were accepted"),
    }
    let mut both_bad = malformed;
    both_bad.insert(Port { node: curve_id, path: curve::CLOSED_PARAMETER.to_owned() }, Value::Vec3(1.0, 0.0, 0.0));
    match resolve(&with(both_bad).unwrap()) {
        Err(d) => assert_eq!(d.code, "E_TYPE", "curve point error ran before the earlier typed option drive"),
        Ok(_) => panic!("invalid option and curve points were accepted"),
    }
}

#[test]
fn a_body_reading_t_is_live_and_one_reading_its_element_is_not() {
    let counts = |text: &str| lower::counts(&lower_source(text));
    assert!(
        counts("\n(workspace w (graph g :context sop\n  (let* [f (sop/grid)] (sop/merge (for [p (sop/point_list f)] (sop/transform (sop/box) :translate [t 0 0]))))))").0 > 0,
        "a body that reads t is not live"
    );
    assert!(
        counts("\n(workspace w (graph g :context sop\n  (let* [f (sop/grid)] (sop/merge (for [p (sop/point_list f)] (sop/transform (sop/box) :translate p))))))").0 == 0,
        "a body that reads its element is live"
    );
    let _ = Arc::new(0);
}
