//! Port of lib/rays_editor/test/test_workspace_live.ml (W2b: live drives, volatile cache slots,
//! fixed-step determinism, realtime edits and the editor's cook path).
use super::support::*;
use super::t3c::frame_at;
use flow_sop::lower;
use flow_sop::value_lane;
use rays_editor::cook;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node;
use sop::session;

fn case_text(name: &str) -> String {
    let text = read_case(name);
    // These are the exact fixture overrides in test_workspace_live.ml.
    match name {
        "sunflower" => text.replacen("(seeds : int 240)", "(seeds : int 8)", 1),
        "orrery" => text.replacen("(rings : int 3)", "(rings : int 2)", 1).replacen("(moons : int 8)", "(moons : int 2)", 1),
        _ => text,
    }
}

fn fail(message: &str) -> ! {
    panic!("test_workspace_live: {message}")
}

fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

fn lower_with(source: &str, compiled_ids: Option<&lower::PathMap>, sites: Option<&[Vec<String>]>) -> lower::T {
    let forms = flow_syntax::syntax::parse(source).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)));
    let mut options = lower::Options::default();
    if let Some(ids) = compiled_ids {
        options.compiled_ids = ids.clone();
    }
    if let Some(s) = sites {
        options.sites = s.to_vec();
    }
    lower::workspace(Some(sop::nodes::factories()), editor_document::kinds::descriptors(), options, &forms).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)))
}

fn lower_text(source: &str) -> lower::T {
    lower_with(source, None, None)
}

/// The symbol `t` replaced by a literal: the static oracle of a live value.
fn at_time(source: &str, time: f64) -> String {
    let literal = flow_syntax::lisp::float(time);
    let chars: Vec<char> = source.chars().collect();
    let sep = |c: char| matches!(c, ' ' | '(' | ')' | '[' | ']' | '\n');
    let mut out = String::new();
    for (i, &c) in chars.iter().enumerate() {
        if c == 't' && i > 0 && i + 1 < chars.len() && sep(chars[i - 1]) && sep(chars[i + 1]) {
            out.push_str(&literal);
        } else {
            out.push(c);
        }
    }
    out
}

struct Player {
    lowered: lower::T,
    graph: lower::Graph,
    root: i64,
    lane: value_lane::Lane,
    session: session::T,
    previous: Option<std::sync::Arc<edit::Compiled>>,
    domains: i64,
}

fn player(domains: i64, lowered: lower::T) -> Player {
    let graph = lowered.graphs.iter().find(|g| g.root.is_some()).cloned().unwrap_or_else(|| fail("a graph with a result"));
    let mut session = session::create(512, 256 * 1024 * 1024).expect("session");
    session::set_volatile(&mut session, lower::is_volatile(&lowered));
    Player { root: graph.root.unwrap(), graph, lowered, lane: value_lane::create(None), session, previous: None, domains }
}

fn frame(p: &mut Player, time: f64) -> (value_lane::Resolved, rdk_core::geometry::T) {
    let resolved = value_lane::resolve(None, None, &p.lane, time, &p.graph.network).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)));
    let compiled = std::sync::Arc::new(edit::compile_all(p.previous.as_deref(), &resolved.geometry));
    p.previous = Some(compiled.clone());
    let node = edit::compiled_node(&compiled, p.root).expect("root");
    let context = sctx::create(ContextOptions { domains: Some(p.domains), grain: Some(97), seed: Some(42), ..Default::default() }).expect("context");
    match session::cook(&mut p.session, None, &context, &node) {
        Ok(output) => (resolved, sop::payload::geometry(&output.payload).expect("geometry")),
        Err(e) => fail(&rays_diagnostic::error_to_string(&e)),
    }
}

const SMALL: &str = r#"(workspace w
  (graph g :context sop []
    (let* [c (sop/uv_sphere :radius 0.3 :segments 8 :rings 4)
           a (sop/uv_sphere :radius (if (> t 1) 1.5 (+ 0.5 t)) :segments 8 :rings 4)
           b (sop/uv_sphere :radius (+ 0.5 (* 0.1 t)) :segments 8 :rings 4)]
      (sop/merge a b c))))"#;

fn spheres(graph: &edit::T) -> Vec<i64> {
    let mut ids: Vec<i64> = edit::inspect(graph).into_iter().filter(|n| n.label == "uv_sphere").map(|n| n.id).collect();
    ids.sort();
    ids
}

/// Value_lane over a lowered workspace.
pub fn value_lanes() {
    let lowered = lower_text(SMALL);
    let graph = lowered.graphs[0].clone();
    let (c, a, b) = match spheres(&graph.network.geometry).as_slice() {
        [x, y, z] => (*x, *y, *z),
        _ => fail("three spheres"),
    };
    // sources are lowered in plan order: c is the static one, a and b live
    check(lowered.pending.len() == 2, "two live arguments");
    let volatile = lower::is_volatile(&lowered);
    check(!volatile(c) && volatile(a) && volatile(b), "volatile: live nodes only");
    check(volatile(graph.root.unwrap()), "volatile: downstream merge");
    let live_markers = edit::inspect(&graph.network.geometry).iter().filter(|n| n.operation == "flow.card" && volatile(n.id)).count() as i64;
    check(lower::counts(&lowered).0 == 3 + live_markers, "counts: 3 live cards");
    let lane = value_lane::create(None);
    let resolve = |time: f64| value_lane::resolve(None, None, &lane, time, &graph.network).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)));
    let r2 = resolve(2.);
    check(r2.time_dependent, "a live network is time dependent");
    let r2b = resolve(2.);
    let node_of = |r: &value_lane::Resolved, id: i64| edit::find(&r.geometry, id).expect("node");
    check(node::ptr_eq(&node_of(&r2b, a), &node_of(&r2, a)) && node::ptr_eq(&node_of(&r2b, b), &node_of(&r2, b)), "same time reuses the resolution");
    let r3 = resolve(3.);
    check(node::ptr_eq(&node_of(&r3, a), &node_of(&r2, a)), "a port that did not change was applied again");
    check(!node::ptr_eq(&node_of(&r3, b), &node_of(&r2, b)), "a changed port was not applied");
    check(node::ptr_eq(&node_of(&r3, c), &node_of(&r2, c)), "a static node was touched");
    let r5 = resolve(5.);
    check(!node::ptr_eq(&node_of(&r5, b), &node_of(&r3, b)) && node::ptr_eq(&node_of(&r5, a), &node_of(&r3, a)), "only b changes at t=5");
    // a static network is resolved once
    let static_ = lower_text(&case_text("sunflower"));
    let sgraph = static_.graphs[0].clone();
    let slane = value_lane::create(None);
    let s0 = value_lane::resolve(None, None, &slane, 0., &sgraph.network).expect("resolve");
    let s9 = value_lane::resolve(None, None, &slane, 9., &sgraph.network).expect("resolve");
    check(!s0.time_dependent && !s9.time_dependent, "a static network was resolved again");
    check(lower::counts(&static_).0 == 0, "sunflower has no live nodes");
}

/// The live value equals the static evaluation at that time.
pub fn live_equals_static_sunflower() {
    live_equals_static("sunflower", SUNFLOWER_LIVE.to_string());
}

pub fn live_equals_static_orrery() {
    live_equals_static("orrery", case_text("orrery"));
}

const SUNFLOWER_LIVE: &str = r#"(workspace sunflower
  (graph sunflower :context sop [(seeds : int 6) (spread : float 0.062)]
    (let* [seeds_each (for [i (range seeds)]
                        (let* [r (* (* spread (+ 1 (* 0.3 (sin t)))) (sqrt i))
                               a (* i 2.4)]
                          (sop/uv_sphere :radius 0.02 :center (value/polar r a 0)
                                         :segments 6 :rings 4)))]
      (sop/merge seeds_each))))"#;

fn live_equals_static(name: &str, source: String) {
    let mut live = player(1, lower_text(&source));
    let mut previous_bytes = Vec::new();
    for time in [0., 2.25, 0.5] {
        let (_, geometry) = frame(&mut live, time);
        let mut oracle = player(1, lower_text(&at_time(&source, time)));
        let (_, expected) = frame(&mut oracle, 0.);
        check(geometry_bytes(&geometry) == geometry_bytes(&expected), &format!("{name} live at t={time} differs from the static evaluation"));
        let bytes = geometry_bytes(&geometry);
        check(bytes != previous_bytes, &format!("{name} did not animate"));
        previous_bytes = bytes;
        session::clear(&mut oracle.session);
        session::close(&mut oracle.session);
    }
    session::close(&mut live.session);
}

/// Orrery playback: 8 frames at Fixed 1/60.
pub fn orrery_playback() {
    let orrery = lower_text(&case_text("orrery"));
    let (live_nodes, cached_nodes) = lower::counts(&orrery);
    let mut p = player(1, orrery);
    let frames = 8;
    for n in 0..frames {
        frame(&mut p, n as f64 / 60.);
    }
    let stats = session::stats(&p.session);
    check(cached_nodes >= 4, "orrery caches base (tube, noise, normals) and plinth");
    check(stats.misses - stats.volatile_misses == cached_nodes, &format!("static nodes cook once ({} misses)", stats.misses - stats.volatile_misses));
    check(stats.hits - stats.volatile_hits == cached_nodes * (frames - 1), "static nodes always hit after the first frame");
    check(stats.volatile_misses == live_nodes * frames && stats.volatile_hits == 0, &format!("live nodes miss every frame ({} misses of {})", stats.volatile_misses, live_nodes * frames));
    check(stats.evictions == 0, "static entries were evicted");
    check(stats.retained_entries == cached_nodes && stats.volatile_entries == live_nodes, "retained: static entries in the LRU, one slot per live node");
    session::close(&mut p.session);
}

/// Orrery fixed-step determinism at 1 and 3 domains.
pub fn orrery_fixed_step() {
    let fixed = |domains: i64| -> Vec<Vec<u8>> {
        let mut p = player(domains, lower_text(&case_text("orrery")));
        let bytes = [0, 137].iter().map(|n| geometry_bytes(&frame(&mut p, *n as f64 / 60.).1)).collect();
        session::close(&mut p.session);
        bytes
    };
    check(fixed(1) == fixed(3), "orrery fixed-step frames differ at 1 and 3 domains");
}

fn chain(amount: &str) -> String {
    format!(
        r##"(workspace w
  (graph g :context sop []
    (let* [a (sop/uv_sphere :radius 0.5 :segments 8 :rings 4)
           b (sop/transform a :translate [{amount} 0 0])
           c (sop/set_color b :color "#336699")
           d (sop/set_color (sop/uv_sphere :radius 0.2 :segments 8 :rings 4) :color "#336699")]
      (sop/merge c d))))"##
    )
}

/// Realtime edit: only the edited node's downstream cone recooks.
pub fn realtime_edit() {
    let first = lower_text(&chain("1"));
    let mut p = player(1, first.clone());
    let (_, original) = frame(&mut p, 0.);
    let before = session::stats(&p.session);
    // six cards; inlined, their bodies' nodes
    check(before.misses >= 6 && before.hits == 0, "chain cooks six cards");
    // scrub: re-lower with the ids kept, same session, same compile chain
    let scrubbed = lower_with(&chain("2"), Some(&first.compiled_ids), Some(&first.sites));
    let graph = scrubbed.graphs[0].clone();
    p.root = graph.root.unwrap();
    p.graph = graph;
    p.lowered = scrubbed;
    p.lane = value_lane::create(None);
    let (_, scrubbed_geometry) = frame(&mut p, 0.);
    let after = session::stats(&p.session);
    // Set Color reads topology/counts, so its cached Cd carries fresh P through the existing
    // component refresh. Only Transform and Merge must recook.
    check(after.misses - before.misses < before.misses, &format!("only the transform's cone recooks ({} misses)", after.misses - before.misses));
    check(after.hits - before.hits >= 4, "the upstream node, unchanged sibling chain and component-cached color hit");
    let mut fresh = player(1, lower_text(&chain("2")));
    let (_, expected) = frame(&mut fresh, 0.);
    check(geometry_bytes(&scrubbed_geometry) == geometry_bytes(&expected), "component-cached color did not carry the transformed positions");
    check(geometry_bytes(&scrubbed_geometry) != geometry_bytes(&original), "the transform scrub returned the previous geometry");
    session::close(&mut fresh.session);
    session::close(&mut p.session);
}

/// The editor's cook path: Lower.objects, timeline time, await.
pub fn editor_cook_path() {
    use rays_editor::timeline as tl;
    let orrery = lower_text(&case_text("orrery"));
    let objects: Vec<_> = lower::objects(&orrery).into_iter().map(|(id, n, d)| (id as i64, n, d)).collect();
    let digest = |g: &rdk_core::geometry::T| geometry_bytes(g);
    let mut cook = cook::create(prepare_surface(), 42, 97, Some(1), Some(true), None, 512, 256 * 1024 * 1024).expect("cook");
    cook::set_volatile(&cook, lower::is_volatile(&orrery));
    let mut reference = player(1, orrery.clone());
    let mut timeline = tl::stop(&tl::create()).0;
    for n in [0i64, 1, 2, 30, 7] {
        let (next, changes) = tl::seek(&timeline, n);
        timeline = next;
        let mut f = frame_at((0., 0.), vec![], 0);
        f.input.dt = 0.;
        let update = cook::update(None, None, false, &[], &rays_editor::pick::Set::default(), None, &cook, &super::cook_t::none_settings(), &objects.iter().map(|(i, n, d)| (*i, n, *d)).collect::<Vec<_>>(), None, param::Effects::default(), &changes, &timeline, &f, None);
        cook = update.cook.clone();
        check(update.edit_error.is_none(), "editor cook: edit error");
        check(cook::status(&cook) == sop::async_cook::Status::Idle && update.prepared_changed, "await: the cook of frame n is published in frame n");
        let expected = digest(&frame(&mut reference, tl::time(&timeline)).1);
        match cook::pieces(&cook) {
            [piece] => check(digest(&piece.prepared.geometry) == expected, &format!("editor cook at frame {n} shows another frame's geometry")),
            _ => fail("editor cook: one piece"),
        }
    }
    let stats = cook::stats(&cook);
    check(stats.evictions == 0 && stats.volatile_entries == lower::counts(&orrery).0, "editor cook: live nodes have one slot each");
    cook::close(&cook);
    session::close(&mut reference.session);
}
