//! Debug lowering must beat the recorded native baseline; failures remain visible.
use super::support::*;
use crate::{catalog, lower};
use crate::operators as ops;
use std::time::Instant;

// ---- perf gate: a DEBUG build lowers faster than archived native (docs/history/eval-design.md) ----
// archived native `Lower.lower_checked` times on the reference Windows host (ms, cold: first lowering in the process).
const GARDEN: &str = "(workspace w (graph garden :context sop [(seed : int 1) (count : int 40)] (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 48) spots (sop/scatter bed :count count :seed seed) dot (sop/uv_sphere :radius 0.06 :segments 8 :rings 4) dots (sop/copy_to_points dot spots) result (sop/set_color (sop/merge bed dots) :color \"#3b7d4e\")] result)))";
const ARCHIVED_NATIVE_BASELINE_MS: [(&str, &str, f64); 4] = [
    ("box", "(workspace w (graph g :context sop (sop/box)))", 36.0),
    ("transform", "(workspace w (graph g :context sop (sop/transform (sop/box) :translate [1 0 0])))", 45.0),
    ("sphere", "(workspace w (graph g :context sop (sop/uv_sphere :radius 0.5)))", 205.0),
    ("garden", GARDEN, 260.0),
];

/// Cold lowering seconds of a checked workspace (the catalog and the check are not timed).
fn cold_lower_seconds(text: &str) -> (f64, f64) {
    let forms = parse(text);
    let factories = sop::nodes::factories();
    let cat = catalog::of_factories(crate::manifest::VERSION, &[], None, &factories).unwrap();
    let checked = catalog::check_workspace(ops::all(), false, None, &cat, &forms).0.unwrap();
    let start = Instant::now();
    let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || start.elapsed().as_secs_f64()));
    lower::of_checked(Some(factories), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let phases = guard.finish();
    if std::env::var("PHASES").is_ok() { eprintln!("PHASES {}", phases.entries.iter().map(|e| format!("{} {:.1}ms x{}", e.phase, e.seconds*1000.0, e.calls)).collect::<Vec<_>>().join(" | ")); }
    (start.elapsed().as_secs_f64(), flow_syntax::phase_timer::seconds(&phases, "evaluate"))
}

#[test]
fn debug_lowering_beats_archived_native() {
    // deep Lisp recursion: the garden needs the main thread's stack in a debug build
    let report = std::thread::Builder::new().stack_size(256 << 20).spawn(|| {
        ARCHIVED_NATIVE_BASELINE_MS.iter().filter(|(n, _, _)| std::env::var("ONLY").map_or(true, |o| o.split(',').any(|o| o == *n))).map(|(name, text, baseline)| { let (total, evaluate) = cold_lower_seconds(text); (*name, total * 1000.0, *baseline, evaluate * 1000.0) }).collect::<Vec<_>>()
    }).unwrap().join().unwrap();
    let build = if cfg!(debug_assertions) { "debug" } else { "release" };
    for (name, ms, baseline, evaluate) in &report { eprintln!("GATE {build} {name}: {ms:.0} ms (evaluator {evaluate:.0} ms; archived native {baseline:.0} ms, {:.1}x)", ms / baseline); }
    let slower: Vec<_> = report.iter().filter(|(_, ms, baseline, _)| ms >= baseline).map(|(name, ms, baseline, _)| format!("{name} {ms:.0} ms >= {baseline:.0} ms")).collect();
    assert!(slower.is_empty(), "{build} lowering is not faster than archived native: {}", slower.join(", "));
}

#[test]
fn validation_without_editor_records_preserves_geometry_and_errors() {
    let source = "(workspace w (defn shift :context value [(x : float)] (+ x 0.5)) (graph g :context sop (sop/mesh :points (exact (map (fn [(i : int)] [(shift (float i)) 0 0]) (array/range 512 :type int))) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))))";
    let (recorded, recorded_root) = lower_text(lower::Options::default(), source);
    let (validation, validation_root) = lower_text(lower::Options { record: false, ..Default::default() }, source);
    assert!(!recorded.evaluated.records.is_empty());
    assert!(validation.evaluated.records.is_empty());
    assert_eq!(geometry_bytes(&cook_once(1, &recorded_root)), geometry_bytes(&cook_once(1, &validation_root)));
    let bad = "(workspace w (graph g :context sop (sop/mesh :points (exact (map (fn [(i : int)] (if (int/eq i 256) (error \"E_COORD\" \"bad coordinate\") [(float i) 0 0])) (array/range 512 :type int))) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))))";
    let errors = [true, false].map(|record| {
        let error = lower::workspace(None, Vec::new(), lower::Options { record, ..Default::default() }, &parse(bad))
            .err().expect("selected computed error");
        (error.code, error.message, error.span)
    });
    assert_eq!(errors[0], errors[1]);
}


#[test]
fn swarm_topology_template_matches_original_at_kernel_and_degenerate_boundaries() {
    let library = include_str!("../../../../sketches/swarm_lib.rays");
    let start = library.find("          quads# ").expect("swarm topology start");
    let finish = library.find("          mesh# ").expect("swarm topology end");
    let topology = &library[start..finish];
    let original = topology.replace("share_body# (and (> ~tubes 1)", "share_body# (and false");
    assert_ne!(original, topology);
    let evaluate = |body: &str, tubes: i64, points: i64, sides: i64| {
        let body = body.replace("~tubes", "tubes").replace("~points", "points").replace("~sides", "sides");
        let source = format!("(workspace w (graph g :context value [(tubes : int {tubes}) (points : int {points}) (sides : int {sides})] (let* [last# (- points 1) rings# (* (* tubes points) sides) {body}] corners#)))");
        let catalog = flow_check::catalog::Catalog { version: 1, kinds: Vec::new() };
        let (checked, errors) = flow_check::workspace::check(ops::all(), false, false, None, &catalog, &parse(&source));
        let checked = checked.expect(&format!("{errors:?}"));
        flow_ir::flow_ir::static_(&flow_ir::flow_ir::StaticOptions { verify: true, ..Default::default() }, &checked)
            .map(|result| result.results[0].1.clone()).map_err(|error| (error.code, error.message))
    };
    for (tubes, points, sides) in [(0, 3, 3), (1, 3, 3), (2, 1, 3), (2, 0, 3), (2, 3, 0), (-1, 3, 3), (2, -1, 3), (2, 113, 3), (2, 114, 3)] {
        assert_eq!(evaluate(topology, tubes, points, sides), evaluate(&original, tubes, points, sides), "{tubes}/{points}/{sides}");
    }
}
