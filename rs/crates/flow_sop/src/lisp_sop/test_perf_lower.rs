//! Lowering time of three small graphs (debug build); run with `cargo t -p flow_sop perf_lower -- --ignored --nocapture`.
use super::support::*;
use crate::{catalog, lower};
use crate::operators as ops;
use std::time::Instant;

fn time(text: &str) -> String {
    let forms = parse(text);
    let t0 = Instant::now();
    let factories = sop::nodes::factories();
    let cat = catalog::of_factories(crate::manifest::VERSION, &[], None, &factories).unwrap();
    let t1 = Instant::now();
    let (checked, _) = catalog::check_workspace(ops::all(), false, None, &cat, &forms);
    let checked = checked.unwrap();
    let t2 = Instant::now();
    use std::sync::atomic::Ordering::Relaxed;
    let prof = std::env::var("PROF").is_ok(); if prof { flow_eval::PROFILE_ON.store(true, Relaxed); }
    let (s0, a0, b0) = (flow_eval::EVAL_STEPS.load(Relaxed), mem_sys::alloc_count(), mem_sys::alloc_bytes_total());
    lower::of_checked(Some(factories.clone()), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let t3 = Instant::now();
    if prof { flow_eval::PROFILE_ON.store(false, Relaxed); eprintln!("{}", flow_eval::profile_report()); }
    let (s1, a1, b1) = (flow_eval::EVAL_STEPS.load(Relaxed), mem_sys::alloc_count(), mem_sys::alloc_bytes_total());
    lower::of_checked(Some(factories), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let t4 = Instant::now();
    format!("catalog {:.3} check {:.3} lower {:.3} warm {:.3} | steps {} allocs {} bytes {} ({:.1} allocs/step)", (t1 - t0).as_secs_f64(), (t2 - t1).as_secs_f64(), (t3 - t2).as_secs_f64(), (t4 - t3).as_secs_f64(), s1 - s0, a1 - a0, b1 - b0, (a1 - a0) as f64 / (s1 - s0).max(1) as f64)
}

#[test]
#[ignore = "benchmark"]
fn perf_lower() {
    for body in ["(sop/box)", "(sop/transform (sop/box) :translate [1 0 0])", "(sop/uv_sphere :radius 0.5)"] {
        eprintln!("PERF {body}: {}", time(&format!("(workspace w (graph g :context sop {body}))")));
    }
}

#[test]
#[ignore = "benchmark"]
fn perf_lower_box() { eprintln!("PERF box: {}", time("(workspace w (graph g :context sop (sop/box)))")); }
#[test]
#[ignore = "benchmark"]
fn perf_lower_transform() { eprintln!("PERF transform: {}", time("(workspace w (graph g :context sop (sop/transform (sop/box) :translate [1 0 0])))")); }
#[test]
#[ignore = "benchmark"]
fn perf_lower_sphere() { eprintln!("PERF sphere: {}", time("(workspace w (graph g :context sop (sop/uv_sphere :radius 0.5)))")); }
#[test]
#[ignore = "benchmark"]
fn perf_loop() { for _ in 0..12 { eprintln!("PERF loop: {}", time("(workspace w (graph g :context sop (sop/transform (sop/box) :translate [1 0 0])))")); } }
#[test]
#[ignore = "benchmark"]
fn perf_check_loop() {
    let forms = parse("(workspace w (graph g :context sop (sop/transform (sop/box) :translate [1 0 0])))");
    let factories = sop::nodes::factories();
    let cat = catalog::of_factories(crate::manifest::VERSION, &[], None, &factories).unwrap();
    for _ in 0..15 { let t = Instant::now(); let _ = catalog::check_workspace(ops::all(), false, None, &cat, &forms); eprintln!("PERF check {:.3}", t.elapsed().as_secs_f64()); }
}
#[test]
#[ignore = "benchmark"]
fn perf_check_phases() {
    let forms = parse("(workspace w (graph g :context sop (sop/transform (sop/box) :translate [1 0 0])))");
    let factories = sop::nodes::factories();
    let cat = catalog::of_factories(crate::manifest::VERSION, &[], None, &factories).unwrap();
    for round in 0..2 {
        let start = Instant::now();
        let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || start.elapsed().as_secs_f64()));
        let _ = catalog::check_workspace(ops::all(), false, None, &cat, &forms);
        let t = guard.finish();
        eprintln!("PERF phases round {round} total {:.3}", t.total);
        for e in t.entries.iter().take(12) { eprintln!("PERF   {} calls {} {:.3}", e.phase, e.calls, e.seconds); }
    }
}
#[test]
#[ignore = "benchmark"]
fn perf_lower_garden() {
    eprintln!("PERF garden: {}", time("(workspace w (graph garden :context sop [(seed : int 1) (count : int 40)] (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 48) spots (sop/scatter bed :count count :seed seed) dot (sop/uv_sphere :radius 0.06 :segments 8 :rings 4) dots (sop/copy_to_points dot spots) result (sop/set_color (sop/merge bed dots) :color \"#3b7d4e\")] result)))"));
}

// ---- perf gate: a DEBUG build lowers faster than native OCaml (rs/port/eval-design.md) ----
// OCaml native `Lower.lower_checked` times on the reference Windows host (ms, cold: first lowering in the process).
const GARDEN: &str = "(workspace w (graph garden :context sop [(seed : int 1) (count : int 40)] (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 48) spots (sop/scatter bed :count count :seed seed) dot (sop/uv_sphere :radius 0.06 :segments 8 :rings 4) dots (sop/copy_to_points dot spots) result (sop/set_color (sop/merge bed dots) :color \"#3b7d4e\")] result)))";
const OCAML_BASELINE_MS: [(&str, &str, f64); 4] = [
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
#[ignore = "perf gate: run in a DEBUG build with `RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats -- --ignored --nocapture`"]
fn debug_lowering_beats_native_ocaml() {
    // deep Lisp recursion: the garden needs the main thread's stack in a debug build
    let report = std::thread::Builder::new().stack_size(256 << 20).spawn(|| {
        OCAML_BASELINE_MS.iter().filter(|(n, _, _)| std::env::var("ONLY").map_or(true, |o| o == *n)).map(|(name, text, ocaml)| { let (total, evaluate) = cold_lower_seconds(text); (*name, total * 1000.0, *ocaml, evaluate * 1000.0) }).collect::<Vec<_>>()
    }).unwrap().join().unwrap();
    let build = if cfg!(debug_assertions) { "debug" } else { "release" };
    for (name, ms, ocaml, evaluate) in &report { eprintln!("GATE {build} {name}: {ms:.0} ms (evaluator {evaluate:.0} ms; OCaml native {ocaml:.0} ms, {:.1}x)", ms / ocaml); }
    let slower: Vec<_> = report.iter().filter(|(_, ms, ocaml, _)| ms >= ocaml).map(|(name, ms, ocaml, _)| format!("{name} {ms:.0} ms >= {ocaml:.0} ms")).collect();
    assert!(slower.is_empty(), "{build} lowering is not faster than native OCaml: {}", slower.join(", "));
}
