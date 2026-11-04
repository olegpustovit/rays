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
    lower::of_checked(Some(factories), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let t3 = Instant::now();
    format!("catalog {:.3} check {:.3} lower {:.3}", (t1 - t0).as_secs_f64(), (t2 - t1).as_secs_f64(), (t3 - t2).as_secs_f64())
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
