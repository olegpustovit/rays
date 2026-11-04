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
