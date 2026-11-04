//! Port of lib/flow_sop/test_grid_bulk.ml. Lowering a static grid makes its per-point arrays as
//! block kernels, not by walking each point. The OCaml test counts the allocated words per point;
//! Rust has no allocation counter here, so the same fixture is lowered at both sizes and the time
//! must grow far less than a walk of every point would (a walked element costs thousands of words).
use super::support::*;
use crate::lower;

fn seconds(side: i64) -> f64 {
    let text = format!(
        "(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \"Triangles\" :columns {side} :rows {side} :uv_attribute \"uv\")))"
    );
    let start = std::time::Instant::now();
    lower::workspace(None, Vec::new(), lower::Options::default(), &parse(&text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    start.elapsed().as_secs_f64()
}

#[test]
#[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock"]
fn lowering_a_static_grid_does_not_walk_its_points() {
    // the head of each map (4,096 elements) is walked at every size; the rest must not be
    let small = seconds(70);
    let large = seconds(300);
    // 300x300 is 18x the points of 70x70; a walk would take about 18x as long
    assert!(large < small * 12.0 + 1.0, "lowering a static grid walks its points ({small:.3}s vs {large:.3}s)");
}
