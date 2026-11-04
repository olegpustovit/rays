//! Port of lib/flow_web/test_js_gpu.ml (P6.4a): the frame of a program whose mark-set attributes the
//! export placed on the GPU. `FrameOptions.gpu` prints the `Invoke` of a GPU region as
//! `await rt.gpuArray(k)`; without it the output is the CPU form, unchanged.
use crate::core_fixtures as fixtures;
use crate::js::{self, FrameOptions};
use crate::node_harness::repo_path;
use crate::test_program::die;
use flow_check::catalog::Catalog;
use flow_check::workspace as W;
use flow_eval as E;
use flow_ir::core_draw;
use flow_syntax::syntax;
use std::sync::Arc;

const SOURCE: &str = "(workspace w (graph g :context draw
  (let* [positions (map (fn [i] [(+ i t) (* i 2) 0]) (array/range 2048))
         radii (map (fn [i] (+ 1 (* 0.001 (+ i t)))) (array/range 2048))]
    (draw/circles positions :radius radii))))";

fn has(text: &str, part: &str) -> bool { text.contains(part) }

#[test]
fn gpu_attribute_regions_print_gpu_array_and_cpu_programs_are_unchanged() {
    let forms = ((syntax::parse(SOURCE))).unwrap_or_else(|d| die(d));
    let ws = match W::check(&[fixtures::offset_op()], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        _ => panic!("does not check"),
    };
    let evaluation = ((E::static_eval(&ws))).unwrap_or_else(|d| die(d));
    let value = evaluation.results.iter().find(|(n, _)| n == "g").expect("g").1.clone();
    let program = match core_draw::lower(None, &evaluation.plan, &value) {
        Ok((p, _)) => p,
        Err(n) => panic!("{n}"),
    };
    assert_eq!(program.regions.len(), 2);
    let width = |region: i64| Some(program.regions[region as usize].outputs[0].len() as i64);
    let (eligible, batches) = core_draw::device_regions(&program, &width);
    assert!(eligible == vec![true, true] && batches == vec![vec![0, 1]]);
    let (eligible, batches) = core_draw::device_regions(&program, &|region| if region == 0 { None } else { width(region) });
    assert!(eligible == vec![false, true] && batches == vec![vec![1]]);
    let cpu = ((js::frame(&FrameOptions::default(), &program))).unwrap_or_else(|d| die(d));
    assert!(has(&cpu, "await rt.invoke(0, [])") && has(&cpu, "await rt.invoke(1, [])") && !has(&cpu, "gpuArray"));
    assert_eq!(cpu, ((js::frame(&FrameOptions { gpu: Some(Arc::new(|_| false)), ..Default::default() }, &program))).unwrap_or_else(|d| die(d)));
    let gpu = ((js::frame(&FrameOptions { gpu: Some(Arc::new(|_| true)), ..Default::default() }, &program))).unwrap_or_else(|d| die(d));
    assert!(has(&gpu, "await rt.gpuArray(0)") && has(&gpu, "await rt.gpuArray(1)") && !has(&gpu, "rt.invoke"));
    // One GPU region and one CPU region: only region 1 changes.
    let mixed = ((js::frame(&FrameOptions { gpu: Some(Arc::new(|k| k == 1)), ..Default::default() }, &program))).unwrap_or_else(|d| die(d));
    assert!(has(&mixed, "await rt.invoke(0, [])") && has(&mixed, "await rt.gpuArray(1)"));
    let path = repo_path("lib/flow_web/goldens/frame_gpu_circles.js");
    let expected = std::fs::read_to_string(&path).expect("golden");
    assert!(expected == gpu, "golden changed: {}", path.display());
}
