//! WebGPU kernel legality as diagnostics with spans.
use crate::target;
use crate::test_program::compile;
use flow_ir::core_ir::region;
use rays_diagnostic::Diagnostic;

fn has(code: &str, ds: &[Diagnostic]) -> bool { ds.iter().any(|d| d.code == code && d.span.is_some()) }

#[test]
fn webgpu_regions_refuse_with_spans() {
    // A legal kernel.
    let small = compile("(map (fn [x] (+ x t)) (array/range 1024))");
    assert!(target::region(&small, Some(1024)).is_empty());
    assert!(target::region(&small, Some(target::MAX_ELEMENTS)).is_empty());
    // Rule 1: more than eight storage buffers (one input, eight outputs, the status word).
    let many = {
        let o = small.outputs[0].clone();
        region::rebuild(&small, region::Rebuild { outputs: Some(vec![o; 8]), ..Default::default() }).expect("outputs")
    };
    let ds = target::region(&many, None);
    assert!(has("E_WEB_BUFFERS", &ds));
    // Rule 2: more elements than one dispatch of 64-wide workgroups can cover.
    let ds = target::region(&small, Some(target::MAX_ELEMENTS + 1));
    assert_eq!(ds.iter().map(|d| d.code.clone()).collect::<Vec<_>>(), vec!["E_WEB_ELEMENTS".to_owned()]);
    assert!(has("E_WEB_ELEMENTS", &ds));
    // The printer's own refusal passes through.
    let sum = compile("(sum [x (array/range 1024)] (+ x t))");
    assert!(has("E_GPU_FORM", &target::region(&sum, Some(1024))));
}
