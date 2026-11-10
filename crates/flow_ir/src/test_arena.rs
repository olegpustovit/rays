use crate::core_exec::arena_buffers;
use crate::core_ir as c;
use crate::core_lower;
use crate::packed;
use crate::test_support::*;
use flow_eval as e;
use flow_syntax::value::Value;
use std::sync::Arc;

#[test]
fn arena_buffer_plan() {
    let plan = arena_buffers;
    // a chain of one size alternates two buffers; alone in its size, a local has none
    let first = [0, 1, 2, 3];
    let last = [1, 2, 3, 4];
    assert_eq!(plan(&first, &last, &[8, 8, 8, 8]), vec![0, 1, 0, 1]);
    assert_eq!(plan(&first, &last, &[8, 6, 4, 2]), vec![-1, -1, -1, -1]);
    // no two overlapping locals share; unplanned locals take nothing
    assert_eq!(plan(&first, &last, &[8, -1, 8, 4]), vec![0, -1, 0, -1]);
    assert_eq!(plan(&[0, 0], &[5, 5], &[8, 8]), vec![-1, -1]);
}

/// 20 regions over each other: an array/nth keeps them from fusing into one.
fn chain(count: usize, size: &dyn Fn(usize) -> i64) -> String {
    let mut out = format!("a0 (map (fn [(i : int)] (+ (float i) t)) (array/range {} :type int))\n", size(0));
    for k in 1..count {
        out.push_str(&format!(
            "a{k} (map (fn [(i : int)] (+ (array/nth a{} (int/mod i (array/count a{}))) 1.0)) (array/range {} :type int))\n",
            k - 1,
            k - 1,
            size(k)
        ));
    }
    out.push_str(&format!("tested (map (fn [(x : float)] (* x 2.0)) a{})", count - 1));
    out
}

fn core(body: &str) -> c::Program {
    let ws = check_workspace(&format!("(workspace w (graph g :context value (let* [{body}] 0.)))"));
    let ev = ok(e::r#static(&ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    match &records[0].1 {
        Value::Residual(r) => match core_lower::lower(Some(Value::Residual(*r)), None, true, Some(32768), None, *r) {
            Ok(p) => p,
            Err(name) => panic!("refused {name}"),
        },
        _ => panic!("expected a residual"),
    }
}

fn result_of(prepared: &crate::core_exec::Prepared, live: &input::Frame) -> String {
    let force: crate::core_exec::ForceFn = {
        let live = live.clone();
        Arc::new(move |p: &crate::core_exec::Packed| packed::force(p, &live, &packed::ForceOptions { strict: true, ..Default::default() }))
    };
    let options = crate::core_exec::Options { strict: true, force: Some(force), ..Default::default() };
    bits(&crate::core_exec::execute(prepared, live, &options).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d))))
}

#[test]
fn chain_of_regions() {
    let live = live_at(0.25);
    let count = 20usize;
    // distinct sizes: nothing can be shared, and every array dies as its reader finishes
    let distinct = |k: usize| 2_000 + (((k * 7) % 20) as i64) * 1_000;
    let program = core(&chain(count, &distinct));
    let mut invokes = 0;
    c::iter(&mut |s| { if matches!(s.node, c::StatementNode::Invoke { .. }) { invokes += 1; } }, &program.body);
    assert!(invokes >= count, "the chain fused into {invokes} regions: {}", c::pp(&program));
    // the structural half of the peak check: distinct sizes mean the plan shares nothing and the result is stable
    let plain = prepare(&program);
    let reference = result_of(&plain, &live);
    assert_eq!(result_of(&prepare_clobber(&program), &live), reference, "a dead array was read");

    // Reuse one arena size across block boundaries; a read after a clobbered array's last use becomes NaN.
    let program = core(&chain(count, &|_| 5_000));
    let plain = prepare(&program);
    let clobbered = prepare_clobber(&program);
    let reference = result_of(&plain, &live);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            for _ in 0..3 {
                assert_eq!(result_of(&plain, &live), reference, "arena changed a chain's result");
                assert_eq!(result_of(&clobbered, &live), reference, "a dead array was read");
            }
        });
    }
}

/// A long body over few live values takes few slots, so it keeps its 1024 lanes; a guarded one keeps its
/// results' own slots; a dead register read in place of a live one would come back NaN.
#[test]
fn long_body_few_slots() {
    let live = live_at(0.25);
    let links = 200;
    let mut body = String::from("tested (map (fn [(i : int)] (let* [x (+ (float i) t) v0 (+ x 1.0)\n");
    for k in 1..=links {
        body.push_str(&format!("v{k} (+ (* v{} 0.5) (sin x))\n", k - 1));
    }
    body.push_str(&format!("] (if (> x 100.0) v{links} (- 0.0 v{links})))) (array/range 4096 :type int))"));
    let program = core(&body);
    let region = &program.regions[0];
    let registers = c::region::registers(region);
    let code = region.code.len();
    assert!(code >= links && registers.slots <= (code / 10) as i64, "{code} registers took {} slots", registers.slots);
    let plain = result_of(&prepare(&program), &live);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            assert_eq!(result_of(&prepare_clobber(&program), &live), plain, "a register was read past its last use");
        });
    }
}
