use crate::core_lower;
use crate::flow_ir::executor;
use crate::test_support::{self as ts, ok};
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::value::Value;
use input::{Event, Frame, Key};
use rays_diagnostic::Diagnostic;

fn check(extra: &str, body: &str) -> W::Workspace { ts::check_workspace(&format!("(workspace w {extra} (graph g :context value {body}))")) }

fn bits(x: f64, y: f64) -> bool { x.to_bits() == y.to_bits() }

fn same_bits(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => bits(*x, *y),
        (Value::List(xs), Value::List(ys)) => xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| same_bits(a, b)),
        (Value::Record(fs), Value::Record(gs)) => fs.len() == gs.len() && fs.iter().zip(gs).all(|((n, a), (m, b))| n == m && same_bits(a, b)),
        (a, b) => a == b,
    }
}

fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(a), Ok(b)) => same_bits(a, b),
        (Err(a), Err(b)) => a.code == b.code && a.span == b.span,
        _ => false,
    }
}

fn live(frame: i64) -> Frame {
    Frame {
        frame,
        tick: frame,
        events: if frame % 2 == 1 { vec![Event::Key_pressed(Key::KeyChar('a')), Event::Key_pressed(Key::KeyChar('b'))] } else { vec![] },
        ..input::at_time(frame as f64 * 0.25)
    }
}

fn state_dump(state: &E::State) -> String { format!("{:?}", E::host::state_values(state)) }

fn values_of(evaluation: &E::Eval) -> Vec<Value> {
    let mut values: Vec<Value> = evaluation.results.iter().map(|(_, v)| v.clone()).collect();
    values.extend(evaluation.states.iter().cloned());
    values
}

fn state_slice(cases: &[(String, String, String)], used: &mut usize, refused: &mut Vec<String>) {
    for (name, extra, body) in cases {
        let evaluation = ok(E::static_eval(&check(extra, &format!("(let* [v {body}] 0.)"))));
        let values = values_of(&evaluation);
        let reference = E::create_state();
        let core = E::create_state();
        let programs: Vec<(Value, Result<executor::Program, Diagnostic>)> = values.into_iter().map(|v| { let p = executor::compile(&v, &Default::default()); (v, p) }).collect();
        for (_, program) in &programs {
            match program {
                Ok(_) => *used += 1,
                Err(d) => refused.push(format!("{name}: {} {}", d.code, d.message)),
            }
        }
        for frame in 0..8 {
            let live = live(frame);
            for (v, program) in &programs {
                let core_before = E::state_stamp(&core);
                let reference_before = E::state_stamp(&reference);
                let expected = E::host::force_reference(v, &live, Some(&reference), None, None);
                let actual = match program {
                    Ok(program) => executor::force(program, &executor::ForceOptions { state: Some(core.clone()), ..Default::default() }, &live),
                    Err(_) => expected.clone(),
                };
                assert!(same(&actual, &expected), "{name} frame {frame}");
                if expected.is_err() {
                    assert!(E::state_stamp(&core) == core_before && E::state_stamp(&reference) == reference_before, "{name} frame {frame}: failed evaluation changed a state epoch");
                } else {
                    assert!(state_dump(&core) == state_dump(&reference), "{name} frame {frame}: state values differ");
                }
            }
        }
    }
}

fn all_cases() -> Vec<(String, String, String)> {
    let mut cases: Vec<(String, String, String)> = ts::state_cases().into_iter().map(|(name, body)| (name, String::new(), body)).collect();
    cases.extend(ts::state_refs());
    cases
}

fn run_state_cases(cases: &[(String, String, String)]) {
    let mut used = 0;
    let mut refused = Vec::new();
    state_slice(cases, &mut used, &mut refused);
    assert!(refused.is_empty(), "{}", refused.join("; "));
    assert!(used > 0);
}

#[test]
fn core_state_slots_pending_writes_keys_errors_and_rollback_match_the_walker_part_1() { run_state_cases(&all_cases()[..5]) }

#[test]
fn core_state_slots_pending_writes_keys_errors_and_rollback_match_the_walker_part_2() { run_state_cases(&all_cases()[5..10]) }

#[test]
fn core_state_slots_pending_writes_keys_errors_and_rollback_match_the_walker_part_3() { run_state_cases(&all_cases()[10..]) }

// Kernels that read state: a result map over a state value, a step map over its previous
// array, and a step map reading a second state. Each runs as a Core region with the state
// lowered before it, bit-identical to the walker in values and state at one and eight domains.
const KERNELS: [(&str, &str); 5] = [
    ("result", "(let* [s (state [a 0.] (+ a (frame/dt)))] (array/sum (map (fn [x] (+ x s)) (array/range 5000))))"),
    ("previous", "(array/sum (state [p (exact (array/range 5000))] (map (fn [x] (+ x t)) p)))"),
    // the state array shrinks every force: a map reading it never reuses the last size
    ("resized", "(let* [p (state [p (exact (array/range 5000))] (array/range (- (array/count p) 700)))] (array/sum (map (fn [x] (+ x 1)) p)))"),
    ("two-states", "(let* [a (state [s 0.] (+ s t))] (array/sum (state [p (exact (array/range 5000))] (map (fn [x] (+ x a)) p))))"),
    ("sum", "(let* [s (state [a 1] (+ a 1))] (array/sum (map (fn [x] (* x s)) (array/range 5000))))"),
];

fn kernel_case(index: usize) {
    let (name, body) = KERNELS[index];
    for domains in [1, 8] {
        ts::with_domains(domains, &|| {
            let evaluation = ok(E::static_eval(&check("", body)));
            let values = values_of(&evaluation);
            let has_region = values.iter().any(|v| match v {
                Value::Residual(r) => matches!(core_lower::lower(None, None, false, None, None, *r), Ok(core) if !core.regions.is_empty()),
                _ => false,
            });
            let outcomes: Vec<String> = values.iter().map(|v| match v { Value::Residual(r) => match core_lower::lower(None, None, false, None, None, *r) { Ok(core) => format!("ok regions={}", core.regions.len()), Err(form) => format!("refused {form}") }, other => format!("not residual {}", String::from_utf8_lossy(&E::show(other))) }).collect();
            assert!(has_region, "{name}: state kernel did not lower to a Core region: {outcomes:?}");
            let reference = E::create_state();
            let core = E::create_state();
            let programs: Vec<(Value, executor::Program)> = values.into_iter().map(|v| { let p = ok(executor::compile(&v, &Default::default())); (v, p) }).collect();
            for frame in 0..6 {
                let live = live(frame);
                for (v, program) in &programs {
                    let expected = E::host::force_reference(v, &live, Some(&reference), None, None);
                    let actual = executor::force(program, &executor::ForceOptions { state: Some(core.clone()), ..Default::default() }, &live);
                    assert!(same(&actual, &expected), "{name} frame {frame} domains {domains}");
                    assert!(state_dump(&core) == state_dump(&reference), "{name} frame {frame} domains {domains}: state values differ");
                }
            }
        });
    }
}

#[test]
fn core_state_kernels_result_match_the_walker_at_one_and_eight_domains() { kernel_case(0) }
#[test]
fn core_state_kernels_previous_match_the_walker_at_one_and_eight_domains() { kernel_case(1) }
#[test]
fn core_state_kernels_resized_match_the_walker_at_one_and_eight_domains() { kernel_case(2) }
#[test]
fn core_state_kernels_two_states_match_the_walker_at_one_and_eight_domains() { kernel_case(3) }
#[test]
fn core_state_kernels_sum_match_the_walker_at_one_and_eight_domains() { kernel_case(4) }
