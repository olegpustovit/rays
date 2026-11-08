// Port of lib/flow_ir/test_region_snapshot.ml: the kernel register programs of the shared
// fixtures and four workspaces, compared byte for byte with crates/flow_ir/snapshots/*.txt.
// Region_snapshot keeps one process-wide buffer, so the tests of this file take a lock.
use crate::core_lower;
use crate::flow_ir::{self, Executor, QualifyOptions};
use crate::packed;
use crate::region_snapshot;
use crate::test_support::{self as ts, ok};
use flow_check::catalog::{self, Catalog};
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::value::Value;
use input::Frame;
use std::sync::{Arc, Mutex};

static SNAPSHOT: Mutex<()> = Mutex::new(());

fn trace() -> ts::Trace { Arc::new(region_snapshot::record) }

fn manifest() -> Catalog {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/flow_sop/flow_manifest.sexp");
    ok(catalog::catalog_of_manifest(&std::fs::read_to_string(path).expect("flow_manifest.sexp"))).0
}

fn state_dump(state: &E::State) -> String { format!("{:?}", E::Host::state_values(state)) }

fn run(require_state_kernel: bool, checked: &W::Workspace) {
    let options = QualifyOptions {
        observe: Some(Arc::new(|_, residual| {
            let term = E::Residual::residual_view(residual).unwrap().term;
            for fusion in [true, false] {
                let options = packed::CompileOptions { trace: Some(trace()), fusion, ..Default::default() };
                let _ = packed::compile_result(&options, residual, &term);
            }
            let options = packed::CompileOptions { trace: Some(trace()), ..Default::default() };
            let _ = packed::compile_template(&options, residual, &term);
        })),
        ..Default::default()
    };
    let (_, evaluation) = ok(flow_ir::qualify_workspace(&options, checked));
    // Frame folds compile their kernels as templates while they run.
    // every Core program of the workspace is plain data
    let mut values: Vec<Value> = evaluation.results.iter().map(|(_, v)| v.clone()).collect();
    values.extend(evaluation.states.iter().cloned());
    for value in &values {
        if let Value::Residual(r) = value {
            if let Ok(program) = core_lower::lower(None, None, false, None, None, *r) {
                ts::plain_roundtrip(&program);
            }
        }
    }
    for value in &evaluation.states {
        if let Value::Residual(r) = value {
            if require_state_kernel {
                let core = core_lower::lower(None, Some(trace()), false, None, None, *r).expect("state lowering");
                if core.regions.is_empty() {
                    panic!("particle state residual did not qualify to a Core kernel");
                }
            }
        }
        let state = E::create_state();
        let reference_state = E::create_state();
        let program = ok(Executor::compile(value, &Executor::CompileOptions { trace: Some(trace()), ..Default::default() }));
        for frame in 0..2 {
            let live = Frame { frame, dt: 1. / 60., size: (800, 600), ..input::at_time(frame as f64 / 60.) };
            let actual = Executor::force(&program, &Executor::ForceOptions { state: Some(state.clone()), ..Default::default() }, &live);
            let expected = E::Host::force_reference(value, &live, Some(&reference_state), None, None);
            let same = match (&actual, &expected) {
                (Ok(a), Ok(b)) => format!("{a:?}") == format!("{b:?}"),
                (Err(a), Err(b)) => rays_diagnostic::to_string(a) == rays_diagnostic::to_string(b),
                _ => false,
            };
            if !same || state_dump(&state) != state_dump(&reference_state) {
                panic!("state residual differs from the reference walker");
            }
        }
    }
}

fn workspace(path: &str) {
    let _guard = SNAPSHOT.lock().unwrap_or_else(|e| e.into_inner());
    region_snapshot::start();
    let file = concat!(env!("CARGO_MANIFEST_DIR"), "/../../");
    let source = std::fs::read_to_string(format!("{file}{path}.rays")).expect("sketch");
    // The file's layout form is editor chrome, not part of the checked workspace.
    let forms = ok(syntax::parse(&source));
    match W::check(&[], false, false, None, &manifest(), &forms[..1]) {
        (Some(workspace), ds) if !ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) => run(path == "sketches/particles", &workspace),
        // A workspace holding graphs of kinds outside flow_ir (sop) cannot be checked here.
        (_, ds) => region_snapshot::append(&format!("skipped: {}\n", rays_diagnostic::to_string(&ds[0]))),
    }
    let name = format!("workspace_{}", path.rsplit('/').next().unwrap());
    if let Err(message) = region_snapshot::finish(&name) {
        panic!("{message}");
    }
}

#[test]
fn workspace_particles_snapshot() { workspace("sketches/particles") }
#[test]
fn workspace_flow_shader_snapshot() { workspace("sketches/flow_shader") }
#[test]
fn workspace_flow_particles_gpu_snapshot() { workspace("sketches/flow_particles_gpu") }
#[test]
fn workspace_flow_image_kernel_snapshot() { workspace("sketches/flow_image_kernel") }

#[test]
fn state_branch_laziness_and_shared_fixture_snapshot() {
    let _guard = SNAPSHOT.lock().unwrap_or_else(|e| e.into_inner());
    region_snapshot::start();
    let source = r#"(workspace lazy_state
    (graph g :context value
      (let* [held (state [s (array/range 3)]
        (if (> t 100.)
          (map (fn [x] (error "E_UNTAKEN" "inactive state map")) s)
          (map (fn [x] (+ x 1)) s)))] (+ (array/sum held) 0.))))"#;
    let checked = ts::check_workspace(source);
    let evaluation = ok(E::static_eval(&checked));
    let value = evaluation.states[0].clone();
    let program = ok(Executor::compile(&value, &Executor::CompileOptions { trace: Some(trace()), ..Default::default() }));
    let reference_state = E::create_state();
    let core_state = E::create_state();
    for time in [0., 101.] {
        let live = input::at_time(time);
        let expected = E::Host::force_reference(&value, &live, Some(&reference_state), None, None);
        let actual = Executor::force(&program, &Executor::ForceOptions { state: Some(core_state.clone()), ..Default::default() }, &live);
        let same = match (&actual, &expected) {
            (Ok(a), Ok(b)) => a == b,
            (Err(a), Err(b)) => rays_diagnostic::to_string(a) == rays_diagnostic::to_string(b),
            _ => false,
        };
        assert!(same, "state branch laziness differs at t={time}");
        assert!(state_dump(&core_state) == state_dump(&reference_state), "state branch changed inactive state at t={time}");
    }
    // OCaml evaluates the elements of a list literal right to left, so the snapshot holds the last
    // fixture of Test_program.fixtures / vector_fixtures first; compile them in that order.
    let array = "(array/range 1024)";
    let traced = |fusion: bool, body: String| { ts::compile_with(Some(trace()), fusion, "", &body); };
    traced(true, format!("(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) {array})"));
    traced(true, format!("(map (fn [x] (if (< x t) (+ x 1) (- x 1))) {array})"));
    traced(true, format!("(map (fn [x] (+ (* x 2) t)) {array})"));
    traced(false, format!("(let* [offset [t 1 2 3]]\n     (map (fn [(p : vec4)] (+ p offset)) (map (fn [x] [x 1 2 3]) {array})))"));
    traced(false, format!("(map (fn [(p : vec2)] [p.y p.x t 1])\n     (map (fn [x] [x (+ x 1)]) {array}))"));
    traced(true, format!("(map (fn [x] [x (+ x t)]) {array})"));
    if let Err(message) = region_snapshot::finish("test_program") {
        panic!("{message}");
    }
}
