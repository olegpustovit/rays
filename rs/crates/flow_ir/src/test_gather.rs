// Port of lib/flow_ir/test_gather.ml (the `--bench`, `--bench-text`, `--profile-text` and
// `--trace-text` blocks are timing/diagnostic harnesses and are not ported).
// Sizes: OCaml used 32769-row maps; 2049 keeps two 1024-lane block crossings (a second block
// boundary for the block/chunk error-order checks at 1500) within the 5 s debug budget. The
// 16383/16384 chunk-edge cases are shrunk to the 1023/1024/1025 block edges.
use crate::core_ir::Region::{self, Instruction};
use crate::packed;
use crate::test_support::*;
use flow_check::op::{self, Op};
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::packed_ops::Unary;
use flow_syntax::syntax;
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::{self as v, Value};
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

const MAX_INT: i64 = 4611686018427387903;
const MIN_INT: i64 = -4611686018427387904;
const MAX_ARRAY: i64 = (1 << 54) - 1;

fn source(ty: &Ty) -> Op {
    let mut o = op::all().iter().find(|o| o.name == "noise3").expect("noise3").clone();
    o.name = "probe/table".into();
    o.packed_extension = None;
    o.signature = op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] };
    let out = ty.clone();
    o.out = {
        let out = out.clone();
        Arc::new(move |_| Ok(out.clone()))
    };
    o.shape = op::Shape::Struct { splice: false };
    o.check = Arc::new(|_| Ok(()));
    o.body = Arc::new(move |_, _, _| Ok(Value::Source("probe/table".into(), out.clone(), vec![])));
    o
}

fn checked(ty: &Ty, text: &str) -> w::Workspace {
    let forms = ok(syntax::parse(text));
    match w::check(&[source(ty)], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}
fn tested(ws: &w::Workspace) -> Value {
    let ev = ok(e::r#static(ws, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).expect("record");
    records[0].1.clone()
}
fn make_fn(bindings: &str, ty: &Ty, body: &str) -> v::FnId {
    let text = format!("(workspace w (graph g :context value (let* [data (probe/table) {bindings} tested {body}] 0.0)))");
    match tested(&checked(ty, &text)) {
        Value::Fn(f) => f,
        _ => panic!("expected a function"),
    }
}
fn elem(ty: &Ty) -> Ty {
    match ty {
        Ty::Array(t) | Ty::List(t) => (**t).clone(),
        other => other.clone(),
    }
}
fn instantiate_with(table: Option<Value>, bindings: &str, ty: &Ty, body: &str, indices: Vec<i64>) -> Value {
    let mut f = make_fn(bindings, ty, body);
    if let Some(table) = table {
        f = e::Host::map_function_bindings(f, &|value| match value {
            Value::Source(name, _, _) if name == "probe/table" => table.clone(),
            other => other.clone(),
        });
    }
    ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int], result: elem(ty) }, f, &[Value::Int_array(Arc::new(indices))]))
}
fn instantiate(table: Option<&Value>, ty: &Ty, body: &str, indices: Vec<i64>) -> Value {
    instantiate_with(table.cloned(), "", ty, body, indices)
}
fn packed_of(fusion: bool, value: &Value) -> packed::T {
    ok_ds(compile_region(residual(value), fusion, None))
}
fn pk(value: &Value) -> packed::T {
    packed_of(true, value)
}
fn same_error(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) {
    match (a, b) {
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause, "{} {:?} {:?} / {} {:?} {:?}", a.code, a.message, a.cause, b.code, b.message, b.cause),
        _ => panic!("Expected gather failure: {:?} / {:?}", a.as_ref().map(bits), b.as_ref().map(bits)),
    }
}
fn array(ty: &Ty, count: usize) -> Value {
    if *ty == Ty::Int {
        let p = [MIN_INT, MAX_INT, 9007199254740993, 0, -1];
        return Value::Int_array(Arc::new((0..count).map(|i| p[i % 5]).collect()));
    }
    let width = flow_syntax::packed_ops::scalar_width(ty).unwrap();
    let p = [-0., 0., f64::from_bits(1), 0.25, -0.75];
    let values = Arc::new((0..count * width).map(|i| p[i % 5]).collect::<Vec<f64>>());
    match width {
        1 => Value::Float_array(values),
        2 => Value::Vec2_array(values),
        3 => Value::Vec3_array(values),
        _ => Value::Vec4_array(values),
    }
}
fn recorded(ty: &Ty, body: &str) -> Value {
    tested(&checked(ty, &format!("(workspace w (graph g :context value (let* [data (probe/table) tested {body}] 0.0)))")))
}
fn arr(ty: Ty) -> Ty {
    Ty::Array(Box::new(ty))
}
fn ints(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}
fn force(p: &packed::T, live: &input::Frame) -> Result<Value, Diagnostic> {
    packed_force(p, live, true, None)
}
fn force_r(p: &packed::T, live: &input::Frame, resolve: &ResolveArc) -> Result<Value, Diagnostic> {
    packed_force(p, live, true, Some(resolve.clone()))
}
fn reference_r(value: &Value, live: &input::Frame, resolve: &ResolveArc) -> Result<Value, Diagnostic> {
    reference_with(value, live, None, Some(resolve.clone()))
}
fn resolver(f: impl Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync + 'static) -> ResolveArc {
    Arc::new(f)
}
fn ok_value(table: Value) -> ResolveArc {
    resolver(move |_| Ok(table.clone()))
}
fn text_bytes(parts: &[&[u8]]) -> Vec<Vec<u8>> {
    parts.iter().map(|p| p.to_vec()).collect()
}
fn texts(parts: &[&[u8]]) -> Value {
    Value::Text_array(Arc::new(text_bytes(parts)))
}
fn lit(bytes: &[u8]) -> String {
    text_literal(bytes)
}
fn data_source_error(code: &str, message: &str, cause: Option<&str>) -> Diagnostic {
    let mut d = v::fail(code, message);
    d.cause = cause.map(|c| c.as_bytes().to_vec());
    d
}

// ---------------------------------------------------------------- numeric gathers

fn independent_tables(ty: Ty) {
    let table = array(&ty, 7);
    for count in [0usize, 1, 13, 2049] {
        let value = instantiate(Some(&table), &arr(ty.clone()), "(fn [(index : int)] (let* [alias data] (array/nth alias (int/mod index 7))))", (0..count as i64).collect());
        let reference = ok(force_reference(&value, &live_at(0.25)));
        for fusion in [false, true] {
            let p = packed_of(fusion, &value);
            assert_eq!(packed::static_count(&p), Some(count as i64));
            assert!(packed::gpu_refusals(false, false, &p).iter().any(|d| d.code == "E_GPU_FORM"));
            let executor = executor_compile(&value, true);
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    assert_eq!(bits(&ok(force(&p, &live_at(0.25)))), bits(&reference));
                    assert_eq!(bits(&ok(executor_force(&executor, true, &live_at(0.25)))), bits(&reference));
                });
            }
        }
    }
}
macro_rules! per_type {
    ($($name:ident: $ty:expr),*) => { $( #[test] fn $name() { independent_tables($ty); } )* };
}
per_type!(tables_int: Ty::Int, tables_float: Ty::Float, tables_vec2: Ty::Vec2, tables_vec3: Ty::Vec3, tables_vec4: Ty::Vec4);

fn int_table() -> Value {
    ints(&[MIN_INT, MAX_INT, 9007199254740993])
}

#[test]
fn helper_function_and_index_errors() {
    let table = int_table();
    let helper = instantiate_with(Some(table.clone()), "lookup (fn [(table : (array int)) (index : int)] (array/nth table index))", &arr(Ty::Int), "(fn [(index : int)] (lookup data (int/mod index 3)))", (0..2049).collect());
    assert_eq!(bits(&ok(force(&pk(&helper), &live_at(0.)))), bits(&ok(force_reference(&helper, &live_at(0.)))));
    for index in [-1, 3, MAX_ARRAY + 1, MAX_INT] {
        let value = instantiate(Some(&table), &arr(Ty::Int), "(fn [(index : int)] (array/nth data index))", vec![index]);
        same_error(&force(&pk(&value), &live_at(0.)), &force_reference(&value, &live_at(0.)));
    }
    let value = instantiate(Some(&table), &arr(Ty::Int), "(fn [(index : int)] (if (int/lt index 0) 0 (array/nth data index)))", vec![-1, 0, 1]);
    assert_eq!(ok(force(&pk(&value), &live_at(0.))), ints(&[0, MIN_INT, MAX_INT]));
    // A later lane's earlier instruction must not replace the first lane's error.
    let value = instantiate(
        Some(&table),
        &arr(Ty::Int),
        "(fn [(index : int)] (let* [divisor (if (int/eq index 1) 0 1) before (int/div 1 divisor)] (array/nth data index)))",
        (0..2049).map(|i| if i == 0 { 3 } else { 1 }).collect(),
    );
    for domains in [1, 4, 8] {
        with_domains(domains, &|| same_error(&force(&pk(&value), &live_at(0.)), &force_reference(&value, &live_at(0.))));
    }
    let value = instantiate(Some(&table), &arr(Ty::Int), "(fn [(index : int)] (array/nth data index))", (0..2049).map(|i| if i == 4 { 3 } else if i == 1500 { -1 } else { 0 }).collect());
    for domains in [1, 4, 8] {
        with_domains(domains, &|| same_error(&force(&pk(&value), &live_at(0.)), &force_reference(&value, &live_at(0.))));
    }
}

/// Host tables resolve on the initial domain, once for the selected execution.
#[test]
fn host_tables_resolve_once_on_the_initial_domain() {
    let table = int_table();
    let thread = std::thread::current().id();
    let calls = Arc::new(AtomicUsize::new(0));
    let resolve: ResolveArc = {
        let (calls, table) = (calls.clone(), table.clone());
        resolver(move |s| match s {
            Value::Source(name, _, _) if name == "probe/table" => {
                assert_eq!(std::thread::current().id(), thread);
                calls.fetch_add(1, Ordering::SeqCst);
                Ok(table.clone())
            }
            _ => Err(v::fail("E_DATA_SOURCE", "Unexpected source")),
        })
    };
    let value = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data (int/mod index 3)))", (0..2049).collect());
    for domains in [1, 8] {
        calls.store(0, Ordering::SeqCst);
        with_domains(domains, &|| {
            ok(force_r(&pk(&value), &live_at(0.), &resolve));
        });
        assert_eq!(calls.load(Ordering::SeqCst), 1);
    }
    let repeated = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (int/add (array/nth data 0) (array/nth data (int/mod index 3))))", (0..2049).collect());
    calls.store(0, Ordering::SeqCst);
    ok(force_r(&pk(&repeated), &live_at(0.), &resolve));
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    let untouched = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (if (int/lt index 0) 0 (array/nth data index)))", vec![-1]);
    calls.store(0, Ordering::SeqCst);
    ok(force_r(&pk(&untouched), &live_at(0.), &resolve));
    assert_eq!(calls.load(Ordering::SeqCst), 0);
}

fn failing_table() -> ResolveArc {
    resolver(|s| match s {
        Value::Source(name, _, _) if name == "probe/table" => Err(data_source_error("E_DATA_SOURCE", "Missing table", Some("table first"))),
        _ => panic!("unexpected source"),
    })
}

// Product defect (flow_eval force_reference vs packed): for `(array/nth data (int/div index 0))` with a failing table the
// packed kernel (and OCaml's reference) report the table error first (E_DATA_SOURCE, cause "table first"); the Rust
// reference reports the index error first (E_RANGE division by zero).
#[test]
fn argument_errors_precede_table_resolution() {
    let failing = failing_table();
    let live = live_at(0.);
    let ordered = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data (int/div index 0)))", vec![0]);
    same_error(&force_r(&pk(&ordered), &live, &failing), &reference_r(&ordered, &live, &failing));
    // Live term evaluation concretizes each let value, even an unused alias. That table position
    // must remain ordered against an earlier/later error.
    for body in [
        "(fn [(index : int)] (let* [alias data] 0))",
        "(fn [(index : int)] (let* [before (int/div index 0) alias data] (array/nth alias 0)))",
        "(fn [(index : int)] (let* [alias data after (int/div index 0)] (array/nth alias 0)))",
    ] {
        let value = instantiate(None, &arr(Ty::Int), body, vec![0]);
        same_error(&force_r(&pk(&value), &live, &failing), &reference_r(&value, &live, &failing));
    }
}

#[test]
fn handles_cannot_escape() {
    let table = int_table();
    let value = instantiate(Some(&table), &arr(Ty::Int), "(fn [(index : int)] (array/nth data index))", (0..2049).map(|i| if i == 4 { 3 } else { 0 }).collect());
    let region = pk(&value);
    let prepare = region.code.iter().position(|i| matches!(i, Instruction::Array_prepare(..))).expect("Array_prepare");
    assert!(Region::rebuild(&region, Region::Rebuild { outputs: Some(vec![vec![prepare as i64]]), ..Default::default() }).is_err());
    let input = region.code.iter().position(|i| matches!(i, Instruction::Int_input(..))).expect("Int_input") as i64;
    let mut code: Vec<Instruction> = region.code.to_vec();
    let last = code.len() - 1;
    code[last] = Instruction::Int_gather(input, input);
    assert!(Region::rebuild(&region, Region::Rebuild { code: Some(code), ..Default::default() }).is_err());
    let mut code: Vec<Instruction> = region.code.to_vec();
    code[prepare] = Instruction::Guarded(vec![(input, true)], Box::new(code[prepare].clone()));
    assert!(Region::rebuild(&region, Region::Rebuild { code: Some(code), ..Default::default() }).is_err());
    let dependent = instantiate(Some(&table), &arr(Ty::Int), "(fn [(index : int)] (array/nth (array/int (int/add index 1)) 0))", vec![0, 1]);
    assert!(compile_region(residual(&dependent), true, None).is_err());
}

#[test]
fn table_counts_are_independent_of_loop_counts() {
    let live = live_at(0.25);
    let table = int_table();
    for body in [
        "(for [i (array/range 2049 :type int) j (array/range 3 :type int)] (int/add (array/nth data (int/mod i 3)) (int/add j (frame/index))))",
        "(array/sum (map (fn [i] (int/add (array/nth data (int/mod i 3)) (frame/index))) (array/range 2049 :type int)))",
        "(fold [a (frame/index)] [i (array/range 2049 :type int)] (int/add a (array/nth data (int/mod i 3))))",
        "(scan [a (frame/index)] [i (array/range 2049 :type int)] (int/xor a (array/nth data (int/mod i 3))))",
    ] {
        let value = recorded(&arr(Ty::Int), body);
        let p = pk(&value);
        let core = executor_compile(&value, true);
        for t in [table.clone(), ints(&[9007199254740993, MIN_INT, MAX_INT])] {
            let resolve = ok_value(t);
            let expected = reference_r(&value, &live, &resolve);
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    assert_eq!(bits(&ok(force_r(&p, &live, &resolve))), bits(expected.as_ref().unwrap()));
                    assert_eq!(bits(&ok(exec_force(&core, &live, None, Some(resolve.clone())))), bits(expected.as_ref().unwrap()));
                });
            }
        }
    }
}

#[test]
fn skips_force_into_and_trust_boundaries() {
    let live = live_at(0.25);
    let resolve = ok_value(int_table());
    // Iteration skips exclude the body, while discarded sinks still execute it.
    let value = recorded(&arr(Ty::Int), "(for [i (array/range 13 :type int)] :skip [4] (if (int/eq i 4) (array/nth data -1) (int/add (array/nth data (int/mod i 3)) (frame/index))))");
    assert_eq!(bits(&ok(force_r(&pk(&value), &live, &resolve))), bits(&ok(reference_r(&value, &live, &resolve))));
    let value = instantiate(Some(&Value::Float_array(Arc::new(vec![0., -0., 1.]))), &arr(Ty::Float), "(fn [(index : int)] (array/nth data index))", vec![3]);
    let column_source: packed::ColumnSource = Arc::new(|_, _| Ok(None));
    let mut outputs = vec![vec![None]];
    let forced = packed::force_into(&pk(&value), &live, &packed::ForceOptions { strict: true, ..Default::default() }, &column_source, 1, &mut outputs).map(|_| Value::Int(0));
    same_error(&forced, &force_reference(&value, &live));
    // Trust boundaries retain source type, unresolved descriptor and finite errors.
    let ordered = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data (int/div index 0)))", vec![0]);
    let bad = instantiate(None, &arr(Ty::Float), "(fn [(index : int)] (array/nth data index))", vec![0]);
    for x in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        let resolve = ok_value(Value::Float_array(Arc::new(vec![x])));
        same_error(&force_r(&pk(&bad), &live, &resolve), &reference_r(&bad, &live, &resolve));
    }
    let empty = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data index))", vec![]);
    let calls = Arc::new(AtomicUsize::new(0));
    let counting: ResolveArc = {
        let calls = calls.clone();
        resolver(move |_| {
            calls.fetch_add(1, Ordering::SeqCst);
            Ok(int_table())
        })
    };
    assert_eq!(ok(force_r(&pk(&empty), &live, &counting)), ints(&[]));
    assert_eq!(calls.load(Ordering::SeqCst), 0);
}

#[test]
fn structured_core_changing_arguments_and_rebind() {
    let live = live_at(0.25);
    let table = int_table();
    let ws = composition_checked("[(table : (array int) (array/int 0))]", "(let* [nested {:table table}] (map (fn [i] (int/add (array/nth nested.table (int/mod i 3)) (frame/index))) (array/range 2049 :type int)))");
    let value = ok(e::r#static(&ws, false, Some(&[("g".to_string(), vec![("table".to_string(), table)])]))).results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
    let core = executor_compile(&value, true);
    assert_eq!(bits(&ok(executor_force(&core, true, &live))), bits(&ok(force_reference(&value, &live))));
    let changing = recorded(&arr(Ty::Int), "(let* [table (array/int 3 (int/add 9007199254740993 (frame/index))) nested {:table table}] (map (fn [i] (array/nth nested.table (int/mod i 3))) (array/range 2049 :type int)))");
    let program = executor_compile(&changing, true);
    for frame in [0i64, 42, 9007199254740993] {
        let mut l = live.clone();
        l.frame = frame;
        l.tick = frame;
        assert_eq!(bits(&ok(executor_force(&program, true, &l))), bits(&ok(force_reference(&changing, &l))));
    }
    let r = residual(&changing);
    let term = e::Residual::residual_view(r).unwrap().term;
    let template = packed::compile_template(&Default::default(), r, &term).expect("template");
    let rebound = packed::rebind(&template, r).expect("rebind");
    assert_eq!(bits(&ok(force(&rebound, &live))), bits(&ok(force_reference(&changing, &live))));
}

#[test]
fn unsupported_producer_has_no_walker_fallback() {
    let live = live_at(0.25);
    let value = recorded(&arr(Ty::Float), "(let* [table (map (fn [x] (value/rand x t)) (array/range 7))] (map (fn [i] (array/nth table i)) (array/range 7 :type int)))");
    match force(&pk(&value), &live) {
        Err(d) => assert!(d.code == "E_KERNEL_FORM" && d.message == "Numeric node bodies must compile to packed kernels."),
        Ok(_) => panic!("Unsupported table producer used reference evaluation"),
    }
}

#[test]
fn borrowed_columns_are_not_written() {
    let live = live_at(0.25);
    let n = 2049usize;
    let borrowed = recorded(&arr(Ty::Float), &format!("(map (fn [x] (array/nth data (int/mod (int x) 3))) (array/range {n}))"));
    let storage: Arc<Vec<f64>> = Arc::new((0..2 * n + 1).map(|i| if i >= 2 && i % 2 == 0 { ((i - 2) / 2) as f64 } else { f64::NAN }).collect());
    let original = format!("{:?}", float_bits_of(&storage));
    let resolve = ok_value(Value::Float_array(Arc::new(vec![-0., 0., 0.25])));
    let column_source: packed::ColumnSource = {
        let storage = storage.clone();
        Arc::new(move |_, _| Ok(Some(vec![packed::Column { data: storage.clone(), offset: 2, stride: 2 }])))
    };
    let mut outputs = vec![vec![Some(vec![0.; n])]];
    ok(packed::force_into(&pk(&borrowed), &live, &packed::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() }, &column_source, n as i64, &mut outputs));
    assert_eq!(format!("{:?}", float_bits_of(&storage)), original);
    let output = Value::Float_array(Arc::new(outputs[0][0].clone().unwrap()));
    assert_eq!(bits(&output), bits(&ok(reference_r(&borrowed, &live, &resolve))));
}
fn float_bits_of(a: &[f64]) -> Vec<u64> {
    a.iter().map(|x| x.to_bits()).collect()
}

fn rollback_resolver(state: e::State, fold: Value, live: input::Frame, table: Value) -> ResolveArc {
    resolver(move |_| {
        ok(e::Host::force_reference(&fold, &live, Some(&state), None, None));
        Ok(table.clone())
    })
}

#[test]
fn state_rolls_back_on_gather_errors() {
    let live = live_at(0.25);
    let table = int_table();
    let fold = recorded(&arr(Ty::Int), "(state [a 0] (int/add a 1))");
    let bad = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data index))", vec![-1]);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let state = e::create_state();
            let reference = e::create_state();
            let (before, reference_before) = (e::state_stamp(&state), e::state_stamp(&reference));
            let forced = packed::force(&pk(&bad), &live, &packed::ForceOptions { strict: true, state: Some(state.clone()), resolve: Some(rollback_resolver(state.clone(), fold.clone(), live.clone(), table.clone())), ..Default::default() });
            let expected = reference_with(&bad, &live, Some(&reference), Some(rollback_resolver(reference.clone(), fold.clone(), live.clone(), table.clone())));
            same_error(&forced, &expected);
            assert!(e::state_stamp(&state) == before && e::state_stamp(&reference) == reference_before);
        });
    }
}

// ---------------------------------------------------------------- typed one-lane copies

#[test]
fn one_lane_copies_constants_and_uniforms() {
    let live = live_at(0.25);
    let cases: Vec<(Ty, Value, String, Value)> = vec![
        (Ty::Int, ints(&[MAX_INT]), MIN_INT.to_string(), Value::Int(9007199254740993)),
        (Ty::Float, Value::Float_array(Arc::new(vec![f64::from_bits(1)])), "-0.0".into(), Value::Float(-0.)),
        (Ty::Text, texts(&[b"\xff\0"]), "\"constant\"".into(), Value::Text(b"\x80\0uniform".to_vec())),
    ];
    let thread = std::thread::current().id();
    for (ty, table, constant, scalar) in cases {
        for count in [0usize, 1, 1025, 2049] {
            let value = instantiate(Some(&table), &arr(ty.clone()), &format!("(fn [(index : int)] (if (int/lt index 0) {constant} (array/nth data index)))"), (0..count as i64).map(|i| if i % 2 == 0 { -1 } else { 0 }).collect());
            let reference = ok(force_reference(&value, &live));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| assert_eq!(bits(&ok(force(&pk(&value), &live))), bits(&reference)));
            }
        }
        for count in [1usize, 1025] {
            let value = recorded(&ty, &format!("(map (fn [(index : int)] (if (int/ge index 0) data (error \"E_UNTAKEN\" \"unused\"))) (array/range {count} :type int))"));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let calls = Arc::new(AtomicUsize::new(0));
                    let resolve: ResolveArc = {
                        let (calls, scalar) = (calls.clone(), scalar.clone());
                        resolver(move |_| {
                            assert_eq!(std::thread::current().id(), thread);
                            calls.fetch_add(1, Ordering::SeqCst);
                            Ok(scalar.clone())
                        })
                    };
                    let result = ok(force_r(&pk(&value), &live, &resolve));
                    assert_eq!(calls.load(Ordering::SeqCst), 1);
                    assert_eq!(bits(&result), bits(&ok(reference_with(&value, &live, None, Some(ok_value(scalar.clone()))))));
                });
            }
        }
    }
}

// ---------------------------------------------------------------- typed total prefix

#[test]
fn typed_total_prefix_wrapping_ints_and_text_bytes() {
    let live = live_at(0.25);
    let table = texts(&[b"raw\xff\0"]);
    let body = format!(
        "(fn [(index : int)] (let* [wrapped (int/add index 4611686018427387903) recovered (int/sub wrapped 4611686018427387903) magnitude (abs recovered) bit (int/and (int/or (int/xor magnitude 0) 0) 1)] (if (int/lt recovered 0) (if (int/eq bit 0) {} {}) (array/nth data 0))))",
        lit(b"even\xff\0"),
        lit(b"odd\x80\0")
    );
    for count in [0usize, 1, 1023, 1024, 1025, 2049] {
        let p6 = [MIN_INT, MAX_INT, 9007199254740993, -2, -1, 0];
        let value = instantiate(Some(&table), &arr(Ty::Text), &body, (0..count).map(|i| p6[i % 6]).collect());
        let reference = ok(force_reference(&value, &live));
        for fusion in [false, true] {
            let p = packed_of(fusion, &value);
            let bare = |i: &Instruction| -> Instruction {
                let mut cur = i.clone();
                while let Instruction::Guarded(_, inner) = cur {
                    cur = *inner;
                }
                cur
            };
            let boundary = p.code.iter().position(|i| matches!(bare(i), Instruction::Array_prepare(..))).expect("boundary");
            assert!(boundary > 0 && p.code[..boundary].iter().any(|i| matches!(i, Instruction::Int_input(..))));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| assert_eq!(bits(&ok(force(&p, &live))), bits(&reference)));
            }
        }
    }
}

/// A malformed public-region input is still checked at its selected instruction, even when earlier
/// total instructions have been batched.
#[test]
fn malformed_public_region_input_is_checked_when_selected() {
    let live = live_at(0.25);
    let table = texts(&[b"raw\xff\0"]);
    let template = pk(&instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (array/nth data 0))", vec![0]));
    let (rid, term) = template.sources[0].clone();
    let mut builder = flow_check::term_arena::TermBuilder::new();
    let form = builder.import_form(&term.form());
    let row = builder.push(None, term.ty(), form, flow_check::term_arena::Draft::RefBinding("bad_storage".into(), vec![]));
    let term = builder.freeze().handle(row);
    for count in [1usize, 2049] {
        let rid = ok(e::Residual::with_bindings(rid, &term, &[("bad_storage".to_string(), Value::Float_array(Arc::new(vec![0.; count])))]));
        for selected in [false, true] {
            let rebuild = Region::Rebuild {
                sources: Some(vec![(rid, Arc::new(term.clone()))]),
                widths: Some(vec![1]),
                uniforms: Some(vec![]),
                code: Some(vec![Instruction::Const(if selected { 1. } else { 0. }), Instruction::Guarded(vec![(0, true)], Box::new(Instruction::Int_input(0))), Instruction::Text_const(b"safe\xff\0".to_vec())]),
                outputs: Some(vec![vec![2]]),
                count: Some(Some(count as i64)),
                ..Default::default()
            };
            let p = Region::rebuild(&template, rebuild).unwrap_or_else(|ds| panic!("{}", show_ds(&ds)));
            for domains in [1, 4, 8] {
                with_domains(domains, &|| match force(&p, &live) {
                    Ok(result) => assert!(!selected && result == Value::Text_array(Arc::new(vec![b"safe\xff\0".to_vec(); count]))),
                    Err(d) => assert!(selected && d.code == "E_ARRAY_TYPE" && d.message == "Kernel input changed its packed element type.", "{}", d.message),
                });
            }
        }
    }
}

/// The later lane's earlier division may not beat this lane's lookup. Both block and worker chunk
/// boundaries must keep the reference cause.
#[test]
fn earliest_row_wins_at_block_boundaries() {
    let live = live_at(0.25);
    let table = texts(&[b"raw\xff\0"]);
    for first in [0usize, 1023, 1024, 1500] {
        let value = instantiate(
            Some(&table),
            &arr(Ty::Text),
            "(fn [(index : int)] (let* [divisor (if (int/eq index 2) 0 1) before (int/div 1 divisor)] (array/nth data index)))",
            (0..2049).map(|i| if i == first { 1 } else if i == first + 1 { 2 } else { 0 }).collect(),
        );
        let reference = force_reference(&value, &live);
        for fusion in [false, true] {
            for domains in [1, 4, 8] {
                with_domains(domains, &|| same_error(&force(&packed_of(fusion, &value), &live), &reference));
            }
        }
    }
}

// ---------------------------------------------------------------- prepared table prefix

#[test]
fn prepared_table_does_not_cross_a_force() {
    // The first two blocks leave the table untaken. Later warmed blocks still contain inactive
    // negative indices. Table contents and cardinality change across forces.
    let thread = std::thread::current().id();
    let value = instantiate(
        None,
        &arr(Ty::Text),
        &format!("(fn [(index : int)] (if (int/lt index 0) {} (array/nth data index)))", lit(b"safe\xff\0")),
        (0..2049).map(|i| if i < 2048 || i % 5 == 0 { -1 } else { i % 3 }).collect(),
    );
    let current = Arc::new(Mutex::new(texts(&[b"old\xff\0", b"old1", b"old2"])));
    for fusion in [false, true] {
        let p = packed_of(fusion, &value);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| {
                for (step, table) in [texts(&[b"old\xff\0", b"old1", b"old2"]), texts(&[b"short"]), texts(&[b"new\x80\0", b"new1", b"new2"])].into_iter().enumerate() {
                    *current.lock().unwrap() = table;
                    let calls = Arc::new(AtomicUsize::new(0));
                    let live = live_at(step as f64);
                    let resolve: ResolveArc = {
                        let (calls, current) = (calls.clone(), current.clone());
                        resolver(move |_| {
                            assert_eq!(std::thread::current().id(), thread);
                            calls.fetch_add(1, Ordering::SeqCst);
                            Ok(current.lock().unwrap().clone())
                        })
                    };
                    let result = force_r(&p, &live, &resolve);
                    let reference = reference_r(&value, &live, &ok_value(current.lock().unwrap().clone()));
                    assert_eq!(calls.load(Ordering::SeqCst), 1);
                    match (&result, &reference) {
                        (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
                        (Err(_), Err(_)) => same_error(&result, &reference),
                        _ => panic!("Prepared table crossed an execution boundary"),
                    }
                }
            });
        }
    }
}

/// A is prepared in block 0. A late bad A index in block 1 must not be reported ahead of an
/// earlier row's later B resolution/error/cause.
#[test]
fn late_bounds_versus_earlier_table_cause() {
    let live = live_at(0.25);
    let thread = std::thread::current().id();
    let fields = Ty::Record(vec![("a".into(), arr(Ty::Text)), ("b".into(), arr(Ty::Text))]);
    let closure = make_fn("", &fields, "(fn [(index : int)] (let* [chosen (array/nth data.a index)] (if (int/eq index 1) (array/nth data.b 0) chosen)))");
    let closure = e::Host::map_function_bindings(closure, &|value| match value {
        Value::Source(name, _, _) if name == "probe/table" => Value::Record(vec![
            ("a".into(), Value::Source("probe/a".into(), arr(Ty::Text), vec![])),
            ("b".into(), Value::Source("probe/b".into(), arr(Ty::Text), vec![])),
        ]),
        other => other.clone(),
    });
    let advancing = recorded(&arr(Ty::Text), "(state [counter 0] (int/add counter 1))");
    for (early, late) in [(500i64, 1050i64), (1050, 500), (1023, 1024), (1024, 1023)] {
        let value = ok(e::Host::map_function(
            None,
            None,
            &FnSignature { params: vec![Ty::Int], result: Ty::Text },
            closure,
            &[Value::Int_array(Arc::new((0..1100).map(|i| if i == early { 1 } else if i == late { 2 } else { 0 }).collect()))],
        ));
        let make = |state: e::State, calls: Arc<Mutex<Vec<&'static str>>>| -> ResolveArc {
            let advancing = advancing.clone();
            let live = live.clone();
            resolver(move |s| match s {
                Value::Source(name, _, _) if name == "probe/a" => {
                    assert_eq!(std::thread::current().id(), thread);
                    calls.lock().unwrap().push("a");
                    Ok(texts(&[b"a0", b"a1"]))
                }
                Value::Source(name, _, _) if name == "probe/b" => {
                    assert_eq!(std::thread::current().id(), thread);
                    calls.lock().unwrap().push("b");
                    ok(e::Host::force_reference(&advancing, &live, Some(&state), None, None));
                    Err(data_source_error("E_LATE_TABLE", "B failed", Some("b\u{ff}\0cause")))
                }
                _ => panic!("unexpected source"),
            })
        };
        let reference_state = e::create_state();
        let reference = reference_with(&value, &live, Some(&reference_state), Some(make(reference_state.clone(), Arc::new(Mutex::new(vec![])))));
        for fusion in [false, true] {
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let calls = Arc::new(Mutex::new(Vec::new()));
                    let state = e::create_state();
                    let (before, reference_before) = (e::state_stamp(&state), e::state_stamp(&reference_state));
                    let forced = packed::force(&packed_of(fusion, &value), &live, &packed::ForceOptions { strict: true, state: Some(state.clone()), resolve: Some(make(state.clone(), calls.clone())), ..Default::default() });
                    same_error(&forced, &reference);
                    assert!(e::state_stamp(&state) == before && e::state_stamp(&reference_state) == reference_before);
                    let want: Vec<&str> = if early < late { vec!["a", "b"] } else { vec!["a"] };
                    assert_eq!(*calls.lock().unwrap(), want);
                });
            }
        }
    }
}

// ---------------------------------------------------------------- text gathers

fn strings() -> Vec<Vec<u8>> {
    text_bytes(&[b"", "é日本😀".as_bytes(), b"nul\0tail", b"\xff\x80", b"\xed\xa0\x80", b"quote\"\\", b"\xef\xbb\xbfBOM"])
}

#[test]
fn text_nested_guards_and_error_arm() {
    // Nested one-lane guards change across blocks; the last lane alone takes the error arm, and
    // untaken invalid reads must never execute.
    let live = live_at(0.25);
    let table = Value::Text_array(Arc::new(strings()));
    for count in [2048usize, 2049] {
        let value = instantiate(
            Some(&table),
            &arr(Ty::Text),
            "(fn [(index : int)] (if (int/lt index 2048) (if (int/eq (int/mod index 2) 0) (array/nth data (int/mod index 7)) (if (int/ge index 0) \"odd\" (array/nth data -1))) (error \"E_LAST\" \"last lane\" :cause \"selected\")))",
            (0..count as i64).collect(),
        );
        let reference = force_reference(&value, &live);
        for fusion in [false, true] {
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let result = force(&packed_of(fusion, &value), &live);
                    if count == 2048 {
                        assert_eq!(bits(&ok(result)), bits(reference.as_ref().unwrap()));
                    } else {
                        same_error(&result, &reference);
                    }
                });
            }
        }
    }
}

#[test]
fn text_leaves_are_immutable_byte_copies() {
    let live = live_at(0.25);
    let strs = strings();
    let table = Value::Text_array(Arc::new(strs.clone()));
    for count in [0usize, 1, 13, 2049] {
        let value = instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (if (int/ge index 0) (array/nth data (int/mod index 7)) \"untaken\"))", (0..count as i64).collect());
        let reference = ok(force_reference(&value, &live));
        for fusion in [false, true] {
            let p = packed_of(fusion, &value);
            assert!(packed::static_count(&p) == Some(count as i64) && !packed::gpu_refusals(false, false, &p).is_empty());
            let core = executor_compile(&value, true);
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    let got = ok(force(&p, &live));
                    assert_eq!(bits(&got), bits(&reference));
                    assert_eq!(bits(&ok(executor_force(&core, true, &live))), bits(&reference));
                    match got {
                        Value::Text_array(out) => {
                            for (i, s) in out.iter().enumerate() {
                                assert_eq!(s, &strs[i % 7]);
                            }
                        }
                        _ => panic!("text array"),
                    }
                });
            }
        }
    }
}

#[test]
fn text_helper_lookup() {
    let live = live_at(0.25);
    let strs = strings();
    let table = Value::Text_array(Arc::new(strs.clone()));
    let value = instantiate_with(Some(table.clone()), "lookup (fn [(names : (array text)) (index : int)] (array/nth names index))", &arr(Ty::Text), "(fn [(index : int)] (let* [alias data] (lookup alias (int/mod index 7))))", (0..2049).collect());
    assert_eq!(bits(&ok(force(&pk(&value), &live))), bits(&ok(force_reference(&value, &live))));
}

#[test]
fn text_select_and_error_causes() {
    let live = live_at(0.25);
    let strs = strings();
    let table = Value::Text_array(Arc::new(strs.clone()));

    let closure = make_fn("", &arr(Ty::Text), "(fn [(index : int) (old : text)] (if (int/ge index 0) (array/nth data (int/mod index 7)) old))");
    let closure = e::Host::map_function_bindings(closure, &|value| match value {
        Value::Source(name, _, _) if name == "probe/table" => table.clone(),
        other => other.clone(),
    });
    let indices: Vec<i64> = (0..2049).map(|i| if i % 5 == 0 { -1 } else { i }).collect();
    let old: Vec<Vec<u8>> = (0..2049).map(|i| strs[(i + 2) % 7].clone()).collect();
    let selected = ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int, Ty::Text], result: Ty::Text }, closure, &[Value::Int_array(Arc::new(indices.clone())), Value::Text_array(Arc::new(old.clone()))]));
    let selected_reference = bits(&ok(force_reference(&selected, &live)));
    for domains in [1, 4, 8] {
        with_domains(domains, &|| {
            let got = ok(force(&pk(&selected), &live));
            assert_eq!(bits(&got), selected_reference);
            match got {
                Value::Text_array(out) => {
                    for (i, s) in out.iter().enumerate() {
                        assert_eq!(s, if indices[i] < 0 { &old[i] } else { &strs[indices[i] as usize % 7] });
                    }
                }
                _ => panic!("text array"),
            }
        });
    }
    for indices in [vec![-1], vec![7], vec![MAX_INT], (0..2049).map(|i| if i == 3 { 7 } else if i == 1500 { -1 } else { 0 }).collect()] {
        let value = instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (array/nth data index))", indices);
        for domains in [1, 4, 8] {
            with_domains(domains, &|| same_error(&force(&pk(&value), &live), &force_reference(&value, &live)));
        }
    }
    let value = instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (if (int/lt index 0) \"safe\" (array/nth data index)))", vec![-1, 0]);
    assert_eq!(ok(force(&pk(&value), &live)), texts(&[b"safe", b""]));
}

#[test]
fn text_identity_uniform_and_failures() {
    let live = live_at(0.25);
    let strs = strings();
    let table = Value::Text_array(Arc::new(strs.clone()));
    let thread = std::thread::current().id();
    let identity = recorded(&arr(Ty::Text), "(map (fn [(old : text)] old) data)");
    assert!(!packed::gpu_refusals(false, false, &pk(&identity)).is_empty());
    let calls = Arc::new(AtomicUsize::new(0));
    let counting: ResolveArc = {
        let (calls, table) = (calls.clone(), table.clone());
        resolver(move |_| {
            assert_eq!(std::thread::current().id(), thread);
            calls.fetch_add(1, Ordering::SeqCst);
            Ok(table.clone())
        })
    };
    for domains in [1, 8] {
        calls.store(0, Ordering::SeqCst);
        with_domains(domains, &|| assert_eq!(ok(force_r(&pk(&identity), &live, &counting)), table));
        assert_eq!(calls.load(Ordering::SeqCst), 1);
    }
    let uniform = recorded(&Ty::Text, "(map (fn [(index : int)] (if (int/ge index 0) data \"safe\")) (array/range 2049 :type int))");
    calls.store(0, Ordering::SeqCst);
    let three = strs[3].clone();
    let text_resolve: ResolveArc = {
        let (calls, three) = (calls.clone(), three.clone());
        resolver(move |_| {
            assert_eq!(std::thread::current().id(), thread);
            calls.fetch_add(1, Ordering::SeqCst);
            Ok(Value::Text(three.clone()))
        })
    };
    assert_eq!(ok(force_r(&pk(&uniform), &live, &text_resolve)), Value::Text_array(Arc::new(vec![three.clone(); 2049])));
    assert_eq!(calls.load(Ordering::SeqCst), 1);

    let bad = instantiate(None, &arr(Ty::Text), "(fn [(index : int)] (array/nth data (int/div index 0)))", vec![0]);
    let failing = resolver({
        let three = three.clone();
        move |_| {
            let mut d = v::fail("E_DATA_SOURCE", "Text source failed");
            d.cause = Some(three.clone());
            Err(d)
        }
    });
    let advancing = recorded(&arr(Ty::Text), "(state [a 0] (int/add a 1))");
    let rollback = instantiate(None, &arr(Ty::Text), "(fn [(index : int)] (array/nth data index))", vec![-1]);
    for domains in [1, 8] {
        with_domains(domains, &|| {
            let state = e::create_state();
            let reference = e::create_state();
            let (before, reference_before) = (e::state_stamp(&state), e::state_stamp(&reference));
            let forced = packed::force(&pk(&rollback), &live, &packed::ForceOptions { strict: true, state: Some(state.clone()), resolve: Some(rollback_resolver(state.clone(), advancing.clone(), live.clone(), table.clone())), ..Default::default() });
            let expected = reference_with(&rollback, &live, Some(&reference), Some(rollback_resolver(reference.clone(), advancing.clone(), live.clone(), table.clone())));
            same_error(&forced, &expected);
            assert!(e::state_stamp(&state) == before && e::state_stamp(&reference) == reference_before);
        });
    }
}

#[test]
fn text_products_changing_tables_fusion_and_refusals() {
    let live = live_at(0.25);
    let table = Value::Text_array(Arc::new(strings()));
    let resolve = ok_value(table.clone());
    let product = recorded(&arr(Ty::Text), "(for [i (array/range 2049 :type int) j (array/range 3 :type int)] :skip [4] (if (int/eq i 1) (if (int/eq j 1) (array/nth data -1) \"old\") (array/nth data (int/mod i 7))))");
    for domains in [1, 4, 8] {
        with_domains(domains, &|| assert_eq!(bits(&ok(force_r(&pk(&product), &live, &resolve))), bits(&ok(reference_r(&product, &live, &resolve)))));
    }
    let changing = recorded(&arr(Ty::Text), &format!("(let* [table (array/text 7 (if (> t 0.) {} \"other\")) nested {{:names table}}] (map (fn [(index : int)] (array/nth nested.names (int/mod index 7))) (array/range 2049 :type int)))", lit("é日本😀".as_bytes())));
    let program = executor_compile(&changing, true);
    let r = residual(&changing);
    let term = e::Residual::residual_view(r).unwrap().term;
    let template = packed::compile_template(&Default::default(), r, &term).expect("template");
    let rebound = packed::rebind(&template, r).expect("rebind");
    for time in [-1., 1.] {
        let l = live_at(time);
        let expected = bits(&ok(force_reference(&changing, &l)));
        assert_eq!(bits(&ok(executor_force(&program, true, &l))), expected);
        assert_eq!(bits(&ok(force(&rebound, &l))), expected);
    }
    let fused = recorded(&arr(Ty::Text), "(map (fn [(old : text)] (if (> t 0.) old \"other\")) (map (fn [(index : int)] (array/nth data (int/mod index 7))) (array/range 2049 :type int)))");
    for fusion in [false, true] {
        for domains in [1, 4, 8] {
            with_domains(domains, &|| assert_eq!(bits(&ok(force_r(&packed_of(fusion, &fused), &live, &resolve))), bits(&ok(reference_r(&fused, &live, &resolve)))));
        }
    }
    let middle = instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (str (array/nth data index)))", vec![0]);
    assert!(compile_region(residual(&middle), true, None).is_err());
    let value = instantiate(Some(&table), &arr(Ty::Text), "(fn [(index : int)] (array/nth data index))", vec![0]);
    let p = pk(&value);
    let column_source: packed::ColumnSource = Arc::new(|_, _| Ok(None));
    let mut outputs = vec![vec![None]];
    assert!(packed::force_into(&p, &live, &packed::ForceOptions { strict: true, ..Default::default() }, &column_source, 1, &mut outputs).is_err());
    let output = p.outputs[0][0];
    let mut code: Vec<Instruction> = p.code.to_vec();
    code.push(Instruction::Unary(Unary::Not, output));
    let last = code.len() as i64 - 1;
    assert!(Region::rebuild(&p, Region::Rebuild { code: Some(code), outputs: Some(vec![vec![last]]), ..Default::default() }).is_err());
}

// ---------------------------------------------------------------- inline invariant table choices

#[test]
fn inline_table_choices_keep_selected_lane_forcing() {
    // Inline invariant table choices retain selected-lane forcing and exact causes. They are
    // compiled tables, not eager outer-let rewrites.
    let body = format!("(fn [(index : int)] (if (int/lt index 0) 0 (array/nth (if (> t 0.) data (error \"E_TABLE\" \"selected inline table\" :cause {})) (int/mod index 3))))", lit(b"raw\xff\0cause"));
    let thread = std::thread::current().id();
    for count in [0usize, 1, 2049] {
        let value = instantiate(None, &arr(Ty::Int), &body, (0..count as i64).collect());
        for fusion in [false, true] {
            let p = packed_of(fusion, &value);
            for domains in [1, 4, 8] {
                with_domains(domains, &|| {
                    for time in [0.25, -0.25] {
                        let live = live_at(time);
                        let calls = Arc::new(AtomicUsize::new(0));
                        let resolve: ResolveArc = {
                            let calls = calls.clone();
                            resolver(move |s| match s {
                                Value::Source(name, _, _) if name == "probe/table" => {
                                    assert_eq!(std::thread::current().id(), thread);
                                    calls.fetch_add(1, Ordering::SeqCst);
                                    Ok(ints(&[MIN_INT, MAX_INT, 9007199254740993]))
                                }
                                _ => panic!("unexpected source"),
                            })
                        };
                        let reference = reference_r(&value, &live, &resolve);
                        calls.store(0, Ordering::SeqCst);
                        let actual = force_r(&p, &live, &resolve);
                        match (&reference, &actual) {
                            (Ok(expected), Ok(got)) => assert_eq!(bits(got), bits(expected)),
                            (Err(_), Err(_)) => same_error(&actual, &reference),
                            _ => panic!("inline table mismatch"),
                        }
                        assert_eq!(calls.load(Ordering::SeqCst), if count == 0 || time < 0. { 0 } else { 1 });
                    }
                });
            }
        }
    }
    let value = instantiate(None, &arr(Ty::Int), &body, vec![-1]);
    let never: ResolveArc = resolver(|_| panic!("resolved"));
    assert_eq!(ok(force_r(&pk(&value), &live_at(-0.25), &never)), ints(&[0]));
    let varying = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth (if (int/eq index 0) data (array/int 1 7)) 0))", vec![0, 1]);
    assert!(compile_region(residual(&varying), true, None).is_err());
}

/// An integer source or table made once (a CSR) converts to the same float array each prepare, so a
/// device's identity check does not upload it again.
#[test]
fn integer_sources_and_tables_convert_once_per_array() {
    let csr = Arc::new((0..4096).collect::<Vec<i64>>());
    let inputs = || {
        packed::host::device_inputs(&packed::host::ValueInputs { arrays: vec![Value::Int_array(csr.clone())], uniforms: vec![Value::Int_array(csr.clone())], frame: vec![], count: 4096 }).unwrap()
    };
    let a = inputs();
    let b = inputs();
    assert!(Arc::ptr_eq(&a.arrays[0], &b.arrays[0]) && Arc::ptr_eq(&a.tables[0], &b.tables[0]));
    assert_eq!(a.arrays[0][4095], 4095.);
}

// ---------------------------------------------------------------- list tables

#[test]
fn list_tables_exact_widths_and_range_guards() {
    let live = live_at(0.);
    for ty in [Ty::Int, Ty::Float, Ty::Vec2, Ty::Vec3, Ty::Vec4] {
        let a = array(&ty, 7);
        let table = Value::List(Arc::new((0..7).map(|i| v::array_get(&a, i).unwrap()).collect()));
        let value = instantiate(Some(&table), &Ty::List(Box::new(ty.clone())), "(fn [(index : int)] (nth data (int/mod index 7)))", (0..2049).collect());
        let expected = ok(force_reference(&value, &live));
        let p = pk(&value);
        for domains in [1, 8] {
            with_domains(domains, &|| assert_eq!(bits(&ok(force(&p, &live))), bits(&expected)));
        }
    }
    let table = Value::List(Arc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    let list_int = Ty::List(Box::new(Ty::Int));
    for index in [-1, 3, MAX_INT] {
        let value = instantiate(Some(&table), &list_int, "(fn [(index : int)] (nth data index))", vec![index]);
        let actual = force(&pk(&value), &live);
        let expected = force_reference(&value, &live);
        assert!(actual.as_ref().err().unwrap().code == "E_LIST_RANGE" && actual.as_ref().err().unwrap().code == expected.as_ref().err().unwrap().code);
        same_error(&packed_force(&pk(&value), &live, false, None), &expected);
    }
    let closure = make_fn("", &list_int, "(fn [(index : float)] (nth data index))");
    let closure = e::Host::map_function_bindings(closure, &|value| match value {
        Value::Source(name, _, _) if name == "probe/table" => table.clone(),
        other => other.clone(),
    });
    for index in [-1., 3., 0.5, 2.0000000001, f64::MAX] {
        let value = ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Float], result: Ty::Int }, closure, &[Value::Float_array(Arc::new(vec![index]))]));
        let actual = force(&pk(&value), &live);
        assert_eq!(actual.as_ref().err().expect("range error").code, "E_LIST_RANGE");
        same_error(&packed_force(&pk(&value), &live, false, None), &force_reference(&value, &live));
    }
    for table in [Value::List(Arc::new(vec![])), Value::List(Arc::new(vec![Value::Int(1), Value::Text(b"mixed".to_vec())]))] {
        let value = instantiate(Some(&table), &list_int, "(fn [(index : int)] (nth data index))", vec![0]);
        match compile_region(residual(&value), true, None) {
            Err(ds) if ds.len() == 1 => assert_eq!(ds[0].code, "E_PACKED_CAPTURE"),
            _ => panic!("empty/mixed list table did not refuse by name"),
        }
    }
    let value = instantiate(Some(&table), &list_int, "(fn [(index : int)] (nth data (int/mod index 3)))", vec![0, 1, 2]);
    let r = residual(&value);
    let term = e::Residual::residual_view(r).unwrap().term;
    let p = packed::compile_template(&Default::default(), r, &term).expect("template");
    let changed = |items: Vec<Value>| -> v::ResidualId {
        match e::Host::rewrite(&value, &|x| match x {
            Value::List(_) => Some(Value::List(Arc::new(items.clone()))),
            _ => None,
        }) {
            Value::Residual(r) => r,
            _ => panic!("residual"),
        }
    };
    let rebound = packed::rebind(&p, changed(vec![Value::Int(9), Value::Int(8), Value::Int(7)])).expect("rebind");
    assert_eq!(ok(force(&rebound, &live)), ints(&[9, 8, 7]));
    assert!(packed::rebind(&p, changed(vec![Value::Int(9)])).is_none());
    assert_eq!(ok(force(&p, &live)), ints(&[1, 2, 3]));
}


// Product defect: Host::force_reference of a two-column Text map fails E_UNBOUND ("$kernel-input:0 is
// not bound") when it is called inside a multi-domain scope (4 or 8 domains); outside one it
// succeeds. OCaml's test calls the reference inside Parallel.run.
#[test]
fn text_select_reference_inside_domains() {
    let live = live_at(0.25);
    let strs = strings();
    let table = Value::Text_array(Arc::new(strs.clone()));
    let closure = make_fn("", &arr(Ty::Text), "(fn [(index : int) (old : text)] (if (int/ge index 0) (array/nth data (int/mod index 7)) old))");
    let closure = e::Host::map_function_bindings(closure, &|value| match value {
        Value::Source(name, _, _) if name == "probe/table" => table.clone(),
        other => other.clone(),
    });
    let indices: Vec<i64> = (0..2049).map(|i| if i % 5 == 0 { -1 } else { i }).collect();
    let old: Vec<Vec<u8>> = (0..2049).map(|i| strs[(i + 2) % 7].clone()).collect();
    let selected = ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int, Ty::Text], result: Ty::Text }, closure, &[Value::Int_array(Arc::new(indices)), Value::Text_array(Arc::new(old))]));
    for domains in [1, 4, 8] {
        with_domains(domains, &|| {
            assert_eq!(bits(&ok(force(&pk(&selected), &live))), bits(&ok(force_reference(&selected, &live))));
        });
    }
}

// Same defect as `argument_errors_precede_table_resolution`, for the trust-boundary and text-source cases.
#[test]
fn index_error_versus_table_source_errors() {
    let live = live_at(0.25);
    let ordered = instantiate(None, &arr(Ty::Int), "(fn [(index : int)] (array/nth data (int/div index 0)))", vec![0]);
    for returned in [Value::Float_array(Arc::new(vec![1.])), Value::Source("probe/table".into(), arr(Ty::Int), vec![])] {
        let resolve = ok_value(returned);
        same_error(&force_r(&pk(&ordered), &live, &resolve), &reference_r(&ordered, &live, &resolve));
    }
    let bad = instantiate(None, &arr(Ty::Text), "(fn [(index : int)] (array/nth data (int/div index 0)))", vec![0]);
    let failing = resolver(|_| Err(data_source_error("E_DATA_SOURCE", "Text source failed", Some("\u{3}"))));
    same_error(&force_r(&pk(&bad), &live, &failing), &reference_r(&bad, &live, &failing));
    for returned in [Value::Float_array(Arc::new(vec![0.])), Value::Source("probe/table".into(), arr(Ty::Text), vec![])] {
        let resolve = ok_value(returned);
        same_error(&force_r(&pk(&bad), &live, &resolve), &reference_r(&bad, &live, &resolve));
    }
}

// Product defect (order dependence): running the `lookup` helper map and then the two-column Text
// select map on one thread (the original single test body) fails the second with E_UNBOUND
// "$kernel-input:0 is not bound"; each passes alone.
#[test]
fn text_helper_then_select_same_thread() {
    text_helper_lookup();
    text_select_and_error_causes();
}

#[test]
fn text_error_cause_raw_bytes() {
    let live = live_at(0.25);
    let table = Value::Text_array(Arc::new(strings()));
    let error = instantiate(Some(&table), &arr(Ty::Text), &format!("(fn [(index : int)] (if (int/lt index 0) (error \"E_TEXT\" \"selected\" :cause {}) (array/nth data index)))", lit(b"raw\xff\0cause")), vec![-1, 0]);
    for domains in [1, 4, 8] {
        with_domains(domains, &|| {
            for fusion in [false, true] {
                let actual = force(&packed_of(fusion, &error), &live);
                assert_eq!(actual.as_ref().unwrap_err().cause.as_deref(), Some(b"raw\xff\0cause".as_slice()));
                same_error(&actual, &force_reference(&error, &live));
            }
        });
    }
}
