//! Port of lib/flow_web/test_region_arguments.ml: real Core Invoke arguments, exact output bytes,
//! lazy captures, malformed composite leaves, and synchronous-preparation refusals under Node.
use crate::js::{self, FrameOptions};
use crate::kernel_table::json_string;
use crate::node_harness::{node_available, run_ok, runtime_url};
use crate::test_program::die;
use flow_check::{catalog::Catalog, workspace as W};
use flow_eval as E;
use flow_ir::{core_exec, core_ir::{self as C, Region, StatementNode}, core_lower, packed};
use flow_syntax::{syntax, ty::Ty, value::{self, Value}};
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

const FRAMES: &[(i64, f64)] = &[(0, 0.25), (32766, 0.25), (32779, 0.25), (37767, 0.25), (65535, 0.25), (65539, 0.75), (65539, -0.25)];
const INT_MAP: &str = "(let* [src (array/range (int/sub (frame/index) 32766) :type int)] (map (fn [(i : int)] (int/add i 9007199254740993)) src))";
const SCALE: &str = "(let* [src (array/range (int/sub (frame/index) 32766)) scale (+ t 1.)] (map (fn [x] (* scale x)) src))";
const VEC3: &str = "(let* [src (array/vec3 (int/sub (frame/index) 32766) [t -0. 3]) captured {:delta [t 1. -0.]}] (map (fn [x] (+ x captured.delta)) src))";
const TABLE: &str = "(let* [src (array/range (int/sub (frame/index) 32766) :type int) table (array/range (int/sub (frame/index) 32766))] (map (fn [(i : int)] (array/nth table i)) src))";
const STORAGE: &str = "Compiled region argument storage differs from its declared type.";

fn checked(source: &str) -> W::Workspace {
    let forms = syntax::parse(source).unwrap_or_else(|d| die(d));
    match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

fn lower(value: &Value, strict: bool) -> C::Program {
    let Value::Residual(r) = value else { panic!("expected live region frame") };
    core_lower::lower(None, None, strict, None, None, *r).unwrap_or_else(|reason| panic!("Core argument case: {reason}"))
}

fn compiled(body: &str, require_arguments: bool) -> (Value, C::Program) {
    let ws = checked(&format!("(workspace bindings (graph g :context host (if (> t -1000.) {body} {body})))"));
    let evaluation = E::static_eval(&ws).unwrap_or_else(|d| die(d));
    let value = evaluation.results.iter().find(|(n, _)| n == "g").expect("g").1.clone();
    let program = lower(&value, true);
    if require_arguments {
        assert!(program.regions.iter().any(|r| !packed::host::argument_types(r).unwrap_or_else(|d| die(d)).is_empty()));
    }
    (value, program)
}

fn emitted(program: &C::Program) -> String { js::frame(&FrameOptions::default(), program).unwrap_or_else(|d| die(d)) }

fn script() -> String {
    assert!(node_available(), "region argument tests require Node.js on PATH");
    format!(r#"import assert from 'node:assert/strict';
import {{createRuntime}} from {};
import {{createPool}} from {};
import * as p from {};
const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);
const snapshot=v=>v instanceof p.PArray||v instanceof p.TextArray?snapshot(v.data):ArrayBuffer.isView(v)?bytes(v).toString('hex'):v instanceof Map?[...v].map(([n,x])=>[n,snapshot(x)]):Array.isArray(v)?v.map(snapshot):typeof v==='bigint'?v.toString():v;
const pools=[null,await createPool({{size:1}}),await createPool({{size:8}})];
try {{
"#, json_string(&runtime_url("runtime.js")), json_string(&runtime_url("workers.js")), json_string(&runtime_url("prims.js")))
}

fn finish(mut source: String) {
    source.push_str("} finally {for(const pool of pools)pool?.close()}\nconsole.log('region arguments passed');\n");
    assert_eq!(run_ok(&source).trim(), "region arguments passed");
}

fn open_frame(source: &mut String, program: &C::Program, with_regions: bool) {
    let regions = if with_regions {
        program.regions.iter().map(|r| format!("{{source:{}}}", json_string(&js::region(false, r).unwrap_or_else(|d| die(d))))).collect::<Vec<_>>().join(",")
    } else { String::new() };
    source.push_str(&format!("{{const {{frame}}=await import('data:text/javascript;base64,'+Buffer.from({}).toString('base64'));for(const pool of pools){{const rt=createRuntime({{regions:[{regions}]}},{{pool}});\n", json_string(&emitted(program))));
}

fn input(source: &mut String, frame: i64, time: f64) -> input::Frame {
    source.push_str(&format!("{{const input={{t:{},dt:0,frame:{frame},tick:{frame},size:[1,1]}};rt.current=input;\n", param::float_text(time)));
    let mut live = input::at_time(time);
    live.frame = frame;
    live.tick = frame;
    live
}

fn data(v: &Value) -> String {
    match v {
        Value::Int_array(a) => format!("BigInt64Array.from([{}])", a.iter().map(|n| format!("{n}n")).collect::<Vec<_>>().join(",")),
        Value::Text_array(a) if !a.is_empty() && a.iter().all(|s| s == &a[0]) => format!("Array({}).fill({})", a.len(), crate::text_literal::quote(&a[0])),
        Value::Text_array(a) => format!("[{}]", a.iter().map(|s| crate::text_literal::quote(s)).collect::<Vec<_>>().join(",")),
        Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a) => format!("Float64Array.from([{}])", a.iter().map(|n| param::float_text(*n)).collect::<Vec<_>>().join(",")),
        _ => panic!("expected array result: {v:?}"),
    }
}

fn expectation(source: &mut String, expected: Result<Value, Diagnostic>) {
    match expected {
        Ok(v) => {
            source.push_str(&format!("const got=await frame(input,rt.slots,rt),expected={};", data(&v)));
            source.push_str(if matches!(v, Value::Text_array(_)) { "assert.deepEqual(got.data,expected);\n" } else { "assert.deepEqual(bytes(got.data),bytes(expected));\n" });
        }
        Err(d) => {
            let cause = d.cause.as_deref().map(crate::text_literal::quote).unwrap_or_else(|| "undefined".into());
            source.push_str(&format!("await assert.rejects(()=>frame(input,rt.slots,rt),e=>{{assert.equal(e.code,{});assert.equal(e.message,{});assert.equal(e.cause,{cause});return true}});\n", json_string(&d.code), json_string(&d.message)));
        }
    }
    source.push_str("}\n");
}

fn compare(body: &str) {
    let (value, program) = compiled(body, true);
    let mut source = script();
    open_frame(&mut source, &program, true);
    source.push_str("const invoke=rt.invoke;rt.invoke=async function(k,args,plan){assert.ok(plan,'computed dependency needs a plan');const before=snapshot(args);try{return await invoke.call(this,k,args,plan)}finally{assert.deepEqual(snapshot(args),before,'arguments mutated')}};\n");
    for &(frame, time) in FRAMES {
        let live = input(&mut source, frame, time);
        expectation(&mut source, E::Host::force_reference(&value, &live, None, None, None));
    }
    source.push_str("}}\n");
    finish(source);
}

fn table_case(condition: &str, message: &str, full_count: bool) -> String {
    let count = if full_count { "(frame/index)" } else { "(int/sub (frame/index) 32766)" };
    format!(r#"(let* [src (array/range {count} :type int)] (map (fn [(i : int)] (if ({condition} i 0) (array/nth (if (> t 0.) (error "E_TABLE" "{message}" :cause "raw\255\000cause") (array/range 1)) 0) (float i))) src))"#)
}

macro_rules! case {
    ($name:ident, $body:expr) => { #[test] fn $name() { compare($body); } };
}
case!(int63_arguments, INT_MAP);
case!(boolean_capture, "(let* [src (array/range (int/sub (frame/index) 32766)) flag (> t 0.)] (map (fn [x] (if flag x (- 0. x))) src))");
case!(text_capture, "(let* [src (array/text (int/sub (frame/index) 32766)) label (str \"frame=\" (frame/index))] (map (fn [(x : text)] (if (> t 0.) label x)) src))");
case!(computed_vec2_argument, "(let* [src (map (fn [x] [x t]) (array/range (int/sub (frame/index) 32766)))] (map (fn [x] x) src))");
case!(float_capture, SCALE);
case!(record_vec3_capture, VEC3);
case!(vec2_result, "(let* [src (array/range (int/sub (frame/index) 32766))] (map (fn [x] [x t]) src))");
case!(vec4_argument, "(let* [src (array/vec4 (int/sub (frame/index) 32766) [t -0. 1. 2.])] (map (fn [x] x) src))");
case!(empty_product, "(let* [src (array/range (frame/index) :type int)] (for [i src j (array/range -1 :type int)] i))");
case!(empty_zip, "(let* [src (array/range (frame/index) :type int)] (map (fn [(i : int) (j : int)] i) src (array/range -1 :type int)))");
case!(unselected_table, &table_case("int/lt", "unreached table", false));
case!(selected_table, &table_case("int/eq", "selected table", false));
case!(computed_table_argument, TABLE);
case!(empty_suppresses_table, &table_case("int/eq", "empty suppresses table", true));

fn byte_text_case() -> String {
    let literal = (0..=255u8).map(|b| format!("\\{b:03}")).collect::<String>();
    format!("(let* [src (array/text (int/sub (frame/index) 32766) \"{literal}\")] (map (fn [(x : text)] (if (> t 0.) x \"untaken\")) src))")
}
case!(all_byte_text_argument, &byte_text_case());

#[test]
fn lazy_capture_statement_survives_workers() {
    let body = table_case("int/eq", "selected capture failed", true).replace("E_TABLE", "E_CAPTURE_LOCATION");
    let (_, program) = compiled(&body, true);
    let mut source = script();
    source.push_str("let expectedStatement;\n");
    open_frame(&mut source, &program, false);
    source.push_str("const invoke=rt.invoke;let reached=false;rt.invoke=async function(k,args,plan){try{return await invoke.call(this,k,args,plan)}catch(e){if(e.code==='E_CAPTURE_LOCATION'){reached=true;assert.notEqual(e.statement,undefined);if(expectedStatement===undefined)expectedStatement=e.statement;else assert.equal(e.statement,expectedStatement)}throw e}};\n");
    for count in [1000, 32769] {
        input(&mut source, count, 0.25);
        source.push_str(&format!("reached=false;await assert.rejects(()=>frame(input,rt.slots,rt),e=>{{assert.ok(reached);assert.equal(e.code,'E_CAPTURE_LOCATION');assert.equal(e.message,'selected capture failed');assert.equal(e.cause,{});assert.equal(e.statement,expectedStatement);return true}});}}\n", crate::text_literal::quote(b"raw\xff\0cause")));
    }
    source.push_str("}}\n");
    finish(source);
}

fn malformed(source: &mut String, body: &str, mutation: &str, code: &str, message: &str) {
    let (_, program) = compiled(body, true);
    malformed_program(source, &program, mutation, 65535, code, message);
}

fn malformed_program(source: &mut String, program: &C::Program, mutation: &str, count: i64, code: &str, message: &str) {
    open_frame(source, program, false);
    source.push_str(&format!("const invoke=rt.invoke;rt.invoke=function(k,args,plan){{if(plan&&args.length){{{mutation}}}return invoke.call(this,k,args,plan)}};\n"));
    input(source, count, 0.25);
    source.push_str(&format!("await assert.rejects(()=>frame(input,rt.slots,rt),e=>{{assert.equal(e.code,{});assert.equal(e.message,{});return true}});}}\n}}}}\n", json_string(code), json_string(message)));
}

#[test]
fn malformed_count_and_integer_storage() {
    let mut source = script();
    for (mutation, message) in [
        ("args=[];", "Compiled region argument count differs from its declaration."),
        ("args[plan.argumentTypes.findIndex(t=>t.array==='int')]=new p.PArray(1,Float64Array.of(1));", STORAGE),
    ] { malformed(&mut source, INT_MAP, mutation, "E_CORE_ARGUMENT", message); }
    finish(source);
}

#[test]
fn malformed_nonfinite_array_inputs() {
    let mut source = script();
    for number in ["NaN", "Infinity", "-Infinity"] {
        malformed(&mut source, SCALE, &format!("args[plan.argumentTypes.findIndex(t=>t.array==='float')]=new p.PArray(1,Float64Array.of({number}));"), "E_NONFINITE", "array input produced a nonfinite value.");
    }
    finish(source);
}

#[test]
fn malformed_vector_and_text_storage() {
    let mut source = script();
    malformed(&mut source, VEC3, "args[plan.argumentTypes.findIndex(t=>t.array==='vec3')]=new p.PArray(3,Float64Array.of(1,2));", "E_ARRAY_TYPE", "Packed vec3 storage has three coordinates per element.");
    malformed(&mut source, &byte_text_case(), "const i=plan.argumentTypes.findIndex(t=>t.array==='text');args[i]=new p.TextArray([]);args[i].data=[3];", "E_ARRAY_TYPE", "Text arrays contain text values.");
    finish(source);
}

// Add a real unused declaration, not fabricated runtime metadata: every physical leaf must bind.
fn unused(value: Value, mut program: C::Program) -> C::Program {
    let local = program.locals.len() as i64;
    let constant = program.constants.len() as i64;
    let ty = value::ty_of(&value);
    program.regions = program.regions.iter().map(|r| {
        let index = packed::host::argument_types(r).unwrap_or_else(|d| die(d)).len();
        let mut uniforms = r.uniforms.clone();
        uniforms.push(value::source_value("$core/argument", ty.clone(), vec![("index".into(), Value::Int(index as i64))]));
        let mut names = r.uniform_names.as_ref().clone();
        names.push((String::new(), vec![]));
        let r = Region::rebuild(r, Region::Rebuild { uniforms: Some(uniforms), uniform_names: Some(names), ..Default::default() }).expect("unused declaration");
        let declared = packed::host::argument_types(&r).unwrap_or_else(|d| die(d));
        assert_eq!(declared.len(), index + 1);
        assert_eq!(declared[index], ty);
        Arc::new(r)
    }).collect();
    fn rewrite(s: &mut C::Statement, local: i64) {
        match &mut s.node {
            StatementNode::Invoke { args, .. } => args.push(local),
            StatementNode::If { yes, no, .. } => for s in yes.iter_mut().chain(no) { rewrite(s, local); },
            _ => (),
        }
    }
    let mut id = 0;
    C::iter(&mut |s| id = id.max(s.id + 1), &program.body);
    for s in &mut program.body { rewrite(s, local); }
    program.body.insert(0, C::Statement { id, source: program.body[0].source.clone(), node: StatementNode::Assign(local, C::Expr::Constant(constant)) });
    program.locals.push(ty);
    program.constants.push(C::literal_of_value(&value).expect("composite literal"));
    program
}

#[test]
fn float_product_empty_and_nonfinite_capture() {
    let mut source = script();
    for body in [
        "(for [x (array/float (int/sub (frame/index) 32766)) y (array/float (if (> t 0.) -1 2))] (+ x y))",
        "(let* [gain (exp (* t 100000.))] (for [x (array/float (int/sub (frame/index) 32766)) y (array/float 2)] (+ x gain)))",
    ] {
        let (value, program) = compiled(body, false);
        let regions: Vec<_> = program.regions.iter().filter(|r| r.iteration == Region::Iteration::Product).collect();
        assert!(!regions.is_empty());
        for r in regions { assert!(r.code.iter().all(|i| matches!(i, Region::Instruction::Input(..) | Region::Instruction::Const(..) | Region::Instruction::Uniform(..) | Region::Instruction::Frame(..) | Region::Instruction::Binary(..) | Region::Instruction::Unary(..)))); }
        let program = unused(Value::List(Arc::new(vec![Value::Int(1)])), program);
        open_frame(&mut source, &program, false);
        for (frame, time) in [(0, 0.25), (32766, 0.25), (32767, 0.25), (65535, -0.25)] {
            let live = input(&mut source, frame, time);
            expectation(&mut source, E::Host::force_reference(&value, &live, None, None, None));
        }
        source.push_str("}}\n");
    }
    finish(source);
}

fn list(values: Vec<Value>) -> Value { Value::List(Arc::new(values)) }
fn leaves() -> Value { list(vec![Value::Int(9007199254740993), Value::Float(0.5)]) }
fn int_array() -> Value { Value::Int_array(Arc::new(vec![1, 2, 3])) }
fn record(value: Value) -> Value { Value::Record(vec![("leaves".into(), value)]) }

#[test]
fn unused_composite_arguments_validate_every_leaf() {
    let (_, original) = compiled(INT_MAP, true);
    let mut source = script();
    for value in [
        leaves(),
        list(vec![Value::Text(b"fallback".to_vec()), Value::Int(9007199254740993), Value::Float(0.5)]),
        Value::Record(vec![("leaves".into(), leaves()), ("large".into(), Value::Int((1i64 << 62) - 1))]),
        list(vec![int_array()]), record(list(vec![int_array()])),
        list(vec![Value::Int(9007199254740993), Value::Float(f64::NAN)]),
        record(list(vec![Value::Int(9007199254740993), Value::Float(f64::INFINITY)])),
    ] {
        let program = unused(value, original.clone());
        let prepared = core_exec::prepare(&program, false, false, &Default::default(), false);
        open_frame(&mut source, &program, false);
        for frame in [32766, 65535] {
            let live = input(&mut source, frame, 0.25);
            expectation(&mut source, core_exec::execute(&prepared, &live, &core_exec::Options { strict: true, rerun_on_error: false, ..Default::default() }));
        }
        source.push_str("}}\n");
    }
    finish(source);
}

#[test]
fn malformed_nested_integer_and_packed_layouts() {
    let (_, original) = compiled(INT_MAP, true);
    let mut source = script();
    for (value, mutation) in [
        (leaves(), "args.at(-1)[0]=1n<<63n"),
        (list(vec![Value::Text(b"fallback".to_vec()), Value::Int(9007199254740993), Value::Float(0.5)]), "args.at(-1)[1]=-(1n<<63n)-1n"),
        (record(leaves()), "args.at(-1).get('leaves')[0]=1n<<63n"),
        (list(vec![int_array()]), "args.at(-1)[0]=new p.PArray(3,BigInt64Array.of(1n,2n,3n))"),
        (record(list(vec![int_array()])), "args.at(-1).get('leaves')[0]=new p.PArray(3,BigInt64Array.of(1n,2n,3n))"),
        (list(vec![Value::Text(b"fallback".to_vec()), int_array()]), "args.at(-1)[1]=new p.PArray(5,Float64Array.of(1))"),
    ] { malformed_program(&mut source, &unused(value, original.clone()), mutation, 32766, "E_CORE_ARGUMENT", STORAGE); }
    finish(source);
}

fn replace_table(program: &C::Program, value: Value) -> C::Program {
    fn slot(i: &Region::Instruction) -> Option<i64> {
        match i { Region::Instruction::Array_prepare(n, _) => Some(*n), Region::Instruction::Guarded(_, body) => slot(body), _ => None }
    }
    let mut program = program.clone();
    let mut replaced = false;
    program.regions = program.regions.iter().map(|r| {
        if let Some(i) = r.code.iter().find_map(slot) {
            let mut uniforms = r.uniforms.clone();
            uniforms[i as usize] = value.clone();
            replaced = true;
            Arc::new(Region::rebuild(r, Region::Rebuild { uniforms: Some(uniforms), ..Default::default() }).expect("replace table"))
        } else { r.clone() }
    }).collect();
    assert!(replaced, "real lazy table preparation required");
    program
}

fn refused(program: &C::Program, message: Option<&str>) {
    let d = js::frame(&FrameOptions::default(), program).expect_err("unsupported region capture accepted");
    assert_eq!(d.code, "E_WEB_SOURCE");
    if let Some(message) = message { assert_eq!(d.message, message); }
}

#[test]
fn capture_preparation_refusals() {
    let (_, table) = compiled(TABLE, true);
    refused(&replace_table(&table, value::source_value("$host/unavailable", Ty::Array(Box::new(Ty::Float)), vec![])), None);
    let ws = checked("(workspace capture (graph g :context host [(source : (array float) (array/float 0))] (map (fn [x] (+ x t)) source)))");
    let inputs = vec![("g".into(), vec![("source".into(), Value::Float_array(Arc::new((0..32769).map(|i| i as f64).collect())))])];
    let evaluation = E::static_with(&ws, false, Some(&inputs), None).unwrap_or_else(|d| die(d));
    let value = evaluation.results.iter().find(|(n, _)| n == "g").expect("g").1.clone();
    let Value::Residual(r) = &value else { panic!("async capture needs a residual") };
    let view = E::Residual::residual_view(*r).unwrap_or_else(|d| die(d));
    packed::compile_result(&packed::CompileOptions::default(), *r, &view.term).expect("packed async capture");
    let program = lower(&value, false);
    assert_eq!(program.call_slots, 0);
    assert!(program.state_slots.is_empty());
    assert!(!program.regions.is_empty());
    emitted(&program);
    // Exercise this producer through a real templated Invoke, not an emitted-source prefix.
    let program = unused(list(vec![Value::Int(1)]), program);
    let mut source = script();
    open_frame(&mut source, &program, true);
    let live = input(&mut source, 3, 0.25);
    expectation(&mut source, E::Host::force_reference(&value, &live, None, None, None));
    source.push_str("}}\n");
    finish(source);
    refused(&replace_table(&table, value), Some("Asynchronous region captures require a compiled synchronous preparation path."));
    let (stateful, _) = compiled("(state [s (array/range 1)] s)", false);
    refused(&replace_table(&table, stateful), Some("Region preparation cannot own state or call-index slots."));
}
