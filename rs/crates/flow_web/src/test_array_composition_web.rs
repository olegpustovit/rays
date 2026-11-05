//! Port of lib/flow_web/test_array_composition_web.ml. Every comparison executes the emitted
//! frame and regions, with real per-frame preparation, on the main thread and workers 1/8.
use crate::{js, kernel_table::json_string, node_harness::{run_ok, runtime_url}, text_literal, wgsl};
use flow_check::{catalog::Catalog, workspace};
use flow_eval as e;
use flow_ir::{core_ir::Region::Instruction, core_lower, packed};
use flow_syntax::{syntax, ty::{FnSignature, Ty}, value::{FnId, Value}};
use std::sync::Arc;

fn evaluation(body: &str, record: bool) -> e::Eval {
    let forms = syntax::parse(&format!("(workspace composition (graph g :context host {body}))")).unwrap();
    let (ws, ds) = workspace::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms);
    assert!(ds.is_empty(), "{body}: {ds:?}");
    e::r#static(&ws.expect("checked workspace"), record, None).unwrap()
}

fn value(body: &str) -> (e::Eval, Value) {
    let ev = evaluation(&format!("(if (> t -1000.) {body} {body})"), false);
    let v = ev.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
    (ev, v)
}

fn data(v: &Value) -> String {
    match v {
        Value::Int_array(a) if !a.is_empty() && a.iter().all(|n| n == &a[0]) => format!("new BigInt64Array({}).fill({}n)", a.len(), a[0]),
        Value::Int_array(a) => format!("BigInt64Array.from([{}])", a.iter().map(|n| format!("{n}n")).collect::<Vec<_>>().join(",")),
        Value::Text_array(a) => {
            let mut palette = Vec::new();
            let mut indices = Vec::with_capacity(a.len());
            let mut seen = std::collections::HashMap::new();
            for text in a.iter() {
                let index = *seen.entry(text).or_insert_with(|| {
                    palette.push(text_literal::quote(text));
                    palette.len() - 1
                });
                indices.push(index.to_string());
            }
            format!("(()=>{{const p=[{}];return [{}].map(i=>p[i])}})()", palette.join(","), indices.join(","))
        }
        Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a) =>
            format!("Float64Array.from([{}])", a.iter().map(|n| param::float_text(*n)).collect::<Vec<_>>().join(",")),
        _ => panic!("composition array expected: {v:?}"),
    }
}

fn prelude() -> String {
    format!("import {{createRuntime}} from {};import {{createPool}} from {};import * as p from {};\n",
        json_string(&runtime_url("runtime.js")), json_string(&runtime_url("workers.js")), json_string(&runtime_url("prims.js"))) + r#"
import assert from 'node:assert/strict';
const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);
const textCache=new Map();
const textBytes=a=>a.map(s=>{if(!textCache.has(s))textCache.set(s,Buffer.from(p.textBytes(s)).toString('hex'));return textCache.get(s)});
const snapshot=v=>v instanceof p.PArray||v instanceof p.TextArray?snapshot(v.data):ArrayBuffer.isView(v)?bytes(v).toString('hex'):v instanceof Map?[...v].map(([n,x])=>[n,snapshot(x)]):Array.isArray(v)?v.map(snapshot):typeof v==='bigint'?v.toString():v;
const pools=[null,await createPool({size:1}),await createPool({size:8})];try{
"#
}

fn append_case(script: &mut String, name: &str, v: &Value) {
    append_case_frames(script, name, v, &[(0, 0.25), (13, 0.25), (32769, 0.25), (13, -0.25)], None);
}

fn append_case_frames(script: &mut String, name: &str, v: &Value, frames: &[(i64, f64)], counted_capture: Option<i64>) {
    let Value::Residual(r) = v else { panic!("live residual expected") };
    let core = core_lower::lower(None, None, true, None, None, *r).unwrap();
    if name.starts_with("closure-") || name == "fixed-count-changing-source" {
        assert!(core.regions.iter().all(|r| packed::host::argument_types(r).unwrap().is_empty()));
    }
    fn table(i: &Instruction) -> bool {
        match i { Instruction::Array_prepare(..) => true, Instruction::Guarded(_, inner) => table(inner), _ => false }
    }
    let regions = core.regions.iter().map(|r| {
        if r.code.iter().any(table) {
            assert!(!packed::gpu_refusals(false, false, r).is_empty());
            assert!(wgsl::kernel(false, r).is_err());
        }
        format!("{{source:{}}}", json_string(&js::region(false, r).unwrap()))
    }).collect::<Vec<_>>().join(",");
    script.push_str(&format!(r#"{{const {{frame}}=await import('data:text/javascript;base64,'+Buffer.from({}).toString('base64'));
const program={{regions:[{regions}]}};for(const pool of pools){{const rt=createRuntime(program,{{pool}});const invoke=rt.invoke;
rt.invoke=async function(k,args,plan){{assert.ok(plan,'Composition producer has no real source-preparation plan');const before=JSON.stringify(snapshot(args));try{{return await invoke.call(this,k,args,plan)}}finally{{assert.equal(JSON.stringify(snapshot(args)),before,'composition changed inputs')}}}};
"#, json_string(&js::frame(&js::FrameOptions::default(), &core).unwrap())));
    for &(count, time) in frames {
        let mut live = input::at_time(time);
        live.frame = count;
        live.tick = count;
        script.push_str(&format!("{{const input={{t:{time},dt:0,frame:{count},tick:{count},size:[1,1]}};rt.current=input;\n"));
        // The captured table has nine elements; concatenation adds exactly frame/index.
        // Avoid the native oracle rebuilding that frame-sized array for every output row.
        let expected = match counted_capture {
            Some(length) => Ok(Value::Int_array(Arc::new(vec![9 + count; length as usize]))),
            None => e::Host::force_reference(v, &live, None, None, None),
        };
        match expected {
            Ok(expected) => {
                let text = matches!(expected, Value::Text_array(_));
                script.push_str(&format!("const got=await frame(input,rt.slots,rt),expected={};assert.deepEqual({},{},{});\n", data(&expected),
                    if text { "textBytes(got.data)" } else { "bytes(got.data)" },
                    if text { "textBytes(expected)" } else { "bytes(expected)" }, json_string(&format!("{name}/{count}/{time} runtime bytes"))));
            }
            Err(d) => {
                let cause = d.cause.as_deref().map(text_literal::quote).unwrap_or_else(|| "undefined".into());
                script.push_str(&format!("await assert.rejects(()=>frame(input,rt.slots,rt),e=>{{assert.equal(e.code,{});assert.equal(e.message,{});assert.equal(e.cause,{});return true}});\n",
                    json_string(&d.code), json_string(&d.message), cause));
            }
        }
        script.push_str("}\n");
    }
    script.push_str("}}\n");
}

fn finish(mut script: String) {
    script.push_str("}finally{for(const pool of pools)pool?.close()}console.log('composition passed');\n");
    assert_eq!(run_ok(&script).trim(), "composition passed");
}

fn composition(ty: &str, operation: &str) {
    let all_bytes = flow_syntax::lisp::quote_string(&(0..=255).collect::<Vec<u8>>());
    let element = match ty {
        "int" => "(int/add i 9007199254740993)".into(),
        "float" => "(if (int/eq i 0) -0. (float i))".into(),
        "vec2" => "[(float i) -0.]".into(),
        "vec3" => "[(float i) -0. t]".into(),
        "vec4" => "[(float i) -0. t 0.125]".into(),
        "text" => format!("(if (int/eq (int/mod i 2) 0) {all_bytes} \"é日本😀\")"),
        _ => unreachable!(),
    };
    let gather = |table| format!("(map (fn [(i : int)] (array/nth {table} (int/mod i 13))) (array/range (frame/index) :type int))");
    let (count, body) = match operation {
        "concat" => ("(frame/index)", "(array/concat table table)".into()),
        "slice" => ("(frame/index)", "(array/slice table (int/div (frame/index) 2) (int/sub (frame/index) (int/div (frame/index) 2)))".into()),
        "hoisted" => ("13", format!("(let* [joined (array/concat table table)] {})", gather("joined"))),
        "inline-concat" => ("13", gather("(array/concat table table)")),
        "inline-slice" => ("13", gather("(array/slice table 0 13)")),
        _ => unreachable!(),
    };
    let (_keep, v) = value(&format!("(let* [table (map (fn [(i : int)] {element}) (array/range {count} :type int))] {body})"));
    let mut script = prelude();
    append_case(&mut script, &format!("{ty}/{operation}"), &v);
    finish(script);
}

macro_rules! storage {
    ($module:ident, $ty:literal) => { mod $module {
        #[test] fn concat() { super::composition($ty, "concat"); }
        #[test] fn slice() { super::composition($ty, "slice"); }
        #[test] fn hoisted() { super::composition($ty, "hoisted"); }
        #[test] fn inline_concat() { super::composition($ty, "inline-concat"); }
        #[test] fn inline_slice() { super::composition($ty, "inline-slice"); }
    }};
}
storage!(int, "int"); storage!(float, "float"); storage!(vec2, "vec2");
storage!(vec3, "vec3"); storage!(vec4, "vec4"); storage!(text, "text");

macro_rules! cases {
    ($($name:ident: $body:literal),* $(,)?) => { $( #[test] fn $name() {
        let (_keep, v) = value($body);
        let mut script = prelude();
        append_case(&mut script, stringify!($name), &v);
        finish(script);
    })* };
}
cases! {
    late_index: "(map (fn [(i : int)] (array/nth (array/concat (array/int 1) (array/int 1)) i)) (array/range (frame/index) :type int))",
    selected_slice: "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) 2 0) i)) (array/range (frame/index) :type int))",
    untaken_slice: "(map (fn [(i : int)] (if (int/lt i 0) (array/nth (array/slice (array/int 1) 2 0) i) i)) (array/range (frame/index) :type int))",
    table_cause: "(map (fn [(i : int)] (array/nth (array/concat (if (> t 0.) (error \"E_COMPOSE\" \"selected table\" :cause \"raw\\255\\000cause\") (array/int 1)) (array/int 1)) i)) (array/range (frame/index) :type int))",
    argument_before_copy: "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) (error \"E_FIRST\" \"slice first\" :cause \"first cause\") (error \"E_COUNT\" \"slice count\")) i)) (array/range (frame/index) :type int))",
    empty_product: "(for [i (array/range (frame/index) :type int) j (array/range -1 :type int)] (array/nth (array/slice (array/int 0) 1 0) i))",
    empty_zip: "(map (fn [(i : int) (j : int)] (array/nth (array/slice (array/int 0) 1 0) i)) (array/range (frame/index) :type int) (array/range -1 :type int))",
    earliest_row: "(map (fn [(i : int)] (if (int/eq i 0) (error \"E_ROW_ZERO\" \"first row\" :cause \"zero cause\") (array/nth (array/slice (array/int 1) 2 0) i))) (array/range (frame/index) :type int))",
}

#[test]
fn fixed_count_changing_source() {
    let (_keep, v) = value("(map (fn [x] x) (array/float 13 t))");
    let mut script = prelude();
    append_case(&mut script, "fixed-count-changing-source", &v);
    finish(script);
}

fn callable(ev: &e::Eval) -> FnId {
    let records = &ev.records.iter().find(|(p, _)| p == &["g".to_owned(), "tested".to_owned()]).unwrap().1;
    let Value::Fn(f) = records[0].1 else { panic!("function expected") };
    f
}

fn closure(body: &str, name: &str, counts: &[i64], frames: &[(i64, f64)], analytic_count: bool) {
    // Keep all three arenas alive: compiled helpers capture functions from distinct evaluations.
    let helper_ev = evaluation(&format!("(let* [data (array/int 9 9) tested (fn [(j : int)] {body})] 0.)"), true);
    let template_ev = evaluation("(let* [data (array/int 1 7) tested (fn [(helper : (fn [int] int)) (i : int)] (if (int/lt i 0) (array/nth data 0) (helper (int/mod i 2))))] 0.)", true);
    let (_root_ev, root) = value("t");
    let Value::Residual(root) = root else { panic!("residual expected") };
    let template = callable(&template_ev);
    let (params, body) = e::Residual::function_body(template).unwrap();
    let mut bindings = vec![("helper".into(), Value::Fn(callable(&helper_ev)))];
    bindings.extend(e::Residual::function_bindings(template));
    let helper = e::Residual::compiled_function(root, e::Residual::residual_view(root).unwrap().instance, vec![], vec!["g".into(), "tested".into()], params[1..].to_vec(), body, bindings).unwrap();
    let mut script = prelude();
    for &count in counts {
        let v = e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int], result: Ty::Int }, helper,
            &[Value::Int_array(Arc::new((0..count).collect()))]).unwrap();
        append_case_frames(&mut script, &format!("{name}/{count}"), &v, frames, analytic_count.then_some(count));
    }
    finish(script);
}

macro_rules! closures {
    ($module:ident, $body:literal, $name:literal, $analytic_count:literal) => { mod $module {
        #[test] fn small_captures() {
            super::closure($body, $name, &[0, 13], &[(0, 0.25), (13, 0.25), (32769, 0.25), (13, -0.25)], $analytic_count);
        }
        #[test] fn large_capture_frame_zero() { super::closure($body, $name, &[32769], &[(0, 0.25)], $analytic_count); }
        #[test] fn large_capture_frame_thirteen() { super::closure($body, $name, &[32769], &[(13, 0.25)], $analytic_count); }
        #[test] fn large_capture_frame_large() { super::closure($body, $name, &[32769], &[(32769, 0.25)], $analytic_count); }
        #[test] fn large_capture_negative_time() { super::closure($body, $name, &[32769], &[(13, -0.25)], $analytic_count); }
    }};
}
closures!(closure_concat, "(array/nth (array/concat (if (> t 0.) data (array/int 9 11)) data) j)", "closure-concat", false);
closures!(closure_array_if, "(array/nth (if (> t 0.) data (array/int 1 11)) 0)", "closure-array-if", false);
closures!(closure_count, "(array/count (array/concat data (array/int (frame/index))))", "closure-count", true);

#[test]
fn primitive_copy_and_malformed_boundaries() {
    let script = format!("import * as p from {};import assert from 'node:assert/strict';\n", json_string(&runtime_url("prims.js"))) + r#"
const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength).toString('hex');
const snapshot=a=>ArrayBuffer.isView(a)?bytes(a):a.map(s=>Buffer.from(p.textBytes(s)).toString('hex'));
for(const original of [new p.PArray(1,BigInt64Array.of(9007199254740993n,-(1n<<62n),(1n<<62n)-1n)),new p.PArray(1,Float64Array.of(-0,Number.MIN_VALUE,1)),new p.PArray(2,Float64Array.of(1,-0,2,3)),new p.PArray(3,Float64Array.of(1,-0,2,3,4,5)),new p.PArray(4,Float64Array.of(1,-0,2,3,4,5,6,7)),new p.TextArray([String.fromCharCode(0xdcff),'é日本😀','\0'])]) {
  const before=snapshot(original.data),width=original.width||1;
  const sliced=p.array_slice(original,0n,1n),joined=p.array_concat(original,original);
  assert.notEqual(sliced.data,original.data);assert.notEqual(joined.data,original.data);
  assert.deepEqual(snapshot(sliced.data),snapshot(original.data.slice(0,width)));
  assert.deepEqual(snapshot(joined.data),snapshot(ArrayBuffer.isView(original.data)?original.data.constructor.from([...original.data,...original.data]):[...original.data,...original.data]));
  sliced.data[0]=typeof sliced.data[0]==='bigint'?7n:typeof sliced.data[0]==='number'?7:'changed';joined.data[0]=sliced.data[0];
  assert.deepEqual(snapshot(original.data),before);
  assert.equal(p.array_slice(original,BigInt(original.data.length/width),0n).data.length,0);
}
for(const [f,code] of [[()=>p.array_slice(new p.PArray(1,BigInt64Array.of(1n)),-1n,0n),'E_ARRAY_RANGE'],[()=>p.array_slice(new p.PArray(1,BigInt64Array.of(1n)),2n,0n),'E_ARRAY_RANGE'],[()=>p.array_concat(new p.PArray(1,BigInt64Array.of(1n)),new p.PArray(1,Float64Array.of(1))),'E_ARRAY_TYPE']])assert.throws(f,e=>e.code===code);
console.log('composition primitives passed');
"#;
    assert_eq!(run_ok(&script).trim(), "composition primitives passed");
}
