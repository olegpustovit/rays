//! Actual generated FMA/multiply regions compare binary64 bits against native fused results, scalar primitives, malformed nonfinite statuses and GPU refusals.
use crate::js;
use crate::node_harness::{node_available, run_ok, runtime_url};
use crate::test_program::die;
use crate::wgsl;
use flow_check::catalog::Catalog;
use flow_check::workspace as W;
use flow_eval as E;
use flow_ir::core_ir::region::{self, Instruction, Projection};
use flow_ir::core_lower;
use flow_ir::packed::{self, host};
use flow_syntax::syntax;
use flow_syntax::value::Value;
use std::sync::Arc;

fn bits(x: f64) -> String { format!("{:016x}", x.to_bits()) }

fn array(xs: &[f64]) -> String { format!("Float64Array.from([{}])", xs.iter().map(|x| param::float_text(*x)).collect::<Vec<_>>().join(",")) }

fn host_value(body: &str, data: Vec<f64>) -> Value {
    let source = format!("(workspace w (graph g :context host [(data : (array vec3) (array/vec3 0))] {body}))");
    let forms = ((syntax::parse(&source))).unwrap_or_else(|d| die(d));
    let ws = match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join(";")),
    };
    let inputs = vec![("g".to_owned(), vec![("data".into(), Value::Floats(3, Arc::new(data)))])];
    let evaluation = ((E::static_with(&ws, false, Some(&inputs), None))).unwrap_or_else(|d| die(d));
    evaluation.results.iter().find(|(n, _)| n == "g").expect("g").1.clone()
}

fn live() -> input::Frame {
    let mut live = input::at_time(1.0);
    live.frame = 3;
    live
}

fn compiled(value: &Value) -> packed::T {
    let Value::Residual(r) = value else { panic!("not live") };
    let view = E::residual::residual_view(*r).unwrap();
    match packed::compile_result(&Default::default(), *r, &view.term) {
        Ok(p) => p,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join(";")),
    }
}

fn flatten(v: &Value) -> Vec<f64> {
    match v {
        Value::Float(x) => vec![*x],
        Value::Int(n) => vec![*n as f64],
        Value::Bool(b) => vec![if *b { 1.0 } else { 0.0 }],
        Value::Vec3(a, b, c) => vec![*a, *b, *c],
        _ => panic!("uniform"),
    }
}

const BITS_HELPER: &str = "const bytes=new DataView(new ArrayBuffer(8)); const bits=x=>{bytes.setFloat64(0,x,false);return bytes.getBigUint64(0,false).toString(16).padStart(16,'0')};\n";

fn expected_array(values: &[f64]) -> String { format!("const expected=[{}];\n", values.iter().map(|x| format!("\"{}\"", bits(*x))).collect::<Vec<_>>().join(",")) }

#[test]
fn explicit_fma_regions_match_exact_binary64() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let minsub = f64::from_bits(1);
    let (a, b) = (1.0000000000000002f64, 0.9999999999999998f64);
    assert!(a.mul_add(b, -1.0) != std::hint::black_box(a * b) - 1.0);
    let mut cases: Vec<(f64, f64, f64)> = vec![
        (1.0000000000000002, 0.9999999999999998, -1.0),
        (0.0, -1.0, 0.0),
        (0.0, -1.0, -0.0),
        (-0.0, 1.0, -0.0),
        (minsub, 0.5, 0.0),
        (minsub, 0.5, minsub),
        (-minsub, 0.5, 0.0),
        (2f64.powi(-537), 2f64.powi(-537), -minsub),
        (f64::MAX, 2.0, -f64::MAX),
        (0.37, 0.83, 0.19),
    ];
    let mut rng = rays_math::rand::seed(0xf4a);
    let mut next = |bound: i64| {
        let (n, g) = rays_math::rand::int(bound, rng).expect("bound");
        rng = g;
        n
    };
    let max_int = (1i64 << 62) - 1;
    let mut sample = || {
        let mut raw = next(max_int);
        if next(2) != 0 {
            raw |= 0x4000000000000000;
        }
        if next(2) != 0 {
            raw |= i64::MIN;
        }
        f64::from_bits((raw as u64) & 0xffef_ffff_ffff_ffff)
    };
    // 10010 sampled cases in RAYS_SLOW=1; the default loop samples 2010.
    let target = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 10010 } else { 2010 };
    while cases.len() < target {
        let (a, b, c) = (sample(), sample(), sample());
        if a.mul_add(b, c).is_finite() {
            cases.push((a, b, c));
        }
    }
    let data: Vec<f64> = cases.iter().flat_map(|(a, b, c)| [*a, *b, *c]).collect();
    let expected: Vec<f64> = cases.iter().map(|(a, b, c)| a.mul_add(*b, *c)).collect();
    let value = host_value("(map (fn [(p : vec3)] (if (> t 0.) (fma p.x p.y p.z) 0.)) data)", data.clone());
    let packed = compiled(&value);
    assert!(!packed::gpu_refusals(false, false, &packed).is_empty());
    let prepared = ((host::prepare_values(&packed, &host::PrepareOptions::default(), &live()))).unwrap_or_else(|d| die(d));
    let mut script = format!("import {{fma}} from \"{}\";\n", runtime_url("prims.js"));
    script.push_str(&((js::region(false, &packed))).unwrap_or_else(|d| die(d)));
    script.push_str(&format!("const input={}; const output=new Float64Array({});\n", array(&data), cases.len()));
    let mut uniforms: Vec<String> = prepared.uniforms.iter().map(|v| array(&flatten(v))).collect();
    uniforms.push(array(&prepared.frame.iter().map(|v| flatten(v)[0]).collect::<Vec<_>>()));
    script.push_str(&format!("if(region([input],[{}],[output],0,{})!==0)throw Error('region refusal');\n", uniforms.join(","), cases.len()));
    script.push_str(BITS_HELPER);
    script.push_str(&expected_array(&expected));
    script.push_str("for(let i=0;i<output.length;i++){for(const x of [output[i],fma(input[3*i],input[3*i+1],input[3*i+2])])if(bits(x)!==expected[i])throw Error('FMA bits '+i+' '+bits(x)+' != '+expected[i]);}\n");
    script.push_str("if(fma(1.0000000000000002,0.9999999999999998,-1)===1.0000000000000002*0.9999999999999998-1)throw Error('FMA lost single rounding');\n");
    script.push_str("for(const args of [[Infinity,0,1],[1,NaN,0],[1,1,-Infinity],[Number.MAX_VALUE,2,0]]){try{fma(...args);throw Error('accepted')}catch(e){if(e.code!=='E_NONFINITE')throw e}}\n");
    // a malformed region with a nonfinite FMA input reports the status, not a value
    let bad = region::rebuild(
        &packed,
        region::Rebuild {
            code: Some(vec![Instruction::Const(f64::INFINITY), Instruction::Const(0.0), Instruction::Const(1.0), Instruction::Fma(0, 1, 2)]),
            outputs: Some(vec![vec![3]]),
            ..Default::default()
        },
    )
    .expect("malformed region");
    assert!(packed::force(&bad, &live(), &packed::ForceOptions { strict: true, ..Default::default() }).is_err());
    script.push_str("{\n");
    script.push_str(&((js::region(false, &bad))).unwrap_or_else(|d| die(d)));
    script.push_str("if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==1)throw Error('nonfinite FMA input accepted');\n}\n");
    run_ok(&script);
}

#[test]
fn checked_arithmetic_regions_match_exact_binary64() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let cases: [(f64, f64, f64); 9] = [
        (f64::MAX, 2.0, 0.0),
        (f64::MAX, 2.0, -f64::MAX),
        (f64::MAX, 2.0, f64::MAX),
        (1.0 + 2f64.powi(-27), 1.0 - 2f64.powi(-27), -1.0),
        (-0.0, 1.0, -0.0),
        (0.0, -1.0, 0.0),
        (f64::from_bits(1), 0.5, 0.0),
        (-f64::from_bits(1), 0.5, -0.0),
        (0.37, 0.83, 0.19),
    ];
    // 32769 elements in RAYS_SLOW=1; the default 4097 still crosses the 4096-element chunk.
    let count = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 32769usize } else { 4097 };
    let data: Vec<f64> = (0..count * 3)
        .map(|i| {
            let (a, b, c) = cases[i / 3 % cases.len()];
            [a, b, c][i % 3]
        })
        .collect();
    let safe = |x: f64| if x.is_finite() { x } else { 0.0 };
    let valid = |x: f64| if x.is_finite() { 1.0 } else { 0.0 };
    let expected: Vec<f64> = (0..count * 4)
        .map(|i| {
            let (a, b, c) = cases[i / 4 % cases.len()];
            let product = std::hint::black_box(a * b);
            let fused = a.mul_add(b, c);
            match i % 4 {
                0 => safe(product),
                1 => valid(product),
                2 => safe(fused),
                _ => valid(fused),
            }
        })
        .collect();
    let body = "(map (fn [(p : vec3)] (let* [multiply (float/mul_checked p.x p.y) fused (float/fma_checked p.x p.y p.z)] [multiply.value (if multiply.valid 1.0 0.0) fused.value (if fused.valid 1.0 0.0)])) data)";
    // A live guard keeps the checked body as a residual while input bits stay exact.
    let source = host_value(&format!("(if (> t 0.) {body} {body})"), data.clone());
    let Value::Residual(residual) = &source else { panic!("not live") };
    let core = core_lower::lower(None, None, true, None, None, *residual).expect("checked main lowers");
    let view = E::residual::residual_view(*residual).unwrap();
    let packed = match packed::compile_result(&Default::default(), *residual, &view.term) {
        Ok(p) => p,
        // The outer structural if belongs to Core; inspect its actual map region.
        Err(_) => (*core.regions[0]).clone(),
    };
    assert!(!packed::gpu_refusals(false, false, &packed).is_empty());
    match wgsl::kernel(false, &packed) {
        Err(d) => assert_eq!(d.code, "E_GPU_FORM"),
        Ok(_) => panic!("checked arithmetic must be refused by the WGSL kernel"),
    }
    let prepared = ((host::prepare_values(&packed, &host::PrepareOptions::default(), &live()))).unwrap_or_else(|d| die(d));
    let mut script = format!("import {{float_mul_checked as mul,float_fma_checked as fma}} from \"{}\";\n", runtime_url("prims.js"));
    script.push_str(&((js::region(false, &packed))).unwrap_or_else(|d| die(d)));
    script.push_str(&format!("const input={}, out=new Float64Array({});\n", array(&data), expected.len()));
    let mut uniforms: Vec<String> = prepared.uniforms.iter().map(|v| array(&flatten(v))).collect();
    uniforms.push(array(&prepared.frame.iter().map(|v| flatten(v)[0]).collect::<Vec<_>>()));
    script.push_str(&format!("if(region([input],[{}],[out],0,{count})!==0)throw Error('checked region refused');\n", uniforms.join(",")));
    script.push_str(BITS_HELPER);
    script.push_str(&expected_array(&expected));
    script.push_str("for(let i=0;i<input.length/3;i++){const a=mul(input[3*i],input[3*i+1]),b=fma(input[3*i],input[3*i+1],input[3*i+2]);const scalar=[a.get('value'),+a.get('valid'),b.get('value'),+b.get('valid')];for(let j=0;j<4;j++){const k=4*i+j;if(bits(out[k])!==expected[k]||bits(scalar[j])!==expected[k])throw Error('checked bits '+k)}}\n");
    script.push_str("for(const f of [mul,(a,b)=>fma(a,b,0)])for(const bad of [Infinity,-Infinity,NaN]){try{f(bad,1);throw Error('accepted bad input')}catch(e){if(e.code!=='E_NONFINITE')throw e}}\n");
    for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        for instruction in [
            Instruction::CheckedMul(Projection::Value, 0, 1),
            Instruction::CheckedMul(Projection::Valid, 0, 1),
            Instruction::CheckedFma(Projection::Value, 0, 1, 2),
            Instruction::CheckedFma(Projection::Valid, 0, 1, 2),
        ] {
            let malformed = region::rebuild(
                &packed,
                region::Rebuild {
                    code: Some(vec![Instruction::Const(bad), Instruction::Const(1.0), Instruction::Const(0.0), instruction]),
                    outputs: Some(vec![vec![3]]),
                    ..Default::default()
                },
            )
            .expect("malformed region");
            match packed::force(&malformed, &live(), &packed::ForceOptions { strict: true, ..Default::default() }) {
                Err(d) => assert_eq!(d.code, "E_NONFINITE"),
                Ok(_) => panic!("Checked malformed input accepted"),
            }
            script.push_str("{\n");
            script.push_str(&((js::region(false, &malformed))).unwrap_or_else(|d| die(d)));
            script.push_str("if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==1)throw Error('checked nonfinite input accepted');\n}\n");
        }
    }
    let guarded = region::rebuild(
        &packed,
        region::Rebuild {
            code: Some(vec![
                Instruction::Const(0.0),
                Instruction::Const(f64::INFINITY),
                Instruction::Const(1.0),
                Instruction::Guarded(vec![(0, true)], Box::new(Instruction::CheckedMul(Projection::Value, 1, 2))),
            ]),
            outputs: Some(vec![vec![0]]),
            ..Default::default()
        },
    )
    .expect("guarded region");
    match ((packed::force(&guarded, &live(), &packed::ForceOptions { strict: true, ..Default::default() }))).unwrap_or_else(|d| die(d)) {
        Value::Floats(1, values) => assert!(values.iter().all(|v| *v == 0.0)),
        other => panic!("unexpected {other:?}"),
    }
    script.push_str("{\n");
    script.push_str(&((js::region(false, &guarded))).unwrap_or_else(|d| die(d)));
    script.push_str("if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==0)throw Error('untaken checked input refused');\n}\n");
    run_ok(&script);
}
