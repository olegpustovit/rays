//! the Text codec of the web runtime (prims.js) round-trips
//! every byte value, invalid UTF-8, NUL, Unicode and BOM, and survives worker threads. Runs under
//! node; skipped when node is absent.
use crate::kernel_table::json_string;
use crate::node_harness::{node_available, run_ok, runtime_url};

#[test]
fn text_codec_round_trips_every_byte_value() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
    let op = flow_check::op::find(&[], "text/repr", &flow_syntax::context::value()).unwrap();
    let mut js = String::new();
    js.push_str(&format!("import {{textBytes,textOfBytes,text_repr}} from \"{}\";\n", runtime_url("prims.js")));
    js.push_str("import {Worker} from 'node:worker_threads';\nconst samples=[];\n");
    let mut sample = |text: &[u8]| {
        let bytes: Vec<String> = text.iter().map(|b| b.to_string()).collect();
        // json_string quotes a Rust string; invalid bytes go through the same reversible escapes
        js.push_str(&format!(
            "samples.push([{},Uint8Array.from([{}]),{}]);\n",
            crate::text_literal::quote(text),
            bytes.join(","),
            json_string(&match (op.body)(&input::at_time(0.0), &|_, _| unreachable!(), &[("text".into(), flow_syntax::value::Value::Text(text.into()))]).unwrap() {
                flow_syntax::value::Value::Text(value) => String::from_utf8(value.to_vec()).unwrap(),
                _ => unreachable!(),
            })
        ));
    };
    let all_bytes: Vec<u8> = (0..=255u8).collect();
    let samples: Vec<&[u8]> = vec![
        b"",
        b"a\0b",
        "é日本😀".as_bytes(),
        b"\xef\xbb\xbfBOM",
        "\u{fffd}".as_bytes(),
        b"\xc0\x80",
        b"\xed\xa0\x80",
        b"\xf4\x90\x80\x80",
        b"\xe2\x82",
        b"valid\xc3\xa9\xff\0\x80\xe6\x97\xa5\xe6\x9c\xac\xc3",
        &all_bytes,
    ];
    for s in samples {
        sample(s);
    }
    for byte in 128..=255u8 {
        sample(&[byte]);
    }
    js.push_str(
        "
const equal=(a,b)=>Buffer.from(a).equals(Buffer.from(b));
for(const [text,bytes,quoted] of samples) {
  if(!equal(textBytes(text),bytes)||!equal(textBytes(textOfBytes(bytes)),bytes)
     ||text_repr(text)!==quoted)throw Error('native Text bytes/repr');
}
for(const count of [1,8]) {
  const workers=Array.from({length:count},()=>new Worker(
    \"require('node:worker_threads').parentPort.on('message',m=>require('node:worker_threads').parentPort.postMessage(m))\",{eval:true}));
  try {await Promise.all(workers.map(w=>new Promise((resolve,reject)=>{
    w.once('error',reject);w.once('message',texts=>{
      try {texts.forEach((text,i)=>{if(!equal(textBytes(text),samples[i][1]))throw Error('worker Text bytes')});resolve()}
      catch(e){reject(e)}});w.postMessage(samples.map(s=>s[0]));
  })))} finally {await Promise.all(workers.map(w=>w.terminate()))}
}
console.log('Text codec: all byte values, invalid UTF-8, NUL, Unicode, BOM, repr and worker round-trip pass');
",
    );
    run_ok(&js);
}

#[test]
fn builtin_integer_boundaries_match_native_i64() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let js = format!("import * as p from {};\n", json_string(&runtime_url("prims.js")));
    run_ok(&(js + r#"
for (const [input, expected] of [
  [-4611686018427387904n, 4611686018427387904n],
  [-9223372036854775808n, -9223372036854775808n],
  [-9223372036854775807n, 9223372036854775807n],
]) if (p.abs(input) !== expected) throw Error('native i64 abs boundary');
for (const [text, expected] of [
  ['  +9223372036854775807  ', 9223372036854775807n], ['-9223372036854775808', -9223372036854775808n],
  ['4611686018427387904', 4611686018427387904n], ['-4611686018427387905', -4611686018427387905n],
  ['9223372036854775808', null], ['-9223372036854775809', null], ['1.0', null],
]) {
  const result = p.text_to_int(text);
  if (result.get('valid') !== (expected !== null) || result.get('value') !== (expected ?? 0n))
    throw Error('native i64 token boundary: ' + text);
}
console.log('native i64 builtin boundaries agree');
"#));
}

#[test]
fn native_shortest_display_matches_web_for_raw_float_bits() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let mut js = format!("import {{show}} from {};\nimport assert from 'node:assert/strict';\nconst data=new DataView(new ArrayBuffer(8));\n", json_string(&runtime_url("prims.js")));
    let edges = [0.0, -0.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY, f64::MAX, f64::MIN_POSITIVE, f64::from_bits(1), 0.03125, -0.03125, 0.00005, 17179869184.03125, 1e21, 1e-7, 0.30000000000000004, 1_234_567_890_123_456.25, -1_234_567_890_123_456.25];
    let boundaries = [1e-6f64, 1e21, 1e23, -1e23, 0.03125, 17179869184.03125, 1_234_567_890_123_456.25, -1_234_567_890_123_456.25].into_iter().flat_map(|value| [f64::from_bits(value.to_bits() - 1), value, f64::from_bits(value.to_bits() + 1)]);
    let samples = edges.into_iter().chain(boundaries).chain((0..256u64).map(|index| f64::from_bits(index.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1))));
    for value in samples {
        let expected = if value.is_nan() { "NaN".to_owned() } else if value.is_infinite() { if value.is_sign_negative() { "-Infinity" } else { "Infinity" }.to_owned() } else { value.to_string() };
        assert_eq!(String::from_utf8(flow_syntax::value::show_with(&Clone::clone, &flow_syntax::value::Value::Float(value))).unwrap(), expected);
        js.push_str(&format!("data.setBigUint64(0,{}n,true);assert.equal(show(data.getFloat64(0,true)),{});\n", value.to_bits(), json_string(&expected)));
    }
    run_ok(&js);
}

#[test]
fn unicode_metadata_names_keep_native_validation_and_byte_text_semantics() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let js = format!(r#"import * as p from {};
import assert from 'node:assert/strict';
for (const name of ['\u000b', '\u0085', '\u00a0', '\u2003', '\u2028', '\u3000']) {{
  assert.throws(() => p.sop_attr(null, name, 'point', 'float'), error => error.code === 'E_ATTR_NAME');
  assert.equal(p.text_blank(name), false);
  assert.equal(p.text_trim(name), name);
}}
"#, json_string(&runtime_url("prims.js")));
    run_ok(&js);
}

#[test]
fn fractional_nth_errors_match_native_shortest_numbers() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let op = flow_check::op::find(&[], "nth", &flow_syntax::context::value()).unwrap();
    let mut js = format!("import {{sequence_get}} from {};\nimport assert from 'node:assert/strict';\n", json_string(&runtime_url("prims.js")));
    for value in [1.23456789, -1.23456789, 0.000000123456789] {
        let failure = (op.body)(&input::at_time(0.0), &|_, _| unreachable!(), &[("index".into(), flow_syntax::value::Value::Float(value)), ("list".into(), flow_syntax::value::Value::List(std::sync::Arc::new(Vec::new())))]).unwrap_err();
        assert_eq!(failure.message, format!("nth index {value} is not a whole number."));
        js.push_str(&format!("assert.throws(()=>sequence_get([],{}),e=>e.code==='E_LIST_RANGE'&&e.message==={});\n", value, json_string(&failure.message)));
    }
    run_ok(&js);
}

#[test]
fn color_hex_tokens_match_the_native_digit_reader() {
    if !node_available() { eprintln!("node is absent: SKIPPED"); return; }
    let mut js = format!("import {{color_parse}} from {};\nimport assert from 'node:assert/strict';\n", json_string(&runtime_url("prims.js")));
    for text in ["f_0000", "00f_00", "0000f_", "#000000f_", "_f0000", "#é1", "#ggg"] {
        assert!(rays_math::color::hex(text).is_err());
        js.push_str(&format!("assert.throws(()=>color_parse({}),e=>e.code==='E_DRAW_COLOR');\n", json_string(text)));
    }
    for text in ["#f08", "#f08c", "00aAfF", "#00aAfF80"] {
        let color = rays_math::color::hex(text).unwrap();
        js.push_str(&format!("assert.deepEqual([...color_parse({}).values()],[{}n,{}n,{}n,{}n]);\n", json_string(text), color.r, color.g, color.b, color.a));
    }
    run_ok(&js);
}
