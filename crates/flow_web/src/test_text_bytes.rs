//! Port of lib/flow_web/test_text_bytes.ml: the Text codec of the web runtime (prims.js) round-trips
//! every byte value, invalid UTF-8, NUL, Unicode and BOM, and survives worker threads. Runs under
//! node; skipped when node is absent.
use crate::kernel_table::json_string;
use crate::node_harness::{node_available, run_ok, runtime_url};

/// OCaml's `%S` of a byte string (decimal escapes for non-printable bytes).
fn ocaml_quoted(bytes: &[u8]) -> String {
    let mut out = String::from("\"");
    for b in bytes {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(*b as char),
            b => out.push_str(&format!("\\{:03}", b)),
        }
    }
    out.push('"');
    out
}

#[test]
fn text_codec_round_trips_every_byte_value() {
    if !node_available() {
        eprintln!("node is absent: SKIPPED");
        return;
    }
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
            json_string(&ocaml_quoted(text))
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
