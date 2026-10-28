let () =
  let js = Buffer.create 20000 in
  Printf.bprintf js "import {textBytes,textOfBytes,text_repr} from %S;\n"
    ("file://" ^ Filename.concat (Sys.getcwd ()) "../web_export/runtime/prims.js");
  Buffer.add_string js "import {Worker} from 'node:worker_threads';\nconst samples=[];\n";
  let sample text =
    let bytes = Array.init (String.length text) (fun i -> string_of_int (Char.code text.[i])) in
    Printf.bprintf js "samples.push([%s,Uint8Array.from([%s]),%s]);\n"
      (Flow_web.Kernel_table.json_string text)
      (String.concat "," (Array.to_list bytes))
      (Flow_web.Kernel_table.json_string (Printf.sprintf "%S" text))
  in
  List.iter sample
    [
      "";
      "a\000b";
      "é日本😀";
      "\239\187\191BOM";
      "�";
      "\192\128";
      "\237\160\128";
      "\244\144\128\128";
      "\226\130";
      "validé\255\000\128日本\195";
      String.init 256 Char.chr;
    ];
  for byte = 128 to 255 do
    sample (String.make 1 (Char.chr byte))
  done;
  Buffer.add_string js
    {|
const equal=(a,b)=>Buffer.from(a).equals(Buffer.from(b));
for(const [text,bytes,quoted] of samples) {
  if(!equal(textBytes(text),bytes)||!equal(textBytes(textOfBytes(bytes)),bytes)
     ||text_repr(text)!==quoted)throw Error('native Text bytes/repr');
}
for(const count of [1,8]) {
  const workers=Array.from({length:count},()=>new Worker(
    "require('node:worker_threads').parentPort.on('message',m=>require('node:worker_threads').parentPort.postMessage(m))",{eval:true}));
  try {await Promise.all(workers.map(w=>new Promise((resolve,reject)=>{
    w.once('error',reject);w.once('message',texts=>{
      try {texts.forEach((text,i)=>{if(!equal(textBytes(text),samples[i][1]))throw Error('worker Text bytes')});resolve()}
      catch(e){reject(e)}});w.postMessage(samples.map(s=>s[0]));
  })))} finally {await Promise.all(workers.map(w=>w.terminate()))}
}
console.log('Text codec: all byte values, invalid UTF-8, NUL, Unicode, BOM, repr and worker round-trip pass');
|};
  let path = Filename.temp_file "text-bytes" ".mjs" in
  Out_channel.with_open_bin path (fun out -> output_string out (Buffer.contents js));
  let status = Sys.command ("node " ^ Filename.quote path) in
  Sys.remove path;
  assert (status = 0)
