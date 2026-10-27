module C=Array_composition_cases
module E=Flow.Eval
module I=Flow_ir
let quote=Flow_web.Kernel_table.json_string
let number value=Param.float_text value
let data=function
 |E.Int_array a->"BigInt64Array.from(["^String.concat ","(Array.to_list(Array.map(fun i->string_of_int i^"n")a))^"])"
 |Text_array a->"["^String.concat ","(Array.to_list(Array.map quote a))^"]"
 |Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->"Float64Array.from(["^String.concat ","(Array.to_list(Array.map number a))^"])"
 |_->failwith "Composition array expected"
let ()=
 let code=Buffer.create 1000000 in
 let path name="file://"^Filename.concat(Sys.getcwd())("../web_export/runtime/"^name)in
 Printf.bprintf code "import {createRuntime} from %s;import {createPool} from %s;import * as p from %s;const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);const textBytes=a=>a.map(s=>Buffer.from(p.textBytes(s)).toString('hex'));const snapshot=v=>v instanceof p.PArray||v instanceof p.TextArray?snapshot(v.data):ArrayBuffer.isView(v)?bytes(v).toString('hex'):v instanceof Map?[...v].map(([n,x])=>[n,snapshot(x)]):Array.isArray(v)?v.map(snapshot):typeof v==='bigint'?v.toString():v;const pools=[null,await createPool({size:1}),await createPool({size:8})];try{\n"
  (quote(path "runtime.js"))(quote(path "workers.js"))(quote(path "prims.js"));
 (* Argument-free kernels with changing counts/captures also require actual
    per-frame plans. No oracle arrays or synthetic arguments feed the runtime. *)
 let cases=List.map(fun(name,body)->name,C.value body)
   (C.cases@C.error_cases@["fixed-count-changing-source","(map (fn [x] x) (array/float 13 t))"])in
 let captures=List.concat_map(fun(name,body)->List.map(fun count->
   name^"/"^string_of_int count,C.helper_value body count)[0;13;32769])
   ["closure-concat","(array/nth (array/concat (if (> t 0.) data (array/int 9 11)) data) j)";
    "closure-array-if","(array/nth (if (> t 0.) data (array/int 1 11)) 0)";
    "closure-count","(array/count (array/concat data (array/int (frame/index))))"]in
 List.iter(fun(name,value)->
  let _,core=C.core value in
  if String.starts_with ~prefix:"closure-"name || name="fixed-count-changing-source" then
    assert(Array.for_all(fun r->I.Packed.Private.argument_types r=[||])core.regions);
  let frame=C.ok(Flow_web.Js.frame core)in
  let regions=Array.to_list(Array.map(fun r->let packed=r in
    let rec table=function I.Core.Region.Array_prepare _->true|Guarded(_,inner)->table inner|_->false in
    if Array.exists table r.I.Core.Region.code then begin
      assert(I.Packed.gpu_refusals packed<>[]);
      assert(Result.is_error(Flow_web.Wgsl.kernel packed))
    end;
    "{source:"^quote(Flow_web.Js.region packed)^"}")core.regions)in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));const program={regions:[%s]};for(const pool of pools){const rt=createRuntime(program,{pool});const invoke=rt.invoke;rt.invoke=async function(k,args,plan){if(!plan)throw Error('Composition producer has no real source-preparation plan');const before=JSON.stringify(snapshot(args));try{return await invoke.call(this,k,args,plan)}finally{if(JSON.stringify(snapshot(args))!==before)throw Error('composition changed inputs')}};\n"
    (quote frame)(String.concat ","regions);
  List.iter(fun(count,time)->let live={(Input.at_time time)with frame=count;tick=count}in
   Printf.bprintf code "{const input={t:%s,dt:0,frame:%d,tick:%d,size:[1,1]};rt.current=input;\n" (number time)count count;
   (match E.Private.force_reference value ~live with
    |Ok expected->Printf.bprintf code "const got=await frame(input,rt.slots,rt),expected=%s;if(%b?JSON.stringify(textBytes(got.data))!==JSON.stringify(textBytes(expected)):!bytes(got.data).equals(bytes(expected)))throw Error(%s);\n"
       (data expected)(match expected with E.Text_array _->true|_->false)(quote(name^" runtime bytes"))
    |Error d->Printf.bprintf code "try{await frame(input,rt.slots,rt);throw Error('composition accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
       (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
   Buffer.add_string code "}\n") [0,0.25;13,0.25;32769,0.25;13,-0.25];
  Buffer.add_string code "}}\n") (cases@captures);
 (* Use the actual exported primitives for direct malformed/mixed storage and
    copying boundaries too; frame references are never substituted as inputs. *)
 Buffer.add_string code "for(const original of [new p.PArray(1,BigInt64Array.of(9007199254740993n,-(1n<<62n),(1n<<62n)-1n)),new p.PArray(1,Float64Array.of(-0,Number.MIN_VALUE,1)),new p.PArray(2,Float64Array.of(1,-0,2,3)),new p.PArray(3,Float64Array.of(1,-0,2,3,4,5)),new p.PArray(4,Float64Array.of(1,-0,2,3,4,5,6,7)),new p.TextArray([String.fromCharCode(0xdcff),'é日本😀','\\0'])]){const before=JSON.stringify(snapshot(original));const sliced=p.array_slice(original,0n,1n),joined=p.array_concat(original,original);if(sliced.data===original.data||joined.data===original.data)throw Error('composition borrowed mutable result');sliced.data[0]=typeof sliced.data[0]==='bigint'?7n:typeof sliced.data[0]==='number'?7:'changed';joined.data[0]=sliced.data[0];if(JSON.stringify(snapshot(original))!==before)throw Error('composition alias');p.array_slice(original,BigInt(original.data.length/(original.width||1)),0n)}\n";
 Buffer.add_string code "for(const [f,code] of [[()=>p.array_slice(new p.PArray(1,BigInt64Array.of(1n)),-1n,0n),'E_ARRAY_RANGE'],[()=>p.array_slice(new p.PArray(1,BigInt64Array.of(1n)),2n,0n),'E_ARRAY_RANGE'],[()=>p.array_concat(new p.PArray(1,BigInt64Array.of(1n)),new p.PArray(1,Float64Array.of(1))),'E_ARRAY_TYPE']]){try{f();throw Error('invalid composition accepted')}catch(e){if(e.code!==code)throw e}}\n";
 Buffer.add_string code "}finally{for(const pool of pools)pool?.close()}console.log('C6 actual emitted Core+region runtime: six arrays, fresh bytes, selected errors and workers1/8');\n";
 let file=Filename.temp_file "array-composition-runtime" ".mjs"in
 Fun.protect ~finally:(fun()->Sys.remove file)(fun()->
  Out_channel.with_open_bin file(fun out->output_string out(Buffer.contents code));
  assert(Sys.command("node "^Filename.quote file)=0))
