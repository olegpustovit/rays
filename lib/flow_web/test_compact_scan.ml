module E=Flow.Eval
module I=Flow_ir
module P=I.Packed
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let packed=function Ok v->v|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let quote=Flow_web.Kernel_table.json_string
let number x=Param.float_text x
let numeric=function
  |E.Int_array a->"BigInt64Array.from(["^String.concat ","(Array.to_list(Array.map(fun i->string_of_int i^"n")a))^"])"
  |E.Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->
      "Float64Array.from(["^String.concat ","(Array.to_list(Array.map number a))^"])"
  |_->failwith "Numeric compact_scan data expected"
let root body=
  let text="(workspace compact_scan (graph g :context host (if (> t -1000.) "^body^" "^body^")))" in
  let ws=match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
    |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
  let value=List.assoc "g"(ok(E.static ws)).results in
  let r=match value with E.Residual r->r|_->failwith "Live compact_scan expected"in
  let program=match I.Core_lower.lower ~strict:true r with Ok p->p|Error form->failwith form in
  value,program
let ()=
  let js=Buffer.create 1000000 in
  let path name="file://"^Filename.concat(Sys.getcwd())("../web_export/runtime/"^name)in
  Printf.bprintf js "import {createRuntime} from %s;import {createPool} from %s;import * as p from %s;const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);const pools=[null,await createPool({size:1}),await createPool({size:8})];try{\n"
    (quote(path "runtime.js"))(quote(path "workers.js"))(quote(path "prims.js"));
  let live={(Input.at_time 0.25)with frame=3} in
  List.iter(fun body->let value,program=root body in
    let result=E.Private.force_reference value ~live in
    let prepared=Array.map(fun region->let packed=region in
      assert(P.gpu_refusals packed<>[] && Result.is_error(Flow_web.Wgsl.kernel packed));
      packed,(if P.Private.argument_types packed=[||] then Some(ok(P.Private.prepare_values packed ~live)) else None))program.regions in
    let frame=ok(Flow_web.Js.frame program)in
    Printf.bprintf js "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));const arrays=[%s];\n"
      (quote frame)(String.concat ","(Array.to_list(Array.map(fun(_,v)->
        match v with None->"[]" |Some v->"["^String.concat ","(Array.to_list(Array.map numeric v.P.Private.arrays))^"]")prepared)));
    (* The runtime's actual worker transport uses private copies of inputs. *)
    Buffer.add_string js "const data=new ArrayBuffer(arrays.flat().reduce((n,a)=>n+a.byteLength,0));let offset=0;const store=a=>{const spec=[offset,a.length,a instanceof BigInt64Array];new a.constructor(data,offset,a.length).set(a);offset+=a.byteLength;return spec};const inputs=arrays.map(xs=>xs.map(store));const inputBytes=bytes(new Uint8Array(data)).toString('hex');\n";
    let entries=Array.to_list(Array.mapi(fun i(p,v)->match v with
      |None->"{source:"^quote(Flow_web.Js.region p)^"}"
      |Some v->let r=p in
      let rec facts slot=function
        |I.Core.Region.Frame name->[Printf.sprintf "[%d,%s,false]"slot(quote name)]
        |Int_frame name->[Printf.sprintf "[%d,%s,true]"slot(quote name)]
        |Guarded(_,inner)->facts slot inner|_->[]in
      let facts=Array.to_list(Array.mapi facts r.code)|>List.concat in
      let uniforms=Array.to_list(Array.map(fun v->match v with
        |E.Int i->"["^quote(string_of_int i)^"]"
        |E.Float x->"["^number x^"]"
        |E.Bool b->if b then "[1]"else "[0]"
        |E.Vec2(x,y)->"["^number x^","^number y^"]"
        |E.Vec3(x,y,z)->"["^number x^","^number y^","^number z^"]"
        |E.Vec4(x,y,z,w)->"["^number x^","^number y^","^number z^","^number w^"]"
        |E.Int_array a->"["^String.concat ","(Array.to_list(Array.map(fun i->quote(string_of_int i))a))^"]"
        |v->"Array.from("^numeric v^")")v.P.Private.uniforms)in
      Printf.sprintf "{source:%s,count:%d,kept:%d,inputs:inputs[%d],uniforms:[%s],frame:[%s],length:%d,width:%d,integer:%b,skip:[]}"
        (quote(Flow_web.Js.region p))v.count v.count i(String.concat ","uniforms)(String.concat ","facts)
        (Array.length r.code)(Array.length r.outputs.(0))(r.term.ty=Flow.Ty.Array Flow.Ty.Int))prepared)in
    Printf.bprintf js "const program={regions:[%s]};for(const pool of pools){const rt=createRuntime(program,{data,pool});const input={t:.25,dt:0,frame:3,tick:3,size:[1,1]};rt.current=input;\n"(String.concat ","entries);
    (match result with
     |Ok expected->Printf.bprintf js "const got=await frame(input,rt.slots,rt);const expected=%s;if(!bytes(got.data).equals(bytes(expected)))throw Error('compact_scan frame bytes');\n"(numeric expected)
     |Error d->Printf.bprintf js "try{await frame(input,rt.slots,rt);throw Error('compact_scan accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
         (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
    Buffer.add_string js "if(bytes(new Uint8Array(data)).toString('hex')!==inputBytes)throw Error('compact_scan changed source');}\n}\n")
["(array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range 32769 :type int)))";
     "(array/scan_sum (map (fn [(i : int)] (int/add i (frame/index))) (array/range 32769 :type int)))";
     "(array/scan_sum (array/int 13 9007199254740993))";
     "(array/scan_sum (array/int 3 4611686018427387903))";
     "(array/scan_sum (array/int 0))";
     "(array/compact (array/int 0))";
     "(array/compact (array/int 32769 1))";
     "(if (> t 0.) (array/int 0) (array/compact (array/int 1 -1)))";
     "(array/compact (array/int 32769 2))";
     "(let* [kept (array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range 32769 :type int))) table (array/range 32769)] (map (fn [(i : int)] (array/nth table i)) kept))";
     "(map (fn [(i : int)] (int/add i (frame/index))) (array/scan_sum (array/int 5001 1)))"];
  Buffer.add_string js "}finally{for(const pool of pools)pool?.close()}console.log('compact_scan actual emitted frames/runtime: ordered bytes, errors, source immutability and worker input maps');\n";
  let path=Filename.temp_file "compact_scan-runtime" ".mjs"in
  Fun.protect ~finally:(fun()->Sys.remove path)(fun()->
    Out_channel.with_open_bin path(fun out->output_string out(Buffer.contents js));
    assert(Sys.command("node "^Filename.quote path)=0))
