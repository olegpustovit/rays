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
  |E.Text_array a->"["^String.concat ","(Array.to_list(Array.map quote a))^"]"
  |_->failwith "Grade/lookup data expected"
let root body=
  let text="(workspace grade_index (graph g :context host (if (> t -1000.) "^body^" "^body^")))" in
  let ws=match Flow.Workspace.check Flow.Check.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
    |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
  let value=List.assoc "g"(ok(E.static ws)).results in
  let r=match value with E.Residual r->r|_->failwith "Live grade_index expected"in
  let program=match I.Core_lower.lower ~strict:true r with Ok p->p|Error form->failwith form in
  value,program
let literal text=fst(Flow.Lisp.print[Flow.Syntax.make(Flow.Syntax.Str text)])
let text_array texts=
  (* A single bounded list retains all256 byte keys within the existing AST budget.
     Its compiled text map uses structured Core; no numeric kernel or variadic concat. *)
  "(let* [texts (list "^String.concat " "(Array.to_list(Array.map literal texts))^") ] "^
  "(map (fn [(i : int)] (nth texts i)) (array/range "^string_of_int(Array.length texts)^" :type int)))"
let raw=[|"";"a";"a";"x\000y";"\255\128";"é日本😀";"\240\144\128\128";"\238\128\128";String.init 256 Char.chr|]
let all=Array.init 256(fun i->String.make 1(Char.chr(255-i)))
let ()=
  let js=Buffer.create 1000000 in
  let path name="file://"^Filename.concat(Sys.getcwd())("../web_export/runtime/"^name)in
  Printf.bprintf js "import {createRuntime} from %s;import {createPool} from %s;import * as p from %s;const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);const pools=[null,await createPool({size:1}),await createPool({size:8})];try{\n"
    (quote(path "runtime.js"))(quote(path "workers.js"))(quote(path "prims.js"));
  let live={(Input.at_time 0.25)with frame=3} in
  List.iter(fun body->let value,program=root body in
    let result=E.Private.force_reference value ~live in
    let prepared=Array.map(fun (region:I.Core.Region.t)->let packed=region in
      let ty=packed.term.ty in
      if ty=Flow.Ty.Array Flow.Ty.Int || ty=Flow.Ty.Array Flow.Ty.Text then
        assert(P.gpu_refusals packed<>[] && Result.is_error(Flow_web.Wgsl.kernel packed));
      packed,(if P.Private.argument_types packed=[||] then Some(ok(P.Private.prepare_values packed ~live)) else None))program.regions in
    let frame=ok(Flow_web.Js.frame program)in
    Printf.bprintf js "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));const arrays=[%s];\n"
      (quote frame)(String.concat ","(Array.to_list(Array.map(fun(_,v)->
        match v with None->"[]" |Some v->"["^String.concat ","(Array.to_list(Array.map numeric v.P.Private.arrays))^"]")prepared)));
    (* The runtime's actual worker transport uses private copies of inputs. *)
    Buffer.add_string js "for(const a of arrays.flat())if(Array.isArray(a))Object.freeze(a);const texts=arrays.flat().filter(Array.isArray).map(a=>JSON.stringify(a));const data=new ArrayBuffer(arrays.flat().reduce((n,a)=>n+(a.byteLength??0),0));let offset=0;const store=a=>{if(Array.isArray(a))return {texts:a};const spec=[offset,a.length,a instanceof BigInt64Array];new a.constructor(data,offset,a.length).set(a);offset+=a.byteLength;return spec};const inputs=arrays.map(xs=>xs.map(store));const inputBytes=bytes(new Uint8Array(data)).toString('hex');\n";
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
        |E.Text x->"{text:"^quote x^"}"
        |E.Text_array a->"{texts:"^numeric(E.Text_array a)^"}"
        |E.Int_array a->"["^String.concat ","(Array.to_list(Array.map(fun i->quote(string_of_int i))a))^"]"
        |v->"Array.from("^numeric v^")")v.P.Private.uniforms)in
      Printf.sprintf "{source:%s,count:%d,kept:%d,inputs:inputs[%d],uniforms:[%s],frame:[%s],length:%d,width:%d,integer:%b,text:%b,skip:[]}"
        (quote(Flow_web.Js.region p))v.count v.count i(String.concat ","uniforms)(String.concat ","facts)
        (Array.length r.code)(Array.length r.outputs.(0))(r.term.ty=Flow.Ty.Array Flow.Ty.Int)(r.term.ty=Flow.Ty.Array Flow.Ty.Text))prepared)in
    Printf.bprintf js "const program={regions:[%s]};for(const pool of pools){const rt=createRuntime(program,{data,pool});const input={t:.25,dt:0,frame:3,tick:3,size:[1,1]};rt.current=input;\n"(String.concat ","entries);
    (match result with
     |Ok expected->Printf.bprintf js "const got=await frame(input,rt.slots,rt);const expected=%s;if(!bytes(got.data).equals(bytes(expected)))throw Error('grade_index frame bytes');\n"(numeric expected)
     |Error d->Printf.bprintf js "try{await frame(input,rt.slots,rt);throw Error('grade_index accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
         (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
    Buffer.add_string js "if(bytes(new Uint8Array(data)).toString('hex')!==inputBytes||arrays.flat().filter(Array.isArray).some((a,i)=>JSON.stringify(a)!==texts[i]))throw Error('grade_index changed source');}\n}\n")
(["(array/grade (map (fn [(i : int)] (int/mod (int/add i (frame/index)) 7)) (array/range 32769 :type int)) :descending true)";
     "(array/grade (map (fn [x] (if (> t 0.) x (- 0. x))) (array/range 32769)))";
     "(array/grade (map (fn [(x : text)] (if (> t 0.) x \"safe\")) (array/text 32769 \"é日本😀\")) :descending true)";
     "(array/index_of (array/int 3 9007199254740993) (map (fn [(i : int)] (if (int/eq i 0) 9007199254740993 i)) (array/range 32769 :type int)))";
     "(array/index_of (array/text 2 \"a\") (map (fn [(x : text)] (if (> t 0.) x \"missing\")) (array/text 32769 \"a\")))";
     "(array/grade (array/text 0))";
     "(array/grade (array/int 13 9007199254740993) :descending true)";
     "(array/index_of (array/int 0) (array/int 13 1))";
     "(array/index_of (array/int 13 1) (array/int 0))";
     "(if (> t 0.) (array/int 0) (array/grade (error \"E_KEYS\" \"bad keys\" :cause \"raw\255\000cause\")))";
     "(array/index_of (error \"E_KEYS\" \"keys first\" :cause \"raw\255\000cause\") (error \"E_QUERIES\" \"queries second\"))"]
    @ ["(array/grade (map (fn [(x : text)] (if (> t 0.) x \"safe\")) (array/text 32769 "^literal(String.init 256 Char.chr)^")) :descending true)";
       "(array/index_of "^text_array raw^" (map (fn [(x : text)] (if (> t 0.) x \"missing\")) (array/text 32769 "^literal "\255\128"^")))"]
    @ List.concat_map(fun descending->List.map(fun keys->"(array/grade "^keys^" :descending "^descending^")")
      [text_array raw;text_array all]) ["false";"true"]
    @ ["(array/index_of "^text_array raw^" "^text_array(Array.append raw all)^")";
       "(array/index_of "^text_array all^" "^text_array(Array.append all[|String.init 256 Char.chr|])^")"]);
  Buffer.add_string js "}finally{for(const pool of pools)pool?.close()}console.log('grade_index actual emitted frames/runtime: stable exact keys, byte Text, first-match lookup, source immutability and workers');\n";
  let path=Filename.temp_file "grade_index-runtime" ".mjs"in
  Fun.protect ~finally:(fun()->Sys.remove path)(fun()->
    Out_channel.with_open_bin path(fun out->output_string out(Buffer.contents js));
    assert(Sys.command("node "^Filename.quote path)=0))
