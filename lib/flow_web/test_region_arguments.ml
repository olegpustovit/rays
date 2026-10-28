module E=Flow.Eval
module I=Flow_ir
let ok=function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let quote=Flow_web.Kernel_table.json_string
let literal text=fst(Flow.Lisp.print[Flow.Syntax.make(Flow.Syntax.Str text)])
let number x=Param.float_text x
let data=function
 |E.Int_array a->"BigInt64Array.from(["^String.concat ","(Array.to_list(Array.map(fun n->string_of_int n^"n")a))^"])"
 |E.Text_array a when Array.length a>0 && Array.for_all((=)a.(0))a->
   "Array("^string_of_int(Array.length a)^").fill("^quote a.(0)^")"
 |E.Text_array a->"["^String.concat ","(Array.to_list(Array.map quote a))^"]"
 |E.Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->"Float64Array.from(["^String.concat ","(Array.to_list(Array.map number a))^"])"
 |_->failwith "Expected array result"
let compiled ?(require_arguments=true) body=
 let text="(workspace bindings (graph g :context host (if (> t -1000.) "^body^" "^body^")))"in
 let checked=match Flow.Workspace.check Flow.Check.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
  |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
 let value=List.assoc "g"(ok(E.static checked)).results in
 let residual=match value with E.Residual r->r|_->failwith "Expected live region frame"in
 let refused=ref [] and previous= !(I.Packed.Private.trace) in
 let lowered=Fun.protect ~finally:(fun()->I.Packed.Private.trace:=previous)(fun()->
  I.Packed.Private.trace:=(function Ok _->()|Error ds->refused:=ds @ !refused);
  I.Core_lower.lower ~strict:true residual)in
 let program=match lowered with Ok p->p|Error reason->
  failwith("Core argument case "^body^": "^reason^"; "^
   String.concat "; "(List.map Diagnostic.to_string(List.rev !refused)))in
 if require_arguments then assert(Array.exists(fun r->I.Packed.Private.argument_types r<>[||])program.regions);
 value,program
let ()=
 let code=Buffer.create 1000000 in
 let path n="file://"^Filename.concat(Sys.getcwd())("../web_export/runtime/"^n)in
 Printf.bprintf code "import {createRuntime} from %s;import {createPool} from %s;import * as p from %s;const bytes=a=>Buffer.from(a.buffer,a.byteOffset,a.byteLength);const snapshot=v=>v instanceof p.PArray||v instanceof p.TextArray?snapshot(v.data):ArrayBuffer.isView(v)?bytes(v).toString('hex'):v instanceof Map?[...v].map(([n,x])=>[n,snapshot(x)]):Array.isArray(v)?v.map(snapshot):typeof v==='bigint'?v.toString():v;const pools=[null,await createPool({size:1}),await createPool({size:8})];try{\n"
  (quote(path"runtime.js"))(quote(path"workers.js"))(quote(path"prims.js"));
 let cases=[
  "(let* [src (array/range (int/sub (frame/index) 32766) :type int)] (map (fn [(i : int)] (int/add i 9007199254740993)) src))";
  "(let* [src (array/range (int/sub (frame/index) 32766)) flag (> t 0.)] (map (fn [x] (if flag x (- 0. x))) src))";
  "(let* [src (array/text (int/sub (frame/index) 32766)) label (str \"frame=\" (frame/index))] (map (fn [(x : text)] (if (> t 0.) label x)) src))";
  "(let* [src (map (fn [x] [x t]) (array/range (int/sub (frame/index) 32766)))] (map (fn [x] x) src))";

  "(let* [src (array/range (int/sub (frame/index) 32766)) scale (+ t 1.)] (map (fn [x] (* scale x)) src))";
  "(let* [src (array/vec3 (int/sub (frame/index) 32766) [t -0. 3]) captured {:delta [t 1. -0.]}] (map (fn [x] (+ x captured.delta)) src))";
  "(let* [src (array/range (int/sub (frame/index) 32766))] (map (fn [x] [x t]) src))";
  "(let* [src (array/vec4 (int/sub (frame/index) 32766) [t -0. 1. 2.])] (map (fn [x] x) src))";
  "(let* [src (array/text (int/sub (frame/index) 32766) "^literal(String.init 256 Char.chr)^")] (map (fn [(x : text)] (if (> t 0.) x \"untaken\")) src))";
  "(let* [src (array/range (frame/index) :type int)] (for [i src j (array/range -1 :type int)] i))";
  "(let* [src (array/range (frame/index) :type int)] (map (fn [(i : int) (j : int)] i) src (array/range -1 :type int)))";
  "(let* [src (array/range (int/sub (frame/index) 32766) :type int)] (map (fn [(i : int)] (if (int/lt i 0) (array/nth (if (> t 0.) (error \"E_TABLE\" \"unreached table\" :cause \"raw\255\000cause\") (array/range 1)) 0) (float i))) src))";
  "(let* [src (array/range (int/sub (frame/index) 32766) :type int)] (map (fn [(i : int)] (if (int/eq i 0) (array/nth (if (> t 0.) (error \"E_TABLE\" \"selected table\" :cause \"raw\255\000cause\") (array/range 1)) 0) (float i))) src))";
  "(let* [src (array/range (int/sub (frame/index) 32766) :type int) table (array/range (int/sub (frame/index) 32766))] (map (fn [(i : int)] (array/nth table i)) src))";
  "(let* [src (array/range (frame/index) :type int)] (map (fn [(i : int)] (if (int/eq i 0) (array/nth (if (> t 0.) (error \"E_TABLE\" \"empty suppresses table\" :cause \"raw\255\000cause\") (array/range 1)) 0) (float i))) src))"
 ]in
 List.iter(fun body->let value,program=compiled body in
  let frame=ok(Flow_web.Js.frame program)in
  (* These are templates. Actual arrays/counts/captures are bound by emitted
     Core.Invoke, never by reference results or unbound prepare_values. *)
  let regions=Array.to_list(Array.map(fun r->"{source:"^quote(Flow_web.Js.region r)^"}")program.regions)in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));const program={regions:[%s]};for(const pool of pools){const rt=createRuntime(program,{pool});const invoke=rt.invoke;rt.invoke=async function(k,args,plan){if(!plan)throw Error('Computed dependency has no region plan');const before=JSON.stringify(snapshot(args));try{return await invoke.call(this,k,args,plan)}finally{if(JSON.stringify(snapshot(args))!==before)throw Error('dynamic arguments mutated')}};\n"(quote frame)(String.concat ","regions);
  List.iter(fun(index,time)->let live={(Input.at_time time)with frame=index;tick=index}in
   Printf.bprintf code "{const input={t:%s,dt:0,frame:%d,tick:%d,size:[1,1]};rt.current=input;\n"(number time)index index;
   (match E.Private.force_reference value ~live with
   |Ok expected->Printf.bprintf code "const result=await frame(input,rt.slots,rt),expected=%s;if(%b?JSON.stringify(result.data)!==JSON.stringify(expected):!bytes(result.data).equals(bytes(expected)))throw Error('argument frame bytes');\n"
       (data expected)(match expected with E.Text_array _->true|_->false)
   |Error d->Printf.bprintf code "try{await frame(input,rt.slots,rt);throw Error('argument frame accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
       (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
   Buffer.add_string code "}\n") [0,0.25;32766,0.25;32779,0.25;37767,0.25;65535,0.25;65539,0.75;65539,-0.25];
  Buffer.add_string code "}}\n")cases;
 (* Actual lazy table preparation supplies its own statement provenance. A
    worker must retain it before the outer frame assigns its Invoke location. *)
 let _,capture_program=compiled
   "(let* [src (array/range (frame/index) :type int)] (map (fn [(i : int)] (if (int/eq i 0) (array/nth (if (> t 0.) (error \"E_CAPTURE_LOCATION\" \"selected capture failed\" :cause \"raw\255\000cause\") (array/range 1)) 0) (float i))) src))"in
 let capture_frame=ok(Flow_web.Js.frame capture_program)in
 Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));let expectedStatement;for(const pool of pools)for(const count of [1000,32769]){const rt=createRuntime({regions:[]},{pool});const invoke=rt.invoke;let reached=false;rt.invoke=async function(k,args,plan){try{return await invoke.call(this,k,args,plan)}catch(e){if(e.code==='E_CAPTURE_LOCATION'){reached=true;if(e.statement===undefined)throw Error('capture statement lost in worker');if(expectedStatement===undefined)expectedStatement=e.statement;else if(e.statement!==expectedStatement)throw Error('capture statement changed with count or pool')}throw e}};const input={t:.25,dt:0,frame:count,tick:count,size:[1,1]};rt.current=input;try{await frame(input,rt.slots,rt);throw Error('capture error accepted')}catch(e){if(!reached||e.code!=='E_CAPTURE_LOCATION'||e.message!=='selected capture failed'||e.cause!==%s||e.statement!==expectedStatement)throw e}}}\n"
   (quote capture_frame)(quote "raw\255\000cause");
 (* Malformed concrete arguments are injected at the real invocation boundary.
    The kernel and its source preparations remain entirely compiler-generated. *)
 List.iter(fun(body,mutate,expectedCode,expectedMessage)->let _,program=compiled body in
  let frame=ok(Flow_web.Js.frame program)in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));for(const pool of pools){const rt=createRuntime({regions:[]},{pool});const invoke=rt.invoke;rt.invoke=function(k,args,plan){if(plan&&args.length){%s}return invoke.call(this,k,args,plan)};const input={t:.25,dt:0,frame:65535,tick:65535,size:[1,1]};rt.current=input;try{await frame(input,rt.slots,rt);throw Error('bad arguments accepted')}catch(e){if(e.code!==%s||e.message!==%s)throw e}}}\n"
   (quote frame)mutate(quote expectedCode)(quote expectedMessage))
 [List.hd cases,"args=[];","E_CORE_ARGUMENT","Compiled region argument count differs from its declaration.";
  List.hd cases,"args[plan.argumentTypes.findIndex(t=>t.array==='int')]=new p.PArray(1,Float64Array.of(1));","E_CORE_ARGUMENT","Compiled region argument storage differs from its declared type.";
  List.hd cases,"args[plan.argumentTypes.findIndex(t=>t.array==='int')]=new p.PArray(1,BigInt64Array.of(-(1n<<63n)));","E_CORE_ARGUMENT","Compiled region argument storage differs from its declared type.";
  List.nth cases 4,"args[plan.argumentTypes.findIndex(t=>t.array==='float')]=new p.PArray(1,Float64Array.of(NaN));","E_NONFINITE","array input produced a nonfinite value.";
  List.nth cases 4,"args[plan.argumentTypes.findIndex(t=>t.array==='float')]=new p.PArray(1,Float64Array.of(Infinity));","E_NONFINITE","array input produced a nonfinite value.";
  List.nth cases 4,"args[plan.argumentTypes.findIndex(t=>t.array==='float')]=new p.PArray(1,Float64Array.of(-Infinity));","E_NONFINITE","array input produced a nonfinite value.";
  List.nth cases 5,"args[plan.argumentTypes.findIndex(t=>t.array==='vec3')]=new p.PArray(3,Float64Array.of(1,2));","E_ARRAY_TYPE","Packed vec3 storage has three coordinates per element.";
  List.nth cases 8,"const i=plan.argumentTypes.findIndex(t=>t.array==='text');args[i]=new p.TextArray([]);args[i].data=[3];","E_ARRAY_TYPE","Text arrays contain text values."];
 (* Declare an unused composite argument in the real Core program, rather than
    changing runtime metadata by hand. Native bind_arguments scans even unused
    uniform declarations; validation must visit every physical List/Record leaf. *)
 let _,original=compiled(List.hd cases) in
 let unused value (program:I.Core.program)=
  let local=Array.length program.locals and constant=Array.length program.constants in
  let ty=Flow.Value.ty_of value in
  let regions=Array.map(fun (r:I.Core.Region.t)->
   let index=Array.length(I.Packed.Private.argument_types r) in
   let marker=E.Source("$core/argument",ty,["index",E.Int index]) in
   (* with_bindings captures only names read by its term, so an unused binding
      cannot declare this argument. Uniforms are canonical region declarations,
      including slots never read by an instruction; bind_arguments scans them. *)
   let r=Result.get_ok(I.Core.Region.rebuild r ~uniforms:(Array.append r.uniforms[|marker|])
     ~uniform_names:(Array.append r.uniform_names[|"",[]|]) ())in
   let declared=I.Packed.Private.argument_types r in
   assert(Array.length declared=index+1 && declared.(index)=ty);
   r)program.regions in
  let rec rewrite (s:I.Core.statement)=
   let node=match s.node with
    |Invoke x->I.Core.Invoke{x with args=x.args@[local]}
    |If x->I.Core.If{x with yes=List.map rewrite x.yes;no=List.map rewrite x.no}
    |node->node in {s with node} in
  let rec maximum (s:I.Core.statement)=match s.node with
   |If{yes;no;_}->List.fold_left(fun n s->max n(maximum s))s.id(yes@no)
   |_->s.id in
  let id=1+List.fold_left(fun n s->max n(maximum s))0 program.body in
  let source=(List.hd program.body).I.Core.source in
  let binding:I.Core.statement={id;source;node=Assign(local,Constant constant)} in
  {program with regions;locals=Array.append program.locals[|ty|];
   constants=Array.append program.constants[|Option.get(I.Core.literal_of_value value)|];
   body=binding::List.map rewrite program.body} in
 (* Force the ordinary Float Product through a real templated Invoke. The
    unused concrete declaration adds no evaluation or numeric instruction. *)
 List.iter(fun body->
  let value,program=compiled ~require_arguments:false body in
  let float_regions=Array.to_list program.I.Core.regions|>List.filter(fun r->r.I.Core.Region.iteration=I.Core.Region.Product)in
  assert(float_regions<>[]);
  List.iter(fun r->assert(Array.for_all(function
    |I.Core.Region.Input _|Const _|Uniform _|Frame _|Binary _|Unary _->true|_->false)r.I.Core.Region.code))float_regions;
  let frame=ok(Flow_web.Js.frame(unused(E.List[|E.Int 1|])program))in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));for(const pool of pools){const rt=createRuntime({regions:[]},{pool});\n"(quote frame);
  List.iter(fun(index,time)->let live={(Input.at_time time)with frame=index;tick=index}in
   Printf.bprintf code "{const input={t:%s,dt:0,frame:%d,tick:%d,size:[1,1]};rt.current=input;\n"(number time)index index;
   (match E.Private.force_reference value ~live with
    |Ok expected->Printf.bprintf code "const result=await frame(input,rt.slots,rt),expected=%s;if(!bytes(result.data).equals(bytes(expected)))throw Error('Float Product bytes');\n"(data expected)
    |Error d->Printf.bprintf code "try{await frame(input,rt.slots,rt);throw Error('Float Product accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
      (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
   Buffer.add_string code "}\n") [0,0.25;32766,0.25;32767,0.25;65535,(-0.25)];
  Buffer.add_string code "}}\n")
 ["(for [x (array/float (int/sub (frame/index) 32766)) y (array/float (if (> t 0.) -1 2))] (+ x y))";
  "(let* [gain (exp (* t 100000.))] (for [x (array/float (int/sub (frame/index) 32766)) y (array/float 2)] (+ x gain)))"];
 let composites=[
  E.List[|E.Int 9007199254740993;E.Float 0.5|];
  E.List[|E.Text "fallback";E.Int 9007199254740993;E.Float 0.5|];
  E.Record["leaves",E.List[|E.Int 9007199254740993;E.Float 0.5|];"large",E.Int max_int];
  E.List[|E.Int_array[|1;2;3|]|];
  E.Record["leaves",E.List[|E.Int_array[|1;2;3|]|]];
  E.List[|E.Int 9007199254740993;E.Float Float.nan|];
  E.Record["leaves",E.List[|E.Int 9007199254740993;E.Float Float.infinity|]]
 ]in
 List.iter(fun value->let program=unused value original in
  let frame=ok(Flow_web.Js.frame program)in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));for(const pool of pools){const rt=createRuntime({regions:[]},{pool});\n"(quote frame);
  List.iter(fun index->let live={(Input.at_time 0.25)with frame=index;tick=index}in
   let native=Test_program.run ~strict:true ~force:(fun p->I.Packed.force ~strict:true p ~live)program ~live in
   Printf.bprintf code "{const input={t:.25,dt:0,frame:%d,tick:%d,size:[1,1]};rt.current=input;\n"index index;
   (match native with
    |Ok expected->Printf.bprintf code "const got=await frame(input,rt.slots,rt),expected=%s;if(!bytes(got.data).equals(bytes(expected)))throw Error('unused argument bytes');\n"(data expected)
    |Error d->Printf.bprintf code "try{await frame(input,rt.slots,rt);throw Error('unused invalid leaf accepted')}catch(e){if(e.code!==%s||e.message!==%s||e.cause!==%s)throw e}\n"
      (quote d.code)(quote d.message)(Option.fold ~none:"undefined" ~some:quote d.cause));
   Buffer.add_string code "}\n")[32766;65535];
  Buffer.add_string code "}}\n")composites;
 (* Invalid signed63 values and nested packed layouts must reject before any
    kernel read, including inside widening and fallback-typed lists. *)
 List.iter(fun(value,mutation)->let frame=ok(Flow_web.Js.frame(unused value original))in
  Printf.bprintf code "{const {frame}=await import('data:text/javascript;base64,'+Buffer.from(%s).toString('base64'));for(const pool of pools){const rt=createRuntime({regions:[]},{pool});const invoke=rt.invoke;rt.invoke=function(k,args,plan){%s;return invoke.call(this,k,args,plan)};const input={t:.25,dt:0,frame:32766,tick:32766,size:[1,1]};rt.current=input;try{await frame(input,rt.slots,rt);throw Error('unused out-of-range Int accepted')}catch(e){if(e.code!=='E_CORE_ARGUMENT'||e.message!=='Compiled region argument storage differs from its declared type.')throw e}}}\n"(quote frame)mutation)
 [E.List[|E.Int 9007199254740993;E.Float 0.5|],"args.at(-1)[0]=1n<<62n";
  E.List[|E.Text "fallback";E.Int 9007199254740993;E.Float 0.5|],"args.at(-1)[1]=-(1n<<62n)-1n";
  E.Record["leaves",E.List[|E.Int 9007199254740993;E.Float 0.5|]],"args.at(-1).get('leaves')[0]=1n<<62n";
  E.List[|E.Int_array[|1;2;3|]|],"args.at(-1)[0]=new p.PArray(3,BigInt64Array.of(1n,2n,3n))";
  E.Record["leaves",E.List[|E.Int_array[|1;2;3|]|]],"args.at(-1).get('leaves')[0]=new p.PArray(3,BigInt64Array.of(1n,2n,3n))";
  E.List[|E.Text "fallback";E.Int_array[|1;2;3|]|],"args.at(-1)[1]=new p.PArray(5,Float64Array.of(1))"];
 (* These compiler refusals retain real table preparation. The emitter cannot
    silently evaluate an unknown host or turn a synchronous capture asynchronous. *)
 let _,table_program=compiled (List.nth cases 13) in
 let replace_table value=
  let replaced=ref false in
  let rec table_slot=function I.Core.Region.Array_prepare(i,_)->Some i
   |Guarded(_,body)->table_slot body|_->None in
  let regions=Array.map(fun (r:I.Core.Region.t)->
   match Array.find_map table_slot r.code with
   |None->r|Some i->let uniforms=Array.copy r.uniforms in
     uniforms.(i)<-value;replaced:=true;Result.get_ok(I.Core.Region.rebuild r ~uniforms ()))table_program.regions in
  assert(!replaced);{table_program with regions} in
 let refused message program=match Flow_web.Js.frame program with
  |Error d->assert(d.code="E_WEB_SOURCE");
    Option.iter(fun message->if d.message<>message then
      failwith("Region capture refusal: "^d.message^"; expected "^message))message
  |Ok _->failwith "Unsupported region capture accepted" in
 refused None (replace_table(E.Source("$host/unavailable",Flow.Ty.Array Flow.Ty.Float,[])));
 (* A concrete graph input gives this producer a genuinely packed residual.
    Its Invoke is asynchronous, with no structured-call slots or state plane. *)
 let forms=ok(Flow.Syntax.parse "(workspace capture (graph g :context host [(source : (array float) (array/float 0))] (map (fn [x] (+ x t)) source)))")in
 let checked=match Flow.Workspace.check Flow.Check.{version=1;kinds=[]}forms with
  |Some ws,[]->ws|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
 let value=List.assoc "g"(ok(E.static ~inputs:["g",["source",E.Float_array(Array.init 32769 float_of_int)]]checked)).results in
 let r=match value with E.Residual r->r|_->failwith "Async capture needs a live residual"in
 let view=E.Residual.residual_view r in
 (match I.Packed.compile_result r view.term with Ok _->()|Error ds->
  failwith("Async capture packed qualification: "^String.concat ";"(List.map Diagnostic.to_string ds)));
 let packed_core=match I.Core_lower.lower r with Ok p->p|Error form->failwith("Async capture Core: "^form)in
 if packed_core.call_slots<>0 || Array.length packed_core.state_slots<>0 || Array.length packed_core.regions=0 then
  failwith(Printf.sprintf "Async capture is not state-free packed Core: calls=%d states=%d regions=%d"
   packed_core.call_slots(Array.length packed_core.state_slots)(Array.length packed_core.regions));
 let fixed=ok(Flow_web.Js.frame packed_core)in
 assert(not(List.exists(String.starts_with ~prefix:"const B")(String.split_on_char '\n' fixed)));
 refused (Some "Asynchronous region captures require a compiled synchronous preparation path.")
  (replace_table value);
 let stateful,_=compiled ~require_arguments:false "(state [s (array/range 1)] s)" in
 refused (Some "Region preparation cannot own state or call-index slots.")
  (replace_table stateful);
 Buffer.add_string code "}finally{for(const pool of pools)pool?.close()}console.log('Core region argument binding: actual counts, lazy captures, changing frames, exact bytes, errors and workers');\n";
 let file=Filename.temp_file "region-arguments" ".mjs"in
 Fun.protect ~finally:(fun()->Sys.remove file)(fun()->
  Out_channel.with_open_bin file(fun out->output_string out(Buffer.contents code));
  assert(Sys.command("node "^Filename.quote file)=0))
