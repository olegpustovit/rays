module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let bits x = Printf.sprintf "%016Lx" (Int64.bits_of_float x)
let number x = Param.float_text x
let array xs = "Float64Array.from([" ^ String.concat "," (Array.to_list (Array.map number xs)) ^ "])"
let value ?inputs body =
  let source="(workspace w (graph g :context host [(data : (array vec3) (array/vec3 0))] "^body^"))" in
  let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok(Flow.Syntax.parse source)) with
    |Some ws,[] -> ws |_,ds -> failwith(String.concat ";" (List.map Diagnostic.to_string ds)) in
  List.assoc "g" (ok(E.static ?inputs ws)).results
let live = {(Input.at_time 1.) with frame=3}
let () =
  let minsub=Int64.float_of_bits 1L in
  let a=1.0000000000000002 and b=0.9999999999999998 in
  assert(Float.fma a b (-1.) <> Sys.opaque_identity(a*.b)-.1.);
  let cases=ref [1.0000000000000002,0.9999999999999998,-1.;
    0.,-1.,0.;0.,-1.,-0.;-0.,1.,-0.;minsub,0.5,0.;minsub,0.5,minsub;
    -.minsub,0.5,0.;2.**(-537.),2.**(-537.),-.minsub;
    Float.max_float,2.,-.Float.max_float;0.37,0.83,0.19] in
  let rng=ref(Rays_math.Rand.seed 0xf4a) in
  let next bound=let n,g=Rays_math.Rand.int ~bound !rng in rng:=g;n in
  let sample()=let raw=Int64.of_int(next max_int) in
    let raw=if next 2=0 then raw else Int64.logor raw 0x4000000000000000L in
    let raw=if next 2=0 then raw else Int64.logor raw Int64.min_int in
    Int64.float_of_bits(Int64.logand raw 0xffefffffffffffffL) in
  let count=ref(List.length !cases) in
  while !count < 10010 do
    let a=sample() and b=sample() and c=sample() in
    if Float.is_finite(Float.fma a b c) then (cases:=(a,b,c)::!cases; incr count)
  done;
  let cases=Array.of_list(List.rev !cases) in
  let data=Array.init(Array.length cases*3)(fun i->let a,b,c=cases.(i/3) in [|a;b;c|].(i mod 3)) in
  let expected=Array.map(fun(a,b,c)->Float.fma a b c) cases in
  let source=value ~inputs:["g",["data",E.Vec3_array data]]
    "(map (fn [(p : vec3)] (if (> t 0.) (fma p.x p.y p.z) 0.)) data)" in
  let residual=match source with E.Residual r->r|_->failwith"FMA not live" in
  let packed=match I.Packed.compile_result residual (E.Residual.residual_view residual).term with
    |Ok p->p|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds)) in
  let compiled=ok(I.Executor.compile ~strict:true source) in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    List.iter(fun result->match result with E.Float_array result->
      assert(Array.length result=Array.length expected);
      Array.iteri(fun i x->if bits x<>bits expected.(i) then failwith(Printf.sprintf"FMA native %d %s <> %s" i(bits x)(bits expected.(i))))result
      |_->failwith"FMA array expected")
      [ok(E.Host.force_reference source ~live);ok(I.Packed.force ~strict:true packed ~live);
       ok(I.Executor.force ~strict:true compiled ~live)])) [1;8];
  assert(I.Packed.gpu_refusals packed<>[]);
  let scalar=Option.get(Flow.Op.find "fma" Flow.Context.value) in
  List.iter(fun args->try ignore(scalar.body ~live ~node:(fun _ _ -> assert false) (List.combine ["a";"b";"c"] args));failwith"Nonfinite FMA accepted"
    with Diagnostic.Error d -> assert(d.code="E_NONFINITE"))
    [[E.Float infinity;E.Float 0.;E.Float 1.];[E.Float 1.;E.Float nan;E.Float 0.];
     [E.Float 1.;E.Float 1.;E.Float neg_infinity];[E.Float Float.max_float;E.Float 2.;E.Float 0.]];
  (* The typed bank also carries Int frame/index and Int source values into FMA. *)
  let mixed=value "(map (fn [x] (fma (float x) (float (frame/index)) t)) (array/range 5001 :type int))" in
  let mixed_compiled=ok(I.Executor.compile ~strict:true mixed) in
  assert(ok(I.Executor.force ~strict:true mixed_compiled ~live)=ok(E.Host.force_reference mixed ~live));
  List.iter(fun body->let v=value body in let p=ok(I.Executor.compile ~strict:true v) in
    assert(ok(I.Executor.force ~strict:true p ~live)=E.Float_array(Array.make 5001 0.)))
    ["(map (fn [x] (if (> t 0.) 0. (fma 1.7976931348623157e308 2. x))) (array/range 5001))"];
  let overflow=value "(map (fn [x] (if (> t 0.) (fma 1.7976931348623157e308 2. x) 0.)) (array/range 5001))" in
  (match I.Executor.force ~strict:true (ok(I.Executor.compile ~strict:true overflow)) ~live with
   |Error d->assert(d.code="E_NONFINITE") |Ok _->failwith"Overflow FMA accepted");
  let prepared=ok(I.Packed.Host.prepare_values packed ~live) in
  let js=Buffer.create 2000000 in
  Printf.bprintf js "import {fma} from %S;\n" ("file://"^Filename.concat(Sys.getcwd())"../web_export/runtime/prims.js");
  Buffer.add_string js (Flow_web.Js.region packed);
  Printf.bprintf js "const input=%s; const output=new Float64Array(%d);\n" (array data)(Array.length cases);
  let flatten=function E.Float x->[|x|]|Int n->[|float n|]|Vec3(a,b,c)->[|a;b;c|]|_->failwith"uniform" in
  let uniforms=Array.to_list(Array.map(fun v->array(flatten v)) prepared.uniforms)
    @ [array(Array.map(function E.Float x->x|Int n->float n|_->assert false) prepared.frame)] in
  Printf.bprintf js "if(region([input],[%s],[output],0,%d)!==0)throw Error('region refusal');\n"
    (String.concat "," uniforms)(Array.length cases);
  Buffer.add_string js "const bytes=new DataView(new ArrayBuffer(8)); const bits=x=>{bytes.setFloat64(0,x,false);return bytes.getBigUint64(0,false).toString(16).padStart(16,'0')};\n";
  Printf.bprintf js "const expected=[%s];\n"(String.concat ","(Array.to_list(Array.map(fun x->Printf.sprintf"%S"(bits x))expected)));
  Buffer.add_string js "for(let i=0;i<output.length;i++){for(const x of [output[i],fma(input[3*i],input[3*i+1],input[3*i+2])])if(bits(x)!==expected[i])throw Error('FMA bits '+i+' '+bits(x)+' != '+expected[i]);}\n";
  Buffer.add_string js "if(fma(1.0000000000000002,0.9999999999999998,-1)===1.0000000000000002*0.9999999999999998-1)throw Error('FMA lost single rounding');\n";
  Buffer.add_string js "for(const args of [[Infinity,0,1],[1,NaN,0],[1,1,-Infinity],[Number.MAX_VALUE,2,0]]){try{fma(...args);throw Error('accepted')}catch(e){if(e.code!=='E_NONFINITE')throw e}}\n";
  let region_test v expected =
    let r=match v with E.Residual r->r|_->assert false in
    let p=match I.Packed.compile_result r (E.Residual.residual_view r).term with
      |Ok p->p |Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds)) in
    let inputs=ok(I.Packed.Host.prepare_values p ~live) in
    let input=function E.Int_array xs->"BigInt64Array.from(["^String.concat ","(Array.to_list(Array.map(fun n->string_of_int n^"n")xs))^"])"
      |E.Float_array xs->array xs|_->assert false in
    let frame=function E.Int n->string_of_int n^"n"|Float x->number x|_->assert false in
    Buffer.add_string js "{\n";Buffer.add_string js(Flow_web.Js.region p);
    Printf.bprintf js "const out=new Float64Array(%d);const status=region([%s],[%s],[out],0,%d);\n"
      inputs.count (String.concat ","(Array.to_list(Array.map input inputs.arrays)))
      (String.concat ","(Array.to_list(Array.map(fun v->array(flatten v))inputs.uniforms)
        @["["^String.concat ","(Array.to_list(Array.map frame inputs.frame))^"]"])) inputs.count;
    (match expected with None->Buffer.add_string js "if(status!==1)throw Error('selected FMA overflow');\n"
     |Some f->Buffer.add_string js "if(status!==0)throw Error('mixed/lazy FMA refused');\n";
       Printf.bprintf js "for(let i=0;i<out.length;i++)if(bits(out[i])!==bits(%s))throw Error('mixed/lazy FMA bits '+i);\n" f);
    Buffer.add_string js "}\n" in
  region_test mixed (Some "i*3+1");
  region_test (value "(map (fn [x] (if (> t 0.) 0. (fma 1.7976931348623157e308 2. x))) (array/range 5001))") (Some "0");
  region_test overflow None;
  let bad=match I.Core.Region.rebuild packed ~code:
    [|Const infinity;Const 0.;Const 1.;Fma(0,1,2)|] ~outputs:[|[|3|]|] () with
    |Ok p->p|Error _->assert false in
  assert(Result.is_error(I.Packed.force ~strict:true bad ~live));
  Buffer.add_string js "{\n";Buffer.add_string js(Flow_web.Js.region bad);
  Buffer.add_string js "if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==1)throw Error('nonfinite FMA input accepted');\n}\n";
  let path=Filename.temp_file "fma" ".mjs" in
  Out_channel.with_open_bin path(fun c->output_string c(Buffer.contents js));
  let status=Sys.command("node "^Filename.quote path) in Sys.remove path;assert(status=0);
  Printf.printf "Explicit FMA: %d exact native/reference/CPU one/eight/JS region+primitive binary64 cases; mixed Int bank, lazy overflow and GPU refusal pass\n" (Array.length cases)

let () =
  let cases=[|Float.max_float,2.,0.;Float.max_float,2.,-.Float.max_float;
    Float.max_float,2.,Float.max_float;1.+.2.**(-27.),1.-.2.**(-27.),-1.;
    -0.,1.,-0.;0.,-1.,0.;Int64.float_of_bits 1L,0.5,0.;
    -.Int64.float_of_bits 1L,0.5,-0.;0.37,0.83,0.19|] in
  let safe x=if Float.is_finite x then x else 0. in
  let valid x=if Float.is_finite x then 1. else 0. in
  let count=32769 in
  let data=Array.init(count*3)(fun i->let a,b,c=cases.(i/3 mod Array.length cases) in
    match i mod 3 with 0->a|1->b|_->c) in
  let expected=Array.init(count*4)(fun i->let a,b,c=cases.(i/4 mod Array.length cases) in
    let product=Sys.opaque_identity(a*.b) and fused=Float.fma a b c in
    match i mod 4 with 0->safe product|1->valid product|2->safe fused|_->valid fused) in
  let body="(map (fn [(p : vec3)] (let* [multiply (float/mul_checked p.x p.y) fused (float/fma_checked p.x p.y p.z)] [multiply.value (if multiply.valid 1.0 0.0) fused.value (if fused.valid 1.0 0.0)])) data)" in
  (* A live guard keeps the checked body as a residual while input bits stay exact. *)
  let source=value ~inputs:["g",["data",E.Vec3_array data]] ("(if (> t 0.) "^body^" "^body^")") in
  let residual=match source with E.Residual r->r|_->assert false in
  let core=Result.get_ok(I.Core_lower.lower ~strict:true residual) in
  let compiled=match I.Executor.compile ~strict:true source with Ok p->p|Error d->
    failwith("Checked main compile: "^Diagnostic.to_string d) in
  let packed=match I.Packed.compile_result residual (E.Residual.residual_view residual).term with
    |Ok p->p|Error _ ->
      (* The outer structural if belongs to Core; inspect its actual map region. *)
      core.regions.(0) in
  let same=function E.Vec4_array values -> assert(Array.length values=Array.length expected);
      Array.iteri(fun i x->if bits x<>bits expected.(i)then
        failwith(Printf.sprintf "Checked arithmetic %d: %s <> %s" i(bits x)(bits expected.(i))))values
    |_->failwith "Checked arithmetic Vec4 array expected" in
  same(ok(E.Host.force_reference source ~live));
  List.iter(fun domains->Parallel.run ~domains(fun()->
    same(ok(I.Executor.force ~strict:true compiled ~live));
    same(ok(Test_program.run ~force:(fun p->I.Packed.force ~strict:true p ~live) core ~live));
    same(ok(I.Packed.force ~strict:true packed ~live)))) [1;8];
  assert(I.Packed.gpu_refusals packed<>[]);
  (match Flow_web.Wgsl.kernel packed with Error d->assert(d.code="E_GPU_FORM")|Ok _->assert false);
  let evaluate body=let source=value body in
    let compiled=match I.Executor.compile ~strict:true source with Ok p->p|Error d->
      failwith(body^": "^Diagnostic.to_string d) in
    let expected=ok(E.Host.force_reference source ~live) in
    assert(ok(I.Executor.force ~strict:true compiled ~live)=expected);expected in
  List.iter(fun body->assert(evaluate body=E.Float_array(Array.make 5001 0.))) [
    "(map (fn [x] (let* [bad (float/mul_checked 1.7976931348623157e308 2.0)] (if bad.valid (+ x t) bad.value))) (array/range 5001))";
    "(map (fn [x] (if (> t 0.) 0.0 (get (float/fma_checked x 1.7976931348623157e308 1.7976931348623157e308) :value))) (array/range 5001))";
    "(let* [helper (fn [(a : float)] (float/mul_checked a 1.7976931348623157e308))] (map (fn [x] (let* [status (helper (+ x t))] (if status.valid 0.0 status.value))) (array/range 5001 :type int)))" ];
  assert(evaluate "(let* [first (float/mul_checked 1.7976931348623157e308 (+ t 1.0)) second (float/mul_checked first.value 0.0)] (and first.valid second.valid))"=E.Bool false);
  List.iter(fun name->let op=Option.get(Flow.Op.find name Flow.Context.value) in
    let keys=List.map fst op.signature.pos in
    List.iter(fun bad->let args=List.mapi(fun i key->key,E.Float(if i=0 then bad else 1.))keys in
      try ignore(op.body ~live ~node:(fun _ _->assert false)args);failwith "Nonfinite checked input accepted"
      with Diagnostic.Error d -> assert(d.code="E_NONFINITE")) [nan;infinity;neg_infinity])
    ["float/mul_checked";"float/fma_checked"];
  let prepared=ok(I.Packed.Host.prepare_values packed ~live) in
  let flatten=function E.Float x->[|x|]|Int n->[|float n|]|Bool b->[|if b then 1. else 0.|]
    |Vec3(a,b,c)->[|a;b;c|]|_->failwith "Checked uniform" in
  let js=Buffer.create 3_000_000 in
  Printf.bprintf js "import {float_mul_checked as mul,float_fma_checked as fma} from %S;\n"
    ("file://"^Filename.concat(Sys.getcwd())"../web_export/runtime/prims.js");
  Buffer.add_string js(Flow_web.Js.region packed);
  Printf.bprintf js "const input=%s, out=new Float64Array(%d);\n" (array data)(Array.length expected);
  let uniforms=Array.to_list(Array.map(fun v->array(flatten v))prepared.uniforms)
    @ [array(Array.map(fun v->(flatten v).(0))prepared.frame)] in
  Printf.bprintf js "if(region([input],[%s],[out],0,%d)!==0)throw Error('checked region refused');\n"
    (String.concat "," uniforms)count;
  Buffer.add_string js "const bytes=new DataView(new ArrayBuffer(8));const bits=x=>{bytes.setFloat64(0,x,false);return bytes.getBigUint64(0,false).toString(16).padStart(16,'0')};\n";
  Printf.bprintf js "const expected=[%s];\n" (String.concat ","(Array.to_list(Array.map(fun x->Printf.sprintf "%S"(bits x))expected)));
  Buffer.add_string js "for(let i=0;i<input.length/3;i++){const a=mul(input[3*i],input[3*i+1]),b=fma(input[3*i],input[3*i+1],input[3*i+2]);const scalar=[a.get('value'),+a.get('valid'),b.get('value'),+b.get('valid')];for(let j=0;j<4;j++){const k=4*i+j;if(bits(out[k])!==expected[k]||bits(scalar[j])!==expected[k])throw Error('checked bits '+k)}}\n";
  Buffer.add_string js "for(const f of [mul,(a,b)=>fma(a,b,0)])for(const bad of [Infinity,-Infinity,NaN]){try{f(bad,1);throw Error('accepted bad input')}catch(e){if(e.code!=='E_NONFINITE')throw e}}\n";
  List.iter(fun bad->List.iter(fun instruction->
    let malformed=Result.get_ok(I.Core.Region.rebuild packed
      ~code:[|Const bad;Const 1.;Const 0.;instruction|] ~outputs:[|[|3|]|] ()) in
    (match I.Packed.force ~strict:true malformed ~live with
     |Error d->assert(d.code="E_NONFINITE")|Ok _->failwith "Checked malformed input accepted");
    Buffer.add_string js "{\n";Buffer.add_string js(Flow_web.Js.region malformed);
    Buffer.add_string js "if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==1)throw Error('checked nonfinite input accepted');\n}\n")
    [I.Packed.Checked_mul(Value,0,1);Checked_mul(Valid,0,1);
     Checked_fma(Value,0,1,2);Checked_fma(Valid,0,1,2)]) [nan;infinity;neg_infinity];
  let guarded=Result.get_ok(I.Core.Region.rebuild packed
    ~code:[|Const 0.;Const infinity;Const 1.;Guarded([0,true],Checked_mul(Value,1,2))|]
    ~outputs:[|[|0|]|] ()) in
  (match ok(I.Packed.force ~strict:true guarded ~live)with
   |E.Float_array values->assert(Array.for_all((=)0.)values)|_->assert false);
  Buffer.add_string js "{\n";Buffer.add_string js(Flow_web.Js.region guarded);
  Buffer.add_string js "if(region([new Float64Array(3)],[],[new Float64Array(1)],0,1)!==0)throw Error('untaken checked input refused');\n}\n";
  let path=Filename.temp_file "checked-arithmetic" ".mjs" in
  Fun.protect ~finally:(fun()->Sys.remove path)(fun()->
    Out_channel.with_open_bin path(fun c->output_string c(Buffer.contents js));
    assert(Sys.command("node "^Filename.quote path)=0));
  Printf.printf "Checked arithmetic: %d finite-only Mul/FMA statuses match reference/Core/strict CPU one/eight/JS; constants, helper fields, lazy arms and GPU refusal pass\n" count
