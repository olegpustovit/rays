module E=Flow.Eval
module V=Flow.Value
module I=Flow_ir
module P=I.Packed
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let bytes v=Marshal.to_string v [Marshal.No_sharing]
let source ty : Flow.Op.t={Flow.Op.noise3 with
  name="probe/table";packed_extension=None;
  signature={pos=[];opt=[];rest=None;kw=[]};out=(fun _->ty);
  shape=Flow.Op.Struct{splice=false};check=(fun _->());
  body=(fun ~live:_ ~node:_ _->Flow.Value.Source("probe/table",ty,[]))}
let fn ?(bindings="") ty body =
  let text="(workspace w (graph g :context value (let* [data (probe/table) "^bindings^" tested "^body^"] 0.0)))" in
  let ws=match Flow.Workspace.check ~ops:[source ty] {Flow.Catalog.version=1;kinds=[]}
    (ok(Flow.Syntax.parse text))with
    |Some ws,[]->ws|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
  match snd(List.hd(List.assoc ["g";"tested"](ok(E.static ~record:true ws)).records))with
  |E.Fn fn->fn|_->assert false
let instantiate ?table ?bindings ty body indices =
  let fn=fn ?bindings ty body in
  let fn=match table with None->fn|Some table->E.Host.map_function_bindings
    (function E.Source("probe/table",_,_)->table|v->v)fn in
  ok(E.Host.map_function ~signature:Flow.Ty.{params=[Int];result=Option.get(elem ty)} fn
    [E.Int_array indices])
let packed ?(fusion=true) value=match value with
  |E.Residual r->(match P.compile_result ~fusion r(E.Residual.residual_view r).term with
    |Ok p->p|Error ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)))
  |_->failwith "Expected a gathered residual"
let diagnostic=function Error (d:Diagnostic.t)->d|Ok _->failwith "Expected gather failure"
let same_error a b=let a=diagnostic a and b=diagnostic b in
  assert(a.Diagnostic.code=b.code && a.message=b.message && a.cause=b.cause)
let array ty count=match ty with
  |Flow.Ty.Int->E.Int_array(Array.init count(fun i->[|min_int;max_int;9007199254740993;0;-1|].(i mod 5)))
  |ty->let width=Option.get(Flow.Packed_ops.scalar_width ty)in
    let values=Array.init(count*width)(fun i->[|-0.;0.;Float.next_after 0. 1.;0.25; -0.75|].(i mod 5))in
    (match width with 1->E.Float_array values|2->Vec2_array values|3->Vec3_array values|_->Vec4_array values)
let recorded ty body=
  let text="(workspace w (graph g :context value (let* [data (probe/table) tested "^body^"] 0.0)))"in
  let ws=match Flow.Workspace.check ~ops:[source ty] Flow.Catalog.{version=1;kinds=[]}
    (ok(Flow.Syntax.parse text))with Some ws,[]->ws|_,ds->
      failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
  snd(List.hd(List.assoc["g";"tested"](ok(E.static ~record:true ws)).records))
let ()=
  List.iter(fun ty->let table=array ty 7 in
    List.iter(fun count->
      let value=instantiate ~table (Flow.Ty.Array ty)
        "(fn [(index : int)] (let* [alias data] (array/nth alias (int/mod index 7))))"
        (Array.init count Fun.id)in
      let reference=ok(E.Host.force_reference value ~live:(Input.at_time 0.25))in
      List.iter(fun fusion->let p=packed ~fusion value in
        assert(P.static_count p=Some count);
        assert(List.exists(fun d->d.Diagnostic.code="E_GPU_FORM")(P.gpu_refusals p));
        let executor=ok(I.Executor.compile ~strict:true value)in
        List.iter(fun domains->Parallel.run ~domains(fun()->
          assert(bytes(ok(P.force ~strict:true p ~live:(Input.at_time 0.25)))=bytes reference);
          assert(bytes(ok(I.Executor.force ~strict:true executor ~live:(Input.at_time 0.25)))=bytes reference))) [1;4;8]) [false;true])
      [0;1;13;32769]) [Flow.Ty.Int;Float;Vec2;Vec3;Vec4];
  let table=E.Int_array[|min_int;max_int;9007199254740993|]in
  let helper=instantiate ~table ~bindings:"lookup (fn [(table : (array int)) (index : int)] (array/nth table index))" (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (lookup data (int/mod index 3)))"
    (Array.init 5001 Fun.id)in
  assert(bytes(ok(P.force ~strict:true(packed helper)~live:(Input.at_time 0.)))=
    bytes(ok(E.Host.force_reference helper ~live:(Input.at_time 0.))));
  List.iter(fun index->let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data index))"[|index|]in
    same_error(P.force ~strict:true(packed value)~live:(Input.at_time 0.))
      (E.Host.force_reference value ~live:(Input.at_time 0.))) [-1;3;Sys.max_array_length+1;max_int];
  let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (if (int/lt index 0) 0 (array/nth data index)))"[|-1;0;1|]in
  assert(ok(P.force ~strict:true(packed value)~live:(Input.at_time 0.))=E.Int_array[|0;min_int;max_int|]);
  (* A later lane's earlier instruction must not replace the first lane's error. *)
  let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (let* [divisor (if (int/eq index 1) 0 1) before (int/div 1 divisor)] (array/nth data index)))"
    (Array.init 32769(fun i->if i=0 then 3 else 1))in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    same_error(P.force ~strict:true(packed value)~live:(Input.at_time 0.))
      (E.Host.force_reference value ~live:(Input.at_time 0.))))[1;4;8];
  let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data index))"(Array.init 32769(fun i->if i=4 then 3 else if i=20000 then -1 else 0))in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    same_error(P.force ~strict:true(packed value)~live:(Input.at_time 0.))
      (E.Host.force_reference value ~live:(Input.at_time 0.))))[1;4;8];
  (* Host tables resolve on the initial domain, once for the selected execution. *)
  let value=instantiate (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data (int/mod index 3)))"(Array.init 32769 Fun.id)in
  let calls=ref 0 and domain=Domain.self()in
  let resolve=function E.Source("probe/table",_,_)->assert(Domain.self()=domain);incr calls;Ok table
    |_->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Unexpected source")in
  List.iter(fun domains->calls:=0;Parallel.run ~domains(fun()->
    ignore(ok(P.force ~strict:true ~resolve(packed value)~live:(Input.at_time 0.))));assert(!calls=1))[1;8];
  let repeated=instantiate(Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (int/add (array/nth data 0) (array/nth data (int/mod index 3))))"(Array.init 5001 Fun.id)in
  calls:=0;ignore(ok(P.force ~strict:true ~resolve(packed repeated)~live:(Input.at_time 0.)));assert(!calls=1);
  let untouched=instantiate (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (if (int/lt index 0) 0 (array/nth data index)))"[|-1|]in
  calls:=0;ignore(ok(P.force ~strict:true ~resolve(packed untouched)~live:(Input.at_time 0.)));assert(!calls=0);
  let failing=function E.Source("probe/table",_,_)->Error(Diagnostic.error ~cause:"table first" ~code:"E_DATA_SOURCE" "Missing table")
    |_->assert false in
  let ordered=instantiate (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data (int/div index 0)))"[|0|]in
  same_error(P.force ~strict:true ~resolve:failing(packed ordered)~live:(Input.at_time 0.))
    (E.Host.force_reference ~resolve:failing ordered ~live:(Input.at_time 0.));
  (* Live term evaluation concretizes each let value, even an unused alias.
     That table position must remain ordered against an earlier/later error. *)
  List.iter(fun body->let value=instantiate(Flow.Ty.Array Flow.Ty.Int)body[|0|]in
    same_error(P.force ~strict:true ~resolve:failing(packed value)~live:(Input.at_time 0.))
      (E.Host.force_reference ~resolve:failing value ~live:(Input.at_time 0.)))
    ["(fn [(index : int)] (let* [alias data] 0))";
     "(fn [(index : int)] (let* [before (int/div index 0) alias data] (array/nth alias 0)))";
     "(fn [(index : int)] (let* [alias data after (int/div index 0)] (array/nth alias 0)))"];
  (* Handles cannot escape or be interpreted as scalar arithmetic in public regions. *)
  let region=packed value in
  let prepare=Array.find_index(function I.Core.Region.Array_prepare _->true|_->false)region.code |>Option.get in
  assert(Result.is_error(I.Core.Region.rebuild region ~outputs:[|[|prepare|]|]()));
  let input=Array.find_index(function I.Core.Region.Int_input _->true|_->false)region.code |>Option.get in
  let code=Array.copy region.code in
  code.(Array.length code-1)<-I.Core.Region.Int_gather(input,input);
  assert(Result.is_error(I.Core.Region.rebuild region ~code()));
  let code=Array.copy region.code in
  code.(prepare)<-I.Core.Region.Guarded([input,true],code.(prepare));
  assert(Result.is_error(I.Core.Region.rebuild region ~code()));
  let dependent=instantiate ~table (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth (array/int (int/add index 1)) 0))"[|0;1|]in
  (match dependent with E.Residual r->assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term))|_->assert false);
  let live=Input.at_time 0.25 in
  (* Table counts are independent of Zip/Product counts and ordered accumulator steps. *)
  List.iter(fun body->let value=recorded(Flow.Ty.Array Flow.Ty.Int)body in
    let p=packed value and core=ok(I.Executor.compile ~strict:true value)in
    List.iter(fun table->let resolve=function E.Source("probe/table",_,_)->Ok table|_->assert false in
      let expected=E.Host.force_reference ~resolve value ~live in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(bytes(ok(P.force ~strict:true ~resolve p ~live))=bytes(ok expected));
        assert(bytes(ok(I.Executor.force ~strict:true ~resolve core ~live))=bytes(ok expected))))[1;4;8])
      [table;E.Int_array[|9007199254740993;min_int;max_int|]])
    ["(for [i (array/range 5001 :type int) j (array/range 3 :type int)] (int/add (array/nth data (int/mod i 3)) (int/add j (frame/index))))";
     "(array/sum (map (fn [i] (int/add (array/nth data (int/mod i 3)) (frame/index))) (array/range 5001 :type int)))";
     "(fold [a (frame/index)] [i (array/range 5001 :type int)] (int/add a (array/nth data (int/mod i 3))))";
     "(scan [a (frame/index)] [i (array/range 5001 :type int)] (int/xor a (array/nth data (int/mod i 3))))"];
  (* Iteration skips exclude the body, while discarded sinks still execute it. *)
  let value=recorded(Flow.Ty.Array Flow.Ty.Int)
    "(for [i (array/range 13 :type int)] :skip [4] (if (int/eq i 4) (array/nth data -1) (int/add (array/nth data (int/mod i 3)) (frame/index))))"in
  calls:=0;
  assert(bytes(ok(P.force ~strict:true ~resolve(packed value)~live))=
    bytes(ok(E.Host.force_reference ~resolve value ~live)));
  let value=instantiate ~table:(E.Float_array[|0.;-0.;1.|])(Flow.Ty.Array Flow.Ty.Float)
    "(fn [(index : int)] (array/nth data index))"[|3|]in
  same_error(P.force_into ~strict:true ~column_source:(fun _ _->Ok None)~count:1
    ~outputs:[|[|None|]|](packed value)~live)(E.Host.force_reference value ~live);
  (* Trust boundaries retain source type, unresolved descriptor and finite errors. *)
  List.iter(fun returned->let resolve _=Ok returned in
    same_error(P.force ~strict:true ~resolve(packed ordered)~live)
      (E.Host.force_reference ~resolve ordered ~live))
    [E.Float_array[|1.|];E.Source("probe/table",Flow.Ty.Array Flow.Ty.Int,[])];
  let bad=instantiate(Flow.Ty.Array Flow.Ty.Float)
    "(fn [(index : int)] (array/nth data index))"[|0|]in
  List.iter(fun x->let resolve _=Ok(E.Float_array[|x|])in
    same_error(P.force ~strict:true ~resolve(packed bad)~live)
      (E.Host.force_reference ~resolve bad ~live))[nan;infinity;neg_infinity];
  let empty=instantiate(Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data index))"[||]in
  calls:=0;assert(ok(P.force ~strict:true ~resolve(packed empty)~live)=E.Int_array[||]);assert(!calls=0);
  (* Structured Core passes changing array arguments to the same compiled region. *)
  let forms=ok(Flow.Syntax.parse
    "(workspace w (graph g :context host [(table : (array int) (array/int 0))] (let* [nested {:table table}] (map (fn [i] (int/add (array/nth nested.table (int/mod i 3)) (frame/index))) (array/range 5001 :type int)))))")in
  let ws=match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}forms with Some ws,[]->ws|_->assert false in
  let value=List.assoc"g"(ok(E.static ~inputs:["g",["table",table]]ws)).results in
  let core=ok(I.Executor.compile ~strict:true value)in
  assert(bytes(ok(I.Executor.force ~strict:true core ~live))=bytes(ok(E.Host.force_reference value ~live)));
  let changing=recorded(Flow.Ty.Array Flow.Ty.Int)
    "(let* [table (array/int 3 (int/add 9007199254740993 (frame/index))) nested {:table table}] (map (fn [i] (array/nth nested.table (int/mod i 3))) (array/range 5001 :type int)))"in
  let program=ok(I.Executor.compile ~strict:true changing)in
  List.iter(fun frame->let live={live with frame;tick=frame}in
    assert(bytes(ok(I.Executor.force ~strict:true program ~live))=
      bytes(ok(E.Host.force_reference changing ~live))))[0;42;9007199254740993];
  let residual=match changing with E.Residual r->r|_->assert false in
  let template=Option.get(P.compile_template residual(E.Residual.residual_view residual).term)in
  let rebound=Option.get(P.rebind template residual)in
  assert(bytes(ok(P.force ~strict:true rebound ~live))=bytes(ok(E.Host.force_reference changing ~live)));
  (* An unsupported captured producer must never hide a tree-walker fallback. *)
  let value=recorded(Flow.Ty.Array Flow.Ty.Float)
    "(let* [table (map (fn [x] (value/rand x t)) (array/range 7))] (map (fn [i] (array/nth table i)) (array/range 7 :type int)))"in
  let p=packed value in
  (match P.force ~strict:true p ~live with Error d->
     assert(d.code="E_KERNEL_FORM" && d.message="Numeric node bodies must compile to packed kernels.")
   |Ok _->failwith "Unsupported table producer used reference evaluation");
  let borrowed=recorded(Flow.Ty.Array Flow.Ty.Float)
    "(map (fn [x] (array/nth data (int/mod (int x) 3))) (array/range 32769))"in
  let storage=Array.init(2*32769+1)(fun i->if i>=2 && i mod 2=0 then float((i-2)/2)else nan)in
  let original=bytes storage and output=Array.make 32769 0. in
  let resolve _=Ok(E.Float_array[|-0.;0.;0.25|])in
  ignore(ok(P.force_into ~strict:true ~resolve ~column_source:(fun _ _->Ok(Some[|P.{data=storage;offset=2;stride=2}|]))
    ~count:32769 ~outputs:[|[|Some output|]|](packed borrowed)~live));
  assert(bytes storage=original);
  assert(bytes(E.Float_array output)=bytes(ok(E.Host.force_reference ~resolve borrowed ~live)));
  let fold=recorded(Flow.Ty.Array Flow.Ty.Int)"(state [a 0] (int/add a 1))"in
  let bad=instantiate(Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth data index))"[|-1|]in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let state=E.create_state()and reference=E.create_state()in
    let before=E.state_stamp state and reference_before=E.state_stamp reference in
    let resolve state _=ignore(ok(E.Host.force_reference ~state fold ~live));Ok table in
    same_error(P.force ~strict:true ~state ~resolve:(resolve state)(packed bad)~live)
      (E.Host.force_reference ~state:reference ~resolve:(resolve reference)bad ~live);
    assert(E.state_stamp state=before && E.state_stamp reference=reference_before)))[1;8];
  print_endline "Numeric gather: independent tables, exact Int/vector bytes, lazy preparation, first-error order and one/four/eight domains pass"

let () =
  let live=Input.at_time 0.25 in
  List.iter(fun(ty,table,constant,scalar)->
    List.iter(fun count->
      let value=instantiate ~table (Flow.Ty.Array ty)
        ("(fn [(index : int)] (if (int/lt index 0) "^constant^" (array/nth data index)))")
        (Array.init count(fun i->if i mod 2=0 then -1 else 0))in
      let reference=ok(E.Host.force_reference value ~live)in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(bytes(ok(P.force ~strict:true(packed value)~live))=bytes reference))) [1;4;8])
      [0;1;1025;32769];
    List.iter(fun count->
      let value=recorded ty
        (Printf.sprintf "(map (fn [(index : int)] (if (int/ge index 0) data (error \"E_UNTAKEN\" \"unused\"))) (array/range %d :type int))"count)in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let calls=ref 0 and domain=Domain.self()in
        let resolve _=assert(Domain.self()=domain);incr calls;Ok scalar in
        let result=ok(P.force ~strict:true ~resolve(packed value)~live)in
        assert(!calls=1);
        assert(bytes result=bytes(ok(E.Host.force_reference ~resolve:(fun _->Ok scalar)value ~live)))))
        [1;4;8]) [1;1025])
    [Flow.Ty.Int,E.Int_array[|max_int|],string_of_int min_int,E.Int 9007199254740993;
     Flow.Ty.Float,E.Float_array[|Float.next_after 0. 1.|],"-0.0",E.Float(-0.);
     Flow.Ty.Text,E.Text_array[|"\255\000"|],"\"constant\"",E.Text "\128\000uniform"];
  print_endline "Typed one-lane copies: constants, initial-domain uniforms, signed zeros/exact Int/native bytes and one/four/eight domains pass"

let () =
  let live=Input.at_time 0.25 and table=E.Text_array[|"raw\255\000"|]in
  let body="(fn [(index : int)] (let* [wrapped (int/add index 4611686018427387903) recovered (int/sub wrapped 4611686018427387903) magnitude (abs recovered) bit (int/and (int/or (int/xor magnitude 0) 0) 1)] (if (int/lt recovered 0) (if (int/eq bit 0) \"even\255\000\" \"odd\128\000\") (array/nth data 0))))"in
  List.iter(fun count->
    let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Text)body
      (Array.init count(fun i->[|min_int;max_int;9007199254740993;-2;-1;0|].(i mod 6)))in
    let reference=ok(E.Host.force_reference value ~live)in
    List.iter(fun fusion->let p=packed ~fusion value in
      let code=p.code in
      let rec bare=function I.Core.Region.Guarded(_,body)->bare body|body->body in
      let boundary=Option.get(Array.find_index(fun instruction->match bare instruction with
        |I.Core.Region.Array_prepare _->true|_->false)code)in
      assert(boundary>0 && Array.exists(function I.Core.Region.Int_input _->true|_->false)(Array.sub code 0 boundary));
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(bytes(ok(P.force ~strict:true p ~live))=bytes reference)))[1;4;8]) [false;true])
    [0;1;1023;1024;1025;16383;16384;16385;32769];
  (* A malformed public-region input is still checked at its selected
     instruction, even when earlier total instructions have been batched. *)
  let template=packed(instantiate ~table(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (array/nth data 0))"[|0|])in
  let residual,term=template.sources.(0)in
  let term={term with Flow.Workspace.node=Flow.Workspace.Ref_binding("bad_storage",[])}in
  List.iter(fun count->let residual=E.Residual.with_bindings residual ~term
      ["bad_storage",E.Float_array(Array.make count 0.)]in
    List.iter(fun selected->
      let p=match I.Core.Region.rebuild template ~sources:[|residual,term|] ~widths:[|1|] ~uniforms:[||]
        ~code:[|I.Core.Region.Const(if selected then 1. else 0.);
          Guarded([0,true],Int_input 0);Text_const "safe\255\000"|] ~outputs:[|[|2|]|] ~count:(Some count) () with
        |Ok p->p|Error ds->
        failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        match P.force ~strict:true p ~live with
        |Ok result->assert(not selected && result=E.Text_array(Array.make count "safe\255\000"))
        |Error d->assert(selected && d.code="E_ARRAY_TYPE" && d.message="Kernel input changed its packed element type.")))[1;4;8])
      [false;true]) [1;32769];
  (* The later lane's earlier division may not beat this lane's lookup.
     Both block and worker chunk boundaries must keep the reference cause. *)
  List.iter(fun first->let value=instantiate ~table(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (let* [divisor (if (int/eq index 2) 0 1) before (int/div 1 divisor)] (array/nth data index)))"
    (Array.init 32769(fun i->if i=first then 1 else if i=first+1 then 2 else 0))in
    let reference=E.Host.force_reference value ~live in
    List.iter(fun fusion->List.iter(fun domains->Parallel.run ~domains(fun()->
      same_error(P.force ~strict:true(packed ~fusion value)~live)reference))[1;4;8]) [false;true])
    [0;1023;1024;16383;16384];
  print_endline "Typed total prefix: wrapping Int/native Text bytes, guarded storage checks and earliest-row block/chunk errors pass"

let () =
  let live=Input.at_time 0.25 and domain=Domain.self()in
  (* The first two blocks leave the table untaken. Later warmed blocks still
     contain inactive negative indices. Table contents and cardinality change
     across forces; no prepared snapshot may cross a force. *)
  let value=instantiate(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (if (int/lt index 0) \"safe\255\000\" (array/nth data index)))"
    (Array.init 32769(fun i->if i<2048 || i mod 5=0 then -1 else i mod 3))in
  let current=ref(E.Text_array[|"old\255\000";"old1";"old2"|])in
  List.iter(fun fusion->let p=packed ~fusion value in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      List.iteri(fun step table->current:=table;let calls=ref 0 in
        let live=Input.at_time(float step)in
        let resolve _=assert(Domain.self()=domain);incr calls;Ok !current in
        let result=P.force ~strict:true ~resolve p ~live in
        let reference=E.Host.force_reference ~resolve:(fun _->Ok !current)value ~live in
        assert(!calls=1);
        match result,reference with
        |Ok result,Ok reference->assert(bytes result=bytes reference)
        |Error _,Error _->same_error result reference
        |_->failwith "Prepared table crossed an execution boundary")
        [E.Text_array[|"old\255\000";"old1";"old2"|];E.Text_array[|"short"|];
         E.Text_array[|"new\128\000";"new1";"new2"|]]))[1;4;8]) [false;true];
  let fields=Flow.Ty.Record["a",Flow.Ty.Array Flow.Ty.Text;"b",Flow.Ty.Array Flow.Ty.Text]in
  let closure=fn fields
    "(fn [(index : int)] (let* [chosen (array/nth data.a index)] (if (int/eq index 1) (array/nth data.b 0) chosen)))"in
  let closure=E.Host.map_function_bindings(function
    |E.Source("probe/table",_,_)->E.Record[
        "a",E.Source("probe/a",Flow.Ty.Array Flow.Ty.Text,[]);
        "b",E.Source("probe/b",Flow.Ty.Array Flow.Ty.Text,[])]
    |value->value)closure in
  let advancing=recorded(Flow.Ty.Array Flow.Ty.Text)"(state [counter 0] (int/add counter 1))"in
  (* A is prepared in block 0. A late bad A index in block 1 must not be
     reported ahead of an earlier row's later B resolution/error/cause. *)
  List.iter(fun (early,late)->
    let value=ok(E.Host.map_function ~signature:Flow.Ty.{params=[Int];result=Text}
      closure [E.Int_array(Array.init 32769(fun i->if i=early then 1 else if i=late then 2 else 0))])in
    let resolve state calls=function
      |E.Source("probe/a",_,_)->assert(Domain.self()=domain);calls:="a"::!calls;Ok(E.Text_array[|"a0";"a1"|])
      |E.Source("probe/b",_,_)->assert(Domain.self()=domain);calls:="b"::!calls;
          ignore(ok(E.Host.force_reference ~state advancing ~live));
          Error(Diagnostic.error ~code:"E_LATE_TABLE" ~cause:"b\255\000cause" "B failed")
      |_->assert false in
    let reference_state=E.create_state()in
    let reference=E.Host.force_reference ~state:reference_state ~resolve:(resolve reference_state(ref[]))value ~live in
    List.iter(fun fusion->List.iter(fun domains->Parallel.run ~domains(fun()->
      let calls=ref []and state=E.create_state()in
      let before=E.state_stamp state and reference_before=E.state_stamp reference_state in
      same_error(P.force ~strict:true ~state ~resolve:(resolve state calls)(packed ~fusion value)~live)reference;
      assert(E.state_stamp state=before && E.state_stamp reference_state=reference_before);
      assert(List.rev !calls=(if early<late then ["a";"b"]else["a"]))))[1;4;8]) [false;true])
    [1100,1600;1600,1100;16383,16384;16384,16383];
  print_endline "Prepared table prefix: delayed forcing, per-force content/count changes, late bounds versus earlier table cause and one/four/eight domains pass"

let ()=if Array.exists((=)"--bench")Sys.argv then begin
  let count=1_000_000 and indices=Array.init 1_000_000 Fun.id in
  Printf.printf "Numeric gather: OCaml %s, direct strict Packed, block1024/chunk16384, million rows, median3; allocations observe the initial domain only\n%!"Sys.ocaml_version;
  List.iter(fun ty->let table=array ty 1021 in
    let value=instantiate ~table(Flow.Ty.Array ty)
      "(fn [(index : int)] (array/nth data (int/mod index 1021)))"indices in
    let p=packed value and width=Option.get(Flow.Packed_ops.scalar_width ty)in
    let native()=match table with
      |E.Int_array data->E.Int_array(Array.init count(fun i->data.(i mod 1021)))
      |_->let data=match table with E.Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->a|_->assert false in
        let output=Array.make(count*width)0. in
        for i=0 to count-1 do Array.blit data (i mod 1021*width) output(i*width)width done;
        match width with 1->E.Float_array output|2->Vec2_array output|3->Vec3_array output|_->Vec4_array output in
    let measure f=let samples=Array.init 3(fun _->Gc.full_major();let before=Gc.allocated_bytes()and started=Unix.gettimeofday()in
      ignore(f());Unix.gettimeofday()-.started,Gc.allocated_bytes()-.before)in
      Array.sort(fun(a,_)(b,_)->Float.compare a b)samples;samples.(1)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let run()=ok(P.force ~strict:true p ~live:(Input.at_time 0.25))in
      assert(bytes(run())=bytes(native()));
      let nt,nb=measure native and pt,pb=measure run in
      Printf.printf "%s domains%d native %.6f/%.0fB packed %.6f/%.0fB ratio %.3f\n%!"
        (Flow.Ty.to_string ty)domains nt nb pt pb(pt/.nt)))[1;4])
    [Flow.Ty.Int;Float;Vec2;Vec3;Vec4]
end

let () =
  let strings=[|"";"é日本😀";"nul\000tail";"\255\128";"\237\160\128";"quote\"\\";"\239\187\191BOM"|] in
  let table=E.Text_array strings and live=Input.at_time 0.25 in
  (* Nested one-lane guards change across blocks; the last lane alone takes
     the error arm, and untaken invalid reads must never execute. *)
  List.iter(fun count->
    let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Text)
      "(fn [(index : int)] (if (int/lt index 32768) (if (int/eq (int/mod index 2) 0) (array/nth data (int/mod index 7)) (if (int/ge index 0) \"odd\" (array/nth data -1))) (error \"E_LAST\" \"last lane\" :cause \"selected\")))"
      (Array.init count Fun.id)in
    let reference=E.Host.force_reference value ~live in
    List.iter(fun fusion->List.iter(fun domains->Parallel.run ~domains(fun()->
      let result=P.force ~strict:true(packed ~fusion value)~live in
      if count=32768 then assert(bytes(ok result)=bytes(ok reference))
      else same_error result reference))[1;4;8])[false;true]) [32768;32769];
  List.iter(fun count->
    let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Text)
      "(fn [(index : int)] (if (int/ge index 0) (array/nth data (int/mod index 7)) \"untaken\"))"
      (Array.init count Fun.id)in
    let reference=ok(E.Host.force_reference value ~live)in
    List.iter(fun fusion->let p=packed ~fusion value in
      assert(P.static_count p=Some count && P.gpu_refusals p<>[]);
      let core=ok(I.Executor.compile ~strict:true value)in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let got=ok(P.force ~strict:true p ~live)in
        assert(bytes got=bytes reference);
        assert(bytes(ok(I.Executor.force ~strict:true core ~live))=bytes reference);
        match got with E.Text_array output->assert(output!=strings);
          Array.iteri(fun i s->assert(s==strings.(i mod 7)))output|_->assert false))[1;4;8]) [false;true])
    [0;1;13;32769];
  let value=instantiate ~table ~bindings:"lookup (fn [(names : (array text)) (index : int)] (array/nth names index))"
    (Flow.Ty.Array Flow.Ty.Text)"(fn [(index : int)] (let* [alias data] (lookup alias (int/mod index 7))))"
    (Array.init 5001 Fun.id)in
  assert(bytes(ok(P.force ~strict:true(packed value)~live))=bytes(ok(E.Host.force_reference value ~live)));
  let closure=fn (Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int) (old : text)] (if (int/ge index 0) (array/nth data (int/mod index 7)) old))"in
  let closure=E.Host.map_function_bindings(function
    |E.Source("probe/table",_,_)->table|v->v)closure in
  let indices=Array.init 32769(fun i->if i mod 5=0 then -1 else i)in
  let old=Array.init 32769(fun i->strings.((i+2)mod 7))in
  let selected=ok(E.Host.map_function ~signature:Flow.Ty.{params=[Int;Text];result=Text}
    closure [E.Int_array indices;E.Text_array old])in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let got=ok(P.force ~strict:true(packed selected)~live)in
    assert(bytes got=bytes(ok(E.Host.force_reference selected ~live)));
    match got with E.Text_array output->assert(output!=old);
      Array.iteri(fun i s->assert(s==(if indices.(i)<0 then old.(i)else strings.(indices.(i)mod 7))))output
    |_->assert false))[1;4;8];
  List.iter(fun indices->let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (array/nth data index))"indices in
    List.iter(fun domains->Parallel.run ~domains(fun()->same_error
      (P.force ~strict:true(packed value)~live)(E.Host.force_reference value ~live)))[1;4;8])
    [[|-1|];[|7|];[|max_int|];Array.init 32769(fun i->if i=3 then 7 else if i=20000 then -1 else 0)];
  let value=instantiate ~table (Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (if (int/lt index 0) \"safe\" (array/nth data index)))"[|-1;0|]in
  assert(ok(P.force ~strict:true(packed value)~live)=E.Text_array[|"safe";""|]);
  let identity=recorded(Flow.Ty.Array Flow.Ty.Text)"(map (fn [(old : text)] old) data)"in
  assert(P.gpu_refusals(packed identity)<>[]);
  let calls=ref 0 and domain=Domain.self()in
  let resolve _=assert(Domain.self()=domain);incr calls;Ok table in
  List.iter(fun domains->Parallel.run ~domains(fun()->calls:=0;
    assert(ok(P.force ~strict:true ~resolve(packed identity)~live)=table);assert(!calls=1)))[1;8];
  let uniform=recorded Flow.Ty.Text
    "(map (fn [(index : int)] (if (int/ge index 0) data \"safe\")) (array/range 5001 :type int))"in
  calls:=0;let resolve _=assert(Domain.self()=domain);incr calls;Ok(E.Text strings.(3))in
  assert(ok(P.force ~strict:true ~resolve(packed uniform)~live)=E.Text_array(Array.make 5001 strings.(3)));assert(!calls=1);
  let bad=instantiate(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (array/nth data (int/div index 0)))"[|0|]in
  let failing _=Error(Diagnostic.error ~code:"E_DATA_SOURCE" ~cause:strings.(3) "Text source failed")in
  same_error(P.force ~strict:true ~resolve:failing(packed bad)~live)(E.Host.force_reference ~resolve:failing bad ~live);
  let error=instantiate ~table(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (if (int/lt index 0) (error \"E_TEXT\" \"selected\" :cause \"raw\255\000cause\") (array/nth data index)))"[|-1;0|]in
  List.iter(fun domains->Parallel.run ~domains(fun()->same_error
    (P.force ~strict:true(packed error)~live)(E.Host.force_reference error ~live)))[1;4;8];
  List.iter(fun returned->let resolve _=Ok returned in
    same_error(P.force ~strict:true ~resolve(packed bad)~live)
      (E.Host.force_reference ~resolve bad ~live))
    [E.Float_array[|0.|];E.Source("probe/table",Flow.Ty.Array Flow.Ty.Text,[])];
  let advancing=recorded(Flow.Ty.Array Flow.Ty.Text)"(state [a 0] (int/add a 1))"in
  let rollback=instantiate(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (array/nth data index))"[|-1|]in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let state=E.create_state()and reference=E.create_state()in
    let before=E.state_stamp state and reference_before=E.state_stamp reference in
    let resolve state _=ignore(ok(E.Host.force_reference ~state advancing ~live));Ok table in
    same_error(P.force ~strict:true ~state ~resolve:(resolve state)(packed rollback)~live)
      (E.Host.force_reference ~state:reference ~resolve:(resolve reference)rollback ~live);
    assert(E.state_stamp state=before && E.state_stamp reference=reference_before)))[1;8];
  let product=recorded(Flow.Ty.Array Flow.Ty.Text)
    "(for [i (array/range 5001 :type int) j (array/range 3 :type int)] :skip [4] (if (int/eq i 1) (if (int/eq j 1) (array/nth data -1) \"old\") (array/nth data (int/mod i 7))))"in
  let resolve _=Ok table in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    assert(bytes(ok(P.force ~strict:true ~resolve(packed product)~live))=
      bytes(ok(E.Host.force_reference ~resolve product ~live)))))[1;4;8];
  let changing=recorded(Flow.Ty.Array Flow.Ty.Text)
    "(let* [table (array/text 7 (if (> t 0.) \"é日本😀\" \"other\")) nested {:names table}] (map (fn [(index : int)] (array/nth nested.names (int/mod index 7))) (array/range 5001 :type int)))"in
  let program=ok(I.Executor.compile ~strict:true changing)in
  let residual=match changing with E.Residual r->r|_->assert false in
  let template=Option.get(P.compile_template residual(E.Residual.residual_view residual).term)in
  let rebound=Option.get(P.rebind template residual)in
  List.iter(fun time->let live=Input.at_time time in
    let expected=bytes(ok(E.Host.force_reference changing ~live))in
    assert(bytes(ok(I.Executor.force ~strict:true program ~live))=expected);
    assert(bytes(ok(P.force ~strict:true rebound ~live))=expected))[-1.;1.];
  let fused=recorded(Flow.Ty.Array Flow.Ty.Text)
    "(map (fn [(old : text)] (if (> t 0.) old \"other\")) (map (fn [(index : int)] (array/nth data (int/mod index 7))) (array/range 5001 :type int)))"in
  List.iter(fun fusion->List.iter(fun domains->Parallel.run ~domains(fun()->
    assert(bytes(ok(P.force ~strict:true ~resolve(packed ~fusion fused)~live))=
      bytes(ok(E.Host.force_reference ~resolve fused ~live)))))[1;4;8])[false;true];
  let middle=instantiate ~table(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (str (array/nth data index)))"[|0|]in
  (match middle with E.Residual r->assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term))|_->assert false);
  let value=instantiate ~table(Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int)] (array/nth data index))"[|0|]in
  let p=packed value in
  assert(Result.is_error(P.force_into ~strict:true p ~live ~count:1 ~outputs:[|[|None|]|]
    ~column_source:(fun _ _->Ok None)));
  let region=p in
  let output=region.outputs.(0).(0)in
  let code=Array.append region.code[|I.Core.Region.Unary(Flow.Packed_ops.Not,output)|]in
  assert(Result.is_error(I.Core.Region.rebuild region ~code ~outputs:[|[|Array.length code-1|]|]()));
  print_endline "Text gather: immutable byte leaves, identity/uniform/select/helper, strict Core and one/four/eight domains pass"

let ()=if Array.exists(fun arg->arg="--bench-text" || arg="--profile-text" || arg="--trace-text")Sys.argv then begin
  let count=1_000_000 and strings=[|"";"é日本😀";"a\000b";"\255\128"|]in
  let indices=Array.init 1_000_000(fun i->if i mod 5=0 then -1 else i mod 4)in
  let old=Array.init count(fun i->strings.((i+1)mod 4))in
  let closure=fn (Flow.Ty.Array Flow.Ty.Text)
    "(fn [(index : int) (old : text)] (if (int/ge index 0) (array/nth data index) old))"in
  let closure=E.Host.map_function_bindings(function
    |E.Source("probe/table",_,_)->E.Text_array strings|v->v)closure in
  let value=ok(E.Host.map_function ~signature:Flow.Ty.{params=[Int;Text];result=Text}
    closure [E.Int_array indices;E.Text_array old])in
  let p=packed value and live=Input.at_time 0.25 in
  if Array.exists((=)"--trace-text")Sys.argv then
    Format.printf "%a\n%!" I.Core.pp I.Core.{constants=[||];locals=[||];body=[];results=[||];
      state_slots=[||];regions=[|p|];call_slots=0};
  let native()=E.Text_array(Array.init count(fun i->if indices.(i)<0 then old.(i)else strings.(indices.(i))))in
  if Array.exists((=)"--profile-text")Sys.argv then Parallel.run ~domains:1(fun()->
    assert(bytes(ok(P.force ~strict:true p ~live))=bytes(native()));
    Gc.full_major();
    let before=Gc.allocated_bytes() and started=Unix.gettimeofday() and runs=ref 0 in
    Printf.printf "Text profile starts: pid%d, million rows, domains1\n%!"(Unix.getpid());
    while Unix.gettimeofday()-.started<8. do ignore(ok(P.force ~strict:true p ~live));incr runs done;
    Printf.printf "Text profile terminal: %d forces, %.6fs, %.0fB/force\n%!"
      !runs (Unix.gettimeofday()-.started) ((Gc.allocated_bytes()-.before)/.float !runs));
  if Array.exists((=)"--bench-text")Sys.argv then begin
  let measure f=let samples=Array.init 3(fun _->Gc.full_major();let before=Gc.allocated_bytes()and started=Unix.gettimeofday()in
    ignore(f());Unix.gettimeofday()-.started,Gc.allocated_bytes()-.before)in
    Array.sort(fun(a,_)(b,_)->Float.compare a b)samples;samples.(1)in
  Printf.printf "Text gather: OCaml %s, strict Packed Collect, block1024/chunk16384, million rows, median3; allocations observe the initial domain only\n%!"Sys.ocaml_version;
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let run()=ok(P.force ~strict:true p ~live)in
    assert(bytes(run())=bytes(native()));
    let nt,nb=measure native and pt,pb=measure run in
    Printf.printf "Text domains%d native %.17g/%.0fB packed %.17g/%.0fB ratio %.17g gate=%s\n%!"
      domains nt nb pt pb(pt/.nt)(if pt<=2.*.nt then "pass" else "fail")))[1;4]
  end
end

let ()=
  (* Inline invariant table choices retain selected-lane forcing and exact causes.
     They are compiled tables, not eager outer-let rewrites. *)
  let body="(fn [(index : int)] (if (int/lt index 0) 0 (array/nth (if (> t 0.) data (error \"E_TABLE\" \"selected inline table\" :cause \"raw\255\000cause\")) (int/mod index 3))))" in
  let domain=Domain.self() in
  List.iter(fun count->let value=instantiate (Flow.Ty.Array Flow.Ty.Int)body(Array.init count Fun.id)in
    List.iter(fun fusion->let p=packed ~fusion value in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        List.iter(fun time->let live=Input.at_time time and calls=ref 0 in
          let resolve=function E.Source("probe/table",_,_)->
            assert(Domain.self()=domain);incr calls;Ok(E.Int_array[|min_int;max_int;9007199254740993|])
            |_->assert false in
          let reference=E.Host.force_reference ~resolve value ~live in
          calls:=0;let actual=P.force ~strict:true ~resolve p ~live in
          (match reference,actual with Ok expected,Ok got->assert(bytes got=bytes expected)
            |Error _,Error _->same_error actual reference|_->assert false);
          assert(!calls=(if count=0 || time<0. then 0 else 1))) [0.25;-0.25])) [1;4;8]) [false;true]) [0;1;32769];
  let value=instantiate (Flow.Ty.Array Flow.Ty.Int)body[|-1|]in
  let resolve _=assert false in
  assert(ok(P.force ~strict:true ~resolve(packed value)~live:(Input.at_time(-0.25)))=E.Int_array[|0|]);
  let varying=instantiate (Flow.Ty.Array Flow.Ty.Int)
    "(fn [(index : int)] (array/nth (if (int/eq index 0) data (array/int 1 7)) 0))"[|0;1|]in
  (match varying with E.Residual r->assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term))|_->assert false)
