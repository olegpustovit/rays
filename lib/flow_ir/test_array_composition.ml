module C=Array_composition_cases
module E=Flow.Eval
module V=Flow.Value
module I=Flow_ir
module P=I.Packed
let packed=function Ok p->p|Error ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))
let call name args=(Option.get(Flow.Op.find name Flow.Context.value)).body
  ~live:(Input.at_time 0.) ~node:(fun _ _->assert false) args
let sample ty count=match ty with
 |Flow.Ty.Int->E.Int_array(Array.init count(fun i->[|min_int;max_int;9007199254740993;-1;0|].(i mod 5)))
 |Text->E.Text_array(Array.init count(fun i->if i mod 2=0 then C.all_bytes else "é日本😀\000"))
 |ty->let width=Option.get(Flow.Packed_ops.scalar_width ty)in
   let data=Array.init(count*width)(fun i->[|-0.;0.;Float.next_after 0. 1.;1.;-1.|].(i mod 5))in
   match ty with Float->E.Float_array data|Vec2->Vec2_array data|Vec3->Vec3_array data|Vec4->Vec4_array data|_->assert false
let copy_operation operation a b first count=match operation,a,b with
 |"concat",E.Int_array a,E.Int_array b->E.Int_array(Array.append a b)
 |"concat",Text_array a,Text_array b->Text_array(Array.append a b)
 |"concat",Float_array a,Float_array b->Float_array(Array.append a b)
 |"concat",Vec2_array a,Vec2_array b->Vec2_array(Array.append a b)
 |"concat",Vec3_array a,Vec3_array b->Vec3_array(Array.append a b)
 |"concat",Vec4_array a,Vec4_array b->Vec4_array(Array.append a b)
 |"slice",Int_array a,_->Int_array(Array.sub a first count)
 |"slice",Text_array a,_->Text_array(Array.sub a first count)
 |"slice",Float_array a,_->Float_array(Array.sub a first count)
 |"slice",Vec2_array a,_->Vec2_array(Array.sub a (2*first)(2*count))
 |"slice",Vec3_array a,_->Vec3_array(Array.sub a (3*first)(3*count))
 |"slice",Vec4_array a,_->Vec4_array(Array.sub a (4*first)(4*count))
 |_->assert false
let mutate=function
 |E.Int_array a->assert(Array.length a>0);a.(0)<-7
 |Text_array a->assert(Array.length a>0);a.(0)<-"changed"
 |Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->assert(Array.length a>0);a.(0)<-7.
 |_->assert false
let instantiated ty table body indices=
 let declarations="[(data : (array "^Flow.Ty.to_string ty^") "^C.constant ty "0"^")]"in
 let workspace=C.checked ~declarations ("(let* [tested (fn [(i : int)] "^body^")] 0.)")in
 let evaluation=C.ok(E.static ~record:true ~inputs:["g",["data",table]] workspace)in
 let fn=match snd(List.hd(List.assoc["g";"tested"]evaluation.records))with E.Fn fn->fn|_->assert false in
 C.ok(E.Host.map_function ~signature:Flow.Ty.{params=[Int];result=ty} fn [E.Int_array indices])
let ()=
 List.iter(fun ty->List.iter(fun count->let a=sample ty count and b=sample ty 13 in
  let before=C.bytes(a,b)in
  let concat=call "array/concat" ["first",a;"second",b]in
  assert(C.bytes concat=C.bytes(copy_operation "concat" a b 0 0));mutate concat;
  assert(C.bytes(a,b)=before);
  List.iter(fun(first,length)->let slice=call "array/slice"["array",a;"first",E.Int first;"count",E.Int length]in
    assert(C.bytes slice=C.bytes(copy_operation "slice" a b first length));
    if length>0 then mutate slice;assert(C.bytes(a,b)=before))
    [0,count;count,0;count/2,count-count/2]) [0;1;13;4097;5001;32769])C.types;
 let reject code f=try ignore(f());failwith("Accepted "^code)with
   Diagnostic.Error d -> assert(d.code=code) in
 List.iter(fun(first,count)->reject "E_ARRAY_RANGE"(fun()->call "array/slice"
  ["array",sample Flow.Ty.Int 13;"first",E.Int first;"count",E.Int count]))[-1,0;0,-1;14,0;13,1;12,2];
 List.iter(fun ty->if ty<>Flow.Ty.Int then reject "E_ARRAY_TYPE"(fun()->call "array/concat"
   ["first",sample Flow.Ty.Int 1;"second",sample ty 1]))C.types;
 List.iter(fun ty->List.iter(fun body->let table=sample ty 13 in
  List.iter(fun count->let value=instantiated ty table body(Array.init count(fun i->i mod 13))in
   let r=match value with E.Residual r->r|_->assert false in
   List.iter(fun fusion->let p=packed(P.compile_result ~fusion r(E.Residual.residual_view r).term)in
    assert(P.gpu_refusals p<>[]);
    let live=Input.at_time 0.25 and before=C.bytes table in
    List.iter(fun domains->Parallel.run ~domains(fun()->
     C.same(P.force ~strict:true p ~live)(E.Host.force_reference value ~live))) [1;4;8];
    assert(C.bytes table=before)) [false;true]) [0;1;13;4097;5001;32769])
  ["(array/nth (array/concat data data) i)";
   "(array/nth (array/slice data 0 13) i)"])C.types;
 List.iter(fun(name,body)->let value=C.value body in
  let residual,core=C.core value and executor=C.ok(I.Executor.compile ~strict:true value)in
  List.iter(fun count->List.iter(fun time->let live={(Input.at_time time)with frame=count;tick=count}in
   let reference=E.Host.force_reference value ~live in
   List.iter(fun domains->Parallel.run ~domains(fun()->
    C.same(Test_program.run ~root:residual ~strict:true core ~live)reference;
    C.same(I.Executor.force ~strict:true executor ~live)reference))[1;4;8])[-0.25;0.25]) [0;13;5001;32769];
  if String.ends_with ~suffix:"inline-concat" name || String.ends_with ~suffix:"inline-slice" name then
   assert(Array.exists(fun(r:I.Core.Region.t)->Array.exists(function
     |I.Core.Region.Array_prepare _|Guarded(_,Array_prepare _)->true|_->false)r.code)core.regions))
   (C.cases@C.error_cases);
 (* Invariant tables may capture live host data; no prepared array survives a
    force. Source errors/types/layouts and selected callback order stay native. *)
 List.iter(fun ty->let array_ty=Flow.Ty.Array ty in
  let text="(workspace sources (graph g :context host [(data : (array "^Flow.Ty.to_string ty^") "^C.constant ty "0"^")] "^
    "(map (fn [(i : int)] (array/nth (array/slice (array/concat data data) 0 (array/count data)) (int/mod i (array/count data)))) (array/range (frame/index) :type int))))"in
  let checked=match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}(C.ok(Flow.Syntax.parse text))with
   |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
  let value=List.assoc "g"(C.ok(E.static ~inputs:["g",["data",E.Source("fixture/table",array_ty,[])]] checked)).results in
  let residual=match value with E.Residual r->r|_->assert false in
  let current=ref(sample ty 13)in
  let resolve=function E.Source("fixture/table",_,_)->Ok !current|_->assert false in
  List.iter(fun fusion->let p=packed(P.compile_result ~fusion residual(E.Residual.residual_view residual).term)in
   assert(P.count_origin p=None && P.gpu_refusals p<>[]);
   List.iter(fun size->current:=sample ty size;let before=C.bytes !current in
    let live={(Input.at_time 0.25)with frame=(if size=0 then 0 else 32769)}in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      C.same(P.force ~strict:true ~resolve p ~live)(E.Host.force_reference ~resolve value ~live))) [1;4;8];
    assert(C.bytes !current=before)) [13;3;0;13];
   let live={(Input.at_time 0.25)with frame=13}in
   List.iter(fun resolve->C.same(P.force ~strict:true ~resolve p ~live)(E.Host.force_reference ~resolve value ~live))
    [(fun _->Error(Diagnostic.error ~code:"E_DATA_SOURCE" ~cause:"table cause" "missing table"));
     (fun _->Error(Diagnostic.error ~code:"E_CANCELLED" ~cause:"host cancel" "cancelled table"));
     (fun _->Ok(E.Int 0));(fun _->Ok(E.Vec3_array[|1.;2.|]));
     (fun _->Ok(E.Float_array[|nan|]))];
   let no_source _=failwith "Empty gather forced composed table"in
   C.same(P.force ~strict:true ~resolve:no_source p ~live:{live with frame=0})
     (E.Host.force_reference ~resolve:no_source value ~live:{live with frame=0})) [false;true];
  let dynamic body=List.assoc "g"(C.ok(E.static ~inputs:["g",["data",E.Source("fixture/table",array_ty,[])]]
    (C.checked ~declarations:("[(data : (array "^Flow.Ty.to_string ty^") "^C.constant ty "0"^")]") body))).results in
  let count_source _ (term:Flow.Workspace.term)=match term.node with
    |Ref_binding("data",[])->Some(["fixture";"data"],[])|_->None in
  List.iter(fun source->let value=dynamic("(map (fn [x] x) "^source^")")in
    let r=match value with E.Residual r->r|_->assert false in
    List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
      let origin=if source="data" && fusion then Some(["fixture";"data"],[])else None in
      assert(P.count_origin p=origin);
      current:=sample ty 13;C.same(P.force ~strict:true ~resolve p ~live:(Input.at_time 0.25))
        (E.Host.force_reference ~resolve value ~live:(Input.at_time 0.25))) [false;true])
    ["data";"(array/concat data data)";"(array/slice data 0 (array/count data))"];
  let value=dynamic "(map (fn [a b] a) data (array/concat data data))"in
  let r=match value with E.Residual r->r|_->assert false in
  List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
    assert(P.count_origin p=None && Array.length p.sources=2)) [false;true])C.types;
 (* Honest row dependence still refuses; no source equality is invented by
    composition, even when both maps have a host count-origin hook. *)
 List.iter(fun body->let v=instantiated Flow.Ty.Int (sample Flow.Ty.Int 13) body [|0;1;2|]in
  let r=match v with E.Residual r->r|_->assert false in
  assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term)))
 ["(array/nth (array/slice data i 1) 0)";
  "(array/nth (array/concat data (array/int i)) 0)"];
 List.iter(fun body->let value=instantiated Flow.Ty.Int
   (E.Source("fixture/order",Flow.Ty.Array Flow.Ty.Int,[])) body [|0;1;2|]in
   let r=match value with E.Residual r->r|_->assert false in
   let p=packed(P.compile_result r(E.Residual.residual_view r).term)in
   let calls=ref 0 in
   let resolve _=incr calls;Error(Diagnostic.error ~code:"E_DATA_SOURCE" ~cause:"order cause" "bad table")in
   let live=Input.at_time 0.25 in
   C.same(P.force ~strict:true ~resolve p ~live)(E.Host.force_reference ~resolve value ~live);
   if String.starts_with ~prefix:"(if" body then assert(!calls=0))
 ["(array/nth (array/concat data data) (error \"E_INDEX\" \"bad index\" :cause \"index cause\"))";
  "(array/nth (array/slice data (error \"E_FIRST\" \"bad first\") 1) (error \"E_INDEX\" \"bad index\"))";
  "(let* [unused (array/concat data data)] (error \"E_INDEX\" \"after alias\"))";
  "(if (int/lt i 0) (array/nth (array/concat data data) 0) i)"];
 (* Two separately checked closures capture the same spelling. Supply the
    helper through the existing callable-binding API: graph-local shadowing,
    Fn graph inputs and escaping Fn results are correctly refused. *)
 List.iter(fun(helper_body,expected)->
  List.iter(fun count->let value=C.helper_value helper_body count in
   let r=match value with E.Residual r->r|_->assert false in
   let _,core=C.core value and executor=C.ok(I.Executor.compile ~strict:true value)in
   List.iter(fun fusion->let p=packed(P.compile_result ~fusion r(E.Residual.residual_view r).term)in
    List.iter(fun time->let live=Input.at_time time in
     let reference=E.Host.force_reference value ~live in
     assert(C.bytes(C.ok reference)=C.bytes(E.Int_array(Array.make count(expected time))));
     List.iter(fun domains->Parallel.run ~domains(fun()->
       C.same(P.force ~strict:true p ~live)reference;
       C.same(Test_program.run ~root:r ~strict:true core ~live)reference;
       C.same(I.Executor.force ~strict:true executor ~live)reference)) [1;8]) [-0.25;0.25]) [false;true]) [0;13;32769])
  ["(array/nth (array/concat data data) j)",(fun _->9);
   "(array/nth (if (> t 0.) data (array/int 1 11)) 0)",(fun time->if time>0. then 9 else 11);
   "(array/count (array/concat data data))",(fun _->18)];
 let stateful=C.value "(let* [counter (state [s 0] (int/add s 1))] (array/slice (array/concat (array/int 1 counter) (array/int 1)) (if (> t 0.) 3 0) 1))"in
 let executor=C.ok(I.Executor.compile ~strict:true stateful)and a=E.create_state()and b=E.create_state()in
 List.iteri(fun frame time->let live={(Input.at_time time)with frame;tick=frame}in
  C.same(I.Executor.force ~strict:true ~state:a executor ~live)(E.Host.force_reference ~state:b stateful ~live);
  assert(C.bytes(E.Host.state_values a)=C.bytes(E.Host.state_values b))) [-1.;1.;-1.;1.;-1.];
 print_endline "C6 composition: six storages, Core/strict gather, selected errors, fresh captures and exact domains1/4/8"
