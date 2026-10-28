module E=Flow.Eval
module V=Flow.Value
module I=Flow_ir
module P=I.Packed
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let packed result=match result with Ok p->p|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let bytes value=Marshal.to_string value[Marshal.No_sharing]
let call name array=
  let op=Option.get(Flow.Op.find name Flow.Context.value)in
  let key=if name="array/compact"then "mask"else "array"in
  op.body ~live:(Input.at_time 0.) ~node:(fun _ _->assert false)[key,array]
let reject code f=try ignore(f());failwith("accepted "^code)
  with Diagnostic.Error d -> assert(d.code=code)
let same a b=match a,b with
  |Ok a,Ok b->assert(bytes a=bytes b)
  |Error(a:Diagnostic.t),Error(b:Diagnostic.t)->
      assert(a.code=b.code && a.message=b.message && a.cause=b.cause)
  |_->let show=function Ok v->"Ok "^V.show_with Fun.id v|Error d->Diagnostic.to_string d in
      failwith("Compaction/scan reference disagreement: "^show a^" / "^show b)
let workspace ?(declarations="") body=
  let text="(workspace scan (graph g :context host "^declarations^" "^body^"))"in
  match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
  |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let live_value body=List.assoc "g"(ok(E.static(workspace("(if (> t -1000.) "^body^" "^body^")")))).results
let compact mask=Array.to_list(Array.mapi(fun i n->i,n)mask)
  |>List.filter_map(fun(i,n)->if n=1 then Some i else None)|>Array.of_list
let scan values=let sum=ref 0 in
  Array.append[|0|](Array.map(fun n->sum:= !sum+n;!sum)values)
let ()=
  List.iter(fun count->List.iter(fun pattern->
    let mask=Array.init count pattern in
    let input=E.Int_array mask and original=Array.copy mask in
    let expected=E.Int_array(compact mask)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let actual=call "array/compact" input in assert(bytes actual=bytes expected);
      (match actual with E.Int_array a->assert(a!=mask);if Array.length a>0 then a.(0)<-999|_->assert false);
      assert(mask=original))) [1;4;8])[(fun _->0);(fun _->1);(fun i->if i mod 3=0 then 1 else 0)])
    [0;1;13;32769];
  List.iter(fun count->let values=Array.init count(fun i->[|max_int;1;min_int;-1;9007199254740993;0;7|].(i mod 7))in
    let original=Array.copy values and expected=E.Int_array(scan values)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let actual=call "array/scan_sum"(E.Int_array values)in assert(bytes actual=bytes expected);
      (match actual with E.Int_array a->assert(a!=values && Array.length a=count+1);a.(0)<-999|_->assert false);
      assert(values=original))) [1;4;8])[0;1;13;32769];
  assert(call "array/compact"(E.Int_array[||])=E.Int_array[||]);
  assert(call "array/scan_sum"(E.Int_array[||])=E.Int_array[|0|]);
  assert(call "array/scan_sum"(E.Int_array[|3;0;2|])=E.Int_array[|0;3;3;5|]);
  assert(call "array/scan_sum"(E.Int_array[|max_int;1;min_int|])=E.Int_array[|0;max_int;min_int;0|]);
  List.iter(fun mask->reject "E_ARRAY_MASK"(fun()->call "array/compact"(E.Int_array mask)))
    [[|2|];[|-1|];[|1;0;max_int|];Array.init 32769(fun i->if i=32768 then min_int else 1)];
  List.iter(fun name->List.iter(fun bad->reject "E_ARRAY_TYPE"(fun()->call name bad))
    [E.Float_array[||];E.Float_array[|0.;1.|];E.Vec3_array[|0.;1.|];E.Text_array[|"0"|];E.Int 1;E.Bool true;E.List[|E.Int 1|]])
    ["array/compact";"array/scan_sum"];
  (* No huge array is fabricated to exercise the allocation bound. The guard
     precedes count+1 and allocation; runtime proof uses real small/large tails. *)
  List.iter(fun body->let value=live_value body in
    let r=match value with E.Residual r->r|_->failwith "Expected live array"in
    let core=match I.Core_lower.lower ~strict:true r with Ok p->p|Error form->failwith form in
    let executor=ok(I.Executor.compile ~strict:true value)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      List.iter(fun time->let live={(Input.at_time time)with frame=3}in
        let reference=E.Private.force_reference value ~live in
        same(Test_program.run core ~live)reference;
        same(I.Executor.force ~strict:true executor ~live)reference)[-1.;0.;0.25;2.]))[1;4;8])
    ["(array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range 32769 :type int)))";
     "(array/scan_sum (map (fn [(i : int)] (int/add i (frame/index))) (array/range 32769 :type int)))";
     "(array/scan_sum (array/int 13 9007199254740993))";
     "(array/scan_sum (array/int 0))";
     "(array/compact (array/int 0))";
     "(if (> t 0.) (array/int 1) (array/compact (array/int 1 2)))";
     "(array/compact (array/int 13 (if (> t 0.) 1 2)))";
     "(let* [collect (fn [(mask : (array int))] (array/compact mask))] (collect (array/int 5001 1)))";
     "(let* [kept (array/compact (map (fn [(i : int)] (if (int/eq (int/mod i 3) 0) 1 0)) (array/range 32769 :type int))) table (array/range 32769)] (map (fn [(i : int)] (array/nth table i)) kept))";
     "(map (fn [(i : int)] (int/add i (frame/index))) (array/scan_sum (array/int 5001 1)))"];
  let unsupported=live_value "(array/compact (map (fn [x] (int (value/rand x t))) (array/range 5001)))"in
  (match I.Executor.compile ~strict:true unsupported with Error d->assert(d.code="E_KERNEL_FORM")|_->failwith "Unsupported producer accepted");
  List.iter(fun operation->let v=live_value("(map (fn [(n : int)] (array/count ("^operation^" (array/int n 1)))) (array/range 5001 :type int))")in
    let r=match v with E.Residual r->r|_->assert false in
    assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term))) ["array/compact";"array/scan_sum"];
  (* Only the actual host array has its origin. Compact changes length from
     mask data and scan has n+1 offsets, so derived maps cannot inherit it. *)
  let inputs=["g",["data",E.Source("fixture/mask",Flow.Ty.Array Flow.Ty.Int,[])]]in
  let dynamic body=List.assoc "g"(ok(E.static ~inputs(workspace
    ~declarations:"[(data : (array int) (array/int 0))]" body))).results in
  let count_source _ (term:Flow.Workspace.term)=match term.node with
    |Ref_binding("data",[])->Some(["fixture";"data"],[])|_->None in
  List.iter(fun operation->let source=if operation="direct"then "data"else "("^operation^" data)"in
    let value=dynamic("(map (fn [(i : int)] (int/add i (frame/index))) "^source^")")in
    let r=match value with E.Residual r->r|_->assert false in
    List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
      let expected=if operation="direct" && fusion then Some(["fixture";"data"],[])else None in
      if P.count_origin p<>expected then
        failwith(Printf.sprintf "Origin %s fusion=%b: %s" operation fusion
          (Option.fold ~none:"None" ~some:(fun(path,iter)->String.concat "/" path^":"^String.concat ","(List.map string_of_int iter))(P.count_origin p)));
      let current=ref[|1;0;1|]in
      let resolve=function E.Source("fixture/mask",_,_)->Ok(E.Int_array !current)|_->assert false in
      List.iter(fun data->current:=data;let live={(Input.at_time 1.)with frame=3}in
        List.iter(fun domains->Parallel.run ~domains(fun()->
          same(P.force ~strict:true ~resolve p ~live)(E.Private.force_reference ~resolve value ~live))) [1;4;8])
        [[|1;0;1|];[|0;1;0;1;1|];[||]]) [false;true]) ["direct";"array/compact";"array/scan_sum"];
  List.iter(fun operation->let value=dynamic("(map (fn [(a : int) (b : int)] (int/add a b)) data ("^operation^" data))")in
    let r=match value with E.Residual r->r|_->assert false in
    List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
      assert(P.count_origin p=None && P.static_count p=None && Array.length p.sources=2);
      let resolve=function E.Source("fixture/mask",_,_)->Ok(E.Int_array[|1;0;1;0|])|_->assert false in
      same(P.force ~strict:true ~resolve p ~live:(Input.at_time 1.))
        (E.Private.force_reference ~resolve value ~live:(Input.at_time 1.))) [false;true]) ["array/compact";"array/scan_sum"];
  List.iter(fun operation->let value=dynamic("("^operation^" data)")in
    let executor=ok(I.Executor.compile ~strict:true value)in
    let missing _=Error(Diagnostic.error ~code:"E_DATA_SOURCE" ~cause:"scan source cause" "missing mask")in
    same(I.Executor.force ~strict:true ~resolve:missing executor ~live:(Input.at_time 1.))
      (E.Private.force_reference ~resolve:missing value ~live:(Input.at_time 1.));
    let wrong _=Ok(E.Float_array[|1.|])in
    same(I.Executor.force ~strict:true ~resolve:wrong executor ~live:(Input.at_time 1.))
      (E.Private.force_reference ~resolve:wrong value ~live:(Input.at_time 1.));
    let skipped=dynamic("(if (> t 0.) (array/int 0) ("^operation^" data))")in
    let no_source _=failwith "Untaken scan resolved Source"in
    same(I.Executor.force ~strict:true ~resolve:no_source(ok(I.Executor.compile ~strict:true skipped))~live:(Input.at_time 1.))
      (E.Private.force_reference ~resolve:no_source skipped ~live:(Input.at_time 1.))) ["array/compact";"array/scan_sum"];
  let stateful=live_value "(let* [counter (state [s 0] (int/add s 1))] (if (> t 0.) (array/compact (array/int 1 2)) (array/scan_sum (array/int 1 counter))))"in
  let a=E.create_state()and b=E.create_state()in
  let executor=ok(I.Executor.compile ~strict:true stateful)in
  List.iteri(fun frame time->let live={(Input.at_time time)with frame;tick=frame}in
    same(I.Executor.force ~strict:true ~state:a executor ~live)(E.Private.force_reference ~state:b stateful ~live);
    assert(bytes(E.Private.state_values a)=bytes(E.Private.state_values b))) [-1.;1.;-1.;1.;-1.];
  (* A state-reading map is a reference kernel by design: strict compilation accepts it and matches the reference. *)
  let reader=live_value "(let* [counter (state [s 0.] (+ s 1.))] (map (fn [x] (+ x counter)) (array/range 4)))"in
  let a=E.create_state()and b=E.create_state()in
  let executor=ok(I.Executor.compile ~strict:true reader)in
  List.iter(fun frame->let live={(Input.at_time 1.)with frame;tick=frame}in
    same(I.Executor.force ~strict:true ~state:a executor ~live)(E.Private.force_reference ~state:b reader ~live);
    assert(bytes(E.Private.state_values a)=bytes(E.Private.state_values b))) [0;1;2];
  print_endline "Compact/scan: kept indices, exclusive offset boundaries, Int wrap, immutable sources, lazy errors and strict/reference domains"
