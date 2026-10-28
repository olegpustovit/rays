module E=Flow.Eval
module V=Flow.Value
module I=Flow_ir
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let bytes value=Marshal.to_string value[Marshal.No_sharing]
let scatter indices values size=
  let op=Option.get(Flow.Op.find "array/scatter_add" Flow.Context.value) in
  op.body ~live:(Input.at_time 0.) ~node:(fun _ _->assert false)
    ["indices",indices;"values",values;"size",size]
let reject code f=try ignore(f());failwith("accepted "^code)
  with Diagnostic.Error d -> assert(d.code=code)
let same a b=match a,b with
  |Ok a,Ok b->assert(bytes a=bytes b)
  |Error(a:Diagnostic.t),Error(b:Diagnostic.t)->
      assert(a.code=b.code && a.message=b.message && a.cause=b.cause)
  |_->let show=function Ok v->"Ok "^V.show_with Fun.id v|Error d->Diagnostic.to_string d in
      failwith("Scatter reference/compiled disagreement: "^show a^" / "^show b)
let workspace ?(declarations="") body=
  let text="(workspace scatter (graph g :context host "^declarations^" "^body^"))" in
  match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
    |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let value body=List.assoc "g" (ok(E.static(workspace("(if (> t -1000.) "^body^" "^body^")")))).results
let reference width indices values size =
  let result=Array.make(size*width)0. in
  Array.iteri(fun row index->for c=0 to width-1 do
    result.(index*width+c)<-result.(index*width+c)+.values.(row*width+c)
  done)indices;result
let ()=
  (* Exact signed63 wrapping, no float conversion of indices or values. *)
  assert(scatter(E.Int_array[|0;0;1;1|])
    (E.Int_array[|max_int;1;9007199254740993;min_int|])(E.Int 3)
    =E.Int_array[|min_int;9007199254740993+min_int;0|]);
  let input=E.Int_array[|1;2|]in
  let output=scatter(E.Int_array[|0;0|])input(E.Int 1)in
  (match input,output with E.Int_array original,E.Int_array result->
    assert(original!=result);result.(0)<-19;assert(original=[|1;2|])|_->assert false);
  let size=97 in
  List.iter(fun count->let indices=Array.init count(fun i->i mod size) in
    List.iter(fun width->let data=Array.init(count*width)(fun i->
      [|1e16;1.;-1e16;-0.;0.;Int64.float_of_bits 1L;-.Int64.float_of_bits 1L|].(i mod 7))in
      let wrap=match width with 1->(fun a->E.Float_array a)|2->(fun a->E.Vec2_array a)
        |3->(fun a->E.Vec3_array a)|_->(fun a->E.Vec4_array a) in
      let inputs=E.Int_array indices,wrap data in
      let before=bytes inputs in
      let expected=wrap(reference width indices data size) in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let got=scatter(fst inputs)(snd inputs)(E.Int size)in
        assert(bytes got=bytes expected);
        (match got with E.Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->
          assert(a!=data);if Array.length a>0 then a.(0)<-17.|_->assert false);
        assert(bytes inputs=before))) [1;4;8]) [1;2;3;4]) [0;1;13;32769];
  assert(scatter(E.Int_array[||])(E.Int_array[||])(E.Int 0)=E.Int_array[||]);
  List.iter(fun index->reject "E_ARRAY_RANGE"(fun()->scatter(E.Int_array[|index|])(E.Int_array[|1|])(E.Int 1)))
    [-1;1;9007199254740993;max_int];
  List.iter(fun size->reject "E_ARRAY_RANGE"(fun()->scatter(E.Int_array[||])(E.Int_array[||])(E.Int size)))
    [-1;Sys.max_array_length+1;max_int];
  reject "E_ARRAY_RANGE"(fun()->scatter(E.Int_array[||])(E.Vec4_array[||])
    (E.Int(Sys.max_floatarray_length/4+1)));
  reject "E_ARRAY_COUNT"(fun()->scatter(E.Int_array[|0|])(E.Int_array[||])(E.Int 1));
  reject "E_ARRAY_TYPE"(fun()->scatter(E.Float_array[|0.|])(E.Float_array[|1.|])(E.Int 1));
  reject "E_ARRAY_TYPE"(fun()->scatter(E.Int_array[||])(E.Text_array[||])(E.Int 1));
  reject "E_ARRAY_TYPE"(fun()->scatter(E.Int_array[|0|])(E.Vec3_array[|1.;2.|])(E.Int 1));
  reject "E_ARRAY_TYPE"(fun()->scatter(E.Int_array[||])(E.Int_array[||])(E.Float 1.));
  (* Error order is source-row, then component. A later invalid index cannot
     replace an earlier overflowing sum; an earlier index wins the reverse. *)
  reject "E_NONFINITE"(fun()->scatter(E.Int_array[|0;0;1|])
    (E.Float_array[|Float.max_float;Float.max_float;1.|])(E.Int 1));
  reject "E_ARRAY_RANGE"(fun()->scatter(E.Int_array[|1;0;0|])
    (E.Float_array[|1.;Float.max_float;Float.max_float|])(E.Int 1));
  List.iter(fun bad->reject "E_NONFINITE"(fun()->scatter(E.Int_array[|0|])(E.Float_array[|bad|])(E.Int 1)))
    [nan;infinity;neg_infinity];
  List.iter(fun body->let v=value body in
    let r=match v with E.Residual r->r|_->failwith "Scatter must remain live"in
    let core=match I.Core_lower.lower ~strict:true r with Ok p->p|Error form->failwith form in
    let executable=ok(I.Executor.compile ~strict:true v)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      List.iter(fun time->let live=Input.at_time time in
        let expected=E.Host.force_reference v ~live in
        same(Test_program.run core ~live) expected;
        same(I.Executor.force ~strict:true executable ~live) expected)[-1.;0.;0.25;2.]))[1;4;8])
    ["(array/scatter_add (map (fn [(i : int)] (int/mod i 97)) (array/range 32769 :type int)) (map (fn [x] (+ x t)) (array/range 32769)) 97)";
     "(array/scatter_add (array/int 13 0) (array/int 13 (int/add (frame/index) 9007199254740993)) 2)";
     "(let* [accumulate (fn [(ids : (array int)) (items : (array float)) (size : int)] (array/scatter_add ids items size))] (accumulate (array/int 13 0) (array/float 13 t) 2))";
     "(array/scatter_add (array/int 5001 0) (array/vec3 5001 [t -0. 1]) 3)";
     "(array/scatter_add (array/int 0) (array/float 0 t) 0)";
     "(if (> t 0.) (array/float 1) (array/scatter_add (array/int 1 -1) (array/float 1) 1))";
     "(array/scatter_add (array/int 3 0) (array/float 3 (* t 1.7976931348623157e308)) 1)";
     "(map (fn [x] (+ x t)) (array/scatter_add (array/range 5001 :type int) (array/float 5001 1.) 5001))"];
  let unsupported=value "(array/scatter_add (array/int 5001 0) (map (fn [x] (value/rand x t)) (array/range 5001)) 1)" in
  (match I.Executor.compile ~strict:true unsupported with Error d->assert(d.code="E_KERNEL_FORM")|Ok _->failwith "Unsupported scatter producer accepted");
  (* Variable-size scatter is a whole-array primitive, never a per-row GPU
     scatter instruction. Its invariant materialized result may feed kernels. *)
  let dynamic=value "(map (fn [(n : int)] (array/nth (array/scatter_add (array/int n 0) (array/float n t) 1) 0)) (array/range 5001 :type int))"in
  let r=match dynamic with E.Residual r->r|_->assert false in
  assert(Result.is_error(I.Packed.compile_result r(E.Residual.residual_view r).term));
  (* Sources resolve in authored argument order and retain type/cause failures.
     A changed host buffer across forces must not be memoized globally. *)
  let ws=workspace ~declarations:"[(indices : (array int) (array/int 0)) (data : (array float) (array/float 0))]"
    "(array/scatter_add indices data 2)" in
  let inputs=["g",["indices",E.Source("fixture/indices",Flow.Ty.Array Flow.Ty.Int,[]);
    "data",E.Source("fixture/values",Flow.Ty.Array Flow.Ty.Float,[])]]in
  let v=List.assoc "g"(ok(E.static ~inputs ws)).results in
  let executable=ok(I.Executor.compile ~strict:true v) in
  let data=ref(E.Float_array[|1.;2.;3.|]) and calls=ref[] in
  let resolve=function
    |E.Source("fixture/indices",_,_)->calls:= !calls@["indices"];Ok(E.Int_array[|0;1;0|])
    |E.Source("fixture/values",_,_)->calls:= !calls@["values"];Ok !data
    |_->assert false in
  List.iter(fun domain->Parallel.run ~domains:domain(fun()->
    List.iter(fun next->data:=E.Float_array next;calls:=[];
      let live=Input.at_time 1. in
      same(I.Executor.force ~strict:true ~resolve executable ~live)
        (E.Host.force_reference ~resolve v ~live);
      assert(!calls=["indices";"values";"indices";"values"]))[[|1.;2.;3.|];[|7.;8.;9.|]]))[1;4;8];
  let wrong=function E.Source("fixture/indices",_,_)->Ok(E.Float_array[|0.;1.;0.|])|_->assert false in
  same(I.Executor.force ~strict:true ~resolve:wrong executable ~live:(Input.at_time 1.))
    (E.Host.force_reference ~resolve:wrong v ~live:(Input.at_time 1.));
  let skipped_ws=workspace ~declarations:"[(indices : (array int) (array/int 0)) (data : (array float) (array/float 0))]"
    "(if (> t 0.) (array/float 2) (array/scatter_add indices data 2))"in
  let skipped=List.assoc "g"(ok(E.static ~inputs skipped_ws)).results in
  let skipped_program=ok(I.Executor.compile ~strict:true skipped)in
  let no_source _=failwith "Untaken scatter resolved its host source"in
  same(I.Executor.force ~strict:true ~resolve:no_source skipped_program ~live:(Input.at_time 1.))
    (E.Host.force_reference ~resolve:no_source skipped ~live:(Input.at_time 1.));
  let failed=function E.Source("fixture/indices",_,_)->Error(Diagnostic.error
      ~code:"E_DATA_SOURCE" ~cause:"scatter input cause" "missing indices")|_->assert false in
  same(I.Executor.force ~strict:true ~resolve:failed executable ~live:(Input.at_time 1.))
    (E.Host.force_reference ~resolve:failed v ~live:(Input.at_time 1.));
  let stateful=value "(let* [counter (state [s 0.] (+ s 1.))] (if (> t 0.) (array/scatter_add (array/int 1 -1) (array/float 1 counter) 1) (array/float 1 counter)))"in
  let reference_state=E.create_state() and compiled_state=E.create_state()in
  let executable=ok(I.Executor.compile ~strict:true stateful)in
  List.iteri(fun frame time->let live={(Input.at_time time)with frame;tick=frame}in
    same(I.Executor.force ~strict:true ~state:compiled_state executable ~live)
      (E.Host.force_reference ~state:reference_state stateful ~live);
    assert(bytes(E.Host.state_values reference_state)=bytes(E.Host.state_values compiled_state)))
    [-1.;1.;-1.;1.;-1.];
  print_endline "Scatter: ordered numeric bytes, Int wrap, malformed inputs, source ownership, lazy errors and strict Core/reference 1/4/8 domains"

let ()=if Array.exists((=)"--bench")Sys.argv then begin
  let count=1_000_000 and size=100_000 in
  let indices=Array.init count(fun i->i mod size)in
  let data=Array.init(count*3)(fun i->[|1e16;1.;-1e16;0.125;-0.;0.|].(i mod 6))in
  let values=E.Vec3_array data and expected=bytes(E.Vec3_array(reference 3 indices data size))in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    List.iter(fun(name,run)->
      let samples=Array.init 3(fun _->Gc.compact();let before=Gc.allocated_bytes()and start=Unix.gettimeofday()in
        let result=run()in let seconds=Unix.gettimeofday()-.start and allocated=Gc.allocated_bytes()-.before in
        assert(bytes result=expected);seconds,allocated)in
      Array.sort(fun(a,_)(b,_)->Float.compare a b)samples;
      let seconds,allocated=samples.(1)in
      Printf.printf "scatter name=%s count=%d size=%d domains=%d ordered=true seconds=%.6f allocated_bytes=%.0f ocaml=%s\n%!"
        name count size domains seconds allocated Sys.ocaml_version)
      ["reference",(fun()->E.Vec3_array(reference 3 indices data size));
       "primitive",(fun()->scatter(E.Int_array indices)values(E.Int size))]))[1;4;8]
end
