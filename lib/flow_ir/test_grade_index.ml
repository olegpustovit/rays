module E=Flow.Eval
module V=Flow.Value
module I=Flow_ir
module P=I.Packed
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let packed=function Ok p->p|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let bytes value=Marshal.to_string value[Marshal.No_sharing]
let call name args=(Option.get(Flow.Op.find name Flow.Context.value)).body
  ~live:(Input.at_time 0.) ~node:(fun _ _->assert false)args
let grade keys descending=call "array/grade"["keys",keys;"descending",E.Bool descending]
let lookup keys queries=call "array/index_of"["keys",keys;"queries",queries]
let reject code f=try ignore(f());failwith("accepted "^code)
  with Diagnostic.Error d -> assert(d.code=code)
let same a b=match a,b with
  |Ok a,Ok b->assert(bytes a=bytes b)
  |Error(a:Diagnostic.t),Error(b:Diagnostic.t)->
      assert(a.code=b.code && a.message=b.message && a.cause=b.cause)
  |_->let show=function Ok v->"Ok "^V.show_with Fun.id v|Error d->Diagnostic.to_string d in
      failwith("Grade/lookup reference disagreement: "^show a^" / "^show b)
let workspace ?(declarations="") body=
  let text="(workspace ordering (graph g :context host "^declarations^" "^body^"))"in
  match Flow.Workspace.check Flow.Check.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
  |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let live_value body=List.assoc "g"(ok(E.static(workspace("(if (> t -1000.) "^body^" "^body^")")))).results
let oracle_grade compare count descending=
  List.init count Fun.id |> List.stable_sort(if descending then fun a b->compare b a else compare)
  |>Array.of_list
let oracle_lookup equal keys queries=Array.map(fun query->
  let rec find i=if i=Array.length keys then -1 else if equal keys.(i)query then i else find(i+1)in find 0)queries
let raw=[|"";"a";"a";"x\000y";"\255\128";"é日本😀";"\240\144\128\128";"\238\128\128";String.init 256 Char.chr|]
let ()=
  assert(grade(E.Int_array[|2;1;2|])false=E.Int_array[|1;0;2|]);
  assert(grade(E.Int_array[|2;1;2|])true=E.Int_array[|0;2;1|]);
  assert(grade(E.Float_array[|-0.;0.;-0.;0.|])true=E.Int_array[|0;1;2;3|]);
  List.iter(fun count->
    let integers=Array.init count(fun i->[|min_int;max_int;9007199254740993;9007199254740992;0;-1;1|].(i mod 7))in
    let floats=Array.init count(fun i->[|-0.;0.;Int64.float_of_bits 1L;-.Float.max_float;Float.max_float;1.;-1.|].(i mod 7))in
    let texts=Array.init count(fun i->raw.(i mod Array.length raw))in
    List.iter(fun(keys,compare)->let before=bytes keys in
      List.iter(fun descending->let expected=E.Int_array(oracle_grade compare count descending)in
        List.iter(fun domains->Parallel.run ~domains(fun()->
          let got=grade keys descending in assert(bytes got=bytes expected);
          (match got with E.Int_array a->if Array.length a>0 then a.(0)<-999|_->assert false);
          assert(bytes keys=before)))[1;4;8])[false;true])
      [E.Int_array integers,(fun a b->Int.compare integers.(a)integers.(b));
       E.Float_array floats,(fun a b->Float.compare floats.(a)floats.(b));
       E.Text_array texts,(fun a b->String.compare texts.(a)texts.(b))]) [0;1;13;32769];
  (* Byte order differs from UTF-16 order for these two valid Unicode strings. *)
  assert(grade(E.Text_array[|"\240\144\128\128";"\238\128\128"|])false=E.Int_array[|1;0|]);
  let all=Array.init 256(fun i->String.make 1(Char.chr(255-i)))in
  assert(grade(E.Text_array all)false=E.Int_array(Array.init 256(fun i->255-i)));
  List.iter(fun count->let keys=Array.init count(fun i->if i mod 3=0 then min_int else 9007199254740993)in
    let queries=Array.init(count+7)(fun i->[|max_int;min_int;9007199254740993;9007199254740992|].(i mod 4))in
    (* These repeated-key fixtures have all first occurrences in their initial
       period; keep the independent linear oracle bounded for the large tail. *)
    let text_keys=Array.init count(fun i->raw.(i mod Array.length raw))and text_queries=Array.append raw all in
    List.iter(fun(k,q,expected)->let original=bytes(k,q)in
      List.iter(fun domains->Parallel.run ~domains(fun()->let got=lookup k q in
        assert(bytes got=bytes(E.Int_array expected));
        (match got with E.Int_array a->if Array.length a>0 then a.(0)<-999|_->assert false);
        assert(bytes(k,q)=original)))[1;4;8])
      [E.Int_array keys,E.Int_array queries,oracle_lookup Int.equal (Array.sub keys 0 (min count 3)) queries;
       E.Text_array text_keys,E.Text_array text_queries,oracle_lookup String.equal (Array.sub text_keys 0 (min count (Array.length raw))) text_queries]) [0;1;13;32769];
  assert(lookup(E.Text_array[|"a";"a";"b"|])(E.Text_array[|"b";"a";"missing"|])=E.Int_array[|2;0;-1|]);
  List.iter(fun x->reject "E_NONFINITE"(fun()->grade(E.Float_array[|0.;x|])false))[nan;infinity;neg_infinity];
  List.iter(fun bad->reject "E_ARRAY_TYPE"(fun()->grade bad false))
    [E.Vec3_array[|0.;0.;0.|];E.Int 1;E.Text "a";E.List[|E.Int 1|]];
  List.iter(fun(a,b)->reject "E_ARRAY_TYPE"(fun()->lookup a b))
    [E.Int_array[||],E.Text_array[||];E.Float_array[||],E.Float_array[||];E.Int 1,E.Int_array[||]];
  List.iter(fun body->let value=live_value body in
    let r=match value with E.Residual r->r|_->failwith "Live ordering expected"in
    let core=match I.Core_lower.lower ~strict:true r with Ok p->p|Error form->failwith form in
    let executor=ok(I.Executor.compile ~strict:true value)in
    List.iter(fun domains->Parallel.run ~domains(fun()->List.iter(fun time->
      let live={(Input.at_time time)with frame=3}in
      let expected=E.Private.force_reference value ~live in
      same(Test_program.run core ~live)expected;
      same(I.Executor.force ~strict:true executor ~live)expected)[-1.;0.;0.25;2.]))[1;4;8])
    ["(array/grade (map (fn [(i : int)] (int/mod (int/add i (frame/index)) 7)) (array/range 32769 :type int)) :descending true)";
     "(array/grade (map (fn [x] (if (> t 0.) x (- 0. x))) (array/range 32769)))";
     "(array/grade (map (fn [(x : text)] (if (> t 0.) x \"safe\")) (array/text 32769 \"é日本😀\")) :descending true)";
     "(array/index_of (array/int 3 9007199254740993) (map (fn [(i : int)] (if (int/eq i 0) 9007199254740993 i)) (array/range 32769 :type int)))";
     "(array/index_of (array/text 2 \"a\") (map (fn [(x : text)] (if (> t 0.) x \"missing\")) (array/text 32769 \"a\")))";
     "(array/grade (array/text 0))";
     "(array/index_of (array/int 0) (array/int 13 1))";
     "(let* [rank (fn [(keys : (array int))] (array/grade keys :descending true))] (rank (array/int 13 (frame/index))))";
     "(let* [table (array/range 32769) order (array/grade table :descending true)] (map (fn [(i : int)] (array/nth table i)) order))";
     "(map (fn [(i : int)] (int/add i (frame/index))) (array/index_of (array/int 2 3) (array/range 5001 :type int)))";
     "(if (> t 0.) (array/int 0) (array/grade (error \"E_KEYS\" \"bad keys\" :cause \"key cause\")))";
     "(array/index_of (error \"E_KEYS\" \"keys first\" :cause \"raw cause\") (error \"E_QUERIES\" \"queries second\"))"];
  let unsupported=live_value "(array/grade (map (fn [x] (value/rand x t)) (array/range 5001)))"in
  (match I.Executor.compile ~strict:true unsupported with Error d->assert(d.code="E_KERNEL_FORM")|_->failwith "Unsupported producer accepted");
  List.iter(fun body->let value=live_value("(map (fn [(i : int)] "^body^") (array/range 5001 :type int))")in
    let r=match value with E.Residual r->r|_->assert false in
    assert(Result.is_error(P.compile_result r(E.Residual.residual_view r).term)))
    ["(array/nth (array/grade (array/int i)) 0)";"(array/nth (array/index_of (array/int i) (array/int 1 i)) 0)"];
  let declarations="[(keys : (array int) (array/int 0)) (queries : (array int) (array/int 0))]"in
  let dynamic body=List.assoc "g"(ok(E.static ~inputs:["g",[
    "keys",E.Source("fixture/keys",Flow.Ty.Array Flow.Ty.Int,[]);
    "queries",E.Source("fixture/queries",Flow.Ty.Array Flow.Ty.Int,[])]](workspace ~declarations body))).results in
  let count_source _ (term:Flow.Workspace.term)=match term.node with
    |Ref_binding("keys",[])->Some(["fixture";"keys"],[])
    |Ref_binding("queries",[])->Some(["fixture";"queries"],[])|_->None in
  List.iter(fun operation->let value=dynamic("(map (fn [(i : int)] (int/add i (frame/index))) "^operation^")")in
    let r=match value with E.Residual r->r|_->assert false in
    List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
      assert(P.count_origin p=None);
      let current=ref([|2;1;2|],[|2;9;1;2|])in
      let resolve=function E.Source("fixture/keys",_,_)->Ok(E.Int_array(fst !current))
        |E.Source("fixture/queries",_,_)->Ok(E.Int_array(snd !current))|_->assert false in
      List.iter(fun buffers->current:=buffers;let before=bytes buffers in
        List.iter(fun domains->Parallel.run ~domains(fun()->let live={(Input.at_time 1.)with frame=3}in
          same(P.force ~strict:true ~resolve p ~live)(E.Private.force_reference ~resolve value ~live);
          assert(bytes buffers=before)))[1;4;8])
        [[|2;1;2|],[|2;9;1;2|];[|min_int;max_int|],[|max_int|];[||],[|0;0|]])[false;true])
    ["(array/grade keys)";"(array/index_of keys queries)"];
  List.iter(fun result->let value=dynamic("(map (fn [(a : int) (b : int)] (int/add a b)) keys "^result^")")in
    let r=match value with E.Residual r->r|_->assert false in
    List.iter(fun fusion->let p=packed(P.compile_result ~fusion ~count_source r(E.Residual.residual_view r).term)in
      assert(P.count_origin p=None && P.static_count p=None && Array.length p.sources=2);
      let resolve=function E.Source("fixture/keys",_,_)->Ok(E.Int_array[|2;1;2;0|])
        |E.Source("fixture/queries",_,_)->Ok(E.Int_array[|1;9|])|_->assert false in
      same(P.force ~strict:true ~resolve p ~live:(Input.at_time 1.))
        (E.Private.force_reference ~resolve value ~live:(Input.at_time 1.))) [false;true])
    ["(array/grade keys)";"(array/index_of keys queries)"];
  let value=dynamic "(array/index_of keys queries)"in
  let executor=ok(I.Executor.compile ~strict:true value)in
  let missing=function E.Source("fixture/keys",_,_)->Error(Diagnostic.error
    ~code:"E_DATA_SOURCE" ~cause:"lookup source cause" "missing keys")
    |E.Source("fixture/queries",_,_)->Ok(E.Int_array[||])|_->assert false in
  same(I.Executor.force ~strict:true ~resolve:missing executor ~live:(Input.at_time 1.))
    (E.Private.force_reference ~resolve:missing value ~live:(Input.at_time 1.));
  let wrong _=Ok(E.Text_array[|"wrong"|])in
  same(I.Executor.force ~strict:true ~resolve:wrong executor ~live:(Input.at_time 1.))
    (E.Private.force_reference ~resolve:wrong value ~live:(Input.at_time 1.));
  let skipped=dynamic "(if (> t 0.) (array/int 0) (array/index_of keys queries))"in
  let no_source _=failwith "Untaken lookup forced Source"in
  same(I.Executor.force ~strict:true ~resolve:no_source(ok(I.Executor.compile ~strict:true skipped))~live:(Input.at_time 1.))
    (E.Private.force_reference ~resolve:no_source skipped ~live:(Input.at_time 1.));
  let stateful=live_value "(let* [counter (state [s 0] (int/add s 1))] (if (> t 0.) (array/index_of (error \"E_KEYS\" \"rollback\" :cause \"same cause\") (array/int 0)) (array/grade (array/int 13 counter))))"in
  let a=E.create_state()and b=E.create_state()in
  let executor=ok(I.Executor.compile ~strict:true stateful)in
  List.iteri(fun frame time->let live={(Input.at_time time)with frame;tick=frame}in
    same(I.Executor.force ~strict:true ~state:a executor ~live)(E.Private.force_reference ~state:b stateful ~live);
    assert(bytes(E.Private.state_values a)=bytes(E.Private.state_values b))) [-1.;1.;-1.;1.;-1.];
  print_endline "Grade/index: stable direction, exact Int/byte Text, first match, ownership, strict/reference and lazy source/state parity"
