module E = Flow.Eval
module I = Flow_ir
module C = I.Core
open Test_support
let rec same (a : E.value) (b : E.value) = match a,b with
  | Float a,Float b -> Int64.bits_of_float a=Int64.bits_of_float b
  | List a,List b -> Array.length a=Array.length b && Array.for_all2 same a b
  | Record a,Record b -> List.length a=List.length b && List.for_all2 (fun (n,a) (m,b) -> n=m && same a b) a b
  | a,b -> a=b
let cases = Core_fixtures.lists
let check (name,body) =
  let tested=if String.starts_with ~prefix:"captured-" name then body
    else "(if (> t -1000.) " ^ body ^ " " ^ body ^ ")" in
  let source="(workspace w (graph g :context value (let* [tested " ^ tested ^ "] 0.)))" in
  let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
    | Some ws,[] -> ws | _,ds -> failwith (name ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds)) in
  let r=match snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) with
    | E.Residual r -> r | _ -> failwith (name ^ ": expected residual") in
  let core=match I.Core_lower.lower r with Ok p -> p | Error n -> failwith (name ^ ": refused " ^ n) in
  if name="iterate-huge-until"then begin
    let seen=ref false in
    C.iter(fun(s:C.statement)->match s.node with
      |Counted{stop;_}->assert(Option.is_some stop);seen:=true|_->())core.body;
    assert(!seen)
  end;
  assert (Format.asprintf "%a" I.Core.pp core = Format.asprintf "%a" I.Core.pp (Result.get_ok (I.Core_lower.lower r)));
  let program=ok (I.Executor.compile (E.Residual r)) in
  List.iter (fun time ->
    let live=Input.at_time time in
    let expected=E.Host.force_reference (E.Residual r) ~live in
    let verify actual = match actual,expected with
      | Ok actual,Ok expected when same actual expected -> ()
      | Error a,Error b when a.Diagnostic.code=b.Diagnostic.code -> ()
      | _ -> let show=function Ok v -> E.show v | Error d -> Diagnostic.to_string d in
          failwith (Printf.sprintf "%s at %g: %s <> %s" name time (show actual) (show expected)) in
    verify (E.force (E.Residual r) ~live);
    verify (Test_program.run core ~live);
    verify (I.Executor.force program ~live))
    [-3.;-2.5;-0.;0.;0.37;1.;1.9;3.;8.;4096.;4097.]
let () = List.iter check cases;print_endline "Core lists: direct/reference/executor parity passed"

let () =
  let module C=I.Core in
  let source : C.source = {site=["builder"];owner=[];instance=0;prefix=[];iter=[];span=None} in
  let statement id node : C.statement = {id;source;node} in
  let program body : C.program =
    {constants=[|Int 2;Int 0;Int 7;List [|Int 0;Int 0|]|];
     locals=[|Flow.Ty.Int;List Int;Int;Int;List Int;Record ["xs",List Int]|];
     body;results=[|1|];state_slots=[||];regions=[||];call_slots=0} in
  let init=[statement 0 (Assign (0,Constant 0));statement 1 (Assign (1,Prim (Sequence_build,["count",0])));
    statement 2 (Assign (2,Constant 1));statement 3 (Assign (3,Constant 2))] in
  let store=statement 4 (Store {sequence=1;index=2;value=3}) in
  let live=Input.at_time 0. in
  assert (Test_program.run (program (init @ [store])) ~live = Ok (E.List [|Int 7;No_geo|]));
  List.iter (fun body -> match Test_program.run (program body) ~live with
    | Error d -> assert (d.code="E_CORE_OWNERSHIP")
    | Ok _ -> failwith "A published or constant sequence accepted a store")
    [init @ [statement 5 (Assign (4,Local 1));statement 6 (Assign (5,Fields ["xs",4]));store];
     [statement 0 (Assign (1,Constant 3));statement 1 (Assign (2,Constant 1));
      statement 2 (Assign (3,Constant 2));store]];
  print_endline "Core list stores: local builder ownership and alias publication passed"

let () =
  let module C=I.Core in
  let source:C.source={site=["cooked-builder"];owner=[];instance=0;prefix=[];iter=[];span=None} in
  let statement id node:C.statement={id;source;node} in
  let program count cooked:C.program={constants=[|Int count;Bool cooked|];
    locals=[|Flow.Ty.Int;Bool;List Flow.Ty.geometry|];
    body=[statement 0(Assign(0,Constant 0));statement 1(Assign(1,Constant 1));
      statement 2(Assign(2,Prim(Sequence_build,["count",0;"cooked",1])))];
    results=[|2|];state_slots=[||];regions=[||];call_slots=0} in
  let live=Input.at_time 0. in
  (match Test_program.run ~strict:true(program 5001 true) ~live with
   |Ok(E.List xs)->assert(Array.length xs=5001 && Array.for_all((=)E.No_geo)xs)
   |_->failwith "Typed cooked collection retained an unrelated value-list cap");
  List.iter(fun(count,cooked)->match Test_program.run ~strict:true(program count cooked) ~live with
    |Error d->assert(d.code="E_ITER_BOUND" && d.message="Core list length exceeds its bound.")
    |_->failwith "Ordinary list or negative cooked builder lost its existing guard") [5001,false;-1,true];
  assert(match Test_program.run ~strict:true(program 4096 false) ~live with
    |Ok(E.List xs)->Array.length xs=4096|_->false)

let () =
  let module C=I.Core in
  let captured ty body value =
    let default=if ty="geometry" then "(list nil)" else "(list 1)" in
    let source="(workspace captures (graph g :context host [(parts : (list "^ty^") "^default^")] "^
      "(let* [clock t tested "^body^"] 0.)))" in
    let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]}(ok(Flow.Syntax.parse source))with
      |Some ws,[]->ws|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
    let r=match snd(List.hd(List.assoc["g";"clock"](ok(E.static ~record:true ws)).records))with
      |E.Residual r->r|_->failwith "Capture context needs a live clock"in
    let term=match (List.hd ws.graphs).body.node with
      |Flow.Workspace.Let(bindings,_)->List.find_map(function
        |Flow.Workspace.Name "tested",term->Some term|_->None)bindings|>Option.get
      |_->failwith "Missing checked capture term"in
    E.Residual.with_bindings r ~term ["parts",value] in
  let xs=Array.init 5001(fun i->if i mod 11=10 then E.No_geo
    else E.Deferred(Flow.Ty.geometry,if i mod 2=0 then 17 else 23))in
  let resolve=function
    |E.Source("$core/cooked",ty,["id",E.Int id])when ty=Flow.Ty.geometry->Ok(E.Deferred(ty,id))
    |E.Source("$core/operator",ty,("$name",E.Text name)::args)
      when ty=Flow.Ty.geometry && (name="nth" || name="first")->
        let op=Option.get(Flow.Op.find name Flow.Context.host)in
        Ok(op.body ~live:(Input.at_time 0.)
          ~node:(fun _ _->failwith "Collection selector unexpectedly constructed a node")args)
    |E.Source(name,ty,args)->Error(Diagnostic.error ~code:"E_DATA_SOURCE"
      ("Unexpected capture source: "^name^" : "^Flow.Ty.to_string ty^" "^
        String.concat "; "(List.map(fun(name,value)->name^"="^E.show value)args)))
    |value->Error(Diagnostic.error ~code:"E_DATA_SOURCE" ("Unexpected capture value: "^E.show value))in
  List.iter(fun body->
    let r=captured "geometry" body (E.List xs)in
    let core=match I.Core_lower.lower ~strict:true r with
      |Ok p->p|Error why->failwith("Known geometry HOF refused: "^why)in
    (* A homogeneous captured collection is constructed once before Counted.
       Its loop must not contain a 5001-arm selector or rebuild its input. *)
    let assignments statements =
      let total = ref 0 in C.iter (fun _ -> incr total) statements; !total in
    let loops=List.filter_map(fun(s:C.statement)->match s.node with C.Counted{body;_}->Some body|_->None)core.body in
    if loops=[] || List.exists(fun body->assignments body>=100)loops then
      failwith(Printf.sprintf "Known collection loop shape: %s loops=%d statements=%s"
        body (List.length loops)(String.concat ","(List.map(fun body->string_of_int(assignments body))loops)));
    let builds statements =
      let count = ref 0 in
      C.iter (fun (s:C.statement) -> match s.node with C.Assign(_,Prim(Sequence_build,_))->incr count |_->()) statements;
      !count in
    assert(List.for_all(fun body->builds body=0)loops);
    List.iter(fun time->let live=Input.at_time time in
      let expected=ok(E.Host.force_reference ~resolve(E.Residual r) ~live)in
      let actual=ok(Test_program.run ~strict:true ~resolve core ~live)in
      assert(same actual expected)) [0.;0.25;0.75;1.])
    ["(map (fn [(geo : geometry)] (nth (list geo) (int/mod (frame/index) 1))) parts)";
     "(filter (fn [(geo : geometry)] (if (< t 0.5) true (equal? geo (first parts)))) parts)"];
  let ordinary=captured "int" "(filter (fn [(x : int)] (< t 0.5)) parts)"(E.List(Array.make 5001(E.Int 1)))in
  assert(match I.Core_lower.lower ~strict:true ordinary with Error _->true|Ok _->false);
  print_endline "Known 5001 geometry map/filter captures: bounded loop shape, ordered native/reference handles and unchanged ordinary list guard"

(* iterate over a packed array: the map is one region lowered once and invoked by one counted
   loop, so 5000 passes do not re-lower anything; the count is a live int, past the 4,096 bound *)
let () =
  let module C=I.Core in
  let source="(workspace w (graph g :context value (let* [tested (iterate (int (+ 5000. (* t 0.))) (array/float 8 1.0)
    (fn [p] (exact (map (fn [(i : int)] (* 0.999 (+ (array/nth p i) (* t 0.)))) (array/range 8 :type int)))))] 0.)))" in
  let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
    | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let r=match snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) with
    | E.Residual r -> r | _ -> failwith "iterate: expected residual" in
  let core=match I.Core_lower.lower ~strict:true r with Ok p -> p | Error n -> failwith ("iterate refused: " ^ n) in
  let counted statements =
    let n = ref 0 in
    C.iter (fun (s:C.statement) -> match s.node with Counted _ -> incr n | _ -> ()) statements;
    !n in
  assert (Array.length core.regions=1);
  assert (counted core.body=1);
  let live=Input.at_time 0. in
  let want=E.Host.force_reference (E.Residual r) ~live in
  (match Test_program.run core ~live,want with
   | Ok (E.Float_array a),Ok (E.Float_array b) ->
       assert (Array.length a=8 && Array.for_all2 (fun x y -> Int64.bits_of_float x=Int64.bits_of_float y) a b)
   | Ok a,Ok b -> failwith (E.show a ^ " <> " ^ E.show b)
   | _ -> failwith "iterate packed: a side failed");
  print_endline "Core iterate: one region, one counted loop for 5000 passes, byte-identical to the walker"
