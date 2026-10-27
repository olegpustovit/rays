module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let workspace body =
  match Flow.Workspace.check {Flow.Check.version=1;kinds=[]}
    (ok (Flow.Syntax.parse ("(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.)))"))) with
  | Some ws,[] -> ws | _,ds -> failwith (body ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))
let same_bits a b = Int64.bits_of_float a=Int64.bits_of_float b
let same_result a b = match a,b with
  | Ok (E.Float a),Ok (E.Float b) -> same_bits a b
  | Ok a,Ok b -> a=b
  | Error (a : Diagnostic.t),Error b -> a=b | _ -> false
(* The core itself fails on these, and the executor must answer with the walker's own
   diagnostic (message and span) rather than a core one. *)
let failing = [
  "division by zero","(int/div (int (+ 10 t)) (int (* t 0)))",[1.;2.];
  "out-of-range nth","(nth (list t 1 2) (+ 5 (int t)))",[0.;1.5];
  "non-finite result","(exp (* t 100000.))",[1.;3.];
  "list slice","(first (take (int t) (list t 1)))",[0.;0.5];
  "authored error","(error (str \"E_RULES_\" (int t)) (str \"exact é日本😀 \" t))",[1.;2.];
  "authored cause","(error \"E_CAUSE\" \"selected failure\" :cause (str \"reason é日本😀 \" t))",[1.;2.];
]
let () =
  List.iter (fun (name,body,times) ->
    let value=snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true (workspace body))).records)) in
    let r=match value with E.Residual r -> r | _ -> failwith (name ^ ": not live") in
    let prepared=I.Core_exec.prepare (match I.Core_lower.lower r with Ok p -> p | Error n -> failwith (name ^ ": refused " ^ n)) in
    let compiled=ok (I.Executor.compile value) in
    List.iter (fun time ->
      let live=Input.at_time time in
      let expected=E.Private.force_reference value ~live in
      assert (Result.is_error expected);
      (match I.Core_exec.execute prepared ~live with
       | Error _ -> ()
       | Ok _ -> failwith (name ^ ": the core should fail"));
      assert (same_result (I.Core_exec.execute ~root:r ~strict:true prepared ~live)
        (I.Core_exec.execute prepared ~live));
      let actual=I.Executor.force compiled ~live in
      if not (same_result actual expected) then failwith (Printf.sprintf "%s at %g: diagnostic differs from the walker" name time)) times) failing;
  (* A failure after a state write restores the plane before the walker answers. *)
  let value=List.hd (ok (E.static (workspace "(state [s 0.] (if (> t 0.6) (error \"E_RULES\" (str \"exact failure \" t) :cause \"state rollback\") (+ s 1.)))"))).states in
  let reference=E.create_state () and core=E.create_state () in
  let failures=ref 0 and compiled=ok (I.Executor.compile value) in
  for frame=0 to 7 do
    let live={(Input.at_time (float_of_int frame *. 0.25)) with frame} in
    let core_before=E.state_stamp core and reference_before=E.state_stamp reference in
    let expected=E.Private.force_reference ~state:reference value ~live in
    let actual,_,walks=I.Executor.Private.force_with_walker_counts ~state:core compiled ~live in
    if Result.is_error expected then incr failures;
    if not (same_result actual expected) then failwith (Printf.sprintf "state frame %d" frame);
    if Result.is_error expected then
      (assert(E.state_stamp core=core_before && E.state_stamp reference=reference_before);
       assert(walks=1))
    else assert(walks=0);
    assert (same_result (Ok(E.List(Array.of_list(E.Private.state_values core))))
      (Ok(E.List(Array.of_list(E.Private.state_values reference)))))
  done;
  assert (!failures>0);
  (* A deliberately unsupported extension op is refused by Core and interpreted
     once. Repeating its residual in a nested value must reuse that evaluation. *)
  let opaque = Flow.Op.{(Option.get (Flow.Op.find "frame/dt" Flow.Context.value)) with
    name="test/opaque"; live=true; arithmetic=None; packed_extension=None;
    body=(fun ~live ~node:_ _ -> Flow.Value.Float (live.t +. 1.))} in
  let forms=match Flow.Syntax.parse "(workspace w (graph g :context value (test/opaque)))" with
    |Ok forms->forms|Error d->failwith(Diagnostic.to_string d) in
  let ws=match Flow.Workspace.check ~ops:[opaque] {Flow.Check.version=1;kinds=[]} forms with
    |Some ws,[]->ws|_,ds->failwith(String.concat "; " (List.map Diagnostic.to_string ds)) in
  let residual=match List.assoc "g" (ok(E.static ws)).results with
    |E.Residual r->r|_->failwith "opaque result was not residual" in
  assert (I.Core_lower.lower residual = Error "test/opaque");
  let shared=E.List [|E.Residual residual;E.List [|E.Residual residual|]|] in
  let program=ok(I.Executor.compile shared) and live=Input.at_time 0.25 in
  let actual,attempts,walks=I.Executor.Private.force_with_walker_counts program ~live in
  assert(actual=E.Private.force_reference shared ~live);
  assert(attempts=1 && walks=1);
  let reference=E.create_state () and strict=E.create_state () in
  let compiled=ok(I.Executor.compile ~strict:true value) in
  for frame=0 to 7 do
    let live={(Input.at_time (float_of_int frame *. 0.25)) with frame} in
    let reference_before=E.state_stamp reference and strict_before=E.state_stamp strict in
    let expected=E.Private.force_reference ~state:reference value ~live in
    (match I.Executor.force ~strict:true ~state:strict compiled ~live,expected with
     |Error actual,Error expected->assert(actual.code=expected.code && actual.message=expected.message && actual.cause=expected.cause)
     |actual,expected->assert(same_result actual expected));
    if Result.is_error expected then assert(E.state_stamp strict=strict_before && E.state_stamp reference=reference_before);
    assert(same_result (Ok(E.List(Array.of_list(E.Private.state_values strict))))
      (Ok(E.List(Array.of_list(E.Private.state_values reference)))))
  done;
  print_endline "Core errors: division by zero, range, non-finite, authored fields and state rollback match the walker"

let () =
  let recorded body=snd(List.hd(List.assoc["g";"tested"]
    (ok(E.static ~record:true(workspace body))).records)) in
  let value=recorded "(if (> t 0.) (error (str \"E_TEXT_\" (int t)) (str \"é日本😀 \" t) :cause (str \"reason \" t)) \"safe\")" in
  let residual=match value with E.Residual r->r|_->assert false in
  let core=Result.get_ok(I.Core_lower.lower ~strict:true residual) in
  let compiled=ok(I.Executor.compile ~strict:true value) in
  List.iter(fun time->let live=Input.at_time time in
    let expected=E.Private.force_reference value ~live in
    let check=function
      |Ok actual->assert(expected=Ok actual)
      |Error(d:Diagnostic.t)->(match expected with Error e->assert(d.code=e.code && d.message=e.message && d.cause=e.cause)|_->assert false) in
    check(I.Core_exec.run core ~live);check(I.Executor.force ~strict:true compiled ~live)) [-1.;1.;2.];
  (match E.static(workspace "(error \"E_LITERAL\" \"literal failure\" :cause \"literal cause\")") with
   |Error d->assert(d.code="E_LITERAL" && d.cause=Some "literal cause" && d.span<>None)
   |Ok _->assert false);
  List.iter(fun count->
    let value=recorded(Printf.sprintf "(map (fn [x] (if (> t 0.) (error \"E_PACKED_RULE\" \"exact é日本😀\" :cause \"selected packed cause\") x)) (array/range %d))" count) in
    let compiled=ok(I.Executor.compile ~strict:true value) in
    List.iter(fun time->let live=Input.at_time time in
      let expected=E.Private.force_reference value ~live in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        match I.Executor.force ~strict:true compiled ~live,expected with
        |Ok actual,Ok expected->assert(actual=expected)
        |Error actual,Error expected->assert(actual.code=expected.code && actual.message=expected.message && actual.cause=expected.cause)
        |_->assert false)) [1;8]) [-1.;1.]) [0;5001];
  let dynamic=recorded "(map (fn [x] (if (> t 0.) (error \"E_DYNAMIC\" (str x)) x)) (array/range 5001))" in
  (match I.Executor.compile ~strict:true dynamic with
   | Error d when d.code="E_KERNEL_FORM" -> ()
   | Error d -> failwith ("dynamic packed refusal returned " ^ Diagnostic.to_string d)
   | Ok _ -> failwith "dynamic packed refusal was accepted");
  assert(Result.is_ok(E.Private.force_reference dynamic ~live:(Input.at_time(-1.))));
  print_endline "Authored errors: exact dynamic messages, lazy text/packed branches, empty inputs, rollback and dynamic packed refusal pass"
