module W = Flow.Workspace
module E = Flow.Eval
module V = Flow.Value
module D = Diagnostic
module I = Flow_ir
module C = I.Core

let ok = function Ok value -> value | Error d -> failwith (D.to_string d)
let catalog = Flow.Catalog.{ version = 1; kinds = [] }
let parse text = ok (Flow.Syntax.parse text)
let source body = "(workspace errors (graph g :context value (let* [tested " ^ body ^ "] 0.)))"

let check text =
  match W.check catalog (parse text) with
  | Some workspace, ds
    when not (List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity) ds) ->
      workspace
  | _, ds -> failwith (String.concat "; " (List.map D.to_string ds))

let mapper = "(fn [failure] failure)"
let diagnostic_ty = Flow.Ty.Record [ ("code", Text); ("message", Text); ("cause", List Text) ]

let residual body =
  let checked = check (source ("(with_error " ^ body ^ " " ^ mapper ^ ")")) in
  let evaluation = ok (E.static ~record:true checked) in
  assert (Array.length evaluation.plan.nodes = 0);
  match snd (List.hd (List.assoc [ "g"; "tested" ] evaluation.records)) with
  | E.Residual r -> r
  | _ -> failwith "with_error was evaluated eagerly"

let compare body =
  let r = residual body in
  let program =
    match I.Core_lower.lower ~strict:true r with
    | Ok program -> program
    | Error form -> failwith form
  in
  let live = Input.at_time 0. in
  let a = Test_program.run program ~live and b = E.Host.force_reference (E.Residual r) ~live in
  (match (a, b) with
  | Ok a, Ok b -> assert (a = b)
  | Error a, Error b ->
      assert (
        a.code = b.code && a.message = b.message && a.cause = b.cause && a.hints = b.hints
        && a.trace = b.trace)
  | _ -> assert false);
  a

let error code message cause = function
  | Error (d : D.t) -> assert (d.code = code && d.message = message && d.cause = cause)
  | Ok _ -> assert false

let () =
  assert (compare ("(with_error nil " ^ mapper ^ ")") = Ok E.No_geo);
  assert (compare "(with_error 7 (fn [failure] (error \"E_MAPPER\" \"never\")))" = Ok (E.Int 7));
  assert (
    compare ("(if false (with_error (error \"E_BAD\" \"never\") " ^ mapper ^ ") 3)") = Ok (E.Int 3));
  error "E_MAPPED" "mapped 日本😀" (Some "raw cause")
    (compare
       "(with_error (error \"E_CHILD\" \"child\" :cause \"raw cause\") (fn [failure] (assoc \
        failure :code \"E_MAPPED\" :message \"mapped 日本😀\")))");
  error "E_MAPPER" "mapper" None
    (compare
       "(with_error (error \"E_CHILD\" \"child\") (fn [failure] (error \"E_MAPPER\" \"mapper\")))");
  error "E_CANCELLED" "cancelled unchanged" None
    (compare
       "(with_error (error \"E_CANCELLED\" \"cancelled unchanged\") (fn [failure] (error \
        \"E_WRONG\" \"mapper ran\")))");
  error "E_OUTER" "inner" None
    (compare
       "(with_error (with_error (error \"E_CHILD\" \"inner\") (fn [failure] (error \"E_MAPPER\" \
        \"inner\"))) (fn [failure] (assoc failure :code \"E_OUTER\")))");
  assert (
    compare ("(with_error (exact (map (fn [x] (+ x 1.0)) (array/float 32769 2.0))) " ^ mapper ^ ")")
    = Ok (E.Float_array (Array.make 32769 3.)));
  error "E_UNUSED" "unused helper" None
    (compare
       "(let* [guard (fn [x] (with_error (error \"E_UNUSED\" \"unused helper\") (fn [failure] \
        failure))) unused (guard 1)] 0)");
  List.iter
    (fun body ->
      let _, diagnostics = W.check catalog (parse (source body)) in
      assert (List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity) diagnostics))
    [
      "(with_error 1 (fn [failure] {:code \"bad\"}))";
      "(with_error 1 (fn [failure] (assoc failure :message (str t))))";
      "(with_error (state [s 0] (+ s 1)) (fn [failure] failure))";
    ]

let () =
  let original =
    D.error ~span:{ start = 19; finish = 23 } ~position:{ line = 2; col = 3 } ~cause:"\255 raw 日本"
      ~hints:[ "hint" ]
      ~trace:[ D.{ node_id = 13; label = "child"; operation = "native.child" } ]
      ~code:"E_CHILD" "child"
  in
  let source : C.source =
    {
      site = [ "g" ];
      owner = [ "g" ];
      instance = 0;
      prefix = [];
      iter = [];
      span = Some { start = 0; finish = 5 };
    }
  in
  let statement id node : C.statement = { id; source; node } in
  let program : C.program =
    {
      constants = [||];
      locals = [| diagnostic_ty; Flow.Ty.Any; diagnostic_ty; Flow.Ty.Any |];
      body =
        [
          statement 0
            (Protect
               {
                 body = [ statement 1 (Assign (1, Prim (Fail_diagnostic original, []))) ];
                 value = 1;
                 failure = 0;
                 handler = [ statement 2 (Assign (2, Local 0)) ];
                 mapped = 2;
                 result = 3;
               });
        ];
      results = [| 3 |];
      state_slots = [||];
      regions = [||];
      call_slots = 0;
    }
  in
  (match Test_program.run program ~live:(Input.at_time 0.) with
  | Error actual -> assert (actual = original)
  | _ -> assert false);
  let without_cause = { original with cause = None } in
  (try
     ignore (V.with_error ~protected:(fun () -> V.raise_diagnostic without_cause) ~map:Fun.id);
     assert false
   with Diagnostic.Error actual -> assert (actual = without_cause));
  let called = ref 0 in
  (try
     ignore
       (V.with_error
          ~protected:(fun () -> V.raise_diagnostic original)
          ~map:(fun _ ->
            incr called;
            V.Record
              [
                ("code", Text "E_NEW");
                ("message", Text "new");
                ("cause", List [| Text "a"; Text "b" |]);
              ]));
     assert false
   with Diagnostic.Error actual ->
     assert (
       actual.code = "E_DIAGNOSTIC_MAP" && actual.trace = original.trace
       && actual.hints = original.hints && actual.cause = original.cause));
  assert (!called = 1);
  (try
     ignore
       (V.with_error
          ~protected:(fun () -> V.raise_diagnostic original)
          ~map:(fun _ -> V.fail "E_CANCELLED" "mapper cancellation"));
     assert false
   with Diagnostic.Error actual ->
     assert (
       actual.code = "E_CANCELLED"
       && actual.message = "mapper cancellation"
       && actual.span = None && actual.hints = [] && actual.trace = []));
  try
    ignore (V.with_error ~protected:(fun () -> raise Exit) ~map:(fun _ -> assert false));
    assert false
  with Exit -> ()

let () =
  let request = residual "(exact (map (fn [x] (+ x 1.0)) (array/float 4 2.0)))" in
  let callback_error = D.error ~code:"E_CALLBACK" ~cause:"bulk hook" "bulk hook failed" in
  let mapped = ref 0 in
  let actual =
    try
      ignore
        (E.Residual.handle
           ~bulk:(fun _ -> raise (Diagnostic.Error callback_error))
           (fun () ->
             V.with_error
               ~protected:(fun () -> Option.get (Effect.perform (E.Residual.Bulk_static request)))
               ~map:(function
                 | V.Record fields ->
                     incr mapped;
                     V.Record
                       (List.map
                          (fun (name, value) ->
                            if name = "code" then (name, V.Text "E_MAPPED") else (name, value))
                          fields)
                 | _ -> assert false)));
      assert false
    with Diagnostic.Error diagnostic -> diagnostic
  in
  assert (!mapped = 1 && actual.code = "E_MAPPED" && actual.cause = callback_error.cause)

let () =
  let forms =
    parse
      (source "(let* [guarded (with_error (array/float 2 3.0) (fn [failure] failure))] guarded)")
  in
  let checked = check (fst (Flow.Lisp.print forms)) in
  let scope = Flow_graph.Projection.of_graph catalog checked "g" in
  let card = Option.get (Flow_graph.Projection.find scope [ "g"; "tested"; "guarded#0" ]) in
  assert (card.head = "array/float");
  let forms, checked =
    ok
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Flow.Syntax.make (Num "4.0") }))
  in
  assert (
    snd (List.hd (List.assoc [ "g"; "tested" ] (ok (E.run ~record:true ~time:0. checked)).records))
    = E.Float_array [| 4.; 4. |]);
  let _, checked_again = W.check catalog (parse (fst (Flow.Lisp.print forms))) in
  assert (not (List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity) checked_again));
  assert (
    Result.is_error
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg
            { node = card.path; key = Pos 0; sub = []; value = Flow.Syntax.make (Str "invalid") })))

let () =
  print_endline
    "Diagnostic boundaries: strict Core/reference, nested nil, unused local helper, lazy mapping, \
     cancellation and lossless envelopes pass"
