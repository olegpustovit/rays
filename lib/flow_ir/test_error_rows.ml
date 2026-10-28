module W = Flow.Workspace
module E = Flow.Eval
module V = Flow.Value
module D = Diagnostic
module I = Flow_ir

let ok = function Ok value -> value | Error d -> failwith (D.to_string d)
let catalog = Flow.Catalog.{ version = 1; kinds = [] }

let mapper =
  "(fn [index] {:code \"E_ROW\" :message (str \"bad \" index) :cause (list (str \"raw 日本😀 \" \
   index))})"

let source body =
  "(workspace rows (graph g :context value [(flags : (array int) (array/int 0))] (let* [tested "
  ^ body ^ "] 0.)))"

let parse text = ok (Flow.Syntax.parse text)

let checked text =
  match W.check catalog (parse text) with
  | Some workspace, ds
    when not (List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity) ds) ->
      workspace
  | _, ds -> failwith (String.concat "; " (List.map D.to_string ds))

let evaluate ?resolve flags body =
  let workspace = checked (source body) in
  let evaluation = ok (E.static ~record:true ~inputs:[ ("g", [ ("flags", flags) ]) ] workspace) in
  let value = snd (List.hd (List.assoc [ "g"; "tested" ] evaluation.records)) in
  let residual = match value with E.Residual r -> r | _ -> failwith "row diagnosis was eager" in
  let program =
    match I.Core_lower.lower ~strict:true residual with Ok p -> p | Error form -> failwith form
  in
  let live = Input.at_time 0. in
  let native = Test_program.run ?resolve program ~live in
  let reference = E.Host.force_reference ?resolve value ~live in
  (match (native, reference) with
  | Ok a, Ok b -> assert (a = b)
  | Error a, Error b -> assert (a.code = b.code && a.message = b.message && a.cause = b.cause)
  | _ -> assert false);
  native

let resolve grain calls = function
  | E.Source ("$core/diagnostic_index", Flow.Ty.Int, [ ("rows", rows); ("policy", E.Text policy) ])
    -> (
      incr calls;
      let grain =
        match policy with "first" -> None | "range-last" -> Some grain | _ -> assert false
      in
      match V.error_row_index ?grain rows with
      | Some index -> Ok (E.Int index)
      | None -> assert false)
  | _ -> assert false

let failure index = function
  | Error (d : D.t) ->
      assert (
        d.code = "E_ROW"
        && d.message = "bad " ^ string_of_int index
        && d.cause = Some ("raw 日本😀 " ^ string_of_int index))
  | _ -> assert false

let () =
  let calls = ref 0 in
  let empty = E.Int_array [||] and zero = E.Int_array (Array.make 32769 0) in
  List.iter
    (fun flags ->
      assert (
        evaluate ~resolve:(resolve 97 calls) flags
          "(error_rows flags (fn [index] (error \"E_UNTAKEN\" \"mapper\")) :policy \"range-last\")"
        = Ok E.No_geo))
    [ empty; zero ];
  assert (!calls = 0);
  List.iter
    (fun bits ->
      let rows = Array.make 32769 0 in
      List.iter (fun i -> rows.(i) <- 1) bits;
      let before = Array.copy rows in
      List.iter
        (fun grain ->
          let selected = Option.get (V.error_row_index ~grain (E.Int_array rows)) in
          failure selected
            (evaluate ~resolve:(resolve grain calls) (E.Int_array rows)
               ("(error_rows flags " ^ mapper ^ " :policy \"range-last\")")))
        [ 1; 97; 4096; max_int ];
      failure (List.hd bits) (evaluate (E.Int_array rows) ("(error_rows flags " ^ mapper ^ ")"));
      assert (rows = before))
    [ [ 0; 1; 96; 97; 98 ]; [ 96; 97; 98 ]; [ 4095; 4096; 4097 ]; [ 32768 ] ];
  (match
     evaluate (E.Int_array [| 1 |]) ("(error_rows flags " ^ mapper ^ " :policy \"range-last\")")
   with
  | Error d -> assert (d.code = "E_DIAGNOSTIC_CONTEXT")
  | _ -> assert false);
  (* A malformed later flag cannot hide behind an earlier ordinary failure. *)
  (match evaluate (E.Int_array [| 1; 0; 2 |]) ("(error_rows flags " ^ mapper ^ ")") with
  | Error d -> assert (d.code = "E_DIAGNOSTIC_ROWS")
  | _ -> assert false);
  assert (
    evaluate (E.Int_array [| 1 |])
      ("(if (< t 0.) (let* [unused (error_rows flags " ^ mapper ^ ")] 0) 0)")
    = Ok (E.Int 0));
  (match
     evaluate
       ~resolve:(fun _ -> Error (D.error ~cause:"raw cancel" ~code:"cancelled" "cancel"))
       (E.Int_array [| 1 |])
       ("(error_rows flags " ^ mapper ^ " :policy \"range-last\")")
   with
  | Error d -> assert (d.code = "cancelled" && d.message = "cancel" && d.cause = Some "raw cancel")
  | _ -> assert false);
  (match
     evaluate (E.Int_array [| 1 |])
       "(error_rows flags (fn [index] (error \"cancelled\" \"mapper cancel\" :cause \"original \
        cancel\")))"
   with
  | Error d ->
      assert (
        d.code = "cancelled" && d.message = "mapper cancel" && d.cause = Some "original cancel")
  | _ -> assert false);
  (try
     ignore (V.error_row_index (E.Float_array [| 1. |]));
     assert false
   with Diagnostic.Error d -> assert (d.code = "E_DIAGNOSTIC_ROWS"));
  let polls = ref 0 in
  (try
     ignore
       (V.error_row_index ~grain:97
          ~check:(fun () ->
            incr polls;
            if !polls = 3 then V.fail "E_CANCELLED" "cancel scan")
          (E.Int_array (Array.make 5001 0)));
     assert false
   with Diagnostic.Error d -> assert (d.code = "E_CANCELLED"));
  List.iter
    (fun body ->
      let _, ds = W.check catalog (parse (source body)) in
      assert (List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity) ds))
    [
      "(error_rows flags (fn [index] index))";
      "(error_rows flags (fn [index] {:code \"x\" :message (str t) :cause (list)}))";
      "(error_rows flags (fn [index] {:code \"x\" :message \"x\" :cause (list)}) :policy (str \
       \"first\"))";
      "(with_error 1 (fn [error] (let* [unused (error_rows flags (fn [i] error))] error)))";
    ]

let () =
  let text = source ("(let* [checked (error_rows (array/int 2 0) " ^ mapper ^ ")] checked)") in
  let forms = parse text and workspace = checked text in
  let scope = Flow_graph.Projection.of_graph catalog workspace "g" in
  let card = Option.get (Flow_graph.Projection.find scope [ "g"; "tested"; "checked#0" ]) in
  assert (card.head = "array/int");
  let forms, workspace =
    ok
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Flow.Syntax.make (Num "1") }))
  in
  let printed = fst (Flow.Lisp.print forms) in
  ignore (checked printed);
  let result =
    ok (E.static ~record:true ~inputs:[ ("g", [ ("flags", E.Int_array [||]) ]) ] workspace)
  in
  failure 0
    (E.Host.force_reference
       (snd (List.hd (List.assoc [ "g"; "tested" ] result.records)))
       ~live:(Input.at_time 0.));
  assert (
    Result.is_error
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg
            {
              node = card.path;
              key = Pos 1;
              sub = [];
              value = Flow.Syntax.make (Str "wrong storage");
            })))

let () =
  let text =
    source ("(let* [checked (error_rows (array/int 2 1) " ^ mapper ^ " :policy \"first\")] checked)")
  in
  let forms = parse text and workspace = checked text in
  let scope = Flow_graph.Projection.of_graph catalog workspace "g" in
  let card = Option.get (Flow_graph.Projection.find scope [ "g"; "tested"; "checked" ]) in
  assert (card.head = "error_rows");
  let forms, _ =
    ok
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg
            {
              node = card.path;
              key = Kw "policy";
              sub = [];
              value = Flow.Syntax.make (Str "range-last");
            }))
  in
  let calls = ref 0 in
  failure 1
    (evaluate ~resolve:(resolve 97 calls) (E.Int_array [||])
       "(error_rows (array/int 2 1) (fn [index] {:code \"E_ROW\" :message (str \"bad \" index) \
        :cause (list (str \"raw 日本😀 \" index))}) :policy \"range-last\")");
  ignore (checked (fst (Flow.Lisp.print forms)));
  assert (
    Result.is_error
      (Flow_graph.Flow_edit.apply_checked catalog forms
         (Set_arg
            {
              node = card.path;
              key = Kw "policy";
              sub = [];
              value = Flow.Syntax.make (Str "unknown");
            })))

let () =
  print_endline
    "Diagnostic rows: strict Core/reference, complete immutable flags, boundary/tail selection, \
     cancellation and checked graph edits pass"
