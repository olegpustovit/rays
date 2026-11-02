(* Undo entries hold no lowering (memory plan S4). The present is the only live Lower.t besides one
   kept beside the entries, so an old lowering's arrays are gone after two edits; stepping further
   back lowers the entry's text under the ids it had, to the same document. *)
module E3 = Rays_editor.Editor
module Doc = Editor_document.Workspace_doc
module F = Flow_graph.Flow_edit
module Document = Editor_document.Document

let check condition message = if not condition then failwith ("test_history_memory: " ^ message)
let ok = function Ok v -> v | Error m -> failwith ("test_history_memory: " ^ m)

let text =
  {|
(workspace memory
  (graph picture :context draw
    (let* [a 1.0]
      (draw/background "#14161c"))))
|}

let catalog = Editor_document.Kinds.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok

let parse text =
  match Flow.Syntax.parse text with
  | Ok [ form ] -> form
  | _ -> failwith "test_history_memory: parse"

let run () =
  let workspace =
    match Doc.of_text catalog text with
    | Ok d -> d
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let presets = Filename.temp_dir "history-memory" "" in
  let charged = Mem.document "editor.history" in
  let e =
    ref
      (ok
         (E3.create ~ui_text_size:11 ~presets ~await:true ~domains:1 ~workspace ~prepare:E3.prepare
            ~draw:E3.draw ()))
  in
  Fun.protect
    ~finally:(fun () ->
      E3.close !e;
      check (Mem.bytes charged = 0) "closing releases the history's charge")
    (fun () ->
      let tick = ref 0 in
      let step ?(keys = []) events =
        incr tick;
        e := E3.update !e (Test_editor_input.frame ~keys (-100., -100.) events !tick)
      in
      (* the text, the objects with their ids, and the plan's nodes with theirs *)
      let dump () =
        let plan = E3.Private.image_plan !e in
        ( Doc.to_text (E3.workspace !e),
          List.map
            (fun (node : Editor_document.Kind_graph.node) -> (node.id, node.label, node.kind))
            (E3.document !e),
          Array.map (fun (node : Flow.Eval.node) -> (node.id, node.kind)) plan.nodes )
      in
      let undo () = step ~keys:[ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'z') ] in
      let edit n =
        e :=
          ok
            (E3.edit !e
               (F.Add_node
                  {
                    scope = [ "picture" ];
                    name = Printf.sprintf "added_%d" n;
                    expr = parse (string_of_float (float n));
                  }));
        step []
      in
      step [];
      let start = dump () in
      check (Mem.bytes charged = 0) "no entry yet, nothing charged";
      (* the lowering of the document before any edit *)
      let weak = Weak.create 1 in
      Weak.set weak 0 (Some (E3.Private.image_plan !e));
      edit 1;
      let after_one = dump () in
      Gc.full_major ();
      check (Weak.check weak 0) "the last present stays live, one slot";
      check (Mem.bytes charged > 0) "an entry is charged";
      edit 2;
      Gc.full_major ();
      Gc.full_major ();
      check (not (Weak.check weak 0)) "an old lowering is gone after two edits";
      edit 3;
      let three = Mem.bytes charged in
      (* the last present comes back as it was; older ones are lowered again, to the same document *)
      undo ();
      undo ();
      check (dump () = after_one) "undo past the kept one lowers to the same document";
      check (Mem.bytes charged >= three / 2) "the entries moved to redo stay charged";
      undo ();
      check (dump () = start) "undo all the way back gives the first document";
      check (not (E3.can_undo !e)) "nothing left to undo";
      step ~keys:[ Input.Meta; Input.Shift ] [ Input.Key_pressed (Input.KeyChar 'z') ];
      check (dump () = after_one) "redo lowers the entry again";
      (* an entry's stated size against what it really reaches *)
      let doc = Editor_document.Lowering.of_workspace workspace |> Result.get_ok in
      let snapshot = Document.snapshot doc in
      let stated = Document.snapshot_bytes snapshot in
      let real = 8 * Obj.reachable_words (Obj.repr (snapshot.saved_ws.source, snapshot.saved_ws.extra)) in
      check (stated >= real) (Printf.sprintf "an entry's size covers its text: %d < %d" stated real);
      check (stated < 16 * real) (Printf.sprintf "and stays near it: %d, %d" stated real));
  print_endline "history memory: no lowering in an entry, gone after two edits, undo and redo relower"
