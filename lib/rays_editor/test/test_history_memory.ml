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
  let e =
    ref
      (ok
         (E3.create ~ui_text_size:11 ~presets ~await:true ~domains:1 ~workspace ~prepare:E3.prepare
            ~draw:E3.draw ()))
  in
  (* the entries the ledger holds: named at a frame's boundary, sized when a report walks *)
  let walked () =
    ignore (Mem.report ());
    Mem.bytes (Mem.document "editor.history")
  in
  let held () =
    e := E3.after_present !e (Test_editor_input.frame (-100., -100.) [] 0);
    walked ()
  in
  Fun.protect
    ~finally:(fun () ->
      E3.close !e;
      check (walked () < 1024) "closing lets the history's entries go")
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
      check (held () < 1024) "no entry yet, nothing held";
      (* the lowering of the document before any edit *)
      let weak = Weak.create 1 in
      Weak.set weak 0 (Some (E3.Private.image_plan !e));
      edit 1;
      let after_one = dump () in
      Gc.full_major ();
      check (Weak.check weak 0) "the last present stays live, one slot";
      check (held () > 1024) "an entry is held";
      edit 2;
      ignore (held ());
      Gc.full_major ();
      Gc.full_major ();
      check (not (Weak.check weak 0)) "an old lowering is gone after two edits";
      edit 3;
      let three = held () in
      (* the last present comes back as it was; older ones are lowered again, to the same document *)
      undo ();
      undo ();
      check (dump () = after_one) "undo past the kept one lowers to the same document";
      check (held () >= three / 2) "the entries moved to redo stay held";
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

(* Over its budget a cook fails with the typed E_MEMORY naming the largest owners, the last good
   frame stays, the error clears when usage is back and the cook runs again; over budget costs no
   collection per frame. *)
let budget_text =
  {|
(workspace budget
  (graph g :context sop
    (sop/transform (sop/grid :columns 8 :rows 8) :translate [(sin t) 0 0])))
|}

let contains text word =
  let n = String.length word in
  Seq.exists
    (fun i -> String.sub text i n = word)
    (Seq.init (max 0 (String.length text - n + 1)) Fun.id)

let run_budget () =
  let workspace =
    match Doc.of_text catalog budget_text with
    | Ok d -> d
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let measure ~frames step =
    match
      Rays_editor.Measure.run ~width:64 ~height:64 ~step ~early:1 ~frames workspace
    with
    | Ok outcome -> outcome
    | Error message -> failwith ("test_budget: " ^ message)
  in
  let cycles () = (Gc.quick_stat ()).major_collections in
  let latched_from = ref 0 and latched_cycles = ref 0 and had_frame = ref false in
  let over =
    measure ~frames:50 (fun tick editor ->
        if tick = 5 then begin
          check (E3.prepared editor <> None) "a first good frame";
          Mem.set_budget 1
        end;
        if tick = 10 then latched_from := cycles ();
        if tick = 50 then begin
          latched_cycles := cycles () - !latched_from;
          had_frame := E3.prepared editor <> None
        end;
        editor)
  in
  let says = function Some text -> contains text "E_MEMORY" | None -> false in
  check (says over.memory_error) "the boundary reports E_MEMORY";
  check (says over.cook_error) "the cook that would pass the limit fails with E_MEMORY";
  check (Option.fold ~none:false ~some:(fun t -> contains t "largest owners") over.memory_error)
    "E_MEMORY names the largest owners";
  check !had_frame "the last good frame stays";
  check (!latched_cycles < 20)
    (Printf.sprintf "no collection per frame while over: %d cycles in 40 frames" !latched_cycles);
  Mem.reset ();
  (* recovery: the same process, the limit lifted *)
  let recovered =
    measure ~frames:30 (fun tick editor ->
        if tick = 2 then Mem.set_budget 1;
        if tick = 15 then Mem.set_budget 100_000_000_000;
        editor)
  in
  check (recovered.memory_error = None) "E_MEMORY clears on recovery";
  check (recovered.cook_error = None) "the cook runs again on recovery";
  Mem.reset ();
  print_endline "memory budget: typed E_MEMORY, last good frame, clears on recovery, no stall"
