(* Editor transactions on a workspace: the node a click selects is the node an edit in the
   same frame changes, undo agrees with the document, a changed graph with the same prepared
   value is drawn again, and one pointer drag is one undo entry. *)
open Rays
open Sop
module E = Rays_editor.Editor
module S = Flow.Syntax

let check condition message = if not condition then failwith message

let rec source ~label:_ ~inputs:_ value =
  Node.parameterize ~schema:Test_editor_commands.schema ~values:value ~rebuild:source
    (Lisp_sop.snapshot (Rdk.Line_geometry.points [|float value, 0., 0.|]))

let factory = Edit_graph.factory ~key:"test_source" ~label:"Test source" ~category:["Test"]
    ~fields:(Param.view Test_editor_commands.schema 0) ~arity:0 (fun _ -> source ~label:"test_source" ~inputs:[] 0)

let text = {|(workspace transactions
  (graph g :context sop
    (let* [first (sop/test_source :value 0)
           second (sop/test_source :value 1)]
      (sop/merge first second))))|}

let run () =
  let workspace = Ws_fixture.of_text ~factories:(factory :: Sop_catalog.Editor.factories) text in
  let draws = Atomic.make 0 in
  let current = ref (E.create ~ui_text_size:11 ~workspace ~factories:(factory :: Sop_catalog.Editor.factories)
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Atomic.incr draws; Scene.D3.empty) () |> Result.get_ok)
  and count = ref 0 in
  Fun.protect ~finally:(fun () -> E.close !current) (fun () ->
    let step ?(mouse = (0., 0.)) ?(keys = []) ?(buttons = []) events =
      incr count;
      current := E.update !current (Test_editor_input.frame ~keys ~buttons mouse events !count) in
    let click point = [Input.Pointer_pressed (Input.LeftButton, point);
      Input.Pointer_released (Input.LeftButton, point)] in
    let wait_draw after = Test_editor_cook.await (fun () -> step []; Atomic.get draws > after) in
    wait_draw 0;
    (* the scene opens as a list: a click on the object's row selects it and [i] enters its graph *)
    let gx, gy, _, _ = (E.panes !current (Test_editor_input.frame (0., 0.) [] 0)).Pxui_shell.Layout.graph in
    let row = float (gx + 60), float (gy + 24 + 12) in
    step ~mouse:row []; step ~mouse:row (click row); step [Input.Key_pressed (Input.KeyChar 'i')];
    count := !count + 30; (* past the double-click interval *)
    (* the lowered node whose value is [n], by stable id *)
    let node n = List.find (fun (info : Edit_graph.node_info) ->
        List.exists (fun (field : Param.field_view) -> field.name = "value"
          && field.current = Param.Int_value n) (Node.parameter_fields info.node))
      (Edit_graph.inspect (E.document !current)) in
    let first = (node 0).id and second = (node 1).id in
    let value id = Edit_graph.find (E.document !current) ~node_id:id |> Option.get
      |> Node.parameter_fields |> List.find (fun (field : Param.field_view) -> field.name = "value")
      |> fun field -> field.current in
    let text () = Editor_document.Workspace_doc.to_text (E.workspace !current) in
    let box path = match E.node_box !current path with
      | Some (x, y, w, h) -> x, y, w, h | None -> failwith "the node has no box in the graph pane" in
    let header path = let x, y, w, _ = box path in float (x + w / 2), float (y + 10) in
    let undo () = step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 'z')] in
    let set path n = match E.edit !current (Flow_graph.Flow_edit.Set_arg { node = path; key = Kw "value";
        sub = []; value = S.make (S.Num (string_of_int n)) }) with
      | Ok env -> current := env | Error message -> failwith message in
    (* select the second source with a click, then edit it: the selection follows the stable node *)
    step []; step [];
    let selected = header [ "g"; "second" ] in
    step ~mouse:selected (click selected); step [];
    let before = text () in
    let before_draws = Atomic.get draws in
    set [ "g"; "second" ] 7;
    check (value first = Param.Int_value 0 && value second = Param.Int_value 7)
      "selecting and editing in one frame changed the wrong stable node";
    check (E.can_undo !current) "the edit did not enter document history";
    wait_draw before_draws;
    let before_draws = Atomic.get draws in
    undo ();
    check (value first = Param.Int_value 0 && value second = Param.Int_value 1
           && E.can_redo !current && text () = before)
      "undo did not restore the history present";
    wait_draw before_draws;
    check (Atomic.get draws >= 3) "a changed graph with the same prepared value retained a stale drawing";
    (* one drag is one undo entry, and two drags are two *)
    let drag path dx =
      let at = header path in
      step ~mouse:at ~buttons:[Input.LeftButton] [Input.Pointer_pressed (Input.LeftButton, at)];
      List.iter (fun step_dx -> let target = fst at +. step_dx, snd at in
        step ~mouse:target ~buttons:[Input.LeftButton] [Input.Pointer_moved target]) [ dx /. 4.; dx /. 2.; dx *. 3. /. 4. ];
      let target = fst at +. dx, snd at in
      step ~mouse:target [Input.Pointer_released (Input.LeftButton, target)] in
    count := !count + 90;
    let unmoved = text () in
    drag [ "g"; "second" ] 24.;
    check (text () <> unmoved) "dragging a node did not change the saved layout";
    let first_move = text () in
    count := !count + 90;
    drag [ "g"; "second" ] 24.;
    undo ();
    check (text () = first_move) "separate node drags merged after release";
    undo ();
    check (text () = unmoved) "one drag did not undo as one entry";
    print_endline "editor transactions: stable selection/edit target, undo agreement, drawing reuse and one-entry drags passed")
