open Rays

let fail message = raise (Failure message)
let checks = ref 0
let check condition message = incr checks; if not condition then fail message
let pointer (x, y) = float x, float y
let mouse_press (button, point) = Input.Pointer_pressed (button, pointer point)
let mouse_release (button, point) = Input.Pointer_released (button, pointer point)
let mouse_move point = Input.Pointer_moved (pointer point)

let frame ?(mouse = 0, 0) ?(keys = []) ?(events = []) () : Frame.t = {
  input = { (Input.at_time 0.) with frame = 0; tick = 0; size = 1000, 700;
    pointer = float (fst mouse), float (snd mouse); keys; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
}

(* ------------------------------------------------------- the node menu *)

module Node_menu = Pxui_graph.Node_menu

(* The menu is built inside a UI frame; hit rectangles come from the previous frame, so each
   step first settles the layout without events. *)
let menu_ui = Pxui.Ui.create ~font_size:11 ()
let bounds = 20, 30, 800, 520
let menu_step menu (frame : Frame.t) =
  let settle = { frame with input = { frame.input with events = [] } } in
  match Pxui.Ui.frame menu_ui settle (fun ui -> Node_menu.update menu ui ~bounds) with
  | None, _ -> None, None
  | Some menu, _ -> Pxui.Ui.frame menu_ui frame (fun ui -> Node_menu.update menu ui ~bounds)

let menu_of entries = Node_menu.create ~x:400 ~y:250 entries
let entry ?(arity = 0) ?(category = [ "Utility" ]) key label = { Node_menu.key; label; category; arity; context = "sop"; output = Flow.Ty.geometry; off = None }
let picks menu events = snd (menu_step menu (frame ~mouse:(400, 250) ~events ()))
let opened entries = match menu_step (menu_of entries) (frame ~mouse:(400, 250) ()) with
  | Some menu, _ -> menu | None, _ -> fail "the menu closed without input"

let run_menu () =
  (* a SOP with an input picks like any other: typing its name and Enter *)
  let null = [ entry ~arity:1 "null" "Null" ] in
  check (picks (opened null) [ Input.Text_input "null"; Input.Key_pressed Input.Enter ] = Some "null")
    "the menu search did not pick a node";
  check (menu_step (opened null) (frame ~mouse:(400, 250) ~events:[ Input.Key_pressed Input.Escape ] ())
         = (None, None))
    "Escape did not close the menu";
  (* the kinds are listed from the start: Enter places the first row, and a click on a row picks it.
     The first row's middle: the window's 1-point edge, its 28-point title (4 above a 24-point
     label), the 28-point search row (a 20-point field in 4 points of padding), then half a
     24-point row. *)
  let nested = [ entry ~category:[ "Create"; "Primitive" ] "box" "Box" ] in
  check (picks (opened nested) [ Input.Key_pressed Input.Enter ] = Some "box")
    "the menu did not list its kinds from the start";
  let row_y = 250 + 1 + 28 + 28 + 12 in
  let point = 420., float row_y in
  let _, picked = menu_step (opened nested) (frame ~mouse:(420, row_y)
    ~events:[ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ] ()) in
  check (picked = Some "box") "a click on a row of the menu did not pick it";
  (* after a node, the kinds that take an input come first *)
  let fitting = Node_menu.create ~after:"grid1" ~x:400 ~y:250
    [ entry "box" "Box"; entry ~arity:1 "null" "Null" ] in
  check (Node_menu.Private.keys fitting ~query:"" = [ "null"; "box" ])
    "the menu did not list what fits after the node first";
  (* one colour per type, for the menu's squares and the pane's ports: a list is its elements' *)
  check (Node_menu.port_color Pxui.default_theme (Flow.Ty.List Flow.Ty.Float) = (Pxui.Theme.ports Pxui.default_theme).float
         && Node_menu.port_color Pxui.default_theme Flow.Ty.scene = (Pxui.Theme.ports Pxui.default_theme).output)
    "a list or a scene is not coloured as the graph pane colours it";
  (* the visual row window never makes later nodes inaccessible *)
  let many = List.init 15 (fun index -> entry (Printf.sprintf "node_%02d" index) (Printf.sprintf "Node %02d" index)) in
  let menu = match menu_step (opened many) (frame ~mouse:(400, 250) ~events:[ Input.Text_input "node" ] ()) with
    | Some menu, _ -> menu | None, _ -> fail "typing closed the menu" in
  let arrows = List.init 12 (fun _ -> Input.Key_pressed Input.ArrowDown) in
  check (picks menu (arrows @ [ Input.Key_pressed Input.Enter ]) = Some "node_12")
    "the menu's row window made later nodes inaccessible";
  (* global search matches a category breadcrumb and uses the shared fuzzy rule *)
  check (picks (opened nested) [ Input.Text_input "primitive"; Input.Key_pressed Input.Enter ] = Some "box")
    "global search did not match a category breadcrumb";
  check (picks (opened nested) [ Input.Text_input "prmtv"; Input.Key_pressed Input.Enter ] = Some "box")
    "node search did not use the shared fuzzy matching rule";
  (* every generated SOP can be reached by typing its key *)
  let entries = List.map (fun factory -> Node_menu.{
    key = Sop.Edit_graph.factory_key factory;
    label = Sop.Edit_graph.factory_label factory;
    category = Sop.Edit_graph.factory_category factory;
    arity = Sop.Edit_graph.factory_arity factory;
    context = "sop"; output = Flow.Ty.geometry; off = None }) Sop.Nodes.factories in
  check (List.length entries = List.length Sop.Nodes.factories)
    "the menu conversion dropped a generated SOP descriptor";
  List.iter (fun (e : Node_menu.entry) ->
    let menu = opened entries in
    let index = Node_menu.Private.keys menu ~query:e.key
      |> List.mapi (fun i key -> i, key)
      |> List.find_map (fun (i, key) -> if key = e.key then Some i else None) |> Option.get in
    check (picks menu (Input.Text_input e.key :: List.init index (fun _ -> Input.Key_pressed Input.ArrowDown)
                       @ [ Input.Key_pressed Input.Enter ]) = Some e.key)
      ("the menu cannot reach generated SOP " ^ e.key)) entries;
  print_endline "pxui graph node menu tests passed"

(* ------------------------------------------------- the workspace pane (W4) *)

module Scope = Pxui_graph.Scope
module P = Flow_graph.Projection

let cases = "../specification/workspace/cases"
let scope_catalog = Editor_document.Contexts.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok
let load_workspace name =
  let text = In_channel.with_open_bin (Filename.concat cases (name ^ ".lisp")) In_channel.input_all in
  match Flow.Syntax.parse text with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok forms -> (match Flow.Workspace.check scope_catalog forms with
      | Some w, _ -> w
      | None, _ -> fail (name ^ " did not check"))
let scope_ui = Pxui.Ui.create ~font_size:11 ()
let scope_step view (frame : Frame.t) =
  let settle = { frame with input = { frame.input with events = [] } } in
  let view, _ = Pxui.Ui.frame scope_ui settle (fun ui -> Scope.update view ui settle) in
  Pxui.Ui.frame scope_ui frame (fun ui -> Scope.update view ui frame)
let recorded ?inputs workspace =
  Flow_graph.Probe.make (Result.get_ok (Flow.Eval.static ~record:true ?inputs workspace))
let scope_view ?inputs ?probe ?at ?level ?collapsed ?folded workspace graph =
  let scope = P.of_graph ?folded scope_catalog workspace graph in
  Scope.create ~width:1000 ~height:700 ()
  |> Scope.with_scope ?probe ?at ?level ?collapsed ~key:graph scope
  |> Scope.with_records (recorded ?inputs workspace), scope
let scope_paint view =
  ignore (scope_step view (frame ()));
  match Scene.Private.stage_native ~width:1000 ~height:700 (Pxui.Ui.scene scope_ui) with
  | Error message -> fail message
  | Ok staged -> List.fold_left (fun total -> function
      | Scene.Private.Ui_layer (batch, _) -> total + Scene_command.Ui_batch.count batch
      | _ -> total) 0 staged.layers
let click point = [mouse_press (Input.LeftButton, point); mouse_release (Input.LeftButton, point)]
let scope_click view point =
  scope_step view (frame ~mouse:point ~events:(mouse_move point :: click point) ())
let rect_center (x, y, w, h) = int_of_float (x +. w /. 2.), int_of_float (y +. h /. 2.)

(* Zoomed out, a node is its shown level's geometry: at every zoom, and with one card pinned
   full among points, each wire end is a port of a node (the dot for a point, the two ends of a
   chip, the sockets of a card). *)
let scope_zoom_geometry () =
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace z (graph g :context sop [(n : int 3)] (let* [a (sop/box) b (sop/scatter a :count n) c (sop/copy_to_points a b) d (sop/transform c)] (sop/merge c d))))"
    |> Result.get_ok in
  let level pinned p = if pinned && p = [ "g"; "b" ] then Some (P.Full, true) else None in
  let scope = P.of_graph scope_catalog ws.checked "g" in
  let paths = List.map (fun (n : P.node) -> n.path) scope.nodes
    @ List.map (fun (i : P.input) -> i.path) scope.inputs in
  List.iter (fun (pinned, factor, expect) ->
    let view, _ = scope_view ~level:(level pinned) ws.checked "g" in
    let view, _ = scope_step view (frame ()) in
    let mouse = (500, 350) in
    let view, _ = scope_step view (frame ~mouse ~events:[ mouse_move mouse; Input.Pinched (factor /. Scope.zoom view) ] ()) in
    let view, _ = scope_step view (frame ~mouse ~events:[] ()) in
    let ports = List.concat_map (Scope.Private.ports view) paths in
    let at (x, y) = List.exists (fun (px, py) -> Float.abs (px -. x) < 0.01 && Float.abs (py -. y) < 0.01) ports in
    let count = Scope.Private.wire_count view in
    check (count > 0) "no wires to check";
    for i = 0 to count - 1 do
      match Scope.Private.wire_points view i with
      | first :: _ as pts ->
          let last = List.nth pts (List.length pts - 1) in
          check (at first && at last)
            (Printf.sprintf "zoom %.2f%s: wire %d does not end on a port" (Scope.zoom view) (if pinned then " (b full)" else "") i)
      | [] -> fail "an empty wire"
    done;
    check (expect (Scope.zoom view)) "the pinch did not reach the zoom under test";
    (* the zoom never reduces a card: b has a written row at every zoom *)
    let _, _, _, bh = Option.get (Scope.Private.box_of view [ "g"; "b" ]) in
    check (bh > (P.head_height +. P.row_height) *. Scope.zoom view -. 0.01)
      (Printf.sprintf "zoom %.2f: a card lost its rows" (Scope.zoom view));
    (* a click on a node's box, at whatever level it is shown, selects it *)
    if Scope.zoom view < 0.5 then List.iter (fun path ->
      let x, y, w, h = Option.get (Scope.Private.box_of view path) in
      let view, _ = scope_click view (int_of_float (x +. w /. 2.), int_of_float (y +. Float.min (h /. 2.) (11. *. Scope.zoom view))) in
      check (Scope.selected view = [ path ]) ("a click on " ^ String.concat "/" path ^ " did not select it")) paths)
    [ false, 1., (fun z -> z >= 0.5); false, 0.45, (fun z -> z < 0.5 && z >= 0.34);
      false, 0.3, (fun z -> z < 0.34); true, 0.3, (fun z -> z < 0.34); true, 0.45, (fun z -> z < 0.5) ]

(* Levels, the header's in-port and letter hints (flow.md 6.4, 7.5; graph.html). *)
let scope_levels () =
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace levels (graph g :context sop (let* [a (sop/box) b (sop/box) c (sop/transform a) d (sop/transform b)] (sop/merge c d))))"
    |> Result.get_ok in
  let a = [ "g"; "a" ] and b = [ "g"; "b" ] and c = [ "g"; "c" ] in
  let view, scope = scope_view ~at:(fun p ->
    if p = a then Some (24., 24.) else if p = b then Some (336., 24.) else if p = c then Some (648., 24.)
    else if p = [ "g"; "d" ] then Some (336., 360.) else if p = [ "g"; "@result" ] then Some (960., 360.) else None)
    ws.checked "g" in
  let view, _ = scope_step view (frame ()) in
  (* the first geometry slot is the header's in-port, never a row *)
  let node_c = Option.get (P.find scope c) in
  check (List.exists (fun (r : P.row) -> r.head) node_c.rows) "the first geometry slot is not the header's";
  (* o opens the selection one level and pins it; p points it, again goes back *)
  let sel = Scope.select [ a ] view in
  let _, changes = Scope.run_command sel Scope.Open_level in
  check (changes = [ Scope.Level_set [ a, Some P.Full, true ] ]) "o is not Level_set full, pinned";
  let pointed, changes = Scope.run_command sel Scope.Point_level in
  check (changes = [ Scope.Level_set [ a, Some P.Point, false ] ]) "p is not Level_set point";
  ignore pointed;
  let pview, _ = scope_view ~at:(fun p -> if p = a then Some (24., 24.) else None)
    ~level:(fun p -> if p = a then Some (P.Point, false) else None) ws.checked "g" in
  let pview, _ = scope_step pview (frame ()) in
  let _, _, pw, ph = Option.get (Scope.Private.box_of pview a) in
  check (ph = P.point_size *. Scope.zoom pview && pw < 100. *. Scope.zoom pview) "a point is the 14-point disc and its name";
  let back, changes = Scope.run_command (Scope.select [ a ] pview) Scope.Point_level in
  ignore back;
  check (match changes with [ Scope.Level_set [ p, l, false ] ] -> p = a && l = None | _ -> false)
    "p on a point does not go back to the card";
  (* the header's in-port takes a wire: dropped anywhere on the header it is Connect on the first slot *)
  let sx, sy = Option.get (Scope.Private.output_socket view b) in
  let tx, ty = Option.get (Scope.Private.row_center view c 0) in
  let start = int_of_float sx, int_of_float sy and stop = int_of_float tx, int_of_float ty in
  let v, _ = scope_step view (frame ~mouse:start ~events:[ mouse_move start; mouse_press (Input.LeftButton, start) ] ()) in
  let v, _ = scope_step v (frame ~mouse:stop ~events:[ mouse_move stop ] ()) in
  let _, changes = scope_step v (frame ~mouse:stop ~events:[ mouse_release (Input.LeftButton, stop) ] ()) in
  check (List.exists (function
    | Scope.Syntax_edit (Flow_graph.Flow_edit.Connect { node; key = Flow_graph.Flow_edit.Pos 0; src = "b"; _ }) -> node = c
    | _ -> false) changes) "a wire dropped on the header did not connect the first slot";
  (* a wire let go on empty canvas selects its source and asks for the add menu there *)
  let empty = 2900, 1900 in
  let v, _ = scope_step view (frame ~mouse:start ~events:[ mouse_move start; mouse_press (Input.LeftButton, start) ] ()) in
  let v, _ = scope_step v (frame ~mouse:empty ~events:[ mouse_move empty ] ()) in
  let v, changes = scope_step v (frame ~mouse:empty ~events:[ mouse_release (Input.LeftButton, empty) ] ()) in
  check (List.mem (Scope.Selected [ b ]) changes && List.exists (function Scope.Menu_requested _ -> true | _ -> false) changes
    && Scope.selected v = [ b ]) "a wire let go on empty canvas did not ask for the node menu after its source";
  (* a wire itself pulled into empty canvas does the same, and stays selected: the host inserts on it *)
  (match List.find_opt (fun i -> match Scope.Private.wire_target view i with Some (n, _) -> n = c | None -> false)
           (List.init (Scope.Private.wire_count view) Fun.id) with
   | None -> fail "no wire into c"
   | Some i ->
       let ax, _, aw, _ = Option.get (Scope.Private.box_of view a)
       and bx, _, _, _ = Option.get (Scope.Private.box_of view b) in
       let mx, my = List.find (fun (x, _) -> x > ax +. aw +. 4. && x < bx -. 4.)
           (Scope.Private.wire_points view i) in
       let mid = int_of_float mx, int_of_float my in
       let v, _ = scope_step view (frame ~mouse:mid ~events:[ mouse_move mid; mouse_press (Input.LeftButton, mid) ] ()) in
       let v, _ = scope_step v (frame ~mouse:empty ~events:[ mouse_move empty ] ()) in
       let v, changes = scope_step v (frame ~mouse:empty ~events:[ mouse_release (Input.LeftButton, empty) ] ()) in
       check (List.mem (Scope.Selected [ a ]) changes && List.exists (function Scope.Menu_requested _ -> true | _ -> false) changes
         && Scope.selected v = [ a ] && Option.map fst (Scope.selected_wire v) = Some c)
         "a wire pulled into empty canvas did not ask for the node menu with the wire selected";
       (* a short press-release on the wire is still just a click *)
       let v, _ = scope_step view (frame ~mouse:mid ~events:[ mouse_move mid; mouse_press (Input.LeftButton, mid) ] ()) in
       let _, changes = scope_step v (frame ~mouse:mid ~events:[ mouse_release (Input.LeftButton, mid) ] ()) in
       check (not (List.exists (function Scope.Menu_requested _ -> true | _ -> false) changes)) "a click on a wire opened the menu";
       (* Alt: the wire is lifted off its input; empty canvas disconnects it, no menu *)
       let v, _ = scope_step view (frame ~mouse:mid ~keys:[ Input.Alt ] ~events:[ mouse_move mid; mouse_press (Input.LeftButton, mid) ] ()) in
       let v, _ = scope_step v (frame ~mouse:empty ~keys:[ Input.Alt ] ~events:[ mouse_move empty ] ()) in
       let _, changes = scope_step v (frame ~mouse:empty ~events:[ mouse_release (Input.LeftButton, empty) ] ()) in
       check (List.exists (function Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect { node; _ }) -> node = c | _ -> false) changes
         && not (List.exists (function Scope.Menu_requested _ -> true | _ -> false) changes))
         "an Alt-drag of a wire into empty canvas did not disconnect it";
       (* Alt on b's out-port lifts its wire too, and dropped on c's first row moves it there *)
       let v, _ = scope_step view (frame ~mouse:start ~keys:[ Input.Alt ] ~events:[ mouse_move start; mouse_press (Input.LeftButton, start) ] ()) in
       let v, _ = scope_step v (frame ~mouse:empty ~keys:[ Input.Alt ] ~events:[ mouse_move empty ] ()) in
       let _, changes = scope_step v (frame ~mouse:empty ~events:[ mouse_release (Input.LeftButton, empty) ] ()) in
       check (List.exists (function Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect _) -> true | _ -> false) changes)
         "an Alt-drag of an out-port into empty canvas did not take its wires off");
  (* a frame's own variable leaves its port on the begin bar (20 points inside the left edge)
     straight into its body: the wire stays inside the frame, below the label row *)
  let mw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace m (graph g :context value (let* [boxes (array/vec3 8) ys (map (fn [i] (let* [p (array/nth boxes (int i))] [p.x p.y 0])) (array/range 4))] (array/count ys))))"
    |> Result.get_ok in
  let mview, _ = scope_view mw.checked "g" in
  let mview, _ = scope_step mview (frame ()) in
  let zx, zy, zw, _ = Option.get (Scope.Private.box_of mview [ "g"; "ys#0" ]) in
  let z = Scope.zoom mview in
  let from_i = List.filter (fun i -> match Scope.Private.wire_points mview i with
    | (x, _) :: _ -> x > zx && x <= zx +. 24. *. z | [] -> false) (List.init (Scope.Private.wire_count mview) Fun.id) in
  check (from_i <> []) "the frame's variable has no wire from its begin bar";
  List.iter (fun i -> List.iter (fun (x, y) ->
    check (x >= zx && x <= zx +. zw -. 3. *. z && y > zy +. P.head_height *. z)
      "a frame's variable wire left the frame or crossed its label row") (Scope.Private.wire_points mview i)) from_i;
  (* f: letter hints; a letter connects the selected output *)
  let hint_view, notices = Scope.run_command (Scope.select [ b ] view) Scope.Show_hints in
  check (notices = [] && Scope.editing hint_view) "f did not start the hints";
  let hint_view, _ = scope_step hint_view (frame ()) in  (* the key that began it *)
  (* one frame with the key down: scope_step's settle frame would take the press *)
  let one_frame key v =
    let f = frame ~keys:[ key ] () in
    Pxui.Ui.frame scope_ui f (fun ui -> Scope.update v ui f) in
  let v, changes = one_frame (Input.KeyChar 'a') hint_view in
  let connects = List.filter (function Scope.Syntax_edit (Flow_graph.Flow_edit.Connect { src = "b"; _ }) -> true | _ -> false)
    changes in
  check (connects <> [] && not (Scope.editing v)) "a hint letter did not connect and end the hints";
  let esc, _ = one_frame Input.Escape hint_view in
  check (not (Scope.editing esc)) "Escape did not end the hints";
  (* a reloaded document ends them: their targets name nodes of the scope they were made over *)
  check (Scope.editing (Scope.with_scope ~key:"g" scope hint_view))
    "the same scope ended the hints";
  let reloaded = Editor_document.Workspace_doc.of_text scope_catalog "(workspace w (graph g :context sop (sop/box)))" |> Result.get_ok in
  check (not (Scope.editing (Scope.with_scope ~key:"g" (P.of_graph scope_catalog reloaded.checked "g") hint_view)))
    "a new scope kept the letter hints";
  (* f frames the selection (all with none); the hints are on w *)
  let key_of action = List.find_map (fun (c : _ Editor_core.Command.t) ->
    if c.action = action then c.trigger else None) Scope.bindings in
  check (key_of Scope.Frame_selection = Some (Editor_core.Keymap.Chord (Input.KeyChar 'f', []))) "f does not frame the selection";
  check (key_of Scope.Show_hints = Some (Editor_core.Keymap.Chord (Input.KeyChar 'w', []))) "w does not start the hints";
  (* a node named as the graph it refers to is not wired to itself *)
  let sw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace s (graph g :context sop (sop/box)) (graph scene :context scene (let* [g (scene/geometry (ref g))] (scene/merge g))))"
    |> Result.get_ok in
  let sview, _ = scope_view sw.checked "scene" in
  let sview, _ = scope_step sview (frame ()) in
  check (List.for_all (fun i -> match Scope.Private.wire_target sview i with
    | Some (path, _) -> path <> [ "scene"; "g" ] | None -> true) (List.init (Scope.Private.wire_count sview) Fun.id))
    "a node that refers to a graph of its own name is wired to itself";
  (* v on a node inside a loop asks for it too: the host shows it at the probed iteration *)
  let lw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace l (graph g :context sop [(n : int 3)] (let* [k 0.4 a (sop/uv_sphere :radius k :segments n) copies (for [i (range 2)] (sop/transform a :translate [i 0 0]))] (sop/merge copies))))"
    |> Result.get_ok in
  let lview, lscope = scope_view lw.checked "g" in
  let lview, _ = scope_step lview (frame ()) in
  let inner = [ "g"; "copies"; "@result" ] in
  check (snd (Scope.run_command (Scope.select [ inner ] lview) Scope.Display) = [ Scope.Display_set inner ])
    "v refused a node inside a loop";
  (* a row wired from a node nothing else reads has a fold button; a click folds that node into the row.
     A row wired from a graph input has none. *)
  let sphere = [ "g"; "a" ] in
  let rows = (Option.get (P.find lscope sphere)).rows in
  let row label = Option.get (List.find_index (fun (r : P.row) -> r.label = label) rows) in
  check (Scope.Private.fold_button lview sphere (row "segments") = None) "a row wired from an input has a fold button";
  let fx, fy = match Scope.Private.fold_button lview sphere (row "radius") with
    | Some at -> at | None -> fail "a row wired from a single-use node has no fold button" in
  let _, changes = scope_click lview (int_of_float fx, int_of_float fy) in
  check (List.mem (Scope.Syntax_edit (Flow_graph.Flow_edit.Fold_into { node = [ "g"; "k" ] })) changes)
    "a click on the fold button did not fold the source into the row";
  (* taking the wire off a named argument writes the value it carried (k is 0.4): the row stays on the card *)
  let rx, ry = Option.get (Scope.Private.row_center lview sphere (row "radius")) in
  let at = int_of_float rx, int_of_float ry in
  let hovered, _ = scope_step lview (frame ~mouse:at ~events:[ mouse_move at ] ()) in
  (match snd (Scope.run_command hovered Scope.Delete) with
   | [ Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect { node; key = Flow_graph.Flow_edit.Kw "radius"; fallback = Some f }) ] ->
       check (node = sphere && Flow.Lisp.flat f = "0.4") ("the wire taken off radius did not keep its last value: " ^ Flow.Lisp.flat f)
   | _ -> fail "Delete over a wired row is not a Disconnect with the last value")

let scope_connection_hover () =
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace hover (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform a)] (sop/merge b c))))"
    |> Result.get_ok in
  let view, _ = scope_view ~at:(function
    | ["g"; "a"] -> Some (0., 0.) | ["g"; "b"] -> Some (400., 0.)
    | ["g"; "c"] -> Some (400., 240.)
    | ["g"; "@result"] -> Some (800., 600.) | _ -> None) ws.checked "g" in
  let view, _ = scope_step view (frame ()) in
  let highlight point =
    let point = int_of_float (fst point), int_of_float (snd point) in
    let hovered, changes = scope_step view (frame ~mouse:point ~events:[mouse_move point] ()) in
    check (changes = []) "hover edited the graph";
    Scope.Private.highlighted_connections hovered in
  let source = Option.get (Scope.Private.output_socket view ["g"; "a"]) in
  let fanout = highlight source in
  check (List.length fanout = 2) "an output hover did not highlight its two connections";
  let ax, ay = source in
  let bx, _, _, _ = Option.get (Scope.Private.box_of view ["g"; "c"]) in
  let _, by = Option.get (Scope.Private.row_center view ["g"; "c"] 0) in
  let wire_hover = highlight ((ax +. bx) /. 2., (ay +. by) /. 2.) in
  check (List.length wire_hover = 1)
    (Printf.sprintf "hover on a wire did not isolate its connection: %d at %.1f %.1f; %s"
      (List.length wire_hover) ((ax +. bx) /. 2.) ((ay +. by) /. 2.)
      (String.concat "; " (List.map (fun ((a,b),(c,d)) -> Printf.sprintf "%.1f,%.1f -> %.1f,%.1f" a b c d) fanout)));
  check (List.length (highlight (bx, by)) = 1) "an input hover did not isolate its connection";
  check (highlight (999., 699.) = []) "connections stayed highlighted after leaving them";
  let midpoint = int_of_float ((ax +. bx) /. 2.), int_of_float ((ay +. by) /. 2.) in
  let selected, _ = scope_click view midpoint in
  check (match snd (Scope.run_command selected Scope.Delete) with
    | [Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect {node=["g";"c"]; key=Pos 0; _})] -> true
    | _ -> false) "a press on the wire did not preserve its disconnect target";
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace capture (graph g :context sop (let* [a (sop/box) copies (for [i (range 2)] (sop/transform a :translate [i 0 0]))] (sop/merge copies))))"
    |> Result.get_ok in
  let view, _ = scope_view ws.checked "g" in
  let view, _ = scope_step view (frame ()) in
  (* a captured value is wired straight across the zone's edge from the node that makes it *)
  let ax, ay = Option.get (Scope.Private.output_socket view [ "g"; "a" ]) in
  let point = int_of_float ax, int_of_float ay in
  let hovered, changes = scope_step view (frame ~mouse:point ~events:[mouse_move point] ()) in
  check (changes = [] && List.length (Scope.Private.highlighted_connections hovered) = 1)
    "hover on a captured value's socket missed its outer connection"

(* [n] nodes in one scope: a chain with a long wire back at every seventh node *)
let big_text n =
  let b = Buffer.create 65536 in
  Buffer.add_string b "(workspace big (graph g :context sop (let* [n0 (sop/box)";
  for i = 1 to n - 1 do
    Printf.bprintf b " n%d (sop/transform n%d :translate [%d 0 0])" i (if i mod 7 = 0 then i / 2 else i - 1) i
  done;
  Printf.bprintf b "] n%d)))" (n - 1);
  Buffer.contents b
let big_scope n =
  let ws = Editor_document.Workspace_doc.of_text scope_catalog (big_text n) |> Result.get_ok in
  P.of_graph scope_catalog ws.checked "g"

(* Frame work does not scale with what is out of view: an idle frame of a pane that shows no card
   of a 2,001-node graph allocates a small constant (it was 6.3 MB when every wire was cut into
   hit boxes before the viewport test), and the level keys answer for every node at once. *)
let scope_idle_frame () =
  let ui = Pxui.Ui.create ~font_size:11 () in
  let view = ref (Scope.create ~width:16 ~height:16 () |> Scope.with_scope ~key:"g" (big_scope 2001)) in
  let step () = view := fst (Pxui.Ui.frame ui (frame ()) (fun ui -> Scope.update !view ui (frame ()))) in
  for _ = 1 to 3 do step () done;
  let before = Gc.allocated_bytes () in
  for _ = 1 to 10 do step () done;
  let per_frame = (Gc.allocated_bytes () -. before) /. 10. in
  check ((Scope.stats !view).nodes >= 2001) "the big graph is not laid out";
  check (per_frame < 500_000.) (Printf.sprintf "an idle frame with nothing in view allocates %.0f bytes" per_frame);
  check (match snd (Scope.run_command !view Scope.Point_all) with
    | [ Scope.Level_set l ] -> List.length l = 2001 && List.for_all (fun (_, level, _) -> level = Some P.Point) l
    | _ -> false) "Point_all did not point every node"

(* A pane that stops being drawn keeps nothing open: a pan gives the pointer back and a text field closes. *)
let scope_suspend () =
  let w = load_workspace "bloom" in
  let view, _ = scope_view w "flower" in
  let view, _ = scope_step view (frame ()) in
  let empty = 990, 690 in
  let held = { (frame ~mouse:empty ~events:[ mouse_move empty; mouse_press (Input.RightButton, empty) ] ()) with
    input = { (frame ~mouse:empty ~events:[ mouse_move empty; mouse_press (Input.RightButton, empty) ] ()).input with
      buttons = [ Input.RightButton ] } } in
  let panning, _ = scope_step view held in
  check (Scope.Private.grabbed panning) "a right-button press on the canvas did not begin a pan";
  let naming, _ = Scope.run_command (Scope.select [ [ "flower"; "heart" ] ] panning) Scope.Edit_name in
  check (Scope.editing naming && Scope.Private.grabbed naming) "the name field did not open during the pan";
  let idle = Scope.suspend naming in
  check (not (Scope.Private.grabbed idle) && not (Scope.editing idle)) "suspend left the grab or the field";
  check (Scope.suspend idle == idle) "suspending an idle pane made a new value";
  (* drawn again as the button comes up: no grab, no field, no edit *)
  let after, changes = scope_step idle (frame ~mouse:empty ~events:[ mouse_release (Input.RightButton, empty) ] ()) in
  let after, later = scope_step after (frame ~mouse:empty ()) in
  let changes = changes @ later in
  check (not (Scope.Private.grabbed after) && not (Scope.editing after)
         && not (List.exists (function Scope.Syntax_edit _ | Scope.Moved _ -> true | _ -> false) changes))
    "the pane woke with a grab, a field or an edit"

(* A trackpad pinch zooms the graph where the wheel does: at the pointer, the
   frame's pinch factors multiplied, clamped like the wheel. *)
let scope_pinch () =
  let w = load_workspace "bloom" in
  let view, _ = scope_view w "flower" in
  let view = fst (scope_step view (frame ())) in
  let before = Scope.zoom view in
  let point = 500, 350 in
  let zoomed events =
    Scope.zoom (fst (scope_step view (frame ~mouse:point ~events:(mouse_move point :: events) ()))) in
  let near a b = Float.abs (a -. b) < 1e-9 in
  check (before > 0.3 && before < 2.0) "the bloom framing leaves no room to zoom both ways";
  check (near (zoomed [ Input.Pinched 1.25 ]) (before *. 1.25))
    "a pinch did not zoom the graph by its factor";
  check (near (zoomed [ Input.Pinched 0.8 ]) (before *. 0.8))
    "a pinch below one did not zoom the graph out";
  check (near (zoomed [ Input.Pinched 1.1; Input.Pinched 1.1 ]) (before *. 1.21))
    "two pinches in a frame did not multiply";
  check (zoomed [ Input.Pinched 100. ] = 2.5) "a huge pinch escaped the zoom clamp";
  check (zoomed [ Input.Pinched 0.001 ] = 0.25) "a tiny pinch escaped the zoom clamp";
  check (near (zoomed [ Input.Scrolled (0., 1.) ]) (before *. 1.1))
    "the wheel no longer zooms by a tenth a step";
  (* the point under the pointer stays put, as with the wheel *)
  let anchored =
    let view = fst (scope_step view (frame ~mouse:point ~events:[ mouse_move point; Input.Pinched 1.5 ] ())) in
    Scope.zoom view in
  check (anchored > before) "pinch-out did not enlarge";
  (* a pinch outside the pane is not the pane's *)
  let outside = 3000, 3000 in
  let unmoved = fst (scope_step view (frame ~mouse:outside ~events:[ mouse_move outside; Input.Pinched 1.5 ] ())) in
  check (Scope.zoom unmoved = before) "a pinch away from the pane zoomed it"

(* W12b: every W3 gesture the pane can make reaches the host as its request. *)
(* A carried payload: the pane reports the node under the pointer (the path), or its empty canvas
   (the graph's own one-segment path), hovering and then released; it never edits. *)
let scope_carry () =
  let frame ?mouse ?keys ?events () =
    let frame = frame ?mouse ?keys ?events () in
    { frame with input = { frame.input with size = 3000, 2000 } } in
  let w = load_workspace "bloom" in
  let scope = P.of_graph scope_catalog w "flower" in
  let view = Scope.create ~width:3000 ~height:2000 () |> Scope.with_scope ~key:"flower" scope
    |> Scope.with_records (recorded w) in
  let heart = [ "flower"; "heart" ] in
  let view = fst (scope_step view (frame ())) in
  let x, y, bw, bh = Option.get (Scope.Private.box_of view heart) in
  let over = int_of_float (x +. bw /. 2.), int_of_float (y +. bh -. 4.) in
  let nothing = 2900, 1900 in
  let drops changes = List.filter_map (function
    | Scope.Drop_over { path; kind; value } -> Some (`Over, path, kind, value)
    | Dropped { path; kind; value } -> Some (`Dropped, path, kind, value)
    | _ -> None) changes in
  (* idle frames report nothing *)
  let view, changes = scope_step view (frame ~mouse:over ~events:[ mouse_move over ] ()) in
  check (drops changes = []) "a pane with nothing carried reported a drop";
  Pxui.Ui.carry scope_ui ~kind:"material" ~value:"(ref cobalt)" ();
  let view, changes = scope_step view (frame ~mouse:over ~events:[ mouse_move over ] ()) in
  check (drops changes = [ `Over, heart, "material", "(ref cobalt)" ]) "a carry over a node did not report the node's path";
  let view, changes = scope_step view (frame ~mouse:nothing ~events:[ mouse_move nothing ] ()) in
  check (drops changes = [ `Over, [ "flower" ], "material", "(ref cobalt)" ])
    "a carry over the empty canvas did not report the graph";
  let view, changes = scope_step view (frame ~mouse:over ~events:[ mouse_move over ] ()) in
  check (List.length (drops changes) = 1) "one place under the pointer";
  let view, changes = scope_step view (frame ~mouse:over ~events:[ mouse_press (Input.LeftButton, over) ] ()) in
  check (drops changes = [ `Dropped, heart, "material", "(ref cobalt)" ]) "a put over a node did not emit Dropped with its path";
  check (not (List.exists (function Scope.Syntax_edit _ | Selected _ | Moved _ -> true | _ -> false) changes))
    "the pane edited, selected or moved on a put";
  check (Pxui.Ui.carrying scope_ui = None) "the payload outlived its put";
  let _, changes = scope_step view (frame ~mouse:over ~events:[ mouse_release (Input.LeftButton, over) ] ()) in
  check (drops changes = []) "the release after a put reported a drop";
  (* the marks of a carry paint without changing what the pane reports *)
  let marked = Scope.with_carry ~lit:[ heart, "a" ] ~hot:(Some (heart, true)) view in
  ignore (scope_step marked (frame ()))

(* A card below Full has no footer; the value of its out-port at the pinned iteration is drawn beside it
   (a ghost value): [Private.ghost] reads it at the pinned iteration. *)
let scope_ghost_values () =
  let w = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace w (graph g :context value (let* [k 6.0 ys (for [i (range 4)] (let* [a (* k (float i)) b (+ a 1.0)] b)) n (reduce (fn [acc x] (+ acc x)) 0.0 ys)] n)))"
    |> function Ok d -> d | Error m -> fail (String.concat "; " (List.map Diagnostic.to_string m))).checked in
  let scope = P.of_graph scope_catalog w "g" in
  let view index = Scope.create ~width:1000 ~height:700 ()
    |> Scope.with_scope ~probe:(fun _ -> index) ~key:"g" scope |> Scope.with_records (recorded w) in
  let ghost index path = Scope.Private.ghost (fst (scope_step (view index) (frame ()))) path in
  check (ghost 0 [ "g"; "k" ] = Some "6") "a card below Full showed no value beside its out-port";
  check (ghost 0 [ "g"; "ys"; "b" ] = Some "1" && ghost 3 [ "g"; "ys"; "b" ] = Some "19")
    "the ghost value did not follow the pinned iteration";
  check (Scope.Private.ghost (fst (scope_step (Scope.create ~width:1000 ~height:700 () |> Scope.with_scope ~key:"g" scope) (frame ()))) [ "g"; "k" ] = None)
    "a ghost value without records"

(* The default cubic curve leaves and enters its ports level. *)
let scope_curve () =
  let w = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace c (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [0 3 0])] b)))" |> Result.get_ok).checked in
  let view, _ = scope_view w "g" in
  let view = fst (scope_step view (frame ())) in
  let pts = Scope.Private.wire_points view 0 in
  let level = function (_, y0) :: (_, y1) :: _ -> Float.abs (y1 -. y0) < 0.5 | _ -> false in
  check (List.length pts > 2 && level pts && level (List.rev pts)) "a curve wire does not leave and enter level";
  (* a loop variable's wire leaves its begin-bar port downward, clear of the name beside the port *)
  let w = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace f (graph g :context value (let* [total (fold [acc 0.0] [k (range 4)] (+ acc (* k 2.0)))] total)))"
    |> function Ok d -> d | Error d -> fail (String.concat "; " (List.map Diagnostic.to_string d))).checked in
  let view = fst (scope_step (fst (scope_view w "g")) (frame ())) in
  let down = function (x0, y0) :: (x1, y1) :: _ -> Float.abs (x1 -. x0) < 0.5 && y1 > y0 | _ -> false in
  check (List.exists (fun i -> down (Scope.Private.wire_points view i)) (List.init (Scope.Private.wire_count view) Fun.id))
    "no begin-bar wire leaves its port downward"

(* a formula in a record field opens as a binding: a field holds no nested card (it raised before) *)
let scope_field_formula () =
  let w = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace r (graph g :context value (let* [a 2.0 r {:x (+ a 1.0)}] r.x)))"
    |> function Ok d -> d | Error d -> fail (String.concat "; " (List.map Diagnostic.to_string d))).checked in
  let view = fst (scope_step (fst (scope_view w "g")) (frame ())) in
  let module E = Flow_graph.Flow_edit in
  let path = [ "g"; "r" ] in
  let row = Option.get (List.find_index (fun (r : P.row) -> r.key = E.Field "x") (Option.get (P.find (P.of_graph scope_catalog w "g") path)).rows) in
  let x, y = Option.get (Scope.Private.expr_chip view path row) in
  (* a double click on the formula opens it, as its glyph does *)
  let at = int_of_float x, int_of_float y in
  let unfolded = List.exists (function Scope.Syntax_edit (E.Unfold { key = E.Field "x"; _ }) -> true | _ -> false)
    (snd (scope_click (fst (scope_click view at)) at)) in
  check unfolded "the formula glyph of a record field did not unfold it"

(* a map's function stacked above it is wired by a spine straight down into the f socket, not a
   wire out of the zone's right side and back across it *)
let scope_spine () =
  let w = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace s (graph g :context value (let* [ys (map (fn [i] (* i 2.0)) (array/range 3))] (array/nth ys 0))))"
    |> function Ok d -> d | Error d -> fail (String.concat "; " (List.map Diagnostic.to_string d))).checked in
  let view = fst (scope_step (fst (scope_view w "g")) (frame ())) in
  let into_f = List.filter_map (fun i -> match Scope.Private.wire_target view i with
    | Some ([ "g"; "ys" ], Flow_graph.Flow_edit.Pos 0) -> Some (Scope.Private.wire_points view i) | _ -> None)
    (List.init (Scope.Private.wire_count view) Fun.id) in
  check (match into_f with
    | [ ((x0, y0) :: _ as pts) ] -> let x1, y1 = List.nth pts (List.length pts - 1) in Float.abs (x1 -. x0) < 0.5 && y1 > y0
    | _ -> false) "a map's stacked function is not wired straight down into its f socket"

let scope_gestures () =
  let module E = Flow_graph.Flow_edit in
  let module S = Flow.Syntax in
  let syntax op = Scope.Syntax_edit op in
  (* a large window keeps the zoom at 1, where every field is built *)
  let frame ?mouse ?keys ?events () =
    let frame = frame ?mouse ?keys ?events () in
    { frame with input = { frame.input with size = 3000, 2000 } } in
  let scope_click view point =
    scope_step view (frame ~mouse:point ~events:(mouse_move point :: click point) ()) in
  let center_of view path = let x, y, w, h = Option.get (Scope.Private.box_of view path) in x, y, w, h in
  let settled view = fst (scope_step view (frame ())) in
  let scope_view ?inputs ?probe ?at ?collapsed workspace graph =
    ignore (inputs, probe, at, collapsed);
    let scope = P.of_graph scope_catalog workspace graph in
    Scope.create ~width:3000 ~height:2000 () |> Scope.with_scope ~key:graph scope
    |> Scope.with_records (recorded workspace), scope in
  let double_click view point = scope_click (fst (scope_click view point)) point in
  let type_text view text =
    scope_step view (frame ~events:[ Input.Text_input text; Input.Key_pressed Input.Enter ] ()) in
  (* rename: a double-click on the title opens a field, Enter commits Rename; F2 does the same *)
  let w = load_workspace "bloom" in
  let heart = [ "flower"; "heart" ] in
  let view = settled (fst (scope_view w "flower")) in
  let x, y, _, _ = center_of view heart in
  let view, _ = double_click view (int_of_float x + 100, int_of_float y + 8) in
  check (Scope.editing view) "a double-click on a title did not open the name field";
  let view = settled view in
  let view, changes = type_text view "core" in
  check (List.mem (syntax (E.Rename { node = heart; to_ = "core" })) changes) "typing a name did not become Rename";
  check (not (Scope.editing view)) "the name field stayed open after Enter";
  let view, changes = Scope.run_command (Scope.select [ heart ] view) Scope.Edit_name in
  check (changes = [] && Scope.editing view) "F2 did not open the name field";
  let view = settled view in
  let view, changes = scope_step view (frame ~events:[ Input.Text_input "two words"; Input.Key_pressed Input.Enter ] ()) in
  check (changes = [] || not (List.exists (function Scope.Syntax_edit (E.Rename _) -> true | _ -> false) changes))
    "a name with a space was accepted";
  let view = fst (scope_step view (frame ~events:[ Input.Key_pressed Input.Escape ] ())) in
  check (not (Scope.editing view)) "Escape left the name field open";
  let _, changes = Scope.run_command (Scope.select [] view) Scope.Edit_name in
  check (match changes with [ Scope.Notice _ ] -> true | _ -> false) "F2 with nothing selected";
  (* a graph input's default: double-click its tile, type a form *)
  let petals = [ "flower"; ":petals" ] in
  let view = settled (Scope.clear_selection view) in
  let x, y, _, _ = center_of view petals in
  let view, _ = double_click view (int_of_float x + 60, int_of_float y + 12) in  (* a value node is its 24-point header *)
  check (Scope.editing view) "a double-click on a graph input did not open its default";
  let view = settled view in
  let _, changes = type_text view "9" in
  check (List.exists (function
    | Scope.Syntax_edit (E.Set_input_default { form = "flower"; input = "petals"; value = { S.node = S.Num "9"; _ } }) -> true
    | _ -> false) changes) "typing an input default did not become Set_input_default";
  (* scrub: a drag on a number field is Set_arg *)
  let view = settled (fst (scope_view w "flower")) in
  let node = Option.get (P.find (P.of_graph scope_catalog w "flower") heart) in
  let i = Option.get (List.find_index (fun (r : P.row) -> r.label = "radius") node.rows) in
  let fx, fy = Option.get (Scope.Private.row_center view heart i) in
  let from = int_of_float fx, int_of_float fy in
  let view, _ = scope_step view (frame ~mouse:from ~events:[ mouse_move from; mouse_press (Input.LeftButton, from) ] ()) in
  let towards = fst from + 40, snd from in
  let view, moved = scope_step view (frame ~mouse:towards ~events:[ mouse_move towards ] ()) in
  let _, released = scope_step view (frame ~mouse:towards ~events:[ mouse_release (Input.LeftButton, towards) ] ()) in
  check (List.exists (function Scope.Syntax_edit (E.Set_arg { node; sub = [ _ ]; _ }) -> node = heart | _ -> false)
           (moved @ released)) "scrubbing a vector field did not become Set_arg";
  (* A float parameter written as an integer scrubs as a float (the kind is the row's type), a
     long value keeps its digits, a click writes nothing and a drag is one history entry *)
  let nw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace n (graph g :context sop (let* [a (sop/uv_sphere :radius 1) b (sop/transform a :translate [1234567.89 2 3])] b)))"
    |> Result.get_ok in
  let field path label ~inset =
    let view = settled (fst (scope_view nw.checked "g")) in
    let node = Option.get (P.find (P.of_graph scope_catalog nw.checked "g") path) in
    let i = Option.get (List.find_index (fun (r : P.row) -> r.label = label) node.rows) in
    let bx, _, bw, _ = Option.get (Scope.Private.box_of view path) in
    view, (int_of_float (bx +. bw -. inset), int_of_float (snd (Option.get (Scope.Private.row_center view path i)))) in
  let scrub (view, from) by =
    let view, _ = scope_step view (frame ~mouse:from ~events:[ mouse_move from; mouse_press (Input.LeftButton, from) ] ()) in
    let towards = fst from + by, snd from in
    let view, moved = if by = 0 then view, [] else scope_step view (frame ~mouse:towards ~events:[ mouse_move towards ] ()) in
    let _, released = scope_step view (frame ~mouse:towards ~events:[ mouse_release (Input.LeftButton, towards) ] ()) in
    List.filter_map (function Scope.Syntax_edit (E.Set_arg _ as op) -> Some op | _ -> None) (moved @ released) in
  let written ops = List.filter_map (function
    | E.Set_arg { value = { S.node = S.Num n; _ }; _ } -> Some n | _ -> None) ops in
  (* at two places: a second press at one would be a double-click, which types *)
  check (scrub (field [ "g"; "a" ] "radius" ~inset:20.) 0 = []) "a click on a number field wrote a value";
  let radius = scrub (field [ "g"; "a" ] "radius" ~inset:60.) 10 in
  check (match written radius with
    | n :: _ -> String.contains n '.' && not (Float.is_integer (float_of_string n)) | [] -> false)
    ("a float written 1 scrubbed as an integer: " ^ String.concat " " (written radius));
  check (List.for_all (fun op -> E.gesture op <> None && E.gesture op = E.gesture (List.hd radius)) radius)
    "a scrub is not one history entry";
  let cell = scrub (field [ "g"; "b" ] "translate" ~inset:110.) 2 in
  check (List.mem "1234567.99" (written cell)) ("a scrub of 1234567.89 lost its digits: " ^ String.concat " " (written cell));
  (* Explicit non-finite values are editable on value cards, while ranged
     catalog fields keep their finite-value boundary. *)
  let source = Flow.Syntax.parse
    "(workspace literals (graph g :context value (let* [x 0.5] x)))" |> Result.get_ok in
  let checked source = match Flow.Workspace.check scope_catalog source with
    | Some w, [] -> w | _ -> fail "literal workspace did not check" in
  let value_field source =
    let view = settled (fst (scope_view (checked source) "g")) in
    let x, y, w, _ = center_of view ["g"; "x"] in
    view, (int_of_float (x +. w -. 40.), int_of_float (y +. 12.)) in
  let edit (view, point) text =
    let view, _ = scope_step view
      (frame ~mouse:point ~keys:[Input.Alt] ~events:(mouse_move point :: click point) ()) in
    let view, changes = type_text view text in
    ignore (scope_step view (frame ~events:[Input.Key_pressed Input.Escape] ()));
    List.filter_map (function Scope.Syntax_edit (E.Set_arg _ as op) -> Some op | _ -> None) changes in
  List.iter (fun text ->
    let ops = edit (value_field source) text in
    check (written ops = [text]) ("value card refused " ^ text);
    let edited, workspace = E.apply_checked scope_catalog source (List.hd ops) |> Result.get_ok in
    let actual = List.assoc "g" (Flow.Eval.run ~time:0. workspace |> Result.get_ok).results in
    check (match actual with Flow.Eval.Float f -> Flow.Lisp.float f = text | _ -> false)
      ("value card did not evaluate " ^ text);
    check (scrub (value_field edited) 10 = []) ("scrubbing " ^ text ^ " changed its value");
    check (written (edit (value_field edited) "7.5") = ["7.5"])
      ("value card could not replace " ^ text)) ["NaN"; "Infinity"; "-Infinity"];
  let rejected = written (edit (field ["g"; "a"] "radius" ~inset:60.) "NaN") in
  check (rejected = [])
    ("a ranged radius field accepted NaN: " ^ String.concat "," rejected);
  (* the selection commands and their requests *)
  let view = settled (Scope.select [ heart ] (fst (scope_view w "flower"))) in
  let only command = snd (Scope.run_command view command) in
  check (only Scope.Fold_into = [ syntax (E.Fold_into { node = heart }) ]) "f: Fold_into";
  check (only Scope.Hoist = [ syntax (E.Hoist { node = heart }) ]) "h: Hoist";
  check (only Scope.Wrap_repeat = [ syntax (E.Wrap { nodes = [ heart ]; loop = E.For }) ]) "r: Wrap For";
  check (only Scope.Wrap_iterate = [ syntax (E.Wrap { nodes = [ heart ]; loop = E.Fold }) ]) "Shift-r: Wrap Fold";
  check (only Scope.Wrap_map = [ syntax (E.Wrap { nodes = [ heart ]; loop = E.Map }) ]) "Shift-m: Wrap Map";
  check (only Scope.Wrap_filter = [ syntax (E.Wrap { nodes = [ heart ]; loop = E.Filter }) ]) "Shift-l: Wrap Filter";
  check (only Scope.Inline_fn = [ syntax (E.Inline_defn { node = heart }) ]) "Alt-Shift-i: Inline_defn";
  check (only Scope.Item_up = [ syntax (E.Move_binding { node = heart; by = -1 }) ]) "Alt-Up off a list: Move_binding";
  check (only Scope.Item_down = [ syntax (E.Move_binding { node = heart; by = 1 }) ]) "Alt-Down off a list: Move_binding";
  check (only Scope.Make_fn = [ syntax (E.Make_local_fn { nodes = [ heart ] }) ]) "l: Make_local_fn";
  check (only Scope.Duplicate = [ syntax (E.Duplicate { nodes = [ heart ] }) ]) "Command-D: Duplicate";
  check (only Scope.Copy = [ Scope.Copy_requested [ heart ] ]) "Command-C: Copy_requested";
  check (only Scope.Cut = [ Scope.Copy_requested [ heart ]; syntax (E.Delete_nodes { nodes = [ heart ] }) ])
    "Command-X: copy, then delete";
  check (only Scope.Paste = [ Scope.Paste_requested ]) "Command-V: Paste_requested";
  (* a screen point as a grid-snapped position in a scope: further right on screen is further
     right in the scope *)
  (let x, y, _, _ = center_of view heart in
   match Scope.scope_point view ~scope:[ "flower" ] (x, y), Scope.scope_point view ~scope:[ "flower" ] (x +. 60., y) with
   | Some (ax, ay), Some (bx, _) ->
       check (Float.rem ax 12. = 0. && Float.rem ay 12. = 0. && bx > ax) "scope_point snaps to 12 and follows the pointer"
   | _ -> fail "scope_point found no scope");
  check (Scope.scope_point view ~scope:[ "nosuch" ] (0., 0.) = None) "an unknown scope has no point";
  (* fold and unfold: the keys, and the right-click menu's rows *)
  let result = [ "flower"; "result" ] in
  (match snd (Scope.run_command (Scope.select [ result ] view) Scope.Unfold) with
   | [ Scope.Syntax_edit (E.Unfold { node; sub = []; _ }) ] when node = result -> ()
   | _ -> failwith "Shift-U on a call with an inline call did not become Unfold");
  let hx, hy, _, _ = center_of view heart in
  let at = int_of_float hx + 20, int_of_float hy + 8 in
  let menu, _ = scope_step view (frame ~mouse:at ~events:[ mouse_move at; mouse_press (Input.RightButton, at);
    mouse_release (Input.RightButton, at) ] ()) in
  let menu, _ = scope_step menu (frame ~mouse:at ()) in
  let item i = fst at + 30, snd at + 3 + (24 * i) + 12 in
  let _, picked = scope_step menu (frame ~mouse:(item 0) ~events:[ mouse_move (item 0); mouse_press (Input.LeftButton, item 0);
    mouse_release (Input.LeftButton, item 0) ] ()) in
  check (List.mem (syntax (E.Fold_into { node = heart })) picked) "the right-click menu's first row did not become Fold_into";
  (* a right-click on empty canvas asks the host for its add menu at that point *)
  let empty = 2900, 1900 in
  let _, requested = scope_step view (frame ~mouse:empty ~events:[ mouse_move empty;
    mouse_press (Input.RightButton, empty); mouse_release (Input.RightButton, empty) ] ()) in
  check (List.mem (Scope.Menu_requested (2900., 1900.)) requested) "a right-click on empty canvas did not ask for the add menu";
  check (only Scope.Display = [ Scope.Display_set heart ]) "v: a geometry node is shown in the viewport";
  (match snd (Scope.run_command (Scope.select [ [ "flower"; "petals" ] ] view) Scope.Display) with
   | [ Scope.Notice _ ] -> () | _ -> failwith "v on a number did not say only geometry can be viewed");
  check (match snd (Scope.run_command (Scope.select [] view) Scope.Duplicate) with [ Scope.Notice _ ] -> true | _ -> false)
    "Command-D with nothing selected did not say so";
  (* the viewed node carries the VIEW mark; the key bindings are the Command table's *)
  let viewed = Scope.with_scope ~key:"flower" ~display:heart (P.of_graph scope_catalog w "flower") view in
  ignore viewed;
  List.iter (fun (id, modifiers) ->
    check (List.exists (fun (c : _ Editor_core.Command.t) -> c.id = id && c.trigger = Some (Editor_core.Keymap.Chord (
      Input.KeyChar (if id = "scope.display" then 'v' else 'd'), modifiers))) Scope.bindings) (id ^ " is not bound"))
    [ "scope.duplicate", [ Input.Meta ]; "scope.duplicate", [ Input.Ctrl ]; "scope.display", [] ];
  (* list rows: the + item row, the up arrow and Alt-Up move an item, a record's + field row *)
  let kw = load_workspace "kit" in
  let kit_scope = P.of_graph scope_catalog kw "kit" in
  let view, _ = scope_view kw "kit" in
  let view = settled view in
  let widths = [ "kit"; "widths" ] in
  let node = Option.get (P.find kit_scope widths) in
  let add = Option.get (List.find_index (fun (r : P.row) -> r.kind = P.Add) node.rows) in
  let ax, ay = Option.get (Scope.Private.row_center view widths add) in
  let _, changes = scope_click view (int_of_float ax, int_of_float ay) in
  check (List.mem (syntax (E.Add_item { node = widths })) changes) "the + item row did not become Add_item";
  let bx, by, bw, _ = center_of view widths in
  let second = Option.get (List.find_index (fun (r : P.row) -> r.key = E.Pos 1) node.rows) in
  let ux = bx +. bw -. 14. *. Scope.zoom view and uy = snd (Option.get (Scope.Private.row_center view widths second)) in
  ignore by;
  let _, changes = scope_click view (int_of_float ux, int_of_float uy) in
  check (List.mem (syntax (E.Move_item { node = widths; pos = 1 })) changes) "the row arrow did not become Move_item";
  let hover = int_of_float (fst (Option.get (Scope.Private.row_center view widths second))), int_of_float uy in
  let hovered, _ = scope_step view (frame ~mouse:hover ~events:[ mouse_move hover ] ()) in
  check (snd (Scope.run_command hovered Scope.Item_up) = [ syntax (E.Move_item { node = widths; pos = 1 }) ]) "Alt-Up: Move_item";
  check (snd (Scope.run_command hovered Scope.Item_down) = [ syntax (E.Move_item { node = widths; pos = 2 }) ]) "Alt-Down: Move_item";
  (* Delete over an item of a variadic input removes the item, whatever it holds *)
  check (snd (Scope.run_command hovered Scope.Delete) = [ syntax (E.Disconnect { node = widths; key = E.Pos 1; fallback = None }) ])
    "Delete on a hovered list item did not remove it";
  check (match snd (Scope.run_command view Scope.Item_up) with [ Scope.Notice _ ] -> true | _ -> false)
    "Alt-Up with no hovered item";
  let rec records (nodes : P.node list) = List.concat_map (fun (n : P.node) ->
    (if n.head = "record" && n.zone = None then [ n ] else [])
    @ (match n.zone with Some z -> records z.scope.nodes | None -> [])) nodes in
  (match records kit_scope.nodes with
   | record :: _ ->
       let add = Option.get (List.find_index (fun (r : P.row) -> r.kind = P.Add) record.rows) in
       (match Scope.Private.row_center view record.path add with
        | Some (rx, ry) ->
            let _, changes = scope_click view (int_of_float rx, int_of_float ry) in
            check (List.exists (function Scope.Syntax_edit (E.Add_field { node; _ }) -> node = record.path | _ -> false) changes)
              "the + field row did not become Add_field"
        | None -> ())
   | [] -> fail "kit has no record node");
  (* the new field takes the first name the record does not have *)
  let rw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace r (graph g :context sop (let* [r {:f2 5} a (sop/uv_sphere :radius r.f2)] a)))" |> Result.get_ok in
  let rview, rscope = scope_view rw.checked "g" in
  let rview, _ = scope_step rview (frame ()) in
  let rx, ry = Option.get (Scope.Private.row_center rview [ "g"; "r" ]
    (Option.get (List.find_index (fun (r : P.row) -> r.kind = P.Add) (Option.get (P.find rscope [ "g"; "r" ])).rows))) in
  let _, changes = scope_click rview (int_of_float rx, int_of_float ry) in
  check (List.exists (function Scope.Syntax_edit (E.Add_field { name = "f1"; _ }) -> true | _ -> false) changes)
    "the + field row did not pick the first unused name";
  (* a wire taken off a variadic input takes the item with it: no nil is left behind *)
  let mw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace m (graph g :context sop (let* [a (sop/box) b (sop/box) m (sop/merge a b)] m)))" |> Result.get_ok in
  let mview = settled (fst (scope_view mw.checked "g")) in
  let wi = Option.get (List.find_index (fun i -> Scope.Private.wire_target mview i = Some ([ "g"; "m" ], E.Pos 1))
    (List.init (Scope.Private.wire_count mview) Fun.id)) in
  let wx, wy = Option.get (Scope.Private.wire_midpoint mview wi) in
  let picked, _ = scope_click mview (int_of_float wx, int_of_float wy) in
  check (snd (Scope.run_command picked Scope.Delete) = [ syntax (E.Disconnect { node = [ "g"; "m" ]; key = E.Pos 1; fallback = None }) ])
    "a wire off a variadic input did not remove the item";
  (* + output names the new output first; a row that only a wire can fill says so *)
  let add_row view (scope : P.scope) path =
    let rec find (nodes : P.node list) = List.find_map (fun (n : P.node) ->
      if n.path = path then Some n else Option.bind n.zone (fun (z : P.zone) -> find z.scope.nodes)) nodes in
    let n = Option.get (find scope.nodes) in
    let x, y = Option.get (Scope.Private.row_center view path
      (Option.get (List.find_index (fun (r : P.row) -> r.kind = P.Add) n.rows))) in
    int_of_float x, int_of_float y in
  let oview, oscope = scope_view kw "def:window" in
  let oview = settled oview in
  let values = (List.find (fun (n : P.node) -> n.head = "values") oscope.nodes).path in
  let naming, changes = scope_click oview (add_row oview oscope values) in
  check (changes = [] && Scope.editing naming) "the + output row did not open a name field";
  let _, changes = scope_step naming (frame ~events:[ Input.Text_input "depth"; Input.Key_pressed Input.Enter ] ()) in
  check (List.mem (syntax (E.Add_field { node = values; name = "depth"; value = S.make (S.Num "0") })) changes)
    "a named output did not become Add_field";
  let cw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace c (graph g :context value (let* [xs (list 1 2) ys (concat xs xs)] (count ys))))" |> Result.get_ok in
  let cview, cscope = scope_view cw.checked "g" in
  let cview = settled cview in
  check (match snd (scope_click cview (add_row cview cscope [ "g"; "ys" ])) with [ Scope.Notice _ ] -> true | _ -> false)
    "the + list row of a concat did not ask for a wire";
  (* a click on the name a row reads selects that binding, so the inspector follows it *)
  let ys = Option.get (P.find cscope [ "g"; "ys" ]) in
  let ri = Option.get (List.find_index (fun (r : P.row) -> r.chip = P.Name "xs") ys.rows) in
  let _, changes = scope_click cview (let x, y = Option.get (Scope.Private.ref_chip cview [ "g"; "ys" ] ri) in
    int_of_float x, int_of_float y) in
  check (List.mem (Scope.Selected [ [ "g"; "xs" ] ]) changes) "a click on a read name did not select its binding";
  (* a click on an expression chip selects its card and the row, [holder @ [":" ^ label]] *)
  let ew = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace e (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a))] (+ a b))))" |> Result.get_ok in
  let eview, escope = scope_view ew.checked "g" in
  let eview = settled eview in
  let eb = Option.get (P.find escope [ "g"; "b" ]) in
  let ei = Option.get (List.find_index (fun (r : P.row) -> match r.chip with P.Inline _ -> true | _ -> false) eb.rows) in
  let label = (List.nth eb.rows ei).label in
  let eview, changes = scope_click eview (let x, y = Option.get (Scope.Private.expr_chip eview [ "g"; "b" ] ei) in
    int_of_float x, int_of_float y) in
  check (List.mem (Scope.Selected [ [ "g"; "b" ]; [ "g"; "b"; ":" ^ label ] ]) changes)
    "a click on an expression chip did not select its row";
  check (Scope.selected eview = [ [ "g"; "b" ]; [ "g"; "b"; ":" ^ label ] ])
    "the pane did not keep the row selected beside its card";
  (* frames: Shift-G makes one around the selection, the corner resizes it, the cross deletes it *)
  let view = settled (Scope.select [ heart; [ "flower"; "bloom" ] ] (fst (scope_view w "flower"))) in
  let view, changes = Scope.run_command view Scope.Make_frame in
  let made = match changes with
    | [ Scope.Frames_set { scope = [ "flower" ]; frames = [ (title, at, size) ] } ] -> title, at, size
    | _ -> fail "Shift-G did not make one frame" in
  let title, at, (fw, fh) = made in
  let framed = Scope.with_scope ~frames:(fun p -> if p = [ "flower" ] then [ made ] else []) ~key:"flower"
      (P.of_graph scope_catalog w "flower") view in
  let framed = settled framed in
  let fx0, fy0 = at in
  let ox, oy = let x, y, _, _ = center_of framed [ "flower"; "heart" ] in ignore (x, y); 0., 0. in
  ignore (ox, oy);
  (* the frame's screen position: its scope's origin is the pane's, so use the pan through a tile *)
  let tile_x, tile_y, _, _ = center_of framed heart in
  let heart_rel = P.layout ~foot:true ~at:(fun _ -> None) (P.of_graph scope_catalog w "flower") in
  let hp = List.find (fun (p : P.placed) -> p.path = heart) heart_rel.placed in
  let z = Scope.zoom framed in
  let sx0 = tile_x -. hp.x *. z and sy0 = tile_y -. hp.y *. z in
  let corner = int_of_float (sx0 +. (fx0 +. fw) *. z -. 6.), int_of_float (sy0 +. (fy0 +. fh) *. z -. 6.) in
  let drag_to = fst corner + 30, snd corner + 20 in
  let sized, _ = scope_step framed (frame ~mouse:corner ~events:[ mouse_move corner; mouse_press (Input.LeftButton, corner) ] ()) in
  let sized, _ = scope_step sized (frame ~mouse:drag_to ~events:[ mouse_move drag_to ] ()) in
  let _, changes = scope_step sized (frame ~mouse:drag_to ~events:[ mouse_release (Input.LeftButton, drag_to) ] ()) in
  (match List.find_map (function Scope.Frames_set { frames = [ (_, _, (w', h')) ]; _ } -> Some (w', h') | _ -> None) changes with
   | Some (w', h') -> check (w' > fw && h' > fh) "dragging the corner did not grow the frame"
   | None -> fail "dragging the corner did not emit Frames_set");
  let cross = int_of_float (sx0 +. (fx0 +. fw) *. z -. 10.), int_of_float (sy0 +. fy0 *. z +. 10.) in
  let _, changes = scope_click framed cross in
  check (List.exists (function Scope.Frames_set { frames = []; _ } -> true | _ -> false) changes) "the cross did not delete the frame";
  let title_at = int_of_float (sx0 +. fx0 *. z +. 20.), int_of_float (sy0 +. fy0 *. z +. 8.) in
  let retitled, _ = double_click framed title_at in
  check (Scope.editing retitled) "a double-click on a frame title did not open its field";
  let retitled = settled retitled in
  let _, changes = type_text retitled "legs" in
  check (List.exists (function Scope.Frames_set { frames = [ (t, _, _) ]; _ } -> t = "legs" && t <> title | _ -> false) changes)
    "typing a frame title did not emit Frames_set";
  (* a drag on the title carries the frame with the nodes inside it, as one Moved and one Frames_set *)
  let towards = fst title_at + 40, snd title_at + 30 in
  let carried, _ = scope_step framed (frame ~mouse:title_at ~events:[ mouse_move title_at; mouse_press (Input.LeftButton, title_at) ] ()) in
  let carried, _ = scope_step carried (frame ~mouse:towards ~events:[ mouse_move towards ] ()) in
  let carried, _ = scope_step carried (frame ~mouse:towards ~events:[] ()) in
  let moved_box = Scope.Private.box_of carried heart and still_box = Scope.Private.box_of framed heart in
  check (match moved_box, still_box with Some (x, _, _, _), Some (x0, _, _, _) -> x > x0 +. 10. | _ -> false)
    "the nodes did not travel with the frame while it was dragged";
  let _, changes = scope_step carried (frame ~mouse:towards ~events:[ mouse_release (Input.LeftButton, towards) ] ()) in
  (match List.filter_map (function Scope.Moved placed -> Some placed | _ -> None) changes,
         List.filter_map (function Scope.Frames_set { frames = [ (_, (x, y), _) ]; _ } -> Some (x, y) | _ -> None) changes with
   | [ placed ], [ (x, y) ] ->
       check (List.exists (fun (p, _, _) -> p = heart) placed && List.exists (fun (p, _, _) -> p = [ "flower"; "bloom" ]) placed)
         "the frame did not move the nodes inside it";
       check (Float.abs (x -. (fx0 +. 40. /. z)) < 2. && Float.abs (y -. (fy0 +. 30. /. z)) < 2.) "the frame did not move with the pointer"
   | _ -> fail "dragging the frame title did not emit one Moved and one Frames_set");
  (* f frames the selection, with nothing selected all of it *)
  let far = Scope.with_bounds ~x:0 ~y:0 ~width:400 ~height:300 framed in
  let zoomed = Scope.select [ heart ] far |> fun v -> fst (Scope.run_command v Scope.Frame_selection) in
  (match Scope.Private.box_of zoomed heart with
   | Some (x, y, w, h) -> check (x >= 0. && y >= 0. && x +. w <= 400. && y +. h <= 300. && w *. h > 0.)
       "framing the selection left the node out of view"
   | None -> fail "the framed node has no box");
  check (Scope.zoom (fst (Scope.run_command (Scope.select [] far) Scope.Frame_selection)) = Scope.zoom (fst (Scope.run_command far Scope.Frame_all)))
    "framing an empty selection is framing all";
  (* marquee: a drag on empty canvas selects the nodes of one scope it covers; Shift adds *)
  let view = settled (fst (scope_view w "flower")) in
  let sc = P.of_graph scope_catalog w "flower" in
  let boxes = List.map (fun (n : P.node) -> Option.get (Scope.Private.box_of view n.path)) sc.nodes in
  let x0 = List.fold_left (fun a (x, _, _, _) -> Float.min a x) infinity boxes -. 8.
  and y0 = List.fold_left (fun a (_, y, _, _) -> Float.min a y) infinity boxes -. 8.
  and x1 = List.fold_left (fun a (x, _, w, _) -> Float.max a (x +. w)) neg_infinity boxes +. 8.
  and y1 = List.fold_left (fun a (_, y, _, h) -> Float.max a (y +. h)) neg_infinity boxes +. 8. in
  let a = int_of_float x0, int_of_float y0 and b = int_of_float x1, int_of_float y1 in
  let drag view ?(shift = false) a b =
    let view, _ = scope_step view (frame ~mouse:a ~events:((if shift then [ Input.Key_pressed Input.Shift ] else [])
      @ [ mouse_move a; mouse_press (Input.LeftButton, a) ]) ()) in
    let view, moved = scope_step view (frame ~mouse:b ~events:[ mouse_move b ] ()) in
    let view, released = scope_step view (frame ~mouse:b ~events:[ mouse_release (Input.LeftButton, b) ] ()) in
    view, moved @ released in
  let view, changes = drag view a b in
  let covered = List.filter (fun p -> List.mem p (Scope.selected view)) (List.map (fun (n : P.node) -> n.path) sc.nodes) in
  check (List.length covered >= 4 && List.mem heart covered) "a marquee over the graph did not select its nodes";
  check (List.exists (function Scope.Selected l -> List.mem heart l | _ -> false) changes) "a marquee did not emit Selected";
  let view = settled (Scope.select [ heart ] (fst (scope_view w "flower"))) in
  let rx, ry, rw, rh = Option.get (Scope.Private.box_of view [ "flower"; "result" ]) in
  let view, _ = drag view ~shift:true (int_of_float (rx -. 4.), int_of_float (ry -. 4.)) (int_of_float (rx +. rw +. 4.), int_of_float (ry +. rh +. 4.)) in
  check (List.mem heart (Scope.selected view) && List.mem [ "flower"; "result" ] (Scope.selected view))
    "Shift marquee did not add to the selection";
  let view, _ = drag (settled (Scope.select [ heart ] (fst (scope_view w "flower")))) (int_of_float (rx -. 4.), int_of_float (ry -. 4.)) (int_of_float (rx +. rw +. 4.), int_of_float (ry +. rh +. 4.)) in
  check (not (List.mem heart (Scope.selected view))) "a plain marquee kept the old selection";
  (* A narrow crossing must select the unnamed result too, without enclosing its card. *)
  let workspace = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace crossing (graph g :context value (let* [a 1.0] (+ a 2.0))))"
    |> Result.get_ok |> fun doc -> doc.checked in
  let scope = P.of_graph scope_catalog workspace "g" in
  let result = ["g"; "@result"] and upstream = ["g"; "a"] in
  check ((Option.get (P.find scope result)).synthetic) "crossing fixture has no unnamed result";
  List.iter (fun level ->
    let view = Scope.create ~width:3000 ~height:2000 ()
      |> Scope.with_scope ~key:"g" ~level:(fun _ -> Some (level, true)) scope |> settled in
    let x, y, w, h = Option.get (Scope.Private.box_of view result) in
    let a = int_of_float (x +. w /. 2. -. 6.), int_of_float (y -. 12.)
    and b = int_of_float (x +. w /. 2. +. 6.), int_of_float (y +. h +. 12.) in
    List.iter (fun (a, b) ->
      let selected, changes = drag view a b in
      check (Scope.selected selected = [result]) "a crossing marquee skipped the unnamed result";
      check (List.exists (function Scope.Selected [path] -> path = result | _ -> false) changes)
        "a crossing marquee did not report the result selection") [a, b; b, a];
    let selected, _ = drag (Scope.select [upstream] view) ~shift:true a b in
    check (List.mem upstream (Scope.selected selected) && List.mem result (Scope.selected selected))
      "Shift crossing did not keep the old selection and add the unnamed result") [P.Card; Chip; Point]

let run_scope () =
  let module E = Flow_graph.Flow_edit in
  let conditional = Editor_document.Workspace_doc.of_text scope_catalog
      "(workspace branches (graph g :context value (let* [x (cond false (+ 1 2) true (* 3 4) :else 5)] x)))"
      |> Result.get_ok |> fun doc -> doc.checked in
  (* the arm's formula opened, so it is a card of its own *)
  let view, _ = scope_view ~folded:(fun _ -> false) conditional "g" in
  let view, _ = scope_step view (frame ()) in
  let arm = ["g"; "x#then~2"] in
  check (Scope.Private.selector view arm = None) "conditional arm incorrectly has an iteration selector";
  let _, changes = Scope.run_command (Scope.select [arm] view) Scope.Add_arm in
  check (List.mem (Scope.Syntax_edit (E.Add_arm {node = ["g"; "x"]; after = 1})) changes) "arm Add uses wrong holder/index";
  let _, changes = Scope.run_command (Scope.select [arm] view) Scope.Delete_arm in
  check (List.mem (Scope.Syntax_edit (E.Delete_arm {node = ["g"; "x"]; index = 1})) changes) "arm Delete uses wrong index";
  let _, changes = Scope.run_command (Scope.select [arm] view) Scope.Unfold in
  check (List.mem (Scope.Syntax_edit (E.Unfold {node = ["g"; "x"]; key = Arm 1; sub = []})) changes) "arm unfold did not address conditional body";
  let view, _ = scope_step (fst (scope_view conditional "g")) (frame ()) in
  let path = ["g"; "x"] in
  let x, y, _, _ = Option.get (Scope.Private.box_of view path) in
  let at = int_of_float x + 20, int_of_float y + 8 in
  let menu, _ = scope_step view (frame ~mouse:at ~events:[mouse_move at; mouse_press (Input.RightButton, at); mouse_release (Input.RightButton, at)] ()) in
  let menu, _ = scope_step menu (frame ~mouse:at ()) in
  let add_arm = Scope.Syntax_edit (E.Add_arm {node = path; after = -1}) in
  check (List.exists (fun row ->
      let point = fst at + 30, snd at + 3 + 24 * row + 12 in
      List.mem add_arm (snd (scope_step menu (frame ~mouse:point ~events:(click point) ())))) (List.init 20 Fun.id))
    "conditional right-click Add arm did not emit edit";
  let imported = Scope.with_scope ~imported:(fun _ -> Some "lib.rays") ~key:"import" (P.of_graph scope_catalog conditional "g") view in
  let imported, _ = scope_step imported (frame ()) in
  let menu, _ = scope_step imported (frame ~mouse:at ~events:[mouse_move at; mouse_press (Input.RightButton, at); mouse_release (Input.RightButton, at)] ()) in
  let menu, _ = scope_step menu (frame ~mouse:at ()) in
  let point = fst at + 30, snd at + 15 in
  let _, changes = scope_step menu (frame ~mouse:point ~events:(click point) ()) in
  check (List.mem (Scope.Open_import "lib.rays") changes) "import context menu did not emit Open file";
  (* every fixture's graphs draw: the pane's counts are the projection's *)
  List.iter (fun (name, graph, nodes, zones, rows) ->
    let w = load_workspace name in
    let view, _ = scope_view w graph in
    let view, _ = scope_step view (frame ()) in
    let s = Scope.stats view in
    check (s.nodes = nodes && s.zones = zones && s.rows = rows)
      (Printf.sprintf "%s/%s: pane counts %d/%d/%d" name graph s.nodes s.zones s.rows);
    check (s.drawn_items > 0 && s.drawn_zones <= s.zones) (name ^ ": nothing drawn"))
    [ "bloom", "flower", 12, 1, 84; "sunflower", "sunflower", 9, 1, 36; "orrery", "orrery", 20, 1, 117;
      "facade", "facade", 14, 2, 89; "kit", "kit", 16, 2, 112; "tree", "tree", 9, 1, 184;
      "garland", "garland", 19, 5, 90; "wave", "wave", 7, 2, 46; "tiles", "tiles", 10, 1, 70 ];
  (* the iteration selector: buttons and track are hit-tested boxes *)
  let w = load_workspace "sunflower" in
  let zone = [ "sunflower"; "seeds_each" ] in
  let view, _ = scope_view ~probe:(fun _ -> 5) w "sunflower" in
  let view, _ = scope_step view (frame ()) in
  let prev, track, next = Option.get (Scope.Private.selector view zone) in
  let _, changes = scope_click view (rect_center next) in
  check (List.mem (Scope.Probe_set { zone; index = 6 }) changes) "the next button did not step the probe";
  let _, changes = scope_click view (rect_center prev) in
  check (List.mem (Scope.Probe_set { zone; index = 4 }) changes) "the previous button did not step the probe";
  (* one stepper pins the iteration of every zone when nothing is selected *)
  let _, pinned = Scope.run_command view (Scope.Probe_step 1) in
  check (List.mem (Scope.Probe_set { zone; index = 6 }) pinned) "[ ] with nothing selected did not step every zone";
  let inline = (Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace w (graph g :context value (let* [ys (map (fn [(x : float)] (+ x 2.0)) (array/float 10003 1.0))] (array/count ys))))"
    |> Result.get_ok).checked in
  let inline_view, _ = scope_view ~probe:(fun _ -> 9999) inline "g" in
  let inline_view, _ = scope_step inline_view (frame ()) in
  let inline_zone = ["g"; "ys#0"] in
  let _, inline_keys = Scope.run_command (Scope.select [inline_zone] inline_view) (Scope.Probe_step 1) in
  check (List.mem (Scope.Probe_set {zone = inline_zone; index = 10000}) inline_keys)
    "inline function keyboard selector did not step beyond the recording cap";
  let _, _, inline_next = Option.get (Scope.Private.selector inline_view inline_zone) in
  let ix, iy, iw, ih = inline_next in
  check (ix >= 0. && iy >= 0. && ix +. iw <= 1000. && iy +. ih <= 700.)
    (Printf.sprintf "inline selector outside viewport: %.1f %.1f %.1f %.1f" ix iy iw ih);
  let _, changes = scope_click inline_view (rect_center inline_next) in
  check (List.mem (Scope.Probe_set {zone = inline_zone; index = 10000}) changes)
    ("an inline function's selector did not step beyond the recording cap: " ^
      Printf.sprintf "%.1f %.1f %.1f %.1f " ix iy iw ih ^
      String.concat ", " (List.filter_map (function Scope.Probe_set {zone; index} ->
        Some (String.concat "/" zone ^ "=" ^ string_of_int index)
        | Scope.Selected paths -> Some ("selected " ^ String.concat "," (List.map (String.concat "/") paths))
        | Scope.Zone_collapsed {zone; _} -> Some ("collapsed " ^ String.concat "/" zone)
        | _ -> None) changes));
  let x, y, tw, th = track in
  let _, changes = scope_click view (int_of_float (x +. tw *. 0.75), int_of_float (y +. th /. 2.)) in
  (* a click at 75 % of the 57.5-point track reads 180 of 240; the one slack is the test's whole-point
     click (up to 1 point is 4.2 iterations): 175 to 181 *)
  check (List.exists (function Scope.Probe_set { zone = z; index } -> z = zone && index >= 175 && index <= 181
                              | _ -> false) changes) "the track did not map the pointer to an iteration";
  let _, changes = scope_click view (10, 690) in
  check (changes = [] || List.for_all (function Scope.Selected _ -> true | _ -> false) changes)
    "a blank click made an edit";
  (* the toggle collapses the zone; a collapsed zone has no selector *)
  let tx, ty, _, _ = Option.get (Scope.Private.box_of view zone) in
  let _, changes = scope_click view (int_of_float tx + 10, int_of_float ty + 10) in
  check (List.mem (Scope.Zone_collapsed { zone; collapsed = true }) changes) "the toggle did not collapse the zone";
  (* only visible zones draw: 1,000 iterations draw what 3 do *)
  let draw_with n =
    let view, _ = scope_view ~inputs:[ "sunflower", [ "seeds", Flow.Eval.Int n ] ] w "sunflower" in
    let view, _ = scope_step view (frame ()) in
    (Scope.stats view).drawn_items, scope_paint view in
  let items3, paint3 = draw_with 3 and items1000, paint1000 = draw_with 1000 in
  check (items3 = items1000) "iterations changed how many tiles are built";
  (* what grows is the sparklines: at most 16 segments each, whatever the count *)
  check (paint1000 < paint3 + 30 * items3) "a 1,000-iteration zone painted much more than a 3-iteration one";
  let base, scope = scope_view ~inputs:["sunflower", ["seeds", Flow.Eval.Int 3]] w "sunflower" in
  let evaluated = Result.get_ok (Flow.Eval.static ~record:true
    ~inputs:["sunflower", ["seeds", Flow.Eval.Int 3]] w) in
  let execution path ~probes:_ = Some Flow_graph.Probe.{tier = "CPU"; group = zone;
    seconds = if path = zone then Some 0.002 else None} in
  let reports = Flow_graph.Probe.make ~execution evaluated in
  let zone_node = Option.get (P.find scope zone) in
  let footer = Flow_graph.Probe.footer reports zone_node ~probes:[] in
  check (footer.execution = execution zone ~probes:[]) "the footer lost its execution report";
  check (List.mem ("tier", "CPU") (Flow_graph.Probe.readouts reports zone_node ~probes:[]))
    "the inspector lost its execution tier";
  let paint_hash view =
    ignore (scope_step view (frame ()));
    match Scene.Private.stage_native ~width:1000 ~height:700 (Pxui.Ui.scene scope_ui) with
    | Error message -> fail message
    | Ok staged -> staged.layers |> List.filter_map (function
        | Scene.Private.Ui_layer (batch, _) -> Some (Scene_command.Ui_batch.instances batch) | _ -> None)
      |> fun batches -> Digest.string (Marshal.to_string batches [Marshal.No_sharing]) in
  check (paint_hash base <> paint_hash (Scope.with_records reports base))
    "execution badges did not change the graph's paint";
  let collapsed_view, _ = scope_view ~collapsed:(fun p -> p = zone) w "sunflower" in
  let collapsed_view, _ = scope_step collapsed_view (frame ()) in
  check ((Scope.stats collapsed_view).drawn_zones = 0) "a collapsed zone drew its body";
  (* gestures become requests: scrub, wire, delete *)
  let ws = load_workspace "bloom" in
  let view, scope = scope_view ws "flower" in
  let view, _ = scope_step view (frame ()) in
  let heart = [ "flower"; "heart" ] in
  let node = Option.get (P.find scope heart) in
  let i = Option.get (List.find_index (fun (r : P.row) -> r.label = "radius") node.rows) in
  let fx, fy = Option.get (Scope.Private.row_center view heart i) in
  ignore (fx, fy);
  (* wire: a node's output socket dropped on another node's geometry row *)
  let source = [ "flower"; "bloom" ] and target = [ "flower"; "result" ] in
  let sx, sy = Option.get (Scope.Private.output_socket view source) in
  let start = int_of_float sx, int_of_float sy in
  let tnode = Option.get (P.find scope target) in
  let row = Option.get (List.find_index (fun (r : P.row) -> r.socket && r.kind = P.Rest) tnode.rows) in
  let rx, ry = Option.get (Scope.Private.row_center view target row) in
  let stop = int_of_float rx, int_of_float ry in
  let view, _ = scope_step view (frame ~mouse:start ~events:[mouse_move start; mouse_press (Input.LeftButton, start)] ()) in
  let view, _ = scope_step view (frame ~mouse:stop ~events:[mouse_move stop] ()) in
  let _, changes = scope_step view (frame ~mouse:stop ~events:[mouse_release (Input.LeftButton, stop)] ()) in
  check (List.exists (function
      | Scope.Syntax_edit (Flow_graph.Flow_edit.Connect { node; src = "bloom"; iter = false; _ }) -> node = target
      | _ -> false) changes) "a wire dropped on a row did not become Connect";
  (* delete: the selected node *)
  let view, _ = scope_click view start in
  let view = Scope.select [ heart ] view in
  let _, changes = Scope.run_command view Scope.Delete in
  check (changes = [ Scope.Syntax_edit (Flow_graph.Flow_edit.Delete_nodes { nodes = [ heart ] }) ])
    "Delete did not become Delete_nodes";
  (* wire selection and deletion *)
  check (Scope.Private.wire_count view > 0) "there are wires in the graph";
  let wire_idx = Option.get (List.find_index (fun i -> Scope.Private.wire_target view i <> None)
    (List.init (Scope.Private.wire_count view) (fun i -> i))) in
  ignore wire_idx;
  let view0 = Scope.select [ heart ] view in
  check (Scope.selected view0 = [ heart ]) "heart is selected";
  (* some wire of the graph (not under a card) is hit at its longest segment's middle *)
  let view = List.fold_left (fun found i ->
    if Scope.selected_wire found <> None || Scope.Private.wire_target view0 i = None then found else
    let wpt = Option.get (Scope.Private.wire_midpoint view0 i) in
    fst (scope_click view0 (int_of_float (fst wpt), int_of_float (snd wpt))))
    view0 (List.init (Scope.Private.wire_count view0) Fun.id) in
  check (Scope.selected_wire view <> None) "clicking wire selected it";
  check (Scope.selected view = []) "clicking wire cleared node selection";
  let _, changes = Scope.run_command view Scope.Delete in
  check (match changes with [ Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect _) ] -> true | _ -> false)
    "Delete on selected wire emitted Disconnect";
  (* a right-click on a node makes the nodes the selection again: Delete then removes the node *)
  let hx, hy, _, _ = Option.get (Scope.Private.box_of view heart) in
  let at = int_of_float hx + 60, int_of_float hy + 8 in
  let menu, changes = scope_step view (frame ~mouse:at ~events:[ mouse_move at; mouse_press (Input.RightButton, at);
    mouse_release (Input.RightButton, at) ] ()) in
  check (List.mem (Scope.Selected [ heart ]) changes) "a right-click on a node did not emit Selected";
  check (List.mem (Scope.Syntax_edit (Flow_graph.Flow_edit.Delete_nodes { nodes = [ heart ] })) (snd (Scope.run_command menu Scope.Delete)))
    "Delete after a right-click on a node removed the wire selected before";
  (* a wire inside a loop's body is hovered and selected like any other: its boxes lie over the zone's tile *)
  let lw = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace l (graph g :context sop (let* [ring (for [i (range 3)] (let* [u (sop/uv_sphere) v (sop/transform u)] v)) m (sop/merge ring)] m)))"
    |> Result.get_ok in
  let lview, _ = scope_view ~at:(function
    | [ "g"; "ring"; "u" ] -> Some (0., 0.) | [ "g"; "ring"; "v" ] -> Some (480., 240.) | _ -> None) lw.checked "g" in
  let lview, _ = scope_step lview (frame ()) in
  let inner = [ "g"; "ring"; "v" ] in
  let wi = Option.get (List.find_index (fun i -> match Scope.Private.wire_target lview i with
    | Some (path, _) -> path = inner | None -> false) (List.init (Scope.Private.wire_count lview) Fun.id)) in
  let mx, my = Option.get (Scope.Private.wire_midpoint lview wi) in
  let mid = int_of_float mx, int_of_float my in
  let hovered, _ = scope_step lview (frame ~mouse:mid ~events:[ mouse_move mid ] ()) in
  check (List.length (Scope.Private.highlighted_connections hovered) = 1) "a wire inside a loop is not hovered";
  let picked, _ = scope_click lview mid in
  check (match snd (Scope.run_command picked Scope.Delete) with
    | Scope.Syntax_edit (Flow_graph.Flow_edit.Disconnect { node; _ }) :: _ -> node = inner | _ -> false)
    "a wire inside a loop is not selected by a click";
  (* keys and menu: every key command maps to a request *)
  let some name = check (List.exists (fun (c : (_, Scope.command) Editor_core.Command.t) -> c.id = "scope." ^ name)
    Scope.bindings) ("no key for " ^ name) in
  List.iter some [ "delete"; "fold"; "unfold"; "hoist"; "bypass"; "repeat"; "iterate"; "function"; "collapse";
                   "probe-prev"; "probe-next"; "frame-all"; "walk.left" ];
  let _, changes = Scope.run_command (Scope.select [ heart ] view) Scope.Bypass in
  check (changes = [ Scope.Syntax_edit (Flow_graph.Flow_edit.Toggle_bypass { node = heart }) ]) "Bypass";
  (* W9: the macro lens, the bypass flag, the make-macro key *)
  let w = load_workspace "rosette" in
  let view, scope = scope_view w "rosette" in
  let view, _ = scope_step view (frame ()) in
  let outer = [ "rosette"; "outer" ] and soft = [ "rosette"; "soft" ] in
  let node = Option.get (P.find scope outer) in
  let lens = Option.get node.lens in
  check (Array.length lens.steps >= 2 && lens.error = None
         && String.starts_with ~prefix:"(radial" lens.steps.(0)
         && String.starts_with ~prefix:"(sop/merge" lens.steps.(1)) "the lens steps are the call, then its expansions";
  check (Scope.Private.macro_step view outer = None) "the lens starts closed";
  let toggle = Scope.Private.lens_toggle view outer |> Option.get in
  let view, _ = scope_click view (int_of_float (fst toggle), int_of_float (snd toggle)) in
  check (Scope.Private.macro_step view outer = Some (Array.length lens.steps - 1)) "the toggle opens the last step";
  let view, _ = scope_step view (frame ()) in
  let button i = let x, y = Option.get (Scope.Private.lens_step_button view outer i) in int_of_float x, int_of_float y in
  let view, changes = scope_click view (button 0) in
  check (Scope.Private.macro_step view outer = Some 0 && changes = []) "the call button shows the call, no edit";
  let view, _ = scope_step view (frame ()) in
  let view, _ = scope_click view (button 1) in
  check (Scope.Private.macro_step view outer = Some 1) "a step button chooses the step";
  let view, _ = scope_step view (frame ()) in
  (* the Template button shows the macro's definition, one step past the expansions *)
  check (List.exists (String.starts_with ~prefix:"(defmacro radial") (String.split_on_char '\n' lens.template))
    "the lens carries the macro's template";
  let view, changes = scope_click view (button (Array.length lens.steps)) in
  check (Scope.Private.macro_step view outer = Some (Array.length lens.steps) && changes = []) "the Template button shows the definition, no edit";
  let view, _ = scope_step view (frame ()) in
  let view, _ = scope_click view (button 1) in
  let view, _ = scope_step view (frame ()) in
  let bx, by, bw, bh = Option.get (Scope.Private.box_of view outer) in
  check (bh > (let _, _, _, h0 = Option.get (Scope.Private.box_of (fst (scope_view w "rosette")) outer) in h0)
         && bw >= Flow_graph.Projection.lens_width *. Scope.zoom view -. 1.) "an open panel grows its card";
  ignore (bx, by);
  let rx, ry = Option.get (Scope.Private.lens_replace view outer) in
  let _, changes = scope_click view (int_of_float rx, int_of_float ry) in
  check (List.mem (Scope.Syntax_edit (Flow_graph.Flow_edit.Inline_macro { node = outer })) changes)
    "the replace button did not become Inline_macro";
  let tx, ty = Option.get (Scope.Private.lens_toggle view outer) in
  let view, _ = scope_click view (int_of_float tx, int_of_float ty) in
  check (Scope.Private.macro_step view outer = None) "the toggle closes the panel";
  (* bypass is the b key (and the context menu): the same request, and a card with nothing to pass
     through has none *)
  let _, changes = Scope.run_command (Scope.select [ soft ] view) Scope.Bypass in
  check (List.mem (Scope.Syntax_edit (Flow_graph.Flow_edit.Toggle_bypass { node = soft })) changes)
    "the b key did not become Toggle_bypass";
  check ((Option.get (P.find scope soft)).bypass) "soft is authored bypassed";
  (* m asks the host for the make-macro dialog over the selection *)
  let _, changes = Scope.run_command (Scope.select [ soft ] view) Scope.Make_macro in
  check (changes = [ Scope.Macro_requested [ soft ] ]) "m did not request the dialog";
  let _, changes = Scope.run_command (Scope.select [] view) Scope.Make_macro in
  check (match changes with [ Scope.Notice _ ] -> true | _ -> false) "m with nothing selected";
  scope_gestures ();
  scope_ghost_values ();
  scope_curve ();
  scope_field_formula ();
  scope_spine ();
  scope_carry ();
  scope_connection_hover ();
  scope_levels ();
  scope_zoom_geometry ();
  scope_pinch ();
  scope_suspend ();
  scope_idle_frame ();
  print_endline "pxui graph scope pane tests passed"

(* the node menu and the pane (its gestures are [scope_gestures]), with the number of checks made *)
(* a node an edit adds goes beside its consumer; every other card stays exactly where it was *)
let run_sticky () =
  let scope_of text =
    let ws = Editor_document.Workspace_doc.of_text scope_catalog text |> Result.get_ok in
    P.of_graph scope_catalog ws.checked "g" in
  let before = scope_of "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [1 0 0]) c (sop/transform b :translate [2 0 0]) d (sop/transform c :translate [3 0 0])] d)))" in
  let after = scope_of "(workspace w (graph g :context sop (let* [z (sop/box) a (sop/transform z :translate [0 0 0]) b (sop/transform a :translate [1 0 0]) c (sop/transform b :translate [2 0 0]) d (sop/transform c :translate [3 0 0])] d)))" in
  let first = P.layout before in
  let table (l : P.layout) = List.map (fun (b : P.placed) -> b.path, b) l.placed in
  let prev path = Option.map (fun (b : P.placed) -> b.x, b.y, b.w, b.h) (List.assoc_opt path (table first)) in
  let sticky = table (P.layout ~prev after) and relaid = table (P.layout after) in
  List.iter (fun (path, (b : P.placed)) ->
    let s = List.assoc path sticky in
    check (s.x = b.x && s.y = b.y) "an edit that added a node moved a card that was there") (table first);
  let pos path l = List.assoc [ "g"; path ] l in
  let z = pos "z" sticky and a = pos "a" sticky in
  check (z.x < a.x && z.y = a.y) "the new node is not on the row of its consumer, one column left";
  check ((pos "a" relaid).x <> a.x) "the fixture does not shift the columns when laid out afresh";
  List.iter (fun (p, (b : P.placed)) -> List.iter (fun (q, (c : P.placed)) ->
    check (p = q || b.x >= c.x +. c.w || c.x >= b.x +. b.w || b.y >= c.y +. c.h || c.y >= b.y +. b.h)
      "two cards overlap") sticky) sticky;
  (* moving a binding in the text moves no card *)
  let text = "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box) c (sop/merge a b)] c)))" in
  let ws = Editor_document.Workspace_doc.of_text scope_catalog text |> Result.get_ok in
  let source, ws' = Flow_graph.Flow_edit.apply_checked scope_catalog (Flow.Syntax.parse text |> Result.get_ok)
    (Flow_graph.Flow_edit.Move_binding { node = [ "g"; "b" ]; by = -1 }) |> Result.get_ok in
  check (fst (Flow.Lisp.print source) <> text ) "Move_binding changed no text";
  let first = P.layout (P.of_graph scope_catalog ws.checked "g") in
  let table (l : P.layout) = List.map (fun (b : P.placed) -> b.path, (b.x, b.y)) l.placed in
  let prev path = Option.map (fun (b : P.placed) -> b.x, b.y, b.w, b.h) (List.find_opt (fun (b : P.placed) -> b.path = path) first.placed) in
  let moved = P.layout ~prev (P.of_graph scope_catalog ws' "g") in
  check (List.sort compare (table first) = List.sort compare (table moved)) "reordering bindings moved a card"

(* Selecting a node lights its chain; a long wire is a tag until its chain is lit. *)
let run_readable () =
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace r (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform b) d (sop/transform a) e (sop/merge c d a)] e)))"
    |> Result.get_ok in
  let view, _ = scope_view ~at:(function
    | ["g"; "a"] -> Some (0., 0.) | ["g"; "b"] -> Some (288., 0.) | ["g"; "c"] -> Some (576., 0.)
    | ["g"; "d"] -> Some (288., 240.) | ["g"; "@result"] -> Some (4000., 0.) | ["g"; "e"] -> Some (864., 0.)
    | _ -> None) ws.checked "g" in
  let view, _ = scope_step view (frame ()) in
  check (Scope.Private.chain view = None) "a chain was lit with nothing selected";
  check (Scope.Private.tagged_wires view = 0) "a short graph drew a tag";
  let view = Scope.select [ [ "g"; "b" ] ] view in
  (match Scope.Private.chain view with
   | Some (up, down) ->
       let has l p = List.mem [ "g"; p ] l in
       check (has up "a" && has up "b" && not (has up "c") && not (has up "d")) "upstream of b is not a and b";
       check (has down "b" && has down "c" && has down "e" && not (has down "a") && not (has down "d"))
         "downstream of b is not b, c and e"
   | None -> fail "selecting a node lit no chain");
  (* a node 4,000 points to the right reads [a] through a tag, until a or e is selected *)
  let far, _ = scope_view ~at:(function
    | ["g"; "e"] -> Some (4000., 0.) | ["g"; "a"] -> Some (0., 0.) | ["g"; "b"] -> Some (288., 0.)
    | ["g"; "c"] -> Some (576., 0.) | ["g"; "d"] -> Some (288., 240.) | _ -> None) ws.checked "g" in
  let far, _ = scope_step far (frame ()) in
  check (Scope.Private.tagged_wires far >= 1) "a wire four thousand points long was not a tag";
  let lit = Scope.select [ [ "g"; "a" ] ] far in
  check (Scope.Private.tagged_wires lit = 0) "selecting the source did not show the real wire";
  let off, _ = Scope.run_command far Scope.Toggle_tags in
  check (Scope.Private.tagged_wires off = 0) "the tags toggle left a tag";
  (* Unfold all: one Unfold per call written in a row of the card *)
  let ws = Editor_document.Workspace_doc.of_text scope_catalog
    "(workspace u (graph g :context sop (let* [a (sop/merge (sop/box) (sop/box))] a)))" |> Result.get_ok in
  let view, _ = scope_view ws.checked "g" in
  let view, _ = scope_step view (frame ()) in
  let _, changes = Scope.run_command (Scope.select [ [ "g"; "a" ] ] view) Scope.Unfold_all in
  let unfolds = List.filter (function Scope.Syntax_edit (Flow_graph.Flow_edit.Unfold _) -> true | _ -> false) changes in
  check (List.length unfolds >= 2) "Unfold all did not unfold every call of the card";
  print_endline "pxui graph readable tests passed"

let run () =
  run_menu ();
  run_readable ();
  run_scope ();
  run_sticky ();
  Printf.printf "test_pxui_graph: %d checks\n" !checks

(* 2,001 nodes in one scope: laying it out and painting a frame.  Command:
   dune exec test/test_main.exe -- bench_scope_big *)
let bench_scope_big () =
  let scope = big_scope 2001 in
  let ui = Pxui.Ui.create ~font_size:11 () in
  let time label runs f =
    let started = Unix.gettimeofday () in
    for _ = 1 to runs do ignore (Sys.opaque_identity (f ())) done;
    Printf.printf "%-28s %.3f ms\n%!" label ((Unix.gettimeofday () -. started) *. 1000. /. float runs) in
  time "2001 nodes: with_scope" 5 (fun () -> Scope.create ~width:1000 ~height:700 () |> Scope.with_scope ~key:"g" scope);
  let view = ref (Scope.create ~width:1000 ~height:700 () |> Scope.with_scope ~key:"g" scope) in
  for _ = 1 to 5 do view := fst (Pxui.Ui.frame ui (frame ()) (fun ui -> Scope.update !view ui (frame ()))) done;
  time "2001 nodes: with_scope again" 20 (fun () -> Scope.with_scope ~key:"g" scope !view);
  (* an idle frame: time and allocation, with cards in view and with none (a 16 x 16 pane) *)
  let idle label view =
    for _ = 1 to 5 do view := fst (Pxui.Ui.frame ui (frame ()) (fun ui -> Scope.update !view ui (frame ()))) done;
    let runs = 100 in
    let started = Unix.gettimeofday () and allocated = Gc.allocated_bytes () in
    for _ = 1 to runs do view := fst (Pxui.Ui.frame ui (frame ()) (fun ui -> Scope.update !view ui (frame ()))) done;
    Printf.printf "%-28s %.3f ms, %.0f bytes allocated\n%!" label
      ((Unix.gettimeofday () -. started) *. 1000. /. float runs)
      ((Gc.allocated_bytes () -. allocated) /. float runs) in
  idle "2001 nodes: frame" view;
  idle "2001 nodes: frame, 16 x 16" (ref (Scope.create ~width:16 ~height:16 () |> Scope.with_scope ~key:"g" scope));
  time "2001 nodes: Point_all" 5 (fun () -> Scope.run_command !view Scope.Point_all);
  (* one tick of a scrub: the rewrite, the print, the parse and the check *)
  let forms = Flow.Syntax.parse (big_text 2001) |> Result.get_ok in
  time "2001 nodes: Set_arg edit" 5 (fun () ->
    Flow_graph.Flow_edit.apply_checked scope_catalog forms (Flow_graph.Flow_edit.Set_arg { node = [ "g"; "n5" ];
      key = Kw "translate"; sub = [ 0 ]; value = Flow.Syntax.make (Flow.Syntax.Num "3") }))
