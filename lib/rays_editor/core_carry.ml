open Core_model
open Rays
open Editor_document
open Core_panes
open Core_inspect
open Core_shell
open Core_text
open Core_menu

(* ---- carry ---- *)

let carry_letters = "asdfghjkl"
let carry_rest = 0.6  (* seconds the pointer rests on a node before the carry enters it *)

(* While a payload is carried the document only navigates: the keys that walk, follow, go back and
   jump work; every edit waits for the put. *)
let carry_allowed : Leader.action -> bool = function
  | Enter | Up | Back _ | Jump | Guide_toggle | Guide_keys | Frame_tile | Pick_up
  | Scope_command (Pxui_graph.Scope.Walk _ | Frame_all | Frame_selection) -> true
  | _ -> false

let carry_place = function
  | Showing { place; _ } | Held_back { place; _ } | Refused { place; _ } -> place

(* the document the panels read: the scratch copy while a target is hot and its picture is on
   screen, else the one that was there *)
let carry_shown (c : _ carry) = match c.preview with
  | Some (Showing { doc; _ }) -> doc
  | Some (Held_back _ | Refused _) | None -> c.original

(* The put at [place], run on the original: the checker's answer is the preview, the refusal's
   reason and the write.  A put that takes longer than the budget (or onto a cook that does) is
   said and not shown. *)
let carry_try value (c : _ carry) place =
  match Lazy.force value.flow_catalog with
  | None -> Refused { place; reason = "The catalog is unavailable" }
  | Some catalog ->
      let start = value.clock () in
      let result = Carry.put ~factories:value.factories ~catalog c.original c.payload place in
      let spent = value.clock () -. start in
      (match result with
       | Error reason -> Refused { place; reason }
       | Ok (doc, what) ->
           (match value.cook.Cook.seconds with
            | _ when spent >= value.carry_budget ->
                Held_back { place; doc; what;
                            reason = Printf.sprintf "applying takes %d ms" (int_of_float (spent *. 1000.)) }
            | Some cook when cook >= value.carry_budget ->
                Held_back { place; doc; what;
                            reason = Printf.sprintf "a cook takes %d ms" (int_of_float (cook *. 1000.)) }
            | _ -> Showing { place; doc; what }))

(* The key route's letters for the graph the pane shows now (they follow navigation). *)
let carry_targets value (c : _ carry) =
  let graph = graph_name value in
  match c.targets with
  | Some (for_graph, _) when for_graph = graph -> c
  | _ ->
      let found = match Lazy.force value.flow_catalog with
        | None -> []
        | Some catalog -> Carry.targets ~factories:value.factories ~catalog c.original c.payload ~graph in
      { c with targets = Some (graph, List.mapi (fun i (place, label) ->
          String.make 1 carry_letters.[i], place, label) found) }

(* What [y] picks up: the open material or SOP graph; else the graph of the selected geometry
   object; else the selected camera object (by its binding name); else the graph the pane's
   selected node references; else the open scene graph (a viewport shows it). *)
let carry_payload value =
  let ws = value.doc.Document.workspace.ws in
  let context name = Option.map (fun (g : Flow.Workspace.graph) -> g.context)
    (List.find_opt (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs) in
  let payload kind name = Ok { Carry.kind; value = "(ref " ^ name ^ ")" } in
  let referenced name = match context name with
    | Some context when context = Flow.Context.material -> Some (payload "material" name)
    | Some context when context = Flow.Context.sop -> Some (payload "sop" name)
    | _ -> None in
  let opened = Option.bind (graph_name value) (fun name -> referenced name) in
  match opened with
  | Some result -> result
  | None ->
      let object_graph = Option.bind (Selection.selected (pane_selection value)) (fun id ->
        if kind value id = Some "geometry" then graph_of_object value id else None) in
      (match Option.bind object_graph referenced with
       | Some result -> result
       | None ->
           (* a camera object: the one selected in the list, else the scene node selected in the pane *)
           let camera_named id =
             if kind value id <> Some "camera" then None else
             match List.assoc_opt id value.doc.Document.homes.objects with
             | Some (Document.Bound_at path) when path <> [] ->
                 Some { Carry.kind = "camera"; value = List.nth path (List.length path - 1) }
             | _ -> None in
           let camera = match Option.bind (Selection.selected (pane_selection value)) camera_named with
             | Some _ as found -> found
             | None ->
                 (match Pxui_graph.Scope.selected (pane_view value) with
                  | [ path ] ->
                      Option.bind (List.find_map (fun (id, home) ->
                        if home = Document.Bound_at path then Some id else None)
                        value.doc.Document.homes.objects) camera_named
                  | _ -> None) in
           (match camera with
            | Some payload -> Ok payload
            | None ->
                (match Option.bind (follow_target value) referenced with
                 | Some result -> result
                 | None ->
                     (match Option.bind (graph_name value) (fun name ->
                        if context name = Some Flow.Context.scene then Some (payload "scene" name) else None) with
                      | Some result -> result
                      | None -> Error "Nothing to pick up · open a material, SOP or scene graph, or select a geometry or camera object"))))

let carry_cancel value (c : _ carry) ~notice =
  Pxui.Ui.cancel_carry value.ui;
  let doc = c.original in
  let level = Document.resolve_level doc (pane_level value) in
  if doc != value.doc then Cook.set_volatile value.cook (Flow_sop.Lower.is_volatile (doc.workspace.lowered));
  with_pane { value with carry = None; doc; notice = Some notice;
    workspace = unrestore value.doc doc value.workspace } (fun pane -> { pane with at = level })

let carry_commit value (c : _ carry) doc what =
  Pxui.Ui.cancel_carry value.ui;
  let value = install { value with carry = None } doc ~label:"Put" ~merge:Editor_core.History.Step in
  { value with notice = Some (Info, Printf.sprintf "Put %s · wrote %s · one undo entry" c.payload.value what) }

(* The carry's turn of a frame, before the panes build: the keys of the route and the ends of the
   gesture, then the preview (the scratch document the panels read), or the put or the restore.
   Returns the frame without the keys the carry took, and whether the document shown changed. *)
let carry_step value ~text_focus (frame : Frame.t) =
  match value.carry with
  | None ->
      (match Pxui.Ui.carrying value.ui with
       | Some payload ->
           { value with carry = Some { payload; via = `Pointer;
               original = Editor_core.History.present value.history; settled = value.cook.Cook.pieces;
               report = None; targets = None;
               chosen = None; anchor = None; preview = None; resting = None; hint = None } }, frame, false
       | None -> value, frame, false)
  | Some c ->
      let pressed = List.filter_map (function Input.Key_pressed k -> Some k | _ -> None) frame.input.events in
      let idle = value.leader = Leader.Idle in
      let escape = idle && List.mem Input.Escape pressed
      and lost = List.exists (function Input.Focus_lost -> true | _ -> false) frame.input.events in
      let typing = (not text_focus) && idle && value.prompt = None && value.menu = None in
      let c = if c.via = `Keys then carry_targets value c else c in
      let letters = match c.targets with
        | Some (_, targets) when c.via = `Keys && typing -> targets | _ -> [] in
      let letter = List.fold_left (fun found key -> match key with
        | Input.KeyChar ch -> (match List.find_opt (fun (l, _, _) -> l = String.make 1 ch) letters with
            | Some _ as hit -> hit | None -> found)
        | _ -> found) None pressed in
      let enter = c.via = `Keys && typing && List.mem Input.Enter pressed in
      let consumed key = (idle && key = Input.Escape)
        || (c.via = `Keys && typing && (key = Input.Enter
            || List.exists (fun (l, _, _) -> Input.KeyChar l.[0] = key) letters)) in
      let taken = { frame with input = { frame.input with
        events = List.filter (function
          | Input.Key_pressed k | Input.Key_released k -> not (consumed k) | _ -> true) frame.input.events;
        keys = List.filter (fun k -> not (consumed k)) frame.input.keys } } in
      let c = match letter with
        | Some (_, place, _) -> { c with chosen = Some place; anchor = Some frame.input.pointer; hint = None }
        | None -> c in
      let dropped = match c.report with Some { dropped = true; over } -> Some over | _ -> None
      and hovering = match c.report with Some { dropped = false; over } -> Some over | _ -> None in
      let pointer_wins = match c.anchor with Some at -> frame.input.pointer <> at | None -> true in
      let desired = match dropped, hovering with
        | Some _, _ -> dropped
        | None, Some _ when pointer_wins -> hovering
        | _ -> c.chosen in
      if escape || lost || (Pxui.Ui.carrying value.ui = None && dropped = None) then
        carry_cancel value c ~notice:(Info, if lost then "Carry dropped · the window lost focus · nothing written"
          else "Carry dropped · nothing written"), taken, true
      else begin
        let preview = if desired = Option.map carry_place c.preview then c.preview
          else Option.map (carry_try value c) desired in
        let c = { c with preview } in
        if dropped <> None || enter then
          (match preview with
           | Some (Showing { doc; what; _ } | Held_back { doc; what; _ }) ->
               carry_commit value c doc what, taken, true
           | Some (Refused { reason; _ }) ->
               carry_cancel value c ~notice:(Refusal, "Refused · " ^ reason ^ " · nothing written"), taken, true
           | None when dropped <> None ->
               carry_cancel value c ~notice:(Info, "Carry dropped · nothing written"), taken, true
           | None -> { value with carry = Some { c with hint = Some "Pick a target letter first, or Esc" } }, taken, false)
        else begin
          (* the pointer resting on a node enters what it references, after a moment *)
          let resting = match hovering with
            | Some (Carry.Node _ as place) ->
                (match c.resting with
                 | Some (q, since) when q = place -> Some (q, since)
                 | _ -> Some (place, frame.input.t))
            | _ -> None in
          let entered = match resting with
            | Some (Carry.Node path, since) when frame.input.t -. since >= carry_rest ->
                (* what the original document references, not the preview *)
                follow_target ~path { value with doc = c.original }
            | _ -> None in
          let c = { c with resting = (if entered <> None then None else resting) } in
          let value = { value with carry = Some c } in
          let value = match entered with Some graph -> go value graph | None -> value in
          let shown = carry_shown c in
          if shown == value.doc then value, taken, false
          else begin
            Cook.set_volatile value.cook (Flow_sop.Lower.is_volatile (shown.workspace.lowered));
            let level = Document.resolve_level shown (pane_level value) in
            with_pane { value with doc = shown; lisp_vocab = vocabulary_for value shown;
              workspace = unrestore value.doc shown value.workspace } (fun pane -> { pane with at = level }),
            taken, true
          end
        end
      end
