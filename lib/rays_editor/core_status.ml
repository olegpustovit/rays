open Rays
open Sop
open Editor_document
include Core_setup

(* the open dialog, for the crash report *)
let prompt_name value = match value.prompt with
  | None -> "-" | Some Keys -> "keys" | Some (Saving _) -> "save preset" | Some (Palette _) -> "commands" | Some (Jumping _) -> "jump" | Some (Symbols _) -> "go to symbol" | Some (Referencing _) -> "references" | Some (Going _) -> "go to line" | Some (Picking _) -> "panel kind" | Some (Splitting _) -> "split"
  | Some (Browsing _) -> "presets" | Some (Making_macro _) -> "make macro"

let level_name value = match value.level with
  | Document.Scene -> "scene"
  | Inside id -> "scene › " ^ Option.fold ~none:"?" ~some:Node.label
      (Edit_graph.find (scene value) ~node_id:id)

(* The strip while a payload is carried: what it is, then (a target hot) what putting it writes in
   the words of the text, or why the place refuses it. *)
let carry_line (c : _ carry) =
  let held = c.payload.value in
  let put = match c.via with `Keys -> "Enter" | `Pointer -> "release" in
  match c.preview with
  | Some (Showing { what; _ }) -> Printf.sprintf "Preview · %s writes %s · Esc drops" put what
  | Some (Held_back { what; reason; _ }) ->
      Printf.sprintf "Would write %s · no preview, %s · %s writes it" what reason put
  | Some (Refused { reason; _ }) -> "Refused · " ^ reason
  | None ->
      (match c.hint with Some hint -> hint ^ " · " | None -> "") ^
      (match c.via, c.targets with
       | `Pointer, _ -> Printf.sprintf "Carrying %s · put it on a place · Esc drops" held
       | `Keys, Some (_, (_ :: _ as targets)) ->
           Printf.sprintf "Carrying %s · put on %s · Enter writes · Esc drops" held
             (String.concat " · " (List.map (fun (letter, _, label) -> letter ^ " " ^ label) targets))
       | `Keys, _ ->
           Printf.sprintf "Carrying %s · nothing here takes it · u, i or / j go elsewhere · Esc drops" held)

let status_text ?(brief = false) value =
  match value.carry with Some c -> carry_line c | None ->
  let cook = match Cook.status value.cook with
    | Async_cook.Cooking { seconds; queued = true; _ }
      when Timeline.mode value.timeline = Timeline.Playing ->
        (* playing and a newer frame is already waiting: frames are skipped *)
        Printf.sprintf "cook %d ms · skipping frames" (int_of_float (seconds *. 1000.))
    | Async_cook.Cooking { seconds; queued; _ } ->
        Printf.sprintf "Cooking… %.1fs%s" seconds
          (if queued then " · latest queued" else "")
    | Idle ->
        (match value.edit_error, value.cook.error, value.cook.seconds with
         | _ when projection value = Text_view && Text_pane.first_error value.text <> None ->
             "Text rejected: " ^ Flow.Diagnostic.to_string
               (Option.get (Text_pane.first_error value.text))
         | Some error, _, _ -> "Graph edit rejected: " ^ error
         | None, Some error, _ -> "Cook rejected: " ^ error
         | None, None, _ when value.notice <> None -> snd (Option.get value.notice)
         | None, None, Some seconds ->
             (match Flow_sop.Lower.status (snd value.doc.Document.workspace) ~seconds with
              | Some text -> text
              | None -> Printf.sprintf "checked · cooked %.3f s" seconds)
         | None, None, None -> "Waiting for first cook") in
  let phases = Flow.Phase_timer.summary value.edit_phases in
  let cook = cook ^ (if phases = "" then "" else " · " ^ phases) in
  (* What the open level's keys do, so the World and the menu are findable. *)
  let hint = match value.level with
    | Document.Scene -> "i/double-click enter · / a add · / e World"
    | Inside id when kind value id = Some "world" ->
        "u up · drag map: move layer/sun · t dome/light · n reseed · d day cycle · [ ] time · / m 3D/map"
    | Inside _ -> "u up · / a add · / l panel kind" in
  (if value.workspace.restored then "Default layout · / z returns to the editor graph · " else "")
  ^ cook ^ (if brief then "" else " · " ^ level_name value ^ " · " ^ hint)

(* "ring · iteration 1 of 12": which iteration the viewport's highlight and the inspector show for the
   node selected in the graph pane (the innermost loop around it, or itself when it is one). *)
let probe_caption value = match value.scope_key, Pxui_graph.Scope.selected value.scope_view with
  | Some { scope; records = Some records; graph; _ }, [ path ] when graph_name value = Some graph ->
      let probe p = Option.value ~default:0 (Layout_by_path.Path_map.find_opt p value.probes) in
      let zones = List.filter (fun (n : Flow_graph.Projection.node) ->
        match n.zone with Some z -> z.kind <> Flow_graph.Projection.Let | None -> false)
        (Flow_graph.Projection.zones scope) in
      let own = List.find_opt (fun (n : Flow_graph.Projection.node) -> n.path = path) zones in
      let zone = match own with
        | Some n -> Some n.path
        | None -> (match Hashtbl.find_opt (Flow_graph.Probe.chains scope) path with
            | Some (_ :: _ as chain) -> Some (List.nth chain (List.length chain - 1))
            | _ -> None) in
      Option.bind zone (fun zone ->
        Option.map (fun count ->
          Printf.sprintf "%s · iteration %d of %d" (List.nth zone (List.length zone - 1)) (probe zone + 1) count)
          (List.assoc_opt zone (Flow_graph.Probe.counts records scope ~probe)))
  | _ -> None

(* The status strip under the view: kit text on the ground, under a hairline. *)
let status_box value ui (frame : Frame.t) ~render_status ~error_status ~context =
  let g = geometry value value.workspace frame in
  (* the pane really in use: the focus when a leaf of its kind is open, else the first leaf *)
  let focus = match List.exists (fun (l : Pxui_shell.Layout.leaf) -> Leader.scope l.panel = Leader.scope value.focus) g.leaves, g.leaves with
    | false, l :: _ -> l.panel | _ -> value.focus in
  let value = { value with focus } in
  let x, y, width, height = g.status_at in
  (* what every strip says first: the workspace, a dot for its state, then the layout in use *)
  let file = value.file in
  let state = if error_status <> None || value.edit_error <> None || value.cook.error <> None then `Error
    else match Cook.status value.cook with Async_cook.Cooking _ -> `Busy | Idle -> `Ok in
  let layout = match Option.bind value.doc.Document.shell (fun s -> s.switch) with
    | Some { layouts; active } when active < List.length layouts ->
        Printf.sprintf "layout %d \xc2\xb7 %s" active (Editor_core.Panels.summary (List.nth layouts active))
    | _ -> "" in
  let line = match error_status with Some error -> error | None ->
    status_text ~brief:true value ^ match render_status with None -> "" | Some status -> " \xc2\xb7 " ^ status in
  (* the focused pane's kind and what is selected: in the graph pane its node, in a viewport the
     object the brackets stand round *)
  let kind = match value.focus with Pxui_shell.Layout.View _ -> "Viewport" | panel -> Editor_core.Panels.name panel in
  let selection = match value.focus, value.selected_box with
    | Pxui_shell.Layout.View _, Some (_, _, name) -> Some name
    | _ ->
    match inspector_paths (Pxui_graph.Scope.selected value.scope_view) with
    | [ path ] -> List.nth_opt (List.rev path) 0
    | [] -> None
    | paths -> Some (Printf.sprintf "%d selected" (List.length paths)) in
  (* a panel being carried: its name in the accent, what the drag does, and how many windows float *)
  let moving = Option.bind value.workspace.window_live (fun (path, _) ->
    List.find_map (fun (l : Pxui_shell.Layout.leaf) ->
      if l.path = path then Some (Editor_core.Panels.name l.panel) else None) g.leaves) in
  let floating = List.length (List.filter (fun (l : Pxui_shell.Layout.leaf) -> l.floating) g.leaves) in
  let graphs = List.length (fst value.doc.Document.workspace).checked.graphs in
  (* the end follows the sheet's context: floating windows give their count alone; a graph that is
     the only docked pane (graph.html) gives its counts and zoom in place of layout and fps; any
     other layout keeps the layout summary and the frame rate *)
  let docked = List.filter (fun (l : Pxui_shell.Layout.leaf) -> not l.floating) g.leaves in
  let alone = floating = 0 && value.focus = Graph && List.length docked = 1 in
  let readout = if not alone then "" else
    let nodes = (Pxui_graph.Scope.stats value.scope_view).nodes in
    let selected = List.length (inspector_paths (Pxui_graph.Scope.selected value.scope_view)) in
    Printf.sprintf "%d node%s%s" nodes (if nodes = 1 then "" else "s")
      (if selected = 0 then "" else Printf.sprintf " \xc2\xb7 %d selected" selected) in
  let notes = if floating > 0 then [ Printf.sprintf "%d floating" floating ]
    else if alone then
      [ Printf.sprintf "zoom %d%%" (int_of_float (Float.round (100. *. Pxui_graph.Scope.zoom value.scope_view))) ]
    else match value.focus with
      | Pxui_shell.Layout.Outline -> [ Printf.sprintf "%d graph%s" graphs (if graphs = 1 then "" else "s") ]
      | _ -> [] in
  let layout, status_fps = if floating > 0 || alone then "", None else layout, value.status_fps in
  (* the keys each pane's sheet lists, as short labels; Lisp, Timeline and Inspector have none *)
  let jump = List.find_map (fun (c : Leader.command) ->
    if c.id = "scene.jump" then Option.map (fun t -> Editor_core.Keymap.label t, "jump") c.trigger else None) Leader.keymap in
  let extra = match value.focus with
    | Pxui_shell.Layout.Outline -> [ "s", "filter"; "i", "enter" ] @ Option.to_list jump
    | Graph -> [ "Tab", "add after"; "o", "open"; "v", "view"; "b", "bypass"; "i", "enter"; "f", "frame"; "w", "hints";
                 "/", "leader" ]
    | View _ -> [ "w", "move"; "e", "rotate"; "r", "scale"; "i", "enter object"; "\xe2\x8c\xa5 drag", "orbit" ]
    | _ -> [] in
  if height <= 0 then ()
  else if (match value.leader with Leader.Pending _ -> true | Idle -> false) then begin
    (* an open leader: the pending prefix in the accent and what the strip waits for *)
    let prefix = match value.leader with Leader.Pending p when p <> "" -> "/ " ^ p | _ -> "/" in
    Pxui_shell.Status_bar.guide ui ~bounds:(x, y, width, height) ~file ~state ~layout ~readout ~text:line
      ?fps:status_fps ~notes ~leader:prefix ~context:Editor_core.Guide_context.Leader ()
  end
  else if moving <> None then begin
    Pxui_shell.Status_bar.guide ui ~bounds:(x, y, width, height) ~file ~state ~layout ~readout ~text:line
      ?fps:status_fps ~notes ~accent:true ~kind:("moving " ^ Option.get moving)
      ~extra:[ "drag to an edge", "dock"; "/ o f", "float or dock"; "/ n", "new window" ]
      ~context:Editor_core.Guide_context.Canvas ()
  end
  else if state <> `Error && value.carry = None && value.guide then begin
    Pxui_shell.Status_bar.guide ui ~bounds:(x, y, width, height) ~file ~state ~layout ~readout ~text:line
      ?fps:status_fps ~notes ~extra ~kind ?selection ~context ()
  end
  else begin
    (* a refusal has the strip to itself: the whole message, up to the layout and frame rate *)
    let kind, selection = if state = `Error then None, None else Some kind, selection in
    Pxui_shell.Status_bar.draw ui ~bounds:(x, y, width, height) ~file ~state ~layout ~notes ~readout ?kind ?selection
      ~text:line ~fps:status_fps ()
  end
