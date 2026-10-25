open Rays
open Sop
open Editor_document
include Core_status

(* Leader actions owned by the workspace; the environment handles the rest
   from [update.actions]. *)
let apply_action value (workspace, selection, tree, timeline, changes) action =
  let module T = Timeline in
  let timeline_step step = let timeline, more = step timeline in
    workspace, selection, tree, timeline, changes @ more in
  let expand panel (workspace : shell) =
    { workspace with hidden = Pxui_shell.Layout.expand panel workspace.hidden } in
  match action with
  | Leader.Toggle_timeline | Toggle_graph | Toggle_inspector -> workspace, selection, tree, timeline, changes
  | Restore_layout ->
      (if workspace.restored then { workspace with restored = false; live = None; window_live = None }
       else { tree = Pxui_shell.Layout.default; hidden = [ Pxui_shell.Layout.Timeline ]; live = None; window_live = None;
              restored = true }), selection, tree, timeline, changes
  | Open_camera ->
      expand Pxui_shell.Layout.Inspector workspace,
      Selection.clear selection, tree, timeline, changes
  | Play_pause -> timeline_step T.toggle_pause
  | Reset -> timeline_step T.reset
  | Stop -> timeline_step T.stop
  | Add_node -> expand Pxui_shell.Layout.Graph workspace, selection, tree, timeline, changes
  | Frame_tile when projection value = List_view ->
      workspace, selection, Pxui_shell.Tree.reveal tree, timeline, changes
  | World_play when Timeline.mode timeline <> Timeline.Playing ->
      timeline_step T.toggle_pause
  | Frame_tile | Hide_ui | Look_through | Look_through_camera | Render_mode _ | Fly | Save_preset | Browse_presets | Save_source
  | Reload_source | Open_source
  | List_command _ | Frame_camera | Undo | Redo | Command_palette
  | Guide_toggle | Guide_keys | Copy_lisp
  | Scope_command _ | Toggle_map | Ui_scale _ | Enter | Up | Back _ | Go_world
  | Panel_split _ | Panel_close | Panel_retype _ | Panel_pick | Window_pick | Panel_split_at _ | Tool _
  | Layout_switch _ | Layout_new | Layout_remove | Window_new _ | Float_toggle | Peek | Pick_up | Jump
  | Text_command _ | Goto_symbol | Goto_line
  | World_emit | World_reseed | World_time _ | World_play | World_preset _ ->
      workspace, selection, tree, timeline, changes

(* The undo label a graph intent gives its document change. *)
let intent_label = function
  | Dock_panels _ -> Some "Dock panel"
  | Panel_state (_, state) -> Some (if state.Editor_core.Panels.collapsed then "Collapse panel" else "Arrange panel")
  | Select_layout _ -> Some "Switch layout"
  | Syntax_edit op -> Some (Flow_graph.Flow_edit.label op)
  | Syntax_batch (label, _) -> Some label
  | Syntax_inline { make; _ } -> Some (Flow_graph.Flow_edit.label (make []))
  | Set_parameter { path; _ } -> Some ("Set " ^ path)
  | Object_arg _ -> Some "Edit expression"
  | Rename _ -> Some "Rename node"
  | Pin_row { pin; _ } -> Some (if pin = Some false then "Unpin row from card" else "Pin row to card")
  | Follow_source _ | Notice _ | Declined _ -> None

let in_world value = match value.level with
  | Document.Inside id -> kind value id = Some "world"
  | Scene -> false

(* World keys: edit the selected layer, or the World node in the scene. *)
let world_keys value (doc : Document.t) selection actions =
  let world_id = match value.level with Inside id -> Some id
    | Scene -> None in
  let module Sync = Editor_document.Scene_sync in
  (* what a key asks, as an edit of the text at a level *)
  let edit_layer f label = match world_id, Selection.selected selection with
    | Some id, Some layer ->
        Option.bind (Document.network doc (Inside id)) (fun network ->
          Option.bind (Edit_graph.find network.graph.geometry ~node_id:layer) f)
        |> Option.map (fun values -> Document.Inside id, Sync.Fields [ layer, values ], label)
    | _ -> None in
  let edit_world f label = Option.bind world_id (fun id ->
    Option.bind (Edit_graph.find doc.Document.scene.graph.geometry ~node_id:id) f
    |> Option.map (fun values -> Document.Scene, Sync.Fields [ id, values ], label)) in
  (* a World the text does not have is edited as a derived object, for reconcile to adopt *)
  let with_fields (doc : Document.t) level edits =
    match Document.network doc level with
    | None -> doc
    | Some network ->
        let graph = List.fold_left (fun graph (id, values) ->
          match Edit_graph.apply_parameters graph ~node_id:id values with
          | Ok (graph, _) -> graph | Error _ -> graph) network.graph.geometry edits in
        Document.with_network doc level
          { network with graph = Result.get_ok (Flow_sop.Network.with_geometry graph network.graph) } in
  let derived (doc : Document.t) level : Sync.edit -> Document.t = function
    | Fields edits -> with_fields doc level edits
    | Layers (network, values) ->
        (match level with
         | Inside id -> with_fields (Document.with_network doc level network) Document.Scene [ id, values ]
         | Scene -> doc)
    | Rename _ | Delete _ | Restack _ | Camera _ | Root _ | Settings _ | Reparent _ | Outdent _ -> doc in
  let field name node = List.find_map (fun (field : Parameter.field_view) ->
      if field.name = name then Some field.current else None) (Node.parameter_fields node) in
  List.filter_map (function
    | Leader.World_emit -> edit_layer (fun node -> match field "emit" node with
        | Some (Parameter.Choice_value "Light") -> Some ["emit", Parameter.Choice_value "Dome"]
        | Some (Parameter.Choice_value _) -> Some ["emit", Parameter.Choice_value "Light"]
        | _ -> None) "Dome / light"
    | World_reseed -> edit_layer (fun node -> match field "seed" node with
        | Some (Parameter.Int_value seed) -> Some ["seed", Parameter.Int_value (seed + 1)]
        | _ -> None) "Reseed"
    | World_time delta -> edit_world (fun node -> match field "time_of_day" node with
        | Some (Parameter.Float_value hours) ->
            Some ["time_of_day", Parameter.Float_value
              (Float.max 0. (Float.min 24. (hours +. delta)))]
        | _ -> None) "Time of day"
    | World_play -> edit_world (fun node -> match field "day_cycle" node with
        | Some (Parameter.Float_value 0.) -> Some ["day_cycle", Parameter.Float_value 1.]
        | _ -> None) "Day cycle"
    | World_preset index ->
        (match world_id, List.nth_opt World.presets index with
         | Some id, Some (name, world) ->
             (match Layers.network_of_world world with
              | Ok network -> Some (Document.Inside id, Sync.Layers (network, snd (Layers.of_world world)), "Preset " ^ name)
              | Error _ -> None)
         | _ -> None)
    | _ -> None) actions
  |> List.fold_left (fun (derived_doc, _, edits) (level, edit, label) ->
    if Sync.writes doc level edit then derived_doc, Some label, edits @ [ level, edit ]
    else derived derived_doc level edit, Some label, edits) (doc, None, [])

(* The ten digit commands take their labels from the layouts when which-key or the palette draws. *)
let layout_labels value keymap =
  let names = layouts value in
  List.filter_map (fun (command : Leader.command) -> match command.action with
    | Leader.Layout_switch i ->
        Option.map (fun (name, active) -> { command with Editor_core.Command.label =
          if active then name ^ " (active)" else name }) (List.nth_opt names i)
    | _ -> Some command) keymap

(* Commands that only mean something on this level and projection. *)
let routed value =
  let graph_shown = graph_family value in
  let listing = listing value and texting = texting value in
  List.filter (fun (command : Leader.command) -> match command.action with
    | List_command _ -> listing && graph_shown
    | Scope_command _ -> scope_name value <> None && graph_shown && not texting
    | Frame_tile -> not texting && graph_shown
        && (listing || command.trigger = Some (Editor_core.Keymap.Leader "f"))
    | Frame_camera -> List.exists (function _, Pxui_shell.Layout.View _ -> true | _ -> false)
        (Editor_core.Panels.leaves (shell_tree value value.workspace))
    (* Tab is the canvas's key for it; in the list Tab indents the row *)
    | Add_node -> not texting && (not listing || command.trigger = Some (Editor_core.Keymap.Leader "a"))
        && List.exists (fun (_, panel) -> panel = Pxui_shell.Layout.Graph)
        (Editor_core.Panels.leaves (shell_tree value value.workspace))
    | World_emit | World_reseed | World_time _ | World_play | World_preset _ ->
        in_world value
    | _ -> true) value.keymap |> layout_labels value

