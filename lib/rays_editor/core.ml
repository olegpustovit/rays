open Core_model
open Rays
open Sop
open Editor_document
open Core_viewport_pane
open Core_shell
open Core_panes
open Core_actions
open Core_text
open Core_carry
open Core_panels
open Core_scope
open Core_menu
open Core_inspect
open Core_list
open Core_status
open Core_add
open Core_inspector_pane
open Core_setup
open Core_reduce

(* a viewer or a canvas: the panels a sketch's picture is drawn in *)
let is_picture : Pxui_shell.Layout.panel -> bool = function
  | View _ | Canvas _ -> true
  | _ -> false

let update_frame ?gpu ?pump ~image ~host_events ~carry_changed value ~all_ui_visible ~text_focus ~camera_panel ~view_handles
    ~render_status ~error_status ~view_state (frame : Frame.t) =
  let carrying = value.carry <> None in
  let window_focus = List.fold_left (fun f -> function
    | Input.Focus_lost -> -1 | Input.Focus_gained -> 0 | _ -> f) value.window_focus frame.input.events in
  let window_focus = if window_focus >= 0 then min 2 (window_focus + 1) else -1 in
  let live_frame, sketch_blocked =
    let pane_is_picture = match Pxui.Ui.root_at value.ui frame.input.pointer (List.map fst value.pane_keys) with
      | None -> true
      | Some key -> (match List.assoc_opt key value.pane_keys with
          | Some (panel, _) -> is_picture panel
          | None -> false) in
    Input.Gate.gate { prompt = value.prompt <> None; menu = value.menu <> None; leader = value.leader <> Leader.Idle;
      text_focus;
      focus_on_picture = is_picture value.focus;
      pointer_on_picture = pane_is_picture;
      window_key = window_focus >= 2;
      pressed = List.exists (function Input.Pointer_pressed _ -> true | _ -> false) frame.input.events;
      previous = value.live_frame; blocked = value.sketch_blocked }
      (Flow_native.Live_frame.of_frame frame) in
  let live_frame=if host_events=[]then live_frame else
    {live_frame with events=host_events @ live_frame.events} in
  let value = {value with live_frame; sketch_blocked; window_focus} in
  let value = { value with workspace = { value.workspace with hidden = shell_hidden value value.workspace } } in
  let value = follow_start value in
  let focus, focus_path = if all_ui_visible then
      match Pxui.Ui.last_press_within value.ui frame
          (List.map fst value.pane_keys) with
      | Some key -> Option.value ~default:(value.focus, value.focus_path)
          (List.assoc_opt key value.pane_keys)
      | None -> value.focus, value.focus_path
    else value.focus, value.focus_path in
  (* a press in another graph panel makes it the pane in use before anything reads the pane *)
  let value = follow_graph value ~focus ~focus_path in
  (* the lists, text panes and outlines open, and the ones the focus makes the ones in use *)
  let hosts = if all_ui_visible then panel_hosts value else [] in
  let value = follow_hosts value ~focus_path hosts in
  (* a field of the graph pane owns the keys only while the pane is drawn: one left open when the
     panel turned to its list or text view must not lock the keyboard *)
  let modal = value.prompt <> None || value.menu <> None
    || (all_ui_visible && graph_family value && scope_name value <> None
        && Pxui_graph.Scope.editing value.scope_view)
    || Pxui_shell.Tree.editing value.tree in
  let text_focus = text_focus || modal in
  let keymap = routed { value with focus } in
  let keymap = if all_ui_visible then keymap else List.filter (fun command ->
      match command.Editor_core.Command.action with
      | Leader.List_command _ | Leader.Frame_camera -> false
      | _ -> true) keymap in
  let held_keys = frame.input.keys in
  let leader, commands, frame = Editor_core.Router.step ~previous_keys:value.held_keys
      keymap ~focus:(Leader.scope focus) ~text_focus ~frame value.leader in
  let hud = match List.rev commands with
    | command :: _ -> Some (Editor_core.Keymap.label (Option.get command.trigger)
        ^ " · " ^ command.label, frame.input.t +. 1.5)
    | [] -> (match leader with
        | Leader.Pending prefix when leader <> value.leader ->
            Some ((if prefix = "" then "Space" else "/ " ^ prefix) ^ " · leader", frame.input.t +. 1.5)
        | _ -> value.hud) in
  let actions = List.map (fun (command : Leader.command) -> command.action) commands in
  let hud = match hud with Some (_, until) when frame.input.t >= until -> None | _ -> hud in
  let guide = List.fold_left (fun guide -> function Leader.Guide_toggle -> not guide
    | _ -> guide) value.guide actions in
  (* Command-Z in a text whose own undo stack is empty (a dragged number applies as it goes)
     reaches the document history, unless a prompt, menu or rename owns the keys, or a text pane
     holds an unapplied draft: an extra Command-Z after the draft is undone must not change the
     document under it *)
  let passed = if text_focus && not modal && not (has_draft value) then
      match Pxui.Ui.passed_undo value.ui with
      | Some `Undo -> [ Leader.Undo ] | Some `Redo -> [ Leader.Redo ] | None -> []
    else [] in
  let actions = passed @ value.queued @ actions in
  let actions = if carrying then List.filter carry_allowed actions else actions in
  let copied = if List.mem Leader.Copy_lisp actions then Some (copy_workspace value) else None in
  (* Command +/-/0: the kit text of the panels (the graph pane zooms, viewports have no text) *)
  List.iter (function
    | Leader.Ui_scale delta ->
        let sizes = [ 8; 9; 10; 11; 12; 13; 14; 16; 18 ] in
        let default = Option.value ~default:5 (List.find_index (( = ) (default_text_size ())) sizes) in
        let here = Option.value ~default (List.find_index (( = ) (Pxui.Ui.font_size value.ui)) sizes) in
        let index = if delta = 0 then default else max 0 (min (List.length sizes - 1) (here + delta)) in
        Pxui.Ui.set_font_size value.ui (List.nth sizes index)
    | _ -> ()) actions;
  let steady = if frame.input.dt > 0. && frame.input.dt = value.last_dt then value.steady + 1 else 0 in
  let sample_fps = frame.input.t < value.status_fps_at
    || frame.input.t -. value.status_fps_at >= 1. in
  let status_fps, status_fps_at = if not sample_fps then
      value.status_fps, value.status_fps_at
    else (if frame.fps > 0. && Float.is_finite frame.fps
      then Some (int_of_float (Float.round frame.fps)) else None), frame.input.t in
  let shortcut_frame = if text_focus then
      { frame with input = { frame.input with events = List.filter (function
        | Input.Key_pressed _ | Input.Key_released _ | Input.Text_input _
        | Input.Text_editing _ -> false | _ -> true) frame.input.events } }
    else frame in
  let timeline, timeline_changes = Timeline.update
      value.timeline shortcut_frame in
  let workspace, selection, tree, timeline, timeline_changes = List.fold_left
      (apply_action value)
      (value.workspace, value.selection, value.tree, timeline, timeline_changes) actions in
  let value = sync_scope {value with timeline} in
  let other_panes = if not all_ui_visible then [] else
    List.filter_map (fun (key, path) ->
      if value.graph_pane = Some key then None else Some (key, path, sync_scope (as_pane value (key, path))))
      (List.sort_uniq (fun (a, _) (b, _) -> compare a b) (graph_leaves value)) in
  let vw = { value with workspace; focus; selection } in
  let graph_shown = all_ui_visible && graph_family vw in
  let listing = listing vw in
  let hosted kind = List.filter (fun (_, _, _, k) -> k = kind) hosts in
  let list_drawn = hosted `List <> [] in
  let ws, _ = value.doc.Document.workspace in
  (* What a text pane shows: a graph panel's own graph and selection, a lisp panel the ones of the
     graph pane in use (the first graph of the workspace when none is open). *)
  (* a panel's pane, laid out: the one in use, or one of the others *)
  let pane_of (key, path, panel, _) =
    let pane = shown_as value (key, path, panel) in
    if pane.graph_pane = value.graph_pane then value
    else match List.find_opt (fun (k, _, _) -> Some k = pane.graph_pane) other_panes with
      | Some (_, _, synced) -> synced | None -> pane in
  let in_use pane = pane.graph_pane = value.graph_pane in
  let text_of state ((_, _, panel, _) as host) =
    let pane = pane_of host in
    let graph = match graph_name pane with
      | None when panel = Lisp -> Option.map (fun (g : Flow.Workspace.graph) -> g.name) (List.nth_opt ws.checked.graphs 0)
      | graph -> graph in
    Option.map (fun graph ->
      let selected = match Pxui_graph.Scope.selected pane.scope_view with
        | [ path ] -> Some path | _ -> None in
      (* while a payload is carried the text is the document it was picked up from: the preview is
         not drawn here, so the byte under the pointer does not move when a put is previewed *)
      let shown_ws = match value.carry with
        | Some c -> fst c.original.Document.workspace
        | None -> ws in
      Text_pane.shown state ~workspace:shown_ws ~source:shown_ws.source ~graph ~selected) graph in
  let text_host = List.find_opt (fun (key, _, _, _) -> Some key = value.text_at) (hosted `Text) in
  let text, text_shown = match Option.bind text_host (text_of value.text) with
    | Some (text, shown) -> text, Some shown
    | None -> value.text, None in
  (* the keys of the text (find, go to) wait in the pane's state for its view; nothing without one *)
  let text = if text_shown = None then text else List.fold_left (fun text -> function
    | Leader.Text_command command -> Text_pane.command text command
    | _ -> text) text actions in
  let other_texts = List.filter_map (fun ((key, path, _, _) as host) ->
    if Some key = value.text_at then None
    else Option.map (fun (state, shown) -> key, path, state, shown) (text_of (local_of value key).code host))
    (hosted `Text) in
  let scope_active = graph_shown && scope_name value <> None in
  let scope_view, scope_command_changes = List.fold_left (fun (view, changes) -> function
    | Leader.Scope_command command when scope_active ->
        let view, emitted = Pxui_graph.Scope.run_command view command in
        view, changes @ emitted
    | Leader.Open_camera -> Pxui_graph.Scope.clear_selection view, changes
    | _ -> view, changes) (value.scope_view, []) actions in
  (* / a: the node menu of the graph the pane shows *)
  let menu, command_changes =
    if List.mem Leader.Add_node actions && graph_shown then begin
      let gx, gy, gw, gh = match graph_leaf value (geometry value workspace frame) with
        | Some leaf -> leaf.body | None -> 0, 0, 0, 0 in
      let mx, my = frame.input.pointer in
      let at = if mx >= float gx && my >= float gy && mx < float (gx + gw) && my < float (gy + gh)
        then int_of_float mx, int_of_float my else gx + (gw / 3), gy + (gh / 3) in
      match open_menu value at with
      | Some menu -> Some menu, []
      | None -> value.menu, [ Declined "Open a graph to add a node to it" ]
    end else value.menu, [] in
  let document = document value in
  let displayed = match value.level with
    | Inside id -> (match Document.Int_map.find_opt id value.viewed with
      | Some (_, displayed) -> Some displayed | None -> (network value).displayed)
    | Scene -> (network value).displayed in
  (* the rows of a level's list, made once per network and kept while a list shows them *)
  let rows_of sets pane =
    let network = network pane in
    let key = network.displayed, pane.doc.Document.active_camera in
    let same (source, k, _) = source == network.graph && k = key in
    match List.find_opt same sets with
    | Some (_, _, rows) -> sets, rows
    | None ->
        let entry = match List.find_opt same value.rows with
          | Some entry -> entry | None -> network.graph, key, rows pane network in
        let _, _, rows = entry in entry :: sets, rows in
  let row_sets, (rows, columns) = if listing || list_drawn then rows_of [] value else [], ([||], []) in
  (* the rows of every list that draws, by panel key: made here, before the frame builds *)
  let row_sets, host_rows = List.fold_left (fun (sets, by_host) ((key, _, _, _) as host) ->
    let sets, rows = rows_of sets (pane_of host) in
    sets, (key, rows) :: by_host) (row_sets, []) (hosted `List) in
  let selected () = Selection.selected_nodes selection
    |> List.sort (fun a b -> if Some a = Selection.selected selection then -1
      else if Some b = Selection.selected selection then 1 else Int.compare a b) in
  let tree, list_intents = List.fold_left (fun (tree, intents) -> function
    | Leader.List_command Pxui_shell.Tree.Activate_row -> tree, intents
    | Leader.List_command command when graph_shown ->
        let tree, emitted = Pxui_shell.Tree.run_command tree rows
            ~selected:(selected ()) command in
        tree, intents @ emitted
    | _ -> tree, intents) (tree, []) actions in
  let open_graph = if not graph_shown then None else
    List.find_map (function
      | Leader.List_command Pxui_shell.Tree.Activate_row when listing ->
          (match Pxui_shell.Tree.focused tree with
           | Some id -> Some id | None -> Selection.selected selection)
      | _ -> None) actions in
  let initial_frame_request = if not (List.mem Leader.Frame_camera actions) then None else displayed in
  (* what the graph pane marks while a payload is carried: the places that take it (the key
     route's letters) and the one under the pointer *)
  let carry_lit, carry_hot = match value.carry with
    | None -> [], None
    | Some c ->
        let path_of = function
          | Carry.Node p -> Some p
          | Graph g -> Some [ g ]
          | Object id | Surface { object_ = id; _ } ->
              (match List.assoc_opt id value.doc.Document.homes.objects with
               | Some (Document.Bound_at p) -> Some p | _ -> None)
          | Viewport _ | Text _ -> None in
        (match c.targets with
         | Some (_, targets) when c.via = `Keys ->
             List.filter_map (fun (letter, place, _) -> Option.map (fun p -> p, letter) (path_of place)) targets
         | _ -> []),
        Option.bind c.preview (fun preview ->
          Option.map (fun p -> p, (match preview with Showing _ -> true | _ -> false))
            (path_of (carry_place preview))) in
  let build ui =
    (* Chrome first: panel backgrounds, splitters and headers.  Its intents fold into the
       shell now (a fold, a drag) or become one editor-graph edit after the frame. *)
    let header_focus = match focus_path with
      | Some _ as path -> path
      | None -> List.find_map (fun (path, p) -> if p = focus then Some path else None)
                  (Editor_core.Panels.leaves (shell_tree value workspace)) in
    (* the tabs standing at a header's end, outermost first: a graph panel's Graph / List / Text,
       then a text view's Selection / Graph / Document.  [Pxui_shell.Chrome.header_slots] places
       them and cuts the title, so a narrow header drops the text tabs, then the views, and never
       draws one over another *)
    let header_groups (leaf : Pxui_shell.Layout.leaf) =
      let text = match text_host, text_shown with
        | Some (_, path, _, _), Some shown when path = leaf.path -> Some (text, shown)
        | _ -> List.find_map (fun (_, path, state, shown) ->
            if path = leaf.path then Some (state, shown) else None) other_texts in
      (if leaf.panel = Graph then [ Pxui.Ui.text_width ui "GraphListText" +. 24. ] else [])
      @ Option.to_list (Option.map (fun (state, shown) ->
          Text_pane.tabs_width ui ~width:(let _, _, w, _ = leaf.body in w) state shown) text) in
    let header_slots (leaf : Pxui_shell.Layout.leaf) =
      fst (Pxui_shell.Chrome.header_slots leaf (header_groups leaf)) in
    let intents = Pxui_shell.Chrome.update ~state:(panel_state value) ~hidden:workspace.hidden ~title:(panel_title vw)
        ~groups:header_groups
        ~key_of:(fun id -> match List.find_opt (fun (c : Leader.command) -> c.id = id) keymap with
          | Some { trigger = Some trigger; _ } -> Editor_core.Keymap.label trigger | _ -> "")
        ?focus:header_focus (shell_tree value workspace) ui shortcut_frame in
    (* where a header's tools begin: after its title as the chrome laid it out *)
    let tools_from (leaf : Pxui_shell.Layout.leaf) =
      Pxui_shell.Chrome.tools_start ui ~focused:(header_focus = Some leaf.path) ~floating:leaf.floating
        ~collapsed:false (panel_title vw leaf) in
    let leaves = Editor_core.Panels.leaves (shell_tree value workspace) in
    let panel_paths panel = List.filter_map (fun (path, p) -> if p = panel then Some path else None) leaves in
    let toggle panel = List.map (fun path -> Pxui_shell.Chrome.Toggle path) (panel_paths panel) in
    let expand panel = List.filter_map (fun path ->
      if (panel_state value path).collapsed then Some (Pxui_shell.Chrome.Toggle path) else None) (panel_paths panel) in
    let intents = intents @ List.concat_map (function
      | Leader.Toggle_graph -> toggle Graph
      | Toggle_inspector -> toggle Inspector
      | Toggle_timeline -> if panel_paths Timeline = [] then [Pxui_shell.Chrome.Toggle [-1]] else toggle Timeline
      | Open_camera -> expand Inspector
      | Add_node -> expand Graph
      | _ -> []) actions in
    (* / o ...: the focused panel's split, close and retype, as its header menu *)
    let intents = intents @ (match focused_leaf (geometry value workspace frame) focus focus_path with
      | None -> []
      | Some leaf -> List.filter_map (function
          | Leader.Panel_split axis -> Some (Pxui_shell.Chrome.Split_panel (leaf.path, axis))
          | Panel_close -> Some (Pxui_shell.Chrome.Close_panel leaf.path)
          | Panel_retype (Graph | List | Lisp) when leaf.panel = Graph -> None  (* a view switch, below *)
          | Panel_retype panel -> Some (Pxui_shell.Chrome.Retype_panel (leaf.path, panel))
          | _ -> None) actions) in
    let intents = intents @ List.filter_map (function
      | Leader.Panel_split_at (path, axis, ratio, panel) -> Some (Pxui_shell.Chrome.Split_panel_at (path, axis, ratio, panel))
      | _ -> None) actions in
    let layout_leaf = focused_leaf (geometry value workspace frame) focus focus_path in
    let workspace, layout_changes = layout_intents vw workspace intents in
    let layout_changes = layout_changes @ layout_actions value workspace ~leaf:layout_leaf actions in
    let vw = { vw with workspace } in
    let g = geometry value workspace frame in
    (* the graph panel's toolbar; a click is one command or edit *)
    let graph_host = graph_leaf vw g in
    let tool_action, view_action, view_pick =
      draw_graph_header ~ui ~value:vw ~geometry:g ~graph_host ~header_slots ~tools_from in
    let root order (leaf : Pxui_shell.Layout.leaf) =
      let box = Pxui_shell.Chrome.pane_root ui frame ~bounds:leaf.body
        ("workspace-pane-" ^ Leader.pane_name leaf.panel ^ Pxui_shell.Chrome.key leaf.path) in
      if leaf.floating then Pxui.Ui.to_front ui ~order box;
      box in
    let roots = List.mapi (fun order leaf -> leaf, root order leaf) g.leaves in
    let viewport_drops = if not carrying then [] else
      List.filter_map (fun ((leaf : Pxui_shell.Layout.leaf), box) -> match leaf.panel with
      | View key ->
          Option.map (fun d -> Carry.Viewport key, (match d with
            | Pxui.Ui.Dropped _ -> true | Hover _ -> false))
            (Pxui.Ui.drop_target ui box)
      | _ -> None) roots in
    let timeline_root = if Pxui_shell.Layout.find g Timeline <> None then None
      else Some (Pxui_shell.Layout.Timeline, Pxui_shell.Chrome.pane_root ui frame ~bounds:g.timeline_at
        "workspace-pane-Timeline") in
    (* every leaf draws in its own root: its PXUI keys are seeded by it *)
    let root_at path = List.find_opt (fun ((l : Pxui_shell.Layout.leaf), _) ->
      l.path = path && (let _, _, _, h = l.body in h > 0)) roots in
    let of_kind panel = List.filter (fun ((l : Pxui_shell.Layout.leaf), _) ->
      l.panel = panel && visible workspace panel && (let _, _, _, h = l.body in h > 0)) roots in
    let none_root () = Pxui_shell.Chrome.pane_root ui frame ~bounds:(0, 0, 0, 0) "workspace-pane-none" in
    let graph_root = match Option.bind graph_host (fun (l : Pxui_shell.Layout.leaf) -> root_at l.path) with
      | Some (_, box) -> box | None -> none_root () in
    let active = active_view vw g in
    let view_root = draw_viewport_pane ~ui ~value ~caption:(probe_caption vw) ~roots ~active ~none_root in
    let graph_body, scope_view, scope_frame_changes, menu, menu_pick = draw_graph_pane
      ~ui ~frame ~value ~shortcut_frame ~scope_view ~scope_active ~guide ~carry_lit ~carry_hot
      ~graph_root ~graph_host ~workspace ~menu ~open_menu in
    (* Each pane builder returns only its frame intents and updated pane-local state. *)
    let tree, list_intents, locals = draw_list_panes ~ui ~shortcut_frame ~value
      ~hosted:(hosted `List) ~root_at ~pane_of ~in_use ~title_of:level_name ~rows ~columns ~host_rows
      ~selected ~tree ~intents:list_intents ~locals:value.locals in
    let text, text_intents, locals, other_texts = draw_text_panes ~ui ~frame ~value
      ~root_at ~header_slots ~text_host ~text_shown ~text ~other_texts ~locals in
    let outline, outline_intents, locals = draw_outline_panes ~ui ~value
      ~hosted:(hosted `Outline) ~root_at ~params_of:navigator_params ~outline:value.outline ~locals in
    let locals = draw_spreadsheet_panes ~ui ~value ~panes:(of_kind Pxui_shell.Layout.Spreadsheet) ~locals in
    let graph_panes = draw_other_graph_panes ~ui ~shortcut_frame ~value ~other_panes ~roots ~stash in
    let scope_changes = scope_command_changes @ scope_frame_changes in
    let changes = command_changes
      @ List.concat_map (function
        | Pxui_graph.Scope.Syntax_edit op -> [ Syntax_edit op ]
        | Notice message -> [ Declined message ]  (* the pane's notices are all refusals *)
        | Defn_requested paths -> [ defn_change vw paths ]
        | Copy_requested paths -> [ copy_bindings value paths ]
        | Paste_requested -> paste_bindings value
        | _ -> []) scope_changes
      @ (match menu_pick with Some key -> scope_add value key | None -> [])
      @ List.filter_map (function
        | Navigator.Set_default { graph; input; value; integer } ->
            let text = if integer then string_of_int (int_of_float (Float.round value))
              else Flow.Lisp.float value in
            Some (Syntax_edit (Flow_graph.Flow_edit.Set_input_default { form = graph; input;
              value = Flow.Syntax.make (Flow.Syntax.Num text) }))
        | Rename { graph; to_ } -> Some (Syntax_edit (Flow_graph.Flow_edit.Rename_graph { name = graph; to_ }))
        | Remove graph -> Some (Syntax_edit (Flow_graph.Flow_edit.Remove_graph { name = graph }))
        | New_graph context -> Some (Syntax_batch ("New graph", [ snd (new_graph value context) ]))
        | Flag { node; name; value } ->
            Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg { node; key = Flow_graph.Flow_edit.Kw name; sub = [];
              value = Flow.Syntax.make (Flow.Syntax.Sym (if value then "true" else "false")) }))
        | Open _ | Macro _ | Layout _ | Add -> None) outline_intents in
    (* a layout row and the add button are commands: they run like a toolbar click *)
    let outline_action = List.fold_left (fun action -> function
      | Navigator.Layout index -> Some (Leader.Layout_switch index)
      | Add -> Some Leader.Add_node
      | _ -> action) None outline_intents in
    (* a click on an earlier part of the graph header's route goes back that far *)
    let crumb_action = List.find_map (function
      | Pxui_shell.Chrome.Crumb (path, index) when value.back <> [] && List.mem (path, Pxui_shell.Layout.Graph) leaves ->
          let steps = min 3 (List.length value.back + 1) - 1 - index in
          if steps > 0 then Some (Leader.Back steps) else None
      | _ -> None) intents in
    (* the latest of the four is the command, as when each overwrote the one before *)
    let bar_action = List.fold_left (fun latest action -> if action = None then latest else action) None
        [ tool_action; view_action; outline_action; crumb_action ] in
    (* Selection is view state; inspection intents retain the selected stable
       id. Topology and parameter application wait until Ui.frame finishes. *)
    let selection = List.fold_left (fun selection -> function
      | Pxui_shell.Tree.Select [] -> Selection.clear
      | Select ids -> Selection.select_nodes ids
      | _ -> selection) selection list_intents in
    let inspector_visible = has_panel vw Pxui_shell.Layout.Inspector in
    let selected_ids = Selection.selected_nodes selection in
    let selected_id = match selected_ids with
      | [_] -> Selection.selected selection | _ -> None in
    let selected = Option.bind selected_id
        (fun node_id -> Edit_graph.find document ~node_id) in
    let unchanged = value.doc.settings in
    let lowered_level = match value.level with
      | Document.Inside id -> kind value id = Some "geometry" | Scene -> false in
    (* the split a card of the editor graph is, sized another way from what it shows now *)
    let resized node how = match node, value.doc.Document.shell with
      | [ graph; name ], Some shell when not workspace.restored
          && Option.map (fun (g : Flow.Workspace.graph) -> g.name)
               (Workspace_doc.editor_graph (fst value.doc.Document.workspace)) = Some graph ->
          Option.bind (List.find_map (fun (path, origin) ->
            if origin = Document.Bound name then Some path else None) shell.origins) (fun path ->
            Option.map (fun splitter -> Pxui_shell.Layout.resized splitter how)
              (List.find_opt (fun (s : Pxui_shell.Layout.splitter) -> s.node = Some path) g.splitters))
      | _ -> None in
    let inspectors = List.map (fun ((leaf : Pxui_shell.Layout.leaf), root) ->
      let pane = pane_of (panel_key value.doc leaf.path, leaf.path, leaf.panel, `List) in
      if in_use pane then leaf, inspect_panel ~image ~camera_panel ~inspector_visible ~unchanged ~resized ~geometry:g ~ui (value, scope_view, selection, scope_active) root leaf.body
      else
        let shown = graph_shown && stored_projection pane = Graph_view && graph_name pane <> None in
        let panel, _, _, _, moves, drops = inspect_panel ~image ~camera_panel ~inspector_visible ~unchanged ~resized ~geometry:g ~ui (pane, pane.scope_view, pane.selection, shown) root leaf.body in
        leaf, (panel, [], [], value.live_cook, moves, drops))
      (of_kind Pxui_shell.Layout.Inspector) in
    let panel = match List.find_opt (fun ((l : Pxui_shell.Layout.leaf), _) ->
        focus = Inspector && Some l.path = focus_path) inspectors, inspectors with
      | Some (_, (panel, _, _, _, _, _)), _ | None, (_, (panel, _, _, _, _, _)) :: _ -> panel
      | None, [] -> None in
    let inspector_changes = List.concat_map (fun (_, (_, changes, _, _, _, _)) -> changes) inspectors
    and settings_changes = List.concat_map (fun (_, (_, _, changes, _, _, _)) -> changes) inspectors
    and workspace_moves = List.concat_map (fun (_, (_, _, _, _, moves, _)) -> moves) inspectors
    and inspector_drops = List.concat_map (fun (_, (_, _, _, _, _, drops)) -> drops) inspectors
    and live_cook = Option.value ~default:value.live_cook
      (List.find_map (fun (_, (_, _, _, live, _, _)) -> if live <> value.live_cook then Some live else None) inspectors) in
    let changes = changes @ inspector_changes @ layout_changes in
    let scope_changes = scope_changes @ workspace_moves in
    (* every timeline leaf, else the strip under the tree *)
    let timeline_slots = match of_kind Timeline with
      | [] -> Option.to_list (Option.map (fun (_, box) -> g.timeline_at, box, true) timeline_root)
      | leaves -> List.map (fun ((l : Pxui_shell.Layout.leaf), box) -> l.body, box, false) leaves in
    let timeline_intents, strip_layout = draw_timeline_pane ~ui ~timeline ~max_frame:value.timeline_frames
      ~slots:timeline_slots ~timeline_root ~strip_bounds:g.timeline_at in
    (* a lowered node is edited through its text: the handles of the node selected in the
       graph pane write its arguments, the list's rows of a geometry object have none *)
    let scope_target = if scope_active then scope_node value else None in
    let handle_target = match scope_target with
      | Some (_, node) -> Some node
      | None -> if lowered_level then None else selected in
    let handle_edits, grab, picked = match active with
      | Some leaf when visible workspace leaf.Pxui_shell.Layout.panel ->
          Pxui.Ui.within ui view_root (fun () ->
            view_handles ui ~selected:handle_target ~space:(space { value with selection })
              ~bounds:leaf.body)
      | _ -> [], false, None in
    let selection = match picked with
      | Some id when value.level = Document.Scene -> Selection.select id
      | Some _ | None -> selection in
    let handle_ops, handle_changes = match scope_target, handle_target, handle_edits with
      | Some (path, node), _, _ :: _ ->
          let parameters = Result.value ~default:[] (Flow_sop.Port.parameters (Node.parameter_fields node)) in
          List.filter_map (fun (p : Flow_sop.Port.parameter) ->
            if List.exists (fun (f : Param.field_view) -> List.mem_assoc f.name handle_edits) p.fields
            then Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = path;
              key = Flow_graph.Flow_edit.Kw p.path; sub = [];
              value = Editor_document.Scene_sync.value_syntax (List.map (fun (f : Param.field_view) ->
                match List.assoc_opt f.name handle_edits with
                | Some current -> { f with current } | None -> f) p.fields) }))
            else None) parameters, None
      | _, Some node, _ :: _ -> [], Some (Node.id node, handle_edits)
      | _ -> [], None in
    let changes = changes @ handle_ops in
    let context = match leader with
      | Leader.Pending _ -> Editor_core.Guide_context.Leader
      | Idle when value.prompt <> None || menu <> None
          || Pxui_graph.Scope.editing scope_view -> Search
      | Idle when listing -> List
      | Idle -> (match (if scope_active then Pxui_graph.Scope.selected scope_view
                        else List.map (fun id -> [ string_of_int id ]) (Selection.selected_nodes selection)) with
          | [] -> Canvas | [_] -> Node | _ -> Multi) in
    status_box { value with workspace; status_fps; selection; guide; focus; leader }
      ui frame ~render_status ~error_status ~context;
    (* echo, the sheet's [08]: messages only (saved, undo and redo results, refusals), never a key
       press; the last one fades after 3 seconds.  It stands at the bottom-left of the focused pane,
       above the strip. *)
    (match value.notice with
     | Some (kind, text) when frame.input.t -. value.notice_at < 3. ->
         let bounds = match List.find_opt (fun ((l : Pxui_shell.Layout.leaf), _) -> Some l.path = header_focus) roots with
           | Some (l, _) -> l.body | None -> graph_body in
         let rec cards (s : Flow_graph.Projection.scope) = List.concat_map (fun (n : Flow_graph.Projection.node) ->
           Option.to_list (Pxui_graph.Scope.Private.box_of scope_view n.path)
           @ (match n.zone with Some z -> cards z.scope | None -> [])) s.nodes in
         let avoid = match value.scope_key with
           | Some k when scope_active -> cards k.scope | _ -> [] in
         Pxui_shell.Status_bar.tips ui ~bounds ~avoid
           [ text, (match kind with Info -> `Info | Refusal -> `Refusal) ]
     | _ -> ());
    let pane_roots = List.map (fun ((l : Pxui_shell.Layout.leaf), box) -> (l.panel, Some l.path), box) roots
      @ Option.to_list (Option.map (fun (panel, box) -> (panel, None), box) timeline_root) in
    let focus, focus_path = List.fold_left (fun (latest, focus) (target, box) ->
      match (Pxui.Ui.signal ui box).subtree_press with
      | Some index when index >= latest -> index, target
      | _ -> latest, focus) (-1, (focus, focus_path)) pane_roots
      |> snd in
    (* a focus whose leaf an edit split, retyped or moved follows it, so the next panel key acts *)
    let focus, focus_path = match focus_path with
      | None -> focus, focus_path
      | Some _ -> (match focused_leaf (geometry value workspace frame) focus focus_path with
          | Some leaf -> leaf.panel, Some leaf.path
          | None -> focus, focus_path) in
    let pane_keys = List.map (fun (target, box) -> Pxui.Ui.key box, target) pane_roots in
    (* The gutters last, on top of every pane's hit area: a drag applies from the next frame. *)
    let grips = Pxui_shell.Chrome.splitters ~state:(panel_state value) ~hidden:workspace.hidden (shell_tree value workspace) ui
           shortcut_frame in
    let drops = Pxui_shell.Chrome.drop_targets ui ~geometry:g ~state:(panel_state value)
      ~dragging:(List.find_map (function Pxui_shell.Chrome.Dragging (path, released) -> Some (path, released) | _ -> None) intents) in
    let timeline_menus = List.concat_map (fun ((leaf : Pxui_shell.Layout.leaf), box) ->
      if leaf.panel <> Timeline || leaf.body = (0, 0, 0, 0) then [] else
      Pxui.Ui.within ui box (fun () ->
        Pxui_shell.Chrome.panel_menu ~state:(panel_state value)
          ~key_of:(fun id -> match List.find_opt (fun (c : Leader.command) -> c.id = id) keymap with
            | Some {trigger = Some trigger; _} -> Editor_core.Keymap.label trigger | _ -> "")
          ui leaf box ~at:(Pxui.Ui.context_at ui box))) (of_kind Timeline) in
    let workspace, drag_changes = layout_intents vw workspace (grips @ drops @ timeline_menus @ strip_layout) in
    let workspace = match workspace.window_live with
      | None -> workspace
      | Some _ when List.exists (function Pxui_shell.Chrome.Window_drag _ -> true | _ -> false)
          (intents @ grips) -> workspace
      | Some _ -> { workspace with window_live = None } in
    (* a gutter drag that ended without its release (the window lost the focus, the pointer was
       cancelled) leaves no size behind for the next click on a gutter to commit *)
    let workspace = match workspace.live with
      | Some _ when not (List.exists (function Pxui_shell.Chrome.Resize _ | Settled -> true | _ -> false) grips)
          && (not (Frame.mouse_down Input.LeftButton frame) || Frame.has_event (function
               | Input.Focus_lost | Input.Pointer_cancelled Input.LeftButton -> true | _ -> false) frame) ->
          { workspace with live = None }
      | _ -> workspace in
    let changes = changes @ drag_changes in
    { workspace; focus; focus_path; pane_keys; outline; outline_intents; selection; menu; menu_pick; scope_view; scope_changes; tree; document = (network value).graph;
      edit_error = value.edit_error;
      effects = Param.no_effects;
      timeline_intents; frame_request = initial_frame_request; prompt = None; prompt_intent = None;
      panel; grab; settings = unchanged;
      opened = None; live_cook; label = "Edit";
      changes; tree_intents = list_intents; text_intents; open_graph;
      settings_changes; graph_panes; locals; other_texts;
      handle_changes; bar_action; view_pick; drops = inspector_drops @ viewport_drops } in
  let leader_panel = match leader with
    | Leader.Pending prefix -> Some (fun ui ->
        Pxui_shell.Which_key.panel ui ~category:Leader.group ~describe:Leader.describe_prefix ~order:Leader.order keymap ~prefix
          ~focus:(Leader.scope focus) ~focus_name:(Leader.pane_name focus))
    | Idle -> None in
  (* Presets: / s names and saves the document, / b browses, loads
     (Enter), and deletes (Delete twice). A load replaces the document below
     as one undo entry. *)
  let initial_prompt = List.fold_left (fun prompt -> function
    | Leader.Save_preset -> Some (Saving (Preset.default_name ()))
    | Browse_presets ->
        browse value ""
    | Guide_keys -> Some Keys
    | Command_palette -> Some (Palette "")
    | Jump -> Some (Jumping "")
    | Goto_symbol when text_shown <> None -> Some (Symbols "")
    | Goto_line when text_shown <> None -> Some (Going "")
    | Panel_pick -> Some (Picking { split = true; at = frame.input.pointer })
    | Window_pick -> Some (Picking { split = false; at = frame.input.pointer })
    | _ -> prompt) value.prompt actions in
  let prompt_panel ui prompt =
    let module Ui = Pxui.Ui in
    (* a search prompt over [all] rows of (name, detail, payload): fuzzy-filtered, a pick is
       [pick payload], any other query change keeps the prompt open as [again query] *)
    let search_prompt ~key ~title ~label ~query all ~again ~pick =
      let matches query = List.filter (fun (name, _, _) -> Ui.fuzzy_match ~query name) all in
      let rows query = Array.of_list (List.map (fun (name, detail, _) -> name, detail) (matches query)) in
      match Pxui_shell.Prompt.search ui ~key ~title ~label ~query ~rows with
      | None | Some (_, `Cancel) -> None, None
      | Some (query, `Pick index) -> let _, _, payload = List.nth (matches query) index in None, Some (pick payload)
      | Some (query, _) -> Some (again query), None in
    let next = match prompt with
    | None -> None, None
        | Some Keys ->
        let commands = List.filter (fun (command : Leader.command) -> match command.action with
          | List_command _ | Frame_tile -> false | _ -> true) Leader.keymap in
        let context = String.lowercase_ascii (Leader.pane_name focus) ^ " focused" in
        (if Pxui_shell.Which_key.sheet ui ~context ~category:Leader.sheet_group
            ~text_keys:Leader.text_keys commands then Some Keys else None), None
    | Some (Saving name) ->
        (match Pxui_shell.Prompt.name ui ~key:"preset-save"
            ~title:"Save preset" ~description:"Name for the current state of the document"
            ~label:"Preset name" ~query:name with
         | None | Some (_, `Cancel) -> None, None
         | Some (name, `Submit) -> None, Some (Save_preset_file name)
         | Some (name, _) -> Some (Saving name), None)
    | Some (Making_macro m) ->
        (match Pxui_shell.Prompt.macro ui ~key:"make-macro" ~title:"Make a macro from the selection"
            ~literals:(Array.of_list (List.map (fun (_, e) -> Flow.Lisp.flat e) m.draft.literals))
            ~free:m.draft.free m.state with
         | None -> None, None
         | Some (state, `Submit) ->
             None, Some (Edit_source (Flow_graph.Flow_edit.macro_op m.draft ~nodes:m.nodes
               ~name:state.name state.holes))
         | Some (state, `None) -> Some (Making_macro { m with state }), None)
    | Some (Browsing { query; presets; last_state }) ->
        let entries query = (Option.fold ~none:[]
          ~some:(fun time -> ["Last edited state", time, true]) last_state
          @ List.map (fun (name, time) -> name, time, false) presets)
          |> List.filter (fun (name, _, _) -> Ui.fuzzy_match ~query name) |> Array.of_list in
        let rows query = entries query |> Array.map (fun (name, time, recovery) ->
            let tm = Unix.localtime time in
            name, Printf.sprintf "%s%02d-%02d %02d:%02d" (if recovery then "Autosave · " else "")
              (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min) in
        (match Pxui_shell.Prompt.search ui ~key:"preset-browse"
            ~title:(Printf.sprintf "Presets · %d" (Array.length (entries "")))
            ~label:"Search presets" ~query ~rows with
         | None | Some (_, `Cancel) -> None, None
         | Some (query, `Pick index) ->
             let name, _, recovery = (entries query).(index) in
             None, Some (if recovery then Load_last_state else Load_preset_file name)
         | Some (query, `Delete index) ->
             let name, _, recovery = (entries query).(index) in
             Some (Browsing { query; presets; last_state }),
             Some (if recovery then Delete_last_state query else Delete_preset_file { name; query })
         | Some (query, _) -> Some (Browsing { query; presets; last_state }), None)
    | Some (Jumping query) ->
        (* every graph, grouped like the outline *)
        let graphs = List.map (fun (name, group) -> name, group, Go name)
          (Navigator.jump_rows (fst value.doc.Document.workspace).checked) in
        (* and every node of the graph the pane shows: picking one selects it and frames it *)
        let rec nodes (s : Flow_graph.Projection.scope) = List.concat_map (fun (n : Flow_graph.Projection.node) ->
          (if n.synthetic then [] else [ Flow_graph.Projection.title n, "node · " ^ n.head, Find_node n.path ])
          @ (match n.zone with Some z -> nodes z.scope | None -> [])) s.nodes in
        let all = graphs @ (match value.scope_key with Some k -> nodes k.scope | None -> []) in
        search_prompt ~key:"graph-jump" ~title:"Jump to graph or node" ~label:"Search graphs and nodes"
          ~query all ~again:(fun q -> Jumping q) ~pick:Fun.id
    | Some (Symbols query) ->
        (* every graph, function, macro and [let*] name of the text, by position *)
        let current = Option.map (fun shown -> let source = Text_pane.current_text text shown in source, shown) text_shown in
        let all = match current with
          | Some (source, _) ->
              List.map (fun (name, kind, byte) ->
                name, Printf.sprintf "%s · line %d" kind (Diagnostic.position_of_offset source byte).line, byte)
                (Text_pane.symbols source)
          | None -> [] in
        search_prompt ~key:"symbol-jump" ~title:"Go to symbol" ~label:"Search bindings" ~query all
          ~again:(fun q -> Symbols q)
          ~pick:(fun byte -> Run_action (Text_command (Text_pane.Select (byte, byte))))
    | Some (Referencing (name, query)) ->
        let all = match text_shown with
          | Some shown -> List.map (fun (byte, line) ->
              line, Printf.sprintf "line %d" (Diagnostic.position_of_offset (Text_pane.current_text text shown) byte).line, byte)
              (Text_pane.references (Text_pane.current_text text shown) name)
          | None -> [] in
        search_prompt ~key:"references" ~title:("References to " ^ name) ~label:"Search uses" ~query all
          ~again:(fun q -> Referencing (name, q))
          ~pick:(fun byte -> Run_action (Text_command (Text_pane.Select (byte, byte))))
    | Some (Going query) ->
        (match Pxui_shell.Prompt.name ui ~key:"goto-line" ~title:"Go to line"
            ~description:"A line, or line:column" ~label:"Line" ~query with
         | None | Some (_, `Cancel) -> None, None
         | Some (query, `Submit) ->
             (match Option.bind text_shown (fun shown ->
                 Text_pane.line_target (Text_pane.current_text text shown) query) with
              | Some byte -> None, Some (Run_action (Text_command (Text_pane.Select (byte, byte))))
              | None -> Some (Going query), None)
         | Some (query, _) -> Some (Going query), None)
    | Some (Picking { split; at } as open_prompt) ->
        let kinds = Pxui_shell.Layout.[ Graph; List; Lisp; Inspector; Outline; Timeline; View ""; Canvas "" ] in
        let digit = List.find_map (function
          | Input.Key_pressed (Input.KeyChar c) when c >= '1' && c < Char.chr (Char.code '1' + List.length kinds) ->
              Some (Char.code c - Char.code '1')
          | _ -> None) frame.input.events in
        let pick i =
          let panel = List.nth kinds i in
          if split then Some (Splitting { panel; axis = `H }), None
          else None, Some (Run_action (Window_new panel)) in
        (match digit with
         | Some i -> pick i
         | None ->
             (match Ui.context_menu ui ~at ~width:200. ~keys:(List.mapi (fun i _ -> string_of_int (i + 1)) kinds)
                      (if split then "split-kind" else "window-kind")
                      (List.map (fun p -> Editor_core.Panels.name p, true) kinds) with
              | `Open -> Some open_prompt, None
              | `Dismiss -> None, None
              | `Pick i -> pick i))
    | Some (Splitting { panel; axis }) ->
        (* the split follows the pointer over the panel under it; Tab turns it, a click places it *)
        let pressed key = List.exists (function Input.Key_pressed k -> k = key | _ -> false) frame.input.events in
        let axis = if pressed Input.Tab then (if axis = `H then `V else `H) else axis in
        let mx, my = frame.input.pointer in
        let leaf = List.find_opt (fun (l : Pxui_shell.Layout.leaf) ->
          let x, y, w, h = l.frame in
          not l.floating && mx >= float x && mx < float (x + w) && my >= float y && my < float (y + h))
          (geometry value workspace frame).leaves in
        let ratio = match leaf with
          | Some l ->
              let x, y, w, h = l.frame in
              let r = if axis = `H then (mx -. float x) /. float (max 1 w) else (my -. float y) /. float (max 1 h) in
              Float.max 0.1 (Float.min 0.9 r)
          | None -> 0.5 in
        let box = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px (float (fst frame.input.size))) ~h:(Ui.Px (float (snd frame.input.size)))
          ~at:(0., 0.) "split-pending" in
        Ui.to_front ui ~order:900 box;
        Ui.draw_over ui box (fun paint _ ->
          let theme = Ui.theme ui in
          Option.iter (fun (l : Pxui_shell.Layout.leaf) ->
            let x, y, w, h = l.frame in
            let x, y, w, h = float x, float y, float w, float h in
            let cut = if axis = `H then x +. (w *. ratio) else y +. (h *. ratio) in
            let nx, ny, nw, nh = if axis = `H then cut, y, x +. w -. cut, h else x, cut, w, y +. h -. cut in
            (* the panel being split is tinted, the room of the new one more *)
            let tint alpha = Rays_math.Color.with_alpha theme.accent alpha in
            Ui.Paint.fill paint ~x ~y ~w ~h (tint 22);
            Ui.Paint.fill paint ~x:nx ~y:ny ~w:nw ~h:nh (tint 56);
            (* the new panel's room: accent brackets 6 inside its corners; the panel kept stays as it is *)
            Ui.Paint.brackets paint ~x:(nx +. 6.) ~y:(ny +. 6.) ~w:(nw -. 12.) ~h:(nh -. 12.) ~offset:0. ~length:12.
              ~width:1. theme.accent;
            (* the cut: a hairline through the panel with a short accent cap at each end *)
            let a, b, (ux, uy) = if axis = `H then (cut, y), (cut, y +. h), (0., 1.) else (x, cut), (x +. w, cut), (1., 0.) in
            Ui.Paint.line paint ~from_:a ~to_:b ~width:1. theme.accent;
            (* a short accent cap across the cut at each end *)
            List.iter (fun (px, py, s) ->
              let px = px +. s *. ux and py = py +. s *. uy in
              Ui.Paint.line paint ~from_:(px -. 5. *. uy, py -. 5. *. ux) ~to_:(px +. 5. *. uy, py +. 5. *. ux)
                ~width:3. theme.accent) [ fst a, snd a, 1.; fst b, snd b, -1. ];
            (* what is being made, in the new panel's room, and the keys that steer it *)
            let name = String.uppercase_ascii (Editor_core.Panels.name panel) in
            let cx = nx +. (nw /. 2.) and cy = ny +. (nh /. 2.) in
            if nw > 90. && nh > 40. then begin
              Ui.Paint.cap paint ~at:(cx -. (Ui.Paint.cap_width paint name /. 2.), cy -. 14.) ~color:theme.accent name;
              let hint = "TAB  TURN   ESC  CANCEL" in
              Ui.Paint.cap paint ~at:(cx -. (Ui.Paint.cap_width paint hint /. 2.), cy +. 4.)
                ~color:(Pxui.Theme.ink_3 theme) hint
            end;
            Ui.Paint.ratio paint ~at:(mx +. 12., my +. 12.) (int_of_float (ratio *. 100.))
              (100 - int_of_float (ratio *. 100.))) leaf);
        let signal = Ui.signal ui box in
        let cancel = pressed Input.Escape
          || List.exists (function Input.Pointer_pressed (Input.RightButton, _) -> true | _ -> false) frame.input.events in
        if cancel then None, None
        else (match signal.clicked, leaf with
          | true, Some l -> None, Some (Run_action (Panel_split_at (l.path, axis, ratio, panel)))
          | _ -> Some (Splitting { panel; axis }), None)
    | Some (Palette query) ->
        (* Every keymap command once per id (undo has several chords). *)
        let commands = List.fold_left (fun seen (command : Leader.command) ->
            if command.scope <> None && command.scope <> Some (Leader.scope focus) then seen else
            if List.exists (fun (c : Leader.command) -> c.id = command.id) seen then seen
            else command :: seen) [] keymap |> List.rev in
        search_prompt ~key:"command-palette" ~title:"Commands" ~label:"Search commands" ~query
          (List.map (fun (c : Leader.command) -> c.label, "", c.action) commands)
          ~again:(fun q -> Palette q) ~pick:(fun action -> Run_action action) in
    (* A closed prompt must not keep keyboard focus into the next frame. *)
    if fst next = None && prompt <> None then Ui.dismiss_popup ui;
    next in
  let result = match Pxui.Ui.frame value.ui frame (fun ui ->
      Option.iter (fun draw -> draw ui) leader_panel;
      if all_ui_visible then Some (
        let prompt, prompt_intent = prompt_panel ui initial_prompt in
        let result = build ui in
        let prompt_intent = match prompt_intent, result.bar_action with
          | None, Some action -> Some (Run_action action) | intent, _ -> intent in
        { result with prompt; prompt_intent }) else None) with
    | Some result -> result
    | None ->
      { workspace; focus; focus_path; pane_keys = []; graph_panes = value.graph_panes; locals = value.locals; other_texts = []; outline = value.outline; outline_intents = []; selection; menu; menu_pick = None; scope_view; scope_changes = scope_command_changes; tree; document = (network value).graph;
        edit_error = value.edit_error;
        effects = Param.no_effects; timeline_intents = [];
        frame_request = initial_frame_request; prompt = initial_prompt;
        prompt_intent = None; panel = None; grab = false;
        settings = value.doc.settings;
        opened = None; live_cook = value.live_cook; label = "Edit";
        changes = command_changes; tree_intents = list_intents;
        text_intents = []; open_graph;
        settings_changes = []; handle_changes = None; bar_action = None; view_pick = None; drops = [] } in
  reduce ?gpu ?pump ~carry_changed ~all_ui_visible ~view_state ~carrying ~held_keys ~leader ~frame ~hud ~actions ~guide ~copied ~steady ~status_fps ~status_fps_at ~timeline ~timeline_changes ~graph_shown ~text ~text_shown ~row_sets ~rows value result
let update ?gpu ?(image=fun _->None) ?(host_events=[]) ?pump value ~all_ui_visible ~text_focus ~camera_panel ~view_handles ~render_status
    ~error_status ~view_state (frame : Frame.t) =
  let update, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
  let value, frame, carry_changed = carry_step value ~text_focus frame in
  update_frame ?gpu ?pump ~image ~host_events ~carry_changed value ~all_ui_visible ~text_focus ~camera_panel ~view_handles
    ~render_status ~error_status ~view_state frame) in
  if update.core.doc == value.doc then update
  else { update with core = { update.core with edit_phases = phases } }
