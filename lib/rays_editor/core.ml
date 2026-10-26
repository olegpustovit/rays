open Rays
open Sop
open Editor_document
include Core_host

(* a viewer or a canvas: the panels a sketch's picture is drawn in *)
let is_picture : Pxui_shell.Layout.panel -> bool = function
  | View _ | Canvas _ -> true
  | _ -> false

let update_frame ~image ~host_events ~carry_changed value ~all_ui_visible ~text_focus ~camera_panel ~view_handles
    ~render_status ~error_status ~view_state (frame : Frame.t) =
  let carrying = value.carry <> None in
  let window_focus = List.fold_left (fun f -> function
    | Event.WindowFocusLost -> -1 | Event.WindowFocusGained -> 0 | _ -> f) value.window_focus frame.events in
  let window_focus = if window_focus >= 0 then min 2 (window_focus + 1) else -1 in
  let live_frame, sketch_blocked =
    let pane_is_picture = match Pxui.Ui.root_at value.ui frame.mouse (List.map fst value.pane_keys) with
      | None -> true
      | Some key -> (match List.assoc_opt key value.pane_keys with
          | Some (panel, _) -> is_picture panel
          | None -> false) in
    Gate.gate { prompt = value.prompt <> None; menu = value.menu <> None; leader = value.leader <> Leader.Idle;
      text_focus;
      focus_on_picture = is_picture value.focus;
      pointer_on_picture = pane_is_picture;
      window_key = window_focus >= 2;
      pressed = List.exists (function Event.MousePressed _ -> true | _ -> false) frame.events;
      previous = value.live_frame; blocked = value.sketch_blocked }
      (Live_frame.of_frame frame) in
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
  let held_keys = frame.keys in
  let leader, commands, frame = Editor_core.Router.step ~previous_keys:value.held_keys
      keymap ~focus:(Leader.scope focus) ~text_focus ~frame value.leader in
  let hud = match List.rev commands with
    | command :: _ -> Some (Editor_core.Keymap.label (Option.get command.trigger)
        ^ " · " ^ command.label, frame.time +. 1.5)
    | [] -> (match leader with
        | Leader.Pending prefix when leader <> value.leader ->
            Some ((if prefix = "" then "Space" else "/ " ^ prefix) ^ " · leader", frame.time +. 1.5)
        | _ -> value.hud) in
  let actions = List.map (fun (command : Leader.command) -> command.action) commands in
  let hud = match hud with Some (_, until) when frame.time >= until -> None | _ -> hud in
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
  let steady = if frame.dt > 0. && frame.dt = value.last_dt then value.steady + 1 else 0 in
  let sample_fps = frame.time < value.status_fps_at
    || frame.time -. value.status_fps_at >= 1. in
  let status_fps, status_fps_at = if not sample_fps then
      value.status_fps, value.status_fps_at
    else (if frame.fps > 0. && Float.is_finite frame.fps
      then Some (int_of_float (Float.round frame.fps)) else None), frame.time in
  let shortcut_frame = if text_focus then
      { frame with Frame.events = List.filter (function
        | Event.KeyPressed _ | Event.KeyReleased _ | Event.TextInput _
        | Event.TextEditing _ -> false | _ -> true) frame.events }
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
      let mx, my = frame.mouse in
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
    let tool_action = match graph_host with
     | Some leaf when projection vw = Graph_view && has_panel vw Pxui_shell.Layout.Graph ->
         (match Bars.graph_tools ~outline:(not (has_panel vw Pxui_shell.Layout.Outline)) ui ~header:leaf.header ~from:(tools_from leaf)
                  (* the wrapping tools act on the selection: without one they are dimmed, never silent *)
                  ~enabled:(fun tool -> tool = Bars.Outline || scope_name vw <> None && (tool = Bars.Add || Pxui_graph.Scope.selected vw.scope_view <> [])) with
          | Some Add -> Some Leader.Add_node
          | Some Repeat -> Some (Leader.Scope_command Pxui_graph.Scope.Wrap_repeat)
          | Some Iterate -> Some (Leader.Scope_command Pxui_graph.Scope.Wrap_iterate)
          | Some Fn -> Some (Leader.Scope_command Pxui_graph.Scope.Make_fn)
          | Some Macro -> Some (Leader.Scope_command Pxui_graph.Scope.Make_macro)
          | Some Defn -> Some (Leader.Scope_command Pxui_graph.Scope.Make_defn)
          | Some Outline -> Some (Leader.Panel_split_at (leaf.path, `H, 0.7, Pxui_shell.Layout.Outline))
          | None -> None)
     | _ -> None in
    let view_pick = match graph_host with
     | Some leaf when has_panel vw Pxui_shell.Layout.Graph ->
         let _, hy, _, hh = leaf.header in
         let active = match projection vw with Graph_view -> 0 | List_view -> 1 | Text_view -> 2 in
         (* 12 apart, 8 and the 20-point collapse button from the edge (a window: dock and close) *)
         (match Option.bind (List.nth_opt (header_slots leaf) 0) (fun slot -> Option.bind slot (fun right ->
            fst (Pxui_shell.Kit.segments ui ~key:"workspace-view" ~right
                   ~y:(float hy +. float (hh - 20) /. 2.) [ "Graph"; "List"; "Text" ] active))) with
          | Some i -> Some (List.nth [ Graph_view; List_view; Text_view ] i)
          | None -> None)
     | _ -> None in
    (* the view's header tools; the last one clicked is the command *)
    let view_action = match active_view vw g, value.view_tools with
     | Some ({ header = (hx, hy, hw, hh); _ } as leaf), Some (looks, mode) when hh > 0 ->
         let from = float hx +. tools_from leaf in
         let modes = [ "Solid"; "Wire"; "Traced" ] in
         let seg_w = List.fold_left (fun w label -> w +. Pxui.Ui.text_width ui label) 24. modes in
         let look_w = Pxui_shell.Kit.button_width ui ~hint:"C" "Look through" in
         let ty = float hy +. float (hh - 20) /. 2. in
         let frame_w = Pxui_shell.Kit.button_width ui ~hint:"F" "Frame" in
         if from +. seg_w +. 8. +. look_w +. 8. +. frame_w < float (hx + hw - 36) then begin
           Bars.rule ui ~key:"workspace-view-rule" ~header:leaf.header ~from:(from -. float hx);
           let render = match fst (Pxui_shell.Kit.segments ui ~key:"workspace-render" ~right:(from +. seg_w) ~y:ty modes mode) with
            | Some i when i <> mode -> Some (Leader.Render_mode i)
            | _ -> None in
           let look = Pxui_shell.Kit.button ui ~key:"workspace-look" ~at:(from +. seg_w +. 8., ty) ~w:look_w
                ~active:looks ~hint:"C" "Look through" in
           (* the key's own command (view.frame-camera): the camera onto the displayed node *)
           let framed = Pxui_shell.Kit.button ui ~key:"workspace-frame" ~at:(from +. seg_w +. 8. +. look_w +. 8., ty) ~w:frame_w
                ~hint:"F" "Frame" in
           if framed then Some Leader.Frame_camera else if look then Some Leader.Look_through else render
         end else None
     | _ -> None in
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
    (* What is painted over a view stays in its pane: the box the marks are drawn in fills a
       clipping one ([Ui.clip] binds a box's children, not its own paint), so a bracket, a label
       or a frame that runs past the pane's edge is cut there and never lies on its neighbour. *)
    let view_layer ui (bx, by, bw, bh) key =
      let pane = Pxui.Ui.box ui ~flags:Pxui.Ui.clip ~w:(Pxui.Ui.Px (float bw)) ~h:(Pxui.Ui.Px (float bh))
          ~at:(float bx, float by) key in
      Pxui.Ui.within ui pane (fun () -> Pxui.Ui.box ui ~w:Pxui.Ui.Grow ~h:Pxui.Ui.Grow "marks") in
    (* the iteration the selected node shows, over the viewport's lower left corner *)
    (match probe_caption vw, active with
     | Some text, Some (leaf : Pxui_shell.Layout.leaf) ->
         (* a label on the ground, like every readout over the view *)
         Pxui.Ui.draw ui (view_layer ui leaf.body "viewport-caption") (fun paint (x, y, _, h) ->
           let px = x +. 12. and py = y +. h -. 24. in
           let theme = Pxui.Ui.theme ui in
           Pxui.Ui.Paint.fill paint ~x:(px -. 4.) ~y:py ~w:(Pxui.Ui.Paint.cap_width paint text +. 8.) ~h:16. theme.panel;
           Pxui.Ui.Paint.cap paint ~at:(px, py +. 2.) text)
     | _ -> ());
    (* over each view, in the kit's marks (viewport.html): the render frame and its label, the
       traced readout at the upper left, the render camera at the upper right, the selected
       object's brackets and name, and the scene's size and the frame rate at the lower right;
       every label is a row high and sits on a patch of the ground *)
    let module P = Pxui.Ui.Paint in
    let theme = Pxui.Ui.theme ui in
    let rh = float (Pxui.Ui.row_height ui) in
    let cap_y y = Pxui_shell.Kit.cap_y ui y rh in
    (* a label in the row at [y]; [pl] and [pr] points of ground beside it; its width *)
    let label paint ~x ~y ?(pl = 0.) ?(pr = 0.) ?color text =
      let w = P.cap_width paint text in
      P.fill paint ~x:(x -. pl) ~y ~w:(w +. pl +. pr) ~h:rh theme.panel;
      P.cap paint ~at:(x, cap_y y) ?color text; w in
    (* a viewport window labels its frame at the top left: the shading and the camera ("Wire · top") *)
    List.iter (fun ((leaf : Pxui_shell.Layout.leaf), _) -> match leaf.panel, value.view_tools with
      | View key, Some (_, mode) when leaf.floating && (let _, _, w, h = leaf.body in w > 120 && h > 60) ->
          let bx, by, _, _ = leaf.body in
          let camera = Option.bind value.doc.Document.active_camera (fun id ->
            Option.map Node.label (Edit_graph.find (scene value) ~node_id:id)) in
          let text = String.concat " \xc2\xb7 " ([ List.nth [ "Solid"; "Wire"; "Traced" ] (max 0 (min 2 mode)) ]
            @ Option.to_list camera) in
          let w = Pxui_shell.Kit.cap_width ui text +. 8. in
          let box = Pxui.Ui.box ui ~w:(Pxui.Ui.Px w) ~h:(Pxui.Ui.Px 16.)
              ~at:(float bx +. 8., float by +. 4.) ("viewport-window-label-" ^ key) in
          Pxui.Ui.to_front ui ~order:max_int box;
          Pxui.Ui.draw ui box (fun paint (x, y, _, h) ->
            Pxui.Ui.Paint.cap paint ~at:(x +. 4., Pxui_shell.Kit.cap_y ui y h) text)
      | _ -> ()) roots;
    List.iter (fun ((leaf : Pxui_shell.Layout.leaf), _) -> match leaf.panel with
      | View key when (let _, _, w, h = leaf.body in w > 320 && h > 160) ->
          let _, _, bw, _ = leaf.body in
          let overlay = view_layer ui leaf.body ("viewport-marks-" ^ key) in
          let gate = List.assoc_opt key value.gates in
          let box = match value.selected_box with Some (k, rect, name) when k = key -> Some (rect, name) | _ -> None in
          let root : Objects.Root.parameters = Option.fold ~none:value.doc.Document.root
              ~some:(fun (r : Document.view_root) -> r.params) (List.assoc_opt key value.doc.Document.view_roots) in
          let camera = Option.bind value.doc.Document.active_camera (fun id -> Edit_graph.find (scene value) ~node_id:id) in
          (* the triangles the scene draws: a polygon of n corners fans into n - 2 (exact for polygons, a
             count of corners and faces, not a walk of every face, for a frame's cost) *)
          let tris = List.fold_left (fun n (piece : _ Cook.piece) ->
            match piece.output.Sop.Session.payload with
            | Payload.Image _ | Payload.Kernel _ -> n
            | Geometry g -> n + max 0 (Rdk.Geometry.vertex_count g - (2 * Rdk.Geometry.primitive_count g))) 0 (pieces value) in
          let objects = List.length (pieces value) in
          let points = Option.bind (piece value) (fun (piece : _ Cook.piece) ->
            Option.map Rdk.Geometry.point_count
              (Result.to_option (Payload.geometry piece.output.Sop.Session.payload))) in
          (* the traced readout: its samples are the caption's "n/cap spp" *)
          let traced = List.assoc_opt key value.traces in
          (* a body about as wide as the workspace sheet's follows that sheet: the narrow traced
             block, no camera readout, no object count; a wide one follows viewport.html *)
          let narrow = bw < 1000 in
          Pxui.Ui.draw ui overlay (fun paint (x, y, w, h) ->
            Option.iter (fun (gx, gy, gw, gh) ->
              let gx = float gx and gy = float gy and gw = float gw and gh = float gh in
              P.stroke paint ~x:(gx +. 0.5) ~y:(gy +. 0.5) ~w:(gw -. 1.) ~h:(gh -. 1.) (Pxui.Theme.edge theme);
              (* the corner marks lie on the frame's own edge: 16 points, 1 wide, in ink *)
              P.brackets paint ~x:gx ~y:gy ~w:gw ~h:gh ~offset:0. ~length:16. ~width:1. theme.foreground;
              (* the label stands over the frame's left end with no ground of its own; the traced
                 readout, drawn after it, covers its head where the sheet's does *)
              P.cap paint ~at:(gx, cap_y (if gy -. y >= 20. then gy -. 20. else gy +. 4.))
                (Printf.sprintf "Render frame \xc2\xb7 %d \xc3\x97 %d" root.width root.height)) gate;
            (* a traced viewport's readout: the path tracer and its backend as labels, the samples as
               a 20-point count over a 4-point bar, the film as a label; 4 apart, on the ground *)
            Option.iter (fun (t : trace) ->
              if w > 200. && h > 120. then begin
                (* the readout's ground stands 8 clear of its text and bar on both sides *)
                let fx = x +. (if narrow then 8. else 12.) and oy = y +. (if narrow then 8. else 12.) in
                let ox = fx +. 8. in
                let title = Pxui.Ui.font_size ui * Pxui.Theme.title_size / Pxui.Theme.font_size in
                let bar_w = if narrow then 112. else Float.min 224. (w -. 24.) in
                P.fill paint ~x:fx ~y:oy ~w:(bar_w +. 16.) ~h:((3. *. rh) +. 24.) theme.panel;
                let tw = label paint ~x:ox ~y:oy ~color:theme.foreground "Path traced" in
                if not narrow then
                  ignore (P.cap paint ~at:(ox +. tw +. 6., cap_y oy) ~color:(Pxui.Theme.ink_2 theme) "Metal RT");
                let ty = Pxui.Ui.text_top ui ~size:title in
                let row2 = oy +. rh +. 4. in
                let count = string_of_int (min t.samples t.cap) in
                P.text paint ~size:title ~at:(ox, ty row2 rh -. 1.) ~color:theme.foreground count;
                P.text paint ~size:title
                  ~at:(ox +. P.text_width paint ~size:title (count ^ " "), ty row2 rh -. 1.)
                  ~color:(Pxui.Theme.ink_2 theme)
                  (if narrow then Printf.sprintf "/ %d" t.cap else Printf.sprintf "/ %d spp" t.cap);
                (* the bar: a line-3 box 4 high, filled inside it with the share of the samples *)
                let by = row2 +. rh +. 4. in
                P.stroke paint ~x:(ox +. 0.5) ~y:(by +. 0.5) ~w:(bar_w -. 1.) ~h:3. (Pxui.Theme.border theme);
                P.fill paint ~x:(ox +. 1.) ~y:(by +. 1.) ~w:((bar_w -. 2.) *. Float.min 1. (float t.samples /. float (max 1 t.cap)))
                  ~h:2. theme.foreground;
                (* under it: the seconds the accumulation took and, on a wide body, the bounces and
                   the film in pixels *)
                let fw, fh = t.film in
                ignore (P.cap paint ~at:(ox, cap_y (by +. 8.))
                  (if narrow then Printf.sprintf "spp \xc2\xb7 %.1f s" t.seconds
                   else Printf.sprintf "%.1f s \xc2\xb7 %d bounce%s \xc2\xb7 %d \xc3\x97 %d" t.seconds t.bounces
                     (if t.bounces = 1 then "" else "s") fw fh))
              end) traced;
            Option.iter (fun ((sx, sy, sw, sh), name) ->
              let sx = float sx and sy = float sy and sw = float sw and sh = float sh in
              if sw > 8. && sh > 8. then begin
                P.brackets paint ~x:sx ~y:sy ~w:sw ~h:sh theme.accent;
                (* the name in the accent, in the row that ends where the brackets begin, then how
                   many points the object cooked to, on the ground *)
                let ny = sy -. 4. -. rh in
                P.cap paint ~at:(sx -. 4., cap_y ny) ~color:theme.accent name;
                Option.iter (fun n ->
                  ignore (label paint ~x:(sx -. 4. +. P.cap_width paint name +. 8.) ~y:ny ~pl:4. ~pr:4.
                    ~color:(Pxui.Theme.ink_2 theme) (group_digits n ^ " pts"))) points
              end) box;
            (* the render camera: its name, its lens and focus as a two-column grid whose labels
               and values both end at their column's right edge, 10 apart *)
            if not narrow then Option.iter (fun node ->
              let field name = List.find_map (fun (f : Parameter.field_view) ->
                match f.name = name, f.current with
                | true, Parameter.Float_value v -> Some v | _ -> None) (Node.parameter_fields node) in
              let rows = [ "Camera", Node.label node ]
                @ (match field "fov" with
                   | Some fov when fov > 0. && fov < 180. ->
                       (* the focal length that gives this vertical angle on a 24 mm gate *)
                       let mm = 12. /. Float.tan (fov *. Float.pi /. 360.) in
                       [ "Lens", Printf.sprintf "%.0f mm" mm
                         ^ (match field "aperture" with Some a when a > 0. -> Printf.sprintf " \xc2\xb7 r %g" a | _ -> "") ]
                   | _ -> [])
                @ (match field "focus_distance" with Some d when d > 0. -> [ "Focus", Printf.sprintf "%.2f" d ] | _ -> []) in
              let value_w = List.fold_left (fun m (_, v) -> Float.max m (P.text_width paint v)) 0. rows in
              let label_w = List.fold_left (fun m (l, _) -> Float.max m (P.cap_width paint l)) 0. rows in
              let right = x +. w -. 12. in
              P.fill paint ~x:(right -. value_w -. 10. -. label_w -. 8.) ~y:(y +. 12.)
                ~w:(value_w +. 10. +. label_w +. 8.) ~h:((rh *. float (List.length rows)) +. 4.) theme.panel;
              List.iteri (fun i (name, text) ->
                let ry = y +. 12. +. (rh *. float i) in
                P.cap paint ~at:(right -. value_w -. 10. -. P.cap_width paint name, cap_y ry) name;
                P.text paint ~at:(right -. P.text_width paint text, Pxui_shell.Kit.text_y ui ry rh)
                  ~color:theme.foreground text) rows) camera;
            (* the scene's size and the frame rate: labels 12 apart, the rate in ink, in the row
               that ends 12 (6 on a narrow body) above the pane's foot *)
            let edge = if narrow then 8. else 12. in
            let row_y = y +. h -. (if narrow then 6. else 12.) -. rh in
            let tris_text = Printf.sprintf "%s tris" (group_digits tris) in
            let items =
              (tris_text, None)
              :: (if narrow then [] else
                  [ Printf.sprintf "%d object%s" objects (if objects = 1 then "" else "s"), None ])
              @ (match value.status_fps with Some fps -> [ Printf.sprintf "%d fps" fps, Some theme.foreground ] | None -> []) in
            let total = List.fold_left (fun sum (text, _) -> sum +. P.cap_width paint text) (12. *. float (List.length items - 1)) items in
            (* the wide sheet's readout stands on a patch of the ground; the narrow one on the view *)
            if not narrow then P.fill paint ~x:(x +. w -. edge -. total -. 8.) ~y:row_y ~w:(total +. 8.) ~h:rh theme.panel;
            ignore (List.fold_left (fun at (text, color) ->
              P.cap paint ~at:(at, cap_y row_y) ?color text;
              at +. P.cap_width paint text +. 12.) (x +. w -. edge -. total) items))
      | _ -> ()) roots;
    let view_root = match active with
      | Some leaf -> snd (List.find (fun ((l : Pxui_shell.Layout.leaf), _) -> l == leaf) roots)
      | None -> none_root () in
    let graph_body = match graph_host with Some leaf -> leaf.body | None -> 0, 0, 0, 0 in
    let gx, gy, gw, gh = graph_body in
    let scope_view, scope_frame_changes =
      (* a pane that is not drawn gives up what it held: a field, the hints, a drag, the pointer *)
      if not scope_active then Pxui_graph.Scope.suspend scope_view, []
      else Pxui.Ui.within ui graph_root (fun () ->
        scope_view
        |> Pxui_graph.Scope.with_guide guide
        |> Pxui_graph.Scope.with_failed (failed_nodes value)
        |> Pxui_graph.Scope.with_theme (let theme = Pxui.Ui.theme ui in
             match graph_host with Some { floating = true; _ } -> sheet_theme theme | _ -> theme)
        |> Pxui_graph.Scope.with_bounds ~x:gx ~y:gy ~width:(max 1 gw) ~height:(max 1 gh)
        |> Pxui_graph.Scope.with_carry ~lit:carry_lit ~hot:carry_hot
        |> fun view -> Pxui_graph.Scope.update view ui shortcut_frame) in
    (* a right-click on the pane's empty canvas opens the add menu there *)
    let menu = match menu, List.find_map (function
        | Pxui_graph.Scope.Menu_requested (x, y) -> Some (int_of_float x, int_of_float y)
        | _ -> None) scope_frame_changes with
      | None, Some at -> open_menu ~scope_view value at
      | menu, _ -> menu in
    let menu, menu_pick = match menu with
      | Some menu -> Pxui.Ui.within ui graph_root (fun () ->
          Pxui_graph.Node_menu.update menu ui ~bounds:(0, 0, frame.width, (let _, sy, _, _ = (geometry value workspace frame).status_at in sy)))
      | None -> None, None in
    (* every list shows the level and selection of its graph panel, with its own folds, filter and
       scroll; the one in use takes the keys.  A list of another pane only shows: a press makes its
       pane the one in use before the frame builds *)
    let tree, list_intents, locals = List.fold_left (fun (tree, intents, locals) ((key, path, _, _) as host) ->
      match root_at path with
      | None -> tree, intents, locals
      | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
          let pane = pane_of host in
          let mine = in_use pane in
          let rows, columns = if mine then rows, columns else List.assoc key host_rows in
          let selected = if mine then selected () else Selection.selected_nodes pane.selection in
          let draw state = on_sheet ui leaf.floating (fun () -> Pxui.Ui.within ui root (fun () ->
            Pxui_shell.Tree.update state ui shortcut_frame ~bounds:leaf.body
              ~title:(level_name pane) ~columns rows ~selected)) in
          if Some key = value.list_at then
            let tree, emitted = draw tree in tree, (if mine then intents @ emitted else intents), locals
          else
            let state, emitted = draw (local_of value key).rows in
            tree, (if mine then intents @ emitted else intents), put_local key (fun l -> { l with rows = state }) locals)
      (tree, list_intents, value.locals) (hosted `List) in
    let names = lazy (completion_names value) in
    let view_text path state shown = match root_at path with
      | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
          Pxui.Ui.within ui root (fun () ->
            Text_pane.view ui ~bounds:leaf.body
              (* a graph panel in text view keeps its Graph / List / Text at the header's end *)
              ~tabs_right:(Option.join (List.nth_opt (List.rev (header_slots leaf)) 0))
              ~vocab:(Lazy.force value.lisp_vocab) ~names:(Lazy.force names) ~time:frame.time state shown)
      | None -> [] in
    let text_intents = match text_shown, text_host with
      | Some shown, Some (_, path, _, _) -> view_text path text shown
      | _ -> [] in
    (* every other text pane, with its own tab and drafts *)
    let locals, other_texts = List.fold_left (fun (locals, texts) (key, path, state, shown) ->
      put_local key (fun l -> { l with code = state }) locals, (key, shown, view_text path state shown) :: texts)
      (locals, []) other_texts in
    let outline, outline_intents, locals = List.fold_left (fun (outline, intents, locals) (key, path, _, _) ->
      match root_at path with
      | None -> outline, intents, locals
      | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
          let draw state = on_sheet ui leaf.floating (fun () -> Pxui.Ui.within ui root (fun () ->
            Navigator.view state ui ~bounds:leaf.body (navigator_params value))) in
          if Some key = value.outline_at then let outline, emitted = draw outline in outline, intents @ emitted, locals
          else
            let state, emitted = draw (local_of value key).nav in
            outline, intents @ emitted, put_local key (fun l -> { l with nav = state }) locals)
      (value.outline, [], locals) (hosted `Outline) in
    let locals = List.fold_left (fun locals ((leaf : Pxui_shell.Layout.leaf), root) ->
          let key = panel_key value.doc leaf.path in
          let pane = shown_as value (key, leaf.path, leaf.panel) in
          let owner = (local_of {value with locals} key).sheet_owner in
          let changed = Pxui.Ui.within ui root (fun () ->
            Spreadsheet.view ui ~bounds:leaf.body ~owner (spreadsheet_source pane)) in
          match changed with None -> locals
          | Some sheet_owner -> put_local key (fun local -> {local with sheet_owner}) locals)
      locals (of_kind Pxui_shell.Layout.Spreadsheet) in
    (* every other graph panel draws its own canvas; a press gives a panel the focus before the
       frame builds, so the pane a gesture starts in is the one in use *)
    let graph_panes = List.filter_map (fun (key, path, (pane : _ t)) ->
      match List.find_opt (fun ((l : Pxui_shell.Layout.leaf), _) -> l.path = path) roots with
      | Some (leaf, hosted) when (let _, _, _, h = leaf.body in h > 0) ->
          let x, y, width, height = leaf.body in
          let view = if stored_projection pane = Graph_view && graph_name pane <> None then
              Pxui.Ui.within ui hosted (fun () ->
                pane.scope_view
                |> Pxui_graph.Scope.with_guide false
                |> Pxui_graph.Scope.with_theme (let theme = Pxui.Ui.theme ui in
                     if leaf.floating then sheet_theme theme else theme)
                |> Pxui_graph.Scope.with_bounds ~x ~y ~width:(max 1 width) ~height:(max 1 height)
                |> fun view -> fst (Pxui_graph.Scope.update view ui shortcut_frame))
            else pane.scope_view in  (* its list or text view is drawn with the lists and text panes *)
          Some (key, { (stash pane) with view })
      | _ -> Some (key, stash pane)) other_panes in
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
      | Pxui_shell.Tree.Select [] -> Selection.clear selection
      | Select ids -> Selection.select_nodes ids selection
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
    (* every inspector shows the same selection in its own root (its scroll and open sections are
       its own); the edits of all are this frame's, the host's panel is the focused one's *)
    let inspect (value, scope_view, selection, scope_active) inspector_root bounds =
      let document = (network value).graph.geometry in
      let selected_ids = Selection.selected_nodes selection in
      let lowered_level = match value.level with
        | Document.Inside id -> kind value id = Some "geometry" | Scene -> false in
      (* a row of a geometry object's list selects its node in the pane: the inspector follows it *)
      let scope_selected = if (match value.scope_key with Some k -> Some k.graph = graph_name value | None -> false)
          || (lowered_level && value.scope_key <> None)
        then inspector_paths (Pxui_graph.Scope.selected scope_view) else [] in
      let open_network = network value in
      let window = List.exists (fun (l : Pxui_shell.Layout.leaf) -> l.floating && l.panel = Inspector && l.body = bounds) g.leaves in
      (* a window keeps its 1-point border on the left, right and bottom: its panel lies inside *)
      let bounds = if window then (let x, y, w, h = bounds in x, y, max 1 w, max 1 (h - 1)) else bounds in
      let inspector_panel ui bounds build = inspector_panel ~window ui bounds build in
      on_sheet ui window
      @@ fun () -> Pxui.Ui.within ui inspector_root (fun () -> match selected_ids with
      | _ when not inspector_visible ->
          None, [], [], value.live_cook, [], []
      | _ when scope_selected <> [] ->
          (match scope_selected with
            | [ path ] ->
                (* the inspector reports each choice control it builds: the material one is a place
                   a carried payload can be put, gathered here and returned with the rows' requests *)
                let drops = ref [] in
                let requests, moves = inspector_panel ui bounds (fun () ->
                  workspace_inspector ~image ~window value ui ~width:(float (let _, _, w, _ = bounds in max 1 w)) path ~resized
                    ~follows:(fun path -> follow_target ~path value)
                    ~on_choice:(fun name box -> if name = "@ref:material" then
                      Option.iter (fun d -> drops := (Carry.Node path, (match d with
                        | Pxui.Ui.Dropped _ -> true | Hover _ -> false)) :: !drops)
                        (Pxui.Ui.drop_target ui box))) in
                None, requests, [], value.live_cook, moves, List.rev !drops
            | paths -> inspector_panel ui bounds (fun () ->
                ignore (Pxui.Ui.inspector_header ui ~key:"multi-header"
                  ~title:(Printf.sprintf "%d nodes" (List.length paths)) ~detail:"Selected" ()));
                None, [], [], value.live_cook, [], [])
      | [] ->
          (* Sketch settings above the environment's camera/render panel. *)
          let changes, live, panel = inspector_panel ui bounds (fun () ->
            let node_count = List.length (Edit_graph.inspect document) in
            let display = Option.fold ~none:"none" ~some:(fun id ->
              Option.fold ~none:("#" ^ string_of_int id) ~some:Node.label
                (Edit_graph.find document ~node_id:id)) open_network.displayed in
            let title, detail = match value.scope_key with
              | Some k when scope_active && Some k.graph = graph_name value ->
                  let rec count (s : Flow_graph.Projection.scope) = List.fold_left
                    (fun n (x : Flow_graph.Projection.node) ->
                      n + 1 + Option.fold ~none:0 ~some:(fun (z : Flow_graph.Projection.zone) -> count z.scope) x.zone)
                    0 s.nodes in
                  let zones = List.length (Flow_graph.Projection.zones k.scope) in
                  k.graph, Printf.sprintf "%d node%s · %d loop%s · select a node to edit it" (count k.scope)
                    (if count k.scope = 1 then "" else "s") zones (if zones = 1 then "" else "s")
              | _ -> level_name value, Printf.sprintf "%d node%s · display %s" node_count (if node_count = 1 then "" else "s") display in
            ignore (Pxui.Ui.inspector_header ui ~key:"network-header" ~title ~detail ());
            Pxui.Ui.inspector_body ui (fun () ->
            (* the row sits under a section named for the graph, as every row of the sheet does *)
            let live = Option.value ~default:value.live_cook (Pxui.Ui.inspector_section ui
                ~key:"live-cook-section" ~expanded:true title (fun () ->
                  Pxui.Ui.inspector_toggle ui ~key:"live-cook" ~label:"Live update" value.live_cook)) in
            let changes = match Settings.fields unchanged |> List.filter (fun (field : Parameter.field_view) ->
                field.name <> "renderer") with
              | [] -> []
              | fields ->
                  Pxui.Ui.scope ui "sketch-settings" (fun () ->
                    let _, _, width, _ = bounds in
                    Option.value ~default:[] (Pxui.Ui.inspector_section ui
                      ~key:"sketch-settings-section" ~expanded:true "Settings"
                      (fun () -> Pxui_shell.Inspector.fields ui
                        ~width:(float width) fields))) in
            changes, live, camera_panel ())) in
          Some panel, [], changes, live, [], []
      | _ :: _ :: _ ->
          let () = inspector_panel ui bounds (fun () ->
            ignore (Pxui.Ui.inspector_header ui ~key:"multi-header"
              ~title:(Printf.sprintf "%d nodes" (List.length selected_ids))
              ~detail:"Selected" ())) in
          None, [], [], value.live_cook, [], []
      | [node_id] ->
          (* the nodes of a geometry object are the lowering of its graph: the graph pane's node
             is the one to edit (its arguments are the text) *)
          let derived = match value.level with
            | Document.Inside id -> kind value id = Some "geometry" | Scene -> false in
          let fields, label, kind = match Edit_graph.find document ~node_id with
            | Some node -> Node.parameter_fields node, Node.label node, Node.operation node
            | None -> [], "#" ^ string_of_int node_id, "unknown" in
          let preview = value.level = Document.Scene && Option.fold ~none:false
            ~some:(fun shell -> List.exists (fun (_, ids) -> List.mem node_id ids) shell.Document.views)
            value.doc.shell in
          let changes = inspector_panel ui bounds (fun () ->
            Pxui.Ui.scope ui (Printf.sprintf "node.%d" node_id) (fun () ->
              let _, _, inspector_width, _ = bounds in
              let inspector_width = float (max 1 inspector_width) in
              let input_count = Array.length (Option.value ~default:[||] (Edit_graph.inputs document ~node_id)) in
              (* the head: the kind as the sheet writes it (scene/camera), VIEW while displayed,
                 the node's number; the name edited in place; what is true of the node *)
              let facts = List.filter_map Fun.id [
                if Edit_graph.is_bypassed document ~node_id then Some "muted" else None;
                if value.doc.Document.active_camera = Some node_id then Some "active camera" else None;
                if input_count > 0 then Some (Printf.sprintf "%d input%s" input_count (if input_count = 1 then "" else "s"))
                else None;
                if preview then Some "preview instance" else None ] in
              let head = Pxui.Ui.inspector_header ui
                  ~key:"flow-inspector-header"
                  ~kind:(Printf.sprintf "%s/%s" (Flow.Context.name open_network.context) kind)
                  ?badge:(if open_network.displayed = Some node_id then Some "view" else None)
                  ~index:(Printf.sprintf "NO. %04d" node_id)
                  ?rename:(if derived || preview then None else Some (fun text -> String.trim text <> ""))
                  ~title:label
                  ~detail:(match facts with
                    | [] -> Printf.sprintf "%d parameter%s" (List.length fields) (if List.length fields = 1 then "" else "s")
                    | facts -> String.concat " · " facts) () in
              let renamed = head.renamed in
              let rename = if renamed = label || derived || preview then [] else
                [Rename {node = node_id; label = renamed}] in
              rename @ Pxui.Ui.inspector_body ui (fun () ->
              let names = Option.value ~default:[]
                (Edit_graph.node_slot_names document ~node_id) in
              let inputs = Option.value ~default:[||]
                (Edit_graph.inputs document ~node_id) in
              if Array.length inputs > 0 then
                ignore (Pxui.Ui.inspector_section ui
                  ~key:"flow-input-heading" ~expanded:true "Inputs"
                  (fun () -> Array.iteri (fun index input ->
                    let name = Option.value
                        ~default:("in" ^ string_of_int index)
                        (List.nth_opt names index) in
                    let source = match input with
                      | None -> "empty"
                      | Some id -> Option.fold
                          ~none:("#" ^ string_of_int id)
                          ~some:Node.label
                          (Edit_graph.find document ~node_id:id) in
                    Pxui.Ui.inspector_readout ui ~width:inspector_width
                      ~key:("flow-input-" ^ string_of_int index)
                      ~label:name ("← " ^ source)) inputs));
              if derived then Pxui.Ui.inspector_message ui ~key:"flow-derived"
                "Made from the graph's text. Select the node in the graph pane to edit its arguments.";
              if preview then Pxui.Ui.inspector_message ui ~key:"flow-preview"
                "Preview instance. Edit its viewport scene reference or source graph.";
              let edits = match object_rows ~locked:(derived || preview) fields with
                | Error diagnostic ->
                    Pxui.Ui.inspector_message ui ~key:"flow-diagnostic"
                      (Flow.Diagnostic.to_string diagnostic); []
                | Ok [] -> []
                | Ok rows ->
                    let expanded = fields |> List.filter_map (fun field ->
                      match field.Parameter.folder with [] -> None
                      | first :: _ -> Some first) |> List.sort_uniq String.compare in
                    Pxui_shell.Inspector.flow_fields ui ~expanded
                      ~kind_label:(kind_label kind) ~width:inspector_width rows in
              List.filter_map (function
                | Pxui_shell.Inspector.Edited (path, value) ->
                    Some (Set_parameter {node = node_id; path; value})
                | Pxui_shell.Inspector.Expression (path, text) ->
                    (* a row is a field of the node; its argument is the parameter holding it (a
                       vector's field is one component of it) *)
                    let parameters = Result.value ~default:[] (Flow_sop.Port.parameters (Editor_document.Contexts.group_triples fields)) in
                    let key_sub = List.find_map (fun (p : Flow_sop.Port.parameter) ->
                      match List.find_index (fun (f : Parameter.field_view) -> f.name = path) p.fields with
                      | Some i -> Some (p.path, if List.length p.fields = 3 then [ i ] else [])
                      | None ->
                          (* a vector's component row is named [row.axis] *)
                          Option.map (fun i -> p.path, [ i ])
                            (List.find_index (fun axis -> p.path ^ "." ^ axis = path) [ "x"; "y"; "z" ])
                          |> fun found -> if List.length p.fields = 3 then found else None) parameters in
                    (match expression_text text, key_sub with
                     | Ok expr, Some (key, sub) -> Some (Object_arg { node = node_id; key; sub; expr })
                     | Error message, _ -> Some (Declined message)
                     | Ok _, None -> None)
                | Follow path ->
                    let home = match value.level with
                      | Document.Scene -> List.assoc_opt node_id value.doc.homes.objects
                      | Inside _ -> List.assoc_opt node_id value.doc.homes.layers in
                    Option.bind home (fun (home : Document.home) -> match home with
                      | Bound_at from ->
                          Option.bind (Flow_graph.Flow_edit.arg_text ws.source from (Kw path)) (fun expr ->
                            match expr.Flow.Syntax.node with
                            | Sym name -> Option.map (fun path -> Follow_source path)
                                (Text_pane.reference_target ws.source ~from name)
                            | _ -> None)
                      | _ -> None)
                | Pinned _ | Reset _ -> None) edits))) in
          None, changes, [], value.live_cook, [], []) in
    (* an inspector shows the pane in use, or the graph panel its [:of] names; one of another pane
       only shows (a press there makes that pane the one in use first) *)
    let inspectors = List.map (fun ((leaf : Pxui_shell.Layout.leaf), root) ->
      let pane = pane_of (panel_key value.doc leaf.path, leaf.path, leaf.panel, `List) in
      if in_use pane then leaf, inspect (value, scope_view, selection, scope_active) root leaf.body
      else
        let shown = graph_shown && stored_projection pane = Graph_view && graph_name pane <> None in
        let panel, _, _, _, moves, drops = inspect (pane, pane.scope_view, pane.selection, shown) root leaf.body in
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
    let timeline_intents = List.concat_map (fun (((_, _, _, height) as bounds), box, edge) ->
      if height <= 0 then [] else
      Pxui.Ui.within ui box (fun () ->
        Pxui_shell.Timeline_bar.draw ui ~bounds ~edge
          ~playing:(Timeline.mode timeline = Timeline.Playing)
          ~frame:(Timeline.frame timeline)
          ~time:(Timeline.time timeline)
          ~max_frame:value.timeline_frames ())) timeline_slots in
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
      | Some id when value.level = Document.Scene -> Selection.select id selection
      | Some _ | None -> selection in
    let handle_ops, handle_changes = match scope_target, handle_target, handle_edits with
      | Some (path, node), _, _ :: _ ->
          let parameters = Result.value ~default:[] (Flow_sop.Port.parameters (Node.parameter_fields node)) in
          List.filter_map (fun (p : Flow_sop.Port.parameter) ->
            if List.exists (fun (f : Parameter.field_view) -> List.mem_assoc f.name handle_edits) p.fields
            then Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = path;
              key = Flow_graph.Flow_edit.Kw p.path; sub = [];
              value = Editor_document.Scene_sync.value_syntax (List.map (fun (f : Parameter.field_view) ->
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
     | Some (kind, text) when frame.time -. value.notice_at < 3. ->
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
    let strip_playback, strip_layout = match timeline_root with
      | Some (_, box) when (let _, _, _, h = g.timeline_at in h > 0) ->
          Pxui.Ui.within ui box (fun () ->
            let module Ui = Pxui.Ui in
            Option.iter (fun (x, y) -> Ui.set_text_state ui box (Some (Printf.sprintf "%g %g" x y)))
              (Ui.context_at ui box);
            match Option.map (String.split_on_char ' ') (Ui.text_state ui box) with
            | Some [x; y] ->
                (match float_of_string_opt x, float_of_string_opt y with
                 | Some x, Some y ->
                     let playing = Timeline.mode timeline = Timeline.Playing in
                     (match Ui.context_menu ui ~at:(x, y) "timeline-context"
                       [(if playing then "Pause" else "Play"), true; "Stop", true; "Hide timeline", true] with
                      | `Open -> [], []
                      | result ->
                          Ui.set_text_state ui box None;
                          (match result with `Pick 0 -> [Pause_toggle], [] | `Pick 1 -> [Stop_playback], []
                           | `Pick 2 -> [], [Pxui_shell.Chrome.Toggle [-1]] | _ -> [], []))
                 | _ -> [], [])
            | _ -> [], [])
      | _ -> [], [] in
    let timeline_intents = timeline_intents @ strip_playback in
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
               | Event.WindowFocusLost | Event.PointerCancelled Input.LeftButton -> true | _ -> false) frame) ->
          { workspace with live = None }
      | _ -> workspace in
    let changes = changes @ drag_changes in
    { workspace; focus; focus_path; pane_keys; outline; outline_intents; selection; menu; menu_pick; scope_view; scope_changes; tree; document = (network value).graph;
      edit_error = value.edit_error;
      effects = Parameter.no_effects;
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
    | Panel_pick -> Some (Picking { split = true; at = frame.mouse })
    | Window_pick -> Some (Picking { split = false; at = frame.mouse })
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
          | List_command _ | Frame_tile -> false | _ -> true) value.keymap @ Leader.text_keys in
        let context = String.lowercase_ascii (Leader.pane_name focus) ^ " focused" in
        (if Pxui_shell.Which_key.sheet ui ~context ~category:Leader.sheet_group commands then Some Keys else None), None
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
                name, Printf.sprintf "%s · line %d" kind (Flow.Diagnostic.position_of_offset source byte).line, byte)
                (Text_pane.symbols source)
          | None -> [] in
        search_prompt ~key:"symbol-jump" ~title:"Go to symbol" ~label:"Search bindings" ~query all
          ~again:(fun q -> Symbols q)
          ~pick:(fun byte -> Run_action (Text_command (Text_pane.Select (byte, byte))))
    | Some (Referencing (name, query)) ->
        let all = match text_shown with
          | Some shown -> List.map (fun (byte, line) ->
              line, Printf.sprintf "line %d" (Flow.Diagnostic.position_of_offset (Text_pane.current_text text shown) byte).line, byte)
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
          | Event.KeyPressed (Input.KeyChar c) when c >= '1' && c < Char.chr (Char.code '1' + List.length kinds) ->
              Some (Char.code c - Char.code '1')
          | _ -> None) frame.events in
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
        let pressed key = List.exists (function Event.KeyPressed k -> k = key | _ -> false) frame.events in
        let axis = if pressed Input.Tab then (if axis = `H then `V else `H) else axis in
        let mx, my = frame.mouse in
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
        let box = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px (float frame.width)) ~h:(Ui.Px (float frame.height))
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
            let tint alpha = Color.with_alpha theme.accent alpha in
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
          || List.exists (function Event.MousePressed (Input.RightButton, _) -> true | _ -> false) frame.events in
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
  let result = match Pxui_shell.Shell.frame value.ui frame
      ~visible:all_ui_visible ~overlay:leader_panel
      ~body:(fun ui ->
        let prompt, prompt_intent = prompt_panel ui initial_prompt in
        let result = build ui in
        let prompt_intent = match prompt_intent, result.bar_action with
          | None, Some action -> Some (Run_action action) | intent, _ -> intent in
        { result with prompt; prompt_intent }) with
    | Some result -> result
    | None ->
      { workspace; focus; focus_path; pane_keys = []; graph_panes = value.graph_panes; locals = value.locals; other_texts = []; outline = value.outline; outline_intents = []; selection; menu; menu_pick = None; scope_view; scope_changes = scope_command_changes; tree; document = (network value).graph;
        edit_error = value.edit_error;
        effects = Parameter.no_effects; timeline_intents = [];
        frame_request = initial_frame_request; prompt = initial_prompt;
        prompt_intent = None; panel = None; grab = false;
        settings = value.doc.settings;
        opened = None; live_cook = value.live_cook; label = "Edit";
        changes = command_changes; tree_intents = list_intents;
        text_intents = []; open_graph;
        settings_changes = []; handle_changes = None; bar_action = None; view_pick = None; drops = [] } in
  reduce ~carry_changed ~all_ui_visible ~view_state ~carrying ~held_keys ~leader ~frame ~hud ~actions ~guide ~copied ~steady ~status_fps ~status_fps_at ~timeline ~timeline_changes ~graph_shown ~text ~text_shown ~row_sets ~rows value result
let update ?(image=fun _->None) ?(host_events=[]) value ~all_ui_visible ~text_focus ~camera_panel ~view_handles ~render_status
    ~error_status ~view_state (frame : Frame.t) =
  let update, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
  let value, frame, carry_changed = carry_step value ~text_focus frame in
  update_frame ~image ~host_events ~carry_changed value ~all_ui_visible ~text_focus ~camera_panel ~view_handles
    ~render_status ~error_status ~view_state frame) in
  if update.core.doc == value.doc then update
  else { update with core = { update.core with edit_phases = phases } }
