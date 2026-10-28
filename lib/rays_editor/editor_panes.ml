open Editor_model
open Editor_shell
open Rays
open Sop
open Editor_document
open Editor_inspect

let stored_projection value =
  match Level_map.find_opt (pane_level value) (pane_views value) with
  | Some projection -> projection
  | None -> (
      match pane_level value with
      | Document.Scene -> List_view
      | Inside id when kind value id = Some "geometry" -> Graph_view
      | Inside _ -> List_view)

(* Any of the panels that hold the graph pane's views is on screen. *)
let graph_family value =
  has_panel value Pxui_shell.Layout.Graph || has_panel value List || has_panel value Lisp

(* What the graph panel shows: the view it says, whatever other panels the layout has. *)
let projection = stored_projection

(* Which keys the focused panel takes: the list's and the text pane's own panels, else
   the graph panel's projection. *)
let listing value = value.focus = List || (value.focus <> Lisp && projection value = List_view)
let texting value = value.focus = Lisp || (value.focus <> List && projection value = Text_view)

(* ---- panel instances: every leaf draws, with its own list, text pane or outline ---- *)

let fresh_local () =
  {
    rows = Pxui_shell.Tree.create ();
    code = Text_pane.initial;
    nav = Navigator.initial;
    sheet_owner = 0;
  }

let local_of value key =
  match List.assoc_opt key value.locals with Some l -> l | None -> fresh_local ()

(* The panels' own states are kept for the 64 panels last drawn or changed (the latest first): a
   bound on what layouts that came and went leave behind. *)
let locals_capacity = 64

let put_local key f locals =
  (key, f (match List.assoc_opt key locals with Some l -> l | None -> fresh_local ()))
  :: List.take (locals_capacity - 1) (List.remove_assoc key locals)

(* The open panels that hold a list, a text pane or an outline: a graph panel holds the one its
   view shows. *)
let panel_hosts value =
  List.filter_map
    (fun (path, panel) ->
      if (not (visible value.workspace panel)) || (panel_state value path).collapsed then None
      else
        let key = panel_key value.doc path in
        let kind =
          match panel with
          | Pxui_shell.Layout.List -> Some `List
          | Lisp -> Some `Text
          | Outline -> Some `Outline
          | Graph -> (
              match projection (as_pane value (key, path)) with
              | List_view -> Some `List
              | Text_view -> Some `Text
              | Graph_view -> None)
          | View _ | Canvas _ | Inspector | Spreadsheet | Timeline -> None
        in
        Option.map (fun kind -> (key, path, panel, kind)) kind)
    (Editor_core.Panels.leaves (shell_tree value value.workspace))

(* The editor as the graph panel a leaf shows has it: a graph leaf its own, a leaf tied by [:of]
   that panel's, any other the pane in use. *)
let shown_as value (key, path, panel) =
  if panel = Pxui_shell.Layout.Graph then as_pane value (key, path)
  else match tied value path with Some target -> as_pane value target | None -> value

(* [tree], [text] and [outline] follow the focus like the graph pane does: each belongs to the
   focused panel of its kind, else to the one it was, else to the first one open; the panel left
   keeps its own in [locals]. *)
let follow_hosts value ~focus_path hosts =
  let focused = Option.map (panel_key value.doc) focus_path in
  let pick kind current =
    let keys = List.filter_map (fun (key, _, _, k) -> if k = kind then Some key else None) hosts in
    match (focused, current) with
    | Some key, _ when List.mem key keys -> Some key
    | _, Some key when List.mem key keys -> current
    | _ -> List.nth_opt keys 0
  in
  let move value ~target ~current ~set ~save ~load =
    match (target, current) with
    | None, _ -> value
    | _ when target = current -> value
    | _, None -> set value target (* the first one: the state so far is its *)
    | Some key, Some old ->
        let value = { value with locals = put_local old (save value) value.locals } in
        load (set value target) (local_of value key)
  in
  let value =
    move value ~target:(pick `List value.list_at) ~current:value.list_at
      ~set:(fun v list_at -> { v with list_at })
      ~save:(fun v l -> { l with rows = v.tree })
      ~load:(fun v l -> { v with tree = l.rows })
  in
  let value =
    move value ~target:(pick `Text value.text_at) ~current:value.text_at
      ~set:(fun v text_at -> { v with text_at })
      ~save:(fun v l -> { l with code = v.text })
      ~load:(fun v l -> { v with text = l.code })
  in
  move value ~target:(pick `Outline value.outline_at) ~current:value.outline_at
    ~set:(fun v outline_at -> { v with outline_at })
    ~save:(fun v l -> { l with nav = v.outline })
    ~load:(fun v l -> { v with outline = l.nav })

(* The start keywords of the document, by panel key. *)
let start_of (doc : Document.t) =
  match doc.shell with
  | None -> { on = None; views = []; tabs = [] }
  | Some s ->
      let keyed l = List.map (fun (path, v) -> (panel_key doc path, v)) l in
      {
        on = Option.map (panel_key doc) s.start.focus;
        views = keyed s.start.graph_views;
        tabs = keyed s.start.tabs;
      }

(* The editor follows a start keyword when its value in the text changed (flow.md 11.11): on
   opening, a reload, a preset, an edit, undo.  What did not change is left as the user has it. *)
let follow_start value =
  let now = start_of value.doc and was = value.started in
  if now = was then value
  else
    let leaves = Editor_core.Panels.leaves (shell_tree value value.workspace) in
    let path_of key =
      List.find_map
        (fun (path, panel) -> if panel_key value.doc path = key then Some (path, panel) else None)
        leaves
    in
    let changed now was = List.filter (fun (key, v) -> List.assoc_opt key was <> Some v) now in
    let value =
      match now.on with
      | Some key when now.on <> was.on -> (
          match path_of key with
          | Some (path, panel) -> { value with focus = panel; focus_path = Some path }
          | None -> value)
      | _ -> value
    in
    let value =
      List.fold_left
        (fun value (key, view) ->
          let view =
            match view with "list" -> List_view | "text" -> Text_view | _ -> Graph_view
          in
          if Panel_map.key value.panes = Some key then
            with_pane value (fun pane ->
                { pane with views = Level_map.add pane.at view pane.views })
          else
            match path_of key with
            | Some (path, _) ->
                let pane = Panel_map.active (as_pane value (key, path)).panes in
                {
                  value with
                  panes =
                    Panel_map.set key
                      { pane with views = Level_map.add pane.at view pane.views }
                      value.panes;
                }
            | None -> value)
        value (changed now.views was.views)
    in
    let value =
      List.fold_left
        (fun value (key, tab) ->
          let tab =
            match tab with "graph" -> Text_pane.Graph | "document" -> Document | _ -> Selection
          in
          if value.text_at = Some key || value.text_at = None then
            { value with text = { value.text with tab } }
          else
            {
              value with
              locals = put_local key (fun l -> { l with code = { l.code with tab } }) value.locals;
            })
        value (changed now.tabs was.tabs)
    in
    { value with started = now }

(* The workspace graph the pane shows with zones and selectors: the one a
   [(ui/graph "name")] panel names, else the one the outline picked, else the graph of the
   geometry object opened (the one its network was lowered from, else the
   [sop] graph its label names), of the World opened, or the scene graph at the scene level.
   A level without such a graph shows its list only. *)
(* The lowered sop graph a geometry object instantiates, by its network. *)
let graph_of_object value id = Document.object_graph value.doc id

let graph_name value =
  match value.doc.Document.workspace.ws with
  | ws -> (
      let exists name =
        List.exists (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs
        || String.length name > 4
           && String.sub name 0 4 = "def:"
           && List.exists (fun (g : Flow.Workspace.graph) -> "def:" ^ g.name = name) ws.checked.defs
      in
      (* the graph the pane's own leaf pins *)
      let named =
        Option.bind value.doc.Document.shell (fun s ->
            Option.bind (graph_path value) (fun path -> List.assoc_opt path s.Document.named))
      in
      match ((if pane_shown value <> None then pane_shown value else named), pane_level value) with
      | Some name, _ when exists name -> Some name
      | _, Document.Inside id when kind value id = Some "geometry" -> (
          match graph_of_object value id with
          | Some _ as name -> name
          | None ->
              Option.bind
                (Kind_graph.find (scene value) id)
                (fun (node : Kind_graph.node) ->
                  let name = node.label in
                  if exists name then Some name else None))
      | _, Inside id when kind value id = Some "world" ->
          List.find_map
            (fun (g : Flow.Workspace.graph) ->
              if g.context = Flow.Context.world then Some g.name else None)
            ws.checked.graphs
      | _, Document.Scene ->
          List.find_map
            (fun (g : Flow.Workspace.graph) ->
              if g.context = Flow.Context.scene then Some g.name else None)
            ws.checked.graphs
      | _ -> None)

(* For the crash report and tests: the graph of every graph panel, and the floating windows. *)
let graph_panels value =
  String.concat ", "
    (List.map
       (fun leaf -> Option.value ~default:"-" (graph_name (as_pane value leaf)))
       (graph_leaves value))

let windows value =
  String.concat ", "
    (List.filter_map
       (fun (path, panel) ->
         Option.map
           (fun (x, y, w, h) ->
             Printf.sprintf "%s %d %d %d %d"
               (String.lowercase_ascii (Editor_core.Panels.name panel))
               x y w h)
           (panel_state value path).window)
       (Editor_core.Panels.leaves (shell_tree value value.workspace)))

(* Every panel by its key's last name, the focused one starred, with what is its own: a text
   pane's tab and draft, a list's focus, folds and filter, an outline's search. *)
let panels_line value =
  let hosts = panel_hosts value in
  String.concat "; "
    (List.map
       (fun (path, panel) ->
         let key = panel_key value.doc path in
         let name =
           String.lowercase_ascii (Editor_core.Panels.name panel)
           ^ " "
           ^ List.nth key (List.length key - 1)
           ^ if value.focus = panel && value.focus_path = Some path then "*" else ""
         in
         let own =
           List.find_map
             (fun (k, p, _, kind) -> if k = key && p = path then Some kind else None)
             hosts
         in
         match own with
         | Some `Text ->
             name ^ ": "
             ^ Text_pane.summary
                 (if Some key = value.text_at then value.text else (local_of value key).code)
         | Some `List ->
             name ^ ": "
             ^ Pxui_shell.Tree.summary
                 (if Some key = value.list_at then value.tree else (local_of value key).rows)
         | Some `Outline ->
             name
             ^ Printf.sprintf ": search %S"
                 (Navigator.query
                    (if Some key = value.outline_at then value.outline else (local_of value key).nav))
         | None -> name)
       (Editor_core.Panels.leaves (shell_tree value value.workspace)))

(* Follow a reference: the pane shows [graph], and [u] comes back to what it showed. *)
let remember value =
  with_pane value (fun pane ->
      { pane with route = List.take 32 ((pane.at, pane.shown) :: pane.route) })

let go value graph =
  if graph_name value = Some graph then value
  else with_pane (remember value) (fun pane -> { pane with shown = Some graph })

(* What the selected node of the graph pane references: the graph its [(ref name)] names, the
   [:material] of a sop/material first (a material node follows to its surface). *)
let follow_target ?path value =
  let ws = value.doc.Document.workspace.ws in
  let module S = Flow.Syntax in
  let exists name =
    List.exists (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs
  in
  let reference (e : S.t) =
    match e.node with
    | S.List [ { node = S.Sym "ref"; _ }; { node = S.Sym name; _ } ] when exists name -> Some name
    | _ -> None
  in
  match match path with Some p -> [ p ] | None -> Pxui_graph.Scope.selected (pane_view value) with
  | [ path ] when graph_name value <> None -> (
      let arg key = Option.bind (Flow_graph.Flow_edit.arg_text ws.source path key) reference in
      match arg (Kw "material") with
      | Some _ as found -> found
      | None -> (
          match Flow_graph.Flow_edit.arg_text ws.source path Whole with
          | Some { node = S.List ({ node = S.Sym h; _ } :: _); _ }
            when List.exists (fun (d : Flow.Workspace.graph) -> d.name = h) ws.checked.defs ->
              Some ("def:" ^ h)
          | Some { node = S.List (_ :: args); _ } -> List.find_map reference args
          | _ -> None))
  | _ -> None

(* [follow_target], else the graph of the object selected in the list *)
let peek_target value =
  match follow_target value with
  | Some _ as found -> found
  | None ->
      Option.bind
        (Selection.selected (pane_selection value))
        (fun id ->
          match kind value id with Some "geometry" -> graph_of_object value id | _ -> None)

(* [i] in a viewport with nothing selected follows to the scene it shows: the graph its
   [(ref scene)] names, else the first scene graph. *)
let viewport_target value =
  let ws = value.doc.Document.workspace.ws in
  match value.focus with
  | Pxui_shell.Layout.View key when Selection.selected (pane_selection value) = None -> (
      let named =
        Option.bind value.doc.Document.shell (fun shell ->
            match List.assoc_opt key shell.preview_sources with
            | Some
                {
                  scene_ref =
                    Some
                      {
                        node = Flow.Syntax.List [ { node = Sym "ref"; _ }; { node = Sym name; _ } ];
                        _;
                      };
                  _;
                } ->
                Some name
            | _ -> None)
      in
      match named with
      | Some _ -> named
      | None ->
          List.find_map
            (fun (g : Flow.Workspace.graph) ->
              if g.context = Flow.Context.scene then Some g.name else None)
            ws.checked.graphs)
  | _ -> None

(* The route taken, as the graph header shows it: scene > shards > cobalt (the last three) *)
let route value =
  List.rev_map
    (fun (at, shown) -> graph_name (with_pane value (fun pane -> { pane with at; shown })))
    (pane_route value)
  @ [ graph_name value ]
  |> List.filter_map Fun.id
  |> (fun names -> List.drop (max 0 (List.length names - 3)) names)
  |> String.concat " > "

let scope_name value = if projection value = Graph_view then graph_name value else None

(* The lowered node of the node selected in the graph pane, at the iteration its zones probe. *)
let scope_node value =
  match
    (pane_laid value, value.doc.Document.workspace, Pxui_graph.Scope.selected (pane_view value))
  with
  | Some { scope; records = Some records; _ }, _, [ path ] when scope_name value <> None ->
      Option.bind (Flow_graph.Projection.find scope path) (fun (n : Flow_graph.Projection.node) ->
          Option.bind
            (compiled_at value (Flow_graph.Probe.chains scope) records n.path)
            (fun node_id -> Option.map (fun node -> (n.path, node)) (compiled_node value node_id)))
  | _ -> None

let lit_tags value =
  match
    ( value.doc.Document.workspace.lowered,
      pane_laid value,
      Pxui_graph.Scope.selected (pane_view value) )
  with
  | lowered, Some { scope; _ }, [ site ] when scope_name value <> None -> (
      match value.lit with
      | Some c
        when c.site = site && c.at == value.probes && c.lowered == lowered && c.scope == scope ->
          (c.tags, value.lit)
      | _ ->
          let iter = probes_of value (Flow_graph.Probe.chains scope) site in
          let tags = Pick.Set.of_list (Flow_sop.Lower.tags lowered ~site ~iter) in
          (tags, Some { site; at = value.probes; lowered; scope; tags }))
  | _ -> (Pick.Set.empty, None)

(* The nodes of the pane's graph that the last failed cook names, each with its diagnostic's code
   (the pane draws the sheet's failed state on them). *)
let failed_nodes value =
  match (Cook.failed_node value.cook, pane_laid value, value.doc.Document.workspace) with
  | Some (code, node_id), Some { scope; records = Some records; _ }, _ when scope_name value <> None
    ->
      let rec nodes (s : Flow_graph.Projection.scope) =
        List.concat_map
          (fun (n : Flow_graph.Projection.node) ->
            n :: (match n.zone with Some z -> nodes z.scope | None -> []))
          s.nodes
      in
      let chains = Flow_graph.Probe.chains scope in
      List.filter_map
        (fun (n : Flow_graph.Projection.node) ->
          if compiled_at value chains records n.path = Some node_id then Some (n.path, code)
          else None)
        (nodes scope)
  | _ -> []

let draw_list_panes ~ui ~shortcut_frame ~value ~hosted ~root_at ~pane_of ~in_use ~title_of ~rows
    ~columns ~host_rows ~selected ~tree ~intents ~locals =
  List.fold_left
    (fun (tree, intents, locals) ((key, path, _, _) as host) ->
      match root_at path with
      | None -> (tree, intents, locals)
      | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
          let pane = pane_of host in
          let mine = in_use pane in
          let rows, columns = if mine then (rows, columns) else List.assoc key host_rows in
          let selected =
            if mine then selected () else Selection.selected_nodes (pane_selection pane)
          in
          let draw state =
            on_sheet ui leaf.floating (fun () ->
                Pxui.Ui.within ui root (fun () ->
                    Pxui_shell.Tree.update state ui shortcut_frame ~bounds:leaf.body
                      ~title:(title_of pane) ~columns rows ~selected))
          in
          if Some key = value.list_at then
            let tree, emitted = draw tree in
            (tree, (if mine then intents @ emitted else intents), locals)
          else
            let state, emitted = draw (local_of value key).rows in
            ( tree,
              (if mine then intents @ emitted else intents),
              put_local key (fun l -> { l with rows = state }) locals ))
    (tree, intents, locals) hosted

let draw_text_panes ~ui ~frame ~value ~root_at ~header_slots ~text_host ~text_shown ~text
    ~other_texts ~locals =
  let names = lazy (completion_names value) in
  let view_text path state shown =
    match root_at path with
    | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
        Pxui.Ui.within ui root (fun () ->
            Text_pane.view ui ~bounds:leaf.body
              ~tabs_right:(Option.join (List.nth_opt (List.rev (header_slots leaf)) 0))
              ~vocab:(Lazy.force value.lisp_vocab) ~names:(Lazy.force names)
              ~time:frame.Frame.input.t state shown)
    | None -> []
  in
  let text_intents =
    match (text_shown, text_host) with
    | Some shown, Some (_, path, _, _) -> view_text path text shown
    | _ -> []
  in
  let locals, other_texts =
    List.fold_left
      (fun (locals, texts) (key, path, state, shown) ->
        ( put_local key (fun l -> { l with code = state }) locals,
          (key, shown, view_text path state shown) :: texts ))
      (locals, []) other_texts
  in
  (text, text_intents, locals, other_texts)

let draw_outline_panes ~ui ~value ~hosted ~root_at ~params_of ~outline ~locals =
  List.fold_left
    (fun (outline, intents, locals) (key, path, _, _) ->
      match root_at path with
      | None -> (outline, intents, locals)
      | Some ((leaf : Pxui_shell.Layout.leaf), root) ->
          let draw state =
            on_sheet ui leaf.floating (fun () ->
                Pxui.Ui.within ui root (fun () ->
                    Navigator.view state ui ~bounds:leaf.body (params_of value)))
          in
          if Some key = value.outline_at then
            let outline, emitted = draw outline in
            (outline, intents @ emitted, locals)
          else
            let state, emitted = draw (local_of value key).nav in
            (outline, intents @ emitted, put_local key (fun l -> { l with nav = state }) locals))
    (outline, [], locals) hosted

let draw_spreadsheet_panes ~ui ~value ~panes ~locals =
  List.fold_left
    (fun locals ((leaf : Pxui_shell.Layout.leaf), root) ->
      let key = panel_key value.doc leaf.path in
      let pane = shown_as value (key, leaf.path, leaf.panel) in
      let owner = (local_of { value with locals } key).sheet_owner in
      let changed =
        Pxui.Ui.within ui root (fun () ->
            Spreadsheet.view ui ~bounds:leaf.body ~owner (spreadsheet_source pane))
      in
      match changed with
      | None -> locals
      | Some sheet_owner -> put_local key (fun local -> { local with sheet_owner }) locals)
    locals panes

(* the keys down this frame that were not down in the last: the edges panes act on *)
let pressed_keys value (frame : Frame.t) =
  List.filter (fun k -> not (List.mem k value.held_keys)) frame.input.keys

let draw_other_graph_panes ~ui ~shortcut_frame ~value ~other_panes ~roots =
  List.filter_map
    (fun (key, path, (pane : t)) ->
      match List.find_opt (fun ((leaf : Pxui_shell.Layout.leaf), _) -> leaf.path = path) roots with
      | Some ((leaf : Pxui_shell.Layout.leaf), hosted)
        when let _, _, _, h = leaf.body in
             h > 0 ->
          let x, y, width, height = leaf.body in
          let view =
            if stored_projection pane = Graph_view && graph_name pane <> None then
              Pxui.Ui.within ui hosted (fun () ->
                  pane_view pane |> Pxui_graph.Scope.with_guide false
                  |> Pxui_graph.Scope.with_theme
                       (let theme = Pxui.Ui.theme ui in
                        if leaf.floating then sheet_theme theme else theme)
                  |> Pxui_graph.Scope.with_bounds ~x ~y ~width:(max 1 width) ~height:(max 1 height)
                  |> fun view ->
                  fst
                    (Pxui_graph.Scope.update
                       ~pressed:(pressed_keys value shortcut_frame)
                       view ui shortcut_frame))
            else pane_view pane
          in
          Some (key, { (Panel_map.active pane.panes) with view })
      | _ -> Some (key, Panel_map.active pane.panes))
    other_panes

let draw_graph_header ~ui ~value ~geometry:g ~(graph_host : Pxui_shell.Layout.leaf option)
    ~header_slots ~tools_from =
  let tool_action =
    match graph_host with
    | Some leaf when projection value = Graph_view && has_panel value Pxui_shell.Layout.Graph -> (
        match
          Bars.graph_tools
            ~outline:(not (has_panel value Pxui_shell.Layout.Outline))
            ui ~header:leaf.header ~from:(tools_from leaf)
            ~enabled:(fun tool ->
              tool = Bars.Outline
              || scope_name value <> None
                 && (tool = Bars.Add || Pxui_graph.Scope.selected (pane_view value) <> []))
        with
        | Some Add -> Some Editor_commands.Add_node
        | Some Repeat -> Some (Editor_commands.Scope_command Pxui_graph.Scope.Wrap_repeat)
        | Some Iterate -> Some (Editor_commands.Scope_command Pxui_graph.Scope.Wrap_iterate)
        | Some Fn -> Some (Editor_commands.Scope_command Pxui_graph.Scope.Make_fn)
        | Some Macro -> Some (Editor_commands.Scope_command Pxui_graph.Scope.Make_macro)
        | Some Defn -> Some (Editor_commands.Scope_command Pxui_graph.Scope.Make_defn)
        | Some Outline ->
            Some (Editor_commands.Panel_split_at (leaf.path, `H, 0.7, Pxui_shell.Layout.Outline))
        | None -> None)
    | _ -> None
  in
  let view_pick =
    match graph_host with
    | Some leaf when has_panel value Pxui_shell.Layout.Graph -> (
        let _, hy, _, hh = leaf.header in
        let active =
          match projection value with Graph_view -> 0 | List_view -> 1 | Text_view -> 2
        in
        match
          Option.bind
            (List.nth_opt (header_slots leaf) 0)
            (fun slot ->
              Option.bind slot (fun right ->
                  fst
                    (Pxui_shell.Kit.segments ui ~key:"workspace-view" ~right
                       ~y:(float hy +. (float (hh - 20) /. 2.))
                       [ "Graph"; "List"; "Text" ] active)))
        with
        | Some i -> Some (List.nth [ Graph_view; List_view; Text_view ] i)
        | None -> None)
    | _ -> None
  in
  let view_action =
    match (active_view value g, value.view_tools) with
    | Some ({ header = hx, hy, hw, hh; _ } as leaf), Some (looks, mode) when hh > 0 ->
        let from = float hx +. tools_from leaf in
        let modes = [ "Solid"; "Wire"; "Traced" ] in
        let seg_w = List.fold_left (fun w label -> w +. Pxui.Ui.text_width ui label) 24. modes in
        let look_w = Pxui_shell.Kit.button_width ui ~hint:"C" "Look through" in
        let ty = float hy +. (float (hh - 20) /. 2.) in
        let frame_w = Pxui_shell.Kit.button_width ui ~hint:"F" "Frame" in
        if from +. seg_w +. 8. +. look_w +. 8. +. frame_w < float (hx + hw - 36) then begin
          Bars.rule ui ~key:"workspace-view-rule" ~header:leaf.header ~from:(from -. float hx);
          let render =
            match
              fst
                (Pxui_shell.Kit.segments ui ~key:"workspace-render" ~right:(from +. seg_w) ~y:ty
                   modes mode)
            with
            | Some i when i <> mode -> Some (Editor_commands.Render_mode i)
            | _ -> None
          in
          let look =
            Pxui_shell.Kit.button ui ~key:"workspace-look"
              ~at:(from +. seg_w +. 8., ty)
              ~w:look_w ~active:looks ~hint:"C" "Look through"
          in
          let framed =
            Pxui_shell.Kit.button ui ~key:"workspace-frame"
              ~at:(from +. seg_w +. 8. +. look_w +. 8., ty)
              ~w:frame_w ~hint:"F" "Frame"
          in
          if framed then Some Editor_commands.Frame_camera
          else if look then Some Editor_commands.Look_through
          else render
        end
        else None
    | _ -> None
  in
  (tool_action, view_action, view_pick)

let draw_graph_pane ~ui ~frame ~value ~shortcut_frame ~scope_view ~scope_active ~guide ~carry_lit
    ~carry_hot ~graph_root ~(graph_host : Pxui_shell.Layout.leaf option) ~workspace ~menu ~open_menu
    =
  let (frame : Frame.t) = frame in
  let graph_body = match graph_host with Some leaf -> leaf.body | None -> (0, 0, 0, 0) in
  let gx, gy, gw, gh = graph_body in
  let scope_view, scope_frame_changes =
    if not scope_active then (Pxui_graph.Scope.suspend scope_view, [])
    else
      Pxui.Ui.within ui graph_root (fun () ->
          scope_view |> Pxui_graph.Scope.with_guide guide
          |> Pxui_graph.Scope.with_failed (failed_nodes value)
          |> Pxui_graph.Scope.with_theme
               (let theme = Pxui.Ui.theme ui in
                match graph_host with
                | Some { floating = true; _ } -> sheet_theme theme
                | _ -> theme)
          |> Pxui_graph.Scope.with_bounds ~x:gx ~y:gy ~width:(max 1 gw) ~height:(max 1 gh)
          |> Pxui_graph.Scope.with_carry ~lit:carry_lit ~hot:carry_hot
          |> fun view ->
          Pxui_graph.Scope.update
            ~pressed:(pressed_keys value shortcut_frame)
            view ui shortcut_frame)
  in
  let menu =
    match
      ( menu,
        List.find_map
          (function
            | Pxui_graph.Scope.Menu_requested (x, y) -> Some (int_of_float x, int_of_float y)
            | _ -> None)
          scope_frame_changes )
    with
    | None, Some at -> open_menu ?scope_view:(Some scope_view) value at
    | menu, _ -> menu
  in
  let menu, menu_pick =
    match menu with
    | Some menu ->
        Pxui.Ui.within ui graph_root (fun () ->
            Pxui_graph.Node_menu.update menu ui
              ~bounds:
                ( 0,
                  0,
                  fst frame.input.size,
                  let _, sy, _, _ = (geometry value workspace frame).status_at in
                  sy ))
    | None -> (None, None)
  in
  (graph_body, scope_view, scope_frame_changes, menu, menu_pick)

let draw_timeline_pane ~ui ~timeline ~max_frame ~slots ~timeline_root ~strip_bounds =
  let intents =
    List.concat_map
      (fun (((_, _, _, height) as bounds), box, edge) ->
        if height <= 0 then []
        else
          Pxui.Ui.within ui box (fun () ->
              Pxui_shell.Timeline_bar.draw ui ~bounds ~edge
                ~playing:(Timeline.mode timeline = Timeline.Playing)
                ~frame:(Timeline.frame timeline) ~time:(Timeline.time timeline) ~max_frame ()))
      slots
  in
  let strip_playback, strip_layout =
    match timeline_root with
    | Some (_, box)
      when let _, _, _, h = strip_bounds in
           h > 0 ->
        Pxui.Ui.within ui box (fun () ->
            let module Ui = Pxui.Ui in
            Option.iter
              (fun (x, y) -> Ui.set_text_state ui box (Some (Printf.sprintf "%g %g" x y)))
              (Ui.context_at ui box);
            match Option.map (String.split_on_char ' ') (Ui.text_state ui box) with
            | Some [ x; y ] -> (
                match (float_of_string_opt x, float_of_string_opt y) with
                | Some x, Some y -> (
                    let playing = Timeline.mode timeline = Timeline.Playing in
                    match
                      Ui.context_menu ui ~at:(x, y) "timeline-context"
                        [
                          ((if playing then "Pause" else "Play"), true);
                          ("Stop", true);
                          ("Hide timeline", true);
                        ]
                    with
                    | `Open -> ([], [])
                    | result -> (
                        Ui.set_text_state ui box None;
                        match result with
                        | `Pick 0 -> ([ Pause_toggle ], [])
                        | `Pick 1 -> ([ Stop_playback ], [])
                        | `Pick 2 -> ([], [ Pxui_shell.Chrome.Toggle [ -1 ] ])
                        | _ -> ([], [])))
                | _ -> ([], []))
            | _ -> ([], []))
    | _ -> ([], [])
  in
  (intents @ strip_playback, strip_layout)
