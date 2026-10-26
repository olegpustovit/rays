open Sop
open Editor_document
include Core_inspect

let stored_projection value =
  match Level_map.find_opt value.level value.projections with
  | Some projection -> projection
  | None -> (match value.level with
    | Document.Scene -> List_view
    | Inside id when kind value id = Some "geometry" -> Graph_view
    | Inside _ -> List_view)

(* Any of the panels that hold the graph pane's views is on screen. *)
let graph_family value = has_panel value Pxui_shell.Layout.Graph || has_panel value List
  || has_panel value Lisp

(* What the graph panel shows: the view it says, whatever other panels the layout has. *)
let projection = stored_projection

(* Which keys the focused panel takes: the list's and the text pane's own panels, else
   the graph panel's projection. *)
let listing value = value.focus = List || (value.focus <> Lisp && projection value = List_view)
let texting value = value.focus = Lisp || (value.focus <> List && projection value = Text_view)

(* ---- panel instances: every leaf draws, with its own list, text pane or outline ---- *)

let fresh_local () = { rows = Pxui_shell.Tree.create (); code = Text_pane.initial; nav = Navigator.initial; sheet_owner = 0 }
let local_of value key = match List.assoc_opt key value.locals with Some l -> l | None -> fresh_local ()
(* The panels' own states are kept for the 64 panels last drawn or changed (the latest first): a
   bound on what layouts that came and went leave behind. *)
let locals_capacity = 64
let put_local key f locals =
  (key, f (match List.assoc_opt key locals with Some l -> l | None -> fresh_local ()))
  :: List.filteri (fun i _ -> i < locals_capacity - 1) (List.remove_assoc key locals)

(* The open panels that hold a list, a text pane or an outline: a graph panel holds the one its
   view shows. *)
let panel_hosts value =
  List.filter_map (fun (path, panel) ->
    if not (visible value.workspace panel) || (panel_state value path).collapsed then None else
    let key = panel_key value.doc path in
    let kind = match panel with
      | Pxui_shell.Layout.List -> Some `List | Lisp -> Some `Text | Outline -> Some `Outline
      | Graph ->
          (match projection (as_pane value (key, path)) with
           | List_view -> Some `List | Text_view -> Some `Text | Graph_view -> None)
      | View _ | Canvas _ | Inspector | Spreadsheet | Timeline -> None in
    Option.map (fun kind -> key, path, panel, kind) kind)
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
    match focused, current with
    | Some key, _ when List.mem key keys -> Some key
    | _, Some key when List.mem key keys -> current
    | _ -> List.nth_opt keys 0 in
  let move value ~target ~current ~set ~save ~load =
    match target, current with
    | None, _ -> value
    | _ when target = current -> value
    | _, None -> set value target  (* the first one: the state so far is its *)
    | Some key, Some old ->
        let value = { value with locals = put_local old (save value) value.locals } in
        load (set value target) (local_of value key) in
  let value = move value ~target:(pick `List value.list_at) ~current:value.list_at
    ~set:(fun v list_at -> { v with list_at }) ~save:(fun v l -> { l with rows = v.tree })
    ~load:(fun v l -> { v with tree = l.rows }) in
  let value = move value ~target:(pick `Text value.text_at) ~current:value.text_at
    ~set:(fun v text_at -> { v with text_at }) ~save:(fun v l -> { l with code = v.text })
    ~load:(fun v l -> { v with text = l.code }) in
  move value ~target:(pick `Outline value.outline_at) ~current:value.outline_at
    ~set:(fun v outline_at -> { v with outline_at }) ~save:(fun v l -> { l with nav = v.outline })
    ~load:(fun v l -> { v with outline = l.nav })

(* The start keywords of the document, by panel key. *)
let start_of (doc : Document.t) = match doc.shell with
  | None -> { on = None; views = []; tabs = [] }
  | Some s ->
      let keyed l = List.map (fun (path, v) -> panel_key doc path, v) l in
      { on = Option.map (panel_key doc) s.start.focus; views = keyed s.start.graph_views; tabs = keyed s.start.tabs }

(* The editor follows a start keyword when its value in the text changed (flow.md 11.11): on
   opening, a reload, a preset, an edit, undo.  What did not change is left as the user has it. *)
let follow_start value =
  let now = start_of value.doc and was = value.started in
  if now = was then value else
  let leaves = Editor_core.Panels.leaves (shell_tree value value.workspace) in
  let path_of key = List.find_map (fun (path, panel) ->
    if panel_key value.doc path = key then Some (path, panel) else None) leaves in
  let changed now was = List.filter (fun (key, v) -> List.assoc_opt key was <> Some v) now in
  let value = match now.on with
    | Some key when now.on <> was.on ->
        (match path_of key with
         | Some (path, panel) -> { value with focus = panel; focus_path = Some path }
         | None -> value)
    | _ -> value in
  let value = List.fold_left (fun value (key, view) ->
    let view = match view with "list" -> List_view | "text" -> Text_view | _ -> Graph_view in
    if value.graph_pane = Some key then { value with projections = Level_map.add value.level view value.projections }
    else match path_of key with
      | Some (path, _) ->
          let pane = stash (as_pane value (key, path)) in
          { value with graph_panes = (key, { pane with views = Level_map.add pane.at view pane.views })
                                     :: List.remove_assoc key value.graph_panes }
      | None -> value) value (changed now.views was.views) in
  let value = List.fold_left (fun value (key, tab) ->
    let tab = match tab with "graph" -> Text_pane.Graph | "document" -> Document | _ -> Selection in
    if value.text_at = Some key || value.text_at = None then { value with text = { value.text with tab } }
    else { value with locals = put_local key (fun l -> { l with code = { l.code with tab } }) value.locals })
    value (changed now.tabs was.tabs) in
  { value with started = now }

(* The workspace graph the pane shows with zones and selectors: the one a
   [(ui/graph "name")] panel names, else the one the outline picked, else the graph of the
   geometry object opened (the one its network was lowered from, else the
   [sop] graph its label names), of the World opened, or the scene graph at the scene level.
   A level without such a graph shows its list only. *)
(* The lowered sop graph a geometry object instantiates, by its network. *)
let graph_of_object value id = Document.object_graph value.doc id

let graph_name value = match value.doc.Document.workspace with
  | ws, _ ->
      let exists name = List.exists (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs
        || (String.length name > 4 && String.sub name 0 4 = "def:"
            && List.exists (fun (g : Flow.Workspace.graph) -> "def:" ^ g.name = name) ws.checked.defs) in
      (* the graph the pane's own leaf pins *)
      let named = Option.bind value.doc.Document.shell (fun s ->
        Option.bind (graph_path value) (fun path -> List.assoc_opt path s.Document.named)) in
      (match (if value.pane_graph <> None then value.pane_graph else named), value.level with
       | Some name, _ when exists name -> Some name
       | _, Document.Inside id when kind value id = Some "geometry" ->
           (match graph_of_object value id with
            | Some _ as name -> name
            | None -> Option.bind (Edit_graph.find (scene value) ~node_id:id) (fun node ->
                let name = Node.label node in if exists name then Some name else None))
       | _, Inside id when kind value id = Some "world" ->
           List.find_map (fun (g : Flow.Workspace.graph) ->
             if g.context = Flow.Context.world then Some g.name else None) ws.checked.graphs
       | _, Document.Scene ->
           List.find_map (fun (g : Flow.Workspace.graph) ->
             if g.context = Flow.Context.scene then Some g.name else None) ws.checked.graphs
       | _ -> None)

(* For the crash report and tests: the graph of every graph panel, and the floating windows. *)
let graph_panels value =
  String.concat ", " (List.map (fun leaf -> Option.value ~default:"-" (graph_name (as_pane value leaf)))
    (graph_leaves value))
let windows value =
  String.concat ", " (List.filter_map (fun (path, panel) ->
    Option.map (fun (x, y, w, h) -> Printf.sprintf "%s %d %d %d %d"
      (String.lowercase_ascii (Editor_core.Panels.name panel)) x y w h) (panel_state value path).window)
    (Editor_core.Panels.leaves (shell_tree value value.workspace)))

(* Every panel by its key's last name, the focused one starred, with what is its own: a text
   pane's tab and draft, a list's focus, folds and filter, an outline's search. *)
let panels_line value =
  let hosts = panel_hosts value in
  String.concat "; " (List.map (fun (path, panel) ->
    let key = panel_key value.doc path in
    let name = String.lowercase_ascii (Editor_core.Panels.name panel) ^ " " ^ List.nth key (List.length key - 1)
      ^ (if value.focus = panel && value.focus_path = Some path then "*" else "") in
    let own = List.find_map (fun (k, p, _, kind) -> if k = key && p = path then Some kind else None) hosts in
    match own with
    | Some `Text -> name ^ ": " ^ Text_pane.summary (if Some key = value.text_at then value.text else (local_of value key).code)
    | Some `List -> name ^ ": " ^ Pxui_shell.Tree.summary (if Some key = value.list_at then value.tree else (local_of value key).rows)
    | Some `Outline -> name ^ Printf.sprintf ": search %S"
        (Navigator.query (if Some key = value.outline_at then value.outline else (local_of value key).nav))
    | None -> name)
    (Editor_core.Panels.leaves (shell_tree value value.workspace)))

(* Follow a reference: the pane shows [graph], and [u] comes back to what it showed. *)
let remember value =
  { value with back = List.filteri (fun i _ -> i < 32) ((value.level, value.pane_graph) :: value.back) }

let go value graph =
  if graph_name value = Some graph then value else { (remember value) with pane_graph = Some graph }

(* What the selected node of the graph pane references: the graph its [(ref name)] names, the
   [:material] of a sop/material first (a material node follows to its surface). *)
let follow_target ?path value =
  let ws, _ = value.doc.Document.workspace in
  let module S = Flow.Syntax in
  let exists name = List.exists (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs in
  let reference (e : S.t) = match e.node with
    | S.List [ { node = S.Sym "ref"; _ }; { node = S.Sym name; _ } ] when exists name -> Some name
    | _ -> None in
  match (match path with Some p -> [ p ] | None -> Pxui_graph.Scope.selected value.scope_view) with
  | [ path ] when graph_name value <> None ->
      let arg key = Option.bind (Flow_graph.Flow_edit.arg_text ws.source path key) reference in
      (match arg (Kw "material") with
       | Some _ as found -> found
       | None ->
           (match Flow_graph.Flow_edit.arg_text ws.source path Whole with
            | Some { node = S.List ({ node = S.Sym h; _ } :: _); _ }
              when List.exists (fun (d : Flow.Workspace.graph) -> d.name = h) ws.checked.defs -> Some ("def:" ^ h)
            | Some { node = S.List (_ :: args); _ } -> List.find_map reference args
            | _ -> None))
  | _ -> None

(* [follow_target], else the graph of the object selected in the list *)
let peek_target value =
  match follow_target value with
  | Some _ as found -> found
  | None -> Option.bind (Selection.selected value.selection) (fun id ->
      match kind value id with
      | Some "geometry" -> graph_of_object value id
      | _ -> None)

(* [i] in a viewport with nothing selected follows to the scene it shows: the graph its
   [(ref scene)] names, else the first scene graph. *)
let viewport_target value =
  let ws, _ = value.doc.Document.workspace in
  match value.focus with
  | Pxui_shell.Layout.View key when Selection.selected value.selection = None ->
      let named = Option.bind value.doc.Document.shell (fun shell ->
        match List.assoc_opt key shell.preview_sources with
        | Some { scene_ref = Some { node = Flow.Syntax.List [ { node = Sym "ref"; _ }; { node = Sym name; _ } ]; _ }; _ } ->
            Some name
        | _ -> None) in
      (match named with
       | Some _ -> named
       | None -> List.find_map (fun (g : Flow.Workspace.graph) ->
           if g.context = Flow.Context.scene then Some g.name else None) ws.checked.graphs)
  | _ -> None

(* The route taken, as the graph header shows it: scene > shards > cobalt (the last three) *)
let route value =
  List.rev_map (fun (level, pane_graph) -> graph_name { value with level; pane_graph }) value.back
  @ [ graph_name value ]
  |> List.filter_map Fun.id
  |> (fun names -> List.filteri (fun i _ -> i >= List.length names - 3) names)
  |> String.concat " > "

let scope_name value = if projection value = Graph_view then graph_name value else None

(* The lowered node of the node selected in the graph pane, at the iteration its zones probe. *)
let scope_node value =
  match value.scope_key, value.doc.Document.workspace, Pxui_graph.Scope.selected value.scope_view with
  | Some { scope; records = Some records; _ }, _, [ path ] when scope_name value <> None ->
      Option.bind (Flow_graph.Projection.find scope path) (fun (n : Flow_graph.Projection.node) ->
        Option.bind (compiled_at value (Flow_graph.Probe.chains scope) records n.path) (fun node_id ->
          Option.map (fun node -> n.path, node) (compiled_node value node_id)))
  | _ -> None


let lit_tags value =
  match value.doc.Document.workspace, value.scope_key,
        Pxui_graph.Scope.selected value.scope_view with
  | (_, lowered), Some { scope; _ }, [ site ] when scope_name value <> None ->
      (match value.lit with
       | Some c when c.site = site && c.at == value.probes && c.lowered == lowered
           && c.scope == scope -> c.tags, value.lit
       | _ ->
           let iter = probes_of value (Flow_graph.Probe.chains scope) site in
           let tags = Pick.Set.of_list (Flow_sop.Lower.tags lowered ~site ~iter) in
           tags, Some { site; at = value.probes; lowered; scope; tags })
  | _ -> Pick.Set.empty, None

(* The nodes of the pane's graph that the last failed cook names, each with its diagnostic's code
   (the pane draws the sheet's failed state on them). *)
let failed_nodes value =
  match Cook.failed_node value.cook, value.scope_key, value.doc.Document.workspace with
  | Some (code, node_id), Some { scope; records = Some records; _ }, _
    when scope_name value <> None ->
      let rec nodes (s : Flow_graph.Projection.scope) = List.concat_map (fun (n : Flow_graph.Projection.node) ->
        n :: (match n.zone with Some z -> nodes z.scope | None -> [])) s.nodes in
      let chains = Flow_graph.Probe.chains scope in
      List.filter_map (fun (n : Flow_graph.Projection.node) ->
        if compiled_at value chains records n.path = Some node_id then Some (n.path, code) else None) (nodes scope)
  | _ -> []
