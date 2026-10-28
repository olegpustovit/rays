open Core_model
open Editor_document

(* ---- the shell ---- *)

(* The layouts / [ switches among: a switch's, named from their panels; else the editor graphs of an
   older file (several named layouts), the current one marked. *)
let layouts value =
  match value.doc.Document.shell with
  | Some { switch = Some sw; _ } ->
      List.mapi (fun i name -> name, i = sw.active) (Editor_core.Panels.labels sw.layouts)
  | _ ->
      let doc = value.doc.Document.workspace.ws in
      let current = Option.map (fun (g : Flow.Workspace.graph) -> g.name) (Workspace_doc.editor_graph doc) in
      (match List.filter (fun (g : Flow.Workspace.graph) -> g.context = Flow.Context.editor) doc.checked.graphs with
       | [] | [ _ ] -> []
       | gs -> List.map (fun (g : Flow.Workspace.graph) -> g.name, Some g.name = current) gs)

(* What the text pane completes beyond its own text: the graphs, the materials, the cameras of the
   first scene graph and the layouts. *)
let completion_names value : Lisp_text.names =
  let ws = value.doc.Document.workspace.ws in
  let graphs = ws.checked.graphs in
  let named context = List.filter_map (fun (g : Flow.Workspace.graph) ->
    if g.context = context then Some g.name else None) graphs in
  { graphs = List.map (fun (g : Flow.Workspace.graph) -> g.name) graphs;
    materials = named Flow.Context.material;
    cameras = (match named Flow.Context.scene with
      | scene :: _ -> Text_pane.cameras ws.source scene | [] -> []);
    layouts = List.map fst (layouts value) }

(* The tree drawn now: the editor graph's (the host's when the document has none, or
   after "Restore layout"), with the split being dragged at its live ratio. *)
let shell_tree value (shell : shell) =
  let base = if shell.restored then shell.tree
    else match value.doc.Document.shell with Some s -> s.tree | None -> shell.tree in
  match shell.live with
  | Some (node, size) -> Editor_core.Panels.set_size node size base
  | None -> base

let visible (shell : shell) panel = not (List.mem panel shell.hidden)

let panel_key (doc : Document.t) path =
  let graph = Option.fold ~none:"@default" ~some:(fun (g : Flow.Workspace.graph) -> g.name)
    (Workspace_doc.editor_graph (doc.workspace.ws)) in
  let origin path = Option.bind doc.shell (fun s -> List.assoc_opt path s.Document.origins) in
  (* a binding several leaves are made from names none of them: each is keyed by its place *)
  let once name = not (Option.fold ~none:false ~some:(fun (s : Document.shell) -> List.mem name s.repeated) doc.shell) in
  match origin path, List.rev path with
  | Some (Document.Bound name), _ when once name -> [graph; name]
  (* [name (ui/floating (ui/inspector))]: the window is the float's binding *)
  | _, 0 :: up when (match Option.bind doc.shell (fun s -> Editor_core.Panels.at (List.rev up) s.tree), origin (List.rev up) with
      | Some (Float _), Some (Document.Bound _) -> true | _ -> false) ->
      (match origin (List.rev up) with Some (Document.Bound name) -> [graph; name] | _ -> assert false)
  | _ -> graph :: "@panel" :: List.map string_of_int path

let panel_state value path =
  let saved key = Layout_by_path.Path_map.find_opt key (value.doc.workspace.ws).layout.panels in
  let state = if value.workspace.restored then Editor_core.Panels.default_state else
    match saved (panel_key value.doc path) with
    | Some state -> state
    | None ->
        (* a leaf of a binding used twice starts from the binding's entry (flow.md 11.11) *)
        let by_name = match Option.bind value.doc.shell (fun s -> List.assoc_opt path s.Document.origins),
                            panel_key value.doc path with
          | Some (Document.Bound name), graph :: "@panel" :: _ -> saved [ graph; name ]
          | _ -> None in
        Option.value by_name ~default:{ Editor_core.Panels.default_state with collapsed = path = [-1] } in
  match value.workspace.window_live with
  | Some (p, window) when p = path -> { state with window = Some window }
  | _ -> state

(* An edit that changes the editor graph's tree ends "Restore layout". *)
let unrestore (before : Document.t) (after : Document.t) (shell : shell) =
  let tree (doc : Document.t) = Option.map (fun (s : Document.shell) -> s.tree) doc.shell in
  if tree before <> tree after then { shell with restored = false; window_live = None } else shell
(* the host bar above the panels: a document with an editor graph has one *)
let shell_hidden value (shell : shell) =
  if shell.restored then shell.hidden else
  if (panel_state value [-1]).collapsed
    && not (List.exists (fun (_, p) -> p = Pxui_shell.Layout.Timeline)
      (Editor_core.Panels.leaves (shell_tree value shell)))
  then [Pxui_shell.Layout.Timeline] else []
let geometry value (shell : shell) frame =
  Pxui_shell.Layout.geometry ~state:(panel_state value) ~hidden:(shell_hidden value shell) (shell_tree value shell) frame
let has_panel value panel =
  visible value.workspace panel
  && List.exists (fun (path, p) -> p = panel && not (panel_state value path).collapsed)
    (Editor_core.Panels.leaves (shell_tree value value.workspace))

(* The viewport that keyboard focus and the camera follow: the focused one, else the first. *)
let active_view value (g : Pxui_shell.Layout.geometry) = match value.focus with
  | View _ as panel when Pxui_shell.Layout.find g panel <> None -> Pxui_shell.Layout.find g panel
  | _ -> Pxui_shell.Layout.first_view g

let panes value frame =
  let g = geometry value value.workspace frame in
  let panes = Pxui_shell.Layout.panes g in
  match active_view value g with
  | Some leaf -> { panes with view = leaf.body }
  | None -> panes

(* The viewports on screen and the bodies they are drawn in. *)
let view_bodies value frame =
  List.filter_map (fun (leaf : Pxui_shell.Layout.leaf) -> match leaf.panel with
    | View key when not (panel_state value leaf.path).collapsed -> Some (key, leaf.body) | _ -> None)
    (geometry value value.workspace frame).leaves

let view_visible value =
  List.exists (fun (path, p) -> match p with Pxui_shell.Layout.View _ ->
    visible value.workspace p && not (panel_state value path).collapsed | _ -> false)
    (Editor_core.Panels.leaves (shell_tree value value.workspace))

(* ---- graph panels: each [ui/graph] leaf is a pane of its own ---- *)

(* The graph leaves of the tree shown, by panel key: walked once a frame ([follow_graph]). *)
let graph_leaves value =
  List.filter_map (fun (path, p) ->
    if p = Pxui_shell.Layout.Graph then Some (panel_key value.doc path, path) else None)
    (Editor_core.Panels.leaves (shell_tree value value.workspace))

(* The leaf of the pane in use: where [graph_pane] was last seen when that is still its leaf (no
   walk: this is read many times a frame), else found again, else the first graph leaf. *)
let graph_path value =
  match value.graph_pane, value.graph_at with
  | Some key, Some path when Editor_core.Panels.at path (shell_tree value value.workspace)
                             = Some (Editor_core.Panels.Leaf Pxui_shell.Layout.Graph)
                             && panel_key value.doc path = key -> Some path
  | _ ->
      let leaves = graph_leaves value in
      (match Option.bind value.graph_pane (fun key -> List.assoc_opt key leaves), leaves with
       | Some path, _ -> Some path
       | None, (_, path) :: _ -> Some path
       | None, [] -> None)

(* What a graph leaf's own keywords open it with: its [:view], else the canvas when it pins a graph. *)
let start_views (doc : Document.t) level path =
  match doc.shell with
  | None -> Level_map.empty
  | Some s ->
      (match List.assoc_opt path s.start.graph_views with
       | Some "list" -> Level_map.singleton level List_view
       | Some "text" -> Level_map.singleton level Text_view
       | Some _ -> Level_map.singleton level Graph_view
       | None -> if List.mem_assoc path s.named then Level_map.singleton level Graph_view else Level_map.empty)

let stash value = { view = value.scope_view; laid = value.scope_key; shown = value.pane_graph;
                    route = value.back; views = value.projections; at = value.level; picked = value.selection }

(* The editor as the graph panel [key] at [path] has it: its own pane when it is not the one in use.
   The document may have changed under a pane that waited (an undo from another panel): its level
   is resolved again, and a level that is gone takes its selection with it. *)
let as_pane value (key, path) =
  if value.graph_pane = Some key then value else
  let pane = match List.assoc_opt key value.graph_panes with
    | Some pane -> pane
    | None -> { view = Pxui_graph.Scope.create (); laid = None; shown = None; route = [];
                views = start_views value.doc value.level path; at = value.level; picked = Selection.empty } in
  let level = Document.resolve_level value.doc pane.at in
  { value with graph_pane = Some key; graph_at = Some path; scope_view = pane.view; scope_key = pane.laid;
               pane_graph = pane.shown; back = pane.route; projections = pane.views;
               level; selection = if level = pane.at then pane.picked else Selection.empty }

(* The graph leaf an inspector, list or lisp leaf is tied to by [:of]. *)
let tied value path =
  if value.workspace.restored then None else
  Option.bind value.doc.Document.shell (fun s ->
    Option.map (fun target -> panel_key value.doc target, target) (List.assoc_opt path s.follows))

(* The pane in use follows the focus: the focused graph leaf, else the one it was, else the first
   one open.  The pane left keeps its canvas, selection, graph and route. *)
let follow_graph value ~focus ~focus_path =
  let all = graph_leaves value in
  let value = { value with graph_at = Option.bind value.graph_pane (fun key -> List.assoc_opt key all) } in
  let leaves = List.filter (fun (_, path) -> not (panel_state value path).collapsed) all in
  (* the focused graph leaf, or the one the focused leaf is tied to: its edits land there *)
  let focused = Option.bind focus_path (fun path ->
    let path = if focus = Pxui_shell.Layout.Graph then Some path else Option.map snd (tied value path) in
    Option.bind path (fun path -> List.find_opt (fun (_, p) -> p = path) leaves)) in
  let target = match focused, value.graph_pane with
    | Some leaf, _ -> Some leaf
    | None, Some key when List.mem_assoc key leaves -> None
    | None, _ -> List.nth_opt leaves 0 in
  match target, value.graph_pane with
  | None, _ -> value
  | Some (key, _), Some current when key = current -> value
  | Some (key, path), None ->  (* the first pane: the state so far is its *)
      { value with graph_pane = Some key; graph_at = Some path }
  | Some leaf, Some current ->
      let left = (current, stash value) :: List.remove_assoc current value.graph_panes in
      let value = as_pane { value with graph_pane = None; graph_panes = left } leaf in
      { value with graph_panes = List.filter (fun (key, _) -> List.mem_assoc key all
                                                              && Some key <> value.graph_pane) left }

(* The inspector column's kit panel. *)
(* A window is the input sheet: its panes paint their ground with the theme's panel colour, so
   they are built with the sheet as that colour. *)
let sheet_theme (theme : Pxui.Theme.t) =
  { theme with panel = theme.input; ink_ground = Some (Option.value theme.ink_ground ~default:theme.panel) }
let on_sheet ui floating build =
  if not floating then build () else begin
    let theme = Pxui.Ui.theme ui in
    Pxui.Ui.set_theme ui (sheet_theme theme);
    Fun.protect ~finally:(fun () -> Pxui.Ui.set_theme ui theme) build
  end

let inspector_panel ?window ui bounds build =
  let x, y, width, height = bounds in
  Pxui.Ui.panel ?window ui ~x:(float_of_int x) ~y:(float_of_int y)
    ~width:(float_of_int width) ~height:(float_of_int height)
    ~padding:0 "workspace-inspector-panel" build

(* The inspector rows of a scene object or a World layer: its fields, a vec3 in one row. *)
let object_rows ?(locked = false) fields =
  Result.map (List.map (fun (parameter : Flow_sop.Port.parameter) ->
    { Pxui_shell.Inspector.path = parameter.path; fields = parameter.fields; shown = false;
      locked; drive = None; live = None }))
    (Flow_sop.Port.parameters ~infer:true fields)
