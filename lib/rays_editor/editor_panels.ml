open Editor_model
open Rays
open Sop
open Editor_document
open Editor_shell
open Editor_panes
open Editor_inspect
open Editor_status
open Editor_text
open Editor_list
open Editor_add

(* A panel header's title: its type, and where a looped panel comes from (register E1). *)
let panel_title value (leaf : Pxui_shell.Layout.leaf) =
  (* a graph panel's title is its own pane's *)
  let value = shown_as value (panel_key value.doc leaf.path, leaf.path, leaf.panel) in
  let graph = Option.value ~default:"" (graph_name value) in
  let name, sub =
    match leaf.panel with
    | Canvas _ -> ("Canvas", "")
    | View _ ->
        ( "Viewport",
          (* the scene it shows and its render camera: "scene / camera" *)
          let scene_name =
            Option.value ~default:""
              (List.find_map
                 (fun (g : Flow.Workspace.graph) ->
                   if g.context = Flow.Context.scene then Some g.name else None)
                 value.doc.Document.workspace.ws.checked.graphs)
          in
          let camera =
            Option.bind value.doc.Document.active_camera (fun id ->
                Option.map
                  (fun (node : Kind_graph.node) -> node.label)
                  (Kind_graph.find (scene value) id))
          in
          String.concat " / "
            (List.filter (( <> ) "") [ scene_name; Option.value ~default:"" camera ]) )
    | Graph ->
        ( "Graph",
          if graph = "" then ""
          else
            let context =
              List.find_map
                (fun (g : Flow.Workspace.graph) ->
                  if g.name = graph then Some (Flow.Workspace.context_name g.context) else None)
                value.doc.Document.workspace.ws.checked.graphs
            in
            (if pane_route value = [] then graph
             else
               String.concat " / " (List.map String.trim (String.split_on_char '>' (route value))))
            ^ Option.fold ~none:"" ~some:(fun c -> " / " ^ c) context )
    | List -> ("List", level_name value)
    | Lisp ->
        ( "Lisp",
          if
            let _, _, w, _ = leaf.frame in
            w < 400
          then "" (* the narrow sheet has the kind alone *)
          else if graph = "" then value.file
          else if value.file = "" then graph
          else value.file ^ " / " ^ graph )
    | Inspector ->
        ( "Inspector",
          match
            if pane_laid value = None then []
            else inspector_paths (Pxui_graph.Scope.selected (pane_view value))
          with
          | [ path ] when graph <> "" ->
              (* the node's title (a result is `result`, not `@result`); the narrow column has the node alone *)
              let title =
                match
                  Option.bind (pane_laid value) (fun (k : scope_key) ->
                      Flow_graph.Projection.find k.scope path)
                with
                | Some n -> Flow_graph.Projection.title n
                | None ->
                    let last = List.nth path (List.length path - 1) in
                    if String.starts_with ~prefix:":" last then
                      String.sub last 1 (String.length last - 1)
                    else last
              in
              if
                let _, _, w, _ = leaf.frame in
                w < 340
              then title
              else graph ^ " / " ^ title
          | _ -> graph )
    | Outline ->
        ( "Outline",
          if
            let _, _, w, _ = leaf.frame in
            w < 300
          then "" (* the narrow sheet has the kind alone *)
          else value.file )
    | Spreadsheet -> ("Spreadsheet", graph)
    | Timeline ->
        ( "Timeline",
          (* the real step of the sketch: the same step every frame is a fixed one *)
          if value.steady >= 3 && value.last_dt > 0. then
            Printf.sprintf "fixed dt 1/%d" (int_of_float (Float.round (1. /. value.last_dt)))
          else "realtime" )
  in
  let name = if sub = "" then name else name ^ "\t" ^ sub in
  match
    Option.bind value.doc.Document.shell (fun s ->
        if value.workspace.restored then None else List.assoc_opt leaf.path s.origins)
  with
  | Some (Document.Loop (from, _)) ->
      name ^ " · from loop " ^ Document.describe value.doc.Document.workspace.ws.source from
  | _ -> name

(* Make a reusable function from the selected nodes: the names they read from outside are typed from the
   graph pane's projection, the context is the result's (a geometry is [sop], anything else [value]). *)
let defn_change value paths =
  let ws = value.doc.Document.workspace.ws in
  let scope = Option.map (fun (k : scope_key) -> k.scope) (pane_laid value) in
  let rec ty_text : Flow.Ty.t -> string option = function
    | Flow.Ty.Named "geometry" -> Some "geometry"
    | Float -> Some "float"
    | Int -> Some "int"
    | Bool -> Some "bool"
    | Vec3 -> Some "vec3"
    | Text -> Some "text"
    | Fn _ -> Some "fn"
    | List t -> Option.map (fun s -> "(list " ^ s ^ ")") (ty_text t)
    | _ -> None
  in
  let rec find_ty name (s : Flow_graph.Projection.scope) =
    match List.find_opt (fun (i : Flow_graph.Projection.input) -> i.name = name) s.inputs with
    | Some i -> Some i.ty
    | None ->
        List.find_map
          (fun (n : Flow_graph.Projection.node) ->
            if List.mem name n.binds then Some n.ty
            else Option.bind n.zone (fun (z : Flow_graph.Projection.zone) -> find_ty name z.scope))
          s.nodes
  in
  match (Flow_graph.Flow_edit.defn_draft ws.source paths, scope) with
  | Error d, _ -> Declined d.Diagnostic.message
  | Ok _, None -> Declined "Open a graph to make a function from its nodes"
  | Ok draft, Some scope -> (
      let typed = List.map (fun n -> (n, Option.bind (find_ty n scope) ty_text)) draft.free in
      match List.find_opt (fun (_, t) -> t = None) typed with
      | Some (n, _) ->
          Declined (Printf.sprintf "The function reads %s, whose type a function cannot take." n)
      | None ->
          let result_ty =
            List.fold_left
              (fun acc path ->
                match Flow_graph.Projection.find scope path with Some n -> Some n.ty | None -> acc)
              None paths
          in
          Syntax_edit
            (Flow_graph.Flow_edit.Make_defn
               {
                 nodes = paths;
                 name = draft.name;
                 context = (if result_ty = Some Flow.Ty.geometry then "sop" else "value");
                 params = List.map (fun (n, t) -> (n, Option.get t)) typed;
               }))

(* The Navigator: what it shows of the document (see {!Navigator}). *)
(* The scene's objects as the Outline lists them: the list panel's rows, with what each is and
   the binding that places it. *)
let outline_objects value : Navigator.obj list =
  let document = scene value in
  let selected =
    if pane_level value = Document.Scene then Selection.selected_nodes (pane_selection value)
    else []
  in
  Array.to_list (scene_rows document)
  |> List.map (fun (row : Pxui_shell.Tree.row) ->
      let node = Kind_graph.find document row.id in
      let flag name =
        Option.bind node (fun node ->
            if Objects.has_flag name node then Some (Objects.flag name node) else None)
      in
      let lead = value.doc.Document.active_camera = Some row.id in
      (* a light says its shape, the render camera that it is active *)
      let choice =
        Option.bind node (fun node ->
            List.find_map
              (fun (f : Param.field_view) ->
                match (f.name, f.current) with
                | ("shape" | "type"), Param.Choice_value v -> Some (String.lowercase_ascii v)
                | _ -> None)
              (Kind_graph.fields node))
      in
      let operation = Option.map (fun (node : Kind_graph.node) -> node.kind) node in
      let field name =
        Option.bind node (fun node ->
            List.find_map
              (fun (f : Param.field_view) ->
                match (f.name = name, f.current) with
                | true, Param.Float_value v -> Some v
                | _ -> None)
              (Kind_graph.fields node))
      in
      (* the sheet's words: a camera its focal length, a geometry object the graph it references, the
       World a reference to its graph; a light its shape *)
      let name, detail =
        match operation with
        | Some "camera" ->
            ( row.label,
              match field "fov" with
              | Some fov when fov > 0. && fov < 180. ->
                  Printf.sprintf "%.0f mm" (12. /. Float.tan (fov *. Float.pi /. 360.))
              | _ -> row.detail )
        | Some "geometry" ->
            ( row.label,
              match graph_of_object value row.id with
              | Some graph -> "ref " ^ graph
              | None -> row.detail )
        | Some "world" -> ("world", "ref " ^ row.label)
        | _ -> (row.label, Option.value choice ~default:row.detail)
      in
      (* the render camera's mark is its flag (the accent); the World renders while it is in the scene;
       neither is a field a press can write *)
      let inert, render =
        match operation with
        | Some "camera" -> (true, Some lead)
        | Some "world" -> (true, Some true)
        | _ -> (false, flag "render")
      in
      (* the render camera has both flags, its visible one only showing *)
      let visible = match operation with Some "camera" -> Some true | _ -> flag "visible" in
      let rank =
        match operation with Some "camera" -> 0 | Some "light" -> 1 | Some "world" -> 4 | _ -> 2
      in
      ( rank,
        {
          Navigator.depth = row.depth;
          letter = fst row.badge;
          name;
          detail;
          visible;
          render;
          lead;
          inert;
          chosen = List.mem row.id selected;
          home =
            (match List.assoc_opt row.id value.doc.Document.homes.objects with
            | Some (Document.Bound_at path) -> Some path
            | _ -> None);
        } ))
  |> fun rows ->
  (* the sheet's order: the camera, the lights, the geometry, the World; a nested scene keeps its tree *)
  if List.for_all (fun (_, (o : Navigator.obj)) -> o.depth = 0) rows then
    List.map snd (List.stable_sort (fun (a, _) (b, _) -> compare a b) rows)
  else List.map snd rows

let navigator_params value : Navigator.params =
  let ws = value.doc.Document.workspace.ws in
  let active = graph_name value in
  let here =
    match (pane_laid value, active) with
    | Some k, Some name when k.graph = name -> Some k
    | _ -> None
  in
  {
    workspace = ws.checked;
    active;
    scope = Option.map (fun (k : scope_key) -> k.scope) here;
    records = Option.bind here (fun (k : scope_key) -> k.records);
    probes = (fun p -> Option.value ~default:0 (Layout_by_path.Path_map.find_opt p value.probes));
    selected = Pxui_graph.Scope.selected (pane_view value);
    chips =
      (match pane_laid value with Some { evaluated = Some ev; _ } -> Navigator.chips ev | _ -> []);
    notes =
      (match pane_laid value with Some { evaluated = Some ev; _ } -> Navigator.notes ev | _ -> []);
    objects = outline_objects value;
    root_detail = Printf.sprintf "%d spp" (Objects.Root.render value.doc.Document.root).max_spp;
    layouts =
      Option.map
        (fun (sw : Document.switch) -> (List.map Editor_core.Panels.summary sw.layouts, sw.active))
        (Option.bind value.doc.Document.shell (fun s -> s.switch));
  }

(* The leaf the graph pane in use draws in, when it is open. *)
let graph_leaf value (g : Pxui_shell.Layout.geometry) =
  Option.bind (graph_path value) (fun path ->
      List.find_opt
        (fun (l : Pxui_shell.Layout.leaf) ->
          l.path = path && l.panel = Graph
          &&
          let _, _, _, h = l.body in
          h > 0)
        g.leaves)

(* Chrome emits document edits; splitter drafts stay local until release. A
   refused structural edit returns a notice and preserves the document. *)
(* The focused leaf: the one clicked when its kind is shown twice, else the first of its kind. *)
let focused_leaf (g : Pxui_shell.Layout.geometry) focus path =
  match
    List.find_opt
      (fun (l : Pxui_shell.Layout.leaf) -> Some l.path = path && l.panel = focus)
      g.leaves
  with
  | Some leaf -> Some leaf
  | None ->
      (* the tree changed under the focus: a split moves the leaf a level down (and a viewport's
         key follows its path), a retype leaves another kind at the path.  The leaf of that kind
         under the old path, else the leaf now at the path, else the first of the kind. *)
      let kind (l : Pxui_shell.Layout.leaf) =
        Editor_commands.scope l.panel = Editor_commands.scope focus
      in
      let rec under prefix path =
        match (prefix, path) with
        | [], _ -> true
        | a :: p, b :: q -> a = b && under p q
        | _ :: _, [] -> false
      in
      let first f = List.find_opt f g.leaves in
      let ( <|> ) a b = match a with Some _ -> a | None -> b () in
      ( ( ( Option.bind path (fun p -> first (fun l -> kind l && under p l.path)) <|> fun () ->
            first (fun l -> Some l.path = path) )
        <|> fun () -> Pxui_shell.Layout.find g focus )
      <|> fun () -> first kind )
      (* its panel was closed and no other is of its kind: the leaf that shares the most of its
         path, the neighbour that took its place *)
      <|> fun () ->
      Option.bind path (fun p ->
          let rec shared a b =
            match (a, b) with x :: a, y :: b when x = y -> 1 + shared a b | _ -> 0
          in
          List.fold_left
            (fun best (l : Pxui_shell.Layout.leaf) ->
              match best with
              | Some (b : Pxui_shell.Layout.leaf) when shared p b.path >= shared p l.path -> best
              | _ -> Some l)
            None g.leaves)

let panel_kind : Pxui_shell.Layout.panel -> string = function
  | Canvas _ -> "canvas"
  | View _ -> "viewport"
  | Graph -> "graph"
  | List -> "list"
  | Lisp -> "lisp"
  | Inspector -> "inspector"
  | Outline -> "outline"
  | Timeline -> "timeline"
  | Spreadsheet -> "spreadsheet"

(* / [ and / n: the layouts of the switch and the floating windows, each one edit of the
   editor graph (written from the layout shown first, when the document has none). *)
let layout_actions value (workspace : shell) ~(leaf : Pxui_shell.Layout.leaf option) actions =
  let graph =
    Option.map
      (fun (g : Flow.Workspace.graph) -> g.name)
      (Workspace_doc.editor_graph value.doc.Document.workspace.ws)
  in
  let edit make =
    match graph with
    | _ when workspace.restored ->
        [ Declined "The default layout is showing. / z returns to the editor graph." ]
    | Some graph -> [ Layout_edit (make graph) ]
    | None -> (
        let scene =
          Option.value ~default:"scene"
            (List.find_map
               (fun (g : Flow.Workspace.graph) ->
                 if g.context = Flow.Context.scene then Some g.name else None)
               value.doc.Document.workspace.ws.checked.graphs)
        in
        let text, _ =
          Bars.tree_text ~name:"editor" ~scene (shell_tree value { workspace with live = None })
        in
        match Flow.Syntax.parse text with
        | Ok [ form ] ->
            [
              Syntax_edit (Flow_graph.Flow_edit.Set_graph { name = "editor"; form });
              Layout_edit (make "editor");
            ]
        | _ -> [ Declined "The layout could not be written as an editor graph." ])
  in
  let window kind =
    match Panel_kinds.expr value.doc.Document.workspace.ws.source kind with
    | Ok panel -> edit (fun graph -> Layout_edit.Layout_window { graph; panel })
    | Error message -> [ Declined message ]
  in
  List.concat_map
    (function
      | Editor_commands.Layout_switch index
        when Option.is_some (Option.bind value.doc.Document.shell (fun s -> s.switch)) ->
          edit (fun graph -> Layout_edit.Set_layout { graph; index })
      | Layout_switch index -> (
          (* an older file: several editor graphs *)
          match List.nth_opt (layouts value) index with
          | Some (name, _) -> [ Select_layout name ]
          | None -> [])
      | Layout_new
        when Option.is_none (Option.bind value.doc.Document.shell (fun s -> s.switch))
             && layouts value <> [] -> (
          (* an older file with several editor graphs: they become the layouts of one switch first *)
          match graph with
          | Some graph -> [ Layout_edit (Merge_then_new { graph }) ]
          | None -> [])
      | Layout_new -> edit (fun graph -> Layout_edit.Layout_new { graph })
      | Layout_remove -> edit (fun graph -> Layout_edit.Layout_remove { graph })
      | Window_new panel -> window (panel_kind panel)
      | Peek -> (
          match peek_target value with
          | Some target -> window ("graph:" ^ target)
          | None -> [ Declined "Nothing selected to peek at" ])
      | Float_toggle -> (
          match leaf with
          | Some leaf -> edit (fun graph -> Layout_edit.Layout_float { graph; at = leaf.path })
          | None -> [])
      | _ -> [])
    actions

let layout_intents value (workspace : shell) intents =
  let base = shell_tree value { workspace with live = None } in
  let editor =
    Option.map
      (fun (g : Flow.Workspace.graph) -> g.name)
      (Workspace_doc.editor_graph value.doc.Document.workspace.ws)
  in
  let shell = if workspace.restored then None else value.doc.Document.shell in
  let edit path make =
    match (editor, shell) with
    | Some graph, Some s -> (
        match List.assoc_opt path s.origins with
        | Some (Document.Bound name) -> [ Layout_edit (make [ graph; name ]) ]
        | Some (Inline (home, key)) -> [ Layout_inline { home; key; make } ]
        | Some (Loop (home, key)) -> (
            (* a loop's panels are copies of its one template: retyping edits the template *)
            match make [] with
            | Layout_edit.Set_panel_kind { panel; _ } ->
                [
                  Layout_inline
                    {
                      home;
                      key;
                      make =
                        (fun p -> Layout_edit.Set_panel_kind { node = p @ [ "@result" ]; panel });
                    };
                ]
            | _ ->
                [
                  Declined
                    ("These panels are copies made by a loop in "
                    ^ Document.describe value.doc.Document.workspace.ws.source home
                    ^ ": retype them (/ o), or edit the loop in the editor graph.");
                ])
        | None -> [ Declined "This panel is not part of the editor graph's tree." ])
    | _ when workspace.restored ->
        [ Declined "The default layout is showing. / z returns to the editor graph." ]
    | _ -> (
        (* no editor graph: the layout shown is written as one first, then edited *)
        let scene =
          Option.value ~default:"scene"
            (List.find_map
               (fun (g : Flow.Workspace.graph) ->
                 if g.context = Flow.Context.scene then Some g.name else None)
               value.doc.Document.workspace.ws.checked.graphs)
        in
        let text, name_of = Bars.tree_text ~name:"editor" ~scene base in
        match (Flow.Syntax.parse text, name_of path) with
        | Ok [ form ], Some leaf ->
            [
              Syntax_edit (Flow_graph.Flow_edit.Set_graph { name = "editor"; form });
              Layout_edit (make [ "editor"; leaf ]);
            ]
        | _ -> [ Declined "This panel is not part of the layout." ])
  in
  (* a panel of [kind] for [make]: the form the editor's panel table writes, or why there is none *)
  let with_panel kind path make =
    match Panel_kinds.expr value.doc.Document.workspace.ws.source kind with
    | Ok panel -> edit path (make panel)
    | Error message -> [ Declined message ]
  in
  let beside path =
    match Editor_core.Panels.at path base with
    | Some (Leaf panel) -> Panel_kinds.beside (panel_kind panel)
    | _ -> Panel_kinds.beside ""
  in
  let save_state path state =
    let prefix =
      if editor <> None then []
      else
        let scene =
          Option.value ~default:"scene"
            (List.find_map
               (fun (g : Flow.Workspace.graph) ->
                 if g.context = Flow.Context.scene then Some g.name else None)
               value.doc.Document.workspace.ws.checked.graphs)
        in
        let text, _ = Bars.tree_text ~name:"editor" ~scene base in
        match Flow.Syntax.parse text with
        | Ok [ form ] -> [ Syntax_edit (Flow_graph.Flow_edit.Set_graph { name = "editor"; form }) ]
        | _ -> []
    in
    prefix @ [ Panel_state (path, state) ]
  in
  List.fold_left
    (fun ((w : shell), changes) -> function
      | Pxui_shell.Chrome.Toggle path ->
          let state = panel_state value path in
          (w, changes @ save_state path { state with collapsed = not state.collapsed })
      | Window (path, window) ->
          (w, changes @ save_state path { (panel_state value path) with window })
      | Window_drag (path, window, released) ->
          if released then
            ( { w with window_live = None },
              changes @ save_state path { (panel_state value path) with window = Some window } )
          else ({ w with window_live = Some (path, window) }, changes)
      | Dragging _ | Crumb _ -> (w, changes)
      | Dock_panel (source, target, side) -> (w, changes @ [ Dock_panels (source, target, side) ])
      | Resize { node; size } -> ({ w with live = Some (node, size) }, changes)
      | Settled -> (
          match w.live with
          | None -> (w, changes)
          | Some (node, size) ->
              let w = { w with live = None } in
              (w, changes @ edit node (fun node -> Layout_edit.Set_layout_size { node; size })))
      | Split_panel (path, axis) ->
          ( w,
            changes
            @ with_panel (beside path) path (fun panel node ->
                Layout_edit.Split_panel { node; axis; ratio = 0.5; panel }) )
      | Split_panel_at (path, axis, ratio, kind) ->
          ( w,
            changes
            @ with_panel (panel_kind kind) path (fun panel node ->
                Layout_edit.Split_panel { node; axis; ratio; panel }) )
      | Close_panel path -> (w, changes @ edit path (fun node -> Layout_edit.Close_panel { node }))
      | Retype_panel (path, kind) ->
          ( w,
            changes
            @ with_panel (panel_kind kind) path (fun panel node ->
                Layout_edit.Set_panel_kind { node; panel }) ))
    (workspace, []) intents

let dock_panels ~factories (doc : Document.t) source target side =
  let ( let* ) = Result.bind in
  let states = doc.workspace.ws.layout.panels in
  let source_key = panel_key doc source and target_key = panel_key doc target in
  let saved key =
    Option.value ~default:Editor_core.Panels.default_state
      (Layout_by_path.Path_map.find_opt key states)
  in
  let bind doc path =
    Editor_document.Document_edit.panel_node ~factories
      ~loop_message:"These panels are copies made by a loop: move their tile in the editor graph."
      doc path
  in
  let* doc, node = bind doc source in
  let* doc, target = bind doc target in
  let* doc =
    Editor_document.Document_edit.layout_op ~factories doc
      (Layout_edit.Dock_panel { node; target; side })
  in
  let copy_pos = if side = `Left || side = `Top then 3 else 2 in
  let copy =
    match Flow_graph.Flow_edit.arg_text doc.workspace.ws.source target (Pos copy_pos) with
    | Some { Flow.Syntax.node = Sym name; _ } -> List.rev (name :: List.tl (List.rev target))
    | _ -> target
  in
  Ok
    (Editor_document.Document_edit.layout_edit doc (fun layout ->
         let panels =
           layout.panels
           |> Layout_by_path.Path_map.remove source_key
           |> Layout_by_path.Path_map.remove target_key
           |> Layout_by_path.Path_map.add node { (saved source_key) with window = None }
           |> Layout_by_path.Path_map.add copy (saved target_key)
         in
         { layout with panels }))

(* A pane's request applied to the open level's objects or layers: a parameter or name of one the
   host made.  Text gestures ([Syntax_edit]) are applied to the whole document afterwards. *)
let apply_change (document, error, effects) = function
  | Set_parameter { node; path; value } -> (
      match Kind_graph.set_fields document node [ (path, value) ] with
      | Error message -> (document, Some message, effects)
      | Ok (document, changed) -> (document, None, Param.union_effects effects changed))
  | Rename { node; label } -> (
      match Kind_graph.find document node with
      | None -> (document, Some (Printf.sprintf "no object #%d" node), effects)
      | Some _ -> (Kind_graph.update document node (fun n -> { n with label }), None, effects))
  | Follow_source _ | Syntax_edit _ | Layout_edit _ | Layout_inline _ | Syntax_batch _
  | Syntax_inline _ | Select_layout _ | Panel_state _ | Dock_panels _ | Object_arg _ | Pin_row _
  | Notice _ | Declined _ ->
      (document, error, effects)

(* Command-C / X: the selected bindings as Lisp pairs ("name expr" per line) on the clipboard,
   the text a let* vector or the Lisp pane takes. *)
let copy_bindings value paths =
  let ws = value.doc.Document.workspace.ws in
  let lines =
    List.filter_map
      (fun path ->
        match Text_pane.binding ws.source path with
        | Some (_, v) ->
            Some
              (List.nth path (List.length path - 1)
              ^ " "
              ^ String.trim (fst (Flow.Lisp.print [ v ])))
        | None -> None)
      paths
  in
  match lines with
  | [] -> Declined "Nothing to copy"
  | lines -> (
      match Clipboard.set_text (String.concat "\n" lines) with
      | Ok () ->
          Notice
            (Printf.sprintf "Copied %d binding%s" (List.length lines)
               (if List.length lines = 1 then "" else "s"))
      | Error message -> Declined ("Clipboard: " ^ message))

(* The palette's "Copy workspace as Lisp": the text Command-S writes, on the clipboard. *)
let copy_workspace value =
  match Clipboard.set_text (Preset.text value.doc) with
  | Ok () -> (Info, "Copied the workspace as Lisp")
  | Error message -> (Refusal, "Clipboard: " ^ message)

(* Command-V: the clipboard's "name expr" pairs (or bare expressions, named by their head) become
   [Add_node]s in the selected scope ({!Text_pane.paste_ops}), one history entry. *)
let paste_bindings value =
  match (add_target value, Clipboard.get_text ()) with
  | None, _ -> [ Declined "Open a graph to paste into" ]
  | Some _, Error message -> [ Declined ("Clipboard: " ^ message) ]
  | Some (graph, _), Ok text -> (
      let ws = value.doc.Document.workspace.ws in
      let scope =
        match Pxui_graph.Scope.selected (pane_view value) with
        | [ path ] when List.length path >= 2 -> List.take (List.length path - 1) path
        | _ -> [ graph ]
      in
      (* one gesture: every binding lands or none does *)
      match Text_pane.paste_ops ws.source ~graph ~scope text with
      | Error message -> [ Declined message ]
      | Ok [ op ] -> [ Syntax_edit op ]
      | Ok ops -> [ Syntax_batch ("Paste", ops) ])

(* The one node the inspector shows for the pane's selection, if exactly one (a test hook) *)
let inspector_subject value =
  if pane_laid value = None then None
  else
    match inspector_paths (Pxui_graph.Scope.selected (pane_view value)) with
    | [ path ] -> Some path
    | _ -> None
