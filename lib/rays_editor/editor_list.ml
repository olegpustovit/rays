open Editor_model
open Rays
open Sop
open Editor_document
open Editor_inspect
open Editor_scope
open Editor_panes
open Editor_add

let enterable value id =
  match kind value id with
  | Some "geometry" -> Document.Int_map.mem id value.doc.Document.networks
  | Some "world" -> Document.Int_map.mem id value.doc.Document.layers
  | Some _ | None -> false

(* Switch the open level: nothing is selected there, and the list starts again. *)
let open_level value level =
  if level = pane_level value then value
  else
    let () = Pxui.Ui.unfocus value.ui in
    let world =
      match level with Document.Inside id -> kind value id = Some "world" | Scene -> false
    in
    with_pane
      { value with map_view = world; tree = Pxui_shell.Tree.create () }
      (fun pane -> { pane with at = level; shown = None; picked = Selection.empty })

(* The geometry objects to cook: visible ones, each with its network. *)
let geometry_objects value =
  (* a workspace without a scene graph shows each graph the lowering made a network of; an image
     graph is one (the canvas previews its nodes) but its payload is not geometry, so the geometry
     cook never takes it (Flow_native.Workspace_images cooks images) *)
  let ws = value.doc.Document.workspace.ws in
  let image_graph (node : Kind_graph.node) =
    List.exists
      (fun (g : Flow.Workspace.graph) ->
        g.name = node.label && Flow.Context.result g.context = Flow.Ty.image)
      ws.checked.graphs
  in
  List.filter_map
    (fun id ->
      match
        (Kind_graph.find (scene value) id, Document.Int_map.find_opt id value.doc.Document.networks)
      with
      | Some node, Some network when Objects.visible node && not (image_graph node) -> (
          match Document.Int_map.find_opt id value.viewed with
          | Some (graph, displayed) -> Some (id, graph, displayed)
          | None ->
              Option.map
                (fun displayed -> (id, network.Document.graph, displayed))
                network.displayed)
      | _ -> None)
    (Objects.ids "geometry" (scene value))

(* The object the sketch-facing single-object accessors describe: the open
   one, else the first visible geometry object. *)
let focus_object value =
  match pane_level value with
  | Inside id when kind value id = Some "geometry" -> Some id
  | Inside _ | Scene -> Option.map (fun (id, _, _) -> id) (List.nth_opt (geometry_objects value) 0)

let pieces value = value.cook.Cook.pieces

let piece value =
  Option.bind (focus_object value) (fun id ->
      List.find_opt (fun (piece : Cook.piece) -> piece.id = id) (pieces value))

let prepared value = Option.map (fun (piece : Cook.piece) -> piece.prepared) (piece value)

(* The object in focus (the selected scene object, or the one whose network is open): its world
   transform, its cooked bounds in its own space, and its name. *)
let selected_bounds value =
  Option.bind (piece value) (fun (piece : Cook.piece) ->
      Option.bind piece.bounds (fun bounds ->
          Option.map
            (fun (node : Kind_graph.node) ->
              (Objects.world (scene value) piece.id, bounds, node.label))
            (Kind_graph.find (scene value) piece.id)))

let no_graph =
  Custom.plain ~operation:"points" ~parameters:"count=0" ~cook_mode:Node.Generator []
    (fun ~context:_ _ -> Ok Rdk_rays.Geometry_view.points_of_nothing)

let graph value =
  match
    ( Option.bind (focus_object value) (fun id -> List.assoc_opt id value.cook.Cook.graphs),
      piece value )
  with
  | Some graph, _ -> graph
  | None, Some piece -> piece.graph
  | None, None -> no_graph

let displayed_node = graph
let settings value = value.doc.Document.settings
let timeline value = value.timeline

let selected_node value =
  match scope_node value with
  | Some (_, node) -> Some (of_sop node)
  | None ->
      Option.bind
        (Selection.selected (pane_selection value))
        (fun id -> Option.map of_object (Kind_graph.find (objects value) id))

(* Scoped SOP handles use their owning object's transform. Navigation
   selection uses the open object or the scene object's parent chain. *)
let space value =
  match Option.bind (scope_node value) (fun (_, node) -> node_owner value (Node.id node)) with
  | Some owner -> Objects.world (scene value) owner
  | None -> (
      match pane_level value with
      | Inside id when kind value id = Some "geometry" -> Objects.world (scene value) id
      | Inside _ -> Rays_math.Mat4.identity
      | Scene -> (
          match Selection.selected (pane_selection value) with
          | Some id -> Objects.parent_world (scene value) id
          | None -> Rays_math.Mat4.identity))

(* ---- the graph as a list ---- *)

(* A row's kind letter and colour (the prototype's palette). *)
let badge operation =
  let hex = Rays_math.Color.hex_exn in
  match operation with
  | "geometry" -> ("G", hex "#3f7a52")
  | "light" -> ("L", hex "#b07a1c")
  | "camera" -> ("C", hex "#5a4f86")
  | "world" -> ("W", hex "#285f77")
  | "sun" -> ("✦", hex "#b07a1c")
  | "gradient" | "sky" | "shape" | "scatter" | "room" -> ("·", hex "#6d8a95")
  | _ -> ("S", hex "#566463")

(* The objects as a tree by parent, siblings in id order. *)
let scene_rows ?active (objects : Kind_graph.t) =
  let children = Hashtbl.create 16 in
  List.iter
    (fun (node : Kind_graph.node) ->
      Hashtbl.replace children node.input
        (node :: Option.value ~default:[] (Hashtbl.find_opt children node.input)))
    objects;
  let sorted parent =
    List.sort
      (fun (a : Kind_graph.node) b -> Int.compare a.id b.id)
      (Option.value ~default:[] (Hashtbl.find_opt children parent))
  in
  let rec emit depth (node : Kind_graph.node) =
    let flags =
      List.filter_map
        (fun name -> if Objects.has_flag name node then Some (Objects.flag name node) else None)
        [ "visible"; "render" ]
    in
    {
      Pxui_shell.Tree.id = node.id;
      depth;
      label = node.label;
      detail = (if active = Some node.id then "* render cam" else node.kind);
      badge = badge node.kind;
      link = false;
      ghost = false;
      flags;
    }
    :: List.concat_map (emit (depth + 1)) (sorted (Some node.id))
  in
  Array.of_list (List.concat_map (emit 0) (sorted None))

(* The World's layers, bottom first. *)
let layer_rows (layers : Kind_graph.t) =
  Array.of_list
    (List.rev_map
       (fun (node : Kind_graph.node) ->
         {
           Pxui_shell.Tree.id = node.id;
           depth = 0;
           link = false;
           ghost = false;
           label = node.label;
           detail = node.kind;
           badge = badge node.kind;
           flags = (if Objects.has_flag "visible" node then [ Objects.flag "visible" node ] else []);
         })
       (Kind_graph.stack layers))

(* The network as list rows [(id, depth, link)], sources first: rows follow each node's first
   input, other inputs nest one level under the node that consumes them, and a node reached
   twice repeats as a [link] row. *)
let trunk (network : Flow_sop.Network.t) =
  let infos = Edit_graph.inspect network.geometry in
  let consumed = Hashtbl.create 64 and inputs = Hashtbl.create 64 in
  List.iter
    (fun (info : Edit_graph.node_info) ->
      Hashtbl.replace inputs info.id (Array.to_list info.inputs);
      Array.iter (Option.iter (fun source -> Hashtbl.replace consumed source ())) info.inputs)
    infos;
  let ids = List.map (fun (info : Edit_graph.node_info) -> info.id) infos in
  let emitted = Hashtbl.create 64 and rows = ref [] in
  let first id =
    match Hashtbl.find_opt inputs id with Some slots -> List.find_map Fun.id slots | None -> None
  in
  let rec emit_trunk id depth =
    (* The chain up the first inputs, stopping at a node already listed. *)
    let rec chain id acc =
      if Hashtbl.mem emitted id then (Some id, acc)
      else
        match first id with
        | Some source when Hashtbl.mem inputs source -> chain source (id :: acc)
        | _ -> (None, id :: acc)
    in
    let shared, nodes = chain id [] in
    Option.iter (fun id -> rows := (id, depth, true) :: !rows) shared;
    List.iter
      (fun id ->
        Hashtbl.replace emitted id ();
        rows := (id, depth, false) :: !rows;
        let primary_seen = ref false in
        List.iter
          (function
            | Some side when Hashtbl.mem inputs side ->
                if not !primary_seen then primary_seen := true
                else if Hashtbl.mem emitted side then rows := (side, depth + 1, true) :: !rows
                else emit_trunk side (depth + 1)
            | _ -> ())
          (Option.value ~default:[] (Hashtbl.find_opt inputs id)))
      nodes
  in
  List.iter (fun id -> if not (Hashtbl.mem consumed id) then emit_trunk id 0) ids;
  Array.of_list (List.rev !rows)

let trunk_rows (network : Flow_sop.Network.t) ~viewed ~flags =
  Array.map
    (fun (id, depth, link) ->
      let node = Edit_graph.find network.Flow_sop.Network.geometry ~node_id:id in
      let label = match node with Some node -> Node.label node | None -> "" in
      let operation = match node with Some node -> Node.operation node | None -> "" in
      let details = [ operation ] @ if viewed = Some id then [ "VIEW" ] else [] in
      {
        Pxui_shell.Tree.id;
        depth;
        link;
        ghost = false;
        label;
        detail = String.concat " · " details;
        badge = badge operation;
        flags = flags id node;
      })
    (trunk network)

(* The open level's list: what it is made from, and its rows and flag columns when asked. *)
let rows value =
  match (pane_level value, network value) with
  | Document.Scene, _ ->
      ( Objects (scene value),
        lazy (scene_rows ?active:value.doc.active_camera (scene value), [ "v"; "r" ]) )
  | Inside _, Some network ->
      ( Network network.graph,
        lazy
          ( trunk_rows network.graph ~viewed:network.displayed ~flags:(fun node_id node ->
                match node with Some _ -> [ Some node_id = network.displayed ] | None -> []),
            [ "d" ] ) )
  | Inside _, None -> (Objects (objects value), lazy (layer_rows (objects value), [ "v" ]))

(* A layer chain in stack order, bottom first. *)
let restack (layers : Kind_graph.t) order =
  snd
    (List.fold_left
       (fun (below, layers) id ->
         (Some id, Kind_graph.update layers id (fun node -> { node with input = below })))
       (None, layers) order)

let apply_parameter objects id name value =
  match Kind_graph.find objects id with
  | Some node when Objects.has_flag name node -> (
      match Kind_graph.set_fields objects id [ (name, value) ] with
      | Ok (objects, _) -> objects
      | Error _ -> objects)
  | Some _ | None -> objects

(* The layer stack a World list's move asks for, in the rows' order. *)
let restacked rows intent =
  let module T = Pxui_shell.Tree in
  let order =
    Array.to_list rows
    |> List.filter_map (fun (row : T.row) -> if row.link then None else Some row.id)
  in
  let move_in ids target drop =
    let rest = List.filter (fun id -> not (List.mem id ids)) order in
    List.concat_map
      (fun id ->
        if id <> target then [ id ]
        else match drop with T.Before -> ids @ [ id ] | After | Inside -> id :: ids)
      rest
  in
  match intent with
  | T.Move { ids; target; drop } -> Some (move_in ids target drop)
  | Reorder { ids; delta } ->
      Option.bind
        (List.find_index (fun id -> List.mem id ids) order)
        (fun index ->
          Option.map
            (fun target -> move_in ids target (if delta < 0 then T.Before else After))
            (List.nth_opt
               (List.filter (fun id -> not (List.mem id ids)) order)
               (max 0 (index + delta))))
  | _ -> None

(* What a scene list's move asks: the objects and the parent they go under (none: the root). *)
let reparented document rows intent =
  let module T = Pxui_shell.Tree in
  match intent with
  | T.Move { ids; target; drop } ->
      Some
        ( ids,
          match drop with
          | T.Inside -> Some target
          | Before | After -> Objects.parent document target )
  | Indent (id :: _ as ids) ->
      (* Under the previous sibling row. *)
      let index = ref None in
      Array.iteri (fun k (row : T.row) -> if row.id = id && !index = None then index := Some k) rows;
      Option.bind !index (fun k ->
          let depth = rows.(k).depth in
          let found = ref None in
          for candidate = k - 1 downto 0 do
            if !found = None && rows.(candidate).depth = depth then
              found := Some rows.(candidate).id
            else if rows.(candidate).depth < depth then found := Some (-1)
          done;
          match !found with Some id when id >= 0 -> Some (ids, Some id) | _ -> None)
  | _ -> None

(* Tree intents become selection, an entry request, and edits of the derived network for the
   objects only the host made (reconcile adopts them): what the text has is edited as text
   ([tree_edit]). *)
let apply_tree value (document, selection, tree, opened, label, rows) intent =
  let module T = Pxui_shell.Tree in
  let in_text = Editor_document.Scene_sync.in_text value.doc (pane_level value) in
  let hosted = List.filter (fun id -> not (in_text id)) in
  let world =
    match pane_level value with
    | Document.Inside id -> kind value id = Some "world"
    | Scene -> false
  in
  (* the nodes of a geometry object are the lowering of its graph: its list only selects *)
  let geometry =
    match pane_level value with
    | Document.Inside id -> kind value id = Some "geometry"
    | Scene -> false
  in
  let document, selection, tree, opened, label, rows =
    match intent with
    | T.Select ids ->
        ( document,
          (match ids with [] -> Selection.clear | ids -> Selection.select_nodes ids),
          tree,
          opened,
          label,
          rows )
    | Flag _ when geometry ->
        (* the display node of a workspace graph is its result *)
        (document, selection, tree, opened, label, rows)
    | Flag { ids; column; value = on } ->
        let name = List.nth (if world then [ "visible" ] else [ "visible"; "render" ]) column in
        let document =
          List.fold_left
            (fun document id -> apply_parameter document id name (Param.Bool_value on))
            document (hosted ids)
        in
        (document, selection, tree, opened, Some (if on then "Show" else "Hide"), rows)
    | (Move _ | Reorder _) when world -> (
        match restacked rows intent with
        | Some order ->
            ( (if List.for_all in_text order then document else restack document order),
              selection,
              tree,
              opened,
              Some "Reorder layers",
              rows )
        | None -> (document, selection, tree, opened, label, rows))
    | (Move _ | Indent _) when pane_level value = Document.Scene -> (
        match reparented document rows intent with
        | Some (ids, parent) ->
            ( Objects.reparent document (hosted ids) parent,
              selection,
              tree,
              opened,
              Some "Reparent",
              rows )
        | None -> (document, selection, tree, opened, label, rows))
    | Outdent ids when pane_level value = Document.Scene ->
        (Objects.outdent document (hosted ids), selection, tree, opened, Some "Reparent", rows)
    | Rename (id, _) when in_text id -> (document, selection, tree, opened, Some "Rename", rows)
    | Rename (id, name) when not geometry -> (
        match Kind_graph.find document id with
        | Some _ ->
            ( Kind_graph.update document id (fun node -> { node with label = name }),
              selection,
              tree,
              opened,
              Some "Rename",
              rows )
        | None -> (document, selection, tree, opened, label, rows))
    | Activate _ when geometry -> (document, selection, tree, opened, label, rows)
    | Activate id -> (document, selection, tree, Some id, label, rows)
    | Delete ids when not geometry ->
        let document = Kind_graph.remove (hosted ids) document in
        (document, Selection.clear, tree, opened, Some "Delete", rows)
    | Rename _ | Delete _ | Move _ | Indent _ | Outdent _ | Reorder _ ->
        (document, selection, tree, opened, label, rows)
  in
  (document, selection, tree, opened, label, rows)

(* A list intent as an edit of the text, for the objects the text has ([Scene_sync.write]). *)
let tree_edit value rows intent : Editor_document.Scene_sync.edit option =
  let module T = Pxui_shell.Tree in
  let in_text = Editor_document.Scene_sync.in_text value.doc (pane_level value) in
  let some ids (make : int list -> Editor_document.Scene_sync.edit) =
    match List.filter in_text ids with [] -> None | ids -> Some (make ids)
  in
  let world =
    match pane_level value with
    | Document.Inside id -> kind value id = Some "world"
    | Scene -> false
  in
  let lowered =
    match pane_level value with
    | Document.Inside id -> kind value id = Some "geometry"
    | Scene -> false
  in
  let graph = objects value in
  let edit =
    if lowered then None
    else
      match intent with
      | T.Flag { ids; column; value = on } ->
          let name = List.nth (if world then [ "visible" ] else [ "visible"; "render" ]) column in
          some
            (List.filter
               (fun id ->
                 match Kind_graph.find graph id with
                 | Some node -> Objects.has_flag name node
                 | None -> false)
               ids)
            (fun ids -> Fields (List.map (fun id -> (id, [ (name, Param.Bool_value on) ])) ids))
      | Rename (id, name) -> some [ id ] (fun _ -> Rename (id, name))
      | Delete ids -> some ids (fun ids -> Delete ids)
      | (Move _ | Reorder _) when world ->
          Option.bind (restacked rows intent) (fun order ->
              if List.for_all in_text order then Some (Editor_document.Scene_sync.Restack order)
              else None)
      | (Move _ | Indent _) when pane_level value = Document.Scene ->
          Option.bind (reparented graph rows intent) (fun (ids, parent) ->
              some ids (fun ids -> Reparent (ids, parent)))
      | Outdent ids when pane_level value = Document.Scene -> some ids (fun ids -> Outdent ids)
      | _ -> None
  in
  edit
