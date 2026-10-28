let cook_effects = Param.add_impact Param.Cook Param.no_effects

(* One gesture on a workspace document: rewrite and re-check the source, then
   lower it into the document's objects.  Atomic: an error changes nothing. *)
let edit_result ~apply ~factories (doc : Document.t) op =
  let ( let* ) = Result.bind in
  let missing graph = not (List.exists (fun (g : Flow.Workspace.graph) -> g.name = graph)
    (doc.workspace.ws).checked.graphs) in
  let run (doc : Document.t) =
    let* catalog = Kinds.catalog ~version:Flow_sop.Manifest.version factories in
    let* workspace = apply catalog (doc.workspace.ws) op in
    Contexts.of_workspace ~factories ~previous:doc workspace in
  (* a gesture on the scene the host composes (the text has no such graph yet, so the edit finds
     nothing to change): the host's own objects are written out first, and the gesture is tried
     on that text.  Kept only when it then checks. *)
  let adopted () = Result.bind (Result.map_error (Diagnostic.error ~code:"E_EDIT")
    (Scene_sync.adopt ~factories doc)) run in
  match run doc with
  | Ok _ as done_ -> done_
  | Error _ as refused ->
      (match (if missing "scene" then Result.to_option (adopted ()) else None) with
       | Some doc -> Ok doc | None -> refused)

let syntax_edit_result ?(factories = Sop.Nodes.factories) doc op = edit_result ~apply:Workspace_doc.edit ~factories doc op

(* a layout gesture is applied like a text one, and is atomic the same way *)
let layout_op_result ?(factories = Sop.Nodes.factories) doc op = edit_result ~apply:Workspace_doc.edit_layout ~factories doc op

let layout_op ?(factories = Sop.Nodes.factories) doc op = Result.map_error Diagnostic.to_string (layout_op_result ~factories doc op)

let syntax_edit ?(factories = Sop.Nodes.factories) (doc : Document.t) op =
  Result.map_error Diagnostic.to_string (syntax_edit_result ~factories doc op)

(* Several rewrites, one gesture: all or none.  The host's own objects are written out first
   when the batch adds to a scene graph the document does not have yet. *)
let syntax_batch ?(factories = Sop.Nodes.factories) (doc : Document.t) ops =
  let adds_to_scene = List.exists (function
    | Flow_graph.Flow_edit.Add_node { scope = [ "scene" ]; _ } -> true | _ -> false) ops in
  let missing = not (List.exists (fun (g : Flow.Workspace.graph) -> g.name = "scene")
    (doc.workspace.ws).checked.graphs) in
  let first = if adds_to_scene && missing then Scene_sync.adopt ~factories doc
    else Ok doc in
  List.fold_left (fun doc op -> Result.bind doc (fun doc -> syntax_edit ~factories doc op))
    (Result.map_error Fun.id first) ops

(* The whole workspace text, edited: parsed and checked as a
   document, lowered, atomic. *)
let text_edit ?imports ?(factories = Sop.Nodes.factories) (doc : Document.t) text =
  let workspace = doc.workspace.ws in
  let ( let* ) = Result.bind in
  let one result = Result.map_error (fun d -> [ d ]) result in
  let* catalog = one (Kinds.catalog ~version:Flow_sop.Manifest.version factories) in
  let* edited = Workspace_doc.of_text ~imports:(Option.value imports
      ~default:(Workspace_doc.import_texts workspace))
      ~ops:workspace.checked.ops ~settings:workspace.settings
      ~layout:workspace.layout ~inputs:workspace.inputs catalog text in
  one (Contexts.of_workspace ~factories ~previous:doc edited)

(* A change to the workspace's layout keys (a moved item, a collapsed zone):
   the source and the lowering are untouched, so nothing recooks. *)
let layout_edit (doc : Document.t) f =
  let ws = doc.workspace.ws in
  { doc with workspace = { doc.workspace with ws = { ws with layout = f ws.layout } } }

let select_layout ?(factories = Sop.Nodes.factories) (doc : Document.t) name =
  let workspace = doc.workspace.ws in
  let workspace = { workspace with layout = { workspace.layout with editor = Some name } } in
  match Workspace_doc.editor_graph workspace with
  | None -> Error ("Unknown editor layout " ^ name ^ ".")
  | Some _ -> Contexts.of_workspace ~factories ~previous:doc workspace
      |> Result.map_error Diagnostic.to_string

(* A derived edit (an object's field, a reparent, a World layer) written to the text. *)
let reconcile ?(factories = Sop.Nodes.factories) ?adopt before after =
  Scene_sync.reconcile ~factories ?adopt before after

(* The binding of the panel at tree [path] (an assignable node for a gesture): a named panel is
   its binding, one written in place is bound first (the call holding it unfolded). *)
let panel_node ?(factories = Sop.Nodes.factories) ?(loop_message = "These panels are copies made by a loop: edit the loop in the editor graph.")
    (doc : Document.t) path =
  let ( let* ) = Result.bind in
  let graph = Option.map (fun (g : Flow.Workspace.graph) -> g.name)
    (Workspace_doc.editor_graph (doc.workspace.ws)) in
  match graph, Option.bind doc.shell (fun s -> List.assoc_opt path s.Document.origins) with
  | Some graph, Some (Document.Bound name) -> Ok (doc, [ graph; name ])
  | _, Some (Inline (home, key)) ->
      let* doc, node = Scene_sync.bind_home ~factories doc home in
      let* doc = syntax_edit ~factories doc (Flow_graph.Flow_edit.Unfold { node; key; sub = [] }) in
      (match Flow_graph.Flow_edit.arg_text (doc.workspace.ws).source node key with
       | Some { Flow.Syntax.node = Sym name; _ } -> Ok (doc, List.rev (name :: List.tl (List.rev node)))
       | _ -> Error "The panel could not be named.")
  | _, Some (Loop _) -> Error loop_message
  | _ -> Error "This panel is not part of an editor layout."
