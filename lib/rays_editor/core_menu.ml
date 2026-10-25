open Editor_document
include Core_panels

(* What the add menu of a scene adds beside the object kinds: the World, a merge, and the
   geometry of each SOP graph (a second object over a graph that already exists) *)
let scene_entries value =
  let entry key label category =
    { Pxui_graph.Node_menu.key; label; category; arity = 0; context = "scene"; output = Flow.Ty.geometry;
      off = None } in
  entry "world" "World" [ "Object" ] :: entry "merge" "Merge" [ "Object" ]
  :: entry "material" "Material" [ "Material" ]
  :: List.map (fun c -> entry ("newgraph:" ^ c) ("New graph: " ^ c) [ "Graph" ]) Navigator.new_graph_contexts
  @ List.filter_map (fun (g : Flow.Workspace.graph) ->
       if g.context = Flow.Context.sop then Some (entry ("of:" ^ g.name) g.name [ "Object"; "Geometry of..." ])
       else None) (fst value.doc.Document.workspace).checked.graphs

(* In a SOP graph: a sop/material of each material graph, after the selection *)
let material_entries value =
  List.filter_map (fun (g : Flow.Workspace.graph) ->
    if g.context = Flow.Context.material then
      Some { Pxui_graph.Node_menu.key = "of-material:" ^ g.name; label = g.name;
             category = [ "Material of..." ]; arity = 1; context = "sop"; output = Flow.Ty.geometry;
             off = None }
    else None) (fst value.doc.Document.workspace).checked.graphs

(* The graph a menu entry is for, as the entries name it *)
let context_name = Flow.Workspace.context_name

(* The kinds the node menu offers where the pane shows [graph], at a screen point; the kinds of the
   other graphs follow in the search, in ink-3, saying that they are not placed here.  The title says
   which node the new one goes after. *)
let open_menu ?scope_view value (x, y) =
  let scope_view = Option.value scope_view ~default:value.scope_view in
  match add_target value with
  | Some (_, context) ->
      let module M = Pxui_graph.Node_menu in
      let extra = (fst value.doc.workspace).checked.ops in
      let factories context = List.map (fun (d : Flow_sop.Catalog.descriptor) -> M.{
        key = d.key; label = d.label; category = d.category;
        arity = List.length d.slots - List.length d.keyword_inputs;
        context = context_name context; output = Flow.Ty.geometry; off = None }) (kinds value context) in
      let not_here entries =
        List.map (fun (e : M.entry) -> { e with off = Some ("not in " ^ context_name context) }) entries in
      let elsewhere =
        (if context = Flow.Context.scene then [] else not_here (scene_entries value))
        @ List.concat_map (fun other -> if other = context then [] else not_here (factories other))
            [ Flow.Context.scene; Flow.Context.world; Flow.Context.sop ] in
      let after = match Pxui_graph.Scope.selected scope_view with
        | [ path ] when List.length path >= 2 -> binding_name path
        | _ -> None in
      Some (M.create ?after ~x ~y
        (factories context
         @ (if context = Flow.Context.value then value_entries else [])
         @ M.of_ops ~extra context
         @ (if context = Flow.Context.scene then scene_entries value else [])
         @ (if context = Flow.Context.sop then material_entries value else []) @ value_entries
         @ M.of_ops ~extra Flow.Context.value
         @ elsewhere))
  | None -> None
