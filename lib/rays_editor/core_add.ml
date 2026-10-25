open Sop
open Editor_document
include Core_scope

(* the binding a selected path ends in, none for a keyword input, a result or a nested call *)
let binding_name path =
  let last = List.hd (List.rev path) in
  if last = "" || last.[0] = ':' || last.[0] = '@' || String.contains last '#' then None else Some last

(* Where a node is added from the list or the pane: the graph the level shows, else the scene
   or World graph that adding an object or a layer creates. *)
let add_target value =
  let ws, _ = value.doc.Document.workspace in
  match graph_name value with
  | Some name ->
      let context = match List.find_opt (fun (g : Flow.Workspace.graph) -> g.name = name) ws.checked.graphs with
        | Some g -> g.context | None -> Flow.Context.sop in
      Some (name, context)
  | None ->
      (match value.level with
       | Document.Scene -> Some ("scene", Flow.Context.scene)
       | Inside id when kind value id = Some "world" -> Some ("world", Flow.Context.world)
       | _ -> None)

(* The language's forms, which are not operators: the menu adds each as a card with a small
   working expression to edit in place.  Section of the Value category, name in the menu, the
   expression it starts as, the binding's name. *)
let forms = [
  "Frame", "state", "(state [previous 0.0] (+ previous (frame/dt)))", "state";
  "Branch", "if", "(if true 1.0 0.0)", "choice";
  "Branch", "cond", "(cond false 1.0 :else 0.0)", "choice";
  "Branch", "case", "(case 0 0 1.0 :else 0.0)", "choice";
  "Loop", "for", "(for [i (range 4)] i)", "each";
  "Loop", "fold", "(fold [total 0.0] [i (range 4)] (+ total i))", "folded";
  "Loop", "iterate", "(iterate 3 0.0 (fn [x] (+ x 1.0)))", "iterated";
  "Loop", "scan", "(scan [total 0.0] [i (range 4)] (+ total i))", "steps";
  "Loop", "sum", "(sum [i (range 4)] i)", "total";
  "Function", "fn", "(fn [x] x)", "function";
  "Function", "map", "(map (fn [x] x) (range 4))", "mapped";
  "Function", "filter", "(filter (fn [x] (> x 1)) (range 4))", "kept";
  "Function", "reduce", "(reduce (fn [total x] (+ total x)) 0 (range 4))", "reduced";
  "Function", "sort-by", "(sort-by (fn [x] x) (range 4))", "sorted";
  "Data", "list", "(list 1.0 2.0 3.0)", "items";
  "Data", "record", "{:a 1.0 :b 2.0}", "record";
  "Data", "let*", "(let* [a 1.0] a)", "scope" ]

(* The menu's "Value" entries: a value is a binding with an expression (a number, the time, an
   operator call), which the other nodes read by name ("=number", "=t", "=+", ...). *)
let value_entries =
  (* the colour of a value's square: what it makes, as far as its section says *)
  let entry ?output sub key label =
    let output = match output, sub with
      | Some ty, _ -> ty | None, "Compare" -> Flow.Ty.Bool | None, "Text" -> Text
      | None, "Convert" when key = "=int" -> Int | None, _ -> Float in
    { Pxui_graph.Node_menu.key; label; category = [ "Value"; sub ]; arity = 0; context = "value"; output;
      off = None } in
  let categorize name = (Option.get (Flow.Op.find name Flow.Context.value)).category in
  [ entry "Math" "=number" "Number"; entry "Math" "=t" "Time (t)"; entry ~output:Flow.Ty.Vec3 "Math" "=vec3" "Vector";
    entry "Text" "=text" "Text"; entry "Text" "=str" "str" ]
  @ List.map (fun (sub, form, _, _) ->
      entry ~output:(match sub with "Loop" | "Data" | "Function" -> Flow.Ty.List Flow.Ty.Float | _ -> Flow.Ty.Float)
        sub ("=" ^ form) form) forms
  @ List.filter_map (fun op -> if List.exists (fun (_, form, _, _) -> form = op) forms then None
      else Some (entry ?output:(if op="array/max_length" then Some Flow.Ty.Int else None) (categorize op) ("=" ^ op) op)) Flow.Workspace.value_ops

(* the expression and the name a "Value" entry makes *)
let value_expression ~ops ?wire context key =
  let module S = Flow.Syntax in
  let mk node = S.make node in
  let num n = mk (S.Num n) in
  let op = String.sub key 1 (String.length key - 1) in
  match op with
  | "number" -> mk (S.Num "1.0"), "value"
  | "t" -> mk (S.Sym "t"), "time"
  | "vec3" -> mk (S.Vec [ num "0.0"; num "0.0"; num "0.0" ]), "vector"
  | "text" -> mk (S.Str "text"), "text"
  | "str" -> mk (S.List [ mk (S.Sym "str"); mk (S.Str "text") ]), "text"
  | "array/index_of" ->
      List.hd (Result.get_ok (S.parse "(array/index_of (array/int 0) (array/int 0))")), "matched"
  | "array/scatter_add" ->
      List.hd (Result.get_ok (S.parse
        "(array/scatter_add (array/int 0) (array/float 0) 0)")), "added"
  | "sop/with_attr" ->
      List.hd (Result.get_ok (S.parse
        "(sop/with_attr (sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) \"attribute\" (array/vec3 0))")), "attribute"
  | ("sop/mesh" | "sop/remap" | "sop/concat" as op) ->
      let mesh = "(sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))" in
      let text, name = if op = "sop/mesh" then mesh, "mesh" else if op = "sop/concat" then
        Printf.sprintf "(sop/concat (list %s %s))" mesh mesh, "concatenated" else
        Printf.sprintf "(sop/remap %s %s :points (array/int 0) :vertices (array/int 0) :primitives (array/int 0))" mesh mesh, "remap" in
      List.hd (Result.get_ok (S.parse text)), name
  | "sop/mesh_planes" ->
      List.hd (Result.get_ok (S.parse
        "(sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))")), "mesh"
  | form when List.exists (fun (_, f, _, _) -> f = form) forms ->
      let _, _, text, name = List.find (fun (_, f, _, _) -> f = form) forms in
      List.hd (Result.get_ok (S.parse text)), name
  | op ->
      let signature = Flow.Workspace.op_signature ~ops context op in
      (* the selected node feeds the first input of its type *)
      let wired = ref false in
      let argument (label, ty) = match wire, Flow_graph.Flow_edit.default_for ty label with
        | Some (name, wty), _ when not !wired && Flow.Ty.fits wty ty -> wired := true; mk (S.Sym name)
        | _, Some d -> d
        | _, None ->
            (match ty with
            | Flow.Ty.List _ -> mk (S.List [ mk (S.Sym "range"); num "4" ])
            | _ -> num "0.5") in
      let args = match signature with
        | Some s -> List.map argument s.Flow.Workspace.pos | None -> [] in
      mk (S.List (mk (S.Sym op) :: args)),
      (match op with
       | "+" -> "sum" | "-" -> "difference" | "*" -> "product" | "/" -> "quotient"
       | "<" | ">" | "<=" | ">=" | "=" -> "test" | op -> op)

(* A kind picked in the node menu, as one [Add_node] at the selected zone (else the graph
   body), wired to the selected node when the kind takes a geometry input. *)
let scope_add value key =
  match add_target value with
  | _ when String.starts_with ~prefix:"newgraph:" key ->
      (* "New graph: <context>" of the add menu: a whole graph, opened once made *)
      let context = String.sub key 9 (String.length key - 9) in
      [ Syntax_batch ("New graph", [ snd (new_graph value context) ]) ]
  | Some (_, context) when context = Flow.Context.scene && (List.mem key [ "geometry"; "world" ] || String.starts_with ~prefix:"of:" key) ->
      (* composition: geometry brings its SOP graph, World its world graph, one gesture each *)
      let geometry existing label =
        [ Syntax_batch (label, Editor_document.Scene_sync.add_geometry value.doc ~existing) ] in
      if key = "geometry" then geometry None "Add geometry"
      else if key = "world" then
        (match Editor_document.Scene_sync.add_world value.doc with
         | Ok ops -> [ Syntax_batch ("Add World", ops) ]
         | Error message -> [ Declined message ])
      else
        let graph = String.sub key 3 (String.length key - 3) in
        geometry (Some graph) ("Add geometry of " ^ graph)
  | Some (_, context) when context = Flow.Context.scene && key = "material" ->
      [ Syntax_batch ("New material", [ snd (new_material value) ]) ]
  | Some (graph, context) when context = Flow.Context.scene && key = "merge" ->
      (* several selected objects move into a new merge; with none selected, an empty merge *)
      (match Pxui_graph.Scope.selected value.scope_view with
       | _ :: _ :: _ as nodes ->
           let ws, _ = value.doc.Document.workspace in
           [ Syntax_edit (Flow_graph.Flow_edit.Group_merge { nodes;
               name = Flow_graph.Flow_edit.fresh_name ws.source ~root:graph "group" }) ]
       | _ ->
           let ws, _ = value.doc.Document.workspace in
           [ Syntax_edit (Flow_graph.Flow_edit.Add_node { scope = [ graph ];
               name = Flow_graph.Flow_edit.fresh_name ws.source ~root:graph "group";
               expr = Flow.Syntax.make (Flow.Syntax.List [ Flow.Syntax.make (Flow.Syntax.Sym "scene/merge") ]) }) ])
  | Some (graph, context) ->
      let ws, _ = value.doc.Document.workspace in
      let is_value = String.length key > 0 && key.[0] = '=' in
      let material_of = String.starts_with ~prefix:"of-material:" key in
      let kind = List.find_opt (fun (d : Flow_sop.Catalog.descriptor) -> d.key = key) (kinds value context) in
      let arity = match kind with
        | Some d -> List.length d.slots - List.length d.keyword_inputs
        | None -> if material_of then 1 else 0 in
      let selected = Pxui_graph.Scope.selected value.scope_view in
      let scope, input = match selected with
        | _ when (context = Flow.Context.scene || context = Flow.Context.world) && not is_value ->
            (* an object joins the scene's merge and a layer goes on top of the stack: the
               edit itself does that *)
            [ graph ], None
        | [ path ] when List.length path >= 2 ->
            List.filteri (fun i _ -> i < List.length path - 1) path,
            (if arity > 0 then binding_name path else None)
        | _ ->
            (* nothing selected: a kind with an input reads the graph's result, so the text
               still checks (a required input is never left open) *)
            let result = match value.scope_key with
              | Some { scope = { Flow_graph.Projection.result = Link name; _ }; _ } -> Some name
              | _ -> None in
            [ graph ], (if arity > 0 then result else None) in
      let expr, base =
        if is_value then begin
          let wire = match selected, value.scope_key with
            | [ path ], Some { scope; _ } when List.length path >= 2 ->
                Option.bind (binding_name path) (fun name ->
                  Option.map (fun (n : Flow_graph.Projection.node) -> name, n.ty) (Flow_graph.Projection.find scope path))
            | _ -> None in
          value_expression ~ops:ws.checked.ops ?wire context key
        end
        else begin
          let head = Flow.Syntax.make (Flow.Syntax.Sym (Flow.Workspace.context_name context ^ "/"
            ^ (if material_of then "material" else key))) in
          let geometry = match context, key, List.find_opt (fun (g : Flow.Workspace.graph) ->
              g.context = Flow.Context.sop) ws.checked.graphs with
            | context, "geometry", Some g when context = Flow.Context.scene -> [ Flow.Syntax.make (Flow.Syntax.List
                [ Flow.Syntax.make (Flow.Syntax.Sym "ref"); Flow.Syntax.make (Flow.Syntax.Sym g.name) ]) ]
            | _ -> [] in
          let positional = geometry @ (match input with
               | Some n -> [ Flow.Syntax.make (Flow.Syntax.Sym n) ]
               | None -> if arity > 0 && geometry = [] then [ Flow.Syntax.make (Flow.Syntax.Sym "nil") ] else []) in
          let defaults = match kind with None -> [] | Some d ->
            let next_position = ref 0 in
            let keywords = d.keyword_inputs in
            let types = if d.slot_types = [] then List.map (fun _ -> "geometry") d.slots else d.slot_types in
            List.map2 (fun (name, required) ty -> name, (required, ty)) d.slots types
            |> List.map (fun (name, (required, ty)) ->
              let supplied = if List.mem name keywords then false else begin
                let i = !next_position in incr next_position; i < List.length positional end in
              if supplied || ty = "geometry" || required <> Edit_graph.Required then [] else
              match Option.bind (Flow.Ty.of_string ty) (fun ty -> Flow_graph.Flow_edit.default_for
                ~fresh:(Flow_graph.Flow_edit.fresh_name ws.source ~root:graph) ty name) with
              | None -> []
              | Some form -> [Flow.Syntax.make (Flow.Syntax.Kw name); form]) |> List.concat in
          Flow.Syntax.make (Flow.Syntax.List (head :: positional @ defaults
            @ (if material_of then [ Flow.Syntax.make (Flow.Syntax.Kw "material");
                Flow.Syntax.make (Flow.Syntax.List [ Flow.Syntax.make (Flow.Syntax.Sym "ref");
                  Flow.Syntax.make (Flow.Syntax.Sym (String.sub key 12 (String.length key - 12))) ]) ] else []))),
          (if material_of then "material" else key)
        end in
      let name = Flow_graph.Flow_edit.fresh_name ws.source ~root:graph base in
      [ Syntax_edit (Flow_graph.Flow_edit.Add_node { scope; name; expr }) ]
  | None -> [ Declined "Open a graph to add a node to it" ]
