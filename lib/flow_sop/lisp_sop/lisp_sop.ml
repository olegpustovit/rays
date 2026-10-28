let factories = Sop.Nodes.factories

let snapshot geometry =
  let parameters =
    Printf.sprintf "data_id=%d;bytes=%d" (Rdk.Geometry.data_id geometry)
      (Rdk.Geometry.payload_bytes geometry)
  in
  Sop.Node.Build.make_geometry ~operation:"snapshot" ~version:1 ~parameters
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _context _inputs -> Ok Sop.Node.Build.{ geometry; instances = None })

(* An external node may itself have inputs, but a factory node cannot: the lowering sees an
   input-free marker per external node and the real nodes are put back afterwards. *)
let marker name =
  Sop.Node.Build.make_geometry ~operation:"lisp_sop_ext" ~version:1 ~parameters:name
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _context _inputs -> failwith "Lisp_sop: marker cooked")

let restore with_ root =
  let module Node = Sop.Node in
  (* a DAG (an inlined body reads its input many times): each node is rebuilt once *)
  let restored = Hashtbl.create 64 in
  let rec go node =
    match Hashtbl.find_opt restored (Node.id node) with
    | Some node -> node
    | None ->
        let result =
          if Node.operation node = "lisp_sop_ext" then List.assoc (Node.parameters node) with_
          else
            match Node.inputs node with
            | [] -> node
            | inputs ->
                let rebuilt = List.map go inputs in
                if List.for_all2 ( == ) inputs rebuilt then node
                else Node.Build.rebuild_with_inputs node (Array.of_list rebuilt)
        in
        Hashtbl.add restored (Node.id node) result;
        result
  in
  go root

let workspace_node_result ?(with_ = []) source =
  let image_name name =
    if String.starts_with ~prefix:"image/" name then
      Some (String.sub name 6 (String.length name - 6))
    else None
  in
  let factory_key name =
    match image_name name with Some name -> "lisp_sop_image_ext_" ^ name | None -> "ext_" ^ name
  in
  let external_factories =
    List.map
      (fun (name, _) ->
        let marker = marker name in
        Sop.Edit_graph.factory ~key:(factory_key name) ~label:("Ext " ^ name) ~category:[ "Test" ]
          ~arity:0 (fun _ -> Ok marker))
      with_
  in
  let image_ops =
    List.filter_map
      (fun (name, _) ->
        Option.map
          (fun image_name ->
            let prototype = Option.get (Flow.Op.find "image/noise" Flow.Context.value) in
            {
              prototype with
              Flow.Op.name = "image/ext_" ^ image_name;
              signature = { pos = []; opt = []; rest = None; kw = [] };
              check = (fun _ -> ());
              packed_extension = None;
              body = (fun ~live:_ ~node _ -> node ("sop/" ^ factory_key name) []);
            })
          (image_name name))
      with_
  in
  let factories = external_factories @ factories in
  match Flow.Syntax.parse source with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok forms -> (
      let lowered =
        Result.bind (Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version factories)
          (fun catalog ->
            match
              Flow_sop.Catalog.check_workspace
                ~ops:(image_ops @ Flow_sop.Operators.all)
                catalog forms
            with
            | Some checked, _ -> Flow_sop.Lower.of_checked ~factories checked
            | None, diagnostics ->
                Error
                  (Option.value
                     ~default:(Diagnostic.error ~code:"E_LOWER" "Workspace did not check")
                     (List.find_opt
                        (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity)
                        diagnostics)))
      in
      match lowered with
      | Error d -> Error (Diagnostic.to_string d)
      | Ok lowered -> (
          match
            List.find_opt (fun (graph : Flow_sop.Lower.graph) -> graph.default) lowered.graphs
          with
          | None -> Error "the workspace has no SOP graph"
          | Some { root = None; _ } -> Error "the graph has no result"
          | Some { root = Some root; network; _ } ->
              Result.map (restore with_)
                (Sop.Edit_graph.compile_node network.geometry ~node_id:root)))

let workspace_node ?with_ text =
  match workspace_node_result ?with_ text with
  | Ok node -> node
  | Error message -> failwith ("Lisp_sop: " ^ message ^ "\n" ^ text)

let node_result ?with_ text =
  workspace_node_result ?with_ ("(workspace w (graph g :context sop " ^ text ^ "))")

let node ?with_ text =
  match node_result ?with_ text with
  | Ok node -> node
  | Error message -> failwith ("Lisp_sop: " ^ message ^ "\n" ^ text)

(* Printing OCaml values as Lisp literals, for the holes of ported tests. *)
let float = Flow.Lisp.float
let vec3 (v : Rays_math.Vec3.t) = Printf.sprintf "[%s %s %s]" (float v.x) (float v.y) (float v.z)

let curve_points points =
  "["
  ^ String.concat " "
      (List.map
         (fun (x, y, z) -> Printf.sprintf "[%s %s %s]" (float x) (float y) (float z))
         (Array.to_list points))
  ^ "]"
