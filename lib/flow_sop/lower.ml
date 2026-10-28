open Flow
module Edit = Sop.Edit_graph
module Zone = Sop.Zone
module E = Eval

let source_attribute = Rdk.Mesh_merge.provenance_attribute

(* Serial for host-call nodes: they are cooked once in a session that keeps no cache entries, so their
   arguments need no content key (marshalling a million-element array costs more than the kernel). *)
let host_serial = Atomic.make 0

type pending = { node : int; field : string; value : E.value }
type origin = { merge : int; input : int; source : int; site : Workspace.path; iter : int list }
type zone = { cid : int; site : Workspace.path; iter : int list; body_site : Workspace.path;
              base : int; count : int Atomic.t; ekey : string option;
              positions : (float * float * float) array Atomic.t }
type graph = {
  name : string; instance : int; default : bool;
  network : Network.t; root : int option;
}
type t = {
  gpu : Flow_ir.Gpu.config;
  graphs : graph list;
  factories : Edit.factory list;
  compiled_ids : int Instance_path.Map.t;
  sites : Workspace.path list;
  compiled : int Network.Int_map.t;
  pending : pending list;
  provenance : origin Network.Int_map.t;
  zones : zone list;
  volatile : unit Network.Int_map.t;
  plan : E.plan;
  image_sites : Workspace.path option array;
  states : E.value list;
  evaluated : E.t;
  approx : Workspace.Paths.t;
  approx_reasons : (Workspace.path * Diagnostic.t list) list;
  profile : Flow_ir.Profile.t;
  preview : node:int -> probes:int list -> Network.t -> (Network.t * int) option;
}

let fail code message = raise (Diagnostic.Error (Diagnostic.error ~code message))
let ok = function Ok value -> value | Error diagnostic -> raise (Diagnostic.Error diagnostic)
let edit = function Ok value -> value | Error message -> fail "E_LOWER" message

type image_context = {compiled:int Network.Int_map.t; network:Network.t}

type image_resolver = ?gpu:Flow_ir.Gpu.config -> ?context:image_context -> E.plan -> state:E.state -> live:Input.t -> E.value ->
  (Sop.Image.t, Diagnostic.t) result
type images = {resolve:image_resolver; metadata:E.plan -> int -> (int * int) option}
let image_provider = Domain.DLS.new_key (fun () -> None)
let with_images ?gpu ?(metadata=fun _ _->None) resolver run =
  let previous=Domain.DLS.get image_provider in
  Domain.DLS.set image_provider (Some {resolve=(fun ?gpu:requested ?context plan ~state ~live value->
    let gpu=match requested with Some _->requested|None->gpu in
    resolver ?gpu ?context plan ~state ~live value);metadata});
  Fun.protect ~finally:(fun()->Domain.DLS.set image_provider previous) run
let image_metadata plan = match Domain.DLS.get image_provider with
  |None->(fun _->None)|Some images->images.metadata plan
let resource_image operation result =
  Sop.Node.Private.make ~operation ~version:1
    ~parameters:(match result with Ok image->string_of_int(Sop.Image.data_id image)|Error _->"unbound")
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _->match result with
      |Ok image->Ok Sop.Node.Private.{payload=Sop.Payload.Image image;instances=None}
      |Error diagnostic->Error diagnostic)

type kernel_input = {index : int; cid : int; identity : int;
  signature : Ty.fn_signature; fn : E.fn; sources : int list}

type prepared = {
  cid : int;
  factory : Edit.factory;
  slots : (int * int) list;  (* slot index, source plan id *)
  arity : int;
  changes : (string * Param.value) list;
  dynamic : (E.value * (E.value -> (string * Param.value) list)) list;
      (* a template node: arguments that read the element, forced for each one *)
  zone : zinfo option;
  kernels : kernel_input list;
}
and zinfo = { plan_id : int; lo : int; hi : int;
              preview : (int * int list) option;
              root : int; order : int list; live : bool; stateful : bool;
              captures : int list; ekey : string option; kind : Zone.kind;
              key : string option; base : int; count : int Atomic.t;
              positions : (float * float * float) array Atomic.t }

let hex text = match Port.color_of_text text with
  | Some (r, g, b) -> E.Vec3 (r, g, b)
  | None -> fail "E_LOWER" ("Bad colour " ^ text)

let typed = function
  | E.Int n -> Port_type.Int_value n
  | Float f -> Float_value f
  | Bool b -> Bool_value b
  | Vec3 (x, y, z) -> Vec3_value (x, y, z)
  | _ -> fail "E_LOWER" "Argument is not a number, bool or vec3"

let changes_of (parameter : Port.parameter) value =
  match parameter.ty, value with
  | None, E.Text text ->
      let literal = match parameter.fields with
        | [{Param.kind = Param.Choice_view _; _}] -> Param.Choice_value text
        | _ -> Param.Text_value text in
      ok (Port.literal_changes parameter (Port.Scalar literal))
  | None, _ -> fail "E_LOWER" ("Port " ^ parameter.path ^ " takes text")
  | Some _, E.Text text -> snd (ok (Port.normalize parameter (typed (hex text))))
  | Some _, value ->
      let finite = match value with
        | E.Float f -> Float.is_finite f
        | Vec3 (x, y, z) -> Float.is_finite x && Float.is_finite y && Float.is_finite z
        | _ -> true in
      if not finite then fail "E_LOWER" ("Port " ^ parameter.path ^ " takes a finite number");
      snd (ok (Port.normalize parameter (typed value)))

let changes parameter value = try Ok (changes_of parameter value) with Diagnostic.Error d -> Error d

let at_zero value =
  if E.is_live value then ok (E.force value ~live:(Input.at_time (0.))) else value

let is_zone kind = kind = "zone/points" || kind = "zone/pieces"
let metadata_lowering_operations =
  Operators.all
  |> List.map (fun (operation : Op.t) -> operation.name)
  |> List.filter (fun name ->
    List.mem name ["sop/rename_attr"; "sop/swap_attr"; "sop/delete_attrs";
      "sop/rename_group"; "sop/delete_group"; "sop/rename_edge"; "sop/delete_edge"])

let metadata_factory ~operation ~args sources =
  let text name default = match List.assoc_opt name args with
    | None -> default | Some (E.Text text) -> text
    | _ -> fail "E_METADATA_TEXT" "Metadata names and patterns must be static text" in
  let bool name = match List.assoc_opt name args with
    | None -> false | Some (E.Bool value) -> value
    | _ -> fail "E_METADATA_BOOL" "Metadata flags must be static booleans" in
  let build = function
    | input :: rest -> (match operation with
      | "sop/rename_attr" -> Attribute_metadata.rename ~owner:(text "owner" "any")
          ~pattern:(text "pattern" "") ~replacement:(text "replacement" "")
          ~conflict:(text "conflict" "error") input
      | "sop/swap_attr" -> Attribute_metadata.swap ~owner:(text "owner" "point")
          ~source:(text "source" "") ~destination:(text "destination" "")
          ~method_:(text "method" "swap") input
      | "sop/delete_attrs" -> Attribute_metadata.delete_attributes
          ?reference:(List.nth_opt rest 0) ~inverse:(bool "inverse")
          ~point:(text "point" "") ~vertex:(text "vertex" "")
          ~primitive:(text "primitive" "") ~detail:(text "detail" "") input
      | "sop/rename_group" -> Attribute_metadata.rename_group ~owner:(text "owner" "any")
          ~pattern:(text "pattern" "") ~replacement:(text "replacement" "")
          ~conflict:(text "conflict" "error") input
      | "sop/delete_group" -> Attribute_metadata.delete_group ~owner:(text "owner" "any")
          ~pattern:(text "pattern" "") ~unused:(bool "unused") input
      | "sop/rename_edge" -> Attribute_metadata.rename_edge
          ~from:(text "pattern" "") ~into:(text "replacement" "") input
      | "sop/delete_edge" -> Attribute_metadata.delete_edge ~name:(text "name" "") input
      | _ -> fail "E_LOWER" ("No metadata lowering for " ^ operation))
    | [] -> invalid_arg "Metadata operations require geometry" in
  Edit.factory ~key:operation ~label:operation ~category:["Flow"; "Metadata"]
    ~arity:(List.length sources) build

let geometry_array_operations =
  let native = ["sop/mesh"; "sop/mesh_planes"; "sop/remap"; "sop/concat"; "sop/with_planes"] in
  Operators.all |> List.filter_map (fun (operation : Op.t) ->
    if List.mem operation.name native then Some operation.name else None)
let is_geometry_arrays kind = List.mem kind geometry_array_operations
let geometry_arrays_key = function
  | "sop/remap" -> "flow.remap" | "sop/concat" -> "flow.concat"
  | "sop/with_planes" -> "flow.with_planes" | _ -> "flow.mesh"
let geometry_arrays_label = function
  | "sop/remap" -> "Remap" | "sop/concat" -> "Concat"
  | "sop/with_planes" -> "With planes" | _ -> "Mesh"
let geometry_arrays_data args =
  E.Record (List.filter (fun (name, _) ->
    not (List.mem name ["source"; "target"; "sources"; "geometry"])) args)

let geometry_arrays_node ~profile ~strict_body ~reference ~approx ~kernel_source
    ?(host = false) ?state ?elems ~operation ~args ~sources inputs =
  let data = geometry_arrays_data args in
  let program = Attribute_kernel.prepare ~profile ~strict:strict_body ~approx
    ~sink:Flow_ir.Sop_input ~sources inputs data in
  if strict_body then ignore (ok program);
  let parameters = Lazy.force kernel_source ^
    (match operation with
     | "sop/remap" -> ";remap;" | "sop/concat" -> ";concat;"
     | "sop/with_planes" -> ";planes;" | _ -> ";mesh;") ^
    (if host then "host" ^ string_of_int (Atomic.fetch_and_add host_serial 1)
     else Flow.Value.key_of ~residual:E.Residual.residual_id data) ^
    Option.fold ~none:"" ~some:E.state_stamp state ^
    Option.fold ~none:"" ~some:(fun fields ->
      Flow.Value.key_of ~residual:E.Residual.residual_id (E.Record fields)) elems in
  let node = Sop.Node.Private.make_geometry ~label:(geometry_arrays_label operation)
    ~operation:(geometry_arrays_key operation) ~version:1 ~parameters
    ~cook_mode:(if List.mem operation ["sop/remap"; "sop/concat"; "sop/with_planes"]
      then Sop.Node.Generic else Sop.Node.Generator)
    ~dependencies:(if E.frame_dependent data then Sop.Context.Dependencies.one Input
      else Sop.Context.Dependencies.static) ~inputs:(Array.of_list inputs)
    (fun ~node_id:_ context geometries ->
      let cancelled () = Sop.Context.cancelled context in
      let error () = Error (Diagnostic.error ~code:"E_CANCELLED" "Mesh kernel cancelled.") in
      let geometry id = Option.map (fun index -> geometries.(index))
        (List.find_index ((=) id) sources) in
      let result = if cancelled () then error () else Result.bind program (fun program ->
        Result.bind (Flow_ir.Executor.force ?state ?elems ~reference ~strict:strict_body
          ~resolve:(Attribute_kernel.resolve ~geometry) program ~live:(Sop.Context.input context))
          (function
            | E.Record fields ->
                if cancelled () then error () else
                let result = if operation = "sop/with_planes" then
                    (match List.assoc_opt "geometry" args with
                     | Some (E.Deferred (ty, id)) when ty = Ty.geometry && geometry id <> None ->
                         Attribute_kernel.with_planes ~grain:(Sop.Context.grain context)
                           (Option.get (geometry id)) fields
                     | _ -> Error (Diagnostic.error ~code:"E_DATA_SOURCE"
                         "With planes needs a cooked geometry."))
                  else if operation = "sop/concat" then
                    let rec collect result = function
                      | [] -> Ok (List.rev result)
                      | E.No_geo :: rest -> collect result rest
                      | E.Deferred (ty, id) :: rest when ty = Ty.geometry ->
                          (match geometry id with
                           | Some value -> collect (value :: result) rest
                           | None -> Error (Diagnostic.error ~code:"E_DATA_SOURCE"
                               "Concat needs cooked source geometries."))
                      | _ -> Error (Diagnostic.error ~code:"E_CONCAT_TYPE"
                          "Concat sources must be a geometry list.") in
                    Result.bind (match List.assoc_opt "sources" args with
                      | Some (E.List values) -> collect [] (Array.to_list values)
                      | _ -> Error (Diagnostic.error ~code:"E_CONCAT_TYPE"
                          "Concat sources must be a geometry list.")) (fun sources ->
                      Topology_kernel_impl.concat ~cancel:(Sop.Context.cancel_token context)
                        ~grain:(Sop.Context.grain context) ~sources fields)
                  else if operation <> "sop/remap" then Topology_kernel.mesh fields
                  else
                    let cooked name = match List.assoc_opt name args with
                      | Some (E.Deferred (ty, id)) when ty = Ty.geometry ->
                          (match geometry id with
                           | Some geometry -> Ok geometry
                           | None -> Error (Diagnostic.error ~code:"E_DATA_SOURCE"
                               "Remap needs two cooked geometries."))
                      | _ -> Error (Diagnostic.error ~code:"E_DATA_SOURCE"
                          "Remap needs two cooked geometries.") in
                    Result.bind (cooked "source") (fun source ->
                      Result.bind (cooked "target") (fun target ->
                        Topology_kernel_impl.remap ~cancel:(Sop.Context.cancel_token context)
                          ~grain:(Sop.Context.grain context) ~source ~target fields)) in
                Result.bind result (fun geometry ->
                  if cancelled () then error () else Ok geometry)
            | _ -> Error (Diagnostic.error ~code:"E_TOPOLOGY_TYPE" "Mesh data must be a record."))) in
      Result.map (fun geometry -> Sop.Node.Private.{geometry; instances=None}) result) in
  Sop.Node.Private.with_facts {(Sop.Node.facts node) with
    reads=(if List.mem operation ["sop/remap"; "sop/concat"; "sop/with_planes"]
      then ["*"] else Attribute_kernel.reads data);
    writes=["*"]; topology=Changed; elementwise=None} node

let attribute_options args =
  let text key default code = match List.assoc_opt key args with
    | None -> default | Some (E.Text text) -> text
    | _ -> fail code ("Attribute " ^ key ^ " must be static text") in
  let owner = ok (Attribute_kernel.owner (text "owner" "point" "E_ATTR_OWNER"))
  and kind = text "type" "vec3" "E_ATTR_TYPE" in
  ignore (ok (Attribute_kernel.value_type kind)); owner, kind
let group_data args = E.Record (List.filter (fun (name, _) -> name="values" || name="order") args)
let attribute_data args =
  if not (List.mem_assoc "selection" args || List.mem_assoc "default" args) then
    List.assoc "values" args
  else E.Record (List.filter (fun (name, _) ->
    List.mem name ["values"; "selection"; "default"]) args)

let attribute_node ~strict_body ~reference ~profile ~approx ~gpu ~kernel_source
    ?state ?elems ?selection ?default ?(owner=Rdk.Attribute.Point) ?(kind="vec3")
    ~name ~values ~sources inputs =
  if strict_body then ignore (ok (Attribute_kernel.prepare ~strict:true ~sources inputs values));
  Attribute_kernel.node ?state ?elems ?selection ?default ~gpu ~strict:strict_body ~reference
    ~profile ~approx ~owner ~kind ~source:(Lazy.force kernel_source)
    ~name ~values ~sources inputs

let group_node ~strict_body ~reference ~profile ~approx ~kernel_source
    ?state ?elems ~args ~sources inputs =
  let text key default = match List.assoc_opt key args with
    | None -> default | Some (E.Text text) -> text
    | _ -> fail "E_GROUP_TYPE" ("Group " ^ key ^ " must be static text") in
  let owner_name = text "owner" "point" and name = text "name" "" in
  let owner = ok (Group_kernel.owner owner_name) and values = group_data args in
  let program = Attribute_kernel.prepare ~profile ~strict:strict_body ~sources inputs values in
  if strict_body then ignore (ok program);
  let node = Sop.Node.Private.make_geometry ~label:("Write group " ^ name)
    ~operation:"flow.with_group" ~version:1
    ~parameters:(Lazy.force kernel_source ^ owner_name ^ ":" ^ name ^
      Flow.Value.key_of ~residual:E.Residual.residual_id values ^
      Option.fold ~none:"" ~some:E.state_stamp state)
    ~cook_mode:Sop.Node.Generic
    ~dependencies:(if E.frame_dependent values then Sop.Context.Dependencies.one Input
      else Sop.Context.Dependencies.static)
    ~inputs:(Array.of_list inputs)
    (fun ~node_id:_ context geometries ->
      let result = if geometries=[||] || List.assoc "geometry" args=E.No_geo then
          Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Group write needs geometry.")
        else if Sop.Context.cancelled context then
          Error (Diagnostic.error ~code:"E_CANCELLED" "Group kernel cancelled.")
        else Result.bind program (fun program ->
          let geometry id = Option.map (fun index -> geometries.(index))
            (List.find_index ((=) id) sources) in
          Result.bind (Flow_ir.Executor.force ?state ?elems ~reference ~strict:strict_body
            ~resolve:(Attribute_kernel.resolve ~geometry) program ~live:(Sop.Context.input context))
            (function
              | E.Record fields ->
                  let order = match List.assoc_opt "order" fields with
                    | None -> Ok None
                    | Some (E.Int_array order) -> Ok (Some order)
                    | Some (E.Record fields) ->
                        (match List.assoc_opt "ordered" fields,List.assoc_opt "indices" fields with
                         | Some (E.Bool ordered),Some (E.Int_array indices) ->
                             Ok (if ordered then Some indices else None)
                         | _ -> Error (Diagnostic.error ~code:"E_GROUP_ORDER"
                             "Group order requires an integer array or an ordered/indices record."))
                    | _ -> Error (Diagnostic.error ~code:"E_GROUP_ORDER"
                        "Group order requires an integer array or an ordered/indices record.") in
                  Result.bind order (fun order -> match List.assoc_opt "values" fields with
                    | Some (E.Int_array mask) -> Group_kernel.write ?order ~owner ~name mask geometries.(0)
                    | _ -> Error (Diagnostic.error ~code:"E_GROUP_MASK"
                        "Group mask requires an integer array."))
              | _ -> assert false)) in
      Result.map (fun geometry -> Sop.Node.Private.{geometry; instances=None}) result) in
  Sop.Node.Private.with_facts {(Sop.Node.facts node) with
    topology=Preserved; reads=["*"]; writes=["*"]} node

let build_lisp_body ~gpu ~lower_nested ~record_profile ~checked
    ~(definition : Workspace.node_definition) ~provenance
    ~label ~context ~inputs ~parameters ~bindings ~functions ~factories ~def_context_use =
  Phase_timer.without (fun () ->
    let rec function_elements = function
      | E.Fn fn -> [fn]
      | E.List values -> Array.to_list values |> List.concat_map function_elements
      | _ -> [] in
    let make_capture_factories aliases = List.mapi (fun index input ->
      let key = "lisp_capture_" ^ string_of_int index in
      let factory = Edit.factory ~key ~label:key ~category:["Flow"] ~arity:0
        (fun _ -> Sop.Node.Private.make ~operation:key ~version:1
          ~parameters:(string_of_int (Sop.Node.id input)) ~cook_mode:Sop.Node.Generator
          ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
          (fun ~node_id:_ _ _ -> Result.map (fun payload ->
            let payload = match payload, List.assoc_opt index aliases with
              | Sop.Payload.Kernel kernel, Some sources ->
                  Sop.Payload.Kernel (Sop.Kernel.Private.remap_geometry_captures ~sources kernel)
              | _ -> payload in
            Sop.Node.Private.{payload;instances=None}) (Lisp_node.input index))) in
      key, factory) inputs in
    let capture_factories = make_capture_factories [] in
    let body = Context_kernel.specialize ?provenance ~label ~context definition.body in
    let body = {body with inputs=List.map (fun (name, ty, default) ->
      let default = match List.assoc_opt name bindings with
        | None -> default
        | Some indices ->
            let element_ty = match ty with Ty.List element -> element | _ -> ty in
            let capture index = let key,_=List.nth capture_factories index in
              Workspace.{path=None;ty=element_ty;form=Syntax.make (List []);
                node=Call {kind="sop/" ^ key;
                  ctx=(if element_ty=Ty.image then Context.image else Context.sop);args=[]}} in
            let node = match ty,indices with
              | Ty.List _, _ -> Workspace.List_lit (List.map capture indices)
              | _, [index] -> (capture index).node
              | _, [] -> Workspace.Nil
              | _ -> fail "E_LOWER" "A fixed Lisp slot has several inputs" in
            Some Workspace.{path=None;ty;form=Syntax.make (List []);node} in
      name, ty, default) body.inputs} in
    let forwarded = List.concat_map (fun (name,value) ->
      let fns = function_elements value and indices = Option.value ~default:[] (List.assoc_opt name bindings) in
      if List.compare_lengths fns indices <> 0 then fail "E_LOWER" "Callable slots do not match their cooked inputs";
      List.mapi (fun offset (fn,index) ->
        Printf.sprintf "lisp_kernel_%s_%d" name offset,fn,index) (List.combine fns indices)) functions in
    let geometry_captures = List.concat_map (fun (_,fn,index) ->
      Attribute_kernel.sources (E.Fn fn) |> List.map (fun source ->
        let key = Printf.sprintf "lisp_capture_geometry_%d_%d" index source in
        let factory = Edit.factory ~key ~label:key ~category:["Flow"] ~arity:0
          (fun _ -> Sop.Node.Private.make_geometry ~operation:key ~version:1
            ~parameters:(string_of_int (Sop.Node.id (List.nth inputs index)))
            ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static
            ~inputs:[||] (fun ~node_id:_ _ _ ->
              Result.bind (Lisp_node.input index) (fun payload ->
              Result.bind (Sop.Payload.kernel payload) (fun kernel ->
                match Sop.Kernel.Private.geometry_capture kernel source with
                | Some geometry -> Ok Sop.Node.Private.{geometry;instances=None}
                | None -> Error (Diagnostic.error ~code:"E_DATA_SOURCE"
                    "A forwarded function geometry capture is unavailable"))))) in
        key, source, index, factory)) forwarded in
    let body = {body with inputs=List.map (fun (name,_,index) ->
      let key,_=List.nth capture_factories index in
      name,Ty.geometry,Some Workspace.{path=None;ty=Ty.geometry;form=Syntax.make (List []);
        node=Call {kind="sop/" ^ key;ctx=Context.sop;args=[]}}) forwarded @
      List.map (fun (name,_,_,_) -> name,Ty.geometry,
        Some Workspace.{path=None;ty=Ty.geometry;form=Syntax.make (List []);
          node=Call {kind="sop/" ^ name;ctx=Context.sop;args=[]}}) geometry_captures @ body.inputs} in
    let validations = List.mapi (fun index (predicate, _, _) ->
      {body with name="lisp_validate_" ^ string_of_int index;
        context=Context.value;body=predicate}) definition.validations in
    let reached = Context_kernel.reachable_defs checked (body::validations) in
    let nested = {checked with graphs=body::validations;
      defs=List.filter_map (fun (def, uses) ->
        if not (List.memq def reached) then None
        else Some (if uses then Context_kernel.specialize ~label ~context def else def))
        (def_context_use ());
      nodes=List.filter (fun (d : Workspace.node_definition) ->
        d.kind.qualified <> definition.kind.qualified) checked.nodes} in
    let overrides = List.map (fun (g : Workspace.graph) -> g.name,
      List.filter (fun (name, _) -> List.exists (fun (input,_,_) -> input=name) g.inputs)
        (parameters @ functions))
      nested.graphs in
    let invalid_parameter code message=invalid_arg
      (if code="E_PARAM" then message else code^": "^message) in
    let validated = match E.static ~inputs:(List.tl overrides) {nested with graphs=validations} with
      |Ok validated->validated
      |Error d->invalid_parameter d.code d.message in
    List.iteri (fun index (_, code, message) ->
      match List.assoc ("lisp_validate_" ^ string_of_int index) validated.results with
      | E.Bool true -> ()
      | E.Bool false -> invalid_parameter code message
      | _ -> invalid_arg "A Lisp node validation must return bool") definition.validations;
    (* Header validation is a constructor contract; evaluated body errors
       belong to cooking. Strict compiler refusals remain construction errors. *)
    try
      let body_overrides, kernel_bindings, aliases = if forwarded=[] then List.hd overrides, [], [] else
        let forwarded_evaluation = match E.static ~inputs:[List.hd overrides] {nested with graphs=[body]} with
          |Ok evaluated->evaluated|Error diagnostic->raise(Diagnostic.Error diagnostic) in
        let marker name = match List.assoc name forwarded_evaluation.plan.instances.(0).inputs with
          | E.Deferred (_,source) -> source
          | _ -> fail "E_LOWER" "A forwarded kernel needs a cooked source" in
        let remapped = List.map (fun (name,fn,index) ->
          let sources = List.filter_map (fun (name,source,slot,_) ->
            if slot=index then Some (source, marker name) else None) geometry_captures in
          name, E.Private.remap_function_geometry ~sources fn, marker name, index, sources) forwarded in
        let functions = List.map (fun (name,value) -> name,
          let remap offset fn = match List.find_opt (fun (marker,_,_,_,_) ->
            marker=Printf.sprintf "lisp_kernel_%s_%d" name offset) remapped with
            | Some (_,fn,_,_,_) -> E.Fn fn | None -> E.Fn fn in
          match value with
          | E.Fn fn -> remap 0 fn
          | E.List _ -> E.List (Array.of_list (List.mapi remap (function_elements value)))
          | _ -> value) functions in
        (body.name, List.filter (fun (name,_) ->
           List.exists (fun (input,_,_) -> name=input) body.inputs) (parameters @ functions)),
        List.map (fun (_,fn,source,_,_) -> fn,source) remapped,
        List.map (fun (_,_,_,index,sources) -> index,sources) remapped in
      let capture_factories = make_capture_factories aliases in
      let nested = ok (lower_nested ~gpu
        ~kernel_bindings ~factories:(List.map snd capture_factories @
          List.map (fun (_,_,_,factory) -> factory) geometry_captures @ factories)
        ~inputs:[body_overrides] {nested with graphs=[body]}) in
      record_profile nested.profile;
      let graph = match nested.graphs with
        | [graph] -> graph | _ -> fail "E_LOWER" "A Lisp node body needs one SOP graph" in
      let root = match graph.root with Some root -> root
        | None -> fail "E_LOWER" "A Lisp node body must return geometry" in
      let compiled = edit (Edit.compile_node graph.network.geometry ~node_id:root) in
      let dependencies = Sop.Node.graph_dependencies compiled in
      let driven = graph.network.states<>[] || not(Port.Map.is_empty graph.network.drives)
          || not(Network.Int_map.is_empty graph.network.frame_nodes) in
      let dependencies = if driven then
        Sop.Context.Dependencies.union dependencies (Sop.Context.Dependencies.one Input)
        else dependencies in
      let reached = Context_kernel.context_ops_reached ~inputs:parameters ~defs:checked.defs definition.body in
      let dependencies = List.fold_left (fun dependencies (op, fact) ->
        if List.mem op reached then Sop.Context.Dependencies.union dependencies (Sop.Context.Dependencies.one fact)
        else dependencies) dependencies
        ["sop/context_seed", Sop.Context.Dependencies.Seed; "sop/context_frame", Frame;
         "sop/context_time", Time] in
      let owned = Sop.Node.inspect compiled |> List.filter (fun (node : Sop.Node.info) ->
        not (String.starts_with ~prefix:"lisp_capture_" node.operation)) in
      let declared = definition.kind.facts in
      (* A declared [:facts] is the node's contract, as the native factory's was: the body's
         selection root reads "*" and replaces topology on paper, which would otherwise defeat
         component caching for every node that declares it. Session still refuses an
         undeclared write or a replaced topology at cook time. *)
      let preserved = match declared with
        | Some facts -> facts.preserves_topology
        | None -> List.for_all (fun (node : Sop.Node.info) ->
            node.facts.topology=Sop.Node.Preserved) owned in
      let input_count = List.length inputs in
      let resolve names = Check.resolve_component_names (fun name ->
        match List.assoc_opt name parameters with Some (E.Text text) -> Some text | _ -> None) names in
      (* Declared names (parameter-named ones resolved), then what the body's attribute kernels
         name themselves. Behind a wildcard (a selection root) only the declaration speaks;
         otherwise only what the body reads counts. *)
      let components declared names =
        let inferred = List.sort_uniq String.compare
          (List.concat_map (fun (node : Sop.Node.info) -> names node.facts) owned) in
        match declared with
        | None -> inferred
        | Some declared ->
            let declared = resolve declared in
            if List.mem "*" inferred then
              declared @ List.filter (fun name -> name <> "*" && not (List.mem name declared)) inferred
            else
              List.filter (fun name -> List.mem name inferred) declared
              @ List.filter (fun name -> not (List.mem name declared)) inferred in
      let facts = Sop.Node.{
        cook_mode=(if input_count=0 then Generator else if preserved && input_count=1
          then Duplicate_input 0 else Generic);
        elementwise=(match declared with
          | Some f when preserved -> (match f.elementwise with
              | Check.Points -> Points | Primitives -> Primitives | Irregular -> None)
          | _ -> if preserved then (Sop.Node.facts compiled).elementwise else None);
        reads=components (Option.map (fun (f : Check.kernel_facts) -> f.reads) declared) (fun f -> f.reads);
        writes=components (Option.map (fun (f : Check.kernel_facts) -> f.writes) declared) (fun f -> f.writes);
        topology=(if preserved then Preserved else Changed);
        exact=Option.fold ~none:true ~some:(fun (facts : Check.kernel_facts) -> facts.exact)
          definition.kind.facts && List.for_all (fun (node : Sop.Node.info) -> node.facts.exact) owned} in
      let body context =
        if not driven then Ok compiled else
        let lane = Value_lane.create () in
        (Result.bind (Value_lane.resolve ~gpu ~live:(Sop.Context.input context)
            lane ~time:(Sop.Context.time context) graph.network) (fun resolved ->
            if resolved.geometry==graph.network.geometry then Ok compiled else
            Result.map_error (Diagnostic.error ~code:"E_LOWER")
              (Edit.compile_node resolved.geometry ~node_id:root))) in
      body, dependencies, facts
    with Diagnostic.Error diagnostic ->
      let dependencies = List.fold_left (fun dependencies (op, fact) ->
        if Context_kernel.depends ~op ~inputs:parameters ~defs:checked.defs definition.body
        then Sop.Context.Dependencies.union dependencies (Sop.Context.Dependencies.one fact)
        else dependencies) Sop.Context.Dependencies.static
        ["sop/context_seed", Sop.Context.Dependencies.Seed; "sop/context_frame", Frame;
         "sop/context_time", Time] in
      let cook_mode=if inputs=[] then Sop.Node.Generator else Generic in
      let exact=Option.fold ~none:true ~some:(fun (facts:Check.kernel_facts)->facts.exact)
        definition.kind.facts in
      let facts=Sop.Node.{cook_mode;elementwise=None;reads=["*"];writes=["*"];topology=Changed;exact} in
      (fun _ -> Error diagnostic), dependencies, facts)
module Build_cache = Lru.Make (String)
(* 512 Lisp node body builds (the 16 lowerings of a 30-node workspace fit); every access under the lock. *)
let build_cache = Build_cache.create 512
let build_lock = Mutex.create ()
let same_gpu (a : Flow_ir.Gpu.config) (b : Flow_ir.Gpu.config) =
  a.fast = b.fast && a.policy = b.policy &&
  match a.backend, b.backend with None, None -> true | Some a, Some b -> a == b | _ -> false
module Private = struct let record = ref false let profiles : Flow_ir.Profile.t list ref = ref []
  let cache_hits = Atomic.make 0
  let clear_cache () = Mutex.protect build_lock (fun () -> Build_cache.clear build_cache) end
let geo_of args =
  let rec sources = function
    | E.Deferred (ty, j) when Ty.is_cooked ty -> [j]
    | E.List values -> Array.to_list values |> List.concat_map sources
    | _ -> [] in
  List.concat_map (fun (_,value) -> sources value) args

let is_volatile lowered id = Network.Int_map.mem id lowered.volatile

let same_function a b = a == b ||
  (E.Residual.function_id a = E.Residual.function_id b
   && E.Residual.function_origin a = E.Residual.function_origin b
   && E.Private.function_scope a = E.Private.function_scope b
   && E.Residual.function_name a = E.Residual.function_name b
   && List.equal (fun (an,av) (bn,bv) -> an=bn && av==bv)
     (E.Residual.function_bindings a) (E.Residual.function_bindings b))

let objects lowered = List.filter_map (fun (graph : graph) ->
  Option.map (fun root -> graph.instance, graph.network, root) graph.root)
  lowered.graphs

(* One host call lowers a synthetic workspace. Memoize the source digest by the checked
   workspace content: 8 entries (each pins a workspace's forms). *)
type digest_key = {
  source_forms : Syntax.t list;
  macro_forms : Syntax.t list;
  definitions : Workspace.graph list;
  node_definitions : Workspace.node_definition list;
  graph_forms : Workspace.graph list;
  extra_definitions : Workspace.node_definition list;
}
(* [==] first: a nested lowering shares its forms, and structural [=] walks a whole library for every lookup. *)
let same_content_list left right = left == right || List.equal (fun a b -> a == b || a = b) left right
let same_digest_key a b =
  same_content_list a.source_forms b.source_forms
  && same_content_list a.macro_forms b.macro_forms
  && same_content_list a.definitions b.definitions
  && same_content_list a.node_definitions b.node_definitions
  && same_content_list a.graph_forms b.graph_forms
  && same_content_list a.extra_definitions b.extra_definitions
let digest_cache = Lru.Identity_list.create ~equal:same_digest_key 8
let digest_lock = Mutex.create ()
let cached_digest (checked : Workspace.t) extra compute =
  (* elementwise: a nested lowering rebuilds the lists (graphs = [body]) but shares their forms *)
  let key = {source_forms=checked.source;macro_forms=checked.macros;definitions=checked.defs;
    node_definitions=checked.nodes;graph_forms=checked.graphs;extra_definitions=extra} in
  match Mutex.protect digest_lock (fun () -> Lru.Identity_list.peek digest_cache key) with
  | Some digest -> digest
  | None ->
      let digest = compute () in
      Mutex.protect digest_lock (fun () -> Lru.Identity_list.add digest_cache key digest);
      digest

(* One lowering of a checked workspace: the qualified plan, the per-kind node builders and the
   zone, kernel and preview machinery over it. [lower_checked] recurses for cooked calls and nested
   bodies, so it arrives as a parameter. *)
module Lowerer (P : sig
  val factories : Edit.factory list
  val provenance_tags : bool
  val reference : bool
  val gpu : Flow_ir.Gpu.config
  val strict_body : bool
  val defer_body_errors : bool
  val kernel_bindings : (E.fn * int) list
  val compiled_ids : int Instance_path.Map.t
  val sites : Workspace.path list
  val inputs : (string * (string * Eval.value) list) list option
  val checked : Workspace.t
  val lower_checked :
    factories:Edit.factory list -> ?provenance_tags:bool -> ?reference:bool -> ?gpu:Flow_ir.Gpu.config ->
    ?strict_body:bool -> ?defer_body_errors:bool -> ?kernel_bindings:(E.fn * int) list ->
    ?compiled_ids:int Instance_path.Map.t -> ?sites:Workspace.path list ->
    ?inputs:(string * (string * Eval.value) list) list -> Workspace.t -> (t, Diagnostic.t) result
end) = struct
  open P
    let image_origins = Hashtbl.create 16
    let observe producer residual =
      let view = E.Residual.residual_view residual in
      let key = view.instance, view.site, view.iter in
      let origin = match Hashtbl.find_opt image_origins key with
        | None -> producer
        | Some (Some previous) when producer = Some previous -> producer
        | _ -> None in
      Hashtbl.replace image_origins key origin
    let checked, evaluated = match Flow_ir.qualify_workspace ~record:true ?inputs ~observe ~gpu checked with
      | Ok evaluated -> evaluated
      | Error diagnostic when defer_body_errors -> raise (Diagnostic.Error diagnostic)
      | Error diagnostic -> raise (Diagnostic.Error diagnostic)
    let profile = Flow_ir.Profile.create ~clock:Unix.gettimeofday
    (* Kernel bodies are cache inputs as well as their captures. The digest is
       lazy so ordinary catalog edits retain their existing pipeline counts.
       ponytail: the whole source invalidates kernels; narrow to body/defn cones
       if unrelated structural edits measurably recook them. *)
    let kernel_source = lazy (cached_digest checked [] (fun () -> Context_kernel.kernel_source checked))
    let attribute_options = attribute_options
    and is_geometry_arrays = is_geometry_arrays
    and geometry_arrays_key = geometry_arrays_key
    and geometry_arrays_label = geometry_arrays_label
    and geometry_arrays_data = geometry_arrays_data
    let geometry_arrays_node ?(strict_body=strict_body) ?host ?state ?elems ~operation ~args ~sources inputs =
      geometry_arrays_node ~profile ~strict_body ~reference ~approx:checked.approx ~kernel_source
        ?host ?state ?elems ~operation ~args ~sources inputs
    let structural_node residual captures =
      let program=match Flow_ir.Core_lower.lower ~strict:true ~budget:32768 residual with
        |Ok program->program|Error name->fail "E_PACKED_FORM" ("Cooked selection cannot compile "^name^".") in
      let prepared=Flow_ir.Core_exec.prepare program in
      let inputs=Array.of_list(List.map(fun(_,_,node)->node)captures) in
      let node=Sop.Node.Private.make ~operation:"flow.geometry_select" ~version:1
        ~parameters:(Lazy.force kernel_source)
        ~cook_mode:Sop.Node.Generic ~inputs
        ~dependencies:(if E.frame_dependent(E.Residual residual) then Sop.Context.Dependencies.one Input
          else Sop.Context.Dependencies.static)
        (fun ~node_id:_ context payloads->
          let session=lazy(let session=Result.get_ok(Sop.Session.create ~max_entries:0 ~max_payload_bytes:0) in
            Sop.Session.Private.set_parallel_override session (Some false);session) in
          Fun.protect ~finally:(fun()->if Lazy.is_val session then Sop.Session.close(Lazy.force session))(fun()->
          let handles=Hashtbl.create 16 and geometries=Hashtbl.create 16 and next=ref(-1) in
          List.iteri(fun index(id,ty,_)->
            Hashtbl.add handles id (ty,None,ref(Some Sop.Session.{payload=payloads.(index);instances=None})))captures;
          let error code message=Error(Diagnostic.error ~code message) in
          let handle ty id=match Hashtbl.find_opt handles id with
            |Some(actual,node,output) when actual=ty->Ok(node,output)
            |Some _->error "E_DATA_SOURCE" "A cooked handle has the wrong declared type."
            |None->error "E_DATA_SOURCE" "A cooked handle is missing from this execution." in
          let snapshot ty id=Result.bind(handle ty id)(fun(node,output)->
            let validate (value:Sop.Session.output)=match ty,value.payload with
              |ty,Sop.Payload.Geometry _ when ty=Ty.geometry->Ok value
              |ty,Sop.Payload.Image _ when ty=Ty.image->Ok value
              |_->error "E_DATA_SOURCE" "A cooked handle returned a payload of the wrong type." in
            match !output,node with Some output,_->validate output|None,None->
              error "E_DATA_SOURCE" "A cooked handle has no result or prepared node."
            |None,Some node->
              Result.bind
                ((Sop.Session.cook (Lazy.force session) ~context node))
                (fun value->Result.map(fun value->output:=Some value;value)(validate value))) in
          let geometry id=match Hashtbl.find_opt geometries id with
            |Some geometry->Some geometry
            |None->
                let output=ok(snapshot Ty.geometry id) in
                let geometry=match output.payload,output.instances with
                  |Sop.Payload.Geometry geometry,None->geometry
                  |Sop.Payload.Geometry source,Some transforms->
                      (match Rdk.Instance_copy.materialize_instances ~transforms source with
                       |Ok geometry->geometry|Error e->fail "E_DATA_SOURCE" (Rdk.Error.to_string e))
                  |_->fail "E_DATA_SOURCE" "A geometry source has an image payload." in
                Hashtbl.add geometries id geometry;Some geometry in
          let register_output ty (output:Sop.Session.output)=
            let id= !next in decr next;
            Hashtbl.add handles id(ty,None,ref(Some output));
            E.Deferred(ty,id) in
          let rec cooked_term factories ty value = match value with
            |E.Deferred(actual,id) when Ty.is_cooked actual->
                if actual<>ty then fail "E_DATA_SOURCE" "A cooked argument has the wrong declared type.";
                ignore(ok(handle actual id));
                let key="lisp_host_capture_"^string_of_int(List.length !factories) in
                (* Editable zero-input factories must return zero-input nodes:
                   compiling a borrowed source directly would disconnect it. *)
                let factory=Edit.factory ~key ~label:key ~category:["Lisp"] ~arity:0(fun _->
                  Sop.Node.Private.make ~operation:key ~version:1 ~parameters:(string_of_int id)
                    ~cook_mode:Sop.Node.Generator ~inputs:[||] ~dependencies:Sop.Context.Dependencies.static
                    (fun ~node_id:_ _context _inputs->
                      (Result.map(fun(output:Sop.Session.output)->Sop.Node.Private.{payload=output.payload;
                          instances=output.instances})(snapshot actual id)))) in
                factories:=factory::!factories;
                Some Workspace.{path=None;ty;form=Syntax.make(List[]);
                  node=Call{kind="sop/"^key;ctx=(if actual=Ty.image then Flow.Context.image else Context.sop);args=[]}}
            |E.No_geo when ty=Ty.geometry->Some Workspace.{path=None;ty;form=Syntax.make(Sym "nil");node=Nil}
            |E.List values->
                let element=match ty with Ty.List element->element|_->fail "E_DATA_SOURCE" "A cooked list has the wrong type." in
                let values=Array.to_list values |>List.map(cooked_term factories element) in
                if List.for_all Option.is_some values then Some Workspace.{path=None;ty;form=Syntax.make(List[]);
                  node=List_lit(List.map Option.get values)} else None
            |_->None in
          let rec deferred=function
            |E.Source _|E.Struct(_,Ty.Array _,_)->true
            |E.Record fields->List.exists(fun(_,value)->deferred value)fields
            |E.List values->Array.exists deferred values
            |_->false in
          let constant=function E.Source(name,_,_)->String.starts_with ~prefix:"$core/constant-array/" name|_->false in
          let force_args ?(keep_constant=false) args=List.map(fun(name,value)->name,if deferred value && not(keep_constant && name="values" && constant value) then
              ok(E.Private.force_reference ~resolve:(Attribute_kernel.resolve ~geometry)
                value ~live:(Sop.Context.input context)) else value)args in
          (* sop/mesh, sop/mesh_planes and sop/remap build their node directly from the evaluated arguments: the former
             synthetic one-node workspace was checked, evaluated and lowered per call. *)
          let host_geometry_arrays ty name args =
            let op=match Flow.Op.find ~extra:checked.ops name Context.sop with Some op->op
              |None->fail "E_DATA_SOURCE" "A host call has no checked operator." in
            (try
              op.Flow.Op.check args;
              let scalar=function E.Int n->Some(Param.Int_value n)|Float n->Some(Param.Float_value n)
                |Bool b->Some(Param.Bool_value b)|Text t->Some(Param.Text_value t)|_->None in
              ignore(Flow.Op.result_type op (List.map(fun(n,v)->n,Flow.Value.ty_of v,scalar v)args))
             with Diagnostic.Error d -> raise (Diagnostic.Error d));
            let sources=let direct=geo_of args in
              direct @ List.filter(fun id->not(List.mem id direct))(Attribute_kernel.sources(geometry_arrays_data args)) in
            let inputs=List.map(fun id->
              let actual=match Hashtbl.find_opt handles id with Some(actual,_,_)->actual
                |None->fail "E_DATA_SOURCE" "A cooked handle is missing from this execution." in
              ignore(ok(handle actual id));
              Sop.Node.Private.make ~operation:"lisp_host_capture" ~version:1 ~parameters:(string_of_int id)
                ~cook_mode:Sop.Node.Generator ~inputs:[||] ~dependencies:Sop.Context.Dependencies.static
                (fun ~node_id:_ _context _inputs->
                  (Result.map(fun(output:Sop.Session.output)->Sop.Node.Private.{payload=output.payload;
                          instances=output.instances})(snapshot actual id))))sources in
            let node=geometry_arrays_node ~strict_body:true ~host:true ~operation:name ~args ~sources inputs in
            let id= !next in decr next;
            Hashtbl.add handles id(ty,Some node,ref None);Ok(E.Deferred(ty,id)) in
          let call ty catalog name args =
            if catalog && not(List.exists(fun(n:Workspace.node_definition)->n.kind.qualified=name)checked.nodes)
              && not(List.exists(fun factory->"sop/"^Edit.factory_key factory=name)factories)
              then fail "E_DATA_SOURCE" "A host call has no checked catalog kind.";
            if not catalog && Option.is_none(Flow.Op.find ~extra:checked.ops name Context.sop)
              then fail "E_DATA_SOURCE" "A host call has no checked operator.";
            let selected=List.mem_assoc "selection" args || List.mem_assoc "default" args in
            let args=if not catalog && name="sop/with_attr"
                && (if selected then (match List.assoc_opt "values" args with
                    |Some(E.Float_array _|Vec2_array _|Vec3_array _|Vec4_array _)->true|Some value->constant value|_->false)
                  else match List.assoc_opt "geometry" args,List.assoc_opt "values" args with
                    |Some(E.Deferred(ty,_)),Some(E.Record _ as values) when ty=Ty.geometry && deferred values->
                        (match List.assoc_opt "type" args with
                         |None->true|Some(E.Text kind)->List.mem kind["vec2";"vec3";"vec4"]|_->false)
                    |_->false)
              then begin
                if Sop.Context.cancelled context then fail "E_CANCELLED" "Attribute kernel cancelled.";
                if not selected then ignore(attribute_options args);
                force_args ~keep_constant:true args
              end else args in
            let borrowed=if catalog || name<>"sop/with_attr" then Ok None else
              match List.assoc_opt "geometry" args,List.assoc_opt "attribute" args,List.assoc_opt "values" args with
              |Some(E.Deferred(actual,id)),Some(E.Text name),Some values when actual=Ty.geometry->
                  if Sop.Context.cancelled context then error "E_CANCELLED" "Attribute kernel cancelled." else
                  let owner,kind=attribute_options args in
                  let target=Option.get(geometry id) in
                  let materialized=match values with
                    |E.Float_array _|Int_array _|Text_array _|Vec2_array _|Vec3_array _|Vec4_array _->true
                    |Record fields->if not selected && List.mem kind["vec2";"vec3";"vec4"] then
                        not(E.is_live values)
                      else (match List.assoc_opt "offsets" fields,List.assoc_opt "values" fields with
                        |Some(E.Int_array _),Some(E.Float_array _|Int_array _)->true|_->false)
                    |_->false in
                  let selection=List.assoc_opt "selection" args and default=List.assoc_opt "default" args in
                  Result.bind(if selection<>None || default<>None then Ok None else
                    Attribute_kernel.write_source ~geometry ~owner ~kind name values target)(fun result->
                  let selected_materialized=match selection,default with
                    |None,None->true|Some(E.Int_array _),Some(E.Float _|Int _|Vec2 _|Vec3 _|Vec4 _)->true|_->false in
                  let result=match result with Some _->Ok result|None when constant values && selected_materialized->
                    Attribute_kernel.write_constant_source ~grain:(Sop.Context.grain context) ~owner ~kind name
                      ?selection ?default values target
                    |None when materialized && selected_materialized->
                    (* The former checked-call input boundary validated before
                       the writer checked destination cardinality. *)
                    Flow.Value.validate values;
                    Result.map Option.some(match selection,default with
                      |None,None->Attribute_kernel.write ~grain:(Sop.Context.grain context) ~owner ~kind name values target
                      |Some selection,Some default->
                          Flow.Value.validate selection;Flow.Value.validate default;
                          Attribute_kernel.write_selected ~grain:(Sop.Context.grain context) ~owner ~kind name
                            ~values ~selection ~default target
                      |_->error "E_ATTR_SELECTION" "Selected writes require both selection and default.")
                    |None->Ok None in
                  Result.bind result(fun result->if Sop.Context.cancelled context then
                      error "E_CANCELLED" "Attribute kernel cancelled." else
                  Ok(Option.map(fun geometry->
                    ignore(ok(snapshot Ty.geometry id));
                    register_output Ty.geometry {payload=Sop.Payload.Geometry geometry;instances=None})result)))
              |_->Ok None in
            Result.bind borrowed(function Some result->Ok result|None->
            let args=force_args args in
            if not catalog && is_geometry_arrays name then host_geometry_arrays ty name args else
            let call_context=if ty=Ty.image then Flow.Context.image else Context.sop in
            let form=Syntax.make(List[]) in
            let capture_factories=ref [] and overrides=ref [] in
            let inputs=List.map(fun(name,value)->
              let ty=Flow.Value.ty_of value in
              let default=cooked_term capture_factories ty value in
              if default=None then overrides:=(name,value)::!overrides;
              name,ty,default)args in
            let args=List.map(fun(name,ty,_)->name,Workspace.{path=None;ty;form;node=Ref_binding(name,[])})inputs in
            let body=Workspace.{path=None;ty;form;node=(if catalog then
                Call{kind=name;ctx=call_context;args} else Op{op=name;args;skip=[]})} in
            let graph=Workspace.{name="lisp_host_call";context=Context.sop;inputs;body;form} in
            Result.bind(lower_checked ~reference ~gpu ~strict_body:true
              ~factories:(!capture_factories@factories) ~inputs:[graph.name,!overrides]
              {checked with graphs=[graph]})(fun lowered->
              match lowered.graphs with
              |[graph]->(match graph.root with None->error "E_DATA_SOURCE" "A cooked call returned no result."
                |Some root->Result.bind(Result.map_error(Diagnostic.error ~code:"E_LOWER")
                    (Edit.compile_node graph.network.geometry ~node_id:root))(fun node->
                  let id= !next in decr next;Hashtbl.add handles id(ty,Some node,ref None);Ok(E.Deferred(ty,id))))
              |_->error "E_DATA_SOURCE" "A cooked call must return one typed result.")) in
          let resolve value=try match value with
            |E.Source("$core/diagnostic_index",Ty.Int,["rows",rows;"policy",E.Text policy])->
                let grain=match policy with "first"->Ok None|"range-last"->Ok(Some(Sop.Context.grain context))
                  |_->error "E_DIAGNOSTIC_CONTEXT" "Unknown host diagnostic policy." in
                Result.bind grain(fun grain->
                  let selected=try
                    let check()=if Sop.Context.cancelled context then
                      Flow.Value.fail "cancelled" "geometry operation was cancelled" in
                    Ok(Flow.Value.Private.error_row_index ?grain ~check rows)
                   with Diagnostic.Error d -> Error d in
                   Result.bind selected(function Some index->Ok(E.Int index)
                     |None->error "E_DIAGNOSTIC_CONTEXT" "Host diagnosis needs a failing row."))
            |E.Source("$core/diagnostic_index",_,_)->error "E_DIAGNOSTIC_CONTEXT" "Host diagnosis needs typed rows and policy."
            |E.Source("$core/complete",ty,["value",E.Deferred(actual,id)]) when actual=ty && Ty.is_cooked ty->
                Result.map(fun _->E.Deferred(ty,id))(snapshot ty id)
            |E.Source("$core/complete",ty,["value",E.No_geo]) when ty=Ty.geometry->Ok E.No_geo
            |E.Source("$core/complete",_,_)->error "E_DATA_SOURCE" "Protected completion needs one matching cooked handle."
            |E.Source("$core/cooked",ty,["id",E.Int id])->Result.map(fun _->E.Deferred(ty,id))(handle ty id)
            |E.Source(("$core/operator"|"$core/catalog" as kind),ty,args)->
                if not(Ty.is_cooked ty)then error "E_DATA_SOURCE" "Host calls return a declared cooked type." else
                (match List.assoc_opt "$name" args with
                 |Some(E.Text name)->call ty (kind="$core/catalog") name
                     (List.filter(fun(name,_)->name<>"$name" && name<>"$context")args)
                 |_->error "E_DATA_SOURCE" "A host call needs its checked kind.")
            |value->Attribute_kernel.resolve ~geometry value
            with Diagnostic.Error d->Error d in
          let result=Flow_ir.Core_exec.execute ~resolve
            ~force:(fun program->Flow_ir.Packed.force ~strict:true ~resolve program ~live:(Sop.Context.input context))
            prepared ~live:(Sop.Context.input context) in
          let result=Result.bind result(function
            |E.Deferred(ty,id) when ty=Ty.geometry->snapshot ty id
            |E.No_geo->error "E_LOWER" "Cooked selection returned empty geometry."
            |_->error "E_DATA_SOURCE" "Cooked selection must return a geometry handle.") in
          (Result.map(fun(output:Sop.Session.output)->Sop.Node.Private.{payload=output.payload;
              instances=output.instances})result))) in
      Sop.Node.Private.with_facts {(Sop.Node.facts node) with reads=["*"];writes=["*"]} node
    let attribute_node ?state ?elems ?selection ?default ?(owner=Rdk.Attribute.Point)
        ?(kind="vec3") ~name ~values ~sources inputs =
      attribute_node ~strict_body ~reference ~profile ~approx:checked.approx ~gpu ~kernel_source
        ?state ?elems ?selection ?default ~owner ~kind ~name ~values ~sources inputs
    let group_data = group_data and attribute_data = attribute_data
    let group_node ?state ?elems ~args ~sources inputs =
      group_node ~strict_body ~reference ~profile ~approx:checked.approx ~kernel_source
        ?state ?elems ~args ~sources inputs
    let plan = evaluated.plan
    let external_kernel fn = List.find_map (fun (candidate,source) ->
      if same_function candidate fn then Some source else None) kernel_bindings
    let image_sites = Array.map (fun (node : E.node) ->
      if node.kind = "image/map" then Option.join
        (Hashtbl.find_opt image_origins (node.inst, node.site, node.iter)) else None) plan.nodes
    let ids = ref compiled_ids and site_table = Hashtbl.create 64
    and site_list = ref (List.rev sites) and site_count = ref (List.length sites)
    let () = List.iteri (fun i path -> Hashtbl.replace site_table path i) sites
    let site_index path = match Hashtbl.find_opt site_table path with
      | Some i -> i
      | None ->
          let i = !site_count in
          incr site_count; Hashtbl.add site_table path i;
          site_list := path :: !site_list; i
    let compiled_id (node : E.node) =
      let key = node.inst :: site_index node.site
        :: List.map (fun k -> -(k + 1)) node.iter in
      match Instance_path.Map.find_opt key !ids with
      | Some id -> id
      | None ->
          let id = Sop.Node.Private.fresh_id () in
          ids := Instance_path.Map.add key id !ids; id
    let compiled = Array.make (Array.length plan.nodes) 0
    let int_arg (n : E.node) k = match List.assoc_opt k n.args with
      | Some (E.Int i) -> i | _ -> fail "E_LOWER" ("Zone node without " ^ k)
    (* does an argument of a template node read [t] (not only its element)? *)
    let reads_t = E.frame_dependent
    let templ = Array.make (Array.length plan.nodes) false
    let () = Array.iter (fun (n : E.node) -> if is_zone n.kind then
      for id = int_arg n "lo" to int_arg n "hi" - 1 do templ.(id) <- true done) plan.nodes
    (* what a zone reads: its collection and the outside geometry of its template *)
    let zinfos = Hashtbl.create 8
    let rec deps (n : E.node) =
      if is_zone n.kind then (match n.args with (_, E.Deferred ((Flow.Ty.Named "geometry"), src)) :: _ -> src | _ -> fail "E_LOWER" "Zone without geometry")
        :: (zinfo n).captures
      else if is_geometry_arrays n.kind then let direct=geo_of n.args in
        direct @ List.filter(fun id->not(List.mem id direct))(Attribute_kernel.sources(geometry_arrays_data n.args))
      else if n.kind = "sop/with_attr" || n.kind="sop/with_group" then
        let geometry = List.assoc "geometry" n.args in
        let root = match geometry with E.Deferred (_, id) -> Some id | E.No_geo -> None
          | _ -> fail "E_LOWER" "Attribute write without geometry" in
        Option.to_list root @ List.filter (fun id -> Some id <> root)
          (Attribute_kernel.sources (if n.kind="sop/with_group" then group_data n.args else attribute_data n.args))
      else let direct = geo_of n.args in
        let rec captures = function
          | E.Fn fn as value -> (match external_kernel fn with
              |Some source->[source]|None->Attribute_kernel.sources value)
          | E.List values -> Array.to_list values |> List.concat_map captures
          | _ -> [] in
        direct @ (List.concat_map (fun (_,value) -> captures value) n.args
          |> List.sort_uniq Int.compare |> List.filter (fun id -> not (List.mem id direct)))
    and zinfo (n : E.node) = match Hashtbl.find_opt zinfos n.id with
      | Some z -> z
      | None ->
          let lo = int_arg n "lo" and hi = int_arg n "hi" and root = int_arg n "body" in
          let seen = Hashtbl.create 16 and caps = ref [] in
          let rec visit j =
            if j >= lo && j < hi then begin
              if not (Hashtbl.mem seen j) then begin
                Hashtbl.add seen j (); List.iter visit (deps plan.nodes.(j)) end
            end else if not (List.mem j !caps) then caps := j :: !caps in
          visit root;
          let ekey = match List.assoc_opt "element" n.args with Some (E.Text k) -> Some k | _ -> None in
          let live = let rec any id = id < hi
              && (List.exists (fun (_, v) -> reads_t v) plan.nodes.(id).args || any (id + 1)) in
            any lo in
          let stateful = let rec any id = id < hi
              && (List.exists (fun (_, v) -> E.state_dependent v) plan.nodes.(id).args || any (id + 1)) in
            any lo in
          let z = { plan_id = n.id; lo; hi; preview = None; root; live; stateful;
            order = Hashtbl.fold (fun id () l -> id :: l) seen [] |> List.sort Int.compare;
            captures = List.rev !caps; ekey;
            kind = if n.kind = "zone/points" then Zone.Points else Zone.Pieces;
            key = (match List.assoc_opt "key" n.args with Some (E.Text k) -> Some k | _ -> None);
            base = 0; count = Atomic.make (-1); positions = Atomic.make [||] } in
          Hashtbl.add zinfos n.id z; z
    let params_cache = Hashtbl.create 32
    let parameters factory =
      let key = Edit.factory_key factory in
      match Hashtbl.find_opt params_cache key with
      | Some parameters -> parameters
      | None ->
          let parameters = ok (Port.parameters (Edit.factory_fields factory)) in
          Hashtbl.add params_cache key parameters; parameters
    (* a helper that names no context operator is its own specialization: shared across instances *)
    let def_context_use =
      let use = lazy (List.map (fun def -> def, Context_kernel.uses_context def) checked.defs)
      and lock = Mutex.create () in
      fun () -> Mutex.protect lock (fun () -> Lazy.force use)
    let lisp_factory ?(functions = []) ?provenance (definition : Workspace.node_definition) =
      let declaration_source=lazy(cached_digest checked [definition] (fun () -> Context_kernel.declaration_source checked definition)) in
      let rec function_elements = function
        | E.Fn fn -> [fn]
        | E.List values -> Array.to_list values |> List.concat_map function_elements
        | _ -> [] in
      let functions = List.filter_map (fun (slot : Check.slot) ->
        let values = List.filter_map (fun (name,value) -> if name=slot.name then Some value else None) functions
          |> List.concat_map function_elements |> List.map (fun fn -> E.Fn fn) in
        match values with
        | [] -> None
        | [value] when not slot.rest -> Some (slot.name,value)
        | _ -> Some (slot.name,E.List (Array.of_list values))) definition.slots in
      let key = String.sub definition.kind.qualified 4
        (String.length definition.kind.qualified - 4) in
      let record_profile profile =
        if !Private.record then Private.profiles := profile :: !Private.profiles in
      let lower_nested ~gpu ~kernel_bindings ~factories ~inputs checked =
        lower_checked ~reference ~gpu ~strict_body:true ~defer_body_errors:true ~kernel_bindings
          ~factories ~inputs checked in
      let build_body bindings ~label ~context inputs parameters =
        build_lisp_body ~gpu ~lower_nested ~record_profile ~checked ~definition ~provenance ~label
          ~context ~inputs ~parameters ~bindings ~functions ~factories ~def_context_use in
      (* A top-level body build is a pure function of the key below, so an edit elsewhere reuses it. Key: declaration,
         label, reference flag, approx, provenance, bindings, context seed/frame/time, input ids, parameters; an
         entry is valid only for the same factories and GPU backend+policy. Bypassed: callable slots, strict nested
         lowerings, kernel bindings, recorded profiles, non-data parameters (key_of raises). *)
      let build bindings ~label ~context inputs parameters =
        let uncached () = build_body bindings ~label ~context inputs parameters in
        if functions<>[] || strict_body || kernel_bindings<>[] || !Private.record then uncached () else
        match String.concat "\000" [Lazy.force declaration_source; label; string_of_bool reference;
            Marshal.to_string (Workspace.Paths.elements checked.approx, provenance, bindings, Sop.Context.seed context,
              Sop.Context.frame context, Int64.bits_of_float (Sop.Context.time context),
              List.map Sop.Node.id inputs) [];
            String.concat "\001" (List.map (fun (name, value) ->
              name ^ "=" ^ Flow.Value.key_of ~residual:(fun _ -> raise Exit) value) parameters)] with
        | exception Exit -> uncached ()
        | key ->
            let cached = Mutex.protect build_lock (fun () -> Build_cache.find_opt build_cache key) in
            (match cached with
             | Some (owner, old_gpu, result) when owner == factories && same_gpu old_gpu gpu -> Atomic.incr Private.cache_hits; result
             | _ -> let result = uncached () in
                 Mutex.protect build_lock (fun () ->
                   Build_cache.add build_cache key (factories, gpu, result)); result) in
      let category = if definition.category=[] then ["Custom"] else definition.category in
      let construct inputs =
          let next = ref 0 in
          let connected = List.mapi (fun slot input ->
            slot, Option.map (fun _ -> let index = !next in incr next; index) input) inputs in
          let bindings = List.mapi (fun slot (s : Check.slot) -> s.name,
            List.filter_map (fun (index, input) ->
              if index=slot || (s.rest && index>=slot) then input else None) connected)
            definition.slots in
          let slots = Array.of_list definition.slots in
          let geometry_inputs = List.filter_map Fun.id (List.mapi (fun position input ->
            Option.map (fun _ ->
              let slot = slots.(min position (Array.length slots - 1)) in
              Workspace.slot_ty ~slot definition.kind = Ty.geometry) input) inputs) in
          Lisp_node.node ~geometry_inputs ~unbound_function:(List.exists
              (fun (slot : Check.slot) -> match slot.ty with Some (Ty.Fn _) ->
                not (List.mem_assoc slot.name functions) && (slot.required ||
                  Option.value ~default:[] (List.assoc_opt slot.name bindings) <> []) | _ -> false)
              definition.slots)
            ~operation:definition.operation ~version:definition.version ~label:key
            ~fields:definition.fields ~normalizers:(Encoded_parameters.for_node checked definition)
            ~source:(Lazy.force (if functions=[] then declaration_source else kernel_source)
              ^ Option.fold ~none:"" ~some:(Printf.sprintf ";provenance=%d") provenance) ~build:(build bindings)
            (List.filter_map Fun.id inputs) in
      if definition.slots=[] then
        Edit.factory ~key ~operation:definition.operation ~label:definition.label ~category ~fields:definition.fields ~arity:0
          (fun _ -> construct [])
      else Edit.factory_slots ~key ~operation:definition.operation ~label:definition.label ~category
        ~slots:(List.map (fun (s : Check.slot) -> s.name) definition.slots)
        ~keyword_inputs:definition.keyword_inputs
        ~fields:definition.fields
        ~input_types:(List.map (fun (s : Check.slot) ->
          Ty.to_string (Workspace.slot_ty ~slot:s definition.kind)) definition.slots)
        ~inputs:(List.map (fun (s : Check.slot) -> match s.required,s.rest with
          |true,false->Edit.Required|false,false->Optional|true,true->Rest|false,true->Optional_rest)
          definition.slots)
        construct
    let lisp_factories = List.map (fun definition -> lisp_factory definition) checked.nodes
    let find_factory key = match List.find_opt (fun factory ->
        Edit.factory_key factory = key) factories with
      | Some factory -> factory
      | None -> match List.find_opt (fun factory -> Edit.factory_key factory=key) lisp_factories with
          | Some factory -> factory
          | None -> fail "E_LOWER" ("Linked catalog has no " ^ key)
    let pending = ref [] and provenance = ref Network.Int_map.empty and tags = ref 0
    and zones = ref []
    let prepared = Hashtbl.create 256
    let curve_points args = match List.assoc_opt "points" args with
      | Some (E.List values) -> Array.map (function
          | E.Vec3 (x, y, z) -> x, y, z
          | _ -> fail "E_LOWER" "sop/curve points must be vec3") values
      | _ -> fail "E_LOWER" "sop/curve needs :points"
    (* B.F: a Lisp point deformer whose only input is another such deformer's single-consumer output
       reads that deformer's P values directly; the producer's own cook is skipped. *)
    let consumers = lazy (let counts = Array.make (Array.length plan.nodes) 0 in
      Array.iter (fun (n : E.node) -> List.iter (fun id -> counts.(id) <- counts.(id) + 1) (deps n)) plan.nodes;
      let rec mark = function
        | E.Deferred (_, id) -> counts.(id) <- counts.(id) + 1
        | E.List values -> Array.iter mark values
        | E.Record fields -> List.iter (fun (_, value) -> mark value) fields
        | _ -> () in
      Array.iter (fun (i : E.instance) -> mark i.result; List.iter (fun (_, value) -> mark value) i.inputs) plan.instances;
      counts)
    let lisp_definition kind = List.find_opt (fun (d : Workspace.node_definition) -> d.kind.qualified = kind) checked.nodes
    let rec fused_view (n : E.node) = match lisp_definition n.kind with
      | Some d when not templ.(n.id) && Context_kernel.fusable d ->
          let slot = (List.hd d.slots).name in
          let own = Some (d, n.args) in
          (match List.assoc_opt slot n.args with
           | Some (E.Deferred (_, id)) when (Lazy.force consumers).(id) = 1 && not templ.(id) ->
               let u = plan.nodes.(id) in
               if List.exists (fun (_, v) -> E.is_live v) u.args then own else
               (match fused_view u with
                | Some (du, uargs) ->
                    let context_free = List.for_all (fun op -> op = "sop/provenance_base")
                      (Context_kernel.context_ops_reached ~inputs:u.args ~defs:checked.defs du.body) in
                    (match if context_free then Context_kernel.fuse d du ~baked:u.args else None with
                     | Some fused -> Some (fused, List.map (fun (name, v) ->
                         if name = slot then name, List.assoc slot uargs else name, v) n.args)
                     | None -> own)
                | None -> own)
           | _ -> own)
      | _ -> None
    let rec prepare (node : E.node) =
      match Hashtbl.find_opt prepared node.id with
      | Some p -> p
      | None ->
          let template = templ.(node.id) in
          let cid = compiled.(node.id) in
          let live = List.filter (fun (_, v) -> E.is_live v) node.args in
          let resource = node.ty=Ty.image && node.kind="exact"
            || node.kind="image/load" || node.kind="image/render" || node.kind="image/map" in
          (* a template node keeps its live arguments: each element forces them *)
          let args = if template || node.kind = "sop/with_attr" || node.kind="sop/with_group" || is_geometry_arrays node.kind || resource then node.args
            else List.map (fun (n, v) -> n, at_zero v) node.args in
          let args = if node.kind <> "sop/material" then args else
            List.concat_map (function
              | "material", E.Struct ("material/standard", _, fields) ->
                  let get name default = Option.value ~default (List.assoc_opt name fields) in
                  ["material", get "name" (E.Text "");
                   "color", get "color" (E.Vec3 (1.,1.,1.));
                   "roughness", get "roughness" (E.Float 0.4);
                   "emission", get "emission" (E.Vec3 (0.,0.,0.))]
              | arg -> [arg]) args in
          let dynamic = ref [] in
          let p = match node.kind with
            | operation when is_geometry_arrays operation ->
                let sources=deps node in
                let factory=Edit.factory ~key:(geometry_arrays_key operation) ~label:(geometry_arrays_label operation) ~category:["Flow";"Topology"]
                  ~arity:(List.length sources)(geometry_arrays_node ~operation ~args ~sources)in
                {cid;factory;arity=List.length sources;changes=[];dynamic=[];zone=None;kernels=[];
                  slots=List.mapi(fun index source->index,source)sources}
            | operation when List.mem operation metadata_lowering_operations ->
                let sources=geo_of args in
                let factory=metadata_factory ~operation ~args sources in
                {cid;factory;arity=List.length sources;changes=[];dynamic=[];zone=None;kernels=[];
                  slots=List.mapi(fun i source->i,source)sources}
            | "sop/with_group" ->
                let sources=deps node in
                let factory=Edit.factory ~key:"flow.with_group" ~label:"Write group"
                  ~category:["Flow"] ~arity:(List.length sources) (group_node ~args ~sources) in
                {cid;factory;arity=List.length sources;changes=[];dynamic=[];zone=None;kernels=[];
                  slots=List.mapi(fun i source->i,source)sources}
            | "image/load" | "image/render" | "image/map" | "exact" when resource ->
                let factory=Edit.factory ~key:node.kind ~label:node.kind ~category:["Image"] ~arity:0
                  (fun _->resource_image node.kind (Error(Diagnostic.error ~code:"E_IMAGE"
                    "This image needs an initial-domain image resolver.")))in
                {cid;factory;slots=[];arity=0;changes=[];dynamic=[];zone=None;kernels=[]}
            | "sop/with_attr" ->
                let owner,kind=attribute_options args in
                let name = match List.assoc "attribute" args with
                  | E.Text name -> name | _ -> fail "E_LOWER" "Attribute name must be static text" in
                let values = List.assoc "values" args in
                let sources = match List.assoc "geometry" args with
                  | E.No_geo -> -1 :: deps node | _ -> deps node in
                let factory = Edit.factory ~key:"flow.with_attr" ~label:"Write attribute"
                  ~category:["Flow"] ~arity:(List.length sources)
                  (attribute_node ?selection:(List.assoc_opt "selection" args) ?default:(List.assoc_opt "default" args)
                    ~owner ~kind ~name ~values ~sources) in
                {cid; factory; arity = List.length sources; changes = []; dynamic = []; zone = None; kernels = [];
                  slots = List.filter_map (fun (i, source) -> if source < 0 then None else Some (i, source))
                    (List.mapi (fun i source -> i, source) sources)}
            | "sop/delete_attr" ->
                let name = match List.assoc "attribute" args with
                  | E.Text name -> name | _ -> fail "E_ATTR_NAME" "An attribute pattern must be text" in
                let owner = match List.assoc_opt "owner" args with
                  | None -> "point" | Some (E.Text owner) -> owner
                  | Some _ -> fail "E_ATTR_OWNER" "An attribute owner must be text" in
                let sources = geo_of args in
                let factory = Edit.factory ~key:"flow.delete_attr" ~label:"Delete attribute"
                  ~category:["Flow"] ~arity:1
                  (function [input] -> Attribute_metadata.delete ~owner ~pattern:name input
                    | _ -> invalid_arg "Attribute deletion needs one geometry") in
                {cid;factory;arity=1;changes=[];dynamic=[];zone=None;kernels=[];
                  slots=List.mapi (fun i source -> i,source) sources}
            | "sop/curve" ->
                let encode points = [Curve.parameter, Param.Text_value (Curve.encode (curve_points points))] in
                let closed = match List.assoc_opt "closed" args with
                  | None -> []
                  | Some (E.Bool closed) -> [Curve.closed_parameter, Param.Bool_value closed]
                  | Some _ -> fail "E_LOWER" "sop/curve :closed must be a constant bool" in
                let filled = match List.assoc_opt "filled" args with
                  | None -> []
                  | Some (E.Bool filled) -> [Curve.filled_parameter, Param.Bool_value filled]
                  | Some _ -> fail "E_LOWER" "sop/curve :filled must be a constant bool" in
                let changes = closed @ filled @ (match List.assoc_opt "points" args with
                  | Some points when E.is_live points ->
                      dynamic := [ points, (fun v -> encode [ "points", v ]) ]; []
                  | _ -> encode args) in
                {cid; factory = Curve.factory; arity = 0; slots = []; changes;
                 dynamic = []; zone = None; kernels = []}
            | "zone/points" | "zone/pieces" ->
                let z = { (zinfo node) with base = !tags; count = Atomic.make (-1);
                                            positions = Atomic.make [||] } in
                tags := z.base + Zone.max_elements;
                let sources = (match geo_of (List.filter (fun (k, _) -> k = "geometry") args) with
                  | [ src ] -> src | _ -> fail "E_LOWER" "Zone without geometry") :: z.captures in
                List.iter (fun id -> let n = plan.nodes.(id) in
                  if n.kind <> "zone/element" then ignore (prepare n)) z.order;
                let base_site = plan.nodes.(z.root).site in
                zones := {cid; site = node.site; iter = node.iter; body_site = base_site;
                          base = z.base; count = z.count; ekey = z.ekey;
                          positions = z.positions} :: !zones;
                let factory = Edit.factory_slots ~key:"zone" ~label:"Loop over geometry"
                  ~slots:["input"] ~category:["Copy"] ~inputs:[Edit.Rest]
                  (fun nodes -> make_zone ~outer:[] z (Array.of_list (List.filter_map Fun.id nodes))) in
                {cid; factory; arity = List.length sources; changes = []; dynamic = []; kernels = [];
                 zone = Some z; slots = List.mapi (fun i s -> i, s) sources}
            | kind ->
                let args = if kind="image/noise" then List.map(fun(name,value)->
                  (if name="freq" then "frequency" else name),value)args else args in
                let args, fused_definition = match fused_view node with
                  | Some (d, fused_args) when fused_args != node.args ->
                      let slot = (List.hd d.slots).name in
                      List.map (fun (name, v) -> if name = slot then name, List.assoc slot fused_args else name, v) args, Some d
                  | _ -> args, None in
                let key = match String.split_on_char '/' kind with
                  | ["sop"; key] -> key
                  | ["image"; "noise"] -> "image_noise"
                  | _ -> fail "E_LOWER" ("Cannot lower " ^ kind) in
                let factory = if key = "image_noise" then Sop.Image_nodes.noise_factory
                  else match List.find_opt (fun (d : Workspace.node_definition) ->
                    d.kind.qualified = kind) checked.nodes with
                    | Some definition ->
                        (* the editor's provenance: a Lisp sop/merge tags input i with base + i *)
                        let tag_base = if kind <> "sop/merge" || not provenance_tags then None else begin
                          let sources = geo_of args in
                          let base = !tags in
                          tags := base + List.length sources;
                          List.iteri (fun index source ->
                            let s = plan.nodes.(source) in
                            provenance := Network.Int_map.add (base + index)
                              {merge = cid; input = index; source = compiled.(source);
                               site = s.site; iter = s.iter} !provenance) sources;
                          Some base end in
                        lisp_factory ~functions:args ?provenance:tag_base (Option.value fused_definition ~default:definition)
                    | None -> find_factory key in
                let names = Edit.factory_slot_names factory
                and parameters = parameters factory in
                let rest = List.find_index (function Edit.Rest | Optional_rest -> true | _ -> false)
                  (Edit.factory_inputs factory) in
                let arity = ref (List.length names) in
                let next_rest = ref (Option.value ~default:0 rest) in
                let slots = ref [] and changes = ref [] and kernels = ref [] in
                let add_function index declared_index name fn =
                  match external_kernel fn with
                  | Some source -> slots := (index,source) :: !slots
                  | None ->
                      let signature = match Ty.of_string (List.nth (Edit.factory_input_types factory) declared_index) with
                        | Some (Ty.Fn (Some signature)) -> signature
                        | _ -> fail "E_LOWER" ("Input " ^ name ^ " does not declare a function signature") in
                      let cid = compiled_id {node with site = node.site @ [Printf.sprintf "~kernel:%s:%d" name index]} in
                      kernels := {index; cid; signature; fn;
                        identity = Sop.Node.Private.fresh_id ();
                        sources = Attribute_kernel.sources (E.Fn fn)} :: !kernels in
                let rec add_rest name = function
                  | E.No_geo -> ()
                  | E.Deferred (ty, id) when Ty.is_cooked ty ->
                      slots := (!next_rest,id) :: !slots;
                      incr next_rest; arity := max !arity !next_rest
                  | E.Fn fn ->
                      add_function !next_rest (Option.get rest) name fn;
                      incr next_rest; arity := max !arity !next_rest
                  | E.List values -> Array.iter (add_rest name) values
                  | _ -> fail "E_LOWER" "Repeated inputs need cooked values or functions" in
                List.iter (fun (name, value) ->
                  match List.find_index (( = ) name) names, value with
                  | Some index, value when Some index = rest -> add_rest name value
                  | Some _, E.No_geo -> ()
                  | Some index, E.Deferred (ty, id) when Ty.is_cooked ty -> slots := (index, id) :: !slots
                  | Some index, E.Fn fn ->
                      add_function index index name fn
                  | Some _, _ -> fail "E_LOWER" ("Slot " ^ name ^ " needs geometry")
                  | None, value ->
                      let parameter = ok (Port.find_parameter parameters name) in
                      if E.is_live value then
                        dynamic := (value, changes_of parameter) :: !dynamic
                      else changes := List.rev_append (changes_of parameter value)
                        !changes) args;
                {cid; factory; arity = !arity; dynamic = []; zone = None;
                 slots = List.rev !slots; changes = List.rev !changes; kernels = List.rev !kernels} in
          let p = { p with dynamic = List.rev !dynamic } in
          if not template && node.kind <> "sop/with_attr" && node.kind<>"sop/with_group" && not(is_geometry_arrays node.kind) && not resource then begin
            let parameters = parameters p.factory in
            pending := List.rev_append (List.filter_map (fun (field, value) ->
              let field = if node.kind="image/noise" && field="freq" then "frequency" else field in
              if List.exists (fun (parameter : Port.parameter) -> parameter.path=field) parameters
              then Some {node=cid; field; value} else None) live) !pending
          end;
          Hashtbl.add prepared node.id p; p
    (* the zone node of a lowered graph: it cooks [z.root] once per element *)
    and kernel_node ?state ?elems (k : kernel_input) inputs =
      Function_kernel.node ?state ~reference ?elems ~identity:k.identity
        ~signature:k.signature ~fn:k.fn ~sources:k.sources inputs
      |> Sop.Node.Private.restore_id k.cid |> edit
    and make_zone ?state ~outer z inputs =
      let stamp = Option.fold ~none:"" ~some:E.state_stamp state in
      let stamp = match z.preview with
        | None -> stamp
        | Some choice -> stamp ^ ";preview=" ^ Digest.to_hex (Digest.string (Marshal.to_string choice [])) in
      let select = Option.bind z.preview (fun (_, probes) ->
        List.nth_opt probes (List.length plan.nodes.(z.plan_id).iter)) in
      Zone.node ~report:(fun elements ->
          Atomic.set z.count (Array.length elements);
          Atomic.set z.positions (Array.map (fun (e : Zone.element) -> e.position) elements))
        ~live:z.live ~stamp ~kind:z.kind ?key:z.key ?select ~source_attribute
        ~source_base:z.base ~inputs ~body:(fun ~inputs ~context ->
          instantiate ?state ~outer ~live:(Sop.Context.input context) z ~inputs) ()
    (* Re-root a geometry-loop template at the selected node. Unused bindings
       may have different captures from the authored result, so follow its cone. *)
    and focus z target probes =
      let contains z = target >= z.lo && target < z.hi in
      let inner = ref None in
      for id = z.lo to z.hi - 1 do
        if is_zone plan.nodes.(id).kind then begin
          let candidate = Option.get (prepare plan.nodes.(id)).zone in
          if contains candidate then match !inner with
            | Some old when old.hi - old.lo >= candidate.hi - candidate.lo -> ()
            | _ -> inner := Some candidate
        end
      done;
      let root = match !inner with Some inner -> inner.plan_id | None -> target in
      let seen = Hashtbl.create 16 and captures = ref [] in
      let rec visit id =
        if id >= z.lo && id < z.hi then begin
          if not (Hashtbl.mem seen id) then begin
            Hashtbl.add seen id ();
            let node = plan.nodes.(id) in
            let sources = if id = root && is_zone node.kind then
              let inner = focus (Option.get (prepare node).zone) target probes in
              int_geo node :: inner.captures else deps node in
            List.iter visit sources
          end
        end else if not (List.mem id !captures) then captures := id :: !captures in
      visit root;
      { z with root; preview = Some (target, probes); captures = List.rev !captures;
        order = Hashtbl.fold (fun id () order -> id :: order) seen [] |> List.sort Int.compare }
    and int_geo node = match geo_of (List.filter (fun (key, _) -> key = "geometry") node.E.args) with
      | [id] -> id | _ -> fail "E_LOWER" "Zone without geometry"
    (* one element's copy of the template, over the zone's own inputs *)
    and instantiate ?state ~outer ~live z ~inputs =
      let build ~outer bound id =
        let n = plan.nodes.(id) in
        let p = prepare n in
        let options = List.init p.arity (fun i ->
          match List.find_opt (fun (k : kernel_input) -> k.index = i) p.kernels with
          | Some k -> Some (kernel_node ?state ~elems:outer k
              (List.map (Hashtbl.find bound) k.sources))
          | None -> Option.bind (List.assoc_opt i p.slots) (Hashtbl.find_opt bound)) in
        let node = match p.zone with
          | Some inner ->
              let inner = match z.preview with
                | Some (target, probes) when target >= inner.lo && target < inner.hi -> focus inner target probes
                | _ -> inner in
              let options = int_geo n :: inner.captures
                |> List.map (Hashtbl.find_opt bound) in
              make_zone ?state ~outer inner (Array.of_list (List.filter_map Fun.id options))
          | None when n.kind = "sop/with_attr" && List.assoc "geometry" n.args <> E.No_geo ->
              let name = match List.assoc "attribute" n.args with
                | E.Text name -> name | _ -> fail "E_LOWER" "Attribute name must be static text" in
              let owner,kind=attribute_options n.args in
              attribute_node ?state ~elems:outer ?selection:(List.assoc_opt "selection" n.args) ?default:(List.assoc_opt "default" n.args) ~owner ~kind ~name
                ~values:(List.assoc "values" n.args) ~sources:(deps n)
                (List.filter_map Fun.id options)
          | None when n.kind="sop/with_group" ->
              group_node ?state ~elems:outer ~args:n.args ~sources:(deps n) (List.filter_map Fun.id options)
          | None when is_geometry_arrays n.kind ->
              geometry_arrays_node ?state ~elems:outer ~operation:n.kind ~args:n.args ~sources:(deps n) (List.filter_map Fun.id options)
          | None -> edit (Lisp_node.with_initial_changes ~operation:(Edit.factory_operation p.factory) p.changes (fun () -> Edit.instantiate_optional p.factory options)) in
        let node = edit (Sop.Node.Private.restore_id p.cid node) in
        let changes = p.changes @ List.concat_map (fun (v, changes) ->
          changes (match E.force ?state:(Option.map E.fork_state state) ~elems:outer v ~live with
            | Ok v -> v | Error d -> failwith (Diagnostic.to_string d))) p.dynamic in
        let node = if changes = [] then node
          else fst (edit (Sop.Node.apply_parameters node changes)) in
        Hashtbl.replace bound id node in
      (* what does not read the element is built once per cook of the zone *)
      let shared = Hashtbl.create 16 and varies = Hashtbl.create 16 in
      List.iteri (fun k j -> Hashtbl.replace shared j inputs.(k + 1)) z.captures;
      List.iter (fun id ->
        let n = plan.nodes.(id) in
        if n.kind = "zone/element" || is_zone n.kind || (prepare n).dynamic <> []
           || (n.kind = "sop/with_attr" && E.is_live (attribute_data n.args))
           || (n.kind="sop/with_group" && E.is_live(group_data n.args))
           || (is_geometry_arrays n.kind && E.is_live(geometry_arrays_data n.args))
           (* ponytail: all function inputs vary in a zone; narrow to actual
              element captures if rebuilding constant functions becomes costly. *)
           || (prepare n).kernels <> []
           || List.exists (Hashtbl.mem varies) (deps n)
        then Hashtbl.replace varies id ()
        else build ~outer:[] shared id) z.order;
      fun (element : Zone.element) ->
        let outer = match z.ekey with
          | Some key -> let x, y, z = element.position in (key, E.Vec3 (x, y, z)) :: outer
          | None -> outer in
        let bound = Hashtbl.copy shared in
        List.iter (fun id ->
          if Hashtbl.mem varies id then
            if plan.nodes.(id).kind = "zone/element" then
              Hashtbl.replace bound id (Zone.element_node element)
            else build ~outer bound id) z.order;
        Hashtbl.find bound z.root
    let contexts = List.map (fun (g : Workspace.graph) -> g.name, g.context)
      checked.graphs
    let sop_instance (i : E.instance) =
      Option.fold ~none:false ~some:(fun context -> Ty.is_cooked (Flow.Context.result context))
        (List.assoc_opt i.graph contexts)
    (* every plan node gets its id up front, in plan order, so ids do not
       depend on which network asks first *)
    let () = Array.iter (fun (node : E.node) ->
      if sop_instance plan.instances.(node.inst) && Ty.is_cooked node.ty then
        compiled.(node.id) <- compiled_id node) plan.nodes
    (* volatile: live, or fed by a volatile node (plan order: inputs first).  A loop over geometry
       whose body reads [t] is live, and so are the template nodes its elements are copies of (a copy
       keeps the template's id, so the elements share one volatile slot per node) *)
    let volatile_nodes = ref Network.Int_map.empty
    let volatile = Array.make (Array.length plan.nodes) false
    let () = Array.iter (fun (node : E.node) ->
      if compiled.(node.id) <> 0
         && (List.exists (fun j -> volatile.(j)) (deps node)
              || (is_zone node.kind && (zinfo node).live)
              || (not (is_zone node.kind)
                  && List.exists (fun (_, v) -> if templ.(node.id) || node.kind = "sop/with_attr" || node.kind="sop/with_group" || is_geometry_arrays node.kind
                      then reads_t v || E.state_dependent v else E.is_live v ||
                        (match v with E.Fn _ -> reads_t v || E.state_dependent v | _ -> false)) node.args)) then begin
        volatile.(node.id) <- true;
        volatile_nodes := Network.Int_map.add compiled.(node.id) () !volatile_nodes
      end) plan.nodes
    let compiled_context = Array.to_list compiled
      |> List.mapi (fun i id -> i, id)
      |> List.filter (fun (i, id) -> id <> 0 && not templ.(i))
      |> List.fold_left (fun m (i, id) -> Network.Int_map.add i id m) Network.Int_map.empty
    let live_network network graph =
      let network = Network.with_states evaluated.states network |> Network.with_profile profile
        |> Network.with_reference reference |> Network.with_approx checked.approx in
      let frame_nodes = Hashtbl.fold (fun _ (p : prepared) nodes -> match p.zone with
        | Some z when z.stateful && Edit.find graph ~node_id:p.cid <> None ->
            Network.Int_map.add p.cid (fun ~network:_ state _live node ->
              let snapshot = E.fork_state state in
              Ok(Sop.Node.Private.adopt_identity ~source:node
                (make_zone ~state:snapshot ~outer:[] z (Sop.Node.Private.input_array node)))) nodes
        | _ -> nodes) prepared Network.Int_map.empty in
      let frame_nodes = Hashtbl.fold (fun _ (p : prepared) nodes ->
        List.fold_left (fun nodes (k : kernel_input) ->
          if not (E.state_dependent (E.Fn k.fn)) || Edit.find graph ~node_id:k.cid = None then nodes
          else Network.Int_map.add k.cid (fun ~network:_ state _live node ->
            Ok (kernel_node ~state:(E.fork_state state) k (Sop.Node.inputs node))) nodes)
          nodes p.kernels) prepared frame_nodes in
      let frame_nodes=Array.fold_left(fun nodes (n:E.node)->
        if not(is_geometry_arrays n.kind) || Edit.find graph ~node_id:compiled.(n.id)=None
          || not(E.state_dependent(geometry_arrays_data n.args))then nodes
        else Network.Int_map.add compiled.(n.id)(fun ~network:_ state _live node->
          Ok(Sop.Node.Private.adopt_identity ~source:node
            (geometry_arrays_node ~state:(E.fork_state state) ~operation:n.kind ~args:n.args ~sources:(deps n)(Sop.Node.inputs node))))nodes)
        frame_nodes plan.nodes in
      let frame_nodes=Array.fold_left(fun nodes (n:E.node)->
        if n.kind<>"sop/with_group" || Edit.find graph ~node_id:compiled.(n.id)=None
          || not(E.state_dependent(group_data n.args)) then nodes
        else Network.Int_map.add compiled.(n.id) (fun ~network:_ state _live node->
          Ok(Sop.Node.Private.adopt_identity ~source:node
            (group_node ~state:(E.fork_state state) ~args:n.args ~sources:(deps n) (Sop.Node.inputs node))))nodes)
        frame_nodes plan.nodes in
      let frame_nodes = Array.fold_left (fun nodes (n : E.node) ->
        let values = List.assoc_opt "values" n.args in
        if n.kind <> "sop/with_attr" || Edit.find graph ~node_id:compiled.(n.id) = None then nodes
        else let values=Option.get values in
          let program=ok(Flow_ir.Executor.compile ~profile ~approx:checked.approx
            ~sink:Flow_ir.Sop_input values) in
          let eligible = Attribute_kernel.sources values=[] &&
            Flow_ir.Executor.root_is_packed_or_readback program in
          if not eligible && not(E.state_dependent(attribute_data n.args)) then nodes else
          Network.Int_map.add compiled.(n.id) (fun ~network:_ state live node ->
          let name = match List.assoc "attribute" n.args with E.Text name -> name | _ -> assert false in
          (* Only input-independent producers can materialize before SOP cooking.
             Attribute-reading cones keep their existing cooked-input CPU path. *)
          Result.bind (if eligible then Flow_ir.Executor.try_display ~reference ~gpu ~state program ~live
            else Ok None) (fun selected ->
          let values=match selected with
            |Some(Flow_ir.Executor.Cpu value)->value
            |None->values
            |Some(Flow_ir.Executor.Gpu _)->assert false in
          if selected=None && not(E.state_dependent(attribute_data n.args)) then Ok node else
          Ok(Sop.Node.Private.adopt_identity ~source:node
            (let owner,kind=attribute_options n.args in
             attribute_node ~state:(E.fork_state state) ?selection:(List.assoc_opt "selection" n.args) ?default:(List.assoc_opt "default" n.args) ~owner ~kind ~name
              ~values ~sources:(deps n) (Sop.Node.inputs node))))) nodes)
        frame_nodes plan.nodes in
      let frame_nodes=Array.fold_left(fun nodes (n:E.node)->
        if (n.kind<>"image/load" && n.kind<>"image/render" && n.kind<>"image/map"
            && not(n.kind="exact" && n.ty=Ty.image)) || Edit.find graph ~node_id:compiled.(n.id)=None
          then nodes else Network.Int_map.add compiled.(n.id)(fun ~network state live node->
            match Domain.DLS.get image_provider with
            |None->Error(Diagnostic.error ~code:"E_IMAGE" "Image resources need an initial-domain resolver.")
            |Some images->Result.map(fun image->
                if Sop.Node.parameters node=string_of_int(Sop.Image.data_id image) then node else
                Sop.Node.Private.adopt_identity ~source:node (resource_image n.kind (Ok image)))
              (images.resolve ~gpu ~context:{compiled=compiled_context;network} plan ~state ~live (E.Deferred(Ty.image,n.id))))nodes)
        frame_nodes plan.nodes in
      let network = Network.with_frame_nodes frame_nodes network in
      let drives = List.fold_left (fun drives (p : pending) ->
        if Edit.find graph ~node_id:p.node = None then drives
        else Port.Map.add Port.{node = p.node; path = p.field}
          p.value drives) Port.Map.empty (List.rev !pending) in
      if Port.Map.is_empty drives then network else ok (Network.with_drives drives network)
    let build index (instance : E.instance) =
      let seen = Hashtbl.create 64 in
      let rec reach id =
        if not (Hashtbl.mem seen id) then begin
          Hashtbl.add seen id ();
          List.iter reach (deps plan.nodes.(id))
        end in
      Array.iter (fun (node : E.node) ->
        if node.inst = index && Ty.is_cooked node.ty && not templ.(node.id) then reach node.id) plan.nodes;
      (match instance.result with E.Deferred (ty, j) when Ty.is_cooked ty -> reach j | _ -> ());
      let order = Hashtbl.fold (fun id () l -> id :: l) seen []
        |> List.sort Int.compare in
      let graph = List.fold_left (fun graph id ->
        let p = prepare plan.nodes.(id) in
        let resources = List.map (fun (k : kernel_input) ->
          let inputs = List.map (fun source ->
            match Edit.find graph ~node_id:compiled.(source) with
            | Some node -> node | None -> fail "E_LOWER" "Function capture was not built before its consumer") k.sources in
          k.index, kernel_node k inputs) p.kernels in
        let graph = List.fold_left (fun graph (_, node) -> edit (Edit.add_node node graph)) graph resources in
        let options = List.init p.arity (fun index ->
          match List.assoc_opt index resources with
          | Some node -> Some node
          | None -> match List.assoc_opt index p.slots with
          | Some source -> Edit.find graph ~node_id:compiled.(source)
          | None -> None) in
        let node = edit (Lisp_node.with_initial_changes ~operation:(Edit.factory_operation p.factory) p.changes (fun () -> Edit.instantiate_optional p.factory options)) in
        let node = edit (Sop.Node.Private.restore_id p.cid node) in
        let inputs = Array.of_list (List.map (Option.map Sop.Node.id)
          options) in
        let graph = edit (Edit.add_node ~factory:p.factory ~inputs node graph) in
        if p.changes = [] then graph
        else fst (edit (Edit.apply_parameters graph ~node_id:p.cid p.changes)))
        Edit.empty order in
      let graph,root = match instance.result with
        | E.Deferred (ty, id) when Ty.is_cooked ty -> Some compiled.(id)
            |>fun root->graph,root
        | No_geo -> graph,None
        | E.Residual residual when (E.Residual.residual_view residual).term.ty=Ty.geometry ->
            let residuals=Hashtbl.create 16 and functions=Hashtbl.create 16 and ids=ref [] in
            let rec sources=function
              |E.Deferred(ty,id) when Ty.is_cooked ty->ids:=id::!ids
              |E.Residual r when not(List.exists((==)r)(Hashtbl.find_all residuals(E.Residual.residual_id r)))->
                  Hashtbl.add residuals(E.Residual.residual_id r)r;
                  List.iter(fun(_,v)->sources v)(E.Residual.residual_view r).bindings
              |E.Fn fn when not(List.exists((==)fn)(Hashtbl.find_all functions(E.Residual.function_id fn)))->
                  Hashtbl.add functions(E.Residual.function_id fn)fn;
                  List.iter(fun(_,v)->sources v)(E.Residual.function_bindings fn)
              |E.Record fields|Struct(_,_,fields)|Source(_,_,fields)->List.iter(fun(_,v)->sources v)fields
              |E.List values->Array.iter sources values
              |_->() in
            sources instance.result;
            let ids=List.sort_uniq Int.compare !ids in
            let captures=List.map(fun id->id,plan.nodes.(id).ty,
              match Edit.find graph ~node_id:compiled.(id) with Some node->node
                |None->fail "E_DATA_SOURCE" "A cooked selection capture was not built.")ids in
            let node=structural_node residual captures in
            edit(Edit.add_node node graph),Some(Sop.Node.id node)
        | _ -> fail "E_LOWER" ("Graph " ^ instance.graph ^ " does not return geometry") in
      let graph = match root with
        | Some id -> edit (Edit.set_root id graph) | None -> graph in
      {name = instance.graph; instance = index; default = instance.default;
       network = live_network (Network.of_geometry graph) graph; root}
    let graphs = List.concat (List.mapi (fun index instance ->
      if sop_instance instance then [build index instance] else [])
      (Array.to_list plan.instances))
    let () = Hashtbl.iter (fun _ (p : prepared) -> List.iter (fun (k : kernel_input) ->
      if E.frame_dependent (E.Fn k.fn) || E.state_dependent (E.Fn k.fn)
          || List.exists (fun id -> volatile.(id)) k.sources then
        volatile_nodes := Network.Int_map.add k.cid () !volatile_nodes) p.kernels) prepared
    (* Previewing an unused template must not allocate provenance or zones after
       this immutable lowering has been handed to the document. *)
    let () = Array.iter (fun (node : E.node) -> if compiled.(node.id) <> 0 && node.kind <> "zone/element"
      && Ty.is_cooked node.ty then ignore (prepare node)) plan.nodes
    let preview ~node:target ~probes network =
      if target < 0 || target >= Array.length compiled || compiled.(target) = 0
        || List.compare_lengths probes plan.nodes.(target).iter <> 0 then None
      else if not templ.(target) then
        if Edit.find network.Network.geometry ~node_id:compiled.(target) = None then None
        else Some (network, compiled.(target))
      else
        let outer = Array.find_opt (fun (node : E.node) -> is_zone node.kind && not templ.(node.id)
          && target >= int_arg node "lo" && target < int_arg node "hi"
          && Edit.find network.geometry ~node_id:compiled.(node.id) <> None) plan.nodes in
        Option.map (fun (node : E.node) ->
          let original = Option.get (prepare node).zone in
          let z = focus original target probes in
          let sources = int_geo node :: z.captures in
          let inputs = Array.of_list (List.map (fun id ->
            match Edit.find network.geometry ~node_id:compiled.(id) with
            | Some node -> node | None -> fail "E_LOWER" "Preview capture is not in the owning network") sources) in
          let viewed = make_zone ~outer:[] z inputs in
          let root = Sop.Node.id viewed in
          let geometry = edit (Edit.add_node viewed network.geometry) in
          let network = ok (Network.with_geometry geometry network) in
          let frames = if not z.stateful then network.frame_nodes else
            Network.Int_map.add root (fun ~network:_ state _live node ->
              Ok(Sop.Node.Private.adopt_identity ~source:node
                (make_zone ~state:(E.fork_state state) ~outer:[] z (Sop.Node.Private.input_array node))))
              network.frame_nodes in
          Network.with_frame_nodes frames network, root) outer
    let result = {gpu; graphs; factories=lisp_factories; compiled_ids = !ids; sites = List.rev !site_list;
        compiled = compiled_context;
        pending = List.rev !pending; provenance = !provenance; zones = List.rev !zones;
        volatile = !volatile_nodes; plan; image_sites; states = evaluated.states; evaluated;
        approx = checked.approx; approx_reasons = checked.approx_reasons; profile; preview}
end

let rec lower_checked ~factories ?(provenance_tags = true) ?(reference = false) ?(gpu=Flow_ir.Gpu.default) ?(strict_body = false) ?(defer_body_errors = false) ?(kernel_bindings = []) ?(compiled_ids = Instance_path.Map.empty)
    ?(sites = []) ?inputs checked =
  Phase_timer.measure "lower" (fun () ->
  try
    let module L = Lowerer (struct
      let factories = factories let provenance_tags = provenance_tags let reference = reference let gpu = gpu
      let strict_body = strict_body let defer_body_errors = defer_body_errors let kernel_bindings = kernel_bindings
      let compiled_ids = compiled_ids let sites = sites let inputs = inputs let checked = checked
      let lower_checked = lower_checked
    end) in
    Ok L.result
  with Diagnostic.Error diagnostic -> Error diagnostic)

let of_checked ~factories ?provenance_tags ?reference ?gpu ?compiled_ids ?sites ?inputs checked =
  Result.bind (Encoded_parameters.checked checked)
    (lower_checked ~factories ?provenance_tags ?reference ?gpu ?compiled_ids ?sites ?inputs)

let workspace ~factories ?extra ?reference ?gpu ?compiled_ids ?sites ?inputs source =
  Result.bind (Catalog.of_factories ~version:Manifest.version ?extra factories) (fun catalog ->
    match Catalog.check_workspace ~ops:Operators.all catalog source with
    | Some checked, _ -> of_checked ~factories ?reference ?gpu ?compiled_ids ?sites ?inputs checked
    | None, diagnostics -> Error (match List.find_opt (fun (d : Diagnostic.t) ->
        d.severity = Diagnostic.Error_severity) diagnostics with
      | Some d -> d
      | None -> Diagnostic.error ~code:"E_LOWER" "Workspace did not check"))

let source_context (lowered:t) ~node =
  if node < 0 || node >= Array.length lowered.plan.nodes
    || not(Ty.is_geometry lowered.plan.nodes.(node).ty) then
    Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture needs a geometry plan node.")
  else match Network.Int_map.find_opt node lowered.compiled with
    |None->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source was not compiled.")
    |Some id->
        match List.find_opt(fun (graph:graph)->graph.instance=lowered.plan.nodes.(node).inst
          && Option.is_some(Edit.find graph.network.geometry ~node_id:id))lowered.graphs with
        |None->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source has no owning network.")
        |Some graph->Ok{compiled=lowered.compiled;network=graph.network}

let source_cone (context:image_context) ~node =
  match Network.Int_map.find_opt node context.compiled with
  |None->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source was not compiled.")
  |Some root when Option.is_none(Edit.find context.network.geometry ~node_id:root)->
      Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source is outside its owning network.")
  |Some root->
      let seen=Hashtbl.create 16 in
      let rec visit id=if not(Hashtbl.mem seen id)then begin
        Hashtbl.add seen id();
        Option.iter(Array.iter(Option.iter visit))(Edit.inputs context.network.geometry ~node_id:id)
      end in
      visit root;
      let removed=Edit.inspect context.network.geometry |> List.filter_map(fun (n:Edit.node_info)->
        if Hashtbl.mem seen n.id then None else Some n.id)in
      Result.map(fun network->network,root)(Network.remove_nodes removed context.network)

let counts (lowered:t) =
  let live = Network.Int_map.fold (fun _ id n ->
    if Network.Int_map.mem id lowered.volatile then n + 1 else n) lowered.compiled 0 in
  live, Network.Int_map.cardinal lowered.compiled - live

let status lowered ~seconds =
  match counts lowered with
  | 0, _ -> None
  | live, cached -> Some (Printf.sprintf "t %d live · %d cached · cook %.1f ms" live cached (seconds *. 1000.))

let origin lowered tag =
  match Network.Int_map.find_opt tag lowered.provenance with
  | Some _ as found -> found
  | None ->
      List.find_map (fun (z : zone) ->
        let index = tag - z.base in
        if index >= 0 && index < Zone.max_elements then
          Some {merge = z.cid; input = index; source = z.cid; site = z.body_site;
                iter = z.iter @ [ index ]}
        else None) lowered.zones

let tags lowered ~site ~iter =
  let plain = Network.Int_map.fold (fun tag (o : origin) tags ->
    if o.site = site && o.iter = iter then tag :: tags else tags) lowered.provenance [] in
  let n = List.length iter in
  List.fold_left (fun tags (z : zone) ->
    if z.site = site && z.iter = iter then
      List.init (max 0 (Atomic.get z.count)) (fun i -> z.base + i) @ tags
    else if z.body_site = site && List.compare_length_with z.iter (n - 1) = 0
       && List.for_all2 ( = ) z.iter (List.take (max 0 (n - 1)) iter)
    then z.base + List.nth iter (n - 1) :: tags else tags) plain lowered.zones

let zone_count lowered site =
  List.find_map (fun (z : zone) ->
    if z.site = site && Atomic.get z.count >= 0 then Some (Atomic.get z.count) else None)
    lowered.zones

let zone_element lowered site k =
  List.find_map (fun (z : zone) ->
    if z.site = site then
      (match z.ekey with
       | Some key ->
           let positions = Atomic.get z.positions in
           if k >= 0 && k < Array.length positions then Some (key, positions.(k)) else None
       | None -> None)
    else None) lowered.zones

let field_calls ?state ~live ~resolve lowered =
  let state = Option.map E.fork_state state in
  let defaults = List.find_map (fun factory ->
    if Edit.factory_key factory = "iso_surface" then Some (Edit.factory_fields factory)
    else None) lowered.factories |> Option.value ~default:[] in
  let vector (node : E.node) name =
    match List.assoc_opt name node.args with
    | Some value -> (match E.Private.force_reference ?state ~resolve value ~live with
        | Ok (E.Vec3 (x,y,z)) -> Some (x,y,z) | _ -> None)
    | None ->
        let component suffix = List.find_map (fun (f : Param.field_view) ->
          if f.name = name ^ suffix then match f.current with
            | Float_value value -> Some value | _ -> None else None) defaults in
        Option.bind (component "_x") (fun x -> Option.bind (component "_y")
          (fun y -> Option.map (fun z -> x,y,z) (component "_z"))) in
  fun zone outer ->
    let offset = ref 0 in
    Array.to_list lowered.plan.nodes |> List.filter_map (fun (node : E.node) ->
      if node.kind <> "sop/iso_surface" then None else
      match List.assoc_opt "field" node.args with
      | Some (E.Fn fn) when E.Private.function_scope fn = Some (zone, outer) ->
          (match vector node "resolution", vector node "min", vector node "max" with
           | Some (rx,ry,rz), Some (lx,ly,lz), Some (hx,hy,hz) ->
               let valid n = Float.is_finite n && n >= 1. && Float.floor n = n
                   && n < float (Sys.max_array_length / 3) in
               if not (valid rx && valid ry && valid rz) then None else
               let nx = int_of_float rx + 1 and ny = int_of_float ry + 1 and nz = int_of_float rz + 1 in
               let limit = Sys.max_array_length / 3 in
               if nx > limit / ny || nx * ny > limit / nz then None else
               let count = nx * ny * nz and start = !offset in
               if count > max_int - start then None else begin
                 offset := start + count;
                 let at k =
                   if k < 0 || k >= count then Error (Diagnostic.error ~code:"E_LIST_RANGE"
                     "Probe index is outside the field grid.") else
                   let p = [|Float.fma (float (k mod nx)) ((hx -. lx) /. rx) lx;
                     Float.fma (float ((k / nx) mod ny)) ((hy -. ly) /. ry) ly;
                     Float.fma (float (k / (nx * ny))) ((hz -. lz) /. rz) lz|] in
                   Result.bind (E.Private.map_function ~signature:Flow.Ty.{params = [Vec3]; result = Float}
                     fn [E.Vec3_array p]) (function
                     | E.Residual residual -> Result.bind (E.Private.map_probe ?state ~resolve
                         ~offset:(start + k) residual ~live) (fun (_, at) -> at 0)
                     | _ -> assert false) in
                 Some (start, count, at)
               end
           | _ -> None)
      | _ -> None)
