module Flow_catalog = Catalog
open Flow
module Edit = Sop.Edit_graph
module Zone = Sop.Zone
module E = Eval

module Path_map = Map.Make (struct
  type t = int list

  let compare = Stdlib.compare
end)

let source_attribute = Rdk.Mesh_merge.provenance_attribute

(* Serial for host-call nodes: they are cooked once in a session that keeps no cache entries, so their
   arguments need no content key (marshalling a million-element array costs more than the kernel). *)
let host_serial = Atomic.make 0

type pending = { node : int; field : string; value : E.value }
type origin = { merge : int; input : int; source : int; site : Workspace.path; iter : int list }

type zone = {
  cid : int;
  site : Workspace.path;
  iter : int list;
  body_site : Workspace.path;
  base : int;
  count : int Atomic.t;
  ekey : string option;
  positions : (float * float * float) array Atomic.t;
}

type graph = {
  name : string;
  instance : int;
  default : bool;
  network : Network.t;
  root : int option;
}

type t = {
  gpu : Flow_ir.Gpu.config;
  graphs : graph list;
  factories : Edit.factory list;
  compiled_ids : int Path_map.t;
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
  card_node : node:int -> (Sop.Node.t, string) result;
}

let fail code message = raise (Diagnostic.Error (Diagnostic.error ~code message))
let ok = function Ok value -> value | Error diagnostic -> raise (Diagnostic.Error diagnostic)
let edit = function Ok value -> value | Error message -> fail "E_LOWER" message

type image_context = { compiled : int Network.Int_map.t; network : Network.t }

type image_resolver =
  ?gpu:Flow_ir.Gpu.config ->
  ?context:image_context ->
  E.plan ->
  state:E.state ->
  live:Input.t ->
  E.value ->
  (Sop.Image.t, Diagnostic.t) result

type images = { resolve : image_resolver; metadata : E.plan -> int -> (int * int) option }

let image_provider = Domain.DLS.new_key (fun () -> None)

let with_images ?gpu ?(metadata = fun _ _ -> None) resolver run =
  let previous = Domain.DLS.get image_provider in
  Domain.DLS.set image_provider
    (Some
       {
         resolve =
           (fun ?gpu:requested ?context plan ~state ~live value ->
             let gpu = match requested with Some _ -> requested | None -> gpu in
             resolver ?gpu ?context plan ~state ~live value);
         metadata;
       });
  Fun.protect ~finally:(fun () -> Domain.DLS.set image_provider previous) run

let image_metadata plan =
  match Domain.DLS.get image_provider with
  | None -> fun _ -> None
  | Some images -> images.metadata plan

let resource_image operation result =
  Sop.Node.Build.make ~operation ~version:1
    ~parameters:
      (match result with
      | Ok image -> string_of_int (Sop.Image.data_id image)
      | Error _ -> "unbound")
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _ ->
      match result with
      | Ok image -> Ok Sop.Node.Build.{ payload = Sop.Payload.Image image; instances = None }
      | Error diagnostic -> Error diagnostic)

type kernel_input = {
  index : int;
  cid : int;
  identity : int;
  signature : Ty.fn_signature;
  fn : E.fn;
  sources : int list;
}

type prepared = {
  cid : int;
  factory : Edit.factory;
  slots : (int * int) list; (* slot index, source plan id *)
  arity : int;
  changes : (string * Param.value) list;
  dynamic : (E.value * (E.value -> (string * Param.value) list)) list;
      (* a template node: arguments that read the element, forced for each one *)
  zone : zinfo option;
  kernels : kernel_input list;
}

and zinfo = {
  plan_id : int;
  lo : int;
  hi : int;
  preview : (int * int list) option;
  root : int;
  order : int list;
  live : bool;
  stateful : bool;
  captures : int list;
  ekey : string option;
  kind : Zone.kind;
  key : string option;
  base : int;
  count : int Atomic.t;
  positions : (float * float * float) array Atomic.t;
}

let hex text =
  match Port.color_of_text text with
  | Some (r, g, b) -> E.Vec3 (r, g, b)
  | None -> fail "E_LOWER" ("Bad colour " ^ text)

let typed = function
  | E.Int n -> Port_type.Int_value n
  | Float f -> Float_value f
  | Bool b -> Bool_value b
  | Vec3 (x, y, z) -> Vec3_value (x, y, z)
  | _ -> fail "E_LOWER" "Argument is not a number, bool or vec3"

let changes_of (parameter : Port.parameter) value =
  match (parameter.ty, value) with
  | None, E.Text text ->
      let literal =
        match parameter.fields with
        | [ { Param.kind = Param.Choice_view _; _ } ] -> Param.Choice_value text
        | _ -> Param.Text_value text
      in
      ok (Port.literal_changes parameter (Port.Scalar literal))
  | None, _ -> fail "E_LOWER" ("Port " ^ parameter.path ^ " takes text")
  | Some _, E.Text text -> snd (ok (Port.normalize parameter (typed (hex text))))
  | Some _, value ->
      let finite =
        match value with
        | E.Float f -> Float.is_finite f
        | Vec3 (x, y, z) -> Float.is_finite x && Float.is_finite y && Float.is_finite z
        | _ -> true
      in
      if not finite then fail "E_LOWER" ("Port " ^ parameter.path ^ " takes a finite number");
      snd (ok (Port.normalize parameter (typed value)))

let changes parameter value =
  try Ok (changes_of parameter value) with Diagnostic.Error d -> Error d

(* sop/material takes a material value as its fields *)
let material_args kind args =
  if kind <> "sop/material" then args
  else
    List.concat_map
      (function
        | "material", E.Struct ("material/standard", _, fields) ->
            let get name default = Option.value ~default (List.assoc_opt name fields) in
            [
              ("material", get "name" (E.Text ""));
              ("color", get "color" (E.Vec3 (1., 1., 1.)));
              ("roughness", get "roughness" (E.Float 0.4));
              ("emission", get "emission" (E.Vec3 (0., 0., 0.)));
            ]
        | arg -> [ arg ])
      args

(* A library definition's cone digest is kept by kind and first form id: library forms have fixed
   ids (Catalog) and the library is part of the binary. An authored one is digested afresh. *)
let library_keys = Hashtbl.create 64
let library_lock = Mutex.create ()

let declaration_key checked (d : Workspace.node_definition) =
  let compute () = Digest.string (Context_kernel.declaration_source checked d) in
  let id = d.body.body.form.id in
  if id < Flow_catalog.library_base then compute ()
  else
    match
      Mutex.protect library_lock (fun () -> Hashtbl.find_opt library_keys (d.kind.qualified, id))
    with
    | Some key -> key
    | None ->
        let key = compute () in
        Mutex.protect library_lock (fun () ->
            Hashtbl.replace library_keys (d.kind.qualified, id) key);
        key

(* An inlined Lisp call's parameters take the node path: the call's changes over the schema
   defaults (ranges, choices, encoded text through the declared normalizers), then the node's
   arguments. A refusal is the lowering error the node's construction raised. The body's context
   operators carry the card's cook label (its kind key): the seed, frame and time are cook-time
   sources its kernels resolve from their cook context; the provenance base is allocated where the
   body's sop/concat lowers. *)
let () =
  Atomic.set E.inline_parameters (fun (checked : Workspace.t) (d : Workspace.node_definition) ->
      let normalizers = lazy (Encoded_parameters.for_node checked d) in
      let schema = lazy (Lisp_node.schema "parameters" d.fields (Lazy.force normalizers))
      and parameters = lazy (ok (Port.parameters d.fields)) in
      let key = String.sub d.kind.qualified 4 (String.length d.kind.qualified - 4) in
      let body, context_inputs = Context_kernel.label_context ~label:key d.body in
      ( { d with body },
        lazy (declaration_key checked d),
        fun vals ->
          let vals = material_args d.kind.qualified vals in
          let live, vals = List.partition (fun (_, value) -> E.is_live value) vals in
          let changes =
            List.concat_map
              (fun (name, value) ->
                match Port.find_parameter (Lazy.force parameters) name with
                | Ok parameter -> changes_of parameter value
                | Error _ -> [])
              vals
          in
          (* a live argument (per frame, or cooked data) is passed as it is, a program input: its
           field must have nothing to canonicalize *)
          let raw (name, _) =
            match List.find_opt (fun (f : Param.field_view) -> f.name = name) d.fields with
            | None ->
                not
                  (List.exists
                     (fun (f : Param.field_view) ->
                       f.vec3 <> None && Option.map fst f.vec3 = Some name)
                     d.fields)
            | Some f -> (
                f.vec3 = None
                && (not (List.mem_assoc name (Lazy.force normalizers)))
                &&
                match f.kind with
                | Param.Toggle_view -> true
                | Integer_view { hard_min = None; hard_max = None; _ }
                | Floating_view { hard_min = None; hard_max = None; _ } ->
                    true
                | _ -> false)
          in
          if not (List.for_all raw live) then None
          else
            let live_names = List.map fst live in
            let override arguments =
              List.filter (fun (name, _) -> not (List.mem name live_names)) arguments @ live
            in
            let schema = Lazy.force schema in
            match
              Result.bind
                (Param.apply_all schema (Param.default schema) changes)
                (fun (values, _) -> Lisp_node.arguments d.fields values)
            with
            | Error message -> fail "E_LOWER" message
            | Ok arguments ->
                (* a context input the call does not reach is bound to a placeholder, so no kernel
               captures its source *)
                let reached =
                  if context_inputs = [] then []
                  else
                    Context_kernel.context_ops_reached ~inputs:arguments ~defs:checked.defs d.body
                in
                Some
                  (override arguments
                  @ List.filter_map
                      (fun (name, op) -> if List.mem op reached then None else Some (name, E.Int 0))
                      context_inputs) ))

let at_zero value = if E.is_live value then ok (E.force value ~live:(Input.at_time 0.)) else value
let is_zone kind = Operators.lowering kind = Some Operators.Zone

let metadata_factory ~operation ~args sources =
  let text name default =
    match List.assoc_opt name args with
    | None -> default
    | Some (E.Text text) -> text
    | _ -> fail "E_METADATA_TEXT" "Metadata names and patterns must be static text"
  in
  let bool name =
    match List.assoc_opt name args with
    | None -> false
    | Some (E.Bool value) -> value
    | _ -> fail "E_METADATA_BOOL" "Metadata flags must be static booleans"
  in
  let build = function
    | input :: rest -> (
        match operation with
        | "sop/rename_attr" ->
            Attribute_metadata.rename ~owner:(text "owner" "any") ~pattern:(text "pattern" "")
              ~replacement:(text "replacement" "") ~conflict:(text "conflict" "error") input
        | "sop/swap_attr" ->
            Attribute_metadata.swap ~owner:(text "owner" "point") ~source:(text "source" "")
              ~destination:(text "destination" "") ~method_:(text "method" "swap") input
        | "sop/delete_attrs" ->
            Attribute_metadata.delete_attributes ?reference:(List.nth_opt rest 0)
              ~inverse:(bool "inverse") ~point:(text "point" "") ~vertex:(text "vertex" "")
              ~primitive:(text "primitive" "") ~detail:(text "detail" "") input
        | "sop/rename_group" ->
            Attribute_metadata.rename_group ~owner:(text "owner" "any") ~pattern:(text "pattern" "")
              ~replacement:(text "replacement" "") ~conflict:(text "conflict" "error") input
        | "sop/delete_group" ->
            Attribute_metadata.delete_group ~owner:(text "owner" "any") ~pattern:(text "pattern" "")
              ~unused:(bool "unused") input
        | "sop/rename_edge" ->
            Attribute_metadata.rename_edge ~from:(text "pattern" "") ~into:(text "replacement" "")
              input
        | "sop/delete_edge" -> Attribute_metadata.delete_edge ~name:(text "name" "") input
        | _ -> fail "E_LOWER" ("No metadata lowering for " ^ operation))
    | [] -> invalid_arg "Metadata operations require geometry"
  in
  Edit.factory ~key:operation ~label:operation ~category:[ "Flow"; "Metadata" ]
    ~arity:(List.length sources) (fun inputs -> Ok (build inputs))

let is_geometry_arrays kind = Operators.lowering kind = Some Operators.Geometry_arrays

let geometry_arrays_key = function
  | "sop/remap" -> "flow.remap"
  | "sop/concat" -> "flow.concat"
  | "sop/with_planes" -> "flow.with_planes"
  | _ -> "flow.mesh"

let geometry_arrays_label = function
  | "sop/remap" -> "Remap"
  | "sop/concat" -> "Concat"
  | "sop/with_planes" -> "With planes"
  | _ -> "Mesh"

let geometry_arrays_data args =
  E.Record
    (List.filter
       (fun (name, _) -> not (List.mem name [ "source"; "target"; "sources"; "geometry" ]))
       args)

(* A kernel over data that reads its cook context (an inlined body's seed, frame or time) is built
   per context from the specialized data, as the nested card rebuilt its body: the last context's
   build is kept. The node at the default context gives the identity, facts and strict refusals. *)
let per_context value (build : (E.value -> E.value) -> Sop.Node.t) =
  let reads = Attribute_kernel.context_reads value in
  if reads = Sop.Context.Dependencies.static then build Fun.id
  else
    let make context = build (Attribute_kernel.specialize context) in
    let base = make (Result.get_ok (Sop.Context.create ())) in
    let key context =
      let has fact = Sop.Context.Dependencies.mem fact reads in
      ( (if has Seed then Sop.Context.seed context else 0L),
        (if has Frame then Sop.Context.frame context else 0L),
        if has Time then Int64.bits_of_float (Sop.Context.time context) else 0L )
    in
    let last = Atomic.make None in
    Sop.Node.Build.make ~label:(Sop.Node.label base) ~operation:(Sop.Node.operation base)
      ~version:(Sop.Node.version base) ~parameters:(Sop.Node.parameters base)
      ~cook_mode:(Sop.Node.facts base).cook_mode
      ~dependencies:(Sop.Context.Dependencies.union (Sop.Node.dependencies base) reads)
      ~input_policy:(Sop.Node.Build.input_policy base)
      ~inputs:(Sop.Node.Build.input_array base)
      (fun ~node_id:_ context payloads ->
        let node =
          match Atomic.get last with
          | Some (k, node) when k = key context -> node
          | _ ->
              let node = make context in
              Atomic.set last (Some (key context, node));
              node
        in
        Sop.Node.Build.cook node context payloads)
    |> Sop.Node.Build.with_facts (Sop.Node.facts base)

let rec geometry_arrays_node ?check ~profile ~strict_body ~reference ~approx ~kernel_source ?host
    ?state ?elems ~operation ~args ~sources inputs =
  per_context (E.Record args) (fun specialize ->
      let args =
        match specialize (E.Record args) with E.Record args -> args | _ -> assert false
      in
      geometry_arrays_node_in ?check ~profile ~strict_body ~reference ~approx ~kernel_source ?host
        ?state ?elems ~operation ~args ~sources inputs)

and geometry_arrays_node_in ?(check = true) ~profile ~strict_body ~reference ~approx ~kernel_source
    ?(host = false) ?state ?elems ~operation ~args ~sources inputs =
  let data = geometry_arrays_data args in
  let program =
    Attribute_kernel.prepare ~profile ~strict:strict_body ~approx ~sink:Flow_ir.Sop_input ~sources
      inputs data
  in
  if strict_body && check then ignore (ok program);
  let parameters =
    Lazy.force kernel_source
    ^ (match operation with
      | "sop/remap" -> ";remap;"
      | "sop/concat" -> ";concat;"
      | "sop/with_planes" -> ";planes;"
      | _ -> ";mesh;")
    ^ (if host then "host" ^ string_of_int (Atomic.fetch_and_add host_serial 1)
       else Flow.Value.key_of ~residual:E.Residual.residual_id data)
    ^ Option.fold ~none:"" ~some:E.state_stamp state
    ^ Option.fold ~none:""
        ~some:(fun fields -> Flow.Value.key_of ~residual:E.Residual.residual_id (E.Record fields))
        elems
  in
  let node =
    Sop.Node.Build.make_geometry ~label:(geometry_arrays_label operation)
      ~operation:(geometry_arrays_key operation) ~version:1 ~parameters
      ~cook_mode:
        (if List.mem operation [ "sop/remap"; "sop/concat"; "sop/with_planes" ] then
           Sop.Node.Generic
         else Sop.Node.Generator)
      ~dependencies:(Attribute_kernel.dependencies data)
      ~inputs:(Array.of_list inputs)
      (fun ~node_id:_ context geometries ->
        let cancelled () = Sop.Context.cancelled context in
        let error () = Error (Diagnostic.error ~code:"E_CANCELLED" "Mesh kernel cancelled.") in
        let geometry id =
          Option.map (fun index -> geometries.(index)) (List.find_index (( = ) id) sources)
        in
        let result =
          if cancelled () then error ()
          else
            Result.bind program (fun program ->
                Result.bind
                  (Flow_ir.Executor.force ?state ?elems ~reference ~strict:strict_body
                     ~resolve:(Attribute_kernel.resolve ~context ~geometry)
                     program ~live:(Sop.Context.input context))
                  (function
                    | E.Record fields ->
                        if cancelled () then error ()
                        else
                          let result =
                            if operation = "sop/with_planes" then
                              match List.assoc_opt "geometry" args with
                              | Some (E.Deferred (ty, id))
                                when ty = Ty.geometry && geometry id <> None ->
                                  Attribute_kernel.with_planes ~grain:(Sop.Context.grain context)
                                    (Option.get (geometry id))
                                    fields
                              | _ ->
                                  Error
                                    (Diagnostic.error ~code:"E_DATA_SOURCE"
                                       "With planes needs a cooked geometry.")
                            else if operation = "sop/concat" then
                              let rec collect result = function
                                | [] -> Ok (List.rev result)
                                | E.No_geo :: rest -> collect result rest
                                | E.Deferred (ty, id) :: rest when ty = Ty.geometry -> (
                                    match geometry id with
                                    | Some value -> collect (value :: result) rest
                                    | None ->
                                        Error
                                          (Diagnostic.error ~code:"E_DATA_SOURCE"
                                             "Concat needs cooked source geometries."))
                                | _ ->
                                    Error
                                      (Diagnostic.error ~code:"E_CONCAT_TYPE"
                                         "Concat sources must be a geometry list.")
                              in
                              Result.bind
                                (match List.assoc_opt "sources" args with
                                | Some (E.List values) -> collect [] (Array.to_list values)
                                | _ ->
                                    Error
                                      (Diagnostic.error ~code:"E_CONCAT_TYPE"
                                         "Concat sources must be a geometry list."))
                                (fun sources ->
                                  Topology_kernel_impl.concat
                                    ~cancel:(Sop.Context.cancel_token context)
                                    ~grain:(Sop.Context.grain context) ~sources fields)
                            else if operation <> "sop/remap" then Topology_kernel.mesh fields
                            else
                              let cooked name =
                                match List.assoc_opt name args with
                                | Some (E.Deferred (ty, id)) when ty = Ty.geometry -> (
                                    match geometry id with
                                    | Some geometry -> Ok geometry
                                    | None ->
                                        Error
                                          (Diagnostic.error ~code:"E_DATA_SOURCE"
                                             "Remap needs two cooked geometries."))
                                | _ ->
                                    Error
                                      (Diagnostic.error ~code:"E_DATA_SOURCE"
                                         "Remap needs two cooked geometries.")
                              in
                              Result.bind (cooked "source") (fun source ->
                                  Result.bind (cooked "target") (fun target ->
                                      Topology_kernel_impl.remap
                                        ~cancel:(Sop.Context.cancel_token context)
                                        ~grain:(Sop.Context.grain context) ~source ~target fields))
                          in
                          Result.bind result (fun geometry ->
                              if cancelled () then error () else Ok geometry)
                    | _ ->
                        Error
                          (Diagnostic.error ~code:"E_TOPOLOGY_TYPE" "Mesh data must be a record.")))
        in
        Result.map (fun geometry -> Sop.Node.Build.{ geometry; instances = None }) result)
  in
  Sop.Node.Build.with_facts
    {
      (Sop.Node.facts node) with
      reads =
        (if List.mem operation [ "sop/remap"; "sop/concat"; "sop/with_planes" ] then [ "*" ]
         else Attribute_kernel.reads data);
      writes = [ "*" ];
      topology = Changed;
      elementwise = None;
    }
    node

let attribute_options args =
  let text key default code =
    match List.assoc_opt key args with
    | None -> default
    | Some (E.Text text) -> text
    | _ -> fail code ("Attribute " ^ key ^ " must be static text")
  in
  let owner = ok (Attribute_kernel.owner (text "owner" "point" "E_ATTR_OWNER"))
  and kind = text "type" "vec3" "E_ATTR_TYPE" in
  ignore (ok (Attribute_kernel.value_type kind));
  (owner, kind)

let group_data args =
  E.Record (List.filter (fun (name, _) -> name = "values" || name = "order") args)

(* the arguments [Lower.fold_writes] adds to a write it fuses *)
let fused_deletes = "$deletes"
and fused_writes = "$writes"
and fused_chain = "$chain"

let attribute_data args =
  let data = [ "selection"; "default"; "when"; fused_writes ] in
  if not (List.exists (fun k -> List.mem_assoc k args) data) then List.assoc "values" args
  else E.Record (List.filter (fun (name, _) -> name = "values" || List.mem name data) args)

let kernel_data (n : E.node) =
  match Operators.lowering n.kind with
  | Some Attribute_write -> Some (attribute_data n.args)
  | Some Group_write -> Some (group_data n.args)
  | Some Geometry_arrays -> Some (geometry_arrays_data n.args)
  | _ -> None

let is_resource (n : E.node) =
  Operators.lowering n.kind = Some Operators.Resource && (n.kind <> "exact" || n.ty = Ty.image)

let attribute_node ?(check = true) ~strict_body ~reference ~profile ~approx ~gpu ~kernel_source
    ?state ?elems ?selection ?default ?guard ?(prior = []) ?(owner = Rdk.Attribute.Point)
    ?(kind = "vec3") ?deletes ~name ~values ~sources inputs =
  let data =
    E.Record
      ([ ("values", values) ]
      @ List.mapi (fun i (_, _, v) -> ("prior" ^ string_of_int i, v)) prior
      @ Option.to_list (Option.map (fun v -> ("selection", v)) selection)
      @ Option.to_list (Option.map (fun v -> ("default", v)) default)
      @ Option.to_list (Option.map (fun v -> ("when", v)) guard))
  in
  per_context data @@ fun specialize ->
  let values = specialize values
  and selection = Option.map specialize selection
  and default = Option.map specialize default
  and guard = Option.map specialize guard
  and prior = List.map (fun (name, kind, v) -> (name, kind, specialize v)) prior in
  if strict_body && check then
    ignore (ok (Attribute_kernel.prepare ~strict:true ~sources inputs values));
  Attribute_kernel.node ?state ?elems ?selection ?default ?guard ~prior ~gpu ~strict:strict_body
    ~reference ~profile ~approx ~owner ~kind ?deletes ~source:(Lazy.force kernel_source) ~name
    ~values ~sources inputs

let group_node ?(check = true) ~strict_body ~reference ~profile ~approx ~kernel_source ?state ?elems
    ~args ~sources inputs =
  per_context (E.Record args) @@ fun specialize ->
  let args = match specialize (E.Record args) with E.Record args -> args | _ -> assert false in
  let text key default =
    match List.assoc_opt key args with
    | None -> default
    | Some (E.Text text) -> text
    | _ -> fail "E_GROUP_TYPE" ("Group " ^ key ^ " must be static text")
  in
  let owner_name = text "owner" "point" and name = text "name" "" in
  let owner = ok (Group_kernel.owner owner_name) and values = group_data args in
  let program = Attribute_kernel.prepare ~profile ~strict:strict_body ~sources inputs values in
  if strict_body && check then ignore (ok program);
  let node =
    Sop.Node.Build.make_geometry ~label:("Write group " ^ name) ~operation:"flow.with_group"
      ~version:1
      ~parameters:
        (Lazy.force kernel_source ^ owner_name ^ ":" ^ name
        ^ Flow.Value.key_of ~residual:E.Residual.residual_id values
        ^ Option.fold ~none:"" ~some:E.state_stamp state)
      ~cook_mode:Sop.Node.Generic
      ~dependencies:(Attribute_kernel.dependencies values)
      ~inputs:(Array.of_list inputs)
      (fun ~node_id:_ context geometries ->
        let result =
          if geometries = [||] || List.assoc "geometry" args = E.No_geo then
            Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Group write needs geometry.")
          else if Sop.Context.cancelled context then
            Error (Diagnostic.error ~code:"E_CANCELLED" "Group kernel cancelled.")
          else
            Result.bind program (fun program ->
                let geometry id =
                  Option.map (fun index -> geometries.(index)) (List.find_index (( = ) id) sources)
                in
                Result.bind
                  (Flow_ir.Executor.force ?state ?elems ~reference ~strict:strict_body
                     ~resolve:(Attribute_kernel.resolve ~context ~geometry)
                     program ~live:(Sop.Context.input context))
                  (function
                    | E.Record fields ->
                        let order =
                          match List.assoc_opt "order" fields with
                          | None -> Ok None
                          | Some (E.Int_array order) -> Ok (Some order)
                          | Some (E.Record fields) -> (
                              match
                                (List.assoc_opt "ordered" fields, List.assoc_opt "indices" fields)
                              with
                              | Some (E.Bool ordered), Some (E.Int_array indices) ->
                                  Ok (if ordered then Some indices else None)
                              | _ ->
                                  Error
                                    (Diagnostic.error ~code:"E_GROUP_ORDER"
                                       "Group order requires an integer array or an \
                                        ordered/indices record."))
                          | _ ->
                              Error
                                (Diagnostic.error ~code:"E_GROUP_ORDER"
                                   "Group order requires an integer array or an ordered/indices \
                                    record.")
                        in
                        Result.bind order (fun order ->
                            match List.assoc_opt "values" fields with
                            | Some (E.Int_array mask) ->
                                Group_kernel.write ?order ~owner ~name mask geometries.(0)
                            | _ ->
                                Error
                                  (Diagnostic.error ~code:"E_GROUP_MASK"
                                     "Group mask requires an integer array."))
                    | _ -> assert false))
        in
        Result.map (fun geometry -> Sop.Node.Build.{ geometry; instances = None }) result)
  in
  Sop.Node.Build.with_facts
    { (Sop.Node.facts node) with topology = Preserved; reads = [ "*" ]; writes = [ "*" ] }
    node

(* An evaluated body error belongs to cooking: the node keeps its inputs and declared facts. *)
let lisp_body_failed ~(checked : Workspace.t) ~(definition : Workspace.node_definition) ~inputs
    ~parameters diagnostic =
  let dependencies =
    List.fold_left
      (fun dependencies (op, fact) ->
        if Context_kernel.depends ~op ~inputs:parameters ~defs:checked.defs definition.body then
          Sop.Context.Dependencies.union dependencies (Sop.Context.Dependencies.one fact)
        else dependencies)
      Sop.Context.Dependencies.static
      [
        ("sop/context_seed", Sop.Context.Dependencies.Seed);
        ("sop/context_frame", Frame);
        ("sop/context_time", Time);
      ]
  in
  let cook_mode = if inputs = [] then Sop.Node.Generator else Generic in
  let exact =
    Option.fold ~none:true
      ~some:(fun (facts : Flow.Catalog.kernel_facts) -> facts.exact)
      definition.kind.facts
  in
  let facts =
    Sop.Node.
      {
        cook_mode;
        elementwise = None;
        reads = [ "*" ];
        writes = [ "*" ];
        topology = Changed;
        exact;
      }
  in
  ((fun _ -> Error diagnostic), dependencies, facts)

let build_lisp_body ~gpu ~lower_nested ~record_profile ~checked
    ~(definition : Workspace.node_definition) ~provenance ~label ~context ~inputs ~parameters
    ~bindings ~functions ~factories ~def_context_use =
  Phase_timer.without (fun () ->
      let rec function_elements = function
        | E.Fn fn -> [ fn ]
        | E.List values -> Array.to_list values |> List.concat_map function_elements
        | _ -> []
      in
      let make_capture_factories aliases =
        List.mapi
          (fun index input ->
            let key = "lisp_capture_" ^ string_of_int index in
            let factory =
              Edit.factory ~key ~label:key ~category:[ "Flow" ] ~arity:0 (fun _ ->
                  Result.ok
                  @@ Sop.Node.Build.make ~operation:key ~version:1
                       ~parameters:(string_of_int (Sop.Node.id input))
                       ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static
                       ~inputs:[||]
                       (fun ~node_id:_ _ _ ->
                         Result.map
                           (fun payload ->
                             let payload =
                               match (payload, List.assoc_opt index aliases) with
                               | Sop.Payload.Kernel kernel, Some sources ->
                                   Sop.Payload.Kernel
                                     (Sop.Kernel.remap_geometry_captures ~sources kernel)
                               | _ -> payload
                             in
                             Sop.Node.Build.{ payload; instances = None })
                           (Lisp_node.input index)))
            in
            (key, factory))
          inputs
      in
      let capture_factories = make_capture_factories [] in
      let body = Context_kernel.specialize ?provenance ~label ~context definition.body in
      let body =
        {
          body with
          inputs =
            List.map
              (fun (name, ty, default) ->
                let default =
                  match List.assoc_opt name bindings with
                  | None -> default
                  | Some indices ->
                      let element_ty = match ty with Ty.List element -> element | _ -> ty in
                      let capture index =
                        let key, _ = List.nth capture_factories index in
                        Workspace.
                          {
                            path = None;
                            ty = element_ty;
                            form = Syntax.make (List []);
                            node =
                              Call
                                {
                                  kind = "sop/" ^ key;
                                  ctx =
                                    (if element_ty = Ty.image then Context.image else Context.sop);
                                  args = [];
                                };
                          }
                      in
                      let node =
                        match (ty, indices) with
                        | Ty.List _, _ -> Workspace.List_lit (List.map capture indices)
                        | _, [ index ] -> (capture index).node
                        | _, [] -> Workspace.Nil
                        | _ -> fail "E_LOWER" "A fixed Lisp slot has several inputs"
                      in
                      Some Workspace.{ path = None; ty; form = Syntax.make (List []); node }
                in
                (name, ty, default))
              body.inputs;
        }
      in
      let forwarded =
        List.concat_map
          (fun (name, value) ->
            let fns = function_elements value
            and indices = Option.value ~default:[] (List.assoc_opt name bindings) in
            if List.compare_lengths fns indices <> 0 then
              fail "E_LOWER" "Callable slots do not match their cooked inputs";
            List.mapi
              (fun offset (fn, index) ->
                (Printf.sprintf "lisp_kernel_%s_%d" name offset, fn, index))
              (List.combine fns indices))
          functions
      in
      let geometry_captures =
        List.concat_map
          (fun (_, fn, index) ->
            Attribute_kernel.sources (E.Fn fn)
            |> List.map (fun source ->
                let key = Printf.sprintf "lisp_capture_geometry_%d_%d" index source in
                let factory =
                  Edit.factory ~key ~label:key ~category:[ "Flow" ] ~arity:0 (fun _ ->
                      Result.ok
                      @@ Sop.Node.Build.make_geometry ~operation:key ~version:1
                           ~parameters:(string_of_int (Sop.Node.id (List.nth inputs index)))
                           ~cook_mode:Sop.Node.Generator
                           ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
                           (fun ~node_id:_ _ _ ->
                             Result.bind (Lisp_node.input index) (fun payload ->
                                 Result.bind (Sop.Payload.kernel payload) (fun kernel ->
                                     match Sop.Kernel.geometry_capture kernel source with
                                     | Some geometry ->
                                         Ok Sop.Node.Build.{ geometry; instances = None }
                                     | None ->
                                         Error
                                           (Diagnostic.error ~code:"E_DATA_SOURCE"
                                              "A forwarded function geometry capture is unavailable")))))
                in
                (key, source, index, factory)))
          forwarded
      in
      let body =
        {
          body with
          inputs =
            List.map
              (fun (name, _, index) ->
                let key, _ = List.nth capture_factories index in
                ( name,
                  Ty.geometry,
                  Some
                    Workspace.
                      {
                        path = None;
                        ty = Ty.geometry;
                        form = Syntax.make (List []);
                        node = Call { kind = "sop/" ^ key; ctx = Context.sop; args = [] };
                      } ))
              forwarded
            @ List.map
                (fun (name, _, _, _) ->
                  ( name,
                    Ty.geometry,
                    Some
                      Workspace.
                        {
                          path = None;
                          ty = Ty.geometry;
                          form = Syntax.make (List []);
                          node = Call { kind = "sop/" ^ name; ctx = Context.sop; args = [] };
                        } ))
                geometry_captures
            @ body.inputs;
        }
      in
      let validations =
        List.mapi
          (fun index (predicate, _, _) ->
            {
              body with
              name = "lisp_validate_" ^ string_of_int index;
              context = Context.value;
              body = predicate;
            })
          definition.validations
      in
      let reached = Context_kernel.reachable_defs checked (body :: validations) in
      let nested =
        {
          checked with
          graphs = body :: validations;
          defs =
            List.filter_map
              (fun (def, uses) ->
                if not (List.memq def reached) then None
                else Some (if uses then Context_kernel.specialize ~label ~context def else def))
              (def_context_use ());
          nodes =
            List.filter
              (fun (d : Workspace.node_definition) -> d.kind.qualified <> definition.kind.qualified)
              checked.nodes;
        }
      in
      let overrides =
        List.map
          (fun (g : Workspace.graph) ->
            ( g.name,
              List.filter
                (fun (name, _) -> List.exists (fun (input, _, _) -> input = name) g.inputs)
                (parameters @ functions) ))
          nested.graphs
      in
      let invalid_parameter code message =
        Error (if code = "E_PARAM" then message else code ^ ": " ^ message)
      in
      let refused =
        match E.static ~inputs:(List.tl overrides) { nested with graphs = validations } with
        | Error d -> invalid_parameter d.code d.message
        | Ok validated ->
            let rec first index = function
              | [] -> Ok ()
              | (_, code, message) :: rest -> (
                  match List.assoc ("lisp_validate_" ^ string_of_int index) validated.results with
                  | E.Bool true -> first (index + 1) rest
                  | E.Bool false -> invalid_parameter code message
                  | _ -> invalid_arg "A Lisp node validation must return bool")
            in
            first 0 definition.validations
      in
      match refused with
      | Error _ as refused -> refused
      | Ok () ->
          (* Header validation is a constructor contract; evaluated body errors
       belong to cooking. Strict compiler refusals remain construction errors. *)
          Ok
            (try
               let body_overrides, kernel_bindings, aliases =
                 if forwarded = [] then (List.hd overrides, [], [])
                 else
                   let forwarded_evaluation =
                     match
                       E.static ~inputs:[ List.hd overrides ] { nested with graphs = [ body ] }
                     with
                     | Ok evaluated -> evaluated
                     | Error diagnostic -> raise (Diagnostic.Error diagnostic)
                   in
                   let marker name =
                     match List.assoc name forwarded_evaluation.plan.instances.(0).inputs with
                     | E.Deferred (_, source) -> source
                     | _ -> fail "E_LOWER" "A forwarded kernel needs a cooked source"
                   in
                   let remapped =
                     List.map
                       (fun (name, fn, index) ->
                         let sources =
                           List.filter_map
                             (fun (name, source, slot, _) ->
                               if slot = index then Some (source, marker name) else None)
                             geometry_captures
                         in
                         ( name,
                           E.Host.remap_function_geometry ~sources fn,
                           marker name,
                           index,
                           sources ))
                       forwarded
                   in
                   let functions =
                     List.map
                       (fun (name, value) ->
                         ( name,
                           let remap offset fn =
                             match
                               List.find_opt
                                 (fun (marker, _, _, _, _) ->
                                   marker = Printf.sprintf "lisp_kernel_%s_%d" name offset)
                                 remapped
                             with
                             | Some (_, fn, _, _, _) -> E.Fn fn
                             | None -> E.Fn fn
                           in
                           match value with
                           | E.Fn fn -> remap 0 fn
                           | E.List _ ->
                               E.List (Array.of_list (List.mapi remap (function_elements value)))
                           | _ -> value ))
                       functions
                   in
                   ( ( body.name,
                       List.filter
                         (fun (name, _) ->
                           List.exists (fun (input, _, _) -> name = input) body.inputs)
                         (parameters @ functions) ),
                     List.map (fun (_, fn, source, _, _) -> (fn, source)) remapped,
                     List.map (fun (_, _, _, index, sources) -> (index, sources)) remapped )
               in
               let capture_factories = make_capture_factories aliases in
               let nested =
                 ok
                   (lower_nested ~gpu ~kernel_bindings
                      ~factories:
                        (List.map snd capture_factories
                        @ List.map (fun (_, _, _, factory) -> factory) geometry_captures
                        @ factories)
                      ~inputs:[ body_overrides ] { nested with graphs = [ body ] })
               in
               record_profile nested.profile;
               let graph =
                 match nested.graphs with
                 | [ graph ] -> graph
                 | _ -> fail "E_LOWER" "A Lisp node body needs one SOP graph"
               in
               let root =
                 match graph.root with
                 | Some root -> root
                 | None -> fail "E_LOWER" "A Lisp node body must return geometry"
               in
               let compiled = edit (Edit.compile_node graph.network.geometry ~node_id:root) in
               let dependencies = Sop.Node.graph_dependencies compiled in
               let driven =
                 graph.network.states <> []
                 || (not (Port.Map.is_empty graph.network.drives))
                 || not (Network.Int_map.is_empty graph.network.frame_nodes)
               in
               let dependencies =
                 if driven then
                   Sop.Context.Dependencies.union dependencies (Sop.Context.Dependencies.one Input)
                 else dependencies
               in
               let reached =
                 Context_kernel.context_ops_reached ~inputs:parameters ~defs:checked.defs
                   definition.body
               in
               let dependencies =
                 List.fold_left
                   (fun dependencies (op, fact) ->
                     if List.mem op reached then
                       Sop.Context.Dependencies.union dependencies
                         (Sop.Context.Dependencies.one fact)
                     else dependencies)
                   dependencies
                   [
                     ("sop/context_seed", Sop.Context.Dependencies.Seed);
                     ("sop/context_frame", Frame);
                     ("sop/context_time", Time);
                   ]
               in
               let owned =
                 Sop.Node.inspect compiled
                 |> List.filter (fun (node : Sop.Node.info) ->
                     not (String.starts_with ~prefix:"lisp_capture_" node.operation))
               in
               let declared = definition.kind.facts in
               (* a driven body is relowered per frame, so this lowering's nodes speak only for
                  it: without a declaration its facts are the conservative ones *)
               let conservative = driven && declared = None in
               (* A declared [:facts] is the node's contract, as the native factory's was: the body's
         selection root reads "*" and replaces topology on paper, which would otherwise defeat
         component caching for every node that declares it. Session still refuses an
         undeclared write or a replaced topology at cook time. *)
               let preserved =
                 match declared with
                 | Some facts -> facts.preserves_topology
                 | None ->
                     (not conservative)
                     && List.for_all
                          (fun (node : Sop.Node.info) -> node.facts.topology = Sop.Node.Preserved)
                          owned
               in
               let input_count = List.length inputs in
               let resolve names =
                 Flow.Catalog.resolve_component_names
                   (fun name ->
                     match List.assoc_opt name parameters with
                     | Some (E.Text text) -> Some text
                     | _ -> None)
                   names
               in
               (* Declared names (parameter-named ones resolved), then what the body's attribute kernels
         name themselves. Behind a wildcard (a selection root) only the declaration speaks;
         otherwise only what the body reads counts. *)
               let components declared names =
                 let inferred =
                   List.sort_uniq String.compare
                     (List.concat_map (fun (node : Sop.Node.info) -> names node.facts) owned)
                 in
                 match declared with
                 | None -> if conservative then [ "*" ] else inferred
                 | Some declared ->
                     let declared = resolve declared in
                     if List.mem "*" inferred then
                       declared
                       @ List.filter
                           (fun name -> name <> "*" && not (List.mem name declared))
                           inferred
                     else
                       List.filter (fun name -> List.mem name inferred) declared
                       @ List.filter (fun name -> not (List.mem name declared)) inferred
               in
               let facts =
                 Sop.Node.
                   {
                     cook_mode =
                       (if input_count = 0 then Generator
                        else if preserved && input_count = 1 then Duplicate_input 0
                        else Generic);
                     elementwise =
                       (match declared with
                       | Some f when preserved -> (
                           match f.elementwise with
                           | Flow.Catalog.Points -> Points
                           | Primitives -> Primitives
                           | Irregular -> None)
                       | _ -> if preserved then (Sop.Node.facts compiled).elementwise else None);
                     reads =
                       components
                         (Option.map (fun (f : Flow.Catalog.kernel_facts) -> f.reads) declared)
                         (fun f -> f.reads);
                     writes =
                       components
                         (Option.map (fun (f : Flow.Catalog.kernel_facts) -> f.writes) declared)
                         (fun f -> f.writes);
                     topology = (if preserved then Preserved else Changed);
                     exact =
                       Option.fold ~none:true
                         ~some:(fun (facts : Flow.Catalog.kernel_facts) -> facts.exact)
                         definition.kind.facts
                       && List.for_all (fun (node : Sop.Node.info) -> node.facts.exact) owned;
                   }
               in
               let body context =
                 if not driven then Ok compiled
                 else
                   let lane = Value_lane.create () in
                   Result.bind
                     (Value_lane.resolve ~gpu ~live:(Sop.Context.input context) lane
                        ~time:(Sop.Context.time context) graph.network)
                     (fun resolved ->
                       if resolved.geometry == graph.network.geometry then Ok compiled
                       else
                         Result.map_error
                           (Diagnostic.error ~code:"E_LOWER")
                           (Edit.compile_node resolved.geometry ~node_id:root))
               in
               (body, dependencies, facts)
             with Diagnostic.Error diagnostic ->
               lisp_body_failed ~checked ~definition ~inputs ~parameters diagnostic))

module Build_cache = Lru.Make (String)

(* 2048 inlined-body kernel nodes, by cook identity (Lowerer.reuse); every access under the lock. *)
module Built = Lru.Make (String)

let built = Built.create 2048
let built_lock = Mutex.create ()

(* 512 Lisp node body builds (the 16 lowerings of a 30-node workspace fit); every access under the lock. *)
let build_cache = Build_cache.create 512
let build_lock = Mutex.create ()

let same_gpu (a : Flow_ir.Gpu.config) (b : Flow_ir.Gpu.config) =
  a.fast = b.fast && a.policy = b.policy
  && match (a.backend, b.backend) with None, None -> true | Some a, Some b -> a == b | _ -> false

module Private = struct
  let cache_hits = Atomic.make 0
  let nested_lowerings = Atomic.make 0
  let clear_cache () = Mutex.protect build_lock (fun () -> Build_cache.clear build_cache)
end

let geo_of args =
  let rec sources = function
    | E.Deferred (ty, j) when Ty.is_cooked ty -> [ j ]
    | E.List values -> Array.to_list values |> List.concat_map sources
    | _ -> []
  in
  List.concat_map (fun (_, value) -> sources value) args

(* The cooked handles (geometry and images) a value reads, through captures: what a structural
   node over it takes as inputs. *)
let cooked_sources value =
  let residuals = Hashtbl.create 16 and functions = Hashtbl.create 16 and ids = ref [] in
  let rec sources = function
    | E.Deferred (ty, id) when Ty.is_cooked ty -> ids := id :: !ids
    | E.Residual r
      when not (List.exists (( == ) r) (Hashtbl.find_all residuals (E.Residual.residual_id r))) ->
        Hashtbl.add residuals (E.Residual.residual_id r) r;
        List.iter (fun (_, v) -> sources v) (E.Residual.residual_view r).bindings
    | E.Fn fn
      when not (List.exists (( == ) fn) (Hashtbl.find_all functions (E.Residual.function_id fn))) ->
        Hashtbl.add functions (E.Residual.function_id fn) fn;
        List.iter (fun (_, v) -> sources v) (E.Residual.function_bindings fn)
    | E.Record fields | Struct (_, _, fields) | Source (_, _, fields) ->
        List.iter (fun (_, v) -> sources v) fields
    | E.List values -> Array.iter sources values
    | _ -> ()
  in
  sources value;
  List.sort_uniq Int.compare !ids

let is_volatile lowered id = Network.Int_map.mem id lowered.volatile

(* Fusion across cards (single-program design, E4). A point write whose geometry is, through card
   markers and deletions of other attributes, a point write that nothing else reads is rewritten to
   write over that write's input: its reads of an attribute the chain writes become the values
   written, its other attribute reads and counts of the chain read the input, the earlier writes
   run first in the same node ([fused_writes], one record program) and the chain's deletions run
   after them ([fused_deletes]; they commute with writes of other attributes). The earlier write is
   no longer cooked, so the maps are one region; folding left to right chains any number of
   writes. The intermediate cards are unchanged, for display and probes. *)
let literal_pattern pattern =
  not
    (String.exists
       (function '*' | '?' | '[' | '\\' | '^' | ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
       pattern)

let fused_entry ~name ~kind values =
  E.Record [ ("attribute", E.Text name); ("type", E.Text kind); ("values", values) ]

let fused_entries args =
  match List.assoc_opt fused_writes args with
  | Some (E.List entries) ->
      Array.to_list entries
      |> List.map (function
        | E.Record [ ("attribute", E.Text name); ("type", E.Text kind); ("values", values) ] ->
            (name, kind, values)
        | _ -> fail "E_LOWER" "A fused write's earlier writes are attribute/type/values records")
  | _ -> []

let fold_writes (plan : E.plan) =
  let nodes = Array.copy plan.nodes in
  let count = Array.length nodes in
  let uses = Array.make count 0 and templ = Array.make count false in
  Array.iter
    (fun (n : E.node) ->
      if is_zone n.kind then
        match (List.assoc_opt "lo" n.args, List.assoc_opt "hi" n.args) with
        | Some (E.Int lo), Some (E.Int hi) ->
            for id = max 0 lo to min count hi - 1 do
              templ.(id) <- true
            done
        | _ -> ())
    nodes;
  (* the copies' residual ids are cache keys: deterministic per plan, outside the evaluator's *)
  let next = ref 0 in
  let fresh () =
    incr next;
    -(1 lsl 50) - !next
  in
  (* A count of a geometry that only writes or deletes attributes (or a card over one) is its
     input's count: counting the input instead, a read of counts alone keeps no chain alive. *)
  let rec root id =
    if id < 0 || id >= count || templ.(id) then id
    else
      let n = nodes.(id) in
      let input key =
        match List.assoc_opt key n.args with
        | Some (E.Deferred (ty, x)) when Flow.Ty.is_geometry ty && x < id -> root x
        | _ -> id
      in
      if n.kind = E.card_kind then input "value"
      else if List.mem n.kind [ "sop/with_attr"; "sop/delete_attr"; "sop/with_group" ] then
        input "geometry"
      else id
  in
  let recount = function
    | (E.Source ("sop/count", ty, fields) | E.Struct ("sop/count", ty, fields)) as read -> (
        match List.assoc_opt "geometry" fields with
        | Some (E.Deferred (gty, x)) when root x <> x ->
            let fields =
              List.map
                (fun (k, v) -> if k = "geometry" then (k, E.Deferred (gty, root x)) else (k, v))
                fields
            in
            Some
              (match read with
              | E.Source _ -> E.Source ("sop/count", ty, fields)
              | _ -> E.Struct ("sop/count", ty, fields))
        | _ -> None)
    | _ -> None
  in
  let rec recounts = function
    | (E.Source _ | E.Struct _) as v when recount v <> None -> true
    | E.Residual r -> List.exists (fun (_, v) -> recounts v) (E.Residual.residual_view r).bindings
    | E.Fn f -> List.exists (fun (_, v) -> recounts v) (E.Residual.function_bindings f)
    | E.List vs -> Array.exists recounts vs
    | E.Record fs | E.Struct (_, _, fs) | E.Source (_, _, fs) ->
        List.exists (fun (_, v) -> recounts v) fs
    | _ -> false
  in
  Array.iteri
    (fun id (n : E.node) ->
      if (not templ.(id)) && List.exists (fun (_, v) -> recounts v) n.args then
        nodes.(id) <-
          { n with args = List.map (fun (k, v) -> (k, E.Host.rewrite ~fresh recount v)) n.args })
    nodes;
  let note values =
    List.iter
      (fun id -> if id >= 0 && id < count then uses.(id) <- uses.(id) + 1)
      (List.sort_uniq Int.compare (List.concat_map Attribute_kernel.sources values))
  in
  Array.iter (fun (n : E.node) -> note (List.map snd n.args)) nodes;
  Array.iter (fun (i : E.instance) -> note (i.result :: List.map snd i.inputs)) plan.instances;
  let text (n : E.node) k =
    match List.assoc_opt k n.args with Some (E.Text t) -> Some t | _ -> None
  in
  let kind_of n = Option.value ~default:"vec3" (text n "type") in
  let point_write (n : E.node) =
    n.kind = "sop/with_attr"
    && (not templ.(n.id))
    && List.for_all
         (fun (k, _) ->
           List.mem k
             [
               "geometry";
               "attribute";
               "values";
               "owner";
               "type";
               fused_deletes;
               fused_writes;
               fused_chain;
             ])
         n.args
    && Option.fold ~none:true ~some:(( = ) "point") (text n "owner")
    && text n "attribute" <> None
  in
  let deletes (n : E.node) =
    match List.assoc_opt fused_deletes n.args with Some (E.List ds) -> Array.to_list ds | _ -> []
  in
  let pattern = function E.Record [ _; ("pattern", E.Text p) ] -> Some p | _ -> None in
  (* back from a write's geometry to the earlier write: the ids passed and the deletions *)
  let rec back name id chain dels =
    if id < 0 || id >= count || uses.(id) <> 1 || templ.(id) then None
    else
      let u = nodes.(id) in
      if u.kind = E.card_kind then
        match List.assoc_opt "value" u.args with
        | Some (E.Deferred (_, x)) -> back name x (id :: chain) dels
        | _ -> None
      else if u.kind = "sop/delete_attr" then
        match (List.assoc_opt "geometry" u.args, text u "attribute") with
        | Some (E.Deferred (_, x)), Some pattern
          when pattern <> name && literal_pattern pattern
               && List.for_all
                    (fun (k, _) -> List.mem k [ "geometry"; "attribute"; "owner" ])
                    u.args ->
            let owner = Option.value ~default:"point" (text u "owner") in
            back name x (id :: chain)
              (E.Record [ ("owner", E.Text owner); ("pattern", E.Text pattern) ] :: dels)
        | _ -> None
      else if point_write u && not (List.mem (Some name) (List.map pattern (deletes u))) then
        match List.assoc_opt "geometry" u.args with
        | Some (E.Deferred (_, base)) -> Some (u, base, id :: chain, deletes u @ dels)
        | _ -> None
      else None
  in
  let fold id (w : E.node) =
    match (List.assoc_opt "geometry" w.args, text w "attribute") with
    | Some (E.Deferred (_, g)), Some name when point_write w -> (
        match back name g [] [] with
        | None -> ()
        | Some (u, base, chain, dels) ->
            let deleted = List.filter_map pattern dels
            and reads = Attribute_kernel.reads (List.assoc "values" w.args) in
            if not (List.mem "*" reads || List.exists (fun p -> List.mem p reads) deleted) then begin
              (* what the chain writes, in order, over [base] *)
              let entries =
                fused_entries u.args
                @ [ (Option.get (text u "attribute"), kind_of u, List.assoc "values" u.args) ]
              in
              let written a = List.find_opt (fun (n, _, _) -> n = a) (List.rev entries) in
              let fits = ref true in
              let on_base fields =
                ("geometry", E.Deferred (Flow.Ty.geometry, base))
                :: List.remove_assoc "geometry" fields
              in
              (* A lazy term that reads the chain itself ([(map f (sop/attr in0 :P))]): its
                 reads of a written attribute become captures of the values written, and the
                 geometry it captures is the input. Any other use of the capture refuses. *)
              let retarget (term : Workspace.term) bindings =
                let chained =
                  List.filter_map
                    (function
                      | name, E.Deferred (ty, x) when Flow.Ty.is_geometry ty && List.mem x chain ->
                          Some name
                      | _ -> None)
                    bindings
                in
                if chained = [] then None
                else
                  let extra = ref [] in
                  let rec binds = function
                    | Workspace.Name n -> List.mem n chained
                    | Seq ps -> List.exists binds ps
                    | Keys ns -> List.exists (fun n -> List.mem n chained) ns
                  in
                  let literal (t : Workspace.term) =
                    match t.node with Text s -> Some s | _ -> None
                  in
                  let rec map (t : Workspace.term) =
                    match t.node with
                    | Ref_binding (x, _) when List.mem x chained -> raise Exit
                    | Op { op = "sop/count"; args; _ }
                      when match List.assoc_opt "geometry" args with
                           | Some { node = Ref_binding (x, []); _ } -> List.mem x chained
                           | _ -> false ->
                        t
                    | Op { op = "sop/attr"; args; _ }
                      when match List.assoc_opt "geometry" args with
                           | Some { node = Ref_binding (x, []); _ } -> List.mem x chained
                           | _ -> false -> (
                        let attribute =
                          match List.assoc_opt "attribute" args with
                          | Some a -> literal a
                          | None -> None
                        in
                        match Option.map written attribute with
                        | (None | Some None) when attribute <> None -> t
                        | Some (Some (_, kind, earlier))
                          when List.for_all
                                 (fun (k, v) ->
                                   match k with
                                   | "geometry" | "attribute" -> true
                                   | "owner" -> literal v = Some "point"
                                   | "type" -> literal v = Some kind
                                   | _ -> false)
                                 args ->
                            let name = "$fold" ^ string_of_int (List.length !extra) in
                            extra := (name, E.Host.rewrite ~fresh (fun _ -> None) earlier) :: !extra;
                            { t with node = Ref_binding (name, []) }
                        | _ -> raise Exit)
                    | Let (steps, _) when List.exists (fun (p, _) -> binds p) steps -> raise Exit
                    | Loop { accs; clauses; _ }
                      when List.exists (fun (p, _) -> binds p) (accs @ clauses) ->
                        raise Exit
                    | Fn { params; _ } when List.exists (fun (p, _) -> binds p) params -> raise Exit
                    | State { binder; _ } when binds binder -> raise Exit
                    | _ -> Workspace.map_children map t
                  in
                  match map term with
                  | term ->
                      Some
                        ( term,
                          List.map (fun name -> (name, E.Deferred (Flow.Ty.geometry, base))) chained
                          @ !extra )
                  | exception Exit ->
                      fits := false;
                      None
              in
              let values =
                E.Host.rewrite ~fresh ~retarget
                  (function
                    | (E.Struct (op, ty, fields) | E.Source (op, ty, fields)) as read -> (
                        let chained =
                          match List.assoc_opt "geometry" fields with
                          | Some (E.Deferred (_, x)) -> List.mem x chain
                          | _ -> false
                        in
                        let rebuilt fields =
                          match read with
                          | E.Struct _ -> E.Struct (op, ty, fields)
                          | _ -> E.Source (op, ty, fields)
                        in
                        match (chained, op, List.assoc_opt "attribute" fields) with
                        | false, _, _ -> None
                        | true, "sop/attr", Some (E.Text a) -> (
                            match written a with
                            | None -> Some (rebuilt (on_base fields))
                            | Some (_, kind, earlier)
                              when List.for_all
                                     (fun (k, v) ->
                                       match k with
                                       | "geometry" | "attribute" -> true
                                       | "owner" -> v = E.Text "point"
                                       | "type" -> v = E.Text kind
                                       | _ -> false)
                                     fields ->
                                Some (E.Host.rewrite ~fresh (fun _ -> None) earlier)
                            | Some _ ->
                                fits := false;
                                Some read)
                        | true, "sop/count", _ -> Some (rebuilt (on_base fields))
                        | true, _, _ ->
                            fits := false;
                            Some read)
                    | E.Deferred (ty, x) when Flow.Ty.is_geometry ty && List.mem x chain ->
                        fits := false;
                        None
                    | _ -> None)
                  (List.assoc "values" w.args)
              in
              (* an attribute's last write wins; this write supersedes the chain's of [name] *)
              let rec last = function
                | [] -> []
                | ((n, _, _) as e) :: rest ->
                    if n = name || List.exists (fun (m, _, _) -> m = n) rest then last rest
                    else e :: last rest
              in
              let earlier =
                List.map (fun (name, kind, values) -> fused_entry ~name ~kind values) (last entries)
              in
              (* the folded writes, whose cards key this one's kernels with its own *)
              let chain_ids =
                (match List.assoc_opt fused_chain u.args with
                  | Some (E.List ids) -> Array.to_list ids
                  | _ -> [])
                @ [ E.Int u.id ]
              in
              if !fits then
                nodes.(id) <-
                  {
                    w with
                    args =
                      (List.filter_map
                         (fun (k, v) ->
                           if k = "geometry" then Some (k, E.Deferred (Flow.Ty.geometry, base))
                           else if k = "values" then Some (k, values)
                           else if k = fused_deletes || k = fused_writes || k = fused_chain then
                             None
                           else Some (k, v))
                         w.args
                      @ [ (fused_chain, E.List (Array.of_list chain_ids)) ]
                      @ (if earlier = [] then []
                         else [ (fused_writes, E.List (Array.of_list earlier)) ])
                      @ if dels = [] then [] else [ (fused_deletes, E.List (Array.of_list dels)) ]);
                  }
            end)
    | _ -> ()
  in
  Array.iteri fold nodes;
  { plan with nodes }

let same_function a b =
  a == b
  || E.Residual.function_id a = E.Residual.function_id b
     && E.Residual.function_origin a = E.Residual.function_origin b
     && E.Host.function_scope a = E.Host.function_scope b
     && E.Residual.function_name a = E.Residual.function_name b
     && List.equal
          (fun (an, av) (bn, bv) -> an = bn && av == bv)
          (E.Residual.function_bindings a) (E.Residual.function_bindings b)

let objects lowered =
  List.filter_map
    (fun (graph : graph) ->
      Option.map (fun root -> (graph.instance, graph.network, root)) graph.root)
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

(* Physical, elementwise: a nested lowering shares its forms. A structural [=] walks the whole
   library for every lookup, seconds per lowering of a freshly loaded workspace. *)
let same_content_list left right = left == right || List.equal ( == ) left right

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
  let key =
    {
      source_forms = checked.source;
      macro_forms = checked.macros;
      definitions = checked.defs;
      node_definitions = checked.nodes;
      graph_forms = checked.graphs;
      extra_definitions = extra;
    }
  in
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
  val clock : unit -> float
  val on_profile : (Flow_ir.Profile.t -> unit) option
  val compiled_ids : int Path_map.t
  val sites : Workspace.path list
  val inputs : (string * (string * Eval.value) list) list option
  val checked : Workspace.t

  val lower_checked :
    factories:Edit.factory list ->
    ?provenance_tags:bool ->
    ?reference:bool ->
    ?gpu:Flow_ir.Gpu.config ->
    ?strict_body:bool ->
    ?defer_body_errors:bool ->
    ?kernel_bindings:(E.fn * int) list ->
    ?clock:(unit -> float) ->
    ?on_profile:(Flow_ir.Profile.t -> unit) ->
    ?compiled_ids:int Path_map.t ->
    ?sites:Workspace.path list ->
    ?inputs:(string * (string * Eval.value) list) list ->
    Workspace.t ->
    (t, Diagnostic.t) result
end) =
struct
  open P

  let image_origins = Hashtbl.create 16

  let observe producer residual =
    let view = E.Residual.residual_view residual in
    let key = (view.instance, view.site, view.iter) in
    let origin =
      match Hashtbl.find_opt image_origins key with
      | None -> producer
      | Some (Some previous) when producer = Some previous -> producer
      | _ -> None
    in
    Hashtbl.replace image_origins key origin

  let checked, evaluated =
    match Flow_ir.qualify_workspace ~record:true ?inputs ~observe ~gpu checked with
    | Ok evaluated -> evaluated
    | Error diagnostic when defer_body_errors -> raise (Diagnostic.Error diagnostic)
    | Error diagnostic -> raise (Diagnostic.Error diagnostic)

  let profile = Flow_ir.Profile.create ~clock

  (* Kernel bodies are cache inputs as well as their captures. The digest is
       lazy so ordinary catalog edits retain their existing pipeline counts.
       ponytail: the whole source invalidates kernels; narrow to body/defn cones
       if unrelated structural edits measurably recook them. *)
  let kernel_source =
    lazy (cached_digest checked [] (fun () -> Context_kernel.kernel_source checked))

  let attribute_options = attribute_options
  and is_geometry_arrays = is_geometry_arrays
  and geometry_arrays_key = geometry_arrays_key
  and geometry_arrays_label = geometry_arrays_label
  and geometry_arrays_data = geometry_arrays_data

  (* A node of an inlined body (its site is under a card's [node:kind]) compiles strictly, as the
     nested body did; its refusals cook as errors rather than refusing the program. *)
  let inlined = function
    | Some (n : E.node) -> List.exists (String.starts_with ~prefix:"node:") n.site
    | None -> false

  (* An inlined body's kernels key on its card (declaration cone and canonical parameters), not on
     the whole authored source, so an edit elsewhere keeps them cached, as the card was. A card
     whose parameters are not plain data (a live argument) keys on the whole source. *)
  let card_sources =
    lazy
      (let table = Hashtbl.create 16 in
       Array.iter
         (fun (n : E.node) ->
           if n.kind = E.card_kind then
             match (List.assoc_opt "kind" n.args, List.assoc_opt "parameters" n.args) with
             | Some (E.Text kind), Some parameters -> (
                 match
                   ( List.find_opt
                       (fun (d : Workspace.node_definition) -> d.kind.qualified = kind)
                       checked.nodes,
                     Flow.Value.key_of ~residual:(fun _ -> raise Exit) parameters )
                 with
                 | Some definition, key ->
                     let source =
                       cached_digest checked [ definition ] (fun () ->
                           Context_kernel.declaration_source checked definition)
                     in
                     Hashtbl.add table
                       (n.site @ [ "node:" ^ kind ])
                       (n.iter, lazy (Digest.to_hex (Digest.string (source ^ key))))
                 | None, _ -> ()
                 | exception Exit -> ())
             | _ -> ())
         evaluated.plan.nodes;
       table)

  let rec kernel_source_of = function
    | Some (n : E.node) when List.mem_assoc fused_chain n.args ->
        (* a fused write keys on every card it folds *)
        let chain =
          match List.assoc fused_chain n.args with
          | E.List ids ->
              Array.to_list ids
              |> List.filter_map (function E.Int id -> Some evaluated.plan.nodes.(id) | _ -> None)
          | _ -> []
        in
        let own = kernel_source_of (Some { n with args = List.remove_assoc fused_chain n.args }) in
        lazy
          (Digest.to_hex
             (Digest.string
                (String.concat ";"
                   (List.map (fun u -> Lazy.force (kernel_source_of (Some u))) chain
                   @ [ Lazy.force own ]))))
    | Some (n : E.node) when inlined (Some n) || n.kind = E.card_kind -> (
        (* the innermost card: the site up to its last node: segment *)
        let rec prefixes acc best = function
          | [] -> best
          | segment :: rest ->
              let acc = acc @ [ segment ] in
              prefixes acc
                (if String.starts_with ~prefix:"node:" segment then Some acc else best)
                rest
        in
        let rec is_prefix a b =
          match (a, b) with [], _ -> true | x :: a, y :: b -> x = y && is_prefix a b | _ -> false
        in
        let site =
          match List.assoc_opt "kind" n.args with
          | Some (E.Text kind) when n.kind = E.card_kind -> Some (n.site @ [ "node:" ^ kind ])
          | _ -> prefixes [] None n.site
        in
        match site with
        | None -> kernel_source
        | Some site -> (
            match
              List.find_opt
                (fun (iter, _) -> is_prefix iter n.iter)
                (Hashtbl.find_all (Lazy.force card_sources) site)
            with
            | Some (_, source) -> source
            | None -> kernel_source))
    | _ -> kernel_source

  (* An inlined body's kernel node, built once for its card: an edit elsewhere relowers the program
     but takes the node it built (over its new inputs), as the nested card took its body build. The
     key is the node's cook identity (the card's key, the data, the inputs'). *)
  let reuse ?node ?state ?elems what data inputs build =
    let key () =
      String.concat "\000"
        ([
           Lazy.force (kernel_source_of node);
           what;
           Flow.Value.key_of ~residual:E.Residual.residual_id data;
           string_of_bool reference;
           string_of_bool strict_body;
           string_of_bool gpu.fast;
         ]
        @ List.map
            (fun input -> string_of_int (Sop.Node.id input) ^ Sop.Node.Build.cache_parameters input)
            inputs)
    in
    let card = match node with Some (n : E.node) -> n.kind = E.card_kind | None -> false in
    if (not (inlined node || card)) || state <> None || elems <> None || gpu.backend <> None then
      build ()
    else
      match key () with
      | exception _ -> build ()
      | key -> (
          match Mutex.protect built_lock (fun () -> Built.find_opt built key) with
          | Some node -> Sop.Node.Build.rebuild_with_inputs node (Array.of_list inputs)
          | None ->
              let node = build () in
              Mutex.protect built_lock (fun () -> Built.add built key node);
              node)

  let geometry_arrays_node ?node ?(strict_body = strict_body) ?host ?state ?elems ~operation ~args
      ~sources inputs =
    reuse ?node ?state ?elems
      ("arrays " ^ operation ^ Option.fold ~none:"" ~some:string_of_bool host)
      (E.Record args) inputs
    @@ fun () ->
    geometry_arrays_node ~check:strict_body ~profile
      ~strict_body:(strict_body || inlined node)
      ~reference ~approx:checked.approx ~kernel_source:(kernel_source_of node) ?host ?state ?elems
      ~operation ~args ~sources inputs

  let structural_node ?node residual captures =
    let kernel_source = kernel_source_of node in
    per_context (E.Residual residual) @@ fun specialize ->
    let residual =
      match specialize (E.Residual residual) with E.Residual r -> r | _ -> assert false
    in
    let program =
      match Flow_ir.Core_lower.lower ~strict:true ~budget:32768 residual with
      | Ok program -> program
      | Error name -> fail "E_PACKED_FORM" ("Cooked selection cannot compile " ^ name ^ ".")
    in
    let prepared = Flow_ir.Core_exec.prepare program in
    let inputs = Array.of_list (List.map (fun (_, _, node) -> node) captures) in
    let node =
      Sop.Node.Build.make ~operation:"flow.geometry_select" ~version:1
        ~parameters:(Lazy.force kernel_source) ~cook_mode:Sop.Node.Generic ~inputs
        ~dependencies:(Attribute_kernel.dependencies (E.Residual residual))
        (fun ~node_id:_ context payloads ->
          let session =
            lazy
              (let session =
                 Result.get_ok (Sop.Session.create ~max_entries:0 ~max_payload_bytes:0)
               in
               Sop.Session.set_parallel_override session (Some false);
               session)
          in
          Fun.protect
            ~finally:(fun () -> if Lazy.is_val session then Sop.Session.close (Lazy.force session))
            (fun () ->
              let handles = Hashtbl.create 16
              and geometries = Hashtbl.create 16
              and next = ref (-1) in
              List.iteri
                (fun index (id, ty, _) ->
                  Hashtbl.add handles id
                    ( ty,
                      None,
                      ref (Some Sop.Session.{ payload = payloads.(index); instances = None }) ))
                captures;
              let error code message = Error (Diagnostic.error ~code message) in
              let handle ty id =
                match Hashtbl.find_opt handles id with
                | Some (actual, node, output) when actual = ty -> Ok (node, output)
                | Some _ -> error "E_DATA_SOURCE" "A cooked handle has the wrong declared type."
                | None -> error "E_DATA_SOURCE" "A cooked handle is missing from this execution."
              in
              let snapshot ty id =
                Result.bind (handle ty id) (fun (node, output) ->
                    let validate (value : Sop.Session.output) =
                      match (ty, value.payload) with
                      | ty, Sop.Payload.Geometry _ when ty = Ty.geometry -> Ok value
                      | ty, Sop.Payload.Image _ when ty = Ty.image -> Ok value
                      | _ ->
                          error "E_DATA_SOURCE"
                            "A cooked handle returned a payload of the wrong type."
                    in
                    match (!output, node) with
                    | Some output, _ -> validate output
                    | None, None ->
                        error "E_DATA_SOURCE" "A cooked handle has no result or prepared node."
                    | None, Some node ->
                        Result.bind
                          (Sop.Session.cook (Lazy.force session) ~context node)
                          (fun value ->
                            Result.map
                              (fun value ->
                                output := Some value;
                                value)
                              (validate value)))
              in
              let geometry id =
                match Hashtbl.find_opt geometries id with
                | Some geometry -> Some geometry
                | None ->
                    let output = ok (snapshot Ty.geometry id) in
                    let geometry =
                      match (output.payload, output.instances) with
                      | Sop.Payload.Geometry geometry, None -> geometry
                      | Sop.Payload.Geometry source, Some transforms -> (
                          match Rdk.Instance_copy.materialize_instances ~transforms source with
                          | Ok geometry -> geometry
                          | Error e -> fail "E_DATA_SOURCE" (Rdk.Error.to_string e))
                      | _ -> fail "E_DATA_SOURCE" "A geometry source has an image payload."
                    in
                    Hashtbl.add geometries id geometry;
                    Some geometry
              in
              let register_output ty (output : Sop.Session.output) =
                let id = !next in
                decr next;
                Hashtbl.add handles id (ty, None, ref (Some output));
                E.Deferred (ty, id)
              in
              let rec cooked_term factories ty value =
                match value with
                | E.Deferred (actual, id) when Ty.is_cooked actual ->
                    if actual <> ty then
                      fail "E_DATA_SOURCE" "A cooked argument has the wrong declared type.";
                    ignore (ok (handle actual id));
                    let key = "lisp_host_capture_" ^ string_of_int (List.length !factories) in
                    (* Editable zero-input factories must return zero-input nodes:
                   compiling a borrowed source directly would disconnect it. *)
                    let factory =
                      Edit.factory ~key ~label:key ~category:[ "Lisp" ] ~arity:0 (fun _ ->
                          Result.ok
                          @@ Sop.Node.Build.make ~operation:key ~version:1
                               ~parameters:(string_of_int id) ~cook_mode:Sop.Node.Generator
                               ~inputs:[||] ~dependencies:Sop.Context.Dependencies.static
                               (fun ~node_id:_ _context _inputs ->
                                 Result.map
                                   (fun (output : Sop.Session.output) ->
                                     Sop.Node.Build.
                                       { payload = output.payload; instances = output.instances })
                                   (snapshot actual id)))
                    in
                    factories := factory :: !factories;
                    Some
                      Workspace.
                        {
                          path = None;
                          ty;
                          form = Syntax.make (List []);
                          node =
                            Call
                              {
                                kind = "sop/" ^ key;
                                ctx =
                                  (if actual = Ty.image then Flow.Context.image else Context.sop);
                                args = [];
                              };
                        }
                | E.No_geo when ty = Ty.geometry ->
                    Some Workspace.{ path = None; ty; form = Syntax.make (Sym "nil"); node = Nil }
                | E.List values ->
                    let element =
                      match ty with
                      | Ty.List element -> element
                      | _ -> fail "E_DATA_SOURCE" "A cooked list has the wrong type."
                    in
                    let values = Array.to_list values |> List.map (cooked_term factories element) in
                    if List.for_all Option.is_some values then
                      Some
                        Workspace.
                          {
                            path = None;
                            ty;
                            form = Syntax.make (List []);
                            node = List_lit (List.map Option.get values);
                          }
                    else None
                | _ -> None
              in
              let rec deferred = function
                | E.Source _ | E.Struct (_, Ty.Array _, _) -> true
                | E.Record fields -> List.exists (fun (_, value) -> deferred value) fields
                | E.List values -> Array.exists deferred values
                | _ -> false
              in
              let constant = function
                | E.Source (name, _, _) -> String.starts_with ~prefix:"$core/constant-array/" name
                | _ -> false
              in
              let force_args ?(keep_constant = false) args =
                List.map
                  (fun (name, value) ->
                    ( name,
                      if deferred value && not (keep_constant && name = "values" && constant value)
                      then
                        ok
                          (E.Host.force_reference
                             ~resolve:(Attribute_kernel.resolve ~context ~geometry)
                             value ~live:(Sop.Context.input context))
                      else value ))
                  args
              in
              (* sop/mesh, sop/mesh_planes and sop/remap build their node directly from the evaluated arguments: the former
             synthetic one-node workspace was checked, evaluated and lowered per call. *)
              let host_geometry_arrays ty name args =
                let op =
                  match Flow.Op.find ~extra:checked.ops name Context.sop with
                  | Some op -> op
                  | None -> fail "E_DATA_SOURCE" "A host call has no checked operator."
                in
                (try
                   op.Flow.Op.check args;
                   let scalar = function
                     | E.Int n -> Some (Param.Int_value n)
                     | Float n -> Some (Param.Float_value n)
                     | Bool b -> Some (Param.Bool_value b)
                     | Text t -> Some (Param.Text_value t)
                     | _ -> None
                   in
                   ignore
                     (Flow.Op.result_type op
                        (List.map (fun (n, v) -> (n, Flow.Value.ty_of v, scalar v)) args))
                 with Diagnostic.Error d -> raise (Diagnostic.Error d));
                let sources =
                  let direct = geo_of args in
                  direct
                  @ List.filter
                      (fun id -> not (List.mem id direct))
                      (Attribute_kernel.sources (geometry_arrays_data args))
                in
                let inputs =
                  List.map
                    (fun id ->
                      let actual =
                        match Hashtbl.find_opt handles id with
                        | Some (actual, _, _) -> actual
                        | None ->
                            fail "E_DATA_SOURCE" "A cooked handle is missing from this execution."
                      in
                      ignore (ok (handle actual id));
                      Sop.Node.Build.make ~operation:"lisp_host_capture" ~version:1
                        ~parameters:(string_of_int id) ~cook_mode:Sop.Node.Generator ~inputs:[||]
                        ~dependencies:Sop.Context.Dependencies.static
                        (fun ~node_id:_ _context _inputs ->
                          Result.map
                            (fun (output : Sop.Session.output) ->
                              Sop.Node.Build.
                                { payload = output.payload; instances = output.instances })
                            (snapshot actual id)))
                    sources
                in
                let node =
                  geometry_arrays_node ~strict_body:true ~host:true ~operation:name ~args ~sources
                    inputs
                in
                let id = !next in
                decr next;
                Hashtbl.add handles id (ty, Some node, ref None);
                Ok (E.Deferred (ty, id))
              in
              let call ty catalog name args =
                if
                  catalog
                  && (not
                        (List.exists
                           (fun (n : Workspace.node_definition) -> n.kind.qualified = name)
                           checked.nodes))
                  && not
                       (List.exists
                          (fun factory -> "sop/" ^ Edit.factory_key factory = name)
                          factories)
                then fail "E_DATA_SOURCE" "A host call has no checked catalog kind.";
                if
                  (not catalog) && Option.is_none (Flow.Op.find ~extra:checked.ops name Context.sop)
                then fail "E_DATA_SOURCE" "A host call has no checked operator.";
                let guard =
                  if catalog || name <> "sop/with_attr" then None
                  else
                    Option.map
                      (fun guard ->
                        match force_args [ ("when", guard) ] with
                        | [ (_, E.Bool b) ] -> b
                        | _ -> fail "E_ATTR_TYPE" "sop/with_attr :when must be a bool")
                      (List.assoc_opt "when" args)
                in
                if guard = Some false then Ok (List.assoc "geometry" args)
                else if (not catalog) && name = "sop/delete_attr" then
                  (* as its node cooks it: a one-call workspace would requalify the catalog *)
                  match force_args args with
                  | args -> (
                      match
                        ( List.assoc_opt "geometry" args,
                          List.assoc_opt "attribute" args,
                          Option.value ~default:(E.Text "point") (List.assoc_opt "owner" args) )
                      with
                      | Some (E.Deferred (actual, id)), Some (E.Text pattern), E.Text owner
                        when actual = Ty.geometry ->
                          let target = Option.get (geometry id) in
                          Result.map
                            (fun geometry ->
                              register_output Ty.geometry
                                { payload = Sop.Payload.Geometry geometry; instances = None })
                            (Attribute_kernel.delete_after
                               [ (owner, pattern) ]
                               (Sop.Context.cancel_token context)
                               target)
                      | _ -> error "E_DATA_SOURCE" "sop/delete_attr needs geometry and text.")
                else
                  let args = List.remove_assoc "when" args in
                  let selected = List.mem_assoc "selection" args || List.mem_assoc "default" args in
                  let args =
                    if
                      (not catalog) && name = "sop/with_attr"
                      &&
                      if selected then
                        match List.assoc_opt "values" args with
                        | Some (E.Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _) ->
                            true
                        | Some value -> constant value
                        | _ -> false
                      else
                        match (List.assoc_opt "geometry" args, List.assoc_opt "values" args) with
                        | Some (E.Deferred (ty, _)), Some (E.Record _ as values)
                          when ty = Ty.geometry && deferred values -> (
                            match List.assoc_opt "type" args with
                            | None -> true
                            | Some (E.Text kind) -> List.mem kind [ "vec2"; "vec3"; "vec4" ]
                            | _ -> false)
                        | _ -> false
                    then begin
                      if Sop.Context.cancelled context then
                        fail "E_CANCELLED" "Attribute kernel cancelled.";
                      if not selected then ignore (attribute_options args);
                      force_args ~keep_constant:true args
                    end
                    else args
                  in
                  let borrowed =
                    if catalog || name <> "sop/with_attr" then Ok None
                    else
                      match
                        ( List.assoc_opt "geometry" args,
                          List.assoc_opt "attribute" args,
                          List.assoc_opt "values" args )
                      with
                      | Some (E.Deferred (actual, id)), Some (E.Text name), Some values
                        when actual = Ty.geometry ->
                          if Sop.Context.cancelled context then
                            error "E_CANCELLED" "Attribute kernel cancelled."
                          else
                            let owner, kind = attribute_options args in
                            let target = Option.get (geometry id) in
                            let materialized =
                              match values with
                              | E.Float_array _ | Int_array _ | Text_array _ | Vec2_array _
                              | Vec3_array _ | Vec4_array _ ->
                                  true
                              | Record fields -> (
                                  if (not selected) && List.mem kind [ "vec2"; "vec3"; "vec4" ] then
                                    not (E.is_live values)
                                  else
                                    match
                                      ( List.assoc_opt "offsets" fields,
                                        List.assoc_opt "values" fields )
                                    with
                                    | Some (E.Int_array _), Some (E.Float_array _ | Int_array _) ->
                                        true
                                    | _ -> false)
                              | _ -> false
                            in
                            let selection = List.assoc_opt "selection" args
                            and default = List.assoc_opt "default" args in
                            Result.bind
                              (if selection <> None || default <> None then Ok None
                               else
                                 Attribute_kernel.write_source ~geometry ~owner ~kind name values
                                   target)
                              (fun result ->
                                let selected_materialized =
                                  match (selection, default) with
                                  | None, None -> true
                                  | ( Some (E.Int_array _),
                                      Some (E.Float _ | Int _ | Vec2 _ | Vec3 _ | Vec4 _) ) ->
                                      true
                                  | _ -> false
                                in
                                let result =
                                  match result with
                                  | Some _ -> Ok result
                                  | None when constant values && selected_materialized ->
                                      Attribute_kernel.write_constant_source
                                        ~grain:(Sop.Context.grain context) ~owner ~kind name
                                        ?selection ?default values target
                                  | None when materialized && selected_materialized ->
                                      (* The former checked-call input boundary validated before
                       the writer checked destination cardinality. *)
                                      Flow.Value.validate values;
                                      Result.map Option.some
                                        (match (selection, default) with
                                        | None, None ->
                                            Attribute_kernel.write
                                              ~grain:(Sop.Context.grain context) ~owner ~kind name
                                              values target
                                        | Some selection, Some default ->
                                            Flow.Value.validate selection;
                                            Flow.Value.validate default;
                                            Attribute_kernel.write_selected
                                              ~grain:(Sop.Context.grain context) ~owner ~kind name
                                              ~values ~selection ~default target
                                        | _ ->
                                            error "E_ATTR_SELECTION"
                                              "Selected writes require both selection and default.")
                                  | None -> Ok None
                                in
                                Result.bind result (fun result ->
                                    if Sop.Context.cancelled context then
                                      error "E_CANCELLED" "Attribute kernel cancelled."
                                    else
                                      Ok
                                        (Option.map
                                           (fun geometry ->
                                             ignore (ok (snapshot Ty.geometry id));
                                             register_output Ty.geometry
                                               {
                                                 payload = Sop.Payload.Geometry geometry;
                                                 instances = None;
                                               })
                                           result)))
                      | _ -> Ok None
                  in
                  Result.bind borrowed (function
                    | Some result -> Ok result
                    | None ->
                        let args = force_args args in
                        if (not catalog) && is_geometry_arrays name then
                          host_geometry_arrays ty name args
                        else
                          let call_context =
                            if ty = Ty.image then Flow.Context.image else Context.sop
                          in
                          let form = Syntax.make (List []) in
                          let capture_factories = ref [] and overrides = ref [] in
                          let inputs =
                            List.map
                              (fun (name, value) ->
                                let ty = Flow.Value.ty_of value in
                                let default = cooked_term capture_factories ty value in
                                if default = None then overrides := (name, value) :: !overrides;
                                (name, ty, default))
                              args
                          in
                          let args =
                            List.map
                              (fun (name, ty, _) ->
                                ( name,
                                  Workspace.{ path = None; ty; form; node = Ref_binding (name, []) }
                                ))
                              inputs
                          in
                          let body =
                            Workspace.
                              {
                                path = None;
                                ty;
                                form;
                                node =
                                  (if catalog then Call { kind = name; ctx = call_context; args }
                                   else Op { op = name; args; skip = [] });
                              }
                          in
                          let graph =
                            Workspace.
                              { name = "lisp_host_call"; context = Context.sop; inputs; body; form }
                          in
                          (* an operator call with no function argument needs none of the library:
                           qualifying it would walk every definition per call *)
                          let rec callable = function
                            | E.Fn _ -> true
                            | E.List vs -> Array.exists callable vs
                            | E.Record fs | E.Struct (_, _, fs) ->
                                List.exists (fun (_, v) -> callable v) fs
                            | _ -> false
                          in
                          let library =
                            if catalog || List.exists (fun (_, v) -> callable v) !overrides then
                              checked
                            else
                              { checked with defs = []; nodes = []; packed = []; packed_roots = [] }
                          in
                          Result.bind
                            (lower_checked ~reference ~gpu ~strict_body:true
                               ~factories:(!capture_factories @ factories)
                               ~inputs:[ (graph.name, !overrides) ]
                               { library with graphs = [ graph ] })
                            (fun lowered ->
                              match lowered.graphs with
                              | [ graph ] -> (
                                  match graph.root with
                                  | None ->
                                      error "E_DATA_SOURCE" "A cooked call returned no result."
                                  | Some root ->
                                      Result.bind
                                        (Result.map_error
                                           (Diagnostic.error ~code:"E_LOWER")
                                           (Edit.compile_node graph.network.geometry ~node_id:root))
                                        (fun node ->
                                          let id = !next in
                                          decr next;
                                          Hashtbl.add handles id (ty, Some node, ref None);
                                          Ok (E.Deferred (ty, id))))
                              | _ ->
                                  error "E_DATA_SOURCE"
                                    "A cooked call must return one typed result."))
              in
              let resolve value =
                try
                  match value with
                  | E.Source
                      ( "$core/diagnostic_index",
                        Ty.Int,
                        [ ("rows", rows); ("policy", E.Text policy) ] ) ->
                      let grain =
                        match policy with
                        | "first" -> Ok None
                        | "range-last" -> Ok (Some (Sop.Context.grain context))
                        | _ -> error "E_DIAGNOSTIC_CONTEXT" "Unknown host diagnostic policy."
                      in
                      Result.bind grain (fun grain ->
                          let selected =
                            try
                              let check () =
                                if Sop.Context.cancelled context then
                                  Flow.Value.fail "cancelled" "geometry operation was cancelled"
                              in
                              Ok (Flow.Value.error_row_index ?grain ~check rows)
                            with Diagnostic.Error d -> Error d
                          in
                          Result.bind selected (function
                            | Some index -> Ok (E.Int index)
                            | None ->
                                error "E_DIAGNOSTIC_CONTEXT" "Host diagnosis needs a failing row."))
                  | E.Source ("$core/diagnostic_index", _, _) ->
                      error "E_DIAGNOSTIC_CONTEXT" "Host diagnosis needs typed rows and policy."
                  | E.Source ("$core/complete", ty, [ ("value", E.Deferred (actual, id)) ])
                    when actual = ty && Ty.is_cooked ty ->
                      Result.map (fun _ -> E.Deferred (ty, id)) (snapshot ty id)
                  | E.Source ("$core/complete", ty, [ ("value", E.No_geo) ]) when ty = Ty.geometry
                    ->
                      Ok E.No_geo
                  | E.Source ("$core/complete", _, _) ->
                      error "E_DATA_SOURCE" "Protected completion needs one matching cooked handle."
                  | E.Source ("$core/cooked", ty, [ ("id", E.Int id) ]) ->
                      Result.map (fun _ -> E.Deferred (ty, id)) (handle ty id)
                  | E.Source ((("$core/operator" | "$core/catalog") as kind), ty, args) -> (
                      if not (Ty.is_cooked ty) then
                        error "E_DATA_SOURCE" "Host calls return a declared cooked type."
                      else
                        match List.assoc_opt "$name" args with
                        | Some (E.Text name) ->
                            call ty (kind = "$core/catalog") name
                              (List.filter
                                 (fun (name, _) -> name <> "$name" && name <> "$context")
                                 args)
                        | _ -> error "E_DATA_SOURCE" "A host call needs its checked kind.")
                  | value -> Attribute_kernel.resolve ~context ~geometry value
                with Diagnostic.Error d -> Error d
              in
              let result =
                Flow_ir.Core_exec.execute ~resolve
                  ~force:(fun program ->
                    Flow_ir.Packed.force ~strict:true ~resolve program
                      ~live:(Sop.Context.input context))
                  prepared ~live:(Sop.Context.input context)
              in
              let result =
                Result.bind result (function
                  | E.Deferred (ty, id) when ty = Ty.geometry -> snapshot ty id
                  | E.No_geo -> error "E_LOWER" "Cooked selection returned empty geometry."
                  | _ -> error "E_DATA_SOURCE" "Cooked selection must return a geometry handle.")
              in
              Result.map
                (fun (output : Sop.Session.output) ->
                  Sop.Node.Build.{ payload = output.payload; instances = output.instances })
                result))
    in
    Sop.Node.Build.with_facts { (Sop.Node.facts node) with reads = [ "*" ]; writes = [ "*" ] } node

  let attribute_node ?node ?state ?elems ?selection ?default ?guard ?(prior = [])
      ?(owner = Rdk.Attribute.Point) ?(kind = "vec3") ?(deletes = []) ~name ~values ~sources inputs
      =
    reuse ?node ?state ?elems
      (Printf.sprintf "attribute %d %s %s %s %s" (Hashtbl.hash owner) kind name
         (String.concat "," (List.map (fun (o, p) -> o ^ ":" ^ p) deletes))
         (String.concat "," (List.map (fun (n, k, _) -> n ^ ":" ^ k) prior)))
      (E.Record
         ([ ("values", values) ]
         @ List.map (fun (_, _, v) -> ("prior", v)) prior
         @ Option.to_list (Option.map (fun v -> ("when", v)) guard)
         @ Option.to_list (Option.map (fun v -> ("selection", v)) selection)
         @ Option.to_list (Option.map (fun v -> ("default", v)) default)))
      inputs
    @@ fun () ->
    attribute_node ~check:strict_body
      ~strict_body:(strict_body || inlined node)
      ~reference ~profile ~approx:checked.approx ~gpu ~kernel_source:(kernel_source_of node) ?state
      ?elems ?selection ?default ?guard ~prior ~owner ~kind ~deletes ~name ~values ~sources inputs

  let group_data = group_data
  and attribute_data = attribute_data

  let group_node ?node ?state ?elems ~args ~sources inputs =
    reuse ?node ?state ?elems "group" (E.Record args) inputs @@ fun () ->
    group_node ~check:strict_body
      ~strict_body:(strict_body || inlined node)
      ~reference ~profile ~approx:checked.approx ~kernel_source:(kernel_source_of node) ?state
      ?elems ~args ~sources inputs

  let plan = if reference then evaluated.plan else fold_writes evaluated.plan

  let external_kernel fn =
    List.find_map
      (fun (candidate, source) -> if same_function candidate fn then Some source else None)
      kernel_bindings

  let image_sites =
    Array.map
      (fun (node : E.node) ->
        if node.kind = "image/map" then
          Option.join (Hashtbl.find_opt image_origins (node.inst, node.site, node.iter))
        else None)
      plan.nodes

  let ids = ref compiled_ids
  and site_table = Hashtbl.create 64
  and site_list = ref (List.rev sites)
  and site_count = ref (List.length sites)

  let () = List.iteri (fun i path -> Hashtbl.replace site_table path i) sites

  let site_index path =
    match Hashtbl.find_opt site_table path with
    | Some i -> i
    | None ->
        let i = !site_count in
        incr site_count;
        Hashtbl.add site_table path i;
        site_list := path :: !site_list;
        i

  let compiled_id (node : E.node) =
    let key = node.inst :: site_index node.site :: List.map (fun k -> -(k + 1)) node.iter in
    match Path_map.find_opt key !ids with
    | Some id -> id
    | None ->
        let id = Sop.Node.Build.fresh_id () in
        ids := Path_map.add key id !ids;
        id

  let compiled = Array.make (Array.length plan.nodes) 0

  let int_arg (n : E.node) k =
    match List.assoc_opt k n.args with
    | Some (E.Int i) -> i
    | _ -> fail "E_LOWER" ("Zone node without " ^ k)

  (* does an argument of a template node read [t] (not only its element)? *)
  let reads_t = E.frame_dependent
  let templ = Array.make (Array.length plan.nodes) false

  let () =
    Array.iter
      (fun (n : E.node) ->
        if is_zone n.kind then
          for id = int_arg n "lo" to int_arg n "hi" - 1 do
            templ.(id) <- true
          done)
      plan.nodes

  (* what a zone reads: its collection and the outside geometry of its template *)
  let zinfos = Hashtbl.create 8

  let rec deps (n : E.node) =
    if is_zone n.kind then
      (match n.args with
      | (_, E.Deferred (Flow.Ty.Named "geometry", src)) :: _ -> src
      | _ -> fail "E_LOWER" "Zone without geometry")
      :: (zinfo n).captures
    else if is_geometry_arrays n.kind then
      let direct = geo_of n.args in
      direct
      @ List.filter
          (fun id -> not (List.mem id direct))
          (Attribute_kernel.sources (geometry_arrays_data n.args))
    else if n.kind = "sop/with_attr" || n.kind = "sop/with_group" then
      let geometry = List.assoc "geometry" n.args in
      let root =
        match geometry with
        | E.Deferred (_, id) -> Some id
        | E.No_geo -> None
        | _ -> fail "E_LOWER" "Attribute write without geometry"
      in
      Option.to_list root
      @ List.filter
          (fun id -> Some id <> root)
          (Attribute_kernel.sources
             (if n.kind = "sop/with_group" then group_data n.args else attribute_data n.args))
    else if n.kind = E.card_kind then cooked_sources (List.assoc "value" n.args)
    else
      let direct = geo_of n.args in
      let rec captures = function
        | E.Fn fn as value -> (
            match external_kernel fn with
            | Some source -> [ source ]
            | None -> Attribute_kernel.sources value)
        | E.List values -> Array.to_list values |> List.concat_map captures
        | _ -> []
      in
      direct
      @ (List.concat_map (fun (_, value) -> captures value) n.args
        |> List.sort_uniq Int.compare
        |> List.filter (fun id -> not (List.mem id direct)))

  and zinfo (n : E.node) =
    match Hashtbl.find_opt zinfos n.id with
    | Some z -> z
    | None ->
        let lo = int_arg n "lo" and hi = int_arg n "hi" and root = int_arg n "body" in
        let seen = Hashtbl.create 16 and caps = ref [] in
        let rec visit j =
          if j >= lo && j < hi then begin
            if not (Hashtbl.mem seen j) then begin
              Hashtbl.add seen j ();
              List.iter visit (deps plan.nodes.(j))
            end
          end
          else if not (List.mem j !caps) then caps := j :: !caps
        in
        visit root;
        let ekey =
          match List.assoc_opt "element" n.args with Some (E.Text k) -> Some k | _ -> None
        in
        let live =
          let rec any id =
            id < hi && (List.exists (fun (_, v) -> reads_t v) plan.nodes.(id).args || any (id + 1))
          in
          any lo
        in
        let stateful =
          let rec any id =
            id < hi
            && (List.exists (fun (_, v) -> E.state_dependent v) plan.nodes.(id).args || any (id + 1))
          in
          any lo
        in
        let z =
          {
            plan_id = n.id;
            lo;
            hi;
            preview = None;
            root;
            live;
            stateful;
            order = Hashtbl.fold (fun id () l -> id :: l) seen [] |> List.sort Int.compare;
            captures = List.rev !caps;
            ekey;
            kind = (if n.kind = "zone/points" then Zone.Points else Zone.Pieces);
            key = (match List.assoc_opt "key" n.args with Some (E.Text k) -> Some k | _ -> None);
            base = 0;
            count = Atomic.make (-1);
            positions = Atomic.make [||];
          }
        in
        Hashtbl.add zinfos n.id z;
        z

  let params_cache = Hashtbl.create 32

  let parameters factory =
    let key = Edit.factory_key factory in
    match Hashtbl.find_opt params_cache key with
    | Some parameters -> parameters
    | None ->
        let parameters = ok (Port.parameters (Edit.factory_fields factory)) in
        Hashtbl.add params_cache key parameters;
        parameters

  (* a helper that names no context operator is its own specialization: shared across instances *)
  let def_context_use =
    let use = lazy (List.map (fun def -> (def, Context_kernel.uses_context def)) checked.defs)
    and lock = Mutex.create () in
    fun () -> Mutex.protect lock (fun () -> Lazy.force use)

  let lisp_factory ?error ?(functions = []) ?provenance (definition : Workspace.node_definition) =
    let declaration_source =
      lazy
        (cached_digest checked [ definition ] (fun () ->
             Context_kernel.declaration_source checked definition))
    in
    let rec function_elements = function
      | E.Fn fn -> [ fn ]
      | E.List values -> Array.to_list values |> List.concat_map function_elements
      | _ -> []
    in
    let functions =
      List.filter_map
        (fun (slot : Flow.Catalog.slot) ->
          let values =
            List.filter_map
              (fun (name, value) -> if name = slot.name then Some value else None)
              functions
            |> List.concat_map function_elements
            |> List.map (fun fn -> E.Fn fn)
          in
          match values with
          | [] -> None
          | [ value ] when not slot.rest -> Some (slot.name, value)
          | _ -> Some (slot.name, E.List (Array.of_list values)))
        definition.slots
    in
    let key =
      String.sub definition.kind.qualified 4 (String.length definition.kind.qualified - 4)
    in
    let record_profile profile = Option.iter (fun record -> record profile) on_profile in
    let lower_nested ~gpu ~kernel_bindings ~factories ~inputs checked =
      Atomic.incr Private.nested_lowerings;
      lower_checked ~reference ~gpu ~strict_body:true ~defer_body_errors:true ~kernel_bindings
        ?on_profile ~factories ~inputs checked
    in
    (* the evaluated error belongs to the call's own parameters (the first build's): a rebuild
       with others evaluates the body *)
    let errored = ref None in
    let build_body bindings ~label ~context inputs parameters =
      let failed =
        match (error, !errored) with
        | Some _, None ->
            errored := Some parameters;
            true
        | Some _, Some original -> original = parameters
        | None, _ -> false
      in
      match error with
      | Some diagnostic when failed ->
          Ok (lisp_body_failed ~checked ~definition ~inputs ~parameters diagnostic)
      | _ ->
          build_lisp_body ~gpu ~lower_nested ~record_profile ~checked ~definition ~provenance ~label
            ~context ~inputs ~parameters ~bindings ~functions ~factories ~def_context_use
    in
    (* A top-level body build is a pure function of the key below, so an edit elsewhere reuses it. Key: declaration,
         label, reference flag, approx, provenance, bindings, context seed/frame/time, input ids, parameters; an
         entry is valid only for the same factories and GPU backend+policy. Bypassed: callable slots, strict nested
         lowerings, kernel bindings, recorded profiles, non-data parameters (key_of raises). *)
    let build bindings ~label ~context inputs parameters =
      let uncached () = build_body bindings ~label ~context inputs parameters in
      if
        functions <> [] || error <> None || strict_body || kernel_bindings <> []
        || on_profile <> None
      then uncached ()
      else
        match
          String.concat "\000"
            [
              Lazy.force declaration_source;
              label;
              string_of_bool reference;
              Marshal.to_string
                ( Workspace.Paths.elements checked.approx,
                  provenance,
                  bindings,
                  Sop.Context.seed context,
                  Sop.Context.frame context,
                  Int64.bits_of_float (Sop.Context.time context),
                  List.map Sop.Node.id inputs )
                [];
              String.concat "\001"
                (List.map
                   (fun (name, value) ->
                     name ^ "=" ^ Flow.Value.key_of ~residual:(fun _ -> raise Exit) value)
                   parameters);
            ]
        with
        | exception Exit -> uncached ()
        | key -> (
            let cached =
              Mutex.protect build_lock (fun () -> Build_cache.find_opt build_cache key)
            in
            match cached with
            | Some (owner, old_gpu, result) when owner == factories && same_gpu old_gpu gpu ->
                Atomic.incr Private.cache_hits;
                result
            | _ ->
                let result = uncached () in
                Mutex.protect build_lock (fun () ->
                    Build_cache.add build_cache key (factories, gpu, result));
                result)
    in
    let category = if definition.category = [] then [ "Custom" ] else definition.category in
    let construct inputs =
      let next = ref 0 in
      let connected =
        List.mapi
          (fun slot input ->
            ( slot,
              Option.map
                (fun _ ->
                  let index = !next in
                  incr next;
                  index)
                input ))
          inputs
      in
      let bindings =
        List.mapi
          (fun slot (s : Flow.Catalog.slot) ->
            ( s.name,
              List.filter_map
                (fun (index, input) ->
                  if index = slot || (s.rest && index >= slot) then input else None)
                connected ))
          definition.slots
      in
      let slots = Array.of_list definition.slots in
      let geometry_inputs =
        List.filter_map Fun.id
          (List.mapi
             (fun position input ->
               Option.map
                 (fun _ ->
                   let slot = slots.(min position (Array.length slots - 1)) in
                   Workspace.slot_ty ~slot definition.kind = Ty.geometry)
                 input)
             inputs)
      in
      Lisp_node.node ~geometry_inputs
        ~unbound_function:
          (List.exists
             (fun (slot : Flow.Catalog.slot) ->
               match slot.ty with
               | Some (Ty.Fn _) ->
                   (not (List.mem_assoc slot.name functions))
                   && (slot.required
                      || Option.value ~default:[] (List.assoc_opt slot.name bindings) <> [])
               | _ -> false)
             definition.slots)
        ~operation:definition.operation ~version:definition.version ~label:key
        ~fields:definition.fields
        ~normalizers:(Encoded_parameters.for_node checked definition)
        ~source:
          (Lazy.force (if functions = [] then declaration_source else kernel_source)
          ^ Option.fold ~none:"" ~some:(Printf.sprintf ";provenance=%d") provenance)
        ~build:(build bindings) (List.filter_map Fun.id inputs)
    in
    if definition.slots = [] then
      Edit.factory ~key ~operation:definition.operation ~label:definition.label ~category
        ~fields:definition.fields ~arity:0 (fun _ -> construct [])
    else
      Edit.factory_slots ~key ~operation:definition.operation ~label:definition.label ~category
        ~slots:(List.map (fun (s : Flow.Catalog.slot) -> s.name) definition.slots)
        ~keyword_inputs:definition.keyword_inputs ~fields:definition.fields
        ~input_types:
          (List.map
             (fun (s : Flow.Catalog.slot) ->
               Ty.to_string (Workspace.slot_ty ~slot:s definition.kind))
             definition.slots)
        ~inputs:
          (List.map
             (fun (s : Flow.Catalog.slot) ->
               match (s.required, s.rest) with
               | true, false -> Edit.Required
               | false, false -> Optional
               | true, true -> Rest
               | false, true -> Optional_rest)
             definition.slots)
        construct

  let lisp_factories = List.map (fun definition -> lisp_factory definition) checked.nodes

  let find_factory key =
    match List.find_opt (fun factory -> Edit.factory_key factory = key) factories with
    | Some factory -> factory
    | None -> (
        match List.find_opt (fun factory -> Edit.factory_key factory = key) lisp_factories with
        | Some factory -> factory
        | None -> fail "E_LOWER" ("Linked catalog has no " ^ key))

  let pending = ref []
  and provenance = ref Network.Int_map.empty
  and tags = ref 0
  and zones = ref []

  let prepared = Hashtbl.create 256

  let curve_points args =
    match List.assoc_opt "points" args with
    | Some (E.List values) ->
        Array.map
          (function
            | E.Vec3 (x, y, z) -> (x, y, z) | _ -> fail "E_LOWER" "sop/curve points must be vec3")
          values
    | _ -> fail "E_LOWER" "sop/curve needs :points"

  (* B.F: a Lisp point deformer whose only input is another such deformer's single-consumer output
       reads that deformer's P values directly; the producer's own cook is skipped. *)
  let consumers =
    lazy
      (let counts = Array.make (Array.length plan.nodes) 0 in
       Array.iter
         (fun (n : E.node) -> List.iter (fun id -> counts.(id) <- counts.(id) + 1) (deps n))
         plan.nodes;
       let rec mark = function
         | E.Deferred (_, id) -> counts.(id) <- counts.(id) + 1
         | E.List values -> Array.iter mark values
         | E.Record fields -> List.iter (fun (_, value) -> mark value) fields
         | _ -> ()
       in
       Array.iter
         (fun (i : E.instance) ->
           mark i.result;
           List.iter (fun (_, value) -> mark value) i.inputs)
         plan.instances;
       counts)

  let lisp_definition kind =
    List.find_opt (fun (d : Workspace.node_definition) -> d.kind.qualified = kind) checked.nodes

  let rec fused_view (n : E.node) =
    match lisp_definition n.kind with
    | Some d
      when (not templ.(n.id))
           && (not (List.mem_assoc E.node_error n.args))
           && Context_kernel.fusable d -> (
        let slot = (List.hd d.slots).name in
        let own = Some (d, n.args) in
        match List.assoc_opt slot n.args with
        | Some (E.Deferred (_, id)) when (Lazy.force consumers).(id) = 1 && not templ.(id) -> (
            let u = plan.nodes.(id) in
            if List.exists (fun (_, v) -> E.is_live v) u.args then own
            else
              match fused_view u with
              | Some (du, uargs) -> (
                  let context_free =
                    List.for_all
                      (fun op -> op = "sop/provenance_base")
                      (Context_kernel.context_ops_reached ~inputs:u.args ~defs:checked.defs du.body)
                  in
                  match if context_free then Context_kernel.fuse d du ~baked:u.args else None with
                  | Some fused ->
                      Some
                        ( fused,
                          List.map
                            (fun (name, v) ->
                              if name = slot then (name, List.assoc slot uargs) else (name, v))
                            n.args )
                  | None -> own)
              | None -> own)
        | _ -> own)
    | _ -> None

  (* the editor's provenance: a merge tags its input i with base + i *)
  let tag_sources cid args =
    let sources = geo_of args in
    let base = !tags in
    tags := base + List.length sources;
    List.iteri
      (fun index source ->
        let s = plan.nodes.(source) in
        provenance :=
          Network.Int_map.add (base + index)
            { merge = cid; input = index; source = compiled.(source); site = s.site; iter = s.iter }
            !provenance)
      sources;
    base

  let lower_default (node : E.node) args cid =
    let kind = node.kind and dynamic = ref [] in
    let node_error =
      match List.assoc_opt E.node_error args with
      | Some (E.Text text) -> Some (Marshal.from_string text 0 : Diagnostic.t)
      | _ -> None
    in
    let args = List.remove_assoc E.node_error args in
    let args =
      if kind = "image/noise" then
        List.map (fun (name, value) -> ((if name = "freq" then "frequency" else name), value)) args
      else args
    in
    let args, fused_definition =
      match fused_view node with
      | Some (d, fused_args) when fused_args != node.args ->
          let slot = (List.hd d.slots).name in
          ( List.map
              (fun (name, v) ->
                if name = slot then (name, List.assoc slot fused_args) else (name, v))
              args,
            Some d )
      | _ -> (args, None)
    in
    let key =
      match String.split_on_char '/' kind with
      | [ "sop"; key ] -> key
      | [ "image"; "noise" ] -> "image_noise"
      | _ -> fail "E_LOWER" ("Cannot lower " ^ kind)
    in
    let factory =
      if key = "image_noise" then Sop.Image_nodes.noise_factory
      else
        match
          List.find_opt
            (fun (d : Workspace.node_definition) -> d.kind.qualified = kind)
            checked.nodes
        with
        | Some definition ->
            (* the editor's provenance: a Lisp sop/merge tags input i with base + i *)
            let tag_base =
              if kind <> "sop/merge" || not provenance_tags then None
              else Some (tag_sources cid args)
            in
            lisp_factory ?error:node_error ~functions:args ?provenance:tag_base
              (Option.value fused_definition ~default:definition)
        | None -> find_factory key
    in
    let names = Edit.factory_slot_names factory and parameters = parameters factory in
    let rest =
      List.find_index
        (function Edit.Rest | Optional_rest -> true | _ -> false)
        (Edit.factory_inputs factory)
    in
    let arity = ref (List.length names) in
    let next_rest = ref (Option.value ~default:0 rest) in
    let slots = ref [] and changes = ref [] and kernels = ref [] in
    let add_function index declared_index name fn =
      match external_kernel fn with
      | Some source -> slots := (index, source) :: !slots
      | None ->
          let signature =
            match Ty.of_string (List.nth (Edit.factory_input_types factory) declared_index) with
            | Some (Ty.Fn (Some signature)) -> signature
            | _ -> fail "E_LOWER" ("Input " ^ name ^ " does not declare a function signature")
          in
          let cid =
            compiled_id
              { node with site = node.site @ [ Printf.sprintf "~kernel:%s:%d" name index ] }
          in
          kernels :=
            {
              index;
              cid;
              signature;
              fn;
              identity = Sop.Node.Build.fresh_id ();
              sources = Attribute_kernel.sources (E.Fn fn);
            }
            :: !kernels
    in
    let rec add_rest name = function
      | E.No_geo -> ()
      | E.Deferred (ty, id) when Ty.is_cooked ty ->
          slots := (!next_rest, id) :: !slots;
          incr next_rest;
          arity := max !arity !next_rest
      | E.Fn fn ->
          add_function !next_rest (Option.get rest) name fn;
          incr next_rest;
          arity := max !arity !next_rest
      | E.List values -> Array.iter (add_rest name) values
      | _ -> fail "E_LOWER" "Repeated inputs need cooked values or functions"
    in
    List.iter
      (fun (name, value) ->
        match (List.find_index (( = ) name) names, value) with
        | Some index, value when Some index = rest -> add_rest name value
        | Some _, E.No_geo -> ()
        | Some index, E.Deferred (ty, id) when Ty.is_cooked ty -> slots := (index, id) :: !slots
        | Some index, E.Fn fn -> add_function index index name fn
        | Some _, _ -> fail "E_LOWER" ("Slot " ^ name ^ " needs geometry")
        | None, value ->
            let parameter = ok (Port.find_parameter parameters name) in
            if E.is_live value then dynamic := (value, changes_of parameter) :: !dynamic
            else changes := List.rev_append (changes_of parameter value) !changes)
      args;
    {
      cid;
      factory;
      arity = !arity;
      dynamic = List.rev !dynamic;
      zone = None;
      slots = List.rev !slots;
      changes = List.rev !changes;
      kernels = List.rev !kernels;
    }

  let natives : (Operators.lowering, E.node -> (string * E.value) list -> int -> prepared) Hashtbl.t
      =
    Hashtbl.create 16

  (* a card's canonical parameters as the values of its schema's fields *)
  let card_values defaults parameters =
    List.map
      (fun (name, default) ->
        ( name,
          match (List.assoc_opt name parameters, default) with
          | Some (E.Bool b), _ -> Param.Bool_value b
          | Some (E.Int n), _ -> Int_value n
          | Some (E.Float f), _ -> Float_value f
          | Some (E.Text t), Param.Choice_value _ -> Choice_value t
          | Some (E.Text t), _ -> Text_value t
          | _, value -> value ))
      defaults

  (* An inlined card's marker: a pass-through of the body's result, or the structural node of a
     body whose geometry is decided by cooked data. *)
  let lower_card (node : E.node) args cid =
    let kind = match List.assoc "kind" args with E.Text kind -> kind | _ -> "" in
    let key =
      match String.index_opt kind '/' with
      | Some i -> String.sub kind (i + 1) (String.length kind - i - 1)
      | None -> kind
    in
    let sources = deps node in
    let build inputs =
      match List.assoc "value" args with
      | E.Residual residual ->
          reuse ~node "structural" (E.Residual residual) inputs (fun () ->
              structural_node ~node residual
                (List.map2 (fun id input -> (id, plan.nodes.(id).ty, input)) sources inputs))
          |> Sop.Node.relabel key
      | _ ->
          Sop.Node.Build.make ~label:key ~operation:"flow.card" ~version:1 ~parameters:kind
            ~cook_mode:(Sop.Node.Passthrough 0) ~dependencies:Sop.Context.Dependencies.static
            ~inputs:(Array.of_list inputs) (fun ~node_id:_ _ payloads ->
              Ok Sop.Node.Build.{ payload = payloads.(0); instances = None })
          |> Sop.Node.Build.with_facts
               Sop.Node.
                 {
                   cook_mode = Passthrough 0;
                   elementwise = None;
                   reads = [];
                   writes = [];
                   topology = Preserved;
                   exact = true;
                 }
    in
    (* the card's fields, for the inspector, handles and display; an edit goes through its text *)
    let schema node =
      match
        ( List.find_opt
            (fun (d : Workspace.node_definition) -> d.kind.qualified = kind)
            checked.nodes,
          List.assoc_opt "parameters" args )
      with
      | Some d, Some (E.Record parameters) ->
          let schema =
            Lisp_node.schema "parameters" d.fields (Encoded_parameters.for_node checked d)
          in
          let values = card_values (Param.default schema) parameters in
          let rec attach node =
            Result.value ~default:node
              (Sop.Node.Build.parameterize_with_key ~schema ~values
                 ~rebuild:(fun ~label:_ ~inputs changed ->
                   (* new inputs keep the card; a new value is an edit of its text *)
                   if changed = values then Ok (attach (build inputs))
                   else Error "An inlined card is edited through its text")
                 node)
          in
          attach node
      | _ -> node
    in
    let build inputs = schema (build inputs) in
    let factory =
      Edit.factory ~key:"flow_card" ~label:key ~category:[ "Flow" ] ~arity:(List.length sources)
        (fun inputs -> Ok (build inputs))
    in
    {
      cid;
      factory;
      arity = List.length sources;
      changes = [];
      dynamic = [];
      zone = None;
      kernels = [];
      slots = List.mapi (fun index source -> (index, source)) sources;
    }

  let native (node : E.node) =
    if node.kind = E.card_kind then Some lower_card
    else if node.kind = "exact" && node.ty <> Ty.image then None
    else Option.bind (Operators.lowering node.kind) (Hashtbl.find_opt natives)

  let rec prepare (node : E.node) =
    match Hashtbl.find_opt prepared node.id with
    | Some p -> p
    | None ->
        let template = templ.(node.id) in
        let cid = compiled.(node.id) in
        (* an inlined merge body's provenance base (its sop/concat), allocated here as its card was *)
        let node =
          if
            List.exists
              (function "tag_base", E.Int n -> n = Operators.provenance_sentinel | _ -> false)
              node.args
          then
            let base = if provenance_tags then tag_sources cid node.args else -1 in
            {
              node with
              args =
                List.map
                  (function
                    | "tag_base", E.Int n when n = Operators.provenance_sentinel ->
                        ("tag_base", E.Int base)
                    | arg -> arg)
                  node.args;
            }
          else node
        in
        let live = List.filter (fun (_, v) -> E.is_live v) node.args in
        let resource = is_resource node in
        (* a template node keeps its live arguments: each element forces them *)
        let args =
          if template || kernel_data node <> None || resource || node.kind = E.card_kind then
            node.args
          else List.map (fun (n, v) -> (n, at_zero v)) node.args
        in
        let args = material_args node.kind args in
        let p =
          match native node with
          | Some lower -> lower node args cid
          | None -> lower_default node args cid
        in
        if (not template) && kernel_data node = None && not resource then begin
          let parameters = parameters p.factory in
          pending :=
            List.rev_append
              (List.filter_map
                 (fun (field, value) ->
                   let field =
                     if node.kind = "image/noise" && field = "freq" then "frequency" else field
                   in
                   if
                     List.exists
                       (fun (parameter : Port.parameter) -> parameter.path = field)
                       parameters
                   then Some { node = cid; field; value }
                   else None)
                 live)
              !pending
        end;
        Hashtbl.add prepared node.id p;
        p

  (* the zone node of a lowered graph: it cooks [z.root] once per element *)
  and kernel_node ?state ?elems (k : kernel_input) inputs =
    Function_kernel.node ?state ~reference ?elems ~identity:k.identity ~signature:k.signature
      ~fn:k.fn ~sources:k.sources inputs
    |> Sop.Node.Build.restore_id k.cid |> edit

  and make_zone ?state ~outer z inputs =
    let stamp = Option.fold ~none:"" ~some:E.state_stamp state in
    let stamp =
      match z.preview with
      | None -> stamp
      | Some choice ->
          stamp ^ ";preview=" ^ Digest.to_hex (Digest.string (Marshal.to_string choice []))
    in
    let select =
      Option.bind z.preview (fun (_, probes) ->
          List.nth_opt probes (List.length plan.nodes.(z.plan_id).iter))
    in
    Zone.node
      ~report:(fun elements ->
        Atomic.set z.count (Array.length elements);
        Atomic.set z.positions (Array.map (fun (e : Zone.element) -> e.position) elements))
      ~live:z.live ~stamp ~kind:z.kind ?key:z.key ?select ~source_attribute ~source_base:z.base
      ~inputs
      ~body:(fun ~inputs ~context ->
        instantiate ?state ~outer ~live:(Sop.Context.input context) z ~inputs)
      ()

  (* Re-root a geometry-loop template at the selected node. Unused bindings
       may have different captures from the authored result, so follow its cone. *)
  and focus z target probes =
    let contains z = target >= z.lo && target < z.hi in
    let inner = ref None in
    for id = z.lo to z.hi - 1 do
      if is_zone plan.nodes.(id).kind then begin
        let candidate = Option.get (prepare plan.nodes.(id)).zone in
        if contains candidate then
          match !inner with
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
          let sources =
            if id = root && is_zone node.kind then
              let inner = focus (Option.get (prepare node).zone) target probes in
              int_geo node :: inner.captures
            else deps node
          in
          List.iter visit sources
        end
      end
      else if not (List.mem id !captures) then captures := id :: !captures
    in
    visit root;
    {
      z with
      root;
      preview = Some (target, probes);
      captures = List.rev !captures;
      order = Hashtbl.fold (fun id () order -> id :: order) seen [] |> List.sort Int.compare;
    }

  and int_geo node =
    match geo_of (List.filter (fun (key, _) -> key = "geometry") node.E.args) with
    | [ id ] -> id
    | _ -> fail "E_LOWER" "Zone without geometry"

  (* one element's copy of the template, over the zone's own inputs *)
  and instantiate ?state ~outer ~live z ~inputs =
    let build ~outer bound id =
      let n = plan.nodes.(id) in
      let p = prepare n in
      let options =
        List.init p.arity (fun i ->
            match List.find_opt (fun (k : kernel_input) -> k.index = i) p.kernels with
            | Some k ->
                Some (kernel_node ?state ~elems:outer k (List.map (Hashtbl.find bound) k.sources))
            | None -> Option.bind (List.assoc_opt i p.slots) (Hashtbl.find_opt bound))
      in
      let node =
        match p.zone with
        | Some inner ->
            let inner =
              match z.preview with
              | Some (target, probes) when target >= inner.lo && target < inner.hi ->
                  focus inner target probes
              | _ -> inner
            in
            let options = int_geo n :: inner.captures |> List.map (Hashtbl.find_opt bound) in
            make_zone ?state ~outer inner (Array.of_list (List.filter_map Fun.id options))
        | None when n.kind = "sop/with_attr" && List.assoc "geometry" n.args <> E.No_geo ->
            let name =
              match List.assoc "attribute" n.args with
              | E.Text name -> name
              | _ -> fail "E_LOWER" "Attribute name must be static text"
            in
            let owner, kind = attribute_options n.args in
            attribute_node ~node:n ?state ~elems:outer
              ?selection:(List.assoc_opt "selection" n.args)
              ?default:(List.assoc_opt "default" n.args) ?guard:(List.assoc_opt "when" n.args)
              ~owner ~kind ~name ~values:(List.assoc "values" n.args) ~sources:(deps n)
              (List.filter_map Fun.id options)
        | None when n.kind = "sop/with_group" ->
            group_node ~node:n ?state ~elems:outer ~args:n.args ~sources:(deps n)
              (List.filter_map Fun.id options)
        | None when is_geometry_arrays n.kind ->
            geometry_arrays_node ~node:n ?state ~elems:outer ~operation:n.kind ~args:n.args
              ~sources:(deps n) (List.filter_map Fun.id options)
        | None ->
            edit
              (Lisp_node.with_initial_changes ~operation:(Edit.factory_operation p.factory)
                 p.changes (fun () -> Edit.instantiate_optional p.factory options))
      in
      let node = edit (Sop.Node.Build.restore_id p.cid node) in
      let changes =
        p.changes
        @ List.concat_map
            (fun (v, changes) ->
              changes
                (match E.force ?state:(Option.map E.fork_state state) ~elems:outer v ~live with
                | Ok v -> v
                | Error d -> failwith (Diagnostic.to_string d)))
            p.dynamic
      in
      let node =
        if changes = [] then node else fst (edit (Sop.Node.apply_parameters node changes))
      in
      Hashtbl.replace bound id node
    in
    (* what does not read the element is built once per cook of the zone *)
    let shared = Hashtbl.create 16 and varies = Hashtbl.create 16 in
    List.iteri (fun k j -> Hashtbl.replace shared j inputs.(k + 1)) z.captures;
    List.iter
      (fun id ->
        let n = plan.nodes.(id) in
        if
          n.kind = "zone/element" || is_zone n.kind || (prepare n).dynamic <> []
          || Option.fold ~none:false ~some:E.is_live (kernel_data n)
          (* ponytail: all function inputs vary in a zone; narrow to actual
              element captures if rebuilding constant functions becomes costly. *)
          || (prepare n).kernels <> []
          || List.exists (Hashtbl.mem varies) (deps n)
        then Hashtbl.replace varies id ()
        else build ~outer:[] shared id)
      z.order;
    fun (element : Zone.element) ->
      let outer =
        match z.ekey with
        | Some key ->
            let x, y, z = element.position in
            (key, E.Vec3 (x, y, z)) :: outer
        | None -> outer
      in
      let bound = Hashtbl.copy shared in
      List.iter
        (fun id ->
          if Hashtbl.mem varies id then
            if plan.nodes.(id).kind = "zone/element" then
              Hashtbl.replace bound id (Zone.element_node element)
            else build ~outer bound id)
        z.order;
      Hashtbl.find bound z.root

  let () =
    let reg key lower = Hashtbl.replace natives key lower in
    reg Geometry_arrays (fun (node : E.node) args cid ->
        let operation = node.kind in
        let sources = deps node in
        let factory =
          Edit.factory ~key:(geometry_arrays_key operation) ~label:(geometry_arrays_label operation)
            ~category:[ "Flow"; "Topology" ] ~arity:(List.length sources) (fun inputs ->
              Ok (geometry_arrays_node ~node ~operation ~args ~sources inputs))
        in
        {
          cid;
          factory;
          arity = List.length sources;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
          slots = List.mapi (fun index source -> (index, source)) sources;
        });
    reg Metadata (fun (node : E.node) args cid ->
        let operation = node.kind in
        let sources = geo_of args in
        let factory = metadata_factory ~operation ~args sources in
        {
          cid;
          factory;
          arity = List.length sources;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
          slots = List.mapi (fun i source -> (i, source)) sources;
        });
    reg Group_write (fun (node : E.node) args cid ->
        let sources = deps node in
        let factory =
          Edit.factory ~key:"flow.with_group" ~label:"Write group" ~category:[ "Flow" ]
            ~arity:(List.length sources)
            (Fun.compose Result.ok (group_node ~node ~args ~sources))
        in
        {
          cid;
          factory;
          arity = List.length sources;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
          slots = List.mapi (fun i source -> (i, source)) sources;
        });
    reg Resource (fun (node : E.node) args cid ->
        let factory =
          Edit.factory ~key:node.kind ~label:node.kind ~category:[ "Image" ] ~arity:0 (fun _ ->
              Result.ok
              @@ resource_image node.kind
                   (Error
                      (Diagnostic.error ~code:"E_IMAGE"
                         "This image needs an initial-domain image resolver.")))
        in
        {
          cid;
          factory;
          slots = [];
          arity = 0;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
        });
    reg Attribute_write (fun (node : E.node) args cid ->
        let deletes =
          match List.assoc_opt fused_deletes args with
          | Some (E.List deletes) ->
              Array.to_list deletes
              |> List.map (function
                | E.Record [ ("owner", E.Text owner); ("pattern", E.Text pattern) ] ->
                    (owner, pattern)
                | _ -> fail "E_LOWER" "A fused write's deletions are owner/pattern records")
          | _ -> []
        in
        let prior = fused_entries args in
        let args =
          List.filter
            (fun (k, _) -> not (List.mem k [ fused_writes; fused_deletes; fused_chain ]))
            args
        in
        let owner, kind = attribute_options args in
        let name =
          match List.assoc "attribute" args with
          | E.Text name -> name
          | _ -> fail "E_LOWER" "Attribute name must be static text"
        in
        let values = List.assoc "values" args in
        let sources =
          match List.assoc "geometry" args with E.No_geo -> -1 :: deps node | _ -> deps node
        in
        let factory =
          Edit.factory ~key:"flow.with_attr" ~label:"Write attribute" ~category:[ "Flow" ]
            ~arity:(List.length sources)
            (Fun.compose Result.ok
               (attribute_node ~node ?selection:(List.assoc_opt "selection" args)
                  ?default:(List.assoc_opt "default" args) ?guard:(List.assoc_opt "when" args)
                  ~prior ~owner ~kind ~deletes ~name ~values ~sources))
        in
        {
          cid;
          factory;
          arity = List.length sources;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
          slots =
            List.filter_map
              (fun (i, source) -> if source < 0 then None else Some (i, source))
              (List.mapi (fun i source -> (i, source)) sources);
        });
    reg Attribute_delete (fun (node : E.node) args cid ->
        let name =
          match List.assoc "attribute" args with
          | E.Text name -> name
          | _ -> fail "E_ATTR_NAME" "An attribute pattern must be text"
        in
        let owner =
          match List.assoc_opt "owner" args with
          | None -> "point"
          | Some (E.Text owner) -> owner
          | Some _ -> fail "E_ATTR_OWNER" "An attribute owner must be text"
        in
        let sources = geo_of args in
        let factory =
          Edit.factory ~key:"flow.delete_attr" ~label:"Delete attribute" ~category:[ "Flow" ]
            ~arity:1 (function
            | [ input ] -> Ok (Attribute_metadata.delete ~owner ~pattern:name input)
            | _ -> Error "Attribute deletion needs one geometry")
        in
        {
          cid;
          factory;
          arity = 1;
          changes = [];
          dynamic = [];
          zone = None;
          kernels = [];
          slots = List.mapi (fun i source -> (i, source)) sources;
        });
    reg Curve (fun (node : E.node) args cid ->
        let dynamic = ref [] in
        let encode points =
          [ (Curve.parameter, Param.Text_value (Curve.encode (curve_points points))) ]
        in
        let closed =
          match List.assoc_opt "closed" args with
          | None -> []
          | Some (E.Bool closed) -> [ (Curve.closed_parameter, Param.Bool_value closed) ]
          | Some _ -> fail "E_LOWER" "sop/curve :closed must be a constant bool"
        in
        let filled =
          match List.assoc_opt "filled" args with
          | None -> []
          | Some (E.Bool filled) -> [ (Curve.filled_parameter, Param.Bool_value filled) ]
          | Some _ -> fail "E_LOWER" "sop/curve :filled must be a constant bool"
        in
        let changes =
          closed @ filled
          @
          match List.assoc_opt "points" args with
          | Some points when E.is_live points ->
              dynamic := [ (points, fun v -> encode [ ("points", v) ]) ];
              []
          | _ -> encode args
        in
        {
          cid;
          factory = Curve.factory;
          arity = 0;
          slots = [];
          changes;
          dynamic = List.rev !dynamic;
          zone = None;
          kernels = [];
        });
    reg Zone (fun (node : E.node) args cid ->
        let z =
          { (zinfo node) with base = !tags; count = Atomic.make (-1); positions = Atomic.make [||] }
        in
        tags := z.base + Zone.max_elements;
        let sources =
          (match geo_of (List.filter (fun (k, _) -> k = "geometry") args) with
          | [ src ] -> src
          | _ -> fail "E_LOWER" "Zone without geometry")
          :: z.captures
        in
        List.iter
          (fun id ->
            let n = plan.nodes.(id) in
            if n.kind <> "zone/element" then ignore (prepare n))
          z.order;
        let base_site = plan.nodes.(z.root).site in
        zones :=
          {
            cid;
            site = node.site;
            iter = node.iter;
            body_site = base_site;
            base = z.base;
            count = z.count;
            ekey = z.ekey;
            positions = z.positions;
          }
          :: !zones;
        let factory =
          Edit.factory_slots ~key:"zone" ~label:"Loop over geometry" ~slots:[ "input" ]
            ~category:[ "Copy" ] ~inputs:[ Edit.Rest ] (fun nodes ->
              Result.ok @@ make_zone ~outer:[] z (Array.of_list (List.filter_map Fun.id nodes)))
        in
        {
          cid;
          factory;
          arity = List.length sources;
          changes = [];
          dynamic = [];
          kernels = [];
          zone = Some z;
          slots = List.mapi (fun i s -> (i, s)) sources;
        });
    ()

  let contexts = List.map (fun (g : Workspace.graph) -> (g.name, g.context)) checked.graphs

  let sop_instance (i : E.instance) =
    Option.fold ~none:false
      ~some:(fun context -> Ty.is_cooked (Flow.Context.result context))
      (List.assoc_opt i.graph contexts)

  (* every plan node gets its id up front, in plan order, so ids do not
       depend on which network asks first *)
  let () =
    Array.iter
      (fun (node : E.node) ->
        if sop_instance plan.instances.(node.inst) && Ty.is_cooked node.ty then
          compiled.(node.id) <- compiled_id node)
      plan.nodes

  (* volatile: live, or fed by a volatile node (plan order: inputs first).  A loop over geometry
       whose body reads [t] is live, and so are the template nodes its elements are copies of (a copy
       keeps the template's id, so the elements share one volatile slot per node) *)
  let volatile_nodes = ref Network.Int_map.empty
  let volatile = Array.make (Array.length plan.nodes) false

  let () =
    Array.iter
      (fun (node : E.node) ->
        if
          compiled.(node.id) <> 0
          && (List.exists (fun j -> volatile.(j)) (deps node)
             || (is_zone node.kind && (zinfo node).live)
             || (not (is_zone node.kind))
                && List.exists
                     (fun (_, v) ->
                       if templ.(node.id) || kernel_data node <> None || node.kind = E.card_kind
                       then reads_t v || E.state_dependent v
                       else
                         E.is_live v
                         || match v with E.Fn _ -> reads_t v || E.state_dependent v | _ -> false)
                     node.args)
        then begin
          volatile.(node.id) <- true;
          volatile_nodes := Network.Int_map.add compiled.(node.id) () !volatile_nodes
        end)
      plan.nodes

  let compiled_context =
    Array.to_list compiled
    |> List.mapi (fun i id -> (i, id))
    |> List.filter (fun (i, id) -> id <> 0 && not templ.(i))
    |> List.fold_left (fun m (i, id) -> Network.Int_map.add i id m) Network.Int_map.empty

  let live_network network graph =
    let network =
      Network.with_states evaluated.states network
      |> Network.with_profile profile |> Network.with_reference reference
      |> Network.with_approx checked.approx
    in
    let frame_nodes =
      Hashtbl.fold
        (fun _ (p : prepared) nodes ->
          match p.zone with
          | Some z when z.stateful && Edit.find graph ~node_id:p.cid <> None ->
              Network.Int_map.add p.cid
                (fun ~network:_ state _live node ->
                  let snapshot = E.fork_state state in
                  Ok
                    (Sop.Node.Build.adopt_identity ~source:node
                       (make_zone ~state:snapshot ~outer:[] z (Sop.Node.Build.input_array node))))
                nodes
          | _ -> nodes)
        prepared Network.Int_map.empty
    in
    let frame_nodes =
      Hashtbl.fold
        (fun _ (p : prepared) nodes ->
          List.fold_left
            (fun nodes (k : kernel_input) ->
              if (not (E.state_dependent (E.Fn k.fn))) || Edit.find graph ~node_id:k.cid = None then
                nodes
              else
                Network.Int_map.add k.cid
                  (fun ~network:_ state _live node ->
                    Ok (kernel_node ~state:(E.fork_state state) k (Sop.Node.inputs node)))
                  nodes)
            nodes p.kernels)
        prepared frame_nodes
    in
    let frame_nodes =
      Array.fold_left
        (fun nodes (n : E.node) ->
          if
            (not (is_geometry_arrays n.kind))
            || Edit.find graph ~node_id:compiled.(n.id) = None
            || not (E.state_dependent (geometry_arrays_data n.args))
          then nodes
          else
            Network.Int_map.add compiled.(n.id)
              (fun ~network:_ state _live node ->
                Ok
                  (Sop.Node.Build.adopt_identity ~source:node
                     (geometry_arrays_node ~state:(E.fork_state state) ~operation:n.kind
                        ~args:n.args ~sources:(deps n) (Sop.Node.inputs node))))
              nodes)
        frame_nodes plan.nodes
    in
    let frame_nodes =
      Array.fold_left
        (fun nodes (n : E.node) ->
          if
            n.kind <> "sop/with_group"
            || Edit.find graph ~node_id:compiled.(n.id) = None
            || not (E.state_dependent (group_data n.args))
          then nodes
          else
            Network.Int_map.add compiled.(n.id)
              (fun ~network:_ state _live node ->
                Ok
                  (Sop.Node.Build.adopt_identity ~source:node
                     (group_node ~state:(E.fork_state state) ~args:n.args ~sources:(deps n)
                        (Sop.Node.inputs node))))
              nodes)
        frame_nodes plan.nodes
    in
    let frame_nodes =
      Array.fold_left
        (fun nodes (n : E.node) ->
          let values = List.assoc_opt "values" n.args in
          if n.kind <> "sop/with_attr" || Edit.find graph ~node_id:compiled.(n.id) = None then nodes
          else
            let values = Option.get values in
            let program =
              ok
                (Flow_ir.Executor.compile ~profile ~approx:checked.approx ~sink:Flow_ir.Sop_input
                   values)
            in
            let eligible =
              Attribute_kernel.sources values = []
              && Flow_ir.Executor.root_is_packed_or_readback program
            in
            if (not eligible) && not (E.state_dependent (attribute_data n.args)) then nodes
            else
              Network.Int_map.add compiled.(n.id)
                (fun ~network:_ state live node ->
                  let name =
                    match List.assoc "attribute" n.args with
                    | E.Text name -> name
                    | _ -> assert false
                  in
                  (* Only input-independent producers can materialize before SOP cooking.
             Attribute-reading cones keep their existing cooked-input CPU path. *)
                  Result.bind
                    (if eligible then
                       Flow_ir.Executor.try_display ~reference ~gpu ~state program ~live
                     else Ok None)
                    (fun selected ->
                      let values =
                        match selected with
                        | Some (Flow_ir.Executor.Cpu value) -> value
                        | None -> values
                        | Some (Flow_ir.Executor.Gpu _) -> assert false
                      in
                      if selected = None && not (E.state_dependent (attribute_data n.args)) then
                        Ok node
                      else
                        Ok
                          (Sop.Node.Build.adopt_identity ~source:node
                             (let owner, kind = attribute_options n.args in
                              attribute_node ~state:(E.fork_state state)
                                ?selection:(List.assoc_opt "selection" n.args)
                                ?default:(List.assoc_opt "default" n.args) ~owner ~kind ~name
                                ~values ~sources:(deps n) (Sop.Node.inputs node)))))
                nodes)
        frame_nodes plan.nodes
    in
    let frame_nodes =
      Array.fold_left
        (fun nodes (n : E.node) ->
          if (not (is_resource n)) || Edit.find graph ~node_id:compiled.(n.id) = None then nodes
          else
            Network.Int_map.add compiled.(n.id)
              (fun ~network state live node ->
                match Domain.DLS.get image_provider with
                | None ->
                    Error
                      (Diagnostic.error ~code:"E_IMAGE"
                         "Image resources need an initial-domain resolver.")
                | Some images ->
                    Result.map
                      (fun image ->
                        if Sop.Node.parameters node = string_of_int (Sop.Image.data_id image) then
                          node
                        else
                          Sop.Node.Build.adopt_identity ~source:node
                            (resource_image n.kind (Ok image)))
                      (images.resolve ~gpu
                         ~context:{ compiled = compiled_context; network }
                         evaluated.plan ~state ~live
                         (E.Deferred (Ty.image, n.id))))
              nodes)
        frame_nodes plan.nodes
    in
    let network = Network.with_frame_nodes frame_nodes network in
    let drives =
      List.fold_left
        (fun drives (p : pending) ->
          if Edit.find graph ~node_id:p.node = None then drives
          else Port.Map.add Port.{ node = p.node; path = p.field } p.value drives)
        Port.Map.empty (List.rev !pending)
    in
    if Port.Map.is_empty drives then network else ok (Network.with_drives drives network)

  let build index (instance : E.instance) =
    let seen = Hashtbl.create 64 in
    let rec reach id =
      if not (Hashtbl.mem seen id) then begin
        Hashtbl.add seen id ();
        List.iter reach (deps plan.nodes.(id))
      end
    in
    Array.iter
      (fun (node : E.node) ->
        if node.inst = index && Ty.is_cooked node.ty && not templ.(node.id) then reach node.id)
      plan.nodes;
    (match instance.result with E.Deferred (ty, j) when Ty.is_cooked ty -> reach j | _ -> ());
    let order = Hashtbl.fold (fun id () l -> id :: l) seen [] |> List.sort Int.compare in
    let graph =
      List.fold_left
        (fun graph id ->
          let p = prepare plan.nodes.(id) in
          let resources =
            List.map
              (fun (k : kernel_input) ->
                let inputs =
                  List.map
                    (fun source ->
                      match Edit.find graph ~node_id:compiled.(source) with
                      | Some node -> node
                      | None -> fail "E_LOWER" "Function capture was not built before its consumer")
                    k.sources
                in
                (k.index, kernel_node k inputs))
              p.kernels
          in
          let graph =
            List.fold_left (fun graph (_, node) -> edit (Edit.add_node node graph)) graph resources
          in
          let options =
            List.init p.arity (fun index ->
                match List.assoc_opt index resources with
                | Some node -> Some node
                | None -> (
                    match List.assoc_opt index p.slots with
                    | Some source -> Edit.find graph ~node_id:compiled.(source)
                    | None -> None))
          in
          let node =
            edit
              (Lisp_node.with_initial_changes ~operation:(Edit.factory_operation p.factory)
                 p.changes (fun () -> Edit.instantiate_optional p.factory options))
          in
          let node = edit (Sop.Node.Build.restore_id p.cid node) in
          let inputs = Array.of_list (List.map (Option.map Sop.Node.id) options) in
          let graph = edit (Edit.add_node ~factory:p.factory ~inputs node graph) in
          if p.changes = [] then graph
          else fst (edit (Edit.apply_parameters graph ~node_id:p.cid p.changes)))
        Edit.empty order
    in
    let graph, root =
      match instance.result with
      | E.Deferred (ty, id) when Ty.is_cooked ty -> Some compiled.(id) |> fun root -> (graph, root)
      | No_geo -> (graph, None)
      | E.Residual residual when (E.Residual.residual_view residual).term.ty = Ty.geometry ->
          let ids = cooked_sources instance.result in
          let captures =
            List.map
              (fun id ->
                ( id,
                  plan.nodes.(id).ty,
                  match Edit.find graph ~node_id:compiled.(id) with
                  | Some node -> node
                  | None -> fail "E_DATA_SOURCE" "A cooked selection capture was not built." ))
              ids
          in
          let node = structural_node residual captures in
          (edit (Edit.add_node node graph), Some (Sop.Node.id node))
      | _ -> fail "E_LOWER" ("Graph " ^ instance.graph ^ " does not return geometry")
    in
    let graph = match root with Some id -> edit (Edit.set_root id graph) | None -> graph in
    {
      name = instance.graph;
      instance = index;
      default = instance.default;
      network = live_network (Network.of_geometry graph) graph;
      root;
    }

  let graphs =
    List.concat
      (List.mapi
         (fun index instance -> if sop_instance instance then [ build index instance ] else [])
         (Array.to_list plan.instances))

  let () =
    Hashtbl.iter
      (fun _ (p : prepared) ->
        List.iter
          (fun (k : kernel_input) ->
            if
              E.frame_dependent (E.Fn k.fn) || E.state_dependent (E.Fn k.fn)
              || List.exists (fun id -> volatile.(id)) k.sources
            then volatile_nodes := Network.Int_map.add k.cid () !volatile_nodes)
          p.kernels)
      prepared

  (* Previewing an unused template must not allocate provenance or zones after
       this immutable lowering has been handed to the document. *)
  let () =
    Array.iter
      (fun (node : E.node) ->
        if compiled.(node.id) <> 0 && node.kind <> "zone/element" && Ty.is_cooked node.ty then
          ignore (prepare node))
      plan.nodes

  let preview ~node:target ~probes network =
    if
      target < 0
      || target >= Array.length compiled
      || compiled.(target) = 0
      || List.compare_lengths probes plan.nodes.(target).iter <> 0
    then None
    else if not templ.(target) then
      if Edit.find network.Network.geometry ~node_id:compiled.(target) = None then None
      else Some (network, compiled.(target))
    else
      let outer =
        Array.find_opt
          (fun (node : E.node) ->
            is_zone node.kind
            && (not templ.(node.id))
            && target >= int_arg node "lo"
            && target < int_arg node "hi"
            && Edit.find network.geometry ~node_id:compiled.(node.id) <> None)
          plan.nodes
      in
      Option.map
        (fun (node : E.node) ->
          let original = Option.get (prepare node).zone in
          let z = focus original target probes in
          let sources = int_geo node :: z.captures in
          let inputs =
            Array.of_list
              (List.map
                 (fun id ->
                   match Edit.find network.geometry ~node_id:compiled.(id) with
                   | Some node -> node
                   | None -> fail "E_LOWER" "Preview capture is not in the owning network")
                 sources)
          in
          let viewed = make_zone ~outer:[] z inputs in
          let root = Sop.Node.id viewed in
          let geometry = edit (Edit.add_node viewed network.geometry) in
          let network = ok (Network.with_geometry geometry network) in
          let frames =
            if not z.stateful then network.frame_nodes
            else
              Network.Int_map.add root
                (fun ~network:_ state _live node ->
                  Ok
                    (Sop.Node.Build.adopt_identity ~source:node
                       (make_zone ~state:(E.fork_state state) ~outer:[] z
                          (Sop.Node.Build.input_array node))))
                network.frame_nodes
          in
          (Network.with_frame_nodes frames network, root))
        outer

  (* A card's factory node (the node menu's and inspector's), over the nodes its call's slots
     name in the graph that holds the card, with the call's parameters and functions. *)
  let card_node ~node:cid =
    match
      Array.find_opt
        (fun (n : E.node) -> n.kind = E.card_kind && compiled.(n.id) = cid)
        evaluated.plan.nodes
    with
    | None -> Error "No card has this compiled id"
    | Some n when List.exists (fun (_, v) -> match v with E.Fn _ -> true | _ -> false) n.args ->
        (* ponytail: the call's functions would be forwarded into a second evaluation whose
           function ids can equal the body's own; narrow [same_function] before allowing it *)
        Error "A card with function arguments has no factory node"
    | Some n -> (
        match
          ( List.assoc_opt "kind" n.args,
            List.assoc_opt "parameters" n.args,
            List.assoc_opt "slots" n.args,
            List.find_opt
              (fun (g : graph) -> Edit.find g.network.geometry ~node_id:cid <> None)
              graphs )
        with
        | Some (E.Text kind), Some (E.Record parameters), Some (E.Record slots), Some graph -> (
            try
              let slot_value = function
                | E.Int id -> E.Deferred (plan.nodes.(id).ty, id)
                | _ -> E.No_geo
              in
              let rest =
                match lisp_definition kind with
                | Some d ->
                    List.filter_map
                      (fun (s : Flow.Catalog.slot) -> if s.rest then Some s.name else None)
                      d.slots
                | None -> []
              in
              let args =
                List.filter_map
                  (fun (name, ids) ->
                    match ids with
                    | E.List [||] -> None
                    | E.List [| id |] when not (List.mem name rest) -> Some (name, slot_value id)
                    | E.List ids -> Some (name, E.List (Array.map slot_value ids))
                    | _ -> None)
                  slots
                @ List.filter (fun (_, v) -> match v with E.Fn _ -> true | _ -> false) n.args
              in
              (* its own site: the call's kernels must not take the ids of the body's *)
              let p =
                lower_default { n with kind; args; site = n.site @ [ "~factory" ] } args cid
              in
              let changes =
                match lisp_definition kind with
                | Some d ->
                    card_values
                      (Param.default
                         (Lisp_node.schema "parameters" d.fields
                            (Encoded_parameters.for_node checked d)))
                      parameters
                | None -> []
              in
              let compile id =
                edit (Edit.compile_node graph.network.geometry ~node_id:compiled.(id))
              in
              let options =
                List.init p.arity (fun i ->
                    match List.find_opt (fun (k : kernel_input) -> k.index = i) p.kernels with
                    | Some k -> Some (kernel_node k (List.map compile k.sources))
                    | None -> Option.map compile (List.assoc_opt i p.slots))
              in
              Ok
                (edit
                   (Sop.Node.Build.restore_id cid
                      (edit
                         (Lisp_node.with_initial_changes
                            ~operation:(Edit.factory_operation p.factory) changes (fun () ->
                              Edit.instantiate_optional p.factory options)))))
            with Diagnostic.Error d -> Error (Diagnostic.to_string d))
        | _ -> Error "The card has no kind, parameters, slots or graph")

  let result =
    {
      gpu;
      graphs;
      factories = lisp_factories;
      card_node;
      compiled_ids = !ids;
      sites = List.rev !site_list;
      compiled = compiled_context;
      pending = List.rev !pending;
      provenance = !provenance;
      zones = List.rev !zones;
      volatile = !volatile_nodes;
      plan = evaluated.plan;
      image_sites;
      states = evaluated.states;
      evaluated;
      approx = checked.approx;
      approx_reasons = checked.approx_reasons;
      profile;
      preview;
    }
end

let rec lower_checked ~factories ?(provenance_tags = true) ?(reference = false)
    ?(gpu = Flow_ir.Gpu.default) ?(strict_body = false) ?(defer_body_errors = false)
    ?(kernel_bindings = []) ?(clock = Unix.gettimeofday) ?on_profile
    ?(compiled_ids = Path_map.empty) ?(sites = []) ?inputs checked =
  Phase_timer.measure "lower" (fun () ->
      try
        let module L = Lowerer (struct
          let factories = factories
          let provenance_tags = provenance_tags
          let reference = reference
          let gpu = gpu
          let strict_body = strict_body
          let defer_body_errors = defer_body_errors
          let kernel_bindings = kernel_bindings
          let clock = clock
          let on_profile = on_profile
          let compiled_ids = compiled_ids
          let sites = sites
          let inputs = inputs
          let checked = checked
          let lower_checked = lower_checked
        end) in
        Ok L.result
      with Diagnostic.Error diagnostic -> Error diagnostic)

let of_checked ?(factories = Sop.Nodes.factories) ?provenance_tags ?reference ?gpu ?clock
    ?on_profile ?compiled_ids ?sites ?inputs checked =
  Result.bind
    (Encoded_parameters.checked checked)
    (lower_checked ~factories ?provenance_tags ?reference ?gpu ?clock ?on_profile ?compiled_ids
       ?sites ?inputs)

let workspace ?(factories = Sop.Nodes.factories) ?extra ?reference ?gpu ?clock ?on_profile
    ?compiled_ids ?sites ?inputs source =
  Result.bind (Flow_catalog.of_factories ~version:Manifest.version ?extra factories) (fun catalog ->
      match Flow_catalog.check_workspace ~ops:Operators.all catalog source with
      | Some checked, _ ->
          of_checked ~factories ?reference ?gpu ?clock ?on_profile ?compiled_ids ?sites ?inputs
            checked
      | None, diagnostics ->
          Error
            (match
               List.find_opt
                 (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity)
                 diagnostics
             with
            | Some d -> d
            | None -> Diagnostic.error ~code:"E_LOWER" "Workspace did not check"))

let source_context (lowered : t) ~node =
  if
    node < 0
    || node >= Array.length lowered.plan.nodes
    || not (Ty.is_geometry lowered.plan.nodes.(node).ty)
  then Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Capture needs a geometry plan node.")
  else
    match Network.Int_map.find_opt node lowered.compiled with
    | None -> Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source was not compiled.")
    | Some id -> (
        match
          List.find_opt
            (fun (graph : graph) ->
              graph.instance = lowered.plan.nodes.(node).inst
              && Option.is_some (Edit.find graph.network.geometry ~node_id:id))
            lowered.graphs
        with
        | None ->
            Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source has no owning network.")
        | Some graph -> Ok { compiled = lowered.compiled; network = graph.network })

let source_cone (context : image_context) ~node =
  match Network.Int_map.find_opt node context.compiled with
  | None -> Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source was not compiled.")
  | Some root when Option.is_none (Edit.find context.network.geometry ~node_id:root) ->
      Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Capture source is outside its owning network.")
  | Some root ->
      let seen = Hashtbl.create 16 in
      let rec visit id =
        if not (Hashtbl.mem seen id) then begin
          Hashtbl.add seen id ();
          Option.iter
            (Array.iter (Option.iter visit))
            (Edit.inputs context.network.geometry ~node_id:id)
        end
      in
      visit root;
      let removed =
        Edit.inspect context.network.geometry
        |> List.filter_map (fun (n : Edit.node_info) ->
            if Hashtbl.mem seen n.id then None else Some n.id)
      in
      Result.map (fun network -> (network, root)) (Network.remove_nodes removed context.network)

let counts (lowered : t) =
  let live =
    Network.Int_map.fold
      (fun _ id n -> if Network.Int_map.mem id lowered.volatile then n + 1 else n)
      lowered.compiled 0
  in
  (live, Network.Int_map.cardinal lowered.compiled - live)

let status lowered ~seconds =
  match counts lowered with
  | 0, _ -> None
  | live, cached ->
      Some (Printf.sprintf "t %d live · %d cached · cook %.1f ms" live cached (seconds *. 1000.))

let origin lowered tag =
  match Network.Int_map.find_opt tag lowered.provenance with
  | Some _ as found -> found
  | None ->
      List.find_map
        (fun (z : zone) ->
          let index = tag - z.base in
          if index >= 0 && index < Zone.max_elements then
            Some
              {
                merge = z.cid;
                input = index;
                source = z.cid;
                site = z.body_site;
                iter = z.iter @ [ index ];
              }
          else None)
        lowered.zones

let tags lowered ~site ~iter =
  let plain =
    Network.Int_map.fold
      (fun tag (o : origin) tags -> if o.site = site && o.iter = iter then tag :: tags else tags)
      lowered.provenance []
  in
  let n = List.length iter in
  List.fold_left
    (fun tags (z : zone) ->
      if z.site = site && z.iter = iter then
        List.init (max 0 (Atomic.get z.count)) (fun i -> z.base + i) @ tags
      else if
        z.body_site = site
        && List.compare_length_with z.iter (n - 1) = 0
        && List.for_all2 ( = ) z.iter (List.take (max 0 (n - 1)) iter)
      then (z.base + List.nth iter (n - 1)) :: tags
      else tags)
    plain lowered.zones

let zone_count lowered site =
  List.find_map
    (fun (z : zone) ->
      if z.site = site && Atomic.get z.count >= 0 then Some (Atomic.get z.count) else None)
    lowered.zones

let zone_element lowered site k =
  List.find_map
    (fun (z : zone) ->
      if z.site = site then
        match z.ekey with
        | Some key ->
            let positions = Atomic.get z.positions in
            if k >= 0 && k < Array.length positions then Some (key, positions.(k)) else None
        | None -> None
      else None)
    lowered.zones

let field_calls ?state ~live ~resolve lowered =
  let state = Option.map E.fork_state state in
  let defaults =
    List.find_map
      (fun factory ->
        if Edit.factory_key factory = "iso_surface" then Some (Edit.factory_fields factory)
        else None)
      lowered.factories
    |> Option.value ~default:[]
  in
  let vector args name =
    match List.assoc_opt name args with
    | Some value -> (
        match E.Host.force_reference ?state ~resolve value ~live with
        | Ok (E.Vec3 (x, y, z)) -> Some (x, y, z)
        | _ -> None)
    | None ->
        let component suffix =
          List.find_map
            (fun (f : Param.field_view) ->
              if f.name = name ^ suffix then
                match f.current with Float_value value -> Some value | _ -> None
              else None)
            defaults
        in
        Option.bind (component "_x") (fun x ->
            Option.bind (component "_y") (fun y -> Option.map (fun z -> (x, y, z)) (component "_z")))
  in
  fun zone outer ->
    let offset = ref 0 in
    Array.to_list lowered.plan.nodes
    |> List.filter_map (fun (node : E.node) ->
        let args =
          if node.kind = "sop/iso_surface" then Some node.args
          else if
            node.kind = E.card_kind
            && List.assoc_opt "kind" node.args = Some (E.Text "sop/iso_surface")
          then
            match List.assoc_opt "parameters" node.args with
            | Some (E.Record parameters) -> Some (node.args @ parameters)
            | _ -> None
          else None
        in
        match args with
        | None -> None
        | Some args -> (
            match List.assoc_opt "field" args with
            | Some (E.Fn fn) when E.Host.function_scope fn = Some (zone, outer) -> (
                match (vector args "resolution", vector args "min", vector args "max") with
                | Some (rx, ry, rz), Some (lx, ly, lz), Some (hx, hy, hz) ->
                    let valid n =
                      Float.is_finite n && n >= 1.
                      && Float.floor n = n
                      && n < float (Sys.max_array_length / 3)
                    in
                    if not (valid rx && valid ry && valid rz) then None
                    else
                      let nx = int_of_float rx + 1
                      and ny = int_of_float ry + 1
                      and nz = int_of_float rz + 1 in
                      let limit = Sys.max_array_length / 3 in
                      if nx > limit / ny || nx * ny > limit / nz then None
                      else
                        let count = nx * ny * nz and start = !offset in
                        if count > max_int - start then None
                        else begin
                          offset := start + count;
                          let at k =
                            if k < 0 || k >= count then
                              Error
                                (Diagnostic.error ~code:"E_LIST_RANGE"
                                   "Probe index is outside the field grid.")
                            else
                              let p =
                                [|
                                  Float.fma (float (k mod nx)) ((hx -. lx) /. rx) lx;
                                  Float.fma (float (k / nx mod ny)) ((hy -. ly) /. ry) ly;
                                  Float.fma (float (k / (nx * ny))) ((hz -. lz) /. rz) lz;
                                |]
                              in
                              Result.bind
                                (E.Host.map_function
                                   ~signature:Flow.Ty.{ params = [ Vec3 ]; result = Float }
                                   fn [ E.Vec3_array p ])
                                (function
                                  | E.Residual residual ->
                                      Result.bind
                                        (E.Host.map_probe ?state ~resolve ~offset:(start + k)
                                           residual ~live)
                                        (fun (_, at) -> at 0)
                                  | _ -> assert false)
                          in
                          Some (start, count, at)
                        end
                | _ -> None)
            | _ -> None))
