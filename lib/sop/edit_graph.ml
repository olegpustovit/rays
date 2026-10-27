module Id_map = Map.Make (Int)
module Id_set = Set.Make (Int)

type input_requirement = Required | Optional | Rest | Optional_rest

type factory = {
  key : string;
  operation : string;
  label : string;
  category : string list;
  fields : Param.field_view list;

  requirements : input_requirement array;
  slots : string array;
  input_types : string list;
  keyword_inputs : string list;
  build : Node.t option list -> Node.t;
  facts : Node.facts Lazy.t;
}

type entry = {
  node : Node.t;
  inputs : int option array;
  factory : factory option;
}

type t = {
  entries : entry Id_map.t;
  order_rev : int list;
  root : int option;
}

type node_info = {
  node : Node.t;
  id : int;
  label : string;
  operation : string;
  version : int;
  parameters : string;
  facts : Node.facts;
  dependencies : Context.Dependencies.t;
  inputs : int option array;
  has_parameters : bool;
}

(* ponytail: a trailing [Rest] slot repeats.  An entry holds any number of inputs
   at least the slot count; the first rest input is required, the others optional,
   and extras are named [name_2], [name_3], ... *)
let repeating = function Rest | Optional_rest -> true | Required | Optional -> false
let last_requirement (f : factory) = f.requirements.(Array.length f.requirements - 1)
let arity_ok (f : factory) n =
  let len = Array.length f.requirements in
  n = len || (len > 0 && repeating (last_requirement f)
    && n >= len - (if last_requirement f = Optional_rest then 1 else 0))
let requirement (f : factory) index =
  let last = Array.length f.requirements - 1 in
  match f.requirements.(min index last) with
  | Rest -> if index = last then Required else Optional
  | Optional_rest -> Optional
  | r -> r
let has_optional (f : factory) = Array.exists (( <> ) Required) f.requirements
let slot_names_of (f : factory) n =
  let len = Array.length f.slots in
  List.init n (fun i -> if i < len then f.slots.(i)
    else f.slots.(len - 1) ^ "_" ^ string_of_int (i - len + 2))

let of_graph graph =
  let infos = Graph.inspect graph in
  let entries = infos |> List.fold_left (fun entries info ->
    let node = Option.get (Graph.find graph ~node_id:info.Graph.id) in
    let inputs = Array.of_list (List.map Option.some info.input_ids) in
    Id_map.add info.id { node; inputs; factory = None } entries) Id_map.empty in
  { entries; order_rev = List.rev_map (fun info -> info.Graph.id) infos;
    root = Some (Node.id graph) }

let empty = { entries = Id_map.empty; order_rev = []; root = None }

let root value = value.root

let set_root node_id value =
  if Id_map.mem node_id value.entries then Ok { value with root = Some node_id }
  else Error (Printf.sprintf "editable graph has no node #%d" node_id)

let info (entry : entry) : node_info =
  let node = entry.node in
  { node; id = Node.id node; label = Node.label node;
    operation = Node.operation node; version = Node.version node;
    parameters = Node.parameters node; facts = Node.facts node;
    dependencies = Node.dependencies node; inputs = Array.copy entry.inputs;
    has_parameters = Node.has_parameters node }

let inspect value = List.filter_map (fun id ->
  Option.map info (Id_map.find_opt id value.entries)) (List.rev value.order_rev)

let find value ~node_id = Option.map (fun (entry : entry) -> entry.node)
    (Id_map.find_opt node_id value.entries)

let node_factory_key value ~node_id =
  Option.bind (Id_map.find_opt node_id value.entries)
    (fun (entry : entry) -> Option.map (fun factory -> factory.key) entry.factory)

let node_factory_fields value ~node_id =
  Option.fold ~none:[] ~some:(fun factory -> factory.fields)
    (Option.bind (Id_map.find_opt node_id value.entries)
      (fun (entry : entry) -> entry.factory))

let inputs value ~node_id = Option.map (fun (entry : entry) ->
    Array.copy entry.inputs)
    (Id_map.find_opt node_id value.entries)

let node_slot_names value ~node_id = Option.map (fun (entry : entry) ->
    match entry.factory with
    | Some factory -> slot_names_of factory (Array.length entry.inputs)
    | None -> List.init (Array.length entry.inputs) (fun index -> "in" ^ string_of_int index))
    (Id_map.find_opt node_id value.entries)

let empty_geometry = lazy (
  let geometry = Result.get_ok (Rdk.Geometry.create
    ~positions:(Rdk.Packed.Float3.Builder.freeze (Rdk.Packed.Float3.Builder.create 0))
    ~topology:(Rdk.Topology.empty ~point_count:0) ()) in
  Node.Private.make_geometry ~operation:"snapshot" ~version:1
    ~parameters:(Printf.sprintf "data_id=%d;bytes=%d" (Rdk.Geometry.data_id geometry)
      (Rdk.Geometry.payload_bytes geometry))
    ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _context _inputs ->
      Ok Node.Private.{ geometry; instances = None }))

(* One compiled node per document entry. [inputs] are the compiled input
   nodes it was built from, so a later compile can reuse [built] when both the
   entry and every input are physically unchanged. *)
type compiled_entry = {
  source : entry;
  compiled_inputs : Node.t option array;
  built : (Graph.t, string) result;
}

type compiled = compiled_entry Id_map.t

let same_inputs left right =
  Array.length left = Array.length right
  && Array.for_all2 (fun a b -> match a, b with
    | Some a, Some b -> a == b | None, None -> true | _ -> false) left right

(* The node already carries exactly these inputs: no rebuild needed. *)
let wired_to (node : Node.t) inputs =
  let rec go index = function
    | [] -> index = Array.length inputs
    | input :: rest -> index < Array.length inputs
        && (match inputs.(index) with Some compiled -> compiled == input | None -> false)
        && go (index + 1) rest in
  go 0 (Node.inputs node)

let rebuild (entry : entry) compiled_inputs =
  match entry.factory with
  | _ when Array.for_all Option.is_some entry.inputs
      && wired_to entry.node compiled_inputs -> Ok entry.node
  | Some factory when has_optional factory ->
      let node = factory.build (Array.to_list compiled_inputs) in
      let changes = Node.parameter_fields entry.node
          |> List.map (fun field -> field.Param.name, field.current) in
      Result.map (fun (node, _) -> Node.Private.adopt_identity ~source:entry.node node)
        (Node.apply_parameters node changes)
  | _ -> Ok (Node.Private.rebuild_with_inputs entry.node
               (Array.map Option.get compiled_inputs))

(* Compile [id] and its inputs into [table], reusing [previous] entries whose
   source entry and compiled inputs are physically unchanged. *)
let compile_into value ~previous table =
  let visiting = Hashtbl.create 16 and cycle_seen = ref false in
  let rec build id =
    match Hashtbl.find_opt table id with
    | Some compiled -> compiled.built
    | None when Hashtbl.mem visiting id ->
        cycle_seen := true;
        Error (Printf.sprintf "editable graph contains a cycle through node #%d" id)
    | None ->
        match Id_map.find_opt id value.entries with
        | None -> Error (Printf.sprintf "editable graph references missing node #%d" id)
        | Some (entry : entry) ->
            Hashtbl.add visiting id ();
            let count = Array.length entry.inputs in
            let compiled_inputs = Array.make count None in
            let rec build_inputs index =
              if index = count then Ok ()
              else match entry.inputs.(index) with
                | None ->
                    let optional = match entry.factory with
                      | Some factory -> requirement factory index = Optional
                      | None -> false in
                    if optional then build_inputs (index + 1)
                    else Error (Printf.sprintf "node %S input %d is disconnected"
                      (Node.label entry.node) index)
                | Some input_id ->
                    Result.bind (build input_id) (fun input ->
                      compiled_inputs.(index) <- Some input;
                      build_inputs (index + 1)) in
            let built = Result.bind (build_inputs 0) (fun () ->
              match Id_map.find_opt id previous with
              | Some reused when reused.source == entry
                    && same_inputs reused.compiled_inputs compiled_inputs ->
                  reused.built
              | _ -> rebuild entry compiled_inputs) in
            Hashtbl.remove visiting id;
            (* A cycle error names where the walk entered the cycle, so once one
               is seen only successes are memoized. *)
            if Result.is_ok built || not !cycle_seen then
              Hashtbl.replace table id { source = entry; compiled_inputs; built };
            built in
  build

let compile_all ?(previous = Id_map.empty) value =
  let table = Hashtbl.create (Id_map.cardinal value.entries) in
  let build = compile_into value ~previous table in
  Id_map.iter (fun id (source : entry) ->
    let built = build id in
    if not (Hashtbl.mem table id) then
      Hashtbl.replace table id { source; compiled_inputs = [||]; built })
    value.entries;
  Hashtbl.fold Id_map.add table Id_map.empty

let compiled_node compiled ~node_id = match Id_map.find_opt node_id compiled with
  | Some compiled -> compiled.built
  | None -> Error (Printf.sprintf "editable graph has no node #%d" node_id)

let compile_node value ~node_id =
  if not (Id_map.mem node_id value.entries) then
    Error (Printf.sprintf "editable graph has no node #%d" node_id)
  else compile_into value ~previous:Id_map.empty
      (Hashtbl.create (Id_map.cardinal value.entries)) node_id

let compile value = match value.root with
  | None -> Error "editable graph has no output node"
  | Some node_id -> compile_node value ~node_id

let replace_node node value =
  let id = Node.id node in
  match Id_map.find_opt id value.entries with
  | None -> Error (Printf.sprintf "editable graph has no node #%d" id)
  | Some entry when entry.node == node -> Ok value
  | Some (entry : entry) ->
      let arity = List.length (Node.inputs node) in
      if entry.factory = None && arity <> Array.length entry.inputs then Error (Printf.sprintf
          "replacement node %S changed input arity from %d to %d"
          (Node.label node) (Array.length entry.inputs) arity)
      else Ok { value with entries = Id_map.add id { entry with node } value.entries }

let apply_parameters value ~node_id changes =
  match find value ~node_id with
  | None -> Error (Printf.sprintf "editable graph has no node #%d" node_id)
  | Some node -> Result.bind (Node.apply_parameters node changes)
      (fun (node, effects) -> Result.map (fun value -> value, effects)
        (replace_node node value))

let add_node ?inputs ?factory node value =
  let id = Node.id node in
  if Id_map.mem id value.entries then Error (Printf.sprintf
      "editable graph already contains node #%d" id)
  else
    let arity = match factory with
      | None -> List.length (Node.inputs node)
      | Some factory -> Array.length factory.requirements in
    let inputs = match inputs with
      | Some inputs -> Array.copy inputs
      | None -> Node.inputs node |> List.map (fun input ->
          let id = Node.id input in
          if Id_map.mem id value.entries then Some id else None)
          |> Array.of_list in
    if not (match factory with
        | None -> Array.length inputs = arity
        | Some factory -> arity_ok factory (Array.length inputs))
    then Error (Printf.sprintf
        "new node %S expects %d inputs, received %d slots"
        (Node.label node) arity (Array.length inputs))
    else
      let missing = Array.find_opt (function
        | Some source -> not (Id_map.mem source value.entries)
        | None -> false) inputs in
      match missing with
      | Some (Some source) -> Error (Printf.sprintf
          "new node %S references missing node #%d" (Node.label node) source)
      | Some None -> assert false
      | None -> Ok { value with
          entries = Id_map.add id { node; inputs; factory } value.entries;
          order_rev = id :: value.order_rev }

let remove_nodes ids value =
  let removed = List.fold_left (fun set id -> Id_set.add id set)
      Id_set.empty ids in
  let entries = Id_map.filter_map (fun id (entry : entry) ->
    if Id_set.mem id removed then None
    else
      let inputs = Array.map (function
        | Some input when Id_set.mem input removed -> None
        | input -> input) entry.inputs in
      Some { entry with inputs }) value.entries in
  { entries;
    order_rev = List.filter (fun id -> not (Id_set.mem id removed)) value.order_rev;
    root = Option.bind value.root (fun id ->
      if Id_set.mem id removed then None else Some id) }

let depends_on value ~node_id ~candidate =
  let seen = Hashtbl.create 16 in
  let rec visit id =
    id = candidate || if Hashtbl.mem seen id then false else begin
      Hashtbl.add seen id ();
      match Id_map.find_opt id value.entries with
      | None -> false
      | Some (entry : entry) -> Array.exists (function
          | None -> false | Some input -> visit input) entry.inputs
    end
  in
  visit node_id

let rebuild_if_connected entries (entry : entry) inputs =
  match entry.factory with
  | Some factory when has_optional factory ->
      (* Optional slots rebuild through the factory, which knows presence. *)
      let nodes = Array.map (fun input -> Option.bind input (fun id ->
          Option.map (fun (source : entry) -> source.node) (Id_map.find_opt id entries)))
          inputs in
      (match rebuild entry nodes with Ok node -> node | Error _ -> entry.node)
  | _ ->
  let nodes = Array.make (Array.length inputs) entry.node in
  let complete = ref true in
  Array.iteri (fun index -> function
    | None -> complete := false
    | Some id ->
        (match Id_map.find_opt id entries with
         | None -> complete := false
         | Some (source : entry) -> nodes.(index) <- source.node)) inputs;
  if !complete then Node.Private.rebuild_with_inputs entry.node nodes
  else entry.node

let connect ~source ~consumer ~input_index value =
  if not (Id_map.mem source value.entries) then Error (Printf.sprintf
      "editable graph has no source node #%d" source)
  else match Id_map.find_opt consumer value.entries with
    | None -> Error (Printf.sprintf "editable graph has no consumer node #%d" consumer)
    | Some (entry : entry) when input_index < 0
        || input_index > Array.length entry.inputs
        || input_index = Array.length entry.inputs
           && not (match entry.factory with
               | Some f -> Array.length f.requirements > 0 && repeating (last_requirement f) | None -> false) ->
        Error (Printf.sprintf "node %S has no input %d"
          (Node.label entry.node) input_index)
    | Some _ when depends_on value ~node_id:source ~candidate:consumer ->
        Error "connection would create a procedural cycle"
    | Some (entry : entry) ->
        (* a rest slot grows by connecting one past its last input *)
        let inputs = if input_index = Array.length entry.inputs
          then Array.append entry.inputs [| None |] else Array.copy entry.inputs in
        inputs.(input_index) <- Some source;
        let node = rebuild_if_connected value.entries entry inputs in
        Ok { value with entries = Id_map.add consumer { entry with node; inputs }
             value.entries }

let disconnect ~consumer ~input_index value =
  match Id_map.find_opt consumer value.entries with
  | None -> Error (Printf.sprintf "editable graph has no consumer node #%d" consumer)
  | Some (entry : entry) when input_index < 0
      || input_index >= Array.length entry.inputs ->
      Error (Printf.sprintf "node %S has no input %d"
        (Node.label entry.node) input_index)
  | Some (entry : entry) ->
      let inputs = Array.copy entry.inputs in
      inputs.(input_index) <- None;
      Ok { value with entries = Id_map.add consumer { entry with inputs }
           value.entries }

let slot_names arity = function
  | None -> Array.init arity (fun index -> "in" ^ string_of_int index)
  | Some names ->
      if List.length names <> arity
          || List.exists (fun name -> String.trim name = "") names
          || List.length (List.sort_uniq String.compare names) <> arity then
        invalid_arg "Edit_graph factory slots must have one distinct name per input";
      Array.of_list names

let slot_types arity = function
  | None -> List.init arity (fun _ -> "geometry")
  | Some names ->
      let valid name = name <> "" && not (String.exists
        (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false) name) in
      if List.length names <> arity || not (List.for_all valid names) then
        invalid_arg "Edit_graph factory input_types must have one valid type name per input";
      names

let keyword_slots slots requirements fields names =
  if List.length (List.sort_uniq String.compare names) <> List.length names
      || List.exists (fun name ->
        match Array.find_index (( = ) name) slots with
        | None -> true
        | Some index -> requirements.(index) <> Required
            || List.exists (fun (f : Param.field_view) ->
                f.name = name || Option.fold ~none:false ~some:(fun (group, _) -> group = name) f.vec3) fields) names then
    invalid_arg "Edit_graph factory keyword_inputs must name distinct required slots without field conflicts";
  names

let share_default_values fields =
  List.map (fun (field : Param.field_view) ->
    let equal = match field.default, field.current with
      | Param.Float_value left, Param.Float_value right ->
          Int64.bits_of_float left = Int64.bits_of_float right
      | _ -> field.default = field.current in
    if field.default == field.current || not equal then field
    else { field with current = field.default }) fields

let default_facts requirements build = lazy (
  let inputs = Array.to_list (Array.map (function
    | Required | Rest -> Some (Lazy.force empty_geometry)
    | Optional | Optional_rest -> None) requirements) in
  Node.facts (build inputs))

let factory ?operation ?slots ?input_types ?(keyword_inputs = []) ?(fields = [])
    ~key ~label ~category ~arity build =
  let operation = Option.value ~default:key operation in
  if String.trim key = "" || String.trim operation = ""
      || String.trim label = ""
      || category = []
      || List.exists (fun item -> String.trim item = "") category then
    invalid_arg "Edit_graph.factory names must not be blank";
  if arity < 0 then invalid_arg "Edit_graph.factory arity must be non-negative";
  let requirements = Array.make arity Required in
  let slots = slot_names arity slots in
  let build inputs = build (List.map Option.get inputs) in
  { key; operation; label; category; fields = share_default_values fields;
    requirements;
    slots;
    input_types = slot_types arity input_types;
    keyword_inputs = keyword_slots slots requirements fields keyword_inputs;
    build; facts = default_facts requirements build }

let factory_slots ?operation ?slots ?input_types ?(keyword_inputs = []) ?(fields = [])
    ~key ~label ~category ~inputs build =
  let operation = Option.value ~default:key operation in
  if String.trim key = "" || String.trim operation = ""
      || String.trim label = ""
      || category = []
      || List.exists (fun item -> String.trim item = "") category then
    invalid_arg "Edit_graph.factory_slots names must not be blank";
  if inputs = [] then invalid_arg
      "Edit_graph.factory_slots requires at least one input slot";
  if List.exists repeating (List.take (List.length inputs - 1) inputs)
  then invalid_arg "Edit_graph.factory_slots: only the last input may be Rest";
  let requirements = Array.of_list inputs in
  let slots = slot_names (List.length inputs) slots in
  { key; operation; label; category; fields = share_default_values fields;
    requirements;
    slots;
    input_types = slot_types (List.length inputs) input_types; build;
    keyword_inputs = keyword_slots slots requirements fields keyword_inputs;
    facts = default_facts requirements build }

let factory_key (value : factory) = value.key
let factory_operation (value : factory) = value.operation
let factory_label (value : factory) = value.label
let factory_category (value : factory) = value.category
let factory_fields (value : factory) = value.fields
let factory_facts (value : factory) = Lazy.force value.facts
let factory_arity (value : factory) = Array.length value.requirements
let factory_inputs (value : factory) = Array.to_list value.requirements
let factory_slot_names (value : factory) = Array.to_list value.slots
let factory_input_types (value : factory) = value.input_types
let factory_keyword_inputs (value : factory) = value.keyword_inputs

let instantiate (value : factory) inputs =
  let arity = Array.length value.requirements in
  if not (arity_ok value (List.length inputs)) then Error (Printf.sprintf
      "%s expects %d input%s" value.label arity
      (if arity = 1 then "" else "s"))
  else try Ok (value.build (List.map Option.some inputs)) with
    | Invalid_argument message -> Error message
    | Failure message -> Error message

let disconnected_placeholder () =
  Node.Private.make_geometry ~label:"disconnected input"
    ~operation:"disconnected_input" ~version:1 ~parameters:""
    ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
    ~inputs:[||] (fun ~node_id:_ _ _ -> Error (Diagnostic.error
      ~code:"disconnected_input" "node input is not connected"))

let instantiate_optional (value : factory) inputs =
  let arity = Array.length value.requirements in
  if not (arity_ok value (List.length inputs)) then Error (Printf.sprintf
      "%s expects %d input slot%s" value.label arity
      (if arity = 1 then "" else "s"))
  else
    let placeholder = lazy (disconnected_placeholder ()) in
    let inputs = List.mapi (fun index input -> match requirement value index, input with
      | _, Some node -> Some node
      | (Optional | Rest | Optional_rest), None -> None
      | Required, None -> Some (Lazy.force placeholder)) inputs in
    try Ok (value.build inputs) with
    | Invalid_argument message -> Error message
    | Failure message -> Error message
