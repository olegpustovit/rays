type cook_mode =
  | Generator
  | Duplicate_input of int
  | In_place of int
  | Instance_input of int
  | Passthrough of int
  | Generic

type elementwise = Points | Primitives | None
type topology = Preserved | Changed
type facts = {
  cook_mode : cook_mode;
  elementwise : elementwise;
  reads : string list;
  writes : string list;
  topology : topology;
  exact : bool;
}

let conservative cook_mode = {cook_mode; elementwise = None; reads = ["*"];
  writes = ["*"]; topology = Changed; exact = true}

module Private_types = struct
  type input_policy = All | Only of int
  type cooked = {
    payload : Payload.t;
    instances : Rays_math.Mat4.t array option;
  }
  type geometry_cooked = {
    geometry : Rdk.Geometry.t;
    instances : Rays_math.Mat4.t array option;
  }
end

type t = {
  id : int;
  label : string;
  operation : string;
  version : int;
  parameters : string;
  parameter_key : string;
  facts : facts;
  facts_key : string;
  dependencies : Context.Dependencies.t;
  input_policy : Private_types.input_policy;
  inputs : t array;
  cook : node_id:int -> Context.t -> Payload.t array ->
    (Private_types.cooked, Diagnostic.t) result;
  expand : (Context.t -> t array -> Payload.t array -> (t array, Diagnostic.t) result) option;
  parameterization : parameterization option;
}
and parameterization = Parameters : {
  cache_parameters : string;
  schema : 'parameters Param.schema;
  values : 'parameters;
  rebuild : label:string -> inputs:t array -> 'parameters -> t;
} -> parameterization

let next_id = Atomic.make 1
let fresh_id () = Atomic.fetch_and_add next_id 1

let id value = value.id
let label value = value.label
let operation value = value.operation
let version value = value.version
let parameters value = value.parameters
let parameter_key value = value.parameter_key
let facts value = value.facts
let dependencies value = value.dependencies
let inputs value = Array.to_list value.inputs
let trace value = Diagnostic.{ node_id = value.id; label = value.label;
                               operation = value.operation }

let parameter_fields value = match value.parameterization with
  | None -> []
  | Some (Parameters parameterization) ->
      Param.view parameterization.schema parameterization.values

let has_parameters value = Option.is_some value.parameterization

let relabel label value =
  if String.trim label = "" then value else { value with label = String.trim label }

let parameterize_with_key ?parameter_key ~schema ~values ~rebuild value =
  let values = match Param.normalize schema values with
    | Ok values -> values
    | Error message -> invalid_arg ("Node.parameterize: " ^ message)
  in
  let rebuild ~label ~inputs values =
    rebuild ~label ~inputs:(Array.to_list inputs) values
  in
  (* an operator with no hand key of its own reads as the schema's text *)
  let parameters = if value.parameters = "" then Param.cook_text schema values
    else value.parameters in
  { value with parameters; parameter_key = (match parameter_key with
      | Some key -> key | None -> Param.cook_key schema values);
               parameterization = Some (Parameters { cache_parameters = value.parameters; schema; values; rebuild }) }

let parameterize ~schema ~values ~rebuild value =
  parameterize_with_key ~schema ~values ~rebuild value

let apply_parameters value changes = match value.parameterization with
  | None when changes = [] -> Ok (value, Param.no_effects)
  | None -> Error (Printf.sprintf "node %S has no exposed parameters" value.label)
  | Some (Parameters parameterization) ->
      Result.bind (Param.apply_all parameterization.schema parameterization.values changes)
        (fun (values, effects) ->
        try Ok (if not (Param.has_effects effects) then value, effects
        else if effects.cook then
          let rebuilt = parameterization.rebuild ~label:value.label
              ~inputs:(Array.copy value.inputs) values in
          { rebuilt with id = value.id }, effects
        else
          { value with parameterization = Some (Parameters {
              parameterization with values }) }, effects)
        with Invalid_argument message -> Error message)

module Private = struct
  include Private_types

  let parameterize_with_key = parameterize_with_key

  let fresh_id = fresh_id

  let reserve_id id =
    if id < 1 || id = max_int then Error "node id is out of range" else
    let rec reserve () =
      let next = Atomic.get next_id in
      if next <= id && not (Atomic.compare_and_set next_id next (id + 1)) then reserve () in
    reserve ();
    Ok ()

  let restore_id id value = Result.map (fun () -> {value with id}) (reserve_id id)

  let make ?label ~operation ~version ~parameters ~cook_mode ~dependencies
      ?(input_policy = All) ?expand ~inputs cook =
    if version < 0 then invalid_arg "Node.make: version must be non-negative";
    if String.trim operation = "" then invalid_arg "Node.make: empty operation";
    let label = match label with
      | None -> operation
      | Some label when String.trim label = "" -> operation
      | Some label -> label
    in
    let inputs = Array.copy inputs in
    (match input_policy with
     | All -> ()
     | Only index when index >= 0 && index < Array.length inputs -> ()
     | Only _ -> invalid_arg "Node.make: selected input is out of bounds");
    let facts = conservative cook_mode in
    { id = fresh_id (); label; operation; version; parameters;
      parameter_key = ""; facts; facts_key = Marshal.to_string facts [Marshal.No_sharing];
      dependencies; input_policy; inputs; cook; expand; parameterization = None }

  let geometries inputs =
    let values = Array.map Payload.geometry inputs in
    match Array.find_opt Result.is_error values with
    | Some (Error error) -> Error error
    | Some (Ok _) -> assert false
    | None -> Ok (Array.map Result.get_ok values)

  (* Existing SOP kernels stay geometry-only behind one typed variant boundary. *)
  let make_geometry ?label ~operation ~version ~parameters ~cook_mode ~dependencies
      ?input_policy ?expand ~inputs cook =
    let expand = Option.map (fun expand context nodes inputs ->
      Result.bind (geometries inputs) (expand context nodes)) expand in
    make ?label ~operation ~version ~parameters ~cook_mode ~dependencies
      ?input_policy ?expand ~inputs (fun ~node_id context inputs ->
        Result.bind (geometries inputs) (fun inputs ->
          Result.map (fun (cooked : geometry_cooked) ->
            {payload=Payload.Geometry cooked.geometry; instances=cooked.instances})
            (cook ~node_id context inputs)))

  let cache_parameters value = match value.parameterization with
    | None -> value.parameters
    | Some (Parameters parameters) -> parameters.cache_parameters

  let cache_facts value = value.facts_key

  let with_facts facts value =
    if facts.cook_mode <> value.facts.cook_mode then
      invalid_arg "Node.with_facts: cook mode cannot change";
    if List.exists (fun name -> String.trim name = "") (facts.reads @ facts.writes) then
      invalid_arg "Node.with_facts: component names must not be blank";
    {value with facts; facts_key = Marshal.to_string facts [Marshal.No_sharing]}

  let input_policy value = value.input_policy
  let input_array value = Array.copy value.inputs
  let with_inputs value inputs =
    let inputs = Array.copy inputs in
    (match value.input_policy with
     | All -> ()
     | Only index when index >= 0 && index < Array.length inputs -> ()
    | Only _ -> invalid_arg "Node.with_inputs: selected input is out of bounds");
    { value with inputs }
  let rebuild_with_inputs value inputs =
    let inputs = Array.copy inputs in
    match value.parameterization with
    | None -> with_inputs value inputs
    | Some (Parameters parameterization) ->
        let rebuilt = parameterization.rebuild ~label:value.label
            ~inputs parameterization.values in
        { rebuilt with id = value.id }
  let adopt_identity ~source value =
    { value with id = source.id; label = source.label }
  let cook value context inputs = value.cook ~node_id:value.id context inputs
  let expand value = value.expand
end
