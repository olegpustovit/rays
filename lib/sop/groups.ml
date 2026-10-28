(* Group nodes, declared once: the record is the editor schema and the factory. *)

open Rays_math
open Support
















(* the declared default of the transfer node's rules *)
let default_transfer_rules = [
  { Rdk.Group_ops.transfer_owner = Rdk.Group_ops.Group_primitives; transfer_pattern = "*";
    transfer_prefix = "" };
  { Rdk.Group_ops.transfer_owner = Rdk.Group_ops.Group_points; transfer_pattern = "*";
    transfer_prefix = "" };
  { Rdk.Group_ops.transfer_owner = Rdk.Group_ops.Group_edges; transfer_pattern = "*";
    transfer_prefix = "" };
]

module Group_transfer = struct
  let encode_rule (rule : Rdk.Group_ops.transfer_rule) = [
      group_owner_token rule.transfer_owner; rule.transfer_pattern;
      rule.transfer_prefix;
    ]
  let decode_rule = function
    | [owner; pattern; prefix] ->
        Result.map (fun transfer_owner -> { Rdk.Group_ops.transfer_owner;
          transfer_pattern = pattern; transfer_prefix = prefix })
          (group_owner_of_token
            (String.lowercase_ascii (String.trim owner)))
    | row -> Error (Printf.sprintf
        "Group Transfer rule needs owner, pattern, and prefix; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    use_rules : bool [@sop.default false] [@sop.label "Use rules"];
    rules : Rdk.Group_ops.transfer_rule list [@sop.default []]
      [@sop.label "Rules (owner, pattern, prefix)"]
      [@sop.kind rules_parameter];
    conflict : Rdk.Group_ops.copy_conflict
      [@sop.default Rdk.Group_ops.Copy_overwrite] [@sop.label "Conflict"]
      [@sop.kind group_copy_conflict_parameter] ;
    create_empty : bool [@sop.default false]
      [@sop.label "Create empty groups"];
    distance : float [@sop.default 0.001] [@sop.label "Maximum distance"]
      [@sop.min 0.] [@sop.max 10.] [@sop.hard_min 0.];
  } [@@sop.node_key "group_transfer"] [@@sop.node_label "Group Transfer"]
    [@@sop.node_category "Group/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters source target ->
    let label = Some label in
    let rules = if parameters.use_rules then Some parameters.rules else None in
    let conflict = parameters.conflict in
    let create_empty = parameters.create_empty in
    let distance = parameters.distance in
    let rules = Option.map (List.map (fun (rule : Rdk.Group_ops.transfer_rule) ->
      { Rdk.Group_ops.transfer_owner = rule.transfer_owner;
        transfer_pattern = rule.transfer_pattern;
        transfer_prefix = rule.transfer_prefix })) rules in
    Node.Build.make_geometry ?label ~operation:"group_transfer" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.transfer { cancel = Some (Context.cancel_token context); grain = Some (Context.grain context); rules = Option.value ~default:(default_transfer_rules) rules; conflict; create_empty; distance; source = inputs.(0); target = inputs.(1) } with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end













