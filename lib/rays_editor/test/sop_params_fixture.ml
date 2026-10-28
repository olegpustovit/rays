type t = { value : int [@sop.default 3] [@sop.min 1] [@sop.max 8] [@sop.hard_min 1] }
[@@deriving sop_params]

open Sop

module Vector_fixture = struct
  type parameters = {
    center_x : float;
        [@sop.default 0.] [@sop.min -1.] [@sop.max 1.] [@sop.primary] [@sop.vec3 "center"]
    center_y : float; [@sop.default 0.] [@sop.min -1.] [@sop.max 1.] [@sop.vec3 "center"]
    center_z : float; [@sop.default 0.] [@sop.min -1.] [@sop.max 1.] [@sop.vec3 "center"]
  }
  [@@deriving sop_params, sop_node]
  [@@sop.node_key "vector_fixture"]
  [@@sop.node_operation "match_size"]
  [@@sop.node_label "Vector Fixture"]
  [@@sop.node_category "Test"]
  [@@sop.node_inputs 2]
  [@@sop.node_optional "1"]
  [@@sop.node_slots "input, target"]

  let build =
    parameters_build (fun ~label:_ _parameters input target ->
        let with_ =
          ("input", input) :: Option.to_list (Option.map (fun t -> ("target", t)) target)
        in
        Lisp_sop.node ~with_
          (if target = None then "(sop/match_size (sop/ext_input))"
           else "(sop/match_size (sop/ext_input) (sop/ext_target))"))

  let factory = parameters_factory build
end

module Editor = struct
  let factories = [ Vector_fixture.factory ]
end

module Optional_rest_fixture = struct
  type parameters = { tag : string [@sop.default ""] [@sop.label "Tag"] }
  [@@deriving sop_params, sop_node]
  [@@sop.node_key "optional_rest_fixture"]
  [@@sop.node_operation "merge"]
  [@@sop.node_label "Optional Rest Fixture"]
  [@@sop.node_category "Test"]
  [@@sop.node_inputs 3]
  [@@sop.node_optional "1,2"]
  [@@sop.node_rest 2]
  [@@sop.node_slots "base, fixed, extras"]

  let build =
    parameters_build (fun ~label:_ parameters base fixed extras ->
        let with_ =
          List.mapi
            (fun i n -> (Printf.sprintf "i%d" i, n))
            ((base :: Option.to_list fixed) @ extras)
        in
        Lisp_sop.node ~with_
          (Printf.sprintf "(sop/merge :source_attribute %S %s)" parameters.tag
             (String.concat " " (List.map (fun (k, _) -> "(sop/ext_" ^ k ^ ")") with_))))

  let factory = parameters_factory build
end

module Dynamic_schema_fixture = struct
  type parameters = {
    input : int;
        [@sop.default 0] [@sop.label "Source"] [@sop.min 0] [@sop.max 9999] [@sop.hard_min 0]
  }
  [@@deriving sop_params, sop_node]
  [@@sop.node_key "dynamic_schema_fixture"]
  [@@sop.node_operation "switch"]
  [@@sop.node_label "Dynamic Schema Fixture"]
  [@@sop.node_category "Test"]
  [@@sop.node_inputs 1]
  [@@sop.node_rest 0]
  [@@sop.node_slots "inputs"]

  let schema _parameters node =
    let labels =
      List.mapi
        (fun index node -> Printf.sprintf "%d · %s" index (Node.label node))
        (Node.inputs node)
    in
    Param.schema ~name:"parameters" ~default:parameters_default
      (List.map
         (fun (Param.Field field as original) ->
           match field.kind with
           | Param.Integer _ -> Param.Field { field with kind = Param.index_choice labels }
           | _ -> original)
         (Param.fields parameters_schema))

  let build =
    parameters_build ~schema (fun ~label:_ parameters inputs ->
        match inputs with
        | _ :: _ :: _ ->
            let with_ = List.mapi (fun i n -> (Printf.sprintf "i%d" i, n)) inputs in
            Lisp_sop.node ~with_
              (Printf.sprintf "(sop/switch :input %d %s)" parameters.input
                 (String.concat " " (List.map (fun (k, _) -> "(sop/ext_" ^ k ^ ")") with_)))
        | _ -> invalid_arg "dynamic schema fixture needs two inputs")

  let factory = parameters_factory build
end
