(* Group nodes, declared once: the record is the editor schema and the factory. *)

open Rays_math
open Support

(* ocamldep must see the PPX's [Sop.X] resolve inside this library. *)
module Sop = Support.Sop

module Group_non_planar = struct
  type parameters = {
    name : string [@sop.default "nonplanar"] [@sop.label "Group name"];
    tolerance : float [@sop.default 1e-6] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.1] [@sop.hard_min 0.];
    base : string [@sop.default ""] [@sop.label "Base group"]
      [@sop.folder "Combine"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.folder "Combine"] [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_non_planar"] [@@sop.node_label "Group Non-Planar"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let base = optional_text parameters.base in
    let merge = parameters.merge in
    let tolerance = parameters.tolerance in
    let name = parameters.name in
    Node.Private.make_geometry ~label ~operation:"group_non_planar" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_non_planar ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?base ~merge ~tolerance ~name inputs.(0)
        with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_backface = struct
  type parameters = {
    name : string [@sop.default "backface"] [@sop.label "Group name"];
    viewpoint_x : float [@sop.default 0.] [@sop.label "Viewpoint X"]
      [@sop.folder "Viewpoint"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "viewpoint"]
    viewpoint_y : float [@sop.default 0.] [@sop.label "Viewpoint Y"]
      [@sop.folder "Viewpoint"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "viewpoint"]
    viewpoint_z : float [@sop.default 10.] [@sop.label "Viewpoint Z"]
      [@sop.folder "Viewpoint"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "viewpoint"]
    base : string [@sop.default ""] [@sop.label "Base group"]
      [@sop.folder "Combine"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.folder "Combine"] [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_backface"] [@@sop.node_label "Group Backfaces"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let base = optional_text parameters.base in
    let merge = parameters.merge in
    let viewpoint = Rays_math.Vec3.create parameters.viewpoint_x parameters.viewpoint_y
            parameters.viewpoint_z in
    let name = parameters.name in
    let viewpoint = vec3_copy viewpoint in
    Node.Private.make_geometry ~label ~operation:"group_backface" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_backface ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?base ~merge ~viewpoint ~name inputs.(0)
        with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_unshared = struct
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_edges]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "unshared"] [@sop.label "Group name"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_unshared"] [@@sop.node_label "Group Unshared"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let merge = parameters.merge in
    let owner = parameters.owner in
    let name = parameters.name in
    Node.Private.make_geometry ~label ~operation:"group_unshared" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_unshared ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~merge ~owner ~name inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_edges = struct
  let incidence_parameter = Param.choice ~equal:( = ) [
      "Any", Rdk.Group_mesh.Any_edge; "Boundary", Rdk.Group_mesh.Boundary_edge;
      "Manifold", Rdk.Group_mesh.Manifold_edge;
      "Non-manifold", Rdk.Group_mesh.Non_manifold_edge;
    ]
  let angle_basis_parameter = Param.choice ~equal:( = ) [
      "Primitive dihedral", Rdk.Group_mesh.Primitive_dihedral;
      "Incident edges", Rdk.Group_mesh.Incident_edges;
    ]
  type parameters = {
    name : string [@sop.default "edges"] [@sop.label "Group name"];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    incidence : Rdk.Group_mesh.incidence [@sop.default Rdk.Group_mesh.Any_edge]
      [@sop.label "Incidence"] [@sop.kind incidence_parameter];
    use_min_length : bool [@sop.default false] [@sop.label "Minimum length"]
      [@sop.folder "Length"];
    min_length : float [@sop.default 0.] [@sop.label "Minimum"]
      [@sop.folder "Length"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    use_max_length : bool [@sop.default false] [@sop.label "Maximum length"]
      [@sop.folder "Length"];
    max_length : float [@sop.default 1.] [@sop.label "Maximum"]
      [@sop.folder "Length"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    angle_basis : Rdk.Group_mesh.angle_basis
      [@sop.default Rdk.Group_mesh.Primitive_dihedral]
      [@sop.label "Angle basis"] [@sop.folder "Angle"]
      [@sop.kind angle_basis_parameter];
    use_min_angle : bool [@sop.default false] [@sop.label "Minimum angle"]
      [@sop.folder "Angle"];
    min_angle : float [@sop.default 0.] [@sop.label "Minimum"]
      [@sop.folder "Angle"] [@sop.min 0.] [@sop.max 3.141592653589793];
    use_max_angle : bool [@sop.default false] [@sop.label "Maximum angle"]
      [@sop.folder "Angle"];
    max_angle : float [@sop.default 3.141592653589793]
      [@sop.label "Maximum"] [@sop.folder "Angle"] [@sop.min 0.]
      [@sop.max 3.141592653589793];
  } [@@sop.node_key "group_edges"] [@@sop.node_label "Group Edges"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let group = optional_text parameters.group in
    let incidence = parameters.incidence in
    let min_length = if parameters.use_min_length then Some parameters.min_length
            else None in
    let max_length = if parameters.use_max_length then Some parameters.max_length
            else None in
    let angle_basis = parameters.angle_basis in
    let min_angle = if parameters.use_min_angle then Some parameters.min_angle
            else None in
    let max_angle = if parameters.use_max_angle then Some parameters.max_angle
            else None in
    Node.Private.make_geometry ?label ~operation:"group_edges" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let primitives = match group with
          | None -> Ok None
          | Some group_name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive
                  group_name inputs.(0) with
               | Some value -> Ok (Some value)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   ~hints:["Create the primitive group before Edge Group"]
                   (Printf.sprintf "group_edges could not find primitive group %S"
                     group_name))) in
        match primitives with
        | Error _ as error -> error
        | Ok primitives ->
            match Rdk.Group_mesh.group_edges ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?primitives ~incidence
                ?min_length ?max_length ~angle_basis ?min_angle ?max_angle
                inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_random = struct
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "random"] [@sop.label "Group name"];
    probability : float [@sop.default 0.5] [@sop.label "Probability"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Random"];
    seed : int [@sop.default 0] [@sop.label "Seed"]
      [@sop.folder "Random"] [@sop.min 0] [@sop.max 9999];
    seed_attribute : string [@sop.default ""] [@sop.label "Seed attribute"]
      [@sop.folder "Random"];
    base : string [@sop.default ""] [@sop.label "Base group"]
      [@sop.folder "Combine"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.folder "Combine"] [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_random"] [@@sop.node_label "Group Random"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let seed = if parameters.context_seed then None else Some parameters.seed in
    let seed_attribute = optional_text parameters.seed_attribute in
    let base = optional_text parameters.base in
    let merge = parameters.merge in
    let probability = parameters.probability in
    let owner = parameters.owner in
    let name = parameters.name in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("group_random:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"group_random" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let identity = Option.value ~default:(Int64.of_int node_id)
            stable_identity in
        let seed = Rays_math.Rand.seed (Option.value ~default:(mixed_seed context identity)
            seed) in
        match Rdk.Group_ops.group_random ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~seed ?seed_attribute ?base ~merge
            ~probability ~owner ~name inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build

end

module Group_edge_depth = struct
  type parameters = {
    point_group : string [@sop.default "seed"] [@sop.label "Seed point group"];
    name : string [@sop.default "depth"] [@sop.label "Output group"];
    depth : int [@sop.default 1] [@sop.label "Depth"] [@sop.min 0]
      [@sop.max 100] [@sop.hard_min 0];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_edge_depth"] [@@sop.node_label "Group Edge Depth"]
    [@@sop.node_category "Group/Expand"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let merge = parameters.merge in
    let depth = parameters.depth in
    let point_group = parameters.point_group in
    let name = parameters.name in
    Node.Private.make_geometry ?label ~operation:"group_edge_depth" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_edge_depth ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~merge ~depth ~point_group ~name
            inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_boundary_components = struct
  let conflict_parameter = Param.choice ~equal:( = ) [
      "Replace", Rdk.Group_ops.Name_replace; "Union", Rdk.Group_ops.Name_union;
    ]
  type parameters = {
    prefix : string [@sop.default "boundary"] [@sop.label "Group prefix"];
    conflict : Rdk.Group_ops.name_conflict
      [@sop.default Rdk.Group_ops.Name_replace] [@sop.label "Conflict"]
      [@sop.kind conflict_parameter];
    max_groups : int [@sop.default 4096] [@sop.label "Maximum groups"]
      [@sop.folder "Limits"] [@sop.min 1] [@sop.max 16384]
      [@sop.hard_min 1];
    max_payload_bytes : int [@sop.default 268435456]
      [@sop.label "Maximum payload bytes"] [@sop.folder "Limits"]
      [@sop.min 1048576] [@sop.max 1073741824] [@sop.hard_min 1];
  } [@@sop.node_key "group_boundary_components"]
    [@@sop.node_label "Group Boundary Components"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let prefix = parameters.prefix in
    let conflict = parameters.conflict in
    let max_groups = parameters.max_groups in
    let max_payload_bytes = parameters.max_payload_bytes in
    Node.Private.make_geometry ?label ~operation:"group_boundary_components" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_boundary_components
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ~prefix ~conflict ~max_groups ~max_payload_bytes inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_from_attribute_boundary = struct
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_edges]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "attribute_boundary"]
      [@sop.label "Group name"];
    attributes : Rdk.Group_ops.boundary_attribute list [@sop.default []]
      [@sop.label "Attributes (owner, pattern)"]
      [@sop.kind boundary_attributes_parameter];
    tolerance : float [@sop.default 0.00001] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    include_unshared_edges : bool [@sop.default false]
      [@sop.label "Include unshared edges"];
    include_all_unshared_curve_edges : bool [@sop.default false]
      [@sop.label "Include all unshared curve edges"];
    include_all_primitives_sharing_boundary_points : bool [@sop.default false]
      [@sop.label "Include primitives sharing boundary points"];
  } [@@sop.node_key "group_from_attribute_boundary"]
    [@@sop.node_label "Group from Attribute Boundary"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let attributes = parameters.attributes in
    let tolerance = parameters.tolerance in
    let include_unshared_edges = parameters.include_unshared_edges in
    let include_all_unshared_curve_edges = parameters.include_all_unshared_curve_edges in
    let include_all_primitives_sharing_boundary_points = parameters.include_all_primitives_sharing_boundary_points in
    let owner = parameters.owner in
    let name = parameters.name in
    let attributes = List.map (fun (rule : Rdk.Group_ops.boundary_attribute) ->
      { Rdk.Group_ops.boundary_attribute_owner = rule.boundary_attribute_owner;
        boundary_attribute_pattern = rule.boundary_attribute_pattern }) attributes in
    Node.Private.make_geometry ?label ~operation:"group_from_attribute_boundary" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_from_attribute_boundary
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ~attributes ~tolerance ~include_unshared_edges
            ~include_all_unshared_curve_edges
            ~include_all_primitives_sharing_boundary_points ~owner ~name inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Groups_from_name = struct
  let conflict_parameter = Param.choice ~equal:( = ) [
      "Replace", Rdk.Group_ops.Name_replace; "Union", Rdk.Group_ops.Name_union;
    ]
  let invalid_parameter = Param.choice ~equal:( = ) [
      "Ignore invalid", Rdk.Group_ops.Ignore_invalid;
      "Force valid", Rdk.Group_ops.Force_valid;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Attribute owner"]
      [@sop.kind element_attribute_owner_parameter];
    attribute : string [@sop.default "name"] [@sop.label "Name attribute"];
    prefix : string [@sop.default ""] [@sop.label "Group prefix"];
    conflict : Rdk.Group_ops.name_conflict
      [@sop.default Rdk.Group_ops.Name_replace] [@sop.label "Conflict"]
      [@sop.kind conflict_parameter];
    invalid_names : Rdk.Group_ops.invalid_name_policy
      [@sop.default Rdk.Group_ops.Ignore_invalid] [@sop.label "Invalid names"]
      [@sop.kind invalid_parameter];
    max_groups : int [@sop.default 4096] [@sop.label "Maximum groups"]
      [@sop.folder "Limits"] [@sop.min 1] [@sop.max 16384]
      [@sop.hard_min 1];
    max_payload_bytes : int [@sop.default 268435456]
      [@sop.label "Maximum payload bytes"] [@sop.folder "Limits"]
      [@sop.min 1048576] [@sop.max 1073741824] [@sop.hard_min 1];
  } [@@sop.node_key "groups_from_name"] [@@sop.node_label "Groups from Name"]
    [@@sop.node_category "Group/Convert"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let prefix = parameters.prefix in
    let conflict = parameters.conflict in
    let invalid_names = parameters.invalid_names in
    let max_groups = parameters.max_groups in
    let max_payload_bytes = parameters.max_payload_bytes in
    let owner = parameters.owner in
    let attribute = parameters.attribute in
    Node.Private.make_geometry ?label ~operation:"groups_from_name" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.groups_from_name ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~prefix ~conflict ~invalid_names
            ~max_groups ~max_payload_bytes ~owner ~attribute inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Name_from_groups = struct
  let overlap_parameter = Param.choice ~equal:( = ) [
      "First group", Rdk.Group_ops.First_group; "Last group", Rdk.Group_ops.Last_group;
      "Error on overlap", Rdk.Group_ops.Error_on_overlap;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Group type"] [@sop.kind element_attribute_owner_parameter];
    attribute : string [@sop.default "name"] [@sop.label "Name attribute"];
    pattern : string [@sop.default "*"] [@sop.label "Group pattern"];
    default : string [@sop.default ""] [@sop.label "Default value"];
    overlap : Rdk.Group_ops.name_overlap [@sop.default Rdk.Group_ops.First_group]
      [@sop.label "Overlapping groups"] [@sop.kind overlap_parameter] ;
    delete_groups : bool [@sop.default false]
      [@sop.label "Delete source groups"];
  } [@@sop.node_key "name_from_groups"] [@@sop.node_label "Name from Groups"]
    [@@sop.node_category "Group/Convert"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let attribute = parameters.attribute in
    let pattern = parameters.pattern in
    let default = parameters.default in
    let overlap = parameters.overlap in
    let delete_groups = parameters.delete_groups in
    let owner = parameters.owner in
    Node.Private.make_geometry ?label ~operation:"name_from_groups" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.name_from_groups ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~attribute ~pattern ~default ~overlap
            ~delete_groups ~owner inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_promote_boundary = struct
  type parameters = {
    source : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_primitives]
      [@sop.label "Source owner"] [@sop.kind group_owner_parameter];
    destination : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_edges]
      [@sop.label "Destination owner"] [@sop.kind group_owner_parameter];
    group : string [@sop.default "group"] [@sop.label "Source group"];
    name : string [@sop.default ""] [@sop.label "New group name"];
    keep_original : bool [@sop.default false]
      [@sop.label "Keep original group"];
    output_attribute : string [@sop.default ""]
      [@sop.label "Output mask attribute"] [@sop.folder "Output"];
    attributes : Rdk.Group_ops.boundary_attribute list [@sop.default []]
      [@sop.label "Boundary attributes (owner, pattern)"]
      [@sop.kind boundary_attributes_parameter];
    tolerance : float [@sop.default 0.00001] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    include_unshared_edges : bool [@sop.default false]
      [@sop.label "Include unshared edges"];
    include_all_unshared_curve_edges : bool [@sop.default false]
      [@sop.label "Include all unshared curve edges"];
    include_all_primitives_sharing_boundary_points : bool [@sop.default false]
      [@sop.label "Include primitives sharing boundary points"];
  } [@@sop.node_key "group_promote_boundary"]
    [@@sop.node_label "Group Promote Boundary"]
    [@@sop.node_category "Group/Convert"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = optional_text parameters.name in
    let keep_original = parameters.keep_original in
    let output_attribute = optional_text parameters.output_attribute in
    let attributes = parameters.attributes in
    let tolerance = parameters.tolerance in
    let include_unshared_edges = parameters.include_unshared_edges in
    let include_all_unshared_curve_edges = parameters.include_all_unshared_curve_edges in
    let include_all_primitives_sharing_boundary_points = parameters.include_all_primitives_sharing_boundary_points in
    let source = parameters.source in
    let destination = parameters.destination in
    let group = parameters.group in
    let attributes = List.map (fun (rule : Rdk.Group_ops.boundary_attribute) ->
      { Rdk.Group_ops.boundary_attribute_owner = rule.boundary_attribute_owner;
        boundary_attribute_pattern = rule.boundary_attribute_pattern }) attributes in
    Node.Private.make_geometry ?label ~operation:"group_promote_boundary" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_promote_boundary
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ?name ~keep_original ?output_attribute ~attributes ~tolerance
            ~include_unshared_edges ~include_all_unshared_curve_edges
            ~include_all_primitives_sharing_boundary_points
            ~source ~destination ~group inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_delete = struct
  let encode_rule (rule : Rdk.Group_ops.delete_rule) = [
      (match rule.delete_owner with None -> "any"
       | Some owner -> group_owner_token owner);
      rule.delete_pattern;
    ]
  let decode_rule = function
    | [owner; pattern] ->
        let owner = String.lowercase_ascii (String.trim owner) in
        let owner = if owner = "any" || owner = "*" then Ok None
          else Result.map Option.some (group_owner_of_token owner) in
        Result.map (fun delete_owner -> {
          Rdk.Group_ops.delete_owner; delete_pattern = pattern }) owner
    | row -> Error (Printf.sprintf
        "Group Delete rule needs owner and pattern, got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    rules : Rdk.Group_ops.delete_rule list [@sop.default []]
      [@sop.label "Rules (owner, pattern)"] [@sop.kind rules_parameter];
    delete_unused : bool [@sop.default false]
      [@sop.label "Delete unused groups"];
  } [@@sop.node_key "group_delete"] [@@sop.node_label "Group Delete"]
    [@@sop.node_category "Group/Edit"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let delete_unused = parameters.delete_unused in
    let rules = parameters.rules in
    let rules = List.map (fun (rule : Rdk.Group_ops.delete_rule) ->
      { Rdk.Group_ops.delete_owner = rule.delete_owner;
        delete_pattern = rule.delete_pattern }) rules in
    Node.Private.make_geometry ?label ~operation:"group_delete" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        match Rdk.Group_ops.delete ~rules ~delete_unused inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_rename = struct
  let conflict_token = function
    | Rdk.Group_ops.Rename_skip -> "skip"
    | Rdk.Group_ops.Rename_error -> "error"
    | Rdk.Group_ops.Rename_overwrite -> "overwrite"
    | Rdk.Group_ops.Rename_union -> "union"
  let conflict_of_token = function
    | "skip" -> Ok Rdk.Group_ops.Rename_skip
    | "error" -> Ok Rdk.Group_ops.Rename_error
    | "overwrite" -> Ok Rdk.Group_ops.Rename_overwrite
    | "union" -> Ok Rdk.Group_ops.Rename_union
    | token -> Error (Printf.sprintf "unknown group rename conflict %S" token)
  let encode_rule (rule : Rdk.Group_ops.rename_rule) = [
      (match rule.rename_owner with None -> "any"
       | Some owner -> group_owner_token owner);
      rule.rename_pattern; rule.rename_replacement;
      conflict_token rule.rename_conflict;
    ]
  let decode_rule = function
    | [owner; pattern; replacement; conflict] ->
        let owner = String.lowercase_ascii (String.trim owner)
        and conflict = String.lowercase_ascii (String.trim conflict) in
        let owner = if owner = "any" || owner = "*" then Ok None
          else Result.map Option.some (group_owner_of_token owner) in
        Result.bind owner (fun rename_owner ->
          Result.map (fun rename_conflict -> {
            Rdk.Group_ops.rename_owner; rename_pattern = pattern;
            rename_replacement = replacement; rename_conflict })
            (conflict_of_token conflict))
    | row -> Error (Printf.sprintf
        "Group Rename rule needs owner, pattern, replacement, and conflict; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    rules : Rdk.Group_ops.rename_rule list [@sop.default []]
      [@sop.label "Rules (owner, pattern, replacement, conflict)"]
      [@sop.kind rules_parameter];
  } [@@sop.node_key "group_rename"] [@@sop.node_label "Group Rename"]
    [@@sop.node_category "Group/Edit"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let rules = parameters.rules in
    let rules = List.map (fun (rule : Rdk.Group_ops.rename_rule) ->
      { Rdk.Group_ops.rename_owner = rule.rename_owner;
        rename_pattern = rule.rename_pattern;
        rename_replacement = rule.rename_replacement;
        rename_conflict = rule.rename_conflict }) rules in
    Node.Private.make_geometry ?label ~operation:"group_rename" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        match Rdk.Group_ops.rename ~rules inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_copy = struct
  let encode_rule (rule : Rdk.Group_ops.copy_rule) = [
      group_owner_token rule.copy_owner; rule.copy_pattern; rule.copy_prefix;
      Option.value ~default:"" rule.match_attribute;
    ]
  let decode_rule = function
    | [owner; pattern; prefix; match_attribute] ->
        Result.map (fun copy_owner -> { Rdk.Group_ops.copy_owner;
          copy_pattern = pattern; copy_prefix = prefix;
          match_attribute = optional_text match_attribute })
          (group_owner_of_token
            (String.lowercase_ascii (String.trim owner)))
    | row -> Error (Printf.sprintf
        "Group Copy rule needs owner, pattern, prefix, and match attribute; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    use_rules : bool [@sop.default false] [@sop.label "Use rules"];
    rules : Rdk.Group_ops.copy_rule list [@sop.default []]
      [@sop.label "Rules (owner, pattern, prefix, match attribute)"]
      [@sop.kind rules_parameter];
    conflict : Rdk.Group_ops.copy_conflict
      [@sop.default Rdk.Group_ops.Copy_overwrite] [@sop.label "Conflict"]
      [@sop.kind group_copy_conflict_parameter] ;
    copy_empty : bool [@sop.default false] [@sop.label "Copy empty groups"];
  } [@@sop.node_key "group_copy"] [@@sop.node_label "Group Copy"]
    [@@sop.node_category "Group/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters source target ->
    let label = Some label in
    let rules = if parameters.use_rules then Some parameters.rules else None in
    let conflict = parameters.conflict in
    let copy_empty = parameters.copy_empty in
    let rules = Option.map (List.map (fun (rule : Rdk.Group_ops.copy_rule) ->
      { Rdk.Group_ops.copy_owner = rule.copy_owner;
        copy_pattern = rule.copy_pattern;
        copy_prefix = rule.copy_prefix;
        match_attribute = rule.match_attribute })) rules in
    Node.Private.make_geometry ?label ~operation:"group_copy" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.copy ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?rules ~conflict ~copy_empty
            ~source:inputs.(0) ~target:inputs.(1) () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

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
    Node.Private.make_geometry ?label ~operation:"group_transfer" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.transfer ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?rules ~conflict ~create_empty ~distance
            ~source:inputs.(0) ~target:inputs.(1) () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Group_find_path = struct
  let mode_parameter = Param.choice ~equal:( = ) [
      "Through each", Rdk.Group_mesh.Through_each;
      "Start/end pairs", Rdk.Group_mesh.Start_end_pairs;
    ]
  let ending_parameter = Param.choice ~equal:( = ) [
      "Stop at end", Rdk.Group_mesh.Stop_at_end; "Close path", Rdk.Group_mesh.Close_path;
    ]
  type parameters = {
    owner : Rdk.Group.owner [@sop.default Rdk.Group.Point]
      [@sop.label "Group type"] [@sop.kind ordinary_group_owner_parameter];
    base_group : string [@sop.default "ordered"] [@sop.label "Base group"];
    name : string [@sop.default "path"] [@sop.label "Output group"];
    mode : Rdk.Group_mesh.path_mode [@sop.default Rdk.Group_mesh.Through_each]
      [@sop.label "Path mode"] [@sop.kind mode_parameter];
    ending : Rdk.Group_mesh.path_ending [@sop.default Rdk.Group_mesh.Stop_at_end]
      [@sop.label "Ending"] [@sop.kind ending_parameter];
    avoid_self_intersection : bool [@sop.default true]
      [@sop.label "Avoid self-intersection"];
    collision_group : string [@sop.default ""]
      [@sop.label "Collision group"] [@sop.folder "Collision"];
    contain : bool [@sop.default false] [@sop.label "Contain path"]
      [@sop.folder "Collision"];
  } [@@sop.node_key "group_find_path"] [@@sop.node_label "Group Find Path"]
    [@@sop.node_category "Group/Path"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let mode = parameters.mode in
    let ending = parameters.ending in
    let avoid_self_intersection = parameters.avoid_self_intersection in
    let owner = parameters.owner in
    let collision_group = optional_text parameters.collision_group in
    let contain = parameters.contain in
    let base_group = parameters.base_group in
    let name = parameters.name in
    Node.Private.make_geometry ?label ~operation:"group_find_path" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match Rdk.Geometry.find_group ~owner base_group geometry with
        | None -> Error (Diagnostic.error ~code:"missing_group"
            (Printf.sprintf "group_find_path could not find %s group %S"
              (group_owner_key owner) base_group))
        | Some base ->
            let collision = match collision_group with
              | None -> Ok None
              | Some group ->
                  (match Rdk.Geometry.find_group ~owner group geometry with
                   | Some group -> Ok (Some group)
                   | None -> Error (Diagnostic.error ~code:"missing_group"
                       (Printf.sprintf
                         "group_find_path could not find collision %s group %S"
                         (group_owner_key owner) group))) in
            Result.bind collision (fun collision ->
              match Rdk.Group_mesh.group_find_path
                  ~cancel:(Context.cancel_token context)
                  ~grain:(Context.grain context) ~mode ~ending
                  ~avoid_self_intersection ?collision ~contain ~base ~name geometry with
              | Ok geometry -> cooked geometry
              | Error error -> structured_rdk_error error)))
  let factory = parameters_factory build
end

module Delete_edge_group = struct
  type parameters = {
    name : string [@sop.default "edges"] [@sop.label "Edge group"];
  } [@@sop.node_key "delete_edge_group"]
    [@@sop.node_label "Delete Edge Group"]
    [@@sop.node_category "Group/Manage"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    Node.Private.make_geometry ?label ~operation:"delete_edge_group" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        cooked (Rdk.Geometry.without_edge_group name inputs.(0))))
  let factory = parameters_factory build
end

module Rename_edge_group = struct
  type parameters = {
    from : string [@sop.default "edges"] [@sop.label "From"];
    into : string [@sop.default "renamed"] [@sop.label "To"];
  } [@@sop.node_key "rename_edge_group"]
    [@@sop.node_label "Rename Edge Group"]
    [@@sop.node_category "Group/Manage"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let from = parameters.from in
    let into = parameters.into in
    Node.Private.make_geometry ?label ~operation:"rename_edge_group" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        match Rdk.Geometry.rename_edge_group ~from ~into inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error message -> rdk_error "rename_edge_group" message))
  let factory = parameters_factory build
end

module Group_invert = struct
  type owner = Support.group_invert_owner = Any | Owner of Rdk.Group_ops.owner
  let owner_parameter = Param.choice ~equal:( = ) [
      "Any", Any; "Points", Owner Rdk.Group_ops.Group_points;
      "Vertices", Owner Rdk.Group_ops.Group_vertices;
      "Primitives", Owner Rdk.Group_ops.Group_primitives;
      "Edges", Owner Rdk.Group_ops.Group_edges;
    ]
  type parameters = {
    owner : owner [@sop.default Any] [@sop.label "Group type"]
      [@sop.kind owner_parameter];
    pattern : string [@sop.default "*"] [@sop.label "Group pattern"];
    new_name : string [@sop.default ""] [@sop.label "New name pattern"];
    conflict : Rdk.Group_ops.rename_conflict
      [@sop.default Rdk.Group_ops.Rename_overwrite] [@sop.label "Conflict"]
      [@sop.kind group_rename_conflict_parameter];
  } [@@sop.node_key "group_invert"] [@@sop.node_label "Group Invert"]
    [@@sop.node_category "Group/Edit"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let conflict = parameters.conflict in
    let owner = parameters.owner in
    let pattern = parameters.pattern in
    let new_name = parameters.new_name in
    let owner = match owner with Any -> None | Owner owner -> Some owner in
    let new_name = optional_text new_name in
    Node.Private.make_geometry ?label ~operation:"group_invert" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        match Rdk.Group_ops.invert ~conflict ?owner ~pattern ?new_name inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_combine = struct
  let bool_token value = if value then "true" else "false"
  let bool_of_token = function
    | "true" | "1" | "yes" -> Ok true
    | "false" | "0" | "no" -> Ok false
    | token -> Error (Printf.sprintf "expected boolean, got %S" token)
  let encode_step (step : Rdk.Group_ops.combine_step) = [
      group_boolean_token step.operation; step.operand.pattern;
      bool_token step.operand.inverted;
    ]
  let decode_step = function
    | [operation; pattern; inverted] ->
        Result.bind (group_boolean_of_token
          (String.lowercase_ascii (String.trim operation)))
          (fun operation -> Result.map (fun inverted -> {
            Rdk.Group_ops.operation; operand = { pattern; inverted } })
            (bool_of_token
              (String.lowercase_ascii (String.trim inverted))))
    | row -> Error (Printf.sprintf
        "Group Combine step needs operation, pattern, and invert; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun steps ->
        Result.map (fun step -> step :: steps) (decode_step row))) (Ok []) rows
      |> Result.map List.rev)
  let steps_parameter = Param.encoded ~equal:( = )
      ~encode:(fun steps -> encode_table (List.map encode_step steps)) ~decode
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "combined"] [@sop.label "Output group"];
    base_pattern : string [@sop.default "*"] [@sop.label "Base pattern"];
    base_inverted : bool [@sop.default false] [@sop.label "Invert base"];
    steps : Rdk.Group_ops.combine_step list [@sop.default []]
      [@sop.label "Steps (operation, pattern, invert)"]
      [@sop.kind steps_parameter];
  } [@@sop.node_key "group_combine"] [@@sop.node_label "Group Combine"]
    [@@sop.node_category "Group/Edit"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let base_pattern = parameters.base_pattern in
    let base_inverted = parameters.base_inverted in
    let steps = parameters.steps in
    let steps = List.map (fun (step : Rdk.Group_ops.combine_step) ->
      { Rdk.Group_ops.operation = step.operation;
        operand = { Rdk.Group_ops.pattern = step.operand.pattern;
          inverted = step.operand.inverted } }) steps in
    let base = { Rdk.Group_ops.pattern = base_pattern; inverted = base_inverted } in
    Node.Private.make_geometry ?label ~operation:"group_combine" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.combine ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~owner ~name ~base ~steps inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_promotions = struct
  let bool_token value = if value then "true" else "false"
  let bool_of_token = function
    | "true" | "1" | "yes" -> Ok true
    | "false" | "0" | "no" -> Ok false
    | token -> Error (Printf.sprintf "expected boolean, got %S" token)
  let encode_attributes attributes = encode_table (List.map
      (fun (attribute : Rdk.Group_ops.boundary_attribute) ->
        [attribute_owner_token attribute.boundary_attribute_owner;
         attribute.boundary_attribute_pattern]) attributes)
  let decode_attributes text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun attributes ->
        match row with
        | [owner; pattern] -> Result.map (fun boundary_attribute_owner ->
            { Rdk.Group_ops.boundary_attribute_owner;
              boundary_attribute_pattern = pattern } :: attributes)
            (attribute_owner_of_token
              (String.lowercase_ascii (String.trim owner)))
        | row -> Error (Printf.sprintf
            "boundary attribute needs owner and pattern, got %d columns"
            (List.length row)))) (Ok []) rows |> Result.map List.rev)
  let encode_operation = function
    | Rdk.Group_ops.Promote_elements mode ->
        let token = match mode with
          | Rdk.Group_ops.Include_any -> "any"
          | Rdk.Group_ops.Include_all -> "all"
          | Rdk.Group_ops.Include_shared_edge -> "shared_edge" in
        [token; "0"; "false"; "false"; "false"; ""]
    | Rdk.Group_ops.Promote_boundary options -> [
        "boundary"; Param.float_text options.promote_boundary_tolerance;
        bool_token options.promote_include_unshared_edges;
        bool_token options.promote_include_all_unshared_curve_edges;
        bool_token options.promote_include_all_primitives_sharing_boundary_points;
        encode_attributes options.promote_boundary_attributes;
      ]
  let decode_operation = function
    | [kind; tolerance; unshared; all_curve; all_primitives; attributes] ->
        (match String.lowercase_ascii (String.trim kind) with
         | "any" -> Ok (Rdk.Group_ops.Promote_elements Rdk.Group_ops.Include_any)
         | "all" -> Ok (Rdk.Group_ops.Promote_elements Rdk.Group_ops.Include_all)
         | "shared_edge" | "shared edge" ->
             Ok (Rdk.Group_ops.Promote_elements Rdk.Group_ops.Include_shared_edge)
         | "boundary" ->
             (match float_of_string_opt (String.trim tolerance) with
              | None -> Error (Printf.sprintf "invalid boundary tolerance %S"
                  tolerance)
              | Some promote_boundary_tolerance ->
                  Result.bind (bool_of_token
                    (String.lowercase_ascii (String.trim unshared)))
                    (fun promote_include_unshared_edges ->
                      Result.bind (bool_of_token
                        (String.lowercase_ascii (String.trim all_curve)))
                        (fun promote_include_all_unshared_curve_edges ->
                          Result.bind (bool_of_token
                            (String.lowercase_ascii
                              (String.trim all_primitives)))
                            (fun promote_include_all_primitives_sharing_boundary_points ->
                              Result.map (fun promote_boundary_attributes ->
                                Rdk.Group_ops.Promote_boundary {
                                  Rdk.Group_ops.promote_boundary_attributes;
                                  promote_boundary_tolerance;
                                  promote_include_unshared_edges;
                                  promote_include_all_unshared_curve_edges;
                                  promote_include_all_primitives_sharing_boundary_points })
                                (decode_attributes attributes)))))
         | token -> Error (Printf.sprintf
             "unknown group promotion operation %S" token))
    | columns -> Error (Printf.sprintf
        "group promotion operation needs 6 columns, got %d"
        (List.length columns))
  let encode_rule (rule : Rdk.Group_ops.promotion_rule) = [
      group_owner_token rule.promotion_source;
      group_owner_token rule.promotion_destination;
      rule.promotion_pattern;
      Option.value ~default:"" rule.promotion_new_name;
      bool_token rule.promotion_keep_original;
      bool_token rule.promotion_output_as_attribute;
    ] @ encode_operation rule.promotion_operation
  let decode_rule = function
    | source :: destination :: pattern :: new_name :: keep_original ::
        output_as_attribute :: operation ->
        Result.bind (group_owner_of_token
          (String.lowercase_ascii (String.trim source)))
          (fun promotion_source -> Result.bind (group_owner_of_token
            (String.lowercase_ascii (String.trim destination)))
            (fun promotion_destination -> Result.bind (bool_of_token
              (String.lowercase_ascii (String.trim keep_original)))
              (fun promotion_keep_original -> Result.bind (bool_of_token
                (String.lowercase_ascii (String.trim output_as_attribute)))
                (fun promotion_output_as_attribute ->
                  Result.map (fun promotion_operation -> {
                    Rdk.Group_ops.promotion_source; promotion_destination;
                    promotion_pattern = pattern;
                    promotion_new_name = optional_text new_name;
                    promotion_keep_original; promotion_output_as_attribute;
                    promotion_operation }) (decode_operation operation)))))
    | row -> Error (Printf.sprintf
        "Group Promotions rule needs 12 columns, got %d"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  let default_rules = [{
      Rdk.Group_ops.promotion_source = Rdk.Group_ops.Group_points;
      promotion_destination = Rdk.Group_ops.Group_primitives;
      promotion_pattern = "*"; promotion_new_name = None;
      promotion_keep_original = false;
      promotion_output_as_attribute = false;
      promotion_operation = Rdk.Group_ops.Promote_elements Rdk.Group_ops.Include_any;
    }]
  type parameters = {
    rules : Rdk.Group_ops.promotion_rule list [@sop.default default_rules]
      [@sop.label "Rules (source, destination, pattern, new name, keep, attribute, operation...)"]
      [@sop.kind rules_parameter];
    max_outputs : int [@sop.default 4096] [@sop.label "Maximum outputs"]
      [@sop.folder "Limits"] [@sop.min 1] [@sop.max 16384]
      [@sop.hard_min 1];
    max_payload_bytes : int [@sop.default 268435456]
      [@sop.label "Maximum payload bytes"] [@sop.folder "Limits"]
      [@sop.min 1048576] [@sop.max 1073741824] [@sop.hard_min 1];
  } [@@sop.node_key "group_promotions"]
    [@@sop.node_label "Group Promotions"]
    [@@sop.node_category "Group/Convert"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let max_outputs = parameters.max_outputs in
    let max_payload_bytes = parameters.max_payload_bytes in
    let rules = parameters.rules in
    let rules = List.filter (fun (rule : Rdk.Group_ops.promotion_rule) ->
        String.trim rule.promotion_pattern <> "") rules in
    Node.Private.make_geometry ?label ~operation:"group_promotions" ~version:1
          ~parameters:""
          ~cook_mode:(Node.Duplicate_input 0)
          ~dependencies:Context.Dependencies.static ~inputs:[|input|]
          (fun ~node_id:_ context inputs ->
            match Rdk.Group_ops.promotions ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~max_outputs ~max_payload_bytes
                ~rules inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_ranges = struct
  let encode_specification = function
    | Rdk.Group_ops.Range_start_end { start; end_ } ->
        ["start_end"; string_of_int start; string_of_int end_]
    | Rdk.Group_ops.Range_from_ends { start; end_offset } ->
        ["from_ends"; string_of_int start; string_of_int end_offset]
    | Rdk.Group_ops.Range_start_length { start; length } ->
        ["start_length"; string_of_int start; string_of_int length]
    | Rdk.Group_ops.Range_partition { partition; partitions } ->
        ["partition"; string_of_int partition; string_of_int partitions]
  let decode_specification = function
    | [kind; a; b] ->
        let ( let* ) = Result.bind in
        let* a = int_of_token a in
        let* b = int_of_token b in
        (match String.lowercase_ascii (String.trim kind) with
         | "start_end" -> Ok (Rdk.Group_ops.Range_start_end { start = a; end_ = b })
         | "from_ends" -> Ok (Rdk.Group_ops.Range_from_ends {
             start = a; end_offset = b })
         | "start_length" -> Ok (Rdk.Group_ops.Range_start_length {
             start = a; length = b })
         | "partition" -> Ok (Rdk.Group_ops.Range_partition {
             partition = a; partitions = b })
         | token -> Error (Printf.sprintf "unknown range kind %S" token))
    | columns -> Error (Printf.sprintf
        "range specification needs 3 columns, got %d" (List.length columns))
  let encode_filter = function
    | None -> ["none"; "0"; "1"; "0"]
    | Some filter -> ["filter"; string_of_int filter.Rdk.Group_ops.select;
        string_of_int filter.of_; string_of_int filter.offset]
  let decode_filter = function
    | [kind; select; of_; offset] ->
        if String.lowercase_ascii (String.trim kind) = "none" then Ok None
        else
          let ( let* ) = Result.bind in
          let* select = int_of_token select in
          let* of_ = int_of_token of_ in
          let* offset = int_of_token offset in
          Ok (Some { Rdk.Group_ops.select; of_; offset })
    | columns -> Error (Printf.sprintf
        "range filter needs 4 columns, got %d" (List.length columns))
  let encode_connectivity = function
    | None -> ["none"; ""; ""; "0"; "false"; "edge"; "";
        "false"; "false"]
    | Some (Rdk.Group_ops.Range_disconnected { region }) -> [
        "disconnected"; Option.fold ~none:"" ~some:string_of_int region;
        ""; "0"; "false"; "edge"; ""; "false"; "false"]
    | Some (Rdk.Group_ops.Range_connected connectivity) ->
        let collision_enabled, collision_owner, collision_pattern, keep_boundary =
          match connectivity.collision with
          | None -> "false", "edge", "", "false"
          | Some collision -> "true",
              group_owner_token collision.collision_owner,
              collision.collision_pattern, bool_token collision.keep_boundary in
        ["connected";
         Option.fold ~none:"" ~some:string_of_int connectivity.region;
         Option.value ~default:"" connectivity.connectivity_attributes;
         Param.float_text connectivity.connectivity_tolerance;
         collision_enabled; collision_owner; collision_pattern; keep_boundary;
         bool_token connectivity.remove_other_regions]
  let decode_connectivity = function
    | [kind; region; attributes; tolerance; collision_enabled;
        collision_owner; collision_pattern; keep_boundary;
        remove_other_regions] ->
        let ( let* ) = Result.bind in
        let region = if String.trim region = "" then Ok None
          else Result.map Option.some (int_of_token region) in
        let* region = region in
        (match String.lowercase_ascii (String.trim kind) with
         | "none" -> Ok None
         | "disconnected" -> Ok (Some (Rdk.Group_ops.Range_disconnected { region }))
         | "connected" ->
             let* connectivity_tolerance = float_of_token tolerance in
             let* collision_enabled = bool_of_token collision_enabled in
             let* collision = if not collision_enabled then Ok None else
               let* collision_owner = group_owner_of_token
                   (String.lowercase_ascii (String.trim collision_owner)) in
               let* keep_boundary = bool_of_token keep_boundary in
               Ok (Some { Rdk.Group_ops.collision_owner; collision_pattern;
                 keep_boundary }) in
             let* remove_other_regions = bool_of_token remove_other_regions in
             Ok (Some (Rdk.Group_ops.Range_connected {
               connectivity_attributes = optional_text attributes;
               connectivity_tolerance; collision; region;
               remove_other_regions }))
         | token -> Error (Printf.sprintf
             "unknown range connectivity %S" token))
    | columns -> Error (Printf.sprintf
        "range connectivity needs 9 columns, got %d" (List.length columns))
  let encode_rule (rule : Rdk.Group_ops.range_rule) =
    [group_owner_token rule.range_owner; rule.range_name;
     Option.value ~default:"" rule.range_base; bool_token rule.range_invert;
     group_boolean_token rule.range_merge]
    @ encode_specification rule.range_specification
    @ encode_filter rule.range_filter
    @ encode_connectivity rule.range_connectivity
  let decode_rule = function
    | [owner; name; base; invert; merge; s0; s1; s2; f0; f1; f2; f3;
        c0; c1; c2; c3; c4; c5; c6; c7; c8] ->
        let ( let* ) = Result.bind in
        let* range_owner = group_owner_of_token
            (String.lowercase_ascii (String.trim owner)) in
        let* range_invert = bool_of_token invert in
        let* range_merge = group_boolean_of_token
            (String.lowercase_ascii (String.trim merge)) in
        let* range_specification = decode_specification [s0; s1; s2] in
        let* range_filter = decode_filter [f0; f1; f2; f3] in
        let* range_connectivity = decode_connectivity
            [c0; c1; c2; c3; c4; c5; c6; c7; c8] in
        Ok { Rdk.Group_ops.range_owner; range_name = name;
          range_base = optional_text base; range_invert; range_filter;
          range_connectivity; range_merge; range_specification }
    | row -> Error (Printf.sprintf
        "Group Ranges rule needs 21 columns, got %d" (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Param.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  let default_rules = [Rdk.Group_ops.range_rule
      ~owner:Rdk.Group_ops.Group_points ~name:"range"
      (Rdk.Group_ops.Range_start_end { start = 0; end_ = -1 })]
  type parameters = {
    rules : Rdk.Group_ops.range_rule list [@sop.default default_rules]
      [@sop.label "Range rules"] [@sop.kind rules_parameter];
  } [@@sop.node_key "group_ranges"] [@@sop.node_label "Group Ranges"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let rules = parameters.rules in
    let rules = List.filter (fun (rule : Rdk.Group_ops.range_rule) ->
        String.trim rule.range_name <> "") rules in
    List.iter (fun (rule : Rdk.Group_ops.range_rule) ->
      Option.iter (fun pattern -> if String.trim pattern = "" then
        invalid_arg "sop/group_ranges: empty base pattern") rule.range_base) rules;
    Node.Private.make_geometry ?label ~operation:"group_ranges" ~version:1
          ~parameters:""
          ~cook_mode:(Node.Duplicate_input 0)
          ~dependencies:Context.Dependencies.static ~inputs:[|input|]
          (fun ~node_id:_ context inputs ->
            match Rdk.Group_ops.ranges ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~rules inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_bounds = struct
  type shape = Support.group_bounds_shape = Box | Sphere
  let shape_parameter = Param.choice ~equal:( = ) [
      "Box", Box; "Sphere", Sphere;
    ]
  let containment_parameter = Param.choice ~equal:( = ) [
      "Fully contained", Rdk.Group_ops.Fully_contained;
      "Partially contained", Rdk.Group_ops.Partially_contained;
    ]
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "bounds"] [@sop.label "Group name"];
    shape : shape [@sop.default Box] [@sop.label "Bounding shape"]
      [@sop.kind shape_parameter];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Bounds/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Bounds/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Bounds/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    size_x : float [@sop.default 1.] [@sop.label "Size X"]
      [@sop.folder "Bounds/Size"] [@sop.min 0.] [@sop.max 20.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    size_y : float [@sop.default 1.] [@sop.label "Size Y"]
      [@sop.folder "Bounds/Size"] [@sop.min 0.] [@sop.max 20.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    size_z : float [@sop.default 1.] [@sop.label "Size Z"]
      [@sop.folder "Bounds/Size"] [@sop.min 0.] [@sop.max 20.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    radius : float [@sop.default 0.5] [@sop.label "Radius"]
      [@sop.folder "Bounds"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    base : string [@sop.default ""] [@sop.label "Base group"]
      [@sop.folder "Combine"];
    containment : Rdk.Group_ops.containment
      [@sop.default Rdk.Group_ops.Fully_contained] [@sop.label "Containment"]
      [@sop.folder "Combine"] [@sop.kind containment_parameter];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.folder "Combine"] [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_bounds"] [@@sop.node_label "Group by Bounds"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let base = optional_text parameters.base in
    let containment = parameters.containment in
    let merge = parameters.merge in
    let shape = parameters.shape in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let size = Rays_math.Vec3.create parameters.size_x parameters.size_y parameters.size_z in
    let radius = parameters.radius in
    let owner = parameters.owner in
    let name = parameters.name in
    let bounds = match shape with
      | Sphere -> Rdk.Group_ops.Bounds_sphere { center = vec3_copy center; radius }
      | Box -> let half = Rays_math.Vec3.scale size 0.5 in
          Rdk.Group_ops.Bounds_box { minimum = Rays_math.Vec3.sub center half;
            maximum = Rays_math.Vec3.add center half } in
    Node.Private.make_geometry ?label ~operation:"group_bounds" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_bounds ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?base ~containment ~merge bounds
            ~owner ~name inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_range = struct
  type range_mode = Support.group_range_mode = Start_end | From_ends | Start_length | Partition
  type connectivity_mode = Support.group_range_connectivity = No_connectivity | Disconnected | Connected
  let range_parameter = Param.choice ~equal:( = ) [
      "Start and end", Start_end; "From ends", From_ends;
      "Start and length", Start_length; "Partition", Partition;
    ]
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "None", No_connectivity; "Disconnected regions", Disconnected;
      "Connected with seams", Connected;
    ]
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    name : string [@sop.default "range"] [@sop.label "Output group"];
    base : string [@sop.default ""] [@sop.label "Base group"];
    invert : bool [@sop.default false] [@sop.label "Invert range"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Merge"]
      [@sop.kind group_merge_parameter];
    range_mode : range_mode [@sop.default Start_end] [@sop.label "Range"]
      [@sop.kind range_parameter];
    start : int [@sop.default 0] [@sop.label "Start"]
      [@sop.folder "Range"] [@sop.min (-100000)] [@sop.max 100000];
    end_ : int [@sop.default (-1)] [@sop.label "End"]
      [@sop.folder "Range"] [@sop.min (-100000)] [@sop.max 100000];
    end_offset : int [@sop.default 0] [@sop.label "End offset"]
      [@sop.folder "Range"] [@sop.min (-100000)] [@sop.max 100000];
    length : int [@sop.default 1] [@sop.label "Length"]
      [@sop.folder "Range"] [@sop.min 0] [@sop.max 100000]
      [@sop.hard_min 0];
    partition : int [@sop.default 0] [@sop.label "Partition"]
      [@sop.folder "Range"] [@sop.min 0] [@sop.max 10000]
      [@sop.hard_min 0];
    partitions : int [@sop.default 2] [@sop.label "Partitions"]
      [@sop.folder "Range"] [@sop.min 1] [@sop.max 10000]
      [@sop.hard_min 1];
    use_filter : bool [@sop.default false] [@sop.label "Use filter"]
      [@sop.folder "Filter"];
    filter_select : int [@sop.default 1] [@sop.label "Select"]
      [@sop.folder "Filter"] [@sop.min 0] [@sop.max 10000]
      [@sop.hard_min 0];
    filter_of : int [@sop.default 1] [@sop.label "Of"]
      [@sop.folder "Filter"] [@sop.min 1] [@sop.max 10000]
      [@sop.hard_min 1];
    filter_offset : int [@sop.default 0] [@sop.label "Offset"]
      [@sop.folder "Filter"] [@sop.min (-10000)] [@sop.max 10000];
    connectivity_mode : connectivity_mode [@sop.default No_connectivity]
      [@sop.label "Connectivity"] [@sop.folder "Connectivity"]
      [@sop.kind connectivity_parameter];
    use_region : bool [@sop.default false] [@sop.label "Restrict region"]
      [@sop.folder "Connectivity"];
    region : int [@sop.default 0] [@sop.label "Region"]
      [@sop.folder "Connectivity"] [@sop.min 0] [@sop.max 100000]
      [@sop.hard_min 0];
    connectivity_attributes : string [@sop.default ""]
      [@sop.label "Connectivity attributes"] [@sop.folder "Connectivity"];
    connectivity_tolerance : float [@sop.default 0.00001]
      [@sop.label "Attribute tolerance"] [@sop.folder "Connectivity"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    use_collision : bool [@sop.default false]
      [@sop.label "Use collision group"] [@sop.folder "Connectivity/Collision"];
    collision_owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_edges]
      [@sop.label "Collision owner"] [@sop.folder "Connectivity/Collision"]
      [@sop.kind group_owner_parameter];
    collision_pattern : string [@sop.default "collision"]
      [@sop.label "Collision pattern"] [@sop.folder "Connectivity/Collision"];
    keep_boundary : bool [@sop.default false] [@sop.label "Keep boundary"]
      [@sop.folder "Connectivity/Collision"];
    remove_other_regions : bool [@sop.default false]
      [@sop.label "Remove other regions"] [@sop.folder "Connectivity"];
  } [@@sop.node_key "group_range"] [@@sop.node_label "Group Range"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let base = optional_text parameters.base in
    let invert = parameters.invert in
    let merge = parameters.merge in
    let owner = parameters.owner in
    let name = parameters.name in
    let range_mode = parameters.range_mode in
    let start = parameters.start in
    let end_ = parameters.end_ in
    let end_offset = parameters.end_offset in
    let length = parameters.length in
    let partition = parameters.partition in
    let partitions = parameters.partitions in
    let use_filter = parameters.use_filter in
    let filter_select = parameters.filter_select in
    let filter_of = parameters.filter_of in
    let filter_offset = parameters.filter_offset in
    let connectivity_mode = parameters.connectivity_mode in
    let use_region = parameters.use_region in
    let region = parameters.region in
    let connectivity_attributes = optional_text parameters.connectivity_attributes in
    let connectivity_tolerance = parameters.connectivity_tolerance in
    let use_collision = parameters.use_collision in
    let collision_owner = parameters.collision_owner in
    let collision_pattern = parameters.collision_pattern in
    let keep_boundary = parameters.keep_boundary in
    let remove_other_regions = parameters.remove_other_regions in
    let range = match range_mode with
      | Start_end -> Rdk.Group_ops.Range_start_end { start; end_ }
      | From_ends -> Rdk.Group_ops.Range_from_ends { start; end_offset }
      | Start_length -> Rdk.Group_ops.Range_start_length { start; length }
      | Partition -> Rdk.Group_ops.Range_partition { partition; partitions } in
    let filter = if use_filter then Some { Rdk.Group_ops.select = filter_select;
        of_ = filter_of; offset = filter_offset } else None in
    let region = if use_region then Some region else None in
    let connectivity = match connectivity_mode with
      | No_connectivity -> None
      | Disconnected -> Some (Rdk.Group_ops.Range_disconnected { region })
      | Connected ->
          let collision = if use_collision then Some { Rdk.Group_ops.collision_owner;
              collision_pattern; keep_boundary } else None in
          Some (Rdk.Group_ops.Range_connected { connectivity_attributes;
            connectivity_tolerance; collision; region; remove_other_regions }) in
    Node.Private.make_geometry ?label ~operation:"group_range" ~version:3
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.range ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?base ~invert ?filter ?connectivity
            ~merge ~owner ~name range inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Ordered_group = struct
  let decode text =
    String.split_on_char ' ' (String.map (function
      | ',' | '\t' | '\n' -> ' ' | character -> character) text)
    |> List.filter (fun token -> token <> "")
    |> List.fold_left (fun result token -> Result.bind result (fun elements ->
        Result.bind (int_of_token token) (fun element ->
          if element < 0 then Error (Printf.sprintf
            "expected a non-negative element index, got %d" element)
          else Ok (element :: elements)))) (Ok [])
    |> Result.map List.rev
  let elements_parameter = Param.encoded ~equal:( = )
      ~encode:(fun elements ->
        String.concat " " (List.map string_of_int elements)) ~decode
  type parameters = {
    owner : Rdk.Group.owner [@sop.default Rdk.Group.Point]
      [@sop.label "Group type"] [@sop.kind ordinary_group_owner_parameter];
    name : string [@sop.default "ordered"] [@sop.label "Output group"];
    elements : int list [@sop.default [0]] [@sop.label "Elements in order"]
      [@sop.kind elements_parameter];
  } [@@sop.node_key "ordered_group"] [@@sop.node_label "Group Ordered"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      if List.exists (fun element -> element < 0) parameters.elements then
        invalid_arg "sop/ordered_group: negative element index"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let elements = parameters.elements in
    let elements = Array.of_list elements in
    Node.Private.make_geometry ?label ~operation:"ordered_group" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        let geometry = inputs.(0) in
        let length = match owner with
          | Rdk.Group.Point -> Rdk.Geometry.point_count geometry
          | Rdk.Group.Vertex -> Rdk.Geometry.vertex_count geometry
          | Rdk.Group.Primitive -> Rdk.Geometry.primitive_count geometry in
        match Rdk.Group.ordered ~owner ~name ~length elements with
        | Error message -> rdk_error "ordered_group" message
        | Ok group ->
            match Rdk.Geometry.with_group group geometry with
            | Ok geometry -> cooked geometry
            | Error message -> rdk_error "ordered_group" message)
  )
  let factory = parameters_factory build
end

module Group_normal = struct
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_primitives]
      [@sop.label "Group type"] [@sop.kind group_normal_owner_parameter];
    name : string [@sop.default "normal"] [@sop.label "Group name"];
    direction_x : float [@sop.default 0.] [@sop.label "Direction X"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_y : float [@sop.default 1.] [@sop.label "Direction Y"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_z : float [@sop.default 0.] [@sop.label "Direction Z"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    spread_angle : float [@sop.default 0.7853981633974483]
      [@sop.label "Spread angle"] [@sop.min 0.]
      [@sop.max 3.141592653589793] [@sop.hard_min 0.]
      [@sop.hard_max 3.141592653589793];
    normal_attribute : string [@sop.default ""] [@sop.label "Normal attribute"]
      [@sop.folder "Normals"];
    use_existing_normal : bool [@sop.default true]
      [@sop.label "Use existing normal"] [@sop.folder "Normals"];
    include_opposite : bool [@sop.default false]
      [@sop.label "Include opposite"] [@sop.folder "Normals"];
    base : string [@sop.default ""] [@sop.label "Base group"]
      [@sop.folder "Combine"];
    merge : Rdk.Group_ops.boolean_operation
      [@sop.default Rdk.Group_ops.Group_replace] [@sop.label "Operation"]
      [@sop.folder "Combine"] [@sop.kind group_merge_parameter];
  } [@@sop.node_key "group_normal"] [@@sop.node_label "Group by Normal"]
    [@@sop.node_category "Group/Create"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      if parameters.direction_x = 0. && parameters.direction_y = 0.
          && parameters.direction_z = 0. then
        invalid_arg "sop/group_normal: direction must be non-zero";
      if parameters.owner = Rdk.Group_ops.Group_vertices then
        invalid_arg "sop/group_normal: vertex groups are not supported"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let normal_attribute = optional_text parameters.normal_attribute in
    let use_existing_normal = parameters.use_existing_normal in
    let base = optional_text parameters.base in
    let include_opposite = parameters.include_opposite in
    let merge = parameters.merge in
    let direction = Rays_math.Vec3.create parameters.direction_x parameters.direction_y
            parameters.direction_z in
    let spread_angle = parameters.spread_angle in
    let owner = parameters.owner in
    let name = parameters.name in
    let direction = vec3_copy direction in
    Node.Private.make_geometry ?label ~operation:"group_normal" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.group_normal ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?normal_attribute ~use_existing_normal ?base
            ~include_opposite ~merge ~direction ~spread_angle ~owner ~name
            inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Group_expand = struct
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Share points", Rdk.Group_ops.Primitive_share_points;
      "Share edges", Rdk.Group_ops.Primitive_share_edges;
    ]
  type parameters = {
    owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Group type"] [@sop.kind group_owner_parameter];
    group : string [@sop.default "group"] [@sop.label "Source group"];
    name : string [@sop.default ""] [@sop.label "Output group"];
    steps : int [@sop.default 1] [@sop.label "Steps"]
      [@sop.min (-100)] [@sop.max 100];
    flood : bool [@sop.default false] [@sop.label "Flood fill"];
    step_attribute : string [@sop.default ""]
      [@sop.label "Step attribute"] [@sop.folder "Output"];
    primitive_connectivity : Rdk.Group_ops.primitive_connectivity
      [@sop.default Rdk.Group_ops.Primitive_share_points]
      [@sop.label "Primitive connectivity"]
      [@sop.folder "Connectivity"] [@sop.kind connectivity_parameter];
    normal_spread : float [@sop.default 3.141592653589793]
      [@sop.label "Normal spread"] [@sop.folder "Connectivity/Normals"]
      [@sop.min 0.] [@sop.max 3.141592653589793]
      [@sop.hard_min 0.] [@sop.hard_max 3.141592653589793];
    use_normal_attribute : bool [@sop.default false]
      [@sop.label "Use normal attribute"] [@sop.folder "Connectivity/Normals"];
    normal_owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Normal owner"] [@sop.folder "Connectivity/Normals"]
      [@sop.kind element_attribute_owner_parameter];
    normal_name : string [@sop.default "N"] [@sop.label "Normal attribute"]
      [@sop.folder "Connectivity/Normals"];
    connectivity_attributes : Rdk.Group_ops.boundary_attribute list
      [@sop.default []] [@sop.label "Boundary attributes (owner, pattern)"]
      [@sop.folder "Connectivity"] [@sop.kind boundary_attributes_parameter];
    connectivity_tolerance : float [@sop.default 0.00001]
      [@sop.label "Attribute tolerance"] [@sop.folder "Connectivity"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    use_collision : bool [@sop.default false]
      [@sop.label "Use collision group"] [@sop.folder "Collision"];
    collision_owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_edges]
      [@sop.label "Collision owner"] [@sop.folder "Collision"]
      [@sop.kind group_owner_parameter];
    collision_group : string [@sop.default "collision"]
      [@sop.label "Collision group"] [@sop.folder "Collision"];
    collision_contain : bool [@sop.default false]
      [@sop.label "Contain region"] [@sop.folder "Collision"];
    collision_allow_boundary : bool [@sop.default false]
      [@sop.label "Allow boundary"] [@sop.folder "Collision"];
  } [@@sop.node_key "group_expand"] [@@sop.node_label "Group Expand"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.group = "" then invalid_arg "sop/group_expand: empty group name";
      if not (Float.is_finite parameters.normal_spread) || parameters.normal_spread < 0.
          || parameters.normal_spread > Float.pi then
        invalid_arg "sop/group_expand: normal spread must be finite and within [0,pi]";
      if not (Float.is_finite parameters.connectivity_tolerance) || parameters.connectivity_tolerance < 0. then
        invalid_arg "sop/group_expand: connectivity tolerance must be finite and non-negative";
      if parameters.normal_owner = Rdk.Attribute.Detail then
        invalid_arg "sop/group_expand: detail normal attributes are unsupported";
      if parameters.use_normal_attribute && String.trim parameters.normal_name = "" then
        invalid_arg "sop/group_expand: empty normal attribute name";
      if parameters.use_collision && String.trim parameters.collision_group = "" then
        invalid_arg "sop/group_expand: empty collision group name";
      if parameters.use_collision && parameters.collision_contain
          && parameters.collision_owner = Rdk.Group_ops.Group_edges then
        invalid_arg "sop/group_expand: edge collision groups cannot contain growth";
      if parameters.owner = Rdk.Group_ops.Group_edges && String.trim parameters.step_attribute <> "" then
        invalid_arg "sop/group_expand: native edges do not own step attributes";
      if parameters.flood && parameters.steps < 0 then
        invalid_arg "sop/group_expand: flood fill cannot be combined with shrinking";
      if (parameters.normal_spread < Float.pi || parameters.use_normal_attribute
          || parameters.connectivity_attributes <> [] || parameters.use_collision)
          && parameters.owner <> Rdk.Group_ops.Group_points
          && parameters.owner <> Rdk.Group_ops.Group_primitives then
        invalid_arg "sop/group_expand: constraints require point or primitive groups";
      if (parameters.connectivity_attributes <> [] || parameters.use_collision)
          && parameters.owner = Rdk.Group_ops.Group_primitives
          && parameters.primitive_connectivity <> Rdk.Group_ops.Primitive_share_edges then
        invalid_arg "sop/group_expand: constrained primitives must share edges"]
    [@@sop.node_category "Group/Expand"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = optional_text parameters.name in
    let steps = parameters.steps in
    let flood = parameters.flood in
    let step_attribute = optional_text parameters.step_attribute in
    let primitive_connectivity = parameters.primitive_connectivity in
    let normal_spread = parameters.normal_spread in
    let use_normal_attribute = parameters.use_normal_attribute in
    let normal_owner = parameters.normal_owner in
    let normal_name = parameters.normal_name in
    let connectivity_attributes = parameters.connectivity_attributes in
    let connectivity_tolerance = parameters.connectivity_tolerance in
    let use_collision = parameters.use_collision in
    let collision_owner = parameters.collision_owner in
    let collision_group = parameters.collision_group in
    let collision_contain = parameters.collision_contain in
    let collision_allow_boundary = parameters.collision_allow_boundary in
    let owner = parameters.owner in
    let group = parameters.group in
    let normal_attribute = if use_normal_attribute then Some {
      Rdk.Group_ops.expand_normal_owner = normal_owner; expand_normal_name = normal_name } else None in
    let normal_spread = if normal_spread = Float.pi && not use_normal_attribute
      then None else Some normal_spread in
    let collision = if use_collision then Some {
      Rdk.Group_ops.expand_collision_owner = collision_owner;
      expand_collision_group = collision_group; expand_collision_contain = collision_contain;
      expand_collision_allow_boundary = collision_allow_boundary } else None in
    Node.Private.make_geometry ?label ~operation:"group_expand" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Group_ops.expand ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?name ~steps ~flood ?step_attribute
            ~primitive_connectivity ?normal_spread ?normal_attribute
            ~connectivity_attributes ~connectivity_tolerance ?collision
            ~owner ~group inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end
