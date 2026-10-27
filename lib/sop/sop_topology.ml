(* Topology nodes, declared once: the record is the editor schema, the
   factory and the typed [Sop] constructor. *)

open Support

(* ocamldep must see the PPX's [Sop.X] resolve inside this library. *)
module Sop = Support.Sop


module Edge_collapse = struct
  let position_parameter = Param.choice ~equal:( = ) [
      "First", Rdk.Fuse_reduce.First_position;
      "Least point", Rdk.Fuse_reduce.Least_point_position;
      "Greatest point", Rdk.Fuse_reduce.Greatest_point_position;
      "Average", Rdk.Fuse_reduce.Average_position;
      "Minimum", Rdk.Fuse_reduce.Minimum_position;
      "Maximum", Rdk.Fuse_reduce.Maximum_position;
      "Mode", Rdk.Fuse_reduce.Mode_position;
      "Median", Rdk.Fuse_reduce.Median_position;
      "Sum", Rdk.Fuse_reduce.Sum_position;
      "Sum squares", Rdk.Fuse_reduce.Sum_squares_position;
      "Root mean square", Rdk.Fuse_reduce.Root_mean_square_position;
      "Weighted average", Rdk.Fuse_reduce.Weighted_average_position;
      "Weighted sum", Rdk.Fuse_reduce.Weighted_sum_position;
      "Minimum weight", Rdk.Fuse_reduce.Minimum_weight_position;
      "Maximum weight", Rdk.Fuse_reduce.Maximum_weight_position;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Edge group"];
    connectivity_attribute : string [@sop.default ""]
      [@sop.label "Connectivity attribute"];
    position : Rdk.Fuse_reduce.position
      [@sop.default Rdk.Fuse_reduce.Average_position]
      [@sop.label "Position"] [@sop.kind position_parameter];
    remove_degenerate_primitives : bool [@sop.default true]
      [@sop.label "Remove degenerate primitives"] [@sop.folder "Cleanup"];
    recompute_point_normals : bool [@sop.default true]
      [@sop.label "Recompute point normals"] [@sop.folder "Cleanup"];
  } [@@sop.node_key "edge_collapse"] [@@sop.node_label "Edge Collapse"]
    [@@sop.node_category "Topology/Edge"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let connectivity_attribute = optional_text
          parameters.connectivity_attribute in
    let position = parameters.position in
    let remove_degenerate_primitives = parameters.remove_degenerate_primitives in
    let recompute_point_normals = parameters.recompute_point_normals in
    Node.Private.make_geometry ?label ~operation:"edge_collapse" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let edges = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some edges -> Ok (Some edges)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "edge_collapse could not find native edge group %S" name))) in
        match edges with
        | Error error -> Error error
        | Ok edges ->
            match Rdk.Edge_collapse.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?edges ?connectivity_attribute
                ~position
                ~remove_degenerate_primitives ~recompute_point_normals geometry with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Dissolve = struct
  let operation_parameter = Param.choice ~equal:( = ) [
      "Selected", Rdk.Dissolve.Dissolve_selected;
      "Non-selected", Rdk.Dissolve.Dissolve_non_selected;
    ]

  let bridge_parameter = Param.choice ~equal:( = ) [
      "Create bridged polygons", Rdk.Dissolve.Create_bridged_polygons;
      "Create disjoint polygons", Rdk.Dissolve.Create_disjoint_polygons;
      "Delete bridge polygons", Rdk.Dissolve.Delete_bridge_polygons;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Edge group"];
    operation : Rdk.Dissolve.operation
      [@sop.default Rdk.Dissolve.Dissolve_selected]
      [@sop.label "Operation"] [@sop.kind operation_parameter];
    bridge_policy : Rdk.Dissolve.bridge_policy
      [@sop.default Rdk.Dissolve.Create_bridged_polygons]
      [@sop.label "Bridge loops"] [@sop.kind bridge_parameter];
    remove_inline_points : bool [@sop.default true]
      [@sop.label "Remove inline points"] [@sop.folder "Cleanup"] ;
    collinearity_tolerance : float [@sop.default 1e-6]
      [@sop.label "Collinearity tolerance"] [@sop.folder "Cleanup"]
      [@sop.min 0.] [@sop.max 0.1] [@sop.hard_min 0.] ;
    remove_unused_points : bool [@sop.default true]
      [@sop.label "Remove unused points"] [@sop.folder "Cleanup"];
    create_boundary_curves : bool [@sop.default false]
      [@sop.label "Create boundary curves"];
    recompute_normals : bool [@sop.default true]
      [@sop.label "Recompute normals"] [@sop.folder "Cleanup"];
  } [@@sop.node_key "dissolve"] [@@sop.node_label "Dissolve"]
    [@@sop.node_category "Topology"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let operation = parameters.operation in
    let bridge_policy = parameters.bridge_policy in
    let remove_inline_points = parameters.remove_inline_points in
    let collinearity_tolerance = parameters.collinearity_tolerance in
    let remove_unused_points = parameters.remove_unused_points in
    let create_boundary_curves = parameters.create_boundary_curves in
    let recompute_normals = parameters.recompute_normals in
    Node.Private.make_geometry ?label ~operation:"dissolve" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let edges = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name inputs.(0) with
               | Some value -> Ok (Some value)
               | None -> Error (Diagnostic.error ~code:"missing_edge_group"
                   (Printf.sprintf "dissolve could not find edge group %S" name))) in
        match edges with
        | Error error -> Error error
        | Ok edges ->
            match Rdk.Dissolve.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?edges ~operation ~bridge_policy
                ~remove_inline_points ~collinearity_tolerance
                ~remove_unused_points ~create_boundary_curves ~recompute_normals
                inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Triangulate = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
  } [@@sop.node_key "triangulate"] [@@sop.node_label "Triangulate"]
    [@@sop.node_category "Topology"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    Node.Private.make_geometry ?label ~operation:"triangulate" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "triangulate could not find primitive group %S" name))) in
        match primitives with
        | Error error -> Error error
        | Ok primitives ->
            match Rdk.Triangulate.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end




module Poly_extrude = struct
  let divide_parameter = Param.choice ~equal:( = ) [
      "Individual elements", Rdk.Poly_extrude.Extrude_individual;
      "Connected components", Rdk.Poly_extrude.Extrude_connected_components;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    split_edges : string [@sop.default ""] [@sop.label "Split edge group"];
    distance : float [@sop.default 0.1] [@sop.label "Distance"]
      [@sop.min (-10.)] [@sop.max 10.];
    divide : Rdk.Poly_extrude.divide
      [@sop.default Rdk.Poly_extrude.Extrude_individual]
      [@sop.label "Divide into"] [@sop.kind divide_parameter];
    divisions : int [@sop.default 1] [@sop.label "Divisions"]
      [@sop.min 1] [@sop.max 64] [@sop.hard_min 1];
    output_front : bool [@sop.default true] [@sop.label "Output front"]
      [@sop.folder "Output"];
    output_back : bool [@sop.default true] [@sop.label "Output back"]
      [@sop.folder "Output"];
    output_side : bool [@sop.default true] [@sop.label "Output side"]
      [@sop.folder "Output"];
    front_group : string [@sop.default ""] [@sop.label "Front group"]
      [@sop.folder "Groups"];
    back_group : string [@sop.default ""] [@sop.label "Back group"]
      [@sop.folder "Groups"];
    side_group : string [@sop.default ""] [@sop.label "Side group"]
      [@sop.folder "Groups"];
    front_boundary_group : string [@sop.default ""]
      [@sop.label "Front boundary group"] [@sop.folder "Groups"];
    back_boundary_group : string [@sop.default ""]
      [@sop.label "Back boundary group"] [@sop.folder "Groups"];
  } [@@sop.node_key "poly_extrude"] [@@sop.node_label "Poly Extrude"]
    [@@sop.node_category "Topology/Polygon"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let split_edges = optional_text parameters.split_edges in
    let divide = parameters.divide in
    let divisions = parameters.divisions in
    let output_front = parameters.output_front in
    let output_back = parameters.output_back in
    let output_side = parameters.output_side in
    let front_group = optional_text parameters.front_group in
    let back_group = optional_text parameters.back_group in
    let side_group = optional_text parameters.side_group in
    let front_boundary_group = optional_text parameters.front_boundary_group in
    let back_boundary_group = optional_text parameters.back_boundary_group in
    let distance = parameters.distance in
    Node.Private.make_geometry ?label ~operation:"poly_extrude" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_extrude could not find primitive group %S" name))) in
        let split = match split_edges with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_extrude could not find edge split group %S" name))) in
        match primitives, split with
        | Error error, _ | _, Error error -> Error error
        | Ok primitives, Ok split_edges ->
            match Rdk.Poly_extrude.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ?split_edges ~divide
                ~divisions ~output_front ~output_back ~output_side ?front_group
                ?back_group ?side_group ?front_boundary_group
                ?back_boundary_group ~distance geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Poly_fill = struct
  let mode_parameter = Param.choice ~equal:( = ) [
      "Single polygon", Rdk.Poly_fill.Fill_single_polygon;
      "Triangles", Rdk.Poly_fill.Fill_triangles;
      "Triangle fan", Rdk.Poly_fill.Fill_triangle_fan;
    ]

  type parameters = {
    boundary_group : string [@sop.default ""] [@sop.label "Boundary group"];
    mode : Rdk.Poly_fill.mode [@sop.default Rdk.Poly_fill.Fill_triangles]
      [@sop.label "Fill mode"] [@sop.kind mode_parameter];
    reverse_patches : bool [@sop.default false]
      [@sop.label "Reverse patches"];
    unique_points : bool [@sop.default false] [@sop.label "Unique points"];
    update_point_normals : bool [@sop.default false]
      [@sop.label "Update point normals"];
    patch_group : string [@sop.default ""] [@sop.label "Patch group"];
  } [@@sop.node_key "poly_fill"] [@@sop.node_label "Poly Fill"]
    [@@sop.node_category "Topology/Polygon"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let boundary_group = optional_text parameters.boundary_group in
    let mode = parameters.mode in
    let reverse_patches = parameters.reverse_patches in
    let unique_points = parameters.unique_points in
    let update_point_normals = parameters.update_point_normals in
    let patch_group = optional_text parameters.patch_group in
    Node.Private.make_geometry ?label ~operation:"poly_fill" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let boundary = match boundary_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_fill could not find boundary edge group %S" name))) in
        match boundary with
        | Error error -> Error error
        | Ok boundary ->
            match Rdk.Poly_fill.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?boundary ~mode ~reverse_patches
                ~unique_points ~update_point_normals ?patch_group geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end









module Remesh = struct
  type parameters = {
    target_length : float [@sop.default 0.1] [@sop.label "Target length"]
      [@sop.min 0.0001] [@sop.max 10.] [@sop.hard_min 0.];
    iterations : int [@sop.default 3] [@sop.label "Iterations"]
      [@sop.min 1] [@sop.max 32] [@sop.hard_min 0];
    smoothing : float [@sop.default 0.5] [@sop.label "Smoothing"]
      [@sop.min 0.] [@sop.max 1.];
    project : bool [@sop.default true] [@sop.label "Project to surface"];
    use_input_points_only : bool [@sop.default false]
      [@sop.label "Use input points only"];
    hard_point_group : string [@sop.default ""] [@sop.label "Hard points"]
      [@sop.folder "Constraints"];
    hard_edge_group : string [@sop.default ""] [@sop.label "Hard edges"]
      [@sop.folder "Constraints"];
    target_size_attribute : string [@sop.default ""]
      [@sop.label "Target size attribute"] [@sop.folder "Adaptivity"];
    preserve_uv_seams : bool [@sop.default true]
      [@sop.label "Preserve UV seams"] [@sop.folder "UV"];
    uv_attribute : string [@sop.default "uv"] [@sop.label "UV attribute"]
      [@sop.folder "UV"];
    output_hard_edges : string [@sop.default ""]
      [@sop.label "Output hard edges"] [@sop.folder "Output"];
    output_mesh_size : string [@sop.default ""]
      [@sop.label "Output mesh size"] [@sop.folder "Output"];
    output_quality : string [@sop.default ""]
      [@sop.label "Output quality"] [@sop.folder "Output"];
    recompute_point_normals : bool [@sop.default true]
      [@sop.label "Recompute point normals"] [@sop.folder "Output"];
  } [@@sop.node_key "remesh"] [@@sop.node_label "Remesh"]
    [@@sop.node_category "Topology/Remesh"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      if parameters.target_length <= 0. then
        invalid_arg "sop/remesh: target length must be finite and positive";
      if parameters.smoothing < 0. || parameters.smoothing > 1. then
        invalid_arg "sop/remesh: smoothing must be finite and within [0,1]";
      if parameters.preserve_uv_seams && String.trim parameters.uv_attribute = "" then
        invalid_arg "sop/remesh: empty UV attribute name"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let iterations = parameters.iterations in
    let smoothing = parameters.smoothing in
    let project = parameters.project in
    let use_input_points_only = parameters.use_input_points_only in
    let hard_point_group = optional_text parameters.hard_point_group in
    let hard_edge_group = optional_text parameters.hard_edge_group in
    let target_size_attribute = optional_text parameters.target_size_attribute in
    let preserve_uv_seams = parameters.preserve_uv_seams in
    let uv_attribute = parameters.uv_attribute in
    let output_hard_edges = optional_text parameters.output_hard_edges in
    let output_mesh_size = optional_text parameters.output_mesh_size in
    let output_quality = optional_text parameters.output_quality in
    let recompute_point_normals = parameters.recompute_point_normals in
    let target_length = parameters.target_length in
    Node.Private.make_geometry ?label ~operation:"remesh" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_point_group "remesh" hard_point_group geometry with
        | Error error -> Error error
        | Ok hard_points ->
            (match resolve_optional_edge_group "remesh" hard_edge_group geometry with
             | Error error -> Error error
             | Ok hard_edges ->
                 match Rdk.Remesh.run ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ~iterations ~smoothing ~project
                     ~use_input_points_only ?hard_points ?hard_edges
                     ?target_size_attribute ~preserve_uv_seams ~uv_attribute
                     ?output_hard_edges ?output_mesh_size ?output_quality
                     ~recompute_point_normals ~target_length geometry with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )

  let factory = parameters_factory build
end







module Convex_hull = struct
  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    preserve_point_payload : bool [@sop.default true]
      [@sop.label "Preserve point payload"];
    source_point_attribute : string [@sop.default "sourcepoint"]
      [@sop.label "Source point attribute"] [@sop.folder "Output"];
    hull_group : string [@sop.default "hull"] [@sop.label "Hull group"]
      [@sop.folder "Output"];
  } [@@sop.node_key "convex_hull"] [@@sop.node_label "Convex Hull"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.source_point_attribute = "P" then
        invalid_arg "sop/convex_hull: source point attribute must not be P"]
    [@@sop.node_category "Topology"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let preserve_point_payload = parameters.preserve_point_payload in
    let source_point_attribute = parameters.source_point_attribute in
    let hull_group = parameters.hull_group in
    let selection = optional_element_group group_owner group in
    let source_point_attribute = optional_text source_point_attribute in
    let hull_group = optional_text hull_group in
    Node.Private.make_geometry ?label ~operation:"convex_hull" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"convex_hull" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Convex_hull.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~preserve_point_payload
                ?source_point_attribute ?hull_group inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end





module Clean = struct
  let overlaps_parameter = Param.choice ~equal:( = ) [
      "Keep first", Clean_keep_first;
      "Delete pairs", Clean_delete_pairs;
      "Auto", Clean_overlap_auto;
    ]

  type parameters = {
    epsilon_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Epsilon mode"] [@sop.folder "Robustness"] [@sop.kind kernel_mode_parameter];
    epsilon : float [@sop.default 1e-9] [@sop.label "Epsilon"]
      [@sop.folder "Robustness"] [@sop.min 0.] [@sop.max 0.001]
      [@sop.hard_min 0.];
    remove_degenerate : bool [@sop.default true]
      [@sop.label "Remove degenerate primitives"];
    consolidate_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Consolidate mode"] [@sop.kind kernel_mode_parameter];
    consolidate_distance : float [@sop.default 0.]
      [@sop.label "Consolidate distance"] [@sop.min 0.] [@sop.max 0.1]
      [@sop.hard_min 0.];
    overlaps : Support.clean_overlap
      [@sop.default Clean_keep_first]
      [@sop.label "Overlaps"] [@sop.kind overlaps_parameter];
    reverse_winding : bool [@sop.default false]
      [@sop.label "Reverse winding"];
    remove_nan_points : bool [@sop.default true]
      [@sop.label "Remove non-finite points"];
    remove_unused_points : bool [@sop.default true]
      [@sop.label "Remove unused points"];
    delete_unused_groups : bool [@sop.default true]
      [@sop.label "Delete unused groups"];
    point_attributes : string [@sop.default ""]
      [@sop.label "Point attributes"] [@sop.folder "Delete attributes"];
    vertex_attributes : string [@sop.default ""]
      [@sop.label "Vertex attributes"] [@sop.folder "Delete attributes"];
    primitive_attributes : string [@sop.default ""]
      [@sop.label "Primitive attributes"] [@sop.folder "Delete attributes"];
    detail_attributes : string [@sop.default ""]
      [@sop.label "Detail attributes"] [@sop.folder "Delete attributes"];
    point_groups : string [@sop.default ""] [@sop.label "Point groups"]
      [@sop.folder "Delete groups"];
    vertex_groups : string [@sop.default ""] [@sop.label "Vertex groups"]
      [@sop.folder "Delete groups"];
    primitive_groups : string [@sop.default ""] [@sop.label "Primitive groups"]
      [@sop.folder "Delete groups"];
    edge_groups : string [@sop.default ""] [@sop.label "Edge groups"]
      [@sop.folder "Delete groups"];
  } [@@sop.node_key "clean"] [@@sop.node_label "Clean"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.epsilon) || parameters.epsilon < 0.
          || not (Float.is_finite parameters.consolidate_distance)
          || parameters.consolidate_distance < 0. then
        invalid_arg "sop/clean: epsilon and consolidate distance must be finite and nonnegative";
      List.iter (fun pattern -> if String.trim pattern <> "" then
        match Rdk.Attribute_pattern.compile pattern with
        | Ok _ -> () | Error message -> invalid_arg ("sop/clean: " ^ message))
        [parameters.point_attributes; parameters.vertex_attributes;
         parameters.primitive_attributes; parameters.detail_attributes;
         parameters.point_groups; parameters.vertex_groups;
         parameters.primitive_groups; parameters.edge_groups]]
    [@@sop.node_category "Topology/Cleanup"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let epsilon_mode = parameters.epsilon_mode in
    let epsilon = parameters.epsilon in
    let remove_degenerate = parameters.remove_degenerate in
    let consolidate_mode = parameters.consolidate_mode in
    let consolidate_distance = parameters.consolidate_distance in
    let overlaps = parameters.overlaps in
    let reverse_winding = parameters.reverse_winding in
    let remove_nan_points = parameters.remove_nan_points in
    let remove_unused_points = parameters.remove_unused_points in
    let delete_unused_groups = parameters.delete_unused_groups in
    let point_attributes = optional_text parameters.point_attributes in
    let vertex_attributes = optional_text parameters.vertex_attributes in
    let primitive_attributes = optional_text parameters.primitive_attributes in
    let detail_attributes = optional_text parameters.detail_attributes in
    let point_groups = optional_text parameters.point_groups in
    let vertex_groups = optional_text parameters.vertex_groups in
    let primitive_groups = optional_text parameters.primitive_groups in
    let edge_groups = optional_text parameters.edge_groups in
    let epsilon = if epsilon_mode = Kernel_auto then None else Some epsilon in
    let consolidate_distance = if consolidate_mode = Kernel_auto then None else Some consolidate_distance in
    let overlaps = match overlaps with
      | Clean_overlap_auto -> None
      | Clean_keep_first -> Some Rdk.Clean.Keep_first_overlap
      | Clean_delete_pairs -> Some Rdk.Clean.Delete_overlap_pairs in
    Node.Private.make_geometry ?label ~operation:"clean" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Clean.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?epsilon ~remove_degenerate
            ?consolidate_distance ?overlaps ~reverse_winding ~remove_nan_points
            ~remove_unused_points ~delete_unused_groups ?point_attributes
            ?vertex_attributes ?primitive_attributes ?detail_attributes
            ?point_groups ?vertex_groups ?primitive_groups ?edge_groups inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Join_curves = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    picked_ends : string [@sop.default ""] [@sop.label "Picked ends"]
    [@sop.description "Comma-separated primitive:start or primitive:end pairs, in traversal order; [] selects no curves."];
    orient_closest : bool [@sop.default true]
      [@sop.label "Orient closest ends"];
    connect_closest_ends : bool [@sop.default false]
      [@sop.label "Connect closest ends"];
    only_connected : bool [@sop.default false]
      [@sop.label "Only connected"];
    use_group_size : bool [@sop.default false] [@sop.label "Use group size"];
    group_size : int [@sop.default 2] [@sop.label "Group size"]
      [@sop.min 1] [@sop.max 1024] [@sop.hard_min 1];
    keep_originals : bool [@sop.default false]
      [@sop.label "Keep originals"];
    tolerance : float [@sop.default 0.] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    wrap : bool [@sop.default false] [@sop.label "Wrap"];
  } [@@sop.node_key "join_curves"] [@@sop.node_label "Join Curves"]
    [@@sop.validate fun parameters ->
      if parameters.group_size < 1 then invalid_arg "sop/join_curves: group size must be positive";
      if not (Float.is_finite parameters.tolerance) || parameters.tolerance < 0. then
        invalid_arg "sop/join_curves: tolerance must be finite and nonnegative";
      match decode_curve_join_picks parameters.picked_ends with
      | None -> ()
      | Some _ ->
          if String.trim parameters.group <> "" || parameters.connect_closest_ends then
            invalid_arg "sop/join_curves: picked ends are mutually exclusive with group and closest ordering"]
    [@@sop.node_category "Topology/Curve"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let picked_ends = parameters.picked_ends in
    let orient_closest = parameters.orient_closest in
    let connect_closest_ends = parameters.connect_closest_ends in
    let only_connected = parameters.only_connected in
    let use_group_size = parameters.use_group_size in
    let group_size = parameters.group_size in
    let keep_originals = parameters.keep_originals in
    let tolerance = parameters.tolerance in
    let wrap = parameters.wrap in
    let group = optional_text group in
    let picked_ends = decode_curve_join_picks picked_ends in
    let group_size = if use_group_size then Some group_size else None in
    Node.Private.make_geometry ?label ~operation:"join_curves" ~version:4
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive
                  name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   ~hints:["Create the primitive group before Curve Join"]
                   (Printf.sprintf "join_curves could not find primitive group %S"
                     name))) in
        match primitives with
        | Error _ as error -> error
        | Ok primitives ->
            match Rdk.Curve_topology.join_curves ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ?picked_ends ~orient_closest
                ~connect_closest_ends ~only_connected ?group_size ~keep_originals
                ~tolerance ~wrap inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end


module Facet = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    group_owner : Support.element_owner [@sop.default Element_primitive]
      [@sop.label "Group owner"] [@sop.kind element_owner_parameter];
    consolidation : Support.facet_consolidation [@sop.default Consolidation_none]
      [@sop.label "Consolidation"] [@sop.folder "Consolidate"] [@sop.kind facet_consolidation_parameter];
    pre_compute_normals : bool [@sop.default false]
      [@sop.label "Pre-compute normals"] [@sop.folder "Normals"];
    make_normals_unit_length : bool [@sop.default false]
      [@sop.label "Make normals unit length"] [@sop.folder "Normals"];
    unique_points : bool [@sop.default false] [@sop.label "Unique points"];
    consolidate_distance : float [@sop.default 0.]
      [@sop.label "Consolidate distance"] [@sop.folder "Consolidate"]
      [@sop.min 0.] [@sop.max 0.1] [@sop.hard_min 0.];
    consolidate_normals_distance : float [@sop.default 0.]
      [@sop.label "Normal distance"] [@sop.folder "Consolidate"]
      [@sop.min 0.] [@sop.max 0.1] [@sop.hard_min 0.];
    remove_inline_points : bool [@sop.default false]
      [@sop.label "Remove inline points"];
    inline_distance : float [@sop.default 1e-6]
      [@sop.label "Inline distance"] [@sop.min 0.] [@sop.max 0.1]
      [@sop.hard_min 0.];
    orient_polygons : bool [@sop.default false]
      [@sop.label "Orient polygons"];
    cusp_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Cusp mode"] [@sop.folder "Normals"] [@sop.kind kernel_mode_parameter];
    cusp_angle : float [@sop.default 3.141592653589793]
      [@sop.label "Cusp angle"] [@sop.folder "Normals"]
      [@sop.min 0.] [@sop.max 3.141592653589793]
      [@sop.hard_min 0.] [@sop.hard_max 3.141592653589793];
    remove_degenerate : bool [@sop.default false]
      [@sop.label "Remove degenerate primitives"];
    make_planar : bool [@sop.default false] [@sop.label "Make planar"];
    post_compute_normals : bool [@sop.default false]
      [@sop.label "Post-compute normals"] [@sop.folder "Normals"];
    reverse_normals : bool [@sop.default false]
      [@sop.label "Reverse normals"] [@sop.folder "Normals"];
  } [@@sop.node_key "facet"] [@@sop.node_label "Facet"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/facet: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.consolidate_distance;
          parameters.consolidate_normals_distance;parameters.inline_distance;parameters.cusp_angle]) then
        refuse "distance and angle controls must be finite";
      if parameters.consolidate_distance < 0. || parameters.consolidate_normals_distance < 0.
          || parameters.inline_distance < 0. then refuse "distances must be nonnegative";
      if parameters.cusp_angle < 0. || parameters.cusp_angle > Float.pi then refuse "cusp angle must be within [0, pi]"]
    [@@sop.node_category "Topology/Cleanup"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let group_owner = parameters.group_owner in
    let pre_compute_normals = parameters.pre_compute_normals in
    let make_normals_unit_length = parameters.make_normals_unit_length in
    let unique_points = parameters.unique_points in
    let consolidation = parameters.consolidation in
    let consolidate_distance = parameters.consolidate_distance in
    let consolidate_normals_distance = parameters.consolidate_normals_distance in
    let remove_inline_points = parameters.remove_inline_points in
    let inline_distance = parameters.inline_distance in
    let orient_polygons = parameters.orient_polygons in
    let cusp_mode = parameters.cusp_mode in
    let cusp_angle = parameters.cusp_angle in
    let remove_degenerate = parameters.remove_degenerate in
    let make_planar = parameters.make_planar in
    let post_compute_normals = parameters.post_compute_normals in
    let reverse_normals = parameters.reverse_normals in
    let selection = optional_element_group group_owner group in
    let consolidate_distance = match consolidation with Consolidation_points -> Some consolidate_distance | _ -> None in
    let consolidate_normals_distance = match consolidation with Consolidation_normals -> Some consolidate_normals_distance | _ -> None in
    let cusp_angle = match cusp_mode with Kernel_explicit -> Some cusp_angle | Kernel_auto -> None in
    Node.Private.make_geometry ?label ~operation:"facet" ~version:4
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_element_group ~operation:"facet" selection geometry with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Facet.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~pre_compute_normals
                ~make_normals_unit_length ~unique_points ?consolidate_distance
                ?consolidate_normals_distance ~remove_inline_points
                ~inline_distance ~orient_polygons ?cusp_angle ~remove_degenerate
                ~make_planar ~post_compute_normals ~reverse_normals geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Poly_cut = struct
  let element_parameter = Param.choice ~equal:( = ) [
      "Points", Rdk.Poly_cut.Poly_cut_points; "Edges", Rdk.Poly_cut.Poly_cut_edges;
    ]
  let strategy_parameter = Param.choice ~equal:( = ) [
      "Remove", Rdk.Poly_cut.Poly_cut_remove; "Cut", Rdk.Poly_cut.Poly_cut_cut;
    ]
  let detection_parameter = Param.choice ~equal:( = ) [
      "All selected", Cut_all; "Attribute crossing", Cut_crossing;
      "Attribute change", Cut_change;
    ]
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    cut_group : string [@sop.default ""] [@sop.label "Cut group"];
    element : Rdk.Poly_cut.element [@sop.default Rdk.Poly_cut.Poly_cut_points]
      [@sop.label "Cut elements"] [@sop.kind element_parameter];
    strategy : Rdk.Poly_cut.strategy
      [@sop.default Rdk.Poly_cut.Poly_cut_remove]
      [@sop.label "Strategy"] [@sop.kind strategy_parameter];
    detection : Support.poly_cut_detection [@sop.default Cut_all] [@sop.label "Detection"]
      [@sop.folder "Detection"] [@sop.kind detection_parameter];
    attribute : string [@sop.default "cut"] [@sop.label "Attribute"]
      [@sop.folder "Detection"];
    value : float [@sop.default 0.5] [@sop.label "Crossing value"]
      [@sop.folder "Detection"] [@sop.min (-10.)] [@sop.max 10.];
    threshold : float [@sop.default 0.] [@sop.label "Change threshold"]
      [@sop.folder "Detection"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    keep_closed : bool [@sop.default true] [@sop.label "Keep closed"];
  } [@@sop.node_key "poly_cut"] [@@sop.node_label "Poly Cut"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/poly_cut: " ^ message) in
      if not (Float.is_finite parameters.value && Float.is_finite parameters.threshold) then refuse "detection controls must be finite";
      if parameters.threshold < 0. then refuse "threshold must be nonnegative";
      if parameters.detection <> Cut_all && String.trim parameters.attribute = "" then refuse "attribute must be nonblank";
      if parameters.detection = Cut_crossing && parameters.attribute = "P" then refuse "crossing requires scalar storage";
      if parameters.detection = Cut_change && parameters.strategy = Rdk.Poly_cut.Poly_cut_cut && parameters.threshold = 0. then
        refuse "cut-at-change requires a positive threshold"]
    [@@sop.node_category "Topology/Curve"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let cut_group = parameters.cut_group in
    let element = parameters.element in
    let strategy = parameters.strategy in
    let detection = parameters.detection in
    let attribute = parameters.attribute in
    let value = parameters.value in
    let threshold = parameters.threshold in
    let keep_closed = parameters.keep_closed in
    let group = optional_text group and cut_group = optional_text cut_group in
    let detection = match detection with
      | Cut_all -> Rdk.Poly_cut.Poly_cut_all
      | Cut_crossing -> Rdk.Poly_cut.Poly_cut_crossing {attribute;value}
      | Cut_change -> Rdk.Poly_cut.Poly_cut_change {attribute;threshold} in
    Node.Private.make_geometry ?label ~operation:"poly_cut" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_primitive_group "poly_cut" group geometry with
        | Error error -> Error error
        | Ok primitives ->
            let cut_selection = match element with
              | Rdk.Poly_cut.Poly_cut_points ->
                  Result.map (fun value -> `Points value)
                    (resolve_optional_point_group "poly_cut" cut_group geometry)
              | Rdk.Poly_cut.Poly_cut_edges ->
                  Result.map (fun value -> `Edges value)
                    (resolve_optional_edge_group "poly_cut" cut_group geometry) in
            match cut_selection with
            | Error error -> Error error
            | Ok (`Points cut_points) ->
                (match Rdk.Poly_cut.cut ~cancel:(Context.cancel_token context)
                    ~grain:(Context.grain context) ?primitives ?cut_points
                    ~element ~strategy ~detection ~keep_closed geometry with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error)
            | Ok (`Edges cut_edges) ->
                (match Rdk.Poly_cut.cut ~cancel:(Context.cancel_token context)
                    ~grain:(Context.grain context) ?primitives ?cut_edges
                    ~element ~strategy ~detection ~keep_closed geometry with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Poly_reduce = struct
  let target_parameter = Param.choice ~equal:( = ) [
      "Percentage", Reduce_ratio; "Primitive count", Reduce_primitive_count;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    hard_point_group : string [@sop.default ""]
      [@sop.label "Hard point group"] [@sop.folder "Constraints"];
    hard_edge_group : string [@sop.default ""]
      [@sop.label "Hard edge group"] [@sop.folder "Constraints"];
    target_mode : Support.poly_reduce_target [@sop.default Reduce_ratio]
      [@sop.label "Target"] [@sop.kind target_parameter];
    ratio : float [@sop.default 0.5] [@sop.label "Percentage"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    primitive_count : int [@sop.default 100] [@sop.label "Primitive count"]
      [@sop.min 0] [@sop.max 1000000] [@sop.hard_min 0];
    preserve_boundary : bool [@sop.default true]
      [@sop.label "Preserve boundary"];
    only_original_positions : bool [@sop.default false]
      [@sop.label "Only original positions"];
    equalize_lengths : float [@sop.default 0.0000000001]
      [@sop.label "Equalize edge lengths"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    limit_normal_deviation : bool [@sop.default false]
      [@sop.label "Limit normal deviation"];
    max_normal_deviation : float [@sop.default 0.5]
      [@sop.label "Maximum normal deviation"] [@sop.min 0.]
      [@sop.max 3.141592653589793] [@sop.hard_min 0.];
    output_group : string [@sop.default ""] [@sop.label "Output group"];
    recompute_point_normals : bool [@sop.default true]
      [@sop.label "Recompute point normals"];
  } [@@sop.node_key "poly_reduce"] [@@sop.node_label "PolyReduce"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/poly_reduce: " ^ message) in
      if not (Float.is_finite parameters.ratio && Float.is_finite parameters.equalize_lengths
          && Float.is_finite parameters.max_normal_deviation) then refuse "controls must be finite";
      if parameters.ratio < 0. || parameters.ratio > 1. then refuse "ratio must be in [0, 1]";
      if parameters.primitive_count < 0 then refuse "primitive count must be nonnegative";
      if parameters.equalize_lengths < 0. then refuse "equalize lengths must be nonnegative";
      if parameters.max_normal_deviation < 0. || parameters.max_normal_deviation > Float.pi then
        refuse "normal deviation must be in [0, pi]"]
    [@@sop.node_category "Topology/Remesh"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let hard_point_group = parameters.hard_point_group in
    let hard_edge_group = parameters.hard_edge_group in
    let target_mode = parameters.target_mode in
    let ratio = parameters.ratio in
    let primitive_count = parameters.primitive_count in
    let preserve_boundary = parameters.preserve_boundary in
    let only_original_positions = parameters.only_original_positions in
    let equalize_lengths = parameters.equalize_lengths in
    let limit_normal_deviation = parameters.limit_normal_deviation in
    let max_normal_deviation = parameters.max_normal_deviation in
    let output_group = parameters.output_group in
    let recompute_point_normals = parameters.recompute_point_normals in
    let group = optional_text group and hard_point_group = optional_text hard_point_group
    and hard_edge_group = optional_text hard_edge_group and output_group = optional_text output_group in
    let target = match target_mode with
      | Reduce_ratio -> Rdk.Poly_reduce.Reduce_ratio ratio
      | Reduce_primitive_count -> Rdk.Poly_reduce.Reduce_primitive_count primitive_count in
    let max_normal_deviation = if limit_normal_deviation then Some max_normal_deviation else None in
    Node.Private.make_geometry ?label ~operation:"poly_reduce" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_reduce could not find primitive group %S" name))) in
        let hard_points = match hard_point_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_reduce could not find hard point group %S" name))) in
        let hard_edges = match hard_edge_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "poly_reduce could not find hard edge group %S" name))) in
        match primitives, hard_points, hard_edges with
        | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
        | Ok primitives, Ok hard_points, Ok hard_edges ->
            match Rdk.Poly_reduce.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~target ?primitives ?hard_points
                ?hard_edges ~preserve_boundary ~only_original_positions
                ~equalize_lengths ?max_normal_deviation ?output_group
                ~recompute_point_normals geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end


module Boolean_seam = struct
  let output_parameter = Param.choice ~equal:( = ) [
      "Seam curves", Rdk.Boolean.Seam_curves;
      "Coincident patches", Rdk.Boolean.Coincident_patches;
    ]
  let treatment_parameter = Param.choice ~equal:( = ) [
      "Solid", Rdk.Boolean.Solid; "Surface", Rdk.Boolean.Surface;
    ]

  type parameters = {
    output : Rdk.Boolean.seam_output [@sop.default Rdk.Boolean.Seam_curves]
      [@sop.label "Output"] [@sop.kind output_parameter];
    left_treatment : Rdk.Boolean.treatment [@sop.default Rdk.Boolean.Solid]
      [@sop.label "A treatment"] [@sop.folder "Operands"]
      [@sop.kind treatment_parameter];
    right_treatment : Rdk.Boolean.treatment [@sop.default Rdk.Boolean.Solid]
      [@sop.label "B treatment"] [@sop.folder "Operands"]
      [@sop.kind treatment_parameter];
    resolve_left_self_intersections : bool [@sop.default false]
      [@sop.label "Resolve A self-intersections"] [@sop.folder "Operands"];
    resolve_right_self_intersections : bool [@sop.default false]
      [@sop.label "Resolve B self-intersections"] [@sop.folder "Operands"];
    left_self_group : string [@sop.default "boolean_left_self_seam"]
      [@sop.label "A self group"] [@sop.folder "Groups"];
    between_group : string [@sop.default "boolean_seam"]
      [@sop.label "Between group"] [@sop.folder "Groups"];
    right_self_group : string [@sop.default "boolean_right_self_seam"]
      [@sop.label "B self group"] [@sop.folder "Groups"];
    coincident_group : string [@sop.default "boolean_coincident"]
      [@sop.label "Coincident group"] [@sop.folder "Groups"];
  } [@@sop.node_key "boolean_seam"] [@@sop.node_label "Boolean Seam"]

    [@@sop.validate fun parameters ->
      let names = match parameters.output with
        | Rdk.Boolean.Seam_curves -> [parameters.left_self_group;parameters.between_group;parameters.right_self_group]
        | Coincident_patches -> [parameters.coincident_group] in
      let names = List.filter_map optional_text names in
      if List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid_arg "sop/boolean_seam: output group names must be distinct"]
    [@@sop.node_category "Boolean/Analysis"] [@@sop.node_inputs 2] [@@sop.node_slots "left, right"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters left right ->
    let label = Some label in
    let output = parameters.output in
    let left_treatment = parameters.left_treatment in
    let right_treatment = parameters.right_treatment in
    let resolve_left_self_intersections = parameters.resolve_left_self_intersections in
    let resolve_right_self_intersections = parameters.resolve_right_self_intersections in
    let left_self_group = parameters.left_self_group in
    let between_group = parameters.between_group in
    let right_self_group = parameters.right_self_group in
    let coincident_group = parameters.coincident_group in
    let left_self_group = optional_text left_self_group and between_group = optional_text between_group
    and right_self_group = optional_text right_self_group and coincident_group = optional_text coincident_group in
    Node.Private.make_geometry ?label ~operation:"boolean_seam" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|left;right|] (fun ~node_id:_ context inputs ->
        match Rdk.Boolean.seam ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~output ~left_treatment
            ~right_treatment ~resolve_left_self_intersections
            ~resolve_right_self_intersections ~left_self_group ~between_group
            ~right_self_group ~coincident_group ~right:inputs.(1) inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Poly_bevel = struct

  let shape_parameter = Param.choice ~equal:( = ) [
      "Chamfer", Poly_chamfer; "Round", Poly_round;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Edge group"];
    shape : Support.poly_bevel_shape [@sop.default Poly_chamfer] [@sop.label "Shape"]
      [@sop.kind shape_parameter];
    convexity : float [@sop.default 0.5] [@sop.label "Convexity"]
      [@sop.folder "Round"] [@sop.min 0.] [@sop.max 1.];
    distance : float [@sop.default 0.1] [@sop.label "Distance"]
      [@sop.min 0.] [@sop.max 2.] [@sop.hard_min 0.];
    divisions : int [@sop.default 1] [@sop.label "Divisions"]
      [@sop.min 1] [@sop.max 32] [@sop.hard_min 1];
    point_scale_attribute : string [@sop.default ""]
      [@sop.label "Point scale attribute"] [@sop.folder "Attributes"];
    ignore_flat_angle : float [@sop.default 0.]
      [@sop.label "Ignore flat angle (0: bevel every edge)"] [@sop.folder "Robustness"]
      [@sop.min 0.] [@sop.max 3.14159] [@sop.hard_min 0.];
    clamp_overlap : bool [@sop.default true]
      [@sop.label "Clamp overlap"] [@sop.folder "Robustness"];
    edge_group : string [@sop.default ""] [@sop.label "Edge group"]
      [@sop.folder "Output groups"];
    corner_group : string [@sop.default ""] [@sop.label "Corner group"]
      [@sop.folder "Output groups"];
    offset_group : string [@sop.default ""] [@sop.label "Offset group"]
      [@sop.folder "Output groups"];
    recompute_point_normals : bool [@sop.default true]
      [@sop.label "Recompute point normals"];
  } [@@sop.node_key "poly_bevel"] [@@sop.node_label "Poly Bevel"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/poly_bevel: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.convexity;parameters.distance;parameters.ignore_flat_angle]) then
        refuse "numeric controls must be finite";
      if parameters.distance < 0. then refuse "distance must be nonnegative";
      if parameters.divisions < 1 then refuse "divisions must be positive";
      if parameters.convexity < -1. || parameters.convexity > 1. then refuse "convexity must be in [-1, 1]";
      if parameters.ignore_flat_angle < 0. || parameters.ignore_flat_angle > Float.pi then refuse "flatness angle must be in [0, pi]"]
    [@@sop.node_category "Topology/Polygon"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let shape = parameters.shape in
    let convexity = parameters.convexity in
    let distance = parameters.distance in
    let divisions = parameters.divisions in
    let point_scale_attribute = parameters.point_scale_attribute in
    let ignore_flat_angle = parameters.ignore_flat_angle in
    let clamp_overlap = parameters.clamp_overlap in
    let edge_group = parameters.edge_group in
    let corner_group = parameters.corner_group in
    let offset_group = parameters.offset_group in
    let recompute_point_normals = parameters.recompute_point_normals in
    let shape = match shape with Poly_chamfer -> Rdk.Poly_bevel.Bevel_chamfer
      | Poly_round -> Rdk.Poly_bevel.Bevel_round {convexity} in
    let group = optional_text group and point_scale_attribute = optional_text point_scale_attribute
    and edge_group = optional_text edge_group and corner_group = optional_text corner_group
    and offset_group = optional_text offset_group in
    let ignore_flat_angle = if ignore_flat_angle > 0. then Some ignore_flat_angle else None in
    Node.Private.make_geometry ?label ~operation:"poly_bevel" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let edges = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some value -> Ok (Some value)
               | None -> Error (Diagnostic.error ~code:"missing_edge_group"
                   (Printf.sprintf "poly_bevel could not find edge group %S" name))) in
        match edges with
        | Error error -> Error error
        | Ok edges ->
            match Rdk.Poly_bevel.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?edges ~shape ~divisions
                ?point_scale_attribute ?ignore_flat_angle ~clamp_overlap
                ?edge_group ?corner_group ?offset_group ~recompute_point_normals
                ~distance geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
  type shape = Chamfer | Round
end

module Boolean_fracture = struct
  let detriangulation_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Boolean.Triangles;
      "Unchanged polygons", Rdk.Boolean.Unchanged_polygons;
      "All polygons", Rdk.Boolean.All_polygons;
    ]

  let conflict_parameter = Param.choice ~equal:( = ) [
    "Reject conflict", Rdk.Boolean.Reject; "Promote to vertex", Rdk.Boolean.Promote_to_vertex;
  ]
  type parameters = {
    point_conflict : Rdk.Boolean.point_conflict [@sop.default Rdk.Boolean.Promote_to_vertex]
      [@sop.label "Point attribute conflicts"] [@sop.folder "Attributes"] [@sop.kind conflict_parameter];
    assume_flat : bool [@sop.default false] [@sop.label "Assume flat"] [@sop.folder "Output"];
    resolve_cutter_self_intersections : bool [@sop.default false]
      [@sop.label "Resolve cutter self-intersections"];
    detriangulation : Rdk.Boolean.detriangulation
      [@sop.default Rdk.Boolean.Triangles]
      [@sop.label "Polygons"] [@sop.kind detriangulation_parameter];
    require_closed : bool [@sop.default true] [@sop.label "Require closed"];
    piece_attribute : string [@sop.default "piece"]
      [@sop.label "Piece attribute"];
    point_tolerance : float [@sop.default 0.] [@sop.label "Point tolerance"]
      [@sop.folder "Robustness"] [@sop.min 0.] [@sop.max 0.001]
      [@sop.hard_min 0.];
    tiny_seam_threshold : float [@sop.default 0.]
      [@sop.label "Tiny seam threshold"] [@sop.folder "Robustness"]
      [@sop.min 0.] [@sop.max 0.001] [@sop.hard_min 0.];
    cleanup_max_batches : int [@sop.default 8]
      [@sop.label "Cleanup batches"] [@sop.folder "Robustness"]
      [@sop.min 1] [@sop.max 32] [@sop.hard_min 1];
    strict_cleanup : bool [@sop.default true] [@sop.label "Strict cleanup"]
      [@sop.folder "Robustness"];
  } [@@sop.node_key "boolean_fracture"] [@@sop.node_label "Boolean Fracture"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/boolean_fracture: " ^ message) in
      if not (Float.is_finite parameters.point_tolerance && Float.is_finite parameters.tiny_seam_threshold)
          || parameters.point_tolerance < 0. || parameters.tiny_seam_threshold < 0. then refuse "tolerances must be finite and nonnegative";
      if parameters.cleanup_max_batches < 1 then refuse "cleanup batches must be positive";
      if String.trim parameters.piece_attribute = "" then refuse "piece attribute must be nonblank"]
    [@@sop.node_operation "boolean"]
    [@@sop.node_category "Boolean"] [@@sop.node_inputs 2] [@@sop.node_slots "source, cutters"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source cutters ->
    let label = Some label in
    let resolve_cutter_self_intersections = parameters.resolve_cutter_self_intersections in
    let point_conflict = parameters.point_conflict in
    let point_tolerance = parameters.point_tolerance in
    let tiny_seam_threshold = parameters.tiny_seam_threshold in
    let cleanup_max_batches = parameters.cleanup_max_batches in
    let strict_cleanup = parameters.strict_cleanup in
    let detriangulation = parameters.detriangulation in
    let assume_flat = parameters.assume_flat in
    let require_closed = parameters.require_closed in
    let piece_attribute = parameters.piece_attribute in
    Node.Private.make_geometry ?label ~operation:"boolean" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs:[|source;cutters|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Boolean.run ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ~operation:Rdk.Boolean.Difference ~left_treatment:Rdk.Boolean.Solid ~right_treatment:Rdk.Boolean.Surface
            ~resolve_right_self_intersections:resolve_cutter_self_intersections ~point_conflict ~point_tolerance ~tiny_seam_threshold
            ~cleanup_max_batches ~strict_cleanup ~seam_points:Rdk.Boolean.Shared_seam_points ~detriangulation ~assume_flat
            ~require_closed ~piece_attribute ~right:inputs.(1) inputs.(0) with
        | Ok geometry -> cooked geometry | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Intersection_analysis = struct
  type parameters = {
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Inputs"];
    collision_group : string [@sop.default ""] [@sop.label "Collision group"]
      [@sop.folder "Inputs"];
    tolerance : float [@sop.default 0.] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.001] [@sop.hard_min 0.];
    include_coplanar : bool [@sop.default true]
      [@sop.label "Include coplanar overlap"];
    input_attribute : string [@sop.default "sourceinput"]
      [@sop.label "Input attribute"] [@sop.folder "Output attributes"];
    primitive_attribute : string [@sop.default "sourceprim"]
      [@sop.label "Primitive attribute"] [@sop.folder "Output attributes"];
    primitive_uvw_attribute : string [@sop.default "sourceprimuv"]
      [@sop.label "Primitive UVW attribute"] [@sop.folder "Output attributes"];
    point_attribute : string [@sop.default "sourcepoint"]
      [@sop.label "Point attribute"] [@sop.folder "Output attributes"];
  } [@@sop.node_key "intersection_analysis"]
    [@@sop.node_label "Intersection Analysis"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/intersection_analysis: " ^ message) in
      if not (Float.is_finite parameters.tolerance) || parameters.tolerance < 0. then refuse "tolerance must be finite and nonnegative";
      let names = List.filter_map optional_text [parameters.input_attribute;parameters.primitive_attribute;
        parameters.primitive_uvw_attribute;parameters.point_attribute] in
      if List.mem "P" names then refuse "output attributes must not replace P";
      if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "output attribute names must be distinct"]
    [@@sop.node_category "Boolean/Analysis"] [@@sop.node_inputs 2] [@@sop.node_slots "input, collision"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input collision ->
    let label = Some label in
    let source_group = parameters.source_group in
    let collision_group = parameters.collision_group in
    let tolerance = parameters.tolerance in
    let include_coplanar = parameters.include_coplanar in
    let input_attribute = parameters.input_attribute in
    let primitive_attribute = parameters.primitive_attribute in
    let primitive_uvw_attribute = parameters.primitive_uvw_attribute in
    let point_attribute = parameters.point_attribute in
    let source_group = optional_text source_group in
    let collision_group = if Option.is_some collision then optional_text collision_group else None in
    let input_attribute = optional_text input_attribute and primitive_attribute = optional_text primitive_attribute
    and primitive_uvw_attribute = optional_text primitive_uvw_attribute and point_attribute = optional_text point_attribute in
    let inputs = match collision with None -> [|input|]
      | Some collision -> [|input; collision|] in
    Node.Private.make_geometry ?label ~operation:"intersection_analysis" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        match resolve_optional_primitive_group "intersection_analysis"
            source_group inputs.(0) with
        | Error error -> Error error
        | Ok source_primitives ->
            let collision_geometry = if Array.length inputs = 1 then None
              else Some inputs.(1) in
            let group_geometry = match collision_geometry with
              | None -> inputs.(0) | Some geometry -> geometry in
            (match resolve_optional_primitive_group "intersection_analysis collision"
                collision_group group_geometry with
             | Error error -> Error error
             | Ok collision_primitives ->
                 match Rdk.Intersection_analysis.run
                     ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ?source_primitives
                     ?collision_primitives ~tolerance ~include_coplanar
                     ~input_attribute ~primitive_attribute
                     ~primitive_uvw_attribute ~point_attribute
                     ?collision:collision_geometry inputs.(0) with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Fuse = struct
  let targeting_parameter = Param.choice ~equal:( = ) [
      "Near points", Fuse_near_points; "Specified points", Fuse_specified_points;
    ]
  let using_parameter = Param.choice ~equal:( = ) [
      "Least target point", Rdk.Fuse_grid.Least_target_point;
      "Closest target point", Rdk.Fuse_grid.Closest_target_point;
    ]
  let position_parameter = Param.choice ~equal:( = ) [
      "First", Rdk.Fuse_reduce.First_position;
      "Least point", Rdk.Fuse_reduce.Least_point_position;
      "Greatest point", Rdk.Fuse_reduce.Greatest_point_position;
      "Average", Rdk.Fuse_reduce.Average_position;
      "Minimum", Rdk.Fuse_reduce.Minimum_position;
      "Maximum", Rdk.Fuse_reduce.Maximum_position;
      "Mode", Rdk.Fuse_reduce.Mode_position;
      "Median", Rdk.Fuse_reduce.Median_position;
      "Sum", Rdk.Fuse_reduce.Sum_position;
      "Sum squares", Rdk.Fuse_reduce.Sum_squares_position;
      "Root mean square", Rdk.Fuse_reduce.Root_mean_square_position;
      "Weighted average", Rdk.Fuse_reduce.Weighted_average_position;
      "Weighted sum", Rdk.Fuse_reduce.Weighted_sum_position;
      "Minimum weight", Rdk.Fuse_reduce.Minimum_weight_position;
      "Maximum weight", Rdk.Fuse_reduce.Maximum_weight_position;
    ]
  let attributes_parameter = Param.choice ~equal:( = ) [
      "Keep first", Rdk.Fuse_reduce.Keep_first;
      "Average numeric", Rdk.Fuse_reduce.Average_numeric;
    ]
  let metric_parameter = Param.choice ~equal:( = ) [
      "Euclidean", Rdk.Fuse_grid.Euclidean;
      "Componentwise", Rdk.Fuse_grid.Componentwise;
    ]
  let condition_parameter = Param.choice ~equal:( = ) [
      "Equal", Rdk.Fuse_grid.Equal_attribute_values;
      "Unequal", Rdk.Fuse_grid.Unequal_attribute_values;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    target_group : string [@sop.default ""] [@sop.label "Target group"];
    targeting : Support.fuse_targeting [@sop.default Fuse_near_points]
      [@sop.label "Targeting"] [@sop.kind targeting_parameter];
    target_attribute : string [@sop.default "targetpoint"]
      [@sop.label "Target point attribute"];
    using : Rdk.Fuse_grid.fuse_using [@sop.default Rdk.Fuse_grid.Least_target_point]
      [@sop.label "Use target"] [@sop.kind using_parameter];
    tolerance : float [@sop.default 0.001] [@sop.label "Snap distance"]
      [@sop.min 0.] [@sop.max 10.] [@sop.hard_min 0.];
    position : Rdk.Fuse_reduce.position [@sop.default Rdk.Fuse_reduce.Average_position]
      [@sop.label "Position"] [@sop.folder "Fuse"]
      [@sop.kind position_parameter];
    weight_attribute : string [@sop.default ""]
      [@sop.label "Weight attribute"] [@sop.folder "Fuse"];
    attributes : Rdk.Fuse_reduce.attributes [@sop.default Rdk.Fuse_reduce.Keep_first]
      [@sop.label "Attributes"] [@sop.folder "Fuse"]
      [@sop.kind attributes_parameter];

    attribute_rules : string [@sop.default ""] [@sop.label "Attribute rules"] [@sop.folder "Fuse"]
      [@sop.description "Escaped tab-separated rows: pattern, reduction method, weight attribute. Weighted methods require a weight name."];
    group_rules : string [@sop.default ""] [@sop.label "Group rules"] [@sop.folder "Fuse"]
      [@sop.description "Escaped tab-separated rows: group pattern, reduction method."];
    metric : Rdk.Fuse_grid.fuse_metric [@sop.default Rdk.Fuse_grid.Euclidean]
      [@sop.label "Metric"] [@sop.folder "Matching"]
      [@sop.kind metric_parameter];
    inclusive : bool [@sop.default true] [@sop.label "Inclusive distance"]
      [@sop.folder "Matching"];
    match_attributes : bool [@sop.default false]
      [@sop.label "Match attributes"] [@sop.folder "Matching"];
    radius_attribute : string [@sop.default ""]
      [@sop.label "Radius attribute"] [@sop.folder "Matching"];
    match_attribute : string [@sop.default ""]
      [@sop.label "Match attribute"] [@sop.folder "Matching"];
    match_condition : Rdk.Fuse_grid.fuse_match_condition
      [@sop.default Rdk.Fuse_grid.Equal_attribute_values]
      [@sop.label "Match condition"] [@sop.folder "Matching"]
      [@sop.kind condition_parameter];
    match_tolerance : float [@sop.default 0.] [@sop.label "Match tolerance"]
      [@sop.folder "Matching"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    modify_target : bool [@sop.default false] [@sop.label "Modify target"];
    fuse_points : bool [@sop.default true] [@sop.label "Fuse points"];
    keep_fused_points : bool [@sop.default false]
      [@sop.label "Keep fused points"];
    snapped_group : string [@sop.default ""] [@sop.label "Snapped group"]
      [@sop.folder "Output"];
    snapped_destination_attribute : string [@sop.default ""]
      [@sop.label "Destination attribute"] [@sop.folder "Output"];
    remove_degenerate_primitives : bool [@sop.default true]
      [@sop.label "Remove degenerate primitives"] [@sop.folder "Cleanup"];
    remove_unused_points_from_degenerate_primitives : bool [@sop.default true]
      [@sop.label "Remove newly unused points"] [@sop.folder "Cleanup"];
    remove_all_unused_points : bool [@sop.default false]
      [@sop.label "Remove all unused points"] [@sop.folder "Cleanup"];
  } [@@sop.node_key "fuse"] [@@sop.node_label "Fuse"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/fuse: " ^ message) in
      if not (Float.is_finite parameters.tolerance && Float.is_finite parameters.match_tolerance)
          || parameters.tolerance < 0. || parameters.match_tolerance < 0. then refuse "tolerances must be finite and nonnegative";
      if parameters.targeting = Fuse_specified_points && String.trim parameters.target_attribute = "" then refuse "specified targets require an attribute name";
      if String.trim parameters.match_attribute = "" && (parameters.match_condition <> Rdk.Fuse_grid.Equal_attribute_values
          || parameters.match_tolerance <> 0.) then refuse "match condition and tolerance require a match attribute";
      if parameters.targeting = Fuse_specified_points && (String.trim parameters.radius_attribute <> ""
          || String.trim parameters.match_attribute <> "") then refuse "specified targets do not accept radius or match attributes";
      if parameters.keep_fused_points && not parameters.fuse_points then refuse "keeping fused points requires fusing points";
      (match parameters.position with
        | Rdk.Fuse_reduce.Weighted_average_position | Weighted_sum_position | Minimum_weight_position | Maximum_weight_position
          when String.trim parameters.weight_attribute = "" -> refuse "weighted position reduction requires a weight attribute"
        | _ -> ());
      ignore (decode_fuse_attribute_rules "sop/fuse" parameters.attribute_rules);
      ignore (decode_fuse_group_rules "sop/fuse" parameters.group_rules)]
    [@@sop.node_category "Topology/Cleanup"] [@@sop.node_inputs 2] [@@sop.node_slots "input, target"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input target ->
    if parameters.modify_target && Option.is_some target then
      invalid_arg "sop/fuse: modify target requires a disconnected target input";
    let label = Some label in
    let group = parameters.group in
    let target_group = parameters.target_group in
    let targeting = parameters.targeting in
    let target_attribute = parameters.target_attribute in
    let using = parameters.using in
    let tolerance = parameters.tolerance in
    let position = parameters.position in
    let weight_attribute = parameters.weight_attribute in
    let attributes = parameters.attributes in
    let attribute_rules = parameters.attribute_rules in
    let group_rules = parameters.group_rules in
    let metric = parameters.metric in
    let inclusive = parameters.inclusive in
    let match_attributes = parameters.match_attributes in
    let radius_attribute = parameters.radius_attribute in
    let match_attribute = parameters.match_attribute in
    let match_condition = parameters.match_condition in
    let match_tolerance = parameters.match_tolerance in
    let modify_target = parameters.modify_target in
    let fuse_points = parameters.fuse_points in
    let keep_fused_points = parameters.keep_fused_points in
    let snapped_group = parameters.snapped_group in
    let snapped_destination_attribute = parameters.snapped_destination_attribute in
    let remove_degenerate_primitives = parameters.remove_degenerate_primitives in
    let remove_unused_points_from_degenerate_primitives = parameters.remove_unused_points_from_degenerate_primitives in
    let remove_all_unused_points = parameters.remove_all_unused_points in
    let targeting = match targeting with Fuse_near_points -> Rdk.Fuse_grid.Near_points
      | Fuse_specified_points -> Rdk.Fuse_grid.Specified_points target_attribute in
    let group = optional_text group and target_group = optional_text target_group
    and weight_attribute = optional_text weight_attribute and radius_attribute = optional_text radius_attribute
    and match_attribute = optional_text match_attribute and snapped_group = optional_text snapped_group
    and snapped_destination_attribute = optional_text snapped_destination_attribute in
    let attribute_rules = decode_fuse_attribute_rules "sop/fuse" attribute_rules
    and group_rules = decode_fuse_group_rules "sop/fuse" group_rules in
    let inputs = match target with None -> [|input|] | Some node -> [|input;node|] in
    Node.Private.make_geometry ?label ~operation:"fuse" ~version:6
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        match resolve_optional_point_group "fuse" group inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            let target_geometry = if Array.length inputs = 2
                then inputs.(1) else inputs.(0) in
            (match resolve_optional_point_group "fuse target" target_group
                target_geometry with
             | Error error -> Error error
             | Ok target_selection ->
                let target = if Array.length inputs = 2
                    then Some target_geometry else None in
                (match Rdk.Fuse_grid.fuse ~cancel:(Context.cancel_token context)
                    ~grain:(Context.grain context) ?selection ?target_selection
                    ~targeting ~using ~tolerance ~position ?weight_attribute
                    ~attributes ~attribute_rules ~group_rules ~metric
                    ~inclusive ~match_attributes ?radius_attribute
                    ?match_attribute ~match_condition ~match_tolerance
                    ~modify_target ~fuse_points ~keep_fused_points
                    ?snapped_group ?snapped_destination_attribute
                    ~remove_degenerate_primitives
                    ~remove_unused_points_from_degenerate_primitives
                    ~remove_all_unused_points
                    ?target inputs.(0) with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error)))
  )
  let factory = parameters_factory build
end


module Subdivide_cracks = struct
  let scheme_parameter = Param.choice ~equal:( = ) [
      "Catmull-Clark", Rdk.Subdivide.Catmull_clark;
      "Loop", Rdk.Subdivide.Loop;
      "Bilinear", Rdk.Subdivide.Bilinear;
    ]

  let cracks_parameter = Param.choice ~equal:( = ) [
      "Do not close", Cracks_do_not_close;
      "Pull, no edge division", Cracks_pull_no_division;
      "Pull, divide edges", Cracks_pull_divide;
      "Pull, triangulate", Cracks_pull_triangulate;
      "Stitch, no edge division", Cracks_stitch_no_division;
      "Stitch, divide edges", Cracks_stitch_divide;
      "Stitch, triangulate", Cracks_stitch_triangulate;
    ]

  let boundary_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Subdivide.Subdivide_boundary_none;
      "Edge only", Rdk.Subdivide.Subdivide_boundary_edge_only;
      "Edge and corner", Rdk.Subdivide.Subdivide_boundary_edge_and_corner;
    ]

  let fvar_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Subdivide.Subdivide_fvar_none;
      "Corners only", Rdk.Subdivide.Subdivide_fvar_corners_only;
      "Corners plus 1", Rdk.Subdivide.Subdivide_fvar_corners_plus1;
      "Corners plus 2", Rdk.Subdivide.Subdivide_fvar_corners_plus2;
      "Boundaries", Rdk.Subdivide.Subdivide_fvar_boundaries;
      "All", Rdk.Subdivide.Subdivide_fvar_all;
    ]

  let triangle_parameter = Param.choice ~equal:( = ) [
      "Catmull-Clark", Rdk.Subdivide.Subdivide_triangles_catmull_clark;
      "Smooth", Rdk.Subdivide.Subdivide_triangles_smooth;
    ]

  let creasing_parameter = Param.choice ~equal:( = ) [
      "Uniform", Rdk.Subdivide.Subdivide_creasing_uniform;
      "Chaikin", Rdk.Subdivide.Subdivide_creasing_chaikin;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    scheme : Rdk.Subdivide.scheme
      [@sop.default Rdk.Subdivide.Catmull_clark]
      [@sop.label "Scheme"] [@sop.kind scheme_parameter];
    iterations : int [@sop.default 1] [@sop.label "Depth"]
      [@sop.min 1] [@sop.max 6] [@sop.hard_min 1];
    cracks : Support.subdivision_cracks [@sop.default Cracks_do_not_close]
      [@sop.label "Close cracks"] [@sop.kind cracks_parameter];
    crack_bias : float [@sop.default 0.5] [@sop.label "Pull bias"]
      [@sop.folder "Cracks"] [@sop.min 0.] [@sop.max 1.];
    consistent_topology : bool [@sop.default false]
      [@sop.label "Consistent topology"] [@sop.folder "Cracks"];
    crease_group : string [@sop.default ""] [@sop.label "Crease group"]
      [@sop.folder "Creases"];

    crease_weight_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Crease weight mode"]
      [@sop.folder "Creases"] [@sop.kind kernel_mode_parameter];
    crease_weight : float [@sop.default 1.] [@sop.label "Crease weight"]
      [@sop.folder "Creases"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    generate_resulting_creases : bool [@sop.default false]
      [@sop.label "Generate resulting creases"] [@sop.folder "Creases"];
    resulting_crease_group : string [@sop.default ""]
      [@sop.label "Resulting crease group"] [@sop.folder "Creases"];
    hole_group : string [@sop.default "subdivision_hole"]
      [@sop.label "Hole group"] [@sop.folder "Holes"];
    remove_holes : bool [@sop.default false] [@sop.label "Remove holes"]
      [@sop.folder "Holes"];
    boundary_interpolation : Rdk.Subdivide.boundary_interpolation
      [@sop.default Rdk.Subdivide.Subdivide_boundary_edge_and_corner]
      [@sop.label "Point boundaries"] [@sop.folder "Interpolation"]
      [@sop.kind boundary_parameter];
    face_varying_interpolation :
      Rdk.Subdivide.face_varying_interpolation
      [@sop.default Rdk.Subdivide.Subdivide_fvar_boundaries]
      [@sop.label "Vertex boundaries"] [@sop.folder "Interpolation"]
      [@sop.kind fvar_parameter];
    triangle_policy : Rdk.Subdivide.triangle_policy
      [@sop.default Rdk.Subdivide.Subdivide_triangles_catmull_clark]
      [@sop.label "Triangles"] [@sop.folder "Interpolation"]
      [@sop.kind triangle_parameter];
    creasing_method : Rdk.Subdivide.creasing_method
      [@sop.default Rdk.Subdivide.Subdivide_creasing_uniform]
      [@sop.label "Creasing method"] [@sop.folder "Creases"]
      [@sop.kind creasing_parameter];
    treat_curves_as_independent : bool [@sop.default false]
      [@sop.label "Treat curves independently"];
    recompute_point_normals : bool [@sop.default false]
      [@sop.label "Recompute point normals"];
  } [@@sop.node_key "subdivide_cracks"] [@@sop.node_label "Subdivide Cracks"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/subdivide_cracks: " ^ message) in
      if parameters.iterations < 1 then refuse "iterations must be positive";
      if not (Float.is_finite parameters.crack_bias) || parameters.crack_bias < 0. || parameters.crack_bias > 1. then refuse "crack bias must be finite and in [0, 1]";
      if not (Float.is_finite parameters.crease_weight) || parameters.crease_weight < 0. then refuse "crease weight must be finite and nonnegative";
      if not parameters.generate_resulting_creases && String.trim parameters.resulting_crease_group <> "" then
        refuse "resulting crease group requires generated creases"]
    [@@sop.node_category "Topology"] [@@sop.node_inputs 2] [@@sop.node_slots "input, creases"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input creases ->
    if Option.is_none creases && String.trim parameters.crease_group <> "" then
      invalid_arg "sop/subdivide_cracks: crease group requires a crease input";
    let label = Some label in
    let group = parameters.group in
    let scheme = parameters.scheme in
    let iterations = parameters.iterations in
    let cracks = parameters.cracks in
    let crack_bias = parameters.crack_bias in
    let consistent_topology = parameters.consistent_topology in
    let crease_group = parameters.crease_group in
    let crease_weight_mode = parameters.crease_weight_mode in
    let crease_weight = parameters.crease_weight in
    let generate_resulting_creases = parameters.generate_resulting_creases in
    let resulting_crease_group = parameters.resulting_crease_group in
    let hole_group = parameters.hole_group in
    let remove_holes = parameters.remove_holes in
    let boundary_interpolation = parameters.boundary_interpolation in
    let face_varying_interpolation = parameters.face_varying_interpolation in
    let triangle_policy = parameters.triangle_policy in
    let creasing_method = parameters.creasing_method in
    let treat_curves_as_independent = parameters.treat_curves_as_independent in
    let recompute_point_normals = parameters.recompute_point_normals in
    let cracks = match cracks with
      | Cracks_do_not_close -> Rdk.Subdivide.Subdivide_do_not_close
      | Cracks_pull_no_division -> Rdk.Subdivide.Subdivide_pull_no_edge_division
      | Cracks_pull_divide -> Rdk.Subdivide.Subdivide_pull_divide_edges crack_bias
      | Cracks_pull_triangulate -> Rdk.Subdivide.Subdivide_pull_triangulate crack_bias
      | Cracks_stitch_no_division -> Rdk.Subdivide.Subdivide_stitch_no_edge_division
      | Cracks_stitch_divide -> Rdk.Subdivide.Subdivide_stitch_divide_edges
      | Cracks_stitch_triangulate -> Rdk.Subdivide.Subdivide_stitch_triangulate in
    let group = optional_text group and crease_group = optional_text crease_group
    and resulting_crease_group = optional_text resulting_crease_group and hole_group = optional_text hole_group in
    let crease_weight = match crease_weight_mode with Kernel_auto -> None | Kernel_explicit -> Some crease_weight in
    let inputs = match creases with None -> [|input|] | Some creases -> [|input; creases|] in
    Node.Private.make_geometry ?label ~operation:"subdivide_cracks" ~version:13
      ~parameters:""
      ~cook_mode:(match creases with None -> Node.Duplicate_input 0 | Some _ -> Node.Generic)
      ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "subdivide could not find primitive group %S" name))) in
        let crease_selection = match creases, crease_group with
          | None, _ | Some _, None -> Ok None
          | Some _, Some name ->
              let creases = inputs.(1) in
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name creases with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "subdivide could not find crease primitive group %S" name))) in
        let hole_selection = match hole_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "subdivide could not find hole primitive group %S"
                     name))) in
        match selection, crease_selection, hole_selection with
        | Error error, _, _ -> Error error
        | _, Error error, _ -> Error error
        | _, _, Error error -> Error error
        | Ok primitives, Ok crease_primitives, Ok hole_primitives ->
            match Rdk.Subdivide.subdivide ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~scheme ~iterations ?primitives
                ~cracks ~consistent_topology
                ?creases:(Option.map (fun _ -> inputs.(1)) creases)
                ?crease_primitives ?crease_weight ~generate_resulting_creases
                ?resulting_crease_group ?hole_primitives ~remove_holes
                ~boundary_interpolation ~face_varying_interpolation
                ~triangle_policy ~creasing_method ~treat_curves_as_independent
                ~recompute_point_normals
                geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Boolean = struct

  let operation_parameter = Param.choice ~equal:( = ) [
      "Union", Rdk.Boolean.Union;
      "Intersection", Rdk.Boolean.Intersection;
      "Subtract B from A", Rdk.Boolean.Difference;
      "Subtract A from B", Rdk.Boolean.Reverse_difference;
      "Exclusive or", Rdk.Boolean.Xor;
      "Shatter", Rdk.Boolean.Shatter;
    ]
  let treatment_parameter = Param.choice ~equal:( = ) [
      "Solid", Rdk.Boolean.Solid; "Surface", Rdk.Boolean.Surface;
    ]
  let conflict_parameter = Param.choice ~equal:( = ) [
      "Reject conflict", Rdk.Boolean.Reject;
      "Promote to vertex", Rdk.Boolean.Promote_to_vertex;
    ]
  let seam_points_parameter = Param.choice ~equal:( = ) [
      "Shared", Rdk.Boolean.Shared_seam_points;
      "Split", Rdk.Boolean.Split_seam_points;
    ]
  let detriangulation_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Boolean.Triangles;
      "Unchanged polygons", Rdk.Boolean.Unchanged_polygons;
      "All polygons", Rdk.Boolean.All_polygons;
    ]
  let closed_parameter = Param.choice ~equal:( = ) [
      "Operation default", Closed_default;
      "Require closed", Closed_required;
      "Allow open", Closed_not_required;
    ]

  type parameters = {
    operation : Rdk.Boolean.operation [@sop.default Rdk.Boolean.Union]
      [@sop.label "Operation"] [@sop.kind operation_parameter];
    left_treatment : Rdk.Boolean.treatment [@sop.default Rdk.Boolean.Solid]
      [@sop.label "A treatment"] [@sop.folder "Operands"]
      [@sop.kind treatment_parameter];
    right_treatment : Rdk.Boolean.treatment [@sop.default Rdk.Boolean.Solid]
      [@sop.label "B treatment"] [@sop.folder "Operands"]
      [@sop.kind treatment_parameter];
    resolve_left_self_intersections : bool [@sop.default false]
      [@sop.label "Resolve A self-intersections"] [@sop.folder "Operands"];
    resolve_right_self_intersections : bool [@sop.default false]
      [@sop.label "Resolve B self-intersections"] [@sop.folder "Operands"];
    point_conflict : Rdk.Boolean.point_conflict
      [@sop.default Rdk.Boolean.Promote_to_vertex]
      [@sop.label "Point attribute conflicts"] [@sop.folder "Attributes"]
      [@sop.kind conflict_parameter];
    point_tolerance : float [@sop.default 0.] [@sop.label "Point tolerance"]
      [@sop.folder "Robustness"] [@sop.min 0.] [@sop.max 0.001]
      [@sop.hard_min 0.];
    tiny_seam_threshold : float [@sop.default 0.]
      [@sop.label "Tiny seam threshold"] [@sop.folder "Robustness"]
      [@sop.min 0.] [@sop.max 0.001] [@sop.hard_min 0.];
    cleanup_max_batches : int [@sop.default 8]
      [@sop.label "Cleanup batches"] [@sop.folder "Robustness"]
      [@sop.min 1] [@sop.max 32] [@sop.hard_min 1];
    strict_cleanup : bool [@sop.default true] [@sop.label "Strict cleanup"]
      [@sop.folder "Robustness"];
    seam_points : Rdk.Boolean.seam_points
      [@sop.default Rdk.Boolean.Shared_seam_points]
      [@sop.label "Seam points"] [@sop.folder "Output"]
      [@sop.kind seam_points_parameter];
    detriangulation : Rdk.Boolean.detriangulation
      [@sop.default Rdk.Boolean.Triangles]
      [@sop.label "Polygons"] [@sop.folder "Output"]
      [@sop.kind detriangulation_parameter];
    assume_flat : bool [@sop.default false] [@sop.label "Assume flat"]
      [@sop.folder "Output"];
    require_closed : Support.boolean_closed_policy [@sop.default Closed_default]
      [@sop.label "Closed output"] [@sop.folder "Output"]
      [@sop.kind closed_parameter];
    piece_attribute : string [@sop.default ""] [@sop.label "Piece attribute"]
      [@sop.folder "Output"];
    left_piece_group : string [@sop.default "boolean_left"]
      [@sop.label "A-only group"] [@sop.folder "Shatter groups"];
    overlap_piece_group : string [@sop.default "boolean_overlap"]
      [@sop.label "Overlap group"] [@sop.folder "Shatter groups"];
    right_piece_group : string [@sop.default "boolean_right"]
      [@sop.label "B-only group"] [@sop.folder "Shatter groups"];
  } [@@sop.node_key "boolean"] [@@sop.node_label "Boolean"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/boolean: " ^ message) in
      if not (Float.is_finite parameters.point_tolerance && Float.is_finite parameters.tiny_seam_threshold)
          || parameters.point_tolerance < 0. || parameters.tiny_seam_threshold < 0. then refuse "tolerances must be finite and nonnegative";
      if parameters.cleanup_max_batches < 1 then refuse "cleanup batches must be positive";
      if parameters.operation = Rdk.Boolean.Shatter then (
        if parameters.left_treatment <> Rdk.Boolean.Solid || parameters.right_treatment <> Rdk.Boolean.Solid then
          refuse "shatter requires two solid operands";
        let names = List.filter_map optional_text [parameters.left_piece_group;parameters.overlap_piece_group;parameters.right_piece_group] in
        if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "shatter group names must be distinct")]
    [@@sop.node_category "Boolean"] [@@sop.node_inputs 2] [@@sop.node_slots "left, right"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters left right ->
    let label = Some label in
    let operation = parameters.operation in
    let left_treatment = parameters.left_treatment in
    let right_treatment = parameters.right_treatment in
    let resolve_left_self_intersections = parameters.resolve_left_self_intersections in
    let resolve_right_self_intersections = parameters.resolve_right_self_intersections in
    let point_conflict = parameters.point_conflict in
    let point_tolerance = parameters.point_tolerance in
    let tiny_seam_threshold = parameters.tiny_seam_threshold in
    let cleanup_max_batches = parameters.cleanup_max_batches in
    let strict_cleanup = parameters.strict_cleanup in
    let seam_points = parameters.seam_points in
    let detriangulation = parameters.detriangulation in
    let assume_flat = parameters.assume_flat in
    let require_closed = parameters.require_closed in
    let piece_attribute = parameters.piece_attribute in
    let left_piece_group = parameters.left_piece_group in
    let overlap_piece_group = parameters.overlap_piece_group in
    let right_piece_group = parameters.right_piece_group in
    let require_closed = match require_closed with Closed_default -> None | Closed_required -> Some true | Closed_not_required -> Some false in
    let piece_attribute = optional_text piece_attribute and left_piece_group = optional_text left_piece_group
    and overlap_piece_group = optional_text overlap_piece_group and right_piece_group = optional_text right_piece_group in
    Node.Private.make_geometry ?label ~operation:"boolean" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|left; right|] (fun ~node_id:_ context inputs ->
        match Rdk.Boolean.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~operation ~left_treatment
            ~right_treatment ~resolve_left_self_intersections
            ~resolve_right_self_intersections ~point_conflict ~point_tolerance
            ~tiny_seam_threshold ~cleanup_max_batches ~strict_cleanup
            ~seam_points ~detriangulation ~assume_flat ?require_closed
            ?piece_attribute
            ~left_piece_group ~overlap_piece_group ~right_piece_group
            ~right:inputs.(1) inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Boolean_detect = struct
  type parameters = {
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Inputs"];
    collision_group : string [@sop.default ""] [@sop.label "Collision group"]
      [@sop.folder "Inputs"];
    tolerance : float [@sop.default 0.] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.001] [@sop.hard_min 0.];
    include_coplanar : bool [@sop.default true]
      [@sop.label "Include coplanar overlap"];
    intersecting_group : string [@sop.default "boolean_intersections"]
      [@sop.label "Intersecting group"] [@sop.folder "A/B outputs"];
    intersections_attribute : string [@sop.default ""]
      [@sop.label "Intersections attribute"] [@sop.folder "A/B outputs"];
    count_attribute : string [@sop.default ""]
      [@sop.label "Count attribute"] [@sop.folder "A/B outputs"];
    self_intersecting_group : string
      [@sop.default "boolean_self_intersections"]
      [@sop.label "Self-intersecting group"] [@sop.folder "Self outputs"];
    self_intersections_attribute : string [@sop.default ""]
      [@sop.label "Self intersections attribute"] [@sop.folder "Self outputs"];
    self_count_attribute : string [@sop.default ""]
      [@sop.label "Self count attribute"] [@sop.folder "Self outputs"];
  } [@@sop.node_key "boolean_detect"] [@@sop.node_label "Boolean Detect"]

    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.tolerance) || parameters.tolerance < 0. then
        invalid_arg "sop/boolean_detect: tolerance must be finite and nonnegative"]
    [@@sop.node_category "Boolean/Analysis"] [@@sop.node_inputs 2] [@@sop.node_slots "input, collision"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input collision ->
    let label = Some label in
    let source_group = parameters.source_group in
    let collision_group = parameters.collision_group in
    let tolerance = parameters.tolerance in
    let include_coplanar = parameters.include_coplanar in
    let intersecting_group = parameters.intersecting_group in
    let intersections_attribute = parameters.intersections_attribute in
    let count_attribute = parameters.count_attribute in
    let self_intersecting_group = parameters.self_intersecting_group in
    let self_intersections_attribute = parameters.self_intersections_attribute in
    let self_count_attribute = parameters.self_count_attribute in
    let source_group = optional_text source_group in
    let cross_name name = if Option.is_some collision then optional_text name else None in
    let collision_group = cross_name collision_group and intersecting_group = cross_name intersecting_group
    and intersections_attribute = cross_name intersections_attribute and count_attribute = cross_name count_attribute in
    let self_intersecting_group = optional_text self_intersecting_group
    and self_intersections_attribute = optional_text self_intersections_attribute and self_count_attribute = optional_text self_count_attribute in

    let refuse message = invalid_arg ("sop/boolean_detect: " ^ message) in
    let attributes = List.filter_map Fun.id [intersections_attribute;count_attribute;self_intersections_attribute;self_count_attribute]
    and groups = List.filter_map Fun.id [intersecting_group;self_intersecting_group] in
    if attributes = [] && groups = [] then refuse "at least one output must be requested";
    if List.length attributes <> List.length (List.sort_uniq String.compare attributes) then refuse "attribute outputs must have distinct names";
    if List.length groups <> List.length (List.sort_uniq String.compare groups) then refuse "group outputs must have distinct names";
    let inputs = match collision with None -> [|input|]
      | Some collision -> [|input; collision|] in
    Node.Private.make_geometry ?label ~operation:"boolean_detect" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs (fun ~node_id:_ context inputs ->
        match resolve_optional_primitive_group "boolean_detect" source_group
            inputs.(0) with
        | Error error -> Error error
        | Ok source_primitives ->
            let collision_geometry = if Array.length inputs = 1 then inputs.(0)
              else inputs.(1) in
            (match resolve_optional_primitive_group "boolean_detect collision"
                collision_group collision_geometry with
             | Error error -> Error error
             | Ok collision_primitives ->
                 match Rdk.Boolean_detect.run_checked
                     ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ?source_primitives
                     ?collision_primitives ~tolerance ~include_coplanar
                     ~intersecting_group ?intersections_attribute
                     ?count_attribute ?self_intersecting_group
                     ?self_intersections_attribute ?self_count_attribute
                     ~collision:collision_geometry inputs.(0) with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Triangulate_2d = struct
  let projection_parameter = Param.choice ~equal:( = ) [
      "Best fit", Triangulate_best_fit; "XY", Triangulate_xy; "YZ", Triangulate_yz; "ZX", Triangulate_zx;
      "Custom plane", Triangulate_plane; "Point attribute", Triangulate_attribute;
    ]
  type parameters = {
    point_group : string [@sop.default ""] [@sop.label "Point group"]
      [@sop.folder "Input"];
    constraint_edge_group : string [@sop.default ""]
      [@sop.label "Constraint edge group"] [@sop.folder "Input"];
    constraint_primitive_group : string [@sop.default ""]
      [@sop.label "Constraint primitive group"] [@sop.folder "Input"];
    projection : Support.triangulate_2d_projection [@sop.default Triangulate_best_fit] [@sop.label "Projection"]
      [@sop.folder "Projection"] [@sop.kind projection_parameter];
    plane_origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Projection/Plane"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "plane_origin"]
    plane_origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Projection/Plane"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "plane_origin"]
    plane_origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Projection/Plane"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "plane_origin"]
    plane_normal_x : float [@sop.default 0.] [@sop.label "Normal X"]
      [@sop.folder "Projection/Plane"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "plane_normal"]
    plane_normal_y : float [@sop.default 0.] [@sop.label "Normal Y"]
      [@sop.folder "Projection/Plane"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "plane_normal"]
    plane_normal_z : float [@sop.default 1.] [@sop.label "Normal Z"]
      [@sop.folder "Projection/Plane"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "plane_normal"]
    point_attribute : string [@sop.default "uv"] [@sop.label "Point attribute"]
      [@sop.folder "Projection"];
    seed : int [@sop.default 0] [@sop.label "Seed"]
      [@sop.folder "Triangulation"] [@sop.min 0] [@sop.max 9999];
    split_crossing_constraints : bool [@sop.default false]
      [@sop.label "Split crossing constraints"] [@sop.folder "Constraints"];
    flood_from_hull_boundary : bool [@sop.default false]
      [@sop.label "Flood from hull boundary"] [@sop.folder "Constraints"];
    remove_outside_constraint_polygons : bool [@sop.default false]
      [@sop.label "Remove outside constraints"] [@sop.folder "Constraints"];
    silhouette_constraints : bool [@sop.default false]
      [@sop.label "Silhouette constraints"] [@sop.folder "Constraints"];
    remove_outside_silhouette : bool [@sop.default false]
      [@sop.label "Remove outside silhouette"] [@sop.folder "Constraints"];
    ignore_non_constraint_points : bool [@sop.default false]
      [@sop.label "Ignore non-constraint points"] [@sop.folder "Constraints"];
    remove_duplicate_points : bool [@sop.default false]
      [@sop.label "Remove duplicate points"] [@sop.folder "Cleanup"];
    refine : bool [@sop.default false] [@sop.label "Refine"]
      [@sop.folder "Refinement"];
    allow_constraint_splitting : bool [@sop.default true]
      [@sop.label "Allow constraint splitting"] [@sop.folder "Refinement"];
    minimum_angle : float [@sop.default 0.3490658503988659]
      [@sop.label "Minimum angle"] [@sop.folder "Refinement"] [@sop.min 0.]
      [@sop.max 1.0471975511965976] [@sop.hard_min 0.];
    use_maximum_area : bool [@sop.default false] [@sop.label "Maximum area"]
      [@sop.folder "Refinement"];
    maximum_area : float [@sop.default 1.] [@sop.label "Area"]
      [@sop.folder "Refinement"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    use_target_edge_length : bool [@sop.default false]
      [@sop.label "Target edge length"] [@sop.folder "Refinement"];
    target_edge_length : float [@sop.default 1.] [@sop.label "Edge length"]
      [@sop.folder "Refinement"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    minimum_edge_length : float [@sop.default 0.]
      [@sop.label "Minimum edge length"] [@sop.folder "Refinement"]
      [@sop.min 0.] [@sop.max 100.] [@sop.hard_min 0.];
    maximum_new_points : int [@sop.default 100000]
      [@sop.label "Maximum new points"] [@sop.folder "Refinement/Limits"]
      [@sop.min 0] [@sop.max 1000000] [@sop.hard_min 0];
    regularization_steps : int [@sop.default 0]
      [@sop.label "Regularization steps"] [@sop.folder "Refinement"]
      [@sop.min 0] [@sop.max 100] [@sop.hard_min 0];
    allow_movement_of_interior_input_points : bool [@sop.default false]
      [@sop.label "Move interior input points"] [@sop.folder "Refinement"];
    preserve_point_payload : bool [@sop.default true]
      [@sop.label "Preserve point payload"] [@sop.folder "Payload"];
    restore_original_point_positions : bool [@sop.default true]
      [@sop.label "Restore original positions"] [@sop.folder "Payload"];
    keep_primitives : bool [@sop.default false] [@sop.label "Keep primitives"]
      [@sop.folder "Output"];
    remove_unused_points : bool [@sop.default false]
      [@sop.label "Remove unused points"] [@sop.folder "Cleanup"];
    recompute_point_normals : bool [@sop.default false]
      [@sop.label "Recompute point normals"] [@sop.folder "Output"];
    split_point_group : string [@sop.default ""]
      [@sop.label "Split point group"] [@sop.folder "Output"];
    refinement_point_group : string [@sop.default ""]
      [@sop.label "Refinement point group"] [@sop.folder "Output"];
    triangle_group : string [@sop.default ""] [@sop.label "Triangle group"]
      [@sop.folder "Output"];
    constraint_group : string [@sop.default ""]
      [@sop.label "Constraint group"] [@sop.folder "Output"];
  } [@@sop.node_key "triangulate_2d"] [@@sop.node_label "Triangulate 2D"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/triangulate_2d: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.plane_origin_x;parameters.plane_origin_y;parameters.plane_origin_z;
        parameters.plane_normal_x;parameters.plane_normal_y;parameters.plane_normal_z;parameters.minimum_angle;parameters.maximum_area;
        parameters.target_edge_length;parameters.minimum_edge_length]) then refuse "numeric values must be finite";
      if parameters.minimum_angle < 0. || parameters.minimum_edge_length < 0. || parameters.maximum_area < 0. || parameters.target_edge_length < 0.
        || parameters.maximum_new_points < 0 || parameters.maximum_new_points >= Sys.max_array_length || parameters.regularization_steps < 0 then refuse "refinement bounds must be nonnegative";
      if parameters.refine && (parameters.minimum_angle <= 0. || parameters.minimum_angle >= Float.pi /. 3.) then refuse "minimum angle must lie between zero and pi/3";
      if parameters.use_maximum_area && parameters.maximum_area <= 0. then refuse "maximum area must be positive";
      if parameters.use_target_edge_length && parameters.target_edge_length <= 0. then refuse "target edge length must be positive";
      if parameters.projection = Triangulate_plane && parameters.plane_normal_x = 0. && parameters.plane_normal_y = 0. && parameters.plane_normal_z = 0. then refuse "plane normal must be nonzero";
      if parameters.projection = Triangulate_attribute && String.trim parameters.point_attribute = "" then refuse "point attribute must be nonblank"]
    [@@sop.node_category "Topology/Triangulate"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let point_group = parameters.point_group in
    let constraint_edge_group = parameters.constraint_edge_group in
    let constraint_primitive_group = parameters.constraint_primitive_group in
    let projection = parameters.projection in
    let plane_origin = Rays_math.Vec3.create parameters.plane_origin_x parameters.plane_origin_y parameters.plane_origin_z in
    let plane_normal = Rays_math.Vec3.create parameters.plane_normal_x parameters.plane_normal_y parameters.plane_normal_z in
    let point_attribute = parameters.point_attribute in
    let seed = parameters.seed in
    let split_crossing_constraints = parameters.split_crossing_constraints in
    let flood_from_hull_boundary = parameters.flood_from_hull_boundary in
    let remove_outside_constraint_polygons = parameters.remove_outside_constraint_polygons in
    let silhouette_constraints = parameters.silhouette_constraints in
    let remove_outside_silhouette = parameters.remove_outside_silhouette in
    let ignore_non_constraint_points = parameters.ignore_non_constraint_points in
    let remove_duplicate_points = parameters.remove_duplicate_points in
    let refine = parameters.refine in
    let allow_constraint_splitting = parameters.allow_constraint_splitting in
    let minimum_angle = parameters.minimum_angle in
    let use_maximum_area = parameters.use_maximum_area in
    let maximum_area = parameters.maximum_area in
    let use_target_edge_length = parameters.use_target_edge_length in
    let target_edge_length = parameters.target_edge_length in
    let minimum_edge_length = parameters.minimum_edge_length in
    let maximum_new_points = parameters.maximum_new_points in
    let regularization_steps = parameters.regularization_steps in
    let allow_movement_of_interior_input_points = parameters.allow_movement_of_interior_input_points in
    let preserve_point_payload = parameters.preserve_point_payload in
    let restore_original_point_positions = parameters.restore_original_point_positions in
    let keep_primitives = parameters.keep_primitives in
    let remove_unused_points = parameters.remove_unused_points in
    let recompute_point_normals = parameters.recompute_point_normals in
    let split_point_group = parameters.split_point_group in
    let refinement_point_group = parameters.refinement_point_group in
    let triangle_group = parameters.triangle_group in
    let constraint_group = parameters.constraint_group in
    let projection = match projection with
      | Triangulate_best_fit -> Rdk.Triangulate2d.Best_fit | Triangulate_xy -> Rdk.Triangulate2d.Plane_xy
      | Triangulate_yz -> Rdk.Triangulate2d.Plane_yz | Triangulate_zx -> Rdk.Triangulate2d.Plane_zx
      | Triangulate_plane -> Rdk.Triangulate2d.Plane {origin=plane_origin;normal=plane_normal}
      | Triangulate_attribute -> Rdk.Triangulate2d.Point_attribute point_attribute in
    let seed = Int64.of_int seed in
    let maximum_area = if use_maximum_area then Some maximum_area else None
    and target_edge_length = if use_target_edge_length then Some target_edge_length else None in
    let point_group = optional_text point_group and constraint_edge_group = optional_text constraint_edge_group
    and constraint_primitive_group = optional_text constraint_primitive_group and split_point_group = optional_text split_point_group
    and refinement_point_group = optional_text refinement_point_group and triangle_group = optional_text triangle_group
    and constraint_group = optional_text constraint_group in
    Node.Private.make_geometry ?label ~operation:"triangulate_2d" ~version:12
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let selection = match point_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some group -> Ok (Some (Rdk.Transform_ops.Selected_points group))
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "triangulate_2d could not find point group %S" name))) in
        let constraint_edges = match constraint_edge_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "triangulate_2d could not find native edge group %S" name))) in
        let constraint_primitives = match constraint_primitive_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "triangulate_2d could not find primitive group %S" name))) in
        match selection,constraint_edges,constraint_primitives with
        | Error error,_,_ -> Error error
        | _,Error error,_ | _,_,Error error -> Error error
        | Ok selection,Ok constraint_edges,Ok constraint_primitives ->
            match Rdk.Triangulate2d.run
                ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
                ?selection ?constraint_edges ?constraint_primitives ~projection
                ~seed ~split_crossing_constraints ~flood_from_hull_boundary
                ~remove_outside_constraint_polygons
                ~silhouette_constraints ~remove_outside_silhouette
                ~ignore_non_constraint_points
                ~remove_duplicate_points
                ~refine ~allow_constraint_splitting ~minimum_angle ?maximum_area
                ?target_edge_length ~minimum_edge_length ~maximum_new_points
                ~regularization_steps ~allow_movement_of_interior_input_points
                ~preserve_point_payload ~restore_original_point_positions
                ~keep_primitives
                ~remove_unused_points ~recompute_point_normals
                ?split_point_group ?refinement_point_group ?triangle_group
                ?constraint_group geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

