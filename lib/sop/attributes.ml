(* Attribute nodes, declared once: the record is the editor schema, the
   factory and the typed [Sop] constructor. *)

open Support



(* The decoded RGBA floats of an image as a carrier geometry, so a Lisp body can sample pixels:
   one point per texel in row-major order, P = (x, y, width), and the Point Float4 attribute "rgba". *)
module Image_texels = struct
  type parameters = unit
    [@@sop.node_key "image_texels"] [@@sop.node_label "Image Texels"]
    [@@sop.node_category "Attribute/Image"] [@@sop.node_inputs 1]
    [@@sop.node_slots "image"] [@@sop.node_types "image"]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label _parameters image ->
    Node.Build.make ~label ~operation:"image_texels" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[|image|] (fun ~node_id:_ _context inputs ->
        Result.bind (Payload.image inputs.(0)) (fun image ->
          let width = Image.width image and rgba = Image.storage image in
          let count = width * Image.height image in
          let geometry = Rdk.Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
            for i = first to last - 1 do
              x.(i) <- float (i mod width); y.(i) <- float (i / width); z.(i) <- float width
            done) in
          let plane k = Array.init count (fun i -> rgba.(4 * i + k)) in
          match Rdk.Packed.Float4.of_owned ~x:(plane 0) ~y:(plane 1) ~z:(plane 2) ~w:(plane 3) with
          | Error message -> Error (Diagnostic.error ~code:"invalid_image_texels" message)
          | Ok values ->
              match Rdk.Attribute.create_owned ~name:"rgba" ~owner:Rdk.Attribute.Point (Float4 values) with
              | Error message -> Error (Diagnostic.error ~code:"invalid_image_texels" message)
              | Ok attribute -> (match Rdk.Geometry.with_attribute attribute geometry with
                  | Error message -> Error (Diagnostic.error ~code:"invalid_image_texels" message)
                  | Ok geometry -> Ok Node.Build.{payload = Payload.Geometry geometry; instances = None}))))
  let factory = parameters_factory build
end

module Uv_flatten = struct
  type parameters = {
    name : string [@sop.default "uv"] [@sop.label "UV attribute"];
    seams : string [@sop.default ""] [@sop.label "Seam group"];
    iterations : int [@sop.default 500] [@sop.label "Iterations"]
      [@sop.min 1] [@sop.max 2000] [@sop.hard_min 1];
    tolerance : float [@sop.default 1e-7] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.01] [@sop.hard_min 0.];
  } [@@sop.node_key "uv_flatten"] [@@sop.node_label "UV Flatten"]
    [@@sop.node_category "UV/Layout"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let seams = optional_text parameters.seams in
    let iterations = parameters.iterations in
    let tolerance = parameters.tolerance in
    Node.Build.make_geometry ?label ~operation:"uv_flatten" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match uv_parameterize_seams "uv_flatten" inputs.(0) seams with
        | Error _ as error -> error
        | Ok (edge_seams, seams) ->
            match Rdk.Uv_ops.flatten ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?seams ?edge_seams
                ~iterations ~tolerance inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Uv_relax = struct
  type parameters = {
    name : string [@sop.default "uv"] [@sop.label "UV attribute"];
    seams : string [@sop.default ""] [@sop.label "Seam group"];
    uv_tolerance : float [@sop.default 1e-9] [@sop.label "UV tolerance"]
      [@sop.min 0.] [@sop.max 0.01] [@sop.hard_min 0.];
    iterations : int [@sop.default 500] [@sop.label "Iterations"]
      [@sop.min 1] [@sop.max 2000] [@sop.hard_min 1];
    tolerance : float [@sop.default 1e-7] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.01] [@sop.hard_min 0.];
  } [@@sop.node_key "uv_relax"] [@@sop.node_label "UV Relax"]
    [@@sop.node_category "UV/Layout"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let seams = optional_text parameters.seams in
    let uv_tolerance = parameters.uv_tolerance in
    let iterations = parameters.iterations in
    let tolerance = parameters.tolerance in
    Node.Build.make_geometry ?label ~operation:"uv_relax" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match uv_parameterize_seams "uv_relax" inputs.(0) seams with
        | Error _ as error -> error
        | Ok (edge_seams, seams) ->
            match Rdk.Uv_ops.relax ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?seams ?edge_seams
                ~uv_tolerance ~iterations ~tolerance inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end






























module Distance_from_geometry = struct
  let reference_parameter = Param.choice ~equal:( = ) [
      "Points", Rdk.Transform_ops.Distance_reference_points;
      "Primitives", Rdk.Transform_ops.Distance_reference_primitives;
    ]

  type parameters = {
    affected_owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_points]
      [@sop.label "Affected group type"] [@sop.folder "Source"]
      [@sop.kind element_owner_parameter];
    affected_group : string [@sop.default ""] [@sop.label "Affected group"]
      [@sop.folder "Source"];
    reference_owner : Rdk.Group_ops.owner [@sop.default Rdk.Group_ops.Group_primitives]
      [@sop.label "Reference group type"] [@sop.folder "Reference"]
      [@sop.kind element_owner_parameter];
    reference_group : string [@sop.default ""] [@sop.label "Reference group"]
      [@sop.folder "Reference"];
    reference_kind : Rdk.Transform_ops.distance_from_geometry_reference
      [@sop.default Rdk.Transform_ops.Distance_reference_primitives]
      [@sop.label "Reference type"] [@sop.folder "Reference"]
      [@sop.kind reference_parameter];
    falloff : Rdk.Transform_ops.soft_transform_falloff
      [@sop.default Rdk.Transform_ops.Soft_linear]
      [@sop.label "Falloff"] [@sop.kind soft_falloff_parameter];
    radius_mode : Support.distance_radius_mode [@sop.default Radius_maximum]
      [@sop.label "Radius"] [@sop.kind distance_radius_parameter];
    radius : float [@sop.default 1.] [@sop.label "Fixed radius"]
      [@sop.min 0.0001] [@sop.max 1000.] [@sop.hard_min 0.];
    distance_attribute : string [@sop.default "distance"]
      [@sop.label "Distance attribute"] [@sop.folder "Output"];
    mask_attribute : string [@sop.default ""] [@sop.label "Mask attribute"]
      [@sop.folder "Output"];
  } [@@sop.node_key "distance_from_geometry"]
    [@@sop.validate fun parameters ->
      let* () = if not (Float.is_finite parameters.radius) || parameters.radius < 0.
          || (parameters.radius_mode = Radius_fixed && parameters.radius = 0.) then
        Error "sop/distance_from_geometry: fixed radius must be positive and finite" else Ok () in
      let names = List.filter_map optional_text [parameters.distance_attribute; parameters.mask_attribute] in
      if names = [] || List.mem "P" names || List.length names <> List.length (List.sort_uniq String.compare names) then
        Error "sop/distance_from_geometry: output names must be present, distinct and cannot be P" else Ok ()]
    [@@sop.node_label "Distance from Geometry"]
    [@@sop.node_category "Attribute/Distance"] [@@sop.node_inputs 2] [@@sop.node_slots "source, reference"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source reference ->
    let label = Some label in
    let affected_owner = parameters.affected_owner in
    let affected_group = parameters.affected_group in
    let reference_owner = parameters.reference_owner in
    let reference_group = parameters.reference_group in
    let reference_kind = parameters.reference_kind in
    let falloff = parameters.falloff in
    let radius_mode = parameters.radius_mode in
    let radius = parameters.radius in
    let distance_attribute = parameters.distance_attribute in
    let mask_attribute = optional_text parameters.mask_attribute in
    let affected = optional_element_group affected_owner affected_group
    and reference_selection = optional_element_group reference_owner reference_group
    and radius = match radius_mode with Radius_fixed -> Rdk.Transform_ops.Distance_fixed radius
      | Radius_maximum -> Rdk.Transform_ops.Distance_maximum in
    let distance_attribute = optional_text distance_attribute in
    Node.Build.make_geometry ?label ~operation:"distance_from_geometry" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|source; reference|]
      (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"distance_from_geometry affected"
            affected inputs.(0) with
        | Error error -> Error error
        | Ok affected ->
            (match resolve_element_group
                ~operation:"distance_from_geometry reference"
                reference_selection inputs.(1) with
             | Error error -> Error error
             | Ok reference_selection ->
                 match Rdk.Transform_ops.distance_from_geometry
                     { cancel = Some (Context.cancel_token context);
                       grain = Some (Context.grain context); affected;
                       reference_selection; reference_kind; falloff; radius;
                       distance_attribute; mask_attribute;
                       reference = inputs.(1) } inputs.(0) with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end



module Attribute_mirror = struct
  open Rays_math
  let owner_parameter = Param.choice ~equal:( = ) [
      "Point", Rdk.Attribute.Point;
      "Vertex", Rdk.Attribute.Vertex;
      "Primitive", Rdk.Attribute.Primitive;
    ]
  let group_use_parameter = Param.choice ~equal:( = ) [
      "Group is source", Rdk.Attribute_mirror.Mirror_group_as_source;
      "Group is destination", Rdk.Attribute_mirror.Mirror_group_as_destination;
    ]
  let method_parameter = Param.choice ~equal:( = ) [
      "Plane", Mirror_plane; "Mapping attribute", Mirror_mapping;
    ]
  let transform_parameter = Param.choice ~equal:( = ) [
      "Copy", Mirror_copy; "UV", Mirror_uv; "Vector", Mirror_vector; "Point", Mirror_point;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner
      [@sop.default Rdk.Attribute.Point]
      [@sop.label "Attribute owner"] [@sop.kind owner_parameter];
    attributes : string [@sop.default "Cd"] [@sop.label "Attributes"];
    group : string [@sop.default ""] [@sop.label "Selection group"];
    group_use : Rdk.Attribute_mirror.group_use
      [@sop.default Rdk.Attribute_mirror.Mirror_group_as_source]
      [@sop.label "Group use"] [@sop.kind group_use_parameter];
    method_ : Support.mirror_method [@sop.default Mirror_plane] [@sop.label "Mirror method"]
      [@sop.kind method_parameter];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    normal_x : float [@sop.default 1.] [@sop.label "Normal X"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    normal_y : float [@sop.default 0.] [@sop.label "Normal Y"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    normal_z : float [@sop.default 0.] [@sop.label "Normal Z"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    distance : float [@sop.default 1000000.] [@sop.label "Maximum distance"]
      [@sop.folder "Plane"] [@sop.min 0.] [@sop.max 1000000.]
      [@sop.hard_min 0.];
    tolerance : float [@sop.default 0.00001] [@sop.label "Tolerance"]
      [@sop.folder "Plane"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    mapping_attribute : string [@sop.default "mirror"]
      [@sop.label "Mapping attribute"] [@sop.folder "Mapping"];
    mapping_destination_group : string [@sop.default "mirror_destination"]
      [@sop.label "Mapping destination group"] [@sop.folder "Mapping"];
    transform : Support.mirror_transform [@sop.default Mirror_copy] [@sop.label "Value transform"]
      [@sop.kind transform_parameter];
    uv_origin_u : float [@sop.default 0.] [@sop.label "UV origin U"]
      [@sop.folder "Value transform/UV"] [@sop.min (-10.)] [@sop.max 10.];
    uv_origin_v : float [@sop.default 0.] [@sop.label "UV origin V"]
      [@sop.folder "Value transform/UV"] [@sop.min (-10.)] [@sop.max 10.];
    uv_direction_u : float [@sop.default 1.] [@sop.label "UV direction U"]
      [@sop.folder "Value transform/UV"] [@sop.min (-10.)] [@sop.max 10.];
    uv_direction_v : float [@sop.default 0.] [@sop.label "UV direction V"]
      [@sop.folder "Value transform/UV"] [@sop.min (-10.)] [@sop.max 10.];
    replace_strings : bool [@sop.default false]
      [@sop.label "Replace strings"] [@sop.folder "Strings"];
    string_search : string [@sop.default "L"] [@sop.label "Search"]
      [@sop.folder "Strings"];
    string_replacement : string [@sop.default "R"] [@sop.label "Replacement"]
      [@sop.folder "Strings"];
    output_mapping : string [@sop.default ""]
      [@sop.label "Output mapping"] [@sop.folder "Output"];
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Output"];
    destination_group : string [@sop.default ""]
      [@sop.label "Destination group"] [@sop.folder "Output"];
  } [@@sop.node_key "attribute_mirror"] [@@sop.node_label "Attribute Mirror"]
    [@@sop.validate fun parameters ->
      let finite value = if not (Float.is_finite value) then
        Error "sop/attribute_mirror: controls must be finite" else Ok () in
      let* () = each finite [parameters.origin_x; parameters.origin_y; parameters.origin_z;
        parameters.normal_x; parameters.normal_y; parameters.normal_z;
        parameters.distance; parameters.tolerance; parameters.uv_origin_u;
        parameters.uv_origin_v; parameters.uv_direction_u; parameters.uv_direction_v] in
      let* () = if parameters.distance < 0. || parameters.tolerance < 0.
          || parameters.tolerance > sqrt Float.max_float then
        Error "sop/attribute_mirror: invalid distance or tolerance" else Ok () in
      let* () = if parameters.replace_strings && parameters.string_search = "" then
        Error "sop/attribute_mirror: empty string search" else Ok () in
      let* () = (match optional_text parameters.source_group, optional_text parameters.destination_group with
       | Some source, Some destination when source = destination ->
           Error "sop/attribute_mirror: source and destination groups must differ"
       | _ -> Ok ()) in
      let* () = if parameters.transform = Mirror_uv
          && parameters.uv_direction_u = 0. && parameters.uv_direction_v = 0. then
        Error "sop/attribute_mirror: UV direction must be non-zero" else Ok () in
      let* () = validate_parameter_pattern "sop/attribute_mirror: attributes" parameters.attributes in
      match parameters.method_ with
      | Mirror_mapping ->
          let* () = if String.trim parameters.mapping_attribute = "" then
            Error "sop/attribute_mirror: empty mapping attribute" else Ok () in
          let* () = if String.trim parameters.mapping_destination_group = "" then
            Error "sop/attribute_mirror: empty mapping destination group" else Ok () in
          if parameters.transform = Mirror_vector || parameters.transform = Mirror_point then
            Error "sop/attribute_mirror: vector/point transforms require a plane" else Ok ()
      | Mirror_plane ->
          let* () = if parameters.owner = Rdk.Attribute.Vertex then
            Error "sop/attribute_mirror: vertex owner requires mapping" else Ok () in
          let nx = parameters.normal_x and ny = parameters.normal_y and nz = parameters.normal_z in
          let scale = Float.max (Float.abs nx) (Float.max (Float.abs ny) (Float.abs nz)) in
          let* () = if scale = 0. then Error "sop/attribute_mirror: normal must be non-zero" else Ok () in
          let x = nx /. scale and y = ny /. scale and z = nz /. scale in
          let length = sqrt (x *. x +. y *. y +. z *. z) in
          each finite [parameters.origin_x +. parameters.distance *. (x /. length);
            parameters.origin_y +. parameters.distance *. (y /. length);
            parameters.origin_z +. parameters.distance *. (z /. length)]]
    [@@sop.node_category "Attribute/Transform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let group_use = parameters.group_use in
    let attributes = parameters.attributes in
    let transform = parameters.transform in
    let replace_strings = parameters.replace_strings in
    let string_search = parameters.string_search in
    let string_replacement = parameters.string_replacement in
    let output_mapping = parameters.output_mapping in
    let source_group = parameters.source_group in
    let destination_group = parameters.destination_group in
    let owner = parameters.owner in
    let method_ = parameters.method_ in
    let origin = Rays_math.Vec3.create parameters.origin_x parameters.origin_y parameters.origin_z in
    let normal = Rays_math.Vec3.create parameters.normal_x parameters.normal_y parameters.normal_z in
    let distance = parameters.distance in
    let tolerance = parameters.tolerance in
    let mapping_attribute = parameters.mapping_attribute in
    let mapping_destination_group = parameters.mapping_destination_group in
    let uv_origin_u = parameters.uv_origin_u in
    let uv_origin_v = parameters.uv_origin_v in
    let uv_direction_u = parameters.uv_direction_u in
    let uv_direction_v = parameters.uv_direction_v in
    let group = optional_text group and output_mapping = optional_text output_mapping
    and source_group = optional_text source_group and destination_group = optional_text destination_group in
    let transform = match transform with
      | Mirror_copy -> Rdk.Attribute_mirror.Mirror_copy
      | Mirror_uv -> Rdk.Attribute_mirror.Mirror_uv { origin_u = uv_origin_u;
          origin_v = uv_origin_v; direction_u = uv_direction_u; direction_v = uv_direction_v }
      | Mirror_vector -> Rdk.Attribute_mirror.Mirror_vector
      | Mirror_point -> Rdk.Attribute_mirror.Mirror_point in
    let string_replace = if replace_strings then Some (string_search, string_replacement) else None in
    Node.Build.make_geometry ?label ~operation:"attribute_mirror" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) and group_owner = attribute_mirror_group_owner owner in
        let resolve label name = match name with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:group_owner name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "attribute_mirror could not find %s %S"
                      label name))) in
        match resolve "selection group" group with
        | Error error -> Error error
        | Ok resolved_group ->
            let resolved_method = match method_ with
              | Mirror_plane ->
                  Ok (Rdk.Attribute_mirror.Mirror_by_plane {
                    origin; normal; distance; tolerance })
              | Mirror_mapping ->
                  let name = mapping_destination_group in
                  (match Rdk.Geometry.find_group ~owner:group_owner name geometry with
                   | Some destination_group -> Ok (Rdk.Attribute_mirror.Mirror_by_mapping {
                       mapping_attribute; destination_group })
                   | None -> Error (Diagnostic.error ~code:"missing_group"
                       (Printf.sprintf
                         "attribute_mirror could not find mapping destination group %S"
                         name))) in
            match resolved_method with
            | Error error -> Error error
            | Ok method_ ->
                match Rdk.Attribute_mirror.run { cancel = Some (Context.cancel_token context); grain = Some (Context.grain context); group = resolved_group; group_use; attributes; transform; string_replace; output_mapping; source_group; destination_group; owner; method_ } geometry with
                | Ok geometry -> cooked geometry
                | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end



module Attribute_transfer_all = struct
  type parameters = {
    point_pattern : string [@sop.default "*"] [@sop.label "Point attributes"];
    vertex_pattern : string [@sop.default ""] [@sop.label "Vertex attributes"];
    primitive_pattern : string [@sop.default ""]
      [@sop.label "Primitive attributes"];
    detail_pattern : string [@sop.default ""] [@sop.label "Detail attributes"];
    mode : Support.transfer_mode [@sop.default Transfer_nearest]
      [@sop.label "Transfer mode"] [@sop.kind transfer_mode_parameter];
    neighbors : int [@sop.default 4] [@sop.label "Neighbors"]
      [@sop.folder "Sampling"] [@sop.min 1] [@sop.max 128]
      [@sop.hard_min 1];
    power : float [@sop.default 2.] [@sop.label "Inverse power"]
      [@sop.folder "Sampling"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    kernel_radius : float [@sop.default 1.] [@sop.label "Kernel radius"]
      [@sop.folder "Sampling"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];

    distance_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Distance mode"] [@sop.folder "Distance"] [@sop.kind kernel_mode_parameter];
    max_distance : float [@sop.default 1.] [@sop.label "Maximum distance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    blend_width : float [@sop.default 0.] [@sop.label "Blend width"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    falloff : Support.transfer_falloff [@sop.default Transfer_linear]
      [@sop.label "Falloff"] [@sop.folder "Distance"]
      [@sop.kind transfer_falloff_parameter];
    uniform_bias : float [@sop.default 0.5] [@sop.label "Uniform bias"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    unmatched : Rdk.Attribute_transfer.unmatched
      [@sop.default Rdk.Attribute_transfer.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
  } [@@sop.node_key "attribute_transfer_all"]
    [@@sop.node_label "Attribute Transfer All"]

    [@@sop.validate fun parameters ->
      let refuse message = Error ("sop/attribute_transfer_all: " ^ message) in
      let patterns = [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern;parameters.detail_pattern] in
      let* () = if List.for_all (fun text -> String.trim text = "") patterns then refuse "at least one owner pattern is required" else Ok () in
      let* () = each (fun text -> if String.trim text <> "" then validate_parameter_pattern "sop/attribute_transfer_all" text else Ok ()) patterns in
      let* () = if parameters.neighbors < 1 then refuse "neighbors must be positive" else Ok () in
      let* () = if not (List.for_all Float.is_finite [parameters.power;parameters.kernel_radius;
          parameters.max_distance;parameters.blend_width;parameters.uniform_bias]) then refuse "sampling controls must be finite" else Ok () in
      let* () = if parameters.power < 0. || (parameters.mode = Transfer_inverse && parameters.power = 0.) then refuse "invalid inverse power" else Ok () in
      let* () = if parameters.kernel_radius < 0. || parameters.kernel_radius > sqrt Float.max_float
          || parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float
          || parameters.blend_width < 0. then refuse "distances must be nonnegative and safely squarable" else Ok () in
      let* () = if parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "uniform bias must be within [0, 1]" else Ok () in
      let* () = if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then refuse "blend width requires an explicit maximum distance" else Ok () in
      if parameters.distance_mode = Kernel_explicit &&
          parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance plus blend width must be safely squarable" else Ok ()]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source target ->
    let label = Some label in
    let point_pattern = parameters.point_pattern in
    let vertex_pattern = parameters.vertex_pattern in
    let primitive_pattern = parameters.primitive_pattern in
    let detail_pattern = parameters.detail_pattern in
    let mode = parameters.mode in
    let neighbors = parameters.neighbors in
    let power = parameters.power in
    let kernel_radius = parameters.kernel_radius in
    let distance_mode = parameters.distance_mode in
    let max_distance = parameters.max_distance in
    let blend_width = parameters.blend_width in
    let falloff = parameters.falloff in
    let uniform_bias = parameters.uniform_bias in
    let unmatched = parameters.unmatched in
    let point_pattern = optional_text point_pattern and vertex_pattern = optional_text vertex_pattern
    and primitive_pattern = optional_text primitive_pattern and detail_pattern = optional_text detail_pattern in
    let mode = Support.transfer_mode mode neighbors power kernel_radius in
    let falloff = Support.transfer_falloff falloff uniform_bias in
    let max_distance = match distance_mode with Kernel_explicit -> Some max_distance | Kernel_auto -> None in
    Node.Build.make_geometry ?label ~operation:"attribute_transfer_all" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Attribute_ops.transfer_all
            { cancel = Some (Context.cancel_token context);
              grain = Some (Context.grain context);
              point_pattern; vertex_pattern; primitive_pattern; detail_pattern;
              mode; max_distance; blend_width; falloff; unmatched;
              source = inputs.(0); target = inputs.(1) } with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_transfer_surface = struct
  let vertex_selection_parameter = Param.choice ~equal:( = ) [
      "All triangle vertices", Rdk.Surface_index.All_triangle_vertices;
      "Any triangle vertex", Rdk.Surface_index.Any_triangle_vertex;
    ]
  type parameters = {
    target_owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Target owner"]
      [@sop.kind element_attribute_owner_parameter];
    attributes : string [@sop.default "point\tCd\tCd"]
      [@sop.label "Attributes (owner, source, target)"];
    distance_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Distance mode"]
      [@sop.folder "Distance"] [@sop.kind kernel_mode_parameter];
    max_distance : float [@sop.default 1.] [@sop.label "Maximum distance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    blend_width : float [@sop.default 0.] [@sop.label "Blend width"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    falloff : transfer_falloff [@sop.default Transfer_linear]
      [@sop.label "Falloff"] [@sop.folder "Distance"]
      [@sop.kind transfer_falloff_parameter];
    uniform_bias : float [@sop.default 0.5] [@sop.label "Uniform bias"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    unmatched : Rdk.Attribute_transfer.unmatched
      [@sop.default Rdk.Attribute_transfer.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
    distance_attribute : string [@sop.default ""]
      [@sop.label "Distance attribute"] [@sop.folder "Output"];
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Groups/Source"];
    source_group_pattern : string [@sop.default ""]
      [@sop.label "Source group pattern"] [@sop.folder "Groups/Source"];
    source_vertex_group : string [@sop.default ""]
      [@sop.label "Source vertex group"] [@sop.folder "Groups/Source"];
    source_vertex_group_pattern : string [@sop.default ""]
      [@sop.label "Source vertex group pattern"]
      [@sop.folder "Groups/Source"];
    source_vertex_selection : Rdk.Surface_index.vertex_selection
      [@sop.default Rdk.Surface_index.All_triangle_vertices]
      [@sop.label "Vertex selection"] [@sop.folder "Groups/Source"]
      [@sop.kind vertex_selection_parameter];
    target_group : string [@sop.default ""] [@sop.label "Target group"]
      [@sop.folder "Groups/Target"];
    target_group_pattern : string [@sop.default ""]
      [@sop.label "Target group pattern"] [@sop.folder "Groups/Target"];
  } [@@sop.node_key "attribute_transfer_surface"]
    [@@sop.node_label "Attribute Transfer Surface"]

    [@@sop.validate fun parameters ->
      let refuse message = Error ("sop/attribute_transfer_surface: " ^ message) in
      let* () = if parameters.target_owner = Rdk.Attribute.Detail then refuse "detail target is not spatial" else Ok () in
      let* attributes = decode_surface_attributes parameters.attributes in
      let names = List.map (fun (attribute : Rdk.Attribute_transfer.surface_attribute) -> attribute.target_name) attributes in
      let* () = if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "target attribute names must be distinct" else Ok () in
      let* () = if parameters.target_owner = Rdk.Attribute.Point && List.mem "P" names then refuse "target P is not an ordinary attribute" else Ok () in
      let distance = optional_text parameters.distance_attribute in
      let* () = if (parameters.target_owner = Rdk.Attribute.Point && distance = Some "P")
          || Option.fold ~none:false ~some:(fun name -> List.mem name names) distance then refuse "distance output conflicts with attributes" else Ok () in
      let* () = if not (Float.is_finite parameters.max_distance && Float.is_finite parameters.blend_width && Float.is_finite parameters.uniform_bias) then
        refuse "distance controls must be finite" else Ok () in
      let* () = if parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float || parameters.blend_width < 0.
          || parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "distance controls are out of range" else Ok () in
      let* () = if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then refuse "blend width requires an explicit maximum distance" else Ok () in
      let* () = if parameters.distance_mode = Kernel_explicit && parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance window exceeds the squared-distance range" else Ok () in
      each (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "attribute_transfer_surface" pattern else Ok ())
        [parameters.source_group_pattern;parameters.source_vertex_group_pattern;parameters.target_group_pattern]]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source target ->
    let label = Some label in
    let target_owner = parameters.target_owner in
    let attributes = parameters.attributes in
    let distance_mode = parameters.distance_mode in
    let max_distance = parameters.max_distance in
    let blend_width = parameters.blend_width in
    let falloff = parameters.falloff in
    let uniform_bias = parameters.uniform_bias in
    let unmatched = parameters.unmatched in
    let distance_attribute = parameters.distance_attribute in
    let source_group = parameters.source_group in
    let source_group_pattern = parameters.source_group_pattern in
    let source_vertex_group = parameters.source_vertex_group in
    let source_vertex_group_pattern = parameters.source_vertex_group_pattern in
    let source_vertex_selection = parameters.source_vertex_selection in
    let target_group = parameters.target_group in
    let target_group_pattern = parameters.target_group_pattern in
    let attributes = validated (decode_surface_attributes attributes) in
    let max_distance = if distance_mode = Kernel_auto then None else Some max_distance in
    let falloff = transfer_falloff falloff uniform_bias and distance_attribute = optional_text distance_attribute in
    let source_group,source_group_pattern = exact_or_pattern source_group source_group_pattern
    and source_vertex_group,source_vertex_group_pattern = exact_or_pattern source_vertex_group source_vertex_group_pattern
    and target_group,target_group_pattern = exact_or_pattern target_group target_group_pattern in
    let source_group_pattern_compiled = validated (compile_transfer_group_pattern
        "attribute_transfer_surface" "source" source_group_pattern)
    and target_group_pattern_compiled = validated (compile_transfer_group_pattern
        "attribute_transfer_surface" "target" target_group_pattern)
    and source_vertex_group_pattern_compiled = validated (compile_transfer_group_pattern
        "attribute_transfer_surface" "source vertex" source_vertex_group_pattern) in
    Node.Build.make_geometry ?label ~operation:"attribute_transfer_surface" ~version:5
      ~parameters:"" ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|]
      (fun ~node_id:_ context inputs ->
        let target_group_owner = match target_owner with
          | Rdk.Attribute.Point -> Rdk.Group.Point
          | Rdk.Attribute.Vertex -> Rdk.Group.Vertex
          | Rdk.Attribute.Primitive -> Rdk.Group.Primitive
          | Rdk.Attribute.Detail -> assert false in
        let cancel = Context.cancel_token context and grain = Context.grain context in
        match resolve_transfer_group ~operation:"attribute_transfer_surface"
            ~owner:Rdk.Group.Primitive ~owner_name:"primitive"
            ~exact:source_group ~pattern:source_group_pattern_compiled
            ~cancel ~grain inputs.(0) with
        | Error error -> Error error
        | Ok source_primitives ->
            (match resolve_transfer_group ~operation:"attribute_transfer_surface"
                ~owner:Rdk.Group.Vertex ~owner_name:"vertex"
                ~exact:source_vertex_group
                ~pattern:source_vertex_group_pattern_compiled
                ~cancel ~grain inputs.(0) with
             | Error error -> Error error
             | Ok source_vertices ->
            (match resolve_transfer_group ~operation:"attribute_transfer_surface"
                ~owner:target_group_owner
                ~owner_name:(if target_group_owner = Rdk.Group.Primitive then "primitive"
                  else if target_group_owner = Rdk.Group.Vertex then "vertex" else "point")
                ~exact:target_group ~pattern:target_group_pattern_compiled
                ~cancel ~grain inputs.(1) with
             | Error error -> Error error
             | Ok target_points ->
                 match Rdk.Attribute_ops.transfer_surface
                     { cancel = Some cancel; grain = Some grain; max_distance;
                       blend_width; falloff; unmatched; target_owner;
                       distance_attribute; source_primitives; source_vertices;
                       source_vertex_selection; target_elements = target_points;
                       attributes; source = inputs.(0); target = inputs.(1) } with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error)))
  )
  let factory = parameters_factory build
end






module Attribute_transfer = struct
  let vertex_selection_parameter = Param.choice ~equal:( = ) [
      "All triangle vertices", Rdk.Surface_index.All_triangle_vertices;
      "Any triangle vertex", Rdk.Surface_index.Any_triangle_vertex;
    ]
  let decode_names text =
    let* rows = parameter_table_rows "sop/attribute_transfer" text in
    map_rows (function [name] -> Ok name | _ ->
      Error "sop/attribute_transfer: exact names need one column per row") rows
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    use_names : bool [@sop.default false] [@sop.label "Use exact names"];
    names : string [@sop.default ""] [@sop.label "Exact attribute names"];
    pattern : string [@sop.default "*"] [@sop.label "Attributes"];
    mode : transfer_mode [@sop.default Transfer_nearest]
      [@sop.label "Transfer mode"] [@sop.kind transfer_mode_parameter];
    neighbors : int [@sop.default 4] [@sop.label "Neighbors"]
      [@sop.folder "Sampling"] [@sop.min 1] [@sop.max 128]
      [@sop.hard_min 1];
    power : float [@sop.default 2.] [@sop.label "Inverse power"]
      [@sop.folder "Sampling"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    kernel_radius : float [@sop.default 1.] [@sop.label "Kernel radius"]
      [@sop.folder "Sampling"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    distance_mode : kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Distance mode"] [@sop.folder "Distance"] [@sop.kind kernel_mode_parameter];
    max_distance : float [@sop.default 1.] [@sop.label "Maximum distance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    blend_width : float [@sop.default 0.] [@sop.label "Blend width"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    falloff : transfer_falloff [@sop.default Transfer_linear]
      [@sop.label "Falloff"] [@sop.folder "Distance"]
      [@sop.kind transfer_falloff_parameter];
    uniform_bias : float [@sop.default 0.5] [@sop.label "Uniform bias"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    unmatched : Rdk.Attribute_transfer.unmatched
      [@sop.default Rdk.Attribute_transfer.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Groups/Source"];
    source_group_pattern : string [@sop.default ""]
      [@sop.label "Source group pattern"] [@sop.folder "Groups/Source"];
    source_vertex_group : string [@sop.default ""]
      [@sop.label "Source vertex group"] [@sop.folder "Groups/Source"];
    source_vertex_group_pattern : string [@sop.default ""]
      [@sop.label "Source vertex pattern"] [@sop.folder "Groups/Source"];
    source_vertex_selection : Rdk.Surface_index.vertex_selection
      [@sop.default Rdk.Surface_index.All_triangle_vertices]
      [@sop.label "Vertex selection"] [@sop.folder "Groups/Source"]
      [@sop.kind vertex_selection_parameter];
    target_group : string [@sop.default ""] [@sop.label "Target group"]
      [@sop.folder "Groups/Target"];
    target_group_pattern : string [@sop.default ""]
      [@sop.label "Target group pattern"] [@sop.folder "Groups/Target"];
  } [@@sop.node_key "attribute_transfer"]
    [@@sop.node_label "Attribute Transfer"]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@sop.validate fun parameters ->
      let refuse message = Error ("sop/attribute_transfer: " ^ message) in
      let* () = if parameters.use_names then begin
        let* names = decode_names parameters.names in
        let* () = each (fun name ->
          if String.trim name = "" then refuse "empty attribute name" else
          if parameters.owner = Rdk.Attribute.Point && name = "P" then
            refuse "canonical P cannot be transferred as an ordinary attribute" else Ok ()) names in
        if List.length (List.sort_uniq String.compare names) <> List.length names then
          refuse "duplicate attribute name" else Ok ()
      end else validate_parameter_pattern "sop/attribute_transfer" parameters.pattern in
      let* () = if parameters.neighbors < 1 then refuse "neighbors must be positive" else Ok () in
      let* () = if parameters.power < 0. || (parameters.mode = Transfer_inverse && parameters.power = 0.) then
        refuse "invalid inverse power" else Ok () in
      let* () = if parameters.kernel_radius < 0. || parameters.kernel_radius > sqrt Float.max_float
          || parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float
          || parameters.blend_width < 0. then refuse "distances must be nonnegative and safely squarable" else Ok () in
      let* () = if parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "uniform bias must be within [0,1]" else Ok () in
      let* () = if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then
        refuse "blend width requires an explicit maximum distance" else Ok () in
      let* () = if parameters.distance_mode = Kernel_explicit &&
          parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance plus blend width must be safely squarable" else Ok () in
      let source_group,source_pattern=exact_or_pattern parameters.source_group parameters.source_group_pattern
      and vertex_group,vertex_pattern=exact_or_pattern parameters.source_vertex_group parameters.source_vertex_group_pattern
      and target_group,target_pattern=exact_or_pattern parameters.target_group parameters.target_group_pattern in
      let* () = each (fun (name,pattern) -> Result.map ignore (compile_transfer_group_pattern "attribute_transfer" name pattern))
        ["source",source_pattern;"source vertex",vertex_pattern;"target",target_pattern] in
      let* () = if parameters.owner <> Rdk.Attribute.Vertex && (vertex_group <> None || vertex_pattern <> None) then
        refuse "source vertex groups require vertex ownership" else Ok () in
      let* () = if parameters.owner = Rdk.Attribute.Detail &&
          (source_group <> None || source_pattern <> None || target_group <> None || target_pattern <> None
           || parameters.distance_mode <> Kernel_auto || parameters.blend_width <> 0.) then
        refuse "detail transfer does not accept spatial options" else Ok () in
      if (parameters.owner = Rdk.Attribute.Detail || parameters.owner = Rdk.Attribute.Vertex)
          && parameters.mode <> Transfer_nearest then refuse "this owner supports nearest mode only" else Ok ()]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters source target ->
    let owner=parameters.owner and source_vertex_selection=parameters.source_vertex_selection in
    let names,pattern = if parameters.use_names then Some (validated (decode_names parameters.names)),None
      else None,Some parameters.pattern in
    let mode=transfer_mode parameters.mode parameters.neighbors parameters.power parameters.kernel_radius
    and falloff=transfer_falloff parameters.falloff parameters.uniform_bias
    and unmatched=parameters.unmatched and blend_width=parameters.blend_width in
    let max_distance=match parameters.distance_mode with Kernel_auto -> None | Kernel_explicit -> Some parameters.max_distance in
    let source_group,source_group_pattern=exact_or_pattern parameters.source_group parameters.source_group_pattern
    and source_vertex_group,source_vertex_group_pattern=exact_or_pattern parameters.source_vertex_group parameters.source_vertex_group_pattern
    and target_group,target_group_pattern=exact_or_pattern parameters.target_group parameters.target_group_pattern in
    let source_group_pattern_compiled=validated (compile_transfer_group_pattern "attribute_transfer" "source" source_group_pattern)
    and source_vertex_group_pattern_compiled=validated (compile_transfer_group_pattern "attribute_transfer" "source vertex" source_vertex_group_pattern)
    and target_group_pattern_compiled=validated (compile_transfer_group_pattern "attribute_transfer" "target" target_group_pattern) in
    Node.Build.make_geometry ~label ~operation:"attribute_transfer" ~version:6
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs:[|source;target|]
    (fun ~node_id:_ context inputs ->
      let source_group_owner, target_group_owner, source_owner_name,
          target_owner_name = match owner with
        | Rdk.Attribute.Point ->
            Rdk.Group.Point, Rdk.Group.Point, "point", "point"
        | Rdk.Attribute.Vertex ->
            Rdk.Group.Primitive, Rdk.Group.Vertex, "primitive", "vertex"
        | Rdk.Attribute.Primitive ->
            Rdk.Group.Primitive, Rdk.Group.Primitive, "primitive", "primitive"
        | Rdk.Attribute.Detail ->
            Rdk.Group.Point, Rdk.Group.Point, "detail", "detail" in
      let cancel = Context.cancel_token context and grain = Context.grain context in
      match resolve_transfer_group ~operation:"attribute_transfer"
          ~owner:source_group_owner ~owner_name:source_owner_name
          ~exact:source_group ~pattern:source_group_pattern_compiled
          ~cancel ~grain inputs.(0) with
      | Error error -> Error error
      | Ok source_elements ->
          (match resolve_transfer_group ~operation:"attribute_transfer"
              ~owner:Rdk.Group.Vertex ~owner_name:"vertex"
              ~exact:source_vertex_group
              ~pattern:source_vertex_group_pattern_compiled
              ~cancel ~grain inputs.(0) with
           | Error error -> Error error
           | Ok source_vertices ->
          (match resolve_transfer_group ~operation:"attribute_transfer"
              ~owner:target_group_owner ~owner_name:target_owner_name
              ~exact:target_group ~pattern:target_group_pattern_compiled
              ~cancel ~grain inputs.(1) with
           | Error error -> Error error
           | Ok target_elements ->
               let result = match owner with
                 | Rdk.Attribute.Point ->
                     Rdk.Attribute_ops.transfer_points
                       { cancel = Some cancel; grain = Some grain; names; pattern;
                         mode; max_distance; blend_width; falloff; unmatched;
                         source_elements; target_elements;
                         source = inputs.(0); target = inputs.(1) }
                 | Rdk.Attribute.Vertex ->
                     Rdk.Attribute_ops.transfer_vertices
                       { cancel = Some cancel; grain = Some grain; names; pattern;
                         max_distance; blend_width; falloff; unmatched;
                         source_primitives = source_elements; source_vertices;
                         source_vertex_selection;
                         target_vertices = target_elements;
                         source = inputs.(0); target = inputs.(1) }
                 | Rdk.Attribute.Primitive ->
                     Rdk.Attribute_ops.transfer_primitives
                       { cancel = Some cancel; grain = Some grain; names; pattern;
                         mode; max_distance; blend_width; falloff; unmatched;
                         source_elements; target_elements;
                         source = inputs.(0); target = inputs.(1) }
                 | Rdk.Attribute.Detail ->
                     Rdk.Attribute_ops.transfer_detail { names; pattern;
                       source = inputs.(0); target = inputs.(1) } in
               match result with
               | Ok geometry -> cooked geometry
               | Error error -> structured_rdk_error error)))
  )
  let factory = parameters_factory build
end


