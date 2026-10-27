(* Attribute nodes, declared once: the record is the editor schema, the
   factory and the typed [Sop] constructor. *)

open Support

(* ocamldep must see the PPX's [Sop.X] resolve inside this library. *)
module Sop = Support.Sop

module Attr_from_image = struct
  let channel_parameter = Parameter.choice ~equal:( = ) [
    "r", Rdk.Attribute_ops.Red; "g", Rdk.Attribute_ops.Green;
    "b", Rdk.Attribute_ops.Blue; "a", Rdk.Attribute_ops.Alpha;
    "luminance", Rdk.Attribute_ops.Luminance]
  type parameters = {
    attribute : string [@sop.default "image"] [@sop.label "Attribute"]
      [@sop.primary];
    channel : Rdk.Attribute_ops.image_channel [@sop.default Rdk.Attribute_ops.Red]
      [@sop.label "Channel"] [@sop.primary] [@sop.kind channel_parameter];
    uv : string [@sop.default "uv"] [@sop.label "UV attribute"];
  } [@@sop.node_key "attr_from_image"] [@@sop.node_label "Attribute from Image"]
    [@@sop.node_category "Attribute/Image"] [@@sop.node_inputs 2]
    [@@sop.node_slots "geometry, image"] [@@sop.node_types "geometry, image"]
    [@@sop.node_facts {elementwise = Node.Points; reads = [parameters.uv];
      writes = [parameters.attribute]; topology = Node.Preserved; exact = true}]
    [@@sop.validate fun parameters -> if parameters.attribute = "P" then
      invalid_arg "sop/attr_from_image: attribute cannot be P"]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters geometry image ->
    Node.Private.make ~label ~operation:"attr_from_image" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|geometry; image|] (fun ~node_id:_ context inputs ->
        Result.bind (Payload.geometry inputs.(0)) (fun geometry ->
          Result.bind (Payload.image inputs.(1)) (fun image ->
            match Rdk.Attribute_ops.from_image ~cancel:(Context.cancel_token context)
              ~grain:(Context.grain context) ~uv:parameters.uv ~attribute:parameters.attribute
              ~channel:parameters.channel ~width:(Image.width image) ~height:(Image.height image)
              ~rgba:(Image.Private.storage image) geometry with
            | Error error -> structured_rdk_error error
            | Ok geometry -> Ok Node.Private.{payload = Payload.Geometry geometry;
                diagnostics = []; instances = None}))))
  let factory = parameters_factory build
end

(* The decoded RGBA floats of an image as a carrier geometry, so a Lisp body can sample pixels:
   one point per texel in row-major order, P = (x, y, width), and the Point Float4 attribute "rgba". *)
module Image_texels = struct
  type parameters = unit
    [@@sop.node_key "image_texels"] [@@sop.node_label "Image Texels"]
    [@@sop.node_category "Attribute/Image"] [@@sop.node_inputs 1]
    [@@sop.node_slots "image"] [@@sop.node_types "image"]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label _parameters image ->
    Node.Private.make ~label ~operation:"image_texels" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[|image|] (fun ~node_id:_ _context inputs ->
        Result.bind (Payload.image inputs.(0)) (fun image ->
          let width = Image.width image and rgba = Image.Private.storage image in
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
                  | Ok geometry -> Ok Node.Private.{payload = Payload.Geometry geometry; diagnostics = []; instances = None}))))
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
    Node.Private.make_geometry ?label ~operation:"uv_flatten" ~version:1
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
module Rest_position = struct
  let mode_parameter = Parameter.choice ~equal:( = ) [
      "Store", Rdk.Motion.Store_rest; "Extract", Rdk.Motion.Extract_rest;
      "Swap", Rdk.Motion.Swap_rest;
    ]
  let normals_parameter = Parameter.choice ~equal:( = ) [
      "None", Rdk.Motion.No_rest_normals;
      "If present", Rdk.Motion.Rest_normals_if_present;
      "Always", Rdk.Motion.Rest_normals_always;
    ]
  type parameters = {
    mode : Rdk.Motion.rest_mode [@sop.default Rdk.Motion.Store_rest]
      [@sop.label "Mode"] [@sop.kind mode_parameter];
    rest_attribute : string [@sop.default "rest"] [@sop.label "Rest position"]
      [@sop.folder "Attributes"];
    normals : Rdk.Motion.rest_normals [@sop.default Rdk.Motion.No_rest_normals]
      [@sop.label "Rest normals"] [@sop.kind normals_parameter];
    normal_attribute : string [@sop.default "N"] [@sop.label "Normal"]
      [@sop.folder "Attributes"];
    rest_normal_attribute : string [@sop.default "restN"]
      [@sop.label "Rest normal"] [@sop.folder "Attributes"];
  } [@@sop.node_key "rest_position"] [@@sop.node_label "Rest Position"]
    [@@sop.node_category "Attribute/Motion"] [@@sop.node_inputs 2] [@@sop.node_slots "input, reference"]
    [@@sop.node_optional "1"]
    [@@sop.validate fun parameters ->
      if parameters.rest_attribute = "P" then invalid_arg "sop/rest_position: rest attribute cannot be P";
      if parameters.normals <> Rdk.Motion.No_rest_normals then
        List.iter (fun name -> if String.trim name = "" || name = "P" then
          invalid_arg "sop/rest_position: normal names must be non-empty and cannot be P")
          [parameters.normal_attribute; parameters.rest_normal_attribute]]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input reference ->
    let inputs, cook_mode = match reference with
      | None -> [|input|], Node.Duplicate_input 0
      | Some reference -> [|input; reference|], Node.Generic in
    Node.Private.make_geometry ~label ~operation:"rest_position" ~version:1 ~parameters:""
      ~cook_mode ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        let reference = if Array.length inputs = 2 then Some inputs.(1) else None in
        match Rdk.Motion.rest_position ~cancel:(Context.cancel_token context)
          ~grain:(Context.grain context) ?reference
          ~rest_attribute:parameters.rest_attribute ~normals:parameters.normals
          ~normal_attribute:parameters.normal_attribute
          ~rest_normal_attribute:parameters.rest_normal_attribute
          parameters.mode inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
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
    Node.Private.make_geometry ?label ~operation:"uv_relax" ~version:1
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

module Rename_attributes = struct
  let conflict_token = function
    | Rdk.Attribute_ops.Attribute_rename_skip -> "skip"
    | Rdk.Attribute_ops.Attribute_rename_error -> "error"
    | Rdk.Attribute_ops.Attribute_rename_overwrite -> "overwrite"
  let conflict_of_token = function
    | "skip" -> Ok Rdk.Attribute_ops.Attribute_rename_skip
    | "error" -> Ok Rdk.Attribute_ops.Attribute_rename_error
    | "overwrite" -> Ok Rdk.Attribute_ops.Attribute_rename_overwrite
    | token -> Error (Printf.sprintf
        "unknown attribute rename conflict %S" token)
  let encode_rule (rule : Rdk.Attribute_ops.rename_rule) = [
      (match rule.rename_attribute_owner with None -> "any"
       | Some owner -> attribute_owner_token owner);
      rule.rename_attribute_pattern; rule.rename_attribute_replacement;
      conflict_token rule.rename_attribute_conflict;
    ]
  let decode_rule = function
    | [owner; pattern; replacement; conflict] ->
        let owner = String.lowercase_ascii (String.trim owner)
        and conflict = String.lowercase_ascii (String.trim conflict) in
        let owner = if owner = "any" || owner = "*" then Ok None
          else Result.map Option.some (attribute_owner_of_token owner) in
        Result.bind owner (fun rename_attribute_owner ->
          Result.map (fun rename_attribute_conflict -> {
            Rdk.Attribute_ops.rename_attribute_owner;
            rename_attribute_pattern = pattern;
            rename_attribute_replacement = replacement;
            rename_attribute_conflict }) (conflict_of_token conflict))
    | row -> Error (Printf.sprintf
        "Attribute Rename rule needs owner, pattern, replacement, and conflict; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Parameter.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    rules : Rdk.Attribute_ops.rename_rule list [@sop.default []]
      [@sop.label "Rules (owner, pattern, replacement, conflict)"]
      [@sop.kind rules_parameter];
  } [@@sop.node_key "rename_attributes"]
    [@@sop.node_operation "attribute_rename_pattern"]
    [@@sop.node_label "Rename Attributes"]
    [@@sop.node_category "Attribute/Manage"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let rules = parameters.rules in
    let rules = List.map (fun (rule : Rdk.Attribute_ops.rename_rule) ->
        match Rdk.Attribute_pattern.compile_rewrite
            ~pattern:rule.rename_attribute_pattern
            ~replacement:rule.rename_attribute_replacement with
        | Ok _ -> rule
        | Error message -> invalid_arg ("sop/rename_attributes: " ^ message))
        rules in
    Node.Private.make_geometry ?label ~operation:"attribute_rename_pattern" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Attribute_ops.rename ~cancel:(Context.cancel_token context)
            ~rules inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Measure = struct
  let kind_parameter = Parameter.choice ~equal:( = ) [
      "Perimeter", Rdk.Analysis.Perimeter;
      "Area", Rdk.Analysis.Area;
      "Signed volume", Rdk.Analysis.Signed_volume;
    ]
  let accumulation_parameter = Parameter.choice ~equal:( = ) [
      "Per element", Rdk.Analysis.Per_element;
      "Throughout", Rdk.Analysis.Throughout;
    ]
  type parameters = {
    kind : Rdk.Analysis.measure [@sop.default Rdk.Analysis.Area]
      [@sop.label "Measure"] [@sop.kind kind_parameter];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    accumulation : Rdk.Analysis.accumulation
      [@sop.default Rdk.Analysis.Per_element]
      [@sop.label "Accumulation"] [@sop.kind accumulation_parameter];
    attribute : string [@sop.default ""] [@sop.label "Attribute"]
      [@sop.folder "Output"];
    total_attribute : string [@sop.default ""]
      [@sop.label "Total attribute"] [@sop.folder "Output"];
  } [@@sop.node_key "measure"] [@@sop.node_label "Measure"]
    [@@sop.node_category "Attribute/Analysis"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let accumulation = parameters.accumulation in
    let name = optional_text parameters.attribute in
    let total_name = optional_text parameters.total_attribute in
    let kind = parameters.kind in
    Node.Private.make_geometry ?label ~operation:"measure" ~version:1
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
                   (Printf.sprintf "measure could not find primitive group %S" name))) in
        match primitives with
        | Error error -> Error error
        | Ok primitives ->
            (match Rdk.Analysis.with_measure ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ~accumulation ?name
                ?total_name kind geometry with
             | Ok geometry -> cooked geometry
             | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Uv_unitize = struct
  let mode_parameter = Parameter.choice ~equal:( = ) [
      "Per face", Rdk.Uv_ops.Per_face; "Islands", Rdk.Uv_ops.Islands;
    ]
  type parameters = {
    mode : Rdk.Uv_ops.unitize_mode [@sop.default Rdk.Uv_ops.Per_face]
      [@sop.label "Mode"] [@sop.kind mode_parameter];
    name : string [@sop.default "uv"] [@sop.label "UV attribute"];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    seams : string [@sop.default ""] [@sop.label "Seam group"];
    tolerance : float [@sop.default 1e-9] [@sop.label "Tolerance"]
      [@sop.min 0.] [@sop.max 0.01] [@sop.hard_min 0.];
    uniform : bool [@sop.default true] [@sop.label "Uniform scale"];
  } [@@sop.node_key "uv_unitize"] [@@sop.node_label "UV Unitize"]
    [@@sop.node_category "UV/Layout"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let group = optional_text parameters.group in
    let seams = optional_text parameters.seams in
    let tolerance = parameters.tolerance in
    let uniform = parameters.uniform in
    let mode = parameters.mode in
    Node.Private.make_geometry ?label ~operation:"uv_unitize" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let find_group owner kind = function
          | None -> Ok None
          | Some group_name ->
              (match Rdk.Geometry.find_group ~owner group_name inputs.(0) with
               | Some value -> Ok (Some value)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   ~hints:["Create the named group before UV Unitize"]
                   (Printf.sprintf "uv_unitize could not find %s group %S"
                     kind group_name))) in
        let find_seams = function
          | None -> Ok (None, None)
          | Some group_name ->
              (match Rdk.Geometry.find_edge_group group_name inputs.(0) with
               | Some value -> Ok (Some value, None)
               | None ->
                   match Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex
                       group_name inputs.(0) with
                   | Some value -> Ok (None, Some value)
                   | None -> Error (Diagnostic.error ~code:"missing_group"
                       ~hints:["Create a native edge group or compatibility vertex-edge group before UV Unitize"]
                       (Printf.sprintf
                         "uv_unitize could not find edge or vertex seam group %S"
                         group_name))) in
        match find_group Rdk.Group.Primitive "primitive" group, find_seams seams with
        | Error _ as error, _ | _, (Error _ as error) -> error
        | Ok primitives, Ok (edge_seams, seams) ->
            match Rdk.Uv_ops.unitize ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?primitives ?seams
                ?edge_seams
                ~tolerance ~uniform mode inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Swap_attributes = struct
  let method_token = function
    | Rdk.Attribute_ops.Attribute_swap -> "swap"
    | Rdk.Attribute_ops.Attribute_move -> "move"
    | Rdk.Attribute_ops.Attribute_copy -> "copy"
  let method_of_token = function
    | "swap" -> Ok Rdk.Attribute_ops.Attribute_swap
    | "move" -> Ok Rdk.Attribute_ops.Attribute_move
    | "copy" -> Ok Rdk.Attribute_ops.Attribute_copy
    | token -> Error (Printf.sprintf "unknown attribute swap method %S" token)
  let encode_rule (rule : Rdk.Attribute_ops.swap_rule) = [
      attribute_owner_token rule.swap_attribute_owner;
      rule.swap_attribute_source; rule.swap_attribute_destination;
      method_token rule.swap_attribute_method;
    ]
  let decode_rule = function
    | [owner; source; destination; method_] ->
        Result.bind (attribute_owner_of_token
          (String.lowercase_ascii (String.trim owner)))
          (fun swap_attribute_owner ->
            Result.map (fun swap_attribute_method -> {
              Rdk.Attribute_ops.swap_attribute_owner;
              swap_attribute_source = source;
              swap_attribute_destination = destination;
              swap_attribute_method })
              (method_of_token
                (String.lowercase_ascii (String.trim method_))))
    | row -> Error (Printf.sprintf
        "Attribute Swap rule needs owner, source, destination, and method; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun rules ->
        Result.map (fun rule -> rule :: rules) (decode_rule row))) (Ok []) rows
      |> Result.map List.rev)
  let rules_parameter = Parameter.encoded ~equal:( = )
      ~encode:(fun rules -> encode_table (List.map encode_rule rules)) ~decode
  type parameters = {
    rules : Rdk.Attribute_ops.swap_rule list [@sop.default []]
      [@sop.label "Rules (owner, source, destination, method)"]
      [@sop.kind rules_parameter];
  } [@@sop.node_key "swap_attributes"]
    [@@sop.node_operation "attribute_swap"]
    [@@sop.node_label "Swap Attributes"]
    [@@sop.node_category "Attribute/Manage"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let rules = parameters.rules in
    let rules = List.map (fun (rule : Rdk.Attribute_ops.swap_rule) ->
        let validate pattern replacement =
          match Rdk.Attribute_pattern.compile_rewrite ~pattern ~replacement with
          | Ok _ -> ()
          | Error message -> invalid_arg ("sop/swap_attributes: " ^ message) in
        validate rule.swap_attribute_source rule.swap_attribute_destination;
        validate rule.swap_attribute_destination rule.swap_attribute_source;
        rule) rules in
    Node.Private.make_geometry ?label ~operation:"attribute_swap" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Attribute_ops.swap ~cancel:(Context.cancel_token context)
            ~rules inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Material = struct
  open Rays_math
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    material : string [@sop.default ""] [@sop.label "Material"];
    color_r : float [@sop.default 1.] [@sop.label "Red"] [@sop.vec3 "color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    color_g : float [@sop.default 1.] [@sop.label "Green"] [@sop.vec3 "color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    color_b : float [@sop.default 1.] [@sop.label "Blue"] [@sop.vec3 "color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    roughness : float [@sop.default 0.4] [@sop.label "Roughness"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    round : float [@sop.default 0.] [@sop.label "Round corners"]
      [@sop.min 0.] [@sop.max 0.5] [@sop.hard_min 0.];
    emission_r : float [@sop.default 0.] [@sop.label "Emission red"] [@sop.vec3 "emission"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    emission_g : float [@sop.default 0.] [@sop.label "Emission green"] [@sop.vec3 "emission"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
    emission_b : float [@sop.default 0.] [@sop.label "Emission blue"] [@sop.vec3 "emission"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
  } [@@sop.node_key "material"] [@@sop.node_label "Material"]
    [@@sop.node_category "Attribute/Material"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let name = parameters.material in
    let color = Vec3.create parameters.color_r parameters.color_g parameters.color_b in
    let roughness = parameters.roughness in
    let emission = Vec3.create parameters.emission_r parameters.emission_g parameters.emission_b in
    Node.Private.make_geometry ?label ~operation:"material" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        Rdk.Material_assign.run ~cancel:(Context.cancel_token context) ?group ~name
          ~color:(color.Vec3.x, color.y, color.z) ~roughness ~round:parameters.round
          ~emission:(emission.Vec3.x, emission.y, emission.z) inputs.(0)
        |> function Ok geometry -> cooked geometry | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Delete_attributes = struct
  type parameters = {
    delete_non_selected : bool [@sop.default false]
      [@sop.label "Delete non-selected"];
    point_pattern : string [@sop.default ""] [@sop.label "Point attributes"]
      [@sop.folder "Patterns"];
    vertex_pattern : string [@sop.default ""] [@sop.label "Vertex attributes"]
      [@sop.folder "Patterns"];
    primitive_pattern : string [@sop.default ""]
      [@sop.label "Primitive attributes"] [@sop.folder "Patterns"];
    detail_pattern : string [@sop.default ""] [@sop.label "Detail attributes"]
      [@sop.folder "Patterns"];
  } [@@sop.node_key "delete_attributes"]
    [@@sop.node_operation "attribute_delete_pattern"]
    [@@sop.node_label "Delete Attributes"]
    [@@sop.node_category "Attribute/Manage"] [@@sop.node_inputs 2] [@@sop.node_slots "input, reference"]
    [@@sop.node_optional "1"]
    [@@sop.validate fun parameters ->
      List.iter (fun pattern ->
        if String.trim pattern <> "" then
          match Rdk.Attribute_pattern.compile pattern with
          | Ok _ -> ()
          | Error message -> invalid_arg ("sop/delete_attributes: " ^ message))
        [parameters.point_pattern; parameters.vertex_pattern;
         parameters.primitive_pattern; parameters.detail_pattern]]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input reference ->
    let label = Some label in
    let delete_non_selected = parameters.delete_non_selected in
    let point_pattern = optional_text parameters.point_pattern in
    let vertex_pattern = optional_text parameters.vertex_pattern in
    let primitive_pattern = optional_text parameters.primitive_pattern in
    let detail_pattern = optional_text parameters.detail_pattern in
    let inputs, cook_mode = match reference with
      | None -> [|input|], Node.Duplicate_input 0
      | Some reference -> [|input; reference|], Node.Generic in
    Node.Private.make_geometry ?label ~operation:"attribute_delete_pattern" ~version:1
      ~parameters:""
      ~cook_mode ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        let reference = if Array.length inputs = 2 then Some inputs.(1) else None in
        match Rdk.Attribute_ops.delete ~cancel:(Context.cancel_token context)
            ?reference ~delete_non_selected ?point_pattern ?vertex_pattern
            ?primitive_pattern ?detail_pattern inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Edge_transport_curves = struct
  type parameters = {
    attribute : string [@sop.default "value"] [@sop.label "Attribute"];
    primitive_group : string [@sop.default ""]
      [@sop.label "Primitive group"];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Attribute owner"] [@sop.kind uv_owner_parameter];
    direction : Rdk.Edge_transport.direction
      [@sop.default Rdk.Edge_transport.Transport_forward] [@sop.label "Direction"]
      [@sop.kind edge_transport_direction_parameter];
    operation : Rdk.Edge_transport.operation
      [@sop.default Rdk.Edge_transport.Transport] [@sop.label "Operation"]
      [@sop.kind edge_transport_operation_parameter];
    root_value : Rdk.Edge_transport.root_value
      [@sop.default Rdk.Edge_transport.Transport_root_zero] [@sop.label "Root value"]
      [@sop.kind edge_transport_root_value_parameter];
    integrate_constant : bool [@sop.default false]
      [@sop.label "Integrate constant"];
    scale_by_edge_length : bool [@sop.default false]
      [@sop.label "Scale by edge length"];
    normalization : Rdk.Edge_transport.normalization
      [@sop.default Rdk.Edge_transport.Transport_no_normalization]
      [@sop.label "Normalization"]
      [@sop.kind edge_transport_normalization_parameter];
  } [@@sop.node_key "edge_transport_curves"]
    [@@sop.node_label "Edge Transport Curves"]
    [@@sop.node_category "Attribute/Transport"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      validate_transport_options "edge_transport_curves" parameters.operation parameters.integrate_constant parameters.scale_by_edge_length;
      if String.trim parameters.attribute = "" || parameters.attribute = "P" then
        invalid_arg "sop/edge_transport_curves: attribute must be non-empty and not P";
      if parameters.owner <> Rdk.Attribute.Point && parameters.owner <> Rdk.Attribute.Vertex then
        invalid_arg "sop/edge_transport_curves: owner must be point or vertex"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let primitive_group = optional_text parameters.primitive_group in
    let owner = parameters.owner in
    let direction = parameters.direction in
    let operation = parameters.operation in
    let root_value = parameters.root_value in
    let integrate_constant = parameters.integrate_constant in
    let scale_by_edge_length = parameters.scale_by_edge_length in
    let normalization = parameters.normalization in
    let attribute = parameters.attribute in
    Node.Private.make_geometry ?label ~operation:"edge_transport_curves" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_primitive_group "edge_transport_curves"
            primitive_group geometry with
        | Error error -> Error error
        | Ok primitives ->
            match Rdk.Edge_transport.run_curves
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ~owner ~direction
                ~operation ~root_value ~integrate_constant ~scale_by_edge_length
                ~normalization ~attribute geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Edge_transport_parent = struct
  type parameters = {
    attribute : string [@sop.default "value"] [@sop.label "Attribute"];
    point_group : string [@sop.default ""] [@sop.label "Point group"];
    parent_attribute : string [@sop.default "parent"]
      [@sop.label "Parent attribute"];
    direction : Rdk.Edge_transport.direction
      [@sop.default Rdk.Edge_transport.Transport_forward] [@sop.label "Direction"]
      [@sop.kind edge_transport_direction_parameter];
    operation : Rdk.Edge_transport.operation
      [@sop.default Rdk.Edge_transport.Transport] [@sop.label "Operation"]
      [@sop.kind edge_transport_operation_parameter];
    root_value : Rdk.Edge_transport.root_value
      [@sop.default Rdk.Edge_transport.Transport_root_zero] [@sop.label "Root value"]
      [@sop.kind edge_transport_root_value_parameter];
    integrate_constant : bool [@sop.default false]
      [@sop.label "Integrate constant"];
    scale_by_edge_length : bool [@sop.default false]
      [@sop.label "Scale by edge length"];
    split : Rdk.Edge_transport.split [@sop.default Rdk.Edge_transport.Transport_copy]
      [@sop.label "Branch split"] [@sop.kind edge_transport_split_parameter];
    merge : Rdk.Edge_transport.merge
      [@sop.default Rdk.Edge_transport.Transport_merge_add]
      [@sop.label "Branch merge"] [@sop.kind edge_transport_merge_parameter];
    normalization : Rdk.Edge_transport.normalization
      [@sop.default Rdk.Edge_transport.Transport_no_normalization]
      [@sop.label "Normalization"]
      [@sop.kind edge_transport_normalization_parameter];
  } [@@sop.node_key "edge_transport_parent"]
    [@@sop.node_label "Edge Transport Parent"]
    [@@sop.node_category "Attribute/Transport"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      validate_transport_options "edge_transport_parent" parameters.operation parameters.integrate_constant parameters.scale_by_edge_length;
      if String.trim parameters.parent_attribute = "" then
        invalid_arg "sop/edge_transport_parent: empty parent attribute name";
      if String.trim parameters.attribute = "" || parameters.attribute = "P" then
        invalid_arg "sop/edge_transport_parent: attribute must be non-empty and not P"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let point_group = optional_text parameters.point_group in
    let parent_attribute = parameters.parent_attribute in
    let direction = parameters.direction in
    let operation = parameters.operation in
    let root_value = parameters.root_value in
    let integrate_constant = parameters.integrate_constant in
    let scale_by_edge_length = parameters.scale_by_edge_length in
    let split = parameters.split in
    let merge = parameters.merge in
    let normalization = parameters.normalization in
    let attribute = parameters.attribute in
    Node.Private.make_geometry ?label ~operation:"edge_transport_parent" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_point_group "edge_transport_parent" point_group
            geometry with
        | Error error -> Error error
        | Ok points ->
            match Rdk.Edge_transport.run_parent
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~parent_attribute ~direction
                ~operation ~root_value ~integrate_constant ~scale_by_edge_length
                ~split ~merge ~normalization ~attribute geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Uv_auto_seam = struct
  type parameters = {
    name : string [@sop.default "uv_seams"] [@sop.label "Seam group"];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    angle : float [@sop.default 1.0471975511965976]
      [@sop.label "Angle threshold"] [@sop.min 0.]
      [@sop.max 3.141592653589793] [@sop.hard_min 0.]
      [@sop.hard_max 3.141592653589793];
    include_boundaries : bool [@sop.default true]
      [@sop.label "Include boundaries"];
    include_non_manifold : bool [@sop.default true]
      [@sop.label "Include non-manifold"];
    partition_attribute : string [@sop.default ""]
      [@sop.label "Partition attribute"] [@sop.folder "Cuts"];
    existing_uv : string [@sop.default ""] [@sop.label "Existing UV"]
      [@sop.folder "Cuts"];
    uv_tolerance : float [@sop.default 1e-9] [@sop.label "UV tolerance"]
      [@sop.folder "Cuts"] [@sop.min 0.] [@sop.max 0.01]
      [@sop.hard_min 0.];
    island_attribute : string [@sop.default ""]
      [@sop.label "Island attribute"] [@sop.folder "Output"];
  } [@@sop.node_key "uv_auto_seam"] [@@sop.node_label "UV Auto Seam"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.name = "" then invalid_arg "sop/uv_auto_seam: empty group name";
      if not (Float.is_finite parameters.angle) || parameters.angle < 0. || parameters.angle > Float.pi then
        invalid_arg "sop/uv_auto_seam: angle must be finite and within [0,pi]";
      if not (Float.is_finite parameters.uv_tolerance) || parameters.uv_tolerance < 0. then
        invalid_arg "sop/uv_auto_seam: UV tolerance must be finite and non-negative"]
    [@@sop.node_category "UV/Seams"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let group = optional_text parameters.group in
    let angle = parameters.angle in
    let include_boundaries = parameters.include_boundaries in
    let include_non_manifold = parameters.include_non_manifold in
    let partition_attribute = optional_text parameters.partition_attribute in
    let existing_uv = optional_text parameters.existing_uv in
    let uv_tolerance = parameters.uv_tolerance in
    let island_attribute = optional_text parameters.island_attribute in
    Node.Private.make_geometry ?label ~operation:"uv_auto_seam" ~version:1
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
                   ~hints:["Create the primitive group before UV Auto Seam"]
                   (Printf.sprintf "uv_auto_seam could not find primitive group %S"
                     group_name))) in
        match primitives with
        | Error _ as error -> error
        | Ok primitives ->
            match Rdk.Uv_ops.auto_seam ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?primitives ~angle
                ~include_boundaries ~include_non_manifold ?partition_attribute
                ?existing_uv ~uv_tolerance ?island_attribute inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_laplacian = struct
  let weighting_parameter = Parameter.choice ~equal:( = ) [
      "Cotangent", Rdk.Laplacian.Laplacian_cotan;
      "Positive cotangent", Rdk.Laplacian.Laplacian_positive_cotan;
      "Uniform", Rdk.Laplacian.Laplacian_uniform;
    ]

  type parameters = {
    point_group : string [@sop.default ""] [@sop.label "Point group"];
    weighting : Rdk.Laplacian.weighting
      [@sop.default Rdk.Laplacian.Laplacian_cotan]
      [@sop.label "Weighting"] [@sop.kind weighting_parameter];
    normalize : bool [@sop.default true] [@sop.label "Normalize"];
    source : string [@sop.default "P"] [@sop.label "Source attribute"];
    output : string [@sop.default "laplacian"] [@sop.label "Output attribute"];
  } [@@sop.node_key "attribute_laplacian"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.output = "P" then
        invalid_arg "sop/attribute_laplacian: output must be non-empty and not P"]
    [@@sop.node_label "Attribute Laplacian"]
    [@@sop.node_category "Attribute/Filter"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let point_group = optional_text parameters.point_group in
    let weighting = parameters.weighting in
    let normalize = parameters.normalize in
    let source = parameters.source in
    let output = parameters.output in
    let output = optional_text output in
    Node.Private.make_geometry ?label ~operation:"attribute_laplacian" ~version:1
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let points = match point_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "attribute_laplacian could not find point group %S" name))) in
        match points with
        | Error error -> Error error
        | Ok points ->
            match Rdk.Laplacian.run
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~weighting ~normalize
                ~source ?output geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Measure_curvature = struct
  let boundary_parameter = Parameter.choice ~equal:( = ) [
      "Zero", Rdk.Curvature.Curvature_boundary_zero;
      "One-sided", Rdk.Curvature.Curvature_boundary_one_sided;
    ]

  type parameters = {
    point_group : string [@sop.default ""] [@sop.label "Point group"];
    boundary : Rdk.Curvature.boundary
      [@sop.default Rdk.Curvature.Curvature_boundary_zero]
      [@sop.label "Boundary"] [@sop.kind boundary_parameter];
    smoothing_iterations : int [@sop.default 0]
      [@sop.label "Smoothing iterations"] [@sop.min 0] [@sop.max 64]
      [@sop.hard_min 0];
    smoothing_strength : float [@sop.default 0.5]
      [@sop.label "Smoothing strength"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    mean : string [@sop.default "curvature"] [@sop.label "Mean"]
      [@sop.folder "Outputs"];
    gaussian : string [@sop.default ""] [@sop.label "Gaussian"]
      [@sop.folder "Outputs"];
    minimum : string [@sop.default ""] [@sop.label "Minimum"]
      [@sop.folder "Outputs"];
    maximum : string [@sop.default ""] [@sop.label "Maximum"]
      [@sop.folder "Outputs"];
    curvedness : string [@sop.default ""] [@sop.label "Curvedness"]
      [@sop.folder "Outputs"];
    shape_index : string [@sop.default ""] [@sop.label "Shape index"]
      [@sop.folder "Outputs"];
  } [@@sop.node_key "measure_curvature"]
    [@@sop.validate fun parameters ->
      if parameters.smoothing_iterations < 0 then
        invalid_arg "sop/measure_curvature: smoothing iterations must be non-negative";
      if not (Float.is_finite parameters.smoothing_strength)
          || parameters.smoothing_strength < 0. || parameters.smoothing_strength > 1. then
        invalid_arg "sop/measure_curvature: smoothing strength must be between zero and one";
      let names = [parameters.mean; parameters.gaussian; parameters.minimum; parameters.maximum;
        parameters.curvedness; parameters.shape_index] |> List.filter_map optional_text in
      if names = [] then invalid_arg "sop/measure_curvature: at least one output must be enabled";
      if List.mem "P" names then invalid_arg "sop/measure_curvature: output names must be non-empty and not P";
      if List.length (List.sort_uniq String.compare names) <> List.length names then
        invalid_arg "sop/measure_curvature: output names must be distinct"]
    [@@sop.node_label "Measure Curvature"]
    [@@sop.node_category "Measure"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let point_group = optional_text parameters.point_group in
    let boundary = parameters.boundary in
    let smoothing_iterations = parameters.smoothing_iterations in
    let smoothing_strength = parameters.smoothing_strength in
    let mean = parameters.mean in
    let gaussian = parameters.gaussian in
    let minimum = parameters.minimum in
    let maximum = parameters.maximum in
    let curvedness = parameters.curvedness in
    let shape_index = parameters.shape_index in
    let outputs : Rdk.Curvature.outputs = {
      mean = optional_text mean; gaussian = optional_text gaussian;
      minimum = optional_text minimum; maximum = optional_text maximum;
      curvedness = optional_text curvedness; shape_index = optional_text shape_index } in
    Node.Private.make_geometry ?label ~operation:"measure_curvature" ~version:1
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let points = match point_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "measure_curvature could not find point group %S" name))) in
        match points with
        | Error error -> Error error
        | Ok points ->
            match Rdk.Curvature.run
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~boundary
                ~smoothing_iterations ~smoothing_strength ~outputs geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Normal = struct
  let owner_parameter = Parameter.choice ~equal:( = ) [
      "Point", Rdk.Attribute.Point;
      "Vertex", Rdk.Attribute.Vertex;
      "Primitive", Rdk.Attribute.Primitive;
      "Detail", Rdk.Attribute.Detail;
    ]

  let weighting_parameter = Parameter.choice ~equal:( = ) [
      "Vertex angle", Rdk.Normal_ops.Vertex_angle;
      "Each vertex", Rdk.Normal_ops.Each_vertex;
      "Face area", Rdk.Normal_ops.Face_area;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"] [@sop.folder "Selection"];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Vertex]
      [@sop.label "Add normals to"] [@sop.kind owner_parameter];
    weighting : Rdk.Normal_ops.weighting
      [@sop.default Rdk.Normal_ops.Vertex_angle]
      [@sop.label "Weighting"] [@sop.kind weighting_parameter];
    cusp_angle : float [@sop.default 3.141592653589793]
      [@sop.label "Cusp angle"] [@sop.min 0.] [@sop.max 3.141592653589793]
      [@sop.hard_min 0.] [@sop.hard_max 3.141592653589793];
    keep_original_zero : bool [@sop.default false]
      [@sop.label "Keep original zero normals"];
    reverse : bool [@sop.default false] [@sop.label "Reverse normals"];
    attribute : string [@sop.default "N"] [@sop.label "Attribute"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "normals"] [@@sop.node_label "Normal"]
    [@@sop.node_facts {elementwise = (match parameters.owner with
        | Rdk.Attribute.Point -> Node.Points | Primitive -> Node.Primitives | Vertex | Detail -> Node.None);
      reads = ["P"; parameters.attribute]; writes = [parameters.attribute];
      topology = Node.Preserved; exact = false}]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.cusp_angle) || parameters.cusp_angle < 0.
          || parameters.cusp_angle > Float.pi then
        invalid_arg "sop/normals: cusp angle must be finite and within [0,pi]"]
    [@@sop.node_category "Attribute"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let owner = parameters.owner in
    let weighting = parameters.weighting in
    let cusp_angle = parameters.cusp_angle in
    let keep_original_zero = parameters.keep_original_zero in
    let reverse = parameters.reverse in
    let attribute = parameters.attribute in
    let selection = optional_element_group group_owner group in
    Node.Private.make_geometry ?label ~operation:"normals" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"normals" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Normal_ops.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~owner ~weighting
                ~cusp_angle ~keep_original_zero ~reverse ~attribute inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end

module Uv_transform = struct
  open Rays_math
  type parameters = {
    name : string [@sop.default "uv"] [@sop.label "UV attribute"];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Vertex]
      [@sop.label "Owner"] [@sop.kind uv_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    translate_u : float [@sop.default 0.] [@sop.label "Translate U"]
      [@sop.folder "Transform/Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.primary]
    translate_v : float [@sop.default 0.] [@sop.label "Translate V"]
      [@sop.folder "Transform/Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.primary]
    scale_u : float [@sop.default 1.] [@sop.label "Scale U"]
      [@sop.folder "Transform/Scale"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.primary]
    scale_v : float [@sop.default 1.] [@sop.label "Scale V"]
      [@sop.folder "Transform/Scale"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.primary]
    angle : float [@sop.default 0.] [@sop.label "Angle"]
      [@sop.folder "Transform"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.primary]
    pivot_u : float [@sop.default 0.5] [@sop.label "Pivot U"]
      [@sop.folder "Transform/Pivot"] [@sop.min (-10.)] [@sop.max 10.];
    pivot_v : float [@sop.default 0.5] [@sop.label "Pivot V"]
      [@sop.folder "Transform/Pivot"] [@sop.min (-10.)] [@sop.max 10.];
  } [@@sop.node_key "uv_transform"] [@@sop.node_label "UV Transform"]
    [@@sop.validate fun parameters ->
      if parameters.owner <> Rdk.Attribute.Point && parameters.owner <> Rdk.Attribute.Vertex then
        invalid_arg "sop/uv_transform: owner must be Point or Vertex";
      if not (List.for_all Float.is_finite [parameters.translate_u; parameters.translate_v;
          parameters.scale_u; parameters.scale_v; parameters.angle; parameters.pivot_u; parameters.pivot_v]) then
        invalid_arg "sop/uv_transform: transform parameters must be finite"]
    [@@sop.node_category "UV/Modify"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let owner = parameters.owner in
    let group = optional_text parameters.group in
    let translate_u = parameters.translate_u in
    let translate_v = parameters.translate_v in
    let scale_u = parameters.scale_u in
    let scale_v = parameters.scale_v in
    let angle = parameters.angle in
    let pivot_u = parameters.pivot_u in
    let pivot_v = parameters.pivot_v in
    let group_owner = match owner with
      | Rdk.Attribute.Point -> Rdk.Group.Point
      | Rdk.Attribute.Vertex -> Rdk.Group.Vertex
      | Rdk.Attribute.Primitive | Rdk.Attribute.Detail ->
          invalid_arg "sop/uv_transform: owner must be Point or Vertex" in
    let translate = Vec2.create translate_u translate_v and scale = Vec2.create scale_u scale_v
    and pivot = Vec2.create pivot_u pivot_v in
    Node.Private.make_geometry ?label ~operation:"uv_transform" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:group_owner name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   ~hints:["Create a group with the same owner as the UV attribute"]
                   (Printf.sprintf "uv_transform could not find %s group %S"
                     (attribute_owner_key owner) name))) in
        match selection with
        | Error _ as error -> error
        | Ok selection ->
            match Rdk.Uv_ops.transform ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?selection ~owner
                ~translate ~scale ~angle ~pivot inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Uv_project = struct
  open Rays_math
  type projection = Support.uv_projection = Planar | Cylindrical | Spherical
  let projection_parameter = Parameter.choice ~equal:( = ) [
      "Planar", Planar; "Cylindrical", Cylindrical; "Spherical", Spherical;
    ]
  type parameters = {
    projection : projection [@sop.default Planar] [@sop.label "Projection"]
      [@sop.kind projection_parameter];
    name : string [@sop.default "uv"] [@sop.label "UV attribute"];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Projection/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Projection/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Projection/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    axis_x : float [@sop.default 0.] [@sop.label "Axis X"]
      [@sop.folder "Projection/Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"]
      [@sop.folder "Projection/Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"]
      [@sop.folder "Projection/Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    seam_x : float [@sop.default 1.] [@sop.label "Seam X"]
      [@sop.folder "Projection/Seam"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "seam"]
    seam_y : float [@sop.default 0.] [@sop.label "Seam Y"]
      [@sop.folder "Projection/Seam"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "seam"]
    seam_z : float [@sop.default 0.] [@sop.label "Seam Z"]
      [@sop.folder "Projection/Seam"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "seam"]
    planar_u_x : float [@sop.default 1.] [@sop.label "U axis X"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_u"]
    planar_u_y : float [@sop.default 0.] [@sop.label "U axis Y"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_u"]
    planar_u_z : float [@sop.default 0.] [@sop.label "U axis Z"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_u"]
    planar_v_x : float [@sop.default 0.] [@sop.label "V axis X"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_v"]
    planar_v_y : float [@sop.default 0.] [@sop.label "V axis Y"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_v"]
    planar_v_z : float [@sop.default 1.] [@sop.label "V axis Z"]
      [@sop.folder "Projection/Planar"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "planar_v"]
    height : float [@sop.default 1.] [@sop.label "Cylinder height"]
      [@sop.folder "Projection/Cylindrical"] [@sop.min 0.01]
      [@sop.max 10.] [@sop.hard_min 0.];
    u_min : float [@sop.default 0.] [@sop.label "U minimum"]
      [@sop.folder "Range/U"] [@sop.min (-10.)] [@sop.max 10.];
    u_max : float [@sop.default 1.] [@sop.label "U maximum"]
      [@sop.folder "Range/U"] [@sop.min (-10.)] [@sop.max 10.];
    v_min : float [@sop.default 0.] [@sop.label "V minimum"]
      [@sop.folder "Range/V"] [@sop.min (-10.)] [@sop.max 10.];
    v_max : float [@sop.default 1.] [@sop.label "V maximum"]
      [@sop.folder "Range/V"] [@sop.min (-10.)] [@sop.max 10.];
    fix_seams : bool [@sop.default true] [@sop.label "Fix seams"];
    fix_poles : bool [@sop.default true] [@sop.label "Fix poles"];
  } [@@sop.node_key "uv_project"] [@@sop.node_label "UV Project"]
    [@@sop.validate fun parameters ->
      if not (List.for_all Float.is_finite [parameters.origin_x; parameters.origin_y;
          parameters.origin_z; parameters.axis_x; parameters.axis_y; parameters.axis_z;
          parameters.seam_x; parameters.seam_y; parameters.seam_z; parameters.planar_u_x;
          parameters.planar_u_y; parameters.planar_u_z; parameters.planar_v_x;
          parameters.planar_v_y; parameters.planar_v_z; parameters.height;
          parameters.u_min; parameters.u_max; parameters.v_min; parameters.v_max]) then
        invalid_arg "sop/uv_project: projection parameters must be finite";
      if parameters.height < 0. || (parameters.projection = Cylindrical && parameters.height = 0.) then
        invalid_arg "sop/uv_project: cylindrical height must be positive";
      let normalize x y z =
        let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
        if scale = 0. then invalid_arg "sop/uv_project: projection axes must be non-zero";
        Vec3.normalize (Vec3.create (x /. scale) (y /. scale) (z /. scale)) in
      let first, second = match parameters.projection with
        | Planar -> normalize parameters.planar_u_x parameters.planar_u_y parameters.planar_u_z,
            normalize parameters.planar_v_x parameters.planar_v_y parameters.planar_v_z
        | Cylindrical | Spherical -> normalize parameters.axis_x parameters.axis_y parameters.axis_z,
            normalize parameters.seam_x parameters.seam_y parameters.seam_z in
      let cosine = Vec3.dot first second in
      let degenerate = match parameters.projection with
        | Planar -> 1. -. cosine *. cosine <= 1e-12
        | Cylindrical | Spherical -> Vec3.length_sq (Vec3.sub second (Vec3.scale first cosine)) <= 1e-24 in
      if degenerate then invalid_arg "sop/uv_project: projection axes must be non-parallel"]
    [@@sop.node_category "UV/Create"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let name = parameters.name in
    let group = optional_text parameters.group in
    let projection = parameters.projection in
    let origin = Vec3.create parameters.origin_x parameters.origin_y parameters.origin_z in
    let axis = Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let seam = Vec3.create parameters.seam_x parameters.seam_y parameters.seam_z in
    let planar_u = Vec3.create parameters.planar_u_x parameters.planar_u_y parameters.planar_u_z in
    let planar_v = Vec3.create parameters.planar_v_x parameters.planar_v_y parameters.planar_v_z in
    let height = parameters.height in
    let u_min = parameters.u_min in
    let u_max = parameters.u_max in
    let v_min = parameters.v_min in
    let v_max = parameters.v_max in
    let fix_seams = parameters.fix_seams in
    let fix_poles = parameters.fix_poles in
    let projection = match projection with
      | Planar -> Rdk.Uv_ops.Planar { origin; u_axis = planar_u; v_axis = planar_v }
      | Cylindrical -> Rdk.Uv_ops.Cylindrical { origin; axis; seam; height }
      | Spherical -> Rdk.Uv_ops.Spherical { origin; axis; seam } in
    let projection = uv_projection_copy projection in
    let u_range = u_min, u_max and v_range = v_min, v_max in
    Node.Private.make_geometry ?label ~operation:"uv_project" ~version:1
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
                   ~hints:["Create the primitive group before UV Project"]
                   (Printf.sprintf "uv_project could not find primitive group %S"
                     name))) in
        match primitives with
        | Error _ as error -> error
        | Ok primitives ->
            match Rdk.Uv_ops.project ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~name ?primitives ~u_range ~v_range
                ~fix_seams ~fix_poles projection inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Color_by_height = struct
  open Rays_math
  type parameters = {
    low_red : int [@sop.default 32] [@sop.label "Red"]
      [@sop.folder "Low color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    low_green : int [@sop.default 64] [@sop.label "Green"]
      [@sop.folder "Low color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    low_blue : int [@sop.default 192] [@sop.label "Blue"]
      [@sop.folder "Low color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    low_alpha : int [@sop.default 255] [@sop.label "Alpha"]
      [@sop.folder "Low color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    high_red : int [@sop.default 255] [@sop.label "Red"]
      [@sop.folder "High color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    high_green : int [@sop.default 160] [@sop.label "Green"]
      [@sop.folder "High color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    high_blue : int [@sop.default 32] [@sop.label "Blue"]
      [@sop.folder "High color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
    high_alpha : int [@sop.default 255] [@sop.label "Alpha"]
      [@sop.folder "High color"] [@sop.min 0] [@sop.max 255]
      [@sop.hard_min 0] [@sop.hard_max 255];
  } [@@sop.node_key "color_by_height"] [@@sop.node_label "Color by Height"]
    [@@sop.node_facts {elementwise = Node.Points; reads = ["P"]; writes = ["Cd"];
      topology = Node.Preserved; exact = false}]
    [@@sop.validate fun parameters ->
      if List.exists (fun value -> value < 0 || value > 255)
          [parameters.low_red; parameters.low_green; parameters.low_blue; parameters.low_alpha;
           parameters.high_red; parameters.high_green; parameters.high_blue; parameters.high_alpha] then
        invalid_arg "sop/color_by_height: channels must be in 0..255"]
    [@@sop.node_category "Attribute/Color"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let low_red = parameters.low_red in
    let low_green = parameters.low_green in
    let low_blue = parameters.low_blue in
    let low_alpha = parameters.low_alpha in
    let high_red = parameters.high_red in
    let high_green = parameters.high_green in
    let high_blue = parameters.high_blue in
    let high_alpha = parameters.high_alpha in
    let low = Color.rgba low_red low_green low_blue low_alpha
    and high = Color.rgba high_red high_green high_blue high_alpha in
    Node.Private.make_geometry ?label ~operation:"color_by_height" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Color_by_height.run ~grain:(Context.grain context)
            ~cancel:(Context.cancel_token context)
            ~low:(Color.to_floats low) ~high:(Color.to_floats high) inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Graph_color = struct
  let connectivity_parameter = Parameter.choice ~equal:( = ) [
      "Primitives by point", Rdk.Graph_color.Graph_primitives_by_point;
      "Points by primitive", Rdk.Graph_color.Graph_points_by_primitive;
      "Primitives by edge", Rdk.Graph_color.Graph_primitives_by_edge;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_primitive]
      [@sop.label "Group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    connectivity : Rdk.Graph_color.connectivity
      [@sop.default Rdk.Graph_color.Graph_primitives_by_point]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];
    color_attribute : string [@sop.default "color"]
      [@sop.label "Color attribute"];
    sort_output : bool [@sop.default false] [@sop.label "Sort output"];
    output_worksets : bool [@sop.default false]
      [@sop.label "Output worksets"];
    workset_begin_attribute : string [@sop.default "workset_begin"]
      [@sop.label "Begin attribute"] [@sop.folder "Worksets"];
    workset_length_attribute : string [@sop.default "workset_length"]
      [@sop.label "Length attribute"] [@sop.folder "Worksets"];
  } [@@sop.node_key "graph_color"] [@@sop.node_label "Graph Color"]
    [@@sop.validate fun parameters ->
      let validate_name label name =
        if String.trim name = "" || String.equal name "P" then
          invalid_arg ("sop/graph_color: " ^ label ^ " must be non-empty and not P") in
      validate_name "color attribute" parameters.color_attribute;
      if parameters.output_worksets then begin
        validate_name "workset begin attribute" parameters.workset_begin_attribute;
        validate_name "workset length attribute" parameters.workset_length_attribute;
        if parameters.workset_begin_attribute = parameters.workset_length_attribute then
          invalid_arg "sop/graph_color: workset attribute names must be distinct";
        if not parameters.sort_output then
          invalid_arg "sop/graph_color: worksets require sorted output"
      end]
    [@@sop.node_category "Attribute"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = optional_text parameters.group in
    let connectivity = parameters.connectivity in
    let color_attribute = parameters.color_attribute in
    let sort_output = parameters.sort_output in
    let output_worksets = parameters.output_worksets in
    let workset_begin_attribute = parameters.workset_begin_attribute in
    let workset_length_attribute = parameters.workset_length_attribute in
    let selection = optional_element_group group_owner (Option.value ~default:"" group) in
    let worksets = if output_worksets then Some {
      Rdk.Graph_color.begin_attribute = workset_begin_attribute;
      length_attribute = workset_length_attribute } else None in
    Node.Private.make_geometry ?label ~operation:"graph_color" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"graph_color" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Graph_color.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~connectivity
                ~color_attribute ~sort_output ?worksets inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Set_float = struct
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "value"] [@sop.label "Attribute"];
    value : float [@sop.default 0.] [@sop.label "Value"]
      [@sop.min (-10.)] [@sop.max 10.];
  } [@@sop.node_key "set_float"] [@@sop.node_label "Set Float"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.name = "" then invalid_arg "sop/set_float: empty name";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then
        invalid_arg "sop/set_float: cannot replace canonical P";
      if not (Float.is_finite parameters.value) then invalid_arg "sop/set_float: value must be finite"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let value = parameters.value in
    Node.Private.make_geometry ?label ~operation:"set_float" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        set_attribute_cook ~operation:"set_float" ~owner ~name
          (fun count -> Ok (Rdk.Attribute.Float (Array.make count value))) inputs.(0))
  )
  let factory = parameters_factory build
end

module Set_int = struct
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "value"] [@sop.label "Attribute"];
    value : int [@sop.default 0] [@sop.label "Value"]
      [@sop.min (-100)] [@sop.max 100];
  } [@@sop.node_key "set_int"] [@@sop.node_label "Set Integer"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.name = "" then invalid_arg "sop/set_int: empty name";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then
        invalid_arg "sop/set_int: cannot replace canonical P"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let value = parameters.value in
    Node.Private.make_geometry ?label ~operation:"set_int" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        set_attribute_cook ~operation:"set_int" ~owner ~name
          (fun count -> Ok (Rdk.Attribute.Int (Array.make count value))) inputs.(0))
  )
  let factory = parameters_factory build
end

module Set_vector = struct
  open Rays_math
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "v"] [@sop.label "Attribute"];
    x : float [@sop.default 0.] [@sop.label "X"] [@sop.folder "Value"]
      [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "value"]
    y : float [@sop.default 0.] [@sop.label "Y"] [@sop.folder "Value"]
      [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "value"]
    z : float [@sop.default 0.] [@sop.label "Z"] [@sop.folder "Value"]
      [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "value"]
  } [@@sop.node_key "set_vector"] [@@sop.node_label "Set Vector"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.name = "" then invalid_arg "sop/set_vector: empty name";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then
        invalid_arg "sop/set_vector: cannot replace canonical P";
      if not (Float.is_finite parameters.x && Float.is_finite parameters.y && Float.is_finite parameters.z) then
        invalid_arg "sop/set_vector: components must be finite"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let value = Vec3.create parameters.x parameters.y parameters.z in
    let value = vec3_copy value in
    Node.Private.make_geometry ?label ~operation:"set_vector" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        set_attribute_cook ~operation:"set_vector" ~owner ~name
          (fun count -> Ok (Rdk.Attribute.Float3 (Rdk.Packed.Float3.Private.of_owned_exn
            ~x:(Array.make count value.x) ~y:(Array.make count value.y)
            ~z:(Array.make count value.z)))) inputs.(0))
  )
  let factory = parameters_factory build
end

module Connectivity = struct
  type output = Support.connectivity_output = Integer | Text
  let owner_parameter = Parameter.choice ~equal:( = ) [
      "Points", Rdk.Analysis.Connectivity_points;
      "Primitives", Rdk.Analysis.Connectivity_primitives;
    ]
  let output_parameter = Parameter.choice ~equal:( = ) [
      "Integer", Integer; "Text", Text;
    ]
  type parameters = {
    owner : Rdk.Analysis.connectivity_owner
      [@sop.default Rdk.Analysis.Connectivity_primitives]
      [@sop.label "Connectivity type"] [@sop.kind owner_parameter];
    primitive_group : string [@sop.default ""] [@sop.label "Primitive group"]
      [@sop.folder "Selection"];
    point_group : string [@sop.default ""] [@sop.label "Point group"]
      [@sop.folder "Selection"];
    seam_group : string [@sop.default ""] [@sop.label "Seam edge group"]
      [@sop.folder "Seams"];
    uv_attribute : string [@sop.default ""] [@sop.label "UV attribute"]
      [@sop.folder "Seams"];
    name : string [@sop.default "class"] [@sop.label "Attribute"]
      [@sop.folder "Output"];
    output : output [@sop.default Integer] [@sop.label "Storage"]
      [@sop.folder "Output"] [@sop.kind output_parameter];
    text_prefix : string [@sop.default "piece"] [@sop.label "Text prefix"]
      [@sop.folder "Output"];
  } [@@sop.node_key "connectivity"] [@@sop.node_label "Connectivity"]
    [@@sop.validate fun parameters ->
      let set name = String.trim name <> "" in
      if parameters.owner = Rdk.Analysis.Connectivity_primitives && set parameters.point_group then
        invalid_arg "sop/connectivity: point include group requires point mode";
      if parameters.owner = Rdk.Analysis.Connectivity_points && set parameters.uv_attribute then
        invalid_arg "sop/connectivity: UV connectivity requires primitive mode";
      if set parameters.seam_group && set parameters.uv_attribute then
        invalid_arg "sop/connectivity: edge seams and UV connectivity are mutually exclusive";
      if parameters.owner = Rdk.Analysis.Connectivity_points && parameters.name = "P" then
        invalid_arg "sop/connectivity: cannot replace canonical P"]
    [@@sop.node_category "Attribute/Analysis"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let primitive_group = optional_text parameters.primitive_group in
    let point_group = optional_text parameters.point_group in
    let seam_group = optional_text parameters.seam_group in
    let uv_attribute = optional_text parameters.uv_attribute in
    let owner = parameters.owner in
    let name = parameters.name in
    let output = parameters.output in
    let text_prefix = parameters.text_prefix in
    let name = optional_text name in
    let attribute = match output with
      | Integer -> Rdk.Analysis.Connectivity_integer
      | Text -> Rdk.Analysis.Connectivity_text text_prefix in
    Node.Private.make_geometry ?label ~operation:"connectivity" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let ordinary owner label = function
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "connectivity could not find %s %S" label name)))
        in
        let seam = match seam_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_edge_group name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "connectivity could not find edge group %S" name)))
        in
        match ordinary Rdk.Group.Primitive "primitive group" primitive_group,
            ordinary Rdk.Group.Point "point group" point_group, seam with
        | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
        | Ok primitives, Ok points, Ok seams ->
            (match Rdk.Analysis.with_connectivity
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ?points ?seams
                ?uv_attribute ~owner ?name ~attribute geometry with
             | Ok geometry -> cooked geometry
             | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Set_orient = struct
  open Rays_math
  type parameters = {
    x : float [@sop.default 0.] [@sop.label "X"] [@sop.min (-1.)]
      [@sop.max 1.];
    y : float [@sop.default 0.] [@sop.label "Y"] [@sop.min (-1.)]
      [@sop.max 1.];
    z : float [@sop.default 0.] [@sop.label "Z"] [@sop.min (-1.)]
      [@sop.max 1.];
    w : float [@sop.default 1.] [@sop.label "W"] [@sop.min (-1.)]
      [@sop.max 1.];
  } [@@sop.node_key "set_orient"] [@@sop.node_label "Set Orient"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.x && Float.is_finite parameters.y
          && Float.is_finite parameters.z && Float.is_finite parameters.w) then
        invalid_arg "sop/set_orient: components must be finite"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let x = parameters.x in
    let y = parameters.y in
    let z = parameters.z in
    let w = parameters.w in
    let value = Quat.create ~x ~y ~z ~w in
    let value = Quat.normalize value in
    Node.Private.make_geometry ?label ~operation:"set_orient" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        set_attribute_cook ~operation:"set_orient" ~owner:Rdk.Attribute.Point ~name:"orient"
        (fun count ->
          Result.map (fun values -> Rdk.Attribute.Float4 values)
            (Rdk.Packed.Float4.of_owned ~x:(Array.make count value.x)
               ~y:(Array.make count value.y) ~z:(Array.make count value.z)
               ~w:(Array.make count value.w))) inputs.(0))
  )
  let factory = parameters_factory build
end

module Set_transform = struct
  open Rays_math
  type parameters = {
    m00 : float [@sop.default 1.] [@sop.label "M00"]
      [@sop.folder "Matrix/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m01 : float [@sop.default 0.] [@sop.label "M01"]
      [@sop.folder "Matrix/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m02 : float [@sop.default 0.] [@sop.label "M02"]
      [@sop.folder "Matrix/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m03 : float [@sop.default 0.] [@sop.label "M03"]
      [@sop.folder "Matrix/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m10 : float [@sop.default 0.] [@sop.label "M10"]
      [@sop.folder "Matrix/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m11 : float [@sop.default 1.] [@sop.label "M11"]
      [@sop.folder "Matrix/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m12 : float [@sop.default 0.] [@sop.label "M12"]
      [@sop.folder "Matrix/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m13 : float [@sop.default 0.] [@sop.label "M13"]
      [@sop.folder "Matrix/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m20 : float [@sop.default 0.] [@sop.label "M20"]
      [@sop.folder "Matrix/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m21 : float [@sop.default 0.] [@sop.label "M21"]
      [@sop.folder "Matrix/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m22 : float [@sop.default 1.] [@sop.label "M22"]
      [@sop.folder "Matrix/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m23 : float [@sop.default 0.] [@sop.label "M23"]
      [@sop.folder "Matrix/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m30 : float [@sop.default 0.] [@sop.label "M30"]
      [@sop.folder "Matrix/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m31 : float [@sop.default 0.] [@sop.label "M31"]
      [@sop.folder "Matrix/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m32 : float [@sop.default 0.] [@sop.label "M32"]
      [@sop.folder "Matrix/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m33 : float [@sop.default 1.] [@sop.label "M33"]
      [@sop.folder "Matrix/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
  } [@@sop.node_key "set_transform"] [@@sop.node_label "Set Transform"]
    [@@sop.validate fun parameters ->
      if not (List.for_all Float.is_finite [parameters.m00; parameters.m01; parameters.m02; parameters.m03;
          parameters.m10; parameters.m11; parameters.m12; parameters.m13;
          parameters.m20; parameters.m21; parameters.m22; parameters.m23;
          parameters.m30; parameters.m31; parameters.m32; parameters.m33]) then
        invalid_arg "sop/set_transform: matrix must be finite";
      if abs_float parameters.m30 > 1e-12 || abs_float parameters.m31 > 1e-12
          || abs_float parameters.m32 > 1e-12 || abs_float (parameters.m33 -. 1.) > 1e-12 then
        invalid_arg "sop/set_transform: matrix must be affine"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let m00 = parameters.m00 in
    let m01 = parameters.m01 in
    let m02 = parameters.m02 in
    let m03 = parameters.m03 in
    let m10 = parameters.m10 in
    let m11 = parameters.m11 in
    let m12 = parameters.m12 in
    let m13 = parameters.m13 in
    let m20 = parameters.m20 in
    let m21 = parameters.m21 in
    let m22 = parameters.m22 in
    let m23 = parameters.m23 in
    let m30 = parameters.m30 in
    let m31 = parameters.m31 in
    let m32 = parameters.m32 in
    let m33 = parameters.m33 in
    let value = Mat4.of_rows (m00, m01, m02, m03) (m10, m11, m12, m13)
        (m20, m21, m22, m23) (m30, m31, m32, m33) in
    Node.Private.make_geometry ?label ~operation:"set_transform" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        set_attribute_cook ~operation:"set_transform" ~owner:Rdk.Attribute.Point ~name:"transform"
        (fun count ->
        if count > Sys.max_array_length / 16 then
          Error "set_transform output exceeds OCaml array limits"
        else begin
          let row = Array.init 16 (fun index ->
            Mat4.get value ~row:(index / 4) ~column:(index mod 4)) in
          let values = Array.make (count * 16) 0. in
          for index = 0 to count - 1 do Array.blit row 0 values (index * 16) 16 done;
          let offsets = Array.init (count + 1) (fun index -> index * 16) in
          Result.map (fun values -> Rdk.Attribute.Float_array values)
            (Rdk.Packed.Float_array.create_owned ~offsets ~values)
        end) inputs.(0))
  )
  let factory = parameters_factory build
end

module Set_color = struct
  open Rays_math
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Group"];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    color_r : float [@sop.default 1.] [@sop.label "Red"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.]
      [@sop.vec3 "color"];
    color_g : float [@sop.default 1.] [@sop.label "Green"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.]
      [@sop.vec3 "color"];
    color_b : float [@sop.default 1.] [@sop.label "Blue"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.]
      [@sop.vec3 "color"];
    alpha : float [@sop.default 1.] [@sop.label "Alpha"] [@sop.min 0.]
      [@sop.max 1.] [@sop.hard_min 0.] [@sop.hard_max 1.];
  } [@@sop.node_key "set_color"] [@@sop.node_label "Set Color"]
    [@@sop.validate fun parameters ->
      if not (List.for_all (fun value -> Float.is_finite value && value >= 0. && value <= 1.)
          [parameters.color_r; parameters.color_g; parameters.color_b; parameters.alpha]) then
        invalid_arg "sop/set_color: channels must be finite and in 0..1";
      if parameters.owner = Rdk.Attribute.Detail && String.trim parameters.group <> "" then
        invalid_arg "sop/set_color: a detail color cannot be restricted to a group"]
    [@@sop.node_category "Attribute/Set"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let owner = parameters.owner in
    let color = Vec3.create parameters.color_r parameters.color_g parameters.color_b in
    let alpha = parameters.alpha in
    let r = color.Vec3.x and g = color.y and b = color.z and a = alpha in
    Node.Private.make_geometry ?label ~operation:"set_color" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs ->
        let geometry = inputs.(0) in
        let count = attribute_count geometry owner in
        let group_owner = match owner with
          | Rdk.Attribute.Point -> Some Rdk.Group.Point
          | Vertex -> Some Rdk.Group.Vertex
          | Primitive -> Some Rdk.Group.Primitive
          | Detail -> None in
        let selected = match group, group_owner with
          | None, _ -> Ok None
          | Some _, None -> Error "a detail color cannot be restricted to a group"
          | Some name, Some group_owner ->
              (match Rdk.Geometry.find_group ~owner:group_owner name geometry with
               | Some found -> Ok (Some found)
               | None -> Error (Printf.sprintf "could not find group %S" name)) in
        let existing = match selected with
          | Ok (Some _) ->
              Rdk.Geometry.find_attribute ~owner "Cd" geometry
              |> Fun.flip Option.bind (Rdk.Attribute.get (Rdk.Attribute.color ~owner))
          | _ -> None in
        match selected with
        | Error message -> rdk_error "set_color" message
        | Ok selected ->
            let channel index fill = match existing with
              | Some packed -> Array.copy (index (Rdk.Packed.Float4.Private.view packed))
              | None -> Array.make count fill in
            let red = channel (fun v -> v.Rdk.Packed.Float4.Private.x) 1.
            and green = channel (fun v -> v.y) 1.
            and blue = channel (fun v -> v.z) 1.
            and alpha = channel (fun v -> v.w) 1. in
            (match selected with
             | None ->
                 Array.fill red 0 count r; Array.fill green 0 count g;
                 Array.fill blue 0 count b; Array.fill alpha 0 count a
             | Some found ->
                 Rdk.Group.iter (fun index ->
                   red.(index) <- r; green.(index) <- g;
                   blue.(index) <- b; alpha.(index) <- a) found);
            match Rdk.Packed.Float4.of_owned ~x:red ~y:green ~z:blue ~w:alpha with
            | Error message -> rdk_error "set_color" message
            | Ok color ->
                match Rdk.Attribute.create_owned ~name:"Cd" ~owner
                    (Rdk.Attribute.Float4 color) with
                | Error message -> rdk_error "set_color" message
                | Ok attribute ->
                    match Rdk.Geometry.with_attribute attribute geometry with
                    | Ok geometry -> cooked geometry
                    | Error message -> rdk_error "set_color" message)
  )
  let factory = parameters_factory build
end

module Enumerate = struct
  type storage = Support.enumerate_storage = Enumerate_integer | Enumerate_text
  let storage_parameter = Parameter.choice ~equal:( = ) [
      "Integer", Enumerate_integer; "Text", Enumerate_text;
    ]
  let mode_parameter = Parameter.choice ~equal:( = ) [
      "Elements within pieces", Rdk.Attribute_ops.Enumerate_piece_elements;
      "Pieces", Rdk.Attribute_ops.Enumerate_pieces;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind element_attribute_owner_parameter];
    name : string [@sop.default "id"] [@sop.label "Attribute"];
    group : string [@sop.default ""] [@sop.label "Group"];
    start : int [@sop.default 0] [@sop.label "Start"]
      [@sop.folder "Sequence"] [@sop.min (-100)] [@sop.max 100];
    step : int [@sop.default 1] [@sop.label "Step"]
      [@sop.folder "Sequence"] [@sop.min (-20)] [@sop.max 20];
    storage : storage [@sop.default Enumerate_integer] [@sop.label "Storage"]
      [@sop.folder "Output"] [@sop.kind storage_parameter];
    prefix : string [@sop.default "piece"] [@sop.label "Text prefix"]
      [@sop.folder "Output"];
    piece_attribute : string [@sop.default ""] [@sop.label "Piece attribute"]
      [@sop.folder "Pieces"];
    mode : Rdk.Attribute_ops.enumeration_mode
      [@sop.default Rdk.Attribute_ops.Enumerate_piece_elements]
      [@sop.label "Piece mode"] [@sop.folder "Pieces"]
      [@sop.kind mode_parameter];
  } [@@sop.node_key "enumerate"] [@@sop.node_label "Enumerate"]
    [@@sop.validate fun parameters ->
      if parameters.owner = Rdk.Attribute.Detail then
        invalid_arg "sop/enumerate: detail ownership is not enumerable";
      if String.trim parameters.name = "" then invalid_arg "sop/enumerate: empty attribute name";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then
        invalid_arg "sop/enumerate: cannot replace canonical P"]
    [@@sop.node_category "Attribute/Generate"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let start = parameters.start in
    let step = parameters.step in
    let storage = parameters.storage in
    let prefix = parameters.prefix in
    let piece_attribute = optional_text parameters.piece_attribute in
    let mode = parameters.mode in
    let owner = parameters.owner in
    let name = parameters.name in
    let storage = match storage with
      | Enumerate_integer -> Rdk.Attribute_ops.Integer
      | Enumerate_text -> Rdk.Attribute_ops.Text { prefix } in
    Node.Private.make_geometry ?label ~operation:"enumerate" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let group_owner = match owner with
          | Rdk.Attribute.Point -> Rdk.Group.Point
          | Rdk.Attribute.Vertex -> Rdk.Group.Vertex
          | Rdk.Attribute.Primitive -> Rdk.Group.Primitive
          | Rdk.Attribute.Detail -> assert false in
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:group_owner name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "enumerate could not find %s group %S"
                      (attribute_owner_key owner) name))) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Attribute_ops.enumerate
                ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
                ?selection ~start ~step ~storage ?piece_attribute ~mode
                ~owner ~name inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_blur = struct
  type mode = Support.smoothing_mode = Laplacian | Custom
  let method_parameter = Parameter.choice ~equal:( = ) [
      "Uniform", Rdk.Attribute_ops.Uniform;
      "Edge length", Rdk.Attribute_ops.Edge_length;
    ]
  let mode_parameter = Parameter.choice ~equal:( = ) [
      "Laplacian", Laplacian; "Custom steps", Custom;
    ]
  type parameters = {
    attributes : string [@sop.default "P"] [@sop.label "Attributes"];
    group : string [@sop.default ""] [@sop.label "Point group"];
    iterations : int [@sop.default 1] [@sop.label "Iterations"]
      [@sop.min 1] [@sop.max 100] [@sop.hard_min 0];
    method_ : Rdk.Attribute_ops.blur_method
      [@sop.default Rdk.Attribute_ops.Uniform]
      [@sop.label "Method"] [@sop.kind method_parameter];
    mode : mode [@sop.default Laplacian] [@sop.label "Mode"]
      [@sop.folder "Step"] [@sop.kind mode_parameter];
    laplacian_step : float [@sop.default 0.5] [@sop.label "Step"]
      [@sop.folder "Step"] [@sop.min (-1.)] [@sop.max 1.];
    odd_step : float [@sop.default 0.5] [@sop.label "Odd step"]
      [@sop.folder "Step/Custom"] [@sop.min (-1.)] [@sop.max 1.];
    even_step : float [@sop.default (-0.51)] [@sop.label "Even step"]
      [@sop.folder "Step/Custom"] [@sop.min (-1.)] [@sop.max 1.];
    weight_attribute : string [@sop.default ""] [@sop.label "Weight attribute"]
      [@sop.folder "Mask"];
    alpha_attribute : string [@sop.default ""] [@sop.label "Alpha attribute"]
      [@sop.folder "Mask"];
    pin_borders : bool [@sop.default false] [@sop.label "Pin borders"];
    original_blend : float [@sop.default 0.] [@sop.label "Original blend"]
      [@sop.folder "Blend"] [@sop.min 0.] [@sop.max 1.];
    blurred_blend : float [@sop.default 1.] [@sop.label "Blurred blend"]
      [@sop.folder "Blend"] [@sop.min 0.] [@sop.max 1.];
  } [@@sop.node_key "attribute_blur"] [@@sop.node_label "Attribute Blur"]
    [@@sop.validate fun parameters ->
      (match Rdk.Attribute_pattern.compile parameters.attributes with
       | Ok _ -> () | Error message -> invalid_arg ("sop/attribute_blur: " ^ message));
      if parameters.iterations < 0 then invalid_arg "sop/attribute_blur: iterations must be non-negative";
      if not (List.for_all Float.is_finite [parameters.laplacian_step; parameters.odd_step;
          parameters.even_step; parameters.original_blend; parameters.blurred_blend]) then
        invalid_arg "sop/attribute_blur: steps and blend amounts must be finite"]
    [@@sop.node_category "Attribute/Filter"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let iterations = parameters.iterations in
    let method_ = parameters.method_ in
    let mode = parameters.mode in
    let laplacian_step = parameters.laplacian_step in
    let odd_step = parameters.odd_step in
    let even_step = parameters.even_step in
    let weight_attribute = optional_text parameters.weight_attribute in
    let alpha_attribute = optional_text parameters.alpha_attribute in
    let pin_borders = parameters.pin_borders in
    let original_blend = parameters.original_blend in
    let blurred_blend = parameters.blurred_blend in
    let attributes = parameters.attributes in
    let mode = match mode with
      | Laplacian -> Rdk.Attribute_ops.Laplacian laplacian_step
      | Custom -> Rdk.Attribute_ops.Custom_steps { odd = odd_step; even = even_step } in
    Node.Private.make_geometry ?label ~operation:"attribute_blur" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "attribute_blur could not find point group %S" name))) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            (match Rdk.Attribute_ops.blur_points
                ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
                ?selection ~iterations ~method_ ~mode ?weight_attribute
                ?alpha_attribute ~pin_borders ~original_blend ~blurred_blend
                ~pattern:attributes inputs.(0) with
             | Ok geometry -> cooked geometry
             | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Promote_attributes = struct
  type parameters = {
    source : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Source owner"] [@sop.kind attribute_owner_parameter];
    destination : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Destination owner"] [@sop.kind attribute_owner_parameter];
    pattern : string [@sop.default "*"] [@sop.label "Attribute pattern"];
    method_ : Rdk.Attribute_ops.method_ [@sop.default Rdk.Attribute_ops.Average]
      [@sop.label "Promotion method"]
      [@sop.kind attribute_promotion_method_parameter];
    delete_source : bool [@sop.default false] [@sop.label "Delete source"];
    piece_attribute : string [@sop.default ""]
      [@sop.label "Piece attribute"] [@sop.folder "Partition"];
    into_pattern : string [@sop.default ""]
      [@sop.label "Rename pattern"] [@sop.folder "Output"];
    index_pattern : string [@sop.default ""]
      [@sop.label "Index pattern"] [@sop.folder "Output"];
  } [@@sop.node_key "promote_attributes"]
    [@@sop.node_label "Promote Attributes"]
    [@@sop.validate fun parameters ->
      let index = optional_text parameters.index_pattern in
      let has_index = match parameters.method_ with
        | Rdk.Attribute_ops.First | Last | Minimum | Maximum | Mode -> true
        | Average | Median | Sum | Sum_squares | Root_mean_square | Array_all | Unique_values -> false in
      if index <> None && not has_index then invalid_arg
        "sop/promote_attributes: source index requires first, last, minimum, maximum, or mode";
      (match Rdk.Attribute_pattern.compile parameters.pattern with
       | Ok _ -> () | Error message -> invalid_arg ("sop/promote_attributes: " ^ message));
      List.iter (Option.iter (fun replacement ->
        match Rdk.Attribute_pattern.compile_rewrite_set ~pattern:parameters.pattern ~replacement with
        | Ok _ -> () | Error message -> invalid_arg ("sop/promote_attributes: " ^ message)))
        [optional_text parameters.into_pattern; index]]
    [@@sop.node_category "Attribute/Promote"] [@@sop.node_inputs 1]
    [@@sop.node_operation "attribute_promote_pattern"]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let method_ = parameters.method_ in
    let delete_source = parameters.delete_source in
    let piece_attribute = optional_text parameters.piece_attribute in
    let into_pattern = optional_text parameters.into_pattern in
    let index_pattern = optional_text parameters.index_pattern in
    let source = parameters.source in
    let destination = parameters.destination in
    let pattern = parameters.pattern in
    Node.Private.make_geometry ?label ~operation:"attribute_promote_pattern" ~version:5
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Attribute_ops.promote_pattern
            ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~method_ ~delete_source ~source
            ~destination ~pattern ?piece_attribute ?into_pattern ?index_pattern
            inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Polyframe = struct
  type style = Support.polyframe_style = Style_first_edge | Style_two_edges | Style_centroid
    | Style_texture_uv | Style_texture_uv_gradient | Style_attribute_gradient
  let style_parameter = Parameter.choice ~equal:( = ) [
      "First edge", Style_first_edge; "Two edges", Style_two_edges;
      "Primitive centroid", Style_centroid; "Texture UV", Style_texture_uv;
      "Texture UV gradient", Style_texture_uv_gradient;
      "Attribute gradient", Style_attribute_gradient;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_primitive]
      [@sop.label "Group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    style : style [@sop.default Style_first_edge]
      [@sop.label "Style"] [@sop.kind style_parameter];
    style_attribute : string [@sop.default "uv"]
      [@sop.label "Style attribute"];
    orthogonal : bool [@sop.default false] [@sop.label "Make orthogonal"];
    left_handed : bool [@sop.default false] [@sop.label "Left handed"];
    normal_attribute : string [@sop.default "N"] [@sop.label "Normal"]
      [@sop.folder "Output attributes"];
    tangent_attribute : string [@sop.default "tangentu"] [@sop.label "Tangent"]
      [@sop.folder "Output attributes"];
    bitangent_attribute : string [@sop.default "tangentv"]
      [@sop.label "Bitangent"] [@sop.folder "Output attributes"];
  } [@@sop.node_key "polyframe"] [@@sop.node_label "PolyFrame"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.normal_attribute = "" then invalid_arg "sop/polyframe: empty normal name";
      let names = parameters.normal_attribute :: List.filter_map optional_text
          [parameters.tangent_attribute; parameters.bitangent_attribute] in
      if List.mem "P" names then invalid_arg "sop/polyframe: outputs cannot replace canonical P";
      if List.length (List.sort_uniq String.compare names) <> List.length names then
        invalid_arg "sop/polyframe: output attribute names must be distinct";
      if parameters.style = Style_attribute_gradient && String.trim parameters.style_attribute = "" then
        invalid_arg "sop/polyframe: empty gradient attribute name"]
    [@@sop.node_category "Attribute"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = optional_text parameters.group in
    let orthogonal = parameters.orthogonal in
    let left_handed = parameters.left_handed in
    let normal_attribute = parameters.normal_attribute in
    let tangent_attribute = parameters.tangent_attribute in
    let bitangent_attribute = parameters.bitangent_attribute in
    let style = parameters.style in
    let style_attribute = parameters.style_attribute in
    let selection = optional_element_group group_owner (Option.value ~default:"" group) in
    let tangent_attribute = optional_text tangent_attribute and bitangent_attribute = optional_text bitangent_attribute in
    let style = match style with
      | Style_first_edge -> Rdk.Polyframe.First_edge
      | Style_two_edges -> Rdk.Polyframe.Two_edges
      | Style_centroid -> Rdk.Polyframe.Primitive_centroid
      | Style_texture_uv -> Rdk.Polyframe.Texture_uv style_attribute
      | Style_texture_uv_gradient -> Rdk.Polyframe.Texture_uv_gradient style_attribute
      | Style_attribute_gradient -> Rdk.Polyframe.Attribute_gradient style_attribute in
    Node.Private.make_geometry ?label ~operation:"polyframe" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"polyframe" selection inputs.(0) with
        | Error _ as error -> error
        | Ok selection ->
            match Rdk.Polyframe.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~orthogonal
                ~left_handed ~normal_attribute ~tangent_attribute
                ~bitangent_attribute style inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Distance_along_geometry = struct
  type parameters = {
    start_owner : element_owner [@sop.default Element_point]
      [@sop.label "Start group type"] [@sop.kind element_owner_parameter];
    start_group : string [@sop.default "start"] [@sop.label "Start group"];
    affected_owner : element_owner [@sop.default Element_point]
      [@sop.label "Affected group type"] [@sop.folder "Affected"]
      [@sop.kind element_owner_parameter];
    affected_group : string [@sop.default ""] [@sop.label "Affected group"]
      [@sop.folder "Affected"];
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
  } [@@sop.node_key "distance_along_geometry"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.radius) || parameters.radius < 0.
          || (parameters.radius_mode = Radius_fixed && parameters.radius = 0.) then
        invalid_arg "sop/distance_along_geometry: fixed radius must be positive and finite";
      let names = List.filter_map optional_text [parameters.distance_attribute; parameters.mask_attribute] in
      if names = [] || List.mem "P" names || List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid_arg "sop/distance_along_geometry: output names must be present, distinct and cannot be P"]
    [@@sop.node_label "Distance Along Geometry"]
    [@@sop.node_category "Attribute/Distance"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let start_owner = parameters.start_owner in
    let start_group = parameters.start_group in
    let affected_owner = parameters.affected_owner in
    let affected_group = parameters.affected_group in
    let falloff = parameters.falloff in
    let radius_mode = parameters.radius_mode in
    let radius = parameters.radius in
    let distance_attribute = parameters.distance_attribute in
    let mask_attribute = optional_text parameters.mask_attribute in
    let start = Option.value ~default:(Point_group "start") (optional_element_group start_owner start_group)
    and affected = optional_element_group affected_owner affected_group
    and radius = match radius_mode with Radius_fixed -> Rdk.Transform_ops.Distance_fixed radius
      | Radius_maximum -> Rdk.Transform_ops.Distance_maximum in
    let distance_attribute = optional_text distance_attribute in
    Node.Private.make_geometry ?label ~operation:"distance_along_geometry" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_element_group ~operation:"distance_along_geometry start"
            (Some start) geometry with
        | Error error -> Error error
        | Ok None -> assert false
        | Ok (Some start) ->
            (match resolve_element_group
                ~operation:"distance_along_geometry affected" affected geometry with
             | Error error -> Error error
             | Ok affected ->
                 match Rdk.Transform_ops.distance_along_geometry
                     ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ?affected ~falloff ~radius
                     ~distance_attribute ?mask_attribute ~start geometry with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Distance_from_target = struct
  open Rays_math
  let projection_parameter = Parameter.choice ~equal:( = ) [
      "Spherical", Rdk.Transform_ops.Distance_target_spherical;
      "Cylindrical", Rdk.Transform_ops.Distance_target_cylindrical;
      "Planar", Rdk.Transform_ops.Distance_target_planar;
    ]
  let metric_parameter = Parameter.choice ~equal:( = ) [
      "Absolute", Rdk.Transform_ops.Distance_target_absolute;
      "Signed", Rdk.Transform_ops.Distance_target_signed;
    ]

  type parameters = {
    affected_owner : element_owner [@sop.default Element_point]
      [@sop.label "Affected group type"] [@sop.kind element_owner_parameter];
    affected_group : string [@sop.default ""] [@sop.label "Affected group"];
    projection : Rdk.Transform_ops.distance_from_target_projection
      [@sop.default Rdk.Transform_ops.Distance_target_spherical]
      [@sop.label "Projection"] [@sop.kind projection_parameter];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Target/Origin"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Target/Origin"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Target/Origin"] [@sop.min (-100.)] [@sop.max 100.]; [@sop.vec3 "origin"]
    direction_x : float [@sop.default 0.] [@sop.label "Direction X"]
      [@sop.folder "Target/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_y : float [@sop.default 1.] [@sop.label "Direction Y"]
      [@sop.folder "Target/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_z : float [@sop.default 0.] [@sop.label "Direction Z"]
      [@sop.folder "Target/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    metric : Rdk.Transform_ops.distance_from_target_metric
      [@sop.default Rdk.Transform_ops.Distance_target_absolute]
      [@sop.label "Metric"] [@sop.kind metric_parameter];
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
  } [@@sop.node_key "distance_from_target"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.radius) || parameters.radius < 0.
          || (parameters.radius_mode = Radius_fixed && parameters.radius = 0.) then
        invalid_arg "sop/distance_from_target: fixed radius must be positive and finite";
      let names = List.filter_map optional_text [parameters.distance_attribute; parameters.mask_attribute] in
      if names = [] || List.mem "P" names || List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid_arg "sop/distance_from_target: output names must be present, distinct and cannot be P";
      if not (List.for_all Float.is_finite [parameters.origin_x; parameters.origin_y; parameters.origin_z;
          parameters.direction_x; parameters.direction_y; parameters.direction_z]) then
        invalid_arg "sop/distance_from_target: origin and direction must be finite";
      if parameters.metric = Rdk.Transform_ops.Distance_target_signed
          && parameters.projection <> Rdk.Transform_ops.Distance_target_planar then
        invalid_arg "sop/distance_from_target: signed distance requires planar projection";
      let squared = parameters.direction_x *. parameters.direction_x
          +. parameters.direction_y *. parameters.direction_y +. parameters.direction_z *. parameters.direction_z in
      if parameters.projection <> Rdk.Transform_ops.Distance_target_spherical
          && (not (Float.is_finite squared) || squared <= 0.) then
        invalid_arg "sop/distance_from_target: cylindrical and planar direction must be nonzero"]
    [@@sop.node_label "Distance from Target"]
    [@@sop.node_category "Attribute/Distance"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let affected_owner = parameters.affected_owner in
    let affected_group = parameters.affected_group in
    let projection = parameters.projection in
    let origin = Vec3.create parameters.origin_x parameters.origin_y
          parameters.origin_z in
    let direction = Vec3.create parameters.direction_x parameters.direction_y
          parameters.direction_z in
    let metric = parameters.metric in
    let falloff = parameters.falloff in
    let radius_mode = parameters.radius_mode in
    let radius = parameters.radius in
    let distance_attribute = parameters.distance_attribute in
    let mask_attribute = optional_text parameters.mask_attribute in
    let affected = optional_element_group affected_owner affected_group
    and radius = match radius_mode with Radius_fixed -> Rdk.Transform_ops.Distance_fixed radius
      | Radius_maximum -> Rdk.Transform_ops.Distance_maximum in
    let distance_attribute = optional_text distance_attribute in
    let origin = vec3_copy origin and direction = vec3_copy direction in
    Node.Private.make_geometry ?label ~operation:"distance_from_target" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"distance_from_target affected"
            affected inputs.(0) with
        | Error error -> Error error
        | Ok affected ->
            match Rdk.Transform_ops.distance_from_target
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?affected ~projection ~origin
                ~direction ~metric ~falloff ~radius ~distance_attribute
                ?mask_attribute inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Distance_from_geometry = struct
  let reference_parameter = Parameter.choice ~equal:( = ) [
      "Points", Rdk.Transform_ops.Distance_reference_points;
      "Primitives", Rdk.Transform_ops.Distance_reference_primitives;
    ]

  type parameters = {
    affected_owner : element_owner [@sop.default Element_point]
      [@sop.label "Affected group type"] [@sop.folder "Source"]
      [@sop.kind element_owner_parameter];
    affected_group : string [@sop.default ""] [@sop.label "Affected group"]
      [@sop.folder "Source"];
    reference_owner : element_owner [@sop.default Element_primitive]
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
      if not (Float.is_finite parameters.radius) || parameters.radius < 0.
          || (parameters.radius_mode = Radius_fixed && parameters.radius = 0.) then
        invalid_arg "sop/distance_from_geometry: fixed radius must be positive and finite";
      let names = List.filter_map optional_text [parameters.distance_attribute; parameters.mask_attribute] in
      if names = [] || List.mem "P" names || List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid_arg "sop/distance_from_geometry: output names must be present, distinct and cannot be P"]
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
    Node.Private.make_geometry ?label ~operation:"distance_from_geometry" ~version:1
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
                     ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ?affected ?reference_selection
                     ~reference_kind ~falloff ~radius ~distance_attribute
                     ?mask_attribute ~reference:inputs.(1) inputs.(0) with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Edge_transport = struct
  type parameters = {
    attribute : string [@sop.default "value"] [@sop.label "Attribute"];
    point_group : string [@sop.default ""] [@sop.label "Point group"];
    roots : Support.transport_roots [@sop.default Transport_first]
      [@sop.label "Roots"] [@sop.kind transport_roots_parameter];
    root_group : string [@sop.default ""] [@sop.label "Root group"];
    direction : Rdk.Edge_transport.direction
      [@sop.default Rdk.Edge_transport.Transport_forward] [@sop.label "Direction"]
      [@sop.kind edge_transport_direction_parameter];
    operation : Rdk.Edge_transport.operation
      [@sop.default Rdk.Edge_transport.Transport] [@sop.label "Operation"]
      [@sop.kind edge_transport_operation_parameter];
    root_value : Rdk.Edge_transport.root_value
      [@sop.default Rdk.Edge_transport.Transport_root_zero] [@sop.label "Root value"]
      [@sop.kind edge_transport_root_value_parameter];
    integrate_constant : bool [@sop.default false]
      [@sop.label "Integrate constant"];
    scale_by_edge_length : bool [@sop.default false]
      [@sop.label "Scale by edge length"];
    split : Rdk.Edge_transport.split [@sop.default Rdk.Edge_transport.Transport_copy]
      [@sop.label "Branch split"] [@sop.kind edge_transport_split_parameter];
    merge : Rdk.Edge_transport.merge
      [@sop.default Rdk.Edge_transport.Transport_merge_add]
      [@sop.label "Branch merge"] [@sop.kind edge_transport_merge_parameter];
    normalization : Rdk.Edge_transport.normalization
      [@sop.default Rdk.Edge_transport.Transport_no_normalization]
      [@sop.label "Normalization"]
      [@sop.kind edge_transport_normalization_parameter];
  } [@@sop.node_key "edge_transport"] [@@sop.node_label "Edge Transport"]
    [@@sop.validate fun parameters ->
      validate_transport_options "edge_transport" parameters.operation parameters.integrate_constant parameters.scale_by_edge_length;
      if String.trim parameters.attribute = "" || parameters.attribute = "P" then
        invalid_arg "sop/edge_transport: attribute must be nonblank and cannot be P"]
    [@@sop.node_category "Attribute/Transport"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let point_group = optional_text parameters.point_group in
    let root_group = optional_text parameters.root_group in
    let roots = parameters.roots in
    let direction = parameters.direction in
    let operation = parameters.operation in
    let root_value = parameters.root_value in
    let integrate_constant = parameters.integrate_constant in
    let scale_by_edge_length = parameters.scale_by_edge_length in
    let split = parameters.split in
    let merge = parameters.merge in
    let normalization = parameters.normalization in
    let attribute = parameters.attribute in
    let root_group, roots = transport_roots roots (Option.value ~default:"" root_group) in
    Node.Private.make_geometry ?label ~operation:"edge_transport" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let points = resolve_optional_point_group "edge_transport" point_group
            geometry in
        let roots = match root_group with
          | None -> Ok roots
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some group -> Ok (Rdk.Edge_transport.Transport_root_group group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "edge_transport could not find root point group %S"
                      name))) in
        match points, roots with
        | Error error, _ | _, Error error -> Error error
        | Ok points, Ok roots ->
            match Rdk.Edge_transport.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~roots ~operation ~root_value
                ~integrate_constant ~scale_by_edge_length ~split ~direction ~merge
                ~normalization ~attribute geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_remap = struct
  open Rays_math
  let input_parameter = Parameter.choice ~equal:( = ) [
      "Automatic", Remap_automatic; "Explicit", Remap_explicit;
    ]
  let policy_parameter = Parameter.choice ~equal:( = ) [
      "Clamp", Rdk.Attribute_ops.Remap_clamp;
      "Cycle", Rdk.Attribute_ops.Remap_cycle;
      "Extrapolate", Rdk.Attribute_ops.Remap_extrapolate;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "value"] [@sop.label "Source attribute"];
    into : string [@sop.default ""] [@sop.label "Destination attribute"];
    group : string [@sop.default ""] [@sop.label "Group"];
    kind : Support.numeric_kind [@sop.default Numeric_scalar] [@sop.label "Value type"]
      [@sop.kind numeric_kind_parameter];
    input_range : Support.remap_range [@sop.default Remap_automatic]
      [@sop.label "Input range"] [@sop.kind input_parameter];
    input_min_x : float [@sop.default 0.] [@sop.label "Minimum X"]
      [@sop.folder "Input/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_min"]
    input_min_y : float [@sop.default 0.] [@sop.label "Minimum Y"]
      [@sop.folder "Input/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_min"]
    input_min_z : float [@sop.default 0.] [@sop.label "Minimum Z"]
      [@sop.folder "Input/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_min"]
    input_min_w : float [@sop.default 0.] [@sop.label "Minimum W"]
      [@sop.folder "Input/Minimum"] [@sop.min (-10.)] [@sop.max 10.];
    input_max_x : float [@sop.default 1.] [@sop.label "Maximum X"]
      [@sop.folder "Input/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_max"]
    input_max_y : float [@sop.default 1.] [@sop.label "Maximum Y"]
      [@sop.folder "Input/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_max"]
    input_max_z : float [@sop.default 1.] [@sop.label "Maximum Z"]
      [@sop.folder "Input/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "input_max"]
    input_max_w : float [@sop.default 1.] [@sop.label "Maximum W"]
      [@sop.folder "Input/Maximum"] [@sop.min (-10.)] [@sop.max 10.];
    output_min_x : float [@sop.default 0.] [@sop.label "Minimum X"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_min"]
    output_min_y : float [@sop.default 0.] [@sop.label "Minimum Y"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_min"]
    output_min_z : float [@sop.default 0.] [@sop.label "Minimum Z"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_min"]
    output_min_w : float [@sop.default 0.] [@sop.label "Minimum W"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.];
    output_max_x : float [@sop.default 1.] [@sop.label "Maximum X"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_max"]
    output_max_y : float [@sop.default 1.] [@sop.label "Maximum Y"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_max"]
    output_max_z : float [@sop.default 1.] [@sop.label "Maximum Z"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "output_max"]
    output_max_w : float [@sop.default 1.] [@sop.label "Maximum W"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.];
    policy : Rdk.Attribute_ops.remap_policy
      [@sop.default Rdk.Attribute_ops.Remap_clamp] [@sop.label "Outside range"]
      [@sop.kind policy_parameter];
    ramp : string [@sop.default ""] [@sop.label "Ramp"]
      [@sop.description "Comma-separated position:value knots, from 0 to 1 in strictly increasing order; blank is linear."];
  } [@@sop.node_key "attribute_remap"] [@@sop.node_label "Attribute Remap"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.name = "" then
        invalid_arg "sop/attribute_remap: empty source attribute name";
      if parameters.owner = Rdk.Attribute.Detail && String.trim parameters.group <> "" then
        invalid_arg "sop/attribute_remap: detail attributes do not accept groups";
      let destination = Option.value ~default:parameters.name (optional_text parameters.into) in
      if parameters.owner = Rdk.Attribute.Point && destination = "P"
          && (parameters.name <> "P" || parameters.kind <> Numeric_vec3) then
        invalid_arg "sop/attribute_remap: only canonical three-channel P can write P";
      let inputs = [parameters.input_min_x, parameters.input_max_x;
        parameters.input_min_y, parameters.input_max_y;
        parameters.input_min_z, parameters.input_max_z;
        parameters.input_min_w, parameters.input_max_w] in
      let outputs = [parameters.output_min_x, parameters.output_max_x;
        parameters.output_min_y, parameters.output_max_y;
        parameters.output_min_z, parameters.output_max_z;
        parameters.output_min_w, parameters.output_max_w] in
      if not (List.for_all (fun (a, b) -> Float.is_finite a && Float.is_finite b) (inputs @ outputs)) then
        invalid_arg "sop/attribute_remap: ranges must be finite";
      let dimensions = match parameters.kind with
        | Numeric_scalar -> 1 | Numeric_vec2 -> 2 | Numeric_vec3 -> 3 | Numeric_vec4 -> 4 in
      if parameters.input_range = Remap_explicit then
        List.iteri (fun index (minimum, maximum) ->
          if index < dimensions && maximum <= minimum then
            invalid_arg "sop/attribute_remap: input maximum must exceed minimum") inputs;
      ignore (decode_parameter_ramp "sop/attribute_remap" parameters.ramp)]
    [@@sop.node_category "Attribute/Modify"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input_node ->
    let label = Some label in
    let group = parameters.group in
    let into = parameters.into in
    let policy = parameters.policy in
    let ramp = parameters.ramp in
    let owner = parameters.owner in
    let name = parameters.name in
    let kind = parameters.kind in
    let input_range = parameters.input_range in
    let input_min = Vec3.create parameters.input_min_x parameters.input_min_y parameters.input_min_z in
    let input_min_w = parameters.input_min_w in
    let input_max = Vec3.create parameters.input_max_x parameters.input_max_y parameters.input_max_z in
    let input_max_w = parameters.input_max_w in
    let output_min = Vec3.create parameters.output_min_x parameters.output_min_y parameters.output_min_z in
    let output_min_w = parameters.output_min_w in
    let output_max = Vec3.create parameters.output_max_x parameters.output_max_y parameters.output_max_z in
    let output_max_w = parameters.output_max_w in
    let group = optional_text group and into = optional_text into in
    let ramp = decode_parameter_ramp "sop/attribute_remap" ramp in
    let value vector w = numeric_value kind vector.Vec3.x vector.y vector.z w in
    let input = match input_range with
      | Remap_automatic -> Rdk.Attribute_ops.Remap_auto
      | Remap_explicit -> Rdk.Attribute_ops.Remap_explicit {
          min = value input_min input_min_w; max = value input_max input_max_w } in
    let output_min = value output_min output_min_w and output_max = value output_max output_max_w in
    Node.Private.make_geometry ?label ~operation:"attribute_remap" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input_node|] (fun ~node_id:_ context inputs ->
        match resolve_attribute_group ~operation:"attribute_remap" ~owner group
            inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Attribute_ops.remap ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~owner ~name ?into ~input
                ~output_min ~output_max ~policy ~ramp inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_mirror = struct
  open Rays_math
  let owner_parameter = Parameter.choice ~equal:( = ) [
      "Point", Rdk.Attribute_mirror.Mirror_point_attributes;
      "Vertex", Rdk.Attribute_mirror.Mirror_vertex_attributes;
      "Primitive", Rdk.Attribute_mirror.Mirror_primitive_attributes;
    ]
  let group_use_parameter = Parameter.choice ~equal:( = ) [
      "Group is source", Rdk.Attribute_mirror.Mirror_group_as_source;
      "Group is destination", Rdk.Attribute_mirror.Mirror_group_as_destination;
    ]
  let method_parameter = Parameter.choice ~equal:( = ) [
      "Plane", Mirror_plane; "Mapping attribute", Mirror_mapping;
    ]
  let transform_parameter = Parameter.choice ~equal:( = ) [
      "Copy", Mirror_copy; "UV", Mirror_uv; "Vector", Mirror_vector; "Point", Mirror_point;
    ]
  type parameters = {
    owner : Rdk.Attribute_mirror.owner
      [@sop.default Rdk.Attribute_mirror.Mirror_point_attributes]
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
        invalid_arg "sop/attribute_mirror: controls must be finite" in
      List.iter finite [parameters.origin_x; parameters.origin_y; parameters.origin_z;
        parameters.normal_x; parameters.normal_y; parameters.normal_z;
        parameters.distance; parameters.tolerance; parameters.uv_origin_u;
        parameters.uv_origin_v; parameters.uv_direction_u; parameters.uv_direction_v];
      if parameters.distance < 0. || parameters.tolerance < 0.
          || parameters.tolerance > sqrt Float.max_float then
        invalid_arg "sop/attribute_mirror: invalid distance or tolerance";
      if parameters.replace_strings && parameters.string_search = "" then
        invalid_arg "sop/attribute_mirror: empty string search";
      (match optional_text parameters.source_group, optional_text parameters.destination_group with
       | Some source, Some destination when source = destination ->
           invalid_arg "sop/attribute_mirror: source and destination groups must differ"
       | _ -> ());
      if parameters.transform = Mirror_uv
          && parameters.uv_direction_u = 0. && parameters.uv_direction_v = 0. then
        invalid_arg "sop/attribute_mirror: UV direction must be non-zero";
      validate_parameter_pattern "sop/attribute_mirror: attributes" parameters.attributes;
      match parameters.method_ with
      | Mirror_mapping ->
          if String.trim parameters.mapping_attribute = "" then
            invalid_arg "sop/attribute_mirror: empty mapping attribute";
          if String.trim parameters.mapping_destination_group = "" then
            invalid_arg "sop/attribute_mirror: empty mapping destination group";
          if parameters.transform = Mirror_vector || parameters.transform = Mirror_point then
            invalid_arg "sop/attribute_mirror: vector/point transforms require a plane"
      | Mirror_plane ->
          if parameters.owner = Rdk.Attribute_mirror.Mirror_vertex_attributes then
            invalid_arg "sop/attribute_mirror: vertex owner requires mapping";
          let nx = parameters.normal_x and ny = parameters.normal_y and nz = parameters.normal_z in
          let scale = Float.max (Float.abs nx) (Float.max (Float.abs ny) (Float.abs nz)) in
          if scale = 0. then invalid_arg "sop/attribute_mirror: normal must be non-zero";
          let x = nx /. scale and y = ny /. scale and z = nz /. scale in
          let length = sqrt (x *. x +. y *. y +. z *. z) in
          List.iter finite [parameters.origin_x +. parameters.distance *. (x /. length);
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
    let origin = Vec3.create parameters.origin_x parameters.origin_y parameters.origin_z in
    let normal = Vec3.create parameters.normal_x parameters.normal_y parameters.normal_z in
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
    Node.Private.make_geometry ?label ~operation:"attribute_mirror" ~version:1
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
                match Rdk.Attribute_mirror.run
                    ~cancel:(Context.cancel_token context)
                    ~grain:(Context.grain context) ?group:resolved_group ~group_use
                    ~attributes ~transform ?string_replace ?output_mapping
                    ?source_group ?destination_group ~owner ~method_ geometry with
                | Ok geometry -> cooked geometry
                | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_fade = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    fade_attribute : string [@sop.default "fade"] [@sop.label "Fade attribute"];
    start_attribute : string [@sop.default ""] [@sop.label "Start attribute"]
      [@sop.folder "Sources"];
    start_retime_offset : float [@sop.default 0.] [@sop.label "Start offset"]
      [@sop.folder "Sources/Start retime"] [@sop.min (-100.)]
      [@sop.max 100.];
    start_retime_scale : float [@sop.default 1.] [@sop.label "Start scale"]
      [@sop.folder "Sources/Start retime"] [@sop.min (-10.)]
      [@sop.max 10.];
    hold_scale_attribute : string [@sop.default ""]
      [@sop.label "Hold scale attribute"] [@sop.folder "Sources"];
    frame_offset : float [@sop.default 0.] [@sop.label "Frame offset"]
      [@sop.folder "Timing"] [@sop.min (-100.)] [@sop.max 100.];
    fade_in : float [@sop.default 2.] [@sop.label "Fade in"]
      [@sop.folder "Timing"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    fade_hold : float [@sop.default 0.] [@sop.label "Hold"]
      [@sop.folder "Timing"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    fade_out : float [@sop.default 2.] [@sop.label "Fade out"]
      [@sop.folder "Timing"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];

    fade_in_ramp : string [@sop.default "0:0,1:1"] [@sop.label "Fade in ramp"]
      [@sop.description "Comma-separated position:value knots spanning 0 to 1; blank is linear."];
    fade_out_ramp : string [@sop.default "0:1,1:0"] [@sop.label "Fade out ramp"]
      [@sop.description "Comma-separated position:value knots spanning 0 to 1; blank is inverse linear."];
    visualize : bool [@sop.default false] [@sop.label "Visualize fade"]
      [@sop.folder "Output"];
  } [@@sop.node_key "attribute_fade"] [@@sop.node_label "Attribute Fade"]
    [@@sop.validate fun parameters ->
      if String.trim parameters.fade_attribute = "" || parameters.fade_attribute = "P"
          || (parameters.visualize && parameters.fade_attribute = "Cd") then
        invalid_arg "sop/attribute_fade: invalid fade attribute";
      List.iter (fun value -> if not (Float.is_finite value) then
          invalid_arg "sop/attribute_fade: timing controls must be finite")
        [parameters.start_retime_offset; parameters.start_retime_scale; parameters.frame_offset;
         parameters.fade_in; parameters.fade_hold; parameters.fade_out];
      if parameters.fade_in < 0. || parameters.fade_hold < 0. || parameters.fade_out < 0. then
        invalid_arg "sop/attribute_fade: durations must be nonnegative";
      ignore (decode_parameter_ramp "sop/attribute_fade" parameters.fade_in_ramp);
      ignore (decode_parameter_ramp "sop/attribute_fade" parameters.fade_out_ramp)]
    [@@sop.node_category "Attribute/Motion"] [@@sop.node_inputs 3] [@@sop.node_slots "input, start_source, hold_source"]
    [@@sop.node_optional "1,2"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input start_source hold_source ->
    let label = Some label in
    let group = parameters.group in
    let fade_attribute = parameters.fade_attribute in
    let start_attribute = parameters.start_attribute in
    let start_retime_offset = parameters.start_retime_offset in
    let start_retime_scale = parameters.start_retime_scale in
    let hold_scale_attribute = parameters.hold_scale_attribute in
    let frame_offset = parameters.frame_offset in
    let fade_in = parameters.fade_in in
    let fade_hold = parameters.fade_hold in
    let fade_out = parameters.fade_out in
    let fade_in_ramp = parameters.fade_in_ramp in
    let fade_out_ramp = parameters.fade_out_ramp in
    let visualize = parameters.visualize in
    let group = optional_text group and start_attribute = optional_text start_attribute
    and hold_scale_attribute = optional_text hold_scale_attribute in
    let ramp fallback text = match decode_parameter_ramp "sop/attribute_fade" text with
      | [] -> fallback | knots -> knots in
    let fade_in_ramp = ramp [0., 0.; 1., 1.] fade_in_ramp
    and fade_out_ramp = ramp [0., 1.; 1., 0.] fade_out_ramp in
    let start_retime = start_retime_offset, start_retime_scale in
    let inputs, start_index, hold_index = match start_source, hold_source with
      | None, None -> [|input|], None, None
      | Some start, None -> [|input; start|], Some 1, None
      | None, Some hold -> [|input; hold|], None, Some 1
      | Some start, Some hold -> [|input; start; hold|], Some 1, Some 2 in
    Node.Private.make_geometry ?label ~operation:"attribute_fade" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:(Context.Dependencies.one Context.Dependencies.Frame)
      ~inputs (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_point_group "attribute_fade" group geometry with
        | Error error -> Error error
        | Ok points ->
            let start_source = Option.map (Array.unsafe_get inputs) start_index
            and hold_source = Option.map (Array.unsafe_get inputs) hold_index in
            match Rdk.Attribute_fade.fade ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ?start_source ?hold_source
                ~fade_attribute ?start_attribute ~start_retime
                ?hold_scale_attribute ~frame:(Int64.to_float (Context.frame context))
                ~frame_offset ~fade_in ~fade_hold ~fade_out ~fade_in_ramp
                ~fade_out_ramp ~visualize geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Point_velocity = struct
  open Rays_math
  let approximation_parameter = Parameter.choice ~equal:( = ) [
      "Backward difference", Rdk.Motion.Backward_difference;
      "Central difference", Rdk.Motion.Central_difference;
      "Forward difference", Rdk.Motion.Forward_difference;
    ]
  let initialization_parameter = Parameter.choice ~equal:( = ) [
      "Compute from deformation", Velocity_compute; "Keep incoming", Velocity_keep;
      "Set value", Velocity_set; "From attribute", Velocity_from_attribute;
    ]
  let unmatched_parameter = Parameter.choice ~equal:( = ) [
      "Error", Rdk.Motion.Velocity_unmatched_error;
      "Zero", Rdk.Motion.Velocity_unmatched_zero;
    ]
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    approximation : Rdk.Motion.velocity_approximation
      [@sop.default Rdk.Motion.Backward_difference]
      [@sop.label "Approximation"] [@sop.kind approximation_parameter];
    dt : float [@sop.default 0.016666666666666666] [@sop.label "Time step"]
      [@sop.min 0.000001] [@sop.max 10.] [@sop.hard_min 0.];
    initialization : Support.velocity_initialization [@sop.default Velocity_compute]
      [@sop.label "Initialization"] [@sop.kind initialization_parameter];
    set_x : float [@sop.default 0.] [@sop.label "Velocity X"]
      [@sop.folder "Initialization/Value"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "set"]
    set_y : float [@sop.default 0.] [@sop.label "Velocity Y"]
      [@sop.folder "Initialization/Value"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "set"]
    set_z : float [@sop.default 0.] [@sop.label "Velocity Z"]
      [@sop.folder "Initialization/Value"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "set"]
    source_attribute : string [@sop.default "v"]
      [@sop.label "Source attribute"] [@sop.folder "Initialization/Attribute"];
    source_scale : float [@sop.default 1.] [@sop.label "Source scale"]
      [@sop.folder "Initialization/Attribute"] [@sop.min (-10.)]
      [@sop.max 10.];
    match_attribute : string [@sop.default ""] [@sop.label "Match attribute"]
      [@sop.folder "Matching"];
    unmatched : Rdk.Motion.velocity_unmatched
      [@sop.default Rdk.Motion.Velocity_unmatched_error]
      [@sop.label "Unmatched"] [@sop.folder "Matching"]
      [@sop.kind unmatched_parameter];
    velocity_attribute : string [@sop.default "v"]
      [@sop.label "Velocity attribute"] [@sop.folder "Output"];
    add_x : float [@sop.default 0.] [@sop.label "Add X"]
      [@sop.folder "Output/Add velocity"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "add"]
    add_y : float [@sop.default 0.] [@sop.label "Add Y"]
      [@sop.folder "Output/Add velocity"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "add"]
    add_z : float [@sop.default 0.] [@sop.label "Add Z"]
      [@sop.folder "Output/Add velocity"] [@sop.min (-100.)]
      [@sop.max 100.]; [@sop.vec3 "add"]
    compute_acceleration : bool [@sop.default false]
      [@sop.label "Compute acceleration"] [@sop.folder "Output"];
    acceleration_attribute : string [@sop.default "accel"]
      [@sop.label "Acceleration attribute"] [@sop.folder "Output"];
  } [@@sop.node_key "point_velocity"] [@@sop.node_label "Point Velocity"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.dt) || parameters.dt <= 0. then
        invalid_arg "sop/point_velocity: time step must be finite and positive";
      List.iter (fun value -> if not (Float.is_finite value) then
          invalid_arg "sop/point_velocity: vector and scale controls must be finite")
        [parameters.set_x; parameters.set_y; parameters.set_z; parameters.source_scale;
         parameters.add_x; parameters.add_y; parameters.add_z];
      if String.trim parameters.velocity_attribute = "" || parameters.velocity_attribute = "P" then
        invalid_arg "sop/point_velocity: invalid velocity attribute";
      if parameters.compute_acceleration && (String.trim parameters.acceleration_attribute = ""
          || parameters.acceleration_attribute = "P"
          || parameters.acceleration_attribute = parameters.velocity_attribute) then
        invalid_arg "sop/point_velocity: invalid acceleration attribute";
      if parameters.compute_acceleration && (parameters.initialization <> Velocity_compute
          || parameters.approximation <> Rdk.Motion.Central_difference) then
        invalid_arg "sop/point_velocity: acceleration requires central deformation";
      if parameters.initialization = Velocity_from_attribute && String.trim parameters.source_attribute = "" then
        invalid_arg "sop/point_velocity: empty source attribute"]
    [@@sop.node_category "Attribute/Motion"] [@@sop.node_inputs 3] [@@sop.node_slots "input, previous, next"]
    [@@sop.node_optional "1,2"]   [@@deriving sop_params, sop_node]
  let initialization parameters = match parameters.initialization with
    | Velocity_compute -> Rdk.Motion.Compute_from_deformation
    | Velocity_keep -> Rdk.Motion.Keep_incoming
    | Velocity_set -> Rdk.Motion.Set_value
        (Vec3.create parameters.set_x parameters.set_y parameters.set_z)
    | Velocity_from_attribute -> Rdk.Motion.From_attribute {
        name = parameters.source_attribute; scale = parameters.source_scale }
  let build = parameters_build (fun ~label parameters input previous next ->
    let inputs = Array.of_list (input :: List.filter_map Fun.id [previous; next]) in
    let cook_mode = if Array.length inputs = 1 then Node.Duplicate_input 0
      else Node.Generic in
    Node.Private.make_geometry ~label ~operation:"point_velocity" ~version:1 ~parameters:"" ~cook_mode
      ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        let slot = ref 1 in
        let take present = if not present then None
          else (let value = Some inputs.(!slot) in incr slot; value) in
        let previous = take (previous <> None) in
        let next = take (next <> None) in
        let points = match optional_text parameters.group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "point_velocity could not find point group %S" name))) in
        Result.bind points (fun points ->
          (match Rdk.Motion.point_velocity
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ?points ?previous ?next ~approximation:parameters.approximation
            ~dt:parameters.dt ~initialization:(initialization parameters)
            ?match_attribute:(optional_text parameters.match_attribute)
            ~unmatched:parameters.unmatched
            ~velocity_attribute:parameters.velocity_attribute
            ~add_velocity:(Vec3.create parameters.add_x parameters.add_y
              parameters.add_z)
            ~compute_acceleration:parameters.compute_acceleration
            ~acceleration_attribute:parameters.acceleration_attribute inputs.(0) with
           | Ok geometry -> cooked geometry
           | Error error -> structured_rdk_error error))))

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
    unmatched : Rdk.Attribute_ops.unmatched
      [@sop.default Rdk.Attribute_ops.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
  } [@@sop.node_key "attribute_transfer_all"]
    [@@sop.node_label "Attribute Transfer All"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/attribute_transfer_all: " ^ message) in
      let patterns = [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern;parameters.detail_pattern] in
      if List.for_all (fun text -> String.trim text = "") patterns then refuse "at least one owner pattern is required";
      List.iter (fun text -> if String.trim text <> "" then validate_parameter_pattern "sop/attribute_transfer_all" text) patterns;
      if parameters.neighbors < 1 then refuse "neighbors must be positive";
      if not (List.for_all Float.is_finite [parameters.power;parameters.kernel_radius;
          parameters.max_distance;parameters.blend_width;parameters.uniform_bias]) then refuse "sampling controls must be finite";
      if parameters.power < 0. || (parameters.mode = Transfer_inverse && parameters.power = 0.) then refuse "invalid inverse power";
      if parameters.kernel_radius < 0. || parameters.kernel_radius > sqrt Float.max_float
          || parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float
          || parameters.blend_width < 0. then refuse "distances must be nonnegative and safely squarable";
      if parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "uniform bias must be within [0, 1]";
      if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then refuse "blend width requires an explicit maximum distance";
      if parameters.distance_mode = Kernel_explicit &&
          parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance plus blend width must be safely squarable"]
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
    if point_pattern = None && vertex_pattern = None
        && primitive_pattern = None && detail_pattern = None then
      invalid_arg "sop/attribute_transfer_all: at least one owner pattern is required";
    List.iter (fun (owner, pattern) -> match pattern with
      | None -> ()
      | Some pattern ->
          (match Rdk.Attribute_pattern.compile pattern with
           | Ok _ -> ()
           | Error message -> invalid_arg (Printf.sprintf
               "sop/attribute_transfer_all: invalid %s pattern: %s" owner message)))
      ["point", point_pattern; "vertex", vertex_pattern;
       "primitive", primitive_pattern; "detail", detail_pattern];
    Node.Private.make_geometry ?label ~operation:"attribute_transfer_all" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Attribute_ops.transfer_all
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
            ~mode ?max_distance ~blend_width ~falloff ~unmatched
            ~source:inputs.(0) ~target:inputs.(1) () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_transfer_surface = struct
  let vertex_selection_parameter = Parameter.choice ~equal:( = ) [
      "All triangle vertices", Rdk.Attribute_ops.All_triangle_vertices;
      "Any triangle vertex", Rdk.Attribute_ops.Any_triangle_vertex;
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
    unmatched : Rdk.Attribute_ops.unmatched
      [@sop.default Rdk.Attribute_ops.Keep_target] [@sop.label "Unmatched"]
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
    source_vertex_selection : Rdk.Attribute_ops.surface_vertex_selection
      [@sop.default Rdk.Attribute_ops.All_triangle_vertices]
      [@sop.label "Vertex selection"] [@sop.folder "Groups/Source"]
      [@sop.kind vertex_selection_parameter];
    target_group : string [@sop.default ""] [@sop.label "Target group"]
      [@sop.folder "Groups/Target"];
    target_group_pattern : string [@sop.default ""]
      [@sop.label "Target group pattern"] [@sop.folder "Groups/Target"];
  } [@@sop.node_key "attribute_transfer_surface"]
    [@@sop.node_label "Attribute Transfer Surface"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/attribute_transfer_surface: " ^ message) in
      if parameters.target_owner = Rdk.Attribute.Detail then refuse "detail target is not spatial";
      let attributes = decode_surface_attributes parameters.attributes in
      let names = List.map (fun (attribute : Rdk.Attribute_ops.surface_attribute) -> attribute.target_name) attributes in
      if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "target attribute names must be distinct";
      if parameters.target_owner = Rdk.Attribute.Point && List.mem "P" names then refuse "target P is not an ordinary attribute";
      let distance = optional_text parameters.distance_attribute in
      if (parameters.target_owner = Rdk.Attribute.Point && distance = Some "P")
          || Option.fold ~none:false ~some:(fun name -> List.mem name names) distance then refuse "distance output conflicts with attributes";
      if not (Float.is_finite parameters.max_distance && Float.is_finite parameters.blend_width && Float.is_finite parameters.uniform_bias) then
        refuse "distance controls must be finite";
      if parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float || parameters.blend_width < 0.
          || parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "distance controls are out of range";
      if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then refuse "blend width requires an explicit maximum distance";
      if parameters.distance_mode = Kernel_explicit && parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance window exceeds the squared-distance range";
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "attribute_transfer_surface" pattern)
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
    let attributes = decode_surface_attributes attributes in
    let max_distance = if distance_mode = Kernel_auto then None else Some max_distance in
    let falloff = transfer_falloff falloff uniform_bias and distance_attribute = optional_text distance_attribute in
    let source_group,source_group_pattern = exact_or_pattern source_group source_group_pattern
    and source_vertex_group,source_vertex_group_pattern = exact_or_pattern source_vertex_group source_vertex_group_pattern
    and target_group,target_group_pattern = exact_or_pattern target_group target_group_pattern in
    let source_group_pattern_compiled = compile_transfer_group_pattern
        "attribute_transfer_surface" "source" source_group_pattern
    and target_group_pattern_compiled = compile_transfer_group_pattern
        "attribute_transfer_surface" "target" target_group_pattern
    and source_vertex_group_pattern_compiled = compile_transfer_group_pattern
        "attribute_transfer_surface" "source vertex" source_vertex_group_pattern in
    Node.Private.make_geometry ?label ~operation:"attribute_transfer_surface" ~version:5
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
                     ~cancel ~grain ?max_distance ~blend_width
                     ~falloff ~unmatched ~target_owner ?distance_attribute
                     ?source_primitives ?source_vertices ~source_vertex_selection
                     ?target_elements:target_points
                     ~attributes ~source:inputs.(0)
                     ~target:inputs.(1) () with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error)))
  )
  let factory = parameters_factory build
end

module Attribute_copy = struct
  let match_parameter = Parameter.choice ~equal:( = ) [
      "Cyclic", Copy_cyclic; "By attribute values", Copy_by_values;
      "To source element", Copy_to_element;
    ]
  type parameters = {
    group_owner : Rdk.Group.owner [@sop.default Rdk.Group.Point]
      [@sop.label "Selection owner"] [@sop.kind ordinary_group_owner_parameter];
    match_ : Support.copy_match [@sop.default Copy_cyclic] [@sop.label "Element matching"]
      [@sop.kind match_parameter];
    source_match_attribute : string [@sop.default "id"]
      [@sop.label "Source match attribute"] [@sop.folder "Matching"];
    target_match_attribute : string [@sop.default "id"]
      [@sop.label "Target match attribute"] [@sop.folder "Matching"];
    target_element_attribute : string [@sop.default "source"]
      [@sop.label "Source element attribute"] [@sop.folder "Matching"];
    allow_position : bool [@sop.default false] [@sop.label "Allow P"];
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Groups/Source"];
    source_group_pattern : string [@sop.default ""]
      [@sop.label "Source group pattern"] [@sop.folder "Groups/Source"];
    target_group : string [@sop.default ""] [@sop.label "Target group"]
      [@sop.folder "Groups/Target"];
    target_group_pattern : string [@sop.default ""]
      [@sop.label "Target group pattern"] [@sop.folder "Groups/Target"];
    rules : string [@sop.default "point\t*\t"]
      [@sop.label "Rules (owner, pattern, destination)"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "attribute_copy"] [@@sop.node_label "Attribute Copy"]

    [@@sop.validate fun parameters ->
      ignore (decode_copy_rules parameters.rules);
      let validate_name name = if String.trim name = "" || name = "P" then
        invalid_arg "sop/attribute_copy: matching requires a nonblank integer or text attribute" in
      (match parameters.match_ with
       | Copy_cyclic -> ()
       | Copy_by_values ->
           if parameters.group_owner = Rdk.Group.Vertex then
             invalid_arg "sop/attribute_copy: value matching supports point or primitive groups only";
           validate_name parameters.source_match_attribute;validate_name parameters.target_match_attribute
       | Copy_to_element -> validate_name parameters.target_element_attribute);
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "sop/attribute_copy" pattern)
        [parameters.source_group_pattern;parameters.target_group_pattern]]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source target ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let match_ = parameters.match_ in
    let source_match_attribute = parameters.source_match_attribute in
    let target_match_attribute = parameters.target_match_attribute in
    let target_element_attribute = parameters.target_element_attribute in
    let allow_position = parameters.allow_position in
    let source_group = parameters.source_group in
    let source_group_pattern = parameters.source_group_pattern in
    let target_group = parameters.target_group in
    let target_group_pattern = parameters.target_group_pattern in
    let rules = parameters.rules in
    let rules = decode_copy_rules rules in
    let match_ = match match_ with
      | Copy_cyclic -> Rdk.Attribute_ops.Cyclic
      | Copy_by_values -> Rdk.Attribute_ops.By_values {source_attribute=source_match_attribute;target_attribute=target_match_attribute}
      | Copy_to_element -> Rdk.Attribute_ops.To_element {target_attribute=target_element_attribute} in
    let source_group,source_group_pattern = exact_or_pattern source_group source_group_pattern
    and target_group,target_group_pattern = exact_or_pattern target_group target_group_pattern in
    let source_group_pattern_compiled = compile_transfer_group_pattern
        "attribute_copy" "source" source_group_pattern
    and target_group_pattern_compiled = compile_transfer_group_pattern
        "attribute_copy" "target" target_group_pattern in
    let group_owner_name = match group_owner with
      | Rdk.Group.Point -> "point"
      | Rdk.Group.Vertex -> "vertex"
      | Rdk.Group.Primitive -> "primitive" in
    Node.Private.make_geometry ?label ~operation:"attribute_copy" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|]
      (fun ~node_id:_ context inputs ->
        let cancel = Context.cancel_token context and grain = Context.grain context in
        match resolve_transfer_group ~operation:"attribute_copy"
            ~owner:group_owner ~owner_name:group_owner_name ~exact:source_group
            ~pattern:source_group_pattern_compiled ~cancel ~grain inputs.(0) with
        | Error error -> Error error
        | Ok source_group ->
            (match resolve_transfer_group ~operation:"attribute_copy"
                ~owner:group_owner ~owner_name:group_owner_name ~exact:target_group
                ~pattern:target_group_pattern_compiled ~cancel ~grain inputs.(1) with
             | Error error -> Error error
             | Ok target_group ->
                 match Rdk.Attribute_ops.copy ~cancel ~grain ?source_group
                     ?target_group ~match_ ~allow_position ~group_owner ~rules
                     ~source:inputs.(0) ~target:inputs.(1) () with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Attribute_noise_quaternion = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Group"];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "orient"] [@sop.label "Attribute"];
    location : string
      [@sop.default "element-number"]
      [@sop.label "Location"] ;
    range : string
      [@sop.default "zero-centered"]
      [@sop.label "Range"] ;
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.min 0]
      [@sop.max 9999];
    frequency_x : float [@sop.default 1.] [@sop.label "Frequency X"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    frequency_y : float [@sop.default 1.] [@sop.label "Frequency Y"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    frequency_z : float [@sop.default 1.] [@sop.label "Frequency Z"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    octaves : int [@sop.default 1] [@sop.label "Octaves"]
      [@sop.min 1] [@sop.max 8] [@sop.hard_min 1];
  } [@@sop.node_key "attribute_noise_quaternion"]
    [@@sop.node_operation "attribute_noise"]
    [@@sop.node_label "Attribute Noise (Quaternion)"]
    [@@sop.node_facts {elementwise = (match parameters.owner with
        | Rdk.Attribute.Point -> Node.Points | Primitive -> Node.Primitives | Vertex | Detail -> Node.None);
      reads = ["P"; parameters.name] @ (match Quaternion_noise_encoding.decode_location parameters.location with
        | Ok (Rdk.Attribute_ops.Noise_attribute name) -> [name] | _ -> []);
      writes = [parameters.name]; topology = Node.Preserved; exact = false}]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/attribute_noise_quaternion: " ^ message) in
      if String.trim parameters.name = "" then refuse "attribute name must be nonblank";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then refuse "quaternion output cannot replace P";
      if parameters.owner = Rdk.Attribute.Detail && optional_text parameters.group <> None then refuse "detail attributes do not accept a group";
      if not (List.for_all Float.is_finite [parameters.frequency_x;parameters.frequency_y;parameters.frequency_z])
          || parameters.frequency_x < 0. || parameters.frequency_y < 0. || parameters.frequency_z < 0. then refuse "frequency must be finite and nonnegative";
      if parameters.octaves < 1 || parameters.octaves > 64 then refuse "octaves must be in [1,64]";
      (match Support.Quaternion_noise_encoding.decode_location parameters.location with
       | Error message -> refuse message
       | Ok (Rdk.Attribute_ops.Noise_attribute name) when String.trim name = "" -> refuse "location attribute must be nonblank"
       | Ok _ -> ());
      (match Support.Quaternion_noise_encoding.decode_range parameters.range with
       | Error message -> refuse message
       | Ok (Rdk.Attribute_ops.Noise_min_max (Rdk.Attribute_ops.Vec4 (a,b,c,d),Rdk.Attribute_ops.Vec4 (e,f,g,h))) ->
           if not (List.for_all Float.is_finite [a;b;c;d;e;f;g;h]) || a > e || b > f || c > g || d > h then refuse "range bounds must be finite and ordered"
       | Ok (Rdk.Attribute_ops.Noise_min_max _) -> refuse "quaternion range bounds must have four components"
       | Ok _ -> ())]
    [@@sop.node_category "Attribute/Noise"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let owner = parameters.owner in
    let name = parameters.name in
    let location = parameters.location in
    let range = parameters.range in
    let seed = parameters.seed in
    let frequency = Rays_math.Vec3.create parameters.frequency_x parameters.frequency_y parameters.frequency_z in
    let octaves = parameters.octaves in
    let group = optional_text group in
    let location = Quaternion_noise_encoding.decode_location location |> Result.get_ok
    and range = Quaternion_noise_encoding.decode_range range |> Result.get_ok in
    Node.Private.make_geometry ?label ~operation:"attribute_noise" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_attribute_group ~operation:"attribute_noise" ~owner group inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Attribute_ops.noise ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
              ?selection ~seed ~owner ~name ~kind:Rdk.Attribute_ops.Noise_quaternion ~location ~range ~operation:Rdk.Attribute_ops.Noise_set
              ~blend:1. ~frequency ~offset:Rays_math.Vec3.zero ~octaves ~lacunarity:2. ~roughness:0.5 inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_noise = struct
  let kind_parameter = Parameter.choice ~equal:( = ) [
      "Float", Rdk.Attribute_ops.Noise_float;
      "Vector", Rdk.Attribute_ops.Noise_vector;
      "Quaternion", Rdk.Attribute_ops.Noise_quaternion;
    ]
  let location_parameter = Parameter.choice ~equal:( = ) [
      "Position", Support.Noise_position; "Element number", Support.Noise_element_number;
      "Attribute", Support.Noise_attribute;
    ]
  let range_parameter = Parameter.choice ~equal:( = ) [
      "Positive", Support.Noise_positive; "Zero centered", Support.Noise_zero_centered;
      "Minimum / maximum", Support.Noise_min_max;
    ]
  let operation_parameter = Parameter.choice ~equal:( = ) [
      "Set initial", Rdk.Attribute_ops.Noise_set_initial;
      "Set", Rdk.Attribute_ops.Noise_set;
      "Add", Rdk.Attribute_ops.Noise_add;
      "Subtract", Rdk.Attribute_ops.Noise_subtract;
      "Multiply", Rdk.Attribute_ops.Noise_multiply;
      "Minimum", Rdk.Attribute_ops.Noise_minimum;
      "Maximum", Rdk.Attribute_ops.Noise_maximum;
    ]
  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "noise"] [@sop.label "Attribute"];
    group : string [@sop.default ""] [@sop.label "Group"];
    kind : Rdk.Attribute_ops.noise_kind
      [@sop.default Rdk.Attribute_ops.Noise_float] [@sop.label "Type"]
      [@sop.kind kind_parameter];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Noise"];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.folder "Noise"]
      [@sop.min 0] [@sop.max 9999];
    location : Support.noise_location [@sop.default Support.Noise_position] [@sop.label "Location"]
      [@sop.folder "Sampling"] [@sop.kind location_parameter] ;
    location_attribute : string [@sop.default "P"]
      [@sop.label "Location attribute"] [@sop.folder "Sampling"];
    range : Support.noise_range [@sop.default Support.Noise_positive] [@sop.label "Range"]
      [@sop.folder "Output"] [@sop.kind range_parameter] ;
    min_x : float [@sop.default 0.] [@sop.label "Minimum X"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "min"]
    min_y : float [@sop.default 0.] [@sop.label "Minimum Y"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "min"]
    min_z : float [@sop.default 0.] [@sop.label "Minimum Z"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "min"]
    min_w : float [@sop.default 0.] [@sop.label "Minimum W"]
      [@sop.folder "Output/Minimum"] [@sop.min (-10.)] [@sop.max 10.];
    max_x : float [@sop.default 1.] [@sop.label "Maximum X"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "max"]
    max_y : float [@sop.default 1.] [@sop.label "Maximum Y"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "max"]
    max_z : float [@sop.default 1.] [@sop.label "Maximum Z"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "max"]
    max_w : float [@sop.default 1.] [@sop.label "Maximum W"]
      [@sop.folder "Output/Maximum"] [@sop.min (-10.)] [@sop.max 10.];
    operation : Rdk.Attribute_ops.noise_operation
      [@sop.default Rdk.Attribute_ops.Noise_set] [@sop.label "Operation"]
      [@sop.folder "Output"] [@sop.kind operation_parameter];
    blend : float [@sop.default 1.] [@sop.label "Blend"]
      [@sop.folder "Output"] [@sop.min 0.] [@sop.max 1.];
    frequency_x : float [@sop.default 1.] [@sop.label "Frequency X"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "frequency"]
    frequency_y : float [@sop.default 1.] [@sop.label "Frequency Y"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "frequency"]
    frequency_z : float [@sop.default 1.] [@sop.label "Frequency Z"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "frequency"]
    offset_x : float [@sop.default 0.] [@sop.label "Offset X"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_y : float [@sop.default 0.] [@sop.label "Offset Y"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_z : float [@sop.default 0.] [@sop.label "Offset Z"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    octaves : int [@sop.default 1] [@sop.label "Octaves"]
      [@sop.folder "Noise/Fractal"] [@sop.min 1] [@sop.max 12]
      [@sop.hard_min 1];
    lacunarity : float [@sop.default 2.] [@sop.label "Lacunarity"]
      [@sop.folder "Noise/Fractal"] [@sop.min 0.] [@sop.max 8.];
    roughness : float [@sop.default 0.5] [@sop.label "Roughness"]
      [@sop.folder "Noise/Fractal"] [@sop.min 0.] [@sop.max 1.];
  } [@@sop.node_key "attribute_noise"] [@@sop.node_label "Attribute Noise"]
    [@@sop.node_facts {elementwise = (match parameters.owner with
        | Rdk.Attribute.Point -> Node.Points | Primitive -> Node.Primitives | Vertex | Detail -> Node.None);
      reads = List.filter (fun name -> String.trim name <> "")
        ["P"; parameters.name; parameters.location_attribute];
      writes = if parameters.name = "P" then ["P"; "N"] else [parameters.name];
      topology = Node.Preserved; exact = false}]
    [@@sop.node_category "Attribute/Noise"] [@@sop.node_inputs 1]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/attribute_noise: " ^ message) in
      if String.trim parameters.name = "" then refuse "attribute name must be nonblank";
      if parameters.owner = Rdk.Attribute.Detail && optional_text parameters.group <> None then refuse "detail attributes do not accept a group";
      if parameters.blend < 0. || parameters.blend > 1. then refuse "blend must be in [0,1]";
      if parameters.octaves < 1 || parameters.octaves > 64 then refuse "octaves must be in [1,64]";
      if parameters.lacunarity <= 0. then refuse "lacunarity must be positive";
      if parameters.roughness < 0. || parameters.roughness > 1. then refuse "roughness must be in [0,1]";
      if parameters.location = Support.Noise_attribute && String.trim parameters.location_attribute = "" then refuse "location attribute must be nonblank";
      if parameters.kind = Rdk.Attribute_ops.Noise_quaternion
          && parameters.operation <> Rdk.Attribute_ops.Noise_set && parameters.operation <> Rdk.Attribute_ops.Noise_set_initial then
        refuse "quaternion output supports set operations only";
      if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" && parameters.kind <> Rdk.Attribute_ops.Noise_vector
          && parameters.operation <> Rdk.Attribute_ops.Noise_set_initial then refuse "P output requires vector storage";
      if parameters.range = Support.Noise_min_max then (
        let bounds = [parameters.min_x,parameters.max_x] @ (match parameters.kind with
          | Rdk.Attribute_ops.Noise_float -> []
          | Noise_vector -> [parameters.min_y,parameters.max_y;parameters.min_z,parameters.max_z]
          | Noise_quaternion -> [parameters.min_y,parameters.max_y;parameters.min_z,parameters.max_z;parameters.min_w,parameters.max_w]) in
        if List.exists (fun (low,high) -> low > high) bounds then refuse "range bounds must be ordered")]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let context_seed = parameters.context_seed in
    let seed = parameters.seed in
    let location = parameters.location in
    let location_attribute = parameters.location_attribute in
    let range = parameters.range in
    let min = Rays_math.Vec3.create parameters.min_x parameters.min_y parameters.min_z in
    let min_w = parameters.min_w in
    let max = Rays_math.Vec3.create parameters.max_x parameters.max_y parameters.max_z in
    let max_w = parameters.max_w in
    let operation = parameters.operation in
    let blend = parameters.blend in
    let frequency = Rays_math.Vec3.create parameters.frequency_x parameters.frequency_y parameters.frequency_z in
    let offset = Rays_math.Vec3.create parameters.offset_x parameters.offset_y parameters.offset_z in
    let octaves = parameters.octaves in
    let lacunarity = parameters.lacunarity in
    let roughness = parameters.roughness in
    let owner = parameters.owner in
    let name = parameters.name in
    let kind = parameters.kind in
    let group = optional_text group in
    let seed = if context_seed then None else Some seed in
    let location = match location with
      | Noise_position -> Rdk.Attribute_ops.Noise_position
      | Noise_element_number -> Rdk.Attribute_ops.Noise_element_number
      | Noise_attribute -> Rdk.Attribute_ops.Noise_attribute location_attribute in
    let range = match range with
      | Noise_positive -> Rdk.Attribute_ops.Noise_positive
      | Noise_zero_centered -> Rdk.Attribute_ops.Noise_zero_centered
      | Noise_min_max -> let numeric_kind = match kind with
          | Rdk.Attribute_ops.Noise_float -> Numeric_scalar | Noise_vector -> Numeric_vec3 | Noise_quaternion -> Numeric_vec4 in
        Rdk.Attribute_ops.Noise_min_max (numeric_value numeric_kind min.Rays_math.Vec3.x min.y min.z min_w,
          numeric_value numeric_kind max.Rays_math.Vec3.x max.y max.z max_w) in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("attribute_noise:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"attribute_noise" ~version:1
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0) ~dependencies
      ~inputs:[|input|] (fun ~node_id context inputs ->
        match resolve_attribute_group ~operation:"attribute_noise" ~owner group
            inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Option.value ~default:(mixed_seed context identity) seed in
            match Rdk.Attribute_ops.noise
                ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
                ?selection ~seed ~owner ~name ~kind ~location ~range ~operation
                ~blend ~frequency ~offset ~octaves ~lacunarity ~roughness inputs.(0)
            with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_randomize = struct
  let distribution_parameter = Parameter.choice ~equal:( = ) [
      "Constant", Support.Random_constant; "Two values", Support.Random_two_values; "Uniform", Support.Random_uniform;
      "Uniform discrete", Support.Random_uniform_discrete; "Normal", Support.Random_normal;
      "Exponential", Support.Random_exponential; "Log normal", Support.Random_log_normal;
      "Cauchy", Support.Random_cauchy; "Direction", Support.Random_direction;
      "Inside sphere", Support.Random_inside_sphere;
      "Inside sphere cone", Support.Random_inside_sphere_cone;
      "Custom ramp", Support.Random_custom_ramp; "Custom discrete", Support.Random_custom_discrete;
      "Custom discrete text", Support.Random_custom_discrete_text;
    ]
  let operation_parameter = Parameter.choice ~equal:( = ) [
      "Set", Rdk.Attribute_ops.Random_set;
      "Add", Rdk.Attribute_ops.Random_add;
      "Minimum", Rdk.Attribute_ops.Random_minimum;
      "Maximum", Rdk.Attribute_ops.Random_maximum;
      "Multiply", Rdk.Attribute_ops.Random_multiply;
    ]
  type parameters = {
    selection_owner : element_owner [@sop.default Element_point]
      [@sop.label "Selection owner"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    selection_group : string [@sop.default ""] [@sop.label "Selection group"] [@sop.folder "Selection"];
    ramp : string [@sop.default "0:0,1:1"] [@sop.label "Ramp"] [@sop.folder "Distribution/Custom"];
    entries : string [@sop.default "0\t1\n1\t1"] [@sop.label "Numeric entries (components, weight)"] [@sop.folder "Distribution/Custom"];
    text_entries : string [@sop.default "low\t1\nhigh\t1"] [@sop.label "Text entries (text, weight)"] [@sop.folder "Distribution/Custom"];
    use_vector_limits : bool [@sop.default false] [@sop.label "Per-component limits"] [@sop.folder "Clamp"];
    minimum_vector_x : float [@sop.default 0.] [@sop.label "minimum x"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "minimum_vector"]
    minimum_vector_y : float [@sop.default 0.] [@sop.label "minimum y"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "minimum_vector"]
    minimum_vector_z : float [@sop.default 0.] [@sop.label "minimum z"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "minimum_vector"]
    minimum_w : float [@sop.default 0.] [@sop.label "minimum w"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.];
    maximum_vector_x : float [@sop.default 1.] [@sop.label "maximum x"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "maximum_vector"]
    maximum_vector_y : float [@sop.default 1.] [@sop.label "maximum y"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "maximum_vector"]
    maximum_vector_z : float [@sop.default 1.] [@sop.label "maximum z"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "maximum_vector"]
    maximum_w : float [@sop.default 1.] [@sop.label "maximum w"] [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.];
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Owner"] [@sop.kind attribute_owner_parameter];
    name : string [@sop.default "random"] [@sop.label "Attribute"];
    group : string [@sop.default ""] [@sop.label "Group"];
    kind : numeric_kind [@sop.default Numeric_scalar] [@sop.label "Value type"]
      [@sop.kind numeric_kind_parameter];
    distribution : Support.random_distribution [@sop.default Support.Random_uniform]
      [@sop.label "Distribution"] [@sop.kind distribution_parameter] ;
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Random"];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.folder "Random"]
      [@sop.min 0] [@sop.max 9999];
    seed_attribute : string [@sop.default ""] [@sop.label "Seed attribute"]
      [@sop.folder "Random"];
    fraction_attribute : string [@sop.default ""]
      [@sop.label "Fraction attribute"] [@sop.folder "Random"];
    a_x : float [@sop.default 0.] [@sop.label "A / minimum X"]
      [@sop.folder "Distribution/A"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "a"]
    a_y : float [@sop.default 0.] [@sop.label "A / minimum Y"]
      [@sop.folder "Distribution/A"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "a"]
    a_z : float [@sop.default 0.] [@sop.label "A / minimum Z"]
      [@sop.folder "Distribution/A"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "a"]
    a_w : float [@sop.default 0.] [@sop.label "A / minimum W"]
      [@sop.folder "Distribution/A"] [@sop.min (-10.)] [@sop.max 10.];
    b_x : float [@sop.default 1.] [@sop.label "B / maximum X"]
      [@sop.folder "Distribution/B"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "b"]
    b_y : float [@sop.default 1.] [@sop.label "B / maximum Y"]
      [@sop.folder "Distribution/B"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "b"]
    b_z : float [@sop.default 1.] [@sop.label "B / maximum Z"]
      [@sop.folder "Distribution/B"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "b"]
    b_w : float [@sop.default 1.] [@sop.label "B / maximum W"]
      [@sop.folder "Distribution/B"] [@sop.min (-10.)] [@sop.max 10.];
    step_x : float [@sop.default 1.] [@sop.label "Step X"]
      [@sop.folder "Distribution/Step"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "step"]
    step_y : float [@sop.default 1.] [@sop.label "Step Y"]
      [@sop.folder "Distribution/Step"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "step"]
    step_z : float [@sop.default 1.] [@sop.label "Step Z"]
      [@sop.folder "Distribution/Step"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "step"]
    step_w : float [@sop.default 1.] [@sop.label "Step W"]
      [@sop.folder "Distribution/Step"] [@sop.min 0.] [@sop.max 10.];
    probability_b : float [@sop.default 0.5] [@sop.label "Probability B"]
      [@sop.folder "Distribution"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    cone_angle : float [@sop.default 0.7853981633974483]
      [@sop.label "Cone angle"] [@sop.folder "Distribution"] [@sop.min 0.]
      [@sop.max 3.141592653589793];
    dimensions : int [@sop.default 3] [@sop.label "Dimensions"]
      [@sop.folder "Distribution"] [@sop.min 1] [@sop.max 4]
      [@sop.hard_min 1] [@sop.hard_max 4];
    use_minimum : bool [@sop.default false] [@sop.label "Clamp minimum"]
      [@sop.folder "Clamp"];
    minimum : float [@sop.default 0.] [@sop.label "Minimum"]
      [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.];
    use_maximum : bool [@sop.default false] [@sop.label "Clamp maximum"]
      [@sop.folder "Clamp"];
    maximum : float [@sop.default 1.] [@sop.label "Maximum"]
      [@sop.folder "Clamp"] [@sop.min (-10.)] [@sop.max 10.];
    direction_bias : float [@sop.default 0.] [@sop.label "Direction bias"]
      [@sop.folder "Output"] [@sop.min (-1.)] [@sop.max 1.];
    operation : Rdk.Attribute_ops.random_operation
      [@sop.default Rdk.Attribute_ops.Random_set] [@sop.label "Operation"]
      [@sop.folder "Output"] [@sop.kind operation_parameter];
    scale : float [@sop.default 1.] [@sop.label "Global scale"]
      [@sop.folder "Output"] [@sop.min (-10.)] [@sop.max 10.];
  } [@@sop.node_key "attribute_randomize"]
    [@@sop.node_label "Attribute Randomize"]
    [@@sop.node_category "Attribute/Random"] [@@sop.node_inputs 1]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/attribute_randomize: " ^ message) in
      if String.trim parameters.name = "" then refuse "attribute name must be nonblank";
      if optional_text parameters.group <> None && optional_text parameters.selection_group <> None then refuse "group and typed selection are mutually exclusive";
      if parameters.owner = Rdk.Attribute.Detail && (optional_text parameters.group <> None || optional_text parameters.selection_group <> None) then refuse "detail attributes do not accept a group";
      let distribution = random_distribution_value ~distribution:parameters.distribution ~kind:parameters.kind ~a:(Rays_math.Vec3.create parameters.a_x parameters.a_y parameters.a_z) ~a_w:parameters.a_w ~b:(Rays_math.Vec3.create parameters.b_x parameters.b_y parameters.b_z) ~b_w:parameters.b_w ~step:(Rays_math.Vec3.create parameters.step_x parameters.step_y parameters.step_z) ~step_w:parameters.step_w ~probability_b:parameters.probability_b ~cone_angle:parameters.cone_angle ~dimensions:parameters.dimensions ~ramp:parameters.ramp ~entries:parameters.entries ~text_entries:parameters.text_entries in
      validate_random_distribution ~direction_bias:parameters.direction_bias distribution;

      if parameters.distribution = Support.Random_custom_discrete_text then (
        if parameters.operation <> Rdk.Attribute_ops.Random_set then refuse "text output only supports Set";
        if parameters.scale <> 1. then refuse "text output does not accept scaling";
        if parameters.use_minimum || parameters.use_maximum then refuse "text output does not accept numeric limits";
        if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" then refuse "P cannot use text storage")
      else (
        let dimension = if parameters.distribution = Support.Random_inside_sphere then parameters.dimensions
          else match parameters.kind with Numeric_scalar -> 1 | Numeric_vec2 -> 2 | Numeric_vec3 -> 3 | Numeric_vec4 -> 4 in
        if parameters.owner = Rdk.Attribute.Point && parameters.name = "P" && dimension <> 3 then refuse "P requires three components";
        let kind_dimension = match parameters.kind with Numeric_scalar -> 1 | Numeric_vec2 -> 2 | Numeric_vec3 -> 3 | Numeric_vec4 -> 4 in
        if (parameters.use_minimum || parameters.use_maximum) && kind_dimension <> dimension then refuse "limit dimensions do not match the distribution";
        if parameters.use_minimum && parameters.use_maximum then (
          let limits = if parameters.use_vector_limits then
            [parameters.minimum_vector_x,parameters.maximum_vector_x] @
              (if kind_dimension >= 2 then [parameters.minimum_vector_y,parameters.maximum_vector_y] else []) @
              (if kind_dimension >= 3 then [parameters.minimum_vector_z,parameters.maximum_vector_z] else []) @
              (if kind_dimension = 4 then [parameters.minimum_w,parameters.maximum_w] else [])
            else [parameters.minimum,parameters.maximum] in
          if List.exists (fun (minimum,maximum) -> minimum > maximum) limits then refuse "limits must be ordered"))]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let name = parameters.name in
    let group = parameters.group in
    let selection_owner = parameters.selection_owner in
    let selection_group = parameters.selection_group in
    let kind = parameters.kind in
    let distribution = parameters.distribution in
    let context_seed = parameters.context_seed in
    let seed = parameters.seed in
    let seed_attribute = parameters.seed_attribute in
    let fraction_attribute = parameters.fraction_attribute in
    let a = Rays_math.Vec3.create parameters.a_x parameters.a_y parameters.a_z in
    let a_w = parameters.a_w in
    let b = Rays_math.Vec3.create parameters.b_x parameters.b_y parameters.b_z in
    let b_w = parameters.b_w in
    let step = Rays_math.Vec3.create parameters.step_x parameters.step_y parameters.step_z in
    let step_w = parameters.step_w in
    let probability_b = parameters.probability_b in
    let cone_angle = parameters.cone_angle in
    let dimensions = parameters.dimensions in
    let ramp = parameters.ramp in
    let entries = parameters.entries in
    let text_entries = parameters.text_entries in
    let use_minimum = parameters.use_minimum in
    let minimum = parameters.minimum in
    let use_maximum = parameters.use_maximum in
    let maximum = parameters.maximum in
    let use_vector_limits = parameters.use_vector_limits in
    let minimum_vector = Rays_math.Vec3.create parameters.minimum_vector_x parameters.minimum_vector_y parameters.minimum_vector_z in
    let minimum_w = parameters.minimum_w in
    let maximum_vector = Rays_math.Vec3.create parameters.maximum_vector_x parameters.maximum_vector_y parameters.maximum_vector_z in
    let maximum_w = parameters.maximum_w in
    let direction_bias = parameters.direction_bias in
    let operation = parameters.operation in
    let scale = parameters.scale in
    let distribution = random_distribution_value ~distribution ~kind ~a ~a_w ~b ~b_w ~step ~step_w ~probability_b ~cone_angle ~dimensions ~ramp ~entries ~text_entries in
    let group = optional_text group and selection = optional_element_group selection_owner selection_group in
    let fraction_attribute = optional_text fraction_attribute in
    let seed_attribute = if fraction_attribute <> None then None else optional_text seed_attribute in
    let seed = if fraction_attribute <> None || context_seed then None else Some seed in
    let limit scalar vector w = if use_vector_limits then numeric_value kind vector.Rays_math.Vec3.x vector.y vector.z w
      else numeric_value kind scalar scalar scalar scalar in
    let minimum = if use_minimum then Some (limit minimum minimum_vector minimum_w) else None
    and maximum = if use_maximum then Some (limit maximum maximum_vector maximum_w) else None in
    let dependencies = match fraction_attribute, seed with
      | Some _, _ | None, Some _ -> Context.Dependencies.static
      | None, None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("attribute_randomize:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"attribute_randomize" ~version:2
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0) ~dependencies
      ~inputs:[|input|] (fun ~node_id context inputs ->
        let selected = match selection with
          | Some selection ->
              Result.map (fun selected -> None, selected)
                (resolve_element_group ~operation:"attribute_randomize"
                   (Some selection) inputs.(0))
          | None ->
              Result.map (fun selected -> selected, None)
                (resolve_attribute_group ~operation:"attribute_randomize" ~owner
                   group inputs.(0)) in
        match selected with
        | Error error -> Error error
        | Ok (selection, element_selection) ->
            let element_selection = Option.map (function
              | Rdk.Transform_ops.Selected_points group ->
                  Rdk.Attribute_ops.Random_points group
              | Rdk.Transform_ops.Selected_vertices group ->
                  Rdk.Attribute_ops.Random_vertices group
              | Rdk.Transform_ops.Selected_primitives group ->
                  Rdk.Attribute_ops.Random_primitives group
              | Rdk.Transform_ops.Selected_edges group ->
                  Rdk.Attribute_ops.Random_edges group) element_selection in
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Rays_math.Rand.seed (match fraction_attribute with
              | Some _ -> 0
              | None -> Option.value ~default:(mixed_seed context identity) seed) in
            match Rdk.Attribute_ops.randomize
                ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
                ?selection ?element_selection ?seed_attribute ?fraction_attribute
                ?minimum ?maximum ~seed ~owner ~name ~direction_bias ~operation ~scale
                distribution inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build

end

module Attribute_interpolate = struct
  let driver_parameter = Parameter.choice ~equal:( = ) [
      "Primitive UVW", Interpolate_primitive_uvw; "Point weights", Interpolate_point_weights;
      "Vertex weights", Interpolate_vertex_weights;
      "Primitive weights", Interpolate_primitive_weights;
    ]
  let decode_attribute = function
    | [owner; source; target] -> Result.map (fun interpolate_owner -> {
        Rdk.Attribute_ops.interpolate_owner; interpolate_source = source;
        interpolate_target = target })
        (attribute_owner_of_token
          (String.lowercase_ascii (String.trim owner)))
    | row -> Error (Printf.sprintf
        "Attribute Interpolate rule needs owner, source, and target; got %d columns"
        (List.length row))
  let decode text = Result.bind (decode_table text) (fun rows ->
      List.fold_left (fun result row -> Result.bind result (fun attributes ->
        Result.map (fun attribute -> attribute :: attributes)
          (decode_attribute row))) (Ok []) rows |> Result.map List.rev)
  type parameters = {
    target_owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Target owner"] [@sop.kind attribute_owner_parameter];
    attributes : string
      [@sop.default "point\tCd\tCd"]
      [@sop.label "Attributes (owner, source, target)"];
    group : string [@sop.default ""] [@sop.label "Target group"];
    group_pattern : string [@sop.default ""]
      [@sop.label "Target group pattern"];
    driver : interpolate_driver [@sop.default Interpolate_primitive_uvw] [@sop.label "Driver"]
      [@sop.kind driver_parameter];
    primitive_attribute : string [@sop.default "sourceprim"]
      [@sop.label "Primitive attribute"] [@sop.folder "Driver"];
    uvw_attribute : string [@sop.default "sourceuvw"]
      [@sop.label "UVW attribute"] [@sop.folder "Driver"];
    numbers_attribute : string [@sop.default "sourcenums"]
      [@sop.label "Numbers attribute"] [@sop.folder "Driver"];
    weights_attribute : string [@sop.default "sourceweights"]
      [@sop.label "Weights attribute"] [@sop.folder "Driver"];
    compute_weights : bool [@sop.default false]
      [@sop.label "Compute weight arrays"] [@sop.folder "Output weights"];
    computed_owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Point]
      [@sop.label "Computed owner"] [@sop.folder "Output weights"]
      [@sop.kind uv_owner_parameter];
    computed_numbers_attribute : string [@sop.default "computednums"]
      [@sop.label "Computed numbers"] [@sop.folder "Output weights"];
    computed_weights_attribute : string [@sop.default "computedweights"]
      [@sop.label "Computed weights"] [@sop.folder "Output weights"];
    point_pattern : string [@sop.default ""] [@sop.label "Point pattern"]
      [@sop.folder "Patterns"];
    vertex_pattern : string [@sop.default ""] [@sop.label "Vertex pattern"]
      [@sop.folder "Patterns"];
    primitive_pattern : string [@sop.default ""]
      [@sop.label "Primitive pattern"] [@sop.folder "Patterns"];
    detail_pattern : string [@sop.default ""] [@sop.label "Detail pattern"]
      [@sop.folder "Patterns"];
    match_groups : bool [@sop.default false] [@sop.label "Match groups"]
      [@sop.folder "Patterns"];
    pre_scale : float [@sop.default 1.] [@sop.label "Pre-scale"]
      [@sop.folder "Weights"] [@sop.min (-10.)] [@sop.max 10.];
    normalize_weights : bool [@sop.default true]
      [@sop.label "Normalize weights"] [@sop.folder "Weights"];
    threshold : float [@sop.default 0.] [@sop.label "Threshold"]
      [@sop.folder "Weights"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    blend : float [@sop.default 1.] [@sop.label "Blend"]
      [@sop.folder "Weights"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.] [@sop.hard_max 1.];
    unmatched : Rdk.Attribute_ops.unmatched
      [@sop.default Rdk.Attribute_ops.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
  } [@@sop.node_key "attribute_interpolate"]
    [@@sop.node_label "Attribute Interpolate"]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 2] [@@sop.node_slots "source, target"]

    [@@sop.validate fun parameters ->
      let fail message = invalid_arg ("sop/attribute_interpolate: " ^ message) in
      if parameters.blend < 0. || parameters.blend > 1. then fail "blend must be in [0,1]";
      if parameters.threshold < 0. then fail "threshold must be nonnegative";
      let nonblank name = if String.trim name = "" then fail "empty attribute name" in
      (match decode parameters.attributes with
       | Error message -> fail message
       | Ok attributes -> List.iter (fun attribute ->
           nonblank attribute.Rdk.Attribute_ops.interpolate_source;
           nonblank attribute.interpolate_target;
           if attribute.interpolate_source = "P" && attribute.interpolate_owner <> Rdk.Attribute.Point then
             fail "canonical source P must be point owned";
           if attribute.interpolate_target = "P" && parameters.target_owner <> Rdk.Attribute.Point then
             fail "canonical target P must be point owned";
           let supported = match parameters.driver with
             | Interpolate_primitive_uvw | Interpolate_vertex_weights -> true
             | Interpolate_point_weights -> attribute.interpolate_owner = Rdk.Attribute.Point
                 || attribute.interpolate_owner = Rdk.Attribute.Detail
             | Interpolate_primitive_weights -> attribute.interpolate_owner = Rdk.Attribute.Primitive
                 || attribute.interpolate_owner = Rdk.Attribute.Detail in
           if not supported then fail "source owner is incompatible with the driver") attributes;
           let names = List.map (fun attribute -> attribute.Rdk.Attribute_ops.interpolate_target) attributes in
           if List.length (List.sort_uniq String.compare names) <> List.length names then
             fail "duplicate target attribute");
      let group, pattern = exact_or_pattern parameters.group parameters.group_pattern in
      if parameters.target_owner = Rdk.Attribute.Detail && (group <> None || pattern <> None)
        then fail "detail attributes do not accept a group";
      ignore (compile_transfer_group_pattern "attribute_interpolate" "target" pattern);
      List.iter (fun text -> match optional_text text with
        | None -> ()
        | Some text -> (match Rdk.Attribute_pattern.compile text with
            | Ok _ -> () | Error message -> fail message))
        [parameters.point_pattern; parameters.vertex_pattern;
         parameters.primitive_pattern; parameters.detail_pattern];
      (match parameters.driver with
       | Interpolate_primitive_uvw ->
           nonblank parameters.primitive_attribute; nonblank parameters.uvw_attribute
       | Interpolate_point_weights | Interpolate_vertex_weights | Interpolate_primitive_weights ->
           nonblank parameters.numbers_attribute; nonblank parameters.weights_attribute;
           if parameters.threshold <= 0. then fail "weighted threshold must be positive";
           if parameters.compute_weights then fail "computed weights require primitive/UVW mode");
      if parameters.compute_weights then begin
        if parameters.computed_owner <> Rdk.Attribute.Point
            && parameters.computed_owner <> Rdk.Attribute.Vertex then
          fail "computed weights require point or vertex ownership";
        nonblank parameters.computed_numbers_attribute;
        nonblank parameters.computed_weights_attribute;
        if parameters.computed_numbers_attribute = parameters.computed_weights_attribute then
          fail "computed number and weight attributes must have different names";
        if parameters.target_owner = Rdk.Attribute.Point
            && (parameters.computed_numbers_attribute = "P" || parameters.computed_weights_attribute = "P")
          then fail "canonical P cannot store computed arrays"
      end]
    [@@deriving sop_params, sop_node]
  let driver parameters = match parameters.driver with
    | Interpolate_primitive_uvw -> Rdk.Attribute_ops.Primitive_uvw {
        primitive_attribute = parameters.primitive_attribute;
        uvw_attribute = parameters.uvw_attribute }
    | Interpolate_point_weights -> Rdk.Attribute_ops.Point_weights {
        numbers_attribute = parameters.numbers_attribute;
        weights_attribute = parameters.weights_attribute }
    | Interpolate_vertex_weights -> Rdk.Attribute_ops.Vertex_weights {
        numbers_attribute = parameters.numbers_attribute;
        weights_attribute = parameters.weights_attribute }
    | Interpolate_primitive_weights -> Rdk.Attribute_ops.Primitive_weights {
        numbers_attribute = parameters.numbers_attribute;
        weights_attribute = parameters.weights_attribute }
  let build = parameters_build (fun ~label parameters source target ->
    let group, group_pattern = exact_or_pattern parameters.group parameters.group_pattern in
    let group_pattern = compile_transfer_group_pattern "attribute_interpolate" "target" group_pattern in
    let attributes = match decode parameters.attributes with
      | Ok attributes -> attributes | Error message -> invalid_arg message in
    let driver = driver parameters in
    let compute_weights = if parameters.compute_weights then Some {
      Rdk.Attribute_ops.computed_owner = parameters.computed_owner;
      computed_numbers_attribute = parameters.computed_numbers_attribute;
      computed_weights_attribute = parameters.computed_weights_attribute }
      else None in
    Node.Private.make_geometry ~label ~operation:"attribute_interpolate" ~version:3
      ~parameters:"" ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; target|] (fun ~node_id:_ context inputs ->
        let cancel = Context.cancel_token context and grain = Context.grain context in
        let owner, owner_name = match parameters.target_owner with
          | Rdk.Attribute.Point -> Rdk.Group.Point, "point"
          | Rdk.Attribute.Vertex -> Rdk.Group.Vertex, "vertex"
          | Rdk.Attribute.Primitive -> Rdk.Group.Primitive, "primitive"
          | Rdk.Attribute.Detail -> Rdk.Group.Point, "detail" in
        let selection = resolve_transfer_group ~operation:"attribute_interpolate"
          ~owner ~owner_name ~exact:group ~pattern:group_pattern ~cancel ~grain inputs.(1) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Attribute_ops.interpolate ~cancel ~grain ?selection ~driver ?compute_weights
              ?point_pattern:(optional_text parameters.point_pattern)
              ?vertex_pattern:(optional_text parameters.vertex_pattern)
              ?primitive_pattern:(optional_text parameters.primitive_pattern)
              ?detail_pattern:(optional_text parameters.detail_pattern)
              ~match_groups:parameters.match_groups ~pre_scale:parameters.pre_scale
              ~normalize_weights:parameters.normalize_weights ~threshold:parameters.threshold
              ~blend:parameters.blend ~unmatched:parameters.unmatched
              ~target_owner:parameters.target_owner ~attributes ~source:inputs.(0) ~target:inputs.(1) () with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Attribute_transfer = struct
  let vertex_selection_parameter = Parameter.choice ~equal:( = ) [
      "All triangle vertices", Rdk.Attribute_ops.All_triangle_vertices;
      "Any triangle vertex", Rdk.Attribute_ops.Any_triangle_vertex;
    ]
  let decode_names text = parameter_table_rows "sop/attribute_transfer" text
    |> List.map (function [name] -> name | _ ->
      invalid_arg "sop/attribute_transfer: exact names need one column per row")
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
    unmatched : Rdk.Attribute_ops.unmatched
      [@sop.default Rdk.Attribute_ops.Keep_target] [@sop.label "Unmatched"]
      [@sop.kind transfer_unmatched_parameter];
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Groups/Source"];
    source_group_pattern : string [@sop.default ""]
      [@sop.label "Source group pattern"] [@sop.folder "Groups/Source"];
    source_vertex_group : string [@sop.default ""]
      [@sop.label "Source vertex group"] [@sop.folder "Groups/Source"];
    source_vertex_group_pattern : string [@sop.default ""]
      [@sop.label "Source vertex pattern"] [@sop.folder "Groups/Source"];
    source_vertex_selection : Rdk.Attribute_ops.surface_vertex_selection
      [@sop.default Rdk.Attribute_ops.All_triangle_vertices]
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
      let refuse message = invalid_arg ("sop/attribute_transfer: " ^ message) in
      if parameters.use_names then begin
        let names = decode_names parameters.names in
        List.iter (fun name ->
          if String.trim name = "" then refuse "empty attribute name";
          if parameters.owner = Rdk.Attribute.Point && name = "P" then
            refuse "canonical P cannot be transferred as an ordinary attribute") names;
        if List.length (List.sort_uniq String.compare names) <> List.length names then
          refuse "duplicate attribute name"
      end else validate_parameter_pattern "sop/attribute_transfer" parameters.pattern;
      if parameters.neighbors < 1 then refuse "neighbors must be positive";
      if parameters.power < 0. || (parameters.mode = Transfer_inverse && parameters.power = 0.) then
        refuse "invalid inverse power";
      if parameters.kernel_radius < 0. || parameters.kernel_radius > sqrt Float.max_float
          || parameters.max_distance < 0. || parameters.max_distance > sqrt Float.max_float
          || parameters.blend_width < 0. then refuse "distances must be nonnegative and safely squarable";
      if parameters.uniform_bias < 0. || parameters.uniform_bias > 1. then refuse "uniform bias must be within [0,1]";
      if parameters.distance_mode = Kernel_auto && parameters.blend_width > 0. then
        refuse "blend width requires an explicit maximum distance";
      if parameters.distance_mode = Kernel_explicit &&
          parameters.max_distance +. parameters.blend_width > sqrt Float.max_float then
        refuse "distance plus blend width must be safely squarable";
      let source_group,source_pattern=exact_or_pattern parameters.source_group parameters.source_group_pattern
      and vertex_group,vertex_pattern=exact_or_pattern parameters.source_vertex_group parameters.source_vertex_group_pattern
      and target_group,target_pattern=exact_or_pattern parameters.target_group parameters.target_group_pattern in
      List.iter (fun (name,pattern) -> ignore (compile_transfer_group_pattern "attribute_transfer" name pattern))
        ["source",source_pattern;"source vertex",vertex_pattern;"target",target_pattern];
      if parameters.owner <> Rdk.Attribute.Vertex && (vertex_group <> None || vertex_pattern <> None) then
        refuse "source vertex groups require vertex ownership";
      if parameters.owner = Rdk.Attribute.Detail &&
          (source_group <> None || source_pattern <> None || target_group <> None || target_pattern <> None
           || parameters.distance_mode <> Kernel_auto || parameters.blend_width <> 0.) then
        refuse "detail transfer does not accept spatial options";
      if (parameters.owner = Rdk.Attribute.Detail || parameters.owner = Rdk.Attribute.Vertex)
          && parameters.mode <> Transfer_nearest then refuse "this owner supports nearest mode only"]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters source target ->
    let owner=parameters.owner and source_vertex_selection=parameters.source_vertex_selection in
    let names,pattern = if parameters.use_names then Some (decode_names parameters.names),None
      else None,Some parameters.pattern in
    let mode=transfer_mode parameters.mode parameters.neighbors parameters.power parameters.kernel_radius
    and falloff=transfer_falloff parameters.falloff parameters.uniform_bias
    and unmatched=parameters.unmatched and blend_width=parameters.blend_width in
    let max_distance=match parameters.distance_mode with Kernel_auto -> None | Kernel_explicit -> Some parameters.max_distance in
    let source_group,source_group_pattern=exact_or_pattern parameters.source_group parameters.source_group_pattern
    and source_vertex_group,source_vertex_group_pattern=exact_or_pattern parameters.source_vertex_group parameters.source_vertex_group_pattern
    and target_group,target_group_pattern=exact_or_pattern parameters.target_group parameters.target_group_pattern in
    let source_group_pattern_compiled=compile_transfer_group_pattern "attribute_transfer" "source" source_group_pattern
    and source_vertex_group_pattern_compiled=compile_transfer_group_pattern "attribute_transfer" "source vertex" source_vertex_group_pattern
    and target_group_pattern_compiled=compile_transfer_group_pattern "attribute_transfer" "target" target_group_pattern in
    Node.Private.make_geometry ~label ~operation:"attribute_transfer" ~version:6
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
                       ~cancel ~grain ?names ?pattern ~mode ?max_distance
                       ~blend_width ~falloff
                       ~unmatched ?source_points:source_elements
                       ?target_points:target_elements ~source:inputs.(0)
                       ~target:inputs.(1) ()
                 | Rdk.Attribute.Vertex ->
                     Rdk.Attribute_ops.transfer_vertices
                       ~cancel ~grain ?names ?pattern ?max_distance
                       ~blend_width ~falloff
                       ~unmatched ?source_primitives:source_elements
                       ?source_vertices ~source_vertex_selection
                       ?target_vertices:target_elements ~source:inputs.(0)
                       ~target:inputs.(1) ()
                 | Rdk.Attribute.Primitive ->
                     Rdk.Attribute_ops.transfer_primitives
                       ~cancel ~grain ?names ?pattern ~mode ?max_distance
                       ~blend_width ~falloff
                       ~unmatched ?source_primitives:source_elements
                       ?target_primitives:target_elements ~source:inputs.(0)
                       ~target:inputs.(1) ()
                 | Rdk.Attribute.Detail ->
                     Rdk.Attribute_ops.transfer_detail ?names ?pattern
                       ~source:inputs.(0) ~target:inputs.(1) () in
               match result with
               | Ok geometry -> cooked geometry
               | Error error -> structured_rdk_error error)))
  )
  let factory = parameters_factory build
end

module Attribute_composite = struct
  let operation_parameter = Parameter.choice ~equal:( = ) [
      "Mean", Rdk.Attribute_composite.Composite_mean;
      "Maximum", Rdk.Attribute_composite.Composite_maximum;
      "Minimum", Rdk.Attribute_composite.Composite_minimum;
      "Over", Rdk.Attribute_composite.Composite_over;
      "Under", Rdk.Attribute_composite.Composite_under;
    ]
  let decode_weights = decode_parameter_weights "sop/attribute_composite"
  type parameters = {
    operation : Rdk.Attribute_composite.operation
      [@sop.default Rdk.Attribute_composite.Composite_mean]
      [@sop.label "Operation"] [@sop.kind operation_parameter];
    weight : float [@sop.default 1.] [@sop.label "Weight"]
      [@sop.min 0.] [@sop.max 1.];
    point_attributes : string [@sop.default "*"]
      [@sop.label "Point attributes"];
    allow_position : bool [@sop.default false] [@sop.label "Allow P"];
    alpha_attribute : string [@sop.default ""] [@sop.label "Alpha attribute"];
    vertex_attributes : string [@sop.default "*"]
      [@sop.label "Vertex attributes"] [@sop.folder "Attributes"];
    primitive_attributes : string [@sop.default "*"]
      [@sop.label "Primitive attributes"] [@sop.folder "Attributes"];
    detail_attributes : string [@sop.default "*"]
      [@sop.label "Detail attributes"] [@sop.folder "Attributes"];
    weight1 : float [@sop.default 1.] [@sop.label "Weight 1"]
      [@sop.folder "Layers"] [@sop.min 0.] [@sop.max 1.];
    weight2 : float [@sop.default 1.] [@sop.label "Weight 2"]
      [@sop.folder "Layers"] [@sop.min 0.] [@sop.max 1.];
    weight3 : float [@sop.default 1.] [@sop.label "Weight 3"]
      [@sop.folder "Layers"] [@sop.min 0.] [@sop.max 1.];
    weight4 : float [@sop.default 1.] [@sop.label "Weight 4"]
      [@sop.folder "Layers"] [@sop.min 0.] [@sop.max 1.];
    weights : string [@sop.default ""] [@sop.label "Additional layer weights"]
      [@sop.folder "Layers"];
  } [@@sop.node_key "attribute_composite"]
    [@@sop.node_label "Attribute Composite"]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 6]
    [@@sop.node_slots "input, layer1, layer2, layer3, layer4, layers"]
    [@@sop.node_optional "1,2,3,4,5"] [@@sop.node_rest 5]

    [@@sop.validate fun parameters ->
      ignore (decode_weights parameters.weights);
      List.iter (validate_parameter_pattern "sop/attribute_composite")
        [parameters.point_attributes; parameters.vertex_attributes;
         parameters.primitive_attributes; parameters.detail_attributes]]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input layer1 layer2 layer3 layer4 layers ->
    let extra_weights = Array.of_list (decode_weights parameters.weights) in
    let weighted = List.filter_map (fun (weight, layer) ->
      Option.map (fun node -> weight, node) layer)
        [parameters.weight1, layer1; parameters.weight2, layer2;
         parameters.weight3, layer3; parameters.weight4, layer4]
      @ List.mapi (fun index node ->
          (if index < Array.length extra_weights then extra_weights.(index) else 1.), node) layers in
    let weights = Array.of_list (List.map fst weighted)
    and nodes = Array.of_list (input :: List.map snd weighted) in
    let alpha_attribute = optional_text parameters.alpha_attribute in
    Node.Private.make_geometry ~label ~operation:"attribute_composite" ~version:1
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:nodes
      (fun ~node_id:_ context geometries ->
        let inputs = Array.to_list (Array.mapi (fun index weight ->
          Rdk.Attribute_composite.input ~weight geometries.(index + 1)) weights) in
        match Rdk.Attribute_composite.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~operation:parameters.operation ~weight:parameters.weight
            ~detail_attributes:parameters.detail_attributes
            ~primitive_attributes:parameters.primitive_attributes
            ~point_attributes:parameters.point_attributes ~vertex_attributes:parameters.vertex_attributes
            ~allow_position:parameters.allow_position ?alpha_attribute ~inputs geometries.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end

module Blend_shapes = struct
  let mode_parameter = Parameter.choice ~equal:( = ) [
      "Normalized", Rdk.Blend_shapes.Blend_normalized;
      "Differencing", Rdk.Blend_shapes.Blend_differencing;
    ]
  let masking_parameter = Parameter.choice ~equal:( = ) [
      "None", Rdk.Blend_shapes.Blend_no_mask;
      "Set from attribute", Rdk.Blend_shapes.Blend_set_from_attribute;
      "Scale from attribute", Rdk.Blend_shapes.Blend_scale_from_attribute;
    ]
  let mask_source_parameter = Parameter.choice ~equal:( = ) [
      "First input", Rdk.Blend_shapes.Blend_mask_first_input;
      "Shape", Rdk.Blend_shapes.Blend_mask_shape;
    ]
  let decode_weights = decode_parameter_weights "sop/blend_shapes"
  let decode_shape_masks text =
    let seen = Hashtbl.create 8 in
    parameter_table_rows "sop/blend_shapes" text |> List.map (function
      | [index; name; source] ->
          let index = match int_of_string_opt (String.trim index) with
            | Some index when index >= 1 -> index
            | _ -> invalid_arg "sop/blend_shapes: shape mask index must be positive" in
          if Hashtbl.mem seen index then invalid_arg "sop/blend_shapes: duplicate shape mask index";
          Hashtbl.add seen index ();
          let source = match String.trim source with
            | "" -> None
            | "first" -> Some Rdk.Blend_shapes.Blend_mask_first_input
            | "shape" -> Some Rdk.Blend_shapes.Blend_mask_shape
            | _ -> invalid_arg "sop/blend_shapes: mask source must be first or shape" in
          index, (optional_text name, source)
      | _ -> invalid_arg "sop/blend_shapes: shape masks need index, name and source columns")
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    mode : Rdk.Blend_shapes.mode [@sop.default Rdk.Blend_shapes.Blend_normalized]
      [@sop.label "Mode"] [@sop.kind mode_parameter];
    attributes : string [@sop.default "*"] [@sop.label "Attributes"];
    weight1 : float [@sop.default 1.] [@sop.label "Weight 1"]
      [@sop.folder "Weights"] [@sop.min (-1.)] [@sop.max 2.];
    weight2 : float [@sop.default 0.] [@sop.label "Weight 2"]
      [@sop.folder "Weights"] [@sop.min (-1.)] [@sop.max 2.];
    weight3 : float [@sop.default 0.] [@sop.label "Weight 3"]
      [@sop.folder "Weights"] [@sop.min (-1.)] [@sop.max 2.];
    weight4 : float [@sop.default 0.] [@sop.label "Weight 4"]
      [@sop.folder "Weights"] [@sop.min (-1.)] [@sop.max 2.];
    masking : Rdk.Blend_shapes.masking
      [@sop.default Rdk.Blend_shapes.Blend_no_mask] [@sop.label "Masking"]
      [@sop.folder "Mask"] [@sop.kind masking_parameter];
    mask_attribute : string [@sop.default "mask"] [@sop.label "Mask attribute"]
      [@sop.folder "Mask"];
    mask_source : Rdk.Blend_shapes.mask_source
      [@sop.default Rdk.Blend_shapes.Blend_mask_first_input]
      [@sop.label "Mask source"] [@sop.folder "Mask"]
      [@sop.kind mask_source_parameter];
    point_id_attribute : string [@sop.default ""]
      [@sop.label "Point ID attribute"] [@sop.folder "Matching"];
    weights : string [@sop.default ""] [@sop.label "Additional shape weights"]
      [@sop.folder "Weights"];
    shape_masks : string [@sop.default ""] [@sop.label "Shape mask overrides"]
      [@sop.folder "Mask"];
  } [@@sop.node_key "blend_shapes"] [@@sop.node_label "Blend Shapes"]
    [@@sop.node_category "Attribute/Transfer"] [@@sop.node_inputs 6]
    [@@sop.node_slots "input, shape1, shape2, shape3, shape4, shapes"]
    [@@sop.node_optional "1,2,3,4,5"] [@@sop.node_rest 5]

    [@@sop.validate fun parameters ->
      ignore (decode_weights parameters.weights);
      ignore (decode_shape_masks parameters.shape_masks);
      validate_parameter_pattern "sop/blend_shapes" parameters.attributes]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input shape1 shape2 shape3 shape4 shapes ->
    let extra_weights = Array.of_list (decode_weights parameters.weights)
    and overrides = decode_shape_masks parameters.shape_masks in
    let connected = List.filter_map (fun (index, weight, shape) ->
      Option.map (fun node -> index, weight, node) shape)
        [1,parameters.weight1,shape1;2,parameters.weight2,shape2;
         3,parameters.weight3,shape3;4,parameters.weight4,shape4]
      @ List.mapi (fun index node -> index+5,
          (if index < Array.length extra_weights then extra_weights.(index) else 0.), node) shapes in
    let settings = Array.of_list (List.map (fun (index,weight,_) ->
      let mask,source = Option.value ~default:(None,None) (List.assoc_opt index overrides) in
      weight,mask,Option.value ~default:parameters.mask_source source) connected) in
    let nodes = Array.of_list (input :: List.map (fun (_,_,node) -> node) connected) in
    let point_group = optional_text parameters.group
    and point_id_attribute = optional_text parameters.point_id_attribute in
    let mask_attribute = if parameters.masking = Rdk.Blend_shapes.Blend_no_mask then None
      else optional_text parameters.mask_attribute in
    Node.Private.make_geometry ~label ~operation:"blend_shapes" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static ~inputs:nodes
      (fun ~node_id:_ context inputs ->
        if Array.length settings = 0 then cooked inputs.(0)
        else match resolve_optional_point_group "blend_shapes" point_group inputs.(0) with
        | Error error -> Error error
        | Ok points ->
            let shapes = Array.to_list (Array.mapi (fun index (weight,mask_attribute,mask_source) ->
              Rdk.Blend_shapes.shape ~weight ?mask_attribute ~mask_source inputs.(index+1)) settings) in
            match Rdk.Blend_shapes.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~mode:parameters.mode ~masking:parameters.masking
                ?mask_attribute ?point_id_attribute ~attributes:parameters.attributes ~shapes inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error))
  let factory = parameters_factory build
end
