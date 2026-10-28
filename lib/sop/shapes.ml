(* Shape nodes, declared once: the record is the editor schema and the factory. *)

open Rays_math
open Support

(* ocamldep must see the PPX's [Sop.X] resolve inside this library. *)
module Sop = Support.Sop










module Null = struct
  type parameters = unit
    [@@sop.node_key "null"] [@@sop.node_label "Null"]
    [@@sop.node_category "Utility"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label _parameters input ->
    let label = Some label in
    Node.Private.make_geometry ?label ~operation:"null" ~version:1 ~parameters:""
      ~cook_mode:(Node.Passthrough 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs -> cooked inputs.(0))
  )
  let factory = parameters_factory build
end

module Switch = struct
  type parameters = {
    input : int [@sop.default 0] [@sop.label "Source"]
      [@sop.description "Input branch displayed and cooked by this Switch SOP"]
      [@sop.min 0] [@sop.max 9999] [@sop.hard_min 0];
  } [@@sop.node_key "switch"] [@@sop.node_label "Switch"]
    [@@sop.node_category "Utility"] [@@sop.node_inputs 3]
    [@@sop.node_slots "a, b, inputs"] [@@sop.node_optional "2"]
    [@@sop.node_rest 2]

    [@@deriving sop_params, sop_node]

  (* the choice lists the connected branches by index and label *)
  let schema _parameters node =
    let labels = List.mapi (fun index input ->
      Printf.sprintf "%d · %s" index (Node.label input)) (Node.inputs node) in
    Param.schema ~name:"parameters" ~default:parameters_default
      (List.map (fun (Param.Field field as original) ->
         match field.kind with
         | Param.Integer _ -> Param.Field { field with
             kind = Param.index_choice labels }
         | _ -> original) (Param.fields parameters_schema))

  let build = parameters_build ~schema (fun ~label parameters a b inputs ->
    let inputs = Array.of_list (a :: b :: inputs) and index = parameters.input in
    if index < 0 || index >= Array.length inputs then
      invalid_arg "sop/switch: input is outside the connected branches";
    Node.Private.make_geometry ~label ~operation:"switch" ~version:1 ~parameters:""
      ~cook_mode:(Node.Passthrough index)
      ~dependencies:Context.Dependencies.static ~input_policy:(Node.Private.Only index)
      ~inputs (fun ~node_id:_ _context selected -> cooked selected.(0)))

  let factory = parameters_factory build
end




module Clip = struct
  let keep_parameter = Param.choice ~equal:( = ) [
      "Above", Rdk.Plane_clip.Above;
      "Below", Rdk.Plane_clip.Below;
      "All", Rdk.Plane_clip.All;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"] [@sop.folder "Selection"];
    keep : Rdk.Plane_clip.keep [@sop.default Rdk.Plane_clip.Above]
      [@sop.label "Keep"] [@sop.kind keep_parameter];
    snapping_tolerance : float [@sop.default 1e-9]
      [@sop.label "Snapping tolerance"] [@sop.folder "Robustness"]
      [@sop.min 0.] [@sop.max 0.001] [@sop.hard_min 0.];
    fill : bool [@sop.default false] [@sop.label "Fill cut"]
      [@sop.folder "Topology"];
    split_connectivity : bool [@sop.default false]
      [@sop.label "Split connectivity"] [@sop.folder "Topology"];
    distance : float [@sop.default 0.] [@sop.label "Distance"]
      [@sop.folder "Plane"] [@sop.min (-10.)] [@sop.max 10.];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Plane/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    normal_x : float [@sop.default 0.] [@sop.label "Normal X"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    normal_y : float [@sop.default 1.] [@sop.label "Normal Y"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    normal_z : float [@sop.default 0.] [@sop.label "Normal Z"]
      [@sop.folder "Plane/Normal"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "normal"]
    clip_attribute : string [@sop.default "P"]
      [@sop.label "Clip attribute"] [@sop.folder "Attributes"];
    clipped_edge_group : string [@sop.default ""]
      [@sop.label "Clipped edges"] [@sop.folder "Output groups"];
    cap_group : string [@sop.default ""] [@sop.label "Caps"]
      [@sop.folder "Output groups"];
    clipped_group : string [@sop.default ""] [@sop.label "Clipped"]
      [@sop.folder "Output groups"];
    above_group : string [@sop.default ""] [@sop.label "Above"]
      [@sop.folder "Output groups"];
    below_group : string [@sop.default ""] [@sop.label "Below"]
      [@sop.folder "Output groups"];
    replace_existing_groups : bool [@sop.default false]
      [@sop.label "Replace existing groups"] [@sop.folder "Output groups"];
  } [@@sop.node_key "clip"] [@@sop.node_label "Clip"]
    [@@sop.validate fun parameters ->
      if not (List.for_all Float.is_finite [parameters.origin_x; parameters.origin_y; parameters.origin_z;
          parameters.normal_x; parameters.normal_y; parameters.normal_z; parameters.distance;
          parameters.snapping_tolerance]) then invalid_arg "sop/clip: plane and tolerance must be finite";
      if parameters.snapping_tolerance < 0. then invalid_arg "sop/clip: snapping tolerance must be non-negative";
      if parameters.split_connectivity && parameters.keep <> Rdk.Plane_clip.All then
        invalid_arg "sop/clip: split connectivity requires keep=All";
      if parameters.normal_x = 0. && parameters.normal_y = 0. && parameters.normal_z = 0. then
        invalid_arg "sop/clip: normal must be non-zero";
      let names = [parameters.cap_group; parameters.clipped_group;
        parameters.above_group; parameters.below_group] |> List.filter_map optional_text in
      if List.length (List.sort_uniq String.compare names) <> List.length names then
        invalid_arg "sop/clip: output primitive group names must be distinct"]
    [@@sop.node_category "Modify"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let keep = parameters.keep in
    let snapping_tolerance = parameters.snapping_tolerance in
    let fill = parameters.fill in
    let split_connectivity = parameters.split_connectivity in
    let clip_attribute = parameters.clip_attribute in
    let distance = parameters.distance in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let replace_existing_groups = parameters.replace_existing_groups in
    let clipped_edge_group = optional_text parameters.clipped_edge_group in
    let cap_group = optional_text parameters.cap_group in
    let clipped_group = optional_text parameters.clipped_group in
    let above_group = optional_text parameters.above_group in
    let below_group = optional_text parameters.below_group in
    let origin = Rays_math.Vec3.create parameters.origin_x parameters.origin_y
          parameters.origin_z in
    let normal = Rays_math.Vec3.create parameters.normal_x parameters.normal_y
          parameters.normal_z in
    let selection = optional_element_group group_owner group in
    let clip_attribute = Option.value ~default:"P" (optional_text clip_attribute) in
    let origin = vec3_copy origin and normal = vec3_copy normal in
    Node.Private.make_geometry ?label ~operation:"clip" ~version:5
      ~parameters:"" ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"clip" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection -> match Rdk.Plane_clip.clip ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~keep ~snapping_tolerance ~fill
            ~split_connectivity ~clip_attribute ~distance ?selection
            ~replace_existing_groups
            ?clipped_edge_group
            ?cap_group ?clipped_group ?above_group ?below_group ~origin ~normal
            inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end








module Exploded_view = struct
  type parameters = {
    amount : float [@sop.default 0.32] [@sop.label "Uniform scale"]
      [@sop.folder "Explosion"] [@sop.min (-0.95)] [@sop.max 8.]
      [@sop.impact "view"];
    scale_x : float [@sop.default 1.] [@sop.label "Scale X"]
      [@sop.folder "Explosion/Scale"] [@sop.min (-2.)] [@sop.max 2.]
      [@sop.impact "view"]; [@sop.vec3 "scale"]
    scale_y : float [@sop.default 1.] [@sop.label "Scale Y"]
      [@sop.folder "Explosion/Scale"] [@sop.min (-2.)] [@sop.max 2.]
      [@sop.impact "view"]; [@sop.vec3 "scale"]
    scale_z : float [@sop.default 1.] [@sop.label "Scale Z"]
      [@sop.folder "Explosion/Scale"] [@sop.min (-2.)] [@sop.max 2.]
      [@sop.impact "view"]; [@sop.vec3 "scale"]
    piece_attribute : string [@sop.default "piece"]
      [@sop.label "Piece attribute"] [@sop.folder "Pieces"];
    noise_amount : float [@sop.default 0.] [@sop.label "Noise amount"]
      [@sop.folder "Noise"] [@sop.min 0.] [@sop.max 1.]
      [@sop.impact "view"];
    noise_frequency : float [@sop.default 0.8]
      [@sop.label "Noise frequency"] [@sop.folder "Noise"]
      [@sop.min 0.02] [@sop.max 4.] [@sop.hard_min 0.]
      [@sop.impact "view"];
    noise_seed : int [@sop.default 0] [@sop.label "Noise seed"]
      [@sop.folder "Noise"] [@sop.min 0] [@sop.max 9999]
      [@sop.impact "view"];
  } [@@sop.node_key "exploded_view"] [@@sop.node_label "Exploded View"]
    [@@sop.validate fun parameters ->
      if not (List.for_all Float.is_finite [parameters.amount; parameters.scale_x;
          parameters.scale_y; parameters.scale_z; parameters.noise_amount; parameters.noise_frequency])
          || parameters.noise_frequency < 0. then
        invalid_arg "sop/exploded_view: controls must be finite and noise frequency nonnegative";
      if String.trim parameters.piece_attribute = "" then
        invalid_arg "sop/exploded_view: piece attribute must be nonblank"]
    [@@sop.node_category "Visualize"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label _parameters input ->
    let label = Some label in
    Node.Private.make_geometry ?label ~operation:"exploded_view" ~version:1 ~parameters:""
      ~cook_mode:(Node.Passthrough 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ _context inputs -> cooked inputs.(0))
  )

  let factory = parameters_factory build

end


(* The packed result a Lisp body cannot say on its own: the source unchanged, drawn at the affine rows
   r0, r1, r2 (Point Float4 attributes of the second input, one point per instance; row 3 is 0 0 0 1). *)
module Pack_instances = struct
  type parameters = unit
    [@@sop.node_key "pack_instances"] [@@sop.node_label "Pack Instances"]
    [@@sop.node_category "Copy"] [@@sop.node_inputs 2] [@@sop.node_slots "source, instances"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label _parameters source instances ->
    Node.Private.make_geometry ~label ~operation:"pack_instances" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|source; instances|]
      (fun ~node_id:_ _context inputs ->
        let row name = match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point name inputs.(1) with
          | Some attribute -> (match Rdk.Attribute.View.storage attribute with
              | Rdk.Attribute.Float4 values -> Ok (Rdk.Packed.Float4.View.view values)
              | _ -> Error (Diagnostic.error ~code:"invalid_attribute"
                  (Printf.sprintf "pack_instances: %s must be point float4" name)))
          | None -> Error (Diagnostic.error ~code:"invalid_attribute"
              (Printf.sprintf "pack_instances: missing point attribute %s" name)) in
        Result.bind (row "r0") (fun r0 -> Result.bind (row "r1") (fun r1 -> Result.bind (row "r2") (fun r2 ->
          let transforms = Array.init (Array.length r0.x) (fun i ->
            Rays_math.Mat4.of_rows (r0.x.(i), r0.y.(i), r0.z.(i), r0.w.(i))
              (r1.x.(i), r1.y.(i), r1.z.(i), r1.w.(i)) (r2.x.(i), r2.y.(i), r2.z.(i), r2.w.(i))
              (0., 0., 0., 1.)) in
          Ok Node.Private.{ geometry = inputs.(0); instances = Some transforms })))))

  let factory = parameters_factory build

end










module Soft_transform = struct
  let order_parameter = Param.choice ~equal:( = ) [
      "SRT", Rdk.Transform_ops.Transform_srt; "STR", Rdk.Transform_ops.Transform_str;
      "RST", Rdk.Transform_ops.Transform_rst; "RTS", Rdk.Transform_ops.Transform_rts;
      "TSR", Rdk.Transform_ops.Transform_tsr; "TRS", Rdk.Transform_ops.Transform_trs;
    ]
  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Transform_ops.Transform_xyz; "XZY", Rdk.Transform_ops.Transform_xzy;
      "YXZ", Rdk.Transform_ops.Transform_yxz; "YZX", Rdk.Transform_ops.Transform_yzx;
      "ZXY", Rdk.Transform_ops.Transform_zxy; "ZYX", Rdk.Transform_ops.Transform_zyx;
    ]
  let metric_parameter = Param.choice ~equal:( = ) [
      "Radius", Soft_radius; "Edge distance", Soft_edge; "Attribute", Soft_attribute;
    ]
  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"]
      [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"]
      [@sop.folder "Selection"];
    order : Rdk.Transform_ops.transform_order [@sop.default Rdk.Transform_ops.Transform_srt]
      [@sop.label "Transform order"] [@sop.folder "Transform"]
      [@sop.kind order_parameter];
    rotation_order : Rdk.Transform_ops.transform_rotation_order
      [@sop.default Rdk.Transform_ops.Transform_xyz] [@sop.label "Rotation order"]
      [@sop.folder "Transform/Rotate"] [@sop.kind rotation_order_parameter];
    translate_x : float [@sop.default 0.] [@sop.label "Translate X"]
      [@sop.folder "Transform/Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    translate_y : float [@sop.default 0.] [@sop.label "Translate Y"]
      [@sop.folder "Transform/Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    translate_z : float [@sop.default 0.] [@sop.label "Translate Z"]
      [@sop.folder "Transform/Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    rotate_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "rotate"]
    rotate_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "rotate"]
    rotate_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "rotate"]
    scale_x : float [@sop.default 1.] [@sop.label "Scale X"]
      [@sop.folder "Transform/Scale"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "scale"]
    scale_y : float [@sop.default 1.] [@sop.label "Scale Y"]
      [@sop.folder "Transform/Scale"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "scale"]
    scale_z : float [@sop.default 1.] [@sop.label "Scale Z"]
      [@sop.folder "Transform/Scale"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "scale"]
    shear_xy : float [@sop.default 0.] [@sop.label "Shear XY"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
    shear_xz : float [@sop.default 0.] [@sop.label "Shear XZ"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
    shear_yz : float [@sop.default 0.] [@sop.label "Shear YZ"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.];
    pivot_x : float [@sop.default 0.] [@sop.label "Pivot X"]
      [@sop.folder "Transform/Pivot"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "pivot"]
    pivot_y : float [@sop.default 0.] [@sop.label "Pivot Y"]
      [@sop.folder "Transform/Pivot"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "pivot"]
    pivot_z : float [@sop.default 0.] [@sop.label "Pivot Z"]
      [@sop.folder "Transform/Pivot"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "pivot"]
    pivot_rotation_x : float [@sop.default 0.] [@sop.label "Pivot rotate X"]
      [@sop.folder "Transform/Pivot rotation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "pivot_rotation"]
    pivot_rotation_y : float [@sop.default 0.] [@sop.label "Pivot rotate Y"]
      [@sop.folder "Transform/Pivot rotation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "pivot_rotation"]
    pivot_rotation_z : float [@sop.default 0.] [@sop.label "Pivot rotate Z"]
      [@sop.folder "Transform/Pivot rotation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "pivot_rotation"]
    invert : bool [@sop.default false] [@sop.label "Invert transform"]
      [@sop.folder "Transform"];
    metric : Support.soft_transform_metric [@sop.default Soft_radius] [@sop.label "Distance metric"]
      [@sop.folder "Soft selection"] [@sop.kind metric_parameter];
    metric_attribute : string [@sop.default "mask"]
      [@sop.label "Metric attribute"] [@sop.folder "Soft selection"];
    apply_rolloff : bool [@sop.default true] [@sop.label "Apply rolloff"]
      [@sop.folder "Soft selection"];
    falloff : Rdk.Transform_ops.soft_transform_falloff
      [@sop.default Rdk.Transform_ops.Soft_cubic] [@sop.label "Falloff"]
      [@sop.folder "Soft selection"] [@sop.kind soft_falloff_parameter];
    radius : float [@sop.default 1.] [@sop.label "Radius"]
      [@sop.folder "Soft selection"] [@sop.min 0.] [@sop.max 100.]
      [@sop.hard_min 0.];
    falloff_attribute : string [@sop.default ""]
      [@sop.label "Falloff output attribute"] [@sop.folder "Output"];
    recompute_normals : bool [@sop.default true]
      [@sop.label "Recompute normals"] [@sop.folder "Output"];
  } [@@sop.node_key "soft_transform"] [@@sop.node_label "Soft Transform"]
    [@@sop.node_facts {elementwise = Node.Points;
      reads = List.filter (fun name -> String.trim name <> "") ["P"; "N"; parameters.metric_attribute];
      writes = List.filter (fun name -> String.trim name <> "") ["P"; "N"; parameters.falloff_attribute];
      topology = Node.Preserved; exact = false}]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/soft_transform_trs: " ^ message) in
      if not (Float.is_finite parameters.radius) || parameters.radius < 0. then refuse "radius must be finite and nonnegative";
      if parameters.radius = 0. && (parameters.metric <> Soft_attribute || parameters.apply_rolloff) then
        refuse "rolloff radius must be positive";
      if parameters.metric = Soft_attribute && String.trim parameters.metric_attribute = "" then
        refuse "metric attribute must be nonblank";
      if parameters.metric = Soft_attribute && parameters.metric_attribute = "P" then
        refuse "metric attribute requires scalar storage";
      if String.trim parameters.falloff_attribute = "P" then refuse "falloff output must not replace P";
      match Rdk.Transform_ops.compose_transform ~order:parameters.order ~rotation_order:parameters.rotation_order
        ~translate:(Rays_math.Vec3.create parameters.translate_x parameters.translate_y parameters.translate_z)
        ~rotate:(Rays_math.Vec3.create parameters.rotate_x parameters.rotate_y parameters.rotate_z)
        ~scale:(Rays_math.Vec3.create parameters.scale_x parameters.scale_y parameters.scale_z)
        ~shear:(Rays_math.Vec3.create parameters.shear_xy parameters.shear_xz parameters.shear_yz)
        ~uniform_scale:parameters.uniform_scale
        ~pivot:(Rays_math.Vec3.create parameters.pivot_x parameters.pivot_y parameters.pivot_z)
        ~pivot_rotation:(Rays_math.Vec3.create parameters.pivot_rotation_x parameters.pivot_rotation_y parameters.pivot_rotation_z)
        ~invert:parameters.invert () with
      | Error error -> refuse (Rdk.Error.to_string error)
      | Ok matrix ->
          for index = 0 to 15 do
            if not (Float.is_finite (Rays_math.Mat4.get matrix ~row:(index / 4) ~column:(index mod 4))) then
              refuse "composed transform must be finite"
          done]
    [@@sop.node_category "Deform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let order = parameters.order in
    let rotation_order = parameters.rotation_order in
    let translate = Rays_math.Vec3.create parameters.translate_x parameters.translate_y parameters.translate_z in
    let rotate = Rays_math.Vec3.create parameters.rotate_x parameters.rotate_y parameters.rotate_z in
    let scale = Rays_math.Vec3.create parameters.scale_x parameters.scale_y parameters.scale_z in
    let shear_xy = parameters.shear_xy in
    let shear_xz = parameters.shear_xz in
    let shear_yz = parameters.shear_yz in
    let uniform_scale = parameters.uniform_scale in
    let pivot = Rays_math.Vec3.create parameters.pivot_x parameters.pivot_y parameters.pivot_z in
    let pivot_rotation = Rays_math.Vec3.create parameters.pivot_rotation_x parameters.pivot_rotation_y parameters.pivot_rotation_z in
    let invert = parameters.invert in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let metric = parameters.metric in
    let metric_attribute = parameters.metric_attribute in
    let apply_rolloff = parameters.apply_rolloff in
    let falloff = parameters.falloff in
    let radius = parameters.radius in
    let falloff_attribute = parameters.falloff_attribute in
    let recompute_normals = parameters.recompute_normals in
    let selection = optional_element_group group_owner group
    and falloff_attribute = optional_text falloff_attribute in
    let metric = match metric with
      | Soft_radius -> Rdk.Transform_ops.Soft_radius
      | Soft_edge -> Rdk.Transform_ops.Soft_edge
      | Soft_attribute -> Rdk.Transform_ops.Soft_attribute {attribute=metric_attribute;apply_rolloff} in
    let matrix = Rdk.Transform_ops.compose_transform ~order ~rotation_order ~translate ~rotate ~scale
        ~shear:(Rays_math.Vec3.create shear_xy shear_xz shear_yz) ~uniform_scale ~pivot ~pivot_rotation ~invert ()
        |> Result.get_ok in
    Node.Private.make_geometry ?label ~operation:"soft_transform" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"soft_transform" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection -> match Rdk.Transform_ops.soft_transform
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ?selection ~metric ~falloff ~radius ?falloff_attribute
            ~recompute_normals matrix inputs.(0) with
          | Ok geometry -> cooked geometry
          | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end


module Ray = struct
  let method_parameter = Param.choice ~equal:( = ) [
      "Minimum distance", Rdk.Ray.Ray_minimum_distance;
      "Project rays", Rdk.Ray.Ray_project;
    ]
  let direction_parameter = Param.choice ~equal:( = ) [
      "Vector", Direction_vector; "Normal", Direction_normal;
      "Attribute", Direction_attribute;
    ]
  let direction_mode_parameter = Param.choice ~equal:( = ) [
      "Forward", Rdk.Ray.Ray_forward; "Reverse", Rdk.Ray.Ray_reverse;
      "Bidirectional closest", Rdk.Ray.Ray_bidirectional_closest;
      "Bidirectional farthest", Rdk.Ray.Ray_bidirectional_farthest;
    ]
  let surface_parameter = Param.choice ~equal:( = ) [
      "First surface", Rdk.Ray.Ray_first_surface;
      "Last surface", Rdk.Ray.Ray_last_surface;
    ]
  let combine_parameter = Param.choice ~equal:( = ) [
      "Average", Rdk.Ray.Ray_average; "Median", Rdk.Ray.Ray_median;
      "Shortest", Rdk.Ray.Ray_shortest; "Longest", Rdk.Ray.Ray_longest;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    collision_group : string [@sop.default ""]
      [@sop.label "Collision primitive group"];
    method_ : Rdk.Ray.method_ [@sop.default Rdk.Ray.Ray_project]
      [@sop.label "Method"] [@sop.kind method_parameter];
    direction : Support.ray_direction [@sop.default Direction_normal]
      [@sop.label "Direction"] [@sop.folder "Ray"]
      [@sop.kind direction_parameter];
    direction_x : float [@sop.default 0.] [@sop.label "Direction X"]
      [@sop.folder "Ray/Vector"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction_vector"]
    direction_y : float [@sop.default 1.] [@sop.label "Direction Y"]
      [@sop.folder "Ray/Vector"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction_vector"]
    direction_z : float [@sop.default 0.] [@sop.label "Direction Z"]
      [@sop.folder "Ray/Vector"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction_vector"]
    direction_attribute : string [@sop.default "N"]
      [@sop.label "Direction attribute"] [@sop.folder "Ray"];
    direction_mode : Rdk.Ray.direction_mode
      [@sop.default Rdk.Ray.Ray_forward]
      [@sop.label "Direction mode"] [@sop.folder "Ray"]
      [@sop.kind direction_mode_parameter];
    surface_hit : Rdk.Ray.surface_hit
      [@sop.default Rdk.Ray.Ray_first_surface]
      [@sop.label "Surface hit"] [@sop.folder "Ray"]
      [@sop.kind surface_parameter];
    samples : int [@sop.default 1] [@sop.label "Samples"]
      [@sop.folder "Jitter"] [@sop.min 1] [@sop.max 1024]
      [@sop.hard_min 1];
    jitter_scale : float [@sop.default 1.] [@sop.label "Jitter scale"]
      [@sop.folder "Jitter"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.folder "Jitter"]
      [@sop.min 0] [@sop.max 999999];
    combine : Rdk.Ray.combine [@sop.default Rdk.Ray.Ray_average]
      [@sop.label "Combine"] [@sop.folder "Jitter"]
      [@sop.kind combine_parameter];
    min_distance : float [@sop.default 0.] [@sop.label "Minimum distance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1000.]
      [@sop.hard_min 0.];
    limit_max_distance : bool [@sop.default false]
      [@sop.label "Limit maximum distance"] [@sop.folder "Distance"];
    max_distance : float [@sop.default 10.] [@sop.label "Maximum distance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1000.]
      [@sop.hard_min 0.];
    tolerance : float [@sop.default 0.] [@sop.label "Tolerance"]
      [@sop.folder "Distance"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    scale : float [@sop.default 1.] [@sop.label "Scale"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.];
    lift : float [@sop.default 0.] [@sop.label "Lift"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.];
    distance_attribute : string [@sop.default ""]
      [@sop.label "Distance"] [@sop.folder "Output"];
    primitive_attribute : string [@sop.default ""]
      [@sop.label "Primitive"] [@sop.folder "Output"];
    source_vertex_numbers_attribute : string [@sop.default ""]
      [@sop.label "Source vertices"] [@sop.folder "Output"];
    source_vertex_weights_attribute : string [@sop.default ""]
      [@sop.label "Source weights"] [@sop.folder "Output"];
    hit_group : string [@sop.default ""] [@sop.label "Hit group"]
      [@sop.folder "Output"];
    normal_attribute : string [@sop.default ""] [@sop.label "Hit normal"]
      [@sop.folder "Output"];
    point_pattern : string [@sop.default ""] [@sop.label "Point attributes"]
      [@sop.folder "Transfer"];
    vertex_pattern : string [@sop.default ""] [@sop.label "Vertex attributes"]
      [@sop.folder "Transfer"];
    primitive_pattern : string [@sop.default ""]
      [@sop.label "Primitive attributes"] [@sop.folder "Transfer"];
    detail_pattern : string [@sop.default ""] [@sop.label "Detail attributes"]
      [@sop.folder "Transfer"];
    match_groups : bool [@sop.default false] [@sop.label "Match groups"]
      [@sop.folder "Transfer"];
  } [@@sop.node_key "ray"] [@@sop.node_label "Ray"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/ray: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.direction_x;parameters.direction_y;parameters.direction_z;
          parameters.jitter_scale;parameters.min_distance;parameters.max_distance;parameters.tolerance;parameters.scale;parameters.lift]) then
        refuse "numeric controls must be finite";
      if parameters.samples < 1 || parameters.samples > 1024 then refuse "samples must be in [1, 1024]";
      if parameters.method_ = Rdk.Ray.Ray_minimum_distance && parameters.samples <> 1 then refuse "multiple samples require projection";
      if parameters.jitter_scale < 0. || parameters.min_distance < 0. || parameters.max_distance < 0.
          || parameters.tolerance < 0. || parameters.tolerance > sqrt Float.max_float then refuse "distance and jitter controls are out of range";
      if parameters.limit_max_distance && parameters.max_distance < parameters.min_distance then refuse "maximum must be at least minimum distance";
      if parameters.limit_max_distance && parameters.method_ = Rdk.Ray.Ray_minimum_distance
          && parameters.max_distance > sqrt Float.max_float then refuse "closest distance must be safely squarable";
      if parameters.method_ = Rdk.Ray.Ray_project then (match parameters.direction with
        | Direction_vector when parameters.direction_x = 0. && parameters.direction_y = 0. && parameters.direction_z = 0. -> refuse "direction must be nonzero"
        | Direction_attribute when String.trim parameters.direction_attribute = "" -> refuse "direction attribute must be nonblank"
        | _ -> ());
      let numbers = optional_text parameters.source_vertex_numbers_attribute
      and weights = optional_text parameters.source_vertex_weights_attribute in
      if Option.is_some numbers <> Option.is_some weights then refuse "vertex numbers and weights must be requested together";
      let names = List.filter_map optional_text [parameters.distance_attribute;parameters.primitive_attribute;
          parameters.source_vertex_numbers_attribute;parameters.source_vertex_weights_attribute;parameters.normal_attribute] in
      if List.mem "P" names then refuse "output must not replace P";
      if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "output names must be distinct";
      let patterns = [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern;parameters.detail_pattern] in
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "sop/ray" pattern) patterns;
      if parameters.match_groups && List.for_all (fun pattern -> String.trim pattern = "")
          [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern] then refuse "group interpolation requires a pattern"]
    [@@sop.node_category "Modify/Project"] [@@sop.node_inputs 2] [@@sop.node_slots "source, collision"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source collision ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let collision_group = parameters.collision_group in
    let method_ = parameters.method_ in
    let direction = parameters.direction in
    let direction_vector = Rays_math.Vec3.create parameters.direction_x parameters.direction_y parameters.direction_z in
    let direction_attribute = parameters.direction_attribute in
    let direction_mode = parameters.direction_mode in
    let surface_hit = parameters.surface_hit in
    let samples = parameters.samples in
    let jitter_scale = parameters.jitter_scale in
    let seed = parameters.seed in
    let combine = parameters.combine in
    let min_distance = parameters.min_distance in
    let limit_max_distance = parameters.limit_max_distance in
    let max_distance = parameters.max_distance in
    let tolerance = parameters.tolerance in
    let scale = parameters.scale in
    let lift = parameters.lift in
    let distance_attribute = parameters.distance_attribute in
    let primitive_attribute = parameters.primitive_attribute in
    let source_vertex_numbers_attribute = parameters.source_vertex_numbers_attribute in
    let source_vertex_weights_attribute = parameters.source_vertex_weights_attribute in
    let hit_group = parameters.hit_group in
    let normal_attribute = parameters.normal_attribute in
    let point_pattern = parameters.point_pattern in
    let vertex_pattern = parameters.vertex_pattern in
    let primitive_pattern = parameters.primitive_pattern in
    let detail_pattern = parameters.detail_pattern in
    let match_groups = parameters.match_groups in
    let selection = optional_element_group group_owner group and collision_group = optional_text collision_group in
    let direction = match direction with Direction_vector -> Rdk.Ray.Ray_vector direction_vector
      | Direction_normal -> Rdk.Ray.Ray_normal | Direction_attribute -> Rdk.Ray.Ray_attribute direction_attribute in
    let max_distance = if limit_max_distance then Some max_distance else None in
    let distance_attribute = optional_text distance_attribute and primitive_attribute = optional_text primitive_attribute
    and source_vertex_numbers_attribute = optional_text source_vertex_numbers_attribute
    and source_vertex_weights_attribute = optional_text source_vertex_weights_attribute
    and hit_group = optional_text hit_group and normal_attribute = optional_text normal_attribute
    and point_pattern = optional_text point_pattern and vertex_pattern = optional_text vertex_pattern
    and primitive_pattern = optional_text primitive_pattern and detail_pattern = optional_text detail_pattern in
    Node.Private.make_geometry ?label ~operation:"ray" ~version:2 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|source; collision|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"ray" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            let collision_primitives = match collision_group with
              | None -> Ok None
              | Some name ->
                  (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name
                      inputs.(1) with
                   | Some group -> Ok (Some group)
                   | None -> Error (Diagnostic.error ~code:"missing_collision_group"
                       (Printf.sprintf "ray could not find collision primitive group %S"
                         name))) in
            (match collision_primitives with
             | Error error -> Error error
             | Ok collision_primitives ->
                 match Rdk.Ray.run ~cancel:(Context.cancel_token context)
                     { grain = Context.grain context; selection;
                       collision_primitives; method_; direction; direction_mode;
                       surface_hit; samples; jitter_scale; seed; combine;
                       min_distance; max_distance; tolerance; scale; lift;
                       distance_attribute; primitive_attribute;
                       source_vertex_numbers_attribute;
                       source_vertex_weights_attribute; hit_group;
                       normal_attribute; point_pattern; vertex_pattern;
                       primitive_pattern; detail_pattern; match_groups }
                     ~source:inputs.(0) ~collision:inputs.(1) () with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end



