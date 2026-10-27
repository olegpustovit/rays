(* Shape nodes, declared once: the record is the editor schema and the factory. *)

open Rays_math
open Support

(* ocamldep must see the PPX's [Sop.X] resolve inside this library. *)
module Sop = Support.Sop

module Iso_surface = struct
  type parameters = {
    resolution_x : float [@sop.default 64.] [@sop.vec3 "resolution"]
      [@sop.min 1.] [@sop.max 128.] [@sop.primary];
    resolution_y : float [@sop.default 64.] [@sop.vec3 "resolution"]
      [@sop.min 1.] [@sop.max 128.];
    resolution_z : float [@sop.default 64.] [@sop.vec3 "resolution"]
      [@sop.min 1.] [@sop.max 128.];
    min_x : float [@sop.default (-2.)] [@sop.vec3 "min"] [@sop.min (-10.)] [@sop.max 10.];
    min_y : float [@sop.default (-2.)] [@sop.vec3 "min"] [@sop.min (-10.)] [@sop.max 10.];
    min_z : float [@sop.default (-2.)] [@sop.vec3 "min"] [@sop.min (-10.)] [@sop.max 10.];
    max_x : float [@sop.default 2.] [@sop.vec3 "max"] [@sop.min (-10.)] [@sop.max 10.];
    max_y : float [@sop.default 2.] [@sop.vec3 "max"] [@sop.min (-10.)] [@sop.max 10.];
    max_z : float [@sop.default 2.] [@sop.vec3 "max"] [@sop.min (-10.)] [@sop.max 10.];
    iso : float [@sop.default 0.] [@sop.min (-10.)] [@sop.max 10.] [@sop.primary];
    smooth : bool [@sop.default true];
  } [@@sop.node_key "iso_surface"] [@@sop.node_label "Iso surface"]
    [@@sop.node_category "Create/Field"] [@@sop.node_inputs 1]
    [@@sop.node_slots "field"] [@@sop.node_keywords "field"]
    [@@sop.node_types ["fn(vec3)->float"]]
    [@@sop.node_facts {elementwise = Node.None; reads = []; writes = ["P"; "N"];
      topology = Node.Changed; exact = true}]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/iso_surface: " ^ message) in
      let limit = Sys.max_array_length / 3 in
      let cells value =
        if not (Float.is_finite value) || value < 1. || value <> Float.floor value
            || value >= float_of_int limit then
          refuse "resolution requires positive integral cell counts";
        int_of_float value + 1 in
      let product a b =
        if a > limit / b then refuse "sample grid exceeds the packed array limit";
        a * b in
      ignore (product (product (cells parameters.resolution_x)
        (cells parameters.resolution_y)) (cells parameters.resolution_z));
      List.iter (fun (lo, hi) ->
        if not (Float.is_finite lo && Float.is_finite hi && Float.is_finite (hi -. lo))
            || hi <= lo then refuse "bounds require finite increasing extents")
        [parameters.min_x, parameters.max_x; parameters.min_y, parameters.max_y;
         parameters.min_z, parameters.max_z];
      if not (Float.is_finite parameters.iso) then refuse "iso must be finite"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters field ->
    let rx = int_of_float parameters.resolution_x
    and ry = int_of_float parameters.resolution_y
    and rz = int_of_float parameters.resolution_z in
    let min = Rays_math.Vec3.create parameters.min_x parameters.min_y parameters.min_z
    and max = Rays_math.Vec3.create parameters.max_x parameters.max_y parameters.max_z in
    let dx = (max.x -. min.x) /. float_of_int rx
    and dy = (max.y -. min.y) /. float_of_int ry
    and dz = (max.z -. min.z) /. float_of_int rz in
    let nx = rx + 1 and ny = ry + 1 and nz = rz + 1 in
    Node.Private.make ~label ~operation:"iso_surface" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|field|] (fun ~node_id:_ context inputs ->
        Result.bind (Payload.kernel inputs.(0)) (fun kernel ->
          let cancelled () = Error (Diagnostic.error ~code:"cancelled"
              "Iso surface sample grid cancelled") in
          if Context.cancelled context then cancelled () else
          let positions = Array.make (3 * nx * ny * nz) 0. in
          let stopped = ref false in
          for z = 0 to rz do
            if Context.cancelled context then stopped := true;
            if not !stopped then
              for y = 0 to ry do
                for x = 0 to rx do
                  let i = 3 * (x + nx * (y + ny * z)) in
                  positions.(i) <- Float.fma (float_of_int x) dx min.x;
                  positions.(i + 1) <- Float.fma (float_of_int y) dy min.y;
                  positions.(i + 2) <- Float.fma (float_of_int z) dz min.z
                done
              done
          done;
          if !stopped then cancelled () else
          Result.bind (Kernel.prepare kernel [Vec3s positions]) (fun run ->
          Result.bind (run context) (function
            | Kernel.Vec3s _ -> Error (Diagnostic.error ~code:"field_type"
                "Iso surface field must return one float per sample")
            | Kernel.Floats samples ->
                match Rdk.Iso_surface.extract_sampled ~cancel:(Context.cancel_token context)
                    ~grain:(Context.grain context) ~smooth:parameters.smooth
                    ~resolution:(rx, ry, rz) ~min ~max ~iso:parameters.iso ~samples () with
                | Error error -> structured_rdk_error error
                | Ok geometry -> Ok Node.Private.{payload = Payload.Geometry geometry;
                    instances = None})))))
  let factory = parameters_factory build
end

module Merge = struct
  type parameters = {
    source_attribute : string [@sop.default ""] [@sop.label "Source attribute"];
    source_base : int [@sop.default 0] [@sop.label "Source base"]
      [@sop.min (-1000)] [@sop.max 1000];
  } [@@sop.node_key "merge"] [@@sop.node_label "Merge"]
    [@@sop.node_category "Copy"] [@@sop.node_inputs 1]
    [@@sop.node_slots "input"] [@@sop.node_rest 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters inputs ->
    Node.Private.make_geometry ~label ~operation:"merge" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:(Array.of_list inputs) (fun ~node_id:_ context inputs ->
        match Rdk.Mesh_merge.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context)
            ?source_attribute:(optional_text parameters.source_attribute)
            ~source_base:parameters.source_base (Array.to_list inputs) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error))

  let factory = parameters_factory build
end

module Line = struct
  let kind_parameter = Param.choice ~equal:( = ) [
      "Polygon curve", Rdk.Line_geometry.Line_curve;
      "Points", Rdk.Line_geometry.Line_points;
    ]

  type parameters = {
    kind : Rdk.Line_geometry.kind [@sop.default Rdk.Line_geometry.Line_curve]
      [@sop.label "Primitive type"] [@sop.kind kind_parameter];
    points : int [@sop.default 2] [@sop.label "Points"]
      [@sop.min 2] [@sop.max 128] [@sop.hard_min 1];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    direction_x : float [@sop.default 0.] [@sop.label "Direction X"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_y : float [@sop.default 1.] [@sop.label "Direction Y"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_z : float [@sop.default 0.] [@sop.label "Direction Z"]
      [@sop.folder "Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    length : float [@sop.default 1.] [@sop.label "Length"]
      [@sop.min 0.] [@sop.max 10.] [@sop.hard_min 0.];
  } [@@sop.node_key "line"] [@@sop.node_label "Line"]
    [@@sop.node_category "Create/Curve"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let kind = parameters.kind in
    let points = parameters.points in
    let origin = Rays_math.Vec3.create parameters.origin_x parameters.origin_y
          parameters.origin_z in
    let direction = Rays_math.Vec3.create parameters.direction_x parameters.direction_y
          parameters.direction_z in
    let length = parameters.length in
    let origin = vec3_copy origin and direction = vec3_copy direction in
    Node.Private.make_geometry ?label ~operation:"line" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Line_geometry.line ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~kind ~points ~origin ~direction
            ~length () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Mirror = struct
  type parameters = {
    keep_original : bool [@sop.default true] [@sop.label "Keep original"];
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
  } [@@sop.node_key "mirror"] [@@sop.node_label "Mirror"]
    [@@sop.node_category "Modify"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let keep_original = parameters.keep_original in
    let origin = Rays_math.Vec3.create parameters.origin_x parameters.origin_y
          parameters.origin_z in
    let normal = Rays_math.Vec3.create parameters.normal_x parameters.normal_y
          parameters.normal_z in
    let origin = vec3_copy origin and normal = vec3_copy normal in
    Node.Private.make_geometry ?label ~operation:"mirror" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Mirror_geometry.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~keep_original ~origin ~normal inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Match_axis = struct
  type parameters = {
    from_x : float [@sop.default 0.] [@sop.label "From X"]
      [@sop.folder "From"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "from"]
    from_y : float [@sop.default 1.] [@sop.label "From Y"]
      [@sop.folder "From"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "from"]
    from_z : float [@sop.default 0.] [@sop.label "From Z"]
      [@sop.folder "From"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "from"]
    into_x : float [@sop.default 0.] [@sop.label "Into X"]
      [@sop.folder "Into"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "into"]
    into_y : float [@sop.default 1.] [@sop.label "Into Y"]
      [@sop.folder "Into"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "into"]
    into_z : float [@sop.default 0.] [@sop.label "Into Z"]
      [@sop.folder "Into"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "into"]
  } [@@sop.node_key "match_axis"] [@@sop.node_label "Match Axis"]
    [@@sop.node_category "Modify/Align"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let from = Rays_math.Vec3.create parameters.from_x parameters.from_y
          parameters.from_z in
    let into = Rays_math.Vec3.create parameters.into_x parameters.into_y
          parameters.into_z in
    let from = vec3_copy from and into = vec3_copy into in
    Node.Private.make_geometry ?label ~operation:"match_axis" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Match_size.match_axis ~grain:(Context.grain context) ~from ~into inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Noise_displace = struct
  let mode_parameter = Param.choice ~equal:( = ) [
    "height_2d", Rdk.Deform.Height_2d;
    "normal_3d", Rdk.Deform.Normal_3d;
  ]
  type parameters = {
    mode : Rdk.Deform.noise_displace_mode [@sop.default Rdk.Deform.Height_2d]
      [@sop.label "Mode"] [@sop.kind mode_parameter];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.min 0]
      [@sop.max 9999];
    amplitude : float [@sop.default 0.1] [@sop.label "Amplitude"]
      [@sop.min (-10.)] [@sop.max 10.];
    frequency : float [@sop.default 1.] [@sop.label "Frequency"]
      [@sop.min 0.] [@sop.max 20.] [@sop.hard_min 0.];
  } [@@sop.node_key "noise_displace"] [@@sop.node_label "Noise Displace"]
    [@@sop.node_facts {elementwise = Node.Points;
      reads = (match parameters.mode with Rdk.Deform.Height_2d -> ["P"] | Normal_3d -> ["P"; "N"]);
      writes = ["P"; "N"];
      topology = Node.Preserved; exact = false}]
    [@@sop.node_category "Deform/Noise"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let seed = if parameters.context_seed then None else Some parameters.seed in
    let amplitude = parameters.amplitude in
    let frequency = parameters.frequency in
    let mode = parameters.mode in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed
    in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("noise_displace:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"noise_displace" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let identity = Option.value ~default:(Int64.of_int node_id) stable_identity in
        let seed = Option.value ~default:(mixed_seed context identity) seed in
        match Rdk.Deform.noise_displace ~grain:(Context.grain context)
            ~cancel:(Context.cancel_token context)
            ~mode ~amplitude ~frequency ~seed inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Separate_pieces = struct
  let owner_parameter = Param.choice ~equal:( = ) [
      "Point", Rdk.Attribute.Point;
      "Primitive", Rdk.Attribute.Primitive;
    ]

  let mode_parameter = Param.choice ~equal:( = ) [
      "Separate", Rdk.Separate_pieces.Separate_pieces_separate;
      "Move back", Rdk.Separate_pieces.Separate_pieces_move_back;
    ]

  type parameters = {
    owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Piece owner"] [@sop.kind owner_parameter];
    piece_attribute : string [@sop.default "piece"]
      [@sop.label "Piece attribute"];
    translation_attribute : string [@sop.default "piece_translation"]
      [@sop.label "Translation attribute"];
    axis_x : float [@sop.default 1.] [@sop.label "Axis X"]
      [@sop.folder "Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_y : float [@sop.default 0.] [@sop.label "Axis Y"]
      [@sop.folder "Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"]
      [@sop.folder "Axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    gap : float [@sop.default 0.001] [@sop.label "Gap"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    mode : Rdk.Separate_pieces.mode
      [@sop.default Rdk.Separate_pieces.Separate_pieces_separate]
      [@sop.label "Mode"] [@sop.kind mode_parameter];
  } [@@sop.node_key "separate_pieces"] [@@sop.node_label "Separate Pieces"]
    [@@sop.node_category "Modify/Pieces"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let owner = parameters.owner in
    let translation_attribute = parameters.translation_attribute in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y
          parameters.axis_z in
    let gap = parameters.gap in
    let mode = parameters.mode in
    let piece_attribute = parameters.piece_attribute in
    let axis = vec3_copy axis in
    Node.Private.make_geometry ?label ~operation:"separate_pieces" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match Rdk.Separate_pieces.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~owner ~translation_attribute ~axis
            ~gap ~mode ~piece_attribute inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Box = struct
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Box_generator.Box_triangles;
      "Quads", Rdk.Box_generator.Box_quads;
      "Surface points", Rdk.Box_generator.Box_surface_points;
      "Lattice points", Rdk.Box_generator.Box_lattice_points;
    ]

  let normals_parameter = Param.choice ~equal:( = ) [
      "Auto", None;
      "None", Some Rdk.Box_generator.Box_no_normals;
      "Point", Some Rdk.Box_generator.Box_point_normals;
      "Vertex", Some Rdk.Box_generator.Box_vertex_normals;
    ]

  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Box_generator.Box_xyz; "XZY", Rdk.Box_generator.Box_xzy;
      "YXZ", Rdk.Box_generator.Box_yxz; "YZX", Rdk.Box_generator.Box_yzx;
      "ZXY", Rdk.Box_generator.Box_zxy; "ZYX", Rdk.Box_generator.Box_zyx;
    ]

  type parameters = {
    connectivity : Rdk.Box_generator.box_connectivity
      [@sop.default Rdk.Box_generator.Box_quads]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];
    normals : Rdk.Box_generator.box_normals option [@sop.default Some Rdk.Box_generator.Box_vertex_normals]
      [@sop.label "Normals"] [@sop.kind normals_parameter];
    size_x : float [@sop.default 1.] [@sop.label "Size X"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.]; [@sop.vec3 "size"] [@sop.primary]
    size_y : float [@sop.default 1.] [@sop.label "Size Y"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.]; [@sop.vec3 "size"] [@sop.primary]
    size_z : float [@sop.default 1.] [@sop.label "Size Z"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.]; [@sop.vec3 "size"] [@sop.primary]
    x_divisions : int [@sop.default 1] [@sop.label "X divisions"]
      [@sop.folder "Divisions"] [@sop.min 1] [@sop.max 24] [@sop.hard_min 1];
    y_divisions : int [@sop.default 1] [@sop.label "Y divisions"]
      [@sop.folder "Divisions"] [@sop.min 1] [@sop.max 24] [@sop.hard_min 1];
    z_divisions : int [@sop.default 1] [@sop.label "Z divisions"]
      [@sop.folder "Divisions"] [@sop.min 1] [@sop.max 24] [@sop.hard_min 1];
    consolidate_points : bool [@sop.default false]
      [@sop.label "Consolidate points"] [@sop.folder "Topology"];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"] [@sop.unit "rad"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"] [@sop.unit "rad"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"] [@sop.unit "rad"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_order : Rdk.Box_generator.box_rotation_order
      [@sop.default Rdk.Box_generator.Box_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"]
      [@sop.kind rotation_order_parameter];
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Transform"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    uv_attribute : string [@sop.default ""] [@sop.label "UV attribute"]
      [@sop.folder "Attributes"];
    face_groups : string [@sop.default ""] [@sop.label "Face group prefix"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "box"] [@@sop.node_label "Box"]
    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let size = Rays_math.Vec3.create parameters.size_x parameters.size_y
        parameters.size_z in
    let connectivity = parameters.connectivity in
    let consolidate_points = parameters.consolidate_points in
    let normals = parameters.normals in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y
          parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y
          parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let uniform_scale = parameters.uniform_scale in
    let x_divisions = parameters.x_divisions in
    let y_divisions = parameters.y_divisions in
    let z_divisions = parameters.z_divisions in
    let uv_attribute = optional_text parameters.uv_attribute in
    let face_groups = optional_text parameters.face_groups in
    let size = vec3_copy size and center = vec3_copy center
    and rotation = vec3_copy rotation in
    Node.Private.make_geometry ?label ~operation:"box" ~version:2 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Box_generator.box ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~connectivity ~consolidate_points
            ?normals ~center ~rotation ~rotation_order ~uniform_scale
            ~x_divisions ~y_divisions ~z_divisions ?uv_attribute ?face_groups
            ~size () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end

module Sort = struct
  open Rays_math
  type key = Support.sort_key = Sort_x | Sort_y | Sort_z | Sort_distance | Sort_vector
    | Sort_attribute | Sort_vertex_order | Sort_primitive_index | Sort_spatial
    | Sort_random | Sort_index_attribute | Sort_reverse | Sort_shift
  let owner_parameter = Param.choice ~equal:( = ) [
      "Points", Rdk.Ordering.Points; "Primitives", Rdk.Ordering.Primitives;
    ]
  let key_parameter = Param.choice ~equal:( = ) [
      "X", Sort_x; "Y", Sort_y; "Z", Sort_z; "Distance to point", Sort_distance;
      "Along vector", Sort_vector; "Attribute component", Sort_attribute;
      "Vertex order", Sort_vertex_order; "Primitive index", Sort_primitive_index;
      "Spatial locality", Sort_spatial; "Random", Sort_random;
      "Index attribute", Sort_index_attribute; "Reverse", Sort_reverse;
      "Shift", Sort_shift;
    ]
  type parameters = {
    owner : Rdk.Ordering.owner [@sop.default Rdk.Ordering.Points]
      [@sop.label "Entity"] [@sop.kind owner_parameter];
    key : key [@sop.default Sort_x] [@sop.label "Sort by"]
      [@sop.kind key_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    descending : bool [@sop.default false] [@sop.label "Descending"];
    x : float [@sop.default 0.] [@sop.label "X"]
      [@sop.folder "Key/Point or vector"] [@sop.min (-10.)] [@sop.max 10.];
    y : float [@sop.default 0.] [@sop.label "Y"]
      [@sop.folder "Key/Point or vector"] [@sop.min (-10.)] [@sop.max 10.];
    z : float [@sop.default 1.] [@sop.label "Z"]
      [@sop.folder "Key/Point or vector"] [@sop.min (-10.)] [@sop.max 10.];
    attribute : string [@sop.default "id"] [@sop.label "Attribute"]
      [@sop.folder "Key/Attribute"];
    component : int [@sop.default 0] [@sop.label "Component"]
      [@sop.folder "Key/Attribute"] [@sop.min 0] [@sop.max 15]
      [@sop.hard_min 0];
    seed : int [@sop.default 0] [@sop.label "Random seed"]
      [@sop.folder "Key"] [@sop.min 0] [@sop.max 9999];
    shift : int [@sop.default 1] [@sop.label "Shift"]
      [@sop.folder "Key"] [@sop.min (-100)] [@sop.max 100];
    output_indices : string [@sop.default ""] [@sop.label "Output indices"]
      [@sop.folder "Output"];
    combine_indices : bool [@sop.default false] [@sop.label "Combine indices"]
      [@sop.folder "Output"];
  } [@@sop.node_key "sort"] [@@sop.node_label "Sort"]
    [@@sop.node_category "Utility"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let descending = parameters.descending in
    let output_indices = optional_text parameters.output_indices in
    let combine_indices = parameters.combine_indices in
    let owner = parameters.owner in
    let key = parameters.key in
    let x = parameters.x in
    let y = parameters.y in
    let z = parameters.z in
    let attribute = parameters.attribute in
    let component = parameters.component in
    let seed = parameters.seed in
    let shift = parameters.shift in
    let key = match key with
      | Sort_x -> Rdk.Ordering.X | Sort_y -> Rdk.Ordering.Y | Sort_z -> Rdk.Ordering.Z
      | Sort_distance -> Rdk.Ordering.Distance_to (Rays_math.Vec3.create x y z)
      | Sort_vector -> Rdk.Ordering.Along_vector (Rays_math.Vec3.create x y z)
      | Sort_attribute -> Rdk.Ordering.Attribute_component { name = attribute; component }
      | Sort_vertex_order -> Rdk.Ordering.By_vertex_order
      | Sort_primitive_index -> Rdk.Ordering.By_primitive_index
      | Sort_spatial -> Rdk.Ordering.Spatial_locality
      | Sort_random -> Rdk.Ordering.Random (Int64.of_int seed)
      | Sort_index_attribute -> Rdk.Ordering.Index_attribute attribute
      | Sort_reverse -> Rdk.Ordering.Reverse
      | Sort_shift -> Rdk.Ordering.Shift shift in
    let owner_key = match owner with Rdk.Ordering.Points -> "points"
      | Rdk.Ordering.Primitives -> "primitives" in
    Node.Private.make_geometry ?label ~operation:"sort" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let group_owner = match owner with Rdk.Ordering.Points -> Rdk.Group.Point
          | Rdk.Ordering.Primitives -> Rdk.Group.Primitive in
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:group_owner name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "sort could not find %s group %S" owner_key name))) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Ordering.sort ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~descending
                ?output_indices ~combine_indices ~owner ~key inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

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

module Extract_centroid = struct
  type run_mode = Support.centroid_run_mode = Detail | Primitives | Point_pieces | Primitive_pieces
  let run_parameter = Param.choice ~equal:( = ) [
      "Detail", Detail; "Primitives", Primitives;
      "Point pieces", Point_pieces; "Primitive pieces", Primitive_pieces;
    ]
  let method_parameter = Param.choice ~equal:( = ) [
      "Point mass", Rdk.Curve_topology.Centroid_point_mass;
      "Bounding box", Rdk.Curve_topology.Centroid_bounding_box;
      "Convex hull", Rdk.Curve_topology.Centroid_convex_hull;
    ]

  type parameters = {
    run_over : run_mode [@sop.default Detail]
      [@sop.label "Run over"] [@sop.kind run_parameter];
    piece_attribute : string [@sop.default "piece"]
      [@sop.label "Piece attribute"];
    method_ : Rdk.Curve_topology.centroid_method
      [@sop.default Rdk.Curve_topology.Centroid_point_mass]
      [@sop.label "Method"] [@sop.kind method_parameter];
    source_primitive_attribute : string [@sop.default ""]
      [@sop.label "Source primitive attribute"] [@sop.folder "Output"];
    piece_output_attribute : string [@sop.default ""]
      [@sop.label "Piece output attribute"] [@sop.folder "Output"];
  } [@@sop.node_key "extract_centroid"]
    [@@sop.node_label "Extract Centroid"]
    [@@sop.node_category "Create/Point"] [@@sop.node_inputs 1]
    [@@sop.validate fun parameters ->
      (match parameters.run_over with
       | Point_pieces | Primitive_pieces ->
           if String.trim parameters.piece_attribute = "" || parameters.piece_attribute = "P" then
             invalid_arg "sop/extract_centroid: piece attribute must be non-empty and not P"
       | Detail | Primitives -> ());
      List.iter (fun (field, name) ->
        if name = "P" then invalid_arg
          ("sop/extract_centroid: " ^ field ^ " must be non-empty and not P"))
        ["source primitive attribute", parameters.source_primitive_attribute;
         "piece output attribute", parameters.piece_output_attribute]]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let run_over = parameters.run_over in
    let piece_attribute = parameters.piece_attribute in
    let method_ = parameters.method_ in
    let source_primitive_attribute = optional_text parameters.source_primitive_attribute in
    let piece_output_attribute = optional_text parameters.piece_output_attribute in
    let run_over = match run_over with
      | Detail -> Rdk.Curve_topology.Centroid_detail
      | Primitives -> Rdk.Curve_topology.Centroid_primitives
      | Point_pieces -> Rdk.Curve_topology.Centroid_pieces {
          owner = Rdk.Curve_topology.Centroid_piece_points; attribute = piece_attribute }
      | Primitive_pieces -> Rdk.Curve_topology.Centroid_pieces {
          owner = Rdk.Curve_topology.Centroid_piece_primitives; attribute = piece_attribute } in
    Node.Private.make_geometry ?label ~operation:"extract_centroid" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match Rdk.Curve_topology.extract_centroid ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~run_over ~method_
            ?source_primitive_attribute ?piece_output_attribute inputs.(0) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Peak = struct
  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"] [@sop.folder "Selection"];
    direction_attribute : string [@sop.default "N"]
      [@sop.label "Direction attribute"] [@sop.folder "Direction"];
    normalize_direction : bool [@sop.default true]
      [@sop.label "Normalize direction"] [@sop.folder "Direction"];
    mask_attribute : string [@sop.default ""] [@sop.label "Mask attribute"];
    distance : float [@sop.default 0.1] [@sop.label "Distance"]
      [@sop.min (-10.)] [@sop.max 10.]; [@sop.primary]
    recompute_normals : bool [@sop.default false]
      [@sop.label "Recompute normals"];
  } [@@sop.node_key "peak"] [@@sop.node_label "Peak"]
    [@@sop.node_facts {elementwise = Node.Points;
      reads = List.filter (fun name -> String.trim name <> "")
        ["P"; (if String.trim parameters.direction_attribute = "" then "N" else parameters.direction_attribute);
          parameters.mask_attribute];
      writes = ["P"; "N"];
      topology = Node.Preserved; exact = false}]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.distance) then invalid_arg "sop/peak: distance must be finite"]
    [@@sop.node_category "Deform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let direction_attribute = parameters.direction_attribute in
    let normalize_direction = parameters.normalize_direction in
    let mask_attribute = optional_text parameters.mask_attribute in
    let distance = parameters.distance in
    let recompute_normals = parameters.recompute_normals in
    let selection = optional_element_group group_owner group in
    let direction_attribute = optional_text direction_attribute in
    Node.Private.make_geometry ?label ~operation:"peak" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"peak" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Deform.peak ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ?direction_attribute
                ~normalize_direction ?mask_attribute ~distance ~recompute_normals
                inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Bend = struct
  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"] [@sop.folder "Selection"];
    mask_attribute : string [@sop.default ""] [@sop.label "Mask attribute"];
    origin_x : float [@sop.default 0.] [@sop.label "Origin X"]
      [@sop.folder "Capture/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_y : float [@sop.default 0.] [@sop.label "Origin Y"]
      [@sop.folder "Capture/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    origin_z : float [@sop.default 0.] [@sop.label "Origin Z"]
      [@sop.folder "Capture/Origin"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "origin"]
    direction_x : float [@sop.default 0.] [@sop.label "Direction X"]
      [@sop.folder "Capture/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_y : float [@sop.default 1.] [@sop.label "Direction Y"]
      [@sop.folder "Capture/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    direction_z : float [@sop.default 0.] [@sop.label "Direction Z"]
      [@sop.folder "Capture/Direction"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "direction"]
    up_x : float [@sop.default 0.] [@sop.label "Up X"]
      [@sop.folder "Capture/Up"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "up"]
    up_y : float [@sop.default 0.] [@sop.label "Up Y"]
      [@sop.folder "Capture/Up"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "up"]
    up_z : float [@sop.default 1.] [@sop.label "Up Z"]
      [@sop.folder "Capture/Up"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "up"]
    length : float [@sop.default 1.] [@sop.label "Length"]
      [@sop.folder "Capture"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    bend_angle : float [@sop.default 0.] [@sop.label "Bend angle"]
      [@sop.folder "Deformation"] [@sop.min (-6.283185)] [@sop.max 6.283185]; [@sop.primary]
    twist_angle : float [@sop.default 0.] [@sop.label "Twist angle"]
      [@sop.folder "Deformation"] [@sop.min (-6.283185)] [@sop.max 6.283185]; [@sop.primary]
    limit : bool [@sop.default true] [@sop.label "Limit deformation"];
    both_directions : bool [@sop.default false]
      [@sop.label "Capture both directions"];
    continuous_twist : bool [@sop.default false]
      [@sop.label "Continuous twist"];
    capture_attribute : string [@sop.default ""]
      [@sop.label "Capture attribute"] [@sop.folder "Attributes"];
    recompute_normals : bool [@sop.default false]
      [@sop.label "Recompute normals"];
  } [@@sop.node_key "bend"] [@@sop.node_label "Bend"]
    [@@sop.node_facts {elementwise = Node.Points;
      reads = List.filter (fun name -> String.trim name <> "") ["P"; parameters.mask_attribute];
      writes = List.filter (fun name -> String.trim name <> "")
        ["P"; "N"; parameters.capture_attribute];
      topology = Node.Preserved; exact = false}]
    [@@sop.validate fun parameters ->
      if not (List.for_all Float.is_finite [parameters.origin_x; parameters.origin_y; parameters.origin_z;
          parameters.direction_x; parameters.direction_y; parameters.direction_z;
          parameters.up_x; parameters.up_y; parameters.up_z; parameters.length;
          parameters.bend_angle; parameters.twist_angle]) then
        invalid_arg "sop/bend: capture vectors, length and angles must be finite";
      if parameters.length <= 0. then invalid_arg "sop/bend: capture length must be positive";
      let unit x y z =
        let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
        if scale = 0. then invalid_arg "sop/bend: capture direction and up must be non-zero";
        Rays_math.Vec3.normalize (Rays_math.Vec3.create (x /. scale) (y /. scale) (z /. scale)) in
      let direction = unit parameters.direction_x parameters.direction_y parameters.direction_z
      and up = unit parameters.up_x parameters.up_y parameters.up_z in
      let projection = Rays_math.Vec3.dot up direction in
      if up.x -. projection *. direction.x = 0. && up.y -. projection *. direction.y = 0.
          && up.z -. projection *. direction.z = 0. then
        invalid_arg "sop/bend: up must not be parallel to capture direction";
      if String.trim parameters.capture_attribute = "P" then
        invalid_arg "sop/bend: capture attribute must not be P"]
    [@@sop.node_category "Deform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let mask_attribute = optional_text parameters.mask_attribute in
    let origin = Rays_math.Vec3.create parameters.origin_x parameters.origin_y
          parameters.origin_z in
    let direction = Rays_math.Vec3.create parameters.direction_x parameters.direction_y
          parameters.direction_z in
    let up = Rays_math.Vec3.create parameters.up_x parameters.up_y parameters.up_z in
    let length = parameters.length in
    let bend_angle = parameters.bend_angle in
    let twist_angle = parameters.twist_angle in
    let limit = parameters.limit in
    let both_directions = parameters.both_directions in
    let continuous_twist = parameters.continuous_twist in
    let capture_attribute = optional_text parameters.capture_attribute in
    let recompute_normals = parameters.recompute_normals in
    let selection = optional_element_group group_owner group in
    let origin = vec3_copy origin and direction = vec3_copy direction
    and up = vec3_copy up in
    Node.Private.make_geometry ?label ~operation:"bend" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"bend" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            match Rdk.Deform.bend ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ?mask_attribute ~origin
                ~direction ~up ~length ~bend_angle ~twist_angle ~limit
                ~both_directions ~continuous_twist ?capture_attribute
                ~recompute_normals inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )

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

module Smooth = struct
  type mode = Support.smoothing_mode = Laplacian | Custom

  let boundary_parameter = Param.choice ~equal:( = ) [
      "Free", Rdk.Smooth.Smooth_free;
      "Pin unshared", Rdk.Smooth.Smooth_unshared;
      "Pin group boundary", Rdk.Smooth.Smooth_group_boundary;
    ]

  let method_parameter = Param.choice ~equal:( = ) [
      "Uniform", Rdk.Attribute_ops.Uniform;
      "Edge length", Rdk.Attribute_ops.Edge_length;
    ]

  let mode_parameter = Param.choice ~equal:( = ) [
      "Laplacian", Laplacian; "Custom steps", Custom;
    ]

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    constrained_points : string [@sop.default ""]
      [@sop.label "Constrained points"];
    boundary : Rdk.Smooth.boundary [@sop.default Rdk.Smooth.Smooth_free]
      [@sop.label "Boundary"] [@sop.kind boundary_parameter];
    iterations : int [@sop.default 10] [@sop.label "Iterations"]
      [@sop.min 1] [@sop.max 200] [@sop.hard_min 1];
    method_ : Rdk.Attribute_ops.blur_method
      [@sop.default Rdk.Attribute_ops.Uniform]
      [@sop.label "Method"] [@sop.kind method_parameter];
    mode : mode [@sop.default Laplacian] [@sop.label "Mode"]
      [@sop.kind mode_parameter];
    step : float [@sop.default 0.5] [@sop.label "Step"]
      [@sop.folder "Smoothing"] [@sop.min 0.] [@sop.max 1.];
    odd_step : float [@sop.default 0.5] [@sop.label "Odd step"]
      [@sop.folder "Custom steps"] [@sop.min (-1.)] [@sop.max 1.];
    even_step : float [@sop.default (-0.53)] [@sop.label "Even step"]
      [@sop.folder "Custom steps"] [@sop.min (-1.)] [@sop.max 1.];
    weight_attribute : string [@sop.default ""]
      [@sop.label "Weight attribute"] [@sop.folder "Attributes"];
    alpha_attribute : string [@sop.default ""]
      [@sop.label "Alpha attribute"] [@sop.folder "Attributes"];
    attributes : string [@sop.default "P"] [@sop.label "Attributes"]
      [@sop.folder "Attributes"];
    original_blend : float [@sop.default 0.] [@sop.label "Original blend"]
      [@sop.folder "Blend"] [@sop.min 0.] [@sop.max 1.];
    smoothed_blend : float [@sop.default 1.] [@sop.label "Smoothed blend"]
      [@sop.folder "Blend"] [@sop.min 0.] [@sop.max 1.];
    recompute_normals : bool [@sop.default false]
      [@sop.label "Recompute normals"];
  } [@@sop.node_key "smooth"] [@@sop.node_label "Smooth"]
    [@@sop.validate fun parameters ->
      (match Rdk.Attribute_pattern.compile parameters.attributes with
       | Ok _ -> () | Error message -> invalid_arg ("sop/smooth: " ^ message));
      if parameters.iterations < 1 then invalid_arg "sop/smooth: iterations must be positive";
      if not (List.for_all Float.is_finite [parameters.step; parameters.odd_step;
          parameters.even_step; parameters.original_blend; parameters.smoothed_blend]) then
        invalid_arg "sop/smooth: steps and blend amounts must be finite"]
    [@@sop.node_category "Deform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = optional_text parameters.group in
    let constrained_points = optional_text parameters.constrained_points in
    let boundary = parameters.boundary in
    let iterations = parameters.iterations in
    let method_ = parameters.method_ in
    let mode = parameters.mode in
    let step = parameters.step in
    let odd_step = parameters.odd_step in
    let even_step = parameters.even_step in
    let weight_attribute = optional_text parameters.weight_attribute in
    let alpha_attribute = optional_text parameters.alpha_attribute in
    let recompute_normals = parameters.recompute_normals in
    let original_blend = parameters.original_blend in
    let smoothed_blend = parameters.smoothed_blend in
    let attributes = parameters.attributes in
    let mode = match mode with
      | Laplacian -> Rdk.Attribute_ops.Laplacian step
      | Custom -> Rdk.Attribute_ops.Custom_steps { odd = odd_step; even = even_step } in
    Node.Private.make_geometry ?label ~operation:"smooth" ~version:1
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        let resolve owner code kind = function
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code
                   (Printf.sprintf "smooth could not find %s %S" kind name))) in
        match resolve Rdk.Group.Primitive "missing_group" "primitive group" group with
        | Error error -> Error error
        | Ok primitives ->
            (match resolve Rdk.Group.Point "missing_constrained_points"
                "constrained point group" constrained_points with
             | Error error -> Error error
             | Ok constrained_points ->
                 match Rdk.Smooth.run ~cancel:(Context.cancel_token context)
                     ~grain:(Context.grain context) ?primitives
                     ?constrained_points ~boundary ~iterations ~method_ ~mode
                     ?weight_attribute ?alpha_attribute ~recompute_normals
                     ~original_blend ~smoothed_blend ~attributes geometry with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )

  let factory = parameters_factory build
end

module Bound = struct
  open Rays_math
  type shape = Support.group_bounds_shape = Box | Sphere
  let shape_parameter = Param.choice ~equal:( = ) [
      "Box", Box; "Sphere", Sphere;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"];
    shape : shape [@sop.default Box] [@sop.label "Shape"]
      [@sop.kind shape_parameter];
    divisions_x : int [@sop.default 1] [@sop.label "X divisions"]
      [@sop.folder "Box"] [@sop.min 1] [@sop.max 64] [@sop.hard_min 1];
    divisions_y : int [@sop.default 1] [@sop.label "Y divisions"]
      [@sop.folder "Box"] [@sop.min 1] [@sop.max 64] [@sop.hard_min 1];
    divisions_z : int [@sop.default 1] [@sop.label "Z divisions"]
      [@sop.folder "Box"] [@sop.min 1] [@sop.max 64] [@sop.hard_min 1];
    segments : int [@sop.default 32] [@sop.label "Segments"]
      [@sop.folder "Sphere"] [@sop.min 3] [@sop.max 256]
      [@sop.hard_min 3];
    rings : int [@sop.default 16] [@sop.label "Rings"]
      [@sop.folder "Sphere"] [@sop.min 2] [@sop.max 256]
      [@sop.hard_min 2];
    minimum_radius : float [@sop.default 0.] [@sop.label "Minimum radius"]
      [@sop.folder "Sphere"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    lower_x : float [@sop.default 0.] [@sop.label "Lower X"]
      [@sop.folder "Padding/Lower"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "lower"]
    lower_y : float [@sop.default 0.] [@sop.label "Lower Y"]
      [@sop.folder "Padding/Lower"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "lower"]
    lower_z : float [@sop.default 0.] [@sop.label "Lower Z"]
      [@sop.folder "Padding/Lower"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "lower"]
    upper_x : float [@sop.default 0.] [@sop.label "Upper X"]
      [@sop.folder "Padding/Upper"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "upper"]
    upper_y : float [@sop.default 0.] [@sop.label "Upper Y"]
      [@sop.folder "Padding/Upper"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "upper"]
    upper_z : float [@sop.default 0.] [@sop.label "Upper Z"]
      [@sop.folder "Padding/Upper"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "upper"]
    bounds_group : string [@sop.default ""] [@sop.label "Bounds group"]
      [@sop.folder "Output"];
    center_attribute : string [@sop.default ""] [@sop.label "Center attribute"]
      [@sop.folder "Output"];
    radii_attribute : string [@sop.default ""] [@sop.label "Radii attribute"]
      [@sop.folder "Output"];
  } [@@sop.node_key "bound"] [@@sop.node_label "Bound"]
    [@@sop.validate fun parameters ->
      if parameters.divisions_x < 1 || parameters.divisions_y < 1 || parameters.divisions_z < 1
          || parameters.segments < 3 || parameters.rings < 2 then
        invalid_arg "sop/bound: invalid divisions or sphere resolution";
      if not (List.for_all (fun value -> Float.is_finite value && value >= 0.)
          [parameters.lower_x; parameters.lower_y; parameters.lower_z; parameters.upper_x;
           parameters.upper_y; parameters.upper_z; parameters.minimum_radius]) then
        invalid_arg "sop/bound: padding and minimum radius must be finite and non-negative";
      let names = List.filter_map optional_text [parameters.center_attribute; parameters.radii_attribute] in
      if List.mem "P" names then invalid_arg "sop/bound: output attributes cannot be P";
      if List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid_arg "sop/bound: output attribute names must be distinct"]
    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = optional_text parameters.group in
    let shape = parameters.shape in
    let divisions_x = parameters.divisions_x in
    let divisions_y = parameters.divisions_y in
    let divisions_z = parameters.divisions_z in
    let segments = parameters.segments in
    let rings = parameters.rings in
    let minimum_radius = parameters.minimum_radius in
    let lower = Rays_math.Vec3.create parameters.lower_x parameters.lower_y
          parameters.lower_z in
    let upper = Rays_math.Vec3.create parameters.upper_x parameters.upper_y
          parameters.upper_z in
    let bounds_group = optional_text parameters.bounds_group in
    let center_attribute = optional_text parameters.center_attribute in
    let radii_attribute = optional_text parameters.radii_attribute in
    let selection = optional_element_group group_owner (Option.value ~default:"" group) in
    let shape = match shape with
      | Box -> Rdk.Bound.Bound_box { divisions = divisions_x, divisions_y, divisions_z }
      | Sphere -> Rdk.Bound.Bound_sphere { segments; rings; minimum_radius } in
    let lower_padding = vec3_copy lower and upper_padding = vec3_copy upper in
    Node.Private.make_geometry ?label ~operation:"bound" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"bound" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            (match Rdk.Bound.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~shape ~lower_padding
                ~upper_padding ?bounds_group ?center_attribute ?radii_attribute
                inputs.(0) with
             | Ok geometry -> cooked geometry
             | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Match_size = struct
  open Rays_math
  type target_justify_mode = Support.target_justify_mode = Justify_explicit | Justify_auto
  let target_justify_parameter = Param.choice ~equal:( = ) [
      "Explicit", Justify_explicit; "Auto", Justify_auto;
    ]
  let fit_parameter = Param.choice ~equal:( = ) [
      "Translate only", Rdk.Match_size.Translate_only;
      "Stretch", Rdk.Match_size.Stretch;
      "Contain", Rdk.Match_size.Contain;
      "Cover", Rdk.Match_size.Cover;
      "Match X", Rdk.Match_size.Match_x;
      "Match Y", Rdk.Match_size.Match_y;
      "Match Z", Rdk.Match_size.Match_z;
      "Match perimeter", Rdk.Match_size.Match_perimeter;
      "Match area", Rdk.Match_size.Match_area;
      "Match volume", Rdk.Match_size.Match_volume;
    ]

  type parameters = {
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Move group type"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Move group"];
    source_group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Source group type"] [@sop.kind element_owner_parameter];
    source_group : string [@sop.default ""] [@sop.label "Source group"];
    target_group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Target group type"] [@sop.kind element_owner_parameter];
    target_group : string [@sop.default ""] [@sop.label "Target group"];
    target_justify_mode : target_justify_mode [@sop.default Justify_explicit]
      [@sop.label "Target justification"] [@sop.kind target_justify_parameter];
    fit : Rdk.Match_size.match_size_fit [@sop.default Rdk.Match_size.Contain]
      [@sop.label "Fit"] [@sop.kind fit_parameter];
    translate_x : bool [@sop.default true] [@sop.label "Translate X"]
      [@sop.folder "Axes/Translate"];
    translate_y : bool [@sop.default true] [@sop.label "Translate Y"]
      [@sop.folder "Axes/Translate"];
    translate_z : bool [@sop.default true] [@sop.label "Translate Z"]
      [@sop.folder "Axes/Translate"];
    scale_x : bool [@sop.default true] [@sop.label "Scale X"]
      [@sop.folder "Axes/Scale"];
    scale_y : bool [@sop.default true] [@sop.label "Scale Y"]
      [@sop.folder "Axes/Scale"];
    scale_z : bool [@sop.default true] [@sop.label "Scale Z"]
      [@sop.folder "Axes/Scale"];
    justify_x : float [@sop.default 0.] [@sop.label "Justify X"]
      [@sop.folder "Justify/Source"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "justify"]
    justify_y : float [@sop.default 0.] [@sop.label "Justify Y"]
      [@sop.folder "Justify/Source"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "justify"]
    justify_z : float [@sop.default 0.] [@sop.label "Justify Z"]
      [@sop.folder "Justify/Source"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "justify"]
    target_justify_x : float [@sop.default 0.] [@sop.label "Justify X"]
      [@sop.folder "Justify/Target"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "target_justify"]
    target_justify_y : float [@sop.default 0.] [@sop.label "Justify Y"]
      [@sop.folder "Justify/Target"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "target_justify"]
    target_justify_z : float [@sop.default 0.] [@sop.label "Justify Z"]
      [@sop.folder "Justify/Target"] [@sop.min (-1.)] [@sop.max 1.]
      [@sop.hard_min (-1.)] [@sop.hard_max 1.]; [@sop.vec3 "target_justify"]
    offset_x : float [@sop.default 0.] [@sop.label "Offset X"]
      [@sop.folder "Transform/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_y : float [@sop.default 0.] [@sop.label "Offset Y"]
      [@sop.folder "Transform/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_z : float [@sop.default 0.] [@sop.label "Offset Z"]
      [@sop.folder "Transform/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    scale : float [@sop.default 1.] [@sop.label "Scale"]
      [@sop.folder "Transform"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    target_center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Numeric target/Center"] [@sop.min (-10.)]
      [@sop.max 10.]; [@sop.vec3 "target_center"]
    target_center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Numeric target/Center"] [@sop.min (-10.)]
      [@sop.max 10.]; [@sop.vec3 "target_center"]
    target_center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Numeric target/Center"] [@sop.min (-10.)]
      [@sop.max 10.]; [@sop.vec3 "target_center"]
    target_size_x : float [@sop.default 1.] [@sop.label "Size X"]
      [@sop.folder "Numeric target/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "target_size"]
    target_size_y : float [@sop.default 1.] [@sop.label "Size Y"]
      [@sop.folder "Numeric target/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "target_size"]
    target_size_z : float [@sop.default 1.] [@sop.label "Size Z"]
      [@sop.folder "Numeric target/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "target_size"]
  } [@@sop.node_key "match_size"] [@@sop.node_label "Match Size"]
    [@@sop.validate fun parameters ->
      if not (List.for_all (fun value -> Float.is_finite value && abs_float value <= 1.)
          [parameters.justify_x; parameters.justify_y; parameters.justify_z;
           parameters.target_justify_x; parameters.target_justify_y; parameters.target_justify_z]) then
        invalid_arg "sop/match_size: justification must be finite and between -1 and 1";
      if not (List.for_all Float.is_finite [parameters.offset_x; parameters.offset_y;
          parameters.offset_z; parameters.target_center_x; parameters.target_center_y;
          parameters.target_center_z]) then invalid_arg "sop/match_size: offset and center must be finite";
      if not (List.for_all (fun value -> Float.is_finite value && value >= 0.)
          [parameters.scale; parameters.target_size_x; parameters.target_size_y;
           parameters.target_size_z]) then invalid_arg "sop/match_size: scale and target size must be finite and non-negative"]
    [@@sop.node_category "Modify"] [@@sop.node_inputs 2] [@@sop.node_slots "input, target"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input target ->
    let label = Some label in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
    let source_group_owner = parameters.source_group_owner in
    let source_group = parameters.source_group in
    let target_group_owner = parameters.target_group_owner in
    let target_group = parameters.target_group in
    let fit = parameters.fit in
    let translate_x = parameters.translate_x in
    let translate_y = parameters.translate_y in
    let translate_z = parameters.translate_z in
    let scale_x = parameters.scale_x in
    let scale_y = parameters.scale_y in
    let scale_z = parameters.scale_z in
    let justify = Rays_math.Vec3.create parameters.justify_x parameters.justify_y
          parameters.justify_z in
    let target_justify_mode = parameters.target_justify_mode in
    let target_justify = Rays_math.Vec3.create parameters.target_justify_x
          parameters.target_justify_y parameters.target_justify_z in
    let offset = Rays_math.Vec3.create parameters.offset_x parameters.offset_y
          parameters.offset_z in
    let scale = parameters.scale in
    let target_center = Rays_math.Vec3.create parameters.target_center_x parameters.target_center_y parameters.target_center_z in
    let target_size = Rays_math.Vec3.create parameters.target_size_x parameters.target_size_y parameters.target_size_z in
    let selection = optional_element_group group_owner group
    and source_selection = optional_element_group source_group_owner source_group
    and target_selection = optional_element_group target_group_owner target_group in
    let translate_axes = translate_x, translate_y, translate_z
    and scale_axes = scale_x, scale_y, scale_z in
    let justify = vec3_copy justify
    and target_justify = match target_justify_mode with
      | Justify_explicit -> Some (vec3_copy target_justify) | Justify_auto -> None in
    let target_center, target_size = match target with
      | Some _ -> None, None
      | None -> Some (vec3_copy target_center), Some (vec3_copy target_size) in
    let offset = vec3_copy offset in
    let inputs = match target with None -> [|input|] | Some target -> [|input; target|] in
    Node.Private.make_geometry ?label ~operation:"match_size" ~version:2 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs
      (fun ~node_id:_ context inputs ->
        let source = inputs.(0)
        and target = if Array.length inputs = 2 then Some inputs.(1) else None in
        match resolve_element_group ~operation:"match_size" selection source with
        | Error error -> Error error
        | Ok selection ->
            (match resolve_element_group ~operation:"match_size source"
                source_selection source with
             | Error error -> Error error
             | Ok source_selection ->
                 let target_selection_result = match target with
                   | None ->
                       (match target_selection with
                        | None -> Ok None
                        | Some _ -> Error (Diagnostic.error ~code:"missing_target"
                            "match_size target selection requires a target node"))
                   | Some target -> resolve_element_group
                       ~operation:"match_size target" target_selection target in
                 (match target_selection_result with
                  | Error error -> Error error
                  | Ok target_selection ->
                      match Rdk.Match_size.run
                          ~cancel:(Context.cancel_token context)
                          ~grain:(Context.grain context) ?selection
                          ?source_selection ?target_selection ~fit ~translate_axes
                          ~scale_axes ~justify ?target_justify ~offset ~scale
                          ?target_center ?target_size ?target source with
                      | Ok geometry -> cooked geometry
                      | Error error -> structured_rdk_error error)))
  )

  let factory = parameters_factory build
end

module Point_generate = struct
  type parameters = {
    points : int [@sop.default 50] [@sop.label "Points"]
      [@sop.min 1] [@sop.max 50] [@sop.hard_min 1] [@sop.hard_max 50];
    generated_group : string [@sop.default ""] [@sop.label "Generated group"];
    source_point_attribute : string [@sop.default "sourcepoint"] [@sop.label "Source point attribute"];
    source_index_attribute : string [@sop.default "sourceindex"] [@sop.label "Source index attribute"];
  } [@@sop.node_key "points"] [@@sop.node_label "Point Generate"]
    [@@sop.validate fun parameters ->
      if parameters.points < 1 || parameters.points > 50 then
        invalid_arg "sop/point_generate_origin: points must be between 1 and 50";
      let names = [parameters.source_point_attribute; parameters.source_index_attribute] in
      if List.exists (fun name -> String.trim name = "" || name = "P") names then
        invalid_arg "sop/point_generate_origin: metadata names must be nonblank and cannot be P";
      if parameters.source_point_attribute = parameters.source_index_attribute then
        invalid_arg "sop/point_generate_origin: metadata names must be distinct"]
    [@@sop.node_operation "point_generate"]
    [@@sop.node_category "Create/Point"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let generated_group = optional_text parameters.generated_group in
    let source_point_attribute = parameters.source_point_attribute in
    let source_index_attribute = parameters.source_index_attribute in
    let points = parameters.points in
    let mode = Rdk.Point_generate.Generate_total points in
    Node.Private.make_geometry ?label ~operation:"point_generate" ~version:1
      ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Point_generate.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ?generated_group
            ~source_point_attribute ~source_index_attribute ~mode
            (Rdk.Line_geometry.points [||]) with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Platonic = struct
  open Rays_math
  type orientation_mode = Support.axis_orientation = Axis_x | Axis_y | Axis_z | Axis_custom

  let kind_parameter = Param.choice ~equal:( = ) [
      "Tetrahedron", Rdk.Parametric_generators.Platonic_tetrahedron;
      "Cube", Rdk.Parametric_generators.Platonic_cube;
      "Octahedron", Rdk.Parametric_generators.Platonic_octahedron;
      "Icosahedron", Rdk.Parametric_generators.Platonic_icosahedron;
      "Dodecahedron", Rdk.Parametric_generators.Platonic_dodecahedron;
      "Soccer ball", Rdk.Parametric_generators.Platonic_soccer_ball;
    ]

  let normals_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Parametric_generators.Platonic_no_normals;
      "Point", Rdk.Parametric_generators.Platonic_point_normals;
      "Vertex", Rdk.Parametric_generators.Platonic_vertex_normals;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "X axis", Axis_x; "Y axis", Axis_y; "Z axis", Axis_z;
      "Custom axis", Axis_custom;
    ]

  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Parametric_generators.Platonic_xyz; "XZY", Rdk.Parametric_generators.Platonic_xzy;
      "YXZ", Rdk.Parametric_generators.Platonic_yxz; "YZX", Rdk.Parametric_generators.Platonic_yzx;
      "ZXY", Rdk.Parametric_generators.Platonic_zxy; "ZYX", Rdk.Parametric_generators.Platonic_zyx;
    ]

  type parameters = {
    kind : Rdk.Parametric_generators.platonic_kind
      [@sop.default Rdk.Parametric_generators.Platonic_dodecahedron]
      [@sop.label "Type"] [@sop.kind kind_parameter]; [@sop.primary]
    normals : Rdk.Parametric_generators.platonic_normals
      [@sop.default Rdk.Parametric_generators.Platonic_vertex_normals]
      [@sop.label "Normals"] [@sop.kind normals_parameter];
    radius : float [@sop.default 1.] [@sop.label "Radius"]
      [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.]; [@sop.primary]
    orientation : orientation_mode [@sop.default Axis_y]
      [@sop.label "Orientation"] [@sop.folder "Transform"]
      [@sop.kind orientation_parameter];
    axis_x : float [@sop.default 0.] [@sop.label "Axis X"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_order : Rdk.Parametric_generators.platonic_rotation_order
      [@sop.default Rdk.Parametric_generators.Platonic_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"]
      [@sop.kind rotation_order_parameter];
    face_groups : string [@sop.default ""] [@sop.label "Face group prefix"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "platonic"] [@@sop.node_label "Platonic Solid"]
    [@@sop.validate fun parameters ->
      if not (Float.is_finite parameters.radius) || parameters.radius <= 0. then
        invalid_arg "sop/platonic: radius must be finite and positive";
      if not (List.for_all Float.is_finite [parameters.axis_x; parameters.axis_y; parameters.axis_z;
          parameters.center_x; parameters.center_y; parameters.center_z;
          parameters.rotation_x; parameters.rotation_y; parameters.rotation_z]) then
        invalid_arg "sop/platonic: axis, center and rotation must be finite";
      if parameters.orientation = Axis_custom && parameters.axis_x = 0.
          && parameters.axis_y = 0. && parameters.axis_z = 0. then
        invalid_arg "sop/platonic: custom axis must be nonzero"]
    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let kind = parameters.kind in
    let normals = parameters.normals in
    let orientation = parameters.orientation in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y
          parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y
          parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let face_groups = optional_text parameters.face_groups in
    let radius = parameters.radius in
    let orientation = match orientation with
      | Axis_x -> Rdk.Parametric_generators.Platonic_x
      | Axis_y -> Rdk.Parametric_generators.Platonic_y
      | Axis_z -> Rdk.Parametric_generators.Platonic_z
      | Axis_custom -> Rdk.Parametric_generators.Platonic_axis (vec3_copy axis) in
    let center = vec3_copy center and rotation = vec3_copy rotation in
    Node.Private.make_geometry ?label ~operation:"platonic" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Parametric_generators.platonic ~cancel:(Context.cancel_token context) ~kind
            ~normals ~orientation ~center ~rotation ~rotation_order ?face_groups
            ~radius () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end

module Snap_to_grid = struct
  let rounding_parameter = Param.choice ~equal:( = ) [
      "Nearest", Rdk.Fuse_grid.Grid_nearest;
      "Down", Rdk.Fuse_grid.Grid_down;
      "Up", Rdk.Fuse_grid.Grid_up;
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

  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    spacing_x : float [@sop.default 1.] [@sop.label "Spacing X"]
      [@sop.folder "Grid/Spacing"] [@sop.min 0.001] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "spacing"]
    spacing_y : float [@sop.default 1.] [@sop.label "Spacing Y"]
      [@sop.folder "Grid/Spacing"] [@sop.min 0.001] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "spacing"]
    spacing_z : float [@sop.default 1.] [@sop.label "Spacing Z"]
      [@sop.folder "Grid/Spacing"] [@sop.min 0.001] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "spacing"]
    offset_x : float [@sop.default 0.] [@sop.label "Offset X"]
      [@sop.folder "Grid/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_y : float [@sop.default 0.] [@sop.label "Offset Y"]
      [@sop.folder "Grid/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    offset_z : float [@sop.default 0.] [@sop.label "Offset Z"]
      [@sop.folder "Grid/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "offset"]
    rounding : Rdk.Fuse_grid.grid_rounding [@sop.default Rdk.Fuse_grid.Grid_nearest]
      [@sop.label "Rounding"] [@sop.kind rounding_parameter];
    limit_distance : bool [@sop.default false]
      [@sop.label "Limit snapping distance"];
    max_distance : float [@sop.default 1.] [@sop.label "Maximum distance"]
      [@sop.min 0.] [@sop.max 10.] [@sop.hard_min 0.];
    fuse_points : bool [@sop.default false] [@sop.label "Fuse points"]
      [@sop.folder "Fuse"];
    position : Rdk.Fuse_reduce.position
      [@sop.default Rdk.Fuse_reduce.Average_position]
      [@sop.label "Position"] [@sop.folder "Fuse"]
      [@sop.kind position_parameter];
    weight_attribute : string [@sop.default ""]
      [@sop.label "Weight attribute"] [@sop.folder "Fuse"];
    attributes : Rdk.Fuse_reduce.attributes
      [@sop.default Rdk.Fuse_reduce.Keep_first]
      [@sop.label "Attributes"] [@sop.folder "Fuse"]
      [@sop.kind attributes_parameter];
    snapped_group : string [@sop.default ""] [@sop.label "Snapped group"]
      [@sop.folder "Output"];
    attribute_rules : string [@sop.default ""] [@sop.label "Attribute rules"]
      [@sop.folder "Fuse"]
      [@sop.description "Escaped tab-separated rows: pattern, reduction method, weight attribute. Weighted methods require a weight name."];
    group_rules : string [@sop.default ""] [@sop.label "Group rules"]
      [@sop.folder "Fuse"]
      [@sop.description "Escaped tab-separated rows: group pattern, reduction method."];
  } [@@sop.node_key "snap_to_grid"] [@@sop.node_label "Snap to Grid"]
    [@@sop.validate fun parameters ->
      if not (List.for_all (fun value -> Float.is_finite value && value > 0.)
          [parameters.spacing_x; parameters.spacing_y; parameters.spacing_z]) then
        invalid_arg "sop/snap_to_grid: spacing must be finite and positive";
      if not (List.for_all (fun value -> Float.is_finite value && value >= 0. && value <= 1.)
          [parameters.offset_x; parameters.offset_y; parameters.offset_z]) then
        invalid_arg "sop/snap_to_grid: offset fractions must be finite and in [0, 1]";
      if not (Float.is_finite parameters.max_distance) || parameters.max_distance < 0. then
        invalid_arg "sop/snap_to_grid: maximum distance must be finite and nonnegative";
      ignore (decode_fuse_attribute_rules "sop/snap_to_grid" parameters.attribute_rules);
      ignore (decode_fuse_group_rules "sop/snap_to_grid" parameters.group_rules)]
    [@@sop.node_category "Point"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let spacing = Rays_math.Vec3.create parameters.spacing_x parameters.spacing_y
          parameters.spacing_z in
    let offset = Rays_math.Vec3.create parameters.offset_x parameters.offset_y
          parameters.offset_z in
    let rounding = parameters.rounding in
    let limit_distance = parameters.limit_distance in
    let max_distance = parameters.max_distance in
    let fuse_points = parameters.fuse_points in
    let position = parameters.position in
    let weight_attribute = parameters.weight_attribute in
    let attributes = parameters.attributes in
    let attribute_rules = parameters.attribute_rules in
    let group_rules = parameters.group_rules in
    let snapped_group = parameters.snapped_group in
    let group = optional_text group and snapped_group = optional_text snapped_group
    and weight_attribute = optional_text weight_attribute in
    let max_distance = if limit_distance then Some max_distance else None in
    let attribute_rules = decode_fuse_attribute_rules "sop/snap_to_grid" attribute_rules
    and group_rules = decode_fuse_group_rules "sop/snap_to_grid" group_rules in
    let spacing = vec3_copy spacing and offset = vec3_copy offset in
    Node.Private.make_geometry ?label ~operation:"snap_to_grid" ~version:3 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_optional_point_group "snap_to_grid" group inputs.(0) with
        | Error error -> Error error
        | Ok selection ->
            (match Rdk.Fuse_grid.snap_to_grid ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ~spacing ~offset ~rounding
                ?max_distance ~fuse_points ~position ?weight_attribute ~attributes
                ~attribute_rules ~group_rules ?snapped_group
                inputs.(0) with
             | Ok geometry -> cooked geometry
             | Error error -> structured_rdk_error error))
  )

  let factory = parameters_factory build
end

module Point_jitter = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Group"];
    mask_attribute : string [@sop.default ""] [@sop.label "Mask attribute"];
    id_attribute : string [@sop.default ""] [@sop.label "ID attribute"];
    seed_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Seed mode"] [@sop.kind kernel_mode_parameter];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.min 0]
      [@sop.max 9999];
    scale : float [@sop.default 1.] [@sop.label "Scale"]
      [@sop.min 0.] [@sop.max 2.] [@sop.hard_min 0.];
    use_point_scale : bool [@sop.default false] [@sop.label "Use point scale"];
    axis_x : float [@sop.default 1.] [@sop.label "Axis X"]
      [@sop.folder "Axis scales"] [@sop.min 0.] [@sop.max 2.]
      [@sop.hard_min 0.]; [@sop.vec3 "axis"]
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"]
      [@sop.folder "Axis scales"] [@sop.min 0.] [@sop.max 2.]
      [@sop.hard_min 0.]; [@sop.vec3 "axis"]
    axis_z : float [@sop.default 1.] [@sop.label "Axis Z"]
      [@sop.folder "Axis scales"] [@sop.min 0.] [@sop.max 2.]
      [@sop.hard_min 0.]; [@sop.vec3 "axis"]
  } [@@sop.node_key "point_jitter"] [@@sop.node_label "Point Jitter"]
    [@@sop.validate fun parameters ->
      if not (List.for_all (fun value -> Float.is_finite value && value >= 0.)
          [parameters.scale; parameters.axis_x; parameters.axis_y; parameters.axis_z]) then
        invalid_arg "sop/point_jitter: scale and axis scales must be finite and nonnegative"]
    [@@sop.node_category "Point"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let mask_attribute = parameters.mask_attribute in
    let id_attribute = parameters.id_attribute in
    let seed_mode = parameters.seed_mode in
    let seed = parameters.seed in
    let scale = parameters.scale in
    let axis_scales = Rays_math.Vec3.create parameters.axis_x parameters.axis_y
          parameters.axis_z in
    let use_point_scale = parameters.use_point_scale in
    let group = optional_text group and mask_attribute = optional_text mask_attribute
    and id_attribute = optional_text id_attribute in
    let seed = if seed_mode = Kernel_auto then None else Some seed in
    let axis_scales = vec3_copy axis_scales in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("point_jitter:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"point_jitter" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let points = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "point_jitter could not find point group %S" name))) in
        match points with
        | Error error -> Error error
        | Ok points ->
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Option.value ~default:(mixed_seed context identity) seed in
            match Rdk.Point_jitter.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ?mask_attribute
                ?id_attribute ~use_point_scale ~seed:(Rays_math.Rand.seed seed) ~scale
                ~axis_scales inputs.(0) with
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

module Copy_to_points = struct
  type parameters = {
    source_group : string [@sop.default ""] [@sop.label "Source group"]
      [@sop.folder "Selection"];
    target_group : string [@sop.default ""] [@sop.label "Target group"]
      [@sop.folder "Selection"];
    piece_attribute : string [@sop.default ""] [@sop.label "Piece attribute"]
      [@sop.folder "Matching"];
    pack : bool [@sop.default false] [@sop.label "Pack and instance"];
    target_attributes : string [@sop.default ""] [@sop.label "Target attribute rules"]
      [@sop.description "Escaped tab-separated rows: pattern, points/vertices/primitives, nothing/copy/add/subtract/multiply."];
  } [@@sop.node_key "copy_to_points"] [@@sop.node_label "Copy to Points"]
    [@@sop.validate fun parameters ->
      ignore (decode_copy_target_rules parameters.target_attributes);
      if parameters.pack && (String.trim parameters.source_group <> ""
          || String.trim parameters.piece_attribute <> "") then
        invalid_arg "sop/copy_to_points: pack requires the whole source without piece matching"]
    [@@sop.node_category "Copy"] [@@sop.node_inputs 2] [@@sop.node_slots "source, targets"]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters source targets ->
    let label = Some label in
    let source_group = parameters.source_group in
    let target_group = parameters.target_group in
    let piece_attribute = parameters.piece_attribute in
    let target_attributes = parameters.target_attributes in
    let pack = parameters.pack in
    let source_group = optional_text source_group and target_group = optional_text target_group
    and piece_attribute = optional_text piece_attribute in
    let target_attributes = decode_copy_target_rules target_attributes in
    Node.Private.make_geometry ?label ~operation:"copy_to_points" ~version:8
      ~parameters:""
      ~cook_mode:Node.Generic
      ~dependencies:Context.Dependencies.static ~inputs:[|source; targets|]
      (fun ~node_id:_ context inputs ->
        let source_primitives = match source_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name
                  inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "copy_to_points could not find source primitive group %S" name))) in
        let target_points = match target_group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name inputs.(1) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                     "copy_to_points could not find target point group %S" name))) in
        match source_primitives, target_points with
        | Error error, _ | _, Error error -> Error error
        | Ok _, Ok target_points when pack ->
            (* Packed: the whole source once, drawn at each target's transform. *)
            (match Rdk.Instance_copy.copy_transforms ~grain:(Context.grain context)
                ~cancel:(Context.cancel_token context) ?target_points inputs.(1) with
              | Ok transforms -> Ok Node.Private.{ geometry = inputs.(0); instances = Some transforms }
              | Error error -> structured_rdk_error error)
        | Ok source_primitives, Ok target_points ->
            match Rdk.Instance_copy.copy_to_points ~grain:(Context.grain context)
                ~cancel:(Context.cancel_token context) ?source_primitives
                ?target_points ?piece_attribute ~target_attributes ~source:inputs.(0)
                ~targets:inputs.(1) () with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
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
          | Some attribute -> (match Rdk.Attribute.Private.storage attribute with
              | Rdk.Attribute.Float4 values -> Ok (Rdk.Packed.Float4.Private.view values)
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

module Spiral = struct
  open Rays_math
  let extent_parameter = Param.choice ~equal:( = ) [
      "Turns and height", Spiral_turns_height; "Height and pitch", Spiral_height_pitch;
    ]
  let radius_parameter = Param.choice ~equal:( = ) [
      "Archimedean change", Spiral_archimedean_change;
      "Archimedean end", Spiral_archimedean_end;
      "Logarithmic change", Spiral_logarithmic_change;
      "Logarithmic end", Spiral_logarithmic_end;
    ]
  let direction_parameter = Param.choice ~equal:( = ) [
      "Counterclockwise", Rdk.Spiral.Spiral_counterclockwise;
      "Clockwise", Rdk.Spiral.Spiral_clockwise;
    ]
  let divisions_parameter = Param.choice ~equal:( = ) [
      "Per curve", Spiral_per_curve; "Per turn", Spiral_per_turn;
    ]
  let orientation_parameter = Param.choice ~equal:( = ) [
      "X axis", Axis_x; "Y axis", Axis_y; "Z axis", Axis_z;
      "Custom axis", Axis_custom;
    ]
  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Spiral.Spiral_xyz; "XZY", Rdk.Spiral.Spiral_xzy;
      "YXZ", Rdk.Spiral.Spiral_yxz; "YZX", Rdk.Spiral.Spiral_yzx;
      "ZXY", Rdk.Spiral.Spiral_zxy; "ZYX", Rdk.Spiral.Spiral_zyx;
    ]

  type parameters = {
    extent_mode : Support.spiral_extent_mode [@sop.default Spiral_turns_height]
      [@sop.label "Extent"] [@sop.kind extent_parameter];
    turns : float [@sop.default 3.] [@sop.label "Turns"]
      [@sop.folder "Extent"] [@sop.min 0.01] [@sop.max 20.]
      [@sop.hard_min 0.];
    height : float [@sop.default 2.] [@sop.label "Height"]
      [@sop.folder "Extent"] [@sop.min (-20.)] [@sop.max 20.];
    pitch : float [@sop.default 0.6666666666666666] [@sop.label "Pitch"]
      [@sop.folder "Extent"] [@sop.min (-10.)] [@sop.max 10.];
    radius_mode : Support.spiral_radius_mode [@sop.default Spiral_archimedean_change]
      [@sop.label "Radius model"] [@sop.kind radius_parameter];
    start_radius : float [@sop.default 1.] [@sop.label "Start radius"]
      [@sop.folder "Radius"] [@sop.min 0.001] [@sop.max 10.]
      [@sop.hard_min 0.];
    radius_change : float [@sop.default 0.] [@sop.label "Increase per turn"]
      [@sop.folder "Radius"] [@sop.min (-5.)] [@sop.max 5.];
    end_radius : float [@sop.default 1.] [@sop.label "End radius"]
      [@sop.folder "Radius"] [@sop.min 0.001] [@sop.max 10.]
      [@sop.hard_min 0.];
    logarithmic_scale : float [@sop.default 1.]
      [@sop.label "Scale per turn"] [@sop.folder "Radius"]
      [@sop.min 0.01] [@sop.max 4.] [@sop.hard_min 0.];

    height_ramp : string [@sop.default ""] [@sop.label "Height ramp"]
      [@sop.description "Comma-separated position:value knots; blank is unset, one knot is constant, multiple knots span 0 through 1."];
    radius_ramp : string [@sop.default ""] [@sop.label "Radius ramp"]
      [@sop.description "Comma-separated position:value knots; blank is unset, one knot is constant, multiple knots span 0 through 1."];
    radius_scale : float [@sop.default 1.] [@sop.label "Radius scale"]
      [@sop.folder "Radius"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    direction : Rdk.Spiral.direction
      [@sop.default Rdk.Spiral.Spiral_counterclockwise]
      [@sop.label "Direction"] [@sop.kind direction_parameter];
    start_angle : float [@sop.default 0.] [@sop.label "Start angle"]
      [@sop.min (-6.283185307179586)] [@sop.max 6.283185307179586];
    divisions_mode : Support.spiral_divisions_mode [@sop.default Spiral_per_turn]
      [@sop.label "Divisions"] [@sop.kind divisions_parameter];
    divisions : int [@sop.default 32] [@sop.label "Division count"]
      [@sop.min 2] [@sop.max 512] [@sop.hard_min 1];
    uniform_angle : bool [@sop.default true] [@sop.label "Uniform angle"];
    spiral_count : int [@sop.default 1] [@sop.label "Spiral count"]
      [@sop.min 1] [@sop.max 64] [@sop.hard_min 1];
    orientation : Support.axis_orientation [@sop.default Axis_y]
      [@sop.label "Orientation"] [@sop.folder "Transform"]
      [@sop.kind orientation_parameter];
    axis_x : float [@sop.default 0.] [@sop.label "Axis X"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"]
      [@sop.folder "Transform/Custom axis"] [@sop.min (-1.)] [@sop.max 1.]; [@sop.vec3 "axis"]
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_order : Rdk.Spiral.rotation_order
      [@sop.default Rdk.Spiral.Spiral_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"]
      [@sop.kind rotation_order_parameter];
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Transform"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    angle_attribute : string [@sop.default ""] [@sop.label "Angle"]
      [@sop.folder "Attributes"];
    x_axis_attribute : string [@sop.default ""] [@sop.label "X axis"]
      [@sop.folder "Attributes"];
    y_axis_attribute : string [@sop.default ""] [@sop.label "Y axis"]
      [@sop.folder "Attributes"];
    tangent_attribute : string [@sop.default ""] [@sop.label "Tangent"]
      [@sop.folder "Attributes"];
    orient_attribute : string [@sop.default ""] [@sop.label "Orient"]
      [@sop.folder "Attributes"];
    distance_attribute : string [@sop.default ""] [@sop.label "Distance"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "spiral"] [@@sop.node_label "Spiral"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/spiral: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.turns; parameters.height; parameters.pitch;
          parameters.start_radius; parameters.radius_change; parameters.end_radius; parameters.logarithmic_scale;
          parameters.radius_scale; parameters.start_angle; parameters.axis_x; parameters.axis_y; parameters.axis_z;
          parameters.center_x; parameters.center_y; parameters.center_z; parameters.rotation_x;
          parameters.rotation_y; parameters.rotation_z; parameters.uniform_scale]) then
        refuse "controls must be finite";
      if parameters.turns < 0. || parameters.start_radius < 0. || parameters.end_radius < 0.
          || parameters.logarithmic_scale < 0. || parameters.radius_scale <= 0. || parameters.uniform_scale <= 0. then
        refuse "invalid radius or scale";
      let turns = match parameters.extent_mode with
        | Spiral_turns_height -> parameters.turns
        | Spiral_height_pitch -> parameters.height /. parameters.pitch in
      if not (Float.is_finite turns) || turns <= 0. then refuse "extent must have positive finite turns";
      (match parameters.radius_mode with
       | Spiral_archimedean_change ->
           let radius = parameters.start_radius +. parameters.radius_change *. turns in
           if not (Float.is_finite radius) || radius < 0. then refuse "radius profile must remain finite/nonnegative"
       | Spiral_archimedean_end -> ()
       | Spiral_logarithmic_change ->
           let radius = parameters.start_radius *. exp (log parameters.logarithmic_scale *. turns) in
           if parameters.start_radius <= 0. || parameters.logarithmic_scale <= 0.
               || not (Float.is_finite radius) || radius <= 0. then refuse "logarithmic radius must remain finite/positive"
       | Spiral_logarithmic_end ->
           if parameters.start_radius <= 0. || parameters.end_radius <= 0. then refuse "logarithmic radii must be positive");
      if parameters.divisions <= 0 || parameters.spiral_count <= 0 then refuse "counts must be positive";
      let segments = match parameters.divisions_mode with
        | Spiral_per_curve -> float_of_int parameters.divisions
        | Spiral_per_turn -> Float.max 1. (ceil (turns *. float_of_int parameters.divisions)) in
      if not (Float.is_finite segments) || segments >= float_of_int Sys.max_array_length then
        refuse "output cardinality exceeds array limits";
      if parameters.spiral_count > min (Sys.max_array_length - 1) Sys.max_string_length
          || parameters.spiral_count > Sys.max_array_length / (int_of_float segments + 1) then
        refuse "output cardinality exceeds array limits";
      if parameters.orientation = Axis_custom && parameters.axis_x = 0. && parameters.axis_y = 0.
          && parameters.axis_z = 0. then refuse "custom axis must be non-zero";
      let names = List.filter_map optional_text [parameters.angle_attribute; parameters.x_axis_attribute;
        parameters.y_axis_attribute; parameters.tangent_attribute; parameters.orient_attribute; parameters.distance_attribute] in
      if List.mem "P" names || List.length names <> List.length (List.sort_uniq String.compare names) then
        refuse "output names must be unique and cannot be P";
      ignore (decode_parameter_ramp ~allow_single:true "sop/spiral" parameters.height_ramp);
      ignore (decode_parameter_ramp ~allow_single:true "sop/spiral" parameters.radius_ramp)]
    [@@sop.node_category "Create/Curve"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let extent_mode = parameters.extent_mode in
    let turns = parameters.turns in
    let height = parameters.height in
    let pitch = parameters.pitch in
    let radius_mode = parameters.radius_mode in
    let start_radius = parameters.start_radius in
    let radius_change = parameters.radius_change in
    let end_radius = parameters.end_radius in
    let logarithmic_scale = parameters.logarithmic_scale in
    let height_ramp = parameters.height_ramp in
    let radius_scale = parameters.radius_scale in
    let radius_ramp = parameters.radius_ramp in
    let direction = parameters.direction in
    let start_angle = parameters.start_angle in
    let divisions_mode = parameters.divisions_mode in
    let divisions = parameters.divisions in
    let uniform_angle = parameters.uniform_angle in
    let spiral_count = parameters.spiral_count in
    let orientation = parameters.orientation in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let uniform_scale = parameters.uniform_scale in
    let angle_attribute = parameters.angle_attribute in
    let x_axis_attribute = parameters.x_axis_attribute in
    let y_axis_attribute = parameters.y_axis_attribute in
    let tangent_attribute = parameters.tangent_attribute in
    let orient_attribute = parameters.orient_attribute in
    let distance_attribute = parameters.distance_attribute in
    let extent = match extent_mode with
      | Spiral_turns_height -> Rdk.Spiral.Spiral_turns {turns; height}
      | Spiral_height_pitch -> Rdk.Spiral.Spiral_height_pitch {height; pitch} in
    let radius = match radius_mode with
      | Spiral_archimedean_change -> Rdk.Spiral.Spiral_archimedean_change {start_radius; increase_per_turn=radius_change}
      | Spiral_archimedean_end -> Rdk.Spiral.Spiral_archimedean_end {start_radius; end_radius}
      | Spiral_logarithmic_change -> Rdk.Spiral.Spiral_logarithmic_change {start_radius; scale_per_turn=logarithmic_scale}
      | Spiral_logarithmic_end -> Rdk.Spiral.Spiral_logarithmic_end {start_radius; end_radius} in
    let divisions = match divisions_mode with
      | Spiral_per_curve -> Rdk.Spiral.Spiral_divisions_per_curve divisions
      | Spiral_per_turn -> Rdk.Spiral.Spiral_divisions_per_turn divisions in
    let orientation = match orientation with
      | Axis_x -> Rdk.Spiral.Spiral_x | Axis_y -> Rdk.Spiral.Spiral_y
      | Axis_z -> Rdk.Spiral.Spiral_z | Axis_custom -> Rdk.Spiral.Spiral_axis axis in
    let height_ramp = decode_parameter_ramp ~allow_single:true "sop/spiral" height_ramp
    and radius_ramp = decode_parameter_ramp ~allow_single:true "sop/spiral" radius_ramp in
    let angle_attribute = optional_text angle_attribute and x_axis_attribute = optional_text x_axis_attribute
    and y_axis_attribute = optional_text y_axis_attribute and tangent_attribute = optional_text tangent_attribute
    and orient_attribute = optional_text orient_attribute and distance_attribute = optional_text distance_attribute in
    Node.Private.make_geometry ?label ~operation:"spiral" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Spiral.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~extent ~radius ~height_ramp
            ~radius_scale ~radius_ramp ~direction ~start_angle ~divisions
            ~uniform_angle ~spiral_count ~orientation ~center ~rotation
            ~rotation_order ~uniform_scale ?angle_attribute ?x_axis_attribute
            ?y_axis_attribute ?tangent_attribute ?orient_attribute
            ?distance_attribute () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Circle = struct
  open Rays_math

  let arc_parameter = Param.choice ~equal:( = ) [
      "Closed", Circle_closed; "Open arc", Circle_open; "Chord closed", Circle_chord;
      "Sliced", Circle_sliced;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "XZ", Plane_xz; "XY", Plane_xy;
      "YZ", Plane_yz; "Custom axes", Plane_axes;
    ]

  type parameters = {
    arc : Support.circle_arc [@sop.default Circle_closed] [@sop.label "Arc"]
      [@sop.kind arc_parameter];
    start_angle : float [@sop.default 0.] [@sop.label "Start angle"]
      [@sop.folder "Arc"] [@sop.min (-6.283185)] [@sop.max 6.283185];
    end_angle : float [@sop.default 6.283185307179586]
      [@sop.label "End angle"] [@sop.folder "Arc"]
      [@sop.min (-6.283185)] [@sop.max 6.283185];
    orientation : Support.plane_orientation
      [@sop.default Plane_xz] [@sop.label "Orientation"]
      [@sop.kind orientation_parameter];

    horizontal_x : float [@sop.default 1.] [@sop.label "Horizontal X"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    horizontal_y : float [@sop.default 0.] [@sop.label "Horizontal Y"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    horizontal_z : float [@sop.default 0.] [@sop.label "Horizontal Z"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    vertical_x : float [@sop.default 0.] [@sop.label "Vertical X"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    vertical_y : float [@sop.default 0.] [@sop.label "Vertical Y"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    vertical_z : float [@sop.default 1.] [@sop.label "Vertical Z"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    radius : float [@sop.default 1.] [@sop.label "Auto radius"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.];
    radius_x_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Radius X mode"] [@sop.kind kernel_mode_parameter];
    radius_y_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Radius Y mode"] [@sop.kind kernel_mode_parameter];
    reverse : bool [@sop.default false] [@sop.label "Reverse"];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    radius_x : float [@sop.default 1.] [@sop.label "Radius X"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    radius_y : float [@sop.default 1.] [@sop.label "Radius Y"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    rotation : float [@sop.default 0.] [@sop.label "Rotation"]
      [@sop.folder "Transform"] [@sop.min (-3.14159)] [@sop.max 3.14159];
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    segments : int [@sop.default 48] [@sop.label "Segments"]
      [@sop.min 3] [@sop.max 256] [@sop.hard_min 3]; [@sop.primary]
  } [@@sop.node_key "circle"] [@@sop.node_label "Circle"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/circle: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.start_angle; parameters.end_angle;
          parameters.horizontal_x; parameters.horizontal_y; parameters.horizontal_z;
          parameters.vertical_x; parameters.vertical_y; parameters.vertical_z;
          parameters.center_x; parameters.center_y; parameters.center_z;
          parameters.radius; parameters.radius_x; parameters.radius_y;
          parameters.rotation; parameters.uniform_scale]) then refuse "controls must be finite";
      if parameters.radius <= 0. || parameters.radius_x <= 0. || parameters.radius_y <= 0.
          || parameters.uniform_scale <= 0. then refuse "radii and scale must be positive";
      let x = if parameters.radius_x_mode = Kernel_auto then parameters.radius else parameters.radius_x
      and y = if parameters.radius_y_mode = Kernel_auto then parameters.radius else parameters.radius_y in
      let x = x *. parameters.uniform_scale and y = y *. parameters.uniform_scale in
      if not (Float.is_finite x && Float.is_finite y && x > 0. && y > 0.) then
        refuse "scaled radii must be finite and positive";
      if parameters.segments < 3 then refuse "segments must be at least three";
      let extra = match parameters.arc with Circle_closed -> 0 | Circle_sliced -> 2 | _ -> 1 in
      if parameters.segments > Sys.max_array_length - extra then refuse "output cardinality exceeds array limits";
      if parameters.arc <> Circle_closed then begin
        let sweep = parameters.end_angle -. parameters.start_angle in
        if not (Float.is_finite sweep) || sweep = 0. then refuse "arc must have finite nonzero sweep"
      end;
      if parameters.orientation = Plane_axes then
        validate_plane_axes "sop/circle"
          (Rays_math.Vec3.create parameters.horizontal_x parameters.horizontal_y parameters.horizontal_z)
          (Rays_math.Vec3.create parameters.vertical_x parameters.vertical_y parameters.vertical_z)]
    [@@sop.node_category "Create/Curve"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let arc = parameters.arc in
    let start_angle = parameters.start_angle in
    let end_angle = parameters.end_angle in
    let orientation = parameters.orientation in
    let horizontal = Rays_math.Vec3.create parameters.horizontal_x parameters.horizontal_y parameters.horizontal_z in
    let vertical = Rays_math.Vec3.create parameters.vertical_x parameters.vertical_y parameters.vertical_z in
    let reverse = parameters.reverse in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let radius = parameters.radius in
    let radius_x_mode = parameters.radius_x_mode in
    let radius_x = parameters.radius_x in
    let radius_y_mode = parameters.radius_y_mode in
    let radius_y = parameters.radius_y in
    let rotation = parameters.rotation in
    let uniform_scale = parameters.uniform_scale in
    let segments = parameters.segments in
    let arc = match arc with
      | Circle_closed -> Rdk.Plane_generators.Circle_closed
      | Circle_open -> Rdk.Plane_generators.Circle_open_arc {start_angle;end_angle}
      | Circle_chord -> Rdk.Plane_generators.Circle_closed_arc {start_angle;end_angle}
      | Circle_sliced -> Rdk.Plane_generators.Circle_sliced_arc {start_angle;end_angle} in
    let orientation = match orientation with
      | Plane_xy -> Rdk.Plane_generators.Circle_xy | Plane_xz -> Rdk.Plane_generators.Circle_xz
      | Plane_yz -> Rdk.Plane_generators.Circle_yz | Plane_axes -> Rdk.Plane_generators.Circle_axes {horizontal;vertical} in
    let radius_x = if radius_x_mode = Kernel_auto then None else Some radius_x
    and radius_y = if radius_y_mode = Kernel_auto then None else Some radius_y in
    Node.Private.make_geometry ?label ~operation:"circle" ~version:2 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Plane_generators.circle ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~arc ~orientation ~reverse ~center
            ?radius_x ?radius_y ~rotation ~uniform_scale ~segments ~radius () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build
end

module Grid = struct
  open Rays_math
  let counts_parameter = Param.choice ~equal:( = ) [
      "Divisions", Rdk.Plane_generators.Grid_divisions;
      "Point counts", Rdk.Plane_generators.Grid_point_counts;
    ]

  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Points", Rdk.Plane_generators.Grid_points;
      "Rows", Rdk.Plane_generators.Grid_rows;
      "Columns", Rdk.Plane_generators.Grid_columns;
      "Rows and columns", Rdk.Plane_generators.Grid_rows_and_columns;
      "Quads", Rdk.Plane_generators.Grid_quads;
      "Triangles", Rdk.Plane_generators.Grid_triangles;
      "Alternating triangles", Rdk.Plane_generators.Grid_alternating_triangles;
      "Reverse triangles", Rdk.Plane_generators.Grid_reverse_triangles;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "XY", Plane_xy;
      "XZ", Plane_xz;
      "YZ", Plane_yz; "Custom axes", Plane_axes;
    ]

  type parameters = {
    counts : Rdk.Plane_generators.grid_counts [@sop.default Rdk.Plane_generators.Grid_divisions]
      [@sop.label "Counts"] [@sop.kind counts_parameter];
    connectivity : Rdk.Plane_generators.grid_connectivity
      [@sop.default Rdk.Plane_generators.Grid_triangles]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];
    orientation : Support.plane_orientation [@sop.default Plane_xz]
      [@sop.label "Orientation"] [@sop.kind orientation_parameter];

    horizontal_x : float [@sop.default 1.] [@sop.label "Horizontal X"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    horizontal_y : float [@sop.default 0.] [@sop.label "Horizontal Y"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    horizontal_z : float [@sop.default 0.] [@sop.label "Horizontal Z"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "horizontal"];
    vertical_x : float [@sop.default 0.] [@sop.label "Vertical X"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    vertical_y : float [@sop.default 0.] [@sop.label "Vertical Y"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    vertical_z : float [@sop.default 1.] [@sop.label "Vertical Z"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "vertical"];
    width_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Width mode"] [@sop.kind kernel_mode_parameter];
    height_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Height mode"] [@sop.kind kernel_mode_parameter];
    columns : int [@sop.default 10] [@sop.label "Columns"]
      [@sop.folder "Resolution"] [@sop.min 1] [@sop.max 64]
      [@sop.hard_min 1]; [@sop.primary]
    rows : int [@sop.default 10] [@sop.label "Rows"]
      [@sop.folder "Resolution"] [@sop.min 1] [@sop.max 64]
      [@sop.hard_min 1]; [@sop.primary]
    size : float [@sop.default 1.] [@sop.label "Size"]
      [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.];
    width : float [@sop.default 1.] [@sop.label "Width"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    height : float [@sop.default 1.] [@sop.label "Height"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation : float [@sop.default 0.] [@sop.label "Rotation"]
      [@sop.folder "Transform"] [@sop.min (-3.14159)] [@sop.max 3.14159];
    uv_attribute : string [@sop.default ""] [@sop.label "UV attribute"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "grid"] [@@sop.node_label "Grid"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/grid: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.size;parameters.width;parameters.height;parameters.rotation;
          parameters.center_x;parameters.center_y;parameters.center_z;parameters.horizontal_x;
          parameters.horizontal_y;parameters.horizontal_z;parameters.vertical_x;parameters.vertical_y;parameters.vertical_z]) then
        refuse "controls must be finite";
      if parameters.size <= 0. || parameters.width <= 0. || parameters.height <= 0. then refuse "dimensions must be positive";
      let columns, rows = match parameters.counts, parameters.connectivity with
        | Rdk.Plane_generators.Grid_divisions, _ | Grid_point_counts, Grid_points -> 1,1
        | Grid_point_counts, Grid_rows -> 2,1 | Grid_point_counts, Grid_columns -> 1,2
        | Grid_point_counts, _ -> 2,2 in
      if parameters.columns < columns || parameters.rows < rows then refuse "insufficient columns/rows for connectivity";
      let extra = if parameters.counts = Rdk.Plane_generators.Grid_divisions then 1 else 0 in
      if parameters.columns > Sys.max_array_length - extra || parameters.rows > Sys.max_array_length - extra then
        refuse "output cardinality exceeds array limits";
      let u = parameters.columns + extra and v = parameters.rows + extra in
      let product a b limit = a <= limit / b in
      if not (product u v Sys.max_array_length) then refuse "output cardinality exceeds array limits";
      let points = u * v and cells = (u - 1) * (v - 1)
      and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let valid = match parameters.connectivity with
        | Rdk.Plane_generators.Grid_points -> true
        | Grid_rows -> v <= primitive_limit | Grid_columns -> u <= primitive_limit
        | Grid_rows_and_columns -> points <= Sys.max_array_length / 2 && u <= primitive_limit - v
        | Grid_quads -> cells <= Sys.max_array_length / 4 && cells <= primitive_limit
        | Grid_triangles | Grid_alternating_triangles | Grid_reverse_triangles ->
            cells <= Sys.max_array_length / 6 && cells <= primitive_limit / 2 in
      if not valid then refuse "topology cardinality exceeds array limits";
      if List.mem (String.trim parameters.uv_attribute) ["P";"N"] then refuse "UV name cannot be P or N";
      if parameters.orientation = Plane_axes then validate_plane_axes "sop/grid"
        (Rays_math.Vec3.create parameters.horizontal_x parameters.horizontal_y parameters.horizontal_z)
        (Rays_math.Vec3.create parameters.vertical_x parameters.vertical_y parameters.vertical_z)]
    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let counts = parameters.counts in
    let connectivity = parameters.connectivity in
    let orientation = parameters.orientation in
    let horizontal = Rays_math.Vec3.create parameters.horizontal_x parameters.horizontal_y parameters.horizontal_z in
    let vertical = Rays_math.Vec3.create parameters.vertical_x parameters.vertical_y parameters.vertical_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let width_mode = parameters.width_mode in
    let width = parameters.width in
    let height_mode = parameters.height_mode in
    let height = parameters.height in
    let rotation = parameters.rotation in
    let uv_attribute = parameters.uv_attribute in
    let columns = parameters.columns in
    let rows = parameters.rows in
    let size = parameters.size in
    let orientation = match orientation with
      | Plane_xy -> Rdk.Plane_generators.Grid_xy | Plane_xz -> Rdk.Plane_generators.Grid_xz
      | Plane_yz -> Rdk.Plane_generators.Grid_yz | Plane_axes -> Rdk.Plane_generators.Grid_axes {horizontal;vertical} in
    let width = if width_mode = Kernel_auto then None else Some width
    and height = if height_mode = Kernel_auto then None else Some height in
    let uv_attribute = optional_text uv_attribute in
    Node.Private.make_geometry ?label ~operation:"grid" ~version:2 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ _context _inputs ->
        match Rdk.Plane_generators.grid ~cancel:(Context.cancel_token _context)
            ~grain:(Context.grain _context) ~counts ~connectivity ~orientation
            ~center ?width ?height ~rotation ?uv_attribute ~columns ~rows ~size () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end

module Uv_sphere = struct
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Uv_sphere.Sphere_triangles;
      "Alternating triangles", Rdk.Uv_sphere.Sphere_alternating_triangles;
      "Quads", Rdk.Uv_sphere.Sphere_quads;
      "Rows", Rdk.Uv_sphere.Sphere_rows;
      "Columns", Rdk.Uv_sphere.Sphere_columns;
      "Rows and columns", Rdk.Uv_sphere.Sphere_rows_and_columns;
      "Points", Rdk.Uv_sphere.Sphere_points;
    ]

  let normals_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Uv_sphere.Sphere_no_normals;
      "Point", Rdk.Uv_sphere.Sphere_point_normals;
      "Vertex", Rdk.Uv_sphere.Sphere_vertex_normals;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "X axis", Axis_x; "Y axis", Axis_y; "Z axis", Axis_z; "Custom axis", Axis_custom]
  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Uv_sphere.Sphere_xyz; "XZY", Rdk.Uv_sphere.Sphere_xzy;
      "YXZ", Rdk.Uv_sphere.Sphere_yxz; "YZX", Rdk.Uv_sphere.Sphere_yzx;
      "ZXY", Rdk.Uv_sphere.Sphere_zxy; "ZYX", Rdk.Uv_sphere.Sphere_zyx]
  type parameters = {
    connectivity : Rdk.Uv_sphere.sphere_connectivity
      [@sop.default Rdk.Uv_sphere.Sphere_triangles]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];

    normals_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Normals mode"] [@sop.kind kernel_mode_parameter];
    normals : Rdk.Uv_sphere.sphere_normals
      [@sop.default Rdk.Uv_sphere.Sphere_point_normals]
      [@sop.label "Normals"] [@sop.kind normals_parameter];
    orientation : Support.axis_orientation
      [@sop.default Axis_y]
      [@sop.label "Pole axis"] [@sop.kind orientation_parameter];

    axis_x : float [@sop.default 0.] [@sop.label "Axis X"] [@sop.folder "Transform/Axis"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"] [@sop.folder "Transform/Axis"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"] [@sop.folder "Transform/Axis"] [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    rotation_order : Rdk.Uv_sphere.sphere_rotation_order [@sop.default Rdk.Uv_sphere.Sphere_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"] [@sop.kind rotation_order_parameter];
    base_radius : float [@sop.default 1.] [@sop.label "Base radius"] [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.];
    radius_x_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Radius X mode"]
      [@sop.folder "Size"] [@sop.kind kernel_mode_parameter];
    radius_y_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Radius Y mode"]
      [@sop.folder "Size"] [@sop.kind kernel_mode_parameter];
    radius_z_mode : Support.kernel_mode [@sop.default Kernel_explicit] [@sop.label "Radius Z mode"]
      [@sop.folder "Size"] [@sop.kind kernel_mode_parameter];
    unique_points_per_pole : bool [@sop.default false]
      [@sop.label "Unique pole points"] [@sop.folder "Topology"];
    triangular_poles : bool [@sop.default true]
      [@sop.label "Triangular poles"] [@sop.folder "Topology"];
    radius_x : float [@sop.default 1.] [@sop.label "Radius X"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "radius"] [@sop.primary]
    radius_y : float [@sop.default 1.] [@sop.label "Radius Y"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "radius"] [@sop.primary]
    radius_z : float [@sop.default 1.] [@sop.label "Radius Z"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "radius"] [@sop.primary]
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    segments : int [@sop.default 48] [@sop.label "Segments"]
      [@sop.folder "Resolution"] [@sop.min 3] [@sop.max 256]
      [@sop.hard_min 3]; [@sop.primary]
    rings : int [@sop.default 24] [@sop.label "Rings"]
      [@sop.folder "Resolution"] [@sop.min 2] [@sop.max 128]
      [@sop.hard_min 2]; [@sop.primary]
    uv_attribute : string [@sop.default "uv"] [@sop.label "UV attribute"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "uv_sphere"] [@@sop.node_label "UV Sphere"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/uv_sphere: " ^ message) in
      if parameters.segments < 3 || parameters.rings < 2 then refuse "segments/rings must be at least 3/2";
      if parameters.segments = max_int || parameters.rings = max_int then refuse "output cardinality overflows";
      if not (List.for_all Float.is_finite [parameters.base_radius;parameters.radius_x;parameters.radius_y;parameters.radius_z;
          parameters.uniform_scale;parameters.axis_x;parameters.axis_y;parameters.axis_z;parameters.center_x;parameters.center_y;
          parameters.center_z;parameters.rotation_x;parameters.rotation_y;parameters.rotation_z]) then refuse "controls must be finite";
      if parameters.base_radius <= 0. || parameters.radius_x <= 0. || parameters.radius_y <= 0.
          || parameters.radius_z <= 0. || parameters.uniform_scale <= 0. then refuse "radii and scale must be positive";
      List.iter (fun (mode,value) ->
        let value = (if mode = Kernel_auto then parameters.base_radius else value) *. parameters.uniform_scale in
        if not (Float.is_finite value) || value <= 0. then refuse "scaled radii must be finite and positive")
        [parameters.radius_x_mode,parameters.radius_x;parameters.radius_y_mode,parameters.radius_y;parameters.radius_z_mode,parameters.radius_z];
      if parameters.orientation = Axis_custom && parameters.axis_x = 0. && parameters.axis_y = 0.
          && parameters.axis_z = 0. then refuse "custom axis must be nonzero";
      let uv = String.trim parameters.uv_attribute in
      if uv = "P" || uv = "N" then refuse "UV attribute cannot be P or N";
      if parameters.connectivity = Rdk.Uv_sphere.Sphere_points && parameters.normals_mode = Kernel_explicit
          && parameters.normals = Rdk.Uv_sphere.Sphere_vertex_normals then refuse "point output cannot carry vertex normals";
      let mul left right limit =
        if left <> 0 && right > limit / left then refuse "output cardinality exceeds OCaml array limits";
        left * right
      and add left right limit =
        if right > limit - left then refuse "output cardinality exceeds OCaml array limits";
        left + right in
      let segments = parameters.segments and rings = parameters.rings in
      let point_limit = Sys.max_array_length
      and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let interior = mul (rings - 1) segments point_limit in
      let poles = if parameters.connectivity = Rdk.Uv_sphere.Sphere_rows then 0
        else if parameters.unique_points_per_pole then segments else 1 in
      ignore (add interior (mul 2 poles point_limit) point_limit);
      let middle = mul (rings - 2) segments point_limit in
      (match parameters.connectivity with
       | Rdk.Uv_sphere.Sphere_points -> ()
       | Sphere_rows -> if rings - 1 > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Sphere_columns -> ignore (mul segments (rings + 1) point_limit);
           if segments > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Sphere_rows_and_columns ->
           ignore (add interior (mul segments (rings + 1) point_limit) point_limit);
           ignore (add segments (rings - 1) primitive_limit)
       | Sphere_triangles | Sphere_alternating_triangles ->
           ignore (mul 3 (mul 2 interior primitive_limit) point_limit)
       | Sphere_quads ->
           let primitives = mul segments rings primitive_limit in
           if parameters.triangular_poles then
             ignore (add (mul 4 middle point_limit) (mul 6 segments point_limit) point_limit)
           else ignore (mul 4 primitives point_limit))]

    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let connectivity = parameters.connectivity in
    let unique_points_per_pole = parameters.unique_points_per_pole in
    let triangular_poles = parameters.triangular_poles in
    let normals_mode = parameters.normals_mode in
    let normals = parameters.normals in
    let orientation = parameters.orientation in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let uniform_scale = parameters.uniform_scale in
    let radius_x_mode = parameters.radius_x_mode in
    let radius_x = parameters.radius_x in
    let radius_y_mode = parameters.radius_y_mode in
    let radius_y = parameters.radius_y in
    let radius_z_mode = parameters.radius_z_mode in
    let radius_z = parameters.radius_z in
    let uv_attribute = parameters.uv_attribute in
    let segments = parameters.segments in
    let rings = parameters.rings in
    let base_radius = parameters.base_radius in
    let radius = base_radius in
    let radius_x = match radius_x_mode with Kernel_explicit -> Some radius_x | Kernel_auto -> None in
    let radius_y = match radius_y_mode with Kernel_explicit -> Some radius_y | Kernel_auto -> None in
    let radius_z = match radius_z_mode with Kernel_explicit -> Some radius_z | Kernel_auto -> None in
    let normals = match normals_mode with Kernel_explicit -> Some normals | Kernel_auto -> None in
    let uv_attribute = optional_text uv_attribute in
    let orientation = match orientation with
      | Axis_x -> Rdk.Uv_sphere.Sphere_x | Axis_y -> Rdk.Uv_sphere.Sphere_y
      | Axis_z -> Rdk.Uv_sphere.Sphere_z | Axis_custom -> Rdk.Uv_sphere.Sphere_axis (vec3_copy axis) in
    let center = vec3_copy center and rotation = vec3_copy rotation in
    Node.Private.make_geometry ?label ~operation:"uv_sphere" ~version:2 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Uv_sphere.run ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~connectivity ~unique_points_per_pole
            ~triangular_poles ?normals ~orientation ~center ~rotation
            ~rotation_order ~uniform_scale ?radius_x ?radius_y ?radius_z
            ?uv_attribute ~segments ~rings ~radius () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Torus = struct
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Parametric_generators.Torus_triangles;
      "Alternating triangles", Rdk.Parametric_generators.Torus_alternating_triangles;
      "Quads", Rdk.Parametric_generators.Torus_quads;
      "Rows", Rdk.Parametric_generators.Torus_rows;
      "Columns", Rdk.Parametric_generators.Torus_columns;
      "Rows and columns", Rdk.Parametric_generators.Torus_rows_and_columns;
      "Points", Rdk.Parametric_generators.Torus_points;
    ]

  let normals_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Parametric_generators.Torus_no_normals;
      "Point", Rdk.Parametric_generators.Torus_point_normals;
      "Vertex", Rdk.Parametric_generators.Torus_vertex_normals;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "X axis", Axis_x; "Y axis", Axis_y; "Z axis", Axis_z; "Custom axis", Axis_custom]
  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Parametric_generators.Torus_xyz; "XZY", Rdk.Parametric_generators.Torus_xzy;
      "YXZ", Rdk.Parametric_generators.Torus_yxz; "YZX", Rdk.Parametric_generators.Torus_yzx;
      "ZXY", Rdk.Parametric_generators.Torus_zxy; "ZYX", Rdk.Parametric_generators.Torus_zyx]
  type parameters = {
    connectivity : Rdk.Parametric_generators.torus_connectivity
      [@sop.default Rdk.Parametric_generators.Torus_triangles]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];

    normals_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Normals mode"] [@sop.kind kernel_mode_parameter];
    normals : Rdk.Parametric_generators.torus_normals
      [@sop.default Rdk.Parametric_generators.Torus_point_normals]
      [@sop.label "Normals"] [@sop.kind normals_parameter];
    orientation : Support.axis_orientation
      [@sop.default Axis_y]
      [@sop.label "Hole axis"] [@sop.kind orientation_parameter];

    axis_x : float [@sop.default 0.] [@sop.label "Axis X"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    rotation_order : Rdk.Parametric_generators.torus_rotation_order [@sop.default Rdk.Parametric_generators.Torus_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"] [@sop.kind rotation_order_parameter];
    major_radius : float [@sop.default 1.] [@sop.label "Major radius"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    minor_radius : float [@sop.default 0.25] [@sop.label "Minor radius"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 5.]
      [@sop.hard_min 0.]; [@sop.primary]
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    u_start : float [@sop.default 0.] [@sop.label "U start"]
      [@sop.folder "Arc/U"] [@sop.min (-6.283185)] [@sop.max 6.283185];
    u_end : float [@sop.default 6.283185307179586] [@sop.label "U end"]
      [@sop.folder "Arc/U"] [@sop.min (-6.283185)] [@sop.max 6.283185];
    v_start : float [@sop.default 0.] [@sop.label "V start"]
      [@sop.folder "Arc/V"] [@sop.min (-6.283185)] [@sop.max 6.283185];
    v_end : float [@sop.default 6.283185307179586] [@sop.label "V end"]
      [@sop.folder "Arc/V"] [@sop.min (-6.283185)] [@sop.max 6.283185];
    u_wrap : bool [@sop.default true] [@sop.label "Wrap U"]
      [@sop.folder "Arc/U"];
    v_wrap : bool [@sop.default true] [@sop.label "Wrap V"]
      [@sop.folder "Arc/V"];
    u_end_caps : bool [@sop.default false] [@sop.label "U end caps"]
      [@sop.folder "Caps"];
    v_end_cap : bool [@sop.default false] [@sop.label "V end cap"]
      [@sop.folder "Caps"];
    rows : int [@sop.default 48] [@sop.label "Rows"]
      [@sop.folder "Resolution"] [@sop.min 3] [@sop.max 256]
      [@sop.hard_min 2]; [@sop.primary]
    columns : int [@sop.default 24] [@sop.label "Columns"]
      [@sop.folder "Resolution"] [@sop.min 3] [@sop.max 256]
      [@sop.hard_min 2]; [@sop.primary]
    uv_attribute : string [@sop.default "uv"] [@sop.label "UV attribute"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "torus"] [@@sop.node_label "Torus"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/torus: " ^ message) in
      if parameters.rows < (if parameters.u_wrap then 3 else 2)
          || parameters.columns < (if parameters.v_wrap then 3 else 2) then refuse "counts are too small for the selected wraps";
      if not (List.for_all Float.is_finite [parameters.major_radius;parameters.minor_radius;parameters.uniform_scale;
          parameters.u_start;parameters.u_end;parameters.v_start;parameters.v_end;parameters.axis_x;parameters.axis_y;parameters.axis_z;
          parameters.center_x;parameters.center_y;parameters.center_z;parameters.rotation_x;parameters.rotation_y;parameters.rotation_z]) then
        refuse "controls must be finite";
      if parameters.major_radius <= 0. || parameters.minor_radius <= 0. || parameters.uniform_scale <= 0. then
        refuse "radii and scale must be positive";
      List.iter (fun radius -> let scaled = radius *. parameters.uniform_scale in
        if not (Float.is_finite scaled) || scaled <= 0. then refuse "scaled radii must be finite and positive")
        [parameters.major_radius;parameters.minor_radius];
      List.iter (fun span -> if not (Float.is_finite span) || span = 0. then refuse "angle spans must be finite and nonzero")
        [parameters.u_end -. parameters.u_start;parameters.v_end -. parameters.v_start];
      if parameters.orientation = Axis_custom && parameters.axis_x = 0. && parameters.axis_y = 0. && parameters.axis_z = 0. then
        refuse "custom axis must be nonzero";
      let uv = String.trim parameters.uv_attribute in
      if uv = "P" || uv = "N" then refuse "UV attribute cannot be P or N";
      if parameters.connectivity = Rdk.Parametric_generators.Torus_points && parameters.normals_mode = Kernel_explicit
          && parameters.normals = Rdk.Parametric_generators.Torus_vertex_normals then refuse "point output cannot carry vertex normals";
      let polygon = match parameters.connectivity with
        | Rdk.Parametric_generators.Torus_triangles | Torus_alternating_triangles | Torus_quads -> true | _ -> false in
      if (parameters.u_end_caps || parameters.v_end_cap) && not polygon then refuse "caps require polygon connectivity";
      if parameters.u_end_caps && (parameters.u_wrap || parameters.columns < 3) then
        refuse "U caps require an open U sweep and at least three columns";
      if parameters.v_end_cap then begin
        if parameters.v_wrap then refuse "V cap requires an open V sweep";
        let chord = Float.max (abs_float (cos parameters.v_end -. cos parameters.v_start))
            (abs_float (sin parameters.v_end -. sin parameters.v_start)) in
        if chord <= 64. *. Float.epsilon then refuse "V cap endpoints are geometrically coincident"
      end;
      let mul left right limit =
        if left <> 0 && right > limit / left then refuse "output cardinality exceeds OCaml array limits";
        left * right
      and add left right limit =
        if right > limit - left then refuse "output cardinality exceeds OCaml array limits";
        left + right in
      let point_limit = Sys.max_array_length and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let rows = parameters.rows and columns = parameters.columns in
      let points = mul rows columns point_limit in
      (match parameters.connectivity with
       | Rdk.Parametric_generators.Torus_points -> ()
       | Torus_rows -> if columns > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Torus_columns -> if rows > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Torus_rows_and_columns -> ignore (mul 2 points point_limit);ignore (add rows columns primitive_limit)
       | Torus_triangles | Torus_alternating_triangles | Torus_quads ->
           let u_cells = if parameters.u_wrap then rows else rows - 1
           and v_cells = if parameters.v_wrap then columns else columns - 1 in
           let cells = mul u_cells v_cells primitive_limit in
           let per_primitive,per_vertex = if parameters.connectivity = Torus_quads then 1,4 else 2,6 in
           let primitives = mul cells per_primitive primitive_limit and vertices = mul cells per_vertex point_limit in
           let primitives = add primitives (if parameters.v_end_cap then mul u_cells per_primitive primitive_limit else 0) primitive_limit
           and vertices = add vertices (if parameters.v_end_cap then mul u_cells per_vertex point_limit else 0) point_limit in
           ignore (add primitives (if parameters.u_end_caps then 2 else 0) primitive_limit);
           ignore (add vertices (if parameters.u_end_caps then mul 2 columns point_limit else 0) point_limit))]

    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let connectivity = parameters.connectivity in
    let normals_mode = parameters.normals_mode in
    let normals = parameters.normals in
    let orientation = parameters.orientation in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let uniform_scale = parameters.uniform_scale in
    let u_start = parameters.u_start in
    let u_end = parameters.u_end in
    let v_start = parameters.v_start in
    let v_end = parameters.v_end in
    let u_wrap = parameters.u_wrap in
    let v_wrap = parameters.v_wrap in
    let u_end_caps = parameters.u_end_caps in
    let v_end_cap = parameters.v_end_cap in
    let uv_attribute = parameters.uv_attribute in
    let rows = parameters.rows in
    let columns = parameters.columns in
    let major_radius = parameters.major_radius in
    let minor_radius = parameters.minor_radius in
    let normals = match normals_mode with Kernel_explicit -> Some normals | Kernel_auto -> None in
    let uv_attribute = optional_text uv_attribute in
    let orientation = match orientation with
      | Axis_x -> Rdk.Parametric_generators.Torus_x | Axis_y -> Rdk.Parametric_generators.Torus_y
      | Axis_z -> Rdk.Parametric_generators.Torus_z | Axis_custom -> Rdk.Parametric_generators.Torus_axis (vec3_copy axis) in
    let center = vec3_copy center and rotation = vec3_copy rotation in
    Node.Private.make_geometry ?label ~operation:"torus" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Parametric_generators.torus ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~connectivity ?normals ~orientation
            ~center ~rotation ~rotation_order ~uniform_scale ~u_start ~u_end
            ~v_start ~v_end ~u_wrap ~v_wrap ~u_end_caps ~v_end_cap ?uv_attribute
            ~rows ~columns ~major_radius ~minor_radius () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Tube = struct
  let connectivity_parameter = Param.choice ~equal:( = ) [
      "Triangles", Rdk.Parametric_generators.Tube_triangles;
      "Alternating triangles", Rdk.Parametric_generators.Tube_alternating_triangles;
      "Quads", Rdk.Parametric_generators.Tube_quads;
      "Rows", Rdk.Parametric_generators.Tube_rows;
      "Columns", Rdk.Parametric_generators.Tube_columns;
      "Rows and columns", Rdk.Parametric_generators.Tube_rows_and_columns;
      "Points", Rdk.Parametric_generators.Tube_points;
    ]

  let normals_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Parametric_generators.Tube_no_normals;
      "Point", Rdk.Parametric_generators.Tube_point_normals;
      "Vertex", Rdk.Parametric_generators.Tube_vertex_normals;
    ]

  let orientation_parameter = Param.choice ~equal:( = ) [
      "X axis", Axis_x; "Y axis", Axis_y; "Z axis", Axis_z; "Custom axis", Axis_custom]
  let rotation_order_parameter = Param.choice ~equal:( = ) [
      "XYZ", Rdk.Parametric_generators.Tube_xyz; "XZY", Rdk.Parametric_generators.Tube_xzy;
      "YXZ", Rdk.Parametric_generators.Tube_yxz; "YZX", Rdk.Parametric_generators.Tube_yzx;
      "ZXY", Rdk.Parametric_generators.Tube_zxy; "ZYX", Rdk.Parametric_generators.Tube_zyx]
  type parameters = {
    connectivity : Rdk.Parametric_generators.tube_connectivity
      [@sop.default Rdk.Parametric_generators.Tube_triangles]
      [@sop.label "Connectivity"] [@sop.kind connectivity_parameter];

    normals_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Normals mode"] [@sop.kind kernel_mode_parameter];
    normals : Rdk.Parametric_generators.tube_normals
      [@sop.default Rdk.Parametric_generators.Tube_point_normals]
      [@sop.label "Normals"] [@sop.kind normals_parameter];
    orientation : Support.axis_orientation
      [@sop.default Axis_y]
      [@sop.label "Primary axis"] [@sop.kind orientation_parameter];

    axis_x : float [@sop.default 0.] [@sop.label "Axis X"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_y : float [@sop.default 1.] [@sop.label "Axis Y"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    axis_z : float [@sop.default 0.] [@sop.label "Axis Z"] [@sop.folder "Transform/Axis"]
      [@sop.min (-1.)] [@sop.max 1.] [@sop.vec3 "axis"];
    rotation_order : Rdk.Parametric_generators.tube_rotation_order [@sop.default Rdk.Parametric_generators.Tube_xyz]
      [@sop.label "Rotation order"] [@sop.folder "Transform/Rotate"] [@sop.kind rotation_order_parameter];
    top_radius : float [@sop.default 1.] [@sop.label "Top radius"]
      [@sop.folder "Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    bottom_radius : float [@sop.default 1.] [@sop.label "Bottom radius"]
      [@sop.folder "Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    height : float [@sop.default 2.] [@sop.label "Height"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.primary]
    radius_scale : float [@sop.default 1.] [@sop.label "Radius scale"]
      [@sop.folder "Size"] [@sop.min 0.01] [@sop.max 10.]
      [@sop.hard_min 0.];
    end_caps : bool [@sop.default true] [@sop.label "End caps"]
      [@sop.folder "Caps"];
    consolidate_cap_points : bool [@sop.default false]
      [@sop.label "Consolidate cap points"] [@sop.folder "Caps"];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Transform/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    rotation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rotation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Transform/Rotate"] [@sop.min (-3.14159)]
      [@sop.max 3.14159]; [@sop.vec3 "rotation"]
    rows : int [@sop.default 2] [@sop.label "Rows"]
      [@sop.folder "Resolution"] [@sop.min 2] [@sop.max 128]
      [@sop.hard_min 2];
    columns : int [@sop.default 32] [@sop.label "Columns"]
      [@sop.folder "Resolution"] [@sop.min 3] [@sop.max 256]
      [@sop.hard_min 3];
    uv_attribute : string [@sop.default "uv"] [@sop.label "UV attribute"]
      [@sop.folder "Attributes"];
    cap_group : string [@sop.default "caps"] [@sop.label "Cap group"]
      [@sop.folder "Attributes"];
  } [@@sop.node_key "tube"] [@@sop.node_label "Tube"]
    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/tube: " ^ message) in
      if parameters.rows < 2 || parameters.columns < 3 then refuse "rows/columns must be at least 2/3";
      if not (List.for_all Float.is_finite [parameters.top_radius;parameters.bottom_radius;parameters.radius_scale;parameters.height;
          parameters.axis_x;parameters.axis_y;parameters.axis_z;parameters.center_x;parameters.center_y;parameters.center_z;
          parameters.rotation_x;parameters.rotation_y;parameters.rotation_z]) then refuse "controls must be finite";
      let top = parameters.top_radius *. parameters.radius_scale and bottom = parameters.bottom_radius *. parameters.radius_scale in
      if parameters.top_radius < 0. || parameters.bottom_radius < 0. || parameters.radius_scale <= 0.
          || not (Float.is_finite top && Float.is_finite bottom) || (top <= 0. && bottom <= 0.) then
        refuse "radii must be nonnegative with at least one positive scaled radius";
      if parameters.height <= 0. then refuse "height must be positive";
      if parameters.orientation = Axis_custom && parameters.axis_x = 0. && parameters.axis_y = 0. && parameters.axis_z = 0. then
        refuse "custom axis must be nonzero";
      let uv = String.trim parameters.uv_attribute in
      if uv = "P" || uv = "N" then refuse "UV attribute cannot be P or N";
      if parameters.connectivity = Rdk.Parametric_generators.Tube_points && parameters.normals_mode = Kernel_explicit
          && parameters.normals = Rdk.Parametric_generators.Tube_vertex_normals then refuse "point output cannot carry vertex normals";
      let polygon = match parameters.connectivity with
        | Rdk.Parametric_generators.Tube_triangles | Tube_alternating_triangles | Tube_quads -> true | _ -> false in
      if parameters.end_caps && not polygon then refuse "caps require polygon connectivity";
      if not parameters.end_caps && String.trim parameters.cap_group <> "" then refuse "cap group requires end caps";
      let mul left right limit =
        if left <> 0 && right > limit / left then refuse "output cardinality exceeds OCaml array limits";
        left * right
      and add left right limit =
        if right > limit - left then refuse "output cardinality exceeds OCaml array limits";
        left + right in
      let point_limit = Sys.max_array_length and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let rows = parameters.rows and columns = parameters.columns in
      let lattice = mul rows columns point_limit in
      let tips = (if top = 0. then 1 else 0) + (if bottom = 0. then 1 else 0) in
      let caps = if parameters.end_caps then 2 - tips else 0 in
      let ring_points = lattice - tips * (columns - 1) in
      ignore (add ring_points (if parameters.consolidate_cap_points then 0 else mul caps columns point_limit) point_limit);
      let ring_curves = rows - tips in
      (match parameters.connectivity with
       | Rdk.Parametric_generators.Tube_points -> ()
       | Tube_rows -> if columns > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Tube_columns -> ignore (mul ring_curves columns point_limit);
           if ring_curves > primitive_limit then refuse "primitive cardinality exceeds OCaml array limits"
       | Tube_rows_and_columns -> ignore (add lattice (mul ring_curves columns point_limit) point_limit);
           ignore (add columns ring_curves primitive_limit)
       | Tube_triangles | Tube_alternating_triangles | Tube_quads ->
           let cells = mul (rows - 1 - tips) columns primitive_limit in
           let per_primitive,per_vertex = if parameters.connectivity = Tube_quads then 1,4 else 2,6 in
           let tip_primitives = mul tips columns primitive_limit in
           let primitives = add (mul cells per_primitive primitive_limit) tip_primitives primitive_limit
           and vertices = add (mul cells per_vertex point_limit) (mul (mul tips columns point_limit) 3 point_limit) point_limit in
           ignore (add primitives caps primitive_limit);
           ignore (add vertices (mul caps columns point_limit) point_limit))]

    [@@sop.node_category "Create/Primitive"] [@@sop.node_inputs 0]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters ->
    let label = Some label in
    let connectivity = parameters.connectivity in
    let end_caps = parameters.end_caps in
    let consolidate_cap_points = parameters.consolidate_cap_points in
    let normals_mode = parameters.normals_mode in
    let normals = parameters.normals in
    let orientation = parameters.orientation in
    let axis = Rays_math.Vec3.create parameters.axis_x parameters.axis_y parameters.axis_z in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let rotation = Rays_math.Vec3.create parameters.rotation_x parameters.rotation_y parameters.rotation_z in
    let rotation_order = parameters.rotation_order in
    let radius_scale = parameters.radius_scale in
    let uv_attribute = parameters.uv_attribute in
    let cap_group = parameters.cap_group in
    let rows = parameters.rows in
    let columns = parameters.columns in
    let top_radius = parameters.top_radius in
    let bottom_radius = parameters.bottom_radius in
    let height = parameters.height in
    let normals = match normals_mode with Kernel_explicit -> Some normals | Kernel_auto -> None in
    let uv_attribute = optional_text uv_attribute and cap_group = optional_text cap_group in
    let orientation = match orientation with
      | Axis_x -> Rdk.Parametric_generators.Tube_x | Axis_y -> Rdk.Parametric_generators.Tube_y
      | Axis_z -> Rdk.Parametric_generators.Tube_z | Axis_custom -> Rdk.Parametric_generators.Tube_axis (vec3_copy axis) in
    let center = vec3_copy center and rotation = vec3_copy rotation in
    Node.Private.make_geometry ?label ~operation:"tube" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[||] (fun ~node_id:_ context _inputs ->
        match Rdk.Parametric_generators.tube ~cancel:(Context.cancel_token context)
            ~grain:(Context.grain context) ~connectivity ~end_caps
            ~consolidate_cap_points ?normals ~orientation ~center ~rotation
            ~rotation_order ~radius_scale ?uv_attribute ?cap_group ~rows ~columns
            ~top_radius ~bottom_radius ~height () with
        | Ok geometry -> cooked geometry
        | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Mountain = struct
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Group"];
    direction_attribute : string [@sop.default ""]
      [@sop.label "Direction attribute"];
    mask_attribute : string [@sop.default ""] [@sop.label "Mask attribute"];
    height_attribute : string [@sop.default ""] [@sop.label "Height attribute"];

    seed_mode : Support.kernel_mode [@sop.default Kernel_explicit]
      [@sop.label "Seed mode"] [@sop.kind kernel_mode_parameter];
    normalize_direction : bool [@sop.default true] [@sop.label "Normalize direction"];
    offset_x : float [@sop.default 0.] [@sop.label "Offset X"] [@sop.folder "Offset"]
      [@sop.min (-10.)] [@sop.max 10.] [@sop.vec3 "offset"];
    offset_y : float [@sop.default 0.] [@sop.label "Offset Y"] [@sop.folder "Offset"]
      [@sop.min (-10.)] [@sop.max 10.] [@sop.vec3 "offset"];
    offset_z : float [@sop.default 0.] [@sop.label "Offset Z"] [@sop.folder "Offset"]
      [@sop.min (-10.)] [@sop.max 10.] [@sop.vec3 "offset"];
    seed : int [@sop.default 0] [@sop.label "Seed"] [@sop.min 0]
      [@sop.max 9999];
    height : float [@sop.default 1.] [@sop.label "Height"]
      [@sop.min 0.] [@sop.max 2.] [@sop.hard_min 0.];
    frequency_x : float [@sop.default 1.] [@sop.label "Frequency X"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    frequency_y : float [@sop.default 1.] [@sop.label "Frequency Y"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    frequency_z : float [@sop.default 1.] [@sop.label "Frequency Z"]
      [@sop.folder "Frequency"] [@sop.min 0.01] [@sop.max 4.]
      [@sop.hard_min 0.]; [@sop.vec3 "frequency"]
    octaves : int [@sop.default 4] [@sop.label "Octaves"]
      [@sop.folder "Fractal"] [@sop.min 1] [@sop.max 8]
      [@sop.hard_min 1];
    lacunarity : float [@sop.default 2.] [@sop.label "Lacunarity"]
      [@sop.folder "Fractal"] [@sop.min 1.] [@sop.max 4.]
      [@sop.hard_min 0.];
    roughness : float [@sop.default 0.5] [@sop.label "Roughness"]
      [@sop.folder "Fractal"] [@sop.min 0.] [@sop.max 1.]
      [@sop.hard_min 0.];
    recompute_normals : bool [@sop.default false]
      [@sop.label "Recompute normals"];
  } [@@sop.node_key "mountain"] [@@sop.node_label "Mountain"]
    [@@sop.node_facts {elementwise = Node.Points;
      reads = List.filter (fun name -> String.trim name <> "")
        ["P"; (if String.trim parameters.direction_attribute = "" then "N" else parameters.direction_attribute);
          parameters.mask_attribute; parameters.height_attribute];
      writes = List.filter (fun name -> String.trim name <> "")
        ["P"; "N"; parameters.height_attribute];
      topology = Node.Preserved; exact = false}]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/mountain: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.height;parameters.frequency_x;parameters.frequency_y;parameters.frequency_z;
          parameters.offset_x;parameters.offset_y;parameters.offset_z;parameters.lacunarity;parameters.roughness]) then
        refuse "numeric controls must be finite";
      if parameters.height < 0. || parameters.frequency_x < 0. || parameters.frequency_y < 0. || parameters.frequency_z < 0. then
        refuse "height and frequency must be nonnegative";
      if parameters.octaves < 1 || parameters.octaves > 64 then refuse "octaves must be within [1, 64]";
      if parameters.lacunarity <= 0. then refuse "lacunarity must be positive";
      if parameters.roughness < 0. || parameters.roughness > 1. then refuse "roughness must be within [0, 1]";
      if String.trim parameters.height_attribute = "P" then refuse "height attribute cannot be P"]
    [@@sop.node_category "Deform"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let seed_mode = parameters.seed_mode in
    let seed = parameters.seed in
    let direction_attribute = parameters.direction_attribute in
    let normalize_direction = parameters.normalize_direction in
    let mask_attribute = parameters.mask_attribute in
    let height = parameters.height in
    let frequency = Rays_math.Vec3.create parameters.frequency_x parameters.frequency_y parameters.frequency_z in
    let offset = Rays_math.Vec3.create parameters.offset_x parameters.offset_y parameters.offset_z in
    let octaves = parameters.octaves in
    let lacunarity = parameters.lacunarity in
    let roughness = parameters.roughness in
    let height_attribute = parameters.height_attribute in
    let recompute_normals = parameters.recompute_normals in
    let group = optional_text group and direction_attribute = optional_text direction_attribute
    and mask_attribute = optional_text mask_attribute and height_attribute = optional_text height_attribute in
    let seed = match seed_mode with Kernel_explicit -> Some seed | Kernel_auto -> None in
    let frequency = vec3_copy frequency and offset = vec3_copy offset in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("mountain:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"mountain" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let selection = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name inputs.(0) with
               | Some group -> Ok (Some (Rdk.Transform_ops.Selected_points group))
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "mountain could not find point group %S" name))) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Option.value ~default:(mixed_seed context identity) seed in
            match Rdk.Deform.mountain ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?selection ?direction_attribute
                ~normalize_direction ?mask_attribute ~seed ~height ~frequency
                ~offset ~octaves ~lacunarity ~roughness ?height_attribute
                ~recompute_normals inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build

end

module Extract_point_from_curve = struct
  let cut_parameter = Param.choice ~equal:( = ) [
      "Constant", Extract_point_constant; "Primitive attribute", Extract_point_primitive_attribute;
      "Current time", Extract_point_current_time;
    ]
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    distance_attribute : string [@sop.default "distance"]
      [@sop.label "Distance attribute"];
    cut : Support.extract_point_cut [@sop.default Extract_point_constant] [@sop.label "Cut value"]
      [@sop.kind cut_parameter];
    constant : float [@sop.default 0.] [@sop.label "Constant"]
      [@sop.folder "Cut"] [@sop.min (-10.)] [@sop.max 10.];
    primitive_attribute : string [@sop.default "cut"]
      [@sop.label "Primitive attribute"] [@sop.folder "Cut"];
    point_attributes : string [@sop.default "P"]
      [@sop.label "Point attributes"] [@sop.folder "Transfer"];
    copy_primitive_attributes : bool [@sop.default false]
      [@sop.label "Copy primitive attributes"] [@sop.folder "Transfer"];
    primitive_attributes : string [@sop.default "*"]
      [@sop.label "Primitive attributes"] [@sop.folder "Transfer"];
    curve_u_attribute : string [@sop.default ""] [@sop.label "Curve U"]
      [@sop.folder "Output"];
    number_cuts_attribute : string [@sop.default ""]
      [@sop.label "Number of cuts"] [@sop.folder "Output"];
    curve_number_attribute : string [@sop.default ""]
      [@sop.label "Curve number"] [@sop.folder "Output"];
  } [@@sop.node_key "extract_point_from_curve"]
    [@@sop.node_label "Extract Point from Curve"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/extract_point_from_curve: " ^ message) in
      let valid name = String.trim name <> "" && String.trim name <> "P" in
      if not (valid parameters.distance_attribute) then refuse "distance attribute must be nonempty and not P";
      if not (Float.is_finite parameters.constant) then refuse "constant must be finite";
      if parameters.cut = Extract_point_primitive_attribute && not (valid parameters.primitive_attribute) then
        refuse "cut attribute must be nonempty and not P";
      let names = [parameters.curve_u_attribute;parameters.number_cuts_attribute;parameters.curve_number_attribute]
        |> List.map String.trim |> List.filter ((<>) "") in
      if List.exists ((=) "P") names then refuse "output names cannot be P";
      if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "output names must be distinct";
      validate_parameter_pattern "sop/extract_point_from_curve" parameters.point_attributes;
      validate_parameter_pattern "sop/extract_point_from_curve" parameters.primitive_attributes]
    [@@sop.node_category "Create/Points"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let cut = parameters.cut in
    let constant = parameters.constant in
    let primitive_attribute = parameters.primitive_attribute in
    let point_attributes = parameters.point_attributes in
    let copy_primitive_attributes = parameters.copy_primitive_attributes in
    let primitive_attributes = parameters.primitive_attributes in
    let curve_u_attribute = parameters.curve_u_attribute in
    let number_cuts_attribute = parameters.number_cuts_attribute in
    let curve_number_attribute = parameters.curve_number_attribute in
    let distance_attribute = parameters.distance_attribute in
    let group = optional_text group and curve_u_attribute = optional_text curve_u_attribute
    and number_cuts_attribute = optional_text number_cuts_attribute and curve_number_attribute = optional_text curve_number_attribute in
    let dependencies = match cut with
      | Extract_point_current_time ->
          Context.Dependencies.one Context.Dependencies.Time
      | Extract_point_constant | Extract_point_primitive_attribute ->
          Context.Dependencies.static in
    Node.Private.make_geometry ?label ~operation:"extract_point_from_curve" ~version:1
      ~parameters:"" ~cook_mode:Node.Generic ~dependencies ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        let geometry = inputs.(0) in
        match resolve_optional_primitive_group "extract_point_from_curve" group
            geometry with
        | Error error -> Error error
        | Ok primitives ->
            let cut = match cut with
              | Extract_point_constant ->
                  Rdk.Curve_topology.Extract_cut_constant constant
              | Extract_point_primitive_attribute ->
                  Rdk.Curve_topology.Extract_cut_primitive_attribute primitive_attribute
              | Extract_point_current_time ->
                  Rdk.Curve_topology.Extract_cut_constant (Context.time context) in
            match Rdk.Curve_topology.extract_point_from_curve
                ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?primitives ~cut ~point_attributes
                ~copy_primitive_attributes ~primitive_attributes
                ?curve_u_attribute ?number_cuts_attribute ?curve_number_attribute
                ~distance_attribute geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Point_generate_from_input = struct
  let mode_parameter = Param.choice ~equal:( = ) [
      "Total count", Point_generate_total; "Per point", Point_generate_per_point;
      "Probability attribute", Point_generate_probability;
    ]
  type parameters = {
    mode : Support.point_generation_mode [@sop.default Point_generate_per_point] [@sop.label "Generation mode"]
      [@sop.kind mode_parameter];
    group : string [@sop.default ""] [@sop.label "Point group"];
    keep_input : bool [@sop.default false] [@sop.label "Keep input"];
    total : int [@sop.default 100] [@sop.label "Total points"]
      [@sop.folder "Generation"] [@sop.min 0] [@sop.max 1000000]
      [@sop.hard_min 0];
    points_per_point : float [@sop.default 1.] [@sop.label "Points per point"]
      [@sop.folder "Generation"] [@sop.min 0.] [@sop.max 1000.]
      [@sop.hard_min 0.];
    scale_attribute : string [@sop.default ""] [@sop.label "Count scale"]
      [@sop.folder "Generation"];
    probability_attribute : string [@sop.default "probability"]
      [@sop.label "Probability attribute"] [@sop.folder "Generation"];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Random"];
    seed : int [@sop.default 0] [@sop.label "Seed"]
      [@sop.folder "Random"] [@sop.min 0] [@sop.max 9999];
    generated_group : string [@sop.default ""] [@sop.label "Generated group"]
      [@sop.folder "Output"];
    source_point_attribute : string [@sop.default "sourcepoint"]
      [@sop.label "Source point"] [@sop.folder "Output"];
    source_index_attribute : string [@sop.default "sourceindex"]
      [@sop.label "Source index"] [@sop.folder "Output"];
    copy_point_attributes : string [@sop.default "*"]
      [@sop.label "Point attributes"] [@sop.folder "Transfer"];
    copy_detail_attributes : string [@sop.default ""]
      [@sop.label "Detail attributes"] [@sop.folder "Transfer"];
  } [@@sop.node_key "point_generate"] [@@sop.node_label "Point Generate"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/point_generate: " ^ message) in
      if parameters.total < 0 || parameters.total > Sys.max_array_length then refuse "total exceeds point array limits";
      if not (Float.is_finite parameters.points_per_point) || parameters.points_per_point < 0.
          || parameters.points_per_point > float_of_int Sys.max_array_length then refuse "invalid points per point";
      List.iter (fun name -> if String.trim name = "" || name = "P" then refuse "metadata names must be nonblank and not P")
        [parameters.source_point_attribute;parameters.source_index_attribute];
      if parameters.source_point_attribute = parameters.source_index_attribute then refuse "metadata names must differ";
      if parameters.mode = Point_generate_probability && String.trim parameters.probability_attribute = "" then
        refuse "probability attribute must be nonblank";
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "sop/point_generate" pattern)
        [parameters.copy_point_attributes;parameters.copy_detail_attributes]]
    [@@sop.node_category "Create/Points"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let group = parameters.group in
    let keep_input = parameters.keep_input in
    let context_seed = parameters.context_seed in
    let seed = parameters.seed in
    let generated_group = parameters.generated_group in
    let source_point_attribute = parameters.source_point_attribute in
    let source_index_attribute = parameters.source_index_attribute in
    let copy_point_attributes = parameters.copy_point_attributes in
    let copy_detail_attributes = parameters.copy_detail_attributes in
    let mode = parameters.mode in
    let total = parameters.total in
    let points_per_point = parameters.points_per_point in
    let scale_attribute = parameters.scale_attribute in
    let probability_attribute = parameters.probability_attribute in
    let group = optional_text group and generated_group = optional_text generated_group in
    let seed = if context_seed then None else Some seed in
    let mode = match mode with
      | Point_generate_total -> Rdk.Point_generate.Generate_total total
      | Point_generate_per_point -> Rdk.Point_generate.Generate_per_point {points_per_point;scale_attribute=optional_text scale_attribute}
      | Point_generate_probability -> Rdk.Point_generate.Generate_probability {attribute=probability_attribute} in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("point_generate:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"point_generate" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let geometry = inputs.(0) in
        let points = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
               | Some value -> Ok (Some value)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf
                      "point_generate could not find point group %S" name))) in
        match points with
        | Error error -> Error error
        | Ok points ->
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Option.value ~default:(mixed_seed context identity) seed in
            match Rdk.Point_generate.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points ~keep_input ~seed:(Rays_math.Rand.seed seed)
                ?generated_group ~source_point_attribute ~source_index_attribute
                ~copy_point_attributes ~copy_detail_attributes ~mode geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
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

module Scatter = struct
  type parameters = {
    count : int [@sop.default 100] [@sop.label "Count"] [@sop.min 1]
      [@sop.max 100000] [@sop.hard_min 0];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Random"];
    seed : int [@sop.default 0] [@sop.label "Seed"]
      [@sop.folder "Random"] [@sop.min 0] [@sop.max 9999];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    use_density : bool [@sop.default false] [@sop.label "Use density"]
      [@sop.folder "Density"];
    density_owner : Rdk.Attribute.owner [@sop.default Rdk.Attribute.Primitive]
      [@sop.label "Owner"] [@sop.folder "Density"]
      [@sop.kind attribute_owner_parameter];
    density_attribute : string [@sop.default "density"]
      [@sop.label "Attribute"] [@sop.folder "Density"];
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
    source_primitive_attribute : string [@sop.default ""]
      [@sop.label "Source primitive"] [@sop.folder "Provenance"];
    source_vertex_numbers_attribute : string [@sop.default ""]
      [@sop.label "Source vertex numbers"] [@sop.folder "Provenance"];
    source_vertex_weights_attribute : string [@sop.default ""]
      [@sop.label "Source vertex weights"] [@sop.folder "Provenance"];
  } [@@sop.node_key "scatter"] [@@sop.node_label "Scatter"]
    [@@sop.node_facts {elementwise = Node.None; reads = ["*"]; writes = ["*"];
      topology = Node.Changed; exact = true}]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/scatter: " ^ message) in
      if parameters.count < 0 || parameters.count > Sys.max_array_length then refuse "count exceeds array limits";
      if parameters.use_density && (String.trim parameters.density_attribute = "" || parameters.density_attribute = "P") then
        refuse "density requires a nonblank scalar attribute";
      let patterns = [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern;parameters.detail_pattern] in
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "scatter" pattern) patterns;
      if parameters.match_groups && List.for_all (fun pattern -> String.trim pattern = "")
          [parameters.point_pattern;parameters.vertex_pattern;parameters.primitive_pattern] then
        refuse "group interpolation requires an attribute/group pattern";
      let numbers = optional_text parameters.source_vertex_numbers_attribute
      and weights = optional_text parameters.source_vertex_weights_attribute in
      if Option.is_some numbers <> Option.is_some weights then refuse "vertex numbers and weights must be requested together";
      let names = List.filter_map optional_text [parameters.source_primitive_attribute;
          parameters.source_vertex_numbers_attribute;parameters.source_vertex_weights_attribute] in
      if List.mem "P" names then refuse "provenance must not replace P";
      if List.length names <> List.length (List.sort_uniq String.compare names) then refuse "provenance names must be distinct";
      if (Option.is_some numbers || List.exists (fun pattern -> String.trim pattern <> "") patterns)
          && parameters.count > Sys.max_array_length / 3 then refuse "provenance cardinality exceeds array limits"]
    [@@sop.node_category "Create/Points"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let count = parameters.count in
    let context_seed = parameters.context_seed in
    let seed = parameters.seed in
    let group = parameters.group in
    let use_density = parameters.use_density in
    let density_owner = parameters.density_owner in
    let density_attribute = parameters.density_attribute in
    let point_pattern = parameters.point_pattern in
    let vertex_pattern = parameters.vertex_pattern in
    let primitive_pattern = parameters.primitive_pattern in
    let detail_pattern = parameters.detail_pattern in
    let match_groups = parameters.match_groups in
    let source_primitive_attribute = parameters.source_primitive_attribute in
    let source_vertex_numbers_attribute = parameters.source_vertex_numbers_attribute in
    let source_vertex_weights_attribute = parameters.source_vertex_weights_attribute in
    let seed = if context_seed then None else Some seed in
    let group = optional_text group and point_pattern = optional_text point_pattern
    and vertex_pattern = optional_text vertex_pattern and primitive_pattern = optional_text primitive_pattern
    and detail_pattern = optional_text detail_pattern and source_primitive_attribute = optional_text source_primitive_attribute
    and source_vertex_numbers_attribute = optional_text source_vertex_numbers_attribute
    and source_vertex_weights_attribute = optional_text source_vertex_weights_attribute in
    let density = if use_density then Some (Rdk.Scatter.density ~owner:density_owner density_attribute) else None in
    let dependencies = match seed with
      | Some _ -> Context.Dependencies.static
      | None -> Context.Dependencies.one Context.Dependencies.Seed in
    let density = Option.map (fun (value : Rdk.Scatter.density) ->
      { Rdk.Scatter.density_owner = value.density_owner;
        density_attribute = String.sub value.density_attribute 0
            (String.length value.density_attribute) }) density in
    let stable_identity = Option.map (fun label ->
      stable_string_hash ("scatter:" ^ label)) label in
    Node.Private.make_geometry ?label ~operation:"scatter" ~version:2 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs:[|input|]
      (fun ~node_id context inputs ->
        let identity = Option.value ~default:(Int64.of_int node_id) stable_identity in
        let seed = Option.value ~default:(mixed_seed context identity) seed in
        let geometry = inputs.(0) in
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "scatter could not find primitive group %S" name))) in
        match primitives with
        | Error _ as error -> error
        | Ok primitives ->
            match Rdk.Scatter.run ~grain:(Context.grain context) ~count ~seed
                ~cancel:(Context.cancel_token context) ?primitives ?density
                ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
                ~match_groups ?source_primitive_attribute
                ?source_vertex_numbers_attribute ?source_vertex_weights_attribute
                geometry with
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
                     ~grain:(Context.grain context) ?selection ?collision_primitives
                     ~method_ ~direction ~direction_mode ~surface_hit ~samples
                     ~jitter_scale ~seed ~combine ~min_distance
                     ?max_distance ~tolerance ~scale ~lift ?distance_attribute
                     ?primitive_attribute ?source_vertex_numbers_attribute
                     ?source_vertex_weights_attribute ?hit_group ?normal_attribute
                     ?point_pattern ?vertex_pattern ?primitive_pattern
                     ?detail_pattern ~match_groups ~source:inputs.(0)
                     ~collision:inputs.(1) () with
                 | Ok geometry -> cooked geometry
                 | Error error -> structured_rdk_error error))
  )
  let factory = parameters_factory build
end

module Point_replicate = struct
  let shape_parameter = Param.choice ~equal:( = ) [
      "Point", Rdk.Point_replication.Replicate_point; "Box", Rdk.Point_replication.Replicate_box;
      "Sphere", Rdk.Point_replication.Replicate_sphere; "Disk", Rdk.Point_replication.Replicate_disk;
      "Line", Rdk.Point_replication.Replicate_line; "Custom", Rdk.Point_replication.Replicate_custom;
    ]
  let velocity_parameter = Param.choice ~equal:( = ) [
      "None", Rdk.Point_replication.Replicate_no_velocity_stretch;
      "Scaled velocity", Rdk.Point_replication.Replicate_scaled_velocity;
      "Velocity only", Rdk.Point_replication.Replicate_velocity_only;
    ]
  type parameters = {
    group : string [@sop.default ""] [@sop.label "Point group"];
    keep_input : bool [@sop.default false] [@sop.label "Keep input"];
    points_per_point : float [@sop.default 10.] [@sop.label "Points per point"]
      [@sop.min 0.] [@sop.max 10000.] [@sop.hard_min 0.];
    scale_attribute : string [@sop.default ""] [@sop.label "Count scale"];
    context_seed : bool [@sop.default false] [@sop.label "Use context seed"]
      [@sop.folder "Random"];
    seed : int [@sop.default 0] [@sop.label "Seed"]
      [@sop.folder "Random"] [@sop.min 0] [@sop.max 9999];
    id_attribute : string [@sop.default "id"] [@sop.label "ID attribute"]
      [@sop.folder "Random"];
    shape : Rdk.Point_replication.shape [@sop.default Rdk.Point_replication.Replicate_sphere]
      [@sop.label "Shape"] [@sop.folder "Shape"] [@sop.kind shape_parameter];
    center_x : float [@sop.default 0.] [@sop.label "Center X"]
      [@sop.folder "Shape/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_y : float [@sop.default 0.] [@sop.label "Center Y"]
      [@sop.folder "Shape/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    center_z : float [@sop.default 0.] [@sop.label "Center Z"]
      [@sop.folder "Shape/Center"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "center"]
    size_x : float [@sop.default 1.] [@sop.label "Size X"]
      [@sop.folder "Shape/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    size_y : float [@sop.default 1.] [@sop.label "Size Y"]
      [@sop.folder "Shape/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    size_z : float [@sop.default 1.] [@sop.label "Size Z"]
      [@sop.folder "Shape/Size"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.]; [@sop.vec3 "size"]
    orientation_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Shape/Orientation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "orientation"]
    orientation_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Shape/Orientation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "orientation"]
    orientation_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Shape/Orientation"] [@sop.min (-3.141592653589793)]
      [@sop.max 3.141592653589793]; [@sop.vec3 "orientation"]
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Shape"] [@sop.min 0.] [@sop.max 10.]
      [@sop.hard_min 0.];
    quasi_stratified : bool [@sop.default false]
      [@sop.label "Quasi-stratified"] [@sop.folder "Random"];
    velocity_stretch : Rdk.Point_replication.velocity_stretch
      [@sop.default Rdk.Point_replication.Replicate_no_velocity_stretch]
      [@sop.label "Velocity stretch"] [@sop.folder "Velocity"]
      [@sop.kind velocity_parameter];
    velocity_scale : float [@sop.default 1.] [@sop.label "Velocity scale"]
      [@sop.folder "Velocity"] [@sop.min (-10.)] [@sop.max 10.];
    inherit_velocity : float [@sop.default 1.] [@sop.label "Inherit velocity"]
      [@sop.folder "Velocity"] [@sop.min (-10.)] [@sop.max 10.];
    radial_velocity : float [@sop.default 0.] [@sop.label "Radial velocity"]
      [@sop.folder "Velocity"] [@sop.min (-10.)] [@sop.max 10.];
    use_noise : bool [@sop.default false] [@sop.label "Enable noise"]
      [@sop.folder "Noise"];
    noise_amplitude_x : float [@sop.default 0.1] [@sop.label "Amplitude X"]
      [@sop.folder "Noise/Amplitude"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "noise_amplitude"]
    noise_amplitude_y : float [@sop.default 0.1] [@sop.label "Amplitude Y"]
      [@sop.folder "Noise/Amplitude"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "noise_amplitude"]
    noise_amplitude_z : float [@sop.default 0.1] [@sop.label "Amplitude Z"]
      [@sop.folder "Noise/Amplitude"] [@sop.min 0.] [@sop.max 10.]; [@sop.vec3 "noise_amplitude"]
    noise_frequency_x : float [@sop.default 1.] [@sop.label "Frequency X"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "noise_frequency"]
    noise_frequency_y : float [@sop.default 1.] [@sop.label "Frequency Y"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "noise_frequency"]
    noise_frequency_z : float [@sop.default 1.] [@sop.label "Frequency Z"]
      [@sop.folder "Noise/Frequency"] [@sop.min 0.] [@sop.max 20.]; [@sop.vec3 "noise_frequency"]
    noise_offset_x : float [@sop.default 0.] [@sop.label "Offset X"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "noise_offset"]
    noise_offset_y : float [@sop.default 0.] [@sop.label "Offset Y"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "noise_offset"]
    noise_offset_z : float [@sop.default 0.] [@sop.label "Offset Z"]
      [@sop.folder "Noise/Offset"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "noise_offset"]
    noise_roughness : float [@sop.default 0.5] [@sop.label "Roughness"]
      [@sop.folder "Noise"] [@sop.min 0.] [@sop.max 1.];
    noise_attenuation : float [@sop.default 1.] [@sop.label "Attenuation"]
      [@sop.folder "Noise"] [@sop.min 0.] [@sop.max 10.];
    noise_turbulence : int [@sop.default 3] [@sop.label "Turbulence"]
      [@sop.folder "Noise"] [@sop.min 1] [@sop.max 12]
      [@sop.hard_min 1];
    noise_context_seed : bool [@sop.default false]
      [@sop.label "Use context noise seed"] [@sop.folder "Noise"];
    noise_seed : int [@sop.default 1] [@sop.label "Noise seed"]
      [@sop.folder "Noise"] [@sop.min 0] [@sop.max 9999];
    generated_group : string [@sop.default ""] [@sop.label "Generated group"]
      [@sop.folder "Output"];
    copy_point_attributes : string [@sop.default "*"]
      [@sop.label "Copy point attributes"] [@sop.folder "Transfer"];
    keep_source_attributes : bool [@sop.default false]
      [@sop.label "Keep source attributes"] [@sop.folder "Transfer"];
    transform_attributes : string [@sop.default "P"]
      [@sop.label "Transform attributes"] [@sop.folder "Transfer"];
    source_point_attribute : string [@sop.default "sourcepoint"]
      [@sop.label "Source point"] [@sop.folder "Output"];
    source_index_attribute : string [@sop.default "sourceindex"]
      [@sop.label "Source index"] [@sop.folder "Output"];
  } [@@sop.node_key "point_replicate"] [@@sop.node_label "Point Replicate"]

    [@@sop.validate fun parameters ->
      let refuse message = invalid_arg ("sop/point_replicate: " ^ message) in
      if not (List.for_all Float.is_finite [parameters.points_per_point;parameters.center_x;parameters.center_y;parameters.center_z;
          parameters.size_x;parameters.size_y;parameters.size_z;parameters.orientation_x;parameters.orientation_y;parameters.orientation_z;
          parameters.uniform_scale;parameters.velocity_scale;parameters.inherit_velocity;parameters.radial_velocity;
          parameters.noise_amplitude_x;parameters.noise_amplitude_y;parameters.noise_amplitude_z;
          parameters.noise_frequency_x;parameters.noise_frequency_y;parameters.noise_frequency_z;
          parameters.noise_offset_x;parameters.noise_offset_y;parameters.noise_offset_z;parameters.noise_roughness;parameters.noise_attenuation]) then
        refuse "numeric controls must be finite";
      if parameters.points_per_point < 0. || parameters.points_per_point > float_of_int Sys.max_array_length then refuse "invalid points per point";
      if List.exists (fun value -> value < 0.) [parameters.size_x;parameters.size_y;parameters.size_z;parameters.uniform_scale] then refuse "size and scale must be nonnegative";
      if parameters.noise_roughness < 0. || parameters.noise_roughness > 1. || parameters.noise_attenuation <= 0. || parameters.noise_turbulence < 1 then
        refuse "invalid noise controls";
      if String.trim parameters.id_attribute = "" then refuse "id attribute must be nonblank";
      if parameters.keep_source_attributes then (
        List.iter (fun name -> if String.trim name = "" || name = "P" then refuse "source metadata names must be nonblank and not P")
          [parameters.source_point_attribute;parameters.source_index_attribute];
        if parameters.source_point_attribute = parameters.source_index_attribute then refuse "source metadata names must differ");
      List.iter (fun pattern -> if String.trim pattern <> "" then validate_parameter_pattern "sop/point_replicate" pattern)
        [parameters.copy_point_attributes;parameters.transform_attributes]]
    [@@sop.node_category "Create/Points"] [@@sop.node_inputs 2] [@@sop.node_slots "input, custom_shape"]
    [@@sop.node_optional "1"]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input custom_shape ->
    if parameters.shape = Rdk.Point_replication.Replicate_custom && Option.is_none custom_shape then
      invalid_arg "sop/point_replicate: custom shape requires shape geometry";
    let label = Some label in
    let group = parameters.group in
    let keep_input = parameters.keep_input in
    let context_seed = parameters.context_seed in
    let seed = parameters.seed in
    let id_attribute = parameters.id_attribute in
    let generated_group = parameters.generated_group in
    let copy_point_attributes = parameters.copy_point_attributes in
    let keep_source_attributes = parameters.keep_source_attributes in
    let transform_attributes = parameters.transform_attributes in
    let source_point_attribute = parameters.source_point_attribute in
    let source_index_attribute = parameters.source_index_attribute in
    let shape = parameters.shape in
    let center = Rays_math.Vec3.create parameters.center_x parameters.center_y parameters.center_z in
    let size = Rays_math.Vec3.create parameters.size_x parameters.size_y parameters.size_z in
    let orientation = Rays_math.Vec3.create parameters.orientation_x parameters.orientation_y parameters.orientation_z in
    let uniform_scale = parameters.uniform_scale in
    let quasi_stratified = parameters.quasi_stratified in
    let velocity_stretch = parameters.velocity_stretch in
    let velocity_scale = parameters.velocity_scale in
    let inherit_velocity = parameters.inherit_velocity in
    let radial_velocity = parameters.radial_velocity in
    let use_noise = parameters.use_noise in
    let noise_amplitude = Rays_math.Vec3.create parameters.noise_amplitude_x parameters.noise_amplitude_y parameters.noise_amplitude_z in
    let noise_frequency = Rays_math.Vec3.create parameters.noise_frequency_x parameters.noise_frequency_y parameters.noise_frequency_z in
    let noise_offset = Rays_math.Vec3.create parameters.noise_offset_x parameters.noise_offset_y parameters.noise_offset_z in
    let noise_roughness = parameters.noise_roughness in
    let noise_attenuation = parameters.noise_attenuation in
    let noise_turbulence = parameters.noise_turbulence in
    let noise_context_seed = parameters.noise_context_seed in
    let noise_seed = parameters.noise_seed in
    let points_per_point = parameters.points_per_point in
    let scale_attribute = parameters.scale_attribute in
    let group = optional_text group and generated_group = optional_text generated_group and scale_attribute = optional_text scale_attribute in
    let seed = if context_seed then None else Some seed in
    let noise_seed = if noise_context_seed then None else Some noise_seed in
    let noise_amplitude = if use_noise then Some noise_amplitude else None in
    let center = vec3_copy center and size = vec3_copy size and orientation = vec3_copy orientation in
    let noise_amplitude = Option.map vec3_copy noise_amplitude
    and noise_frequency = vec3_copy noise_frequency and noise_offset = vec3_copy noise_offset in
    let dependencies = if Option.is_none seed
        || (Option.is_some noise_amplitude && Option.is_none noise_seed)
      then Context.Dependencies.one Context.Dependencies.Seed
      else Context.Dependencies.static in
    let stable_identity = Option.map (fun label ->
        stable_string_hash ("point_replicate:" ^ label)) label in
    let inputs = match custom_shape with None -> [|input|]
      | Some custom_shape -> [|input;custom_shape|] in
    Node.Private.make_geometry ?label ~operation:"point_replicate" ~version:1 ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies ~inputs
      (fun ~node_id context inputs ->
        let geometry = inputs.(0) in
        let selection = match group with
          | None -> Ok None
          | Some name -> (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point
              name geometry with
            | Some value -> Ok (Some value)
            | None -> Error (Diagnostic.error ~code:"missing_group"
                (Printf.sprintf "point_replicate could not find point group %S" name))) in
        match selection with
        | Error error -> Error error
        | Ok selection ->
            let identity = Option.value ~default:(Int64.of_int node_id)
                stable_identity in
            let seed = Option.value ~default:(mixed_seed context identity) seed in
            let noise_seed = Option.value ~default:(mixed_seed context
                (Int64.logxor identity 0x6a09e667f3bcc909L)) noise_seed in
            let custom_shape = if shape = Rdk.Point_replication.Replicate_custom && Array.length inputs = 2 then Some inputs.(1) else None in
            match Rdk.Point_replication.run ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ?points:selection ~keep_input
                ~seed:(Rays_math.Rand.seed seed) ~id_attribute ?generated_group
                ~copy_point_attributes ~keep_source_attributes
                ~transform_attributes
                ~source_point_attribute ~source_index_attribute ~shape ?custom_shape
                ~center ~size ~orientation ~uniform_scale ~quasi_stratified
                ~velocity_stretch ~velocity_scale ~inherit_velocity ~radial_velocity
                ?noise_amplitude ~noise_frequency ~noise_offset ~noise_roughness
                ~noise_attenuation ~noise_turbulence ~noise_seed
                ~points_per_point ?scale_attribute geometry with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Duplicate = struct
  type parameters = {
    copies : int [@sop.default 1] [@sop.label "Copies"]
      [@sop.min 1] [@sop.max 1024] [@sop.hard_min 0];
    cumulative : bool [@sop.default true] [@sop.label "Cumulative transform"];
    m00 : float [@sop.default 1.] [@sop.label "M00"]
      [@sop.folder "Transform/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m01 : float [@sop.default 0.] [@sop.label "M01"]
      [@sop.folder "Transform/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m02 : float [@sop.default 0.] [@sop.label "M02"]
      [@sop.folder "Transform/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m03 : float [@sop.default 0.] [@sop.label "M03"]
      [@sop.folder "Transform/Row 0"] [@sop.min (-10.)] [@sop.max 10.];
    m10 : float [@sop.default 0.] [@sop.label "M10"]
      [@sop.folder "Transform/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m11 : float [@sop.default 1.] [@sop.label "M11"]
      [@sop.folder "Transform/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m12 : float [@sop.default 0.] [@sop.label "M12"]
      [@sop.folder "Transform/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m13 : float [@sop.default 0.] [@sop.label "M13"]
      [@sop.folder "Transform/Row 1"] [@sop.min (-10.)] [@sop.max 10.];
    m20 : float [@sop.default 0.] [@sop.label "M20"]
      [@sop.folder "Transform/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m21 : float [@sop.default 0.] [@sop.label "M21"]
      [@sop.folder "Transform/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m22 : float [@sop.default 1.] [@sop.label "M22"]
      [@sop.folder "Transform/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m23 : float [@sop.default 0.] [@sop.label "M23"]
      [@sop.folder "Transform/Row 2"] [@sop.min (-10.)] [@sop.max 10.];
    m30 : float [@sop.default 0.] [@sop.label "M30"]
      [@sop.folder "Transform/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m31 : float [@sop.default 0.] [@sop.label "M31"]
      [@sop.folder "Transform/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m32 : float [@sop.default 0.] [@sop.label "M32"]
      [@sop.folder "Transform/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    m33 : float [@sop.default 1.] [@sop.label "M33"]
      [@sop.folder "Transform/Row 3"] [@sop.min (-10.)] [@sop.max 10.];
    group : string [@sop.default ""] [@sop.label "Primitive group"];
    copy_group_prefix : string [@sop.default ""]
      [@sop.label "Copy group prefix"] [@sop.folder "Output"];
    preserve_groups : bool [@sop.default false]
      [@sop.label "Preserve groups"] [@sop.folder "Output"];
  } [@@sop.node_key "duplicate"] [@@sop.node_label "Duplicate"]

    [@@sop.validate fun parameters ->
      if parameters.copies < 0 || parameters.copies >= Sys.max_array_length then invalid_arg "sop/duplicate: copies must fit a nonnegative array count";
      if not (List.for_all Float.is_finite [parameters.m00;parameters.m01;parameters.m02;parameters.m03;
        parameters.m10;parameters.m11;parameters.m12;parameters.m13;parameters.m20;parameters.m21;parameters.m22;parameters.m23;
        parameters.m30;parameters.m31;parameters.m32;parameters.m33]) then invalid_arg "sop/duplicate: matrix must be finite"]
    [@@sop.node_category "Copy"] [@@sop.node_inputs 1]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let copies = parameters.copies in
    let cumulative = parameters.cumulative in
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
    let group = parameters.group in
    let copy_group_prefix = parameters.copy_group_prefix in
    let preserve_groups = parameters.preserve_groups in
    let transform = Rays_math.Mat4.of_rows (m00,m01,m02,m03) (m10,m11,m12,m13) (m20,m21,m22,m23) (m30,m31,m32,m33) in
    let group = optional_text group and copy_group_prefix = optional_text copy_group_prefix in
    Node.Private.make_geometry ?label ~operation:"duplicate" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
      ~inputs:[|input|] (fun ~node_id:_ context inputs ->
        let primitives = match group with
          | None -> Ok None
          | Some name ->
              (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name inputs.(0) with
               | Some group -> Ok (Some group)
               | None -> Error (Diagnostic.error ~code:"missing_group"
                   (Printf.sprintf "duplicate could not find primitive group %S" name))) in
        match primitives with
        | Error error -> Error error
        | Ok primitives ->
            match Rdk.Instance_copy.duplicate ~cancel:(Context.cancel_token context)
                ~grain:(Context.grain context) ~copies ~cumulative ~transform
                ?primitives ?copy_group_prefix ~preserve_groups inputs.(0) with
            | Ok geometry -> cooked geometry
            | Error error -> structured_rdk_error error)
  )
  let factory = parameters_factory build
end

module Transform = struct
  let order_parameter = Soft_transform.order_parameter
  let rotation_order_parameter = Soft_transform.rotation_order_parameter
  let mode_parameter = Param.choice ~equal:( = ) ["TRS", Support.Transform_trs; "Matrix", Support.Transform_matrix]
  type parameters = {
    mode : Support.transform_mode [@sop.default Transform_trs]
      [@sop.label "Transform mode"] [@sop.kind mode_parameter];
    group_owner : element_owner [@sop.default Element_point]
      [@sop.label "Group type"] [@sop.folder "Selection"] [@sop.kind element_owner_parameter];
    group : string [@sop.default ""] [@sop.label "Group"] [@sop.folder "Selection"];
    order : Rdk.Transform_ops.transform_order [@sop.default Rdk.Transform_ops.Transform_srt]
      [@sop.label "Transform order"] [@sop.kind order_parameter];
    rotation_order : Rdk.Transform_ops.transform_rotation_order [@sop.default Rdk.Transform_ops.Transform_xyz]
      [@sop.label "Rotation order"] [@sop.kind rotation_order_parameter];
    shear_xy : float [@sop.default 0.] [@sop.label "Shear XY"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
    shear_xz : float [@sop.default 0.] [@sop.label "Shear XZ"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
    shear_yz : float [@sop.default 0.] [@sop.label "Shear YZ"]
      [@sop.folder "Transform/Shear"] [@sop.min (-4.)] [@sop.max 4.];
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
    m00 : float [@sop.default 1.] [@sop.label "M00"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m01 : float [@sop.default 0.] [@sop.label "M01"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m02 : float [@sop.default 0.] [@sop.label "M02"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m03 : float [@sop.default 0.] [@sop.label "M03"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m10 : float [@sop.default 0.] [@sop.label "M10"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m11 : float [@sop.default 1.] [@sop.label "M11"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m12 : float [@sop.default 0.] [@sop.label "M12"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m13 : float [@sop.default 0.] [@sop.label "M13"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m20 : float [@sop.default 0.] [@sop.label "M20"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m21 : float [@sop.default 0.] [@sop.label "M21"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m22 : float [@sop.default 1.] [@sop.label "M22"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m23 : float [@sop.default 0.] [@sop.label "M23"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m30 : float [@sop.default 0.] [@sop.label "M30"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m31 : float [@sop.default 0.] [@sop.label "M31"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m32 : float [@sop.default 0.] [@sop.label "M32"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    m33 : float [@sop.default 1.] [@sop.label "M33"] [@sop.folder "Matrix"] [@sop.min (-10.)] [@sop.max 10.];
    translate_x : float [@sop.default 0.] [@sop.label "Translate X"]
      [@sop.folder "Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    translate_y : float [@sop.default 0.] [@sop.label "Translate Y"]
      [@sop.folder "Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    translate_z : float [@sop.default 0.] [@sop.label "Translate Z"]
      [@sop.folder "Translate"] [@sop.min (-10.)] [@sop.max 10.]; [@sop.vec3 "translate"]
    rotate_x : float [@sop.default 0.] [@sop.label "Rotate X"]
      [@sop.folder "Rotate"] [@sop.min (-3.14159)] [@sop.max 3.14159]; [@sop.vec3 "rotate"]
    rotate_y : float [@sop.default 0.] [@sop.label "Rotate Y"]
      [@sop.folder "Rotate"] [@sop.min (-3.14159)] [@sop.max 3.14159]; [@sop.vec3 "rotate"]
    rotate_z : float [@sop.default 0.] [@sop.label "Rotate Z"]
      [@sop.folder "Rotate"] [@sop.min (-3.14159)] [@sop.max 3.14159]; [@sop.vec3 "rotate"]
    scale_x : float [@sop.default 1.] [@sop.label "Scale X"]
      [@sop.folder "Scale"] [@sop.min 0.01] [@sop.max 10.]; [@sop.vec3 "scale"]
    scale_y : float [@sop.default 1.] [@sop.label "Scale Y"]
      [@sop.folder "Scale"] [@sop.min 0.01] [@sop.max 10.]; [@sop.vec3 "scale"]
    scale_z : float [@sop.default 1.] [@sop.label "Scale Z"]
      [@sop.folder "Scale"] [@sop.min 0.01] [@sop.max 10.]; [@sop.vec3 "scale"]
    uniform_scale : float [@sop.default 1.] [@sop.label "Uniform scale"]
      [@sop.folder "Scale"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.];
    preserve_normal_length : bool [@sop.default false]
      [@sop.label "Preserve normal length"] [@sop.folder "Normals"];
    recompute_normals : bool [@sop.default false]
      [@sop.label "Recompute normals"] [@sop.folder "Normals"];
  } [@@sop.node_key "transform"] [@@sop.node_label "Transform"]
    [@@sop.node_facts {elementwise = Node.Points; reads = ["P"; "N"]; writes = ["P"; "N"];
      topology = Node.Preserved; exact = false}]
    [@@sop.node_category "Modify"] [@@sop.node_inputs 1]

    [@@sop.validate fun parameters ->
    let matrix = match parameters.mode with
      | Transform_matrix -> Rays_math.Mat4.of_rows
          (parameters.m00,parameters.m01,parameters.m02,parameters.m03)
          (parameters.m10,parameters.m11,parameters.m12,parameters.m13)
          (parameters.m20,parameters.m21,parameters.m22,parameters.m23)
          (parameters.m30,parameters.m31,parameters.m32,parameters.m33)
      | Transform_trs -> Rdk.Transform_ops.compose_transform
          ~order:parameters.order ~rotation_order:parameters.rotation_order
          ~translate:(Rays_math.Vec3.create parameters.translate_x parameters.translate_y parameters.translate_z)
          ~rotate:(Rays_math.Vec3.create parameters.rotate_x parameters.rotate_y parameters.rotate_z)
          ~scale:(Rays_math.Vec3.create parameters.scale_x parameters.scale_y parameters.scale_z)
          ~shear:(Rays_math.Vec3.create parameters.shear_xy parameters.shear_xz parameters.shear_yz)
          ~uniform_scale:parameters.uniform_scale
          ~pivot:(Rays_math.Vec3.create parameters.pivot_x parameters.pivot_y parameters.pivot_z)
          ~pivot_rotation:(Rays_math.Vec3.create parameters.pivot_rotation_x parameters.pivot_rotation_y parameters.pivot_rotation_z)
          ~invert:parameters.invert () |> function
            | Ok matrix -> matrix
            | Error error -> invalid_arg ("sop/transform: " ^ Rdk.Error.to_string error)
    in
    for index = 0 to 15 do
      if not (Float.is_finite (Rays_math.Mat4.get matrix ~row:(index / 4) ~column:(index mod 4))) then
        invalid_arg "sop/transform: matrix must be finite"
    done]

    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label parameters input ->
    let label = Some label in
    let mode = parameters.mode in
    let group_owner = parameters.group_owner in
    let group = parameters.group in
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
    let preserve_normal_length = parameters.preserve_normal_length in
    let recompute_normals = parameters.recompute_normals in
    let matrix = match mode with
      | Transform_matrix -> Rays_math.Mat4.of_rows (m00,m01,m02,m03) (m10,m11,m12,m13) (m20,m21,m22,m23) (m30,m31,m32,m33)
      | Transform_trs -> Rdk.Transform_ops.compose_transform ~order ~rotation_order ~translate ~rotate ~scale
          ~shear:(Rays_math.Vec3.create shear_xy shear_xz shear_yz) ~uniform_scale ~pivot ~pivot_rotation ~invert () |> Result.get_ok in
    let selection = optional_element_group group_owner group in
    Node.Private.make_geometry ?label ~operation:"transform" ~version:2
      ~parameters:""
      ~cook_mode:(Node.Duplicate_input 0)
      ~dependencies:Context.Dependencies.static ~inputs:[|input|]
      (fun ~node_id:_ context inputs ->
        match resolve_element_group ~operation:"transform" selection inputs.(0) with
        | Error error -> Error error
        | Ok selection -> match Rdk.Transform_ops.transform_selected
            ~cancel:(Context.cancel_token context) ~grain:(Context.grain context)
            ?selection ~preserve_normal_length ~recompute_normals matrix inputs.(0) with
          | Ok geometry -> cooked geometry
          | Error error -> structured_rdk_error error)
  )

  let factory = parameters_factory build

end
