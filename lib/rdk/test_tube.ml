open Rays
open Rdk
open Rdk_test_support

let float2_attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float3_attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && begin
    let equal = ref true in
    for index = 0 to Group.length left - 1 do
      if Group.mem index left <> Group.mem index right then equal := false
    done;
    !equal
  end

let equal_geometry left right =
  let left_positions = positions left and right_positions = positions right
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)

let check_vertex_normal_winding geometry message =
  let point = positions geometry
  and normal = float3_attribute geometry Attribute.Vertex "N"
  and topology = Topology.Private.view (Geometry.topology geometry) in
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let nx = ref 0. and ny = ref 0. and nz = ref 0. in
    for vertex = first to last - 1 do
      let next = if vertex + 1 = last then first else vertex + 1 in
      let a = topology.vertex_points.(vertex)
      and b = topology.vertex_points.(next) in
      nx := !nx +. ((point.y.(a) -. point.y.(b))
          *. (point.z.(a) +. point.z.(b)));
      ny := !ny +. ((point.z.(a) -. point.z.(b))
          *. (point.x.(a) +. point.x.(b)));
      nz := !nz +. ((point.x.(a) -. point.x.(b))
          *. (point.y.(a) +. point.y.(b)))
    done;
    let alignment = (!nx *. normal.x.(first)) +. (!ny *. normal.y.(first))
        +. (!nz *. normal.z.(first)) in
    check (alignment > 1e-12)
      (Printf.sprintf "%s (primitive %d, alignment %.17g)"
        message primitive alignment)
  done

let make ?(connectivity = Parametric_generators.Tube_quads) ?end_caps ?consolidate_cap_points
    ?normals ?orientation ?center ?rotation ?rotation_order ?radius_scale
    ?uv_attribute ?cap_group ?(rows = 3) ?(columns = 8)
    ?(top_radius = 1.) ?(bottom_radius = 1.) ?(height = 2.) () =
  Parametric_generators.tube ~connectivity ?end_caps ?consolidate_cap_points ?normals
    ?orientation ?center ?rotation ?rotation_order ?radius_scale ?uv_attribute
    ?cap_group ~rows ~columns ~top_radius ~bottom_radius ~height () |> get_ok

let check_default_and_connectivity () =
  let quads = make ()
  and triangles = make ~connectivity:Parametric_generators.Tube_triangles ()
  and alternating = make ~connectivity:Parametric_generators.Tube_alternating_triangles ()
  and rows = make ~connectivity:Parametric_generators.Tube_rows ()
  and columns = make ~connectivity:Parametric_generators.Tube_columns ()
  and both = make ~connectivity:Parametric_generators.Tube_rows_and_columns ()
  and points = make ~connectivity:Parametric_generators.Tube_points ()
  and no_normals = make ~normals:Parametric_generators.Tube_no_normals () in
  let point = positions quads
  and normal = float3_attribute quads Attribute.Point "N"
  and topology = Topology.Private.view (Geometry.topology quads) in
  check (Geometry.point_count quads = 24 && Geometry.vertex_count quads = 64
      && Geometry.primitive_count quads = 16)
    "default Tube cardinality";
  check (near point.x.(0) 1. && near point.y.(0) (-1.) && near point.z.(0) 0.
      && near normal.x.(0) 1. && near normal.y.(0) 0.
      && near normal.z.(0) 0.)
    "default Tube frame or normals";
  check (Array.sub topology.vertex_points 0 4 = [|0; 8; 9; 1|]
      && topology.primitive_offsets = Array.init 17 (fun index -> index * 4))
    "default Tube topology/order";
  check (Geometry.vertex_count triangles = 96
      && Geometry.primitive_count triangles = 32)
    "triangle Tube cardinality";
  check (Geometry.vertex_count rows = 24 && Geometry.primitive_count rows = 8)
    "Tube row cardinality";
  check (Geometry.vertex_count columns = 24
      && Geometry.primitive_count columns = 3)
    "Tube column cardinality";
  check (Geometry.vertex_count both = 48 && Geometry.primitive_count both = 11)
    "Tube combined-curve cardinality";
  check (Geometry.vertex_count points = 0 && Geometry.primitive_count points = 0)
    "Tube point cardinality";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" no_normals = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" no_normals = None)
    "Tube_no_normals emitted an N attribute";
  for primitive = 0 to 7 do
    check (Topology.primitive_kind (Geometry.topology both) primitive
        = Topology.Open_polyline) "Tube longitudinal row was not open"
  done;
  for primitive = 8 to 10 do
    check (Topology.primitive_kind (Geometry.topology both) primitive
        = Topology.Closed_polyline) "Tube radial column was not closed"
  done;
  let regular = Topology.Private.view (Geometry.topology triangles)
  and checker = Topology.Private.view (Geometry.topology alternating) in
  check (regular.vertex_points <> checker.vertex_points)
    "alternating Tube collapsed to regular triangles"

let check_cones_caps_normals_and_uv () =
  let cone = make ~connectivity:Parametric_generators.Tube_triangles ~top_radius:0. ()
  and cone_quads = make ~top_radius:0. () in
  check (Geometry.point_count cone = 17 && Geometry.vertex_count cone = 72
      && Geometry.primitive_count cone = 24)
    "triangle cone cardinality";
  check (Geometry.point_count cone_quads = 17
      && Geometry.vertex_count cone_quads = 56
      && Geometry.primitive_count cone_quads = 16)
    "quad cone cardinality";
  let apex = 16 and topology = Geometry.topology cone_quads in
  for primitive = 8 to 15 do
    check (Topology.primitive_size topology primitive = 3)
      "quad cone emitted a degenerate apex quad";
    let first, last = Topology.primitive_vertex_range topology primitive in
    let apex_references = ref 0 in
    for vertex = first to last - 1 do
      if Topology.point_of_vertex topology vertex = apex then incr apex_references
    done;
    check (!apex_references = 1) "cone side did not use one shared apex"
  done;
  let capped = make ~connectivity:Parametric_generators.Tube_triangles ~top_radius:0.
      ~end_caps:true ~consolidate_cap_points:false
      ~normals:Parametric_generators.Tube_vertex_normals ~uv_attribute:"uv"
      ~cap_group:"caps" () in
  check (Geometry.point_count capped = 25 && Geometry.vertex_count capped = 80
      && Geometry.primitive_count capped = 25)
    "unconsolidated capped cone cardinality";
  (match Geometry.find_group ~owner:Group.Primitive "caps" capped with
   | Some group -> check (Group.cardinality group = 1 && Group.mem 24 group)
       "Tube cap group membership"
   | None -> fail "Tube dropped cap group");
  let uv = float2_attribute capped Attribute.Vertex "uv"
  and normal = float3_attribute capped Attribute.Vertex "N" in
  for vertex = 0 to Geometry.vertex_count capped - 1 do
    let length = sqrt ((normal.x.(vertex) *. normal.x.(vertex))
        +. (normal.y.(vertex) *. normal.y.(vertex))
        +. (normal.z.(vertex) *. normal.z.(vertex))) in
    check (near length 1. && uv.x.(vertex) >= 0. && uv.x.(vertex) <= 1.
        && uv.y.(vertex) >= 0. && uv.y.(vertex) <= 1.)
      "Tube emitted invalid vertex normal/UV"
  done;
  check_vertex_normal_winding capped
    "capped cone winding disagrees with vertex normals";
  let mesh = Rdk_rays.Rays_mesh.to_mesh capped |> get_ok in
  check (Mesh.index_count mesh > 0 && Option.fold ~none:false ~some:(fun a -> Array.length a = Mesh.vertex_count mesh)
        (Mesh.Private.view mesh).normals
      && Option.fold ~none:false ~some:(fun a -> Array.length a = Mesh.vertex_count mesh)
        (Mesh.Private.view mesh).tex_coords)
    "capped cone failed mesh conversion";
  let mixed_mesh = Rdk_rays.Rays_mesh.to_mesh cone_quads |> get_ok in
  check (Mesh.index_count mixed_mesh = 72)
    "quad Tube with triangle apex failed mixed-topology mesh conversion";
  let bottom_cone = make ~bottom_radius:0. ~end_caps:true
      ~normals:Parametric_generators.Tube_vertex_normals () in
  check (Geometry.point_count bottom_cone = 17
      && Geometry.primitive_count bottom_cone = 17)
    "bottom-apex cone cardinality";
  check_vertex_normal_winding bottom_cone
    "bottom-apex cone winding disagrees with vertex normals";
  let cylinder_shared = make ~end_caps:true ~consolidate_cap_points:true
      ~normals:Parametric_generators.Tube_vertex_normals ~uv_attribute:"uv" ()
  and cylinder_unique = make ~end_caps:true ~consolidate_cap_points:false
      ~normals:Parametric_generators.Tube_point_normals () in
  check (Geometry.point_count cylinder_shared = 24
      && Geometry.vertex_count cylinder_shared = 80
      && Geometry.primitive_count cylinder_shared = 18)
    "consolidated capped cylinder cardinality";
  check (Geometry.point_count cylinder_unique = 40)
    "unconsolidated cap corner points";
  let unique_n = float3_attribute cylinder_unique Attribute.Point "N" in
  check (near unique_n.y.(24) (-1.) && near unique_n.y.(32) 1.)
    "unconsolidated cap point normals are not hard";
  check_vertex_normal_winding cylinder_shared
    "capped cylinder winding disagrees with vertex normals";
  let point_output = make ~connectivity:Parametric_generators.Tube_points ~top_radius:0.
      ~uv_attribute:"uv" () in
  check (Geometry.point_count point_output = 17
      && Geometry.find_attribute ~owner:Attribute.Point "uv" point_output <> None)
    "cone point lattice or point UV ownership";
  let column_curves = make ~connectivity:Parametric_generators.Tube_columns ~top_radius:0. () in
  check (Geometry.primitive_count column_curves = 2
      && Geometry.vertex_count column_curves = 16)
    "cone emitted a zero-area apex ring curve"

let check_frustum_orientation_and_rotation () =
  let frustum = make ~connectivity:Parametric_generators.Tube_points
      ~top_radius:1. ~bottom_radius:2. ~height:2. () in
  let normal = float3_attribute frustum Attribute.Point "N" in
  check (near normal.x.(0) (2. /. sqrt 5.)
      && near normal.y.(0) (1. /. sqrt 5.) && near normal.z.(0) 0.)
    "frustum analytic side normal";
  let bounds orientation = make ~connectivity:Parametric_generators.Tube_points ~orientation
      ~top_radius:1. ~bottom_radius:2. ~height:6. ~rows:3 ~columns:8 ()
      |> Analysis.bounds |> Option.get in
  let x = bounds Parametric_generators.Tube_x and y = bounds Parametric_generators.Tube_y
  and z = bounds Parametric_generators.Tube_z
  and custom = bounds (Parametric_generators.Tube_axis (Rays_math.Vec3.create 0. max_float 0.)) in
  check (near x.size.x 6. && near x.size.y 4. && near x.size.z 4.
      && near y.size.x 4. && near y.size.y 6. && near y.size.z 4.
      && near z.size.x 4. && near z.size.y 4. && near z.size.z 6.
      && near custom.size.x y.size.x && near custom.size.y y.size.y
      && near custom.size.z y.size.z)
    "Tube orientation bounds";
  let rotation = Rays_math.Vec3.create 0.3 0.5 0.7 in
  let first order = make ~connectivity:Parametric_generators.Tube_points ~rotation
      ~rotation_order:order ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~radius_scale:2. ~top_radius:1. ~bottom_radius:1. ~height:6.
      ~rows:2 ~columns:8 () |> positions
      |> fun values -> Rays_math.Vec3.create values.x.(0) values.y.(0) values.z.(0) in
  let source = Rays_math.Vec3.create 2. (-3.) 0. in
  let cases = [
    Parametric_generators.Tube_xyz, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_x rotation.x));
    Parametric_generators.Tube_xzy, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z) (Rays_math.Mat4.rotation_x rotation.x));
    Parametric_generators.Tube_yxz, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x) (Rays_math.Mat4.rotation_y rotation.y));
    Parametric_generators.Tube_yzx, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z) (Rays_math.Mat4.rotation_y rotation.y));
    Parametric_generators.Tube_zxy, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x) (Rays_math.Mat4.rotation_z rotation.z));
    Parametric_generators.Tube_zyx, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_z rotation.z)) ] in
  List.iter (fun (order, matrix) ->
    let expected = Rays_math.Mat4.transform_point matrix source
        |> fun value -> Rays_math.Vec3.add value (Rays_math.Vec3.create 3. (-2.) 5.) in
    let actual = first order in
    check (near actual.x expected.x && near actual.y expected.y
        && near actual.z expected.z) "Tube Euler rotation order") cases

let check_validation () =
  let run ?grain ?connectivity ?end_caps ?consolidate_cap_points ?normals
      ?orientation ?center ?rotation ?radius_scale ?uv_attribute ?cap_group
      ?rows ?columns ?(top_radius = 1.) ?(bottom_radius = 1.) ?(height = 2.) () =
    Parametric_generators.tube ?grain ?connectivity ?end_caps ?consolidate_cap_points ?normals
      ?orientation ?center ?rotation ?radius_scale ?uv_attribute ?cap_group
      ?rows ?columns ~top_radius ~bottom_radius ~height () in
  expect_code "invalid_parameter" (run ~grain:0 ());
  expect_code "invalid_parameter" (run ~rows:1 ());
  expect_code "invalid_parameter" (run ~columns:2 ());
  expect_code "invalid_parameter" (run ~top_radius:(-1.) ());
  expect_code "invalid_parameter" (run ~top_radius:0. ~bottom_radius:0. ());
  expect_code "invalid_parameter" (run ~radius_scale:0. ());
  expect_code "invalid_parameter" (run ~height:Float.nan ());
  expect_code "invalid_parameter"
    (run ~center:(Rays_math.Vec3.create max_float 0. 0.) ~bottom_radius:max_float ());
  expect_code "invalid_parameter"
    (run ~rotation:(Rays_math.Vec3.create Float.infinity 0. 0.) ());
  expect_code "invalid_parameter"
    (run ~orientation:(Parametric_generators.Tube_axis Rays_math.Vec3.zero) ());
  expect_code "invalid_parameter" (run ~uv_attribute:"N" ());
  expect_code "invalid_parameter" (run ~cap_group:"" ~end_caps:true ());
  expect_code "invalid_parameter" (run ~cap_group:"caps" ());
  expect_code "invalid_parameter"
    (run ~connectivity:Parametric_generators.Tube_points ~normals:Parametric_generators.Tube_vertex_normals ());
  expect_code "invalid_parameter"
    (run ~connectivity:Parametric_generators.Tube_rows ~end_caps:true ());
  expect_code "invalid_parameter" (run ~rows:max_int ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Parametric_generators.tube ~cancel:cancelled ~rows:1_000 ~columns:500
       ~top_radius:1. ~bottom_radius:2. ~height:3. ())

let check_parallel_exact () =
  let run domains make = Parallel.run ~domains (fun () -> make () |> get_ok) in
  let cases = [
    (fun () -> Parametric_generators.tube ~grain:1024 ~connectivity:Parametric_generators.Tube_quads
      ~rows:256 ~columns:128 ~top_radius:2. ~bottom_radius:3. ~height:5. ());
    (fun () -> Parametric_generators.tube ~grain:1024
      ~connectivity:Parametric_generators.Tube_alternating_triangles ~end_caps:true
      ~consolidate_cap_points:false ~normals:Parametric_generators.Tube_vertex_normals
      ~orientation:(Parametric_generators.Tube_axis (Rays_math.Vec3.create 1. 2. 3.))
      ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7) ~rotation_order:Parametric_generators.Tube_yzx
      ~radius_scale:1.2 ~uv_attribute:"uv" ~cap_group:"caps"
      ~rows:256 ~columns:128 ~top_radius:0. ~bottom_radius:3. ~height:5. ());
    (fun () -> Parametric_generators.tube ~grain:1024
      ~connectivity:Parametric_generators.Tube_rows_and_columns
      ~normals:Parametric_generators.Tube_vertex_normals ~uv_attribute:"uv"
      ~rows:256 ~columns:128 ~top_radius:2. ~bottom_radius:3. ~height:5. ());
    (fun () -> Parametric_generators.tube ~grain:1024 ~connectivity:Parametric_generators.Tube_points
      ~uv_attribute:"uv" ~rows:256 ~columns:128
      ~top_radius:0. ~bottom_radius:3. ~height:5. ()) ] in
  List.iter (fun make ->
    let one = run 1 make and many = run 4 make in
    check (equal_geometry one many)
      "one-domain and four-domain Tube geometry differ") cases

let run () =
  check_default_and_connectivity ();
  check_cones_caps_normals_and_uv ();
  check_frustum_orientation_and_rotation ();
  check_validation ();
  check_parallel_exact ();
  print_endline "Tube tests passed"
