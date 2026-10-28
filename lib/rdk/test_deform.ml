open Rays
open Rdk
open Rdk_test_support

let near left right = abs_float (left -. right) <= 1e-11

let equal_positions left right =
  let left = positions left and right = positions right in
  left.x = right.x && left.y = right.y && left.z = right.z

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get_string_ok

let float_attribute name values =
  Attribute.create_owned ~name ~owner:Attribute.Point (Attribute.Float values)
  |> get_string_ok

let float3_attribute ~owner name x y z =
  let values = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  Attribute.create_owned ~name ~owner (Attribute.Float3 values) |> get_string_ok

let point_float name geometry =
  Geometry.find_attribute ~owner:Attribute.Point name geometry |> Option.get
  |> Attribute.get (Attribute.key ~name ~owner:Attribute.Point Attribute.float)
  |> Option.get

let point_float3 name geometry =
  Geometry.find_attribute ~owner:Attribute.Point name geometry |> Option.get
  |> Attribute.get (Attribute.key ~name ~owner:Attribute.Point Attribute.float3)
  |> Option.get |> Packed.Float3.View.view

let expect_error code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let expect_only_points_moved source output expected delta_x delta_y delta_z =
  let source = positions source and output = positions output in
  for point = 0 to Array.length source.x - 1 do
    let selected = Array.mem point expected in
    let dx = output.x.(point) -. source.x.(point)
    and dy = output.y.(point) -. source.y.(point)
    and dz = output.z.(point) -. source.z.(point) in
    if selected then begin
      if not (near dx delta_x && near dy delta_y && near dz delta_z) then
        fail (Printf.sprintf "selected point %d moved incorrectly" point)
    end else if dx <> 0. || dy <> 0. || dz <> 0. then
      fail (Printf.sprintf "unselected point %d moved" point)
  done

let run () =
  let quad_positions = Packed.Float3.View.of_owned_exn
      ~x:[|-1.; 1.; 1.; -1.|] ~y:[|0.; 0.; 0.; 0.|]
      ~z:[|-1.; -1.; 1.; 1.|] in
  let quad_topology_builder = Tb.create ~point_count:4 () in
  Tb.add_polygon quad_topology_builder [|0; 1; 2; 3|];
  let quad_topology = Tb.freeze quad_topology_builder in
  let quad = Geometry.create ~positions:quad_positions ~topology:quad_topology ()
      |> get_string_ok in
  let normal_quad = Normal_ops.run quad |> get_ok in
  if Topology.data_id (Geometry.topology normal_quad)
      <> Topology.data_id quad_topology
      || Geometry.primitive_count normal_quad <> 1
      || Geometry.vertex_count normal_quad <> 4 then
    fail "Normals changed N-gon topology";
  let normal = point_float3 "N" normal_quad in
  if not (Array.for_all Float.is_finite normal.x
      && Array.for_all Float.is_finite normal.y
      && Array.for_all Float.is_finite normal.z) then
    fail "Normals produced non-finite values";

  let peaked = Deform.peak ~distance:0.75 normal_quad |> get_ok in
  let noisy = Deform.noise_displace ~amplitude:0.2
      ~frequency:0.5 ~seed:17 normal_quad |> get_ok in
  if Geometry.point_count noisy <> 4
      || Topology.data_id (Geometry.topology noisy)
         <> Topology.data_id quad_topology
      || equal_positions noisy normal_quad then
    fail "Noise Displace kept topology but did not move the points";
  (* The default height mode moves Y only and invalidates the point normals. *)
  if (positions noisy).x <> (positions normal_quad).x
      || (positions noisy).z <> (positions normal_quad).z
      || Geometry.find_attribute ~owner:Attribute.Point "N" noisy <> None then
    fail "Height noise moved X/Z or kept stale normals";
  (let p = positions noisy and q = positions normal_quad in
   Array.iteri (fun i _ ->
     if not (Float.is_finite p.x.(i) && Float.is_finite p.y.(i)
         && Float.is_finite p.z.(i)) then
       fail "Noise Displace produced non-finite positions";
     if Float.abs (p.x.(i) -. q.x.(i)) > 0.2 +. 1e-9
         || Float.abs (p.y.(i) -. q.y.(i)) > 0.2 +. 1e-9
         || Float.abs (p.z.(i) -. q.z.(i)) > 0.2 +. 1e-9 then
       fail "Noise Displace exceeded its amplitude") p.x);
  (match Deform.noise_displace ~amplitude:nan ~frequency:0.5
      ~seed:17 normal_quad with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Noise Displace accepted non-finite amplitude");
  let count = 2051 in
  let source = Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
    for i = first to last - 1 do
      x.(i) <- float i *. 0.137 -. 128.;
      y.(i) <- sin (float i *. 0.31);
      z.(i) <- float (i mod 23) *. (-0.2)
    done) in
  let nx = Array.init count (fun i -> cos (float i *. 0.23))
  and ny = Array.make count 0.37 and nz = Array.make count (-1.2) in
  let source = add_attribute (float3_attribute ~owner:Attribute.Point "N" nx ny nz) source in
  let p = positions source in
  let source_bytes = geometry_bytes source in
  let noise = Rays_math.Noise.create 0 in
  let height domains grain = Parallel.run ~domains (fun () ->
    Deform.noise_displace ~grain ~amplitude:0.8 ~frequency:0.16 ~seed:0 source |> get_ok) in
  let height_one = height 1 257 and height_eight = height 8 257 in
  let samples = Array.make count 0. in
  Rays_math.Noise.Private.sample2_into noise ~first:0 ~last:count ~frequency:0.16
    ~x:p.x ~y:p.z ~output:samples;
  let height_expected = Kernel.edit_point_ranges (fun ~first ~last ~x:_ ~y ~z:_ ->
    for i = first to last - 1 do
      y.(i) <- p.y.(i) +. 0.8 *. ((samples.(i) *. 2.) -. 1.)
    done) source |> Geometry.without_attribute ~owner:Attribute.Point "N" in
  check (geometry_bytes height_one = geometry_bytes height_expected
    && geometry_bytes height_one = geometry_bytes height_eight
    && geometry_bytes height_one = geometry_bytes (height 1 max_int))
    "Height noise differs from the scalar formula or across domains/grains";
  let height_positions = positions height_one in
  check (height_positions.x == p.x && height_positions.z == p.z && height_positions.y != p.y)
    "Height noise must borrow immutable X/Z and own Y";
  check (Geometry.topology height_one == Geometry.topology source)
    "Height noise replaced topology";
  check (geometry_bytes source = source_bytes) "Height noise mutated its input";
  let expected = Kernel.edit_point_ranges (fun ~first ~last ~x ~y ~z ->
    for i = first to last - 1 do
      let scale = 0.8 *. Rays_math.Noise.sample3 noise
        ~x:(p.x.(i) *. 0.16) ~y:(p.y.(i) *. 0.16) ~z:(p.z.(i) *. 0.16) in
      x.(i) <- p.x.(i) +. nx.(i) *. scale;
      y.(i) <- p.y.(i) +. ny.(i) *. scale;
      z.(i) <- p.z.(i) +. nz.(i) *. scale
    done) source |> Geometry.without_attribute ~owner:Attribute.Point "N" in
  let one = Parallel.run ~domains:1 (fun () -> Deform.noise_displace ~mode:Deform.Normal_3d
    ~grain:257 ~amplitude:0.8 ~frequency:0.16 ~seed:0 source |> get_ok) in
  let eight = Parallel.run ~domains:8 (fun () -> Deform.noise_displace ~mode:Deform.Normal_3d
    ~grain:257 ~amplitude:0.8 ~frequency:0.16 ~seed:0 source |> get_ok) in
  check (geometry_bytes one = geometry_bytes expected && geometry_bytes one = geometry_bytes eight)
    "Normal 3D noise differs from scalar formula or across domains";
  check (Geometry.topology one == Geometry.topology source) "Normal 3D noise replaced topology";
  check (geometry_bytes source = source_bytes) "Normal 3D noise mutated its input";
  let oversized_grain = Deform.noise_displace ~mode:Deform.Normal_3d ~grain:max_int
    ~amplitude:0.8 ~frequency:0.16 ~seed:0 source |> get_ok in
  check (geometry_bytes oversized_grain = geometry_bytes one)
    "Normal 3D noise overflowed the range count with an oversized grain";
  expect_error "invalid_parameter" (Deform.noise_displace ~mode:Deform.Normal_3d
    ~amplitude:0.8 ~frequency:0.16 ~seed:0
    (Geometry.without_attribute ~owner:Attribute.Point "N" source));
  let wrong = add_attribute (float_attribute "N" (Array.make count 0.)) source in
  expect_error "invalid_parameter" (Deform.noise_displace ~mode:Deform.Normal_3d
    ~amplitude:0.8 ~frequency:0.16 ~seed:0 wrong);
  expect_error "invalid_parameter" (Deform.noise_displace ~mode:Deform.Normal_3d
    ~amplitude:0.8 ~frequency:Float.max_float ~seed:0 source);
  expect_error "invalid_parameter" (Deform.noise_displace ~grain:0
    ~amplitude:0.8 ~frequency:0.16 ~seed:0 source);
  let cancel = Cancel.create () in Cancel.cancel cancel;
  expect_error "cancelled" (Deform.noise_displace ~cancel ~mode:Deform.Normal_3d
    ~amplitude:0.8 ~frequency:0.16 ~seed:0 source);
  let before = positions normal_quad and after = positions peaked in
  for point = 0 to 3 do
    if not (near (after.x.(point) -. before.x.(point)) (normal.x.(point) *. 0.75)
        && near (after.y.(point) -. before.y.(point)) (normal.y.(point) *. 0.75)
        && near (after.z.(point) -. before.z.(point)) (normal.z.(point) *. 0.75))
    then fail "Peak did not follow point normals"
  done;
  if Geometry.find_attribute ~owner:Attribute.Point "N" peaked <> None then
    fail "Peak retained stale point normals";

  let grid = Plane_generators.grid ~columns:1 ~rows:1 ~size:2. () |> get_ok in
  let point_count = Geometry.point_count grid
  and vertex_count = Geometry.vertex_count grid
  and primitive_count = Geometry.primitive_count grid in
  let direction = float3_attribute ~owner:Attribute.Point "direction"
      (Array.make point_count 2.) (Array.make point_count 0.)
      (Array.make point_count 0.) in
  let directed = add_attribute direction grid in
  let point_group = Group.init ~owner:Group.Point ~name:"one" point_count
      (fun point -> point = 0) in
  let point_peak = Deform.peak ~selection:(Transform_ops.Selected_points point_group)
      ~direction_attribute:"direction" ~distance:1. directed |> get_ok in
  expect_only_points_moved directed point_peak [|0|] 1. 0. 0.;
  let unnormalized = Deform.peak ~selection:(Transform_ops.Selected_points point_group)
      ~direction_attribute:"direction" ~normalize_direction:false ~distance:1.
      directed |> get_ok in
  expect_only_points_moved directed unnormalized [|0|] 2. 0. 0.;

  let vertex_n = float3_attribute ~owner:Attribute.Vertex "N"
      (Array.init vertex_count (fun vertex -> float_of_int (vertex + 1)))
      (Array.make vertex_count 0.) (Array.make vertex_count 0.) in
  let vertex_normal_source = grid
      |> Geometry.without_attribute ~owner:Attribute.Point "N"
      |> add_attribute vertex_n in
  let vertex_normal_peak = Deform.peak ~normalize_direction:false ~distance:1.
      vertex_normal_source |> get_ok in
  let expected_sum = Array.make point_count 0.
  and expected_count = Array.make point_count 0 in
  for vertex = 0 to vertex_count - 1 do
    let point = Topology.point_of_vertex (Geometry.topology grid) vertex in
    expected_sum.(point) <- expected_sum.(point) +. float_of_int (vertex + 1);
    expected_count.(point) <- expected_count.(point) + 1
  done;
  let vertex_before = positions vertex_normal_source
  and vertex_after = positions vertex_normal_peak in
  for point = 0 to point_count - 1 do
    let expected = expected_sum.(point) /. float_of_int expected_count.(point) in
    if not (near (vertex_after.x.(point) -. vertex_before.x.(point)) expected)
        || vertex_after.y.(point) <> vertex_before.y.(point)
        || vertex_after.z.(point) <> vertex_before.z.(point) then
      fail "Peak did not average vertex normals at shared points"
  done;

  let masked_source = directed |> add_attribute
      (float_attribute "mask" [|0.5; 0.; 1.; 1.|]) in
  let masked = Deform.peak ~direction_attribute:"direction" ~mask_attribute:"mask"
      ~distance:2. masked_source |> get_ok in
  let masked_positions = positions masked in
  let masked_source_positions = positions masked_source in
  if masked_positions.x.(0) -. masked_source_positions.x.(0) <> 1.
      || masked_positions.x.(1) <> masked_source_positions.x.(1)
      || masked_positions.x.(2) -. masked_source_positions.x.(2) <> 2.
      || masked_positions.x.(3) -. (positions masked_source).x.(3) <> 2. then
    fail "Peak mask did not scale each point independently";

  let topology = Geometry.topology directed in
  let vertex_point = Topology.point_of_vertex topology 0 in
  let vertices = Group.init ~owner:Group.Vertex ~name:"corner" vertex_count
      (fun vertex -> vertex = 0) in
  let vertex_peak = Deform.peak ~selection:(Transform_ops.Selected_vertices vertices)
      ~direction_attribute:"direction" ~distance:1. directed |> get_ok in
  expect_only_points_moved directed vertex_peak [|vertex_point|] 1. 0. 0.;

  let primitives = Group.init ~owner:Group.Primitive ~name:"face" primitive_count
      (fun primitive -> primitive = 0) in
  let primitive_points =
    let first, last = Topology.primitive_vertex_range topology 0 in
    Array.init (last - first) (fun local -> Topology.point_of_vertex topology
      (first + local)) in
  let primitive_peak = Deform.peak ~selection:(Transform_ops.Selected_primitives primitives)
      ~direction_attribute:"direction" ~distance:1. directed |> get_ok in
  expect_only_points_moved directed primitive_peak primitive_points 1. 0. 0.;

  let index = Topology_index.create topology in
  let edges = Edge_group.init ~topology ~index ~name:"edge" (fun edge -> edge = 0) in
  let edge_a, edge_b = Topology_index.edge_points index 0 in
  let edge_peak = Deform.peak ~selection:(Transform_ops.Selected_edges edges)
      ~direction_attribute:"direction" ~distance:1. directed |> get_ok in
  expect_only_points_moved directed edge_peak [|edge_a; edge_b|] 1. 0. 0.;

  let recomputed = Deform.peak ~direction_attribute:"direction" ~distance:0.2
      ~recompute_normals:true directed |> get_ok in
  let recomputed_n = point_float3 "N" recomputed in
  for point = 0 to point_count - 1 do
    let length = sqrt ((recomputed_n.x.(point) *. recomputed_n.x.(point))
        +. (recomputed_n.y.(point) *. recomputed_n.y.(point))
        +. (recomputed_n.z.(point) *. recomputed_n.z.(point))) in
    if not (near length 1.) then fail "Peak recomputed a non-unit point normal"
  done;

  let extreme_direction = float3_attribute ~owner:Attribute.Point "extreme"
      (Array.make point_count max_float) (Array.make point_count max_float)
      (Array.make point_count max_float) in
  let extreme = add_attribute extreme_direction grid
      |> Deform.peak ~direction_attribute:"extreme" ~distance:1. |> get_ok in
  let extreme_positions = positions extreme in
  if not (Array.for_all Float.is_finite extreme_positions.x
      && Array.for_all Float.is_finite extreme_positions.y
      && Array.for_all Float.is_finite extreme_positions.z) then
    fail "Peak overflowed while normalizing an extreme finite direction";

  expect_error "invalid_deformation"
    (Deform.peak ~direction_attribute:"missing" ~distance:1. grid);
  expect_error "invalid_deformation"
    (Deform.peak ~direction_attribute:"missing" ~distance:0. grid);
  expect_error "invalid_deformation"
    (Deform.peak ~mask_attribute:"missing" ~distance:1. grid);
  let wrong_group = Group.init ~owner:Group.Vertex ~name:"wrong" vertex_count
      (fun _ -> true) in
  expect_error "invalid_deformation"
    (Deform.peak ~selection:(Transform_ops.Selected_points wrong_group)
      ~direction_attribute:"direction" ~distance:1. directed);
  let other = Plane_generators.grid ~columns:1 ~rows:1 ~size:3. () |> get_ok in
  let other_index = Topology_index.create (Geometry.topology other) in
  let other_edges = Edge_group.init ~topology:(Geometry.topology other)
      ~index:other_index ~name:"other" (fun _ -> true) in
  expect_error "invalid_deformation"
    (Deform.peak ~selection:(Transform_ops.Selected_edges other_edges)
      ~direction_attribute:"direction" ~distance:1. directed);
  expect_error "invalid_deformation"
    (Deform.peak ~direction_attribute:"direction" ~distance:Float.infinity directed);

  let mountain_source = Plane_generators.grid ~columns:160 ~rows:120 ~size:12. () |> get_ok in
  let large_count = Geometry.point_count mountain_source in
  let geometric_source = mountain_source
      |> Geometry.without_attribute ~owner:Attribute.Point "N"
      |> Geometry.without_attribute ~owner:Attribute.Vertex "N" in
  let normals domains = Parallel.run ~domains (fun () ->
    Normal_ops.run ~grain:2_048 geometric_source |> get_ok) in
  let normals_one = point_float3 "N" (normals 1)
  and normals_many = point_float3 "N" (normals 4) in
  if normals_one.x <> normals_many.x || normals_one.y <> normals_many.y
      || normals_one.z <> normals_many.z then
    fail "Normals differ between one and four domains";
  let middle = Group.init ~owner:Group.Point ~name:"middle" large_count
      (fun point -> point mod 3 <> 0) in
  let mountain domains seed = Parallel.run ~domains (fun () ->
    Deform.mountain ~grain:2_048 ~selection:(Transform_ops.Selected_points middle) ~seed
      ~height:1.25 ~frequency:(Rays_math.Vec3.create 0.35 0.7 0.55)
      ~offset:(Rays_math.Vec3.create 1. 2. 3.) ~octaves:6 ~lacunarity:2.1
      ~roughness:0.47 ~height_attribute:"height" mountain_source |> get_ok) in
  let one = mountain 1 73 and many = mountain 4 73 in
  if not (equal_positions one many)
      || point_float "height" one <> point_float "height" many then
    fail "Mountain differs between one and four domains";
  let source_positions = positions mountain_source and output_positions = positions one
  and heights = point_float "height" one in
  let moved = ref false in
  for point = 0 to large_count - 1 do
    if Group.mem point middle then begin
      if heights.(point) <> 0. then moved := true
    end else if output_positions.x.(point) <> source_positions.x.(point)
        || output_positions.y.(point) <> source_positions.y.(point)
        || output_positions.z.(point) <> source_positions.z.(point)
        || heights.(point) <> 0. then
      fail "Mountain changed an unselected point or height"
  done;
  if not !moved then fail "Mountain produced no displacement";
  let another_seed = mountain 1 74 in
  if equal_positions one another_seed then fail "Mountain ignored its seed";

  let zero_normal = float3_attribute ~owner:Attribute.Point "N"
      (Array.make point_count 0.) (Array.make point_count 0.)
      (Array.make point_count 0.) in
  let zero_normal_source = grid |> add_attribute zero_normal in
  let zero_normal_mountain = Deform.mountain ~seed:17 ~height:2.
      zero_normal_source |> get_ok in
  if not (equal_positions zero_normal_source zero_normal_mountain) then
    fail "Mountain moved points whose normals are zero";

  let mountain_normals = Deform.mountain ~seed:4 ~height:0.2
      ~recompute_normals:true grid |> get_ok in
  if Geometry.find_attribute ~owner:Attribute.Point "N" mountain_normals = None
      || Geometry.find_attribute ~owner:Attribute.Vertex "N" mountain_normals <> None
  then fail "Mountain normal recomputation contract";

  List.iter (fun result -> expect_error "invalid_deformation" result) [
    Deform.mountain ~height:1. ~octaves:0 grid;
    Deform.mountain ~height:1. ~octaves:65 grid;
    Deform.mountain ~height:1. ~lacunarity:0. grid;
    Deform.mountain ~height:1. ~roughness:(-0.1) grid;
    Deform.mountain ~height:1. ~roughness:1.1 grid;
    Deform.mountain ~height:Float.nan grid;
    Deform.mountain ~height:1. ~mask_attribute:"missing" grid;
  ];
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_error "cancelled" (Deform.peak ~cancel:cancelled
    ~direction_attribute:"direction" ~distance:1. directed);
  expect_error "cancelled" (Deform.mountain ~cancel:cancelled ~height:1. grid);
  print_endline "deform tests passed"
