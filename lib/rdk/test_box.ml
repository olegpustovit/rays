open Rays
open Rdk
open Rdk_test_support

let attribute_storage geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> Attribute.View.storage attribute
  | None -> fail ("missing attribute " ^ name)

let float2_attribute geometry owner name = match attribute_storage geometry owner name with
  | Attribute.Float2 values -> Packed.Float2.View.view values
  | _ -> fail (name ^ " has unexpected storage")

let float3_attribute geometry owner name = match attribute_storage geometry owner name with
  | Attribute.Float3 values -> Packed.Float3.View.view values
  | _ -> fail (name ^ " has unexpected storage")

let equal_storage left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

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
  and left_topology = Topology.View.view (Geometry.topology left)
  and right_topology = Topology.View.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)

let check_default_compatibility () =
  let geometry = Box_generator.box ~size:(Rays_math.Vec3.create 2. 4. 6.) () |> get_ok in
  let point = positions geometry
  and topology = Topology.View.view (Geometry.topology geometry)
  and normal = float3_attribute geometry Attribute.Point "N" in
  check (Geometry.point_count geometry = 24
      && Geometry.vertex_count geometry = 36
      && Geometry.primitive_count geometry = 12)
    "default Box cardinality changed";
  check (Array.sub point.x 0 4 = [|1.;1.;1.;1.|]
      && Array.sub point.y 0 4 = [|-2.;2.;2.;-2.|]
      && Array.sub point.z 0 4 = [|-3.;-3.;3.;3.|])
    "default Box first face changed";
  check (Array.sub topology.vertex_points 0 6 = [|0;1;2;0;2;3|]
      && topology.primitive_offsets = Array.init 13 (fun index -> index * 3)
      && topology.primitive_kinds = Bytes.make 12 '\000')
    "default Box topology changed";
  check (Array.sub normal.x 0 4 = [|1.;1.;1.;1.|]
      && Array.sub normal.y 0 4 = [|0.;0.;0.;0.|]
      && Array.sub normal.z 0 4 = [|0.;0.;0.;0.|])
    "default Box normals changed"

let check_divisions_connectivity_and_groups () =
  let quads = Box_generator.box ~connectivity:Box_generator.Box_quads
      ~normals:Box_generator.Box_vertex_normals ~uv_attribute:"uv" ~face_groups:"face"
      ~x_divisions:2 ~y_divisions:3 ~z_divisions:4
      ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok in
  check (Geometry.point_count quads = 94
      && Geometry.vertex_count quads = 208
      && Geometry.primitive_count quads = 52)
    "divided face-local quad Box cardinality";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" quads = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" quads <> None)
    "Box vertex-normal ownership";
  let uv = float2_attribute quads Attribute.Vertex "uv" in
  check (Array.sub uv.x 0 4 = [|0.; 1./.3.; 1./.3.; 0.|]
      && Array.sub uv.y 0 4 = [|0.; 0.; 0.25; 0.25|])
    "Box per-face normalized UVs";
  let expected = [
    "face__right", 12; "face__left", 12;
    "face__top", 8; "face__bottom", 8;
    "face__front", 6; "face__back", 6 ] in
  check (List.map (fun group -> Group.name group, Group.cardinality group)
      (Geometry.groups quads) = expected)
    "Box face-group ranges";
  let shared = Box_generator.box ~connectivity:Box_generator.Box_triangles
      ~consolidate_points:true ~normals:Box_generator.Box_vertex_normals
      ~x_divisions:2 ~y_divisions:3 ~z_divisions:4
      ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok in
  check (Geometry.point_count shared = 54
      && Geometry.vertex_count shared = 312
      && Geometry.primitive_count shared = 104)
    "divided welded triangle Box cardinality";
  let topology = Topology.View.view (Geometry.topology shared)
  and point = positions shared
  and normal = float3_attribute shared Attribute.Vertex "N" in
  for primitive = 0 to Geometry.primitive_count shared - 1 do
    let first = topology.primitive_offsets.(primitive) in
    let a = topology.vertex_points.(first)
    and b = topology.vertex_points.(first + 1)
    and c = topology.vertex_points.(first + 2) in
    let ux = point.x.(b) -. point.x.(a)
    and uy = point.y.(b) -. point.y.(a)
    and uz = point.z.(b) -. point.z.(a)
    and vx = point.x.(c) -. point.x.(a)
    and vy = point.y.(c) -. point.y.(a)
    and vz = point.z.(c) -. point.z.(a) in
    let nx = (uy *. vz) -. (uz *. vy)
    and ny = (uz *. vx) -. (ux *. vz)
    and nz = (ux *. vy) -. (uy *. vx) in
    check ((nx *. normal.x.(first)) +. (ny *. normal.y.(first))
        +. (nz *. normal.z.(first)) > 0.)
      "Box winding disagrees with hard vertex N"
  done

let check_point_modes () =
  let face_local = Box_generator.box ~connectivity:Box_generator.Box_surface_points
      ~x_divisions:2 ~y_divisions:3 ~z_divisions:4
      ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok in
  check (Geometry.point_count face_local = 94
      && Geometry.vertex_count face_local = 0
      && Geometry.attributes face_local = [])
    "Box face-local surface points";
  let welded = Box_generator.box ~connectivity:Box_generator.Box_surface_points
      ~consolidate_points:true ~normals:Box_generator.Box_point_normals
      ~x_divisions:2 ~y_divisions:3 ~z_divisions:4
      ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok in
  let normals = float3_attribute welded Attribute.Point "N" in
  check (Geometry.point_count welded = 54
      && near (sqrt ((normals.x.(0) *. normals.x.(0))
          +. (normals.y.(0) *. normals.y.(0))
          +. (normals.z.(0) *. normals.z.(0)))) 1.)
    "Box welded surface points/smooth normals";
  let inverse_sqrt_three = 1. /. sqrt 3. in
  check (near normals.x.(0) (-.inverse_sqrt_three)
      && near normals.y.(0) (-.inverse_sqrt_three)
      && near normals.z.(0) (-.inverse_sqrt_three)
      && near normals.x.(3) (-.(1. /. sqrt 2.))
      && near normals.y.(3) 0.
      && near normals.z.(3) (-.(1. /. sqrt 2.))
      && near normals.x.(4) 0. && near normals.y.(4) 0.
      && near normals.z.(4) (-1.))
    "Box welded normals do not average incident faces";
  let lattice = Box_generator.box ~connectivity:Box_generator.Box_lattice_points
      ~center:(Rays_math.Vec3.create 1. 2. 3.)
      ~x_divisions:2 ~y_divisions:3 ~z_divisions:4
      ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok in
  let point = positions lattice in
  check (Geometry.point_count lattice = 60
      && Geometry.vertex_count lattice = 0
      && point.x.(0) = 0. && point.y.(0) = 0.5 && point.z.(0) = 1.
      && point.x.(59) = 2. && point.y.(59) = 3.5 && point.z.(59) = 5.)
    "Box volume-lattice points/order"

let check_transform_and_rotation_order () =
  let transformed = Box_generator.box ~connectivity:Box_generator.Box_quads
      ~center:(Rays_math.Vec3.create 4. 5. 6.)
      ~rotation:(Rays_math.Vec3.create 0. 0. (Float.pi *. 0.5))
      ~uniform_scale:0.5 ~size:(Rays_math.Vec3.create 2. 4. 6.) () |> get_ok
      |> Analysis.bounds |> Option.get in
  check (near transformed.center.x 4. && near transformed.center.y 5.
      && near transformed.center.z 6. && near transformed.size.x 2.
      && near transformed.size.y 1. && near transformed.size.z 3.)
    "Box center/rotation/uniform-scale bounds";
  let rotation = Rays_math.Vec3.create 0.3 0.5 0.7 in
  let first order = Box_generator.box ~connectivity:Box_generator.Box_surface_points
      ~rotation ~rotation_order:order ~size:(Rays_math.Vec3.create 2. 4. 6.) ()
      |> get_ok |> positions |> fun values ->
      Rays_math.Vec3.create values.x.(0) values.y.(0) values.z.(0) in
  let source = Rays_math.Vec3.create 1. (-2.) (-3.) in
  let expected = Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_x rotation.x))
      |> fun matrix -> Rays_math.Mat4.transform_point matrix source in
  let actual = first Box_generator.Box_xyz in
  check (near actual.x expected.x && near actual.y expected.y
      && near actual.z expected.z)
    "Box XYZ rotation order";
  let other = first Box_generator.Box_zyx in
  check (not (near actual.x other.x && near actual.y other.y
      && near actual.z other.z))
    "Box rotation orders collapsed to one order"

let check_validation () =
  let size = Rays_math.Vec3.create 1. 1. 1. in
  expect_code "invalid_parameter" (Box_generator.box ~grain:0 ~size ());
  expect_code "invalid_parameter" (Box_generator.box ~x_divisions:0 ~size ());
  expect_code "invalid_parameter" (Box_generator.box ~x_divisions:max_int ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~size:(Rays_math.Vec3.create Float.nan 1. 1.) ());
  expect_code "invalid_parameter" (Box_generator.box ~uniform_scale:0. ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~center:(Rays_math.Vec3.create Float.nan 0. 0.) ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~rotation:(Rays_math.Vec3.create Float.infinity 0. 0.) ~size ());
  expect_code "invalid_parameter" (Box_generator.box ~uv_attribute:"N" ~size ());
  expect_code "invalid_parameter" (Box_generator.box ~face_groups:" " ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~connectivity:Box_generator.Box_surface_points ~uv_attribute:"uv" ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~connectivity:Box_generator.Box_surface_points ~face_groups:"face" ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~connectivity:Box_generator.Box_surface_points
       ~normals:Box_generator.Box_vertex_normals ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~connectivity:Box_generator.Box_lattice_points
       ~normals:Box_generator.Box_point_normals ~size ());
  expect_code "invalid_parameter"
    (Box_generator.box ~center:(Rays_math.Vec3.create max_float 0. 0.)
       ~size:(Rays_math.Vec3.create max_float 1. 1.) ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Box_generator.box ~cancel:cancelled ~connectivity:Box_generator.Box_quads
       ~x_divisions:500 ~y_divisions:500 ~z_divisions:500 ~size ())

let check_parallel_exact () =
  let run domains make = Parallel.run ~domains (fun () -> make () |> get_ok) in
  let cases = [
    (fun () -> Box_generator.box ~grain:1024 ~connectivity:Box_generator.Box_triangles
      ~normals:Box_generator.Box_point_normals ~x_divisions:256 ~y_divisions:192
      ~z_divisions:128 ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7) ~rotation_order:Box_generator.Box_yzx
      ~size:(Rays_math.Vec3.create 40. 25. 18.) ());
    (fun () -> Box_generator.box ~grain:1024 ~connectivity:Box_generator.Box_quads
      ~consolidate_points:true ~normals:Box_generator.Box_vertex_normals
      ~uv_attribute:"uv" ~face_groups:"face"
      ~x_divisions:256 ~y_divisions:192 ~z_divisions:128
      ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7) ~rotation_order:Box_generator.Box_zxy
      ~size:(Rays_math.Vec3.create 40. 25. 18.) ());
    (fun () -> Box_generator.box ~grain:1024 ~connectivity:Box_generator.Box_surface_points
      ~consolidate_points:true ~normals:Box_generator.Box_point_normals
      ~x_divisions:256 ~y_divisions:192 ~z_divisions:128
      ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7)
      ~size:(Rays_math.Vec3.create 40. 25. 18.) ());
    (fun () -> Box_generator.box ~grain:1024 ~connectivity:Box_generator.Box_lattice_points
      ~x_divisions:100 ~y_divisions:80 ~z_divisions:60
      ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7)
      ~size:(Rays_math.Vec3.create 40. 25. 18.) ()) ] in
  List.iter (fun make ->
    let one = run 1 make and many = run 4 make in
    check (equal_geometry one many)
      "one-domain and four-domain Box geometry differ") cases

let run () =
  check_default_compatibility ();
  check_divisions_connectivity_and_groups ();
  check_point_modes ();
  check_transform_and_rotation_order ();
  check_validation ();
  check_parallel_exact ();
  print_endline "box tests passed"
