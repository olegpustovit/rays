open Rays
open Rdk
open Rdk_test_support

let equal_storage left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

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
  && List.equal equal_attribute (Geometry.attributes left) (Geometry.attributes right)

let check_default_compatibility () =
  let geometry = Plane_generators.circle ~segments:4 ~radius:2. () |> get_ok in
  let point = positions geometry
  and topology = Topology.Private.view (Geometry.topology geometry) in
  let step = Float.pi *. 2. /. 4. in
  check (point.x = Array.init 4 (fun index -> 2. *. cos (float_of_int index *. step))
      && point.y = Array.make 4 0.
      && point.z = Array.init 4 (fun index -> 2. *. sin (float_of_int index *. step)))
    "default Circle point ordering changed";
  check (topology.vertex_points = [|0;1;2;3|]
      && topology.primitive_offsets = [|0;4|]
      && topology.primitive_kinds = Bytes.make 1 '\002')
    "default Circle topology changed"

let check_arc_modes () =
  let make arc = Plane_generators.circle ~arc ~segments:2 ~radius:2. () |> get_ok in
  let open_arc = make (Plane_generators.Circle_open_arc {
      start_angle = 0.; end_angle = Float.pi *. 0.5 }) in
  let open_points = positions open_arc in
  check (Geometry.point_count open_arc = 3
      && Geometry.vertex_count open_arc = 3
      && Geometry.primitive_count open_arc = 1
      && Topology.primitive_kind (Geometry.topology open_arc) 0
         = Topology.Open_polyline
      && near open_points.x.(0) 2. && near open_points.z.(0) 0.
      && near open_points.x.(2) 0. && near open_points.z.(2) 2.)
    "Circle open arc topology/endpoints";
  let closed_arc = make (Plane_generators.Circle_closed_arc {
      start_angle = 0.; end_angle = Float.pi *. 0.5 }) in
  check (Geometry.point_count closed_arc = 3
      && Topology.primitive_kind (Geometry.topology closed_arc) 0
         = Topology.Closed_polyline)
    "Circle chord-closed arc topology";
  let sliced = make (Plane_generators.Circle_sliced_arc {
      start_angle = 0.; end_angle = Float.pi *. 0.5 }) in
  let sliced_points = positions sliced in
  check (Geometry.point_count sliced = 4
      && Topology.primitive_kind (Geometry.topology sliced) 0
         = Topology.Closed_polyline
      && sliced_points.x.(3) = 0. && sliced_points.y.(3) = 0.
      && sliced_points.z.(3) = 0.)
    "Circle sliced arc center/topology";
  let reversed = Plane_generators.circle ~reverse:true
      ~arc:(Plane_generators.Circle_open_arc {
        start_angle = 0.; end_angle = Float.pi *. 0.5 })
      ~segments:2 ~radius:2. () |> get_ok |> positions in
  check (near reversed.x.(0) 0. && near reversed.z.(0) 2.
      && near reversed.x.(2) 2. && near reversed.z.(2) 0.)
    "Circle reverse traversal"

let check_orientation_and_ellipse () =
  let ellipse = Plane_generators.circle ~orientation:Plane_generators.Circle_xy
      ~center:(Rays_math.Vec3.create 1. 2. 3.) ~radius_x:3. ~radius_y:1.
      ~uniform_scale:2. ~rotation:(Float.pi *. 0.5)
      ~segments:64 ~radius:1. () |> get_ok in
  let bounds = Analysis.bounds ellipse |> Option.get in
  check (near bounds.center.x 1. && near bounds.center.y 2.
      && near bounds.center.z 3. && near bounds.size.x 4.
      && near bounds.size.y 12. && near bounds.size.z 0.)
    "Circle XY ellipse dimensions/center/rotation";
  let yz = Plane_generators.circle ~orientation:Plane_generators.Circle_yz
      ~radius_x:4. ~radius_y:2. ~segments:64 ~radius:1. () |> get_ok
      |> Analysis.bounds |> Option.get in
  check (near yz.size.x 0. && near yz.size.y 8. && near yz.size.z 4.)
    "Circle YZ orientation";
  let custom = Plane_generators.circle ~orientation:(Plane_generators.Circle_axes {
        horizontal = Rays_math.Vec3.create max_float max_float 0.;
        vertical = Rays_math.Vec3.create 0. 0. max_float })
      ~center:(Rays_math.Vec3.create 4. 5. 6.) ~radius_x:3. ~radius_y:2.
      ~segments:128 ~radius:1. () |> get_ok in
  let point = positions custom in
  let dx = point.x.(0) -. 4. and dy = point.y.(0) -. 5.
  and dz = point.z.(0) -. 6. in
  check (near ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) 9.)
    "Circle robust custom plane frame"

let check_validation () =
  expect_code "invalid_parameter"
    (Plane_generators.circle ~grain:0 ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~segments:2 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~arc:(Plane_generators.Circle_open_arc {
       start_angle=0.; end_angle=1. }) ~segments:0 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~arc:(Plane_generators.Circle_closed_arc {
       start_angle=0.; end_angle=1. }) ~segments:1 ~radius:1. ());
  expect_code "invalid_parameter" (Plane_generators.circle ~segments:3 ~radius:0. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~radius_x:Float.nan ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~uniform_scale:(-1.) ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~center:(Rays_math.Vec3.create Float.nan 0. 0.)
       ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~rotation:Float.infinity ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~arc:(Plane_generators.Circle_open_arc {
       start_angle=Float.nan; end_angle=1. }) ~segments:2 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~arc:(Plane_generators.Circle_sliced_arc {
       start_angle=1.; end_angle=1. }) ~segments:2 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~arc:(Plane_generators.Circle_open_arc {
       start_angle=(-.max_float); end_angle=max_float })
       ~segments:2 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~orientation:(Plane_generators.Circle_axes {
       horizontal=Rays_math.Vec3.zero; vertical=Rays_math.Vec3.unit_z })
       ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~orientation:(Plane_generators.Circle_axes {
       horizontal=Rays_math.Vec3.unit_x; vertical=Rays_math.Vec3.unit_x })
       ~segments:3 ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~segments:max_int ~radius:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.circle ~center:(Rays_math.Vec3.create max_float 0. 0.) ~radius_x:max_float
       ~segments:3 ~radius:1. ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Plane_generators.circle ~cancel:cancelled ~segments:500_000 ~radius:1. ())

let check_parallel_exact () =
  let arcs = [
    Plane_generators.Circle_closed;
    Plane_generators.Circle_open_arc { start_angle = -0.7; end_angle = 5.2 };
    Plane_generators.Circle_closed_arc { start_angle = 0.3; end_angle = 4.8 };
    Plane_generators.Circle_sliced_arc { start_angle = -1.2; end_angle = 2.7 } ] in
  List.iter (fun arc ->
    let run domains = Parallel.run ~domains (fun () ->
      Plane_generators.circle ~grain:1024 ~arc ~reverse:true
        ~orientation:(Plane_generators.Circle_axes {
          horizontal = Rays_math.Vec3.create 1. 2. 0.5;
          vertical = Rays_math.Vec3.create (-0.25) 0.75 2. })
        ~center:(Rays_math.Vec3.create 3. (-2.) 5.) ~radius_x:40. ~radius_y:25.
        ~rotation:0.37 ~uniform_scale:1.2 ~segments:500_000 ~radius:1. ()
      |> get_ok) in
    let one = run 1 and many = run 4 in
    check (equal_geometry one many)
      "one-domain and four-domain Circle geometry differ";
    check (Geometry.point_count one >= 500_000)
      "Circle scale fixture cardinality") arcs

let run () =
  check_default_compatibility ();
  check_arc_modes ();
  check_orientation_and_ellipse ();
  check_validation ();
  check_parallel_exact ();
  print_endline "circle tests passed"
