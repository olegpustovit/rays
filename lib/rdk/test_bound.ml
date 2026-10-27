open Rays
open Rdk
open Rdk_test_support

let detail_float3 geometry name =
  match Geometry.find_attribute ~owner:Attribute.Detail name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing detail float3 " ^ name)

let point_normals geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail "N has unexpected storage")
  | None -> fail "missing point normals"

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right)
  and ln = point_normals left and rn = point_normals right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && lt.primitive_kinds = rt.primitive_kinds
  && ln.x = rn.x && ln.y = rn.y && ln.z = rn.z
  && (match Geometry.find_group ~owner:Group.Primitive "bounds" left,
      Geometry.find_group ~owner:Group.Primitive "bounds" right with
      | Some left_group, Some right_group ->
          Group.cardinality left_group = Group.cardinality right_group
          && Group.cardinality left_group = Geometry.primitive_count left
      | None, None -> true | _ -> false)

let check_divided_box () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 2. 3. 4.) () |> get_ok
      |> Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 3. (-2.) 5.)) in
  let output = Bound.run ~shape:(Bound.Bound_box { divisions = 2, 3, 4 })
      ~lower_padding:(Rays_math.Vec3.create 1. 2. 3.)
      ~upper_padding:(Rays_math.Vec3.create 0.5 1. 1.5) ~bounds_group:"bounds"
      ~center_attribute:"bound_center" ~radii_attribute:"bound_radii" source
      |> get_ok in
  check (Geometry.point_count output = 94
      && Geometry.vertex_count output = 312
      && Geometry.primitive_count output = 104)
    "divided Bound box cardinality";
  (match Analysis.bounds output with
   | Some bounds ->
       check (near bounds.min.x 1. && near bounds.min.y (-5.5)
           && near bounds.min.z 0. && near bounds.max.x 4.5
           && near bounds.max.y 0.5 && near bounds.max.z 8.5)
         "divided Bound box asymmetric padding"
   | None -> fail "divided Bound box has no bounds");
  let center = detail_float3 output "bound_center"
  and radii = detail_float3 output "bound_radii" in
  check (near center.x.(0) 2.75 && near center.y.(0) (-2.5)
      && near center.z.(0) 4.25 && near radii.x.(0) 1.75
      && near radii.y.(0) 3. && near radii.z.(0) 4.25)
    "Bound detail center/radii metadata";
  (match Geometry.find_group ~owner:Group.Primitive "bounds" output with
   | Some group -> check (Group.cardinality group = 104)
       "Bound primitive output group"
   | None -> fail "Bound primitive output group missing");
  let p = positions output and topology = Topology.Private.view
      (Geometry.topology output) in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    let first = topology.primitive_offsets.(primitive) in
    let a = topology.vertex_points.(first)
    and b = topology.vertex_points.(first + 1)
    and c = topology.vertex_points.(first + 2) in
    let ux = p.x.(b) -. p.x.(a) and uy = p.y.(b) -. p.y.(a)
    and uz = p.z.(b) -. p.z.(a) and vx = p.x.(c) -. p.x.(a)
    and vy = p.y.(c) -. p.y.(a) and vz = p.z.(c) -. p.z.(a) in
    let nx = (uy *. vz) -. (uz *. vy)
    and ny = (uz *. vx) -. (ux *. vz)
    and nz = (ux *. vy) -. (uy *. vx) in
    let cx = ((p.x.(a) +. p.x.(b) +. p.x.(c)) /. 3.) -. 2.75
    and cy = ((p.y.(a) +. p.y.(b) +. p.y.(c)) /. 3.) +. 2.5
    and cz = ((p.z.(a) +. p.z.(b) +. p.z.(c)) /. 3.) -. 4.25 in
    check ((nx *. cx) +. (ny *. cy) +. (nz *. cz) > 0.)
      "divided Bound box winding"
  done

let check_typed_selection () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let faces = Group.init ~owner:Group.Primitive ~name:"positive_x" 12
      (fun primitive -> primitive < 2) in
  let output = Bound.run ~selection:(Transform_ops.Selected_primitives faces)
      ~lower_padding:(Rays_math.Vec3.create 0.1 0. 0.)
      ~upper_padding:(Rays_math.Vec3.create 0.1 0. 0.) source |> get_ok in
  (match Analysis.bounds output with
   | Some bounds -> check (near bounds.center.x 1. && near bounds.size.x 0.2
       && near bounds.size.y 2. && near bounds.size.z 2.)
       "Bound primitive selection"
   | None -> fail "selected Bound output empty")

let check_sphere () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let output = Bound.run
      ~shape:(Bound.Bound_sphere { segments = 16; rings = 8; minimum_radius = 0. })
      ~lower_padding:(Rays_math.Vec3.create 0.2 0.4 0.6)
      ~upper_padding:(Rays_math.Vec3.create 0.6 0.4 0.2)
      ~center_attribute:"center" ~radii_attribute:"radii" source |> get_ok in
  check (Geometry.point_count output = 114
      && Geometry.primitive_count output = 224)
    "Bound sphere cardinality";
  let center = detail_float3 output "center"
  and radii = detail_float3 output "radii" in
  let base = sqrt 3. in
  check (near center.x.(0) 0.2 && near center.y.(0) 0.
      && near center.z.(0) (-0.2) && near radii.x.(0) (base +. 0.4)
      && near radii.y.(0) (base +. 0.4)
      && near radii.z.(0) (base +. 0.4))
    "Bound sphere padding/metadata";
  let point = Line_geometry.points [|(4.,5.,6.)|] in
  let minimum = Bound.run
      ~shape:(Bound.Bound_sphere { segments = 8; rings = 4; minimum_radius = 2. })
      point |> get_ok in
  (match Analysis.bounds minimum with
   | Some bounds -> check (near bounds.center.x 4. && near bounds.size.y 4.)
       "Bound sphere minimum radius"
   | None -> fail "minimum-radius Bound sphere empty")

let check_validation () =
  let source = Line_geometry.points [|(0.,0.,0.)|] in
  expect_code "invalid_geometry" (Bound.run source);
  expect_code "invalid_geometry" (Bound.run
      ~shape:(Bound.Bound_box { divisions = 0, 1, 1 }) source);
  expect_code "invalid_geometry" (Bound.run
      ~shape:(Bound.Bound_sphere { segments = 2; rings = 1;
        minimum_radius = -1. }) source);
  expect_code "invalid_geometry" (Bound.run
      ~lower_padding:(Rays_math.Vec3.create Float.nan 0. 0.) source);
  expect_code "invalid_geometry" (Bound.run ~center_attribute:"same"
      ~radii_attribute:"same" source);
  expect_code "invalid_geometry" (Bound.run ~bounds_group:"" source);
  expect_code "invalid_geometry" (Bound.run
      ~shape:(Bound.Bound_box { divisions = max_int, max_int, max_int })
      ~lower_padding:(Rays_math.Vec3.create 1. 1. 1.) source);
  let empty = Group.init ~owner:Group.Point ~name:"empty" 1 (fun _ -> false) in
  expect_code "invalid_geometry" (Bound.run
      ~selection:(Transform_ops.Selected_points empty) source);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Bound.run ~cancel:cancelled
      ~shape:(Bound.Bound_sphere { segments = 64; rings = 32; minimum_radius = 0. })
      source)

let check_parallel_exact () =
  let source = Plane_generators.grid ~columns:500 ~rows:300 ~size:30. () |> get_ok
      |> Deform.noise_displace ~seed:929 ~amplitude:2. ~frequency:0.23 |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
    Bound.run ~grain:1024
      ~shape:(Bound.Bound_box { divisions = 256, 128, 64 })
      ~lower_padding:(Rays_math.Vec3.create 0.25 0.5 0.75)
      ~upper_padding:(Rays_math.Vec3.create 0.75 0.5 0.25)
      ~bounds_group:"bounds" source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Bound geometry differ";
  check (Geometry.point_count one = 116_486
      && Geometry.primitive_count one = 229_376)
    "Bound scale cardinality";
  let run_sphere domains = Parallel.run ~domains (fun () ->
    Bound.run ~grain:1024 ~shape:(Bound.Bound_sphere {
        segments = 512; rings = 256; minimum_radius = 0. })
      ~bounds_group:"bounds" source |> get_ok) in
  let sphere_one = run_sphere 1 and sphere_many = run_sphere 4 in
  check (equal_geometry sphere_one sphere_many)
    "one-domain and four-domain Bound sphere differ";
  check (Geometry.point_count sphere_one = 130_562
      && Geometry.primitive_count sphere_one = 261_120)
    "Bound sphere scale cardinality"

let run () =
  check_divided_box ();
  check_typed_selection ();
  check_sphere ();
  check_validation ();
  check_parallel_exact ();
  print_endline "bound tests passed"
