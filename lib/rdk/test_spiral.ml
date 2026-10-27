open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

(* v + 2w (q x v) + 2 q x (q x v) for a unit quaternion. *)
let rotate (q : Rays_math.Quat.t) (v : Rays_math.Vec3.t) =
  let cross (ax, ay, az) (bx, by, bz) =
    (ay *. bz) -. (az *. by), (az *. bx) -. (ax *. bz), (ax *. by) -. (ay *. bx) in
  let u = q.x, q.y, q.z in
  let tx, ty, tz = cross u (v.x, v.y, v.z) in
  let cx, cy, cz = cross u (tx, ty, tz) in
  Rays_math.Vec3.create (v.x +. 2. *. ((q.w *. tx) +. cx)) (v.y +. 2. *. ((q.w *. ty) +. cy))
    (v.z +. 2. *. ((q.w *. tz) +. cz))

let float_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float3_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float4_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float4 values -> Packed.Float4.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
       && equal_storage left right) (Geometry.attributes left)
       (Geometry.attributes right)

let make ?grain ?extent ?radius ?height_ramp ?radius_scale ?radius_ramp
    ?direction ?start_angle ?divisions ?uniform_angle ?spiral_count
    ?orientation ?center ?rotation ?rotation_order ?uniform_scale
    ?angle_attribute ?x_axis_attribute ?y_axis_attribute ?tangent_attribute
    ?orient_attribute ?distance_attribute () =
  Spiral.run ?grain ?extent ?radius ?height_ramp ?radius_scale ?radius_ramp
    ?direction ?start_angle ?divisions ?uniform_angle ?spiral_count
    ?orientation ?center ?rotation ?rotation_order ?uniform_scale
    ?angle_attribute ?x_axis_attribute ?y_axis_attribute ?tangent_attribute
    ?orient_attribute ?distance_attribute () |> get_ok

let check_default_and_extent () =
  let geometry = make () and topology = make () |> Geometry.topology in
  let point = positions geometry in
  check (Geometry.point_count geometry = 97
      && Geometry.vertex_count geometry = 97
      && Geometry.primitive_count geometry = 1)
    "default Spiral cardinality";
  check (Topology.primitive_kind topology 0 = Topology.Open_polyline
      && near point.x.(0) 1. && near point.y.(0) 0. && near point.z.(0) 0.
      && near point.x.(96) 1. && near point.y.(96) 2.
      && near point.z.(96) 0.)
    "default Spiral endpoints/topology";
  let pitch = make ~extent:(Spiral.Spiral_height_pitch { height = 6.; pitch = 1.5 })
      ~divisions:(Spiral.Spiral_divisions_per_turn 8) () in
  check (Geometry.point_count pitch = 33 && near (positions pitch).y.(32) 6.)
    "height/pitch Spiral extent or divisions"

let check_radius_families_ramps_and_count () =
  let arch = make ~extent:(Spiral.Spiral_turns { turns = 2.5; height = -4. })
      ~radius:(Spiral.Spiral_archimedean_end { start_radius = 1.; end_radius = 3. })
      ~height_ramp:[0., 1.; 0.5, 0.25; 1., 1.]
      ~radius_scale:2. ~radius_ramp:[0., 1.; 1., 0.5]
      ~direction:Spiral.Spiral_clockwise ~start_angle:0.3
      ~divisions:(Spiral.Spiral_divisions_per_curve 10) ~spiral_count:3 () in
  let p = positions arch in
  check (Geometry.point_count arch = 33 && Geometry.primitive_count arch = 3)
    "multi-Spiral cardinality";
  let radius point = sqrt ((p.x.(point) *. p.x.(point))
      +. (p.z.(point) *. p.z.(point))) in
  check (near (radius 0) 2. && near (radius 10) 3.
      && near p.y.(5) (-0.5) && near p.y.(10) (-4.))
    "Archimedean/ramp evaluation";
  let phase0 = atan2 p.z.(0) p.x.(0)
  and phase1 = atan2 p.z.(11) p.x.(11) in
  check (near phase0 0.3 && near phase1 (0.3 +. (2. *. Float.pi /. 3.)))
    "spiral phase distribution";
  let logarithmic = make ~extent:(Spiral.Spiral_turns { turns = 3.; height = 0. })
      ~radius:(Spiral.Spiral_logarithmic_change {
        start_radius = 2.; scale_per_turn = 2. })
      ~divisions:(Spiral.Spiral_divisions_per_curve 6) () in
  let p = positions logarithmic in
  check (near p.x.(0) 2. && near p.x.(6) 16.)
    "logarithmic change-per-turn radius";
  let logarithmic_end = make
      ~radius:(Spiral.Spiral_logarithmic_end { start_radius = 2.; end_radius = 8. })
      ~divisions:(Spiral.Spiral_divisions_per_curve 2) () |> positions in
  let middle_radius = sqrt ((logarithmic_end.x.(1) *. logarithmic_end.x.(1))
      +. (logarithmic_end.z.(1) *. logarithmic_end.z.(1))) in
  check (near middle_radius 4.) "explicit logarithmic end radius"

let check_equal_arc_spacing () =
  let divisions = 400 in
  let make_spacing uniform_angle =
    make ~extent:(Spiral.Spiral_turns { turns = 2.3; height = 4. })
      ~radius:(Spiral.Spiral_archimedean_end { start_radius = 0.3; end_radius = 4. })
      ~height_ramp:[0., 1.; 0.347, 0.45; 1., 1.2]
      ~radius_ramp:[0., 0.8; 0.613, 1.3; 1., 0.7]
      ~uniform_angle ~divisions:(Spiral.Spiral_divisions_per_curve divisions)
      ~distance_attribute:"distance" () in
  let segment_ratio geometry =
    let distance = float_attribute geometry "distance" in
    let minimum = ref max_float and maximum = ref 0. in
    for point = 1 to Array.length distance - 1 do
      let segment = distance.(point) -. distance.(point - 1) in
      minimum := min !minimum segment; maximum := max !maximum segment
    done;
    !maximum /. !minimum, distance
  in
  let equal_ratio, distance = make_spacing false |> segment_ratio
  and angle_ratio, _ = make_spacing true |> segment_ratio in
  check (equal_ratio < 1.006
      && equal_ratio -. 1. < (angle_ratio -. 1.) *. 0.1)
    (Printf.sprintf
      "equal-arc Spiral spacing ratio %.9g did not improve angle spacing %.9g"
      equal_ratio angle_ratio);
  check (distance.(0) = 0. && distance.(divisions) > 0.)
    "Spiral distance output"

let check_frames_and_composition () =
  let geometry = make ~extent:(Spiral.Spiral_turns { turns = 2.; height = 3. })
      ~radius:(Spiral.Spiral_archimedean_change {
        start_radius = 0.5; increase_per_turn = 0.3 })
      ~divisions:(Spiral.Spiral_divisions_per_curve 64) ~spiral_count:2
      ~angle_attribute:"angle" ~x_axis_attribute:"xaxis"
      ~y_axis_attribute:"yaxis" ~tangent_attribute:"tangent"
      ~orient_attribute:"orient" ~distance_attribute:"distance" () in
  let x = float3_attribute geometry "xaxis"
  and y = float3_attribute geometry "yaxis"
  and z = float3_attribute geometry "tangent"
  and q = float4_attribute geometry "orient"
  and angle = float_attribute geometry "angle"
  and distance = float_attribute geometry "distance" in
  for point = 0 to Geometry.point_count geometry - 1 do
    let dot ax ay az bx by bz = (ax *. bx) +. (ay *. by) +. (az *. bz) in
    check (near (dot x.x.(point) x.y.(point) x.z.(point)
        x.x.(point) x.y.(point) x.z.(point)) 1.
      && near (dot y.x.(point) y.y.(point) y.z.(point)
        y.x.(point) y.y.(point) y.z.(point)) 1.
      && near (dot z.x.(point) z.y.(point) z.z.(point)
        z.x.(point) z.y.(point) z.z.(point)) 1.
      && near (dot x.x.(point) x.y.(point) x.z.(point)
        y.x.(point) y.y.(point) y.z.(point)) 0.
      && near (dot x.x.(point) x.y.(point) x.z.(point)
        z.x.(point) z.y.(point) z.z.(point)) 0.
      && near (dot y.x.(point) y.y.(point) y.z.(point)
        z.x.(point) z.y.(point) z.z.(point)) 0.)
      "Spiral frame is not orthonormal";
    let orient = Rays_math.Quat.create ~x:q.x.(point) ~y:q.y.(point)
        ~z:q.z.(point) ~w:q.w.(point) in
    let qx = rotate orient Rays_math.Vec3.unit_x
    and qy = rotate orient Rays_math.Vec3.unit_y
    and qz = rotate orient Rays_math.Vec3.unit_z in
    check (near qx.x x.x.(point) && near qx.y x.y.(point)
        && near qx.z x.z.(point) && near qy.x y.x.(point)
        && near qy.y y.y.(point) && near qy.z y.z.(point)
        && near qz.x z.x.(point) && near qz.y z.y.(point)
        && near qz.z z.z.(point)) "Spiral orient does not encode its frame"
  done;
  check (near (angle.(65) -. angle.(0)) Float.pi
      && distance.(0) = 0. && distance.(65) = 0.)
    "Spiral per-curve angle/distance reset";
  let swept = Sweep_circle.run ~sides:8 ~radius:0.05 geometry |> get_ok in
  check (Geometry.primitive_count swept > 0)
    "Spiral curve failed Sweep composition"

let check_transform_and_validation () =
  let rotation = Rays_math.Vec3.create 0.3 0.5 0.7 and center = Rays_math.Vec3.create 3. (-2.) 5. in
  let source = Rays_math.Vec3.create 2. 0. 0. in
  let first order = make
      ~radius:(Spiral.Spiral_archimedean_change {
        start_radius = 2.; increase_per_turn = 0. })
      ~rotation ~rotation_order:order ~center
      ~divisions:(Spiral.Spiral_divisions_per_curve 2) () |> positions
      |> fun p -> Rays_math.Vec3.create p.x.(0) p.y.(0) p.z.(0) in
  let cases = [
    Spiral.Spiral_xyz, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_x rotation.x));
    Spiral.Spiral_xzy, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z) (Rays_math.Mat4.rotation_x rotation.x));
    Spiral.Spiral_yxz, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x) (Rays_math.Mat4.rotation_y rotation.y));
    Spiral.Spiral_yzx, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z) (Rays_math.Mat4.rotation_y rotation.y));
    Spiral.Spiral_zxy, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x) (Rays_math.Mat4.rotation_z rotation.z));
    Spiral.Spiral_zyx, Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x rotation.x)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_z rotation.z));
  ] in
  List.iter (fun (order, matrix) ->
    let expected = Rays_math.Vec3.add (Rays_math.Mat4.transform_point matrix source) center
    and actual = first order in
    check (near actual.x expected.x && near actual.y expected.y
        && near actual.z expected.z) "Spiral Euler rotation order") cases;
  let run ?grain ?extent ?radius ?height_ramp ?radius_scale ?radius_ramp
      ?direction ?start_angle ?divisions ?uniform_angle ?spiral_count
      ?orientation ?center ?rotation ?uniform_scale ?angle_attribute
      ?x_axis_attribute ?y_axis_attribute ?tangent_attribute ?orient_attribute
      ?distance_attribute () =
    Spiral.run ?grain ?extent ?radius ?height_ramp ?radius_scale ?radius_ramp
      ?direction ?start_angle ?divisions ?uniform_angle ?spiral_count
      ?orientation ?center ?rotation ?uniform_scale ?angle_attribute
      ?x_axis_attribute ?y_axis_attribute ?tangent_attribute ?orient_attribute
      ?distance_attribute () in
  expect_code "invalid_parameter" (run ~grain:0 ());
  expect_code "invalid_parameter"
    (run ~extent:(Spiral.Spiral_turns { turns = 0.; height = 1. }) ());
  expect_code "invalid_parameter"
    (run ~extent:(Spiral.Spiral_height_pitch { height = 1.; pitch = -1. }) ());
  expect_code "invalid_parameter"
    (run ~radius:(Spiral.Spiral_archimedean_end {
      start_radius = -1.; end_radius = 2. }) ());
  expect_code "invalid_parameter"
    (run ~radius:(Spiral.Spiral_archimedean_change {
      start_radius = 1.; increase_per_turn = -1. }) ());
  expect_code "invalid_parameter"
    (run ~radius:(Spiral.Spiral_logarithmic_change {
      start_radius = 1.; scale_per_turn = 0. }) ());
  expect_code "invalid_parameter" (run ~radius_scale:0. ());
  expect_code "invalid_parameter" (run ~uniform_scale:Float.infinity ());
  expect_code "invalid_parameter" (run ~start_angle:Float.nan ());
  expect_code "invalid_parameter"
    (run ~divisions:(Spiral.Spiral_divisions_per_curve 0) ());
  expect_code "invalid_parameter" (run ~spiral_count:0 ());
  expect_code "invalid_parameter"
    (run ~orientation:(Spiral.Spiral_axis Rays_math.Vec3.zero) ());
  expect_code "invalid_parameter" (run ~height_ramp:[0.2, 1.; 1., 1.] ());
  expect_code "invalid_parameter" (run ~radius_ramp:[0., 1.; 0., 2.] ());
  expect_code "invalid_parameter" (run ~angle_attribute:"P" ());
  expect_code "invalid_parameter"
    (run ~angle_attribute:"same" ~distance_attribute:"same" ());
  expect_code "invalid_parameter" (run ~radius_scale:max_float
      ~center:(Rays_math.Vec3.create max_float 0. 0.) ());
  expect_code "invalid_parameter" (run ~uniform_angle:false
      ~extent:(Spiral.Spiral_turns { turns = 1.; height = 0. })
      ~radius_ramp:[0.5, 0.] ~tangent_attribute:"tangent" ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Spiral.run ~cancel:cancelled ())

let check_parallel_exact () =
  let run domains = Parallel.run ~domains (fun () ->
    Spiral.run ~grain:257
      ~extent:(Spiral.Spiral_height_pitch { height = -18.; pitch = -0.37 })
      ~radius:(Spiral.Spiral_logarithmic_end {
        start_radius = 0.35; end_radius = 8. })
      ~height_ramp:[0., 0.8; 0.35, 1.2; 0.7, 0.55; 1., 1.]
      ~radius_scale:1.3 ~radius_ramp:[0., 1.; 0.4, 0.6; 1., 1.15]
      ~direction:Spiral.Spiral_clockwise ~start_angle:(-0.7)
      ~divisions:(Spiral.Spiral_divisions_per_curve 20_000)
      ~uniform_angle:false ~spiral_count:5
      ~orientation:(Spiral.Spiral_axis (Rays_math.Vec3.create 1. 2. 3.))
      ~center:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotation:(Rays_math.Vec3.create 0.3 0.5 0.7)
      ~rotation_order:Spiral.Spiral_yzx ~uniform_scale:1.2
      ~angle_attribute:"angle" ~x_axis_attribute:"xaxis"
      ~y_axis_attribute:"yaxis" ~tangent_attribute:"tangent"
      ~orient_attribute:"orient" ~distance_attribute:"distance" ()
    |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Spiral geometry differ";
  check (Geometry.point_count one = 100_005
      && Geometry.vertex_count one = 100_005
      && Geometry.primitive_count one = 5)
    "advanced Spiral exactness cardinality"

let run () =
  check_default_and_extent ();
  check_radius_families_ramps_and_count ();
  check_equal_arc_spacing ();
  check_frames_and_composition ();
  check_transform_and_validation ();
  check_parallel_exact ();
  print_endline "Spiral tests passed"
