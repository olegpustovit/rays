open Rays
open Rdk
open Rdk_test_support

let close left right = abs_float (left -. right) <= 1e-10

let point geometry index =
  let values = Packed.Float3.Private.view (Geometry.positions geometry) in
  values.x.(index), values.y.(index), values.z.(index)

let check_point geometry index (x, y, z) message =
  let ax, ay, az = point geometry index in
  check (close ax x && close ay y && close az z) message

let normal owner geometry =
  match Geometry.find_attribute ~owner "N" geometry with
  | None -> fail "missing normal"
  | Some attribute ->
      (match Attribute.get (Attribute.normal ~owner) attribute with
       | Some value -> Packed.Float3.Private.view value
       | None -> fail "invalid normal storage")

let same_float3 left right =
  let left = Packed.Float3.Private.view left
  and right = Packed.Float3.Private.view right in
  same_float_array left.x right.x && same_float_array left.y right.y
  && same_float_array left.z right.z

let same_geometry left right =
  Geometry.topology left == Geometry.topology right
  && same_float3 (Geometry.positions left) (Geometry.positions right)
  && List.for_all (fun owner ->
      match Geometry.find_attribute ~owner "N" left,
          Geometry.find_attribute ~owner "N" right with
      | None, None -> true
      | Some left, Some right ->
          (match Attribute.get (Attribute.normal ~owner) left,
              Attribute.get (Attribute.normal ~owner) right with
           | Some left, Some right -> same_float3 left right
           | None, None -> true
           | None, Some _ | Some _, None -> false)
      | None, Some _ | Some _, None -> false)
      [Attribute.Point; Attribute.Vertex]

let test_composition () =
  let translation = Rays_math.Vec3.create 10. 0. 0. and rotation = Rays_math.Vec3.create 0. 0. (Float.pi /. 2.)
  and scale = Rays_math.Vec3.create 2. 1. 1. in
  let srt = Transform_ops.compose_transform ~order:Transform_ops.Transform_srt
      ~translate:translation ~rotate:rotation ~scale () |> get_ok in
  let x, y, z = Rays_math.Mat4.transform_point srt (Rays_math.Vec3.create 1. 0. 0.) |> fun v -> v.x,v.y,v.z in
  check (close x 10. && close y 2. && close z 0.)
    "Transform SRT application order";
  let trs = Transform_ops.compose_transform ~order:Transform_ops.Transform_trs
      ~translate:translation ~rotate:rotation ~scale () |> get_ok in
  let value = Rays_math.Mat4.transform_point trs (Rays_math.Vec3.create 1. 0. 0.) in
  check (close value.x 0. && close value.y 11. && close value.z 0.)
    "Transform TRS application order";
  let s = Rays_math.Mat4.scaling scale and r = Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z rotation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y rotation.y) (Rays_math.Mat4.rotation_x rotation.x))
  and t = Rays_math.Mat4.translation translation in
  let applied matrices = List.fold_left (fun matrix operation ->
      Rays_math.Mat4.mul operation matrix) Rays_math.Mat4.identity matrices in
  List.iter (fun (order, expected) ->
    let actual = Transform_ops.compose_transform ~order ~translate:translation
        ~rotate:rotation ~scale () |> get_ok in
    check (Rays_math.Mat4.nearly_equal actual (applied expected) ~eps:1e-12)
      "Transform affine-order matrix")
    [Transform_ops.Transform_srt, [s;r;t]; Transform_ops.Transform_str, [s;t;r];
     Transform_ops.Transform_rst, [r;s;t]; Transform_ops.Transform_rts, [r;t;s];
     Transform_ops.Transform_tsr, [t;s;r]; Transform_ops.Transform_trs, [t;r;s]];
  let rx = Rays_math.Mat4.rotation_x 0.2 and ry = Rays_math.Mat4.rotation_y (-0.3)
  and rz = Rays_math.Mat4.rotation_z 0.4 and angles = Rays_math.Vec3.create 0.2 (-0.3) 0.4 in
  List.iter (fun (rotation_order, expected) ->
    let actual = Transform_ops.compose_transform ~rotation_order ~rotate:angles ()
        |> get_ok in
    check (Rays_math.Mat4.nearly_equal actual (applied expected) ~eps:1e-12)
      "Transform Euler-order matrix")
    [Transform_ops.Transform_xyz, [rx;ry;rz]; Transform_ops.Transform_xzy, [rx;rz;ry];
     Transform_ops.Transform_yxz, [ry;rx;rz]; Transform_ops.Transform_yzx, [ry;rz;rx];
     Transform_ops.Transform_zxy, [rz;rx;ry]; Transform_ops.Transform_zyx, [rz;ry;rx]];
  let pivoted = Transform_ops.compose_transform ~scale:(Rays_math.Vec3.create 2. 2. 2.)
      ~pivot:(Rays_math.Vec3.create 1. 0. 0.) () |> get_ok in
  let value = Rays_math.Mat4.transform_point pivoted (Rays_math.Vec3.create 2. 0. 0.) in
  check (close value.x 3. && close value.y 0. && close value.z 0.)
    "Transform pivot";
  let forward = Transform_ops.compose_transform ~translate:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotate:(Rays_math.Vec3.create 0.2 (-0.4) 0.7)
      ~scale:(Rays_math.Vec3.create 2. 3. 4.) ~shear:(Rays_math.Vec3.create 0.1 (-0.2) 0.3) ()
      |> get_ok in
  let inverse = Transform_ops.compose_transform ~translate:(Rays_math.Vec3.create 3. (-2.) 5.)
      ~rotate:(Rays_math.Vec3.create 0.2 (-0.4) 0.7)
      ~scale:(Rays_math.Vec3.create 2. 3. 4.) ~shear:(Rays_math.Vec3.create 0.1 (-0.2) 0.3)
      ~invert:true () |> get_ok in
  let source = Rays_math.Vec3.create (-1.2) 4.1 0.7 in
  let round_trip = Rays_math.Mat4.transform_point inverse (Rays_math.Mat4.transform_point forward source) in
  check (close source.x round_trip.x && close source.y round_trip.y
      && close source.z round_trip.z) "Transform inverse round trip";
  (match Transform_ops.compose_transform ~scale:Rays_math.Vec3.zero ~invert:true () with
   | Error error -> check (Error.code error = "invalid_transform")
       "singular inverse error code"
   | Ok _ -> fail "Transform accepted singular inversion")

let two_triangles () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] ~z:(Array.make 4 0.) in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2; 0;2;3|] ~primitive_offsets:[|0;3;6|]
      |> Result.get_ok in
  let point_n = Packed.Float3.Private.of_owned_exn
      ~x:(Array.make 4 2.) ~y:(Array.make 4 2.) ~z:(Array.make 4 0.) in
  let vertex_n = Packed.Float3.Private.of_owned_exn
      ~x:(Array.make 6 0.) ~y:(Array.make 6 0.) ~z:(Array.make 6 2.) in
  let point_n = Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point)
      point_n |> Result.get_ok
  and vertex_n = Attribute.create_key_owned
      (Attribute.normal ~owner:Attribute.Vertex) vertex_n |> Result.get_ok in
  Geometry.create ~positions ~topology
    ~attributes:[point_n; vertex_n] () |> Result.get_ok

let run () =
  test_composition ();
  print_endline "transform tests passed"
