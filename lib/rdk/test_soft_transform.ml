open Rays
open Rdk
open Rdk_test_support

let close left right = abs_float (left -. right) <= 1e-10

let line_with_free_point () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.1;3.;4.|] ~y:(Array.make 5 0.) ~z:(Array.make 5 0.) in
  let topology = Topology.Builder.create ~point_count:5 ~vertex_capacity:4
      ~primitive_capacity:1 () in
  Topology.Builder.add_open_polyline topology [|0;1;3;4|];
  Geometry.create ~positions ~topology:(Topology.Builder.freeze topology) ()
  |> Result.get_ok

let point_group name length members =
  Group.init ~grain:1 ~owner:Group.Point ~name length members

let y_positions geometry =
  let values = Packed.Float3.View.view (Geometry.positions geometry) in
  values.y

let float_attribute name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let with_float name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float values) |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let test_radius_and_edge () =
  let source = line_with_free_point () in
  let seed = point_group "seed" 5 (fun point -> point = 0) in
  let matrix = Rays_math.Mat4.translation (Rays_math.Vec3.create 0. 2. 0.) in
  let radius = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~metric:Transform_ops.Soft_radius
      ~falloff:Transform_ops.Soft_linear ~radius:2. ~falloff_attribute:"falloff"
      matrix source |> get_ok in
  let y = y_positions radius and weights = float_attribute "falloff" radius in
  check (close y.(0) 2. && close y.(1) 1. && close y.(2) 1.9
      && close y.(3) 0. && close y.(4) 0.)
    "Soft Transform radius positions";
  check (close weights.(0) 1. && close weights.(1) 0.5
      && close weights.(2) 0.95 && close weights.(3) 0.)
    "Soft Transform radius weights";
  let edge = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~metric:Transform_ops.Soft_edge
      ~falloff:Transform_ops.Soft_linear ~radius:3.5 ~falloff_attribute:"falloff"
      matrix source |> get_ok in
  let y = y_positions edge and weights = float_attribute "falloff" edge in
  check (close y.(0) 2. && close weights.(1) (1. -. 1. /. 3.5)
      && close y.(2) 0. && close weights.(2) 0.
      && close weights.(3) (1. -. 3. /. 3.5) && close weights.(4) 0.)
    "Soft Transform edge connectivity/distance";
  let quadratic = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~metric:Transform_ops.Soft_radius
      ~falloff:Transform_ops.Soft_quadratic ~radius:2. matrix source |> get_ok in
  check (close (y_positions quadratic).(1) 1.5)
    "Soft Transform quadratic falloff";
  let cubic = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~metric:Transform_ops.Soft_radius
      ~falloff:Transform_ops.Soft_cubic ~radius:2. matrix source |> get_ok in
  check (close (y_positions cubic).(1) 1.)
    "Soft Transform cubic falloff"

let test_attribute () =
  let source = line_with_free_point ()
      |> with_float "weight" [|1.;0.5;2.;-0.5;0.|]
      |> with_float "distance" [|0.;1.;2.;3.;4.|] in
  let affected = point_group "affected" 5 (fun point -> point = 1 || point = 2) in
  let matrix = Rays_math.Mat4.translation (Rays_math.Vec3.create 0. 2. 0.) in
  let direct = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points affected)
      ~metric:(Transform_ops.Soft_attribute { attribute = "weight";
        apply_rolloff = false }) ~falloff_attribute:"used_weight"
      matrix source |> get_ok in
  let y = y_positions direct and weights = float_attribute "used_weight" direct in
  check (close y.(0) 0. && close y.(1) 1. && close y.(2) 4.
      && close y.(3) 0. && close weights.(0) 0. && close weights.(2) 2.)
    "Soft Transform direct attribute/group weights";
  let rolled = Transform_ops.soft_transform ~grain:1
      ~metric:(Transform_ops.Soft_attribute { attribute = "distance";
        apply_rolloff = true }) ~falloff:Transform_ops.Soft_linear ~radius:4.
      matrix source |> get_ok in
  let y = y_positions rolled in
  check (close y.(0) 2. && close y.(1) 1.5 && close y.(2) 1.
      && close y.(3) 0.5 && close y.(4) 0.)
    "Soft Transform rolled distance attribute";
  let no_normals = Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> get_ok in
  let seed = point_group "seed" (Geometry.point_count no_normals)
      (fun point -> point = 4) in
  let deformed = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~radius:2.
      ~recompute_normals:false matrix no_normals |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" deformed = None)
    "Soft Transform stale-normal invalidation";
  let repaired = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points seed) ~radius:2. matrix no_normals
      |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" repaired <> None)
    "Soft Transform normal recomputation";
  let empty = point_group "empty" 5 (fun _ -> false) in
  let unchanged = Transform_ops.soft_transform ~grain:1
      ~selection:(Transform_ops.Selected_points empty) ~radius:2. matrix source |> get_ok in
  check (unchanged == source) "Soft Transform empty influence sharing"

let test_errors_and_parallel () =
  let source = line_with_free_point () in
  let seed = point_group "seed" 5 (fun point -> point = 0) in
  (match Transform_ops.soft_transform ~radius:0. ~selection:(Transform_ops.Selected_points seed)
      Rays_math.Mat4.identity source with
   | Error error -> check (Error.code error = "invalid_transform")
       "Soft Transform zero radius code"
   | Ok _ -> fail "Soft Transform accepted zero rolloff radius");
  (match Transform_ops.soft_transform
      ~metric:(Transform_ops.Soft_attribute { attribute = "missing";
        apply_rolloff = false }) Rays_math.Mat4.identity source with
   | Error error -> check (Error.code error = "invalid_transform")
       "Soft Transform missing attribute code"
   | Ok _ -> fail "Soft Transform accepted missing attribute");
  let wrong = Attribute.create_owned ~owner:Attribute.Point ~name:"wrong"
      (Attribute.Int (Array.make 5 1)) |> Result.get_ok in
  let wrong = Geometry.with_attribute wrong source |> Result.get_ok in
  (match Transform_ops.soft_transform
      ~metric:(Transform_ops.Soft_attribute { attribute = "wrong";
        apply_rolloff = false }) Rays_math.Mat4.identity wrong with
   | Error error -> check (Error.code error = "invalid_transform")
       "Soft Transform wrong attribute storage code"
   | Ok _ -> fail "Soft Transform accepted integer distance attribute");
  let nonfinite = source |> with_float "bad" [|0.;1.;Float.nan;3.;4.|] in
  (match Transform_ops.soft_transform
      ~metric:(Transform_ops.Soft_attribute { attribute = "bad";
        apply_rolloff = false }) Rays_math.Mat4.identity nonfinite with
   | Error error -> check (Error.code error = "invalid_transform")
       "Soft Transform non-finite attribute code"
   | Ok _ -> fail "Soft Transform accepted non-finite distance attribute");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Transform_ops.soft_transform ~cancel:cancelled
      ~selection:(Transform_ops.Selected_points seed) (Rays_math.Mat4.translation Rays_math.Vec3.unit_y)
      source with
   | Error error -> check (Error.code error = "cancelled")
       "Soft Transform cancellation code"
   | Ok _ -> fail "cancelled Soft Transform published geometry");
  let dense = Plane_generators.grid ~columns:400 ~rows:240 ~size:20. () |> get_ok in
  let width = 401 and count = Geometry.point_count dense in
  let seed = point_group "seed" count (fun point ->
      point = (120 * width) + 200) in
  let run metric domains = Parallel.run ~domains (fun () ->
      Transform_ops.soft_transform ~grain:257 ~selection:(Transform_ops.Selected_points seed)
        ~metric ~falloff:Transform_ops.Soft_cubic ~radius:4.
        ~falloff_attribute:"falloff" (Rays_math.Mat4.translation (Rays_math.Vec3.create 0. 1. 0.))
        dense |> get_ok) in
  List.iter (fun metric ->
    let one = run metric 1 and four = run metric 4 in
    let one_p = Packed.Float3.View.view (Geometry.positions one)
    and four_p = Packed.Float3.View.view (Geometry.positions four) in
    check (same_float_array one_p.x four_p.x
        && same_float_array one_p.y four_p.y
        && same_float_array one_p.z four_p.z
        && same_float_array (float_attribute "falloff" one)
             (float_attribute "falloff" four))
      "Soft Transform one/four-domain exactness")
    [Transform_ops.Soft_radius; Transform_ops.Soft_edge]

let run () =
  test_radius_and_edge ();
  test_attribute ();
  test_errors_and_parallel ();
  print_endline "soft transform tests passed"
