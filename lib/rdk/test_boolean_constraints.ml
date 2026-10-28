open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
let approximate_point plan point =
  Rdk_geom.Implicit_point.approximate (Constraints.View.point plan point)

let close left right = abs_float (left -. right) <= 1e-12

let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let left_triangle () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|]

let crossing_triangle () = geometry
    [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] [|0;1;2|]

let test_right_operand_constraints () =
  let left = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
      [|0;1;2; 3;4;5|] in
  let plan = Constraints.build ~resolve_right_self_intersections:true
      ~grain:1 ~left ~right () |> get_rdk in
  check (Constraints.constraint_count plan = 1
      && Constraints.constraint_first_side plan 0 = Constraints.Right
      && Constraints.constraint_second_side plan 0 = Constraints.Right)
    "right-operand self constraint lost its typed endpoint sides";
  check (Constraints.right_constraint_range plan 0 = (0,1)
      && Constraints.right_constraint_range plan 1 = (1,2)
      && Constraints.left_constraint_range plan 0 = (0,0))
    "right-operand self constraint has incorrect face incidence"

let test_same_operand_coplanar_pair () =
  let left = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.25,0.25,0.; 1.,0.25,0.; 0.25,1.,0.|]
      [|0;1;2; 3;4;5|]
  and right = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|] in
  let plan = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left ~right () |> get_rdk in
  check (Constraints.coplanar_pair_count plan = 1
      && Constraints.coplanar_first_side plan 0 = Constraints.Left
      && Constraints.coplanar_second_side plan 0 = Constraints.Left)
    "same-operand coplanar pair lost typed endpoint sides";
  let first = Constraints.coplanar_first_triangle plan 0
  and second = Constraints.coplanar_second_triangle plan 0 in
  check ((first = 0 && second = 1) || (first = 1 && second = 0))
    "same-operand coplanar triangle ancestry is wrong"

let test_invalid_grain () =
  match Constraints.build ~grain:0 ~left:(left_triangle ())
      ~right:(crossing_triangle ()) () with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> failf "unexpected invalid-grain error: %s" (Error.to_string error)
  | Ok _ -> failf "zero grain was accepted"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Constraints.build ~cancel ~grain:1 ~left:(left_triangle ())
      ~right:(crossing_triangle ()) () with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled constraint planning completed"

let run () =
  test_right_operand_constraints ();
  test_same_operand_coplanar_pair ();
  test_invalid_grain ();
  test_cancellation ()
