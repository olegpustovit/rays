open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Arrangement = Rdk_boolean.Boolean_face_arrangement
module Triangulation = Rdk_boolean.Boolean_face_cdt


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

let crossing_input () =
  let left = geometry
      [|-3.,-3.,0.; 3.,-3.,0.; 0.,3.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,-2.,-1.; 0.,2.,-1.; 0.,0.,1.;
        -2.,0.,-1.; 2.,0.,-1.; 0.,0.,1.|]
      [|0;1;2; 3;4;5|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get_rdk in
  let arrangement = Arrangement.build constraints
      ~side:Constraints.Left ~triangle:0 |> get_rdk in
  constraints, arrangement

let edge_present triangulation first second =
  let present = ref false in
  for triangle = 0 to Triangulation.triangle_count triangulation - 1 do
    let a = Triangulation.triangle_point triangulation triangle 0
    and b = Triangulation.triangle_point triangulation triangle 1
    and c = Triangulation.triangle_point triangulation triangle 2 in
    if (a = first && b = second) || (a = second && b = first)
        || (b = first && c = second) || (b = second && c = first)
        || (c = first && a = second) || (c = second && a = first)
    then present := true
  done;
  !present

let coplanar_input () =
  let left = geometry
      [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] [|0;1;2|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get_rdk in
  let coplanar = Coplanar.build ~grain:1 constraints |> get_rdk in
  let arrangement = Arrangement.build ~coplanar constraints
      ~side:Constraints.Left ~triangle:0 |> get_rdk in
  constraints, arrangement

let many_constraint_input count =
  let scale = float_of_int count in
  let left = geometry
      [|-.scale,-.scale,0.; 2.*.scale,-.scale,0.;
        scale/.2.,2.*.scale,0.|] [|0;1;2|] in
  let points = Array.make (count * 3) (0.,0.,0.)
  and triangles = Array.init (count * 3) Fun.id in
  for segment = 0 to count - 1 do
    let point = segment * 3 and x = float_of_int segment +. 0.25 in
    points.(point) <- x,-1.,-1.;
    points.(point + 1) <- x,1.,-1.;
    points.(point + 2) <- x,0.,1.
  done;
  let right = geometry points triangles in
  let constraints = Constraints.build ~grain:7 ~left ~right () |> get_rdk in
  let arrangement = Arrangement.build constraints
      ~side:Constraints.Left ~triangle:0 |> get_rdk in
  constraints,arrangement

let test_cancellation () =
  let constraints, arrangement = crossing_input () in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Triangulation.build ~cancel constraints arrangement
      ~side:Constraints.Left ~triangle:0 with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled face CDT completed"

let run () =
  test_cancellation ()
