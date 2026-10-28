open Rays
open Rdk
open Rdk_test_support

let geometry positions curves =
  let packed = Packed.Float3.View.of_owned_exn
      ~x:(Array.map (fun (x, _, _) -> x) positions)
      ~y:(Array.map (fun (_, y, _) -> y) positions)
      ~z:(Array.map (fun (_, _, z) -> z) positions) in
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  Array.iter (Rdk_test_support.Tb.add_open_polyline topology) curves;
  Geometry.create ~positions:packed ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok value -> value | Error message -> fail message

let ordered name length elements =
  Group.ordered ~owner:Group.Point ~name ~length elements
  |> function Ok value -> value | Error message -> fail message

let ordinary name geometry =
  match Geometry.find_group ~owner:Group.Point name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let order group =
  match Group.ordered_elements group with
  | Some values -> values
  | None -> fail ("group " ^ Group.name group ^ " is not ordered")

let primitive_fan () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; -1.; 1.; 1.; -1.|]
      ~y:[|0.; 0.; 0.; 0.; 0.|]
      ~z:[|0.; -1.; -1.; 1.; 1.|] in
  let topology = Rdk_test_support.Tb.create ~point_count:5 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 0 2 3;
  Rdk_test_support.Tb.add_triangle topology 0 3 4;
  Rdk_test_support.Tb.add_triangle topology 0 4 1;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok value -> value | Error message -> fail message

let ordered_primitives name length elements =
  Group.ordered ~owner:Group.Primitive ~name ~length elements
  |> function Ok value -> value | Error message -> fail message

let diamond ?(upper_y = 2.) ?(lower_y = 0.1) () =
  geometry
    [|(0., 0., 0.); (1., upper_y, 0.); (1., lower_y, 0.);
      (2., 0., 0.); (3., 0., 0.)|]
    [|[|0; 1; 3|]; [|0; 2; 3|]; [|3; 4|]|]

let run () =
  print_endline "group find path tests passed"
