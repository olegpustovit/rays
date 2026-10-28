open Rays
open Rdk
open Rdk_test_support

let geometry positions primitives =
  let packed = Packed.Float3.Builder.create (Array.length positions) in
  Array.iteri (fun point (x, y, z) ->
    Packed.Float3.Builder.set packed point x y z) positions;
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  Array.iter (fun (kind, points) -> match kind with
    | `Polygon -> Rdk_test_support.Tb.add_polygon topology points
    | `Open -> Rdk_test_support.Tb.add_open_polyline topology points) primitives;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze packed)
    ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok

let sample () = geometry
    [|(0.,0.,0.); (1.,0.,0.); (1.,1.,0.); (0.,1.,0.);
      (2.,0.,0.); (3.,0.,0.); (4.,0.,0.); (5.,0.,0.);
      (7.,0.,0.); (8.,0.,0.); (7.5,1.,0.)|]
    [|`Polygon, [|0;1;2|]; `Polygon, [|0;2;3|];
      `Open, [|4;5;6;7|]; `Polygon, [|8;9;10|]|]

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry = match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun element -> output := element :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let run () =
  print_endline "group unshared/boundary tests passed"
