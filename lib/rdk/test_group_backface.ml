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

let group name geometry = match Geometry.find_group ~owner:Group.Primitive name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let sample () = geometry
    [|(-1., -1., 0.); (1., -1., 0.); (0., 1., 0.);
      (-1., -1., 2.); (0., 1., 2.); (1., -1., 2.);
      (0., -1., 3.); (0., 1., 3.); (0., 0., 5.);
      (-1., 0., 7.); (1., 0., 7.)|]
    [|`Polygon, [|0; 1; 2|]; `Polygon, [|3; 4; 5|];
      `Polygon, [|6; 7; 8|]; `Open, [|9; 10|]|]

let run () =
  print_endline "group backface tests passed"
