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
    [|(0., 0., 0.); (1., 0., 0.); (1., 0., 1.); (0., 0., 1.);
      (2., 0., 0.); (3., 0., 0.); (3., 0.2, 1.); (2., 0., 1.);
      (4., 0., 0.); (5., 0., 0.); (6., 0., 0.); (7., 0., 0.);
      (8., 0., 0.); (9., 0., 0.)|]
    [|`Polygon, [|0; 1; 2; 3|]; `Polygon, [|4; 5; 6; 7|];
      `Polygon, [|8; 9; 10; 11|]; `Open, [|12; 13|]|]

let quad_strip count =
  let point_count = count * 4 in
  let positions = Packed.Float3.Builder.create point_count
  and topology = Rdk_test_support.Tb.create ~point_count () in
  for primitive = 0 to count - 1 do
    let point = primitive * 4 and x = float_of_int primitive *. 1.25 in
    let warp = if primitive mod 3 = 0 then 0.125 else 0. in
    Packed.Float3.Builder.set positions point x 0. 0.;
    Packed.Float3.Builder.set positions (point + 1) (x +. 1.) 0. 0.;
    Packed.Float3.Builder.set positions (point + 2) (x +. 1.) warp 1.;
    Packed.Float3.Builder.set positions (point + 3) x 0. 1.;
    Rdk_test_support.Tb.add_polygon topology
      [|point; point + 1; point + 2; point + 3|]
  done;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok

let run () =
  print_endline "group non-planar tests passed"
