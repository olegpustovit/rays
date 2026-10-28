open Rays
open Rdk
open Rdk_test_support

let geometry positions primitives =
  let packed = Packed.Float3.Builder.create (Array.length positions) in
  Array.iteri (fun point (x, y, z) -> Packed.Float3.Builder.set packed point x y z) positions;
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  Array.iter
    (fun (kind, points) ->
      match kind with
      | `Polygon -> Rdk_test_support.Tb.add_polygon topology points
      | `Open -> Rdk_test_support.Tb.add_open_polyline topology points)
    primitives;
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze packed)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> Result.get_ok

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let edge_members group =
  let output = ref [] in
  Edge_group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let expect_members expected group message = check (members group = expected) message

let expect_invalid operation message =
  match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let float3_attribute ~owner ~name values =
  let packed = Packed.Float3.Builder.create (Array.length values) in
  Array.iteri (fun index (x, y, z) -> Packed.Float3.Builder.set packed index x y z) values;
  Attribute.create_owned ~owner ~name (Attribute.Float3 (Packed.Float3.Builder.freeze packed))
  |> Result.get_ok

let same_group left right =
  Group.owner left = Group.owner right
  && Group.length left = Group.length right
  && members left = members right

let run () = print_endline "group normal tests passed"
