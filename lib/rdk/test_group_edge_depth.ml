open Rays
open Rdk
open Rdk_test_support

let geometry point_count curves =
  let positions = Packed.Float3.Builder.create point_count in
  for point = 0 to point_count - 1 do
    Packed.Float3.Builder.set positions point (float_of_int point) 0. 0.
  done;
  let topology = Topology.Builder.create ~point_count () in
  Array.iter (Topology.Builder.add_open_polyline topology) curves;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Topology.Builder.freeze topology) () |> Result.get_ok

let point_group name geometry =
  match Geometry.find_group ~owner:Group.Point name geometry with
  | Some group -> group
  | None -> fail ("missing point group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun point -> output := point :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let sample () =
  let source = geometry 13
      [|[|0; 1; 2; 3; 4; 5; 6; 7; 8|]; [|9; 10; 11|]|] in
  let seed = Group.init ~owner:Group.Point ~name:"seed" 13
      (fun point -> point = 4 || point = 10 || point = 12) in
  Geometry.with_group seed source |> Result.get_ok

let run () =
  print_endline "group edge-depth tests passed"
