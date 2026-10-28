open Rays
open Rdk
open Rdk_test_support

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

let same_group left right =
  Group.owner left = Group.owner right
  && Group.length left = Group.length right
  && members left = members right

let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let with_int owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values) |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let expect_invalid operation message =
  match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let run () = print_endline "group random tests passed"
