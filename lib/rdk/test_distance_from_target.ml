open Rays
open Rdk
open Rdk_test_support

let close left right = abs_float (left -. right) <= 1e-10

let point_group name length predicate =
  Group.init ~grain:1 ~owner:Group.Point ~name length predicate

let float_attribute name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let with_float name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float values) |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let expect_invalid work message = match work () with
  | Error error -> check (Error.code error = "invalid_distance") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "distance from target tests passed"
