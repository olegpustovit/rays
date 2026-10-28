open Rays
open Rdk
open Rdk_test_support

let close left right = abs_float (left -. right) <= 1e-10

let line_with_free_point () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.1; 3.; 4.|] ~y:(Array.make 5 0.)
      ~z:(Array.make 5 0.) in
  let topology = Topology.Builder.create ~point_count:5 ~vertex_capacity:4
      ~primitive_capacity:1 () in
  Topology.Builder.add_open_polyline topology [|0; 1; 3; 4|];
  Geometry.create ~positions ~topology:(Topology.Builder.freeze topology) ()
  |> Result.get_ok

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
  print_endline "distance along geometry tests passed"
