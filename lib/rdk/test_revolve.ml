open Rays
open Rdk
open Rdk_test_support

let topology geometry = Topology.View.view (Geometry.topology geometry)

let attribute_storage geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail ("missing attribute " ^ name)
  | Some attribute -> Attribute.View.storage attribute

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_group group geometry = Geometry.with_group group geometry |> get_string
let base_profile () = Line_geometry_front.polyline [| (1., -1., 0.); (1., 1., 0.) |] |> get_ok
let run () = print_endline "Revolve tests passed"
