open Rays
open Rdk
open Rdk_test_support

let topology geometry = Topology.Private.view (Geometry.topology geometry)

let attribute_storage geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail ("missing attribute " ^ name)
  | Some attribute -> Attribute.Private.storage attribute

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = topology left and rt = topology right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_group group geometry = Geometry.with_group group geometry |> get_string

let base_profile () =
  Line_geometry.polyline [|(1., -1., 0.); (1., 1., 0.)|] |> get_ok

let run () =
  print_endline "Revolve tests passed"
