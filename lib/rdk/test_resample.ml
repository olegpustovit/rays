open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

let attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> attribute
  | None -> fail ("missing attribute " ^ name)

let float_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " has unexpected storage")

let int_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " has unexpected storage")

let float3_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Float3 values -> Packed.Float3.View.view values
  | _ -> fail (name ^ " has unexpected storage")

let equal_geometry left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected error " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid Resample input"

let x_positions geometry =
  (Packed.Float3.View.view (Geometry.positions geometry)).x

let with_primitive_attribute name storage geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Primitive ~name storage
      |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let run () =
  print_endline "Resample tests passed"
