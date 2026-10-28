open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

let float3_attribute ~owner geometry name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
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

let norm (values : Packed.Float3.Private.view) index =
  sqrt ((values.x.(index) *. values.x.(index))
    +. (values.y.(index) *. values.y.(index))
    +. (values.z.(index) *. values.z.(index)))

let dot (left : Packed.Float3.Private.view)
    (right : Packed.Float3.Private.view) index =
  (left.x.(index) *. right.x.(index))
    +. (left.y.(index) *. right.y.(index))
    +. (left.z.(index) *. right.z.(index))

let frame_orientation (normal : Packed.Float3.Private.view)
    (tangent : Packed.Float3.Private.view)
    (bitangent : Packed.Float3.Private.view) index =
  let cx = (tangent.y.(index) *. bitangent.z.(index))
      -. (tangent.z.(index) *. bitangent.y.(index))
  and cy = (tangent.z.(index) *. bitangent.x.(index))
      -. (tangent.x.(index) *. bitangent.z.(index))
  and cz = (tangent.x.(index) *. bitangent.y.(index))
      -. (tangent.y.(index) *. bitangent.x.(index)) in
  (cx *. normal.x.(index)) +. (cy *. normal.y.(index))
    +. (cz *. normal.z.(index))

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected error " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid PolyFrame input"

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let run () =
  print_endline "PolyFrame tests passed"
