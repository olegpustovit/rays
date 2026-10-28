open Rays
open Rdk
open Rdk_test_support

let detail_float3 geometry name =
  match Geometry.find_attribute ~owner:Attribute.Detail name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing detail float3 " ^ name)

let point_normals geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail "N has unexpected storage")
  | None -> fail "missing point normals"

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right)
  and ln = point_normals left and rn = point_normals right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && lt.primitive_kinds = rt.primitive_kinds
  && ln.x = rn.x && ln.y = rn.y && ln.z = rn.z
  && (match Geometry.find_group ~owner:Group.Primitive "bounds" left,
      Geometry.find_group ~owner:Group.Primitive "bounds" right with
      | Some left_group, Some right_group ->
          Group.cardinality left_group = Group.cardinality right_group
          && Group.cardinality left_group = Geometry.primitive_count left
      | None, None -> true | _ -> false)

let run () =
  print_endline "bound tests passed"
