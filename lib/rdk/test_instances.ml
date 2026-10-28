open Rays
open Rdk
open Rdk_test_support


let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let geometry_equal left right =
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
      && equal_storage left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && String.equal (Group.name left) (Group.name right)
      && Group.ordered_elements left = Group.ordered_elements right
      && Group.length left = Group.length right
      && let equal = ref true in
         for element = 0 to Group.length left - 1 do
           if Group.mem element left <> Group.mem element right then equal := false
         done;
         !equal) (Geometry.groups left) (Geometry.groups right)
  && List.equal (fun left right ->
      String.equal (Edge_group.name left) (Edge_group.name right)
      && Edge_group.length left = Edge_group.length right
      && let equal = ref true in
         for edge = 0 to Edge_group.length left - 1 do
           if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
         done;
         !equal) (Geometry.edge_groups left) (Geometry.edge_groups right)

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get

let transforms count = Array.init count (fun instance ->
  let angle = float_of_int instance *. 0.17 in
  Rays_math.Mat4.mul
    (Rays_math.Mat4.translation (Rays_math.Vec3.create (float_of_int instance *. 1.25)
      (sin angle *. 0.5) (cos angle *. 0.25)))
    (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y angle)
      (Rays_math.Mat4.scaling (Rays_math.Vec3.create 1. (0.75 +. float_of_int (instance mod 3)
        *. 0.125) 1.))))

let legacy_materialize matrices source =
  Array.to_list matrices
  |> List.map (fun matrix -> Transform_ops_front.transform ~grain:1 matrix source)
  |> Mesh_merge.run ~grain:1
  |> get_rdk

let run () =
  let triangle = Line_geometry.polyline ~closed:true
      [|(0., 0., 0.); (1., 0., 0.); (0., 1., 0.)|] |> get_rdk in
  let scale_count = 100_000 in
  let scaled = Instance_copy.materialize_instances ~transforms:(Array.make scale_count Rays_math.Mat4.identity) triangle |> get_rdk in
  check (Geometry.point_count scaled = scale_count * 3
      && Geometry.vertex_count scaled = scale_count * 3
      && Geometry.primitive_count scaled = scale_count)
    "scale materialization cardinality is not exact";
  print_endline "RDK instance materialization tests passed"
