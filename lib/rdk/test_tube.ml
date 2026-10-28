open Rays
open Rdk
open Rdk_test_support

let float2_attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float3_attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && begin
    let equal = ref true in
    for index = 0 to Group.length left - 1 do
      if Group.mem index left <> Group.mem index right then equal := false
    done;
    !equal
  end

let equal_geometry left right =
  let left_positions = positions left and right_positions = positions right
  and left_topology = Topology.View.view (Geometry.topology left)
  and right_topology = Topology.View.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)

let check_vertex_normal_winding geometry message =
  let point = positions geometry
  and normal = float3_attribute geometry Attribute.Vertex "N"
  and topology = Topology.View.view (Geometry.topology geometry) in
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let nx = ref 0. and ny = ref 0. and nz = ref 0. in
    for vertex = first to last - 1 do
      let next = if vertex + 1 = last then first else vertex + 1 in
      let a = topology.vertex_points.(vertex)
      and b = topology.vertex_points.(next) in
      nx := !nx +. ((point.y.(a) -. point.y.(b))
          *. (point.z.(a) +. point.z.(b)));
      ny := !ny +. ((point.z.(a) -. point.z.(b))
          *. (point.x.(a) +. point.x.(b)));
      nz := !nz +. ((point.x.(a) -. point.x.(b))
          *. (point.y.(a) +. point.y.(b)))
    done;
    let alignment = (!nx *. normal.x.(first)) +. (!ny *. normal.y.(first))
        +. (!nz *. normal.z.(first)) in
    check (alignment > 1e-12)
      (Printf.sprintf "%s (primitive %d, alignment %.17g)"
        message primitive alignment)
  done

let run () =
  print_endline "Tube tests passed"
