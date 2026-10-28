open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let vertex_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Text values -> values
      | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let primitive_group name geometry =
  Geometry.find_group ~owner:Group.Primitive name geometry |> Option.get

let edge_group geometry = Geometry.find_edge_group "all_edges" geometry
    |> Option.get

let equal_large left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && point_int "id" left = point_int "id" right
  && vertex_int "corner" left = vertex_int "corner" right
  && let lg = Geometry.find_group ~owner:Group.Point "marked" left |> Option.get
     and rg = Geometry.find_group ~owner:Group.Point "marked" right |> Option.get in
     Group.cardinality lg = Group.cardinality rg
     && let same_group = ref true in
        for point = 0 to Group.length lg - 1 do
          if Group.mem point lg <> Group.mem point rg then same_group := false
        done;
        !same_group
     && let le = edge_group left and re = edge_group right in
        Edge_group.length le = Edge_group.length re
        && Edge_group.cardinality le = Edge_group.cardinality re
        && let same = ref true in
           for edge = 0 to Edge_group.length le - 1 do
             if Edge_group.mem edge le <> Edge_group.mem edge re then same := false
           done;
           !same

let run () =
  print_endline "Ends tests passed"
