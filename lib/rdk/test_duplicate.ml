open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Text values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let group owner name geometry =
  Geometry.find_group ~owner name geometry |> Option.get

let equal_geometry left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && int_attribute Attribute.Point "id" left
      = int_attribute Attribute.Point "id" right
  && int_attribute Attribute.Vertex "corner" left
      = int_attribute Attribute.Vertex "corner" right
  && let lg = group Group.Point "marked" left
     and rg = group Group.Point "marked" right in
     Group.ordered_elements lg = Group.ordered_elements rg
     && let equal_groups =
          let left = Geometry.groups left and right = Geometry.groups right in
          List.length left = List.length right
          && List.for_all2 (fun left right ->
            Group.owner left = Group.owner right
            && String.equal (Group.name left) (Group.name right)
            && Group.length left = Group.length right
            && Group.ordered_elements left = Group.ordered_elements right
            && let same = ref true in
               for element = 0 to Group.length left - 1 do
                 if Group.mem element left <> Group.mem element right then
                   same := false
               done;
               !same) left right in
        equal_groups
     && let le = Geometry.find_edge_group "all_edges" left |> Option.get
        and re = Geometry.find_edge_group "all_edges" right |> Option.get in
        Edge_group.length le = Edge_group.length re
        && let same = ref true in
           for edge = 0 to Edge_group.length le - 1 do
             if Edge_group.mem edge le <> Edge_group.mem edge re then same := false
           done;
           !same

let run () =
  print_endline "Duplicate tests passed"
