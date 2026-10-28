open Rays
open Rdk
open Rdk_test_support

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let topology_points geometry =
  (Topology.View.view (Geometry.topology geometry)).vertex_points

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail ("wrong integer storage for " ^ name))
  | None -> fail ("missing integer attribute " ^ name)

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.ordered_elements left = Group.ordered_elements right
  && Array.for_all (fun index -> Group.mem index left = Group.mem index right)
       (Array.init (Group.length left) Fun.id)

let equal_geometry left right =
  topology_points left = topology_points right
  && List.equal (fun left right -> Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.for_all2 (fun left right ->
       String.equal (Edge_group.name left) (Edge_group.name right)
       && Edge_group.length left = Edge_group.length right
       && Array.for_all (fun edge -> Edge_group.mem edge left = Edge_group.mem edge right)
            (Array.init (Edge_group.length left) Fun.id))
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let group owner name geometry =
  Geometry.find_group ~owner name geometry |> Option.get

let run () =
  print_endline "Reverse tests passed"
