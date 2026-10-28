open Rays
open Rdk
open Rdk_test_support

let attribute owner name storage = Attribute.create_owned ~owner ~name storage |> get_string
let topology_points geometry = (Topology.View.view (Geometry.topology geometry)).vertex_points

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail ("wrong integer storage for " ^ name))
  | None -> fail ("missing integer attribute " ^ name)

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.ordered_elements left = Group.ordered_elements right
  && Array.for_all
       (fun index -> Group.mem index left = Group.mem index right)
       (Array.init (Group.length left) Fun.id)

let group owner name geometry = Geometry.find_group ~owner name geometry |> Option.get
let run () = print_endline "Reverse tests passed"
