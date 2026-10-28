open Rdk
open Rdk_test_support

let with_attribute owner name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let group_members owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | None -> fail ("missing group " ^ name)
  | Some group ->
      let result = ref [] in
      Group.iter (fun element -> result := element :: !result) group;
      List.rev !result

let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
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

let equal_geometry left right =
  let left_positions = Packed.Float3.View.view (Geometry.positions left)
  and right_positions = Packed.Float3.View.view (Geometry.positions right)
  and left_topology = Topology.View.view (Geometry.topology left)
  and right_topology = Topology.View.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
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

let point_source () =
  let geometry = Line_geometry.points (Array.init 6 (fun point -> float_of_int point, 0., 0.))
      |> with_attribute Attribute.Point "value"
           (Attribute.Float [|0.; 1.; 2.; 3.; 4.; 5.|])
      |> with_attribute Attribute.Point "class"
           (Attribute.Int [|0; 1; 2; 3; 4; 5|]) in
  let base = Group.init ~grain:1 ~owner:Group.Point ~name:"base" 6
      (fun point -> point = 1 || point = 2 || point = 4)
  and old = Group.init ~grain:1 ~owner:Group.Point ~name:"picked" 6
      (fun _ -> true) in
  geometry |> Geometry.with_group base |> Result.get_ok
  |> Geometry.with_group old |> Result.get_ok

let expect_invalid work message = match work () with
  | Error error -> check (Error.code error = "invalid_blast") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "blast by attribute tests passed"
