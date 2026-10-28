open Rays
open Rdk
open Rdk_test_support

(* A rule with its optional fields given as arguments. *)

let geometry_of_faces point_count faces =
  let positions = Packed.Float3.Builder.create point_count in
  for point = 0 to point_count - 1 do
    Packed.Float3.Builder.set positions point (float_of_int point) 0. 0.
  done;
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Array.iter (Rdk_test_support.Tb.add_polygon topology) faces;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok geometry -> geometry | Error message -> fail message

let fixture () = geometry_of_faces 12 [|
  [|0; 1; 2|]; [|1; 3; 2|]; [|3; 4; 2|];
  [|5; 6; 7|]; [|6; 8; 7|];
  [|9; 10; 11|]
|]

let group owner name geometry = match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun element -> output := element :: !output) group;
  List.rev !output

let expect_members expected group message =
  if members group <> expected then fail message

let disconnected ?region () =
  Group_ops.Range_disconnected { region }

let connected ?attributes ?(tolerance = 1e-6) ?collision ?region
    ?(remove_other_regions = true) () =
  Group_ops.Range_connected {
    connectivity_attributes = attributes;
    connectivity_tolerance = tolerance;
    collision;
    region;
    remove_other_regions;
  }

let with_attribute name owner storage geometry =
  let attribute = Attribute.create_owned ~name ~owner storage
      |> function Ok value -> value | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok value -> value | Error message -> fail message

let with_group owner name predicate geometry =
  let count = match owner with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry in
  Geometry.with_group (Group.init ~owner ~name count predicate) geometry
  |> function Ok value -> value | Error message -> fail message

let with_edge_group name ~a ~b geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  let selected = Topology_index.find_edge_index index ~a ~b in
  if selected < 0 then fail "missing fixture edge";
  let edge_group = Edge_group.init ~topology ~index ~name
      (fun edge -> edge = selected) in
  Geometry.with_edge_group edge_group geometry
  |> function Ok value -> value | Error message -> fail message

let triangle_soup count =
  let faces = Array.init count (fun triangle ->
    let point = triangle * 3 in [|point; point + 1; point + 2|]) in
  geometry_of_faces (count * 3) faces

let run () =
  print_endline "group range connectivity tests passed"
