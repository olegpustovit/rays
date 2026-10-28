open Rays
open Rdk
open Rdk_test_support

let geometry point_count faces =
  let positions = Packed.Float3.Builder.create point_count in
  for point = 0 to point_count - 1 do
    Packed.Float3.Builder.set positions point (float_of_int point) 0. 0.
  done;
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Array.iter (Rdk_test_support.Tb.add_polygon topology) faces;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok geometry -> geometry | Error message -> fail message

let two_quads () = geometry 6 [|[|0; 1; 4; 3|]; [|1; 2; 5; 4|]|]

let owner_count geometry = function
  | Group.Point -> Geometry.point_count geometry
  | Group.Vertex -> Geometry.vertex_count geometry
  | Group.Primitive -> Geometry.primitive_count geometry

let with_group owner name predicate geometry =
  Geometry.with_group (Group.init ~grain:1 ~owner ~name
      (owner_count geometry owner) predicate) geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let with_int owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values)
      |> function Ok value -> value | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let group owner name geometry = match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry = match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail ("non-integer attribute " ^ name))
  | None -> fail ("missing attribute " ^ name)

let same_group left right =
  Bytes.equal (Group.View.bits_view left) (Group.View.bits_view right)

let same_edge_group left right =
  Edge_group.length left = Edge_group.length right
  && let equal = ref true in
     for edge = 0 to Edge_group.length left - 1 do
       if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
     done;
     !equal

let run () =
  print_endline "group promote boundary tests passed"
