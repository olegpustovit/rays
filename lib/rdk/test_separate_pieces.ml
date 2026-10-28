open Rays
open Rdk
open Rdk_test_support

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let make_curves positions primitive_points =
  let point_count = Array.length positions in
  let primitive_count = Array.length primitive_points in
  let primitive_offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    primitive_offsets.(primitive + 1) <- primitive_offsets.(primitive)
      + Array.length primitive_points.(primitive)
  done;
  let vertex_points = Array.make primitive_offsets.(primitive_count) 0 in
  Array.iteri (fun primitive points ->
    Array.blit points 0 vertex_points primitive_offsets.(primitive)
      (Array.length points)) primitive_points;
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets
      ~primitive_kinds:(Array.make primitive_count Topology.Open_polyline)
      |> Result.get_ok in
  let x = Array.map (fun (x, _, _) -> x) positions
  and y = Array.map (fun (_, y, _) -> y) positions
  and z = Array.map (fun (_, _, z) -> z) positions in
  Geometry.create
    ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> Result.get_ok

let position_view geometry =
  Packed.Float3.View.view (Geometry.positions geometry)

let float3_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail (name ^ " is not float3"))
  | None -> fail (name ^ " is missing")

let equal_storage left right = match left, right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      Packed.Float2.View.view left = Packed.Float2.View.view right
  | Attribute.Float3 left, Attribute.Float3 right ->
      Packed.Float3.View.view left = Packed.Float3.View.view right
  | Attribute.Float4 left, Attribute.Float4 right ->
      Packed.Float4.View.view left = Packed.Float4.View.view right
  | Attribute.Int_array left, Attribute.Int_array right ->
      Packed.Int_array.View.view left = Packed.Int_array.View.view right
  | Attribute.Float_array left, Attribute.Float_array right ->
      Packed.Float_array.View.view left = Packed.Float_array.View.view right
  | _ -> false

let equal_geometry left right =
  position_view left = position_view right
  && Topology.View.view (Geometry.topology left)
     = Topology.View.view (Geometry.topology right)
  && List.equal (fun left right ->
      Attribute.owner left = Attribute.owner right
      && Attribute.name left = Attribute.name right
      && equal_storage (Attribute.View.storage left)
           (Attribute.View.storage right))
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && Group.name left = Group.name right
      && Group.ordered_elements left = Group.ordered_elements right)
      (Geometry.groups left) (Geometry.groups right)
  && List.equal (fun left right ->
      Edge_group.name left = Edge_group.name right
      && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
         = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right))
      (Geometry.edge_groups left) (Geometry.edge_groups right)

let expect_invalid work message = match work () with
  | Error error ->
      check (Error.code error = "invalid_separate_pieces")
        (message ^ ": wrong code")
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "rdk Separate Pieces tests passed"
