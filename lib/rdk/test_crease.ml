open Rays
open Rdk
open Rdk_test_support

let attribute owner name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun value -> Geometry.with_attribute value geometry |> Result.get_ok

let float_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value ->
      (match Attribute.View.storage value with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " storage changed"))
  | None -> fail ("missing " ^ name)

let float4_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value ->
      (match Attribute.View.storage value with
       | Attribute.Float4 values -> Packed.Float4.View.view values
       | _ -> fail (name ^ " storage changed"))
  | None -> fail ("missing " ^ name)

let two_quads () =
  Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:1 ~size:2. () |> get_ok

let shared_edge geometry =
  let index = Topology_index.create (Geometry.topology geometry) in
  let edge = ref (-1) in
  for candidate = 0 to Topology_index.edge_count index - 1 do
    if Topology_index.edge_incidence_count index candidate = 2 then
      edge := candidate
  done;
  if !edge < 0 then fail "fixture has no shared edge";
  index, !edge

let edge_group geometry index selected =
  Edge_group.init ~grain:1 ~topology:(Geometry.topology geometry) ~index
    ~name:"crease_edges" selected

let incident_vertices index edge =
  Array.init (Topology_index.edge_incidence_count index edge)
    (fun local -> Topology_index.edge_vertex index ~edge ~local)

let non_manifold () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; 0.|]
      ~y:[|0.; 0.; 1.; -1.; 0.|]
      ~z:[|0.; 0.; 0.; 0.; 1.|] in
  let topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0; 1; 2; 1; 0; 3; 0; 1; 4|]
      ~primitive_offsets:[|0; 3; 6; 9|] |> Result.get_ok in
  Geometry.create ~positions ~topology () |> Result.get_ok

let equal_float4 left right =
  let left = Packed.Float4.View.view left
  and right = Packed.Float4.View.view right in
  left.x = right.x && left.y = right.y && left.z = right.z && left.w = right.w

let equal_storage left right = match left, right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Float4 left, Attribute.Float4 right -> equal_float4 left right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_attributes left right =
  List.equal (fun left right -> Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && equal_storage (Attribute.View.storage left)
           (Attribute.View.storage right))
    (Geometry.attributes left) (Geometry.attributes right)

let expect_invalid work message = match work () with
  | Error error -> check (Error.code error = "invalid_crease") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "rdk crease tests passed"
