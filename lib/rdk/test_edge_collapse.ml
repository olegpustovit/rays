open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error message -> fail message

let equal_storage left right =
  match Attribute.Private.storage left, Attribute.Private.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_geometry left right =
  let left_positions = Packed.Float3.Private.view (Geometry.positions left)
  and right_positions = Packed.Float3.Private.view (Geometry.positions right)
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal (fun left right -> Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_ok

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let quad () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;2.;2.;0.|] ~y:[|0.;0.;2.;2.|] ~z:[|0.;0.;0.;0.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|] |> get_ok in
  let collapse = edge_group_of_pairs topology "collapse" [|0,1|]
  and all = edge_group_of_pairs topology "all" [|0,1;1,2;2,3;3,0|] in
  let ordered = Group.ordered ~owner:Group.Point ~name:"ordered"
      ~length:4 [|1;0;2|] |> get_ok
  and corners = Group.init ~grain:1 ~owner:Group.Vertex ~name:"corners" 4
      (fun vertex -> vertex < 3) in
  let rows = Packed.Int_array.create_owned ~offsets:[|0;1;2;3;4|]
      ~values:[|10;11;12;13|] |> get_ok in
  Geometry.create ~positions ~topology
    ~attributes:[
      attribute Attribute.Point "weight" (Attribute.Float [|0.;2.;4.;6.|]);
      attribute Attribute.Point "piece" (Attribute.Int [|0;0;1;1|]);
      attribute Attribute.Point "label"
        (Attribute.Text [|"a";"b";"c";"d"|]);
      attribute Attribute.Point "rows" (Attribute.Int_array rows);
      attribute Attribute.Point "__rdk_edge_collapse_target"
        (Attribute.Int [|9;8;7;6|]);
      attribute Attribute.Point "N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|1.;1.;1.;1.|]
          ~y:[|0.;0.;0.;0.|] ~z:[|0.;0.;0.;0.|]));
      attribute Attribute.Vertex "corner"
        (Attribute.Float [|10.;20.;30.;40.|]);
      attribute Attribute.Primitive "material" (Attribute.Text [|"panel"|]);
      attribute Attribute.Detail "meta" (Attribute.Int [|42|]);
    ] ~groups:[ordered; corners] ~edge_groups:[collapse; all] () |> get_ok

let point_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry
      |> Option.get |> Attribute.Private.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not point float")

let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry
      |> Option.get |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " is not point integer")

let vertex_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry
      |> Option.get |> Attribute.Private.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not vertex float")

let edge_cardinality name geometry =
  Geometry.find_edge_group name geometry |> Option.get |> Edge_group.cardinality

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let run () =
  let source = quad () in
  let collapse = Geometry.find_edge_group "collapse" source |> Option.get in
  let output = Edge_collapse.run ~grain:1 ~edges:collapse source |> get_rdk in
  let positions = Packed.Float3.Private.view (Geometry.positions output)
  and topology = Topology.Private.view (Geometry.topology output) in
  check (Geometry.point_count output = 3 && Geometry.vertex_count output = 3
      && Geometry.primitive_count output = 1)
    "Edge Collapse cardinality";
  check (positions.x = [|1.;2.;0.|] && positions.y = [|0.;2.;2.|]
      && positions.z = [|0.;0.;0.|])
    "Edge Collapse center positions";
  check (topology.vertex_points = [|0;1;2|]
      && topology.primitive_offsets = [|0;3|])
    "Edge Collapse topology cleanup";
  check (point_float "weight" output = [|0.;4.;6.|]
      && point_int "piece" output = [|0;1;1|]
      && point_int "__rdk_edge_collapse_target" output = [|9;7;6|])
    "Edge Collapse stable point payload and temporary-name isolation";
  check (vertex_float "corner" output = [|10.;30.;40.|])
    "Edge Collapse vertex ancestry";
  check (edge_cardinality "collapse" output = 0
      && edge_cardinality "all" output = 3)
    "Edge Collapse native edge ancestry";
  let ordered = Geometry.find_group ~owner:Group.Point "ordered" output
      |> Option.get in
  check (Group.ordered_elements ordered = Some [|0;1|])
    "Edge Collapse ordered point-group ancestry";
  (match Geometry.find_attribute ~owner:Attribute.Point "N" output
      |> Option.get |> Attribute.Private.storage with
   | Attribute.Float3 values ->
       let values = Packed.Float3.Private.view values in
       check (values.x = [|0.;0.;0.|] && values.y = [|0.;0.;0.|]
           && values.z = [|1.;1.;1.|])
         "Edge Collapse point-normal recomputation"
   | _ -> fail "Edge Collapse point normals have wrong storage");

  let path = edge_group_of_pairs (Geometry.topology source) "path"
      [|0,1;1,2|] in
  let separated = Edge_collapse.run ~grain:1 ~edges:path
      ~connectivity_attribute:"piece" source |> get_rdk in
  let separated_positions = Packed.Float3.Private.view
      (Geometry.positions separated) in
  check (Geometry.point_count separated = 3
      && separated_positions.x = [|1.;2.;0.|]
      && separated_positions.y = [|0.;2.;2.|])
    "Edge Collapse connectivity boundary";
  check (Edge_collapse.run ~grain:1 ~edges:path
      ~connectivity_attribute:"rows" source |> get_rdk == source)
    "Edge Collapse ragged connectivity boundaries were ignored";

  let no_cleanup = Edge_collapse.run ~grain:1 ~edges:collapse
      ~remove_degenerate_primitives:false ~recompute_point_normals:false source
      |> get_rdk in
  let no_cleanup_topology = Topology.Private.view
      (Geometry.topology no_cleanup) in
  check (Geometry.point_count no_cleanup = 3
      && Geometry.vertex_count no_cleanup = 4
      && no_cleanup_topology.vertex_points = [|0;0;1;2|])
    "Edge Collapse keep-degenerate mode";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" no_cleanup = None)
    "Edge Collapse retained stale normals";
  let without_normals = Geometry.without_attribute ~owner:Attribute.Point "N"
      source in
  let no_invented_normals = Edge_collapse.run ~edges:collapse without_normals
      |> get_rdk in
  check (Geometry.find_attribute ~owner:Attribute.Point "N"
      no_invented_normals = None)
    "Edge Collapse invented point normals";

  let empty = Edge_group.init ~grain:1 ~topology:(Geometry.topology source)
      ~index:(Topology_index.create (Geometry.topology source)) ~name:"empty"
      (Fun.const false) in
  check (Edge_collapse.run ~edges:empty source |> get_rdk == source)
    "empty Edge Collapse was not an identity";

  let curve = Line_geometry.polyline [|0.,0.,0.;1.,0.,0.;2.,0.,0.;3.,0.,0.|]
      |> get_rdk in
  let collapsed_curve = Edge_collapse.run curve |> get_rdk in
  check (Geometry.point_count collapsed_curve = 0
      && Geometry.vertex_count collapsed_curve = 0
      && Geometry.primitive_count collapsed_curve = 0)
    "whole-curve Edge Collapse cleanup";

  expect_code "invalid_topology"
    (Edge_collapse.run ~edges:collapse ~connectivity_attribute:"missing" source);
  expect_code "invalid_topology"
    (Edge_collapse.run ~edges:collapse ~connectivity_attribute:" " source);
  let non_finite_topology = Topology.create_owned ~point_count:2
      ~vertex_points:[|0;1|] ~primitive_offsets:[|0;2|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> get_ok in
  let non_finite = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn
        ~x:[|nan;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|])
      ~topology:non_finite_topology () |> get_ok in
  expect_code "invalid_topology" (Edge_collapse.run non_finite);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Edge_collapse.run ~cancel:cancelled source);

  let large = Plane_generators.grid ~grain:127 ~connectivity:Plane_generators.Grid_quads
      ~columns:180 ~rows:140 ~size:20. () |> get_rdk in
  let large_topology = Geometry.topology large
  and large_index = Topology_index.create (Geometry.topology large) in
  let disjoint = Edge_group.init ~grain:127 ~topology:large_topology
      ~index:large_index ~name:"disjoint" (fun edge ->
        let a, b = Topology_index.edge_points large_index edge in
        let low = min a b and high = max a b in
        high = low + 1 && low mod 4 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
    Edge_collapse.run ~grain:127 ~edges:disjoint large |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "Edge Collapse differs across domain counts";
  check (Geometry.point_count one < Geometry.point_count large
      && Geometry.point_count one > 0)
    "Edge Collapse scale cardinality";
  print_endline "edge collapse tests passed"
