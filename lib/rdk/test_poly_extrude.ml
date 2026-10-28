open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let attribute_storage_equal left right =
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
  | _ -> false

let geometry_equal left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
      Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && attribute_storage_equal left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && String.equal (Group.name left) (Group.name right)
      && Group.length left = Group.length right
      && Group.ordered_elements left = Group.ordered_elements right
      && let equal = ref true in
         for index = 0 to Group.length left - 1 do
           if Group.mem index left <> Group.mem index right then equal := false
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

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get_ok

let group_cardinality owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> Group.cardinality group
  | None -> fail ("missing group " ^ name)

let edge_cardinality name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> Edge_group.cardinality group
  | None -> fail ("missing edge group " ^ name)

let run () =
  let mixed_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|-1.; 1.; -1.; 1.; 3.; 4.|] ~y:[|0.; 0.; 0.; 0.; 0.; 0.|]
      ~z:[|-1.; -1.; 1.; 1.; 0.; 0.|] in
  let mixed_topology = Topology.create_owned ~point_count:6
      ~vertex_points:[|0; 1; 3; 0; 3; 2; 4; 5|]
      ~primitive_offsets:[|0; 3; 6; 8|]
      ~primitive_kinds:[|Topology.Polygon; Topology.Polygon;
        Topology.Open_polyline|] |> get_ok in
  let mixed = Geometry.create ~positions:mixed_positions ~topology:mixed_topology ()
      |> get_ok in
  let polygon_selection = Group.init ~owner:Group.Primitive ~name:"polygons"
      (Geometry.primitive_count mixed) (fun primitive -> primitive < 2) in
  let mixed = Geometry.with_group polygon_selection mixed |> get_ok in
  let mixed_output = Poly_extrude.run ~primitives:polygon_selection
      ~divide:Poly_extrude.Extrude_connected_components ~distance:1. mixed |> get_rdk in
  check (Geometry.primitive_count mixed_output = 9
      && Array.exists (fun primitive ->
        Topology.primitive_kind (Geometry.topology mixed_output) primitive
          = Topology.Open_polyline)
        (Array.init (Geometry.primitive_count mixed_output) Fun.id))
    "Poly Extrude did not preserve an unselected curve";
  let curve_selection = Group.init ~owner:Group.Primitive ~name:"curve"
      (Geometry.primitive_count mixed) (fun primitive -> primitive = 2) in
  (match Poly_extrude.run ~primitives:curve_selection
      ~divide:Poly_extrude.Extrude_connected_components ~distance:1. mixed with
   | Error error -> check (Error.code error = "invalid_geometry")
       "selected-curve Poly Extrude diagnostic"
   | Ok _ -> fail "Poly Extrude accepted a selected curve");

  let non_manifold_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; 0.|] ~y:[|0.; 0.; 1.; -1.; 0.|]
      ~z:[|0.; 0.; 0.; 0.; 1.|] in
  let non_manifold_topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 1;0;3; 0;1;4|]
      ~primitive_offsets:[|0;3;6;9|] |> get_ok in
  let non_manifold = Geometry.create ~positions:non_manifold_positions
      ~topology:non_manifold_topology () |> get_ok in
  (match Poly_extrude.run ~divide:Poly_extrude.Extrude_connected_components
      ~distance:1. non_manifold with
   | Error error -> check (Error.code error = "invalid_geometry")
       "non-manifold Poly Extrude diagnostic"
   | Ok _ -> fail "connected Poly Extrude accepted a non-manifold edge");

  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  let dense = Plane_generators.grid ~columns:320 ~rows:240 ~size:20. () |> get_rdk in
  let run domains = Parallel.run ~domains (fun () ->
      Poly_extrude.run ~grain:257
        ~divide:Poly_extrude.Extrude_connected_components ~divisions:4
        ~front_group:"front" ~side_group:"side"
        ~front_boundary_group:"front_boundary"
        ~back_boundary_group:"back_boundary" ~distance:0.75 dense |> get_rdk) in
  let one = run 1 and many = run 4 in
  check (geometry_equal one many)
    "connected Poly Extrude differs across domain counts";
  check (Geometry.point_count one = (321 * 241 * 5)
      && group_cardinality Group.Primitive "front" one = 320 * 240 * 2
      && group_cardinality Group.Primitive "side" one = (320 + 240) * 2 * 4)
    "connected Poly Extrude scale cardinality";
  print_endline "poly extrude tests passed"
