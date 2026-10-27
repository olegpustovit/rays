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

let two_quads () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 2.; 0.; 1.; 2.|]
      ~y:[|0.; 0.; 0.; 1.; 1.; 1.|]
      ~z:[|0.; 0.; 0.; 0.; 0.; 0.|] in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;1;4;3; 1;2;5;4|]
      ~primitive_offsets:[|0;4;8|] |> get_ok in
  let cut = edge_group_of_pairs topology "cut" [|1, 4|] in
  let index = Topology_index.create topology in
  let all_edges = Edge_group.init ~grain:1 ~topology ~index ~name:"all_edges"
      (Fun.const true) in
  let point_order = Group.ordered ~owner:Group.Point ~name:"edge_points"
      ~length:6 [|4;1|] |> get_ok
  and vertex_group = Group.init ~grain:1 ~owner:Group.Vertex
      ~name:"edge_corners" 8 (fun vertex -> Array.mem vertex [|1;2;4;7|])
  and primitive_group = Group.ordered ~owner:Group.Primitive ~name:"faces"
      ~length:2 [|1;0|] |> get_ok in
  let point_rows = Packed.Int_array.create_owned
      ~offsets:[|0;1;2;3;4;5;6|] ~values:[|0;1;2;3;4;5|] |> get_ok in
  Geometry.create ~positions ~topology
    ~attributes:[
      attribute Attribute.Point "weight"
        (Attribute.Float [|0.;10.;20.;30.;40.;50.|]);
      attribute Attribute.Point "id" (Attribute.Int [|0;1;2;3;4;5|]);
      attribute Attribute.Point "label"
        (Attribute.Text [|"p0";"p1";"p2";"p3";"p4";"p5"|]);
      attribute Attribute.Point "uv" (Attribute.Float2
        (Packed.Float2.of_owned ~x:[|0.;1.;2.;0.;1.;2.|]
           ~y:[|0.;0.;0.;1.;1.;1.|] |> get_ok));
      attribute Attribute.Point "rows" (Attribute.Int_array point_rows);
      attribute Attribute.Vertex "corner"
        (Attribute.Float [|0.;1.;2.;3.;4.;5.;6.;7.|]);
      attribute Attribute.Primitive "material" (Attribute.Text [|"a";"b"|]);
      attribute Attribute.Detail "meta" (Attribute.Float4
        (Packed.Float4.of_owned ~x:[|1.|] ~y:[|2.|] ~z:[|3.|] ~w:[|4.|]
         |> get_ok));
    ] ~groups:[point_order; vertex_group; primitive_group]
      ~edge_groups:[cut; all_edges] () |> get_ok

let float_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
      |> Attribute.Private.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not float")

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
      |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " is not integer")

let edge_cardinality name geometry =
  Geometry.find_edge_group name geometry |> Option.get |> Edge_group.cardinality

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let run () =
  let source = two_quads () in
  let cut = Geometry.find_edge_group "cut" source |> Option.get in
  let shared = Subdivide.edge_divide ~grain:1 ~edges:cut ~divisions:3 source
      |> get_rdk in
  let topology = Topology.Private.view (Geometry.topology shared)
  and positions = Packed.Float3.Private.view (Geometry.positions shared) in
  check (Geometry.point_count shared = 8 && Geometry.vertex_count shared = 12
      && Geometry.primitive_count shared = 2)
    "shared Edge Divide cardinality";
  check (topology.vertex_points = [|0;1;6;7;4;3; 1;2;5;4;7;6|]
      && topology.primitive_offsets = [|0;6;12|])
    "shared Edge Divide topology/order";
  check (positions.x = [|0.;1.;2.;0.;1.;2.;1.;1.|]
      && abs_float (positions.y.(6) -. (1. /. 3.)) < 1e-14
      && abs_float (positions.y.(7) -. (2. /. 3.)) < 1e-14)
    "shared Edge Divide positions";
  let weights = float_values Attribute.Point "weight" shared
  and ids = int_values Attribute.Point "id" shared
  and corners = float_values Attribute.Vertex "corner" shared in
  check (Array.sub weights 0 6 = [|0.;10.;20.;30.;40.;50.|]
      && abs_float (weights.(6) -. 20.) < 1e-14
      && abs_float (weights.(7) -. 30.) < 1e-14)
    "shared Edge Divide numeric point interpolation";
  check (ids = [|0;1;2;3;4;5;1;4|])
    "shared Edge Divide discrete point ancestry";
  check (Array.length corners = 12
      && abs_float (corners.(2) -. (4. /. 3.)) < 1e-14
      && abs_float (corners.(3) -. (5. /. 3.)) < 1e-14
      && corners.(10) = 6. && corners.(11) = 5.)
    "shared Edge Divide face-varying interpolation";
  check (edge_cardinality "cut" shared = 3
      && edge_cardinality "all_edges" shared = 9)
    "shared Edge Divide native edge ancestry";
  let points = Geometry.find_group ~owner:Group.Point "edge_points" shared
      |> Option.get
  and corners_group = Geometry.find_group ~owner:Group.Vertex "edge_corners"
      shared |> Option.get in
  check (Group.ordered_elements points = Some [|4;1;6;7|]
      && Group.cardinality points = 4)
    "shared Edge Divide ordered point-group ancestry";
  check (Group.cardinality corners_group = 8)
    "shared Edge Divide vertex-group interpolation";
  let source_detail = Geometry.find_attribute ~owner:Attribute.Detail "meta" source
      |> Option.get
  and output_detail = Geometry.find_attribute ~owner:Attribute.Detail "meta" shared
      |> Option.get in
  check (source_detail == output_detail)
    "Edge Divide did not structurally share detail payload";

  let unique = Subdivide.edge_divide ~grain:1 ~edges:cut ~divisions:3
      ~share_points:false source |> get_rdk in
  let unique_topology = Topology.Private.view (Geometry.topology unique)
  and unique_positions = Packed.Float3.Private.view (Geometry.positions unique) in
  check (Geometry.point_count unique = 10 && Geometry.vertex_count unique = 12
      && unique_topology.vertex_points
         = [|0;1;6;7;4;3; 1;2;5;4;8;9|])
    "unique Edge Divide topology/cardinality";
  check (abs_float (unique_positions.y.(8) -. (2. /. 3.)) < 1e-14
      && abs_float (unique_positions.y.(9) -. (1. /. 3.)) < 1e-14)
    "unique Edge Divide reversed incident interpolation";
  check (edge_cardinality "cut" unique = 6
      && edge_cardinality "all_edges" unique = 12)
    "unique Edge Divide native edge ancestry";

  check (Subdivide.edge_divide source |> get_rdk == source)
    "empty Edge Divide selection was not an identity";
  check (Subdivide.edge_divide ~edges:cut ~divisions:1 source |> get_rdk == source)
    "one-segment Edge Divide was not an identity";
  let empty = Edge_group.init ~grain:1 ~topology:(Geometry.topology source)
      ~index:(Topology_index.create (Geometry.topology source)) ~name:"empty"
      (Fun.const false) in
  check (Subdivide.edge_divide ~edges:empty ~divisions:5 source |> get_rdk == source)
    "empty edge-group Edge Divide was not an identity";

  let curve = Line_geometry.polyline [|0.,0.,0.; 2.,0.,0.; 2.,2.,0.|] |> get_rdk
      |> Group_mesh.group_edges ~name:"curve_edges" |> get_rdk in
  let curve_edges = Geometry.find_edge_group "curve_edges" curve |> Option.get in
  let curve_output = Subdivide.edge_divide ~edges:curve_edges ~divisions:2 curve
      |> get_rdk in
  let curve_topology = Topology.Private.view (Geometry.topology curve_output) in
  check (Geometry.point_count curve_output = 5
      && curve_topology.vertex_points = [|0;3;1;4;2|]
      && Bytes.get curve_topology.primitive_kinds 0 = '\001')
    "open-curve Edge Divide";

  let closed_curve = Line_geometry.polyline ~closed:true
      [|0.,0.,0.; 2.,0.,0.; 1.,2.,0.|] |> get_rdk
      |> Group_mesh.group_edges ~name:"closed_edges" |> get_rdk in
  let closed_edges = Geometry.find_edge_group "closed_edges" closed_curve
      |> Option.get in
  let closed_output = Subdivide.edge_divide ~edges:closed_edges ~divisions:2
      closed_curve |> get_rdk in
  let closed_topology = Topology.Private.view
      (Geometry.topology closed_output) in
  check (Geometry.point_count closed_output = 6
      && closed_topology.vertex_points = [|0;3;1;4;2;5|]
      && Bytes.get closed_topology.primitive_kinds 0 = '\002'
      && edge_cardinality "closed_edges" closed_output = 6)
    "closed-curve Edge Divide";

  let nonmanifold_topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 1;0;3; 0;1;4|]
      ~primitive_offsets:[|0;3;6;9|] |> get_ok in
  let nonmanifold = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn
        ~x:[|0.;1.;0.;0.;0.|] ~y:[|0.;0.;1.;(-1.);0.|]
        ~z:[|0.;0.;0.;0.;1.|]) ~topology:nonmanifold_topology () |> get_ok in
  let shared_edge = edge_group_of_pairs nonmanifold_topology "shared" [|0,1|] in
  let nonmanifold_output = Subdivide.edge_divide ~edges:shared_edge ~divisions:4
      nonmanifold |> get_rdk in
  check (Geometry.point_count nonmanifold_output = 8
      && Geometry.vertex_count nonmanifold_output = 18)
    "non-manifold shared Edge Divide cardinality";

  let other = Box_generator.box ~size:(Rays_math.Vec3.create 1. 1. 1.) () |> get_rdk
      |> Group_mesh.group_edges ~name:"other" |> get_rdk in
  let other_edges = Geometry.find_edge_group "other" other |> Option.get in
  expect_code "invalid_topology"
    (Subdivide.edge_divide ~edges:other_edges ~divisions:2 source);
  expect_code "invalid_topology"
    (Subdivide.edge_divide ~edges:cut ~divisions:0 source);
  let bad_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|nan;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|] in
  let bad_topology = Topology.create_owned ~point_count:2
      ~vertex_points:[|0;1|] ~primitive_offsets:[|0;2|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> get_ok in
  let bad = Geometry.create ~positions:bad_positions ~topology:bad_topology ()
      |> get_ok in
  let bad_edge = edge_group_of_pairs bad_topology "bad" [|0,1|] in
  expect_code "invalid_topology"
    (Subdivide.edge_divide ~edges:bad_edge ~divisions:2 bad);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Subdivide.edge_divide ~cancel:cancelled ~edges:cut ~divisions:2 source);

  let large = Plane_generators.grid ~grain:127 ~connectivity:Plane_generators.Grid_quads
      ~columns:160 ~rows:120 ~size:20. () |> get_rdk
      |> Group_mesh.group_edges ~grain:127 ~name:"all" |> get_rdk in
  let all = Geometry.find_edge_group "all" large |> Option.get in
  let run domains share_points = Parallel.run ~domains (fun () ->
    Subdivide.edge_divide ~grain:127 ~edges:all ~divisions:3 ~share_points large
    |> get_rdk) in
  let shared_one = run 1 true and shared_four = run 4 true
  and unique_one = run 1 false and unique_four = run 4 false in
  check (equal_geometry shared_one shared_four)
    "shared Edge Divide differed between one and four domains";
  check (equal_geometry unique_one unique_four)
    "unique Edge Divide differed between one and four domains";
  (* 161x121 points, 160*121 + 161*120 edges, two new points per shared edge *)
  check (Geometry.point_count shared_one
         = 161 * 121 + 2 * (160 * 121 + 161 * 120))
    "shared Edge Divide scale point cardinality";
  check (Geometry.point_count unique_one > Geometry.point_count shared_one)
    "unique Edge Divide duplicates interior edge points";
  print_endline "edge divide tests passed"
