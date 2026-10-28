open Rdk_test_support
open Rdk

let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let edge_group geometry name predicate =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  Edge_group.init ~grain:1 ~topology ~index ~name predicate

let manifold_edges geometry =
  let index = Topology_index.create (Geometry.topology geometry) in
  edge_group geometry "interior" (fun edge ->
    Topology_index.edge_incidence_count index edge = 2)

let boundary_edges geometry =
  let index = Topology_index.create (Geometry.topology geometry) in
  edge_group geometry "boundary" (fun edge ->
    Topology_index.edge_incidence_count index edge = 1)

let add_payload geometry =
  let vertex_count = Geometry.vertex_count geometry
  and primitive_count = Geometry.primitive_count geometry
  and point_count = Geometry.point_count geometry in
  let point = Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.init point_count float_of_int)) |> get
  and vertex = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int (Array.init vertex_count (fun value -> 100 + value))) |> get
  and primitive = Attribute.create_owned ~owner:Attribute.Primitive ~name:"face"
      (Attribute.Int (Array.init primitive_count (fun value -> 10 + value))) |> get
  and vertex_lists = Attribute.create_owned ~owner:Attribute.Vertex
      ~name:"neighbors" (Attribute.Int_array
        (Packed.Int_array.create_owned
          ~offsets:(Array.init (vertex_count + 1) (fun value -> value * 2))
          ~values:(Array.init (vertex_count * 2) (fun value -> value mod 7))
         |> get)) |> get
  and vertex_group = Group.ordered ~owner:Group.Vertex ~name:"ordered_corners"
      ~length:vertex_count
      (Array.init vertex_count (fun value -> vertex_count - value - 1)) |> get
  and primitive_group = Group.ordered ~owner:Group.Primitive ~name:"ordered_faces"
      ~length:primitive_count
      (Array.init primitive_count (fun value -> primitive_count - value - 1)) |> get in
  geometry |> Geometry.with_attribute point |> get
  |> Geometry.with_attribute vertex |> get
  |> Geometry.with_attribute primitive |> get
  |> Geometry.with_attribute vertex_lists |> get
  |> Geometry.with_group vertex_group |> get
  |> Geometry.with_group primitive_group |> get

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.View.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let equal_geometry left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.compare_lengths (Geometry.attributes left)
       (Geometry.attributes right) = 0
  && List.for_all2 (fun left right ->
    Attribute.owner left = Attribute.owner right
    && Attribute.name left = Attribute.name right
    && match Attribute.View.storage left, Attribute.View.storage right with
       | Attribute.Float a, Attribute.Float b -> a = b
       | Attribute.Int a, Attribute.Int b -> a = b
       | Attribute.Text a, Attribute.Text b -> a = b
       | Attribute.Float2 a, Attribute.Float2 b ->
           Packed.Float2.View.view a = Packed.Float2.View.view b
       | Attribute.Float3 a, Attribute.Float3 b ->
           Packed.Float3.View.view a = Packed.Float3.View.view b
       | Attribute.Float4 a, Attribute.Float4 b ->
           Packed.Float4.View.view a = Packed.Float4.View.view b
       | Attribute.Int_array a, Attribute.Int_array b ->
           Packed.Int_array.View.view a = Packed.Int_array.View.view b
       | Attribute.Float_array a, Attribute.Float_array b ->
           Packed.Float_array.View.view a = Packed.Float_array.View.view b
       | _ -> false) (Geometry.attributes left) (Geometry.attributes right)
  && List.compare_lengths (Geometry.groups left) (Geometry.groups right) = 0
  && List.for_all2 (fun left right ->
    Group.owner left = Group.owner right
    && Group.name left = Group.name right
    && Group.length left = Group.length right
    && Group.ordered_elements left = Group.ordered_elements right
    && Group.View.bits_view left = Group.View.bits_view right)
      (Geometry.groups left) (Geometry.groups right)
  && List.compare_lengths (Geometry.edge_groups left)
       (Geometry.edge_groups right) = 0
  && List.for_all2 (fun left right ->
    Edge_group.name left = Edge_group.name right
    && Edge_group.length left = Edge_group.length right
    && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
       = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right))
      (Geometry.edge_groups left) (Geometry.edge_groups right)

let two_quads () =
  Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:1 ~size:2. ()
  |> get_rdk |> add_payload
  |> fun geometry -> Geometry.with_edge_group (boundary_edges geometry) geometry
       |> get

let test_interior () =
  let source = two_quads () in
  let edges = manifold_edges source in
  let output = Dissolve.run ~edges ~remove_unused_points:false source |> get_rdk in
  check (Geometry.point_count output = 6 && Geometry.vertex_count output = 6
      && Geometry.primitive_count output = 1) "two-quad cardinality";
  check (int_attribute Attribute.Primitive "face" output = [|10|])
    "primitive ancestry";
  check (Array.length (int_attribute Attribute.Vertex "corner" output) = 6)
    "vertex ancestry";
  check (Geometry.find_attribute ~owner:Attribute.Vertex "neighbors" output
      |> Option.get |> Attribute.length = 6) "CSR vertex ancestry";
  check (Geometry.find_group ~owner:Group.Vertex "ordered_corners" output
      |> Option.get |> Group.cardinality = 6) "ordered vertex group ancestry";
  check (Geometry.find_group ~owner:Group.Primitive "ordered_faces" output
      |> Option.get |> Group.ordered_elements = Some [|0|])
    "ordered primitive group ancestry";
  check (Geometry.find_edge_group "boundary" output
      |> Option.get |> Edge_group.cardinality = 6) "native edge-group ancestry";
  let source_weight = Geometry.find_attribute ~owner:Attribute.Point "weight" source
      |> Option.get and output_weight = Geometry.find_attribute
      ~owner:Attribute.Point "weight" output |> Option.get in
  check (Attribute.storage_id source_weight = Attribute.storage_id output_weight)
    "point payload was copied without point compaction";
  let cleaned = Dissolve.run ~edges ~remove_inline_points:true
      ~collinearity_tolerance:1e-10 source |> get_rdk in
  check (Geometry.point_count cleaned = 4 && Geometry.vertex_count cleaned = 4
      && Geometry.primitive_count cleaned = 1) "inline cleanup rectangle";
  let boundary = boundary_edges source in
  let inverted = Dissolve.run ~edges:boundary
      ~operation:Dissolve.Dissolve_non_selected ~remove_unused_points:false source
      |> get_rdk in
  check (Geometry.primitive_count inverted = 1 && Geometry.vertex_count inverted = 6)
    "Dissolve Non-Selected"

let test_boundary () =
  let source = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:1 ~rows:1 ~size:1. ()
      |> get_rdk in
  let index = Topology_index.create (Geometry.topology source) in
  let selected = edge_group source "one" (fun edge -> edge = 0) in
  check (Topology_index.edge_incidence_count index 0 = 1) "fixture boundary edge";
  let deleted = Dissolve.run ~edges:selected source |> get_rdk in
  check (Geometry.point_count deleted = 0 && Geometry.primitive_count deleted = 0)
    "boundary dissolve deletion";
  let curve = Dissolve.run ~edges:selected ~create_boundary_curves:true source
      |> get_rdk in
  check (Geometry.point_count curve = 4 && Geometry.vertex_count curve = 4
      && Geometry.primitive_count curve = 1
      && Topology.primitive_kind (Geometry.topology curve) 0
         = Topology.Open_polyline) "boundary dissolve curve"

let ring () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|-2.;2.;2.;-2.; -1.;1.;1.;-1.|]
      ~y:[|0.;0.;0.;0.; 0.;0.;0.;0.|]
      ~z:[|-2.;-2.;2.;2.; -1.;-1.;1.;1.|] in
  let topology = Topology.polygons_owned ~point_count:8
      ~vertex_points:[|0;1;5;4; 1;2;6;5; 2;3;7;6; 3;0;4;7|]
      ~primitive_offsets:[|0;4;8;12;16|] |> get in
  Geometry.create ~positions ~topology () |> get

let test_bridges () =
  let source = ring () in
  let edges = manifold_edges source in
  let disjoint = Dissolve.run ~edges
      ~bridge_policy:Dissolve.Create_disjoint_polygons ~remove_unused_points:false source
      |> get_rdk in
  check (Geometry.primitive_count disjoint = 2
      && Geometry.vertex_count disjoint = 8) "disjoint bridge loops";
  let bridged = Dissolve.run ~edges
      ~bridge_policy:Dissolve.Create_bridged_polygons ~remove_unused_points:false source
      |> get_rdk in
  check (Geometry.primitive_count bridged = 1
      && Geometry.vertex_count bridged = 10) "bridged polygon loop";
  let deleted = Dissolve.run ~edges
      ~bridge_policy:Dissolve.Delete_bridge_polygons source |> get_rdk in
  check (Geometry.primitive_count deleted = 0 && Geometry.point_count deleted = 0)
    "delete bridge component"

let test_errors_and_curves () =
  let curve = Line_geometry.polyline [|0.,0.,0.;1.,0.,0.;2.,0.,0.|] |> get_rdk in
  let all = edge_group curve "all" (fun _ -> true) in
  check (Dissolve.run ~edges:all curve |> get_rdk == curve)
    "polygon-curve edges were not ignored";
  let cancel = Cancel.create () in Cancel.cancel cancel;
  let cancelled_source = two_quads () in
  (match Dissolve.run ~cancel ~edges:(manifold_edges cancelled_source)
      cancelled_source with
   | Error error -> check (Error.code error = "cancelled") "cancellation code"
   | Ok _ -> fail "cancelled dissolve succeeded");
  let source = two_quads () in
  let foreign = manifold_edges (ring ()) in
  (match Dissolve.run ~edges:foreign source with
   | Error error -> check (Error.code error = "invalid_topology")
       "foreign topology diagnostic"
   | Ok _ -> fail "foreign edge group accepted");
  let malformed vertex_points =
    let positions = Packed.Float3.View.of_owned_exn
        ~x:[|0.;1.;0.;1.;0.5|] ~y:[|0.;0.;1.;1.;2.|]
        ~z:(Array.make 5 0.) in
    let topology = Topology.polygons_owned ~point_count:5 ~vertex_points
        ~primitive_offsets:(Array.init ((Array.length vertex_points / 3) + 1)
          (fun primitive -> primitive * 3)) |> get in
    Geometry.create ~positions ~topology () |> get in
  let expect_invalid label geometry =
    let topology = Geometry.topology geometry in
    let index = Topology_index.create topology in
    let edge = Topology_index.find_edge_index index ~a:0 ~b:1 in
    check (edge >= 0) (label ^ " fixture edge");
    let selected = Edge_group.init ~grain:1 ~topology ~index ~name:"bad"
        (fun candidate -> candidate = edge) in
    match Dissolve.run ~edges:selected geometry with
    | Error error -> check (Error.code error = "invalid_topology")
        (label ^ " diagnostic")
    | Ok _ -> fail (label ^ " accepted") in
  expect_invalid "non-manifold" (malformed [|0;1;2; 1;0;3; 0;1;4|]);
  expect_invalid "inconsistent winding" (malformed [|0;1;2; 0;1;3|]);
  (match Dissolve.run ~collinearity_tolerance:nan source with
   | Error error -> check (Error.code error = "invalid_topology")
       "non-finite tolerance diagnostic"
   | Ok _ -> fail "non-finite tolerance accepted")

let test_normals () =
  let source = two_quads () in
  let normals = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn
        ~x:(Array.make (Geometry.point_count source) 1.)
        ~y:(Array.make (Geometry.point_count source) 0.)
        ~z:(Array.make (Geometry.point_count source) 0.))) |> get in
  let source = Geometry.with_attribute normals source |> get in
  let output = Dissolve.run ~edges:(manifold_edges source)
      ~remove_inline_points:true source |> get_rdk in
  let normal = Geometry.find_attribute ~owner:Attribute.Point "N" output
      |> Option.get in
  check (Attribute.length normal = Geometry.point_count output)
    "recomputed normal cardinality";
  match Attribute.View.storage normal with
  | Attribute.Float3 values ->
      let values = Packed.Float3.View.view values in
      check (Array.for_all (fun point ->
        let length = sqrt ((values.x.(point) *. values.x.(point))
          +. (values.y.(point) *. values.y.(point))
          +. (values.z.(point) *. values.z.(point))) in
        abs_float (length -. 1.) < 1e-12
        && (abs_float (values.x.(point) -. 1.) > 1e-12
            || abs_float values.y.(point) > 1e-12
            || abs_float values.z.(point) > 1e-12))
          (Array.init (Array.length values.x) Fun.id))
        "recomputed normal values"
  | _ -> fail "recomputed normal storage"

let test_parallel () =
  let source = Plane_generators.grid ~grain:127 ~connectivity:Plane_generators.Grid_quads
      ~columns:300 ~rows:220 ~size:20. () |> get_rdk |> add_payload in
  let edges = manifold_edges source in
  let run domains = Parallel.run ~domains (fun () ->
      Dissolve.run ~grain:257 ~edges ~remove_inline_points:true
        ~collinearity_tolerance:1e-10 source |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four) "one/four-domain output differs";
  check (Geometry.point_count one = 4 && Geometry.vertex_count one = 4
      && Geometry.primitive_count one = 1) "large grid cardinality"

let run () =
  test_interior ();
  test_boundary ();
  test_bridges ();
  test_errors_and_curves ();
  test_normals ();
  test_parallel ();
  print_endline "test_dissolve: ok"
