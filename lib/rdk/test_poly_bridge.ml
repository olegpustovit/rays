open Rdk

let fail message = prerr_endline ("test_poly_bridge: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let loop_pairs ?(pairs = 1) ?(source_points = 4) ?destination_points () =
  let destination_points = Option.value ~default:source_points destination_points in
  let points_per_pair = source_points + destination_points in
  let point_count = pairs * points_per_pair
  and primitive_count = pairs * 2 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0.
  and vertex_points = Array.make point_count 0
  and primitive_offsets = Array.make (primitive_count + 1) 0 in
  let point_at = ref 0 and vertex_at = ref 0 and primitive_at = ref 0 in
  for pair = 0 to pairs - 1 do
    for side = 0 to 1 do
      let count = if side = 0 then source_points else destination_points in
      primitive_offsets.(!primitive_at) <- !vertex_at;
      for local = 0 to count - 1 do
        let point = !point_at + local
        and angle = 2. *. Float.pi *. float_of_int local /. float_of_int count in
        x.(point) <- (float_of_int pair *. 3.) +. cos angle;
        y.(point) <- float_of_int side;
        z.(point) <- sin angle;
        vertex_points.(!vertex_at + local) <- point
      done;
      point_at := !point_at + count;
      vertex_at := !vertex_at + count;
      incr primitive_at
    done
  done;
  primitive_offsets.(primitive_count) <- point_count;
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets ~primitive_kinds:(Array.make primitive_count Topology.Polygon)
      |> get in
  let point_attribute = Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.init point_count float_of_int)) |> get
  and height_attribute = Attribute.create_owned ~owner:Attribute.Point ~name:"height"
      (Attribute.Float (Array.copy y)) |> get
  and point_ids = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init point_count Fun.id)) |> get
  and point_labels = Attribute.create_owned ~owner:Attribute.Point ~name:"side"
      (Attribute.Text (Array.init point_count (fun point ->
        if point mod points_per_pair < source_points then "source" else "destination")))
      |> get
  and normals = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:(Array.make point_count 1.) ~y:(Array.make point_count 0.)
        ~z:(Array.make point_count 0.))) |> get
  and vertex_attribute = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int (Array.init point_count (fun value -> 100 + value))) |> get
  and vertex_height = Attribute.create_owned ~owner:Attribute.Vertex
      ~name:"corner_height" (Attribute.Float (Array.copy y)) |> get
  and primitive_attribute = Attribute.create_owned ~owner:Attribute.Primitive
      ~name:"surface" (Attribute.Int (Array.init primitive_count Fun.id)) |> get
  and vertex_group = Group.init ~grain:1 ~owner:Group.Vertex ~name:"all_corners"
      point_count (fun _ -> true)
  and source_faces = Group.init ~grain:1 ~owner:Group.Primitive ~name:"source_faces"
      primitive_count (fun primitive -> primitive land 1 = 0)
  and source_points_group = Group.init ~grain:1 ~owner:Group.Point
      ~name:"source_points" point_count (fun point ->
        point mod points_per_pair < source_points) in
  let geometry = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z) ~topology
      ~attributes:[point_attribute;height_attribute;point_ids;point_labels;normals;
        vertex_attribute;vertex_height;primitive_attribute]
      ~groups:[vertex_group;source_faces;source_points_group] () |> get in
  let index = Topology_index.create topology in
  let source = Edge_group.init ~grain:1 ~topology ~index ~name:"source_edges"
      (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        let local = a mod points_per_pair in local < source_points)
  and destination = Edge_group.init ~grain:1 ~topology ~index
      ~name:"destination_edges" (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        let local = a mod points_per_pair in local >= source_points) in
  let geometry = Geometry.with_edge_group source geometry |> get
      |> Geometry.with_edge_group destination |> get in
  geometry, source, destination

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let float_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.Private.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " storage")

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.Private.storage with
  | Attribute.Text values -> values
  | _ -> fail (name ^ " storage")

let test_closed_bridge_and_payload () =
  let source_geometry, source, destination = loop_pairs () in
  let point_attribute = Geometry.find_attribute ~owner:Attribute.Point "weight"
      source_geometry |> Option.get in
  let output = Poly_bridge.run ~source ~destination
      ~connect_closest_ends:false ~reverse_destination:true
      ~output_group:"bridge" source_geometry |> get_rdk in
  check (Geometry.point_count output = 8
      && Geometry.primitive_count output = 6
      && Geometry.vertex_count output = 24) "closed bridge cardinality";
  check (Geometry.find_group ~owner:Group.Primitive "bridge" output
      |> Option.get |> Group.cardinality = 4) "output group";
  check (Geometry.find_group ~owner:Group.Vertex "all_corners" output
      |> Option.get |> Group.cardinality = 24) "vertex-group ancestry";
  check (Geometry.find_group ~owner:Group.Primitive "source_faces" output
      |> Option.get |> Group.cardinality = 5) "primitive-group ancestry";
  check (int_attribute Attribute.Primitive "surface" output
      = [|0;1;0;0;0;0|]) "primitive ancestry";
  check (Geometry.find_edge_group "source_edges" output
      |> Option.get |> Edge_group.cardinality = 4) "source edge ancestry";
  check (Geometry.find_edge_group "destination_edges" output
      |> Option.get |> Edge_group.cardinality = 4) "destination edge ancestry";
  let output_point = Geometry.find_attribute ~owner:Attribute.Point "weight" output
      |> Option.get in
  check (Attribute.storage_id point_attribute = Attribute.storage_id output_point)
    "point payload not shared";
  let normals = Geometry.find_attribute ~owner:Attribute.Point "N" output
      |> Option.get in
  check (Attribute.length normals = 8) "normal cardinality";
  let index = Topology_index.create (Geometry.topology output) in
  check (Topology_index.boundary_edge_count index = 0
      && Topology_index.non_manifold_edge_count index = 0) "closed manifold";
  let bridge_only = Poly_bridge.run ~source ~destination ~keep_input:false
      ~connect_closest_ends:false source_geometry |> get_rdk in
  check (Geometry.primitive_count bridge_only = 4
      && Geometry.vertex_count bridge_only = 16) "bridge-only cardinality";
  let without_normals = Poly_bridge.run ~source ~destination
      ~recompute_normals:false source_geometry |> get_rdk in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" without_normals = None)
    "stale normal was retained";
  let shifted = Poly_bridge.run ~source ~destination ~keep_input:false
      ~connect_closest_ends:false ~pairing_shift:1 source_geometry |> get_rdk in
  let base_topology = Topology.Private.view (Geometry.topology bridge_only)
  and shifted_topology = Topology.Private.view (Geometry.topology shifted) in
  check (base_topology.vertex_points <> shifted_topology.vertex_points)
    "pairing shift did not change correspondence"

let test_divided_bridge_interpolation () =
  let geometry, source, destination = loop_pairs () in
  let output = Poly_bridge.run ~source ~destination ~divisions:3
      ~connect_closest_ends:false ~output_group:"bridge" geometry |> get_rdk in
  check (Geometry.point_count output = 16
      && Geometry.primitive_count output = 14
      && Geometry.vertex_count output = 56) "divided cardinality";
  let positions = Packed.Float3.Private.view (Geometry.positions output) in
  for point = 8 to 11 do
    check (abs_float (positions.y.(point) -. (1. /. 3.)) < 1e-12)
      "first interpolated row position"
  done;
  for point = 12 to 15 do
    check (abs_float (positions.y.(point) -. (2. /. 3.)) < 1e-12)
      "second interpolated row position"
  done;
  let height = float_attribute Attribute.Point "height" output in
  for point = 8 to 15 do
    check (abs_float (height.(point) -. positions.y.(point)) < 1e-12)
      "point float interpolation"
  done;
  let ids = int_attribute Attribute.Point "point_id" output
  and labels = text_attribute Attribute.Point "side" output in
  for point = 8 to 11 do
    check (ids.(point) < 4 && labels.(point) = "source")
      "first row nearest discrete payload"
  done;
  for point = 12 to 15 do
    check (ids.(point) >= 4 && labels.(point) = "destination")
      "second row nearest discrete payload"
  done;
  let corner_height = float_attribute Attribute.Vertex "corner_height" output in
  check (abs_float corner_height.(8) < 1e-12
      && abs_float corner_height.(9) < 1e-12
      && abs_float (corner_height.(10) -. (1. /. 3.)) < 1e-12
      && abs_float (corner_height.(11) -. (1. /. 3.)) < 1e-12)
    "vertex float interpolation";
  check (Geometry.find_group ~owner:Group.Point "source_points" output
      |> Option.get |> Group.cardinality = 8) "nearest point-group policy";
  check (Geometry.find_group ~owner:Group.Vertex "all_corners" output
      |> Option.get |> Group.cardinality = 56) "divided vertex group";
  check (Geometry.find_edge_group "source_edges" output
      |> Option.get |> Edge_group.cardinality = 4) "divided boundary ancestry";
  (match Poly_bridge.run ~source ~destination ~divisions:0 geometry with
   | Error error -> check (Error.code error = "invalid_topology")
       "division diagnostic"
   | Ok _ -> fail "zero divisions accepted");
  let unequal, source, destination =
    loop_pairs ~source_points:3 ~destination_points:5 () in
  (match Poly_bridge.run ~source ~destination ~divisions:2 unequal with
   | Error error -> check (Error.code error = "invalid_topology")
       "unequal divided diagnostic"
   | Ok _ -> fail "unequal divided bridge accepted")

let open_paths () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;2.; 0.;1.;2.|] ~y:[|0.;0.;0.;1.;1.;1.|]
      ~z:(Array.make 6 0.) in
  let topology = Topology.create_owned ~point_count:6
      ~vertex_points:[|0;1;2;3;4;5|] ~primitive_offsets:[|0;3;6|]
      ~primitive_kinds:[|Topology.Open_polyline;Topology.Open_polyline|] |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let index = Topology_index.create topology in
  let source = Edge_group.init ~grain:1 ~topology ~index ~name:"source"
      (fun edge -> let a, _ = Topology_index.edge_points index edge in a < 3)
  and destination = Edge_group.init ~grain:1 ~topology ~index ~name:"destination"
      (fun edge -> let a, _ = Topology_index.edge_points index edge in a >= 3) in
  geometry, source, destination

let test_open_unequal_and_errors () =
  let geometry, source, destination = open_paths () in
  let output = Poly_bridge.run ~source ~destination ~keep_input:false geometry
      |> get_rdk in
  check (Geometry.primitive_count output = 2
      && Geometry.vertex_count output = 8) "open bridge cardinality";
  (match Poly_bridge.run ~source ~destination ~pairing_shift:1 geometry with
   | Error error -> check (Error.code error = "invalid_topology")
       "open pairing-shift diagnostic"
   | Ok _ -> fail "open pairing shift accepted");
  (match Poly_bridge.run ~source ~destination:source geometry with
   | Error error -> check (Error.code error = "invalid_topology")
       "overlap diagnostic"
   | Ok _ -> fail "overlapping groups accepted");
  let _, foreign_source, _ = loop_pairs () in
  (match Poly_bridge.run ~source:foreign_source ~destination geometry with
   | Error error -> check (Error.code error = "invalid_topology")
       "foreign-affinity diagnostic"
   | Ok _ -> fail "foreign edge group accepted");
  let empty_destination = Edge_group.init ~grain:1
      ~topology:(Geometry.topology geometry)
      ~index:(Topology_index.create (Geometry.topology geometry)) ~name:"empty"
      (fun _ -> false) in
  (match Poly_bridge.run ~source ~destination:empty_destination geometry with
   | Error error -> check (Error.code error = "invalid_topology")
       "component-count diagnostic"
   | Ok _ -> fail "component-count mismatch accepted");
  let unequal_geometry, unequal_source, unequal_destination =
    loop_pairs ~source_points:3 ~destination_points:5 () in
  let unequal = Poly_bridge.run ~source:unequal_source
      ~destination:unequal_destination ~keep_input:false unequal_geometry
      |> get_rdk in
  check (Geometry.primitive_count unequal = 8
      && Geometry.vertex_count unequal = 24) "unequal zipper cardinality";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Poly_bridge.run ~cancel ~source ~destination geometry with
   | Error error -> check (Error.code error = "cancelled") "cancellation code"
   | Ok _ -> fail "cancelled bridge succeeded")

let test_branched_selection () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;0.;-1.; 0.;1.|] ~y:[|0.;0.;1.;0.; 2.;2.|]
      ~z:(Array.make 6 0.) in
  let topology = Topology.create_owned ~point_count:6
      ~vertex_points:[|0;1; 0;2; 0;3; 4;5|]
      ~primitive_offsets:[|0;2;4;6;8|]
      ~primitive_kinds:(Array.make 4 Topology.Open_polyline) |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let index = Topology_index.create topology in
  let source = Edge_group.init ~grain:1 ~topology ~index ~name:"branched"
      (fun edge -> let a, b = Topology_index.edge_points index edge in
        a = 0 || b = 0)
  and destination = Edge_group.init ~grain:1 ~topology ~index ~name:"target"
      (fun edge -> let a, b = Topology_index.edge_points index edge in
        a = 4 || b = 4) in
  match Poly_bridge.run ~source ~destination geometry with
  | Error error -> check (Error.code error = "invalid_topology")
      "branched selection diagnostic"
  | Ok _ -> fail "branched selection accepted"

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp = rp && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && int_attribute Attribute.Vertex "corner" left
      = int_attribute Attribute.Vertex "corner" right
  && float_attribute Attribute.Vertex "corner_height" left
      = float_attribute Attribute.Vertex "corner_height" right
  && int_attribute Attribute.Primitive "surface" left
      = int_attribute Attribute.Primitive "surface" right
  && float_attribute Attribute.Point "height" left
      = float_attribute Attribute.Point "height" right
  && float_attribute Attribute.Point "weight" left
      = float_attribute Attribute.Point "weight" right
  && int_attribute Attribute.Point "point_id" left
      = int_attribute Attribute.Point "point_id" right
  && text_attribute Attribute.Point "side" left
      = text_attribute Attribute.Point "side" right
  && (match Geometry.find_attribute ~owner:Attribute.Point "N" left,
      Geometry.find_attribute ~owner:Attribute.Point "N" right with
      | Some left, Some right ->
          (match Attribute.Private.storage left, Attribute.Private.storage right with
           | Attribute.Float3 left, Attribute.Float3 right ->
               Packed.Float3.Private.view left = Packed.Float3.Private.view right
           | _ -> false)
      | _ -> false)
  && List.for_all (fun name ->
    (Geometry.find_group ~owner:Group.Primitive name left
      |> Option.get |> Group.Private.bits_view)
    = (Geometry.find_group ~owner:Group.Primitive name right
      |> Option.get |> Group.Private.bits_view)) ["source_faces";"bridge"]
  && (Geometry.find_group ~owner:Group.Point "source_points" left
      |> Option.get |> Group.Private.bits_view)
     = (Geometry.find_group ~owner:Group.Point "source_points" right
        |> Option.get |> Group.Private.bits_view)
  && (Geometry.find_group ~owner:Group.Vertex "all_corners" left
      |> Option.get |> Group.Private.bits_view)
     = (Geometry.find_group ~owner:Group.Vertex "all_corners" right
        |> Option.get |> Group.Private.bits_view)
  && List.for_all (fun name ->
    let left = Geometry.find_edge_group name left |> Option.get
    and right = Geometry.find_edge_group name right |> Option.get in
    Edge_group.length left = Edge_group.length right
    && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
       = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right))
      ["source_edges";"destination_edges"]

let test_multiple_parallel_exactness () =
  let geometry, source, destination = loop_pairs ~pairs:128 ~source_points:129 () in
  let run domains = Parallel.run ~domains (fun () ->
      Poly_bridge.run ~grain:97 ~source ~destination
        ~pairing:Poly_bridge.Bridge_by_centroid ~divisions:4
        ~output_group:"bridge" geometry
      |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four) "one/four-domain bridge differs";
  check (Geometry.point_count one = (128 * 258) + (128 * 129 * 3)
      && Geometry.primitive_count one = (128 * 2) + (128 * 129 * 4)
      && Geometry.vertex_count one = (128 * 258) + (128 * 129 * 4 * 4))
    "multiple bridge cardinality"

let run () =
  test_closed_bridge_and_payload ();
  test_divided_bridge_interpolation ();
  test_open_unequal_and_errors ();
  test_branched_selection ();
  test_multiple_parallel_exactness ();
  print_endline "test_poly_bridge: ok"
