open Rdk

let fail message = prerr_endline ("test_skin: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let rings ?(sections = 2) ?(points = 4) () =
  let count = sections * points in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  for section = 0 to sections - 1 do
    for local = 0 to points - 1 do
      let point = (section * points) + local in
      let angle = 2. *. Float.pi *. float_of_int local /. float_of_int points in
      let radius = 1. +. (0.08 *. sin (float_of_int section *. 0.31)) in
      x.(point) <- radius *. cos angle;
      y.(point) <- float_of_int section *. 0.2;
      z.(point) <- radius *. sin angle
    done
  done;
  let topology = Topology.create_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (sections + 1) (fun value -> value * points))
      ~primitive_kinds:(Array.make sections Topology.Closed_polyline) |> get in
  let vertex = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int (Array.init count (fun value -> 100 + value))) |> get
  and primitive = Attribute.create_owned ~owner:Attribute.Primitive ~name:"section"
      (Attribute.Int (Array.init sections (fun value -> 10 + value))) |> get
  and corner_group = Group.init ~grain:1 ~owner:Group.Vertex ~name:"corners"
      count (fun _ -> true) in
  let geometry = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
      ~topology ~attributes:[vertex; primitive] ~groups:[corner_group] () |> get in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~grain:1 ~topology ~index ~name:"section_edges"
      (fun _ -> true) in
  Geometry.with_edge_group edges geometry |> get

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let test_quads_and_payload () =
  let source = rings () in
  let output = Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~connect_closest_ends:false ~output_group:"skin" source
      |> get_rdk in
  check (Geometry.point_count output = 8
      && Geometry.primitive_count output = 4
      && Geometry.vertex_count output = 16) "quad cardinality";
  let topology = Topology.Private.view (Geometry.topology output) in
  check (topology.vertex_points =
      [|0;1;5;4; 1;2;6;5; 2;3;7;6; 3;0;4;7|]) "quad ordering";
  check (topology.primitive_offsets = [|0;4;8;12;16|]) "quad offsets";
  check (int_attribute Attribute.Primitive "section" output = [|10;10;10;10|])
    "primitive ancestry";
  check (int_attribute Attribute.Vertex "corner" output =
      [|100;101;105;104; 101;102;106;105;
        102;103;107;106; 103;100;104;107|]) "vertex ancestry";
  check (Geometry.find_group ~owner:Group.Primitive "skin" output
      |> Option.get |> Group.cardinality = 4) "output group";
  check (Geometry.find_group ~owner:Group.Vertex "corners" output
      |> Option.get |> Group.cardinality = 16) "vertex-group ancestry";
  check (Geometry.find_edge_group "section_edges" output
      |> Option.get |> Edge_group.cardinality = 8) "native-edge ancestry";
  let kept = Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~connect_closest_ends:false ~keep_primitives:true source
      |> get_rdk in
  check (Geometry.primitive_count kept = 6 && Geometry.vertex_count kept = 24)
    "keep source curves";
  let index = Topology_index.create (Geometry.topology output) in
  check (Topology_index.boundary_edge_count index = 8
      && Topology_index.non_manifold_edge_count index = 0) "quad manifold strip"

let unequal_open () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;2.; 0.;0.6;1.4;2.|]
      ~y:[|0.;0.;0.; 1.;1.;1.;1.|] ~z:(Array.make 7 0.) in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2; 3;4;5;6|] ~primitive_offsets:[|0;3;7|]
      ~primitive_kinds:[|Topology.Open_polyline;Topology.Open_polyline|] |> get in
  Geometry.create ~positions ~topology () |> get

let test_unequal_wrap_and_errors () =
  let unequal = Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" (unequal_open ()) |> get_rdk in
  check (Geometry.primitive_count unequal = 5
      && Geometry.vertex_count unequal = 15) "unequal triangle fallback";
  let wrapped = Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~v_wrap:true (rings ~sections:3 ()) |> get_rdk in
  let index = Topology_index.create (Geometry.topology wrapped) in
  check (Geometry.primitive_count wrapped = 12
      && Geometry.vertex_count wrapped = 48
      && Topology_index.boundary_edge_count index = 0
      && Topology_index.non_manifold_edge_count index = 0) "V-wrapped skin";
  let source = rings () in
  let wrong = Group.init ~grain:1 ~owner:Group.Point ~name:"wrong" 8
      (fun _ -> true) in
  (match Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~primitives:wrong source with
   | Error error -> check (Error.code error = "invalid_topology")
       "selection-owner diagnostic"
   | Ok _ -> fail "point group accepted");
  (match Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~output_group:" " source with
   | Error error -> check (Error.code error = "invalid_topology")
       "empty output-group diagnostic"
   | Ok _ -> fail "empty output group accepted");
  (match Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~rest:(Line_geometry.points [|0.,0.,0.|]) source with
   | Error error -> check (Error.code error = "invalid_topology")
       "rest-cardinality diagnostic"
   | Ok _ -> fail "mismatched rest geometry accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~cancel source with
   | Error error -> check (Error.code error = "cancelled") "cancellation code"
   | Ok _ -> fail "cancelled skin succeeded")

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
  && int_attribute Attribute.Primitive "section" left
      = int_attribute Attribute.Primitive "section" right
  && (Geometry.find_group ~owner:Group.Vertex "corners" left
      |> Option.get |> Group.Private.bits_view)
     = (Geometry.find_group ~owner:Group.Vertex "corners" right
        |> Option.get |> Group.Private.bits_view)
  && (Geometry.find_group ~owner:Group.Primitive "skin" left
      |> Option.get |> Group.Private.bits_view)
     = (Geometry.find_group ~owner:Group.Primitive "skin" right
        |> Option.get |> Group.Private.bits_view)
  && let left = Geometry.find_edge_group "section_edges" left |> Option.get
     and right = Geometry.find_edge_group "section_edges" right |> Option.get in
     Edge_group.length left = Edge_group.length right
     && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
        = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right)

let test_parallel_exactness () =
  let source = rings ~sections:96 ~points:257 () in
  let run domains = Parallel.run ~domains (fun () ->
      Poly_loft.run ~output:Poly_loft.Polygons ~operation:"skin" ~grain:113 ~output_group:"skin" source |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four) "one/four-domain output differs";
  check (Geometry.primitive_count one = 95 * 257
      && Geometry.vertex_count one = 95 * 257 * 4) "large cardinality"

let run () =
  test_quads_and_payload ();
  test_unequal_wrap_and_errors ();
  test_parallel_exactness ();
  print_endline "test_skin: ok"
