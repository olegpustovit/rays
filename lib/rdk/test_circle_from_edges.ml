open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let geometry_owned ?(attributes = []) ?(groups = []) ?(edge_groups = [])
    ~kinds points vertex_points primitive_offsets =
  let points = Array.of_list points and point_count = List.length points in
  let x = Array.map (fun (x, _, _) -> x) points
  and y = Array.map (fun (_, y, _) -> y) points
  and z = Array.map (fun (_, _, z) -> z) points in
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets ~primitive_kinds:kinds |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology ~attributes ~groups ~edge_groups () |> get_string

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let closed_loops count vertices_per_loop =
  let point_count = count * vertices_per_loop in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for component = 0 to count - 1 do
    let cx = float_of_int (component mod 250) *. 4.5
    and cy = float_of_int (component / 250) *. 4.5 in
    for local = 0 to vertices_per_loop - 1 do
      let point = (component * vertices_per_loop) + local in
      let angle = 2. *. Float.pi *. float_of_int local
          /. float_of_int vertices_per_loop in
      let radius = 1. +. (0.18 *. sin (3. *. angle)) in
      x.(point) <- cx +. (radius *. cos angle);
      y.(point) <- cy +. (radius *. sin angle);
      z.(point) <- 0.08 *. cos (2. *. angle)
    done
  done;
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1)
        (fun primitive -> primitive * vertices_per_loop))
      ~primitive_kinds:(Array.make count Topology.Closed_polyline)
      |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && Attribute.storage left = Attribute.storage right

let equal_group left right =
  Group.owner left = Group.owner right && Group.name left = Group.name right
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && Array.for_all Fun.id (Array.init (Group.length left) (fun element ->
    Group.mem element left = Group.mem element right))

let equal_edge_group left right =
  Edge_group.name left = Edge_group.name right
  && Edge_group.length left = Edge_group.length right
  && Array.for_all Fun.id (Array.init (Edge_group.length left) (fun edge ->
    Edge_group.mem edge left = Edge_group.mem edge right))

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && lt.primitive_kinds = rt.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
       (Geometry.edge_groups right)

let expect_code code work message = match work () with
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "%s: expected %s, received %s"
      message code (Error.to_string error))
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let circumcenter_xy geometry a b c =
  let p = positions geometry in
  let ax = p.x.(a) and ay = p.y.(a)
  and bx = p.x.(b) and by = p.y.(b)
  and cx = p.x.(c) and cy = p.y.(c) in
  let d = 2. *. ((ax *. (by -. cy)) +. (bx *. (cy -. ay))
      +. (cx *. (ay -. by))) in
  if abs_float d < 1e-15 then fail "test circumcenter is degenerate";
  let aa = (ax *. ax) +. (ay *. ay)
  and bb = (bx *. bx) +. (by *. by)
  and cc = (cx *. cx) +. (cy *. cy) in
  ((aa *. (by -. cy)) +. (bb *. (cy -. ay)) +. (cc *. (ay -. by))) /. d,
  ((aa *. (cx -. bx)) +. (bb *. (ax -. cx)) +. (cc *. (bx -. ax))) /. d

let test_fit_radius_and_scale () =
  let source = Line_geometry.polyline ~closed:true
      [|-2.,-0.2,0.; 0.,-1.,0.; 1.,0.1,0.; 0.3,2.,0.|] |> get in
  let fitted = Circle_from_edges.run source |> get in
  let p = positions fitted and cx, cy = circumcenter_xy fitted 0 1 2 in
  let radius point = sqrt (((p.x.(point) -. cx) ** 2.)
      +. ((p.y.(point) -. cy) ** 2.)) in
  let r = radius 0 in
  check (Array.for_all Fun.id (Array.init 4 (fun point ->
      near ~epsilon:2e-10 (radius point) r && near p.z.(point) 0.)))
    "Circle from Edges did not create one fitted planar radius";

  let square = Line_geometry.polyline ~closed:true
      [|-1.,-1.,0.; 1.,-1.,0.; 1.,1.,0.; -1.,1.,0.|] |> get in
  let scaled = Circle_from_edges.run ~radius:2.
      ~scale:(Rays_math.Vec3.create 1. 0.5 1.) square |> get in
  let p = positions scaled and root2 = sqrt 2. in
  check (near p.x.(0) (-.root2) && near p.y.(0) (-.root2 *. 0.5)
      && near p.x.(2) root2 && near p.y.(2) (root2 *. 0.5))
    "Circle from Edges explicit radius/scale"

let test_best_fit_plane () =
  let source = Line_geometry.polyline ~closed:true
      [|-1.,-1.,0.1; 1.,-1.,1.9; 1.,1.,2.2; -1.,1.,0.2;
        -1.4,0.,-0.4|] |> get in
  let output = Circle_from_edges.run source |> get in
  let p = positions output in
  let ax = p.x.(1) -. p.x.(0) and ay = p.y.(1) -. p.y.(0)
  and az = p.z.(1) -. p.z.(0)
  and bx = p.x.(2) -. p.x.(0) and by = p.y.(2) -. p.y.(0)
  and bz = p.z.(2) -. p.z.(0) in
  let nx = (ay *. bz) -. (az *. by)
  and ny = (az *. bx) -. (ax *. bz)
  and nz = (ax *. by) -. (ay *. bx) in
  let length = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
  check (length > 1e-8) "Circle from Edges produced a degenerate plane";
  for point = 3 to 4 do
    let dx = p.x.(point) -. p.x.(0) and dy = p.y.(point) -. p.y.(0)
    and dz = p.z.(point) -. p.z.(0) in
    check (abs_float ((dx *. nx) +. (dy *. ny) +. (dz *. nz))
        <= 2e-10 *. length)
      "Circle from Edges best-fit projection is not planar"
  done

let test_selection_boundary_payload () =
  let base = geometry_owned
      ~kinds:[|Topology.Closed_polyline;Topology.Closed_polyline|]
      [-2.,-1.,0.;0.,-2.,0.;2.,-1.,0.;0.,1.4,0.;
       8.,-1.,0.;10.,-1.,0.;10.,1.,0.;8.,1.,0.]
      [|0;1;2;3;4;5;6;7|] [|0;4;8|] in
  let topology = Geometry.topology base in
  let selected = edge_group_of_pairs topology "first_loop"
      [|0,1;1,2;2,3;3,0|]
  and retained = edge_group_of_pairs topology "retained" [|4,5|] in
  let ordered = Group.ordered ~owner:Group.Point ~name:"picked" ~length:8
      [|7;0;3|] |> get_string in
  let point_id = attribute Attribute.Point "id"
      (Attribute.Int (Array.init 8 Fun.id))
  and point_n = attribute Attribute.Point "N" (Attribute.Float3
      (Packed.Float3.Private.of_owned_exn ~x:(Array.make 8 0.)
        ~y:(Array.make 8 0.) ~z:(Array.make 8 1.)))
  and vertex_n = attribute Attribute.Vertex "N" (Attribute.Float3
      (Packed.Float3.Private.of_owned_exn ~x:(Array.make 8 0.)
        ~y:(Array.make 8 0.) ~z:(Array.make 8 1.))) in
  let source = Geometry.create ~positions:(Geometry.positions base) ~topology
      ~attributes:[point_id;point_n;vertex_n] ~groups:[ordered]
      ~edge_groups:[selected;retained] () |> get_string in
  let before = positions source in
  let output = Circle_from_edges.run ~grain:1 ~edges:selected
      ~output_group:"circle_edges" source |> get in
  let after = positions output in
  check (Geometry.topology output == topology
      && Geometry.find_attribute ~owner:Attribute.Point "id" output
         = Some point_id
      && Geometry.find_group ~owner:Group.Point "picked" output = Some ordered
      && Geometry.find_edge_group "retained" output = Some retained)
    "Circle from Edges did not structurally share untouched payload";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Circle from Edges retained stale normals";
  for point = 4 to 7 do
    check (after.x.(point) = before.x.(point)
        && after.y.(point) = before.y.(point)
        && after.z.(point) = before.z.(point))
      "Circle from Edges changed an unselected point"
  done;
  check ((Geometry.find_edge_group "circle_edges" output |> Option.get
          |> Edge_group.cardinality) = 4)
    "Circle from Edges output edge group cardinality";

  let empty = edge_group_of_pairs topology "empty" [||] in
  check (Circle_from_edges.run ~edges:empty source |> get == source)
    "empty Circle from Edges was not an identity";
  let empty_output = Circle_from_edges.run ~edges:empty
      ~output_group:"none" source |> get in
  check ((Geometry.find_edge_group "none" empty_output |> Option.get
          |> Edge_group.cardinality) = 0)
    "empty Circle from Edges omitted its requested group"

let test_default_boundary () =
  let points =
    [-1.,-1.,0.;0.,-1.,0.;1.,-1.,0.;
     -1.,0.,0.;0.,0.,0.;1.,0.,0.;
     -1.,1.,0.;0.,1.,0.;1.,1.,0.] in
  let source = geometry_owned ~kinds:(Array.make 4 Topology.Polygon)
      points [|0;1;4;3; 1;2;5;4; 3;4;7;6; 4;5;8;7|]
      [|0;4;8;12;16|] in
  let output = Circle_from_edges.run ~output_group:"boundary" source |> get in
  let p = positions output in
  check (p.x.(4) = 0. && p.y.(4) = 0. && p.z.(4) = 0.)
    "Circle from Edges moved an interior point";
  check ((Geometry.find_edge_group "boundary" output |> Option.get
          |> Edge_group.cardinality) = 8)
    "Circle from Edges did not select the topology boundary"

let test_validation_and_atomicity () =
  let source = Line_geometry.polyline ~closed:true
      [|-1.,-1.,0.;1.,-1.,0.;1.,1.,0.;-1.,1.,0.|] |> get in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run ~grain:0 source)
    "zero grain";
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run ~radius:0. source)
    "zero radius";
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run ~radius:nan source)
    "non-finite radius";
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run
      ~scale:(Rays_math.Vec3.create nan 1. 1.) source) "non-finite scale";
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run
      ~output_group:" " source) "empty output group";
  let other = Line_geometry.polyline ~closed:true
      [|0.,0.,0.;1.,0.,0.;0.,1.,0.|] |> get in
  let foreign = edge_group_of_pairs (Geometry.topology other) "foreign"
      [|0,1;1,2;2,0|] in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run
      ~edges:foreign source) "foreign edge group";
  let short = Line_geometry.polyline [|0.,0.,0.;1.,0.,0.|] |> get in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run
      ~edges:(edge_group_of_pairs (Geometry.topology short) "short" [|0,1|])
      short) "component with fewer than three points";
  let collinear = Line_geometry.polyline [|0.,0.,0.;1.,0.,0.;2.,0.,0.|] |> get in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run collinear)
    "collinear component";
  let branch = geometry_owned ~kinds:(Array.make 3 Topology.Open_polyline)
      [0.,0.,0.;1.,0.,0.;0.,1.,0.;-1.,0.,0.]
      [|0;1;0;2;0;3|] [|0;2;4;6|] in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run branch)
    "branched component";
  let nonfinite = geometry_owned ~kinds:[|Topology.Closed_polyline|]
      [0.,0.,0.;nan,1.,0.;1.,1.,0.;1.,0.,0.]
      [|0;1;2;3|] [|0;4|] in
  expect_code "invalid_circle" (fun () -> Circle_from_edges.run nonfinite)
    "non-finite selected endpoint";
  let overflow = Circle_from_edges.run ~radius:max_float
      ~scale:(Rays_math.Vec3.create max_float max_float max_float) source in
  expect_code "invalid_circle" (fun () -> overflow) "unrepresentable output";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (fun () -> Circle_from_edges.run
      ~cancel:cancelled source) "cancellation"

let test_parallel_exact () =
  let source = closed_loops 10_000 16 in
  let run domains = Parallel.run ~domains (fun () ->
    Circle_from_edges.run ~grain:257 ~output_group:"circles" source |> get) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "Circle from Edges differs across one and four domains";
  check (Geometry.point_count one = 160_000
      && Geometry.vertex_count one = 160_000
      && Geometry.primitive_count one = 10_000
      && (Geometry.find_edge_group "circles" one |> Option.get
          |> Edge_group.cardinality) = 160_000)
    "Circle from Edges scale cardinality"

let run () =
  test_fit_radius_and_scale ();
  test_best_fit_plane ();
  test_selection_boundary_payload ();
  test_default_boundary ();
  test_validation_and_atomicity ();
  test_parallel_exact ();
  print_endline "circle from edges tests passed"
