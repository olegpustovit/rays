open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let point_cloud values = Line_geometry.points values

let same_storage left right = match Attribute.Private.storage left,
    Attribute.Private.storage right with
  | Attribute.Float a, Attribute.Float b -> a = b
  | Attribute.Int a, Attribute.Int b -> a = b
  | Attribute.Text a, Attribute.Text b -> a = b
  | Attribute.Float2 a, Attribute.Float2 b ->
      let a = Packed.Float2.Private.view a and b = Packed.Float2.Private.view b in
      a.x = b.x && a.y = b.y
  | Attribute.Float3 a, Attribute.Float3 b ->
      let a = Packed.Float3.Private.view a and b = Packed.Float3.Private.view b in
      a.x = b.x && a.y = b.y && a.z = b.z
  | Attribute.Float4 a, Attribute.Float4 b ->
      let a = Packed.Float4.Private.view a and b = Packed.Float4.Private.view b in
      a.x = b.x && a.y = b.y && a.z = b.z && a.w = b.w
  | Attribute.Int_array a, Attribute.Int_array b ->
      let a = Packed.Int_array.Private.view a and b = Packed.Int_array.Private.view b in
      a.offsets = b.offsets && a.values = b.values
  | Attribute.Float_array a, Attribute.Float_array b ->
      let a = Packed.Float_array.Private.view a
      and b = Packed.Float_array.Private.view b in
      a.offsets = b.offsets && a.values = b.values
  | _ -> false

let same_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.length (Geometry.attributes left) = List.length (Geometry.attributes right)
  && List.for_all2 (fun a b -> Attribute.name a = Attribute.name b
      && Attribute.owner a = Attribute.owner b && same_storage a b)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.length (Geometry.groups left) = List.length (Geometry.groups right)
  && List.for_all2 (fun a b -> Group.name a = Group.name b
      && Group.owner a = Group.owner b
      && Group.ordered_elements a = Group.ordered_elements b
      && Array.for_all Fun.id (Array.init (Group.length a)
        (fun element -> Group.mem element a = Group.mem element b)))
      (Geometry.groups left) (Geometry.groups right)

let expect_error code = function
  | Error error -> check (Error.code error = code)
      (Printf.sprintf "expected %s, received %s" code (Error.to_string error))
  | Ok _ -> fail ("expected " ^ code ^ " error")

let check_closed geometry message =
  let index = Topology_index.create (Geometry.topology geometry) in
  check (Topology_index.boundary_edge_count index = 0
      && Topology_index.non_manifold_edge_count index = 0) message

let check_contains source hull message =
  let source_positions = Packed.Float3.Private.view (Geometry.positions source)
  and hull_positions = Packed.Float3.Private.view (Geometry.positions hull)
  and topology = Topology.Private.view (Geometry.topology hull) in
  for primitive = 0 to Geometry.primitive_count hull - 1 do
    let first = topology.primitive_offsets.(primitive) in
    check (topology.primitive_offsets.(primitive + 1) - first = 3)
      (message ^ ": non-triangle face");
    let a = topology.vertex_points.(first)
    and b = topology.vertex_points.(first + 1)
    and c = topology.vertex_points.(first + 2) in
    for point = 0 to Geometry.point_count source - 1 do
      let sign = Predicates.orient3d
          ~ax:hull_positions.x.(a) ~ay:hull_positions.y.(a)
          ~az:hull_positions.z.(a)
          ~bx:hull_positions.x.(b) ~by:hull_positions.y.(b)
          ~bz:hull_positions.z.(b)
          ~cx:hull_positions.x.(c) ~cy:hull_positions.y.(c)
          ~cz:hull_positions.z.(c)
          ~dx:source_positions.x.(point) ~dy:source_positions.y.(point)
          ~dz:source_positions.z.(point) in
      check (sign <> Predicates.Negative)
        (Printf.sprintf "%s: point %d is outside face %d" message point primitive)
    done
  done

let test_affine_dimensions () =
  let point = point_cloud [|2.,3.,4.; 2.,3.,4.; 2.,3.,4.|]
      |> Convex_hull.run ~source_point_attribute:"source" |> get_rdk in
  check (Geometry.point_count point = 1 && Geometry.vertex_count point = 0
      && Geometry.primitive_count point = 0) "singleton hull";
  let line = point_cloud [|2.,0.,0.; -1.,0.,0.; 5.,0.,0.; 0.,0.,0.; 5.,0.,0.|]
      |> Convex_hull.run |> get_rdk in
  let line_positions = Packed.Float3.Private.view (Geometry.positions line) in
  check (Geometry.point_count line = 2 && Geometry.vertex_count line = 2
      && Geometry.primitive_count line = 1
      && Topology.primitive_kind (Geometry.topology line) 0
          = Topology.Open_polyline
      && line_positions.x = [|-1.;5.|]) "collinear endpoint hull";
  let plane = point_cloud [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.;
      0.5,0.5,0.; 0.5,0.,0.; 1.,1.,0.|]
      |> Convex_hull.run |> get_rdk in
  check (Geometry.point_count plane = 4 && Geometry.vertex_count plane = 4
      && Geometry.primitive_count plane = 1) "coplanar polygon hull";
  check (Topology.primitive_kind (Geometry.topology plane) 0 = Topology.Polygon)
    "coplanar hull is not a polygon"

let decorated_cube () =
  let values = [|
    -1.,-1.,-1.; 1.,-1.,-1.; 1.,1.,-1.; -1.,1.,-1.;
    -1.,-1.,1.; 1.,-1.,1.; 1.,1.,1.; -1.,1.,1.;
    0.,0.,0.; 0.25,0.2,-0.1; -1.,-1.,-1.
  |] in
  let geometry = point_cloud values in
  let points = Array.length values in
  let id = Attribute.create_owned ~owner:Attribute.Point ~name:"id"
      (Attribute.Int (Array.init points (fun point -> 100 + point))) |> get
  and normal = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:(Array.make points 1.) ~y:(Array.make points 0.)
        ~z:(Array.make points 0.))) |> get
  and detail = Attribute.create_owned ~owner:Attribute.Detail ~name:"tag"
      (Attribute.Text [|"cube"|]) |> get
  and corners = Group.ordered ~owner:Group.Point ~name:"corners" ~length:points
      [|7;0;3;4|] |> get in
  Geometry.create ~positions:(Geometry.positions geometry)
    ~topology:(Geometry.topology geometry) ~attributes:[id;normal;detail]
    ~groups:[corners] () |> get

let test_solid_payload_and_selection () =
  let source = decorated_cube () in
  let hull = Convex_hull.run ~grain:1 ~source_point_attribute:"source_point"
      ~hull_group:"hull" source |> get_rdk in
  check (Geometry.point_count hull = 8 && Geometry.vertex_count hull = 36
      && Geometry.primitive_count hull = 12) "cube hull cardinality";
  check_closed hull "cube hull is not closed two-manifold";
  check_contains source hull "cube hull containment";
  check (Analysis.signed_volume hull |> get > 0.) "cube hull winding";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" hull = None)
    "stale point normal survived";
  check (Geometry.find_attribute ~owner:Attribute.Detail "tag" hull <> None)
    "detail payload was dropped";
  let ids = match Geometry.find_attribute ~owner:Attribute.Point "id" hull
      |> Option.get |> Attribute.Private.storage with
    | Attribute.Int values -> values | _ -> fail "id storage" in
  let ancestry = match Geometry.find_attribute ~owner:Attribute.Point
      "source_point" hull |> Option.get |> Attribute.Private.storage with
    | Attribute.Int values -> values | _ -> fail "ancestry storage" in
  check (ancestry = [|0;1;2;3;4;5;6;7|]
      && ids = Array.map (fun point -> 100 + point) ancestry)
    "source-point ancestry";
  check (Geometry.find_group ~owner:Group.Primitive "hull" hull
      |> Option.get |> Group.cardinality = 12) "hull output group";
  check (Geometry.find_group ~owner:Group.Point "corners" hull
      |> Option.get |> Group.ordered_elements = Some [|7;0;3;4|])
    "ordered point-group ancestry";
  let selected = Group.init ~grain:1 ~owner:Group.Point ~name:"bottom" 11
      (fun point -> point < 4) in
  let plane = Convex_hull.run ~selection:(Transform_ops.Selected_points selected) source
      |> get_rdk in
  check (Geometry.point_count plane = 4 && Geometry.primitive_count plane = 1)
    "typed selected hull";
  let stripped = Convex_hull.run ~preserve_point_payload:false source |> get_rdk in
  check (Geometry.find_attribute ~owner:Attribute.Point "id" stripped = None
      && Geometry.find_attribute ~owner:Attribute.Detail "tag" stripped <> None
      && Geometry.groups stripped = [])
    "payload suppression"

let test_exact_and_errors () =
  let tiny = Int64.float_of_bits 1L in
  let exact = point_cloud [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.; 0.,0.,tiny|]
      |> Convex_hull.run |> get_rdk in
  check (Geometry.point_count exact = 4 && Geometry.primitive_count exact = 4)
    "subnormal full-dimensional hull";
  check_closed exact "subnormal hull incidence";
  let empty = point_cloud [||] in
  expect_error "invalid_geometry" (Convex_hull.run empty);
  expect_error "invalid_geometry" (Convex_hull.run ~grain:0 exact);
  expect_error "invalid_geometry"
    (Convex_hull.run ~source_point_attribute:"P" exact);
  let bad = point_cloud [|0.,0.,0.; nan,0.,0.|] in
  expect_error "invalid_geometry" (Convex_hull.run bad);
  let wrong = Group.init ~grain:1 ~owner:Group.Point ~name:"wrong" 3
      (Fun.const true) in
  expect_error "invalid_geometry"
    (Convex_hull.run ~selection:(Transform_ops.Selected_points wrong) exact);
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect_error "cancelled" (Convex_hull.run ~cancel exact)

let test_randomized_containment () =
  for case = 0 to 31 do
    let points = Array.init 72 (fun point ->
      let coordinate multiplier offset =
        let value = ((point + (case * offset)) * multiplier + (case * 97))
            mod 10_007 in
        2. *. float_of_int value /. 10_007. -. 1. in
      coordinate 7919 17, coordinate 6841 29, coordinate 5503 43) in
    let source = point_cloud points in
    let hull = Convex_hull.run ~grain:7 source |> get_rdk in
    check_closed hull (Printf.sprintf "random hull %d incidence" case);
    check_contains source hull (Printf.sprintf "random hull %d containment" case);
    let index = Topology_index.create (Geometry.topology hull) in
    check (Geometry.point_count hull - Topology_index.edge_count index
        + Geometry.primitive_count hull = 2)
      (Printf.sprintf "random hull %d Euler characteristic" case)
  done

let sphere_cloud rings columns =
  let surface = rings * columns in
  Array.init (surface + 20_000) (fun point ->
    if point < surface then begin
      let ring = point / columns and column = point mod columns in
      let theta = Float.pi *. (float_of_int ring +. 0.5) /. float_of_int rings
      and phi = 2. *. Float.pi *. float_of_int column /. float_of_int columns in
      sin theta *. cos phi, cos theta, sin theta *. sin phi
    end else begin
      let value = point - surface in
      let x = float_of_int ((value * 17) mod 997) /. 997. -. 0.5
      and y = float_of_int ((value * 29) mod 991) /. 991. -. 0.5
      and z = float_of_int ((value * 43) mod 983) /. 983. -. 0.5 in
      x, y, z
    end)

let test_parallel_exact () =
  let source = point_cloud (sphere_cloud 16 32) in
  let run domains = Parallel.run ~domains (fun () ->
      Convex_hull.run ~grain:127 ~source_point_attribute:"source" source
      |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (same_geometry one four) "one/four-domain hull mismatch";
  check (Geometry.point_count one = 512
      && Geometry.primitive_count one = 1020) "sphere hull cardinality";
  check_closed one "sphere hull incidence"

let run () =
  test_affine_dimensions ();
  test_solid_payload_and_selection ();
  test_exact_and_errors ();
  test_randomized_containment ();
  test_parallel_exact ();
  print_endline "convex hull tests passed"
