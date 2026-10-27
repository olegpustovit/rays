open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
let approximate_point value pair point =
  Rdk_exact.Implicit_point.approximate (Coplanar.Private.point value pair point)

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message
let close left right = abs_float (left -. right) <= 1e-12

let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let triangle points = geometry points [|0; 1; 2|]

let arrange left right =
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let result = Coplanar.build ~grain:1 constraints |> get in
  check (Coplanar.pair_count result = 1) "expected one coplanar candidate pair";
  result

let sorted_points result =
  let points = Array.init (Coplanar.point_count result 0)
      (approximate_point result 0) in
  Array.sort Stdlib.compare points;
  points

let expect_points expected actual =
  let actual = sorted_points actual in
  check (Array.length actual = Array.length expected) "overlap point count differs";
  Array.sort Stdlib.compare expected;
  Array.iteri (fun index (expected_x, expected_y, expected_z) ->
    let actual_x, actual_y, actual_z = actual.(index) in
    if not (close expected_x actual_x && close expected_y actual_y
        && close expected_z actual_z) then
      fail "point %d: expected (%g,%g,%g), got (%.17g,%.17g,%.17g)"
        index expected_x expected_y expected_z actual_x actual_y actual_z) expected

let left_triangle () = triangle
    [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]

let downward_triangle () = triangle
    [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|]

let test_six_edge_overlap () =
  let result = arrange (left_triangle ()) (downward_triangle ()) in
  check (Coplanar.kind result 0 = Coplanar.Polygon) "six-edge overlap is not a polygon";
  check (Coplanar.boundary_count result 0 = 6) "six-edge boundary was incomplete";
  expect_points [|
      0.75,1.5,0.; 1.5,0.,0.; 1.5,3.,0.;
      2.5,0.,0.; 2.5,3.,0.; 3.25,1.5,0.;
    |] result;
  let area = ref 0. in
  for edge = 0 to Coplanar.boundary_count result 0 - 1 do
    let first = Coplanar.boundary_first result 0 edge
    and second = Coplanar.boundary_second result 0 edge in
    let ax, ay, _ = approximate_point result 0 first
    and bx, by, _ = approximate_point result 0 second in
    area := !area +. ((ax *. by) -. (ay *. bx))
  done;
  check (!area > 0.) "coplanar polygon boundary is not projected CCW"

let test_containment_and_reversed_identity () =
  let inside = triangle [|1.,1.,0.; 2.,1.,0.; 1.,2.,0.|] in
  let contained = arrange (left_triangle ()) inside in
  check (Coplanar.kind contained 0 = Coplanar.Polygon
      && Coplanar.point_count contained 0 = 3) "contained triangle was not retained";
  expect_points [|1.,1.,0.; 2.,1.,0.; 1.,2.,0.|] contained;
  let reversed = triangle [|2.,4.,0.; 4.,0.,0.; 0.,0.,0.|] in
  let identical = arrange (left_triangle ()) reversed in
  check (Coplanar.kind identical 0 = Coplanar.Polygon
      && Coplanar.point_count identical 0 = 3)
    "opposite-winding coincident triangles were not merged";
  expect_points [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|] identical

let test_lower_dimensional_contacts () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] in
  let edge = triangle [|0.,0.,0.; 2.,0.,0.; 1.,-1.,0.|] in
  let segment = arrange left edge in
  check (Coplanar.kind segment 0 = Coplanar.Segment
      && Coplanar.point_count segment 0 = 2
      && Coplanar.boundary_count segment 0 = 1)
    "coincident edge contact was not a segment";
  expect_points [|0.,0.,0.; 2.,0.,0.|] segment;
  let tip = triangle [|2.,0.,0.; 3.,-1.,0.; 3.,1.,0.|] in
  let point = arrange left tip in
  check (Coplanar.kind point 0 = Coplanar.Point
      && Coplanar.point_count point 0 = 1
      && Coplanar.boundary_count point 0 = 0)
    "vertex contact was not a point";
  expect_points [|2.,0.,0.|] point

let test_aabb_overlap_without_triangle_overlap () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle [|1.5,1.5,0.; 3.,1.5,0.; 1.5,3.,0.|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let result = Coplanar.build ~grain:1 constraints |> get in
  check (Constraints.candidate_pair_count constraints = 0
      && Coplanar.pair_count result = 0)
    "diagonally separated coplanar triangles survived K-DOP broad phase"

let test_same_operand_coplanar_and_topology_suppression () =
  let left = geometry
      [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.;
        1.,1.,0.; 2.,1.,0.; 1.,2.,0.|]
      [|0;1;2; 3;4;5|]
  and far = triangle [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] in
  let constraints = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left ~right:far () |> get in
  let result = Coplanar.build ~grain:1 constraints |> get in
  check (Coplanar.pair_count result = 1
      && Coplanar.first_side result 0 = Constraints.Left
      && Coplanar.second_side result 0 = Constraints.Left
      && Coplanar.kind result 0 = Coplanar.Polygon)
    "same-operand coplanar overlap was not arranged as a polygon";
  expect_points [|1.,1.,0.; 2.,1.,0.; 1.,2.,0.|] result;
  let adjacent = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 2.,2.,0.|]
      [|0;1;2; 1;3;2|] in
  let constraints = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left:adjacent ~right:far () |> get in
  let result = Coplanar.build ~grain:1 constraints |> get in
  check (Coplanar.pair_count result = 0)
    "ordinary shared source edge was treated as a self-overlap";
  let overlapping_edge = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.5,0.5,0.|]
      [|0;1;2; 0;1;3|] in
  let constraints = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left:overlapping_edge ~right:far () |> get in
  let result = Coplanar.build ~grain:1 constraints |> get in
  check (Coplanar.pair_count result = 1
      && Coplanar.kind result 0 = Coplanar.Polygon)
    "same-side shared-edge area overlap was suppressed as ordinary topology"

let repeated count points =
  let output_points = Array.make (count * 3) (0.,0.,0.)
  and triangles = Array.make (count * 3) 0 in
  for face = 0 to count - 1 do
    let offset = float_of_int (face * 10) in
    for local = 0 to 2 do
      let x, y, z = points.(local) in
      output_points.((face * 3) + local) <- x +. offset, y, z;
      triangles.((face * 3) + local) <- (face * 3) + local
    done
  done;
  geometry output_points triangles

let signature result =
  Array.init (Coplanar.pair_count result) (fun pair ->
    Coplanar.left_triangle result pair,
    Coplanar.right_triangle result pair,
    Coplanar.kind result pair,
    Array.init (Coplanar.point_count result pair)
      (approximate_point result pair))

let test_domain_exactness () =
  let left = repeated 257 [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and right = repeated 257 [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let run domains = Parallel.run ~domains (fun () ->
      let constraints = Constraints.build ~grain:19 ~left ~right () |> get in
      Coplanar.build ~grain:17 constraints |> get |> signature) in
  let one = run 1 and four = run 4 in
  if one <> four then fail "coplanar arrangements differ between one and four domains"

let test_invalid_grain_and_cancellation () =
  let constraints = Constraints.build ~grain:1 ~left:(left_triangle ())
      ~right:(downward_triangle ()) () |> get in
  (match Coplanar.build ~grain:0 constraints with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected invalid-grain error: %s" (Error.to_string error)
   | Ok _ -> fail "zero coplanar grain was accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Coplanar.build ~cancel ~grain:1 constraints with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled coplanar arrangement completed"

let run () =
  test_six_edge_overlap ();
  test_containment_and_reversed_identity ();
  test_lower_dimensional_contacts ();
  test_aabb_overlap_without_triangle_overlap ();
  test_same_operand_coplanar_and_topology_suppression ();
  test_domain_exactness ();
  test_invalid_grain_and_cancellation ()
