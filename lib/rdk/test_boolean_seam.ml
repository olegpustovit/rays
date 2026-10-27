open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Seam = Rdk_boolean.Boolean_seam

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message
let close left right = abs_float (left -. right) <= 1e-11

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

let triangle points = geometry points [|0;1;2|]

let curve_geometry points primitives kinds =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let offsets = Array.make (Array.length primitives + 1) 0 in
  Array.iteri (fun primitive points ->
      offsets.(primitive + 1) <- offsets.(primitive) + Array.length points)
    primitives;
  let vertices = Array.make offsets.(Array.length primitives) 0 in
  Array.iteri (fun primitive points ->
      Array.blit points 0 vertices offsets.(primitive) (Array.length points))
    primitives;
  let topology = Topology.create_owned ~point_count:count ~vertex_points:vertices
      ~primitive_offsets:offsets ~primitive_kinds:kinds |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let pipeline ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false) left right =
  let constraints = Constraints.build ~resolve_left_self_intersections
      ~resolve_right_self_intersections ~grain:1 ~left ~right () |> get in
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get in
  Complex.build constraints refinement |> get

let seam ?resolve_left_self_intersections ?resolve_right_self_intersections
    left right =
  pipeline ?resolve_left_self_intersections ?resolve_right_self_intersections
    left right |> Seam.build |> get

let test_transverse_between_curve () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] in
  let complex = pipeline left right in
  let value = Seam.build complex |> get
  and expected = [|0.5,0.5,0.; 0.5,1.5,0.|] in
  check (Seam.Private.complex value == complex)
    "Boolean seam product did not retain its exact complex identity";
  let curves = Seam.curves value in
  check (Geometry.primitive_count curves = 1)
    "transverse intersection did not produce one seam curve";
  check (Geometry.point_count curves = 2)
    "transverse seam did not contain its two exact endpoints";
  check (Topology.primitive_kind (Geometry.topology curves) 0
      = Topology.Open_polyline)
    "transverse seam is not an open polyline";
  check (Seam.curve_kind value 0 = Seam.Between)
    "transverse operand seam has the wrong kind";
  check (Seam.curve_edge_range value 0 = (0, 1))
    "transverse curve lost complex-edge ancestry";
  check (Seam.Private.is_seam_edge value (Seam.curve_edge value 0))
    "transverse curve ancestry points at a non-seam complex edge";
  let positions = Packed.Float3.Private.view (Geometry.positions curves) in
  let actual = Array.init 2 (fun point ->
      positions.x.(point), positions.y.(point), positions.z.(point)) in
  Array.sort Stdlib.compare actual;
  Array.sort Stdlib.compare expected;
  Array.iteri (fun point (x, y, z) ->
      let ex, ey, ez = expected.(point) in
      check (close x ex && close y ey && close z ez)
        "transverse seam endpoint is wrong") actual;
  check (Geometry.primitive_count (Seam.coincident value) = 0)
    "transverse intersection produced coincident area"

let test_coincident_products () =
  let identical_left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and identical_right = triangle [|0.,2.,0.; 2.,0.,0.; 0.,0.,0.|] in
  let identical = seam identical_left identical_right in
  check (Geometry.primitive_count (Seam.curves identical) = 0)
    "fully coincident triangles produced a false boundary seam";
  check (Geometry.primitive_count (Seam.coincident identical) = 1)
    "fully coincident triangles were not emitted once";
  let partial_left = triangle [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and partial_right = triangle [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let partial = seam partial_left partial_right in
  check (Geometry.primitive_count (Seam.coincident partial) = 4)
    "partial coplanar overlap lost refined coincident facets";
  check (Geometry.primitive_count (Seam.curves partial) > 0)
    "partial coplanar overlap lost its area-boundary seam";
  for curve = 0 to Geometry.primitive_count (Seam.curves partial) - 1 do
    check (Seam.curve_kind partial curve = Seam.Between)
      "partial overlap boundary has a non-between kind"
  done

let far_triangle () =
  triangle [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|]

let transverse_pair () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
      0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
    [|0;1;2; 3;4;5|]

let test_self_kinds () =
  let left = seam ~resolve_left_self_intersections:true
      (transverse_pair ()) (far_triangle ()) in
  check (Geometry.primitive_count (Seam.curves left) = 1
      && Seam.curve_kind left 0 = Seam.Left_self)
    "left same-operand intersection did not produce a left-self seam";
  let right = seam ~resolve_right_self_intersections:true
      (far_triangle ()) (transverse_pair ()) in
  check (Geometry.primitive_count (Seam.curves right) = 1
      && Seam.curve_kind right 0 = Seam.Right_self)
    "right same-operand intersection did not produce a right-self seam"

let test_same_operand_coincidence () =
  let duplicate = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
      [|0;1;2; 3;4;5|] in
  let left = seam ~resolve_left_self_intersections:true
      duplicate (far_triangle ()) in
  check (Geometry.primitive_count (Seam.coincident left) = 1
      && Geometry.primitive_count (Seam.curves left) = 0)
    "left same-operand coincident facets were not emitted once";
  let right = seam ~resolve_right_self_intersections:true
      (far_triangle ()) duplicate in
  check (Geometry.primitive_count (Seam.coincident right) = 1
      && Geometry.primitive_count (Seam.curves right) = 0)
    "right same-operand coincident facets were not emitted once"

let test_native_nonmanifold_is_not_self_intersection () =
  let native = geometry
      [|0.,0.,0.; 2.,0.,0.; 1.,1.,0.; 1.,0.,1.; 1.,-1.,0.|]
      [|0;1;2; 1;0;3; 0;1;4|] in
  let value = seam ~resolve_left_self_intersections:true native (far_triangle ()) in
  check (Geometry.primitive_count (Seam.curves value) = 0)
    "ordinary source-native non-manifold adjacency became a self seam"

let tetra ~origin:(ox, oy, oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox,oy+.size,oz; ox,oy,oz+.size|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3|]

let test_closed_between_curve () =
  let value = seam
      (tetra ~origin:(0.,0.,0.) 2.)
      (tetra ~origin:(0.5,0.2,0.2) 2.) in
  let curves = Seam.curves value in
  check (Geometry.primitive_count curves = 1)
    "two convex solid boundaries did not chain into one intersection loop";
  check (Topology.primitive_kind (Geometry.topology curves) 0
      = Topology.Closed_polyline)
    "solid intersection seam did not close";
  check (Seam.curve_kind value 0 = Seam.Between)
    "closed solid intersection has the wrong seam kind";
  let first, last = Seam.curve_edge_range value 0 in
  check (last - first = Topology.primitive_size (Geometry.topology curves) 0)
    "closed seam edge ancestry cardinality differs from its curve cardinality"

let geometry_signature geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
  Array.copy topology.vertex_points, Array.copy topology.primitive_offsets,
  Bytes.copy topology.primitive_kinds

let seam_signature value =
  let curve_count = Geometry.primitive_count (Seam.curves value) in
  let edge_count = if curve_count = 0 then 0 else
      snd (Seam.curve_edge_range value (curve_count - 1)) in
  geometry_signature (Seam.curves value),
  geometry_signature (Seam.coincident value),
  Array.init curve_count (Seam.curve_kind value),
  Array.init (curve_count + 1) (fun curve ->
      if curve = curve_count then edge_count
      else fst (Seam.curve_edge_range value curve)),
  Array.concat (Array.to_list (Array.init curve_count (fun curve ->
      let first, last = Seam.curve_edge_range value curve in
      Array.init (last - first) (fun local -> Seam.curve_edge value (first + local)))))

let test_domain_exactness () =
  let left = triangle [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and right = triangle [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let run domains = Parallel.run ~domains (fun () ->
      pipeline left right
      |> Seam.build ~grain:1 ~parallel_cutoff:1 |> get |> seam_signature) in
  check (run 1 = run 4) "Boolean seam product differs between domain counts"

let test_cancellation () =
  let complex = pipeline
      (triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|])
      (triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]) in
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Seam.build ~cancel complex with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled Boolean seam extraction completed"

let test_invalid_grain () =
  let complex = pipeline
      (triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|])
      (triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]) in
  (match Seam.build ~grain:0 complex with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected grain error: %s" (Error.to_string error)
   | Ok _ -> fail "zero Boolean seam grain was accepted");
  match Seam.build ~parallel_cutoff:0 complex with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected cutoff error: %s" (Error.to_string error)
  | Ok _ -> fail "zero Boolean seam parallel cutoff was accepted"

let test_edge_classification_retention () =
  let complex = pipeline
      (triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|])
      (triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]) in
  let value = Seam.build complex |> get in
  for edge = 0 to Complex.edge_count complex - 1 do
    check (not (Seam.Private.is_seam_edge value edge))
      "disjoint source boundary was retained as a Boolean seam edge"
  done;
  let rejected edge =
    try ignore (Seam.Private.is_seam_edge value edge); false
    with Invalid_argument _ -> true in
  check (rejected (-1) && rejected (Complex.edge_count complex))
    "Boolean seam edge classification accepted an invalid edge index"

let test_post_rounding_verification () =
  let verify ?cancel geometry = Seam.Private.verify_curves ?cancel ~grain:1 geometry in
  let crossing = curve_geometry
      [|-1.,0.,0.; 1.,0.,0.; 0.,-1.,0.; 0.,1.,0.|]
      [|[|0;1|];[|2;3|]|]
      [|Topology.Open_polyline;Topology.Open_polyline|] in
  let crossing_error = match verify crossing with
    | Error error when Error.code error = "seam_self_intersection" ->
        Error.to_string error
    | Error error -> fail "unexpected seam verification error: %s"
        (Error.to_string error)
    | Ok () -> fail "crossing rounded seam curves passed verification" in
  check (String.contains crossing_error '0' && String.contains crossing_error '1')
    "seam crossing diagnostic lost stable curve IDs";
  let branch = curve_geometry
      [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.|]
      [|[|0;1|];[|0;2|]|]
      [|Topology.Open_polyline;Topology.Open_polyline|] in
  ignore (verify branch |> get);
  let overlap = curve_geometry
      [|0.,0.,0.; 1.,0.,0.; 2.,0.,0.|]
      [|[|0;2|];[|0;1|]|]
      [|Topology.Open_polyline;Topology.Open_polyline|] in
  (match verify overlap with
   | Error error when Error.code error = "seam_self_intersection" -> ()
   | Error error -> fail "unexpected overlap error: %s" (Error.to_string error)
   | Ok () -> fail "overlapping rounded seam segments passed verification");
  let bow_tie = curve_geometry
      [|-1.,-1.,0.;1.,1.,0.;-1.,1.,0.;1.,-1.,0.|]
      [|[|0;1;2;3|]|] [|Topology.Closed_polyline|] in
  (match verify bow_tie with
   | Error error when Error.code error = "seam_self_intersection" -> ()
   | Error error -> fail "unexpected closed-curve error: %s"
       (Error.to_string error)
   | Ok () -> fail "self-crossing closed seam passed verification");
  let skew = curve_geometry
      [|-1.,0.,0.; 1.,0.,0.; 0.,-1.,1.; 0.,1.,1.|]
      [|[|0;1|];[|2;3|]|]
      [|Topology.Open_polyline;Topology.Open_polyline|] in
  ignore (verify skew |> get);
  let tiny = Int64.float_of_bits 1L in
  let subnormal = curve_geometry [|0.,0.,0.; tiny,0.,0.|]
      [|[|0;1|]|] [|Topology.Open_polyline|] in
  ignore (verify subnormal |> get);
  (match verify (triangle [|0.,0.,0.;1.,0.,0.;0.,1.,0.|]) with
   | Error error when Error.code error = "invalid_output" -> ()
   | Error error -> fail "unexpected polygon verification error: %s"
       (Error.to_string error)
   | Ok () -> fail "polygon entered seam curve verification");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match verify ~cancel branch with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected seam verification cancellation: %s"
       (Error.to_string error)
   | Ok () -> fail "cancelled seam verification completed");
  let run domains = Parallel.run ~domains (fun () -> verify crossing) in
  check (Result.map_error Error.to_string (run 1)
      = Result.map_error Error.to_string (run 4))
    "seam verification diagnostic differs between domain counts"

let test_verification_scale () =
  let count = 4_096 in
  let points = Array.init (count * 2) (fun point ->
      let segment = point / 2 in
      float_of_int segment *. 4., float_of_int (point land 1), 0.)
  and primitives = Array.init count (fun segment ->
      [|segment * 2; (segment * 2) + 1|])
  and kinds = Array.make count Topology.Open_polyline in
  let curves = curve_geometry points primitives kinds in
  let run domains = Parallel.run ~domains (fun () ->
      Seam.Private.verify_curves ~grain:256 curves) in
  check (run 1 = Ok () && run 4 = Ok ())
    "scale seam verification failed or differs across domain counts"

let run () =
  test_transverse_between_curve ();
  test_coincident_products ();
  test_self_kinds ();
  test_same_operand_coincidence ();
  test_native_nonmanifold_is_not_self_intersection ();
  test_closed_between_curve ();
  test_domain_exactness ();
  test_cancellation ();
  test_invalid_grain ();
  test_edge_classification_retention ();
  test_post_rounding_verification ();
  test_verification_scale ()
