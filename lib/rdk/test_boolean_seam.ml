open Rdk
open Rdk_test_support
module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Seam = Rdk_boolean.Boolean_seam

let close left right = abs_float (left -. right) <= 1e-11

let geometry points triangles =
  let count = Array.length points in
  let x =
    Array.init count (fun point ->
        let x, _, _ = points.(point) in
        x)
  and y =
    Array.init count (fun point ->
        let _, y, _ = points.(point) in
        y)
  and z =
    Array.init count (fun point ->
        let _, _, z = points.(point) in
        z)
  in
  let topology =
    Topology.polygons_owned ~point_count:count ~vertex_points:triangles
      ~primitive_offsets:
        (Array.init ((Array.length triangles / 3) + 1) (fun primitive -> primitive * 3))
    |> get_string
  in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z) ~topology () |> get_string

let triangle points = geometry points [| 0; 1; 2 |]

let curve_geometry points primitives kinds =
  let count = Array.length points in
  let x =
    Array.init count (fun point ->
        let x, _, _ = points.(point) in
        x)
  and y =
    Array.init count (fun point ->
        let _, y, _ = points.(point) in
        y)
  and z =
    Array.init count (fun point ->
        let _, _, z = points.(point) in
        z)
  in
  let offsets = Array.make (Array.length primitives + 1) 0 in
  Array.iteri
    (fun primitive points -> offsets.(primitive + 1) <- offsets.(primitive) + Array.length points)
    primitives;
  let vertices = Array.make offsets.(Array.length primitives) 0 in
  Array.iteri
    (fun primitive points -> Array.blit points 0 vertices offsets.(primitive) (Array.length points))
    primitives;
  let topology =
    Topology.create_owned ~point_count:count ~vertex_points:vertices ~primitive_offsets:offsets
      ~primitive_kinds:kinds
    |> get_string
  in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z) ~topology () |> get_string

let pipeline ?(resolve_left_self_intersections = false) ?(resolve_right_self_intersections = false)
    left right =
  let constraints =
    Constraints.build ~resolve_left_self_intersections ~resolve_right_self_intersections ~grain:1
      ~left ~right ()
    |> get_rdk
  in
  let coplanar = Coplanar.build ~grain:1 constraints |> get_rdk in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get_rdk in
  Complex.build constraints refinement |> get_rdk

let seam ?resolve_left_self_intersections ?resolve_right_self_intersections left right =
  pipeline ?resolve_left_self_intersections ?resolve_right_self_intersections left right
  |> Seam.build |> get_rdk

let test_coincident_products () =
  let identical_left = triangle [| (0., 0., 0.); (2., 0., 0.); (0., 2., 0.) |]
  and identical_right = triangle [| (0., 2., 0.); (2., 0., 0.); (0., 0., 0.) |] in
  let identical = seam identical_left identical_right in
  check
    (Geometry.primitive_count (Seam.curves identical) = 0)
    "fully coincident triangles produced a false boundary seam";
  check
    (Geometry.primitive_count (Seam.coincident identical) = 1)
    "fully coincident triangles were not emitted once";
  let partial_left = triangle [| (0., 0., 0.); (4., 0., 0.); (2., 4., 0.) |]
  and partial_right = triangle [| (0., 3., 0.); (4., 3., 0.); (2., -1., 0.) |] in
  let partial = seam partial_left partial_right in
  check
    (Geometry.primitive_count (Seam.coincident partial) = 4)
    "partial coplanar overlap lost refined coincident facets";
  check
    (Geometry.primitive_count (Seam.curves partial) > 0)
    "partial coplanar overlap lost its area-boundary seam";
  for curve = 0 to Geometry.primitive_count (Seam.curves partial) - 1 do
    check
      (Seam.curve_kind partial curve = Seam.Between)
      "partial overlap boundary has a non-between kind"
  done

let far_triangle () = triangle [| (10., 10., 0.); (11., 10., 0.); (10., 11., 0.) |]

let transverse_pair () =
  geometry
    [|
      (0., 0., 0.); (2., 0., 0.); (0., 2., 0.); (0.5, -0.5, -1.); (0.5, 1.5, 1.); (0.5, 1.5, -1.);
    |]
    [| 0; 1; 2; 3; 4; 5 |]

let test_self_kinds () =
  let left = seam ~resolve_left_self_intersections:true (transverse_pair ()) (far_triangle ()) in
  check
    (Geometry.primitive_count (Seam.curves left) = 1 && Seam.curve_kind left 0 = Seam.Left_self)
    "left same-operand intersection did not produce a left-self seam";
  let right = seam ~resolve_right_self_intersections:true (far_triangle ()) (transverse_pair ()) in
  check
    (Geometry.primitive_count (Seam.curves right) = 1 && Seam.curve_kind right 0 = Seam.Right_self)
    "right same-operand intersection did not produce a right-self seam"

let test_same_operand_coincidence () =
  let duplicate =
    geometry
      [| (0., 0., 0.); (2., 0., 0.); (0., 2., 0.); (0., 0., 0.); (2., 0., 0.); (0., 2., 0.) |]
      [| 0; 1; 2; 3; 4; 5 |]
  in
  let left = seam ~resolve_left_self_intersections:true duplicate (far_triangle ()) in
  check
    (Geometry.primitive_count (Seam.coincident left) = 1
    && Geometry.primitive_count (Seam.curves left) = 0)
    "left same-operand coincident facets were not emitted once";
  let right = seam ~resolve_right_self_intersections:true (far_triangle ()) duplicate in
  check
    (Geometry.primitive_count (Seam.coincident right) = 1
    && Geometry.primitive_count (Seam.curves right) = 0)
    "right same-operand coincident facets were not emitted once"

let test_native_nonmanifold_is_not_self_intersection () =
  let native =
    geometry
      [| (0., 0., 0.); (2., 0., 0.); (1., 1., 0.); (1., 0., 1.); (1., -1., 0.) |]
      [| 0; 1; 2; 1; 0; 3; 0; 1; 4 |]
  in
  let value = seam ~resolve_left_self_intersections:true native (far_triangle ()) in
  check
    (Geometry.primitive_count (Seam.curves value) = 0)
    "ordinary source-native non-manifold adjacency became a self seam"

let tetra ~origin:(ox, oy, oz) size =
  geometry
    [| (ox, oy, oz); (ox +. size, oy, oz); (ox, oy +. size, oz); (ox, oy, oz +. size) |]
    [| 0; 2; 1; 0; 1; 3; 1; 2; 3; 2; 0; 3 |]

let geometry_signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  ( Array.copy positions.x,
    Array.copy positions.y,
    Array.copy positions.z,
    Array.copy topology.vertex_points,
    Array.copy topology.primitive_offsets,
    Bytes.copy topology.primitive_kinds )

let test_cancellation () =
  let complex =
    pipeline
      (triangle [| (0., 0., 0.); (2., 0., 0.); (0., 2., 0.) |])
      (triangle [| (10., 0., 0.); (12., 0., 0.); (10., 2., 0.) |])
  in
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Seam.build ~cancel complex with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled Boolean seam extraction completed"

let test_invalid_grain () =
  let complex =
    pipeline
      (triangle [| (0., 0., 0.); (2., 0., 0.); (0., 2., 0.) |])
      (triangle [| (10., 0., 0.); (12., 0., 0.); (10., 2., 0.) |])
  in
  (match Seam.build ~grain:0 complex with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> failf "unexpected grain error: %s" (Error.to_string error)
  | Ok _ -> failf "zero Boolean seam grain was accepted");
  match Seam.build ~parallel_cutoff:0 complex with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> failf "unexpected cutoff error: %s" (Error.to_string error)
  | Ok _ -> failf "zero Boolean seam parallel cutoff was accepted"

let test_edge_classification_retention () =
  let complex =
    pipeline
      (triangle [| (0., 0., 0.); (2., 0., 0.); (0., 2., 0.) |])
      (triangle [| (10., 0., 0.); (12., 0., 0.); (10., 2., 0.) |])
  in
  let value = Seam.build complex |> get_rdk in
  for edge = 0 to Complex.edge_count complex - 1 do
    check
      (not (Seam.View.is_seam_edge value edge))
      "disjoint source boundary was retained as a Boolean seam edge"
  done;
  let rejected edge =
    try
      ignore (Seam.View.is_seam_edge value edge);
      false
    with Invalid_argument _ -> true
  in
  check
    (rejected (-1) && rejected (Complex.edge_count complex))
    "Boolean seam edge classification accepted an invalid edge index"

let test_post_rounding_verification () =
  let verify ?cancel geometry = Seam.View.verify_curves ?cancel ~grain:1 geometry in
  let crossing =
    curve_geometry
      [| (-1., 0., 0.); (1., 0., 0.); (0., -1., 0.); (0., 1., 0.) |]
      [| [| 0; 1 |]; [| 2; 3 |] |]
      [| Topology.Open_polyline; Topology.Open_polyline |]
  in
  let crossing_error =
    match verify crossing with
    | Error error when Error.code error = "seam_self_intersection" -> Error.to_string error
    | Error error -> failf "unexpected seam verification error: %s" (Error.to_string error)
    | Ok () -> failf "crossing rounded seam curves passed verification"
  in
  check
    (String.contains crossing_error '0' && String.contains crossing_error '1')
    "seam crossing diagnostic lost stable curve IDs";
  let branch =
    curve_geometry
      [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |]
      [| [| 0; 1 |]; [| 0; 2 |] |]
      [| Topology.Open_polyline; Topology.Open_polyline |]
  in
  ignore (verify branch |> get_rdk);
  let overlap =
    curve_geometry
      [| (0., 0., 0.); (1., 0., 0.); (2., 0., 0.) |]
      [| [| 0; 2 |]; [| 0; 1 |] |]
      [| Topology.Open_polyline; Topology.Open_polyline |]
  in
  (match verify overlap with
  | Error error when Error.code error = "seam_self_intersection" -> ()
  | Error error -> failf "unexpected overlap error: %s" (Error.to_string error)
  | Ok () -> failf "overlapping rounded seam segments passed verification");
  let bow_tie =
    curve_geometry
      [| (-1., -1., 0.); (1., 1., 0.); (-1., 1., 0.); (1., -1., 0.) |]
      [| [| 0; 1; 2; 3 |] |]
      [| Topology.Closed_polyline |]
  in
  (match verify bow_tie with
  | Error error when Error.code error = "seam_self_intersection" -> ()
  | Error error -> failf "unexpected closed-curve error: %s" (Error.to_string error)
  | Ok () -> failf "self-crossing closed seam passed verification");
  let skew =
    curve_geometry
      [| (-1., 0., 0.); (1., 0., 0.); (0., -1., 1.); (0., 1., 1.) |]
      [| [| 0; 1 |]; [| 2; 3 |] |]
      [| Topology.Open_polyline; Topology.Open_polyline |]
  in
  ignore (verify skew |> get_rdk);
  let tiny = Int64.float_of_bits 1L in
  let subnormal =
    curve_geometry [| (0., 0., 0.); (tiny, 0., 0.) |] [| [| 0; 1 |] |] [| Topology.Open_polyline |]
  in
  ignore (verify subnormal |> get_rdk);
  (match verify (triangle [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |]) with
  | Error error when Error.code error = "invalid_output" -> ()
  | Error error -> failf "unexpected polygon verification error: %s" (Error.to_string error)
  | Ok () -> failf "polygon entered seam curve verification");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match verify ~cancel branch with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected seam verification cancellation: %s" (Error.to_string error)
  | Ok () -> failf "cancelled seam verification completed");
  let run domains = Parallel.run ~domains (fun () -> verify crossing) in
  check
    (Result.map_error Error.to_string (run 1) = Result.map_error Error.to_string (run 4))
    "seam verification diagnostic differs between domain counts"

let test_verification_scale () =
  let count = 4_096 in
  let points =
    Array.init (count * 2) (fun point ->
        let segment = point / 2 in
        (float_of_int segment *. 4., float_of_int (point land 1), 0.))
  and primitives = Array.init count (fun segment -> [| segment * 2; (segment * 2) + 1 |])
  and kinds = Array.make count Topology.Open_polyline in
  let curves = curve_geometry points primitives kinds in
  let run domains = Parallel.run ~domains (fun () -> Seam.View.verify_curves ~grain:256 curves) in
  check
    (run 1 = Ok () && run 4 = Ok ())
    "scale seam verification failed or differs across domain counts"

let run () =
  test_coincident_products ();
  test_self_kinds ();
  test_same_operand_coincidence ();
  test_native_nonmanifold_is_not_self_intersection ();
  test_cancellation ();
  test_invalid_grain ();
  test_edge_classification_retention ();
  test_post_rounding_verification ();
  test_verification_scale ()
