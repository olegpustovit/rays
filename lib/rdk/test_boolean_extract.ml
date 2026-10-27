open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Radial = Rdk_boolean.Boolean_radial
module Weiler = Rdk_boolean.Boolean_weiler
module Cells = Rdk_boolean.Boolean_cells
module Extract = Rdk_boolean.Boolean_extract

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

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

let polygon_geometry points vertex_points primitive_offsets =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count ~vertex_points
      ~primitive_offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let tetra ~origin:(ox,oy,oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox,oy+.size,oz; ox,oy,oz+.size|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3|]

let two_transverse_tetra_shells () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.;
      0.5,0.2,0.2; 2.5,0.2,0.2; 0.5,2.2,0.2; 0.5,0.2,2.2|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3;
      4;6;5; 4;5;7; 5;6;7; 6;4;7|]

let two_identical_tetra_shells () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.;
      0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3;
      4;6;5; 4;5;7; 5;6;7; 6;4;7|]

let cube_quads () = polygon_geometry
    [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.;
      0.,0.,1.; 1.,0.,1.; 1.,1.,1.; 0.,1.,1.|]
    [|0;3;2;1; 4;5;6;7; 0;1;5;4; 3;7;6;2; 0;4;7;3; 1;2;6;5|]
    [|0;4;8;12;16;20;24|]

let pipeline ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false) left right =
  let constraints = Constraints.build ~resolve_left_self_intersections
      ~resolve_right_self_intersections ~grain:1 ~left ~right () |> get in
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get in
  let complex = Complex.build constraints refinement |> get in
  let radial = Radial.build complex |> get in
  let weiler = Weiler.build complex radial |> get in
  let cells = Cells.build complex weiler |> get in
  complex, weiler, cells

let extract expression (complex, weiler, cells) =
  Extract.build ~expression complex weiler cells |> get

let signed_volume geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  let volume = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = triangle * 3 in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    volume := !volume +.
      (positions.x.(a) *. ((positions.y.(b) *. positions.z.(c))
                           -. (positions.z.(b) *. positions.y.(c)))
       +. positions.y.(a) *. ((positions.z.(b) *. positions.x.(c))
                              -. (positions.x.(b) *. positions.z.(c)))
       +. positions.z.(a) *. ((positions.x.(b) *. positions.y.(c))
                              -. (positions.y.(b) *. positions.x.(c)))) /. 6.
  done;
  !volume

let test_disjoint_operations () =
  let pipeline = pipeline
      (tetra ~origin:(0.,0.,0.) 1.)
      (tetra ~origin:(10.,0.,0.) 1.) in
  let union = extract Extract.union pipeline
  and intersection = extract Extract.intersection pipeline
  and difference = extract Extract.difference pipeline
  and xor = extract Extract.xor pipeline in
  check (Geometry.point_count union = 8 && Geometry.primitive_count union = 8)
    "disjoint union cardinality is wrong";
  check (Geometry.point_count intersection = 0
      && Geometry.primitive_count intersection = 0)
    "disjoint intersection is not empty";
  check (Geometry.point_count difference = 4
      && Geometry.primitive_count difference = 4)
    "disjoint difference did not retain the left solid";
  check (Geometry.primitive_count xor = 8) "disjoint XOR is wrong";
  check (signed_volume union > 0. && signed_volume difference > 0.)
    "disjoint extraction winding points inward"

let test_nested_operations () =
  let pipeline = pipeline
      (tetra ~origin:(0.,0.,0.) 4.)
      (tetra ~origin:(1.,1.,1.) 0.5) in
  let union = extract Extract.union pipeline
  and intersection = extract Extract.intersection pipeline
  and difference = extract Extract.difference pipeline
  and reverse = extract Extract.reverse_difference pipeline
  and xor = extract Extract.xor pipeline in
  check (Geometry.primitive_count union = 4)
    "nested union did not discard the inner boundary";
  check (Geometry.primitive_count intersection = 4)
    "nested intersection did not select the inner solid";
  check (Geometry.primitive_count difference = 8)
    "nested subtraction did not create a cavity";
  check (Geometry.primitive_count reverse = 0)
    "inner minus containing solid is not empty";
  check (Geometry.primitive_count xor = 8)
    "nested XOR did not create the shell between solids";
  check (signed_volume union > signed_volume difference
      && signed_volume difference > 0.
      && signed_volume intersection > 0.)
    "nested output winding/volume relation is wrong";
  let custom = extract
      (Extract.And (Extract.Left, Extract.Not Extract.Right)) pipeline in
  check (Geometry.primitive_count custom = Geometry.primitive_count difference
      && Geometry.point_count custom = Geometry.point_count difference)
    "typed custom expression disagrees with difference"

let test_transverse_solid_operations () =
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let pipeline = pipeline left right in
  let named name expression =
    let complex, weiler, cells = pipeline in
    match Extract.build ~expression complex weiler cells with
    | Ok geometry -> geometry
    | Error error -> fail "%s: %s" name (Error.to_string error) in
  let union = named "union" Extract.union in
  let intersection = named "intersection" Extract.intersection in
  let left_difference = named "difference" Extract.difference in
  let right_difference = named "reverse difference" Extract.reverse_difference in
  let xor = named "xor" Extract.xor in
  let union_volume = signed_volume union
  and intersection_volume = signed_volume intersection
  and left_volume = signed_volume left_difference
  and right_volume = signed_volume right_difference
  and xor_volume = signed_volume xor in
  check (Geometry.primitive_count union > 4
      && Geometry.primitive_count intersection > 0
      && Geometry.primitive_count left_difference > 0
      && Geometry.primitive_count right_difference > 0)
    "transverse solid operation lost a non-empty region";
  check (union_volume > 0. && intersection_volume > 0.
      && left_volume > 0. && right_volume > 0. && xor_volume > 0.)
    "transverse solid extraction has inward or zero volume";
  let close_volume left right = abs_float (left -. right) <= 1e-10 in
  check (close_volume union_volume
      (left_volume +. right_volume +. intersection_volume))
    "transverse Boolean volumes violate region partitioning";
  check (close_volume xor_volume (left_volume +. right_volume))
    "transverse XOR volume violates region partitioning"

let test_identical_solids () =
  let solid = tetra ~origin:(0.,0.,0.) 2. in
  let pipeline = pipeline solid solid in
  let union = extract Extract.union pipeline
  and intersection = extract Extract.intersection pipeline
  and difference = extract Extract.difference pipeline
  and reverse = extract Extract.reverse_difference pipeline
  and xor = extract Extract.xor pipeline in
  check (Geometry.primitive_count union = 4
      && Geometry.primitive_count intersection = 4)
    "identical solid union/intersection did not retain one boundary";
  check (Geometry.primitive_count difference = 0
      && Geometry.primitive_count reverse = 0
      && Geometry.primitive_count xor = 0)
    "identical solid subtraction/XOR is not empty"

let test_face_touching_solids () =
  let upper = tetra ~origin:(0.,0.,0.) 1.
  and lower = geometry
      [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.; 0.,0.,-1.|]
      [|0;1;2; 0;3;1; 1;3;2; 2;3;0|] in
  let pipeline = pipeline upper lower in
  let union = extract Extract.union pipeline
  and intersection = extract Extract.intersection pipeline
  and upper_difference = extract Extract.difference pipeline
  and lower_difference = extract Extract.reverse_difference pipeline
  and xor = extract Extract.xor pipeline in
  check (Geometry.primitive_count union = 6)
    "face-touching union retained the internal coincident face";
  check (Geometry.primitive_count intersection = 0)
    "face-only contact produced a volumetric intersection";
  check (Geometry.primitive_count upper_difference = 4
      && Geometry.primitive_count lower_difference = 4)
    "face contact changed a subtraction operand";
  check (Geometry.primitive_count xor = 6)
    "face-touching XOR retained the coincident interface"

let test_same_operand_overlapping_shells () =
  let self = two_transverse_tetra_shells ()
  and far = tetra ~origin:(10.,0.,0.) 1. in
  let combined = pipeline ~resolve_left_self_intersections:true self far in
  let union = extract Extract.union combined
  and difference = extract Extract.difference combined
  and intersection = extract Extract.intersection combined in
  let pair_union = pipeline
      (tetra ~origin:(0.,0.,0.) 2.)
      (tetra ~origin:(0.5,0.2,0.2) 2.)
      |> extract Extract.union in
  let expected_self = signed_volume pair_union
  and expected_far = signed_volume far in
  let close_volume left right = abs_float (left -. right) <= 1e-10 in
  check (close_volume (signed_volume difference) expected_self)
    "same-operand winding did not resolve overlapping shells to their union";
  check (close_volume (signed_volume union) (expected_self +. expected_far))
    "same-operand union changed the disjoint second operand";
  check (Geometry.primitive_count intersection = 0)
    "disjoint second operand intersected a self-arranged first operand";
  let mirrored = pipeline ~resolve_right_self_intersections:true far self in
  check (close_volume
      (signed_volume (extract Extract.reverse_difference mirrored)) expected_self)
    "right-operand self-intersection policy changed resolved winding";
  check (Geometry.primitive_count (extract Extract.intersection mirrored) = 0)
    "disjoint first operand intersected a self-arranged second operand";
  let result_signature geometry =
    let positions = Packed.Float3.Private.view (Geometry.positions geometry)
    and topology = Topology.Private.view (Geometry.topology geometry) in
    Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
    Array.copy topology.vertex_points, Array.copy topology.primitive_offsets in
  let run domains = Parallel.run ~domains (fun () ->
      pipeline ~resolve_left_self_intersections:true self far
      |> extract Extract.difference |> result_signature) in
  check (run 1 = run 4)
    "same-operand self-arrangement differs between domain counts"

let test_same_operand_coincident_shells () =
  let self = two_identical_tetra_shells ()
  and far = tetra ~origin:(10.,0.,0.) 1. in
  let output = pipeline ~resolve_left_self_intersections:true self far
      |> extract Extract.difference in
  check (Geometry.primitive_count output = 4)
    "same-operand coincident shells retained duplicate boundary facets";
  check (abs_float (signed_volume output -. (8. /. 6.)) <= 1e-10)
    "same-operand coincident shells changed the represented solid volume"

let signature geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
  Array.copy topology.vertex_points, Array.copy topology.primitive_offsets

let validate_ancestry_positions left right ancestry =
  let output = Extract.geometry ancestry in
  let output_positions = Packed.Float3.Private.view (Geometry.positions output)
  and output_topology = Topology.Private.view (Geometry.topology output)
  and left_positions = Packed.Float3.Private.view (Geometry.positions left)
  and right_positions = Packed.Float3.Private.view (Geometry.positions right) in
  let close expected actual = abs_float (expected -. actual) <= 1e-12 in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    let source_positions = match Extract.primitive_side ancestry primitive with
      | Complex.Left -> left_positions | Complex.Right -> right_positions in
    let p0 = Extract.primitive_source_point ancestry primitive 0
    and p1 = Extract.primitive_source_point ancestry primitive 1
    and p2 = Extract.primitive_source_point ancestry primitive 2 in
    for local = 0 to 2 do
      let wa, wb, wc = Extract.corner_barycentric ancestry primitive local in
      check (close 1. (wa +. wb +. wc))
        "Boolean ancestry barycentric weights do not sum to one";
      let point = output_topology.vertex_points.((primitive * 3) + local) in
      let interpolate plane =
        (wa *. plane.(p0)) +. (wb *. plane.(p1)) +. (wc *. plane.(p2)) in
      check (close output_positions.x.(point) (interpolate source_positions.x)
          && close output_positions.y.(point) (interpolate source_positions.y)
          && close output_positions.z.(point) (interpolate source_positions.z))
        "Boolean ancestry barycentrics do not reconstruct an output corner"
    done
  done

let test_domain_exactness () =
  let left = tetra ~origin:(0.,0.,0.) 4.
  and right = tetra ~origin:(1.,1.,1.) 0.5 in
  let run domains = Parallel.run ~domains (fun () ->
      pipeline left right |> extract Extract.difference |> signature) in
  if run 1 <> run 4 then fail "Boolean extraction differs between domain counts"

let test_cancellation () =
  let complex, weiler, cells = pipeline
      (tetra ~origin:(0.,0.,0.) 1.)
      (tetra ~origin:(10.,0.,0.) 1.) in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Extract.build ~cancel ~expression:Extract.union complex weiler cells with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled Boolean extraction completed"

let test_source_ancestry () =
  let left = tetra ~origin:(0.,0.,0.) 4.
  and right = tetra ~origin:(1.,1.,1.) 0.5 in
  let complex, weiler, cells = pipeline left right in
  let ancestry = Extract.build_with_ancestry ~expression:Extract.difference
      complex weiler cells |> get in
  let geometry = Extract.geometry ancestry in
  check (Geometry.primitive_count geometry = 8)
    "ancestry extraction changed difference cardinality";
  check (Extract.Private.complex ancestry == complex)
    "Boolean ancestry did not retain its exact complex identity";
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  for point = 0 to Geometry.point_count geometry - 1 do
    let complex_vertex = Extract.Private.point_complex_vertex ancestry point in
    check (complex_vertex >= 0 && complex_vertex < Complex.vertex_count complex)
      "Boolean output point has invalid complex-vertex ancestry";
    let x, y, z = Complex.approximate_vertex complex complex_vertex in
    check (positions.x.(point) = x && positions.y.(point) = y
        && positions.z.(point) = z)
      "Boolean output point differs from its once-rounded complex vertex"
  done;
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    let facet = Extract.Private.primitive_complex_facet ancestry primitive in
    check (facet >= 0 && facet < Complex.facet_count complex)
      "Boolean output primitive has invalid complex-facet ancestry";
    for local = 0 to 2 do
      let output_point = topology.vertex_points.((primitive * 3) + local) in
      let complex_vertex = Extract.Private.point_complex_vertex ancestry output_point in
      let belongs = ref false in
      for facet_local = 0 to 2 do
        if Complex.facet_vertex complex facet facet_local = complex_vertex then
          belongs := true
      done;
      check !belongs
        "Boolean output corner is not a vertex of its exact source facet"
    done
  done;
  let left = ref 0 and right = ref 0 and reversed_right = ref 0 in
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    check (Extract.primitive_face ancestry primitive >= 0
        && Extract.primitive_face ancestry primitive < 4)
      "Boolean ancestry source face is out of range";
    check (Extract.primitive_triangle ancestry primitive >= -1)
      "Boolean ancestry source triangle is invalid";
    check (Extract.primitive_refined_triangle ancestry primitive >= -1)
      "Boolean ancestry refinement child is invalid";
    check (abs (Extract.primitive_winding ancestry primitive) = 1)
      "Boolean ancestry winding is not signed unit orientation";
    match Extract.primitive_side ancestry primitive with
    | Complex.Left -> incr left
    | Complex.Right ->
        incr right;
        if Extract.primitive_winding ancestry primitive < 0 then
          incr reversed_right
  done;
  check (!left = 4 && !right = 4 && !reversed_right = 4)
    "nested difference ancestry did not retain left shell/right reversed cavity";
  validate_ancestry_positions
    (tetra ~origin:(0.,0.,0.) 4.) (tetra ~origin:(1.,1.,1.) 0.5) ancestry;
  let identical = tetra ~origin:(0.,0.,0.) 2. in
  let complex, weiler, cells = pipeline identical identical in
  let ancestry = Extract.build_with_ancestry ~expression:Extract.union
      complex weiler cells |> get in
  for primitive = 0 to Geometry.primitive_count (Extract.geometry ancestry) - 1 do
    check (Extract.primitive_side ancestry primitive = Complex.Left)
      "coincident ownership did not choose the stable first source member"
  done;
  let complex, weiler, cells = pipeline (cube_quads ())
      (tetra ~origin:(10.,0.,0.) 1.) in
  let ancestry = Extract.build_with_ancestry ~expression:Extract.union
      complex weiler cells |> get in
  let saw_later_triangle = ref false in
  for primitive = 0 to Geometry.primitive_count (Extract.geometry ancestry) - 1 do
    if Extract.primitive_side ancestry primitive = Complex.Left then begin
      check (Extract.primitive_face ancestry primitive < 6)
        "n-gon ancestry confused an internal triangle with a source primitive";
      if Extract.primitive_triangle ancestry primitive >= 6 then
        saw_later_triangle := true
    end
  done;
  check !saw_later_triangle
    "n-gon ancestry fixture did not exercise distinct primitive/triangle IDs";
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let complex, weiler, cells = pipeline left right in
  let ancestry = Extract.build_with_ancestry ~expression:Extract.union
      complex weiler cells |> get in
  validate_ancestry_positions left right ancestry

let test_materialization_validation () =
  let validate = Extract.Private.validate_materialized in
  (match validate ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Ok () -> ()
   | Error error -> fail "valid rounded triangle failed: %s" (Error.to_string error));
  (match validate ~x:[|0.;0.|] ~y:[|0.;0.|] ~z:[|0.;0.|]
      ~vertex_points:[||] () with
   | Error error when Error.code error = "rounding_collision" -> ()
   | Error error -> fail "unexpected rounding collision error: %s" (Error.to_string error)
   | Ok () -> fail "rounded exact-point collision was accepted");
  (match validate ~x:[|0.;1.;2.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Error error when Error.code error = "rounding_degenerate" -> ()
   | Error error -> fail "unexpected rounding degeneracy error: %s" (Error.to_string error)
   | Ok () -> fail "rounded collinear output triangle was accepted");
  (match validate ~allow_degenerate:true
      ~x:[|0.;1.;2.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Ok () -> ()
   | Error error -> fail "explicit deferred rounding repair was rejected: %s"
       (Error.to_string error));
  let x, y, z, representatives, map = Extract.Private.coalesce_positions
      ~complex_vertices:[|10;11;12;13;14|]
      ~x:[|0.;1.;1.;2.;1.|] ~y:[|0.;0.;0.;0.;0.|]
      ~z:[|0.;0.;0.;0.;0.|] in
  check (x = [|0.;1.;2.|] && y = [|0.;0.;0.|] && z = [|0.;0.;0.|]
      && representatives = [|10;11;13|] && map = [|0;1;1;2;1|])
    "rounded-point coalescing lost stable representatives after duplicate gaps"

let run () =
  test_disjoint_operations ();
  test_nested_operations ();
  test_transverse_solid_operations ();
  test_identical_solids ();
  test_face_touching_solids ();
  test_same_operand_overlapping_shells ();
  test_same_operand_coincident_shells ();
  test_domain_exactness ();
  test_cancellation ();
  test_source_ancestry ();
  test_materialization_validation ()
