module Point = struct
  include Rdk_exact.Implicit_point
  let sign value = match reference value with
    | -1 -> Rdk.Predicates.Negative
    | 0 -> Rdk.Predicates.Zero
    | 1 -> Rdk.Predicates.Positive
    | _ -> assert false
  let compare_reference_x a b = reference (Compare_x (a,b))
  let compare_reference_y a b = reference (Compare_y (a,b))
  let compare_reference_z a b = reference (Compare_z (a,b))
  let orient2d_reference_xy a b c = sign (Orient2d_xy (a,b,c))
  let orient2d_reference_yz a b c = sign (Orient2d_yz (a,b,c))
  let orient2d_reference_zx a b c = sign (Orient2d_zx (a,b,c))
  let orient3d_reference a b c d = sign (Orient3d (a,b,c,d))
  let radial_dot_reference a b c d = sign (Radial_dot (a,b,c,d))
  let ray_edge_reference ~query ~first ~second ~dx ~dy ~dz =
    sign (Ray_edge (query,first,second,dx,dy,dz))
  let ray_edge_symbolic_reference ~query ~first ~second =
    sign (Ray_edge_symbolic (query,first,second))
  let normal_dot_direction_reference a b c ~dx ~dy ~dz =
    sign (Normal_dot_direction (a,b,c,dx,dy,dz))
  let normal_dot_symbolic_reference a b c = sign (Normal_dot_symbolic (a,b,c))
  let incircle_reference_xy a b c d = sign (Incircle_xy (a,b,c,d))
  let incircle_reference_yz a b c d = sign (Incircle_yz (a,b,c,d))
  let incircle_reference_zx a b c d = sign (Incircle_zx (a,b,c,d))
end

let fail format = Printf.ksprintf failwith format

let expect_ok = function
  | Ok value -> value
  | Error _ -> fail "expected exact point construction to succeed"

let expect_error expected = function
  | Error actual when actual = expected -> ()
  | Error _ -> fail "unexpected exact point construction error"
  | Ok _ -> fail "expected exact point construction to fail"

let assert_close expected actual =
  if abs_float (expected -. actual) > 1e-14 then
    fail "expected %.17g, got %.17g" expected actual

let assert_sign expected actual =
  if actual <> expected then fail "unexpected predicate sign"

let coordinates = [|
  (0., 0., 0.);       (* 0 *)
  (2., 0., 0.);       (* 1 *)
  (1., -1., -1.);     (* 2: x = 1 plane *)
  (1., 1., -1.);      (* 3 *)
  (1., 0., 1.);       (* 4 *)
  (1., 0., 0.);       (* 5: first LPI explicitly *)
  (1., 0., 0.);       (* 6: x = 1 plane *)
  (1., 1., 0.);       (* 7 *)
  (1., 0., 1.);       (* 8 *)
  (0., 2., 0.);       (* 9: y = 2 plane *)
  (1., 2., 0.);       (* 10 *)
  (0., 2., 1.);       (* 11 *)
  (0., 0., 3.);       (* 12: z = 3 plane *)
  (1., 0., 3.);       (* 13 *)
  (0., 1., 3.);       (* 14 *)
  (1., 2., 3.);       (* 15: TPI explicitly *)
  (0., 2., 0.);       (* 16: mixed orientation points *)
  (2., 2., 0.);       (* 17 *)
  (1., 1., 1.);       (* 18 *)
|]

let x = Array.map (fun (x, _, _) -> x) coordinates
let y = Array.map (fun (_, y, _) -> y) coordinates
let z = Array.map (fun (_, _, z) -> z) coordinates

let test_source_validation () =
  expect_error Point.Coordinate_plane_size_mismatch
    (Point.source ~x:[|0.|] ~y:[||] ~z:[|0.|]);
  expect_error (Point.Non_finite_coordinate 1)
    (Point.source ~x:[|0.; Float.infinity|] ~y:[|0.; 0.|] ~z:[|0.; 0.|])

let test_line_plane source =
  let intersection = expect_ok
      (Point.line_plane source ~line_start:0 ~line_end:1
         ~plane_a:2 ~plane_b:3 ~plane_c:4)
  and reversed = expect_ok
      (Point.line_plane source ~line_start:1 ~line_end:0
         ~plane_a:4 ~plane_b:3 ~plane_c:2)
  and explicit = expect_ok (Point.explicit source 5) in
  if not (Point.equal intersection explicit) then
    fail "LPI did not equal its explicit Cartesian point";
  if not (Point.equal intersection reversed) then
    fail "reversing both LPI recipes changed the exact point";
  let px, py, pz = Point.approximate intersection in
  let (xl, xu), (yl, yu), (zl, zu) = Point.bounds intersection in
  if not (xl <= 1. && xu >= 1. && yl <= 0. && yu >= 0.
      && zl <= 0. && zu >= 0.) then
    fail "LPI interval does not enclose exact point: x=[%g,%g] y=[%g,%g] z=[%g,%g]"
      xl xu yl yu zl zu;
  if xu -. xl > 1e-12 || yu -. yl > 1e-12 || zu -. zl > 1e-12 then
    fail "LPI interval unexpectedly wide: x=[%.17g,%.17g] y=[%.17g,%.17g] z=[%.17g,%.17g]"
      xl xu yl yu zl zu;
  assert_close 1. px; assert_close 0. py; assert_close 0. pz;
  if Point.compare_x (expect_ok (Point.explicit source 0)) intersection >= 0
      || Point.compare_x intersection (expect_ok (Point.explicit source 1)) >= 0
  then fail "exact LPI axis ordering failed";
  expect_error Point.Parallel_line_and_plane
    (Point.line_plane source ~line_start:0 ~line_end:1
       ~plane_a:9 ~plane_b:10 ~plane_c:11);
  expect_error (Point.Index_out_of_bounds 999)
    (Point.line_plane source ~line_start:0 ~line_end:999
       ~plane_a:2 ~plane_b:3 ~plane_c:4);
  intersection

let test_triple_plane source =
  let intersection = expect_ok
      (Point.triple_plane source
         ~first_a:6 ~first_b:7 ~first_c:8
         ~second_a:9 ~second_b:10 ~second_c:11
         ~third_a:12 ~third_b:13 ~third_c:14)
  and permuted = expect_ok
      (Point.triple_plane source
         ~first_a:14 ~first_b:13 ~first_c:12
         ~second_a:11 ~second_b:10 ~second_c:9
         ~third_a:8 ~third_b:7 ~third_c:6)
  and explicit = expect_ok (Point.explicit source 15) in
  if not (Point.equal intersection explicit) then
    fail "TPI did not equal its explicit Cartesian point";
  if not (Point.equal intersection permuted) then
    fail "plane permutation/orientation changed the exact TPI";
  let px, py, pz = Point.approximate intersection in
  assert_close 1. px; assert_close 2. py; assert_close 3. pz;
  expect_error Point.Dependent_planes
    (Point.triple_plane source
       ~first_a:6 ~first_b:7 ~first_c:8
       ~second_a:2 ~second_b:3 ~second_c:4
       ~third_a:12 ~third_b:13 ~third_c:14);
  intersection

let test_line_line () =
  let coordinates = [|
    (-1., 0., 0.); (1., 0., 0.);
    (0., -1., 0.); (0., 1., 0.);
    (0., 0., 0.);
    (0., -1., 0.); (0., 1., 0.);
    (0., 0., -1.); (0., 0., 1.);
    (0., 0., 0.);
    (-1., 0., 0.); (1., 0., 0.);
    (0., 0., -1.); (0., 0., 1.);
    (0., 0., 0.);
  |] in
  let x = Array.map (fun (x, _, _) -> x) coordinates
  and y = Array.map (fun (_, y, _) -> y) coordinates
  and z = Array.map (fun (_, _, z) -> z) coordinates in
  let source = expect_ok (Point.source ~x ~y ~z) in
  let check projection first_start first_end second_start second_end explicit =
    let intersection = expect_ok (Point.line_line source ~projection
        ~first_start ~first_end ~second_start ~second_end)
    and reversed = expect_ok (Point.line_line source ~projection
        ~first_start:first_end ~first_end:first_start
        ~second_start:second_end ~second_end:second_start)
    and explicit = expect_ok (Point.explicit source explicit) in
    if not (Point.equal intersection explicit) then
      fail "line-line construction did not equal its explicit point";
    if not (Point.equal intersection reversed) then
      fail "reversing both line recipes changed their exact intersection";
    let px, py, pz = Point.approximate intersection in
    assert_close 0. px; assert_close 0. py; assert_close 0. pz;
    intersection
  in
  let xy = check Point.XY 0 1 2 3 4
  and yz = check Point.YZ 5 6 7 8 9
  and zx = check Point.ZX 10 11 12 13 14 in
  let origin = expect_ok (Point.explicit source 4)
  and x_positive = expect_ok (Point.explicit source 1)
  and y_positive = expect_ok (Point.explicit source 3) in
  assert_sign Rdk.Predicates.Zero (Point.orient2d_xy origin xy x_positive);
  assert_sign Rdk.Predicates.Positive
    (Point.orient2d_xy xy x_positive y_positive);
  if not (Point.equal xy yz && Point.equal yz zx) then
    fail "equivalent projected line-line constructions disagreed";
  expect_error Point.Parallel_lines
    (Point.line_line source ~projection:Point.XY
       ~first_start:0 ~first_end:1 ~second_start:10 ~second_end:11);
  expect_error (Point.Index_out_of_bounds 999)
    (Point.line_line source ~projection:Point.XY
       ~first_start:0 ~first_end:1 ~second_start:2 ~second_end:999)

let test_randomized_line_line () =
  let deterministic multiplier offset index =
    ((index * multiplier + offset) mod 101) - 50 in
  let direction projection = match projection with
    | Point.XY -> (2, 1, 3), (-1, 2, -2)
    | Point.YZ -> (3, 2, 1), (-2, -1, 2)
    | Point.ZX -> (1, 3, 2), (2, -2, -1) in
  let add_scaled (x, y, z) scale (dx, dy, dz) =
    float_of_int (x + (scale * dx)),
    float_of_int (y + (scale * dy)),
    float_of_int (z + (scale * dz)) in
  Array.iter (fun projection ->
    let first_direction, second_direction = direction projection in
    for index = 0 to 499 do
      let center = deterministic 17 3 index,
          deterministic 23 5 index,
          deterministic 31 7 index in
      let coordinates = [|
        add_scaled center (-2) first_direction;
        add_scaled center 3 first_direction;
        add_scaled center (-3) second_direction;
        add_scaled center 2 second_direction;
        add_scaled center 0 first_direction;
      |] in
      let x = Array.map (fun (x, _, _) -> x) coordinates
      and y = Array.map (fun (_, y, _) -> y) coordinates
      and z = Array.map (fun (_, _, z) -> z) coordinates in
      let source = expect_ok (Point.source ~x ~y ~z) in
      let intersection = expect_ok (Point.line_line source ~projection
          ~first_start:0 ~first_end:1 ~second_start:2 ~second_end:3)
      and explicit = expect_ok (Point.explicit source 4) in
      if not (Point.equal intersection explicit) then
        fail "randomized projected line intersection %d was not exact" index;
      let first = expect_ok (Point.explicit source 0)
      and second = expect_ok (Point.explicit source 1)
      and third = expect_ok (Point.explicit source 2)
      and fourth = expect_ok (Point.explicit source 3) in
      let orient = match projection with
        | Point.XY -> Point.orient2d_xy
        | Point.YZ -> Point.orient2d_yz
        | Point.ZX -> Point.orient2d_zx in
      assert_sign Rdk.Predicates.Zero (orient first second intersection);
      assert_sign Rdk.Predicates.Zero (orient third fourth intersection)
    done) [|Point.XY; Point.YZ; Point.ZX|]

let test_ill_conditioned_line_line () =
  let tiny = 1e-300 in
  let source = expect_ok (Point.source
      ~x:[|0.; 1.; 0.; 1.|]
      ~y:[|0.; tiny; tiny; Float.next_after (2. *. tiny) Float.infinity|]
      ~z:[|0.; 0.; 0.; 0.|]) in
  let intersection = expect_ok (Point.line_line source ~projection:Point.XY
      ~first_start:0 ~first_end:1 ~second_start:2 ~second_end:3) in
  let points = Array.init 4 (fun index -> expect_ok (Point.explicit source index)) in
  assert_sign Rdk.Predicates.Zero
    (Point.orient2d_xy points.(0) points.(1) intersection);
  assert_sign Rdk.Predicates.Zero
    (Point.orient2d_xy points.(2) points.(3) intersection);
  let parallel = expect_ok (Point.source
      ~x:[|0.; 1.; 0.; 1.|]
      ~y:[|0.; tiny; tiny; 2. *. tiny|]
      ~z:[|0.; 0.; 0.; 0.|]) in
  expect_error Point.Parallel_lines
    (Point.line_line parallel ~projection:Point.XY
       ~first_start:0 ~first_end:1 ~second_start:2 ~second_end:3)

let test_centroid () =
  let source = expect_ok (Point.source
      ~x:[|0.; 3.; 0.; 1.; 1.|]
      ~y:[|0.; 0.; 3.; 1.; -1.|]
      ~z:[|0.; 0.; 0.; 0.; 0.|]) in
  let a = expect_ok (Point.explicit source 0)
  and b = expect_ok (Point.explicit source 1)
  and c = expect_ok (Point.explicit source 2)
  and explicit = expect_ok (Point.explicit source 3) in
  let centroid = expect_ok (Point.centroid3 a b c) in
  if not (Point.equal centroid explicit) then
    fail "exact implicit centroid disagreed with (1,1,0)";
  let line = expect_ok (Point.line_line source ~projection:Point.XY
      ~first_start:0 ~first_end:1 ~second_start:3 ~second_end:4) in
  let mixed = expect_ok (Point.centroid3 line b c) in
  assert_sign Rdk.Predicates.Zero (Point.orient3d a b c mixed);
  let other_source = expect_ok (Point.source
      ~x:[|0.|] ~y:[|0.|] ~z:[|0.|]) in
  let other = expect_ok (Point.explicit other_source 0) in
  expect_error Point.Point_source_mismatch (Point.centroid3 a b other)

let test_midpoint_and_circumcenter () =
  let source = expect_ok (Point.source
      ~x:[|0.;4.;0.;2.;2.;2.|]
      ~y:[|0.;0.;2.;0.;1.;3.|]
      ~z:[|0.;0.;0.;0.;0.;0.|]) in
  let points = Array.init 6 (fun point -> expect_ok (Point.explicit source point)) in
  let midpoint = expect_ok (Point.midpoint points.(0) points.(1)) in
  if not (Point.equal midpoint points.(3)) then
    fail "exact implicit midpoint disagreed with (2,0,0)";
  let rounded = expect_ok
      (Point.rounded ~reference:points.(0) ~x:2. ~y:1. ~z:0.) in
  if not (Point.equal rounded points.(4)) then
    fail "rounded implicit point disagreed with explicit binary64 point";
  expect_error Point.Non_finite_approximation
    (Point.rounded ~reference:points.(0) ~x:infinity ~y:0. ~z:0.);
  let circumcenter = expect_ok
      (Point.circumcenter2_xy points.(0) points.(1) points.(2)) in
  if not (Point.equal circumcenter points.(4)) then
    fail "exact implicit circumcenter disagreed with (2,1,0)";
  assert_sign Rdk.Predicates.Negative
    (Point.diametral_dot_xy ~first:points.(0) ~second:points.(1) points.(4));
  assert_sign Rdk.Predicates.Zero
    (Point.diametral_dot_xy ~first:points.(0) ~second:points.(1) points.(0));
  assert_sign Rdk.Predicates.Positive
    (Point.diametral_dot_xy ~first:points.(0) ~second:points.(1) points.(5));
  expect_error Point.Degenerate_triangle
    (Point.circumcenter2_xy points.(0) points.(1) points.(3));
  let other_source = expect_ok (Point.source
      ~x:[|0.|] ~y:[|0.|] ~z:[|0.|]) in
  let other = expect_ok (Point.explicit other_source 0) in
  expect_error Point.Point_source_mismatch (Point.midpoint points.(0) other);
  expect_error Point.Point_source_mismatch
    (Point.circumcenter2_xy points.(0) points.(1) other)

let test_mixed_predicates source lpi tpi =
  let origin = expect_ok (Point.explicit source 0)
  and x_axis = expect_ok (Point.explicit source 1)
  and upper_left = expect_ok (Point.explicit source 16)
  and upper_right = expect_ok (Point.explicit source 17)
  and unit = expect_ok (Point.explicit source 18)
  and explicit_lpi = expect_ok (Point.explicit source 5)
  and explicit_tpi = expect_ok (Point.explicit source 15) in
  assert_sign Rdk.Predicates.Positive
    (Point.orient2d_xy origin x_axis upper_left);
  assert_sign Rdk.Predicates.Zero
    (Point.orient2d_xy origin lpi x_axis);
  assert_sign Rdk.Predicates.Positive
    (Point.orient2d_xy lpi upper_right upper_left);
  assert_sign (Rdk.Predicates.orient3d
      ~ax:0. ~ay:0. ~az:0.
      ~bx:2. ~by:0. ~bz:0.
      ~cx:0. ~cy:2. ~cz:0.
      ~dx:1. ~dy:1. ~dz:1.)
    (Point.orient3d origin x_axis upper_left unit);
  assert_sign Rdk.Predicates.Zero
    (Point.orient3d origin lpi x_axis explicit_lpi);
  assert_sign Rdk.Predicates.Zero
    (Point.orient3d tpi explicit_tpi origin x_axis);
  let compare arena reference message =
    if arena <> reference then fail "%s" message in
  compare (Point.compare_x lpi explicit_lpi)
    (Point.compare_reference_x lpi explicit_lpi)
    "packed exact X comparison differs from reference";
  compare (Point.compare_y tpi explicit_tpi)
    (Point.compare_reference_y tpi explicit_tpi)
    "packed exact Y comparison differs from reference";
  compare (Point.compare_z tpi explicit_tpi)
    (Point.compare_reference_z tpi explicit_tpi)
    "packed exact Z comparison differs from reference";
  compare (Point.orient2d_xy origin lpi x_axis)
    (Point.orient2d_reference_xy origin lpi x_axis)
    "packed exact XY orientation differs from reference";
  compare (Point.orient2d_yz lpi upper_right upper_left)
    (Point.orient2d_reference_yz lpi upper_right upper_left)
    "packed exact YZ orientation differs from reference";
  compare (Point.orient2d_zx tpi explicit_tpi unit)
    (Point.orient2d_reference_zx tpi explicit_tpi unit)
    "packed exact ZX orientation differs from reference";
  compare (Point.orient3d tpi explicit_tpi origin x_axis)
    (Point.orient3d_reference tpi explicit_tpi origin x_axis)
    "packed exact 3D orientation differs from reference"

let deterministic_value multiplier offset index =
  ((index * multiplier + offset) mod 101) - 50

let evaluate_line_plane_case index =
  let y0 = float_of_int (deterministic_value 37 11 index)
  and z0 = float_of_int (deterministic_value 43 17 index)
  and y1 = float_of_int (deterministic_value 53 23 index)
  and z1 = float_of_int (deterministic_value 61 29 index) in
  let expected_y = (y0 +. y1) *. 0.5
  and expected_z = (z0 +. z1) *. 0.5 in
  let source = expect_ok (Point.source
      ~x:[|-1.; 3.; 1.; 1.; 1.; 1.|]
      ~y:[|y0; y1; 0.; 1.; 0.; expected_y|]
      ~z:[|z0; z1; 0.; 0.; 1.; expected_z|]) in
  let lpi = expect_ok (Point.line_plane source
      ~line_start:0 ~line_end:1 ~plane_a:2 ~plane_b:3 ~plane_c:4)
  and explicit = expect_ok (Point.explicit source 5)
  and plane_a = expect_ok (Point.explicit source 2)
  and plane_b = expect_ok (Point.explicit source 3)
  and plane_c = expect_ok (Point.explicit source 4)
  and line_start = expect_ok (Point.explicit source 0) in
  if not (Point.equal lpi explicit) then
    fail "randomized LPI %d lost an exactly representable midpoint" index;
  let x, y, z = Point.approximate lpi in
  if abs_float (x -. 1.) > 1e-12
      || abs_float (y -. expected_y) > 1e-12
      || abs_float (z -. expected_z) > 1e-12 then
    fail "randomized LPI %d produced an unstable approximation" index;
  let expected_2d = Rdk.Predicates.orient2d
      ~ax:expected_y ~ay:expected_z ~bx:1. ~by:0. ~cx:0. ~cy:1. in
  if Point.orient2d_yz lpi plane_b plane_c <> expected_2d
      || Point.orient2d_yz explicit plane_b plane_c <> expected_2d then
    fail "randomized LPI %d changed a filtered projected orientation" index;
  if Point.orient2d_yz lpi plane_b plane_c
      <> Point.orient2d_reference_yz lpi plane_b plane_c then
    fail "randomized packed exact projected orientation differed at %d" index;
  let expected_3d = Rdk.Predicates.orient3d
      ~ax:1. ~ay:expected_y ~az:expected_z
      ~bx:1. ~by:0. ~bz:0.
      ~cx:1. ~cy:1. ~cz:0.
      ~dx:(-1.) ~dy:y0 ~dz:z0 in
  if Point.orient3d lpi plane_a plane_b line_start <> expected_3d
      || Point.orient3d explicit plane_a plane_b line_start <> expected_3d then
    fail "randomized LPI %d changed a filtered 3D orientation" index;
  if Point.orient3d lpi plane_a plane_b line_start
      <> Point.orient3d_reference lpi plane_a plane_b line_start then
    fail "randomized packed exact 3D orientation differed at %d" index;
  if Point.compare_x lpi explicit <> Point.compare_reference_x lpi explicit
      || Point.compare_y lpi explicit <> Point.compare_reference_y lpi explicit
      || Point.compare_z lpi explicit <> Point.compare_reference_z lpi explicit then
    fail "randomized packed exact comparison differed at %d" index;
  (Point.compare_x lpi explicit * 31)
  + (Point.compare_y lpi explicit * 17)
  + (Point.compare_z lpi explicit * 13)

let test_randomized_constructions () =
  for index = 0 to 1_999 do
    if evaluate_line_plane_case index <> 0 then
      fail "randomized exact comparison failed"
  done

let construction_signature domains =
  let count = 2_048 and output = Array.make 2_048 min_int in
  Parallel.run ~domains (fun () ->
    Parallel.for_ ~chunk_size:37 ~start:0 ~finish:(count - 1)
      (fun index -> output.(index) <- evaluate_line_plane_case index));
  output

let test_domain_exactness () =
  let one = construction_signature 1 and four = construction_signature 4 in
  if one <> four then
    fail "implicit exact constructions differ between one and four domains"

let incircle_points coordinates =
  let x = Array.map (fun (x, _, _) -> x) coordinates
  and y = Array.map (fun (_, y, _) -> y) coordinates
  and z = Array.map (fun (_, _, z) -> z) coordinates in
  let source = expect_ok (Point.source ~x ~y ~z) in
  (source, Array.init (Array.length coordinates)
      (fun index -> expect_ok (Point.explicit source index)))

let test_incircle () =
  let source, points = incircle_points [|
      (1., 0., 0.); (0., 1., 0.); (-1., 0., 0.);
      (0., 0., 0.); (0., -1., 0.); (0., -2., 0.);
      (-2., 0., 0.); (2., 0., 0.);
      (0., 0., 0.); (0., 1., 0.); (0., 0., 1.);
    |] in
  assert_sign Rdk.Predicates.Positive
    (Point.incircle_xy points.(0) points.(1) points.(2) points.(3));
  assert_sign Rdk.Predicates.Zero
    (Point.incircle_xy points.(0) points.(1) points.(2) points.(4));
  assert_sign Rdk.Predicates.Negative
    (Point.incircle_xy points.(0) points.(1) points.(2) points.(5));
  assert_sign Rdk.Predicates.Negative
    (Point.incircle_xy points.(2) points.(1) points.(0) points.(3));
  let lpi = expect_ok (Point.line_plane source
      ~line_start:6 ~line_end:7 ~plane_a:8 ~plane_b:9 ~plane_c:10)
  and tpi = expect_ok (Point.triple_plane source
      ~first_a:8 ~first_b:9 ~first_c:10
      ~second_a:8 ~second_b:0 ~second_c:10
      ~third_a:8 ~third_b:0 ~third_c:1) in
  assert_sign Rdk.Predicates.Positive
    (Point.incircle_xy points.(0) points.(1) points.(2) lpi);
  assert_sign Rdk.Predicates.Positive
    (Point.incircle_xy points.(0) points.(1) points.(2) tpi);
  let _, yz = incircle_points [|
      (0., 1., 0.); (0., 0., 1.); (0., -1., 0.);
      (0., 0., 0.); (0., 0., -1.); (0., 0., -2.);
    |] in
  assert_sign Rdk.Predicates.Positive
    (Point.incircle_yz yz.(0) yz.(1) yz.(2) yz.(3));
  assert_sign Rdk.Predicates.Zero
    (Point.incircle_yz yz.(0) yz.(1) yz.(2) yz.(4));
  assert_sign Rdk.Predicates.Negative
    (Point.incircle_yz yz.(0) yz.(1) yz.(2) yz.(5));
  let _, zx = incircle_points [|
      (0., 0., 1.); (1., 0., 0.); (0., 0., -1.);
      (0., 0., 0.); (-1., 0., 0.); (-2., 0., 0.);
    |] in
  assert_sign Rdk.Predicates.Positive
    (Point.incircle_zx zx.(0) zx.(1) zx.(2) zx.(3));
  assert_sign Rdk.Predicates.Zero
    (Point.incircle_zx zx.(0) zx.(1) zx.(2) zx.(4));
  assert_sign Rdk.Predicates.Negative
    (Point.incircle_zx zx.(0) zx.(1) zx.(2) zx.(5));
  Array.iter (fun point ->
    if Point.incircle_xy points.(0) points.(1) points.(2) point
        <> Point.incircle_reference_xy points.(0) points.(1) points.(2) point then
      fail "packed XY incircle differs from immutable reference")
    [|points.(3); points.(4); points.(5); lpi; tpi|];
  for point = 3 to 5 do
    if Point.incircle_yz yz.(0) yz.(1) yz.(2) yz.(point)
        <> Point.incircle_reference_yz yz.(0) yz.(1) yz.(2) yz.(point) then
      fail "packed YZ incircle differs from immutable reference";
    if Point.incircle_zx zx.(0) zx.(1) zx.(2) zx.(point)
        <> Point.incircle_reference_zx zx.(0) zx.(1) zx.(2) zx.(point) then
      fail "packed ZX incircle differs from immutable reference"
  done

let incircle_integer_sign ax ay bx by cx cy dx dy =
  let open Int64 in
  let ax = of_int (ax - dx) and ay = of_int (ay - dy)
  and bx = of_int (bx - dx) and by = of_int (by - dy)
  and cx = of_int (cx - dx) and cy = of_int (cy - dy) in
  let lift x y = add (mul x x) (mul y y)
  and cross ax ay bx by = sub (mul ax by) (mul ay bx) in
  let determinant = add
      (add
         (mul (lift ax ay) (cross bx by cx cy))
         (mul (lift bx by) (cross cx cy ax ay)))
      (mul (lift cx cy) (cross ax ay bx by)) in
  if determinant < 0L then Rdk.Predicates.Negative
  else if determinant > 0L then Rdk.Predicates.Positive
  else Rdk.Predicates.Zero

let test_incircle_integer_oracle () =
  for index = 0 to 4_999 do
    let ax = deterministic_value 17 3 index
    and ay = deterministic_value 19 5 index
    and bx = deterministic_value 23 7 index
    and by = deterministic_value 29 11 index
    and cx = deterministic_value 31 13 index
    and cy = deterministic_value 37 17 index
    and dx = deterministic_value 41 19 index
    and dy = deterministic_value 43 23 index in
    let expected = incircle_integer_sign ax ay bx by cx cy dx dy in
    let first = Array.map float_of_int [|ax; bx; cx; dx|]
    and second = Array.map float_of_int [|ay; by; cy; dy|]
    and zero = Array.make 4 0. in
    let xy_source = expect_ok (Point.source ~x:first ~y:second ~z:zero) in
    let xy = Array.init 4 (fun i -> expect_ok (Point.explicit xy_source i)) in
    if Point.incircle_xy xy.(0) xy.(1) xy.(2) xy.(3) <> expected then
      fail "filtered XY incircle disagreed with integer oracle at %d" index;
    if Point.incircle_reference_xy xy.(0) xy.(1) xy.(2) xy.(3) <> expected then
      fail "reference XY incircle disagreed with integer oracle at %d" index;
    let yz_source = expect_ok (Point.source ~x:zero ~y:first ~z:second) in
    let yz = Array.init 4 (fun i -> expect_ok (Point.explicit yz_source i)) in
    if Point.incircle_yz yz.(0) yz.(1) yz.(2) yz.(3) <> expected then
      fail "filtered YZ incircle disagreed with integer oracle at %d" index;
    if Point.incircle_reference_yz yz.(0) yz.(1) yz.(2) yz.(3) <> expected then
      fail "reference YZ incircle disagreed with integer oracle at %d" index;
    let zx_source = expect_ok (Point.source ~x:second ~y:zero ~z:first) in
    let zx = Array.init 4 (fun i -> expect_ok (Point.explicit zx_source i)) in
    if Point.incircle_zx zx.(0) zx.(1) zx.(2) zx.(3) <> expected then
      fail "filtered ZX incircle disagreed with integer oracle at %d" index;
    if Point.incircle_reference_zx zx.(0) zx.(1) zx.(2) zx.(3) <> expected then
      fail "reference ZX incircle disagreed with integer oracle at %d" index
  done

let test_exact_ray_direction () =
  let coordinates = [|
    (0., 0.25, 0.25);
    (1., 0., 0.);
    (1., 1., 0.);
    (1., 0., 1.);
  |] in
  let source = expect_ok (Point.source
      ~x:(Array.map (fun (x, _, _) -> x) coordinates)
      ~y:(Array.map (fun (_, y, _) -> y) coordinates)
      ~z:(Array.map (fun (_, _, z) -> z) coordinates)) in
  let points = Array.init 4 (fun index -> expect_ok (Point.explicit source index)) in
  let query = points.(0) and a = points.(1) and b = points.(2) and c = points.(3) in
  assert_sign Rdk.Predicates.Positive
    (Point.normal_dot_direction a b c ~dx:1. ~dy:0. ~dz:0.);
  assert_sign Rdk.Predicates.Negative
    (Point.normal_dot_direction a b c ~dx:(-1.) ~dy:0. ~dz:0.);
  assert_sign Rdk.Predicates.Positive (Point.normal_dot_symbolic a b c);
  assert_sign Rdk.Predicates.Positive
    (Point.ray_edge_symbolic ~query ~first:a ~second:b);
  (match Point.symbolic_ray_triangle ~query ~first:a ~second:b ~third:c with
   | Point.Symbolic_hit Rdk.Predicates.Positive -> ()
   | _ -> fail "positive-infinitesimal ray missed a transverse triangle");
  assert_sign (Point.orient2d_yz a b query)
    (Point.ray_edge ~query ~first:a ~second:b ~dx:1. ~dy:0. ~dz:0.);
  assert_sign (Point.orient2d_yz b c query)
    (Point.ray_edge ~query ~first:b ~second:c ~dx:1. ~dy:0. ~dz:0.);
  assert_sign (Point.orient2d_yz c a query)
    (Point.ray_edge ~query ~first:c ~second:a ~dx:1. ~dy:0. ~dz:0.);
  for index = 0 to 1_999 do
    let direction multiplier offset =
      let value = deterministic_value multiplier offset index in
      if value = 0 then 0.5 else float_of_int value in
    let dx = direction 17 3 and dy = direction 23 5
    and dz = direction 31 7 in
    Array.iter (fun (first, second) ->
      let arena = Point.ray_edge ~query ~first ~second ~dx ~dy ~dz
      and reference = Point.ray_edge_reference
          ~query ~first ~second ~dx ~dy ~dz in
      if arena <> reference then
        fail "packed exact ray-edge predicate differed at %d" index)
      [|(a, b); (b, c); (c, a)|];
    if Point.normal_dot_direction a b c ~dx ~dy ~dz
        <> Point.normal_dot_direction_reference a b c ~dx ~dy ~dz then
      fail "packed exact normal-direction predicate differed at %d" index;
    Array.iter (fun (first, second) ->
      if Point.ray_edge_symbolic ~query ~first ~second
          <> Point.ray_edge_symbolic_reference ~query ~first ~second then
        fail "packed symbolic ray-edge predicate differed at %d" index)
      [|(a, b); (b, c); (c, a)|];
    if Point.normal_dot_symbolic a b c
        <> Point.normal_dot_symbolic_reference a b c then
      fail "packed symbolic normal predicate differed at %d" index;
    if Point.radial_dot a b query c
        <> Point.radial_dot_reference a b query c then
      fail "packed exact radial predicate differed at %d" index
  done;
  let expect_invalid thunk = match thunk () with
    | _ -> fail "non-finite exact direction was accepted"
    | exception Invalid_argument _ -> () in
  expect_invalid (fun () ->
    Point.ray_edge ~query ~first:a ~second:b
      ~dx:Float.nan ~dy:0. ~dz:0.);
  expect_invalid (fun () ->
    Point.normal_dot_direction a b c
      ~dx:0. ~dy:Float.infinity ~dz:0.);
  let axis_source = expect_ok (Point.source
      ~x:[|0.; 0.; 1.; 0.; 2.; 0.; 2.|]
      ~y:[|0.; 0.; 0.; 1.; 0.; 0.; 2.|]
      ~z:[|1.; 0.; 0.; 0.; 0.; -1.; -1.|]) in
  let axis = Array.init 7 (fun index ->
      expect_ok (Point.explicit axis_source index)) in
  let axis_direction axis positive =
    let value = if positive then 1. else -1. in
    if axis = 0 then value, 0., 0.
    else if axis = 1 then 0., value, 0.
    else 0., 0., value in
  let axis_reference query first second third axis positive =
    let dx, dy, dz = axis_direction axis positive in
    let normal = Point.normal_dot_direction first second third ~dx ~dy ~dz in
    if normal = Rdk.Predicates.Zero then Point.Axis_parallel
    else begin
      let first_edge = Point.ray_edge ~query ~first ~second ~dx ~dy ~dz
      and second_edge = Point.ray_edge ~query ~first:second ~second:third
          ~dx ~dy ~dz
      and third_edge = Point.ray_edge ~query ~first:third ~second:first
          ~dx ~dy ~dz
      and plane = Point.orient3d first second third query in
      let opposite left right =
        left <> Rdk.Predicates.Zero && right <> Rdk.Predicates.Zero
        && left <> right in
      if opposite first_edge normal || opposite second_edge normal
          || opposite third_edge normal then Point.Axis_miss
      else if plane = Rdk.Predicates.Zero then Point.Axis_boundary
      else if plane <> normal then Point.Axis_miss
      else if first_edge = Rdk.Predicates.Zero
          || second_edge = Rdk.Predicates.Zero
          || third_edge = Rdk.Predicates.Zero then Point.Axis_boundary
      else Point.Axis_hit normal
    end in
  let check_axis_direct query a b c =
    for axis_index = 0 to 2 do
      List.iter (fun positive ->
          let reference = axis_reference query axis.(a) axis.(b) axis.(c)
              axis_index positive
          and direct = Point.axis_ray_source_triangle axis_source
              ~a ~b ~c ~axis:axis_index ~positive query in
          if direct <> reference then
            fail "packed source axis ray/triangle decision differs from reference")
        [false; true]
    done in
  assert_sign Rdk.Predicates.Positive
    (Point.normal_dot_symbolic axis.(1) axis.(2) axis.(3));
  assert_sign Rdk.Predicates.Negative
    (Point.ray_edge_symbolic ~query:axis.(0)
       ~first:axis.(1) ~second:axis.(2));
  assert_sign Rdk.Predicates.Zero
    (Point.normal_dot_symbolic axis.(1) axis.(2) axis.(4));
  assert_sign Rdk.Predicates.Zero
    (Point.ray_edge_symbolic ~query:axis.(1)
       ~first:axis.(2) ~second:axis.(4));
  (match Point.symbolic_ray_triangle ~query:axis.(5)
      ~first:axis.(1) ~second:axis.(2) ~third:axis.(3) with
   | Point.Symbolic_miss -> ()
   | _ -> fail "positive-infinitesimal ray accepted a far lateral crossing");
  (match Point.symbolic_ray_triangle ~query:axis.(0)
      ~first:axis.(1) ~second:axis.(2) ~third:axis.(3) with
   | Point.Symbolic_miss -> ()
   | _ -> fail "positive-infinitesimal ray accepted a triangle behind it");
  (match Point.symbolic_ray_triangle ~query:axis.(6)
      ~first:axis.(1) ~second:axis.(2) ~third:axis.(3) with
   | Point.Symbolic_miss -> ()
   | _ -> fail "positive-infinitesimal ray accepted a lateral miss");
  (match Point.symbolic_ray_triangle ~query:axis.(1)
      ~first:axis.(1) ~second:axis.(2) ~third:axis.(3) with
   | Point.Symbolic_origin_boundary -> ()
   | _ -> fail "positive-infinitesimal ray lost its origin-boundary case");
  (match Point.symbolic_ray_triangle ~query:axis.(0)
      ~first:axis.(1) ~second:axis.(2) ~third:axis.(4) with
   | Point.Symbolic_degenerate -> ()
   | _ -> fail "positive-infinitesimal ray accepted a degenerate triangle");
  let check_direct query a b c =
    let reference = Point.symbolic_ray_triangle ~query
        ~first:axis.(a) ~second:axis.(b) ~third:axis.(c)
    and direct = Point.symbolic_ray_source_triangle axis_source ~a ~b ~c query in
    if direct <> reference then
      fail "packed source symbolic ray/triangle decision differs from reference" in
  check_direct axis.(5) 1 2 3;
  check_direct axis.(0) 1 2 3;
  check_direct axis.(6) 1 2 3;
  check_direct axis.(1) 1 2 3;
  check_direct axis.(0) 1 2 4;
  Array.iter (fun query ->
      check_axis_direct query 1 2 3;
      check_axis_direct query 1 2 4)
    axis

let test_axis_ray_extreme_ranges () =
  let exercise scale =
    let source = expect_ok (Point.source
        ~x:[|0.; scale; scale; scale|]
        ~y:[|0.25 *. scale; 0.; scale; 0.|]
        ~z:[|0.25 *. scale; 0.; 0.; scale|]) in
    let points = Array.init 4 (fun index -> expect_ok (Point.explicit source index)) in
    let query = points.(0) and first = points.(1)
    and second = points.(2) and third = points.(3) in
    for axis = 0 to 2 do
      List.iter (fun positive ->
          let value = if positive then 1. else -1. in
          let dx, dy, dz = if axis = 0 then value, 0., 0.
            else if axis = 1 then 0., value, 0. else 0., 0., value in
          let normal = Point.normal_dot_direction first second third
              ~dx ~dy ~dz in
          let reference = if normal = Rdk.Predicates.Zero then Point.Axis_parallel
            else begin
              let e0 = Point.ray_edge ~query ~first ~second ~dx ~dy ~dz
              and e1 = Point.ray_edge ~query ~first:second ~second:third
                  ~dx ~dy ~dz
              and e2 = Point.ray_edge ~query ~first:third ~second:first
                  ~dx ~dy ~dz
              and plane = Point.orient3d first second third query in
              let opposite left right = left <> Rdk.Predicates.Zero
                  && right <> Rdk.Predicates.Zero && left <> right in
              if opposite e0 normal || opposite e1 normal || opposite e2 normal
              then Point.Axis_miss
              else if plane = Rdk.Predicates.Zero then Point.Axis_boundary
              else if plane <> normal then Point.Axis_miss
              else if e0 = Rdk.Predicates.Zero || e1 = Rdk.Predicates.Zero
                  || e2 = Rdk.Predicates.Zero then Point.Axis_boundary
              else Point.Axis_hit normal
            end in
          let direct = Point.axis_ray_source_triangle source
              ~a:1 ~b:2 ~c:3 ~axis ~positive query in
          if direct <> reference then
            fail "extreme-range packed source axis decision differs")
        [false; true]
    done in
  exercise 1e-300;
  exercise (Float.max_float /. 4.)

let test_mixed_ray_radial_differential source lpi tpi =
  let points = [|
    expect_ok (Point.explicit source 0);
    expect_ok (Point.explicit source 1);
    expect_ok (Point.explicit source 4);
    expect_ok (Point.explicit source 16);
    expect_ok (Point.explicit source 17);
    lpi;
    tpi;
  |] in
  for index = 0 to 999 do
    let first = points.(index mod Array.length points)
    and second = points.((index * 3 + 1) mod Array.length points)
    and third = points.((index * 5 + 2) mod Array.length points)
    and fourth = points.((index * 7 + 3) mod Array.length points) in
    if Point.radial_dot first second third fourth
        <> Point.radial_dot_reference first second third fourth then
      fail "mixed implicit packed radial predicate differed at %d" index;
    let component multiplier offset =
      let value = deterministic_value multiplier offset index in
      if value = 0 then 0.25 else float_of_int value in
    let dx = component 11 1 and dy = component 17 3
    and dz = component 29 5 in
    if Point.ray_edge ~query:first ~first:second ~second:third ~dx ~dy ~dz
        <> Point.ray_edge_reference
             ~query:first ~first:second ~second:third ~dx ~dy ~dz then
      fail "mixed implicit packed ray-edge predicate differed at %d" index;
    if Point.normal_dot_direction first second third ~dx ~dy ~dz
        <> Point.normal_dot_direction_reference first second third ~dx ~dy ~dz then
      fail "mixed implicit packed normal predicate differed at %d" index;
    if Point.ray_edge_symbolic ~query:first ~first:second ~second:third
        <> Point.ray_edge_symbolic_reference
             ~query:first ~first:second ~second:third then
      fail "mixed implicit symbolic ray predicate differed at %d" index;
    if Point.normal_dot_symbolic first second third
        <> Point.normal_dot_symbolic_reference first second third then
      fail "mixed implicit symbolic normal predicate differed at %d" index
  done

let ray_radial_signature domains source lpi tpi =
  let points = [|
    expect_ok (Point.explicit source 0);
    expect_ok (Point.explicit source 1);
    expect_ok (Point.explicit source 16);
    expect_ok (Point.explicit source 17);
    lpi;
    tpi;
  |] in
  let output = Array.make 2_048 0 in
  Parallel.run ~domains (fun () ->
    Parallel.for_ ~chunk_size:31 ~start:0
      ~finish:(Array.length output - 1) (fun index ->
        let a = points.(index mod Array.length points)
        and b = points.((index * 3 + 1) mod Array.length points)
        and c = points.((index * 5 + 2) mod Array.length points)
        and d = points.((index * 7 + 3) mod Array.length points) in
        let dx = float_of_int (deterministic_value 11 1 index)
        and dy = float_of_int (deterministic_value 17 3 index)
        and dz = float_of_int (deterministic_value 29 5 index) in
        let sign = function Rdk.Predicates.Negative -> -1
          | Rdk.Predicates.Zero -> 0 | Rdk.Predicates.Positive -> 1 in
        output.(index) <-
          (31 * sign (Point.radial_dot a b c d))
          + (17 * sign (Point.ray_edge
              ~query:a ~first:b ~second:c ~dx ~dy ~dz))
          + (13 * sign (Point.normal_dot_direction a b c ~dx ~dy ~dz))
          + (7 * sign (Point.ray_edge_symbolic
              ~query:a ~first:b ~second:c))
          + (5 * sign (Point.normal_dot_symbolic a b c))));
  output

let test_ray_radial_domain_exactness source lpi tpi =
  let one = ray_radial_signature 1 source lpi tpi
  and four = ray_radial_signature 4 source lpi tpi in
  if one <> four then
    fail "packed ray/radial predicates differ between one and four domains"

let test_exact_barycentric () =
  let source = expect_ok (Point.source
      ~x:[|0.; 2.; 0.; 0.|]
      ~y:[|0.; 0.; 2.; 0.|]
      ~z:[|0.; 0.; 0.; 1.|]) in
  let a = expect_ok (Point.explicit source 0)
  and b = expect_ok (Point.explicit source 1)
  and c = expect_ok (Point.explicit source 2) in
  let centroid = expect_ok (Point.centroid3 a b c) in
  let wa, wb, wc = Point.barycentric a b c centroid in
  assert_close (1. /. 3.) wa;
  assert_close (1. /. 3.) wb;
  assert_close (1. /. 3.) wc;
  let wa, wb, wc = Point.barycentric a b c b in
  assert_close 0. wa; assert_close 1. wb; assert_close 0. wc;
  let direct = Point.barycentric_source_triangle source ~a:0 ~b:1 ~c:2 centroid in
  let dwa, dwb, dwc = direct in
  assert_close (1. /. 3.) dwa; assert_close (1. /. 3.) dwb;
  assert_close (1. /. 3.) dwc;
  let dwa, dwb, dwc = Point.barycentric_source_triangle source
      ~a:0 ~b:1 ~c:2 b in
  assert_close 0. dwa; assert_close 1. dwb; assert_close 0. dwc;
  let outside = expect_ok (Point.explicit source 3) in
  (match Point.barycentric a b c outside with
   | _ -> fail "non-coplanar barycentric query was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.barycentric_source_triangle source ~a:0 ~b:1 ~c:2 outside with
   | _ -> fail "direct non-coplanar barycentric query was accepted"
   | exception Invalid_argument _ -> ())

let test_source_feature_queries source lpi tpi =
  let projection = Point.source_triangle_projection source 0 1 9 in
  if projection <> Point.XY then fail "source triangle selected the wrong projection";
  let reference first second point =
    let a = expect_ok (Point.explicit source first)
    and b = expect_ok (Point.explicit source second) in
    let orientation = match projection with
      | Point.XY -> Point.orient2d_xy a b point
      | Point.YZ -> Point.orient2d_yz a b point
      | Point.ZX -> Point.orient2d_zx a b point in
    let between compare =
      if compare a b <= 0 then compare a point <= 0 && compare point b <= 0
      else compare b point <= 0 && compare point a <= 0 in
    orientation = Rdk.Predicates.Zero
    && if Point.compare_x a b <> 0 then between Point.compare_x
       else if Point.compare_y a b <> 0 then between Point.compare_y
       else between Point.compare_z in
  let points = [|lpi; tpi; expect_ok (Point.explicit source 0);
    expect_ok (Point.explicit source 1); expect_ok (Point.explicit source 18)|] in
  Array.iter (fun point ->
      let direct = Point.source_segment_contains source ~projection
          ~first:0 ~second:1 point in
      if direct <> reference 0 1 point then
        fail "packed source-segment query differs from explicit reference") points;
  let a = expect_ok (Point.explicit source 0)
  and b = expect_ok (Point.explicit source 1)
  and c = expect_ok (Point.explicit source 9) in
  Array.iter (fun point ->
      let wa, wb, wc = Point.barycentric a b c point
      and dwa, dwb, dwc = Point.barycentric_source_triangle source
          ~a:0 ~b:1 ~c:9 point in
      assert_close wa dwa; assert_close wb dwb; assert_close wc dwc)
    [|lpi; expect_ok (Point.explicit source 0);
      expect_ok (Point.explicit source 1); expect_ok (Point.explicit source 5)|];
  Array.iter (fun query ->
      let reference = Point.symbolic_ray_triangle ~query
          ~first:a ~second:b ~third:c
      and direct = Point.symbolic_ray_source_triangle source
          ~a:0 ~b:1 ~c:9 query in
      if direct <> reference then
        fail "mixed implicit packed source symbolic decision differs")
    [|lpi; tpi; expect_ok (Point.explicit source 0);
      expect_ok (Point.explicit source 18)|];
  let axis_reference query first second third axis positive =
    let dx, dy, dz =
      let value = if positive then 1. else -1. in
      if axis = 0 then value, 0., 0.
      else if axis = 1 then 0., value, 0.
      else 0., 0., value in
    let normal = Point.normal_dot_direction first second third ~dx ~dy ~dz in
    if normal = Rdk.Predicates.Zero then Point.Axis_parallel
    else begin
      let e0 = Point.ray_edge ~query ~first ~second ~dx ~dy ~dz
      and e1 = Point.ray_edge ~query ~first:second ~second:third ~dx ~dy ~dz
      and e2 = Point.ray_edge ~query ~first:third ~second:first ~dx ~dy ~dz
      and plane = Point.orient3d first second third query in
      let opposite left right = left <> Rdk.Predicates.Zero
          && right <> Rdk.Predicates.Zero && left <> right in
      if opposite e0 normal || opposite e1 normal || opposite e2 normal then
        Point.Axis_miss
      else if plane = Rdk.Predicates.Zero then Point.Axis_boundary
      else if plane <> normal then Point.Axis_miss
      else if e0 = Rdk.Predicates.Zero || e1 = Rdk.Predicates.Zero
          || e2 = Rdk.Predicates.Zero then Point.Axis_boundary
      else Point.Axis_hit normal
    end in
  Array.iter (fun query ->
      for axis = 0 to 2 do
        List.iter (fun positive ->
            let direct = Point.axis_ray_source_triangle source
                ~a:0 ~b:1 ~c:9 ~axis ~positive query in
            if direct <> axis_reference query a b c axis positive then
              fail "mixed implicit packed source axis decision differs")
          [false; true]
      done)
    [|lpi; tpi; expect_ok (Point.explicit source 0);
      expect_ok (Point.explicit source 18)|];
  let queries = [|lpi; tpi; expect_ok (Point.explicit source 0);
    expect_ok (Point.explicit source 18)|] in
  for index = 0 to 1_999 do
    let ai = (index * 17 + 1) mod Array.length x
    and bi = (index * 29 + 5) mod Array.length x
    and ci = (index * 43 + 9) mod Array.length x
    and query = queries.(index land 3) in
    let first = expect_ok (Point.explicit source ai)
    and second = expect_ok (Point.explicit source bi)
    and third = expect_ok (Point.explicit source ci) in
    for axis = 0 to 2 do
      List.iter (fun positive ->
          let direct = Point.axis_ray_source_triangle source
              ~a:ai ~b:bi ~c:ci ~axis ~positive query
          and reference = axis_reference query first second third axis positive in
          if direct <> reference then
            fail "randomized packed source axis decision differs at %d" index)
        [false; true]
    done
  done;
  (match Point.source_triangle_projection source 0 1 5 with
   | _ -> fail "degenerate source triangle selected a projection"
   | exception Invalid_argument _ -> ());
  (match Point.source_segment_contains source ~projection ~first:(-1) ~second:1 lpi with
   | _ -> fail "invalid source segment index was accepted"
   | exception Invalid_argument _ -> ());
  let other_source = expect_ok (Point.source ~x:[|0.|] ~y:[|0.|] ~z:[|0.|]) in
  (match Point.source_segment_contains other_source ~projection
      ~first:0 ~second:0 lpi with
   | _ -> fail "cross-source segment query was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.barycentric_source_triangle source ~a:(-1) ~b:1 ~c:9 lpi with
   | _ -> fail "invalid direct barycentric source index was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.barycentric_source_triangle other_source ~a:0 ~b:0 ~c:0 lpi with
   | _ -> fail "cross-source direct barycentric query was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.symbolic_ray_source_triangle source ~a:(-1) ~b:1 ~c:9 lpi with
   | _ -> fail "invalid direct symbolic source index was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.symbolic_ray_source_triangle other_source ~a:0 ~b:0 ~c:0 lpi with
   | _ -> fail "cross-source direct symbolic query was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.axis_ray_source_triangle source ~a:0 ~b:1 ~c:9
      ~axis:3 ~positive:true lpi with
   | _ -> fail "invalid direct axis was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.axis_ray_source_triangle source ~a:(-1) ~b:1 ~c:9
      ~axis:0 ~positive:true lpi with
   | _ -> fail "invalid direct axis source index was accepted"
   | exception Invalid_argument _ -> ());
  (match Point.axis_ray_source_triangle other_source ~a:0 ~b:0 ~c:0
      ~axis:0 ~positive:true lpi with
   | _ -> fail "cross-source direct axis query was accepted"
   | exception Invalid_argument _ -> ())

let run () =
  test_source_validation ();
  let source = expect_ok (Point.source ~x ~y ~z) in
  expect_error (Point.Index_out_of_bounds (-1)) (Point.explicit source (-1));
  let lpi = test_line_plane source in
  let tpi = test_triple_plane source in
  test_line_line ();
  test_randomized_line_line ();
  test_ill_conditioned_line_line ();
  test_centroid ();
  test_midpoint_and_circumcenter ();
  test_mixed_predicates source lpi tpi;
  test_randomized_constructions ();
  test_domain_exactness ();
  test_incircle ();
  test_incircle_integer_oracle ();
  test_exact_ray_direction ();
  test_axis_ray_extreme_ranges ();
  test_mixed_ray_radial_differential source lpi tpi;
  test_ray_radial_domain_exactness source lpi tpi;
  test_exact_barycentric ();
  test_source_feature_queries source lpi tpi
