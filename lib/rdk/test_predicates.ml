open Rdk
open Rdk_test_support

let opposite = function
  | Predicates.Negative -> Predicates.Positive
  | Predicates.Zero -> Predicates.Zero
  | Predicates.Positive -> Predicates.Negative

let check_orient2d expected (ax, ay) (bx, by) (cx, cy) message =
  let actual = Predicates.orient2d ~ax ~ay ~bx ~by ~cx ~cy in
  check (actual = expected) message;
  let reversed = Predicates.orient2d ~ax:bx ~ay:by ~bx:ax ~by:ay ~cx ~cy in
  check (reversed = opposite expected) (message ^ " (antisymmetry)")

let check_orient3d expected (ax, ay, az) (bx, by, bz) (cx, cy, cz)
    (dx, dy, dz) message =
  let actual = Predicates.orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz
      ~dx ~dy ~dz in
  check (actual = expected) message;
  let reversed = Predicates.orient3d ~ax:bx ~ay:by ~az:bz ~bx:ax ~by:ay
      ~bz:az ~cx ~cy ~cz ~dx ~dy ~dz in
  check (reversed = opposite expected) (message ^ " (antisymmetry)")

let check_incircle expected (ax, ay) (bx, by) (cx, cy) (dx, dy) message =
  let actual = Predicates.incircle ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy in
  check (actual = expected) message;
  let reversed = Predicates.incircle ~ax:bx ~ay:by ~bx:ax ~by:ay
      ~cx ~cy ~dx ~dy in
  check (reversed = opposite expected) (message ^ " (orientation reversal)")

let invalid operation =
  match operation () with
  | exception Invalid_argument _ -> true
  | _ -> false

let check_segment expected x y z p q a b c message =
  let actual = Predicates.segment_triangle_packed ~x ~y ~z
      ~segment_start:p ~segment_end:q ~triangle_a:a ~triangle_b:b
      ~triangle_c:c in
  check (actual = expected) message

let check_segment_segment expected x y z left_a left_b right_a right_b message =
  let actual = Predicates.segment_segment_packed ~x ~y ~z
      ~left_a ~left_b ~right_a ~right_b in
  check (actual = expected) message;
  let reversed = Predicates.segment_segment_packed ~x ~y ~z
      ~left_a:right_b ~left_b:right_a ~right_a:left_b ~right_b:left_a in
  check (reversed = expected) (message ^ " (operand/endpoint reversal)")

let triangle_event_count x y z la lb lc ra rb rc =
  let output = Array.make 4 (-1) in
  Predicates.Private.triangle_triangle_features_into ~x ~y ~z
    ~left_a:la ~left_b:lb ~left_c:lc ~right_a:ra ~right_b:rb ~right_c:rc
    output, output

let sign_of_int64 value =
  if value < 0L then Predicates.Negative
  else if value > 0L then Predicates.Positive
  else Predicates.Zero

let planar_segment_oracle ax ay bx by cx cy dx dy =
  let i = Int64.of_int and sub = Int64.sub and mul = Int64.mul in
  let orient ax ay bx by cx cy =
    sub (mul (sub (i ax) (i cx)) (sub (i by) (i cy)))
      (mul (sub (i ay) (i cy)) (sub (i bx) (i cx))) in
  if ax = bx && ay = by || cx = dx && cy = dy then
    Predicates.Segments_degenerate
  else
    let first = orient ax ay bx by cx cy
    and second = orient ax ay bx by dx dy
    and third = orient cx cy dx dy ax ay
    and fourth = orient cx cy dx dy bx by in
    if first = 0L && second = 0L && third = 0L && fourth = 0L then begin
      let a, b, c, d = if ax <> bx || cx <> dx then ax, bx, cx, dx
        else ay, by, cy, dy in
      let lower = max (min a b) (min c d) and upper = min (max a b) (max c d) in
      if lower > upper then Predicates.Segments_disjoint
      else if lower = upper then Predicates.Segments_point
      else Predicates.Segments_overlap
    end else begin
      let opposite left right = left < 0L && right > 0L
          || left > 0L && right < 0L in
      let on_segment ax ay bx by px py =
        px >= min ax bx && px <= max ax bx && py >= min ay by && py <= max ay by in
      if opposite first second && opposite third fourth
          || first = 0L && on_segment ax ay bx by cx cy
          || second = 0L && on_segment ax ay bx by dx dy
          || third = 0L && on_segment cx cy dx dy ax ay
          || fourth = 0L && on_segment cx cy dx dy bx by
      then Predicates.Segments_point else Predicates.Segments_disjoint
    end

let deterministic_coordinate index salt extent =
  let mixed = (index * 1_103_515_245) + (salt * 97_531) + 12_345 in
  (mixed mod ((2 * extent) + 1)) - extent

let scaled_coordinate index salt =
  let exponents = [|-1074; -1022; -700; -300; -53; -1; 0; 17; 300; 700; 1023|] in
  let mixed = (index * 1_664_525) + (salt * 1_013_904_223) + 97 in
  let exponent = exponents.((mixed land max_int) mod Array.length exponents) in
  let signed = ((mixed lsr 5) mod 2047) - 1023 in
  let mantissa = if signed = 0 then 1. else float_of_int signed /. 1024. in
  Float.ldexp mantissa exponent

let run () =
  check_orient2d Predicates.Positive (0., 0.) (1., 0.) (0., 1.)
    "ordinary orient2d";
  check_orient2d Predicates.Zero (0., 0.) (1., 1.) (2., 2.)
    "collinear orient2d";

  (* Every input is exactly representable, but the two rounded products are
     indistinguishable at the filter's certified error scale. The exact
     determinant is -1. Some compiler/architecture pairs recover that final
     unit through contraction, so the test asserts the exact answer rather
     than requiring one particular naïve intermediate. *)
  let n = Float.ldexp 1. 52 in
  check_orient2d Predicates.Negative (0., 0.) (n, n -. 1.)
    (n -. 1., n -. 2.) "cancelled orient2d exact fallback";

  let smallest = Int64.float_of_bits 1L
  and minimum_normal = Float.ldexp 1. (-1022) in
  check (minimum_normal *. smallest = 0.)
    "underflow fixture did not underflow";
  check_orient2d Predicates.Positive (0., 0.) (minimum_normal, 0.)
    (0., smallest) "underflow orient2d exact fallback";
  check_orient2d Predicates.Positive (0., 0.) (Float.max_float, 0.)
    (0., Float.max_float) "overflow orient2d exact fallback";
  let packed_x = [|0.; n; n -. 1.|]
  and packed_y = [|0.; n -. 1.; n -. 2.|] in
  check (Predicates.orient2d_packed ~x:packed_x ~y:packed_y 0 1 2
      = Predicates.Negative) "packed orient2d exact fallback";

  check_orient3d Predicates.Positive (1., 0., 0.) (0., 1., 0.)
    (0., 0., 1.) (0., 0., 0.) "ordinary orient3d";
  check_orient3d Predicates.Zero (0., 0., 0.) (1., 0., 0.)
    (0., 1., 0.) (1., 1., 0.) "coplanar orient3d";
  check_orient3d Predicates.Negative (n, n -. 1., 0.)
    (n -. 1., n -. 2., 0.) (0., 0., 1.) (0., 0., 0.)
    "cancelled orient3d exact fallback";
  check_orient3d Predicates.Positive (minimum_normal, 0., 0.)
    (0., smallest, 0.) (0., 0., smallest) (0., 0., 0.)
    "underflow orient3d exact fallback";
  let packed_z = [|0.;0.;1.;0.|] in
  check (Predicates.orient3d_packed ~x:[|n;n -. 1.;0.;0.|]
      ~y:[|n -. 1.;n -. 2.;0.;0.|] ~z:packed_z 0 1 2 3
      = Predicates.Negative) "packed orient3d exact fallback";

  check_incircle Predicates.Positive (0.,0.) (2.,0.) (0.,2.) (0.5,0.5)
    "ordinary incircle inside";
  check_incircle Predicates.Zero (0.,0.) (2.,0.) (0.,2.) (2.,2.)
    "ordinary incircle cocircular";
  check_incircle Predicates.Negative (0.,0.) (2.,0.) (0.,2.) (3.,3.)
    "ordinary incircle outside";
  let incircle_x = [|0.;2.;0.;0.5|]
  and incircle_y = [|0.;0.;2.;0.5|] in
  check (Predicates.incircle_packed ~x:incircle_x ~y:incircle_y 0 1 2 3
      = Predicates.Positive) "packed incircle";
  check_incircle Predicates.Zero (0.,0.) (Float.max_float,0.)
    (0.,Float.max_float) (Float.max_float,Float.max_float)
    "overflow incircle exact fallback";
  check_incircle Predicates.Zero (0.,0.) (smallest,0.) (0.,smallest)
    (smallest,smallest) "underflow incircle exact fallback";

  let x = [|0.;2.;0.; 0.5;0.5; 1.; 0.;2.; 3.; 1.; 0.; 1.|]
  and y = [|0.;0.;2.; 0.5;0.5; 0.; 0.;0.; 3.; 0.; 0.; 0.|]
  and z = [|0.;0.;0.; -1.;1.; -1.; -1.;-1.; 1.; 0.; 1.; -1.|] in
  check_segment
    (Predicates.Segment_hit (Predicates.Interior, Predicates.Face))
    x y z 3 4 0 1 2 "segment/triangle interior hit";
  check_segment
    (Predicates.Segment_hit (Predicates.Interior, Predicates.Edge_ab))
    x y z 5 10 0 1 2 "segment/triangle edge hit";
  check_segment
    (Predicates.Segment_hit (Predicates.Interior, Predicates.Vertex_a))
    x y z 6 10 0 1 2 "segment/triangle vertex hit";
  check_segment Predicates.Segment_disjoint x y z 7 8 0 1 2
    "segment/triangle outside crossing";
  check_segment
    (Predicates.Segment_hit (Predicates.Start, Predicates.Edge_ab))
    x y z 9 10 0 1 2 "segment/triangle start hit";
  check_segment Predicates.Segment_coplanar x y z 0 1 0 1 2
    "segment/triangle coplanar classification";
  check_segment Predicates.Segment_degenerate_triangle x y z 3 4 0 9 1
    "segment/triangle degenerate triangle";
  check_segment
    (Predicates.Segment_hit (Predicates.Interior, Predicates.Face))
    x y z 4 3 0 2 1 "segment/triangle reversal invariance";

  let sx = [|0.;2.; 1.;1.; 1.;1.; 0.;2.; 3.;4.; 2.;3.; 0.|]
  and sy = [|0.;0.; -1.;1.; -1.;1.; 0.;0.; 0.;0.; 0.;0.; 0.|]
  and sz = [|0.;0.; 0.;0.; 1.;1.; 0.;0.; 0.;0.; 0.;0.; 0.|] in
  check_segment_segment Predicates.Segments_point sx sy sz 0 1 2 3
    "coplanar transverse segment contact";
  check_segment_segment Predicates.Segments_disjoint sx sy sz 0 1 4 5
    "skew segment rejection";
  check_segment_segment Predicates.Segments_overlap sx sy sz 0 1 6 7
    "collinear segment overlap";
  check_segment_segment Predicates.Segments_disjoint sx sy sz 0 1 8 9
    "collinear segment separation";
  check_segment_segment Predicates.Segments_point sx sy sz 0 1 10 11
    "collinear endpoint contact";
  check_segment_segment Predicates.Segments_degenerate sx sy sz 0 12 2 3
    "zero-length segment classification";
  let tiny = Int64.float_of_bits 1L in
  check_segment_segment Predicates.Segments_point
    [|0.; tiny; 0.; tiny|] [|0.; tiny; tiny; 0.|] [|0.;0.;0.;0.|]
    0 1 2 3 "subnormal exact segment contact";
  check_segment_segment Predicates.Segments_point
    [|0.;0.;0.;0.|] [|0.;2.;1.;1.|] [|0.;0.;-1.;1.|]
    0 1 2 3 "YZ-projected segment contact";
  check_segment_segment Predicates.Segments_point
    [|0.;0.;-1.;1.|] [|0.;0.;0.;0.|] [|0.;2.;1.;1.|]
    0 1 2 3 "ZX-projected segment contact";
  let largest = Float.max_float in
  check_segment_segment Predicates.Segments_overlap
    [|-.largest;largest;0.;1.|] [|0.;0.;0.;0.|] [|0.;0.;0.;0.|]
    0 1 2 3 "overflow-range collinear overlap";

  let tx = [|0.;2.;0.; 0.5;0.5;0.5; 4.;5.;4.; 0.;1.;2.|]
  and ty = [|0.;0.;2.; -0.5;1.5;1.5; 4.;4.;5.; 0.;0.;0.|]
  and tz = [|0.;0.;0.; -1.;1.;-1.; 1.;2.;1.; 0.;0.;0.|] in
  let count, features = triangle_event_count tx ty tz 0 1 2 3 4 5 in
  check (count = 2 && features.(0) <= features.(2))
    "triangle/triangle segment features";
  check (match Predicates.triangle_triangle_packed ~x:tx ~y:ty ~z:tz
      ~left_a:0 ~left_b:1 ~left_c:2 ~right_a:3 ~right_b:4 ~right_c:5 with
    | Predicates.Triangle_segment _ -> true | _ -> false)
    "triangle/triangle readable segment result";
  let count, _ = triangle_event_count tx ty tz 0 1 2 6 7 8 in
  check (count = 0) "triangle/triangle disjoint result";
  let count, _ = triangle_event_count tx ty tz 0 1 2 0 1 2 in
  check (count = -1) "triangle/triangle coplanar result";
  let coplanar_contact la lb lc ra rb rc =
    Predicates.Private.coplanar_triangles_contact_packed ~x:tx ~y:ty ~z:tz
      ~left_a:la ~left_b:lb ~left_c:lc ~right_a:ra ~right_b:rb ~right_c:rc in
  check (coplanar_contact 0 1 2 0 1 2)
    "identical coplanar triangles do not contact";
  check (not (coplanar_contact 0 1 2 6 7 8))
    "disjoint coplanar triangles contact";
  let cx = [|0.;4.;0.; 1.;2.;1.; 4.;5.;4.; 4.;5.;4.|]
  and cy = [|0.;0.;4.; 1.;1.;2.; 0.;0.;1.; 0.;0.;-1.|]
  and cz = Array.make 12 0. in
  let contact la lb lc ra rb rc =
    Predicates.Private.coplanar_triangles_contact_packed ~x:cx ~y:cy ~z:cz
      ~left_a:la ~left_b:lb ~left_c:lc ~right_a:ra ~right_b:rb ~right_c:rc in
  check (contact 0 1 2 3 4 5) "contained coplanar triangle does not contact";
  check (contact 0 1 2 6 7 8) "edge-touching coplanar triangle does not contact";
  check (contact 0 1 2 9 10 11) "vertex-touching coplanar triangle does not contact";
  let count, _ = triangle_event_count tx ty tz 0 9 10 3 4 5 in
  check (count = -2) "triangle/triangle degenerate result";

  check (invalid (fun () -> Predicates.orient2d ~ax:Float.nan ~ay:0.
      ~bx:0. ~by:0. ~cx:0. ~cy:0.)) "orient2d accepted NaN";
  check (invalid (fun () -> Predicates.orient3d ~ax:Float.infinity ~ay:0.
      ~az:0. ~bx:0. ~by:0. ~bz:0. ~cx:0. ~cy:0. ~cz:0.
      ~dx:0. ~dy:0. ~dz:0.)) "orient3d accepted infinity";
  check (invalid (fun () -> Predicates.incircle ~ax:0. ~ay:0.
      ~bx:1. ~by:0. ~cx:0. ~cy:1. ~dx:Float.nan ~dy:0.))
    "incircle accepted NaN";
  check (invalid (fun () -> Predicates.segment_segment_packed
      ~x:[|Float.nan;0.;0.;1.|] ~y:[|0.;0.;1.;0.|] ~z:[|0.;0.;0.;0.|]
      ~left_a:0 ~left_b:1 ~right_a:2 ~right_b:3))
    "segment/segment accepted NaN";

  (* Independent signed-int64 oracles cover both filter and fallback dispatch
     over many exact integer configurations. The selected ranges keep every
     reference product and sum strictly inside int64. *)
  for index = 0 to 49_999 do
    let ax = deterministic_coordinate index 0 1_000_000
    and ay = deterministic_coordinate index 1 1_000_000
    and bx = deterministic_coordinate index 2 1_000_000
    and by = deterministic_coordinate index 3 1_000_000
    and cx = deterministic_coordinate index 4 1_000_000
    and cy = deterministic_coordinate index 5 1_000_000 in
    let i value = Int64.of_int value in
    let determinant = Int64.sub
        (Int64.mul (Int64.sub (i ax) (i cx)) (Int64.sub (i by) (i cy)))
        (Int64.mul (Int64.sub (i ay) (i cy)) (Int64.sub (i bx) (i cx))) in
    let actual = Predicates.orient2d
        ~ax:(float ax) ~ay:(float ay) ~bx:(float bx) ~by:(float by)
        ~cx:(float cx) ~cy:(float cy) in
    check (actual = sign_of_int64 determinant)
      "orient2d disagrees with the int64 oracle"
  done;
  for index = 0 to 19_999 do
    let coordinate salt = deterministic_coordinate index salt 10_000 in
    let ax = coordinate 0 and ay = coordinate 1
    and bx = coordinate 2 and by = coordinate 3
    and cx = coordinate 4 and cy = coordinate 5
    and dx = coordinate 6 and dy = coordinate 7 in
    let expected = planar_segment_oracle ax ay bx by cx cy dx dy
    and actual = Predicates.segment_segment_packed
        ~x:[|float ax;float bx;float cx;float dx|]
        ~y:[|float ay;float by;float cy;float dy|]
        ~z:[|0.;0.;0.;0.|]
        ~left_a:0 ~left_b:1 ~right_a:2 ~right_b:3 in
    check (actual = expected)
      "segment/segment disagrees with the planar int64 oracle"
  done;
  for index = 0 to 19_999 do
    let coordinate salt = deterministic_coordinate index salt 1_000 in
    let ax = coordinate 0 and ay = coordinate 1
    and bx = coordinate 2 and by = coordinate 3
    and cx = coordinate 4 and cy = coordinate 5
    and dx = coordinate 6 and dy = coordinate 7 in
    let i = Int64.of_int and sub = Int64.sub and mul = Int64.mul in
    let adx = sub (i ax) (i dx) and ady = sub (i ay) (i dy)
    and bdx = sub (i bx) (i dx) and bdy = sub (i by) (i dy)
    and cdx = sub (i cx) (i dx) and cdy = sub (i cy) (i dy) in
    let alift = Int64.add (mul adx adx) (mul ady ady)
    and blift = Int64.add (mul bdx bdx) (mul bdy bdy)
    and clift = Int64.add (mul cdx cdx) (mul cdy cdy) in
    let bcdet = sub (mul bdx cdy) (mul cdx bdy)
    and cadet = sub (mul cdx ady) (mul adx cdy)
    and abdet = sub (mul adx bdy) (mul bdx ady) in
    let determinant = Int64.add
        (Int64.add (mul alift bcdet) (mul blift cadet))
        (mul clift abdet) in
    let actual = Predicates.incircle
        ~ax:(float ax) ~ay:(float ay) ~bx:(float bx) ~by:(float by)
        ~cx:(float cx) ~cy:(float cy) ~dx:(float dx) ~dy:(float dy) in
    check (actual = sign_of_int64 determinant)
      "incircle disagrees with the int64 oracle"
  done;
  for index = 0 to 19_999 do
    let coordinate salt = deterministic_coordinate index salt 10_000 in
    let ax = coordinate 0 and ay = coordinate 1 and az = coordinate 2
    and bx = coordinate 3 and by = coordinate 4 and bz = coordinate 5
    and cx = coordinate 6 and cy = coordinate 7 and cz = coordinate 8
    and dx = coordinate 9 and dy = coordinate 10 and dz = coordinate 11 in
    let i value = Int64.of_int value and sub left right = Int64.sub left right in
    let adx = sub (i ax) (i dx) and ady = sub (i ay) (i dy)
    and adz = sub (i az) (i dz) and bdx = sub (i bx) (i dx)
    and bdy = sub (i by) (i dy) and bdz = sub (i bz) (i dz)
    and cdx = sub (i cx) (i dx) and cdy = sub (i cy) (i dy)
    and cdz = sub (i cz) (i dz) in
    let mul = Int64.mul in
    let determinant = Int64.add
        (Int64.add
           (mul adx (sub (mul bdy cdz) (mul bdz cdy)))
           (mul ady (sub (mul bdz cdx) (mul bdx cdz))))
        (mul adz (sub (mul bdx cdy) (mul bdy cdx))) in
    let actual = Predicates.orient3d
        ~ax:(float ax) ~ay:(float ay) ~az:(float az)
        ~bx:(float bx) ~by:(float by) ~bz:(float bz)
        ~cx:(float cx) ~cy:(float cy) ~cz:(float cz)
        ~dx:(float dx) ~dy:(float dy) ~dz:(float dz) in
    check (actual = sign_of_int64 determinant)
      "orient3d disagrees with the int64 oracle"
  done;

  (* Force both exact engines over the complete binary64 exponent range. The
     packed arena resets between calls and must remain identical to the
     immutable reference even when operand limb widths alternate sharply. *)
  for index = 0 to 4_999 do
    let coordinate salt = scaled_coordinate index salt in
    let ax = coordinate 0 and ay = coordinate 1
    and bx = coordinate 2 and by = coordinate 3
    and cx = coordinate 4 and cy = coordinate 5 in
    let arena = Predicates.Private.exact_orient2d_arena
        ~ax ~ay ~bx ~by ~cx ~cy
    and reference = Predicates.Private.exact_orient2d_reference
        ~ax ~ay ~bx ~by ~cx ~cy in
    check (arena = reference) "packed exact orient2d differs from reference"
  done;
  for index = 0 to 1_999 do
    let coordinate salt = scaled_coordinate index salt in
    let ax = coordinate 0 and ay = coordinate 1 and az = coordinate 2
    and bx = coordinate 3 and by = coordinate 4 and bz = coordinate 5
    and cx = coordinate 6 and cy = coordinate 7 and cz = coordinate 8
    and dx = coordinate 9 and dy = coordinate 10 and dz = coordinate 11 in
    let arena = Predicates.Private.exact_orient3d_arena
        ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz
    and reference = Predicates.Private.exact_orient3d_reference
        ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz in
    check (arena = reference) "packed exact orient3d differs from reference"
  done;
  for index = 0 to 1_999 do
    let coordinate salt = scaled_coordinate index salt in
    let ax = coordinate 0 and ay = coordinate 1
    and bx = coordinate 2 and by = coordinate 3
    and cx = coordinate 4 and cy = coordinate 5
    and dx = coordinate 6 and dy = coordinate 7 in
    let arena = Predicates.Private.exact_incircle_arena
        ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy
    and reference = Predicates.Private.exact_incircle_reference
        ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy in
    check (arena = reference) "packed exact incircle differs from reference"
  done;

  let polygon_sign x y = Predicates.polygon_area_sign_packed ~x ~y
      ~points:(Array.init (Array.length x) Fun.id) ~first:0
      ~count:(Array.length x) in
  check (polygon_sign [|0.;1.;1.;0.|] [|0.;0.;1.;1.|]
      = Predicates.Positive) "polygon area ordinary sign";
  check (polygon_sign [|0.;0.;1.;1.|] [|0.;1.;1.;0.|]
      = Predicates.Negative) "polygon area reversed sign";
  let maximum = Float.max_float in
  check (polygon_sign [|-.maximum;maximum;maximum;-.maximum|]
      [|-.maximum;-.maximum;maximum;maximum|] = Predicates.Positive)
    "polygon area overflow fallback";
  let tiny = Float.next_after 0. Float.infinity in
  check (polygon_sign [|0.;tiny;tiny;0.|] [|0.;0.;tiny;tiny|]
      = Predicates.Positive) "polygon area underflow fallback";
  check (polygon_sign [|0.;1.;0.;1.|] [|0.;1.;1.;0.|]
      = Predicates.Zero) "polygon area exact cancellation";
  for index = 0 to 4_999 do
    let count = 7 in
    let xi = Array.init count (fun local ->
        deterministic_coordinate index (local * 2) 1_000)
    and yi = Array.init count (fun local ->
        deterministic_coordinate index ((local * 2) + 1) 1_000) in
    let area = ref 0L in
    for local = 0 to count - 1 do
      let next = if local + 1 = count then 0 else local + 1 in
      area := Int64.add !area (Int64.sub
          (Int64.mul (Int64.of_int xi.(local)) (Int64.of_int yi.(next)))
          (Int64.mul (Int64.of_int xi.(next)) (Int64.of_int yi.(local))))
    done;
    check (polygon_sign (Array.map float xi) (Array.map float yi)
        = sign_of_int64 !area) "polygon area disagrees with int64 oracle"
  done;
  check (try ignore (Predicates.polygon_area_sign_packed ~x:[|0.;1.|]
      ~y:[|0.;1.|] ~points:[|0;1|] ~first:0 ~count:2); false
    with Invalid_argument _ -> true) "polygon area accepted a short range";

  (* The filtered path and exact fallback are pure and domain-independent. *)
  let sample domains = Parallel.run ~domains (fun () ->
      Array.init 20_000 (fun index ->
        let offset = float_of_int index *. 0.125 in
        Predicates.orient2d ~ax:offset ~ay:0. ~bx:(offset +. 1.) ~by:0.
          ~cx:offset ~cy:1.)) in
  check (sample 1 = sample 4) "predicate signs differ across domains"
