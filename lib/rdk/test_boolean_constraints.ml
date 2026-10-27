open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
let approximate_point plan point =
  Rdk_exact.Implicit_point.approximate (Constraints.Private.point plan point)

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

let left_triangle () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|]

let crossing_triangle () = geometry
    [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] [|0;1;2|]

let signature plan =
  let points = Array.init (Constraints.point_count plan)
      (approximate_point plan) in
  let constraints = Array.init (Constraints.constraint_count plan) (fun index ->
      Constraints.constraint_kind plan index,
      Constraints.constraint_first plan index,
      Constraints.constraint_second plan index,
      Constraints.constraint_left_triangle plan index,
      Constraints.constraint_right_triangle plan index) in
  let left_slots = Array.init
      (let _, last = Constraints.left_constraint_range plan
           (Constraints.left_triangle_count plan - 1) in last)
      (Constraints.left_constraint plan)
  and right_slots = Array.init
      (let _, last = Constraints.right_constraint_range plan
           (Constraints.right_triangle_count plan - 1) in last)
      (Constraints.right_constraint plan) in
  points, constraints, left_slots, right_slots,
  Constraints.coplanar_pair_count plan, Constraints.degenerate_pair_count plan

let test_crossing () =
  let plan = Constraints.build ~grain:1 ~left:(left_triangle ())
      ~right:(crossing_triangle ()) () |> get in
  check (Constraints.point_count plan = 2) "crossing should have two exact points";
  check (Constraints.constraint_count plan = 1) "crossing should have one constraint";
  check (Constraints.constraint_kind plan 0 = Constraints.Segment)
    "crossing constraint should be a segment";
  let first = Constraints.constraint_first plan 0
  and second = Constraints.constraint_second plan 0 in
  check (first <> second) "crossing segment endpoints collapsed";
  let endpoints = [approximate_point plan first;
                   approximate_point plan second]
      |> List.sort Stdlib.compare in
  (match endpoints with
   | [(x0,y0,z0); (x1,y1,z1)] ->
       if not (close x0 0.5 && close y0 0.5 && close z0 0.) then
         fail "first crossing endpoint is wrong: %.17g %.17g %.17g" x0 y0 z0;
       if not (close x1 0.5 && close y1 1.5 && close z1 0.) then
         fail "second crossing endpoint is wrong: %.17g %.17g %.17g" x1 y1 z1
   | _ -> assert false);
  check (Constraints.left_constraint_range plan 0 = (0,1)
      && Constraints.left_constraint plan 0 = 0)
    "left face CSR is wrong";
  check (Constraints.right_constraint_range plan 0 = (0,1)
      && Constraints.right_constraint plan 0 = 0)
    "right face CSR is wrong";
  check (Constraints.coplanar_pair_count plan = 0) "crossing marked coplanar"

let test_point_contact () =
  let right = geometry
      [|0.,0.,0.; -1.,0.,1.; -1.,0.,-1.|] [|0;1;2|] in
  let plan = Constraints.build ~grain:7 ~left:(left_triangle ()) ~right () |> get in
  check (Constraints.constraint_count plan = 1) "point contact was lost";
  check (Constraints.constraint_kind plan 0 = Constraints.Point)
    "point contact became a segment";
  check (Constraints.point_count plan = 1) "point contact was not deduplicated";
  let x,y,z = approximate_point plan 0 in
  check (close x 0. && close y 0. && close z 0.)
    "point contact coordinate is wrong"

let test_coplanar_and_disjoint () =
  let coplanar = geometry
      [|0.25,0.25,0.; 1.,0.25,0.; 0.25,1.,0.|] [|0;1;2|] in
  let plan = Constraints.build ~grain:1 ~left:(left_triangle ())
      ~right:coplanar () |> get in
  check (Constraints.constraint_count plan = 0)
    "coplanar pair entered non-coplanar constraints";
  check (Constraints.coplanar_pair_count plan = 1
      && Constraints.coplanar_left_triangle plan 0 = 0
      && Constraints.coplanar_right_triangle plan 0 = 0)
    "coplanar pair plan is wrong";
  let disjoint = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|] in
  let empty = Constraints.build ~grain:1 ~left:(left_triangle ())
      ~right:disjoint () |> get in
  check (Constraints.constraint_count empty = 0
      && Constraints.point_count empty = 0
      && Constraints.coplanar_pair_count empty = 0)
    "disjoint faces produced constraints"

let test_domain_exactness () =
  let run domains = Parallel.run ~domains (fun () ->
      Constraints.build ~grain:1 ~left:(left_triangle ())
        ~right:(crossing_triangle ()) () |> get |> signature) in
  if run 1 <> run 4 then fail "constraint plans differ between one and four domains"

let test_cross_face_deduplication () =
  let left = geometry
      [|0.,0.,0.; 2.,0.,0.; 2.,2.,0.; 0.,2.,0.|]
      [|0;1;2; 0;2;3|]
  and right = geometry
      [|1.,-1.,-1.; 1.,3.,-1.; 1.,3.,1.; 1.,-1.,1.|]
      [|0;1;2; 0;2;3|] in
  let plan = Constraints.build ~grain:1 ~left ~right () |> get in
  check (Constraints.point_count plan = 3)
    "shared face/edge intersection points were not exactly deduplicated";
  let segment_count = ref 0 in
  for constraint_index = 0 to Constraints.constraint_count plan - 1 do
    if Constraints.constraint_kind plan constraint_index = Constraints.Segment then
      incr segment_count
  done;
  check (!segment_count = 2) "split square should produce two segment constraints";
  let points = Array.init 3 (approximate_point plan) in
  Array.sort (fun (_, left_y, _) (_, right_y, _) ->
      Float.compare left_y right_y) points;
  Array.iteri (fun index (x,y,z) ->
    if not (close x 1. && close y (float_of_int index) && close z 0.) then
      fail "deduplicated cross-face point %d is wrong: %.17g %.17g %.17g"
        index x y z) points

let test_same_operand_constraints () =
  let left = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
      [|0;1;2; 3;4;5|]
  and right = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|] in
  let clean_plan = Constraints.build ~grain:1 ~left ~right () |> get in
  check (Constraints.constraint_count clean_plan = 0)
    "clean-input policy unexpectedly ran the left self broad phase";
  let plan = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left ~right () |> get in
  check (Constraints.constraint_count plan = 1
      && Constraints.constraint_kind plan 0 = Constraints.Segment)
    "same-operand transverse pair did not produce one segment";
  check (Constraints.constraint_first_side plan 0 = Constraints.Left
      && Constraints.constraint_second_side plan 0 = Constraints.Left)
    "same-operand constraint lost its typed endpoint sides";
  let first = Constraints.constraint_first_triangle plan 0
  and second = Constraints.constraint_second_triangle plan 0 in
  check ((first = 0 && second = 1) || (first = 1 && second = 0))
    "same-operand constraint triangle ancestry is wrong";
  check (Constraints.left_constraint_range plan 0 = (0,1)
      && Constraints.left_constraint_range plan 1 = (1,2)
      && Constraints.left_constraint plan 0 = 0
      && Constraints.left_constraint plan 1 = 0)
    "same-operand constraint was not incident on both source faces";
  check (Constraints.right_constraint_range plan 0 = (0,0))
    "same-operand constraint leaked into the other operand"

let test_right_operand_constraints () =
  let left = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
      [|0;1;2; 3;4;5|] in
  let plan = Constraints.build ~resolve_right_self_intersections:true
      ~grain:1 ~left ~right () |> get in
  check (Constraints.constraint_count plan = 1
      && Constraints.constraint_first_side plan 0 = Constraints.Right
      && Constraints.constraint_second_side plan 0 = Constraints.Right)
    "right-operand self constraint lost its typed endpoint sides";
  check (Constraints.right_constraint_range plan 0 = (0,1)
      && Constraints.right_constraint_range plan 1 = (1,2)
      && Constraints.left_constraint_range plan 0 = (0,0))
    "right-operand self constraint has incorrect face incidence"

let test_same_operand_shared_vertex_policy () =
  let far = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|] in
  let touching = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; -1.,0.,1.; -1.,0.,-1.|]
      [|0;1;2; 0;3;4|] in
  let plan = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left:touching ~right:far () |> get in
  check (Constraints.constraint_count plan = 0)
    "ordinary same-operand shared vertex became a self-intersection";
  let crossing = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 1.,1.,-1.; 1.,1.,1.|]
      [|0;1;2; 0;3;4|] in
  let plan = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left:crossing ~right:far () |> get in
  check (Constraints.constraint_count plan = 1
      && Constraints.constraint_kind plan 0 = Constraints.Segment)
    "self-intersection extending from a shared vertex was suppressed"

let test_same_operand_coplanar_pair () =
  let left = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.25,0.25,0.; 1.,0.25,0.; 0.25,1.,0.|]
      [|0;1;2; 3;4;5|]
  and right = geometry
      [|10.,10.,0.; 11.,10.,0.; 10.,11.,0.|] [|0;1;2|] in
  let plan = Constraints.build ~resolve_left_self_intersections:true
      ~grain:1 ~left ~right () |> get in
  check (Constraints.coplanar_pair_count plan = 1
      && Constraints.coplanar_first_side plan 0 = Constraints.Left
      && Constraints.coplanar_second_side plan 0 = Constraints.Left)
    "same-operand coplanar pair lost typed endpoint sides";
  let first = Constraints.coplanar_first_triangle plan 0
  and second = Constraints.coplanar_second_triangle plan 0 in
  check ((first = 0 && second = 1) || (first = 1 && second = 0))
    "same-operand coplanar triangle ancestry is wrong"

let test_invalid_grain () =
  match Constraints.build ~grain:0 ~left:(left_triangle ())
      ~right:(crossing_triangle ()) () with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected invalid-grain error: %s" (Error.to_string error)
  | Ok _ -> fail "zero grain was accepted"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Constraints.build ~cancel ~grain:1 ~left:(left_triangle ())
      ~right:(crossing_triangle ()) () with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled constraint planning completed"

let run () =
  test_crossing ();
  test_point_contact ();
  test_coplanar_and_disjoint ();
  test_domain_exactness ();
  test_cross_face_deduplication ();
  test_same_operand_constraints ();
  test_right_operand_constraints ();
  test_same_operand_shared_vertex_policy ();
  test_same_operand_coplanar_pair ();
  test_invalid_grain ();
  test_cancellation ()
