open Rdk

let fail format = Printf.ksprintf failwith format
let check condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format
let get_string = function Ok value -> value | Error message -> fail "%s" message

let triangulate x y = Delaunay2.build ~seed:17L ~x ~y () |> get_string

let cdt ?workspace ?(flood = false) ?winding ?(remove_outside_polygons = false)
    x y constraints =
  let seed = triangulate x y in
  let view = Delaunay2.Private.view seed in
  Planar_cdt.build ?workspace ~point_count:(Array.length x)
    ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
    ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
    ~bounds_overlap:(fun a b u v ->
      max (min x.(a) x.(b)) (min x.(u) x.(v))
        <= min (max x.(a) x.(b)) (max x.(u) x.(v))
      && max (min y.(a) y.(b)) (min y.(u) y.(v))
        <= min (max y.(a) y.(b)) (max y.(u) y.(v)))
    ~triangle_points:view.triangle_points ~flood_from_hull_boundary:flood
    ~constraint_points:constraints ?constraint_winding:winding
    ~remove_outside_constraint_polygons:remove_outside_polygons ()

let edge_present value first second =
  let found = ref false in
  for triangle = 0 to Planar_cdt.triangle_count value - 1 do
    let a = Planar_cdt.triangle_point value triangle 0
    and b = Planar_cdt.triangle_point value triangle 1
    and c = Planar_cdt.triangle_point value triangle 2 in
    if (a = first && b = second) || (a = second && b = first)
        || (b = first && c = second) || (b = second && c = first)
        || (c = first && a = second) || (c = second && a = first) then
      found := true
  done;
  !found

let signature value =
  Array.init (Planar_cdt.triangle_count value) (fun triangle ->
      Planar_cdt.triangle_point value triangle 0,
      Planar_cdt.triangle_point value triangle 1,
      Planar_cdt.triangle_point value triangle 2),
  Array.init (Planar_cdt.constraint_count value) (fun constraint_index ->
      Planar_cdt.constraint_first value constraint_index,
      Planar_cdt.constraint_second value constraint_index)

let validate x y value =
  for triangle = 0 to Planar_cdt.triangle_count value - 1 do
    let a = Planar_cdt.triangle_point value triangle 0
    and b = Planar_cdt.triangle_point value triangle 1
    and c = Planar_cdt.triangle_point value triangle 2 in
    check (Predicates.orient2d_packed ~x ~y a b c = Predicates.Positive)
      "triangle %d is not counter-clockwise" triangle
  done;
  for constraint_index = 0 to Planar_cdt.constraint_count value - 1 do
    check (edge_present value
        (Planar_cdt.constraint_first value constraint_index)
        (Planar_cdt.constraint_second value constraint_index))
      "constraint %d is missing" constraint_index
  done

let test_forced_square_diagonal () =
  let x = [|0.;1.;1.;0.|] and y = [|0.;0.;1.;1.|] in
  let seed = triangulate x y in
  let diagonal = if edge_present
      (Planar_cdt.build ~point_count:4
        ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
        ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
        ~triangle_points:(Delaunay2.Private.view seed).triangle_points
        ~constraint_points:[||] () |> get_string) 0 2 then [|1;3|] else [|0;2|] in
  let value = cdt x y diagonal |> get_string in
  check (edge_present value diagonal.(0) diagonal.(1))
    "forced square diagonal was not recovered";
  validate x y value

let test_long_recovery_and_domains () =
  let count = 500 in
  let x = Array.init count (fun point ->
      if point = 0 then -1000. else if point = 1 then 1000.
      else float_of_int (((point * 7919) mod 1901) - 950))
  and y = Array.init count (fun point ->
      if point < 2 then 0.
      else let value = ((point * 3571) mod 997) + 1 in
        if point land 1 = 0 then float_of_int value else -.float_of_int value) in
  let run domains = Parallel.run ~domains (fun () ->
      cdt x y [|0;1|] |> get_string) in
  let one = run 1 and four = run 4 in
  check (signature one = signature four) "domain count changed constrained topology";
  check (edge_present one 0 1) "long constraint was not recovered";
  validate x y one

let test_workspace_reuse_and_result_ownership () =
  let workspace = Planar_cdt.Private.create_workspace ~triangle_capacity:1 () in
  let square_x = [|0.;1.;1.;0.|] and square_y = [|0.;0.;1.;1.|] in
  let first = cdt ~workspace square_x square_y [|0;2|] |> get_string
  and reference = cdt square_x square_y [|0;2|] |> get_string in
  check (signature first = signature reference)
    "workspace changed the initial constrained topology";
  let retained = signature first in
  let count = 257 in
  let x = Array.init count (fun point ->
      if point = 0 then -1000. else if point = 1 then 1000.
      else float_of_int (((point * 7919) mod 1901) - 950))
  and y = Array.init count (fun point ->
      if point < 2 then 0.
      else let value = ((point * 3571) mod 997) + 1 in
        if point land 1 = 0 then float_of_int value else -.float_of_int value) in
  let grown = cdt ~workspace x y [|0;1|] |> get_string
  and grown_reference = cdt x y [|0;1|] |> get_string in
  check (signature grown = signature grown_reference)
    "grown workspace changed constrained topology";
  check (signature first = retained)
    "workspace reuse mutated an earlier independently owned result";
  let seed = triangulate square_x square_y |> Delaunay2.Private.view in
  let malformed = Planar_cdt.build ~workspace ~point_count:4
      ~orient:(fun a b c ->
        Predicates.orient2d_packed ~x:square_x ~y:square_y a b c)
      ~incircle:(fun a b c d ->
        Predicates.incircle_packed ~x:square_x ~y:square_y a b c d)
      ~triangle_points:[|0;1;2;0;1;2;0;1;2|] ~constraint_points:[||] () in
  (match malformed with Error _ -> ()
   | Ok _ -> fail "malformed workspace build unexpectedly succeeded");
  let recovered = Planar_cdt.build ~workspace ~point_count:4
      ~orient:(fun a b c ->
        Predicates.orient2d_packed ~x:square_x ~y:square_y a b c)
      ~incircle:(fun a b c d ->
        Predicates.incircle_packed ~x:square_x ~y:square_y a b c d)
      ~triangle_points:seed.triangle_points ~constraint_points:[|0;2|] ()
      |> get_string in
  check (signature recovered = signature reference)
    "workspace did not recover after a failed build"

let test_incremental_workspace_continuation () =
  let x = [|0.;2.;0.;1.|] and y = [|0.;0.;2.;0.|] in
  let seed = Delaunay2.build ~x:(Array.sub x 0 3) ~y:(Array.sub y 0 3) ()
      |> get_string |> Delaunay2.Private.view in
  let workspace = Planar_cdt.Private.create_workspace ~point_capacity:4
      ~triangle_capacity:1 () in
  let run ?workspace ~point_count ~triangle_points ?(insert_points = [||])
      constraints =
    Planar_cdt.build ?workspace ~point_count
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points ~insert_points ~constraint_points:constraints ()
      |> get_string in
  let first = run ~workspace ~point_count:3
      ~triangle_points:seed.triangle_points [||] in
  let first_triangles = (Planar_cdt.Private.view first).triangle_points in
  let continued = run ~workspace ~point_count:4
      ~triangle_points:first_triangles ~insert_points:[|3|] [|0;3;3;1|] in
  let reference = run ~point_count:4 ~triangle_points:first_triangles
      ~insert_points:[|3|] [|0;3;3;1|] in
  check (signature continued = signature reference)
    "incremental point insertion differs from a full CDT rebuild";
  let continued_triangles = (Planar_cdt.Private.view continued).triangle_points in
  let repaired = run ~workspace ~point_count:4
      ~triangle_points:continued_triangles [|0;3;3;1|] in
  check (signature repaired = signature continued)
    "incremental coordinate-repair continuation changed stable topology";
  check (Planar_cdt.Private.build_counts workspace = (1,2))
    "workspace did not take the expected full/incremental paths";
  let repaired_triangles = (Planar_cdt.Private.view repaired).triangle_points in
  let unconstrained = run ~workspace ~point_count:4
      ~triangle_points:repaired_triangles [||]
  and unconstrained_reference = run ~point_count:4
      ~triangle_points:repaired_triangles [||] in
  check (signature unconstrained = signature unconstrained_reference)
    "incremental constraint removal differs from full rebuild";
  check (Planar_cdt.Private.build_counts workspace = (1,3))
    "constraint removal did not remain incremental";
  let copied = Array.copy
      (Planar_cdt.Private.view unconstrained).triangle_points in
  ignore (run ~workspace ~point_count:4 ~triangle_points:copied [|0;3;3;1|]);
  check (Planar_cdt.Private.build_counts workspace = (2,3))
    "structurally copied input did not take the safe full-rebuild fallback"

let test_incremental_constraint_replacement () =
  let x = [|0.;1.;1.;0.|] and y = [|0.;0.;1.;1.|] in
  let seed = triangulate x y |> Delaunay2.Private.view in
  let seed_value = Planar_cdt.build ~point_count:4
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points:seed.triangle_points ~constraint_points:[||] ()
      |> get_string in
  let first,replacement =
    if edge_present seed_value 0 2 then [|0;2|],[|1;3|]
    else [|1;3|],[|0;2|] in
  let workspace = Planar_cdt.Private.create_workspace ~point_capacity:4
      ~triangle_capacity:2 () in
  let run ?workspace triangle_points constraints =
    Planar_cdt.build ?workspace ~point_count:4
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points ~constraint_points:constraints () |> get_string in
  let constrained = run ~workspace seed.triangle_points first in
  let constrained_triangles =
    (Planar_cdt.Private.view constrained).triangle_points in
  let replaced = run ~workspace constrained_triangles replacement
  and reference = run constrained_triangles replacement in
  check (signature replaced = signature reference)
    "incremental constraint replacement differs from full rebuild";
  check (edge_present replaced replacement.(0) replacement.(1))
    "incremental constraint replacement did not recover the added diagonal";
  check (not (edge_present replaced first.(0) first.(1)))
    "incremental constraint replacement retained the removed diagonal";
  check (Planar_cdt.Private.build_counts workspace = (1,1))
    "constraint replacement did not remain incremental"

let test_incremental_workspace_batches () =
  let count = 68 in
  let x = Array.make count 0. and y = Array.make count 0. in
  x.(0) <- 0.; y.(0) <- 0.; x.(1) <- 1.; y.(1) <- 0.;
  x.(2) <- 1.; y.(2) <- 1.; x.(3) <- 0.; y.(3) <- 1.;
  for point = 4 to count - 1 do
    let state = Rays_math.Rand.seed ((point * 7919) + 17) in
    let px,state = Rays_math.Rand.float state in
    let py,_ = Rays_math.Rand.float state in
    x.(point) <- 0.05 +. (0.9 *. px);
    y.(point) <- 0.05 +. (0.9 *. py)
  done;
  let seed = Delaunay2.build ~x:(Array.sub x 0 4) ~y:(Array.sub y 0 4) ()
      |> get_string |> Delaunay2.Private.view in
  let constraints = [|0;1;1;2;2;3;3;0|] in
  let workspace = Planar_cdt.Private.create_workspace ~point_capacity:count
      ~triangle_capacity:2 () in
  let run ?workspace ~point_count ~triangle_points ?(insert_points = [||]) () =
    Planar_cdt.build ?workspace ~point_count
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points ~insert_points ~constraint_points:constraints ()
      |> get_string in
  let incremental = ref (run ~workspace ~point_count:4
      ~triangle_points:seed.triangle_points ())
  and reference = ref (run ~point_count:4
      ~triangle_points:seed.triangle_points ())
  and first = ref 4 and batch_count = ref 0 in
  while !first < count do
    let last = min count (!first + 9) in
    let insert_points = Array.init (last - !first) (fun index -> !first + index) in
    incremental := run ~workspace ~point_count:last
        ~triangle_points:(Planar_cdt.Private.view !incremental).triangle_points
        ~insert_points ();
    reference := run ~point_count:last
        ~triangle_points:(Planar_cdt.Private.view !reference).triangle_points
        ~insert_points ();
    check (signature !incremental = signature !reference)
      "incremental batch ending at point %d differs from full rebuild" last;
    first := last;
    incr batch_count
  done;
  check (Planar_cdt.Private.build_counts workspace = (1,!batch_count))
    "multi-generation workspace did not remain incremental"

let test_incremental_coordinate_validation () =
  let x = [|0.;1.;0.|] and y = [|0.;0.;1.|] in
  let seed = triangulate x y |> Delaunay2.Private.view in
  let workspace = Planar_cdt.Private.create_workspace ~point_capacity:3
      ~triangle_capacity:1 () in
  let run ?workspace triangle_points =
    Planar_cdt.build ?workspace ~point_count:3
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points ~constraint_points:[||] () in
  let first = run ~workspace seed.triangle_points |> get_string in
  y.(2) <- -1.;
  let input = (Planar_cdt.Private.view first).triangle_points in
  let inverted = run ~workspace input |> get_string
  and reference = run input |> get_string in
  check (signature inverted = signature reference)
    "incremental coordinate repair did not normalize inverted orientation";
  y.(2) <- 0.;
  let degenerate_input = (Planar_cdt.Private.view inverted).triangle_points in
  (match run ~workspace degenerate_input with
   | Error _ -> ()
   | Ok _ -> fail "incremental degenerate coordinate repair succeeded");
  y.(2) <- 1.;
  let recovered = run ~workspace degenerate_input |> get_string
  and recovered_reference = run degenerate_input |> get_string in
  check (signature recovered = signature recovered_reference)
    "workspace did not rebuild after failed coordinate continuation";
  check (Planar_cdt.Private.build_counts workspace = (2,1))
    "coordinate continuation diagnostics are incorrect"

let test_crossing_rejected () =
  let x = [|0.;1.;1.;0.|] and y = [|0.;0.;1.;1.|] in
  match cdt x y [|0;2; 1;3|] with
  | Error _ -> ()
  | Ok _ -> fail "crossing constraints succeeded without splitting"

let test_inserted_points () =
  let boundary_x = [|0.;2.;0.;1.|] and boundary_y = [|0.;0.;2.;0.|] in
  let boundary_seed = Delaunay2.build ~x:(Array.sub boundary_x 0 3)
      ~y:(Array.sub boundary_y 0 3) () |> get_string in
  let boundary = Planar_cdt.build ~point_count:4
      ~orient:(fun a b c -> Predicates.orient2d_packed
        ~x:boundary_x ~y:boundary_y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed
        ~x:boundary_x ~y:boundary_y a b c d)
      ~triangle_points:(Delaunay2.Private.view boundary_seed).triangle_points
      ~insert_points:[|3|] ~constraint_points:[|0;3;3;1|] () |> get_string in
  check (Planar_cdt.triangle_count boundary = 2
      && edge_present boundary 0 3 && edge_present boundary 3 1)
    "inserted boundary-edge split is incorrect";
  let x = [|0.;2.;2.;0.;1.|] and y = [|0.;0.;2.;2.;1.|] in
  let seed = Delaunay2.build ~x:(Array.sub x 0 4) ~y:(Array.sub y 0 4) ()
      |> get_string in
  let output = Planar_cdt.build ~point_count:5
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points:(Delaunay2.Private.view seed).triangle_points
      ~insert_points:[|4|]
      ~constraint_points:[|0;4; 4;2; 1;4; 4;3|] () |> get_string in
  check (Planar_cdt.triangle_count output = 4) "inserted center cardinality";
  List.iter (fun (a,b) -> check (edge_present output a b)
      "inserted constraint (%d,%d) is missing" a b)
    [0,4;4,2;1,4;4,3];
  validate x y output;

  let module Point = Rdk_exact.Implicit_point in
  let source = Point.source ~x:(Array.sub x 0 4) ~y:(Array.sub y 0 4)
      ~z:[|0.;0.;0.;0.|] |> function Ok value -> value | Error _ ->
        fail "implicit source construction failed" in
  let points = Array.init 5 (fun point ->
      if point < 4 then Point.explicit source point |> function
        | Ok value -> value | Error _ -> fail "explicit point construction failed"
      else Point.line_line source ~projection:Point.XY
          ~first_start:0 ~first_end:2 ~second_start:1 ~second_end:3
          |> function Ok value -> value | Error _ ->
            fail "line-line construction failed") in
  let output = Planar_cdt.build ~point_count:5
      ~orient:(fun a b c -> Point.orient2d_xy points.(a) points.(b) points.(c))
      ~incircle:(fun a b c d ->
        Point.incircle_xy points.(a) points.(b) points.(c) points.(d))
      ~triangle_points:(Delaunay2.Private.view seed).triangle_points
      ~insert_points:[|4|]
      ~constraint_points:[|0;4; 4;2; 1;4; 4;3|] () |> get_string in
  check (Planar_cdt.triangle_count output = 4)
    "exact implicit inserted center cardinality";
  List.iter (fun (a,b) -> check (edge_present output a b)
      "exact inserted constraint (%d,%d) is missing" a b)
    [0,4;4,2;1,4;4,3]

let test_hull_boundary_flood () =
  let x = [|-2.;2.;2.;-2.; -1.;1.;1.;-1.; 0.|]
  and y = [|-2.;-2.;2.;2.; -1.;-1.;1.;1.; 0.|] in
  let constraints = [|4;5;5;6;6;7;7;4|] in
  let output = cdt ~flood:true x y constraints |> get_string in
  check (Planar_cdt.triangle_count output = 4)
    "hull flood did not retain exactly the enclosed square";
  for triangle = 0 to Planar_cdt.triangle_count output - 1 do
    for local = 0 to 2 do
      check (Planar_cdt.triangle_point output triangle local >= 4)
        "hull flood retained an exterior point"
    done
  done;
  check (Planar_cdt.constraint_count output = 4)
    "hull flood lost a retained boundary constraint";
  validate x y output;
  let empty = cdt ~flood:true [|0.;1.;0.|] [|0.;0.;1.|] [||]
      |> get_string in
  check (Planar_cdt.triangle_count empty = 0
      && Planar_cdt.constraint_count empty = 0)
    "unblocked hull flood did not remove the complete triangulation";
  let hull = cdt ~flood:true [|0.;1.;1.;0.|] [|0.;0.;1.;1.|]
      [|0;1;1;2;2;3;3;0|] |> get_string in
  check (Planar_cdt.triangle_count hull = 2
      && Planar_cdt.constraint_count hull = 4)
    "constrained hull boundary did not block exterior flooding"

let test_constraint_polygon_winding () =
  let x = [|-2.;2.;2.;-2.; -1.;1.;1.;-1.; 0.|]
  and y = [|-2.;-2.;2.;2.; -1.;-1.;1.;1.; 0.|] in
  let ccw = cdt ~winding:[|1;1;1;1|] ~remove_outside_polygons:true
      x y [|4;5;5;6;6;7;7;4|] |> get_string in
  check (Planar_cdt.triangle_count ccw = 4)
    "constraint-polygon winding retained the wrong region";
  for triangle = 0 to Planar_cdt.triangle_count ccw - 1 do
    for local = 0 to 2 do
      check (Planar_cdt.triangle_point ccw triangle local >= 4)
        "constraint-polygon winding retained an exterior point"
    done
  done;
  let clockwise = cdt ~winding:[|1;1;1;1|]
      ~remove_outside_polygons:true x y [|4;7;7;6;6;5;5;4|]
      |> get_string in
  check (signature clockwise = signature ccw)
    "polygon orientation changed non-zero interior selection";
  let cancelled = cdt ~winding:[|1;1;-1;-1;1;1;-1;-1|]
      ~remove_outside_polygons:true x y
      [|4;5;5;6;6;7;7;4; 5;4;6;5;7;6;4;7|] |> get_string in
  check (Planar_cdt.triangle_count cancelled = 0)
    "opposite duplicate loops did not cancel winding";
  let x = [|-3.;3.;3.;-3.; -2.;2.;2.;-2.; -1.;1.;1.;-1.; 0.|]
  and y = [|-3.;-3.;3.;3.; -2.;-2.;2.;2.; -1.;-1.;1.;1.; 0.|] in
  let outer = [|4;5;5;6;6;7;7;4|]
  and inner_ccw = [|8;9;9;10;10;11;11;8|]
  and inner_cw = [|8;11;11;10;10;9;9;8|] in
  let constraints inner = Array.init 16 (fun index ->
      if index < 8 then outer.(index) else inner.(index - 8)) in
  let weights = Array.make 8 1 in
  let solid = cdt ~winding:weights ~remove_outside_polygons:true
      x y (constraints inner_ccw) |> get_string
  and hole = cdt ~winding:weights ~remove_outside_polygons:true
      x y (constraints inner_cw) |> get_string in
  let references value point =
    let found = ref false in
    for triangle = 0 to Planar_cdt.triangle_count value - 1 do
      for local = 0 to 2 do
        if Planar_cdt.triangle_point value triangle local = point then found := true
      done
    done;
    !found in
  check (references solid 12) "same-winding nested loop removed its interior";
  check (not (references hole 12) && Planar_cdt.triangle_count hole > 0)
    "opposite-winding nested loop did not create a hole"

let test_constraint_cavity_reinserts_interior_points () =
  let random_count = 96 and source_count = 100 in
  let x = Array.make source_count 0. and y = Array.make source_count 0. in
  for point = 0 to random_count - 1 do
    let state = Rays_math.Rand.seed ((point * 37) + 11) in
    let px,state = Rays_math.Rand.float state in
    let py,_ = Rays_math.Rand.float state in
    x.(point) <- (px *. 290.) +. 15.; y.(point) <- (py *. 190.) +. 15.
  done;
  x.(96) <- 20.; y.(96) <- 20.; x.(97) <- 300.; y.(97) <- 20.;
  x.(98) <- 300.; y.(98) <- 200.; x.(99) <- 20.; y.(99) <- 200.;
  let seed = Delaunay2.build ~seed:0L ~x ~y () |> get_string in
  let arrangement = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:[|96;97;97;98;98;99;99;96;96;98;97;99|] ()
      |> get_string in
  let view = Planar_constraints.Private.view arrangement in
  let output = Planar_cdt.build
      ~point_count:(Planar_constraints.point_count arrangement)
      ~orient:(Planar_constraints.Private.orient2d arrangement)
      ~incircle:(Planar_constraints.Private.incircle arrangement)
      ~bounds_overlap:(fun a b u v ->
        max (min view.x.(a) view.x.(b)) (min view.x.(u) view.x.(v))
          <= min (max view.x.(a) view.x.(b)) (max view.x.(u) view.x.(v))
        && max (min view.y.(a) view.y.(b)) (min view.y.(u) view.y.(v))
          <= min (max view.y.(a) view.y.(b)) (max view.y.(u) view.y.(v)))
      ~triangle_points:(Delaunay2.Private.view seed).triangle_points
      ~insert_points:view.insert_points ~constraint_points:view.constraint_points ()
      |> get_string in
  check (Planar_cdt.triangle_count output = Delaunay2.triangle_count seed + 2)
    "constraint cavity did not retain/reinsert every interior point";
  validate view.x view.y output

let test_validation_and_cancellation () =
  let x = [|0.;1.;0.|] and y = [|0.;0.;1.|] in
  let seed = triangulate x y in
  let view = Delaunay2.Private.view seed in
  let run ?cancel ?workspace constraints = Planar_cdt.build ?cancel ?workspace
      ~point_count:3
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points:view.triangle_points ~constraint_points:constraints () in
  (match run [|0;0|] with Error _ -> () | Ok _ -> fail "self constraint succeeded");
  (match run [|0;9|] with Error _ -> () | Ok _ -> fail "invalid endpoint succeeded");
  check (try
      ignore (Planar_cdt.Private.create_workspace ~point_capacity:0
        ~triangle_capacity:1 ());
      false
    with Invalid_argument _ -> true)
    "zero point-capacity workspace succeeded";
  let workspace = Planar_cdt.Private.create_workspace ~triangle_capacity:1 () in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  (match run ~cancel ~workspace [|0;1|] with
   | Error _ -> () | Ok _ -> fail "cancelled CDT succeeded");
  let recovered = run ~workspace [|0;1|] |> get_string
  and reference = run [|0;1|] |> get_string in
  check (signature recovered = signature reference)
    "workspace did not recover after cancellation"

let run () =
  test_forced_square_diagonal ();
  test_long_recovery_and_domains ();
  test_workspace_reuse_and_result_ownership ();
  test_incremental_workspace_continuation ();
  test_incremental_constraint_replacement ();
  test_incremental_workspace_batches ();
  test_incremental_coordinate_validation ();
  test_crossing_rejected ();
  test_inserted_points ();
  test_hull_boundary_flood ();
  test_constraint_polygon_winding ();
  test_constraint_cavity_reinserts_interior_points ();
  test_validation_and_cancellation ()
